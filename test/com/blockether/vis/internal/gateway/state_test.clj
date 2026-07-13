(ns com.blockether.vis.internal.gateway.state-test
  "Wire-event projection. Form errors ship LEAN, single-surface:
   `block.output` carries human text (message + line/col + hint), never the
   pr-str'd op-error map (which nests host trace/data chains), and is omitted
   entirely when an errored op in the form's sink slice already renders the
   same failure as an op card — the web thread painted that failure twice."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.persistance :as persistance]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tool-error
  {:message "rg spec has unknown keys: spec."
   :data {:phase :python/host :type :vis/tool-failure :symbol :rg}})

(defdescribe
  thinking-newline-normalization-test
  "Gateway-owned thinking normalization keeps live SSE, poll/replay, and session
   consumers in sync. The web channel may still render defensively, but it must
   not be the first place where blank-line runs disappear."
  (it "does not emit streamed reasoning deltas over the gateway"
      (expect (nil? (#'state/chunk->event
                     {:phase :reasoning :text " first  \n\n\t\nsecond\r\n\r\nthird  "}))))
  (it "normalizes iteration-boundary thinking for pinned session history"
      (let [[type store? payload]
            (#'state/chunk->event
             {:phase :iteration-final :done? true :thinking " alpha\n\n\n beta  \n\t\n gamma "})]
        (expect (= "iteration.completed" type))
        (expect store?)
        (expect (= "alpha\n beta\n gamma" (:thinking payload)))))
  (it "normalizes persisted transcript thinking the same way as live events"
      (with-redefs [persistance/db-list-session-turns
                    (fn [_ sid]
                      [{:id sid :status :success}])

                    persistance/db-list-session-turn-iterations
                    (fn [_ _]
                      [{:thinking " alpha\n\n beta  \n"}])]

        (expect (= "alpha\n beta"
                   (-> (state/transcript :session-1)
                       first
                       :iterations
                       first
                       :thinking))))))

(defdescribe
  form-result-error-wire-test
  ;; Op cards are gone, so there is no op-dedup: a form error ALWAYS surfaces
  ;; on the wire. What remains to assert is the lean text shape.
  (it "ships a lean text error (message + line/col + hint), never the pr-str'd map"
      (let [[_ _ payload] (#'state/chunk->event
                           {:phase :form-result
                            :position 0
                            :code "1/0"
                            :error {:message "ZeroDivisionError: division by zero"
                                    :hint "check denominator"
                                    :data {:phase :python/runtime :line 1 :column 1}}})]
        (expect (= "ZeroDivisionError: division by zero (line 1, col 1)\nhint: check denominator"
                   (:error payload)))
        (expect (not (str/includes? (:error payload) ":data")))))
  (it "a form error always surfaces on the wire"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :form-result :position 0 :code "rg(...)" :error tool-error})]
        (expect (= "block.output" type))
        (expect (= "rg spec has unknown keys: spec." (:error payload))))))

(defdescribe
  form-event-iteration-wire-test
  ;; THE "live shows reasoning but no code" bug: every streaming chunk carries
  ;; its iteration POSITION under `:iteration`, and that MUST ride the wire
  ;; event — `make-progress-tracker` DROPS any chunk with no iteration, which is
  ;; how `block.started` / `block.output` once lost their forms and the live
  ;; bubble showed reasoning but no code.
  (it "block.started carries :iteration on the wire"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :form-start :iteration 1 :position 0 :code "import hashlib"})]
        (expect (= "block.started" type))
        (expect (= 1 (:iteration payload)))))
  (it "block.output carries :iteration on the wire"
      (let [[type _ payload]
            (#'state/chunk->event
             {:phase :form-result :iteration 3 :position 0 :code "print(42)" :stdout "42"})]
        (expect (= "block.output" type))
        (expect (= 3 (:iteration payload)))))
  (it "reasoning.delta is not emitted on the gateway wire"
      (expect (nil? (#'state/chunk->event {:phase :reasoning :iteration 2 :text "hmm"}))))
  (it "iteration-final carries :iteration and complete assistant prose on the wire"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :iteration-final
                               :iteration 5
                               :done true
                               :thinking "t"
                               :assistant-prose " full prose "})]
        (expect (= "iteration.completed" type))
        (expect (= 5 (:iteration payload)))
        (expect (= "full prose" (:assistant-prose payload))))))

(defdescribe
  iteration-attachment-descriptor-wire-test
  ;; Live `iteration.completed` carries metadata-ONLY attachment descriptors
  ;; (NEVER base64) so a native client (iOS/RN) learns an image was produced and
  ;; lazy-fetches it from the byte endpoint. `:index` is the position in the SAME
  ;; ordered list the byte endpoint serves, so index N always names one artifact.
  (it "omits :attachments when the iteration produced none"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :iteration-final :iteration 2 :done true :thinking "t"})]
        (expect (= "iteration.completed" type))
        (expect (not (contains? payload :attachments)))))
  (it "omits :attachments when there is no iteration-id to address them"
      (let [[_ _ payload] (#'state/chunk->event
                           {:phase :iteration-final :iteration 2 :done true :attachment-count 3})]
        (expect (not (contains? payload :attachments)))))
  (it "projects lean snake-case descriptors and NEVER leaks base64"
      (with-redefs [state/iteration-attachments
                    (fn [_iid]
                      [{:tool-call-id "call_A"
                        :kind "image"
                        :media-type "image/png"
                        :filename "fig.png"
                        :size 1234
                        :base64 "SECRET"} {:tool-call-id nil :media-type "image/svg+xml" :size 0}])]
        (let [[_ _ payload] (#'state/chunk->event
                             {:phase :iteration-final
                              :iteration 4
                              :done false
                              :iteration-id "00000000-0000-0000-0000-0000000000ab"
                              :attachment-count 2})
              atts (:attachments payload)]

          (expect (= 2 (count atts)))
          (expect (= [0 1] (mapv :index atts)))
          (expect (= "image/png" (:media_type (first atts))))
          (expect (= "call_A" (:tool_call_id (first atts))))
          (expect (= 1234 (:size (first atts))))
          ;; default kind for the un-kinded second artifact
          (expect (= "image" (:kind (second atts))))
          ;; bytes NEVER ride the frame
          (expect (not (str/includes? (pr-str payload) "SECRET")))
          (expect (every? #(not (contains? % :base64)) atts))))))

(defdescribe
  broadcast-title-poll-parity-test
  "A sibling-session title update must be STORED on every other registered
   session, so the /poll fallback (which reads the replay ring and never
   registers as a subscriber) delivers the identical frame the live SSE
   client gets. Previously the copy was live-only + gated on `:subscribers`,
   so poll clients silently missed it."
  (it "stores the sibling title event on a session with NO subscriber (poll-only)"
      (let [a
            (java.util.UUID/randomUUID)

            b
            (java.util.UUID/randomUUID)

            registry
            @#'state/registry

            saved
            @registry]

        (try
          ;; b carries no :subscribers — exactly a client on the /poll fallback
          (reset! registry {a {:next-seq 0} b {:next-seq 0}})
          (#'state/broadcast-title-event! a "Tidal Forces")
          (let [a-events
                (state/events-since a 0)

                b-events
                (state/events-since b 0)]

            ;; the titled session keeps its own stored event
            (expect (= 1 (count a-events)))
            (expect (= "session.title_updated" (:type (first a-events))))
            ;; the sibling (poll-only) session ALSO has it stored → /poll sees it
            (expect (= 1 (count b-events)))
            (expect (= "session.title_updated" (:type (first b-events))))
            ;; foreign copy carries the TITLED session's id, not b's
            (expect (= (str a) (:session_id (first b-events)))))
          (finally (reset! registry saved))))))

(defdescribe
  provider-error-ir-answer-wire-test
  "An engine IR-AST answer (the provider-error / fatal-iteration fallback)
   must flatten to PLAIN TEXT for :answer_md — never a pr-str'd `[:ir …]`
   vector dumped into the chat bubble. The rich AST rides :answer_ir for
   the IR walker."
  (let [ir-answer
        [:ir {:vis/provider-error true} [:h {:level 2} [:span {} "🚨 PROVIDER_ERROR"]]
         [:p {} [:strong {} [:span {} "Provider call failed before the model could run."]]]
         [:p {}
          [:strong {}
           [:span {} "WHAT HAPPENED: provider rejected the request before the model ran."]]]]]
    (it "recognises an IR-AST answer vs markdown / maps"
        (expect (#'state/ir-ast-answer? ir-answer))
        (expect (not (#'state/ir-ast-answer? "## hello")))
        (expect (not (#'state/ir-ast-answer? {:answer "hi"}))))
    (it "answer-md flattens the AST to plain text, never a pr-str'd vector"
        (let [md (#'state/answer-md ir-answer)]
          (expect (string? md))
          (expect (not (str/starts-with? md "[:ir")))
          (expect (not (str/includes? md ":vis/provider-error")))
          (expect (str/includes? md "PROVIDER_ERROR"))
          (expect (str/includes? md "Provider call failed before the model could run."))))
    (it "leaves a plain markdown answer untouched"
        (expect (= "## hello" (#'state/answer-md "## hello"))))))

(defdescribe
  list-turns-dedup-test
  "A refreshed web page hydrates from gateway/list-turns. Once the engine DB row
  exists, the completed gateway overlay row must disappear; otherwise the last
  request/response pair renders twice and the transient duplicate has no DB
  iterations disclosure."
  (it
    "prefers the persisted row over a matching completed live row with engine id"
    (let [sid
          (java.util.UUID/randomUUID)

          gateway-id
          "gateway-turn"

          engine-id
          (java.util.UUID/randomUUID)

          registry
          @#'state/registry

          saved
          @registry]

      (try (reset! registry {sid {:next-seq 0
                                  :turn-order [gateway-id]
                                  :turns {gateway-id {:turn_id gateway-id
                                                      :engine_turn_id (str engine-id)
                                                      :session_id (str sid)
                                                      :status "completed"
                                                      :request "hello"
                                                      :answer_md "hi"
                                                      :started_at 1000}}}})
           (with-redefs [persistance/db-list-session-turns (fn [_ _]
                                                             [{:id engine-id
                                                               :status :success
                                                               :user-request "hello"
                                                               :answer-markdown "hi"
                                                               :iteration-count 2
                                                               :created-at (java.util.Date.
                                                                             1010)}])]
             (let [turns (state/list-turns sid)]
               (expect (= 1 (count turns)))
               (expect (= (str engine-id) (:turn_id (first turns))))
               (expect (= 2 (:iteration_count (first turns))))))
           (finally (reset! registry saved)))))
  (it
    "prefers the persisted row over a matching completed live row with no engine id"
    (let [sid
          (java.util.UUID/randomUUID)

          gateway-id
          "gateway-turn"

          engine-id
          (java.util.UUID/randomUUID)

          started
          1000

          registry
          @#'state/registry

          saved
          @registry]

      (try (reset! registry {sid {:next-seq 0
                                  :turn-order [gateway-id]
                                  :turns {gateway-id {:turn_id gateway-id
                                                      :session_id (str sid)
                                                      :status "completed"
                                                      :request "hello"
                                                      :answer_md "hi"
                                                      :started_at started}}}})
           (with-redefs [persistance/db-list-session-turns (fn [_ s]
                                                             (expect (= sid s))
                                                             [{:id engine-id
                                                               :status :success
                                                               :user-request "hello"
                                                               :answer-markdown "hi"
                                                               :iteration-count 2
                                                               :created-at (java.util.Date.
                                                                             (+ started 10))}])]
             (let [turns (state/list-turns sid)]
               (expect (= 1 (count turns)))
               (expect (= (str engine-id) (:turn_id (first turns))))
               (expect (= 2 (:iteration_count (first turns))))))
           (finally (reset! registry saved))))))

(defdescribe
  queued-update-payload-test
  "Editing a queued request must also edit the provider message payload;
  otherwise the drained queued turn answers the pre-edit prompt."
  (it "replaces the last user message content"
      (let [messages [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                      {:role "assistant" :content "old answer"}
                      {:role :user :content "queued old"}]]
        (expect (= [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                    {:role "assistant" :content "old answer"} {:role :user :content "queued new"}]
                   (#'state/replace-last-user-message-content messages "queued new"))))))

(defdescribe persisted-duplicate-of-live-test
             ;; A FAILED/cancelled turn's error text lands ONLY on the durable row
             ;; (`answer-markdown`); the transient live row's `answer_md` is blank, so
             ;; a strict answer-equality check never matched an error turn and BOTH
             ;; rows rendered — the same "Could not reach provider" twice. Request +
             ;; terminal-status + created-after-start still identifies the one turn.
             (let [dup?
                   #'state/persisted-duplicate-of-live?

                   at
                   (fn [ms]
                     (java.util.Date. (long ms)))]

               (it "dedups an error turn whose live row has no answer to compare"
                   (expect (dup? {:engine_turn_id nil
                                  :status "error"
                                  :request "add zprint"
                                  :answer_md ""
                                  :started_at 1000}
                                 {:id "soul-1"
                                  :user-request "add zprint"
                                  :answer-markdown "## Could not reach provider"
                                  :created-at (at 2000)})))
               (it "does NOT over-dedup two distinct completed answers with the same request"
                   (expect (not (dup? {:engine_turn_id nil
                                       :status "completed"
                                       :request "hi"
                                       :answer_md "answer A"
                                       :started_at 1000}
                                      {:id "soul-2"
                                       :user-request "hi"
                                       :answer-markdown "answer B"
                                       :created-at (at 2000)}))))
               (it "still matches on the engine-turn-id primary key"
                   (expect (dup? {:engine_turn_id "eng-9" :status "completed"} {:id "eng-9"})))))

(defdescribe
  mirror-turn-row-test
  "A turn running in a SIBLING process arrives only as bus-mirrored events. To
   render it like a locally-started turn (user bubble + running chip, not bare
   deltas leaking under the previous answer), `ingest-mirrored-event!` must
   materialize a running row in :turns/:turn-order on `turn.started` and mark it
   terminal on `turn.completed`/`turn.failed` — carrying :engine_turn_id so
   list-turns can dedup it against the durable DB row once persisted."
  (let [reg
        @#'state/registry

        sid
        "mirror-test-sid"]

    (it "materializes a running row on turn.started, terminal on turn.completed"
        (try (swap! reg assoc sid {:next-seq 0})
             (#'state/ingest-mirrored-event!
              sid
              false
              {:type "turn.started" :turn_id "T1" :request "hello world" :started_at 777})
             (let [started (get @reg sid)]
               (expect (= "T1" (:current-turn started)))
               (expect (= ["T1"] (:turn-order started)))
               (expect (= "running" (get-in started [:turns "T1" :status])))
               (expect (= "hello world" (get-in started [:turns "T1" :request])))
               ;; the mirror adopts the PRODUCER's canonical run-start clock —
               ;; stamping mirror-local time desynced elapsed across processes
               (expect (= 777 (get-in started [:turns "T1" :started_at]))))
             (#'state/ingest-mirrored-event!
              sid
              false
              {:type "turn.completed"
               :turn_id "T1"
               :status "completed"
               :answer_md "the answer"
               :engine_turn_id "E1"})
             (let [done (get @reg sid)]
               (expect (nil? (:current-turn done)))
               (expect (= "completed" (get-in done [:turns "T1" :status])))
               (expect (= "E1" (get-in done [:turns "T1" :engine_turn_id])))
               (expect (= "the answer" (get-in done [:turns "T1" :answer_md]))))
             (finally (swap! reg dissoc sid))))
    (it "ignores mirrored events for a session this process never touched"
        (expect (nil? (#'state/ingest-mirrored-event!
                       "never-touched-sid"
                       false
                       {:type "turn.started" :turn_id "X" :request "hi"})))
        (expect (not (contains? @reg "never-touched-sid"))))))

(defdescribe
  queue-drain-mirror-event-test
  ;; Cross-channel queue sync: when the gateway auto-drains the queue head it
  ;; must emit `turn.queued.drained` BEFORE the turn starts, so every mirror
  ;; (TUI :sync-queued-turn) drops the entry and a replayed log nets to zero.
  (it "drain-next-queued! emits turn.queued.drained for the promoted head"
      (let [sid
            (str "drain-test-" (java.util.UUID/randomUUID))

            registry
            @#'state/registry

            launched
            (atom nil)]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :turns
                {"q1"
                 {:turn_id "q1" :session_id sid :status "queued" :request "hello" :queued_at 1}}
                :turn-order ["q1"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec (take 2 args))))}
               #(#'state/drain-next-queued! sid))
             (expect (= [sid "q1"] @launched))
             (expect (= "running" (:status (state/get-turn sid "q1"))))
             (let [events
                   (state/events-since sid 0)

                   drained
                   (filterv #(= "turn.queued.drained" (:type %)) events)]

               (expect (= 1 (count drained)))
               (expect (= "q1" (:turn_id (first drained)))))
             (finally (swap! registry dissoc sid))))))

(defdescribe
  delta-coalesce-test
  ;; model text phases are withheld from the gateway wire; coalescing still
  ;; guards the internal skip path from doing repeated work inside the window.
  (let [coalesce? @#'state/coalesce-delta?]
    (it "skips a reasoning delta inside the window"
        (expect (true? (coalesce? {:reasoning 1000} {:phase :reasoning} 1050))))
    (it "passes a reasoning delta once the window elapsed"
        (expect (false? (coalesce? {:reasoning 1000} {:phase :reasoning} 1101))))
    (it "a :done? frame always passes"
        (expect (false? (coalesce? {:reasoning 1000} {:phase :reasoning :done? true} 1050))))
    (it "phases track independent clocks"
        (expect (false? (coalesce? {:reasoning 1000} {:phase :content} 1050))))
    (it "tool phases always pass"
        (expect (false? (coalesce? {:reasoning 1000} {:phase :form-result} 1050))))
    (it "the window grows with the cumulative text size"
        ;; ~60KB of thinking -> ~234ms window: a frame at +150ms is
        ;; still coalesced, where a small frame (100ms floor) passes.
        (let [big (apply str (repeat 60000 "x"))]
          (expect (true? (coalesce? {:reasoning 1000} {:phase :reasoning :thinking big} 1150)))
          (expect (false?
                    (coalesce? {:reasoning 1000} {:phase :reasoning :thinking "tiny"} 1150)))))
    (it "the adaptive window caps at 1s so huge streams still tick"
        (let [huge (apply str (repeat 1000000 "x"))]
          (expect (false?
                    (coalesce? {:reasoning 1000} {:phase :reasoning :thinking huge} 2001)))))))

(defdescribe
  turn-stall-watchdog-test
  "A turn wedged with NO chunk activity past the backstop ceiling is
   force-cancelled: the shared cancellation token flips (which closes the
   in-flight stream) and the turn is flagged stalled so the queue can drain.
   This covers a stuck `:provider-call` AND the between-iteration
   `:iteration-final` gap. A legitimately long tool/eval phase is left untouched."
  (let [watchdog
        @#'state/start-turn-stall-watchdog!

        registry
        @#'state/registry

        await-cancel
        (fn [token ms]
          (let [deadline (+ (System/currentTimeMillis) (long ms))]
            (loop []

              (cond (cancellation/cancelled? token) true
                    (>= (System/currentTimeMillis) deadline) false
                    :else (do (Thread/sleep 25) (recur))))))]

    (it "force-cancels a turn stuck in :provider-call past the ceiling"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :provider-call :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               ;; await INSIDE with-redefs so the async watchdog thread reads the
               ;; lowered ceiling before with-redefs reverts it (alter-var-root is
               ;; global, not thread-local).
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 (expect (true? (await-cancel token 4000))))
               (expect (true? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "force-cancels a turn wedged in the between-iteration :iteration-final gap"
        ;; Regression: a turn that finished an iteration but hangs building the
        ;; next provider call (e.g. a blocked auth-header refresh) emits NO more
        ;; chunks and NO terminal event, so its phase stays :iteration-final.
        ;; The old `:provider-call`-only gate never caught it and the session
        ;; queue wedged forever.
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :iteration-final :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 (expect (true? (await-cancel token 4000))))
               (expect (true? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "leaves a turn alone while it runs a legitimately long tool/eval phase"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :tool-start :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 (expect (false? (await-cancel token 1200))))
               (expect (nil? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "leaves a turn alone once it is no longer the current turn"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :provider-call :last-ms (- (System/currentTimeMillis) 60000)})]

          (try
            ;; a DIFFERENT current turn than the one the watchdog guards
            (swap! registry assoc sid {:next-seq 0 :current-turn "other"})
            (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
              (watchdog sid "t1" token stall)
              (expect (false? (await-cancel token 1200))))
            (expect (nil? (:stalled? @stall)))
            (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))))
