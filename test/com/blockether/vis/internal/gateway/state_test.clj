(ns com.blockether.vis.internal.gateway.state-test
  "Wire-event projection. Form errors ship LEAN, single-surface:
   `block.output` carries human text (message + line/col + hint), never the
   pr-str'd op-error map (which nests host trace/data chains), and is omitted
   entirely when an errored op in the form's sink slice already renders the
   same failure as an op card — the web thread painted that failure twice."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance :as persistance]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tool-error
  {:message "rg spec has unknown keys: spec."
   :data {:phase :python/host :type :vis/tool-failure :symbol :rg}})

(defdescribe
  thinking-newline-normalization-test
  "Gateway-owned thinking normalization keeps live SSE, poll/replay, and session
   consumers in sync. A client may still render defensively, but it must
   not be the first place where blank-line runs disappear."
  (it "streams reasoning deltas with normalized thinking over the gateway"
      (let
        [[type store? payload] (#'state/chunk->event
                                {:phase :reasoning
                                 :iteration 1
                                 :thinking " first  \n\n\t\nsecond\r\n\r\nthird  "
                                 :stream-block-id "t1:reasoning:1"
                                 :stream-delta "first\nsecond\nthird"})]
        (expect (= "content.block.delta" type))
        (expect store?)
        (expect (= "first\nsecond\nthird" (:text payload)))))
  (it "normalizes iteration-boundary thinking for pinned session history"
      (let
        [[type store? payload]
         (#'state/chunk->event
          {:phase :iteration-final :done? true :thinking " alpha\n\n\n beta  \n\t\n gamma "})]
        (expect (= "iteration.completed" type))
        (expect store?)
        (expect (= "alpha\n beta\n gamma" (:thinking payload)))))
  (it "normalizes persisted transcript thinking the same way as live events"
      (with-redefs
        [persistance/db-list-session-turns
         (fn [_ sid]
           [{:id sid :status :success}])

         persistance/db-list-session-turn-iterations
         (fn [_ _]
           [{:thinking " alpha\n\n beta  \n"}])]

        (expect (= "alpha\n beta"
                   (-> (state/transcript :session-1)
                       first
                       (get "iterations")
                       first
                       (get "thinking")))))))

(defdescribe
  form-result-error-wire-test
  ;; Op cards are gone, so there is no op-dedup: a form error ALWAYS surfaces
  ;; on the wire. What remains to assert is the lean text shape.
  (it "ships a lean text error (message + line/col + hint), never the pr-str'd map"
      (let
        [[_ _ payload] (#'state/chunk->event
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
      (let
        [[type _ payload] (#'state/chunk->event
                           {:phase :form-result :position 0 :code "rg(...)" :error tool-error})]
        (expect (= "block.output" type))
        (expect (= "rg spec has unknown keys: spec." (:error payload))))))

(defdescribe
  activity-wire-event-test
  "Coarse live-progress phases (provider wait, response parse, nested shell/tool
   call) ship as an EPHEMERAL `activity` event (store? false) so a long call
   never leaves the bubble frozen; nothing persists into the durable trace."
  (it "a nested tool-start ships an ephemeral activity event naming the op"
      (let
        [[type store? payload] (#'state/chunk->event
                                {:phase :tool-start :iteration 2 :tool-event {:op :shell_run}})]
        (expect (= "activity" type))
        (expect (false? store?))
        (expect (= {:activity "tool" :iteration 2 :op "shell_run"} payload))))
  (it "provider-call and shell-run project to ephemeral activity events"
      (expect (= ["activity" false {:activity "provider-call" :iteration 1}]
                 (#'state/chunk->event {:phase :provider-call :iteration 1})))
      (expect (= ["activity" false {:activity "shell-run" :iteration 1 :cmd "clojure -M:test"}]
                 (#'state/chunk->event {:phase :shell-run :iteration 1 :cmd "clojure -M:test"}))))
  (it "response-parse :done does NOT emit an activity event (the parse finished)"
      (let [[type] (#'state/chunk->event {:phase :response-parse :iteration 1 :status :done})]
        (expect (not= "activity" type)))))

(defdescribe form-event-iteration-wire-test
             ;; THE "live shows reasoning but no code" bug: every streaming chunk carries
             ;; its iteration POSITION under `:iteration`, and that MUST ride the wire
             ;; event — `make-progress-tracker` DROPS any chunk with no iteration, which is
             ;; how `block.started` / `block.output` once lost their forms and the live
             ;; bubble showed reasoning but no code.
             (it "block.started carries :iteration on the wire"
                 (let
                   [[type _ payload]
                    (#'state/chunk->event
                     {:phase :form-start :iteration 1 :position 0 :code "import hashlib"})]
                   (expect (= "block.started" type))
                   (expect (= 1 (:iteration payload)))))
             (it "block.output carries :iteration on the wire"
                 (let
                   [[type _ payload]
                    (#'state/chunk->event
                     {:phase :form-result :iteration 3 :position 0 :code "print(42)" :stdout "42"})]
                   (expect (= "block.output" type))
                   (expect (= 3 (:iteration payload)))))
             (it "reasoning streams as a replayable typed block delta"
                 (let
                   [[type store? payload] (#'state/chunk->event
                                           {:phase :reasoning
                                            :iteration 2
                                            :thinking "hmm"
                                            :stream-block-id "t1:reasoning:2"
                                            :stream-delta "hmm"})]
                   (expect (= "content.block.delta" type))
                   (expect store?)
                   (expect (= 2 (:iteration payload)))
                   (expect (= "t1:reasoning:2" (:block_id payload)))
                   (expect (= "text" (:field payload)))
                   (expect (= "hmm" (:text payload)))))
             (it "iteration-final carries :iteration and complete assistant prose on the wire"
                 (let
                   [[type _ payload] (#'state/chunk->event
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
      (let
        [[type _ payload] (#'state/chunk->event
                           {:phase :iteration-final :iteration 2 :done true :thinking "t"})]
        (expect (= "iteration.completed" type))
        (expect (not (contains? payload :attachments)))))
  (it "omits :attachments when there is no iteration-id to address them"
      (let
        [[_ _ payload] (#'state/chunk->event
                        {:phase :iteration-final :iteration 2 :done true :attachment-count 3})]
        (expect (not (contains? payload :attachments)))))
  (it "projects lean snake-case descriptors and NEVER leaks base64"
      (with-redefs
        [state/iteration-attachments (fn [_iid]
                                       [{:tool-call-id "call_A"
                                         :kind "image"
                                         :media-type "image/png"
                                         :filename "fig.png"
                                         :size 1234
                                         :base64 "SECRET"}
                                        {:tool-call-id nil :media-type "image/svg+xml" :size 0}])]
        (let
          [[_ _ payload] (#'state/chunk->event
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
  (it
    "stores the sibling title event on a session with NO subscriber (poll-only)"
    (let
      [a
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
        (let
          [a-events
           (state/events-since a 0)

           b-events
           (state/events-since b 0)]

          ;; the titled session keeps its own stored event
          (expect (= 1 (count a-events)))
          (expect (= "session.title_updated" (get (first a-events) "type")))
          ;; the sibling (poll-only) session ALSO has it stored → /poll sees it
          (expect (= 1 (count b-events)))
          (expect (= "session.title_updated" (get (first b-events) "type")))
          ;; foreign copy carries the TITLED session's id, not b's
          (expect (= (str a) (get (first b-events) "session_id"))))
        (finally (reset! registry saved))))))

(defdescribe canonical-answer-content-test
             (it "normalizes Markdown to one prose block"
                 (let [blocks (#'state/answer-content {:answer "## hello"})]
                   (expect (= 1 (count blocks)))
                   (expect (= "prose" (get-in blocks [0 "type"])))
                   (expect (= "## hello" (get-in blocks [0 "markdown"])))))
             (it "passes typed error content without creating a second answer shape"
                 (let
                   [blocks [{"id" "e1"
                             "type" "error"
                             "code" "provider_unavailable"
                             "message" "Provider failed"
                             "retryable" true}]]
                   (expect (= blocks (#'state/answer-content blocks))))))

(defdescribe
  list-turns-dedup-test
  "A refreshed web page hydrates from gateway/list-turns. Once the engine DB row
  exists, the completed gateway overlay row must disappear; otherwise the last
  request/response pair renders twice and the transient duplicate has no DB
  iterations disclosure."
  (it
    "prefers the persisted row over a matching completed live row with engine id"
    (let
      [sid
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
                                                      :content
                                                      [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                      :started_at 1000}}}})
           (with-redefs
             [persistance/db-list-session-turns (fn [_ _]
                                                  [{:id engine-id
                                                    :status :success
                                                    :user-request "hello"
                                                    :content
                                                    [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                    :iteration-count 2
                                                    :input-tokens 1200
                                                    :input-regular-tokens 500
                                                    :input-cache-write-tokens 100
                                                    :input-cache-read-tokens 600
                                                    :output-tokens 150
                                                    :output-reasoning-tokens 80
                                                    :total-cost 0.0123
                                                    :provider :openai
                                                    :model "gpt-4o"
                                                    :created-at (java.util.Date. 1010)}])]
             (let [turns (state/list-turns sid)]
               (expect (= 1 (count turns)))
               (let [turn (first turns)]
                 (expect (= (str engine-id) (get turn "turn_id")))
                 (expect (= 2 (get turn "iteration_count")))
                 (expect (= {"input" 1200
                             "input_regular" 500
                             "cache_created" 100
                             "cached" 600
                             "output" 150
                             "reasoning" 80}
                            (get turn "tokens")))
                 (expect (= {"total_cost" 0.0123 "provider" "openai" "model" "gpt-4o"}
                            (get turn "cost"))))))
           (finally (reset! registry saved)))))
  (it
    "prefers the persisted row over a matching completed live row with no engine id"
    (let
      [sid
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
                                                      :content
                                                      [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                      :started_at started}}}})
           (with-redefs
             [persistance/db-list-session-turns (fn [_ s]
                                                  (expect (= sid s))
                                                  [{:id engine-id
                                                    :status :success
                                                    :user-request "hello"
                                                    :content
                                                    [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                    :iteration-count 2
                                                    :created-at (java.util.Date. (+ started 10))}])]
             (let [turns (state/list-turns sid)]
               (expect (= 1 (count turns)))
               (expect (= (str engine-id) (get (first turns) "turn_id")))
               (expect (= 2 (get (first turns) "iteration_count")))))
           (finally (reset! registry saved))))))

(defdescribe
  queued-update-payload-test
  "Editing a queued request must also edit the provider message payload;
  otherwise the drained queued turn answers the pre-edit prompt."
  (it "replaces the last user message content"
      (let
        [messages [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                   {:role "assistant" :content "old answer"} {:role :user :content "queued old"}]]
        (expect (= [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                    {:role "assistant" :content "old answer"} {:role :user :content "queued new"}]
                   (#'state/replace-last-user-message-content messages "queued new"))))))

(defdescribe
  persisted-duplicate-of-live-test
  ;; Terminal identity is request + status + timestamps; content blocks belong
  ;; to the durable row and are not duplicated onto terminal events.
  (let
    [dup?
     #'state/persisted-duplicate-of-live?

     at
     (fn [ms]
       (java.util.Date. (long ms)))]

    (it "dedups an error turn whose live row has no answer to compare"
        (expect
          (dup?
            {:engine_turn_id nil :status "error" :request "add zprint" :content [] :started_at 1000}
            {:id "soul-1"
             :user-request "add zprint"
             :content
             [{"id" "e1" "type" "error" "code" "failed" "message" "Could not reach provider"}]
             :created-at (at 2000)})))
    (it "does NOT over-dedup two distinct completed answers with the same request"
        (expect (not (dup? {:engine_turn_id nil
                            :status "completed"
                            :request "hi"
                            :content [{"id" "a" "type" "prose" "markdown" "answer A"}]
                            :started_at 1000}
                           {:id "soul-2"
                            :user-request "hi"
                            :content [{"id" "b" "type" "prose" "markdown" "answer B"}]
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
  (let
    [reg
     @#'state/registry

     sid
     "mirror-test-sid"]

    (it "materializes a running row on turn.started, terminal on turn.completed"
        (try (swap! reg assoc sid {:next-seq 0})
             (#'state/ingest-mirrored-event!
              sid
              false
              {"type" "turn.started" "turn_id" "T1" "request" "hello world" "started_at" 777})
             (let [started (get @reg sid)]
               (expect (= "T1" (:current-turn started)))
               (expect (= ["T1"] (:turn-order started)))
               (expect (= "running" (get-in started [:turns "T1" :status])))
               (expect (= "hello world" (get-in started [:turns "T1" :request])))
               ;; The turn row owns its replay boundary. Channels must not scan
               ;; the event ring to rediscover the matching turn.started event.
               (expect (= 1 (get-in started [:turns "T1" :event_start_seq])))
               (expect (= 1 (get (state/get-turn sid "T1") "event_start_seq")))
               ;; the mirror adopts the PRODUCER's canonical run-start clock —
               ;; stamping mirror-local time desynced elapsed across processes
               (expect (= 777 (get-in started [:turns "T1" :started_at]))))
             (#'state/ingest-mirrored-event!
              sid
              false
              {"type" "turn.completed" "turn_id" "T1" "status" "completed" "engine_turn_id" "E1"})
             (let [done (get @reg sid)]
               (expect (nil? (:current-turn done)))
               (expect (= "completed" (get-in done [:turns "T1" :status])))
               (expect (= "E1" (get-in done [:turns "T1" :engine_turn_id]))))
             (finally (swap! reg dissoc sid))))
    (it "ignores mirrored events for a session this process never touched"
        (expect (nil? (#'state/ingest-mirrored-event!
                       "never-touched-sid"
                       false
                       {"type" "turn.started" "turn_id" "X" "request" "hi"})))
        (expect (not (contains? @reg "never-touched-sid"))))))

(defdescribe
  queue-drain-mirror-event-test
  (it "broadcasts queue drain live without adding it to replay persistence"
      (let
        [sid
         (str "drain-test-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom nil)

         seen
         (atom [])]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :subscribers {"test" #(swap! seen conj %)}
                :turns
                {"q1"
                 {:turn_id "q1" :session_id sid :status "queued" :request "hello" :queued_at 1}}
                :turn-order ["q1"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec (take 2 args))))}
               #(#'state/drain-next-queued! sid))
             (expect (= [sid "q1"] @launched))
             (expect (= "streaming" (get (state/get-turn sid "q1") "status")))
             (expect (= ["turn.queued.drained"] (mapv #(get % "type") @seen)))
             (expect (empty? (state/events-since sid 0)))
             (finally (swap! registry dissoc sid))))))

(defdescribe
  drain-idle-test
  ;; Auto-start on open/resume: an attaching channel calls `drain-idle!` to kick
  ;; an orphaned queued backlog into motion — but ONLY when the session is idle.
  ;; A turn already in flight must be left alone (one engine turn per session).
  (it "drain-idle! starts the queued head when the session is idle"
      (let
        [sid
         (str "drain-idle-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom nil)]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :turns {"q1"
                        {:turn_id "q1" :session_id sid :status "queued" :request "hi" :queued_at 1}}
                :turn-order ["q1"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec (take 2 args))))}
               #(state/drain-idle! sid))
             (expect (= [sid "q1"] @launched))
             (expect (= "streaming" (get (state/get-turn sid "q1") "status")))
             (finally (swap! registry dissoc sid)))))
  (it "drain-idle! is a no-op while a turn is already running"
      (let
        [sid
         (str "drain-idle-busy-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom nil)

         result
         (atom :unset)]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :current-turn "r0"
                :turns {"r0" {:turn_id "r0" :session_id sid :status "running" :request "run"}
                        "q1"
                        {:turn_id "q1" :session_id sid :status "queued" :request "hi" :queued_at 1}}
                :turn-order ["r0" "q1"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec (take 2 args))))}
               #(reset! result (state/drain-idle! sid)))
             (expect (nil? @result))
             (expect (nil? @launched))
             (expect (= "queued" (get (state/get-turn sid "q1") "status")))
             (finally (swap! registry dissoc sid))))))

(defdescribe
  delta-coalesce-test
  ;; Model text phases stream LIVE but coalesced to SENTENCE granularity: a frame
  ;; is skipped only while still mid-sentence AND within the time cap. A closed
  ;; sentence, the cap, a `:done?` frame, and the first frame of a phase all pass.
  ;; `last-emit` is [phase iteration] -> {:ms emit-epoch :len emitted-text-length}.
  (let [coalesce? @#'state/coalesce-delta?]
    (it "skips a mid-sentence reasoning delta inside the time cap"
        (expect (true? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                  {:phase :reasoning :thinking "still going"}
                                  1500))))
    (it "passes once a sentence closes, even inside the time cap"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :reasoning :thinking "done here. "}
                                   1500))))
    (it "passes once the time cap elapses, even mid-sentence"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :reasoning :thinking "still going"}
                                   3001))))
    (it "a :done? frame always passes"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :reasoning :thinking "x" :done? true}
                                   1500))))
    (it "the FIRST frame of a phase always passes (no prior emit)"
        (expect (false? (coalesce? {} {:phase :reasoning :thinking "still going"} 1500))))
    (it "only a sentence in the NEW suffix (past :len) flushes"
        ;; the '.' sits BEFORE :len — already emitted — so the fresh tail is
        ;; mid-sentence and coalesces inside the cap.
        (expect (true? (coalesce? {[:reasoning 0] {:ms 1000 :len 5}}
                                  {:phase :reasoning :thinking "done. more"}
                                  1500)))
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 5}}
                                   {:phase :reasoning :thinking "done. more!"}
                                   1500))))
    (it "phases track independent clocks"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :content :content "fresh"}
                                   1050))))
    (it "tool phases always pass"
        (expect (false?
                  (coalesce? {[:reasoning 0] {:ms 1000 :len 0}} {:phase :form-result} 1050))))))

(defdescribe volatile-queue-reconciliation-test
             (it "marks orphaned running turns interrupted without reconstructing messages"
                 (let [sweeps (atom 0)]
                   (with-redefs
                     [lp/db-sweep-orphaned-running-turns! (fn []
                                                            (swap! sweeps inc)
                                                            :swept)]
                     (expect (= :swept (state/reconcile-orphaned-turns!)))
                     (expect (= 1 @sweeps))))))

(defdescribe
  turn-stall-watchdog-test
  "A turn wedged with NO chunk activity past the backstop ceiling is
   force-cancelled: the shared cancellation token flips (which closes the
   in-flight stream) and the turn is flagged stalled so the queue can drain.
   This covers a stuck `:provider-call` AND the between-iteration
   `:iteration-final` gap. A legitimately long tool/eval phase is left untouched."
  (let
    [watchdog
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
        (let
          [sid
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
        (let
          [sid
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
        (let
          [sid
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
        (let
          [sid
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

(defdescribe
  gateway-prewarm-pool-test
  (it
    "adopts a ready session and requests an asynchronous refill"
    (let
      [pool
       @#'state/prewarm-pool

       prior
       @pool

       sid
       (java.util.UUID/randomUUID)

       refills
       (atom [])]

      (try (reset! pool
             {:ready
              {:api [{:id sid :channel :api :title nil :external-id nil :workspace-id :workspace}]}
              :in-flight {}
              :accepting? true})
           (with-redefs
             [state/ensure-prewarmed!
              #(swap! refills conj %)

              state/claim-prewarmed!
              (fn [session title]
                (assoc session :title title))]

             (let [created (state/create-session! {:channel :api :title "Ready"})]
               (expect (= (str sid) (get created "id")))
               (expect (= "Ready" (get created "title")))
               (expect (= [:api] @refills))
               (expect (empty? (get-in @pool [:ready :api])))))
           (finally (reset! pool prior)))))
  (it "bypasses the pool for a purpose-built workspace"
      (let
        [pool
         @#'state/prewarm-pool

         prior
         @pool

         pooled-id
         (java.util.UUID/randomUUID)

         cold-id
         (java.util.UUID/randomUUID)

         cold-calls
         (atom [])]

        (try (reset! pool {:ready {:api [{:id pooled-id :channel :api}]}
                           :in-flight {}
                           :accepting? true})
             (with-redefs
               [state/create-session-cold!
                (fn [opts]
                  (swap! cold-calls conj opts)
                  {:id cold-id :channel :api :workspace-id (:workspace-id opts)})]
               (let [created (state/create-session! {:channel :api :workspace-id :branch})]
                 (expect (= (str cold-id) (get created "id")))
                 (expect (= [:branch] (mapv :workspace-id @cold-calls)))
                 (expect (= pooled-id (get-in @pool [:ready :api 0 :id])))))
             (finally (reset! pool prior))))))
