(ns com.blockether.vis.internal.gateway.state-test
  "Wire-event projection. Form errors ship LEAN, single-surface:
   `block.output` carries human text (message + line/col + hint), never the
   pr-str'd op-error map (which nests host trace/data chains), and is omitted
   entirely when an errored op in the form's sink slice already renders the
   same failure as an op card — the web thread painted that failure twice."
  (:require [clojure.string :as str]
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
  (it "normalizes streamed reasoning deltas before they enter the event log"
      (let [[type store? payload] (#'state/chunk->event
                                   {:phase :reasoning
                                    :text " first  \n\n\t\nsecond\r\n\r\nthird  "})]
        (expect (= "reasoning.delta" type))
        (expect (false? store?))
        (expect (= "first\nsecond\nthird" (:text payload)))))
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
  (it "reasoning.delta carries :iteration on the wire"
      (let [[type _ payload] (#'state/chunk->event {:phase :reasoning :iteration 2 :text "hmm"})]
        (expect (= "reasoning.delta" type))
        (expect (= 2 (:iteration payload)))))
  (it "iteration-final carries :iteration on the wire"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :iteration-final :iteration 5 :done true :thinking "t"})]
        (expect (= "iteration.completed" type))
        (expect (= 5 (:iteration payload))))))

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
             (let [dup? #'state/persisted-duplicate-of-live?
                   at   (fn [ms] (java.util.Date. (long ms)))]
               (it "dedups an error turn whose live row has no answer to compare"
                   (expect (dup? {:engine_turn_id nil :status "error" :request "add zprint"
                                  :answer_md "" :started_at 1000}
                                 {:id "soul-1" :user-request "add zprint"
                                  :answer-markdown "## Could not reach provider" :created-at (at 2000)})))
               (it "does NOT over-dedup two distinct completed answers with the same request"
                   (expect (not (dup? {:engine_turn_id nil :status "completed" :request "hi"
                                       :answer_md "answer A" :started_at 1000}
                                      {:id "soul-2" :user-request "hi"
                                       :answer-markdown "answer B" :created-at (at 2000)}))))
               (it "still matches on the engine-turn-id primary key"
                   (expect (dup? {:engine_turn_id "eng-9" :status "completed"} {:id "eng-9"})))))
