(ns com.blockether.vis.internal.gateway.state-test
  "Wire-event projection. Form errors ship LEAN, single-surface:
   `block.output` carries human text (message + line/col + hint), never the
   pr-str'd op-error map (which nests host trace/data chains), and is omitted
   entirely when an errored op in the form's sink slice already renders the
   same failure as an op card — the web thread painted that failure twice."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.gateway.state :as state]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tool-error
  {:message "rg spec has unknown keys: spec."
   :data {:phase :python/host :type :vis/tool-failure :symbol :rg}})

(defdescribe thinking-newline-normalization-test
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
    (let [[type store? payload] (#'state/chunk->event
                                 {:phase :iteration-final
                                  :done? true
                                  :thinking " alpha\n\n\n beta  \n\t\n gamma "})]
      (expect (= "iteration.completed" type))
      (expect store?)
      (expect (= "alpha\n beta\n gamma" (:thinking payload))))))

(defdescribe form-result-error-wire-test
  (it "omits the block-level error when an errored op already carries the same failure"
    (let [[type _ payload] (#'state/chunk->event
                            {:phase :form-result :position 0 :code "rg(...)"
                             :error tool-error
                             :channel [{:position 0 :success? false
                                        :error {:message "rg spec has unknown keys: spec."
                                                :trace "clojure.lang.ExceptionInfo: ..."}}]})]
      (expect (= "block.output" type))
      (expect (nil? (:error payload)))))
  (it "ships a lean text error (message + line/col + hint), never the pr-str'd map"
    (let [[_ _ payload] (#'state/chunk->event
                         {:phase :form-result :position 0 :code "1/0"
                          :error {:message "ZeroDivisionError: division by zero"
                                  :hint "check denominator"
                                  :data {:phase :python/runtime :line 1 :column 1}}})]
      (expect (= "ZeroDivisionError: division by zero (line 1, col 1)\nhint: check denominator"
                (:error payload)))
      (expect (not (str/includes? (:error payload) ":data")))))
  (it "an errored op with a DIFFERENT message does not swallow the block error"
    (let [[_ _ payload] (#'state/chunk->event
                         {:phase :form-result :position 0 :code "x"
                          :error tool-error
                          :channel [{:position 0 :success? false
                                     :error {:message "some other failure"}}]})]
      (expect (= "rg spec has unknown keys: spec." (:error payload))))))

(defdescribe broadcast-title-poll-parity-test
  "A sibling-session title update must be STORED on every other registered
   session, so the /poll fallback (which reads the replay ring and never
   registers as a subscriber) delivers the identical frame the live SSE
   client gets. Previously the copy was live-only + gated on `:subscribers`,
   so poll clients silently missed it."
  (it "stores the sibling title event on a session with NO subscriber (poll-only)"
    (let [a        (java.util.UUID/randomUUID)
          b        (java.util.UUID/randomUUID)
          registry @#'state/registry
          saved    @registry]
      (try
        ;; b carries no :subscribers — exactly a client on the /poll fallback
        (reset! registry {a {:next-seq 0} b {:next-seq 0}})
        (#'state/broadcast-title-event! a "Tidal Forces")
        (let [a-events (state/events-since a 0)
              b-events (state/events-since b 0)]
          ;; the titled session keeps its own stored event
          (expect (= 1 (count a-events)))
          (expect (= "session.title_updated" (:type (first a-events))))
          ;; the sibling (poll-only) session ALSO has it stored → /poll sees it
          (expect (= 1 (count b-events)))
          (expect (= "session.title_updated" (:type (first b-events))))
          ;; foreign copy carries the TITLED session's id, not b's
          (expect (= (str a) (:session_id (first b-events)))))
        (finally (reset! registry saved))))))

(defdescribe provider-error-ir-answer-wire-test
  "An engine IR-AST answer (the provider-error / fatal-iteration fallback)
   must flatten to PLAIN TEXT for :answer_md — never a pr-str'd `[:ir …]`
   vector dumped into the chat bubble. The rich AST rides :answer_ir for
   the IR walker."
  (let [ir-answer [:ir {:vis/provider-error true}
                   [:h {:level 2} [:span {} "🚨 PROVIDER_ERROR"]]
                   [:p {} [:strong {} [:span {} "Provider call failed before the model could run."]]]
                   [:p {} [:strong {} [:span {} "WHAT HAPPENED: provider rejected the request before the model ran."]]]]]
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
