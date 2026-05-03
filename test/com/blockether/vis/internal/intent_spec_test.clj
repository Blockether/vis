(ns com.blockether.vis.internal.intent-spec-test
  (:require
   [clojure.spec.alpha :as s]
   [com.blockether.vis.internal.intent-spec :as intent-spec]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private canonical-ref "turn/3f2a91c0/iteration/4/block/2")

(defdescribe intent-spec-test
  (it "validates public writer opts with actual public key names"
    (expect (s/valid? ::intent-spec/issue-intent-opts
              {:title "Ship it" :rationale "User asked for it."}))
    (expect (s/valid? ::intent-spec/issue-plan-opts
              {:intent-id (random-uuid)
               :summary "Inspect, verify, answer."}))
    (let [intent-id (random-uuid)
          slot [intent-id :verification]]
      (expect (s/valid? ::intent-spec/issue-gate-opts
                {:plan-id (random-uuid)
                 :proposition "Verification passes."
                 :expected-proof {:slots {slot {:required? true}}
                                  :guard [:exists [:slot slot :ref]]}})))
    (expect (s/valid? ::intent-spec/relate-intents-opts
              {:from-intent-id (random-uuid)
               :to-intent-id (random-uuid)
               :relation :subintent})))

  (it "rejects accidental :id-based public opts for plan writers"
    (expect (false? (s/valid? ::intent-spec/issue-plan-opts
                      {:id (random-uuid) :summary "Wrong key."}))))

  (it "accepts canonical provenance refs and rejects compact aliases"
    (expect (s/valid? ::intent-spec/ref canonical-ref))
    (doseq [bad ["i4.2" "i4.2/tool" "E1" "G1"]]
      (expect (false? (s/valid? ::intent-spec/ref bad)))))

  (it "requires non-empty provenance refs for resolution writers"
    (expect (s/valid? ::intent-spec/prove-gate-opts
              {:summary "Targeted verification passed."
               :refs [canonical-ref]}))
    (expect (false? (s/valid? ::intent-spec/prove-gate-opts
                      {:summary "No evidence."
                       :refs []}))))

  (it "rejects presentation keys inside provenance maps"
    (expect (s/valid? ::intent-spec/provenance
              {:ref canonical-ref :op :sci/eval :status :done}))
    (expect (false? (s/valid? ::intent-spec/provenance
                      {:ref canonical-ref
                       :op :sci/eval
                       :status :done
                       :markdown "presentation does not belong here"}))))

  (it "requires provenance and namespaced rendering-kind on executed blocks"
    (expect (s/valid? ::intent-spec/executed-block
              {:idx 0
               :code "(+ 1 2)"
               :result 3
               :provenance {:ref canonical-ref :op :sci/eval :status :done}
               :rendering-kind :vis/sci}))
    (expect (false? (s/valid? ::intent-spec/executed-block
                      {:idx 0
                       :code "(+ 1 2)"
                       :result 3
                       :rendering-kind :sci/eval})))
    (expect (false? (s/valid? ::intent-spec/executed-block
                      {:idx 0
                       :code "(+ 1 2)"
                       :result 3
                       :provenance {:ref canonical-ref :op :sci/eval :status :done}}))))

  (it "validates the v/intents aggregate read shape"
    (expect (s/valid? ::intent-spec/intents-report
              {:ok? false
               :scope :conversation
               :conversation-id (random-uuid)
               :turn-state-id (random-uuid)
               :focused-intent-ids []
               :unfocused-active-intent-ids []
               :intents []
               :checks []
               :violations []
               :report "## Intents\n"}))))
