(ns com.blockether.vis.internal.proof-test
  (:require
   [clojure.spec.alpha :as s]
   [com.blockether.vis.internal.proof :as proof]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private canonical-ref "turn/3f2a91c0/iteration/4/block/2")
(def ^:private intent-id #uuid "11111111-1111-1111-1111-111111111111")

(defn- successful-tool-event
  ([] (successful-tool-event {:result {:exit 0
                                       :stdout "ok"
                                       :tags ["verified" "fresh"]}}))
  ([payload]
   {:event/ref canonical-ref
    :event/status :done
    :event/kind :tool
    :event/op :v/bash
    :event/rendering-kind :vis/tool
    :event/payload payload}))

(defn- verification-requirement
  ([] (verification-requirement {}))
  ([overrides]
   (merge {:evidence/slot [intent-id :verification-exit]
           :evidence/from-ref canonical-ref
           :evidence/extract [:result :exit]
           :evidence/guard [:= [:value] 0]
           :event/kind :tool
           :event/op :v/bash}
     overrides)))

(defdescribe proof-domain-test
  (it "owns canonical ref parsing, formatting, and compact-alias rejection"
    (expect (= {:scope :turn
                :turn-prefix "3f2a91c0"
                :iteration 4
                :block 2}
              (proof/parse-ref canonical-ref)))
    (expect (= "turn/3f2a91c0/iteration/4/block/2/tool/v.bash"
              (proof/format-ref {:turn-prefix "3F2A91C0"
                                 :iteration 4
                                 :block 2
                                 :child {:kind :tool :op "v.bash"}})))
    (doseq [ref ["i4.2" "i4.2/tool" "E1" "G1" "turn/nope/iteration/1/block/1"]]
      (expect (nil? (proof/parse-ref ref)))
      (expect (false? (proof/canonical-ref? ref)))))

  (it "keeps running work out of proof and blocker compatibility"
    (let [started (proof/start-event
                    {:ref "turn/3f2a91c0/iteration/4/block/2/tool/job-7"
                     :parent-ref canonical-ref
                     :op :v/async-tool
                     :rendering-kind :vis/tool})
          done (proof/finish-event started {:status :done})
          timeout (proof/finish-event started {:status :timeout})]
      (expect (= :running (get-in started [:provenance :status])))
      (expect (false? (proof/proof-compatible? started)))
      (expect (false? (proof/blocker-compatible? started)))
      (expect (true? (proof/proof-compatible? done)))
      (expect (false? (proof/proof-compatible? timeout)))
      (expect (true? (proof/blocker-compatible? timeout)))))

  (it "specs proof events and terminal/successful event subsets"
    (let [base {:event/ref canonical-ref
                :event/status :done
                :event/op :sci/eval
                :event/rendering-kind :vis/sci}
          running (assoc base :event/status :running)]
      (expect (s/valid? ::proof/event base))
      (expect (s/valid? ::proof/terminal-event base))
      (expect (s/valid? ::proof/successful-event base))
      (expect (s/valid? ::proof/event running))
      (expect (false? (s/valid? ::proof/terminal-event running)))
      (expect (false? (s/valid? ::proof/successful-event running))))
    (expect (false? (s/valid? ::proof/event
                      {:event/ref "i4.2"
                       :event/status :done
                       :event/op :sci/eval
                       :event/rendering-kind :vis/sci}))))

  (it "evaluates guards over derived binding values only"
    (let [binding {:evidence/value 0
                   :evidence/event (successful-tool-event)}]
      (expect (true? (proof/evaluate-guard [:= [:value] 0] binding)))
      (expect (true? (proof/evaluate-guard [:and [:= [:value] 0] [:exists [:event :event/payload :result :stdout]]] binding)))
      (expect (true? (proof/evaluate-guard [:contains [:event :event/payload :result :tags] "verified"] binding)))
      (expect (false? (proof/evaluate-guard [:= [:value] 1] binding)))
      (expect (false? (proof/evaluate-guard [:eval "arbitrary code"] binding)))))

  (it "derives bindings from runtime payloads and ignores fake caller slot payloads"
    (let [event (successful-tool-event {:result {:exit 1}})
          fake-requirement (verification-requirement {:evidence/value 0})
          binding (proof/derive-binding fake-requirement event)]
      (expect (= 1 (:evidence/value binding)))
      (expect (= :guard-false (:evidence/error-code binding)))
      (expect (false? (:evidence/guard-ok binding)))
      (expect (false? (:gate/proven? (proof/evaluate-gate [event] [fake-requirement]))))))

  (it "rejects fake extension aggregate proof payloads as mutable sidecar state"
    (let [event (assoc (successful-tool-event {:proof {:gate-passed true}
                                               :result {:exit 0}})
                  :event/kind :extension/aggregate
                  :event/op :extension/write-aggregate)
          result (proof/evaluate-gate [event] [(verification-requirement)])]
      (expect (false? (:gate/proven? result)))
      (expect (= :mutable-extension-state (get-in result [:gate/errors 0 :evidence/error-code])))))

  (it "rejects compact refs before they can prove a gate"
    (let [event (assoc (successful-tool-event) :event/ref "i4.2")
          result (proof/evaluate-gate [event] [(verification-requirement {:evidence/from-ref "i4.2"})])]
      (expect (false? (:gate/proven? result)))
      (expect (= :invalid-requirement (get-in result [:gate/errors 0 :evidence/error-code])))))

  (it "rejects running events as proof"
    (let [event (assoc (successful-tool-event) :event/status :running)
          result (proof/evaluate-gate [event] [(verification-requirement)])]
      (expect (false? (:gate/proven? result)))
      (expect (= :non-successful-event (get-in result [:gate/errors 0 :evidence/error-code])))))

  (it "rejects wrong event kind and op mismatches"
    (let [wrong-kind (assoc (successful-tool-event) :event/kind :answer)
          wrong-op (assoc (successful-tool-event) :event/op :v/read)
          kind-result (proof/evaluate-gate [wrong-kind] [(verification-requirement)])
          op-result (proof/evaluate-gate [wrong-op] [(verification-requirement)])]
      (expect (false? (:gate/proven? kind-result)))
      (expect (= :event-kind-mismatch (get-in kind-result [:gate/errors 0 :evidence/error-code])))
      (expect (false? (:gate/proven? op-result)))
      (expect (= :event-op-mismatch (get-in op-result [:gate/errors 0 :evidence/error-code])))))

  (it "rejects missing extraction paths and false guards"
    (let [missing-result (proof/evaluate-gate [(successful-tool-event {:result {:stdout "no exit"}})]
                           [(verification-requirement)])
          false-result (proof/evaluate-gate [(successful-tool-event {:result {:exit 2}})]
                         [(verification-requirement)])]
      (expect (false? (:gate/proven? missing-result)))
      (expect (= :missing-extract (get-in missing-result [:gate/errors 0 :evidence/error-code])))
      (expect (false? (:gate/proven? false-result)))
      (expect (= :guard-false (get-in false-result [:gate/errors 0 :evidence/error-code])))))

  (it "accepts a gate only after every required binding derives from successful events"
    (let [result (proof/evaluate-gate [(successful-tool-event)] [(verification-requirement)])]
      (expect (true? (:gate/proven? result)))
      (expect (= :accepted (:attestation/status result)))
      (expect (= :proven (:attestation/decision result)))
      (expect (= 0 (get-in result [:bundle/bindings 0 :evidence/value])))
      (expect (empty? (:gate/errors result)))))

  (it "specs evidence clauses as runtime-derived requests, not caller facts"
    (let [intent-id (random-uuid)
          clause {:evidence/slot [intent-id :verification-exit]
                  :evidence/from-ref canonical-ref
                  :evidence/extract [:result :exit]
                  :evidence/guard [:= [:value] 0]
                  :event/kind :tool
                  :event/op :v/bash}]
      (expect (s/valid? ::proof/evidence-requirement clause))
      (expect (s/valid? ::proof/derived-binding
                (assoc clause :evidence/value 0 :evidence/guard-ok true)))
      (expect (false? (s/valid? ::proof/evidence-requirement
                        (assoc clause :evidence/from-ref "i4.2"))))
      (expect (false? (s/valid? ::proof/evidence-requirement
                        (assoc clause :evidence/guard [:eval "arbitrary code"]))))))

  (it "specs bundles, attestations, resolutions, and audit reports"
    (let [intent-id (random-uuid)
          gate-id (random-uuid)
          bundle-id (random-uuid)
          attestation-id (random-uuid)
          binding {:evidence/slot [intent-id :verification-exit]
                   :evidence/from-ref canonical-ref
                   :evidence/extract [:result :exit]
                   :evidence/value 0
                   :evidence/guard [:= [:value] 0]
                   :evidence/guard-ok true}
          bundle {:bundle/id bundle-id
                  :bundle/kind :proof
                  :bundle/subject-kind :gate
                  :bundle/subject-id gate-id
                  :bundle/source :derived
                  :bundle/bindings [binding]}
          attestation {:attestation/id attestation-id
                       :attestation/kind :gate/proven
                       :attestation/subject-kind :gate
                       :attestation/subject-id gate-id
                       :attestation/evidence-bundle-id bundle-id
                       :attestation/decision :proven
                       :attestation/status :accepted}
          resolution {:resolution/subject-kind :gate
                      :resolution/subject-id gate-id
                      :resolution/status :proven
                      :resolution/attestation-id attestation-id
                      :resolution/bundle-id bundle-id}
          audit {:audit/success? true
                 :audit/violations []
                 :audit/report "ok"}]
      (expect (s/valid? ::proof/evidence-bundle bundle))
      (expect (s/valid? ::proof/attestation attestation))
      (expect (s/valid? ::proof/gate-resolution resolution))
      (expect (false? (s/valid? ::proof/plan-resolution resolution)))
      (expect (s/valid? ::proof/audit-result audit)))))
