(ns com.blockether.vis.internal.proof-test
  (:require
   [clojure.spec.alpha :as s]
   [com.blockether.vis.internal.proof :as proof]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private canonical-ref "turn/3f2a91c0/iteration/4/block/2")

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
