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
      ;; Compact display aliases must surface the precise `:non-canonical-ref`
      ;; error code instead of being swallowed by the spec catch-all.
      ;; This is the bug the persistence ref-rejection regression hit when
      ;; it saw `:invalid-requirement` instead of
      ;; the expected canonical-ness diagnostic. The runtime branch in
      ;; `derive-binding` is the source of truth for ref shape; the spec
      ;; on `:evidence/from-ref` no longer pre-empts it.
      (expect (= :non-canonical-ref (get-in result [:gate/errors 0 :evidence/error-code])))))

  (it "still surfaces :invalid-requirement for genuinely malformed requirements"
    ;; Sanity: when the ref is canonical but a required key is missing, the
    ;; spec catch-all must still fire so authors get a structural error.
    (let [event (successful-tool-event)
          ;; Drop :evidence/slot to break the spec, keep canonical from-ref.
          bad-req (dissoc (verification-requirement) :evidence/slot)
          result (proof/evaluate-gate [event] [bad-req])]
      (expect (false? (:gate/proven? result)))
      (expect (= :invalid-requirement (get-in result [:gate/errors 0 :evidence/error-code])))))

  (it "rejects empty requirements as a fake-proof bypass (PROOF.md trust spine)"
    ;; A bundle with zero requirements would otherwise be trivially proven
    ;; (`every? guard-ok []` = true). PROOF.md's evidence-bundle layer must
    ;; not let a caller satisfy a gate by pointing at no evidence at all.
    ;; Reproduces the persistence-side fake-proof bypass found while finishing
    ;; the attestation ledger work: `(v/db-create-evidence-bundle! ... :requirements [])`
    ;; combined with `db-attest-gate!` could prove ANY gate.
    (let [result (proof/evaluate-gate [(successful-tool-event)] [])]
      (expect (false? (:gate/proven? result)))
      (expect (= :rejected (:attestation/status result)))
      (expect (= :impeded (:attestation/decision result)))
      (expect (= [] (:bundle/bindings result)))
      (expect (= :empty-requirements (get-in result [:gate/errors 0 :evidence/error-code]))))
    ;; Empty-requirements rejection holds even with NO events present.
    (let [result (proof/evaluate-gate [] [])]
      (expect (false? (:gate/proven? result)))
      (expect (= :empty-requirements (get-in result [:gate/errors 0 :evidence/error-code])))))

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

;; ---------------------------------------------------------------------------
;; PROOF.md Task 28 — intent lifecycle vocabulary specs
;;
;; The intent tree decisions in ADR-0003 add suggested / deferred states, an
;; explicit acceptance/resume actor model that excludes extensions, a tiny
;; defer-trigger vocabulary, sibling policy, abandonment scope, and a strict
;; transition table. These tests lock all of that in at the data-shape level
;; before any persistence work can drift away from it.
;; ---------------------------------------------------------------------------

(def ^:private intent-uuid "22222222-2222-2222-2222-222222222222")
(def ^:private parent-uuid "33333333-3333-3333-3333-333333333333")
(def ^:private conversation-uuid "44444444-4444-4444-4444-444444444444")

(defn- valid-intent
  ([] (valid-intent {}))
  ([overrides]
   (merge {:intent/id intent-uuid
           :intent/conversation-id conversation-uuid
           :intent/title "Configure telegram bot"
           :intent/rationale "User asked to add Telegram channel"
           :intent/status :suggested
           :intent/source :extension
           :intent/owner-extension-id "vis.ext.telegram"}
     overrides)))

(defdescribe intent-lifecycle-vocabulary-test
  (it "enumerates intent lifecycle states with `:active` as the only blocker"
    (expect (= #{:suggested :deferred :active :fulfilled :abandoned}
              proof/intent-statuses))
    (expect (= #{:active} proof/intent-blocking-statuses))
    (expect (= #{:fulfilled :abandoned} proof/intent-resolved-statuses))
    (expect (= #{:suggested :deferred} proof/intent-pending-commitment-statuses)))

  (it "caps intent acceptance/resume actors so extensions cannot self-accept"
    ;; PROOF.md Task 28: only `:user` and `:system-policy` may accept
    ;; suggestions or resume deferred intents. The presence of `:extension`
    ;; in this set would be the silent-self-acceptance bypass the ADR
    ;; explicitly forbids.
    (expect (= #{:user :system-policy} proof/intent-acceptance-actor-kinds))
    (expect (false? (proof/intent-acceptance-actor-kind? :extension)))
    (expect (false? (proof/intent-acceptance-actor-kind? nil)))
    (expect (true? (proof/intent-acceptance-actor-kind? :user)))
    (expect (true? (proof/intent-acceptance-actor-kind? :system-policy))))

  (it "enumerates the minimal Defer Trigger and Sibling Policy vocabulary"
    (expect (= #{:defer/user-input :defer/time :defer/extension-signal :defer/intent}
              proof/defer-trigger-kinds))
    (expect (= #{:defer/continue-siblings :defer/block-parent}
              proof/defer-sibling-policies))
    (expect (true? (proof/defer-trigger-kind? :defer/user-input)))
    (expect (false? (proof/defer-trigger-kind? :defer/random-thing))))

  (it "enumerates Abandonment Scope choices (current-intent, branch, all-running)"
    (expect (= #{:abandon/current-intent :abandon/current-branch :abandon/all-running}
              proof/abandonment-scopes))
    (expect (true? (proof/abandonment-scope? :abandon/current-branch)))
    (expect (false? (proof/abandonment-scope? :abandon/whatever))))

  (it "validates the intent shape against ::proof/intent"
    (expect (true? (s/valid? ::proof/intent (valid-intent))))
    ;; Status must be one of the allowed lifecycle states.
    (expect (false? (s/valid? ::proof/intent (valid-intent {:intent/status :weird}))))
    ;; Source must be enumerated.
    (expect (false? (s/valid? ::proof/intent (valid-intent {:intent/source :random}))))
    ;; Title and rationale must be non-blank.
    (expect (false? (s/valid? ::proof/intent (valid-intent {:intent/title ""})))))

  (it "validates defer / resume / abandonment / acceptance decision shapes"
    (expect (true? (s/valid? ::proof/defer-decision
                     {:intent/id intent-uuid
                      :intent/defer-trigger-kind :defer/user-input
                      :intent/defer-sibling-policy :defer/block-parent})))
    (expect (true? (s/valid? ::proof/resume-decision
                     {:intent/id intent-uuid
                      :intent/resumed-by-kind :user
                      :intent/resumed-by-id "alice"})))
    (expect (true? (s/valid? ::proof/abandonment-decision
                     {:intent/id intent-uuid
                      :intent/abandonment-scope :abandon/current-branch
                      :intent/abandonment-reason "user changed direction"})))
    (expect (true? (s/valid? ::proof/acceptance-decision
                     {:intent/id intent-uuid :intent/accepted-by-kind :user})))
    ;; A resume decision with `:extension` actor is shape-invalid.
    (expect (false? (s/valid? ::proof/resume-decision
                      {:intent/id intent-uuid :intent/resumed-by-kind :extension}))))

  (it "enforces the intent lifecycle transition table"
    ;; Creation transitions.
    (expect (true? (proof/legal-intent-transition? nil :suggested)))
    (expect (true? (proof/legal-intent-transition? nil :active)))
    (expect (true? (proof/legal-intent-transition? nil :deferred)))
    (expect (false? (proof/legal-intent-transition? nil :fulfilled)))
    (expect (false? (proof/legal-intent-transition? nil :abandoned)))
    ;; Suggested moves into a commitment or abandonment, never directly to
    ;; fulfilled (that requires an `:active` plan first).
    (expect (true? (proof/legal-intent-transition? :suggested :active)))
    (expect (true? (proof/legal-intent-transition? :suggested :deferred)))
    (expect (true? (proof/legal-intent-transition? :suggested :abandoned)))
    (expect (false? (proof/legal-intent-transition? :suggested :fulfilled)))
    ;; Deferred can only become active or abandoned (not back to suggested).
    (expect (true? (proof/legal-intent-transition? :deferred :active)))
    (expect (true? (proof/legal-intent-transition? :deferred :abandoned)))
    (expect (false? (proof/legal-intent-transition? :deferred :suggested)))
    (expect (false? (proof/legal-intent-transition? :deferred :fulfilled)))
    ;; Active can defer, fulfill, or abandon — not jump back to suggested.
    (expect (true? (proof/legal-intent-transition? :active :deferred)))
    (expect (true? (proof/legal-intent-transition? :active :fulfilled)))
    (expect (true? (proof/legal-intent-transition? :active :abandoned)))
    (expect (false? (proof/legal-intent-transition? :active :suggested)))
    ;; Terminal states cannot transition. Closing this loophole is the whole
    ;; reason `db-attest-intent!` requires evidence; the lifecycle spec
    ;; mirrors that invariant at the pure layer.
    (doseq [from #{:fulfilled :abandoned}
            to   proof/intent-statuses]
      (expect (false? (proof/legal-intent-transition? from to))))))

(defdescribe intent-cursor-spec-test
  (it "specs intent cursor as one row per conversation"
    (expect (true? (s/valid? ::proof/intent-cursor
                     {:intent-cursor/conversation-id conversation-uuid
                      :intent-cursor/intent-id intent-uuid})))
    (expect (true? (s/valid? ::proof/intent-cursor
                     {:intent-cursor/conversation-id conversation-uuid
                      :intent-cursor/intent-id nil})))
    (expect (false? (s/valid? ::proof/intent-cursor
                      {:intent-cursor/intent-id intent-uuid})))))

(defdescribe intent-attestation-vocab-test
  (it "adds intent lifecycle attestation kinds without dropping legacy ones"
    (let [legacy #{:gate/proven :gate/impeded :plan/completed :plan/blocked
                   :intent/fulfilled :intent/abandoned}
          new    #{:intent/suggested :intent/accepted :intent/deferred :intent/resumed}]
      (expect (every? proof/attestation-kinds legacy))
      (expect (every? proof/attestation-kinds new))))

  (it "adds matching attestation decisions and includes :system-policy in attesters"
    (expect (contains? proof/attestation-decisions :accepted))
    (expect (contains? proof/attestation-decisions :deferred))
    (expect (contains? proof/attestation-decisions :resumed))
    (expect (contains? proof/attester-kinds :system-policy))))

(defdescribe intent-tree-parent-test
  (it "a child intent points at its parent via :intent/parent-intent-id"
    (let [child (valid-intent {:intent/parent-intent-id parent-uuid})]
      (expect (true? (s/valid? ::proof/intent child)))
      (expect (= parent-uuid (:intent/parent-intent-id child))))))
