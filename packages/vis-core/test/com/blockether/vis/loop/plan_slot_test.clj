(ns com.blockether.vis.loop.plan-slot-test
  "Phase 1 + Phase 0b — plan-as-first-class-slot.

   Tests for the projection layer (build-iteration-context), the persistence
   round-trip (store-iteration! → db-list-query-iterations), the sticky-carry
   semantics, plan-diff computation, and the cross-field validator. None of
   these tests touch the LLM — every fixture feeds synthetic spec-parsed
   maps directly into the loop helpers."
  (:require
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.test-helpers :as h]
   [lazytest.core :refer [defdescribe it expect]]))

(h/use-mem-store!)

;; =============================================================================
;; Helpers — synthesize a query + N iterations directly via store-iteration!,
;; then read back through the public DB facade. No LLM involvement.
;; =============================================================================

(defn- bootstrap-conversation+query!
  "Returns {:store … :conv-id … :query-id …}."
  [store]
  (let [conv-id  (db/store-conversation! store {:channel :vis :title "test"})
        query-id (db/store-query! store
                   {:parent-conversation-id conv-id
                    :query "test query"
                    :status :running})]
    {:store store :conv-id conv-id :query-id query-id}))

(defn- store-iteration!
  "Store a synthetic iteration row with explicit plan/breadcrumb fields."
  [store query-id {:keys [plan-state breadcrumb plan-diff thinking expressions]
                   :or {expressions []}}]
  (db/store-iteration! store
    (cond-> {:query-id    query-id
             :expressions expressions
             :duration-ms 100
             :llm-model   "test-model"
             :metadata    {}}
      plan-state (assoc :plan-state plan-state)
      breadcrumb (assoc :breadcrumb breadcrumb)
      plan-diff  (assoc :plan-diff plan-diff)
      thinking   (assoc :thinking thinking))))

;; =============================================================================
;; Persistence round-trip
;; =============================================================================

(defdescribe persistence-round-trip-test
  (it "stores plan_state, breadcrumb, plan_diff and reads them back"
    (let [{:keys [store query-id]} (bootstrap-conversation+query! (h/store))
          plan {:goal  "do the thing"
                :items [{:id 1 :content "first" :status :in_progress}
                        {:id 2 :content "second" :status :pending}]
                :open  ["q1?"]
                :decided ["rejected approach X"]}]
      (store-iteration! store query-id
        {:plan-state plan
         :breadcrumb "i0  decomposed task; starting [1]"
         :plan-diff  {:added [1 2] :removed [] :status-changed [] :goal-changed? true}})
      (let [iters (db/db-list-query-iterations store query-id)]
        (expect (= 1 (count iters)))
        (expect (= plan (:plan-state (first iters))))
        (expect (= "i0  decomposed task; starting [1]" (:breadcrumb (first iters))))
        (expect (= [1 2] (:added (:plan-diff (first iters))))))))

  (it "leaves columns nil when caller omits plan keys (legacy path)"
    (let [{:keys [store query-id]} (bootstrap-conversation+query! (h/store))]
      (store-iteration! store query-id {:thinking "tactical step"})
      (let [it (first (db/db-list-query-iterations store query-id))]
        (expect (nil? (:plan-state it)))
        (expect (nil? (:breadcrumb it)))
        (expect (nil? (:plan-diff it)))))))

;; =============================================================================
;; Sticky carry — load-effective-plan
;; =============================================================================

(defdescribe sticky-plan-test
  (it "returns most recent non-nil plan-state across iterations"
    (let [{:keys [store query-id]} (bootstrap-conversation+query! (h/store))
          plan-v1 {:goal "v1" :items [{:id 1 :content "a" :status :pending}]}
          plan-v2 {:goal "v2" :items [{:id 1 :content "a" :status :done}
                                      {:id 2 :content "b" :status :in_progress}]}]
      (store-iteration! store query-id {:plan-state plan-v1 :breadcrumb "i0"})
      (store-iteration! store query-id {:breadcrumb "i1 — no plan re-emit"})
      (store-iteration! store query-id {:plan-state plan-v2 :breadcrumb "i2"})
      (store-iteration! store query-id {:breadcrumb "i3 — no plan re-emit"})
      (expect (= plan-v2 (iterate/load-effective-plan store query-id)))))

  (it "returns nil when no iteration ever emitted a plan"
    (let [{:keys [store query-id]} (bootstrap-conversation+query! (h/store))]
      (store-iteration! store query-id {:thinking "no plan yet"})
      (expect (nil? (iterate/load-effective-plan store query-id)))))

  (it "load-breadcrumb-chain returns oldest-first, capped"
    (let [{:keys [store query-id]} (bootstrap-conversation+query! (h/store))]
      (doseq [n (range 25)]
        (store-iteration! store query-id {:breadcrumb (str "i" n " breadcrumb")}))
      (let [chain (iterate/load-breadcrumb-chain store query-id)]
        (expect (= 20 (count chain)))
        (expect (= "i5 breadcrumb" (:breadcrumb (first chain))))
        (expect (= "i24 breadcrumb" (:breadcrumb (last chain))))))))

;; =============================================================================
;; Plan diff (Phase 0b — plan-edit-distance metric)
;; =============================================================================

(defdescribe plan-diff-test
  (it "no diff for identical plans"
    (let [p {:goal "g" :items [{:id 1 :content "a" :status :pending}]}]
      (expect (nil? (iterate/compute-plan-diff p p)))
      (expect (zero? (iterate/plan-edit-distance nil)))))

  (it "added items"
    (let [d (iterate/compute-plan-diff
              {:goal "g" :items [{:id 1 :content "a" :status :pending}]}
              {:goal "g" :items [{:id 1 :content "a" :status :pending}
                                 {:id 2 :content "b" :status :pending}]})]
      (expect (= [2] (:added d)))
      (expect (= 1 (iterate/plan-edit-distance d)))))

  (it "status change bumps distance by 1"
    (let [d (iterate/compute-plan-diff
              {:goal "g" :items [{:id 1 :content "a" :status :pending}]}
              {:goal "g" :items [{:id 1 :content "a" :status :in_progress}]})]
      (expect (= 1 (count (:status-changed d))))
      (expect (= :pending (-> d :status-changed first :from)))
      (expect (= :in_progress (-> d :status-changed first :to)))
      (expect (= 1 (iterate/plan-edit-distance d)))))

  (it "goal change is observable but does not count toward distance"
    (let [d (iterate/compute-plan-diff
              {:goal "g1" :items [{:id 1 :content "a" :status :pending}]}
              {:goal "g2" :items [{:id 1 :content "a" :status :pending}]})]
      (expect (true? (:goal-changed? d)))
      (expect (= 0 (iterate/plan-edit-distance d))))))

;; =============================================================================
;; Cross-field validation
;; =============================================================================

(defdescribe plan-validation-test
  (it "accepts a well-formed plan"
    (expect (nil? (iterate/validate-plan-state
                    {:goal "g"
                     :items [{:id 1 :content "a" :status :pending}
                             {:id 2 :content "b" :status :in_progress}
                             {:id 3 :content "c" :status :done}]}))))

  (it "rejects >20 items"
    (let [items (mapv (fn [i] {:id i :content (str i) :status :pending}) (range 21))
          err (iterate/validate-plan-state {:goal "g" :items items})]
      (expect (= :vis/plan-too-large (:type err)))
      (expect (= 21 (:item-count err)))))

  (it "rejects two :in_progress items"
    (let [err (iterate/validate-plan-state
                {:goal "g"
                 :items [{:id 1 :content "a" :status :in_progress}
                         {:id 2 :content "b" :status :in_progress}]})]
      (expect (= :vis/plan-multiple-in-progress (:type err)))))

  (it "rejects non-monotonic ids"
    (let [err (iterate/validate-plan-state
                {:goal "g"
                 :items [{:id 2 :content "a" :status :pending}
                         {:id 1 :content "b" :status :pending}]})]
      (expect (= :vis/plan-non-monotonic-ids (:type err)))))

  (it "tolerates :in-progress (kebab-case) as :in_progress"
    (expect (nil? (iterate/validate-plan-state
                    {:goal "g"
                     :items [{:id 1 :content "a" :status :in-progress}]})))))

;; =============================================================================
;; Projection — build-iteration-context layered output
;; =============================================================================

;; =============================================================================
;; SYSTEM_VAR_NAMES — fixed registry (UPPERCASE, no earmuffs)
;; =============================================================================

(defdescribe system-var-registry-test
  (it "SYSTEM_VAR_NAMES contains exactly QUERY, ANSWER, REASONING"
    (expect (= '#{QUERY ANSWER REASONING}
              com.blockether.vis.loop.runtime.conversation.environment.core/SYSTEM_VAR_NAMES)))

  (it "system-var-sym? is true for registered names, false otherwise"
    (let [system-var-sym? (resolve
                            'com.blockether.vis.loop.runtime.conversation.environment.core/system-var-sym?)]
      (expect (true?  (system-var-sym? 'QUERY)))
      (expect (true?  (system-var-sym? 'ANSWER)))
      (expect (true?  (system-var-sym? 'REASONING)))
      (expect (false? (system-var-sym? 'CONFIG)))    ;; user uppercase var
      (expect (false? (system-var-sym? '*query*)))   ;; legacy earmuff form
      (expect (false? (system-var-sym? 'foo))))))

(defdescribe projection-test
  (it "renders <plan> when plan-state is provided"
    (let [out (iterate/build-iteration-context {}
                {:iteration 1 :current-max-iterations 10
                 :active-extensions []
                 :plan-state {:goal "ship phase 1"
                              :items [{:id 1 :content "spec" :status :done}
                                      {:id 2 :content "store" :status :in_progress}]}})]
      (expect (re-find #"<plan>" out))
      (expect (re-find #"goal: ship phase 1" out))
      (expect (re-find #"\[1\] done" out))
      (expect (re-find #"\[2\] in_progress" out))))

  (it "renders <breadcrumbs> with iN prefix"
    (let [out (iterate/build-iteration-context {}
                {:iteration 2 :current-max-iterations 10
                 :active-extensions []
                 :breadcrumbs [{:position 0 :breadcrumb "decomposed"}
                               {:position 1 :breadcrumb "[1] done"}]})]
      (expect (re-find #"<breadcrumbs>" out))
      (expect (re-find #"i0  decomposed" out))
      (expect (re-find #"i1  \[1\] done" out))))

  (it "renders <recent_thought> capped at 4000c"
    (let [long-thought (apply str (repeat 5000 "x"))
          out (iterate/build-iteration-context {}
                {:iteration 1 :current-max-iterations 10
                 :active-extensions []
                 :recent-thought long-thought})]
      (expect (re-find #"<recent_thought>" out))
      ;; truncated; should not contain a 5000-char run of x
      (expect (= 4000 (count (re-find #"x{1,}" out))))))

  (it "renders <system_state> with QUERY and PRIOR_TURN digest"
    (let [out (iterate/build-iteration-context {}
                {:iteration 0 :current-max-iterations 10
                 :active-extensions []
                 :system-vars {:QUERY "do the thing"}
                 :prior-turn  {:goal "previous goal" :outcome :complete
                               :counts {:done 3 :pending 0}}})]
      (expect (re-find #"<system_state>" out))
      (expect (re-find #"QUERY" out))
      (expect (re-find #"PRIOR_TURN" out))
      (expect (re-find #":outcome :complete" out))))

  (it "renders <recent> with addressable iN.K ids"
    (let [out (iterate/build-iteration-context {}
                {:iteration 4 :current-max-iterations 10
                 :active-extensions []
                 :expressions-by-iteration [[3 [{:code "(+ 1 2)" :result 3 :execution-time-ms 1}
                                           {:code "(* 2 2)" :result 4 :execution-time-ms 1}]]]})]
      (expect (re-find #"<recent>" out))
      (expect (re-find #"i3\.1" out))
      (expect (re-find #"i3\.2" out))))

  (it "omits all sections when nothing supplied (returns nil-or-iteration-header-only)"
    (let [out (iterate/build-iteration-context {}
                {:iteration 0 :current-max-iterations 10
                 :active-extensions []})]
      ;; iteration header is the only non-blank piece
      (expect (re-find #"\[iteration 1/10\]" out))
      (expect (not (re-find #"<plan>" out)))
      (expect (not (re-find #"<breadcrumbs>" out))))))


