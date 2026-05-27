(ns com.blockether.vis.internal.ctx-engine-scenario-test
  "Multi-turn scenario tests for the CTX engine.

   Scenarios are flat data: a vec of turns, each turn declaring its ops and
   the assertions to hold after every op has been applied. The replayer is
   `run-scenario`; assertions are a small DSL evaluated against the final
   ctx + warnings + progression + next-actions.

   These tests are the design-doc validation harness — when a turn fails its
   assertion, that's a real gap in the engine, not a test bug. Each scenario
   doubles as a regression for the invariant it exercises."
  (:require
   [clojure.spec.alpha :as s]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-spec :as cs]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; =============================================================================
;; Replayer
;; =============================================================================

(defn- run-turn
  "Apply every op in `ops` against ctx, synthesizing a tN/i1/fK scope for each
   op. Returns the post-turn ctx + accumulated warnings + the latest cursor."
  [ctx turn ops]
  (let [start (-> ctx
                (assoc :session/turn turn)
                (assoc :session/scope {:turn turn :iter 1 :next-form 1}))]
    (reduce
      (fn [{:keys [ctx warnings]} [idx [mutator & args]]]
        (let [scope  (str "t" turn "/i1/f" (inc idx))
              cursor (:session/scope ctx)
              ctx'   (assoc ctx :session/scope (assoc cursor :next-form (inc idx)))
              {new-ctx :ctx mut-ws :warnings stamped? :stamped?}
              (eng/apply-mutator ctx' scope mutator args)
              ctx''  (assoc new-ctx :session/scope
                       (assoc cursor :next-form (+ idx 2)))]
          {:ctx ctx''
           :warnings (into warnings (or mut-ws []))
           :last-stamped? stamped?}))
      {:ctx start :warnings []}
      (map-indexed vector ops))))

(defn run-scenario
  "Replay a scenario vector. Returns `{:turns […] :final-ctx …}`. Each turn
   entry carries `:ctx :warnings :progression :next-actions`. Caller asserts
   over the per-turn data."
  [scenario]
  (reduce
    (fn [{:keys [ctx turns]} {:keys [turn ops]}]
      (let [{ctx' :ctx mut-warnings :warnings} (run-turn ctx turn ops)
            indexes      (eng/build-indexes ctx')
            progression  (eng/derive-progression ctx' indexes)
            derived      (eng/derive-warnings ctx' indexes)
            actions      (eng/derive-plan ctx' indexes progression)
            turn-result  {:turn turn
                          :ctx ctx'
                          :mutation-warnings mut-warnings
                          :derived-warnings derived
                          :progression progression
                          :next-actions actions}]
        {:ctx ctx' :turns (conj turns turn-result)}))
    {:ctx (eng/empty-ctx "scenario-test") :turns []}
    scenario))

;; =============================================================================
;; Assertion helpers
;; =============================================================================

(defn- warning-codes [ws] (set (map :code ws)))

(defn- turn-at [result n]
  (first (filter #(= n (:turn %)) (:turns result))))

;; =============================================================================
;; Rate-limiter reconsider — the full inflection-point scenario
;; =============================================================================

(def ^:private rate-limiter-scenario
  "Compressed transcript of the multi-turn rate-limiter formal-verification
   session: observe → spec → reconsider sliding-window → token-bucket → audit
   add → prove → done → reopen for boundary edge → final done."
  [;; ─────────────────────────────────────────────────────────────────────
   ;; Turn 1 — observation: two facts about the buggy sliding-window
   {:turn 1
    :ops
    [[:fact-set! :rl-impl-sliding-window
      {:content "atom+swap! sliding window; trim+conj+check non-atomic"}]
     [:fact-set! :rl-race-trim-inc
      {:content "two concurrent allow? can both observe count<=limit and both write"}]]}

   ;; Turn 2 — formal spec + 3 reqs + 2 tasks
   {:turn 2
    :ops
    [[:spec-set! :rl-correctness
      {:title "rate_limit/allow? is race-free" :status :draft}]
     [:req-add! :rl-correctness
      {:id :linearizable-count :title "concurrent allow? preserves count<=limit"
       :facts [:rl-race-trim-inc]}]
     [:req-add! :rl-correctness
      {:id :no-lost-updates :title "two trues never combined exceed limit"
       :facts [:rl-race-trim-inc]}]
     [:req-add! :rl-correctness
      {:id :determinism-replay :title "fixed ts seq -> deterministic outcomes"}]
     [:task-set! :swap-to-cas
      {:title "replace swap! with CAS retry" :specs {:rl-correctness []}
       :status :todo}]
     [:task-set! :concurrency-test
      {:title "jstress-style test" :specs {:rl-correctness []}
       :depends-on [:swap-to-cas] :status :todo}]]}

   ;; Turn 3 — progress: CAS rewrite as prereq (no proof yet)
   {:turn 3
    :ops
    [[:task-set! :swap-to-cas {:status :doing}]
     [:task-set! :swap-to-cas {:status :done}]]}

   ;; Turn 4 — RECONSIDER: sliding-window bad → token-bucket
   {:turn 4
    :ops
    [;; supersede stale facts
     [:fact-set! :rl-impl-sliding-window {:status :superseded}]
     [:fact-set! :rl-race-trim-inc       {:status :superseded}]
     ;; drop old requirements granularly
     [:req-remove! :rl-correctness :linearizable-count]
     [:req-remove! :rl-correctness :no-lost-updates]
     [:req-remove! :rl-correctness :determinism-replay]
     ;; rename spec direction
     [:spec-set! :rl-correctness {:title "rate_limit/allow? is a correct token-bucket"}]
     ;; new requirements
     [:req-add! :rl-correctness
      {:id :bucket-monotone :title "tokens never exceed cap; refill bounded"}]
     [:req-add! :rl-correctness
      {:id :no-overshoot :title "1000 concurrent allow? with cap=10 -> <=10 true"}]
     [:req-add! :rl-correctness
      {:id :memory-bounded :title "per-key state is {:tokens :last-refill-ms}"}]
     ;; cancel orphaned tasks
     [:task-set! :swap-to-cas       {:status :cancelled}]
     [:task-set! :concurrency-test  {:status :cancelled}]
     ;; new direction
     [:fact-set! :rl-token-bucket-rationale
      {:content "token-bucket per key {tokens last-refill-ms}; bounded memory; exact rate"}]
     [:task-set! :rewrite-bucket
      {:title "rewrite allow? as token-bucket with CAS"
       :specs {:rl-correctness []} :status :todo}]
     [:task-set! :bucket-property-test
      {:title "property test bucket invariants"
       :specs {:rl-correctness []} :depends-on [:rewrite-bucket] :status :todo}]]}

   ;; Turn 5 — add 4th requirement (audit log)
   {:turn 5
    :ops
    [[:req-add! :rl-correctness
      {:id :audit-log-denials :title "false return emits {:rate-limit/denied …}"
       :facts [:rl-token-bucket-rationale]}]
     [:task-set! :audit-log-hook
      {:title "emit-event in allow? false branch"
       :specs {:rl-correctness []} :depends-on [:rewrite-bucket] :status :todo}]]}

   ;; Turn 6 — prove everything + spec :done
   {:turn 6
    :ops
    [[:task-set!  :rewrite-bucket {:status :done}]
     [:task-set!  :bucket-property-test {:status :done}]
     [:proof-add! :bucket-property-test :rl-correctness
      {:requirement :bucket-monotone :proof "t6/i1/f2"}]
     [:proof-add! :bucket-property-test :rl-correctness
      {:requirement :no-overshoot :proof "t6/i1/f2"}]
     [:proof-add! :bucket-property-test :rl-correctness
      {:requirement :memory-bounded :proof "t6/i1/f2"}]
     [:task-set!  :audit-log-hook {:status :done}]
     [:proof-add! :audit-log-hook :rl-correctness
      {:requirement :audit-log-denials :proof "t6/i1/f3"}]
     [:spec-set!  :rl-correctness {:status :done}]]}

   ;; Turn 7 — REOPEN to add edge-case requirement
   {:turn 7
    :ops
    [[:spec-set! :rl-correctness {:status :doing}]
     [:req-add!  :rl-correctness
      {:id :zero-refill-boundary
       :title "tokens=0 + refill=0 -> false; tokens=1 -> true"}]
     [:task-set! :zero-refill-test
      {:title "boundary test" :specs {:rl-correctness []} :status :todo}]
     [:task-set! :zero-refill-test {:status :done}]
     [:proof-add! :zero-refill-test :rl-correctness
      {:requirement :zero-refill-boundary :proof "t7/i1/f5"}]
     [:spec-set! :rl-correctness {:status :done}]]}])

;; =============================================================================
;; Scenario assertions — each turn's expected state
;; =============================================================================

(defdescribe rate-limiter-scenario-test
  (describe "rate-limiter reconsider scenario, end-to-end"
    (let [result (run-scenario rate-limiter-scenario)
          ctx-final (:ctx result)]

      ;; ─── Turn 1: observation produced two facts, no warnings
      (let [t1 (turn-at result 1)]
        (it "T1: two facts recorded as :active by default"
          (expect (= 2 (count (:session/facts (:ctx t1)))))
          (expect (= :active
                    (get-in t1 [:ctx :session/facts :rl-impl-sliding-window :status]
                      :active)))
          (expect (= "t1/i1/f1"
                    (get-in t1 [:ctx :session/facts :rl-impl-sliding-window :born]))))

        (it "T1: no warnings (clean observation phase)"
          (expect (empty? (:derived-warnings t1)))))

      ;; ─── Turn 2: spec drafted with 3 requirements, 2 tasks
      (let [t2 (turn-at result 2)]
        (it "T2: spec has 3 requirements"
          (expect (= 3 (count (get-in t2 [:ctx :session/specs :rl-correctness :requirements])))))

        (it "T2: spec born stamped at first emission"
          (expect (= "t2/i1/f1"
                    (get-in t2 [:ctx :session/specs :rl-correctness :born]))))

        (it "T2: progression 0/3 :open"
          (expect (= 3 (get-in t2 [:progression :rl-correctness :total])))
          (expect (= 0 (get-in t2 [:progression :rl-correctness :proven])))
          (expect (= :open (get-in t2 [:progression :rl-correctness :state]))))

        (it "T2: plan surfaces :prove-requirement entries (Phase G shape: :kind not :type)"
          (expect (some #(= :prove-requirement (:kind %)) (:next-actions t2))))

        (it "T2: plan also surfaces :work-unblocked-todo for :swap-to-cas (Phase G shape: :id not :target)"
          (expect (some #(and (= :work-unblocked-todo (:kind %))
                           (= :swap-to-cas (:id %)))
                    (:next-actions t2)))))

      ;; ─── Turn 3: prereq task done, no proofs yet
      (let [t3 (turn-at result 3)]
        (it "T3: :swap-to-cas is :done with :done-born stamped"
          (expect (= :done (get-in t3 [:ctx :session/tasks :swap-to-cas :status])))
          (expect (some? (get-in t3 [:ctx :session/tasks :swap-to-cas :done-born]))))

        (it "T3: progression unchanged (no proofs added)"
          (expect (= 0 (get-in t3 [:progression :rl-correctness :proven])))))

      ;; ─── Turn 4: full reconsider — 3 facts superseded, 3 reqs swapped, 2 cancels
      (let [t4 (turn-at result 4)]
        (it "T4: old facts flipped to :superseded with :done-born stamped"
          (expect (= :superseded
                    (get-in t4 [:ctx :session/facts :rl-impl-sliding-window :status])))
          (expect (some? (get-in t4 [:ctx :session/facts :rl-impl-sliding-window :done-born]))))

        (it "T4: spec :requirements rebuilt with new ids"
          (let [ids (set (map :id (get-in t4 [:ctx :session/specs :rl-correctness :requirements])))]
            (expect (= #{:bucket-monotone :no-overshoot :memory-bounded} ids))))

        (it "T4: cancelled tasks have :done-born stamped"
          (expect (= :cancelled (get-in t4 [:ctx :session/tasks :swap-to-cas :status])))
          (expect (some? (get-in t4 [:ctx :session/tasks :swap-to-cas :done-born]))))

        (it "T4: no orphan-proof warnings (no proofs existed before removal)"
          (expect (not (contains? (warning-codes (:derived-warnings t4))
                         :req-removed-orphaned-proof))))

        (it "T4: progression reset to 0/3 on the new requirement set"
          (expect (= 3 (get-in t4 [:progression :rl-correctness :total])))
          (expect (= 0 (get-in t4 [:progression :rl-correctness :proven])))))

      ;; ─── Turn 5: requirement added
      (let [t5 (turn-at result 5)]
        (it "T5: spec now has 4 requirements"
          (expect (= 4 (count (get-in t5 [:ctx :session/specs :rl-correctness :requirements])))))

        (it "T5: :audit-log-hook depends on :rewrite-bucket"
          (expect (= [:rewrite-bucket]
                    (get-in t5 [:ctx :session/tasks :audit-log-hook :depends-on])))))

      ;; ─── Turn 6: prove all + spec :done
      (let [t6 (turn-at result 6)]
        (it "T6: spec is :done with :done-born stamped"
          (expect (= :done (get-in t6 [:ctx :session/specs :rl-correctness :status])))
          (expect (some? (get-in t6 [:ctx :session/specs :rl-correctness :done-born]))))

        (it "T6: progression :ready (4/4)"
          (expect (= :ready (get-in t6 [:progression :rl-correctness :state])))
          (expect (= 4 (get-in t6 [:progression :rl-correctness :proven]))))

        (it "T6: no :spec-done-unproven warning"
          (expect (not (contains? (warning-codes (:derived-warnings t6))
                         :spec-done-unproven))))

        (it "T6: next-actions empty (no work pending)"
          (expect (empty? (:next-actions t6)))))

      ;; ─── Turn 7: reopen + add req + prove + close
      (let [t7 (turn-at result 7)]
        (it "T7: spec re-entered :done with 5 requirements proven"
          (expect (= 5 (count (get-in t7 [:ctx :session/specs :rl-correctness :requirements]))))
          (expect (= :done (get-in t7 [:ctx :session/specs :rl-correctness :status])))
          (expect (= :ready (get-in t7 [:progression :rl-correctness :state]))))

        (it "T7: :done-born scope updates to the final close form, not the original t6"
          (expect (= "t7"
                    (subs (get-in t7 [:ctx :session/specs :rl-correctness :done-born]) 0 2)))))

      ;; ─── Cross-turn invariants
      (it "Across all turns: no :depends-on-cycle warning (no cycles introduced)"
        (expect (not (some #(contains? (warning-codes (:derived-warnings %))
                              :depends-on-cycle)
                       (:turns result)))))

      (it "Final CTX validates against ::cs/ctx"
        (expect (s/valid? ::cs/ctx ctx-final))))))

;; =============================================================================
;; req-remove! cascade: orphan-proof warnings appear when proofs reference
;; the removed requirement.
;; =============================================================================

(def ^:private cascade-scenario
  [{:turn 1
    :ops
    [[:spec-set! :s {:title "x" :status :draft}]
     [:req-add!  :s {:id :r1 :title "first"}]
     [:req-add!  :s {:id :r2 :title "second"}]
     [:task-set! :t {:title "y" :specs {:s []} :status :todo}]
     [:proof-add! :t :s {:requirement :r1 :proof "t1/i1/f4"}]
     [:proof-add! :t :s {:requirement :r2 :proof "t1/i1/f5"}]]}
   {:turn 2
    :ops
    [[:req-remove! :s :r1]]}])

(defdescribe cascade-test
  (describe "req-remove! emits orphan-proof warnings via mutator"
    (let [result (run-scenario cascade-scenario)
          t2 (turn-at result 2)]
      (it "spec only has :r2 after removal"
        (expect (= [:r2]
                  (mapv :id (get-in t2 [:ctx :session/specs :s :requirements])))))

      (it "task still carries the orphaned :r1 proof (engine does not auto-clean)"
        (expect (= 2 (count (get-in t2 [:ctx :session/tasks :t :specs :s])))))

      (it "derive-warnings flags the orphan via :proof-unknown-req"
        (expect (contains? (warning-codes (:derived-warnings t2))
                  :proof-unknown-req))))))

;; =============================================================================
;; Collision: req-add! on existing :id soft-rejects and emits a mutation warn
;; =============================================================================

(def ^:private collision-scenario
  [{:turn 1
    :ops
    [[:spec-set! :s {:title "x" :status :draft}]
     [:req-add!  :s {:id :r1 :title "first"}]
     [:req-add!  :s {:id :r1 :title "duplicate"}]]}])

(defdescribe collision-test
  (describe "req-add! collision is soft-rejected"
    (let [result (run-scenario collision-scenario)
          t1 (turn-at result 1)]
      (it "spec keeps the original :r1 (duplicate not appended)"
        (expect (= 1 (count (get-in t1 [:ctx :session/specs :s :requirements]))))
        (expect (= "first"
                  (get-in t1 [:ctx :session/specs :s :requirements 0 :title]))))

      (it "mutation produced a :req-add-collision warning"
        (expect (contains? (warning-codes (:mutation-warnings t1))
                  :req-add-collision))))))

;; =============================================================================
;; Cycle hard reject: task-set! with a cycle does not write and warns
;; =============================================================================

(def ^:private cycle-scenario
  [{:turn 1
    :ops
    [[:task-set! :a {:title "a" :specs {} :status :todo}]
     [:task-set! :b {:title "b" :specs {} :status :todo :depends-on [:a]}]
     [:task-set! :a {:depends-on [:b]}]]}])

(defdescribe cycle-test
  (describe "task-set! :depends-on with cycle is hard-rejected"
    (let [result (run-scenario cycle-scenario)
          t1 (turn-at result 1)]
      (it ":a still has no :depends-on (write refused)"
        (expect (not (contains? (get-in t1 [:ctx :session/tasks :a]) :depends-on))))

      (it "mutation produced a :depends-on-cycle warning"
        (expect (contains? (warning-codes (:mutation-warnings t1))
                  :depends-on-cycle))))))

;; =============================================================================
;; gc-pass: terminal entries past TTL are dropped from live CTX
;; =============================================================================

(defdescribe gc-pass-test
  (describe "gc-pass drops entries past TTL"
    (let [base (-> (eng/empty-ctx)
                 (assoc-in [:session/scope :turn] 13)
                 (assoc :session/turn 13))
          ctx (-> base
                ;; task :done at turn 6, TTL 6 → eligible at turn 12+
                (assoc-in [:session/tasks :old-done]
                  {:title "x" :specs {} :status :done :born "t6/i1/f1"
                   :done-born "t6/i1/f3"})
                ;; task :cancelled at turn 4, TTL 10 → eligible at turn 14+
                (assoc-in [:session/tasks :recent-cancelled]
                  {:title "y" :specs {} :status :cancelled :born "t4/i1/f1"
                   :done-born "t4/i1/f4"})
                ;; spec :done at turn 7, TTL 6 → eligible at turn 13+
                (assoc-in [:session/specs :old-spec]
                  {:title "z" :requirements [{:id :r :title "x"}]
                   :status :done :born "t2/i1/f1" :done-born "t7/i2/f9"}))
          gced (eng/gc-pass ctx)]
      (it "task done past TTL is archived from live tree"
        (expect (nil? (get-in gced [:session/tasks :old-done]))))

      (it "spec done at the exact TTL boundary is archived"
        (expect (nil? (get-in gced [:session/specs :old-spec]))))

      (it "task cancelled but inside TTL stays live"
        (expect (some? (get-in gced [:session/tasks :recent-cancelled])))))))
