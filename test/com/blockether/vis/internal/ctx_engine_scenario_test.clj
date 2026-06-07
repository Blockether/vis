(ns com.blockether.vis.internal.ctx-engine-scenario-test
  "Multi-turn scenario tests for the CTX engine under the tasks + facts model.

   Scenarios are flat data: a vec of turns, each turn declaring its ops and
   the assertions to hold after every op has been applied. The replayer is
   `run-scenario`; assertions are a small DSL evaluated against the final
   ctx + warnings.

   These tests are the design-doc validation harness — when a turn fails its
   assertion, that's a real gap in the engine, not a test bug. Each scenario
   doubles as a regression for the invariant it exercises."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string]
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
   entry carries `:ctx :mutation-warnings :derived-warnings`. Caller asserts
   over the per-turn data."
  [scenario]
  (reduce
    (fn [{:keys [ctx turns]} {:keys [turn ops]}]
      (let [{ctx' :ctx mut-warnings :warnings} (run-turn ctx turn ops)
            indexes      (eng/build-indexes ctx')
            derived      (eng/derive-warnings ctx' indexes)
            turn-result  {:turn turn
                          :ctx ctx'
                          :mutation-warnings mut-warnings
                          :derived-warnings derived}]
        {:ctx ctx' :turns (conj turns turn-result)}))
    {:ctx (eng/empty-ctx "scenario-test") :turns []}
    scenario))

;; =============================================================================
;; Assertion helpers
;; =============================================================================

(defn- mutation-codes
  "derive-warnings returns plain strings now, but mutation warnings (the
   :warnings vec returned directly by apply-mutator) are still {:code …}
   maps. This helper projects their :code set."
  [ws]
  (set (map :code ws)))

(defn- derived-has?
  "True if any derived warning string contains `frag`."
  [ws frag]
  (boolean (some #(clojure.string/includes? % frag) ws)))

(defn- turn-at [result n]
  (first (filter #(= n (:turn %)) (:turns result))))

;; =============================================================================
;; Rate-limiter reconsider — tasks + facts, deps, supersede, done, cancel
;; =============================================================================

(def ^:private rate-limiter-scenario
  "Compressed transcript of the multi-turn rate-limiter session reframed for
   the tasks+facts model: observe (facts) → plan (tasks + deps) → progress
   (done) → reconsider (supersede facts, cancel tasks, new direction) →
   close out remaining work as :done."
  [;; ─────────────────────────────────────────────────────────────────────
   ;; Turn 1 — observation: two facts about the buggy sliding-window
   {:turn 1
    :ops
    [[:fact-set! :rl-impl-sliding-window
      {:content "atom+swap! sliding window; trim+conj+check non-atomic"}]
     [:fact-set! :rl-race-trim-inc
      {:content "two concurrent allow? can both observe count<=limit and both write"}]]}

   ;; Turn 2 — plan: two tasks, one depending on the other
   {:turn 2
    :ops
    [[:task-set! :swap-to-cas
      {:title "replace swap! with CAS retry" :status :todo}]
     [:task-set! :concurrency-test
      {:title "jstress-style test" :depends_on [:swap-to-cas] :status :todo}]]}

   ;; Turn 3 — progress: CAS rewrite to :doing then :done
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
     ;; cancel orphaned tasks
     [:task-set! :swap-to-cas       {:status :cancelled}]
     [:task-set! :concurrency-test  {:status :cancelled}]
     ;; new direction
     [:fact-set! :rl-token-bucket-rationale
      {:content "token-bucket per key {tokens last-refill-ms}; bounded memory; exact rate"}]
     [:task-set! :rewrite-bucket
      {:title "rewrite allow? as token-bucket with CAS" :status :todo}]
     [:task-set! :bucket-property-test
      {:title "property test bucket invariants"
       :depends_on [:rewrite-bucket] :status :todo}]]}

   ;; Turn 5 — add audit-log task depending on the rewrite
   {:turn 5
    :ops
    [[:task-set! :audit-log-hook
      {:title "emit-event in allow? false branch"
       :depends_on [:rewrite-bucket] :status :todo}]]}

   ;; Turn 6 — close everything out
   {:turn 6
    :ops
    [[:task-set! :rewrite-bucket {:status :done}]
     [:task-set! :bucket-property-test {:status :done}]
     [:task-set! :audit-log-hook {:status :done}]]}])

;; =============================================================================
;; Scenario assertions — each turn's expected state
;; =============================================================================

(defdescribe rate-limiter-scenario-test
  (describe "rate-limiter reconsider scenario, end-to-end (tasks + facts)"
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

      ;; ─── Turn 2: two tasks, dependency wired
      (let [t2 (turn-at result 2)]
        (it "T2: two tasks recorded, both :todo"
          (expect (= 2 (count (:session/tasks (:ctx t2)))))
          (expect (= :todo (get-in t2 [:ctx :session/tasks :swap-to-cas :status])))
          (expect (= :todo (get-in t2 [:ctx :session/tasks :concurrency-test :status]))))

        (it "T2: task born stamped at first emission"
          (expect (= "t2/i1/f1"
                    (get-in t2 [:ctx :session/tasks :swap-to-cas :born]))))

        (it "T2: :concurrency-test depends on :swap-to-cas"
          (expect (= [:swap-to-cas]
                    (get-in t2 [:ctx :session/tasks :concurrency-test :depends_on]))))

        (it "T2: no dangling-dep or cycle warnings"
          (expect (not (derived-has? (:derived-warnings t2) "nonexistent")))
          (expect (empty? (filter #(clojure.string/includes? % "cycle")
                            (:derived-warnings t2))))))

      ;; ─── Turn 3: prereq task done, done-born stamped
      (let [t3 (turn-at result 3)]
        (it "T3: :swap-to-cas is :done with :done-born stamped"
          (expect (= :done (get-in t3 [:ctx :session/tasks :swap-to-cas :status])))
          (expect (some? (get-in t3 [:ctx :session/tasks :swap-to-cas :done-born]))))

        (it "T3: done is self-asserted — no reversion, status stays :done"
          (expect (= :done (get-in t3 [:ctx :session/tasks :swap-to-cas :status])))))

      ;; ─── Turn 4: full reconsider — facts superseded, tasks cancelled, new work
      (let [t4 (turn-at result 4)]
        (it "T4: old facts flipped to :superseded with :done-born stamped"
          (expect (= :superseded
                    (get-in t4 [:ctx :session/facts :rl-impl-sliding-window :status])))
          (expect (some? (get-in t4 [:ctx :session/facts :rl-impl-sliding-window :done-born]))))

        (it "T4: cancelled tasks have :done-born stamped"
          (expect (= :cancelled (get-in t4 [:ctx :session/tasks :swap-to-cas :status])))
          (expect (some? (get-in t4 [:ctx :session/tasks :swap-to-cas :done-born]))))

        (it "T4: new token-bucket fact + tasks present"
          (expect (some? (get-in t4 [:ctx :session/facts :rl-token-bucket-rationale])))
          (expect (= :todo (get-in t4 [:ctx :session/tasks :rewrite-bucket :status])))
          (expect (= [:rewrite-bucket]
                    (get-in t4 [:ctx :session/tasks :bucket-property-test :depends_on])))))

      ;; ─── Turn 5: audit task added, depends on the rewrite
      (let [t5 (turn-at result 5)]
        (it "T5: :audit-log-hook depends on :rewrite-bucket"
          (expect (= [:rewrite-bucket]
                    (get-in t5 [:ctx :session/tasks :audit-log-hook :depends_on]))))

        (it "T5: a :done task with non-terminal dep surfaces no done-dep warning yet"
          ;; :rewrite-bucket is still :todo here but nothing :done depends on it
          (expect (not (derived-has? (:derived-warnings t5) ":done but dep")))))

      ;; ─── Turn 6: close everything out, all terminal
      (let [t6 (turn-at result 6)]
        (it "T6: every task is :done with :done-born stamped"
          (doseq [k [:rewrite-bucket :bucket-property-test :audit-log-hook]]
            (expect (= :done (get-in t6 [:ctx :session/tasks k :status])))
            (expect (some? (get-in t6 [:ctx :session/tasks k :done-born])))))

        (it "T6: no task-done-pending-dep warning (all deps terminal)"
          (expect (not (derived-has? (:derived-warnings t6) ":done but dep")))))

      ;; ─── Cross-turn invariants
      (it "Across all turns: no cycle warning (no cycles introduced)"
        (expect (not (some #(some (fn [w] (clojure.string/includes? w "cycle"))
                              (:derived-warnings %))
                       (:turns result)))))

      (it "Final CTX validates against ::cs/ctx"
        (expect (s/valid? ::cs/ctx ctx-final))))))

;; =============================================================================
;; task-done-pending-dep: a :done task whose dep is non-terminal warns (soft)
;; =============================================================================

(def ^:private done-pending-dep-scenario
  [{:turn 1
    :ops
    [[:task-set! :prereq {:title "do first" :status :todo}]
     [:task-set! :follow {:title "do second" :depends_on [:prereq] :status :todo}]
     ;; close the follow-up while prereq is still open
     [:task-set! :follow {:status :done}]]}])

(defdescribe done-pending-dep-test
  (describe "task :done while a dep is non-terminal emits a soft warning"
    (let [result (run-scenario done-pending-dep-scenario)
          t1 (turn-at result 1)]
      (it ":follow is :done (self-asserted, never reverted)"
        (expect (= :done (get-in t1 [:ctx :session/tasks :follow :status]))))

      (it "derive-warnings surfaces a :done-but-pending-dep advisory string"
        (expect (derived-has? (:derived-warnings t1) ":done but dep :prereq"))))))

;; =============================================================================
;; Contradicting facts: symmetric contradiction + soft warning while both active
;; =============================================================================

(def ^:private contradiction-scenario
  [{:turn 1
    :ops
    [[:fact-set! :a {:content "uses bcrypt"}]
     [:fact-set! :b {:content "uses argon2"}]
     [:fact-contradicts! :a :b]]}
   {:turn 2
    :ops
    [;; resolve by superseding one side
     [:fact-set! :a {:status :superseded}]]}])

(defdescribe contradiction-test
  (describe "fact-contradicts! is symmetric and warns while both stay :active"
    (let [result (run-scenario contradiction-scenario)
          t1 (turn-at result 1)
          t2 (turn-at result 2)]
      (it "T1: contradiction written symmetrically on both facts"
        (expect (contains? (get-in t1 [:ctx :session/facts :a :contradicts]) :b))
        (expect (contains? (get-in t1 [:ctx :session/facts :b :contradicts]) :a)))

      (it "T1: both :active → derive-warnings flags the contradiction"
        (expect (derived-has? (:derived-warnings t1) "↔")))

      (it "T2: superseding one side clears the contradiction warning"
        (expect (not (derived-has? (:derived-warnings t2) "↔")))))))

;; =============================================================================
;; Cycle hard reject: task-set! with a cycle does not write and warns
;; =============================================================================

(def ^:private cycle-scenario
  [{:turn 1
    :ops
    [[:task-set! :a {:title "a" :status :todo}]
     [:task-set! :b {:title "b" :status :todo :depends_on [:a]}]
     [:task-set! :a {:depends_on [:b]}]]}])

(defdescribe cycle-test
  (describe "task-set! :depends_on with cycle is hard-rejected"
    (let [result (run-scenario cycle-scenario)
          t1 (turn-at result 1)]
      (it ":a still has no :depends_on (write refused)"
        (expect (not (contains? (get-in t1 [:ctx :session/tasks :a]) :depends_on))))

      (it "mutation produced a :depends_on_cycle warning"
        (expect (contains? (mutation-codes (:mutation-warnings t1))
                  :depends_on_cycle))))))

;; =============================================================================
;; Dangling dep: a :depends_on ref to a nonexistent entity surfaces structurally
;; =============================================================================

(def ^:private dangling-dep-scenario
  [{:turn 1
    :ops
    [[:task-set! :solo {:title "depends on a ghost" :depends_on [:ghost] :status :todo}]]}])

(defdescribe dangling-dep-test
  (describe "task :depends_on a nonexistent entity → structural dangling warning"
    (let [result (run-scenario dangling-dep-scenario)
          t1 (turn-at result 1)]
      (it "the dep was written (cycle check passed; target just doesn't exist)"
        (expect (= [:ghost] (get-in t1 [:ctx :session/tasks :solo :depends_on]))))

      (it "derive-warnings flags the nonexistent target"
        (expect (derived-has? (:derived-warnings t1) "nonexistent"))))))

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
                  {:title "x" :status :done :born "t6/i1/f1"
                   :done-born "t6/i1/f3"})
                ;; task :cancelled at turn 4, TTL 10 → eligible at turn 14+
                (assoc-in [:session/tasks :recent-cancelled]
                  {:title "y" :status :cancelled :born "t4/i1/f1"
                   :done-born "t4/i1/f4"})
                ;; fact :superseded at turn 7, TTL 6 → eligible at turn 13+
                (assoc-in [:session/facts :old-fact]
                  {:content "z" :status :superseded :born "t2/i1/f1"
                   :done-born "t7/i2/f9"}))
          gced (eng/gc-pass ctx)]
      (it "task done past TTL is archived from live tree"
        (expect (nil? (get-in gced [:session/tasks :old-done]))))

      (it "fact superseded at the exact TTL boundary is archived"
        (expect (nil? (get-in gced [:session/facts :old-fact]))))

      (it "task cancelled but inside TTL stays live"
        (expect (some? (get-in gced [:session/tasks :recent-cancelled])))))))
