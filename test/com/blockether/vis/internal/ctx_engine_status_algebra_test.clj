(ns com.blockether.vis.internal.ctx-engine-status-algebra-test
  "Behavior-tree status algebra (3-valued outcome + composite rollup) and the
   evidence / reason / subtree-rollup conformance passes.
   See dev/TASK_GATES_PROPOSAL.md."
  (:require
   [com.blockether.vis.internal.ctx-engine :as eng]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- task [status & {:as extra}]
  (merge {:status status :title "t" :born "t1/i1/f1"} extra))

;; =============================================================================
;; node-outcome — status -> {:success :failure :running :pending :skipped}
;; =============================================================================
(defdescribe node-outcome-test
  (describe "node-outcome"
    (it "maps the three BT states + pending/skipped"
      (expect (= :success (eng/node-outcome (task :done))))
      (expect (= :failure (eng/node-outcome (task :failed))))
      (expect (= :running (eng/node-outcome (task :doing))))
      (expect (= :pending (eng/node-outcome (task :todo))))
      (expect (= :pending (eng/node-outcome (task :candidate)))))
    (it "non-success terminals are NEUTRAL (skipped), not failure"
      (expect (= :skipped (eng/node-outcome (task :cancelled))))
      (expect (= :skipped (eng/node-outcome (task :rejected))))
      (expect (= :skipped (eng/node-outcome (task :deferred)))))
    (it "unknown status defaults to :pending"
      (expect (= :pending (eng/node-outcome (task :weird)))))))

;; =============================================================================
;; composite-rollup — :sequence/:parallel (all-succeed) vs :selector (any-succeed)
;; =============================================================================
(defdescribe composite-rollup-test
  (describe ":sequence / :parallel (all-succeed)"
    (it "any failure fails the node"
      (expect (= :failure (eng/composite-rollup :sequence [:success :failure :pending])))
      (expect (= :failure (eng/composite-rollup :parallel [:success :failure]))))
    (it "succeeds only when all succeed (empty = vacuously success)"
      (expect (= :success (eng/composite-rollup :sequence [:success :success])))
      (expect (= :success (eng/composite-rollup :sequence []))))
    (it "running dominates pending when nothing failed"
      (expect (= :running (eng/composite-rollup :sequence [:success :running :pending]))))
    (it "skipped children are neutral"
      (expect (= :success (eng/composite-rollup :sequence [:success :skipped])))
      (expect (= :success (eng/composite-rollup :sequence [:skipped :skipped]))))
    (it ":sequence and :parallel agree on the same inputs"
      (expect (= (eng/composite-rollup :sequence [:success :running])
                (eng/composite-rollup :parallel [:success :running])))))
  (describe ":selector (any-succeed / fallback)"
    (it "succeeds on the first success even after a failure"
      (expect (= :success (eng/composite-rollup :selector [:failure :success]))))
    (it "fails only when every effective child failed"
      (expect (= :failure (eng/composite-rollup :selector [:failure :failure]))))
    (it "running/pending take precedence over an all-failed verdict"
      (expect (= :running (eng/composite-rollup :selector [:failure :running])))
      (expect (= :pending (eng/composite-rollup :selector [:failure :pending]))))
    (it "empty selector is :pending (nothing tried), not :failure"
      (expect (= :pending (eng/composite-rollup :selector [])))
      (expect (= :pending (eng/composite-rollup :selector [:skipped]))))))

;; =============================================================================
;; derived-outcome — leaf vs parent rollup over live children
;; =============================================================================
(defdescribe derived-outcome-test
  (describe "derived-outcome"
    (it "a leaf rolls up from its own status"
      (expect (= :success (eng/derived-outcome {:session/tasks {"a" (task :done)}} "a"))))
    (it "a :sequence parent fails if any child failed"
      (let [ctx {:session/tasks {"p"  (task :doing :composite :sequence)
                                 "c1" (task :done :parent "p")
                                 "c2" (task :failed :parent "p")}}]
        (expect (= :failure (eng/derived-outcome ctx "p")))))
    (it "a :selector parent succeeds if any child succeeded"
      (let [ctx {:session/tasks {"p"  (task :doing :composite :selector)
                                 "c1" (task :failed :parent "p")
                                 "c2" (task :done :parent "p")}}]
        (expect (= :success (eng/derived-outcome ctx "p")))))
    (it "default composite is :sequence"
      (let [ctx {:session/tasks {"p"  (task :doing)
                                 "c1" (task :done :parent "p")
                                 "c2" (task :done :parent "p")}}]
        (expect (= :success (eng/derived-outcome ctx "p")))))))

;; =============================================================================
;; conformance passes via derive-warnings (public surface)
;; =============================================================================
(defn- warnings [tasks]
  (eng/derive-warnings {:session/tasks tasks} nil))

(defdescribe conformance-passes-test
  (describe "done-gate: evidence"
    (it "warns when :done + :acceptance but no :evidence"
      (expect (some #(re-find #"no :evidence" %)
                (warnings {"a" (task :done :acceptance "tests green")}))))
    (it "no warning once :evidence is present"
      (expect (not (some #(re-find #"no :evidence" %)
                     (warnings {"a" (task :done :acceptance "x"
                                      :evidence "ran clj -M:test -> 0 fail")})))))
    (it "no warning when there's no acceptance to verify"
      (expect (not (some #(re-find #"no :evidence" %)
                     (warnings {"a" (task :done)}))))))
  (describe "terminal: reason"
    (it "warns on :failed/:deferred/:cancelled/:rejected without :reason"
      (expect (some #(re-find #"without a :reason" %) (warnings {"a" (task :failed)})))
      (expect (some #(re-find #"without a :reason" %) (warnings {"a" (task :deferred)}))))
    (it "no warning once a :reason is given"
      (expect (not (some #(re-find #"without a :reason" %)
                     (warnings {"a" (task :deferred :reason "blocked on upstream API")}))))))
  (describe "subtree rollup conformance"
    (it "warns when a parent is :done but its subtree rollup is not :success"
      (expect (some #(re-find #"subtree rollup is :failure" %)
                (warnings {"p" (task :done :composite :sequence)
                           "c" (task :failed :parent "p" :reason "x")}))))
    (it "no warning when the subtree actually rolls up to :success"
      (expect (not (some #(re-find #"subtree rollup" %)
                     (warnings {"p" (task :done :composite :sequence)
                                "c" (task :done :parent "p")})))))))
