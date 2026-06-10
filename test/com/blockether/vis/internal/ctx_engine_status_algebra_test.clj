(ns com.blockether.vis.internal.ctx-engine-status-algebra-test
  "Behavior-tree status algebra (3-valued outcome + composite rollup) and the
   evidence / reason / subtree-rollup conformance passes.
   See dev/TASK_GATES_PROPOSAL.md."
  (:require
   [clojure.string :as str]
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

;; =============================================================================
;; :parent / :composite tree-creation surface (model-facing) + parent passes
;; =============================================================================
(defn- plan-task [k m] {k (merge {:title (name k) :born "t1/i1/f1" :plan? true} m)})

(defdescribe parent-tree-surface-test
  (describe "verbs set :parent + :composite"
    (it "update_plan sets :composite on a parent and :parent on children"
      (let [c (:ctx (eng/apply-mutator {:session/tasks {}} "t1/i1/f1" :update-plan!
                      [[{:step "auth" :composite "selector"}
                        {:step "mw" :parent "auth" :status "todo"}]]))
            tasks (:session/tasks c)]
        (expect (= :selector (:composite (get tasks "auth"))))
        (expect (= "auth" (:parent (get tasks "mw"))))))
    (it "plan_step sets :parent (canonicalized to the step key)"
      (let [c (:ctx (eng/apply-mutator
                      {:session/tasks (plan-task "auth" {:status :todo})}
                      "t1/i2/f1" :plan-step! ["mw" {:parent "auth" :status "todo"}]))]
        (expect (= "auth" (:parent (get-in c [:session/tasks "mw"])))))))
  (describe "pass-task-parent flags bad tree edges"
    (it "warns on a dangling :parent"
      (expect (some #(re-find #"parent ghost does not exist" %)
                (eng/derive-warnings {:session/tasks (plan-task "x" {:status :todo :parent "ghost"})} nil))))
    (it "warns on a :parent cycle"
      (expect (some #(re-find #"cyclic" %)
                (eng/derive-warnings {:session/tasks (merge (plan-task "a" {:status :todo :parent "b"})
                                                       (plan-task "b" {:status :todo :parent "a"}))} nil))))
    (it "no parent warning when the parent exists and is acyclic"
      (expect (not (some #(re-find #"parent" %)
                     (eng/derive-warnings {:session/tasks (merge (plan-task "auth" {:status :todo})
                                                            (plan-task "mw" {:status :todo :parent "auth"}))} nil)))))))

;; =============================================================================
;; tree render — nest-tasks (model dict) + task-tree-lines (human/F2 panel)
;; =============================================================================
(defn- tw [k status & {:as extra}]
  [k (merge {:status status :title (name k) :born "t1/i1/f1"} extra)])

(defdescribe nest-tasks-test
  (describe "nest-tasks (ordered annotated tree)"
    (it "orders parents before children (DFS) and stamps :depth"
      (let [tasks (into {} [(tw "auth" :doing :order 1 :composite :sequence)
                            (tw "mw" :done :order 2 :parent "auth")
                            (tw "tok" :todo :order 3 :parent "auth")])
            nested (eng/nest-tasks tasks)]
        (expect (= ["auth" "mw" "tok"] (vec (keys nested))))
        (expect (= 0 (:depth (get nested "auth"))))
        (expect (= 1 (:depth (get nested "mw"))))
        (expect (= 1 (:depth (get nested "tok"))))))
    (it "annotates each node with its ROLLED-UP :outcome"
      (let [tasks (into {} [(tw "p" :doing :order 1 :composite :sequence)
                            (tw "c1" :done :order 2 :parent "p")
                            (tw "c2" :failed :order 3 :parent "p" :reason "x")])
            nested (eng/nest-tasks tasks)]
        ;; sequence parent rolls up to :failure because a child failed
        (expect (= :failure (:outcome (get nested "p"))))
        (expect (= :success (:outcome (get nested "c1"))))
        (expect (= :failure (:outcome (get nested "c2"))))))
    (it "is an ordered array-map (Python dict insertion order = the tree shape)"
      (expect (instance? clojure.lang.IPersistentMap (eng/nest-tasks {})))
      (expect (= {} (eng/nest-tasks {}))))
    (it "keeps an orphan trapped in a :parent cycle (nothing dropped), flat"
      (let [tasks (into {} [(tw "a" :todo :order 1 :parent "b")
                            (tw "b" :todo :order 2 :parent "a")])
            nested (eng/nest-tasks tasks)]
        (expect (= #{"a" "b"} (set (keys nested))))
        (expect (every? #(= 0 (:depth %)) (vals nested)))))))

(defdescribe task-tree-lines-test
  (describe "task-tree-lines (human / F2 render)"
    (it "indents children 2 spaces per depth and shows the rollup glyph"
      (let [tasks (into {} [(tw "auth" :doing :order 1 :composite :sequence)
                            (tw "mw" :doing :order 2 :parent "auth" :title "Middleware")])
            lines (eng/task-tree-lines tasks)]
        (expect (= 2 (count lines)))
        ;; parent rolls up from its child (a :doing child → :running ◐), child indented
        (expect (str/starts-with? (first lines) "◐ auth"))
        (expect (str/starts-with? (second lines) "  ◐ mw — Middleware"))))
    (it "omits the title suffix when it equals the key"
      (expect (= ["○ x"] (eng/task-tree-lines (into {} [(tw "x" :todo)])))))))

;; =============================================================================
;; FORCING plan-gate policy — approved-plan? + plan-gate-block (Decision 2 / G1)
;; =============================================================================
(defdescribe approved-plan-test
  (describe "approved-plan?"
    (it "false with no tasks / only candidates"
      (expect (not (eng/approved-plan? {})))
      (expect (not (eng/approved-plan? {"a" {:plan? true :status :candidate}}))))
    (it "true once a plan step is accepted (non-candidate)"
      (expect (eng/approved-plan? {"a" {:plan? true :status :todo}}))
      (expect (eng/approved-plan? {"a" {:plan? true :status :doing}})))
    (it "ignores non-plan tasks (hook tasks don't count as a plan)"
      (expect (not (eng/approved-plan? {"h" {:source :hook :status :todo}}))))))

(defdescribe plan-gate-block-test
  (describe "the FIRST file is always free"
    (it "allows the first mutation of a turn (no prior files)"
      (expect (nil? (eng/plan-gate-block #{} ["a.clj"] false false)))))
  (describe "the 2nd DISTINCT file arms the gate"
    (it "blocks a 2nd distinct file with no plan + no atomic"
      (expect (some? (eng/plan-gate-block #{"a.clj"} ["b.clj"] false false)))
      (expect (re-find #"Plan required" (eng/plan-gate-block #{"a.clj"} ["b.clj"] false false))))
    (it "blocks a single call that itself spans 2 new files"
      (expect (some? (eng/plan-gate-block #{} ["a.clj" "b.clj"] false false))))
    (it "does NOT arm for re-editing the SAME file"
      (expect (nil? (eng/plan-gate-block #{"a.clj"} ["a.clj"] false false)))))
  (describe "PASS conditions"
    (it "an approved plan clears the gate"
      (expect (nil? (eng/plan-gate-block #{"a.clj"} ["b.clj"] true false))))
    (it "the atomic escape clears the gate"
      (expect (nil? (eng/plan-gate-block #{"a.clj"} ["b.clj"] false true)))))
  (describe "refusal text names the already-changed + incoming files"
    (it "lists both for the model to reason about"
      (let [msg (eng/plan-gate-block #{"a.clj"} ["b.clj"] false false)]
        (expect (re-find #"a\.clj" msg))
        (expect (re-find #"b\.clj" msg))))))

;; =============================================================================
;; subtree-scoped update_plan — update_plan(steps, parent)  (UNIFIED, one verb)
;; =============================================================================
(defn- seed-tree []
  ;; two subtrees + a root step: auth{oauth,apikey}, ui{button}
  {:session/tasks
   {"auth"   {:title "auth" :status :doing :order 1 :plan? true :composite :selector :born "t1/i1/f1"}
    "oauth"  {:title "oauth" :status :todo :order 2 :plan? true :parent "auth" :born "t1/i1/f1"}
    "apikey" {:title "apikey" :status :todo :order 3 :plan? true :parent "auth" :born "t1/i1/f1"}
    "ui"     {:title "ui" :status :todo :order 4 :plan? true :born "t1/i1/f1"}
    "button" {:title "button" :status :todo :order 5 :plan? true :parent "ui" :born "t1/i1/f1"}}})

(defdescribe update-subplan-test
  (describe "replaces ONLY the target subtree (scoped update_plan)"
    (it "swaps auth's children, leaving ui/button + auth itself untouched"
      (let [c (:ctx (eng/apply-mutator (seed-tree) "t2/i1/f1" :update-plan!
                      [[{:step "saml"} {:step "ldap"}] "auth"]))
            tasks (:session/tasks c)]
        (expect (not (contains? tasks "oauth")))
        (expect (not (contains? tasks "apikey")))
        (expect (= "auth" (:parent (get tasks "saml"))))
        (expect (= "auth" (:parent (get tasks "ldap"))))
        (expect (= :selector (:composite (get tasks "auth"))))
        (expect (contains? tasks "ui"))
        (expect (= "ui" (:parent (get tasks "button"))))))
    (it "blows away the WHOLE subtree (grandchildren too), no orphans"
      (let [seed (assoc-in (seed-tree) [:session/tasks "gc"]
                   {:title "gc" :status :todo :order 6 :plan? true :parent "oauth" :born "t1/i1/f1"})
            c (:ctx (eng/apply-mutator seed "t2/i1/f1" :update-plan! [[{:step "saml"}] "auth"]))
            tasks (:session/tasks c)]
        (expect (not (contains? tasks "gc")))
        (expect (not (some #(re-find #"ghost|does not exist" %) (eng/derive-warnings c nil)))))))
  (describe "guards"
    (it "a BLANK scope falls back to a whole-plan replace (not a subtree)"
      (let [c (:ctx (eng/apply-mutator (seed-tree) "t2/i1/f1" :update-plan!
                      [[{:step "fresh"}] "  "]))
            tasks (:session/tasks c)]
        ;; whole-plan replace wiped ALL prior plan steps, kept only the new one
        (expect (contains? tasks "fresh"))
        (expect (not (contains? tasks "auth")))))
    (it "unknown parent → soft warn, no write (stamped? false)"
      (let [r (eng/apply-mutator (seed-tree) "t2/i1/f1" :update-plan! [[{:step "x"}] "nope"])]
        (expect (false? (:stamped? r)))
        (expect (some #(re-find #"not a known task" (:message %)) (:warnings r)))))
    (it "a child may nest a deeper :parent within the new subtree"
      (let [c (:ctx (eng/apply-mutator (seed-tree) "t2/i1/f1" :update-plan!
                      [[{:key "saml" :step "saml"} {:step "saml_cb" :parent "saml"}] "auth"]))
            tasks (:session/tasks c)]
        (expect (= "auth" (:parent (get tasks "saml"))))
        (expect (= "saml" (:parent (get tasks "saml_cb"))))))))

;; =============================================================================
;; Node contract — DOWN dispatch packet + UP rollup report
;; =============================================================================
(defn- contract-ctx []
  {:session/tasks
   {"auth"   {:title "Add auth" :status :doing :order 1 :plan? true :composite :selector
              :acceptance "login works" :born "t1/i1/f1"}
    "oauth"  {:title "OAuth path" :status :doing :order 2 :plan? true :parent "auth"
              :facts ["oauth_loc"] :decorators [{:type :retry :n 2}] :born "t1/i1/f1"}
    "apikey" {:title "API-key path" :status :done :order 3 :plan? true :parent "auth"
              :evidence "probe -> 200" :facts ["apikey_loc"] :born "t1/i1/f1"}}
   :session/facts
   {"oauth_loc"  {:content "oauth handler"
                  :files [{:path "auth.clj" :regions [{:src "(defn oauth ...)"}]}]}
    "apikey_loc" {:content "apikey handler"
                  :files [{:path "key.clj" :regions [{:src "(defn apikey ...)"}]}]}}})

(defdescribe node-contract-test
  (describe "node-files gathers linked facts' :files, tagged by source fact"
    (it "pulls the regions a node targets"
      (let [files (eng/node-files (contract-ctx) "oauth")]
        (expect (= 1 (count files)))
        (expect (= "auth.clj" (:path (first files))))
        (expect (= "oauth_loc" (:fact (first files)))))))
  (describe "DOWN: node-dispatch-packet is self-contained"
    (it "a leaf with files is dispatchable and carries goal/acceptance-via-parent/decorators/files"
      (let [p (eng/node-dispatch-packet (contract-ctx) "oauth")]
        (expect (= "OAuth path" (:goal p)))
        (expect (= [{:type :retry :n 2}] (:decorators p)))
        (expect (= "auth.clj" (:path (first (:files p)))))
        (expect (empty? (:children p)))
        (expect (:dispatchable? p))))
    (it "a parent is dispatchable via its children even with no files"
      (let [p (eng/node-dispatch-packet (contract-ctx) "auth")]
        (expect (= ["oauth" "apikey"] (:children p)))
        (expect (= :selector (:composite p)))
        (expect (:dispatchable? p))))
    (it "a leaf with NO files and NO children is NOT dispatchable"
      (let [ctx {:session/tasks {"x" {:title "x" :status :todo :plan? true}}}]
        (expect (not (:dispatchable? (eng/node-dispatch-packet ctx "x")))))))
  (describe "UP: node-rollup-report folds outcome + evidence + subtree facts"
    (it "a selector parent rolls up to :success and folds children's facts"
      (let [r (eng/node-rollup-report (contract-ctx) "auth")]
        (expect (= :success (:outcome r)))               ; apikey done → selector success
        (expect (= #{"oauth_loc" "apikey_loc"} (set (:facts r))))))
    (it "a leaf reports its own outcome + evidence"
      (let [r (eng/node-rollup-report (contract-ctx) "apikey")]
        (expect (= :success (:outcome r)))
        (expect (= "probe -> 200" (:evidence r)))
        (expect (= ["apikey_loc"] (:facts r)))))))

(defdescribe explicit-step-key-test
  (it "explicit :key decouples the stable step key from a multi-word title (parent ref stays valid)"
    (let [c (:ctx (eng/apply-mutator {:session/tasks {}} "t1/i1/f1" :update-plan!
                    [[{:key "auth" :title "Auth strategy" :composite "selector"}
                      {:key "mw" :title "The middleware layer" :parent "auth"}]]))
          tasks (:session/tasks c)]
      (expect (contains? tasks "auth"))
      (expect (= "Auth strategy" (:title (get tasks "auth"))))
      (expect (= "auth" (:parent (get tasks "mw"))))
      ;; and no dangling-parent warning, because the ref resolves
      (expect (not (some #(re-find #"does not exist" %) (eng/derive-warnings c nil)))))))
