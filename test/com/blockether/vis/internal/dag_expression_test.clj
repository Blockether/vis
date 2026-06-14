(ns com.blockether.vis.internal.dag-expression-test
  (:require
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.dag-expression :as sut]
   [com.blockether.vis.internal.env-python :as env]
   [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- invalid-advance?
  [f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= :vis/invalid-advance (:type (ex-data e))))))

(defdescribe source-contract-test
  (it "accepts one root advance expression with nested evidence calls"
    (expect (nil? (sut/source-error
                    "# inspect and advance\nadvance({'tasks': {'verify': {'evidence': probe()}}})"))))

  (it "accepts literal answer templates with nested calls only in evidence"
    (expect (nil? (sut/source-error
                    (str "advance({'tasks': {'inspect': {'evidence': git_diff({'is_patch': True})}}, "
                      "'answer_template': 'Summary: {{tasks.inspect.evidence | git_diff_summary}}'})")))))

  (it "accepts bare read-only observation call sequences"
    (expect (nil? (sut/source-error "cat('a.go')")))
    (expect (nil? (sut/source-error "cat('a.go')\ncat('b.go')")))
    (expect (nil? (sut/source-error "ls()\nfind('.')"))))

  (it "rejects prose, assignments, and graph control mutations outside advance"
    (expect (some? (sut/source-error "x = 1")))
    (expect (some? (sut/source-error "x = cat('a.go')")))
    (expect (some? (sut/source-error "done('x')")))
    (expect (some? (sut/source-error "cat('a.go')\ndone('x')"))))

  (it "rejects statements, sibling expressions, and non-advance roots when advance is present"
    (expect (some? (sut/source-error "x = advance({'answer': 'x'})")))
    (expect (some? (sut/source-error "advance({'answer': 'x'})\nprobe()"))))

  (it "rejects nested graph and control calls"
    (expect (some? (sut/source-error "advance({'answer': done('x')})")))
    (expect (some? (sut/source-error
                     "advance({'tasks': {'x': plan_step('x', {'status': 'done'})}})")))
    (expect (some? (sut/source-error "advance({'answer': advance({'answer': 'x'})})"))))

  (it "rejects executable answers before nested calls can bypass DAG slots"
    (expect (some? (sut/source-error
                     "advance({'answer': str(git_diff({'is_patch': True}))})")))
    (expect (some? (sut/source-error
                     "advance({'answer_template': summarize(git_diff({'is_patch': True}))})")))
    (expect (some? (sut/source-error
                     "advance({'answer': 'prefix ' + suffix})"))))

  (it "rejects removed no_goal before nested calls can execute"
    (expect (some? (sut/source-error
                     (str "advance({'no_goal': True, "
                       "'tasks': {'status': {'evidence': git_status()}}})")))))

  (it "rejects terminal literal answers over fresh same-advance tool evidence"
    (expect (some? (sut/source-error
                     (str "advance({'tasks': {'inspect': {'evidence': git_status()}}, "
                       "'answer': 'Working tree is clean.', 'done': True})")))))

  (it "rejects raw dump transforms before nested tool calls can execute"
    (expect (some? (sut/source-error
                     (str "advance({'tasks': {'inspect': {'evidence': git_status()}}, "
                       "'answer_template': '{{tasks.inspect.evidence | json}}', "
                       "'done': True})"))))))

(defdescribe advance-validation-test
  (it "tags a valid advance"
    (let [result (sut/advance {:tasks {:verify {:status "done"}}
                               :answer "Complete"})]
      (expect (true? (:vis_advance result)))
      (expect (= "Complete" (:answer result)))))

  (it "accepts answer_template as the graph-rendered prose path"
    (let [result (sut/advance {:tasks {:inspect {:status "done"}}
                               :answer_template "Summary: {{tasks.inspect.evidence | evidence_summary}}"})]
      (expect (true? (:vis_advance result)))
      (expect (= "Summary: {{tasks.inspect.evidence | evidence_summary}}"
                (:answer_template result)))))

  (it "rejects unknown keys, invalid entity maps, and unstable ids"
    (expect (invalid-advance? #(sut/advance {:unknown true})))
    (expect (invalid-advance? #(sut/advance {:tasks {:x "done"}})))
    (expect (invalid-advance? #(sut/advance {:facts {42 {:content "x"}}}))))

  (it "rejects ambiguous answer rendering fields"
    (expect (invalid-advance?
              #(sut/advance {:answer "literal"
                             :answer_template "{{tasks.x.evidence | evidence_summary}}"})))
    (expect (invalid-advance?
              #(sut/advance {:answer_template "{{tasks.x.evidence}}"})))
    (expect (invalid-advance?
              #(sut/advance {:answer "ok"
                             :no_goal true})))))

(defdescribe terminal-flag-validation-test
  (it "accepts and threads an explicit :done terminal flag"
    (let [result (sut/advance {:tasks {:verify {:status "done"}}
                               :answer "Complete" :done true})]
      (expect (true? (:done result)))
      (expect (= "Complete" (:answer result)))))

  (it "defaults :done to false when absent"
    (let [result (sut/advance {:tasks {:verify {:status "done"}}})]
      (expect (false? (:done result)))))

  (it "rejects a non-boolean :done flag"
    (expect (invalid-advance? #(sut/advance {:answer "x" :done "yes"}))))

  (it "rejects the removed :no_goal flag"
    (expect (invalid-advance?
              #(sut/advance {:answer "Hey! What can I help you with?"
                             :no_goal true})))))

(defdescribe eager-evidence-evaluation-test
  (it "evaluates an inline sandbox call before advance receives the payload"
    (let [calls (atom 0)
          {:keys [^Context python-context]}
          (env/create-python-context
            {'probe (fn []
                      (swap! calls inc)
                      {:exit 0 :stdout "verified"})
             'advance sut/advance})]
      (try
        (let [{:keys [error result]}
              (env/run-python-block python-context
                "advance({'tasks': {'verify': {'status': 'done', 'evidence': probe()}}, 'answer': 'ok'})")]
          (expect (nil? error))
          (expect (= 1 @calls))
          (expect (true? (:vis_advance result)))
          (expect (= "verified" (get-in result [:tasks :verify :evidence :stdout]))))
        (finally
          (.close python-context true))))))

(defdescribe graph-transaction-test
  (it "applies task and fact updates as one pure advance"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/advance
                  {:tasks {:verify {:title "Verify" :status "done"
                                    :evidence {:exit 0}}}
                   :facts {:result {:content "Tests passed"}}
                   :answer "Complete"})
          {:keys [ctx receipt]} (sut/apply-advance base "t1/i1/f1" value)]
      (expect (= :done (get-in ctx [:session/tasks "verify" :status])))
      (expect (= "{:exit 0}" (get-in ctx [:session/tasks "verify" :evidence])))
      (expect (= "Tests passed" (get-in ctx [:session/facts "result" :content])))
      (expect (= ["verify"] (:tasks receipt)))
      (expect (= ["result"] (:facts receipt)))
      (expect (:answered? receipt))
      (expect (= [{:id "evidence/verify/0"
                   :task "verify"
                   :kind "observed"
                   :status "observed"
                   :value "{:exit 0}"}]
                (:resolved_evidence receipt)))
      (expect (= [nil :done] (get-in receipt [:graph_diff :tasks "verify" :status])))
      (expect (true? (get-in receipt [:graph_diff :tasks "verify" :evidence_added])))))

  (it "renders answer_template from raw resolved task evidence"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/advance
                  {:tasks {:inspect {:title "Inspect" :status "done"
                                     :evidence {:stat {:files 2 :add 10 :del 3}
                                                :files [{:file "README.md"}
                                                        {:file "src/bridge/cli.clj"}]}}}
                   :answer_template "Uncommitted changes: {{tasks.inspect.evidence | git_diff_summary}}."
                   :done true})
          {:keys [receipt]} (sut/apply-advance base "t1/i1/f1" value)]
      (expect (= "Uncommitted changes: 2 files changed (+10/-3): README.md, src/bridge/cli.clj."
                (:answer receipt)))
      (expect (:answered? receipt))))

  (it "rejects answer_template references to missing slots and unknown transforms"
    (let [base (ctx-engine/empty-ctx "scenario")
          missing (sut/advance
                    {:tasks {:inspect {:title "Inspect"}}
                     :answer_template "{{tasks.inspect.evidence | evidence_summary}}"})]
      (expect (invalid-advance? #(sut/apply-advance base "t1/i1/f1" missing)))
      (expect (invalid-advance?
                #(sut/advance
                   {:tasks {:inspect {:title "Inspect" :evidence "x"}}
                    :answer_template "{{tasks.inspect.evidence | run_shell}}"})))))

  (it "rejects a cycle without exposing the partially updated graph"
    (let [base (-> (ctx-engine/empty-ctx "scenario")
                 (assoc-in [:session/tasks "T"]
                   {:title "task" :born "t1/i1/f1" :depends_on [[:fact "F"]]})
                 (assoc-in [:session/facts "F"]
                   {:content "fact" :born "t1/i1/f1"}))
          value (sut/advance
                  {:tasks {:other {:title "Would otherwise be written"}}
                   :facts {:F {:depends_on [[:task "T"]]}}})]
      (expect (invalid-advance? #(sut/apply-advance base "t1/i2/f1" value)))
      (expect (nil? (get-in base [:session/tasks "other"])))
      (expect (nil? (get-in base [:session/facts "F" :depends_on])))))

  (it "rejects parent cycles in the final task tree"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/advance
                  {:tasks {:a {:title "A" :parent "b"}
                           :b {:title "B" :parent "a"}}})]
      (expect (invalid-advance? #(sut/apply-advance base "t1/i1/f1" value))))))
