(ns com.blockether.vis.internal.dag-expression-test
  (:require
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.dag-expression :as sut]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- invalid-advance?
  [f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= :vis/invalid-advance (:type (ex-data e))))))

(defdescribe source-contract-test
  (it "accepts one root advance expression with literal requests"
    (expect (nil? (sut/source-error
                    (str "# inspect and advance\n"
                      "advance({'requests': [{'request_id': 'r1', 'tool': 'rg', "
                      "'mode': 'read', 'args': [{'any': ['needle']}], "
                      "'purpose': 'inspect source'}], "
                      "'graph': {'tasks': {'verify': {'status': 'doing'}}}})")))))

  (it "rejects bare read-only observation call sequences"
    (expect (some? (sut/source-error "cat('a.go')")))
    (expect (some? (sut/source-error "cat('a.go')\ncat('b.go')")))
    (expect (some? (sut/source-error "ls()\nfind('.')"))))

  (it "rejects prose, assignments, and graph control mutations outside advance"
    (expect (some? (sut/source-error "x = 1")))
    (expect (some? (sut/source-error "x = cat('a.go')")))
    (expect (some? (sut/source-error "done('x')")))
    (expect (some? (sut/source-error "cat('a.go')\ndone('x')"))))

  (it "rejects every graph/control call that would mutate an observation turn"
    (doseq [call ["done" "fact_set" "plan_step" "settle" "summarize" "update_plan"]]
      (expect (some? (sut/source-error (str "cat('a.go')\n" call "('x')"))))))

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
                     "advance({'answer': 'prefix ' + suffix})"))))

  (it "rejects removed no_goal before nested calls can execute"
    (expect (some? (sut/source-error
                     (str "advance({'no_goal': True, "
                       "'tasks': {'status': {'evidence': git_status()}}})")))))

  (it "rejects terminal literal answers over fresh same-advance tool evidence"
    (expect (some? (sut/source-error
                     (str "advance({'graph': {'tasks': {'inspect': {'evidence': git_status()}}}, "
                       "'answer': 'Working tree is clean.', 'finalization': True})"))))))

(defdescribe advance-validation-test
  (it "tags a valid advance"
    (let [result (sut/advance {:graph {:tasks {:verify {:status "done"}}}
                               :answer "Complete"})]
      (expect (true? (:vis_advance result)))
      (expect (= "Complete" (:answer result)))))

  (it "rejects unknown keys, invalid entity maps, and unstable ids"
    (expect (invalid-advance? #(sut/advance {:unknown true})))
    (expect (invalid-advance? #(sut/advance {:graph {:tasks {:x "done"}}})))
    (expect (invalid-advance? #(sut/advance {:graph {:facts {42 {:content "x"}}}}))))

  (it "rejects ambiguous answer fields"
    (expect (invalid-advance?
              #(sut/advance {:answer "literal"
                             :prose "also literal"})))
    (expect (invalid-advance?
              #(sut/advance {:answer "ok"
                             :no_goal true})))))

(defdescribe terminal-flag-validation-test
  (it "accepts and threads an explicit finalization flag"
    (let [result (sut/advance {:graph {:tasks {:verify {:status "done"}}}
                               :answer "Complete" :finalization true})]
      (expect (true? (:done result)))
      (expect (= "Complete" (:answer result)))))

  (it "defaults :done to false when absent"
    (let [result (sut/advance {:graph {:tasks {:verify {:status "done"}}}})]
      (expect (false? (:done result)))))

  (it "rejects an invalid finalization flag"
    (expect (invalid-advance? #(sut/advance {:answer "x" :finalization "yes"}))))

  (it "rejects the removed :no_goal flag"
    (expect (invalid-advance?
              #(sut/advance {:answer "Hey! What can I help you with?"
                             :no_goal true})))))

(defdescribe literal-request-test
  (it "rejects inline sandbox calls before Python can evaluate them"
    (expect (some? (sut/source-error
                     "advance({'graph': {'tasks': {'verify': {'status': 'done', 'evidence': probe()}}}, 'answer': 'ok'})")))))

(defdescribe graph-transaction-test
  (it "applies task and fact updates as one pure advance"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/advance
                  {:graph {:tasks {:verify {:title "Verify" :status "done"
                                            :evidence {:exit 0}}}
                           :facts {:result {:content "Tests passed"}}}
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

  (it "rejects citations to observations absent from context and the accepted receipt"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/advance
                  {:graph {:tasks {:verify {:status "done"}}}
                   :citations [{:target [:task "verify"]
                                :observation "from_failed_validation"}]})]
      (expect (invalid-advance? #(sut/apply-advance base "t1/i1/f1" value [])))))

  (it "accepts citations to current request observations"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/advance
                  {:graph {:tasks {:verify {:status "done"}}}
                   :citations [{:target [:task "verify"]
                                :observation "verify-tests"}]})
          {:keys [receipt]} (sut/apply-advance base "t1/i1/f1" value
                              [{:request_id "verify-tests"
                                :result {:stdout "ok"}}])]
      (expect (= [{:target [:task "verify"] :observation "verify-tests"}]
                (:citations receipt)))))

  (it "rejects a cycle without exposing the partially updated graph"
    (let [base (-> (ctx-engine/empty-ctx "scenario")
                 (assoc-in [:session/tasks "T"]
                   {:title "task" :born "t1/i1/f1" :depends_on [[:fact "F"]]})
                 (assoc-in [:session/facts "F"]
                   {:content "fact" :born "t1/i1/f1"}))
          value (sut/advance
                  {:graph {:tasks {:other {:title "Would otherwise be written"}}
                           :facts {:F {:depends_on [[:task "T"]]}}}})]
      (expect (invalid-advance? #(sut/apply-advance base "t1/i2/f1" value)))
      (expect (nil? (get-in base [:session/tasks "other"])))
      (expect (nil? (get-in base [:session/facts "F" :depends_on])))))

  (it "rejects parent cycles in the final task tree"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/advance
                  {:graph {:tasks {:a {:title "A" :parent "b"}
                                   :b {:title "B" :parent "a"}}}})]
      (expect (invalid-advance? #(sut/apply-advance base "t1/i1/f1" value))))))
