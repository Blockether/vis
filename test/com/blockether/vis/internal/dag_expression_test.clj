(ns com.blockether.vis.internal.dag-expression-test
  (:require
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.dag-expression :as sut]
   [com.blockether.vis.internal.env-python :as env]
   [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- invalid-settlement?
  [f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= :vis/invalid-settlement (:type (ex-data e))))))

(defdescribe source-contract-test
  (it "accepts one root settle expression with nested evidence calls"
    (expect (nil? (sut/source-error
                    "# inspect and settle\nsettle({'tasks': {'verify': {'evidence': probe()}}})"))))

  (it "accepts bare read-only observation call sequences"
    (expect (nil? (sut/source-error "cat('a.go')")))
    (expect (nil? (sut/source-error "cat('a.go')\ncat('b.go')")))
    (expect (nil? (sut/source-error "ls()\nfind('.')"))))

  (it "rejects prose, assignments, and graph control mutations outside settle"
    (expect (some? (sut/source-error "x = 1")))
    (expect (some? (sut/source-error "x = cat('a.go')")))
    (expect (some? (sut/source-error "done('x')")))
    (expect (some? (sut/source-error "cat('a.go')\ndone('x')"))))

  (it "rejects statements, sibling expressions, and non-settle roots when settle is present"
    (expect (some? (sut/source-error "x = settle({'answer': 'x'})")))
    (expect (some? (sut/source-error "settle({'answer': 'x'})\nprobe()"))))

  (it "rejects nested graph and control calls"
    (expect (some? (sut/source-error "settle({'answer': done('x')})")))
    (expect (some? (sut/source-error
                     "settle({'tasks': {'x': plan_step('x', {'status': 'done'})}})")))
    (expect (some? (sut/source-error "settle({'answer': settle({'answer': 'x'})})")))))

(defdescribe settlement-validation-test
  (it "tags a valid settlement"
    (let [result (sut/settlement {:tasks {:verify {:status "done"}}
                                  :answer "Complete"})]
      (expect (true? (:vis_settlement result)))
      (expect (= "Complete" (:answer result)))))

  (it "rejects unknown keys, invalid entity maps, and unstable ids"
    (expect (invalid-settlement? #(sut/settlement {:unknown true})))
    (expect (invalid-settlement? #(sut/settlement {:tasks {:x "done"}})))
    (expect (invalid-settlement? #(sut/settlement {:facts {42 {:content "x"}}})))))

(defdescribe terminal-flag-validation-test
  (it "accepts and threads an explicit :done terminal flag"
    (let [result (sut/settlement {:tasks {:verify {:status "done"}}
                                  :answer "Complete" :done true})]
      (expect (true? (:done result)))
      (expect (= "Complete" (:answer result)))))

  (it "defaults :done to false when absent"
    (let [result (sut/settlement {:tasks {:verify {:status "done"}}})]
      (expect (false? (:done result)))))

  (it "rejects a non-boolean :done flag"
    (expect (invalid-settlement? #(sut/settlement {:answer "x" :done "yes"})))))

(defdescribe eager-evidence-evaluation-test
  (it "evaluates an inline sandbox call before settle receives the payload"
    (let [calls (atom 0)
          {:keys [^Context python-context]}
          (env/create-python-context
            {'probe (fn []
                      (swap! calls inc)
                      {:exit 0 :stdout "verified"})
             'settle sut/settlement})]
      (try
        (let [{:keys [error result]}
              (env/run-python-block python-context
                "settle({'tasks': {'verify': {'status': 'done', 'evidence': probe()}}, 'answer': 'ok'})")]
          (expect (nil? error))
          (expect (= 1 @calls))
          (expect (true? (:vis_settlement result)))
          (expect (= "verified" (get-in result [:tasks :verify :evidence :stdout]))))
        (finally
          (.close python-context true))))))

(defdescribe graph-transaction-test
  (it "applies task and fact updates as one pure settlement"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/settlement
                  {:tasks {:verify {:title "Verify" :status "done"
                                    :evidence {:exit 0}}}
                   :facts {:result {:content "Tests passed"}}
                   :answer "Complete"})
          {:keys [ctx receipt]} (sut/apply-settlement base "t1/i1/f1" value)]
      (expect (= :done (get-in ctx [:session/tasks "verify" :status])))
      (expect (= "{:exit 0}" (get-in ctx [:session/tasks "verify" :evidence])))
      (expect (= "Tests passed" (get-in ctx [:session/facts "result" :content])))
      (expect (= {:tasks ["verify"] :facts ["result"] :answered? true}
                receipt))))

  (it "rejects a cycle without exposing the partially updated graph"
    (let [base (-> (ctx-engine/empty-ctx "scenario")
                 (assoc-in [:session/tasks "T"]
                   {:title "task" :born "t1/i1/f1" :depends_on [[:fact "F"]]})
                 (assoc-in [:session/facts "F"]
                   {:content "fact" :born "t1/i1/f1"}))
          value (sut/settlement
                  {:tasks {:other {:title "Would otherwise be written"}}
                   :facts {:F {:depends_on [[:task "T"]]}}})]
      (expect (invalid-settlement? #(sut/apply-settlement base "t1/i2/f1" value)))
      (expect (nil? (get-in base [:session/tasks "other"])))
      (expect (nil? (get-in base [:session/facts "F" :depends_on])))))

  (it "rejects parent cycles in the final task tree"
    (let [base (ctx-engine/empty-ctx "scenario")
          value (sut/settlement
                  {:tasks {:a {:title "A" :parent "b"}
                           :b {:title "B" :parent "a"}}})]
      (expect (invalid-settlement? #(sut/apply-settlement base "t1/i1/f1" value))))))
