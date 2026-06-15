(ns com.blockether.vis.internal.dag-expression-runtime-test
  (:require
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.dag-expression :as dag-expression]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.loop]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- private-var
  [sym]
  (ns-resolve 'com.blockether.vis.internal.loop sym))

(defn- invoke-dag
  [environment code]
  ((deref (private-var 'execute-dag-expression)) environment code 1000 nil))

(defn- test-environment
  ([] (test-environment {}))
  ([{:keys [blocking-answer? real-gate?]}]
   (let [ctx-atom (atom (ctx-engine/empty-ctx "runtime"))
         answer-atom (atom nil)
         best-answer-atom (atom nil)
         environment-atom (atom nil)
        ;; When blocking-answer? is true, answer-fn simulates a refused
        ;; finalization (open-plan-steps-block gate): it returns the reason
        ;; string and leaves answer-atom nil, so the turn does not close.
         gate-msg "Cannot finalize — 1 unresolved plan step(s): impl (done — needs :evidence)"
         env {:db-info :db
              :session/state-id :state
              :ctx-atom ctx-atom
              :answer-atom answer-atom
              :best-answer-atom best-answer-atom
              :answer-fn (cond
                           blocking-answer?
                           (fn [_answer] gate-msg)

                           real-gate?
                           (fn [answer]
                             (if-let [block-msg ((deref (private-var 'open-plan-steps-block))
                                                 (:session/tasks @ctx-atom))]
                               block-msg
                               (do
                                 (reset! answer-atom answer)
                                 {:status :accepted})))

                           :else
                           (fn [answer]
                             (reset! answer-atom answer)
                             {:status :accepted}))
              :workspace {:id :parent :root "/parent" :repo-root "/repo"}
              :workspace/id :parent
              :workspace/root "/parent"
              :workspace/context-roots []
              :environment-atom environment-atom
              :turn-state-atom (atom {:turn-position 2 :iteration 3 :form-idx 0})}]
     (reset! environment-atom env)
     env)))

(defn- run-advance
  "Build an advance value, stub out checkpoint/exec/accept, and invoke the DAG
   executor against `environment`. Returns the public receipt under :result."
  [environment advance]
  (let [checkpoint {:id :child :root "/child" :repo-root "/repo"
                    :parent-workspace-id :parent}]
    (with-redefs-fn
      {#'workspace/checkpoint-supported? (constantly true)
       #'workspace/checkpoint-create! (fn [_ _] checkpoint)
       (private-var 'execute-code-raw)
       (fn [& _] {:result advance :forms [{:result advance}] :error nil})
       #'workspace/checkpoint-accept!
       (fn [_ _]
         {:status :accepted
          :workspace checkpoint
          :diff {:parent-workspace-id :parent :changes []}})
       #'workspace/checkpoint-reject! (fn [& _] nil)}
      #(invoke-dag environment "advance({...})"))))

(defdescribe checkpointed-expression-runtime-test
  (it "runs nested effects in the child workspace and commits graph + pointer together"
    (let [environment (test-environment)
          environment-atom (:environment-atom environment)
          events (atom [])
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:graph {:tasks {:verify {:title "Verify" :status "done"}}}
                     :answer "Complete"})]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create!
         (fn [_ _]
           (swap! events conj :created)
           checkpoint)
         (private-var 'execute-code-raw)
         (fn [child-env _code & _]
           (swap! events conj [:executed (:workspace/id child-env)
                               (:workspace/id @environment-atom)])
           {:result advance :forms [{:result advance}] :error nil})
         #'workspace/checkpoint-accept!
         (fn [_ _]
           (swap! events conj :accepted)
           {:status :accepted
            :workspace checkpoint
            :diff {:parent-workspace-id :parent :changes []}})
         #'workspace/checkpoint-reject!
         (fn [& _] (swap! events conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= [:created [:executed :child :child] :accepted] @events))
            (expect (= :child (:workspace/id @environment-atom)))
            (expect (= :done (get-in @(:ctx-atom environment)
                               [:session/tasks "verify" :status])))
            ;; answer-alone does NOT close the turn (authority separation:
            ;; `answer` is narration, only :done true is terminal).
            (expect (nil? @(:answer-atom environment)))
            (expect (= "accepted" (get-in result [:result :status])))
            (expect (= "filesystem" (get-in result [:result :transaction_mode])))
            (expect (not (get-in result [:result :turn_closed]))))))))

  (it "executes read requests and records receipt observations without graph mutation"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "read-a"
                                 :tool "cat"
                                 :mode "read"
                                 :args ["a.clj"]
                                 :purpose "inspect file"}]})
          cat-result {:path "a.clj" :lines [[1 "(ns a)"]] :range [1 1]}
          calls (atom [])]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:cat #{:read :verify}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (swap! calls conj code)
           (if (= "advance({...})" code)
             {:result advance :forms [{:source code :result advance}] :error nil}
             {:result cat-result
              :forms [{:source code :result cat-result}]
              :error nil}))
         #'workspace/checkpoint-accept!
         (fn [_ _]
           {:status :accepted
            :workspace checkpoint
            :diff {:parent-workspace-id :parent :changes []}})
         #'workspace/checkpoint-reject! (fn [& _] nil)}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= ["advance({...})" "cat(\"a.clj\")"] @calls))
            (expect (= {} (:session/tasks @(:ctx-atom environment))))
            (expect (= [{:request_id "read-a"
                         :tool "cat"
                         :mode "read"
                         :purpose "inspect file"
                         :src "cat(\"a.clj\")"
                         :result cat-result}]
                      (get-in result [:result :observations])))
            (expect (= "accepted" (get-in result [:result :status]))))))))

  (it "commits verify failures as observations without accepted evidence"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "verify-tests"
                                 :tool "cat"
                                 :mode "verify"
                                 :args ["missing.txt"]
                                 :purpose "check test output"}]
                     :graph {:tasks {:verify {:status "doing"}}}})
          error {:message "missing.txt not found"}]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:cat #{:read :verify}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (if (= "advance({...})" code)
             {:result advance :forms [{:source code :result advance}] :error nil}
             {:result nil
              :forms [{:source code :error error}]
              :error error}))
         #'workspace/checkpoint-accept!
         (fn [_ _]
           {:status :accepted
            :workspace checkpoint
            :diff {:parent-workspace-id :parent :changes []}})
         #'workspace/checkpoint-reject! (fn [& _] nil)}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= error (get-in result [:result :observations 0 :error])))
            (expect (= [] (get-in result [:result :resolved_evidence])))
            (expect (= :doing (get-in @(:ctx-atom environment)
                                [:session/tasks "verify" :status]))))))))

  (it "accepts clj_eval verify requests through explicit request modes"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "eval-proof"
                                 :tool "clj_eval"
                                 :mode "verify"
                                 :args ["(+ 1 1)"]}]})
          calls (atom [])]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:clj/eval #{:verify}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (swap! calls conj code)
           (if (= "advance({...})" code)
             {:result advance :forms [{:source code :result advance}] :error nil}
             {:result {:value "2"}
              :forms [{:source code :result {:value "2"}}]
              :error nil}))
         #'workspace/checkpoint-accept!
         (fn [_ _]
           {:status :accepted
            :workspace checkpoint
            :diff {:parent-workspace-id :parent :changes []}})
         #'workspace/checkpoint-reject! (fn [& _] nil)}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= ["advance({...})" "clj_eval(\"(+ 1 1)\")"] @calls))
            (expect (= "eval-proof"
                      (get-in result [:result :observations 0 :request_id]))))))))

  (it "rejects verify on mutation-tag-compatible tools without verify request mode"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "bad-verify"
                                 :tool "patch"
                                 :mode "verify"
                                 :args [[]]}]})
          calls (atom [])]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:patch #{:write}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (swap! calls conj code)
           {:result advance :forms [{:source code :result advance}] :error nil})
         #'workspace/checkpoint-accept! (fn [& _] (swap! calls conj :accepted))
         #'workspace/checkpoint-reject! (fn [& _] (swap! calls conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= ["advance({...})" :rejected] @calls))
            (expect (some? (:error result)))
            (expect (re-find #"No requests executed"
                      (get-in result [:error :message]))))))))

  (it "prevalidates all requests before executing the first one"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "read-a"
                                 :tool "cat"
                                 :mode "read"
                                 :args ["a.clj"]}
                                {:request_id "bad"
                                 :tool "missing_tool"
                                 :mode "read"}]})
          calls (atom [])]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:cat #{:read :verify}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (swap! calls conj code)
           (if (= "advance({...})" code)
             {:result advance :forms [{:source code :result advance}] :error nil}
             {:result {:path "a.clj"} :forms [{:source code :result {:path "a.clj"}}]}))
         #'workspace/checkpoint-accept! (fn [& _] (swap! calls conj :accepted))
         #'workspace/checkpoint-reject! (fn [& _] (swap! calls conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= ["advance({...})" :rejected] @calls))
            (expect (some? (:error result)))
            (expect (nil? (get-in result [:result :observations]))))))))

  (it "rolls back a failed write request before graph commit"
    (let [environment (test-environment)
          environment-atom (:environment-atom environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "write-a"
                                 :tool "patch"
                                 :mode "write"
                                 :args [[{:path "a.clj" :search "x" :replace "y"}]]
                                 :purpose "edit file"}]
                     :graph {:facts {:edited {:content "should roll back"}}}})
          events (atom [])
          error {:message "patch failed"}]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:patch #{:write}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (if (= "advance({...})" code)
             {:result advance :forms [{:source code :result advance}] :error nil}
             {:result nil
              :forms [{:source code :error error}]
              :error error}))
         #'workspace/checkpoint-accept! (fn [& _] (swap! events conj :accepted))
         #'workspace/checkpoint-reject! (fn [& _] (swap! events conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= [:rejected] @events))
            (expect (some? (:error result)))
            (expect (= :parent (:workspace/id @environment-atom)))
            (expect (nil? (get-in @(:ctx-atom environment)
                            [:session/facts "edited"]))))))))

  (it "rejects the child and restores graph, answer, and environment on graph failure"
    (let [environment (test-environment)
          environment-atom (:environment-atom environment)
          base (-> @(:ctx-atom environment)
                 (assoc-in [:session/tasks "T"]
                   {:title "task" :born "t1/i1/f1" :depends_on [[:fact "F"]]})
                 (assoc-in [:session/facts "F"]
                   {:content "fact" :born "t1/i1/f1"}))
          _ (reset! (:ctx-atom environment) base)
          events (atom [])
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:graph {:tasks {:other {:title "Must roll back"}}
                             :facts {:F {:depends_on [[:task "T"]]}}}})]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         (private-var 'execute-code-raw)
         (fn [& _] {:result advance :forms [{:result advance}] :error nil})
         #'workspace/checkpoint-accept! (fn [& _] (swap! events conj :accepted))
         #'workspace/checkpoint-reject! (fn [& _] (swap! events conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= [:rejected] @events))
            (expect (some? (:error result)))
            (expect (= :parent (:workspace/id @environment-atom)))
            (expect (= base @(:ctx-atom environment)))
            (expect (nil? @(:answer-atom environment)))))))))

(defdescribe terminal-authority-test
  (it "does NOT close the turn on answer alone — the bank-account regression"
    ;; Mirrors eval 2026-06-13T205924-vis-dag/go-bank-account: an advance with
    ;; prose `answer` but no work and no :done must not finalize.
    (let [environment (test-environment)
          receipt (run-advance
                    environment
                    (dag-expression/advance {:answer "read both files"}))]
      (expect (nil? @(:answer-atom environment)))
      (expect (not (:turn_closed receipt)))))

  (it "closes the turn only when :done is explicitly true"
    (let [environment (test-environment)
          receipt (run-advance
                    environment
                    (dag-expression/advance
                      {:graph {:tasks {:impl {:status "done" :evidence "go test ok"}}}
                       :answer "Implemented and verified."
                       :finalization true}))]
      (expect (some? @(:answer-atom environment)))
      (expect (get-in receipt [:result :turn_closed]))
      (expect (= "go test ok"
                (get-in receipt [:result :resolved_evidence 0 :value])))
      (expect (true? (get-in receipt [:result :graph_diff :tasks "impl" :evidence_added])))))

  (it "closes terminal advances with a rendered answer_template"
    (let [environment (test-environment)
          receipt (run-advance
                    environment
                    (dag-expression/advance
                      {:graph {:tasks {:inspect {:status "done"
                                                 :evidence {:stat {:files 1 :add 2 :del 0}
                                                            :files [{:file "README.md"}]}}}}
                       :answer_template "Summary: {{tasks.inspect.evidence | git_diff_summary}}."
                       :finalization true}))]
      (expect (= "Summary: 1 file changed (+2/-0): README.md."
                @(:answer-atom environment)))
      (expect (get-in receipt [:result :turn_closed]))))

  (it "closes non-actionable dialogue through a tiny completed root task"
    (let [environment (test-environment {:real-gate? true})
          receipt (run-advance
                    environment
                    (dag-expression/advance
                      {:graph {:tasks {:respond {:status "done"
                                                 :title "Respond to greeting"
                                                 :evidence "User sent a greeting."}}}
                       :answer "Hey! What can I help you with?"
                       :finalization true}))]
      (expect (= "Hey! What can I help you with?" @(:answer-atom environment)))
      (expect (get-in receipt [:result :turn_closed]))
      (expect (= ["respond"] (get-in receipt [:result :tasks])))
      (expect (true? (get-in receipt [:result :graph_diff :tasks "respond" :evidence_added])))))

  (it "rejects terminal answer-only advances because every turn needs a graph"
    (let [environment (test-environment {:real-gate? true})
          receipt (run-advance
                    environment
                    (dag-expression/advance
                      {:answer "Hey."
                       :finalization true}))]
      (expect (nil? @(:answer-atom environment)))
      (expect (not (get-in receipt [:result :turn_closed])))
      (expect (some #(re-find #"plan is empty" (str %))
                (get-in receipt [:result :warnings])))))

  (it "does not close terminal advances without a non-blank answer"
    (let [environment (test-environment {:real-gate? true})
          receipt (run-advance
                    environment
                    (dag-expression/advance
                      {:graph {:tasks {:explore {:status "done"
                                                 :evidence "rg returned no files"}}}
                       :finalization true}))]
      (expect (nil? @(:answer-atom environment)))
      (expect (not (get-in receipt [:result :turn_closed])))
      (expect (some #(re-find #"non-blank rendered answer" (str %))
                (get-in receipt [:result :warnings])))))

  (it "still blocks empty-plan finalization for actionable DAG turns"
    (let [environment (test-environment {:real-gate? true})
          receipt (run-advance
                    environment
                    (dag-expression/advance
                      {:answer "Done."
                       :finalization true}))]
      (expect (nil? @(:answer-atom environment)))
      (expect (not (get-in receipt [:result :turn_closed])))
      (expect (some #(re-find #"plan is empty" (str %))
                (get-in receipt [:result :warnings])))))

  (it "refuses finalization when the plan gate blocks (done task lacks evidence)"
    ;; answer-fn simulates open-plan-steps-block refusing (returns the reason,
    ;; leaves answer-atom nil). Advance is still accepted (mutations persist)
    ;; but the turn does not close and the gate reason surfaces as a warning.
    (let [environment (test-environment {:blocking-answer? true})
          receipt (run-advance
                    environment
                    (dag-expression/advance
                      {:graph {:tasks {:impl {:status "done"}}}
                       :answer "done"
                       :finalization true}))]
      (expect (nil? @(:answer-atom environment)))
      (expect (not (get-in receipt [:result :turn_closed])))
      (expect (some #(re-find #"Cannot finalize" (str %))
                (get-in receipt [:result :warnings])))))

  (it "handles observation-only turns by rolling back the checkpoint and returning raw results"
    ;; When the code is bare cat('...') rather than advance, it executes read-only.
    ;; The child workspace checkpoint must be rejected (rolled back),
    ;; graph/answers are untouched, and the raw observation data returns.
    (let [environment (test-environment)
          environment-atom (:environment-atom environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          obs-result {:exit 0 :stdout "data"}
          events (atom [])]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [_ _] checkpoint)
         (private-var 'execute-code-raw)
         (fn [child-env _code & _]
           (swap! events conj [:executed (:workspace/id child-env)])
           {:result obs-result :forms [{:result obs-result}] :error nil})
         #'workspace/checkpoint-accept! (fn [& _] (swap! events conj :accepted))
         #'workspace/checkpoint-reject! (fn [& _] (swap! events conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "cat('file.go')")]
            (expect (= [[:executed :child] :rejected] @events))
            (expect (= :parent (:workspace/id @environment-atom)))
            (expect (= obs-result (:result result)))
            (expect (nil? @(:answer-atom environment)))
            (expect (nil? (get-in result [:result :turn_closed])))))))))

(defdescribe logical-expression-runtime-test
  (it "commits graph-only advances without a filesystem backend"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/parent" :repo-root "/repo"
                      :workspace-backend :live
                      :parent-workspace-id :parent}
          created-opts (atom nil)
          executed-env (atom nil)
          advance (dag-expression/advance
                    {:graph {:facts {:observed {:content "read-only result"}}}})]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly false)
         #'workspace/checkpoint-create!
         (fn [_ opts]
           (reset! created-opts opts)
           checkpoint)
         (private-var 'execute-code-raw)
         (fn [child-env _code & _]
           (reset! executed-env child-env)
           {:result advance :forms [{:result advance}] :error nil})
         #'workspace/checkpoint-accept!
         (fn [_ _]
           {:status :accepted
            :workspace checkpoint
            :diff {:parent-workspace-id :parent :changes []}})
         #'workspace/checkpoint-reject! (fn [& _] nil)}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (:logical? @created-opts))
            (expect (false? (:workspace/mutations-allowed? @executed-env)))
            (expect (= "logical" (get-in result [:result :transaction_mode])))
            (expect (= :child (:workspace/id @(:environment-atom environment)))))))))

  (it "fails write requests early when checkpoint support is logical-only"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/parent" :repo-root "/repo"
                      :workspace-backend :live
                      :checkpoint/warning "Filesystem checkpoint fork failed; falling back to logical checkpoint. Rift marker mismatch."
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "write-a"
                                 :tool "patch"
                                 :mode "write"
                                 :args [[]]}]})
          calls (atom [])]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:patch #{:write}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (swap! calls conj code)
           {:result advance :forms [{:source code :result advance}] :error nil})
         #'workspace/checkpoint-accept! (fn [& _] (swap! calls conj :accepted))
         #'workspace/checkpoint-reject! (fn [& _] (swap! calls conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= ["advance({...})" :rejected] @calls))
            (expect (some? (:error result)))
            (expect (re-find #"Rift marker"
                      (get-in result [:error :message]))))))))

  (it "surfaces logical transaction warnings for read advances after fork degradation"
    (let [environment (test-environment)
          checkpoint {:id :child :root "/parent" :repo-root "/repo"
                      :workspace-backend :live
                      :checkpoint/warning "Filesystem checkpoint fork failed; falling back to logical checkpoint. Rift marker mismatch."
                      :parent-workspace-id :parent}
          advance (dag-expression/advance
                    {:requests [{:request_id "read-a"
                                 :tool "cat"
                                 :mode "read"
                                 :args ["a.clj"]}]})]
      (with-redefs-fn
        {#'workspace/checkpoint-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         #'extension/request-mode-index (fn [] {:cat #{:read :verify}})
         (private-var 'execute-code-raw)
         (fn [_env code & _]
           (if (= "advance({...})" code)
             {:result advance :forms [{:source code :result advance}] :error nil}
             {:result {:path "a.clj"}
              :forms [{:source code :result {:path "a.clj"}}]
              :error nil}))
         #'workspace/checkpoint-accept!
         (fn [_ _]
           {:status :accepted
            :workspace checkpoint
            :diff {:parent-workspace-id :parent :changes []}})
         #'workspace/checkpoint-reject! (fn [& _] nil)}
        (fn []
          (let [result (invoke-dag environment "advance({...})")]
            (expect (= "logical" (get-in result [:result :transaction_mode])))
            (expect (re-find #"Rift marker"
                      (get-in result [:result :transaction_warning])))
            (expect (some #(re-find #"Rift marker" (str %))
                      (get-in result [:result :warnings]))))))))

  (it "requires isolation for child-agent coordinators"
    (let [environment (test-environment)
          result (with-redefs-fn
                   {#'workspace/checkpoint-supported? (constantly false)
                    #'workspace/workspace-capability-matrix (constantly [])}
                   #(try
                      (invoke-dag environment
                        "advance({'evidence': sub_loop('inspect')})")
                      nil
                      (catch clojure.lang.ExceptionInfo e
                        (ex-data e))))]
      (expect (= :vis/dag-isolation-required (:type result))))))
