(ns com.blockether.vis.internal.dag-expression-runtime-test
  (:require
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.dag-expression :as dag-expression]
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
  ([{:keys [blocking-answer?]}]
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
             :answer-fn (if blocking-answer?
                          (fn [_answer] gate-msg)
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

(defn- run-settlement
  "Build a settlement value, stub out rift/exec/accept, and invoke the DAG
   executor against `environment`. Returns the public receipt under :result."
  [environment settlement]
  (let [environment-atom (:environment-atom environment)
        checkpoint {:id :child :root "/child" :repo-root "/repo"
                    :parent-workspace-id :parent}]
    (with-redefs-fn
      {#'workspace/rift-supported? (constantly true)
       #'workspace/checkpoint-create! (fn [_ _] checkpoint)
       (private-var 'execute-code-raw)
       (fn [& _] {:result settlement :forms [{:result settlement}] :error nil})
       #'workspace/checkpoint-accept!
       (fn [_ _]
         {:status :accepted
          :workspace checkpoint
          :diff {:parent-workspace-id :parent :changes []}})
       #'workspace/checkpoint-reject! (fn [& _] nil)}
      #(invoke-dag environment "settle({...})"))))

(defdescribe checkpointed-expression-runtime-test
  (it "runs nested effects in the child workspace and commits graph + pointer together"
    (let [environment (test-environment)
          environment-atom (:environment-atom environment)
          events (atom [])
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          settlement (dag-expression/settlement
                       {:tasks {:verify {:title "Verify" :status "done"}}
                        :answer "Complete"})]
      (with-redefs-fn
        {#'workspace/rift-supported? (constantly true)
         #'workspace/checkpoint-create!
         (fn [_ _]
           (swap! events conj :created)
           checkpoint)
         (private-var 'execute-code-raw)
         (fn [child-env _code & _]
           (swap! events conj [:executed (:workspace/id child-env)
                               (:workspace/id @environment-atom)])
           {:result settlement :forms [{:result settlement}] :error nil})
         #'workspace/checkpoint-accept!
         (fn [_ _]
           (swap! events conj :accepted)
           {:status :accepted
            :workspace checkpoint
            :diff {:parent-workspace-id :parent :changes []}})
         #'workspace/checkpoint-reject!
         (fn [& _] (swap! events conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "settle({...})")]
            (expect (= [:created [:executed :child :child] :accepted] @events))
            (expect (= :child (:workspace/id @environment-atom)))
            (expect (= :done (get-in @(:ctx-atom environment)
                               [:session/tasks "verify" :status])))
            ;; answer-alone does NOT close the turn (authority separation:
            ;; `answer` is narration, only :done true is terminal).
            (expect (nil? @(:answer-atom environment)))
            (expect (= "accepted" (get-in result [:result :status])))
            (expect (not (get-in result [:result :turn_closed]))))))))

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
          settlement (dag-expression/settlement
                       {:tasks {:other {:title "Must roll back"}}
                        :facts {:F {:depends_on [[:task "T"]]}}})]
      (with-redefs-fn
        {#'workspace/rift-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [& _] checkpoint)
         (private-var 'execute-code-raw)
         (fn [& _] {:result settlement :forms [{:result settlement}] :error nil})
         #'workspace/checkpoint-accept! (fn [& _] (swap! events conj :accepted))
         #'workspace/checkpoint-reject! (fn [& _] (swap! events conj :rejected))}
        (fn []
          (let [result (invoke-dag environment "settle({...})")]
            (expect (= [:rejected] @events))
            (expect (some? (:error result)))
            (expect (= :parent (:workspace/id @environment-atom)))
            (expect (= base @(:ctx-atom environment)))
            (expect (nil? @(:answer-atom environment)))))))))

(defdescribe terminal-authority-test
  (it "does NOT close the turn on answer alone — the bank-account regression"
    ;; Mirrors eval 2026-06-13T205924-vis-dag/go-bank-account: a settle with
    ;; prose `answer` but no work and no :done must not finalize.
    (let [environment (test-environment)
          receipt (run-settlement
                    environment
                    (dag-expression/settlement {:answer "read both files"}))]
      (expect (nil? @(:answer-atom environment)))
      (expect (not (:turn_closed receipt)))))

  (it "closes the turn only when :done is explicitly true"
    (let [environment (test-environment)
          receipt (run-settlement
                    environment
                    (dag-expression/settlement
                      {:tasks {:impl {:status "done" :evidence "go test ok"}}
                       :answer "Implemented and verified."
                       :done true}))]
      (expect (some? @(:answer-atom environment)))
      (expect (get-in receipt [:result :turn_closed]))))

  (it "refuses finalization when the plan gate blocks (done task lacks evidence)"
    ;; answer-fn simulates open-plan-steps-block refusing (returns the reason,
    ;; leaves answer-atom nil). Settlement is still accepted (mutations persist)
    ;; but the turn does not close and the gate reason surfaces as a warning.
    (let [environment (test-environment {:blocking-answer? true})
          receipt (run-settlement
                    environment
                    (dag-expression/settlement
                      {:tasks {:impl {:status "done"}}
                       :answer "done"
                       :done true}))]
      (expect (nil? @(:answer-atom environment)))
      (expect (not (get-in receipt [:result :turn_closed])))
      (expect (some #(re-find #"Cannot finalize" (str %))
                    (get-in receipt [:result :warnings])))))

  (it "handles observation-only turns by rolling back the checkpoint and returning raw results"
    ;; When the code is bare cat('...') rather than settle, it executes read-only.
    ;; The child workspace checkpoint must be rejected (rolled back),
    ;; graph/answers are untouched, and the raw observation data returns.
    (let [environment (test-environment)
          environment-atom (:environment-atom environment)
          checkpoint {:id :child :root "/child" :repo-root "/repo"
                      :parent-workspace-id :parent}
          obs-result {:exit 0 :stdout "data"}
          events (atom [])]
      (with-redefs-fn
        {#'workspace/rift-supported? (constantly true)
         #'workspace/checkpoint-create! (fn [_ _] checkpoint)
         (private-var 'execute-code-raw)
         (fn [child-env code & _]
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
