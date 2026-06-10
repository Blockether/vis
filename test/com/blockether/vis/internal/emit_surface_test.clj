(ns com.blockether.vis.internal.emit-surface-test
  "Extensions can emit :session/tasks / :session/facts from both hooks
   and SCI tool symbols, NOT just slashes. Each emit routes through
   ctx-loop/apply-and-record! so the engine FSM checks, dedup, and
   dependency invariants stay identical to a model-emitted
   (task-set! ...) / (fact-set! ...)."
  (:require
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- mock-env []
  {:ctx-atom        (ctx-loop/make-ctx-atom "test-soul")
   :turn-state-atom (ctx-loop/make-turn-state-atom)})

(def ^:dynamic *fired?* nil)

(defn emit-tool
  "demo emit symbol"
  []
  (when *fired?* (reset! *fired?* true))
  (extension/success
    {:result {:ok true}
     :emit   {:tasks {"task/migrate" {:title "migrate"
                                      :status :todo}}
              :facts {"fact/built" {:content "built ok"}}}}))

(defdescribe symbol-emit-test
  (it "symbol envelope :emit routes tasks/facts through apply-and-record!"
    (let [env       (mock-env)
          fired?    (atom false)
          sym-entry (extension/symbol #'emit-tool
                      {:symbol 'demo
                       :tag :observation
                       :render-fn (fn [_] [:ir {}])})
          ext (extension/extension
                {:ext/name "test.emit"
                 :ext/description "Symbol emit surface test."})]
      (binding [*fired?* fired?]
        (extension/invoke-symbol-wrapper ext sym-entry [] env))
      (expect (true? @fired?))
      (let [ctx @(:ctx-atom env)]
        (expect (= :todo
                  (get-in ctx [:session/tasks "task/migrate" :status])))
        (expect (= "migrate"
                  (get-in ctx [:session/tasks "task/migrate" :title])))
        (expect (= "built ok"
                  (get-in ctx [:session/facts "fact/built" :content])))
        (expect (= "t1/i1/f1"
                  (get-in ctx [:session/tasks "task/migrate" :born])))))))

(defdescribe hook-emit-test
  (it "iteration-start-hook-hit surfaces :emit alongside :task"
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)
          out (hit {:ext/name "test"} :h nil
                {:title "do thing"
                 :emit {:facts {"fact/seed" {:content "seed"}}}})]
      (expect (= :h (:id out)))
      (expect (= "do thing" (:title (:task out))))
      (expect (= {:facts {"fact/seed" {:content "seed"}}} (:emit out)))))

  (it "stamps :lifetime onto the hook-task when the hook spec declares one"
    ;; Lifetime threads from the hook registration spec into the task
    ;; so gc-pass can prune `:lifetime :turn` entries at advance-turn
    ;; without consulting the registry at GC time.
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)
          out (hit {:ext/name "test"} :h :turn
                {:title "warn"})]
      (expect (= :turn (:lifetime (:task out))))))

  (it "omits :lifetime when the hook spec does not declare one (back-compat)"
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)
          out (hit {:ext/name "test"} :h nil
                {:title "warn"})]
      (expect (not (contains? (:task out) :lifetime)))))

  (it "hooks may return ONLY :emit (no hook-task) and still surface emits"
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)
          out (hit {:ext/name "test"} :h nil
                {:emit {:facts {"fact/seeded" {:content "1"}}}})]
      (expect (= :h (:id out)))
      (expect (nil? (:task out)))
      (expect (= {:facts {"fact/seeded" {:content "1"}}} (:emit out)))))

  (it "hook with neither :title nor :emit drops to nil"
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)]
      (expect (nil? (hit {:ext/name "test"} :h nil {})))
      (expect (nil? (hit {:ext/name "test"} :h nil {:foo :bar}))))))
