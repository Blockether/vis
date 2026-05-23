(ns com.blockether.vis.internal.emit-surface-test
  "PLAN.md section 12 step 7 follow-up: extensions can emit
   :session/specs / :session/tasks / :session/facts from both
   hooks and SCI tool symbols, NOT just slashes. Each emit routes
   through ctx-loop/apply-and-record! so the engine FSM checks,
   dedup, and validator-fn satisfaction stay identical to a model-
   emitted (spec-set! ...) / (task-set! ...) / (fact-set! ...)."
  (:require
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- mock-env []
  {:ctx-atom        (ctx-loop/make-ctx-atom "test-soul")
   :turn-state-atom (ctx-loop/make-turn-state-atom)})

(defdescribe symbol-emit-test
  (it "symbol envelope :emit routes specs/tasks/facts through apply-and-record!"
    (extension/register-op! :demo {:tag :observation})
    (let [env     (mock-env)
          fired?  (atom false)
          tool-fn (fn []
                    (reset! fired? true)
                    (extension/success
                      {:result {:ok true}
                       :emit   {:specs {:design/api {:summary "API surface"}}
                                :tasks {:task/migrate {:title "migrate"
                                                       :status :todo}}
                                :facts {:fact/built {:value true}}}}))
          sym-entry (extension/symbol 'demo tool-fn
                      {:doc "demo emit symbol"
                       :arglists '([])
                       :render-fn (fn [_] [:ir {}])})
          ext (extension/extension
                {:ext/name "test.emit"
                 :ext/description "Symbol emit surface test."})]
      (extension/invoke-symbol-wrapper ext sym-entry [] env)
      (expect (true? @fired?))
      (let [ctx @(:ctx-atom env)]
        (expect (= "API surface"
                  (get-in ctx [:session/specs :design/api :summary])))
        (expect (= :todo
                  (get-in ctx [:session/tasks :task/migrate :status])))
        (expect (= true
                  (get-in ctx [:session/facts :fact/built :value])))
        (expect (= "t1/i1/f1"
                  (get-in ctx [:session/specs :design/api :born])))))))

(defdescribe hook-emit-test
  (it "iteration-start-hook-hit surfaces :emit alongside :task"
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)
          out (hit {:ext/name "test"} :h
                {:title "do thing"
                 :validator-fn "(fn [_] true)"
                 :emit {:specs {:design/seed {:summary "seed"}}}})]
      (expect (= :h (:id out)))
      (expect (= "do thing" (:title (:task out))))
      (expect (= {:specs {:design/seed {:summary "seed"}}} (:emit out)))))

  (it "hooks may return ONLY :emit (no hook-task) and still surface emits"
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)
          out (hit {:ext/name "test"} :h
                {:emit {:facts {:fact/seeded {:value 1}}}})]
      (expect (= :h (:id out)))
      (expect (nil? (:task out)))
      (expect (= {:facts {:fact/seeded {:value 1}}} (:emit out)))))

  (it "hook with neither :title nor :emit drops to nil"
    (let [hit @(ns-resolve 'com.blockether.vis.internal.loop
                 'iteration-start-hook-hit)]
      (expect (nil? (hit {:ext/name "test"} :h {})))
      (expect (nil? (hit {:ext/name "test"} :h {:foo :bar}))))))
