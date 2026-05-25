(ns com.blockether.vis.internal.consult-test
  "Unit tests for Phase H async consult layer. Router calls are mocked
   via with-redefs; we test the declarative request → engine processes
   between iters → fact in next-iter pattern, plus all bounded-failure
   paths (budget, recursion, preference, question shape)."
  (:require
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.consult :as consult]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env
  ([] (mk-env {}))
  ([overrides]
   (merge {:router ::router
           :ctx-atom (atom {:session/id "test" :session/turn 1
                            :session/scope {:turn 1 :iter 1 :next-form 1}
                            :session/facts {}})
           :consult-budget-atom (consult/fresh-budget-atom)
           :consult-config {:fast     {:provider :anthropic :model "haiku"}
                            :balanced {:provider :anthropic :model "sonnet"}
                            :deep     {:provider :anthropic :model "opus"}}
           :turn/system-prompt "PRIMARY-SYS"
           :turn/user-request  "user question text"}
     overrides)))

(defdescribe request-consult-test
  (describe "request-consult! pushes an intent to :engine/pending-consults"
    (let [env (mk-env)
          out (consult/request-consult! env :review :deep "should I do X?")]
      (it "returns :ok? true with :status :pending"
        (expect (true? (:ok? out)))
        (expect (= :pending (:status out))))

      (it "echoes :consult-id and :preference"
        (expect (= :review (:consult-id out)))
        (expect (= :deep (:preference out))))

      (it "writes the intent onto :engine/pending-consults"
        (let [pending (-> env :ctx-atom deref :engine/pending-consults)]
          (expect (= 1 (count pending)))
          (expect (= :review (-> pending first :consult-id)))
          (expect (= :deep (-> pending first :preference)))
          (expect (= "should I do X?" (-> pending first :question)))))

      (it "does NOT bump the budget at request time (only execution does)"
        (expect (= 0 (-> env :consult-budget-atom deref :used))))))

  (describe "request-consult! rejects bad inputs synchronously"
    (let [env (mk-env)]
      (it "invalid :consult-id (string) → :ok? false :invalid-consult-id"
        (let [out (consult/request-consult! env "not-a-kw" :fast "x")]
          (expect (false? (:ok? out)))
          (expect (= :invalid-consult-id (:error out)))))

      (it "unknown preference → :ok? false :unknown-preference"
        (let [out (consult/request-consult! env :K :nuclear "x")]
          (expect (false? (:ok? out)))
          (expect (= :unknown-preference (:error out)))))

      (it "empty question → :ok? false :empty-question"
        (let [out (consult/request-consult! env :K :fast "  ")]
          (expect (false? (:ok? out)))
          (expect (= :empty-question (:error out)))))

      (it "rejected intents do NOT touch :engine/pending-consults"
        (consult/request-consult! env :K :nuclear "x")
        (expect (empty? (-> env :ctx-atom deref :engine/pending-consults))))))

  (describe "request-consult! refuses when budget is exhausted"
    (let [env (mk-env {:consult-budget-atom (atom {:used 20 :cap 20})})
          out (consult/request-consult! env :K :fast "x")]
      (it ":ok? false :budget-exhausted"
        (expect (false? (:ok? out)))
        (expect (= :budget-exhausted (:error out))))
      (it "no intent enqueued"
        (expect (empty? (-> env :ctx-atom deref :engine/pending-consults)))))))

(defdescribe execute-pending-test
  (describe "execute-pending! drains intents and materialises facts"
    (let [env (mk-env)]
      (consult/request-consult! env :critique :fast "Critique my draft.")
      (consult/request-consult! env :research :deep "Research X.")

      (with-redefs [svar/ask-code!
                    (fn [_router opts]
                      (let [model (-> opts :routing :model)]
                        {:content (str "answer from " model)
                         :raw "raw"}))]
        (let [processed (consult/execute-pending! env)
              facts (-> env :ctx-atom deref :session/facts)]

          (it "returns the vec of processed consult-ids"
            (expect (= 2 (count processed))))

          (it "materialises each result under :session/facts :consult/<id>"
            (expect (= "answer from haiku"
                      (-> facts :consult/critique :content)))
            (expect (= "answer from opus"
                      (-> facts :consult/research :content))))

          (it "facts carry :status :active when consult succeeded"
            (expect (= :active (-> facts :consult/critique :status)))
            (expect (= :active (-> facts :consult/research :status))))

          (it ":consult-meta carries :preference + :call-no + :duration-ms"
            (let [m (-> facts :consult/critique :consult-meta)]
              (expect (= :fast (:preference m)))
              (expect (some? (:call-no m)))
              (expect (number? (:duration-ms m)))))

          (it "drains :engine/pending-consults to empty"
            (expect (empty? (-> env :ctx-atom deref :engine/pending-consults))))

          (it "bumps budget once per executed intent"
            (expect (= 2 (-> env :consult-budget-atom deref :used))))))))

  (describe "execute-pending! emits :status :failed facts on router error"
    (let [env (mk-env)]
      (consult/request-consult! env :bad :fast "x")
      (with-redefs [svar/ask-code! (fn [& _] (throw (ex-info "boom" {})))]
        (consult/execute-pending! env)
        (let [fact (-> env :ctx-atom deref :session/facts :consult/bad)]
          (it ":status :failed"
            (expect (= :failed (:status fact))))
          (it ":consult-meta :error :consult-error + :reason"
            (let [m (:consult-meta fact)]
              (expect (= :consult-error (:error m)))
              (expect (= "boom" (:reason m)))))))))

  (describe "execute-pending! is a no-op when nothing is pending"
    (let [env (mk-env)]
      (it "returns nil"
        (expect (nil? (consult/execute-pending! env)))))))
