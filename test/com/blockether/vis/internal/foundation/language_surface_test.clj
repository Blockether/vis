(ns com.blockether.vis.internal.foundation.language-surface-test
  (:require
   [com.blockether.vis.internal.foundation.language-surface :as language-surface]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- fake-env
  [handlers]
  {:env/project {:primary_language "clojure"}
   :extensions (atom [{:ext/name "fake-clj"
                       :ext/language-tools handlers}])})

(defdescribe language-surface-dispatch-test
  (it "dispatches format to the active language handler"
    (let [seen (atom nil)
          env  (fake-env [{:language :clojure
                           :format-fn (fn [_ arg]
                                        (reset! seen arg)
                                        {:success? true
                                         :result {:op :fake-format
                                                  :text (:code arg)}})}])
          r    (language-surface/format env {:code "(+ 1 2)"})]
      (expect (= {:code "(+ 1 2)"} @seen))
      (expect (= {:op :fake-format :text "(+ 1 2)"} (:result r)))))

  (it "uses an explicit language to disambiguate handlers"
    (let [env (fake-env [{:language :clojure
                          :test-fn (fn [_ arg]
                                     {:success? true :result {:language :clojure :arg arg}})}
                         {:language :python
                          :test-fn (fn [_ arg]
                                     {:success? true :result {:language :python :arg arg}})}])]
      (expect (= {:language :python :arg {:language "python" :ns "x"}}
                (:result (language-surface/test env {:language "python" :ns "x"}))))))

  (it "passes clj_repl-shaped start_repl op and opts to language handlers"
    (let [env (fake-env [{:language :clojure
                          :start-repl-fn (fn [_ op opts]
                                           {:success? true :result {:op op :opts opts}})}])]
      (expect (= {:op "restart" :opts {:dir "ext" :aliases ["dev"]}}
                (:result (language-surface/start-repl env "restart" {:dir "ext" :aliases ["dev"]}))))
      (expect (= {:op :start :opts {:aliases ["dev"]}}
                (:result (language-surface/start-repl env {:aliases ["dev"]}))))
      (expect (= {:op :start :opts {}}
                (:result (language-surface/start-repl env))))))

  (it "reports missing language handlers with available languages"
    (let [env (fake-env [{:language :clojure
                          :repl-eval-fn (fn [_ _]
                                          {:success? true :result :ok})}])]
      (expect (= :language-surface/no-language-handler
                (try
                  (language-surface/repl-eval env {:language "python" :code "1"})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (-> e ex-data :type))))))))
