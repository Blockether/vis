(ns com.blockether.vis.internal.foundation.language-surface-test
  (:require
   [com.blockether.vis.internal.foundation.language-surface :as language-surface]
   [com.blockether.vis.internal.resources :as resources]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- fake-env
  [handlers]
  {:session-id (str "ls-test-" (random-uuid))
   :env/project {:primary_language "clojure"}
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
            r    (language-surface/format-code env {:code "(+ 1 2)"})]
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
                   (:result (language-surface/run-tests env {:language "python" :ns "x"}))))))

  (it "passes clj_repl-shaped repl_start op and opts to language handlers"
      (let [env (fake-env [{:language :clojure
                            :start-repl-fn (fn [_ op opts]
                                             {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "restart" :opts {:dir "ext" :aliases ["dev"]}}
                   (:result (language-surface/start-repl env "restart" {:dir "ext" :aliases ["dev"]}))))
        (expect (= {:op :start :opts {:aliases ["dev"]}}
                   (:result (language-surface/start-repl env {:aliases ["dev"]}))))
        (expect (= {:op :start :opts {}}
                   (:result (language-surface/start-repl env))))))

  (it "accepts language-first calls for repl eval"
      (let [seen (atom nil)
            env  (fake-env [{:language :clojure
                             :repl-eval-fn (fn [_ arg]
                                             (reset! seen arg)
                                             {:success? true :result {:value "3"}})}])]
        (expect (= {:value "3"}
                   (:result (language-surface/repl-eval env "clojure" "(+ 1 2)"))))
        (expect (= "(+ 1 2)" @seen))))

  (it "passes language-first repl_start id and opts to language handlers"
      (let [env (fake-env [{:language :clojure
                            :start-repl-fn (fn [_ op opts]
                                             {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "restart" :opts {:id "main" :dir "ext"}}
                   (:result (language-surface/start-repl env "clojure" "main" "restart" {:dir "ext"}))))
        (expect (= {:op :start :opts {:id "main" :aliases ["dev"]}}
                   (:result (language-surface/start-repl env "clojure" {:id "main" :aliases ["dev"]}))))))

  (it "reports and stops repl resources through the resource model"
      (let [stopped? (atom false)
            env      (fake-env [])
            sid      (:session-id env)]
        (try
          (resources/register! sid
                               {:id "main-repl"
                                :kind :nrepl
                                :language :clojure
                                :label "main"}
                               {:stop-fn (fn [] (reset! stopped? true))})
          (expect (= ["main-repl"]
                     (mapv :id (get-in (language-surface/repl-status env "clojure") [:result :resources]))))
          (expect (= :stopped
                     (get-in (language-surface/repl-stop env "main-repl") [:result :result])))
          (expect (true? @stopped?))
          (expect (empty? (resources/list-resources sid)))
          (finally
            (resources/stop-all! sid)))))

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

(defdescribe capability-matrix-test
  (it "renders the facade verbs per ACTIVE language pack"
      (let [env {:active-extensions
                 (atom [{:ext/language-tools
                         [{:language :clojure :format-fn identity :test-fn identity
                           :repl-eval-fn identity :start-repl-fn identity}
                          {:language :python :repl-eval-fn identity :start-repl-fn identity}]}])}
            m   (language-surface/capability-matrix env)]
        (expect (str/includes? m "clojure : format_code · run_tests · repl_eval · repl_start"))
        (expect (str/includes? m "python : repl_eval · repl_start"))))
  (it "is nil when no language pack is active (nothing dead in the prompt)"
      (expect (nil? (language-surface/capability-matrix {:active-extensions (atom [{}])})))))

(defdescribe render-test-result-test
  (let [render #'language-surface/render-test-result]
    (it "passes render as ✓ with a pass/total headline and no body"
        (let [{:keys [summary body]} (render {:ns "foo-test" :pass 69 :total 69 :fail 0})]
          (expect (= "✓ foo-test — 69/69 passed" summary))
          (expect (nil? body))))
    (it "marks a failing run ✗ and surfaces the output"
        (let [{:keys [summary body]} (render {:ns "foo-test" :pass 60 :total 69 :fail 9 :output "9 failures"})]
          (expect (str/starts-with? summary "✗"))
          (expect (str/includes? summary "9 failed"))
          (expect (str/includes? body "9 failures"))))
    (it "surfaces the error text when the run could not produce a result"
        (let [{:keys [summary body]} (render {:error "could not parse test result"})]
          (expect (str/starts-with? summary "✗"))
          (expect (str/includes? summary "error"))
          (expect (str/includes? body "could not parse test result"))))
    (it "NEVER renders a blank card — even a degenerate empty result surfaces something"
        (doseq [r [{} {:ns nil :output ""} {:mode "repl" :port 7888}]]
          (let [{:keys [summary body]} (render r)]
            (expect (str/starts-with? summary "✗"))
            (expect (seq body)))))))
