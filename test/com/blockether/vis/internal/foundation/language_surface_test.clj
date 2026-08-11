(ns com.blockether.vis.internal.foundation.language-surface-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.environment.core :as environment]
            [com.blockether.vis.internal.foundation.language-surface :as language-surface]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [com.blockether.vis.internal.resources :as resources]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- fake-env
  [handlers]
  {:session-id (str "ls-test-" (random-uuid))
   :jail-policy-fn (constantly {:roots-fn (constantly [(System/getProperty "java.io.tmpdir")])
                                :net-enabled? false})
   :env/project {:primary_language "clojure"}
   :env/languages {:languages []}
   :extensions (atom [{:ext/name "fake-clj" :ext/language-tools handlers}])})

(defdescribe
  language-surface-dispatch-test
  (it "dispatches format to the active language handler"
      (let
        [seen
         (atom nil)

         env
         (fake-env [{:language "clojure"
                     :format-fn (fn [_ arg]
                                  (reset! seen arg)
                                  {:success? true
                                   :result {:op :fake-format :text (get arg "code")}})}])

         r
         (language-surface/format-code env {"code" "(+ 1 2)"})]

        (expect (= {"code" "(+ 1 2)"} @seen))
        (expect (= {:op :fake-format :text "(+ 1 2)"} (:result r)))))
  (it "uses an explicit language to disambiguate handlers"
      (let
        [env
         (fake-env [{:language "clojure"
                     :test-fn (fn [_ arg]
                                {:success? true :result {:language "clojure" :arg arg}})}
                    {:language "python"
                     :test-fn (fn [_ arg]
                                {:success? true :result {:language "python" :arg arg}})}])

         result
         (:result (language-surface/run-tests env {"language" "python" "ns" "x"}))]

        (expect (= "python" (:language result)))
        (expect (= {"language" "python" "ns" "x"} (:arg result)))))
  (it "puts the completed verdict and elapsed time inside the public test result"
      (let
        [env
         (fake-env [{:language "clojure"
                     :test-fn (fn [_ _]
                                {:success? true :result {"pass" 2 "fail" 0}})}])

         envelope
         (language-surface/run-tests env {})

         result
         (:result envelope)]

        (expect (true? (get result "is_pass")))
        (expect (= 2 (get result "total")))
        (expect (nat-int? (get result "ms")))
        (expect (nil? (get envelope "ms")))))
  (it "passes clj_repl-shaped repl op and opts to language handlers"
      (let
        [env (fake-env [{:language "clojure"
                         :start-repl-fn (fn [_ op opts]
                                          {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "connect" :opts {"cwd" "ext" "aliases" ["dev"]}}
                   (:result
                     (language-surface/start-repl env "connect" {"cwd" "ext" "aliases" ["dev"]}))))
        (expect (= {:op "start" :opts {"aliases" ["dev"]}}
                   (:result (language-surface/start-repl env {"aliases" ["dev"]}))))
        (expect (= {:op "status" :opts {"cwd" "ext"}}
                   (:result (language-surface/start-repl env {"op" "status" "cwd" "ext"}))))
        (expect (= {:op "start" :opts {}} (:result (language-surface/start-repl env))))))
  (it "REFUSES the removed `restart` op on every arity instead of silently starting a REPL"
      (let
        [env
         (fake-env [{:language "clojure"
                     :start-repl-fn (fn [_ op opts]
                                      {:success? true :result {:op op :opts opts}})}])

         ;; `restart` must stay RECOGNIZED as an op: parsed as a repl *id* it
         ;; would silently start a REPL, which is the bug we are removing.
         refuses
         (fn [& args]
           (try (apply language-surface/start-repl env args)
                nil
                (catch clojure.lang.ExceptionInfo e (ex-data e))))]

        (expect (= :language-surface/removed-op (:type (refuses "restart" {"cwd" "ext"}))))
        (expect (= :language-surface/removed-op (:type (refuses "clojure" "restart" {}))))
        (expect (= :language-surface/removed-op (:type (refuses "clojure" "main" "restart" {}))))
        (expect (= :language-surface/removed-op (:type (refuses {"op" "restart" "cwd" "ext"}))))
        (expect (= ["connect" "start" "status" "stop"] (:allowed (refuses "restart"))))))
  (it "documents the explicit REPL lifecycle in its own doc text"
      (let
        [start
         (:ext.symbol/description language-surface/start-repl-symbol)

         result
         (:ext.symbol/result language-surface/start-repl-symbol)

         stop
         (:ext.symbol/description language-surface/repl-stop-symbol)]

        ;; Regression, issue #ctx-resources: the doc used to send the model to
        ;; `session["resources"]["repls"][language][cwd]`, a ctx key that no longer exists.
        (expect (not (str/includes? start "session[\"resources\"]")))
        (expect (str/includes? start "`status` is the only answer"))
        (expect (str/includes? start "absent/down/failed"))
        (expect (str/includes? start "`stop` ends a managed REPL"))
        (expect (str/includes? start "`status` reports that directory's state"))
        (expect (str/includes? result "never a `{resources: [...]}` list"))
        (expect (str/includes? result "stamped with `op`"))
        (expect (str/includes? (:ext.symbol/result language-surface/repl-eval-symbol)
                               "stamped with `op`"))
        (expect (str/includes? (:ext.symbol/result language-surface/test-symbol)
                               "absent fields mean not applicable"))
        (expect (not (str/includes? (:ext.symbol/result language-surface/test-symbol)
                                    "always present")))
        (expect (str/includes? stop "managed REPL you started"))
        (expect (str/includes? stop "never killed"))))
  (it "accepts language-first calls for repl eval"
      (let
        [seen
         (atom nil)

         env
         (fake-env [{:language "clojure"
                     :repl-eval-fn (fn [_ arg]
                                     (reset! seen arg)
                                     {:success? true :result {:value "3"}})}])]

        (expect (= {:value "3"} (:result (language-surface/repl-eval env "clojure" "(+ 1 2)"))))
        (expect (= "(+ 1 2)" @seen))))
  (it "passes language-first repl id and opts to language handlers"
      (let
        [env (fake-env [{:language "clojure"
                         :start-repl-fn (fn [_ op opts]
                                          {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "connect" :opts {"id" "main" "cwd" "ext"}}
                   (:result
                     (language-surface/start-repl env "clojure" "main" "connect" {"cwd" "ext"}))))
        (expect (= {:op "start" :opts {"id" "main" "aliases" ["dev"]}}
                   (:result (language-surface/start-repl env
                                                         "clojure"
                                                         {"id" "main" "aliases" ["dev"]}))))))
  (it "reports and stops repl resources through the resource model"
      (let
        [stopped?
         (atom false)

         env
         (fake-env [])

         sid
         (:session-id env)]

        (try (resources/register! sid
                                  {:id "main-repl" :kind :nrepl :language "clojure" :label "main"}
                                  {:stop-fn (fn []
                                              (reset! stopped? true))})
             (expect (= ["main-repl"]
                        (mapv #(get % "id")
                              (get-in (language-surface/repl-status env "clojure")
                                      [:result "resources"]))))
             (expect (= "stopped"
                        (get-in (language-surface/repl-stop env "main-repl") [:result "result"])))
             (expect (true? @stopped?))
             (expect (empty? (resources/list-resources sid)))
             (finally (resources/stop-all! sid)))))
  (it "reports missing language handlers with available languages"
      (let
        [env (fake-env [{:language "clojure"
                         :repl-eval-fn (fn [_ _]
                                         {:success? true :result :ok})}])]
        (expect (= :language-surface/no-language-handler
                   (try (language-surface/repl-eval env {"language" "python" "code" "1"})
                        nil
                        (catch clojure.lang.ExceptionInfo e
                          (-> e
                              ex-data
                              :type))))))))

(defn- scan-env
  "fake-env variant carrying an explicit workspace primary + a scanned language
   roll-up (file-count order), for exercising handler-resolution heuristics."
  [primary scanned handlers]
  {:session-id (str "ls-test-" (random-uuid))
   :jail-policy-fn (constantly {:roots-fn (constantly [(System/getProperty "java.io.tmpdir")])
                                :net-enabled? false})
   :env/project {:primary_language primary}
   :env/languages {:languages (mapv (fn [l]
                                      {:language l})
                                    scanned)}
   :extensions (atom [{:ext/name "fake" :ext/language-tools handlers}])})

(defn- echo-lang-handler
  [language]
  {:language language
   :repl-eval-fn (fn [_ _]
                   {:success? true :result {:language language}})})

(defn- resolved-language
  [env & args]
  (get-in (apply language-surface/repl-eval env (concat args ["1"])) [:result :language]))

(defn- error-type [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defdescribe
  language-resolution-heuristics-test
  (it "falls through a data primary to the first REAL code language a pack handles"
      ;; json dominates by file count but has no pack; the ts pack still resolves
      ;; a BARE repl_eval — this is the 'couldn't use it' fix.
      (let
        [env (scan-env "json"
                       ["json" "typescript" "clojure"]
                       [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= "typescript" (resolved-language env)))))
  (it "prefers the workspace primary over other scanned languages"
      (let
        [env (scan-env "clojure"
                       ["clojure" "typescript"]
                       [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= "clojure" (resolved-language env)))))
  (it "uses the file-count primary from the workspace snapshot when tool env metadata is absent"
      (let
        [env (dissoc (scan-env nil [] [(echo-lang-handler "python") (echo-lang-handler "clojure")])
               :env/project
               :env/languages)]
        (with-redefs
          [environment/snapshot (constantly {:languages {:primary "clojure"
                                                         :languages
                                                         [{:language "clojure" :files 260}
                                                          {:language "python" :files 47}]}})]
          (expect (= "clojure" (resolved-language env))))))
  (it "resolves a grammar variant to its base family handler via the alias map"
      ;; a pack registering only 'typescript'/'javascript' still serves tsx/jsx.
      (let
        [env (scan-env "json"
                       ["json"]
                       [(echo-lang-handler "typescript") (echo-lang-handler "javascript")])]
        (expect (= "typescript" (resolved-language env "tsx")))
        (expect (= "javascript" (resolved-language env "jsx")))
        (expect (= "typescript" (resolved-language env "mts")))))
  (it "still errors on an EXPLICIT unsupported language (no silent fallback)"
      (let [env (scan-env "json" ["json" "typescript"] [(echo-lang-handler "typescript")])]
        (expect (= :language-surface/no-language-handler
                   (error-type #(language-surface/repl-eval env {"language" "rust" "code" "1"}))))))
  (it "asks for a language when several packs match and none can be inferred"
      (let
        [env (scan-env "json"
                       ["json"]
                       [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= :language-surface/ambiguous-language
                   (error-type #(language-surface/repl-eval env {"code" "1"})))))))

(defdescribe
  capability-matrix-test
  (it "renders the facade verbs per ACTIVE language pack"
      (let
        [env
         {:active-extensions (atom [{:ext/language-tools [{:language "clojure"
                                                           :format-fn identity
                                                           :test-fn identity
                                                           :repl-eval-fn identity
                                                           :start-repl-fn identity}
                                                          {:language "python"
                                                           :repl-eval-fn identity
                                                           :start-repl-fn identity}]}])}

         m
         (language-surface/capability-matrix env)]

        (expect (str/includes? m "clojure : format_code · run_tests · repl_eval · repl"))
        (expect (str/includes? m "python : repl_eval · repl"))
        (expect (str/includes? m "reuse the managed REPL and execute its already-loaded Vars"))
        (expect (str/includes? m "do NOT reload namespaces automatically"))
        (expect (str/includes? m "tests may exercise stale code"))
        (expect (str/includes? m "every changed production and test namespace"))
        (expect (str/includes? m "prefer a FRESH REPL (stop, then start) over `:reload-all`"))
        (expect (not (str/includes? m "session[\"resources\"]")))
        (expect (not (str/includes? m "Keep managed REPLs alive")))))
  (it "is nil when no language pack is active (nothing dead in the prompt)"
      (expect (nil? (language-surface/capability-matrix {:active-extensions (atom [{}])})))))

;; Regression, issue #133: the finished headline had no notion of what the CALL
;; asked for, so it could not tell two runs apart.
(defdescribe test-target-test
             (let [target #'language-surface/test-target]
               (it "reports the narrowest selection the call made"
                   (expect (= "foo-test/a, foo-test/b"
                              (target {"only" ["foo-test/a" "foo-test/b"]
                                       "namespaces" ["foo-test"]})))
                   (expect (= "foo-test" (target {"namespaces" ["foo-test"] "paths" ["test"]})))
                   (expect (= "test/foo" (target {"paths" ["test/foo"]})))
                   (expect (= "filter: slow" (target {"filter" "slow"}))))
               (it "falls back to the whole suite when nothing narrows the run"
                   (expect (= "full suite" (target {})))
                   (expect (= "full suite" (target {"paths" [] "only" [nil ""] "filter" "   "}))))))

(defdescribe
  language-process-jail-refresh-test
  (it "refreshes the session jail before a test handler launches a process"
      (let
        [env
         (fake-env [{:language "clojure"
                     :test-fn (fn [handler-env _]
                                {:success? true
                                 :result {:launch? (boolean (seq (:argv (vis/session-process-launch
                                                                          (:session-id handler-env)
                                                                          ["clojure"
                                                                           "-Sdescribe"]))))}})}])

         session-id
         (:session-id env)

         result
         (try (language-surface/run-tests env {})
              (finally (process-jail/unregister-session-jail! session-id)))]

        (expect (true? (get-in result [:result :launch?])))))
  (it "refreshes the session jail before repl_eval can auto-start a REPL"
      (let
        [env
         (fake-env [{:language "clojure"
                     :repl-eval-fn
                     (fn [handler-env _]
                       {:success? true
                        :result {:launch? (boolean (seq (:argv (vis/session-process-launch
                                                                 (:session-id handler-env)
                                                                 ["clojure" "-Sdescribe"]))))}})}])

         session-id
         (:session-id env)

         result
         (try (language-surface/repl-eval env "(+ 1 1)")
              (finally (process-jail/unregister-session-jail! session-id)))]

        (expect (true? (get-in result [:result :launch?])))))
  (it "refreshes the session jail before starting a REPL"
      (let
        [env
         (fake-env [{:language "clojure"
                     :start-repl-fn
                     (fn [handler-env _ _]
                       {:success? true
                        :result {:launch? (boolean (seq (:argv (vis/session-process-launch
                                                                 (:session-id handler-env)
                                                                 ["clojure" "-Sdescribe"]))))}})}])

         session-id
         (:session-id env)

         result
         (try (language-surface/start-repl env "start")
              (finally (process-jail/unregister-session-jail! session-id)))]

        (expect (true? (get-in result [:result :launch?]))))))

(defdescribe repl-connect-trust-boundary-test
             (it "advertises external ownership and detach-only lifecycle"
                 (let
                   [description
                    (:ext.symbol/description language-surface/connect-repl-symbol)

                    stop-description
                    (:ext.symbol/description language-surface/repl-stop-symbol)]

                   (expect (str/includes? description "external"))
                   (expect (str/includes? description "never owns or kills"))
                   (expect (str/includes? stop-description "detached")))))

(defdescribe language-surface-env-injection-test
             (it "uses declarative env injection rather than a before middleware shim"
                 (doseq [symbol language-surface/symbols]
                   (expect (true? (:ext.symbol/inject-env? symbol)))
                   (expect (nil? (:ext.symbol/before-fn symbol))))))
