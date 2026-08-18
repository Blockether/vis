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
  (it "dispatches each lifecycle VERB to the language handler with its own op"
      (let
        [env (fake-env [{:language "clojure"
                         :start-repl-fn (fn [_ op opts]
                                          {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "connect" :opts {"cwd" "ext" "aliases" ["dev"]}}
                   (:result
                     (language-surface/connect-repl env "clojure" {"cwd" "ext" "aliases" ["dev"]}))))
        (expect (= {:op "start" :opts {"aliases" ["dev"]}}
                   (:result (language-surface/repl-start env {"aliases" ["dev"]}))))
        (expect (= {:op "status" :opts {"cwd" "ext"}}
                   (:result (language-surface/repl-status env {"cwd" "ext"}))))
        (expect (= {:op "stop" :opts {"cwd" "ext"}}
                   (:result (language-surface/repl-stop env "clojure" {"cwd" "ext"}))))
        (expect (= {:op "start" :opts {}} (:result (language-surface/repl-start env))))))
  ;; Regression, issue #repl-ops: one `repl` verb carried an `op` STRING, so a
  ;; call read as a lifecycle step only after resolving its second argument —
  ;; and a stale `op` (`restart`) could be parsed as a REPL id and start one.
  (it "takes no `op` at all — an op string is just this verb's language or id"
      (let
        [env
         (fake-env [{:language "clojure"
                     :start-repl-fn (fn [_ op opts]
                                      {:success? true :result {:op op :opts opts}})}])]
        (expect (= "start" (:op (:result (language-surface/repl-start env {"op" "restart"})))))
        (expect (= "status" (:op (:result (language-surface/repl-status env)))))
        (expect (nil? (resolve 'com.blockether.vis.internal.foundation.language-surface/start-repl)))))
  (it "documents the explicit REPL lifecycle in its own doc text"
      (let
        [start
         (:ext.symbol/description language-surface/repl-start-symbol)

         result
         (:ext.symbol/result language-surface/repl-start-symbol)

         status
         (:ext.symbol/description language-surface/repl-status-symbol)

         stop
         (:ext.symbol/description language-surface/repl-stop-symbol)]

        ;; Regression, issue #ctx-resources: the doc used to send the model to
        ;; `session["resources"]["repls"][language][cwd]`, a ctx key that no longer exists.
        (expect (not (str/includes? start "session[\"resources\"]")))
        (expect (str/includes? start "`repl_status` is the only answer"))
        (expect (str/includes? start "absent/down/failed"))
        (expect (str/includes? start "`repl_stop` then `repl_start`"))
        (expect (str/includes? status "the only answer"))
        (expect (str/includes? result "stamped with `op`"))
        (expect (str/includes? (:ext.symbol/result language-surface/repl-eval-symbol)
                               "stamped with `op`"))
        (expect (str/includes? (:ext.symbol/result language-surface/test-symbol)
                               "absent fields mean not applicable"))
        (expect (not (str/includes? (:ext.symbol/result language-surface/test-symbol)
                                    "always present")))
        (expect (str/includes? stop "after verification"))
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
  (it "passes a language-first repl id and opts to language handlers"
      (let
        [env (fake-env [{:language "clojure"
                         :start-repl-fn (fn [_ op opts]
                                          {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "connect" :opts {"id" "main" "cwd" "ext"}}
                   (:result
                     (language-surface/connect-repl env "clojure" {"id" "main" "cwd" "ext"}))))
        (expect (= {:op "start" :opts {"id" "main" "aliases" ["dev"]}}
                   (:result (language-surface/repl-start env
                                                         "clojure"
                                                         {"id" "main" "aliases" ["dev"]}))))))
  (it "stops a repl resource BY ID through the resource model, with no pack at all"
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
             (expect (= "stopped"
                        (get-in (language-surface/repl-stop env "main-repl") [:result "result"])))
             (expect (true? @stopped?))
             (expect (empty? (resources/list-resources sid)))
             (finally (resources/stop-all! sid)))))
  ;; Regression, issue #repl-ops: `repl_stop(id="…")` folds to (id, {}) at the
  ;; Python boundary, and a bare leading string was then read as a LANGUAGE — the
  ;; by-id stop went looking for a pack named after the REPL's own id.
  (it "reads a leading string as the REPL id even when an empty opts map trails it"
      (let
        [stopped?
         (atom false)

         env
         (fake-env [{:language "clojure"
                     :start-repl-fn (fn [_ op opts]
                                      {:success? true :result {:op op :opts opts}})}])

         sid
         (:session-id env)]

        (try (resources/register! sid
                                  {:id "main-repl" :kind :nrepl :language "clojure" :label "main"}
                                  {:stop-fn (fn []
                                              (reset! stopped? true))})
             (expect (= "stopped"
                        (get-in (language-surface/repl-stop env "main-repl" {}) [:result "result"])))
             (expect (true? @stopped?))
             ;; ...while a LANGUAGE still reaches the pack's own stop handler.
             (expect (= {:op "stop" :opts {"cwd" "ext"}}
                        (:result (language-surface/repl-stop env "clojure" {"cwd" "ext"}))))
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
        ;; Two facts about run_tests a session cannot infer from a result. FIRST: it
        ;; starts NOTHING -- with no REPL up the suite runs in a clean JVM, so a run
        ;; never spawns a server the caller then has to reason about. SECOND: on the
        ;; REUSE path the runner `(require … :reload)`s every namespace it RUNS, so
        ;; the stale-Var trap is never the test namespace -- it is the PRODUCTION
        ;; namespace that test depends on, whose Vars the reused REPL keeps.
        (expect (str/includes? m "run_tests NEVER starts a REPL"))
        (expect (str/includes? m "CLEAN JVM"))
        (expect (str/includes? m "reloads the namespaces it RUNS but NEVER their dependencies"))
        (expect (str/includes? m "changed PRODUCTION ns still serves the Vars"))
        (expect (str/includes? m "(require 'my.prod.ns :reload)"))
        (expect (not (str/includes? m "REUSES this session's managed REPL")))
        (expect (not (str/includes? m "do NOT reload namespaces automatically")))
        (expect (not (str/includes? m "session[\"resources\"]")))
        (expect (not (str/includes? m "Keep managed REPLs alive")))))
  (it "is nil when no language pack is active (nothing dead in the prompt)"
      (expect (nil? (language-surface/capability-matrix {:active-extensions (atom [{}])})))))

;; Regression, issue #133: the finished headline had no notion of what the CALL
;; asked for, so it could not tell two runs apart.
(defdescribe test-target-test
             (let [target #'language-surface/test-target]
               (it "reports what the call selected - its paths, node ids and all"
                   (expect (= "test/foo" (target {"paths" ["test/foo"]})))
                   (expect (= "test/a_test.clj::adds-test, ::subs-test"
                              (target {"paths" ["test/a_test.clj::adds-test" "::subs-test"]}))))
               ;; A namespace / var selector narrows the run exactly as a path
               ;; does, so the headline must NAME it - "full suite" for a
               ;; one-namespace run would read like the whole workspace ran.
               (it "reads the namespace and var selectors beside paths"
                   (expect (= "foo-test" (target {"namespaces" ["foo-test"]})))
                   ;; a bare string is ONE entry, never a sequence of characters
                   (expect (= "a.core-test" (target {"ns" "a.core-test"})))
                   (expect (= "test/a_test.clj, foo-test/a"
                              (target {"paths" ["test/a_test.clj"] "only" ["foo-test/a"]}))))
               (it "echoes nothing for a key no pack selects by"
                   (expect (= "full suite" (target {"filter" "slow"}))))
               (it "falls back to the whole suite when nothing narrows the run"
                   (expect (= "full suite" (target {})))
                   (expect (= "full suite" (target {"paths" [nil "  "]}))))))

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
         (try (language-surface/repl-start env)
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
