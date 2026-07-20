(ns com.blockether.vis.internal.foundation.language-surface-test
  (:require [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.language-surface :as language-surface]
            [com.blockether.vis.internal.resources :as resources]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- fake-env
  [handlers]
  {:session-id (str "ls-test-" (random-uuid))
   :env/project {:primary_language "clojure"}
   :extensions (atom [{:ext/name "fake-clj" :ext/language-tools handlers}])})

(defdescribe
  language-surface-dispatch-test
  (it "dispatches format to the active language handler"
      (let [seen
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
      (let [env (fake-env [{:language "clojure"
                            :test-fn (fn [_ arg]
                                       {:success? true :result {:language "clojure" :arg arg}})}
                           {:language "python"
                            :test-fn (fn [_ arg]
                                       {:success? true :result {:language "python" :arg arg}})}])]
        (expect (= {:language "python" :arg {"language" "python" "ns" "x"}}
                   (:result (language-surface/run-tests env {"language" "python" "ns" "x"}))))))
  (it "parks the test run OUTSIDE the native tool wall"
      (let [parked
            (atom 0)

            env
            (assoc (fake-env [{:language "clojure"
                               :test-fn (fn [_ arg]
                                          {:success? true :result {:arg arg}})}])
              :vis/outside-tool-wall (fn [thunk]
                                       (swap! parked inc)
                                       (thunk)))

            r
            (language-surface/run-tests env {"ns" "x"})]

        (expect (= 1 @parked))
        (expect (= {"ns" "x"} (get-in r [:result :arg])))))
  (it "passes clj_repl-shaped repl_start op and opts to language handlers"
      (let [env (fake-env [{:language "clojure"
                            :start-repl-fn (fn [_ op opts]
                                             {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "restart" :opts {"dir" "ext" "aliases" ["dev"]}}
                   (:result
                     (language-surface/start-repl env "restart" {"dir" "ext" "aliases" ["dev"]}))))
        (expect (= {:op "start" :opts {"aliases" ["dev"]}}
                   (:result (language-surface/start-repl env {"aliases" ["dev"]}))))
        (expect (= {:op "start" :opts {}} (:result (language-surface/start-repl env))))))
  (it "accepts language-first calls for repl eval"
      (let [seen
            (atom nil)

            env
            (fake-env [{:language "clojure"
                        :repl-eval-fn (fn [_ arg]
                                        (reset! seen arg)
                                        {:success? true :result {:value "3"}})}])]

        (expect (= {:value "3"} (:result (language-surface/repl-eval env "clojure" "(+ 1 2)"))))
        (expect (= "(+ 1 2)" @seen))))
  (it "passes language-first repl_start id and opts to language handlers"
      (let [env (fake-env [{:language "clojure"
                            :start-repl-fn (fn [_ op opts]
                                             {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "restart" :opts {"id" "main" "dir" "ext"}}
                   (:result
                     (language-surface/start-repl env "clojure" "main" "restart" {"dir" "ext"}))))
        (expect (= {:op "start" :opts {"id" "main" "aliases" ["dev"]}}
                   (:result (language-surface/start-repl env
                                                         "clojure"
                                                         {"id" "main" "aliases" ["dev"]}))))))
  (it "reports and stops repl resources through the resource model"
      (let [stopped?
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
      (let [env (fake-env [{:language "clojure"
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
      (let [env (scan-env "json"
                          ["json" "typescript" "clojure"]
                          [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= "typescript" (resolved-language env)))))
  (it "prefers the workspace primary over other scanned languages"
      (let [env (scan-env "clojure"
                          ["clojure" "typescript"]
                          [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= "clojure" (resolved-language env)))))
  (it "resolves a grammar variant to its base family handler via the alias map"
      ;; a pack registering only 'typescript'/'javascript' still serves tsx/jsx.
      (let [env (scan-env "json"
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
      (let [env (scan-env "json"
                          ["json"]
                          [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= :language-surface/ambiguous-language
                   (error-type #(language-surface/repl-eval env {"code" "1"})))))))

(defdescribe
  capability-matrix-test
  (it "renders the facade verbs per ACTIVE language pack"
      (let [env
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

        (expect (str/includes? m "clojure : format_code · run_tests · repl_eval · repl_start"))
        (expect (str/includes? m "python : repl_eval · repl_start"))
        (expect (str/includes? m "session[\"resources\"][\"repls\"][language][dir]"))
        (expect (not (str/includes? m "session[\"env\"][\"languages\"]")))))
  (it "is nil when no language pack is active (nothing dead in the prompt)"
      (expect (nil? (language-surface/capability-matrix {:active-extensions (atom [{}])})))))

(defdescribe
  render-test-result-test
  (let [render #'language-surface/render-test-result]
    (it "passes render has NO success glyph, a pass/total headline with the run time, and no body"
        (let [{:keys [summary body]} (render
                                       {"ns" "foo-test" "pass" 69 "total" 69 "fail" 0 "ms" 123})]
          (expect (= "foo-test — 69/69 passed (123ms)" summary))
          (expect (nil? body))))
    (it "marks a failing run ✗ and surfaces the output"
        (let [{:keys [summary body]}
              (render {"ns" "foo-test" "pass" 60 "total" 69 "fail" 9 "output" "9 failures"})]
          (expect (str/starts-with? summary "✗"))
          (expect (str/includes? summary "9 failed"))
          (expect (str/includes? body "9 failures"))))
    (it "surfaces the error text when the run could not produce a result"
        (let [{:keys [summary body]} (render {"error" "could not parse test result"})]
          (expect (str/starts-with? summary "✗"))
          (expect (str/includes? summary "error"))
          (expect (str/includes? body "could not parse test result"))))
    (it "NEVER renders a blank card — even a degenerate empty result surfaces something"
        (doseq [r [{} {"ns" nil "output" ""} {"mode" "repl" "port" 7888}]]
          (let [{:keys [summary body]} (render r)]
            (expect (str/starts-with? summary "✗"))
            (expect (seq body)))))))

(defdescribe render-repl-start-result-test
             (let [render #'language-surface/render-repl-start-result]
               (it "surfaces failed startup details instead of a bare starting line"
                   (let [{:keys [summary body]} (render {"result" "failed"
                                                         "status" "failed"
                                                         "id" "nrepl:/repo"
                                                         "port" 5555
                                                         "exit" 42
                                                         "message" "launcher died"
                                                         "log" "/tmp/vis-nrepl.log"
                                                         "cmd" ["clojure" "-M:vis/nrepl-launch"]
                                                         "log_tail" ["boom" "stack"]})]
                     (expect (str/starts-with? summary "✗ REPL nrepl:/repo failed :5555"))
                     (expect (str/includes? body "MESSAGE\nlauncher died"))
                     (expect (str/includes? body "EXIT\n42"))
                     (expect (str/includes? body "LOG\n/tmp/vis-nrepl.log"))
                     (expect (str/includes? body "CMD\nclojure -M:vis/nrepl-launch"))
                     (expect (str/includes? body "LOG TAIL\nboom\nstack"))))))

(defdescribe
  render-repl-eval-result-test
  (let [render #'language-surface/render-repl-eval-result]
    (it "collapses a short form inline with a value preview, no FORM section"
        (let [{:keys [summary body]} (render {"code" "(+ 1 1)" "value" "2"})]
          (expect (= "(+ 1 1)  ⇒ 2" summary))
          (expect (str/includes? body "**RESULT**"))
          (expect (str/includes? body "2"))
          (expect (not (str/includes? body "**FORM**")))))
    (it "omits RESULT when the value is nil, but still shows STDOUT"
        (let [{:keys [summary body]}
              (render {"code" "(dotimes [i 2] (println i))" "value" "nil" "out" "0\n1\n"})]
          (expect (str/includes? summary "⇒ nil"))
          (expect (not (str/includes? body "**RESULT**")))
          (expect (not (str/includes? body "```clojure\nnil\n```")))
          (expect (str/includes? body "**STDOUT**"))))
    (it "promotes a long / multi-line form to its own FORM section, clipped on the chip"
        (let [code "(->> (range 1000000)\n     (filter even?)\n     (map inc)\n     (reduce +))"
              {:keys [summary body]} (render {"code" code "value" "250000500000"})]

          (expect (str/ends-with? summary "⇒ 250000500000"))
          (expect (str/includes? summary "…"))
          (expect (str/includes? body "**FORM**"))
          (expect (str/includes? body "(reduce +))"))))
    (it "renders an eval error as ✗ headline + ERROR section, replacing RESULT"
        (let [{:keys [summary body]} (render {"code" "(/ 1 0)"
                                              "error_message" "ArithmeticException: Divide by zero"
                                              "trace"
                                              ["clojure.lang.Numbers.divide (Numbers.java:190)"]
                                              "status" #{"eval-error" "done"}})]
          (expect (str/includes? summary "✗ ArithmeticException"))
          (expect (str/includes? body "**ERROR**"))
          (expect (str/includes? body "Divide by zero"))
          (expect (not (str/includes? body "**RESULT**")))))
    (it "treats stderr on a successful nil eval as STDERR, not an error/result"
        (let [{:keys [summary body]}
              (render {"code" "(warn!)" "value" "nil" "err" "warn\n" "status" ["done"]})]
          (expect (str/includes? summary "⇒ nil"))
          (expect (not (str/includes? body "**RESULT**")))
          (expect (str/includes? body "**STDERR**"))
          (expect (not (str/includes? body "**ERROR**")))))
    (it "separates sections by exactly one blank line"
        (let [{:keys [body]} (render {"code" "x" "value" "1" "out" "hi"})]
          (expect (str/includes? body "```\n\n**STDOUT**"))))
    (it "renders a timeout as ⧖ headline + always-shown FORM + TIMEOUT note"
        (let [{:keys [summary body]} (render {"code" "(Thread/sleep 999999)"
                                              "timed_out" true
                                              "ms" 30000
                                              "out" "partial output"
                                              "status" ["timeout"]})]
          (expect (= "(Thread/sleep 999999)  ⧖ timed out after 30000ms" summary))
          (expect (str/includes? body "**FORM**"))
          (expect (str/includes? body "(Thread/sleep 999999)"))
          (expect (str/includes? body "**STDOUT**"))
          (expect (str/includes? body "partial output"))
          (expect (str/includes? body "**TIMEOUT**"))
          (expect (str/includes? body "timed out after 30000ms"))
          (expect (not (str/includes? body "**RESULT**")))
          (expect (not (str/includes? body "**ERROR**")))))
    (it "detects a timeout from a timeout status even without a timed_out flag, always showing FORM"
        (let [{:keys [summary body]} (render {"code" "(loop [] (recur))" "status" ["timeout"]})]
          (expect (str/includes? summary "⧖ timed out"))
          (expect (str/includes? body "**FORM**"))
          (expect (str/includes? body "(loop [] (recur))"))))))

(defdescribe format-schema-advertises-recursion-test
             (it "format_code schema + doc advertise directory recursion and the omit-all default"
                 (let [paths-desc
                       (get-in language-surface/format-symbol
                               [:ext.symbol/schema :properties "paths" :description])

                       path-desc
                       (get-in language-surface/format-symbol
                               [:ext.symbol/schema :properties "path" :description])

                       doc
                       (:ext.symbol/doc language-surface/format-symbol)]

                   ;; a directory in :paths is walked recursively for source files
                   (expect (re-find #"(?i)recursiv" paths-desc))
                   (expect (re-find #"(?i)director" paths-desc))
                   ;; omitting everything formats the workspace default source paths
                   (expect (re-find #"(?i)OMIT all" paths-desc))
                   ;; a bare :path pointing at a directory also recurses
                   (expect (re-find #"(?i)recursiv" path-desc))
                   ;; the facade docstring documents both behaviours too
                   (expect (re-find #"(?i)recursiv" doc))
                   (expect (re-find #"(?i)default source paths" doc))))
             (it "lint_code + run_tests schemas already advertise dirs/files"
                 (let [lint-paths
                       (get-in language-surface/lint-symbol
                               [:ext.symbol/schema :properties "paths" :description])

                       test-paths
                       (get-in language-surface/test-symbol
                               [:ext.symbol/schema :properties "paths" :description])]

                   (expect (str/includes? lint-paths "files/dirs"))
                   (expect (re-find #"(?i)dirs/files" test-paths))))
             (it "run_tests is a direct native handler, not Python-watchdog-bound"
                 (let [handlers (extension/native-tool-handlers [{:ext/engine
                                                                  {:ext.engine/symbols
                                                                   [language-surface/test-symbol]}}]
                                                                (fake-env []))]
                   (expect (contains? handlers "run_tests")))))

(defdescribe
  render-lint-result-names-target-test
  (let [render #'language-surface/render-lint-result]
    (it "a single-file lint headlines the FILE PATH, never a bare `1 file`"
        (let [{:keys [summary]}
              (@render
               {"error" 0 "warning" 0 "info" 0 "files" 1 "findings" [] "targets" ["src/foo.clj"]})]
          (expect (= "`src/foo.clj` — clean" summary))
          (expect (str/includes? summary "foo.clj"))
          (expect (not (str/includes? summary "1 file")))))
    (it "several path targets collapse to `N targets`"
        (expect (= "2 targets — clean"
                   (:summary (@render
                              {"error" 0
                               "warning" 0
                               "info" 0
                               "files" 2
                               "findings" []
                               "targets" ["src/a.clj" "src/b.clj"]})))))
    (it "a stdin snippet lint (no targets) reads `snippet`, not the misleading `1 file`"
        (let [{:keys [summary]} (@render {"error" 0 "warning" 0 "info" 0 "files" 1 "findings" []})]
          (expect (= "snippet — clean" summary))
          (expect (not (str/includes? summary "1 file")))))
    (it "a bare workspace lint (no targets, many files) keeps `N files`"
        (expect (= "7 files — clean"
                   (:summary (@render {"error" 0 "warning" 0 "info" 0 "files" 7 "findings" []})))))
    (it "findings still render counts in the headline and lines in the body"
        (let [{:keys [summary body]}
              (@render
               {"error" 1
                "warning" 0
                "info" 0
                "files" 1
                "targets" ["src/foo.clj"]
                "findings"
                [{"file" "src/foo.clj" "row" 3 "col" 5 "level" "error" "message" "boom"}]})]
          (expect (str/includes? summary "`src/foo.clj`"))
          (expect (str/includes? summary "1 error"))
          (expect (str/includes? body "src/foo.clj"))
          (expect (str/includes? body "  3:5 error: boom"))))
    (it "findings across many files GROUP under one path header each, path never repeated"
        (let [{:keys [body]}
              (@render
               {"error" 3
                "warning" 0
                "info" 0
                "files" 2
                "targets" ["src/a.clj" "src/b.clj"]
                "findings"
                [{"file" "src/a.clj" "row" 1 "col" 1 "level" "error" "message" "one"}
                 {"file" "src/a.clj" "row" 9 "col" 2 "level" "error" "message" "two"}
                 {"file" "src/b.clj" "row" 4 "col" 3 "level" "error" "message" "three"}]})]
          ;; each path header appears EXACTLY once, findings indented beneath it
          (expect (= 1 (count (re-seq #"(?m)^src/a\.clj$" body))))
          (expect (= 1 (count (re-seq #"(?m)^src/b\.clj$" body))))
          (expect (str/includes? body "  1:1 error: one"))
          (expect (str/includes? body "  9:2 error: two"))
          (expect (str/includes? body "  4:3 error: three"))))))
