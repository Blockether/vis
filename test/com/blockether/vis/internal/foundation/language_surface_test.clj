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
      (let [env
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
      (let [env
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
      (let [env (fake-env [{:language "clojure"
                            :start-repl-fn (fn [_ op opts]
                                             {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "connect" :opts {"cwd" "ext" "aliases" ["dev"]}}
                   (:result (language-surface/connect-repl env
                                                           "clojure"
                                                           {"cwd" "ext" "aliases" ["dev"]}))))
        (expect (= {:op "start" :opts {"aliases" ["dev"]}}
                   (:result (language-surface/repl-start env {"aliases" ["dev"]}))))
        (expect (= {:op "status" :opts {"cwd" "ext"} "resources" []}
                   (:result (language-surface/repl-status env {"cwd" "ext"}))))
        (expect (= {:op "stop" :opts {"cwd" "ext"}}
                   (:result (language-surface/repl-stop env "clojure" {"cwd" "ext"}))))
        (expect (= {:op "start" :opts {}} (:result (language-surface/repl-start env))))))
  ;; Regression, issue #repl-ops: one `repl` verb carried an `op` STRING, so a
  ;; call read as a lifecycle step only after resolving its second argument —
  ;; and a stale `op` (`restart`) could be parsed as a REPL id and start one.
  (it "takes no `op` at all — an op string is just this verb's language or id"
      (let [env (fake-env [{:language "clojure"
                            :start-repl-fn (fn [_ op opts]
                                             {:success? true :result {:op op :opts opts}})}])]
        (expect (= "start" (:op (:result (language-surface/repl-start env {"op" "restart"})))))
        (expect (= "status" (:op (:result (language-surface/repl-status env)))))
        (expect (nil? (resolve
                        'com.blockether.vis.internal.foundation.language-surface/start-repl)))))
  (it "documents the explicit REPL lifecycle in its own doc text"
      (let [start
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
      (let [seen
            (atom nil)

            env
            (fake-env [{:language "clojure"
                        :repl-eval-fn (fn [_ arg]
                                        (reset! seen arg)
                                        {:success? true :result {:value "3"}})}])]

        (expect (= {:value "3"} (:result (language-surface/repl-eval env "clojure" "(+ 1 2)"))))
        (expect (= "(+ 1 2)" @seen))))
  (it
    "passes a language-first repl id and opts to language handlers"
    (let [env (fake-env [{:language "clojure"
                          :start-repl-fn (fn [_ op opts]
                                           {:success? true :result {:op op :opts opts}})}])]
      (expect (= {:op "connect" :opts {"id" "main" "cwd" "ext"}}
                 (:result (language-surface/connect-repl env "clojure" {"id" "main" "cwd" "ext"}))))
      (expect (= {:op "start" :opts {"id" "main" "aliases" ["dev"]}}
                 (:result
                   (language-surface/repl-start env "clojure" {"id" "main" "aliases" ["dev"]}))))))
  (it "stops a repl resource BY ID through the resource model, with no pack at all"
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
             (expect (= "stopped"
                        (get-in (language-surface/repl-stop env "main-repl") [:result "result"])))
             (expect (true? @stopped?))
             (expect (empty? (resources/list-resources sid)))
             (finally (resources/stop-all! sid)))))
  ;; Regression, issue #repl-ops: `repl_stop(id="…")` folds to (id, {}) at the
  ;; Python boundary, and a bare leading string was then read as a LANGUAGE — the
  ;; by-id stop went looking for a pack named after the REPL's own id.
  (it "reads a leading string as the REPL id even when an empty opts map trails it"
      (let [stopped?
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
                        (get-in (language-surface/repl-stop env "main-repl" {})
                                [:result "result"])))
             (expect (true? @stopped?))
             ;; ...while a LANGUAGE still reaches the pack's own stop handler.
             (expect (= {:op "stop" :opts {"cwd" "ext"}}
                        (:result (language-surface/repl-stop env "clojure" {"cwd" "ext"}))))
             (finally (resources/stop-all! sid)))))
  ;; Regression, issue #repl-args: a bare string where the options map belongs was
  ;; swallowed into `{:arg "..."}` — `repl_start("clojure", "extensions/foo")` reported
  ;; success while starting the REPL at the workspace ROOT.
  (it
    "REFUSES a bare string where the options map belongs, on every lifecycle verb"
    (let [env
          (fake-env [{:language "clojure"
                      :start-repl-fn (fn [_ op opts]
                                       {:success? true :result {:op op :opts opts}})}])

          refuses
          (fn [f & args]
            (try (apply f env args) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

      (expect (= :language-surface/bad-args
                 (refuses language-surface/repl-start "clojure" "extensions/foo")))
      (expect (= :language-surface/bad-args
                 (refuses language-surface/repl-status "clojure" "extensions/foo")))
      (expect (= :language-surface/bad-args
                 (refuses language-surface/repl-stop "clojure" "extensions/foo")))
      (expect (= :language-surface/bad-args
                 (refuses language-surface/connect-repl "clojure" "extensions/foo")))))
  ;; Regression, session report a114a0ab-8083-45dc-924f-0e005b20f965: a call that
  ;; spelled its directory `root` was ignored, so tools ran at the workspace root.
  (it
    "reads `root` and `project` as `cwd` on every verb, and refuses disagreements"
    (let [seen
          (atom nil)

          seen-root
          (atom nil)

          env
          (fake-env [{:language "clojure"
                      :start-repl-fn (fn [_ op opts]
                                       {:success? true :result {:op op :opts opts}})
                      :format-fn (fn [call-env arg]
                                   (reset! seen [arg (:workspace/root call-env)])
                                   {:success? true :result {"changed" false}})
                      :test-fn (fn [_ arg]
                                 (reset! seen arg)
                                 {:success? true :result {"pass" 0}})
                      :repl-eval-fn (fn [_ arg]
                                      (reset! seen arg)
                                      {:success? true :result {:value "3"}})}])]

      (language-surface/format-code env {"root" "repositories/plc3"})
      (reset! seen-root (second @seen))
      (expect (str/ends-with? @seen-root "repositories/plc3"))
      (expect (= {"cwd" "repositories/plc3"} (first @seen)))
      (expect (= {:op "start" :opts {"language" "clojure" "cwd" "repositories/plc3"}}
                 (:result (language-surface/repl-start env
                                                       {"language" "clojure"
                                                        "root" "repositories/plc3"}))))
      (expect (= {:op "status" :opts {"cwd" "ext"} "resources" []}
                 (:result (language-surface/repl-status env "clojure" {"project" "ext"}))))
      (language-surface/repl-eval env "clojure" {"code" "(+ 1 2)" "root" "ext"})
      (expect (= {"code" "(+ 1 2)" "cwd" "ext"} @seen))
      (language-surface/run-tests env {"root" "ext"})
      (expect (= {"cwd" "ext"} @seen))
      ;; Repeating one directory under aliases is valid; differing ones are ambiguous.
      (expect (= {:op "start" :opts {"cwd" "ext"}}
                 (:result
                   (language-surface/repl-start env {"root" "ext" "cwd" "ext" "project" "ext"}))))
      (expect (= :language-surface/bad-args
                 (try (language-surface/repl-start env {"root" "ext" "project" "other"})
                      nil
                      (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
  (it
    "states each verb's real requiredness: only repl_eval's `code`, never `language`"
    (let [keys-of
          (fn [sym]
            (into {} (map (juxt :name identity)) (:ext.symbol/params sym)))

          start
          (keys-of language-surface/repl-start-symbol)

          evaluate
          (keys-of language-surface/repl-eval-symbol)

          tests
          (keys-of language-surface/test-symbol)

          stop
          (keys-of language-surface/repl-stop-symbol)

          connect
          (keys-of language-surface/connect-repl-symbol)]

      (expect (true? (:required? (get evaluate "code"))))
      (expect (str/includes? (:note (get start "cwd")) "project"))
      (expect (str/includes? (:note (get evaluate "cwd")) "project"))
      (expect (str/includes? (:note (get (keys-of language-surface/repl-status-symbol) "cwd"))
                             "project"))
      ;; `language` is INFERRED on every verb (choose-handler falls back to the
      ;; workspace's candidate languages and to a single active pack), and
      ;; repl_stop needs no `id` when the pack's REPL under `cwd` is the target:
      ;; nothing on this surface is required except repl_eval's `code`.
      (expect (nil? (some :required? (vals stop))))
      (expect (str/includes? (:note (get stop "language")) "inferred"))
      (expect (str/includes? (:note (get stop "cwd")) "project"))
      (expect (str/includes? (:ext.symbol/description language-surface/repl-stop-symbol)
                             "NOTHING is required"))
      (expect (str/includes? (:note (get start "language")) "inferred"))
      (expect (str/includes? (:note (get tests "language")) "inferred"))
      ;; `port` is the ONE key a pack refuses without (clojure repl_connect, and
      ;; only clojure attaches at all), so it stays marked.
      (expect (true? (:required? (get connect "port"))))
      (expect (str/includes? (:note (get connect "port")) "build"))
      (expect (str/includes? (:ext.symbol/description language-surface/connect-repl-symbol)
                             "CLOJURE only"))
      ;; `build` selects a shadow-cljs build to ATTACH to or to run cljs tests
      ;; in — it never starts one, so it is a repl_connect/run_tests key.
      (expect (contains? connect "build"))
      (expect (contains? tests "build"))
      (expect (not (contains? start "build")))
      ;; `aliases` is the deps.edn escape hatch on BOTH verbs that boot a JVM:
      ;; repl_start's REPL and run_tests' clean-JVM `clojure -M:test`. It ADDS —
      ;; a note reading "default" invites a caller to believe naming one REPLACES
      ;; :dev/:test and drops the test alias off its own classpath.
      (expect (contains? start "aliases"))
      (expect (contains? tests "aliases"))
      (expect (str/includes? (:note (get start "aliases")) "EXTRA"))
      (expect (str/includes? (:ext.symbol/description language-surface/repl-start-symbol) "ADDS"))
      (expect (str/includes? (:note (get tests "aliases")) "EXTRA"))
      (expect (str/includes? (:ext.symbol/description language-surface/test-symbol) "-M:test"))
      (expect (nil? (:required? (get tests "aliases"))))
      (expect (str/includes? (:ext.symbol/description language-surface/repl-start-symbol)
                             "`repl_connect`"))
      ;; run_tests and repl_eval are the two verbs a session calls without reading
      ;; the page first: what they REQUIRE, and where they run when `cwd` is
      ;; omitted, has to be on their own params.
      (expect (str/includes? (:note (get tests "cwd")) "project"))
      (expect (str/includes? (:note (get tests "cwd")) "workspace ROOT"))
      (expect (str/includes? (:note (get tests "paths")) "omit"))
      (expect (nil? (some :required? (vals tests))))
      (expect (str/includes? (:ext.symbol/description language-surface/test-symbol)
                             "NOTHING is required"))
      (expect (str/includes? (:ext.symbol/description language-surface/repl-eval-symbol)
                             "WORKSPACE ROOT"))
      (expect (str/includes? (:ext.symbol/description language-surface/repl-start-symbol)
                             "WORKSPACE ROOT"))))
  ;; Cross-validated against the packs, not from memory: a key a handler READS is a
  ;; key the page declares. These were read and undeclared — ruff's own knobs
  ;; (language_python/ruff.clj `call-opts`) and clojure's direct-dial eval keys
  ;; (language_clojure/core.clj `clj-eval-fn`) — so a caller could not reach them.
  (it
    "declares the pack-only keys the handlers actually read, each naming its pack"
    (let [keys-of
          (fn [sym]
            (into {} (map (juxt :name identity)) (:ext.symbol/params sym)))

          fmt
          (keys-of language-surface/format-symbol)

          lint
          (keys-of language-surface/lint-symbol)

          evaluate
          (keys-of language-surface/repl-eval-symbol)

          tests
          (keys-of language-surface/test-symbol)

          scoped?
          (fn [params k pack]
            (str/starts-with? (str (:note (get params k))) (str pack " — ")))]

      (doseq [k ["line_length" "config"]]
        (expect (contains? fmt k) (str "format_code hides " k))
        (expect (scoped? fmt k "python") (str "format_code " k)))
      (doseq [k ["select" "ignore" "line_length" "config"]]
        (expect (contains? lint k) (str "lint_code hides " k))
        (expect (scoped? lint k "python") (str "lint_code " k)))
      (doseq [k ["ns" "port" "host"]]
        (expect (contains? evaluate k) (str "repl_eval hides " k))
        (expect (scoped? evaluate k "clojure") (str "repl_eval " k)))
      ;; Only the clojure runner reads metadata tags; python's pytest never sees them.
      (doseq [k ["include" "exclude" "ns" "build" "aliases"]]
        (expect (scoped? tests k "clojure") (str "run_tests " k)))
      (expect (scoped? tests "runner" "python"))))
  ;; Regression: run_tests' description carried the same clause twice — two `str`
  ;; lines of a multi-line description are trivially duplicated, and the page read as
  ;; a stutter to every caller.
  (it "never repeats a phrase inside one description"
      (doseq [sym language-surface/symbols]
        (let [words (str/split (str (:ext.symbol/description sym)) #"\s+")
              windows (map #(str/join " " %) (partition 6 1 words))]

          (expect (= (count windows) (count (distinct windows)))
                  (str (:ext.symbol/symbol sym)
                       " repeats: "
                       (first (for [[w n] (frequencies windows)
                                    :when (> n 1)]

                                w)))))))
  ;; Regression, issue #repl-enumerate: repl_status answered for the pack's own
  ;; directory alone, so a REPL under another cwd — or a shadow-cljs attachment beside
  ;; the JVM one — was invisible, while the verb's doc promised it was the only way to
  ;; see live REPLs.
  (it "lists EVERY live REPL of the session beside the pack's per-directory status"
      (let [env
            (fake-env [{:language "clojure"
                        :start-repl-fn (fn [_ _ _]
                                         {:success? true :result {"status" "down"}})}])

            sid
            (:session-id env)]

        (try (resources/register!
               sid
               {:id "nrepl:~/proj#ext" :kind :nrepl :language "clojure" :label "ext"}
               {})
             (let [result (:result (language-surface/repl-status env "clojure"))]
               (expect (= "down" (get result "status")))
               (expect (= ["nrepl:~/proj#ext"] (mapv #(get % "id") (get result "resources")))))
             ;; ...and asking BY ID answers for that REPL alone, pack or no pack.
             (let [one (:result (language-surface/repl-status env "nrepl:~/proj#ext"))]
               (expect (= "nrepl:~/proj#ext" (get one "id")))
               (expect (= "up" (get one "status"))))
             (expect (= "unknown"
                        (get (:result (language-surface/repl-status env "nrepl:~/gone")) "status")))
             (finally (resources/stop-all! sid)))))
  (it "keeps a pack's own REPL LABEL with the pack, and a session id with the resource model"
      (let [env (fake-env [{:language "clojure"
                            :start-repl-fn (fn [_ op opts]
                                             {:success? true :result {:op op :opts opts}})}])]
        (expect (= {:op "stop" :opts {"id" "worker" "cwd" "ext"}}
                   (:result
                     (language-surface/repl-stop env "clojure" {"id" "worker" "cwd" "ext"}))))
        (expect (= "unknown"
                   (get-in (language-surface/repl-stop env "clojure" {"id" "nrepl:~/gone"})
                           [:result "result"])))))
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
      (let [env (scan-env "json"
                          ["json" "typescript" "clojure"]
                          [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= "typescript" (resolved-language env)))))
  (it "prefers the workspace primary over other scanned languages"
      (let [env (scan-env "clojure"
                          ["clojure" "typescript"]
                          [(echo-lang-handler "typescript") (echo-lang-handler "clojure")])]
        (expect (= "clojure" (resolved-language env)))))
  (it "uses the file-count primary from the workspace snapshot when tool env metadata is absent"
      (let [env
            (dissoc (scan-env nil [] [(echo-lang-handler "python") (echo-lang-handler "clojure")])
              :env/project
              :env/languages)]
        (with-redefs [environment/snapshot
                      (constantly {:languages {:primary "clojure"
                                               :languages [{:language "clojure" :files 260}
                                                           {:language "python" :files 47}]}})]
          (expect (= "clojure" (resolved-language env))))))
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
      (let [env
            (fake-env [{:language "clojure"
                        :test-fn (fn [handler-env _]
                                   {:success? true
                                    :result {:launch? (boolean (seq (:argv
                                                                      (vis/session-process-launch
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
      (let [env
            (fake-env [{:language "clojure"
                        :repl-eval-fn (fn [handler-env _]
                                        {:success? true
                                         :result {:launch? (boolean
                                                             (seq (:argv (vis/session-process-launch
                                                                           (:session-id handler-env)
                                                                           ["clojure"
                                                                            "-Sdescribe"]))))}})}])

            session-id
            (:session-id env)

            result
            (try (language-surface/repl-eval env "(+ 1 1)")
                 (finally (process-jail/unregister-session-jail! session-id)))]

        (expect (true? (get-in result [:result :launch?])))))
  (it "refreshes the session jail before starting a REPL"
      (let [env
            (fake-env [{:language "clojure"
                        :start-repl-fn (fn [handler-env _ _]
                                         {:success? true
                                          :result {:launch?
                                                   (boolean (seq (:argv (vis/session-process-launch
                                                                          (:session-id handler-env)
                                                                          ["clojure"
                                                                           "-Sdescribe"]))))}})}])

            session-id
            (:session-id env)

            result
            (try (language-surface/repl-start env)
                 (finally (process-jail/unregister-session-jail! session-id)))]

        (expect (true? (get-in result [:result :launch?]))))))

(defdescribe repl-connect-trust-boundary-test
             (it "advertises external ownership and detach-only lifecycle"
                 (let [description
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
