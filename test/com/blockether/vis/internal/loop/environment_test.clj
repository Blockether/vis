(ns com.blockether.vis.internal.loop.environment-test
  "Extension and tool selection for a new session: `--extensions` and `--tools` in the
   CLI and the TUI, and the `extensions` and `tools` fields of `POST /v1/sessions`."
  (:require [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private extension-selection (deref #'loop-env/extension-selection))

(def ^:private extensions-to-turn-off (deref #'loop-env/extensions-to-turn-off))

(def ^:private registered
  [{:ext/name "gh" :optional true} {:ext/name "spel" :optional true}
   {:ext/name "clj" :optional true} {:ext/name "foundation-harness"}])

(defn- turned-off
  [names]
  (with-redefs [python-extensions/ensure-python-extensions-loaded!
                (constantly nil)

                extension/registered-extensions
                (constantly registered)

                scoped/engine-setting!
                (fn [ext]
                  (when (:optional ext) {:id (str "extension_" (:ext/name ext))}))]

    (sort (map :ext/name (extensions-to-turn-off (extension-selection names))))))

(defn- error-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe
  extension-selection-test
  (describe "parsing"
            (it "keeps every extension without a selection"
                (expect (nil? (extension-selection nil))))
            (it "reads names as a list to keep and -names as a list to turn off"
                (expect (= {:keep #{"gh" "clj"}} (extension-selection ["gh" " clj"])))
                (expect (= {:drop #{"spel"}} (extension-selection ["-spel"])))
                (expect (= {:keep #{}} (extension-selection []))))
            (it "rejects a list that mixes both forms"
                (expect (= {:status 400 :type :extension/mixed-selection}
                           (error-data #(extension-selection ["gh" "-spel"]))))))
  (describe "optional extensions to turn off"
            (it "turns off every optional extension outside the keep list"
                (expect (= ["spel"] (turned-off ["gh" "clj"])))
                (expect (= ["clj" "gh" "spel"] (turned-off []))))
            (it "turns off only the -names" (expect (= ["spel"] (turned-off ["-spel"]))))
            (it "rejects an unknown name before the session starts"
                (let [data (error-data #(turned-off ["-spell"]))]
                  (expect (= :extension/unknown (:type data)))
                  (expect (= ["spell"] (:extensions data)))))
            (it "rejects an engine part that has no Auto/On/Off setting"
                (expect (=
                          {:status 400 :type :extension/required :extensions ["foundation-harness"]}
                          (error-data #(turned-off ["-foundation-harness"]))))))
  (describe
    "configuration tiers"
    (it "treats an extension from a tier that the session does not read as unknown"
        (with-redefs [python-extensions/ensure-python-extensions-loaded!
                      (constantly nil)

                      extension/registered-extensions
                      (constantly [{:ext/name "gh" :optional true :ext/source-scope "project"}
                                   {:ext/name "mcp" :optional true}])

                      scoped/engine-setting!
                      (fn [ext]
                        (when (:optional ext) {:id (str "extension_" (:ext/name ext))}))]

          (binding [workspace/*config-sources* #{}]
            (expect (= :extension/unknown
                       (:type (error-data #(doall (extensions-to-turn-off (extension-selection
                                                                            ["gh"])))))))
            (expect (= ["mcp"] (map :ext/name (extensions-to-turn-off (extension-selection []))))))
          (binding [workspace/*config-sources* #{"project"}]
            (expect (= ["mcp"]
                       (map :ext/name (extensions-to-turn-off (extension-selection ["gh"]))))))))))

(def ^:private py-tools ["py.lint_code" "py.repl_start" "py.repl_eval" "py.repl_stop"])

(defn- kept
  "The tools of `py-tools` that the tool list `names` keeps."
  [names]
  (let [selection (loop-env/tool-selection names)]
    (filterv #(loop-env/tool-on? selection py-tools %) py-tools)))

(defn- checked
  "Validate the tool list `names` against one optional `py` extension and one part of Vis."
  [names]
  (with-redefs [python-extensions/ensure-python-extensions-loaded!
                (constantly nil)

                extension/registered-extensions
                (constantly [{:ext/name "py" :optional true :tools (conj py-tools "py._private")}
                             {:ext/name "foundation-harness" :tools ["cat"]}])

                scoped/engine-setting!
                (fn [ext]
                  (when (:optional ext) (str "engines_" (:ext/name ext))))

                loop-env/ext-tool-names
                :tools]

    (loop-env/check-tool-selection! names)))

(defn- py-extension
  []
  {:ext/name "py"
   :ext/engine {:ext.engine/alias 'py
                :ext.engine/exact-symbol-names? true
                :ext.engine/symbols (mapv #(hash-map :ext.symbol/symbol (symbol %)) py-tools)}})

(defn- synced
  "Sync `py-extension` for a session with the tool list `names`. Return the bound and the
   removed names. `optional?` false makes the extension a part of Vis."
  [names optional?]
  (let [bound
        (atom #{})

        removed
        (atom #{})

        ext
        (py-extension)]

    (with-redefs [extension/wrap-extension
                  (fn [ext _]
                    (into {}
                          (map (fn [entry]
                                 [(:ext.symbol/symbol entry) identity]))
                          (extension/ext-symbols ext)))

                  scoped/engine-setting!
                  (constantly (when optional? "engines_py"))

                  env/set-python-bindings!
                  (fn [_ entries]
                    (swap! bound into (map (comp env/sym->py-name :sym)) entries))

                  env/remove-python-bindings!
                  (fn [_ targets]
                    (swap! removed into (map env/sym->py-name) targets))]

      (loop-env/sync-extension-symbols-into! ::context
                                             {:extensions (atom [ext])
                                              :config/tools (atom (loop-env/tool-selection names))}
                                             [ext]))
    {:bound @bound :removed @removed}))

(defn- temp-db-path
  "A database path in a new temporary directory."
  []
  (str (java.io.File. (str (java.nio.file.Files/createTempDirectory
                             "vis-tools"
                             (make-array java.nio.file.attribute.FileAttribute 0)))
                      "vis.mdb")))

(defdescribe
  tool-selection-test
  (describe "parsing"
            (it "keeps every tool without a list" (expect (nil? (loop-env/tool-selection nil))))
            (it "rejects an empty list and a list that mixes both forms"
                (expect (= {:status 400 :type :tool/empty-selection}
                           (error-data #(loop-env/tool-selection []))))
                (expect (= {:status 400 :type :tool/mixed-selection}
                           (error-data #(loop-env/tool-selection ["py.lint_code"
                                                                  "-py.repl_stop"]))))))
  (describe
    "tools that a list keeps"
    (it "turns off only the -names"
        (expect (= ["py.repl_start" "py.repl_eval" "py.repl_stop"] (kept ["-py.lint_code"])))
        (expect (= ["py.lint_code" "py.repl_start" "py.repl_eval"] (kept ["-py.repl_stop"]))))
    (it "keeps only the named tools of the extension that they name"
        (expect (= ["py.lint_code" "py.repl_eval"] (kept ["py.lint_code" " py.repl_eval"]))))
    (it "reads * as any characters and a dot as a dot"
        (expect (= ["py.lint_code"] (kept ["-py.repl_*"])))
        (expect (= ["py.repl_start" "py.repl_eval" "py.repl_stop"] (kept ["py.repl_*"])))
        (expect (= py-tools (kept ["-py-lint_code"]))))
    (it "leaves an extension alone when a keep list names none of its tools"
        (expect (= py-tools (kept ["gh.runs"])))))
  (describe "validation"
            (it "returns the trimmed list to save"
                (expect (= ["-py.repl_*"] (checked [" -py.repl_*"]))))
            (it "rejects a name that matches no tool and lists the tools of its extension"
                (let [e (try (checked ["py.lint"]) nil (catch clojure.lang.ExceptionInfo e e))]
                  (expect (= {:status 400 :type :tool/unknown :tools ["py.lint"]} (ex-data e)))
                  (expect (= (str "Unknown tool: py.lint. Tools of py: py.lint_code, py.repl_eval, "
                                  "py.repl_start, py.repl_stop.")
                             (ex-message e)))))
            (it "rejects a tool of a part of Vis"
                (expect (= ["cat"] (:tools (error-data #(checked ["-cat"])))))))
  (describe "sandbox bindings"
            (it "binds only the tools that the session's list keeps"
                (expect (= {:bound #{"py.lint_code" "py.repl_start" "py.repl_eval"}
                            :removed #{"py.repl_stop"}}
                           (synced ["-py.repl_stop"] true))))
            (it "never removes a tool of a part of Vis"
                (expect (= {:bound (set py-tools) :removed #{}} (synced ["-py.repl_stop"] false)))))
  (describe
    "a saved session"
    (it "keeps the tool list that it was created with"
        (let [path
              (temp-db-path)

              router
              {:providers [{:id :lmstudio}]}

              created
              (loop-env/create-environment router {:db path :tools ["-py.repl_stop"]})

              sid
              (:session-id created)

              _
              (loop-env/dispose-environment! created)

              resumed
              (loop-env/create-environment router {:db path :session sid})]

          (try (expect (= ["py.repl_stop"] (:names @(:config/tools resumed))))
               (finally (loop-env/dispose-environment! resumed)))))
    (it
      "replaces the tool list of a live session and keeps the new list for a resume"
      (let [path
            (temp-db-path)

            router
            {:providers [{:id :lmstudio}]}

            created
            (loop-env/create-environment router {:db path :tools ["-py.repl_stop"]})

            sid
            (:session-id created)

            k
            (loop-env/cache-key sid)

            cache
            @#'loop-env/cache]

        (swap! cache assoc k {:environment created})
        (try (with-redefs [config/resolve-db-spec
                           (constantly path)

                           python-extensions/prepare-project!
                           (constantly nil)

                           loop-env/check-tool-selection!
                           vec]

               (expect (= ["py.lint_code"] (loop-env/set-session-tools! sid ["py.lint_code"]))))
             (expect (= ["py.lint_code"] (:names @(:config/tools created))))
             (finally (swap! cache dissoc k) (loop-env/dispose-environment! created)))
        (let [resumed (loop-env/create-environment router {:db path :session sid})]
          (try (expect (= ["py.lint_code"] (:names @(:config/tools resumed))))
               (finally (loop-env/dispose-environment! resumed))))))))

(defdescribe
  reload-unbind-test
  ;; #415: a reload dropped an extension row or one of its symbols, and the session kept
  ;; the old tool bound. No row asked to remove the name.
  (it
    "removes each name that the last sync bound and the current rows no longer bind"
    (let [removed
          (atom nil)

          reserved
          (atom #{})

          ext
          (py-extension)

          environment
          {:extensions (atom [ext]) :bound-extension-symbols (atom #{})}

          sync!
          #(loop-env/sync-extension-symbols-into! ::context environment @(:extensions environment))

          names
          #(set (map env/sym->py-name %))]

      (with-redefs [extension/wrap-extension
                    (fn [ext _]
                      (into {}
                            (map (fn [entry]
                                   [(:ext.symbol/symbol entry) identity]))
                            (extension/ext-symbols ext)))

                    env/set-python-bindings!
                    (fn [_ _])

                    env/unowned-bindings
                    (fn [_ syms]
                      (remove #(@reserved (env/sym->py-name %)) syms))

                    env/remove-python-bindings!
                    (fn [_ targets]
                      (reset! removed (names targets)))]

        (sync!)
        (let [bound (names @(:bound-extension-symbols environment))]
          (expect (contains? bound "py.lint_code") (pr-str bound))
          (expect (empty? @removed))
          (swap! (:extensions environment)
            (fn [[row]]
              [(update-in row
                          [:ext/engine :ext.engine/symbols]
                          (partial filterv #(not= 'py.lint_code (:ext.symbol/symbol %))))]))
          (sync!)
          (expect (= #{"py.lint_code"} @removed))
          ;; The sandbox now reserves one name, so the sync keeps it and stops tracking it.
          (reset! reserved #{"py.repl_stop"})
          (reset! (:extensions environment) [])
          (sync!)
          (expect (= (disj bound "py.lint_code" "py.repl_stop") @removed))
          (expect (empty? @(:bound-extension-symbols environment))))))))
