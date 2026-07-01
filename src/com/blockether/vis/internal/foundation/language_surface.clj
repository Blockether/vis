(ns com.blockether.vis.internal.foundation.language-surface
  "Language-neutral FORMAT / TEST / REPL_EVAL / START_REPL dispatch.

  Language extensions register handlers under `:ext/language-tools`; this
  foundation surface exposes stable bare tool names and dispatches to the
  active handler for the requested/current language. REPL lifecycle is resource
  backed: `repl_start` creates a language-owned session resource and `repl_stop`
  stops one by id. Live REPLs also surface in the ctx `resources` block."
  (:refer-clojure :exclude [format test])
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]))

(defn- normalize-language [x]
  (when x
    (keyword (-> (str x)
                 (str/replace #"^:" "")
                 (str/lower-case)))))

(defn- env-language [env]
  (or (normalize-language (get-in env [:env/project :primary_language]))
      (normalize-language (get-in env [:project :primary_language]))
      (some->> (get-in env [:env/languages :languages])
               (map #(normalize-language (or (:language %) (:name %) %)))
               (remove nil?)
               first)))

(defn- active-extensions [env]
  (or (some-> env :active-extensions deref seq)
      (some-> env :extensions deref seq)
      (extension/registered-extensions)))

(defn- registered-handlers [env capability]
  (->> (active-extensions env)
       (mapcat :ext/language-tools)
       (keep (fn [entry]
               (let [language (normalize-language (:language entry))
                     f        (get entry capability)]
                 (when f
                   (assoc entry :language language :handler f)))))))

(def ^:private capability->tool
  "language-tool key -> the facade verb shown in the capability matrix."
  {:format-fn "format_code" :lint-fn "lint_code" :test-fn "run_tests" :repl-eval-fn "repl_eval" :start-repl-fn "repl_start"})

(def ^:private tool-order ["format_code" "lint_code" "run_tests" "repl_eval" "repl_start"])

(defn capability-data
  "STRUCTURED capability map for the ACTIVE language packs:
   `{\"clojure\" [\"format\" \"test\" \"repl_eval\" \"repl_start\"], \"python\" [...]}`
   — nil when none active. Recomputed every turn from active-extensions, so it
   GAINS a language the moment its pack activates (e.g. a .py file appears). Goes
   in ctx (`session[\"language_tools\"]`) so the model can read it programmatically
   AND it always reflects the current turn."
  [env]
  (let [by-lang (reduce (fn [m cap]
                          (reduce (fn [m h]
                                    (update m (:language h) (fnil conj #{}) (capability->tool cap)))
                                  m (registered-handlers env cap)))
                        {} (keys capability->tool))]
    (when (seq by-lang)
      (into (sorted-map)
            (for [[lang tools] by-lang]
              [(name lang) (vec (filter tools tool-order))])))))

(defn capability-matrix
  "AUTO capability matrix for the system prompt — the active packs' facade verbs
   + a CERTAIN statement of when each is the tool. nil when no pack is active.
     LANGUAGE TOOLS (active packs; call via the facade, language first):
       clojure : format_code · run_tests · repl_eval · repl_start
       python  : repl_eval · repl_start"
  [env]
  (when-let [data (capability-data env)]
    (str "LANGUAGE TOOLS (active packs; call via the facade, language first):\n"
         (str/join "\n"
                   (for [[lang tools] data]
                     (str "  " lang " : " (str/join " · " tools))))
      ;; CERTAIN: name when each verb IS the tool, so it's not ambiguous.
         "\n  → To RUN or VERIFY code in a listed language you MUST use repl_eval(language, code): it executes in the PROJECT interpreter (its modules + installed deps; globals persist across calls), which your own sandbox CANNOT import — do NOT importlib/open a project file. (Pure-stdlib scratch compute may run in your own sandbox.) Run the project's tests with run_tests(language); tidy hand-written source with format_code — it accepts either a raw code string (returns the formatted text) or a {\"path\": file} map (formats that file IN PLACE and returns a LEAN ack — which file + changed? — NOT the file's text, so don't print it back). The leading `language` arg is OPTIONAL — inferred from the workspace/path when omitted, so format_code({\"path\": file}) works; pass it first only to disambiguate when several packs match.")))

(defn- language-like? [x]
  (or (keyword? x)
      (and (string? x) (re-matches #"[A-Za-z][A-Za-z0-9_-]*" x))))

(defn- coerce-opts [arg]
  (cond
    (nil? arg) {}
    (map? arg) arg
    :else {:arg arg}))

(defn- opts-language [opts]
  (or (:language opts) (get opts "language")))

(defn- target-language [env opts]
  (or (normalize-language (opts-language opts))
      (env-language env)))

(defn- choose-handler [env capability opts]
  (let [handlers (vec (registered-handlers env capability))
        lang     (target-language env opts)
        matches  (if lang (filter #(= lang (:language %)) handlers) handlers)]
    (cond
      (= 1 (count matches)) (first matches)
      (empty? handlers) (throw (ex-info (str "No language extension registered for " (name capability))
                                        {:type :language-surface/no-handler
                                         :capability capability}))
      (empty? matches) (throw (ex-info (str "No " (name capability) " handler for language " lang)
                                       {:type :language-surface/no-language-handler
                                        :language lang
                                        :capability capability
                                        :available (vec (keep :language handlers))}))
      :else (throw (ex-info (str "Multiple language handlers match " (or lang "current workspace")
                                 "; pass the language as first arg, e.g. repl_eval with language first")
                            {:type :language-surface/ambiguous-language
                             :language lang
                             :capability capability
                             :available (vec (keep :language matches))})))))

(defn- parse-language-call [args]
  (case (count args)
    0 {:opts {} :payload {}}
    1 (let [arg (first args)]
        {:opts (coerce-opts arg)
         :payload arg})
    2 (let [[language payload] args]
        (if (language-like? language)
          {:opts (assoc (coerce-opts payload) :language language)
           :payload payload}
          (throw (ex-info "Expected language as first arg, e.g. repl_eval(language, ...)."
                          {:type :language-surface/bad-args
                           :got args}))))
    (throw (ex-info "Expected (arg) or (language, arg)."
                    {:type :language-surface/bad-args
                     :got args}))))

(defn- dispatch! [env capability args]
  (let [{:keys [opts payload]} (parse-language-call args)
        handler (choose-handler env capability opts)]
    ((:handler handler) env payload)))

(def ^:private repl-ops #{:status :start :stop :restart})

(defn- repl-op? [x]
  (contains? repl-ops (keyword x)))

(defn- start-repl-payload [args]
  (let [[language more] (if (and (seq args) (language-like? (first args)) (not (repl-op? (first args))))
                          [(first args) (next args)]
                          [nil args])]
    (case (count more)
      0 {:language language :id nil :op :start :opts {}}
      1 (let [arg (first more)]
          (cond
            (nil? arg) {:language language :id nil :op :start :opts {}}
            (map? arg) {:language (or language (opts-language arg)) :id (or (:id arg) (:repl_id arg) (get arg "id") (get arg "repl_id")) :op :start :opts arg}
            :else      {:language language :id nil :op arg :opts nil}))
      2 (let [[a b] more]
          (if (map? b)
            {:language (or language (opts-language b))
             :id (when-not (repl-op? a) a)
             :op (if (repl-op? a) a :start)
             :opts b}
            {:language language :id a :op b :opts nil}))
      3 (let [[id op opts] more]
          {:language (or language (opts-language opts)) :id id :op op :opts opts})
      (throw (ex-info "repl_start expects (language?), (language, opts), (language, op, opts), or (language, id, op, opts)."
                      {:type :language-surface/bad-args
                       :got args
                       :examples ["repl_start('clojure')"
                                  "repl_start('clojure', {'id': 'main', 'aliases': ['dev']})"
                                  "repl_start('clojure', 'status')"
                                  "repl_start('clojure', 'main', 'restart', {'dir': 'extensions/foo'})"]})))))

(defn- dispatch-start-repl! [env args]
  (let [{:keys [language id op opts]} (start-repl-payload args)
        dispatch-opts (cond-> (coerce-opts opts)
                        language (assoc :language language)
                        id       (assoc :id id))
        handler (choose-handler env :start-repl-fn dispatch-opts)
        opts    (cond-> (or opts {}) id (assoc :id id))]
    ((:handler handler) env op opts)))

(defn- repl-resources [env language]
  (let [lang (normalize-language language)]
    (->> (vis/list-resources (:session-id env))
         (filter #(or (= :repl (:kind %))
                      (= :nrepl (:kind %))
                      (str/ends-with? (str (:kind %)) "repl")))
         (filter #(or (nil? lang) (= lang (normalize-language (:language %)))))
         vec)))

(defn repl-status
  "List REPL resources, optionally filtered by language or id."
  ([env] (repl-status env nil))
  ([env arg]
   (let [opts (coerce-opts arg)
         lang (or (opts-language opts) (when (language-like? arg) arg))
         id   (or (:id opts) (:repl_id opts) (get opts "id") (get opts "repl_id"))]
     (extension/success
      {:result {:resources (cond->> (repl-resources env lang)
                             id (filter #(= (str id) (:id %)))
                             true vec)}}))))

(defn repl-stop
  "Stop a REPL by session resource id. This is the REPL-specific wrapper around resource_stop(id)."
  [env id]
  (extension/success {:result (vis/stop-resource! (:session-id env) id)}))

(defn- inject-env [env f args]
  {:env env :fn f :args (into [env] args)})

;; =============================================================================
;; Native op-card renderers — `:result` → `{:summary :body}`. Keys arrive
;; keywordized snake_case (trailing ?/! stripped), the injected env gone.
;; Defensive: language results vary per pack, so every access is nil-safe.
;; =============================================================================

(defn- fence [label s]
  (when (seq (str s)) (str (when label (str label ":\n")) "```\n" s "\n```")))

(defn- render-format-result
  "format_code → `` `path` (changed) `` when writing a file (the FORMAT_CODE
   badge already names the tool), else the formatted text as a code block."
  [r]
  (let [changed (:changed r)
        note    (if changed "(changed)" "(no change)")]
    (if-let [path (:path r)]
      {:summary (str "`" path "` " note)}
      {:summary note
       :body    (fence nil (:text r))})))

(defn- render-lint-result
  "lint_code → `N files — E errors, W warnings` headline (the LINT_CODE badge
   already names the tool); the findings (`file:row:col level message`) in the body."
  [r]
  (let [errors   (or (:error r) 0)
        warnings (or (:warning r) 0)
        infos    (or (:info r) 0)
        findings (:findings r)
        clean?   (and (zero? errors) (zero? warnings) (zero? infos))
        lines    (for [f findings]
                   (str (:file f) ":" (:row f) ":" (:col f)
                        " " (:level f) ": " (:message f)))]
    {:summary (not-empty
               (str (when-let [n (:files r)] (str n " file" (when (not= 1 n) "s")))
                    (if clean?
                      (when (:files r) " — clean")
                      (str " — " errors " error" (when (not= 1 errors) "s")
                           ", " warnings " warning" (when (not= 1 warnings) "s")
                           (when (pos? infos) (str ", " infos " info"))))))
     :body    (when (seq lines) (fence nil (str/join "\n" lines)))}))

(defn- render-test-result
  "run_tests → `✓/✗ <ns> — pass/total` headline (the RUN_TESTS badge already
   names the tool, so no redundant `tests` word); the run output on failure, or
   the error text when the run itself could not produce a result. A failing run
   NEVER renders blank — with neither output nor error we surface the raw result
   so the user always sees *something* went wrong, never an empty card."
  [r]
  (let [pass   (:pass r)
        fail   (:fail r)
        total  (:total r)
        error  (:error r)
        ok     (and (not error)
                    (if (number? fail) (zero? fail) (boolean (:pass r))))
        detail (or (not-empty (str (:output r)))
                   (not-empty (str error))
                   (when-not ok (str "no test result returned — " (pr-str r))))]
    {:summary (str (if ok "✓" "✗")
                   (when (seq (str (:ns r))) (str " " (:ns r)))
                   (when total (str " — " pass "/" total " passed"
                                    (when (and (number? fail) (pos? fail)) (str ", " fail " failed"))))
                   (when (and (not ok) (not total)) " — error")
                   (when (:note r) (str " (" (:note r) ")")))
     :body    (when-not ok (fence nil detail))}))

(defn- render-repl-eval-result
  "repl_eval → `(Nms)` headline (REPL_EVAL badge names the tool); value / out / err code blocks."
  [r]
  (let [err  (or (:err r) (:ex r) (:root_ex r))
        body (->> [(fence nil (:value r))
                   (fence "out" (:out r))
                   (fence "err" (when (seq (str err)) err))]
                  (remove nil?)
                  (str/join "\n\n"))]
    {:summary (not-empty
               (str (when (:ms r) (str "(" (:ms r) "ms)"))
                    (when (seq (str err)) (str (when (:ms r) " ") "— error"))))
     :body    (when (seq body) body)}))

(defn- render-repl-status-result
  "repl_status → `N REPLs: id (status), …`."
  [r]
  (let [res (:resources r)]
    {:summary (str (count res) " REPL" (when (not= 1 (count res)) "s")
                   (when (seq res)
                     (str ": " (str/join ", "
                                         (map #(str (:id %) " (" (:status %) ")") res)))))}))

(defn- render-repl-start-result
  "repl_start → a short lifecycle line (id + status / port when present)."
  [r]
  (if (contains? r :resources)
    (render-repl-status-result r)
    {:summary (str "REPL " (or (:id r) (:language r) "")
                   " " (or (:status r) "ready")
                   (when-let [p (:port r)] (str " :" p)))}))

(defn- render-repl-stop-result
  "repl_stop → `stopped REPL <id>`."
  [r]
  {:summary (str "stopped REPL" (when-let [id (:id r)] (str " " id)))})

(defn format-code
  "Format source using a language extension. `language` is OPTIONAL — when omitted it is inferred from the active workspace (so format_code({\"path\": file}) works); pass format_code(language, arg) only to disambiguate when several packs match.
   `arg` is either a raw code string / {\"code\": ...} (returns the formatted text) or a {\"path\": file} map (formats that file IN PLACE and returns a LEAN ack — which file + changed? — NOT the file's text, so don't print it back). The payload is passed through to the language handler verbatim."
  [env & args]
  (dispatch! env :format-fn args))

(defn lint-code
  "Lint source using a language extension. `language` is OPTIONAL — inferred from
   the active workspace when omitted; pass lint_code(language, arg) only to
   disambiguate when several packs match. `arg` is a raw code string / {\"code\": ...}
   (lints the snippet), a {\"path\": file} or {\"paths\": […]} map (lints those on
   disk), or nothing (lints the workspace's default source paths). Returns the
   linter's findings + severity counts."
  [env & args]
  (dispatch! env :lint-fn args))

(defn run-tests
  "Run tests using a language extension. Prefer run_tests(language, arg); the one-arg form uses the active workspace language. `arg` selects what to run: a namespace/module string (e.g. run_tests(\"clojure\", \"my.app.core-test\")), or a dict — {\"namespaces\": [\"a-test\" \"b-test\"]} (alias :ns) to run several, {\"paths\": [\"test\" ...]} to discover *_test namespaces under dirs/files, plus optional {\"only\": [...] :include/:exclude [tags]} selectors. Omit arg to run the whole suite."
  [env & args]
  (dispatch! env :test-fn args))

(defn repl-eval
  "Evaluate code in a language REPL. Prefer repl_eval(language, arg). `arg` may include `id`/`repl_id` to target a registered REPL resource."
  [env & args]
  (dispatch! env :repl-eval-fn args))

(defn start-repl
  "Start/manage a language REPL resource. Prefer repl_start(language, opts); opts may include `id` and language-specific options."
  [env & args]
  (dispatch-start-repl! env args))

(def format-symbol
  (vis/symbol #'format-code
              {:symbol 'format_code
               :native-tool? true
               ;; NAME(language, {payload}) — optional leading `language`, the rest a
               ;; pure options dict (always emitted so the payload stays a map).
               :call {:lead-opt "language" :rest :always}
               :render render-format-result
               :color-role :tool-color/edit
               :schema {:type "object"
                        :properties {"language" {:type "string" :description "Language pack (e.g. \"clojure\"); OMIT to infer from the workspace."}
                                     "code"     {:type "string" :description "Source to format (returns the formatted text)."}
                                     "path"     {:type "string" :description "Format this file IN PLACE (returns a lean ack, not the text). Mutually exclusive with code."}}
                        :required []}
               :before-fn inject-env
               :tag :mutation}))

(def lint-symbol
  (vis/symbol #'lint-code
              {:symbol 'lint_code
               :native-tool? true
               :render render-lint-result
               :color-role :tool-color/read
               :schema {:type "object"
                        :properties {"language" {:type "string" :description "Language pack (e.g. \"clojure\"); OMIT to infer from the workspace."}
                                     "code"     {:type "string" :description "Source to lint (returns findings). Mutually exclusive with path/paths."}
                                     "path"     {:type "string" :description "Lint this file on disk."}
                                     "paths"    {:type "array" :items {:type "string"} :description "Lint these files/dirs. OMIT all to lint the workspace's default source paths."}}
                        :required []}
               :before-fn inject-env
               :tag :observation}))

(def test-symbol
  (vis/symbol #'run-tests
              {:symbol 'run_tests
               :native-tool? true
               :call {:lead-opt "language" :rest :always}
               :render render-test-result
               :color-role :tool-color/test
               :schema {:type "object"
                        :properties {"language"   {:type "string" :description "Language pack; OMIT to infer from the workspace."}
                                     "namespaces" {:type "array" :items {:type "string"} :description "Test namespaces/modules to run (e.g. [\"my.app.core-test\"])."}
                                     "paths"      {:type "array" :items {:type "string"} :description "Dirs/files to discover *_test namespaces under."}
                                     "only"       {:type "array" :items {:type "string"} :description "Restrict to these fully-qualified test vars."}
                                     "include"    {:type "array" :items {:type "string"} :description "Only run tests carrying these tags."}
                                     "exclude"    {:type "array" :items {:type "string"} :description "Skip tests carrying these tags."}}
                        :required []}
               :before-fn inject-env
               :tag :mutation}))

(def repl-eval-symbol
  (vis/symbol #'repl-eval
              {:symbol 'repl_eval
               :native-tool? true
               :call {:lead-opt "language" :rest :always}
               :render render-repl-eval-result
               :color-role :tool-color/shell
               :schema {:type "object"
                        :properties {"language" {:type "string" :description "Language pack; OMIT to infer from the workspace."}
                                     "code"     {:type "string" :description "Source to evaluate in the language REPL."}
                                     "id"       {:type "string" :description "Target a specific registered REPL resource by id."}}
                        :required ["code"]}
               :before-fn inject-env
               :tag :mutation}))

(def start-repl-symbol
  (vis/symbol #'start-repl
              {:symbol 'repl_start
               :native-tool? true
               :call {:lead-opt "language" :rest :always}
               :render render-repl-start-result
               :color-role :tool-color/shell
               :schema {:type "object"
                        :properties {"language" {:type "string" :description "Language pack; OMIT to infer from the workspace."}
                                     "id"       {:type "string" :description "Resource id for the REPL (default per language)."}
                                     "dir"      {:type "string" :description "Directory to start the REPL in."}
                                     "aliases"  {:type "array" :items {:type "string"} :description "Build-tool aliases to activate (e.g. deps.edn :dev)."}}
                        :required []}
               :before-fn inject-env
               :tag :mutation}))

(def repl-stop-symbol
  (vis/symbol #'repl-stop
              {:symbol 'repl_stop
               :native-tool? true
               ;; repl_stop(id) — one positional id. (lint_code intentionally has NO
               ;; :call: its fn takes the whole input dict, so the generic form fits.)
               :call {:pos ["id"]}
               :render render-repl-stop-result
               :color-role :tool-color/delete
               :schema {:type "object"
                        :properties {"id" {:type "string" :description "Session resource id of the REPL to stop."}}
                        :required ["id"]}
               :before-fn inject-env
               :tag :mutation}))

(def symbols [format-symbol lint-symbol test-symbol repl-eval-symbol start-repl-symbol repl-stop-symbol])

(defn prompt
  "The language-facade reference: the AUTO capability matrix (active packs only)
   + the bare facade verbs. nil when no language pack is active, so a non-coding
   or single-language workspace carries nothing extra. Each verb's own docstring
   holds its args/return; `language` is explicit only when several packs match."
  [env]
  (when-let [matrix (capability-matrix env)]
    (str matrix "\n"
         "  facade (language-first, or inferred): format_code · lint_code · run_tests · repl_eval · repl_start · repl_stop")))
