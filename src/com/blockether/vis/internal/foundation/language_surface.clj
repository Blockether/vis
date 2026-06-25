(ns com.blockether.vis.internal.foundation.language-surface
  "Language-neutral FORMAT / TEST / REPL_EVAL / START_REPL dispatch.

  Language extensions register handlers under `:ext/language-tools`; this
  foundation surface exposes stable bare tool names and dispatches to the
  active handler for the requested/current language. REPL lifecycle is resource
  backed: `repl_start` creates a language-owned session resource, while
  `repl_status`/`repl_stop` inspect/stop those resources by id."
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

(defn format
  "Format source using a language extension. Prefer format(language, arg); one-arg form uses the active workspace language."
  [env & args]
  (dispatch! env :format-fn args))

(defn test
  "Run tests using a language extension. Prefer test(language, arg); one-arg form uses the active workspace language."
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
  (vis/symbol #'format
    {:symbol 'format :before-fn inject-env :tag :mutation}))

(def test-symbol
  (vis/symbol #'test
    {:symbol 'test :before-fn inject-env :tag :mutation}))

(def repl-eval-symbol
  (vis/symbol #'repl-eval
    {:symbol 'repl_eval :before-fn inject-env :tag :mutation}))

(def start-repl-symbol
  (vis/symbol #'start-repl
    {:symbol 'repl_start :before-fn inject-env :tag :mutation}))

(def repl-status-symbol
  (vis/symbol #'repl-status
    {:symbol 'repl_status :before-fn inject-env :tag :observation}))

(def repl-stop-symbol
  (vis/symbol #'repl-stop
    {:symbol 'repl_stop :before-fn inject-env :tag :mutation}))

(def symbols [format-symbol test-symbol repl-eval-symbol start-repl-symbol repl-status-symbol repl-stop-symbol])

(def prompt
  (str "Language facade tools (bare; prefer over language wrappers):
"
    "  format(language, arg) / format(arg) — format source; arg string or opts map.
"
    "  test(language, arg) / test(arg) — run tests; selectors are pack-defined.
"
    "  repl_eval(language, arg) / repl_eval(arg) — eval in REPL; opts may include id/repl_id.
"
    "  repl_start(language, opts?) — start managed REPL; opts may include id/dir/aliases.
"
    "  repl_start(language, id, op, opts?) — lifecycle op (:start/:stop/:restart/:status).
"
    "  repl_status(language_or_opts?) — list REPLs; repl_stop(id) stops one.
"
    "Use explicit `language` with several packs; else workspace language is inferred."))
