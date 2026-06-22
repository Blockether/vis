(ns com.blockether.vis.internal.foundation.language-surface
  "Language-neutral FORMAT / TEST / REPL_EVAL / START_REPL dispatch.

  Language extensions register handlers under `:ext/language-tools`; this
  foundation surface exposes the stable bare tool names and dispatches to the
  active handler for the requested/current language."
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

(defn- coerce-opts [arg]
  (cond
    (nil? arg) {}
    (map? arg) arg
    :else {:arg arg}))

(defn- target-language [env opts]
  (or (normalize-language (or (:language opts) (get opts "language")))
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
                              "; pass {\"language\": ...}")
                     {:type :language-surface/ambiguous-language
                      :language lang
                      :capability capability
                      :available (vec (keep :language matches))})))))

(defn- dispatch! [env capability arg]
  (let [opts    (coerce-opts arg)
        handler (choose-handler env capability opts)
        payload (if (contains? opts :arg) (:arg opts) opts)]
    ((:handler handler) env payload)))

(defn- start-repl-payload [args]
  (case (count args)
    0 {:op :start :opts {}}
    1 (let [arg (first args)]
        (cond
          (nil? arg)    {:op :start :opts {}}
          (map? arg)    {:op :start :opts arg}
          :else         {:op arg :opts nil}))
    2 {:op (first args) :opts (second args)}
    (throw (ex-info "start_repl expects (), (opts), (op), or (op, opts)"
             {:type :language-surface/bad-args
              :got args
              :examples ["start_repl()"
                         "start_repl({\"aliases\": [\"dev\"]})"
                         "start_repl(\"status\")"
                         "start_repl(\"restart\", {\"dir\": \"extensions/foo\", \"aliases\": [\"dev\"]})"]}))))

(defn- dispatch-start-repl! [env args]
  (let [{:keys [op opts]} (start-repl-payload args)
        dispatch-opts (cond-> (coerce-opts opts)
                        (and (map? opts) (contains? opts :language)) (assoc :language (:language opts))
                        (and (map? opts) (contains? opts "language")) (assoc "language" (get opts "language")))
        handler (choose-handler env :start-repl-fn dispatch-opts)]
    ((:handler handler) env op opts)))

(defn- inject-env [env f args]
  {:env env :fn f :args (into [env] args)})

(defn format
  "Format source using the active language extension. Accepts the language pack's normal format arg, or a dict with `language` to disambiguate. For Clojure this delegates to clj_format."
  [env arg]
  (dispatch! env :format-fn arg))

(defn test
  "Run tests using the active language extension. Accepts the language pack's normal test arg, or a dict with `language` to disambiguate. For Clojure this delegates to clj_test."
  [env arg]
  (dispatch! env :test-fn arg))

(defn repl-eval
  "Evaluate code in the active language REPL. Accepts the language pack's normal eval arg, or a dict with `language` to disambiguate. For Clojure this delegates to clj_eval."
  [env arg]
  (dispatch! env :repl-eval-fn arg))

(defn start-repl
  "Start/manage the active language REPL with clj_repl-shaped args. Defaults to start for the generic START_REPL tool, accepts an op (status/start/stop/restart) plus optional opts. For Clojure opts are {dir, aliases}."
  [env & args]
  (dispatch-start-repl! env args))

(defn- render-simple [label result]
  {:summary {:left (vis/ir-strong label)}
   :display (vis/ir-root (vis/ir-code-block "text" (pr-str result) {:wrap? true}))})

(def format-symbol
  (vis/symbol #'format
    {:symbol 'format :before-fn inject-env :tag :mutation
     :render-fn (partial render-simple "FORMAT")}))

(def test-symbol
  (vis/symbol #'test
    {:symbol 'test :before-fn inject-env :tag :mutation
     :render-fn (partial render-simple "TEST")}))

(def repl-eval-symbol
  (vis/symbol #'repl-eval
    {:symbol 'repl_eval :before-fn inject-env :tag :mutation
     :render-fn (partial render-simple "REPL_EVAL")}))

(def start-repl-symbol
  (vis/symbol #'start-repl
    {:symbol 'start_repl :before-fn inject-env :tag :mutation
     :render-fn (partial render-simple "START_REPL")}))

(def symbols [format-symbol test-symbol repl-eval-symbol start-repl-symbol])

(def prompt
  (str "Language-neutral tools (bare):
"
    "  format(arg) — dispatches to the active language formatter (Clojure → clj_format). Pass {\"language\": \"clojure\", ...} to disambiguate.
"
    "  test(arg) — dispatches to the active language test runner (Clojure → clj_test).
"
    "  repl_eval(arg) — dispatches to the active language REPL evaluator (Clojure → clj_eval).
"
    "  start_repl() | start_repl(\"status\"|\"start\"|\"stop\"|\"restart\", opts?) — clj_repl-shaped lifecycle; opts include {\"dir\": ..., \"aliases\": [...]}. Generic no-arg start_repl() starts the active REPL.
"
    "Language-specific tools remain available as escape hatches, but prefer these generic names when the workspace language is clear."))
