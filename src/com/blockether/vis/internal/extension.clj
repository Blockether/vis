(ns com.blockether.vis.internal.extension
  "Extension subsystem: spec, builders, hook execution, the global
   registry, parse-error rescue, and the inline-docs catalog.

   An extension is the SINGLE entry point for everything a third-party
   bundle contributes to vis. Whatever surfaces it populates — SCI
   sandbox symbols, CLI commands, channels, providers, persistence
   backends — it does so by listing them in the matching `:ext/<surface>`
   slot, and `register-extension!` dispatches each slot to its concrete
   sub-registry. The same data feeds:

     - the active-extensions list every iteration consults
     - the system-prompt block rendered from `:ext/symbols`
     - the per-iteration nudge composers
     - the parse-error rescue chain
     - the `(meta/extensions)` / `(meta/extension-doc id name)`
       catalog the agent reads to discover its own surface

   Channel and provider registries live in `internal.registry` (the
   sub-registry that lights up when this module dispatches their
   contributions). Backend dispatch lives in `internal.persistance`.
   Classpath manifest scanning lives in `internal.manifest`. This
   module is the only consumer of all four."
  (:refer-clojure :exclude [symbol])
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.vis.internal.manifest :as manifest]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.registry :as registry]
   [taoensso.telemere :as tel]))

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

;; =============================================================================
;; Symbol entry spec
;; =============================================================================

;; Symbol name bound in the SCI sandbox.
(s/def :ext.symbol/sym symbol?)

;; Implementation function the LLM calls from :code blocks.
(s/def :ext.symbol/fn fn?)

;; One-liner description shown in the sandbox var's docstring.
(s/def :ext.symbol/doc non-blank-string?)

;; Argument signatures, e.g. '([query] [query opts]).
;; Shown in var meta :arglists and used to derive :examples when missing.
(s/def :ext.symbol/arglists (s/and vector? seq))

;; Usage examples injected into the system prompt so the LLM sees
;; concrete call patterns, e.g. ["(search-documents \"neural\")"]
(s/def :ext.symbol/examples (s/and vector? seq #(every? non-blank-string? %)))

;; Entry decorator: (fn [env f args] → map). Wraps :fn on the way in.
(s/def :ext.symbol/before-fn fn?)

;; Exit decorator: (fn [env f args result] → map). Wraps :fn on the way out.
(s/def :ext.symbol/after-fn fn?)

;; Error decorator: (fn [err env f args] → map). Called when :fn throws.
(s/def :ext.symbol/on-error-fn fn?)

;; Plain value bound in the sandbox (constant, data, config).
;; Mutually exclusive with :ext.symbol/fn.
(s/def :ext.symbol/val some?)

(s/def ::fn-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/fn :ext.symbol/doc
                :ext.symbol/arglists :ext.symbol/examples]
    :opt [:ext.symbol/before-fn :ext.symbol/after-fn
          :ext.symbol/on-error-fn]))

(s/def ::val-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/val :ext.symbol/doc]))

(s/def ::symbol-entry
  (s/or :fn  ::fn-symbol-entry
    :val ::val-symbol-entry))

;; =============================================================================
;; Extension spec
;; =============================================================================

;; Fully qualified extension name, e.g. 'com.blockether.vis.ext.common.
(s/def :ext/namespace symbol?)

;; Extension-level documentation - describes what this bundle provides.
(s/def :ext/doc non-blank-string?)

;; Top-level group for prompt rendering, e.g. "knowledge", "conversation".
(s/def :ext/group non-blank-string?)

;; Subgroup within the group, e.g. "documents" under "knowledge".
(s/def :ext/subgroup non-blank-string?)

;; Guard evaluated at each query boundary. (fn [env] -> bool).
;; Default: (constantly true).
(s/def :ext/activation-fn fn?)

;; Optional extra LLM-facing documentation appended AFTER the canonical
;; symbol-derived prompt block when the extension is active.
(s/def :ext/prompt fn?)

;; Optional per-iteration nudge composer.
(s/def :ext/nudge-fn fn?)

;; Optional source-code rewriter for SCI/edamame parse errors.
(s/def :ext/on-parse-error-fn fn?)

;; Optional dependency declaration. Vector of extension namespace symbols.
(s/def :ext/requires (s/coll-of symbol? :kind vector?))

;; Semver version string, e.g. "1.0.0", "0.3.1-SNAPSHOT".
(s/def :ext/version non-blank-string?)

;; Author name or org.
(s/def :ext/author non-blank-string?)

;; SPDX license identifier.
(s/def :ext/license non-blank-string?)

;; ============================================================================
;; Surface slots
;; ============================================================================

;; CLI commands exported by this extension.
(s/def :ext/cli
  (s/coll-of :com.blockether.vis.internal.registry/command :kind vector?))

;; Channels exported by this extension.
(s/def :ext/channels
  (s/coll-of :com.blockether.vis.internal.registry/channel :kind vector?))

;; LLM providers exported by this extension. Each entry mirrors the
;; canonical provider shape; we accept any IFn (or absence) for the
;; optional runtime fns so a minimal provider doesn't ship no-op stubs.
(let [or-nil-or-fn (fn [k] #(let [v (get % k ::absent)] (or (= v ::absent) (ifn? v))))]
  (s/def ::provider-entry
    (s/and map?
      #(keyword? (:provider/id %))
      #(non-blank-string? (:provider/label %))
      (or-nil-or-fn :provider/status-fn)
      (or-nil-or-fn :provider/logout-fn)
      (or-nil-or-fn :provider/detect-fn)
      (or-nil-or-fn :provider/auth-fn)
      (or-nil-or-fn :provider/get-token-fn))))
(s/def :ext/providers (s/coll-of ::provider-entry :kind vector?))

;; Persistence backends exported by this extension.
(s/def :persistance/id keyword?)
(s/def :persistance/ns
  (s/and symbol?
    #(nil? (namespace %))
    #(re-find #"\." (name %))))
(s/def :ext/persistance-entry (s/keys :req [:persistance/id :persistance/ns]))
(s/def :ext/persistance (s/coll-of :ext/persistance-entry :kind vector?))

;; Vector of symbol entries this extension binds into the sandbox.
(s/def :ext/symbols (s/coll-of ::symbol-entry :kind vector?))

;; Map of fully-qualified Java classes to expose in the sandbox.
(s/def :ext/classes
  (s/and map?
    #(every? symbol? (keys %))
    #(every? class? (vals %))))

;; Map of short-name imports for Java classes.
(s/def :ext/imports
  (s/and map?
    #(every? symbol? (keys %))
    #(every? symbol? (vals %))))

;; Optional SCI namespace alias for this extension's symbols.
(s/def :ext.ns-alias/ns    (s/and symbol? #(nil? (namespace %))))
(s/def :ext.ns-alias/alias (s/and symbol? #(nil? (namespace %))))
(s/def :ext/ns-alias
  (s/and map?
    #(s/valid? :ext.ns-alias/ns    (:ns %))
    #(s/valid? :ext.ns-alias/alias (:alias %))))

(defn- ns-alias-required-when-symbols?
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/ns-alias ext))))

(defn- group-required-when-symbols?
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/group ext))))

(s/def ::extension
  (s/and
    (s/keys :req [:ext/namespace :ext/doc]
      :opt [:ext/group :ext/subgroup :ext/activation-fn
            :ext/symbols :ext/classes :ext/imports
            :ext/ns-alias :ext/prompt :ext/nudge-fn
            :ext/on-parse-error-fn :ext/requires
            :ext/version :ext/author :ext/license
            :ext/cli :ext/channels :ext/providers :ext/persistance])
    ns-alias-required-when-symbols?
    group-required-when-symbols?))

;; =============================================================================
;; Symbol helpers (builder fns)
;; =============================================================================

(defn- validate-symbol-entry!
  "Assert a symbol entry conforms to ::symbol-entry. Throws on violation."
  [entry]
  (when-not (s/valid? ::symbol-entry entry)
    (throw (ex-info (str "Invalid symbol '" (:ext.symbol/sym entry) "':\n"
                      (with-out-str (s/explain ::symbol-entry entry)))
             {:type   :extension/invalid-symbol
              :sym    (:ext.symbol/sym entry)
              :explain (s/explain-data ::symbol-entry entry)})))
  entry)

(defn- derive-examples [sym arglists]
  (if-let [al (first (seq arglists))]
    (let [args (->> al (remove #{'&}) (map str) (str/join " "))]
      [(str "(" sym (when (seq args) (str " " args)) ")")])
    [(str "(" sym ")")]))

(defn symbol
  "Build a function symbol entry.

   Required: :doc, :arglists
   Optional: :examples, :before-fn, :after-fn, :on-error-fn,
             :on-parse-error-fn

   Defaults:
     :examples — derived from :arglists when not provided

   See `docs/src/extensions/hooks.md` for hook semantics."
  [sym-name f opts]
  (let [arglists (:arglists opts)
        arglists (when arglists (if (seq? arglists) (vec arglists) arglists))
        examples (or (:examples opts)
                   (when arglists (derive-examples sym-name arglists)))]
    (validate-symbol-entry!
      (cond-> #:ext.symbol{:sym sym-name
                           :fn  f}
        (:doc opts)               (assoc :ext.symbol/doc (:doc opts))
        arglists                  (assoc :ext.symbol/arglists arglists)
        examples                  (assoc :ext.symbol/examples (vec examples))
        (:before-fn opts)         (assoc :ext.symbol/before-fn (:before-fn opts))
        (:after-fn opts)          (assoc :ext.symbol/after-fn (:after-fn opts))
        (:on-error-fn opts)       (assoc :ext.symbol/on-error-fn (:on-error-fn opts))))))

(defn value
  "Build a value symbol entry - a plain constant/data binding.

   (value 'max-retries 3 {:doc \"Maximum retry attempts.\"})

   All three args required. :doc in opts is required."
  [sym-name val opts]
  (let [entry #:ext.symbol{:sym sym-name
                           :val val
                           :doc (:doc opts)}]
    (validate-symbol-entry! entry)))

(defn- arglist->call-form
  [alias-sym sym-name arglist]
  (let [args   (->> arglist (remove #{'&}) (map str) (str/join " "))
        target (if alias-sym
                 (str alias-sym "/" sym-name)
                 (str sym-name))]
    (str "(" target (when (seq args) (str " " args)) ")")))

(defn- render-symbol-line
  [alias-sym entry]
  (let [{sym-name :ext.symbol/sym
         doc      :ext.symbol/doc
         arglists :ext.symbol/arglists} entry]
    (if (:ext.symbol/fn entry)
      (str "- "
        (str/join " or " (map #(arglist->call-form alias-sym sym-name %) arglists))
        " — " doc)
      (str "- "
        (if alias-sym
          (str alias-sym "/" sym-name)
          (str sym-name))
        " — " doc))))

(defn render-prompt
  "Render canonical :ext/prompt text from symbol docstrings + arglists.

   Accepts an extension map or any map with:
   - :ext/doc      or :heading
   - :ext/ns-alias optional {:alias 'vis}
   - :ext/symbols  vector of symbol + value entries
   - :usage-note   optional extra note added to the heading
   - :notes        optional string or seq of extra lines appended verbatim

   Returns a prompt string suitable for :ext/prompt."
  [{:keys [heading usage-note notes] :as opts}]
  (let [alias-sym    (get-in opts [:ext/ns-alias :alias])
        symbols      (or (:symbols opts) (:ext/symbols opts))
        heading      (or heading (:ext/doc opts) "Extension tools")
        header-notes (vec (remove nil?
                            [(when alias-sym (str "use " alias-sym "/ prefix"))
                             (when (non-blank-string? usage-note) usage-note)]))
        extra-lines  (cond
                       (nil? notes)        []
                       (string? notes)     [notes]
                       (sequential? notes) (vec notes)
                       :else               [(str notes)])
        body-lines   (mapv #(render-symbol-line alias-sym %) symbols)]
    (str/join "\n"
      (concat [(str heading
                 (when (seq header-notes)
                   (str " (" (str/join "; " header-notes) ")")))]
        body-lines
        extra-lines))))

;; =============================================================================
;; Normalization + validation
;; =============================================================================

(defn- normalize-prompt [prompt]
  (cond
    (nil? prompt)    nil
    (fn? prompt)     prompt
    (string? prompt) (constantly prompt)
    :else (throw (ex-info ":ext/prompt must be a string or (fn [env] string)"
                   {:got (type prompt)}))))

(defn validate!
  "Normalize and assert that an extension map conforms to ::extension.
   Normalizes `:ext/prompt` (string → fn) before checking the spec
   when the key is present. Throws with spec explain-data on violation."
  [ext]
  (let [ext (cond-> ext
              (contains? ext :ext/prompt) (update :ext/prompt normalize-prompt))]
    (when-not (s/valid? ::extension ext)
      (throw (ex-info (str "Invalid extension '" (:ext/namespace ext) "':\n"
                        (with-out-str (s/explain ::extension ext)))
               {:type      :extension/invalid-spec
                :namespace (:ext/namespace ext)
                :explain   (s/explain-data ::extension ext)})))
    ext))

;; =============================================================================
;; Hook execution - runtime wrappers with output validation + logging
;; =============================================================================

(defn- validate-hook-return!
  [hook-name sym returned]
  (when-not (map? returned)
    (throw (ex-info (str hook-name " for '" sym "' must return a map, got: " (type returned))
             {:type (keyword "extension" (str hook-name "-error")) :sym sym :returned returned}))))

(defn- call-hook
  [hook-name sym hook-fn hook-args]
  (try
    (apply hook-fn hook-args)
    (catch clojure.lang.ArityException e
      (throw (ex-info (str hook-name " for '" sym "' has wrong arity: " (ex-message e))
               {:type (keyword "extension" (str hook-name "-error")) :sym sym} e)))
    (catch Throwable e
      (throw (ex-info (str hook-name " for '" sym "' threw: " (ex-message e))
               {:type (keyword "extension" (str hook-name "-error")) :sym sym} e)))))

(defn- elapsed-ms [t0] (/ (- (System/nanoTime) t0) 1e6))

(defn- log-hook! [level id ext-ns sym phase ms extra-msg]
  (tel/log! {:level level :id id
             :data {:ext ext-ns :sym sym :phase phase :ms ms}
             :msg (str ext-ns "/" sym " :invoke"
                    (when phase (str " " phase))
                    (when ms (str " " (format "%.1fms" (double ms))))
                    (when extra-msg (str " " extra-msg)))}))

(defn- run-before [ext-ns sym-entry env f args]
  (if-let [before (:ext.symbol/before-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log-hook! :debug ::before-fn ext-ns sym :before-fn nil nil)
          ret (call-hook ":before-fn" sym before [env f args])
          _   (validate-hook-return! ":before-fn" sym ret)
          ms  (elapsed-ms t0)]
      (if (contains? ret :result)
        (do (log-hook! :debug ::before-fn-done ext-ns sym :before-fn ms "short-circuited")
          {:result (:result ret)})
        (do (log-hook! :debug ::before-fn-done ext-ns sym :before-fn ms nil)
          {:env  (get ret :env env)
           :fn   (get ret :fn f)
           :args (vec (get ret :args args))})))
    {:env env :fn f :args args}))

(defn- run-after [ext-ns sym-entry env f args result]
  (if-let [after (:ext.symbol/after-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log-hook! :debug ::after-fn ext-ns sym :after-fn nil nil)
          ret (call-hook ":after-fn" sym after [env f args result])
          _   (validate-hook-return! ":after-fn" sym ret)
          ms  (elapsed-ms t0)]
      (log-hook! :debug ::after-fn-done ext-ns sym :after-fn ms nil)
      {:env    (get ret :env env)
       :fn     (get ret :fn f)
       :args   (vec (get ret :args args))
       :result (get ret :result result)})
    {:env env :fn f :args args :result result}))

(defn- run-on-error [ext-ns sym-entry err env f args]
  (if-let [on-error (:ext.symbol/on-error-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log-hook! :warn ::on-error-fn ext-ns sym :on-error-fn nil (str "handling: " (ex-message err)))
          ret (try
                (call-hook ":on-error-fn" sym on-error [err env f args])
                (catch Throwable e
                  (if (identical? e err)
                    (throw e)
                    (throw (ex-info (str ":on-error-fn for '" sym "' threw: " (ex-message e))
                             {:type :extension/on-error-fn-error :sym sym} e)))))
          _   (validate-hook-return! ":on-error-fn" sym ret)
          ms  (elapsed-ms t0)]
      (cond
        (contains? ret :result)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "fallback result") ret)
        (contains? ret :error)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "surfacing error") ret)
        :else
        (do (log-hook! :info ::on-error-fn-done ext-ns sym :on-error-fn ms "retrying") ret)))
    (throw err)))

(defn invoke-symbol-wrapper
  "Full invocation pipeline for a function symbol entry:
   before-fn → fn → after-fn, with on-error-fn catching :fn errors.

   Every hook can override :fn, :args, :env via its return map.
   :before-fn can return {:result val} to short-circuit.
   :on-error-fn can return {:result val}, {:error err}, or {:fn :args :env} to retry.

   Returns the final result. Throws on any unrecoverable error."
  [ext sym-entry args env]
  (let [sym    (:ext.symbol/sym sym-entry)
        ext-ns (:ext/namespace ext)
        t0     (System/nanoTime)
        _      (log-hook! :info ::invoke ext-ns sym nil nil nil)
        before-out (run-before ext-ns sym-entry env (:ext.symbol/fn sym-entry) args)]
    (if (contains? before-out :result)
      (let [ms (elapsed-ms t0)]
        (log-hook! :info ::invoke-done ext-ns sym nil ms "short-circuited")
        (:result before-out))
      (let [{env  :env
             f    :fn
             args :args} before-out

            call-result
            (let [ct0 (System/nanoTime)]
              (try
                (let [r  (apply f args)
                      ms (elapsed-ms ct0)]
                  (log-hook! :debug ::fn-returned ext-ns sym :call ms nil)
                  {:result r})
                (catch Throwable e
                  (let [ms (elapsed-ms ct0)]
                    (log-hook! :warn ::fn-threw ext-ns sym :call ms (ex-message e))
                    (let [recovery (run-on-error ext-ns sym-entry e env f args)]
                      (cond
                        (contains? recovery :result) recovery
                        (contains? recovery :error)  (throw (:error recovery))
                        :else {:result (apply (get recovery :fn f)
                                         (vec (get recovery :args args)))}))))))

            {:keys [result]} (run-after ext-ns sym-entry env f args (:result call-result))
            ms (elapsed-ms t0)]
        (log-hook! :info ::invoke-done ext-ns sym nil ms nil)
        result))))

(def ^:private ^:dynamic *log-writer*
  "Writer that sends output to the log file instead of stdout/stderr.
   Bound during extension invocations so tool fns never bleed into the TUI."
  nil)

(defn- get-log-writer []
  (or *log-writer*
    (let [log-path (str (System/getProperty "user.home") "/.vis/vis.log")]
      (alter-var-root #'*log-writer*
        (fn [cur] (or cur (io/writer log-path :append true))))
      *log-writer*)))

(defn wrap-extension
  "Wrap all function symbols in an extension into invocation fns.

   Returns a map of {sym → (fn [& args] result)} where each fn
   closes over the extension, symbol entry, and environment, then
   routes through `invoke-symbol-wrapper`.

   All stdout/stderr from extension calls is redirected to the log
   file so nothing bleeds into the TUI.

   Value symbols are returned as {sym → value}."
  [ext env]
  (into {}
    (map (fn [sym-entry]
           (let [sym (:ext.symbol/sym sym-entry)]
             (if (contains? sym-entry :ext.symbol/fn)
               [sym (fn [& args]
                      (let [w (get-log-writer)]
                        (binding [*out* w *err* w]
                          (invoke-symbol-wrapper ext sym-entry (vec args) env))))]
               [sym (:ext.symbol/val sym-entry)]))))
    (:ext/symbols ext)))

;; =============================================================================
;; Parse-error rescue — walked by the iteration loop
;; =============================================================================

(defn- code-mentions-symbol?
  [^String code ^String sym-name alias-name]
  (let [esc-name (java.util.regex.Pattern/quote sym-name)
        bare     (re-pattern (str "\\(\\s*" esc-name "(?:[\\s)\\[]|$)"))
        prefixed (when (and alias-name (seq alias-name))
                   (re-pattern (str "\\(\\s*"
                                 (java.util.regex.Pattern/quote alias-name)
                                 "/" esc-name "(?:[\\s)\\[]|$)")))]
    (boolean (or (re-find bare code)
               (and prefixed (re-find prefixed code))))))

(defn- run-parse-rescue-hook
  [id hook ctx]
  (try
    (hook ctx)
    (catch Throwable t
      (tel/log! {:level :warn :id ::on-parse-error-fn-threw
                 :data {:source id :error (ex-message t)}
                 :msg   (str ":on-parse-error-fn (" id ") threw: "
                          (ex-message t))})
      nil)))

(defn- try-symbol-parse-rescue
  [extensions code error environment]
  (loop [exts (seq extensions)]
    (when exts
      (let [ext   (first exts)
            alias (some-> (:ext/ns-alias ext) :alias clojure.core/name)
            hit
            (loop [syms (seq (:ext/symbols ext))]
              (when syms
                (let [entry (first syms)
                      sym   (:ext.symbol/sym entry)
                      hook  (:ext.symbol/on-parse-error-fn entry)]
                  (if (and hook sym (code-mentions-symbol? code (str sym) alias))
                    (let [out (run-parse-rescue-hook
                                (str (:ext/namespace ext) "/" sym)
                                hook
                                {:code        code
                                 :error       error
                                 :sym         sym
                                 :environment environment})]
                      (if (and (string? out) (not= out code))
                        out
                        (recur (next syms))))
                    (recur (next syms))))))]
        (or hit (recur (next exts)))))))

(defn- try-extension-parse-rescue
  [extensions code error environment]
  (loop [exts (seq extensions)]
    (when exts
      (let [ext  (first exts)
            hook (:ext/on-parse-error-fn ext)
            out  (when hook
                   (run-parse-rescue-hook (str (:ext/namespace ext))
                     hook
                     {:code        code
                      :error       error
                      :environment environment}))]
        (if (and (string? out) (not= out code))
          out
          (recur (next exts)))))))

(defn try-rescue-parse-error
  "Walk `extensions` and produce a rewritten source string for a
   broken `code`, or nil when nothing wants to rescue.

   Resolution order:
     1. Per-symbol `:ext.symbol/on-parse-error-fn` of any registered
        symbol whose name appears in `code`.
     2. Extension-level `:ext/on-parse-error-fn` as a fallback.

   Hooks that throw or return non-strings or the unchanged code are
   skipped."
  [extensions code error environment]
  (or (try-symbol-parse-rescue extensions code error environment)
    (try-extension-parse-rescue extensions code error environment)))

;; =============================================================================
;; Public API — extension builder
;; =============================================================================

(defn extension
  "Build and validate an extension. The canonical constructor.

   See docs/src/extensions/extension-spec.md for the full key list."
  [spec]
  (-> spec
    (cond-> (contains? spec :ext/prompt) (update :ext/prompt normalize-prompt))
    (cond->
      (not (:ext/activation-fn spec))                  (assoc :ext/activation-fn (constantly true))
      (and (not (:ext/subgroup spec))
        (some? (:ext/group spec)))                     (assoc :ext/subgroup (:ext/group spec))
      (not (:ext/symbols spec))                        (assoc :ext/symbols [])
      (not (:ext/classes spec))                        (assoc :ext/classes {})
      (not (:ext/imports spec))                        (assoc :ext/imports {})
      (not (:ext/requires spec))                       (assoc :ext/requires [])
      (not (:ext/cli spec))                            (assoc :ext/cli [])
      (not (:ext/channels spec))                       (assoc :ext/channels [])
      (not (:ext/providers spec))                      (assoc :ext/providers [])
      (not (:ext/persistance spec))                    (assoc :ext/persistance []))
    (validate!)))

;; =============================================================================
;; Global Extension Registry
;; =============================================================================

(defonce ^:private extension-registry
  ;; Process-level atom holding all globally registered extensions.
  ;; Keyed by :ext/namespace to prevent duplicates.
  (atom {}))

(defn- dispatch-providers! [providers]
  (doseq [provider-entry providers]
    (registry/register-provider! provider-entry)))

(defn- dispatch-persistance! [entries]
  (doseq [{:persistance/keys [id ns]} entries]
    (persistance/register-backend! id ns)))

(def ^:private EXTENSIONS_PARENT ["extensions"])

(defn- mount-under-extensions
  "Auto-place an `:ext/cli` entry under the `vis extensions` parent.

   Authors who want nested placement (e.g. `vis extensions git status`)
   can pass `:cmd/parent [\"extensions\" \"git\"]` and the dispatcher
   respects it AS LONG AS the first element is `\"extensions\"`. Any
   other parent is rejected."
  [{:cmd/keys [parent name] :as entry}]
  (cond
    (or (nil? parent) (= [] parent))
    (assoc entry :cmd/parent EXTENSIONS_PARENT)

    (= "extensions" (first parent))
    entry

    :else
    (throw (ex-info
             (str ":ext/cli entry '" name "' has :cmd/parent " (pr-str parent)
               " -- :ext/cli mounts only under [\"extensions\" ...]."
               " Use register-cmd! directly for arbitrary placement.")
             {:type :ext/cli-bad-parent
              :entry entry}))))

(defn register-extension!
  "Register an extension in the global process-level registry.

   This is THE single entry point for everything an extension
   contributes to vis. Whatever the extension declares -- SCI sandbox
   symbols (`:ext/symbols`), CLI commands (`:ext/cli`), channels
   (`:ext/channels`), LLM providers (`:ext/providers`), persistence
   backends (`:ext/persistance`) -- gets routed here and dispatched into
   the matching sub-registry as a side effect.

   Idempotent on `:ext/namespace`. Returns the validated extension."
  [ext]
  (let [ext    (extension ext)
        ns-sym (:ext/namespace ext)]
    (swap! extension-registry assoc ns-sym ext)
    (tel/log! {:level :info :id ::register-global
               :data {:ext ns-sym
                      :symbols     (count (:ext/symbols ext))
                      :cli         (count (:ext/cli ext))
                      :channels    (count (:ext/channels ext))
                      :providers   (count (:ext/providers ext))
                      :persistance (count (:ext/persistance ext))}
               :msg (str "Extension '" ns-sym "' registered globally")})
    (doseq [c (:ext/cli ext)]      (registry/register-cmd! (mount-under-extensions c)))
    (doseq [c (:ext/channels ext)] (registry/register-channel! c))
    (dispatch-providers!   (:ext/providers ext))
    (dispatch-persistance! (:ext/persistance ext))
    ext))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn deregister-extension! [ns-sym]
  (swap! extension-registry dissoc ns-sym)
  nil)

(defn registered-extensions []
  (vec (vals @extension-registry)))

(defn- topo-sort-extensions
  "Topologically sort extensions by :ext/requires.
   Throws on missing dependencies or cycles."
  [extensions]
  (let [by-ns   (into {} (map (juxt :ext/namespace identity)) extensions)
        visited (volatile! #{})
        path    (volatile! #{})
        result  (volatile! [])]
    (letfn [(visit [ns-sym]
              (when (contains? @path ns-sym)
                (throw (ex-info (str "Circular extension dependency: " ns-sym
                                  " → ... → " ns-sym)
                         {:type :extension/circular-dependency
                          :extension ns-sym
                          :path @path})))
              (when-not (contains? @visited ns-sym)
                (vswap! path conj ns-sym)
                (let [ext (get by-ns ns-sym)]
                  (when-not ext
                    (throw (ex-info (str "Extension '" ns-sym "' required but not registered")
                             {:type :extension/missing-dependency
                              :extension ns-sym
                              :available (vec (keys by-ns))})))
                  (doseq [dep (:ext/requires ext)]
                    (visit dep)))
                (vswap! path disj ns-sym)
                (vswap! visited conj ns-sym)
                (vswap! result conj (get by-ns ns-sym))))]
      (doseq [ns-sym (keys by-ns)]
        (visit ns-sym)))
    @result))

(defn register-extensions!
  "Install all globally registered extensions into an environment.

   Topologically sorts by :ext/requires so dependencies are registered
   before dependents. Throws on missing dependencies or cycles.

   Called by `create-environment` automatically. Returns environment."
  [environment register-fn!]
  (let [exts   (registered-extensions)
        sorted (when (seq exts) (topo-sort-extensions exts))]
    (doseq [ext sorted]
      (register-fn! environment ext))
    environment))

(defn load-extension!
  "Dynamically load an extension from a Clojure namespace.

   Requires the namespace (which should call `register-extension!` at
   load time), then returns the extension from the global registry."
  [ns-sym]
  (require ns-sym)
  (or (get @extension-registry ns-sym)
    (throw (ex-info (str "Namespace '" ns-sym
                      "' was loaded but did not call register-extension!")
             {:type :extension/no-registration
              :namespace ns-sym
              :registered (vec (keys @extension-registry))}))))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn reload-extension!
  "Reload an extension namespace and hot-swap it everywhere.

   1. Forces `(require ns :reload)` — re-executes `register-extension!`
   2. Updates the global registry (automatic via register-extension!)
   3. If `environments` are provided, replaces the old version in
      each live environment's `:extensions` atom immediately.

   Arity:
     (reload-extension! ns-sym)              — global registry only.
     (reload-extension! ns-sym environment)  — hot-swap into one env.
     (reload-extension! ns-sym environments) — hot-swap into all envs.

   Returns the updated extension."
  ([ns-sym]
   (reload-extension! ns-sym nil))
  ([ns-sym env-or-envs]
   (require ns-sym :reload)
   (let [ext (or (get @extension-registry ns-sym)
               (throw (ex-info (str "Namespace '" ns-sym
                                 "' was reloaded but did not call register-extension!")
                        {:type :extension/no-registration
                         :namespace ns-sym
                         :registered (vec (keys @extension-registry))})))
         envs (cond
                (nil? env-or-envs)        nil
                (map? env-or-envs)        [env-or-envs]
                (sequential? env-or-envs) env-or-envs)]
     (doseq [environment envs]
       (when-let [ext-atom (:extensions environment)]
         (swap! ext-atom
           (fn [exts]
             (let [without (vec (remove #(= (:ext/namespace %) ns-sym) exts))]
               (conj without ext))))
         (tel/log! {:level :info :id ::reload-hot-swap
                    :data {:ext ns-sym :environment-id (:environment-id environment)}
                    :msg (str "Hot-swapped '" ns-sym "' into environment " (:environment-id environment))})))
     ext)))

;; =============================================================================
;; Inline extension docs catalog
;;
;; Filled by `discover-extensions!` from every
;; `META-INF/vis-extension/vis.edn` on the classpath. Multiple jars
;; that declare the same id are merged: `:nses` deduped (preserving
;; first-occurrence order); `:docs` map-merged (later wins per name).
;; `:reflinks` are recomputed from the union of all `:links` on every
;; merge, so a later jar's links can target an earlier jar's docs.
;; =============================================================================

(defonce ^:private extension-docs-registry (atom {}))

(defn registered-extension-ids
  "Sorted vector of every extension id known to the docs registry."
  []
  (vec (sort (keys @extension-docs-registry))))

(defn extension-namespaces
  "Vector of namespaces declared under `:nses` for an id. Empty when
   the id is unknown."
  [id]
  (vec (get-in @extension-docs-registry [id :nses] [])))

(defn extension-id-of-ns
  "Reverse lookup: given a namespace symbol, return the extension id
   that registered it under `:nses`, or `nil`."
  [ns-sym]
  (some (fn [[id {nses :nses}]]
          (when (some #(= ns-sym %) nses) id))
    @extension-docs-registry))

(defn extension-doc
  "Return the full descriptor map for a declared extension doc:
   `{:name :created-at :description :content :links :reflinks}`. Returns
   `nil` when the id is unknown or no doc by that name was declared."
  [id doc-name]
  (when-let [descriptor (and id doc-name
                          (get-in @extension-docs-registry [id :docs doc-name]))]
    (assoc descriptor :name doc-name)))

(defn extension-doc-content
  "Plain `:content` body (Markdown string) of a declared doc, or `nil`
   when the doc is unknown."
  [id doc-name]
  (:content (extension-doc id doc-name)))

(defn extension-doc-description
  "Return the `:description` field of a declared extension doc, or `nil`
   when the doc is unknown."
  [id doc-name]
  (:description (extension-doc id doc-name)))

(defn extension-doc-summary
  "Lightweight doc descriptor (no `:content`):
   `{:name :created-at :description :links :reflinks}`. Returns `nil`
   when the doc is unknown."
  [id doc-name]
  (when-let [descriptor (and id doc-name
                          (get-in @extension-docs-registry [id :docs doc-name]))]
    (-> descriptor
      (dissoc :content)
      (assoc :name doc-name))))

(defn extension-docs
  "With one arg, return a vector of doc summaries for every doc
   declared by `id`. With no arg, return the full registry as
   `{<id-sym> [<summary> ...]}`. Sorted by doc name within each id."
  ([]
   (into {}
     (map (fn [[id {docs :docs}]]
            [id (mapv #(extension-doc-summary id %) (sort (keys docs)))]))
     @extension-docs-registry))
  ([id]
   (let [docs (get-in @extension-docs-registry [id :docs])]
     (mapv #(extension-doc-summary id %) (sort (keys docs))))))

(defn extension-doc-names
  "Plain sorted vector of doc names declared by `id`."
  [id]
  (vec (sort (keys (get-in @extension-docs-registry [id :docs])))))

(defn- merge-manifest-entry!
  [id entry]
  (swap! extension-docs-registry
    update id
    (fn [existing]
      (let [merged-nses (vec (distinct (concat (:nses existing) (:nses entry))))
            merged-docs (merge (or (:docs existing) {}) (or (:docs entry) {}))]
        {:nses merged-nses :docs merged-docs}))))

(defn- link-target
  "Return `[<target-id> <target-doc>]` for a cross-ext or same-ext
   doc link, or `nil` for url/file/external links."
  [from-id link]
  (cond
    (and (symbol? (:to-id link)) (string? (:to-doc link)))
    [(:to-id link) (:to-doc link)]

    (and (nil? (:to-id link)) (string? (:to-doc link)))
    [from-id (:to-doc link)]

    :else nil))

(defn- recompute-reflinks!
  "Walk every doc's `:links` across the entire registry and rebuild
   the `:reflinks` vector on each target. Idempotent."
  []
  (swap! extension-docs-registry
    (fn [registry]
      (let [cleared (reduce-kv
                      (fn [acc id entry]
                        (assoc acc id
                          (update entry :docs
                            (fn [docs]
                              (reduce-kv (fn [d name descriptor]
                                           (assoc d name (assoc descriptor :reflinks [])))
                                {} docs)))))
                      {} registry)
            with-reflinks
            (reduce-kv
              (fn [acc from-id entry]
                (reduce-kv
                  (fn [acc2 from-doc descriptor]
                    (reduce
                      (fn [acc3 link]
                        (if-let [[to-id to-doc] (link-target from-id link)]
                          (if (get-in acc3 [to-id :docs to-doc])
                            (update-in acc3 [to-id :docs to-doc :reflinks]
                              (fnil conj [])
                              (cond-> {:from-id  from-id
                                       :from-doc from-doc}
                                (string? (:context link))
                                (assoc :context (:context link))))
                            acc3)
                          acc3))
                      acc2 (:links descriptor)))
                  acc (:docs entry)))
              cleared cleared)]
        with-reflinks))))

(defn registered-extensions-summary
  "Pure data view of the docs registry: returns
   `{<id> {:nses [...] :docs {<name> <summary>}}}` for every loaded
   extension."
  []
  (reduce-kv
    (fn [acc id entry]
      (assoc acc id
        {:nses (:nses entry)
         :docs (reduce-kv (fn [d name _] (assoc d name (extension-doc-summary id name)))
                 {} (:docs entry))}))
    {} @extension-docs-registry))

(defn discover-extensions!
  "Public entry point for vis's classpath auto-discovery.

   Runs `manifest/scan-extensions!` (which scans every
   `META-INF/vis-extension/vis.edn`, `require`s every namespace
   listed under each manifest's `:nses` key, and returns the merged
   parsed manifests) and then merges every loaded extension's inline
   docs into this namespace's docs registry. Returns the count of
   namespaces declared under `:nses` across the merged manifests.

   Idempotent on both layers."
  []
  (let [manifests (manifest/scan-extensions!)]
    (doseq [[id entry] manifests]
      (merge-manifest-entry! id entry))
    (recompute-reflinks!)
    (count (mapcat :nses (vals manifests)))))

;; =============================================================================
;; CLI bridge -- the `vis extensions` parent
;;
;; Self-registers a top-level `extensions` command into the registry
;; whose subcommands are computed lazily from every command registered
;; with `:cmd/parent ["extensions"]`. Extensions populate this slot
;; through `:ext/cli` on `extension`; the `register-extension!`
;; dispatcher above forwards each entry to `register-cmd!`.
;; =============================================================================

(registry/register-cmd!
  {:cmd/name        "extensions"
   :cmd/doc         "Run an extension-provided CLI command."
   :cmd/usage       "vis extensions <cmd> [args…]"
   :cmd/subcommands #(registry/registered-under ["extensions"])})
