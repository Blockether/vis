(ns com.blockether.vis.extension
  "Extension specification for the SCI sandbox environment.

   An extension is a namespace-like bundle that adds symbols, classes,
   and docs to the SCI sandbox. Extensions are the ONLY way to extend
   the sandbox.

   Two ways to register extensions:

   1. **Global registry** — `(register-global! ext)` at ns load time.
      When an environment is created, `register-extensions!`
      topologically sorts by `:ext/requires` and registers them all.

   2. **Per-environment** — `(register-extension! environment ext)`
      from `loop.core` for ad-hoc registration.

   Extensions can dynamically load other extensions via
   `(load-extension! 'some.ext.ns)` which `require`s the namespace
   (triggering its `register-global!` call) and returns the extension.

   Use `(extension spec)` to build and validate."
  (:refer-clojure :exclude [symbol])
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- fn-or-string? [x] (or (fn? x) (non-blank-string? x)))

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

;; Before hook: (fn [env f args] → map). Runs before :fn.
;; Receives the environment, the implementation fn, and the original args.
;; Returns a map — see Hook Protocol in EXTENSION.md:
;;   {:args [...]}    — override args passed to :fn
;;   {:fn f'}         — override the implementation fn
;;   {:env env'}      — override env for the call
;;   {:result val}    — short-circuit: skip :fn entirely, return val
;; Missing keys keep the current value. Throw to abort.
(s/def :ext.symbol/before-fn fn?)

;; After hook: (fn [env f args result] → map). Runs after :fn returns.
;; Receives the environment, the implementation fn, the args, and the raw result.
;; Returns a map — see Hook Protocol in EXTENSION.md:
;;   {:result val}    — override the result
;;   {:env :fn :args} — override (rarely needed)
;; Missing keys keep the current value.
(s/def :ext.symbol/after-fn fn?)

;; Error handler: (fn [err env f args] → map). Called when :fn throws.
;; Receives the exception, environment, the implementation fn, and the original args.
;; Returns a map — see Hook Protocol in EXTENSION.md:
;;   {:result val}    — use as fallback result
;;   {:error err}     — throw this error instead
;;   {:fn f' :args a'} — retry with (possibly different) fn and args
;; If no :on-error-fn is defined, the original exception propagates.
(s/def :ext.symbol/on-error-fn fn?)

;; Plain value bound in the sandbox (constant, data, config).
;; Mutually exclusive with :ext.symbol/fn - a symbol is either a
;; function or a value, never both.
(s/def :ext.symbol/val some?)

;; Function symbol: :fn is required, hooks are optional.
(s/def ::fn-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/fn :ext.symbol/doc
                :ext.symbol/arglists :ext.symbol/examples]
    :opt [:ext.symbol/before-fn :ext.symbol/after-fn
          :ext.symbol/on-error-fn]))

;; Value symbol: just name + value + doc. No hooks, no arglists.
(s/def ::val-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/val :ext.symbol/doc]))

;; A symbol entry is either a function or a value.
(s/def ::symbol-entry
  (s/or :fn  ::fn-symbol-entry
    :val ::val-symbol-entry))

;; =============================================================================
;; Extension spec
;; =============================================================================

;; Fully qualified extension name, e.g. 'com.blockether.vis.ext.common.
;; Used as the identity key in the extension registry and stored in
;; iteration metadata for post-mortem / reproducibility.
(s/def :ext/namespace symbol?)

;; Extension-level documentation - describes what this bundle provides.
(s/def :ext/doc non-blank-string?)

;; Top-level group for prompt rendering, e.g. "knowledge", "conversation".
;; Extensions in the same group are rendered together in the system prompt.
(s/def :ext/group non-blank-string?)

;; Subgroup within the group, e.g. "documents" under "knowledge".
;; Finer-grained grouping for prompt layout.
(s/def :ext/subgroup non-blank-string?)

;; Guard evaluated at each query boundary. When falsy, ALL symbols in
;; this extension are unbound from the sandbox - the LLM cannot call them.
;; (fn [env] -> bool). Default: (constantly true).
(s/def :ext/activation-fn fn?)

;; Optional extra LLM-facing documentation appended AFTER the canonical
;; symbol-derived prompt block when the extension is active.
;; Accepts string | (fn [env] → string). Nil means: rely only on the
;; auto-rendered prompt derived from :ext/doc + :ext/symbols metadata.
(s/def :ext/prompt fn?)

;; Optional per-iteration nudge composer.
;; (fn [ctx] → string-or-nil). Called every iteration; return a
;; `[system_nudge] …` string to inject a nudge, nil to skip.
;; See docs/src/extensions/nudges.md for the ctx shape.
(s/def :ext/nudge-fn fn?)

;; Optional dependency declaration.
;; Vector of extension namespace symbols that must be registered
;; before this extension. Checked at `register-extension!` time.
;; e.g. ['filesystem 'git]
(s/def :ext/requires (s/coll-of symbol? :kind vector?))

;; Semver version string, e.g. "1.0.0", "0.3.1-SNAPSHOT".
(s/def :ext/version non-blank-string?)

;; Author name or org, e.g. "Blockether", "Jane Doe <jane@example.com>".
(s/def :ext/author non-blank-string?)

;; SPDX license identifier, e.g. "MIT", "Apache-2.0", "EPL-2.0".
(s/def :ext/license non-blank-string?)

;; Optional CLI commands exported by this extension.
;; Vector of {:cmd "name" :doc "description" :fn (fn [args] ...)}
;; Enables `vis ext <cmd> [args...]` invocation from the terminal.
(s/def :ext/cli (s/coll-of map? :kind vector?))

;; Vector of symbol entries this extension binds into the sandbox.
(s/def :ext/symbols (s/coll-of ::symbol-entry :kind vector?))

;; Map of fully-qualified Java classes to expose in the sandbox.
;; Keys are FQ symbols, values are the Class objects.
;; Enables `(java.time.LocalDate/now)` style access.
;; e.g. {'java.time.LocalDate java.time.LocalDate}
(s/def :ext/classes
  (s/and map?
    #(every? symbol? (keys %))
    #(every? class? (vals %))))

;; Map of short-name imports for Java classes.
;; Keys are short symbols, values are FQ symbols.
;; Enables `(LocalDate/now)` style access.
;; e.g. {'LocalDate java.time.LocalDate}
(s/def :ext/imports
  (s/and map?
    #(every? symbol? (keys %))
    #(every? symbol? (vals %))))

;; Optional SCI namespace alias for this extension's symbols.
;; When set, a dedicated SCI namespace is created and aliased so
;; the LLM can call `(fs/read-file "x")` in addition to `(read-file "x")`.
;; e.g. {:ns 'vis.ext.fs :alias 'fs}
(s/def :ext/ns-alias
  (s/and map?
    #(symbol? (:ns %))
    #(symbol? (:alias %))))

(defn- ns-alias-required-when-symbols?
  "When :ext/symbols is non-empty, :ext/ns-alias must be present."
  [ext]
  (or (empty? (:ext/symbols ext))
      (some? (:ext/ns-alias ext))))

(s/def ::extension
  (s/and
    (s/keys :req [:ext/namespace :ext/doc :ext/group :ext/subgroup
                  :ext/activation-fn :ext/symbols
                  :ext/classes :ext/imports]
      :opt [:ext/ns-alias :ext/prompt :ext/nudge-fn :ext/requires
            :ext/version :ext/author :ext/license
            :ext/cli])
    ns-alias-required-when-symbols?))

;; =============================================================================
;; Symbol helper
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
   Optional: :examples, :before-fn, :after-fn, :on-error-fn

   Defaults:
     :examples — derived from :arglists when not provided

   (symbol 'search-documents search-fn
     {:doc      \"Full-text search across documents.\"
      :arglists '([query] [query opts])
      :examples [\"(search-documents \\\"neural\\\")\"]
      ;; Optional hooks — each returns a MAP, not a direct value.
      ;; See Hook Protocol in EXTENSION.md for every return key.
      :before-fn   (fn [env f args] {:args (transform args)})    ;; override args/fn/env, or {:result v} to short-circuit
      :after-fn    (fn [env f args result] {:result (transform result)})  ;; override result
      :on-error-fn (fn [err env f args] {:result fallback})})    ;; recover, retry, or {:error e} to re-throw"
  [sym-name f opts]
  (let [arglists (:arglists opts)
        arglists (when arglists (if (seq? arglists) (vec arglists) arglists))
        examples (or (:examples opts)
                   (when arglists (derive-examples sym-name arglists)))]
    (validate-symbol-entry!
      (cond-> #:ext.symbol{:sym sym-name
                           :fn  f}
        (:doc opts)        (assoc :ext.symbol/doc (:doc opts))
        arglists           (assoc :ext.symbol/arglists arglists)
        examples           (assoc :ext.symbol/examples (vec examples))
        (:before-fn opts)  (assoc :ext.symbol/before-fn (:before-fn opts))
        (:after-fn opts)   (assoc :ext.symbol/after-fn (:after-fn opts))
        (:on-error-fn opts)(assoc :ext.symbol/on-error-fn (:on-error-fn opts))))))


(defn value
  "Build a value symbol entry - a plain constant/data binding.

   (value 'max-retries 3
     {:doc \"Maximum retry attempts.\"})

   (value 'config {:host \"localhost\" :port 3000}
     {:doc \"Server configuration map.\"})

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
   - :ext/ns-alias optional {:alias 'fs}
   - :ext/symbols  vector of ext/symbol + ext/value entries
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
;; Normalization
;; =============================================================================

(defn- normalize-prompt [prompt]
  (cond
    (nil? prompt)    nil
    (fn? prompt)     prompt
    (string? prompt) (constantly prompt)
    :else (throw (ex-info ":ext/prompt must be a string or (fn [env] string)"
                   {:got (type prompt)}))))

;; =============================================================================
;; Validation
;; =============================================================================

(defn validate!
  "Normalize and assert that an extension map conforms to ::extension.
   Normalizes :ext/prompt (string → fn) before checking the spec.
   Throws with spec explain-data on violation."
  [ext]
  (let [ext (update ext :ext/prompt normalize-prompt)]
    (when-not (s/valid? ::extension ext)
      (throw (ex-info (str "Invalid extension '" (:ext/namespace ext) "':\n"
                        (with-out-str (s/explain ::extension ext)))
               {:type      :extension/invalid-spec
                :namespace (:ext/namespace ext)
                :explain   (s/explain-data ::extension ext)})))
    ext))

;; =============================================================================
;; Hook execution - runtime wrappers with output validation + logging
;;
;; Every hook returns {:fn f :args args :env env} to override the call
;; context. All keys are optional - missing keys keep the current value.
;; :before-fn can return {:result val} to short-circuit without calling :fn.
;; :on-error-fn can return {:result val}, {:error err}, or {:fn :args :env} to retry.
;; :after-fn can return {:result val} to override the result.
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

(defn- log! [level id ext-ns sym phase ms extra-msg]
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
          _   (log! :debug ::before-fn ext-ns sym :before-fn nil nil)
          ret (call-hook ":before-fn" sym before [env f args])
          _   (validate-hook-return! ":before-fn" sym ret)
          ms  (elapsed-ms t0)]
      (if (contains? ret :result)
        (do (log! :debug ::before-fn-done ext-ns sym :before-fn ms "short-circuited")
          {:result (:result ret)})
        (do (log! :debug ::before-fn-done ext-ns sym :before-fn ms nil)
          {:env  (get ret :env env)
           :fn   (get ret :fn f)
           :args (vec (get ret :args args))})))
    {:env env :fn f :args args}))

(defn- run-after [ext-ns sym-entry env f args result]
  (if-let [after (:ext.symbol/after-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log! :debug ::after-fn ext-ns sym :after-fn nil nil)
          ret (call-hook ":after-fn" sym after [env f args result])
          _   (validate-hook-return! ":after-fn" sym ret)
          ms  (elapsed-ms t0)]
      (log! :debug ::after-fn-done ext-ns sym :after-fn ms nil)
      {:env    (get ret :env env)
       :fn     (get ret :fn f)
       :args   (vec (get ret :args args))
       :result (get ret :result result)})
    {:env env :fn f :args args :result result}))

(defn- run-on-error [ext-ns sym-entry err env f args]
  (if-let [on-error (:ext.symbol/on-error-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log! :warn ::on-error-fn ext-ns sym :on-error-fn nil (str "handling: " (ex-message err)))
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
        (do (log! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "fallback result") ret)
        (contains? ret :error)
        (do (log! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "surfacing error") ret)
        :else
        (do (log! :info ::on-error-fn-done ext-ns sym :on-error-fn ms "retrying") ret)))
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
        _      (log! :info ::invoke ext-ns sym nil nil nil)
        before-out (run-before ext-ns sym-entry env (:ext.symbol/fn sym-entry) args)]
    (if (contains? before-out :result)
      (let [ms (elapsed-ms t0)]
        (log! :info ::invoke-done ext-ns sym nil ms "short-circuited")
        (:result before-out))
      (let [{env  :env
             f    :fn
             args :args} before-out

            call-result
            (let [ct0 (System/nanoTime)]
              (try
                (let [r  (apply f args)
                      ms (elapsed-ms ct0)]
                  (log! :debug ::fn-returned ext-ns sym :call ms nil)
                  {:result r})
                (catch Throwable e
                  (let [ms (elapsed-ms ct0)]
                    (log! :warn ::fn-threw ext-ns sym :call ms (ex-message e))
                    (let [recovery (run-on-error ext-ns sym-entry e env f args)]
                      (cond
                        (contains? recovery :result) recovery
                        (contains? recovery :error)  (throw (:error recovery))
                        :else {:result (apply (get recovery :fn f)
                                         (vec (get recovery :args args)))}))))))  

            {:keys [result]} (run-after ext-ns sym-entry env f args (:result call-result))
            ms (elapsed-ms t0)]
        (log! :info ::invoke-done ext-ns sym nil ms nil)
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
;; Public API
;; =============================================================================

(defn extension
  "Build and validate an extension. The canonical constructor.

   Keys:
     :ext/namespace      — required, fully qualified symbol, e.g. 'com.blockether.vis.ext.common
     :ext/doc            — required, extension-level description
     :ext/group          — required, prompt group (e.g. \"knowledge\")
     :ext/subgroup       — optional, defaults to :ext/group
     :ext/activation-fn  — optional, (fn [env] → bool), default (constantly true)
     :ext/prompt         — optional, string or (fn [env] → string);
                           appended after the canonical auto-rendered
                           symbol prompt
     :ext/nudge-fn       — optional, (fn [ctx] → string|nil)
     :ext/requires       — optional, vector of extension namespace symbols
                           that must be registered first, default []
     :ext/version        — optional, semver string, e.g. \"1.0.0\"
     :ext/author         — optional, author name/org
     :ext/license        — optional, SPDX identifier, e.g. \"MIT\"
     :ext/symbols        — required, vector of symbol entries
     :ext/classes        — optional, {fq-symbol → Class}, default {}
     :ext/imports        — optional, {short-symbol → fq-symbol}, default {}
     :ext/ns-alias       — required when :ext/symbols is non-empty,
                           {:ns 'vis.ext.fs :alias 'fs}
                           Creates a dedicated SCI namespace with an alias
                           so the LLM can call (fs/read-file ...) in addition
                           to (read-file ...). Symbols are always also bound
                           into the sandbox namespace.

   Example:

   (extension
     {:ext/namespace     'com.blockether.vis.ext.documents
      :ext/doc           \"Document search and retrieval\"
      :ext/group         \"knowledge\"
      :ext/requires      ['filesystem]
      :ext/prompt        \"Prefer narrow searches before broad scans.\"
      :ext/activation-fn (fn [env] (seq (list-docs (:db-info env))))
      :ext/nudge-fn      (fn [{:keys [environment iteration]}]
                           (when (> iteration 8)
                             \"[system_nudge] Narrow search scope.\"))
      :ext/symbols       [search-sym max-results-sym]})

   Returns a validated extension map conforming to ::extension."
  [spec]
  (-> spec
    (update :ext/prompt normalize-prompt)
    (cond->
      (not (:ext/activation-fn spec)) (assoc :ext/activation-fn (constantly true))
      (not (:ext/subgroup spec))      (assoc :ext/subgroup (:ext/group spec))
      (not (:ext/classes spec))       (assoc :ext/classes {})
      (not (:ext/imports spec))       (assoc :ext/imports {})
      (not (:ext/requires spec))      (assoc :ext/requires []))
    (validate!)))

;; =============================================================================
;; Global Extension Registry
;; =============================================================================

(defonce ^:private global-registry
  ;; Process-level atom holding all globally registered extensions.
  ;; Keyed by :ext/namespace to prevent duplicates.
  (atom {}))

(defn register-global!
  "Register an extension in the global process-level registry.

   Call this at namespace load time so the extension is available
   to every environment created afterwards:

     (ns my.company.ext.git
       (:require [....extension :as ext]))

     (ext/register-global!
       (ext/extension
         {:ext/namespace 'com.acme.ext.git
          :ext/requires  ['com.blockether.vis.ext.common]
          ...}))

   Idempotent — re-registering the same :ext/namespace replaces
   the previous version. Returns the extension."
  [ext]
  (let [ns-sym (:ext/namespace ext)]
    (swap! global-registry assoc ns-sym ext)
    (tel/log! {:level :info :id ::register-global
               :data {:ext ns-sym}
               :msg (str "Extension '" ns-sym "' registered globally")})
    ext))

(defn deregister-global!
  "Remove an extension from the global registry by namespace symbol."
  [ns-sym]
  (swap! global-registry dissoc ns-sym)
  nil)

(defn registered-extensions
  "Returns all globally registered extensions as a vector."
  []
  (vec (vals @global-registry)))

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

   Requires the namespace (which should call `register-global!` at
   load time), then returns the extension from the global registry.

   This is how an extension loads another extension at runtime:

     (ext/load-extension! 'my.company.ext.git)

   The loaded extension's `register-global!` fires during `require`,
   making it available for the next `register-extensions!` call
   or for immediate `register-extension!` into a live environment.

   Returns the extension map, or throws if the namespace doesn't
   register an extension."
  [ns-sym]
  (require ns-sym)
  (or (get @global-registry ns-sym)
    (throw (ex-info (str "Namespace '" ns-sym
                      "' was loaded but did not call register-global!")
             {:type :extension/no-registration
              :namespace ns-sym
              :registered (vec (keys @global-registry))}))))

(defn reload-extension!
  "Reload an extension namespace and hot-swap it everywhere.

   1. Forces `(require ns :reload)` — re-executes `register-global!`
   2. Updates the global registry (automatic via register-global!)
   3. If `environments` are provided, replaces the old version in
      each live environment's `:extensions` atom immediately.

   Arity:
     (reload-extension! ns-sym)
       Reload + update global registry only.

     (reload-extension! ns-sym environment)
       Reload + update global + hot-swap into one environment.

     (reload-extension! ns-sym environments)
       Reload + update global + hot-swap into all environments.

   This is what a meta-extension calls to hot-reload another
   extension into running conversations without restart:

     (ext/reload-extension! 'my.company.ext.git environment)

   Returns the updated extension."
  ([ns-sym]
   (reload-extension! ns-sym nil))
  ([ns-sym env-or-envs]
   (require ns-sym :reload)
   (let [ext (or (get @global-registry ns-sym)
               (throw (ex-info (str "Namespace '" ns-sym
                                 "' was reloaded but did not call register-global!")
                        {:type :extension/no-registration
                         :namespace ns-sym
                         :registered (vec (keys @global-registry))})))
         envs (cond
                (nil? env-or-envs)  nil
                (map? env-or-envs)  [env-or-envs]
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
;; Classpath auto-discovery
;; =============================================================================

(def ^:private EXTENSIONS_RESOURCE "META-INF/vis/extensions.edn")

(defn discover-extensions!
  "Scan the classpath for `META-INF/vis/extensions.edn` resources.

   Each file contains a vector of namespace symbols, e.g.:
     [com.blockether.vis.ext.editing
      com.acme.ext.git]

   Every discovered namespace is `require`d (which triggers its
   `register-global!` call). Already-registered namespaces are
   skipped. Returns the count of newly loaded extensions.

   Convention: drop a `META-INF/vis/extensions.edn` into your
   extension's resources/ and it will be picked up automatically
   on any classpath that includes it — no manual `require` needed."
  []
  (let [urls  (try
                (enumeration-seq
                  (.getResources
                    (.getContextClassLoader (Thread/currentThread))
                    EXTENSIONS_RESOURCE))
                (catch Exception _ nil))
        already (into #{} (keys @global-registry))
        loaded  (atom 0)]
    (doseq [^java.net.URL url urls]
      (try
        (let [content (slurp url)
              ns-syms (clojure.edn/read-string content)]
          (when (and (sequential? ns-syms) (seq ns-syms))
            (doseq [ns-sym ns-syms]
              (when (and (symbol? ns-sym) (not (contains? already ns-sym)))
                (try
                  (require ns-sym)
                  (swap! loaded inc)
                  (tel/log! {:level :info :id ::discover-ext
                             :data {:ext ns-sym :source (str url)}
                             :msg (str "Auto-discovered extension '" ns-sym "' from " url)})
                  (catch Throwable t
                    (tel/log! {:level :error :id ::discover-ext-failed
                               :data {:ext ns-sym :source (str url)
                                      :class (.getName (class t))
                                      :message (ex-message t)}
                               :msg (str "Failed to load auto-discovered extension '" ns-sym "': " (ex-message t))})))))))
        (catch Throwable t
          (tel/log! {:level :error :id ::discover-ext-parse-failed
                     :data {:source (str url) :message (ex-message t)}
                     :msg (str "Failed to parse " url ": " (ex-message t))}))))
    @loaded))
