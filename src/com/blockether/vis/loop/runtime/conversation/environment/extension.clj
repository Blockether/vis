(ns com.blockether.vis.loop.runtime.conversation.environment.extension
  "Extension specification for the SCI sandbox environment.

   An extension is a namespace-like bundle that adds symbols, classes,
   and docs to the SCI sandbox. Extensions are the ONLY way to extend
   the sandbox.

   Use `(extension spec)` to build and validate."
  (:refer-clojure :exclude [symbol])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [taoensso.trove :as trove]))

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

;; Before hook: (fn [env f args] args'). Runs before :fn.
;; Receives the env, the implementation fn, and the original args.
;; Returns the (possibly transformed) args vector to pass to :fn.
;; Throw to abort the call.
(s/def :ext.symbol/before-fn fn?)

;; After hook: (fn [env f args result] result'). Runs after :fn returns.
;; Receives the env, the implementation fn, the args, and the raw result.
;; Returns the (possibly transformed) result.
(s/def :ext.symbol/after-fn fn?)

;; Error handler: (fn [err env f args] result-or-throw). Called when :fn throws.
;; Receives the exception, env, the implementation fn, and the original args.
;; Can return a fallback value, re-invoke f with different args, or re-throw.
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

;; Unique name for the extension, e.g. 'documents, 'git, 'filesystem.
;; Used as the identity key in the extension registry.
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

;; Rich documentation for the LLM - injected into the system prompt when
;; the extension is active. After normalization, always a fn.
;; The `extension` constructor and `validate!` both accept string | fn
;; and normalize strings to (constantly s).
(s/def :ext/prompt fn?)

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

(s/def ::extension
  (s/keys :req [:ext/namespace :ext/doc :ext/group :ext/subgroup
                :ext/activation-fn :ext/prompt :ext/symbols
                :ext/classes :ext/imports]))

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
      ;; Optional hooks:
      :before-fn   (fn [env f args] args')          ;; transform args before call
      :after-fn    (fn [env f args result] result')  ;; transform result after call
      :on-error-fn (fn [err env f args] ...)})       ;; handle error, can retry via (apply f args')"
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
  (trove/log! {:level level :id id
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

(defn wrap-extension
  "Wrap all function symbols in an extension into invocation fns.

   Returns a map of {sym → (fn [& args] result)} where each fn
   closes over the extension and symbol entry, routes through
   invoke-symbol-wrapper, and takes env as the last arg.

   Value symbols are returned as {sym → value}."
  [ext env]
  (into {}
    (map (fn [sym-entry]
           (let [sym (:ext.symbol/sym sym-entry)]
             (if (contains? sym-entry :ext.symbol/fn)
               [sym (fn [& args] (invoke-symbol-wrapper ext sym-entry (vec args) env))]
               [sym (:ext.symbol/val sym-entry)]))))
    (:ext/symbols ext)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn extension
  "Build and validate an extension. The canonical constructor.

   (def search-sym
     (symbol 'search-documents search-fn
       {:doc              \"Full-text search across documents.\"
        :arglists         '([query] [query opts])
        :examples         [\"(search-documents neural)\"]
        :before-fn        my-before-fn
        :after-fn         my-after-fn
        :on-error-fn      my-on-error-fn}))

   (def fetch-sym
     (symbol 'fetch-document-content fetch-fn
       {:doc              \"Fetch content by lookup ref.\"
        :arglists         '([lookup-id])
        :examples         [\"(fetch-document-content [:node/id page-42])\"]}))

   (def max-retries-sym
     (value 'max-retries 3
       {:doc \"Maximum retry attempts.\"}))

   (extension
     {:ext/namespace     'documents
      :ext/doc           \"Document search and retrieval\"
      :ext/group         \"knowledge\"
      :ext/prompt        \"Full-text search across ingested documents...\"
      :ext/activation-fn (fn [env] (seq (db/list-docs (:db-info env))))
      :ext/symbols       [search-sym fetch-sym max-retries-sym]
      :ext/classes       {'java.time.LocalDate java.time.LocalDate}
      :ext/imports       {'LocalDate java.time.LocalDate}})

   Returns a validated extension map conforming to ::extension."
  [spec]
  (-> spec
    (update :ext/prompt normalize-prompt)
    (cond->
      (not (:ext/activation-fn spec)) (assoc :ext/activation-fn (constantly true))
      (not (:ext/subgroup spec))      (assoc :ext/subgroup (:ext/group spec))
      (not (:ext/classes spec))       (assoc :ext/classes {})
      (not (:ext/imports spec))       (assoc :ext/imports {}))
    (validate!)))
