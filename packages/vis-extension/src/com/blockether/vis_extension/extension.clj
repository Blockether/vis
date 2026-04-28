(ns com.blockether.vis-extension.extension
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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis-extension.channel :as channel]
            [com.blockether.vis-extension.commandline.base :as cmd]
            [com.blockether.vis-persistance.core :as persistance]
            [com.blockether.vis-extension.provider :as provider]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Predicates
;; =============================================================================

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
;; Receives the environment, the implementation fn, and the original args.
;; Returns a map — see Symbol Decorators in docs/src/extensions/hooks.md:
;;   {:args [...]}    — override args passed to :fn
;;   {:fn f'}         — override the implementation fn
;;   {:env env'}      — override env for the call
;;   {:result val}    — short-circuit: skip :fn entirely, return val
;; Missing keys keep the current value. Throw to abort.
;; This is the same pattern as Ring middleware / Pedestal :enter.
(s/def :ext.symbol/before-fn fn?)

;; Exit decorator: (fn [env f args result] → map). Wraps :fn on the way out.
;; Receives the environment, the implementation fn, the args, and the raw result.
;; Returns a map — see Symbol Decorators in docs/src/extensions/hooks.md:
;;   {:result val}    — override the result
;;   {:env :fn :args} — override (rarely needed)
;; Missing keys keep the current value.
;; This is the same pattern as Ring middleware / Pedestal :leave.
(s/def :ext.symbol/after-fn fn?)

;; Error decorator: (fn [err env f args] → map). Called when :fn throws.
;; Receives the exception, environment, the implementation fn, and the original args.
;; Returns a map — see Symbol Decorators in docs/src/extensions/hooks.md:
;;   {:result val}    — use as fallback result
;;   {:error err}     — throw this error instead
;;   {:fn f' :args a'} — retry with (possibly different) fn and args
;; If no :on-error-fn is defined, the original exception propagates.
;; This is the same pattern as Pedestal :error.
(s/def :ext.symbol/on-error-fn fn?)

;; Post-call autobind hook.
;; (fn [{:keys [args result environment]}] → map|nil)
;; Called only after a successful symbol call (after :after-fn).
;; Return nil to opt out for this invocation.
;;
;; Non-nil return map shape:
;;   {:bindings [{:kind keyword
;;                :id any
;;                :content any
;;                :doc string?
;;                :tag any?}
;;               ...]}
;;
;; The runtime owns symbol naming and persistence. Hooks only describe
;; WHAT should be bound (kind + id + content), not HOW symbols are named.
(s/def :ext.symbol/autobind-fn fn?)

;; Source-code rewriter for SCI/edamame parse errors that mention this
;; symbol. (fn [{:code :error :sym :environment}] → string-or-nil).
;; The iteration loop scans the broken source for any registered
;; symbol whose name appears (with or without :ext/ns-alias prefix)
;; and only invokes that symbol's hook — keeping rescue logic
;; co-located with the tool it repairs.
;;
;; Returns:
;;   - a NEW source string — the loop retries parsing with it; if it
;;     parses cleanly the rewritten code is executed and the
;;     expression result is tagged :repaired? true
;;   - nil — pass; the next matching symbol's hook is consulted, then
;;     the extension-level :ext/on-parse-error-fn fallback, then the
;;     original error is surfaced to the LLM
;;
;; Hooks that throw are logged and treated as if they returned nil.
;; A parse error is by definition a pre-dispatch failure — there is
;; no `:fn` to call — so this hook is the ONLY way a symbol can
;; influence parse-time recovery.
(s/def :ext.symbol/on-parse-error-fn fn?)

;; Plain value bound in the sandbox (constant, data, config).
;; Mutually exclusive with :ext.symbol/fn - a symbol is either a
;; function or a value, never both.
(s/def :ext.symbol/val some?)

;; Function symbol: :fn is required, hooks are optional.
(s/def ::fn-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/fn :ext.symbol/doc
                :ext.symbol/arglists :ext.symbol/examples]
    :opt [:ext.symbol/before-fn :ext.symbol/after-fn
          :ext.symbol/on-error-fn :ext.symbol/autobind-fn
          :ext.symbol/on-parse-error-fn]))

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

;; Optional source-code rewriter for SCI/edamame parse errors.
;; (fn [ctx] → string-or-nil), where ctx is
;;   {:code        original code string the LLM emitted
;;    :error       edamame's error message (e.g.
;;                 "[line 1, col 12] Unsupported escape character: \|")
;;    :environment the live conversation environment}
;; Returns:
;;   - a NEW source string — the iteration loop retries parsing/eval
;;     with the rewritten code and tags the result :repaired? true
;;   - nil — pass; the next extension's hook is consulted, or the
;;     parse error is surfaced to the LLM as before
;;
;; Symbol-level :on-error-fn cannot help here because parse failures
;; happen before any tool fn is dispatched, so the recovery is
;; necessarily extension-wide. Hooks that throw are logged and
;; treated as if they returned nil.
(s/def :ext/on-parse-error-fn fn?)

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

;; ============================================================================
;; Surface slots
;;
;; An extension declaration is the SINGLE entry point for everything
;; an extension contributes to vis. Whatever surfaces the extension
;; populates -- SCI sandbox symbols, CLI commands, channels,
;; providers, persistance entries -- it does so by listing them in
;; the matching `:ext/<surface>` slot. `ext/register-global!` then
;; dispatches each slot to its concrete sub-registry under the hood.
;;
;; The slot specs validate the FULL entry shape per surface -- vector
;; of `map?` is not enough, because a typo at extension-author time
;; (e.g. `:cmd/run` vs `:cmd/run-fn`, missing `:channel/main-fn`,
;; non-keyword `:provider/id`) would otherwise only blow up at
;; dispatch time, far from the offending `(register-global! …)` call
;; that introduced it. Concrete entry specs catch these at validation.
;;
;; Where possible we DELEGATE to the canonical spec for that surface
;; instead of duplicating field rules:
;;   :ext/cli      -> :com.blockether.vis-extension.commandline.base/command
;;   :ext/channels -> :com.blockether.vis-extension.channel/channel
;; All registries now live inside vis-loop. The extension facade can
;; require them directly; there is no longer a separate compatibility
;; boundary between extension, commandline, provider, and persistence.
;; ============================================================================

;; CLI commands exported by this extension. Each entry must conform
;; to the canonical commandline command shape
;; (`:com.blockether.vis-extension.commandline.base/command`): `:cmd/name` +
;; `:cmd/doc` are required; `:cmd/usage`, `:cmd/args`, `:cmd/run-fn`,
;; `:cmd/subcommands`, `:cmd/owns-tty?`, `:cmd/examples`,
;; `:cmd/parent` optional. See commandline.base for the per-key specs.
;;
;; Every entry is auto-mounted under `vis extensions <name>` -- THIS
;; SLOT IS THE EXTENSIONS SUBCOMMAND TREE. The dispatcher sets
;; `:cmd/parent ["extensions"]` for entries that don't specify one;
;; entries that DO specify must start with `"extensions"` (deeper
;; nests like `["extensions" "git"]` are allowed for sub-trees) --
;; that's enforced at register time by `validate-cli-entry-parent!`,
;; below. The spec here only checks structural shape.
;;
;; Top-level built-ins like `vis run` / `vis auth` are the binary's
;; commands, not an extension's, and use `cmd/register-global!`
;; directly -- `:ext/cli` does NOT and CANNOT mount at the top level.
;;
;;     :ext/cli [{:cmd/name   "blame"
;;                :cmd/doc    "Show git blame."
;;                :cmd/run-fn #'cli-blame}]
;;
;;     ;; deeper nest -- shows up as `vis extensions git status`
;;     :ext/cli [{:cmd/name   "status"
;;                :cmd/doc    "Show git status."
;;                :cmd/parent ["extensions" "git"]
;;                :cmd/run-fn #'cli-git-status}]
(s/def :ext/cli
  (s/coll-of :com.blockether.vis-extension.commandline.base/command :kind vector?))

;; Channels exported by this extension. Each entry must conform to
;; the canonical channel shape (`:com.blockether.vis-extension.channel/channel`):
;; `:channel/id` (keyword), `:channel/cmd` (non-blank string),
;; `:channel/doc` (non-blank string), `:channel/main-fn` (ifn) are
;; required; `:channel/usage` (non-blank string), `:channel/owns-tty?`
;; (boolean) are optional. Each entry is forwarded to
;; `channel/register-global!`; it appears under `vis channels <cmd>`.
(s/def :ext/channels
  (s/coll-of :com.blockether.vis-extension.channel/channel :kind vector?))

;; LLM providers exported by this extension. Each entry mirrors the
;; canonical provider shape. Required: `:provider/id` (keyword),
;; `:provider/label` (non-blank string). Optional fns (every one ifn):
;; `:provider/status-fn`, `:provider/logout-fn`, `:provider/detect-fn`,
;; `:provider/auth-fn`, `:provider/get-token-fn`. The `or-nil-or-fn`
;; predicate accepts an absent key (treated as nil) or any IFn.
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

;; Persistence backends exported by this extension. Each entry is
;; `{:persistance/id <keyword>
;;   :persistance/ns <fully-qualified-symbol>}` -- the id is the
;; backend tag stored in config (`{:backend :sqlite ...}`) and the
;; ns is the namespace to `require` so its `register-backend!` call
;; runs. Each entry is forwarded to `persistance/register-backend!`.
;;
;; Slot name preserves the existing persistence vocabulary (and the
;; existing `:cli`/`:channels`/`:providers` slot-naming convention --
;; capability area, not the implementation noun "backend").
(s/def :persistance/id keyword?)
(s/def :persistance/ns
  ;; Must be a SYMBOL we can pass to `require` -- a fully-qualified
  ;; namespace symbol like 'com.blockether.vis.ext.persistance-sqlite.core,
  ;; not just any symbol. Allowing bare symbols would let an entry
  ;; ship a typo'd value that only blows up at the requiring-resolve
  ;; site (mid-boot, far from the extension declaration).
  (s/and symbol?
    #(nil? (namespace %))
    #(re-find #"\." (name %))))
(s/def :ext/persistance-entry (s/keys :req [:persistance/id :persistance/ns]))
(s/def :ext/persistance (s/coll-of :ext/persistance-entry :kind vector?))

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
;; the LLM can call `(vis/cat "x")` in addition to `(cat "x")`.
;; e.g. {:ns 'vis.ext.tools :alias 'vis}
;;
;; Both `:ns` and `:alias` must be plain (non-namespaced) symbols --
;; SCI uses them as namespace names and aliases respectively, neither
;; of which is allowed to itself carry a namespace.
(s/def :ext.ns-alias/ns    (s/and symbol? #(nil? (namespace %))))
(s/def :ext.ns-alias/alias (s/and symbol? #(nil? (namespace %))))
(s/def :ext/ns-alias
  (s/and map?
    #(s/valid? :ext.ns-alias/ns    (:ns %))
    #(s/valid? :ext.ns-alias/alias (:alias %))))

(defn- ns-alias-required-when-symbols?
  "When :ext/symbols is non-empty, :ext/ns-alias must be present."
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/ns-alias ext))))

(defn- group-required-when-symbols?
  "When :ext/symbols is non-empty, :ext/group must be present (it's the
   prompt-rendering bucket for those symbols)."
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/group ext))))

(s/def ::extension
  (s/and
    ;; Only `:ext/namespace` and `:ext/doc` are unconditionally required.
    ;; Everything else is optional and defaulted -- an extension that
    ;; only ships, say, `:ext/channels` shouldn't be forced to declare
    ;; `:ext/group`, `:ext/symbols`, `:ext/activation-fn`, etc. just
    ;; because the SCI surface exists.
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
   Optional: :examples, :before-fn, :after-fn, :on-error-fn,
             :autobind-fn, :on-parse-error-fn

   Defaults:
     :examples — derived from :arglists when not provided

   (symbol 'search-documents search-fn
     {:doc      \"Full-text search across documents.\"
      :arglists '([query] [query opts])
      :examples [\"(search-documents \\\"neural\\\")\"]
      ;; Runtime hooks — fire AFTER `:fn` is dispatched. Each returns a
      ;; MAP, not a direct value. See Symbol Decorators in docs/src/extensions/hooks.md.
      :before-fn   (fn [env f args] {:args (transform args)})    ;; override args/fn/env, or {:result v} to short-circuit
      :after-fn    (fn [env f args result] {:result (transform result)})  ;; override result
      :on-error-fn (fn [err env f args] {:result fallback})    ;; recover, retry, or {:error e} to re-throw
      :autobind-fn (fn [{:keys [args result environment]}]
                     {:bindings [{:kind :file :id (first args) :content result}]})
      ;; Parse-time hook — fires when SCI/edamame rejects the LLM's
      ;; source AND this symbol's name appears in the broken code.
      ;; Returns rewritten source (string) or nil.
      :on-parse-error-fn (fn [{:keys [code error sym environment]}]
                           (rewrite-source code error))})"
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
        (:on-error-fn opts)       (assoc :ext.symbol/on-error-fn (:on-error-fn opts))
        (:autobind-fn opts)       (assoc :ext.symbol/autobind-fn (:autobind-fn opts))
        (:on-parse-error-fn opts) (assoc :ext.symbol/on-parse-error-fn (:on-parse-error-fn opts))))))

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
   - :ext/ns-alias optional {:alias 'vis}
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
;; Parse-error rescue — walked by the iteration loop
;;
;; SCI/edamame parse failures happen BEFORE any tool fn is dispatched,
;; so symbol-level :on-error-fn is useless for them. Two recovery
;; layers exist instead, in priority order:
;;
;;   1. SYMBOL-level `:ext.symbol/on-parse-error-fn` — fires only for
;;      symbols whose name is mentioned in the broken source. This
;;      keeps rescue logic co-located with the tool that caused it.
;;   2. EXTENSION-level `:ext/on-parse-error-fn` — a catch-all for
;;      cross-cutting rewrites (e.g. \"strip every JSON-style
;;      smart-quote\"). Fires only when no symbol-level hook produced
;;      a rewrite.
;;
;; Both layers receive `{:code :error :environment}`; symbol-level
;; hooks additionally receive `:sym`. First non-nil rewrite different
;; from `code` wins. Hooks that throw are logged and skipped — a
;; buggy hook can never break query execution.
;; =============================================================================

(defn- code-mentions-symbol?
  "Cheap regex check: does `code` look like it invokes `sym-name`,
   either bare `(name ...)` or aliased `(alias/name ...)`? We can't
   parse the broken source, so this is a substring scan with word
   boundaries. False positives are harmless — the worst case is the
   hook gets called and returns nil."
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
  "Invoke a single parse-error hook, swallowing throws.
   `id` is purely a tag for the warn log so we can tell symbol- vs
   extension-level breakage apart."
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
  "Walk every symbol of every extension. For symbols whose name appears
   in `code` AND that carry `:ext.symbol/on-parse-error-fn`, call the
   hook. First non-nil rewrite wins."
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
  "Walk extension-level `:ext/on-parse-error-fn` hooks as a catch-all
   layer for cross-cutting rewrites that aren't tied to one symbol."
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
     :ext/on-parse-error-fn — optional, (fn [{:code :error :environment}]
                           → string|nil). Called when SCI/edamame
                           rejects the LLM's source. Return a
                           rewritten source string to retry, nil to
                           defer to the next extension. See
                           `try-rescue-parse-error`.
     :ext/requires       — optional, vector of extension namespace symbols
                           that must be registered first, default []
     :ext/version        — optional, semver string, e.g. \"1.0.0\"
     :ext/author         — optional, author name/org
     :ext/license        — optional, SPDX identifier, e.g. \"MIT\"
     :ext/symbols        — required, vector of symbol entries
     :ext/classes        — optional, {fq-symbol → Class}, default {}
     :ext/imports        — optional, {short-symbol → fq-symbol}, default {}
     :ext/ns-alias       — required when :ext/symbols is non-empty,
                           {:ns 'vis.ext.tools :alias 'vis}
                           Creates a dedicated SCI namespace with that alias.
                           Symbols are bound ONLY into this aliased namespace,
                           NEVER into the `sandbox` namespace directly. The
                           alias is auto-required in the sandbox so the LLM
                           must call (vis/cat ...) — bare
                           (cat ...) does not resolve.

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
    ;; `:ext/prompt` is optional. Only run normalize-prompt when the
    ;; key is present, otherwise we'd insert a nil value that fails
    ;; the (s/def :ext/prompt fn?) spec.
    (cond-> (contains? spec :ext/prompt) (update :ext/prompt normalize-prompt))
    (cond->
      (not (:ext/activation-fn spec))                  (assoc :ext/activation-fn (constantly true))
      ;; `:ext/subgroup` defaults to `:ext/group` -- but only when
      ;; `:ext/group` is itself present (extensions that don't ship
      ;; SCI symbols don't need either).
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

(defonce ^:private global-registry
  ;; Process-level atom holding all globally registered extensions.
  ;; Keyed by :ext/namespace to prevent duplicates.
  (atom {}))

(defn- dispatch-providers!
  "Forward each `:ext/providers` entry to the provider registry."
  [providers]
  (doseq [provider-entry providers]
    (provider/register-global! provider-entry)))

(defn- dispatch-persistance!
  "Forward each `:ext/persistance` entry to the persistence registry."
  [entries]
  (doseq [{:persistance/keys [id ns]} entries]
    (persistance/register-backend! id ns)))

(def ^:private EXTENSIONS_PARENT ["extensions"])

(defn- mount-under-extensions
  "Auto-place an `:ext/cli` entry under the `vis extensions` parent.

   `:ext/cli` is reserved for commands the extension contributes to
   `vis extensions <cmd>` -- top-level built-ins like `vis run` are
   the binary's, not an extension's, and use `cmd/register-global!`
   directly. So every entry here gets `:cmd/parent` defaulted to
   `[\"extensions\"]`. Authors who want nested placement (e.g.
   `vis extensions git status`) can pass `:cmd/parent
   [\"extensions\" \"git\"]` and the dispatcher respects it AS LONG
   AS the first element is `\"extensions\"`. Any other parent is
   rejected -- `:ext/cli` is the extensions slot; mount somewhere
   else through `cmd/register-global!` direct."
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
               " Use cmd/register-global! directly for arbitrary placement.")
             {:type :ext/cli-bad-parent
              :entry entry}))))

(defn register-global!
  "Register an extension in the global process-level registry.

   This is THE single entry point for everything an extension
   contributes to vis. Whatever the extension declares -- SCI sandbox
   symbols (`:ext/symbols`), CLI commands (`:ext/cli`), channels
   (`:ext/channels`), LLM providers (`:ext/providers`), persistence
   backends (`:ext/persistance`) -- gets routed here and dispatched into
   the matching sub-registry as a side effect.

   Call this at namespace load time so the extension is available
   to every environment created afterwards:

     (ns my.company.ext.git
       (:require [com.blockether.vis-extension.extension :as ext]))

     (ext/register-global!
       (ext/extension
         {:ext/namespace 'com.acme.ext.git
          :ext/doc       \"Git integration.\"
          :ext/symbols   [git-status-sym git-blame-sym]
          :ext/cli       [{:cmd/name \"git-status\"
                           :cmd/parent [\"extensions\"]
                           :cmd/run-fn #'cli-git-status}]}))

   Idempotent on `:ext/namespace` -- re-registering replaces the
   previous version of the extension AND re-applies every sub-registry
   side effect (the inner registrars are themselves idempotent on
   their respective identity keys).

   Returns the validated extension."
  [ext]
  (let [ext    (extension ext)
        ns-sym (:ext/namespace ext)]
    (swap! global-registry assoc ns-sym ext)
    (tel/log! {:level :info :id ::register-global
               :data {:ext ns-sym
                      :symbols     (count (:ext/symbols ext))
                      :cli         (count (:ext/cli ext))
                      :channels    (count (:ext/channels ext))
                      :providers   (count (:ext/providers ext))
                      :persistance (count (:ext/persistance ext))}
               :msg (str "Extension '" ns-sym "' registered globally")})
    ;; Side-effect: route each surface to its concrete registry.
    ;; The inner registrars validate their own input via specs and
    ;; throw on bad shape -- we trust that here.
    (doseq [c (:ext/cli ext)]      (cmd/register-global! (mount-under-extensions c)))
    (doseq [c (:ext/channels ext)] (channel/register-global! c))
    (dispatch-providers!   (:ext/providers ext))
    (dispatch-persistance! (:ext/persistance ext))
    ext))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
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

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
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
;; Unified extension auto-discovery + inline extension docs
;;
;; ONE classpath scan, ONE resource per jar
;; (`META-INF/vis-extension/vis.edn`), ONE entry point. Every
;; extension surface in the system -- ext symbols, channels, commands,
;; providers, persistance entries -- routes through this single fn.
;; "Extension" here is the SUPERSET term: a channel is a kind of
;; extension, a CLI command is a kind of extension, a provider is a
;; kind of extension. The bespoke per-subsystem `discover-*!` fns are
;; gone; this is the only discovery mechanism in the codebase.
;;
;; The loader is type-agnostic: it just `require`s every namespace
;; listed under `:nses` in every `META-INF/vis-extension/vis.edn` it
;; finds on the classpath; whichever of those namespaces calls
;; `(ext/register-global! ...)`, `(channel/register-global! ...)`,
;; `(cmd/register-global! ...)`, `(provider/register-global! ...)`, or
;; `(persistance/register-backend! ...)` ends up in the matching
;; subsystem registry as a side effect.
;;
;; Resource shape (a single EDN map keyed by extension id):
;;
;;     ;; resources/META-INF/vis-extension/vis.edn
;;     {git
;;      {:nses [com.acme.ext.git.core           ; ext/register-global!
;;              com.acme.channel.web.bot        ; channel/register-global!
;;              com.acme.commands.git           ; cmd/register-global!
;;              com.acme.providers.openai       ; provider/register-global!
;;              com.acme.persistance.postgres]  ; persistance/register-backend!
;;       :docs {\"README.md\" {:created-at #inst \"2026-04-28\"
;;                              :abstract   \"...\"
;;                              :content    \"# Git\n...\"
;;                              :links      [{:to-id meta :to-doc \"README.md\"
;;                                            :context \"...\"}
;;                                           {:url \"https://...\" :context \"...\"}
;;                                           {:file \"packages/.../foo.clj\"
;;                                            :context \"...\"}]}
;;              \"EXAMPLES.md\" {:created-at ... :abstract ... :content ...}}}}
;;
;; The id (top-level key, here `git`) is the LLM-facing token \u2014 same as
;; `:ext/ns-alias :alias` on the registered extension. `:nses` is the
;; vector of namespaces to require. `:docs` is a map
;; `{<doc-name> <descriptor>}` where each descriptor is a map with:
;;
;;   :created-at \u2014 #inst, when the doc was first authored.
;;   :abstract   \u2014 one-paragraph LLM-facing summary (mandatory).
;;   :content    \u2014 full Markdown body (mandatory).
;;   :links      \u2014 author-declared outgoing links. Each link is a map:
;;                  - cross-ext doc:  {:to-id <id> :to-doc <name> :context ...}
;;                  - same-ext doc:   {:to-doc <name> :context ...}
;;                  - external URL:   {:url <url> :context ...}
;;                  - repo file:      {:file <path> :context ...}
;;
;; The loader derives `:reflinks` automatically by inverting every
;; cross-ext / same-ext outgoing link into a `{:from-id :from-doc
;; :context}` entry on the target descriptor. Authors never write
;; reflinks by hand.
;;
;; Why inline + structured: one resource per jar means one classpath
;; read at boot, no nested doc tree, no path-vs-call ambiguity
;; (`vis-extension/<id>` reads cleanly whether `<id>` is `meta`,
;; `vis`, or anything else), and `(meta/extension-doc id name)` is a
;; registry lookup with zero I/O. The structured shape lets the LLM
;; scan abstracts and follow link graphs as plain Clojure data \u2014 no
;; YAML frontmatter parsing, no Markdown link extraction.
;;
;; Why this lives in `com.blockether.vis-extension.extension`: the extension
;; facade is the canonical home for everything extension-shaped. Other
;; registries stay free of bespoke scanners; namespaces that need a
;; lazy safety-net discovery can resolve THIS fn.
;; =============================================================================

(def ^:private EXTENSIONS_RESOURCE "META-INF/vis-extension/vis.edn")

(defonce ^:private extension-docs-registry
  ;; {<id-symbol>
  ;;  {:nses [<ns-symbol> ...]
  ;;   :docs {<doc-name>
  ;;          {:created-at <inst>
  ;;           :abstract   <string>
  ;;           :content    <string>
  ;;           :links      [<link> ...]
  ;;           :reflinks   [<reflink> ...]}}}}
  ;;
  ;; Populated by `discover-extensions!` from every
  ;; `META-INF/vis-extension/vis.edn` on the classpath. Multiple jars
  ;; that declare the same id are merged: `:nses` are deduped
  ;; (preserving order of first occurrence), `:docs` are merged with
  ;; later entries winning per doc name. `:reflinks` are recomputed
  ;; from the union of all `:links` on every merge.
  (atom {}))

(defn- valid-link? [link]
  (and (map? link)
    (or (and (symbol? (:to-id link)) (string? (:to-doc link)))
      (string? (:to-doc link))
      (string? (:url link))
      (string? (:file link)))))

(defn- normalize-doc-descriptor
  "Validate one `[doc-name descriptor]` pair from a vis.edn `:docs`
   map. Returns the descriptor with empty defaults filled in, or
   `nil` when the entry is malformed (missing :abstract, missing
   :content, etc.). Logs the rejection reason at `:warn`."
  [doc-name descriptor]
  (cond
    (not (string? doc-name))
    (do (tel/log! {:level :warn :id ::doc-bad-name
                   :data {:doc-name doc-name}
                   :msg  (str "Doc name must be a string, got " (pr-str doc-name))})
      nil)

    (not (map? descriptor))
    (do (tel/log! {:level :warn :id ::doc-bad-shape
                   :data {:doc-name doc-name :type (some-> descriptor class .getName)}
                   :msg  (str "Doc descriptor must be a map: " doc-name)})
      nil)

    (not (string? (:abstract descriptor)))
    (do (tel/log! {:level :warn :id ::doc-missing-abstract
                   :data {:doc-name doc-name}
                   :msg  (str "Doc " doc-name " missing required :abstract string")})
      nil)

    (not (string? (:content descriptor)))
    (do (tel/log! {:level :warn :id ::doc-missing-content
                   :data {:doc-name doc-name}
                   :msg  (str "Doc " doc-name " missing required :content string")})
      nil)

    :else
    {:created-at (:created-at descriptor)
     :abstract   (:abstract descriptor)
     :content    (:content descriptor)
     :links      (vec (filter valid-link? (:links descriptor)))
     :reflinks   []}))

(defn- normalize-vis-edn
  "Coerce a parsed `vis.edn` payload into the canonical map shape
   `{<id-sym> {:nses [<ns-sym> ...] :docs {<doc-name> <descriptor>}}}`.
   Drops malformed entries silently; returns `{}` for unrecognized
   shapes. The legacy flat-vector, ns-keyed, and string-doc-body
   forms are NOT supported \u2014 the migration to the structured doc
   descriptor is a hard switch."
  [parsed]
  (when (map? parsed)
    (into {}
      (keep (fn [[id entry]]
              (when (and (symbol? id) (map? entry))
                (let [nses (vec (filter symbol? (:nses entry)))
                      docs (when (map? (:docs entry))
                             (into {}
                               (keep (fn [[doc-name descriptor]]
                                       (when-let [norm (normalize-doc-descriptor doc-name descriptor)]
                                         [doc-name norm])))
                               (:docs entry)))]
                  (when (seq nses)
                    [id {:nses nses :docs (or docs {})}])))))
      parsed)))

(defn registered-extension-ids
  "Sorted vector of every extension id known to the docs registry.
   Each id is the top-level key from a `vis.edn` and the same token
   the LLM uses as the SCI sandbox alias."
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
   `{:name :created-at :abstract :content :links :reflinks}`. Returns
   `nil` when the id is unknown or no doc by that name was declared."
  [id doc-name]
  (when-let [descriptor (and id doc-name
                          (get-in @extension-docs-registry [id :docs doc-name]))]
    (assoc descriptor :name doc-name)))

(defn extension-doc-content
  "Plain `:content` body (Markdown string) of a declared doc, or `nil`
   when the doc is unknown. Convenience over `(:content (extension-doc
   id name))`."
  [id doc-name]
  (:content (extension-doc id doc-name)))

(defn extension-doc-abstract
  "Return the `:abstract` field of a declared extension doc, or `nil`
   when the doc is unknown."
  [id doc-name]
  (:abstract (extension-doc id doc-name)))

(defn extension-doc-summary
  "Lightweight doc descriptor (no `:content`):
   `{:name :created-at :abstract :links :reflinks}`. Returns `nil`
   when the doc is unknown. Use this for catalog listings; pull the
   full body with `extension-doc-content` only when needed."
  [id doc-name]
  (when-let [descriptor (and id doc-name
                          (get-in @extension-docs-registry [id :docs doc-name]))]
    (-> descriptor
      (dissoc :content)
      (assoc :name doc-name))))

(defn extension-docs
  "With one arg, return a vector of doc summaries
   `[{:name :created-at :abstract :links :reflinks} ...]` for every
   doc declared by `id`. With no arg, return the full registry as
   `{<id-sym> [<summary> ...]}`. Sorted by doc name within each id so
   the catalog is deterministic."
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
  "Merge one `[id {:nses [...] :docs {name <descriptor>}}]` into the
   registry. `:nses` are deduped (existing order preserved); `:docs`
   is a map merge with later entries winning per name. Reflinks are
   recomputed by `recompute-reflinks!` after every merge so a later
   jar's links can target an earlier jar's docs."
  [id entry]
  (swap! extension-docs-registry
    update id
    (fn [existing]
      (let [merged-nses (vec (distinct (concat (:nses existing) (:nses entry))))
            merged-docs (merge (or (:docs existing) {}) (or (:docs entry) {}))]
        {:nses merged-nses :docs merged-docs}))))

(defn- link-target
  "Return `[<target-id> <target-doc>]` for a cross-ext or same-ext
   doc link, or `nil` for url/file/external links. `from-id` is the
   id of the extension that authored the link \u2014 used to resolve
   same-ext refs that omit `:to-id`."
  [from-id link]
  (cond
    (and (symbol? (:to-id link)) (string? (:to-doc link)))
    [(:to-id link) (:to-doc link)]

    (and (nil? (:to-id link)) (string? (:to-doc link)))
    [from-id (:to-doc link)]

    :else nil))

(defn- recompute-reflinks!
  "Walk every doc's `:links` across the entire registry and rebuild
   the `:reflinks` vector on each target. Idempotent: produces the
   same registry shape regardless of merge order."
  []
  (swap! extension-docs-registry
    (fn [registry]
      (let [;; Strip any prior :reflinks so this pass starts clean.
            cleared (reduce-kv
                      (fn [acc id entry]
                        (assoc acc id
                          (update entry :docs
                            (fn [docs]
                              (reduce-kv (fn [d name descriptor]
                                           (assoc d name (assoc descriptor :reflinks [])))
                                {} docs)))))
                      {} registry)
            ;; Walk every authored link; route a {:from-id :from-doc
            ;; :context} reflink onto the target descriptor.
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
   extension. Useful for snapshot tests and ad-hoc inspection."
  []
  (reduce-kv
    (fn [acc id entry]
      (assoc acc id
        {:nses (:nses entry)
         :docs (reduce-kv (fn [d name _] (assoc d name (extension-doc-summary id name)))
                 {} (:docs entry))}))
    {} @extension-docs-registry))

(defn discover-extensions!
  "Scan the classpath for every `META-INF/vis-extension/vis.edn`
   resource. Each resource is an EDN map keyed by extension id, where
   each value declares `:nses` (vector of namespaces to `require`)
   and `:docs` (map of doc name to inline doc body). Discovery merges
   every entry into the docs registry, then `require`s each declared
   namespace exactly once across all resources.

   The loaded namespaces self-register into whichever subsystem
   registry they target (extension symbols, channels, commands,
   providers, persistance entries, ...). `Extension` here is the
   umbrella term: every extension surface in vis -- channels,
   commands, providers, backends -- is treated as a kind of extension
   and discovered through this one fn.

   Returns the count of namespaces this call actually loaded (deduped
   across resource files within a single invocation). Idempotent
   through Clojure's `require` cache -- calling it twice does no extra
   work.

   Failures (broken EDN, missing namespace, throwing
   `register-global!`) are logged at `:error` per offending entry and
   the scan continues. A bad extension cannot abort discovery for its
   siblings."
  []
  (let [urls   (try
                 (enumeration-seq
                   (.getResources
                     (.getContextClassLoader (Thread/currentThread))
                     EXTENSIONS_RESOURCE))
                 (catch Exception _ nil))
        loaded (atom 0)
        seen   (atom #{})]
    (doseq [^java.net.URL url urls]
      (try
        (let [content   (slurp url)
              parsed    (edn/read-string {:readers {} :default (fn [_ form] form)} content)
              normalized (normalize-vis-edn parsed)]
          (doseq [[id entry] normalized]
            (merge-manifest-entry! id entry)
            (doseq [ns-sym (:nses entry)]
              (when (not (@seen ns-sym))
                (swap! seen conj ns-sym)
                (try
                  (require ns-sym)
                  (swap! loaded inc)
                  (tel/log! {:level :info :id ::discover-extension
                             :data  {:extension-id id
                                     :extension-ns ns-sym
                                     :source (str url)}
                             :msg   (str "Auto-discovered extension ns '"
                                      ns-sym "' (id " id ") from " url)})
                  (catch Throwable t
                    (tel/log! {:level :error :id ::discover-extension-failed
                               :data  {:extension-id id
                                       :extension-ns ns-sym
                                       :source (str url)
                                       :class (.getName (class t))
                                       :message (ex-message t)}
                               :msg   (str "Failed to load extension ns '"
                                        ns-sym "': " (ex-message t))}))))))
          (when (empty? normalized)
            (tel/log! {:level :warn :id ::discover-extension-empty
                       :data {:source (str url)}
                       :msg  (str url " parsed but declared no extensions")})))
        (catch Throwable t
          (tel/log! {:level :error :id ::discover-extension-parse-failed
                     :data  {:source (str url) :message (ex-message t)}
                     :msg   (str "Failed to parse " url ": " (ex-message t))}))))
    ;; All authored links are now in the registry; invert them into
    ;; reflinks so each target descriptor carries its inbound graph.
    (recompute-reflinks!)
    @loaded))

;; =============================================================================
;; CLI bridge -- the `vis extensions` parent
;;
;; Self-registers a top-level `extensions` command into commandline.base
;; whose subcommands are computed lazily from every command registered
;; with `:cmd/parent ["extensions"]`. Extensions populate this slot
;; through `:ext/cli` on `ext/extension` -- the `register-global!`
;; dispatcher above forwards each entry to `cmd/register-global!`.
;;
;; Lives here (not in commandline.base) because this facade is the
;; canonical home for everything extension-shaped. The `cmd` alias is
;; established at the top-of-ns require form.
;; =============================================================================

(cmd/register-global!
  {:cmd/name        "extensions"
   :cmd/doc         "Run an extension-provided CLI command."
   :cmd/usage       "vis extensions <cmd> [args…]"
   :cmd/subcommands #(cmd/registered-under ["extensions"])})
