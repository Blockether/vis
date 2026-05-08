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
     - the `(v/extensions)` / `(v/extension-doc id name)`
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
   [com.blockether.vis.internal.theme :as theme]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
   [taoensso.telemere :as tel])
  (:import
   (java.io ByteArrayOutputStream InputStream)
   (java.net URL)
   (java.security MessageDigest)
   (java.util.jar JarEntry JarFile)))

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

;; =============================================================================
;; Tool-result contract
;; =============================================================================

(def ^:private max-trace-frames 12)

(defn- now-ms [] (System/currentTimeMillis))

(s/def ::success? boolean?)
(s/def ::result any?)
(s/def ::rendered string?)

(s/def ::op keyword?)
(s/def ::started-at-ms integer?)
(s/def ::finished-at-ms integer?)
(s/def ::duration-ms nat-int?)

(s/def ::sym symbol?)
(s/def ::alias symbol?)
(s/def ::call (s/and string? #(not (str/blank? %))))
(s/def ::tool (s/keys :req-un [::sym ::call]
                :opt-un [::alias]))

(s/def ::namespace symbol?)
(s/def ::registry-id symbol?)
(s/def ::kind (s/and string? #(not (str/blank? %))))
(s/def ::doc (s/and string? #(not (str/blank? %))))
(s/def ::version (s/and string? #(not (str/blank? %))))
(s/def ::author (s/and string? #(not (str/blank? %))))
(s/def ::owner (s/and string? #(not (str/blank? %))))
(s/def ::license (s/and string? #(not (str/blank? %))))
(s/def ::tool-result-extension
  (s/keys :req-un [::namespace]
    :opt-un [::registry-id ::kind ::doc ::version
             ::author ::owner ::license]))

(s/def ::paths (s/coll-of string? :kind vector?))
(s/def ::mtime-max integer?)
(s/def ::hash-sha256 (s/nilable (s/and string? #(= 64 (count %)))))
(s/def ::source (s/keys :req-un [::paths ::mtime-max ::hash-sha256]))

(s/def ::provenance
  (s/and
    (s/keys :req-un [::op ::started-at-ms ::finished-at-ms ::duration-ms]
      :opt-un [::tool ::source])
    #(or (not (contains? % :extension))
       (s/valid? ::tool-result-extension (:extension %)))))

(s/def ::type (s/and string? #(not (str/blank? %))))
(s/def ::message string?)
(s/def ::class string?)
(s/def ::method string?)
(s/def ::file string?)
(s/def ::line pos-int?)
(s/def ::origin #{:user-code :tool :runtime :library})
(s/def ::trace-frame (s/keys :req-un [::class ::method ::file ::origin]
                       :opt-un [::line]))
(s/def ::trace (s/coll-of ::trace-frame :kind vector?))
(s/def ::error-map (s/keys :req-un [::type ::message ::trace]))
(s/def ::error (s/nilable ::error-map))

(s/def ::tool-result-base
  (s/keys :req-un [::success? ::result ::provenance ::error]))

(s/def ::tool-result
  (s/and
    ::tool-result-base
    (fn [{:keys [success? error]}]
      (if success?
        (nil? error)
        (some? error)))))

(def ^:dynamic *preview-sink*
  "Optional per-eval atom used by v/preview to surface every preview call
   from a block, even when the final form result is not the preview value."
  nil)

(defn record-preview!
  [preview-result]
  (when *preview-sink*
    (swap! *preview-sink* conj preview-result))
  preview-result)

(def ^:dynamic *tool-event-sink*
  "Optional per-eval sink for observable tool lifecycle events. Bound by
   tests and UI/progress adapters that need to know a tool started before
   its fn returns. The sink receives plain event maps."
  nil)

(defn- record-tool-event!
  [event]
  (when *tool-event-sink*
    (*tool-event-sink* event))
  event)

(defn tool-result?
  [x]
  (s/valid? ::tool-result x))

(defn assert-tool-result!
  [x]
  (when-not (tool-result? x)
    (throw (ex-info "Invalid tool result"
             {:type :vis/invalid-tool-result
              :value x
              :explain (s/explain-data ::tool-result x)})))
  x)

(defn normalize-provenance
  "Fill the common provenance clock keys when absent.

   Required by the tool-result contract:
     :op
     :started-at-ms
     :finished-at-ms
     :duration-ms

   Callers may pass richer maps (tool / extension / source metadata);
   this helper only normalizes the shared timing surface."
  [provenance]
  (let [provenance (or provenance {})
        t          (now-ms)
        started    (long (or (:started-at-ms provenance) t))
        finished   (long (or (:finished-at-ms provenance) t))
        duration   (long (or (:duration-ms provenance)
                           (max 0 (- finished started))))]
    (assoc provenance
      :started-at-ms started
      :finished-at-ms finished
      :duration-ms duration)))

(defn merge-provenance
  "Merge `extra` into an already-valid tool-result envelope, re-check
   the contract, and preserve metadata. Used by the extension wrapper
   to stamp extension/source provenance onto tool-like returns."
  [tool-result extra]
  (let [meta*  (meta tool-result)
        merged (-> tool-result
                 (update :provenance #(merge (or % {}) extra))
                 assert-tool-result!)]
    (with-meta merged meta*)))

(defn- frame-origin
  [^StackTraceElement frame]
  (let [class-name (.getClassName frame)
        file-name  (.getFileName frame)
        method     (.getMethodName frame)]
    (cond
      (or (= class-name "user")
        (= class-name "sandbox")
        (= file-name "iteration")
        (= method "anonymous-fn"))
      :user-code

      (str/starts-with? class-name "com.blockether.vis.ext.")
      :tool

      (str/starts-with? class-name "com.blockether.vis.internal.")
      :runtime

      :else
      :library)))

(defn- noisy-frame?
  [^StackTraceElement frame]
  (let [class-name (.getClassName frame)]
    (or (str/starts-with? class-name "sci.impl.")
      (str/starts-with? class-name "sci.ctx_store")
      (str/starts-with? class-name "clojure.lang.AFn")
      (str/starts-with? class-name "clojure.lang.RestFn")
      (str/starts-with? class-name "clojure.lang.MultiFn")
      (str/starts-with? class-name "clojure.lang.Var")
      (str/starts-with? class-name "java.lang.reflect.")
      (str/starts-with? class-name "jdk.internal.reflect."))))

(defn normalize-trace
  [^Throwable t]
  (->> (.getStackTrace t)
    (remove noisy-frame?)
    (map (fn [^StackTraceElement frame]
           (cond-> {:class  (.getClassName frame)
                    :method (.getMethodName frame)
                    :file   (or (.getFileName frame) "unknown")
                    :origin (frame-origin frame)}
             (pos? (.getLineNumber frame))
             (assoc :line (.getLineNumber frame)))))
    (take max-trace-frames)
    vec))

(defn normalize-error
  [^Throwable t]
  {:type    (.getName (class t))
   :message (or (ex-message t) "")
   :trace   (normalize-trace t)})

(defn success
  [{:keys [result provenance]}]
  (-> {:success?   true
       :result     result
       :provenance (normalize-provenance provenance)
       :error      nil}
    assert-tool-result!))

(defn failure
  [{:keys [result provenance error throwable]}]
  (let [err (or error
              (when throwable (normalize-error throwable)))]
    (-> {:success?   false
         :result     result
         :provenance (normalize-provenance provenance)
         :error      err}
      assert-tool-result!)))

;; =============================================================================
;; Symbol entry spec
;; =============================================================================

;; Symbol name bound in the SCI sandbox.
(s/def :ext.symbol/sym symbol?)

;; Implementation function the LLM calls from :code blocks.
(s/def :ext.symbol/fn fn?)

;; One-liner description shown in the sandbox var's docstring.
(s/def :ext.symbol/doc non-blank-string?)

;; Argument signatures, e.g. '([term] [term opts]).
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

;; Optional parse-error rescue: (fn [{:keys [code error sym environment]}] → string|nil).
;; Runs BEFORE dispatch when SCI/edamame cannot parse the source.
(s/def :ext.symbol/on-parse-error-fn fn?)

;; Optional parsed-source normalizer: (fn [{:keys [code sym environment]}] → string|nil).
;; Runs before SCI eval when source already parses. Use for symbol-local sugar / repair.
(s/def :ext.symbol/source-rewrite-fn fn?)

;; Optional result contract enforced on the final public return value
;; after `:before-fn` / call / `:after-fn` complete.
(s/def :ext.symbol/result-spec some?)

;; Renderer for this symbol's result. Called by runtime consumers
;; (journal, transcript, TUI) with a context map, e.g.
;; `{:surface :journal :tool-result result}`. Every fn-symbol MUST
;; provide one — either explicitly via `:render-fn` or implicitly via
;; the `vis/symbol` builder attaching `render-value`.
(s/def :ext.symbol/render-fn fn?)

;; Extension-owned renderers for semantic rendering kinds carried by
;; data/preview metadata. Each fn receives a context map with at least
;; `:surface`, `:rendering-kind`, and `:value`; callers may include
;; `:tool-result` or other surface context. Return markdown/plain text.
(s/def :ext/rendering-kinds (s/map-of keyword? fn?))

;; Extension-owned renderers for Markdown fenced code blocks. Channels call
;; `render-fenced-block` from their Markdown projection path; the first
;; renderer whose `:renderer/langs` contains the normalized fence language
;; and returns a non-nil result wins.
(s/def :renderer/id keyword?)
(s/def :renderer/langs
  (s/and set? seq #(every? non-blank-string? %)))
(s/def :renderer/render-fn ifn?)
(s/def ::fenced-renderer
  (s/keys :req [:renderer/id :renderer/langs :renderer/render-fn]))
(s/def :ext/fenced-renderers
  (s/coll-of ::fenced-renderer :kind vector?))

;; Plain value bound in the sandbox (constant, data, config).
;; Mutually exclusive with :ext.symbol/fn.
(s/def :ext.symbol/val some?)

(s/def ::fn-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/fn :ext.symbol/doc
                :ext.symbol/arglists :ext.symbol/examples
                :ext.symbol/render-fn]
    :opt [:ext.symbol/before-fn :ext.symbol/after-fn
          :ext.symbol/on-error-fn :ext.symbol/on-parse-error-fn
          :ext.symbol/source-rewrite-fn :ext.symbol/result-spec]))

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

;; Top-level kind — the *category* of surface this extension
;; contributes. Used for prompt-rendering section labels AND as the
;; section heading in `vis extensions list`. Examples: "foundation",
;; "languages", "providers", "channels", "persistance". Authors may
;; set it explicitly; for the common categorical cases (extensions
;; that only contribute providers / channels / persistence backends)
;; the `extension` builder auto-derives it.
(s/def :ext/kind non-blank-string?)

;; Guard evaluated at each turn boundary. (fn [env] -> bool).
;; Default: (constantly true).
(s/def :ext/activation-fn fn?)

;; Optional extra LLM-facing documentation appended when the extension is active.
(s/def :ext/prompt fn?)

;; Optional system-prompt environment-info contributor. Called once at
;; system-prompt assembly with the live environment. Any active extension
;; can add repo/runtime/project facts here without taking over the whole
;; `:ext/prompt` fragment.
(s/def :ext/environment-info-fn fn?)

;; Optional per-iteration nudge composer.
;; Return value is checked at runtime because specs cannot validate a fn's
;; output from the extension map alone. Valid returns:
;;   nil
;;   "nudge text"
;;   {:importance :low|:normal|:high|:critical :text "nudge text"}
;; `:message` or `:body` are accepted aliases for `:text`.
(def system-nudge-importances #{:low :normal :high :critical})

(defn- system-nudge-map?
  [x]
  (and (map? x)
    (every? #{:importance :text} (keys x))
    (or (nil? (:importance x))
      (contains? system-nudge-importances (:importance x)))
    (non-blank-string? (:text x))))

(s/def ::system-nudge-result
  (s/nilable
    (s/or :text non-blank-string?
      :map system-nudge-map?)))

(defn system-nudge-result?
  "True when an extension :ext/nudge-fn return value conforms to the
   supported nudge contract. Used at runtime after calling the nudge fn."
  [x]
  (s/valid? ::system-nudge-result x))

(defn explain-system-nudge-result
  [x]
  (s/explain-data ::system-nudge-result x))

(s/def :ext/nudge-fn fn?)

;; Optional source-code rewriter for SCI/edamame parse errors.
(s/def :ext/on-parse-error-fn fn?)

;; Optional pre-eval source normalizer. Runs after source parses but before SCI
;; eval, so extensions can repair source-shape footguns that are valid Clojure
;; syntax but would fail during evaluation (for example unquoted prose in
;; markdown helper calls).
(s/def :ext/source-rewrite-fn fn?)

;; ----------------------------------------------------------------------------
;; Iteration-loop lifecycle hooks. Side-effecting fns invoked by the
;; loop at well-defined boundaries. See
;; `internal.lifecycle/phase->manifest-key` for the canonical
;; phase↔key map and `docs/src/extensions/lifecycle.md` for payload
;; shapes. ALL four are optional; an extension that doesn't care
;; about lifecycle simply omits them.
;;
;; Composition is broadcast: every active extension's listener fires
;; for every event, plus the per-call `:hooks` slot a channel/test
;; passed in. A listener that throws is caught + logged via Telemere
;; — it MUST NOT take the loop down or starve sibling listeners.
;; ----------------------------------------------------------------------------
(s/def :ext/on-turn-start-fn      fn?)
(s/def :ext/on-iteration-start-fn fn?)
(s/def :ext/on-iteration-end-fn   fn?)
(s/def :ext/on-turn-end-fn        fn?)

(def proof-event-kinds
  "Structured proof lifecycle events emitted by persistence writes and audit.
   Extensions receive these through `:ext/on-proof-event-fn`; they must observe
   data, not scrape audit/proof Markdown."
  #{:proof/event-appended
    :proof/evidence-bundle-created
    :proof/attestation-accepted
    :proof/audit-violation})

(s/def :proof/event proof-event-kinds)
(s/def :ext/on-proof-event-fn fn?)

;; Channel-local hooks let extensions contribute UI commands/status behavior to
;; concrete channels without requiring those channel namespaces. The TUI uses
;; this for voice commands; other channels may ignore the surface.
(s/def :ext.channel-hook/channel-id keyword?)
(s/def :ext.channel-hook/hook-id keyword?)
(s/def :ext.channel-hook/commands-fn ifn?) ;; (fn [ctx]) -> seq<{:id :label :run-fn}>
(s/def ::channel-hook
  (s/keys :req-un [:ext.channel-hook/channel-id :ext.channel-hook/hook-id]
    :opt-un [:ext.channel-hook/commands-fn]))
(s/def :ext/channel-hooks (s/coll-of ::channel-hook :kind vector?))

;; Optional extension-owned environment/config declarations. These name
;; OS-style environment variables that can also be overridden from
;; Vis config/TUI under `:environment`. The host never mutates the
;; process env; extension code resolves them through config helpers.
(s/def :ext.env/name non-blank-string?)
(s/def :ext.env/label non-blank-string?)
(s/def :ext.env/description string?)
(s/def :ext.env/secret? boolean?)
(s/def :ext.env/required? boolean?)
(s/def ::env-entry
  (s/keys :req-un [:ext.env/name]
    :opt-un [:ext.env/label :ext.env/description :ext.env/secret? :ext.env/required?]))
(s/def :ext/env (s/coll-of ::env-entry :kind vector?))

;; Optional extension-owned TUI setting declarations. The TUI stores the
;; values, but the extension owns the row metadata so extension-specific
;; knobs appear under Extensions → <extension> instead of hardcoded host
;; buckets.
(s/def :ext.setting/key (s/or :keyword keyword? :string non-blank-string?))
(s/def :ext.setting/type #{:toggle :choice :action})
(s/def :ext.setting/label non-blank-string?)
(s/def :ext.setting/description string?)
(s/def :ext.setting/choices (s/coll-of keyword? :kind vector? :min-count 1))
(s/def ::setting-entry
  (s/keys :req-un [:ext.setting/key :ext.setting/type :ext.setting/label]
    :opt-un [:ext.setting/description :ext.setting/choices]))
(s/def :ext/settings (s/coll-of ::setting-entry :kind vector?))

;; Optional extension-owned theme declarations. Plain EDN shape:
;;   {:ext/theme {"THEME_NAME" {"PADDING" "0px"}}}
;; The internal `com.blockether.vis.internal.theme` namespace owns the reusable theme
;; token spec and built-in palettes; extensions can add channel-agnostic
;; string-key settings here for channels to adapt.
(s/def :ext/theme theme/extension-theme-map?)

;; Optional dependency declaration. Vector of extension namespace symbols.
(s/def :ext/requires (s/coll-of symbol? :kind vector?))

;; Semver version string, e.g. "1.0.0", "0.3.1-SNAPSHOT".
(s/def :ext/version non-blank-string?)

;; Author name or org — the entity that *created* the extension
;; (e.g. "Blockether", "Acme Corp.").
(s/def :ext/author non-blank-string?)

;; Owner of the *package* — the project / distribution that ships
;; this extension. For everything bundled in this repo: "vis".
;; Third-party packages set their own owner (often the same as
;; `:ext/author`, but they're independent: a Blockether-authored
;; extension can be vendored by a downstream distribution).
(s/def :ext/owner non-blank-string?)

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
      (or-nil-or-fn :provider/get-token-fn)
      (or-nil-or-fn :provider/limits-fn)
      (or-nil-or-fn :provider/on-selected-fn)
      (or-nil-or-fn :provider/prompt-fn))))
(s/def :ext/providers (s/coll-of ::provider-entry :kind vector?))

;; Persistence backends exported by this extension.
(s/def :persistance/id keyword?)
(s/def :persistance/ns
  (s/and symbol?
    #(nil? (namespace %))
    #(re-find #"\." (name %))))
(s/def :ext/persistance-entry (s/keys :req [:persistance/id :persistance/ns]))
(s/def :ext/persistance (s/coll-of :ext/persistance-entry :kind vector?))

;; Doctor contribution from this extension: ONE function the `vis doctor`
;; aggregator calls with the live environment and that returns a seq of
;; diagnostic message maps. Replaces the previous `:ext/doctor-checks`
;; vec-of-`{:check/id :check/name :check/description :check/run-fn}` shape —
;; the metadata fields (`:check/name`, `:check/description`) were never
;; surfaced anywhere; only `:check/id` made it onto messages, and the
;; extension can stamp `:check-id` on its own messages just as easily
;; without the host walking a vec of structured maps. Plan §1 Q19 + §10.
;; Authors who don't ship checks just omit the field.
;;
;; Naming follows the `:ext/<surface>-fn` convention already used for
;; `:ext/activation-fn`, `:ext/nudge-fn`, `:ext/on-parse-error-fn` — ONE fn,
;; called by the host, returns data.
;;
;; Per-message expectations (host coerces missing/invalid):
;;   {:level :info|:warn|:error
;;    :message "…"            ; required, non-blank
;;    :remediation "…"        ; optional; renders as `→ …` indented line
;;    :check-id ::keyword     ; optional; renders as the prefix
;;    :data {…}}              ; optional; passthrough for callers
(s/def :ext/doctor-check-fn fn?)

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

;; Canonical source markers attached to registered extensions via the
;; sidecar atom. Also surfaced in TURN_ACTIVE_EXTENSIONS / v/extensions
;; and stamped onto tool-result provenance.
(s/def ::alias symbol?)
(s/def ::namespace symbol?)
(s/def ::doc non-blank-string?)
(s/def ::kind non-blank-string?)
(s/def ::version non-blank-string?)
(s/def ::author non-blank-string?)
(s/def ::owner non-blank-string?)
(s/def ::license non-blank-string?)
(s/def ::source-paths (s/coll-of string? :kind vector?))
(s/def ::source-mtime-max integer?)
(s/def ::source-hash-sha256
  (s/nilable (s/and string? #(= 64 (count %)))))
(s/def ::registry-id symbol?)

(s/def ::source-markers
  (s/keys :req-un [::source-paths ::source-mtime-max ::source-hash-sha256]))

(s/def ::extension-provenance
  (s/keys :req-un [::namespace ::source-paths ::source-mtime-max ::source-hash-sha256]
    :opt-un [::alias ::doc ::kind ::version ::author ::owner ::license ::registry-id]))

(defn- ns-alias-required-when-symbols?
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/ns-alias ext))))

(defn- kind-required-when-symbols?
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/kind ext))))

(s/def ::extension
  (s/and
    (s/keys :req [:ext/namespace :ext/doc]
      :opt [:ext/kind :ext/activation-fn
            :ext/symbols :ext/classes :ext/imports
            :ext/ns-alias :ext/prompt :ext/environment-info-fn :ext/nudge-fn
            :ext/on-parse-error-fn :ext/source-rewrite-fn :ext/rendering-kinds :ext/fenced-renderers
            :ext/env :ext/settings :ext/theme :ext/requires
            :ext/version :ext/author :ext/owner :ext/license
            :ext/cli :ext/channels :ext/providers :ext/persistance
            :ext/channel-hooks
            :ext/doctor-check-fn
            :ext/on-turn-start-fn :ext/on-iteration-start-fn
            :ext/on-iteration-end-fn :ext/on-turn-end-fn
            :ext/on-proof-event-fn])
    ns-alias-required-when-symbols?
    kind-required-when-symbols?))

;; =============================================================================
;; Symbol helpers (builder fns)
;; =============================================================================

(defn render-value
  "Default renderer for plain-value symbols. Returns string representation
   of the tool result. Attached automatically by `vis/symbol` when no
   explicit `:render-fn` is provided."
  [{:keys [tool-result]}]
  (if (map? tool-result)
    (if-let [result (:result tool-result)]
      (pr-str result)
      (pr-str tool-result))
    (if (string? tool-result)
      tool-result
      (pr-str tool-result))))

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
             :on-parse-error-fn, :source-rewrite-fn, :result-spec, :render-fn

   Defaults:
     :examples  — derived from :arglists when not provided
     :render-fn — `render-value` when not provided

   See `docs/src/extensions/hooks.md` for hook semantics."
  [sym-name f opts]
  (let [arglists (:arglists opts)
        arglists (when arglists (if (seq? arglists) (vec arglists) arglists))
        examples (or (:examples opts)
                   (when arglists (derive-examples sym-name arglists)))]
    (validate-symbol-entry!
      (cond-> #:ext.symbol{:sym      sym-name
                           :fn       f
                           :render-fn (or (:render-fn opts) render-value)}
        (:doc opts)               (assoc :ext.symbol/doc (:doc opts))
        arglists                  (assoc :ext.symbol/arglists arglists)
        examples                  (assoc :ext.symbol/examples (vec examples))
        (:before-fn opts)         (assoc :ext.symbol/before-fn (:before-fn opts))
        (:after-fn opts)          (assoc :ext.symbol/after-fn (:after-fn opts))
        (:on-error-fn opts)       (assoc :ext.symbol/on-error-fn (:on-error-fn opts))
        (:on-parse-error-fn opts) (assoc :ext.symbol/on-parse-error-fn (:on-parse-error-fn opts))
        (:source-rewrite-fn opts) (assoc :ext.symbol/source-rewrite-fn (:source-rewrite-fn opts))
        (:result-spec opts)       (assoc :ext.symbol/result-spec (:result-spec opts))
        (:render-fn opts)         (assoc :ext.symbol/render-fn (:render-fn opts))))))

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
   - :ext/ns-alias optional {:alias 'v}
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

(defn- validate-symbol-result!
  [sym spec-ref result]
  (when spec-ref
    (when-not (s/valid? spec-ref result)
      (throw (ex-info (str "Symbol '" sym "' returned a value that does not satisfy " spec-ref)
               {:type    :extension/invalid-symbol-result
                :symbol  sym
                :spec    spec-ref
                :value   result
                :explain (s/explain-data spec-ref result)}))))
  result)

(defn- tool-call-name
  [ext sym]
  (if-let [alias (get-in ext [:ext/ns-alias :alias])]
    (str alias "/" sym)
    (str sym)))

(defn- tool-start-event
  [ext sym-entry started-at-ms]
  (let [sym (:ext.symbol/sym sym-entry)]
    {:phase :tool-start
     :status :running
     :op (keyword (tool-call-name ext sym))
     :extension (:ext/namespace ext)
     :symbol sym
     :started-at-ms (long started-at-ms)}))

(declare extension-provenance)

(defn- enrich-tool-result-provenance
  [ext sym-entry result]
  (if (tool-result? result)
    (let [ext-prov (extension-provenance ext)]
      (merge-provenance
        result
        {:tool      (cond-> {:sym  (:ext.symbol/sym sym-entry)
                             :call (tool-call-name ext (:ext.symbol/sym sym-entry))}
                      (get-in ext [:ext/ns-alias :alias])
                      (assoc :alias (get-in ext [:ext/ns-alias :alias])))
         :extension (dissoc ext-prov :source-paths :source-mtime-max :source-hash-sha256)
         :source    {:paths       (:source-paths ext-prov)
                     :mtime-max   (:source-mtime-max ext-prov)
                     :hash-sha256 (:source-hash-sha256 ext-prov)}}))
    result))

(defn- maybe-record-preview-result!
  [result]
  (if (and (tool-result? result)
        (= :v/preview (get-in result [:provenance :op])))
    (record-preview! result)
    result))

(def ^:dynamic *current-extension*
  "Extension map currently executing on an extension callback thread.
   Bound by symbol wrappers so extension-owned helper APIs can fill the
   caller's stable extension identity without accepting user-supplied ids."
  nil)

(def ^:dynamic *current-symbol*
  "Sandbox symbol currently executing, when a symbol callback is active."
  nil)

(defn current-extension []
  *current-extension*)

(defn current-extension-id []
  (some-> *current-extension* :ext/namespace str))

(defn invoke-symbol-wrapper
  "Full invocation pipeline for a function symbol entry:
   before-fn → fn → after-fn, with on-error-fn catching :fn errors.

   Every hook can override :fn, :args, :env via its return map.
   :before-fn can return {:result val} to short-circuit.
   :on-error-fn can return {:result val}, {:error err}, or {:fn :args :env} to retry.

   When `:ext.symbol/result-spec` is present, the FINAL public return
   value is validated against it right before control returns to SCI.

   Returns the final result. Throws on any unrecoverable error."
  [ext sym-entry args env]
  (binding [*current-extension* ext
            *current-symbol* (:ext.symbol/sym sym-entry)]
    (let [sym    (:ext.symbol/sym sym-entry)
          ext-ns (:ext/namespace ext)
          spec-ref (:ext.symbol/result-spec sym-entry)
          t0     (System/nanoTime)
          _      (log-hook! :debug ::invoke ext-ns sym nil nil nil)
          before-out (run-before ext-ns sym-entry env (:ext.symbol/fn sym-entry) args)]
      (if (contains? before-out :result)
        (let [ms (elapsed-ms t0)
              result (->> (:result before-out)
                       (enrich-tool-result-provenance ext sym-entry)
                       (validate-symbol-result! sym spec-ref)
                       (maybe-record-preview-result!))]
          (log-hook! :debug ::invoke-done ext-ns sym nil ms "short-circuited")
          result)
        (let [{env  :env
               f    :fn
               args :args} before-out

              call-result
              (let [ct0 (System/nanoTime)
                    call-started-at-ms (now-ms)]
                (record-tool-event! (tool-start-event ext sym-entry call-started-at-ms))
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
              result (->> result
                       (enrich-tool-result-provenance ext sym-entry)
                       (validate-symbol-result! sym spec-ref)
                       (maybe-record-preview-result!))
              ms (elapsed-ms t0)]
          (log-hook! :debug ::invoke-done ext-ns sym nil ms nil)
          result)))))

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
                        (binding [*out* w
                                  *err* w
                                  workspace-context/*workspace-root* (workspace-context/workspace-root env)]
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

(defn- try-symbol-source-rewrite
  [extensions code environment]
  (loop [exts (seq extensions)]
    (when exts
      (let [ext   (first exts)
            alias (some-> (:ext/ns-alias ext) :alias clojure.core/name)
            hit
            (loop [syms (seq (:ext/symbols ext))]
              (when syms
                (let [entry (first syms)
                      sym   (:ext.symbol/sym entry)
                      hook  (:ext.symbol/source-rewrite-fn entry)]
                  (if (and hook sym (code-mentions-symbol? code (str sym) alias))
                    (let [out (run-parse-rescue-hook
                                (str (:ext/namespace ext) "/" sym "/source-rewrite")
                                hook
                                {:code        code
                                 :sym         sym
                                 :environment environment})]
                      (if (and (string? out) (not= out code))
                        out
                        (recur (next syms))))
                    (recur (next syms))))))]
        (or hit (recur (next exts)))))))

(defn- try-extension-source-rewrite
  [extensions code environment]
  (loop [exts (seq extensions)]
    (when exts
      (let [ext  (first exts)
            hook (:ext/source-rewrite-fn ext)
            out  (when hook
                   (run-parse-rescue-hook (str (:ext/namespace ext) "/source-rewrite")
                     hook
                     {:code code :environment environment}))]
        (if (and (string? out) (not= out code))
          out
          (recur (next exts)))))))

(defn try-rewrite-source
  "Walk active extensions and let source-rewrite hooks normalize parsed source
   before SCI eval. This is not parse-error rescue: callers use it for valid
   Clojure source that is likely to fail during eval because of extension-local
   surface syntax.

   Resolution order mirrors parse rescue:
     1. Per-symbol `:ext.symbol/source-rewrite-fn` whose symbol appears in code.
     2. Extension-level `:ext/source-rewrite-fn` fallback.

   Hooks are pure source→source; throw/non-string/unchanged results are skipped."
  [extensions code environment]
  (or (try-symbol-source-rewrite extensions code environment)
    (try-extension-source-rewrite extensions code environment)))

;; =============================================================================
;; Public API — extension builder
;; =============================================================================

(defn- derive-kind
  "Auto-derive `:ext/kind` for the categorical cases when the author
   didn't set one. Extensions that contribute providers, channels, or
   persistence backends (and nothing forcing a different label) get
   bucketed under `\"providers\"` / `\"channels\"` / `\"persistance\"`
   so `vis extensions list` reads as a clean grouped table instead
   of a column of blanks.

   Explicit `:ext/kind` always wins. Extensions that fit no
   categorical bucket (and don't set a kind themselves) stay
   blank — that's a legitimate \"uncategorized\" outcome."
  [spec]
  (cond
    (some? (:ext/kind spec))            (:ext/kind spec)
    (seq (:ext/providers spec))         "providers"
    (seq (:ext/channels spec))          "channels"
    (seq (:ext/persistance spec))       "persistance"
    :else                               nil))

(defn extension
  "Build and validate an extension. The canonical constructor.

   See docs/src/extensions/extension-spec.md for the full key list."
  [spec]
  (-> spec
    (cond-> (contains? spec :ext/prompt) (update :ext/prompt normalize-prompt))
    (cond->
      (not (:ext/activation-fn spec))                  (assoc :ext/activation-fn (constantly true))
      (some? (derive-kind spec))                       (assoc :ext/kind (derive-kind spec))
      (not (:ext/symbols spec))                        (assoc :ext/symbols [])
      (not (:ext/classes spec))                        (assoc :ext/classes {})
      (not (:ext/imports spec))                        (assoc :ext/imports {})
      (not (:ext/env spec))                            (assoc :ext/env [])
      (not (:ext/settings spec))                       (assoc :ext/settings [])
      (not (:ext/theme spec))                          (assoc :ext/theme {})
      (not (:ext/requires spec))                       (assoc :ext/requires [])
      (not (:ext/cli spec))                            (assoc :ext/cli [])
      (not (:ext/channels spec))                       (assoc :ext/channels [])
      (not (:ext/providers spec))                      (assoc :ext/providers [])
      (not (:ext/persistance spec))                    (assoc :ext/persistance [])
      (not (:ext/channel-hooks spec))                  (assoc :ext/channel-hooks [])
      (not (:ext/fenced-renderers spec))               (assoc :ext/fenced-renderers [])
      (not (:ext/doctor-check-fn spec))                (assoc :ext/doctor-check-fn (constantly [])))
    (validate!)))

;; =============================================================================
;; Extension source markers
;; =============================================================================

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Hash + mtime primitives.
;; ---------------------------------------------------------------------------

(defn- sha256-digest ^MessageDigest []
  (MessageDigest/getInstance "SHA-256"))

(defn- bytes->hex ^String [^bytes b]
  (let [sb (StringBuilder. (* 2 (alength b)))]
    (dotimes [i (alength b)]
      (let [v (bit-and (aget b i) 0xff)]
        (when (< v 16) (.append sb \0))
        (.append sb (Integer/toString v 16))))
    (.toString sb)))

(defn- read-stream-bytes ^bytes [^InputStream in]
  (with-open [out (ByteArrayOutputStream.)]
    (let [buf (byte-array 8192)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n)
            (.write out buf 0 n)
            (recur)))))
    (.toByteArray out)))

;; ---------------------------------------------------------------------------
;; Resolve one namespace → entry map.
;; ---------------------------------------------------------------------------

(defn- ns->resource-path
  "Convert `clojure.core` to `clojure/core.clj`. Tries .clj first;
   .cljc / .cljs fallback if the .clj resolves to nothing."
  [ns-sym]
  (let [base (-> (str ns-sym)
               (str/replace \- \_)
               (str/replace \. \/))]
    [(str base ".clj") (str base ".cljc")]))

(defn- find-source-resource
  ^URL [^ClassLoader cl ns-sym]
  ;; Locate a resource URL for the namespace, trying .clj before .cljc.
  ;; Returns nil when nothing on the classpath corresponds.
  (let [paths (ns->resource-path ns-sym)]
    (some (fn [p] (.getResource cl ^String p)) paths)))

(defrecord ^:private SourceEntry [^String locator ^long mtime ^bytes content])

(defn- file-entry
  "Build a SourceEntry for a `file:` URL. Reads the file content for
   hashing; mtime from `.lastModified`."
  ^SourceEntry [^URL url]
  (let [f       (java.io.File. (.toURI url))
        path    (.getAbsolutePath f)
        mtime   (.lastModified f)
        content (try (read-stream-bytes (java.io.FileInputStream. f))
                  (catch Throwable t
                    (tel/log! {:level :warn :id ::file-read-failed
                               :data  {:path path :error (ex-message t)}})
                    (byte-array 0)))]
    (->SourceEntry path mtime content)))

(defn- jar-entry-locator
  "Build a stable locator string for a jar entry: `jar-path!entry-path`.
   Same convention as the JDK's `jar:` URL form, more readable."
  [^String jar-path ^String entry-name]
  (str jar-path "!" entry-name))

(defn- jar-entry
  "Build a SourceEntry for a `jar:file:` URL. Opens the jar, reads
   the named entry, hashes its content. mtime is the entry's
   `getTime` (= jar build time for entries that weren't individually
   timestamped). Closes the jar on exit."
  ^SourceEntry [^URL url]
  (let [conn      (.openConnection url)
        ;; The cast is paranoid — `.getJarFileURL` lives on `JarURLConnection`,
        ;; we know URL was a jar: URL when we got here.
        jconn     ^java.net.JarURLConnection conn
        jar-url   (.getJarFileURL jconn)
        jar-file  (java.io.File. (.toURI jar-url))
        jar-path  (.getAbsolutePath jar-file)
        entry-nm  (.getEntryName jconn)]
    (with-open [jar (JarFile. jar-file)]
      (let [^JarEntry e (.getJarEntry jar entry-nm)]
        (if (nil? e)
          (do (tel/log! {:level :warn :id ::jar-entry-missing
                         :data  {:jar jar-path :entry entry-nm}})
            nil)
          (let [mtime   (.getTime e)
                content (try (with-open [in (.getInputStream jar e)]
                               (read-stream-bytes in))
                          (catch Throwable t
                            (tel/log! {:level :warn :id ::jar-entry-read-failed
                                       :data  {:jar jar-path :entry entry-nm
                                               :error (ex-message t)}})
                            (byte-array 0)))]
            (->SourceEntry (jar-entry-locator jar-path entry-nm) mtime content)))))))

(defn- url->entry
  "Dispatch on URL protocol to the right reader. Returns SourceEntry
   or nil on unrecognized protocol."
  [^URL url]
  (try
    (case (some-> url .getProtocol str/lower-case)
      "file" (file-entry url)
      "jar"  (jar-entry url)
      (do (tel/log! {:level :warn :id ::unsupported-protocol
                     :data  {:protocol (.getProtocol url) :url (str url)}})
        nil))
    (catch Throwable t
      (tel/log! {:level :warn :id ::resolve-failed
                 :data  {:url (str url) :error (ex-message t)}})
      nil)))

;; ---------------------------------------------------------------------------
;; Public API.
;; ---------------------------------------------------------------------------

(defn resolve-markers
  "Resolve every namespace in `ns-syms` to its source on the classpath
   and compute aggregate markers.

   Returns
     {:source-paths      [\"...\"]               ;; sorted entry locators
      :source-mtime-max  long                    ;; -1 if nothing resolved
      :source-hash-sha256 \"hex\"}                ;; nil if nothing resolved

   Always returns a map (never throws). Per-namespace failures are
   logged at :warn and skipped; an extension whose nses partially
   resolve still gets markers from the parts that did."
  [ns-syms]
  (let [cl       (.getContextClassLoader (Thread/currentThread))
        urls     (->> ns-syms
                   (map #(find-source-resource cl %))
                   (remove nil?))
        entries  (->> urls
                   (map url->entry)
                   (remove nil?)
                   (sort-by :locator)
                   vec)]
    (if (empty? entries)
      {:source-paths       []
       :source-mtime-max   -1
       :source-hash-sha256 nil}
      (let [paths       (mapv :locator entries)
            mtime-max   (long (reduce max 0 (map :mtime entries)))
            digest      (sha256-digest)
            _           (doseq [^SourceEntry e entries]
                          (let [^bytes c (:content e)]
                            (.update digest c 0 (alength c))))
            hash-bytes  (.digest digest)
            hash-hex    (bytes->hex hash-bytes)]
        {:source-paths       paths
         :source-mtime-max   mtime-max
         :source-hash-sha256 hash-hex}))))

(defn resolve-markers-for-extension
  "Convenience wrapper: pull `:nses` (or `:ext/nses`) off the
   extension map / manifest entry, fall back to `:ext/namespace`
   when no list is provided. Returns the same shape as
   [[resolve-markers]]."
  [ext-or-manifest]
  (let [ns-syms (or (some-> (:nses ext-or-manifest) seq vec)
                  (some-> (:ext/nses ext-or-manifest) seq vec)
                  (when-let [n (:ext/namespace ext-or-manifest)]
                    [n]))]
    (resolve-markers (or ns-syms []))))

;; =============================================================================
;; Global Extension Registry
;; =============================================================================

(defonce ^:private extension-registry
  ;; Process-level atom holding all globally registered extensions.
  ;; Keyed by :ext/namespace to prevent duplicates.
  (atom {}))

(defonce ^:private extension-order
  ;; Namespace insertion order for `registered-extensions`. A plain
  ;; hash-map does not preserve order, and adding/removing unrelated
  ;; extensions can reshuffle doctor/lifecycle output.
  (atom []))

(defonce ^:private extension-source-markers
  ;; Sidecar atom holding source-file markers per registered extension.
  ;; Keyed by :ext/namespace. Populated at register-time, dropped at
  ;; deregister-time. Read by `iteration-metadata` (first iteration of
  ;; each turn) and by `reload-extensions!`'s diff. Kept
  ;; OUT of the extension map itself so `extension/validate!` doesn't have
  ;; to know about runtime-derived fields. Plan §5.5.
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
               " -- extension-owned CLI mounts only under [\"extensions\" ...].")
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

   Also computes source-file markers (paths, max-mtime, sha256) and
   stores them in a sidecar atom for the iteration-metadata writer
   and the v2 change-detector. Plan §5.5.

   Idempotent on `:ext/namespace`. Returns the validated extension."
  [ext]
  (let [ext    (extension ext)
        ns-sym (:ext/namespace ext)]
    (when-not (contains? @extension-registry ns-sym)
      (swap! extension-order conj ns-sym))
    (swap! extension-registry assoc ns-sym ext)
    (tel/log! {:level :info :id ::register-global
               :data {:ext ns-sym
                      :symbols     (count (:ext/symbols ext))
                      :cli         (count (:ext/cli ext))
                      :channels    (count (:ext/channels ext))
                      :providers   (count (:ext/providers ext))
                      :persistance (count (:ext/persistance ext))
                      :themes      (count (:ext/theme ext))
                      :fenced-renderers (count (:ext/fenced-renderers ext))}
               :msg (str "Extension '" ns-sym "' registered globally")})
    (doseq [c (:ext/cli ext)]      (registry/register-cmd! (mount-under-extensions c)))
    (doseq [c (:ext/channels ext)] (registry/register-channel! c))
    (dispatch-providers!   (:ext/providers ext))
    (dispatch-persistance! (:ext/persistance ext))
    (theme/register-themes! (:ext/theme ext))
    ;; Compute and store source markers in the sidecar atom. Resolved
    ;; via the helper (see source_markers.clj) which knows how to walk
    ;; both file: and jar: classpath URLs. Failures are logged at :warn
    ;; and degrade to empty markers — they don't fail registration.
    (try
      (let [markers (resolve-markers-for-extension ext)]
        (swap! extension-source-markers assoc ns-sym markers))
      (catch Throwable t
        (tel/log! {:level :warn :id ::source-markers-failed
                   :data  {:ext ns-sym :error (ex-message t)}})))
    ext))

(declare extension-id-of-ns)

(def ^:private empty-source-markers
  {:source-paths       []
   :source-mtime-max   -1
   :source-hash-sha256 nil})

(defn extension-source-markers-of
  "Lookup the source markers stored for `ns-sym`. Returns the marker
   map (`{:source-paths :source-mtime-max :source-hash-sha256}`) or
   nil when the extension was never registered (or its markers
   computation failed at register time)."
  [ns-sym]
  (get @extension-source-markers ns-sym))

(defn- source-markers-for-extension
  [ext]
  (or (extension-source-markers-of (:ext/namespace ext))
    (try
      (resolve-markers-for-extension ext)
      (catch Throwable t
        (tel/log! {:level :warn :id ::source-markers-on-demand-failed
                   :data  {:ext (:ext/namespace ext)
                           :error (ex-message t)}})
        empty-source-markers))
    empty-source-markers))

(defn extension-provenance
  "Canonical extension provenance map.

   Merges author-declared extension metadata with source markers:
     {:namespace :alias? :doc? :kind? :version? :author? :owner?
      :license? :registry-id? :source-paths :source-mtime-max
      :source-hash-sha256}

   This is the single provenance shape used by TURN_ACTIVE_EXTENSIONS,
   `v/extensions`, and tool-result enrichment."
  [ext]
  (let [ext-ns     (:ext/namespace ext)
        alias      (get-in ext [:ext/ns-alias :alias])
        registry-id (or (try (extension-id-of-ns ext-ns)
                          (catch Throwable _ nil))
                      alias)
        markers    (source-markers-for-extension ext)
        prov       (cond-> {:namespace          ext-ns
                            :source-paths       (:source-paths markers)
                            :source-mtime-max   (:source-mtime-max markers)
                            :source-hash-sha256 (:source-hash-sha256 markers)}
                     alias                (assoc :alias alias)
                     (:ext/doc ext)       (assoc :doc (:ext/doc ext))
                     (:ext/kind ext)      (assoc :kind (:ext/kind ext))
                     (:ext/version ext)   (assoc :version (:ext/version ext))
                     (:ext/author ext)    (assoc :author (:ext/author ext))
                     (:ext/owner ext)     (assoc :owner (:ext/owner ext))
                     (:ext/license ext)   (assoc :license (:ext/license ext))
                     registry-id          (assoc :registry-id registry-id))]
    (when-not (s/valid? ::extension-provenance prov)
      (throw (ex-info "Invalid extension provenance"
               {:type      :extension/invalid-provenance
                :namespace ext-ns
                :value     prov
                :explain   (s/explain-data ::extension-provenance prov)})))
    prov))

(defn deregister-extension!
  "Drop an extension from the global registry AND reverse every side
   effect `register-extension!` dispatched: deregister each CLI
   subcommand, channel, provider, and persistence backend. Returns nil.

   Plan caveat: side-effect cleanup on `:removed` extensions. Used by
   `reload-extensions!` when an extension disappears between scans."
  [ns-sym]
  (when-let [ext (get @extension-registry ns-sym)]
    (doseq [c (:ext/cli ext)]
      (let [mounted (mount-under-extensions c)]
        (try (registry/deregister-cmd! (:cmd/parent mounted) (:cmd/name mounted))
          (catch Throwable t
            (tel/log! {:level :warn :id ::deregister-cmd-failed
                       :data  {:ext ns-sym :cmd (:cmd/name mounted)
                               :error (ex-message t)}})))))
    (doseq [c (:ext/channels ext)]
      (try (registry/deregister-channel! (:channel/id c))
        (catch Throwable t
          (tel/log! {:level :warn :id ::deregister-channel-failed
                     :data  {:ext ns-sym :channel-id (:channel/id c)
                             :error (ex-message t)}}))))
    (doseq [p (:ext/providers ext)]
      (try (registry/deregister-provider! (:provider/id p))
        (catch Throwable t
          (tel/log! {:level :warn :id ::deregister-provider-failed
                     :data  {:ext ns-sym :provider-id (:provider/id p)
                             :error (ex-message t)}}))))
    (doseq [{:persistance/keys [id]} (:ext/persistance ext)]
      (try (persistance/deregister-backend! id)
        (catch Throwable t
          (tel/log! {:level :warn :id ::deregister-backend-failed
                     :data  {:ext ns-sym :backend-id id
                             :error (ex-message t)}}))))
    (theme/unregister-themes! (keys (:ext/theme ext)))
    (tel/log! {:level :info :id ::deregister-global
               :data {:ext ns-sym}
               :msg  (str "Extension '" ns-sym "' deregistered globally")}))
  (swap! extension-registry dissoc ns-sym)
  (swap! extension-order (fn [order] (vec (remove #{ns-sym} order))))
  (swap! extension-source-markers dissoc ns-sym)
  nil)

(defn registered-extensions []
  (let [registry @extension-registry]
    (into [] (keep registry) @extension-order)))

(defn emit-proof-event!
  "Broadcast one structured proof lifecycle event to registered extensions.
   Payload must contain `:proof/event`. Listener failures are logged and never
   prevent the persistence operation that emitted the event."
  [{:proof/keys [event] :as payload}]
  (when-not (contains? proof-event-kinds event)
    (throw (ex-info "unsupported proof lifecycle event" {:proof/event event})))
  (doseq [ext (registered-extensions)
          :let [listener (:ext/on-proof-event-fn ext)]
          :when listener]
    (try
      (binding [*current-extension* ext
                *current-symbol* nil]
        (listener payload))
      (catch Throwable t
        (tel/log! {:level :warn :id ::proof-event-listener-threw
                   :data {:ext (:ext/namespace ext)
                          :proof-event event
                          :error (ex-message t)
                          :ex-class (.getName (class t))}
                   :msg "proof lifecycle listener threw; broadcast continues"}))))
  nil)

(defn channel-hooks-for
  "Return registered extension channel hooks for `channel-id` in extension
   registration order. Hooks are passive data; channels decide which hook
   keys they support."
  [channel-id]
  (->> (registered-extensions)
    (mapcat :ext/channel-hooks)
    (filter #(= channel-id (:channel-id %)))
    vec))

(defn- rendering-kind-fn
  [rendering-kind]
  (some (fn [ext]
          (get-in ext [:ext/rendering-kinds rendering-kind]))
    (registered-extensions)))

(defn render-rendering-kind
  "Render `value` using the first registered extension renderer for
     `rendering-kind`. Returns nil when no extension owns that kind."
  ([surface rendering-kind value]
   (render-rendering-kind surface rendering-kind value {}))
  ([surface rendering-kind value ctx]
   (when-let [render-fn (rendering-kind-fn rendering-kind)]
     (let [ctx* (assoc ctx
                  :surface surface
                  :rendering-kind rendering-kind
                  :value value)
           rendered (render-fn ctx*)]
       (if (string? rendered) rendered (pr-str rendered))))))

(defn- normalize-fence-lang
  [lang]
  (let [lang (some-> lang str str/trim str/lower-case)]
    (when (non-blank-string? lang) lang)))

(defn fenced-renderers
  "Return extension-owned Markdown fenced-code renderers in registration order."
  []
  (->> (registered-extensions)
    (mapcat :ext/fenced-renderers)
    vec))

(defn- fenced-renderer-supports?
  [renderer normalized-lang]
  (contains? (set (keep normalize-fence-lang (:renderer/langs renderer)))
    normalized-lang))

(defn- normalize-fenced-render-result
  [renderer-id result]
  (let [lines (cond
                (nil? result) nil
                (string? result) (str/split-lines result)
                (sequential? result) (mapv str result)
                (map? result) (let [v (or (:lines result) (:text result))]
                                (cond
                                  (string? v) (str/split-lines v)
                                  (sequential? v) (mapv str v)
                                  :else nil))
                :else [(str result)])]
    (when (seq lines)
      (cond-> (if (map? result) result {})
        :always (assoc :renderer/id renderer-id
                  :lines (mapv str lines))))))

(defn render-fenced-block
  "Render a Markdown fenced code block through extension-owned renderers.

     `ctx` keys are channel-defined but should include at least
     `:surface`, `:lang`, `:source`, and `:width`. Returns normalized
     `{:renderer/id kw :lines [string ...] ...}` or nil for fallback.
     Renderer exceptions are logged and treated as nil so display falls back
     to Vis normal fenced code block renderer."
  [ctx]
  (let [lang (normalize-fence-lang (:lang ctx))]
    (when lang
      (some
        (fn [{:renderer/keys [id render-fn] :as renderer}]
          (when (fenced-renderer-supports? renderer lang)
            (try
              (normalize-fenced-render-result id
                (render-fn (assoc ctx :lang lang :renderer/id id)))
              (catch Throwable t
                (tel/log! {:level :warn :id ::fenced-renderer-failed
                           :data {:renderer/id id
                                  :lang lang
                                  :error (ex-message t)
                                  :ex-class (.getName (class t))}
                           :msg (str "fenced renderer " id " failed for ```" lang "`; falling back")})
                nil))))
        (fenced-renderers)))))

(defn- tool-result-symbol-entry
  [tool-result]
  (let [ext-ns (get-in tool-result [:provenance :extension :namespace])
        sym    (get-in tool-result [:provenance :tool :sym])]
    (when (and ext-ns sym)
      (some (fn [entry]
              (when (= sym (:ext.symbol/sym entry))
                entry))
        (:ext/symbols (get @extension-registry ext-ns))))))

(defn render-tool-result
  "Render a structured tool result for one consumer surface.

    Dispatch is extension/symbol-owned: `:ext.symbol/render-fn` receives a
    context map with `:surface` and `:tool-result`. Every fn-symbol has a
    render-fn (either custom or the default `render-value`). There is no
    generic fallback — each symbol owns its presentation."
  ([surface tool-result]
   (render-tool-result surface tool-result {}))
  ([surface tool-result ctx]
   (let [ctx*              (assoc ctx :surface surface :tool-result tool-result)
         render-fn         (some-> (tool-result-symbol-entry tool-result)
                             :ext.symbol/render-fn)
         presentation-kind (get-in tool-result [:presentation :kind])]
     (cond
       render-fn
       (let [rendered (render-fn ctx*)]
         (if (string? rendered) rendered (pr-str rendered)))

       presentation-kind
       (or (render-rendering-kind surface presentation-kind (:result tool-result) ctx*)
         (throw (AssertionError.
                  (str "No render-fn for tool result with op "
                    (pr-str (get-in tool-result [:provenance :op]))))))

       :else
       (throw (AssertionError.
                (str "No render-fn for tool result with op "
                  (pr-str (get-in tool-result [:provenance :op])))))))))

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
   `{:name :created-at :abstract :content :links :reflinks}`. Returns
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

(defn extension-doc-abstract
  "Return the `:abstract` field of a declared extension doc, or `nil`
   when the doc is unknown."
  [id doc-name]
  (:abstract (extension-doc id doc-name)))

(defn extension-doc-summary
  "Lightweight doc descriptor (no `:content`):
   `{:name :created-at :abstract :links :reflinks}`. Returns `nil`
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
