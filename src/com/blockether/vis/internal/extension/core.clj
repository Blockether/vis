(ns com.blockether.vis.internal.extension.core
  "Extension subsystem: spec, builders, hook execution, the global registry,
   and parse-error rescue.

   An extension is the SINGLE entry point for everything a third-party
   bundle contributes to vis. Whatever surfaces it populates - Python
   sandbox symbols, CLI commands, channels, providers, persistence
   backends - it does so by listing them in the matching `:ext/<surface>`
   slot, and `register-extension!` dispatches each slot to its concrete
   sub-registry. The same data feeds:

     - the active-extensions list every iteration consults
     - the system-prompt block rendered from `:ext.engine/symbols`
     - the per-iteration `:ext/hooks` checks
     - the parse-error rescue chain

   Channel and provider registries live in `internal.registry`; the persistence
   backend table lives in `internal.persistance`. The one ordered distribution manifest invokes
   each extension's explicit registration function."
  (:refer-clojure :exclude [symbol])
  (:require [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.anomaly.core :as anomaly]
            [com.blockether.vis.internal.activity.event :as activity-event]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress-proxy]
            [com.blockether.vis.internal.extension.manifest :as manifest]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.channel.theme :as theme]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.net URL)
           (java.util.jar JarEntry JarFile)))

;; Tool-result contract
(def ^:private max-trace-frames 12)

(declare op-tag op-tags op-keyword->tag op-keyword->batch-hint tool-call-name)

(defn- optional-field? [m k pred] (or (not (contains? m k)) (pred (get m k))))

(defn- non-blank-string? [x] (util/non-blank-string? x))

(defn- error-block?
  [x]
  (or (nil? x)
      (and (map? x)
           (string? (:source x))
           (= :preflight (:phase x))
           (optional-field? x :row pos-int?)
           (optional-field? x :col pos-int?)
           (optional-field? x
                            :opened-loc
                            #(or (nil? %)
                                 (and (map? %) (pos-int? (:row %)) (pos-int? (:col %))))))))

(defn- tool-error?
  [x]
  (or (nil? x)
      (and (map? x)
           (non-blank-string? (:message x))
           (optional-field? x :trace #(or (nil? %) (string? %)))
           (optional-field? x :hint #(or (nil? %) (non-blank-string? %)))
           (optional-field? x :block error-block?))))

(defn tool-result?
  "True when `x` is a valid tool-result envelope."
  [x]
  (and (map? x)
       (contains? x :success?)
       (boolean? (:success? x))
       (optional-field? x :symbol #(or (keyword? %) (symbol? %)))
       (optional-field? x :tag keyword?)
       (optional-field? x :metadata #(and (map? %) (every? keyword? (keys %))))
       (optional-field? x :error tool-error?)
       (if (:success? x) (nil? (:error x)) (some? (:error x)))))

(defn assert-tool-result!
  [x]
  (when-not (tool-result? x)
    (throw (ex-info "Invalid tool result" {:type :vis/invalid-tool-result :value x})))
  x)

(def ^:dynamic *tool-event-sink*
  "Optional per-eval sink for immutable tool lifecycle events. The sink is a
   presentation observer: a failure is logged and cannot change tool behavior."
  nil)

(def ^:dynamic *tool-event-context*
  "Evaluation-scoped Activity identity, sequence allocators, and form anchor."
  nil)

(def ^:dynamic *current-invocation-id*
  "Invocation whose body is running, used only as observed parentage."
  nil)

(defn- record-tool-event!
  [event]
  (when *tool-event-sink*
    (try (*tool-event-sink* event)
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::tool-event-sink-failed
                      :error t
                      :msg "Activity observer rejected a lifecycle event"}))))
  event)

(def ^:dynamic *current-form-idx*
  "Zero-based index of the top-level form currently evaluating, bound
   per-form by `run-python-code` so the render sink writer can stamp
   `:form-idx` on every entry.

   The render sink atom itself is iteration-scoped (one channel-sink
   per `run-python-code` invocation, fed by every tool call in the
   block, which runs as one whole-block coroutine)."
  nil)

(defn normalize-metadata
  "Fill timing keys on the `:metadata` map when absent. Returns a
   metadata map (NOT an envelope). The envelope wraps the result of
   this fn under `:metadata`.

   Timing keys (always populated):
     :started-at-ms  :finished-at-ms  :duration-ms

   Callers may pass richer maps (tool / extension / source metadata,
   tool-specific :paths / :hit-count / :command); this helper only
   normalizes the shared timing surface."
  [metadata]
  (let [metadata
        (or metadata {})

        t
        (util/now-ms)

        started
        (long (or (:started-at-ms metadata) t))

        finished
        (long (or (:finished-at-ms metadata) t))

        duration
        (long (or (:duration-ms metadata) (max 0 (- finished started))))]

    (assoc metadata
      :started-at-ms started
      :finished-at-ms finished
      :duration-ms duration)))

(defn merge-into-metadata
  "Merge `extra` into the `:metadata` slot of an already-valid
   envelope, re-check the contract, and preserve metadata. Used by the
   extension wrapper to stamp extension/source info onto tool-like
   returns."
  [envelope extra]
  (let [meta*
        (meta envelope)

        merged
        (-> envelope
            (update :metadata #(merge (or % {}) extra))
            assert-tool-result!)]

    (with-meta merged meta*)))

(defn- noisy-frame?
  [^StackTraceElement frame]
  (let [class-name (.getClassName frame)]
    (or (str/starts-with? class-name "clojure.lang.AFn")
        (str/starts-with? class-name "clojure.lang.RestFn")
        (str/starts-with? class-name "clojure.lang.MultiFn")
        (str/starts-with? class-name "clojure.lang.Var")
        (str/starts-with? class-name "java.lang.reflect.")
        (str/starts-with? class-name "jdk.internal.reflect."))))

(defn normalize-trace
  "Convert a Throwable's stack into the preformatted, babashka-style
   single-string `::op.error/trace`. First line is
   `<ClassName>: <message>` (matches babashka error-handler header);
   subsequent lines are filtered frames (one per line, `class/method
   - file:line`).

   Frames in `noisy-frame?` (clojure.lang reflection,
   java.lang.reflect, jdk.internal.reflect) are dropped to keep the
   trace LLM-friendly. Capped at `max-trace-frames` lines after the
   header."
  [^Throwable t]
  (let [header
        (str (.getName (class t)) ": " (or (ex-message t) ""))

        frames
        (->> (.getStackTrace t)
             (remove noisy-frame?)
             (take max-trace-frames)
             (map (fn [^StackTraceElement f]
                    (str (.getClassName f)
                         "/"
                         (.getMethodName f)
                         " - "
                         (or (.getFileName f) "unknown")
                         (when (pos? (.getLineNumber f)) (str ":" (.getLineNumber f)))))))]

    (str/join "\n" (cons header frames))))

(defn normalize-error
  "Build a structured `:error` map from a Throwable.
   Required `:message`; optional `:trace` (preformatted string
   including header + frames). `:hint` and `:block` are tool/engine-
   supplied via `merge-into-metadata` style updates after
   construction."
  [^Throwable t]
  (let [trace (normalize-trace t)]
    (cond-> {:message (or (not-empty (ex-message t)) (.getName (class t)))}
      (not (str/blank? trace))
      (assoc :trace trace))))

(defn- envelope-of
  "Internal builder used by both `success` and `failure`. Accepts
   only the canonical shape:

     :result   raw Python eval value; stored under `:result`
     :op       op symbol e.g. :cat (nil for raw user code)
     :metadata free-form aux map: :tool, :extension, :source,
               :paths, :hit-count, :command, :started-at-ms,
               :finished-at-ms, :duration-ms, etc.

   Produces a flat `:envelope` map."
  [{:keys [result op metadata]} success? error]
  (cond-> {:result result :success? success? :error error :metadata (normalize-metadata metadata)}
    op
    (assoc :symbol
      op :tag
      (op-tag op))

    :always
    assert-tool-result!))

(defn success
  "Construct a successful tool-result envelope. See `envelope-of` for
   the call shape. Returns a `:envelope` map (flat, all metadata
   under `op/*`)."
  [args]
  (envelope-of args true nil))

(defn failure
  "Construct a failing tool-result envelope. `:throwable` auto-builds
   an `:error` map via `normalize-error`. Explicit `:error`
   (already structured) wins."
  [{:keys [error throwable] :as args}]
  (let [err (or error (when throwable (normalize-error throwable)))]
    (envelope-of args false err)))

(defn envelope-success?
  "True when `envelope` is an `:envelope` and `:success?` is
   true. Use this instead of raw `(:success? e)` in renderers and
   guards — it (a) reads as English and (b) returns false for non-
   envelopes (defensive against shape drift)."
  [envelope]
  (and (tool-result? envelope) (true? (:success? envelope))))

(defn envelope-failure?
  "True when `envelope` is an `:envelope` and `:success?` is
   false (i.e. failure path with a structured `:error`). Returns
   false for non-envelopes."
  [envelope]
  (and (tool-result? envelope) (false? (:success? envelope))))

(defn ex->op-error
  "Convert an arbitrary `Throwable` to a structured `:error` map.

   Output shape:
     {:message <one-line headline, required>
      :trace   <preformatted multi-line string, optional>
      :hint    <recovery suggestion, optional>
      :block   {:source :phase :row :col :opened-loc?, optional}}

   Throwables reaching here are transport / spec / wrapping failures —
   Python eval errors are mapped to op-error shape inside the engine via
   `env-python/map-python-error`, so the block `:phase` is `:preflight`.

   Optional opts:
     :form-source  the verbatim source the form was built from;
                    embedded in `:block.source` so the model sees its
                    own input echoed back.
     :hint          override / pre-supply a recovery hint string."
  [^Throwable t & [{:keys [form-source hint]}]]
  (let [d
        (ex-data t)

        cause
        (some-> t
                .getCause)

        message
        (or (not-empty (ex-message t)) (.getName (class t)))

        trace
        (try (normalize-trace t) (catch Throwable _ nil))

        block
        (when form-source {:source form-source :phase :preflight})

        cause-data
        (when cause (ex-data cause))

        ;; A tool that returned `{:success? false :error <map>}` is
        ;; un-structured back into a thrown ExceptionInfo by
        ;; `tool-result->public-value`. The structured `:error` map
        ;; carries the model-actionable info (`:reason`, `:failures`,
        ;; `:loop-hint`, `:checks` …). Lift it into `:data` so the
        ;; iteration trailer (`error-lines` in `ctx.clj`) renders it as
        ;; `;; ! data {…}`. Without this lift the model only sees
        ;; `:message` + `:trace` and has to decode `:reason` from prose.
        tool-error-data
        (when (= :vis/tool-failure (:type d))
          (let [e (:error d)]
            (when (map? e)
              (not-empty (cond-> {}
                           (some? (:reason e))
                           (assoc :reason (:reason e))

                           (seq (:failures e))
                           (assoc :failures (:failures e))

                           (seq (:checks e))
                           (assoc :checks (:checks e))

                           (some? (:loop-hint e))
                           (assoc :loop-hint (:loop-hint e))

                           (some? (:mode e))
                           (assoc :mode (:mode e)))))))]

    (cond-> {:message message}
      (not (str/blank? trace))
      (assoc :trace trace)

      hint
      (assoc :hint hint)

      block
      (assoc :block block)

      cause-data
      (assoc :cause-data cause-data)

      tool-error-data
      (assoc :data tool-error-data))))

;; Extension declaration validation
(defn- vector-of? [pred x] (and (vector? x) (every? pred x)))

(defn- set-of? [pred x] (and (set? x) (every? pred x)))

(defn- unqualified-symbol? [x] (and (symbol? x) (nil? (namespace x))))

(defn- map-values?
  [key-pred value-pred x]
  (and (map? x) (every? key-pred (keys x)) (every? value-pred (vals x))))

(defn- param-entry?
  [x]
  (and (map? x)
       (non-blank-string? (:name x))
       (optional-field? x :required? boolean?)
       (optional-field? x :note non-blank-string?)))

(defn- params? [x] (and (vector-of? param-entry? x) (seq x)))

(defn- fn-symbol-entry?
  [x]
  (and (map? x)
       (symbol? (:ext.symbol/symbol x))
       (fn? (:ext.symbol/fn x))
       (non-blank-string? (:ext.symbol/doc x))
       (vector? (:ext.symbol/arglists x))
       (seq (:ext.symbol/arglists x))
       (optional-field? x :ext.symbol/raw? boolean?)
       (optional-field? x :ext.symbol/hidden? boolean?)
       (optional-field? x :ext.symbol/tag #{:observation :mutation})
       (optional-field? x :ext.symbol/presenter keyword?)
       (optional-field? x :ext.symbol/batch-hint pos-int?)
       (every? #(optional-field? x % fn?)
               [:ext.symbol/before-fn :ext.symbol/active-fn :ext.symbol/after-fn
                :ext.symbol/on-error-fn :ext.symbol/ticker-fn])
       (optional-field? x :ext.symbol/inject-env? boolean?)
       (optional-field? x :ext.symbol/source non-blank-string?)
       (optional-field? x :ext.symbol/name non-blank-string?)
       (optional-field? x :ext.symbol/call #(or (map? %) (fn? %)))
       (optional-field? x :ext.symbol/description non-blank-string?)
       (optional-field? x :ext.symbol/result non-blank-string?)
       (optional-field? x :ext.symbol/params params?)))

(defn- val-symbol-entry?
  [x]
  (and (map? x)
       (symbol? (:ext.symbol/symbol x))
       (contains? x :ext.symbol/val)
       (some? (:ext.symbol/val x))
       (non-blank-string? (:ext.symbol/doc x))
       (optional-field? x :ext.symbol/source non-blank-string?)))

(defn- symbol-entry? [x] (or (fn-symbol-entry? x) (val-symbol-entry? x)))

(def canonical-hook-phases
  "Canonical lifecycle phase keywords accepted by `:ext/hooks`."
  #{:session_provider_kickoff :turn.answer/validate})

(defn hook-phase? [phase] (contains? canonical-hook-phases phase))

(defn- op-hook?
  [x]
  (and (map? x)
       (keyword? (:op x))
       (ifn? (:fn x))
       (optional-field? x :phase #{:before :around :after :gate})))

(defn- hook?
  [x]
  (and (map? x)
       (keyword? (:id x))
       (non-blank-string? (:doc x))
       (hook-phase? (:phase x))
       (fn? (:fn x))))

(defn session-provider-kickoff-headers?
  [x]
  (and (map? x)
       (= #{:llm-headers} (set (keys x)))
       (map-values? non-blank-string? non-blank-string? (:llm-headers x))
       (seq (:llm-headers x))))

(defn iteration-start-hint?
  [x]
  (and (map? x) (non-blank-string? (:text x)) (optional-field? x :importance keyword?)))

(defn answer-validation-reject?
  [x]
  (and (map? x)
       (true? (:reject x))
       (optional-field? x :message non-blank-string?)
       (optional-field? x :hint non-blank-string?)))

(defn- channel-slot?
  [x]
  (and (keyword? x)
       (some-> (namespace x)
               (str/ends-with? ".slot"))))

(defn- channel-slot->channel-id
  [slot]
  (let [ns (namespace slot)]
    (when-not (and (keyword? slot) ns (str/ends-with? ns ".slot"))
      (throw (ex-info "Channel contribution slot must be a qualified keyword ending in .slot"
                      {:type :extension/invalid-channel-contribution-slot :slot slot})))
    (keyword (subs ns 0 (- (count ns) (count ".slot"))))))

(defn- channel-contribution? [x] (and (map? x) (keyword? (:id x)) (ifn? (:fn x))))

(defn- channel-contributions?
  [x]
  (and (map? x)
       (every? channel-slot? (keys x))
       (every? #(vector-of? channel-contribution? %) (vals x))))

(defn- slash?
  [x]
  (and (map? x)
       (non-blank-string? (:slash/name x))
       (optional-field? x :slash/parent #(vector-of? non-blank-string? %))
       (optional-field? x :slash/doc non-blank-string?)
       (optional-field? x :slash/usage non-blank-string?)
       (optional-field? x :slash/run-fn ifn?)
       (optional-field? x :slash/requires #(set-of? #{:session :workspace :channel} %))
       (optional-field? x :slash/availability-fn ifn?)
       (optional-field? x :slash/subcommands #(vector-of? non-blank-string? %))))

(defn slash-path
  "Canonical full path vector of a slash declaration."
  [slash-spec]
  (conj (vec (:slash/parent slash-spec)) (:slash/name slash-spec)))

(defn- env-entry?
  [x]
  (and (map? x)
       (non-blank-string? (:name x))
       (optional-field? x :label non-blank-string?)
       (optional-field? x :description string?)
       (optional-field? x :secret? boolean?)
       (optional-field? x :required? boolean?)))

(defn- setting-entry?
  [x]
  (and (map? x)
       (or (keyword? (:key x)) (non-blank-string? (:key x)))
       (#{:toggle :choice :action} (:type x))
       (non-blank-string? (:label x))
       (optional-field? x :description string?)
       (optional-field? x :choices #(and (vector-of? keyword? %) (seq %)))))

(defn- provider-entry?
  [x]
  (let [optional-fn? (fn [k]
                       (optional-field? x k #(or (nil? %) (ifn? %))))]
    (and (map? x)
         (not (contains? x :provider/prompt-fn))
         (keyword? (:provider/id x))
         (non-blank-string? (:provider/label x))
         (every? optional-fn?
                 [:provider/status-fn :provider/logout-fn :provider/detect-fn :provider/auth-fn
                  :provider/get-token-fn :provider/refresh-token-fn :provider/limits-fn
                  :provider/enrich-models-fn :provider/on-selected-fn])
         (optional-field? x :provider/is-managed boolean?)
         (optional-field? x :provider/limits-cache-ms pos-int?))))

(defn- sandbox-shim?
  [x]
  (and (map? x)
       (non-blank-string? (:shim/name x))
       (non-blank-string? (:shim/source x))
       (optional-field? x :shim/imports #(and (vector-of? non-blank-string? %) (apply distinct? %)))
       (optional-field? x :shim/globals #(and (vector-of? non-blank-string? %) (apply distinct? %)))
       (optional-field? x :shim/docs non-blank-string?)
       (optional-field? x :shim/bindings #(or (ifn? %) (map-values? string? ifn? %)))))

(defn- engine?
  [x]
  (and (map? x)
       (optional-field? x :ext.engine/ns unqualified-symbol?)
       (optional-field? x :ext.engine/alias unqualified-symbol?)
       (optional-field? x :ext.engine/builtin? boolean?)
       (optional-field? x :ext.engine/exact-symbol-names? boolean?)
       (optional-field? x :ext.engine/symbols #(vector-of? symbol-entry? %))
       (optional-field? x :ext.engine/classes #(map-values? symbol? class? %))
       (optional-field? x :ext.engine/imports #(map-values? symbol? symbol? %))))

(defn- extension-info?
  [x]
  (and (map? x)
       (non-blank-string? (:name x))
       (vector-of? string? (:source-paths x))
       (integer? (:source-mtime-max x))
       (or (nil? (:source-hash-sha256 x))
           (and (string? (:source-hash-sha256 x)) (= 64 (count (:source-hash-sha256 x)))))
       (optional-field? x :alias symbol?)
       (every? #(optional-field? x % non-blank-string?)
               [:description :kind :version :author :owner :license])
       (optional-field? x :registry-id symbol?)))

(defn ext-symbols [ext] (vec (or (get-in ext [:ext/engine :ext.engine/symbols]) [])))

(defn ext-sandbox-shims [ext] (vec (or (:ext/sandbox-shims ext) [])))

(defn symbol-active?
  "Whether a symbol entry is active for `env`."
  [entry env]
  (if-let [active-fn (:ext.symbol/active-fn entry)]
    (boolean (try (active-fn env) (catch Throwable _ false)))
    true))

(defn ext-alias-symbol [ext] (get-in ext [:ext/engine :ext.engine/alias]))

(defn ext-exact-symbol-names?
  [ext]
  (boolean (get-in ext [:ext/engine :ext.engine/exact-symbol-names?])))

(defn ext-builtin? [ext] (boolean (get-in ext [:ext/engine :ext.engine/builtin?])))

(defn ext-source-nses [ext] (vec (or (:ext/source-nses ext) [])))

(defn- ns-alias-required-when-symbols?
  [ext]
  (or (empty? (ext-symbols ext)) (some? (ext-alias-symbol ext)) (ext-builtin? ext)))

(defn- kind-required-when-symbols? [ext] (or (empty? (ext-symbols ext)) (some? (:ext/kind ext))))

(defn- extension?
  [x]
  (and (map? x)
       (non-blank-string? (:ext/name x))
       (non-blank-string? (:ext/description x))
       (optional-field? x :ext/source-nses #(vector-of? symbol? %))
       (optional-field? x :ext/kind non-blank-string?)
       (every? #(optional-field? x % fn?)
               [:ext/activation-fn :ext/prompt-fn :ext/ctx-fn :ext/doctor-fn])
       (optional-field? x :ext/hooks #(vector-of? hook? %))
       (optional-field? x :ext/op-hooks #(every? op-hook? %))
       (optional-field? x :ext/network-filters #(every? ifn? %))
       (optional-field? x :ext/env #(vector-of? env-entry? %))
       (optional-field? x :ext/settings #(vector-of? setting-entry? %))
       (optional-field? x :ext/theme theme/extension-theme-map?)
       (every? #(optional-field? x % non-blank-string?)
               [:ext/version :ext/author :ext/owner :ext/license])
       (optional-field? x :ext/cli #(vector-of? registry/command? %))
       (optional-field? x :ext/channels #(vector-of? registry/channel? %))
       (optional-field? x :ext/providers #(vector-of? provider-entry? %))
       (optional-field? x :ext/attachment-storage #(vector-of? map? %))
       (optional-field? x :ext/channel-contributions channel-contributions?)
       (optional-field? x :ext/slash-commands #(vector-of? slash? %))
       (optional-field? x :ext/sandbox-shims #(vector-of? sandbox-shim? %))
       (optional-field? x :ext/engine engine?)
       (ns-alias-required-when-symbols? x)
       (kind-required-when-symbols? x)))

(defn- validate-symbol-entry!
  [entry]
  (when-not (symbol-entry? entry)
    (throw (ex-info
             (str "Invalid symbol '" (:ext.symbol/symbol entry) "'")
             {:type :extension/invalid-symbol :symbol (:ext.symbol/symbol entry) :entry entry})))
  entry)

(defn- var-source
  "Best-effort source form for a host Var. Stored on extension symbol entries
   so the Python sandbox's `source(...)` can show source for aliased
   extension vars whose sandbox namespace (`v.`, ...) is synthetic."
  [v]
  (let [m
        (meta v)

        ns
        (:ns m)

        nm
        (:name m)]

    (when (and ns nm)
      (try (repl/source-fn (clojure.core/symbol (str (ns-name ns)) (str nm)))
           (catch Throwable _ nil)))))

(defn- var-meta
  "Read `:doc` / `:arglists` / `:name` / source from a var's metadata. Throws when the
   var lacks a non-blank docstring or non-empty arglists - extension symbols
   carry their canonical surface from the underlying defn, not from a side
   map. Without these, the Python sandbox cannot expose `doc(sym)` to the
   model and the prompt-listing has no doc line to render.

   Opts can supply `:doc`, `:doc-fn`, or `:arglists` for third-party vars
   whose metadata is incomplete. Raw helpers default missing arglists to
   `([& args])` so library APIs can still be surfaced without per-var glue."
  ([v require-arglists?] (var-meta v require-arglists? nil))
  ([v require-arglists? opts]
   (when-not (var? v)
     (anomaly/incorrect! "vis/symbol and vis/value require a Clojure var (e.g. #'my-tool)"
                         {:type :extension/symbol-not-a-var :given v}))
   (let [m
         (meta v)

         nm
         (:name m)

         doc-fn
         (:doc-fn opts)

         doc
         (or (:doc opts) (:doc m) (when doc-fn (doc-fn (or (:symbol opts) nm) v)))

         al
         (or (:arglists opts) (:arglists m) (when (:raw? opts) '([& args])))]

     (when-not (util/non-blank-string? doc)
       (anomaly/incorrect! (str "Var " v
                                " is missing a docstring; extension symbols inherit "
                                ":doc from the underlying defn (no side maps).")
                           {:type :extension/missing-doc :var v}))
     ;; defn auto-attaches :arglists as a LIST (e.g. '([x] [x y])); manual
     ;; ^{:arglists ...} likewise. The downstream spec requires vector?.
     ;; Accept any non-empty sequential and coerce to a vector here so the
     ;; spec stays strict at the storage boundary while callers stay free
     ;; to use either shape.
     (when (and require-arglists? (not (and (sequential? al) (seq al))))
       (anomaly/incorrect! (str "Var " v
                                " is missing :arglists in its metadata; extension fn "
                                "symbols inherit :arglists from the underlying defn.")
                           {:type :extension/missing-arglists :var v}))
     (let [source (var-source v)]
       (cond-> {:symbol nm :doc doc :arglists (when (seq al) (vec al))}
         source
         (assoc :source source))))))

(defn- build-symbol-entry
  "Shared core that turns `{:symbol :fn :doc :arglists :source}` plus opts into
   a validated `::fn-symbol-entry`. Observed tools keep their symbol-specific
   channel renderer; raw helpers do not render. Used by both the
   var-based public API and the test-friendly direct-args form below.

   Opts may carry `:tag :observation | :mutation`. When present,
   `register-extension!` walks the symbol vec and auto-populates the
   global op-keyword -> tag index so call-sites don't need an
   out-of-band registration step per symbol."
  [{sym :symbol :keys [fn doc arglists source]} opts]
  (let [raw?
        (true? (:raw? opts))

        entry
        (cond-> #:ext.symbol{:symbol sym :fn fn :doc doc :arglists arglists}
          raw?
          (assoc :ext.symbol/raw? true)

          (:hidden? opts)
          (assoc :ext.symbol/hidden? true)

          source
          (assoc :ext.symbol/source source)

          (:tag opts)
          (assoc :ext.symbol/tag (:tag opts))

          (:presenter opts)
          (assoc :ext.symbol/presenter (:presenter opts))

          ;; :call — how a Python call's folded kwargs re-expand onto this symbol's
          ;; positional parameters. See the `:ext.symbol/call` spec above.
          (:call opts)
          (assoc :ext.symbol/call (:call opts))

          (:name opts)
          (assoc :ext.symbol/name (:name opts))

          (:description opts)
          (assoc :ext.symbol/description (:description opts))

          (:result opts)
          (assoc :ext.symbol/result (:result opts))

          ;; :params — the options-dict key vocabulary rendered as `doc`'s `Keys:`
          ;; line. See the `:ext.symbol/params` spec above.
          (:params opts)
          (assoc :ext.symbol/params (vec (:params opts)))

          ;; :active-fn (fn [env] -> bool) — dynamic per-symbol activation gate.
          (:active-fn opts)
          (assoc :ext.symbol/active-fn (:active-fn opts))

          ;; :inject-env? true — prepend the live env as the call's first arg.
          (contains? opts :inject-env?)
          (assoc :ext.symbol/inject-env? (boolean (:inject-env? opts)))

          (:batch-hint opts)
          (assoc :ext.symbol/batch-hint (:batch-hint opts))

          (:before-fn opts)
          (assoc :ext.symbol/before-fn (:before-fn opts))

          (:after-fn opts)
          (assoc :ext.symbol/after-fn (:after-fn opts))

          (:ticker-fn opts)
          (assoc :ext.symbol/ticker-fn (:ticker-fn opts))

          (:on-error-fn opts)
          (assoc :ext.symbol/on-error-fn (:on-error-fn opts)))]

    (validate-symbol-entry! entry)))

(defn symbol
  "Build a function symbol entry FROM A CLOJURE VAR.

   The 3-arg form `(symbol sym-name f opts)` is a test-friendly direct
   constructor: pass the sandbox-visible symbol, the implementation fn, and
   an opts map whose `:doc` / `:arglists` are read directly from opts
   instead of var meta. Production code uses the var form.

   The var supplies `:symbol` (var name), `:fn` (the var's value), `:doc` and
   `:arglists` (read from var metadata - i.e. the underlying defn's
   docstring + arglists). Pass it as `#'my-tool`.

   Observed tools return canonical internal envelope maps. The model-facing
   surface is the per-iteration trailer (real Python form values); there is
   no per-symbol render callback — a printed result is painted from the
   result's own data.

   Raw helpers pass `:raw? true` and return plain values directly, with no
   envelope enforcement, channel sink, or tool metadata.

   Optional opts:
     :symbol      - override the Python sandbox name (default: var name).
     :doc-fn      - compute doc lazily from `(sym v)` when the var
                    lacks a docstring (third-party vars only).
     :raw?        - true for plain composable helpers.
     :tag         - REQUIRED `:observation | :mutation` for observed
                    tools (unless `:raw? true`).
     :params      - options-dict key vocabulary `[{:name \"paths\" :required? true}
                    {:name \"ranges\"}]`, rendered by `doc(name)`. REQUIRED of every
                    tool whose call ends in an options dict — a `**kwargs`
                    signature states nothing a caller can act on.
     :before-fn :after-fn :on-error-fn :ticker-fn

   Observed tool functions return canonical internal envelope maps. The
   wrapper records the envelope, then returns only its payload to Python; failure
   envelopes are converted into thrown ex-info so Python reports normal errors.

   `:doc` and `:arglists` ALWAYS come from var metadata — the previous
   test-only `(symbol sym-name f opts)` 3-arg form is RETIRED. Tests
   that want to register an inline fn must `defn` it first and pass
   `#'the-fn`.

   See `docs/src/extensions/hooks.md` for hook semantics."
  ([v] (symbol v nil))
  ([v opts]
   (when-not (var? v)
     (anomaly/incorrect!
       "vis/symbol expects a Clojure var (e.g. #'my-tool); inline fns must be `defn`'d first and passed by var."
       {:type :extension/symbol-not-a-var :given v}))
   (let [{default-symbol :symbol :keys [doc arglists source]}
         (var-meta v true opts)

         sym
         (or (:symbol opts) default-symbol)

         f
         @v]

     (when-not (fn? f)
       (anomaly/incorrect!
         (str "Var " v " does not hold a function; use vis/value for plain values.")
         {:type :extension/symbol-not-a-fn :var v}))
     (build-symbol-entry {:symbol sym :fn f :doc doc :arglists arglists :source source} opts))))

(defn symbol-entry
  "Build a symbol entry from ALREADY-RESOLVED parts instead of a Clojure var.

   The constructor for hosts that have no var to point at — the Python
   extension bridge, where `:fn`, `:doc` and `:arglists` are derived from a
   Python function. `parts` is `{:symbol :fn :doc :arglists}` (plus optional
   `:source`) and `opts` is EXACTLY the `symbol` opts map, so a Python-declared
   symbol runs the SAME validation and can never skip the `:description` /
   `:result` contract."
  [parts opts]
  (build-symbol-entry parts opts))

(defn helper
  "Build a raw callable helper entry FROM A CLOJURE VAR.

   Helpers are bound as plain values in Python, not observed tools: no envelope
   validation, no channel renderer. Use for composable host helper functions
   such as `snapshot`, not for user-observable tool calls."
  ([v] (helper v nil))
  ([v opts]
   (if (var? v)
     (let [{default-symbol :symbol :keys [doc arglists source]}
           (var-meta v true (assoc opts :raw? true))

           sym
           (or (:symbol opts) default-symbol)

           val
           @v]

       (when-not (fn? val)
         (anomaly/incorrect!
           (str "Var " v " does not hold a function; use vis/value for plain values.")
           {:type :extension/helper-not-a-fn :var v}))
       (validate-symbol-entry!
         (cond-> #:ext.symbol{:symbol sym :val val :doc doc :arglists arglists}
           source
           (assoc :ext.symbol/source source))))
     (anomaly/incorrect! "vis/helper expects a Clojure var (e.g. #'my-helper)."
                         {:type :extension/helper-not-a-var :given v}))))

(defn value
  "Build a value symbol entry FROM A CLOJURE VAR - a plain constant/data binding.

   The var supplies `:symbol` (var name), `:val` (the var's value, unless `:val`
   is provided in opts to override - used by macro-shim entries), and `:doc`
   (from var metadata, i.e. the defn's docstring).

   (def ^{:doc \"Maximum retry attempts.\"} max-retries 3)
   (vis/value #'max-retries)

   Opts:
     :symbol - override the Python sandbox name (default: var name).
     :val - explicit value override (rare; for macro shims that bind a
            marker map instead of the var's own value)."
  ([v] (value v nil))
  ([v opts-or-val]
   (if (var? v)
     (let [opts
           opts-or-val

           {default-symbol :symbol :keys [doc source]}
           (var-meta v false opts)

           sym
           (or (:symbol opts) default-symbol)

           val
           (if (contains? opts :val) (:val opts) @v)

           entry
           (cond-> #:ext.symbol{:symbol sym :val val :doc doc}
             source
             (assoc :ext.symbol/source source))]

       (validate-symbol-entry! entry))
     (anomaly/incorrect!
       "vis/value expects a Clojure var (e.g. #'my-const); use the 3-arg form (value sym-name val opts) for test-only direct construction."
       {:type :extension/value-not-a-var :given v})))
  ([sym-name val opts]
   ;; Test-only direct-construction arity. `:doc` comes from opts.
   (let [doc (:doc opts)]
     (when-not (util/non-blank-string? doc)
       (anomaly/incorrect! (str "3-arg value '" sym-name "' missing :doc in opts.")
                           {:type :extension/missing-doc :symbol sym-name}))
     (validate-symbol-entry! #:ext.symbol{:symbol sym-name :val val :doc doc}))))

(defn- arglist->call-form
  [alias-sym sym-name arglist]
  (let [args
        (->> arglist
             (remove #{'&})
             (map str)
             (str/join " "))

        target
        (if alias-sym (str alias-sym "/" sym-name) (str sym-name))]

    (str "(" target (when (seq args) (str " " args)) ")")))

(defn- render-symbol-line
  [alias-sym entry]
  (let [{sym-name :ext.symbol/symbol doc :ext.symbol/doc arglists :ext.symbol/arglists}
        entry

        callable?
        (or (:ext.symbol/fn entry) (and (fn? (:ext.symbol/val entry)) (seq arglists)))]

    (if callable?
      (str "- " (str/join " or " (map #(arglist->call-form alias-sym sym-name %) arglists))
           " - " doc)
      (str "- " (if alias-sym (str alias-sym "/" sym-name) (str sym-name)) " - " doc))))

(defn- prompt-line-indent [line] (count (or (re-find #"^[ \t]*" line) "")))

(defn- trim-prompt-edge
  [lines]
  (->> lines
       (drop-while str/blank?)
       reverse
       (drop-while str/blank?)
       reverse
       vec))

(defn normalize-prompt-text
  "Normalize model-facing prompt text.

   Removes source indentation from multiline literals, trims leading/trailing
   blank lines, trims trailing horizontal whitespace, and collapses runs of
   blank lines to a single blank line."
  [text]
  (when (string? text)
    (let [lines
          (->> (str/split-lines (str/replace (str/replace text "\r\n" "\n") "\r" "\n"))
               (mapv #(str/replace % #"[ \t]+$" ""))
               trim-prompt-edge)

          indent
          (if-let [xs (seq (remove str/blank? lines))]
            (apply min (map prompt-line-indent xs))
            0)

          deindented
          (mapv (fn [line]
                  (let [indent
                        (long indent)

                        c
                        (long (count line))]

                    (if (str/blank? line) "" (subs line (min indent c)))))
                lines)

          collapsed
          (reduce (fn [acc line]
                    (if (str/blank? line) (if (= "" (peek acc)) acc (conj acc "")) (conj acc line)))
                  []
                  deindented)]

      (str/join "\n" collapsed))))

(defn render-prompt
  "Render canonical `:ext/prompt-fn` text for an extension's symbols.

   A prompt fragment states ROUTING and POLICY only: when this extension is the
   right approach, and what it refuses. It NEVER restates a signature, an
   argument name, a return shape or an example call — that text is the symbol's
   own `:ext.symbol/description`, reached on demand with `doc(name)`. A fragment
   is pushed into EVERY request; a docstring is pulled once, so a signature
   copied up here is paid for on every turn and drifts from the one that runs.

   Accepts an extension map or any map with:
   - :ext/description      or :heading
   - :ext.engine/alias optional {:alias 'v}
   - :ext.engine/symbols  vector of symbol + value entries
   - :usage-note   optional extra note added to the heading
   - :notes        optional string or seq of extra lines appended verbatim

   Returns a prompt string suitable for :ext/prompt-fn."
  [{:keys [heading usage-note notes] :as opts}]
  (let [alias-sym
        (ext-alias-symbol opts)

        symbols
        (or (:symbols opts) (ext-symbols opts))

        heading
        (or heading (:ext/description opts) "Extension tools")

        header-notes
        (vec (remove nil?
               [(when alias-sym (str "use " alias-sym "/ prefix"))
                (when (util/non-blank-string? usage-note) usage-note)]))

        extra-lines
        (cond (nil? notes) []
              (string? notes) [notes]
              (sequential? notes) (vec notes)
              :else [(str notes)])

        body-lines
        (mapv #(render-symbol-line alias-sym %) symbols)]

    (normalize-prompt-text (str/join "\n"
                                     (concat [(str heading
                                                   (when (seq header-notes)
                                                     (str " (" (str/join "; " header-notes) ")")))]
                                             body-lines
                                             extra-lines)))))

;; Normalization + validation
(defn- normalize-prompt
  [prompt]
  (cond (nil? prompt) nil
        (fn? prompt) (fn [env]
                       (let [result (prompt env)]
                         (if (string? result) (normalize-prompt-text result) result)))
        (string? prompt) (constantly (normalize-prompt-text prompt))
        :else (throw (ex-info ":ext/prompt-fn must be a string or (fn [env] string)"
                              {:got (type prompt)}))))

(defn- extension-symbol-op-keyword
  [ext sym-entry]
  (keyword (tool-call-name ext (:ext.symbol/symbol sym-entry))))

(defn- validate-symbol-op-tags!
  "Fail closed: every observed extension tool MUST carry an inline
   `:tag :observation | :mutation` on its `vis/symbol` opts map.
   Raw helpers (`:raw? true`) are exempt. `register-extension!`
   walks the symbol vec at registration time and populates the
   global op-keyword -> tag index automatically."
  [ext]
  (doseq [sym-entry
          (ext-symbols ext)

          :when (and (:ext.symbol/fn sym-entry) (not (:ext.symbol/raw? sym-entry)))]

    (let [op (extension-symbol-op-keyword ext sym-entry)]
      (when-not (:ext.symbol/tag sym-entry)
        (anomaly/incorrect! (str "Extension '"
                                 (:ext/name ext)
                                 "' symbol '"
                                 (:ext.symbol/symbol sym-entry)
                                 "' is missing mandatory `:tag` on "
                                 "its (vis/symbol ...) opts map. Declare `:tag :observation` "
                                 "or `:tag :mutation` inline. (op-keyword for reference: "
                                 (pr-str op)
                                 ".)")
                            {:type :extension/missing-op-tag
                             :extension (:ext/name ext)
                             :symbol (:ext.symbol/symbol sym-entry)
                             :op op
                             :allowed op-tags}))))
  ext)

(defn validate!
  "Normalize and validate an extension declaration.
   String prompt contributions become functions before ordinary predicate validation."
  [ext]
  (when (contains? ext :ext/environment-prompt-fn)
    (throw
      (ex-info
        ":ext/environment-prompt-fn was removed; put model-facing environment text in :ext/prompt-fn"
        {:type :extension/retired-environment-prompt-fn :name (:ext/name ext)})))
  (let [ext (cond-> ext
              (contains? ext :ext/prompt-fn)
              (update :ext/prompt-fn normalize-prompt))]
    (when-not (extension? ext)
      (throw (ex-info (str "Invalid extension '" (:ext/name ext) "'")
                      {:type :extension/invalid-declaration :name (:ext/name ext) :extension ext})))
    (validate-symbol-op-tags! ext)))

;; Hook execution - runtime wrappers with output validation + logging
(defn- validate-hook-return!
  [hook-name sym returned]
  (when-not (map? returned)
    (throw (ex-info (str hook-name " for '" sym "' must return a map, got: " (type returned))
                    {:type (keyword "extension" (str hook-name "-error"))
                     :symbol sym
                     :returned returned}))))

(defn- call-hook
  [hook-name sym hook-fn hook-args]
  (try (apply hook-fn hook-args)
       (catch clojure.lang.ArityException e
         (throw (ex-info (str hook-name " for '" sym "' has wrong arity: " (ex-message e))
                         {:type (keyword "extension" (str hook-name "-error")) :symbol sym}
                         e)))
       (catch Throwable e
         (throw (ex-info (str hook-name " for '" sym "' threw: " (ex-message e))
                         {:type (keyword "extension" (str hook-name "-error")) :symbol sym}
                         e)))))

(defn- elapsed-ms [^long t0] (/ (double (- (System/nanoTime) t0)) 1e6))

(defn- log-hook!
  [level id ext-ns sym phase ms extra-msg]
  (tel/log! {:level level
             :id id
             :data {:ext ext-ns :symbol sym :phase phase :ms ms}
             :msg (str ext-ns
                       "/"
                       sym
                       " :invoke"
                       (when phase (str " " phase))
                       (when ms (str " " (format "%.1fms" (double ms))))
                       (when extra-msg (str " " extra-msg)))}))

(defn- run-before
  [ext-ns sym-entry env f args]
  (if-let [before (:ext.symbol/before-fn sym-entry)]
    (let [sym (:ext.symbol/symbol sym-entry)
          t0 (System/nanoTime)
          _ (log-hook! :debug ::before-fn ext-ns sym :before-fn nil nil)
          ret (call-hook ":before-fn" sym before [env f args])
          _ (validate-hook-return! ":before-fn" sym ret)
          ms (elapsed-ms t0)]

      (if (contains? ret :result)
        (do (log-hook! :debug ::before-fn-done ext-ns sym :before-fn ms "short-circuited")
            {:result (:result ret)})
        (do (log-hook! :debug ::before-fn-done ext-ns sym :before-fn ms nil)
            {:env (get ret :env env) :fn (get ret :fn f) :args (vec (get ret :args args))})))
    {:env env :fn f :args args}))

(defn- run-after
  [ext-ns sym-entry env f args result]
  (if-let [after (:ext.symbol/after-fn sym-entry)]
    (let [sym (:ext.symbol/symbol sym-entry)
          t0 (System/nanoTime)
          _ (log-hook! :debug ::after-fn ext-ns sym :after-fn nil nil)
          ret (call-hook ":after-fn" sym after [env f args result])
          _ (validate-hook-return! ":after-fn" sym ret)
          ms (elapsed-ms t0)]

      (log-hook! :debug ::after-fn-done ext-ns sym :after-fn ms nil)
      {:env (get ret :env env)
       :fn (get ret :fn f)
       :args (vec (get ret :args args))
       :result (get ret :result result)})
    {:env env :fn f :args args :result result}))

(defn- run-on-error
  [ext-ns sym-entry err env f args]
  (if-let [on-error (:ext.symbol/on-error-fn sym-entry)]
    (let [sym (:ext.symbol/symbol sym-entry)
          t0 (System/nanoTime)
          _ (log-hook! :warn
                       ::on-error-fn
                       ext-ns
                       sym
                       :on-error-fn
                       nil
                       (str "handling: " (ex-message err)))
          ret (try (call-hook ":on-error-fn" sym on-error [err env f args])
                   (catch Throwable e
                     (if (identical? e err)
                       (throw e)
                       (throw (ex-info (str ":on-error-fn for '" sym "' threw: " (ex-message e))
                                       {:type :extension/on-error-fn-error :symbol sym}
                                       e)))))
          _ (validate-hook-return! ":on-error-fn" sym ret)
          ms (elapsed-ms t0)]

      (cond
        (contains? ret :result)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "fallback result") ret)
        (contains? ret :error)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "surfacing error") ret)
        :else (do (log-hook! :info ::on-error-fn-done ext-ns sym :on-error-fn ms "retrying") ret)))
    (throw err)))

(defn- assert-symbol-envelope!
  [sym result]
  (when-not (tool-result? result)
    (throw (ex-info (str "Symbol '" sym "' must return a canonical :envelope map")
                    {:type :extension/invalid-symbol-result :symbol sym :value result})))
  result)

(defn- tool-call-name
  [ext sym]
  (if-let [alias (ext-alias-symbol ext)]
    (str alias "/" sym)
    (str sym)))

(defn- tool-start-label
  "One-line human label for the PRIMARY argument of a tool call, so Activity can
   adapt the evidence to the channel width instead of inheriting a premature visual
   truncation. Best-effort: the first string positional arg, or a common key
   (cmd/path/query/code/name/id) of a leading map arg. The Activity event boundary
   performs the canonical byte bound. nil when nothing sensible.

   A label is DISPLAY: an absolute path under home is rendered `~/…`, the spelling the
   footer, the navigator and the dialogs already use. `abbreviate-home` rewrites nothing
   else, so a command, a query or a path typed relative arrives exactly as it was typed."
  [args]
  (let [primary
        (first args)

        pick
        (cond (string? primary) primary
              (map? primary) (some (fn [k]
                                     (let [v (or (get primary k) (get primary (name k)))]
                                       (when (string? v) v)))
                                   [:cmd :command :path :query :code :name :id])
              :else nil)

        line
        (some-> pick
                str
                str/split-lines
                first
                str/trim)]

    (when-not (str/blank? (or line "")) (paths/abbreviate-home line))))

(defn- tool-start-phrase
  "The tool's OWN live-ticker phrase for this call, completing `Vis is …`. A tool
   declares it with `:ticker-fn`; nothing else supplies one, so the generic
   `<op> <label>` sentence stays the default. Best-effort — a ticker that throws
   or answers blank simply leaves the default in place. First line, 96 chars."
  [sym-entry env args]
  (when-let [f (:ext.symbol/ticker-fn sym-entry)]
    (let [line (try (some-> (f env args)
                            str
                            str/split-lines
                            first
                            str/trim)
                    (catch Throwable _ nil))]
      (when-not (str/blank? (or line ""))
        (if (> (count line) 96) (str (subs line 0 93) "…") line)))))

(defn- default-tool-op-keyword
  [ext sym-entry]
  (keyword (tool-call-name ext (:ext.symbol/symbol sym-entry))))

(defn- ensure-tool-result-op
  "Observed extension tools must carry canonical op metadata. The wrapper
   derives it deterministically from active alias + symbol (`grep`,
   `db/fetch!`, ...). The tag comes from the symbol entry's inline
   `:ext.symbol/tag` (source of truth); a derived op->tag index covers
   call-sites without a sym-entry handle. Missing tag in BOTH places throws
   via `op-tag` so unregistered ops still fail closed."
  [ext sym-entry result]
  (if (and (tool-result? result) (nil? (:symbol result)))
    (let [op
          (default-tool-op-keyword ext sym-entry)

          tag
          (or (:ext.symbol/tag sym-entry) (op-tag op))]

      (assoc result
        :symbol op
        :tag tag))
    result))

(defn- public-op-keyword
  "User-facing op keyword for payload EDN. Tool symbols use `!` for mutation
   (`shell/run!`), but result maps read like porcelain (`:shell/run`)."
  [op]
  (when op
    (let [ns-part
          (namespace op)

          n
          (name op)

          n
          (str/replace n #"!$" "")]

      (if ns-part (keyword ns-part n) (keyword n)))))

(defn- op-kw->str
  "The STRING a tool op-keyword takes on the Python boundary: namespace folded
   with `_`, kebab→snake, trailing `?`/`!` stripped (`grep`→\"grep\",
   `exists?`→\"exists\", `:shell/run!`→\"shell_run\"). The boundary is
   strings-only, so this is applied AT THE STAMP — no keyword ever rides a
   result map."
  [op-kw]
  (let [s (if (namespace op-kw) (str (namespace op-kw) "_" (name op-kw)) (name op-kw))]
    (-> s
        (str/replace "-" "_")
        (str/replace #"[?!]$" ""))))

(defn- stamp-public-result-op
  "Public Python value is the envelope's `:result`, not the envelope. If the
   payload is a map, stamp the canonical tool op — as the STRING key `\"op\"`
   with a STRING value (strings-only boundary) — so extension implementations
   do not hand-maintain it. Tool-specific operation details must use a
   different key (`\"edit_op\"`, `\"action\"`, etc.)."
  [result]
  (if (and (tool-result? result) (:success? result) (:symbol result) (map? (:result result)))
    (update result :result assoc "op" (op-kw->str (public-op-keyword (:symbol result))))
    result))

(defn- enrich-tool-result-info
  "Stamp the MINIMAL tool identity on a result's metadata: symbol, call
   name, alias, and the owning extension NAME (one short string —
   `tool-result-symbol-entry` resolves the registry entry from it).
   The full extension descriptor (license/author/description/version/
   owner/registry-id) and the source forensics (paths/mtime/sha256)
   were stamped PER CALL and persisted with every form envelope — pure
   DB bloat with zero readers; `extension-info` still serves the ctx
   `:extensions` digest and `vis-agent extension list` from the registry."
  [ext sym-entry result]
  (if (tool-result? result)
    (merge-into-metadata (stamp-public-result-op (ensure-tool-result-op ext sym-entry result))
                         {:tool (cond-> {:symbol (:ext.symbol/symbol sym-entry)
                                         :call (tool-call-name ext (:ext.symbol/symbol sym-entry))
                                         :ext (:ext/name ext)}
                                  (ext-alias-symbol ext)
                                  (assoc :alias (ext-alias-symbol ext)))})
    result))

(def ^:dynamic *current-extension*
  "Extension map currently executing on an extension callback thread.
   Bound by symbol wrappers so extension-owned helper APIs can fill the
   caller's stable extension identity without accepting user-supplied ids."
  nil)

(def ^:dynamic *current-symbol*
  "Sandbox symbol currently executing, when a symbol callback is active."
  nil)

(def ^:dynamic *current-environment*
  "Live session environment for the extension callback currently executing."
  nil)

(defn current-extension-id
  []
  (some-> *current-extension*
          :ext/name))

(defmacro with-context
  "Install THE extension context around `body` and evaluate it.

   There is exactly ONE context, and it is the session `environment` map.
   Everything else an extension callback can read ambiently is DERIVED from it
   here — the extension identity (`*current-extension*`, `*current-symbol*`)
   and the workspace view (`workspace/*workspace-root*`,
   `workspace/*filesystem-roots*`) — so a callback can never see half a
   session. EVERY extension callback site enters through this macro (symbol
   calls, turn hooks, ctx / prompt / activation callbacks, Python adapters);
   nothing else binds those vars. Sites that bound only a subset used to hand
   `vis.ask` / `vis.shell` a session-less environment.

   Opts `{:ext ext :symbol sym :env environment}`: a nil or EMPTY `:env`, and a
   nil `:ext`, INHERIT the ambient one, so a nested callback stays in its
   caller's session and a caller that hands a hook no environment at all cannot
   silently drop the session out from under it. `:symbol` is never inherited —
   only a symbol call has one."
  [{:keys [ext env] sym :symbol} & body]
  `(let [env# (or (not-empty ~env) *current-environment*)]
     (binding [*current-extension* (or ~ext *current-extension*)
               *current-symbol* ~sym
               *current-environment* env#
               workspace/*workspace-root* (workspace/workspace-root env#)
               workspace/*filesystem-roots* (workspace/env-filesystem-roots env#)]

       ~@body)))

(defn session-provider-kickoff-llm-headers
  "Run active extensions' `:session_provider_kickoff` hooks for `provider`.

   A hook receives `{:phase :session_provider_kickoff :environment environment
   :provider provider}` and returns nil or
   `{:llm-headers {nonblank-string nonblank-string}}`. Contributions merge in
   registry order, with later hooks owning the same header. Hook exceptions and
   malformed returns fail the kickoff so required transport metadata is never
   silently omitted."
  [environment active-extensions provider]
  (reduce
    (fn [headers ext]
      (reduce
        (fn [acc {:keys [id phase] hook-fn :fn}]
          (if (= :session_provider_kickoff phase)
            (let [contribution
                  (try (with-context {:ext ext :env environment}
                                     (hook-fn {:phase :session_provider_kickoff
                                               :environment environment
                                               :provider provider}))
                       (catch Throwable t
                         (throw (ex-info (str "Extension session provider kickoff hook failed: "
                                              (:ext/name ext)
                                              "/" id)
                                         {:type :extension/session-provider-kickoff-hook-failed
                                          :extension (:ext/name ext)
                                          :hook id
                                          :phase :session_provider_kickoff
                                          :provider-id (:id provider)}
                                         t))))]
              (cond (nil? contribution) acc
                    (not (session-provider-kickoff-headers? contribution))
                    (throw (ex-info
                             (str
                               "Extension session provider kickoff hook returned invalid headers: "
                               (:ext/name ext)
                               "/" id)
                             {:type :extension/invalid-session-provider-kickoff-hook-return
                              :extension (:ext/name ext)
                              :hook id
                              :phase :session_provider_kickoff
                              :provider-id (:id provider)}))
                    :else (merge acc (:llm-headers contribution))))
            acc))
        headers
        (or (:ext/hooks ext) [])))
    {}
    (or active-extensions [])))

(defn- deep-merge
  [& maps]
  (letfn [(merge-entry [a b] (if (and (map? a) (map? b)) (merge-with merge-entry a b) b))]
    (apply merge-with merge-entry maps)))

(defn ctx-contributions
  "Return merged structured `ctx` contributions for active extensions.

   Each active extension may declare `:ext/ctx-fn` as `(fn [env] -> map)`.
   The contribution CONTRACT is STRING-KEYED: top-level keys are the
   folded `session_*` strings (`\"session_env\"`, `\"session_workspace\"`, ...)
   and values are string-keyed all the way down — the merged map crosses the
   Python boundary as the model's `session` dict, which throws on any keyword
   key/value. This fn only aggregates (deep-merge); producers own the keys.
   Exceptions and non-map returns are logged and ignored so bad optional
   context never blocks a turn."
  [environment active-extensions]
  (reduce (fn [acc ext]
            (if-let [f (:ext/ctx-fn ext)]
              (let [contribution (try (with-context {:ext ext :env environment} (f environment))
                                      (catch Throwable t
                                        (tel/log! {:level :warn
                                                   :id ::ctx-contribution-error
                                                   :data {:ext (:ext/name ext)
                                                          :error (ex-message t)}}
                                                  "Extension :ext/ctx-fn fn threw")
                                        nil))]
                (if (map? contribution)
                  (deep-merge acc contribution)
                  (do (when (some? contribution)
                        (tel/log! {:level :warn
                                   :id ::ctx-contribution-invalid
                                   :data {:ext (:ext/name ext) :returned (type contribution)}}
                                  "Extension :ext/ctx-fn fn returned non-map"))
                      acc)))
              acc))
          {}
          (or active-extensions [])))

(defn- tool-result->public-value
  [result]
  (if (:success? result)
    (:result result)
    (let [err (:error result)]
      ;; ex-data carries the structured `:error` ONLY — never the whole
      ;; failure envelope. The envelope nests the same `:error` (message +
      ;; trace) again, so embedding it doubled every byte of the failure in
      ;; whatever pr-strs this exception downstream; the full envelope is
      ;; already persisted via the sink entry for anyone who needs fidelity.
      (throw (ex-info (or (:message err) "Tool failed")
                      {:type :vis/tool-failure :symbol (:symbol result) :error err})))))

(def ^:private ^:dynamic *tool-result-observer*
  "Receives the canonical envelope immediately before its payload crosses to
   Python. Activity binds this only while observing a tool call, so semantic
   metadata such as a patch diff can become evidence without changing the value
   the extension receives."
  nil)

(defn- observed-public-value
  [result]
  (when *tool-result-observer* (*tool-result-observer* result))
  (tool-result->public-value result))

;; ── GATE ops ────────────────────────────────────────────────────────────────
;; A gate is the one op shape that is ASKED rather than wrapped. Every hook
;; registered for it is consulted in registration order, none of them receives
;; `next`, and the FIRST refusal stands — so no extension can loosen another
;; extension's boundary, which is the property a cross-extension precedence rule
;; used to buy. A gate hook that THROWS refuses (fail CLOSED); every other hook
;; keeps failing open, because a broken convenience guard must not brick the loop
;; while a broken boundary must not open it.

(def gate-ops
  "Gate ops, keyed by op keyword and valued by the spelling a hook author writes
   (`vis.op_hook([\"fs_access\"], guard)` in Python, `{:op :fs/access}` in Clojure).

   `:fs/access` is asked with `{:operation :path}` for every path the host file
   tools touch — `cat`, `grep`, `patch`, `ls` (`foundation/editing/core`). It does
   not reach the Python interpreter: `open(p, 'w')` inside a block is bounded by
   the sandbox roots, not by a gate hook."
  {:fs/access "fs_access"})

(defn gate-op
  "The gate op keyword `s` names, or nil when it names no gate. Both spellings
   resolve: the author's `\"fs_access\"` and the engine's `:fs/access`."
  [s]
  (let [n (if (keyword? s) (subs (str s) 1) (str s))]
    (some (fn [[op spelling]]
            (when (or (= n spelling) (= n (subs (str op) 1))) op))
          gate-ops)))

;; ── Cross-cutting operation hooks ────────────────────────────────────────────
;; A generic, op-keyword-keyed hook registry so ANY extension can decorate an
;; operation it does NOT own — e.g. the Clojure pack repairs `.clj` files after
;; the foundation's `patch`. Distinct from a symbol's own
;; `:before-fn`/`:after-fn` (which only its DEFINER can set): these compose ON TOP
;; at the single `invoke-symbol-wrapper` chokepoint, so the hook is wired ONCE and
;; applies wherever that op is called. Before/after hooks are best-effort;
;; :around hooks are control flow, so their return values and exceptions
;; propagate to the operation caller.
(defonce ^:private op-hooks (atom {}))   ; op-keyword -> [{:phase :owner :fn}]

(defn register-op-hook!
  "Register a cross-cutting hook on operation `:op` (its op-keyword, e.g.
   :patch). `:phase` is :after (default — sees & may rewrite the result
   envelope), :before (sees & may rewrite the args vector), :around (MIDDLEWARE
   — wraps the call), or :gate (the op is ASKED, never wrapped — see `gate-ops`;
   a gate op forces this phase whatever the caller declared, because the op
   decides the shape). `:fn` is, for :after, (fn [env op-kw args result] ->
   result-envelope); for :before, (fn [env op-kw args] -> args-vector); for
   :around, (fn [env op-kw args next] -> result) where `next` runs the inner call
   and may be invoked zero+ times (skip / retry) or wrapped in try/catch (recover
   — this is how an op is made NOT to fail); for :gate, (fn [env op-kw ctx] ->
   nil | reason). `:owner` (an ext keyword) makes the registration idempotent
   across `:reload`s — re-registering the same owner+phase for an op REPLACES the
   prior one. Returns the op-keyword."
  [{:keys [op phase owner] hook-fn :fn :or {phase :after}}]
  (assert (#{:before :around :after :gate} phase)
          "op hook :phase must be :before, :around, :after, or :gate")
  (assert (ifn? hook-fn) "op hook :fn must be a function")
  (let [gate-kw
        (gate-op op)

        op-kw
        (or gate-kw (keyword op))

        phase
        (if gate-kw :gate phase)]

    (swap! op-hooks update
      op-kw
      (fn [hooks]
        (conj (vec (remove #(and owner (= owner (:owner %)) (= phase (:phase %))) hooks))
              {:phase phase :owner owner :fn hook-fn})))
    op-kw))

(defn unregister-op-hooks-for-owner!
  "Remove EVERY op-hook registered by `owner` (all ops + phases). Driven by
   `deregister-extension!` so an extension's hooks die with it; also callable
   directly to dynamically tear an extension's hooks down."
  [owner]
  (swap! op-hooks (fn [m]
                    (reduce-kv (fn [acc op hooks]
                                 (let [kept (vec (remove #(= owner (:owner %)) hooks))]
                                   (if (seq kept) (assoc acc op kept) acc)))
                               {}
                               m)))
  owner)

(defn- ext-op-hook-owner
  "Owner keyword an extension's declarative `:ext/op-hooks` register under,
   derived from its name (e.g. \"language-clojure\" -> :ext/language-clojure)."
  [ext]
  (keyword "ext" (str (:ext/name ext))))

(defn- install-op-hooks!
  "Register an extension's declarative `:ext/op-hooks` under its derived owner —
   idempotent on reload (replaces same owner+phase per op)."
  [ext]
  (let [owner (ext-op-hook-owner ext)]
    (doseq [h (:ext/op-hooks ext)]
      (register-op-hook! (assoc h :owner owner)))))

(defn- install-egress-filters!
  "Register an extension's declarative `:ext/network-filters` into the egress
   proxy under its derived owner — idempotent on reload (owner replaced first)."
  [ext]
  (let [owner (ext-op-hook-owner ext)]
    (egress-proxy/unregister-network-filters-for-owner! owner)
    (doseq [f (:ext/network-filters ext)]
      (egress-proxy/register-network-filter! owner f))))

(defn- run-op-before-hooks
  "Thread `args` through every :before hook registered for `op-kw`."
  [op-kw env args]
  (reduce (fn [as {:keys [phase owner] hook-fn :fn}]
            (if (= :before phase)
              (try (let [r (hook-fn env op-kw as)]
                     (if (sequential? r) (vec r) as))
                   (catch Throwable e
                     (log-hook! :warn
                                ::op-hook-threw
                                nil
                                op-kw
                                :before-op-hook
                                nil
                                (str (or owner "?") ": " (ex-message e)))
                     as))
              as))
          (vec args)
          (get @op-hooks op-kw)))

(defn- run-op-after-hooks
  "Thread `result` through every :after hook registered for `op-kw`."
  [op-kw env args result]
  (reduce (fn [res {:keys [phase owner] hook-fn :fn}]
            (if (= :after phase)
              (try (let [r (hook-fn env op-kw args res)]
                     (if (tool-result? r) r res))
                   (catch Throwable e
                     (log-hook! :warn
                                ::op-hook-threw
                                nil
                                op-kw
                                :after-op-hook
                                nil
                                (str (or owner "?") ": " (ex-message e)))
                     res))
              res))
          result
          (get @op-hooks op-kw)))

(defn- run-op-around
  "MIDDLEWARE: wrap the actual op fn with every :around hook for `op-kw`. Each
   hook is (fn [env op-kw args next] -> result) where `next` = (fn [args] ->
   result) runs the inner call. A hook may call `next` zero or more times (skip,
   retry with rewritten args), catch its throw and recover, or substitute a
   result outright — this is how an extension makes an op it doesn't own NOT fail
   (e.g. the Clojure pack paren-repairs + retries a `patch`). Hooks compose;
   with none registered this is just `(apply f args)`."
  [op-kw env f args]
  (let [arounds
        (filter #(= :around (:phase %)) (get @op-hooks op-kw))

        base
        (fn [as]
          (apply f as))]

    (if (empty? arounds)
      (base args)
      ((reduce (fn [nxt {hook-fn :fn}]
                 (fn [as]
                   (hook-fn env op-kw as nxt)))
               base
               arounds)
        args))))

(def ^:dynamic ^:private *in-gate*
  "True while a gate hook is running on THIS thread. A guard that reads a file in
   order to decide re-enters the gate; skipping it there is what keeps the guard
   from recursing into itself."
  false)

(defn- gate-refusal
  "Normalize a gate hook's answer: nil — or anything that is not a sentence —
   ALLOWS; a non-blank string, or a map carrying `:reason`/`:hint`, REFUSES with
   it."
  [answer]
  (cond (string? answer) (when-not (str/blank? answer) {:reason answer})
        (map? answer) (let [reason (or (:reason answer) (:hint answer))]
                        (when (util/non-blank-string? reason) (assoc answer :reason reason)))
        :else nil))

(defn gate-hooked?
  "Whether any gate hook is registered for `op-kw` — the short-circuit that keeps
   an engine with no guard installed paying one map lookup per operation."
  [op-kw]
  (boolean (some #(= :gate (:phase %)) (get @op-hooks op-kw))))

(defn run-gate-hooks
  "Ask the gate hooks registered for `op-kw` whether the operation `ctx` describes
   may proceed. Each hook is `(fn [env op-kw ctx] -> nil | reason)`. Returns nil to
   ALLOW, or `{:reason <sentence> :owner <ext>}` to REFUSE — see `gate-ops` for the
   contract the mechanism carries so the guard author does not have to."
  [op-kw env ctx]
  (when-not *in-gate*
    (let [gates (filterv #(= :gate (:phase %)) (get @op-hooks op-kw))]
      (when (seq gates)
        (binding [*in-gate* true]
          (some (fn [{:keys [owner] hook-fn :fn}]
                  (some-> (try (gate-refusal (hook-fn env op-kw ctx))
                               (catch Throwable e
                                 {:reason (str "the " (or owner "?")
                                               " guard failed, and a boundary fails closed: "
                                               (ex-message e))}))
                          (assoc :owner owner)))
                gates))))))

(defn invoke-operation
  "Invoke host operation `f` through the declarative :around hooks for
   `op-kw`. This is the non-model-tool entry point for operations such as a TUI
   Git commit; hook lifecycle remains owned by extension registration."
  [op-kw env f args]
  (run-op-around (keyword op-kw) env f (vec args)))

(defn- folded-kwargs->positional
  "Re-expand a folded kwargs dict for the DIRECT-python surface. When the agent
   calls a symbol in a `python_execution` block with ALL-KEYWORD args
   (`tool(id=…, n=…)`), CPython folds those kwargs into ONE trailing
   dict positional (see `__vis_exec_call__` in `env-python`). A fixed-arity impl
   `[env id n]` would then receive the whole `{id n}` map in its `id` slot.
   Re-expand that lone map
   into the positional args the symbol's `:call` SHAPE describes, so keyword and
   positional calls bind IDENTICALLY.

   Fires ONLY for the unambiguous folded-kwargs case: a MAP `:call` shape, a
   single NON-empty map arg with all-string keys, every required `:pos` key
   present, and no undeclared leftover keys unless the shape opts into `:rest`.
   Everything else — positional calls, function-valued shapes, tools with no
   `:call`, genuine single-map positionals — passes through untouched."
  [shape args]
  (if (and (map? shape)
           (= 1 (count args))
           (map? (first args))
           (seq (first args))
           (every? string? (keys (first args))))
    (let [m
          (first args)

          lead
          (:lead-opt shape)

          pos
          (vec (:pos shape))

          opt-pos
          (vec (:opt-pos shape))

          rest-mode
          (:rest shape)

          opt-present
          (take-while #(contains? m %) opt-pos)

          consumed
          (cond-> (set pos)
            lead
            (conj lead)

            (seq opt-present)
            (into opt-present))

          leftover
          (apply dissoc m consumed)]

      (if (and (or lead (seq pos) (seq opt-pos))
               (every? #(contains? m %) pos)
               (or rest-mode (empty? leftover)))
        (vec (concat (when (and lead (contains? m lead)) [(get m lead)])
                     (map #(get m %) pos)
                     (map #(get m %) opt-present)
                     (when (and rest-mode (or (= rest-mode :always) (seq leftover))) [leftover])))
        args))
    args))

(defn- invoke-symbol-wrapper*
  "Full invocation pipeline for an observed tool symbol entry:
   before-fn -> fn -> after-fn, with on-error-fn catching :fn errors.

   Every hook can override :fn, :args, :env via its return map.
   :before-fn can return {:result val} to short-circuit.
   :on-error-fn can return {:result val}, {:error err}, or {:fn :args :env} to retry.

   The implementation's final value must be a canonical internal envelope.
   The wrapper records channel/provenance from that envelope, then
   returns only the payload `:result` to Python. Failure envelopes are converted
   into thrown ex-info so ordinary Python error reporting handles them.

   Raw helper symbols (`:ext.symbol/raw? true`) bypass this function entirely."
  [ext sym-entry args env]
  (with-context
    {:ext ext :symbol (:ext.symbol/symbol sym-entry) :env env}
    (let [sym
          (:ext.symbol/symbol sym-entry)

          ext-ns
          (:ext/name ext)

          op-kw
          (keyword (tool-call-name ext sym))

          _original-args
          args

          args
          (folded-kwargs->positional (:ext.symbol/call sym-entry) args)

          t0
          (System/nanoTime)

          _
          (log-hook! :debug ::invoke ext-ns sym nil nil nil)

          before-out
          (run-before ext-ns sym-entry env (:ext.symbol/fn sym-entry) args)]

      (if (contains? before-out :result)
        (let [ms
              (elapsed-ms t0)

              result
              (->> (:result before-out)
                   (enrich-tool-result-info ext sym-entry)
                   (assert-symbol-envelope! sym))]

          (log-hook! :debug ::invoke-done ext-ns sym nil ms "short-circuited")
          (observed-public-value result))
        (let [{call-env :env f :fn call-args :args}
              before-out

              ;; :inject-env? prepends the live env as the first arg — decoupled
              ;; from before-fn, which is now a pure hook (not a gate / injector).
              call-args
              (if (:ext.symbol/inject-env? sym-entry) (into [call-env] call-args) call-args)

              call-args
              (run-op-before-hooks op-kw call-env call-args)

              call-result
              (let [ct0 (System/nanoTime)]
                (try (let [r (invoke-operation op-kw call-env f call-args)
                           ms (elapsed-ms ct0)]

                       (log-hook! :debug ::fn-returned ext-ns sym :call ms nil)
                       {:result r})
                     (catch Throwable e
                       (let [ms (elapsed-ms ct0)]
                         (log-hook! :warn ::fn-threw ext-ns sym :call ms (ex-message e))
                         (try (let [recovery (run-on-error ext-ns sym-entry e call-env f call-args)]
                                (cond (contains? recovery :result) recovery
                                      (contains? recovery :error) (throw (:error recovery))
                                      :else {:result (apply (get recovery :fn f)
                                                       (vec (get recovery :args call-args)))}))
                              (catch Throwable e2 (throw e2)))))))

              {:keys [result]}
              (run-after ext-ns sym-entry call-env f call-args (:result call-result))

              result
              (run-op-after-hooks op-kw call-env call-args result)

              result
              (->> result
                   (enrich-tool-result-info ext sym-entry)
                   (assert-symbol-envelope! sym))

              ms
              (elapsed-ms t0)]

          (log-hook! :debug ::invoke-done ext-ns sym nil ms nil)
          (observed-public-value result))))))

(defn invoke-symbol-wrapper
  "Run one observed tool invocation and emit paired Activity lifecycle events.

   Identity and wrapper-entry order are allocated before hooks. The terminal is
   emitted only after recovery, hooks, envelope validation, and public-value
   conversion determine exactly what Python receives. Nested wrappers inherit the
   actual parent invocation id; concurrent terminal order follows observation."
  [ext sym-entry args env]
  (if-not *tool-event-sink*
    (invoke-symbol-wrapper* ext sym-entry args env)
    (let [ctx
          (or *tool-event-context* (activity-event/context))

          invocation
          (activity-event/invocation ctx *current-invocation-id*)

          sym
          (:ext.symbol/symbol sym-entry)

          operation
          (keyword (tool-call-name ext sym))

          presenter
          (or (:ext.symbol/presenter sym-entry) :generic)

          started-at-ms
          (util/now-ms)

          details
          {:operation operation
           :presenter presenter
           :classification (:ext.symbol/tag sym-entry)
           :extension (:ext/name ext)
           :symbol sym
           :label (tool-start-label args)
           :phrase (tool-start-phrase sym-entry env args)
           :args args}]

      (record-tool-event! (activity-event/start-event ctx invocation details))
      (binding [*tool-event-context*
                ctx

                *current-invocation-id*
                (:invocation-id invocation)]

        (try (let [envelope
                   (volatile! nil)

                   value
                   (binding [*tool-result-observer* #(vreset! envelope %)]
                     (invoke-symbol-wrapper* ext sym-entry args env))]

               (record-tool-event! (activity-event/terminal-event ctx
                                                                  invocation
                                                                  (assoc details
                                                                    :started-at-ms started-at-ms
                                                                    :outcome :succeeded
                                                                    :result value
                                                                    :result-envelope @envelope)))
               value)
             (catch Throwable t
               (record-tool-event! (activity-event/terminal-event
                                     ctx
                                     invocation
                                     (assoc details
                                       :started-at-ms started-at-ms
                                       :outcome
                                       (if (cancellation/cancellation? t) :cancelled :failed)
                                       :error t)))
               (throw t)))))))

(def ^:private ^:dynamic *log-writer*
  "Writer that sends output to the log file instead of stdout/stderr.
   Bound during extension invocations so tool fns never bleed into the TUI."
  nil)

(defn- get-log-writer
  []
  (or *log-writer*
      ;; Per-process file: Telemere rotates the log by renaming it, so a path
      ;; shared with another vis process orphans this writer's fd.
      (let [log-path (paths/log-file)]
        (alter-var-root #'*log-writer*
                        (fn [cur]
                          (or cur
                              ;; Diagnostics sink ONLY — an unwritable log path
                              ;; (confined process, read-only $HOME) must NEVER
                              ;; fail the tool invocation it wraps: fall back to
                              ;; a null writer and keep the call running.
                              (try (io/writer log-path :append true)
                                   (catch Throwable _
                                     (io/writer (java.io.OutputStream/nullOutputStream)))))))
        *log-writer*)))

(declare wrap-extension-thunked)

(defn wrap-extension
  "Wrap all function symbols in an extension into invocation fns.

   Returns a map of {sym -> (fn [& args] result)} where each fn
   closes over the extension, symbol entry, and environment, then
   routes through `invoke-symbol-wrapper`.

   All stdout/stderr from extension calls is redirected to the log
   file so nothing bleeds into the TUI.

   Value symbols are returned as {sym -> value}.

   Returns every extension symbol."
  [ext env]
  (wrap-extension-thunked ext (constantly env)))

(defn wrap-extension-thunked
  "Like `wrap-extension` but resolves the environment LAZILY via `env-thunk`
   (a 0-arg fn) at CALL time instead of closing over a concrete `env`. Interns
   BUILT-IN extension symbols into the sandbox at Python-context creation —
   BEFORE the environment map exists — mirroring how `doc`/`apropos` defer
   through `environment-atom`. Same wrapping/IO-redirect as `wrap-extension`."
  [ext env-thunk]
  (let [entries (ext-symbols ext)]
    (into {}
          (map
            (fn [sym-entry]
              (let [sym (:ext.symbol/symbol sym-entry)]
                (if (contains? sym-entry :ext.symbol/fn)
                  [sym
                   (if (:ext.symbol/raw? sym-entry)
                     (fn [& args]
                       (with-context {:ext ext :symbol sym :env (env-thunk)}
                                     (apply (:ext.symbol/fn sym-entry) args)))
                     (fn [& args]
                       (let [env (env-thunk)
                             w (get-log-writer)]

                         ;; Only the IO redirect lives here — `invoke-symbol-wrapper`
                         ;; installs the extension context itself.
                         (binding [*out* w
                                   *err* w]

                           (invoke-symbol-wrapper ext sym-entry (vec args) env)))))]
                  [sym (:ext.symbol/val sym-entry)]))))
          entries)))

;; Public API - extension builder
(defn- derive-kind
  "Auto-derive `:ext/kind` for the categorical cases when the author
   didn't set one. Extensions that contribute providers, channels or
   channel contributions (and nothing forcing a different label) get
   bucketed under `\"providers\"` / `\"channels\"` so `vis-agent extension list`
   reads as a clean grouped table instead of a column of blanks.

   Explicit `:ext/kind` always wins. Extensions that fit no
   categorical bucket (and don't set a kind themselves) stay
   blank - that's a legitimate \"uncategorized\" outcome."
  [spec]
  (cond (some? (:ext/kind spec)) (:ext/kind spec)
        (seq (:ext/providers spec)) "providers"
        (seq (:ext/channels spec)) "channels"
        (seq (:ext/channel-contributions spec)) "channels"
        :else nil))

(defn extension
  "Build and validate an extension. The canonical constructor.

   See docs/src/extensions/extension-spec.md for the full key list."
  [spec]
  (-> spec
      (cond->
        (contains? spec :ext/prompt-fn)
        (update :ext/prompt-fn normalize-prompt))
      (cond->
        (not (:ext/activation-fn spec))
        (assoc :ext/activation-fn (constantly true))

        (some? (derive-kind spec))
        (assoc :ext/kind (derive-kind spec))

        (not (:ext/engine spec))
        (assoc :ext/engine {})

        (and (get-in spec [:ext/engine :ext.engine/alias])
             (nil? (get-in spec [:ext/engine :ext.engine/ns])))
        (assoc-in [:ext/engine :ext.engine/ns]
          (clojure.core/symbol (str "vis.ext."
                                    (name (get-in spec [:ext/engine :ext.engine/alias])))))

        (nil? (get-in spec [:ext/engine :ext.engine/symbols]))
        (assoc-in [:ext/engine :ext.engine/symbols] [])

        (nil? (get-in spec [:ext/engine :ext.engine/classes]))
        (assoc-in [:ext/engine :ext.engine/classes] {})

        (nil? (get-in spec [:ext/engine :ext.engine/imports]))
        (assoc-in [:ext/engine :ext.engine/imports] {})

        (not (:ext/env spec))
        (assoc :ext/env [])

        (not (:ext/settings spec))
        (assoc :ext/settings [])

        (not (:ext/theme spec))
        (assoc :ext/theme {})

        (not (:ext/cli spec))
        (assoc :ext/cli [])

        (not (:ext/channels spec))
        (assoc :ext/channels [])

        (not (:ext/providers spec))
        (assoc :ext/providers [])

        (not (:ext/attachment-storage spec))
        (assoc :ext/attachment-storage [])

        (not (:ext/channel-contributions spec))
        (assoc :ext/channel-contributions {})

        (not (:ext/slash-commands spec))
        (assoc :ext/slash-commands [])

        (not (:ext/doctor-fn spec))
        (assoc :ext/doctor-fn (constantly [])))
      (validate!)))

;; Extension source markers
;; Hash + mtime primitives.
(defn- read-stream-bytes
  ^bytes [^InputStream in]
  (with-open [out (ByteArrayOutputStream.)]
    (let [buf (byte-array 8192)]
      (loop []

        (let [n (.read in buf)]
          (when (pos? n) (.write out buf 0 n) (recur)))))
    (.toByteArray out)))

;; Resolve one namespace -> entry map.
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
    (some (fn [p]
            (.getResource cl ^String p))
          paths)))

(defrecord ^:private SourceEntry [^String locator ^long mtime ^bytes content])

(defn- file-entry
  "Build a SourceEntry for a `file:` URL. Reads the file content for
   hashing; mtime from `.lastModified`."
  ^SourceEntry [^URL url]
  (let [f
        (java.io.File. (.toURI url))

        path
        (.getAbsolutePath f)

        mtime
        (.lastModified f)

        content
        (try (read-stream-bytes (java.io.FileInputStream. f))
             (catch Throwable t
               (tel/log!
                 {:level :warn :id ::file-read-failed :data {:path path :error (ex-message t)}})
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
  (let [conn
        (.openConnection url)

        ;; The cast is paranoid - `.getJarFileURL` lives on `JarURLConnection`,
        ;; we know URL was a jar: URL when we got here.
        jconn
        ^java.net.JarURLConnection conn

        jar-url
        (.getJarFileURL jconn)

        jar-file
        (java.io.File. (.toURI jar-url))

        jar-path
        (.getAbsolutePath jar-file)

        entry-nm
        (.getEntryName jconn)]

    (with-open [jar (JarFile. jar-file)]
      (let [^JarEntry e (.getJarEntry jar entry-nm)]
        (if (nil? e)
          (do (tel/log!
                {:level :warn :id ::jar-entry-missing :data {:jar jar-path :entry entry-nm}})
              nil)
          (let [mtime (.getTime e)
                content (try (with-open [in (.getInputStream jar e)]
                               (read-stream-bytes in))
                             (catch Throwable t
                               (tel/log! {:level :warn
                                          :id ::jar-entry-read-failed
                                          :data
                                          {:jar jar-path :entry entry-nm :error (ex-message t)}})
                               (byte-array 0)))]

            (->SourceEntry (jar-entry-locator jar-path entry-nm) mtime content)))))))

(defn- url->entry
  "Dispatch on URL protocol to the right reader. Returns SourceEntry
   or nil on unrecognized protocol."
  [^URL url]
  (try (case (some-> url
                     .getProtocol
                     str/lower-case)
         "file"
         (file-entry url)

         "jar"
         (jar-entry url)

         (do (tel/log! {:level :warn
                        :id ::unsupported-protocol
                        :data {:protocol (.getProtocol url) :url (str url)}})
             nil))
       (catch Throwable t
         (tel/log! {:level :warn :id ::resolve-failed :data {:url (str url) :error (ex-message t)}})
         nil)))

;; Public API.
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
  (let [cl
        (.getContextClassLoader (Thread/currentThread))

        urls
        (->> ns-syms
             (map #(find-source-resource cl %))
             (remove nil?))

        entries
        (->> urls
             (map url->entry)
             (remove nil?)
             (sort-by :locator)
             vec)]

    (if (empty? entries)
      {:source-paths [] :source-mtime-max -1 :source-hash-sha256 nil}
      (let [paths
            (mapv :locator entries)

            mtime-max
            (long (reduce max 0 (map :mtime entries)))

            digest
            (util/sha256-digest)

            _
            (doseq [^SourceEntry e entries]
              (let [^bytes c (:content e)]
                (.update digest c 0 (alength c))))

            hash-bytes
            (.digest digest)

            hash-hex
            (util/bytes->hex hash-bytes)]

        {:source-paths paths :source-mtime-max mtime-max :source-hash-sha256 hash-hex}))))

(defn resolve-markers-for-extension
  "Resolve source markers from `:ext/source-nses` — the declaring namespace the
   `vis/extension` macro stamps on every extension."
  [ext]
  (resolve-markers (vec (or (:ext/source-nses ext) []))))

;; Global Extension Registry
(defonce ^:private extension-registry
  ;; Process-level atom holding all globally registered extensions.
  ;; Keyed by :ext/name to prevent duplicates.
  (atom {}))

(defonce ^:private extension-order
  ;; Namespace insertion order for `registered-extensions`. A plain
  ;; hash-map does not preserve order, and adding/removing unrelated
  ;; extensions can reshuffle doctor/lifecycle output.
  (atom []))

(defonce ^:private extension-source-markers
  ;; Sidecar atom holding source-file markers per registered extension.
  ;; Keyed by :ext/name. Populated at register-time, dropped at
  ;; deregister-time. Read by the tool-envelope emitter for the
  ;; UI extension provenance label. Kept
  ;; OUT of the extension map itself so `extension/validate!` doesn't have
  ;; to know about runtime-derived fields. Plan §5.5.
  (atom {}))

(defn- dispatch-providers!
  [providers]
  (doseq [provider-entry providers]
    (registry/register-provider! provider-entry)))

(defn- dispatch-attachment-storage!
  [entries]
  (doseq [entry entries]
    (attachment-storage/register-backend! entry)))

(def ^:private EXT_PARENT ["extension"])

(defn- mount-under-ext
  "Auto-place an `:ext/cli` entry under the `vis-agent extension` parent.

   Authors who want nested placement (e.g. `vis-agent extension db status`)
   can pass `:cmd/parent [\"extension\" \"db\"]` and the dispatcher
   respects it. The legacy `\"ext\"` first element is accepted and
   canonicalized to `\"extension\"`. Any other parent is rejected."
  [{:cmd/keys [parent name] :as entry}]
  (cond (or (nil? parent) (= [] parent)) (assoc entry :cmd/parent EXT_PARENT)
        (#{"ext" "extension"} (first parent)) (assoc entry
                                                :cmd/parent (into ["extension"] (rest parent)))
        :else (throw (ex-info (str ":ext/cli entry '"
                                   name
                                   "' has :cmd/parent "
                                   (pr-str parent)
                                   " -- extension-owned CLI mounts only under [\"extension\" ...].")
                              {:type :ext/cli-bad-parent :entry entry}))))

(defn register-extension!
  "Register an extension in the global process-level registry.

   This is THE single entry point for everything an extension
   contributes to vis. Whatever the extension declares -- Python sandbox
   symbols (`:ext.engine/symbols`), CLI commands (`:ext/cli`), channels
   (`:ext/channels`), LLM providers (`:ext/providers`) -- gets routed here and dispatched into
   the matching sub-registry as a side effect.

   Also computes source-file markers (paths, max-mtime, sha256) and
   stores them in a sidecar atom read by the tool-envelope emitter
   (UI extension provenance label).

   Idempotent on `:ext/name`. Returns the validated extension."
  [ext]
  (let [ext
        (extension ext)

        ns-sym
        (:ext/name ext)]

    ;; Slash paths must be unique across the union of `:ext/slash-commands` from every active
    ;; extension. Reject registration when this extension declares a `[parent name]`
    ;; that any OTHER currently-registered extension already owns
    ;; AND whose `:slash/availability-fn` intersects on the known
    ;; channel set. Two specs with the same path but DISJOINT
    ;; channel availability (e.g. TUI `/voice` vs another channel's `/voice`)
    ;; do not collide — the dispatcher resolves them via per-channel availability at runtime.
    (let [known-channels
          [:tui :discord :cli :repl :slack]

          slash-channels
          (fn [spec]
            (if-let [f (:slash/availability-fn spec)]
              (set (filter (fn [ch]
                             (try (boolean (f {:channel/id ch})) (catch Throwable _ false)))
                           known-channels))
              (set known-channels)))

          new-by-path
          (reduce (fn [m spec]
                    (assoc m (slash-path spec) spec))
                  {}
                  (:ext/slash-commands ext))]

      (when (seq new-by-path)
        (let [collisions (for [[other-ns other-ext] @extension-registry
                               :when (not= other-ns ns-sym)
                               other-slash (:ext/slash-commands other-ext)
                               :let [p (slash-path other-slash)
                                     new-spec (get new-by-path p)]
                               :when (and new-spec
                                          (seq (set/intersection (slash-channels new-spec)
                                                                 (slash-channels other-slash))))]

                           {:path p :other-ext other-ns})]
          (when (seq collisions)
            (throw (ex-info (str "Slash path collision while registering '" ns-sym
                                 "': " (str/join
                                         ", "
                                         (for [{:keys [path other-ext]} collisions]
                                           (str (pr-str path) " already owned by " other-ext))))
                            {:type :extension/slash-path-collision
                             :ext ns-sym
                             :collisions (vec collisions)}))))))
    (when-not (contains? @extension-registry ns-sym) (swap! extension-order conj ns-sym))
    (swap! extension-registry assoc ns-sym ext)
    (tel/log! {:level :info
               :id ::register-global
               :data {:ext ns-sym
                      :symbols (count (ext-symbols ext))
                      :cli (count (:ext/cli ext))
                      :channels (count (:ext/channels ext))
                      :providers (count (:ext/providers ext))
                      :themes (count (:ext/theme ext))}
               :msg (str "Extension '" ns-sym "' registered globally")})
    (doseq [c (:ext/cli ext)]
      (registry/register-cmd! (mount-under-ext c)))
    (doseq [c (:ext/channels ext)]
      (registry/register-channel! c))
    (dispatch-providers! (:ext/providers ext))
    (dispatch-attachment-storage! (:ext/attachment-storage ext))
    (install-op-hooks! ext)
    (install-egress-filters! ext)
    (theme/register-themes! (:ext/theme ext))
    ;; Index every symbol's inline `:ext.symbol/tag` into the
    ;; global op-keyword -> tag map. The sym-entry remains the source
    ;; of truth; this index is a cheap lookup for sites (e.g.
    ;; `envelope-of`) that have an op keyword but no sym-entry handle.
    (doseq [sym-entry
            (ext-symbols ext)

            :let [tag
                  (:ext.symbol/tag sym-entry)]
            :when tag]

      (let [op-kw (keyword (tool-call-name ext (:ext.symbol/symbol sym-entry)))]
        (swap! op-keyword->tag assoc op-kw tag)))
    ;; Index every symbol's optional inline `:ext.symbol/batch-hint`
    ;; high-fan-out threshold the same way (Phase 4). Advisory only.
    (doseq [sym-entry
            (ext-symbols ext)

            :let [hint
                  (:ext.symbol/batch-hint sym-entry)]
            :when hint]

      (let [op-kw (keyword (tool-call-name ext (:ext.symbol/symbol sym-entry)))]
        (swap! op-keyword->batch-hint assoc op-kw hint)))
    ;; Compute and store source markers in the sidecar atom. Resolved
    ;; via the helper (see source_markers.clj) which knows how to walk
    ;; both file: and jar: classpath URLs. Failures are logged at :warn
    ;; and degrade to empty markers - they don't fail registration.
    (try (let [markers (resolve-markers-for-extension ext)]
           (swap! extension-source-markers assoc ns-sym markers))
         (catch Throwable t
           (tel/log!
             {:level :warn :id ::source-markers-failed :data {:ext ns-sym :error (ex-message t)}})))
    ext))

(def ^:private empty-source-markers {:source-paths [] :source-mtime-max -1 :source-hash-sha256 nil})

(defn extension-source-markers-of
  "Lookup the source markers stored for `ns-sym`. Returns the marker
   map (`{:source-paths :source-mtime-max :source-hash-sha256}`) or
   nil when the extension was never registered (or its markers
   computation failed at register time)."
  [ns-sym]
  (get @extension-source-markers ns-sym))

(defn- source-markers-for-extension
  [ext]
  (or (extension-source-markers-of (:ext/name ext))
      (try (resolve-markers-for-extension ext)
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::source-markers-on-demand-failed
                        :data {:ext (:ext/name ext) :error (ex-message t)}})
             empty-source-markers))
      empty-source-markers))

(defn extension-info
  "Canonical extension info map.

   Merges author-declared extension metadata with source markers:
     {:namespace :alias? :doc? :kind? :version? :author? :owner?
      :license? :registry-id? :source-paths :source-mtime-max
      :source-hash-sha256}

   This is the single info shape used by ctx :extensions and tool-result enrichment."
  [ext]
  (let [name
        (:ext/name ext)

        alias
        (ext-alias-symbol ext)

        registry-id
        alias

        markers
        (source-markers-for-extension ext)

        prov
        (cond-> {:name name
                 :source-paths (:source-paths markers)
                 :source-mtime-max (:source-mtime-max markers)
                 :source-hash-sha256 (:source-hash-sha256 markers)}
          alias
          (assoc :alias alias)

          (:ext/description ext)
          (assoc :description (:ext/description ext))

          (:ext/kind ext)
          (assoc :kind (:ext/kind ext))

          (:ext/version ext)
          (assoc :version (:ext/version ext))

          (:ext/author ext)
          (assoc :author (:ext/author ext))

          (:ext/owner ext)
          (assoc :owner (:ext/owner ext))

          (:ext/license ext)
          (assoc :license (:ext/license ext))

          registry-id
          (assoc :registry-id registry-id))]

    (when-not (extension-info? prov)
      (throw (ex-info "Invalid extension info"
                      {:type :extension/invalid-info :name name :value prov})))
    prov))

(defn deregister-extension!
  "Drop an extension from the global registry AND reverse every side
   effect `register-extension!` dispatched: deregister each CLI
   subcommand, channel, provider, and persistence backend. Returns nil.

   Plan caveat: side-effect cleanup on `:removed` extensions. Used by
   Stays available for diagnostic surfaces."
  [ns-sym]
  (when-let [ext (get @extension-registry ns-sym)]
    (doseq [c (:ext/cli ext)]
      (let [mounted (mount-under-ext c)]
        (try (registry/deregister-cmd! (:cmd/parent mounted) (:cmd/name mounted))
             (catch Throwable t
               (tel/log! {:level :warn
                          :id ::deregister-cmd-failed
                          :data {:ext ns-sym :cmd (:cmd/name mounted) :error (ex-message t)}})))))
    (doseq [c (:ext/channels ext)]
      (try (registry/deregister-channel! (:channel/id c))
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::deregister-channel-failed
                        :data {:ext ns-sym :channel-id (:channel/id c) :error (ex-message t)}}))))
    (doseq [p (:ext/providers ext)]
      (try (registry/deregister-provider! (:provider/id p))
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::deregister-provider-failed
                        :data {:ext ns-sym :provider-id (:provider/id p) :error (ex-message t)}}))))
    (doseq [backend (:ext/attachment-storage ext)]
      (try (attachment-storage/deregister-backend! (:storage/id backend))
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::deregister-attachment-storage-failed
                        :data
                        {:ext ns-sym :backend-id (:storage/id backend) :error (ex-message t)}}))))
    (theme/unregister-themes! (keys (:ext/theme ext)))
    (unregister-op-hooks-for-owner! (ext-op-hook-owner ext))
    (egress-proxy/unregister-network-filters-for-owner! (ext-op-hook-owner ext))
    (tel/log! {:level :info
               :id ::deregister-global
               :data {:ext ns-sym}
               :msg (str "Extension '" ns-sym "' deregistered globally")}))
  (swap! extension-registry dissoc ns-sym)
  (swap! extension-order (fn [order]
                           (vec (remove #{ns-sym} order))))
  (swap! extension-source-markers dissoc ns-sym)
  nil)

(defn registered-extensions
  []
  (let [registry @extension-registry]
    (into [] (keep registry) @extension-order)))

;; Reload hooks — the seam `/reload` uses to refresh EXTENSION-owned resource
;; caches (harness skills/agents discovery, …) without core knowing about the
;; extension namespaces. Core-owned resources (project guidance, prompt
;; templates) are reloaded directly by the `/reload` slash.

(defonce ^:private reload-hooks (atom {}))

(defn register-reload-hook!
  "Register a zero-arg `f` to run on `/reload`. Idempotent per `id` —
   re-registering replaces. Hooks must be cheap and safe to call at any
   time; a throwing hook is reported, never fatal."
  [id f]
  (swap! reload-hooks assoc id f)
  id)

(defn run-reload-hooks!
  "Run every registered reload hook. Returns `{id {:ok? bool :error msg}}`."
  []
  (reduce-kv (fn [acc id f]
               (assoc acc
                 id (try (f)
                         {:ok? true}
                         (catch Throwable t
                           (tel/log! {:level :warn
                                      :id ::reload-hook-failed
                                      :data {:hook id :error (ex-message t)}})
                           {:ok? false :error (ex-message t)}))))
             {}
             @reload-hooks))

(defn- normalized-channel-contribution
  [slot contribution]
  (assoc contribution
    :channel-id (channel-slot->channel-id slot)
    :slot slot))

(defn channel-contributions-for
  "Return registered extension channel contributions for `channel-id` in
   extension registration order. With `slot`, return only contributions for
   that channel slot. Contributions are passive data; the channel owns each
   slot's fn arity + return contract."
  ([channel-id] (channel-contributions-for channel-id nil))
  ([channel-id slot]
   (let [rows (->> (registered-extensions)
                   (mapcat (fn [ext]
                             (mapcat (fn [[slot contributions]]
                                       (map #(normalized-channel-contribution slot %)
                                            contributions))
                                     (:ext/channel-contributions ext))))
                   (filter #(= channel-id (:channel-id %))))]
     (vec (cond->> rows
            slot
            (filter #(= slot (:slot %))))))))

(defn register-extensions!
  "Install all globally registered extensions into an environment in registry order.

   Called by `create-environment` automatically. Returns environment."
  [environment register-fn!]
  (doseq [ext (registered-extensions)]
    (register-fn! environment ext))
  environment)

(defn- registered-extensions-for-source-ns
  [ns-sym]
  (vec (filter #(contains? (set (ext-source-nses %)) ns-sym) (registered-extensions))))

(defn load-extension!
  "Dynamically load extension namespace and return extensions it registered."
  [ns-sym]
  (require ns-sym)
  (let [exts (registered-extensions-for-source-ns ns-sym)]
    (if (seq exts)
      exts
      (throw (ex-info (str "Namespace '" ns-sym "' was loaded but did not call register-extension!")
                      {:type :extension/no-registration
                       :namespace ns-sym
                       :registered (vec (keys @extension-registry))})))))

(def op-tags
  "Closed set of operation tags a tool can declare. The two values
   map to the observation/mutation half of the OODA loop. The prior
   granular enum collapses into these two:

     :observation   reads state without changing it — cat,
                           ls, exists?, locators, rg, env
                           queries, registry lookups

     :mutation      mutates state — patch, write, append,
                           mkdir, touch, delete, move, copy.

   Channels that want to color tools by tag look it up themselves;
   the engine never carries presentation in the tool envelope."
  #{:observation :mutation})

(defonce ^:private op-keyword->tag
  ;; Inverse index from canonical op-keyword to its `:observation` /
  ;; `:mutation` tag. Populated as a side-effect of `register-extension!`
  ;; from each symbol entry's inline `:ext.symbol/tag` — the sym-entry
  ;; stays the source of truth; this atom is just a cheap lookup for
  ;; sites (e.g. `envelope-of`) that have an op keyword but no sym-entry
  ;; handle. There is no public `register-op!` writer — registration
  ;; funnels through `register-extension!` from inline symbol metadata.
  ;;
  ;; `defonce`, NOT `def`: the distribution manifest invokes each explicit
  ;; `register!` initializer once. Reloading this namespace must not erase tags
  ;; registered by initializers that are not invoked again. Matches the other
  ;; registration-populated registries in this ns (`extension-registry`,
  ;; `extension-order`, `extension-source-markers`).
  ;; (`defonce` takes no docstring — hence the `;;` comment.)
  (atom {}))

(defn op-tag
  "Return the `:observation | :mutation` tag for `op-keyword`. Unknown
   ops fail closed; every symbol must declare `:tag` inline on its
   `vis/symbol` entry."
  [op-keyword]
  (if-let [tag (get @op-keyword->tag op-keyword)]
    tag
    (anomaly/incorrect! (str "Unregistered extension op "
                             (pr-str op-keyword)
                             " has no mandatory observation/mutation tag")
                        {:type :extension/unregistered-op :op op-keyword :allowed op-tags})))

(defn op-tag-index
  "Read-only snapshot of the canonical op-keyword -> tag map. Lets
   call-sites that only hold a Python-snake call HEAD (the
   `classify-form-tag` resolver in `loop.clj`, which reads the head off
   the model's source) fold each registered op to its Python name and
   recover the tag — there is no `vis/symbol` handle at that point.
   Never throws; an unknown head simply misses the folded view."
  []
  @op-keyword->tag)

(defonce ^:private op-keyword->batch-hint
  ;; Inverse index from canonical op-keyword to its per-tool high-fan-out
  ;; batch-hint threshold (`:ext.symbol/batch-hint`). Populated as a
  ;; side-effect of `register-extension!`. Unlike `:tag`, this is OPTIONAL —
  ;; tools without an explicit override fall back to the iteration ns default.
  ;;
  ;; `defonce` for the same reason as `op-keyword->tag`: registration is a
  ;; reload-surviving side effect, so a plain `def` would wipe it on every
  ;; `:reload`. (`defonce` takes no docstring — hence the `;;` comment.)
  (atom {}))

(defn op-presentation
  "Engine-owned presentation metadata for a tool's `:op` keyword:
   `{:tag ...}`. Tool wrappers merge this into their `:info`/`:metadata`
   so channels read canonical keys.

   Badge LABEL is derived from `:tag` by the channel, not stored here.
   Color / glyph / layout remain pure channel concerns."
  [op]
  {:tag (op-tag op)})

(defn- python-param-name
  "One declared parameter as a PYTHON identifier: kebab-case becomes snake_case
   and Clojure's marks (`?`, `!`, `*`) are dropped. nil when nothing legal
   survives — a destructuring form or a `&` has no name to show."
  [param]
  (when (or (simple-symbol? param) (string? param) (keyword? param))
    (let [n (-> (name param)
                (str/replace "-" "_")
                (str/replace #"[^A-Za-z0-9_]" ""))]
      (when (re-matches #"[A-Za-z_][A-Za-z0-9_]*" n) n))))

(defn- call-shape-signature
  "Python parameter list for a `:ext.symbol/call` SHAPE. The shape already
   declares how a Python call's keywords re-expand onto the implementation's
   positionals, so it IS the sandbox-facing signature: `:lead-opt` is one
   optional leading parameter, `:pos` the required ones, `:opt-pos` the optional
   trailing ones. `**kwargs` appears ONLY under `:rest` — without it an
   undeclared keyword stops `folded-kwargs->positional` from re-expanding at
   all, so promising kwargs would be a lie. nil when a declared key has no legal
   Python name."
  [{lead :lead-opt pos :pos opt-pos :opt-pos rest-mode :rest}]
  (let [named
        (mapv python-param-name (concat (when lead [lead]) pos opt-pos))

        lead-n
        (if lead 1 0)]

    (when (and (seq named) (every? some? named))
      (str/join ", "
                (concat (map #(str % "=None") (take lead-n named))
                        (take (count pos) (drop lead-n named))
                        (map #(str % "=None") (drop (+ lead-n (count pos)) named))
                        (when rest-mode ["**kwargs"]))))))

(defn- arglists-signature
  "Python parameter list from the implementation's `:ext.symbol/arglists`, for an
   entry that declares no `:call` shape. The longest fixed arity names the
   parameters, everything past the SHORTEST arity is optional, a `&` tail is
   `*args`, and `**kwargs` is always accepted because CPython folds keywords
   into exactly the trailing dict positional such a tool receives as its options
   map. `env` leads the arglist of an env-injected impl and is dropped: the host
   passes it, never the model. A tool that takes NOTHING answers the EMPTY
   parameter list, which is a fact and not a missing one. nil only when the
   arglists carry no names at all (`[[& args]]`) — the wrapper's own `(*a, **k)`
   already says that much."
  [arglists inject-env?]
  (let [lists
        (->> arglists
             (filter sequential?)
             (map vec)
             (map (fn [al]
                    (if (and inject-env? (= 'env (first al))) (subvec al 1) al))))

        fixed
        (map (fn [al]
               (vec (take-while #(not= '& %) al)))
             lists)

        longest
        (last (sort-by count fixed))

        required
        (when (seq fixed) (apply min (map count fixed)))

        variadic?
        (boolean (some (fn [al]
                         (some #(= '& %) al))
                       lists))

        named
        (mapv python-param-name longest)]

    (cond
      ;; `languages()` — declared with no parameters at all. "" is what renders
      ;; that; nil would leave the sandbox reporting the async trampoline's own
      ;; `(*a, **k)`, promising arguments the tool refuses.
      (and (seq lists) (empty? named) (not variadic?)) ""
      (and (seq named) (every? some? named))
      (str/join ", "
                (concat (map-indexed (fn [i n]
                                       (if (< (long i) (long required)) n (str n "=None")))
                                     named)
                        (when variadic? ["*args"])
                        ["**kwargs"])))))

(defn symbol-signature
  "Python parameter list for ONE symbol entry — what stands between the
   parentheses of the signature the sandbox reports for it, e.g.
   `\"language=None, **kwargs\"`. The `:call` shape wins (it is the declared
   keyword->positional contract); the implementation's arglists are the
   fallback. nil when the entry declares nothing a caller could act on.

   `env-python` ships these to the sandbox as `__vis_sigs__`, where a deferred
   tool hangs its parameters off `__wrapped__` so `inspect.signature(tool)` and
   `help(tool)` answer with them instead of with the async trampoline's own
   `(*a, **k)`."
  [entry]
  (when (:ext.symbol/fn entry)
    (let [shape (:ext.symbol/call entry)]
      (or (when (map? shape) (call-shape-signature shape))
          (arglists-signature (:ext.symbol/arglists entry)
                              (boolean (:ext.symbol/inject-env? entry)))))))

(defn symbol-keys-line
  "`Keys: language · code (REQUIRED) · id` — the options-dict vocabulary from
   `:ext.symbol/params`, in DECLARED order (authors lead with what a caller cannot
   omit). The signature of a dict-shaped tool ends in `**kwargs`, which names
   nothing; this line is where its required keys are stated. nil when the entry
   declares no params.

   STRUCTURE, never prose: `env-python` ships it to the sandbox as `__vis_keys__`
   and `doc-corpus/entry-text` prints it under the call line. `apropos` filters
   names only, so this structure cannot affect discovery; keeping it out of the
   prose also avoids a second signature-shaped contract."
  [entry]
  (when-let [params (seq (:ext.symbol/params entry))]
    (str "Keys: "
         (str/join " · "
                   (map (fn [{nm :name required? :required? note :note}]
                          (let [mark (str/join " — "
                                               (remove nil? [(when required? "REQUIRED") note]))]
                            (cond-> (str nm)
                              (seq mark)
                              (str " (" mark ")"))))
                        params)))))

(defn symbol-doc-text
  "Model-facing doc text for ONE symbol ENTRY: the compact `:description` (falling
   back to the implementation docstring), then the raw-result contract whenever the
   entry declares one. Returns nil without prose — a handle with no description has
   no page. This is the single source `doc(name)` answers from.

   PROSE ONLY. How the handle is CALLED is structure, not text: `symbol-signature`
   renders the call line and `symbol-keys-line` the required keys, and
   `doc-corpus/entry-text` prints both above this document."
  [entry]
  (let [prose
        (or (:ext.symbol/description entry) (:ext.symbol/doc entry))

        ;; The raw-result contract belongs to EVERY doc-bearing symbol: a sandbox
        ;; verb is called from Python with nothing in front of it, so `doc(name)`
        ;; is the only place its result keys are ever stated.
        result
        (:ext.symbol/result entry)

        text
        (cond-> prose
          (and (string? prose) result)
          (str "\n\nRaw result: " result))]

    (when (util/non-blank-string? text) text)))

(defn sandbox-symbol-signatures
  "Map `{sandbox-symbol -> python-parameter-list}` for every engine-bound
   callable across the registered extensions, from `symbol-signature`. The
   signature twin of `sandbox-symbol-docs`, seeded into the sandbox by
   `env-python/build-agent-context` and keyed the same way: by the BARE symbol,
   so aliased extensions (which bind per turn) seed their own through
   `env-python/set-python-binding-signature!`."
  []
  (manifest/initialize!)
  (into {}
        (for [ext
              (registered-extensions)

              entry
              (ext-symbols ext)

              :let [sym
                    (:ext.symbol/symbol entry)

                    sig
                    (symbol-signature entry)]
              :when (and sym sig)]

          [sym sig])))

(defn sandbox-symbol-keys
  "Map `{sandbox-symbol -> keys-line}` for every engine-bound callable whose
   contract lives inside an options dict, from `symbol-keys-line`. The
   requiredness twin of `sandbox-symbol-signatures`: the signature says HOW the
   verb is called, this says WHICH keys the dict must carry. Seeded into the
   sandbox as `__vis_keys__` by `env-python/build-agent-context`, and per turn by
   aliased extensions through `env-python/set-python-binding-keys!`."
  []
  (manifest/initialize!)
  (into {}
        (for [ext
              (registered-extensions)

              entry
              (ext-symbols ext)

              :let [sym
                    (:ext.symbol/symbol entry)

                    line
                    (symbol-keys-line entry)]
              :when (and sym line)]

          [sym line])))

(defn sandbox-symbol-docs
  "Map `{sandbox-symbol -> doc-text}` for every engine-bound symbol across the
   registered extensions, keyed by the `:ext.symbol/symbol` as it is bound in
   the Python sandbox. `doc-text` comes from `symbol-doc-text`.

   `env_python/build-agent-context` seeds the sandbox `__vis_docs__` table from
   this so in-sandbox `doc(name)` returns the tool's real description instead of
   a bare `name (callable)`. Loads built-ins first (idempotent) so the registry
   is populated before we read it. Symbols absent here simply have no doc entry.

   NOTE: this keys by the BARE symbol, so it only serves BUILT-IN (unaliased)
   extensions bound eagerly at context creation. Aliased extensions bind their
   `<alias>_<name>` symbols LATER (per turn) and seed `__vis_docs__` themselves
   through `symbol-doc-text` — see `loop/sync-active-extension-symbols!`."
  []
  (manifest/initialize!)
  (into {}
        (for [ext
              (registered-extensions)

              entry
              (ext-symbols ext)

              :let [sym
                    (:ext.symbol/symbol entry)

                    text
                    (symbol-doc-text entry)]
              :when (and sym text)]

          [sym text])))

(defn builtin-sandbox-bindings
  "`{sym -> fn}` bindings for EVERY registered built-in extension
   (`ext-builtin?`), merged into the Python sandbox globals alongside the engine
   verbs at sandbox-context creation. `env-thunk` (0-arg) resolves the live
   environment at call time, so these can be wired before the env map exists.
   Loads built-ins first (idempotent) so registration is guaranteed before we
   read the registry. Later extensions win on key collisions, but built-ins are
   disjoint by construction (kernel tools vs engine verbs).

   Each value is the plain wrapped tool fn; `env_python/create-python-context`
   installs it as a Python callable (ProxyExecutable). Per-tool docstrings are
   surfaced through the sandbox's own `doc`/`apropos` introspection."
  [env-thunk]
  (manifest/initialize!)
  (into {}
        (comp (filter ext-builtin?)
              (mapcat (fn [ext]
                        (wrap-extension-thunked ext env-thunk))))
        (registered-extensions)))

(defn sandbox-shims
  "Every Python sandbox SHIM contributed across all registered extensions, in
   registration order (built-ins first). `env-python/build-agent-context`
   installs each into the model sandbox Context at creation time — wiring the
   shim's host `:shim/bindings` onto the globals, then eval'ing its
   `:shim/source` Python file — turning a host / JVM capability into an importable
   module. Loads built-ins first (idempotent) so the registry is populated
   before we read it."
  []
  (manifest/initialize!)
  (into [] (mapcat ext-sandbox-shims) (registered-extensions)))

(defn shim-src
  "Python source of `shim`, slurped from its `:shim/source` CLASSPATH RESOURCE
   (e.g. \"vis-shims/yaml.py\"). This is the single reader for shim source: the
   Python source never lives in a Clojure string. Works identically in the native
   image because build.clj embeds `vis-shims/.*` via `-H:IncludeResources`.
   Throws when the resource is missing - a shim whose file did not make it onto
   the classpath must fail loudly, not install a silently empty module."
  ^String [shim]
  (let [res (:shim/source shim)]
    (if-let [u (io/resource res)]
      (slurp u)
      (throw (ex-info (str "sandbox shim source not found on classpath: " res)
                      {:shim/name (:shim/name shim) :shim/source res})))))

;; CLI bridge -- the `vis-agent extension` parent lives in `internal.main` next to the
;; other top-level built-in parents (`providers`, `sessions`, `doctor`,
;; `update`). Extensions populate it via `:ext/cli` on `extension`;
;; `register-extension!` above forwards each entry through `mount-under-ext`
;; to `register-cmd!`.
