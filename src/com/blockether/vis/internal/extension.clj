(ns com.blockether.vis.internal.extension
  "Extension subsystem: spec, builders, hook execution, the global
   registry, parse-error rescue, and manifest namespace catalog.

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
     - the manifest id/namespace catalog used for extension metadata

   Channel and provider registries live in `internal.registry` (the
   sub-registry that lights up when this module dispatches their
   contributions). Backend dispatch lives in `internal.persistance`.
   Classpath manifest scanning lives in `internal.manifest`. This
   module is the only consumer of all four."
  (:refer-clojure :exclude [symbol])
  (:require [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.anomaly.core :as anomaly]
            [com.blockether.vis.internal.manifest :as manifest]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.render :as render]
            [com.blockether.vis.internal.theme :as theme]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.net URL)
           (java.security MessageDigest)
           (java.util.jar JarEntry JarFile)))
(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn render-ir?
  "True when `x` is an IR value acceptable from `:render-fn`.
   Non-canonical `[:ir ...]` is accepted and normalized at render time."
  [x]
  (render/ir? x))
(defn render-value? "Render fns must return answer IR (`[:ir ...]`)." [x] (render-ir? x))
(defn literal-ir
  "Build literal-text IR. For compatibility wrappers and error placeholders,
   not for `:render-fn` return strings."
  [x]
  (render/->ast (str x)))
(defn normalize-render-value
  "Normalize an accepted render-fn IR value to canonical IR."
  [x]
  (render/->ast x))
(defn combine-render-values
  "Combine per-form IR values into one canonical IR root."
  [values]
  (let [values (vec (remove nil? values))]
    (if (empty? values)
      [:ir {}]
      (let [blocks (map #(drop 2 (normalize-render-value %)) values)
            separators (repeat [[:p {} [:span {} ""]]])]
        (into [:ir {}] (mapcat identity (butlast (interleave blocks separators))))))))
;; ---------------------------------------------------------------------------
;; IR builder helpers for `:render-fn` / `:render-error-fn` authors.
;;
;; Tool authors hand-build canonical channel IR. These thin constructors keep
;; render-fns readable and consistent without forcing authors to remember the
;; `[tag attrs & children]` shape. Every builder returns canonical IR nodes;
;; `ir-root` wraps blocks into the `[:ir ...]` document the contract requires.
;; ---------------------------------------------------------------------------
(defn ir-root
  "Canonical IR document `[:ir {} & blocks]`. Wrap your blocks before returning
   them as `:display` (or as plain-IR `:summary`)."
  [& blocks]
  (into [:ir {}] blocks))
(defn ir-p
  "One paragraph block `[:p {} & inlines]`. Children may be strings or inline
   IR nodes (`ir-strong`, `ir-code`, plain text)."
  [& inlines]
  (into [:p {}] (map (fn [x] (if (string? x) [:span {} x] x)) inlines)))
(defn ir-strong
  "Bold inline `[:strong {} [:span {} text]]`. In a summary, the FIRST
   `ir-strong` is the row label by convention (no `:label` field)."
  [text]
  [:strong {} [:span {} (str text)]])
(defn ir-code "Inline code span `[:c {} text]`." [text] [:c {} (str text)])
(defn ir-code-block
  "Fenced code block `[:code {:lang lang} text]`. `lang` defaults to `\"text\"`."
  ([text] (ir-code-block "text" text))
  ([lang text] [:code {:lang (or lang "text")} (str text)]))
;; ---------------------------------------------------------------------------
;; `:render-fn` / `:render-error-fn` contract — {:summary :display}
;;
;; Every observed tool's renderer returns this map.
;;   :summary — the single visual badge row. EITHER canonical IR (one [:p ...]
;;              paragraph, first [:strong ...] = label) OR a zone map
;;              {:left <ir-or-string> :center? <ir-or-string>
;;               :right? <ir-or-string>} painted left … (center) … right.
;;   :display — canonical channel IR ([:ir ...]); the full expanded body.
;; ---------------------------------------------------------------------------
(defn render-value-or-string?
  "Zone payloads accept a plain string, a full `[:ir ...]` document, or a
   single IR/hiccup node (inline like `(ir-strong …)` / `(ir-code …)` or a
   block). Zones are flattened into a paragraph by `summary->ir`, so the
   common case is an inline node such as the label."
  [x]
  (or (string? x) (render-value? x) (and (vector? x) (keyword? (first x)))))
(s/def :render.zone/left render-value-or-string?)
(s/def :render.zone/center render-value-or-string?)
(s/def :render.zone/right render-value-or-string?)
(s/def :render/zones
  (s/keys :req-un [:render.zone/left] :opt-un [:render.zone/center :render.zone/right]))
(s/def :render/summary
  (s/or :ir render-value?
    :zones :render/zones))
(s/def :render/display render-value?)
(s/def ::render-fn-result (s/keys :req-un [:render/summary :render/display]))
(defn render-fn-result?
  "True when `x` conforms to the `{:summary :display}` render contract."
  [x]
  (s/valid? ::render-fn-result x))
(defn render-zones?
  "True when `summary` is a zone map (`{:left … :center? … :right? …}`)
   rather than plain IR."
  [summary]
  (and (map? summary) (s/valid? :render/zones summary)))
(defn assert-render-fn-result!
  "Throw unless `result` conforms to `::render-fn-result`. `label`
   (`:render-fn` / `:render-error-fn`) and `sym` flow into the error for
   actionable diagnostics. Returns `result` on success."
  [result sym label]
  (when-not (render-fn-result? result)
    (throw (ex-info (str label
                      " for symbol '" sym
                      "' must return {:summary <ir-or-zones> :display <ir>}, got " (pr-str
                                                                                     result))
             {:type :extension/render-non-contract,
              :symbol sym,
              :label label,
              :value result,
              :explain (s/explain-data ::render-fn-result result)})))
  result)
(defn summary->ir
  "Normalize a `:summary` (plain IR or a zone map) into a single canonical
   `[:p ...]`-bearing IR root for channels that paint a flat badge row.

   Zone maps flatten left → center → right, separated by a single space
   span, preserving the first-[:strong]-is-label convention (a string
   `:left` is wrapped verbatim, an IR `:left` keeps its own [:strong]).
   Plain-IR summaries pass through `normalize-render-value`."
  [summary]
  (if (render-zones? summary)
    (let [zone->inlines
          (fn [z]
            (cond (nil? z) nil
              (string? z) [[:span {} z]]
                    ;; full [:ir ...] document → lift inline children out of its paragraph
                    ;; blocks
              (render-value? z)
              (let [blocks (drop 2 (normalize-render-value z))]
                (mapcat (fn [b] (if (and (vector? b) (= :p (first b))) (drop 2 b) [b]))
                  blocks))
                    ;; bare inline/block IR node ((ir-strong …), (ir-code …), …)
              (and (vector? z) (keyword? (first z))) [z]
              :else [[:span {} (str z)]]))
          sep [[:span {} "  "]]
          parts (->> [(:left summary) (:center summary) (:right summary)]
                  (map zone->inlines)
                  (remove nil?)
                  (remove empty?))
          inlines (mapcat identity (butlast (interleave parts (repeat sep))))]
      (normalize-render-value (into [:ir {} (into [:p {}] inlines)])))
    (normalize-render-value summary)))

(defn- op->alias
  "Alias namespace of a tool op, e.g. :git_log / 'git_log / :git/log -> \"GIT\".
   Tools without an alias namespace (cat, ls, patch) return nil so their badge
   stays the bare verb."
  [op]
  (when op
    (let [s    (-> op str (str/replace #"^:" ""))
          head (first (str/split s #"[/_]"))]
      (when (and (seq head) (not= head s))
        (str/upper-case head)))))

(defn- summary-left->inlines
  "Lift a zone `:left` value (string / IR doc / bare node) to a vector of inline
   nodes - same rules as summary->ir's internal zone->inlines."
  [left]
  (cond
    (nil? left)                                  []
    (string? left)                               [[:span {} left]]
    (render-value? left)                         (mapcat (fn [b] (if (and (vector? b) (= :p (first b))) (drop 2 b) [b]))
                                                   (drop 2 (normalize-render-value left)))
    (and (vector? left) (keyword? (first left))) [left]
    :else                                        [[:span {} (str left)]]))

(defn- prepend-op-alias "Prefix a tool `:summary` with a dim (non-bold) alias breadcrumb derived from\n   `op` - e.g. LOG -> GIT · LOG. The verb/status label keeps its own\n   [:strong]; the alias is a plain span so channels render it lighter than the\n   bold label. Tools without an alias namespace pass through unchanged, as do\n   labels that ALREADY lead with the alias (e.g. op `:rg/files` + label\n   \"RG files\" stays \"RG files\", not \"RG · RG files\")." [summary op] (if-let [alias (op->alias op)] (let [label (some->> (if (render-zones? summary) (:left summary) summary) (tree-seq coll? seq) (filter string?) first str str/upper-case)] (if (and label (str/starts-with? label alias)) summary (let [pfx [:span {} (str alias " · ")]] (if (render-zones? summary) (update summary :left (fn [left] [:ir {} (into [:p {} pfx] (summary-left->inlines left))])) (let [[tag attrs & blocks] (normalize-render-value summary) [b0 & more] blocks b0' (if (and (vector? b0) (= :p (first b0))) (into [:p (if (map? (second b0)) (second b0) {}) pfx] (drop 2 b0)) b0)] (into [tag attrs] (cons b0' more))))))) summary))

(defn render-fn-result->ir
  "Flatten one `{:summary :display}` value into a single canonical IR root
   whose FIRST block is the summary paragraph (badge row) and whose
   remaining blocks are the `:display` body. Channels that paint a
   summary-then-body stack (TUI collapse, Telegram) consume this directly;
   the first block is guaranteed to be the badge by construction, not by
   sniffing.

   With an `op` (the tool's op keyword / symbol, e.g. :git_log) the badge
   label gains a dim alias breadcrumb (`GIT \u00b7 LOG`); tools without an
   alias namespace (cat, ls, patch) render the bare verb unchanged."
  ([result] (render-fn-result->ir result nil))
  ([{:keys [summary display]} op]
   (let [summary-ir (summary->ir (prepend-op-alias summary op))
         display-ir (normalize-render-value display)]
     (into (vec (take 2 summary-ir)) (concat (drop 2 summary-ir) (drop 2 display-ir))))))
;; =============================================================================
;; Tool-result contract
;; =============================================================================
(def ^:private max-trace-frames 12)
(defn- now-ms [] (System/currentTimeMillis))
(declare op-tag op-tags op-keyword->tag op-keyword->batch-hint tool-call-name)
;; ---- envelope leaf specs (op/*) ----
(s/def ::symbol
  (s/or :op keyword?
    :tool-symbol symbol?))
; op e.g. :cat, tool symbol e.g. 'cat
(s/def ::tag keyword?)
; #{:observation :mutation}
(s/def ::result any?)
; the actual Python eval value; shape varies per tool
(s/def ::success? boolean?)
(s/def ::metadata (s/map-of keyword? any?))
; free-form aux: :duration-ms, :paths, :hit-count, :tool, :source, :extension, etc.
;; ---- structured op/error sub-specs ----
(s/def :op.error/message (s/and string? #(not (str/blank? %))))
(s/def :op.error/trace (s/nilable string?))
(s/def :op.error/hint (s/nilable (s/and string? #(not (str/blank? %)))))
(s/def :op.error.block/source string?)
(s/def :op.error.block/row pos-int?)
(s/def :op.error.block/col pos-int?)
(s/def :op.error.block/opened-loc
  (s/nilable (s/keys :req-un [:op.error.block/row :op.error.block/col])))
(s/def :op.error.block/phase #{:preflight})
(s/def :op.error/block
  (s/nilable (s/keys :req-un [:op.error.block/source :op.error.block/phase]
               :opt-un [:op.error.block/row :op.error.block/col :op.error.block/opened-loc])))
(s/def ::error
  (s/nilable (s/keys :req-un [:op.error/message]
               :opt-un [:op.error/trace :op.error/hint :op.error/block])))
;; ---- the envelope ----
(s/def ::envelope
  (s/and (s/keys :opt-un [::symbol ::tag ::result ::success? ::error ::metadata])
         ;; Distinguishing-marker requirement: a real envelope MUST carry
         ;; the canonical boolean `:success?` field. Without this gate,
         ;; plain maps (e.g. user data, results from non-envelope code)
         ;; would validate as envelopes because every field is optional.
         ;; Renderers that special-case envelopes would then mis-categorise
         ;; plain data — see env_test.clj/build-bindings rendering bug.
    #(contains? % :success?)
    (fn [{:keys [success? error]}]
      (if success? (nil? error) (or (nil? success?) (some? error))))))
;; ---------------------------------------------------------------------------
;; Sink-entry shape (one entry per tool-symbol call in a top-level form)
;;
;; Only the channel sink remains. The model-facing surface is the bare-EDN
;; CTX block (`ctx-renderer/render-ctx`) built from the live ctx-atom plus
;; engine-derived warnings / progression / next-actions, not a per-tool
;; string render.
;; ---------------------------------------------------------------------------
(s/def :ext.sink/position (s/and integer? (complement neg?)))
(s/def :ext.sink/form non-blank-string?)
(s/def :ext.sink/form-idx (s/and integer? (complement neg?)))
(s/def :ext.sink/success? boolean?)
;; The sink `:result` now carries the FULL `{:summary :display}` render
;; contract value, not bare IR. `:summary` is the badge row (IR or zones);
;; `:display` is the expanded body IR. Renderers read both directly — the
;; "first paragraph = summary" heuristic is gone.
(s/def :ext.sink/result (s/nilable ::render-fn-result))
(s/def :ext.sink/error ::error)
; ::error is itself nilable per its spec
(s/def :ext.sink/symbol
  (s/nilable (s/or :kw keyword?
               :sym symbol?)))
(s/def :ext.sink/tag (s/nilable keyword?))
;; Canonical op keyword for the call (e.g. `:git/status`), plus wall-clock
;; lifecycle stamps so renderers can show real durations without reaching
;; back into the envelope metadata.
(s/def :ext.sink/op (s/nilable keyword?))
(s/def :ext.sink/started-at-ms (s/nilable (s/and integer? (complement neg?))))
(s/def :ext.sink/finished-at-ms (s/nilable (s/and integer? (complement neg?))))
(s/def ::sink-entry
  (s/and (s/keys :req-un [:ext.sink/position :ext.sink/form :ext.sink/success? :ext.sink/result
                          :ext.sink/error]
           :opt-un [:ext.sink/form-idx :ext.sink/symbol :ext.sink/tag :ext.sink/op
                    :ext.sink/started-at-ms :ext.sink/finished-at-ms])
         ;; NOTE: `s/and` threads the CONFORMED output of the `s/keys` stage
         ;; into this predicate, so by the time we see `:result` its `:summary`
         ;; has been wrapped by the `:render/summary` `s/or` (e.g. `[:zones …]`)
         ;; and `:symbol` by its `s/or`. Do NOT re-validate the render shape
         ;; here — `(render-fn-result? result)` on the conformed value always
         ;; fails, which silently broke EVERY tool's sink write. The
         ;; `:ext.sink/result` key spec (`(s/nilable ::render-fn-result)`)
         ;; already validates the shape against the RAW value; this predicate
         ;; only enforces the success/result/error relationship.
    (fn [{:keys [success? result error]}]
      (if success? (and (some? result) (nil? error)) (and (nil? result) (some? error))))))
(defn assert-sink-entry!
  "Throw on shape drift before a render sink write. Cheap; runs inside
   `record-render-entry!` per call."
  [entry]
  (when-not (s/valid? ::sink-entry entry)
    (throw (ex-info "Invalid sink entry"
             {:type :vis/invalid-sink-entry,
              :entry entry,
              :explain (s/explain-data ::sink-entry entry)})))
  entry)
(def ^:dynamic *tool-event-sink*
  "Optional per-eval sink for observable tool lifecycle events. Bound by
   tests and UI/progress adapters that need to know a tool started before
   its fn returns. The sink receives plain event maps."
  nil)
(defn- record-tool-event! [event] (when *tool-event-sink* (*tool-event-sink* event)) event)
;; ============================================================================
;; Per-top-level-form render sink
;;
;; `run-python-code` (`internal/loop.clj`) binds these dynamic vars before
;; evaluating each top-level form. `invoke-symbol-wrapper` writes ONE entry
;; to the sink per tool-symbol call, regardless of nesting depth
;; (`(do ...)`, `(let ...)`, deeply nested) and regardless of whether the
;; tool-result bubbles up as the form's return value.
;;
;; Late writes from threads spawned by tools that survive past the form's
;; return are silently dropped - the dynamic var has unwound, the `when`
;; guard turns the write into a no-op. Tools that need observation must
;; complete inline.
;;
;; The MODEL never reads this sink. The new CTX engine's per-iter trailer
;; pins (`:session/trailer` in the rendered ctx block) carry the real per-
;; form envelopes. This sink exists solely so runtime channels
;; (TUI, Telegram, …) can paint each tool call's IR.
;; ============================================================================
(def ^:dynamic *render-sink*
  "Per-top-level-form atom holding a vec of `::sink-entry`s, one per
   tool-symbol call. Bound fresh by `run-python-code` before each form's
   eval; deref'd into the block result map under `:channel` after."
  nil)
(def ^:dynamic *sink-position*
  "Per-top-level-form atom holding a monotonic long counter; bumped once
   per tool-symbol call so each entry in `*render-sink*` carries
   a stable `:position` even across nested calls."
  nil)
(def ^:dynamic *current-form-idx*
  "Zero-based index of the top-level form currently evaluating, bound
   per-form by `run-python-code` so the render sink writer can stamp
   `:form-idx` on every entry.

   The render sink atom itself is iteration-scoped (one channel-sink
   per `run-python-code` invocation, fed by every tool call across every
   top-level form). Persisted iteration rows carry a `:forms` envelope
   vec; on rebuild `expanded-blocks` partitions the fence's channel
   slice by `:form-idx` so each form envelope only carries the IR for
   tool calls it actually made. Without per-entry form-idx every tool
   IR ride-shared on form 0 and a 4-form fence (\"three ls calls\")
   restored as one bubble with three pre-rendered IRs glommed onto
   the first `(def ...)` form and nothing on the rest."
  nil)
(def ^:dynamic *turn-observation-cache*
  "Turn-scoped atom (or nil) of `{form-string {:iteration N :position P}}`.
   The iteration loop binds this once per turn so the sink writer can
   collapse repeated observation tool calls into a one-line reference
   instead of replaying the full peek. Mutation tool calls invalidate the
   cache (the writer resets it to `{}` after recording the mutation).

   Cache key is the literal call form string — includes the alias / symbol
   head and the EVALUATED arg vector (`sink-form-string`). Different args
   miss; identical args within the same turn hit.

   Cache is in-memory only; it is NOT persisted to SQLite. It exists
   purely as an engine-side render dedup signal so the model and the UI
   stop seeing the same data three times."
  nil)
(def ^:dynamic *current-iteration-position*
  "1-based current iteration position, bound by the iteration loop so the
   sink writer can stamp `:cached-from :iteration` references on cache
   hits without reaching back into environment atoms."
  nil)
(defn- next-sink-position!
  "Atomically claim the next position in the per-form counter. Returns the
   incremented integer (post-increment so the first call returns 0)."
  []
  (when *sink-position* (let [v (swap! *sink-position* (fnil inc -1))] (long v))))
(defn record-render-entry!
  "Validate `entry` against `::sink-entry` and conj into the active
   render sink atom. No-op when `*render-sink*` is unbound.

   Auto-stamps `:form-idx` from `*current-form-idx*` when the caller
   didn't supply one; the rebuild path uses this key to partition the
   fence's render sink back onto per-form envelopes."
  [entry]
  (when *render-sink*
    (let [entry (cond-> entry
                  (and (some? *current-form-idx*) (not (contains? entry :form-idx)))
                  (assoc :form-idx *current-form-idx*))]
      (assert-sink-entry! entry)
      (swap! *render-sink* conj entry)))
  entry)
(defn tool-result?
  "True when `x` is a valid `:envelope` map. Renamed conceptually;
   name kept for caller compatibility."
  [x]
  (s/valid? ::envelope x))
(defn assert-tool-result!
  [x]
  (when-not (tool-result? x)
    (throw (ex-info
             "Invalid tool result"
             {:type :vis/invalid-tool-result, :value x, :explain (s/explain-data ::envelope x)})))
  x)
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
  (let [metadata (or metadata {})
        t (now-ms)
        started (long (or (:started-at-ms metadata) t))
        finished (long (or (:finished-at-ms metadata) t))
        duration (long (or (:duration-ms metadata) (max 0 (- finished started))))]
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
  (let [meta* (meta envelope)
        merged (-> envelope
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
  (let [header (str (.getName (class t)) ": " (or (ex-message t) ""))
        frames (->> (.getStackTrace t)
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
      (not (str/blank? trace)) (assoc :trace trace))))
(defn- envelope-of
  "Internal builder used by both `success` and `failure`. Accepts
   only the canonical shape:

     :result   raw Python eval value; stored under `:result`
     :op       op symbol e.g. :cat (nil for raw user code)
     :metadata free-form aux map: :tool, :extension, :source,
               :paths, :hit-count, :command, :started-at-ms,
               :finished-at-ms, :duration-ms, etc.
     :emit     optional CTX mutation payload routed through the
               engine after the tool returns. Shape:
                 {:tasks {entry-key partial-task-map}
                  :facts {entry-key partial-fact-map}}
               The wrapper applies each entry via
               `ctx-loop/apply-and-record!` so the surface matches
               a model-emitted `(task-set! ...)` / `(fact-set! ...)`
               exactly: same engine FSM checks, dedup, warnings.

   Produces a flat `:envelope` map."
  [{:keys [result op metadata emit]} success? error]
  (cond->
    {:result result, :success? success?, :error error, :metadata (normalize-metadata metadata)}
    (map? emit) (assoc :emit emit)
    op (assoc :symbol
         op :tag
         (op-tag op))
    :always assert-tool-result!))
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
  [{:keys [error throwable], :as args}]
  (let [err (or error (when throwable (normalize-error throwable)))] (envelope-of args false err)))
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
   `env-python/map-polyglot-error`, so the block `:phase` is `:preflight`.

   Optional opts:
     :form-source  the verbatim source the form was built from;
                    embedded in `:block.source` so the model sees its
                    own input echoed back.
     :hint          override / pre-supply a recovery hint string."
  [^Throwable t & [{:keys [form-source hint]}]]
  (let [d (ex-data t)
        cause (some-> t .getCause)
        message (or (not-empty (ex-message t)) (.getName (class t)))
        trace (try (normalize-trace t) (catch Throwable _ nil))
        block (when form-source {:source form-source, :phase :preflight})
        cause-data (when cause (ex-data cause))
        ;; A tool that returned `{:success? false :error <map>}` is
        ;; un-structured back into a thrown ExceptionInfo by
        ;; `tool-result->public-value`. The structured `:error` map
        ;; carries the model-actionable info (`:reason`, `:failures`,
        ;; `:loop-hint`, `:checks` …). Lift it into `:data` so the
        ;; iteration trailer (`error-lines` in `ctx.clj`) renders it as
        ;; `;; ! data {…}`. Without this lift the model only sees
        ;; `:message` + `:trace` and has to decode `:reason` from prose.
        tool-error-data (when (= :vis/tool-failure (:type d))
                          (let [e (:error d)]
                            (when (map? e)
                              (not-empty (cond-> {}
                                           (some? (:reason e)) (assoc :reason (:reason e))
                                           (seq (:failures e)) (assoc :failures (:failures e))
                                           (seq (:checks e)) (assoc :checks (:checks e))
                                           (some? (:loop-hint e)) (assoc :loop-hint (:loop-hint e))
                                           (some? (:mode e)) (assoc :mode (:mode e)))))))]
    (cond-> {:message message}
      (not (str/blank? trace)) (assoc :trace trace)
      hint (assoc :hint hint)
      block (assoc :block block)
      cause-data (assoc :cause-data cause-data)
      tool-error-data (assoc :data tool-error-data))))
(defn- render-error-context-text
  "Babashka-style source context text for `render-error-context` IR."
  [{:keys [source row col opened-loc]} {:keys [form-start-row form-end-row]}]
  (when (string? source)
    (let [;; opened-loc beats row/col for arrow placement.
          arrow-row (or (:row opened-loc) row)
          arrow-col (or (:col opened-loc) col)
          lines (vec (str/split source #"\n" -1))
          total (count lines)
          gutter-w (count (str total))
          in-form? (fn [ln-1based]
                     (and form-start-row form-end-row (<= form-start-row ln-1based form-end-row)))
          fmt-line (fn [idx0]
                     (let [ln (inc idx0)
                           marker (if (in-form? ln) ">" " ")]
                       (format (str "%s %" gutter-w "d: %s") marker ln (nth lines idx0))))
          arrow-line (when (and arrow-row arrow-col (<= 1 arrow-row total))
                       (str (apply str (repeat (+ gutter-w 4) \space))
                         (apply str (repeat (max 0 (dec arrow-col)) \space))
                         "^---"))
          arrow-idx0 (when arrow-line (dec arrow-row))]
      (->> (range total)
        (mapcat (fn [idx0] (cond-> [(fmt-line idx0)] (= idx0 arrow-idx0) (conj arrow-line))))
        (str/join "\n")))))
(defn render-error-context
  "Render the source from an `:error :block` map as canonical IR.

   Layout: babashka-style. Every line is gutter-numbered. The failing
   line gets a `^---` arrow under the exact column. Lines belonging to
   the failing form get a `>` gutter prefix. No truncation.

   Returns `[:ir ...]`. Channel error paths use this directly; no Markdown
   or string renderer hop."
  ([block] (render-error-context block nil))
  ([block opts]
   (if-let [text (render-error-context-text block opts)]
     [:ir {} [:code {:lang "text"} text]]
     [:ir {}])))
;; =============================================================================
;; Symbol entry spec
;; =============================================================================
;; Symbol name bound in the Python sandbox.
(s/def :ext.symbol/symbol symbol?)
;; Implementation function the LLM calls from :code blocks.
(s/def :ext.symbol/fn fn?)
;; One-liner description shown in the sandbox var's docstring.
(s/def :ext.symbol/doc non-blank-string?)
;; Original host-side source form for REPL `source(alias.sym)` in Python.
(s/def :ext.symbol/source non-blank-string?)
;; Argument signatures, e.g. '([term] [term opts]).
;; Shown in var meta :arglists and used by `render-symbol-line` to
;; build the model-facing call form (e.g. `(cat path)`).
(s/def :ext.symbol/arglists (s/and vector? seq))
;; Raw callable helpers compose as normal Clojure values. They bypass the
;; observed-tool envelope/channel wrapper and return their function's
;; value directly in Python.
(s/def :ext.symbol/raw? boolean?)
;; Entry decorator: (fn [env f args] -> map). Wraps :fn on the way in.
(s/def :ext.symbol/before-fn fn?)
;; Exit decorator: (fn [env f args result] -> map). Wraps :fn on the way out.
(s/def :ext.symbol/after-fn fn?)
;; Error decorator: (fn [err env f args] -> map). Called when :fn throws.
(s/def :ext.symbol/on-error-fn fn?)
;; Renderer for this symbol's runtime channel result (TUI, Telegram, ...).
;; Receives only the unwrapped `:result` value. MUST return the
;; `{:summary <ir-or-zones> :display <ir>}` contract (`::render-fn-result`):
;;   :summary — the badge row, EITHER canonical IR (one [:p ...]; first
;;              [:strong ...] = label) OR a zone map
;;              {:left … :center? … :right?}.
;;   :display — canonical channel IR ([:ir ...]); the full expanded body.
;; Returning bare IR is invalid and rejected on first call. Mandatory for
;; observed fn-symbols; raw helpers skip rendering.
;;
;; NOTE: there is intentionally no model-facing renderer. The RLM reads
;; the actual Python form value out of the per-iteration trailer; no per-tool
;; string curation happens.
(s/def :ext.symbol/render-fn fn?)
;; Optional override for failure rendering. Receives the tool-result
;; envelope only. MUST return the same `{:summary :display}` contract as
;; `:render-fn`. When absent, the engine uses `default-error-result`.
(s/def :ext.symbol/render-error-fn fn?)
;; Op classification carried INLINE on the symbol entry — every
;; observed tool declares its tag right on `vis/symbol`'s opts map,
;; and `register-extension!` walks the symbol vec to populate the
;; op-keyword -> tag index automatically.
(s/def :ext.symbol/tag #{:observation :mutation})
;; Optional REGISTER-TIME contract sample (Phase 7). When present, it is a
;; representative tool-result value `register-extension!` feeds through
;; `:render-fn` (and, if declared, `:render-error-fn`) at registration time,
;; HARD-rejecting the extension when the render-fn does not return the
;; `{:summary :display}` contract (`::render-fn-result`). This makes the
;; contract a register-time gate, not only a sink-write assertion. Optional
;; because render-fns are written against their tool's concrete result shape
;; and a fabricated value would spuriously fail; tools that supply a sample
;; get the strongest, earliest rejection.
(s/def :ext.symbol/render-sample any?)
;; High-fan-out batch-hint threshold (Phase 4). When a single display-block
;; accumulates MORE than this many ops with the same `:op`, the iteration
;; surfaces a soft "BATCH HINT" note nudging the agent to call the tool once
;; with a vector argument instead of N times. Optional per-tool override of
;; the default threshold (`iteration/default-batch-hint-threshold`).
(s/def :ext.symbol/batch-hint pos-int?)
;; Hidden alias symbols still bind into the Python sandbox but are omitted from the
;; model-facing prompt symbol catalog (see prompt.clj). Used for back-compat
;; aliases like git/add! ↔ git/add and git/commit ↔ git/commit! so both
;; spellings resolve while only the canonical name is advertised.
(s/def :ext.symbol/hidden? boolean?)
;; Plain value bound in the sandbox (constant, data, config).
;; Mutually exclusive with :ext.symbol/fn.
(s/def :ext.symbol/val some?)
(s/def ::fn-symbol-entry
  (s/keys :req [:ext.symbol/symbol :ext.symbol/fn :ext.symbol/doc :ext.symbol/arglists]
    :opt [:ext.symbol/raw? :ext.symbol/hidden? :ext.symbol/tag :ext.symbol/batch-hint
          :ext.symbol/render-sample :ext.symbol/render-fn :ext.symbol/before-fn
          :ext.symbol/after-fn :ext.symbol/on-error-fn :ext.symbol/source
          :ext.symbol/render-error-fn]))
(s/def ::val-symbol-entry
  (s/keys :req [:ext.symbol/symbol :ext.symbol/val :ext.symbol/doc] :opt [:ext.symbol/source]))
(s/def ::symbol-entry
  (s/or :fn ::fn-symbol-entry
    :val ::val-symbol-entry))
;; =============================================================================
;; Extension spec
;; =============================================================================
;; Logical extension id, e.g. "foundation" or "github-copilot".
(s/def :ext/name non-blank-string?)
;; Extension-level documentation - describes what this bundle provides.
(s/def :ext/description non-blank-string?)
;; Namespace(s) whose source loads/registers this extension. `vis/extension`
;; captures this from the callsite; generated logical extensions may share it.
(s/def :ext/source-nses (s/coll-of symbol? :kind vector?))
;; Top-level kind - the *category* of surface this extension
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
;; Extension-owned file boundary declarations. The callback receives the
;; live environment and returns rules in first-match-wins order *within
;; that extension*. Global enforcement across extensions is handled by
;; the editing core with most-restrictive-wins semantics.
(s/def :ext.protected-path/glob string?)
(s/def :ext.protected-path/access #{:read-only :read-write :none})
(s/def :ext.protected-path/hint string?)
(s/def ::protected-path
  (s/keys :req-un [:ext.protected-path/glob :ext.protected-path/access :ext.protected-path/hint]))
(s/def ::protected-paths-result (s/coll-of ::protected-path :kind vector?))
(s/def :ext/protected-paths fn?)
;; Optional structured data merged into engine `ctx` before every model call.
;; Return a map such as `{:project {...}}`; engine-owned keys still win on
;; collision.
(s/def :ext/ctx fn?)
;; ----------------------------------------------------------------------------
;; Hooks: the single mechanism extensions use to plug into the turn lifecycle.
;; A hook is a named callback that fires at a declared `:phase`; its `:fn`
;; receives a phase-shaped context map.
;;
;; Canonical phase keywords:
;;   :turn.iteration/start — every iteration, BEFORE the model call. Returns
;;                           nil | {:text :importance?}; text flows into
;;                           `(get-in ctx [:session :hints])`.
;;   :turn.answer/validate — when a `(done ...)` form produced a candidate
;;                           final answer. Return nil to accept or
;;                           {:reject true :message ... :hint ...} to reject.
;;
;; Legacy / unused phases are intentionally rejected. No compatibility shim hides
;; stale extension code.
;;
;; Every hook declares :id, :doc, :phase, :fn — the contract is explicit
;; and the failure surface reviewable. One extension can ship many
;; independent hooks as a flat vector. Exceptions thrown in :fn are
;; caught + logged via Telemere; a misbehaving hook never blocks the
;; loop or starves siblings.
;;
;; Start hooks do NOT block evaluation. They emit advisory
;; `(get-in ctx [:session :hints])` entries. For HARD final-answer
;; rejection, use :turn.answer/validate.
;; ----------------------------------------------------------------------------
(def canonical-hook-phases
  "Canonical namespaced lifecycle phases accepted by `:ext/hooks`."
  #{:turn.iteration/start :turn.answer/validate})
(defn hook-phase?
  "True when `phase` is a canonical namespaced hook phase."
  [phase]
  (contains? canonical-hook-phases phase))
(def canonical-hook-lifetimes
  "Hook-task lifetime policies. Controls how long an emitted hook-task
   lingers in `:session/tasks` after the hint stops firing.

     :session    Default. Task survives across turns and is GC'd by the
                 standard TTL machinery (`TTL-TASK-DONE`/
                 `TTL-TASK-CANCELLED` turns after terminal status). Right
                 for cross-turn concerns like
                 `:vis.foundation/session-title` whose work product (the
                 session title) is itself session-scoped.

     :turn       Ephemeral. Task is dropped from `:session/tasks` at
                 `advance-turn` regardless of status. If the originating
                 hint condition still holds, the next iter recreates the
                 task; if not, it stays gone. Right for transient signals
                 like `:vis.foundation/context-pressure` whose advisory
                 value evaporates the moment the next request's input
                 size drops below threshold. Prevents the cargo-cult
                 pattern where a stale `:done :validated? false` task
                 keeps showing up in the CTX render for 6 turns and the
                 model keeps re-emitting `(task-set! … :done)` to silence
                 it (Vis conv 11d4f817 / t14–t16).

     :iteration  Hyper-transient. Task is dropped at `advance-iter`
                 (every iter boundary), not just turn boundary. Right
                 for hints whose firing condition is recomputed from
                 per-iter state (e.g. a one-iter retry-shape warning,
                 an in-flight tool-call status banner). The next iter's
                 hook fire is the single source of truth; if the
                 condition still holds the task re-materialises
                 immediately."
  #{:iteration :turn :session})
(defn hook-lifetime?
  "True when `lifetime` is one of the canonical hook-task lifetimes."
  [lifetime]
  (contains? canonical-hook-lifetimes lifetime))
(s/def :ext.hook/id keyword?)
(s/def :ext.hook/doc non-blank-string?)
(s/def :ext.hook/phase (s/and keyword? hook-phase?))
(s/def :ext.hook/fn fn?)
(s/def :ext.hook/lifetime (s/and keyword? hook-lifetime?))
(s/def ::hook
  (s/keys :req-un [:ext.hook/id :ext.hook/doc :ext.hook/phase :ext.hook/fn]
    :opt-un [:ext.hook/lifetime]))
(s/def :ext/hooks (s/coll-of ::hook :kind vector?))
(s/def :ext.hook.return/text non-blank-string?)
(s/def :ext.hook.return/importance keyword?)
(s/def ::iteration-start-hint
  (s/keys :req-un [:ext.hook.return/text] :opt-un [:ext.hook.return/importance]))
(s/def :ext.hook.return/hint non-blank-string?)
(s/def :ext.hook.return/reject true?)
(s/def :ext.hook.return/message non-blank-string?)
(s/def ::answer-validation-reject
  (s/keys :req-un [:ext.hook.return/reject]
    :opt-un [:ext.hook.return/message :ext.hook.return/hint]))
;; Channel contributions let extensions add passive UI/command parts to
;; concrete channel slots without requiring those channel namespaces.
;; Shape: {:ext/channel-contributions {:tui.slot/commands [{:id :voice/input
;;                                                          :fn f}]}}
;; The slot key declares where the contribution goes; the channel owns that
;; slot's fn arity + return contract.
(defn- channel-slot?
  [x]
  (and (keyword? x) (when-let [ns (namespace x)] (str/ends-with? ns ".slot"))))
(defn- channel-slot->channel-id
  [slot]
  (let [ns (namespace slot)]
    (when-not (and (keyword? slot) ns (str/ends-with? ns ".slot"))
      (throw (ex-info "Channel contribution slot must be a qualified keyword ending in .slot"
               {:type :extension/invalid-channel-contribution-slot, :slot slot})))
    (keyword (subs ns 0 (- (count ns) (count ".slot"))))))
(s/def :ext.channel-contribution/id keyword?)
(s/def :ext.channel-contribution/fn ifn?)
(s/def ::channel-contribution
  (s/keys :req-un [:ext.channel-contribution/id :ext.channel-contribution/fn]))
(s/def :ext/channel-contributions
  (s/map-of channel-slot? (s/coll-of ::channel-contribution :kind vector?)))
;; ----------------------------------------------------------------------------
;; Slash commands
;;
;; Declarative cross-channel slash surface, mirroring `:ext/hooks` /
;; `:ext/channel-contributions`. NO global atom, NO `register-slash!`.
;; Every extension carries its slash specs on `:ext/slash-commands`;
;; the engine derives the active slash set by walking
;; `(active-extensions)` at lookup time (see `internal/slash.clj`).
;;
;; Coordinates: `:slash/parent` is the lineage vec (top-level commands
;; like `/workspace` have `:parent []`; `/workspace apply` has
;; `:parent ["workspace"]`, etc.). The canonical full path of a slash
;; is `(conj parent name)`. `register-extension!` refuses two
;; extensions declaring the same `[parent name]`.
;; ----------------------------------------------------------------------------
(s/def :slash/name non-blank-string?)
(s/def :slash/parent (s/coll-of non-blank-string? :kind vector?))
(s/def :slash/doc non-blank-string?)
(s/def :slash/usage non-blank-string?)
(s/def :slash/run-fn ifn?)
(s/def :slash/requires (s/coll-of #{:session :workspace :channel} :kind set?))
(s/def :slash/availability-fn ifn?)
(s/def :slash/subcommands (s/coll-of non-blank-string? :kind vector?))
(s/def ::slash
  (s/keys :req [:slash/name]
    :opt [:slash/parent :slash/doc :slash/usage :slash/run-fn :slash/requires
          :slash/availability-fn :slash/subcommands]))
(s/def :ext/slash-commands (s/coll-of ::slash :kind vector?))
(defn slash-path
  "Canonical full path vec of a slash spec: parent ++ [name]. Used as the
   lookup key in `internal/slash.clj`."
  [slash-spec]
  (conj (vec (:slash/parent slash-spec)) (:slash/name slash-spec)))
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
;; knobs appear under Extensions -> <extension> instead of hardcoded host
;; buckets.
(s/def :ext.setting/key
  (s/or :keyword keyword?
    :string non-blank-string?))
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
;; Optional dependency declaration. Vector of logical extension names.
(s/def :ext/requires (s/coll-of non-blank-string? :kind vector?))
;; Semver version string, e.g. "1.0.0", "0.3.1-SNAPSHOT".
(s/def :ext/version non-blank-string?)
;; Author name or org - the entity that *created* the extension
;; (e.g. "Blockether", "Acme Corp.").
(s/def :ext/author non-blank-string?)
;; Owner of the *package* - the project / distribution that ships
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
(s/def :ext/cli (s/coll-of :com.blockether.vis.internal.registry/command :kind vector?))
;; Channels exported by this extension.
(s/def :ext/channels (s/coll-of :com.blockether.vis.internal.registry/channel :kind vector?))
;; LLM providers exported by this extension. Each entry mirrors the
;; canonical provider shape; we accept any IFn (or absence) for the
;; optional runtime fns so a minimal provider doesn't ship no-op stubs.
(let [or-nil-or-fn (fn [k] #(let [v (get % k ::absent)] (or (= v ::absent) (ifn? v))))]
  (s/def ::provider-entry
    (s/and map?
      #(not (contains? % :provider/prompt-fn))
      #(keyword? (:provider/id %))
      #(non-blank-string? (:provider/label %))
      (or-nil-or-fn :provider/status-fn)
      (or-nil-or-fn :provider/logout-fn)
      (or-nil-or-fn :provider/detect-fn)
      (or-nil-or-fn :provider/auth-fn)
      (or-nil-or-fn :provider/get-token-fn)
      (or-nil-or-fn :provider/refresh-token-fn)
      (or-nil-or-fn :provider/limits-fn)
      (or-nil-or-fn :provider/on-selected-fn))))
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
;; diagnostic message maps. Replaces the previous doctor check vector
;; of `{:check/id :check/name :check/description :check/run-fn}` maps -
;; the metadata fields (`:check/name`, `:check/description`) were never
;; surfaced anywhere; only `:check/id` made it onto messages, and the
;; extension can stamp `:check-id` on its own messages just as easily
;; without the host walking a vec of structured maps. Plan §1 Q19 + §10.
;; Authors who don't ship checks just omit the field.
;;
;; Naming follows the `:ext/<surface>-fn` convention already used for
;; `:ext/activation-fn` - ONE fn, called by the host, returns data.
;;
;; Per-message expectations (host coerces missing/invalid):
;;   {:level :info|:warn|:error
;;    :message "..."            ; required, non-blank
;;    :remediation "..."        ; optional; renders as `-> ...` indented line
;;    :check-id ::keyword     ; optional; renders as the prefix
;;    :data {...}}              ; optional; passthrough for callers
(s/def :ext/doctor-fn fn?)
;; Python sandbox contribution.
(s/def :ext.engine/symbols (s/coll-of ::symbol-entry :kind vector?))
;; Map of fully-qualified Java classes to expose in the sandbox.
(s/def :ext.engine/classes
  (s/and map?
    #(every? symbol? (keys %))
    #(every? class? (vals %))))
;; Map of short-name imports for Java classes.
(s/def :ext.engine/imports
  (s/and map?
    #(every? symbol? (keys %))
    #(every? symbol? (vals %))))
;; Optional Python namespace alias for this extension's symbols.
(s/def :ext.engine/ns (s/and symbol? #(nil? (namespace %))))
(s/def :ext.engine/alias (s/and symbol? #(nil? (namespace %))))
;; Built-in extensions ship in the main jar and bind their symbols BARE into the
;; sandbox ns (no alias), like the engine verbs. Mutually exclusive with :alias.
(s/def :ext.engine/builtin? boolean?)
(s/def :ext/engine
  (s/keys :opt [:ext.engine/ns :ext.engine/alias :ext.engine/builtin? :ext.engine/symbols :ext.engine/classes
                :ext.engine/imports]))
;; Canonical source markers attached to registered extensions via the
;; sidecar atom. Also surfaced in ctx :extensions / extension summaries
;; and stamped onto tool-result info.
(s/def ::alias symbol?)
(s/def ::name non-blank-string?)
(s/def ::description non-blank-string?)
(s/def ::kind non-blank-string?)
(s/def ::version non-blank-string?)
(s/def ::author non-blank-string?)
(s/def ::owner non-blank-string?)
(s/def ::license non-blank-string?)
(s/def ::source-paths (s/coll-of string? :kind vector?))
(s/def ::source-mtime-max integer?)
(s/def ::source-hash-sha256 (s/nilable (s/and string? #(= 64 (count %)))))
(s/def ::registry-id symbol?)
(s/def ::extension-info
  (s/keys :req-un [::name ::source-paths ::source-mtime-max ::source-hash-sha256]
    :opt-un [::alias ::description ::kind ::version ::author ::owner ::license
             ::registry-id]))
(defn ext-engine [ext] (or (:ext/engine ext) {}))
(defn ext-symbols [ext] (vec (or (get-in ext [:ext/engine :ext.engine/symbols]) [])))
(defn ext-classes [ext] (or (get-in ext [:ext/engine :ext.engine/classes]) {}))
(defn ext-imports [ext] (or (get-in ext [:ext/engine :ext.engine/imports]) {}))
(defn ext-alias-symbol [ext] (get-in ext [:ext/engine :ext.engine/alias]))
(defn ext-builtin?
  "True when this extension is a BUILT-IN: its symbols bind BARE into the
   sandbox ns (no alias), alongside the engine verbs. See
   `builtin-sandbox-bindings`."
  [ext]
  (boolean (get-in ext [:ext/engine :ext.engine/builtin?])))
(defn ext-engine-ns
  [ext]
  (or (get-in ext [:ext/engine :ext.engine/ns])
    (when-let [alias (ext-alias-symbol ext)]
      (clojure.core/symbol (str "vis.ext." (name alias))))))
(defn ext-alias
  [ext]
  (when-let [alias (ext-alias-symbol ext)] {:ns (ext-engine-ns ext), :alias alias}))
(defn ext-source-nses [ext] (vec (or (:ext/source-nses ext) [])))
(defn ext-display-name [ext] (:ext/name ext))
(defn- ns-alias-required-when-symbols?
  "Symbols need a home: an `:ext.engine/alias` (third-party → aliased ns) OR
   `:ext.engine/builtin? true` (core → bare in the sandbox ns). One is required
   when the extension contributes symbols."
  [ext]
  (or (empty? (ext-symbols ext)) (some? (ext-alias-symbol ext)) (ext-builtin? ext)))
(defn- kind-required-when-symbols? [ext] (or (empty? (ext-symbols ext)) (some? (:ext/kind ext))))
(s/def ::extension
  (s/and (s/keys :req [:ext/name :ext/description]
           :opt [:ext/source-nses :ext/kind :ext/activation-fn :ext/engine :ext/prompt :ext/ctx
                 :ext/protected-paths :ext/hooks :ext/env :ext/settings :ext/theme
                 :ext/requires :ext/version :ext/author :ext/owner :ext/license :ext/cli
                 :ext/channels :ext/providers :ext/persistance :ext/channel-contributions
                 :ext/slash-commands :ext/doctor-fn])
    ns-alias-required-when-symbols?
    kind-required-when-symbols?))
;; =============================================================================
;; Symbol helpers (builder fns)
;; =============================================================================
(defn render-string
  "Render fn for string-shaped `:result`; returns literal IR."
  [result]
  (literal-ir (str result)))
(defn- validate-symbol-entry!
  "Assert a symbol entry conforms to ::symbol-entry. Throws on violation."
  [entry]
  (when-not (s/valid? ::symbol-entry entry)
    (throw (ex-info (str "Invalid symbol '" (:ext.symbol/symbol entry)
                      "':\n" (with-out-str (s/explain ::symbol-entry entry)))
             {:type :extension/invalid-symbol,
              :symbol (:ext.symbol/symbol entry),
              :explain (s/explain-data ::symbol-entry entry)})))
  entry)
(defn- var-source
  "Best-effort source form for a host Var. Stored on extension symbol entries
   so the Python sandbox's `source(...)` can show source for aliased
   extension vars whose sandbox namespace (`v.`, ...) is synthetic."
  [v]
  (let [m (meta v)
        ns (:ns m)
        nm (:name m)]
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
       {:type :extension/symbol-not-a-var, :given v}))
   (let [m (meta v)
         nm (:name m)
         doc-fn (:doc-fn opts)
         doc (or (:doc opts) (:doc m) (when doc-fn (doc-fn (or (:symbol opts) nm) v)))
         al (or (:arglists opts) (:arglists m) (when (:raw? opts) '([& args])))]
     (when-not (non-blank-string? doc)
       (anomaly/incorrect! (str "Var " v
                             " is missing a docstring; extension symbols inherit "
                             ":doc from the underlying defn (no side maps).")
         {:type :extension/missing-doc, :var v}))
     ;; defn auto-attaches :arglists as a LIST (e.g. '([x] [x y])); manual
     ;; ^{:arglists ...} likewise. The downstream spec requires vector?.
     ;; Accept any non-empty sequential and coerce to a vector here so the
     ;; spec stays strict at the storage boundary while callers stay free
     ;; to use either shape.
     (when (and require-arglists? (not (and (sequential? al) (seq al))))
       (anomaly/incorrect! (str "Var " v
                             " is missing :arglists in its metadata; extension fn "
                             "symbols inherit :arglists from the underlying defn.")
         {:type :extension/missing-arglists, :var v}))
     (let [source (var-source v)]
       (cond-> {:symbol nm, :doc doc, :arglists (when (seq al) (vec al))}
         source (assoc :source source))))))
(defn- build-symbol-entry
  "Shared core that turns `{:symbol :fn :doc :arglists :source}` plus opts into
   a validated `::fn-symbol-entry`. Observed tools keep their symbol-specific
   channel renderer; raw helpers do not render. Used by both the
   var-based public API and the test-friendly direct-args form below.

   Opts may carry `:tag :observation | :mutation`. When present,
   `register-extension!` walks the symbol vec and auto-populates the
   global op-keyword -> tag index so call-sites don't need an
   out-of-band registration step per symbol."
  [{sym :symbol, :keys [fn doc arglists source]} opts]
  (let [raw? (true? (:raw? opts))]
    (validate-symbol-entry!
      (cond-> #:ext.symbol{:symbol sym, :fn fn, :doc doc, :arglists arglists}
        raw? (assoc :ext.symbol/raw? true)
        (:hidden? opts) (assoc :ext.symbol/hidden? true)
        source (assoc :ext.symbol/source source)
        (:tag opts) (assoc :ext.symbol/tag (:tag opts))
        (:batch-hint opts) (assoc :ext.symbol/batch-hint (:batch-hint opts))
        (contains? opts :render-sample) (assoc :ext.symbol/render-sample (:render-sample opts))
        (:render-fn opts) (assoc :ext.symbol/render-fn (:render-fn opts))
        (:before-fn opts) (assoc :ext.symbol/before-fn (:before-fn opts))
        (:after-fn opts) (assoc :ext.symbol/after-fn (:after-fn opts))
        (:on-error-fn opts) (assoc :ext.symbol/on-error-fn (:on-error-fn opts))
        (:render-error-fn opts) (assoc :ext.symbol/render-error-fn (:render-error-fn opts))))))
(defn symbol
  "Build a function symbol entry FROM A CLOJURE VAR.

   The 3-arg form `(symbol sym-name f opts)` is a test-friendly direct
   constructor: pass the sandbox-visible symbol, the implementation fn, and
   an opts map whose `:doc` / `:arglists` are read directly from opts
   instead of var meta. Production code uses the var form.

   The var supplies `:symbol` (var name), `:fn` (the var's value), `:doc` and
   `:arglists` (read from var metadata - i.e. the underlying defn's
   docstring + arglists). Pass it as `#'my-tool`.

   Observed tools return canonical internal envelope maps and must provide
   a symbol-specific channel renderer. The model-facing surface is the
   per-iteration trailer (real Python form values); no per-tool model-side
   render exists.

   Raw helpers pass `:raw? true` and return plain values directly, with no
   envelope enforcement, channel sink, or tool metadata.

   Optional opts:
     :symbol      - override the Python sandbox name (default: var name).
     :doc-fn      - compute doc lazily from `(sym v)` when the var
                    lacks a docstring (third-party vars only).
     :raw?        - true for plain composable helpers.
     :tag         - REQUIRED `:observation | :mutation` for observed
                    tools (unless `:raw? true`).
     :render-fn   - REQUIRED for observed tools; returns the
                    `{:summary :display}` contract.
     :render-sample - OPTIONAL representative tool-result value;
                    `register-extension!` smoke-calls `:render-fn` (and
                    `:render-error-fn`) with it and HARD-rejects a
                    non-`{:summary :display}` result at register time.
     :before-fn :after-fn :on-error-fn :render-error-fn

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
       {:type :extension/symbol-not-a-var, :given v}))
   (let [{default-symbol :symbol, :keys [doc arglists source]} (var-meta v true opts)
         sym (or (:symbol opts) default-symbol)
         f @v]
     (when-not (fn? f)
       (anomaly/incorrect!
         (str "Var " v " does not hold a function; use vis/value for plain values.")
         {:type :extension/symbol-not-a-fn, :var v}))
     (build-symbol-entry {:symbol sym, :fn f, :doc doc, :arglists arglists, :source source} opts))))
(defn helper
  "Build a raw callable helper entry FROM A CLOJURE VAR.

   Helpers are bound as plain values in Python, not observed tools: no envelope
   validation, no channel renderer. Use for composable host helper functions
   such as `snapshot`, not for user-observable tool calls."
  ([v] (helper v nil))
  ([v opts]
   (if (var? v)
     (let [{default-symbol :symbol, :keys [doc arglists source]}
           (var-meta v true (assoc opts :raw? true))
           sym (or (:symbol opts) default-symbol)
           val @v]
       (when-not (fn? val)
         (anomaly/incorrect!
           (str "Var " v " does not hold a function; use vis/value for plain values.")
           {:type :extension/helper-not-a-fn, :var v}))
       (validate-symbol-entry! (cond->
                                 #:ext.symbol{:symbol sym, :val val, :doc doc, :arglists arglists}
                                 source (assoc :ext.symbol/source source))))
     (anomaly/incorrect! "vis/helper expects a Clojure var (e.g. #'my-helper)."
       {:type :extension/helper-not-a-var, :given v}))))
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
     (let [opts opts-or-val
           {default-symbol :symbol, :keys [doc source]} (var-meta v false opts)
           sym (or (:symbol opts) default-symbol)
           val (if (contains? opts :val) (:val opts) @v)
           entry (cond-> #:ext.symbol{:symbol sym, :val val, :doc doc}
                   source (assoc :ext.symbol/source source))]
       (validate-symbol-entry! entry))
     (anomaly/incorrect!
       "vis/value expects a Clojure var (e.g. #'my-const); use the 3-arg form (value sym-name val opts) for test-only direct construction."
       {:type :extension/value-not-a-var, :given v})))
  ([sym-name val opts]
   ;; Test-only direct-construction arity. `:doc` comes from opts.
   (let [doc (:doc opts)]
     (when-not (non-blank-string? doc)
       (anomaly/incorrect! (str "3-arg value '" sym-name "' missing :doc in opts.")
         {:type :extension/missing-doc, :symbol sym-name}))
     (validate-symbol-entry! #:ext.symbol{:symbol sym-name, :val val, :doc doc}))))
(defn- arglist->call-form
  [alias-sym sym-name arglist]
  (let [args (->> arglist
               (remove #{'&})
               (map str)
               (str/join " "))
        target (if alias-sym (str alias-sym "/" sym-name) (str sym-name))]
    (str "(" target (when (seq args) (str " " args)) ")")))
(defn- render-symbol-line
  [alias-sym entry]
  (let [{sym-name :ext.symbol/symbol, doc :ext.symbol/doc, arglists :ext.symbol/arglists} entry
        callable? (or (:ext.symbol/fn entry) (and (fn? (:ext.symbol/val entry)) (seq arglists)))]
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
    (let [lines (->> (str/split-lines (str/replace (str/replace text "\r\n" "\n") "\r" "\n"))
                  (mapv #(str/replace % #"[ \t]+$" ""))
                  trim-prompt-edge)
          indent (if-let [xs (seq (remove str/blank? lines))]
                   (apply min (map prompt-line-indent xs))
                   0)
          deindented
          (mapv (fn [line] (if (str/blank? line) "" (subs line (min indent (count line))))) lines)
          collapsed (reduce (fn [acc line]
                              (if (str/blank? line)
                                (if (= "" (peek acc)) acc (conj acc ""))
                                (conj acc line)))
                      []
                      deindented)]
      (str/join "\n" collapsed))))
(defn render-prompt
  "Render canonical :ext/prompt text from symbol docstrings + arglists.

   Accepts an extension map or any map with:
   - :ext/description      or :heading
   - :ext.engine/alias optional {:alias 'v}
   - :ext.engine/symbols  vector of symbol + value entries
   - :usage-note   optional extra note added to the heading
   - :notes        optional string or seq of extra lines appended verbatim

   Returns a prompt string suitable for :ext/prompt."
  [{:keys [heading usage-note notes], :as opts}]
  (let [alias-sym (ext-alias-symbol opts)
        symbols (or (:symbols opts) (ext-symbols opts))
        heading (or heading (:ext/description opts) "Extension tools")
        header-notes (vec (remove nil?
                            [(when alias-sym (str "use " alias-sym "/ prefix"))
                             (when (non-blank-string? usage-note) usage-note)]))
        extra-lines (cond (nil? notes) []
                      (string? notes) [notes]
                      (sequential? notes) (vec notes)
                      :else [(str notes)])
        body-lines (mapv #(render-symbol-line alias-sym %) symbols)]
    (normalize-prompt-text (str/join "\n"
                             (concat [(str heading
                                        (when (seq header-notes)
                                          (str " (" (str/join "; " header-notes) ")")))]
                               body-lines
                               extra-lines)))))
;; =============================================================================
;; Normalization + validation
;; =============================================================================
(defn- normalize-prompt
  [prompt]
  (cond (nil? prompt) nil
    (fn? prompt) (fn [env]
                   (let [result (prompt env)]
                     (if (string? result) (normalize-prompt-text result) result)))
    (string? prompt) (constantly (normalize-prompt-text prompt))
    :else (throw (ex-info ":ext/prompt must be a string or (fn [env] string)"
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
  (doseq [sym-entry (ext-symbols ext)
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
          {:type :extension/missing-op-tag,
           :extension (:ext/name ext),
           :symbol (:ext.symbol/symbol sym-entry),
           :op op,
           :allowed op-tags}))))
  ext)
(defn- validate-symbol-renderers!
  "Fail closed: every observed tool owns its channel rendering. The model-
   facing surface is the trailer (real Python form values); no second
   model-side render is required or accepted.

   Two register-time gates (Phase 7):

   1. Presence — every observed (non-raw) tool MUST declare a `:render-fn`.

   2. Contract — when a tool also declares a `:ext.symbol/render-sample`
      (a representative tool-result value), `:render-fn` (and, if present,
      `:render-error-fn`) is SMOKE-CALLED here and HARD-rejected unless it
      returns the `{:summary :display}` contract (`::render-fn-result`). This
      is the register-time enforcement of the render contract — not only a
      sink-write assertion. We can only smoke-call against a tool-declared
      sample because render-fns are written for their tool's concrete result
      shape; a fabricated value would spuriously fail, so the sample is the
      tool's own representative payload.

   Independently, `assert-render-fn-result!` still runs at the single call
   site `render-value` on every sink write, so a tool WITHOUT a sample is
   still hard-rejected on its first real call. There is no raw-IR
   fallback path."
  [ext]
  (doseq [sym-entry (ext-symbols ext)
          :when (and (:ext.symbol/fn sym-entry) (not (:ext.symbol/raw? sym-entry)))]
    (let [sym (:ext.symbol/symbol sym-entry)]
      (when-not (:ext.symbol/render-fn sym-entry)
        (anomaly/incorrect!
          (str "Extension '" (:ext/name ext) "' symbol '" sym "' is missing :render-fn.")
          {:type :extension/missing-renderer, :extension (:ext/name ext), :symbol sym}))
      ;; Register-time contract gate: smoke-call against the declared sample.
      (when (contains? sym-entry :ext.symbol/render-sample)
        (let [sample (:ext.symbol/render-sample sym-entry)]
          (assert-render-fn-result! ((:ext.symbol/render-fn sym-entry) sample) sym :render-fn)
          (when-let [ef (:ext.symbol/render-error-fn sym-entry)]
            (assert-render-fn-result! (ef sample) sym :render-error-fn))))))
  ext)
(defn validate!
  "Normalize and assert that an extension map conforms to ::extension.
   Normalizes `:ext/prompt` (string -> fn) before checking the spec
   when the key is present. Throws with spec explain-data on violation."
  [ext]
  (when (contains? ext :ext/environment-prompt-fn)
    (throw
      (ex-info
        ":ext/environment-prompt-fn was removed; put model-facing environment text in :ext/prompt"
        {:type :extension/retired-environment-prompt-fn, :name (:ext/name ext)})))
  (let [ext (cond-> ext (contains? ext :ext/prompt) (update :ext/prompt normalize-prompt))]
    (when-not (s/valid? ::extension ext)
      (throw (ex-info (str "Invalid extension '" (:ext/name ext)
                        "':\n" (with-out-str (s/explain ::extension ext)))
               {:type :extension/invalid-spec,
                :name (:ext/name ext),
                :explain (s/explain-data ::extension ext)})))
    (-> ext
      validate-symbol-op-tags!
      validate-symbol-renderers!)))
;; =============================================================================
;; Hook execution - runtime wrappers with output validation + logging
;; =============================================================================
(defn- validate-hook-return!
  [hook-name sym returned]
  (when-not (map? returned)
    (throw (ex-info (str hook-name " for '" sym "' must return a map, got: " (type returned))
             {:type (keyword "extension" (str hook-name "-error")),
              :symbol sym,
              :returned returned}))))
(defn- call-hook
  [hook-name sym hook-fn hook-args]
  (try (apply hook-fn hook-args)
    (catch clojure.lang.ArityException e
      (throw (ex-info (str hook-name " for '" sym "' has wrong arity: " (ex-message e))
               {:type (keyword "extension" (str hook-name "-error")), :symbol sym}
               e)))
    (catch Throwable e
      (throw (ex-info (str hook-name " for '" sym "' threw: " (ex-message e))
               {:type (keyword "extension" (str hook-name "-error")), :symbol sym}
               e)))))
(defn- elapsed-ms [t0] (/ (- (System/nanoTime) t0) 1e6))
(defn- log-hook!
  [level id ext-ns sym phase ms extra-msg]
  (tel/log! {:level level,
             :id id,
             :data {:ext ext-ns, :symbol sym, :phase phase, :ms ms},
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
          {:env (get ret :env env), :fn (get ret :fn f), :args (vec (get ret :args args))})))
    {:env env, :fn f, :args args}))
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
      {:env (get ret :env env),
       :fn (get ret :fn f),
       :args (vec (get ret :args args)),
       :result (get ret :result result)})
    {:env env, :fn f, :args args, :result result}))
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
                             {:type :extension/on-error-fn-error, :symbol sym}
                             e)))))
          _ (validate-hook-return! ":on-error-fn" sym ret)
          ms (elapsed-ms t0)]
      (cond (contains? ret :result)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "fallback result")
          ret)
        (contains? ret :error)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "surfacing error")
          ret)
        :else (do (log-hook! :info ::on-error-fn-done ext-ns sym :on-error-fn ms "retrying")
                ret)))
    (throw err)))
(defn- assert-symbol-envelope!
  [sym result]
  (when-not (tool-result? result)
    (throw (ex-info (str "Symbol '" sym "' must return a canonical :envelope map")
             {:type :extension/invalid-symbol-result,
              :symbol sym,
              :spec :envelope,
              :value result,
              :explain (s/explain-data ::envelope result)})))
  result)
(defn- tool-call-name
  [ext sym]
  (if-let [alias (ext-alias-symbol ext)]
    (str alias "/" sym)
    (str sym)))
(defn- tool-start-event
  [ext sym-entry started-at-ms]
  (let [sym (:ext.symbol/symbol sym-entry)]
    {:phase :tool-start,
     :status :running,
     :op (keyword (tool-call-name ext sym)),
     :extension (:ext/name ext),
     :symbol sym,
     :started-at-ms (long started-at-ms)}))
(defn- extension-info-now
  [ext]
  ((requiring-resolve 'com.blockether.vis.internal.extension/extension-info) ext))
(defn- default-tool-op-keyword
  [ext sym-entry]
  (keyword (tool-call-name ext (:ext.symbol/symbol sym-entry))))
(defn- ensure-tool-result-op
  "Observed extension tools must carry canonical op metadata. The wrapper
   derives it deterministically from active alias + symbol (`cat`,
   `git/fetch!`, ...). The tag comes from the symbol entry's inline
   `:ext.symbol/tag` (source of truth); a derived op->tag index covers
   call-sites without a sym-entry handle. Missing tag in BOTH places throws
   via `op-tag` so unregistered ops still fail closed."
  [ext sym-entry result]
  (if (and (tool-result? result) (nil? (:symbol result)))
    (let [op (default-tool-op-keyword ext sym-entry)
          tag (or (:ext.symbol/tag sym-entry) (op-tag op))]
      (assoc result
        :symbol op
        :tag tag))
    result))
(defn- public-op-keyword
  "User-facing op keyword for payload EDN. Tool symbols use `!` for mutation
   (`git/fetch!`), but result maps read like porcelain (`:git/fetch`)."
  [op]
  (when op
    (let [ns-part (namespace op)
          n (name op)
          n (str/replace n #"!$" "")]
      (if ns-part (keyword ns-part n) (keyword n)))))
(defn- stamp-public-result-op
  "Public Python value is the envelope's `:result`, not the envelope. If the
   payload is a map, stamp the canonical tool op so extension implementations
   do not hand-maintain it. Tool-specific operation details must use a
   different key (`:edit-op`, `:action`, etc.)."
  [result]
  (if (and (tool-result? result) (:success? result) (:symbol result) (map? (:result result)))
    (update result :result assoc :op (public-op-keyword (:symbol result)))
    result))
(defn- enrich-tool-result-info
  [ext sym-entry result]
  (if (tool-result? result)
    (let [ext-prov (extension-info-now ext)]
      (merge-into-metadata
        (stamp-public-result-op (ensure-tool-result-op ext sym-entry result))
        {:tool (cond-> {:symbol (:ext.symbol/symbol sym-entry),
                        :call (tool-call-name ext (:ext.symbol/symbol sym-entry))}
                 (ext-alias-symbol ext) (assoc :alias (ext-alias-symbol ext))),
         :extension (dissoc ext-prov :source-paths :source-mtime-max :source-hash-sha256),
         :source {:paths (:source-paths ext-prov),
                  :mtime-max (:source-mtime-max ext-prov),
                  :hash-sha256 (:source-hash-sha256 ext-prov)}}))
    result))
(defn- sink-form-string
  "Reconstruct the call form for `:form` in sink entries: `(alias/sym args...)`
   pr-str'd. Args are the EVALUATED args (Python passes evaluated values into
   the wrapper); the form reflects the actual call made, not the lexical
   source. Returns a non-blank string suitable for the spec."
  [ext sym-entry args]
  (let [alias-sym (ext-alias-symbol ext)
        sym-name (:ext.symbol/symbol sym-entry)
        head (if alias-sym (clojure.core/symbol (str alias-sym) (str sym-name)) sym-name)]
    (pr-str (cons head (vec args)))))
(defn- missing-tool-renderer-ir
  [tool-result]
  (render/->ast [:ir {}
                 [:p {} [:span {} "Tool result for "]
                  [:c {} (str (or (:symbol tool-result) "unknown tool"))]
                  [:span {} " has no registered channel renderer; payload omitted."]]]))
(defn- render-value
  "Run symbol-owned render fn for sink recording. Returns the validated
   `{:summary :display}` contract value."
  [sym-entry value]
  (assert-render-fn-result! ((:ext.symbol/render-fn sym-entry) value)
    (:ext.symbol/symbol sym-entry)
    :render-fn))
(defn- write-sink-entries!
  "After a tool symbol's `invoke-symbol-wrapper` produces a final
   tool-result, write ONE entry to `*render-sink*` so runtime channels
   (TUI, Telegram, …) can paint each call.

   No-op when:
     - `result` is not a tool-result (defensive; fn-symbols always
       return one, but ad-hoc consumers might bypass).
     - The render sink is unbound (no observer; skip all rendering work).

   Stamps the originating `sym-entry`'s `:ext.symbol/symbol` and
   `:ext.symbol/tag` onto the sink entry. The rebuild path (restored
   sessions) derives the TUI's tool-badge label (`OBSERVATION ls`,
   `MUTATION patch`, …) from the channel slice rather than from the
   bound assignment result — a Python assignment unwraps the envelope to
   its inner `:result` value before binding, so `tool-result?` on the
   restored block-level `:result` is false and `form-result-detail`
   returns nil. Without these keys restored tool bubbles paint only
   the bold inline badge text from the IR (`**LS**`, `**PORTS**`)
   and lose the colored label / chrome row — user-visible regression
   on EVERY session restore that touched an `x = tool(…)` form."
  [ext sym-entry args result]
  (when (and (tool-result? result) *render-sink*)
    (let [position (next-sink-position!)
          form-str (sink-form-string ext sym-entry args)
          sym-id (:ext.symbol/symbol sym-entry)
          sym-tag (:ext.symbol/tag sym-entry)
          op (keyword (tool-call-name ext sym-id))
          metadata (:metadata result)
          base (cond-> {:position position, :form form-str}
                 (some? sym-id) (assoc :symbol sym-id)
                 (some? sym-tag) (assoc :tag sym-tag)
                 (some? op) (assoc :op op)
                 (some? (:started-at-ms metadata)) (assoc :started-at-ms (:started-at-ms metadata))
                 (some? (:finished-at-ms metadata)) (assoc :finished-at-ms
                                                      (:finished-at-ms metadata)))]
      (if (:success? result)
        (let [unwrapped (:result result)
              rendered (render-value sym-entry unwrapped)]
          (record-render-entry! (assoc base
                                  :success? true
                                  :result rendered
                                  :error nil)))
        (record-render-entry! (assoc base
                                :success? false
                                :result nil
                                :error (:error result))))))
  result)
(def ^:dynamic *current-extension*
  "Extension map currently executing on an extension callback thread.
   Bound by symbol wrappers so extension-owned helper APIs can fill the
   caller's stable extension identity without accepting user-supplied ids."
  nil)
(def ^:dynamic *current-symbol*
  "Sandbox symbol currently executing, when a symbol callback is active."
  nil)
(defn current-extension [] *current-extension*)
(defn current-extension-id
  []
  (some-> *current-extension*
    :ext/name))
(defn- call-extension-env-fn
  [ext f environment]
  (binding [*current-extension* ext
            *current-symbol* nil
            workspace/*workspace-root* (workspace/workspace-root environment)]
    (f environment)))
(defn- active-extension?
  [environment ext]
  (try (boolean
         (call-extension-env-fn ext (or (:ext/activation-fn ext) (constantly true)) environment))
    (catch Throwable t
      (tel/log! {:level :error,
                 :id ::ext-activation-error,
                 :data {:ext (:ext/name ext), :error (ex-message t)}}
        (str "Extension '" (:ext/name ext) "' activation-fn threw"))
      false)))
(defn- active-extension-rows
  [environment]
  (let [active-value (:active-extensions environment)]
    (cond (some? active-value)
      (vec (if (instance? clojure.lang.IDeref active-value) @active-value active-value))
      :else (filterv #(active-extension? environment %)
              (vec (or (some-> (:extensions environment)
                         deref)
                     []))))))
(defn active-protected-globs
  "Aggregate protected path rules from active extensions in extension order.

   Each active extension may declare `:ext/protected-paths` as
   `(fn [env] -> [{:glob string :access :read-only|:read-write|:none
                   :hint string} ...])`. Rule order is preserved within
   each extension; the foundation editing core resolves first-match-wins
   per extension and most-restrictive-wins globally. Returned rows are
   enriched with `:extension/name` for diagnostics."
  [environment]
  (mapv identity
    (mapcat (fn [ext]
              (when-let [f (:ext/protected-paths ext)]
                (let [rows (call-extension-env-fn ext f environment)]
                  (when-not (s/valid? ::protected-paths-result rows)
                    (throw (ex-info ":ext/protected-paths returned invalid protected path rules"
                             {:type :extension/invalid-protected-paths,
                              :extension (:ext/name ext),
                              :value rows,
                              :explain (s/explain-data ::protected-paths-result rows)})))
                  (mapv #(assoc % :extension/name (:ext/name ext)) rows))))
      (active-extension-rows environment))))
(defn- deep-merge
  [& maps]
  (letfn [(merge-entry [a b] (if (and (map? a) (map? b)) (merge-with merge-entry a b) b))]
    (apply merge-with merge-entry maps)))
(defn ctx-contributions
  "Return merged structured `ctx` contributions for active extensions.

   Each active extension may declare `:ext/ctx` as `(fn [env] -> map)`.
   Exceptions and non-map returns are logged and ignored so bad optional
   context never blocks a turn."
  [environment active-extensions]
  (reduce (fn [acc ext]
            (if-let [f (:ext/ctx ext)]
              (let [contribution (try (binding [*current-extension* ext
                                                *current-symbol* nil
                                                workspace/*workspace-root* (workspace/workspace-root
                                                                             environment)]
                                        (f environment))
                                   (catch Throwable t
                                     (tel/log! {:level :warn,
                                                :id ::ctx-contribution-error,
                                                :data {:ext (:ext/name ext),
                                                       :error (ex-message t)}}
                                       "Extension :ext/ctx fn threw")
                                     nil))]
                (if (map? contribution)
                  (deep-merge acc contribution)
                  (do (when (some? contribution)
                        (tel/log! {:level :warn,
                                   :id ::ctx-contribution-invalid,
                                   :data {:ext (:ext/name ext), :returned (type contribution)}}
                          "Extension :ext/ctx fn returned non-map"))
                    acc)))
              acc))
    {}
    (or active-extensions [])))
(defn- tool-result->public-value
  [result]
  (if (:success? result)
    (:result result)
    (let [err (:error result)]
      (throw
        (ex-info
          (or (:message err) "Tool failed")
          {:type :vis/tool-failure, :symbol (:symbol result), :error err, :tool-result result})))))
(defn invoke-symbol-wrapper
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
  (binding [*current-extension* ext
            *current-symbol* (:ext.symbol/symbol sym-entry)]
    (let [sym (:ext.symbol/symbol sym-entry)
          ext-ns (:ext/name ext)
          original-args args
          t0 (System/nanoTime)
          _ (log-hook! :debug ::invoke ext-ns sym nil nil nil)
          before-out (run-before ext-ns sym-entry env (:ext.symbol/fn sym-entry) args)]
      (if (contains? before-out :result)
        (let [ms (elapsed-ms t0)
              result (->> (:result before-out)
                       (enrich-tool-result-info ext sym-entry)
                       (assert-symbol-envelope! sym))]
          (write-sink-entries! ext sym-entry original-args result)
          (log-hook! :debug ::invoke-done ext-ns sym nil ms "short-circuited")
          (tool-result->public-value result))
        (let [{call-env :env, f :fn, call-args :args} before-out
              call-result
              (let [ct0 (System/nanoTime)
                    call-started-at-ms (now-ms)]
                (record-tool-event! (tool-start-event ext sym-entry call-started-at-ms))
                (try (let [r (apply f call-args)
                           ms (elapsed-ms ct0)]
                       (log-hook! :debug ::fn-returned ext-ns sym :call ms nil)
                       {:result r})
                  (catch Throwable e
                    (let [ms (elapsed-ms ct0)]
                      (log-hook! :warn ::fn-threw ext-ns sym :call ms (ex-message e))
                      (try (let [recovery
                                 (run-on-error ext-ns sym-entry e call-env f call-args)]
                             (cond (contains? recovery :result) recovery
                               (contains? recovery :error) (throw (:error recovery))
                               :else {:result (apply (get recovery :fn f)
                                                (vec (get recovery :args call-args)))}))
                        (catch Throwable e2
                                  ;; Unrecoverable: no on-error-fn or it surfaced the
                                  ;; error. Write a failure sink entry derived from
                                  ;; the original throwable BEFORE the throw escapes,
                                  ;; so consumers see exactly which call broke even
                                  ;; when the form bubbles up the exception.
                          (write-sink-entries! ext
                            sym-entry
                            original-args
                            (failure {:result nil,
                                      :op (keyword (tool-call-name ext
                                                     sym)),
                                      :throwable e2}))
                          (throw e2)))))))
              {:keys [result]}
              (run-after ext-ns sym-entry call-env f call-args (:result call-result))
              result (->> result
                       (enrich-tool-result-info ext sym-entry)
                       (assert-symbol-envelope! sym))
              ms (elapsed-ms t0)]
          (write-sink-entries! ext sym-entry original-args result)
          ;; If the symbol's envelope declared `:emit/{tasks,facts}`,
          ;; route each entry through `ctx-loop/apply-and-record!` so
          ;; extensions can mutate CTX from inside an op without
          ;; reaching for an explicit `(task-set! ...)` form. Pure
          ;; declarative: the tool returns data; the engine writes.
          (when-let [emit (:emit result)]
            (when-let [apply-and-record! (requiring-resolve
                                           'com.blockether.vis.internal.ctx-loop/apply-and-record!)]
              (doseq [[k partial] (:tasks emit)]
                (apply-and-record! call-env :task-set! [k partial]))
              (doseq [[k partial] (:facts emit)]
                (apply-and-record! call-env :fact-set! [k partial]))))
          (log-hook! :debug ::invoke-done ext-ns sym nil ms nil)
          (tool-result->public-value result))))))
(def ^:private ^:dynamic *log-writer*
  "Writer that sends output to the log file instead of stdout/stderr.
   Bound during extension invocations so tool fns never bleed into the TUI."
  nil)
(defn- get-log-writer
  []
  (or *log-writer*
    (let [log-path (str (System/getProperty "user.home") "/.vis/vis.log")]
      (alter-var-root #'*log-writer* (fn [cur] (or cur (io/writer log-path :append true))))
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
                   (let [env (env-thunk)]
                     (binding [workspace/*workspace-root* (workspace/workspace-root env)]
                       (apply (:ext.symbol/fn sym-entry) args))))
                 (fn [& args]
                   (let [env (env-thunk)
                         w (get-log-writer)]
                     (binding [*out* w
                               *err* w
                               workspace/*workspace-root* (workspace/workspace-root env)]
                       (invoke-symbol-wrapper ext sym-entry (vec args) env)))))]
              [sym (:ext.symbol/val sym-entry)]))))
      entries)))
;; =============================================================================
;; Public API - extension builder
;; =============================================================================
(defn- derive-kind
  "Auto-derive `:ext/kind` for the categorical cases when the author
   didn't set one. Extensions that contribute providers, channels,
   channel contributions, or persistence backends (and nothing forcing a different
   label) get bucketed under `\"providers\"` / `\"channels\"` /
   `\"persistance\"` so `vis extensions list` reads as a clean grouped
   table instead of a column of blanks.

   Explicit `:ext/kind` always wins. Extensions that fit no
   categorical bucket (and don't set a kind themselves) stay
   blank - that's a legitimate \"uncategorized\" outcome."
  [spec]
  (cond (some? (:ext/kind spec)) (:ext/kind spec)
    (seq (:ext/providers spec)) "providers"
    (seq (:ext/channels spec)) "channels"
    (seq (:ext/channel-contributions spec)) "channels"
    (seq (:ext/persistance spec)) "persistance"
    :else nil))
(defn extension
  "Build and validate an extension. The canonical constructor.

   See docs/src/extensions/extension-spec.md for the full key list."
  [spec]
  (-> spec
    (cond-> (contains? spec :ext/prompt) (update :ext/prompt normalize-prompt))
    (cond->
      (not (:ext/activation-fn spec)) (assoc :ext/activation-fn (constantly true))
      (some? (derive-kind spec)) (assoc :ext/kind (derive-kind spec))
      (not (:ext/engine spec)) (assoc :ext/engine {})
      (and (get-in spec [:ext/engine :ext.engine/alias]) (nil? (get-in spec [:ext/engine :ext.engine/ns])))
      (assoc-in [:ext/engine :ext.engine/ns]
        (clojure.core/symbol (str "vis.ext." (name (get-in spec [:ext/engine :ext.engine/alias])))))
      (nil? (get-in spec [:ext/engine :ext.engine/symbols])) (assoc-in [:ext/engine :ext.engine/symbols] [])
      (nil? (get-in spec [:ext/engine :ext.engine/classes])) (assoc-in [:ext/engine :ext.engine/classes] {})
      (nil? (get-in spec [:ext/engine :ext.engine/imports])) (assoc-in [:ext/engine :ext.engine/imports] {})
      (not (:ext/env spec)) (assoc :ext/env [])
      (not (:ext/settings spec)) (assoc :ext/settings [])
      (not (:ext/theme spec)) (assoc :ext/theme {})
      (not (:ext/requires spec)) (assoc :ext/requires [])
      (not (:ext/cli spec)) (assoc :ext/cli [])
      (not (:ext/channels spec)) (assoc :ext/channels [])
      (not (:ext/providers spec)) (assoc :ext/providers [])
      (not (:ext/persistance spec)) (assoc :ext/persistance [])
      (not (:ext/channel-contributions spec)) (assoc :ext/channel-contributions {})
      (not (:ext/slash-commands spec)) (assoc :ext/slash-commands [])
      (not (:ext/doctor-fn spec)) (assoc :ext/doctor-fn (constantly [])))
    (validate!)))
;; =============================================================================
;; Extension source markers
;; =============================================================================
;; ---------------------------------------------------------------------------
;; Hash + mtime primitives.
;; ---------------------------------------------------------------------------
(defn- sha256-digest ^MessageDigest [] (MessageDigest/getInstance "SHA-256"))
(defn- bytes->hex
  ^String [^bytes b]
  (let [sb (StringBuilder. (* 2 (alength b)))]
    (dotimes [i (alength b)]
      (let [v (bit-and (aget b i) 0xff)]
        (when (< v 16) (.append sb \0))
        (.append sb (Integer/toString v 16))))
    (.toString sb)))
(defn- read-stream-bytes
  ^bytes [^InputStream in]
  (with-open [out (ByteArrayOutputStream.)]
    (let [buf (byte-array 8192)]
      (loop [] (let [n (.read in buf)] (when (pos? n) (.write out buf 0 n) (recur)))))
    (.toByteArray out)))
;; ---------------------------------------------------------------------------
;; Resolve one namespace -> entry map.
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
  (let [paths (ns->resource-path ns-sym)] (some (fn [p] (.getResource cl ^String p)) paths)))
(defrecord ^:private SourceEntry [^String locator ^long mtime ^bytes content])
(defn- file-entry
  "Build a SourceEntry for a `file:` URL. Reads the file content for
   hashing; mtime from `.lastModified`."
  ^SourceEntry [^URL url]
  (let [f (java.io.File. (.toURI url))
        path (.getAbsolutePath f)
        mtime (.lastModified f)
        content (try (read-stream-bytes (java.io.FileInputStream. f))
                  (catch Throwable t
                    (tel/log! {:level :warn,
                               :id ::file-read-failed,
                               :data {:path path, :error (ex-message t)}})
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
  (let [conn (.openConnection url)
        ;; The cast is paranoid - `.getJarFileURL` lives on `JarURLConnection`,
        ;; we know URL was a jar: URL when we got here.
        jconn ^java.net.JarURLConnection conn
        jar-url (.getJarFileURL jconn)
        jar-file (java.io.File. (.toURI jar-url))
        jar-path (.getAbsolutePath jar-file)
        entry-nm (.getEntryName jconn)]
    (with-open [jar (JarFile. jar-file)]
      (let [^JarEntry e (.getJarEntry jar entry-nm)]
        (if (nil? e)
          (do (tel/log!
                {:level :warn, :id ::jar-entry-missing, :data {:jar jar-path, :entry entry-nm}})
            nil)
          (let [mtime (.getTime e)
                content (try (with-open [in (.getInputStream jar e)] (read-stream-bytes in))
                          (catch Throwable t
                            (tel/log!
                              {:level :warn,
                               :id ::jar-entry-read-failed,
                               :data {:jar jar-path, :entry entry-nm, :error (ex-message t)}})
                            (byte-array 0)))]
            (->SourceEntry (jar-entry-locator jar-path entry-nm) mtime content)))))))
(defn- url->entry
  "Dispatch on URL protocol to the right reader. Returns SourceEntry
   or nil on unrecognized protocol."
  [^URL url]
  (try (case (some-> url
               .getProtocol
               str/lower-case)
         "file" (file-entry url)
         "jar" (jar-entry url)
         (do (tel/log! {:level :warn,
                        :id ::unsupported-protocol,
                        :data {:protocol (.getProtocol url), :url (str url)}})
           nil))
    (catch Throwable t
      (tel/log!
        {:level :warn, :id ::resolve-failed, :data {:url (str url), :error (ex-message t)}})
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
  (let [cl (.getContextClassLoader (Thread/currentThread))
        urls (->> ns-syms
               (map #(find-source-resource cl %))
               (remove nil?))
        entries (->> urls
                  (map url->entry)
                  (remove nil?)
                  (sort-by :locator)
                  vec)]
    (if (empty? entries)
      {:source-paths [], :source-mtime-max -1, :source-hash-sha256 nil}
      (let [paths (mapv :locator entries)
            mtime-max (long (reduce max 0 (map :mtime entries)))
            digest (sha256-digest)
            _ (doseq [^SourceEntry e entries]
                (let [^bytes c (:content e)] (.update digest c 0 (alength c))))
            hash-bytes (.digest digest)
            hash-hex (bytes->hex hash-bytes)]
        {:source-paths paths, :source-mtime-max mtime-max, :source-hash-sha256 hash-hex}))))
(defn resolve-markers-for-extension
  "Resolve source markers from manifest `:nses` or extension `:ext/source-nses`."
  [ext-or-manifest]
  (let [ns-syms (or (some-> (:nses ext-or-manifest)
                      seq
                      vec)
                  (some-> (:ext/source-nses ext-or-manifest)
                    seq
                    vec))]
    (resolve-markers (or ns-syms []))))
;; =============================================================================
;; Global Extension Registry
;; =============================================================================
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
  (doseq [provider-entry providers] (registry/register-provider! provider-entry)))
(defn- dispatch-persistance!
  [entries]
  (doseq [{:persistance/keys [id ns]} entries] (persistance/register-backend! id ns)))
(def ^:private EXT_PARENT ["ext"])
(defn- mount-under-ext
  "Auto-place an `:ext/cli` entry under the `vis ext` parent.

   Authors who want nested placement (e.g. `vis ext git status`)
   can pass `:cmd/parent [\"ext\" \"git\"]` and the dispatcher
   respects it AS LONG AS the first element is `\"ext\"`. Any
   other parent is rejected."
  [{:cmd/keys [parent name], :as entry}]
  (cond (or (nil? parent) (= [] parent)) (assoc entry :cmd/parent EXT_PARENT)
    (= "ext" (first parent)) entry
    :else (throw (ex-info (str ":ext/cli entry '"
                            name
                            "' has :cmd/parent "
                            (pr-str parent)
                            " -- extension-owned CLI mounts only under [\"ext\" ...].")
                   {:type :ext/cli-bad-parent, :entry entry}))))
(defn register-extension!
  "Register an extension in the global process-level registry.

   This is THE single entry point for everything an extension
   contributes to vis. Whatever the extension declares -- Python sandbox
   symbols (`:ext.engine/symbols`), CLI commands (`:ext/cli`), channels
   (`:ext/channels`), LLM providers (`:ext/providers`), persistence
   backends (`:ext/persistance`) -- gets routed here and dispatched into
   the matching sub-registry as a side effect.

   Also computes source-file markers (paths, max-mtime, sha256) and
   stores them in a sidecar atom read by the tool-envelope emitter
   (UI extension provenance label).

   Idempotent on `:ext/name`. Returns the validated extension."
  [ext]
  (let [ext (extension ext)
        ns-sym (:ext/name ext)]
    ;; Slash paths must be unique across the union of `:ext/slash-commands` from every active
    ;; extension. Reject registration when this extension declares a `[parent name]`
    ;; that any OTHER currently-registered extension already owns
    ;; AND whose `:slash/availability-fn` intersects on the known
    ;; channel set. Two specs with the same path but DISJOINT
    ;; channel availability (e.g. TUI `/voice` vs Telegram `/voice`)
    ;; do not collide — the dispatcher resolves them via per-channel availability at runtime.
    (let [known-channels [:tui :telegram :web :discord :cli :repl :slack]
          slash-channels (fn [spec]
                           (if-let [f (:slash/availability-fn spec)]
                             (set (filter (fn [ch]
                                            (try (boolean (f {:channel/id ch}))
                                              (catch Throwable _ false)))
                                    known-channels))
                             (set known-channels)))
          new-by-path
          (reduce (fn [m spec] (assoc m (slash-path spec) spec)) {} (:ext/slash-commands ext))]
      (when (seq new-by-path)
        (let [collisions (for [[other-ns other-ext] @extension-registry
                               :when (not= other-ns ns-sym)
                               other-slash (:ext/slash-commands other-ext)
                               :let [p (slash-path other-slash)
                                     new-spec (get new-by-path p)]
                               :when (and new-spec
                                       (seq (set/intersection (slash-channels new-spec)
                                              (slash-channels other-slash))))]
                           {:path p, :other-ext other-ns})]
          (when (seq collisions)
            (throw (ex-info (str "Slash path collision while registering '" ns-sym
                              "': " (str/join
                                      ", "
                                      (for [{:keys [path other-ext]} collisions]
                                        (str (pr-str path) " already owned by " other-ext))))
                     {:type :extension/slash-path-collision,
                      :ext ns-sym,
                      :collisions (vec collisions)}))))))
    (when-not (contains? @extension-registry ns-sym) (swap! extension-order conj ns-sym))
    (swap! extension-registry assoc ns-sym ext)
    (tel/log! {:level :info,
               :id ::register-global,
               :data {:ext ns-sym,
                      :symbols (count (ext-symbols ext)),
                      :cli (count (:ext/cli ext)),
                      :channels (count (:ext/channels ext)),
                      :providers (count (:ext/providers ext)),
                      :persistance (count (:ext/persistance ext)),
                      :themes (count (:ext/theme ext))},
               :msg (str "Extension '" ns-sym "' registered globally")})
    (doseq [c (:ext/cli ext)] (registry/register-cmd! (mount-under-ext c)))
    (doseq [c (:ext/channels ext)] (registry/register-channel! c))
    (dispatch-providers! (:ext/providers ext))
    (dispatch-persistance! (:ext/persistance ext))
    (theme/register-themes! (:ext/theme ext))
    ;; Index every symbol's inline `:ext.symbol/tag` into the
    ;; global op-keyword -> tag map. The sym-entry remains the source
    ;; of truth; this index is a cheap lookup for sites (e.g.
    ;; `envelope-of`) that have an op keyword but no sym-entry handle.
    (doseq [sym-entry (ext-symbols ext)
            :let [tag (:ext.symbol/tag sym-entry)]
            :when tag]
      (let [op-kw (keyword (tool-call-name ext (:ext.symbol/symbol sym-entry)))]
        (swap! op-keyword->tag assoc op-kw tag)))
    ;; Index every symbol's optional inline `:ext.symbol/batch-hint`
    ;; high-fan-out threshold the same way (Phase 4). Advisory only.
    (doseq [sym-entry (ext-symbols ext)
            :let [hint (:ext.symbol/batch-hint sym-entry)]
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
        (tel/log! {:level :warn,
                   :id ::source-markers-failed,
                   :data {:ext ns-sym, :error (ex-message t)}})))
    ext))
;; Manifest id -> namespaces registry. Defined here so `extension-info`
;; can reverse-lookup an extension id from a namespace without a forward
;; declare.
(defonce ^:private extension-manifest-registry (atom {}))
(defn extension-id-of-ns
  "Reverse lookup: given a namespace symbol, return the extension id
   that registered it under `:nses`, or `nil`."
  [ns-sym]
  (some (fn [[id {nses :nses}]] (when (some #(= ns-sym %) nses) id)) @extension-manifest-registry))
(def ^:private empty-source-markers
  {:source-paths [], :source-mtime-max -1, :source-hash-sha256 nil})
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
        (tel/log! {:level :warn,
                   :id ::source-markers-on-demand-failed,
                   :data {:ext (:ext/name ext), :error (ex-message t)}})
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
  (let [name (:ext/name ext)
        alias (ext-alias-symbol ext)
        registry-id (or (some (fn [ns-sym]
                                (try (extension-id-of-ns ns-sym) (catch Throwable _ nil)))
                          (ext-source-nses ext))
                      alias)
        markers (source-markers-for-extension ext)
        prov (cond-> {:name name,
                      :source-paths (:source-paths markers),
                      :source-mtime-max (:source-mtime-max markers),
                      :source-hash-sha256 (:source-hash-sha256 markers)}
               alias (assoc :alias alias)
               (:ext/description ext) (assoc :description (:ext/description ext))
               (:ext/kind ext) (assoc :kind (:ext/kind ext))
               (:ext/version ext) (assoc :version (:ext/version ext))
               (:ext/author ext) (assoc :author (:ext/author ext))
               (:ext/owner ext) (assoc :owner (:ext/owner ext))
               (:ext/license ext) (assoc :license (:ext/license ext))
               registry-id (assoc :registry-id registry-id))]
    (when-not (s/valid? ::extension-info prov)
      (throw (ex-info "Invalid extension info"
               {:type :extension/invalid-info,
                :name name,
                :value prov,
                :explain (s/explain-data ::extension-info prov)})))
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
            (tel/log! {:level :warn,
                       :id ::deregister-cmd-failed,
                       :data {:ext ns-sym, :cmd (:cmd/name mounted), :error (ex-message t)}})))))
    (doseq [c (:ext/channels ext)]
      (try (registry/deregister-channel! (:channel/id c))
        (catch Throwable t
          (tel/log! {:level :warn,
                     :id ::deregister-channel-failed,
                     :data {:ext ns-sym, :channel-id (:channel/id c), :error (ex-message t)}}))))
    (doseq [p (:ext/providers ext)]
      (try (registry/deregister-provider! (:provider/id p))
        (catch Throwable t
          (tel/log! {:level :warn,
                     :id ::deregister-provider-failed,
                     :data
                     {:ext ns-sym, :provider-id (:provider/id p), :error (ex-message t)}}))))
    (doseq [{:persistance/keys [id]} (:ext/persistance ext)]
      (try (persistance/deregister-backend! id)
        (catch Throwable t
          (tel/log! {:level :warn,
                     :id ::deregister-backend-failed,
                     :data {:ext ns-sym, :backend-id id, :error (ex-message t)}}))))
    (theme/unregister-themes! (keys (:ext/theme ext)))
    (tel/log! {:level :info,
               :id ::deregister-global,
               :data {:ext ns-sym},
               :msg (str "Extension '" ns-sym "' deregistered globally")}))
  (swap! extension-registry dissoc ns-sym)
  (swap! extension-order (fn [order] (vec (remove #{ns-sym} order))))
  (swap! extension-source-markers dissoc ns-sym)
  nil)
(defn registered-extensions
  []
  (let [registry @extension-registry] (into [] (keep registry) @extension-order)))
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
     (vec (cond->> rows slot (filter #(= slot (:slot %))))))))
(defn tool-result-symbol-entry
  [tool-result]
  ;; `:tool` and `:extension` blobs live under `:metadata` on the flat
  ;; envelope (they were inside `:info` on the original shape).
  (let [ext-name (get-in tool-result [:metadata :extension :name])
        sym (get-in tool-result [:metadata :tool :symbol])]
    (when (and ext-name sym)
      (some (fn [entry] (when (= sym (:ext.symbol/symbol entry)) entry))
        (ext-symbols (get @extension-registry ext-name))))))
(defn- format-error-fields
  "Pull the `:message` (and an inferred `:type` from the trace's first
   line) out of an `:error` map for the engine's default error
   formatters. Defensive: never throws inside a renderer.

   The structured error has `:message :trace :hint :block`. The
   `:type` historical field is no longer carried; the underlying
   exception class name appears as the prefix of the preformatted
   `:trace` first line (e.g. `clojure.lang.ArityException: ...`)."
  [error]
  (let [type-from-trace (fn [trace]
                          (when-let [first-line (some-> trace
                                                  (str/split #"\n" 2)
                                                  first)]
                            (when (str/includes? first-line ": ")
                              (first (str/split first-line #": " 2)))))]
    (cond (map? error) {:type (or (type-from-trace (:trace error)) "error"),
                        :message (or (:message error) "")}
      (instance? Throwable error) {:type (.getName (class error)),
                                   :message (or (.getMessage ^Throwable error) "")}
      :else {:type "unknown", :message (str error)})))
(defn default-error-result
  "Engine fallback used by `render-tool-result` when a symbol does
   not declare `:ext.symbol/render-error-fn`. Returns the canonical
   `{:summary :display}` render contract.

   `:summary` is a zone map `{:left **ERROR** :right <type>}` so channels
   paint a labelled badge with the error class anchored right. `:display`
   is the full error IR: the headline plus rendered source context when
   `:error :block` is present."
  [tool-result]
  (let [op (:symbol tool-result)
        error (:error tool-result)
        {:keys [type message]} (format-error-fields error)
        context-ir (when (:block error) (render-error-context (:block error)))
        display-ir (render/->ast (into [:ir {}
                                        [:p {} [:strong {} [:span {} "ERROR"]]
                                         (when op [:span {} (str " " op)])
                                         [:span {} (str " — " type)]
                                         (when (seq message) [:span {} (str ": " message)])]]
                                   (when context-ir (drop 2 context-ir))))]
    {:summary (cond-> {:left (ir-strong "ERROR")} (seq type) (assoc :right type)),
     :display display-ir}))
(defn default-error-ir
  "Deprecated shim retained for callers that still expect a flat IR for the
   default error. Returns `(:display (default-error-result …))`. New code
   should consume `default-error-result` and read `:summary` / `:display`."
  [tool-result]
  (:display (default-error-result tool-result)))
(defn render-tool-result
  "Render a tool-result for runtime channels (TUI, telegram, ...) as the
   canonical `{:summary :display}` contract. Successful results use the
   registered `:render-fn`; failures use the symbol's `:render-error-fn`
   (validated against the same contract) or `default-error-result`.
   Missing renderer means payload omitted, never pr-str dumped."
  [tool-result]
  (if-not (:success? tool-result)
    (if-let [sym-entry (tool-result-symbol-entry tool-result)]
      (if-let [f (:ext.symbol/render-error-fn sym-entry)]
        (assert-render-fn-result! (f tool-result) (:ext.symbol/symbol sym-entry) :render-error-fn)
        (default-error-result tool-result))
      (default-error-result tool-result))
    (if-let [sym-entry (tool-result-symbol-entry tool-result)]
      (render-value sym-entry (:result tool-result))
      {:summary (ir-root (ir-p (ir-strong "TOOL"))),
       :display (missing-tool-renderer-ir tool-result)})))
(defn- topo-sort-extensions
  "Topologically sort extensions by :ext/requires.
   Throws on missing dependencies or cycles."
  [extensions]
  (let [by-ns (into {} (map (juxt :ext/name identity)) extensions)
        visited (volatile! #{})
        path (volatile! #{})
        result (volatile! [])]
    (letfn
      [(visit [ns-sym]
         (when (contains? @path ns-sym)
           (throw (ex-info (str "Circular extension dependency: " ns-sym " -> ... -> " ns-sym)
                    {:type :extension/circular-dependency, :extension ns-sym, :path @path})))
         (when-not (contains? @visited ns-sym)
           (vswap! path conj ns-sym)
           (let [ext (get by-ns ns-sym)]
             (when-not ext
               (throw (ex-info (str "Extension '" ns-sym "' required but not registered")
                        {:type :extension/missing-dependency,
                         :extension ns-sym,
                         :available (vec (keys by-ns))})))
             (doseq [dep (:ext/requires ext)] (visit dep)))
           (vswap! path disj ns-sym)
           (vswap! visited conj ns-sym)
           (vswap! result conj (get by-ns ns-sym))))]
      (doseq [ns-sym (keys by-ns)] (visit ns-sym)))
    @result))
(defn register-extensions!
  "Install all globally registered extensions into an environment.

   Topologically sorts by :ext/requires so dependencies are registered
   before dependents. Throws on missing dependencies or cycles.

   Called by `create-environment` automatically. Returns environment."
  [environment register-fn!]
  (let [exts (registered-extensions)
        sorted (when (seq exts) (topo-sort-extensions exts))]
    (doseq [ext sorted] (register-fn! environment ext))
    environment))
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
               {:type :extension/no-registration,
                :namespace ns-sym,
                :registered (vec (keys @extension-registry))})))))
;; =============================================================================
;; Extension manifest catalog
;;
;; Filled by `discover-extensions!` from every
;; `META-INF/vis-extension/vis.edn` on the classpath. Multiple jars
;; that declare the same id are merged with `:nses` deduped while
;; preserving first-occurrence order.
;; =============================================================================
(defn registered-extension-ids
  "Sorted vector of every extension id known to the manifest registry."
  []
  (vec (sort (keys @extension-manifest-registry))))
(defn extension-namespaces
  "Vector of namespaces declared under `:nses` for an id. Empty when
   the id is unknown."
  [id]
  (vec (get-in @extension-manifest-registry [id :nses] [])))
(defn- merge-manifest-entry!
  [id entry]
  (swap! extension-manifest-registry update
    id
    (fn [existing] {:nses (vec (distinct (concat (:nses existing) (:nses entry))))})))
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
  ;; `defonce`, NOT `def`: registration is a side effect that `require`
  ;; won't re-fire (it's idempotent on already-loaded extension nses), so
  ;; a plain `def` would reset this to `{}` on every `:reload` and orphan
  ;; every registered tag until the app reboots. Matches the other
  ;; registration-populated registries in this ns (`extension-registry`,
  ;; `extension-order`, `extension-source-markers`,
  ;; `extension-manifest-registry`), all `defonce` for the same reason.
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
      {:type :extension/unregistered-op, :op op-keyword, :allowed op-tags})))
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
(defn op-batch-hint-threshold
  "The per-tool high-fan-out batch-hint threshold for `op-keyword`, or nil
   when the tool declared no override (callers fall back to the default).
   Never throws on unknown ops — the hint is advisory, not load-bearing."
  [op-keyword]
  (get @op-keyword->batch-hint op-keyword))
(defn op-presentation
  "Engine-owned presentation metadata for a tool's `:op` keyword:
   `{:tag ...}`. Tool wrappers merge this into their `:info`/`:metadata`
   so channels read canonical keys.

   Badge LABEL is derived from `:tag` by the channel, not stored here.
   Color / glyph / layout remain pure channel concerns."
  [op]
  {:tag (op-tag op)})
(defn registered-extensions-summary
  "Pure data view of the manifest registry: returns `{<id> {:nses [...]}}`
   for every loaded extension."
  []
  (into {} (map (fn [[id entry]] [id {:nses (:nses entry)}])) @extension-manifest-registry))
(def ^:private builtin-extension-nses
  "Core modules that register through the extension API but ship IN the main
   jar (NOT classpath plug-ins discovered via `META-INF/vis-extension/vis.edn`).
   Loaded explicitly here so their top-level `(register-extension! …)` fires as
   a built-in — internal is a first-class contributor of Python symbols /
   render-fns / ctx hooks, same path third-party extensions use.

     foundation — the `v/` kernel (cat/ls/rg/patch + workspace/env ctx). It is
       mandatory (the sandbox bans `slurp` in favour of `cat`; the session
       workspace block waits for its `:ext/ctx`), so it lives in core, not as a
       droppable extension."
  '[com.blockether.vis.internal.foundation.core])
(defn- load-builtin-extensions!
  "`require` each built-in extension ns so its top-level `register-extension!`
   side-effect runs. Idempotent (require won't reload; register is idempotent)."
  []
  (doseq [ns-sym builtin-extension-nses] (require ns-sym)))
(defn discover-extensions!
  "Public entry point for vis's extension wiring.

   First loads the BUILT-IN extensions (`load-builtin-extensions!` — core
   modules like the foundation `v/` kernel that register via the same API but
   ship in the main jar). Then runs `manifest/scan-extensions!` (which scans
   every `META-INF/vis-extension/vis.edn`, `require`s every namespace listed
   under each manifest's `:nses` key, and returns the merged parsed manifests)
   and merges those manifests into this namespace's manifest registry. Returns
   the count of namespaces declared under `:nses` across the merged manifests.

   Idempotent on every layer."
  []
  (load-builtin-extensions!)
  (let [manifests (manifest/scan-extensions!)]
    (doseq [[id entry] manifests] (merge-manifest-entry! id entry))
    (count (mapcat :nses (vals manifests)))))
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
  (load-builtin-extensions!)
  (into {}
    (comp (filter ext-builtin?)
      (mapcat (fn [ext] (wrap-extension-thunked ext env-thunk))))
    (registered-extensions)))
;; =============================================================================
;; CLI bridge -- the `vis ext` parent lives in `internal.main` next to the
;; other top-level built-in parents (`providers`, `sessions`, `doctor`,
;; `update`). Extensions populate it via `:ext/cli` on `extension`;
;; `register-extension!` above forwards each entry through `mount-under-ext`
;; to `register-cmd!`.
;; =============================================================================
