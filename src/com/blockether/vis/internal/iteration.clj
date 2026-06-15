(ns com.blockether.vis.internal.iteration
  "Canonical iteration-entry shape — the single source of truth shared by
   the LIVE progress tracker (`internal/progress`) and the RESUME projection
   (`channel-tui/chat`). Phase 0 of the TUI BLOCK revamp.

   Background: the live-vs-resume split was the root cause of every TUI
   regression. The live tracker accumulated chunks into one map shape; the
   resume path rebuilt a *different* map shape from persisted rows; the
   renderer then had to paper over the divergence with heuristics. This ns
   pins ONE shape both paths populate, plus ONE builder the renderer consumes.

   ## Vocabulary (Phase 0 naming)

     display-block   one TUI card per emitted code fence / merged code-entry.
     ops             ordered tool sink events under a display-block. Each op
                     carries DISPLAY state: the `{:summary :display}` render
                     contract, `:op` keyword, `:status`, timestamps.
     proof envelope  / form — the model-facing per-top-level-form record (the
                     engine `:forms` BLOB). Stays proof-granular.

   ## Canonical iteration-entry

     {:position    n              ;; 0-based display position of the iteration
      :scope       \"tN/iM\"        ;; BLOCK-level scope, never /fK
      :thinking    string-or-nil  ;; reasoning text for this iteration
      :code        \"<full merged fence body>\"
      :ops         [<op> ...]     ;; sink-derived, DISPLAY state, ordered
      :forms       [<form> ...]   ;; proof envelopes (engine :forms BLOB)
      :status      :ok|:error|:running|:cancelled|:timeout
      :duration-ms long
      :error       error-map-or-nil}

   Both pipelines ALSO keep the legacy per-form fields the existing renderer
   reads (`:thinking` / `:forms` with `:result-render` etc.); the canonical
   block-level fields above are layered on top so the renderer can migrate to
   the BLOCK header without breaking the per-form body painter mid-flight.

   ## One op (sink-derived DISPLAY state)

     {:position      n               ;; sink call position within the block
      :op            :git/status     ;; canonical op keyword (or symbol)
      :summary       <ir-or-zones>   ;; render contract :summary
      :display       <ir>            ;; render contract :display
      :status        :ok|:error      ;; per-op status enum
      :started-at-ms long-or-nil
      :finished-at-ms long-or-nil
      :error         error-map-or-nil}

   ## display-block (renderer input)

   `iteration-entry->display-block` projects ONE iteration-entry into ONE
   display-block. Merged multi-fence iterations carry `:merged-fences int`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]))

;; ---------------------------------------------------------------------------
;; Status enum — standardised across engine / progress / restore.
;; ---------------------------------------------------------------------------

(def statuses
  "The canonical iteration / op status enum."
  #{:ok :error :running :cancelled :timeout})

(defn status?
  [x]
  (contains? statuses x))

(def status-glyph
  "Glyph for each status, matching the REVAMP target UX header."
  {:ok        "✓"
   :error     "✗"
   :running   "↻"
   :cancelled "⊘"
   :timeout   "⏱"})

;; ---------------------------------------------------------------------------
;; Block-level scope: strip the per-form `/fK` (or `/bK`) tail so the BLOCK
;; header reports `tN/iM`, never `tN/iM/fK`.
;; ---------------------------------------------------------------------------

(defn block-scope
  "Reduce a form/op scope (`tN/iM/fK`) to its block-level `tN/iM`.
   Tolerates already-block scopes and nil."
  [scope]
  (some-> scope
    str
    (str/replace #"/[fbc][^/]*\s*$" "")
    str/trim
    not-empty))

(defn entry-scope
  "Derive the block-level scope for an iteration entry from the first form
   that carries one. Returns nil when no form scope is available (the
   renderer falls back to a synthesised `tN/iM` from positions)."
  [{:keys [forms]}]
  (some (fn [f] (block-scope (:scope f))) forms))

;; ---------------------------------------------------------------------------
;; Op status: per-op success?/error → enum.
;; ---------------------------------------------------------------------------

(defn op-status
  [{:keys [success? error]}]
  (cond
    (some? error) :error
    (false? success?) :error
    :else :ok))

;; ---------------------------------------------------------------------------
;; Ops — DISPLAY state, one per tool sink event under the block, ordered by
;; sink `:position`. Sourced identically from the per-form `:channel` sink
;; slices both pipelines already carry (live: progress chunk `:channel`;
;; resume: the persisted envelope `:channel`). This is the part the renderer
;; paints as `▶ <LABEL>  <summary-row>` op rows.
;; ---------------------------------------------------------------------------

(defn sink-entry->op
  "Project ONE channel sink entry into a canonical op. The sink entry's
   `:result` is the `{:summary :display}` render contract on success; on
   failure we synthesise the default error contract so the op always carries
   a paintable summary/display."
  [{:keys [position symbol op success? result error started-at-ms finished-at-ms]}]
  (let [contract (if success?
                   result
                   (extension/default-error-result
                     {:success? false :result nil :info {} :error error}))]
    {:position       position
     :op             (or op symbol)
     :summary        (:summary contract)
     :display        (:display contract)
     :status         (if success? :ok :error)
     :started-at-ms  started-at-ms
     :finished-at-ms finished-at-ms
     :error          error}))

(defn form->ops
  "All ops contributed by ONE form (proof envelope), in sink `:position`
   order. A form's `:channel` slice holds one sink entry per tool call it
   touched (regardless of nesting), so a `(let [a (cat) b (cat)] …)` form
   yields the cat/cat/… ops in source order."
  [{:keys [channel]}]
  (->> channel
    (sort-by :position)
    (mapv sink-entry->op)))

(defn entry-ops
  "Ordered ops across ALL forms of an iteration entry. The block is the unit;
   ops from every form flatten into one ordered op list (source order within
   forms, form order across the block)."
  [{:keys [forms]}]
  (into [] (mapcat form->ops) forms))

;; ---------------------------------------------------------------------------
;; Block-level merged code + duration + status + error.
;; ---------------------------------------------------------------------------

(defn entry-code
  "Full merged fence body for the block: every visible form's source joined
   in source order. Falls back to a single form's `:code`."
  [{:keys [forms code]}]
  (or (some-> code str not-empty)
    (let [srcs (keep (fn [f] (some-> (:code f) str str/trim not-empty)) forms)]
      (when (seq srcs) (str/join "\n" srcs)))))

(defn entry-duration-ms
  "Sum of per-form durations (the block ran them sequentially)."
  ^long [{:keys [forms duration-ms]}]
  (if (some? duration-ms)
    (long duration-ms)
    (long (reduce + 0 (keep :duration-ms forms)))))

(defn entry-error
  "First error across the iteration: iter-level `:error` wins, else the first
   errored form's `:error`."
  [{:keys [error forms]}]
  (or error (some :error forms)))

(defn entry-status
  "Standardised iteration status enum derived from the entry. Honours an
   explicit `:status` when one of the canonical enum values is already set
   (cancellation / timeout propagate from the engine); otherwise derives from
   errors and running forms."
  [{:keys [status forms] :as entry}]
  (cond
    (status? status) status
    (some? (entry-error entry)) :error
    (some (fn [{:keys [started-at-ms success?]}]
            (and (some? started-at-ms) (nil? success?)))
      forms) :running
    :else :ok))

;; ---------------------------------------------------------------------------
;; Canonicalisation — fill the block-level fields on an entry that already
;; carries `:forms` (+ legacy per-form fields). Both pipelines call this as
;; their final step so the parity invariant compares fully-canonical entries.
;; ---------------------------------------------------------------------------

(defn canonicalize
  "Layer the canonical block-level fields onto an iteration entry that
   already carries `:forms` and the legacy per-form display fields. Idempotent.

   The result is the shared iteration-entry shape: the SAME map for the live
   and resume paths given the same forms, which is exactly what the parity
   invariant test asserts."
  [entry]
  (let [forms  (vec (or (:forms entry) []))
        entry  (assoc entry :forms forms)
        scope  (or (block-scope (:scope entry)) (entry-scope entry))]
    (assoc entry
      :scope       scope
      :code        (entry-code entry)
      :ops         (entry-ops entry)
      :status      (entry-status entry)
      :duration-ms (entry-duration-ms entry)
      :error       (entry-error entry))))

(def canonical-keys
  "The keys that define the SHARED iteration-entry. The parity invariant
   compares entries projected to exactly these keys, so transient per-path
   bookkeeping (live `:activity` / `:content-stream` / `:elided-form-idxs`;
   resume `:recaps`) never spuriously fails parity. `:forms` is included as
   proof-granular state; the parity fixture feeds identical forms to both
   paths, so the projection must match field-for-field."
  [:position :scope :thinking :code :ops :forms :status :duration-ms :error])

(defn canonical-entry
  "Project any iteration entry (live or resume, pre- or post-`canonicalize`)
   down to the canonical shared shape. Running `canonicalize` first so the
   block-level fields are always present, then `select-keys` to drop
   path-specific bookkeeping. This is the value the parity invariant test
   asserts equal across the live and resume paths."
  [entry]
  (-> entry canonicalize (select-keys canonical-keys)))

(def parity-keys
  "The DISPLAY-relevant subset of the canonical entry the parity invariant
   compares. `:forms` is intentionally EXCLUDED: forms are proof-granular,
   model-facing envelopes whose internal shape legitimately differs by path
   (the live tracker stamps `:started-at-ms`, carries the pre-combined
   `:result-render`; the resume path re-groups envelopes and keeps `:result`).
   The renderer paints `:ops` — the canonical DISPLAY state — so `:ops` plus
   the block-level fields ARE the regression surface."
  [:position :scope :thinking :code :ops :status :duration-ms :error])

(defn parity-entry
  "Project an iteration entry (live or resume) down to the DISPLAY-relevant
   canonical shape and normalise the transient wall-clock stamps that
   legitimately differ by path (the live tracker stamps op/form
   `:started-at-ms` / `:finished-at-ms` from the chunk envelope; the resume
   path has no wall clock). The PARITY INVARIANT compares this projection:
   same chunk fixture, two paths, equal `parity-entry`. Any divergence in
   scope / merged code / ops / status / counts / error is a regression and
   fails the gate."
  [entry]
  (let [strip-op #(dissoc % :started-at-ms :finished-at-ms)]
    (-> entry
      canonicalize
      (select-keys parity-keys)
      (update :ops (fn [os] (mapv strip-op os))))))

;; ---------------------------------------------------------------------------
;; The ONE builder the renderer consumes: iteration-entry -> display-block.
;; One display-block per code fence / merged code-entry, NOT per form.
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Phase 4 — high-fan-out policy.
;;
;; 100 same-op rows in one block is both bad agent behaviour and bad UX. Two
;; orthogonal responses, both PURE projections off the canonical `:ops`:
;;
;;   1. Engine soft warning (`block-batch-hints`): when a block runs MORE
;;      than the threshold ops with the SAME `:op`, surface a "BATCH HINT"
;;      note nudging the agent to call the tool once with a vector arg. The
;;      threshold defaults to `default-batch-hint-threshold` and is overridable
;;      per-tool via `:ext.symbol/batch-hint`.
;;   2. Renderer aggregation (`aggregate-ops`): when a same-op RUN exceeds
;;      `aggregate-threshold`, collapse it into ONE synthetic op the renderer
;;      paints as `▶ CAT × 100 (a, b, c, …)`, expandable to the full list.
;;
;; Aggregation preserves underlying ops for expansion; the renderer decides
;; how much detail to show.
;; ---------------------------------------------------------------------------

(def ^:const default-batch-hint-threshold
  "Default high-fan-out threshold: MORE than this many same-op calls in one
   block trips the soft batch hint. Per-tool override via
   `:ext.symbol/batch-hint`."
  5)

(def ^:const aggregate-threshold
  "Renderer aggregation kicks in when a same-op RUN exceeds this count: the
   run paints as ONE synthetic `▶ OP × N` row instead of N individual rows."
  10)

(def ^:private op-friendly-labels
  "Friendly TUI labels for ops whose raw keyword tail is cryptic.
   Keys are full op keywords; values are the display string."
  {:cat "READ"
   :rg  "SEARCH"
   :ls  "LIST"})

(defn op-label
  "Short label for an op keyword/symbol. Prefers `op-friendly-labels`,
   then falls back to the tail name upcased.
   `:git/status` → \"STATUS\", `:cat` → \"READ\"."
  [op]
  (or (get op-friendly-labels op)
    (-> op name (str/split #"/") last str/upper-case)))

(defn- node-text
  "All string leaves under an IR node, joined and trimmed."
  [node]
  (->> node
    (tree-seq #(or (vector? %) (seq? %)) seq)
    (filter string?)
    (str/join "")
    str/trim
    not-empty))

(defn- op-sample-text
  "A terse identifier for ONE op, used in the aggregated `(a, b, c, …)` head.
   Prefers the op's summary label (first [:strong …] text, recursing into the
   nested `[:span …]` wrapper), else the position."
  [{:keys [summary position]}]
  (or
    (some->> summary
      (tree-seq #(or (vector? %) (seq? %)) seq)
      (some (fn [node]
              (when (and (vector? node) (= :strong (first node)))
                (node-text node)))))
    (str "#" position)))

(defn block-batch-hints
  "Soft engine warnings for a canonicalised entry: one hint per `:op` whose
   same-op count EXCEEDS its threshold. Pure projection off `:ops`.

   `threshold-fn` maps an op keyword to its threshold (per-tool override);
   defaults to `default-batch-hint-threshold` for every op. Returns a vec of
   hint maps in first-seen op order:

     {:op :cat :count 7 :threshold 5
      :text \"BATCH HINT  call (cat [...]) once instead of 7 times\"}"
  ([entry] (block-batch-hints entry (constantly default-batch-hint-threshold)))
  ([entry threshold-fn]
   (let [ops    (:ops (canonicalize entry))
         ;; Preserve first-seen op order for stable, readable output.
         order  (distinct (map :op ops))
         counts (frequencies (map :op ops))]
     (into []
       (keep (fn [op]
               (let [n  (long (get counts op 0))
                     th (long (or (threshold-fn op) default-batch-hint-threshold))]
                 (when (> n th)
                   {:op        op
                    :count     n
                    :threshold th
                    :text      (str "BATCH HINT  call (" (name op)
                                 " [...]) once instead of " n " times")}))))
       order))))

(defn aggregate-ops
  "Collapse maximal RUNS of the same consecutive `:op` longer than
   `aggregate-threshold` into ONE synthetic op for the renderer. Non-runs (and
   short runs) pass through untouched, so a `(let [a (cat) b (patch)] …)` block
   keeps its individual rows while a `(doseq … (cat …))` fan-out collapses.

   A synthetic op carries:
    {:op :cat :aggregate true
      :count n :samples [\"a\" \"b\" \"c\"] :ops [<the n underlying ops>]
      :status (:error when any underlying op errored, else :ok)
      :position (first op's position)}

   The renderer paints it as `▶ CAT × n (a, b, c, …)` and, when expanded,
   the underlying `:ops` as the full per-op list. Order is preserved."
  ([ops] (aggregate-ops ops aggregate-threshold))
  ([ops threshold]
   (let [threshold (long threshold)]
     (into []
       (mapcat (fn [run]
                 (if (> (count run) threshold)
                   (let [op (:op (first run))]
                     [{:op        op
                       :aggregate true
                       :count     (count run)
                       :samples   (mapv op-sample-text run)
                       :ops       (vec run)
                       :status    (if (some (fn [o] (= :error (:status o))) run)
                                    :error :ok)
                       :position  (:position (first run))}])
                   run)))
       (partition-by :op ops)))))

(defn aggregate-op-head
  "The one-line synthetic head text for an aggregated op:
   `CAT × 100 (a, b, c, …)`. `max-samples` caps the inline sample list."
  ([agg] (aggregate-op-head agg 3))
  ([{:keys [op count samples]} max-samples]
   (let [shown   (take max-samples samples)
         more?   (> (clojure.core/count samples) (clojure.core/count shown))
         sample  (when (seq shown)
                   (str " (" (str/join ", " shown) (when more? ", …") ")"))]
     (str (op-label op) " × " count sample))))

(defn iteration-entry->display-block
  "Project ONE canonical iteration-entry into ONE display-block — the single
   shape the renderer paints. `:merged-fences` (when > 1) annotates that this
   block merged several source fences into one card.

   The display-block carries:
     :scope        block-level scope `tN/iM`
     :position     iteration display position
     :code         full merged fence body
     :ops          ordered ops (DISPLAY state)
    :status       enum
     :duration-ms  long
     :error        error-map-or-nil
     :thinking     reasoning text
     :forms        proof envelopes (kept for the body painter)
     :batch-hints  high-fan-out soft warnings (vec, possibly empty)
     :aggregated-ops aggregated op view (runs > aggregate-threshold collapsed)
     :merged-fences int (only when > 1)

   Per-tool batch-hint thresholds come from the extension registry
   (`:ext.symbol/batch-hint`), defaulting to `default-batch-hint-threshold`."
  [entry]
  (let [{:keys [position scope thinking code ops forms status duration-ms error]
         :as ce} (canonicalize entry)
        merged (count (filter (fn [f] (some-> (:code f) str str/trim not-empty)) forms))
        threshold-fn (fn [op]
                       (or (extension/op-batch-hint-threshold op)
                         default-batch-hint-threshold))]
    (cond-> {:position       position
             :scope          scope
             :thinking       thinking
             :code           code
             :ops            ops
             :batch-hints    (block-batch-hints ce threshold-fn)
             :aggregated-ops (aggregate-ops ops)
             :forms          forms
             :status         status
             :duration-ms    duration-ms
             :error          error
             ;; Keep the legacy per-form display fields available to the
             ;; current body painter during the renderer migration.
             :entry          ce}
      (> merged 1) (assoc :merged-fences merged))))
