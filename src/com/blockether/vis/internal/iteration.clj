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
                     contract, `:op` keyword, `:tag`, `:status`, timestamps.
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
      :tag           :observation    ;; :observation | :mutation
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
   [clojure.string :as str]))

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
;; Stdout — the SINGLE display surface for a code block. Whatever the program
;; PRINTED is what both the model (its tool_result) and the human channels
;; (TUI / web bubbles) see. Bare return values are never echoed; render-fn op
;; cards are gone. This joins the per-form `:stdout` slices both pipelines
;; carry (live: progress chunk forms; resume: persisted envelope `:forms`).
;; ---------------------------------------------------------------------------

(def ^:const max-form-stdout-chars
  "Per-form printed-output ceiling for DISPLAY — mirrors the model-facing wire
   cap so one runaway `print()` is head-clipped instead of blowing the
   transcript. The form value still lives in the sandbox to re-slice."
  65536)

(defn forms->stdout
  "Joined printed output across a block's `forms`, in order — the single
   display surface for a code block. Each form's `:stdout` is right-trimmed
   and head-clipped at `max-form-stdout-chars`; blank slices are skipped.
   Returns nil when nothing was printed. Errors are NOT included (channels
   render those as their own rows); this is purely what the program printed,
   matching what the model reads back as its `tool_result`."
  [forms]
  (let [parts (keep (fn [f]
                      (let [s (some-> (:stdout f) str str/trimr)]
                        (when-not (str/blank? s)
                          (let [n (count s)]
                            (if (> n max-form-stdout-chars)
                              (str (subs s 0 max-form-stdout-chars)
                                "\n# ⋯ output clipped at " max-form-stdout-chars "/" n " chars")
                              s)))))
                forms)]
    (when (seq parts) (str/join "\n" parts))))

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
  [:position :scope :thinking :code :forms :status :duration-ms :error])

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
   The block-level fields (scope / merged code / status / duration / error)
   ARE the regression surface."
  [:position :scope :thinking :code :status :duration-ms :error])

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
  (-> entry
    canonicalize
    (select-keys parity-keys)))

