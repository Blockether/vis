(ns com.blockether.vis.internal.iteration
  "Canonical iteration-entry shape — the single source of truth shared by
   the LIVE progress tracker (`internal/progress`) and the RESUME projection
   (`channel-tui/chat`).

   Background: the live-vs-resume split was the root cause of every TUI
   regression. The live tracker accumulated chunks into one map shape; the
   resume path rebuilt a *different* map shape from persisted rows. This ns
   pins ONE shape both paths populate.

   ## Vocabulary

     form envelope — one block = one form record (the engine `:forms`
                     BLOB), carrying `:code`, `:result`, `:error`, and
                     `:stdout` (what the block PRINTED — the single display
                     surface; op cards / render-fns are gone).

   ## Canonical iteration-entry

     {:position    n              ;; 0-based display position of the iteration
      :scope       \"tN/iM\"        ;; BLOCK-level scope, never /fK
      :thinking    string-or-nil  ;; reasoning text for this iteration
      :code        \"<block source>\"
      :forms       [<form> ...]   ;; form envelopes (engine :forms BLOB)
      :status      :ok|:error|:running|:cancelled|:timeout
      :duration-ms long
      :error       error-map-or-nil}"
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Status enum — standardised across engine / progress / restore.
;; ---------------------------------------------------------------------------

(def statuses
  "The canonical iteration / op status enum."
  #{:ok :error :running :cancelled :timeout})

(defn status? [x] (contains? statuses x))

(def status-glyph
  "Glyph for each status, matching the REVAMP target UX header."
  {:ok "✓" :error "✗" :running "↻" :cancelled "⊘" :timeout "⏱"})

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
  (some (fn [f]
          (block-scope (:scope f)))
        forms))

;; ---------------------------------------------------------------------------
;; Op status: per-op success?/error → enum.
;; ---------------------------------------------------------------------------

(defn op-status
  [{:keys [success? error]}]
  (cond (some? error) :error
        (false? success?) :error
        :else :ok))

;; ---------------------------------------------------------------------------
;; Stdout — the SINGLE display surface for a code block. Whatever the program
;; PRINTED is what both the model (its tool_result) and the human channels
;; (TUI / web bubbles) see. Bare return values are never echoed; render-fn op
;; cards are gone. Each block carries its OWN `:stdout` directly (live:
;; progress chunk forms; resume: persisted envelope `:forms`).
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Block-level merged code + duration + status + error.
;; ---------------------------------------------------------------------------

(defn entry-code
  "The block's source. Uses the entry's `:code` when present, else joins
   the forms' `:code` slices in order."
  [{:keys [forms code]}]
  (or (some-> code
              str
              not-empty)
      (let [srcs (keep (fn [f]
                         (some-> (:code f)
                                 str
                                 str/trim
                                 not-empty))
                       forms)]
        (when (seq srcs) (str/join "\n" srcs)))))

(defn entry-duration-ms
  "Block duration: the explicit `:duration-ms` when present, else the sum
   of the forms' durations."
  ^long [{:keys [forms duration-ms]}]
  (if (some? duration-ms) (long duration-ms) (long (reduce + 0 (keep :duration-ms forms)))))

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
  (cond (status? status) status
        (some? (entry-error entry)) :error
        (some (fn [{:keys [started-at-ms success?]}]
                (and (some? started-at-ms) (nil? success?)))
              forms)
        :running
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
  (let [forms
        (vec (or (:forms entry) []))

        entry
        (assoc entry :forms forms)

        scope
        (or (block-scope (:scope entry)) (entry-scope entry))]

    (assoc entry
      :scope scope
      :code (entry-code entry)
      :status (entry-status entry)
      :duration-ms (entry-duration-ms entry)
      :error (entry-error entry))))

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
  (-> entry
      canonicalize
      (select-keys canonical-keys)))

(def parity-keys
  "The DISPLAY-relevant subset of the canonical entry the parity invariant
   compares. `:forms` is intentionally EXCLUDED: form envelopes are
   model-facing and their internal shape legitimately differs by path
   (the live tracker stamps `:started-at-ms`; the resume path has no wall
   clock). The block-level fields (scope / code / status / duration / error)
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

