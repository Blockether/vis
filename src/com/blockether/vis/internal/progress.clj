(ns com.blockether.vis.internal.progress
  "Streaming progress tracker — leaf module.

   Channels (TUI, CLI agent, Telegram) consume the iteration loop's
   PHASED chunks via this tracker. Every chunk carries a `:phase`
   keyword that tells the tracker what to do with it; the tracker
   accumulates the chunks into a per-iteration timeline that the
   channel re-renders incrementally.

   Phases (every chunk has exactly one):

     :reasoning        LLM is streaming reasoning text. Updates the
                       iteration entry's `:thinking` field.

     :form-result      One top-level form finished evaluating. Carries
                       `:form-idx`, `:code`, `:result`/`:error`,
                       `:stdout`, `:stderr`, `:execution-time-ms`. The
                       tracker writes per-form data into the
                       iteration entry's parallel vectors at index
                       `:form-idx`.

     :iteration-final  Iteration is complete. Carries `:final` (nil
                       when the turn isn't done yet) and `:done?`
                       (true when this iteration produced the
                       turn-terminal answer). Per-form chunks have
                       already streamed; this is the trim
                       \"iteration done\" marker.

     :iteration-error  Iteration aborted before forms could run
                       (e.g. LLM call failed). Carries `:thinking`
                       and `:error`.

   Public API:

     `(make-progress-tracker)`              — fresh tracker, no callback
     `(make-progress-tracker {:on-update})` — invokes `(on-update timeline chunk)`
                                              on every chunk

   Returns `{:on-chunk fn :get-timeline fn}`. Pass the `:on-chunk` fn
   under `:hooks {:on-chunk ...}` of `conversations/send!`. Each
   timeline entry has the shape:

     {:iteration N
      :thinking  str-or-nil
      :code      [str ...]            ;; per-form, idx-aligned
      :results   [str-or-formatted-error ...]
      :stdouts   [str ...]
      :stderrs   [str ...]
      :durations [int-ms ...]
      :successes [bool ...]
      :error     nil-or-iteration-error
      :final     nil-or-{:answer :iteration-count :status}
      :done?     bool}"
  (:require
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.prompt :as prompt]))

(defn- empty-iteration-entry [iteration]
  {:iteration iteration
   :thinking  nil
   :code      []
   :comments  []
   :results   []
   :stdouts   []
   :stderrs   []
   :durations []
   :successes []
   :error     nil
   :final     nil
   :done?     false})

(defn- pad-to [v target-count]
  (if (< (count v) target-count)
    (into v (repeat (- target-count (count v)) nil))
    v))

(defn- format-form-result
  "Pre-format a per-form chunk's result for renderer consumption.
   Errors get the standard `ERROR: …` prefix; successes get
   `safe-pr-str` (bounded pr-str). Mirrors the formatting the
   pre-streaming bulk chunk used so the TUI render code stays the
   same."
  [chunk]
  (if (:error chunk)
    (error/format-error (:error chunk))
    (prompt/safe-pr-str (:result chunk))))

(defn- write-form-slot
  "Per-form chunks land at `:form-idx`. Pad parallel vectors with
   nils up to that index, then assoc the chunk's data. This
   tolerates out-of-order arrivals (e.g. a future async eval)
   without crashing on `assoc out-of-bounds`."
  [entry chunk]
  (let [idx (:form-idx chunk)
        need (inc idx)]
    (-> entry
      (update :code      #(assoc (pad-to % need) idx (:code chunk)))
      (update :comments  #(assoc (pad-to % need) idx (:comment chunk)))
      (update :results   #(assoc (pad-to % need) idx (format-form-result chunk)))
      (update :stdouts   #(assoc (pad-to % need) idx (or (:stdout chunk) "")))
      (update :stderrs   #(assoc (pad-to % need) idx (or (:stderr chunk) "")))
      (update :durations #(assoc (pad-to % need) idx (or (:execution-time-ms chunk) 0)))
      (update :successes #(assoc (pad-to % need) idx (nil? (:error chunk)))))))

(defn- drop-slot
  "Drop index `idx` from `v`. Out-of-bounds idx returns `v` unchanged.
   Used to ELIDE the `(answer …)` form from the iteration's per-form
   parallel vectors when an iteration produces a final answer — the
   channel renders the answer text below; showing the answer call
   itself in the code trace is redundant noise."
  [v idx]
  (if (and (vector? v) (integer? idx) (not (neg? idx)) (< idx (count v)))
    (into (subvec v 0 idx) (subvec v (inc idx)))
    v))

(defn- elide-form-slots
  "Remove form slots at the given indices from every parallel vector
   in `entry`. Indices shift down, which is fine — the channel
   re-numbers in display order. Used to elide both the `(answer …)`
   form and any form that returned `:vis/silent` (side-effect
   primitives like `conversation-title` that shouldn't appear in
   the code trace)."
  [entry idx-set]
  (reduce
    (fn [e idx]
      (-> e
        (update :code      drop-slot idx)
        (update :comments  drop-slot idx)
        (update :results   drop-slot idx)
        (update :stdouts   drop-slot idx)
        (update :stderrs   drop-slot idx)
        (update :durations drop-slot idx)
        (update :successes drop-slot idx)))
    entry
    (sort > idx-set)))

(defn- update-entry
  "Apply a single chunk to its iteration's timeline entry. Dispatches
   on `:phase`; unknown phases pass through unchanged so a future
   loop-side phase doesn't crash older trackers."
  [entry chunk]
  (case (:phase chunk)
    :reasoning
    (assoc entry :thinking (or (:thinking chunk) (:thinking entry)))

    :form-result
    (write-form-slot entry chunk)

    :iteration-final
    (let [base (assoc entry
                 :thinking (or (:thinking chunk) (:thinking entry))
                 :final    (:final chunk)
                 :done?    (boolean (:done? chunk)))
          ;; Collect all form indices to elide: the answer-bearing
          ;; form (if present) and any forms that returned :vis/silent.
          answer-idx   (when (:final chunk) (:answer-form-idx chunk))
          silent-idxs  (or (:silent-form-idxs chunk) #{})
          elide-idxs   (cond-> silent-idxs
                         (some? answer-idx) (conj answer-idx))]
      (if (seq elide-idxs)
        (elide-form-slots base elide-idxs)
        base))

    :iteration-error
    (assoc entry
      :thinking (or (:thinking chunk) (:thinking entry))
      :error    (:error chunk)
      :done?    true)

    ;; Unknown / missing :phase — leave the entry as-is.
    entry))

(defn make-progress-tracker
  "Create a phased progress tracker.

   Returns `{:on-chunk fn :get-timeline fn}`. The `:on-chunk` fn
   accepts the loop's phased chunks and returns nil. The
   `:get-timeline` fn returns the accumulated timeline vec
   (oldest-iteration first).

   `on-update` (when supplied) is called `(on-update timeline chunk)`
   after every chunk update, so the consumer can re-render
   incrementally."
  ([] (make-progress-tracker nil))
  ([{:keys [on-update]}]
   (let [timeline (atom (sorted-map))
         as-vec   #(vec (vals %))]
     {:on-chunk     (fn [chunk]
                      (let [iteration (:iteration chunk)
                            tl        (swap! timeline update iteration
                                        (fn [entry]
                                          (update-entry
                                            (or entry (empty-iteration-entry iteration))
                                            chunk)))]
                        (when on-update
                          (on-update (as-vec tl) chunk))))
      :get-timeline #(as-vec @timeline)})))
