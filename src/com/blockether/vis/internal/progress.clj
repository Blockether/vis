(ns com.blockether.vis.internal.progress
  "Streaming progress tracker - leaf module.

   Channels (TUI, CLI agent, Telegram) consume the iteration loop's
   PHASED chunks via this tracker. Every chunk carries a `:phase`
   keyword that tells the tracker what to do with it; the tracker
   accumulates the chunks into a per-iteration timeline that the
   channel re-renders incrementally.

   Phases (every chunk has exactly one):

     :reasoning        LLM is streaming reasoning text. Updates the
                       iteration entry's `:thinking` field.

     :form-start       One block is about to evaluate. Carries
                       `:position` and `:code`. The tracker writes the
                       code immediately so channels can show the
                       currently-running block before the result lands.

     :form-result      One block finished evaluating. Carries
                       `:position`, `:code`, `:result`/`:error`,
                       and `:envelope` timestamps. The
                       tracker writes per-block data into the
                       iteration entry's parallel vectors at index
                       `:position`. Chunks tagged `:silent?` (or
                       returning `:vis/silent`) are retained with a
                       parallel `:silents` marker so channels can
                       toggle their visibility.

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

     `(make-progress-tracker)`              - fresh tracker, no callback
     `(make-progress-tracker {:on-update})` - invokes `(on-update timeline chunk)`
                                              on every chunk

   Returns `{:on-chunk fn :get-timeline fn}`. Pass the `:on-chunk` fn
   under `:hooks {:on-chunk ...}` of `sessions/send!`. Each timeline
   entry has the shape:

     {:iteration N
      :thinking  str-or-nil
      :forms     [{:position        int            ;; display index after elision
                   :engine-idx      int            ;; original loop form index
                   :code            str
                   :comment         str-or-nil
                   :render-segments [{:kind ...} ...] ;; source classification
                   :result-render   str-or-IR-or-nil  ;; pre-rendered tool result
                   :result-kind     :tool|:value|:error
                   :result-detail   map-or-nil        ;; tool metadata
                   :error           map-or-nil
                   :duration-ms     int
                   :success?        bool
                   :silent?         bool
                   :started-at-ms   int-or-nil
                   :running?        bool} ...]
      :recaps             [str ...]   ;; user-facing recap lines for hidden host work
      :provider-fallbacks [map ...]   ;; routed provider fallback notices
      :activity           nil-or-keyword ;; live coarse phase (:provider-call/:response-parse)
      :elided-form-idxs   #{int ...}  ;; original loop indices hidden from :forms
      :error              nil-or-iteration-error
      :final              nil-or-{:answer :iteration-count :status}
      :done?              bool}

   The pre-existing `:events` interleaving log was removed: it lived
   only in memory (never persisted), and resumed bubbles re-render
   from this single flat layout. One layout path is enough."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]))

(defn- empty-iteration-entry [iteration]
  ;; Per-iteration timeline entry. `:forms` is the canonical per-form
  ;; vector — every form a map carrying primitives (`:code`, `:result`,
  ;; `:error`, ...) and a few pre-derived display projections (the
  ;; pre-rendered `:result` IR, the `:result-kind` tag, the
  ;; `:result-detail` op metadata, and the `:render-segments` source
  ;; classification). The renderer reads `:forms` directly; no parallel
  ;; arrays.
  {:iteration iteration
   :thinking  nil
   :forms     []
   :recaps    []
   :provider-fallbacks []
   :activity  nil
   :elided-form-idxs #{}
   :error     nil
   :final     nil
   :done?     false})

(defn- pad-forms-to
  "Pad `:forms` with placeholder maps until it has at least `n` entries.
   Tolerates out-of-order chunk arrivals (futures landing at higher
   positions before earlier ones)."
  [forms n]
  (if (< (count forms) n)
    (into forms (repeat (- n (count forms)) {:position nil}))
    forms))

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
          (nat-int? (:started-at-ms envelope))
          (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope))
             (long (:started-at-ms envelope))))))

(defn- form-result-kind
  [chunk]
  (cond
    (:error chunk) :error
    (extension/tool-result? (:result chunk)) :tool
    :else :value))

(defn- tool-result-detail
  "Project tool-result envelope to the small detail map TUI labels
   consume. Envelope is flat (`:symbol`, `:tag`, `:metadata`)."
  [tool-result]
  (when (extension/tool-result? tool-result)
    (let [meta (or (:metadata tool-result) {})]
      (cond-> {}
        (:symbol tool-result) (assoc :op (:symbol tool-result))
        (:tag tool-result)    (assoc :tag (:tag tool-result))
        :always (merge (select-keys meta [:spec :paths :hit-count :truncated-by
                                          :command :cwd :target]))))))

(defn- form-result-detail
  [chunk]
  (tool-result-detail (:result chunk)))

(defn- format-form-result
  "Pre-format a successful per-form chunk's result for renderer consumption.

   Eval errors are stored separately in :errors; TUI renders them inline
   with the failing source and caret marker.

   Tool calls inside the form land in `:channel` as a vec of sink
   entries (one per call, regardless of nesting). When non-empty, we
   concat their pre-rendered markdown so the TUI bubble shows EVERY
   call's render, not just the form's last-expression value.

   When `:channel` is empty (plain-value form: `(+ 1 2)`, a `def` whose
   value isn't a tool-result, etc.) the form-level `:result` IS what
   the model wrote: render via `render-tool-result` when the
   value is an `:envelope`, otherwise bounded plain-value text."
  [chunk]
  (if (:error chunk)
    (extension/default-error-ir {:success? false :error (:error chunk)})
    (let [channel-entries (seq (:channel chunk))]
      (cond
        channel-entries
        ;; Sort by :position so racy futures (which can land in
        ;; completion order rather than source order) render in canonical
        ;; source order.
        (extension/combine-render-values
          (map (fn [{:keys [success? result error]}]
                 (if success?
                   result
                   ;; Per PLAN §2.1: build the envelope shape the
                   ;; default error formatter expects.
                   (extension/default-error-ir
                     {:success? false :error error})))
            (sort-by :position channel-entries)))

        (extension/tool-result? (:result chunk))
        (extension/render-tool-result (:result chunk))

        :else
        (fmt/bounded-value-str (:result chunk))))))

(defn- normalize-thinking-text [thinking]
  (some-> thinking str str/trim))

(defn- display-form-idx
  "Map original engine `form-idx` to the visible vector index after any
   prior silent system-call forms have been elided."
  [entry idx]
  (if (integer? idx)
    (let [elided (or (:elided-form-idxs entry) #{})]
      (- idx (count (filter #(< % idx) elided))))
    idx))

(defn- silent-chunk?
  [chunk]
  (boolean
    (or (:silent? chunk)
      (= :vis/silent (:result chunk)))))

(defn- visible-code-segments?
  "True when the chunk has at least one `:code` segment that should
   reach the user, regardless of structural subforms. Reads from
   `:render-segments` first (the loop carries the parsed segments on
   the chunk); falls back to a quick textual check when missing."
  [chunk]
  (let [segments (:render-segments chunk)]
    (if (seq segments)
      (boolean (some #(= :code (:kind %)) segments))
      (let [code (str/trim (str (:code chunk)))]
        (and (not (str/blank? code))
          (not (or (str/starts-with? code "(set-session-title!")
                 (str/starts-with? code "(done"))))))))

(defn- structurally-silent-chunk?
  "True for host-bookkeeping forms that should never appear in user traces:
   session-title updates and answer-emission forms. They may still execute
   and affect channel chrome/final answer, but the code/result row itself is
   noise in both TUI and CLI trace views. Mixed blocks with visible code
   segments are not silent; channels consume :render-segments to hide only the
   structural subforms."
  [chunk]
  (let [code (str (:code chunk))
        trimmed (str/triml code)]
    (boolean
      (or (:vis/structurally-silent? chunk)
        (str/starts-with? trimmed "(done")
        (and (not (visible-code-segments? chunk))
          (or (str/includes? code "(set-session-title!")
            (str/includes? code "(done")))
        (and (= :vis/silent (:result chunk))
          (not (seq (:render-segments chunk)))
          (or (str/includes? code "(set-session-title!")
            (str/includes? code "(done")))))))

(defn- title-recap
  [value]
  (let [title (some-> value str str/trim not-empty)]
    (if title
      (str "Title changed to \"" title "\".")
      "Title changed.")))

(defn- render-segment-recaps
  [segments]
  (->> segments
    (keep (fn [{:keys [kind value]}]
            (when (= :title kind)
              (title-recap value))))
    vec))

(defn- append-recaps
  [entry recaps]
  (let [recaps (->> recaps (remove str/blank?) distinct vec)]
    (if (seq recaps)
      (update entry :recaps #(vec (distinct (concat (or % []) recaps))))
      entry)))

(defn- chunk->form-start
  "Build the initial `:forms` entry for a `:form-start` chunk. Only the
   code/comment/start timestamp are known; result-side fields stay nil."
  [display-idx chunk]
  {:position        display-idx
   :engine-idx      (:position chunk)
   :code            (:code chunk)
   :comment         (:comment chunk)
   :render-segments (:render-segments chunk)
   :started-at-ms   (:started-at-ms chunk)
   :silent?         (silent-chunk? chunk)
   :running?        true})

(defn- chunk->form-result
  "Build the completed `:forms` entry for a `:form-result` chunk.
   Carries the raw `:error` and the pre-derived display projections
   (`:result-render`, `:result-kind`, `:result-detail`) the renderer
   reads verbatim.

   `:result-render` is nil when:
     - the form errored (the renderer paints `:error` inline with the
       failing source caret; the per-form result row is suppressed)
     - the form returned `:vis/answer` for the answer slot (the
       channel renders the answer text below the trace)"
  [display-idx prev-form chunk]
  (let [errored?     (some? (:error chunk))
        answer-slot? (and (not errored?)
                       (= :vis/answer (:result chunk))
                       (visible-code-segments? chunk))]
    {:position        display-idx
     :engine-idx      (:position chunk)
     :code            (:code chunk)
     :comment         (:comment chunk)
     :render-segments (:render-segments chunk)
     :started-at-ms   (or (:started-at-ms chunk) (:started-at-ms prev-form))
     :duration-ms     (or (envelope-duration-ms (:envelope chunk)) 0)
     :result-render   (when-not (or errored? answer-slot?) (format-form-result chunk))
     :result-kind     (form-result-kind chunk)
     :result-detail   (form-result-detail chunk)
     :error           (:error chunk)
     :success?        (not errored?)
     :silent?         (and (not errored?) (silent-chunk? chunk))
     :running?        false}))

(defn- assoc-form
  [entry display-idx form]
  (let [need (inc display-idx)]
    (update entry :forms #(assoc (pad-forms-to % need) display-idx form))))

(defn- drop-form-at
  "Remove the form at display index `idx` from `:forms`. Indices shift
   down; the channel re-numbers in display order."
  [entry idx]
  (update entry :forms
    (fn [forms]
      (if (and (vector? forms) (integer? idx) (not (neg? idx)) (< idx (count forms)))
        (into (subvec forms 0 idx) (subvec forms (inc idx)))
        forms))))

(defn- insert-empty-form-at
  "Insert a placeholder form at display index `idx`, padding if needed.
   Used by `unhide-form-slot` when a previously silent form must become
   visible without overwriting later visible slots."
  [entry idx]
  (update entry :forms
    (fn [forms]
      (if (and (vector? forms) (integer? idx) (not (neg? idx)))
        (let [padded (pad-forms-to forms idx)]
          (into (conj (subvec padded 0 idx) {:position idx})
            (subvec padded idx)))
        forms))))

(defn- hide-form-slot
  "Remember original form index `idx` as elided and drop its current
   visible slot if present. Future chunks with higher original indices
   are shifted left by `display-form-idx`, avoiding nil holes in live
   progress when a silent system call appears before visible work."
  ([entry idx]
   (hide-form-slot entry idx nil))
  ([entry idx recaps]
   (let [entry (append-recaps entry recaps)]
     (if (contains? (or (:elided-form-idxs entry) #{}) idx)
       entry
       (let [display-idx (display-form-idx entry idx)]
         (-> entry
           (update :elided-form-idxs (fnil conj #{}) idx)
           (drop-form-at display-idx)))))))

(defn- unhide-form-slot
  "Make original form index `idx` visible again. This happens when an
   answer form first succeeded silently, then validation re-emitted the
   same form with an error."
  [entry idx]
  (if-not (contains? (or (:elided-form-idxs entry) #{}) idx)
    entry
    (let [display-idx (display-form-idx entry idx)]
      (-> entry
        (update :elided-form-idxs disj idx)
        (insert-empty-form-at display-idx)))))

(defn- update-entry
  "Apply a single chunk to its iteration's timeline entry. Dispatches
   on `:phase`; unknown phases pass through unchanged so a future
   loop-side phase doesn't crash older trackers."
  [entry chunk]
  (case (:phase chunk)
    :provider-call
    (assoc entry :activity :provider-call)

    :response-parse
    (if (= :done (:status chunk))
      (assoc entry :activity nil :response-parse chunk)
      (assoc entry :activity :response-parse :response-parse chunk))

    :reasoning
    (let [next-thinking (or (normalize-thinking-text (:thinking chunk))
                          (normalize-thinking-text (:thinking entry)))]
      (assoc entry :thinking next-thinking :activity nil))

    :provider-fallback
    (-> entry
      (assoc :activity :provider-call)
      (update :provider-fallbacks conj
        (or (:event chunk) (select-keys chunk [:reason :failed-provider :new-provider :fallback]))))

    :form-start
    (let [display-idx (display-form-idx entry (:position chunk))
          entry'      (unhide-form-slot entry (:position chunk))]
      (assoc (assoc-form entry' display-idx (chunk->form-start display-idx chunk))
        :activity nil))

    :form-result
    (let [silent? (or (structurally-silent-chunk? chunk)
                    (and (not (:error chunk))
                      (= :vis/answer (:result chunk))
                      (not (visible-code-segments? chunk))))]
      (if silent?
        (assoc (hide-form-slot entry (:position chunk)
                 (render-segment-recaps (:render-segments chunk)))
          :activity nil)
        (let [entry'      (unhide-form-slot entry (:position chunk))
              display-idx (display-form-idx entry' (:position chunk))
              prev-form   (get (:forms entry') display-idx)]
          (assoc (assoc-form entry' display-idx
                   (chunk->form-result display-idx prev-form chunk))
            :activity nil))))

    :iteration-final
    (let [duplicate-final? (and (:done? entry) (:final entry) (:final chunk))
          base (assoc entry
                 :thinking (or (normalize-thinking-text (:thinking chunk))
                             (normalize-thinking-text (:thinking entry)))
                 :activity nil
                 :final    (:final chunk)
                 :done?    (boolean (:done? chunk)))
          ;; Elide `(done ...)`: the answer text already renders below;
          ;; showing the answer call itself in the trace is redundant.
          ;; Structurally-silent bookkeeping forms (answer emission /
          ;; title updates) are hidden as chunks arrive. Other
          ;; successful `:vis/silent` forms stay in the timeline with
          ;; `:silent? true`; channel settings decide whether to render
          ;; them.
          answer-idx   (when-not duplicate-final?
                         (when (:final chunk) (:answer-position chunk)))
          silent-idxs  (if duplicate-final? #{} (or (:silent-form-idxs chunk) #{}))
          base         (reduce (fn [e engine-idx]
                                 (let [display-idx (display-form-idx e engine-idx)]
                                   (if (and (integer? display-idx)
                                         (< display-idx (count (:forms e))))
                                     (assoc-in e [:forms display-idx :silent?] true)
                                     e)))
                         base
                         (sort silent-idxs))]
      (if (some? answer-idx)
        (hide-form-slot base answer-idx)
        base))

    :iteration-error
    (assoc entry
      :thinking (or (normalize-thinking-text (:thinking chunk))
                  (normalize-thinking-text (:thinking entry)))
      :activity nil
      :error    (:error chunk)
      :done?    true)

    ;; Unknown / missing :phase - leave the entry as-is.
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
                      ;; Loop emits a mix of chunk shapes: per-iteration
                      ;; lifecycle chunks (`:reasoning`, `:form-result`,
                      ;; `:iteration-final`, `:provider-fallback`) carry
                      ;; `:iteration-count`, while transport-level chunks
                      ;; (`:provider-call`, `:response-parse`,
                      ;; `:iteration-error`) historically carried only
                      ;; `:iteration`. Both keys hold the same 1-based
                      ;; iteration position. Reading just `:iteration-count`
                      ;; routed the latter group into a `nil` bucket of
                      ;; the sorted-map, where it sorted BEFORE every
                      ;; real iteration and shifted live iteration
                      ;; numbering by +1 (the visible "iteration 2" for
                      ;; what the final result reported as a single
                      ;; iteration). Accept either key here; skip when
                      ;; neither is set so a malformed chunk does not
                      ;; resurrect the phantom-bucket bug.
                      (when-let [iteration (or (:iteration-count chunk)
                                             (:iteration chunk))]
                        (let [tl (swap! timeline update iteration
                                   (fn [entry]
                                     (update-entry
                                       (or entry (empty-iteration-entry iteration))
                                       chunk)))]
                          (when on-update
                            (on-update (as-vec tl) chunk)))))
      :get-timeline #(as-vec @timeline)})))
