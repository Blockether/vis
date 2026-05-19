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
   under `:hooks {:on-chunk ...}` of `sessions/send!`. Each
   timeline entry has the shape:

     {:iteration N
      :thinking  str-or-nil
      :code      [str ...]            ;; per-form, idx-aligned
      :render-segments [[{:kind ...}] ...] ;; optional per-block display split
      :results   [str-or-IR-or-formatted-error ...]
      :result-kinds [keyword ...] ;; :tool, :value, or :error
      :result-details [map-or-str ...] ;; extra result metadata
      :durations [int-ms ...]
      :successes [bool ...]
      :started-at-ms [int-ms ...] ;; running form start timestamps
      :silents   [bool ...]       ;; per-form :vis/silent visibility marker
      :provider-fallbacks [map ...] ;; routed provider fallback notices
      :activity  nil-or-keyword      ;; live coarse phase (:provider-call/:response-parse)
      :error     nil-or-iteration-error
      :final     nil-or-{:answer :iteration-count :status}
      :done?     bool}

   The pre-existing `:events` interleaving log was removed: it lived
   only in memory (never persisted), and resumed bubbles re-render
   from this single flat layout. One layout path is enough."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]))

(defn- empty-iteration-entry [iteration]
  {:iteration iteration
   :thinking  nil
   :code      []
   :comments  []
   :render-segments []
   :results   []
   :result-kinds []
   :result-details []
   :durations []
   :successes []
   :started-at-ms []
   :silents   []
   :provider-fallbacks []
   :activity  nil
   :elided-form-idxs #{}
   :error     nil
   :final     nil
   :done?     false})

(defn- pad-to [v target-count]
  (if (< (count v) target-count)
    (into v (repeat (- target-count (count v)) nil))
    v))

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
  "Pre-format a per-form chunk's result for renderer consumption.

   Errors get the standard `ERROR: ...` prefix.

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

(defn- write-form-start-slot
  "Per-block start chunks land at `:position` before eval completes.
   Only `:code` / `:comments` / `:started-at-ms` are populated; result-
   side vectors intentionally stay empty so renderers can distinguish
   running code from completed success or failure."
  [entry chunk]
  (let [idx  (display-form-idx entry (:position chunk))
        need (inc idx)]
    (-> entry
      (update :code     #(assoc (pad-to % need) idx (:code chunk)))
      (update :comments #(assoc (pad-to % need) idx (:comment chunk)))
      (update :render-segments #(assoc (pad-to % need) idx (:render-segments chunk)))
      (update :started-at-ms #(assoc (pad-to % need) idx (:started-at-ms chunk)))
      (update :silents  #(assoc (pad-to % need) idx (silent-chunk? chunk))))))

(defn- write-form-slot
  "Per-block chunks land at `:position`. Pad parallel vectors with
   nils up to that index, then assoc the chunk's data. This
   tolerates out-of-order arrivals (e.g. a future async eval)
   without crashing on `assoc out-of-bounds`."
  [entry chunk]
  (let [idx (display-form-idx entry (:position chunk))
        need (inc idx)]
    (-> entry
      (update :code      #(assoc (pad-to % need) idx (:code chunk)))
      (update :comments  #(assoc (pad-to % need) idx (:comment chunk)))
      (update :render-segments #(assoc (pad-to % need) idx (:render-segments chunk)))
      (update :results   #(assoc (pad-to % need) idx (when-not (and (= :vis/answer (:result chunk))
                                                                 (visible-code-segments? chunk))
                                                       (format-form-result chunk))))
      (update :result-kinds #(assoc (pad-to % need) idx (form-result-kind chunk)))
      (update :result-details #(assoc (pad-to % need) idx (form-result-detail chunk)))
      (update :durations #(assoc (pad-to % need) idx (or (envelope-duration-ms (:envelope chunk)) 0)))
      (update :successes #(assoc (pad-to % need) idx (nil? (:error chunk))))
      (update :silents   #(assoc (pad-to % need) idx (and (nil? (:error chunk))
                                                       (silent-chunk? chunk)))))))

(defn- drop-slot
  "Drop index `idx` from `v`. Out-of-bounds idx returns `v` unchanged.
   Used to ELIDE the `(done ...)` form from the iteration's per-form
   parallel vectors when an iteration produces a final answer - the
   channel renders the answer text below; showing the answer call
   itself in the code trace is redundant noise."
  [v idx]
  (if (and (vector? v) (integer? idx) (not (neg? idx)) (< idx (count v)))
    (into (subvec v 0 idx) (subvec v (inc idx)))
    v))

(defn- insert-slot
  "Insert nil at index `idx` in vector `v`, padding if needed. Used when
   a previously silent form is re-emitted with an error and must become
   visible again without overwriting later visible forms."
  [v idx]
  (if (and (vector? v) (integer? idx) (not (neg? idx)))
    (let [padded (pad-to v idx)]
      (into (conj (subvec padded 0 idx) nil) (subvec padded idx)))
    v))

(defn- elide-form-slots
  "Remove visible form slots at the given indices from every parallel
   vector in `entry`. Indices shift down, which is fine - the channel
   re-numbers in display order."
  [entry idx-set]
  (reduce
    (fn [e idx]
      (-> e
        (update :code      drop-slot idx)
        (update :comments  drop-slot idx)
        (update :render-segments drop-slot idx)
        (update :results   drop-slot idx)
        (update :result-kinds drop-slot idx)
        (update :result-details drop-slot idx)
        (update :durations drop-slot idx)
        (update :successes drop-slot idx)
        (update :started-at-ms drop-slot idx)
        (update :silents drop-slot idx)))
    entry
    (sort > idx-set)))

(defn- hide-form-slot
  "Remember original form index `idx` as elided and remove its current
   visible slot if present. Future chunks with higher original indices
   are shifted left by `display-form-idx`, avoiding nil holes in live
   progress when a silent system call appears before visible work."
  [entry idx]
  (if (contains? (or (:elided-form-idxs entry) #{}) idx)
    entry
    (let [display-idx (display-form-idx entry idx)]
      (-> entry
        (update :elided-form-idxs (fnil conj #{}) idx)
        (elide-form-slots #{display-idx})))))

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
        (update :code      insert-slot display-idx)
        (update :comments  insert-slot display-idx)
        (update :render-segments insert-slot display-idx)
        (update :results   insert-slot display-idx)
        (update :result-kinds insert-slot display-idx)
        (update :result-details insert-slot display-idx)
        (update :durations insert-slot display-idx)
        (update :successes insert-slot display-idx)
        (update :started-at-ms insert-slot display-idx)
        (update :silents insert-slot display-idx)))))

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
    (assoc (write-form-start-slot entry chunk) :activity nil)

    :form-result
    (assoc
      (if (or (structurally-silent-chunk? chunk)
            (and (not (:error chunk))
              (= :vis/answer (:result chunk))
              (not (visible-code-segments? chunk))))
        (hide-form-slot entry (:position chunk))
        (write-form-slot (unhide-form-slot entry (:position chunk)) chunk))
      :activity nil)

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
          ;; Structurally-silent bookkeeping forms (answer emission / title
          ;; updates) are hidden as chunks arrive. Other successful
          ;; `:vis/silent` forms stay in the timeline and are marked in
          ;; `:silents`; channel settings decide whether to render them.
          answer-idx   (when-not duplicate-final?
                         (when (:final chunk) (:answer-position chunk)))
          silent-idxs  (if duplicate-final? #{} (or (:silent-form-idxs chunk) #{}))
          base         (reduce (fn [e idx]
                                 (let [display-idx (display-form-idx e idx)]
                                   (update e :silents #(assoc (pad-to % (inc display-idx)) display-idx true))))
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
                      (let [iteration (:iteration-count chunk)
                            tl        (swap! timeline update iteration
                                        (fn [entry]
                                          (update-entry
                                            (or entry (empty-iteration-entry iteration))
                                            chunk)))]
                        (when on-update
                          (on-update (as-vec tl) chunk))))
      :get-timeline #(as-vec @timeline)})))
