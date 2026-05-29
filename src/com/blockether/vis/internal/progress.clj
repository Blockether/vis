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
                       and `:envelope` timestamps. The tracker writes
                       the completed form record into `:forms` at the
                       chunk's display index. Chunks tagged `:silent?`
                       (or returning `:vis/silent`) keep their `:silent?`
                       flag so channels can toggle visibility.

     :iteration-final  Iteration is complete. Carries `:final` (nil
                       when the turn isn't done yet) and `:done?`
                       (true when this iteration produced the
                       turn-terminal answer). Per-form chunks have
                       already streamed; this is the trim
                       \"iteration done\" marker.

     :iteration-error  Iteration aborted before forms could run
                       (e.g. LLM call failed). Carries `:thinking`
                       and `:error`.

     :provider-retry-reset
                       Provider stream failed before code eval and Vis is
                       retrying the provider call. Clears stale live
                       reasoning/content for this attempt and keeps a retry
                       recap in `:provider-fallbacks`.

   Public API:

     `(make-progress-tracker)`              - fresh tracker, no callback
     `(make-progress-tracker {:on-update})` - invokes `(on-update timeline chunk)`
                                              on every chunk

   Returns `{:on-chunk fn :get-timeline fn}`. Pass the `:on-chunk` fn
   under `:hooks {:on-chunk ...}` of `sessions/send!`. Each timeline
   entry has the shape:

     {:iteration N
      :thinking  str-or-nil
      :forms     [{:code            str
                   :comment         str-or-nil
                   :render-segments [{:kind ...} ...] ;; source classification
                   :result-render   str-or-IR-or-nil  ;; pre-rendered tool result
                   :result-kind     :tool|:value|:error
                   :result-detail   map-or-nil        ;; tool metadata
                   :error           map-or-nil
                   :duration-ms     int
                   :success?        bool
                   :silent?         bool
                   :started-at-ms   int-or-nil} ...]
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
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.iteration :as iteration]))

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
  "Classify a form chunk for the channel's `show-result?` gate.

   Rules:
     :error  —  the form threw / preflight rejected.
     :tool   —  the form *touched* the tool surface (either its
                top-level value is a tool envelope, OR it carries
                `:channel` sink entries from inner tool calls like
                `(def r (v/ls …))`). Without this, a tool
                preview computed by `format-form-result` would be
                suppressed by the renderer's `(= :tool result-kind)`
                gate when the FENCE's last value is plain data
                (e.g. `(select-keys r …)`).
     :value  —  everything else (plain Clojure values)."
  [chunk]
  (cond
    (:error chunk)                            :error
    (extension/tool-result? (:result chunk))  :tool
    (seq (:channel chunk))                    :tool
    :else                                     :value))

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
        ;; source order. Each sink entry's `:result` is the
        ;; `{:summary :display}` contract; flatten to a summary-led IR
        ;; (badge paragraph first, expanded body after).
        (extension/combine-render-values
          (map (fn [{:keys [success? result error]}]
                 (if success?
                   (extension/render-fn-result->ir result)
                   (extension/render-fn-result->ir
                     (extension/default-error-result
                       {:success? false :error error}))))
            (sort-by :position channel-entries)))

        (extension/tool-result? (:result chunk))
        (extension/render-fn-result->ir
          (extension/render-tool-result (:result chunk)))

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

;; Engine-form detection ("is this form silent UI chrome?") delegates
;; to `ctx-engine/engine-form-src?`. It edamame-parses the head symbol
;; so reader meta / discard forms / tagged literals never trip it, and
;; there is exactly ONE list of engine-form heads in the codebase
;; (`engine-form-heads` in `ctx-engine`). The old string-prefix scan
;; over raw source was a false-positive magnet (a `"(done x)"` inside
;; a string would have matched).

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
          (not (ctx-engine/engine-form-src? code)))))))

(defn- structurally-silent-chunk?
  "True for host-bookkeeping forms that should never appear in user traces:
   session-title updates, answer-emission forms, ctx mutators
   (`task-set!`, `fact-set!`), and engine-internal
   probes (`introspect-*`). They may still execute and feed channel
   chrome / final answer, but the code/result row itself is noise in
   both TUI and CLI trace views. Mixed blocks with visible code
   segments are not silent; channels consume :render-segments to hide
   only the structural subforms."
  [chunk]
  (let [code (str (:code chunk))]
    (boolean
      (or (:vis/structurally-silent? chunk)
        (and (not (visible-code-segments? chunk))
          (ctx-engine/engine-form-src? code))
        (and (= :vis/silent (:result chunk))
          (not (seq (:render-segments chunk)))
          (ctx-engine/engine-form-src? code))))))

(defn- title-recap
  [value]
  (let [title (some-> value str str/trim not-empty)]
    (if title
      (str "Title — \"" title "\"")
      "Title changed.")))

(defn- status-glyph
  "Compact prefix glyph for a ctx status: :done -> '✓', :cancelled
   -> '×', :todo/:doing -> '…', nil/unknown -> ''. Pure ASCII would
   undercut the visual call-out, but the three glyphs we use here
   render in every terminal we ship to."
  [status]
  (case status
    :done       "✓ "
    :cancelled  "× "
    (:todo :doing) "… "
    ""))

(defn- task-recap
  [{:keys [id status title]}]
  (let [head (cond
               (and id status) (str (status-glyph status) (pr-str id) "  :" (name status))
               id              (str (pr-str id))
               :else           "task update")]
    (str "Task — " head
      (when title (str "  \"" title "\"")))))

(defn- fact-recap
  [{:keys [id status]}]
  (str "Fact — " (pr-str id)
    (when status (str "  :" (name status)))))

(defn- render-segment-recaps
  [segments]
  (->> segments
    (keep (fn [{:keys [kind value] :as seg}]
            (case kind
              :title         (title-recap value)
              :task-update   (task-recap seg)
              :fact-update   (fact-recap seg)
              nil)))
    vec))

(defn- append-recaps
  [entry recaps]
  (let [recaps (->> recaps (remove str/blank?) distinct vec)]
    (if (seq recaps)
      (update entry :recaps #(vec (distinct (concat (or % []) recaps))))
      entry)))

(defn- chunk->form-start
  "Build the initial `:forms` entry for a `:form-start` chunk. Only the
   code/comment/start timestamp are known; result-side fields stay nil.
   The form is running implicitly: `:started-at-ms` is set and
   `:success?` is nil."
  [chunk]
  {:code            (:code chunk)
   :comment         (:comment chunk)
   :render-segments (:render-segments chunk)
   :scope           (:scope chunk)
   :started-at-ms   (:started-at-ms chunk)
   :silent?         (silent-chunk? chunk)})

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
  [prev-form chunk]
  (let [errored?     (some? (:error chunk))
        answer-slot? (and (not errored?)
                       (= :vis/answer (:result chunk))
                       (visible-code-segments? chunk))]
    {:code            (:code chunk)
     :comment         (:comment chunk)
     :render-segments (:render-segments chunk)
     :scope           (or (:scope chunk) (:scope prev-form))
     :started-at-ms   (or (:started-at-ms chunk) (:started-at-ms prev-form))
     :duration-ms     (or (envelope-duration-ms (:envelope chunk)) 0)
     ;; Carry the raw sink slice so the shared `iteration/entry-ops`
     ;; derives DISPLAY-state ops identically to the resume path (which
     ;; keeps `:channel` on its restored blocks). `:result-render` stays
     ;; the pre-combined IR the legacy per-form body painter consumes.
     :channel         (vec (:channel chunk))
     :result-render   (when-not (or errored? answer-slot?) (format-form-result chunk))
     :result-kind     (form-result-kind chunk)
     :result-detail   (form-result-detail chunk)
     :error           (:error chunk)
     :success?        (not errored?)
     :silent?         (and (not errored?) (silent-chunk? chunk))}))

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
      (-> entry
        (dissoc :content-stream)
        (assoc :activity nil :response-parse chunk))
      (assoc entry :activity :response-parse :response-parse chunk))

    :reasoning
    (let [next-thinking (or (normalize-thinking-text (:thinking chunk))
                          (normalize-thinking-text (:thinking entry)))]
      (assoc entry :thinking next-thinking :activity nil))

    :content
    ;; Provider content stream (answer Markdown + code fence) — kept
    ;; on entry as `:content-stream` so the live bubble can render it
    ;; below the reasoning text. Cleared by :response-parse :done and
    ;; :iteration-final once parsed forms take over.
    (let [next-content (or (normalize-thinking-text (:content chunk))
                         (:content-stream entry))]
      (assoc entry :content-stream next-content :activity nil))

    :provider-fallback
    (-> entry
      (assoc :activity :provider-call)
      (update :provider-fallbacks conj
        (or (:event chunk) (select-keys chunk [:reason :failed-provider :new-provider :fallback]))))

    :provider-retry-reset
    ;; Provider stream died after emitting live text. Vis retries the whole
    ;; provider call before any code eval, so rewind the live attempt: drop
    ;; stale reasoning/content/parse state and keep only the retry recap.
    (-> entry
      (assoc :activity :provider-call)
      (dissoc :thinking :content-stream :response-parse)
      (update :provider-fallbacks conj
        (or (:event chunk) (select-keys chunk [:reason :failed-provider :new-provider :fallback]))))

    :form-start
    (let [entry'      (unhide-form-slot entry (:position chunk))
          display-idx (display-form-idx entry' (:position chunk))]
      (assoc (assoc-form entry' display-idx (chunk->form-start chunk))
        :activity nil))

    :form-result
    (let [silent?       (or (structurally-silent-chunk? chunk)
                          (and (not (:error chunk))
                            (= :vis/answer (:result chunk))
                            (not (visible-code-segments? chunk))))
          ;; Done-blocked refusal lands as a form result whose value
          ;; is `{:vis/done-blocked? true :reason :pending-consults}`.
          ;; Surface as a synthetic CONSULT recap so the channel
          ;; renders an inline banner explaining the refusal.
          done-blocked? (and (map? (:result chunk))
                          (:vis/done-blocked? (:result chunk)))
          entry-with-recap
          (if-not done-blocked?
            entry
            (let [pending (mapv :id (:pending-consults entry))
                  banner  (str "CONSULT  done refused — "
                            (count pending) " consult"
                            (when (not= 1 (count pending)) "s")
                            " pending"
                            (when (seq pending)
                              (str ": " (clojure.string/join ", "
                                          (map str pending)))))]
              (update entry :recaps (fnil conj []) banner)))]
      (if silent?
        (assoc (hide-form-slot entry-with-recap (:position chunk)
                 (render-segment-recaps (:render-segments chunk)))
          :activity nil)
        (let [entry'      (unhide-form-slot entry-with-recap (:position chunk))
              display-idx (display-form-idx entry' (:position chunk))
              prev-form   (get (:forms entry') display-idx)]
          (assoc (assoc-form entry' display-idx
                   (chunk->form-result prev-form chunk))
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

    :consult-requested
    ;; Async consult kicked off on a side thread. Track on
    ;; `:pending-consults` until a `:consult-resolved` for the same
    ;; `:id` lands, then drop. Channels render a pending pill from
    ;; this vec; resolved entries live on `:consults`.
    (update entry :pending-consults (fnil conj [])
      {:id         (:id chunk)
       :preference (:preference chunk)
       :focus      (:focus chunk)
       :question   (:question chunk)
       :scope      (:scope chunk)})

    :consult-resolved
    ;; Async consult resolved between iters. We accumulate resolutions
    ;; on `:consults` so the channel renders them as a distinct lane,
    ;; not interleaved with the model's own form-result rows. Drop the
    ;; matching pending entry (by `:id`) since it's now a resolution.
    (let [resolution {:id     (:id chunk)
                      :scope  (:scope chunk)
                      :result (:result chunk)}]
      (-> entry
        (update :consults (fnil conj []) resolution)
        (update :pending-consults
          (fn [pending]
            (vec (remove #(= (:id %) (:id chunk)) (or pending [])))))))

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
         ;; Canonicalize every entry as it leaves the tracker: layer the
         ;; shared block-level fields (`:scope` `:code` `:ops` `:status`
         ;; `:duration-ms` `:error` + 0-based `:position`) on top of the
         ;; raw per-iteration accumulation. Stored entries stay raw so the
         ;; chunk reducer keeps its transient bookkeeping; consumers
         ;; (renderer, parity test) see the SAME canonical shape the resume
         ;; path produces.
         canon    (fn [iteration entry]
                    (iteration/canonicalize
                      (assoc entry :position (when (integer? iteration)
                                               (max 0 (dec (long iteration)))))))
         as-vec   #(vec (map (fn [[it entry]] (canon it entry)) %))]
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
