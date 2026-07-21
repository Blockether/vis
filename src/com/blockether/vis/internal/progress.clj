(ns com.blockether.vis.internal.progress
  "Streaming progress tracker - leaf module.

   Channels (TUI, CLI agent) consume the iteration loop's
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
                       turn-terminal answer). The block chunk has
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
      :provider-fallbacks [map ...]   ;; routed provider fallback notices
      :activity           nil-or-keyword ;; live coarse phase (:provider-call/:response-parse)
      :elided-form-idxs   #{int ...}  ;; original loop indices hidden from :forms
      :error              nil-or-iteration-error
      :final              nil-or-{:answer :iteration-count :status}
      :done?              bool}

   The pre-existing `:events` interleaving log was removed: it lived
   only in memory (never persisted), and resumed bubbles re-render
   from this single flat layout. One layout path is enough."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.form :as form]
            [com.blockether.vis.internal.iteration :as iteration]))

(defn- empty-iteration-entry
  [iteration]
  ;; Per-iteration timeline entry. `:forms` is the canonical per-form
  ;; vector — every form a map carrying primitives (`:code`, `:result`,
  ;; `:error`, ...) and a few pre-derived display projections (the
  ;; pre-rendered `:result` IR, the `:result-kind` tag, the
  ;; `:result-detail` op metadata, and the `:render-segments` source
  ;; classification). The renderer reads `:forms` directly; no parallel
  ;; arrays.
  {:iteration iteration
   :thinking nil
   :forms []
   :provider-fallbacks []
   :activity nil
   :elided-form-idxs #{}
   :error nil
   :final nil
   :done? false})

(defn- pad-forms-to
  "Pad `:forms` with placeholder maps until it has at least `n` entries.
   Tolerates out-of-order chunk arrivals (futures landing at higher
   positions before earlier ones)."
  [forms ^long n]
  (let [c (long (count forms))]
    (if (< c n) (into forms (repeat (- n c) {:position nil})) forms)))

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
             (nat-int? (:started-at-ms envelope))
             (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))

(defn- form-result-kind
  "Classify a form chunk for the channel's `show-result?` gate.

   Rules:
     :error  —  the form threw / preflight rejected.
     :tool   —  the form *touched* the tool surface (either its
                top-level value is a tool envelope, OR it carries
                `:channel` sink entries from inner tool calls like
                `(def r (ls …))`). Without this, a tool
                preview computed by `format-form-result` would be
                suppressed by the renderer's `(= :tool result-kind)`
                gate when the form's last value is plain data
                (e.g. `(select-keys r …)`).
     :value  —  everything else (plain Clojure values)."
  [chunk]
  (cond (:error chunk) :error
        (extension/tool-result? (:result chunk)) :tool
        (seq (:channel chunk)) :tool
        :else :value))

(defn- tool-result-detail
  "Project tool-result envelope to the small detail map TUI labels
   consume. Envelope is flat (`:symbol`, `:tag`, `:metadata`)."
  [tool-result]
  (when (extension/tool-result? tool-result)
    (let [meta (or (:metadata tool-result) {})]
      (cond-> {}
        (:symbol tool-result)
        (assoc :op (:symbol tool-result))

        (:tag tool-result)
        (assoc :tag (:tag tool-result))

        :always
        (merge (select-keys meta [:spec :paths :hit-count :truncated-by :command :cwd :target]))))))

(defn- form-result-detail [chunk] (tool-result-detail (:result chunk)))

(defn- normalize-thinking-text
  [thinking]
  (some-> thinking
          str
          str/trim))

(defn- display-form-idx
  "Map original engine `form-idx` to the visible vector index after any
   prior silent system-call forms have been elided."
  [entry ^long idx]
  (if (integer? idx)
    (let [elided (or (:elided-form-idxs entry) #{})]
      (- idx (long (count (filter #(< (long %) idx) elided)))))
    idx))

(defn- silent-chunk?
  "Whether a chunk's whole form is hidden from the trace. Driven by the
   loop's structural `:silent?` flag (code-free blocks: answer / title
   recaps). A bare `vis_silent` RESULT does not hide a code-bearing form —
   it only suppresses the result echo (see `format-form-result`); the form's
   call still shows. Full chrome hiding is `structurally-silent-chunk?`."
  [chunk]
  (boolean (:silent? chunk)))

;; Engine chrome ("is this form silent UI?") is detected from the RESULT
;; sentinel, not a call-head name list: a form returning "vis_silent"
;; suppresses its result echo. Plus structurally code-free recap blocks.
;; That is the whole rule.

(defn- structurally-silent-chunk?
  "True for host-bookkeeping forms that should never appear in user traces:
   structurally code-free recap blocks, and forms whose RESULT is the
   `vis_silent` (title) sentinel. They may still execute and feed channel
   chrome, but the code/result row itself is noise in both TUI and CLI
   trace views."
  [chunk]
  (boolean (or (:vis/structurally-silent? chunk) (= "vis_silent" (:result chunk)))))

(defn- chunk->form-start
  "Build the initial `:forms` entry for a `:form-start` chunk. Only the
   code/comment/start timestamp are known; result-side fields stay nil.
   The form is running implicitly: `:started-at-ms` is set and
   `:success?` is nil."
  [chunk]
  (merge
    ;; Native-tool badge identity (`:vis/tool-name` + colour), projected from the
    ;; ONE canonical list so a running form can hide its invocation code the same
    ;; way the completed form does.
    (form/->display chunk)
    {:code (:code chunk)
     :comment (:comment chunk)
     :render-segments (:render-segments chunk)
     :scope (:scope chunk)
     :started-at-ms (:started-at-ms chunk)
     :silent? (silent-chunk? chunk)}))

(defn- chunk->form-result
  "Build the completed `:forms` entry for a `:form-result` chunk.
   Carries the raw `:error` and the pre-derived display projections
   (`:result-render`, `:result-kind`, `:result-detail`) the renderer
   reads verbatim.

   `:result-render` is nil when the form errored (the renderer paints
   `:error` inline with the failing source caret; the per-form result row
   is suppressed)."
  [prev-form chunk]
  (let [errored? (some? (:error chunk))]
    (merge
      ;; Native-tool op-card fields (pre-rendered card + badge label + colour),
      ;; projected from the ONE canonical list so this builder can't drift from
      ;; the gateway / restore builders (see internal/form.clj).
      (form/->display chunk)
      {:code (:code chunk)
       :comment (:comment chunk)
       :render-segments (:render-segments chunk)
       :scope (or (:scope chunk) (:scope prev-form))
       :started-at-ms (or (:started-at-ms chunk) (:started-at-ms prev-form))
       :duration-ms (or (envelope-duration-ms (:envelope chunk)) 0)
       ;; The SINGLE display surface the channels paint (render.clj / web both read
       ;; `(:result form)`): the pre-rendered markdown the loop built — a native
       ;; tool's custom card, a pretty-printed result, or python_execution's stdout.
       :result (:result chunk)
       ;; raw stdout kept for any model-context / resume consumer.
       :stdout (:stdout chunk)
       :result-kind (form-result-kind chunk)
       :result-detail (form-result-detail chunk)
       :error (:error chunk)
       :success? (not errored?)
       :silent? (and (not errored?) (silent-chunk? chunk))})))

(defn- assoc-form
  [entry ^long display-idx form]
  (let [need (inc display-idx)]
    (update entry :forms #(assoc (pad-forms-to % need) display-idx form))))

(defn- drop-form-at
  "Remove the form at display index `idx` from `:forms`. Indices shift
   down; the channel re-numbers in display order."
  [entry idx]
  (update entry
          :forms
          (fn [forms]
            (let
              [idx
               (long idx)

               c
               (long (count forms))]

              (if (and (vector? forms) (not (neg? idx)) (< idx c))
                (into (subvec forms 0 idx) (subvec forms (inc idx)))
                forms)))))

(defn- insert-empty-form-at
  "Insert a placeholder form at display index `idx`, padding if needed.
   Used by `unhide-form-slot` when a previously silent form must become
   visible without overwriting later visible slots."
  [entry idx]
  (update entry
          :forms
          (fn [forms]
            (let [idx (long idx)]
              (if (and (vector? forms) (not (neg? idx)))
                (let [padded (pad-forms-to forms idx)]
                  (into (conj (subvec padded 0 idx) {:position idx}) (subvec padded idx)))
                forms)))))

(defn- hide-form-slot
  "Remember original form index `idx` as elided and drop its current
   visible slot if present. Future chunks with higher original indices
   are shifted left by `display-form-idx`, avoiding nil holes in live
   progress when a silent system call appears before visible work."
  [entry idx]
  (if (contains? (or (:elided-form-idxs entry) #{}) idx)
    entry
    (let [display-idx (display-form-idx entry idx)]
      (-> entry
          (update :elided-form-idxs (fnil conj #{}) idx)
          (drop-form-at display-idx)))))

(defn- unhide-form-slot
  "Make original form index `idx` visible again. This happens when a block
   first succeeded silently, then was re-emitted with an error."
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

    :shell-run
    (assoc entry
      :activity :shell-run
      :shell/cmd (:cmd chunk))

    :shell-bg
    (assoc entry
      :activity :shell-bg
      :shell/cmd (:cmd chunk))

    :slash
    (assoc entry
      :activity :slash
      :slash/label (:slash chunk))

    :tool-start
    ;; A native tool (shell_run/shell_bg/cat/rg/…) began executing INSIDE a
    ;; python_execution block. `:form-start` cleared `:activity` to nil, so a
    ;; long-running nested tool (e.g. a multi-minute shell_run) would otherwise
    ;; leave the bubble frozen with no sign anything is happening. Surface it as
    ;; coarse `:tool-call` activity naming the op, so the spinner reads
    ;; "Vis is running: <op>" with a live wall-clock. Reset by the next
    ;; :form-start/:form-result (`:activity nil`).
    (assoc entry
      :activity :tool-call
      :tool/op (some-> (:op (:tool-event chunk))
                       name))

    :response-parse
    (if (= :done (:status chunk))
      (-> entry
          (dissoc :content-stream)
          (assoc :activity nil
                 :response-parse chunk))
      (assoc entry
        :activity :response-parse
        :response-parse chunk))

    :reasoning
    (let
      [next-thinking (or (normalize-thinking-text (:thinking chunk))
                         (normalize-thinking-text (:thinking entry)))]
      (assoc entry
        :thinking next-thinking
        :activity nil))

    :content
    ;; Provider content stream (answer Markdown) — kept on entry as
    ;; `:content-stream` so the live bubble can render it below the
    ;; reasoning text. Cleared by :response-parse :done and
    ;; :iteration-final once the parsed block takes over.
    (let [next-content (or (normalize-thinking-text (:content chunk)) (:content-stream entry))]
      (assoc entry
        :content-stream next-content
        :activity nil))

    :assistant-prose
    ;; Full end-of-iteration commentary (the "prose beyond the code"). The loop
    ;; emits this AFTER the parse (content-stream already cleared) and BEFORE the
    ;; forms run. Persist it on the entry as `:assistant-prose` so the renderer
    ;; paints it as its OWN block between the thinking trace and the code+result
    ;; — LIVE, not only on the DB-restored trace. Without this the prose vanished
    ;; the moment the code block landed, because `:content-stream` is dropped
    ;; once `:forms` exists (see render's `content-stream (when (empty? forms) …)`).
    (assoc entry
      :assistant-prose (or (some-> (:text chunk)
                                   str
                                   str/trim
                                   not-empty)
                           (:assistant-prose entry))
      :content-stream nil
      :activity nil)

    :provider-fallback
    (-> entry
        (assoc :activity :provider-call)
        (update :provider-fallbacks
                conj
                (or (:event chunk)
                    (select-keys chunk [:reason :failed-provider :new-provider :fallback]))))

    :provider-retry-reset
    ;; Provider stream died after emitting live text. Vis retries the whole
    ;; provider call before any code eval, so rewind the live attempt: drop
    ;; stale reasoning/content/parse state and keep only the retry recap.
    (-> entry
        (assoc :activity :provider-call)
        (dissoc :thinking :content-stream :response-parse)
        (update :provider-fallbacks
                conj
                (or (:event chunk)
                    (select-keys chunk [:reason :failed-provider :new-provider :fallback]))))

    :form-start
    (let
      [entry'
       (unhide-form-slot entry (:position chunk))

       display-idx
       (display-form-idx entry' (:position chunk))]

      (assoc (assoc-form entry' display-idx (chunk->form-start chunk)) :activity nil))

    :form-result
    (let [silent? (structurally-silent-chunk? chunk)]
      (if silent?
        (assoc (hide-form-slot entry (:position chunk)) :activity nil)
        (let
          [entry' (unhide-form-slot entry (:position chunk))
           display-idx (display-form-idx entry' (:position chunk))
           prev-form (get (:forms entry') display-idx)]

          (assoc (assoc-form entry' display-idx (chunk->form-result prev-form chunk))
            :activity nil))))

    :iteration-final
    (let
      [duplicate-final?
       (and (:done? entry) (:final entry) (:final chunk))

       base
       (assoc entry
         :thinking (or (normalize-thinking-text (:thinking chunk))
                       (normalize-thinking-text (:thinking entry)))
         :assistant-prose (or (some-> (:assistant-prose chunk)
                                      str
                                      str/trim
                                      not-empty)
                              (:assistant-prose entry))
         :activity nil
         :final (:final chunk)
         :done? (boolean (:done? chunk)))

       ;; Structurally-silent bookkeeping blocks (e.g. title updates)
       ;; are hidden as chunks arrive. Other successful `:vis/silent`
       ;; blocks stay in the timeline with `:silent? true`; channel
       ;; settings decide whether to render them.
       answer-idx
       (when-not duplicate-final? (when (:final chunk) (:answer-position chunk)))

       silent-idxs
       (if duplicate-final? #{} (or (:silent-form-idxs chunk) #{}))

       base
       (reduce (fn [e engine-idx]
                 (let
                   [display-idx
                    (display-form-idx e engine-idx)

                    c
                    (long (count (:forms e)))]

                   (if (and (integer? display-idx) (< (long display-idx) c))
                     (assoc-in e [:forms display-idx :silent?] true)
                     e)))
               base
               (sort silent-idxs))]

      (if (some? answer-idx) (hide-form-slot base answer-idx) base))

    :iteration-error
    (assoc entry
      :thinking (or (normalize-thinking-text (:thinking chunk))
                    (normalize-thinking-text (:thinking entry)))
      :activity nil
      :error (:error chunk)
      :done? true)

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
   (let
     [timeline
      (atom (sorted-map))

      ;; Canonicalize every entry as it leaves the tracker: layer the
      ;; shared block-level fields (`:scope` `:code` `:ops` `:status`
      ;; `:duration-ms` `:error` + 0-based `:position`) on top of the
      ;; raw per-iteration accumulation. Stored entries stay raw so the
      ;; chunk reducer keeps its transient bookkeeping; consumers
      ;; (renderer, parity test) see the SAME canonical shape the resume
      ;; path produces.
      canon
      (fn [iteration entry]
        (iteration/canonicalize
          (assoc entry :position (when (integer? iteration) (max 0 (dec (long iteration)))))))

      as-vec
      #(vec (map (fn [[it entry]]
                   (canon it entry))
                 %))]

     {:on-chunk (fn [chunk]
                  ;; Every streaming chunk carries its 1-based iteration
                  ;; POSITION under `:iteration` (the result map uses
                  ;; `:iteration-count` for the TOTAL — a different thing).
                  ;; Skip a chunk with no `:iteration` rather than route it
                  ;; into a `nil` bucket: that bucket sorts BEFORE every
                  ;; real iteration and shifts live numbering by +1.
                  (when-let [iteration (:iteration chunk)]
                    (let
                      [tl (swap! timeline update
                            iteration
                            (fn [entry]
                              (update-entry (or entry (empty-iteration-entry iteration)) chunk)))]
                      (when on-update (on-update (as-vec tl) chunk)))))
      :get-timeline #(as-vec @timeline)})))
