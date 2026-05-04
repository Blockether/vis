(ns com.blockether.vis.ext.channel-tui.screen
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.header :as header]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.selection :as selection]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.internal.external-opener :as opener]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna SGR TerminalPosition]
           [com.googlecode.lanterna.input KeyStroke KeyType
            MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
           [com.googlecode.lanterna.terminal MouseCaptureMode]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]
           [java.nio.charset Charset]
           [java.util.concurrent TimeUnit]
           [java.util.concurrent.locks ReentrantLock]))

;;; ── Threading model ─────────────────────────────────────────────────────────────
;;
;; The TUI now runs two threads against the Lanterna screen:
;;
;;   1. Input thread (the original main loop): polls keys, dispatches
;;      events into app-db, opens modal dialogs. Never draws the chat
;;      view itself — just mutates state.
;;
;;   2. Render thread: sleeps on `state/render-monitor`, wakes on
;;      every dispatched event, and only repaints if `:render-version`
;;      moved or the terminal got resized. While a dialog is up the
;;      input thread holds `draw-lock` for the dialog's whole session
;;      so the render thread cannot scribble underneath it.
;;
;; `screen-size`/`doResizeIfNecessary`, `setCharacter`, `refresh`, and
;; `setCursorPosition` are owned EXCLUSIVELY by the holder of
;; `draw-lock`. `pollInput` lives on its own input queue inside
;; Lanterna and is safe to call concurrently from the input thread.

(defonce ^:private ^ReentrantLock draw-lock
  ^{:doc "Single screen-mutation lock. Held by the render thread for the
          duration of one paint, and by `with-dialog-lock` for the
          duration of a modal dialog session."}
  (ReentrantLock.))

;; Input box auto-sizing: starts at one row when empty (the smallest
;; surface that still reads as an input field) and grows by one row
;; per soft-wrap line of typed content, capped at four rows. Beyond
;; the cap `draw-input-box!` keeps the cursor in view by scrolling
;; the visible window vertically over the underlying text. The cap
;; was 8 rows; four is the sweet spot — long enough to show a real
;; multi-paragraph prompt, short enough that the input never eats
;; more than ~25% of a 1080p terminal's chat area.
(def ^:private input-min-lines 1)

(def ^:private arrow-scroll-step
  "Lines moved per Up/Down/PageUp/PageDown press in the messages area.
   Mouse wheel events stay at 3 (the OS already emits multiple events
   per wheel notch); arrow keys are explicit and benefit from a
   bigger jump so long iteration traces clear in a few presses."
  5)
(def ^:private input-max-lines 4)

(def ^:private mouse-double-click-ms
  "Maximum time between two selection clicks on the same row for line-select."
  500)
;; Hint strip rules
;;
;; Idle, input EMPTY    → show newline + history cycle + menu. The
;;                       empty box is the moment someone is deciding
;;                       what to type, so we surface `↑↓` for
;;                       cycling through prior prompts and `Alt+Enter`
;;                       for multi-line composition right where the
;;                       eye lands.
;; Idle, input NON-EMPTY → just menu. \"Enter send\" is monkey-obvious; once
;;                        someone has typed the hint is redundant,
;;                        and history-cycle would clobber the buffer.
;; Loading              → cancel + quit. Same as before.
;; Cancelling            → progress message + quit.
;;
;; Removed from idle: `Enter send` (universally obvious). PageUp/PageDown
;; remain available for transcript scrolling. Model/reasoning/verbosity
;; shortcuts live in the footer next to the values they change.
(def ^:private hint-idle-empty " Alt+Enter newline · ↑↓ history · Ctrl+G conversations · Ctrl+K menu ")
(def ^:private hint-idle-typed " Ctrl+G conversations · Ctrl+K menu ")
(def ^:private hint-loading    " Esc cancel · Ctrl+C quit ")
(def ^:private hint-cancelling " Cancelling… please wait · Ctrl+C quit ")

(defn- input-empty?
  "True when the input editor has no text. The empty editor is `{:lines [\"\"]
   :crow 0 :ccol 0}` — a one-element vec with the empty string — so we
   can't just `(empty? lines)`."
  [{:keys [lines]}]
  (or (empty? lines)
    (every? str/blank? lines)))

(defn- current-hint [{:keys [loading? cancelling? input]}]
  (cond
    cancelling?         hint-cancelling
    loading?            hint-loading
    (input-empty? input) hint-idle-empty
    :else               hint-idle-typed))

(defn- submit-input!
  "Submit the current editor buffer, then clear it.

   Important: `:send-message` must run before `:reset-input`. Large
   paste placeholders expand from `app-db :pastes`, and `:reset-input`
   clears that registry. Resetting first sends the cosmetic
   `[Pasted #N: ...]` token to the provider instead of the payload."
  [db input-state]
  (let [text (input/input->text input-state)]
    (when (and (seq (str/trim text))
            (:conversation db)
            (not (:loading? db)))
      (state/dispatch [:send-message text]))
    (state/dispatch [:reset-input])))

(def ^:private copy-success-ttl-ms 1500)

(defn- copy-conversation-id! [text]
  (future
    (try (input/clipboard-copy! text)
      (catch Throwable _ nil)))
  (vis/notify! "✓ Copied conversation ID"
    :level :success :ttl-ms copy-success-ttl-ms))

(defn- copy-selection!
  ([text]
   (copy-selection! text :transcript))
  ([text source]
   (future
     (try (input/clipboard-copy! (or text ""))
       (catch Throwable _ nil)))
   (vis/notify! (if (= source :input)
                  "✓ Copied input selection"
                  "✓ Copied selection")
     :level :success :ttl-ms copy-success-ttl-ms)))

(defn- copy-bubble! [text]
  (future
    (try (input/clipboard-copy! (or text ""))
      (catch Throwable _ nil)))
  (vis/notify! "✓ Copied bubble"
    :level :success :ttl-ms copy-success-ttl-ms))

(defn- copy-conversation-as-markdown! [conversation-id]
  (future
    (try
      (if-not conversation-id
        (vis/notify! "No conversation to copy as Markdown"
          :level :warn :ttl-ms copy-success-ttl-ms)
        (let [env      (vis/env-for conversation-id)
              markdown (when env
                         (vis/conversation->markdown (:db-info env) conversation-id))]
          (if (seq markdown)
            (do (input/clipboard-copy! markdown)
              (vis/notify! "✓ Copied conversation as Markdown"
                :level :success :ttl-ms copy-success-ttl-ms))
            (vis/notify! "No persisted turns to copy as Markdown"
              :level :warn :ttl-ms copy-success-ttl-ms))))
      (catch Throwable t
        (vis/notify!
          (str "Markdown copy failed: " (or (.getMessage t) (.getName (class t))))
          :level :error :ttl-ms copy-success-ttl-ms)))))

(defn- capture-screen-cells
  "Read the current Lanterna back-buffer as per-cell strings.

   The snapshot is captured before selection highlighting is overlaid, so the
   copy payload is the visible text, not an artifact of the reverse-video pass."
  [^TerminalScreen screen cols rows]
  (vec
    (for [row (range rows)]
      (vec
        (for [col (range cols)]
          (let [tc (.getBackCharacter screen (int col) (int row))]
            (or (some-> tc .getCharacterString) " ")))))))

(defn- paint-selection!
  "Overlay reverse-video on the selected back-buffer cells."
  [^TerminalScreen screen selection cols rows selectable-ranges viewport]
  (let [screen-selection (selection/document->screen-selection selection viewport)]
    (doseq [{:keys [row col width]} (selection/selected-ranges screen-selection cols rows selectable-ranges)
            x (range col (+ col width))]
      (when-let [tc (.getBackCharacter screen (int x) (int row))]
        (.setCharacter screen (int x) (int row) (.withModifier tc SGR/REVERSE))))))

(def ^:private bubble-content-h-pad
  "Horizontal text inset inside `render/draw-chat-bubble!` content rows."
  2)

(def ^:private transcript-copy-skip-markers
  "Line markers that paint TUI chrome rather than message content.

   Mouse-selection copy operates on rendered screen cells, so clipping these
   rows before extraction keeps copied transcript text free of role banners,
   answer dividers, padding bands, iteration labels, and provider/model footers."
  #{p/MARKER_ITERATION_HDR
    p/MARKER_DURATION
    p/MARKER_STDOUT_SEP
    p/MARKER_STDOUT_PAD
    p/MARKER_SEP
    p/MARKER_ANSWER_SEP
    p/MARKER_ANSWER_HDR
    p/MARKER_ANSWER_PAD
    p/MARKER_CODE_PAD
    p/MARKER_CODE_OK_PAD
    p/MARKER_CODE_ERR_PAD
    p/MARKER_ITERATION_PAD})

(defn- copyable-transcript-line?
  [line]
  (let [line (or line "")]
    (not-any? #(str/starts-with? line %) transcript-copy-skip-markers)))

(defn- projected-content-lines
  [message content-w]
  (or (:prewrapped-lines message)
    (render/wrap-text (:text message) content-w)))

(defn- bubble-selectable-ranges
  "Return absolute screen-cell ranges for visible transcript message content.

   Selection deliberately excludes the header, input box, footer, scrollbar
   gutter, message-area margins, role/timestamp row, final inter-bubble gap,
   assistant provider/model footer, and structural separator/padding rows.
   Dragging across those cells may continue a gesture, but highlight/copy is
   clipped back to user/model-authored text rows."
  [layout text-top inner-h cols]
  (let [bubble-left  (long render/MESSAGE_MARGIN_LEFT)
        bubble-w     (long (max 0 (- (long cols) render/MESSAGE_SIDE_PAD)))
        text-left    (+ bubble-left bubble-content-h-pad)
        content-w    (long (max 0 (- bubble-w (* 2 bubble-content-h-pad))))
        top-limit    (long text-top)
        bottom-limit (+ top-limit (long (max 0 inner-h)))]
    (if (or (not (pos? content-w)) (<= bottom-limit top-limit))
      []
      (vec
        (for [{:keys [top projected]} (:visible layout)
              :let [message     (or projected {})
                    sep-pad     (if (:turn-separator? message) 2 0)
                    top-pad     (if (= :user (:role message)) 1 0)
                    content-top (+ top-limit (long top) sep-pad 1 top-pad)]
              [idx line] (map-indexed vector (projected-content-lines message content-w))
              :let [row (+ content-top (long idx))]
              :when (and (<= top-limit row)
                      (< row bottom-limit)
                      (copyable-transcript-line? line))]
          {:row row :col text-left :width content-w})))))

(defn- copyable-bubble-text [message]
  (let [text (or (:raw-answer message) (:text message) "")]
    (if (string? text) text (pr-str text))))

(defn- bubble-copy-regions
  "Return absolute screen-cell rectangles for single-click whole-bubble copy.

   Drag selection remains clipped to `bubble-selectable-ranges`; these wider
   regions only answer the release-time question: which message did this
   simple click land on? They cover the visible bubble rectangle except for
   the final inter-bubble gap row, so clicking the role row or bubble padding
   still copies the message while clicking between bubbles does nothing."
  [layout messages text-top inner-h cols]
  (let [bubble-left  (long render/MESSAGE_MARGIN_LEFT)
        bubble-w     (long (max 0 (- (long cols) render/MESSAGE_SIDE_PAD)))
        top-limit    (long text-top)
        bottom-limit (+ top-limit (long (max 0 inner-h)))]
    (if (or (not (pos? bubble-w)) (<= bottom-limit top-limit))
      []
      (vec
        (for [{:keys [idx top height projected]} (:visible layout)
              :let [message       (nth messages idx nil)
                    text          (copyable-bubble-text message)
                    sep-pad       (if (:turn-separator? projected) 2 0)
                    bubble-top    (+ top-limit (long top) sep-pad)
                    copy-height   (max 1 (- (long height) sep-pad 1))
                    copy-bottom   (min bottom-limit (+ bubble-top copy-height))
                    row           (max top-limit bubble-top)
                    clipped-height (- copy-bottom row)]
              :when (and (pos? clipped-height)
                      (not (str/blank? text)))]
          {:row row :col bubble-left :width bubble-w
           :height clipped-height :text text})))))

(defn- bubble-copy-hit
  [point regions]
  (let [col (long (:col point))
        row (long (:row point))]
    (some (fn [{r :row c :col w :width h :height :as region}]
            (when (and (>= row (long r))
                    (< row (+ (long r) (long h)))
                    (>= col (long c))
                    (< col (+ (long c) (long w))))
              region))
      regions)))

(defn- input-selectable-ranges
  "Return absolute screen-cell ranges for the visible input editor text rows.

   The ranges start at the same horizontal text inset used by
   `render/draw-input-box!`, so selection copies the user's draft text without
   the input box padding or border chrome."
  [input-top text-rows cols]
  (let [cols     (long (max 0 cols))
        text-w   (long (render/input-text-w cols))
        left     (long (max 0 (quot (- cols text-w) 2)))
        text-top (+ (long input-top) 1 (long render/input-pad-y))
        n        (long (max 0 text-rows))]
    (if (or (not (pos? text-w)) (not (pos? n)))
      []
      (vec
        (for [row (range text-top (+ text-top n))]
          {:row row :col left :width text-w})))))

(defn- selectable-ranges-for-source
  [source transcript-ranges input-ranges]
  (if (= source :input)
    input-ranges
    transcript-ranges))

(defn- with-dialog-lock
  "Mark a dialog open in app-db AND grab `draw-lock` for the dialog's
   whole session. Holding the lock blocks the render thread cleanly
   regardless of timing: even if the version bump from
   `:set-dialog-open true` races a render in flight, the dialog can't
   start drawing until the render thread releases the lock, and once
   we hold it, the render thread's next attempt blocks until we're
   done. After the dialog returns we release the lock and the
   `:set-dialog-open false` dispatch wakes the render thread to
   repaint over the dialog area."
  [f]
  (.lock draw-lock)
  (try
    (state/dispatch [:set-dialog-open true])
    (try
      (f)
      (finally
        (state/dispatch [:set-dialog-open false])))
    (finally
      (.unlock draw-lock))))

(defn- open-click-target!
  [{:keys [kind url]}]
  (future
    (try
      (if (= :file kind)
        (opener/open-file-in-editor! url)
        (opener/open! url))
      (catch Throwable _ nil))))

(defn- open-resources-popup!
  [^TerminalScreen screen refs]
  (when-let [ref (with-dialog-lock #(dlg/resources-dialog! screen refs))]
    (open-click-target! ref)))

(defn- screen-size
  "Lanterna size + lazy resize handling. MUST be called with `draw-lock`
   held (or before the render thread is started) because
   `doResizeIfNecessary` reallocates the back buffer."
  ^com.googlecode.lanterna.TerminalSize [^TerminalScreen screen]
  (if-let [new-size (.doResizeIfNecessary screen)]
    (do (try (.refresh screen Screen$RefreshType/COMPLETE)
          (catch NullPointerException _
            ;; Lanterna buffer may have null cells after resize before first
            ;; full render.  DELTA is safe because it only touches dirty cells.
            (try (.refresh screen Screen$RefreshType/DELTA)
              (catch Exception _ nil))))
      new-size)
    (.getTerminalSize screen)))

;; `apply-settings` was retired in favour of
;; `com.blockether.vis.ext.channel-tui.virtual/layout`, which
;; projects ONLY the messages whose viewport interval is non-empty
;; (cold-open of long conversations no longer pays
;; `format-answer-with-thinking` for every off-screen bubble before
;; the first frame). The `:show-timestamps` projection moved into
;; `virtual/project-message`; the loading-bubble swap moved into
;; `virtual/layout`'s pass-2 logic. See `virtual.clj` for the why.

(defn- input-text-rows
  "Compute visible text rows for the input box based on content,
   counting SOFT-WRAPPED visual rows so the box grows as a single
   logical line overflows the box width. Capped between
   `input-min-lines` and `input-max-lines`; beyond the cap the box
   stops growing and `draw-input-box!` scrolls vertically to keep
   the cursor visible."
  [{:keys [lines]} cols]
  (let [text-w (render/input-text-w cols)
        n     (render/input-visual-row-count lines text-w)]
    (min input-max-lines (max input-min-lines n))))

(defn- render-frame!
  "Draw one frame: background, messages area (bubbles), input box,
   and two footer rows.

   Returns the layout map `{:total-h, :inner-h, :cols, :rows}` so the
   render thread can publish it back into app-db for the input thread's
   scroll handlers. `apply-settings` runs ONCE here and feeds both the
   layout calculation and the actual draw — the old code path computed
   it twice per frame, which doubled cost on long traces."
  [^TerminalScreen screen cols rows
   {:keys [messages messages-scroll input progress loading? cancelling?
           turn-start-ms settings] :as db}
   now-ms]
  (let [now-ms       (long now-ms)
        g            (.newTextGraphics screen)
        text-rows    (input-text-rows input cols)
        input-box-h  (+ text-rows 2 (* 2 render/input-pad-y))
        ;; Reserve the bottom-most two rows for the dedicated footer
        ;; (model/status first, provider limits second) and the top-most
        ;; band for the dedicated header (top rule + title row + bottom
        ;; rule). The input box sits directly above the footer; the
        ;; messages area fills everything from `messages-top` down to the
        ;; input-box top.
        header-top   0
        footer-row   (- rows 2)
        input-top    (- rows input-box-h 2)
        messages-top    header/HEADER_ROWS
        messages-bottom input-top
        ;; Single source of truth for the gutter math lives in
        ;; `render.clj` (`MESSAGE_SIDE_PAD`). Reference it directly; do
        ;; NOT inline a literal here. Two layers disagreeing by even
        ;; one column makes `format-iteration-entry` size labels for
        ;; one bubble-w while `draw-chat-bubble!` paints into a
        ;; different bubble-w — right-aligned labels (`BLOCK 3`,
        ;; `✓ 3ms`, `FINAL ANSWER`) wrap onto two lines from the
        ;; mismatch. Use the const, never the value.
        bubble-w     (max 1 (- cols render/MESSAGE_SIDE_PAD))
        progress-extra {:now-ms         now-ms
                        :turn-start-ms turn-start-ms
                        :cancelling?    (boolean cancelling?)}
        inner-h      (max 0 (- messages-bottom messages-top 2)) ;; top + bottom margins
        ;; Single virtualized layout pass: cheap height estimate for
        ;; every message, full projection + real height ONLY for
        ;; messages whose viewport interval is non-empty. The
        ;; resulting `:total-h` feeds the scrollbar geometry +
        ;; gets published into app-db so input-thread scroll handlers
        ;; have an accurate ceiling.
        layout       (virtual/layout messages bubble-w settings
                       messages-scroll inner-h
                       {:progress       progress
                        :loading?       loading?
                        :progress-extra progress-extra}
                       {:conversation-id    (get-in db [:conversation :id])
                        :detail-expansions (:detail-expansions db)})
        total-h      (long (:total-h layout))
        text-top     (+ messages-top render/MESSAGE_MARGIN_TOP)
        transcript-selectable-ranges (bubble-selectable-ranges layout text-top inner-h cols)
        transcript-bubble-copy-regions (bubble-copy-regions layout messages text-top inner-h cols)
        input-selectable-ranges (input-selectable-ranges input-top text-rows cols)
        selectable-ranges (into transcript-selectable-ranges input-selectable-ranges)]
    (render/fill-background! g cols rows)
    ;; Messages area draws FIRST. It opens a new click-region staging
    ;; pass via `cr/begin-frame!` and registers every painted chrome
    ;; row (links, image markers, file links). The header then
    ;; registers its :copy-id region. The published click-region
    ;; registry is unchanged until `cr/commit-frame!` runs at the end
    ;; of this fn — so the input thread can `cr/lookup` at any time
    ;; during the paint and still get a complete previous frame back
    ;; instead of a half-filled buffer (the bug that made the header
    ;; copy-id button feel \"sometimes broken\" when the spinner was
    ;; ticking).
    (render/draw-messages-area! g layout messages-top messages-bottom cols)
    (header/draw-header! g db header-top cols)
    (let [[cx cy] (render/draw-input-box! g input input-top text-rows cols
                    (current-hint db))]
      (footer/draw-footer! g db footer-row cols now-ms)
      (.setCursorPosition screen (TerminalPosition. cx cy)))
    ;; Atomically publish every chrome region painted above. Until
    ;; this swap runs the input thread sees the PREVIOUS frame's
    ;; regions, which is the correct fallback — the previous frame
    ;; matches what's actually still on the user's screen up to this
    ;; instant.
    (let [screen-cells (capture-screen-cells screen cols rows)
          viewport     {:viewport-top text-top
                        :eff-scroll   (:eff-scroll layout)}]
      (when-let [sel (:mouse-selection db)]
        (paint-selection! screen sel cols rows
          (selectable-ranges-for-source
            (:source sel) transcript-selectable-ranges input-selectable-ranges)
          viewport))
      (cr/commit-frame!)
      (.refresh screen Screen$RefreshType/DELTA)
      {:cols cols :rows rows :total-h total-h :inner-h inner-h
       :messages-top messages-top
       :text-top text-top
       :eff-scroll (:eff-scroll layout)
       :screen-cells screen-cells
       :selectable-ranges selectable-ranges
       :transcript-selectable-ranges transcript-selectable-ranges
       :transcript-bubble-copy-regions transcript-bubble-copy-regions
       :input-selectable-ranges input-selectable-ranges})))

;;; ── Render thread ───────────────────────────────────────────────────────────────

(def ^:private spinner-tick-ms
  "How often the spinner advances while a turn is in flight. Drives
   both the wait-timeout cap and the animate? predicate, so a quiet
   render thread still repaints on the same cadence as the spinner."
  100)

(defn- render-loop!
  "The render thread's main loop. Sleeps on `state/render-monitor` and
   only paints when `:render-version` advances, the terminal gets
   resized, or — while loading — the spinner frame advances. Skips
   painting entirely while a dialog is up by failing to acquire
   `draw-lock`."
  [^TerminalScreen screen]
  (loop [last-v -1 last-cols -1 last-rows -1 last-frame-ms 0]
    (let [db @state/app-db]
      (when-not (:shutdown? db)
        (let [version (long (or (:render-version db) 0))
              ;; tryLock so a dialog session (which holds the lock for
              ;; seconds) doesn't pin us. Time out fast and re-poll.
              got-lock? (.tryLock draw-lock 50 TimeUnit/MILLISECONDS)
              [rendered? new-cols new-rows new-frame-ms]
              (if-not got-lock?
                [false last-cols last-rows last-frame-ms]
                (try
                  ;; Re-read AFTER acquiring the lock — dialog state
                  ;; could have flipped while we were waiting.
                  (let [db       @state/app-db
                        size     (screen-size screen)
                        cols     (.getColumns size)
                        rows     (.getRows size)
                        now-ms   (System/currentTimeMillis)
                        loading? (boolean (:loading? db))
                        animate? (and loading?
                                   (>= (- now-ms (long last-frame-ms))
                                     spinner-tick-ms))]
                    (if (and (not (:shutdown? db))
                          (not (:dialog-open? db))
                          (or (not= last-v version)
                            (not= last-cols cols)
                            (not= last-rows rows)
                            animate?))
                      (let [layout (render-frame! screen cols rows db now-ms)]
                        ;; Publish layout back to app-db without
                        ;; bumping the version (see no-render-bump-events).
                        (state/dispatch [:set-layout layout])
                        [true cols rows now-ms])
                      [false cols rows last-frame-ms]))
                  (catch Throwable t
                    ;; Drawing must never crash the thread — a stray
                    ;; resize race or null cell will recover next frame.
                    (try (require 'taoensso.telemere)
                      ((resolve 'taoensso.telemere/log!)
                       :warn (str "render frame failed: " (or (ex-message t) (str t))))
                      (catch Throwable _ nil))
                    [false last-cols last-rows last-frame-ms])
                  (finally (.unlock draw-lock))))]
          (when-not rendered?
            ;; Park until the next dispatch wakes us, or until the
            ;; spinner needs to tick. Idle conversations sleep up to
            ;; ~250ms (defensive cap on lost wakeups); active queries
            ;; sleep no longer than the spinner tick so the animation
            ;; stays smooth without spamming repaints.
            (locking state/render-monitor
              (let [v-now (long (or (:render-version @state/app-db) 0))
                    loading? (boolean (:loading? @state/app-db))]
                (when (= v-now version)
                  (try (.wait ^Object state/render-monitor
                         (long (if loading? spinner-tick-ms 250)))
                    (catch InterruptedException _ nil))))))
          (recur (if rendered? version last-v)
            (long (or new-cols last-cols))
            (long (or new-rows last-rows))
            (long new-frame-ms)))))))

(defn- start-render-thread!
  "Spawn the render thread. Daemon so the JVM can still exit even if a
   bug ever traps it in the loop."
  ^Thread [^TerminalScreen screen]
  (let [t (Thread.
            ^Runnable (fn [] (render-loop! screen))
            "vis-channel-tui-render")]
    (.setDaemon t true)
    (.start t)
    t))

(def ^:private provider-limits-refresh-ms
  60000)

(defn- active-provider-id
  []
  (when-let [router (try (vis/get-router) (catch Throwable _ nil))]
    (some-> (try (vis/resolve-effective-model router) (catch Throwable _ nil))
      :provider)))

(defn- start-provider-limits-thread!
  "Refresh provider limit metadata outside the render thread."
  ^Thread []
  (let [t (Thread.
            ^Runnable
            (fn []
              (loop [last-provider-id nil last-refresh-ms 0]
                (when-not (:shutdown? @state/app-db)
                  (let [now-ms      (System/currentTimeMillis)
                        provider-id (active-provider-id)
                        changed?    (not= provider-id last-provider-id)
                        stale?      (>= (- now-ms (long last-refresh-ms)) provider-limits-refresh-ms)]
                    (try
                      (cond
                        (nil? provider-id)
                        (when last-provider-id
                          (state/dispatch [:clear-provider-limits]))

                        (or changed? stale?)
                        (state/dispatch [:set-provider-limits provider-id (vis/provider-limits provider-id)]))
                      (catch Throwable t
                        (tel/log! {:level :warn :id ::provider-limits-refresh-failed
                                   :data  {:provider provider-id
                                           :error    (or (ex-message t) (str t))}
                                   :msg   "Provider limits refresh failed"})))
                    (try (Thread/sleep 1000) (catch InterruptedException _ nil))
                    (recur provider-id (if (or changed? stale?) now-ms last-refresh-ms))))))
            "vis-channel-tui-provider-limits")]
    (.setDaemon t true)
    (.start t)
    t))

(defn- format-conversation-not-found
  "Build a friendly multi-line message for `--conversation-id` misses,
   listing the most recent :tui conversations so the user has
   something to copy-paste."
  [cid]
  (let [available (try (vec (take 10 (vis/by-channel :tui)))
                    (catch Throwable _ []))
        line (fn [c]
               (let [id-str (str (:id c))
                     id8    (if (>= (count id-str) 8) (subs id-str 0 8) id-str)
                     title  (let [t (:title c)] (when-not (str/blank? t) t))]
                 (str "  " id8 "  " (or title "(untitled)"))))]
    (str "Conversation not found: " cid
      (if (seq available)
        (str "\n\nAvailable :tui conversations (most recent first):\n"
          (str/join "\n" (map line available))
          "\n\nUse the 8-char prefix or full UUID with --conversation-id.")
        "\n\nNo :tui conversations exist yet — run `vis channels tui` without --conversation-id first."))))

(defn- current-conversation-id
  []
  (some-> @state/app-db :conversation :id str))

(defn- register-conversation-shutdown-hook!
  "Register a JVM shutdown hook that prints the TUI resume command for
   the active conversation. TUI-local: the printed string is a
   `vis channels tui …` sub-command, so this hook lives next to the
   only consumer instead of in vis-runtime or vis-cli."
  []
  (let [hook (Thread. (fn []
                        (when-let [conversation-id (current-conversation-id)]
                          (let [^java.io.PrintStream out vis/original-stdout]
                            (.println out "")
                            (.println out (str "  vis channels tui --conversation-id " conversation-id))
                            (.println out "")
                            (.flush out)))))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))

(defn- register-shutdown-hook!
  "Thin wrapper over `Runtime/addShutdownHook` so call-sites read as
   plain Clojure instead of a `(Thread. ^Runnable (fn [] ...))` casting
   ritual. `f` is a zero-arg fn; thrown exceptions are swallowed (the
   hook chain MUST NOT propagate — a single noisy listener can hang
   the whole shutdown sequence). Returns the registered Thread so
   tests can deregister it via `.removeShutdownHook` if needed."
  [^Runnable f]
  (let [hook (Thread. ^Runnable
               (fn [] (try (f) (catch Throwable _ nil))))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))

(defn- subscribe-title-listener!
  "Wire `(conversation-title \"...\")` calls inside this conversation's iteration
   loop to the TUI header: every change dispatches `[:set-title]`
   into app-db so the next render frame paints the new title without
   polling. Returns a zero-arg cleanup fn.

   The listener is scoped to the currently-active conversation. That
   matters once the TUI can switch tabs: a stale background listener must
   never overwrite the title of the tab the user is looking at now."
  [conversation-id]
  (let [conversation-id (str conversation-id)
        listener (vis/add-title-listener! conversation-id
                   (fn [new-title]
                     (when (= conversation-id (current-conversation-id))
                       (state/dispatch [:set-title (or new-title "")]))))
        cleanup  #(vis/remove-title-listener! conversation-id listener)]
    (register-shutdown-hook! cleanup)
    cleanup))

(defn- date->millis
  [v]
  (cond
    (instance? java.util.Date v) (.getTime ^java.util.Date v)
    (instance? java.time.Instant v) (.toEpochMilli ^java.time.Instant v)
    (number? v) (long v)
    :else nil))

(defn- latest-turn-created-at
  [turns]
  (->> turns
    (keep :created-at)
    (sort-by #(or (date->millis %) 0))
    last))

(defn- conversation-summary
  [db-info conversation]
  (let [turns       (try (vec (vis/db-list-conversation-turns db-info (:id conversation)))
                      (catch Throwable _ []))
        modified-at (or (latest-turn-created-at turns) (:created-at conversation))]
    (assoc conversation
      :turn-count (count turns)
      :modified-at modified-at)))

(defn- latest-modified-first
  [conversations]
  (sort-by #(or (date->millis (:modified-at %)) 0) > conversations))

(defn- tui-conversation-summaries
  []
  (try
    (let [db-info (vis/db-info)]
      (->> (vis/by-channel :tui)
        (map #(conversation-summary db-info %))
        latest-modified-first
        vec))
    (catch Throwable _ [])))

(defn- conversation-db-title
  [conversation-id]
  (when-let [conversation (try (vis/by-id conversation-id) (catch Throwable _ nil))]
    (let [title (:title conversation)]
      (when-not (str/blank? (str title))
        (str title)))))

(defn- init-visible-conversation!
  "Install a conversation into app-db and repaint the tab strip. Returns the
   cleanup fn for that conversation's title listener."
  [{:keys [id history]}]
  (state/dispatch [:init-conversation {:id id} history])
  (when-let [title (conversation-db-title id)]
    (state/dispatch [:set-title title]))
  (subscribe-title-listener! id))

(defn- standalone?
  [opts]
  (true? (:standalone opts)))

(defn- create-terminal!
  [opts]
  (if (standalone? opts)
    ((requiring-resolve 'com.blockether.vis.ext.channel-tui.standalone/create-terminal!) opts)
    (UnixTerminal. @vis/tty-in @vis/tty-out (Charset/defaultCharset))))

(defn- configure-terminal-input!
  [terminal opts]
  (when (instance? UnixTerminal terminal)
    (input/register-custom-patterns! terminal))
  (when-not (standalone? opts)
    (try (.setMouseCaptureMode terminal MouseCaptureMode/CLICK_RELEASE_DRAG_MOVE)
      (catch Throwable _ nil))))

(defn- enable-terminal-escape-modes!
  [opts]
  (when-not (standalone? opts)
    (input/enable-bracketed-paste! @vis/tty-out)
    (input/enable-sgr-mouse! @vis/tty-out)))

(defn- disable-terminal-escape-modes!
  [opts]
  (when-not (standalone? opts)
    (try (input/disable-bracketed-paste! @vis/tty-out) (catch Throwable _ nil))
    (try (input/disable-sgr-mouse! @vis/tty-out) (catch Throwable _ nil))))

(defn run-chat!
  "Start the fullscreen chat TUI. Blocks until user quits.
   Optional `opts` map:
     :conversation-id uuid-string — resume a specific conversation
     :resume          true        — resume the latest :tui conversation"
  ([] (run-chat! {}))
  ([opts]
  ;; Validate --conversation-id BEFORE we boot Lanterna. A miss here
  ;; used to crash mid-screen-startup with a stack trace; now it
  ;; surfaces as a `:vis/user-error` and `channel-main` prints a
  ;; clean, actionable message + exit code 2 (no trace, no torn-down
  ;; terminal state).
   (let [resumed-from-flag (when-let [cid (:conversation-id opts)]
                             (or (chat/resume-conversation cid)
                               (throw (ex-info (format-conversation-not-found cid)
                                        {:vis/user-error true
                                         :id             cid}))))]
     (state/init!)

  ;; Subscribe to host notifications so any (vis/notify! …) push
  ;; — from anywhere: this channel's click handler, an extension,
  ;; the iteration loop — wakes the render thread immediately. The
  ;; header band reads `(vis/notifications)` on every paint, so we
  ;; only need a render bump here; no app-db copy.
     (vis/watch-notifications! :tui-screen
       (fn [_snapshot]
         (state/dispatch [:bump-render-version])))

  ;; Load persisted config
     (when-let [c (vis/load-config)]
       (state/dispatch [:set-config c]))

     (let [terminal (create-terminal! opts)
           _        (configure-terminal-input! terminal opts)
           screen   (TerminalScreen. terminal)
        ;; Render thread handle is held in a volatile so the `finally`
        ;; clause can join it. (Locals from the `try` body aren't in
        ;; scope inside `finally`.)
           render-thread (volatile! nil)
           title-listener-cleanup (volatile! nil)
           ;; Daemon thread that pre-formats every assistant bubble
           ;; in the background so the FIRST scroll-up doesn't pay
           ;; ~500 ms / big-trace-bubble on the render thread.
           ;; Stays nil for fresh conversations (nothing to warm).
           prewarm-thread (volatile! nil)
           provider-limits-thread (volatile! nil)]
       (.startScreen screen)
       (try
      ;; Show provider dialog on first launch if no config
         (when-not (:config @state/app-db)
           (when (not (:dialog-open? @state/app-db))
             (when-let [c (with-dialog-lock #(provider/show-provider-dialog! screen (:config @state/app-db)))]
               (state/dispatch [:set-config c]))))

      ;; Sweep orphaned running turns from previous crashes so the
      ;; rebuilt history shows resolved rows only — no raw turn text
      ;; from a half-completed turn.
         (try (vis/db-sweep-orphaned-running-turns!) (catch Throwable _ nil))

      ;; Init conversation: resume if --conversation-id given, else fresh.
      ;; The --conversation-id case was already validated above (before
      ;; Lanterna started), so here we only need the pre-resolved value.
         (when-let [config (:config @state/app-db)]
           (let [{:keys [id history]}
                 (if (:conversation-id opts)
                   resumed-from-flag
                   (if (:resume opts)
                  ;; --resume: pick up the latest :tui conversation
                     (if-let [latest (first (vis/by-channel :tui))]
                       (or (chat/resume-conversation (:id latest))
                         (chat/make-conversation config))
                       (chat/make-conversation config))
                     (chat/make-conversation config)))]
             (vreset! title-listener-cleanup (init-visible-conversation! {:id id :history history}))
             (register-conversation-shutdown-hook!)
             ;; Kick off background pre-warm of the LRU. Walks the
             ;; history bottom-up calling project + bubble-height,
             ;; so by the time the user scrolls UP the cache is
             ;; already hot. Empty conversations skip this entirely.
             ;; Cancelled in the shutdown hook below.
             (when (seq history)
               (let [size     (screen-size screen)
                     cols     (.getColumns size)
                     bubble-w (max 1 (- cols render/MESSAGE_SIDE_PAD))
                     settings (or (:settings @state/app-db) {})]
                 (vreset! prewarm-thread
                   (virtual/pre-warm! history bubble-w settings
                     {:conversation-id    id
                      :detail-expansions (:detail-expansions @state/app-db)}))))))

      ;; Spawn the render thread BEFORE the input loop. It will paint
      ;; the first frame as soon as `:render-version` is non-zero (every
      ;; init dispatch above bumps it).
         (vreset! render-thread (start-render-thread! screen))
         (vreset! provider-limits-thread (start-provider-limits-thread!))
         ;; Local UI state that lives only in the input thread.
         ;;
         ;; `scrollbar-drag-offset` is `nil` when no drag is in
         ;; progress; otherwise it carries the integer row offset
         ;; between the click row and the TOP of the thumb captured
         ;; at CLICK_DOWN time (i.e. "how many rows from the top of
         ;; the thumb did the user grab?").
         ;;
         ;; Drag math then becomes `new-thumb-top = my - offset`,
         ;; which keeps the grip-point fixed under the cursor for
         ;; the entire drag — same contract every GUI scroll thumb
         ;; honours. A simple boolean would force the thumb to snap
         ;; its TOP to the cursor on the first DRAG event, which is
         ;; what made the previous implementation "jump" the moment
         ;; the user started moving.
         ;;
         ;; Clicks anywhere outside the thumb itself — the messages
         ;; area, the track above/below the thumb, the right gutter
         ;; columns — are deliberately ignored (no jump-to-position).
         ;; Wheel scroll, keyboard PageUp/PageDown and arrows remain
         ;; the supported ways to move the viewport without grabbing
         ;; the thumb.
         ;; Bracketed-paste mode is opt-in per terminal. Send the
         ;; `ESC[?2004h` enable sequence right after the screen is
         ;; up so xterm-class terminals (Apple Terminal, iTerm,
         ;; Alacritty, kitty, gnome-terminal, mintty, vscode) wrap
         ;; subsequent pastes in `ESC[200~ … ESC[201~`. Disabling
         ;; happens in the outer `finally` block, so a crashed TUI
         ;; can't leave the user's shell stuck with bracketing on.
         (enable-terminal-escape-modes! opts)
         ;; SGR mouse mode (1006). Lanterna's `setMouseCaptureMode`
         ;; above already enabled legacy 1003 on the native terminal
         ;; backend, but its parser only understands the X10 binary
         ;; encoding — which corrupts coordinates the moment
         ;; `col + 32` exceeds 0x7F (i.e. col >= 96), because the JVM
         ;; UTF-8 decoder replaces the high byte with U+FFFD. SGR sends
         ;; the same payload as pure ASCII text, so wide terminals (the
         ;; copy-id glyph lives near the right edge!) survive intact.
         ;; The custom pattern registered above turns SGR sequences into
         ;; `MouseAction` instances with correct integer mx/my. The
         ;; standalone Swing backend receives mouse input from Swing and
         ;; intentionally skips terminal escape-mode writes.
         (let [scrollbar-drag-offset (volatile! nil)
               ;; `click-action-fired?` is set to true when the
               ;; CLICK_DOWN branch already handled a click region
               ;; (copy / link / image). The CLICK_RELEASE branch
               ;; reads it to decide whether to fire the fallback
               ;; release-only path — needed for terminals that
               ;; deliver clicks as a single CLICK_RELEASE event
               ;; (X10-style mouse mode, some SSH-tunnelled
               ;; setups). Without this guard a normal
               ;; DOWN+RELEASE pair would double-fire (open the
               ;; link twice, copy twice).
               click-action-fired?   (volatile! false)
               ;; App-side drag selection. Native terminal selection is
               ;; unavailable while mouse reporting is enabled, so Vis tracks
               ;; drag coordinates, highlights the range during render, then
               ;; copies the visible text when the button is released.
               mouse-selection-anchor (volatile! nil)
               mouse-selection-focus  (volatile! nil)
               mouse-selection-source (volatile! nil)
               last-selection-click   (volatile! nil)
               ;; `paste-buffer` accumulates every keystroke received
               ;; between `paste-start?` and `paste-end?`. We treat
               ;; the whole block as one paste — newlines included —
               ;; so a multi-line clipboard payload doesn't fire
               ;; `KeyType/Enter` -> send mid-paste. The buffer is
               ;; kept in a StringBuilder so accumulation stays
               ;; allocation-cheap even for kilobyte pastes.
               paste-buffer          (volatile! nil)
               prewarm-conversation! (fn [{:keys [id history]}]
                                       (virtual/stop-pre-warm! @prewarm-thread)
                                       (vreset! prewarm-thread nil)
                                       (when (seq history)
                                         (let [size     (screen-size screen)
                                               cols     (.getColumns size)
                                               bubble-w (max 1 (- cols render/MESSAGE_SIDE_PAD))
                                               settings (or (:settings @state/app-db) {})]
                                           (vreset! prewarm-thread
                                             (virtual/pre-warm! history bubble-w settings
                                               {:conversation-id    id
                                                :detail-expansions (:detail-expansions @state/app-db)})))))
               install-conversation! (fn [{:keys [id] :as conversation-result} notify?]
                                       (when (and id conversation-result)
                                         (when-let [cleanup @title-listener-cleanup]
                                           (try (cleanup) (catch Throwable _ nil)))
                                         (vreset! title-listener-cleanup nil)
                                         (when-let [old-conversation (:conversation @state/app-db)]
                                           (when-not (= (str (:id old-conversation)) (str id))
                                             (chat/dispose! old-conversation)))
                                         (render/invalidate-cache!)
                                         (virtual/invalidate-heights!)
                                         (vreset! title-listener-cleanup
                                           (init-visible-conversation! conversation-result))
                                         (prewarm-conversation! conversation-result)
                                         (when notify?
                                           (vis/notify! "Switched conversation"
                                             :level :success :ttl-ms copy-success-ttl-ms))))
               switch-conversation!  (fn [choice]
                                       (cond
                                         (:loading? @state/app-db)
                                         (vis/notify! "Finish or cancel the running turn before switching conversations"
                                           :level :warn :ttl-ms copy-success-ttl-ms)

                                         (= :new (:action choice))
                                         (when-let [config (:config @state/app-db)]
                                           (install-conversation! (chat/make-conversation config) true))

                                         (= :fork (:action choice))
                                         (if-let [current-id (current-conversation-id)]
                                           (let [fork-state-id (try (vis/db-fork-conversation! (vis/db-info) current-id {})
                                                                 (catch Throwable _ nil))]
                                             (if fork-state-id
                                               (do
                                                 (when-let [old-conversation (:conversation @state/app-db)]
                                                   (chat/dispose! old-conversation))
                                                 (if-let [conversation-result (chat/resume-conversation current-id)]
                                                   (do
                                                     (install-conversation! conversation-result false)
                                                     (vis/notify! "Forked current conversation"
                                                       :level :success :ttl-ms copy-success-ttl-ms))
                                                   (vis/notify! "Forked, but failed to reload conversation"
                                                     :level :warn :ttl-ms copy-success-ttl-ms)))
                                               (vis/notify! "Could not fork current conversation"
                                                 :level :warn :ttl-ms copy-success-ttl-ms)))
                                           (vis/notify! "No current conversation to fork"
                                             :level :warn :ttl-ms copy-success-ttl-ms))

                                         (= :switch (:action choice))
                                         (let [target-id (:id choice)]
                                           (when-not (= (str target-id) (current-conversation-id))
                                             (if-let [conversation-result (chat/resume-conversation target-id)]
                                               (install-conversation! conversation-result true)
                                               (vis/notify! "Conversation no longer exists"
                                                 :level :warn :ttl-ms copy-success-ttl-ms))))))
               show-conversations!   (fn []
                                       (when-not (:dialog-open? @state/app-db)
                                         (let [conversations (tui-conversation-summaries)]
                                           (when-let [choice (with-dialog-lock
                                                               #(dlg/conversation-picker-dialog! screen conversations
                                                                  (current-conversation-id)))]
                                             (switch-conversation! choice)))))]
           (loop []
           ;; Layout fields are populated by the render thread after
           ;; the first paint. Until then, scroll handlers fall back
           ;; to safe defaults and act as a no-op.
           ;; Pure poll — no rendering on this thread anymore. The
           ;; render thread handles all screen output.
             (let [db      @state/app-db
                   {:keys [cols total-h inner-h messages-top]} (:layout db)
                   cols    (or cols 0)
                   total-h (or total-h 0)
                   inner-h (or inner-h 0)
                   messages-top (or messages-top 0)
                   key     (.pollInput screen)]
               (cond
                 (nil? key) (do (Thread/sleep 16) (recur))

               ;; ── Bracketed paste ───────────────────────────────────────────────────
               ;; Three-state machine sitting BEFORE the regular
               ;; key dispatch:
               ;;
               ;;   START arrives  -> open a new StringBuilder,
               ;;                     swallow the key.
               ;;   any key while open -> append its char into the
               ;;                     buffer, swallow.
               ;;   END arrives    -> flush the buffered text into
               ;;                     the input via `paste-text`,
               ;;                     close the buffer.
               ;;
               ;; Mouse events are excluded from the paste
               ;; state machine below — they take a separate cond
               ;; branch that fires BEFORE this one (see the
               ;; `(instance? MouseAction key)` clause). A stuck
               ;; paste buffer therefore can't silently swallow
               ;; clicks on the header copy affordance or the
               ;; scrollbar.
                 (input/paste-start? key)
                 (do (vreset! paste-buffer (StringBuilder.))
                   (recur))

                 (input/paste-end? key)
                 (let [^StringBuilder sb @paste-buffer]
                   (when sb
                     (let [text (.toString sb)]
                       (vreset! paste-buffer nil)
                       (when-not (.isEmpty text)
                         (if (input/use-placeholder? text)
                           ;; Pi-style: stash the payload, insert a
                           ;; one-line `[Pasted #N: …]` placeholder.
                           ;; The send path expands every active
                           ;; placeholder back into its content via
                           ;; `expand-paste-placeholders`. Reading
                           ;; the new id back out of the atom right
                           ;; after the dispatch is safe: every db
                           ;; event handler runs on the dispatching
                           ;; thread, so the swap is already visible.
                           (do (state/dispatch [:add-paste text])
                             (let [{:keys [paste-counter pastes]} @state/app-db
                                   entry  (get pastes paste-counter)
                                   token  (input/format-paste-placeholder entry)
                                   db'    @state/app-db]
                               (state/dispatch
                                 [:update-input
                                  (input/paste-text (:input db') token)])))
                           ;; Short single-line paste: inline,
                           ;; matches the natural feel of
                           ;; `git rev-parse HEAD`-style copies.
                           (state/dispatch
                             [:update-input (input/paste-text (:input db) text)])))))
                   (recur))

               ;; Mouse events: scrollbar grab/drag + wheel scroll.
               ;; Bypass `input/handle-key` entirely — those events
               ;; need access to the layout published by the render
               ;; thread, which `handle-key` doesn't see. Placed
               ;; BEFORE the paste-buffer clause so a stuck paste
               ;; bracket can't silently swallow mouse events (mouse
               ;; and paste are physically disjoint channels —
               ;; nothing in this branch can mutate paste state).
                 (instance? MouseAction key)
                 (let [^MouseAction ma key
                       atype     (.getActionType ma)
                       pos       (.getPosition ma)
                       mx        (.getColumn pos)
                       my        (.getRow pos)
                       _         (when-not (or (= atype MouseActionType/MOVE)
                                             (= atype MouseActionType/DRAG))
                                   ;; MOVE/DRAG fire dozens of times
                                   ;; per second — logging them would
                                   ;; flood the file. Every other
                                   ;; mouse event (CLICK_DOWN,
                                   ;; CLICK_RELEASE, SCROLL_UP/DOWN)
                                   ;; is rare and worth recording so
                                   ;; \"my click does nothing\" reports
                                   ;; can be diagnosed against the
                                   ;; log: `tail ~/.vis/vis.log` shows
                                   ;; the cursor coords + hit-test
                                   ;; result for every received event.
                                   (try
                                     (let [hit-kind (some-> (cr/lookup mx my) :kind)]
                                       (tel/log!
                                         {:level :info
                                          :id    ::mouse-event
                                          :data  {:type   (str atype)
                                                  :mx     mx
                                                  :my     my
                                                  :cols   cols
                                                  :hit    hit-kind}
                                          :msg   (str "tui mouse " atype
                                                   " at (" mx "," my ")"
                                                   " cols=" cols
                                                   " hit=" hit-kind)}))
                                     (catch Throwable _ nil)))
                       bar-top   (+ messages-top render/MESSAGE_MARGIN_TOP)
                       ;; Single source of truth for thumb geometry
                       ;; lives in `render/scrollbar-thumb-geometry`,
                       ;; so painter and hit-test cannot drift apart.
                       ;; A nil return means there's no overflow — no
                       ;; thumb is painted, and every click below is
                       ;; correctly classified as off-thumb.
                       geom      (render/scrollbar-thumb-geometry
                                   total-h inner-h (:messages-scroll db))
                       thumb-top (when geom
                                   (+ bar-top (long (:thumb-top-rel geom))))
                       thumb-h   (long (or (:thumb-h geom) 0))
                       ;; Hit-zone: the thumb's actual rows, with a
                       ;; 3-column-wide x-band on the right gutter so
                       ;; the user doesn't need pixel-perfect aim.
                       on-thumb? (and (some? geom)
                                   (>= mx (- cols render/MESSAGE_MARGIN_RIGHT))
                                   (< mx cols)
                                   (>= my (long thumb-top))
                                   (< my (+ (long thumb-top) thumb-h)))
                       selection-copy? (true? (get-in db [:settings :mouse-selection-copy]))
                       transcript-selectable-ranges (get-in db [:layout :transcript-selectable-ranges])
                       transcript-bubble-copy-regions (get-in db [:layout :transcript-bubble-copy-regions])
                       input-selectable-ranges (get-in db [:layout :input-selectable-ranges])
                       selection-viewport {:viewport-top bar-top
                                           :eff-scroll   (get-in db [:layout :eff-scroll])}]
                   (cond
                     (= atype MouseActionType/SCROLL_UP)
                     (do (state/dispatch [:scroll-up 3 total-h inner-h])
                       (recur))

                     (= atype MouseActionType/SCROLL_DOWN)
                     (do (state/dispatch [:scroll-down 3 total-h inner-h])
                       (recur))

                     ;; CLICK_DOWN on the thumb itself: arm a drag.
                     ;; Record the offset between the click row and
                     ;; the thumb's top so subsequent DRAG events can
                     ;; preserve the grip-point. Crucially we do NOT
                     ;; dispatch any scroll mutation here — a bare
                     ;; click without movement must not move content.
                     (and (= atype MouseActionType/CLICK_DOWN) on-thumb?)
                     (do (vreset! scrollbar-drag-offset (- my thumb-top))
                       (recur))

                     ;; CLICK_DOWN on the scrollbar TRACK (right
                     ;; gutter, anywhere in the messages-area rows,
                     ;; off-thumb): jump the thumb so it CENTERS on
                     ;; the cursor row, then arm a drag with the same
                     ;; centered grip offset so a follow-up motion
                     ;; tracks naturally. This is the modern macOS
                     ;; \"jump to spot\" behaviour — a click anywhere
                     ;; on the scrollbar moves you there, instead of
                     ;; the legacy paged \"click-above-thumb = page-up\"
                     ;; convention. The previous build silently
                     ;; ignored every off-thumb click in the gutter,
                     ;; which felt broken (\"I'm clicking on the
                     ;; scrollbar and nothing scrolls\").
                     (and (= atype MouseActionType/CLICK_DOWN)
                       (some? geom)
                       (>= mx (- cols render/MESSAGE_MARGIN_RIGHT))
                       (< mx cols)
                       (>= my bar-top)
                       (< my (+ bar-top inner-h)))
                     (let [grip (long (quot thumb-h 2))]
                       (vreset! scrollbar-drag-offset grip)
                       (state/dispatch
                         [:scroll-to-y
                          (- my grip)
                          bar-top inner-h total-h inner-h])
                       (recur))

                     ;; Drag continues to track the cursor's Y as
                     ;; long as the user is holding the button after
                     ;; a thumb grab. We feed `(my - drag-offset)` to
                     ;; `:scroll-to-y` so the row under the user's
                     ;; finger stays glued to the same point on the
                     ;; thumb — no jump, no snap. X is intentionally
                     ;; ignored once dragging starts so the thumb
                     ;; doesn't pop loose if the cursor strays out
                     ;; of the right gutter.
                     (and (= atype MouseActionType/DRAG)
                       (some? @scrollbar-drag-offset))
                     (do (state/dispatch
                           [:scroll-to-y
                            (- my (long @scrollbar-drag-offset))
                            bar-top inner-h total-h inner-h])
                       (recur))

                     (and selection-copy?
                       (= atype MouseActionType/DRAG)
                       (some? @mouse-selection-anchor))
                     (let [screen-focus (selection/point mx my)
                           doc-focus    (selection/screen->document-point
                                          screen-focus selection-viewport)
                           source       @mouse-selection-source]
                       (vreset! mouse-selection-focus doc-focus)
                       (state/dispatch
                         [:set-mouse-selection
                          {:anchor @mouse-selection-anchor
                           :focus  doc-focus
                           :source source}])
                       (when-not (= source :input)
                         (when-let [{:keys [direction amount]}
                                    (selection/auto-scroll-step
                                      screen-focus {:top bar-top
                                                    :bottom (+ bar-top inner-h)
                                                    :edge-size 6
                                                    :max-step 6})]
                           (case direction
                             :up   (state/dispatch [:scroll-up amount total-h inner-h])
                             :down (state/dispatch [:scroll-down amount total-h inner-h])
                             nil)))
                       (recur))

                     ;; CLICK_RELEASE — ends a drag, and serves as
                     ;; a FALLBACK click trigger for terminals that
                     ;; deliver clicks as a single CLICK_RELEASE
                     ;; (X10 mouse mode, some SSH-tunnelled
                     ;; sessions) instead of the standard
                     ;; CLICK_DOWN/CLICK_RELEASE pair. The fallback
                     ;; is gated on (a) no drag in progress, and
                     ;; (b) the corresponding CLICK_DOWN didn't
                     ;; already handle the same region — otherwise
                     ;; a normal terminal would double-fire (copy
                     ;; the id twice, open the link twice).
                     (= atype MouseActionType/CLICK_RELEASE)
                     (let [was-dragging?    (some? @scrollbar-drag-offset)
                           already-handled? @click-action-fired?
                           anchor           @mouse-selection-anchor
                           focus            (or @mouse-selection-focus
                                              (selection/screen->document-point
                                                (selection/point mx my)
                                                selection-viewport))
                           source           @mouse-selection-source]
                       (vreset! scrollbar-drag-offset nil)
                       (vreset! click-action-fired? false)
                       (vreset! mouse-selection-anchor nil)
                       (vreset! mouse-selection-focus nil)
                       (vreset! mouse-selection-source nil)
                       (if (and selection-copy? anchor)
                         (let [sel           {:anchor anchor
                                              :focus  focus
                                              :source source}
                               simple-click? (= anchor (:focus sel))
                               screen-point  (selection/point mx my)
                               bubble-hit    (when (and simple-click?
                                                     (not= source :input))
                                               (bubble-copy-hit
                                                 screen-point
                                                 transcript-bubble-copy-regions))
                               screen-sel    (selection/document->screen-selection
                                               sel selection-viewport)
                               payload       (selection/selected-text
                                               (get-in db [:layout :screen-cells])
                                               screen-sel
                                               (selectable-ranges-for-source
                                                 source transcript-selectable-ranges input-selectable-ranges))]
                           (state/dispatch [:clear-mouse-selection])
                           (cond
                             bubble-hit
                             (copy-bubble! (:text bubble-hit))

                             (and (not simple-click?)
                               (not (str/blank? payload)))
                             (copy-selection! payload source)))
                         (when (and (not was-dragging?) (not already-handled?))
                           (if-let [hit (cr/lookup mx my)]
                             (case (:kind hit)
                               :copy-id
                               (copy-conversation-id! (:text hit))

                               :copy-as-markdown
                               (copy-conversation-as-markdown! (:text hit))

                               :switch-conversation
                               (switch-conversation! {:action :switch :id (:text hit)})

                               :toggle-details
                               (state/dispatch [:toggle-detail (:conversation-id hit) (:node-id hit)])

                               :resources
                               (open-resources-popup! screen (:refs hit))

                               (open-click-target! hit))
                             (when-let [bubble-hit (bubble-copy-hit
                                                     (selection/point mx my)
                                                     transcript-bubble-copy-regions)]
                               (copy-bubble! (:text bubble-hit))))))
                       (recur))

                     ;; MOVE — hover. We want the chat link-chrome
                     ;; rows to highlight when the user hovers over
                     ;; them. Look up the click region under the
                     ;; cursor; if it changed, update the hover
                     ;; pointer and bump the render version so the
                     ;; renderer repaints the highlighted row. The
                     ;; bump is gated by `set-hovered!` returning
                     ;; true so a cursor twitch inside the same
                     ;; chrome row doesn't trigger a repaint storm.
                     (= atype MouseActionType/MOVE)
                     (do (when (cr/set-hovered! (cr/lookup mx my))
                           (state/dispatch [:bump-render-version]))
                       (recur))

                     ;; CLICK_DOWN that didn't grab the scrollbar:
                     ;; if it landed on a registered click region
                     ;; (a markdown link / image / file-link
                     ;; chrome row), hand the URL to the OS opener
                     ;; on a side thread — a slow `xdg-open` cannot
                     ;; freeze the input loop's redraw cadence.
                     (= atype MouseActionType/CLICK_DOWN)
                     (do (if-let [hit (cr/lookup mx my)]
                           (do
                             ;; Tell the matching CLICK_RELEASE in
                             ;; the same gesture pair to skip the
                             ;; fallback fire — we just handled it.
                             (vreset! click-action-fired? true)
                             (vreset! last-selection-click nil)
                             (case (:kind hit)
                             ;; Header copy-id affordance: drop the
                             ;; FULL UUID onto the system clipboard,
                             ;; then push a host notification — the
                             ;; header band's LEFT slot subscribes to
                             ;; `vis.core/notifications` and surfaces
                             ;; `✓ Copied conversation ID` for ~1.5s
                             ;; before the entry expires. No
                             ;; TUI-specific flash state; the
                             ;; cross-channel notifications system
                             ;; carries the feedback.
                               :copy-id
                               (copy-conversation-id! (:text hit))

                               :copy-as-markdown
                               (copy-conversation-as-markdown! (:text hit))

                               :switch-conversation
                               (switch-conversation! {:action :switch :id (:text hit)})

                               :toggle-details
                               (state/dispatch [:toggle-detail (:conversation-id hit) (:node-id hit)])

                               :resources
                               (open-resources-popup! screen (:refs hit))

                             ;; Default (markdown link / image /
                             ;; file-link chrome): hand the URL to
                             ;; the OS opener on a side thread —
                             ;; a slow `xdg-open` cannot freeze the
                             ;; input loop's redraw cadence.
                               (open-click-target! hit)))
                           (when selection-copy?
                             (let [screen-anchor (selection/point mx my)
                                   source        (selection/source-at-point
                                                   screen-anchor
                                                   transcript-selectable-ranges
                                                   input-selectable-ranges
                                                   {:row-padding 2})]
                               (if-not source
                                 (vreset! last-selection-click nil)
                                 (let [now-ms       (System/currentTimeMillis)
                                       source-ranges (selectable-ranges-for-source
                                                       source
                                                       transcript-selectable-ranges
                                                       input-selectable-ranges)
                                       line-sel      (when (selection/double-click?
                                                             @last-selection-click
                                                             now-ms
                                                             source
                                                             screen-anchor
                                                             mouse-double-click-ms)
                                                       (selection/line-selection-at-point
                                                         screen-anchor
                                                         source-ranges
                                                         selection-viewport))
                                       doc-anchor    (or (:anchor line-sel)
                                                       (selection/screen->document-point
                                                         screen-anchor selection-viewport))
                                       doc-focus     (or (:focus line-sel) doc-anchor)]
                                   (vreset! last-selection-click
                                     (when-not line-sel
                                       {:source source
                                        :point  screen-anchor
                                        :time-ms now-ms}))
                                   (vreset! mouse-selection-anchor doc-anchor)
                                   (vreset! mouse-selection-focus (:focus line-sel))
                                   (vreset! mouse-selection-source source)
                                   (state/dispatch
                                     [:set-mouse-selection
                                      {:anchor doc-anchor
                                       :focus  doc-focus
                                       :source source}]))))))
                       (recur))

                     ;; Every other click — inside the input box,
                     ;; on the footer, etc. — falls through here.
                     ;; The scrollbar branch above already covers
                     ;; the right-gutter \"click on track to jump\";
                     ;; this `:else` is effectively the no-op tail.
                     :else (recur)))

               ;; Paste-buffer accumulator runs AFTER the mouse
               ;; branch so a stuck paste bracket can't swallow
               ;; clicks. The buffer only collects character-bearing
               ;; KeyStrokes; a MouseAction would have matched above.
                 (some? @paste-buffer)
                 (do (when-let [ch (input/keystroke->paste-char key)]
                       (.append ^StringBuilder @paste-buffer ^String ch))
                   (recur))

                 ;; Pi-style placeholder smart-delete: a single
                 ;; Backspace right after the closing `]` of a
                 ;; `[Pasted #N: …]` token nukes the WHOLE token in
                 ;; one keystroke, and drops the matching entry from
                 ;; `:pastes` so memory tracks what the user can
                 ;; still see in their input. Without this, the user
                 ;; would have to mash Backspace 27+ times to remove
                 ;; one placeholder — the visual unit-of-edit is the
                 ;; whole token, not its individual characters.
                 (and (instance? KeyStroke key)
                   (= KeyType/Backspace (.getKeyType ^KeyStroke key))
                   (input/placeholder-id-before-cursor (:input db)))
                 (let [paste-id (input/placeholder-id-before-cursor (:input db))]
                   (state/dispatch
                     [:update-input (input/delete-placeholder-backward (:input db))])
                   (when paste-id
                     (state/dispatch [:remove-paste paste-id]))
                   (recur))

                 :else
                 (let [{:keys [action state]} (input/handle-key key (:input db))]
                   (state/dispatch [:update-input state])
                   (let [run-command!
                         (fn [cmd]
                           (when-not (:dialog-open? @state/app-db)
                             (case cmd
                               :new-conversation
                               (switch-conversation! {:action :new})

                               :fork-conversation
                               (switch-conversation! {:action :fork})

                               :switch-conversation
                               (show-conversations!)

                               :providers
                               (when-let [c (with-dialog-lock
                                              #(provider/show-provider-dialog! screen (:config @state/app-db)))]
                                 (state/dispatch [:set-config c]))

                               :settings
                               (when-let [s (with-dialog-lock
                                              #(dlg/settings-dialog! screen
                                                 (:settings @state/app-db)))]
                                 (state/dispatch [:update-settings s]))

                            ;; No :quit branch — the palette has no Quit
                            ;; entry; Ctrl+C is the only quit path.
                               nil)))]
                     (case action
                       :quit nil

                       :clear-input
                       (do (state/dispatch [:reset-input])
                         (recur))

                       :show-palette
                     ;; Modal stack: each command runs nested inside the
                     ;; palette's open state, so ESC from a sub-dialog
                     ;; (Settings, etc.) reopens the palette
                     ;; instead of dropping the user back to the chat.
                     ;; Only ESC pressed *inside* the palette itself
                     ;; closes the whole stack.
                       (do
                         (when-not (:dialog-open? @state/app-db)
                           (loop []
                             (when-let [cmd (with-dialog-lock #(dlg/command-palette! screen))]
                               (run-command! cmd)
                               (recur))))
                         (recur))

                       :history-up
                       (do (state/dispatch [:history-up])
                         (recur))

                       :history-down
                       (do (state/dispatch [:history-down])
                         (recur))

                       :cycle-reasoning
                       (do (state/dispatch [:cycle-reasoning-level])
                         (recur))

                       :cycle-verbosity
                       (do (state/dispatch [:cycle-codex-verbosity])
                         (recur))

                       :cycle-model
                       (do (state/dispatch [:cycle-model])
                         (recur))

                       :show-conversations
                       (do (show-conversations!)
                         (recur))

                       :pick-file
                       (do
                         (when-not (:dialog-open? @state/app-db)
                           (when-let [path (with-dialog-lock #(dlg/file-picker-dialog! screen))]
                             (state/dispatch
                               [:update-input
                                (input/paste-text state
                                  (str (input/format-file-mention path) " "))])))
                         (recur))

                       :send
                       (do (submit-input! @state/app-db state)
                         (recur))

                       :cancel
                       (do (when (:loading? @state/app-db)
                             (state/dispatch [:cancel-turn]))
                         (recur))

                       :scroll-up
                       (do (state/dispatch [:scroll-up arrow-scroll-step total-h inner-h])
                         (recur))

                       :scroll-down
                       (do (state/dispatch [:scroll-down arrow-scroll-step total-h inner-h])
                         (recur))

                       :continue (recur))))))))
         (finally
        ;; Tell native terminals to stop wrapping pastes and reporting
        ;; SGR mouse events. Standalone Swing never writes these escape
        ;; modes to the user's controlling TTY.
           (disable-terminal-escape-modes! opts)
        ;; Drop the notifications watcher so the next TUI session
        ;; doesn't accumulate stale hooks (the screen is short-lived
        ;; relative to the JVM — leaving stale watchers around would
        ;; eventually hold references to dead atoms).
           (try (vis/unwatch-notifications! :tui-screen) (catch Throwable _ nil))
        ;; Tell the render thread to exit and wake it so the wait
        ;; finishes immediately. Daemon thread, so the join is
        ;; optional — doing it anyway lets the final paint (or the
        ;; no-op when shutdown? was already true) finish before we
        ;; tear down the screen.
           (state/dispatch [:shutdown])
           ;; Cancel the pre-warm worker BEFORE joining the render
           ;; thread — it might still be holding `cached*` work
           ;; that we'd rather drop than wait on.
           (virtual/stop-pre-warm! @prewarm-thread)
           (when-let [t @render-thread]
             (try (.join ^Thread t 500) (catch Throwable _ nil)))
           (when-let [t @provider-limits-thread]
             (try (.join ^Thread t 500) (catch Throwable _ nil)))
           (when-let [cleanup @title-listener-cleanup]
             (try (cleanup) (catch Throwable _ nil)))
           (when-let [conversation (:conversation @state/app-db)]
             (chat/dispose! conversation))
           (.stopScreen screen)))))))

;;; ── CLI argument parsing for the TUI channel ─────────────────────────

(def ^:private tui-usage
  (str "vis channels tui [--conversation-id ID | --resume] [--standalone] "
    "[--font-size N] [--font-bundle NAME] [--font-family FAMILY] [--font-path PATH] "
    "[--standalone-cols N] [--standalone-rows N]"))

(defn- missing-value?
  [v]
  (or (nil? v) (str/starts-with? v "--")))

(defn- flag-value
  [flag more]
  (let [v (first more)]
    (when (missing-value? v)
      (throw (ex-info (str flag " requires a value"
                        "\nUsage: " tui-usage)
               {:vis/user-error true})))
    v))

(defn- parse-positive-int
  [flag value]
  (try
    (let [n (Integer/parseInt value)]
      (when-not (pos? n)
        (throw (NumberFormatException. value)))
      n)
    (catch NumberFormatException _
      (throw (ex-info (str flag " requires a positive integer, got: " value
                        "\nUsage: " tui-usage)
               {:vis/user-error true
                :flag           flag
                :value          value})))))

(defn- parse-args
  "Parse `vis channels tui` flags.
     --conversation-id ID   Resume a conversation (full UUID or short prefix)
     --resume               Resume the latest :tui conversation
     --standalone           Use Lanterna's Swing backend instead of /dev/tty
     --font-size N          Standalone Swing font size, default 16
     --font-bundle NAME     Bundled Cascadia variant: code, code-pl, code-nf, mono, mono-pl, mono-nf
     --font-family FAMILY   Standalone Swing system font stack
     --font-path PATH       Standalone Swing TTF/OTF file
     --standalone-cols N    Standalone initial columns, default 120
     --standalone-rows N    Standalone initial rows, default 36

   Unknown flags and missing flag values throw a `:vis/user-error` ex-info
   so the user sees a clean error instead of the TUI silently swallowing a
   typo (e.g. `--conversations-id`)."
  [args]
  (loop [args (seq args) opts {}]
    (if-not args
      opts
      (let [arg  (first args)
            more (next args)]
        (case arg
          "--conversation-id"
          (let [v (flag-value arg more)]
            (recur (next more) (assoc opts :conversation-id v)))

          "--resume"
          (recur more (assoc opts :resume true))

          "--standalone"
          (recur more (assoc opts :standalone true))

          "--font-size"
          (let [v (flag-value arg more)]
            (recur (next more) (assoc opts :font-size (parse-positive-int arg v))))

          "--font-bundle"
          (let [v (flag-value arg more)]
            (recur (next more) (assoc opts :font-bundle (keyword v))))

          "--font-family"
          (let [v (flag-value arg more)]
            (recur (next more) (assoc opts :font-family v)))

          "--font-path"
          (let [v (flag-value arg more)]
            (recur (next more) (assoc opts :font-path v)))

          "--standalone-cols"
          (let [v (flag-value arg more)]
            (recur (next more) (assoc opts :columns (parse-positive-int arg v))))

          "--standalone-rows"
          (let [v (flag-value arg more)]
            (recur (next more) (assoc opts :rows (parse-positive-int arg v))))

          (throw (ex-info (str "unknown flag: " arg
                            "\nUsage: " tui-usage)
                   {:vis/user-error true})))))))

(defn- redirect-stdio-to-log!
  "Lanterna writes to /dev/tty directly. Everything else (Telemere, SLF4J,
   library prints, JVM warnings) MUST be redirected to ~/.vis/vis.log
   before any other code runs — otherwise stray bytes corrupt the screen."
  []
  (try (require 'taoensso.telemere)
    ((resolve 'taoensso.telemere/remove-handler!) :default/console)
    (catch Throwable _ nil))
  (let [log-dir  (java.io.File. (str (System/getProperty "user.home") "/.vis"))
        _        (when-not (.exists log-dir) (.mkdirs log-dir))
        log-path (str log-dir "/vis.log")
        log-ps   (java.io.PrintStream.
                   (java.io.FileOutputStream. log-path true) true)
        log-w    (java.io.OutputStreamWriter. log-ps)]
    (System/setOut log-ps)
    (System/setErr log-ps)
    (alter-var-root #'*out* (constantly log-w))
    (alter-var-root #'*err* (constantly log-w))))

(defn channel-main
  "Channel entry point: full TUI bootstrap. Performs the stdout/stderr
   redirect, runs `vis/init!`, then hands off to `run-chat!`. Errors
   surface on the original terminal and the log file.

   Invoked by `com.blockether.vis.core` dispatch — not called from
   vis-runtime directly."
  [args]
  (redirect-stdio-to-log!)
  (vis/init!)
  (let [exit-code (atom 0)]
    (try
      (run-chat! (parse-args args))
      (catch Throwable t
        (if (:vis/user-error (ex-data t))
          ;; Caller-facing error: invalid flag value, missing
          ;; conversation id, etc. Print the message clean and let the
          ;; process exit non-zero — no Java stack trace, no rethrow
          ;; (which would trigger clojure.main's auto-trace dump).
          (do (.println ^java.io.PrintStream vis/original-stdout (str "vis: " (.getMessage t)))
            (reset! exit-code 2))
          ;; Genuine fatal: dump the trace to the terminal AND the log
          ;; so we can post-mortem it.
          (do (.println ^java.io.PrintStream vis/original-stdout (str "vis: fatal error — " (.getMessage t)))
            (.printStackTrace t (java.io.PrintStream. ^java.io.OutputStream @vis/tty-out true))
            (throw t))))
      (finally
        (vis/shutdown!)))
    (when (pos? @exit-code)
      (System/exit @exit-code))))

;;; ── Channel registration (auto-discovered via META-INF/vis-extension/vis.edn) ──

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-tui.screen
     :ext/doc       "Lanterna-based terminal UI channel."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/channels  [{:channel/id        :tui
                      :channel/cmd       "tui"
                      :channel/doc       "Interactive terminal UI."
                      :channel/usage     tui-usage
                      :channel/owns-tty? true
                      :channel/main-fn   #'channel-main}]}))
