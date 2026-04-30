(ns com.blockether.vis.ext.channel-tui.screen
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.external-opener :as opener]
            [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.header :as header]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input MouseAction MouseActionType]
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
;; Hint strip rules
;;
;; Idle, input EMPTY    → show newline + history cycle + menu. The
;;                       empty box is the moment someone is deciding
;;                       what to type, so we surface `Ctrl+P/N` for
;;                       cycling through prior prompts and `Alt+Enter`
;;                       for multi-line composition right where the
;;                       eye lands.
;; Idle, input NON-EMPTY → just menu. \"Enter send\" is monkey-obvious; once
;;                        someone has typed the hint is redundant,
;;                        and history-cycle would clobber the buffer.
;; Loading              → cancel + quit. Same as before.
;; Cancelling            → progress message + quit.
;;
;; Removed from idle: `Enter send` (universally obvious), `↑↓ scroll`
;; (intuitive in any text input).
(def ^:private hint-idle-empty " Alt+Enter newline · Ctrl+P/N history · Ctrl+K menu ")
(def ^:private hint-idle-typed " Ctrl+K menu ")
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

(defn- apply-settings
  "Project messages for display: apply settings to all assistant messages
   that carry a :trace, and replace the live placeholder with the
   spinner-led progress text while loading. Runs every frame so
   toggling settings is immediately reactive.

   `progress-extra` carries the wall-clock start, cancelling flag, and
   `:now-ms` so `progress->text` can render the spinner frame.

   Every assistant turn that carries a `:trace` renders fully: code
   blocks, results, thinking, all of it. The persisted trace is
   already in app-db (rebuilt from the DB on resume), so older turns
   render the same way the most recent one does. There is no
   collapse mode; if you want a quieter transcript, scroll."
  [messages progress loading? bubble-w settings progress-extra]
  (let [;; The `:show-timestamps` toggle drops the per-message
        ;; date/time stamp from the role-label row. We do this here
        ;; in the projection pass instead of threading the setting
        ;; into `draw-chat-bubble!` because the bubble already
        ;; respects \"absent timestamp\" — it just doesn't render the
        ;; right-aligned `time-str` when `:timestamp` is nil. Same
        ;; behavior, fewer arg-threading scars.
        show-timestamps? (get settings :show-timestamps false)
        strip-ts (fn [m] (if show-timestamps? m (dissoc m :timestamp)))
        ;; Apply trace→text projection and markdown to assistant messages.
        projected (vec
                    (map-indexed
                      (fn [_idx message]
                        (cond
                          ;; Has trace: full iteration + answer rendering.
                          ;; Cancelled bubbles flow through here too —
                          ;; format-answer-with-thinking emits a plain
                          ;; status footer instead of an answer block,
                          ;; so partial work stays visible above the
                          ;; \"Cancelled by user.\" line.
                          (and (= :assistant (:role message)) (:trace message))
                          (-> message
                            (assoc :text
                              (render/format-answer-with-thinking
                                (:raw-answer message) (:trace message) bubble-w settings
                                (:confidence message)
                                (= :cancelled (:status message))))
                            strip-ts)
                          ;; Plain assistant message: apply markdown
                          (= :assistant (:role message))
                          (-> message
                            (assoc :text
                              (render/format-answer-markdown (:text message) bubble-w))
                            strip-ts)
                          ;; User messages: only timestamp gating.
                          :else (strip-ts message)))
                      messages))]
    ;; Replace the loading placeholder with the live progress block
    ;; (spinner + phase + iteration trace).
    (if (and loading? (seq projected))
      (let [last-idx     (dec (count projected))
            last-message (get projected last-idx)]
        (if (= :assistant (:role last-message))
          (assoc projected last-idx
            (assoc last-message :text (render/progress->text progress bubble-w settings progress-extra)))
          projected))
      projected)))

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
  "Draw one frame: background, messages area (bubbles), input box.
   The input box's top border carries keybinding hints and the bottom
   border carries either the live model/context status line (idle) or
   an animated spinner + phase + elapsed time (loading).

   Returns the layout map `{:total-h, :inner-h, :cols, :rows}` so the
   render thread can publish it back into app-db for the input thread's
   scroll handlers. `apply-settings` runs ONCE here and feeds both the
   layout calculation and the actual draw — the old code path computed
   it twice per frame, which doubled cost on long traces."
  [^TerminalScreen screen cols rows
   {:keys [messages messages-scroll input progress loading? cancelling?
           query-start-ms settings] :as db}
   now-ms]
  (let [now-ms       (long now-ms)
        g            (.newTextGraphics screen)
        text-rows    (input-text-rows input cols)
        input-box-h  (+ text-rows 2 (* 2 render/input-pad-y))
        ;; Reserve the bottom-most row for the dedicated footer
        ;; (model / run-state / ctx-left%) and the top-most band for
        ;; the dedicated header (top rule + title row + bottom rule).
        ;; The input box sits directly above the footer; the messages
        ;; area fills everything from `messages-top` down to the
        ;; input-box top.
        header-top   0
        footer-row   (dec rows)
        input-top    (- rows input-box-h 1)
        messages-top    header/HEADER_ROWS
        messages-bottom input-top
        ;; Single source of truth for the gutter math lives in
        ;; `render.clj` (`MESSAGE_SIDE_PAD`). Reference it directly; do
        ;; NOT inline a literal here. Two layers disagreeing by even
        ;; one column makes `format-iteration-entry` size labels for
        ;; one bubble-w while `draw-chat-bubble!` paints into a
        ;; different bubble-w — right-aligned labels (`CODE 3`,
        ;; `✓ 3ms`, `FINAL ANSWER`) wrap onto two lines from the
        ;; mismatch. Use the const, never the value.
        bubble-w     (max 1 (- cols render/MESSAGE_SIDE_PAD))
        progress-extra {:now-ms         now-ms
                        :query-start-ms query-start-ms
                        :cancelling?    (boolean cancelling?)}
        effective-messages (apply-settings messages progress loading? bubble-w settings progress-extra)
        inner-h      (max 0 (- messages-bottom messages-top 2)) ;; top + bottom margins
        total-h      (render/total-messages-height effective-messages bubble-w)]
    (render/fill-background! g cols rows)
    (header/draw-header! g db header-top cols)
    (render/draw-messages-area! g effective-messages messages-top messages-bottom cols messages-scroll)
    (let [[cx cy] (render/draw-input-box! g input input-top text-rows cols
                    (current-hint db))]
      (footer/draw-footer! g db footer-row cols now-ms)
      (.setCursorPosition screen (TerminalPosition. cx cy)))
    (.refresh screen Screen$RefreshType/DELTA)
    {:cols cols :rows rows :total-h total-h :inner-h inner-h
     :messages-top messages-top}))

;;; ── Render thread ───────────────────────────────────────────────────────────────

(def ^:private spinner-tick-ms
  "How often the spinner advances while a query is in flight. Drives
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

(defn- register-conversation-shutdown-hook!
  "Register a JVM shutdown hook that prints the TUI resume command for
   the conversation the user was on. TUI-local: the printed string is
   a `vis channels tui …` sub-command, so this hook lives next to the
   only consumer instead of in vis-runtime or vis-cli."
  [conversation-id]
  (let [hook (Thread. (fn []
                        (let [^java.io.PrintStream out vis/original-stdout]
                          (.println out "")
                          (.println out (str "  vis channels tui --conversation-id " conversation-id))
                          (.println out "")
                          (.flush out))))]
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
   polling. Returns the listener fn so the caller can deregister it.
   Also installs a JVM shutdown hook that drops the listener —
   long-running TUI sessions should not leak entries in the global
   `title-listeners` registry across hot reloads."
  [conversation-id]
  (let [listener (vis/add-title-listener! conversation-id
                   (fn [new-title]
                     (state/dispatch [:set-title (or new-title "")])))]
    (register-shutdown-hook!
      #(vis/remove-title-listener! conversation-id listener))
    listener))

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

     (let [terminal (UnixTerminal. @vis/tty-in @vis/tty-out (Charset/defaultCharset))
           _        (input/register-custom-patterns! terminal)
           ;; Enable mouse capture: scrollbar drag, wheel scroll,
           ;; AND click-to-open + hover-highlight on link/image
           ;; chrome rows. CLICK_RELEASE_DRAG_MOVE is the most
           ;; verbose mode but it's the only one that delivers bare
           ;; `MOVE` events — we need them so a hovered link row
           ;; can repaint with a hover-bg before the user clicks.
           ;; The MOVE handler debounces by row so it never re-paints
           ;; on cursor twitches inside the same chrome row. Wheel
           ;; scroll arrives as `MouseActionType/SCROLL_UP/_DOWN`
           ;; in every mode. If the host terminal doesn't honour the
           ;; mode-set escape, the entire mouse surface becomes
           ;; inert (no clicks, no hover) — by design, since the
           ;; mouse picker is the only entry point to the opener.
           _        (try (.setMouseCaptureMode terminal MouseCaptureMode/CLICK_RELEASE_DRAG_MOVE)
                      (catch Throwable _ nil))
           screen   (TerminalScreen. terminal)
        ;; Render thread handle is held in a volatile so the `finally`
        ;; clause can join it. (Locals from the `try` body aren't in
        ;; scope inside `finally`.)
           render-thread (volatile! nil)]
       (.startScreen screen)
       (try
      ;; Show provider dialog on first launch if no config
         (when-not (:config @state/app-db)
           (when (not (:dialog-open? @state/app-db))
             (when-let [c (with-dialog-lock #(provider/show-provider-dialog! screen (:config @state/app-db)))]
               (state/dispatch [:set-config c]))))

      ;; Sweep orphaned running queries from previous crashes so the
      ;; rebuilt history shows resolved rows only — no raw query text
      ;; from a half-completed turn.
         (try (vis/db-sweep-orphaned-running-queries!) (catch Throwable _ nil))

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
                     (chat/make-conversation config)))
              ;; Set title from DB when present — the DB row is the source of truth.
              ;; Synthesizing from messages stays out of the TUI; auto-title runs
              ;; from the runtime loop after the first turn completes.
                 conversation-info (when-let [c (vis/by-id id)] c)
                 title     (when-let [t (some-> conversation-info :title)]
                             (when-not (str/blank? t) t))]
             (state/dispatch [:init-conversation {:id id} history])
             (when title
               (state/dispatch [:set-title title]))
             (subscribe-title-listener! id)
             (register-conversation-shutdown-hook! id)))

      ;; Spawn the render thread BEFORE the input loop. It will paint
      ;; the first frame as soon as `:render-version` is non-zero (every
      ;; init dispatch above bumps it).
         (vreset! render-thread (start-render-thread! screen))
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
         (let [scrollbar-drag-offset (volatile! nil)]
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

               ;; Mouse events: scrollbar grab/drag + wheel scroll.
               ;; Bypass `input/handle-key` entirely — those events
               ;; need access to the layout published by the render
               ;; thread, which `handle-key` doesn't see.
                 (instance? MouseAction key)
                 (let [^MouseAction ma key
                       atype     (.getActionType ma)
                       pos       (.getPosition ma)
                       mx        (.getColumn pos)
                       my        (.getRow pos)
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
                                   (< my (+ (long thumb-top) thumb-h)))]
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

                     (= atype MouseActionType/CLICK_RELEASE)
                     (do (vreset! scrollbar-drag-offset nil)
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
                     (do (when-let [hit (cr/lookup mx my)]
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
                             (let [text (:text hit)]
                               (future
                                 (try (input/clipboard-copy! text)
                                   (vis/notify! "✓ Copied conversation ID"
                                     :level :success :ttl-ms 1500)
                                   (catch Throwable _ nil))))

                             ;; Default (markdown link / image /
                             ;; file-link chrome): hand the URL to
                             ;; the OS opener on a side thread —
                             ;; a slow `xdg-open` cannot freeze the
                             ;; input loop's redraw cadence.
                             (future
                               (try (opener/open! (:url hit))
                                 (catch Throwable _ nil)))))
                       (recur))

                     ;; Every other click — track above/below the
                     ;; thumb, the rest of the messages area, the
                     ;; input box, headers, footers — is ignored.
                     ;; No "click-on-track jumps the thumb here" page
                     ;; behaviour; users found that surprising and
                     ;; lossy on long conversations.
                     :else (recur)))

                 :else
                 (let [{:keys [action state]} (input/handle-key key (:input db))]
                   (state/dispatch [:update-input state])
                   (let [run-command!
                         (fn [cmd]
                           (when-not (:dialog-open? @state/app-db)
                             (case cmd
                               :configure-provider
                               (when-let [c (with-dialog-lock #(provider/show-provider-dialog! screen (:config @state/app-db)))]
                                 (state/dispatch [:set-config c]))

                               :copy
                               (with-dialog-lock #(dlg/copy-dialog! screen (:messages @state/app-db)))

                               :copy-as-markdown
                               ;; One-shot "give me the whole
                               ;; conversation as Markdown on the
                               ;; clipboard". Goes through the host
                               ;; helper `vis/conversation->markdown`
                               ;; so every channel renders the same
                               ;; projection. We surface a viewer
                               ;; dialog with the result so the user
                               ;; can confirm what was copied (and
                               ;; re-copy via the system shortcut if
                               ;; the AWT clipboard write was a no-op
                               ;; on a remote / SSH session).
                               (with-dialog-lock
                                 #(let [conversation-id (get-in @state/app-db [:conversation :id])
                                        env             (when conversation-id (vis/env-for conversation-id))
                                        markdown        (when (and env conversation-id)
                                                          (try
                                                            (vis/conversation->markdown
                                                              (:db-info env) conversation-id)
                                                            (catch Throwable t
                                                              (str "Markdown export failed: "
                                                                (or (.getMessage t) (.getName (class t)))))))]
                                    (cond
                                      (nil? conversation-id)
                                      (dlg/text-viewer-dialog! screen "Copy Conversation as Markdown"
                                        "(no conversation)")

                                      (nil? markdown)
                                      (dlg/text-viewer-dialog! screen "Copy Conversation as Markdown"
                                        "(conversation has no persisted turns yet)")

                                      :else
                                      (do (input/clipboard-copy! markdown)
                                        (dlg/text-viewer-dialog!
                                          screen
                                          "Copied conversation as Markdown (clipboard)"
                                          markdown)))))

                               :toggles
                               (when-let [s (with-dialog-lock #(dlg/settings-dialog! screen (:settings @state/app-db)))]
                                 (state/dispatch [:update-settings s]))

                               :system-prompt
                               (with-dialog-lock
                                 #(let [conversation-id (get-in @state/app-db [:conversation :id])
                                        prompt  (if conversation-id
                                                  (or (vis/effective-system-prompt conversation-id)
                                                    "(no system prompt)")
                                                  "(no conversation)")]
                                    (dlg/text-viewer-dialog! screen "Inspect Latest System Prompt" prompt)))

                            ;; No :quit branch — the palette has no Quit
                            ;; entry; Ctrl+C is the only quit path.
                               nil)))]
                     (case action
                       :quit nil

                       :show-palette
                     ;; Modal stack: each command runs nested inside the
                     ;; palette's open state, so ESC from a sub-dialog
                     ;; (Settings, Copy, etc.) reopens the palette
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

                       :send
                       (let [text (input/input->text state)]
                         (state/dispatch [:reset-input])
                         (when (and (seq (str/trim text))
                                 (:conversation @state/app-db)
                                 (not (:loading? @state/app-db)))
                           (state/dispatch [:send-message text]))
                         (recur))

                       :cancel
                       (do (when (:loading? @state/app-db)
                             (state/dispatch [:cancel-query]))
                         (recur))

                       :scroll-up
                       (do (state/dispatch [:scroll-up arrow-scroll-step total-h inner-h])
                         (recur))

                       :scroll-down
                       (do (state/dispatch [:scroll-down arrow-scroll-step total-h inner-h])
                         (recur))

                       :continue (recur))))))))
         (finally
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
           (when-let [t @render-thread]
             (try (.join ^Thread t 500) (catch Throwable _ nil)))
           (when-let [conversation (:conversation @state/app-db)]
             (chat/dispose! conversation))
           (.stopScreen screen)))))))

;;; ── CLI argument parsing for the TUI channel ─────────────────────────

(defn- parse-args
  "Parse `vis tui` flags. Unknown flags are ignored on purpose so the
   TUI never refuses to start because of a stray argument.
     --conversation-id ID   Resume a conversation (full UUID or short prefix)
     --resume               Resume the latest :tui conversation"
  [args]
  (loop [args (seq args) opts {}]
    (if-not args
      opts
      (let [arg  (first args)
            more (next args)]
        (case arg
          "--conversation-id" (recur (next more) (assoc opts :conversation-id (first more)))
          "--resume"          (recur more (assoc opts :resume true))
          (recur more opts))))))

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
                      :channel/usage     "vis tui [--conversation-id ID | --resume]"
                      :channel/owns-tty? true
                      :channel/main-fn   #'channel-main}]}))
