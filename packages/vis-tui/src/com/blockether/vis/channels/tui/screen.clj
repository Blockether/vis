(ns com.blockether.vis.channels.tui.screen
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.extension :as ext]
            [com.blockether.vis.channels.tui.chat :as chat]
            [com.blockether.vis.channels.tui.footer :as footer]
            [com.blockether.vis.channels.tui.input :as input]
            [com.blockether.vis.channels.tui.provider :as provider]
            [com.blockether.vis.channels.tui.render :as render]
            [com.blockether.vis.channels.tui.state :as state]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.channels.tui.dialogs :as dlg]
            [com.blockether.vis.loop.runtime.conversation.core :as conversations])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
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

(def ^:private input-min-lines 3)
(def ^:private input-max-lines 8)
;; Hint strip rules
;;
;; Idle, input EMPTY    → show newline + menu (newcomers see how to wrap +
;;                       discover the menu).
;; Idle, input NON-EMPTY → just menu. \"Enter send\" is monkey-obvious; once
;;                        someone has typed they don't need that hint.
;; Loading              → cancel + quit. Same as before.
;; Cancelling            → progress message + quit.
;;
;; Removed from idle: `Enter send` (universally obvious), `↑↓ scroll`
;; (intuitive in any text input), `Ctrl+P/N history` (now discoverable
;; from the Ctrl+K menu).
(def ^:private hint-idle-empty " Alt+Enter newline · Ctrl+K menu ")
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
   `:now-ms` so `progress->text` can render the spinner frame."
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
        projected (mapv (fn [msg]
                          (cond
                            ;; Has trace: full iteration + answer rendering
                            (and (= :assistant (:role msg)) (:trace msg))
                            (-> msg
                              (assoc :text
                                (render/format-answer-with-thinking
                                  (:raw-answer msg) (:trace msg) bubble-w settings
                                  (:confidence msg)))
                              strip-ts)
                            ;; Plain assistant message: apply markdown
                            (= :assistant (:role msg))
                            (-> msg
                              (assoc :text
                                (render/format-answer-markdown (:text msg) bubble-w))
                              strip-ts)
                            ;; User messages: only timestamp gating.
                            :else (strip-ts msg)))
                    messages)]
    ;; Replace the loading placeholder with the live progress block
    ;; (spinner + phase + iteration trace).
    (if (and loading? (seq projected))
      (let [last-idx (dec (count projected))
            last-msg (get projected last-idx)]
        (if (= :assistant (:role last-msg))
          (assoc projected last-idx
            (assoc last-msg :text (render/progress->text progress bubble-w settings progress-extra)))
          projected))
      projected)))

(defn- input-text-rows
  "Compute visible text rows for the input box based on content."
  [{:keys [lines]}]
  (let [n (count lines)]
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
   {:keys [messages msg-scroll input progress loading? cancelling?
           query-start-ms settings] :as db}
   now-ms]
  (let [now-ms       (long now-ms)
        g            (.newTextGraphics screen)
        text-rows    (input-text-rows input)
        input-box-h  (+ text-rows 2 (* 2 render/input-pad-y))
        ;; Reserve the bottom-most row for the dedicated footer
        ;; (model / run-state / ctx-left%). The input box sits
        ;; directly above it; the messages area fills everything
        ;; from the top down to the input-box top.
        footer-row   (dec rows)
        input-top    (- rows input-box-h 1)
        msg-top      0
        msg-bottom   input-top
        ;; Mirror `draw-messages-area!`'s gutter math so width
        ;; calculations match the renderer exactly.
        msg-side-pad 6 ;; 3 left + 3 right gutter
        bubble-w     (max 1 (- cols msg-side-pad))
        progress-extra {:now-ms         now-ms
                        :query-start-ms query-start-ms
                        :cancelling?    (boolean cancelling?)}
        effective-messages (apply-settings messages progress loading? bubble-w settings progress-extra)
        inner-h      (max 0 (- msg-bottom msg-top 2)) ;; top + bottom margins
        total-h      (render/total-messages-height effective-messages bubble-w)]
    (render/fill-background! g cols rows)
    (render/draw-messages-area! g effective-messages msg-top msg-bottom cols msg-scroll nil)
    (let [[cx cy] (render/draw-input-box! g input input-top text-rows cols
                    (current-hint db))]
      (footer/draw-footer! g db footer-row cols now-ms)
      (.setCursorPosition screen (TerminalPosition. cx cy)))
    (.refresh screen Screen$RefreshType/DELTA)
    {:cols cols :rows rows :total-h total-h :inner-h inner-h}))

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
            "vis-tui-render")]
    (.setDaemon t true)
    (.start t)
    t))

(defn- format-conversation-not-found
  "Build a friendly multi-line message for `--conversation-id` misses,
   listing the most recent :vis conversations so the user has
   something to copy-paste."
  [cid]
  (let [available (try (vec (take 10 (conversations/by-channel :vis)))
                    (catch Throwable _ []))
        line (fn [c]
               (let [id-str (str (:id c))
                     id8    (if (>= (count id-str) 8) (subs id-str 0 8) id-str)
                     title  (let [t (:title c)] (when-not (str/blank? t) t))]
                 (str "  " id8 "  " (or title "(untitled)"))))]
    (str "Conversation not found: " cid
      (if (seq available)
        (str "\n\nAvailable :vis conversations (most recent first):\n"
          (str/join "\n" (map line available))
          "\n\nUse the 8-char prefix or full UUID with --conversation-id.")
        "\n\nNo :vis conversations exist yet — run `vis channels tui` without --conversation-id first."))))

(defn run-chat!
  "Start the fullscreen chat TUI. Blocks until user quits.
   Optional `opts` map:
     :conversation-id uuid-string — resume a specific conversation
     :resume          true        — resume the latest :vis conversation"
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

  ;; Load persisted config
     (when-let [c (config/load-config)]
       (state/dispatch [:set-config c]))

     (let [terminal (UnixTerminal. @config/tty-in @config/tty-out (Charset/defaultCharset))
           _        (input/register-custom-patterns! terminal)
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

      ;; Sweep orphaned running queries from previous crashes so they
      ;; don't show raw query text in the rebuilt history.
         (try (conversations/sweep-orphaned-running-queries!) (catch Throwable _ nil))

      ;; Init conversation: resume if --conversation-id given, else fresh.
      ;; The --conversation-id case was already validated above (before
      ;; Lanterna started), so here we only need the pre-resolved value.
         (when-let [config (:config @state/app-db)]
           (let [{:keys [id history]}
                 (if (:conversation-id opts)
                   resumed-from-flag
                   (if (:resume opts)
                  ;; --resume: pick up the latest :vis conversation
                     (if-let [latest (first (conversations/by-channel :vis))]
                       (or (chat/resume-conversation (:id latest))
                         (chat/make-conversation config))
                       (chat/make-conversation config))
                     (chat/make-conversation config)))
              ;; Set title from DB if present; do not synthesize from messages.
                 conv-info (when-let [c (conversations/by-id id)] c)
                 title     (when-let [t (some-> conv-info :title)]
                             (when-not (str/blank? t) t))]
             (state/dispatch [:init-conversation {:id id} history])
             (when title
               (state/dispatch [:set-title title]))
             (channels/register-conversation-shutdown-hook! id)))

      ;; Spawn the render thread BEFORE the input loop. It will paint
      ;; the first frame as soon as `:render-version` is non-zero (every
      ;; init dispatch above bumps it).
         (vreset! render-thread (start-render-thread! screen))
         (loop []
           ;; Layout fields are populated by the render thread after
           ;; the first paint. Until then, scroll handlers fall back
           ;; to safe defaults and act as a no-op.
           ;; Pure poll — no rendering on this thread anymore. The
           ;; render thread handles all screen output.
           (let [db      @state/app-db
                 {:keys [total-h inner-h]} (:layout db)
                 total-h (or total-h 0)
                 inner-h (or inner-h 0)
                 key     (.pollInput screen)]
             (if (nil? key)
               (do (Thread/sleep 16) (recur))
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

                             :toggles
                             (when-let [s (with-dialog-lock #(dlg/settings-dialog! screen (:settings @state/app-db)))]
                               (state/dispatch [:update-settings s]))

                             :system-prompt
                             (with-dialog-lock
                               #(let [conv-id (get-in @state/app-db [:conv :id])
                                      prompt  (if conv-id
                                                (or (conversations/effective-system-prompt conv-id)
                                                  "(no system prompt)")
                                                "(no conversation)")]
                                  (dlg/text-viewer-dialog! screen "Inspect Latest System Prompt" prompt)))

                            ;; No :quit branch — the palette has no Quit
                            ;; entry; Ctrl+C is the only quit path.
                             nil)))]
                   (case action
                     :quit nil

                     :show-palette
                     (if (:dialog-open? @state/app-db)
                       (recur)
                       (let [cmd (with-dialog-lock #(dlg/command-palette! screen))]
                         (when cmd (run-command! cmd))
                         (recur)))

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
                               (:conv @state/app-db)
                               (not (:loading? @state/app-db)))
                         (state/dispatch [:send-message text]))
                       (recur))

                     :cancel
                     (do (when (:loading? @state/app-db)
                           (state/dispatch [:cancel-query]))
                       (recur))

                     :scroll-up
                     (do (state/dispatch [:scroll-up 3 total-h inner-h])
                       (recur))

                     :scroll-down
                     (do (state/dispatch [:scroll-down 3 total-h inner-h])
                       (recur))

                     :continue (recur)))))))
         (finally
        ;; Tell the render thread to exit and wake it so the wait
        ;; doesn't sit out its full timeout. Daemon thread, so we don't
        ;; strictly have to join — but doing so ensures the final paint
        ;; (or no paint, if shutdown? was already true) finishes before
        ;; we tear down the screen.
           (state/dispatch [:shutdown])
           (when-let [t @render-thread]
             (try (.join ^Thread t 500) (catch Throwable _ nil)))
           (when-let [conv (:conv @state/app-db)]
             (chat/dispose! conv))
           (.stopScreen screen)))))))

;;; ── CLI argument parsing for the TUI channel ─────────────────────────

(defn- parse-args
  "Parse `vis tui` flags. Unknown flags are ignored on purpose so the
   TUI never refuses to start because of a stray argument.
     --conversation-id ID   Resume a conversation (full UUID or short prefix)
     --resume               Resume the latest :vis conversation"
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
   redirect, runs `config/init!`, then hands off to `run-chat!`. Errors
   surface on the original terminal and the log file.

   Invoked by `com.blockether.vis.channel` dispatch — not called from
   vis-core directly."
  [args]
  (redirect-stdio-to-log!)
  (config/init!)
  (let [exit-code (atom 0)]
    (try
      (run-chat! (parse-args args))
      (catch Throwable t
        (if (:vis/user-error (ex-data t))
          ;; Caller-facing error: invalid flag value, missing
          ;; conversation id, etc. Print the message clean and let the
          ;; process exit non-zero — no Java stack trace, no rethrow
          ;; (which would trigger clojure.main's auto-trace dump).
          (do (.println ^java.io.PrintStream config/original-stdout (str "vis: " (.getMessage t)))
            (reset! exit-code 2))
          ;; Genuine fatal: dump the trace to the terminal AND the log
          ;; so we can post-mortem it.
          (do (.println ^java.io.PrintStream config/original-stdout (str "vis: fatal error — " (.getMessage t)))
            (.printStackTrace t (java.io.PrintStream. ^java.io.OutputStream @config/tty-out true))
            (throw t))))
      (finally
        (config/shutdown!)))
    (when (pos? @exit-code)
      (System/exit @exit-code))))

;;; ── Channel registration (auto-discovered via META-INF/vis.edn) ──

(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.blockether.vis.channels.tui.screen
     :ext/doc       "Lanterna-based terminal UI channel."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/channels  [{:channel/id        :tui
                      :channel/cmd       "tui"
                      :channel/doc       "Interactive terminal UI."
                      :channel/usage     "vis tui [--conversation-id ID | --resume]"
                      :channel/owns-tty? true
                      :channel/main-fn   #'channel-main}]}))
