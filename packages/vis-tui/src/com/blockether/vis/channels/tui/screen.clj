(ns com.blockether.vis.channels.tui.screen
  (:require [clojure.string :as str]
            [com.blockether.svar.internal.router :as router]
            [com.blockether.vis.channel :as channel]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.chat :as chat]
            [com.blockether.vis.channels.tui.input :as input]
            [com.blockether.vis.channels.tui.provider :as provider]
            [com.blockether.vis.channels.tui.render :as render]
            [com.blockether.vis.channels.tui.state :as state]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.channels.tui.dialogs :as dlg]
            [com.blockether.vis.loop.runtime.conversation.core :as conversations]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core])
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

(defonce ^:private draw-lock
  ^{:doc "Single screen-mutation lock. Held by the render thread for the
          duration of one paint, and by `with-dialog-lock` for the
          duration of a modal dialog session."}
  (ReentrantLock.))

(def ^:private input-min-lines 3)
(def ^:private input-max-lines 8)
(def ^:private hint-idle " Enter send · Alt+Enter newline · ↑↓ scroll · Ctrl+P/N history · Ctrl+K commands ")
(def ^:private hint-loading " Esc cancel · ↑↓ scroll · Ctrl+C quit ")
(def ^:private hint-cancelling " Cancelling… please wait · Ctrl+C quit ")

(defn- current-hint [{:keys [loading? cancelling?]}]
  (cond
    cancelling? hint-cancelling
    loading?    hint-loading
    :else       hint-idle))

(def ^:private default-reasoning-level
  "Mirrors `query/core.clj`'s `balanced-reasoning` constant. The TUI
   doesn't expose a per-conversation override yet, so this is the level
   every new turn runs at when the model supports reasoning."
  :balanced)

(defn- chosen-model-info
  "Resolved model map for the configured root model, or nil. Carries
   `:name`, `:reasoning?`, and the rest of svar's normalized model
   metadata, so the status line can render reasoning support without
   reaching into config shape directly."
  []
  (when-let [r (try (query-core/get-router) (catch Throwable _ nil))]
    (try (query-core/resolve-effective-model r) (catch Throwable _ nil))))

(defn- format-token-count
  "e.g. 12345 → \"12.3k\"; 999 → \"999\"; nil/0 → nil."
  [n]
  (when (and (number? n) (pos? n))
    (cond
      (>= n 1000000) (format "%.1fM" (/ n 1e6))
      (>= n 1000)    (format "%.1fk" (/ n 1e3))
      :else          (str (long n)))))

(defn- last-assistant-tokens
  "Token map `{:input n :output n}` of the most recent finalized
   assistant message, or nil if none."
  [messages]
  (some->> (reverse messages)
    (some (fn [m] (when (and (= :assistant (:role m)) (:tokens m)) (:tokens m))))))

(defn- estimate-next-context-chars
  "Approximate next-iteration context size = char-count of all chat
   messages so far. Crude (no tokenizer here), but useful as a rough
   indicator while a turn is in flight."
  ^long [messages]
  (long (reduce + 0
          (keep (fn [m]
                  (let [t (:text m)]
                    (when (string? t) (count t))))
            messages))))

(defn- format-ctx-segment
  "\"12.3k/128k (10%)\" / \"128k\" / nil. `used` and `max-tokens` are
   raw token counts (longs)."
  [used max-tokens]
  (let [used-str (format-token-count used)
        max-str  (format-token-count max-tokens)]
    (cond
      (and used-str max-str (pos? max-tokens))
      (let [pct (long (Math/round (* 100.0 (/ (double used) (double max-tokens)))))]
        (str used-str "/" max-str " (" pct "%)"))

      max-str max-str
      :else   nil)))

(defn- input-status-line
  "Build the live one-line status embedded in the bottom border of the
   input box.

   Always names the chosen model + how full the context window will be
   for the next query (estimate while loading, last turn's actual
   `:input` tokens once a turn finishes). Adds the default reasoning
   level when the model exposes reasoning support.

   `inner-w` is the available width of the border (cols - 2). The
   status string fits within that budget by dropping optional segments
   (least-important first) when too wide. Returns nil if no info is
   available yet (no provider configured).

   Output is bracketed with single spaces so it doesn't touch the
   horizontal border characters."
  [{:keys [messages loading?]} inner-w]
  (let [info        (chosen-model-info)
        model       (:name info)
        reasoning?  (boolean (:reasoning? info))
        ctx-max     (when model
                      (try (router/context-limit model) (catch Throwable _ nil)))
        ;; Estimated next-query input tokens. Priority order:
        ;;   1) prior turn's actual :input tokens  (most accurate)
        ;;   2) ~4 chars/token over current chat   (fallback first turn)
        ;; The estimate is what svar will most likely send for the
        ;; *next* request, so labelling it "next" is honest.
        last-tok    (last-assistant-tokens messages)
        next-tok    (or (:input last-tok)
                      (let [chars (estimate-next-context-chars messages)]
                        (when (pos? chars) (long (/ chars 4)))))
        ctx-segment (format-ctx-segment next-tok ctx-max)
        ;; Last turn's output (only meaningful between turns, not while
        ;; the next one is in flight).
        out-str     (format-token-count (:output last-tok))
        ;; Default reasoning level only shows when the model actually
        ;; supports reasoning — otherwise it's misleading noise.
        reasoning-segment (when reasoning?
                            (str "reasoning: " (name default-reasoning-level)))
        ;; Segments in priority order: model first, ctx utilization,
        ;; reasoning, then last turn's output for context. The shrink
        ;; loop drops from the tail when the line gets too wide.
        segments (cond-> []
                   model             (conj (str "model: " model))
                   ctx-segment       (conj (str "ctx: " ctx-segment))
                   reasoning-segment (conj reasoning-segment)
                   (and (not loading?) out-str)
                   (conj (str "last out: " out-str)))
        budget   (max 0 (- inner-w 2))]
    (loop [parts segments]
      (cond
        (empty? parts) nil
        :else
        (let [s (str " " (str/join "  ·  " parts) " ")]
          (if (<= (count s) budget)
            s
            (recur (vec (butlast parts)))))))))

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
  [^TerminalScreen screen]
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
   that carry a :trace, and replace the live placeholder with progress text.
   This runs every frame so toggling settings is immediately reactive.

   `progress-extra` is forwarded to `render/progress->text` so the live
   status line can show elapsed time and a cancelling marker."
  [messages progress loading? bubble-w settings progress-extra]
  (let [;; Apply trace→text projection and markdown to assistant messages
        projected (mapv (fn [msg]
                          (cond
                            ;; Has trace: full iteration + answer rendering
                            (and (= :assistant (:role msg)) (:trace msg))
                            (assoc msg :text
                              (render/format-answer-with-thinking
                                (:raw-answer msg) (:trace msg) bubble-w settings
                                (:confidence msg)))
                            ;; Plain assistant message: apply markdown
                            (= :assistant (:role msg))
                            (assoc msg :text
                              (render/format-answer-markdown (:text msg) bubble-w))
                            ;; User messages: unchanged
                            :else msg))
                    messages)]
    ;; Replace loading placeholder with live progress
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

(defn- short-id
  [id]
  (when-let [s (some-> id str)]
    (subs s 0 (min 8 (count s)))))

(defn- render-frame!
  "Draw one frame: background, messages area (bubbles), input box.
   The input box's top border carries keybinding hints and the bottom
   border carries the live model/context status line.

   Returns the layout map `{:total-h, :inner-h, :cols, :rows}` so the
   render thread can publish it back into app-db for the input thread's
   scroll handlers. `apply-settings` runs ONCE here and feeds both the
   layout calculation and the actual draw — the old code path computed
   it twice per frame, which doubled cost on long traces."
  [^TerminalScreen screen cols rows
   {:keys [messages msg-scroll input progress loading? cancelling?
           query-start-ms title settings conv] :as db}]
  (let [g           (.newTextGraphics screen)
        text-rows   (input-text-rows input)
        input-box-h (+ text-rows 2 (* 2 render/input-pad-y))
        input-top   (- rows input-box-h)
        msg-top     0
        msg-bottom  input-top
        bubble-w    (- cols 4)
        progress-extra {:query-start-ms query-start-ms
                        :cancelling?    (boolean cancelling?)}
        effective-messages (apply-settings messages progress loading? bubble-w settings progress-extra)
        sid         (short-id (get conv :id))
        display-title (when-not (str/blank? title)
                        (if sid
                          (str title " · " sid)
                          title))
        status-line (input-status-line db (max 0 (- cols 2)))
        ;; Inner messages-box height — mirrors `draw-messages-area!`'s
        ;; computation so scroll math in the input thread stays correct.
        inner-h     (max 0 (- msg-bottom msg-top 1 2)) ;; -1 border, -2 margins
        total-h     (render/total-messages-height effective-messages bubble-w)]
    (render/fill-background! g cols rows)
    (render/draw-messages-area! g effective-messages msg-top msg-bottom cols msg-scroll {:title display-title})
    (let [[cx cy] (render/draw-input-box! g input input-top text-rows cols
                    (current-hint db) status-line)]
      (.setCursorPosition screen (TerminalPosition. cx cy)))
    (.refresh screen Screen$RefreshType/DELTA)
    {:cols cols :rows rows :total-h total-h :inner-h inner-h}))

;;; ── Render thread ───────────────────────────────────────────────────────────────

(defn- render-loop!
  "The render thread's main loop. Sleeps on `state/render-monitor` and
   only paints when `:render-version` advances (or the terminal got
   resized). Skips painting entirely while a dialog is up by failing to
   acquire `draw-lock`."
  [^TerminalScreen screen]
  (loop [last-v -1 last-cols -1 last-rows -1]
    (let [db @state/app-db]
      (when-not (:shutdown? db)
        (let [version (long (or (:render-version db) 0))
              ;; tryLock so a dialog session (which holds the lock for
              ;; seconds) doesn't pin us. Time out fast and re-poll.
              got-lock? (.tryLock draw-lock 50 TimeUnit/MILLISECONDS)
              [rendered? new-cols new-rows]
              (if-not got-lock?
                [false last-cols last-rows]
                (try
                  ;; Re-read AFTER acquiring the lock — dialog state
                  ;; could have flipped while we were waiting.
                  (let [db   @state/app-db
                        size (screen-size screen)
                        cols (.getColumns size)
                        rows (.getRows size)]
                    (if (and (not (:shutdown? db))
                          (not (:dialog-open? db))
                          (or (not= last-v version)
                            (not= last-cols cols)
                            (not= last-rows rows)))
                      (let [layout (render-frame! screen cols rows db)]
                        ;; Publish layout back to app-db without
                        ;; bumping the version (see no-render-bump-events).
                        (state/dispatch [:set-layout layout])
                        [true cols rows])
                      [false cols rows]))
                  (catch Throwable t
                    ;; Drawing must never crash the thread — a stray
                    ;; resize race or null cell will recover next frame.
                    (try (require 'taoensso.telemere)
                      ((resolve 'taoensso.telemere/log!)
                        :warn (str "render frame failed: " (or (ex-message t) (str t))))
                      (catch Throwable _ nil))
                    [false last-cols last-rows])
                  (finally (.unlock draw-lock))))]
          (when-not rendered?
            ;; Park until the next dispatch wakes us. The 100ms ceiling
            ;; is just defensive — every state mutation calls
            ;; notifyAll, so we should never actually time out except
            ;; while a dialog is up (in which case the tryLock 50ms
            ;; backoff also kicks in on the next iteration).
            ;;
            ;; The re-check while holding the monitor is the standard
            ;; lost-wakeup guard: a dispatch that called notifyAll
            ;; between our outer version read and acquiring the
            ;; monitor would otherwise be missed and we'd sit out the
            ;; full 100ms timeout.
            (locking state/render-monitor
              (let [v-now (long (or (:render-version @state/app-db) 0))]
                (when (= v-now version)
                  (try (.wait state/render-monitor 100)
                    (catch InterruptedException _ nil))))))
          (recur (if rendered? version last-v)
            (long (or new-cols last-cols))
            (long (or new-rows last-rows))))))))

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

(defn run-chat!
  "Start the fullscreen chat TUI. Blocks until user quits.
   Optional `opts` map:
     :conversation-id uuid-string — resume a specific conversation
     :resume          true        — resume the latest :vis conversation"
  ([] (run-chat! {}))
  ([opts]
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
      (when-let [config (:config @state/app-db)]
        (let [{:keys [id history]}
              (if-let [cid (:conversation-id opts)]
                (or (chat/resume-conversation cid)
                    (throw (ex-info (str "Conversation not found: " cid) {:id cid})))
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
        (let [db    @state/app-db
              ;; Layout fields are populated by the render thread after
              ;; the first paint. Until then, scroll handlers fall back
              ;; to safe defaults and act as a no-op.
              {:keys [total-h inner-h]} (:layout db)
              total-h (or total-h 0)
              inner-h (or inner-h 0)]

          ;; Pure poll — no rendering on this thread anymore. The
          ;; render thread handles all screen output.
          (let [key (.pollInput screen)]
            (if (nil? key)
              (do (Thread/sleep 16) (recur))
              (let [{:keys [action state col row]} (input/handle-key key (:input db))]
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

                  :continue (recur))))))))
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
        (.stopScreen screen))))))

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
  (try
    (run-chat! (parse-args args))
    (catch Throwable t
      (.println config/original-stdout (str "vis: fatal error — " (.getMessage t)))
      (.printStackTrace t (java.io.PrintStream. @config/tty-out true))
      (throw t))
    (finally
      (config/shutdown!))))

;;; ── Channel registration (auto-discovered via META-INF/vis/channels.edn) ──

(channel/register-global!
  {:channel/id        :tui
   :channel/cmd       "tui"
   :channel/doc       "Interactive terminal UI."
   :channel/usage     "vis tui [--conversation-id ID | --resume]"
   :channel/owns-tty? true
   :channel/main-fn   #'channel-main})
