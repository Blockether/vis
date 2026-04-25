(ns com.blockether.vis.channels.tui.screen
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.chat :as chat]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.channels.tui.dialogs :as dlg]
            [com.blockether.vis.channels.tui.input :as input]
            [com.blockether.vis.channels.tui.provider :as provider]
            [com.blockether.vis.channels.tui.render :as render]
            [com.blockether.vis.channels.tui.state :as state]
            [com.blockether.vis.loop.runtime.conversation.core :as conversations])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
           [com.googlecode.lanterna.terminal MouseCaptureMode]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]
           [java.nio.charset Charset]))

(def ^:private input-min-lines 3)
(def ^:private input-max-lines 8)
(def ^:private hint " Enter send · Alt+Enter newline · Ctrl+P provider · Ctrl+C quit ")

(defn- with-dialog-lock
  [f]
  (state/dispatch [:set-dialog-open true])
  (try
    (f)
    (finally
      (state/dispatch [:set-dialog-open false]))))

(defn- screen-size [^TerminalScreen screen]
  (if-let [new-size (.doResizeIfNecessary screen)]
    (do (.refresh screen Screen$RefreshType/COMPLETE)
        new-size)
    (.getTerminalSize screen)))

(defn- messages-with-progress
  "When the RLM is mid-query, replace the assistant placeholder bubble's
   text with a live per-iteration progress timeline built from
   `progress`. The placeholder is always the last message in the vec (added
   by the :send-message handler). On non-loading frames, returns messages
   unchanged.

   This keeps the renderer pure — it never sees the progress atom, only a
   derived `:text` field on the last bubble."
  [messages progress loading? bubble-w]
  (if (and loading? (seq messages))
    (let [last-idx (dec (count messages))
          last-msg (get messages last-idx)]
      (if (= :assistant (:role last-msg))
        (assoc messages last-idx
          (assoc last-msg :text (render/progress->text progress bubble-w)))
        messages))
    messages))

(defn- input-text-rows
  "Compute visible text rows for the input box based on content."
  [{:keys [lines]}]
  (let [n (count lines)]
    (min input-max-lines (max input-min-lines n))))

(defn- render-frame!
  "Draw one frame: background, messages area (bubbles), input box."
  [screen g cols rows {:keys [messages msg-scroll input progress loading? title]}]
  (let [text-rows   (input-text-rows input)
        input-box-h (+ text-rows 2 (* 2 render/input-pad-y))
        input-top   (- rows input-box-h)
        msg-top     0
        msg-bottom  input-top
        bubble-w    (- cols 4)
        effective-messages (messages-with-progress messages progress loading? bubble-w)]
    (render/fill-background! g cols rows)
    (render/draw-messages-area! g effective-messages msg-top msg-bottom cols msg-scroll {:title title})
    (let [[cx cy] (render/draw-input-box! g input input-top text-rows cols hint)]
      (.setCursorPosition screen (TerminalPosition. cx cy)))
    (.refresh screen Screen$RefreshType/DELTA)))

(defn run-chat!
  "Start the fullscreen chat TUI. Blocks until user quits.
   Optional `opts` map: {:conversation-id uuid-string} to resume."
  ([] (run-chat! {}))
  ([opts]
  (state/init!)

  ;; Load persisted config
  (when-let [c (config/load-config)]
    (state/dispatch [:set-config c]))

  (let [terminal (UnixTerminal. @config/tty-in @config/tty-out (Charset/defaultCharset))
        _        (input/register-custom-patterns! terminal)
        screen   (TerminalScreen. terminal)]
    (.startScreen screen)
    (.setMouseCaptureMode terminal MouseCaptureMode/CLICK_RELEASE)
    (try
      ;; Show provider dialog on first launch if no config
      (when-not (:config @state/app-db)
        (when (not (:dialog-open? @state/app-db))
          (when-let [c (with-dialog-lock #(provider/show-provider-dialog! screen (:config @state/app-db)))]
            (state/dispatch [:set-config c]))))

      ;; Init conversation: resume if --conversation-id given, else fresh.
      (when-let [config (:config @state/app-db)]
        (let [{:keys [id history]}
              (if-let [cid (:conversation-id opts)]
                (or (chat/resume-conversation cid)
                    (throw (ex-info (str "Conversation not found: " cid) {:id cid})))
                (chat/make-conversation config))
              ;; Set title from DB if resuming, else nil (auto-set on first turn)
              conv-info (when-let [c (conversations/by-id id)] c)
              title     (when conv-info (:title conv-info))]
          (state/dispatch [:init-conversation {:id id} history])
          (when (and title (not (str/blank? title)))
            (state/dispatch [:set-title title]))
          (channels/register-conversation-shutdown-hook! id)))

      (loop []
        (let [db    @state/app-db
              size  (screen-size screen)
              cols  (.getColumns size)
              rows  (.getRows size)
              g     (.newTextGraphics screen)
              bubble-w (- cols 4)
              text-rows (input-text-rows (:input db))
              inner-h  (- (- rows (+ text-rows 2 (* 2 render/input-pad-y))) 0 1)
              ;; Scroll math must account for the (possibly expanded)
              ;; progress placeholder — otherwise mid-stream the bubble
              ;; grows off-screen and the user can't scroll down to it.
              displayed-messages (messages-with-progress
                                   (:messages db) (:progress db) (:loading? db) bubble-w)
              total-h  (render/total-messages-height displayed-messages bubble-w)]

          (render-frame! screen g cols rows db)

          ;; Always poll (non-blocking) so terminal resize is detected immediately.
          (let [key (.pollInput screen)]
            (if (nil? key)
              (do (Thread/sleep 16) (recur))
              (let [{:keys [action state]} (input/handle-key key (:input db))]
                (state/dispatch [:update-input state])
                (case action
                  :quit nil

                  :show-provider
                  (if (:dialog-open? @state/app-db)
                    (recur)
                    (do
                      (when-let [c (with-dialog-lock #(provider/show-provider-dialog! screen (:config @state/app-db)))]
                        (state/dispatch [:set-config c]))
                      (recur)))

                  :show-copy
                  (if (:dialog-open? @state/app-db)
                    (recur)
                    (do (with-dialog-lock #(dlg/copy-dialog! screen (:messages @state/app-db)))
                      (recur)))

                  :send
                  (let [text (input/input->text state)]
                    (state/dispatch [:reset-input])
                    (when (and (seq (str/trim text))
                            (:conv @state/app-db)
                            (not (:loading? @state/app-db)))
                      (state/dispatch [:send-message text]))
                    (recur))

                  :scroll-up
                  (do (state/dispatch [:scroll-up 3 total-h inner-h])
                    (recur))

                  :scroll-down
                  (do (state/dispatch [:scroll-down 3 total-h inner-h])
                    (recur))

                  :continue (recur)))))))
      (finally
        (when-let [conv (:conv @state/app-db)]
          (chat/dispose! conv))
        (.setMouseCaptureMode terminal nil)
        (.stopScreen screen))))))
