(ns com.blockether.vis.adapters.tui.screen
  (:require [clojure.string :as str]
            [com.blockether.vis.adapters.tui.chat :as chat]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.adapters.tui.dialogs :as dlg]
            [com.blockether.vis.adapters.tui.input :as input]
            [com.blockether.vis.adapters.tui.provider :as provider]
            [com.blockether.vis.adapters.tui.render :as render]
            [com.blockether.vis.adapters.tui.state :as state])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
           [com.googlecode.lanterna.terminal MouseCaptureMode]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]
           [java.nio.charset Charset]))

(def ^:private input-height 5)
(def ^:private hint " Enter send · Alt+Enter newline · Ctrl+P provider · Ctrl+C quit ")

(defn- with-dialog-lock
  [f]
  (state/dispatch [:set-dialog-open true])
  (try
    (f)
    (finally
      (state/dispatch [:set-dialog-open false]))))

(defn- screen-size [^TerminalScreen screen]
  (or (.doResizeIfNecessary screen)
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

(defn- render-frame!
  "Draw one frame: background, messages area (bubbles), input box."
  [screen g cols rows {:keys [messages msg-scroll input progress loading?]}]
  (let [input-box-h (+ input-height 2)
        input-top   (- rows input-box-h)
        msg-top     0
        msg-bottom  input-top
        bubble-w    (- cols 4)
        effective-messages (messages-with-progress messages progress loading? bubble-w)]
    (render/fill-background! g cols rows)
    (render/draw-messages-area! g effective-messages msg-top msg-bottom cols msg-scroll)
    (let [[cx cy] (render/draw-input-box! g input input-top input-height cols hint)]
      (.setCursorPosition screen (TerminalPosition. cx cy)))
    (.refresh screen Screen$RefreshType/DELTA)))

(defn run-chat!
  "Start the fullscreen chat TUI. Blocks until user quits."
  []
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

      ;; Init conversation with config — fresh :vis conversation each boot.
      (when-let [config (:config @state/app-db)]
        (let [{:keys [id history]} (chat/make-conversation config)]
          (state/dispatch [:init-conversation {:id id} history])))

      (loop []
        (let [db    @state/app-db
              size  (screen-size screen)
              cols  (.getColumns size)
              rows  (.getRows size)
              g     (.newTextGraphics screen)
              bubble-w (- cols 4)
              inner-h  (- (- rows (+ input-height 2)) 0 1)
              ;; Scroll math must account for the (possibly expanded)
              ;; progress placeholder — otherwise mid-stream the bubble
              ;; grows off-screen and the user can't scroll down to it.
              displayed-messages (messages-with-progress
                                   (:messages db) (:progress db) (:loading? db) bubble-w)
              total-h  (render/total-messages-height displayed-messages bubble-w)]

          (render-frame! screen g cols rows db)

          ;; pollInput while loading (non-blocking), readInput otherwise
          (let [key (if (:loading? db)
                      (.pollInput screen)
                      (.readInput screen))]
            (if (nil? key)
              ;; No input — sleep briefly and re-render (keeps UI alive during loading)
              (do (Thread/sleep 50) (recur))
              (let [{:keys [action state]} (input/handle-key key (:input db))]
                (state/dispatch [:update-input state])
                (case action
                  :quit nil

                  :show-provider
                  (if (:dialog-open? @state/app-db)
                    (recur)
                    (do
                      (when-let [c (with-dialog-lock #(provider/show-provider-dialog! screen (:config @state/app-db)))]
                        (state/dispatch [:set-config c])
                        (let [{:keys [id history]} (chat/make-conversation c)]
                          (state/dispatch [:init-conversation {:id id} history])))
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
        (.stopScreen screen)))))
