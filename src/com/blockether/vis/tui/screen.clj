(ns com.blockether.vis.tui.screen
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.logging :as logging]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.provider :as provider]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.state :as state])
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

(defn- render-frame!
  "Draw one frame: background, messages area (bubbles), input box."
  [screen g cols rows {:keys [messages msg-scroll input]}]
  (let [input-box-h (+ input-height 2)
        input-top   (- rows input-box-h)
        msg-top     0
        msg-bottom  input-top]
    (render/fill-background! g cols rows)
    (render/draw-messages-area! g messages msg-top msg-bottom cols msg-scroll)
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

  (let [terminal (UnixTerminal. @logging/tty-in @logging/tty-out (Charset/defaultCharset))
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

      ;; Init conversation with config
      (when-let [config (:config @state/app-db)]
        (let [{:keys [env history]} (chat/make-conversation config)]
          (state/dispatch [:init-conversation {:env env} history])))

      (loop []
        (let [db    @state/app-db
              size  (screen-size screen)
              cols  (.getColumns size)
              rows  (.getRows size)
              g     (.newTextGraphics screen)
              bubble-w (- cols 4)
              inner-h  (- (- rows (+ input-height 2)) 0 1)
              total-h  (render/total-messages-height (:messages db) bubble-w)]

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
                        (let [{:keys [env history]} (chat/make-conversation c)]
                          (state/dispatch [:init-conversation {:env env} history])))
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
