(ns com.blockether.vis.tui.dialogs
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna TextColor$RGB]
           [com.googlecode.lanterna.input KeyType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]))

;;; ── Shared dialog chrome & components ───────────────────────────────────────

(def ^:private input-field-bg (TextColor$RGB. 255 255 252))

(defn clear-screen!
  "Fill the entire screen with terminal background. Call before sub-dialogs
   to cleanly replace the current dialog (wizard step pattern)."
  [^TerminalScreen screen]
  (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
        cols (.getColumns size)
        rows (.getRows size)
        g    (.newTextGraphics screen)]
    (p/set-bg! g t/terminal-bg)
    (p/fill-rect! g 0 0 cols rows)
    (.refresh screen Screen$RefreshType/DELTA)))

(defn clamp [x lo hi]
  (max lo (min hi x)))

(defn ellipsize [s max-w]
  (let [txt (or s "")]
    (cond
      (<= max-w 0) ""
      (<= (count txt) max-w) txt
      (= max-w 1) "…"
      :else (str (subs txt 0 (dec max-w)) "…"))))

(defn dialog-layout
  "Compute content area layout. When `content-count` is provided and smaller than
   the available height, content is vertically centered within the frame.
   Layout: border → title bar → top separator → CONTENT → bottom separator → hint → border."
  ([bounds] (dialog-layout bounds nil))
  ([{:keys [top bottom]} content-count]
   (let [raw-top      (+ top 3)
         hint-row     (- bottom 1)
         bot-sep-row  (- bottom 2)
         content-bot  (dec bot-sep-row)
         full-h      (max 1 (inc (- content-bot raw-top)))
         v-offset    (if (and content-count (< content-count full-h))
                       (quot (- full-h content-count) 2)
                       0)
         content-top (+ raw-top v-offset)
         ;; Usable height from centered top — never exceeds content-bot
         content-h   (max 1 (inc (- content-bot content-top)))]
     {:content-top content-top
      :content-bottom content-bot
      :content-h content-h
      :hint-row hint-row})))

(defn visible-window-start
  [idx current-start visible-count total-count]
  (let [last-start (max 0 (- total-count visible-count))
        start      (clamp current-start 0 last-start)]
    (cond
      (< idx start) idx
      (>= idx (+ start visible-count)) (max 0 (- idx (dec visible-count)))
      :else start)))

(defn draw-hint-bar!
  "Draw hint bar. `hint` can be:
   - a string: rendered as-is
   - a vec of strings: spread with space-between
   - a vec of [key action] pairs: key in italic, action normal, spread with space-between
   Examples:
     \"simple hint\"
     [\"↑/↓ move\" \"Enter select\" \"Esc cancel\"]
     [[\"↑/↓\" \"move\"] [\"Enter\" \"select\"] [\"Esc\" \"cancel\"]]"
  [g left row inner-w hint]
  (let [text-w  (max 0 (- inner-w 2))
        text-x  (+ left 2)]
    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (cond
      ;; Plain string
      (string? hint)
      (p/put-str! g text-x row (ellipsize hint text-w))

      ;; Vec of [key action] pairs — render keys in italic
      (and (vector? hint) (seq hint) (vector? (first hint)))
      (let [labels    (mapv (fn [[k a]] (str k " " a)) hint)
            positions (p/space-between labels text-w)
            ;; Walk through the laid-out string to find each key's position
            ;; Simpler: compute column offsets from space-between math
            n         (count labels)
            total-txt (reduce + (map count labels))
            total-gap (- text-w total-txt)
            gap-count (max 1 (dec n))
            base-gap  (max 1 (quot total-gap gap-count))
            extra     (- total-gap (* base-gap gap-count))]
        (loop [i 0 col text-x]
          (when (< i n)
            (let [[k a]   (nth hint i)
                  gap     (if (< i (dec n))
                            (+ base-gap (if (< i extra) 1 0))
                            0)]
              ;; Key part — bold, stronger color
              (p/set-fg! g t/dialog-hint-key)
              (p/styled g [p/BOLD]
                        (p/put-str! g col row k))
              ;; Action part — normal hint color, italic
              (p/set-fg! g t/dialog-hint)
              (p/styled g [p/ITALIC]
                        (p/put-str! g (+ col (count k)) row (str " " a)))
              (recur (inc i) (+ col (count k) 1 (count a) gap))))))

      ;; Vec of strings — space-between, all italic
      (vector? hint)
      (p/styled g [p/ITALIC]
                (p/draw-space-between! g text-x row text-w hint)))))

(defn- draw-list-item!
  [g left row inner-w selected? label]
  (let [prefix    (if selected? "▸ " "  ")
        draw-text (ellipsize (str prefix label) (max 0 (- inner-w 2)))]
    (if selected?
      (p/set-colors! g t/dialog-bg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/fill-rect! g (inc left) row inner-w 1)
    (p/put-str! g (+ left 2) row draw-text)))

(defn- draw-checkbox-item!
  [g left row inner-w selected? checked? label]
  (let [mark      (if checked? "✓" " ")
        prefix    (str "[" mark "] ")
        draw-text (ellipsize (str prefix label) (max 0 (- inner-w 2)))]
    (if selected?
      (p/set-colors! g t/dialog-bg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/fill-rect! g (inc left) row inner-w 1)
    (p/put-str! g (+ left 2) row draw-text)))

(defn- draw-text-input-field!
  [g left row inner-w text cursor]
  (let [field-left (+ left 2)
        field-w    (max 1 (- inner-w 2))
        h-off      (max 0 (- cursor (dec field-w)))
        visible    (subs text h-off (min (count text) (+ h-off field-w)))]
    (p/set-colors! g t/box-fg input-field-bg)
    (p/fill-rect! g field-left row field-w 1)
    (p/put-str! g field-left row visible)
    (p/cursor-pos (+ field-left (- cursor h-off)) row)))

(defn draw-dialog-chrome!
  "Draw dialog background, shadow, border, and title.
   `content-w` and `content-h` define the content area — the dialog sizes to
   fit them while maintaining golden ratio proportions.
   Returns {:left :top :right :bottom :inner-w :inner-h}."
  [g cols rows title content-w content-h]
  (let [[box-w box-h] (render/golden-dialog-size cols rows content-w content-h)
        box-left      (quot (- cols box-w) 2)
        box-top       (quot (- rows box-h) 2)
        box-right     (+ box-left box-w -1)
        box-bottom    (+ box-top box-h -1)
        inner-w       (- box-w 2)]

    ;; Shadow — clipped to terminal bounds
    (let [shd-left (+ box-left 2)
          shd-top  (inc box-top)
          shd-w    (min box-w (- cols shd-left))
          shd-h    (min box-h (- rows shd-top))]
      (when (and (pos? shd-w) (pos? shd-h))
        (p/set-bg! g t/dialog-shadow)
        (p/fill-rect! g shd-left shd-top shd-w shd-h)))

    ;; Background
    (p/set-bg! g t/dialog-bg)
    (p/fill-rect! g box-left box-top box-w box-h)

    ;; Border
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/draw-box! g box-left box-top box-w box-h)

    ;; Title bar — full-width accent stripe with centered title
    (let [title-row  (inc box-top)
          title-text (ellipsize (or title "") (max 0 (- inner-w 2)))
          tx         (+ box-left 1 (quot (- inner-w (count title-text)) 2))]
      ;; Accent bar background
      (p/set-bg! g t/dialog-title-bg)
      (p/fill-rect! g (inc box-left) title-row inner-w 1)
      ;; Title text
      (p/set-fg! g t/dialog-title-fg)
      (p/put-str! g tx title-row title-text)
      ;; Top separator — below title bar
      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/draw-separator! g box-left box-right (inc title-row))
      ;; Bottom separator — above hint bar
      (let [bot-sep (- box-bottom 2)]
        (when (> bot-sep (+ box-top 3))
          (p/draw-separator! g box-left box-right bot-sep))))

    {:left box-left :top box-top :right box-right :bottom box-bottom
     :inner-w inner-w :inner-h (- box-h 2)}))

;;; ── Selection dialog ────────────────────────────────────────────────────────

(defn select-dialog!
  "Show a selection list dialog. Returns selected item map or nil on Esc.
   `items` is a vec of {:label str, ...} maps."
  [^TerminalScreen screen title items]
  (let [selected  (atom 0)
        scroll    (atom 0)
        max-label (apply max 1 (map (comp count :label) items))
        cw        (max (+ max-label 6) (+ (count title) 4))
        ch        (count items)]
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title cw ch)
            {:keys [left inner-w]} bounds
            total  (count items)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
            visible (min total content-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % content-h total))]

        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]
            (when (< idx total)
              (draw-list-item! g left row inner-w (= idx @selected)
                               (:label (nth items idx))))))

        (draw-hint-bar! g left hint-row inner-w [["↑/↓" "move"] ["Enter" "select"] ["Esc" "cancel"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape    nil
              KeyType/ArrowUp   (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
              KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
              KeyType/Enter     (when (pos? total) (nth items @selected))
              (recur))))))))

;;; ── Text input dialog ───────────────────────────────────────────────────────

(defn text-input-dialog!
  "Show a text input dialog. Returns string or nil on Esc.
   Options: :mask char (e.g. \\* for passwords), :initial string."
  [^TerminalScreen screen title label & {:keys [mask initial] :or {initial ""}}]
  (let [text   (atom (vec initial))
        cursor (atom (count initial))
        cw     (max (count label) (count title) 30)
        ch     2]
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title cw ch)
            {:keys [left inner-w]} bounds
            {:keys [content-top hint-row]} (dialog-layout bounds 2)
            label-row  content-top
            input-row  (inc content-top)
            txt        (apply str @text)
            display    (if mask (apply str (repeat (count txt) mask)) txt)
            cursor-pos (draw-text-input-field! g left input-row inner-w display @cursor)]

        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc left) label-row inner-w 1)
        (p/put-str! g (+ left 2) label-row (ellipsize label (max 0 (- inner-w 2))))

        (draw-hint-bar! g left hint-row inner-w [["←/→" "move"] ["Enter" "confirm"] ["Esc" "cancel"]])
        (.setCursorPosition screen cursor-pos)
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape nil
              KeyType/Enter  (apply str @text)

              KeyType/Character
              (let [c (.getCharacter key)]
                (swap! text #(into (subvec % 0 @cursor) (cons c (subvec % @cursor))))
                (swap! cursor inc)
                (recur))

              KeyType/Backspace
              (do (when (pos? @cursor)
                    (swap! text #(into (subvec % 0 (dec @cursor)) (subvec % @cursor)))
                    (swap! cursor dec))
                  (recur))

              KeyType/ArrowLeft  (do (swap! cursor #(max 0 (dec %))) (recur))
              KeyType/ArrowRight (do (swap! cursor #(min (count @text) (inc %))) (recur))
              (recur))))))))

;;; ── Confirm dialog ──────────────────────────────────────────────────────────

(defn- draw-button!
  "Draw a button label. Selected = accent bg, normal = dialog bg."
  [g col row label selected?]
  (let [text (str " " label " ")
        w    (count text)]
    (if selected?
      (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/put-str! g col row text)
    w))

(defn confirm-dialog!
  "Show Y/N confirmation with side-by-side buttons. Returns true/false, nil on Esc."
  [^TerminalScreen screen title message]
  (let [raw-lines  (if (string? message) [message] message)
        max-line-w (apply max 1 (map count raw-lines))
        btn-yes    "Yes"
        btn-no     "No"
        btn-w      (+ 2 (max (count btn-yes) (count btn-no))) ;; " Yes " / " No  "
        btn-gap    4
        cw         (max (+ btn-w btn-gap btn-w 4) max-line-w (+ (count title) 4))
        ;; content: message lines + blank + button row = lines + 2
        ch         (+ (count raw-lines) 2)
        focus      (atom 0)] ;; 0 = Yes, 1 = No
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title cw ch)
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds ch)
            text-w (max 0 (- inner-w 2))
            lines  (vec (mapcat #(render/wrap-text % text-w) raw-lines))
            btn-row (+ content-top (count lines) 1) ;; blank line then buttons
            ;; Center buttons horizontally
            total-btn-w (+ btn-w btn-gap btn-w)
            btn-start   (+ left 1 (quot (- inner-w total-btn-w) 2))]

        ;; Message text — centered per line
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[i line] (map-indexed vector lines)]
          (let [row (+ content-top i)]
            (when (< row (+ content-top content-h))
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/draw-centered! g (inc left) row inner-w line))))

        ;; Buttons — side by side
        (p/set-bg! g t/dialog-bg)
        (p/fill-rect! g (inc left) btn-row inner-w 1)
        (draw-button! g btn-start btn-row btn-yes (= @focus 0))
        (draw-button! g (+ btn-start btn-w btn-gap) btn-row btn-no (= @focus 1))

        (draw-hint-bar! g left hint-row inner-w [["←/→" "switch"] ["Enter" "confirm"] ["Esc" "cancel"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape     nil
              KeyType/Enter      (= @focus 0) ;; true if Yes focused
              KeyType/ArrowLeft  (do (reset! focus 0) (recur))
              KeyType/ArrowRight (do (reset! focus 1) (recur))
              KeyType/Tab        (do (swap! focus #(if (zero? %) 1 0)) (recur))
              KeyType/Character
              (let [c (Character/toLowerCase (.getCharacter key))]
                (cond (= c \y) true (= c \n) false :else (recur)))
              (recur))))))))

;;; ── Copy dialog ─────────────────────────────────────────────────────────────

(defn- role-label [role] (name (or role :assistant)))

(defn- message-preview [{:keys [role text]}]
  (str (role-label role) ": "
       (-> (or text "") (str/replace #"\r?\n+" " ") str/trim)))

(defn- format-selected-messages [messages selected]
  (->> (range (count messages))
       (filter #(contains? selected %))
       (map (fn [idx]
              (let [{:keys [role text]} (nth messages idx)]
                (str (role-label role) ": " (or text "")))))
       (str/join "\n\n")))

(defn- copy-last-assistant! [messages]
  (when-let [m (last (filter #(= (:role %) :assistant) messages))]
    (input/clipboard-copy! (or (:text m) ""))
    true))

(defn copy-dialog!
  "Show copy dialog for chat messages.
   Space toggles, A toggles all, Enter copies selected, Ctrl+Y copies last assistant, Esc cancels."
  [^TerminalScreen screen messages]
  (let [items       (vec messages)
        selected    (atom 0)
        scroll      (atom 0)
        checked     (atom #{})
        max-preview (apply max 1 (map (comp count :text) items))
        cw          (max (+ 4 (min max-preview 50)) (+ (count "Copy Messages") 4))
        ch          (count items)]
    (loop [status [["Space" "toggle"] ["A" "all"] ["Enter" "copy"] ["^Y" "last"] ["Esc" "cancel"]]]
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows "Copy Messages" cw ch)
            {:keys [left inner-w]} bounds
            total  (count items)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
            visible (min total content-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % content-h total))]

        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]
            (when (< idx total)
              (draw-checkbox-item! g left row inner-w (= idx @selected)
                                   (contains? @checked idx)
                                   (message-preview (nth items idx))))))

        (draw-hint-bar! g left hint-row inner-w status)
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (let [ktype (.getKeyType key)]
              (condp = ktype
                KeyType/Escape nil

                KeyType/ArrowUp
                (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total))))
                    (recur status))

                KeyType/ArrowDown
                (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                    (recur status))

                KeyType/Character
                (let [c    (Character/toLowerCase (.getCharacter key))
                      ctrl (.isCtrlDown key)]
                  (cond
                    (and ctrl (= c \y))
                    (if (copy-last-assistant! items) true
                        (recur "No assistant response available"))

                    (= c \space)
                    (do (when (pos? total)
                          (swap! checked (fn [s] (if (contains? s @selected)
                                                   (disj s @selected)
                                                   (conj s @selected)))))
                        (recur status))

                    (= c \a)
                    (do (swap! checked (fn [s] (if (= (count s) total) #{} (set (range total)))))
                        (recur status))

                    :else (recur status)))

                KeyType/Enter
                (let [payload (format-selected-messages items @checked)]
                  (if (seq payload)
                    (do (input/clipboard-copy! payload) true)
                    (recur "No messages selected")))

                (recur status)))))))))
