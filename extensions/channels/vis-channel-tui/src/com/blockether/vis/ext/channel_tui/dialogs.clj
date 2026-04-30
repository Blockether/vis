(ns com.blockether.vis.ext.channel-tui.dialogs
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.theme :as t])
  (:import [com.googlecode.lanterna TextColor$RGB]
           [com.googlecode.lanterna Symbols]
           [com.googlecode.lanterna.input KeyType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]))

;;; ── Shared dialog chrome & components ───────────────────────────────────────

(def ^:private input-field-bg (TextColor$RGB. 255 255 252))

;;; ── Default modal footprint ─────────────────────────────────────────────────
;;
;; Every modal in the TUI shares ONE default WIDTH (terminal-proportional,
;; clamped 50–100 cols). HEIGHT is content-driven and grows only as far
;; as the body needs, with a small floor (`DEFAULT_DIALOG_MIN_HEIGHT`,
;; box rows). Rationale: an ESC-stack of nested dialogs reads more cleanly
;; when each layer lines up at the same column footprint, but pinning a
;; one-size-fits-all height (70% of terminal) wiped out most of the chat
;; underneath every popup — even a 4-item palette painted as a giant
;; box. Flexible height keeps small dialogs small (chat stays visible)
;; while letting big ones (text viewer, long settings) still use the
;; vertical room they actually need.
;;
;; Every dialog passes its own `content-h` to `draw-dialog-chrome!`'s
;; default-width arity. The chrome floors that against `MIN_HEIGHT` so
;; tiny content (1-line confirm) still renders in a deliberate-feeling
;; box, not a squashed 5-row one.

(def ^:const DEFAULT_DIALOG_WIDTH_RATIO
  "Fraction of terminal width every modal occupies by default."
  0.7)

(def ^:const DEFAULT_DIALOG_MIN_WIDTH 50)
(def ^:const DEFAULT_DIALOG_MAX_WIDTH 100)

(def ^:const DEFAULT_DIALOG_MIN_HEIGHT
  "Minimum dialog box height in rows. Floor so a 1-line confirm, the
   API-key text input, and the router/model picker all render at the
   same deliberate-feeling size. 12 rows = title bar (1) + separator
   (1) + body (~7) + separator (1) + hint (1) + borders (2). Below
   this the chrome eats the body and small dialogs look squashed next
   to each other."
  14)

(def ^:const DIALOG_CHROME_W 4)  ;; border(1) + pad(1) each side
(def ^:const DIALOG_CHROME_H 6)  ;; top + title + sep + body + sep + hint + bot

(defn default-content-width
  "Shared content width every dialog uses, derived from `cols`. Clamped
   between `DEFAULT_DIALOG_MIN_WIDTH` and `DEFAULT_DIALOG_MAX_WIDTH` and
   bounded by the terminal so the box never paints off-screen."
  [cols]
  (let [box-w (-> (int (* cols DEFAULT_DIALOG_WIDTH_RATIO))
                (max DEFAULT_DIALOG_MIN_WIDTH)
                (min DEFAULT_DIALOG_MAX_WIDTH)
                (min (max DEFAULT_DIALOG_MIN_WIDTH (- cols 4))))]
    (max 1 (- box-w DIALOG_CHROME_W))))

(defn floor-content-height
  "Raise `content-h` to the value that produces a box at least
   `DEFAULT_DIALOG_MIN_HEIGHT` rows tall. Idempotent; never shrinks."
  [content-h]
  (max (or content-h 0) (- DEFAULT_DIALOG_MIN_HEIGHT DIALOG_CHROME_H)))

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

   Three arities:
   - `(g cols rows title content-h)` — shared default width
     (`default-content-width`), caller-provided content height (floored
     to `DEFAULT_DIALOG_MIN_HEIGHT`). PREFERRED. Width stays consistent
     across the ESC stack; height grows only as far as the content needs.
   - `(g cols rows title content-w content-h)` — fully explicit. Use
     only when a dialog genuinely needs a non-default width.

   Returns {:left :top :right :bottom :inner-w :inner-h}."
  ([g cols rows title content-h]
   (draw-dialog-chrome! g cols rows title
     (default-content-width cols)
     (floor-content-height content-h)))
  ([g cols rows title content-w content-h]
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
      :inner-w inner-w :inner-h (- box-h 2)})))

;;; ── Selection dialog ────────────────────────────────────────────────────────

(defn select-dialog!
  "Show a selection list dialog. Returns selected item map or nil on Esc.
   `items` is a vec of {:label str, ...} maps."
  [^TerminalScreen screen title items]
  (let [selected  (atom 0)
        scroll    (atom 0)
        ch        (count items)]
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title ch)
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
  (let [text         (atom (vec initial))
        cursor       (atom (count initial))
        paste-buffer (volatile! nil)]
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            ;; Content: label row + input row.
            bounds (draw-dialog-chrome! g cols rows title 2)
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
            (cond
              ;; ── Bracketed paste ──────────────────────────────
              ;; Three-state machine matching the main input loop.
              ;; START → open buffer; END → flush into text.
              ;; Prevents PUA marker chars (\uE200, \uE201) from
              ;; leaking into the dialog value — they break HTTP
              ;; Authorization headers when pasted API keys carry
              ;; them into the Bearer token.
              (input/paste-start? key)
              (do (vreset! paste-buffer (StringBuilder.))
                  (recur))

              (input/paste-end? key)
              (let [^StringBuilder sb @paste-buffer]
                (when sb
                  (let [payload (.toString sb)
                        chars   (vec payload)]
                    (vreset! paste-buffer nil)
                    (when-not (.isEmpty payload)
                      (swap! text
                        (fn [t]
                          (into (subvec t 0 @cursor)
                            (concat chars (subvec t @cursor)))))
                      (swap! cursor + (count chars)))))
                (recur))

              ;; Accumulate chars into the paste buffer while open.
              (some? @paste-buffer)
              (do (when-let [ch (input/keystroke->paste-char key)]
                    (.append ^StringBuilder @paste-buffer ch))
                  (recur))

              ;; ── Regular key dispatch ─────────────────────────
              :else
              (condp = (.getKeyType key)
                KeyType/Escape nil
                KeyType/Enter  (str/trim (apply str @text))

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
                (recur)))))))))

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
        btn-yes    "Yes"
        btn-no     "No"
        btn-w      (+ 2 (max (count btn-yes) (count btn-no))) ;; " Yes " / " No  "
        btn-gap    4
        ;; content: message lines + blank + button row = lines + 2
        ch         (+ (count raw-lines) 2)
        focus      (atom 0)] ;; 0 = Yes, 1 = No
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title ch)
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

;;; ── Settings dialog ─────────────────────────────────────────────────────────

(defn settings-dialog!
  "Show a settings dialog with toggleable options as a 3-column table:
   checkbox, label, description. The description column tells the
   user what each toggle actually does — important because some of
   them control subtle chrome (right-aligned superscripts) that
   aren't obvious from the label alone.

   `settings` is a map of toggle keys to booleans (see
   `state/default-settings`). Returns the updated settings map, or
   nil on Esc.

   The toggle list is the canonical user-controlled chrome surface.
   Adding a new toggle here + adding the matching key+default in
   `state/default-settings` is the only thing required to expose a
   new on/off knob — persistence and merge are handled centrally."
  [^TerminalScreen screen settings]
  (let [options [{:key :show-thinking
                  :label "Show thinking / reasoning"
                  :description "LLM's chain-of-thought reasoning above each iteration"}
                 {:key :show-iterations
                  :label "Show full execution trace"
                  :description "Code blocks, eval results, stdout, errors — the whole iteration history"}
                 {:key :show-timestamps
                  :label "Show per-message timestamps"
                  :description "Date+time next to every 'You' / 'Vis' label"}]
        n        (count options)
        selected (atom 0)
        values   (atom (or settings {}))
        ;; Three-column layout:
        ;;   col 1: \"[✓] \" or \"[ ] \"   (4 chars, fixed)
        ;;   col 2: label text                (label-w, computed from longest label)
        ;;   col 3: description text          (rest of inner-w, ellipsized if needed)
        ;; Plus 2-col gap between label and description so the eye
        ;; doesn't smudge the two columns together.
        check-w  4   ;; \"[x] \"
        gap      "  "
        gap-w    (count gap)
        label-w  (apply max (map (comp count :label) options))]
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows "Settings" n)
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds n)
            ;; Pad description to fill remaining space; ellipsize if
            ;; the dialog is narrower than the natural content width.
            actual-desc-w (max 1 (- inner-w 2 check-w label-w gap-w))]

        (dotimes [i n]
          (let [{:keys [key label description]} (nth options i)
                row       (+ content-top i)
                checked?  (get @values key true)
                sel?      (= i @selected)
                mark      (if checked? "✓" " ")
                checkbox  (str "[" mark "] ")
                label-pad (str label
                            (apply str (repeat (max 0 (- label-w (count label))) \space)))
                desc-trunc (if (<= (count description) actual-desc-w)
                             description
                             (str (subs description 0 (max 0 (dec actual-desc-w))) "…"))]
            (when (< row (+ content-top content-h))
              ;; Selected row uses inverted (title-bg) background; the
              ;; description block stays muted on a normal row to make
              ;; the label the dominant signal.
              (if sel?
                (do (p/set-colors! g t/dialog-bg t/dialog-title-bg)
                  (p/fill-rect! g (inc left) row inner-w 1)
                  (p/put-str! g (+ left 2) row
                    (str checkbox label-pad gap desc-trunc)))
                (do
                  ;; Background fill (single color across the row).
                  (p/set-colors! g t/dialog-fg t/dialog-bg)
                  (p/fill-rect! g (inc left) row inner-w 1)
                  ;; Checkbox + label in normal text color.
                  (p/put-str! g (+ left 2) row (str checkbox label-pad))
                  ;; Description in muted hint color.
                  (p/set-colors! g t/dialog-hint t/dialog-bg)
                  (p/put-str! g (+ left 2 check-w label-w gap-w) row desc-trunc))))))

        (draw-hint-bar! g left hint-row inner-w [["↑/↓" "move"] ["Space" "toggle"] ["Esc" "done"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape @values

              KeyType/ArrowUp
              (do (swap! selected #(clamp (dec %) 0 (max 0 (dec n))))
                (recur))

              KeyType/ArrowDown
              (do (swap! selected #(clamp (inc %) 0 (max 0 (dec n))))
                (recur))

              KeyType/Character
              (let [c (.getCharacter key)]
                (if (= c \space)
                  (let [k (:key (nth options @selected))]
                    (swap! values update k not)
                    (recur))
                  (recur)))

              KeyType/Enter
              (let [k (:key (nth options @selected))]
                (swap! values update k not)
                (recur))

              (recur))))))))

;;; ── Command palette ─────────────────────────────────────────────────────────

(def palette-commands
  "Command palette entries. Each is {:id keyword :label str}.
   Quit is intentionally NOT here — use Ctrl+C to quit.

   `:copy` is the per-message picker (Space toggles, Enter copies
   selected as plain `role: text`). `:copy-as-markdown` is the
   one-shot “give me the whole conversation as a Markdown document
   I can paste into a GitHub issue” action; it routes through the
   shared host helper `com.blockether.vis.core/conversation->markdown`
   so the CLI agent and Telegram channel can ship the same affordance
   without re-implementing the projection."
  [{:id :configure-provider :label "Configure Provider"}
   {:id :toggles            :label "Toggles"}
   {:id :copy               :label "Copy Messages"}
   {:id :copy-as-markdown   :label "Copy Conversation as Markdown"}])

(defn command-palette!
  "Show a command palette dialog. Returns the :id of the chosen command, or nil on Esc.
   No bespoke padding — `select-dialog!` runs at the shared default modal
   footprint, and `draw-list-item!` already fills the highlight stripe
   across the full inner width regardless of label length."
  [^TerminalScreen screen]
  (let [items (mapv (fn [cmd] {:label (:label cmd)}) palette-commands)]
    (when-let [choice (select-dialog! screen "Commands" items)]
      (:id (nth palette-commands (.indexOf ^java.util.List (mapv :label items) (:label choice)))))))

;;; ── Text viewer dialog ─────────────────────────────────────────────────────────

(defn text-viewer-dialog!
  "Show a scrollable read-only text viewer dialog.
   `title` is the dialog header. `text` is a string (may contain newlines)
   that is rendered VERBATIM — same content the LLM receives, only soft-
   wrapped to fit the dialog width. No markdown, no truncation, no
   reformatting.
   Returns nil on Esc. Supports keyboard scrolling."
  [^TerminalScreen screen title text]
  (let [scroll (atom 0)]
    (loop []
      (let [size    (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols    (.getColumns size)
            rows    (.getRows size)
            g       (.newTextGraphics screen)
            ;; Text viewer is the only dialog that should consume the
            ;; vertical room it can get — it scrolls long content. Ask
            ;; for terminal-bound height so the viewport is generous,
            ;; while still sharing the standard width.
            bounds  (draw-dialog-chrome! g cols rows title (max 12 (- rows 8)))
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds)
            ;; Reserve the last inner column for a scrollbar that matches
            ;; the chat area's track+thumb style. Text wraps into the
            ;; remaining width so nothing collides with the bar.
            scroll-col (+ left inner-w)
            text-w  (max 1 (- inner-w 3))
            lines   (vec (mapcat #(render/wrap-text % text-w)
                           (str/split-lines (or text "(empty)"))))
            total   (count lines)
            max-scroll (max 0 (- total content-h))
            _       (swap! scroll #(clamp % 0 max-scroll))
            visible (subvec lines @scroll
                      (min total (+ @scroll content-h)))]

        ;; Body — verbatim line render, no ellipsization (wrap-text
        ;; already produced lines that fit `text-w`).
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[i line] (map-indexed vector visible)]
          (let [row (+ content-top i)]
            (when (< row (+ content-top content-h))
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/put-str! g (+ left 2) row line))))
        ;; Clear remaining rows in the content area
        (doseq [row (range (+ content-top (count visible))
                      (+ content-top content-h))]
          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (p/fill-rect! g (inc left) row inner-w 1))

        ;; Scrollbar — same style as the chat messages area: a vertical
        ;; track of │ plus a solid █ thumb sized proportionally to the
        ;; visible window. Drawn over the content's right margin, on the
        ;; dialog background so it visually blends with the dialog frame.
        (when (> total content-h)
          (let [track-h  content-h
                ratio    (/ (double content-h) total)
                thumb-h  (max 1 (int (* track-h ratio)))
                den      (max 1 max-scroll)
                thumb-pos (int (* (- track-h thumb-h) (/ (double @scroll) den)))]
            (doseq [r (range track-h)]
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/set-char! g scroll-col (+ content-top r) Symbols/SINGLE_LINE_VERTICAL))
            (doseq [r (range thumb-h)]
              (p/set-colors! g t/dialog-hint-key t/dialog-bg)
              (p/set-char! g scroll-col (+ content-top thumb-pos r) \█))))

        (draw-hint-bar! g left hint-row inner-w
          [["↑/↓" "scroll"] ["PgUp/PgDn" "page"] ["Esc" "close"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape nil

              KeyType/ArrowUp
              (do (swap! scroll #(max 0 (dec %))) (recur))

              KeyType/ArrowDown
              (do (swap! scroll #(min max-scroll (inc %))) (recur))

              KeyType/PageUp
              (do (swap! scroll #(max 0 (- % content-h))) (recur))

              KeyType/PageDown
              (do (swap! scroll #(min max-scroll (+ % content-h))) (recur))

              KeyType/Character (recur)

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

(defn copy-dialog!
  "Show copy dialog for chat messages.
   Space toggles, A toggles all, Enter copies selected, Esc cancels."
  [^TerminalScreen screen messages]
  (let [items       (vec messages)
        selected    (atom 0)
        scroll      (atom 0)
        checked     (atom #{})
        ch          (count items)]
    (loop [status [["Space" "toggle"] ["A" "all"] ["Enter" "copy"] ["Esc" "cancel"]]]
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows "Copy Messages" ch)
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
                (let [c (Character/toLowerCase (.getCharacter key))]
                  (cond
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
