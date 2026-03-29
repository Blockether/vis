(ns com.blockether.vis.tui.primitives
  "Low-level drawing primitives wrapping Lanterna TextGraphics.
   All rendering code should use these instead of raw Java interop."
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize Symbols]))

;;; ── Color ──────────────────────────────────────────────────────────────────

(defn set-fg! [g color] (.setForegroundColor g color) g)
(defn set-bg! [g color] (.setBackgroundColor g color) g)

(defn set-colors!
  "Set both foreground and background color in one call."
  [g fg bg]
  (.setForegroundColor g fg)
  (.setBackgroundColor g bg)
  g)

;;; ── Text styles (SGR modifiers) ─────────────────────────────────────────────

(def BOLD        SGR/BOLD)
(def ITALIC      SGR/ITALIC)
(def UNDERLINE   SGR/UNDERLINE)
(def REVERSE     SGR/REVERSE)
(def CROSSED-OUT SGR/CROSSED_OUT)
(def BLINK       SGR/BLINK)
(def BORDERED    SGR/BORDERED)

(defn enable!
  "Enable one or more text styles. `modifiers` are SGR constants (BOLD, ITALIC, etc.)."
  [g & modifiers]
  (.enableModifiers g (into-array SGR modifiers))
  g)

(defn disable!
  "Disable one or more text styles."
  [g & modifiers]
  (.disableModifiers g (into-array SGR modifiers))
  g)

(defn clear-styles!
  "Remove all active text styles."
  [g]
  (.clearModifiers g)
  g)

(defn with-style
  "Execute `body-fn` (fn [g] ...) with the given styles enabled, then restore.
   Returns the result of `body-fn`."
  [g styles body-fn]
  (let [prev (vec (.getActiveModifiers g))]
    (.enableModifiers g (into-array SGR styles))
    (let [result (body-fn g)]
      (.clearModifiers g)
      (when (seq prev)
        (.enableModifiers g (into-array SGR prev)))
      result)))

(defmacro styled
  "Draw with styles temporarily enabled. Restores previous styles after body.
   Usage: (styled g [BOLD ITALIC] (put-str! g 0 0 \"hello\"))"
  [g styles & body]
  `(with-style ~g ~styles (fn [~'_g] ~@body)))

;;; ── Text ───────────────────────────────────────────────────────────────────

(defn put-str!
  "Draw a string at (col, row)."
  [g col row text]
  (.putString g (int col) (int row) (str text))
  g)

(defn set-char!
  "Draw a single character at (col, row)."
  [g col row ch]
  (.setCharacter g (int col) (int row) (char ch))
  g)

;;; ── Rectangles ─────────────────────────────────────────────────────────────

(defn fill-rect!
  "Fill a rectangle at (col, row) of size w×h with the given char (default space)."
  ([g col row w h]    (fill-rect! g col row w h \space))
  ([g col row w h ch]
   (.fillRectangle g (TerminalPosition. (int col) (int row))
                   (TerminalSize. (int w) (int h)) (char ch))
   g))

;;; ── Box drawing characters ────────────────────────────────────────────────

(def ^:const BOX_TL  Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
(def ^:const BOX_TR  Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)
(def ^:const BOX_BL  Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
(def ^:const BOX_BR  Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)
(def ^:const BOX_H   Symbols/SINGLE_LINE_HORIZONTAL)
(def ^:const BOX_V   Symbols/SINGLE_LINE_VERTICAL)
(def ^:const BOX_T_R Symbols/SINGLE_LINE_T_RIGHT)
(def ^:const BOX_T_L Symbols/SINGLE_LINE_T_LEFT)

(defn horiz-line
  "Return a string of `n` horizontal box-drawing chars."
  [n]
  (apply str (repeat n BOX_H)))

;;; ── Composite primitives ──────────────────────────────────────────────────

(defn draw-box!
  "Draw a single-line bordered box at (left, top) of size w×h.
   Draws corners, edges. Does NOT fill interior."
  [g left top w h]
  (let [right  (+ left w -1)
        bottom (+ top h -1)
        inner  (- w 2)]
    ;; Corners
    (set-char! g left top BOX_TL)
    (set-char! g right top BOX_TR)
    (set-char! g left bottom BOX_BL)
    (set-char! g right bottom BOX_BR)
    ;; Horizontal edges
    (put-str! g (inc left) top (horiz-line inner))
    (put-str! g (inc left) bottom (horiz-line inner))
    ;; Vertical edges
    (doseq [r (range (inc top) bottom)]
      (set-char! g left r BOX_V)
      (set-char! g right r BOX_V))
    g))

(defn draw-separator!
  "Draw a horizontal separator with T-junctions at left/right edges."
  [g left right row]
  (let [inner (- right left 1)]
    (set-char! g left row BOX_T_R)
    (set-char! g right row BOX_T_L)
    (put-str! g (inc left) row (horiz-line inner))
    g))

;;; ── Flex layout (pure string functions) ─────────────────────────────────────

(defn pad-right
  "Pad string to `w` chars, right-filling with spaces. Truncates if too long."
  [s w]
  (let [txt (or s "")
        len (count txt)]
    (cond
      (= len w) txt
      (> len w) (subs txt 0 w)
      :else     (str txt (apply str (repeat (- w len) \space))))))

(defn pad-left
  "Pad string to `w` chars, left-filling with spaces. Truncates if too long."
  [s w]
  (let [txt (or s "")
        len (count txt)]
    (cond
      (= len w) txt
      (> len w) (subs txt 0 w)
      :else     (str (apply str (repeat (- w len) \space)) txt))))

(defn center-text
  "Center string within `w` chars, padding both sides."
  [s w]
  (let [txt (or s "")
        len (count txt)]
    (cond
      (>= len w) (subs txt 0 w)
      :else      (let [left-pad  (quot (- w len) 2)
                       right-pad (- w len left-pad)]
                   (str (apply str (repeat left-pad \space))
                        txt
                        (apply str (repeat right-pad \space)))))))

(defn space-between
  "Distribute items across `w` chars with equal gaps between them.
   First item flush-left, last item flush-right, rest evenly spaced.
   Like CSS justify-content: space-between."
  [items w]
  (let [n (count items)]
    (cond
      (zero? n) (apply str (repeat w \space))
      (= n 1)   (center-text (first items) w)
      :else
      (let [total-text (reduce + (map count items))
            total-gaps (- w total-text)
            gap-count  (dec n)
            base-gap   (max 1 (quot total-gaps gap-count))
            extra      (- total-gaps (* base-gap gap-count))]
        (apply str
               (interleave
                items
                (concat
                 ;; Distribute remainder across first gaps
                 (map (fn [i]
                        (apply str (repeat (+ base-gap (if (< i extra) 1 0)) \space)))
                      (range gap-count))
                 ;; sentinel so interleave doesn't drop last item
                 [""])))))))

(defn space-around
  "Distribute items across `w` chars with equal space around each item.
   Like CSS justify-content: space-around."
  [items w]
  (let [n (count items)]
    (cond
      (zero? n) (apply str (repeat w \space))
      (= n 1)   (center-text (first items) w)
      :else
      (let [total-text (reduce + (map count items))
            total-gaps (- w total-text)
            slots      (* 2 n) ;; each item gets space on both sides
            base       (max 0 (quot total-gaps slots))
            unit-gap   (apply str (repeat base \space))
            ;; Build: gap item gap | gap item gap | ...
            parts      (mapcat (fn [item] [unit-gap item unit-gap]) items)]
        (let [result (apply str parts)
              len    (count result)]
          (cond
            (= len w) result
            (< len w) (str result (apply str (repeat (- w len) \space)))
            :else     (subs result 0 w)))))))

(defn v-center-offset
  "Compute vertical offset to center `content-h` rows within `container-h` rows."
  [content-h container-h]
  (if (< content-h container-h)
    (quot (- container-h content-h) 2)
    0))

;;; ── Flex drawing helpers (layout + draw in one call) ───────────────────────

(defn draw-centered!
  "Draw text centered at row within [left, left+width)."
  [g left row width text]
  (put-str! g left row (center-text text width)))

(defn draw-space-between!
  "Draw items spread across row within [left, left+width) with space-between."
  [g left row width items]
  (put-str! g left row (space-between items width)))

(defn draw-space-around!
  "Draw items spread across row within [left, left+width) with space-around."
  [g left row width items]
  (put-str! g left row (space-around items width)))

;;; ── Cursor ─────────────────────────────────────────────────────────────────

(defn cursor-pos
  "Create a TerminalPosition for cursor placement."
  [col row]
  (TerminalPosition. (int col) (int row)))
