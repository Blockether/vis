(ns com.blockether.vis.channels.tui.primitives
  "Low-level drawing primitives wrapping Lanterna TextGraphics.
   All rendering code should use these instead of raw Java interop."
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize Symbols]
           [com.googlecode.lanterna.graphics TextGraphics]))

;;; ── Color ──────────────────────────────────────────────────────────────────

(defn set-fg! [^TextGraphics g color] (.setForegroundColor g color) g)
(defn set-bg! [^TextGraphics g color] (.setBackgroundColor g color) g)

(defn set-colors!
  "Set both foreground and background color in one call."
  [^TextGraphics g fg bg]
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
  [^TextGraphics g & modifiers]
  (.enableModifiers g (into-array SGR modifiers))
  g)

(defn disable!
  "Disable one or more text styles."
  [^TextGraphics g & modifiers]
  (.disableModifiers g (into-array SGR modifiers))
  g)

(defn clear-styles!
  "Remove all active text styles."
  [^TextGraphics g]
  (.clearModifiers g)
  g)

(defn with-style
  "Execute `body-fn` (fn [g] ...) with the given styles enabled, then restore.
   Returns the result of `body-fn`."
  [^TextGraphics g styles body-fn]
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

(defn- sanitize-for-lanterna
  "Strip control characters (0x00-0x1F except tab/newline) that Lanterna
   rejects with IllegalArgumentException. Replaces them with spaces."
  ^String [^String s]
  (.replaceAll s "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F]" " "))

(defn put-str!
  "Draw a string at (col, row). Control characters are sanitized."
  [^TextGraphics g col row text]
  (.putString g (int col) (int row) (sanitize-for-lanterna (str text)))
  g)

(defn set-char!
  "Draw a single character at (col, row)."
  [^TextGraphics g col row ch]
  (.setCharacter g (int col) (int row) (char ch))
  g)

;;; ── Rectangles ─────────────────────────────────────────────────────────────

(defn fill-rect!
  "Fill a rectangle at (col, row) of size w×h with the given char (default space)."
  ([g col row w h]    (fill-rect! g col row w h \space))
  ([^TextGraphics g col row w h ch]
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
            parts      (mapcat (fn [item] [unit-gap item unit-gap]) items)
            result     (apply str parts)]
        (cond
          (= (count result) w) result
          (< (count result) w) (str result (apply str (repeat (- w (count result)) \space)))
          :else                (subs result 0 w))))))

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

;;; ── Bubble line markers ────────────────────────────────────────────────────────────
;;
;; Zero-width invisible prefixes on bubble text lines.
;; The bubble renderer checks these to apply per-line styles
;; (italic for thinking, dim for code, bold for headers, etc.)
;; without visible noise.

(def MARKER_THINKING   "\u200B")  ;; zero-width space       → italic, dim (reasoning)
(def MARKER_CODE       "\u200C")  ;; zero-width non-joiner  → code style
(def MARKER_RESULT     "\u200D")  ;; zero-width joiner      → result/return value (success)
(def MARKER_STDOUT     "\uFEFF")  ;; byte-order mark        → stdout output
(def MARKER_SEP        "\u2060")  ;; word-joiner            → separator line
(def MARKER_CODE_OK    "\u2061")  ;; function application   → code with success status
(def MARKER_CODE_ERR   "\u2062")  ;; invisible times        → code with error status
(def MARKER_ERR_RESULT "\u2063")  ;; invisible separator    → error result line
(def MARKER_DURATION   "\u2064")  ;; invisible plus         → duration annotation
(def MARKER_ITER_HDR   "\u2066")  ;; LRI                    → iteration header with bg
(def MARKER_STDOUT_SEP "\u2067")  ;; RLI                    → stdout separator line
(def MARKER_STDOUT_PAD "\u2068")  ;; FSI                    → stdout empty padding line
(def MARKER_ANSWER_SEP "\u2069")  ;; PDI                    → answer separator (trace→answer break)
(def MARKER_CODE_PAD   "\u206A")  ;; ISS                    → code block empty padding line
(def MARKER_CODE_ERR_PAD "\u206B") ;; ASS                   → error code block padding line
(def MARKER_ITER_PAD   "\u206C")  ;; IAFS                   → iteration zone padding (margin between blocks)
(def MARKER_ANSWER_HDR "\u206D")  ;; AAFS                   → final answer header
(def MARKER_ANSWER_TXT "\u206E")  ;; NADS                   → answer text line (with answer bg)
(def MARKER_ANSWER_PAD "\u206F")  ;; NODS                   → answer padding line
;; Markdown markers (PUA \uE000+) — guaranteed unique, never collide
;; with the iteration/answer markers above. Two parallel sets:
;;   - MARKER_MD_*    → answer-zone markdown (answer-bg)
;;   - MARKER_TH_MD_* → thinking-zone markdown (iter-header-bg, italic)
(def MARKER_MD_H1         "\uE001") ;; markdown heading 1 (answer)
(def MARKER_MD_H2         "\uE002") ;; markdown heading 2 (answer)
(def MARKER_MD_H3         "\uE003") ;; markdown heading 3 (answer)
(def MARKER_MD_BOLD       "\uE004") ;; markdown bold line (answer)
(def MARKER_MD_CODE       "\uE005") ;; markdown fenced code (answer)
(def MARKER_MD_BULLET     "\uE006") ;; markdown bullet list item (answer)
(def MARKER_MD_TABLE_HEAD "\uE007") ;; markdown table header row (answer)
(def MARKER_MD_TABLE_SEP  "\uE008") ;; markdown table border / separator (answer)
(def MARKER_MD_TABLE_ROW  "\uE009") ;; markdown table data row (answer)
(def MARKER_MD_QUOTE      "\uE00A") ;; markdown blockquote (answer)
(def MARKER_MD_HR         "\uE00B") ;; markdown horizontal rule (answer)

(def MARKER_TH_MD_H1         "\uE021") ;; markdown heading 1 (thinking)
(def MARKER_TH_MD_H2         "\uE022") ;; markdown heading 2 (thinking)
(def MARKER_TH_MD_H3         "\uE023") ;; markdown heading 3 (thinking)
(def MARKER_TH_MD_BOLD       "\uE024") ;; markdown bold line (thinking)
(def MARKER_TH_MD_CODE       "\uE025") ;; markdown fenced code (thinking)
(def MARKER_TH_MD_BULLET     "\uE026") ;; markdown bullet list item (thinking)
(def MARKER_TH_MD_TABLE_HEAD "\uE027") ;; markdown table header row (thinking)
(def MARKER_TH_MD_TABLE_SEP  "\uE028") ;; markdown table border (thinking)
(def MARKER_TH_MD_TABLE_ROW  "\uE029") ;; markdown table data row (thinking)
(def MARKER_TH_MD_QUOTE      "\uE02A") ;; markdown blockquote (thinking)
(def MARKER_TH_MD_HR         "\uE02B") ;; markdown horizontal rule (thinking)
