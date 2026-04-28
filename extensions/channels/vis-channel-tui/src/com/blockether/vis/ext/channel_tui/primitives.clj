(ns com.blockether.vis.ext.channel-tui.primitives
  "Low-level drawing primitives wrapping Lanterna TextGraphics.
   All rendering code should use these instead of raw Java interop.

   Width-math contract
   ===================

   String width here means **display columns the terminal will paint**,
   NOT Java `.length()` and NOT (count s). Those count UTF-16 code units;
   a single emoji like \uD83D\uDCC1 is two code units but two columns,
   a CJK glyph like \u65E5 is one code unit but two columns, a flag
   sequence \uD83C\uDDF5\uD83C\uDDF1 is four code units, one grapheme,
   two columns. Mix the two and your alignment drifts.

   Use `display-width` and `truncate-cols` for any string that could
   contain non-ASCII. Plain `count`/`subs` are still allowed for ASCII
   internals (box-drawing strings we authored, single-glyph keystroke
   labels, etc.) where the answer is identical and the call is a hot path."
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize Symbols TextCharacter]
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

;;; ── Display-width (terminal columns, not Java chars) ──────────────────────
;;
;; Inline span sentinels are a SECOND PUA range (\uE110…\uE117);
;; see the `INLINE_*` constants further down in this file. The
;; range bounds + the `inline-sentinel?` predicate are inlined here
;; so display-width and friends can reference them without a forward
;; declare; the canonical name lives with the marker constants block.

(def INLINE_BOLD_ON     "\uE110")
(def INLINE_BOLD_OFF    "\uE111")
(def INLINE_ITALIC_ON   "\uE112")
(def INLINE_ITALIC_OFF  "\uE113")
(def INLINE_STRIKE_ON   "\uE114")
(def INLINE_STRIKE_OFF  "\uE115")
(def INLINE_CODE_ON     "\uE116")
(def INLINE_CODE_OFF    "\uE117")

(def ^:private ^:const INLINE_SENTINEL_LO 0xE110)
(def ^:private ^:const INLINE_SENTINEL_HI 0xE117)

(defn inline-sentinel?
  "True when `g` (a single grapheme String) is one of the eight inline
   span sentinels above. Cheap range check, no map lookup."
  [^String g]
  (and (= 1 (.length g))
    (let [c (int (.charAt g 0))]
      (and (>= c (long INLINE_SENTINEL_LO)) (<= c (long INLINE_SENTINEL_HI))))))

(defn display-width
  "Number of terminal columns `s` will occupy when painted by lanterna.

   Built on `TextCharacter/fromString`, the same routine
   `AbstractTextGraphics.putString` uses internally after PR #625, so
   what we measure matches what the renderer actually paints — grapheme
   clusters honoured (BreakIterator-based), CJK + emoji counted as two
   columns, ASCII as one.

   Inline span sentinels (`INLINE_*_ON`/`OFF`, range \uE110…\uE117)
   count as zero columns: they're invisible style toggles, never
   painted, never advance the cursor.

   Returns 0 for nil/empty input."
  ^long [s]
  (if (or (nil? s) (zero? (.length ^CharSequence s)))
    0
    (let [cells (TextCharacter/fromString ^String s)
          n     (alength cells)]
      (loop [i 0 width 0]
        (if (>= i n)
          width
          (let [tc ^TextCharacter (aget cells i)
                g  ^String (.getCharacterString tc)
                w  (cond
                     (inline-sentinel? g) 0
                     (.isDoubleWidth tc)  2
                     :else                1)]
            (recur (inc i) (+ width w))))))))

(defn col-prefix-end
  "Return the char-index `i` such that `(subs s 0 i)` is the longest
   prefix of `s` whose `display-width` is ≤ `max-cols` AND that does NOT
   split a grapheme cluster.

   Use this when you need both the prefix and the position of the
   un-consumed remainder (e.g. in word-wrapping). For just the prefix,
   `truncate-cols` is friendlier.

   Returns 0 for nil/empty or non-positive `max-cols`."
  ^long [s ^long max-cols]
  (cond
    (or (nil? s) (<= max-cols 0)) 0
    (<= (display-width s) max-cols) (long (.length ^CharSequence s))
    :else
    (let [cells (TextCharacter/fromString ^String s)
          n     (alength cells)]
      (loop [i 0 char-idx 0 used 0]
        (if (>= i n)
          char-idx
          (let [tc           ^TextCharacter (aget cells i)
                grapheme     ^String (.getCharacterString tc)
                grapheme-len (long (.length grapheme))
                w            (cond
                               (inline-sentinel? grapheme) 0
                               (.isDoubleWidth tc)         2
                               :else                       1)]
            (if (> (+ used w) max-cols)
              char-idx
              (recur (inc i) (+ char-idx grapheme-len) (+ used w)))))))))

(defn truncate-cols
  "Return the longest prefix of `s` that fits in at most `max-cols`
   terminal columns, never splitting a grapheme cluster.

   Edge cases honoured:
   - nil or `max-cols <= 0` returns `\"\"`.
   - If a double-width grapheme would straddle the cut, it is dropped
     (NOT half-included), and one space is appended in its place so the
     returned string's `display-width` is exactly `max-cols`. This keeps
     `pad-right` / `pad-left` idempotent under repeated truncation."
  ^String [s ^long max-cols]
  (cond
    (or (nil? s) (<= max-cols 0)) ""
    (<= (display-width s) max-cols) s
    :else
    (let [cells (TextCharacter/fromString ^String s)
          n     (alength cells)
          sb    (StringBuilder.)]
      ;; Walk every grapheme, emit it iff it fits, stop on overflow.
      ;; The earlier (= next max-cols) early-exit was deleted because
      ;; it stranded trailing zero-width sentinels (style closers): if
      ;; the budget filled exactly on a visible char and the next
      ;; grapheme was a `INLINE_*_OFF`, the closer never made it into
      ;; the output and the SGR style leaked past the cut. The
      ;; structure below keeps walking sentinels for free (their `w`
      ;; is zero, so `next` stays under budget) and only stops when a
      ;; visible grapheme would push past `max-cols`.
      (loop [i 0 used 0]
        (if (>= i n)
          (.toString sb)
          (let [tc   ^TextCharacter (aget cells i)
                gs   ^String (.getCharacterString tc)
                w    (cond
                       (inline-sentinel? gs) 0
                       (.isDoubleWidth tc)   2
                       :else                 1)
                next (+ used w)]
            (if (> next max-cols)
              ;; A wide grapheme would have straddled the cut: drop it
              ;; and pad with one space so the result's display-width
              ;; is exactly `max-cols`. Sentinels can't reach this
              ;; branch (their `w` is zero, so `next` never exceeds).
              (do (when (< used max-cols) (.append sb \space))
                (.toString sb))
              ;; In-budget OR zero-width sentinel: append, advance,
              ;; continue. For sentinels `w` is 0 so `used` stays put.
              (do (.append sb gs)
                (recur (inc i) next)))))))))

;;; ── Inline-styled line painter ─────────────────────────────────────────────

(defn paint-styled-line!
  "Paint a single line at (x, y) honouring inline span sentinels.

   The line may contain interleaved text and `INLINE_*_ON`/`OFF`
   sentinels (range \\uE110…\\uE117). Sentinels themselves are NEVER
   painted; they toggle the SGR style (BOLD/ITALIC/CROSSED-OUT) or
   the fg/bg colors (CODE) for the spans that follow.

   `base-fg` / `base-bg` are the colors used for non-code spans.
   Code spans force `code-fg` / `code-bg` until INLINE_CODE_OFF.

   Robustness contract:
   - Walks by GRAPHEME CLUSTER (lanterna's `TextCharacter/fromString`),
     so column tracking is exact for emoji + CJK.
   - INHERITS any SGR modifiers already enabled on `g` at entry (e.g.
     a wrapping `(p/styled g [p/ITALIC] ...)` for blockquotes). Inline
     toggles STACK on top of the inherited set: `> **bold**` inside a
     quote renders as bold-italic, not bold-without-italic. The pre-fix
     version called `clearModifiers` at entry and silently dropped the
     wrapping italic — user-visible bug on every blockquote that
     contained inline emphasis.
   - Unmatched / dangling sentinels (e.g. an OFF without a prior ON,
     or a line that ends mid-bold) are tolerated: at exit we restore
     exactly the inherited modifier set, so SGR state never leaks past
     the call.
   - When NO sentinel appears in the line this collapses to a single
     `put-str!` call — the same as the old non-styled path — so the
     hot path is not penalised for the common case.

   This is the entry point that lets the markdown renderer support
   **bold** / *italic* / ~~strike~~ / `code` mid-line without
   abandoning the marker-prefix-per-line architecture above."
  [^TextGraphics g x y ^String line base-fg base-bg code-fg code-bg]
  (let [;; Capture pre-existing modifiers so inline toggles can stack
        ;; on top of them and we can restore exactly at exit.
        inherited ^java.util.EnumSet (java.util.EnumSet/copyOf (.getActiveModifiers g))
        cells     (TextCharacter/fromString line)
        n         (alength cells)]
    (.setForegroundColor g base-fg)
    (.setBackgroundColor g base-bg)
    (cond
      (zero? n) nil

      ;; Fast path: no sentinels at all → single putString. The
      ;; inherited modifiers are still active, so this paints with
      ;; whatever style the wrapping `styled` block enabled.
      (loop [i 0]
        (cond
          (>= i n) true
          (inline-sentinel? (.getCharacterString ^TextCharacter (aget cells i))) false
          :else (recur (inc i))))
      (.putString g (int x) (int y) line)

      ;; Slow path: walk graphemes, buffer text segments, flush on
      ;; style transitions. `inline` is the set of toggles activated
      ;; by sentinels we've seen so far; effective SGR set per flush
      ;; is `inherited ∪ inline`.
      :else
      (let [sb     (StringBuilder.)
            inline (java.util.EnumSet/noneOf SGR)
            col    (int-array 1 0)
            code?  (boolean-array 1 false)
            flush! (fn []
                     (when (pos? (.length sb))
                       (let [seg (.toString sb)]
                         (.clearModifiers g)
                         (if (aget code? 0)
                           ;; Code span: hard-override fg/bg; modifiers
                           ;; cleared so code reads as a flat zone.
                           (do (.setForegroundColor g code-fg)
                             (.setBackgroundColor g code-bg))
                           ;; Plain span: base colors + (inherited ∪ inline) SGR.
                           (let [effective ^java.util.EnumSet (java.util.EnumSet/copyOf inherited)]
                             (.addAll effective inline)
                             (.setForegroundColor g base-fg)
                             (.setBackgroundColor g base-bg)
                             (when-not (.isEmpty effective)
                               (.enableModifiers g (into-array SGR effective)))))
                         (.putString g (+ (int x) (aget col 0)) (int y) seg)
                         (aset col 0 (int (+ (aget col 0) (display-width seg))))
                         (.setLength sb 0))))]
        (dotimes [i n]
          (let [tc ^TextCharacter (aget cells i)
                gs ^String (.getCharacterString tc)]
            (cond
              (= gs INLINE_BOLD_ON)    (do (flush!) (.add inline SGR/BOLD))
              (= gs INLINE_BOLD_OFF)   (do (flush!) (.remove inline SGR/BOLD))
              (= gs INLINE_ITALIC_ON)  (do (flush!) (.add inline SGR/ITALIC))
              (= gs INLINE_ITALIC_OFF) (do (flush!) (.remove inline SGR/ITALIC))
              (= gs INLINE_STRIKE_ON)  (do (flush!) (.add inline SGR/CROSSED_OUT))
              (= gs INLINE_STRIKE_OFF) (do (flush!) (.remove inline SGR/CROSSED_OUT))
              (= gs INLINE_CODE_ON)    (do (flush!) (aset code? 0 true))
              (= gs INLINE_CODE_OFF)   (do (flush!) (aset code? 0 false))
              :else                    (.append sb gs))))
        (flush!)
        ;; Restore the inherited modifier set exactly so the wrapping
        ;; `styled` form's cleanup sees the state it expects. Without
        ;; this, an unbalanced sentinel (e.g. line ending mid-bold)
        ;; could leak BOLD into the next paint call.
        (.clearModifiers g)
        (.setForegroundColor g base-fg)
        (.setBackgroundColor g base-bg)
        (when-not (.isEmpty inherited)
          (.enableModifiers g (into-array SGR inherited)))))))

;;; ── Flex layout (pure string functions, column-aware) ─────────────────────

(defn pad-right
  "Pad string to `w` terminal columns, right-filling with spaces.
   Truncates (column-aware) if too wide."
  [s w]
  (let [txt  (or s "")
        cols (display-width txt)
        w    (long w)]
    (cond
      (= cols w) txt
      (> cols w) (truncate-cols txt w)
      :else     (str txt (apply str (repeat (- w cols) \space))))))

(defn pad-left
  "Pad string to `w` terminal columns, left-filling with spaces.
   Truncates (column-aware) if too wide."
  [s w]
  (let [txt  (or s "")
        cols (display-width txt)
        w    (long w)]
    (cond
      (= cols w) txt
      (> cols w) (truncate-cols txt w)
      :else     (str (apply str (repeat (- w cols) \space)) txt))))

(defn center-text
  "Center string within `w` terminal columns, padding both sides.
   Truncates (column-aware) if too wide."
  [s w]
  (let [txt  (or s "")
        cols (display-width txt)
        w    (long w)]
    (cond
      (>= cols w) (truncate-cols txt w)
      :else      (let [left-pad  (quot (- w cols) 2)
                       right-pad (- w cols left-pad)]
                   (str (apply str (repeat left-pad \space))
                     txt
                     (apply str (repeat right-pad \space)))))))

(defn space-between
  "Distribute items across `w` terminal columns with equal gaps.
   First item flush-left, last item flush-right, rest evenly spaced.
   Like CSS justify-content: space-between."
  [items w]
  (let [n (count items)
        w (long w)]
    (cond
      (zero? n) (apply str (repeat w \space))
      (= n 1)   (center-text (first items) w)
      :else
      (let [total-text (long (reduce + (map display-width items)))
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
                     (apply str (repeat (+ base-gap (if (< (long i) (long extra)) 1 0)) \space)))
                (range gap-count))
                 ;; sentinel so interleave doesn't drop last item
              [""])))))))

(defn space-around
  "Distribute items across `w` terminal columns with equal space
   around each item. Like CSS justify-content: space-around."
  [items w]
  (let [n (count items)
        w (long w)]
    (cond
      (zero? n) (apply str (repeat w \space))
      (= n 1)   (center-text (first items) w)
      :else
      (let [total-text (long (reduce + (map display-width items)))
            total-gaps (- w total-text)
            slots      (* 2 n) ;; each item gets space on both sides
            base       (max 0 (quot total-gaps slots))
            unit-gap   (apply str (repeat base \space))
            ;; Build: gap item gap | gap item gap | ...
            parts      (mapcat (fn [item] [unit-gap item unit-gap]) items)
            result     (apply str parts)
            result-w   (display-width result)]
        (cond
          (= result-w w) result
          (< result-w w) (str result (apply str (repeat (- w result-w) \space)))
          :else          (truncate-cols result w))))))

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

;; Inline span sentinels (\uE110…\uE117) live in their own section
;; near the top of this file because both the width math
;; (`display-width` etc.) AND the painter (`paint-styled-line!`)
;; need them; defining them here would force a forward declare.
;; Search UP for `INLINE_BOLD_ON`.
