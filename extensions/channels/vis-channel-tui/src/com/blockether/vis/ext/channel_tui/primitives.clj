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
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize Symbols TextCharacter
            TerminalTextUtils]
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

(def BOLD SGR/BOLD)
(def ITALIC SGR/ITALIC)
(def UNDERLINE SGR/UNDERLINE)
(def REVERSE SGR/REVERSE)
(def CROSSED-OUT SGR/CROSSED_OUT)
(def BLINK SGR/BLINK)
(def BORDERED SGR/BORDERED)

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

(defn clear-styles! "Remove all active text styles." [^TextGraphics g] (.clearModifiers g) g)

(defn with-style
  "Execute `body-fn` (fn [g] ...) with the given styles enabled, then restore.
   Returns the result of `body-fn`."
  [^TextGraphics g styles body-fn]
  (let [previous-modifiers (vec (.getActiveModifiers g))]
    (.enableModifiers g (into-array SGR styles))
    (let [result (body-fn g)]
      (.clearModifiers g)
      (when (seq previous-modifiers) (.enableModifiers g (into-array SGR previous-modifiers)))
      result)))

(defmacro styled
  "Draw with styles temporarily enabled. Restores previous styles after body.
   Usage: (styled g [BOLD ITALIC] (put-str! g 0 0 \"hello\"))"
  [g styles & body]
  `(with-style ~g
               ~styles
               (fn [~'_g]
                 ~@body)))

;;; ── Text ───────────────────────────────────────────────────────────────────

(defn- sanitize-for-lanterna
  "Strip paint-only/control characters that must never hit raw Lanterna
   putString. Control chars become spaces. Inline style sentinels are removed:
   styled painters consume them, but raw fallback paths must not show PUA tofu."
  ^String [^String s]
  (-> s
      (.replaceAll "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F]" " ")
      (.replaceAll "[\\uE110-\\uE119]" "")))

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

(defn underline-cell!
  "Add the UNDERLINE modifier to the SINGLE already-painted cell at (col,row),
   preserving its glyph + background. Reads the cell back via `getCharacter`
   and re-sets it with the modifier folded in, so callers can paint a
   PER-COLUMN border (a steady / marching underline) that the global
   `enable!`/`UNDERLINE` attribute — which would underline every cell touched
   after it — can't express. No-op when the cell is empty/off-screen.

   3-arity keeps the cell's own foreground (the underline inherits it).
   4-arity RECOLOURS the foreground to `fg` first, so the underline — which a
   terminal draws in the cell's foreground colour — renders in `fg`. Recolour
   is uniform across blank padding AND glyph cells, giving ONE consistent
   underline shape/weight everywhere (no block-glyph vs SGR-line mismatch)."
  ([^TextGraphics g col row]
   (when-let [tc (.getCharacter g (int col) (int row))]
     (.setCharacter g (int col) (int row) (.withModifier tc SGR/UNDERLINE)))
   g)
  ([^TextGraphics g col row fg]
   (when-let [tc (.getCharacter g (int col) (int row))]
     (.setCharacter g
                    (int col)
                    (int row)
                    (-> tc
                        (.withForegroundColor fg)
                        (.withModifier SGR/UNDERLINE))))
   g))

(defn dot-cell!
  "Overlay a bottom-flush `▁` mark in `fg` colour onto the SINGLE cell at
   (col,row), keeping its existing background, but ONLY when that cell is
   currently a blank space OR an existing `▁` mark. `▁` (LOWER ONE EIGHTH
   BLOCK) sits flush on the cell's bottom edge — the SAME row as an
   `underline-cell!` border — so the
   running tab's marching marks line up with the ready tab's underline instead
   of floating one line high like a baseline `.` did (which read as flicker).
   Only blank padding cells are touched, so the number, ` | ` separator,
   label, and close ✕ (non-blank) are never clobbered — but a prior `▁` may be
   RE-COLOURED, which lets a running tab paint a dim base line then overlay a
   bright marching band on top. No-op off-screen or over
   any non-blank glyph."
  [^TextGraphics g col row fg]
  (when-let [tc (.getCharacter g (int col) (int row))]
    (when (#{" " "▁"} (.getCharacterString tc))
      (.setCharacter g
                     (int col)
                     (int row)
                     (-> tc
                         (.withCharacter \▁)
                         (.withForegroundColor fg)))))
  g)

;;; ── Rectangles ─────────────────────────────────────────────────────────────

(defn fill-rect!
  "Fill a rectangle at (col, row) of size wxh with the given char (default space)."
  ([g col row w h] (fill-rect! g col row w h \space))
  ([^TextGraphics g col row w h ch]
   (.fillRectangle g
                   (TerminalPosition. (int col) (int row))
                   (TerminalSize. (int w) (int h))
                   (char ch))
   g))

;;; ── Box drawing characters ────────────────────────────────────────────────

(def ^:const BOX_TL Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
(def ^:const BOX_TR Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)
(def ^:const BOX_BL Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
(def ^:const BOX_BR Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)
(def ^:const BOX_H Symbols/SINGLE_LINE_HORIZONTAL)
(def ^:const BOX_V Symbols/SINGLE_LINE_VERTICAL)
(def ^:const BOX_T_R Symbols/SINGLE_LINE_T_RIGHT)
(def ^:const BOX_T_L Symbols/SINGLE_LINE_T_LEFT)

(defn horiz-line
  "Return a string of `n` horizontal box-drawing chars."
  [n]
  (apply str (repeat n BOX_H)))

;;; ── Selection marker (dot cursor component) ─────────────────────
;;
;; Universal cursor marker for every up/down navigable list in the
;; TUI: dialogs (select, settings, sessions, file picker,
;; resources, providers, models), command palette, slash overlay,
;; etc.
;;
;; Selected rows use one left-anchored dot, not row inversion and not
;; ad-hoc `>` glyphs. Both selected and unselected states reserve the
;; same display width (2 cols) so column layout survives navigation.
;;
;; The painter (`draw-selection-marker!`) is defined further down,
;; AFTER the `styled` macro is in scope; the constants and the
;; pure-string helper live up here so callers that just want the
;; prefix string don't need to load the full painter graph.

(def ^:const SELECTION_GLYPH
  "Two-col selection marker. Selected rows show `•`+space, unselected
   rows show two spaces, so the body content stays column-aligned.

   The glyph MUST be display-width 1 so glyph+space == 2 cols and
   exactly fills the reserved gutter. `•` (U+2022) is East-Asian
   *ambiguous* width; the lanterna fork (>= 3.1.5-vis.7) scores EAW=A
   as NARROW by default, so `• ` is 2 cols. (A regression in vis.6
   briefly made it wide — the 'marker eats a character' bug.)"
  "• ")

(def ^:const SELECTION_BLANK "  ")

(def ^:const SELECTION_WIDTH 2)

(defn selection-prefix
  "Return the leading marker string for a list/menu row.

   Use this for rows where the marker can be inlined into the body
   text (simple list items, checkbox rows, slash-command rows). For
   rows where the marker must live OUTSIDE a fixed-column body
   (file/session pickers, provider/model cards) call
   `draw-selection-marker!` from the caller's row loop instead."
  [selected?]
  (if selected? SELECTION_GLYPH SELECTION_BLANK))

;;; ── Status glyph vocabulary (● ○ ◆ ▸) ──────────────────────────────────────
;; ONE shared status-mark language across the TUI — the footer's resource ●, the
;; settings toggle rows, and the managed-resource rows all speak it, matching the
;; web's status dots. Each glyph MUST be display-width 1 (bare geometric, never a
;; VS-16 emoji) so `glyph + space` fills a 2-col gutter and the cell grid stays
;; aligned — same rule as SELECTION_GLYPH above.
(def ^:const STATUS_ON "●") ;; filled  — enabled / live / healthy
(def ^:const STATUS_OFF "○") ;; hollow  — disabled / idle
(def ^:const MARK_VALUE "◆") ;; a value you can cycle (enum / choice)
(def ^:const MARK_ACTION "▸") ;; an action you can run

;; Footer chip icons — replace the literal words "resources"/"dir" in the
;; status footer. Bare BMP glyphs (display-width 1, NOT VS-16 emoji) so
;; `(count text)` matches the rendered cell count — same grid rule as above.
(def ^:const GLYPH_RESOURCES "⚙") ;; managed resources (nREPLs, daemons…)
(def ^:const GLYPH_DIR "⌂") ;; filesystem-root directories

(def ^:const STATUS_WIDTH 2) ;; glyph (1) + trailing gap (1)

(defn status-mark!
  "Paint a 1-col status `glyph` in colour `fg` on `bg` at (col,row) and return
   the next col (`col + STATUS_WIDTH`) so the caller can place the label right
   after. The reusable mark behind settings rows + resource rows."
  [g col row glyph fg bg]
  (set-colors! g fg bg)
  (put-str! g col row glyph)
  (+ col STATUS_WIDTH))

;;; ── Composite primitives ──────────────────────────────────────────────────

(defn draw-box!
  "Draw a single-line bordered box at (left, top) of size wxh.
   Draws corners, edges. Does NOT fill interior."
  [g left top w h]
  (let [right
        (+ left w -1)

        bottom
        (+ top h -1)

        inner
        (- w 2)]

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

(defn draw-selection-marker!
  "Paint the selection marker at (col, row) when `selected?` is
   truthy. Unselected rows get nothing — the surrounding row fill is
   expected to already cover those cells.

   The glyph is rendered BOLD. Provide `marker-fg` to give it its
   own color (e.g. `dialog-hint-key`); when omitted, whatever fg is
   currently set on `g` is used.

   See the `Selection marker` block above for the project-wide
   rationale. Callers that need the prefix as a STRING (to inline
   into a row label) should use `selection-prefix` instead."
  ([g col row selected?] (draw-selection-marker! g col row selected? nil))
  ([^TextGraphics g col row selected? marker-fg]
   (when selected?
     (let [prev-fg (when marker-fg (.getForegroundColor g))]
       (when marker-fg (set-fg! g marker-fg))
       (styled g [BOLD] (put-str! g col row SELECTION_GLYPH))
       (when marker-fg (set-fg! g prev-fg))))
   g))

;;; ── Display-width (terminal columns, not Java chars) ──────────────────────
;;
;; Inline span sentinels are a SECOND PUA range (\uE110...\uE117);
;; see the `INLINE_*` constants further down in this file. The
;; range bounds + the `inline-sentinel?` predicate are inlined here
;; so display-width and friends can reference them without a forward
;; declare; the canonical name lives with the marker constants block.

(def INLINE_BOLD_ON "\uE110")
(def INLINE_BOLD_OFF "\uE111")
(def INLINE_ITALIC_ON "\uE112")
(def INLINE_ITALIC_OFF "\uE113")
(def INLINE_STRIKE_ON "\uE114")
(def INLINE_STRIKE_OFF "\uE115")
(def INLINE_CODE_ON "\uE116")
(def INLINE_CODE_OFF "\uE117")
(def INLINE_LINK_ON "\uE118")
(def INLINE_LINK_OFF "\uE119")

(def ^:private ^:const INLINE_SENTINEL_LO 0xE110)
(def ^:private ^:const INLINE_SENTINEL_HI 0xE119)

(defn inline-sentinel?
  "True when `g` (a single grapheme String) is one of the eight inline
   span sentinels above. Cheap range check, no map lookup."
  [^String g]
  (and (= 1 (.length g))
       (let [c (int (.charAt g 0))]
         (and (>= c (long INLINE_SENTINEL_LO)) (<= c (long INLINE_SENTINEL_HI))))))

(defn- sanitize-control-chars
  "Replace every ASCII control character in `s` (codepoints 0x00-0x1F)
   with `/` so a stray `\n` / `\t` / `\r` from a malformed link
   parse or bad upstream string can NEVER take the render thread
   down. Returns `s` UNCHANGED (same identity, no allocation) when
   the input is clean - the overwhelmingly common case - so we
   don't pay a StringBuilder allocation on every grapheme-width
   call.

   Session 954bf315 was the live trigger: a multi-line string with
   an embedded `0x0a` reached `display-width`, Lanterna's
   `TextCharacter.fromString` threw on the control char, the render
   thread's catch-all swallowed the throw, the bubble silently
   failed to paint, and the user saw a blank scrollback. This is the
   belt-and-braces fallback for any caller that lets a control char
   slip into a paint string.

   Inline-span sentinels (\uE110...\uE117) live in the BMP
   private-use area, not C0, so they pass through untouched."
  ^String [^String s]
  (let [n
        (.length s)

        first-bad
        (loop [i 0]
          (cond (>= i n) -1
                (< (int (.charAt s i)) 0x20) i
                :else (recur (inc i))))]

    (if (neg? first-bad)
      s
      (let [sb (StringBuilder. n)]
        (.append sb s (int 0) (int first-bad))
        (loop [k first-bad]
          (if (>= k n)
            (.toString sb)
            (let [c (.charAt s k)]
              (.append sb (if (< (int c) 0x20) / c))
              (recur (inc k)))))))))

;; Per-terminal glyph width (VS-16 emoji narrow on Apple Terminal.app, wide
;; elsewhere) lives in the lanterna fork's `TextCharacter.isDoubleWidth`
;; (auto-detected from TERM_PROGRAM). Everything here defers to it, so
;; measurement always matches what the fork's painter/screen emit.

(defn- all-narrow-ascii?
  "True when every char of `s` (length `n`) is printable ASCII (0x20-0x7E).
   Such a string occupies exactly `n` terminal columns, so we can skip the
   expensive grapheme/width segmentation in `TextCharacter/fromString` — no
   `TextCharacter[]` allocation, no `Character$UnicodeBlock.of` per char. This
   is the overwhelmingly common case (English prose, code, digits, punctuation)
   and `display-width` is on the hot render/keystroke path, called across the
   entire scrollback every frame."
  [^String s ^long n]
  (loop [i 0]
    (if (>= i n)
      true
      (let [c (int (.charAt s i))]
        (if (and (>= c 0x20) (<= c 0x7E)) (recur (inc i)) false)))))

(defn display-width
  "Number of terminal columns `s` will occupy when painted by lanterna.

   Built on `TextCharacter/fromString`, the same routine
   `AbstractTextGraphics.putString` uses internally after PR #625, so
   what we measure matches what the renderer actually paints - grapheme
   clusters honoured (BreakIterator-based), CJK + emoji counted as two
   columns, ASCII as one.

   Inline span sentinels (`INLINE_*_ON`/`OFF`, range \uE110...\uE119)
   count as zero columns: they're invisible style toggles, never
   painted, never advance the cursor.

   Stray ASCII control bytes (0x00-0x1F) get sanitized to `/`
   before reaching Lanterna - see `sanitize-control-chars` for the
   why. Without that, a single rogue `\n` in a paint string used to
   take down the entire render thread.

   Pure printable-ASCII strings (the common case) short-circuit to
   `(.length s)` — one column per char — skipping the grapheme/width
   segmentation entirely.

   Returns 0 for nil/empty input."
  ^long [s]
  ;; Coerce to String FIRST. `s` is usually a String, but a caller that
  ;; maps display-width over a string (or hands us a Character / number)
  ;; must never crash the render thread — a single ClassCastException here
  ;; throws every frame and freezes the whole TUI. `(str s)` makes any input
  ;; measurable; nil → 0.
  (if (nil? s)
    0
    (let [^String safe
          (sanitize-control-chars (str s))

          len
          (.length safe)]

      (cond (zero? len) 0
            ;; Fast path: printable ASCII ⇒ 1 column per char, no allocation.
            (all-narrow-ascii? safe len) len
            :else (let [cells
                        (TextCharacter/fromString safe)

                        n
                        (alength cells)]

                    (loop [i
                           0

                           width
                           0]

                      (if (>= i n)
                        width
                        (let [tc
                              ^TextCharacter (aget cells i)

                              g
                              ^String (.getCharacterString tc)

                              w
                              (cond (inline-sentinel? g) 0
                                    ;; Defer to lanterna's isDoubleWidth. The fork owns the
                                    ;; per-terminal width policy (VS-16 = 2 on iTerm2/Ghostty/…,
                                    ;; but 1 on Apple Terminal.app — auto-detected there), so
                                    ;; measurement always matches what putString paints.
                                    (.isDoubleWidth tc) 2
                                    :else 1)]

                          (recur (inc i) (+ width w))))))))))

(defn col-prefix-end
  "Return the char-index `i` such that `(subs s 0 i)` is the longest
   prefix of `s` whose `display-width` is <= `max-cols` AND that does NOT
   split a grapheme cluster.

   Use this when you need both the prefix and the position of the
   un-consumed remainder (e.g. in word-wrapping). For just the prefix,
   `truncate-cols` is friendlier.

   Returns 0 for nil/empty or non-positive `max-cols`."
  ^long [s ^long max-cols]
  (cond (or (nil? s) (<= max-cols 0)) 0
        (<= (display-width s) max-cols) (long (.length ^CharSequence s))
        :else (let [cells
                    (TextCharacter/fromString ^String s)

                    n
                    (alength cells)]

                (loop [i
                       0

                       char-idx
                       0

                       used
                       0]

                  (if (>= i n)
                    char-idx
                    (let [tc
                          ^TextCharacter (aget cells i)

                          grapheme
                          ^String (.getCharacterString tc)

                          grapheme-len
                          (long (.length grapheme))

                          w
                          (cond (inline-sentinel? grapheme) 0
                                (.isDoubleWidth tc) 2
                                :else 1)]

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
  (cond (or (nil? s) (<= max-cols 0)) ""
        (<= (display-width s) max-cols) s
        :else (let [cells
                    (TextCharacter/fromString ^String s)

                    n
                    (alength cells)

                    sb
                    (StringBuilder.)]

                ;; Walk every grapheme, emit it iff it fits, stop on overflow.
                ;; The earlier (= next max-cols) early-exit was deleted because
                ;; it stranded trailing zero-width sentinels (style closers): if
                ;; the budget filled exactly on a visible char and the next
                ;; grapheme was a `INLINE_*_OFF`, the closer never made it into
                ;; the output and the SGR style leaked past the cut. The
                ;; structure below keeps walking sentinels for free (their `w`
                ;; is zero, so `next` stays under budget) and only stops when a
                ;; visible grapheme would push past `max-cols`.
                (loop [i
                       0

                       used
                       0]

                  (if (>= i n)
                    (.toString sb)
                    (let [tc
                          ^TextCharacter (aget cells i)

                          gs
                          ^String (.getCharacterString tc)

                          w
                          (cond (inline-sentinel? gs) 0
                                (.isDoubleWidth tc) 2
                                :else 1)

                          next
                          (+ used w)]

                      (if (> next max-cols)
                        ;; A wide grapheme would have straddled the cut: drop it
                        ;; and pad with one space so the result's display-width
                        ;; is exactly `max-cols`. Sentinels can't reach this
                        ;; branch (their `w` is zero, so `next` never exceeds).
                        (do (when (< used max-cols) (.append sb \space)) (.toString sb))
                        ;; In-budget OR zero-width sentinel: append, advance,
                        ;; continue. For sentinels `w` is 0 so `used` stays put.
                        (do (.append sb gs) (recur (inc i) next)))))))))

(defn fold-cols
  "Character-fold `s` into a vector of segments, each at most `max-cols`
   display columns wide, never splitting a grapheme cluster.

   This is a terminal-style SOFT WRAP, the soft-wrap primitive shared by
   the agent code rail and the tool-result code blocks: unlike word-wrap it
   never drops or reflows whitespace — the bytes are preserved exactly and a
   break is inserted only at the column boundary. So a pathologically wide
   single line (a one-line `git_commit` message arg, a wide `clj_eval`
   value map) folds at the bubble edge instead of overflowing or being
   clipped, while indentation and in-row column alignment survive.

   The fold itself is lanterna's `TerminalTextUtils/foldColumns` — the same
   grapheme/EAW-aware text-flow family the screen paints with (`displayWidth`
   / `wordWrap` / `justify`). A line already within budget comes back `[s]`
   unchanged, so normal multi-line source is untouched and only the over-wide
   rows fold. nil/empty returns `[\"\"]`.

   Input carrying an ESC (0x1b) is returned unfolded — `foldColumns` is
   plain-text-only (the grapheme splitter throws on ESC); callers that need
   ANSI-aware width clip such rows separately."
  [s ^long max-cols]
  (let [^String s (str (or s ""))]
    (if (<= 0 (.indexOf s (int 27))) ; ESC (0x1b): plain-text-only primitive
      [s]
      (vec (TerminalTextUtils/foldColumns max-cols s)))))

(defn ansi-fold-cols
  "Like `fold-cols`, but ANSI-SGR aware: char-fold `s` into a vector of
   segments each at most `max-cols` display columns wide, never splitting a
   grapheme cluster and never counting an `\\u001b[..m` escape toward the
   width. The SGR sequence active at a cut is RE-OPENED at the head of the
   next segment (and the cut segment is closed with `\\u001b[0m`), so a
   syntax-highlighted line that folds keeps each token's color across the
   break instead of being clipped at the bubble edge.

   This is what lets a colorized fence (a wide `javascript:` bookmarklet, a
   long JSON row) wrap into the bubble the way a plain fence already does,
   with no ANSI leaking past a row. ESC-free input delegates to `fold-cols`
   (lanterna's grapheme/EAW-aware `foldColumns`); a segment always makes
   progress so a pathological width can't loop. nil/empty returns `[\"\"]`."
  [s ^long max-cols]
  (let [^String s
        (str (or s ""))

        budget
        (max 1 max-cols)]

    (if (neg? (.indexOf s (int 27))) ; no ESC: plain fold is enough
      (fold-cols s budget)
      (loop [rest
             s

             active
             ""

             ; SGR prefix to re-open on the next segment
             col
             0

             ^StringBuilder seg
             (StringBuilder.)

             out
             (transient [])]

        (cond (zero? (.length ^String rest)) (persistent! (conj! out (.toString seg)))
              (.startsWith ^String rest "\u001b[")
              (let [m (.indexOf ^String rest (int \m))]
                (if (neg? m)
                  ;; malformed trailing escape: keep it verbatim and stop
                  (persistent! (conj! out (.toString (.append seg ^String rest))))
                  (let [esc (subs rest 0 (inc m))
                        body (subs rest 2 m)
                        active' (if (contains? #{"" "0" "00"} body) "" (str active esc))]

                    (recur (subs rest (inc m)) active' col (.append seg esc) out))))
              :else
              (let [esc-idx
                    (.indexOf ^String rest "\u001b[")

                    run
                    (if (neg? esc-idx) rest (subs rest 0 esc-idx))

                    after
                    (if (neg? esc-idx) "" (subs rest esc-idx))

                    avail
                    (- budget col)

                    k
                    (col-prefix-end run avail)]

                (cond
                  ;; whole run fits on the current row
                  (>= k (.length ^String run))
                  (recur after active (+ col (display-width run)) (.append seg run) out)
                  ;; nothing more fits on a partial row: close it, restart fresh
                  (and (zero? k) (pos? col)) (recur (str run after)
                                                    active
                                                    0
                                                    (StringBuilder. ^String active)
                                                    (conj! out (str (.toString seg) "\u001b[0m")))
                  ;; overflow at a fresh row: emit what fits (force >=1 grapheme so
                  ;; a double-width glyph under a tiny budget still progresses),
                  ;; close the row, and continue on a new row carrying `active`.
                  :else (let [k
                              (if (zero? k)
                                (long (.length (.getCharacterString
                                                 ^TextCharacter
                                                 (aget (TextCharacter/fromString ^String run) 0))))
                                k)

                              head
                              (subs run 0 k)

                              tail
                              (subs run k)]

                          (recur (str tail after)
                                 active
                                 0
                                 (StringBuilder. ^String active)
                                 (conj! out
                                        (str (.toString (.append seg head)) "\u001b[0m")))))))))))
(defn ansi-slice-cols
  "Return the display-column WINDOW `[start, start+width)` of `s` as a string —
   the horizontal `less -S` clip the code pager paints each row with (CHOP, not
   fold). ANSI-SGR aware: `\u001b[..m` escapes never count toward a column, the
   SGR active at the window's LEFT edge is RE-OPENED at the head of the result,
   escapes that fall INSIDE the window are kept inline, and the result is closed
   with `\u001b[0m` whenever any SGR was emitted — so a syntax-highlighted row
   keeps its colors when scrolled sideways.

   ESC-free input is a plain grapheme-safe column slice (never splits a
   cluster). Negative `start` clamps to 0; non-positive `width` yields \"\"."
  [s ^long start ^long width]
  (let [^String s
        (str (or s ""))

        start
        (max 0 start)

        end
        (+ start (max 0 width))]

    (cond (<= width 0) ""
          ;; Plain text: two grapheme-safe prefix cuts bound the window exactly.
          (neg? (.indexOf s (int 27))) (subs s (col-prefix-end s start) (col-prefix-end s end))
          :else
          (loop [^String rest
                 s

                 active
                 ""

                 ; SGR prefix active at the cursor
                 col
                 0

                 ; display column of the next glyph
                 ^StringBuilder out
                 (StringBuilder.)

                 opened?
                 false

                 ; emitted the active-SGR head yet?
                 sgr?
                 false]

            ; emitted ANY escape (⇒ needs a reset)?
            (cond (or (zero? (.length rest)) (>= col end)) (do (when sgr? (.append out "\u001b[0m"))
                                                               (.toString out))
                  (.startsWith rest "\u001b[")
                  (let [m (.indexOf rest (int \m))]
                    (if (neg? m)
                      (do (when sgr? (.append out "\u001b[0m")) (.toString out))
                      (let [esc (subs rest 0 (inc m))
                            body (subs rest 2 m)
                            active' (if (contains? #{"" "0" "00"} body) "" (str active esc))]

                        ;; Escapes past the window's left edge paint inline; earlier
                        ;; ones only update `active` (re-opened when emitting starts).
                        (if opened?
                          (recur (subs rest (inc m)) active' col (.append out esc) opened? true)
                          (recur (subs rest (inc m)) active' col out opened? sgr?)))))
                  :else (let [esc-idx
                              (.indexOf rest "\u001b[")

                              run
                              (if (neg? esc-idx) rest (subs rest 0 esc-idx))

                              after
                              (if (neg? esc-idx) "" (subs rest esc-idx))

                              w
                              (display-width run)

                              run-end
                              (+ col w)

                              lo
                              (- (max start col) col)

                              ; cols to skip from run head
                              hi
                              (- (min end run-end) col)]

                          ; cols to keep to
                          (if (< lo hi)
                            (let [piece
                                  (subs run (col-prefix-end run lo) (col-prefix-end run hi))

                                  open
                                  (and (not opened?) (pos? (.length ^String active)))]

                              (when open (.append out ^String active))
                              (.append out ^String piece)
                              (recur after active run-end out true (or sgr? open)))
                            (recur after active run-end out opened? sgr?))))))))

;;; ── Inline-styled line painter ─────────────────────────────────────────────

(defn paint-styled-line!
  "Paint a single line at (x, y) honouring inline span sentinels.

   The line may contain interleaved text and `INLINE_*_ON`/`OFF`
   sentinels (range \\uE110...\\uE117). Sentinels themselves are NEVER
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
     wrapping italic - user-visible bug on every blockquote that
     contained inline emphasis.
   - Unmatched / dangling sentinels (e.g. an OFF without a prior ON,
     or a line that ends mid-bold) are tolerated: at exit we restore
     exactly the inherited modifier set, so SGR state never leaks past
     the call.
   - When NO sentinel appears in the line this collapses to a single
     `put-str!` call - the same as the old non-styled path - so the
     hot path is not penalised for the common case.

   This is the entry point that lets the markdown renderer support
   **bold** / *italic* / ~~strike~~ / `code` mid-line without
   replacing the marker-prefix-per-line architecture above."
  [^TextGraphics g x y ^String line base-fg base-bg code-fg code-bg]
  (let [;; Capture pre-existing modifiers so inline toggles can stack
        ;; on top of them and we can restore exactly at exit.
        inherited
        ^java.util.EnumSet (java.util.EnumSet/copyOf (.getActiveModifiers g))

        cells
        (TextCharacter/fromString line)

        n
        (alength cells)]

    (.setForegroundColor g base-fg)
    (.setBackgroundColor g base-bg)
    (cond (zero? n) nil
          ;; Fast path: no sentinels at all -> single putString. The
          ;; inherited modifiers are still active, so this paints with
          ;; whatever style the wrapping `styled` block enabled.
          (loop [i 0]
            (cond (>= i n) true
                  (inline-sentinel? (.getCharacterString ^TextCharacter (aget cells i))) false
                  :else (recur (inc i))))
          (.putString g (int x) (int y) line)
          ;; Slow path: walk graphemes, buffer text segments, flush on
          ;; style transitions. `inline` is the set of toggles activated
          ;; by sentinels we've seen so far; effective SGR set per flush
          ;; is `inherited ∪ inline`.
          :else
          (let [sb
                (StringBuilder.)

                inline
                (java.util.EnumSet/noneOf SGR)

                col
                (int-array 1 0)

                code?
                (boolean-array 1 false)

                flush!
                (fn []
                  (when (pos? (.length sb))
                    (let [seg (.toString sb)]
                      (.clearModifiers g)
                      (if (aget code? 0)
                        ;; Code span: hard-override fg/bg; modifiers
                        ;; cleared so code reads as a flat zone.
                        (do (.setForegroundColor g code-fg) (.setBackgroundColor g code-bg))
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

                (cond (= gs INLINE_BOLD_ON) (do (flush!) (.add inline SGR/BOLD))
                      (= gs INLINE_BOLD_OFF) (do (flush!) (.remove inline SGR/BOLD))
                      (= gs INLINE_ITALIC_ON) (do (flush!) (.add inline SGR/ITALIC))
                      (= gs INLINE_ITALIC_OFF) (do (flush!) (.remove inline SGR/ITALIC))
                      (= gs INLINE_STRIKE_ON) (do (flush!) (.add inline SGR/CROSSED_OUT))
                      (= gs INLINE_STRIKE_OFF) (do (flush!) (.remove inline SGR/CROSSED_OUT))
                      (= gs INLINE_CODE_ON) (do (flush!) (aset code? 0 true))
                      (= gs INLINE_CODE_OFF) (do (flush!) (aset code? 0 false))
                      (= gs INLINE_LINK_ON) (do (flush!) (.add inline SGR/UNDERLINE))
                      (= gs INLINE_LINK_OFF) (do (flush!) (.remove inline SGR/UNDERLINE))
                      :else (.append sb gs))))
            (flush!)
            ;; Restore the inherited modifier set exactly so the wrapping
            ;; `styled` form's cleanup sees the state it expects. Without
            ;; this, an unbalanced sentinel (e.g. line ending mid-bold)
            ;; could leak BOLD into the next paint call.
            (.clearModifiers g)
            (.setForegroundColor g base-fg)
            (.setBackgroundColor g base-bg)
            (when-not (.isEmpty inherited) (.enableModifiers g (into-array SGR inherited)))))))

;;; ── Flex layout (pure string functions, column-aware) ─────────────────────

(defn pad-right
  "Pad string to `w` terminal columns, right-filling with spaces.
   Truncates (column-aware) if too wide."
  [s w]
  (let [txt
        (or s "")

        cols
        (display-width txt)

        w
        (long w)]

    (cond (= cols w) txt
          (> cols w) (truncate-cols txt w)
          :else (str txt (apply str (repeat (- w cols) \space))))))

(defn pad-left
  "Pad string to `w` terminal columns, left-filling with spaces.
   Truncates (column-aware) if too wide."
  [s w]
  (let [txt
        (or s "")

        cols
        (display-width txt)

        w
        (long w)]

    (cond (= cols w) txt
          (> cols w) (truncate-cols txt w)
          :else (str (apply str (repeat (- w cols) \space)) txt))))

(defn center-text
  "Center string within `w` terminal columns, padding both sides.
   Truncates (column-aware) if too wide."
  [s w]
  (let [txt
        (or s "")

        cols
        (display-width txt)

        w
        (long w)]

    (cond (>= cols w) (truncate-cols txt w)
          :else
          (let [left-pad
                (quot (- w cols) 2)

                right-pad
                (- w cols left-pad)]

            (str (apply str (repeat left-pad \space)) txt (apply str (repeat right-pad \space)))))))

(defn space-between
  "Distribute items across `w` terminal columns with equal gaps.
   First item flush-left, last item flush-right, rest evenly spaced.
   Like CSS justify-content: space-between."
  [items w]
  (let [n
        (count items)

        w
        (long w)]

    (cond (zero? n) (apply str (repeat w \space))
          (= n 1) (center-text (first items) w)
          :else (let [total-text
                      (long (reduce + (map display-width items)))

                      total-gaps
                      (- w total-text)

                      gap-count
                      (dec n)

                      base-gap
                      (max 1 (quot total-gaps gap-count))

                      extra
                      (- total-gaps (* base-gap gap-count))]

                  (apply str
                    (interleave items
                                (concat
                                  ;; Distribute remainder across first gaps
                                  (map (fn [i]
                                         (apply str
                                           (repeat (+ base-gap (if (< (long i) (long extra)) 1 0))
                                                   \space)))
                                       (range gap-count))
                                  ;; sentinel so interleave doesn't drop last item
                                  [""])))))))

(defn space-around
  "Distribute items across `w` terminal columns with equal space
   around each item. Like CSS justify-content: space-around."
  [items w]
  (let [n
        (count items)

        w
        (long w)]

    (cond (zero? n) (apply str (repeat w \space))
          (= n 1) (center-text (first items) w)
          :else (let [total-text
                      (long (reduce + (map display-width items)))

                      total-gaps
                      (- w total-text)

                      slots
                      (* 2 n)

                      ;; each item gets space on both sides
                      base
                      (max 0 (quot total-gaps slots))

                      unit-gap
                      (apply str (repeat base \space))

                      ;; Build: gap item gap | gap item gap | ...
                      parts
                      (mapcat (fn [item]
                                [unit-gap item unit-gap])
                              items)

                      result
                      (apply str parts)

                      result-w
                      (display-width result)]

                  (cond (= result-w w) result
                        (< result-w w) (str result (apply str (repeat (- w result-w) \space)))
                        :else (truncate-cols result w))))))

(defn v-center-offset
  "Compute vertical offset to center `content-h` rows within `container-h` rows."
  [content-h container-h]
  (if (< content-h container-h) (quot (- container-h content-h) 2) 0))

;;; ── Word-wrap & justification ──────────────────────────────────────────────
;; Backed by the native, grapheme/EAW-aware `TerminalTextUtils` methods in the
;; lanterna fork, so wrap points and justified widths match what the screen
;; paints — one implementation, shared by Clojure and Java. Plain text only
;; (no inline-style sentinels / ANSI); styled-run wrapping lives in render*.

(defn word-wrap
  "Greedy word-wrap `s` into a vec of lines, each fitting `width` DISPLAY
   columns. Breaks on whitespace; a token wider than `width` is hard-split at
   grapheme boundaries (never mid-cluster); embedded newlines are honoured.
   Blank input or `width` <= 0 yields `[\"\"]`."
  [s width]
  (vec (TerminalTextUtils/wordWrap (int width) (str (or s "")))))

(defn justify-line
  "Full-justify the words on `line` to EXACTLY `width` display columns by
   distributing inter-word spaces (flush to both margins). A blank or
   single-word line is left-aligned (right-padded) instead — nothing to
   stretch."
  [line width]
  (TerminalTextUtils/justifyLine (str (or line "")) (int width)))

(defn justify
  "Word-wrap `s` to `width` columns, then FULL-JUSTIFY every line, leaving each
   paragraph's final line left-aligned (the standard typographic convention).
   Pass `justify-last?` true to stretch the final line too. Returns a vec of
   lines."
  ([s width] (justify s width false))
  ([s width justify-last?]
   (vec (TerminalTextUtils/justify (int width) (str (or s "")) (boolean justify-last?)))))

(defn align
  "Align `s` to `width` display columns by `mode`:
     :left    pad on the right (default for any unknown mode)
     :right   pad on the left
     :center  pad both sides
     :justify full-justify the words (flush both margins)."
  [s width mode]
  (case mode
    :right
    (pad-left s width)

    :center
    (center-text s width)

    :justify
    (justify-line s width)

    (pad-right s width)))

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

;;; ── Tabs ───────────────────────────────────────────────────────────────────

(def ^:private tab-default-gap 1)

(defn tab-display-label
  "Compact label for a tab map.

   Supported tab keys:
   - `:label` / `:id` - visible base label.
   - `:dirty?` - appends a dirty marker.
   - `:state` - appends a compact state badge for running / verified /
     accepted / error tabs.

   This is terminal-column aware when later passed through `tab-layout`, so
   callers may use emoji or CJK labels without hand-padding."
  [{:keys [id label dirty? state]}]
  (let [base (or label id "")]
    (str (if (keyword? base) (name base) base)
         (when dirty? " •")
         (case state
           :running
           " ▶"

           :verified
           " ✓"

           :accepted
           " ✓"

           :error
           " !"

           nil))))

(defn tab-layout
  "Return tab geometry for a horizontal tab strip.

   `tabs` is a seq of maps. Returned maps include `:left`, `:width`,
   `:active?`, and `:text` truncated to fit `:width`. Width math uses terminal
   columns, not Java chars. When the strip is too narrow, later tabs may get
   zero width; draw helpers skip those safely."
  ([tabs left width active-id] (tab-layout tabs left width active-id {}))
  ([tabs left width active-id {:keys [gap]}]
   (let [tabs
         (vec tabs)

         n
         (count tabs)

         left
         (long left)

         width
         (max 0 (long width))

         gap
         (max 0 (long (or gap tab-default-gap)))]

     (if (or (zero? n) (zero? width))
       []
       (let [gap
             (if (>= width (+ n (* gap (dec n)))) gap 0)

             gap-total
             (* gap (dec n))

             content-w
             (max 0 (- width gap-total))

             base
             (quot content-w n)

             extra
             (rem content-w n)]

         (loop [idx
                0

                x
                left

                out
                []]

           (if (= idx n)
             out
             (let [w
                   (+ base (if (< idx extra) 1 0))

                   tab
                   (nth tabs idx)

                   text
                   (truncate-cols (tab-display-label tab) w)

                   active?
                   (if (some? active-id) (= (:id tab) active-id) (true? (:active? tab)))

                   next-x
                   (+ x w (if (= idx (dec n)) 0 gap))]

               (recur (inc idx)
                      next-x
                      (conj out
                            (assoc tab
                              :left x
                              :width w
                              :active? active?
                              :text text)))))))))))

(defn tab-at
  "Return the tab geometry under `col`, or nil. Expects `tab-layout` output."
  [layout col]
  (let [col (long col)]
    (some
      (fn [{:keys [left width] :as tab}]
        (when (and (pos? (long width)) (>= col (long left)) (< col (+ (long left) (long width))))
          tab))
      layout)))

(defn draw-tabs!
  "Draw a tab strip and return its geometry.

   Required opts: `:left`, `:row`, `:width`, `:active-id`, `:fg`, `:bg`,
   `:active-fg`, `:active-bg`, `:inactive-fg`, `:inactive-bg`.
   Optional: `:gap`, `:bordered?`.

   This primitive knows layout and drawing only. Callers own domain actions and
   click-region registration."
  [g tabs
   {:keys [left row width active-id gap fg bg active-fg active-bg inactive-fg inactive-bg
           bordered?]}]
  (let [layout (tab-layout tabs left width active-id {:gap gap})]
    (set-colors! g (or fg inactive-fg active-fg) (or bg inactive-bg active-bg))
    (fill-rect! g left row width 1)
    (doseq [{:keys [left width active? text]} layout
            :when (pos? (long width))]

      (clear-styles! g)
      (if active?
        (do (set-colors! g active-fg active-bg) (enable! g BOLD))
        (do (set-colors! g inactive-fg inactive-bg) (enable! g ITALIC)))
      (when bordered? (enable! g BORDERED))
      (fill-rect! g left row width 1)
      (draw-centered! g left row width text))
    (clear-styles! g)
    layout))

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

(def MARKER_THINKING "\u200B")  ;; zero-width space       -> italic, dim (reasoning)
(def MARKER_CODE "\u200C")  ;; zero-width non-joiner  -> code style
(def MARKER_RESULT "\u200D")  ;; zero-width joiner      -> result/return value (success)
(def MARKER_SEP "\u2060")  ;; word-joiner            -> separator line
(def MARKER_CODE_OK "\u2061")  ;; function application   -> code with success status
(def MARKER_CODE_ERR "\u2062")  ;; invisible times        -> code with error status
(def MARKER_ERR_RESULT "\u2063")  ;; invisible separator    -> error result line
(def MARKER_DURATION "\u2064")  ;; invisible plus         -> duration annotation
(def MARKER_ITERATION_HDR "\u2066")  ;; LRI                    -> iteration header with bg
(def MARKER_ANSWER_SEP "\u2069")  ;; PDI                    -> answer separator (trace->answer break)
(def MARKER_CODE_PAD "\u206A")  ;; ISS                    -> running/neutral code block padding line
(def MARKER_CODE_ERR_PAD "\u206B") ;; ASS                   -> error code block padding line
(def MARKER_CODE_OK_PAD "\uE000") ;; PUA                    -> successful code block padding line
(def MARKER_RECAP "\uE00E") ;; PUA                    -> iteration recap line (header-bg, bold+italic)
(def MARKER_ITERATION_PAD "\u206C")  ;; IAFS                   -> iteration zone padding (margin between blocks)
(def MARKER_ANSWER_HDR "\u206D")  ;; AAFS                   -> final answer header
(def MARKER_ANSWER_TXT "\u206E")  ;; NADS                   -> answer text line (with answer bg)
(def MARKER_ANSWER_PAD "\u206F")  ;; NODS                   -> answer padding line
;; Markdown markers (PUA \uE000+) - guaranteed unique, never collide
;; with the iteration/answer markers above. Two parallel sets:
;;   - MARKER_MD_*    -> answer-zone markdown (answer-bg)
;;   - MARKER_TH_MD_* -> thinking-zone markdown (iteration-header-bg, italic)
(def MARKER_MD_H1 "\uE001") ;; markdown heading 1 (answer)
(def MARKER_MD_H2 "\uE002") ;; markdown heading 2 (answer)
(def MARKER_MD_H3 "\uE003") ;; markdown heading 3 (answer)
(def MARKER_MD_BOLD "\uE004") ;; markdown bold line (answer)
(def MARKER_MD_CODE "\uE005") ;; markdown fenced code (answer)
(def MARKER_MD_BULLET "\uE006") ;; markdown bullet list item (answer)
(def MARKER_MD_TABLE_HEAD "\uE007") ;; markdown table header row (answer)
(def MARKER_MD_TABLE_SEP "\uE008") ;; markdown table border / separator (answer)
(def MARKER_MD_TABLE_ROW "\uE009") ;; markdown table data row (answer)
(def MARKER_MD_QUOTE "\uE00A") ;; markdown blockquote (answer)
(def MARKER_MD_HR "\uE00B") ;; markdown horizontal rule (answer)
(def MARKER_MD_SUMMARY "\uE00C") ;; markdown <summary> disclosure label (answer)
(def MARKER_OP_ROW "\uE00F") ;; BLOCK op row -> black-on-white badge (answer-fg/bg), \u25B6/\u25BC disclosure
(def MARKER_HINT "\uE010") ;; affordance hint (e.g. "↑ to edit") -> accent fg on regular terminal bg, NOT the queue band
(def MARKER_QUEUE_HDR "\uE011") ;; "Messages Queue" section header -> bold accent fg on regular bg, leading bar glyph (NOT the gray band)
(def MARKER_QUEUE_ITEM "\uE012") ;; queued message row -> ordinal in accent gutter on regular bg, preview text on the gray queue band
(def MARKER_QUEUE_BORDER "\uE013") ;; queue bottom border -> accent corner + horizontal rule that caps the left rail, above the edit hint

(def MARKER_TH_MD_H1 "\uE021") ;; markdown heading 1 (thinking)
(def MARKER_TH_MD_H2 "\uE022") ;; markdown heading 2 (thinking)
(def MARKER_TH_MD_H3 "\uE023") ;; markdown heading 3 (thinking)
(def MARKER_TH_MD_BOLD "\uE024") ;; markdown bold line (thinking)
(def MARKER_TH_MD_CODE "\uE025") ;; markdown fenced code (thinking)
(def MARKER_TH_MD_BULLET "\uE026") ;; markdown bullet list item (thinking)
(def MARKER_TH_MD_TABLE_HEAD "\uE027") ;; markdown table header row (thinking)
(def MARKER_TH_MD_TABLE_SEP "\uE028") ;; markdown table border (thinking)
(def MARKER_TH_MD_TABLE_ROW "\uE029") ;; markdown table data row (thinking)
(def MARKER_TH_MD_QUOTE "\uE02A") ;; markdown blockquote (thinking)
(def MARKER_TH_MD_HR "\uE02B") ;; markdown horizontal rule (thinking)
(def MARKER_TH_MD_SUMMARY "\uE02C") ;; markdown <summary> disclosure label (thinking)

;; Inline span sentinels (\uE110...\uE117) live in their own section
;; near the top of this file because both the width math
;; (`display-width` etc.) AND the painter (`paint-styled-line!`)
;; need them; defining them here would force a forward declare.
;; Search UP for `INLINE_BOLD_ON`.
