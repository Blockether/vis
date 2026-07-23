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
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize Symbols TerminalTextUtils
            TextCharacter TextColor]
           [com.googlecode.lanterna.graphics TextGraphics]))

;;; ── Numeric ─────────────────────────────────────────────────────────────────

(defn clamp
  "Clamp `x` into the inclusive range [lo, hi]. Primitive-long in and out —
   the one canonical range clamp every dialog / header / provider view shares.
   Delegates to the lanterna fork's `TerminalTextUtils/clamp` (>= 3.1.5-vis.27)
   so the whole channel reuses ONE primitive range clamp."
  ^long [^long x ^long lo ^long hi]
  (TerminalTextUtils/clamp x lo hi))

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

;;; ── Cached line blitting ─────────────────────────────────────────────────────
;; `.putString(String)` re-segments the whole line into a fresh `TextCharacter[]`
;; (grapheme walk + N substrings + N TextCharacter objects) on EVERY call — ~9.3KB
;; for an 80-col line, allocated and thrown away every frame. But a STABLE line
;; repaints byte-identical tick after tick (only the live/streaming bubble's text
;; churns), so we segment ONCE, cache the array, and paint it via the fork's
;; zero-parse `putString(TextCharacter[])` overload (>= 3.1.5-vis.33) — 0 alloc on
;; a hit. Measured: 9296 B / 5266 ns  →  0 B / 451 ns per 80-col line.
;;
;; PURE-FUNCTION cache: the array is fully determined by [line fg bg mods]
;; (`fromString` is deterministic), so a raced or LRU-evicted entry can only ever
;; be RECOMPUTED to an equal value. No generation guard is needed here — unlike
;; the render/virtual caches whose keys under-approximate content and can serve
;; stale VALUES; this key IS the content. Bounded access-order LRU caps memory;
;; the only structural mutation (access-order `.get`) is guarded by the
;; synchronized-map wrapper.
(def ^:private ^:const line-cell-cap 4096)

(def ^:private ^java.util.Map line-cell-cache
  (java.util.Collections/synchronizedMap
    (proxy [java.util.LinkedHashMap] [1024 0.75 true]
      (removeEldestEntry [_] (> (.size ^java.util.LinkedHashMap this) (long line-cell-cap))))))

(defn invalidate-line-cells!
  "Drop every cached segmented line. Safe to call anytime (pure-function cache —
   worst case is recompute). Wired to the same settings/theme busts as the other
   caches so a color/width change can't paint a stale array."
  []
  (.clear line-cell-cache))

(def ^:private ^java.util.EnumSet no-mods (java.util.EnumSet/noneOf SGR))

(defn blit-line!
  "Paint `line` at (x,y) in fg/bg with SGR `mods`, caching the segmented
   `TextCharacter[]` across frames and painting it via the fork's pre-segmented
   `putString` overload (0 alloc on a cache hit).

   CONTRACT: `line` must already be sanitized + tab-expanded (no raw C0 controls,
   tabs, or newlines) — the same guarantee the markdown projector and `put-str!`s
   `sanitize-for-lanterna` provide — because the array path does NOT run
   lanterna's `prepareStringForPut` (tab expansion / newline truncation). For
   already-clean single lines it is byte-identical to `.putString(String)`,
   including double-width CJK/emoji column advance."
  [^TextGraphics g x y ^String line ^TextColor fg ^TextColor bg ^java.util.EnumSet mods]
  (let
    [mempty?
     (.isEmpty mods)

     ;; Lookup key: for the common no-modifier line, a shared `:none` token
     ;; avoids touching the EnumSet at all. For a styled run the LIVE `mods`
     ;; set discriminates by content (EnumSet hashCode/equals are
     ;; content-based); it is only READ here, never stored.
     k
     (if mempty? [line fg bg :none] [line fg bg mods])

     ^"[Lcom.googlecode.lanterna.TextCharacter;" cached
     (.get line-cell-cache k)]

    (if cached
      (.putString g (int x) (int y) cached)
      (let [arr (TextCharacter/fromString line fg bg (if mempty? no-mods mods))]
        ;; The stored key must own an IMMUTABLE mods snapshot: a later
        ;; enable/disableModifiers on the caller's live set must not mutate our
        ;; key out from under the map.
        (.put line-cell-cache (if mempty? k [line fg bg (java.util.EnumSet/copyOf mods)]) arr)
        (.putString g (int x) (int y) arr)))
    g))

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

(def ^:const BOX_T_DOWN Symbols/SINGLE_LINE_T_DOWN)

(def ^:const BOX_T_UP Symbols/SINGLE_LINE_T_UP)

(def ^:const BOX_CROSS Symbols/SINGLE_LINE_CROSS)

(defn- int-widths ^ints [widths] (int-array (map #(int (long %)) widths)))

(defn horiz-line
  "Return a string of `n` horizontal box-drawing chars. Delegates to lanterna's
   primitive terminal-rule builder instead of rebuilding via Clojure seqs."
  ^String [n]
  (TerminalTextUtils/repeat BOX_H (int n)))

(defn joined-horiz-line
  "Return horizontal runs joined by `junction`, e.g. `──┬───`."
  ^String [widths junction]
  (TerminalTextUtils/joinedLine BOX_H (int-widths widths) (char junction)))

(defn boxed-horiz-line
  "Return a bordered horizontal rule, e.g. `┌──┬───┐`."
  ^String [widths left junction right]
  (TerminalTextUtils/boxedLine (int-widths widths) (char left) BOX_H (char junction) (char right)))

;;; ── Selection marker (dot cursor component) ─────────────────────
;;
;; Universal cursor marker for every up/down navigable list in the
;; TUI: dialogs (select, settings, sessions, file picker,
;; resources, providers, models), command palette, slash overlay,
;; etc.
;;
;; Selected rows use one left-anchored `•`, not row inversion and not
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
   exactly fills the reserved gutter. `•` is the project-wide selector
   dot, kept bare (no VS-16) to avoid terminal width surprises."
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
  (let
    [col
     (long col)

     row
     (long row)]

    (set-colors! g fg bg)
    (put-str! g col row glyph)
    (+ col STATUS_WIDTH)))

;;; ── Composite primitives ──────────────────────────────────────────────────

(defn draw-box!
  "Draw a single-line bordered box at (left, top) of size wxh.
   Draws corners, edges. Does NOT fill interior."
  [g left top w h]
  (let
    [left
     (long left)

     top
     (long top)

     w
     (long w)

     h
     (long h)

     right
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
    (doseq [^long r (range (inc top) bottom)]
      (set-char! g left r BOX_V)
      (set-char! g right r BOX_V))
    g))

(defn draw-separator!
  "Draw a horizontal separator with T-junctions at left/right edges."
  [g left right row]
  (let
    [left
     (long left)

     right
     (long right)

     row
     (long row)

     inner
     (- right left 1)]

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

(def INLINE_ERR_ON "\uE11A")

(def INLINE_ERR_OFF "\uE11B")

(def ^:private ^:const INLINE_SENTINEL_LO 0xE110)

(def ^:private ^:const INLINE_SENTINEL_HI 0xE11B)

(defn inline-sentinel?
  "True when `g` (a single grapheme String) is one of the eight inline
   span sentinels above. Cheap range check, no map lookup."
  [^String g]
  (and (= 1 (.length g))
       (let [c (int (.charAt g 0))]
         (and (>= c (long INLINE_SENTINEL_LO)) (<= c (long INLINE_SENTINEL_HI))))))


;; Per-terminal glyph width (VS-16 emoji narrow on Apple Terminal.app, wide
;; elsewhere) lives in the lanterna fork's `TextCharacter.isDoubleWidth`
;; (auto-detected from TERM_PROGRAM). Everything here defers to it, so
;; measurement always matches what the fork's painter/screen emit.


(defn display-width
  "Number of terminal columns `s` will occupy when painted by lanterna.

   Delegates to the lanterna fork's `TerminalTextUtils/displayColumns`
   (>= 3.1.5-vis.24): grapheme clusters honoured, CJK + emoji counted as
   two columns, ASCII as one, inline-span sentinels (\\uE110..\\uE119) as
   zero, C0 control bytes sanitized to `/` first. This measures by the
   exact same rule `AbstractTextGraphics.putString` paints by, so what we
   measure always matches what the renderer emits.

   `s` is coerced via `(str s)` first: a caller that maps display-width
   over a string (handing us a Character / number) must never crash the
   render thread with a ClassCastException. nil → 0.

   Returns 0 for nil/empty input."
  ^long [s]
  (if (nil? s)
    0
    (let [^String s (str s)]
      (TerminalTextUtils/displayColumns
        (if (or (>= (.indexOf s (int 0xE11A)) 0) (>= (.indexOf s (int 0xE11B)) 0))
          (let [^String stripped (.replace s ^String INLINE_ERR_ON "")]
            (.replace stripped ^String INLINE_ERR_OFF ""))
          s)))))

(def ^:const tab-width
  "Columns the lanterna fork's `putString` advances a hard TAB to. The fork
   expands tabs to FIXED tab stops at paint time; kept in sync here so tab
   expansion done before layout matches what the painter would emit."
  4)

(defn expand-tabs
  "Replace every hard TAB in `s` with spaces up to the next `tab-width` tab
   stop, counted from the START of `s` (column 0).

   WHY: the fork's `putString` expands tabs to fixed 4-column stops at PAINT
   time, but the width measure / soft-wrap (`display-width` / `fold-cols`,
   which sanitize a TAB to a single `/`) count each tab as ONE column. So
   `\t`-bearing tool output (e.g. `gh issue list`, any tab-separated columns)
   was folded as if tabs were 1 column wide, then OVERFLOWED the bubble's
   right edge once paint re-expanded them. Expanding up front makes
   measure == fold == paint and leaves `putString` no tabs to re-expand.

   Column tracking is by char count (ASCII tool output); nil/empty → \"\".
   Tab-free input is returned unchanged (same instance). Backed by the
   lanterna fork's `TerminalTextUtils/expandTabs` (>= 3.1.5-vis.30)."
  ^String [s]
  (TerminalTextUtils/expandTabs (str (or s "")) (int tab-width)))

(defn col-prefix-end
  "Return the char-index `i` such that `(subs s 0 i)` is the longest
   prefix of `s` whose `display-width` is <= `max-cols` AND that does NOT
   split a grapheme cluster.

   Use this when you need both the prefix and the position of the
   un-consumed remainder (e.g. in word-wrapping). For just the prefix,
   `truncate-cols` is friendlier.

   Delegates to the lanterna fork's `TerminalTextUtils/columnPrefixLength`
   (>= 3.1.5-vis.24). Returns 0 for nil/empty or non-positive `max-cols`."
  ^long [s ^long max-cols]
  (TerminalTextUtils/columnPrefixLength ^String s (int max-cols)))

(defn truncate-cols
  "Return the longest prefix of `s` that fits in at most `max-cols`
   terminal columns, never splitting a grapheme cluster.

   Edge cases honoured:
   - nil or `max-cols <= 0` returns `\"\"`.
   - If a double-width grapheme would straddle the cut, it is dropped
     (NOT half-included), and one space is appended in its place so the
     returned string's `display-width` is exactly `max-cols`. This keeps
     `pad-right` / `pad-left` idempotent under repeated truncation.
   - Trailing zero-width inline-span sentinels (style closers) are always
     emitted, never stranded past the cut.

   Delegates to the lanterna fork's `TerminalTextUtils/truncateColumns`
   (>= 3.1.5-vis.24)."
  ^String [s ^long max-cols]
  (TerminalTextUtils/truncateColumns ^String s (int max-cols)))

(defn ellipsize
  "Shorten `s` to at most `max-cols` terminal columns, appending an ellipsis
   `marker` (default `…`) when truncation happens. Grapheme-cluster + EAW safe
   and WIDTH-CORRECT — the marker's own display width is charged against the
   budget, so the result never exceeds `max-cols` (even for tiny budgets, where
   the marker itself is truncated to fit). nil `s` → treated as empty;
   `max-cols <= 0` → `\"\"`; a string already within budget is returned as-is.

   The single canonical column-ellipsis for the TUI — `dialogs`/`table`/`header`
   delegate here. Backed by the lanterna fork's `TerminalTextUtils/ellipsize`
   (>= 3.1.5-vis.24)."
  (^String [s ^long max-cols] (ellipsize s max-cols "…"))
  (^String [s ^long max-cols ^String marker]
   (TerminalTextUtils/ellipsize (str s) (int max-cols) marker)))

(defn truncate-middle
  "Shorten `s` to at most `max-cols` columns by ELIDING THE MIDDLE behind a
   single `…`, keeping both the HEAD and the TAIL. Ideal for file paths, where
   the basename (tail) is as informative as the leading dirs — plain
   `truncate-cols` drops the filename. Falls back to head truncation when there
   isn't room for both sides plus the ellipsis. Grapheme-cluster safe.

   Backed by the lanterna fork's `TerminalTextUtils/truncateMiddle`
   (>= 3.1.5-vis.25)."
  ^String [s ^long max-cols]
  (TerminalTextUtils/truncateMiddle (some-> s
                                            str)
                                    (int max-cols)))

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
   next segment (the cut segment closed with `\\u001b[0m`), so a
   syntax-highlighted line that folds keeps each token's color across the
   break instead of being clipped at the bubble edge.

   This is lanterna's `TerminalTextUtils/ansiFoldColumns` (>= 3.1.5-vis.26) —
   the same grapheme/EAW-aware column engine as `fold-cols`/`display-width`;
   ESC-free input takes the plain fold fast path. nil/empty returns `[\"\"]`."
  [s ^long max-cols]
  (vec (TerminalTextUtils/ansiFoldColumns max-cols (str (or s "")))))

(defn ansi-slice-cols
  "Return the display-column WINDOW `[start, start+width)` of `s` as a string —
   the horizontal `less -S` clip the code pager paints each row with (CHOP, not
   fold). ANSI-SGR aware: `\\u001b[..m` escapes never count toward a column, the
   SGR active at the window's LEFT edge is RE-OPENED at the head of the result,
   escapes that fall INSIDE the window are kept inline, and the result is closed
   with `\\u001b[0m` whenever any SGR was emitted — so a syntax-highlighted row
   keeps its colors when scrolled sideways.

   This is lanterna's `TerminalTextUtils/ansiSliceColumns` (>= 3.1.5-vis.26).
   ESC-free input is a plain grapheme-safe column slice (never splits a
   cluster). Negative `start` clamps to 0; non-positive `width` yields \"\"."
  [s ^long start ^long width]
  (TerminalTextUtils/ansiSliceColumns (str (or s "")) start width))

(defn ansi-truncate-cols
  "ANSI-SGR-aware column TRUNCATE (hard CLIP to a prefix): keep at most `max-cols`
   display columns of `s`, never splitting a grapheme and never counting an
   `\\u001b[..m` escape toward the width. Escapes are kept inline VERBATIM (no
   re-open / trailing reset — unlike `ansi-slice-cols`); a malformed / non-SGR
   control escape renders as a single middle dot so a raw ESC (0x1b) never reaches
   the grapheme splitter (which throws on it, blanking the TUI before first paint).

   This is lanterna's `TerminalTextUtils/ansiTruncateColumns` (>= 3.1.5-vis.29) —
   the CHOP sibling of `ansi-fold-cols`/`ansi-slice-cols`, same grapheme/EAW-aware
   column engine. nil/empty or `max-cols <= 0` yields \"\"."
  ^String [s ^long max-cols]
  (TerminalTextUtils/ansiTruncateColumns (str (or s "")) (int max-cols)))

;;; ── Inline-styled line painter ─────────────────────────────────────────────

(defn- build-styled-cells
  "Resolve a sentinel-carrying `line` into ONE fully-styled `TextCharacter[]`.
   Inline sentinels toggle SGR/code state but occupy no columns, so the
   concatenated per-run `fromString` cells are EXACTLY what the per-run
   `putString` walk paints (proven byte-identical). `inherited` is the SGR set
   active on `g` at entry; inline toggles stack on top of it. Built once per
   unique styled line, then cached by `blit-styled-line!`."
  ^"[Lcom.googlecode.lanterna.TextCharacter;"
  [^String line ^TextColor base-fg ^TextColor base-bg ^TextColor code-fg ^TextColor code-bg
   ^TextColor err-fg ^java.util.EnumSet inherited]
  (let
    [len
     (.length line)

     inline
     (java.util.EnumSet/noneOf SGR)

     code?
     (boolean-array 1 false)

     err?
     (boolean-array 1 false)

     ^java.util.ArrayList out
     (java.util.ArrayList. len)

     emit!
     (fn [^String seg]
       (when (pos? (.length seg))
         (let
           [^"[Lcom.googlecode.lanterna.TextCharacter;" arr
            (if (aget code? 0)
              (TextCharacter/fromString seg code-fg code-bg no-mods)
              (let [eff ^java.util.EnumSet (java.util.EnumSet/copyOf inherited)]
                (.addAll eff inline)
                (TextCharacter/fromString seg (if (aget err? 0) err-fg base-fg) base-bg eff)))

            n
            (alength arr)]

           (loop [i 0]
             (when (< i n) (.add out (aget arr i)) (recur (inc i)))))))]

    (loop
      [i
       0

       run-start
       0]

      (if (>= i len)
        (emit! (subs line run-start i))
        (let [c (int (.charAt line i))]
          (if (and (>= c INLINE_SENTINEL_LO) (<= c INLINE_SENTINEL_HI))
            (let [gs (subs line i (inc i))]
              (emit! (subs line run-start i))
              (cond (= gs INLINE_BOLD_ON) (.add inline SGR/BOLD)
                    (= gs INLINE_BOLD_OFF) (.remove inline SGR/BOLD)
                    (= gs INLINE_ITALIC_ON) (.add inline SGR/ITALIC)
                    (= gs INLINE_ITALIC_OFF) (.remove inline SGR/ITALIC)
                    (= gs INLINE_STRIKE_ON) (.add inline SGR/CROSSED_OUT)
                    (= gs INLINE_STRIKE_OFF) (.remove inline SGR/CROSSED_OUT)
                    (= gs INLINE_CODE_ON) (aset code? 0 true)
                    (= gs INLINE_CODE_OFF) (aset code? 0 false)
                    (= gs INLINE_LINK_ON) (.add inline SGR/UNDERLINE)
                    (= gs INLINE_LINK_OFF) (.remove inline SGR/UNDERLINE)
                    (= gs INLINE_ERR_ON) (aset err? 0 true)
                    (= gs INLINE_ERR_OFF) (aset err? 0 false))
              (recur (inc i) (inc i)))
            (recur (inc i) run-start)))))
    (.toArray out
              ^"[Lcom.googlecode.lanterna.TextCharacter;" (make-array TextCharacter (.size out)))))

(defn blit-styled-line!
  "Paint a sentinel-carrying `line` at (x,y), caching its fully-resolved
   `TextCharacter[]` across frames and painting it via the fork's pre-segmented
   `putString` overload (0 alloc on a hit). Byte-identical to the per-run
   `putString` walk. Leaves g's SGR/color state untouched (each cell carries its
   own style), so the `inherited` set active at entry is preserved for the
   wrapping `styled` form -- no explicit restore needed. Shares `line-cell-cache`
   with `blit-line!` via a `:sty`-tagged key (distinct shape, never collides)."
  [^TextGraphics g x y ^String line ^TextColor base-fg ^TextColor base-bg ^TextColor code-fg
   ^TextColor code-bg ^TextColor err-fg ^java.util.EnumSet inherited]
  (let
    [iempty?
     (.isEmpty inherited)

     k
     (if iempty?
       [:sty line base-fg base-bg code-fg code-bg err-fg]
       [:sty line base-fg base-bg code-fg code-bg err-fg inherited])

     ^"[Lcom.googlecode.lanterna.TextCharacter;" cached
     (.get line-cell-cache k)]

    (if cached
      (.putString g (int x) (int y) cached)
      (let [arr (build-styled-cells line base-fg base-bg code-fg code-bg err-fg inherited)]
        (.put line-cell-cache
              (if iempty?
                k
                [:sty line base-fg base-bg code-fg code-bg err-fg
                 (java.util.EnumSet/copyOf inherited)])
              arr)
        (.putString g (int x) (int y) arr)))
    g))

(defn paint-styled-line!
  "Paint a single line at (x, y) honouring inline span sentinels.

   The line may contain interleaved text and `INLINE_*_ON`/`OFF`
   sentinels (range \\uE110..\\uE11B). Sentinels themselves are NEVER
   painted; they toggle the SGR style (BOLD/ITALIC/CROSSED-OUT/UNDERLINE),
   the fg/bg colors (CODE), or the foreground to `err-fg` (ERR) for the
   spans that follow.

   `base-fg` / `base-bg` are the colors for non-code, non-error spans.
   Code spans force `code-fg` / `code-bg` until INLINE_CODE_OFF; ERR spans
   force `err-fg` (on `base-bg`) until INLINE_ERR_OFF. The 8-arg arity
   defaults `err-fg` to `base-fg`, so an ERR sentinel is a no-op unless the
   caller opts in with the 9-arg arity.

   Robustness: walks by grapheme cluster, inherits any SGR modifiers active
   on `g` at entry, tolerates dangling sentinels, and collapses to a single
   cached blit when the line carries no sentinel (the common ASCII case)."
  ([^TextGraphics g x y ^String line base-fg base-bg code-fg code-bg]
   (paint-styled-line! g x y line base-fg base-bg code-fg code-bg base-fg))
  ([^TextGraphics g x y ^String line base-fg base-bg code-fg code-bg err-fg]
   (let
     [len
      (.length line)

      has-sentinel?
      (loop [i 0]
        (if (>= i len)
          false
          (let [c (int (.charAt line i))]
            (if (and (>= c INLINE_SENTINEL_LO) (<= c INLINE_SENTINEL_HI)) true (recur (inc i))))))]

     (.setForegroundColor g base-fg)
     (.setBackgroundColor g base-bg)
     (cond (zero? len) nil
           (not has-sentinel?)
           (blit-line! g (int x) (int y) line base-fg base-bg (.getActiveModifiers g))
           :else (blit-styled-line! g
                                    (int x)
                                    (int y)
                                    line
                                    base-fg
                                    base-bg
                                    code-fg
                                    code-bg
                                    err-fg
                                    (.getActiveModifiers g))))))

;;; ── Flex layout (pure string functions, column-aware) ─────────────────────

(defn pad-right
  "Pad string to `w` terminal columns, right-filling with spaces.
   Truncates (column-aware) if too wide. Backed by the lanterna fork's
   `TerminalTextUtils/padRight` (>= 3.1.5-vis.25)."
  ^String [s w]
  (TerminalTextUtils/padRight (str (or s "")) (int w)))

(defn pad-left
  "Pad string to `w` terminal columns, left-filling with spaces.
   Truncates (column-aware) if too wide. Backed by the lanterna fork's
   `TerminalTextUtils/padLeft` (>= 3.1.5-vis.25)."
  ^String [s w]
  (TerminalTextUtils/padLeft (str (or s "")) (int w)))

(defn center-text
  "Center string within `w` terminal columns, padding both sides.
   Truncates (column-aware) if too wide. Backed by the lanterna fork's
   `TerminalTextUtils/center` (>= 3.1.5-vis.25)."
  ^String [s w]
  (TerminalTextUtils/center (str (or s "")) (int w)))

(defn space-between
  "Distribute items across `w` terminal columns with equal gaps.
   First item flush-left, last item flush-right, rest evenly spaced.
   Like CSS justify-content: space-between. Backed by the lanterna fork's
   `TerminalTextUtils/spaceBetween` (>= 3.1.5-vis.25)."
  ^String [items w]
  (TerminalTextUtils/spaceBetween (mapv str items) (int w)))

(defn space-around
  "Distribute items across `w` terminal columns with equal space
   around each item. Like CSS justify-content: space-around. Backed by the
   lanterna fork's `TerminalTextUtils/spaceAround` (>= 3.1.5-vis.25)."
  ^String [items w]
  (TerminalTextUtils/spaceAround (mapv str items) (int w)))

(defn v-center-offset
  "Compute vertical offset to center `content-h` rows within `container-h` rows.
   Backed by the lanterna fork's `TerminalTextUtils/verticalCenterOffset`
   (>= 3.1.5-vis.25)."
  ^long [content-h container-h]
  (TerminalTextUtils/verticalCenterOffset (int content-h) (int container-h)))

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
   (let
     [tabs
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
       (let
         [gap
          (if (>= width (+ n (* gap (dec n)))) gap 0)

          gap-total
          (* gap (dec n))

          content-w
          (max 0 (- width gap-total))

          base
          (quot content-w n)

          extra
          (rem content-w n)]

         (loop
           [idx
            0

            x
            left

            out
            []]

           (if (= idx n)
             out
             (let
               [w
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
    (doseq
      [{:keys [left width active? text]} layout
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
