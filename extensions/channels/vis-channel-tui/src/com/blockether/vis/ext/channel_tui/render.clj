(ns com.blockether.vis.ext.channel-tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.highlight :as hl]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.terminal-image :as timg]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.provider-error :as perr])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize Symbols]
           [com.googlecode.lanterna.graphics TextGraphics]
           [java.util LinkedHashMap]))
;;; ── Render caches ──────────────────────────────────────────────────────────
;;
;; The TUI's main render path is hot: every visible frame used to
;; re-run full Markdown + iteration-trace formatting for every
;; assistant bubble in scrollback, then re-wrap every message text
;; multiple times. With a 50-iteration trace and a few finalized
;; bubbles in history that pegged the input thread.
;;
;; Messages are immutable once added to app-db (`update :messages
;; conj` only ever appends, the loading placeholder is replaced as a
;; whole map by `:message-received`), so identity-hash-code on the
;; `:answer` / `:traces` / `:text` slot is a valid stand-in for
;; content.
;;
;; Eviction is proper LRU via `LinkedHashMap` in access-order mode:
;; each `get` promotes its entry to the most-recently-used end, and
;; `removeEldestEntry` returns true once the map exceeds `fmt-cache-cap`,
;; so `put` automatically drops the single least-recently-used entry.
;; That keeps the working set hot (the bubbles you're currently
;; scrolling through) while the cache stays bounded - no "oh you got
;; one entry over the cap, here's a 5x latency spike on every visible
;; bubble while we re-format from scratch" cliff like the old
;; clear-the-whole-thing strategy.
(def ^:private ^:const fmt-cache-cap 4096)
(defn- make-fmt-cache
  ^LinkedHashMap []
  (proxy [LinkedHashMap] [64 0.75 true] ;; true = access-order (LRU)
    (removeEldestEntry [_eldest] (> (.size ^LinkedHashMap this) fmt-cache-cap))))
(defonce ^:private ^LinkedHashMap fmt-cache (make-fmt-cache))
(defn- cached*
  [k compute-fn]
  ;; LinkedHashMap is not thread-safe - the render thread used to be
  ;; the only caller, but `virtual/pre-warm!` now hits these caches
  ;; from a worker thread to keep first-scroll smooth. Two callers
  ;; means the lock matters, AND the lock granularity matters: if
  ;; we held it across `compute-fn` (which can run for ~500 ms on a
  ;; big trace bubble's `format-answer-with-thinking*`), the render
  ;; thread would stall on every cache miss the worker hit. So we
  ;; do double-checked locking: lock only for the dictionary
  ;; touches (.get / .put), never for the compute.
  ;;
  ;; If two threads race on the same missing key, both compute, but
  ;; the second one's `(or (.get ...) ...)` discards its result and
  ;; reuses the first one's value, so the cache map stays
  ;; deterministic. Wasted CPU on the duplicate compute is bounded
  ;; by the cache cap and rare in practice (the worker walks
  ;; messages in order; by the time the render thread asks for the
  ;; same key it's almost always already a hit).
  (let [hit (locking fmt-cache (.get fmt-cache k))]
    (if (some? hit)
      hit
      (let [v (compute-fn)]
        (locking fmt-cache (or (.get fmt-cache k) (do (.put fmt-cache k v) v)))))))
(defn invalidate-cache!
  "Drop every cached projection. Call on settings changes that the
   key tuple doesn't capture, or from tests."
  []
  (locking fmt-cache (.clear fmt-cache)))
(defn cache-size
  "Current number of entries (handy for tests/diagnostics)."
  ^long []
  (locking fmt-cache (.size fmt-cache)))
(defn repeat-str
  "Allocate a String of `n` copies of `ch`. Drop-in replacement for
   `(apply str (repeat n ch))` - bypasses lazy-seq + per-char
   StringBuilder appends, so the box-border + padding code paths stop
   showing up on the flame graph."
  ^String [ch ^long n]
  (if (<= n 0) "" (String. (char-array n ch))))
;;; ── Text wrapping ───────────────────────────────────────────────────────────
(defn- structural-line-marker?
  "True for invisible first-column markers consumed by `draw-chat-bubble!`.
   These markers select a row background/style. When wrapping a long marked
   row, every continuation must keep the same marker; otherwise only the first
   visual row paints the code/result/thinking background and the rest
   falls through as plain assistant text."
  [^Character ch]
  (boolean (or (#{\u200B \u200C \u200D \uFEFF} ch)
               (<= (int \u2060) (int ch) (int \u206F))
               (<= (int \uE001) (int ch) (int \uE02D)))))
(defn- split-structural-line-marker
  [^String line]
  (when (and (string? line) (pos? (count line)))
    (let [ch (.charAt line 0)]
      (when (structural-line-marker? ch) [(subs line 0 1) (subs line 1)]))))
(def ^:private inline-style-order [:bold :italic :strike :code :link])
(def ^:private inline-style->sentinels
  {:bold [p/INLINE_BOLD_ON p/INLINE_BOLD_OFF]
   :italic [p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF]
   :strike [p/INLINE_STRIKE_ON p/INLINE_STRIKE_OFF]
   :code [p/INLINE_CODE_ON p/INLINE_CODE_OFF]
   :link [p/INLINE_LINK_ON p/INLINE_LINK_OFF]})
(def ^:private inline-sentinel->transition
  {p/INLINE_BOLD_ON [:on :bold]
   p/INLINE_BOLD_OFF [:off :bold]
   p/INLINE_ITALIC_ON [:on :italic]
   p/INLINE_ITALIC_OFF [:off :italic]
   p/INLINE_STRIKE_ON [:on :strike]
   p/INLINE_STRIKE_OFF [:off :strike]
   p/INLINE_CODE_ON [:on :code]
   p/INLINE_CODE_OFF [:off :code]
   p/INLINE_LINK_ON [:on :link]
   p/INLINE_LINK_OFF [:off :link]})
(defn- inline-state-after
  [active ^String line]
  (loop [idx
         0

         active
         (vec active)]

    (if (>= idx (.length line))
      active
      (let [token (subs line idx (inc idx))]
        (if-let [[op style] (inline-sentinel->transition token)]
          (recur (inc idx)
                 (case op
                   :on
                   (if (some #{style} active) active (conj active style))

                   :off
                   (vec (remove #{style} active))))
          (recur (inc idx) active))))))
(defn- active-inline-prefix
  [active]
  (let [active-set (set active)]
    (apply str
      (keep (fn [style]
              (when (active-set style) (first (inline-style->sentinels style))))
            inline-style-order))))
(defn- active-inline-suffix
  [active]
  (let [active-set (set active)]
    (apply str
      (keep (fn [style]
              (when (active-set style) (second (inline-style->sentinels style))))
            (reverse inline-style-order)))))
(defn- rebalance-inline-sentinel-wraps
  "When a markdown inline span crosses a visual wrap boundary, repeat its
   zero-width sentinels on each physical row. Otherwise a long inline code span
   starts with code styling on row 1, then every continuation paints as plain
   prose until the final row's CODE_OFF. Added sentinels are zero-width, so
   wrapping width is unchanged."
  [lines]
  (loop [remaining
         (seq lines)

         active
         []

         out
         []]

    (if-let [line (first remaining)]
      (let [active' (inline-state-after active line)
            line' (str (active-inline-prefix active) line (active-inline-suffix active'))]

        (recur (next remaining) active' (conj out line')))
      out)))
(defn- wrap-unmarked-line-chunks
  [^String line ^long max-width]
  (if (<= (p/display-width line) max-width)
    [line]
    (loop [remaining
           line

           acc
           []]

      (if (<= (p/display-width remaining) max-width)
        (conj acc remaining)
        ;; `cut` is the CHAR index that bounds the longest prefix fitting
        ;; in `max-width` COLUMNS without splitting a grapheme. Char-index,
        ;; not column, because we still need to slice `remaining`.
        (let [cut (p/col-prefix-end remaining max-width)]
          (if (zero? cut)
            ;; A single glyph wider than `max-width` (emoji in a 1-column
            ;; bubble): force one full code point onto its own line so the
            ;; loop always makes progress. The old code re-queued the SAME
            ;; string here (`(subs remaining 0)` = `remaining`) and spun
            ;; the render thread forever.
            (let [end (Character/charCount (.codePointAt ^String remaining 0))]
              (recur (subs remaining end) (conj acc (subs remaining 0 end))))
            (let [chunk (subs remaining 0 cut)
                  ;; The cut landing exactly ON a word boundary means the
                  ;; whole chunk is already a clean line — do NOT retreat to
                  ;; the previous space inside it. Matches the lanterna
                  ;; fork's `TerminalTextUtils/wordWrap` packing ("launch
                  ;; pad rocket" @10 → ["launch pad" "rocket"], not
                  ;; ["launch" "pad rocket"]), keeping bubble wrap in step
                  ;; with every lanterna-wrapped surface.
                  boundary? (and (< cut (.length ^String remaining))
                                 (= \space (.charAt ^String remaining cut)))
                  last-sp (when-not boundary? (str/last-index-of chunk " "))]

              (cond boundary? (recur (subs remaining (inc cut)) (conj acc chunk))
                    (and last-sp (pos? (long last-sp)))
                    ;; Break at word boundary
                    (recur (subs remaining (inc (long last-sp)))
                           (conj acc (subs remaining 0 (long last-sp))))
                    :else
                    ;; No space found - hard break at column boundary
                    (recur (subs remaining cut) (conj acc chunk))))))))))
(defn- wrap-unmarked-line
  [^String line ^long max-width]
  (let [lines (wrap-unmarked-line-chunks line max-width)]
    (if (> (count lines) 1) (rebalance-inline-sentinel-wraps lines) lines)))
(defn- wrap-line-preserving-marker
  [line max-width]
  (if-let [[marker body] (split-structural-line-marker line)]
    (if (<= (p/display-width line) (long max-width))
      [line]
      (mapv #(str marker %) (wrap-unmarked-line body max-width)))
    (wrap-unmarked-line line max-width)))
(defn- sgr-escape-end
  ^long [^String s ^long esc-idx]
  (let [n (.length s)]
    (if-not (and (< (inc esc-idx) n) (= \[ (.charAt s (inc esc-idx))))
      -1
      (let [m-idx (str/index-of s "m" (+ esc-idx 2))]
        (if (and m-idx
                 (every? #(or (Character/isDigit ^char %) (= \; %)) (subs s (+ esc-idx 2) m-idx)))
          (long m-idx)
          -1)))))
(defn- truncate-ansi-cols
  "Like `p/truncate-cols`, but preserves ANSI SGR escapes as zero-width.
   Zprint emits ANSI-colored Clojure code; feeding those rows directly to
   Lanterna's grapheme splitter throws on ESC (0x1b), which can blank the
   whole TUI before first paint."
  ^String [s ^long max-cols]
  (let [s
        (str s)

        max-cols
        (max 0 max-cols)]

    (cond (zero? max-cols) ""
          (not (str/includes? s "\u001b")) (p/truncate-cols s max-cols)
          :else (let [n
                      (.length s)

                      sb
                      (StringBuilder.)]

                  (loop [i
                         0

                         used
                         0]

                    (cond (>= (long i) (long n)) (.toString sb)
                          (>= used max-cols) (.toString sb)
                          :else (let [esc-idx (or (str/index-of s "\u001b" i) n)]
                                  (if (< (long i) (long esc-idx))
                                    (let [chunk (subs s i esc-idx)
                                          w (p/display-width chunk)]

                                      (if (<= (+ used w) max-cols)
                                        (do (.append sb chunk) (recur esc-idx (+ used w)))
                                        (do (.append sb (p/truncate-cols chunk (- max-cols used)))
                                            (.toString sb))))
                                    (let [m-idx (sgr-escape-end s esc-idx)]
                                      (if (neg? m-idx)
                                        ;; Unknown control escape: never let it reach Lanterna.
                                        ;; Render it visibly as a middle dot and continue.
                                        (let [w 1]
                                          (if (<= (+ used w) max-cols)
                                            (do (.append sb /)
                                                (recur (inc (long esc-idx))
                                                       (+ (long used) (long w))))
                                            (.toString sb)))
                                        (do (.append sb s (int esc-idx) (int (inc m-idx)))
                                            (recur (inc m-idx) used))))))))))))
(defn- clip-line-preserving-marker
  "Clip a formatted chat-bubble row to `max-width` display columns.

   Wrapping handles normal text, but formatter-produced/prewrapped rows can
   still be too wide. Clip at the bubble boundary before any raw terminal
   paint call sees the row. Structural markers are zero-width style sentinels;
   preserve them and clip only the visible body."
  [line max-width]
  (let [max-width (max 0 (long max-width))]
    (if-let [[marker body] (split-structural-line-marker line)]
      (str marker (truncate-ansi-cols body max-width))
      (truncate-ansi-cols (str line) max-width))))
(defn- clip-lines-preserving-markers
  [lines max-width]
  (mapv #(clip-line-preserving-marker % max-width) lines))
(defn- full-band-marker?
  [line]
  (let [marker (when (and (string? line) (pos? (count line))) (subs line 0 1))]
    (contains? #{p/MARKER_CODE_PAD p/MARKER_CODE_OK_PAD p/MARKER_CODE_ERR_PAD} marker)))
(defn- clipped-lines
  "Memoized clipping for prewrapped painter lines. Large trace bubbles can
   carry hundreds of rows plus occasional huge raw tool-output rows; scrolling
   must not re-clip every off-screen row on each frame."
  [raw-lines content-w full-w]
  (let [content-w
        (long content-w)

        full-w
        (long full-w)]

    (cached*
      [::clipped-lines (System/identityHashCode raw-lines) content-w full-w]
      #(if (some full-band-marker? raw-lines)
         (mapv (fn [line]
                 (clip-line-preserving-marker line (if (full-band-marker? line) full-w content-w)))
               raw-lines)
         (clip-lines-preserving-markers raw-lines content-w)))))
(defn wrap-text*
  "Uncached implementation. Prefer `wrap-text` everywhere except inside
   `wrap-text` itself."
  [text max-width]
  (if (or (str/blank? text) (<= (long max-width) 0))
    [""]
    (let [input-lines (str/split-lines text)]
      (into [] (mapcat #(wrap-line-preserving-marker % max-width)) input-lines))))
(defn wrap-text
  "Memoized `wrap-text*`. Keyed by source-string identity so finalized
   message texts hit the cache across frames (their string instance is
   stable on the immutable message map)."
  [text max-width]
  (cached* [::wrap (System/identityHashCode text) (long max-width)] #(wrap-text* text max-width)))
(defn wrap-messages
  "Wrap a vec of display lines to fit within max-width. Returns flat vec of wrapped lines."
  [messages max-width]
  (into [] (mapcat #(wrap-text % max-width)) messages))
(def ^:private ^:const phi 1.618)
(def ^:private dialog-chrome-w 4)
;; border(1) + pad(1) each side
(def ^:private dialog-chrome-h 6)
;; top-border + title-bar + top-sep + ... + bot-sep + hint + bottom-border
(defn golden-dialog-size
  "Compute golden-ratio dialog size [w h] sized to fit content.
   `content-w` = widest content line (chars). `content-h` = content line count.
   Only expands WIDTH when content is too narrow for golden ratio.
   Never inflates height beyond what content needs.
   Clamped to terminal bounds."
  [^long cols ^long rows ^long content-w ^long content-h]
  (let [;; Minimum box size to contain content + chrome
        min-w
        (+ content-w (long dialog-chrome-w))

        min-h
        (+ content-h (long dialog-chrome-h))

        ;; Only widen to approach φ - never heighten
        golden-w
        (long (* min-h phi))

        box-w
        (max min-w golden-w)

        box-h
        min-h

        ;; Floor - minimum size generous enough for hint bars + labels
        box-w
        (max box-w 40)

        box-h
        (max box-h 7)

        ;; Clamp to terminal - hard constraint, always final so the
        ;; frame can never leave the screen even on a tiny terminal
        box-w
        (min box-w (- cols 4))

        box-h
        (min box-h (- rows 4))]

    [box-w box-h]))
;;; ── Box drawing ────────────────────────────────────────────────────────────
(defn- embed-in-bar
  "Centre `label` in `bar` (a string of horizontal box characters) and
   return the resulting string. `bar` length is unchanged. nil/blank
   labels return the bar untouched."
  [bar label]
  (let [w (count bar)]
    (if (and label (not (str/blank? label)) (<= (count label) w))
      (let [start (max 0 (quot (- w (count label)) 2))
            end (min w (+ start (count label)))]

        (str (subs bar 0 start) label (subs bar end)))
      bar)))
;; ----------------------------------------------------------------------------
;; Sideless box-border padding
;;
;; The input box uses a sideless variant of `draw-box-border!` (top +
;; bottom rules, no left/right rails, no corner glyphs). Both rules
;; sit inset from the screen edges by `INPUT_BORDER_HORIZONTAL_PAD`
;; columns on each side so the rule visually anchors to the message
;; column instead of running edge-to-edge.
;;
;; Tweak this single number to grow / shrink both rules symmetrically.
;; The previous bullet-point tuning history (4 -> 5 -> 4 -> 3 -> 2)
;; lived inline in `draw-box-border!`; lifting it to a const def keeps
;; the painter's body free of magic numbers and makes layout reviews
;; trivial - grep for `INPUT_BORDER_HORIZONTAL_PAD`.
(def ^:private INPUT_BORDER_HORIZONTAL_PAD
  "Cols of empty space on each end of the input-box top/bottom rules.
   Total rule width = `cols - 2 * INPUT_BORDER_HORIZONTAL_PAD`."
  2)
(defn- draw-box-border!
  "Draw a single-line box border. Optionally embeds a centered hint
   string on the top edge (typically the keybinding strip).

   Live status (model, ctx %, run-state) used to live on the bottom
   edge via a second hint argument; that path was deleted when the
   dedicated footer row took over (see
   `com.blockether.vis.ext.channel-tui.footer`). The bottom edge is now
   always a plain horizontal rule.

   `sides?` (default true) controls whether to paint the vertical
   `│` rails on the left and right edges of the inner rows. The
   input box passes false: it gets a top + bottom rule but no side
   rails, so the typing area sits flush against the message column."
  ([g box-top box-bottom cols top-hint] (draw-box-border! g box-top box-bottom cols top-hint true))
  ([^TextGraphics g box-top box-bottom cols top-hint sides?]
   (let [inner-w
         (- (long cols) 2)

         bar
         (repeat-str Symbols/SINGLE_LINE_HORIZONTAL inner-w)]

     (.setForegroundColor g t/border-fg)
     (.setBackgroundColor g t/terminal-bg)
     (if sides?
       (do
         ;; Top: ┌── top-hint ──┐
         (.setCharacter g 0 (int box-top) Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
         (.putString g 1 (int box-top) (embed-in-bar bar top-hint))
         (.setCharacter g
                        (int (dec (long cols)))
                        (int box-top)
                        Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)
         ;; Bottom: └──────┘ (plain rule - status moved to footer row).
         (.setCharacter g 0 (int box-bottom) Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
         (.putString g 1 (int box-bottom) bar)
         (.setCharacter g
                        (int (dec (long cols)))
                        (int box-bottom)
                        Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)
         ;; Sides: │ ... │
         (doseq [row (range (inc (long box-top)) box-bottom)]
           (.setForegroundColor g t/border-fg)
           (.setBackgroundColor g t/terminal-bg)
           (.setCharacter g 0 (int row) Symbols/SINGLE_LINE_VERTICAL)
           (.setCharacter g (int (dec (long cols))) (int row) Symbols/SINGLE_LINE_VERTICAL)))
       ;; Sideless variant: top + bottom rules with horizontal padding
       ;; on each end so the rule doesn't kiss the screen edges.
       ;; `pad` cols of empty space on each side; bar spans the inner
       ;; (cols - 2*pad) columns. No corners, no side rails. Top hint
       ;; embeds inside the padded bar.
       (let [pad
             INPUT_BORDER_HORIZONTAL_PAD

             rule-w
             (max 0 (- (long cols) (* 2 (long pad))))

             padded-bar
             (repeat-str Symbols/SINGLE_LINE_HORIZONTAL rule-w)]

         (.putString g (int pad) (int box-top) (embed-in-bar padded-bar top-hint))
         (.putString g (int pad) (int box-bottom) padded-bar))))))
(defn- fill-box-interior!
  "Fill the interior of a box with the standard box background."
  [^TextGraphics g box-top box-bottom cols]
  (let [inner-w
        (- (long cols) 2)

        text-top
        (inc (long box-top))

        rows
        (- (long box-bottom) (long box-top) 1)]

    (.setForegroundColor g t/box-fg)
    (.setBackgroundColor g t/box-bg)
    (.fillRectangle g (TerminalPosition. 1 text-top) (TerminalSize. inner-w rows) \space)))
;;; ── Messages box ───────────────────────────────────────────────────────────
(defn draw-messages-box!
  "Draw bordered message area with top-anchored scrollable messages."
  [^TextGraphics g messages box-top box-bottom cols scroll]
  (let [inner-rows
        (- (long box-bottom) (long box-top) 1)

        text-top
        (inc (long box-top))

        text-w
        (- (long cols) 4)

        total
        (count messages)

        offset
        (min (long scroll) (max 0 (- (long total) (long inner-rows))))

        visible
        (subvec messages offset (min (long total) (+ (long offset) (long inner-rows))))]

    (draw-box-border! g box-top box-bottom cols "")
    (fill-box-interior! g box-top box-bottom cols)
    (doseq [[i message] (map-indexed vector visible)]
      (.setForegroundColor g t/box-fg)
      (.setBackgroundColor g t/box-bg)
      ;; truncate-cols handles "shorter than width" (returns input verbatim)
      ;; and column-aware truncation in one call. No min-clamp needed.
      (p/put-str! g
                  (inc (long t/pad-x))
                  (+ (long text-top) (long i))
                  (p/truncate-cols message text-w)))))
;;; ── Input box ──────────────────────────────────────────────────────────────
(def input-pad-y
  "Internal vertical padding (rows above/below text) inside the input
   box. Set to 0: the top + bottom rule lines already provide all the
   visual breathing room the editor needs; an extra padded row on each
   side just wasted vertical space and pushed the cursor down a line.
   The previous value (1) made an empty editor render four rows tall
   (rule + pad + line + pad + rule) when three is the right minimum."
  0)
(def ^:private input-pad-x "Horizontal padding (cols left/right of text inside the input box)." 2)
(defn input-text-w
  "Visible text width (in columns) inside the input box for a given
   terminal `cols`. Single source of truth so screen.clj can compute
   wrapped row counts and `draw-input-box!` can render with the same
   wrap point.

   The input box is SIDELESS (top/bottom rules only, no `│` rails)
   and `input-pad-x` is now 0, so the typing zone spans the full
   terminal width - `text-w` = `cols` (clamped to >=1)."
  ^long [^long cols]
  (max 1 (- cols (* 2 (long input-pad-x)))))
(defn- wrap-input-line
  "Soft-wrap one logical input line into visual segments at `text-w`
   columns. Always returns a non-empty vec (empty input -> [\"\"])."
  [^String line ^long text-w]
  (let [text-w
        (max 1 text-w)

        n
        (count line)]

    (if (<= n text-w)
      [line]
      (loop [start
             0

             acc
             (transient [])]

        (if (>= start n)
          (persistent! acc)
          (let [end (min n (+ start text-w))]
            (recur end (conj! acc (subs line start end)))))))))
(defn visual-rows-for-line
  "How many visual rows logical line `line` occupies when soft-wrapped
   at `text-w` cols. Empty line still counts as 1 (a typable row)."
  ^long [^String line ^long text-w]
  (let [text-w
        (max 1 text-w)

        n
        (count line)]

    (cond (zero? n) 1
          :else (cond-> (quot n text-w)
                  (pos? (long (mod (long n) (long text-w))))
                  inc))))
(defn input-visual-row-count
  "Total visual rows occupied by every logical line in the input
   editor when soft-wrapped at `text-w` cols."
  ^long [lines ^long text-w]
  (reduce (fn [^long acc line]
            (+ acc (visual-rows-for-line line text-w)))
          0
          lines))
(defn- soft-wrap-input
  "Translate the logical editor state `{:lines :crow :ccol}` into a
   visual layout suitable for rendering. Returns:
     {:visual-lines vec<str>  every visual row, top-to-bottom
      :cursor-vrow  int       visual row of the cursor
      :cursor-vcol  int       column within that visual row}"
  [{:keys [lines crow ccol]} text-w]
  (let [text-w
        (max 1 (long text-w))

        wrapped
        (mapv #(wrap-input-line % text-w) lines)

        offsets
        (vec (reductions + 0 (map count wrapped)))

        line-len
        (count (nth lines crow))

        seg-count
        (count (nth wrapped crow))

        ;; When the cursor sits at end-of-line AND the line length is
        ;; an exact multiple of text-w, `quot ccol text-w` would land
        ;; on a non-existent next visual row. Pin it to the end of
        ;; the last segment instead so the cursor stays painted.
        seg-idx
        (if (and (= ccol line-len)
                 (pos? line-len)
                 (zero? (long (mod (long line-len) (long text-w)))))
          (dec seg-count)
          (quot (long ccol) (long text-w)))

        seg-off
        (- (long ccol) (* (long seg-idx) (long text-w)))]

    {:visual-lines (into [] cat wrapped)
     :cursor-vrow (+ (long (nth offsets crow)) (long seg-idx))
     :cursor-vcol seg-off}))
(defn- input-more-hint
  "Left-edge label for the input top border when the editor has more
   visual rows than the visible input body can show. The count is the
   number of hidden visual rows, rendered as a compact `N more` cue."
  [total-visual-rows text-rows]
  (let [hidden (max 0 (- (long total-visual-rows) (long text-rows)))]
    (when (pos? hidden) (str " " hidden " more "))))
(defn- bang-prefix
  "The `!`/`!&` shell-sugar marker at the head of `line`. Returns \"!&\", \"!\",
   or nil, so the input tints the marker in the shell accent the INSTANT the
   prefix is typed — a bare `!`/`!&` already signals shell intent, we don't wait
   for a command to follow. Nested cond so a bare `!&` never falls through to
   the `!` branch."
  [line]
  (when (string? line)
    (let [t (str/triml line)]
      (cond (str/starts-with? t "!&") "!&"
            (str/starts-with? t "!") "!"))))

(defn draw-input-box!
  "Draw bordered input area with internal padding. Returns
   [cursor-col cursor-row] in screen coords.

   Long logical lines are SOFT-WRAPPED at the box width: typing past
   the right edge flows onto the next visual row instead of scrolling
   the line horizontally. The logical model (`{:lines :crow :ccol}`)
   is unchanged - wrapping is a render-time projection only.

   `hint` optionally embeds a short label in the top border. Runtime
   keybinding helpers live in the echo area (`footer/draw-echo-area!`), not here,
   so input/editor paint stays isolated from footer chrome.

   No left/right side rails: the input area is framed by top and bottom
   rules only, so the typing zone sits flush with the message column on
   either side and the eye tracks the prompt directly without `│`-noise."
  [^TextGraphics g input box-top text-rows cols hint]
  (let [box-top
        (long box-top)

        text-rows
        (long text-rows)

        cols
        (long cols)

        input-pad-y
        (long input-pad-y)

        input-pad-x
        (long input-pad-x)

        box-bottom
        (+ box-top (* 2 input-pad-y) text-rows 1)

        text-top
        (+ (inc box-top) input-pad-y)

        text-w
        (long (input-text-w cols))

        {:keys [visual-lines cursor-vrow cursor-vcol]}
        (soft-wrap-input input text-w)

        v-scroll
        (max 0 (- (long cursor-vrow) (dec text-rows)))

        more-hint
        (input-more-hint (count visual-lines) text-rows)

        bang-pfx
        (bang-prefix (first visual-lines))]

    (draw-box-border! g box-top box-bottom cols hint false)
    ;; Shell-sugar affordance: a `!`/`!&` turn that WILL run a shell command
    ;; dresses the whole prompt in the shell accent so it reads as "runs in
    ;; your shell, not the model" - MIRRORS the web composer (frame tint + a
    ;; `shell`/`shell &` pill). Re-tint the top + bottom rules and embed a
    ;; centered pill in the top rule (`more-hint` repaints over it just below).
    ;; Pure overpaint of already-drawn chrome - no extra rows, no layout shift.
    (when bang-pfx
      (let [pad
            (long INPUT_BORDER_HORIZONTAL_PAD)

            rule-w
            (max 0 (- cols (* 2 pad)))

            bar
            (repeat-str Symbols/SINGLE_LINE_HORIZONTAL rule-w)

            pill
            (if (= bang-pfx "!&") " shell & " " shell ")]

        (.setForegroundColor g t/tool-color-shell)
        (.setBackgroundColor g t/terminal-bg)
        (.putString g (int pad) (int box-top) (embed-in-bar bar pill))
        (.putString g (int pad) (int box-bottom) bar)))
    (when more-hint
      (.setForegroundColor g t/border-fg)
      (.setBackgroundColor g t/terminal-bg)
      (.putString g
                  (+ (long INPUT_BORDER_HORIZONTAL_PAD) 2)
                  (int box-top)
                  (p/truncate-cols more-hint
                                   (max 0 (- cols (* 2 (long INPUT_BORDER_HORIZONTAL_PAD)) 4)))))
    (fill-box-interior! g box-top box-bottom cols)
    ;; Text
    (.setForegroundColor g t/box-fg)
    (.setBackgroundColor g t/box-bg)
    (dotimes [i text-rows]
      (let [vi (+ v-scroll i)]
        (when (< vi (count visual-lines))
          (let [line (nth visual-lines vi)]
            (when (pos? (count line))
              (.putString g input-pad-x (+ text-top i) (subs line 0 (min (count line) text-w))))))))
    ;; Marker: tint JUST the leading `!`/`!&` in the typed text (only when the
    ;; first row is actually on-screen, i.e. not scrolled off the top). Pairs
    ;; with the shell-accent frame drawn above.
    (when (and bang-pfx (zero? v-scroll))
      (let [line0
            (first visual-lines)

            lead
            (- (count line0) (count (str/triml line0)))]

        (.setForegroundColor g t/tool-color-shell)
        (.setBackgroundColor g t/box-bg)
        (.putString g (+ input-pad-x lead) text-top bang-pfx)))
    ;; Cursor position (visual coords)
    [(+ input-pad-x (long cursor-vcol)) (+ text-top (- (long cursor-vrow) v-scroll))]))
(def ^:private slash-desc-separator
  "Visual separator between the inline-code usage chip and the italic
   description. Mirrors the markdown convention `\\`/cmd\\` - description`
   so the row reads like one rendered list bullet."
  " - ")
(defn- draw-slash-suggestion-row!
  "Paint one suggestion row inside the inset span [`left`, `left+inner-w`).

   Two layouts share this row, keyed by `:file/mention?`:

   - SLASH commands read like the markdown line `\\`/cmd\\` - description`:
     an inline-code chip, a ` - ` separator, then a left-anchored italic
     description.
   - FILE `@` mentions read like a proper file picker: the path chip on the
     LEFT and the `size · age · status` meta RIGHT-ALIGNED to the row's right
     edge. Right-aligning the meta spans the full width instead of stranding
     it next to the chip with a wide empty gutter (the old symmetric padding).

   The first `p/SELECTION_WIDTH` cols are the selection gutter (`•` on the
   selected row, painted by the caller); ONE col on the right is kept clear for
   the overlay scrollbar thumb. Truncation drops the description first; the
   chip renders fully whenever at all possible."
  [^TextGraphics g row left inner-w suggestion]
  (let [row
        (long row)

        left
        (long left)

        inner-w
        (long inner-w)

        pad
        (long p/SELECTION_WIDTH)

        file?
        (:file/mention? suggestion)

        ;; One space between the selection marker gutter and the chip so
        ;; the candidate never abuts the `•` — reads as `•  chip`.
        x0
        (+ left pad 1)

        ;; Last paintable col of the inset body. Keep 1 col clear on the
        ;; right for the scrollbar thumb PLUS a 2-col right margin so the
        ;; description / meta never kisses the overlay's right edge.
        row-right
        (+ left (max 0 (- inner-w 3)))

        avail
        (max 0 (- row-right x0))

        usage-raw
        (or (:slash/usage suggestion) "")

        usage
        (if file? (p/truncate-middle usage-raw avail) (p/truncate-cols usage-raw avail))

        usage-w
        (long (p/display-width usage))

        ;; Inline code chip = usage padded by 1 space on each side.
        chip-w
        (if (pos? usage-w) (+ usage-w 2) 0)

        chip-end
        (+ x0 chip-w)

        desc-raw
        (or (:label suggestion) "")

        row-fg
        (.getForegroundColor g)

        row-bg
        (.getBackgroundColor g)]

    ;; Inline code chip for the usage/path — markdown `\\`…\\`` look.
    (when (pos? chip-w)
      (p/set-colors! g t/code-block-fg t/code-block-bg)
      (p/fill-rect! g x0 row chip-w 1)
      ;; Paint the candidate PLAIN on the code-chip background — the colored
      ;; chip IS the markdown inline-code look, so no literal backticks. The
      ;; 1-col pad each side (chip-w = usage-w + 2) reads as a rendered chip.
      (p/put-str! g (inc x0) row usage)
      (p/set-colors! g row-fg row-bg))
    (if file?
      ;; FILE row: size · age · status meta, RIGHT-ALIGNED to the row edge.
      (let [desc-avail
            (max 0 (- row-right chip-end 2))

            desc
            (when (pos? desc-avail) (p/truncate-cols desc-raw desc-avail))

            desc-w
            (long (if desc (p/display-width desc) 0))]

        (when (and desc (pos? desc-w))
          (let [desc-x (max (+ chip-end 2) (- row-right desc-w))]
            (p/set-fg! g t/dialog-hint)
            (p/styled g [p/ITALIC] (p/put-str! g desc-x row desc))
            (p/set-fg! g row-fg))))
      ;; SLASH row: ` - ` separator + left-anchored italic description.
      (let [sep
            slash-desc-separator

            sep-w
            (if (and (pos? chip-w) (< (+ chip-w (count sep)) avail)) (count sep) 0)

            desc-w
            (max 0 (- (long avail) (long chip-w) (long sep-w)))

            desc
            (when (pos? (long desc-w)) (p/truncate-cols desc-raw desc-w))]

        (when (pos? (long sep-w)) (p/put-str! g (long chip-end) row sep))
        (when (and desc (pos? (p/display-width desc)))
          (p/set-fg! g t/dialog-hint)
          (p/styled g [p/ITALIC] (p/put-str! g (+ (long chip-end) (long sep-w)) row desc))
          (p/set-fg! g row-fg))))))
(def ^:private slash-title-label "Slash commands")
(def ^:private slash-title-hints
  ;; Flex items rendered space-between in the title bar. Keys are
  ;; rendered BOLD to match the dialog `draw-hint-bar!` idiom; the
  ;; whole row sits on the `dialog-title-bg` accent stripe to match
  ;; the `draw-dialog-chrome!` title bar style.
  ;;
  ;; Enter and Tab both complete the selected suggestion into
  ;; the input so the user can edit args before running. Keep this
  ;; in sync with screen.clj slash-suggestion key handling.
  [["↑↓/wheel" "select"] ["Enter/Tab" "complete"]])
(def ^:private file-title-label "Files")
(def ^:private file-title-hints
  ;; Same flex layout as the slash title, but the completion verb is
  ;; "attach" — the `@` picker inserts a file mention, it doesn't run a command.
  [["↑↓/wheel" "select"] ["Enter/Tab" "attach"]])
(defn- draw-slash-title-bar!
  "Render the suggestion overlay title row — for slash commands OR the inline
   `@` file picker (same overlay, different `title-label`/`title-hints`).

   Layout: an accent stripe (`dialog-title-bg`) spanning
   [`left`, `left + inner-w`) carrying a BOLD label on the left and
   `[key action]` hint pairs distributed space-between across the rest
   of the row, mirroring the `draw-hint-bar!` flex idiom but on the
   title-bar accent so the header reads as something IMPORTANT, not as
   another hint line.

   `left` and `inner-w` come from the same horizontal-padding rule the
   input box uses (`INPUT_BORDER_HORIZONTAL_PAD`), so the overlay
   visually anchors to the same column span as the typing zone."
  ([^TextGraphics g title-row left inner-w]
   (draw-slash-title-bar! g title-row left inner-w slash-title-label slash-title-hints))
  ([^TextGraphics g title-row left inner-w title-label title-hints]
   ;; Accent stripe over the inner span only — the margin columns on
   ;; each side stay terminal-bg to read as breathing room.
   (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
   (p/fill-rect! g left title-row inner-w 1)
   (let [title-row
         (long title-row)

         left
         (long left)

         inner-w
         (long inner-w)

         ;; Inner content sits one col inside the accent stripe so the
         ;; BOLD label doesn't kiss the stripe edge.
         content-pad
         1

         text-w
         (max 0 (- inner-w (* 2 content-pad)))

         text-x0
         (+ left content-pad)

         text-x1
         (+ text-x0 text-w)

         ;; Items: label first, then each [key action] pair joined as
         ;; one display token so space-between distributes them evenly.
         labels
         (into [title-label]
               (mapv (fn [[k a]]
                       (str k " " a))
                     title-hints))

         n
         (count labels)

         sizes
         (mapv p/display-width labels)

         total
         (reduce + sizes)

         slack
         (max 0 (- (long text-w) (long total)))

         gaps
         (max 1 (dec n))

         base
         (max 1 (quot (long slack) (long gaps)))

         extra
         (max 0 (- (long slack) (* (long base) (long gaps))))]

     (loop [i
            0

            col
            text-x0]

       (when (and (< (long i) (long n)) (< (long col) (long text-x1)))
         (let [size
               (long (nth sizes i))

               gap
               (if (< (long i) (dec (long n)))
                 (+ (long base) (if (< (long i) (long extra)) 1 0))
                 0)]

           (if (zero? i)
             ;; Bold left-anchored label.
             (p/styled g
                       [p/BOLD]
                       (p/put-str! g
                                   col
                                   title-row
                                   (p/truncate-cols title-label
                                                    (max 0 (- (long text-x1) (long col))))))
             ;; [key action] pair: BOLD key, plain action.
             (let [[k a]
                   (nth title-hints (dec i))

                   k-w
                   (long (p/display-width k))]

               (p/styled g
                         [p/BOLD]
                         (p/put-str! g
                                     col
                                     title-row
                                     (p/truncate-cols k (max 0 (- (long text-x1) (long col))))))
               (p/put-str! g
                           (+ (long col) (long k-w))
                           title-row
                           (p/truncate-cols (str " " a)
                                            (max 0 (- (long text-x1) (+ (long col) (long k-w))))))))
           (recur (inc (long i)) (+ (long col) (long size) (long gap)))))))))
(defn draw-slash-command-suggestions!
  "Overlay fuzzy slash-command suggestions immediately above the input box.

   Visual stack from top to bottom:
     (top margin row — terminal-bg gap)
     [Slash commands  …  Enter/Tab complete]   ; title bar (dialog-title-bg accent)
     ─────────────────────────────────────────   ; border under title (dialog-border)
     suggestion rows…

   The accent title + under-rule signal `this is a menu`, matching the
   `draw-dialog-chrome!` chrome idiom but in overlay form (no side
   rails, since the input box below has no side rails either).

   The whole overlay is inset by `INPUT_BORDER_HORIZONTAL_PAD` cols on
   each side so the accent stripe and rule line up exactly with the
   input box's top/bottom rules below."
  ([g suggestions input-top cols] (draw-slash-command-suggestions! g suggestions input-top cols 0))
  ([g suggestions input-top cols selected-index]
   (when (seq suggestions)
     (let [pad
           INPUT_BORDER_HORIZONTAL_PAD

           left
           pad

           inner-w
           (max 1 (- (long cols) (* 2 (long pad))))

           ;; Layout above the input box (rows decrease as we go up):
           ;;   margin-row    -> terminal-bg gap (optional, drops first)
           ;;   title-row     -> accent stripe (non-negotiable)
           ;;   border-row    -> ─ rule under title (drops second)
           ;;   suggestion rows...
           ;;
           ;; Sizing priority: title + at least 1 suggestion > border >
           ;; top margin. Border and margin drop when input-top is tight
           ;; (small terminal, lots of suggestions); only the title row
           ;; is reserved up-front.
           max-list
           (max 0 (dec (long input-top)))

           ; reserve title only
           visible-cap
           (max 0 (min 6 (long max-list)))

           total
           (count suggestions)

           selected-pos
           (or (some (fn [[idx suggestion]]
                       (when (:slash/selected? suggestion) idx))
                     (map-indexed vector suggestions))
               selected-index
               0)

           sel
           (max 0 (min (dec total) (long selected-pos)))

           first-idx
           (if (pos? (long visible-cap))
             (min (max 0 (- (long total) (long visible-cap)))
                  (max 0 (- (long sel) (quot (long visible-cap) 2))))
             0)

           visible
           (->> suggestions
                (drop first-idx)
                (take visible-cap))

           n
           (count visible)

           have-border?
           (>= (long input-top) (+ (long n) 2))

           have-margin?
           (>= (long input-top) (+ (long n) 3))

           ;; Suggestions occupy rows: input-top - n .. input-top - 1.
           ;; Border (if present) sits one row above the first suggestion,
           ;; title sits one row above the border (or above the first
           ;; suggestion if border dropped).
           first-sug
           (- (long input-top) (long n))

           border-row
           (when have-border? (dec (long first-sug)))

           title-row
           (cond have-border? (dec (long border-row))
                 (pos? (long first-sug)) (dec (long first-sug))
                 :else 0)

           margin-row
           (when have-margin? (dec (long title-row)))]

       (when (pos? n)
         ;; Top margin — paint the gap row in terminal-bg so any chat
         ;; content peeking through gets cleared. This is the breathing
         ;; room above the title bar.
         (when (and margin-row (>= (long margin-row) 0))
           (p/set-colors! g t/text-fg t/terminal-bg)
           (p/fill-rect! g 0 margin-row cols 1))
         ;; Title bar (accent + flex hints) — the inline `@` file picker rides
         ;; the SAME overlay as slash commands, just relabelled.
         (if (:file/mention? (first suggestions))
           (draw-slash-title-bar! g title-row left inner-w file-title-label file-title-hints)
           (draw-slash-title-bar! g title-row left inner-w))
         ;; Border under the title — single horizontal rule that
         ;; visually delimits the title from the suggestion list, on
         ;; `terminal-bg` (outside the accent) using the same width as
         ;; the input box's top/bottom rules.
         (when (and border-row (>= (long border-row) 0))
           (p/set-colors! g t/dialog-border t/terminal-bg)
           ;; Clear margin columns to terminal-bg first so the rule
           ;; sits flush within the same column span as the title.
           (p/fill-rect! g 0 border-row cols 1)
           (p/put-str! g left border-row (p/horiz-line inner-w)))
         ;; Suggestion rows — inset by the same margin so the row body
         ;; lines up with the title accent and the input box rule.
         ;; Selection is signalled by a `•` cursor glyph in the FIRST
         ;; col of the inset row (matching the project-wide convention;
         ;; see `p/SELECTION_GLYPH`). The marker sits INSIDE the inset
         ;; body, not in the terminal-bg margin outside of it, so it
         ;; reads as part of the menu rather than floating loose.
         (doseq [[i suggestion] (map-indexed vector visible)]
           (let [row (+ (long first-sug) (long i))]
             ;; Clear the full row to terminal-bg so the margin gutters
             ;; on each side don't bleed leftover paint.
             (p/set-colors! g t/text-fg t/terminal-bg)
             (p/fill-rect! g 0 row cols 1)
             ;; Body row in the normal dialog palette (no inversion).
             (p/set-colors! g t/dialog-fg t/dialog-bg)
             (p/fill-rect! g left row inner-w 1)
             ;; Cursor glyph one col IN from the inset edge (a 1-col
             ;; left margin before the dot) in the dialog palette so it
             ;; visually belongs to the row. The bullet then abuts the
             ;; candidate chip directly — no wide empty gutter between.
             (p/set-colors! g t/dialog-hint-key t/dialog-bg)
             (p/draw-selection-marker! g (inc (long left)) row (:slash/selected? suggestion))
             ;; Inline-code chip + ` - ` + italic description.
             (p/set-colors! g t/dialog-fg t/dialog-bg)
             (draw-slash-suggestion-row! g row left inner-w suggestion)))
         ;; Right-side scrollbar when more matches exist than visible rows.
         ;; Bespoke thumb math is gone — routes through the shared
         ;; `scrollbar/draw!` so the slash overlay scrolls with the same
         ;; feel and the same 1-row thumb as every other modal. The
         ;; suggestion list is item-windowed: total = number of
         ;; matches, viewport = visible count, scroll = first visible.
         (when (and (pos? (long n)) (> (long inner-w) 2))
           (scrollbar/draw! g
                            {:col (+ (long left) (dec (long inner-w)))
                             :top first-sug
                             :track-h n
                             :total-h total
                             :inner-h n
                             :scroll first-idx
                             :thumb-fg t/dialog-title-bg})))))))
;;; ── Background fill ────────────────────────────────────────────────────────
(defn fill-background!
  "Fill entire screen with the terminal background color."
  [^TextGraphics g cols rows]
  (.setBackgroundColor g t/terminal-bg)
  (.setForegroundColor g t/text-fg)
  (.fillRectangle g (TerminalPosition. 0 0) (TerminalSize. cols rows) \space))
;;; ── Dialog ─────────────────────────────────────────────────────────────────
(defn draw-dialog!
  "Draw a centered confirmation dialog with text wrapping.
   `body` can be a string or vec of strings. Long lines are wrapped to fit.
   Optional `max-w` limits dialog width (defaults to 60% of screen)."
  ([g cols rows title body] (draw-dialog! g cols rows title body nil))
  ([^TextGraphics g cols rows title body max-w]
   (let [limit-w
         (or max-w (max 30 (int (* (long cols) 0.6))))

         content-w
         (- (long limit-w) 6)

         ;; padding inside dialog
         raw-lines
         (if (string? body) [body] body)

         text-lines
         (into [] (mapcat #(wrap-text % content-w)) raw-lines)

         max-line-w
         (apply max (map count (cons title text-lines)))

         box-w
         (min (long limit-w) (+ (long max-line-w) 6))

         box-h
         (+ (count text-lines) 4)

         box-left
         (quot (- (long cols) (long box-w)) 2)

         box-top
         (quot (- (long rows) (long box-h)) 2)

         box-right
         (+ (long box-left) (long box-w) -1)

         box-bottom
         (+ (long box-top) (long box-h) -1)

         inner-w
         (- (long box-w) 2)

         h-bar
         (repeat-str Symbols/SINGLE_LINE_HORIZONTAL inner-w)]

     ;; Shadow
     (.setBackgroundColor g t/dialog-shadow)
     (.fillRectangle g
                     (TerminalPosition. (+ (long box-left) 2) (inc (long box-top)))
                     (TerminalSize. box-w box-h)
                     \space)
     ;; Background
     (.setBackgroundColor g t/dialog-bg)
     (.fillRectangle g (TerminalPosition. box-left box-top) (TerminalSize. box-w box-h) \space)
     ;; Border
     (.setForegroundColor g t/dialog-border)
     (.setBackgroundColor g t/dialog-bg)
     (.setCharacter g (int box-left) (int box-top) Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
     (.setCharacter g (int box-right) (int box-top) Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)
     (.setCharacter g (int box-left) (int box-bottom) Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
     (.setCharacter g (int box-right) (int box-bottom) Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)
     (.putString g (inc (long box-left)) box-top h-bar)
     (.putString g (inc (long box-left)) box-bottom h-bar)
     (doseq [r (range (inc (long box-top)) box-bottom)]
       (.setCharacter g (int box-left) (int r) Symbols/SINGLE_LINE_VERTICAL)
       (.setCharacter g (int box-right) (int r) Symbols/SINGLE_LINE_VERTICAL))
     ;; Title
     (.setForegroundColor g t/dialog-title-bg)
     (let [title-x (+ (long box-left) (quot (- (long box-w) (count title)) 2))]
       (.putString g title-x (inc (long box-top)) title))
     ;; Body (wrapped)
     (.setForegroundColor g t/dialog-fg)
     (doseq [[i line] (map-indexed vector text-lines)]
       (.putString g (+ (long box-left) 3) (+ (long box-top) 3 (long i)) line)))))
;;; ── Chat bubble ────────────────────────────────────────────────────────────
;; Line markers live in primitives - aliases for local readability.
(def ^:private thinking-marker p/MARKER_THINKING)
(def ^:private code-marker p/MARKER_CODE)
(def ^:private result-marker p/MARKER_RESULT)
(def ^:private sep-marker p/MARKER_SEP)
(def ^:private code-ok-marker p/MARKER_CODE_OK)
(def ^:private code-err-marker p/MARKER_CODE_ERR)
(def ^:private err-result-marker p/MARKER_ERR_RESULT)
(def ^:private duration-marker p/MARKER_DURATION)
(def ^:private iteration-hdr-marker p/MARKER_ITERATION_HDR)
(def ^:private recap-marker p/MARKER_RECAP)
(def ^:private recap-kinds
  "Known recap badge tokens. Drives both the per-row meta tag and
   the painter's color picker. Keep the keys EXACTLY equal to the
   leading token emitted by `segment->recap-text` /
   `recap-row-entries` so the parser stays a simple
   `(first (str/split text #\"\\s+\"))` lookup.

   Declared near `recap-marker` because the paint dispatch (which
   reads it) lives in the same paint loop, hundreds of lines above
   the recap row builders."
  #{"TITLE" "TASK" "SPEC" "FACT" "RECAP" "CONSULT"})
(def ^:private answer-sep-marker p/MARKER_ANSWER_SEP)
(def ^:private code-pad-marker p/MARKER_CODE_PAD)
(def ^:private code-ok-pad-marker p/MARKER_CODE_OK_PAD)
(def ^:private code-err-pad-marker p/MARKER_CODE_ERR_PAD)
(def ^:private iteration-pad-marker p/MARKER_ITERATION_PAD)
(def ^:private answer-hdr-marker p/MARKER_ANSWER_HDR)
(def ^:private answer-txt-marker p/MARKER_ANSWER_TXT)
(def ^:private answer-pad-marker p/MARKER_ANSWER_PAD)
(def ^:private hint-marker p/MARKER_HINT)
(def ^:private queue-hdr-marker p/MARKER_QUEUE_HDR)
(def ^:private queue-item-marker p/MARKER_QUEUE_ITEM)
(def ^:private queue-border-marker p/MARKER_QUEUE_BORDER)
(def ^:private md-h1-marker p/MARKER_MD_H1)
(def ^:private md-h2-marker p/MARKER_MD_H2)
(def ^:private md-h3-marker p/MARKER_MD_H3)
(def ^:private md-bold-marker p/MARKER_MD_BOLD)
(def ^:private md-code-marker p/MARKER_MD_CODE)
(def ^:private md-bullet-marker p/MARKER_MD_BULLET)
(def ^:private md-table-head-marker p/MARKER_MD_TABLE_HEAD)
(def ^:private md-table-sep-marker p/MARKER_MD_TABLE_SEP)
(def ^:private md-table-row-marker p/MARKER_MD_TABLE_ROW)
(def ^:private md-quote-marker p/MARKER_MD_QUOTE)
(def ^:private md-hr-marker p/MARKER_MD_HR)
(def ^:private md-summary-marker p/MARKER_MD_SUMMARY)
(def ^:private tool-output-indent "Visible left margin for result rows under a tool call." "  ")
(def ^:private tool-output-indent-cols (p/display-width tool-output-indent))
(def ^:private code-block-h-pad
  "Left padding (cols) for ANSWER markdown fenced-code rows so the code
   text doesn't hug the colored band's left edge. The band itself fills
   the full message column; the right gap comes from text using the
   narrower content width while the fill uses the wider bubble width."
  2)
(def ^:private output-indentable-markers #{code-err-pad-marker})
(def ^:private th-md-h1-marker p/MARKER_TH_MD_H1)
(def ^:private th-md-h2-marker p/MARKER_TH_MD_H2)
(def ^:private th-md-h3-marker p/MARKER_TH_MD_H3)
(def ^:private th-md-bold-marker p/MARKER_TH_MD_BOLD)
(def ^:private th-md-code-marker p/MARKER_TH_MD_CODE)
(def ^:private th-md-bullet-marker p/MARKER_TH_MD_BULLET)
(def ^:private th-md-table-head-marker p/MARKER_TH_MD_TABLE_HEAD)
(def ^:private th-md-table-sep-marker p/MARKER_TH_MD_TABLE_SEP)
(def ^:private th-md-table-row-marker p/MARKER_TH_MD_TABLE_ROW)
(def ^:private th-md-quote-marker p/MARKER_TH_MD_QUOTE)
(def ^:private th-md-hr-marker p/MARKER_TH_MD_HR)
(def ^:private th-md-summary-marker p/MARKER_TH_MD_SUMMARY)
(def ^:private turn-stamp-pattern #"\bt\d+/i\d+/(?:b|f)\d+\b")
(def ^:private code-text-inset-markers
  #{code-marker code-ok-marker code-err-marker result-marker err-result-marker th-md-code-marker
    thinking-marker th-md-h1-marker th-md-h2-marker th-md-h3-marker th-md-bold-marker
    th-md-bullet-marker th-md-quote-marker th-md-hr-marker th-md-summary-marker
    th-md-table-head-marker th-md-table-sep-marker th-md-table-row-marker})
(defn- ansi-code->fg
  [code current-fg base-fg]
  (case code
    7
    t/warning-fg

    0
    base-fg

    30
    t/code-block-fg

    31
    t/code-syntax-string-fg

    32
    t/code-success-fg

    33
    t/warning-fg

    34
    t/code-syntax-number-fg

    35
    t/code-syntax-special-fg

    36
    t/code-syntax-keyword-fg

    37
    t/text-fg

    90
    t/code-syntax-comment-fg

    91
    t/code-error-fg

    92
    t/code-success-fg

    current-fg))
(defn- parse-ansi-codes
  [s]
  (into []
        (keep (fn [part]
                (try (Long/parseLong part) (catch NumberFormatException _ nil))))
        (str/split (or s "") #";")))
(defn- ansi-codes->fg
  [codes current-fg base-fg]
  ;; An empty SGR (`ESC[m`, canonically ESC[0m) is a full reset — git's own
  ;; `%C(reset)` emits exactly that, so treat no codes as "back to base".
  (if (empty? codes)
    base-fg
    (reduce (fn [fg code]
              (if (= 0 code) base-fg (ansi-code->fg code fg base-fg)))
            current-fg
            codes)))

(defn paint-ansi-line!
  "Paint a possibly ANSI-colored zprint line onto a Lanterna surface.
   ANSI foreground codes are translated to Lanterna foreground colors;
   `bg` is always controlled by Vis so success/running/error code
   zones keep their own background.

   Tool render-fns now return Markdown for result bodies. That Markdown
   may contain Vis inline-style sentinels (U+E110..U+E117) for spans.
   These sentinels are paint directives, not glyphs; consume them here
   before they hit Lanterna's raw putString path."
  ([^TextGraphics g x y ^String line base-fg bg]
   (paint-ansi-line! g x y line base-fg bg t/code-block-fg t/code-block-bg))
  ([^TextGraphics g x y ^String line base-fg bg code-fg code-bg]
   (letfn [(sentinel-chunk? [^String chunk]
             (boolean (some #(str/index-of chunk ^String %)
                            [p/INLINE_BOLD_ON p/INLINE_BOLD_OFF p/INLINE_ITALIC_ON
                             p/INLINE_ITALIC_OFF p/INLINE_STRIKE_ON p/INLINE_STRIKE_OFF
                             p/INLINE_CODE_ON p/INLINE_CODE_OFF])))
           (paint-chunk! [col ^String chunk fg]
             (p/set-colors! g fg bg)
             (if (sentinel-chunk? chunk)
               (p/paint-styled-line! g (+ (long x) (long col)) y chunk fg bg code-fg code-bg)
               (p/put-str! g (+ (long x) (long col)) y chunk)))]
     (loop [i
            0

            col
            0

            fg
            base-fg]

       (if (>= (long i) (.length line))
         g
         (let [esc-idx (str/index-of line "\u001b[" i)]
           (if (or (nil? esc-idx) (< (long i) (long esc-idx)))
             (let [end (or esc-idx (.length line))
                   chunk (subs line i end)]

               (paint-chunk! col chunk fg)
               (recur end (+ col (p/display-width chunk)) fg))
             (let [m-idx (str/index-of line "m" (+ (long esc-idx) 2))]
               (if (nil? m-idx)
                 (let [chunk (subs line esc-idx)]
                   (paint-chunk! col chunk fg)
                   g)
                 (let [codes (parse-ansi-codes (subs line (+ (long esc-idx) 2) m-idx))
                       fg* (ansi-codes->fg codes fg base-fg)]

                   (recur (inc (long m-idx)) col fg*)))))))))))
(defn- paint-turn-stamp!
  "Overdraw canonical tN/iN/bN stamps in muted italic. Used by footer
   rows and collapsed tool badge rows so scope stays visible but dim."
  [^TextGraphics g x y raw bg]
  (when-let [stamp (re-find turn-stamp-pattern (str raw))]
    (when-let [ci (str/index-of (str raw) stamp)]
      (let [col (p/display-width (subs (str raw) 0 ci))]
        (p/set-colors! g t/dialog-hint bg)
        (p/styled g [p/ITALIC] (p/put-str! g (+ (long x) (long col)) y stamp))))))
(defn- paint-code-pad-payload!
  "Paint optional code-pad payloads. Blank pad rows only fill bg;
   payload rows can also show turn/block stamps in muted italic."
  [^TextGraphics g x y raw base-fg bg]
  (when (seq raw) (paint-ansi-line! g x y raw base-fg bg) (paint-turn-stamp! g x y raw bg)))
(defn- warning-message? [text] (and (string? text) (str/starts-with? text "Warning:")))
(defn- paint-table-data-line!
  "Two-pass paint for a table header or body row.

   The line carries both box-drawing chrome (`│┌─┐│├┼┤│└┴┘` etc.)
   and cell text. Painting it as a single string would force one
   foreground for everything; the chrome ends up the same dark color
   as the text and the table reads as solid ink. We split the work:

     pass 1  paint the whole line in `border-fg` (muted) on `bg`
     pass 2  walk every region BETWEEN consecutive `│` chars and
             overdraw the cell text in `text-fg` (optionally bold)

   The result: chrome stays quiet, cell text reads sharp, headers
   show bold without dragging the column dividers along with them.

   `body-styles` (e.g. `[p/BOLD]` for header rows, `[p/ITALIC]` for
   thinking-mode body rows) applies to the text overdraw only - not
   to the chrome pass."
  [^TextGraphics g x y ^String line text-fg border-fg bg body-styles]
  (p/clear-styles! g)
  (p/set-colors! g border-fg bg)
  (p/put-str! g x y line)
  (p/clear-styles! g)
  (p/set-colors! g text-fg bg)
  ;; Walk by grapheme cluster (lanterna's BreakIterator-based
  ;; `TextCharacter/fromString`), tracking BOTH the char-index in
  ;; the source string AND the screen-column offset relative to x.
  ;;
  ;; The two diverge whenever a wide grapheme appears (📄 = 2 chars
  ;; / 2 cols, 🏿️ = 3 chars / 2 cols, 日 = 1 char / 2 cols, etc.).
  ;; The PRE-FIX version used (+ x i) for the screen-col of every
  ;; segment overdraw; that worked for ASCII but drifted by 1 col
  ;; per VS-16 / 2 cols per flag etc., shifting the body text past
  ;; the chrome `┃` separators and overwriting them with cell
  ;; content. THAT was the visible “🏿️ row eats its separators” bug.
  (let [cells
        (com.googlecode.lanterna.TextCharacter/fromString line)

        n
        (alength cells)

        ;; end-col would be symmetric with end-char but lanterna's
        ;; putString computes the actual paint advance from the seg
        ;; itself, so we only need start-col for placement.
        paint-seg!
        (fn [start-char end-char start-col]
          (when (and start-char (< (long start-char) (long end-char)))
            (let [seg (subs line start-char end-char)]
              (when (pos? (count seg))
                (if (seq body-styles)
                  (p/styled g body-styles (p/put-str! g (+ (long x) (long start-col)) y seg))
                  (p/put-str! g (+ (long x) (long start-col)) y seg))))))]

    (loop [i
           0

           char-pos
           0

           col-pos
           0

           seg-start-char
           nil

           seg-start-col
           nil]

      (if (>= i n)
        (paint-seg! seg-start-char char-pos seg-start-col)
        (let [tc
              ^com.googlecode.lanterna.TextCharacter (aget cells i)

              grapheme
              ^String (.getCharacterString tc)

              g-chars
              (long (.length grapheme))

              g-cols
              (if (.isDoubleWidth tc) 2 1)

              divider?
              (and (= 1 g-chars) (or (= (.charAt grapheme 0) \┃) (= (.charAt grapheme 0) \│)))]

          (if divider?
            (do (paint-seg! seg-start-char char-pos seg-start-col)
                (recur (inc i) (+ char-pos g-chars) (+ col-pos g-cols) nil nil))
            (recur (inc i)
                   (+ char-pos g-chars)
                   (+ col-pos g-cols)
                   (or seg-start-char char-pos)
                   (or seg-start-col col-pos))))))))
;; ---------------------------------------------------------------------------
(defn- tool-color-role->fg
  [role]
  (case role
    :tool-color/read
    t/tool-color-read

    :tool-color/search
    t/tool-color-search

    :tool-color/preview
    t/tool-color-preview

    :tool-color/edit
    t/tool-color-edit

    :tool-color/create
    t/tool-color-create

    :tool-color/delete
    t/tool-color-delete

    :tool-color/move
    t/tool-color-move

    :tool-color/shell
    t/tool-color-shell

    :tool-color/meta
    t/tool-color-meta

    :tool-color/test
    t/tool-color-test

    :tool-color/error
    t/code-error-fg

    nil))

;; The assistant footer line + routing fallback note are the SHARED, humanized
;; turn-summary formatters in `internal.format` (`vis/meta-summary-line` +
;; `vis/meta-fallback-note`), so the CLI bracket, this TUI footer, and the
;; Telegram tagline read identically. The TUI is the only surface that floats
;; the note on its own faint row; CLI/Telegram fold it inline via
;; `vis/format-meta-line`.
(def ^:dynamic *image-placements*
  "When bound to an atom holding a vector, `draw-chat-bubble!` conj's the
   EXACT painted screen position of every inline-image row it draws:
   `{:row abs-screen-row :col abs-screen-col :img {...}}`. The screen loop
   drains it after the delta refresh to place Kitty/iTerm2 graphics precisely
   over the reserved cells — recording from inside the paint loop avoids
   re-deriving the bubble's chrome offsets (role banner, top pad, turn
   separator) that the copy-region geometry deliberately skips."
  nil)

(defn draw-chat-bubble!
  "Draw a chat message at the given row. No border, no bubble container.
   `message` is a map: {:role :user|:assistant, :text str, :timestamp #inst}
   Optional `:duration-ms` for assistant response time.
   `left` and `max-w` define the horizontal bounds.

   Layout (no outer border, no horizontal rule under the label,
   both roles left-anchored):
     [role-label]                              [timestamp]
     [content lines, each with role bg fill]
     [meta line, dimmed, when present]
     [blank gap]

   User content rows get a subtle blue-gray background block
   (`user-bubble-bg`) to visually separate user input from the rest of
   the session. Assistant content rows render on terminal bg -
   the only fills come from inline marker zones (code blocks,
   answer-bg, etc.).

   Returns the number of screen rows consumed (including spacing).

   Extra params:
     `viewport-top` and `viewport-h` describe the absolute screen
     window the messages-area paints into. They’re consulted by the
     per-row click-region painters to decide whether to register a
     click region for an off-screen row (they don’t).
     Callers that paint outside `draw-messages-area!` (tests, REPL
     exploration) can pass `0 / 0` to disable click registration."
  [^TextGraphics g {:keys [role text timestamp status slash?] :as message} start-row left max-w &
   [{:keys [viewport-top viewport-h] :or {viewport-top 0 viewport-h 0}}]]
  (let [user?
        (= role :user)

        queued?
        (= :queued status)

        warning?
        (warning-message? text)

        ;; Cancelled turns are status messages, not real answers -
        ;; render the entire bubble dim (gray + italic), drop the
        ;; meta line, dim the role label too. Skips markdown so a
        ;; bare text like \"Cancelled by user.\" reads naturally.
        cancelled?
        (= :cancelled status)

        ;; Legacy turn separators are disabled. Keep top-sep-h in
        ;; layout math so old callers stay shape-compatible without
        ;; reserving any spacer row.
        top-sep-h
        0

        label
        (cond queued? "Queued"
              user? "You"
              :else "Vis")

        bubble-w
        max-w

        ;; Symmetric inner padding (2 cols each side) inside the
        ;; message column. Applies to plain text AND to every
        ;; styled-marker zone (code blocks, answer, iteration
        ;; headers) so right-aligned labels like "ITERATION 1" /
        ;; "FINAL ANSWER" sit nicely inset from the right edge
        ;; instead of mashed against it.
        h-pad
        2

        content-w
        (max 1 (- (long bubble-w) (* 2 (long h-pad))))

        ;; `:prewrapped-lines` is set by `virtual.clj` projection for
        ;; every visible bubble (walker output); the `wrap-text`
        ;; fallback only triggers for placeholders / synthetic msgs
        ;; that bypass projection (rare).
        raw-lines
        (or (:prewrapped-lines message) (wrap-text text content-w))

        lines
        (clipped-lines raw-lines content-w bubble-w)

        line-meta
        (or (:line-meta message) (vec (repeat (count lines) nil)))

        ;; Mid-window walker support: when `:lines-window {:start :total-h}`
        ;; is set on the message (by virtual/layout for genuine mid-scroll),
        ;; `lines` is only rows [start, start+count) of the full bubble.
        ;; Total height comes from `:total-h`; the painter loop translates
        ;; logical row `i` to lines-vec index via `(- i lines-offset)`.
        lines-window
        (:lines-window message)

        lines-offset
        (long (or (:start lines-window) 0))

        bubble-h
        (long (or (:total-h lines-window) (count lines)))

        bx
        left

        ;; No bg fill on plain assistant text - we sit on terminal-bg.
        ;; `:warning` and user messages each get a tinted block so
        ;; they're impossible to miss in the timeline. Cancelled turns
        ;; intentionally render on bare terminal-bg - the muted italic
        ;; fg + status footer carry the "aborted" signal without a
        ;; bubble-wide fill that competed visually with adjacent
        ;; assistant messages.
        bg-color
        (cond warning? t/warning-bg
              queued? t/terminal-bg
              ;; User messages fill their content rows with a
              ;; very pale warm-yellow block so "you said this"
              ;; reads as its own zone, distinct from the white
              ;; assistant area.
              user? t/user-bubble-bg
              :else t/terminal-bg)

        fg-color
        (cond cancelled? t/cancelled-fg
              queued? t/dialog-hint
              warning? t/warning-fg
              user? t/user-bubble-fg
              :else t/ai-bubble-fg)

        role-fg
        (cond cancelled? t/dialog-hint
              queued? t/dialog-hint
              user? t/user-role-fg
              :else t/ai-role-fg)

        time-str
        (vis/format-date timestamp)

        ;; Below-message meta (assistant only): "blockether/glm-5.1 /
        ;; 1 iter / tok 11461→35 / ~$0.006954 / 4.9s". Same surface
        ;; form `format-meta-line` produces for the CLI bracket and
        ;; the Telegram tagline. Provider + model auto-extract from
        ;; `:cost :provider` / `:cost :model` (where the iteration
        ;; runtime persists them). The chat-state's bare-name `:model`
        ;; field is intentionally NOT passed as the `:model` override
        ;; here - doing so would defeat the provider/model rendering.
        ;; Cancelled turns skip the whole block; there's no answer to
        ;; attribute and "0 iters / no model" reads as clutter under
        ;; a "Cancelled" placeholder. Slash/command turns skip it too: a
        ;; `/voice`-style toggle ran no model and took no meaningful time,
        ;; so "<model> / <time>" under it is pure noise.
        meta-str
        (when (and (not user?) (not cancelled?) (not slash?)) (vis/meta-summary-line message))

        ;; Two-tier footer: the main line stays clean; the routing/fallback
        ;; story rides a faint, italic second row that only exists on a fallback.
        fallback-note
        (when (and meta-str (not user?) (not cancelled?)) (vis/meta-fallback-note message))]

    ;; Role label (bold, role-colored) + timestamp.
    (when (pos? top-sep-h)
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-border t/terminal-bg)
      (p/put-str! g bx start-row (p/horiz-line bubble-w)))
    (let [label-row (+ (long start-row) (long top-sep-h))]
      (p/clear-styles! g)
      (p/set-colors! g role-fg t/terminal-bg)
      (p/styled g [p/BOLD] (p/put-str! g bx label-row label))
      (when time-str
        (let [right-edge (+ (long bx) (long bubble-w))
              time-w (p/display-width time-str)
              time-x (- (long right-edge) (long time-w))]

          (p/set-colors! g t/dialog-hint t/terminal-bg)
          (p/put-str! g time-x label-row time-str))))
    ;; Content begins directly under the role banner for ASSISTANT
    ;; bubbles - the previous breathing row read as an unwanted top
    ;; margin above the thinking block. USER bubbles keep the
    ;; breathing row so the yellow block has visible vertical
    ;; padding above the typed text (otherwise the yellow band
    ;; hugs the first character and reads as a single-row strip).
    ;; User bubbles also keep one breathing row BELOW the typed text.
    ;; Mirror this in `bubble-height*` so the math stays in sync.
    (let [top-pad
          (if user? 1 0)

          bottom-pad
          (if user? 1 0)

          btop
          (+ (long start-row) (long top-sep-h) 1 (long top-pad))]

      ;; No bubble-wide background fill. Plain user / assistant text
      ;; renders directly on terminal bg - the only fills come from
      ;; structured-trace marker zones (code blocks, answer
      ;; section). Roles are visually distinguished by the colored
      ;; label and the blank row beneath it (no horizontal divider).
      ;; Bulk fill the content rows for the cases that want a
      ;; bubble-wide colored block: warnings (amber alarm) and user
      ;; messages (warm light-yellow zone). Assistant plain text and
      ;; cancelled status keep terminal bg - their per-line marker
      ;; switch handles any zone-specific fills (code blocks,
      ;; answer-bg, etc.) on its own.
      ;;
      ;; Warnings: fill ONLY the content rows so the amber alarm
      ;; reads as the message itself, no extra chrome.
      (when warning? (p/set-bg! g bg-color) (p/fill-rect! g bx btop bubble-w (max 1 bubble-h)))
      ;;
      ;; Cancelled: NO bubble-wide fill. The muted italic fg
      ;; (`cancelled-fg`) + dimmed role label + plain status footer
      ;; carry the "aborted" signal on bare terminal-bg.
      ;;
      ;; User messages: fill content rows only; the single message gap remains outside the
      ;; fill.
      (when (and user? (not queued?))
        (p/set-bg! g bg-color)
        ;; Shrink-wrap the user block to its content instead of always
        ;; spanning the full message column: a short prompt gets a snug
        ;; band (widest content row + symmetric h-pad) rather than a
        ;; terminal-wide yellow stripe. Capped at `bubble-w`, so a long
        ;; line that already fills the column still reaches both edges.
        (let [content-cols
              (reduce (fn [^long m l]
                        (max m (p/display-width l)))
                      0
                      lines)

              user-fill-w
              (max 1 (min (long bubble-w) (+ (long content-cols) (* 2 (long h-pad)))))]

          (p/fill-rect! g
                        bx
                        (- (long btop) (long top-pad))
                        user-fill-w
                        (+ (max 1 (long bubble-h)) (long top-pad) (long bottom-pad)))))
      ;; Text content - per-line styling via invisible marker prefixes
      ;;
      ;; The \"answer zone\" starts at the first line carrying ANY
      ;; structural answer-* marker. Every line AFTER that index
      ;; renders on `answer-bg`. We used to anchor strictly on
      ;; `answer-hdr-marker` (the right-aligned \"FINAL ANSWER\"
      ;; superscript), but that marker became opt-in via
      ;; `:show-final-answer-header` - when off, the answer body lost
      ;; its blue zone and rendered on white terminal bg. Now we
      ;; accept any of the three structural markers
      ;; `format-answer-with-thinking*` always emits:
      ;;   answer-sep-marker  bold rule between trace and answer
      ;;   answer-hdr-marker  optional \"FINAL ANSWER\" superscript
      ;;   answer-pad-marker  blank padding above/below the answer
      ;; Whichever appears first wins.
      (let [n
            (count lines)

            ;; Restrict iteration to rows that actually intersect the
            ;; viewport. Pre-virtualisation we walked all `n` lines
            ;; on every redraw and let Lanterna clip OS-side; for an
            ;; 11k-row trace bubble that pegged the render thread at
            ;; ~110 ms / frame even with a fully warm cache. Now we
            ;; only touch the rows whose screen
            ;; offset is inside [0, viewport-h).
            ;;
            ;; `viewport-h` is the messages-area inner height; the
            ;; caller in `draw-messages-area!` passes it. Tests /
            ;; REPL exploration that pass `viewport-h=0` (the
            ;; arity-3 default) keep the old paint-everything
            ;; behaviour.
            i-start
            (long (if (pos? (long viewport-h)) (max 0 (- (long btop))) 0))

            i-end
            (long (if (pos? (long viewport-h)) (min (long n) (- (long viewport-h) (long btop))) n))

            iteration-bg
            t/iteration-header-bg

            answer-marker?
            (fn [^String l]
              (or (str/starts-with? l answer-sep-marker)
                  (str/starts-with? l answer-hdr-marker)
                  (str/starts-with? l answer-pad-marker)))

            ;; Allocation-free O(n) scan, cached by `lines`-identity.
            ;; The previous `(some (map-indexed vector lines))`
            ;; allocated a fresh `[i l]` tuple per element on every
            ;; redraw - 11k tuples per frame on the big bubbles.
            ;; `wrap-text` is identity-stable across frames, so the
            ;; cached answer-start hits next time around.
            answer-start
            (cached* [::ans-start (System/identityHashCode lines)]
                     #(loop [i
                             0]

                        (cond (>= i n) n
                              (answer-marker? (nth lines i)) i
                              :else (recur (inc i)))))

            ;; Per-code-block band width: each contiguous run of
            ;; `md-code-marker` rows (incl. the blank pad rows + ``` fences)
            ;; shares ONE width = widest content row + 2*code-block-h-pad, so
            ;; the colored band hugs the code with even left/right padding
            ;; instead of stretching to the bubble edge. Map is lines-idx -> w.
            code-band-w
            (cached* [::code-band (System/identityHashCode lines)]
                     #(loop [i
                             0

                             acc
                             (transient {})]

                        (if (>= (long i) (long n))
                          (persistent! acc)
                          (if (str/starts-with? (nth lines i) md-code-marker)
                            (let [j
                                  (loop [j i]
                                    (if (and (< (long j) (long n))
                                             (str/starts-with? (nth lines j) md-code-marker))
                                      (recur (inc (long j)))
                                      j))

                                  w
                                  (reduce (fn [m k]
                                            (max (long m)
                                                 (long (p/display-width (subs (nth lines k) 1)))))
                                          0
                                          (range i j))]

                              (recur j
                                     (reduce (fn [a k]
                                               (assoc! a k w))
                                             acc
                                             (range i j))))
                            (recur (inc (long i)) acc)))))]

        (loop [i i-start]
          (when (< i i-end)
            ;; Mid-window walker support: when `:lines-window` is
            ;; set, `lines` is only rows [start, start+count) of the
            ;; full bubble. Out-of-window rows render blank — the
            ;; bubble's allotted band stays clear for those rows,
            ;; matching what the user expects when scrolled past the
            ;; rendered window. `lines-offset` is bound at the top
            ;; of `draw-chat-bubble!` from `:lines-window :start` (or
            ;; 0 when no window). See virtual/layout's mid-scroll
            ;; wireup for `:window-start` / `:window-num`.
            (let [lines-idx (- i lines-offset)
                  noop-row? (or (neg? lines-idx) (>= lines-idx (count lines)))
                  line (when-not noop-row? (nth lines lines-idx))
                  meta (when-not noop-row? (nth line-meta lines-idx nil))]

              (when-not noop-row?
                (p/clear-styles! g)
                (let [in-answer? (> i (long answer-start))
                      ;; Two coordinate systems per content row:
                      ;;   text  at `x = bx + h-pad`, runs `content-w` cols  - keeps
                      ;;         body padded inside the column.
                      ;;   fills at `fbx = bx`,        run  `bubble-w`  cols - every
                      ;;         marker zone (code, answer, iteration header, thinking,
                      ;;         table...) paints the FULL message column so the colored band
                      ;;         reaches both edges of the messages area instead of leaving a
                      ;;         2-col white strip on each side.
                      ;; Right-aligned labels in `format-iteration-entry` write at
                      ;; `x` and inherit `content-w`, so they still sit inset from
                      ;; the right edge by h-pad even though the bg fills past them.
                      ;; Assistant answer text starts at the same column as
                      ;; the `Vis` label. User bubbles keep their inset so the
                      ;; left rail remains visually separate from prompt text.
                      x (+ (long bx) (long (if user? h-pad 0)))
                      y (+ (long btop) (long i))
                      iw bubble-w
                      fbx bx
                      marker (when (pos? (count line)) (subs line 0 1))
                      body (when marker (subs line 1))
                      output-indented? (and (contains? output-indentable-markers marker)
                                            (str/starts-with? body tool-output-indent))
                      line
                      (if output-indented? (str marker (subs body (count tool-output-indent))) line)
                      ;; Result/code rows inset 2 cols for breathing room — EXCEPT a
                      ;; no-chevron summary headline (`:result-headline`): with no
                      ;; chevron to fill the slot, that inset reads as a dangling left
                      ;; margin, so paint it flush against the band's left edge.
                      code-text-inset? (and (not user?)
                                            (contains? code-text-inset-markers marker)
                                            (not= :result-headline (:kind meta)))
                      x (cond-> x
                          output-indented?
                          (+ (long tool-output-indent-cols))

                          code-text-inset?
                          (+ 2))
                      iw
                      (if output-indented? (max 0 (- (long iw) (long tool-output-indent-cols))) iw)
                      fbx (if output-indented? (+ (long fbx) (long tool-output-indent-cols)) fbx)]

                  ;; Pre-fill answer zone bg so ALL line types get it
                  (when in-answer? (p/set-bg! g t/answer-bg) (p/fill-rect! g fbx y iw 1))
                  ;; Reserved inline-image row: record its EXACT painted
                  ;; position (absolute screen coords) for the post-refresh
                  ;; graphics pass. The row itself paints blank below.
                  (when (and *image-placements* (= :image (:kind meta)))
                    (swap! *image-placements* conj
                      {:row (+ (long viewport-top) (long y)) :col x :img (:img meta)}))
                  (cond
                    ;; ── Iteration header - right-aligned, subtle ──
                    (str/starts-with? line iteration-hdr-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/put-str! g x y (subs line 1))
                        ;; BLOCK header is a disclosure toggle: clicking it
                        ;; collapses/expands the whole card (code + op rows).
                        (when (= :toggle-details (:kind meta))
                          (let [abs-row (+ (long viewport-top) (long y))
                                click-width (long (or (:click-width meta) iw))]

                            (cr/register! {:bounds {:row abs-row :col x :width click-width}
                                           :kind :toggle-details
                                           :session-id (:session-id meta)
                                           :node-id (:node-id meta)
                                           :collapsed? (:collapsed? meta)}))))
                    ;; ── Iteration recap — triple-zone paint ──
                    ;;
                    ;; Each recap line carries `:meta {:recap-kind :task |
                    ;; :spec | :fact | :title | :recap}` so we can paint a
                    ;; per-kind accent without changing the marker scheme.
                    ;;
                    ;; Layout (terminal-bg throughout):
                    ;;   col 0       — marker (zero-width sentinel)
                    ;;   col 1       — leading pad (single space)
                    ;;   col 2       — gutter glyph `▎` in kind-fg col 3       — pad
                    ;;   col 4..N    — badge token (TASK / SPEC / …) in
                    ;;                 kind-fg, BOLD
                    ;;   col N+2..   — body in dialog-hint, BOLD + ITALIC
                    ;;
                    ;; Falls back to the old uniform paint when the line
                    ;; lacks a recognised badge (legacy recaps, untagged
                    ;; rows, defensive guard for shape drift).
                    (str/starts-with? line recap-marker)
                    (let [raw (subs line 1)
                          recap-kind (:recap-kind meta)
                          kind-fg (case recap-kind
                                    :task
                                    t/tool-color-edit

                                    :spec
                                    t/tool-color-meta

                                    :fact
                                    t/tool-color-read

                                    :title
                                    t/md-h1-fg

                                    :recap
                                    t/dialog-hint-key

                                    :consult
                                    t/tool-color-search

                                    t/dialog-hint)
                          trimmed (str/triml raw)
                          parts (str/split trimmed #"\s+" 2)
                          badge-token (first parts)
                          rest-text (or (second parts) "")
                          badge? (and recap-kind (contains? recap-kinds badge-token))]

                      (if-not badge?
                        ;; Untagged or first-wrap continuation row — same legacy paint.
                        (do (p/set-colors! g t/dialog-hint t/terminal-bg)
                            (p/styled g
                                      [p/BOLD p/ITALIC]
                                      (p/paint-styled-line! g
                                                            x
                                                            y
                                                            raw
                                                            t/dialog-hint
                                                            t/terminal-bg
                                                            t/code-block-fg
                                                            t/code-block-bg)))
                        ;; Tagged head row — paint gutter + badge + body.
                        (let [gutter-x (inc (long x))       ; col after leading pad
                              badge-x (+ (long gutter-x) 2) ; ▎ + space
                              body-x (+ (long badge-x) (count badge-token) 2)]

                          ;; Leading pad space first — keeps the band
                          ;; aligned with the surrounding chrome.
                          (p/set-colors! g t/dialog-hint t/terminal-bg)
                          (p/put-str! g x y " ")
                          ;; Gutter glyph in kind color.
                          (p/set-colors! g kind-fg t/terminal-bg)
                          (p/styled g [p/BOLD] (p/put-str! g gutter-x y "▎"))
                          (p/put-str! g (inc (long gutter-x)) y " ")
                          ;; Badge token — kind-fg, BOLD.
                          (p/set-colors! g kind-fg t/terminal-bg)
                          (p/styled g [p/BOLD] (p/put-str! g badge-x y badge-token))
                          ;; Two-space separator between badge and body.
                          (p/set-colors! g t/dialog-hint t/terminal-bg)
                          (p/put-str! g (+ (long badge-x) (count badge-token)) y "  ")
                          ;; Body — dialog-hint, BOLD + ITALIC, inline sentinels honoured.
                          (p/styled g
                                    [p/BOLD p/ITALIC]
                                    (p/paint-styled-line! g
                                                          body-x
                                                          y
                                                          rest-text
                                                          t/dialog-hint
                                                          t/terminal-bg
                                                          t/code-block-fg
                                                          t/code-block-bg)))))
                    ;; ── Thinking - dimmed bg, italic ── Inline span sentinels (**bold** etc.)
                    ;; embedded in thinking-mode prose are honoured via paint-styled-line!
                    ;; on top of the italic base style. Italic stacks with
                    ;; SGR/BOLD per terminal SGR rules, so a bolded word
                    ;; inside thinking renders as bold-italic, not as plain
                    ;; bold (which would visually escape the thinking zone).
                    ;; ── Affordance hint (e.g. "↑ to edit") — accent fg on the
                    ;; REGULAR bubble bg, italic. Deliberately NOT the dim queue
                    ;; band, so the hint reads as an actionable control, not as
                    ;; another queued message.
                    (str/starts-with? line hint-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/header-active-tab-accent bg-color)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/ITALIC] (p/put-str! g x y raw)))
                    ;; ── Queued section header — a top-left corner glyph `┌` plus a
                    ;; bold accent label ("Queued · N") on the regular bubble
                    ;; bg. The rail runs down the WHOLE queue block (header,
                    ;; items, and the spacer rows between them) so the section
                    ;; reads as one bracketed group, like a "You" bubble's
                    ;; left edge.
                    (str/starts-with? line queue-hdr-marker)
                    (let [raw (subs line 1)]
                      (p/set-bg! g bg-color)
                      (p/fill-rect! g fbx y iw 1)
                      (p/set-colors! g t/header-active-tab-accent bg-color)
                      (p/styled g [p/BOLD] (p/put-str! g x y (str "┌ " raw))))
                    ;; ── Queued message row — left rail `│`, then the ordinal
                    ;; ("1. ") in the accent gutter, then the single-line
                    ;; preview (already right-clipped with an ellipsis so it
                    ;; never wraps) in dim italic. Everything sits on the
                    ;; regular bubble bg — NO gray band — visually bracketed by
                    ;; the rail. `:queue-gutter` in meta is the ordinal column
                    ;; width (0 for the spacer rows, which paint the rail only).
                    (str/starts-with? line queue-item-marker)
                    (let [raw (subs line 1)
                          gutter-n (long (or (:queue-gutter meta) 0))
                          cut (min gutter-n (count raw))
                          ord (subs raw 0 cut)
                          msg (subs raw cut)]

                      (p/set-bg! g bg-color)
                      (p/fill-rect! g fbx y iw 1)
                      ;; Rail glyph at col x, ordinal two cols in (past the
                      ;; rail + its trailing space) — both accent, bold.
                      (p/set-colors! g t/header-active-tab-accent bg-color)
                      (p/styled g
                                [p/BOLD]
                                (p/put-str! g x y "│")
                                (when (pos? (long gutter-n)) (p/put-str! g (+ (long x) 2) y ord)))
                      ;; Preview — dim italic, after the ordinal.
                      (p/set-colors! g t/dialog-hint bg-color)
                      (p/styled g
                                [p/ITALIC]
                                (p/paint-styled-line! g
                                                      (+ (long x) 2 (long gutter-n))
                                                      y
                                                      msg
                                                      t/dialog-hint
                                                      bg-color
                                                      t/code-block-fg
                                                      t/code-block-bg)))
                    ;; ── Queue bottom border — a square corner `└` at the rail
                    ;; column, then a horizontal rule filling the rest of the
                    ;; width. Caps the left rail and separates the queued items
                    ;; from the "↑ to edit" hint below.
                    (str/starts-with? line queue-border-marker)
                    (do (p/set-colors! g t/header-active-tab-accent bg-color)
                        (p/fill-rect! g fbx y iw 1)
                        (p/put-str! g x y (str "└" (repeat-str "─" (max 0 (dec (long iw)))))))
                    (str/starts-with? line thinking-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/ITALIC]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      raw
                                                      t/dialog-hint
                                                      t/iteration-header-bg
                                                      t/code-block-fg
                                                      t/code-block-bg))
                      ;; A thinking-band row may BE the clickable THINKING
                      ;; disclosure header (kept inside the dim band so the
                      ;; label reads as part of the thinking bubble, not a
                      ;; detached op-row). Register the same toggle click
                      ;; region the op-row branch does.
                      (when (= :toggle-details (:kind meta))
                        (let [abs-row (+ (long viewport-top) (long y))
                              click-width (long (or (:click-width meta) iw))]

                          (cr/register! {:bounds {:row abs-row :col x :width click-width}
                                         :kind :toggle-details
                                         :session-id (:session-id meta)
                                         :node-id (:node-id meta)
                                         :collapsed? (:collapsed? meta)}))))
                    ;; ── Code (success) - light green bg ──
                    (str/starts-with? line code-ok-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/code-block-fg t/code-ok-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y raw t/code-block-fg t/code-ok-bg))
                    ;; ── Code (error) - light red bg ──
                    (str/starts-with? line code-err-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/code-block-fg t/code-err-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y raw t/code-block-fg t/code-err-bg))
                    ;; ── Code (running, no status yet) - neutral bg ──
                    (str/starts-with? line code-marker)
                    (do (p/set-colors! g t/code-block-fg t/code-block-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (paint-ansi-line! g x y (subs line 1) t/code-block-fg t/code-block-bg))
                    ;; ── Duration annotation ──
                    (str/starts-with? line duration-marker)
                    (do (p/set-colors! g t/code-duration-fg iteration-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/put-str! g x y (subs line 1)))
                    ;; ── Result (success) - neutral code-block bg ──
                    (str/starts-with? line result-marker)
                    ;; The RESULT zone gets its OWN background band (`result-bg` —
                    ;; warmer than the cool code-bg / thinking gray) so a tool op-card
                    ;; / eval output reads as a distinct zone, not blended with code.
                    ;; A native-tool badge carries `:color-role` in its meta → paint
                    ;; the text in the tool's color; plain result rows fall back to the
                    ;; neutral result fg. Embedded ANSI (diff +/-) still translates.
                    (let [res-fg (or (tool-color-role->fg (:color-role meta)) t/code-result-fg)]
                      (p/set-colors! g res-fg t/result-bg)
                      (p/fill-rect! g fbx y iw 1)
                      ;; Inline-code chips in a result body (rg per-file path
                      ;; headers, patch/move targets) paint on the distinct
                      ;; `result-path` accent so filenames read as headers
                      ;; instead of blending into the neutral result ink.
                      (paint-ansi-line! g
                                        x
                                        y
                                        (subs line 1)
                                        res-fg
                                        t/result-bg
                                        t/result-path-fg
                                        t/result-path-bg)
                      (paint-turn-stamp! g x y (subs line 1) t/result-bg)
                      (when (= :toggle-details (:kind meta))
                        (let [abs-row (+ (long viewport-top) (long y))
                              click-width (long (or (:click-width meta) iw))]

                          (cr/register! {:bounds {:row abs-row :col x :width click-width}
                                         :kind :toggle-details
                                         :session-id (:session-id meta)
                                         :node-id (:node-id meta)
                                         :collapsed? (:collapsed? meta)}))))
                    ;; ── Result (error) - neutral code-block bg ──
                    (str/starts-with? line err-result-marker)
                    (do
                      (p/set-colors! g t/code-error-result-fg t/code-block-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y (subs line 1) t/code-error-result-fg t/code-block-bg)
                      (paint-turn-stamp! g x y (subs line 1) t/code-block-bg)
                      (when (= :toggle-details (:kind meta))
                        (let [abs-row (+ (long viewport-top) (long y))
                              click-width (long (or (:click-width meta) iw))]

                          (cr/register! {:bounds {:row abs-row :col x :width click-width}
                                         :kind :toggle-details
                                         :session-id (:session-id meta)
                                         :node-id (:node-id meta)
                                         :collapsed? (:collapsed? meta)}))))
                    ;; ── Code block padding (running / neutral) ──
                    ;; These rows are usually blank top/bottom band edges,
                    ;; but the per-form footer deliberately rides the same
                    ;; marker. Paint optional payload instead of swallowing it.
                    (str/starts-with? line code-pad-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/code-block-fg t/code-block-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-code-pad-payload! g x y raw t/code-block-fg t/code-block-bg))
                    ;; ── Code block padding (success) ──
                    (str/starts-with? line code-ok-pad-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/code-block-fg t/code-ok-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-code-pad-payload! g x y raw t/code-block-fg t/code-ok-bg))
                    ;; ── Code block padding (error) ──
                    (str/starts-with? line code-err-pad-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/code-block-fg t/code-err-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-code-pad-payload! g x y raw t/code-block-fg t/code-err-bg))
                    ;; ── Iteration zone padding (margin between blocks) ──
                    (str/starts-with? line iteration-pad-marker) (do (p/set-bg! g bg-color)
                                                                     (p/fill-rect! g fbx y iw 1))
                    ;; ── Answer separator - bold horizontal rule between iterations and answer
                    ;; ──
                    (str/starts-with? line answer-sep-marker)
                    (do (p/set-colors! g t/answer-sep-fg bg-color)
                        (p/styled g [p/BOLD] (p/put-str! g fbx y (repeat-str \u2500 iw))))
                    ;; ── Answer header - right-aligned superscript on bubble bg ──
                    (str/starts-with? line answer-hdr-marker)
                    (do (p/set-colors! g t/iteration-header-fg bg-color)
                        (p/put-str! g x y (subs line 1)))
                    ;; ── Answer-mode markdown headings (gold gradient) ──
                    ;;
                    ;; H1->H3 use a saturated amber/gold gradient
                    ;; (`t/md-h1-fg` etc.) instead of the body fg + bold,
                    ;; so headings stop disappearing into surrounding
                    ;; prose. The colour stack is engineered to pop on
                    ;; both the white assistant bg and the new pale-blue
                    ;; answer-bg - see the WCAG ratios in theme.clj.
                    ;; Heading lines now run their content through
                    ;; `markdown->inline` before reaching here, so the line
                    ;; payload may contain INLINE_*_ON/OFF sentinels for
                    ;; nested bold/italic/code spans. Painting via
                    ;; `paint-styled-line!` (instead of raw `put-str!`)
                    ;; consumes the sentinels in-place - they stay out of
                    ;; the terminal output (no PUA glyphs). Heading colour + BOLD
                    ;; remain the BASE style; inline spans STACK on top
                    ;; (e.g. `## **plain** *and italic*` keeps the gold +
                    ;; bold base, plus italic on the second word).
                    (str/starts-with? line md-h1-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g t/md-h1-fg lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/BOLD]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      (subs line 1)
                                                      t/md-h1-fg
                                                      lbg
                                                      t/code-block-fg
                                                      t/code-block-bg)))
                    (str/starts-with? line md-h2-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g t/md-h2-fg lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/BOLD]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      (subs line 1)
                                                      t/md-h2-fg
                                                      lbg
                                                      t/code-block-fg
                                                      t/code-block-bg)))
                    (str/starts-with? line md-h3-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g t/md-h3-fg lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/BOLD]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      (subs line 1)
                                                      t/md-h3-fg
                                                      lbg
                                                      t/code-block-fg
                                                      t/code-block-bg)))
                    (str/starts-with? line md-bold-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g fg-color lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/BOLD]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      (subs line 1)
                                                      fg-color
                                                      lbg
                                                      t/code-block-fg
                                                      t/code-block-bg)))
                    ;; <summary> disclosure label - lavender band that
                    ;; spans the full bubble inner-width. Bold + violet
                    ;; foreground reads as a section heading; the bg
                    ;; tint is unique among answer-zone tints (code is
                    ;; blue-gray, warning is amber, answer is white) so
                    ;; the user can locate disclosure regions at a
                    ;; glance even with no click affordance. The fill
                    ;; covers `iw` so the band extends to the bubble's
                    ;; right edge, not just the text width.
                    (str/starts-with? line md-summary-marker)
                    (let [abs-row (+ (long viewport-top) (long y))
                          hovered? (and (= :toggle-details (:kind meta))
                                        (= abs-row (:row (:bounds (cr/hovered)))))
                          bg (if hovered? t/link-chrome-hover-bg t/md-summary-bg)
                          tool-fg (tool-color-role->fg (:color-role meta))
                          fg (cond hovered? t/link-chrome-hover-fg
                                   tool-fg tool-fg
                                   :else t/md-summary-fg)]

                      (p/set-colors! g fg bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/BOLD]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      (subs line 1)
                                                      fg
                                                      bg
                                                      t/code-block-fg
                                                      t/code-block-bg))
                      (case (:kind meta)
                        :toggle-details
                        (let [click-width (long (or (:click-width meta) iw))]
                          (cr/register! {:bounds {:row abs-row :col fbx :width click-width}
                                         :kind :toggle-details
                                         :session-id (:session-id meta)
                                         :node-id (:node-id meta)
                                         :collapsed? (:collapsed? meta)}))

                        nil))
                    (str/starts-with? line md-code-marker)
                    (let [band-w (if (and in-answer? (:list-nested-code? meta))
                                   (min (long iw)
                                        (+ (long (get code-band-w lines-idx 0))
                                           (* 2 (long code-block-h-pad))))
                                   iw)]
                      (p/set-colors! g t/code-block-fg t/code-block-bg)
                      (p/fill-rect! g fbx y band-w 1)
                      (paint-ansi-line! g
                                        (+ (long x) (long code-block-h-pad))
                                        y
                                        (subs line 1)
                                        t/code-block-fg
                                        t/code-block-bg))
                    ;; Bullet items: same inline-span treatment as plain text.
                    ;; `- **bold** thing` should bold the word.
                    (str/starts-with? line md-bullet-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g fg-color lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/paint-styled-line! g
                                            x
                                            y
                                            (subs line 1)
                                            fg-color
                                            lbg
                                            t/code-block-fg
                                            t/code-block-bg))
                    ;; Blockquote: italic + dim base, inline spans honoured
                    ;; on top. Was the user-visible bug - `> **Lącznie:**`
                    ;; rendered with literal asterisks because the previous
                    ;; quote painter used raw put-str! and the quote branch
                    ;; in markdown->lines didn't run markdown->inline. Both
                    ;; halves now fixed; this is the painter half.
                    (str/starts-with? line md-quote-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g t/dialog-hint lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/ITALIC]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      (subs line 1)
                                                      t/dialog-hint
                                                      lbg
                                                      t/code-block-fg
                                                      t/code-block-bg)))
                    (str/starts-with? line md-hr-marker) (let [lbg
                                                               (if in-answer? t/answer-bg bg-color)]
                                                           (p/set-colors! g t/answer-sep-fg lbg)
                                                           (p/fill-rect! g fbx y iw 1)
                                                           (p/put-str! g x y (subs line 1)))
                    ;; ── Markdown table (answer) ── grid blends into surrounding zone
                    ;; Chrome (│┌─┐├┼┤└┴┘─) stays in muted `code-border-fg`,
                    ;; cell text in dark text color, headers bold.
                    ;;
                    ;; Background follows context: if the table is INSIDE
                    ;; the answer zone (after `answer-hdr-marker`) it sits
                    ;; on `answer-bg` so it visually belongs to the answer
                    ;; block; otherwise on `code-block-bg` (e.g. a plain
                    ;; assistant message without an answer zone). The user
                    ;; complaint that triggered this: tables in the answer
                    ;; zone used to break the answer's blue band with a
                    ;; different gray, looking like an alien element.
                    (or (str/starts-with? line md-table-head-marker)
                        (str/starts-with? line md-table-sep-marker)
                        (str/starts-with? line md-table-row-marker))
                    (let [stripped (subs line 1)
                          head? (str/starts-with? line md-table-head-marker)
                          border? (str/starts-with? line md-table-sep-marker)
                          tbg (if in-answer? t/answer-bg t/code-block-bg)
                          tfg (if in-answer? t/answer-fg t/code-block-fg)]

                      (p/clear-styles! g)
                      (p/set-colors! g t/code-border-fg tbg)
                      (p/fill-rect! g fbx y iw 1)
                      (if border?
                        ;; Pure box-drawing line - single muted paint.
                        (p/put-str! g x y stripped)
                        ;; Header / body data row - dual-color split.
                        (paint-table-data-line! g
                                                x
                                                y
                                                stripped
                                                tfg
                                                t/code-border-fg
                                                tbg
                                                (when head? [p/BOLD]))))
                    ;; ── Thinking-mode markdown headings ── dim italic on iteration bg
                    ;;
                    ;; All thinking-mode prose branches go through
                    ;; `paint-styled-line!` instead of raw `put-str!` because
                    ;; their CONTENT now contains inline sentinels:
                    ;; markdown->lines runs `markdown->inline` on heading,
                    ;; bullet, quote, and bold-line bodies. Without the
                    ;; styled painter the sentinels (INLINE_CODE_ON/OFF etc.)
                    ;; would be written to the terminal as PUA chars and the
                    ;; backtick code spans would silently lose their styling.
                    ;; Fenced code (th-md-code-marker) is the one exception:
                    ;; its body is intentionally NOT inline-tokenised, so a
                    ;; raw put-str! is correct there.
                    (str/starts-with? line th-md-h1-marker)
                    (do (p/set-colors! g t/iteration-header-fg t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/styled g
                                  [p/BOLD p/ITALIC]
                                  (p/paint-styled-line! g
                                                        x
                                                        y
                                                        (subs line 1)
                                                        t/iteration-header-fg
                                                        t/iteration-header-bg
                                                        t/code-result-fg
                                                        t/code-block-bg)))
                    (str/starts-with? line th-md-h2-marker)
                    (do (p/set-colors! g t/iteration-header-fg t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/styled g
                                  [p/BOLD p/ITALIC]
                                  (p/paint-styled-line! g
                                                        x
                                                        y
                                                        (subs line 1)
                                                        t/iteration-header-fg
                                                        t/iteration-header-bg
                                                        t/code-result-fg
                                                        t/code-block-bg)))
                    (str/starts-with? line th-md-h3-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/styled g
                                  [p/BOLD p/ITALIC]
                                  (p/paint-styled-line! g
                                                        x
                                                        y
                                                        (subs line 1)
                                                        t/dialog-hint
                                                        t/iteration-header-bg
                                                        t/code-result-fg
                                                        t/code-block-bg)))
                    (str/starts-with? line th-md-bold-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/styled g
                                  [p/BOLD p/ITALIC]
                                  (p/paint-styled-line! g
                                                        x
                                                        y
                                                        (subs line 1)
                                                        t/dialog-hint
                                                        t/iteration-header-bg
                                                        t/code-result-fg
                                                        t/code-block-bg)))
                    ;; <summary> disclosure label inside the thinking
                    ;; zone. Same lavender-family band as the answer
                    ;; mode but darker / desaturated so it stays inside
                    ;; the dim reasoning region instead of stealing the
                    ;; eye like the answer-mode band would. Italic
                    ;; matches every other thinking-mode marker so the
                    ;; whole reasoning block reads as one cohesive zone.
                    (str/starts-with? line th-md-summary-marker)
                    (let [abs-row (+ (long viewport-top) (long y))
                          hovered? (and (= :toggle-details (:kind meta))
                                        (= abs-row (:row (:bounds (cr/hovered)))))
                          bg (if hovered? t/link-chrome-hover-bg t/th-md-summary-bg)
                          tool-fg (tool-color-role->fg (:color-role meta))
                          fg (cond hovered? t/link-chrome-hover-fg
                                   tool-fg tool-fg
                                   :else t/th-md-summary-fg)]

                      (p/set-colors! g fg bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g
                                [p/BOLD p/ITALIC]
                                (p/paint-styled-line! g
                                                      x
                                                      y
                                                      (subs line 1)
                                                      fg
                                                      bg
                                                      t/code-result-fg
                                                      t/code-block-bg))
                      (case (:kind meta)
                        :toggle-details
                        (let [click-width (long (or (:click-width meta) iw))]
                          (cr/register! {:bounds {:row abs-row :col fbx :width click-width}
                                         :kind :toggle-details
                                         :session-id (:session-id meta)
                                         :node-id (:node-id meta)
                                         :collapsed? (:collapsed? meta)}))

                        nil))
                    ;; Thinking fenced code: visible code-block bg, italic dim text.
                    ;; Clojure/EDN fences can carry zprint ANSI syntax color;
                    ;; the painter translates ANSI foreground codes to Lanterna.
                    (str/starts-with? line th-md-code-marker)
                    (do (p/set-colors! g t/code-result-fg t/code-block-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/styled
                          g
                          [p/ITALIC]
                          (paint-ansi-line! g x y (subs line 1) t/code-result-fg t/code-block-bg)))
                    (str/starts-with? line th-md-bullet-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/styled g
                                  [p/ITALIC]
                                  (p/paint-styled-line! g
                                                        x
                                                        y
                                                        (subs line 1)
                                                        t/dialog-hint
                                                        t/iteration-header-bg
                                                        t/code-result-fg
                                                        t/code-block-bg)))
                    (str/starts-with? line th-md-quote-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/styled g
                                  [p/ITALIC]
                                  (p/paint-styled-line! g
                                                        x
                                                        y
                                                        (subs line 1)
                                                        t/dialog-hint
                                                        t/iteration-header-bg
                                                        t/code-result-fg
                                                        t/code-block-bg)))
                    (str/starts-with? line th-md-hr-marker)
                    (do (p/set-colors! g t/answer-sep-fg t/iteration-header-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/put-str! g x y (subs line 1)))
                    ;; ── Markdown table (thinking) ── grid blends into thinking zone
                    ;; Same dual-color treatment as the answer-mode table,
                    ;; with italic on text segments to match the rest of
                    ;; the thinking zone (every thinking-mode marker uses
                    ;; italic; staying consistent keeps the zone readable
                    ;; as one cohesive dim region). Background is
                    ;; `iteration-header-bg` so the table merges into the
                    ;; surrounding thinking block instead of breaking it
                    ;; with a different shade.
                    (or (str/starts-with? line th-md-table-head-marker)
                        (str/starts-with? line th-md-table-sep-marker)
                        (str/starts-with? line th-md-table-row-marker))
                    (let [stripped (subs line 1)
                          head? (str/starts-with? line th-md-table-head-marker)
                          border? (str/starts-with? line th-md-table-sep-marker)
                          tbg t/iteration-header-bg]

                      (p/clear-styles! g)
                      (p/set-colors! g t/code-border-fg tbg)
                      (p/fill-rect! g fbx y iw 1)
                      (if border?
                        (p/put-str! g x y stripped)
                        (paint-table-data-line! g
                                                x
                                                y
                                                stripped
                                                t/code-result-fg
                                                t/code-border-fg
                                                tbg
                                                (cond head? [p/BOLD p/ITALIC]
                                                      :else [p/ITALIC]))))
                    ;; ── Legacy separator - dim ──
                    (str/starts-with? line sep-marker) (do (p/set-colors! g t/dialog-hint bg-color)
                                                           (p/put-str! g x y (subs line 1)))
                    ;; ── Answer text - answer bg ──. Plain final-answer paragraphs can still
                    ;; contain inline sentinels from IR spans, e.g. `[:c "/command"]` becomes
                    ;; INLINE_CODE_ON/OFF around the body text. Consume those
                    ;; here instead of writing raw PUA glyphs to the terminal.
                    (str/starts-with? line answer-txt-marker)
                    (do (p/set-colors! g t/answer-fg t/answer-bg)
                        (p/fill-rect! g fbx y iw 1)
                        (p/paint-styled-line! g
                                              x
                                              y
                                              (subs line 1)
                                              t/answer-fg
                                              t/answer-bg
                                              t/code-block-fg
                                              t/code-block-bg))
                    ;; ── Answer padding ──
                    (str/starts-with? line answer-pad-marker) (do (p/set-bg! g t/answer-bg)
                                                                  (p/fill-rect! g fbx y iw 1))
                    ;; ── Plain text - answer bg if in answer zone, else bubble bg ──
                    ;; Cancelled status messages render in muted italic on
                    ;; terminal bg (no fill) so the line reads as a system
                    ;; note, not as something the model said.
                    :else
                    ;; Cancelled bubbles never enter the answer zone - the
                    ;; status footer ("Cancelled by user.") renders flat on
                    ;; terminal-bg, no answer-bg fill underneath, even if a
                    ;; structural answer marker sits earlier in the trailer.
                    (let [in-answer-zone? (and in-answer? (not cancelled?))
                          line-bg (if in-answer-zone? t/answer-bg bg-color)
                          line-fg (if in-answer-zone? t/answer-fg fg-color)]

                      (when in-answer-zone? (p/set-bg! g line-bg) (p/fill-rect! g fbx y iw 1))
                      (p/set-colors! g line-fg line-bg)
                      (if (or cancelled? queued?)
                        ;; Cancelled / queued status messages: render as a
                        ;; muted italic block. Inline markdown spans are
                        ;; intentionally NOT honoured here - a system note
                        ;; saying "Cancelled by user" should stay flat,
                        ;; never bold.
                        (p/styled g [p/ITALIC] (p/put-str! g x y line))
                        ;; Plain assistant prose: walk the line, switch SGR
                        ;; on each inline sentinel pair. paint-styled-line!
                        ;; falls back to a single put-str! when no sentinels
                        ;; are present, so this is free for ASCII-only text.
                        (p/paint-styled-line! g
                                              x
                                              y
                                              line
                                              line-fg
                                              line-bg
                                              t/code-block-fg
                                              t/code-block-bg))
                      (paint-turn-stamp! g x y line line-bg)
                      (when (= :toggle-details (:kind meta))
                        (let [abs-row (+ (long viewport-top) (long y))
                              click-width (long (or (:click-width meta) iw))]

                          (cr/register! {:bounds {:row abs-row :col x :width click-width}
                                         :kind :toggle-details
                                         :session-id (:session-id meta)
                                         :node-id (:node-id meta)
                                         :collapsed? (:collapsed? meta)})))))
                  ;; Inline markdown links: register a `:url` click region per
                  ;; link span so a click hands the href to the OS opener (the
                  ;; MOVE/CLICK_DOWN mouse handler looks these up). `:col` is a
                  ;; body-relative offset; markers are zero-width so `x` is the
                  ;; first visible column of the body.
                  (when-let [links (:links meta)]
                    (let [abs-row (+ (long viewport-top) (long y))
                          hovered (cr/hovered)
                          hover-url? (= :url (:kind hovered))]

                      (doseq [{:keys [col width url]} links]
                        (let [abs-col (+ (long x) (long col))]
                          (cr/register! {:bounds {:row abs-row :col abs-col :width (long width)}
                                         :kind :url
                                         :url url})
                          ;; Hover affordance: when the pointer is over this
                          ;; link span, brighten its already-underlined cells
                          ;; to the link-chrome hover fg so the link lights up
                          ;; under the cursor. At rest the static underline
                          ;; (INLINE_LINK sentinels) still marks it as a link.
                          (when (and hover-url?
                                     (= abs-row (:row (:bounds hovered)))
                                     (= abs-col (:col (:bounds hovered))))
                            (dotimes [dc (long width)]
                              (p/underline-cell! g (+ abs-col dc) y t/link-chrome-hover-fg)))))))
                  (when user?
                    (p/clear-styles! g)
                    (p/set-colors! g role-fg bg-color)
                    (p/put-str! g bx (+ (long btop) (long i)) "│")))
                (recur (inc i))))))
        ;; Below-content footer row: optional right-aligned meta, with
        ;; one breathing row between answer body and footer.
        ;;
        ;; Final per-message layout (no outer box, no bg fill, no
        ;; horizontal rule under the label):
        ;;   row 0                    : label + timestamp
        ;;   row 1 ... N                : wrapped content (with marker-zone fills)
        ;;   row 1+N                  : bottom pad (user only)
        ;;   row 1+N+P                : blank footer margin, when meta present row 2+N+P
        ;;           : meta right, when present final row                : single blank gap
        ;;   before the next message
        (p/clear-styles! g)
        (let [footer?
              (some? meta-str)

              note?
              (and footer? (some? fallback-note))

              footer-gap
              (if footer? 1 0)

              footer-row
              (+ (long btop) (long bubble-h) (long bottom-pad) (long footer-gap))]

          (when footer?
            (p/clear-styles! g)
            (p/set-colors! g t/dialog-hint t/terminal-bg)
            (p/put-str! g
                        (+ (long bx) (max 0 (- (long bubble-w) (long (p/display-width meta-str)))))
                        footer-row
                        meta-str))
          ;; Faint italic routing sub-note, right-aligned under the main line.
          (when note?
            (p/clear-styles! g)
            (p/set-colors! g t/footer-fg-muted t/terminal-bg)
            (p/styled g
                      [p/ITALIC]
                      (p/put-str!
                        g
                        (+ (long bx)
                           (max 0 (- (long bubble-w) (long (p/display-width fallback-note)))))
                        (inc (long footer-row))
                        fallback-note))
            (p/clear-styles! g))
          ;; Return: rows consumed
          ;;   = label(1) + top-pad(user only) + content(N)
          ;;     + bottom-pad(user only)
          ;;     + footer-gap(meta only)(0|1)
          ;;     + footer(meta)(0|1) + fallback-note(0|1)
          ;;     + gap(1)
          (+ top-sep-h
             1
             top-pad
             bubble-h
             bottom-pad
             footer-gap
             (if footer? 1 0)
             (if note? 1 0)
             1))))))
(defn bubble-height*
  "Uncached calculation: rows a chat message will consume without drawing.
   label(1) + optional top-pad(1, user only) + wrapped-lines
   + optional bottom-pad(1, user only) + optional meta top margin/footer(0|2)
   + gap(1).
   Mirrors `draw-chat-bubble!`'s wrap width (`bubble-w - 2*h-pad`) so
   layout math stays consistent across the height calc and the draw."
  [{:keys [text role prewrapped-lines status] :as message} max-w]
  (let [bubble-w
        max-w

        top-sep-h
        0

        h-pad
        2

        content-w
        (max 1 (- (long bubble-w) (* 2 (long h-pad))))

        ;; Same contract: virtual.clj projection populates
        ;; `:prewrapped-lines` via the IR walker for every visible
        ;; bubble; `wrap-text` is the bare-string fallback. Route
        ;; through `clipped-lines` here too: height calculation happens
        ;; during pre-warm/layout, so it warms the exact clipped vector
        ;; draw-chat-bubble! will need while scrolling.
        raw-lines
        (or prewrapped-lines (wrap-text text content-w))

        lines
        (clipped-lines raw-lines content-w bubble-w)

        top-pad
        (if (= role :user) 1 0)

        bottom-pad
        (if (= role :user) 1 0)

        cancelled?
        (= :cancelled status)

        meta-str
        (when (and (not= role :user) (not cancelled?)) (vis/meta-summary-line message))

        fallback-note
        (when (and meta-str (not= role :user) (not cancelled?)) (vis/meta-fallback-note message))

        footer?
        (some? meta-str)

        note?
        (and footer? (some? fallback-note))

        footer-gap
        (if footer? 1 0)]

    (+ top-sep-h 1 top-pad (count lines) bottom-pad footer-gap (if footer? 1 0) (if note? 1 0) 1)))
(defn bubble-height
  "Memoized `bubble-height*`. Keyed by projected line identity when
   available; live progress keeps stable prewrapped body lines and only
   appends a cheap spinner row. Metadata that can add/remove the
   assistant footer is part of the key; otherwise a no-usage render can
   stale-cache the shorter height before usage arrives."
  [{:keys [text role prewrapped-lines turn-separator? iteration-count duration-ms tokens cost status
           llm-selected llm-actual llm-fallback? llm-routing-trace]
    :as message} max-w]
  (cached* [::bh (System/identityHashCode text) (System/identityHashCode prewrapped-lines) role
            (boolean turn-separator?) iteration-count duration-ms tokens cost status llm-selected
            llm-actual llm-fallback? llm-routing-trace (long max-w)]
           #(bubble-height* message max-w)))
(defn total-messages-height
  "Calculate total row height for a vec of structured messages."
  [messages max-w]
  (reduce + 0 (map #(bubble-height % max-w) messages)))
;;; ── Progress timeline formatting ───────────────────────────────────────────
(defn- label-text
  "Format a label string: UPPERCASED text + plain number.
   e.g. (label-text \"iteration\" 1) => \"ITERATION 1\""
  ([s] (str/upper-case (str s)))
  ([s n] (str (str/upper-case (str s)) " " n)))
(defn- error-trace-headline
  [error]
  (when-let [trace (:trace error)]
    (some-> trace
            str
            str/split-lines
            first
            str/trim
            not-empty)))
(defn- error-detail-text
  "A non-empty, INFORMATIVE one-line error string — NEVER a content-free
   \"unknown error\". Prefers `:message`, then the first `:trace` line, then
   `:type`; and when none of those exist it surfaces whatever the error map DOES
   carry (`:data`, or its remaining keys) so a dropped `:message` can't hide the
   real failure behind \"we don't know nothing\". Only a genuinely empty error
   says so plainly — and that is itself a bug worth seeing."
  [error]
  (or (not-empty (str (:message error)))
      (error-trace-headline error)
      (not-empty (some-> (:type error)
                         str))
      (when-let [detail (or (not-empty (:data error))
                            (and (map? error) (not-empty (dissoc error :message :type :trace))))]
        (str "error: " (pr-str detail)))
      (when (and error (not (map? error))) (not-empty (str error)))
      "error: the engine produced no message (please report — this is a bug)"))
(defn- form-error-headline [error] (error-detail-text error))
(defn- inline-error-context-lines
  "Babashka-style code context for form eval errors. Kept inside the
   code band so failing source, caret, error message, and status occupy
   one visual block instead of code + error + repeated context blocks."
  [code-text error]
  (let [block
        (:block error)

        source
        (or (:source block) code-text)

        opened
        (:opened-loc block)

        arrow-row
        (or (:row opened) (:row block))

        arrow-col
        (or (:col opened) (:col block))]

    (when (and (string? source) (not (str/blank? source)))
      (let [lines
            (vec (str/split source #"\n" -1))

            total
            (count lines)

            fmt-line
            (fn [idx0]
              (nth lines idx0))

            arrow-line
            (when (and arrow-row arrow-col (<= 1 arrow-row total))
              (str (apply str (repeat (max 0 (dec (long arrow-col))) \space)) "^---"))

            arrow-idx0
            (when arrow-line (dec (long arrow-row)))]

        (vec (mapcat (fn [idx0]
                       (cond-> [(fmt-line idx0)]
                         (= idx0 arrow-idx0)
                         (conj arrow-line)))
                     (range total)))))))
(defn- form-error-only-iteration?
  "True when an iteration carries no iter-level `:error` but its forms
   reduce to a single zero-code form that errored. Provider transport
   failures (`:svar.core/stream-truncated`, 5xx mid-stream) land in
   this shape: a placeholder form with `:code \"\"` and the error map
   attached — no real reasoning, no real code execution."
  [entry]
  (let [forms (:forms entry)]
    (and (not (map? (:error entry)))
         (str/blank? (str (:thinking entry)))
         (= 1 (count forms))
         (let [f (first forms)]
           (and (str/blank? (str (:code f))) (map? (:error f)) (= :error (:result-kind f)))))))
(defn- form-error-only-error
  "Pull the placeholder form's `:error` map out of a form-error-only
   iteration. Returns nil when the iteration is not in that shape."
  [entry]
  (when (form-error-only-iteration? entry)
    (-> entry
        :forms
        first
        :error)))
(defn- error-map-signature
  [err]
  (when (map? err) [(:type err) (:message err) (get-in err [:data :raw-data])]))
(defn error-signature
  "Stable comparison key for two trace `:error` maps. Returns nil for
   non-error iterations (so they never collapse).

   Iter-level `:error` is the canonical surface; iterations whose
   only content is a zero-code form carrying the transport error
   (the provider-truncated-stream shape) hash with the same key so
   they collapse alongside true iter-level errors.

   Public so the virtual-layout pre-pass can squash runs of
   error-only assistant messages across turns using the same key
   the iteration-level collapser uses."
  [entry]
  (when-let [err (or (:error entry) (form-error-only-error entry))]
    (error-map-signature err)))
(defn- inline-form-error-signatures
  [forms]
  (->> forms
       (keep (fn [{:keys [code error]}]
               (when (and (map? error) (not (str/blank? (str code)))) (error-map-signature error))))
       set))
(defn- inline-rendered-form-error?
  [forms error]
  (contains? (inline-form-error-signatures forms) (error-map-signature error)))
(def ^:private reasoning-auto-collapse-line-threshold
  "Reasoning PREVIEW height. Up to this many rows of reasoning are
   ALWAYS shown — short reasoning (≤ this many rows) renders inline with
   no disclosure; longer reasoning shows these first rows as a peek and
   collapses only the REMAINDER behind a ▸ THINKING `+N more` toggle
   (same affordance as tool op rows). The opening of the reasoning
   (usually the plan) stays visible without the full wall of text."
  vis/reasoning-preview-line-limit)
(defn- text-fingerprint
  "Bounded structural fingerprint for a string. Survives `(vec ...)` /
   `assoc` round-trips that would change `identityHashCode` but leave
   content identical, so per-iteration caches actually hit during
   streaming. Collision probability for our render-cache use is
   negligible (length + first 64 + last 256 chars uniquely identifies
   the bubble's iteration content in practice); a collision would
   produce a stale render frame, not a crash, and would self-heal
   on the next chunk that bumps the length."
  [s]
  (when s
    (let [^String s
          (str s)

          n
          (.length s)]

      [n (subs s 0 (min 64 n)) (subs s (max 0 (- n 256)))])))
(defn- visible-iteration-entry
  "Filter `:forms` down to visibly-running/completed entries. When
   `show-silent?` is true the entry passes through unchanged; otherwise
   `:silent?` slots drop out."
  [entry show-silent?]
  (if show-silent?
    entry
    (update entry
            :forms
            (fn [forms]
              (vec (remove :silent? (or forms [])))))))
(defn- form-fingerprint
  "Content-derived fingerprint of one form map. Captures every field
   the iteration renderer reads."
  [{:keys [code comment render-segments result-render result-summary result-kind result-detail error
           success? silent? tool-color-role cards]
    tool-name :vis/tool-name}]
  [(text-fingerprint code) (text-fingerprint comment) render-segments
   (text-fingerprint result-render) (text-fingerprint result-summary) result-kind
   ;; result-detail is a small op-metadata map; compared structurally.
   result-detail error success? silent?
   ;; The native-tool BADGE identity the renderer paints (label + color); without
   ;; these in the key, a form that gains them renders from a STALE cache entry.
   tool-name tool-color-role
   ;; Print-many cards: a CHEAP per-card digest (name/colour/summary + a body
   ;; fingerprint, never the raw 20KB body) so two card-forms with the same code +
   ;; summary but different cards can't collide on a stale cache entry.
   (mapv (fn [c]
           [(:vis/tool-name c) (:tool-color-role c) (text-fingerprint (:result-summary c))
            (text-fingerprint (:result-render c))])
         cards)])
(defn- iteration-fingerprint
  "Content-derived fingerprint of an iteration entry. Captures every
   field `format-iteration-entry-entries` reads. No `identityHashCode`
   anywhere — identical content always produces identical fingerprint,
   so completed iterations hit the cache forever even when the parent
   `:iterations` vec is rebuilt by `(vec (vals @timeline))` on every
   progress chunk."
  [{:keys [thinking forms recaps provider-fallbacks error repeat-count]}]
  [(text-fingerprint thinking) (mapv form-fingerprint forms) recaps provider-fallbacks
   ;; `:error` is USUALLY a map, but some paths (e.g. CONSULT failures) carry a
   ;; plain String. `select-keys` only works on associatives and throws on a
   ;; String — which crashed the render thread every frame. Guard it
   ;; like the sibling `error-map-signature` does, keeping non-map errors in the
   ;; fingerprint (as their string) so cache invalidation still tracks them.
   (cond (map? error) (select-keys error [:type :message])
         (some? error) (str error)
         :else nil) repeat-count])
(defn- short-id-fragment
  ^String [id]
  (let [s (str (or id ""))]
    (subs s 0 (min 8 (count s)))))
(defn- detail-expanded?
  ([detail-expansions session-id node-id]
   (detail-expanded? detail-expansions session-id node-id true))
  ([detail-expansions session-id node-id default-expanded?]
   ;; `:vis.channel-tui/expand-all-details?` is the copy-only FORCE flag (set on a
   ;; throwaway map when rendering a fully-expanded body for copy) — it wins over
   ;; everything. Otherwise a per-node override (mouse click / label toggle) wins,
   ;; then the bulk `:baseline` (C-x c collapse-all / C-x e expand-all), then the
   ;; row's own default. This ordering lets a bulk op and later per-item toggles
   ;; compose instead of the global flag swallowing the click.
   (if (:vis.channel-tui/expand-all-details? detail-expansions)
     true
     (let [k [(str session-id) (str node-id)]]
       (if (contains? detail-expansions k)
         (boolean (get detail-expansions k))
         (case (:vis.channel-tui/baseline detail-expansions)
           :expand
           true

           :collapse
           false

           (boolean default-expanded?)))))))
(defn- detail-id-suffix
  ;; User-facing badge displayed at the right edge of disclosure rows.
  ;;   - Render positions (ints), never UUIDs.
  ;;   - Format: `[turn 7 · iteration 3 · block 0 · tool · patch]`
  ;;   - Lowercase level words, dot separator (·).
  ;;   - Optional :role and :op-symbol segments after the positions.
  ;;   - No abbreviations: "iteration" not "iter".
  ^String [{:keys [turn-position iteration-number block-number role op-symbol details-path]}]
  (let [parts (cond-> []
                (some? turn-position)
                (conj (str "turn " turn-position))

                iteration-number
                (conj (str "iteration " iteration-number))

                block-number
                (conj (str "block " block-number))

                role
                (conj (name role))

                op-symbol
                (conj (str op-symbol))

                (seq details-path)
                (conj (str "details " (str/join "." details-path))))]
    ;; No bare `[details]` decoration — the chevron already signals the toggle.
    ;; Only the informative `[turn 7 · iteration 3 · …]` form is kept, when present.
    (if (seq parts) (str "[" (str/join " · " parts) "]") "")))
(defn- truncate-with-suffix
  "Truncate `s` so that `s` + `suffix` together fit within `max-w` display
   columns, then append `suffix`. The result NEVER exceeds `max-w`, so the
   painter won't re-wrap the marker onto its own line. Unlike `ellipsize-cols`
   (which adds a marker only WHEN `s` overflows), `suffix` is ALWAYS appended —
   use this for \"there's more\" markers such as \" …\".

   Truncation is ANSI-aware via `truncate-ansi-cols`: a collapsed thinking-band
   peek row can be a syntax-highlighted code line carrying `\\u001b[..m` SGR
   runs, and `p/truncate-cols` would strip the ESC bytes (leaving literal
   `[36m`/`[0m` text and dropping the colours). `truncate-ansi-cols` keeps SGR
   zero-width, so the row keeps its colour and the visible body still fits."
  ^{:tag String} [s suffix max-w]
  (let [mw
        (max 0 (long (or max-w 0)))

        room
        (max 0 (- mw (p/display-width suffix)))]

    (str (truncate-ansi-cols (str/trimr (str s)) room) suffix)))
(defn- ellipsize-cols
  ^{:tag String} [s max-w]
  (cond (<= (long max-w) 0) ""
        (<= (p/display-width s) (long max-w)) s
        (= max-w 1) "..."
        :else (truncate-with-suffix s "..." max-w)))
(defn- close-dangling-code-span
  "After a raw-markdown summary is ellipsized to fit a collapsed badge, its
   trailing inline-code path chip (`` `…/file.clj` ``) can lose its CLOSING
   backtick to the truncation — leaving `` `src/…/foo... `` unmatched. The IR
   walker then treats the whole span as literal text and the path chip's `` 
   background is silently dropped. Re-close an odd trailing backtick so a
   truncated path keeps its code-chip styling (the `` `…` `` markup chars are
   zero-width once rendered, so this doesn't push past the width budget)."
  ^String [^String s]
  (if (odd? (count (filterv #(= \` %) s))) (str s "`") s))
(defn- format-detail-summary-line
  "Put the human-readable detail info on the right edge. The
   whole row is already painted as a bold disclosure band by
   `draw-chat-bubble!`, so keep the suffix as plain text. Inline code
   would drop that inherited bold style and switch to a weaker code
   background."
  ^String [left suffix max-w]
  (let [suffix-w
        (p/display-width suffix)

        gap-w
        2]

    (if (> (+ (long suffix-w) (long gap-w) 1) (long max-w))
      (str left " / " suffix)
      (let [left-w
            (max 1 (- (long max-w) (long suffix-w) (long gap-w)))

            left
            (close-dangling-code-span (ellipsize-cols left left-w))

            pad-w
            (max (long gap-w) (- (long max-w) (long (p/display-width left)) (long suffix-w)))]

        (str left (repeat-str \space pad-w) suffix)))))
(defn- detail-node-base-id
  ^String [{:keys [session-turn-id iteration-number block-number section kind]}]
  (str (or (some-> section
                   name)
           "answer")
       (when session-turn-id (str ":t" (short-id-fragment session-turn-id)))
       (when iteration-number (str ":i" iteration-number))
       (when block-number (str ":b" block-number))
       (when kind (str ":" (name kind)))))
(defn- detail-node-id
  ^String [{:keys [details-path] :as detail-ctx}]
  (str (detail-node-base-id detail-ctx)
       (when (seq details-path) (str ":d" (str/join "." details-path)))))
(defn- relevant-detail-expansions-key
  "Stable cache key for only the disclosure nodes this Markdown projection
   can render. A click in one old answer must not bust cached projections for
   every other visible answer. Scope by session + node-id base.

   Like `turn-detail-expansions-key`, the bulk fold state lives under KEYWORD
   keys (`:vis.channel-tui/baseline`, `:vis.channel-tui/expand-all-details?`)
   that the per-node `keep` drops — so it is folded in explicitly here too, else
   a bulk collapse/expand-all leaves this bubble on its stale cached render."
  [opts]
  (let [detail-expansions
        (:detail-expansions opts)

        session-id
        (some-> (:session-id opts)
                str)

        base
        (detail-node-base-id opts)

        prefix
        (str base ":")

        per-node
        (->> detail-expansions
             (keep (fn [[k expanded?]]
                     (when (vector? k)
                       (let [[cid node-id]
                             k

                             node-id
                             (str node-id)]

                         (when (and (= session-id (str cid))
                                    (or (= base node-id) (str/starts-with? node-id prefix)))
                           [node-id expanded?])))))
             sort
             vec)]

    (cond-> per-node
      (:vis.channel-tui/baseline detail-expansions)
      (conj [:vis.channel-tui/baseline (:vis.channel-tui/baseline detail-expansions)])

      (:vis.channel-tui/expand-all-details? detail-expansions)
      (conj [:vis.channel-tui/expand-all-details? true]))))
(defn- turn-detail-expansions-key
  "Stable cache key for any disclosure belonging to this rendered assistant
   turn. Used by the outer trace+answer projection, which may contain thinking,
   iteration, tool/result, and final-answer disclosures.

   The bulk fold state — `:vis.channel-tui/baseline` (C-x [ collapse-all /
   C-x ] expand-all) and the copy-only `:vis.channel-tui/expand-all-details?`
   FORCE flag — lives under KEYWORD keys, not `[cid nid]` vectors, so it is
   invisible to the per-node `keep` below. It MUST be folded into the key:
   without it a bulk collapse/expand returns the previously-cached projection
   and the transcript never repaints until a per-node click finally changes a
   vector key (the \"C-x ] does nothing until I click something\" bug)."
  [opts]
  (let [detail-expansions
        (:detail-expansions opts)

        session-id
        (some-> (:session-id opts)
                str)

        turn-fragment
        (some-> (:session-turn-id opts)
                short-id-fragment)

        turn-token
        (when turn-fragment (str ":t" turn-fragment))

        per-node
        (->> detail-expansions
             (keep (fn [[k expanded?]]
                     (when (vector? k)
                       (let [[cid node-id]
                             k

                             node-id
                             (str node-id)]

                         (when (and (= session-id (str cid))
                                    (or (nil? turn-token) (str/includes? node-id turn-token)))
                           [node-id expanded?])))))
             sort
             vec)]

    (cond-> per-node
      (:vis.channel-tui/baseline detail-expansions)
      (conj [:vis.channel-tui/baseline (:vis.channel-tui/baseline detail-expansions)])

      (:vis.channel-tui/expand-all-details? detail-expansions)
      (conj [:vis.channel-tui/expand-all-details? true]))))
(defn message-detail-expansions-key
  "Per-message disclosure-expansion fingerprint for the height cache: the
   subset of `detail-expansions` whose disclosure node-ids belong to
   `message`'s turn (matched by the `:t<short-id>` token every node-id
   carries), or `:expand-all` when the global expand flag is set. A bulk
   `:baseline` (C-x [ collapse-all / C-x ] expand-all) is prepended so a bulk
   op busts every message's cached height; with no bulk active the key keeps
   its original shape so unrelated fold clicks don't churn the cache. Lets the
   height cache key a message's height to its OWN expand/collapse state
   without busting every other message's cached height — the SAME per-turn
   scoping this file already uses for its projection cache."
  [session-id message detail-expansions]
  (cond (:vis.channel-tui/expand-all-details? detail-expansions) :expand-all
        :else (let [baseline
                    (:vis.channel-tui/baseline detail-expansions)

                    per-turn
                    (if (and (not= :assistant (:role message))
                             (nil? (:client-turn-id message))
                             (nil? (:session-turn-id message)))
                      ;; A bubble with no turn id can't be turn-scoped AND carries no
                      ;; disclosures (only user prompts with a `[Pasted #N]` marker do,
                      ;; and those always land with a `:client-turn-id`) — keep the
                      ;; cheap constant key so an unrelated fold click never busts its
                      ;; cached height.
                      []
                      (turn-detail-expansions-key {:session-id session-id
                                                   :session-turn-id (or (:client-turn-id message)
                                                                        (:session-turn-id message))
                                                   :detail-expansions detail-expansions}))]

                ;; Fold the bulk baseline in ONLY when a bulk op is active, so the no-bulk
                ;; key shape (and every already-warmed cache entry) is unchanged.
                (if baseline (into [baseline] per-turn) per-turn))))
(defn- detail-summary-entries
  [{:keys [marker max-w summary collapsed? session-id node-id color-role] :as detail-ctx}]
  (let [suffix
        (detail-id-suffix detail-ctx)

        summary
        (or summary "Details")

        left
        (str " " (if collapsed? "▸ " "▾ ") summary)

        visible
        (format-detail-summary-line left suffix (max 1 (long max-w)))

        ;; Lift visible-label string through the IR walker so inline
        ;; emphasis (`**bold**`, `` `code` ``, etc.) renders with
        ;; sentinel-wrapped runs the painter understands; legacy
        ;; `markdown->inline` regex parser is gone.
        wrapped
        (wrap-text (ir-tui/ir->inline-sentinel-string (vis/markdown->ir visible))
                   (max 1 (long max-w)))

        meta
        {:kind :toggle-details
         :session-id (str session-id)
         :node-id (str node-id)
         :collapsed? collapsed?
         :color-role color-role}]

    (mapv (fn [line]
            {:line (str marker line) :meta meta})
          wrapped)))

(defn- tag-copy-block-body
  "Stamp every body row of an expanded disclosure with copy metadata so a
   single click on the body lines copies the WHOLE disclosure body, not
   the entire enclosing assistant message. Rows that already carry meta
   (nested toggle-details, links, ...) keep theirs - those have their
   own click handling and must not be hijacked."
  [entries node-id text]
  (if (or (nil? node-id) (str/blank? (str text)))
    entries
    (let [body-meta {:kind :copy-block-body :node-id (str node-id) :text (str text)}]
      (mapv (fn [{:keys [meta] :as e}]
              (if meta e (assoc e :meta body-meta)))
            entries))))
(defn- marker-prefix?
  "True when the first char of `line` is a block paint marker the
   renderer prepends (zero-width format codepoints or PUA marker band
   U+E000..U+E0FF). Inline style sentinels start at U+E110, so badge
   labels beginning with bold/code sentinels stay intact."
  ^Boolean [^String line]
  (and (string? line)
       (pos? (count line))
       (let [c
             (.charAt line 0)

             i
             (int c)]

         (or (= (int Character/FORMAT) (int (Character/getType c)))
             (and (>= i 0xE000) (<= i 0xE0FF))))))
(def ^:private chrome-meta-kinds
  ;; Row kinds that paint display-only chrome (`▾ SUMMARY [Turn: ...]`).
  ;; Skipped when reconstructing the user-facing body text so nested
  ;; disclosure copy doesn't drag the visual summary glyph + details
  ;; suffix into the clipboard.
  #{:toggle-details})
(defn- entries->body-text
  "Reconstruct the user-readable body text from a vec of `{:line :meta}`
   entries. Strips the leading paint marker (one zero-width / format
   codepoint) ONLY when present; plain answer-markdown rows have no
   prefix and stay intact. Skips chrome rows (disclosure summaries) so
   a nested-details copy carries body text only - not the toggle glyph.
   Used as the copy payload for disclosure body rows."
  [entries]
  (->> entries
       (remove (fn [{:keys [meta]}]
                 (contains? chrome-meta-kinds (:kind meta))))
       (map (fn [{:keys [line]}]
              (cond (not (string? line)) ""
                    (zero? (count line)) ""
                    (marker-prefix? line) (subs line 1)
                    :else line)))
       (str/join "\n")
       str/trim))

(defn- thinking-padded-block
  "Wrap reasoning content rows in the thinking-bg top/bottom padding the
   bubble painter expects (neutral blank above, thinking pad row inside)."
  [content-entries]
  (let [line-entry (fn [l]
                     {:line l :meta nil})]
    (vec (concat [(line-entry "") (line-entry (str thinking-marker ""))]
                 content-entries
                 [(line-entry (str thinking-marker ""))]))))
(defn- maybe-collapse-thinking-entries
  "Render reasoning INSIDE the bubble's dim thinking band, headed by a
   `THINKING` label that lives in the SAME band (not a detached op-row),
   so the label + reasoning read as one cohesive thinking bubble.

   Short reasoning (≤ `reasoning-auto-collapse-line-threshold` rows)
   paints in full with no disclosure. Longer reasoning shows the
   clickable header `▸ THINKING  +N more` as the band's TOP line and
   PEEKS the first N rows below it; clicking expands in place to the
   full reasoning (`▾ THINKING`). Standard accordion: chevron at the
   header, content below.

   The header carries `:toggle-details` meta on a `thinking-marker`
   row — the painter registers the click region for that marker so the
   dim header is itself the hit target. The node id is turn-scoped
   (`thinking:t<frag>:i<N>:reasoning`) so it matches
   `turn-detail-expansions-key`; a toggle busts the live per-iteration
   render cache, so it collapses/expands mid-turn. Default COLLAPSED.
   No `session-id` (e.g. synthetic previews) → full inline, no header.

   Returns the COMPLETE thinking band, used verbatim by the caller."
  [{:keys [entries session-id detail-expansions session-turn-id iteration-number max-w]}]
  (let [entries (vec entries)]
    (if (or (nil? session-id)
            (empty? entries)
            ;; Don't collapse a trace whose hidden remainder is tiny: a toggle
            ;; that reveals fewer than `reasoning-collapse-min-hidden` extra rows
            ;; is pure friction (uncollapse just to see one more line). Render
            ;; those inline in full.
            (< (- (count entries) (long reasoning-auto-collapse-line-threshold))
               (long vis/reasoning-collapse-min-hidden)))
      (thinking-padded-block entries)
      (let [detail-ctx {:session-id session-id
                        :session-turn-id session-turn-id
                        :iteration-number iteration-number
                        :details-path nil
                        :section :thinking
                        :kind :reasoning}
            node-id (detail-node-id detail-ctx)
            expanded? (detail-expanded? detail-expansions session-id node-id false)
            ;; Accordion header at the TOP of the band: ▸ collapsed,
            ;; ▾ expanded (content reveals below the header).
            chevron (if expanded? "▾" "▸")
            ;; Full reasoning text is the copy payload for BOTH states
            ;; (a peek still copies everything the model reasoned).
            full-copy (entries->body-text entries)
            ;; Collapsed shows the first-N PEEK; expanded shows all.
            preview-n reasoning-auto-collapse-line-threshold
            hidden-n (max 0 (- (count entries) (long preview-n)))
            shown (if expanded? entries (vec (take preview-n entries)))
            label
            (if (or expanded? (zero? hidden-n)) "THINKING" (str "THINKING  +" hidden-n " more"))
            ;; Header is a THINKING-MARKER row → painted in the dim
            ;; band (so it sits INSIDE the bubble), and carries the
            ;; toggle-details meta the thinking-marker painter now
            ;; registers as a click region.
            header {:line (str thinking-marker
                               (ellipsize-cols (str chevron " " label) (max 1 (long (or max-w 1)))))
                    :meta {:kind :toggle-details
                           :session-id (str session-id)
                           :node-id (str node-id)
                           :collapsed? (not expanded?)
                           :color-role nil}}]

        ;; One neutral blank above, then the dim band: top edge, the
        ;; THINKING header, reasoning (peek or full), bottom edge — all one thinking bubble.
        (vec
          (concat
            [{:line "" :meta nil} {:line (str thinking-marker "") :meta nil} header
             ;; One blank band row between the THINKING badge and the
             ;; reasoning body — a margin INSIDE the dim band so the
             ;; label doesn't sit flush against the first reasoning line.
             {:line (str thinking-marker "") :meta nil}]
            ;; Collapsed: append a dim " …" to the LAST peeked line
            ;; ITSELF — right where the reasoning is trimmed — so the
            ;; bottom edge signals "there's more — click the THINKING
            ;; chevron". One space then …, in-place; NOT a separate
            ;; ellipsis row on its own line. The trimmed line keeps the
            ;; header's toggle-details meta so the painter registers it
            ;; as the SAME hit target. Expanded shows every row → none.
            (let [body (vec (tag-copy-block-body shown node-id full-copy))
                  ;; A row is visually blank once its leading structural
                  ;; paint marker is stripped: thinking rows are prefixed
                  ;; with the zero-width thinking marker (`​`), which
                  ;; `str/blank?` does NOT count as whitespace, so the raw
                  ;; line always reads non-blank. Strip the marker first.
                  blank-row? (fn [row]
                               (let [line (:line row)
                                     [_ rest] (split-structural-line-marker line)]

                                 (str/blank? (or rest line))))]

              (if (and (not expanded?) (pos? hidden-n) (seq body))
                ;; Drop trailing visually-blank peek rows so " …" lands on
                ;; the last row that actually shows reasoning — not on a
                ;; paragraph separator (whose only glyph is the invisible
                ;; thinking marker), which would make " …" appear to float
                ;; on its own line.
                (let [trimmed (loop [b body]
                                (if (and (> (count b) 1) (blank-row? (peek b))) (recur (pop b)) b))
                      last-i (dec (count trimmed))]

                  (-> trimmed
                      (assoc-in [last-i :line]
                                ;; Append the dim " …" marker, width-clamped via
                                ;; truncate-with-suffix so it stays on the SAME row.
                                (truncate-with-suffix (:line (nth trimmed last-i)) " …" max-w))
                      (assoc-in [last-i :meta] (or (:meta (nth trimmed last-i)) (:meta header)))))
                body))
            [{:line (str thinking-marker "") :meta nil}]))))))

(defn- strip-paint-markers-line
  "Return user-visible text for a prewrapped internal painter line.
   The TUI painter consumes these markers from `:lines`; `:text` is
   used by copy/debug/projection paths and must never expose PUA or
   bidi-control glyphs to the user."
  [line]
  (let [s
        (str (or line ""))

        ;; Strip ANSI SGR sequences (diff fences carry `\u001b[..m` colour codes
        ;; the painter translates) so copied / projected text is clean.
        s
        (str/replace s #"\u001b\[[0-9;]*m" "")

        s
        (if (and (pos? (count s))
                 (let [c (.charAt ^String s 0)]
                   (or (= (int Character/FORMAT) (int (Character/getType c)))
                       (and (>= (int c) 0xE000) (<= (int c) 0xE0FF)))))
          (subs s 1)
          s)

        ;; Drop the inline paint sentinels (PUA E110..E2FF) in ONE StringBuilder
        ;; pass instead of a lazy char seq + apply-str — this runs per projected
        ;; line and showed up as a render/restore hotspot.
        ^String s
        s

        n
        (.length s)

        sb
        (StringBuilder. n)]

    (dotimes [i n]
      (let [c (.charAt s i)]
        (when-not (<= 0xE110 (int c) 0xE2FF) (.append sb c))))
    (.toString sb)))
(defn- entries->payload
  [entries]
  (let [lines
        (mapv :line entries)

        line-meta
        (mapv :meta entries)]

    {:lines lines :line-meta line-meta :text (str/join "\n" (map strip-paint-markers-line lines))}))
(declare paste-aware-ir->entries)
;;; ── Inline markdown tokenizer (mid-line bold / italic / strike / code) ──
;;
;; `markdown->inline` is forward-declared once at the top of the
;; file (search `(declare markdown->inline)`), so no second declare
;; here.
;; ── Markdown link / image pre-pass ────────────────────────────────────────
;;
;; Hand-rolled scanner that mirrors the regex
;;
;;     #"(!)?\[([^\]]*?)\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)"
;;
;; - `[text](url)` becomes `text`, `![alt](url)` becomes `""`. Used to
;; live as `(str/replace s #"..." (fn [m] ...))` inside `markdown->inline`.
;; That allocated a Pattern call, a Matcher, and a per-match callback
;; closure on every line of every assistant bubble on every redraw,
;; which dominated cold-open cost on long sessions (multi-MB of
;; trace lines, dozens of `[path](path)` links per answer).
;;
(defn- collapse-repeated-error-runs
  "Walk an iterations vec; collapse runs of consecutive iterations that
   share the same error signature into one rendered block carrying a
   `:repeat-count`. Non-error iterations pass through unchanged with
   `:repeat-count 1`.

   Returns a vec of `[orig-idx entry]` pairs (mirroring the input order
   of `map-indexed`) so callers can keep using `(inc orig-idx)` as the
   visible iteration number while skipping the duplicates.

   Form-error-only iterations (zero-code placeholder form carrying the
   transport error) are lifted to iter-level `:error` on the merged
   entry and have their forms dropped so the `error-lines` painter
   produces a single `ERROR x N` row instead of N separate form
   error rows."
  [iterations]
  (loop [acc
         []

         i
         0

         remaining
         (vec iterations)]

    (if (empty? remaining)
      acc
      (let [head
            (first remaining)

            sig
            (error-signature head)

            run
            (if (nil? sig) 1 (count (take-while #(= sig (error-signature %)) remaining)))

            entry
            (cond-> head
              (form-error-only-iteration? head)
              (-> (assoc :error (form-error-only-error head))
                  (assoc :forms []))

              true
              (assoc :repeat-count run))]

        (recur (conj acc [i entry]) (+ (long i) (long run)) (subvec remaining run))))))

(defn- section-label-entry?
  "A body row that is a whole-line **LABEL** heading (COMMAND / STATUS / STDOUT /
   RESULT …): after its structural marker + indent the first visible glyph is the
   bold sentinel. Used to reinstate exactly ONE separator row before each section."
  [entry]
  (let [line
        (str (:line entry))

        body
        (or (second (split-structural-line-marker line)) line)]

    (str/starts-with? (str/replace body #"^\s+" "") p/INLINE_BOLD_ON)))

(defn- compact-tool-card-body-entries
  "Reflow a native op-card body (REPL/GIT/SHELL sections) for the TUI. The shared
   markdown->entries pass emits DOUBLED blank rows around every code fence, so the
   labelled sections collapse into one un-separated wall. Drop that fence padding,
   then reinstate exactly ONE blank row before each **LABEL** section — gluing each
   label to its own content — plus ONE trailing pad row so the card breathes."
  [entries]
  (let [blank?
        #(str/blank? (strip-paint-markers-line (:line %)))

        pad
        (or (first (filter blank? entries)) {:line (str result-marker "") :meta nil})

        content
        (into [] (remove blank?) entries)]

    (if (empty? content)
      []
      (-> (reduce (fn [acc e]
                    (if (and (seq acc) (section-label-entry? e)) (conj acc pad e) (conj acc e)))
                  []
                  content)
          (conj pad)))))

(defn- tool-card-entries
  "Render ONE op-card (`vis/result-card` descriptor) into TUI line entries: the tool
   LABEL + tool-authored SUMMARY on a headline painted in the tool's colour, the
   markdown body nested UNDER it (collapsible via `node-id`). A summary-only card
   (no body) is a single painted headline row with no expand triangle. The per-card
   renderer shared by a single native-tool form AND each card of a print-many block,
   so every op-card paints identically however many results one form carries."
  [{:keys [label color-role summary body]}
   {:keys [fill-w session-id detail-expansions node-id] :as opts}]
  (let [body-text
        (some-> body
                str
                str/trimr
                not-empty)

        ;; Op-card sections are compact: keep exactly one spacer row after the
        ;; headline so expanded cards breathe, but drop markdown/code-fence pad rows
        ;; inside COMMAND / RESULT / STDOUT sections. The labels themselves provide
        ;; the visual structure after that first separator.
        head-line
        (str (when (seq (str label)) (str "**" label "**")) (when summary (str "  " summary)))

        ->result
        (fn [e]
          (let [l
                (str (:line e))

                stripped
                (or (second (split-structural-line-marker l)) l)]

            (assoc e
              :line (str result-marker
                         (if (str/blank? stripped) stripped (str tool-output-indent stripped))))))

        ir
        (some-> body-text
                vis/markdown->ir)

        ;; A `vis-image` result must NEVER start life collapsed: the whole
        ;; point is to SEE the picture. Detect one in the body so the op-card
        ;; defaults to expanded (and force the inner image disclosure open).
        has-image?
        (boolean (some (fn [n]
                         (and (vector? n) (= :code (first n)) (= "vis-image" (:lang (second n)))))
                       (some-> ir
                               (nthrest 2))))

        entries
        (when ir
          (tag-copy-block-body (vec (paste-aware-ir->entries
                                      ir
                                      (max 1 (- (long fill-w) (long tool-output-indent-cols)))
                                      (assoc opts
                                        :mode :channel
                                        :image-default-expanded? true)))
                               node-id
                               body-text))]

    (if (and node-id (seq entries))
      (let [expanded?
            (detail-expanded? detail-expansions session-id node-id has-image?)

            body-entries
            (compact-tool-card-body-entries (mapv ->result entries))

            header
            (detail-summary-entries {:marker result-marker
                                     :max-w fill-w
                                     :summary head-line
                                     :hidden-entries body-entries
                                     :collapsed? (not expanded?)
                                     :session-id session-id
                                     :node-id node-id
                                     :color-role color-role})]

        ;; Collapsed op-card gets ONE trailing `result-bg` pad row so the
        ;; badge reads as its own background BAND (not a lone colored line).
        ;; Expanded cards keep ONE separator row after the headline — otherwise a
        ;; nil-result REPL with only STDOUT visually glues the first label to the
        ;; `▾ REPL ...` row — but no decorative gutters inside the labeled body.
        (vec (concat header
                     (if expanded?
                       (into [{:line (str result-marker "") :meta nil}] body-entries)
                       [{:line (str result-marker "") :meta nil}]))))
      (let [meta
            {:kind :result-headline :color-role color-role}

            ;; No chevron to fill the slot, so the headline sits at ONE col of
            ;; breathing room (flush-painted result-headline + a single space)
            ;; — the SAME left column as a chevron card's body rows, not the
            ;; deeper `▸ `-slot indent that read as a dangling left margin.
            headline
            (mapv (fn [line]
                    {:line (str result-marker " " line) :meta meta})
                  (wrap-text (ir-tui/ir->inline-sentinel-string (vis/markdown->ir head-line))
                             (max 1 (- (long fill-w) 1))))

            ;; A card can carry a body yet have NO node-id (nothing to fold it
            ;; under — e.g. a nil session-id). Never DROP that body: render it
            ;; inline, always-expanded, so the result still produces its output.
            body-rows
            (when (seq entries) (compact-tool-card-body-entries (mapv ->result entries)))]

        ;; Summary-only cards still get the result-band pad. Body cards keep the
        ;; same one-row headline separator as collapsible cards, then stay tight
        ;; inside their labeled sections.
        (vec (concat headline
                     (when (seq body-rows) [{:line (str result-marker "") :meta nil}])
                     body-rows
                     (when-not (seq body-rows) [{:line (str result-marker "") :meta nil}])))))))

(defn- format-iteration-entry-entries
  [entry code-width iteration-number &
   [{:keys [show-header? session-id detail-expansions session-turn-id live-preview?]
     :or {show-header? false live-preview? false}}]]
  ;; Iteration / block header labels removed per user directive. The
  ;; `show-header?` argument is retained as a no-op for callers; we
  ;; never paint the right-aligned ITERATION N band any more.
  (let [{:keys [thinking content-stream assistant-prose forms recaps provider-fallbacks error
                repeat-count]}
        entry

        ;; `:content-stream` is the LIVE prose accumulation streamed alongside
        ;; reasoning (dropped after parse). `:assistant-prose` is the SAME markdown
        ;; persisted on the trace-entry; it renders as its OWN block BETWEEN the
        ;; thinking trace and the code+result (see `prose-body` / the final layout)
        ;; — a "here's what I'm doing" read, placed ABOVE the code to match the
        ;; live stream and the web channel.
        ;; `content-stream` is the LIVE stream (prose OR the tool-call code the loop
        ;; re-emits as :content). Once a real `forms` block exists it ALREADY shows the
        ;; code, and genuine prose renders below via `:assistant-prose` — so echoing
        ;; content-stream into the thinking bubble would DUPLICATE the code. Only merge
        ;; it while no form has landed yet (the live pre-block window).
        content-stream
        (when (empty? forms) content-stream)

        thinking
        (cond (and (seq (some-> thinking
                                str
                                str/trim))
                   (seq (some-> content-stream
                                str
                                str/trim)))
              [thinking content-stream]
              (seq (some-> content-stream
                           str
                           str/trim))
              content-stream
              :else thinking)

        _
        show-header?

        fill-w
        (max 1 (dec (long code-width)))

        line-entry
        (fn [line]
          {:line line :meta nil})

        header
        []

        ;; Margin-top above Recap fires ONLY when this iteration actually
        ;; carries one (user directive). Without recap-lines the iteration
        ;; starts flush; with recap-lines the bubble gets a neutral blank
        ;; row between the "Vis" label (or prior iteration) and the Recap
        ;; text, so Recap breathes the way thinking and code blocks do.
        ;; The RECAP rail is retired entirely (per user directive). It
        ;; duplicated state already visible in the ctx block and
        ;; accumulated one stale row per iteration (`RECAP Task — ×
        ;; :K :cancelled`, SPEC, FACT, TITLE, plus provider / consult
        ;; notices). Provider errors still surface via `error-lines`
        ;; below; these destructured fields are intentionally unused.
        _
        [recaps provider-fallbacks]

        recap-lines
        []

        thinking-lines
        (fn [thinking-text-or-texts]
          ;; Per user direction: do NOT truncate reasoning while it's
          ;; streaming live. The full reasoning text flows into the
          ;; bubble as it arrives. Post-stream collapse (the ▾ REASONING
          ;; summary toggle) still fires once the iteration completes
          ;; via `maybe-collapse-thinking-entries` below.
          (let [raw-texts
                (if (sequential? thinking-text-or-texts)
                  thinking-text-or-texts
                  [thinking-text-or-texts])

                texts
                raw-texts

                entries
                (into []
                      (mapcat
                        (fn [thinking-text]
                          (when (and (string? thinking-text) (not (str/blank? thinking-text)))
                            ;; Thinking text comes from the LLM as plain
                            ;; markdown; lift to canonical IR via the SHARED
                            ;; `vis/reasoning->ir` (normalize + :soft-break
                            ;; :hard) — the SAME path the web thinking card
                            ;; uses, so a bold heading keeps its own line
                            ;; instead of collapsing onto its body. Then walk
                            ;; in `:thinking` mode (iter-header-bg / italic).
                            (let [ir (vis/reasoning->ir thinking-text)]
                              (or (seq (ir-tui/ir->entries ir
                                                           fill-w
                                                           {:mode :thinking
                                                            :session-id session-id
                                                            :session-turn-id session-turn-id
                                                            :detail-expansions detail-expansions
                                                            :iteration-number iteration-number
                                                            :section :thinking}))
                                  (mapv #(line-entry (str thinking-marker %))
                                        (wrap-text thinking-text fill-w)))))))
                      texts)]

            (when (seq entries)
              ;; THINKING ALWAYS collapses behind the plain ▸ THINKING badge
              ;; (op-row look) — live or finalized — to match the tool
              ;; affordance. `live-preview?` no longer forces it open; the
              ;; user expands on demand and the state persists across frames.
              ;; `maybe-collapse-thinking-entries` owns the full block
              ;; (badge + padding), so use its result verbatim.
              (let [_ live-preview?]
                (maybe-collapse-thinking-entries {:entries entries
                                                  :session-id session-id
                                                  :detail-expansions detail-expansions
                                                  :session-turn-id session-turn-id
                                                  :iteration-number iteration-number
                                                  :max-w fill-w})))))

        error-lines
        (fn []
          (when (and (map? error) (not (inline-rendered-form-error? forms error)))
            (let [repeat-count
                  (max 1 (long (or repeat-count 1)))

                  badge
                  (when (> repeat-count 1) (str "  x " repeat-count))

                  data
                  (:data error)

                  ;; A provider failure is one the shared classifier recognizes
                  ;; (`perr/provider-error-kind` ≠ :generic) OR one that carries HTTP
                  ;; facts. The kind check is what catches a TRANSPORT blip: it has NO
                  ;; :status/:body/:request-id (nothing answered), so the field probe
                  ;; alone would miss it and dump it as one plain generic line instead
                  ;; of the structured explanation / next-step / facts rows below.
                  provider-error?
                  (or (:status data)
                      (:body data)
                      (:request-id data)
                      (:request_id data)
                      (not= :generic (perr/provider-error-kind error)))

                  hdr-label
                  (str (label-text (if provider-error? "provider error" "error")) (or badge ""))

                  hdr-pad
                  (max 0 (- (long fill-w) (count hdr-label) 1))

                  hdr-line
                  (str iteration-hdr-marker (repeat-str \space hdr-pad) hdr-label " ")

                  err-message
                  (error-detail-text error)

                  err-headline
                  (if (> repeat-count 1) (str "ERROR x " repeat-count ": " err-message) err-message)

                  raw
                  (some-> (get-in error [:data :raw-data])
                          str
                          str/trim)

                  recv
                  (get-in error [:data :received-type])

                  provider-rows
                  (when provider-error?
                    (let [;; Bold the leading `WHAT HAPPENED:` / `NEXT STEP:` LABEL so the
                          ;; live trace reads like the styled answer IR (bold label, plain
                          ;; body). The label/body split uses the SHARED `split-error-label`
                          ;; (same helper the answer IR uses) so the convention never
                          ;; diverges between surfaces. Sentinels go on the FIRST wrapped
                          ;; row only — the label is short and never wraps. `paint-ansi-line!`
                          ;; (used by the err-result band) translates these to SGR/BOLD.
                          bold-label-on-first
                          (fn [s]
                            (let [[fst & rest] (wrap-text s fill-w)
                                  [label body] (perr/split-error-label fst)]

                              (when fst
                                (if (seq label)
                                  ;; Bold the LABEL on the first wrapped row only;
                                  ;; `body` is that row's remainder, `rest` are the
                                  ;; remaining wrapped rows (plain).
                                  (into [(str p/INLINE_BOLD_ON label p/INLINE_BOLD_OFF body)]
                                        (or rest []))
                                  (into [fst] (or rest []))))))]
                      ;; Same wording + facts the shared provider-error IR renders
                      ;; for the final answer / Web — one source of truth so a
                      ;; failure reads identically everywhere.
                      (mapv #(line-entry (str err-result-marker %))
                            (mapcat bold-label-on-first
                                    (concat
                                      ;; NEXT STEP is a SEPARATE block now (split out
                                      ;; of the explanation) — surface it here too so
                                      ;; the recap matches the shared IR.
                                      [(perr/provider-error-explanation error)
                                       (perr/provider-error-next-step error)]
                                      (mapv (fn [[label value]]
                                              (str label ": " value))
                                            (perr/provider-error-facts error))
                                      (when-let [rb (perr/provider-error-raw-body error)]
                                        ["Provider response:" rb]))))))

                  err-message-rows
                  (mapv #(line-entry (str err-result-marker %)) (wrap-text err-headline fill-w))

                  raw-rows
                  (when (and raw (not (str/blank? raw)))
                    (let [hdr
                          (str "provider returned" (when recv (str " (" recv ")")) ":")

                          raw-trim
                          (if (> (count raw) 600) (str (subs raw 0 600) "...") raw)

                          body-lines
                          (mapv #(line-entry (str err-result-marker %))
                                (wrap-text raw-trim fill-w))]

                      (into [(line-entry (str err-result-marker hdr))] body-lines)))]

              (vec (concat [(line-entry (str iteration-pad-marker ""))]
                           (when show-header? [(line-entry (str iteration-pad-marker ""))])
                           (when show-header? [(line-entry hdr-line)])
                           [(line-entry (str code-err-pad-marker ""))]
                           (or provider-rows err-message-rows)
                           (when (seq raw-rows) [(line-entry (str code-err-pad-marker ""))])
                           (or raw-rows [])
                           [(line-entry (str code-err-pad-marker ""))])))))

        form-lines
        (fn [form block-number]
          (let [{:keys [code comment error success?]}
                form

                has-status?
                (some? success?)

                is-error?
                (and has-status? (not success?))

                ;; BLOCK N header removed per user directive (also gated
                ;; on `show-header?` which is now always false). Keep
                ;; `expr-hdr` defined as empty so the existing `(when
                ;; show-header? ...)` branch is dead but type-safe.
                _expr-num
                block-number

                expr-hdr
                ""

                c-marker
                (cond (not has-status?) code-marker
                      success? code-ok-marker
                      :else code-err-marker)

                c-pad
                (cond is-error? code-err-pad-marker
                      success? code-ok-pad-marker
                      :else code-pad-marker)

                comment-lines
                (when (and (string? comment) (not (str/blank? comment)))
                  (let [trimmed
                        (str/trim comment)

                        ;; Form comments sit in their own thinking-style band
                        ;; above the code block. Give the visible text the same
                        ;; one-column left breathing room as code rows, without
                        ;; shifting all reasoning/thinking rows globally.
                        comment-w
                        (max 1 (dec (long fill-w)))

                        wrapped
                        (mapcat (fn [line]
                                  (wrap-text line comment-w))
                                (str/split-lines trimmed))]

                    (mapv #(line-entry (str thinking-marker " " %)) wrapped)))

                ;; Engine-mutation recap rows (TITLE/TASK/SPEC/FACT) were
                ;; retired alongside the recap rail. Code body + op rows are
                ;; the only per-form surface now.
                title-lines
                []

                ;; Canonical code surface — the SAME contract as web's
                ;; `block-code` (channel_web/core): paint the model's raw `:code`,
                ;; beautified via ruff (cached; verbatim fallback when ruff is
                ;; unavailable). NO `:render-segments` / `:vis/show-raw-code`
                ;; gate — a structurally-silent (engine-chrome / answer) form
                ;; carries no code and is already filtered upstream, so a blank
                ;; `code` is the only thing that drops the row (`hide-code-chrome?`).
                code-text
                (str/trim (str (vis/beautify-python code)))

                ;; Syntax-color the executed source (always Python in the engine
                ;; loop) with the tree-sitter highlighter — the SAME ANSI-SGR run
                ;; mechanism `paint-ansi-line!` already translates for diff fences,
                ;; so no painter change is needed. Split into per-line colored rows.
                colored-lines
                (when-not error
                  (some-> (hl/highlight "python" code-text)
                          str/split-lines))

                inline-error-code-lines
                (when error (inline-error-context-lines code-text error))

                ;; A pathologically wide single line (a one-line `git_commit({...})`
                ;; arg) is SOFT-FOLDED at the bubble edge via `p/fold-cols` so it
                ;; stops overflowing / being clipped; indentation and in-row
                ;; alignment survive and lines within budget pass through. The
                ;; error path (`inline-error-code-lines`) is left UNFOLDED so its
                ;; `^---` caret stays column-aligned to the source. A row that fits
                ;; keeps its COLORED form; a folded (over-wide) row falls back to
                ;; its plain segments so the column math (ANSI-blind) and overflow
                ;; guards stay correct.
                code-lines
                (or inline-error-code-lines
                    (vec (mapcat (fn [plain colored]
                                   (let [folded (p/fold-cols plain fill-w)]
                                     (if (and colored (= 1 (count folded))) [colored] folded)))
                                 (str/split-lines code-text)
                                 (or colored-lines (repeat nil)))))

                code-node-id
                (when session-id
                  (detail-node-id {:session-turn-id session-turn-id
                                   :iteration-number iteration-number
                                   :block-number block-number
                                   :section :iteration
                                   :kind :code}))

                c-lines
                (tag-copy-block-body (mapv #(line-entry (str c-marker %)) code-lines)
                                     code-node-id
                                     code-text)

                ;; Human result surface: the form RETURN value as markdown. Stdout is
                ;; model-context only and is not rendered in human channels.
                ;; Long results mirror thinking: keep the first rows visible and
                ;; collapse only the surplus behind a compact details row.
                ;; Prefer the loop's pre-rendered display STRING (`:result-render` —
                ;; the native-tool card / pretty result) for NATIVE TOOL forms only
                ;; (gated on `:vis/tool-name`). It's persisted, so a DB-restored trace
                ;; shows the SAME card the live stream did instead of pr-str'ing the
                ;; raw `:result` map. Plain `:value` form results have no tool name
                ;; and stay hidden while streaming (per the no-bare-value directive).
                tool-name*
                (:vis/tool-name form)

                ;; Canonical op-card descriptor — `tool?`, badge LABEL/colour, the
                ;; HEADLINE `:summary`, and `:collapsible?` are decided ONCE in the
                ;; gateway (`vis/result-card`) so the TUI badge can't drift from the
                ;; web one. nil for a non-tool form (its body stays the EDN below).
                card
                (vis/result-card form)

                ;; The op-card HEADLINE — a real tool-authored summary
                ;; ("5 hits in 1 file", "moved `a` → `b`"), never a first-line
                ;; slice of the body. Only native-tool forms carry one.
                head-summary
                (:summary card)

                result-text
                (let [rendered
                      (when tool-name* (:result-render form))

                      v
                      (:result form)]

                  (cond (and (string? rendered) (not (str/blank? rendered))) (str/trimr rendered)
                        ;; A native tool that returned ONLY a `:result-summary`
                        ;; (move/delete/exists) has no body — the summary alone
                        ;; IS the card; never fall back to an EDN dump of :result.
                        head-summary nil
                        (nil? v) nil
                        (string? v) (some-> v
                                            str/trimr
                                            not-empty)
                        :else (str "```edn\n" (pr-str v) "\n```")))

                result-node-id
                (when (and session-id result-text)
                  (detail-node-id {:session-turn-id session-turn-id
                                   :iteration-number iteration-number
                                   :block-number block-number
                                   :section :iteration
                                   :kind :result}))

                ;; Result renders as MARKDOWN — same IR pipeline as the answer, in
                ;; `:channel` mode so plain prose has no answer-bg but headings /
                ;; lists / code bands still style. This is what makes the trace
                ;; readable instead of a flat text dump.
                ;; PRINT-MANY: a python block that printed several tool results carries a
                ;; canonical mini-form per result in `:cards`. Render EACH as its OWN
                ;; collapsible op-card in its tool's colour, with a distinct
                ;; `:details-path` node-id so each card's expand state is independent.
                ;; `vis/result-cards` is the ONE projection the web uses too.
                printed-cards?
                (seq (:cards form))

                result-lines
                (cond
                  printed-cards?
                  (vec (mapcat (fn [i c]
                                 (tool-card-entries
                                   c
                                   {:fill-w fill-w
                                    :session-id session-id
                                    :detail-expansions detail-expansions
                                    :node-id (when (and session-id (or (:body c) (:summary c)))
                                               (detail-node-id {:session-turn-id session-turn-id
                                                                :iteration-number iteration-number
                                                                :block-number block-number
                                                                :section :iteration
                                                                :kind :result
                                                                :details-path [i]}))}))
                               (range)
                               (vis/result-cards form)))
                  (or result-text head-summary)
                  (let [entries
                        (when result-text
                          (tag-copy-block-body (vec (paste-aware-ir->entries
                                                      (vis/markdown->ir result-text)
                                                      fill-w
                                                      {:mode :channel
                                                       :session-id session-id
                                                       :session-turn-id session-turn-id
                                                       :detail-expansions detail-expansions
                                                       :image-section :iteration
                                                       :image-default-expanded? true}))
                                               result-node-id
                                               result-text))

                        entries
                        (vec entries)

                        ;; Native tools (cat/rg/patch/…) carry a tool name +
                        ;; `:tool-color-role`. The LABEL renames a few wire
                        ;; names (python_execution → CODE).
                        tool-label
                        (:label card)

                        preview-n
                        reasoning-auto-collapse-line-threshold

                        hidden
                        (vec (drop preview-n entries))

                        ;; Re-mark every body line into the RESULT zone (strip
                        ;; its md/prose marker, prepend the result marker) so the
                        ;; WHOLE op-card paints on one `result-bg` band — code AND
                        ;; prose/eval output alike. Embedded ANSI (diff +/-)
                        ;; survives via `paint-ansi-line!` in that paint branch.
                        _->result
                        (fn [e]
                          (let [l
                                (str (:line e))

                                stripped
                                (or (second (split-structural-line-marker l)) l)]

                            (assoc e :line (str result-marker stripped))))]

                    (cond
                      ;; NATIVE TOOL result: the tool LABEL + the tool-authored
                      ;; SUMMARY ride ON the headline, painted in the tool's
                      ;; colour; the WHOLE `:result-render` body nests UNDER it
                      ;; (collapsible). The op-card look — label is never mixed
                      ;; into a body line, no `[details]` decoration, and the
                      ;; headline is a REAL summary the tool returned, never a
                      ;; first-line slice of the body. A summary-only tool
                      ;; (move/delete/exists) renders a plain headline with no
                      ;; expand triangle since there's nothing beneath it.
                      tool-label (tool-card-entries card
                                                    {:fill-w fill-w
                                                     :session-id session-id
                                                     :detail-expansions detail-expansions
                                                     :node-id result-node-id})
                      ;; Non-tool, long result: keep the first rows visible
                      ;; and collapse only the surplus (unlabeled).
                      (and result-node-id (seq hidden))
                      (let [expanded?
                            (detail-expanded? detail-expansions session-id result-node-id false)

                            visible
                            (vec (take preview-n entries))

                            summary
                            (detail-summary-entries
                              {:marker result-marker
                               :max-w fill-w
                               :summary
                               (if expanded? "result" (str "+" (count hidden) " more result lines"))
                               :hidden-entries hidden
                               :collapsed? (not expanded?)
                               :session-id session-id
                               :node-id result-node-id
                               :color-role nil})]

                        (vec (concat visible summary (when expanded? hidden))))
                      :else entries)))

                inline-error-message-lines
                (when error
                  (mapv #(line-entry (str c-marker %))
                        (wrap-text (form-error-headline error) fill-w)))

                ;; NATIVE tool calls (cat/rg/patch/…) already render as an op-card
                ;; (headline + result body); their synthesized invocation source is
                ;; redundant chrome, so drop the code block. ONLY `python_execution`
                ;; — the model's OWN program — keeps its code visible. A blank-code
                ;; form (or any non-tool form with no code) also drops the chrome.
                ;; Errors always keep the code so the inline caret has context.
                ;; The native-tool code-chrome decision lives ONCE in the gateway
                ;; (`vis/hide-tool-code?`) — a successful native non-python call drops
                ;; its redundant `name(args)` source — so the TUI and web can't drift.
                ;; A blank-code non-tool form also drops the (empty) chrome.
                hide-code-chrome?
                (or (and (not is-error?) (str/blank? code-text)) (vis/hide-tool-code? form))

                code-block
                (cond hide-code-chrome?
                      ;; When raw code is hidden (def-wrapped tool
                      ;; call or any successful tool form), drop the
                      ;; code chrome entirely: the block-level op
                      ;; rows (`▸ <LABEL> <summary>`) speak for the
                      ;; form, and the BLOCK header carries the
                      ;; aggregate status + duration. Only title
                      ;; recap rows (ctx mutations like TITLE) stay,
                      ;; since those are recap text, not code, and
                      ;; have no op row. No per-form footer.
                      (vec (concat (when (seq title-lines) [(line-entry "")])
                                   title-lines
                                   (when (seq title-lines) [(line-entry "")])))
                      :else (vec
                              (concat
                                ;; Title-recap call-out: one TRUE neutral
                                ;; (terminal-bg) blank row above and below.
                                ;; The thinking-pad row that may precede
                                ;; this block paints with iteration-header-bg
                                ;; (gray stripe) and reads as the tail of
                                ;; thinking — NOT as a margin. The user
                                ;; sees the recap glued to thinking unless
                                ;; we add a terminal-bg blank here.
                                (when (seq title-lines) [(line-entry "")])
                                title-lines
                                (when (seq title-lines) [(line-entry "")])
                                (when show-header?
                                  [(line-entry (str iteration-hdr-marker expr-hdr))])
                                (when (seq comment-lines)
                                  (concat [(line-entry (str thinking-marker ""))]
                                          comment-lines
                                          [(line-entry (str thinking-marker ""))]))
                                [(line-entry (str c-pad ""))]
                                c-lines
                                (when (seq inline-error-message-lines) inline-error-message-lines)
                                ;; Bottom band edge. No per-form status
                                ;; footer; code blocks stay source-only.
                                [(line-entry (str c-pad ""))])))]

            ;; The form contributes its code body (errors keep their inline
            ;; caret) followed by what it PRINTED (stdout) — the single result
            ;; surface, one ` ` gutter row of breathing space above it.
            (vec (concat code-block
                         (when (seq result-lines)
                           (concat [(line-entry (str result-marker ""))] result-lines))))))

        ;; The display-block's CODE BODY: per-proof-envelope (`:forms`) code
        ;; rows joined into the one card. Phase-5 dropped per-form result
        ;; panes, so this carries code lines only (tool output paints below
        ;; as block-level op rows). `forms` here = proof envelopes, the
        ;; canonical meaning; the rendered card is the display block.
        block-code-body
        (when (seq forms)
          (let [forms-vec
                (vis/coalesce-forms (vec forms))

                ;; A code-less native op-card (cat/rg/patch/…, not
                ;; python_execution, no error) hides its code chrome
                ;; and paints ONLY the op rows — which already open
                ;; with their own leading breathe blank. So when one
                ;; such card immediately follows another, the
                ;; inter-form terminal-bg pad below is a REDUNDANT
                ;; second margin; drop it so consecutive native calls
                ;; stack flush (one breathe row, not two).
                chrome-hidden?
                (fn [form]
                  (let [tn
                        (:vis/tool-name form)

                        err?
                        (and (some? (:success? form)) (not (:success? form)))]

                    (boolean (and tn (not= tn "python_execution") (not err?)))))

                block-code-lines
                (into []
                      (mapcat (fn [[idx form]]
                                (let [;; Adjacent code-less native op-cards share ONE
                                      ;; continuous result-bg BAND: every card keeps its
                                      ;; own leading breathe + trailing pad row — the
                                      ;; pre-allocated background under each op headline,
                                      ;; so a grouped tool call is always TWO rows
                                      ;; (headline + band row), and an EXPANDED body
                                      ;; keeps a band row before the next headline
                                      ;; instead of gluing to it. The blank-coalesce
                                      ;; pass folds the seam (this card's trailing pad +
                                      ;; the next card's leading breathe) into a single
                                      ;; band row. Only the terminal-bg inter-form gap
                                      ;; is skipped inside such a run, so the band never
                                      ;; tears back to terminal bg mid-run.
                                      prev-native?
                                      (and (pos? (long idx))
                                           (chrome-hidden? form)
                                           (chrome-hidden? (nth forms-vec (dec (long idx)))))

                                      fl
                                      (form-lines form (inc (long idx)))]

                                  ;; ONE terminal-bg blank between consecutive
                                  ;; forms inside the same iteration — skipped when
                                  ;; both are chrome-hidden native cards (their shared
                                  ;; result-bg band already separates them).
                                  (concat (when (and (pos? (long idx)) (not prev-native?))
                                            [(line-entry (str iteration-pad-marker ""))])
                                          fl)))
                              (map-indexed vector forms-vec)))]

            ;; TRAILING iter-pad only. It separates this iteration's
            ;; body from the NEXT iteration below by a single
            ;; terminal-bg blank (coalesces with the next iteration's
            ;; leading thinking blank, same `:gap` family).
            ;;
            ;; The LEADING iter-pad was removed: it predates the
            ;; retired RECAP rail (it used to separate the code from a
            ;; recap row above). With recaps gone it only wedged a
            ;; blank row between the THINKING band/badge and the code
            ;; of the SAME iteration, which read as a false section
            ;; break. Dropping it realises the documented contract
            ;; ("zero gap between the thinking band and the code"):
            ;;   [thinking badge / band edge]
            ;;   [green code top]   <- glued directly under thinking
            ;;   code lines
            ;;   [green code bot]
            ;;   [gap]              <- this trailing iter-pad
            ;;   op rows / next iteration ...
            (when (seq block-code-lines)
              (-> (vec block-code-lines)
                  (conj (line-entry (str iteration-pad-marker "")))))))

        body
        (or block-code-body [])

        trailing-errors
        (error-lines)

        thinking-body
        (or (thinking-lines thinking) [])

        ;; The model's persisted prose renders as its OWN markdown block BETWEEN
        ;; the thinking trace and the code+result — NOT through the thinking
        ;; formatter (it is the model's commentary/answer, not reasoning). Placed
        ;; ABOVE the code to match the live stream (loop emits `:assistant-prose`
        ;; before the code runs) and the web channel — else it read as missing/
        ;; detached at the bottom. Same `:mode :channel` markdown path the per-form
        ;; result text uses, so a bold word / list paints normally instead of as a
        ;; dim italic thinking trace.
        prose-body
        (when-let [p (some-> assistant-prose
                             str
                             str/trim
                             not-empty)]
          ;; One neutral blank above AND below the prose so the
          ;; commentary reads as its own block instead of gluing to
          ;; the thinking band above / the green code band below. The
          ;; coalesce pass upstream collapses any doubled blank to one.
          (-> [(line-entry "")]
              (into (ir-tui/ir->entries (vis/markdown->ir p) fill-w {:mode :channel}))
              (conj (line-entry ""))))

        ;; Block count headers (`1 observation · 2 mutations`) and their
        ;; block-level collapse toggle are intentionally gone. Code body always
        ;; stays visible; op rows below it remain the only compact tool-output
        ;; controls.
        ;; Op rows are gone — tool output now paints per-form as stdout (in
        ;; `form-lines` above). The block contributes thinking + code + stdout.
        header-lines
        []]

    ;; Layout: header (optional ITERATION-N label) + recap lines
    ;; (provider-fallback notices, provider-error recap, recap
    ;; segments) + thinking lines + error rows + body (per-form
    ;; code/result pairs). Resume / live share the same flat layout.
    ;;
    ;; Spacing contract is enforced two layers up by the coalesce
    ;; pass in `trace-render-entries`: any run of adjacent blank
    ;; rows collapses to ONE, with the THINKING pad (`MARKER_THINKING`,
    ;; `\u200B`) preserved so the dim thinking band paints cleanly.
    ;; That guarantees max-1-blank between rendered sections and
    ;; zero gap between the thinking band and the code (the
    ;; thinking pad is the bottom \"band edge\"; the code chrome
    ;; takes over immediately).
    (-> (vec (concat header header-lines recap-lines thinking-body trailing-errors))
        (into (or prose-body []))
        (into body))))
(defn format-iteration-entry
  [entry code-width iteration-number & [opts]]
  (mapv :line (apply format-iteration-entry-entries entry code-width iteration-number [opts])))
;;; ── Spinner glyph (used by the in-bubble “working...” row) ──────────────────
(def ^:private spinner-frames
  "Braille-dot spinner frames; one frame advances every ~100ms."
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
(defn spinner-frame
  "Pick the spinner glyph for `now-ms`. Pure - same input always
   returns the same frame, which makes the render loop testable."
  ^String [^long now-ms]
  (nth spinner-frames (mod (quot now-ms 100) (count spinner-frames))))
(defn- prettify-error-type
  "`:svar.core/http-error` -> \"http error\". Drops the namespace and
   replaces dashes with spaces so the spinner line reads naturally,
   e.g. \"iter 0 - http error - retrying\". Returns nil when there's
   nothing useful to print."
  [error-data]
  (when-let [t (some-> error-data
                       :type)]
    (let [bare (cond-> t
                 (keyword? t)
                 name)]
      (when (and (string? bare) (not (str/blank? bare))) (str/replace bare "-" " ")))))
(defn- progress-phase
  "Human-readable phase label for the current iteration state. Drives
   the spinner row text so the user can tell whether Vis is calling
   the provider, thinking, executing, running a shell command, retrying,
   or cancelling.

   Anthropomorphic `Vis is ...` phrasing reads as a status sentence
   instead of a system log line. No elapsed-time-driven escalation -
   wall-clock is already shown right next to this string in the
   spinner row, the user can read the seconds themselves.

   A `!`/`!&` bang turn carries `{:activity :shell-run|:shell-bg}` and
   `:shell/cmd` on its single iteration; the shell branches read those
   so the bubble says `Vis is running: <cmd>` while the shell blocks."
  [iterations cancelling?]
  (let [n
        (count iterations)

        last-iteration
        (last iterations)

        err
        (:error last-iteration)

        activity
        (:activity last-iteration)

        tool-op
        (:tool/op last-iteration)

        errored?
        (some? err)

        thinking?
        (and (not errored?)
             (some? (:thinking last-iteration))
             (not (str/blank? (:thinking last-iteration))))

        executing?
        (and (not errored?) last-iteration (seq (:forms last-iteration)))

        shell-cmd
        (some-> (:shell/cmd last-iteration)
                str
                (str/split #"\n")
                first
                str/trim)

        shell-label
        (cond (str/blank? shell-cmd) "…"
              (> (count shell-cmd) 64) (str (subs shell-cmd 0 61) "…")
              :else shell-cmd)]

    (cond cancelling? "Vis is cancelling"
          errored? (let [label (prettify-error-type err)]
                     (str "Vis is retrying" (when label (str " after " label)) " (iter " n ")"))
          (zero? n) "Vis is calling the provider"
          (= :shell-run activity) (str "Vis is running: " shell-label)
          (= :shell-bg activity) (str "Vis is starting: " shell-label)
          (= :provider-call activity) (str "Vis is calling the provider (iter " n ")")
          (= :response-parse activity) (str "Vis is parsing model response (iter " n ")")
          (= :tool-call activity) (str "Vis is running: " (or tool-op "tool") " (iter " n ")")
          thinking? (str "Vis is thinking (iter " n ")")
          executing? (str "Vis is running code (iter " n ")")
          :else (str "Vis is working (iter " n ")"))))
(defn- coalesce-bubble-blanks
  "Collapse adjacent SAME-FAMILY blank rows down to ONE; preserve
   adjacent DIFFERENT-FAMILY blanks (each paints a distinct visual
   band the user reads as part of the section, not as a gap).

   Marker families:
     - terminal-bg / outer-margin gap — the empty string
       (`\"\"`) and MARKER_ITERATION_PAD (`\u206C`). Painted with the
       terminal default bg — the literal \"blank row\" the user expects
       between sections.
     - thinking pad — MARKER_THINKING (`\u200B`). Paints the dim band
       wrapping a thinking section; band edges, NOT gaps. The test
       suite pins their presence; reading them as gaps collapses the
       band to a single line.
     - code/tool/status PUA pads — (`\uE000`..`\uE0FF`). Paint a
       colored band-edge row at the top/bottom of a code block or
       tool result; the row itself is glyph-less but the bg color
       reads as \"top/bottom of the bubble\".
     - answer pad — MARKER_ANSWER_PAD (`\u206F`). Paints the answer-
       bg band edge.

   The user's spacing contract: green code blocks keep their colored
   top/bottom pad rows AND a terminal-bg gap separates them from
   the next section. The previous \"keep the most neutral blank\"
   rule collapsed the green pad out, gluing the next badge against
   the code body and dropping the green band-edge entirely.

   Same-family adjacent: drop duplicates (no visual value).
   Different-family adjacent: keep both (each row paints a distinct
   band the user reads as semantically meaningful).

   Use this at every bubble-assembly seam (trace stream, full
   bubble payload) so live and restored paths share one contract."
  [entries]
  (let [family
        (fn [{:keys [^String line]}]
          (cond (or (nil? line) (zero? (count line))) :gap
                :else (let [n0 (int (.charAt line 0))]
                        (cond
                          ;; Pure terminal-bg / outer-margin family.
                          (= n0 0x206C) ; MARKER_ITERATION_PAD
                          :gap
                          ;; Thinking pad — NEVER coalesce; flushes via the
                          ;; explicit branch below.
                          (= n0 0x200B) :thinking
                          ;; Answer-pad band edge.
                          (= n0 0x206F) :answer-pad
                          ;; Code/tool/status PUA band edges — distinct per
                          ;; codepoint so an OK pad never coalesces with an
                          ;; ERR pad even when both happen to be blank.
                          (<= 0xE000 n0 0xE0FF) [:pua n0]
                          ;; Other invisible format chars.
                          :else [:other n0]))))

        blank?
        (fn [{:keys [^String line]}]
          (let [body (if (and (string? line) (pos? (count line))) (subs line 1) (str line))]
            (str/blank? body)))]

    (loop [out
           (transient [])

           prev-family
           nil

           xs
           (seq entries)]

      (if (nil? xs)
        (persistent! out)
        (let [e (first xs)]
          (cond (and (blank? e) (= :thinking (family e)))
                ;; Thinking band: emit verbatim, reset family so a
                ;; following gap row still counts as a fresh blank.
                (recur (conj! out e) :thinking (next xs))
                (blank? e) (let [f (family e)]
                             (if (= f prev-family)
                               ;; Same-family duplicate — drop.
                               (recur out prev-family (next xs))
                               (recur (conj! out e) f (next xs))))
                :else (recur (conj! out e) nil (next xs))))))))
(defn- mergeable-iteration-forms
  "Forms of an iteration when it is a PLAIN tool iteration — has forms, carries no
   iteration-level error / recap / provider-fallback, and no `:cards` block on any
   form. Returns the forms vector, else nil.

   No tool-name whitelist and no same-tool requirement: ANY maximal run of
   consecutive plain tool iterations (see `render-iteration-entries`) merges into
   ONE flush-stacked bubble. NARRATION does not disqualify a run's HEAD — its
   thinking / prose renders above the merged forms — but breaks the run on an
   INTERIOR iteration so mid-burst commentary never floats out of place."
  [entry]
  (let [{:keys [forms recaps provider-fallbacks error]} entry]
    (when (and (nil? error)
               (empty? recaps)
               (empty? provider-fallbacks)
               (seq forms)
               (every? #(empty? (:cards %)) forms))
      (vec forms))))

(defn- iteration-narration?
  "True when an iteration carries visible NARRATION that must render on its own —
   a thinking badge (only when `show-thinking?`, since hidden thinking paints
   nothing) or an assistant-prose block. A narrated call may still OPEN a merged
   run (its narration renders above the flush-stacked forms); narration on an
   INTERIOR call breaks the run so mid-burst commentary never floats out of place."
  [entry show-thinking?]
  (boolean (or (and show-thinking? (not (str/blank? (str (:thinking entry)))))
               (not (str/blank? (str (:assistant-prose entry)))))))

(defn- render-iteration-entries
  "Turn the visible `[idx entry]` iteration pairs into painter entries. A MAXIMAL
   run of consecutive PLAIN tool iterations (`mergeable-iteration-forms`) — any
   tools, mixed — MERGES into ONE synthetic iteration rendered once through
   `iter-entry-fn`, so their op-cards flush-stack into a single bubble with no
   inter-iteration gap (the within-iteration flush in `format-iteration-entry-…`
   already paints adjacent native cards without a blank between them). Prose /
   thinking is the ONLY separator: a narrated iteration may OPEN a run (its
   narration renders ABOVE the merged forms) but an INTERIOR narrated call breaks
   it. A non-tool iteration (narration only, an iteration-level error / recap /
   provider-fallback, a `:cards` block) renders on its own through `iter-entry-fn`.

   This is the uniform-compaction contract: EVERY consecutive tool run collapses
   the same way — there is no tool-name whitelist and no per-tool summary band."
  [visible-iterations iter-entry-fn show-silent? show-thinking? _group-ctx]
  (let [tagged (mapv (fn [pair]
                       (let [e (visible-iteration-entry (second pair) show-silent?)]
                         [pair (mergeable-iteration-forms e)
                          (iteration-narration? e show-thinking?)]))
                     visible-iterations)]
    (loop [out (transient [])
           xs (seq tagged)]

      (if (nil? xs)
        (persistent! out)
        (let [[_pair mf _narr?] (first xs)]
          (if mf
            ;; Head is a plain tool iteration (its narration, if any, renders
            ;; above the merged forms). Extend the run over following plain tool
            ;; iterations that are NOT narrated, then merge every form into ONE
            ;; synthetic iteration rendered once — the within-iteration flush
            ;; stacks the op-cards into a single gap-less bubble.
            (let [run (cons (first xs)
                            (take-while (fn [[_ f narr?]]
                                          (and (some? f) (not narr?)))
                                        (rest xs)))
                  cnt (count run)
                  forms (into []
                              (mapcat (fn [[_ f]]
                                        f))
                              run)]

              (if (>= cnt 2)
                (let [[[first-idx head-entry] _ _] (first run)
                      merged (iter-entry-fn [first-idx (assoc head-entry :forms forms)])]

                  (recur (reduce conj! out merged) (seq (drop cnt xs))))
                (recur (reduce conj! out (iter-entry-fn (first (first xs)))) (next xs))))
            (recur (reduce conj! out (iter-entry-fn (first (first xs)))) (next xs))))))))

(defn- trace-render-entries
  "Unified renderer for iteration traces in live, cancelled, and completed
   assistant bubbles. Live progress and final/cancel rendering must call this
   instead of formatting iterations themselves. The only caller-specific UI is
   the trailer after these entries (spinner, final answer, or cancelled note)."
  [{:keys [iterations content-w settings session-id session-turn-id detail-expansions live?
           suppress-trace?]
    :or {live? false suppress-trace? false}}]
  (let [raw-iterations
        (or iterations [])

        iterations
        (if (vector? raw-iterations) raw-iterations (vec raw-iterations))

        show-thinking?
        (get settings :show-thinking true)

        show-iterations?
        (get settings :show-iterations true)

        show-silent?
        (get settings :show-silent false)

        ;; One trace renderer means one visual contract: no iteration/block
        ;; label bands in live, completed, or cancelled bubbles.
        show-iteration-headers?
        false

        ;; Per user directive: every iteration is always visible.
        grouped-iterations
        (collapse-repeated-error-runs iterations)

        visible-iterations
        grouped-iterations

        iter-entry-fn
        (fn [[idx entry]]
          (let [visible
                (visible-iteration-entry entry show-silent?)

                stripped
                (if show-thinking? visible (dissoc visible :thinking))

                iter-num
                (inc (long idx))

                detail-scope-opts
                {:section :iteration
                 :iteration-number iter-num
                 :session-id session-id
                 :session-turn-id session-turn-id
                 :detail-expansions detail-expansions}

                k
                [::iter-entries (if live? :live :final) iter-num (iteration-fingerprint stripped)
                 (long content-w) show-iteration-headers? (boolean show-thinking?)
                 (boolean show-silent?) session-id session-turn-id
                 ;; Tool-badge / op-row disclosures are keyed
                 ;; `iter<N>:t<frag>:op<M>` — scoped by the TURN
                 ;; token, NOT the `iteration:t<frag>:i<N>` base
                 ;; that `relevant-detail-expansions-key` filters
                 ;; on. Using the iteration-scoped key here meant
                 ;; toggling a badge in LIVE view never busted
                 ;; this cache, so the badge never collapsed/
                 ;; expanded until the turn finished. Turn-scoped
                 ;; key catches every disclosure in the bubble.
                 (turn-detail-expansions-key detail-scope-opts)]

                inner-opts
                {:show-header? show-iteration-headers?
                 :session-id session-id
                 :session-turn-id session-turn-id
                 :detail-expansions detail-expansions
                 :live-preview? live?}

                render!
                #(format-iteration-entry-entries stripped content-w iter-num inner-opts)]

            (if live? (cached* k render!) (render!))))]

    (when (and show-iterations? (not suppress-trace?) (seq iterations))
      ;; The code blocks render flat — no TURN wrapper. The turn-level
      ;; collapsible header was removed (it only hid the blocks and carried no
      ;; information the per-block headers + op rows don't already convey).
      (coalesce-bubble-blanks (render-iteration-entries visible-iterations
                                                        iter-entry-fn
                                                        show-silent?
                                                        show-thinking?
                                                        {:fill-w (max 1 (dec (long content-w)))
                                                         :session-id session-id
                                                         :session-turn-id session-turn-id
                                                         :detail-expansions detail-expansions})))))
(defn- queued-preview
  [text]
  (let [s (-> (str (or text ""))
              (str/replace #"\s+" " ")
              str/trim)]
    (if (> (count s) 240) (str (subs s 0 240) "…") s)))
(defn- queued-progress-entries
  [pending-sends content-w]
  (let [queued (vec (or pending-sends []))]
    (when (seq queued)
      (let [;; Every queue row carries a leading rail glyph `│` (painted by
            ;; the marker painters) so the whole block reads as ONE bracketed
            ;; group — the same left-bar affordance a "You" bubble uses.
            ;;
            ;; Header row: bold accent "Queued".
            hdr-line (str queue-hdr-marker "Queued")
            ;; Rail + its trailing space eat 2 cols before any content.
            rail-w 2
            ;; Ordinals count in SEND ORDER, top to bottom: #1 is the item that
            ;; fires NEXT (oldest, first in the vec, rendered at the top), then
            ;; #2, #3 … down to #N — the newest queued submission at the bottom,
            ;; nearest the input box (the one ArrowUp pulls back for editing).
            ;; Reading top-to-bottom is 1,2,3,…,N, matching the order they send.
            ;; Each row is ONE clipped line: the ordinal in the accent gutter,
            ;; then the preview right-clipped with an ellipsis so it always fits
            ;; the width and never wraps.
            item-line (fn [idx entry]
                        (let [ord (str (inc (long idx)) ". ")
                              gutter-n (count ord)
                              avail (max 1 (- (long content-w) (long rail-w) (long gutter-n)))
                              preview (ellipsize-cols (queued-preview (:text entry)) avail)]

                          {:line (str queue-item-marker ord preview)
                           :meta {:queue-gutter gutter-n}}))
            ;; Items stack directly, one line each — no blank rows between them.
            item-lines (vec (map-indexed item-line queued))
            ;; Bottom border closes the block and caps the left rail, sitting
            ;; between the queued items and the edit hint.
            border {:line queue-border-marker :meta nil}
            ;; Nudge: ArrowUp on an empty input box pulls the newest queued
            ;; submission (item #N, the bottom row) back into the editor (see state.clj
            ;; :history-up). Accent hint on the REGULAR bubble bg (via
            ;; `hint-marker`) so the affordance pops as a control.
            hint {:line (str hint-marker "↑ to edit") :meta nil}]

        (vec
          (concat [{:line "" :meta nil} {:line hdr-line :meta nil}] item-lines [border hint]))))))
(defn progress->lines-data
  "Build prewrapped lines for the live progress placeholder bubble.

   The bubble lives in the assistant slot (right where the final
   answer will land), so the user sees the agent thinking/working in
   place instead of a status line wedged into the input box.

   Layout while loading (spinner ALWAYS at the bottom):
     <iteration trace, if iterations exist and not hidden>
     <blank>
     [spinner] phase... elapsed / Esc to cancel

   The activity row sits last so it tracks the natural reading
   direction - trace history flows top-down like a transcript, and
   what is happening RIGHT NOW lives where the cursor is about to
   write next. Putting the spinner above the trace forced the user
   to look at a moving line, then drop their eye further down to
   read static history that doesn't change. Inverted now.

   `progress` is the `:progress` slot from app-db: `{:iterations [...]}`.
   `bubble-w` is the outer bubble width in chars (we subtract the
   bubble's symmetric inner padding to match draw-chat-bubble!).
   `settings` is the display settings map: `{:show-thinking bool :show-iterations bool}`.
   `extra` carries:
     :now-ms         - `System/currentTimeMillis` from the render thread
                       (drives the spinner frame); defaults to current ms
     :turn-start-ms - wall-clock start, used for elapsed time
     :cancelling?    - true once Esc was pressed
     :session-id - current session id; enables live detail rows
     :session-turn-id - optional turn id, when known
     :detail-expansions - detail expansion state keyed by session/node"
  ([progress bubble-w settings] (progress->lines-data progress bubble-w settings nil))
  ([progress bubble-w settings extra]
   (let [raw-iterations
         (or (:iterations progress) [])

         iterations
         (if (vector? raw-iterations) raw-iterations (vec raw-iterations))

         content-w
         (max 10 (- (long bubble-w) 4))

         {:keys [now-ms turn-start-ms cancelling? session-id session-turn-id detail-expansions
                 viewport-rows pending-sends]}
         extra

         now-ms
         (long (or now-ms (System/currentTimeMillis)))

         elapsed-ms
         (when turn-start-ms (max 0 (- now-ms (long turn-start-ms))))

         elapsed-str
         (or (vis/format-duration elapsed-ms) "0ms")

         ;; The ONLY per-tick-volatile row: it embeds the animated spinner
         ;; glyph + elapsed clock, both a pure function of `now-ms`. Always
         ;; non-blank (glyph + phase text), which is what lets the body split
         ;; below stay byte-identical to a single-pass coalesce.
         spinner-line
         (str (spinner-frame now-ms)
              "  "
              (progress-phase iterations cancelling?)
              "...  "
              elapsed-str
              "  /  Esc to cancel")

         line-entry
         (fn [line]
           {:line line :meta nil})

         ;; --- Content body cache (everything EXCEPT the spinner row) ----------
         ;; The heavy per-tick work — `trace-render-entries` (re-walks every
         ;; iteration), `queued-progress-entries`, `coalesce-bubble-blanks`, and
         ;; the per-line `strip-paint-markers-line` that builds `:text` — all
         ;; scale with the WHOLE bubble and dominated the 80ms live-frame
         ;; profile as a stream grows. NONE of it depends on the spinner clock:
         ;; `trace-render-entries` ignores `:now-ms`, so the body only changes
         ;; when the ITERATIONS / queue / geometry / settings / expansions
         ;; change. Memoize it by CONTENT and splice the freshly-animated
         ;; spinner row in below, so a bare spinner tick is O(1) + one stripped
         ;; line instead of O(bubble).
         ;;
         ;; Coalesce equivalence: `coalesce-bubble-blanks` is a forward fold and
         ;; the spinner row is always non-blank, so it forms a fold boundary.
         ;; Coalescing the prefix (trace + one blank) and the queue
         ;; INDEPENDENTLY, then splicing the spinner between them, produces the
         ;; exact same rows as coalescing the whole `(concat prefix [spinner]
         ;; queued)` in one pass (a following element never changes an earlier
         ;; element's keep/drop decision).
         body
         (cached*
           [::progress-body (long content-w) (mapv iteration-fingerprint iterations)
            (boolean (get settings :show-thinking true))
            (boolean (get settings :show-iterations true))
            (boolean (get settings :show-silent false)) session-id session-turn-id
            (turn-detail-expansions-key {:section :iteration
                                         :session-id session-id
                                         :session-turn-id session-turn-id
                                         :detail-expansions detail-expansions})
            (mapv :text (vec (or pending-sends [])))]
           (fn []
             (let [trace-entries
                   (trace-render-entries {:iterations iterations
                                          :content-w content-w
                                          :settings settings
                                          :now-ms now-ms
                                          :viewport-rows viewport-rows
                                          :session-id session-id
                                          :session-turn-id session-turn-id
                                          :detail-expansions detail-expansions
                                          :live? true})

                   queued-entries
                   (queued-progress-entries pending-sends content-w)

                   ;; Top margin invariant: the spinner row always has ONE blank
                   ;; line above it inside the bubble, whether or not any
                   ;; iterations have been recorded yet. Without this the iter-0
                   ;; "Vis is calling the provider" state sits flush against the
                   ;; bubble's top border while every subsequent state (iter>=1,
                   ;; where trace-entries naturally end with a blank) gets a row
                   ;; of breathing room - a visible jump the moment the first
                   ;; iteration lands. Keeping the blank in both branches makes
                   ;; the bubble height transition smooth and the spinner
                   ;; vertically anchored.
                   prefix-entries
                   (if (seq trace-entries)
                     (conj (vec trace-entries) (line-entry ""))
                     [(line-entry "")])

                   prefix
                   (vec (coalesce-bubble-blanks prefix-entries))

                   queued
                   (vec (coalesce-bubble-blanks queued-entries))]

               {:prefix-lines (mapv :line prefix)
                :prefix-meta (mapv :meta prefix)
                :prefix-text (mapv (comp strip-paint-markers-line :line) prefix)
                :queued-lines (mapv :line queued)
                :queued-meta (mapv :meta queued)
                :queued-text (mapv (comp strip-paint-markers-line :line) queued)})))]

     ;; Splice the fresh spinner row between the cached prefix and queue. This
     ;; mirrors `entries->payload` exactly: `:text` is the newline-join of the
     ;; per-line `strip-paint-markers-line` outputs, in the same order as
     ;; `:lines`.
     {:lines (-> (:prefix-lines body)
                 (conj spinner-line)
                 (into (:queued-lines body)))
      :line-meta (-> (:prefix-meta body)
                     (conj nil)
                     (into (:queued-meta body)))
      :text (str/join "\n"
                      (-> (:prefix-text body)
                          (conj (strip-paint-markers-line spinner-line))
                          (into (:queued-text body))))})))
(defn progress->text
  "Build the text body of the live progress placeholder bubble."
  ([progress bubble-w settings] (progress->text progress bubble-w settings nil))
  ([progress bubble-w settings extra]
   (:text (progress->lines-data progress bubble-w settings extra))))
;;; ── Markdown table parsing ───────────────────────────────────────────────
;; **prefix:** value
(defn- assert-canonical-ir!
  "STRICT: bubble layout takes canonical answer-IR (`[:ir & nodes]`)
   or nil only. Strings/Hiccup/EDN are programmer bugs at this layer
   — the IR boundary is upstream."
  [ir]
  (when-not (or (nil? ir) (and (vector? ir) (= :ir (first ir))))
    (throw (ex-info "format-answer-with-thinking-data: answer must be canonical [:ir ...] (or nil)"
                    {:got-type (some-> ir
                                       class
                                       .getName)
                     :got-preview (let [s (pr-str ir)]
                                    (subs s 0 (min 200 (count s))))}))))
(defn- ir-non-empty? [ir] (and (vector? ir) (= :ir (first ir)) (> (count ir) 2)))
(defn- provider-error-answer?
  [ir]
  (boolean (and (vector? ir) (= :ir (first ir)) (true? (get-in ir [1 :vis/provider-error])))))
(defn format-answer-with-thinking-data*
  "Uncached implementation. Returns `{:text :lines :line-meta}` so the
   bubble painter can keep clickable summary-row metadata aligned with
   the already-wrapped lines.

   STRICT: `answer` is canonical answer-IR (`[:ir & nodes]`) or nil."
  [answer trace bubble-w settings _confidence cancelled? opts]
  (let [content-w
        (max 10 (- (long bubble-w) 4))

        fill-w
        (max 1 (dec (long content-w)))

        line-entry
        (fn [line]
          {:line line :meta nil})

        _
        (assert-canonical-ir! answer)

        suppress-trace?
        (provider-error-answer? answer)

        trace-entries
        (trace-render-entries {:iterations trace
                               :content-w content-w
                               :settings settings
                               :session-id (:session-id opts)
                               :session-turn-id (:session-turn-id opts)
                               :detail-expansions (:detail-expansions opts)
                               :suppress-trace? suppress-trace?})

        ;; IR walker emits painter-ready entries directly. No
        ;; markdown round-trip, no `markdown->entries` rebuild.
        ans-entries
        (if (ir-non-empty? answer)
          (vec (ir-tui/ir->entries answer
                                   (max 1 (- (long fill-w) 2))
                                   {:session-id (:session-id opts)
                                    :session-turn-id (:session-turn-id opts)
                                    :detail-expansions (:detail-expansions opts)
                                    :section :answer}))
          [])

        ans-pad
        (line-entry (str answer-pad-marker ""))

        cancel-text
        (if (ir-non-empty? answer) (str/trim (vis/extract-text answer)) "Cancelled by user.")

        ;; Wrap each cancel body line in INLINE_ITALIC sentinels so the
        ;; painter applies italic on top of the `cancelled-fg` color.
        ;; The Vis role label already paints muted; the body row stayed
        ;; plain text and looked like a normal answer in the wrong color.
        cancel-rows
        (mapv (fn [line]
                (line-entry (str p/INLINE_ITALIC_ON line p/INLINE_ITALIC_OFF)))
              (wrap-text cancel-text (max 1 (- (long fill-w) 2))))

        ;; Answer layout shape mirrors code blocks:
        ;;   neutral blank row = outside top margin (unless the trace
        ;;                       already ended with a neutral margin row)
        ;;   answer-pad row    = inside top padding on answer bg answer rows
        ;;   answer-pad row    = inside bottom padding on answer bg
        ;; A code-bearing iteration often already ends with a neutral
        ;; `iteration-pad-marker`; a thinking-only iteration ends with
        ;; a thinking-bg pad, so it still needs the neutral answer margin.
        has-trace?
        (seq trace-entries)

        neutral-margin-entry?
        (fn [entry]
          (let [line (:line entry)]
            (or (= "" line) (= iteration-pad-marker line))))

        recap-entry?
        (fn [entry]
          (= :recap (get-in entry [:meta :kind])))

        answer-top-margin
        (when-not (and has-trace?
                       (or (neutral-margin-entry? (peek trace-entries))
                           (recap-entry? (peek trace-entries))))
          (line-entry ""))

        cancel-block
        (vec (concat (when answer-top-margin [answer-top-margin]) cancel-rows [(line-entry "")]))

        answer-block
        (if has-trace?
          (cond-> []
            answer-top-margin
            (conj answer-top-margin)

            :always
            (conj ans-pad)

            :always
            (into ans-entries)

            :always
            (conj ans-pad))
          (-> [(line-entry "")]
              (cond->
                :always
                (into ans-entries))))

        trailer
        ;; A cancelled turn that produced NO real answer shows the flat
        ;; italic "Cancelled by user." placeholder. But a turn cancelled
        ;; AFTER the model already wrote a genuine markdown answer must
        ;; render that answer through the NORMAL markdown block (headings /
        ;; lists / bold intact) — the cancel-block flattens it via
        ;; `extract-text` + italic, which is what made real answers read as
        ;; "italics shit" instead of markdown. Bubble-level dim still applies.
        (if (and cancelled? (not (ir-non-empty? answer))) cancel-block answer-block)

        entries
        (if has-trace? (vec (concat trace-entries trailer)) (vec trailer))

        ;; One more coalesce pass across the WHOLE bubble (trace +
        ;; trailer). The trace half is already collapsed by
        ;; `trace-render-entries`, but the seam between the last
        ;; trace entry and the first trailer entry (answer-top
        ;; margin / answer-pad / cancel block) can still stack
        ;; blanks. Same exempt rule for THINKING pad rows.
        entries
        (vec (coalesce-bubble-blanks entries))]

    (entries->payload entries)))
(defn format-answer-with-thinking*
  [answer trace bubble-w settings confidence cancelled?]
  (:text
    (format-answer-with-thinking-data* answer trace bubble-w settings confidence cancelled? nil)))
(defn format-answer-with-thinking-data
  ([answer trace bubble-w]
   (format-answer-with-thinking-data answer trace bubble-w nil nil false nil))
  ([answer trace bubble-w settings]
   (format-answer-with-thinking-data answer trace bubble-w settings nil false nil))
  ([answer trace bubble-w settings confidence]
   (format-answer-with-thinking-data answer trace bubble-w settings confidence false nil))
  ([answer trace bubble-w settings confidence cancelled?]
   (format-answer-with-thinking-data answer trace bubble-w settings confidence cancelled? nil))
  ([answer trace bubble-w settings confidence cancelled? opts]
   (cached* [::fawt-data (System/identityHashCode answer) (System/identityHashCode trace)
             (long bubble-w) (boolean (get settings :show-thinking true))
             (boolean (get settings :show-iterations true))
             (boolean (get settings :show-silent false)) confidence (boolean cancelled?)
             (:session-turn-id opts) (turn-detail-expansions-key opts)
             ;; tail-lines opt switches to the back-walking renderer;
             ;; must be in the cache key so a tail-pinned bubble's
             ;; tail-N result doesn't shadow the same bubble's full
             ;; render after the user scrolls up.
             (:tail-lines opts)]
            #(format-answer-with-thinking-data* answer
                                                trace
                                                bubble-w
                                                settings
                                                confidence
                                                cancelled?
                                                opts))))
(defn format-answer-with-thinking
  ([answer trace bubble-w] (format-answer-with-thinking answer trace bubble-w nil nil false nil))
  ([answer trace bubble-w settings]
   (format-answer-with-thinking answer trace bubble-w settings nil false nil))
  ([answer trace bubble-w settings confidence]
   (format-answer-with-thinking answer trace bubble-w settings confidence false nil))
  ([answer trace bubble-w settings confidence cancelled?]
   (format-answer-with-thinking answer trace bubble-w settings confidence cancelled? nil))
  ([answer trace bubble-w settings confidence cancelled? opts]
   (:text
     (format-answer-with-thinking-data answer trace bubble-w settings confidence cancelled? opts))))
(defn- user-prompt-margin-entry?
  [entry]
  ;; Reserved inline-image rows LOOK blank but must survive trimming — the
  ;; graphics sequence is painted over them after the delta refresh.
  (if (#{:image :image-pad} (:kind (:meta entry)))
    false
    (let [visible (strip-paint-markers-line (:line entry))]
      (or (str/blank? visible) (boolean (re-matches #"^\s*│\s*$" visible))))))
(defn- trim-user-prompt-margin-entries
  [entries]
  (let [trimmed-leading
        (vec (drop-while user-prompt-margin-entry? entries))

        trimmed-trailing
        (vec (reverse (drop-while user-prompt-margin-entry? (reverse trimmed-leading))))]

    trimmed-trailing))
(defn- paste-code-block?
  "True for a `vis-paste` code node — the collapsed-transcript shape
   `input/collapse-paste-placeholders` emits for a `[Pasted #N: ...]`
   token: `[:code {:lang \"vis-paste\"} \"<token>\\n<payload>\"]`."
  [node]
  (and (vector? node) (= :code (first node)) (= "vis-paste" (:lang (second node)))))

(defn- image-code-block?
  "True for a `vis-image` code node — the collapsed-transcript shape
   `input/collapse-paste-placeholders` emits for an `[Image #N: ...]`
   token. Body lines: summary, absolute path, mime, `WxH`, size-label."
  [node]
  (and (vector? node) (= :code (first node)) (= "vis-image" (:lang (second node)))))

(defn- disclosure-code-block? [node] (or (paste-code-block? node) (image-code-block? node)))

(defn- image-block-parts
  "Parse a `vis-image` node body into `{:summary :path :mime :width :height
   :size-label}`. Missing fields come back nil."
  [node]
  (let [lines
        (str/split (str (nth node 2 "")) #"\n" -1)

        [summary path mime dims size-label]
        lines

        [w h]
        (when (seq dims) (str/split (str dims) #"x"))

        ascii
        (let [a (str/join "\n" (drop 5 lines))]
          (when-not (str/blank? a) a))]

    {:summary (str/trim (str summary))
     :path (str path)
     :mime (when (seq mime) mime)
     :width (some-> w
                    str/trim
                    not-empty
                    parse-long)
     :height (some-> h
                     str/trim
                     not-empty
                     parse-long)
     :size-label (not-empty (str/trim (str size-label)))
     :ascii ascii}))

(defn- paste-block-parts
  "Split a `vis-paste` code node body into `[summary payload]` — the
   collapse pass writes the `[Pasted #N: ...]` token as the FIRST body
   line, the verbatim payload underneath."
  [node]
  (let [body
        (str (nth node 2 ""))

        nl
        (.indexOf body "\n")]

    (if (neg? nl) [body ""] [(subs body 0 nl) (subs body (inc nl))])))

(defn- paste-disclosure-entries
  "Render one `vis-paste` block as a collapsible disclosure: the
   `[Pasted #N: ...]` token is the chevron summary row (`▸`/`▾` +
   `:toggle-details` click meta), the verbatim payload the body shown
   only when expanded. Collapsed by default so a pasted wall stays a
   one-liner. A trailing blank row gives the next content breathing
   room."
  [node content-w {:keys [session-id session-turn-id detail-expansions] :as opts}]
  (let [[summary payload]
        (paste-block-parts node)

        summary
        (str/trim summary)

        id
        (or (some-> (re-find #"#(\d+)" summary)
                    second)
            "0")

        node-id
        (detail-node-id
          {:session-turn-id session-turn-id :section :user :kind :paste :details-path [id]})

        expanded?
        (detail-expanded? detail-expansions session-id node-id false)

        header
        (detail-summary-entries {:marker md-summary-marker
                                 :max-w content-w
                                 :summary summary
                                 :collapsed? (not expanded?)
                                 :session-id session-id
                                 :node-id node-id})

        body
        (when expanded?
          (tag-copy-block-body
            (vec (ir-tui/ir->entries [:ir {} [:code {:wrap? true} payload]] content-w opts))
            node-id
            payload))]

    (vec (concat header body))))

(def ^:private image-max-cols
  "Cap the inline-image box width so a big screenshot doesn't consume the
   whole bubble; matches pi's default `maxWidthCells`."
  60)

(defn- image-disclosure-entries
  "Render one `vis-image` block as a collapsible disclosure: the
   `[Image #N: ...]` token is the chevron summary row; expanding it reserves
   `rows` blank body rows whose FIRST row carries `:kind :image` meta so the
   screen loop can paint the actual picture (Kitty/iTerm2 graphics) over them
   AFTER Lanterna's delta refresh. Terminals without inline-image support (or
   an unreadable file) fall back to a single descriptive text row."
  [node content-w {:keys [session-id session-turn-id detail-expansions] :as opts}]
  (let [{:keys [summary path mime width height size-label ascii]}
        (image-block-parts node)

        id
        (or (some-> (re-find #"#(\d+)" summary)
                    second)
            "0")

        node-id
        (detail-node-id {:session-turn-id session-turn-id
                         :section (:image-section opts :user)
                         :kind :image
                         :details-path [id]})

        expanded?
        (detail-expanded? detail-expansions
                          session-id
                          node-id
                          (:image-default-expanded? opts (timg/graphical-terminal?)))

        header
        (detail-summary-entries {:marker md-summary-marker
                                 :max-w content-w
                                 :summary summary
                                 :collapsed? (not expanded?)
                                 :session-id session-id
                                 :node-id node-id})

        can-draw?
        (and expanded? (timg/graphical-terminal?) width height)

        body
        (when expanded?
          (if can-draw?
            (let [box
                  (timg/cell-size {:w width :h height}
                                  (min (long image-max-cols) (max 1 (long content-w)))
                                  40)

                  rows
                  (max 1 (long (:rows box)))

                  img
                  {:path path :mime mime :cols (:cols box) :rows rows}]

              ;; First reserved row carries the paint meta; the rest
              ;; are blanks the graphics sequence spans over. Every
              ;; row is tagged so `trim-user-prompt-margin-entries`
              ;; doesn't mistake the reserved box for trailing margin
              ;; and collapse the space the image needs.
              (into [{:line "" :meta {:kind :image :img img :node-id (str node-id)}}]
                    (repeat (dec rows) {:line "" :meta {:kind :image-pad}})))
            ;; Fallback: describe the image so headless / unsupported
            ;; terminals still see what was attached.
            (let [dims
                  (when (and width height) (str width "×" height))

                  desc
                  (str (or (not-empty (last (str/split (str path) #"/"))) "image")
                       (when dims (str "  " dims))
                       (when size-label (str "  " size-label)))

                  ir
                  (if ascii [:ir {} [:p {} desc] [:code {} ascii]] [:ir {} [:p {} desc]])]

              (tag-copy-block-body (vec (ir-tui/ir->entries ir content-w {})) node-id path))))]

    (vec (concat header body))))

(defn- paste-aware-ir->entries
  "`ir-tui/ir->entries`, but each top-level `vis-paste` code block becomes
   a collapsible paste disclosure instead of an inline code chip. Falls
   back to the plain walker (zero overhead) when the IR carries no paste."
  [answer content-w opts]
  (let [blocks (vec (drop 2 answer))]
    (if (not-any? disclosure-code-block? blocks)
      (vec (ir-tui/ir->entries answer content-w opts))
      (->> (partition-by disclosure-code-block? blocks)
           (mapcat (fn [run]
                     (if (disclosure-code-block? (first run))
                       (mapcat (fn [node]
                                 (if (image-code-block? node)
                                   (image-disclosure-entries node content-w opts)
                                   (paste-disclosure-entries node content-w opts)))
                               run)
                       (ir-tui/ir->entries (into [:ir {}] run) content-w opts))))
           vec))))

(defn format-answer-markdown-data*
  [answer bubble-w opts]
  (assert-canonical-ir! answer)
  (let [content-w
        (max 10 (- (long bubble-w) 4))

        raw-entries
        (if (ir-non-empty? answer) (paste-aware-ir->entries answer content-w opts) [])

        ;; Assistant answers keep a blank margin row so the first line of
        ;; answer content is never flush against the top of the bubble or the
        ;; bottom of a preceding user message. User prompts already have their
        ;; own bubble padding; adding an answer margin there creates a visible
        ;; blank line inside the quoted prompt block.
        entries
        (if (= :user (:section opts))
          (trim-user-prompt-margin-entries raw-entries)
          (into [{:line "" :meta nil}] raw-entries))]

    (entries->payload entries)))
(defn format-answer-markdown*
  [answer bubble-w]
  (:text (format-answer-markdown-data* answer bubble-w nil)))
(defn format-answer-markdown-data
  ([answer bubble-w] (format-answer-markdown-data answer bubble-w nil))
  ([answer bubble-w opts]
   (cached* [::fam-data (System/identityHashCode answer) (long bubble-w) (:session-turn-id opts)
             (relevant-detail-expansions-key opts)
             ;; see comment in format-answer-with-thinking-data
             (:tail-lines opts)]
            #(format-answer-markdown-data* answer bubble-w opts))))
(defn format-answer-markdown
  ([answer bubble-w] (format-answer-markdown answer bubble-w nil))
  ([answer bubble-w opts] (:text (format-answer-markdown-data answer bubble-w opts))))
;;; ── Messages area (bubble-based) ───────────────────────────────────────────
;; -- Messages-area layout constants ----------------------------------------
;;
;; PUBLIC because `screen.clj` must compute bubble width with the same
;; gutter math the painter uses; if the two layers disagree by even one
;; column, `format-iteration-entry` sizes labels (`BLOCK 3`, `FINAL ANSWER`)
;; for one bubble-w while `draw-chat-bubble!` paints
;; into a different bubble-w and the right-aligned labels wrap onto
;; two lines.
;;
;; Single source of truth lives here; the consumer in `screen.clj` is
;; required to derive its own width from `MESSAGE_SIDE_PAD` rather than a
;; literal.
(def ^:const MESSAGE_MARGIN_TOP 1)
;; rows above first message
(def ^:const MESSAGE_MARGIN_BOTTOM 1)
;; rows below last message
(def ^:const MESSAGE_MARGIN_LEFT 2)
;; cols left gutter
(def ^:const MESSAGE_MARGIN_RIGHT 3)
;; cols right gutter (1 col padding before
;; the scrollbar at cols-2, then 1 col edge after).
(def ^:const MESSAGE_SIDE_PAD (+ MESSAGE_MARGIN_LEFT MESSAGE_MARGIN_RIGHT))
;; ^ Convenience: the total horizontal gutter consumed on each row.
;; `bubble-w = cols - MESSAGE_SIDE_PAD`. Both this file's painter and
;; `screen.clj`'s height calculator MUST use this exact derivation.
(defn draw-messages-area!
  "Draw structured chat messages as left-aligned blocks inside a clean,
   border-less scrolling area.

   `layout` is a virtual-layout plan produced by
   `com.blockether.vis.ext.channel-tui.virtual/layout`. Carries
   `{:total-h :eff-scroll :visible}` - the painter only iterates
   `:visible` (the messages whose interval intersects the viewport),
   so off-screen bubbles never trigger `format-answer-with-thinking`
   / `wrap-text` / `markdown->lines`. That's the single largest
   user-visible win for cold-opening long sessions: cold paint
   drops from O(N x trace-size) to O(visible x trace-size).

   No outer box, no title bar - just a vertical column of messages with
   generous side gutters. The right gutter doubles as scrollbar space
   when the session overflows. The session title (if any) is
   surfaced via the input-box bottom status line, not here."
  [^TextGraphics g layout box-top box-bottom cols]
  (let [box-top
        (long box-top)

        box-bottom
        (long box-bottom)

        cols
        (long cols)

        text-top
        (+ box-top (long MESSAGE_MARGIN_TOP))

        inner-h
        (max 0 (- box-bottom text-top (long MESSAGE_MARGIN_BOTTOM)))

        bubble-w
        (max 1 (- cols (long MESSAGE_SIDE_PAD)))

        total-h
        (long (:total-h layout))

        eff-scroll
        (long (:eff-scroll layout))

        visible
        (:visible layout)]

    ;; Background fill (no border, no title bar).
    (p/set-colors! g t/text-fg t/terminal-bg)
    (p/fill-rect! g 0 box-top cols (max 0 (- box-bottom box-top)))
    ;; Open a new staging pass for click regions. Bubbles register
    ;; their chrome rows during paint; downstream painters (header,
    ;; ...) register theirs too. The published registry that the
    ;; input thread reads doesn't change until the screen calls
    ;; `cr/commit-frame!` after every painter has run - so the
    ;; input thread never lookup-misses on a half-filled buffer.
    (cr/begin-frame!)
    (let [clip (.newTextGraphics g (TerminalPosition. 0 text-top) (TerminalSize. cols inner-h))]
      (doseq [{:keys [^long top projected]} visible]
        (draw-chat-bubble! clip
                           projected
                           top
                           MESSAGE_MARGIN_LEFT
                           bubble-w
                           {:viewport-top text-top :viewport-h inner-h}))
      (let [bar-top box-top
            track-h (max 0 (- box-bottom box-top))]

        ;; Place the scrollbar inside the right gutter so it never
        ;; overlaps message content. The track spans the whole message
        ;; panel, including top/bottom breathing-room rows; otherwise a
        ;; one-row blank gap appears above the scrollbar.
        (scrollbar/draw! g
                         {:col (- cols 2)
                          :top bar-top
                          :track-h track-h
                          :total-h total-h
                          :inner-h inner-h
                          :scroll eff-scroll
                          :track-fg t/border-fg
                          :track-bg t/terminal-bg
                          :thumb-fg t/dialog-hint-key
                          :thumb-bg t/terminal-bg})))))

(defn draw-detail-labels!
  "Vim-style jump-label overlay for collapsible disclosures.

   `frozen` is the `[label region]` assignment captured ONCE when the mode
   opened (`cr/assign-labels` on that frame), read from `db` — NOT re-derived
   per frame. Freezing matters mid-turn: a live stream keeps re-registering
   `cr/current` as the trace grows, so a per-frame `assign-labels` would
   reshuffle the letters under the user's fingers and race the keypress. With a
   frozen map the letter→fold binding is stable. Each badge is re-anchored to
   its fold's CURRENT painted position — the region is matched by
   `[session-id node-id]` against this frame's `cr/current`, so the letter
   tracks the fold as the transcript scrolls and is simply dropped when the
   fold scrolls off. Falls back to a live `assign-labels` when no frozen set is
   present (the command-palette entry can open the mode from a dialog frame
   that had nothing to freeze). Inactive / empty ⇒ paints nothing.

   Called by the full-frame painter right AFTER `cr/commit-frame!`, so
   `cr/current` holds this frame's freshly-registered regions. The input
   handler resolves a typed letter against the SAME frozen map, so a badge and
   its keypress always point at the same fold without shared mutable state."
  [^TextGraphics g active? frozen]
  (when active?
    (let [labels
          (if (seq frozen) frozen (cr/assign-labels (cr/current)))

          live-by-node
          (reduce (fn [m r]
                    (if (= :toggle-details (:kind r)) (assoc m [(:session-id r) (:node-id r)] r) m))
                  {}
                  (cr/current))]

      (doseq [[label region]
              labels

              :let [live
                    (get live-by-node [(:session-id region) (:node-id region)])]
              :when live]

        (let [{:keys [bounds]} live]
          ;; Loud avy-style lead badge: ground-colored text on the saturated
          ;; `warning-fg` fill so the label pops in EVERY theme (the old pairing
          ;; painted terminal-bg on the pale `warning-bg`, ~1.1:1 contrast — invisible
          ;; on Solarized). warning-fg gives 8:1+ AAA contrast light or dark. Letters
          ;; are upper-cased so they stand out of the surrounding lowercase prose;
          ;; the input handler lower-cases the keypress, so the match still holds.
          (p/set-colors! g t/terminal-bg t/warning-fg)
          (p/styled g
                    [p/BOLD]
                    (p/put-str! g (:col bounds) (:row bounds) (.toUpperCase ^String label))))))))
