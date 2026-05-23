(ns com.blockether.vis.ext.channel-tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]

            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.links :as links]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.format :as fmt])
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
(def ^:private ^:const max-link-ref-text-chars 24000)

(defn- make-fmt-cache ^LinkedHashMap []
  (proxy [LinkedHashMap] [64 0.75 true] ;; true = access-order (LRU)
    (removeEldestEntry [_eldest]
      (> (.size ^LinkedHashMap this) fmt-cache-cap))))

(defonce ^:private ^LinkedHashMap fmt-cache (make-fmt-cache))

(defn- cached* [k compute-fn]
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
        (locking fmt-cache
          (or (.get fmt-cache k)
            (do (.put fmt-cache k v) v)))))))

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
  (boolean
    (or (#{\u200B \u200C \u200D \uFEFF} ch)
      (<= (int \u2060) (int ch) (int \u206F))
      (<= (int \uE001) (int ch) (int \uE02D)))))

(defn- split-structural-line-marker
  [^String line]
  (when (and (string? line) (pos? (count line)))
    (let [ch (.charAt line 0)]
      (when (structural-line-marker? ch)
        [(subs line 0 1) (subs line 1)]))))

(def ^:private inline-style-order [:bold :italic :strike :code :link])

(def ^:private inline-style->sentinels
  {:bold   [p/INLINE_BOLD_ON p/INLINE_BOLD_OFF]
   :italic [p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF]
   :strike [p/INLINE_STRIKE_ON p/INLINE_STRIKE_OFF]
   :code   [p/INLINE_CODE_ON p/INLINE_CODE_OFF]
   :link   [p/INLINE_LINK_ON p/INLINE_LINK_OFF]})

(def ^:private inline-sentinel->transition
  {p/INLINE_BOLD_ON    [:on :bold]
   p/INLINE_BOLD_OFF   [:off :bold]
   p/INLINE_ITALIC_ON  [:on :italic]
   p/INLINE_ITALIC_OFF [:off :italic]
   p/INLINE_STRIKE_ON  [:on :strike]
   p/INLINE_STRIKE_OFF [:off :strike]
   p/INLINE_CODE_ON    [:on :code]
   p/INLINE_CODE_OFF   [:off :code]
   p/INLINE_LINK_ON    [:on :link]
   p/INLINE_LINK_OFF   [:off :link]})

(defn- inline-state-after
  [active ^String line]
  (loop [idx 0
         active (vec active)]
    (if (>= idx (.length line))
      active
      (let [token (subs line idx (inc idx))]
        (if-let [[op style] (inline-sentinel->transition token)]
          (recur (inc idx)
            (case op
              :on  (if (some #{style} active) active (conj active style))
              :off (vec (remove #{style} active))))
          (recur (inc idx) active))))))

(defn- active-inline-prefix
  [active]
  (let [active-set (set active)]
    (apply str
      (keep (fn [style]
              (when (active-set style)
                (first (inline-style->sentinels style))))
        inline-style-order))))

(defn- active-inline-suffix
  [active]
  (let [active-set (set active)]
    (apply str
      (keep (fn [style]
              (when (active-set style)
                (second (inline-style->sentinels style))))
        (reverse inline-style-order)))))

(defn- rebalance-inline-sentinel-wraps
  "When a markdown inline span crosses a visual wrap boundary, repeat its
   zero-width sentinels on each physical row. Otherwise a long inline code span
   starts with code styling on row 1, then every continuation paints as plain
   prose until the final row's CODE_OFF. Added sentinels are zero-width, so
   wrapping width is unchanged."
  [lines]
  (loop [remaining (seq lines)
         active    []
         out       []]
    (if-let [line (first remaining)]
      (let [active' (inline-state-after active line)
            line'   (str (active-inline-prefix active) line (active-inline-suffix active'))]
        (recur (next remaining) active' (conj out line')))
      out)))

(defn- wrap-unmarked-line-chunks
  [^String line ^long max-width]
  (if (<= (p/display-width line) max-width)
    [line]
    (loop [remaining line
           acc       []]
      (if (<= (p/display-width remaining) max-width)
        (conj acc remaining)
        ;; `cut` is the CHAR index that bounds the longest prefix fitting
        ;; in `max-width` COLUMNS without splitting a grapheme. Char-index,
        ;; not column, because we still need to slice `remaining`.
        (let [cut     (p/col-prefix-end remaining max-width)
              chunk   (subs remaining 0 cut)
              last-sp (str/last-index-of chunk " ")]
          (if (and last-sp (pos? last-sp))
            ;; Break at word boundary
            (recur (subs remaining (inc (int last-sp)))
              (conj acc (subs remaining 0 (int last-sp))))
            ;; No space found - hard break at column boundary
            (recur (subs remaining cut)
              (conj acc chunk))))))))

(defn- wrap-unmarked-line
  [^String line ^long max-width]
  (let [lines (wrap-unmarked-line-chunks line max-width)]
    (if (> (count lines) 1)
      (rebalance-inline-sentinel-wraps lines)
      lines)))

(defn- wrap-line-preserving-marker
  [line max-width]
  (if-let [[marker body] (split-structural-line-marker line)]
    (if (<= (p/display-width line) max-width)
      [line]
      (mapv #(str marker %) (wrap-unmarked-line body max-width)))
    (wrap-unmarked-line line max-width)))

(defn- sgr-escape-end
  ^long [^String s ^long esc-idx]
  (let [n (.length s)]
    (if-not (and (< (inc esc-idx) n)
              (= \[ (.charAt s (inc esc-idx))))
      -1
      (let [m-idx (str/index-of s "m" (+ esc-idx 2))]
        (if (and m-idx
              (every? #(or (Character/isDigit ^char %)
                         (= \; %))
                (subs s (+ esc-idx 2) m-idx)))
          (long m-idx)
          -1)))))

(defn- truncate-ansi-cols
  "Like `p/truncate-cols`, but preserves ANSI SGR escapes as zero-width.
   Zprint emits ANSI-colored Clojure code; feeding those rows directly to
   Lanterna's grapheme splitter throws on ESC (0x1b), which can blank the
   whole TUI before first paint."
  ^String [s ^long max-cols]
  (let [s        (str s)
        max-cols (max 0 max-cols)]
    (cond
      (zero? max-cols) ""
      (not (str/includes? s "\u001b")) (p/truncate-cols s max-cols)
      :else
      (let [n  (.length s)
            sb (StringBuilder.)]
        (loop [i 0 used 0]
          (cond
            (>= i n) (.toString sb)
            (>= used max-cols) (.toString sb)
            :else
            (let [esc-idx (or (str/index-of s "\u001b" i) n)]
              (if (< i esc-idx)
                (let [chunk (subs s i esc-idx)
                      w     (p/display-width chunk)]
                  (if (<= (+ used w) max-cols)
                    (do (.append sb chunk)
                      (recur esc-idx (+ used w)))
                    (do (.append sb (p/truncate-cols chunk (- max-cols used)))
                      (.toString sb))))
                (let [m-idx (sgr-escape-end s esc-idx)]
                  (if (neg? m-idx)
                    ;; Unknown control escape: never let it reach Lanterna.
                    ;; Render it visibly as a middle dot and continue.
                    (let [w 1]
                      (if (<= (+ used w) max-cols)
                        (do (.append sb /)
                          (recur (inc esc-idx) (+ used w)))
                        (.toString sb)))
                    (do (.append sb s esc-idx (inc m-idx))
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

(defn- clipped-lines
  "Memoized clipping for prewrapped painter lines. Large trace bubbles can
   carry hundreds of rows plus occasional huge raw tool-output rows; scrolling
   must not re-clip every off-screen row on each frame."
  [raw-lines content-w]
  (cached* [::clipped-lines
            (System/identityHashCode raw-lines)
            (long content-w)]
    #(clip-lines-preserving-markers raw-lines content-w)))

(defn wrap-text*
  "Uncached implementation. Prefer `wrap-text` everywhere except inside
   `wrap-text` itself."
  [text max-width]
  (if (or (str/blank? text) (<= max-width 0))
    [""]
    (let [input-lines (str/split-lines text)]
      (into [] (mapcat #(wrap-line-preserving-marker % max-width)) input-lines))))

(defn wrap-text
  "Memoized `wrap-text*`. Keyed by source-string identity so finalized
   message texts hit the cache across frames (their string instance is
   stable on the immutable message map)."
  [text max-width]
  (cached* [::wrap (System/identityHashCode text) (long max-width)]
    #(wrap-text* text max-width)))

(defn wrap-messages
  "Wrap a vec of display lines to fit within max-width. Returns flat vec of wrapped lines."
  [messages max-width]
  (into [] (mapcat #(wrap-text % max-width)) messages))

(def ^:private phi 1.618)
(def ^:private dialog-chrome-w 4)  ;; border(1) + pad(1) each side
(def ^:private dialog-chrome-h 6)  ;; top-border + title-bar + top-sep + ... + bot-sep + hint + bottom-border

(defn golden-dialog-size
  "Compute golden-ratio dialog size [w h] sized to fit content.
   `content-w` = widest content line (chars). `content-h` = content line count.
   Only expands WIDTH when content is too narrow for golden ratio.
   Never inflates height beyond what content needs.
   Clamped to terminal bounds."
  [cols rows content-w content-h]
  (let [;; Minimum box size to contain content + chrome
        min-w (+ content-w dialog-chrome-w)
        min-h (+ content-h dialog-chrome-h)
        ;; Only widen to approach φ - never heighten
        golden-w (int (* min-h phi))
        box-w (max min-w golden-w)
        box-h min-h
        ;; Clamp to terminal
        box-w (min box-w (- cols 4))
        box-h (min box-h (- rows 4))
        ;; Floor - minimum width generous enough for hint bars + labels
        box-w (max box-w 40)
        box-h (max box-h 7)]
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
            end   (min w (+ start (count label)))]
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
  ([g box-top box-bottom cols top-hint]
   (draw-box-border! g box-top box-bottom cols top-hint true))
  ([^TextGraphics g box-top box-bottom cols top-hint sides?]
   (let [inner-w (- cols 2)
         bar     (repeat-str Symbols/SINGLE_LINE_HORIZONTAL inner-w)]
     (.setForegroundColor g t/border-fg)
     (.setBackgroundColor g t/terminal-bg)

     (if sides?
       (do
         ;; Top: ┌── top-hint ──┐
         (.setCharacter g 0 (int box-top) Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
         (.putString g 1 (int box-top) (embed-in-bar bar top-hint))
         (.setCharacter g (int (dec cols)) (int box-top) Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)

         ;; Bottom: └──────┘ (plain rule - status moved to footer row).
         (.setCharacter g 0 (int box-bottom) Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
         (.putString g 1 (int box-bottom) bar)
         (.setCharacter g (int (dec cols)) (int box-bottom) Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)

         ;; Sides: │ ... │
         (doseq [row (range (inc box-top) box-bottom)]
           (.setForegroundColor g t/border-fg)
           (.setBackgroundColor g t/terminal-bg)
           (.setCharacter g 0 (int row) Symbols/SINGLE_LINE_VERTICAL)
           (.setCharacter g (int (dec cols)) (int row) Symbols/SINGLE_LINE_VERTICAL)))
       ;; Sideless variant: top + bottom rules with horizontal padding
       ;; on each end so the rule doesn't kiss the screen edges.
       ;; `pad` cols of empty space on each side; bar spans the inner
       ;; (cols - 2*pad) columns. No corners, no side rails. Top hint
       ;; embeds inside the padded bar. `:tui.input/omit-top-border`
       ;; lets footer/subtitle own that row while this painter keeps the
       ;; editor interior and bottom rule.
       (let [pad        INPUT_BORDER_HORIZONTAL_PAD
             rule-w     (max 0 (- cols (* 2 pad)))
             padded-bar (repeat-str Symbols/SINGLE_LINE_HORIZONTAL rule-w)]
         (when-not (= :tui.input/omit-top-border top-hint)
           (.putString g (int pad) (int box-top) (embed-in-bar padded-bar top-hint)))
         (.putString g (int pad) (int box-bottom) padded-bar))))))

(defn- fill-box-interior!
  "Fill the interior of a box with the standard box background."
  [^TextGraphics g box-top box-bottom cols]
  (let [inner-w  (- cols 2)
        text-top (inc box-top)
        rows     (- box-bottom box-top 1)]
    (.setForegroundColor g t/box-fg)
    (.setBackgroundColor g t/box-bg)
    (.fillRectangle g (TerminalPosition. 1 text-top) (TerminalSize. inner-w rows) \space)))

;;; ── Messages box ───────────────────────────────────────────────────────────

(defn draw-messages-box!
  "Draw bordered message area with top-anchored scrollable messages."
  [^TextGraphics g messages box-top box-bottom cols scroll]
  (let [inner-rows (- box-bottom box-top 1)
        text-top   (inc box-top)
        text-w     (- cols 4)
        total      (count messages)
        offset     (min scroll (max 0 (- total inner-rows)))
        visible    (subvec messages offset (min total (+ offset inner-rows)))]

    (draw-box-border! g box-top box-bottom cols "")
    (fill-box-interior! g box-top box-bottom cols)

    (doseq [[i message] (map-indexed vector visible)]
      (.setForegroundColor g t/box-fg)
      (.setBackgroundColor g t/box-bg)
      ;; truncate-cols handles "shorter than width" (returns input verbatim)
      ;; and column-aware truncation in one call. No min-clamp needed.
      (p/put-str! g (inc t/pad-x) (+ text-top i)
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
(def ^:private input-pad-x
  "Horizontal padding (cols left/right of text inside the input box)."

  2)

(defn input-text-w
  "Visible text width (in columns) inside the input box for a given
   terminal `cols`. Single source of truth so screen.clj can compute
   wrapped row counts and `draw-input-box!` can render with the same
   wrap point.

   The input box is SIDELESS (top/bottom rules only, no `│` rails)
   and `input-pad-x` is now 0, so the typing zone spans the full
   terminal width - `text-w` = `cols` (clamped to >=1)."
  [cols]
  (max 1 (- cols (* 2 input-pad-x))))

(defn- wrap-input-line
  "Soft-wrap one logical input line into visual segments at `text-w`
   columns. Always returns a non-empty vec (empty input -> [\"\"])."
  [^String line text-w]
  (let [text-w (max 1 text-w)
        n     (count line)]
    (if (<= n text-w)
      [line]
      (loop [start 0 acc (transient [])]
        (if (>= start n)
          (persistent! acc)
          (let [end (min n (+ start text-w))]
            (recur end (conj! acc (subs line start end)))))))))

(defn visual-rows-for-line
  "How many visual rows logical line `line` occupies when soft-wrapped
   at `text-w` cols. Empty line still counts as 1 (a typable row)."
  [^String line text-w]
  (let [text-w (max 1 text-w)
        n     (count line)]
    (cond
      (zero? n) 1
      :else     (cond-> (quot n text-w)
                  (pos? (mod n text-w)) inc))))

(defn input-visual-row-count
  "Total visual rows occupied by every logical line in the input
   editor when soft-wrapped at `text-w` cols."
  [lines text-w]
  (reduce + (map #(visual-rows-for-line % text-w) lines)))

(defn- soft-wrap-input
  "Translate the logical editor state `{:lines :crow :ccol}` into a
   visual layout suitable for rendering. Returns:
     {:visual-lines vec<str>  every visual row, top-to-bottom
      :cursor-vrow  int       visual row of the cursor
      :cursor-vcol  int       column within that visual row}"
  [{:keys [lines crow ccol]} text-w]
  (let [text-w     (max 1 text-w)
        wrapped    (mapv #(wrap-input-line % text-w) lines)
        offsets    (vec (reductions + 0 (map count wrapped)))
        line-len   (count (nth lines crow))
        seg-count  (count (nth wrapped crow))
        ;; When the cursor sits at end-of-line AND the line length is
        ;; an exact multiple of text-w, `quot ccol text-w` would land
        ;; on a non-existent next visual row. Pin it to the end of
        ;; the last segment instead so the cursor stays painted.
        seg-idx    (if (and (= ccol line-len) (pos? line-len)
                         (zero? (mod line-len text-w)))
                     (dec seg-count)
                     (quot ccol text-w))
        seg-off    (- ccol (* seg-idx text-w))]
    {:visual-lines (into [] cat wrapped)
     :cursor-vrow  (+ (nth offsets crow) seg-idx)
     :cursor-vcol  seg-off}))

(defn- input-more-hint
  "Left-edge label for the input top border when the editor has more
   visual rows than the visible input body can show. The count is the
   number of hidden visual rows, rendered as a compact `N more` cue."
  [total-visual-rows text-rows]
  (let [hidden (max 0 (- (long total-visual-rows) (long text-rows)))]
    (when (pos? hidden)
      (str " " hidden " more "))))

(defn draw-input-box!
  "Draw bordered input area with internal padding. Returns
   [cursor-col cursor-row] in screen coords.

   Long logical lines are SOFT-WRAPPED at the box width: typing past
   the right edge flows onto the next visual row instead of scrolling
   the line horizontally. The logical model (`{:lines :crow :ccol}`)
   is unchanged - wrapping is a render-time projection only.

   `hint` optionally embeds a short label in the top border. Runtime
   keybinding helpers live in `footer/draw-footer-subtitle!`, not here,
   so input/editor paint stays isolated from footer chrome.

   No left/right side rails: the input area is framed by top and bottom
   rules only, so the typing zone sits flush with the message column on
   either side and the eye tracks the prompt directly without `│`-noise."
  [^TextGraphics g input box-top text-rows cols hint]
  (let [box-bottom (+ box-top (* 2 input-pad-y) text-rows 1)
        text-top   (+ (inc box-top) input-pad-y)
        text-w     (input-text-w cols)
        {:keys [visual-lines cursor-vrow cursor-vcol]}
        (soft-wrap-input input text-w)
        v-scroll   (max 0 (- cursor-vrow (dec text-rows)))
        more-hint  (input-more-hint (count visual-lines) text-rows)]
    (draw-box-border! g box-top box-bottom cols hint false)
    (when more-hint
      (.setForegroundColor g t/border-fg)
      (.setBackgroundColor g t/terminal-bg)
      (.putString g (+ INPUT_BORDER_HORIZONTAL_PAD 2) (int box-top)
        (p/truncate-cols more-hint (max 0 (- cols (* 2 INPUT_BORDER_HORIZONTAL_PAD) 4)))))
    (fill-box-interior! g box-top box-bottom cols)

    ;; Text
    (.setForegroundColor g t/box-fg)
    (.setBackgroundColor g t/box-bg)
    (dotimes [i text-rows]
      (let [vi (+ v-scroll i)]
        (when (< vi (count visual-lines))
          (let [line (nth visual-lines vi)]
            (when (pos? (count line))
              (.putString g input-pad-x (+ text-top i)
                (subs line 0 (min (count line) text-w))))))))

    ;; Cursor position (visual coords)
    [(+ input-pad-x cursor-vcol)
     (+ text-top (- cursor-vrow v-scroll))]))

(def ^:private slash-desc-separator
  "Visual separator between the inline-code usage chip and the italic
   description. Mirrors the markdown convention `\\`/cmd\\` - description`
   so the row reads like one rendered list bullet."
  " - ")

(defn- draw-slash-suggestion-row!
  "Paint one suggestion row inside the inset span [`left`, `left+inner-w`).

   Layout (mirrors the markdown line `\\`/cmd\\` - description`):
     <selection gutter> <⎹ /cmd ⎺> <\" - \"> <italic description>

   - The first `p/SELECTION_WIDTH` cols of the row are the selection
     gutter: a `>` cursor on selected rows, blank otherwise. The
     marker itself is painted at `left` by the caller; this function
     only reserves the cols by shifting the chip start.
   - The usage is rendered as an INLINE CODE chip: the cells under
     the chip get `code-block-bg`/`code-block-fg`, padded by one
     space inside the chip on each side, so it reads as a markdown
     code span the way the rest of the chat renders backtick spans.
   - A plain ` - ` separator follows on the row's normal fg/bg.
   - The description is ITALIC, dimmed (`dialog-hint`) on every row
     — selection is signalled by the `>` glyph, not by re-coloring
     the description.
   - Truncation drops the description first; the chip always renders
     fully if at all possible."
  [^TextGraphics g row left inner-w suggestion]
  (let [;; Skip the selection gutter (`>` + 1 col margin) at the start
        ;; of the row so the chip never overlaps with the cursor.
        pad        p/SELECTION_WIDTH
        avail      (max 0 (- inner-w (* 2 pad)))
        usage-raw  (or (:slash/usage suggestion) "")
        usage      (p/truncate-cols usage-raw avail)
        usage-w    (p/display-width usage)
        ;; Inline code chip = usage padded by 1 space on each side.
        chip-w     (if (pos? usage-w) (+ usage-w 2) 0)
        sep        slash-desc-separator
        sep-w      (if (and (pos? chip-w)
                         (< (+ chip-w (count sep)) avail))
                     (count sep)
                     0)
        desc-w     (max 0 (- avail chip-w sep-w))
        desc-raw   (or (:label suggestion) "")
        desc       (when (pos? desc-w)
                     (p/truncate-cols desc-raw desc-w))
        x0         (+ left pad)
        ;; Save row colors so we can restore between segments.
        row-fg     (.getForegroundColor g)
        row-bg     (.getBackgroundColor g)]
    ;; Inline code chip for the usage — markdown `\\`/cmd\\`` look.
    (when (pos? chip-w)
      (p/set-colors! g t/code-block-fg t/code-block-bg)
      (p/fill-rect! g x0 row chip-w 1)
      (p/put-str! g (inc x0) row usage)
      (p/set-colors! g row-fg row-bg))
    ;; Plain ` - ` separator on the row's normal colors.
    (when (pos? sep-w)
      (p/put-str! g (+ x0 chip-w) row sep))
    ;; Italic description in the dim hint color. The cursor glyph in
    ;; the left margin (painted by the caller) carries the selection
    ;; cue, so the description palette is the same on every row.
    (when (and desc (pos? (p/display-width desc)))
      (p/set-fg! g t/dialog-hint)
      (p/styled g [p/ITALIC]
        (p/put-str! g (+ x0 chip-w sep-w) row desc))
      (p/set-fg! g row-fg))))

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
  [["↑↓/wheel" "select"]
   ["Enter/Tab" "complete"]])

(defn- draw-slash-title-bar!
  "Render the slash-command overlay title row.

   Layout: an accent stripe (`dialog-title-bg`) spanning
   [`left`, `left + inner-w`) carrying a BOLD label on the left and
   `[key action]` hint pairs distributed space-between across the rest
   of the row, mirroring the `draw-hint-bar!` flex idiom but on the
   title-bar accent so the header reads as something IMPORTANT, not as
   another hint line.

   `left` and `inner-w` come from the same horizontal-padding rule the
   input box uses (`INPUT_BORDER_HORIZONTAL_PAD`), so the overlay
   visually anchors to the same column span as the typing zone."
  [^TextGraphics g title-row left inner-w]
  ;; Accent stripe over the inner span only — the margin columns on
  ;; each side stay terminal-bg to read as breathing room.
  (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
  (p/fill-rect! g left title-row inner-w 1)
  (let [;; Inner content sits one col inside the accent stripe so the
        ;; BOLD label doesn't kiss the stripe edge.
        content-pad 1
        text-w  (max 0 (- inner-w (* 2 content-pad)))
        text-x0 (+ left content-pad)
        text-x1 (+ text-x0 text-w)
        ;; Items: label first, then each [key action] pair joined as
        ;; one display token so space-between distributes them evenly.
        labels  (into [slash-title-label]
                  (mapv (fn [[k a]] (str k " " a)) slash-title-hints))
        n       (count labels)
        sizes   (mapv p/display-width labels)
        total   (reduce + sizes)
        slack   (max 0 (- text-w total))
        gaps    (max 1 (dec n))
        base    (max 1 (quot slack gaps))
        extra   (max 0 (- slack (* base gaps)))]
    (loop [i 0 col text-x0]
      (when (and (< i n) (< col text-x1))
        (let [size (nth sizes i)
              gap  (if (< i (dec n))
                     (+ base (if (< i extra) 1 0))
                     0)]
          (if (zero? i)
            ;; Bold left-anchored label.
            (p/styled g [p/BOLD]
              (p/put-str! g col title-row
                (p/truncate-cols slash-title-label
                  (max 0 (- text-x1 col)))))
            ;; [key action] pair: BOLD key, plain action.
            (let [[k a] (nth slash-title-hints (dec i))
                  k-w   (p/display-width k)]
              (p/styled g [p/BOLD]
                (p/put-str! g col title-row
                  (p/truncate-cols k
                    (max 0 (- text-x1 col)))))
              (p/put-str! g (+ col k-w) title-row
                (p/truncate-cols (str " " a)
                  (max 0 (- text-x1 (+ col k-w)))))))
          (recur (inc i) (+ col size gap)))))))

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
  ([g suggestions input-top cols]
   (draw-slash-command-suggestions! g suggestions input-top cols 0))
  ([g suggestions input-top cols selected-index]
   (when (seq suggestions)
     (let [pad          INPUT_BORDER_HORIZONTAL_PAD
           left         pad
           inner-w      (max 1 (- cols (* 2 pad)))
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
           max-list     (max 0 (dec input-top))   ; reserve title only
           visible-cap  (max 0 (min 6 max-list))
           total        (count suggestions)
           selected-pos (or (some (fn [[idx suggestion]]
                                    (when (:slash/selected? suggestion) idx))
                              (map-indexed vector suggestions))
                          selected-index
                          0)
           sel          (max 0 (min (dec total) (long selected-pos)))
           first-idx    (if (pos? visible-cap)
                          (min (max 0 (- total visible-cap))
                            (max 0 (- sel (quot visible-cap 2))))
                          0)
           visible      (->> suggestions (drop first-idx) (take visible-cap))
           n            (count visible)
           have-border? (>= input-top (+ n 2))
           have-margin? (>= input-top (+ n 3))
          ;; Suggestions occupy rows: input-top - n .. input-top - 1.
          ;; Border (if present) sits one row above the first suggestion,
          ;; title sits one row above the border (or above the first
          ;; suggestion if border dropped).
           first-sug    (- input-top n)
           border-row   (when have-border? (dec first-sug))
           title-row    (cond
                          have-border? (dec border-row)
                          (pos? first-sug) (dec first-sug)
                          :else 0)
           margin-row   (when have-margin? (dec title-row))]
       (when (pos? n)
        ;; Top margin — paint the gap row in terminal-bg so any chat
        ;; content peeking through gets cleared. This is the breathing
        ;; room above the title bar.
         (when (and margin-row (>= margin-row 0))
           (p/set-colors! g t/text-fg t/terminal-bg)
           (p/fill-rect! g 0 margin-row cols 1))

        ;; Title bar (accent + flex hints).
         (draw-slash-title-bar! g title-row left inner-w)

        ;; Border under the title — single horizontal rule that
        ;; visually delimits the title from the suggestion list, on
        ;; `terminal-bg` (outside the accent) using the same width as
        ;; the input box's top/bottom rules.
         (when (and border-row (>= border-row 0))
           (p/set-colors! g t/dialog-border t/terminal-bg)
          ;; Clear margin columns to terminal-bg first so the rule
          ;; sits flush within the same column span as the title.
           (p/fill-rect! g 0 border-row cols 1)
           (p/put-str! g left border-row (p/horiz-line inner-w)))

        ;; Suggestion rows — inset by the same margin so the row body
        ;; lines up with the title accent and the input box rule.
        ;; Selection is signalled by a `>` cursor glyph in the FIRST
        ;; col of the inset row (matching the project-wide convention;
        ;; see `p/SELECTION_GLYPH`). The marker sits INSIDE the inset
        ;; body, not in the terminal-bg margin outside of it, so it
        ;; reads as part of the menu rather than floating loose.
         (doseq [[i suggestion] (map-indexed vector visible)]
           (let [row (+ first-sug i)]
            ;; Clear the full row to terminal-bg so the margin gutters
            ;; on each side don't bleed leftover paint.
             (p/set-colors! g t/text-fg t/terminal-bg)
             (p/fill-rect! g 0 row cols 1)
            ;; Body row in the normal dialog palette (no inversion).
             (p/set-colors! g t/dialog-fg t/dialog-bg)
             (p/fill-rect! g left row inner-w 1)
            ;; Cursor glyph at the FIRST col of the inset body, in
            ;; the dialog palette so it visually belongs to the row.
             (p/set-colors! g t/dialog-hint-key t/dialog-bg)
             (p/draw-selection-marker! g left row (:slash/selected? suggestion))
            ;; Inline-code chip + ` - ` + italic description.
             (p/set-colors! g t/dialog-fg t/dialog-bg)
             (draw-slash-suggestion-row! g row left inner-w suggestion)))

        ;; Right-side scrollbar when more matches exist than visible rows.
         (when (and (> total n) (pos? n) (> inner-w 2))
           (let [bar-col   (+ left (dec inner-w))
                 thumb-h   (max 1 (int (* n (/ (double n) total))))
                 denom     (max 1 (- total n))
                 track-den (max 1 (- n thumb-h))
                 thumb-top (int (* track-den (/ (double first-idx) denom)))]
             (p/set-colors! g t/dialog-border t/dialog-bg)
             (doseq [r (range n)]
               (p/set-char! g bar-col (+ first-sug r) \│))
             (p/set-colors! g t/dialog-title-bg t/dialog-bg)
             (doseq [r (range thumb-h)]
               (p/set-char! g bar-col (+ first-sug thumb-top r) \█)))))))))

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
   (let [limit-w     (or max-w (max 30 (int (* cols 0.6))))
         content-w   (- limit-w 6) ;; padding inside dialog
         raw-lines   (if (string? body) [body] body)
         text-lines  (into [] (mapcat #(wrap-text % content-w)) raw-lines)
         max-line-w  (apply max (map count (cons title text-lines)))
         box-w       (min limit-w (+ max-line-w 6))
         box-h       (+ (count text-lines) 4)
         box-left    (quot (- cols box-w) 2)
         box-top     (quot (- rows box-h) 2)
         box-right   (+ box-left box-w -1)
         box-bottom  (+ box-top box-h -1)
         inner-w     (- box-w 2)
         h-bar       (repeat-str Symbols/SINGLE_LINE_HORIZONTAL inner-w)]

     ;; Shadow
     (.setBackgroundColor g t/dialog-shadow)
     (.fillRectangle g
       (TerminalPosition. (+ box-left 2) (inc box-top))
       (TerminalSize. box-w box-h) \space)

     ;; Background
     (.setBackgroundColor g t/dialog-bg)
     (.fillRectangle g
       (TerminalPosition. box-left box-top)
       (TerminalSize. box-w box-h) \space)

     ;; Border
     (.setForegroundColor g t/dialog-border)
     (.setBackgroundColor g t/dialog-bg)
     (.setCharacter g (int box-left)  (int box-top)    Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
     (.setCharacter g (int box-right) (int box-top)    Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)
     (.setCharacter g (int box-left)  (int box-bottom) Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
     (.setCharacter g (int box-right) (int box-bottom) Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)
     (.putString g (inc box-left) box-top h-bar)
     (.putString g (inc box-left) box-bottom h-bar)
     (doseq [r (range (inc box-top) box-bottom)]
       (.setCharacter g (int box-left)  (int r) Symbols/SINGLE_LINE_VERTICAL)
       (.setCharacter g (int box-right) (int r) Symbols/SINGLE_LINE_VERTICAL))

     ;; Title
     (.setForegroundColor g t/dialog-title-bg)
     (let [title-x (+ box-left (quot (- box-w (count title)) 2))]
       (.putString g title-x (inc box-top) title))

     ;; Body (wrapped)
     (.setForegroundColor g t/dialog-fg)
     (doseq [[i line] (map-indexed vector text-lines)]
       (.putString g (+ box-left 3) (+ box-top 3 i) line)))))

;;; ── Chat bubble ────────────────────────────────────────────────────────────

;; Line markers live in primitives - aliases for local readability.
(def ^:private thinking-marker  p/MARKER_THINKING)
(def ^:private code-marker      p/MARKER_CODE)
(def ^:private result-marker    p/MARKER_RESULT)
(def ^:private sep-marker       p/MARKER_SEP)
(def ^:private code-ok-marker   p/MARKER_CODE_OK)
(def ^:private code-err-marker  p/MARKER_CODE_ERR)
(def ^:private err-result-marker p/MARKER_ERR_RESULT)
(def ^:private code-status-marker p/MARKER_CODE_STATUS)
(def ^:private duration-marker  p/MARKER_DURATION)
(def ^:private iteration-hdr-marker  p/MARKER_ITERATION_HDR)
(def ^:private recap-marker          p/MARKER_RECAP)
(def ^:private answer-sep-marker p/MARKER_ANSWER_SEP)
(def ^:private code-pad-marker   p/MARKER_CODE_PAD)
(def ^:private code-ok-pad-marker p/MARKER_CODE_OK_PAD)
(def ^:private code-err-pad-marker p/MARKER_CODE_ERR_PAD)
(def ^:private iteration-pad-marker   p/MARKER_ITERATION_PAD)
(def ^:private answer-hdr-marker p/MARKER_ANSWER_HDR)
(def ^:private answer-txt-marker p/MARKER_ANSWER_TXT)
(def ^:private answer-pad-marker p/MARKER_ANSWER_PAD)
(def ^:private md-h1-marker         p/MARKER_MD_H1)
(def ^:private md-h2-marker         p/MARKER_MD_H2)
(def ^:private md-h3-marker         p/MARKER_MD_H3)
(def ^:private md-bold-marker       p/MARKER_MD_BOLD)
(def ^:private md-code-marker       p/MARKER_MD_CODE)
(def ^:private md-bullet-marker     p/MARKER_MD_BULLET)
(def ^:private md-table-head-marker p/MARKER_MD_TABLE_HEAD)
(def ^:private md-table-sep-marker  p/MARKER_MD_TABLE_SEP)
(def ^:private md-table-row-marker  p/MARKER_MD_TABLE_ROW)
(def ^:private md-quote-marker      p/MARKER_MD_QUOTE)
(def ^:private md-hr-marker         p/MARKER_MD_HR)
(def ^:private md-summary-marker    p/MARKER_MD_SUMMARY)

(def ^:private tool-output-indent
  "Visible left margin for result rows under a tool call."
  "  ")

(def ^:private tool-output-indent-cols
  (p/display-width tool-output-indent))

(def ^:private output-indentable-markers
  #{code-err-pad-marker})

(def ^:private th-md-h1-marker         p/MARKER_TH_MD_H1)
(def ^:private th-md-h2-marker         p/MARKER_TH_MD_H2)
(def ^:private th-md-h3-marker         p/MARKER_TH_MD_H3)
(def ^:private th-md-bold-marker       p/MARKER_TH_MD_BOLD)
(def ^:private th-md-code-marker       p/MARKER_TH_MD_CODE)
(def ^:private th-md-bullet-marker     p/MARKER_TH_MD_BULLET)
(def ^:private th-md-table-head-marker p/MARKER_TH_MD_TABLE_HEAD)
(def ^:private th-md-table-sep-marker  p/MARKER_TH_MD_TABLE_SEP)
(def ^:private th-md-table-row-marker  p/MARKER_TH_MD_TABLE_ROW)
(def ^:private th-md-quote-marker      p/MARKER_TH_MD_QUOTE)
(def ^:private th-md-hr-marker         p/MARKER_TH_MD_HR)
(def ^:private th-md-summary-marker    p/MARKER_TH_MD_SUMMARY)

(def ^:private code-text-inset-markers
  #{code-marker code-ok-marker code-err-marker code-status-marker
    result-marker err-result-marker
    md-code-marker th-md-code-marker
    thinking-marker
    th-md-h1-marker th-md-h2-marker th-md-h3-marker
    th-md-bold-marker th-md-bullet-marker th-md-quote-marker
    th-md-hr-marker th-md-summary-marker
    th-md-table-head-marker th-md-table-sep-marker th-md-table-row-marker})

(defn- ansi-code->fg [code current-fg base-fg]
  (case code
    0 base-fg
    30 t/code-block-fg
    31 t/code-syntax-string-fg
    32 t/code-success-fg
    33 t/warning-fg
    34 t/code-syntax-number-fg
    35 t/code-syntax-special-fg
    36 t/code-syntax-keyword-fg
    37 t/text-fg
    90 t/code-syntax-comment-fg
    91 t/code-error-fg
    92 t/code-success-fg
    current-fg))

(defn- parse-ansi-codes [s]
  (into []
    (keep (fn [part]
            (try
              (Long/parseLong part)
              (catch NumberFormatException _ nil))))
    (str/split (or s "") #";")))

(defn- ansi-codes->fg [codes current-fg base-fg]
  (reduce
    (fn [fg code]
      (if (= 0 code)
        base-fg
        (ansi-code->fg code fg base-fg)))
    current-fg
    codes))

(defn- format-clojure-ansi
  "Format Clojure/EDN source via zprint and keep zprint's ANSI syntax
   coloring. The TUI painter translates those ANSI SGR codes to
   Lanterna colors; raw ANSI is never written to the terminal.

   Used for CODE blocks only (the user's authored source). Result
   bodies use `format-clojure-plain` instead: still pretty-printed,
   but with no ANSI syntax coloring.

   Channel render strips `(def NAME \"doc\" …)` docstring slots before
   pretty-printing — the human reader sees the binding name + the
   value, not the model-forced docstring noise. The persisted source
   keeps the docstring so var meta + restore continue to work.

   Live progress redraws on the spinner cadence while a provider call
   is in flight. Without this cache, every tick re-ran zprint over
   every already-finished form in the progress trace, so a long trace
   could make the TUI look frozen even though app-db had advanced."
  [code-text width]
  (let [code-text (str code-text)
        width     (long width)
        stripped  (fmt/strip-def-docstrings code-text)]
    (cached* [:clojure-ansi width stripped]
      #(fmt/format-clojure-ansi stripped width))))

(defn- format-clojure-plain
  "Pretty-print Clojure/EDN source via zprint WITHOUT ANSI syntax
   coloring. This is the result-body formatter: pretty layout is OK,
   ANSI syntax highlighting is not.

   Same docstring-strip pass as `format-clojure-ansi` runs before
   zprint formats the source."
  [code-text width]
  (let [code-text (str code-text)
        width     (long width)
        stripped  (fmt/strip-def-docstrings code-text)]
    (cached* [:clojure-plain width stripped]
      #(fmt/format-clojure stripped width))))

(defn- paint-ansi-line!
  "Paint a possibly ANSI-colored zprint line onto a Lanterna surface.
   ANSI foreground codes are translated to Lanterna foreground colors;
   `bg` is always controlled by Vis so success/running/error code
   zones keep their own background.

   Tool render-fns now return Markdown for result bodies. That Markdown
   may contain Vis inline-style sentinels (U+E110..U+E117) for spans.
   These sentinels are paint directives, not glyphs; consume them here
   before they hit Lanterna's raw putString path."
  [^TextGraphics g x y ^String line base-fg bg]
  (letfn [(sentinel-chunk? [^String chunk]
            (boolean
              (some #(str/index-of chunk ^String %)
                [p/INLINE_BOLD_ON p/INLINE_BOLD_OFF
                 p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF
                 p/INLINE_STRIKE_ON p/INLINE_STRIKE_OFF
                 p/INLINE_CODE_ON p/INLINE_CODE_OFF])))
          (paint-chunk! [col ^String chunk fg]
            (p/set-colors! g fg bg)
            (if (sentinel-chunk? chunk)
              (p/paint-styled-line! g (+ x col) y chunk
                fg bg t/code-block-fg t/code-block-bg)
              (p/put-str! g (+ x col) y chunk)))]
    (loop [i 0 col 0 fg base-fg]
      (if (>= i (.length line))
        g
        (let [esc-idx (str/index-of line "\u001b[" i)]
          (if (or (nil? esc-idx) (< i esc-idx))
            (let [end   (or esc-idx (.length line))
                  chunk (subs line i end)]
              (paint-chunk! col chunk fg)
              (recur end (+ col (p/display-width chunk)) fg))
            (let [m-idx (str/index-of line "m" (+ esc-idx 2))]
              (if (nil? m-idx)
                (let [chunk (subs line esc-idx)]
                  (paint-chunk! col chunk fg)
                  g)
                (let [codes (parse-ansi-codes (subs line (+ esc-idx 2) m-idx))
                      fg*   (ansi-codes->fg codes fg base-fg)]
                  (recur (inc m-idx) col fg*))))))))))

(defn- warning-message? [text]
  (and (string? text) (str/starts-with? text "Warning:")))

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
  (let [cells (com.googlecode.lanterna.TextCharacter/fromString line)
        n     (alength cells)
        ;; end-col would be symmetric with end-char but lanterna's
        ;; putString computes the actual paint advance from the seg
        ;; itself, so we only need start-col for placement.
        paint-seg! (fn [start-char end-char start-col]
                     (when (and start-char (< start-char end-char))
                       (let [seg (subs line start-char end-char)]
                         (when (pos? (count seg))
                           (if (seq body-styles)
                             (p/styled g body-styles
                               (p/put-str! g (+ x start-col) y seg))
                             (p/put-str! g (+ x start-col) y seg))))))]
    (loop [i              0
           char-pos       0
           col-pos        0
           seg-start-char nil
           seg-start-col  nil]
      (if (>= i n)
        (paint-seg! seg-start-char char-pos seg-start-col)
        (let [tc        ^com.googlecode.lanterna.TextCharacter (aget cells i)
              grapheme  ^String (.getCharacterString tc)
              g-chars   (long (.length grapheme))
              g-cols    (if (.isDoubleWidth tc) 2 1)
              divider?  (and (= 1 g-chars)
                          (or (= (.charAt grapheme 0) \┃)
                            (= (.charAt grapheme 0) \│)))]
          (if divider?
            (do (paint-seg! seg-start-char char-pos seg-start-col)
              (recur (inc i)
                (+ char-pos g-chars)
                (+ col-pos g-cols)
                nil nil))
            (recur (inc i)
              (+ char-pos g-chars)
              (+ col-pos g-cols)
              (or seg-start-char char-pos)
              (or seg-start-col col-pos))))))))

;; ---------------------------------------------------------------------------
;; Link / image / file-link resources
;;
;; Markdown references (`[text](url)`, `![alt](url)`,
;; `[path:line](path#Lline)`) are collected from the message body,
;; surfaced as one compact top-right resources badge, then shown in a
;; modal picker when clicked. The old one-row-per-link strip helpers
;; stay here for compatibility with tests and potential future layouts,
;; but the active chat layout uses the badge + popup path.
;;
;; Why a badge instead of inline clickable spans:
;;
;;   - The inline tokeniser already runs over `:answer` text and
;;     intentionally drops link/image markup as plain prose. Touching
;;     it would force a second pass through the styled-line painter
;;     and risk regressions in the existing markdown rendering.
;;
;;   - A single row-level click target is easier to hit in terminal
;;     mouse mode than a set of mid-line spans.
;;
;;   - The badge gives a clean discoverability cue without pushing the
;;     answer body down by one row per resource.
;;
;; Resources are OPT-OUT for callers that pass `:hide-links? true` on
;; the message map. Today that flag isn’t set anywhere; reserved as
;; an escape hatch for warnings / cancelled messages where we deem
;; the chrome noise.

(def ^:private link-icon-image    "\uD83D\uDCF7")  ; 📷
(def ^:private link-icon-link     "\uD83D\uDD17")  ; 🔗
(def ^:private link-icon-file     "\uD83D\uDCC4")  ; 📄
(def ^:private link-icon-blocked  "\uD83D\uDEAB")  ; 🚫
(def ^:private resources-icon     "\uD83D\uDCDA")  ; 📚

(defn- resources-badge-label
  "Return the widest resources badge that fits `max-w` display cells,
   or nil when even the compact count cannot fit. The top-row badge is
   intentionally compact because it shares the right edge with the
   timestamp."
  [n max-w]
  (let [candidates [(str resources-icon " Resources " n)
                    (str resources-icon " Resources")
                    (str "Resources " n)
                    (str resources-icon " " n)
                    resources-icon]]
    (some #(when (<= (p/display-width %) max-w) %) candidates)))

(defn- in-viewport?
  "True when `abs-row` is inside the currently-painted messages
   viewport `[viewport-top, viewport-top + viewport-h)`."
  [^long viewport-top ^long viewport-h ^long abs-row]
  (and (>= abs-row viewport-top)
    (< abs-row (+ viewport-top viewport-h))))

(defn- paint-resources-badge!
  "Paint and register the clickable top-row resources badge for one
   bubble. Clicking opens a resources popup instead of opening a single
   URL immediately."
  [^TextGraphics g label refs x y width abs-col-base abs-row viewport-top viewport-h]
  (let [hovered (cr/hovered)
        hovered? (and (= :resources (:kind hovered))
                   (= abs-row (:row (:bounds hovered)))
                   (= (+ abs-col-base x) (:col (:bounds hovered))))
        bg       (if hovered? t/link-chrome-hover-bg t/terminal-bg)
        fg       (if hovered? t/link-chrome-hover-fg t/link-chrome-fg)]
    (p/clear-styles! g)
    (p/set-colors! g fg bg)
    (when hovered?
      (p/fill-rect! g x y width 1))
    (p/put-str! g x y label)
    (when (in-viewport? viewport-top viewport-h abs-row)
      (cr/register!
        {:bounds   {:row abs-row :col (+ abs-col-base x) :width width}
         :kind     :resources
         :refs     refs
         :enabled? true}))))

(defn- ref-icon
  "Return the chrome icon string for `ref`. Disabled refs always
   render with the blocked icon regardless of kind so the user
   reads “this one is dead” before reading the URL."
  ^String [{:keys [kind enabled?]}]
  (cond
    (not enabled?) link-icon-blocked
    (= kind :image)      link-icon-image
    (= kind :file)       link-icon-file
    :else                link-icon-link))

;; Inline-style sentinel codepoints in the U+E110-U+E117 PUA range
;; (BOLD/ITALIC/STRIKE/CODE on/off pairs) are emitted upstream by the
;; canonical IR walker. The chrome strip doesn't paint per-character
;; SGR — it's a single-tone hover band — so we strip the sentinels
;; back out before display, leaving plain text. The pre-pass lifts
;; `**bold**` to `bold`, `*italic*` to `italic`, `` `code` `` to
;; `code`, while unmatched openers (`*half`, `**incomplete`) stay
;; literal — same behaviour the prose body gets. Without this pass
;; the chrome shows raw markdown markup verbatim, e.g.
;;   [See **here**](url) -> "🔗 See **here** -> url"
;; which looks like a typo to the user.
(defn- ir-node-visible-text
  "Recursive walk: concatenate every visible text segment from a
   canonical IR node, dropping link URLs and image srcs (we only
   want the visible label, not the target). Used by chrome rows
   where `[See **here**](url)` should read `See here`, not
   `See here (url)` (which is what `extract-text`'s plain renderer
   would emit for a link whose label differs from its href)."
  ^String [node]
  (cond
    (string? node) node
    (not (vector? node)) ""
    :else
    (let [tag (first node)
          children (drop 2 node)]
      (case tag
        :br   " "
        :img  ""
        (:span :c :code :kbd) (or (some #(when (string? %) %) children) "")
        (apply str (map ir-node-visible-text children))))))

(defn- strip-inline-markup
  "Strip inline markdown markup (`**bold**`, `` `code` ``, link
   decoration, etc.) from `text`, yielding the plain visible label.
   Used by chrome-row rendering so refs like `[See **here**](url)`
   show as `🔗 See here -> url` (visible label only) instead of
   leaking asterisks or the URL inside the bracket portion.

   Lifts through `vis/markdown->ir` (commonmark parser) then walks the
   IR concatenating only visible text — link/image targets dropped."
  ^String [^String text]
  (let [s (or text "")]
    (if (str/blank? s)
      s
      (str/trim (ir-node-visible-text (vis/markdown->ir s))))))

(defn- chrome-display-text
  "Build the visible chrome row for a ref:
     <icon> <text> -> <url>
   The url tail is shortened to fit `max-w`. Pure.

   `text` is run through `strip-inline-markup` so inline emphasis
   inside the bracket portion (e.g. `[See **here**](url)`,
   ``[Title with `code`](url)``) renders as plain text in the
   chrome row instead of leaking raw `**` / backticks / `*`."
  ^String [ref ^long max-w]
  (let [icon  (ref-icon ref)
        text  (strip-inline-markup (or (:text ref) ""))
        url   (or (:url ref)  "")
        sep   " -> "
        ;; Reserve room: icon (2 cols) + space + sep + at least 8 cols
        ;; of url.
        head     (str icon " " text)
        head-w   (p/display-width head)
        sep-w    (p/display-width sep)
        avail    (max 0 (- max-w head-w sep-w))
        url-disp (if (<= (count url) avail)
                   url
                   (let [keep (max 0 (- avail 1))]
                     (str (subs url 0 (min (count url) keep)) "...")))]
    (str head sep url-disp)))

(defn extract-link-refs*
  "Pure ref extractor. Walks `text` once and returns the vec of
   refs `links/parse-md-refs` produces, plus a synthesised
   `:display` string for chrome painting at width `max-w`."
  [^String text ^long max-w]
  (let [refs (links/parse-md-refs text)]
    (mapv (fn [r] (assoc r :display (chrome-display-text r max-w))) refs)))

(defn extract-link-refs
  "Memoised wrapper around `extract-link-refs*`. Keyed by the
   message text’s identity-hash + width - the same trick
   `bubble-height` uses to dodge re-walking immutable message bodies
   on every paint."
  [{:keys [text hide-links?]} max-w]
  (if (or hide-links?
        (> (count (or text "")) max-link-ref-text-chars))
    []
    (cached* [::refs (System/identityHashCode text) (long max-w)]
      #(extract-link-refs* (or text "") max-w))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- paint-link-chrome-row!
  "Paint one chrome row at `(x, y)` (clip-relative coords) inside a
   `bubble-w`-wide column. When the row’s absolute screen position
   `abs-row` is inside the viewport AND the ref is enabled, register
   a click region.

   The row paints `<icon> <text> -> <url>` in three colour zones so
   the affordance reads cleanly (link colour for the text, muted
   gray for the arrow + url tail). Hover state inverts the row to a
   pale blue band."
  [^TextGraphics g
   {:keys [display enabled?] :as ref}
   x y bubble-w
   abs-col-base abs-row
   viewport-top viewport-h]
  (let [hovered? (and enabled?
                   (= abs-row (:row (:bounds (cr/hovered)))))
        bg       (cond
                   hovered? t/link-chrome-hover-bg
                   :else    t/terminal-bg)
        fg-text  (cond
                   (not enabled?) t/link-chrome-blocked-fg
                   hovered?       t/link-chrome-hover-fg
                   :else          t/link-chrome-fg)]
    (p/clear-styles! g)
    (p/set-colors! g t/text-fg bg)
    (p/fill-rect! g x y bubble-w 1)
    ;; Whole row painted in one colour pass; the text/arrow/url
    ;; tri-tone is a future polish - the single-tone read is
    ;; already obviously a link, the tri-tone added complexity
    ;; without changing the click affordance.
    (p/set-colors! g fg-text bg)
    (p/put-str! g x y display)
    (when (and enabled? (in-viewport? viewport-top viewport-h abs-row))
      (cr/register!
        {:bounds   {:row abs-row :col (+ abs-col-base x) :width bubble-w}
         :url      (:url ref)
         :kind     (:kind ref)
         :line     (:line ref)
         :scheme   (:scheme ref)
         :enabled? true}))))

;; ---------------------------------------------------------------------------

(defn- op-tag->color-role
  "Channel-local mapping from the engine `:observation | :mutation` value to a
   TUI color role. Editing extensions emit `:tag` ONLY — they
   don't decide colors. This is the single point where the TUI
   binds the engine contract to its own theme vocabulary.

   Tags collapsed from 8 (read/search/edit/create/delete/move/
   shell/meta) to 2 (observation/mutation) per PLAN.md §2.1. The
   TUI keeps its 8 color roles for visual richness; renderers can
   look at `:op` (the symbol) for finer-grained painting if
   needed, but the canonical mapping below uses just the tag."
  [op-tag]
  (case op-tag
    :observation :tool-color/read
    :mutation    :tool-color/edit
    nil))

(defn- tool-color-role->fg
  [role]
  (case role
    :tool-color/read t/tool-color-read
    :tool-color/search t/tool-color-search
    :tool-color/preview t/tool-color-preview
    :tool-color/edit t/tool-color-edit
    :tool-color/create t/tool-color-create
    :tool-color/delete t/tool-color-delete
    :tool-color/move t/tool-color-move
    :tool-color/shell t/tool-color-shell
    :tool-color/meta t/tool-color-meta
    nil))

(defn- meta->color-role
  "Resolve a color role from a meta map carrying `:tag`. Single
   source of truth: the engine-derived tag (PLAN §2.1, 2-value enum)."
  [m]
  (op-tag->color-role (:tag m)))

(defn- silent-form-count
  [message]
  (let [n (reduce + 0
            (map (fn [trace]
                   (count (filter :silent? (:forms trace))))
              (or (:traces message) [])))]
    (when (pos? n) n)))

(defn- llm-label
  [{:keys [provider model]}]
  (when (or provider model)
    (str (or provider "unknown") (when model (str "/" model)))))

(defn- fallback-summary
  "Compact assistant-footer line describing the routing fallback.

   The resumed/live footer surfaces:
     - provider-id/model pair `from → to`
     - retry count (`:llm.routing/provider-retry` events in the trace)
     - reason / status that triggered the fallback (HTTP status
       preferred over the free-form `:error` string so it matches the
       spec example `— 3 retries, 429`).

   Falls back to the textual `:error` from the fallback event when no
   status is present (e.g. format-fallback paths)."
  [{:keys [llm-selected llm-actual llm-fallback? llm-routing-trace]}]
  (when llm-fallback?
    (let [from (llm-label llm-selected)
          to   (llm-label llm-actual)
          fallback-event (first (filter #(contains? #{:llm.routing/provider-fallback
                                                      :llm.routing/format-fallback}
                                           (:event/type %))
                                  llm-routing-trace))
          retry-count (count (filter #(= :llm.routing/provider-retry (:event/type %))
                               llm-routing-trace))
          status (:status fallback-event)
          ;; Prefer HTTP status (matches spec example "3 retries, 429")
          ;; over the free-form error message. Fall back to the reason
          ;; keyword name or the error text for format-fallback paths.
          why (cond
                (some? status) (str status)
                (some? (:reason fallback-event)) (name (:reason fallback-event))
                :else (:error fallback-event))
          retries (when (pos? retry-count)
                    (str retry-count " retr" (if (= 1 retry-count) "y" "ies")))
          suffix-parts (->> [retries why]
                         (remove (fn [s] (or (nil? s) (str/blank? (str s))))))]
      (str "fallback"
        (when (or from to)
          (str " " (or from "unknown") " → " (or to "unknown")))
        (when (seq suffix-parts)
          (str " — " (str/join ", " suffix-parts)))))))

(defn- assistant-meta-line
  ;; The footer is a Settings-driven toggle. `:message-meta-mode` rides on
  ;; the message map (injected by virtual.clj projection) so both the
  ;; painter and `bubble-height*` see the same mode without threading
  ;; settings through every call. Missing key → `:full` (back-compat for
  ;; synthetic / pre-projection messages).
  ;;   :full  -> model / iters / tokens / cost / duration (+ suffix)
  ;;   :short -> cost / duration (+ suffix); drops model, iters, tokens
  ;;   :off   -> nil → bubble drops footer-gap + footer rows entirely
  [{:keys [iteration-count duration-ms tokens cost message-meta-mode] :as message}]
  (let [mode (or message-meta-mode :full)]
    (when-not (= mode :off)
      (let [short? (= mode :short)
            line   (vis/format-meta-line
                     (cond-> {:iteration-count iteration-count
                              :silent-count (silent-form-count message)
                              :duration-ms duration-ms
                              :tokens tokens
                              :cost cost}
                       short? (assoc :iteration-count nil :tokens nil))
                     (cond-> {:suffix (when-let [fallback (fallback-summary message)]
                                        [fallback])}
                       short? (assoc :model false)))]
        (when-not (str/blank? line) line)))))

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
     window the messages-area paints into. They’re only consulted
     by the link-chrome painter to decide whether to register a
     click region for an off-screen chrome row (it doesn’t).
     Callers that paint outside `draw-messages-area!` (tests, REPL
     exploration) can pass `0 / 0` to disable click registration."
  [^TextGraphics g
   {:keys [role text timestamp status] :as message}
   start-row left max-w
   & [{:keys [viewport-top viewport-h]
       :or   {viewport-top 0 viewport-h 0}}]]
  (let [user?     (= role :user)
        queued?   (= :queued status)
        warning?  (warning-message? text)
        ;; Cancelled turns are status messages, not real answers -
        ;; render the entire bubble dim (gray + italic), drop the
        ;; meta line, dim the role label too. Skips markdown so a
        ;; bare text like \"Cancelled by user.\" reads naturally.
        cancelled? (= :cancelled status)
        ;; Legacy turn separators are disabled. Keep top-sep-h in
        ;; layout math so old callers stay shape-compatible without
        ;; reserving any spacer row.
        top-sep-h 0
        label     (cond
                    queued? "Queued"
                    user?   "You"
                    :else   "Vis")
        bubble-w  max-w
        ;; Symmetric inner padding (2 cols each side) inside the
        ;; message column. Applies to plain text AND to every
        ;; styled-marker zone (code blocks, answer, iteration
        ;; headers) so right-aligned labels like "ITERATION 1" /
        ;; "FINAL ANSWER" sit nicely inset from the right edge
        ;; instead of mashed against it.
        h-pad     2
        content-w (max 1 (- bubble-w (* 2 h-pad)))
        ;; `:prewrapped-lines` is set by `virtual.clj` projection for
        ;; every visible bubble (walker output); the `wrap-text`
        ;; fallback only triggers for placeholders / synthetic msgs
        ;; that bypass projection (rare).
        raw-lines (or (:prewrapped-lines message)
                    (wrap-text text content-w))
        lines     (clipped-lines raw-lines content-w)
        line-meta (or (:line-meta message)
                    (vec (repeat (count lines) nil)))
        ;; Mid-window walker support: when `:lines-window {:start :total-h}`
        ;; is set on the message (by virtual/layout for genuine mid-scroll),
        ;; `lines` is only rows [start, start+count) of the full bubble.
        ;; Total height comes from `:total-h`; the painter loop translates
        ;; logical row `i` to lines-vec index via `(- i lines-offset)`.
        lines-window (:lines-window message)
        lines-offset (long (or (:start lines-window) 0))
        bubble-h  (long (or (:total-h lines-window) (count lines)))
        bx        left
        ;; No bg fill on plain assistant text - we sit on terminal-bg.
        ;; `:warning` and user messages each get a tinted block so
        ;; they're impossible to miss in the timeline. Cancelled turns
        ;; intentionally render on bare terminal-bg - the muted italic
        ;; fg + status footer carry the "aborted" signal without a
        ;; bubble-wide fill that competed visually with adjacent
        ;; assistant messages.
        bg-color  (cond
                    warning?   t/warning-bg
                    queued?    t/terminal-bg
                    ;; User messages fill their content rows with a
                    ;; very pale warm-yellow block so "you said this"
                    ;; reads as its own zone, distinct from the white
                    ;; assistant area.
                    user?      t/user-bubble-bg
                    :else      t/terminal-bg)
        fg-color  (cond
                    cancelled? t/cancelled-fg
                    queued?    t/dialog-hint
                    warning?   t/warning-fg
                    user?      t/user-bubble-fg
                    :else      t/ai-bubble-fg)
        role-fg   (cond
                    cancelled? t/dialog-hint
                    queued?    t/dialog-hint
                    user?      t/user-role-fg
                    :else      t/ai-role-fg)
        time-str   (vis/format-date timestamp)
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
        ;; a "Cancelled" placeholder.
        meta-str   (when (and (not user?) (not cancelled?))
                     (assistant-meta-line message))
        refs       (extract-link-refs message bubble-w)]

    ;; Role label (bold, role-colored) + optional resources badge +
    ;; timestamp. Resources sit in the top-right chrome instead of
    ;; taking one row per link under the answer body.

    (when (pos? top-sep-h)
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-border t/terminal-bg)
      (p/put-str! g bx start-row (p/horiz-line bubble-w)))

    (let [label-row (+ start-row top-sep-h)]
      (p/clear-styles! g)
      (p/set-colors! g role-fg t/terminal-bg)
      (p/styled g [p/BOLD]
        (p/put-str! g bx label-row label))
      (let [right-edge   (+ bx bubble-w)
            time-w       (if time-str (p/display-width time-str) 0)
            time-x       (when time-str (- right-edge time-w))
            badge-end    (if time-x (- time-x 2) right-edge)
            badge-min-x  (+ bx (p/display-width label) 2)
            badge-max-w  (max 0 (- badge-end badge-min-x))
            badge-label  (when (pos? (count refs))
                           (resources-badge-label (count refs) badge-max-w))
            badge-w      (if badge-label (p/display-width badge-label) 0)
            badge-x      (- badge-end badge-w)
            abs-row      (+ (long viewport-top) (long label-row))]
        (when badge-label
          (paint-resources-badge!
            g badge-label refs badge-x label-row badge-w
            0 abs-row (long viewport-top) (long viewport-h)))
        (when time-str
          (p/set-colors! g t/dialog-hint t/terminal-bg)
          (p/put-str! g (+ bx (max (count label) (- bubble-w (count time-str)))) label-row time-str))))

    ;; Content begins directly under the role banner for ASSISTANT
    ;; bubbles - the previous breathing row read as an unwanted top
    ;; margin above the thinking block. USER bubbles keep the
    ;; breathing row so the yellow block has visible vertical
    ;; padding above the typed text (otherwise the yellow band
    ;; hugs the first character and reads as a single-row strip).
    ;; User bubbles also keep one breathing row BELOW the typed text.
    ;; Mirror this in `bubble-height*` so the math stays in sync.
    (let [top-pad    (if user? 1 0)
          bottom-pad (if user? 1 0)
          btop       (+ start-row top-sep-h 1 top-pad)]
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
      (when warning?
        (p/set-bg! g bg-color)
        (p/fill-rect! g bx btop bubble-w (max 1 bubble-h)))
      ;;
      ;; Cancelled: NO bubble-wide fill. The muted italic fg
      ;; (`cancelled-fg`) + dimmed role label + plain status footer
      ;; carry the "aborted" signal on bare terminal-bg.
      ;;
      ;; User messages: fill content rows only; the single message gap
      ;; remains outside the fill.
      (when (and user? (not queued?))
        (p/set-bg! g bg-color)
        (p/fill-rect! g bx (- btop top-pad) bubble-w (+ (max 1 bubble-h) top-pad bottom-pad)))

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
      (let [n            (count lines)
            ;; Restrict iteration to rows that actually intersect the
            ;; viewport. Pre-virtualisation we walked all `n` lines
            ;; on every redraw and let Lanterna clip OS-side; for an
            ;; 11k-row trace bubble (session 7b18414d) that pegged the
            ;; render thread at ~110 ms / frame even with a fully
            ;; warm cache. Now we only touch the rows whose screen
            ;; offset is inside [0, viewport-h).
            ;;
            ;; `viewport-h` is the messages-area inner height; the
            ;; caller in `draw-messages-area!` passes it. Tests /
            ;; REPL exploration that pass `viewport-h=0` (the
            ;; arity-3 default) keep the old paint-everything
            ;; behaviour.
            i-start      (long (if (pos? (long viewport-h)) (max 0 (- btop)) 0))
            i-end        (long (if (pos? (long viewport-h)) (min n (- (long viewport-h) btop)) n))
            iteration-bg t/iteration-header-bg
            answer-marker? (fn [^String l]
                             (or (str/starts-with? l answer-sep-marker)
                               (str/starts-with? l answer-hdr-marker)
                               (str/starts-with? l answer-pad-marker)))
            ;; Allocation-free O(n) scan, cached by `lines`-identity.
            ;; The previous `(some (map-indexed vector lines))`
            ;; allocated a fresh `[i l]` tuple per element on every
            ;; redraw - 11k tuples per frame on the big bubbles.
            ;; `wrap-text` is identity-stable across frames, so the
            ;; cached answer-start hits next time around.
            answer-start (cached* [::ans-start (System/identityHashCode lines)]
                           #(loop [i 0]
                              (cond
                                (>= i n) n
                                (answer-marker? (nth lines i)) i
                                :else (recur (inc i)))))]
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
                  line      (when-not noop-row? (nth lines lines-idx))
                  meta      (when-not noop-row? (nth line-meta lines-idx nil))]
              (when-not noop-row?
                (p/clear-styles! g)
                (let [in-answer? (> i (long answer-start))
                ;; Two coordinate systems per content row:
                ;;   text  at `x = bx + h-pad`, runs `content-w` cols  - keeps
                ;;         body padded inside the column.
                ;;   fills at `fbx = bx`,        run  `bubble-w`  cols - every
                ;;         marker zone (code, answer, iteration
                ;;         header, thinking, table...) paints the FULL message
                ;;         column so the colored band reaches both edges of
                ;;         the messages area instead of leaving a 2-col
                ;;         white strip on each side.
                ;; Right-aligned labels in `format-iteration-entry` write at
                ;; `x` and inherit `content-w`, so they still sit inset from
                ;; the right edge by h-pad even though the bg fills past them.
                      ;; Assistant answer text starts at the same column as
                      ;; the `Vis` label. User bubbles keep their inset so the
                      ;; left rail remains visually separate from prompt text.
                      x   (+ bx (if user? h-pad 0)) y (+ btop i)
                      iw  bubble-w
                      fbx bx
                      marker (when (pos? (count line)) (subs line 0 1))
                      body   (when marker (subs line 1))
                      output-indented? (and (contains? output-indentable-markers marker)
                                         (str/starts-with? body tool-output-indent))
                      line (if output-indented?
                             (str marker (subs body (count tool-output-indent)))
                             line)
                      code-text-inset? (and (not user?) (contains? code-text-inset-markers marker))
                      x   (cond-> x
                            output-indented? (+ tool-output-indent-cols)
                            code-text-inset? inc)
                      iw  (if output-indented? (max 0 (- iw tool-output-indent-cols)) iw)
                      fbx (if output-indented? (+ fbx tool-output-indent-cols) fbx)]
            ;; Pre-fill answer zone bg so ALL line types get it
                  (when in-answer?
                    (p/set-bg! g t/answer-bg)
                    (p/fill-rect! g fbx y iw 1))
                  (cond
              ;; ── Iteration header - right-aligned, subtle ──
                    (str/starts-with? line iteration-hdr-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/put-str! g x y (subs line 1)))

              ;; ── Iteration recap - terminal-bg, BOLD + ITALIC ──
              ;; No bubble-wide fill: recap text breathes on the bare
              ;; transcript background so it cannot be mistaken for the
              ;; iteration-header chrome. The bold + italic styling
              ;; (plus the leading `* Recap:` bullet) is what calls the
              ;; row out; surrounding pad rows render as truly empty
              ;; terminal-bg rows so the block has visible top + bottom
              ;; breathing room.
                    (str/starts-with? line recap-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/dialog-hint t/terminal-bg)
                      (p/styled g [p/BOLD p/ITALIC]
                        (p/paint-styled-line! g x y raw
                          t/dialog-hint t/terminal-bg
                          t/code-block-fg t/code-block-bg)))

              ;; ── Thinking - dimmed bg, italic ──
              ;; Inline span sentinels (**bold** etc.) embedded in
              ;; thinking-mode prose are honoured via paint-styled-line!
              ;; on top of the italic base style. Italic stacks with
              ;; SGR/BOLD per terminal SGR rules, so a bolded word
              ;; inside thinking renders as bold-italic, not as plain
              ;; bold (which would visually escape the thinking zone).
                    (str/starts-with? line thinking-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/ITALIC]
                        (p/paint-styled-line! g x y raw
                          t/dialog-hint t/iteration-header-bg
                          t/code-block-fg t/code-block-bg)))

              ;; ── Code (success) - light green bg, green ✓ suffix ──
                    (str/starts-with? line code-ok-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/code-block-fg t/code-ok-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y raw t/code-block-fg t/code-ok-bg)
                      (when-let [ci (str/index-of raw "✓")]
                        (p/set-colors! g t/code-success-fg t/code-ok-bg)
                        (p/put-str! g (+ x ci) y (subs raw ci))))

              ;; ── Code (error) - light red bg, red ✗ suffix ──
                    (str/starts-with? line code-err-marker)
                    (let [raw (subs line 1)]
                      (p/set-colors! g t/code-block-fg t/code-err-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y raw t/code-block-fg t/code-err-bg)
                      (when-let [ci (str/index-of raw "✗")]
                        (p/set-colors! g t/code-error-fg t/code-err-bg)
                        (p/put-str! g (+ x ci) y (subs raw ci))))

              ;; ── Code status line (✓/✗/↻ timing) - skip in copy, style by glyph ──
                    (str/starts-with? line code-status-marker)
                    (let [raw (subs line 1)]
                      (cond
                        (str/includes? raw "\u2713")
                        (do (p/set-colors! g t/code-block-fg t/code-ok-bg)
                          (p/fill-rect! g fbx y iw 1)
                          (paint-ansi-line! g x y raw t/code-block-fg t/code-ok-bg)
                          (when-let [ci (str/index-of raw "\u2713")]
                            (p/set-colors! g t/code-success-fg t/code-ok-bg)
                            (p/put-str! g (+ x ci) y (subs raw ci))))
                        (str/includes? raw "\u2717")
                        (do (p/set-colors! g t/code-block-fg t/code-err-bg)
                          (p/fill-rect! g fbx y iw 1)
                          (paint-ansi-line! g x y raw t/code-block-fg t/code-err-bg)
                          (when-let [ci (str/index-of raw "\u2717")]
                            (p/set-colors! g t/code-error-fg t/code-err-bg)
                            (p/put-str! g (+ x ci) y (subs raw ci))))
                        :else
                        (do (p/set-colors! g t/code-block-fg t/code-block-bg)
                          (p/fill-rect! g fbx y iw 1)
                          (paint-ansi-line! g x y raw t/code-block-fg t/code-block-bg))))

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
                    (do (p/set-colors! g t/code-result-fg t/code-block-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y (subs line 1) t/code-result-fg t/code-block-bg))

              ;; ── Result (error) - neutral code-block bg ──
                    (str/starts-with? line err-result-marker)
                    (do (p/set-colors! g t/code-error-result-fg t/code-block-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y (subs line 1) t/code-error-result-fg t/code-block-bg))

              ;; ── Code block padding (running / neutral) ──
                    (str/starts-with? line code-pad-marker)
                    (do (p/set-bg! g t/code-block-bg)
                      (p/fill-rect! g fbx y iw 1))

              ;; ── Code block padding (success) ──
                    (str/starts-with? line code-ok-pad-marker)
                    (do (p/set-bg! g t/code-ok-bg)
                      (p/fill-rect! g fbx y iw 1))

              ;; ── Code block padding (error) ──
                    (str/starts-with? line code-err-pad-marker)
                    (do (p/set-bg! g t/code-err-bg)
                      (p/fill-rect! g fbx y iw 1))

              ;; ── Iteration zone padding (margin between blocks) ──
                    (str/starts-with? line iteration-pad-marker)
                    (do (p/set-bg! g bg-color)
                      (p/fill-rect! g fbx y iw 1))

              ;; ── Answer separator - bold horizontal rule between iterations and answer ──
                    (str/starts-with? line answer-sep-marker)
                    (do (p/set-colors! g t/answer-sep-fg bg-color)
                      (p/styled g [p/BOLD]
                        (p/put-str! g fbx y (repeat-str \u2500 iw))))

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
                      (p/styled g [p/BOLD]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/md-h1-fg lbg t/code-block-fg t/code-block-bg)))

                    (str/starts-with? line md-h2-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g t/md-h2-fg lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/md-h2-fg lbg t/code-block-fg t/code-block-bg)))

                    (str/starts-with? line md-h3-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g t/md-h3-fg lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/md-h3-fg lbg t/code-block-fg t/code-block-bg)))

                    (str/starts-with? line md-bold-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g fg-color lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD]
                        (p/paint-styled-line! g x y (subs line 1)
                          fg-color lbg t/code-block-fg t/code-block-bg)))

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
                    (let [abs-row (+ (long viewport-top) y)
                          hovered? (and (= :toggle-details (:kind meta))
                                     (= abs-row (:row (:bounds (cr/hovered)))))
                          bg       (if hovered? t/link-chrome-hover-bg t/md-summary-bg)
                          tool-fg  (tool-color-role->fg (:color-role meta))
                          fg       (cond
                                     hovered? t/link-chrome-hover-fg
                                     tool-fg  tool-fg
                                     :else    t/md-summary-fg)]
                      (p/set-colors! g fg bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD]
                        (p/paint-styled-line! g x y (subs line 1)
                          fg bg
                          t/code-block-fg t/code-block-bg))
                      (case (:kind meta)
                        :toggle-details
                        (cr/register!
                          {:bounds {:row abs-row :col fbx :width iw}
                           :kind :toggle-details
                           :session-id (:session-id meta)
                           :node-id (:node-id meta)
                           :collapsed? (:collapsed? meta)})

                        nil))

                    (str/starts-with? line md-code-marker)
                    (do
                      (p/set-colors! g t/code-block-fg t/code-block-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (paint-ansi-line! g x y (subs line 1) t/code-block-fg t/code-block-bg))

              ;; Bullet items: same inline-span treatment as plain text.
              ;; `- **bold** thing` should bold the word.
                    (str/starts-with? line md-bullet-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
                      (p/set-colors! g fg-color lbg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/paint-styled-line! g x y (subs line 1)
                        fg-color lbg t/code-block-fg t/code-block-bg))

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
                      (p/styled g [p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/dialog-hint lbg t/code-block-fg t/code-block-bg)))

                    (str/starts-with? line md-hr-marker)
                    (let [lbg (if in-answer? t/answer-bg bg-color)]
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
                          head?    (str/starts-with? line md-table-head-marker)
                          border?  (str/starts-with? line md-table-sep-marker)
                          tbg      (if in-answer? t/answer-bg t/code-block-bg)
                          tfg      (if in-answer? t/answer-fg  t/code-block-fg)]
                      (p/clear-styles! g)
                      (p/set-colors! g t/code-border-fg tbg)
                      (p/fill-rect! g fbx y iw 1)
                      (if border?
                  ;; Pure box-drawing line - single muted paint.
                        (p/put-str! g x y stripped)
                  ;; Header / body data row - dual-color split.
                        (paint-table-data-line! g x y stripped
                          tfg t/code-border-fg tbg
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
                      (p/styled g [p/BOLD p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/iteration-header-fg t/iteration-header-bg
                          t/code-result-fg t/code-block-bg)))

                    (str/starts-with? line th-md-h2-marker)
                    (do (p/set-colors! g t/iteration-header-fg t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/iteration-header-fg t/iteration-header-bg
                          t/code-result-fg t/code-block-bg)))

                    (str/starts-with? line th-md-h3-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/dialog-hint t/iteration-header-bg
                          t/code-result-fg t/code-block-bg)))

                    (str/starts-with? line th-md-bold-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/dialog-hint t/iteration-header-bg
                          t/code-result-fg t/code-block-bg)))

                  ;; <summary> disclosure label inside the thinking
                  ;; zone. Same lavender-family band as the answer
                  ;; mode but darker / desaturated so it stays inside
                  ;; the dim reasoning region instead of stealing the
                  ;; eye like the answer-mode band would. Italic
                  ;; matches every other thinking-mode marker so the
                  ;; whole reasoning block reads as one cohesive zone.
                    (str/starts-with? line th-md-summary-marker)
                    (let [abs-row (+ (long viewport-top) y)
                          hovered? (and (= :toggle-details (:kind meta))
                                     (= abs-row (:row (:bounds (cr/hovered)))))
                          bg       (if hovered? t/link-chrome-hover-bg t/th-md-summary-bg)
                          tool-fg  (tool-color-role->fg (:color-role meta))
                          fg       (cond
                                     hovered? t/link-chrome-hover-fg
                                     tool-fg  tool-fg
                                     :else    t/th-md-summary-fg)]
                      (p/set-colors! g fg bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/BOLD p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          fg bg
                          t/code-result-fg t/code-block-bg))
                      (case (:kind meta)
                        :toggle-details
                        (cr/register!
                          {:bounds {:row abs-row :col fbx :width iw}
                           :kind :toggle-details
                           :session-id (:session-id meta)
                           :node-id (:node-id meta)
                           :collapsed? (:collapsed? meta)})

                        nil))

              ;; Thinking fenced code: visible code-block bg, italic dim text.
              ;; Clojure/EDN fences can carry zprint ANSI syntax color;
              ;; the painter translates ANSI foreground codes to Lanterna.
                    (str/starts-with? line th-md-code-marker)
                    (do
                      (p/set-colors! g t/code-result-fg t/code-block-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/ITALIC]
                        (paint-ansi-line! g x y (subs line 1) t/code-result-fg t/code-block-bg)))

                    (str/starts-with? line th-md-bullet-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/dialog-hint t/iteration-header-bg
                          t/code-result-fg t/code-block-bg)))

                    (str/starts-with? line th-md-quote-marker)
                    (do (p/set-colors! g t/dialog-hint t/iteration-header-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/styled g [p/ITALIC]
                        (p/paint-styled-line! g x y (subs line 1)
                          t/dialog-hint t/iteration-header-bg
                          t/code-result-fg t/code-block-bg)))

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
                          head?    (str/starts-with? line th-md-table-head-marker)
                          border?  (str/starts-with? line th-md-table-sep-marker)
                          tbg      t/iteration-header-bg]
                      (p/clear-styles! g)
                      (p/set-colors! g t/code-border-fg tbg)
                      (p/fill-rect! g fbx y iw 1)
                      (if border?
                        (p/put-str! g x y stripped)
                        (paint-table-data-line! g x y stripped
                          t/code-result-fg t/code-border-fg tbg
                          (cond
                            head? [p/BOLD p/ITALIC]
                            :else [p/ITALIC]))))

              ;; ── Legacy separator - dim ──
                    (str/starts-with? line sep-marker)
                    (do (p/set-colors! g t/dialog-hint bg-color)
                      (p/put-str! g x y (subs line 1)))

              ;; ── Answer text - answer bg ──
              ;; Plain final-answer paragraphs can still contain inline
              ;; sentinels from IR spans, e.g. `[:c "/command"]` becomes
              ;; INLINE_CODE_ON/OFF around the body text. Consume those
              ;; here instead of writing raw PUA glyphs to the terminal.
                    (str/starts-with? line answer-txt-marker)
                    (do (p/set-colors! g t/answer-fg t/answer-bg)
                      (p/fill-rect! g fbx y iw 1)
                      (p/paint-styled-line! g x y (subs line 1)
                        t/answer-fg t/answer-bg
                        t/code-block-fg t/code-block-bg))

              ;; ── Answer padding ──
                    (str/starts-with? line answer-pad-marker)
                    (do (p/set-bg! g t/answer-bg)
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
                      (when in-answer-zone?
                        (p/set-bg! g line-bg)
                        (p/fill-rect! g fbx y iw 1))
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
                        (p/paint-styled-line! g x y line
                          line-fg line-bg
                          t/code-block-fg t/code-block-bg)))))
                (when user?
                  (p/clear-styles! g)
                  (p/set-colors! g role-fg bg-color)
                  (p/put-str! g bx (+ btop i) "│")))
              (recur (inc i))))))

      ;; Below-content footer row: optional right-aligned meta, with
      ;; one breathing row between answer body and footer.
      ;; Resource links are exposed through the header badge, so they
      ;; no longer consume one extra chrome row per link under the
      ;; answer body.
      ;;
      ;; Final per-message layout (no outer box, no bg fill, no
      ;; horizontal rule under the label):
      ;;   row 0                    : label + resources badge + timestamp
      ;;   row 1 ... N                : wrapped content (with marker-zone fills)
      ;;   row 1+N                  : bottom pad (user only)
      ;;   row 1+N+P                : blank footer margin, when meta present
      ;;   row 2+N+P                : meta right, when present
      ;;   final row                : single blank gap before the next message
      (p/clear-styles! g)
      (let [footer?    (some? meta-str)
            footer-gap (if footer? 1 0)
            footer-row (+ btop bubble-h bottom-pad footer-gap)]
        (when footer?
          (p/set-colors! g t/dialog-hint t/terminal-bg)
          (p/put-str! g (+ bx (max 0 (- bubble-w (count meta-str)))) footer-row meta-str))
        ;; Return: rows consumed
        ;;   = label(1) + top-pad(user only) + content(N)
        ;;     + bottom-pad(user only)
        ;;     + footer-gap(meta only)(0|1)
        ;;     + footer(meta)(0|1)
        ;;     + gap(1)
        (+ top-sep-h 1 top-pad bubble-h bottom-pad footer-gap (if footer? 1 0) 1)))))

(defn bubble-height*
  "Uncached calculation: rows a chat message will consume without drawing.
   label(1) + optional top-pad(1, user only) + wrapped-lines
   + optional bottom-pad(1, user only) + optional meta top margin/footer(0|2)
   + gap(1).
   Mirrors `draw-chat-bubble!`'s wrap width (`bubble-w - 2*h-pad`) so
   layout math stays consistent across the height calc and the draw."
  [{:keys [text role prewrapped-lines status] :as message} max-w]
  (let [bubble-w   max-w
        top-sep-h  0
        h-pad      2
        content-w  (max 1 (- bubble-w (* 2 h-pad)))
        ;; Same contract: virtual.clj projection populates
        ;; `:prewrapped-lines` via the IR walker for every visible
        ;; bubble; `wrap-text` is the bare-string fallback. Route
        ;; through `clipped-lines` here too: height calculation happens
        ;; during pre-warm/layout, so it warms the exact clipped vector
        ;; draw-chat-bubble! will need while scrolling.
        raw-lines  (or prewrapped-lines
                     (wrap-text text content-w))
        lines      (clipped-lines raw-lines content-w)
        top-pad    (if (= role :user) 1 0)
        bottom-pad (if (= role :user) 1 0)
        cancelled? (= :cancelled status)
        meta-str   (when (and (not= role :user) (not cancelled?))
                     (assistant-meta-line message))
        footer?    (some? meta-str)
        footer-gap (if footer? 1 0)]
    (+ top-sep-h 1 top-pad (count lines) bottom-pad footer-gap (if footer? 1 0) 1)))

(defn bubble-height
  "Memoized `bubble-height*`. Keyed by projected line identity when
   available; live progress keeps stable prewrapped body lines and only
   appends a cheap spinner row. Metadata that can add/remove the
   assistant footer is part of the key; otherwise a no-usage render can
   stale-cache the shorter height before usage arrives."
  [{:keys [text role prewrapped-lines turn-separator?
           iteration-count duration-ms tokens cost status
           llm-selected llm-actual llm-fallback? llm-routing-trace] :as message} max-w]
  (cached* [::bh
            (System/identityHashCode text)
            (System/identityHashCode prewrapped-lines)
            role
            (boolean turn-separator?)
            iteration-count
            duration-ms
            tokens
            cost
            status
            llm-selected
            llm-actual
            llm-fallback?
            llm-routing-trace
            (long max-w)]
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
    (some-> trace str str/split-lines first str/trim not-empty)))

(defn- form-error-headline
  [error]
  (let [headline (or (error-trace-headline error)
                   (:message error)
                   (some-> (:type error) str)
                   "unknown error")]
    (str "ERROR — " headline)))

(defn- inline-error-context-lines
  "Babashka-style code context for form eval errors. Kept inside the
   code band so failing source, caret, error message, and status occupy
   one visual block instead of code + error + repeated context blocks."
  [code-text error]
  (let [block     (:block error)
        source    (or (:source block) code-text)
        opened    (:opened-loc block)
        arrow-row (or (:row opened) (:row block))
        arrow-col (or (:col opened) (:col block))]
    (when (and (string? source) (not (str/blank? source)))
      (let [lines      (vec (str/split source #"\n" -1))
            total      (count lines)
            gutter-w   (count (str total))
            fmt-line   (fn [idx0]
                         (format (str " %" gutter-w "d: %s")
                           (inc idx0) (nth lines idx0)))
            arrow-line (when (and arrow-row arrow-col
                               (<= 1 arrow-row total))
                         (str (apply str (repeat (+ gutter-w 3) \space))
                           (apply str (repeat (max 0 (dec (long arrow-col))) \space))
                           "^---"))
            arrow-idx0 (when arrow-line (dec (long arrow-row)))]
        (vec
          (mapcat (fn [idx0]
                    (cond-> [(fmt-line idx0)]
                      (= idx0 arrow-idx0) (conj arrow-line)))
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
        (and (str/blank? (str (:code f)))
          (map? (:error f))
          (= :error (:result-kind f)))))))

(defn- form-error-only-error
  "Pull the placeholder form's `:error` map out of a form-error-only
   iteration. Returns nil when the iteration is not in that shape."
  [entry]
  (when (form-error-only-iteration? entry)
    (-> entry :forms first :error)))

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
    [(:type err) (:message err) (get-in err [:data :raw-data])]))

(def ^:private auto-collapse-line-threshold 12)
(def ^:private auto-collapse-char-threshold 700)
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
    (let [^String s (str s)
          n (.length s)]
      [n
       (subs s 0 (min 64 n))
       (subs s (max 0 (- n 256)))])))

(defn- iteration-running?
  "True when at least one form in the iteration has started but not
   yet completed. Drives the per-iteration cache key: iterations with
   running forms include a 1-second time bucket so the `↻ Ns`
   running-duration label refreshes; fully-done iterations cache
   forever (until LRU eviction)."
  [{:keys [forms]}]
  (boolean
    (some (fn [{:keys [started-at-ms success?]}]
            (and (some? started-at-ms) (nil? success?)))
      (or forms []))))

(defn- visible-iteration-entry
  "Filter `:forms` down to visibly-running/completed entries. When
   `show-silent?` is true the entry passes through unchanged; otherwise
   `:silent?` slots drop out."
  [entry show-silent?]
  (if show-silent?
    entry
    (update entry :forms (fn [forms] (vec (remove :silent? (or forms [])))))))

(defn- form-fingerprint
  "Content-derived fingerprint of one form map. Captures every field
   the iteration renderer reads."
  [{:keys [code comment render-segments result-render result-kind result-detail
           error duration-ms success? silent? started-at-ms]}]
  [(text-fingerprint code)
   (text-fingerprint comment)
   render-segments
   (text-fingerprint result-render)
   result-kind
   ;; result-detail is a small op-metadata map; compared structurally.
   result-detail
   error
   duration-ms
   success?
   silent?
   started-at-ms])

(defn- iteration-fingerprint
  "Content-derived fingerprint of an iteration entry. Captures every
   field `format-iteration-entry-entries` reads. No `identityHashCode`
   anywhere — identical content always produces identical fingerprint,
   so completed iterations hit the cache forever even when the parent
   `:iterations` vec is rebuilt by `(vec (vals @timeline))` on every
   progress chunk."
  [{:keys [thinking forms recaps provider-fallbacks error repeat-count]}]
  [(text-fingerprint thinking)
   (mapv form-fingerprint forms)
   recaps
   provider-fallbacks
   (when error (select-keys error [:type :message]))
   repeat-count])

(defn- short-id-fragment
  ^String [id]
  (let [s (str (or id ""))]
    (subs s 0 (min 8 (count s)))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} detail-expanded?
  ([detail-expansions session-id node-id]
   (detail-expanded? detail-expansions session-id node-id true))
  ([detail-expansions session-id node-id default-expanded?]
   (boolean
     (or (:vis.channel-tui/expand-all-details? detail-expansions)
       (get detail-expansions
         [(str session-id) (str node-id)]
         default-expanded?)))))

(defn- hidden-size-hint
  ^String [entries]
  (let [line-count (count entries)
        char-count (reduce + 0 (map (comp count :line) entries))]
    (cond
      (> line-count 1) (str line-count " lines hidden")
      (pos? char-count) (str char-count " chars hidden")
      :else "empty")))

(defn- detail-id-suffix
  ;; User-facing badge displayed at the right edge of disclosure rows.
  ;; Per PLAN §2.8 + §2.9 + §2.10 + §5.1:
  ;;   - Render positions (ints), never UUIDs.
  ;;   - Format: `[turn 7 · iteration 3 · block 0 · tool · v/patch]`
  ;;   - Lowercase level words, dot separator (·) per PLAN §2.10.
  ;;   - Optional :role and :op-symbol segments after the positions.
  ;;   - No abbreviations: "iteration" not "iter".
  ^String [{:keys [turn-position iteration-number block-number
                   role op-symbol details-path]}]
  (let [parts (cond-> []
                (some? turn-position) (conj (str "turn " turn-position))
                iteration-number      (conj (str "iteration " iteration-number))
                block-number          (conj (str "block " block-number))
                role                  (conj (name role))
                op-symbol             (conj (str op-symbol))
                (seq details-path)    (conj (str "details " (str/join "." details-path))))]
    (if (seq parts)
      (str "[" (str/join " · " parts) "]")
      "[details]")))

(defn- ellipsize-cols
  ^String [s max-w]
  (cond
    (<= max-w 0) ""
    (<= (p/display-width s) max-w) s
    (= max-w 1) "..."
    :else (str (p/truncate-cols s (dec max-w)) "...")))

(defn- format-detail-summary-line
  "Put the human-readable detail info on the right edge. The
   whole row is already painted as a bold disclosure band by
   `draw-chat-bubble!`, so keep the suffix as plain text. Inline code
   would drop that inherited bold style and switch to a weaker code
   background."
  ^String [left suffix max-w]
  (let [suffix-w (p/display-width suffix)
        gap-w    2]
    (if (> (+ suffix-w gap-w 1) max-w)
      (str left " / " suffix)
      (let [left-w (max 1 (- max-w suffix-w gap-w))
            left   (ellipsize-cols left left-w)
            pad-w  (max gap-w (- max-w (p/display-width left) suffix-w))]
        (str left (repeat-str \space pad-w) suffix)))))

(defn- detail-node-base-id
  ^String [{:keys [session-turn-id iteration-number block-number section kind]}]
  (str
    (or (some-> section name) "answer")
    (when session-turn-id (str ":t" (short-id-fragment session-turn-id)))
    (when iteration-number (str ":i" iteration-number))
    (when block-number (str ":b" block-number))
    (when kind (str ":" (name kind)))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} detail-node-id
  ^String [{:keys [details-path] :as detail-ctx}]
  (str
    (detail-node-base-id detail-ctx)
    (when (seq details-path) (str ":d" (str/join "." details-path)))))

(defn- relevant-detail-expansions-key
  "Stable cache key for only the disclosure nodes this Markdown projection
   can render. A click in one old answer must not bust cached projections for
   every other visible answer. Scope by session + node-id base."
  [opts]
  (let [session-id (some-> (:session-id opts) str)
        base            (detail-node-base-id opts)
        prefix          (str base ":")]
    (->> (:detail-expansions opts)
      (keep (fn [[k expanded?]]
              (when (vector? k)
                (let [[cid node-id] k
                      node-id (str node-id)]
                  (when (and (= session-id (str cid))
                          (or (= base node-id)
                            (str/starts-with? node-id prefix)))
                    [node-id expanded?])))))
      sort
      vec)))

(defn- turn-detail-expansions-key
  "Stable cache key for any disclosure belonging to this rendered assistant
   turn. Used by the outer trace+answer projection, which may contain thinking,
   iteration, tool/result, and final-answer disclosures."
  [opts]
  (let [session-id (some-> (:session-id opts) str)
        turn-fragment   (some-> (:session-turn-id opts) short-id-fragment)
        turn-token      (when turn-fragment (str ":t" turn-fragment))]
    (->> (:detail-expansions opts)
      (keep (fn [[k expanded?]]
              (when (vector? k)
                (let [[cid node-id] k
                      node-id (str node-id)]
                  (when (and (= session-id (str cid))
                          (or (nil? turn-token)
                            (str/includes? node-id turn-token)))
                    [node-id expanded?])))))
      sort
      vec)))

(defn- tool-detail-badge
  "Compose the short summary line painted on tool-result rows.
   Label = OBSERVATION/MUTATION (derived from `:tag`); suffix =
   op-name. No per-op badge field anywhere."
  [detail]
  (when-let [tag (some-> detail :tag)]
    (let [label (case tag
                  :observation "OBSERVATION"
                  :mutation    "MUTATION"
                  nil)
          op    (:op detail)]
      (when label
        (str label (when (and op (not (#{:all :any} op)))
                     (str " " (name op))))))))

(defn- self-describing-tool-result?
  "Whether the op's body output speaks for itself (shell output,
   search hits, file listings) so the channel SHOULD skip the
   redundant badge row. Declared by the extension via
   `extension/register-op!` `:self-describing?`."
  [detail]
  (true? (:self-describing? detail)))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} detail-summary-entries
  [{:keys [marker max-w summary hidden-entries collapsed? session-id node-id color-role]
    :as detail-ctx}]
  (let [suffix    (detail-id-suffix detail-ctx)
        hint      (when collapsed? (str " / " (hidden-size-hint hidden-entries)))
        summary   (or summary "Details")
        left      (str (if collapsed? "▸ " "▾ ")
                    summary
                    (or hint ""))
        visible   (format-detail-summary-line left suffix (max 1 max-w))
        ;; Lift visible-label string through the IR walker so inline
        ;; emphasis (`**bold**`, `` `code` ``, etc.) renders with
        ;; sentinel-wrapped runs the painter understands; legacy
        ;; `markdown->inline` regex parser is gone.
        wrapped   (wrap-text (ir-tui/ir->inline-sentinel-string
                               (vis/markdown->ir visible))
                    (max 1 max-w))
        meta      {:kind :toggle-details
                   :session-id (str session-id)
                   :node-id (str node-id)
                   :collapsed? collapsed?
                   :color-role color-role}]
    (mapv (fn [line] {:line (str marker line) :meta meta}) wrapped)))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} auto-collapse-needed?
  [lines raw-text]
  (or (> (count lines) auto-collapse-line-threshold)
    (> (count (str (or raw-text ""))) auto-collapse-char-threshold)))

(defn- channel-ir?
  [x]
  (and (vector? x) (= :ir (first x))))

(defn- channel-body-blank?
  [x]
  (cond
    (nil? x) true
    (channel-ir? x) (<= (count (vis/->ast x)) 2)
    :else (str/blank? (str x))))

(defn- channel-body-plain-text
  [x]
  (if (channel-ir? x) (vis/render x :plain) (str x)))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} channel-body-copy-text
  ;; Retained alongside `tag-copy-block-body` for callers that still pass
  ;; disclosure-copy payloads. The toggle UI itself is gone, but the
  ;; helpers stay so external code can compose its own copy flow.
  [x]
  (if (channel-ir? x) (vis/render x :markdown) (str x)))

(defn- ir-body-entries
  "Render canonical channel IR into painter entries. This path is IR-only:
   render-fn output must already be `[:ir ...]`; strings belong to
   raw value fallback paths, not tool/channel rendering.

   Channel IR renders in place. Do not stringify it and do not prepend
   generic result/body markers: plain IR paragraphs should inherit the
   surrounding assistant background, while explicit IR structure (code,
   headings, lists, tables) owns its own row styling.

   Returns `nil` for empty IR."
  [ir _body-marker max-w]
  (when (and (channel-ir? ir) (not (channel-body-blank? ir)))
    (let [entries (ir-tui/ir->entries (vis/->ast ir) max-w {:mode :channel})]
      (when (seq entries)
        entries))))

(defn- tag-copy-block-body
  "Stamp every body row of an expanded disclosure with copy metadata so a
   single click on the body lines copies the WHOLE disclosure body, not
   the entire enclosing assistant message. Rows that already carry meta
   (nested toggle-details, links, ...) keep theirs - those have their
   own click handling and must not be hijacked."
  [entries node-id text]
  (if (or (nil? node-id) (str/blank? (str text)))
    entries
    (let [body-meta {:kind :copy-block-body
                     :node-id (str node-id)
                     :text    (str text)}]
      (mapv (fn [{:keys [meta] :as e}]
              (if meta e (assoc e :meta body-meta)))
        entries))))

(defn- marker-prefix?
  "True when the first char of `line` is a paint-zone marker the
   renderer prepends (zero-width / format codepoints from
   `primitives` like `MARKER_RESULT`, ...). Plain
   answer-markdown lines never start with one, so the test lets us
   strip markers without nibbling the first letter of regular prose."
  ^Boolean [^String line]
  (and (string? line)
    (pos? (count line))
    (= (int Character/FORMAT)
      (int (Character/getType (.charAt line 0))))))

(def ^:private chrome-meta-kinds
  ;; Row kinds that paint display-only chrome (`▾ SUMMARY [Turn: ...]`).
  ;; Skipped when reconstructing the user-facing body text so nested
  ;; disclosure copy doesn't drag the visual summary glyph + details
  ;; suffix into the clipboard.
  #{:toggle-details})

(defn- ^{:clj-kondo/ignore [:unused-private-var]} entries->body-text
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
           (cond
             (not (string? line))   ""
             (zero? (count line))   ""
             (marker-prefix? line)  (subs line 1)
             :else                  line)))
    (str/join "\n")
    str/trim))

(defn- maybe-collapse-block
  "Always render the body fully inline. Collapsible disclosure rows
   were removed per user directive: no `▾`/`▸` toggle, no auto-collapse
   threshold. The function name is retained for call-site compatibility
   and reads as a historical no-op."
  [{:keys [body-marker lines max-w raw-text]}]
  (let [body-value (or raw-text (str/join "\n" lines))
        ir-entries (when (channel-ir? body-value)
                     (ir-body-entries body-value body-marker max-w))]
    (or ir-entries
      (mapv (fn [line] {:line (str body-marker line) :meta nil}) lines))))

(defn- maybe-collapse-thinking-entries
  "Always return reasoning entries inline. Collapsible disclosure was
   removed per user directive — reasoning stays fully visible whether
   streaming or finalized."
  [{:keys [entries]}]
  (vec entries))

(defn- markdown-fence-marker-line?
  "True for standalone Markdown fence opener/closer lines. Tool
   results are output, not prose; TUI result panes must not parse or
   paint these as Markdown. We drop only the fence marker rows, keeping
   the payload lines literal."
  [line]
  (let [s (str/triml (str line))]
    (or (str/starts-with? s "```")
      (str/starts-with? s "~~~"))))

(defn- strip-markdown-fence-marker-lines
  "Remove Markdown fence marker rows from result text while preserving
   the fenced body exactly. Applied to successful result bodies before
   wrapping so ` ```diagram ` / ` ```diff ` never trigger TUI Markdown
   structural rendering in result panes."
  [text]
  (->> (str/split-lines (str text))
    (remove markdown-fence-marker-line?)
    (str/join "\n")))

(defn- maybe-collapse-raw-text-block
  "Render a result block fully inline. Collapsible disclosure was
   removed per user directive — the body always paints in place."
  [{:keys [raw-text max-w] :as opts}]
  (let [raw-value (if (channel-ir? raw-text) raw-text (str/trim (str raw-text)))]
    (when-not (channel-body-blank? raw-value)
      (let [lines (when-not (channel-ir? raw-value)
                    (wrap-text (channel-body-plain-text raw-value) max-w))]
        (maybe-collapse-block (assoc opts :lines lines :raw-text raw-value))))))

(defn- strip-paint-markers-line
  "Return user-visible text for a prewrapped internal painter line.
   The TUI painter consumes these markers from `:lines`; `:text` is
   used by copy/debug/projection paths and must never expose PUA or
   bidi-control glyphs to the user."
  [line]
  (let [s (str (or line ""))
        s (if (and (pos? (count s))
                (let [c (.charAt ^String s 0)]
                  (or (= (int Character/FORMAT) (int (Character/getType c)))
                    (and (>= (int c) 0xE000) (<= (int c) 0xE0FF)))))
            (subs s 1)
            s)]
    (->> s
      (remove (fn [c]
                (let [i (int c)]
                  (<= 0xE110 i 0xE2FF))))
      (apply str))))

(defn- entries->payload
  [entries]
  (let [lines     (mapv :line entries)
        line-meta (mapv :meta entries)]
    {:lines     lines
     :line-meta line-meta
     :text      (str/join "\n" (map strip-paint-markers-line lines))}))

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
  (loop [acc [] i 0 remaining (vec iterations)]
    (if (empty? remaining)
      acc
      (let [head (first remaining)
            sig  (error-signature head)
            run  (if (nil? sig)
                   1
                   (count (take-while #(= sig (error-signature %)) remaining)))
            entry (cond-> head
                    (form-error-only-iteration? head)
                    (-> (assoc :error (form-error-only-error head))
                      (assoc :forms []))
                    true (assoc :repeat-count run))]
        (recur (conj acc [i entry])
          (+ i run)
          (subvec remaining run))))))

(defn- format-fallback-notice
  [{:keys [reason failed-provider new-provider] :as notice}]
  (let [event? (contains? notice :event/type)
        failed-provider (or failed-provider notice)
        retry? (= :llm.routing/provider-retry (:event/type notice))
        failed-id (if event?
                    (or (:from-provider notice) (:provider notice))
                    (or (some-> (:id failed-provider) name)
                      (some-> (:provider-id failed-provider) name)
                      (some-> (:provider failed-provider) name)
                      "unknown"))
        failed-model (if event? (or (:from-model notice) (:model notice)) (:model failed-provider))
        new-id (if event?
                 (:to-provider notice)
                 (or (some-> (:id new-provider) name)
                   (some-> (:provider-id new-provider) name)
                   (some-> (:provider new-provider) name)))
        new-model (if event? (:to-model notice) (:model new-provider))
        from (str failed-id (when failed-model (str "/" failed-model)))
        to   (when (or new-id new-model)
               (str (or new-id "unknown") (when new-model (str "/" new-model))))
        why  (or (:error notice) (:error failed-provider) (some-> reason name) "provider fallback")
        kind (if retry? "retry same provider" "provider fallback")
        delay (when (and retry? (:delay-ms notice))
                (str ", retry in " (long (/ (long (:delay-ms notice)) 1000)) "s"))]
    (str (if retry? "↻ " "↪ ") kind ": " from
      (when to (str " → " to))
      " — " why
      delay)))

(defn- fallback-recap
  [notice]
  (let [text (str/replace (format-fallback-notice notice) #"^[↻↪] " "")]
    (cond
      (str/starts-with? text "provider fallback:")
      (str "Provider fallback:" (subs text (count "provider fallback:")))

      (str/starts-with? text "retry same provider:")
      (str "Provider retry:" (subs text (count "retry same provider:")))

      :else
      (str "Provider: " text))))

(defn- provider-error-recap
  [error]
  (let [data (:data error)]
    (when (and (map? error) (or (:status data) (:body data) (:request-id data) (:request_id data)))
      (let [status (:status data)
            msg    (or (:message error) (some-> (:body data) str str/trim) (str (:type error)) "provider error")]
        (str "Provider error" (when status (str " HTTP " status)) ": " msg)))))

(defn- recap-entries
  [line-entry recaps fill-w]
  (when (seq recaps)
    (let [pad-w (max 1 (dec fill-w))]
      (vec
        (mapcat (fn [recap]
                  ;; Single leading space inside the header-bg zone gives
                  ;; Recap rows the same left breathing room as thinking
                  ;; content (`(str thinking-marker " " %)`), so the bubble
                  ;; no longer paints Recap text flush against the marker
                  ;; column. wrap-text width drops by 1 to compensate.
                  (map #(line-entry (str recap-marker " " %))
                    (wrap-text (str "* Recap: " recap) pad-w)))
          recaps)))))

(defn- code-source-from-render-segments
  [segments fallback-code]
  (let [sources (keep (fn [{:keys [kind source]}]
                        (when (and (= :code kind) (not (str/blank? (str source))))
                          (str/trim (str source))))
                  segments)]
    (if (seq sources)
      (str/join "\n" sources)
      fallback-code)))

(defn- render-segment-title-entries
  [line-entry segments fill-w]
  (let [pad-w (max 1 (dec fill-w))]
    (vec
      (mapcat (fn [{:keys [kind value]}]
                (when (= :title kind)
                  (let [title (if (str/blank? (str value))
                                "Title changed."
                                (str "Title changed to \"" value "\"."))]
                    ;; Mirror recap-entries: 1-col left padding inside the
                    ;; header-bg zone, wrap-text width compensated.
                    (map #(line-entry (str recap-marker " " %))
                      (wrap-text (str "* Recap: " title) pad-w)))))
        segments))))

(defn- format-iteration-entry-entries
  [entry
   code-width iteration-number
   & [{:keys [show-header? session-id detail-expansions session-turn-id now-ms live-preview?]
       :or   {show-header? false live-preview? false}}]]
  ;; Iteration / block header labels removed per user directive. The
  ;; `show-header?` argument is retained as a no-op for callers; we
  ;; never paint the right-aligned ITERATION N band any more.
  (let [{:keys [thinking forms recaps provider-fallbacks error repeat-count]} entry
        _ show-header?
        fill-w      (max 1 (dec code-width))
        line-entry  (fn [line] {:line line :meta nil})
        header      []
        ;; Margin-top above Recap fires ONLY when this iteration actually
        ;; carries one (user directive). Without recap-lines the iteration
        ;; starts flush; with recap-lines the bubble gets a neutral blank
        ;; row between the "Vis" label (or prior iteration) and the Recap
        ;; text, so Recap breathes the way thinking and code blocks do.
        raw-recap-lines (recap-entries line-entry
                          (cond-> (vec (or recaps []))
                            (seq provider-fallbacks) (into (map fallback-recap provider-fallbacks))
                            (provider-error-recap error) (conj (provider-error-recap error)))
                          fill-w)
        recap-lines (if (seq raw-recap-lines)
                      ;; Recap chrome paints on terminal-bg: there is
                      ;; no header-bg fill any more, so the call-out is
                      ;; carried purely by the bold + italic recap rows
                      ;; plus a single blank row above and below.
                      ;;   neutral    = top margin
                      ;;   recap-rows = bold + italic text
                      ;;   neutral    = bottom margin
                      ;; The neutral row above thinking comes from
                      ;; `thinking-lines` itself, which separates this
                      ;; recap block from the thinking that follows.
                      (vec (concat [(line-entry "")]
                             raw-recap-lines
                             [(line-entry "")]))
                      raw-recap-lines)
        thinking-lines
        (fn [thinking-text-or-texts]
          ;; Per user direction: do NOT truncate reasoning while it's
          ;; streaming live. The full reasoning text flows into the
          ;; bubble as it arrives. Post-stream collapse (the ▾ REASONING
          ;; summary toggle) still fires once the iteration completes
          ;; via `maybe-collapse-thinking-entries` below.
          (let [raw-texts (if (sequential? thinking-text-or-texts)
                            thinking-text-or-texts
                            [thinking-text-or-texts])
                texts     raw-texts
                entries (into []
                          (mapcat
                            (fn [thinking-text]
                              (when (and (string? thinking-text) (not (str/blank? thinking-text)))
                                ;; Thinking text comes from the LLM as plain markdown; lift to
                                ;; canonical IR via `vis/markdown->ir`, then walker over it in
                                ;; `:thinking` mode (uses the iter-header-bg / italic marker set).
                                (let [ir (vis/markdown->ir thinking-text)]
                                  (or (seq (ir-tui/ir->entries ir fill-w
                                             {:mode                 :thinking
                                              :session-id      session-id
                                              :session-turn-id session-turn-id
                                              :detail-expansions   detail-expansions
                                              :iteration-number    iteration-number
                                              :section             :thinking}))
                                    (mapv #(line-entry (str thinking-marker %))
                                      (wrap-text thinking-text fill-w)))))))
                          texts)]
            (when (seq entries)
              ;; Live reasoning stays visible while it streams; completed
              ;; reasoning defaults to a compact disclosure so system-like
              ;; chain-of-thought does not visually merge with the answer.
              ;; Layout shape mirrors code blocks:
              ;;   neutral blank row  = outside top margin
              ;;   thinking blank row = inside top padding
              ;;   content/summary rows
              ;;   thinking blank row = inside bottom padding
              (let [preview-entries (if live-preview?
                                      entries
                                      (maybe-collapse-thinking-entries
                                        {:entries              entries
                                         :session-id      session-id
                                         :detail-expansions   detail-expansions
                                         :session-turn-id session-turn-id
                                         :iteration-number    iteration-number
                                         :max-w               fill-w}))]
                (vec (concat [(line-entry "")
                              (line-entry (str thinking-marker ""))]
                       preview-entries
                       [(line-entry (str thinking-marker ""))]))))))
        error-lines
        (fn []
          (when (map? error)
            (let [repeat-count      (max 1 (long (or repeat-count 1)))
                  badge             (when (> repeat-count 1) (str "  x " repeat-count))
                  data              (:data error)
                  provider-error?   (or (:status data) (:body data) (:request-id data) (:request_id data))
                  hdr-label         (str (label-text (if provider-error? "provider error" "error")) (or badge ""))
                  hdr-pad           (max 0 (- fill-w (count hdr-label) 1))
                  hdr-line          (str iteration-hdr-marker (repeat-str \space hdr-pad) hdr-label " ")
                  err-message       (or (:message error) (str (:type error)) "unknown error")
                  err-headline      (if (> repeat-count 1)
                                      (str "ERROR x " repeat-count ": " err-message)
                                      (vis/format-error err-message))
                  raw               (some-> (get-in error [:data :raw-data]) str str/trim)
                  recv              (get-in error [:data :received-type])
                  body              (some-> (:body data) str str/trim)
                  status            (:status data)
                  request-id        (or (:request-id data) (:request_id data))
                  invalid-thinking? (and body (re-find #"(?i)invalid.*signature.*thinking block" body))
                  provider-rows     (when provider-error?
                                      (mapv #(line-entry (str err-result-marker %))
                                        (mapcat #(wrap-text % fill-w)
                                          (cond-> [(str "PROVIDER_ERROR" (when status (str "  HTTP " status))
                                                     (when request-id (str "  " request-id)))]
                                            invalid-thinking?
                                            (conj "WHAT HAPPENED: Anthropic rejected the request before the model ran because Vis sent a thinking block with a signature that is not valid for Anthropic. This usually means preserved-thinking from another provider/model was replayed into Anthropic.")
                                            (and body (not (str/blank? body)))
                                            (conj (str "provider response: " (if (> (count body) 1200)
                                                                               (str (subs body 0 1200) "...")
                                                                               body)))))))
                  err-message-rows  (mapv #(line-entry (str err-result-marker %))
                                      (wrap-text err-headline fill-w))
                  raw-rows          (when (and raw (not (str/blank? raw)))
                                      (let [hdr        (str "provider returned"
                                                         (when recv (str " (" recv ")"))
                                                         ":")
                                            raw-trim   (if (> (count raw) 600) (str (subs raw 0 600) "...") raw)
                                            body-lines (mapv #(line-entry (str err-result-marker %))
                                                         (wrap-text raw-trim fill-w))]
                                        (into [(line-entry (str err-result-marker hdr))] body-lines)))]
              (vec (concat
                     [(line-entry (str iteration-pad-marker ""))]
                     (when show-header? [(line-entry (str iteration-pad-marker ""))])
                     (when show-header? [(line-entry hdr-line)])
                     [(line-entry (str code-err-pad-marker ""))]
                     (or provider-rows err-message-rows)
                     (when (seq raw-rows) [(line-entry (str code-err-pad-marker ""))])
                     (or raw-rows [])
                     [(line-entry (str code-err-pad-marker ""))])))))
        form-lines
        (fn [form block-number]
          (let [{:keys [code comment render-segments result-render result-kind result-detail
                        error duration-ms started-at-ms success?]} form
                has-status?   (some? success?)
                is-error?     (and has-status? (not success?))
                duration-str  (vis/format-duration duration-ms)
                ;; BLOCK N header removed per user directive (also gated
                ;; on `show-header?` which is now always false). Keep
                ;; `expr-hdr` defined as empty so the existing `(when
                ;; show-header? ...)` branch is dead but type-safe.
                _expr-num     block-number
                expr-hdr      ""
                running-ms    (when (and started-at-ms now-ms)
                                (max 0 (- (long now-ms) (long started-at-ms))))
                running-ms    (when running-ms (* 1000 (quot running-ms 1000)))
                running-str   (or (some-> running-ms vis/format-duration) "0ms")
                status-text   (if has-status?
                                (str (if success? "✓" "✗")
                                  (when duration-str (str " " duration-str)))
                                (str "↻ " running-str))
                status-line   (let [s-marker code-status-marker
                                    pl       (max 0 (- fill-w (count status-text) 1))]
                                (line-entry (str s-marker (repeat-str \space pl) status-text " ")))
                c-marker      (cond
                                (not has-status?) code-marker
                                success?          code-ok-marker
                                :else             code-err-marker)
                c-pad         (cond
                                is-error? code-err-pad-marker
                                success?  code-ok-pad-marker
                                :else     code-pad-marker)
                comment-lines (when (and (string? comment)
                                      (not (str/blank? comment)))
                                (let [trimmed   (str/trim comment)
                                      ;; Form comments sit in their own thinking-style band above
                                      ;; the code block. Give the visible text the same one-column
                                      ;; left breathing room as code rows, without shifting all
                                      ;; reasoning/thinking rows globally.
                                      comment-w (max 1 (dec fill-w))
                                      wrapped   (mapcat (fn [line] (wrap-text line comment-w))
                                                  (str/split-lines trimmed))]
                                  (mapv #(line-entry (str thinking-marker " " %)) wrapped)))
                title-lines   (render-segment-title-entries line-entry render-segments fill-w)
                code-text     (str/trim (or (code-source-from-render-segments render-segments code) ""))
                inline-error-code-lines (when error
                                          (inline-error-context-lines code-text error))
                formatted     (format-clojure-ansi code-text fill-w)
                code-lines    (or inline-error-code-lines
                                (str/split-lines formatted))
                code-node-id  (when session-id
                                (detail-node-id {:session-turn-id session-turn-id
                                                 :iteration-number    iteration-number
                                                 :block-number        block-number
                                                 :section             :iteration
                                                 :kind                :code}))
                c-lines       (tag-copy-block-body
                                (mapv #(line-entry (str c-marker %)) code-lines)
                                code-node-id
                                code-text)
                raw-result-text (if (and (= :value result-kind)
                                      (not is-error?)
                                      (not (channel-ir? result-render)))
                                  (format-clojure-plain result-render fill-w)
                                  result-render)
                result-text     (if (and raw-result-text (not is-error?) (string? raw-result-text))
                                  (strip-markdown-fence-marker-lines raw-result-text)
                                  raw-result-text)
                inline-error-message-lines (when error
                                             (mapv #(line-entry (str c-marker %))
                                               (wrap-text (form-error-headline error) fill-w)))
                tool-badge    (tool-detail-badge result-detail)
                r-marker      (if is-error? err-result-marker result-marker)
                ;; Per user directive: only tool calls show a result
                ;; pane (the channel-render-fn output — "what the tool
                ;; changed"). Plain-value form results (`:result-kind
                ;; :value`) are hidden; errors keep their inline caret
                ;; treatment above and never reach this branch.
                show-result?  (and (= :tool result-kind)
                                result-text
                                (not (str/blank? (str result-text))))
                result-lines  (when show-result?
                                (let [color-role     (when (map? result-detail) (meta->color-role result-detail))
                                      detail-entries (maybe-collapse-raw-text-block
                                                       {:session-id      session-id
                                                        :detail-expansions   detail-expansions
                                                        :session-turn-id session-turn-id
                                                        :iteration-number    iteration-number
                                                        :block-number        block-number
                                                        :kind                :result
                                                        :summary             (or tool-badge "RESULT")
                                                        :color-role          color-role
                                                        :summary-marker      md-summary-marker
                                                        :body-marker         r-marker
                                                        :raw-text            result-text
                                                        :max-w               fill-w})
                                      badge-entry     (when (and tool-badge
                                                              (not (self-describing-tool-result? result-detail)))
                                                        {:line (str md-summary-marker tool-badge)
                                                         :meta {:kind :tool-badge
                                                                :color-role color-role}})]
                                  (vec (concat
                                         (when badge-entry [badge-entry])
                                         detail-entries))))
                code-block    (vec (concat
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
                                     (when show-header? [(line-entry (str iteration-hdr-marker expr-hdr))])
                                     (when (seq comment-lines)
                                       (concat [(line-entry (str thinking-marker ""))]
                                         comment-lines
                                         [(line-entry (str thinking-marker ""))]))
                                     [(line-entry (str c-pad ""))]
                                     c-lines
                                     (when (seq inline-error-message-lines) inline-error-message-lines)
                                     (when status-line [status-line])
                                     [(line-entry (str c-pad ""))]))
                result-margin nil]
            (vec (concat code-block result-margin result-lines))))
        grouped
        (when (seq forms)
          (let [code+result-lines
                (into []
                  (mapcat (fn [[idx form]]
                            (concat
                              (when (pos? idx) [(line-entry (str iteration-pad-marker ""))])
                              (form-lines form (inc idx))))
                    (map-indexed vector forms)))]
            (when (seq code+result-lines)
              (vec (concat
                     [(line-entry (str iteration-pad-marker ""))]
                     code+result-lines
                     [(line-entry (str iteration-pad-marker ""))])))))
        body (or grouped [])
        trailing-errors (error-lines)
        thinking-body (or (thinking-lines thinking) [])
        direct-thinking-code? (and (seq thinking-body) (seq body) (empty? trailing-errors))
        outer-iter-pad-top? (let [line (or (:line (first body)) "")
                                  body-text (if (pos? (count line)) (subs line 1) line)]
                              (and (pos? (count line))
                                (.startsWith ^String line ^String iteration-pad-marker)
                                (str/blank? body-text)))
        ;; When reasoning is immediately followed by code, drop ONLY
        ;; the outer iter-pad blank that wraps the form vector. The
        ;; thinking trailing pad keeps its gray stripe and the code
        ;; block keeps its own code-bg top pad, so the boundary paints
        ;; as thinking-bg blank + code-bg blank with no neutral seam.
        body (if (and direct-thinking-code? outer-iter-pad-top?)
               (vec (rest body))
               body)]
    ;; Layout: header (with optional ITERATION-N label) + recap lines
    ;; (which already include provider-fallback notices and any
    ;; provider-error recap) + collected thinking lines + any error
    ;; rows + body (per-form code/result pairs). Resume / live share
    ;; the same flat layout. When reasoning is immediately followed by
    ;; code, keep thinking bottom padding but trim duplicated code-side
    ;; pad rows so TUI shows exactly one blank row on the boundary.
    (into (vec (concat header recap-lines thinking-body trailing-errors))
      body)))

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
  (when-let [t (some-> error-data :type)]
    (let [bare (cond-> t (keyword? t) name)]
      (when (and (string? bare) (not (str/blank? bare)))
        (str/replace bare "-" " ")))))

(defn- progress-phase
  "Human-readable phase label for the current iteration state. Drives
   the spinner row text so the user can tell whether Vis is calling
   the provider, thinking, executing, retrying, or cancelling.

   Anthropomorphic `Vis is ...` phrasing reads as a status sentence
   instead of a system log line. No elapsed-time-driven escalation -
   wall-clock is already shown right next to this string in the
   spinner row, the user can read the seconds themselves."
  [iterations cancelling?]
  (let [n              (count iterations)
        last-iteration (last iterations)
        err            (:error last-iteration)
        activity       (:activity last-iteration)
        errored?       (some? err)
        thinking?      (and (not errored?)
                         (some? (:thinking last-iteration))
                         (not (str/blank? (:thinking last-iteration))))
        executing?     (and (not errored?) last-iteration (seq (:forms last-iteration)))]
    (cond
      cancelling? "Vis is cancelling"
      errored?    (let [label (prettify-error-type err)]
                    (str "Vis is retrying"
                      (when label (str " after " label))
                      " (iter " n ")"))
      (zero? n)   "Vis is calling the provider"
      (= :provider-call activity) (str "Vis is calling the provider (iter " n ")")
      (= :response-parse activity) (str "Vis is parsing model response (iter " n ")")
      thinking?   (str "Vis is thinking (iter " n ")")
      executing?  (str "Vis is running code (iter " n ")")
      :else       (str "Vis is working (iter " n ")"))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} trace-render-entries
  "Unified renderer for iteration traces in live, cancelled, and completed
   assistant bubbles. Live progress and final/cancel rendering must call this
   instead of formatting iterations themselves. The only caller-specific UI is
   the trailer after these entries (spinner, final answer, or cancelled note)."
  [{:keys [iterations content-w settings now-ms session-id
           session-turn-id detail-expansions live? suppress-trace?]
    :or   {live? false suppress-trace? false}}]
  (let [raw-iterations (or iterations [])
        iterations     (if (vector? raw-iterations) raw-iterations (vec raw-iterations))
        show-thinking? (get settings :show-thinking true)
        show-iterations? (get settings :show-iterations true)
        show-silent?   (get settings :show-silent false)
        ;; One trace renderer means one visual contract: no iteration/block
        ;; label bands in live, completed, or cancelled bubbles.
        show-iteration-headers? false
        ;; Per user directive: every iteration is always visible.
        grouped-iterations (collapse-repeated-error-runs iterations)
        visible-iterations grouped-iterations
        iter-entry-fn  (fn [[idx entry]]
                         (let [visible  (visible-iteration-entry entry show-silent?)
                               stripped (if show-thinking? visible (dissoc visible :thinking))
                               running? (and live? (iteration-running? stripped))
                               sec-bucket (when running? (quot (long (or now-ms 0)) 1000))
                               iter-num (inc (long idx))
                               detail-scope-opts {:section :iteration
                                                  :iteration-number iter-num
                                                  :session-id session-id
                                                  :session-turn-id session-turn-id
                                                  :detail-expansions detail-expansions}
                               k [::iter-entries
                                  (if live? :live :final)
                                  iter-num
                                  (iteration-fingerprint stripped)
                                  (long content-w)
                                  show-iteration-headers?
                                  (boolean show-thinking?)
                                  (boolean show-silent?)
                                  session-id
                                  session-turn-id
                                  (relevant-detail-expansions-key detail-scope-opts)
                                  sec-bucket]
                               inner-opts {:show-header?         show-iteration-headers?
                                           :now-ms               (when running? now-ms)
                                           :session-id      session-id
                                           :session-turn-id session-turn-id
                                           :detail-expansions   detail-expansions
                                           :live-preview?        live?}
                               render!    #(format-iteration-entry-entries
                                             stripped content-w iter-num inner-opts)]
                           (if live?
                             (cached* k render!)
                             (render!))))]
    (when (and show-iterations? (not suppress-trace?) (seq iterations))
      (vec (mapcat iter-entry-fn visible-iterations)))))

(defn- queued-preview
  [text]
  (let [s (-> (str (or text ""))
            (str/replace #"\s+" " ")
            str/trim)]
    (if (> (count s) 240)
      (str (subs s 0 240) "…")
      s)))

(defn- queued-progress-entries
  [pending-sends content-w]
  (let [queued (vec (or pending-sends []))]
    (when (seq queued)
      (let [line-entry (fn [line] {:line line :meta nil})
            preview-lines
            (mapcat (fn [entry]
                      (map #(str thinking-marker %)
                        (wrap-text (queued-preview (:text entry)) content-w)))
              queued)]
        (mapv line-entry (concat [""] preview-lines))))))

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
   (let [raw-iterations   (or (:iterations progress) [])
         iterations       (if (vector? raw-iterations) raw-iterations (vec raw-iterations))
         content-w        (max 10 (- bubble-w 4))
         {:keys [now-ms turn-start-ms cancelling? session-id
                 session-turn-id detail-expansions viewport-rows pending-sends]} extra
         now-ms           (long (or now-ms (System/currentTimeMillis)))
         elapsed-ms       (when turn-start-ms
                            (max 0 (- now-ms (long turn-start-ms))))
         elapsed-str      (or (vis/format-duration elapsed-ms) "0ms")
         spinner-line     (str (spinner-frame now-ms) "  "
                            (progress-phase iterations cancelling?) "...  "
                            elapsed-str "  /  Esc to cancel")
         line-entry       (fn [line] {:line line :meta nil})
         trace-entries    (trace-render-entries
                            {:iterations iterations
                             :content-w content-w
                             :settings settings
                             :now-ms now-ms
                             :viewport-rows viewport-rows
                             :session-id session-id
                             :session-turn-id session-turn-id
                             :detail-expansions detail-expansions
                             :live? true})
         queued-entries   (queued-progress-entries pending-sends content-w)
         ;; Top margin invariant: the spinner row always has ONE blank
         ;; line above it inside the bubble, regardless of whether any
         ;; iterations have been recorded yet. Without this the iter-0
         ;; "Vis is calling the provider" state sits flush against the
         ;; bubble's top border while every subsequent state (iter≥1,
         ;; where trace-entries naturally end with a blank) gets a row
         ;; of breathing room — a visible jump the moment the first
         ;; iteration lands. Keeping the blank in both branches makes
         ;; the bubble height transition smooth and the spinner
         ;; vertically anchored.
         base-entries     (if (seq trace-entries)
                            (conj (conj trace-entries (line-entry ""))
                              (line-entry spinner-line))
                            [(line-entry "") (line-entry spinner-line)])
         entries          (vec (concat base-entries queued-entries))]
     (entries->payload entries))))

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
             {:got-type (some-> ir class .getName)
              :got-preview (let [s (pr-str ir)]
                             (subs s 0 (min 200 (count s))))}))))

(defn- ir-non-empty? [ir]
  (and (vector? ir) (= :ir (first ir)) (> (count ir) 2)))

(defn- provider-error-answer? [ir]
  (boolean (and (vector? ir)
             (= :ir (first ir))
             (true? (get-in ir [1 :vis/provider-error])))))

(defn format-answer-with-thinking-data*
  "Uncached implementation. Returns `{:text :lines :line-meta}` so the
   bubble painter can keep clickable summary-row metadata aligned with
   the already-wrapped lines.

   STRICT: `answer` is canonical answer-IR (`[:ir & nodes]`) or nil."
  [answer trace bubble-w settings _confidence cancelled? opts]
  (let [content-w               (max 10 (- bubble-w 4))
        fill-w                  (max 1 (dec content-w))
        line-entry              (fn [line] {:line line :meta nil})
        _                       (assert-canonical-ir! answer)
        suppress-trace?         (provider-error-answer? answer)
        trace-entries           (trace-render-entries
                                  {:iterations trace
                                   :content-w content-w
                                   :settings settings
                                   :session-id (:session-id opts)
                                   :session-turn-id (:session-turn-id opts)
                                   :detail-expansions (:detail-expansions opts)
                                   :suppress-trace? suppress-trace?})
        ;; IR walker emits painter-ready entries directly. No
        ;; markdown round-trip, no `markdown->entries` rebuild.
        ans-entries             (if (ir-non-empty? answer)
                                  (vec (ir-tui/ir->entries answer (max 1 (- fill-w 2))
                                         {:session-id      (:session-id opts)
                                          :session-turn-id (:session-turn-id opts)
                                          :detail-expansions   (:detail-expansions opts)
                                          :section             :answer}))
                                  [])
        ans-pad                 (line-entry (str answer-pad-marker ""))
        cancel-text             (if (ir-non-empty? answer)
                                  (str/trim (vis/extract-text answer))
                                  "Cancelled by user.")
        cancel-rows             (mapv line-entry (wrap-text cancel-text (max 1 (- fill-w 2))))
        ;; Answer layout shape mirrors code blocks:
        ;;   neutral blank row = outside top margin (unless the trace
        ;;                       already ended with a neutral margin row)
        ;;   answer-pad row    = inside top padding on answer bg
        ;;   answer rows
        ;;   answer-pad row    = inside bottom padding on answer bg
        ;; A code-bearing iteration often already ends with a neutral
        ;; `iteration-pad-marker`; a thinking-only iteration ends with
        ;; a thinking-bg pad, so it still needs the neutral answer margin.
        has-trace?              (seq trace-entries)
        neutral-margin-entry?   (fn [entry]
                                  (let [line (:line entry)]
                                    (or (= "" line)
                                      (= iteration-pad-marker line))))
        answer-top-margin       (when-not (and has-trace?
                                            (neutral-margin-entry? (peek trace-entries)))
                                  (line-entry ""))
        cancel-block            (vec (concat
                                       (when answer-top-margin [answer-top-margin])
                                       cancel-rows
                                       [(line-entry "")]))
        answer-block            (if has-trace?
                                  (cond-> []
                                    answer-top-margin (conj answer-top-margin)
                                    :always           (conj ans-pad)
                                    :always           (into ans-entries)
                                    :always           (conj ans-pad))
                                  (-> [(line-entry "")]
                                    (cond->
                                      :always (into ans-entries))))
        trailer                 (if cancelled? cancel-block answer-block)
        entries                 (if has-trace?
                                  (vec (concat trace-entries trailer))
                                  (vec trailer))]
    (entries->payload entries)))

(defn format-answer-with-thinking*
  [answer trace bubble-w settings confidence cancelled?]
  (:text (format-answer-with-thinking-data* answer trace bubble-w settings confidence cancelled? nil)))

(defn format-answer-with-thinking-data
  ([answer trace bubble-w] (format-answer-with-thinking-data answer trace bubble-w nil nil false nil))
  ([answer trace bubble-w settings] (format-answer-with-thinking-data answer trace bubble-w settings nil false nil))
  ([answer trace bubble-w settings confidence] (format-answer-with-thinking-data answer trace bubble-w settings confidence false nil))
  ([answer trace bubble-w settings confidence cancelled?] (format-answer-with-thinking-data answer trace bubble-w settings confidence cancelled? nil))
  ([answer trace bubble-w settings confidence cancelled? opts]
   (cached* [::fawt-data
             (System/identityHashCode answer)
             (System/identityHashCode trace)
             (long bubble-w)
             (boolean (get settings :show-thinking true))
             (boolean (get settings :show-iterations true))
             (boolean (get settings :show-silent false))
             confidence
             (boolean cancelled?)
             (:session-turn-id opts)
             (turn-detail-expansions-key opts)
             ;; tail-lines opt switches to the back-walking renderer;
             ;; must be in the cache key so a tail-pinned bubble's
             ;; tail-N result doesn't shadow the same bubble's full
             ;; render after the user scrolls up.
             (:tail-lines opts)]
     #(format-answer-with-thinking-data* answer trace bubble-w settings confidence cancelled? opts))))

(defn format-answer-with-thinking
  ([answer trace bubble-w] (format-answer-with-thinking answer trace bubble-w nil nil false nil))
  ([answer trace bubble-w settings] (format-answer-with-thinking answer trace bubble-w settings nil false nil))
  ([answer trace bubble-w settings confidence] (format-answer-with-thinking answer trace bubble-w settings confidence false nil))
  ([answer trace bubble-w settings confidence cancelled?] (format-answer-with-thinking answer trace bubble-w settings confidence cancelled? nil))
  ([answer trace bubble-w settings confidence cancelled? opts]
   (:text (format-answer-with-thinking-data answer trace bubble-w settings confidence cancelled? opts))))

(defn- user-prompt-margin-entry?
  [entry]
  (let [visible (strip-paint-markers-line (:line entry))]
    (or (str/blank? visible)
      (boolean (re-matches #"^\s*│\s*$" visible)))))

(defn- trim-user-prompt-margin-entries
  [entries]
  (let [trimmed-leading  (vec (drop-while user-prompt-margin-entry? entries))
        trimmed-trailing (vec (reverse (drop-while user-prompt-margin-entry?
                                         (reverse trimmed-leading))))]
    trimmed-trailing))

(defn format-answer-markdown-data*
  [answer bubble-w opts]
  (assert-canonical-ir! answer)
  (let [content-w (max 10 (- bubble-w 4))
        raw-entries (if (ir-non-empty? answer)
                      (vec (ir-tui/ir->entries answer content-w opts))
                      [])
        ;; Assistant answers keep a blank margin row so the first line of
        ;; answer content is never flush against the top of the bubble or the
        ;; bottom of a preceding user message. User prompts already have their
        ;; own bubble padding; adding an answer margin there creates a visible
        ;; blank line inside the quoted prompt block.
        entries (if (= :user (:section opts))
                  (trim-user-prompt-margin-entries raw-entries)
                  (into [{:line "" :meta nil}] raw-entries))]
    (entries->payload entries)))

(defn format-answer-markdown*
  [answer bubble-w]
  (:text (format-answer-markdown-data* answer bubble-w nil)))

(defn format-answer-markdown-data
  ([answer bubble-w] (format-answer-markdown-data answer bubble-w nil))
  ([answer bubble-w opts]
   (cached* [::fam-data
             (System/identityHashCode answer)
             (long bubble-w)
             (:session-turn-id opts)
             (relevant-detail-expansions-key opts)
             ;; see comment in format-answer-with-thinking-data
             (:tail-lines opts)]
     #(format-answer-markdown-data* answer bubble-w opts))))

(defn format-answer-markdown
  ([answer bubble-w] (format-answer-markdown answer bubble-w nil))
  ([answer bubble-w opts]
   (:text (format-answer-markdown-data answer bubble-w opts))))

;;; ── Messages area (bubble-based) ───────────────────────────────────────────

;; -- Messages-area layout constants ----------------------------------------
;;
;; PUBLIC because `screen.clj` must compute bubble width with the same
;; gutter math the painter uses; if the two layers disagree by even one
;; column, `format-iteration-entry` sizes labels (`BLOCK 3`, `✓ 3ms`,
;; `FINAL ANSWER`) for one bubble-w while `draw-chat-bubble!` paints
;; into a different bubble-w and the right-aligned labels wrap onto
;; two lines.
;;
;; Single source of truth lives here; the consumer in `screen.clj` is
;; required to derive its own width from `MESSAGE_SIDE_PAD` rather than a
;; literal.
(def ^:const MESSAGE_MARGIN_TOP    1)  ;; rows above first message
(def ^:const MESSAGE_MARGIN_BOTTOM 1)  ;; rows below last message
(def ^:const MESSAGE_MARGIN_LEFT   2)  ;; cols left gutter
(def ^:const MESSAGE_MARGIN_RIGHT  3)  ;; cols right gutter (1 col padding before
;; the scrollbar at cols-2, then 1 col edge after).
(def ^:const MESSAGE_SIDE_PAD      (+ MESSAGE_MARGIN_LEFT MESSAGE_MARGIN_RIGHT))

(defn scrollbar-thumb-geometry
  "Pure thumb-row math, shared by the painter (`draw-messages-area!`)
   and the click-detector (`screen.clj` mouse handler) so the two
   layers can NEVER disagree about which rows belong to the thumb.

   Inputs:
   - `total-h`   total rendered height of all message bubbles.
   - `inner-h`   visible viewport height.
   - `track-h`   optional scrollbar track height; defaults to `inner-h`.
   - `scroll`    current row offset; `nil` means auto-bottom.

   Returns `{:thumb-top-rel long :thumb-h long :max-scroll long}`,
   where `:thumb-top-rel` is rows from the TOP of the track (caller
   adds the absolute `bar-top` to convert to screen coordinates).
   The visible thumb is intentionally one terminal cell high: Terminal.app
   renders stacked full-block cells as a ladder of separate boxes when
   the window is tall, which reads as multiple scroll positions instead
   of one current-position marker. Returns `nil` when there's no overflow
   - no thumb is drawn, no click should hit-test as on-thumb."
  ([^long total-h ^long inner-h scroll]
   (scrollbar-thumb-geometry total-h inner-h inner-h scroll))
  ([^long total-h ^long inner-h ^long track-h scroll]
   (when (and (pos? inner-h) (pos? track-h) (> total-h inner-h))
     (let [max-scroll (max 1 (- total-h inner-h))
           eff-scroll (let [s (long (or scroll max-scroll))]
                        (max 0 (min s max-scroll)))
           thumb-h    1
           thumb-top  (long (* (- track-h thumb-h)
                              (/ (double eff-scroll) max-scroll)))]
       {:thumb-top-rel thumb-top
        :thumb-h       thumb-h
        :max-scroll    max-scroll}))))
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
  (let [text-top   (+ box-top MESSAGE_MARGIN_TOP)
        inner-h    (max 0 (- box-bottom text-top MESSAGE_MARGIN_BOTTOM))
        bubble-w   (max 1 (- cols MESSAGE_SIDE_PAD))
        total-h    (long (:total-h layout))
        eff-scroll (long (:eff-scroll layout))
        visible    (:visible layout)]

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

    (let [clip (.newTextGraphics g
                 (TerminalPosition. 0 text-top)
                 (TerminalSize. cols inner-h))]
      (doseq [{:keys [^long top projected]} visible]
        (draw-chat-bubble! clip projected top
          MESSAGE_MARGIN_LEFT bubble-w
          {:viewport-top text-top :viewport-h inner-h}))

      (let [bar-top box-top
            track-h (max 0 (- box-bottom box-top))]
        (when-let [{:keys [thumb-top-rel thumb-h]}
                   (scrollbar-thumb-geometry total-h inner-h track-h eff-scroll)]
        ;; Place the scrollbar inside the right gutter so it never
        ;; overlaps message content. The track spans the whole message
        ;; panel, including top/bottom breathing-room rows; otherwise a
        ;; one-row blank gap appears above the scrollbar.
          (let [bar-col (- cols 2)
                bar-top bar-top
                track-h track-h]
            (doseq [r (range track-h)]
              (p/set-colors! g t/border-fg t/terminal-bg)
              (p/set-char! g bar-col (+ bar-top r) Symbols/SINGLE_LINE_VERTICAL))
            (doseq [r (range thumb-h)]
              (p/set-colors! g t/dialog-hint-key t/terminal-bg)
              (p/set-char! g bar-col (+ bar-top thumb-top-rel r) \u2588))))))))
