(ns com.blockether.vis.ext.channel-tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.markdown :as md-repair]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.links :as links]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
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
;; `:answer` / `:trace` / `:text` slot is a valid stand-in for
;; content.
;;
;; Eviction is proper LRU via `LinkedHashMap` in access-order mode:
;; each `get` promotes its entry to the most-recently-used end, and
;; `removeEldestEntry` returns true once the map exceeds `fmt-cache-cap`,
;; so `put` automatically drops the single least-recently-used entry.
;; That keeps the working set hot (the bubbles you're currently
;; scrolling through) while the cache stays bounded — no "oh you got
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
  ;; LinkedHashMap is not thread-safe — the render thread used to be
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
  ;; the second one's `(or (.get …) …)` discards its result and
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
   `(apply str (repeat n ch))` — bypasses lazy-seq + per-char
   StringBuilder appends, so the box-border + padding code paths stop
   showing up on the flame graph."
  ^String [ch ^long n]
  (if (<= n 0) "" (String. (char-array n ch))))

;;; ── Text wrapping ───────────────────────────────────────────────────────────

(defn- structural-line-marker?
  "True for invisible first-column markers consumed by `draw-chat-bubble!`.
   These markers select a row background/style. When wrapping a long marked
   row, every continuation must keep the same marker; otherwise only the first
   visual row paints the code/result/stdout/thinking background and the rest
   falls through as plain assistant text."
  [^Character ch]
  (boolean
    (or (#{\u200B \u200C \u200D \uFEFF} ch)
      (<= (int \u2060) (int ch) (int \u206F))
      (<= (int \uE001) (int ch) (int \uE02C)))))

(defn- split-structural-line-marker
  [^String line]
  (when (and (string? line) (pos? (count line)))
    (let [ch (.charAt line 0)]
      (when (structural-line-marker? ch)
        [(subs line 0 1) (subs line 1)]))))

(def ^:private inline-style-order [:bold :italic :strike :code])

(def ^:private inline-style->sentinels
  {:bold   [p/INLINE_BOLD_ON p/INLINE_BOLD_OFF]
   :italic [p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF]
   :strike [p/INLINE_STRIKE_ON p/INLINE_STRIKE_OFF]
   :code   [p/INLINE_CODE_ON p/INLINE_CODE_OFF]})

(def ^:private inline-sentinel->transition
  {p/INLINE_BOLD_ON    [:on :bold]
   p/INLINE_BOLD_OFF   [:off :bold]
   p/INLINE_ITALIC_ON  [:on :italic]
   p/INLINE_ITALIC_OFF [:off :italic]
   p/INLINE_STRIKE_ON  [:on :strike]
   p/INLINE_STRIKE_OFF [:off :strike]
   p/INLINE_CODE_ON    [:on :code]
   p/INLINE_CODE_OFF   [:off :code]})

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
   zero-width sentinels on each physical row. Otherwise a long inline command
   like `vis channels tui --conversation-id ...` starts with code styling on
   row 1, then every continuation paints as plain prose until the final row's
   CODE_OFF. Added sentinels are zero-width, so wrapping width is unchanged."
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
            ;; No space found — hard break at column boundary
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
        max-cols (max 0 (long max-cols))]
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
                        (do (.append sb \u00b7)
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
        ;; Only widen to approach φ — never heighten
        golden-w (int (* min-h phi))
        box-w (max min-w golden-w)
        box-h min-h
        ;; Clamp to terminal
        box-w (min box-w (- cols 4))
        box-h (min box-h (- rows 4))
        ;; Floor — minimum width generous enough for hint bars + labels
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
;; The previous bullet-point tuning history (4 → 5 → 4 → 3 → 2)
;; lived inline in `draw-box-border!`; lifting it to a const def keeps
;; the painter's body free of magic numbers and makes layout reviews
;; trivial — grep for `INPUT_BORDER_HORIZONTAL_PAD`.
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

         ;; Bottom: └──────┘ (plain rule — status moved to footer row).
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
       ;; embeds inside the padded bar.
       (let [pad        INPUT_BORDER_HORIZONTAL_PAD
             rule-w     (max 0 (- cols (* 2 pad)))
             padded-bar (repeat-str Symbols/SINGLE_LINE_HORIZONTAL rule-w)]
         (.putString g (int pad) (int box-top) (embed-in-bar padded-bar top-hint))
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
   terminal width — `text-w` = `cols` (clamped to ≥1)."
  [cols]
  (max 1 (- cols (* 2 input-pad-x))))

(defn- wrap-input-line
  "Soft-wrap one logical input line into visual segments at `text-w`
   columns. Always returns a non-empty vec (empty input → [\"\"])."
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
   number of hidden visual rows, matching pi's compact `N more` cue."
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
   is unchanged — wrapping is a render-time projection only.

   `hint` is the keybinding strip embedded in the TOP border. The
   bottom border is always a plain horizontal rule — live status
   (model / run-state / ctx %) lives in the dedicated footer row
   below this box (see `footer/draw-footer!`).

   No left/right side rails: the input area is framed by the top
   keybinding strip and the bottom rule only, so the typing zone
   sits flush with the message column on either side and the eye
   tracks the prompt directly without `│`-noise on the edges."
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

;; Line markers live in primitives — aliases for local readability.
(def ^:private thinking-marker  p/MARKER_THINKING)
(def ^:private code-marker      p/MARKER_CODE)
(def ^:private result-marker    p/MARKER_RESULT)
(def ^:private stdout-marker    p/MARKER_STDOUT)
(def ^:private sep-marker       p/MARKER_SEP)
(def ^:private code-ok-marker   p/MARKER_CODE_OK)
(def ^:private code-err-marker  p/MARKER_CODE_ERR)
(def ^:private err-result-marker p/MARKER_ERR_RESULT)
(def ^:private duration-marker  p/MARKER_DURATION)
(def ^:private iteration-hdr-marker  p/MARKER_ITERATION_HDR)
(def ^:private stdout-sep-marker p/MARKER_STDOUT_SEP)
(def ^:private stdout-pad-marker p/MARKER_STDOUT_PAD)
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

(def ^:private md-marker-sets
  "Per-mode marker bundle. Selected by `markdown->lines` mode arg.
   `:answer` lines render with answer-bg (no italic). `:thinking`
   lines render with iteration-header-bg + italic so the whole reasoning
   block reads as one dim, italicized region."
  {:answer   {:h1 md-h1-marker :h2 md-h2-marker :h3 md-h3-marker
              :bold md-bold-marker :code md-code-marker
              :bullet md-bullet-marker :quote md-quote-marker :hr md-hr-marker
              :thead md-table-head-marker :tsep md-table-sep-marker :trow md-table-row-marker
              :summary md-summary-marker
              :plain ""}
   :thinking {:h1 th-md-h1-marker :h2 th-md-h2-marker :h3 th-md-h3-marker
              :bold th-md-bold-marker :code th-md-code-marker
              :bullet th-md-bullet-marker :quote th-md-quote-marker :hr th-md-hr-marker
              :thead th-md-table-head-marker :tsep th-md-table-sep-marker :trow th-md-table-row-marker
              :summary th-md-summary-marker
              :plain thinking-marker}})

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

(defn- clojure-lang? [lang]
  (contains? #{"clj" "cljc" "cljs" "clojure" "edn"}
    (str/lower-case (str/trim (or lang "")))))

(defn- fence-lang [line]
  (let [trimmed (str/trim (or line ""))]
    (when (str/starts-with? trimmed "```")
      (str/trim (subs trimmed 3)))))

(defn- leading-space-count [^String s]
  (count (take-while #(= \space %) (or s ""))))

(defn- strip-leading-spaces-up-to [n line]
  (let [line (or line "")
        drop-n (min (long n) (leading-space-count line))]
    (subs line drop-n)))

(defn- nested-prefix
  [indent labels]
  (let [level (min 3 (quot (leading-space-count indent) 2))]
    (str (repeat-str \space (+ 2 (* level 2)))
      (nth labels level))))

(defn- format-clojure-ansi
  "Format Clojure/EDN source via zprint and keep zprint's ANSI syntax
   coloring. The TUI painter translates those ANSI SGR codes to
   Lanterna colors; raw ANSI is never written to the terminal.

   The source may be a complete file or fenced block with leading
   comments and multiple top-level forms. `fmt/format-clojure-ansi`
   owns the zprint source/file contract (`:parse-string-all?`).

   Live progress redraws on the spinner cadence while a provider call
   is in flight. Without this cache, every tick re-ran zprint over
   every already-finished form in the progress trace, so a long trace
   could make the TUI look frozen even though app-db had advanced."
  [code-text width]
  (let [code-text (str code-text)
        width     (long width)]
    (cached* [:clojure-ansi width code-text]
      #(fmt/format-clojure-ansi code-text width))))

(defn- code-block-lines
  ([m lang code-lines max-w]
   (code-block-lines m lang code-lines max-w ""))
  ([m lang code-lines max-w prefix]
   (let [prefix-w   (p/display-width prefix)
         code-w     (max 1 (- (long max-w) prefix-w))
         code-text  (str/join "\n" code-lines)
         body-lines (if (clojure-lang? lang)
                      (str/split-lines (format-clojure-ansi code-text code-w))
                      (mapcat #(wrap-text % code-w) code-lines))]
     (vec (concat [(str (:code m) prefix)]
            (map #(str (:code m) prefix %) body-lines)
            [(str (:code m) prefix)])))))

(defn- paint-ansi-line!
  "Paint a possibly ANSI-colored zprint line onto a Lanterna surface.
   ANSI foreground codes are translated to Lanterna foreground colors;
   `bg` is always controlled by Vis so success/running/error code
   zones keep their own background."
  [^TextGraphics g x y ^String line base-fg bg]
  (loop [i 0 col 0 fg base-fg]
    (if (>= i (.length line))
      g
      (let [esc-idx (str/index-of line "\u001b[" i)]
        (if (or (nil? esc-idx) (< i esc-idx))
          (let [end   (or esc-idx (.length line))
                chunk (subs line i end)]
            (p/set-colors! g fg bg)
            (p/put-str! g (+ x col) y chunk)
            (recur end (+ col (p/display-width chunk)) fg))
          (let [m-idx (str/index-of line "m" (+ esc-idx 2))]
            (if (nil? m-idx)
              (let [chunk (subs line esc-idx)]
                (p/set-colors! g fg bg)
                (p/put-str! g (+ x col) y chunk)
                g)
              (let [codes (parse-ansi-codes (subs line (+ esc-idx 2) m-idx))
                    fg*   (ansi-codes->fg codes fg base-fg)]
                (recur (inc m-idx) col fg*)))))))))

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
   thinking-mode body rows) applies to the text overdraw only — not
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

;; Forward declarations — the link-chrome painter + ref extractor
;; are defined further down (between the inline-marker tokeniser
;; and `bubble-height*`) but are reached from the prose-painting
;; loop in `draw-chat-bubble!` defined right below. Pulling the
;; definitions up here would split a long, cohesive block of
;; markdown / inline-marker code; the forward declares are the
;; cheaper move.
(declare extract-link-refs in-viewport? paint-link-chrome-row!
  paint-resources-badge! resources-badge-label)

;; `chrome-display-text` (further down) calls into `markdown->inline`
;; via `strip-inline-markup` so the chrome row doesn't show raw
;; `**` / `_` / backticks that the LLM put inside link bracket text.
;; The actual `markdown->inline` body lives in the inline tokeniser
;; section ~900 lines below; the existing forward-decl down there is
;; far too late for a chrome helper this high in the file. We pin
;; the var here so Clojure can resolve the symbol at compile time.
(declare markdown->inline markdown->lines)
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
   the conversation. Assistant content rows render on terminal bg —
   the only fills come from inline marker zones (code blocks,
   answer-bg, stdout, etc.).

   Returns the number of screen rows consumed (including spacing).

   Extra params:
     `viewport-top` and `viewport-h` describe the absolute screen
     window the messages-area paints into. They’re only consulted
     by the link-chrome painter to decide whether to register a
     click region for an off-screen chrome row (it doesn’t).
     Callers that paint outside `draw-messages-area!` (tests, REPL
     exploration) can pass `0 / 0` to disable click registration."
  [^TextGraphics g
   {:keys [role text timestamp duration-ms iteration-count tokens cost status] :as message}
   start-row left max-w
   & [{:keys [viewport-top viewport-h]
       :or   {viewport-top 0 viewport-h 0}}]]
  (let [user?     (= role :user)
        warning?  (warning-message? text)
        ;; Cancelled turns are status messages, not real answers —
        ;; render the entire bubble dim (gray + italic), drop the
        ;; meta line, dim the role label too. Skips markdown so a
        ;; bare text like \"Cancelled by user.\" reads naturally.
        cancelled? (= :cancelled status)
        turn-separator? (boolean (:turn-separator? message))
        top-sep-h (if turn-separator? 2 0)
        label     (if user? "You" "Vis")
        bubble-w  max-w
        ;; Symmetric inner padding (2 cols each side) inside the
        ;; message column. Applies to plain text AND to every
        ;; styled-marker zone (code blocks, stdout, answer, iteration
        ;; headers) so right-aligned labels like "ITERATION 1" /
        ;; "FINAL ANSWER" sit nicely inset from the right edge
        ;; instead of mashed against it.
        h-pad     2
        content-w (max 1 (- bubble-w (* 2 h-pad)))
        raw-lines (or (:prewrapped-lines message)
                    (and user? (markdown->lines text content-w :answer))
                    (wrap-text text content-w))
        lines     (clip-lines-preserving-markers raw-lines content-w)
        line-meta (or (:line-meta message)
                    (vec (repeat (count lines) nil)))
        bubble-h  (count lines)
        bx        left
        ;; No bg fill on plain assistant text — we sit on terminal-bg.
        ;; `:warning` and user messages each get a tinted block so
        ;; they're impossible to miss in the timeline. Cancelled turns
        ;; intentionally render on bare terminal-bg — the muted italic
        ;; fg + status footer carry the "aborted" signal without a
        ;; bubble-wide fill that competed visually with adjacent
        ;; assistant messages.
        bg-color  (cond
                    warning?   t/warning-bg
                    ;; User messages fill their content rows with a
                    ;; very pale warm-yellow block so "you said this"
                    ;; reads as its own zone, distinct from the white
                    ;; assistant area.
                    user?      t/user-bubble-bg
                    :else      t/terminal-bg)
        fg-color  (cond
                    cancelled? t/cancelled-fg
                    warning?   t/warning-fg
                    user?      t/user-bubble-fg
                    :else      t/ai-bubble-fg)
        role-fg   (cond
                    cancelled? t/dialog-hint
                    user?      t/user-role-fg
                    :else      t/ai-role-fg)
        time-str   (vis/format-date timestamp)
        ;; Below-message meta (assistant only): "blockether/glm-5.1 ·
        ;; 1 iter · ↑11461 · ↓35 · ~$0.006954 · 4.9s". Same surface
        ;; form `format-meta-line` produces for the CLI bracket and
        ;; the Telegram tagline. Provider + model auto-extract from
        ;; `:cost :provider` / `:cost :model` (where the iteration
        ;; runtime persists them). The chat-state's bare-name `:model`
        ;; field is intentionally NOT passed as the `:model` override
        ;; here — doing so would defeat the provider/model rendering.
        ;; Cancelled turns skip the whole block; there's no answer to
        ;; attribute and "0 iters / no model" reads as clutter under
        ;; a "Cancelled" placeholder.
        meta-str   (when (and (not user?) (not cancelled?))
                     (let [line (vis/format-meta-line
                                  {:iteration-count iteration-count
                                   :duration-ms duration-ms
                                   :tokens tokens
                                   :cost cost})]
                       (when-not (str/blank? line) line)))
        refs       (extract-link-refs message bubble-w)]

    ;; Row 0: label (bold, role-colored) + optional resources badge +
    ;; timestamp. Resources sit in the top-right chrome instead of
    ;; taking one row per link under the answer body.
    (when turn-separator?
      (p/clear-styles! g)
      (p/set-colors! g t/turn-separator-fg t/turn-separator-bg)
      (p/fill-rect! g bx start-row bubble-w 1)
      (p/put-str! g bx start-row (repeat-str \─ bubble-w)))

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
    ;; bubbles — the previous breathing row read as an unwanted top
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
      ;; renders directly on terminal bg — the only fills come from
      ;; structured-trace marker zones (code blocks, stdout, answer
      ;; section). Roles are visually distinguished by the colored
      ;; label and the blank row beneath it (no horizontal divider).
      ;; Bulk fill the content rows for the cases that want a
      ;; bubble-wide colored block: warnings (amber alarm) and user
      ;; messages (warm light-yellow zone). Assistant plain text and
      ;; cancelled status keep terminal bg — their per-line marker
      ;; switch handles any zone-specific fills (code blocks,
      ;; answer-bg, stdout, etc.) on its own.
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
      ;; User messages: fill the breathing row above content, the
      ;; content rows, AND one breathing row below the content.
      (when user?
        (p/set-bg! g bg-color)
        (p/fill-rect! g bx (- btop top-pad) bubble-w (+ (max 1 bubble-h) top-pad bottom-pad)))

      ;; Text content — per-line styling via invisible marker prefixes
      ;;
      ;; The \"answer zone\" starts at the first line carrying ANY
      ;; structural answer-* marker. Every line AFTER that index
      ;; renders on `answer-bg`. We used to anchor strictly on
      ;; `answer-hdr-marker` (the right-aligned \"FINAL ANSWER\"
      ;; superscript), but that marker became opt-in via
      ;; `:show-final-answer-header` — when off, the answer body lost
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
            ;; 11k-row trace bubble (conv 7b18414d) that pegged the
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
            ;; redraw — 11k tuples per frame on the big bubbles.
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
            (let [line (nth lines i)
                  meta (nth line-meta i nil)]
              (p/clear-styles! g)
              (let [in-answer? (> i (long answer-start))
                ;; Two coordinate systems per content row:
                ;;   text  at `x = bx + h-pad`, runs `content-w` cols  — keeps
                ;;         body padded inside the column.
                ;;   fills at `fbx = bx`,        run  `bubble-w`  cols — every
                ;;         marker zone (code, answer, stdout, iteration
                ;;         header, thinking, table…) paints the FULL message
                ;;         column so the colored band reaches both edges of
                ;;         the messages area instead of leaving a 2-col
                ;;         white strip on each side.
                ;; Right-aligned labels in `format-iteration-entry` write at
                ;; `x` and inherit `content-w`, so they still sit inset from
                ;; the right edge by h-pad even though the bg fills past them.
                    x   (+ bx h-pad) y (+ btop i)
                    iw  bubble-w
                    fbx bx]
            ;; Pre-fill answer zone bg so ALL line types get it
                (when in-answer?
                  (p/set-bg! g t/answer-bg)
                  (p/fill-rect! g fbx y iw 1))
                (cond
              ;; ── Iteration header — right-aligned, subtle ──
                  (str/starts-with? line iteration-hdr-marker)
                  (do (p/set-colors! g t/iteration-header-fg bg-color)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Thinking — dimmed bg, italic ──
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

              ;; ── Code (success) — light green bg, green ✓ suffix ──
                  (str/starts-with? line code-ok-marker)
                  (let [raw (subs line 1)]
                    (p/set-colors! g t/code-block-fg t/code-ok-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (paint-ansi-line! g x y raw t/code-block-fg t/code-ok-bg)
                    (when-let [ci (str/index-of raw "✓")]
                      (p/set-colors! g t/code-success-fg t/code-ok-bg)
                      (p/put-str! g (+ x ci) y (subs raw ci))))

              ;; ── Code (error) — light red bg, red ✗ suffix ──
                  (str/starts-with? line code-err-marker)
                  (let [raw (subs line 1)]
                    (p/set-colors! g t/code-block-fg t/code-err-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (paint-ansi-line! g x y raw t/code-block-fg t/code-err-bg)
                    (when-let [ci (str/index-of raw "✗")]
                      (p/set-colors! g t/code-error-fg t/code-err-bg)
                      (p/put-str! g (+ x ci) y (subs raw ci))))

              ;; ── Code (running, no status yet) — neutral bg ──
                  (str/starts-with? line code-marker)
                  (do (p/set-colors! g t/code-block-fg t/code-block-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (paint-ansi-line! g x y (subs line 1) t/code-block-fg t/code-block-bg))

              ;; ── Duration annotation ──
                  (str/starts-with? line duration-marker)
                  (do (p/set-colors! g t/code-duration-fg iteration-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Result (success) — dim on success bg ──
                  (str/starts-with? line result-marker)
                  (do (p/set-colors! g t/code-result-fg t/code-ok-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Result (error) — red on light red bg ──
                  (str/starts-with? line err-result-marker)
                  (do (p/set-colors! g t/code-error-result-fg t/code-err-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Stdout text — distinct stdout bg, italic ──
                  (str/starts-with? line stdout-marker)
                  (do (p/set-colors! g t/stdout-fg t/stdout-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/styled g [p/ITALIC]
                      (p/put-str! g x y (subs line 1))))

              ;; ── Stdout separator (dashes) ──
                  (str/starts-with? line stdout-sep-marker)
                  (do (p/set-colors! g t/stdout-sep-fg t/stdout-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Stdout padding ──
                  (str/starts-with? line stdout-pad-marker)
                  (do (p/set-bg! g t/stdout-bg)
                    (p/fill-rect! g fbx y iw 1))

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

              ;; ── Answer separator — bold horizontal rule between iterations and answer ──
                  (str/starts-with? line answer-sep-marker)
                  (do (p/set-colors! g t/answer-sep-fg bg-color)
                    (p/styled g [p/BOLD]
                      (p/put-str! g fbx y (repeat-str \u2500 iw))))

              ;; ── Answer header — right-aligned superscript on bubble bg ──
                  (str/starts-with? line answer-hdr-marker)
                  (do (p/set-colors! g t/iteration-header-fg bg-color)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Answer-mode markdown headings (gold gradient) ──
              ;;
              ;; H1→H3 use a saturated amber/gold gradient
              ;; (`t/md-h1-fg` etc.) instead of the body fg + bold,
              ;; so headings stop disappearing into surrounding
              ;; prose. The colour stack is engineered to pop on
              ;; both the white assistant bg and the new pale-blue
              ;; answer-bg — see the WCAG ratios in theme.clj.
              ;; Heading lines now run their content through
              ;; `markdown->inline` before reaching here, so the line
              ;; payload may contain INLINE_*_ON/OFF sentinels for
              ;; nested bold/italic/code spans. Painting via
              ;; `paint-styled-line!` (instead of raw `put-str!`)
              ;; consumes the sentinels in-place — they stay out of
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

                  ;; <summary> disclosure label — lavender band that
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
                        proof?   (:proofs? meta)
                        bg       (cond
                                   hovered? t/link-chrome-hover-bg
                                   proof?   t/proof-summary-bg
                                   :else    t/md-summary-bg)
                        fg       (cond
                                   hovered? t/link-chrome-hover-fg
                                   proof?   t/proof-summary-fg
                                   :else    t/md-summary-fg)]
                    (p/set-colors! g fg bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/styled g [p/BOLD]
                      (p/paint-styled-line! g x y (subs line 1)
                        fg bg
                        t/code-block-fg t/code-block-bg))
                    (when (= :toggle-details (:kind meta))
                      (cr/register!
                        {:bounds {:row abs-row :col fbx :width iw}
                         :kind :toggle-details
                         :conversation-id (:conversation-id meta)
                         :node-id (:node-id meta)
                         :collapsed? (:collapsed? meta)
                         :proofs? (:proofs? meta)})))

                  (str/starts-with? line md-code-marker)
                  (do (p/set-colors! g t/code-block-fg t/code-block-bg)
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
              ;; on top. Was the user-visible bug — `> **Lącznie:**`
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
                  ;; Pure box-drawing line — single muted paint.
                      (p/put-str! g x y stripped)
                  ;; Header / body data row — dual-color split.
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
                        proof?   (:proofs? meta)
                        bg       (cond
                                   hovered? t/link-chrome-hover-bg
                                   proof?   t/th-proof-summary-bg
                                   :else    t/th-md-summary-bg)
                        fg       (cond
                                   hovered? t/link-chrome-hover-fg
                                   proof?   t/th-proof-summary-fg
                                   :else    t/th-md-summary-fg)]
                    (p/set-colors! g fg bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/styled g [p/BOLD p/ITALIC]
                      (p/paint-styled-line! g x y (subs line 1)
                        fg bg
                        t/code-result-fg t/code-block-bg))
                    (when (= :toggle-details (:kind meta))
                      (cr/register!
                        {:bounds {:row abs-row :col fbx :width iw}
                         :kind :toggle-details
                         :conversation-id (:conversation-id meta)
                         :node-id (:node-id meta)
                         :collapsed? (:collapsed? meta)
                         :proofs? (:proofs? meta)})))

              ;; Thinking fenced code: visible code-block bg, italic dim text.
              ;; Clojure/EDN fences can carry zprint ANSI syntax color;
              ;; the painter translates ANSI foreground codes to Lanterna.
                  (str/starts-with? line th-md-code-marker)
                  (do (p/set-colors! g t/code-result-fg t/code-block-bg)
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

              ;; ── Legacy separator — dim ──
                  (str/starts-with? line sep-marker)
                  (do (p/set-colors! g t/dialog-hint bg-color)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Answer text — answer bg ──
                  (str/starts-with? line answer-txt-marker)
                  (do (p/set-colors! g t/answer-fg t/answer-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Answer padding ──
                  (str/starts-with? line answer-pad-marker)
                  (do (p/set-bg! g t/answer-bg)
                    (p/fill-rect! g fbx y iw 1))

              ;; ── Plain text — answer bg if in answer zone, else bubble bg ──
              ;; Cancelled status messages render in muted italic on
              ;; terminal bg (no fill) so the line reads as a system
              ;; note, not as something the model said.
                  :else
              ;; Cancelled bubbles never enter the answer zone — the
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
                    (if cancelled?
                  ;; Cancelled / system status messages: render as a
                  ;; muted italic block. Inline markdown spans are
                  ;; intentionally NOT honoured here — a system note
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
                (p/put-str! g bx (+ btop i) "│"))
              (recur (inc i))))))

      ;; Below-content footer row: optional right-aligned meta.
      ;; Resource links are exposed through the header badge, so they
      ;; no longer consume one extra chrome row per link under the
      ;; answer body.
      ;;
      ;; Final per-message layout (no outer box, no bg fill, no
      ;; horizontal rule under the label):
      ;;   row 0                    : label + resources badge + timestamp
      ;;   row 1 … N                : wrapped content (with marker-zone fills)
      ;;   row 1+N                  : bottom pad (user only)
      ;;   row 1+N+P                : meta right, when present
      ;;   final row                : single blank gap before the next message
      (p/clear-styles! g)
      (let [footer?    (some? meta-str)
            footer-row (+ btop bubble-h bottom-pad)]
        (when footer?
          (p/set-colors! g t/dialog-hint t/terminal-bg)
          (p/put-str! g (+ bx (max 0 (- bubble-w (count meta-str)))) footer-row meta-str))
        ;; Return: rows consumed
        ;;   = optional turn separator + one spacer row (0|2)
        ;;     + label(1) + top-pad(user only) + content(N)
        ;;     + bottom-pad(user only)
        ;;     + footer(meta)(0|1)
        ;;     + gap(1)
        (+ top-sep-h 1 top-pad bubble-h bottom-pad (if footer? 1 0) 1)))))

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
(def ^:private link-icon-proof    "✓")              ; provenance proof ref
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
    (= kind :provenance) link-icon-proof
    :else                link-icon-link))

;; Inline-style sentinel codepoints emitted by `markdown->inline`
;; (BOLD/ITALIC/STRIKE/CODE on/off pairs in the U+E110–U+E117 PUA
;; range). The chrome strip doesn't paint per-character SGR — it's
;; a single-tone hover band — so we strip the sentinels back out
;; before display, leaving plain text. The pre-pass through
;; `markdown->inline` is the trick: `**bold**` cleanly becomes
;; `bold`, `*italic*` becomes `italic`, ` `code` ` becomes `code`,
;; and unmatched openers (`*half`, `**incomplete`) stay literal
;; — same behaviour the prose body gets. Without this pass the
;; chrome shows raw markdown markup verbatim, e.g.
;;   [See **here**](url) -> "🔗 See **here** → url"
;; which looks like a typo to the user.
(def ^:private chrome-text-sentinel-re #"[\uE110-\uE117]")

(defn- strip-inline-markup
  "Run `text` through `markdown->inline` then drop the styling
   sentinels, yielding plain visible text. See the comment above
   `chrome-text-sentinel-re` for the rationale."
  ^String [^String text]
  (-> (or text "")
    markdown->inline
    (str/replace chrome-text-sentinel-re "")))

(defn- chrome-display-text
  "Build the visible chrome row for a ref:
     <icon> <text> → <url>
   The url tail is shortened to fit `max-w`. Pure.

   `text` is run through `strip-inline-markup` so inline emphasis
   inside the bracket portion (e.g. `[See **here**](url)`,
   ``[Title with `code`](url)``) renders as plain text in the
   chrome row instead of leaking raw `**` / backticks / `*`."
  ^String [ref ^long max-w]
  (let [icon  (ref-icon ref)
        text  (strip-inline-markup (or (:text ref) ""))
        url   (or (:url ref)  "")
        sep   " → "
        ;; Reserve room: icon (2 cols) + space + sep + at least 8 cols
        ;; of url.
        head     (str icon " " text)
        head-w   (p/display-width head)
        sep-w    (p/display-width sep)
        avail    (max 0 (- max-w head-w sep-w))
        url-disp (if (<= (count url) avail)
                   url
                   (let [keep (max 0 (- avail 1))]
                     (str (subs url 0 (min (count url) keep)) "…")))]
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
   message text’s identity-hash + width — the same trick
   `bubble-height` uses to dodge re-walking immutable message bodies
   on every paint."
  [{:keys [text hide-links?]} max-w]
  (if (or hide-links?
        (> (count (or text "")) max-link-ref-text-chars))
    []
    (cached* [::refs (System/identityHashCode text) (long max-w)]
      #(extract-link-refs* (or text "") max-w))))

(defn- in-viewport?
  "True when `abs-row` is inside the currently-painted messages
   viewport `[viewport-top, viewport-top + viewport-h)`."
  [^long viewport-top ^long viewport-h ^long abs-row]
  (and (>= abs-row viewport-top)
    (< abs-row (+ viewport-top viewport-h))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- paint-link-chrome-row!
  "Paint one chrome row at `(x, y)` (clip-relative coords) inside a
   `bubble-w`-wide column. When the row’s absolute screen position
   `abs-row` is inside the viewport AND the ref is enabled, register
   a click region.

   The row paints `<icon> <text> → <url>` in three colour zones so
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
    ;; tri-tone is a future polish — the single-tone read is
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

(defn bubble-height*
  "Uncached calculation: rows a chat message will consume without drawing.
   label(1) + optional top-pad(1, user only) + wrapped-lines
   + optional bottom-pad(1, user only) + optional meta footer(0|1)
   + gap(1).
   Mirrors `draw-chat-bubble!`'s wrap width (`bubble-w - 2*h-pad`) so
   layout math stays consistent across the height calc and the draw."
  [{:keys [text role prewrapped-lines iteration-count duration-ms tokens cost status turn-separator?]} max-w]
  (let [bubble-w   max-w
        top-sep-h  (if turn-separator? 2 0)
        h-pad      2
        content-w  (max 1 (- bubble-w (* 2 h-pad)))
        lines      (or prewrapped-lines
                     (and (= role :user) (markdown->lines text content-w :answer))
                     (wrap-text text content-w))
        top-pad    (if (= role :user) 1 0)
        bottom-pad (if (= role :user) 1 0)
        cancelled? (= :cancelled status)
        meta-str   (when (and (not= role :user) (not cancelled?))
                     (let [line (vis/format-meta-line
                                  {:iteration-count iteration-count
                                   :duration-ms duration-ms
                                   :tokens tokens
                                   :cost cost})]
                       (when-not (str/blank? line) line)))
        footer?    (some? meta-str)]
    (+ top-sep-h 1 top-pad (count lines) bottom-pad (if footer? 1 0) 1)))

(defn bubble-height
  "Memoized `bubble-height*`. Keyed by projected line identity when
   available; live progress keeps stable prewrapped body lines and only
   appends a cheap spinner row. Metadata that can add/remove the
   assistant footer is part of the key; otherwise a no-usage render can
   stale-cache the shorter height before usage arrives."
  [{:keys [text role prewrapped-lines turn-separator?
           iteration-count duration-ms tokens cost status] :as message} max-w]
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

(defn- error-signature
  "Stable comparison key for two trace `:error` maps. Returns nil for
   non-error iterations (so they never collapse)."
  [entry]
  (when-let [err (:error entry)]
    [(:type err) (:message err) (get-in err [:data :raw-data])]))

(def ^:private auto-collapse-line-threshold 12)
(def ^:private auto-collapse-char-threshold 700)
(def ^:private thinking-preview-edge-lines 10)

(defn- short-id-fragment
  ^String [id]
  (let [s (str (or id ""))]
    (subs s 0 (min 8 (count s)))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} detail-expanded?
  ([detail-expansions conversation-id node-id]
   (detail-expanded? detail-expansions conversation-id node-id true))
  ([detail-expansions conversation-id node-id default-expanded?]
   (boolean
     (get detail-expansions
       [(str conversation-id) (str node-id)]
       default-expanded?))))

(defn- hidden-size-hint
  ^String [entries]
  (let [line-count (count entries)
        char-count (reduce + 0 (map (comp count :line) entries))]
    (cond
      (> line-count 1) (str line-count " lines hidden")
      (pos? char-count) (str char-count " chars hidden")
      :else "empty")))

(defn- detail-id-suffix
  ^String [{:keys [conversation-turn-id iteration-number block-number details-path]}]
  (let [parts (cond-> []
                (some? conversation-turn-id) (conj (str "Turn: " (short-id-fragment conversation-turn-id)))
                iteration-number (conj (str "Iteration: " iteration-number))
                block-number (conj (str "Block: " block-number))
                (seq details-path) (conj (str "Details: " (str/join "." details-path))))]
    (if (seq parts)
      (str "[" (str/join ", " parts) "]")
      "[Details]")))

(defn- ellipsize-cols
  ^String [s max-w]
  (cond
    (<= max-w 0) ""
    (<= (p/display-width s) max-w) s
    (= max-w 1) "…"
    :else (str (p/truncate-cols s (dec max-w)) "…")))

(defn- format-detail-summary-line
  "Put the human-readable detail provenance on the right edge. The
   whole row is already painted as a bold disclosure band by
   `draw-chat-bubble!`, so keep the suffix as plain text. Inline code
   would drop that inherited bold style and switch to a weaker code
   background."
  ^String [left suffix max-w]
  (let [suffix-w (p/display-width suffix)
        gap-w    2]
    (if (> (+ suffix-w gap-w 1) max-w)
      (str left " · " suffix)
      (let [left-w (max 1 (- max-w suffix-w gap-w))
            left   (ellipsize-cols left left-w)
            pad-w  (max gap-w (- max-w (p/display-width left) suffix-w))]
        (str left (repeat-str \space pad-w) suffix)))))

(defn- detail-node-base-id
  ^String [{:keys [conversation-turn-id iteration-number block-number section kind]}]
  (str
    (or (some-> section name) "answer")
    (when conversation-turn-id (str ":t" (short-id-fragment conversation-turn-id)))
    (when iteration-number (str ":i" iteration-number))
    (when block-number (str ":b" block-number))
    (when kind (str ":" (name kind)))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} detail-node-id
  ^String [{:keys [details-path] :as detail-ctx}]
  (str
    (detail-node-base-id detail-ctx)
    (when (seq details-path) (str ":d" (str/join "." details-path)))))

(def ^:private proof-summary-icon "✓")

(defn- proof-summary-label
  [summary]
  (let [summary (or summary "Proofs")]
    (if (str/includes? summary proof-summary-icon)
      summary
      (str proof-summary-icon " " summary))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} detail-summary-entries
  [{:keys [marker max-w summary hidden-entries collapsed? conversation-id node-id proofs?]
    :as detail-ctx}]
  (let [suffix    (detail-id-suffix detail-ctx)
        hint      (when collapsed? (str " · " (hidden-size-hint hidden-entries)))
        summary   (if proofs? (proof-summary-label summary) (or summary "Details"))
        left      (str (if collapsed? "▸ " "▾ ")
                    summary
                    (or hint ""))
        visible   (format-detail-summary-line left suffix (max 1 max-w))
        wrapped   (wrap-text (markdown->inline visible) (max 1 max-w))
        meta      {:kind :toggle-details
                   :conversation-id (str conversation-id)
                   :node-id (str node-id)
                   :collapsed? collapsed?
                   :proofs? (boolean proofs?)}]
    (mapv (fn [line] {:line (str marker line) :meta meta}) wrapped)))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} auto-collapse-needed?
  [lines raw-text]
  (or (> (count lines) auto-collapse-line-threshold)
    (> (count (str (or raw-text ""))) auto-collapse-char-threshold)))

(defn- maybe-collapse-block
  [{:keys [conversation-id detail-expansions conversation-turn-id iteration-number
           block-number kind summary summary-marker body-marker lines max-w]}]
  (let [entries (mapv (fn [line] {:line (str body-marker line) :meta nil}) lines)]
    (if (or (nil? conversation-id)
          (not (auto-collapse-needed? lines (str/join "\n" lines))))
      entries
      (let [detail-ctx {:conversation-id conversation-id
                        :conversation-turn-id conversation-turn-id
                        :iteration-number iteration-number
                        :block-number block-number
                        :details-path nil
                        :section :iteration
                        :kind kind}
            node-id    (detail-node-id detail-ctx)
            collapsed? (not (detail-expanded? detail-expansions conversation-id node-id false))]
        (vec (concat
               (detail-summary-entries (assoc detail-ctx
                                         :marker summary-marker
                                         :max-w max-w
                                         :summary summary
                                         :hidden-entries entries
                                         :collapsed? collapsed?
                                         :node-id node-id))
               (when-not collapsed? entries)))))))

(defn- maybe-preview-thinking-entries
  "Collapse long reasoning to first/last preview rows with a clickable
   disclosure row in the middle. Expanded state uses the same
   detail-expansions map as result/stdout/details popouts."
  [{:keys [entries conversation-id detail-expansions conversation-turn-id iteration-number max-w]}]
  (let [entries (vec entries)
        n       (count entries)
        edge    thinking-preview-edge-lines]
    (if (or (nil? conversation-id)
          (<= n (* 2 edge)))
      entries
      (let [detail-ctx {:conversation-id conversation-id
                        :conversation-turn-id conversation-turn-id
                        :iteration-number iteration-number
                        :details-path nil
                        :section :thinking
                        :kind :reasoning}
            node-id    (detail-node-id detail-ctx)
            expanded?  (detail-expanded? detail-expansions conversation-id node-id false)
            head       (subvec entries 0 edge)
            tail       (subvec entries (- n edge) n)
            hidden     (subvec entries edge (- n edge))
            summary    (detail-summary-entries
                         (assoc detail-ctx
                           :marker th-md-summary-marker
                           :max-w max-w
                           :summary (if expanded?
                                      (str "REASONING · " (hidden-size-hint hidden))
                                      "REASONING")
                           :hidden-entries hidden
                           :collapsed? (not expanded?)
                           :node-id node-id))]
        (vec (concat
               head
               summary
               (when expanded? hidden)
               tail))))))

(defn- maybe-collapse-raw-text-block
  "Render a result/stdout-like block without wrapping huge collapsed
   bodies first. `wrap-text` is intentionally display-width-aware and
   expensive on long single-line data dumps; when the detail row is
   collapsed by default, the first frame only needs the summary hint."
  [{:keys [conversation-id detail-expansions conversation-turn-id iteration-number
           block-number kind summary summary-marker raw-text max-w]
    :as opts}]
  (let [raw-text (str/trim (str raw-text))]
    (when-not (str/blank? raw-text)
      (let [detail-ctx {:conversation-id conversation-id
                        :conversation-turn-id conversation-turn-id
                        :iteration-number iteration-number
                        :block-number block-number
                        :details-path nil
                        :section :iteration
                        :kind kind}
            node-id    (detail-node-id detail-ctx)
            collapsed? (not (detail-expanded? detail-expansions conversation-id node-id false))]
        (if (and conversation-id
              collapsed?
              (> (count raw-text) auto-collapse-char-threshold))
          (detail-summary-entries (assoc detail-ctx
                                    :marker summary-marker
                                    :max-w max-w
                                    :summary summary
                                    :hidden-entries [{:line raw-text :meta nil}]
                                    :collapsed? true
                                    :node-id node-id))
          (let [lines (wrap-text raw-text max-w)]
            (maybe-collapse-block (assoc opts :lines lines))))))))

(defn- entries->payload
  [entries]
  (let [lines     (mapv :line entries)
        line-meta (mapv :meta entries)]
    {:lines     lines
     :line-meta line-meta
     :text      (str/join "\n" lines)}))

(declare markdown->entries)

;;; ── Inline markdown tokenizer (mid-line bold / italic / strike / code) ──
;;
;; `markdown->inline` is forward-declared once at the top of the
;; file (search `(declare markdown->inline)`), so no second declare
;; here.

(defn- word-char-at?
  "True iff index `i` is inside `s` and points at a letter or digit.
   Out-of-bounds -> false. Used to enforce CommonMark/GFM's intra-word
   underscore rule: `VAR_NAME_HERE` must not light up as italic."
  [^String s ^long i]
  (and (>= i 0)
    (< i (.length s))
    (Character/isLetterOrDigit (.charAt s i))))

(defn- find-inline-close
  "Scan `s` from index `from` for the next occurrence of `closer`,
   stepping over nested constructs that stay atomic:

   - Code spans (`` `...` ``) are atomic; their content can include any
     character (including the closer we're hunting for) without our
     scanner being fooled. Skip from one backtick to its matching pair.
     The exception is when WE are the code-span scanner (closer = ``\\`
     ``) — then we just want the next backtick, no further skipping.
   - When `closer` is a SINGLE char (`*` or `_`), a doubled-pair like
     `**…**` or `__…__` belongs to a nested bold span, NOT to the open
     italic we're trying to close. Skip the whole nested pair. This is
     what makes `*italic with **bold** inside*` parse the way a human
     reads it (italic wrapping bold) instead of `italic with` /
     literal-`*bold` / italic-` inside`.
   - Underscore closers (`_` and `__`) are rejected when the character
     immediately after the candidate match is a letter or digit.
     CommonMark/GFM forbids intra-word underscore emphasis, so
     `FOO_BAR_BAZ` must NOT close at the second `_`. Companion rule on
     opener-side lives in `markdown->inline`.

   Empty spans (`**` immediately followed by `**`, etc.) are rejected
   by returning -1 — GitHub does the same.

   Returns the index of the matched closer or -1 if none."
  [^String s ^String closer ^long from]
  (let [n           (.length s)
        cl-len      (.length closer)
        single-ch   (when (= 1 cl-len) (.charAt closer 0))
        underscore? (= \_ (.charAt closer 0))]
    (loop [k from]
      (cond
        (> (+ k cl-len) n) -1

        ;; Step over code spans — unless we ARE the code-span scanner.
        (and (not= closer "`") (= (.charAt s k) \`))
        (let [end (.indexOf s "`" (inc k))]
          (if (neg? end) (recur (inc k)) (recur (inc end))))

        ;; When closing a single `*` (or `_`), skip past nested
        ;; doubled `**…**` (or `__…__`) so the outer italic doesn't
        ;; eat the inner bold's opener.
        (and single-ch
          (= (.charAt s k) ^char single-ch)
          (< (inc k) n)
          (= (.charAt s (inc k)) ^char single-ch))
        (let [doubled (str single-ch single-ch)
              end     (.indexOf s doubled (+ k 2))]
          (if (neg? end)
            ;; Unmatched double — step past the first char only and continue.
            (recur (inc k))
            (recur (+ end 2))))

        ;; Match!
        (.regionMatches s k closer 0 cl-len)
        (cond
          ;; Empty span (`__` immediately after the opener, etc.).
          (= k from) -1
          ;; Intra-word underscore: candidate `_` / `__` followed by
          ;; a word char cannot close. Keep scanning past it.
          (and underscore? (word-char-at? s (+ k cl-len)))
          (recur (+ k cl-len))
          :else k)

        :else
        (recur (inc k))))))

;; ── Markdown link / image pre-pass ────────────────────────────────────────
;;
;; Hand-rolled scanner that mirrors the regex
;;
;;     #"(!)?\[([^\]]*?)\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)"
;;
;; — `[text](url)` becomes `text`, `![alt](url)` becomes `""`. Used to
;; live as `(str/replace s #"…" (fn [m] …))` inside `markdown->inline`.
;; That allocated a Pattern call, a Matcher, and a per-match callback
;; closure on every line of every assistant bubble on every redraw,
;; which dominated cold-open cost on long conversations (multi-MB of
;; trace lines, dozens of `[path](path)` links per answer).
;;
;; The new version walks chars in a single loop and lazily allocates
;; a `StringBuilder` only when the first match is actually found —
;; the (very common) case where a line carries no link markup at all
;; returns the input string unchanged with zero allocations.

(defn- find-md-link-url-end
  "Given `s` of length `n` and `start` pointing at the first char of
   the URL inside `[text](url)`, return the index of the closing `)`
   that ends the link, or -1 if no valid URL ends at this position.

   Mirrors the URL portion of the regex
   `([^)\\s]+)(?:\\s+\"[^\"]*\")?\\)`: at least one non-space
   non-paren URL char, optional ` \"title\"`, then `)`."
  ^long [^String s ^long n ^long start]
  (loop [k start]
    (if (>= k n)
      -1
      (let [c (.charAt s k)]
        (cond
          (= c \))
          (if (> k start) k -1)            ;; URL must have ≥1 char

          (or (= c \space) (= c \tab))
          (if (= k start)
            -1                              ;; whitespace before any URL char
            (let [ws-end (loop [m k]
                           (if (and (< m n)
                                 (let [cc (.charAt s m)]
                                   (or (= cc \space) (= cc \tab))))
                             (recur (inc m))
                             m))]
              (if (and (< ws-end n) (= \" (.charAt s ws-end)))
                (let [title-close (.indexOf s "\"" (inc ws-end))]
                  (if (and (>= title-close 0)
                        (< (inc title-close) n)
                        (= \) (.charAt s (inc title-close))))
                    (inc title-close)
                    -1))
                -1)))

          :else (recur (inc k)))))))

(defn- strip-md-links
  "Remove markdown link / image markup from `s`, leaving the anchor
   text for links and dropping images entirely. Hand-rolled scanner;
   see the comment block above `find-md-link-url-end` for why.

   Returns `s` UNCHANGED (same identity) when the input carries no
   link/image markup, so downstream identity-keyed caches stay hot."
  ^String [^String s]
  (let [n (.length s)]
    (loop [i 0
           last-write 0
           ^StringBuilder sb nil]
      (if (>= i n)
        (if (nil? sb)
          s
          (do (.append sb s last-write n)
            (.toString sb)))
        (let [c (.charAt s i)
              image? (and (= c \!)
                       (< (inc i) n)
                       (= \[ (.charAt s (inc i))))]
          (if (or image? (= c \[))
            (let [bracket-i  (if image? (inc i) i)
                  text-start (inc bracket-i)
                  bracket-close (.indexOf s "]" text-start)]
              (if (and (>= bracket-close 0)
                    (< (inc bracket-close) n)
                    (= \( (.charAt s (inc bracket-close))))
                (let [url-from (+ bracket-close 2)
                      url-end  (find-md-link-url-end s n url-from)]
                  (if (neg? url-end)
                    (recur (inc i) last-write sb)
                    (let [match-end (inc url-end)
                          ^StringBuilder b (or sb (StringBuilder. n))]
                      (.append b s last-write i)
                      (when-not image?
                        (.append b s text-start bracket-close))
                      (recur match-end match-end b))))
                (recur (inc i) last-write sb)))
            (recur (inc i) last-write sb)))))))

(defn- markdown->inline
  "Tokenize CommonMark inline syntax (`**bold**`, `__bold__`,
   `*italic*`, `_italic_`, `~~strike~~`, `` `code` ``) into
   sentinel-decorated text the bubble painter can render with SGR.

   Strategy: left-to-right scan with a fixed priority table
   (longest opener first: `**`/`__` before `*`/`_`, `~~` before `~`,
   then backtick). When a span is matched, its CONTENT is recursively
   re-tokenized so nested spans render correctly:

     **bold *italic* bold-again**
     ~~strike `with code` strike-again~~
     *italic with **bold** inside*

   The `find-inline-close` helper above understands the nesting
   rules: it steps over doubled-char pairs when scanning for a
   single-char close, and steps over code spans (which are atomic).

   Code spans are the ONE exception: their content is NOT re-tokenized.
   `` `**not-bold**` `` keeps the asterisks literal, mirroring GitHub.

   Empty spans (`****`, `__`, `~~~~`) and orphan openers fall through
   as literal text — also matches GitHub behaviour for the same input.

   Link and image markup (`[text](url)`, `![alt](url)`) is stripped
   in a pre-pass BEFORE tokenisation: only the anchor text survives
   into the sentinel stream. The clickable affordance lives in the
   link-chrome strip painted below the prose, so duplicating the URL
   inline would show the same information twice.

   What this is NOT: a full CommonMark parser. We DO honour CommonMark's
   intra-word underscore rule (`VAR_NAME_HERE` is left alone — `_` /
   `__` cannot open when preceded by a word char, cannot close when
   followed by one), because identifier-mangling is the single most
   visible regression a markdown presenter can ship. Asterisks keep
   the permissive behaviour: `a*b*c` is still italic-b, mirroring real
   prose. No autolinks. No HTML. No images. We cover the 95% of inline
   markup real assistant answers emit; the other 5% is either rare
   enough to ignore or already handled by line-level markers
   (`# heading`, `- bullet`, `> quote`, fenced code)."
  ^String [^String s]
  (if (or (nil? s) (zero? (.length ^String s)))
    (or s "")
    ;; NOTE: do NOT wrap this body in `cached*`. Every line of every
    ;; iteration trace would land its own entry; the 512-cap LRU
    ;; saturates after a single 22-iter bubble (~228 entries) and
    ;; evicts the much-more-valuable `format-answer-with-thinking`
    ;; entries that are keyed at the WHOLE-bubble level. The
    ;; algorithmic win comes from `strip-md-links` (StringBuilder),
    ;; the steady-state win comes from the outer fawt cache; an
    ;; inline-level cache between them just thrashes both.
    (let [s  (strip-md-links s)
          n  (.length ^String s)
          sb (StringBuilder.)
          ;; Each token entry: [opener, closer, ON-sentinel, OFF-sentinel,
          ;;                    recurse-into-content?].
          ;; Order matters — longer openers tried first (`**` before `*`)
          ;; so `**foo**` parses as bold rather than italic-`foo`-italic.
          tokens [["**" "**" p/INLINE_BOLD_ON   p/INLINE_BOLD_OFF   true]
                  ["__" "__" p/INLINE_BOLD_ON   p/INLINE_BOLD_OFF   true]
                  ["~~" "~~" p/INLINE_STRIKE_ON p/INLINE_STRIKE_OFF true]
                  ["*"  "*"  p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF true]
                  ["_"  "_"  p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF true]
                  ["`"  "`"  p/INLINE_CODE_ON   p/INLINE_CODE_OFF   false]]
          ;; CommonMark/GFM intra-word underscore rule: `_` and `__`
          ;; cannot OPEN emphasis when the immediately preceding
          ;; character is a letter or digit. This is what keeps
          ;; `VARIABLES_LIKE_THIS`, `snake_case_var`, `text_with_under`
          ;; etc. rendering as plain literal text instead of being
          ;; mangled into italics with the underscores eaten.
          ;; Asterisks (`*` / `**`) keep CommonMark's permissive
          ;; behaviour — `a*b*c` is still italic-b — because real
          ;; prose relies on it.
          opener-allowed?
          (fn [^String op ^long i]
            (or (not= \_ (.charAt op 0))
              (not (word-char-at? s (dec i)))))
          match-opener (fn [^long i]
                         (some (fn [[op _ _ _ _ :as t]]
                                 (let [op-len (.length ^String op)]
                                   (when (and (<= (+ i op-len) n)
                                           (.regionMatches s i ^String op 0 op-len)
                                           (opener-allowed? op i))
                                     t)))
                           tokens))]
      (loop [i 0]
        (if (>= i n)
          (.toString sb)
          (if-let [[opener closer on off recurse?] (match-opener i)]
            (let [content-from (+ i (.length ^String opener))
                  close-at     (find-inline-close s closer content-from)]
              (if (neg? close-at)
                ;; No matching close — emit opener char as literal,
                ;; advance one char and let later positions reattempt.
                (do (.append sb (.charAt s i))
                  (recur (inc i)))
                (let [content (subs s content-from close-at)]
                  (.append sb ^String on)
                  (.append sb ^String (if recurse?
                                        (markdown->inline content)
                                        content))
                  (.append sb ^String off)
                  (recur (+ close-at (.length ^String closer))))))
            (do (.append sb (.charAt s i))
              (recur (inc i)))))))))

(defn- collapse-repeated-error-runs
  "Walk an iterations vec; collapse runs of consecutive iterations that
   share the same error signature into one rendered block carrying a
   `:repeat-count`. Non-error iterations pass through unchanged with
   `:repeat-count 1`.

   Returns a vec of `[orig-idx entry]` pairs (mirroring the input order
   of `map-indexed`) so callers can keep using `(inc orig-idx)` as the
   visible iteration number while skipping the duplicates."
  [iterations]
  (loop [acc [] i 0 remaining (vec iterations)]
    (if (empty? remaining)
      acc
      (let [head (first remaining)
            sig  (error-signature head)
            run  (if (nil? sig)
                   1
                   (count (take-while #(= sig (error-signature %)) remaining)))]
        (recur (conj acc [i (assoc head :repeat-count run)])
          (+ i run)
          (subvec remaining run))))))

(defn- format-iteration-entry-entries
  [{:keys [thinking events code comments results stdouts durations successes started-at-ms error repeat-count]}
   code-width iteration-number
   & [{:keys [show-header? conversation-id detail-expansions conversation-turn-id now-ms]
       :or   {show-header? true}}]]
  (let [fill-w      (max 1 (dec code-width))
        line-entry  (fn [line] {:line line :meta nil})
        label       (label-text "iteration" iteration-number)
        pad-len     (max 0 (- fill-w (count label) 1))
        header-line (str (repeat-str \space pad-len) label " ")
        header      (if show-header?
                      [(line-entry (str iteration-hdr-marker header-line))]
                      [])
        thinking-lines
        (fn [thinking-text-or-texts]
          (let [texts (if (sequential? thinking-text-or-texts)
                        thinking-text-or-texts
                        [thinking-text-or-texts])
                entries (into []
                          (mapcat
                            (fn [thinking-text]
                              (when (and (string? thinking-text) (not (str/blank? thinking-text)))
                                (or (seq (markdown->entries thinking-text fill-w :thinking
                                           {:conversation-id      conversation-id
                                            :conversation-turn-id conversation-turn-id
                                            :detail-expansions   detail-expansions
                                            :iteration-number    iteration-number
                                            :section             :thinking}))
                                  (mapv #(line-entry (str thinking-marker %))
                                    (wrap-text thinking-text fill-w))))))
                          texts)]
            (when (seq entries)
              (let [preview-entries (maybe-preview-thinking-entries
                                      {:entries              entries
                                       :conversation-id      conversation-id
                                       :detail-expansions   detail-expansions
                                       :conversation-turn-id conversation-turn-id
                                       :iteration-number    iteration-number
                                       :max-w               fill-w})]
                (vec (concat [(line-entry (str thinking-marker ""))]
                       preview-entries
                       [(line-entry (str thinking-marker ""))]))))))
        error-lines
        (fn []
          (when (map? error)
            (let [repeat-count      (max 1 (long (or repeat-count 1)))
                  badge             (when (> repeat-count 1) (str "  × " repeat-count))
                  hdr-label         (str (label-text "error") (or badge ""))
                  hdr-pad           (max 0 (- fill-w (count hdr-label) 1))
                  hdr-line          (str iteration-hdr-marker (repeat-str \space hdr-pad) hdr-label " ")
                  err-message       (or (:message error) (str (:type error)) "unknown error")
                  raw               (some-> (get-in error [:data :raw-data]) str str/trim)
                  recv              (get-in error [:data :received-type])
                  err-message-rows  (mapv #(line-entry (str err-result-marker %))
                                      (wrap-text (vis/format-error err-message) fill-w))
                  raw-rows          (when (and raw (not (str/blank? raw)))
                                      (let [hdr        (str "provider returned"
                                                         (when recv (str " (" recv ")"))
                                                         ":")
                                            raw-trim   (if (> (count raw) 600) (str (subs raw 0 600) "…") raw)
                                            body-lines (mapv #(line-entry (str err-result-marker %))
                                                         (wrap-text raw-trim fill-w))]
                                        (into [(line-entry (str err-result-marker hdr))] body-lines)))]
              (vec (concat
                     [(line-entry (str iteration-pad-marker ""))]
                     (when show-header? [(line-entry (str iteration-pad-marker ""))])
                     (when show-header? [(line-entry hdr-line)])
                     [(line-entry (str code-err-pad-marker ""))]
                     err-message-rows
                     (when (seq raw-rows) [(line-entry (str code-err-pad-marker ""))])
                     (or raw-rows [])
                     [(line-entry (str code-err-pad-marker ""))])))))
        form-lines
        (fn [idx block-number]
          (let [form          (when code (get code idx))
                success?      (when successes (get successes idx))
                has-status?   (some? success?)
                is-error?     (and has-status? (not success?))
                duration-ms   (when durations (get durations idx))
                duration-str  (vis/format-duration duration-ms)
                expr-label    (label-text "block" block-number)
                expr-hdr      (let [pl (max 0 (- fill-w (count expr-label) 1))]
                                (str (repeat-str \space pl) expr-label " "))
                running-start (when started-at-ms (get started-at-ms idx))
                running-ms    (when (and running-start now-ms)
                                (max 0 (- (long now-ms) (long running-start))))
                running-ms    (when running-ms (* 1000 (quot running-ms 1000)))
                running-str   (or (some-> running-ms vis/format-duration) "0ms")
                status-text   (if has-status?
                                (str (if success? "✓" "✗")
                                  (when duration-str (str " " duration-str)))
                                (str "↻ " running-str))
                status-line   (let [s-marker (cond
                                               (not has-status?) code-marker
                                               success?          code-ok-marker
                                               :else             code-err-marker)
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
                comment-text  (some-> comments (get idx))
                comment-lines (when (and (string? comment-text)
                                      (not (str/blank? comment-text)))
                                (let [trimmed (str/trim comment-text)
                                      wrapped (mapcat (fn [line] (wrap-text line fill-w))
                                                (str/split-lines trimmed))]
                                  (mapv #(line-entry (str thinking-marker %)) wrapped)))
                code-text     (str/trim (or form ""))
                formatted     (format-clojure-ansi code-text fill-w)
                code-lines    (str/split-lines formatted)
                c-lines       (mapv #(line-entry (str c-marker %)) code-lines)
                result-str    (when results (get results idx))
                r-marker      (if is-error? err-result-marker result-marker)
                result-lines  (when (and result-str (not (str/blank? (str result-str))))
                                (maybe-collapse-raw-text-block
                                  {:conversation-id      conversation-id
                                   :detail-expansions   detail-expansions
                                   :conversation-turn-id conversation-turn-id
                                   :iteration-number    iteration-number
                                   :block-number        block-number
                                   :kind                :result
                                   :summary             "RESULT"
                                   :summary-marker      md-summary-marker
                                   :body-marker         r-marker
                                   :raw-text            result-str
                                   :max-w               fill-w}))
                code-block    (vec (concat
                                     (when show-header? [(line-entry (str iteration-hdr-marker expr-hdr))])
                                     (when (seq comment-lines)
                                       (concat [(line-entry (str thinking-marker ""))]
                                         comment-lines
                                         [(line-entry (str thinking-marker ""))]))
                                     [(line-entry (str c-pad ""))]
                                     c-lines
                                     (when (seq result-lines) [(line-entry (str c-pad ""))])
                                     result-lines
                                     (when status-line [status-line])
                                     [(line-entry (str c-pad ""))]))
                stdout-str    (when stdouts (get stdouts idx))
                stdout-block  (when (and stdout-str (not (str/blank? (str stdout-str))))
                                (let [text-lines      (wrap-text (str/trim (str stdout-str)) fill-w)
                                      plain-entries   (mapv #(line-entry (str stdout-marker %)) text-lines)
                                      collapsed-entries
                                      (maybe-collapse-block
                                        {:conversation-id      conversation-id
                                         :detail-expansions   detail-expansions
                                         :conversation-turn-id conversation-turn-id
                                         :iteration-number    iteration-number
                                         :block-number        block-number
                                         :kind                :stdout
                                         :summary             "STDOUT"
                                         :summary-marker      md-summary-marker
                                         :body-marker         stdout-marker
                                         :lines               text-lines
                                         :max-w               fill-w})]
                                  (if (not= collapsed-entries plain-entries)
                                    collapsed-entries
                                    (let [slabel     (label-text "stdout")
                                          slabel-pad (max 0 (- fill-w (count slabel) 1))
                                          slabel-ln  (line-entry (str iteration-hdr-marker
                                                                   (repeat-str \space slabel-pad)
                                                                   slabel " "))]
                                      (vec (concat
                                             (when show-header? [slabel-ln])
                                             [(line-entry (str stdout-pad-marker ""))]
                                             plain-entries
                                             [(line-entry (str stdout-pad-marker ""))]))))))
                margin        (when (seq stdout-block) [(line-entry (str iteration-pad-marker ""))])]
            (vec (concat code-block margin stdout-block))))
        grouped
        (when (seq code)
          (let [code+result-lines
                (into []
                  (mapcat (fn [[idx _form]]
                            (concat
                              (when (pos? idx) [(line-entry (str iteration-pad-marker ""))])
                              (form-lines idx (inc idx))))
                    (map-indexed vector code)))]
            (when (seq code+result-lines)
              (vec (concat
                     [(line-entry (str iteration-pad-marker ""))]
                     code+result-lines
                     [(line-entry (str iteration-pad-marker ""))])))))
        ordered
        (when (seq events)
          (let [event-lines (loop [remaining    events
                                   block-number 1
                                   out          []]
                              (if-let [event (first remaining)]
                                (case (:type event)
                                  :thinking
                                  (let [[thinking-run more] (split-with #(= :thinking (:type %)) remaining)]
                                    (recur more
                                      block-number
                                      (into out
                                        (concat
                                          (when (seq out) [(line-entry (str iteration-pad-marker ""))])
                                          (thinking-lines (mapv :thinking thinking-run))))))

                                  :form-result
                                  (recur (rest remaining)
                                    (inc block-number)
                                    (into out
                                      (concat
                                        (when (seq out) [(line-entry (str iteration-pad-marker ""))])
                                        (form-lines (:form-idx event) block-number))))

                                  (recur (rest remaining) block-number out))
                                out))]
            (when (seq event-lines)
              (vec (concat
                     [(line-entry (str iteration-pad-marker ""))]
                     event-lines
                     [(line-entry (str iteration-pad-marker ""))])))))
        body (or ordered grouped)
        trailing-errors (error-lines)]
    (if (seq ordered)
      (into (vec header) (concat body trailing-errors))
      (into (vec (concat header (thinking-lines thinking) trailing-errors)) body))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} format-iteration-entry
  [entry code-width iteration-number & [opts]]
  (mapv :line (apply format-iteration-entry-entries entry code-width iteration-number [opts])))

;;; ── Spinner glyph (used by the in-bubble “working…” row) ──────────────────

(def ^:private spinner-frames
  "Braille-dot spinner frames; one frame advances every ~100ms."
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defn spinner-frame
  "Pick the spinner glyph for `now-ms`. Pure — same input always
   returns the same frame, which makes the render loop testable."
  ^String [^long now-ms]
  (nth spinner-frames (mod (quot now-ms 100) (count spinner-frames))))

(defn- prettify-error-type
  "`:svar.core/http-error` → \"http error\". Drops the namespace and
   replaces dashes with spaces so the spinner line reads naturally,
   e.g. \"iter 0 — http error — retrying\". Returns nil when there's
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

   Anthropomorphic `Vis is …` phrasing matches what other agent CLIs
   (pi, Claude Code, Codex) converged on; reads as a status sentence
   instead of a system log line. No elapsed-time-driven escalation —
   wall-clock is already shown right next to this string in the
   spinner row, the user can read the seconds themselves."
  [iterations cancelling?]
  (let [n              (count iterations)
        last-iteration (last iterations)
        err            (:error last-iteration)
        errored?       (some? err)
        thinking?      (and (not errored?)
                         (some? (:thinking last-iteration))
                         (not (str/blank? (:thinking last-iteration))))
        executing?     (and (not errored?) last-iteration (seq (:code last-iteration)))]
    (cond
      cancelling? "Vis is cancelling"
      errored?    (let [label (prettify-error-type err)]
                    (str "Vis is retrying"
                      (when label (str " after " label))
                      " (iter " n ")"))
      (zero? n)   "Vis is calling the provider"
      thinking?   (str "Vis is thinking (iter " n ")")
      executing?  (str "Vis is running code (iter " n ")")
      :else       (str "Vis is working (iter " n ")"))))

(defn progress->lines-data
  "Build prewrapped lines for the live progress placeholder bubble.

   The bubble lives in the assistant slot (right where the final
   answer will land), so the user sees the agent thinking/working in
   place instead of a status line wedged into the input box.

   Layout while loading (spinner ALWAYS at the bottom):
     <iteration trace, if iterations exist and not hidden>
     <blank>
     [spinner] phase… elapsed · Esc to cancel

   The activity row sits last so it tracks the natural reading
   direction — trace history flows top-down like a transcript, and
   \"what's happening RIGHT NOW\" lives where the cursor is about to
   write next. Putting the spinner above the trace forced the user
   to look at a moving line, then drop their eye further down to
   read static history that doesn't change. Inverted now.

   `progress` is the `:progress` slot from app-db: `{:iterations [...]}`.
   `bubble-w` is the outer bubble width in chars (we subtract the
   bubble's symmetric inner padding to match draw-chat-bubble!).
   `settings` is the display settings map: `{:show-thinking bool :show-iterations bool}`.
   `extra` carries:
     :now-ms         — `System/currentTimeMillis` from the render thread
                       (drives the spinner frame); defaults to current ms
     :turn-start-ms — wall-clock start, used for elapsed time
     :cancelling?    — true once Esc was pressed
     :conversation-id — current conversation id; enables live detail rows
     :conversation-turn-id — optional turn id, when known
     :detail-expansions — detail expansion state keyed by conversation/node"
  ([progress bubble-w settings] (progress->lines-data progress bubble-w settings nil))
  ([progress bubble-w settings extra]
   (let [iterations       (vec (:iterations progress))
         content-w        (max 10 (- bubble-w 4))
         show-thinking?   (get settings :show-thinking true)
         show-iterations? (get settings :show-iterations true)
         show-iteration-headers?  (get settings :show-iteration-headers false)
         {:keys [now-ms turn-start-ms cancelling? conversation-id
                 conversation-turn-id detail-expansions]} extra
         now-ms           (long (or now-ms (System/currentTimeMillis)))
         elapsed-ms       (when turn-start-ms
                            (max 0 (- now-ms (long turn-start-ms))))
         elapsed-str      (or (vis/format-duration elapsed-ms) "0ms")
         spinner-line     (str (spinner-frame now-ms) "  "
                            (progress-phase iterations cancelling?) "…  "
                            elapsed-str "  ·  Esc to cancel")
         line-entry       (fn [line] {:line line :meta nil})
         trace-entries    (when (and show-iterations? (seq iterations))
                            (into []
                              (mapcat (fn [[idx entry]]
                                        (format-iteration-entry-entries
                                          (if show-thinking? entry (dissoc entry :thinking :events))
                                          content-w (inc idx)
                                          {:show-header?         show-iteration-headers?
                                           :now-ms               now-ms
                                           :conversation-id      conversation-id
                                           :conversation-turn-id conversation-turn-id
                                           :detail-expansions   detail-expansions})))
                              (collapse-repeated-error-runs iterations)))
         entries          (if (seq trace-entries)
                            (conj (conj trace-entries (line-entry ""))
                              (line-entry spinner-line))
                            [(line-entry spinner-line)])]
     (entries->payload entries))))

(defn progress->text
  "Build the text body of the live progress placeholder bubble."
  ([progress bubble-w settings] (progress->text progress bubble-w settings nil))
  ([progress bubble-w settings extra]
   (:text (progress->lines-data progress bubble-w settings extra))))

;;; ── Markdown table parsing ───────────────────────────────────────────────

(defn- table-row?
  "Truthy when `s` looks like a markdown pipe table row: starts and
   ends with `|` after trim and contains at least one inner pipe."
  [s]
  (when (string? s)
    (let [t (str/trim s)]
      (and (>= (count t) 3)
        (str/starts-with? t "|")
        (str/ends-with?   t "|")
        (> (count (str/split t #"\|")) 2)))))

(defn- table-separator-row?
  "Truthy when `s` is the `|---|:---:|---:|` style separator that
   marks the divider between header and body of a markdown table."
  [s]
  (and (table-row? s)
    (let [t (str/trim s)]
      (boolean (re-matches #"^\|\s*:?-{2,}:?\s*(\|\s*:?-{2,}:?\s*)*\|$" t)))))

(defn- parse-table-row
  "Split `| a | b | c |` into [\"a\" \"b\" \"c\"]. Strips outer pipes,
   trims each cell, and drops any inline markdown emphasis since we
   can't render mid-cell styled spans in the grid."
  [s]
  (let [t (str/trim s)
        ;; chop leading/trailing |
        inner (subs t 1 (max 1 (dec (count t))))
        cells (str/split inner #"\|" -1)]
    (mapv (fn [c]
            (-> (or c "")
              str/trim
              (str/replace #"\*\*([^*]+)\*\*" "$1")
              (str/replace #"`([^`]+)`" "$1")))
      cells)))

(defn- numeric-cell?
  "True when `s` parses as a number after stripping spaces, NBSP,
   thousands separators, and surrounding whitespace. Handles Polish
   formatting like `4 879`, `12 104`, `1,5`, `-3.14`. Empty cells
   are NOT numeric — they shouldn't pin a column to right-align on
   their own."
  [s]
  (let [t (some-> s str str/trim)]
    (and (string? t)
      (pos? (count t))
      (try
        (let [normalized (-> t
                           (str/replace "\u00a0" "")  ;; NBSP
                           (str/replace " " "")
                           (str/replace "," "."))]
          (Double/parseDouble normalized)
          true)
        (catch NumberFormatException _ false)))))

(defn- column-align
  "Return `:right` when every non-empty body cell in `col-cells` is
   numeric; `:left` otherwise. Headers are excluded from the
   detection — a header named \"2024\" shouldn't right-align a
   column of free-form text below it. Empty body cells are ignored."
  [col-cells]
  (let [filled (filter (fn [s] (and s (pos? (count (str/trim (str s)))))) col-cells)]
    (if (and (seq filled) (every? numeric-cell? filled))
      :right
      :left)))

(defn- pad-cell
  "Render one cell padded to `w` terminal COLUMNS with the given
   alignment. Returns ` <padded content> ` — the leading and trailing
   spaces are mandatory: they sit between the `│` column dividers,
   giving every cell a one-space inner margin so text never touches
   the vertical lines.

   `align` is `:left` (default, trailing pad) or `:right` (leading
   pad). Numeric columns are auto-detected as `:right` so columns of
   `389 / 12 104 / 4 879` line up on the units position — the only
   way numeric tables read naturally.

   Width math: every length here is **display columns**, not Java
   chars. Without this, a cell containing ‘🏿️’ (3 Java chars / 2 cols)
   ends up one column narrower than a sibling cell containing ‘📄’
   (2 chars / 2 cols), which silently shifts every `┃` separator on
   that row and breaks the grid alignment."
  [text w align]
  (let [t (str text)
        t-cols (p/display-width t)
        t (cond
            (<= t-cols w) t
            ;; Truncate to (w-1) columns and append …; ellipsis is 1 col,
            ;; so total width is exactly `w`. truncate-cols is
            ;; grapheme-safe and won't split an emoji in half.
            (>= w 2)      (str (p/truncate-cols t (dec w)) "…")
            ;; w == 1: no room for ellipsis, just slice.
            :else         (p/truncate-cols t w))
        ;; Re-measure after possible truncation so the pad amount is
        ;; expressed in COLUMNS, not Java chars. truncate-cols
        ;; guarantees display-width ≤ (dec w); the +1 for the ellipsis
        ;; or unchanged path keeps the math honest.
        ;;
        ;; (VS-16 padding compensation lived here briefly when the
        ;; lanterna fork still reported `isDoubleWidth=true` for
        ;; 🏷️ / ❤️. Lanterna 3.1.5-vis.3 fixes that at the source
        ;; — see the dep comment in vis-channel-tui/deps.edn — so VS-16
        ;; graphemes now report `display-width=1` consistently and
        ;; pad-cell allocates the right space without a per-cell hack.)
        pad-cols (max 0 (- w (p/display-width t)))
        pad (repeat-str \space pad-cols)
        body (case align
               :right (str pad t)
               (str t pad))]
    (str " " body " ")))

(def ^:private TABLE_CELL_MIN_W_CAP
  "Soft floor for column width when shrinking a table to fit the
   viewport. Picked so a column carrying one nasty 30-char token
   (URL, hex digest, identifier) doesn't blow the whole table out:
   that token will word-break across lines instead of forcing every
   other column down to 1 column. 12 cols reads as a comfortable
   lower bound for typical prose; only kicks in when the natural
   widths don't fit."
  12)

(defn- longest-token-width
  "Display width of the widest whitespace-delimited token across
   `cells`. Used as the per-column floor when shrinking — keeps the
   word-wrapper from constantly hard-breaking ordinary words. Floors
   at 1 so the result is always a sensible column width."
  ^long [cells]
  (long
    (reduce
      (fn [m s]
        (let [s (str s)]
          (if (str/blank? s)
            m
            (apply max m
              (map p/display-width (str/split s #"\s+"))))))
      1
      cells)))

(defn- wrap-cell-lines
  "Word-wrap one cell's text to fit `w` display columns, reusing the
   shared `wrap-text*` so behaviour matches the rest of the bubble:
   break at spaces when possible, hard-break on column otherwise.
   Returns at least one line; an empty/blank cell yields `[\"\"]` so
   row-height equalisation always sees something to pad against."
  [text ^long w]
  (let [s (str text)]
    (cond
      (<= w 0)                   [""]
      (str/blank? s)             [""]
      (<= (p/display-width s) w) [s]
      :else                      (wrap-text* s w))))

(defn- render-table
  "Render a parsed markdown table (headers vec + rows vec-of-vecs) as
   a list of marker-prefixed display lines.

   Style: full grid — square outer corners (`┌┐└┘`), `┬┼┴` T-junctions
   where columns meet horizontals, `│` between columns, `─` for
   horizontals.

   The drawing of these lines is split across two passes by
   `paint-table-line!` (in the per-line marker switch above): the
   box-drawing glyphs paint in the muted `code-border-fg` so the
   chrome doesn't compete with the content, and the cell text on
   top paints in `code-block-fg` (bold for header rows). Putting
   everything in the same dark color made the grid read as solid
   ink — the user complaint that triggered this design.

   `markers` is `{:thead :tsep :trow}` from `md-marker-sets`. Tables
   wider than `max-w` first try to shrink each column toward its
   `longest-token-width` floor (capped at `TABLE_CELL_MIN_W_CAP`),
   then word-wrap each cell vertically across as many sub-rows as
   needed to keep all content inside the grid. Only when the floors
   themselves can't fit (extreme case: many columns, narrow bubble)
   does the renderer fall back to uniform scale-down + hard breaks.
   Net effect: a `vis ls` table no longer truncates filenames; long
   prose cells flow onto the next visual line within the same row."

  [headers rows max-w {:keys [thead tsep trow]}]
  (let [n-cols    (max 1 (apply max (count headers) (map count rows)))
        pad-cells (fn [r] (vec (concat r (repeat (max 0 (- n-cols (count r))) ""))))
        h         (pad-cells headers)
        rs        (mapv pad-cells rows)
        ;; Per-column width = max DISPLAY-WIDTH (terminal columns) of
        ;; every cell in that column, including the header.
        ;;
        ;; VS-16 compensation lives in pad-cell, not here. Each cell
        ;; carrying a VS-16 grapheme adds its own +1 padding so its
        ;; visual width matches sibling cells — see pad-cell. Tried
        ;; expanding col-width for the whole column instead; rejected
        ;; because it over-pads non-VS-16 cells (📄, 📁, ✅ etc. render
        ;; correctly without the bonus, so adding it everywhere shifts
        ;; them right by 1 — the user-visible 'still misaligned, need
        ;; even more' regression). Per-cell bonus is the right grain.
        col-widths-raw
        (mapv (fn [i]
                (apply max 1
                  (p/display-width (nth h i ""))
                  (map #(p/display-width (nth % i "")) rs)))
          (range n-cols))
        ;; Total layout width = sum(col widths) + 2*n_cols (per-cell
        ;; one-space padding on each side of the cell text) +
        ;; (n_cols + 1) verticals (the `│` column dividers, one on
        ;; each end + one between every pair of cells).
        overhead   (+ (* 2 n-cols) (inc n-cols))
        avail      (max n-cols (- max-w overhead))
        sum-w      (max 1 (reduce + 0 col-widths-raw))
        ;; Per-column floor: never shrink a column below its widest
        ;; single token (capped). Word-wrap then breaks naturally on
        ;; spaces inside the column instead of slicing 'AGENTS.md' to
        ;; 'AGE…' just because the bubble is narrow. The cap stops a
        ;; lone 200-char URL from monopolising the table.
        col-mins   (mapv (fn [i]
                           (min TABLE_CELL_MIN_W_CAP
                             (longest-token-width
                               (cons (nth h i "")
                                 (map #(nth % i "") rs)))))
                     (range n-cols))
        sum-min    (reduce + 0 col-mins)
        col-widths (cond
                     ;; Natural widths fit — no shrinking needed.
                     (<= (+ sum-w overhead) max-w)
                     col-widths-raw

                     ;; Mins fit. Distribute remaining budget on per-
                     ;; column slack (raw - min) so wider columns
                     ;; receive proportionally more of the surplus,
                     ;; while every column keeps its word-wrap floor.
                     (<= sum-min avail)
                     (let [budget    (- avail sum-min)
                           slack     (mapv (fn [i]
                                             (max 0 (- (nth col-widths-raw i)
                                                      (nth col-mins i))))
                                       (range n-cols))
                           slack-sum (reduce + 0 slack)
                           extra     (if (zero? slack-sum)
                                       (vec (repeat n-cols 0))
                                       (mapv (fn [s]
                                               (int (* (/ (double s) slack-sum)
                                                      budget)))
                                         slack))
                           base      (mapv + col-mins extra)
                           used      (reduce + 0 base)
                           leftover  (max 0 (- avail used))]
                       (vec (map-indexed
                              (fn [i w] (if (< i leftover) (inc w) w))
                              base)))

                     ;; Even the floors don't fit (many narrow cols /
                     ;; tiny bubble). Degrade to the original uniform
                     ;; scale-down — content will hard-break, but the
                     ;; table still lands inside `max-w`.
                     :else
                     (let [scale  (/ (double avail) sum-w)
                           shrunk (mapv #(max 1 (int (* % scale))) col-widths-raw)
                           used   (reduce + 0 shrunk)
                           extra  (max 0 (- avail used))]
                       (if (zero? extra)
                         shrunk
                         (vec (map-indexed
                                (fn [i w] (if (< i extra) (inc w) w))
                                shrunk)))))
        ;; LIGHT box-drawing variants — the heavy glyphs read as
        ;; aggressive bold lines on macOS Terminal / iTerm2 with a
        ;; thick monospaced font; users wanted a quieter grid that
        ;; does not compete with the cell text for attention. Light
        ;; glyphs render thinner, the muted `code-border-fg`
        ;; foreground keeps them visible without dominating.
        bar        (fn [w] (repeat-str \─ (+ w 2)))
        top-line   (str "┌" (str/join "┬" (map bar col-widths)) "┐")
        head-sep   (str "├" (str/join "┼" (map bar col-widths)) "┤")
        ;; Same shape as `head-sep` — inserted between every pair of
        ;; body rows so each row sits in its own visual cell. Without
        ;; this the body looked like a stack of pipe-separated text
        ;; rows; with it, the table reads as a proper grid.
        row-sep    head-sep
        bot-line   (str "└" (str/join "┴" (map bar col-widths)) "┘")
        ;; Per-column alignment: numeric columns right-align, every
        ;; other column stays left. The header inherits the column's
        ;; alignment so the title sits over the data correctly
        ;; (e.g. a `Predkosc (km/h)` header above right-aligned
        ;; integers also right-aligns).
        col-aligns (mapv (fn [i] (column-align (map #(nth % i "") rs)))
                     (range n-cols))
        ;; Word-wrap each cell to its column width, then equalise
        ;; per-cell line counts with empty lines so the grid stays
        ;; aligned across multi-line rows. A 1-line row wraps to
        ;; itself; a row whose widest cell wraps to N lines emits N
        ;; physical lines, every other cell padded with blanks.
        format-row-lines
        (fn [cells]
          (let [wrapped (vec (map-indexed
                               (fn [i c]
                                 (wrap-cell-lines c (nth col-widths i)))
                               cells))
                row-h   (long (apply max 1 (map count wrapped)))
                padded  (mapv (fn [ws]
                                (vec (concat ws
                                       (repeat (- row-h (count ws)) ""))))
                          wrapped)]
            (mapv
              (fn [k]
                (str "│"
                  (str/join "│"
                    (map-indexed
                      (fn [i col-lines]
                        (pad-cell (nth col-lines k)
                          (nth col-widths i)
                          (nth col-aligns i)))
                      padded))
                  "│"))
              (range row-h))))]
    (vec (concat
           [(str tsep top-line)]
           ;; Header may itself wrap when a long heading meets a
           ;; shrunk column; mark every sub-line with `thead` so the
           ;; bubble paints them all bold.
           (mapv #(str thead %) (format-row-lines h))
           [(str tsep head-sep)]
           ;; Interleave `row-sep` between every pair of body rows
           ;; so each logical row sits in its own boxed cell. Each
           ;; logical row may now expand to multiple physical lines
           ;; (cell-level word-wrap), so flatten after interposing.
           (apply concat
             (interpose
               [(str tsep row-sep)]
               (mapv (fn [r]
                       (mapv #(str trow %) (format-row-lines r)))
                 rs)))
           [(str tsep bot-line)]))))

(defn- consume-table
  "At a markdown table boundary, peel off the header + separator + body
   rows from `lines` (a seq) and return `[table-render-lines
   remaining-lines]`. Caller has already verified `(table-row? hdr)`
   and `(table-separator-row? (second lines))`."
  [lines max-w markers]
  (let [hdr  (first lines)
        sep  (second lines)
        body (next (next lines))
        [body-rows tail]
        (loop [bs body acc []]
          (if (and bs (table-row? (first bs)) (not (table-separator-row? (first bs))))
            (recur (next bs) (conj acc (parse-table-row (first bs))))
            [acc bs]))
        headers (parse-table-row hdr)
        _       sep ;; separator just consumed
        rendered (render-table headers body-rows max-w markers)]
    [rendered tail]))

(def ^:private list-marker-line-re
  ;; Single-source regex shared by `coalesce-loose-list-items` (the
  ;; pre-pass) and the bullet branch of `markdown->lines`. Matches
  ;; both `- foo` / `* foo` / `+ foo` and `1. foo` / `1) foo` so the
  ;; pre-pass covers numbered lists too.
  #"^\s*(?:[-*+]|\d+[.)])\s+.*")

(defn- list-marker-line? [^String line]
  (boolean (re-matches list-marker-line-re line)))

(defn- top-level-bold-paragraph-line?
  "True when a line opens with `**...**` AND has more content after
   the closing `**`. Matches the shape of a top-level \"label: value\"
   paragraph (`**Loop clean:** true`, `**Status:** done.`) that we
   want to peel off the end of a preceding list.

   Excluded by design: lines that are PURELY a `**...**` span and
   nothing else (`**reader boundary split**`). Those are an artefact
   of `md/join` separating an inline bold from its surrounding prose
   with `\n\n`; the line is meant to flow inline as part of the
   bullet's body, not to start a fresh paragraph.

   The two shapes are visually identical at the start (`**...`);
   the trailing content after the closing `**` is what tells them
   apart. `find-inline-close` would do the same check at parse time,
   but here we only need a cheap structural sniff."
  [^String t]
  (and (str/starts-with? t "**")
    (let [close-idx (str/index-of t "**" 2)]
      (boolean
        (and close-idx
          ;; At least one non-whitespace char after the closing `**`.
          (let [tail (subs t (+ (long close-idx) 2))]
            (not (str/blank? tail))))))))

(defn- structural-non-list-line?
  "Block-level elements that close an open list scope when they
   appear on their own line. We close on these so a heading or fence
   landing right after a list item doesn't get sucked into the item
   as a continuation paragraph. Mirrors the cases `markdown->lines`
   has dedicated branches for; keep the two in sync.

   Bold paragraphs are split into two cases by
   `top-level-bold-paragraph-line?`:
     - `**Status:** done.` (label + value) — closes the list, the
       summary lines that authors put after a list never want to
       fold INTO the last bullet.
     - `**word**` alone (`md/join` artefact) — stays a continuation,
       so the bullet that contains a flowing sentence with embedded
       bold spans renders as one wrapped bullet, not as the bullet
       header followed by an avalanche of one-word paragraphs (the
       `Let / me / dig / deeper` regression)."
  [^String line]
  (let [t (str/trim line)]
    (or (str/starts-with? t "```")               ;; code fence
      (re-matches #"^#{1,6} .*" t)              ;; ATX heading
      (re-matches #"^([-*_])\1{2,}$" t)         ;; horizontal rule
      (re-matches #"^>\s?.*" t)                 ;; blockquote
      (re-matches #"^\|.*\|$" t)                ;; pipe-table row
      (re-matches #"^</?(?:details|proofs)(\s[^>]*)?>$" t) ;; details/proofs framing tags
      (re-matches #"^<summary>.*</summary>$" t) ;; summary tag
      (top-level-bold-paragraph-line? t))))      ;; **prefix:** value

(def ^:private continuation-tail-char?
  "Trailing characters on the running bullet line that signal `the
   sentence isn't done`. When the previous chunk ended with one of
   these, the next chunk — even a flush-left, letter-starting one —
   is treated as a continuation rather than a fresh paragraph. Covers
   open brackets / parens / braces, dashes, colons, semicolons,
   commas — every punctuator that grammar guarantees demands more
   text after it."
  #{\( \[ \{ \: \; \, \— \– \-})

(defn- fresh-paragraph-line?
  "True when `line` looks like a brand-new top-level paragraph that
   appeared after a blank line — i.e. flush-left, opens with an
   alphanumeric character. Used by `coalesce-loose-list-items` to
   close an open list scope when the post-blank line is OBVIOUSLY a
   new paragraph rather than an `md/join`-fragmented continuation
   of the current bullet.

   The two real-world shapes this discriminates:

     Case A (close — fresh paragraph after a list, the AGENTS.md
     pattern):
       - bullet item …
       - bullet item …

       Modes:

     Case B (keep coalescing — `md/join` artefact INSIDE a bullet's
     body, the eeaf9651 regression):
       - **Header:** intro

        38 failures across iterations …

       `src/com/blockether/...`

       , `db.clj`

   Case B's continuations always start with whitespace (md/join
   author-supplied leading space), a backtick (` `code` `), a `*`
   (bold span), or punctuation (`,`, `(`, `)`, `:`). None of those
   trip the `[A-Za-z0-9]` test. Case A's `Modes:` / `Logs:` /
   `Baseline = …` ALL open with a letter, so the test fires and
   the list closes cleanly.

   Indented continuations (`  more text` directly under a bullet)
   are NOT flush-left, so this returns false and the line still
   folds into the current bullet — preserving the documented
   `- foo\n\n indented continuation` reflow."
  [^String line]
  (boolean (re-matches #"^[A-Za-z0-9].*" line)))

(defn- coalesce-loose-list-items
  "Pre-pass over input lines that collapses multi-paragraph list
   items into single-line items.

   Why: poorly-formatted LLM (or hand-typed) markdown often emits

       - `dialogs.clj`

        — removed `:system-prompt` palette command

   where blank lines INSIDE a list item turn the bullet's body into
   a chain of loose top-level paragraphs (CommonMark spec-compliant
   but visually broken: only the first fragment paints under the
   `• ` marker, every subsequent paragraph shows up flush-left as
   if it weren't part of the bullet at all). The TUI bubble has
   nowhere to render that hierarchy correctly, so we coalesce the
   loose paragraphs back into the bullet line before parsing.

   The list scope ends on:
   - another bullet / numbered list marker (start of next item),
   - any non-list structural line (heading, code fence, blockquote,
     pipe-table row, horizontal rule, `<details>` / `<summary>`),
   - two consecutive blank lines (CommonMark loose-list close),
   - end of input.

   Lines outside an open list pass through unchanged — we only
   reshape content that's clearly a fragmented bullet item.

   Continuations are joined with a single ASCII space; leading
   whitespace on the continuation line is dropped (it was Markdown
   indentation, not visible content)."
  [lines]
  (loop [lines    (seq lines)
         current  nil      ;; the bullet line we're accumulating into
         blanks   0         ;; buffered consecutive blank lines
         acc      []]
    (cond
      ;; End of input — flush.
      (nil? lines)
      (cond-> acc current (conj current))

      :else
      (let [line (first lines) rst (next lines)]
        (cond
          ;; In list scope, see another list marker → flush current,
          ;; start a new item. Buffered blanks were intra-item; drop
          ;; them (the new item separates itself).
          (and current (list-marker-line? line))
          (recur rst line 0 (conj acc current))

          ;; In list scope, hit a structural non-list line → flush
          ;; current with one blank separator (so the heading / fence
          ;; that follows reads as a fresh block), then re-process
          ;; this line in idle mode by NOT advancing `lines`.
          (and current (structural-non-list-line? line))
          (recur lines nil 0 (conj acc current ""))

          ;; In list scope, blank line. One blank → buffer; two in a
          ;; row → list closes (CommonMark loose-list end).
          (and current (str/blank? line))
          (let [n (inc blanks)]
            (if (>= n 2)
              (recur rst nil 0 (conj acc current ""))
              (recur rst current n acc)))

          ;; In list scope, plain text line that LOOKS LIKE a fresh
          ;; top-level paragraph (flush-left, opens with a letter or
          ;; digit) AND we just saw a blank line → close the list
          ;; scope and re-process the line in idle mode. This stops
          ;; AGENTS.md-style summary paragraphs (`Modes:`, `Logs: …`,
          ;; `Baseline = …`) after a bullet list from being silently
          ;; folded into the last bullet's body. CommonMark closes a
          ;; list on a single blank line + non-indented continuation;
          ;; we mirror that for the plain-prose case while leaving
          ;; `md/join` artefacts (lines opening with whitespace,
          ;; backtick, `*`, punctuation) on the continuation path so
          ;; the eeaf9651 regression stays fixed.
          ;;
          ;; Exception: if `current` ends with an open-grammar tail
          ;; char (`(`, `[`, `{`, `:`, `;`, `,`, `—`, `–`, `-`), the
          ;; sentence is mid-clause and the next line continues it,
          ;; even when that line is flush-left and letter-starting.
          ;; Covers the `- foo (\n\nbar\n\n, baz\n\n)` reflow case.
          ;; Tested by AGENTS.md repro + `loose-bullet-coalesce-test`.
          (and current
            (pos? blanks)
            (fresh-paragraph-line? line)
            (let [^String trimmed (str/trimr current)
                  n (.length trimmed)]
              (or (zero? n)
                (not (continuation-tail-char? (.charAt trimmed (dec n)))))))
          (recur lines nil 0 (conj acc current ""))

          ;; In list scope, plain text line → continuation. Strip
          ;; leading indentation (Markdown structural whitespace),
          ;; join with one space onto the running bullet text.
          ;; Then re-flow whitespace around punctuation that the
          ;; fragmentation left orphaned: ill-formed input often
          ;; has `(\n\nfoo\n\n,\n\nbar\n\n)` which would otherwise
          ;; emerge as `( foo , bar )`. The replacements collapse
          ;; ` ,` / ` .` / ` ;` / ` :` / ` )` and `( ` back to their
          ;; natural prose forms.
          ;;
          ;; `[` / `]` are intentionally NOT reflowed: in real prose
          ;; they almost always appear inside quoted text or code
          ;; spans (`\"Unmatched delimiter: ]\"`). Tightening ` ]` to
          ;; `]` there would mangle the user-visible payload.
          current
          (let [cont (str/trim line)
                joined (if (str/blank? cont)
                         current
                         (-> (str (str/trimr current) " " cont)
                           (str/replace #" +([,;:.\)])" "$1")
                           (str/replace #"(\() +" "$1")))]
            (recur rst joined 0 acc))

          ;; Idle, see a list marker → enter list scope.
          (list-marker-line? line)
          (recur rst line 0 acc)

          ;; Idle, anything else → pass through unchanged.
          :else
          (recur rst nil 0 (conj acc line)))))))

(defn- markdown->lines-plain
  "Markdown renderer without real `<details>` state. Kept as the base
   parser; `markdown->entries` wraps it with recursive details support
   and click metadata."
  ([text max-w] (markdown->lines-plain text max-w :answer))
  ([text max-w mode]
   (when (and text (not (str/blank? text)))
     (let [m       (get md-marker-sets mode (md-marker-sets :answer))
           plain-m (:plain m)
           emit-plain (fn [s] (str plain-m s))
           input-lines (coalesce-loose-list-items (str/split-lines text))]
       (loop [lines  (seq input-lines)
              in-code? false
              acc    []]
         (if-not lines
           acc
           (let [line (first lines)
                 rst  (next lines)]
             (cond
               (and (not in-code?) (str/starts-with? (str/trim line) "```"))
               (let [lang        (fence-lang line)
                     indent      (leading-space-count line)
                     prefix      (repeat-str \space indent)
                     [code tail] (split-with #(not (str/starts-with? (str/trim %) "```")) rst)
                     code        (if (pos? indent)
                                   (mapv #(strip-leading-spaces-up-to indent %) code)
                                   code)
                     tail        (if (seq tail) (next tail) tail)]
                 (recur (seq tail) false
                   (into acc (code-block-lines m lang code max-w prefix))))

               (str/starts-with? (str/trim line) "```")
               (recur rst false (conj acc (str (:code m) "")))

               in-code?
               (recur rst true
                 (into acc (mapv #(str (:code m) %)
                             (wrap-text line max-w))))

               (and (table-row? line)
                 (table-separator-row? (first rst)))
               (let [[tbl tail] (consume-table lines max-w m)]
                 (recur (seq tail) false (into acc tbl)))

               (re-matches #"^\s*</?(?:details|proofs)(\s[^>]*)?>\s*$" line)
               (recur rst false acc)

               (re-matches #"^\s*<summary>.*</summary>\s*$" line)
               (let [[_ inner] (re-matches #"^\s*<summary>(.*)</summary>\s*$" line)
                     decorated (markdown->inline (or inner ""))
                     prefix    "▾ "
                     wrap-w    (max 1 (- max-w (count prefix)))]
                 (recur rst false
                   (into acc
                     (into [(str (:summary m) prefix (first (wrap-text decorated wrap-w)))]
                       (mapv #(str (:summary m) "  " %)
                         (rest (wrap-text decorated wrap-w)))))))

               (re-matches #"^#{3,6} .*" line)
               (let [hash-count (count (re-find #"^#+" line))
                     body       (markdown->inline (subs line (inc hash-count)))]
                 (recur rst false
                   (into acc (mapv #(str (:h3 m) %)
                               (wrap-text body max-w)))))

               (str/starts-with? line "## ")
               (recur rst false
                 (into acc (mapv #(str (:h2 m) %)
                             (wrap-text (markdown->inline (subs line 3)) max-w))))

               (str/starts-with? line "# ")
               (recur rst false
                 (into acc (mapv #(str (:h1 m) %)
                             (wrap-text (markdown->inline (subs line 2)) max-w))))

               (re-matches #"^\s*([-*_])\1{2,}\s*$" line)
               (recur rst false
                 (conj acc (str (:hr m) (repeat-str \─ max-w))))

               (re-matches #"^\s*>\s?.*" line)
               (let [trimmed (str/replace line #"^\s*>\s?" "")
                     decorated (markdown->inline trimmed)
                     wrapped (wrap-text decorated (max 1 (- max-w 2)))]
                 (recur rst false
                   (into acc (mapv #(str (:quote m) "┃ " %) wrapped))))

               (re-matches #"^\s*[-*+]\s+.*" line)
               (let [[_ indent trimmed] (re-matches #"^(\s*)[-*+]\s+(.*)$" line)
                     prefix    (nested-prefix indent ["• " "◦ " "▪ " "• "])
                     decorated (markdown->inline trimmed)
                     wrapped   (wrap-text decorated (max 1 (- max-w (count prefix))))]
                 (recur rst false
                   (into acc
                     (into [(str (:bullet m) prefix (first wrapped))]
                       (mapv #(str (:bullet m) (repeat-str \space (count prefix)) %) (rest wrapped))))))

               (re-matches #"^\s*\d+[.)]\s+.*" line)
               (let [[_ indent num body] (re-matches #"^(\s*)(\d+)[.)]\s+(.*)$" line)
                     decorated (markdown->inline (or body ""))
                     prefix    (nested-prefix indent [(str num ". ") (str num ". ") (str num ". ") (str num ". ")])
                     wrapped   (wrap-text decorated (max 1 (- max-w (count prefix))))]
                 (recur rst false
                   (into acc
                     (into [(str (:bullet m) prefix (first wrapped))]
                       (mapv #(str (:bullet m) (repeat-str \space (count prefix)) %)
                         (rest wrapped))))))

               (and (str/starts-with? (str/trim line) "**")
                 (str/ends-with? (str/trim line) "**")
                 (> (count (str/trim line)) 4))
               (let [trimmed (str/trim line)
                     inner   (markdown->inline (subs trimmed 2 (- (count trimmed) 2)))]
                 (recur rst false
                   (into acc (mapv #(str (:bold m) %)
                               (wrap-text inner max-w)))))

               (str/blank? line)
               (recur rst false (conj acc (emit-plain "")))

               :else
               (let [decorated (markdown->inline line)]
                 (recur rst false
                   (into acc (mapv emit-plain (wrap-text decorated max-w)))))))))))))

(defn- detail-open-kind [line]
  (when-let [[_ tag] (and (string? line)
                       (re-matches #"^\s*<(details|proofs)(?:\s[^>]*)?>\s*$" line))]
    (keyword tag)))

(defn- details-open-line? [line]
  (boolean (detail-open-kind line)))

(defn- details-close-line? [line]
  (boolean (and (string? line)
             (re-matches #"^\s*</(?:details|proofs)>\s*$" line))))

(defn- summary-content [line]
  (when-let [[_ inner] (and (string? line)
                         (re-matches #"^\s*<summary>(.*)</summary>\s*$" line))]
    inner))

(defn- trim-trailing-blank-lines
  [lines]
  (vec (reverse (drop-while str/blank? (reverse (or lines []))))))

(defn- parse-detail-segments
  [lines]
  (letfn [(consume* [lines path]
            (loop [remaining (seq lines)
                   plain []
                   segs []
                   next-idx 1]
              (cond
                (nil? remaining)
                [(cond-> segs (seq plain) (conj {:type :plain :lines plain})) nil]

                (details-close-line? (first remaining))
                [(cond-> segs (seq plain) (conj {:type :plain :lines plain})) remaining]

                (details-open-line? (first remaining))
                (let [plain'           (trim-trailing-blank-lines plain)
                      margin-top?      (boolean (or (seq segs) (seq plain')))
                      segs             (cond-> segs (seq plain') (conj {:type :plain :lines plain'}))
                      detail-kind      (detail-open-kind (first remaining))
                      path'            (conj path next-idx)
                      after-open       (next remaining)
                      explicit-summary (summary-content (first after-open))
                      summary          (or explicit-summary
                                         (if (= :proofs detail-kind) "Proofs" "Details"))
                      body-start       (if explicit-summary (next after-open) after-open)
                      [body rem]       (consume* body-start path')
                      rem'             (if (and rem (details-close-line? (first rem))) (next rem) rem)
                      seg              {:type :detail :detail-kind detail-kind :summary summary :path path' :segments body :margin-top? margin-top?}]
                  (recur rem' [] (conj segs seg) (inc next-idx)))

                :else
                (recur (next remaining) (conj plain (first remaining)) segs next-idx))))]
    (first (consume* lines []))))

(defn- render-detail-segments
  [segments max-w mode opts]
  (mapcat
    (fn [{:keys [type lines summary path segments detail-kind margin-top?]}]
      (case type
        :plain
        (mapv (fn [line] {:line line :meta nil})
          (or (markdown->lines-plain (str/join "\n" lines) max-w mode) []))

        :detail
        (let [m            (get md-marker-sets mode (md-marker-sets :answer))
              marker       (:summary m)
              conversation-id (:conversation-id opts)
              turn-id      (:conversation-turn-id opts)
              section      (:section opts)
              iteration-number (:iteration-number opts)
              detail-ctx   {:conversation-id conversation-id
                            :conversation-turn-id turn-id
                            :iteration-number iteration-number
                            :block-number (:block-number opts)
                            :details-path path
                            :section section
                            :kind detail-kind
                            :proofs? (= :proofs detail-kind)}
              node-id      (detail-node-id detail-ctx)
              expanded?    (detail-expanded? (:detail-expansions opts)
                             conversation-id
                             node-id
                             (not= :proofs detail-kind))
              body-entries (vec (render-detail-segments segments max-w mode opts))]
          (vec (concat
                 (when margin-top? [{:line "" :meta nil}])
                 (detail-summary-entries (assoc detail-ctx
                                           :marker marker
                                           :max-w max-w
                                           :summary summary
                                           :hidden-entries body-entries
                                           :collapsed? (not expanded?)
                                           :node-id node-id))
                 (when expanded? body-entries))))))
    segments))

(defn- relevant-detail-expansions-key
  "Stable cache key for only the disclosure nodes this Markdown projection
   can render. A click in one old answer must not bust cached projections for
   every other visible answer. Scope by conversation + node-id base."
  [opts]
  (let [conversation-id (some-> (:conversation-id opts) str)
        base            (detail-node-base-id opts)
        prefix          (str base ":")]
    (->> (:detail-expansions opts)
      (keep (fn [[[cid node-id] expanded?]]
              (let [node-id (str node-id)]
                (when (and (= conversation-id (str cid))
                        (or (= base node-id)
                          (str/starts-with? node-id prefix)))
                  [node-id (boolean expanded?)]))))
      sort
      vec)))

(defn- turn-detail-expansions-key
  "Stable cache key for any disclosure belonging to this rendered assistant
   turn. Used by the outer trace+answer projection, which may contain thinking,
   iteration, tool/result, and final-answer disclosures."
  [opts]
  (let [conversation-id (some-> (:conversation-id opts) str)
        turn-fragment   (some-> (:conversation-turn-id opts) short-id-fragment)
        turn-token      (when turn-fragment (str ":t" turn-fragment))]
    (->> (:detail-expansions opts)
      (keep (fn [[[cid node-id] expanded?]]
              (let [node-id (str node-id)]
                (when (and (= conversation-id (str cid))
                        (or (nil? turn-token)
                          (str/includes? node-id turn-token)))
                  [node-id (boolean expanded?)]))))
      sort
      vec)))

(defn- markdown-entries-cache-key
  [text max-w mode opts]
  ;; Never call `(hash text)` here: first hash of a huge String scans
  ;; the whole thing, exactly what this cache is supposed to avoid.
  ;; Bounded fingerprint only. Collision risk is acceptable for a render
  ;; cache; wrong cache entry would be cosmetic and LRU-bounded, while
  ;; full hashing can freeze the UI.
  (let [n (count text)]
    [::md-entries
     n
     (subs text 0 (min 64 n))
     (subs text (max 0 (- n 256)))
     (long max-w)
     mode
     (:conversation-id opts)
     (:conversation-turn-id opts)
     (:iteration-number opts)
     (:block-number opts)
     (:section opts)
     (relevant-detail-expansions-key opts)]))

(defn markdown->entries
  ([text max-w] (markdown->entries text max-w :answer nil))
  ([text max-w mode] (markdown->entries text max-w mode nil))
  ([text max-w mode opts]
   (when (and text (not (str/blank? text)))
     (let [text (str text)]
       (cached* (markdown-entries-cache-key text max-w mode opts)
         #(let [text     (md-repair/normalize-chat-markdown text)
                segments (parse-detail-segments (coalesce-loose-list-items (str/split-lines text)))]
            (vec (render-detail-segments segments max-w mode opts))))))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} markdown->lines
  ([text max-w] (markdown->lines text max-w :answer nil))
  ([text max-w mode] (markdown->lines text max-w mode nil))
  ([text max-w mode opts]
   (some->> (markdown->entries text max-w mode opts)
     (mapv :line))))

(defn format-answer-with-thinking-data*
  "Uncached implementation. Returns `{:text :lines :line-meta}` so the
   bubble painter can keep clickable summary-row metadata aligned with
   the already-wrapped lines."
  [answer trace bubble-w settings confidence cancelled? opts]
  (let [content-w               (max 10 (- bubble-w 4))
        fill-w                  (max 1 (dec content-w))
        show-thinking?          (get settings :show-thinking true)
        show-iterations?        (get settings :show-iterations true)
        show-iteration-headers? (get settings :show-iteration-headers false)
        show-final-hdr?         (get settings :show-final-answer-header false)
        line-entry              (fn [line] {:line line :meta nil})
        trace-entries           (when (and show-iterations? (seq trace))
                                  (into []
                                    (mapcat (fn [[idx entry]]
                                              (format-iteration-entry-entries
                                                (if show-thinking? entry (dissoc entry :thinking))
                                                content-w (inc idx)
                                                {:show-header?         show-iteration-headers?
                                                 :conversation-id      (:conversation-id opts)
                                                 :detail-expansions   (:detail-expansions opts)
                                                 :conversation-turn-id (:conversation-turn-id opts)})))
                                    (collapse-repeated-error-runs trace)))
        answer-str              (or answer "")
        fa-label                (label-text "final answer")
        conf-str                (when confidence (str " · " (name confidence)))
        full-label              (str fa-label (or conf-str ""))
        fa-pad                  (max 0 (- fill-w (count full-label) 1))
        fa-hdr                  (when (and show-final-hdr? (not cancelled?))
                                  (line-entry (str answer-hdr-marker (repeat-str \space fa-pad) full-label " ")))
        md-entries              (markdown->entries answer-str (max 1 (- fill-w 2)) :answer
                                  {:conversation-id      (:conversation-id opts)
                                   :conversation-turn-id (:conversation-turn-id opts)
                                   :detail-expansions   (:detail-expansions opts)
                                   :section             :answer})
        ans-entries             (if (seq md-entries)
                                  md-entries
                                  (mapv line-entry (wrap-text answer-str (max 1 (- fill-w 2)))))
        ans-pad                 (line-entry (str answer-pad-marker ""))
        cancel-text             (or answer "Cancelled by user.")
        cancel-rows             (mapv line-entry (wrap-text cancel-text (max 1 (- fill-w 2))))
        cancel-block            (vec (concat [(line-entry "")] cancel-rows [(line-entry "")]))
        answer-block            (cond-> []
                                  fa-hdr (conj fa-hdr)
                                  :always (conj ans-pad)
                                  :always (into ans-entries)
                                  :always (conj ans-pad))
        trailer                 (if cancelled? cancel-block answer-block)
        entries                 (if (seq trace-entries)
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
             confidence
             (boolean cancelled?)
             (:conversation-turn-id opts)
             (turn-detail-expansions-key opts)]
     #(format-answer-with-thinking-data* answer trace bubble-w settings confidence cancelled? opts))))

(defn format-answer-with-thinking
  ([answer trace bubble-w] (format-answer-with-thinking answer trace bubble-w nil nil false nil))
  ([answer trace bubble-w settings] (format-answer-with-thinking answer trace bubble-w settings nil false nil))
  ([answer trace bubble-w settings confidence] (format-answer-with-thinking answer trace bubble-w settings confidence false nil))
  ([answer trace bubble-w settings confidence cancelled?] (format-answer-with-thinking answer trace bubble-w settings confidence cancelled? nil))
  ([answer trace bubble-w settings confidence cancelled? opts]
   (:text (format-answer-with-thinking-data answer trace bubble-w settings confidence cancelled? opts))))

(defn format-answer-markdown-data*
  [answer bubble-w opts]
  (let [content-w  (max 10 (- bubble-w 4))
        md-entries (markdown->entries (or answer "") content-w :answer opts)
        entries    (if (seq md-entries)
                     (vec md-entries)
                     (mapv (fn [line] {:line line :meta nil}) (wrap-text (or answer "") content-w)))]
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
             (:conversation-turn-id opts)
             (relevant-detail-expansions-key opts)]
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
   - `inner-h`   visible viewport height (== `track-h`).
   - `scroll`    current row offset; `nil` means auto-bottom.

   Returns `{:thumb-top-rel long :thumb-h long :max-scroll long}`,
   where `:thumb-top-rel` is rows from the TOP of the track (caller
   adds the absolute `bar-top` to convert to screen coordinates).
   The visible thumb is intentionally one terminal cell high: Terminal.app
   renders stacked full-block cells as a ladder of separate boxes when
   the window is tall, which reads as multiple scroll positions instead
   of one current-position marker. Returns `nil` when there's no overflow
   — no thumb is drawn, no click should hit-test as on-thumb."
  [^long total-h ^long inner-h scroll]
  (when (and (pos? inner-h) (> total-h inner-h))
    (let [track-h    inner-h
          max-scroll (max 1 (- total-h inner-h))
          eff-scroll (let [s (long (or scroll max-scroll))]
                       (max 0 (min s max-scroll)))
          thumb-h    1
          thumb-top  (long (* (- track-h thumb-h)
                             (/ (double eff-scroll) max-scroll)))]
      {:thumb-top-rel thumb-top
       :thumb-h       thumb-h
       :max-scroll    max-scroll})))
;; ^ Convenience: the total horizontal gutter consumed on each row.
;; `bubble-w = cols - MESSAGE_SIDE_PAD`. Both this file's painter and
;; `screen.clj`'s height calculator MUST use this exact derivation.

(defn draw-messages-area!
  "Draw structured chat messages as left-aligned blocks inside a clean,
   border-less scrolling area.

   `layout` is a virtual-layout plan produced by
   `com.blockether.vis.ext.channel-tui.virtual/layout`. Carries
   `{:total-h :eff-scroll :visible}` — the painter only iterates
   `:visible` (the messages whose interval intersects the viewport),
   so off-screen bubbles never trigger `format-answer-with-thinking`
   / `wrap-text` / `markdown->lines`. That's the single largest
   user-visible win for cold-opening long conversations: cold paint
   drops from O(N × trace-size) to O(visible × trace-size).

   No outer box, no title bar — just a vertical column of messages with
   generous side gutters. The right gutter doubles as scrollbar space
   when the conversation overflows. The conversation title (if any) is
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
    ;; …) register theirs too. The published registry that the
    ;; input thread reads doesn't change until the screen calls
    ;; `cr/commit-frame!` after every painter has run — so the
    ;; input thread never lookup-misses on a half-filled buffer.
    (cr/begin-frame!)

    (let [clip (.newTextGraphics g
                 (TerminalPosition. 0 text-top)
                 (TerminalSize. cols inner-h))]
      (doseq [{:keys [^long top projected]} visible]
        (draw-chat-bubble! clip projected top
          MESSAGE_MARGIN_LEFT bubble-w
          {:viewport-top text-top :viewport-h inner-h}))

      (when-let [{:keys [thumb-top-rel thumb-h]}
                 (scrollbar-thumb-geometry total-h inner-h eff-scroll)]
        ;; Place the scrollbar inside the right gutter so it never
        ;; overlaps message content.
        (let [bar-col (- cols 2)
              bar-top text-top
              track-h inner-h]
          (doseq [r (range track-h)]
            (p/set-colors! g t/border-fg t/terminal-bg)
            (p/set-char! g bar-col (+ bar-top r) Symbols/SINGLE_LINE_VERTICAL))
          (doseq [r (range thumb-h)]
            (p/set-colors! g t/dialog-hint-key t/terminal-bg)
            (p/set-char! g bar-col (+ bar-top thumb-top-rel r) \u2588)))))))
