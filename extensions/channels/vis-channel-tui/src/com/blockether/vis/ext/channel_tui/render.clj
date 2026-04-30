(ns com.blockether.vis.ext.channel-tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.links :as links]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t])
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

(defn wrap-text*
  "Uncached implementation. Prefer `wrap-text` everywhere except inside
   `wrap-text` itself."
  [text max-width]
  (if (or (str/blank? text) (<= max-width 0))
    [""]
    (let [input-lines (str/split-lines text)]
      (into []
        (mapcat
          (fn [line]
            (if (<= (p/display-width line) max-width)
              [line]
              (loop [remaining line
                     acc       []]
                (if (<= (p/display-width remaining) max-width)
                  (conj acc remaining)
                     ;; `cut` is the CHAR index that bounds the longest
                     ;; prefix fitting in `max-width` COLUMNS without
                     ;; splitting a grapheme. Char-index, not column,
                     ;; because we still need to slice `remaining`.
                  (let [cut     (p/col-prefix-end remaining max-width)
                        chunk   (subs remaining 0 cut)
                        last-sp (str/last-index-of chunk " ")]
                    (if (and last-sp (pos? last-sp))
                         ;; Break at word boundary
                      (recur (subs remaining (inc (int last-sp)))
                        (conj acc (subs remaining 0 (int last-sp))))
                         ;; No space found — hard break at column boundary
                      (recur (subs remaining cut)
                        (conj acc chunk)))))))))
        input-lines))))

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
        v-scroll   (max 0 (- cursor-vrow (dec text-rows)))]
    (draw-box-border! g box-top box-bottom cols hint false)
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
(declare extract-link-refs paint-link-chrome-row!)

;; `chrome-display-text` (further down) calls into `markdown->inline`
;; via `strip-inline-markup` so the chrome row doesn't show raw
;; `**` / `_` / backticks that the LLM put inside link bracket text.
;; The actual `markdown->inline` body lives in the inline tokeniser
;; section ~900 lines below; the existing forward-decl down there is
;; far too late for a chrome helper this high in the file. We pin
;; the var here so Clojure can resolve the symbol at compile time.
(declare markdown->inline)

(defn draw-chat-bubble!
  "Draw a chat message at the given row. No border, no bubble container.
   `message` is a map: {:role :user|:assistant, :text str, :timestamp #inst}
   Optional `:duration-ms` for assistant response time.
   `left` and `max-w` define the horizontal bounds.

   Layout (no outer border, no horizontal rule under the label,
   both roles left-anchored):
     [role-label]                              [timestamp]
     [content lines, each with role bg fill]
     [meta line, dimmed (skipped for cancelled status)]
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
        lines     (wrap-text text content-w)
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
                       (when-not (str/blank? line) line)))]

    ;; Row 0: label (bold, role-colored) + timestamp (dim, right-aligned).
    (p/clear-styles! g)
    (p/set-colors! g role-fg t/terminal-bg)
    (p/styled g [p/BOLD]
      (p/put-str! g bx start-row label))
    (when time-str
      (p/set-colors! g t/dialog-hint t/terminal-bg)
      (p/put-str! g (+ bx (max (count label) (- bubble-w (count time-str)))) start-row time-str))

    ;; Content begins directly under the role banner for ASSISTANT
    ;; bubbles — the previous breathing row read as an unwanted top
    ;; margin above the thinking block. USER bubbles keep the
    ;; breathing row so the yellow block has visible vertical
    ;; padding above the typed text (otherwise the yellow band
    ;; hugs the first character and reads as a single-row strip).
    ;; Mirror this in `bubble-height*` so the math stays in sync.
    (let [top-pad (if user? 1 0)
          btop    (+ start-row 1 top-pad)]
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
      ;; User messages: fill the breathing row above content + the
      ;; content rows + the would-be meta-row below (always empty
      ;; for user messages per `meta-str (when (and (not user?) ...))`).
      ;; The two extra rows give the yellow block visible vertical
      ;; padding so the bubble reads as a proper highlighted zone
      ;; instead of a single-row band hugging the text.
      (when user?
        (p/set-bg! g bg-color)
        (p/fill-rect! g bx (- btop top-pad) bubble-w (+ (max 1 bubble-h) top-pad 1)))

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
        (loop [i (long i-start)]
          (when (< i i-end)
            (let [line (nth lines i)]
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

              ;; ── Code (success) — neutral code bg, green ✓ suffix ──
                  (str/starts-with? line code-ok-marker)
                  (let [raw (subs line 1)]
                    (p/set-colors! g t/code-block-fg t/code-block-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y raw)
                    (when-let [ci (str/index-of raw "✓")]
                      (p/set-colors! g t/code-success-fg t/code-block-bg)
                      (p/put-str! g (+ x ci) y (subs raw ci))))

              ;; ── Code (error) — light red bg, red ✗ suffix ──
                  (str/starts-with? line code-err-marker)
                  (let [raw (subs line 1)]
                    (p/set-colors! g t/code-block-fg t/code-err-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y raw)
                    (when-let [ci (str/index-of raw "✗")]
                      (p/set-colors! g t/code-error-fg t/code-err-bg)
                      (p/put-str! g (+ x ci) y (subs raw ci))))

              ;; ── Code (streaming, no status yet) — neutral gray bg ──
                  (str/starts-with? line code-marker)
                  (do (p/set-colors! g t/code-block-fg t/code-block-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Duration annotation ──
                  (str/starts-with? line duration-marker)
                  (do (p/set-colors! g t/code-duration-fg iteration-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

              ;; ── Result (success) — dim on code bg ──
                  (str/starts-with? line result-marker)
                  (do (p/set-colors! g t/code-result-fg t/code-block-bg)
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

              ;; ── Code block padding (success) ──
                  (str/starts-with? line code-pad-marker)
                  (do (p/set-bg! g t/code-block-bg)
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
                  (do (p/set-colors! g t/md-summary-fg t/md-summary-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/styled g [p/BOLD]
                      (p/paint-styled-line! g x y (subs line 1)
                        t/md-summary-fg t/md-summary-bg
                        t/code-block-fg t/code-block-bg)))

                  (str/starts-with? line md-code-marker)
                  (do (p/set-colors! g t/code-block-fg t/code-block-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/put-str! g x y (subs line 1)))

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
                  (do (p/set-colors! g t/th-md-summary-fg t/th-md-summary-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/styled g [p/BOLD p/ITALIC]
                      (p/paint-styled-line! g x y (subs line 1)
                        t/th-md-summary-fg t/th-md-summary-bg
                        t/code-result-fg t/code-block-bg)))

              ;; Thinking fenced code: visible code-block bg, italic dim text.
              ;; No sentinels in code-block bodies (markdown->lines does
              ;; NOT recurse into fenced code), so a raw put-str! is fine.
                  (str/starts-with? line th-md-code-marker)
                  (do (p/set-colors! g t/code-result-fg t/code-block-bg)
                    (p/fill-rect! g fbx y iw 1)
                    (p/styled g [p/ITALIC]
                      (p/put-str! g x y (subs line 1))))

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
              (recur (inc i))))))

      ;; Below-content link-chrome strip.
      ;;
      ;; One chrome row per (md/link …) / (md/image …) /
      ;; (md/file-link …) reference present in the message body.
      ;; Painted between content and meta so it sits inside the
      ;; bubble’s footprint but below the prose. Each row registers
      ;; itself with `click-regions` so the screen mouse handler
      ;; can hand the click off to the OS opener.
      (let [refs           (extract-link-refs message bubble-w)
            chrome-rows    (count refs)
            chrome-top     (+ btop bubble-h)
            ;; Coordinate translation: `clip` was created at
            ;; `(0, viewport-top)`, so y inside the clip == y +
            ;; viewport-top on the absolute screen. The chrome
            ;; painter needs the absolute row to register a click
            ;; region.
            abs-col-base   0
            abs-row-of     (fn ^long [^long y] (+ (long viewport-top) y))]
        (when (pos? chrome-rows)
          (doseq [[i ref] (map-indexed vector refs)]
            (let [y       (+ chrome-top i)
                  abs-row (abs-row-of y)]
              (paint-link-chrome-row!
                g ref bx y bubble-w
                abs-col-base abs-row
                (long viewport-top) (long viewport-h)))))

        ;; Below-content meta row.
        ;;
        ;; Final per-message layout (no outer box, no bg fill, no
        ;; horizontal rule under the label):
        ;;   row 0                    : label + timestamp
        ;;   row 1 … N                : wrapped content (with marker-zone fills)
        ;;   row 1+N … 1+N+M-1        : link chrome (M refs)
        ;;   row 1+N+M                : meta (right-aligned, dim) — skipped for cancelled
        ;;   row 2+N+M                : single blank gap before the next message
        (p/clear-styles! g)
        (let [meta-row (+ chrome-top chrome-rows)]
          (when meta-str
            (p/set-colors! g t/dialog-hint t/terminal-bg)
            (p/put-str! g (+ bx (max 0 (- bubble-w (count meta-str)))) meta-row meta-str))
          ;; Return: rows consumed
          ;;   = label(1) + content(N) + chrome(M) + meta(1) + gap(1)
          (+ 1 bubble-h chrome-rows 1 1))))))

;; ---------------------------------------------------------------------------
;; Link / image / file-link chrome
;;
;; Painted as a strip of one-row chrome entries between the wrapped
;; content and the meta line. Each entry is the user-visible
;; clickable affordance for a markdown reference (`[text](url)`,
;; `![alt](url)`, `[path:line](path#Lline)`) the model emitted in
;; the answer.
;;
;; Why a sidecar strip instead of inlining clickable spans into the
;; existing wrap-and-paint pipeline:
;;
;;   - The inline tokeniser already runs over `:answer` text and
;;     intentionally drops link/image markup as plain prose. Touching
;;     it would force a second pass through the styled-line painter
;;     and risk regressions in the existing markdown rendering.
;;
;;   - Click-region bounds are easier to reason about row-aligned
;;     than mid-line. A whole row is the click target; hover
;;     highlights the whole row.
;;
;;   - The chrome doubles as a discoverability cue — the user sees
;;     “these N things in this answer are clickable” at a glance,
;;     not a sea of inline blue text they have to hunt for.
;;
;; The strip is OPT-OUT for callers that pass `:hide-links? true` on
;; the message map. Today that flag isn’t set anywhere; reserved as
;; an escape hatch for warnings / cancelled messages where we deem
;; the chrome noise.

(def ^:private link-icon-image    "\uD83D\uDCF7")  ; 📷
(def ^:private link-icon-link     "\uD83D\uDD17")  ; 🔗
(def ^:private link-icon-file     "\uD83D\uDCC4")  ; 📄
(def ^:private link-icon-blocked  "\uD83D\uDEAB")  ; 🚫

(defn- ref-icon
  "Return the chrome icon string for `ref`. Disabled refs always
   render with the blocked icon regardless of kind so the user
   reads “this one is dead” before reading the URL."
  ^String [{:keys [kind enabled?]}]
  (cond
    (not enabled?) link-icon-blocked
    (= kind :image) link-icon-image
    (= kind :file)  link-icon-file
    :else           link-icon-link))

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
  (if hide-links?
    []
    (cached* [::refs (System/identityHashCode text) (long max-w)]
      #(extract-link-refs* (or text "") max-w))))

(defn- in-viewport?
  "True when `abs-row` is inside the currently-painted messages
   viewport `[viewport-top, viewport-top + viewport-h)`."
  [^long viewport-top ^long viewport-h ^long abs-row]
  (and (>= abs-row viewport-top)
    (< abs-row (+ viewport-top viewport-h))))

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
   + link-chrome(refs) + meta(1) + gap(1). Mirrors `draw-chat-bubble!`'s
   wrap width (`bubble-w - 2*h-pad`) so layout math stays consistent
   across the height calc and the draw."
  [{:keys [text role] :as message} max-w]
  (let [bubble-w  max-w
        h-pad     2
        content-w (max 1 (- bubble-w (* 2 h-pad)))
        lines     (wrap-text text content-w)
        top-pad   (if (= role :user) 1 0)
        refs      (extract-link-refs message bubble-w)]
    (+ 1 top-pad (count lines) (count refs) 1 1)))

(defn bubble-height
  "Memoized `bubble-height*`. Keyed by `:text` identity + role +
   width — same string instance + role + width = same height, no
   need to re-wrap."
  [{:keys [text role] :as message} max-w]
  (cached* [::bh (System/identityHashCode text) role (long max-w)]
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

(declare markdown->lines)

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

(defn- format-iteration-entry
  "Format one iteration's thinking + code + results + stdout into display lines.
   Each line is prefixed with an invisible marker so draw-chat-bubble! can
   apply per-line styles (italic thinking, dim code, etc.).
   `iteration-number` is the 1-based iteration number (for the header).

   Entry shape:
     {:thinking  str-or-nil
      :code      [str ...]        ;; code blocks
      :comments  [str-or-nil ...] ;; per-block leading `;; … / #_(...)` block, paint
                                  ;; in italic above the code; nil/blank renders nothing
      :results   [str ...]        ;; per-block result strings
      :stdouts   [str ...]        ;; per-block stdout
      :durations [int ...]        ;; per-block ms
      :successes [bool ...]       ;; per-block success? (nil = unknown / streaming)
      :error     {:type kw :message str :data {:raw-data str :received-type str}}}
                                  ;; iteration-level error (LLM call/spec failure).
                                  ;; Rendered in red so the user sees the actual
                                  ;; provider response (e.g. an HTTP plain-text
                                  ;; auth rejection) instead of just an empty bubble."
  [{:keys [thinking code comments results stdouts durations successes error repeat-count]}
   code-width iteration-number & [{:keys [show-header?] :or {show-header? true}}]]
  (let [;; All marker-prefixed lines must have visible content ≤ (dec code-width)
        ;; because wrap-text in draw-chat-bubble! wraps at code-width and the
        ;; invisible 1-char marker prefix counts toward string length.
        fill-w     (max 1 (dec code-width))
        ;; Right-aligned superscript label with 1 char right margin.
        ;; Setting-gated by `:show-iteration-headers` — when off, every
        ;; section label (ITERATION N, CODE N, STDOUT, ERROR) is
        ;; omitted along with the leading blank above thinking. The
        ;; iteration-bg + iteration-pad marker between blocks still
        ;; separates iterations visually; superscripts are opt-in chrome.
        label      (label-text "iteration" iteration-number)
        pad-len    (max 0 (- fill-w (count label) 1))
        header-line (str (repeat-str \space pad-len) label " ")
        header     (if show-header?
                     [(str iteration-hdr-marker header-line)]
                     [])

        thinking-lines
        (when (and (string? thinking) (not (str/blank? thinking)))
          ;; Render thinking via the markdown pipeline so the LLM's
          ;; reasoning gets headings, fenced code, bullets, tables,
          ;; blockquotes, and horizontal rules — same as the final
          ;; answer. Mode `:thinking` selects a parallel marker set
          ;; that paints with the dim iteration-header bg and italic style,
          ;; so the whole reasoning block still reads as one cohesive
          ;; "reasoning" region instead of clashing with the answer.
          (let [lines (markdown->lines (str/trim thinking) fill-w :thinking)
                lines (or (seq lines)
                        ;; Fallback: if md parser produced nothing
                        ;; (e.g. all-blank), keep the legacy plain-line
                        ;; behavior so the reasoning never disappears.
                        (mapv #(str thinking-marker %)
                          (wrap-text (str/trim thinking) fill-w)))]
            ;; Always emit a leading blank: gives thinking a one-row
            ;; top padding inside the dim thinking zone (the blank
            ;; row carries `thinking-marker` so it paints with the
            ;; iteration-bg + italic, not bubble bg). When the iteration
            ;; header is on, the blank reads as the natural gap between
            ;; header and reasoning; when off, it gives the italic
            ;; block a visible "breath" inside its own zone instead of
            ;; slamming the first character against the zone's top edge.
            ;; Trailing blank stays unconditional because thinking is
            ;; always followed by either code, error, or the answer
            ;; separator, and the gap reads as the natural section
            ;; break between thinking and what comes next.
            (vec (concat
                   [(str thinking-marker "")]
                   lines
                   [(str thinking-marker "")]))))

        ;; Iteration-level error block. Header + wrapper message + raw
        ;; provider response (when the spec layer captured one). Same
        ;; red styling as a failed code block so it reads as \"this iteration
        ;; ate it\" at a glance.
        ;;
        ;; `:repeat-count` (set by collapse-repeated-error-runs) marks
        ;; how many consecutive iterations produced this exact error.
        ;; When >1 the sub-header gets a \"× N\" badge — one block,
        ;; one count, no repeated multi-line spam.
        error-lines
        (when (map? error)
          (let [repeat-count (max 1 (long (or repeat-count 1)))
                badge        (when (> repeat-count 1) (str "  × " repeat-count))
                hdr-label    (str (label-text "error") (or badge ""))
                hdr-pad      (max 0 (- fill-w (count hdr-label) 1))
                hdr-line     (str iteration-hdr-marker (repeat-str \space hdr-pad) hdr-label " ")
                err-message      (or (:message error) (str (:type error)) "unknown error")
                raw              (some-> (get-in error [:data :raw-data]) str str/trim)
                recv             (get-in error [:data :received-type])
                err-message-wrapped (wrap-text (vis/format-error err-message) fill-w)
                err-message-rows    (mapv #(str err-result-marker %) err-message-wrapped)
                raw-rows     (when (and raw (not (str/blank? raw)))
                               (let [hdr (str "provider returned"
                                           (when recv (str " (" recv ")"))
                                           ":")
                                     raw-trim (if (> (count raw) 600) (str (subs raw 0 600) "…") raw)
                                     body-lines (mapv #(str err-result-marker %)
                                                  (wrap-text raw-trim fill-w))]
                                 (into [(str err-result-marker hdr)] body-lines)))]
            (vec (concat
                   ;; Top margin so the ERROR sub-header doesn't sit
                   ;; flush against the ITERATION header. Two padding
                   ;; lines guarantee at least one visible blank row
                   ;; between the headers regardless of how the
                   ;; surrounding bubble background fills in.
                   [(str iteration-pad-marker "")]
                   (when show-header? [(str iteration-pad-marker "")])
                   (when show-header? [hdr-line])
                   [(str code-err-pad-marker "")]
                   err-message-rows
                   (when (seq raw-rows) [(str code-err-pad-marker "")])
                   (or raw-rows [])
                   [(str code-err-pad-marker "")]))))

        code+result-lines
        (when (seq code)
          (into []
            (mapcat
              (fn [[idx form]]
                (let [success?    (when successes (get successes idx))
                      has-status? (some? success?)
                      is-error?   (and has-status? (not success?))
                      duration-ms  (when durations (get durations idx))
                      duration-str (vis/format-duration duration-ms)
                      ;; Right-aligned superscript code label with right padding
                      expr-label  (label-text "code" (inc idx))
                      expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]
                                    (str (repeat-str \space pl) expr-label " "))
                      ;; Right-aligned status line: ✓ 3ms or ✗ 3ms
                      status-text (when has-status?
                                    (str (if success? "✓" "✗")
                                      (when duration-str (str " " duration-str))))
                      status-line (when status-text
                                    (let [s-marker (if success? code-ok-marker code-err-marker)
                                          pl (max 0 (- fill-w (count status-text) 1))]
                                      (str s-marker (repeat-str \space pl) status-text " ")))
                      ;; Pick marker based on status
                      c-marker    (cond
                                    (not has-status?) code-marker
                                    success?          code-ok-marker
                                    :else             code-err-marker)
                      c-pad       (if is-error? code-err-pad-marker code-pad-marker)
                      ;; Per-block leading comment(s). Rendered ABOVE
                      ;; the code in the dim italic `thinking-marker`
                      ;; style so the model's authored prose reads
                      ;; naturally as a header for the block. Nil /
                      ;; blank — no rows emitted.
                      comment-text (some-> comments (get idx))
                      comment-lines
                      (when (and (string? comment-text)
                              (not (str/blank? comment-text)))
                        (let [trimmed (str/trim comment-text)
                              wrapped (mapcat (fn [line] (wrap-text line fill-w))
                                        (str/split-lines trimmed))]
                          (mapv #(str thinking-marker %) wrapped)))
                      ;; Code lines: pretty-printed. NO literal left
                      ;; padding — the bubble's `h-pad` already insets
                      ;; the whole zone by 2 cols, matching the
                      ;; user-question and final-answer text columns.
                      code-text   (str/trim (or form ""))
                      formatted   (vis/format-clojure code-text fill-w)
                      code-lines  (str/split-lines formatted)
                      c-lines     (mapv #(str c-marker %) code-lines)
                      ;; Result
                      result-str  (when results (get results idx))
                      r-marker    (if is-error? err-result-marker result-marker)
                      r-wrapped   (when (and result-str (not (str/blank? (str result-str))))
                                    (wrap-text (str/trim (str result-str)) fill-w))
                      r-lines     (when (seq r-wrapped)
                                    (into [(str r-marker (first r-wrapped))]
                                      (map #(str r-marker %) (rest r-wrapped))))
                      ;; Code block = header + (optional comment block) + pad-top + code + gap + result + status + pad-bottom
                      code-block  (vec (concat
                                         (when show-header? [(str iteration-hdr-marker expr-hdr)])  ;; right-aligned code ₁
                                         (when (seq comment-lines)
                                           (concat [(str thinking-marker "")] ;; top breath
                                             comment-lines
                                             [(str thinking-marker "")])) ;; bottom breath, separates comment from code
                                         [(str c-pad "")]        ;; pad top
                                         c-lines
                                         (when (seq r-lines) [(str c-pad "")])  ;; gap between code and result
                                         r-lines
                                         (when status-line [status-line])  ;; right-aligned ✓/✗
                                         [(str c-pad "")]))
                      ;; Stdout block with right-aligned header + padding
                      stdout-str  (when stdouts (get stdouts idx))
                      stdout-block (when (and stdout-str (not (str/blank? (str stdout-str))))
                                     (let [slabel    (label-text "stdout")
                                           slabel-pad (max 0 (- fill-w (count slabel) 1))
                                           slabel-ln  (str iteration-hdr-marker
                                                        (repeat-str \space slabel-pad)
                                                        slabel " ")
                                           text-lines (mapv #(str stdout-marker %)
                                                        (wrap-text (str/trim (str stdout-str))
                                                          fill-w))]
                                       (vec (concat
                                              (when show-header? [slabel-ln])               ;; right-aligned ˢᵗᵈᵒᵘᵗ
                                              [(str stdout-pad-marker "")] ;; pad top
                                              text-lines
                                              [(str stdout-pad-marker "")]))))  ;; pad bottom
                      ;; Iter-bg gap between code and stdout within the group
                      margin      (when stdout-block [(str iteration-pad-marker "")])]
                  (concat
                    ;; Spacer between consecutive code blocks (not before the first)
                    (when (pos? idx) [(str iteration-pad-marker "")])
                    code-block margin stdout-block))))
            (map-indexed vector code)))
        ;; Wrap all blocks in an iteration-bg group:
        ;; iteration-pad top + expression blocks + iteration-pad bottom
        grouped (when (seq code+result-lines)
                  (vec (concat
                         [(str iteration-pad-marker "")] ;; group top
                         code+result-lines
                         [(str iteration-pad-marker "")]))) ;; group bottom
        ]
    (into (vec (concat header thinking-lines error-lines)) grouped)))

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

(defn progress->text
  "Build the text body of the live progress placeholder bubble.

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
     :query-start-ms — wall-clock start, used for elapsed time
     :cancelling?    — true once Esc was pressed"
  ([progress bubble-w settings] (progress->text progress bubble-w settings nil))
  ([progress bubble-w settings extra]
   (let [iterations       (vec (:iterations progress))
         content-w        (max 10 (- bubble-w 4))
         show-thinking?   (get settings :show-thinking true)
         show-iterations? (get settings :show-iterations true)
         show-iteration-headers?  (get settings :show-iteration-headers false)
         {:keys [now-ms query-start-ms cancelling?]} extra
         now-ms           (long (or now-ms (System/currentTimeMillis)))
         elapsed-ms       (when query-start-ms
                            (max 0 (- now-ms (long query-start-ms))))
         elapsed-str      (or (vis/format-duration elapsed-ms) "0ms")
         spinner-line     (str (spinner-frame now-ms) "  "
                            (progress-phase iterations cancelling?) "…  "
                            elapsed-str "  ·  Esc to cancel")
         trace-lines      (when (and show-iterations? (seq iterations))
                            (into []
                              (mapcat (fn [[idx entry]]
                                        (format-iteration-entry
                                          (if show-thinking? entry (dissoc entry :thinking))
                                          content-w (inc idx)
                                          {:show-header? show-iteration-headers?})))
                              (collapse-repeated-error-runs iterations)))]
     (if (seq trace-lines)
       (str/join "\n" (conj (conj trace-lines "") spinner-line))
       spinner-line))))

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

(defn- markdown->lines
  "Parse markdown text into marker-prefixed lines for styled rendering.

   Supports:
     - `# / ## / ###` ATX headings (1–3) styled distinctly
     - `#### / ##### / ######` (H4–H6) collapsed onto the H3
       marker — terminal palettes can't visually distinguish six
       heading levels, and rendering raw `####` into the answer
       body is worse than re-using the deepest styled heading.
       Same convention as `glow` / `mdcat` / `bat`.
     - `**bold**` line
     - `⁠```` fenced code blocks (any info string)
     - `- ` / `* ` / `+ ` bullet lists
     - `> ` blockquote lines
     - `---` / `***` / `___` horizontal rule
     - GFM pipe tables `| a | b |` with `|---|---|` separator
     - inline `**bold**` / `` `code` `` stripped to plain text
       (terminal can't easily style mid-line spans)

   `max-w` is the max visible width per emitted line. Returns a vec
   of marker-prefixed lines. Returns `nil` for blank/nil input.

   `mode` selects the marker bundle: `:answer` (default) or `:thinking`.
   Thinking-mode lines all carry markers that the renderer paints with
   the dim iteration-header background and italic styling, so the
   whole reasoning block reads as one cohesive italicized region."
  ([text max-w] (markdown->lines text max-w :answer))
  ([text max-w mode]
   (when (and text (not (str/blank? text)))
     (let [m       (get md-marker-sets mode (md-marker-sets :answer))
           plain-m (:plain m)
           emit-plain (fn [s] (str plain-m s))
           input-lines (str/split-lines text)]
       (loop [lines  (seq input-lines)
              in-code? false
              acc    []]
         (if-not lines
           acc
           (let [line (first lines)
                 rst  (next lines)]
             (cond
               ;; Code fence toggle (start or end). Even if the model
               ;; never closes the fence we just keep painting code
               ;; bg until the section ends.
               (str/starts-with? (str/trim line) "```")
               (recur rst (not in-code?) (conj acc (str (:code m) "")))

               ;; Inside fenced code block — preserve verbatim, no md.
               ;; No literal left-padding inside the code line: the
               ;; bubble's `h-pad` (2 cols) is the ONLY left inset and
               ;; must match the user/answer text columns. Adding a
               ;; second `"  "` here used to push fenced code 2 cols
               ;; further in than the surrounding answer prose.
               in-code?
               (recur rst true
                 (into acc (mapv #(str (:code m) %)
                             (wrap-text line max-w))))

               ;; GFM pipe table: header + |---| + rows
               (and (table-row? line)
                 (table-separator-row? (first rst)))
               (let [[tbl tail] (consume-table lines max-w m)]
                 (recur (seq tail) false (into acc tbl)))

               ;; <details> / </details> framing tags. The TUI doesn't
               ;; (yet) model true collapsibility — there's no per-bubble
               ;; "is this section collapsed" state and no click region
               ;; on the summary line. Without that plumbing, leaking
               ;; raw `<details>` and `</details>` into the answer body
               ;; just shows the user HTML they can't act on. We drop
               ;; the framing tags here; the inner body (already
               ;; separated by blank lines in `md/details` output)
               ;; renders as normal markdown between them, and the
               ;; <summary> branch below paints the disclosure label.
               ;;
               ;; The regex tolerates attributes (`<details open>`)
               ;; and any leading whitespace. Tag must be the only
               ;; content on its line — inline `<details>` mid-prose
               ;; (rare) falls through to plain rendering.
               (re-matches #"^\s*</?details(\s[^>]*)?>\s*$" line)
               (recur rst false acc)

               ;; <summary>label</summary> on its own line — the
               ;; disclosure heading inside a <details> block.
               ;; Carries its own `:summary` marker (separate from
               ;; `:bold`) so the painter can paint it as a tinted
               ;; band: `md-summary-bg` lavender wash full-width,
               ;; `md-summary-fg` violet text, BOLD weight, prefixed
               ;; with `▾ ` so the user reads it as an expanded
               ;; disclosure section. The band makes the whole
               ;; <details> region pop out of the surrounding prose
               ;; without needing real click-to-collapse plumbing.
               ;;
               ;; Inner content runs through `markdown->inline` so
               ;; `<summary>**Logs**</summary>` keeps the inner bold,
               ;; ` `code` `, etc. — the painter strips inline
               ;; sentinels into per-char SGR overlays exactly the
               ;; way it does for real bold-line markers.
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

               ;; Heading 3 — and H4/H5/H6 collapsed onto the H3
               ;; marker. Terminal palettes top out at ~3 visually
               ;; distinct heading styles; matching `glow`/`mdcat`,
               ;; we render anything deeper than H3 as H3 rather
               ;; than leaking raw `####` into the body. The regex
               ;; allows 3-6 leading hashes and a single space.
               ;;
               ;; Heading body is run through `markdown->inline` so
               ;; `## **bold** title` keeps the inner bold instead of
               ;; rendering the asterisks literally.
               (re-matches #"^#{3,6} .*" line)
               (let [hash-count (count (re-find #"^#+" line))
                     body       (markdown->inline (subs line (inc hash-count)))]
                 (recur rst false
                   (into acc (mapv #(str (:h3 m) %)
                               (wrap-text body max-w)))))

               ;; Heading 2
               (str/starts-with? line "## ")
               (recur rst false
                 (into acc (mapv #(str (:h2 m) %)
                             (wrap-text (markdown->inline (subs line 3)) max-w))))

               ;; Heading 1
               (str/starts-with? line "# ")
               (recur rst false
                 (into acc (mapv #(str (:h1 m) %)
                             (wrap-text (markdown->inline (subs line 2)) max-w))))

               ;; Horizontal rule: --- / *** / ___
               (re-matches #"^\s*([-*_])\1{2,}\s*$" line)
               (recur rst false
                 (conj acc (str (:hr m) (repeat-str \─ max-w))))

               ;; Blockquote.
               ;;
               ;; Was the headline bug after PR #625 + emoji width fix
               ;; landed: `> **Lącznie:** ~10 MB w 16 pozycjach 🎉`
               ;; rendered with literal asterisks because the quote
               ;; branch never tokenised inline spans. Now goes through
               ;; `markdown->inline` like every other prose branch.
               (re-matches #"^\s*>\s?.*" line)
               (let [trimmed (str/replace line #"^\s*>\s?" "")
                     decorated (markdown->inline trimmed)
                     wrapped (wrap-text decorated (max 1 (- max-w 2)))]
                 (recur rst false
                   (into acc (mapv #(str (:quote m) "┃ " %) wrapped))))

               ;; Bullet list (- / * / +) — keep one trailing space.
               ;; Inline spans inside the bullet body get tokenised so
               ;; `- **bold** thing` actually bolds the word.
               (re-matches #"^\s*[-*+]\s+.*" line)
               (let [trimmed   (str/replace line #"^\s*[-*+]\s+" "")
                     decorated (markdown->inline trimmed)
                     wrapped   (wrap-text decorated (max 1 (- max-w 4)))]
                 (recur rst false
                   (into acc
                     (into [(str (:bullet m) "  • " (first wrapped))]
                       (mapv #(str (:bullet m) "    " %) (rest wrapped))))))

               ;; Numbered list (1. 2.) — same inline tokenisation.
               (re-matches #"^\s*\d+[.)]\s+.*" line)
               (let [[_ num body] (re-matches #"^\s*(\d+)[.)]\s+(.*)$" line)
                     decorated (markdown->inline (or body ""))
                     prefix    (str num ". ")
                     wrapped   (wrap-text decorated (max 1 (- max-w (count prefix) 2)))]
                 (recur rst false
                   (into acc
                     (into [(str (:bullet m) "  " prefix (first wrapped))]
                       (mapv #(str (:bullet m) "    " (repeat-str \space (count prefix)) %)
                         (rest wrapped))))))

               ;; Bold-only line: **text**.
               ;; Inner content is run through `markdown->inline` in
               ;; case it carries inline `code` / `*italic*` etc.
               (and (str/starts-with? (str/trim line) "**")
                 (str/ends-with? (str/trim line) "**")
                 (> (count (str/trim line)) 4))
               (let [trimmed (str/trim line)
                     inner   (markdown->inline (subs trimmed 2 (- (count trimmed) 2)))]
                 (recur rst false
                   (into acc (mapv #(str (:bold m) %)
                               (wrap-text inner max-w)))))

               ;; Blank line
               (str/blank? line)
               (recur rst false (conj acc (emit-plain "")))

               ;; Plain text — tokenize inline **bold** / *italic* /
               ;; ~~strike~~ / `code` into invisible sentinel pairs so
               ;; the per-line painter can switch SGR mid-line. The
               ;; sentinels are zero-width per `display-width`, so
               ;; wrap-text wraps by visible columns and the spans
               ;; round-trip cleanly through the wrap.
               :else
               (let [decorated (markdown->inline line)]
                 (recur rst false
                   (into acc (mapv emit-plain (wrap-text decorated max-w)))))))))))))

(defn format-answer-with-thinking*
  "Uncached implementation. See `format-answer-with-thinking`.
   `cancelled?` switches the trailer from a real answer block to a
   plain status footer (\"Cancelled by user.\") rendered without
   answer-pad markers, so the footer reads as a system note on bare
   terminal-bg rather than a half-baked answer in the answer zone."
  [answer trace bubble-w settings confidence cancelled?]
  (let [content-w (max 10 (- bubble-w 4))
        fill-w    (max 1 (dec content-w))
        show-thinking?     (get settings :show-thinking true)
        show-iterations?   (get settings :show-iterations true)
        show-iteration-headers?    (get settings :show-iteration-headers false)
        show-final-hdr?    (get settings :show-final-answer-header false)
        trace-lines (when (and show-iterations? (seq trace))
                      (into []
                        (mapcat (fn [[idx entry]]
                                  (format-iteration-entry
                                    (if show-thinking? entry (dissoc entry :thinking))
                                    content-w (inc idx)
                                    {:show-header? show-iteration-headers?})))
                        (collapse-repeated-error-runs trace)))
        answer-str  (or answer "")
        ;; Right-aligned "FINAL ANSWER" header with confidence —
        ;; opt-in chrome. The blue answer-bg + bold horizontal rule
        ;; (`ans-sep`) above the answer body already shout \"this is
        ;; the answer\"; the superscript is redundant unless the
        ;; user explicitly turns it back on. Cancelled bubbles never
        ;; get the FINAL ANSWER chrome — there is no final answer.
        fa-label    (label-text "final answer")
        conf-str    (when confidence (str " · " (name confidence)))
        full-label  (str fa-label (or conf-str ""))
        fa-pad      (max 0 (- fill-w (count full-label) 1))
        fa-hdr      (when (and show-final-hdr? (not cancelled?))
                      (str answer-hdr-marker (repeat-str \space fa-pad) full-label " "))
        ;; The answer-bg paint is anchored on the FIRST line carrying
        ;; an `answer-*` marker (`draw-chat-bubble!` searches for
        ;; `answer-hdr-marker` first; if absent, falls back to
        ;; `answer-pad-marker` / `answer-txt-marker`). Without the
        ;; explicit fa-hdr we just rely on the leading `ans-pad` to
        ;; anchor the answer zone — it carries `answer-pad-marker`
        ;; which the renderer treats as part of the answer block.
        md-lines    (markdown->lines answer-str (max 1 (- fill-w 2)))
        ans-lines   (if (seq md-lines)
                      md-lines
                      (wrap-text answer-str (max 1 (- fill-w 2))))
        ans-pad     (str answer-pad-marker "")
        ans-sep     (str answer-sep-marker "")
        ;; Cancelled trailer: plain lines (no answer-pad markers)
        ;; so the status text renders flat on terminal-bg with no
        ;; answer-zone fill. The wrapped status string \"Cancelled by
        ;; user.\" is short enough to fit on one row at any sane
        ;; bubble width, but we wrap-text defensively in case the
        ;; column gets squeezed below ~20 chars.
        cancel-text   (or answer "Cancelled by user.")
        cancel-rows   (wrap-text cancel-text (max 1 (- fill-w 2)))
        cancel-block  (vec (concat [ans-sep ""] cancel-rows [""]))
        answer-block (cond-> [ans-sep]
                       fa-hdr (conj fa-hdr)
                       :always (conj ans-pad)
                       :always (into ans-lines)
                       :always (conj ans-pad))
        trailer       (if cancelled? cancel-block answer-block)]
    (if (seq trace-lines)
      (str/join "\n" (concat trace-lines trailer))
      ;; No trace — skip the trace/trailer separator at the top of an
      ;; otherwise-empty bubble; jump straight to the trailer body
      ;; (drop the leading separator marker).
      (str/join "\n" (rest trailer)))))

(defn format-answer-with-thinking
  "Build the final bubble text: thinking trace + answer. Memoized by
   `(answer-identity, trace-identity, bubble-w, settings, confidence,
   cancelled?)`.

   Finalized assistant messages never mutate after `:message-received`,
   so the same key tuple comes in on every subsequent frame and we
   short-circuit straight to the cached string. This is the single
   biggest CPU win for long conversations."
  ([answer trace bubble-w] (format-answer-with-thinking answer trace bubble-w nil nil false))
  ([answer trace bubble-w settings] (format-answer-with-thinking answer trace bubble-w settings nil false))
  ([answer trace bubble-w settings confidence] (format-answer-with-thinking answer trace bubble-w settings confidence false))
  ([answer trace bubble-w settings confidence cancelled?]
   (cached* [::fawt
             (System/identityHashCode answer)
             (System/identityHashCode trace)
             (long bubble-w)
             (boolean (get settings :show-thinking true))
             (boolean (get settings :show-iterations true))
             confidence
             (boolean cancelled?)]
     #(format-answer-with-thinking* answer trace bubble-w settings confidence cancelled?))))

(defn format-answer-markdown*
  "Uncached implementation. See `format-answer-markdown`."
  [answer bubble-w]
  (let [content-w (max 10 (- bubble-w 4))
        md-lines  (markdown->lines (or answer "") content-w)]
    (if (seq md-lines)
      (str/join "\n" md-lines)
      (or answer ""))))

(defn format-answer-markdown
  "Memoized `format-answer-markdown*`."
  [answer bubble-w]
  (cached* [::fam (System/identityHashCode answer) (long bubble-w)]
    #(format-answer-markdown* answer bubble-w)))

;;; ── Messages area (bubble-based) ───────────────────────────────────────────

;; -- Messages-area layout constants ----------------------------------------
;;
;; PUBLIC because `screen.clj` must compute bubble width with the same
;; gutter math the painter uses; if the two layers disagree by even one
;; column, `format-iteration-entry` sizes labels (`CODE 3`, `✓ 3ms`,
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
   Returns `nil` when there's no overflow — no thumb is drawn, no
   click should hit-test as on-thumb."
  [^long total-h ^long inner-h scroll]
  (when (and (pos? inner-h) (> total-h inner-h))
    (let [track-h    inner-h
          max-scroll (max 1 (- total-h inner-h))
          eff-scroll (let [s (long (or scroll max-scroll))]
                       (max 0 (min s max-scroll)))
          thumb-h    (max 1 (long (* track-h (/ (double inner-h) total-h))))
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
