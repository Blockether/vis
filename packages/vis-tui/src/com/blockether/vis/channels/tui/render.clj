(ns com.blockether.vis.channels.tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.primitives :as p]
            [com.blockether.vis.channels.tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize Symbols]
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

(def ^:private ^:const fmt-cache-cap 512)

(defn- make-fmt-cache ^LinkedHashMap []
  (proxy [LinkedHashMap] [64 0.75 true] ;; true = access-order (LRU)
    (removeEldestEntry [_eldest]
      (> (.size ^LinkedHashMap this) fmt-cache-cap))))

(defonce ^:private ^LinkedHashMap fmt-cache (make-fmt-cache))

(defn- cached* [k compute-fn]
  ;; LinkedHashMap is not thread-safe; the render thread is the only
  ;; caller after the Layer-C refactor but the sentinel + lock keeps
  ;; us safe if anyone ever invokes a formatter from elsewhere
  ;; (tests, REPL exploration, future channels).
  (locking fmt-cache
    (let [hit (.get fmt-cache k)]
      (if (some? hit)
        hit
        (let [v (compute-fn)]
          (.put fmt-cache k v)
          v)))))

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
            (if (<= (count line) max-width)
              [line]
              (loop [remaining line
                     acc       []]
                (if (<= (count remaining) max-width)
                  (conj acc remaining)
                     ;; Find last space within max-width
                  (let [chunk   (subs remaining 0 max-width)
                        last-sp (str/last-index-of chunk " ")]
                    (if (and last-sp (pos? last-sp))
                         ;; Break at word boundary
                      (recur (subs remaining (inc (int last-sp)))
                        (conj acc (subs remaining 0 (int last-sp))))
                         ;; No space found — hard break
                      (recur (subs remaining max-width)
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

(defn- draw-box-border!
  "Draw a single-line box border. Optionally embeds a centered hint
   string on the top edge (typically the keybinding strip).

   Live status (model, ctx %, run-state) used to live on the bottom
   edge via a second hint argument; that path was deleted when the
   dedicated footer row took over (see
   `com.blockether.vis.channels.tui.footer`). The bottom edge is now
   always a plain horizontal rule."
  [g box-top box-bottom cols top-hint]
  (let [inner-w (- cols 2)
        bar     (repeat-str Symbols/SINGLE_LINE_HORIZONTAL inner-w)]
    (.setForegroundColor g t/border-fg)
    (.setBackgroundColor g t/terminal-bg)

    ;; Top: ┌── top-hint ──┐
    (.setCharacter g 0 box-top Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
    (.putString g 1 box-top (embed-in-bar bar top-hint))
    (.setCharacter g (dec cols) box-top Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)

    ;; Bottom: └──────┘ (plain rule — status moved to footer row).
    (.setCharacter g 0 box-bottom Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
    (.putString g 1 box-bottom bar)
    (.setCharacter g (dec cols) box-bottom Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)

    ;; Sides: │ ... │
    (doseq [row (range (inc box-top) box-bottom)]
      (.setForegroundColor g t/border-fg)
      (.setBackgroundColor g t/terminal-bg)
      (.setCharacter g 0 row Symbols/SINGLE_LINE_VERTICAL)
      (.setCharacter g (dec cols) row Symbols/SINGLE_LINE_VERTICAL))))

(defn- fill-box-interior!
  "Fill the interior of a box with the standard box background."
  [g box-top box-bottom cols]
  (let [inner-w  (- cols 2)
        text-top (inc box-top)
        rows     (- box-bottom box-top 1)]
    (.setForegroundColor g t/box-fg)
    (.setBackgroundColor g t/box-bg)
    (.fillRectangle g (TerminalPosition. 1 text-top) (TerminalSize. inner-w rows) \space)))

;;; ── Messages box ───────────────────────────────────────────────────────────

(defn draw-messages-box!
  "Draw bordered message area with top-anchored scrollable messages."
  [g messages box-top box-bottom cols scroll]
  (let [inner-rows (- box-bottom box-top 1)
        text-top   (inc box-top)
        text-w     (- cols 4)
        total      (count messages)
        offset     (min scroll (max 0 (- total inner-rows)))
        visible    (subvec messages offset (min total (+ offset inner-rows)))]

    (draw-box-border! g box-top box-bottom cols "")
    (fill-box-interior! g box-top box-bottom cols)

    (doseq [[i msg] (map-indexed vector visible)]
      (.setForegroundColor g t/box-fg)
      (.setBackgroundColor g t/box-bg)
      (.putString g (inc t/pad-x) (+ text-top i)
        (subs msg 0 (min (count msg) text-w))))))

;;; ── Input box ──────────────────────────────────────────────────────────────

(def input-pad-y 1)  ;; internal vertical padding (rows above/below text)
(def ^:private input-pad-x 2)  ;; internal horizontal padding (cols left/right of text)

(defn draw-input-box!
  "Draw bordered input area with internal padding. Returns
   [cursor-col cursor-row] in screen coords.

   `hint` is the keybinding strip embedded in the TOP border. The
   bottom border is always a plain horizontal rule — live status
   (model / run-state / ctx %) lives in the dedicated footer row
   below this box (see `footer/draw-footer!`)."
  [g {:keys [lines crow ccol]} box-top text-rows cols hint]
  (let [box-bottom (+ box-top (* 2 input-pad-y) text-rows 1)
        text-top   (+ (inc box-top) input-pad-y)
        text-w     (- cols 2 (* 2 input-pad-x))
        v-scroll   (max 0 (- crow (dec text-rows)))
        h-scroll   (max 0 (- ccol (dec text-w)))]
    (draw-box-border! g box-top box-bottom cols hint)
    (fill-box-interior! g box-top box-bottom cols)

    ;; Text
    (dotimes [i text-rows]
      (let [li (+ v-scroll i)]
        (when (< li (count lines))
          (let [line   (nth lines li)
                offset (if (= li crow) h-scroll 0)
                len    (count line)]
            (when (< offset len)
              (.setForegroundColor g t/box-fg)
              (.setBackgroundColor g t/box-bg)
              (.putString g input-pad-x (+ text-top i)
                (subs line offset (min len (+ offset text-w)))))))))

    ;; Cursor position
    [(+ input-pad-x (- ccol h-scroll))
     (+ text-top (- crow v-scroll))]))

;;; ── Background fill ────────────────────────────────────────────────────────

(defn fill-background!
  "Fill entire screen with the terminal background color."
  [g cols rows]
  (.setBackgroundColor g t/terminal-bg)
  (.setForegroundColor g t/text-fg)
  (.fillRectangle g (TerminalPosition. 0 0) (TerminalSize. cols rows) \space))

;;; ── Dialog ─────────────────────────────────────────────────────────────────

(defn draw-dialog!
  "Draw a centered confirmation dialog with text wrapping.
   `body` can be a string or vec of strings. Long lines are wrapped to fit.
   Optional `max-w` limits dialog width (defaults to 60% of screen)."
  ([g cols rows title body] (draw-dialog! g cols rows title body nil))
  ([g cols rows title body max-w]
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
     (.setCharacter g box-left  box-top    Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
     (.setCharacter g box-right box-top    Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)
     (.setCharacter g box-left  box-bottom Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
     (.setCharacter g box-right box-bottom Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)
     (.putString g (inc box-left) box-top h-bar)
     (.putString g (inc box-left) box-bottom h-bar)
     (doseq [r (range (inc box-top) box-bottom)]
       (.setCharacter g box-left  r Symbols/SINGLE_LINE_VERTICAL)
       (.setCharacter g box-right r Symbols/SINGLE_LINE_VERTICAL))

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
(def ^:private iter-hdr-marker  p/MARKER_ITER_HDR)
(def ^:private stdout-sep-marker p/MARKER_STDOUT_SEP)
(def ^:private stdout-pad-marker p/MARKER_STDOUT_PAD)
(def ^:private answer-sep-marker p/MARKER_ANSWER_SEP)
(def ^:private code-pad-marker   p/MARKER_CODE_PAD)
(def ^:private code-err-pad-marker p/MARKER_CODE_ERR_PAD)
(def ^:private iter-pad-marker   p/MARKER_ITER_PAD)
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

(def ^:private md-marker-sets
  "Per-mode marker bundle. Selected by `markdown->lines` mode arg.
   `:answer` lines render with answer-bg (no italic). `:thinking`
   lines render with iter-header-bg + italic so the whole reasoning
   block reads as one dim, italicized region."
  {:answer   {:h1 md-h1-marker :h2 md-h2-marker :h3 md-h3-marker
              :bold md-bold-marker :code md-code-marker
              :bullet md-bullet-marker :quote md-quote-marker :hr md-hr-marker
              :thead md-table-head-marker :tsep md-table-sep-marker :trow md-table-row-marker
              :plain ""}
   :thinking {:h1 th-md-h1-marker :h2 th-md-h2-marker :h3 th-md-h3-marker
              :bold th-md-bold-marker :code th-md-code-marker
              :bullet th-md-bullet-marker :quote th-md-quote-marker :hr th-md-hr-marker
              :thead th-md-table-head-marker :tsep th-md-table-sep-marker :trow th-md-table-row-marker
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
  [g x y line text-fg border-fg bg body-styles]
  (p/clear-styles! g)
  (p/set-colors! g border-fg bg)
  (p/put-str! g x y line)
  (p/clear-styles! g)
  (p/set-colors! g text-fg bg)
  (let [n (count line)
        paint-seg! (fn [start end]
                     (when (and start (< start end))
                       (let [seg (subs line start end)]
                         (when (pos? (count seg))
                           (if (seq body-styles)
                             (p/styled g body-styles
                               (p/put-str! g (+ x start) y seg))
                             (p/put-str! g (+ x start) y seg))))))]
    (loop [i 0 seg-start nil]
      (cond
        (= i n)               (paint-seg! seg-start i)
        (= (.charAt line i) \│) (do (paint-seg! seg-start i)
                                  (recur (inc i) nil))
        :else                  (recur (inc i) (or seg-start i))))))

(defn draw-chat-bubble!
  "Draw a chat message at the given row. No border, no bubble container.
   `msg` is a map: {:role :user|:assistant, :text str, :timestamp #inst}
   Optional `:duration-ms` for assistant response time.
   `left` and `max-w` define the horizontal bounds.

   Layout (no outer border, no horizontal rule under the label,
   both roles left-anchored):
     [role-label]                              [timestamp]
     [blank breathing row]
     [content lines, each with role bg fill]
     [meta line, dimmed (skipped for cancelled status)]
     [blank gap]

   User content rows get a subtle blue-gray background block
   (`user-bubble-bg`) to visually separate user input from the rest of
   the conversation. Assistant content rows render on terminal bg —
   the only fills come from inline marker zones (code blocks,
   answer-bg, stdout, etc.).

   Returns the number of screen rows consumed (including spacing)."
  [g {:keys [role text timestamp duration-ms model iterations tokens cost status]} start-row left max-w]
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
        ;; No bg fill on plain user / assistant text — we sit on
        ;; terminal-bg. Only `:warning` messages still get a tinted
        ;; block so they're impossible to miss in the timeline.
        bg-color  (cond
                    warning? t/warning-bg
                    ;; User messages fill their content rows with a
                    ;; very pale warm-yellow block so "you said this"
                    ;; reads as its own zone, distinct from the white
                    ;; assistant area. Cancelled / warning paths
                    ;; already had their own bg; user is the third
                    ;; flavor.
                    user?    t/user-bubble-bg
                    :else    t/terminal-bg)
        fg-color  (cond
                    cancelled? t/dialog-hint
                    warning?   t/warning-fg
                    user?      t/user-bubble-fg
                    :else      t/ai-bubble-fg)
        role-fg   (cond
                    cancelled? t/dialog-hint
                    user?      t/user-role-fg
                    :else      t/ai-role-fg)
        time-str  (channels/format-date timestamp)
        dur-str   (channels/format-duration duration-ms)
        tok-in    (when-let [n (:input tokens)] (str "↑" n))
        tok-out   (when-let [n (:output tokens)] (str "↓" n))
        iter-str  (when (and (not user?) iterations (pos? iterations)) (str iterations (if (= 1 iterations) " iter" " iters")))
        cost-str  (when-let [c (some-> cost :total-cost)]
                    (str "~$" (String/format java.util.Locale/US "%.6f" (into-array Object [(double c)]))))
        ;; Below-message meta (assistant only): model · iters · ctx-in · ctx-out · ~cost · duration.
        ;; Cancelled turns skip this entirely — there's no \"answer\"
        ;; to attribute, and showing 0 iters / no model under a
        ;; \"Cancelled\" placeholder is just clutter.
        meta-parts (when (and (not user?) (not cancelled?))
                     (remove nil? [model iter-str tok-in tok-out cost-str dur-str]))
        meta-str   (when (seq meta-parts) (str/join " · " meta-parts))]

    ;; Row 0: label (bold, role-colored) + timestamp (dim, right-aligned).
    (p/clear-styles! g)
    (p/set-colors! g role-fg t/terminal-bg)
    (p/styled g [p/BOLD]
      (p/put-str! g bx start-row label))
    (when time-str
      (p/set-colors! g t/dialog-hint t/terminal-bg)
      (p/put-str! g (+ bx (max (count label) (- bubble-w (count time-str)))) start-row time-str))

    ;; Row 1: blank breathing row — the only separator between the
    ;; role banner and the content. We used to draw a full-width
    ;; horizontal rule here; whitespace alone reads cleanly enough
    ;; without the visual noise of a hard divider on every message.
    ;; Content begins on row 2 (= start-row + 2).
    (let [btop (+ start-row 2)]
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
      ;; User messages: fill content rows PLUS one row above (the
      ;; blank breathing row between label and content) and one row
      ;; below (the would-be meta-row, always empty for user msgs
      ;; per `meta-parts (when (and (not user?) ...) ...)`). The two
      ;; extra rows give the yellow block visible vertical padding
      ;; — the bubble reads as a proper highlighted zone instead of
      ;; a single-row band hugging the text. Total bubble height is
      ;; unchanged (those rows were already allocated to label-gap
      ;; and meta-row); we're just painting bg into them.
      (when user?
        (p/set-bg! g bg-color)
        (p/fill-rect! g bx (dec btop) bubble-w (+ (max 1 bubble-h) 2)))

      ;; Text content — per-line styling via invisible marker prefixes
      ;;
      ;; After the answer-hdr marker, all remaining lines get answer-bg
      ;; as their base fill — this includes plain text and markdown lines.
      (let [iter-bg      t/iter-header-bg
            answer-start (or (some (fn [[i l]] (when (str/starts-with? l answer-hdr-marker) i))
                               (map-indexed vector lines))
                           (count lines))]
        (doseq [[i line] (map-indexed vector lines)]
          (p/clear-styles! g)
          (let [in-answer? (> i answer-start)
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
              (str/starts-with? line iter-hdr-marker)
              (do (p/set-colors! g t/iter-header-fg bg-color)
                (p/put-str! g x y (subs line 1)))

              ;; ── Thinking — dimmed bg, italic ──
              (str/starts-with? line thinking-marker)
              (do (p/set-colors! g t/dialog-hint t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

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
              (do (p/set-colors! g t/code-duration-fg iter-bg)
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
              (str/starts-with? line iter-pad-marker)
              (do (p/set-bg! g bg-color)
                (p/fill-rect! g fbx y iw 1))

              ;; ── Answer separator — bold horizontal rule between iterations and answer ──
              (str/starts-with? line answer-sep-marker)
              (do (p/set-colors! g t/answer-sep-fg bg-color)
                (p/styled g [p/BOLD]
                  (p/put-str! g fbx y (repeat-str \u2500 iw))))

              ;; ── Answer header — right-aligned superscript on bubble bg ──
              (str/starts-with? line answer-hdr-marker)
              (do (p/set-colors! g t/iter-header-fg bg-color)
                (p/put-str! g x y (subs line 1)))

              ;; ── Answer-mode markdown headings ──
              (str/starts-with? line md-h1-marker)
              (let [lbg (if in-answer? t/answer-bg bg-color)]
                (p/set-colors! g fg-color lbg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line md-h2-marker)
              (let [lbg (if in-answer? t/answer-bg bg-color)]
                (p/set-colors! g fg-color lbg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line md-h3-marker)
              (let [lbg (if in-answer? t/answer-bg bg-color)]
                (p/set-colors! g t/dialog-hint lbg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line md-bold-marker)
              (let [lbg (if in-answer? t/answer-bg bg-color)]
                (p/set-colors! g fg-color lbg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line md-code-marker)
              (do (p/set-colors! g t/code-block-fg t/code-block-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/put-str! g x y (subs line 1)))

              (str/starts-with? line md-bullet-marker)
              (let [lbg (if in-answer? t/answer-bg bg-color)]
                (p/set-colors! g fg-color lbg)
                (p/fill-rect! g fbx y iw 1)
                (p/put-str! g x y (subs line 1)))

              (str/starts-with? line md-quote-marker)
              (let [lbg (if in-answer? t/answer-bg bg-color)]
                (p/set-colors! g t/dialog-hint lbg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line md-hr-marker)
              (let [lbg (if in-answer? t/answer-bg bg-color)]
                (p/set-colors! g t/answer-sep-fg lbg)
                (p/fill-rect! g fbx y iw 1)
                (p/put-str! g x y (subs line 1)))

              ;; ── Markdown table (answer) ── grid on code-block bg
              ;; Chrome (│┌─┐├┼┤└┴┘─) stays in muted `code-border-fg`,
              ;; cell text in dark `code-block-fg`, headers bold.
              (or (str/starts-with? line md-table-head-marker)
                (str/starts-with? line md-table-sep-marker)
                (str/starts-with? line md-table-row-marker))
              (let [stripped (subs line 1)
                    head?    (str/starts-with? line md-table-head-marker)
                    border?  (str/starts-with? line md-table-sep-marker)
                    tbg      t/code-block-bg]
                (p/clear-styles! g)
                (p/set-colors! g t/code-border-fg tbg)
                (p/fill-rect! g fbx y iw 1)
                (if border?
                  ;; Pure box-drawing line — single muted paint.
                  (p/put-str! g x y stripped)
                  ;; Header / body data row — dual-color split.
                  (paint-table-data-line! g x y stripped
                    t/code-block-fg t/code-border-fg tbg
                    (when head? [p/BOLD]))))

              ;; ── Thinking-mode markdown headings ── dim italic on iter bg
              (str/starts-with? line th-md-h1-marker)
              (do (p/set-colors! g t/iter-header-fg t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line th-md-h2-marker)
              (do (p/set-colors! g t/iter-header-fg t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line th-md-h3-marker)
              (do (p/set-colors! g t/dialog-hint t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line th-md-bold-marker)
              (do (p/set-colors! g t/dialog-hint t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/BOLD p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              ;; Thinking fenced code: visible code-block bg, italic dim text.
              (str/starts-with? line th-md-code-marker)
              (do (p/set-colors! g t/code-result-fg t/code-block-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line th-md-bullet-marker)
              (do (p/set-colors! g t/dialog-hint t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line th-md-quote-marker)
              (do (p/set-colors! g t/dialog-hint t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/styled g [p/ITALIC]
                  (p/put-str! g x y (subs line 1))))

              (str/starts-with? line th-md-hr-marker)
              (do (p/set-colors! g t/answer-sep-fg t/iter-header-bg)
                (p/fill-rect! g fbx y iw 1)
                (p/put-str! g x y (subs line 1)))

              ;; ── Markdown table (thinking) ── grid on code-block bg, italic body
              ;; Same dual-color treatment as the answer-mode table,
              ;; with italic on text segments to match the rest of
              ;; the thinking zone (every thinking-mode marker uses
              ;; italic; staying consistent keeps the zone readable
              ;; as one cohesive dim region).
              (or (str/starts-with? line th-md-table-head-marker)
                (str/starts-with? line th-md-table-sep-marker)
                (str/starts-with? line th-md-table-row-marker))
              (let [stripped (subs line 1)
                    head?    (str/starts-with? line th-md-table-head-marker)
                    border?  (str/starts-with? line th-md-table-sep-marker)
                    tbg      t/code-block-bg]
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
              (let [line-bg (if in-answer? t/answer-bg bg-color)
                    line-fg (if in-answer? t/answer-fg fg-color)]
                (when in-answer?
                  (p/set-bg! g line-bg)
                  (p/fill-rect! g fbx y iw 1))
                (p/set-colors! g line-fg line-bg)
                (if cancelled?
                  (p/styled g [p/ITALIC] (p/put-str! g x y line))
                  (p/put-str! g x y line)))))))

      ;; Below-content meta row.
      ;;
      ;; Final per-message layout (no outer box, no bg fill, no
      ;; horizontal rule under the label):
      ;;   row 0          : label + timestamp
      ;;   row 1          : blank breathing row
      ;;   row 2 … 1+N    : wrapped content (with marker-zone fills)
      ;;   row 2+N        : meta (right-aligned, dim) — skipped for cancelled
      ;;   row 3+N        : single blank gap before the next message
      (p/clear-styles! g)
      (let [meta-row (+ btop bubble-h)]
        (when meta-str
          (p/set-colors! g t/dialog-hint t/terminal-bg)
          (p/put-str! g (+ bx (max 0 (- bubble-w (count meta-str)))) meta-row meta-str))
        ;; Return: rows consumed
        ;;   = label(1) + breathing-gap(1) + content(N) + meta(1) + gap(1)
        (+ 1 1 bubble-h 1 1)))))

(defn bubble-height*
  "Uncached calculation: rows a chat message will consume without drawing.
   label(1) + breathing-gap(1) + wrapped-lines + meta(1) + gap(1).
   Mirrors `draw-chat-bubble!`'s wrap width (`bubble-w - 2*h-pad`) so
   layout math stays consistent across the height calc and the draw.
   The horizontal rule under the role label was deleted in favor of
   a single blank row — fewer pixels of chrome, same readable
   separation between banner and content."
  [{:keys [text]} max-w]
  (let [bubble-w  max-w
        h-pad     2
        content-w (max 1 (- bubble-w (* 2 h-pad)))
        lines     (wrap-text text content-w)]
    (+ 1 1 (count lines) 1 1)))

(defn bubble-height
  "Memoized `bubble-height*`. Keyed by `:text` identity + width — same
   string instance + width = same height, no need to re-wrap."
  [{:keys [text] :as msg} max-w]
  (cached* [::bh (System/identityHashCode text) (long max-w)]
    #(bubble-height* msg max-w)))

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
   `iter-num` is the 1-based iteration number (for the header).

   Entry shape:
     {:thinking  str-or-nil
      :code      [str ...]        ;; code expressions
      :results   [str ...]        ;; per-expression result strings
      :stdouts   [str ...]        ;; per-expression stdout
      :durations [int ...]        ;; per-expression ms
      :successes [bool ...]      ;; per-expression success? (nil = unknown / streaming)
      :error     {:type kw :message str :data {:raw-data str :received-type str}}}
                                  ;; iteration-level error (LLM call/spec failure).
                                  ;; Rendered in red so the user sees the actual
                                  ;; provider response (e.g. an HTTP plain-text
                                  ;; auth rejection) instead of just an empty bubble."
  [{:keys [thinking code results stdouts durations successes error repeat-count]} code-width iter-num]
  (let [;; All marker-prefixed lines must have visible content ≤ (dec code-width)
        ;; because wrap-text in draw-chat-bubble! wraps at code-width and the
        ;; invisible 1-char marker prefix counts toward string length.
        fill-w     (max 1 (dec code-width))
        ;; Right-aligned superscript label with 1 char right margin
        label      (label-text "iteration" iter-num)
        pad-len    (max 0 (- fill-w (count label) 1))
        header-line (str (repeat-str \space pad-len) label " ")
        header [(str iter-hdr-marker header-line)]

        thinking-lines
        (when (and (string? thinking) (not (str/blank? thinking)))
          ;; Render thinking via the markdown pipeline so the LLM's
          ;; reasoning gets headings, fenced code, bullets, tables,
          ;; blockquotes, and horizontal rules — same as the final
          ;; answer. Mode `:thinking` selects a parallel marker set
          ;; that paints with the dim iter-header bg and italic style,
          ;; so the whole reasoning block still reads as one cohesive
          ;; "reasoning" region instead of clashing with the answer.
          (let [lines (markdown->lines (str/trim thinking) fill-w :thinking)
                lines (or (seq lines)
                        ;; Fallback: if md parser produced nothing
                        ;; (e.g. all-blank), keep the legacy plain-line
                        ;; behavior so the reasoning never disappears.
                        (mapv #(str thinking-marker %)
                          (wrap-text (str/trim thinking) fill-w)))]
            (vec (concat [(str thinking-marker "")] lines [(str thinking-marker "")]))))

        ;; Iteration-level error block. Header + wrapper message + raw
        ;; provider response (when the spec layer captured one). Same
        ;; red styling as a failed code block so it reads as \"this iter
        ;; ate it\" at a glance.
        ;;
        ;; `:repeat-count` (set by collapse-repeated-error-runs) marks
        ;; how many consecutive iterations produced this exact error.
        ;; When >1 the sub-header gets a \"× N\" badge so we don't repeat
        ;; the same multi-line block over and over.
        error-lines
        (when (map? error)
          (let [repeat-count (max 1 (long (or repeat-count 1)))
                badge        (when (> repeat-count 1) (str "  × " repeat-count))
                hdr-label    (str (label-text "error") (or badge ""))
                hdr-pad      (max 0 (- fill-w (count hdr-label) 1))
                hdr-line     (str iter-hdr-marker (repeat-str \space hdr-pad) hdr-label " ")
                msg          (or (:message error) (str (:type error)) "unknown error")
                raw          (some-> (get-in error [:data :raw-data]) str str/trim)
                recv         (get-in error [:data :received-type])
                msg-wrapped  (wrap-text (str "✘ " msg) (max 1 (- fill-w 2)))
                msg-rows     (mapv #(str err-result-marker "  " %) msg-wrapped)
                raw-rows     (when (and raw (not (str/blank? raw)))
                               (let [hdr (str "provider returned"
                                           (when recv (str " (" recv ")"))
                                           ":")
                                     raw-trim (if (> (count raw) 600) (str (subs raw 0 600) "…") raw)
                                     body-lines (mapv #(str err-result-marker "  " %)
                                                  (wrap-text raw-trim (max 1 (- fill-w 2))))]
                                 (into [(str err-result-marker "  " hdr)] body-lines)))]
            (vec (concat
                   ;; Top margin so the ERROR sub-header doesn't sit
                   ;; flush against the ITERATION header. Two padding
                   ;; lines guarantee at least one visible blank row
                   ;; between the headers regardless of how the
                   ;; surrounding bubble background fills in.
                   [(str iter-pad-marker "")]
                   [(str iter-pad-marker "")]
                   [hdr-line]
                   [(str code-err-pad-marker "")]
                   msg-rows
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
                      dur-ms      (when durations (get durations idx))
                      dur-str     (channels/format-duration dur-ms)
                      ;; Right-aligned superscript code label with right padding
                      expr-label  (label-text "code" (inc idx))
                      expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]
                                    (str (repeat-str \space pl) expr-label " "))
                      ;; Right-aligned status line: ✓ 3ms or ✗ 3ms
                      status-text (when has-status?
                                    (str (if success? "✓" "✗")
                                      (when dur-str (str " " dur-str))))
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
                      ;; Code lines: pretty-printed, indented
                      code-text   (str/trim (or form ""))
                      formatted   (channels/format-clojure code-text (max 1 (- fill-w 2)))
                      code-lines  (str/split-lines formatted)
                      c-lines     (mapv #(str c-marker "  " %) code-lines)
                      ;; Result
                      result-str  (when results (get results idx))
                      r-marker    (if is-error? err-result-marker result-marker)
                      r-wrapped   (when (and result-str (not (str/blank? (str result-str))))
                                    (wrap-text (str/trim (str result-str))
                                      (max 1 (- fill-w 2))))
                      r-lines     (when (seq r-wrapped)
                                    (into [(str r-marker "  " (first r-wrapped))]
                                      (map #(str r-marker "  " %) (rest r-wrapped))))
                      ;; Code block = header + pad-top + code + gap + result + status + pad-bottom
                      code-block  (vec (concat
                                         [(str iter-hdr-marker expr-hdr)]  ;; right-aligned code ₁
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
                                           slabel-ln  (str iter-hdr-marker
                                                        (repeat-str \space slabel-pad)
                                                        slabel " ")
                                           text-lines (mapv #(str stdout-marker "  " %)
                                                        (wrap-text (str/trim (str stdout-str))
                                                          (max 1 (- fill-w 2))))]
                                       (vec (concat
                                              [slabel-ln]               ;; right-aligned ˢᵗᵈᵒᵘᵗ
                                              [(str stdout-pad-marker "")] ;; pad top
                                              text-lines
                                              [(str stdout-pad-marker "")]))))  ;; pad bottom
                      ;; Iter-bg gap between code and stdout within the group
                      margin      (when stdout-block [(str iter-pad-marker "")])]
                  (concat
                    ;; Spacer between consecutive code blocks (not before the first)
                    (when (pos? idx) [(str iter-pad-marker "")])
                    code-block margin stdout-block))))
            (map-indexed vector code)))
        ;; Wrap all expressions in an iteration-bg group:
        ;; iter-pad top + expression blocks + iter-pad bottom
        grouped (when (seq code+result-lines)
                  (vec (concat
                         [(str iter-pad-marker "")] ;; group top
                         code+result-lines
                         [(str iter-pad-marker "")]))) ;; group bottom
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

(defn- slow-suffix
  "Escalating suffix that tells the user the provider is unusually
   quiet. We show nothing for the first 30s (every LLM call takes a
   few seconds), bump up at 30s, sharpen at 60s, and turn it into a
   call-to-action at 120s. The thresholds match what humans actually
   feel: <30s is \"normal\", 60s is \"this is slow\", 2 min is
   \"something is wrong, you can bail\". Without this the spinner
   said `sending request to provider…` for 143 seconds straight
   while glm-5.1 hung — zero feedback that anything was off."
  [^Long elapsed-ms]
  (cond
    (or (nil? elapsed-ms) (< (long elapsed-ms) 30000)) ""
    (< (long elapsed-ms) 60000)  " — still waiting"
    (< (long elapsed-ms) 120000) " — provider slow"
    :else                         " — provider unresponsive, Esc to cancel"))

(defn- progress-phase
  "Human-readable phase label for the current iteration state. Drives
   the spinner row text so the user can tell whether we're waiting on
   the provider, thinking, executing, or recovering.

   `elapsed-ms` is the wall-clock since query-start. When a single
   iteration drags on — typically iter 0 hanging on `(llm/ask!…)` —
   the phase escalates via `slow-suffix` so the user knows the issue
   is upstream, not a UI freeze."
  [iterations cancelling? elapsed-ms]
  (let [n          (count iterations)
        last-iter  (last iterations)
        err        (:error last-iter)
        errored?   (some? err)
        thinking?  (and (not errored?)
                     (some? (:thinking last-iter))
                     (not (str/blank? (:thinking last-iter))))
        executing? (and (not errored?) last-iter (seq (:code last-iter)))
        suffix     (slow-suffix elapsed-ms)]
    (cond
      cancelling? "cancelling"
      errored?    (let [label (prettify-error-type err)]
                    (str "iter " n
                      (when label (str " — " label))
                      " — retrying"))
      (zero? n)   (str "sending request to provider" suffix)
      thinking?   (str "thinking (iter " n ")" suffix)
      executing?  (str "executing code (iter " n ")" suffix)
      :else       (str "working (iter " n ")" suffix))))

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
         {:keys [now-ms query-start-ms cancelling?]} extra
         now-ms           (long (or now-ms (System/currentTimeMillis)))
         elapsed-ms       (when query-start-ms
                            (max 0 (- now-ms (long query-start-ms))))
         elapsed-str      (or (channels/format-duration elapsed-ms) "0ms")
         spinner-line     (str (spinner-frame now-ms) "  "
                            (progress-phase iterations cancelling? elapsed-ms) "…  "
                            elapsed-str "  ·  Esc to cancel")
         trace-lines      (when (and show-iterations? (seq iterations))
                            (into []
                              (mapcat (fn [[idx entry]]
                                        (format-iteration-entry
                                          (if show-thinking? entry (dissoc entry :thinking))
                                          content-w (inc idx))))
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
  "Render one cell padded to `w` columns with the given alignment.
   Returns ` <padded content> ` — the leading and trailing spaces are
   mandatory: they sit between the `│` column dividers, giving every
   cell a one-space inner margin so text never touches the vertical
   lines.

   `align` is `:left` (default, trailing pad) or `:right` (leading
   pad). Numeric columns are auto-detected as `:right` so columns of
   `389 / 12 104 / 4 879` line up on the units position — the only
   way numeric tables read naturally."
  [text w align]
  (let [t (str text)
        t (cond
            (<= (count t) w) t
            (>= w 2)         (str (subs t 0 (max 0 (dec w))) "…")
            :else            (subs t 0 (max 0 w)))
        pad (repeat-str \space (max 0 (- w (count t))))
        body (case align
               :right (str pad t)
               (str t pad))]
    (str " " body " ")))

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
   that exceed `max-w` get their column widths shrunk proportionally
   so the result always fits inside the bubble."
   
  [headers rows max-w {:keys [thead tsep trow]}]
  (let [n-cols    (max 1 (apply max (count headers) (map count rows)))
        pad-cells (fn [r] (vec (concat r (repeat (max 0 (- n-cols (count r))) ""))))
        h         (pad-cells headers)
        rs        (mapv pad-cells rows)
        col-widths-raw
        (mapv (fn [i]
                (apply max 1
                  (count (nth h i ""))
                  (map #(count (nth % i "")) rs)))
          (range n-cols))
        ;; Total layout width = sum(col widths) + 2*n_cols (per-cell
        ;; one-space padding on each side of the cell text) +
        ;; (n_cols + 1) verticals (the `│` column dividers, one on
        ;; each end + one between every pair of cells).
        overhead   (+ (* 2 n-cols) (inc n-cols))
        avail      (max n-cols (- max-w overhead))
        sum-w      (max 1 (reduce + 0 col-widths-raw))
        col-widths (if (<= (+ sum-w overhead) max-w)
                     col-widths-raw
                     (let [scale  (/ (double avail) sum-w)
                           shrunk (mapv #(max 1 (int (* % scale))) col-widths-raw)
                           used   (reduce + 0 shrunk)
                           extra  (max 0 (- avail used))]
                       (if (zero? extra)
                         shrunk
                         (vec (map-indexed
                                (fn [i w] (if (< i extra) (inc w) w))
                                shrunk)))))
        bar        (fn [w] (repeat-str \─ (+ w 2)))
        top-line   (str "┌" (str/join "┬" (map bar col-widths)) "┐")
        head-sep   (str "├" (str/join "┼" (map bar col-widths)) "┤")
        bot-line   (str "└" (str/join "┴" (map bar col-widths)) "┘")
        ;; Per-column alignment: numeric columns right-align, every
        ;; other column stays left. The header inherits the column's
        ;; alignment so the title sits over the data correctly
        ;; (e.g. a `Predkosc (km/h)` header above right-aligned
        ;; integers also right-aligns).
        col-aligns (mapv (fn [i] (column-align (map #(nth % i "") rs)))
                     (range n-cols))
        format-row (fn [cells]
                     (str "│"
                       (str/join "│"
                         (map-indexed
                           (fn [i c] (pad-cell c (nth col-widths i) (nth col-aligns i)))
                           cells))
                       "│"))]
    (vec (concat
           [(str tsep top-line)]
           [(str thead (format-row h))]
           [(str tsep head-sep)]
           (mapv #(str trow (format-row %)) rs)
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
     - `# / ## / ###` ATX headings (1–3)
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
               in-code?
               (recur rst true
                 (into acc (mapv #(str (:code m) "  " %)
                             (wrap-text line (max 1 (- max-w 2))))))

               ;; GFM pipe table: header + |---| + rows
               (and (table-row? line)
                 (table-separator-row? (first rst)))
               (let [[tbl tail] (consume-table lines max-w m)]
                 (recur (seq tail) false (into acc tbl)))

               ;; Heading 3
               (str/starts-with? line "### ")
               (recur rst false
                 (into acc (mapv #(str (:h3 m) %)
                             (wrap-text (subs line 4) max-w))))

               ;; Heading 2
               (str/starts-with? line "## ")
               (recur rst false
                 (into acc (mapv #(str (:h2 m) %)
                             (wrap-text (subs line 3) max-w))))

               ;; Heading 1
               (str/starts-with? line "# ")
               (recur rst false
                 (into acc (mapv #(str (:h1 m) %)
                             (wrap-text (subs line 2) max-w))))

               ;; Horizontal rule: --- / *** / ___
               (re-matches #"^\s*([-*_])\1{2,}\s*$" line)
               (recur rst false
                 (conj acc (str (:hr m) (repeat-str \─ max-w))))

               ;; Blockquote
               (re-matches #"^\s*>\s?.*" line)
               (let [trimmed (str/replace line #"^\s*>\s?" "")
                     wrapped (wrap-text trimmed (max 1 (- max-w 2)))]
                 (recur rst false
                   (into acc (mapv #(str (:quote m) "┃ " %) wrapped))))

               ;; Bullet list (- / * / +) — keep one trailing space.
               (re-matches #"^\s*[-*+]\s+.*" line)
               (let [trimmed (str/replace line #"^\s*[-*+]\s+" "")
                     trimmed (-> trimmed
                               (str/replace #"\*\*([^*]+)\*\*" "$1")
                               (str/replace #"`([^`]+)`" "$1"))
                     wrapped (wrap-text trimmed (max 1 (- max-w 4)))]
                 (recur rst false
                   (into acc
                     (into [(str (:bullet m) "  • " (first wrapped))]
                       (mapv #(str (:bullet m) "    " %) (rest wrapped))))))

               ;; Numbered list (1. 2.)
               (re-matches #"^\s*\d+[.)]\s+.*" line)
               (let [[_ num body] (re-matches #"^\s*(\d+)[.)]\s+(.*)$" line)
                     body (-> (or body "")
                            (str/replace #"\*\*([^*]+)\*\*" "$1")
                            (str/replace #"`([^`]+)`" "$1"))
                     prefix (str num ". ")
                     wrapped (wrap-text body (max 1 (- max-w (count prefix) 2)))]
                 (recur rst false
                   (into acc
                     (into [(str (:bullet m) "  " prefix (first wrapped))]
                       (mapv #(str (:bullet m) "    " (repeat-str \space (count prefix)) %)
                         (rest wrapped))))))

               ;; Bold-only line: **text**
               (and (str/starts-with? (str/trim line) "**")
                 (str/ends-with? (str/trim line) "**")
                 (> (count (str/trim line)) 4))
               (let [trimmed (str/trim line)
                     inner   (subs trimmed 2 (- (count trimmed) 2))]
                 (recur rst false
                   (into acc (mapv #(str (:bold m) %)
                               (wrap-text inner max-w)))))

               ;; Blank line
               (str/blank? line)
               (recur rst false (conj acc (emit-plain "")))

               ;; Plain text — strip inline **bold** and `code` markers
               ;; (mid-line styled spans aren't supported in the grid).
               :else
               (let [clean (-> line
                             (str/replace #"\*\*([^*]+)\*\*" "$1")
                             (str/replace #"`([^`]+)`" "$1"))]
                 (recur rst false
                   (into acc (mapv emit-plain (wrap-text clean max-w)))))))))))))

(defn format-answer-with-thinking*
  "Uncached implementation. See `format-answer-with-thinking`."
  [answer trace bubble-w settings confidence]
  (let [content-w (max 10 (- bubble-w 4))
        fill-w    (max 1 (dec content-w))
        show-thinking?   (get settings :show-thinking true)
        show-iterations? (get settings :show-iterations true)
        trace-lines (when (and show-iterations? (seq trace))
                      (into []
                        (mapcat (fn [[idx entry]]
                                  (format-iteration-entry
                                    (if show-thinking? entry (dissoc entry :thinking))
                                    content-w (inc idx))))
                        (collapse-repeated-error-runs trace)))
        answer-str  (or answer "")
        ;; Right-aligned "FINAL ANSWER" header with confidence
        fa-label    (label-text "final answer")
        conf-str    (when confidence (str " · " (name confidence)))
        full-label  (str fa-label (or conf-str ""))
        fa-pad      (max 0 (- fill-w (count full-label) 1))
        fa-hdr      (str answer-hdr-marker (repeat-str \space fa-pad) full-label " ")
        ;; Answer lines — keep markdown markers intact so they render properly.
        ;; The in-answer? zone in draw-chat-bubble! fills answer-bg for ALL
        ;; lines after the answer-hdr, regardless of their marker type.
        md-lines    (markdown->lines answer-str (max 1 (- fill-w 2)))
        ans-lines   (if (seq md-lines)
                      md-lines
                      (wrap-text answer-str (max 1 (- fill-w 2))))
        ans-pad     (str answer-pad-marker "")
        ans-sep     (str answer-sep-marker "")]
    (if (seq trace-lines)
      (str/join "\n" (concat trace-lines
                       [ans-sep] [fa-hdr] [ans-pad] ans-lines [ans-pad]))
      (str/join "\n" (concat [fa-hdr] [ans-pad] ans-lines [ans-pad])))))

(defn format-answer-with-thinking
  "Build the final bubble text: thinking trace + answer. Memoized by
   `(answer-identity, trace-identity, bubble-w, settings, confidence)`.

   Finalized assistant messages never mutate after `:message-received`,
   so the same key tuple comes in on every subsequent frame and we
   short-circuit straight to the cached string. This is the single
   biggest CPU win for long conversations."
  ([answer trace bubble-w] (format-answer-with-thinking answer trace bubble-w nil nil))
  ([answer trace bubble-w settings] (format-answer-with-thinking answer trace bubble-w settings nil))
  ([answer trace bubble-w settings confidence]
   (cached* [::fawt
             (System/identityHashCode answer)
             (System/identityHashCode trace)
             (long bubble-w)
             (boolean (get settings :show-thinking true))
             (boolean (get settings :show-iterations true))
             confidence]
     #(format-answer-with-thinking* answer trace bubble-w settings confidence))))

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

(def ^:private msg-margin-top    1)  ;; rows above first message
(def ^:private msg-margin-bottom 1)  ;; rows below last message
(def ^:private msg-margin-left   3)  ;; cols left gutter (matches input box gutter)
(def ^:private msg-margin-right  3)  ;; cols right gutter (room for scrollbar + air)

(defn draw-messages-area!
  "Draw structured chat messages as left-aligned blocks inside a clean,
   border-less scrolling area.

   No outer box, no title bar — just a vertical column of messages with
   generous side gutters. The right gutter doubles as scrollbar space
   when the conversation overflows. The conversation title (if any) is
   surfaced via the input-box bottom status line, not here.

   `messages` is a vec of {:role :user|:assistant, :text str}.
   `scroll` is a row offset into the virtual content, or nil for auto-bottom.
   `opts` is currently unused (kept for back-compat)."
  ([g messages box-top box-bottom cols scroll]
   (draw-messages-area! g messages box-top box-bottom cols scroll nil))
  ([g messages box-top box-bottom cols scroll _opts]
   (let [text-top   (+ box-top msg-margin-top)
         inner-h    (max 0 (- box-bottom text-top msg-margin-bottom))
         bubble-w   (max 1 (- cols msg-margin-left msg-margin-right))
         heights    (mapv #(bubble-height % bubble-w) messages)
         total-h    (reduce + 0 heights)
         eff-scroll (let [s (or scroll (max 0 (- total-h inner-h)))]
                      (min s (max 0 (- total-h inner-h))))
         offsets    (reductions + 0 heights)]

     ;; Background fill (no border, no title bar).
     (p/set-colors! g t/text-fg t/terminal-bg)
     (p/fill-rect! g 0 box-top cols (max 0 (- box-bottom box-top)))

     (let [clip (.newTextGraphics g
                  (TerminalPosition. 0 text-top)
                  (TerminalSize. cols inner-h))]
       (doseq [idx (range (count messages))]
         (let [msg-top (- (long (nth offsets idx)) eff-scroll)
               msg-h   (nth heights idx)]
           (when (and (> (+ msg-top msg-h) 0)
                   (< msg-top inner-h))
             (draw-chat-bubble! clip (nth messages idx) msg-top msg-margin-left bubble-w))))

       (when (> total-h inner-h)
         (let [max-scroll (max 1 (- total-h inner-h))
               track-h    inner-h
               thumb-h    (max 1 (int (* track-h (/ (double inner-h) total-h))))
               thumb-pos  (int (* (- track-h thumb-h) (/ (double eff-scroll) max-scroll)))
               ;; Place the scrollbar inside the right gutter so it
               ;; never overlaps message content.
               bar-col    (- cols 2)
               bar-top    text-top]
           (doseq [r (range track-h)]
             (p/set-colors! g t/border-fg t/terminal-bg)
             (p/set-char! g bar-col (+ bar-top r) Symbols/SINGLE_LINE_VERTICAL))
           (doseq [r (range thumb-h)]
             (p/set-colors! g t/dialog-hint-key t/terminal-bg)
             (p/set-char! g bar-col (+ bar-top thumb-pos r) \u2588))))))))
