(ns com.blockether.vis.adapters.tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.adapters.tui.primitives :as p]
            [com.blockether.vis.adapters.tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize Symbols]
           [java.text SimpleDateFormat]))

;;; ── Text wrapping ───────────────────────────────────────────────────────────

(defn wrap-text
  "Wrap text to fit within max-width. Breaks at word boundaries when possible,
   hard-breaks mid-word only if a single word exceeds max-width.
   Handles multi-line input (splits on newlines first).
   Returns a vec of wrapped lines."
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

(defn- draw-box-border!
  "Draw a single-line box border. Optionally center a hint string in the top edge."
  [g box-top box-bottom cols hint]
  (let [inner-w (- cols 2)]
    (.setForegroundColor g t/border-fg)
    (.setBackgroundColor g t/terminal-bg)

    ;; Top: ┌── hint ──┐
    (let [bar (apply str (repeat inner-w Symbols/SINGLE_LINE_HORIZONTAL))]
      (if hint
        (let [start (max 0 (quot (- inner-w (count hint)) 2))
              end   (min inner-w (+ start (count hint)))
              inner (str (subs bar 0 start) hint (subs bar end))]
          (.setCharacter g 0 box-top Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
          (.putString g 1 box-top inner)
          (.setCharacter g (dec cols) box-top Symbols/SINGLE_LINE_TOP_RIGHT_CORNER))
        (do
          (.setCharacter g 0 box-top Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
          (.putString g 1 box-top bar)
          (.setCharacter g (dec cols) box-top Symbols/SINGLE_LINE_TOP_RIGHT_CORNER))))

    ;; Bottom: └──────┘
    (.setCharacter g 0 box-bottom Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
    (.putString g 1 box-bottom (apply str (repeat inner-w Symbols/SINGLE_LINE_HORIZONTAL)))
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

    (draw-box-border! g box-top box-bottom cols " messages ")
    (fill-box-interior! g box-top box-bottom cols)

    (doseq [[i msg] (map-indexed vector visible)]
      (.setForegroundColor g t/box-fg)
      (.setBackgroundColor g t/box-bg)
      (.putString g (inc t/pad-x) (+ text-top i)
        (subs msg 0 (min (count msg) text-w))))))

;;; ── Input box ──────────────────────────────────────────────────────────────

(defn draw-input-box!
  "Draw bordered input area. Returns [cursor-col cursor-row] in screen coords."
  [g {:keys [lines crow ccol]} box-top text-rows cols hint]
  (let [text-top   (inc box-top)
        box-bottom (+ box-top text-rows 1)
        text-w     (- cols 2 (* 2 t/pad-x))
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
              (.putString g (inc t/pad-x) (+ text-top i)
                (subs line offset (min len (+ offset text-w)))))))))

    ;; Cursor position
    [(+ (inc t/pad-x) (- ccol h-scroll))
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
         inner-w     (- box-w 2)]

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
     (.putString g (inc box-left) box-top
       (apply str (repeat inner-w Symbols/SINGLE_LINE_HORIZONTAL)))
     (.putString g (inc box-left) box-bottom
       (apply str (repeat inner-w Symbols/SINGLE_LINE_HORIZONTAL)))
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

(def ^:private time-fmt (SimpleDateFormat. "HH:mm"))
(def ^:private date-fmt (SimpleDateFormat. "MMM d, HH:mm"))

(defn- format-timestamp
  "Format timestamp for display. Shows 'HH:mm' for today, 'MMM d, HH:mm' for older."
  [^java.util.Date ts]
  (when ts
    (let [now   (java.util.Date.)
          today (.format (SimpleDateFormat. "yyyyMMdd") now)
          day   (.format (SimpleDateFormat. "yyyyMMdd") ts)]
      (if (= today day)
        (.format time-fmt ts)
        (.format date-fmt ts)))))

(defn- format-duration
  "Format millisecond duration as human-readable. e.g. '2.3s', '1m 15s'"
  [ms]
  (when (and ms (pos? ms))
    (cond
      (< ms 1000)  (str ms "ms")
      (< ms 60000) (format "%.1fs" (/ ms 1000.0))
      :else        (let [m (quot ms 60000)
                         s (quot (mod ms 60000) 1000)]
                     (str m "m " s "s")))))

(defn draw-chat-bubble!
  "Draw a chat message bubble at the given row.
   `msg` is a map: {:role :user|:assistant, :text str, :timestamp #inst}
   Optional `:duration-ms` for assistant response time.
   `left` and `max-w` define the horizontal bounds.
   Returns the number of screen rows consumed (including spacing)."
  [g {:keys [role text timestamp duration-ms]} start-row left max-w]
  (let [user?     (= role :user)
        label     (if user? "you" "vis")
        label-w   (count label)
        bubble-w  (min max-w (max 20 (+ 4 (min (count text) (- max-w 4)))))
        content-w (- bubble-w 4)
        lines     (wrap-text text content-w)
        bubble-h  (+ (count lines) 2)
        bx        (if user? (+ left (- max-w bubble-w)) left)
        bg-color  (if user? t/user-bubble-bg t/ai-bubble-bg)
        fg-color  (if user? t/user-bubble-fg t/ai-bubble-fg)
        brd-color (if user? t/user-bubble-border t/ai-bubble-border)
        role-fg   (if user? t/user-role-fg t/ai-role-fg)
        inner-w   (- bubble-w 2)
        ;; Metadata line below bubble (assistant only)
        time-str  (format-timestamp timestamp)
        dur-str   (format-duration duration-ms)
        meta-str  (when (and (not user?) (or time-str dur-str))
                    (str/join "  " (remove nil? [time-str (when dur-str (str "⏱ " dur-str))])))]

    ;; Role label (above bubble)
    (p/set-colors! g role-fg t/terminal-bg)
    (p/put-str! g (if user? (+ bx bubble-w (- label-w) -1) (inc bx)) start-row label)

    (let [btop (inc start-row)]
      ;; Fill interior
      (p/set-bg! g bg-color)
      (p/fill-rect! g bx btop bubble-w bubble-h)

      ;; Border
      (p/set-colors! g brd-color bg-color)
      (p/set-char! g bx btop Symbols/SINGLE_LINE_TOP_LEFT_CORNER)
      (p/set-char! g (+ bx bubble-w -1) btop Symbols/SINGLE_LINE_TOP_RIGHT_CORNER)
      (p/put-str! g (inc bx) btop (apply str (repeat inner-w Symbols/SINGLE_LINE_HORIZONTAL)))
      (let [bbot (+ btop bubble-h -1)]
        (p/set-char! g bx bbot Symbols/SINGLE_LINE_BOTTOM_LEFT_CORNER)
        (p/set-char! g (+ bx bubble-w -1) bbot Symbols/SINGLE_LINE_BOTTOM_RIGHT_CORNER)
        (p/put-str! g (inc bx) bbot (apply str (repeat inner-w Symbols/SINGLE_LINE_HORIZONTAL))))
      (doseq [r (range (inc btop) (+ btop bubble-h -1))]
        (p/set-char! g bx r Symbols/SINGLE_LINE_VERTICAL)
        (p/set-char! g (+ bx bubble-w -1) r Symbols/SINGLE_LINE_VERTICAL))

      ;; Text content
      (p/set-colors! g fg-color bg-color)
      (doseq [[i line] (map-indexed vector lines)]
        (p/put-str! g (+ bx 2) (+ btop 1 i) line))

      ;; Metadata line (assistant only, offset left from bubble right edge)
      (let [meta-row (+ btop bubble-h)]
        (when meta-str
          (p/set-colors! g t/dialog-hint t/terminal-bg)
          (p/styled g [p/ITALIC]
            (p/put-str! g (+ bx (max 0 (- bubble-w (count meta-str) 2))) meta-row meta-str))))

      ;; Total rows consumed: label(1) + bubble-h + meta(1) + gap(1)
      (+ 1 bubble-h 2))))

(defn bubble-height
  "Calculate rows a chat bubble will consume without drawing.
   label(1) + border(2) + wrapped-lines + meta(1) + gap(1)."
  [{:keys [text]} max-w]
  (let [bubble-w  (min max-w (max 20 (+ 4 (min (count text) (- max-w 4)))))
        content-w (- bubble-w 4)
        lines     (wrap-text text content-w)]
    (+ 1 (count lines) 2 2)))

(defn total-messages-height
  "Calculate total row height for a vec of structured messages."
  [messages max-w]
  (reduce + 0 (map #(bubble-height % max-w) messages)))

;;; ── Progress timeline formatting ───────────────────────────────────────────

(def ^:private progress-thinking-prefix "  ")
(def ^:private progress-code-prefix     "  ↳ ")

(defn- truncate-single-line
  "Collapse multi-line text to a single line (first non-blank) and cap length."
  [s max-len]
  (when (string? s)
    (let [first-line (->> (str/split-lines s)
                       (map str/trim)
                       (remove str/blank?)
                       first)]
      (when first-line
        (if (> (count first-line) max-len)
          (str (subs first-line 0 (max 0 (- max-len 1))) "…")
          first-line)))))

(defn- format-iteration-entry
  "Turn one progress iteration into display lines. Returns a vec of strings.
   First line is the iteration header; subsequent lines are thinking tail
   (multi-line, preserved) and streamed code forms (single-line each)."
  [{:keys [iteration thinking code final?]} code-width]
  (let [header (str "• Iteration " (inc iteration)
                 (when final? " · finalizing"))
        thinking-lines (when (and (string? thinking) (seq (str/trim thinking)))
                         (->> (str/split-lines thinking)
                           (map str/trimr)
                           (remove str/blank?)
                           (mapv #(str progress-thinking-prefix %))))
        code-lines (when (seq code)
                     (into []
                       (keep (fn [form]
                               (when-let [one (truncate-single-line form code-width)]
                                 (str progress-code-prefix one))))
                       code))]
    (into [header] (concat thinking-lines code-lines))))

(defn progress->text
  "Build the text body of the live progress placeholder bubble.

   `progress` is the `:progress` slot from app-db: `{:iterations [...]}`.
   `bubble-w` is the outer bubble width in chars — we size inner content
   conservatively so wrap-text inside draw-chat-bubble! doesn't blow up.
   Returns a single string with newlines separating lines.

   Returns the literal \"thinking...\" string when the progress timeline is
   empty, so the placeholder still shows *something* in the first 100ms
   before the first chunk arrives."
  [progress bubble-w]
  (let [iterations (:iterations progress)
        content-w  (max 10 (- bubble-w 6))] ;; leave room for prefix + padding
    (if (empty? iterations)
      "thinking..."
      (str/join "\n"
        (into []
          (mapcat #(format-iteration-entry % content-w))
          iterations)))))

;;; ── Messages area (bubble-based) ───────────────────────────────────────────

(defn draw-messages-area!
  "Draw structured chat messages as bubbles inside a bordered area with scroll.
   `messages` is a vec of {:role :user|:assistant, :text str}.
   `scroll` is a row offset into the virtual content, or nil for auto-bottom."
  [g messages box-top box-bottom cols scroll]
  (let [inner-h   (- box-bottom box-top 1)
        text-top  (inc box-top)
        bubble-w  (- cols 4)
        ;; Pre-compute cumulative heights for scroll math
        heights   (mapv #(bubble-height % bubble-w) messages)
        total-h   (reduce + 0 heights)
        ;; Scroll: nil = auto-bottom
        eff-scroll (let [s (or scroll (max 0 (- total-h inner-h)))]
                     (min s (max 0 (- total-h inner-h))))
        ;; Cumulative row offsets for each message
        offsets   (reductions + 0 heights)]

    ;; Draw the container
    (draw-box-border! g box-top box-bottom cols " messages ")
    (fill-box-interior! g box-top box-bottom cols)

    ;; Render visible bubbles (clipped to inner area)
    (doseq [idx (range (count messages))]
      (let [msg-top    (- (long (nth offsets idx)) eff-scroll)
            msg-h      (nth heights idx)
            screen-row (+ text-top msg-top)]
        ;; Draw only if bubble overlaps the visible viewport
        (when (and (> (+ msg-top msg-h) 0)
                (< msg-top inner-h)
                (< screen-row box-bottom)
                (>= (+ screen-row msg-h) text-top))
          (draw-chat-bubble! g (nth messages idx) screen-row (inc t/pad-x) bubble-w))))))
