(ns com.blockether.vis.channels.tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.tui.primitives :as p]
            [com.blockether.vis.channels.tui.theme :as t])
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

(def input-pad-y 1)  ;; internal vertical padding (rows above/below text)
(def ^:private input-pad-x 2)  ;; internal horizontal padding (cols left/right of text)

(defn draw-input-box!
  "Draw bordered input area with internal padding. Returns [cursor-col cursor-row] in screen coords."
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

(defn- format-timestamp
  "Format timestamp for display in local timezone. Always dd-MM-yyyy HH:mm."
  [^java.util.Date ts]
  (when ts
    (let [tz (java.util.TimeZone/getDefault)]
      (.format (doto (SimpleDateFormat. "dd-MM-yyyy HH:mm") (.setTimeZone tz)) ts))))

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

(def ^:private thinking-marker "\u200B") ;; zero-width space marks italic lines

(defn draw-chat-bubble!
  "Draw a chat message bubble at the given row.
   `msg` is a map: {:role :user|:assistant, :text str, :timestamp #inst}
   Optional `:duration-ms` for assistant response time.
   `left` and `max-w` define the horizontal bounds.
   Returns the number of screen rows consumed (including spacing)."
  [g {:keys [role text timestamp duration-ms model iterations tokens cost]} start-row left max-w]
  (let [user?     (= role :user)
        label     (if user? "you" "vis")
        label-w   (count label)
        bubble-w  (min max-w (max 60 (+ 4 (min (count text) (- max-w 4)))))
        content-w (- bubble-w 4)
        lines     (wrap-text text content-w)
        bubble-h  (+ (count lines) 2)
        bx        (if user? left (+ left (- max-w bubble-w)))
        bg-color  (if user? t/user-bubble-bg t/ai-bubble-bg)
        fg-color  (if user? t/user-bubble-fg t/ai-bubble-fg)
        brd-color (if user? t/user-bubble-border t/ai-bubble-border)
        role-fg   (if user? t/user-role-fg t/ai-role-fg)
        inner-w   (- bubble-w 2)
        time-str  (format-timestamp timestamp)
        dur-str   (format-duration duration-ms)
        tok-in    (when-let [n (:input tokens)] (str "↑" n))
        tok-out   (when-let [n (:output tokens)] (str "↓" n))
        iter-str  (when (and (not user?) iterations) (str iterations (if (= 1 iterations) " iter" " iters")))
        cost-str  (when-let [c (some-> cost :total-cost)]
                    (str "~$" (String/format java.util.Locale/US "%.6f" (into-array Object [(double c)]))))
        ;; Below-bubble meta (assistant only): model · iters · ctx-in · ctx-out · ~cost · duration
        meta-parts (when (not user?)
                     (remove nil? [model iter-str tok-in tok-out cost-str dur-str]))
        meta-str   (when (seq meta-parts) (str/join " · " meta-parts))]

    ;; Label row: role name left (bold), timestamp right-aligned
    (p/set-colors! g role-fg t/terminal-bg)
    (p/enable! g p/BOLD)
    (p/put-str! g (inc bx) start-row label)
    (p/disable! g p/BOLD)
    (when time-str
      (p/set-colors! g t/dialog-hint t/terminal-bg)
      (p/put-str! g (+ bx (- bubble-w (count time-str) 1)) start-row time-str))

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

      ;; Text content — lines prefixed with thinking-marker render italic
      (doseq [[i line] (map-indexed vector lines)]
        (if (str/starts-with? line thinking-marker)
          (do (p/set-colors! g t/dialog-hint bg-color)
            (p/styled g [p/ITALIC]
              (p/put-str! g (+ bx 2) (+ btop 1 i) (subs line (count thinking-marker)))))
          (do (p/set-colors! g fg-color bg-color)
            (p/put-str! g (+ bx 2) (+ btop 1 i) line))))

      ;; Below-bubble meta (assistant only): model · iters · duration · tokens
      (let [meta-row (+ btop bubble-h)]
        (when meta-str
          (p/set-colors! g t/dialog-hint t/terminal-bg)
          (p/put-str! g (+ bx (max 0 (- bubble-w (count meta-str) 1))) meta-row meta-str)))

      ;; Total rows consumed: label(1) + bubble-h + meta(1) + gap(1)
      (+ 1 bubble-h 2))))

(defn bubble-height
  "Calculate rows a chat bubble will consume without drawing.
   label(1) + border(2) + wrapped-lines + meta(1) + gap(1)."
  [{:keys [text]} max-w]
  (let [bubble-w  (min max-w (max 60 (+ 4 (min (count text) (- max-w 4)))))
        content-w (- bubble-w 4)
        lines     (wrap-text text content-w)]
    (+ 1 (count lines) 2 2)))

(defn total-messages-height
  "Calculate total row height for a vec of structured messages."
  [messages max-w]
  (reduce + 0 (map #(bubble-height % max-w) messages)))

;;; ── Progress timeline formatting ───────────────────────────────────────────

(defn- truncate-line [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 (max 0 (- max-len 1))) "...")
      s)))

(defn- format-iteration-entry
  "Format one iteration's thinking + code + results into display lines.
   Thinking lines get the thinking-marker prefix for italic rendering.
   Code lines are shown in full (wrapped). Results follow each code block."
  [{:keys [thinking code results]} code-width]
  (let [thinking-lines (when (and (string? thinking) (not (str/blank? thinking)))
                         (mapv #(str thinking-marker "> " %)
                           (wrap-text (str/trim thinking)
                             (max 1 (- code-width 2)))))
        code+result-lines
        (when (seq code)
          (into []
            (mapcat
              (fn [[idx form]]
                (let [code-lines (mapv #(truncate-line % code-width)
                                  (str/split-lines (str/trim (or form ""))))
                      result-str (when results (get results idx))
                      result-lines (when (and result-str (not (str/blank? (str result-str))))
                                     [(truncate-line (str "-> " (str/trim (str result-str))) code-width)])]
                  (concat code-lines result-lines))))
            (map-indexed vector code)))]
    (into (or thinking-lines []) code+result-lines)))

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
        content-w  (max 10 (- bubble-w 4))]
    (if (empty? iterations)
      "..."
      (str/join "\n"
        (into []
          (mapcat #(format-iteration-entry % content-w))
          iterations)))))

(defn format-answer-with-thinking
  "Build the final bubble text: thinking trace + answer.
   `trace` is the progress iterations vec [{:thinking :code} ...]."
  [answer trace bubble-w]
  (let [content-w (max 10 (- bubble-w 4))
        trace-lines (when (seq trace)
                      (into []
                        (mapcat #(format-iteration-entry % content-w))
                        trace))
        answer-str (or answer "")]
    (if (seq trace-lines)
      (str (str/join "\n" trace-lines) "\n\n" answer-str)
      answer-str)))

;;; ── Messages area (bubble-based) ───────────────────────────────────────────

(def ^:private msg-margin-top 1)
(def ^:private msg-margin-bottom 1)

(defn draw-messages-area!
  "Draw structured chat messages as bubbles inside a bordered area with scroll.
   `messages` is a vec of {:role :user|:assistant, :text str}.
   `scroll` is a row offset into the virtual content, or nil for auto-bottom.
   `opts` may contain `:title` for the box header."
  ([g messages box-top box-bottom cols scroll]
   (draw-messages-area! g messages box-top box-bottom cols scroll nil))
  ([g messages box-top box-bottom cols scroll {:keys [title]}]
  (let [inner-h   (- box-bottom box-top 1 msg-margin-top msg-margin-bottom)
        text-top  (+ (inc box-top) msg-margin-top)
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
    (draw-box-border! g box-top box-bottom cols
      (if title
        (str " " (subs title 0 (min (count title) (- cols 6))) " ")
        " messages "))
    (fill-box-interior! g box-top box-bottom cols)

    ;; Clip to interior so bubbles never overflow into borders or input box
    (let [clip (.newTextGraphics g
                 (TerminalPosition. 0 text-top)
                 (TerminalSize. cols inner-h))]
      ;; Render visible bubbles inside clipped region
      ;; Coordinates passed to draw-chat-bubble! are in screen space,
      ;; but the clip offsets them — so we adjust to clip-local coords.
      (doseq [idx (range (count messages))]
        (let [msg-top    (- (long (nth offsets idx)) eff-scroll)
              msg-h      (nth heights idx)]
          ;; Draw only if bubble overlaps the visible viewport
          (when (and (> (+ msg-top msg-h) 0)
                  (< msg-top inner-h))
            (draw-chat-bubble! clip (nth messages idx) msg-top (inc t/pad-x) bubble-w))))))))
