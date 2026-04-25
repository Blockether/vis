(ns com.blockether.vis.channels.tui.render
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.primitives :as p]
            [com.blockether.vis.channels.tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize Symbols]))

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
        time-str  (channels/format-date timestamp)
        dur-str   (channels/format-duration duration-ms)
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

      ;; Text content — per-line styling via invisible marker prefixes
      (doseq [[i line] (map-indexed vector lines)]
        (let [x (+ bx 2) y (+ btop 1 i)
              inner-w (- bubble-w 4)]
          (cond
            ;; Iteration header — distinct background with colored badges
            (str/starts-with? line iter-hdr-marker)
            (let [raw (subs line 1)]
              ;; Background fill + base text
              (p/set-colors! g t/iter-header-fg t/iter-header-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/put-str! g x y raw)
              ;; Overlay colored ✓N / ✗N fragments
              (doseq [[pattern color] [["✓" t/code-success-fg] ["✗" t/code-error-fg]]]
                (when-let [i (str/index-of raw pattern)]
                  ;; Find the end of this badge (next space or end)
                  (let [rest-str (subs raw i)
                        sp       (str/index-of rest-str " ")
                        badge    (if sp (subs rest-str 0 sp) rest-str)]
                    (p/set-colors! g color t/iter-header-bg)
                    (p/put-str! g (+ x i) y badge)))))

            ;; Thinking/reasoning — italic, dim
            (str/starts-with? line thinking-marker)
            (do (p/set-colors! g t/dialog-hint bg-color)
              (p/styled g [p/ITALIC]
                (p/put-str! g x y (subs line 1))))

            ;; Code with success status — code bg, green ✓ suffix
            (str/starts-with? line code-ok-marker)
            (let [raw (subs line 1)]
              (p/set-colors! g t/code-block-fg t/code-block-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/put-str! g x y raw)
              ;; Color the ✓ + duration suffix green
              (when-let [ci (str/index-of raw "✓")]
                (p/set-colors! g t/code-success-fg t/code-block-bg)
                (p/put-str! g (+ x ci) y (subs raw ci))))

            ;; Code with error status — code bg, red ✗ suffix
            (str/starts-with? line code-err-marker)
            (let [raw (subs line 1)]
              (p/set-colors! g t/code-block-fg t/code-block-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/put-str! g x y raw)
              ;; Color the ✗ + duration suffix red
              (when-let [ci (str/index-of raw "✗")]
                (p/set-colors! g t/code-error-fg t/code-block-bg)
                (p/put-str! g (+ x ci) y (subs raw ci))))

            ;; Code (no status yet — streaming) — code bg
            (str/starts-with? line code-marker)
            (do (p/set-colors! g t/code-block-fg t/code-block-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/set-colors! g t/code-block-fg t/code-block-bg)
              (p/put-str! g x y (subs line 1)))

            ;; Duration annotation — code bg, muted
            (str/starts-with? line duration-marker)
            (do (p/set-colors! g t/code-duration-fg t/code-block-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/set-colors! g t/code-duration-fg t/code-block-bg)
              (p/put-str! g x y (subs line 1)))

            ;; Success result — code bg, dim
            (str/starts-with? line result-marker)
            (do (p/set-colors! g t/code-result-fg t/code-block-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/set-colors! g t/code-result-fg t/code-block-bg)
              (p/put-str! g x y (subs line 1)))

            ;; Error result — code bg, red
            (str/starts-with? line err-result-marker)
            (do (p/set-colors! g t/code-error-result-fg t/code-block-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/set-colors! g t/code-error-result-fg t/code-block-bg)
              (p/put-str! g x y (subs line 1)))

            ;; Stdout — code bg, italic, dim
            (str/starts-with? line stdout-marker)
            (do (p/set-colors! g t/code-result-fg t/code-block-bg)
              (p/fill-rect! g (inc bx) y inner-w 1)
              (p/set-colors! g t/code-result-fg t/code-block-bg)
              (p/styled g [p/ITALIC]
                (p/put-str! g x y (subs line 1))))

            ;; Separator — dim
            (str/starts-with? line sep-marker)
            (do (p/set-colors! g t/dialog-hint bg-color)
              (p/put-str! g x y (subs line 1)))

            ;; Plain text (answer) — normal
            :else
            (do (p/set-colors! g fg-color bg-color)
              (p/put-str! g x y line)))))

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
      :successes [bool ...]}      ;; per-expression success? (nil = unknown / streaming)"
  [{:keys [thinking code results stdouts durations successes]} code-width iter-num]
  (let [bar (p/horiz-line 3)
        ;; Count successes/failures for the header badge
        total-exprs  (count (or code []))
        ok-count     (when successes (count (filter true? successes)))
        err-count    (when successes (count (filter false? successes)))
        badge        (cond
                       (and ok-count err-count (pos? total-exprs))
                       (str " ✓" ok-count
                         (when (pos? err-count) (str " ✗" err-count)))
                       (pos? total-exprs)
                       (str " " total-exprs " expr")
                       :else "")
        left-text    (str bar " iter " iter-num " ")
        right-text   (str badge " " bar)
        ;; Pad middle to fill code-width
        mid-len      (max 0 (- code-width (count left-text) (count right-text)))
        header-line  (str left-text (apply str (repeat mid-len \space)) right-text)
        header [(str iter-hdr-marker header-line)]

        thinking-lines
        (when (and (string? thinking) (not (str/blank? thinking)))
          (mapv #(str thinking-marker "> " %)
            (wrap-text (str/trim thinking) (max 1 (- code-width 2)))))

        code+result-lines
        (when (seq code)
          (into []
            (mapcat
              (fn [[idx form]]
                (let [success?  (when successes (get successes idx))
                      has-status? (some? success?)
                      dur-ms    (when durations (get durations idx))
                      dur-str   (channels/format-duration dur-ms)
                      ;; Status marker: ✓/✗ + duration on the first code line
                      status-suffix (when has-status?
                                      (str (if success? " ✓" " ✗")
                                        (when dur-str (str " " dur-str))))
                      ;; Pick marker based on status
                      c-marker  (cond
                                  (not has-status?) code-marker
                                  success?          code-ok-marker
                                  :else             code-err-marker)
                      ;; Code lines: '| ' prefix, wrapped
                      code-text (str/trim (or form ""))
                      wrapped   (wrap-text code-text (max 1 (- code-width 2
                                                              (if status-suffix
                                                                (+ 1 (count status-suffix))
                                                                0))))
                      ;; Append status suffix to last line of code
                      c-lines   (if (and status-suffix (seq wrapped))
                                  (let [last-idx (dec (count wrapped))
                                        last-line (nth wrapped last-idx)
                                        padded (str last-line status-suffix)]
                                    (mapv #(str c-marker "| " %)
                                      (assoc wrapped last-idx padded)))
                                  (mapv #(str c-marker "| " %) wrapped))
                      ;; Result
                      result-str (when results (get results idx))
                      is-error?  (and has-status? (not success?))
                      r-marker   (if is-error? err-result-marker result-marker)
                      r-lines (when (and result-str (not (str/blank? (str result-str))))
                                (mapv #(str r-marker "  => " %)
                                  (wrap-text (str/trim (str result-str))
                                    (max 1 (- code-width 5)))))
                      ;; Stdout — separator + padding + right-aligned "stdout" label + text + padding
                      stdout-str (when stdouts (get stdouts idx))
                      s-lines (when (and stdout-str (not (str/blank? (str stdout-str))))
                                (let [label   "stdout"
                                      sep-w   (max 1 (- code-width 2))
                                      sep-ln  (str stdout-sep-marker (apply str (repeat sep-w \-)))
                                      ;; First line: right-aligned "stdout" label
                                      label-pad (max 0 (- code-width (count label)))
                                      label-ln  (str stdout-marker (apply str (repeat label-pad \space)) label)
                                      text-lines (mapv #(str stdout-marker "  " %)
                                                   (wrap-text (str/trim (str stdout-str))
                                                     (max 1 (- code-width 2))))
                                      pad-ln  (str stdout-pad-marker "")]
                                  (into [sep-ln pad-ln label-ln] (conj text-lines pad-ln))))]
                  (concat c-lines r-lines s-lines))))
            (map-indexed vector code)))]
    (into (vec (concat header thinking-lines)) code+result-lines)))

(defn progress->text
  "Build the text body of the live progress placeholder bubble.

   `progress` is the `:progress` slot from app-db: `{:iterations [...]}`.
   `bubble-w` is the outer bubble width in chars — we size inner content
   conservatively so wrap-text inside draw-chat-bubble! doesn't blow up.
   `settings` is the display settings map: `{:show-thinking bool :show-iterations bool}`.
   Returns a single string with newlines separating lines.

   Returns the literal \"...\" string when the progress timeline is
   empty, so the placeholder still shows *something* in the first 100ms
   before the first chunk arrives."
  [progress bubble-w settings]
  (let [iterations (:iterations progress)
        content-w  (max 10 (- bubble-w 4))
        show-thinking?  (get settings :show-thinking true)
        show-iterations? (get settings :show-iterations true)]
    (if (empty? iterations)
      "..."
      (if-not show-iterations?
        ;; When iterations hidden, just show a compact status line
        (let [n (count iterations)
              last-iter (last iterations)
              thinking? (and show-thinking?
                          (some? (:thinking last-iter))
                          (not (str/blank? (:thinking last-iter))))]
          (if thinking?
            (str "thinking... (iter " n ")")
            (str "working... (iter " n ")")))
        (str/join "\n"
          (into []
            (mapcat (fn [[idx entry]]
                      (format-iteration-entry
                        (if show-thinking? entry (dissoc entry :thinking))
                        content-w (inc idx))))
            (map-indexed vector iterations)))))))

(defn format-answer-with-thinking
  "Build the final bubble text: thinking trace + answer.
   `trace` is the progress iterations vec [{:thinking :code :results} ...].
   `settings` is `{:show-thinking bool :show-iterations bool}`.
   Trace is visually separated from the answer by a dashed line."
  ([answer trace bubble-w] (format-answer-with-thinking answer trace bubble-w nil))
  ([answer trace bubble-w settings]
   (let [content-w (max 10 (- bubble-w 4))
         show-thinking?   (get settings :show-thinking true)
         show-iterations? (get settings :show-iterations true)
         trace-lines (when (and show-iterations? (seq trace))
                       (into []
                         (mapcat (fn [[idx entry]]
                                   (format-iteration-entry
                                     (if show-thinking? entry (dissoc entry :thinking))
                                     content-w (inc idx))))
                         (map-indexed vector trace)))
         answer-str (or answer "")]
     (if (seq trace-lines)
       (let [sep (str sep-marker (p/horiz-line (min 40 content-w)))]
         (str (str/join "\n" trace-lines) "\n" sep "\n" answer-str))
       answer-str))))

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
