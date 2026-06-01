(ns com.blockether.vis.ext.channel-tui.dialogs
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.boxed-table :as boxed-table]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.theme :as shared-theme]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.file-picker :as picker]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import [com.googlecode.lanterna Symbols TerminalPosition]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
           [java.text SimpleDateFormat]
           [java.util Locale TimeZone]))
;;; ── Shared dialog chrome & components ───────────────────────────────────────
(defn- input-field-bg [] t/input-field-bg)
;;; ── Default modal footprint ─────────────────────────────────────────────────
;;
;; Every modal in the TUI now shares ONE default WIDTH and ONE default
;; HEIGHT. Users asked for the dialog stack to stop "breathing" as they
;; moved between Settings / Providers / confirm / input popups, so the
;; default arity of `draw-dialog-chrome!` ignores caller-specific body
;; height and uses a common terminal-proportional footprint instead.
;;
;; Dialogs that genuinely need a bespoke size can still call the fully
;; explicit width+height arity, but the common path stays uniform.
(defn default-content-width
  "Shared content width every dialog uses, derived from `cols`. Clamped
   between the theme's dialog min/max widths and bounded by the terminal so
   the box never paints off-screen."
  [cols]
  (let [terminal-w (max 40 (- cols 4))
        min-w (min t/dialog-min-width terminal-w)
        box-w (-> (int (* cols t/dialog-width-ratio))
                (max min-w)
                (min t/dialog-max-width)
                (min terminal-w))]
    (max 1 (- box-w t/dialog-chrome-w))))
(defn default-content-height
  "Shared content height every dialog uses, derived from `rows`.
   Clamped to a common modal footprint so dialogs keep equal height."
  [rows]
  (let [terminal-h (max 8 (- rows 4))
        min-h (min t/dialog-min-height terminal-h)
        box-h (-> (int (* rows t/dialog-height-ratio))
                (max min-h)
                (min t/dialog-max-height)
                (min terminal-h))]
    (max 1 (- box-h t/dialog-chrome-h))))
(defn clear-screen!
  "Fill the entire screen with terminal background. Call before sub-dialogs
   to cleanly replace the current dialog (wizard step pattern)."
  [^TerminalScreen screen]
  (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
        cols (.getColumns size)
        rows (.getRows size)
        g (.newTextGraphics screen)]
    (p/set-bg! g t/terminal-bg)
    (p/fill-rect! g 0 0 cols rows)
    (.refresh screen Screen$RefreshType/DELTA)))
(defn clamp [x lo hi] (max lo (min hi x)))
(defn ellipsize
  [s max-w]
  (let [txt (or s "")
        max-w (long max-w)]
    (cond (<= max-w 0) ""
      (<= (p/display-width txt) max-w) txt
      (= max-w 1) "..."
      :else (str (p/truncate-cols txt (dec max-w)) "..."))))
(defn dialog-layout
  "Compute content area layout. When `content-count` is provided and smaller than
   the available height, content is vertically centered within the frame.
   Layout: border -> title bar -> top separator -> CONTENT -> bottom separator -> hint -> border."
  ([bounds] (dialog-layout bounds nil))
  ([{:keys [top bottom]} content-count]
   (let [raw-top (+ top 3)
         hint-row (- bottom 1)
         bot-sep-row (- bottom 2)
         content-bot (dec bot-sep-row)
         full-h (max 1 (inc (- content-bot raw-top)))
         v-offset
         (if (and content-count (< content-count full-h)) (quot (- full-h content-count) 2) 0)
         content-top (+ raw-top v-offset)
         ;; Usable height from centered top - never exceeds content-bot
         content-h (max 1 (inc (- content-bot content-top)))]
     {:content-top content-top,
      :content-bottom content-bot,
      :content-h content-h,
      :hint-row hint-row})))
(defn visible-window-start
  [idx current-start visible-count total-count]
  (let [last-start (max 0 (- total-count visible-count))
        start (clamp current-start 0 last-start)]
    (cond (< idx start) idx
      (>= idx (+ start visible-count)) (max 0 (- idx (dec visible-count)))
      :else start)))
(defn modal-wheel-delta
  "Return list-selection delta for a wheel mouse event, else nil.
   Negative moves up; positive moves down."
  [key]
  (when (instance? MouseAction key)
    (let [action (.getActionType ^MouseAction key)]
      (cond (= action MouseActionType/SCROLL_UP) -1
        (= action MouseActionType/SCROLL_DOWN) 1
        :else nil))))
(defn modal-wheel-step
  "Return wheel delta multiplied by any coalesced event count."
  [key]
  (when-let [delta (modal-wheel-delta key)]
    (* (long delta) (max 1 (long (.getButton ^MouseAction key))))))
(def ^:private modal-pending-key (ThreadLocal/withInitial #(atom nil)))
(defn normalize-modal-key
  "Normalize raw terminal CR/LF/ESC character keystrokes to Lanterna
   Enter/Escape key types. Some terminals surface modal Enter/Escape as
   `KeyType/Character`; modal code should not need to special-case that."
  [key]
  (if (and key (not (instance? MouseAction key)) (= KeyType/Character (.getKeyType key)))
    (case (.getCharacter key)
      (\newline \return) (KeyStroke. KeyType/Enter)
      \u001B (KeyStroke. KeyType/Escape)
      key)
    key))
(defn modal-enter-key?
  [key]
  (let [key (normalize-modal-key key)]
    (and key (not (instance? MouseAction key)) (= KeyType/Enter (.getKeyType key)))))
(defn modal-escape-key?
  [key]
  (let [key (normalize-modal-key key)]
    (and key (not (instance? MouseAction key)) (= KeyType/Escape (.getKeyType key)))))
(defn read-modal-input!
  "Read one modal input event. Consecutive pending wheel events are drained
   and returned as one `:scroll-delta`, so a wheel flood costs one redraw.
   The first non-wheel event encountered while draining is held for the next
   modal read on this thread."
  [^TerminalScreen screen]
  (let [pending-key (.get ^ThreadLocal modal-pending-key)
        key (normalize-modal-key (or @pending-key (.readInput screen)))]
    (reset! pending-key nil)
    (if-let [delta (modal-wheel-delta key)]
      (loop [acc (long delta)]
        (if-let [next-key (some-> (.pollInput screen)
                            normalize-modal-key)]
          (if-let [next-delta (modal-wheel-delta next-key)]
            (recur (+ acc (long next-delta)))
            (do (reset! pending-key next-key) {:scroll-delta acc}))
          {:scroll-delta acc}))
      {:key key})))
(defn read-modal-key!
  "Like `Screen/readInput`, but drains wheel floods into one synthetic wheel
   event. Existing modal loops can use it without bespoke scroll-delta code."
  ^KeyStroke [^TerminalScreen screen]
  (let [{:keys [key scroll-delta]} (read-modal-input! screen)]
    (or key
      (when scroll-delta
        (MouseAction.
          (if (neg? (long scroll-delta)) MouseActionType/SCROLL_UP MouseActionType/SCROLL_DOWN)
          1
          (TerminalPosition. 0 0))))))
(defn drain-modal-paste!
  "After a bracketed-paste START keystroke is seen, drain `screen` until
   PASTE_END and return the pasted text (PUA markers stripped). Lets any
   modal text input accept clipboard paste without re-implementing the
   paste state machine. Returns \"\" on a clipboard that yields no chars."
  ^String [^TerminalScreen screen]
  (let [sb (StringBuilder.)]
    (loop []
      (let [k (read-modal-key! screen)]
        (cond
          (nil? k)            (.toString sb)
          (input/paste-end? k) (.toString sb)
          :else (do (when-let [ch (input/keystroke->paste-char k)]
                      (.append sb ^String ch))
                  (recur)))))))
(defn draw-hint-bar!
  "Draw hint bar. `hint` can be:
   - a string: rendered as-is
   - a vec of strings: spread with space-between
   - a vec of [key action] pairs: key in italic, action normal, spread with space-between
   Examples:
     \"simple hint\"
     [\"↑/↓ move\" \"Enter select\" \"Esc cancel\"]
     [[\"↑/↓\" \"move\"] [\"Enter\" \"select\"] [\"Esc\" \"cancel\"]]"
  [g left row inner-w hint]
  (let [text-w (max 0 (- inner-w 2))
        text-x (+ left 2)]
    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (cond
      ;; Plain string
      (string? hint) (p/put-str! g text-x row (ellipsize hint text-w))
      ;; Vec of [key action] pairs - render keys in italic
      (and (vector? hint) (seq hint) (vector? (first hint)))
      (let [labels (mapv (fn [[k a]] (str k " " a)) hint)
              ;; Walk through the laid-out string to find each key's position
              ;; Simpler: compute column offsets from space-between math
            n (count labels)
            total-txt (reduce + (map count labels))
            total-gap (- text-w total-txt)
            gap-count (max 1 (dec n))
            base-gap (max 1 (quot total-gap gap-count))
            extra (- total-gap (* base-gap gap-count))]
        (loop [i 0
               col text-x]
          (when (< i n)
            (let [[k a] (nth hint i)
                  gap (if (< i (dec n)) (+ base-gap (if (< i extra) 1 0)) 0)]
                ;; Key part - bold, stronger color
              (p/set-fg! g t/dialog-hint-key)
              (p/styled g [p/BOLD] (p/put-str! g col row k))
                ;; Action part - normal hint color, italic
              (p/set-fg! g t/dialog-hint)
              (p/styled g [p/ITALIC] (p/put-str! g (+ col (count k)) row (str " " a)))
              (recur (inc i) (+ col (count k) 1 (count a) gap))))))
      ;; Vec of strings - space-between, all italic
      (vector? hint) (p/styled g [p/ITALIC] (p/draw-space-between! g text-x row text-w hint)))))
(defn- draw-list-item!
  ;; Selection visual:
  ;;   col left   : │ (frame, painted by chrome)
  ;;   col left+1 : `>` cursor glyph (or blank if not selected)
  ;;   col left+2 : ` ` margin between marker and body
  ;;   col left+3+: body label (BOLD on selected)
  ;;
  ;; The 2-col `selection-prefix` (`> ` / `  `) is concatenated to the
  ;; label and the whole string is drawn at `(inc left)` so the marker
  ;; lands RIGHT AT the inner edge of the dialog (no padding column
  ;; between the frame and the marker), then a 1-col margin, then the
  ;; label — matching the project-wide `>`-cursor convention.
  [g left row inner-w selected? label]
  (let [prefix (p/selection-prefix selected?)
        draw-text (ellipsize (str prefix label) (max 0 (- inner-w 2)))]
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (if selected?
      (p/styled g [p/BOLD] (p/put-str! g (inc left) row draw-text))
      (p/put-str! g (inc left) row draw-text))))
(defn- draw-checkbox-item!
  ;; `> [✓] label` when selected, `  [✓] label` otherwise. The cursor
  ;; glyph and the checkbox glyph carry independent meaning: the
  ;; first says "this row is the cursor", the second says "this
  ;; option is currently on". Drop the accent-bg highlight — the `>`
  ;; alone is the selection cue. Anchored at `(inc left)` so the
  ;; marker sits right at the dialog's inner edge (see `draw-list-item!`).
  [g left row inner-w selected? checked? label]
  (let [mark (if checked? "✓" " ")
        prefix (str (p/selection-prefix selected?) "[" mark "] ")
        draw-text (ellipsize (str prefix label) (max 0 (- inner-w 2)))]
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (if selected?
      (p/styled g [p/BOLD] (p/put-str! g (inc left) row draw-text))
      (p/put-str! g (inc left) row draw-text))))
(defn- draw-text-input-field!
  [g left row inner-w text cursor]
  (let [box-left (+ left 2)
        box-w (max 3 (- inner-w 2))
        input-left (inc box-left)
        input-w (max 1 (- box-w 2))
        h-off (max 0 (- cursor (dec input-w)))
        visible (subs text h-off (min (count text) (+ h-off input-w)))]
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/draw-box! g box-left row box-w 3)
    (p/set-colors! g t/box-fg (input-field-bg))
    (p/fill-rect! g input-left (inc row) input-w 1)
    (p/put-str! g input-left (inc row) visible)
    (p/cursor-pos (+ input-left (- cursor h-off)) (inc row))))
(defn draw-dialog-chrome!
  "Draw dialog background, shadow, border, and title.

   Three arities:
   - `(g cols rows title content-h)` - shared default width and height.
     Caller-supplied `content-h` is ignored in this arity on purpose;
     the whole TUI dialog stack now uses one common footprint.
   - `(g cols rows title content-w content-h)` - fully explicit. Use
     only when a dialog genuinely needs a non-default width.

   Returns {:left :top :right :bottom :inner-w :inner-h}."
  ([g cols rows title _content-h]
   (draw-dialog-chrome! g
     cols
     rows
     title
     (default-content-width cols)
     (default-content-height rows)))
  ([g cols rows title content-w content-h]
   (let [[box-w box-h] (render/golden-dialog-size cols rows content-w content-h)
         box-left (quot (- cols box-w) 2)
         box-top (quot (- rows box-h) 2)
         box-right (+ box-left box-w -1)
         box-bottom (+ box-top box-h -1)
         inner-w (- box-w 2)]
     ;; Shadow - clipped to terminal bounds
     (let [shd-left (+ box-left 2)
           shd-top (inc box-top)
           shd-w (min box-w (- cols shd-left))
           shd-h (min box-h (- rows shd-top))]
       (when (and (pos? shd-w) (pos? shd-h))
         (p/set-bg! g t/dialog-shadow)
         (p/fill-rect! g shd-left shd-top shd-w shd-h)))
     ;; Background
     (p/set-bg! g t/dialog-bg)
     (p/fill-rect! g box-left box-top box-w box-h)
     ;; Border
     (p/set-colors! g t/dialog-border t/dialog-bg)
     (p/draw-box! g box-left box-top box-w box-h)
     ;; Title bar - full-width accent stripe with centered title
     (let [title-row (inc box-top)
           title-text (ellipsize (or title "") (max 0 (- inner-w 2)))
           tx (+ box-left 1 (quot (- inner-w (count title-text)) 2))]
       ;; Accent bar background
       (p/set-bg! g t/dialog-title-bg)
       (p/fill-rect! g (inc box-left) title-row inner-w 1)
       ;; Title text
       (p/set-fg! g t/dialog-title-fg)
       (p/put-str! g tx title-row title-text)
       ;; Top separator - below title bar
       (p/set-colors! g t/dialog-border t/dialog-bg)
       (p/draw-separator! g box-left box-right (inc title-row))
       ;; Bottom separator - above hint bar
       (let [bot-sep (- box-bottom 2)]
         (when (> bot-sep (+ box-top 3)) (p/draw-separator! g box-left box-right bot-sep))))
     {:left box-left,
      :top box-top,
      :right box-right,
      :bottom box-bottom,
      :inner-w inner-w,
      :inner-h (- box-h 2)})))
;;; ── Selection dialog ────────────────────────────────────────────────────────
(defn select-dialog!
  "Show a selection list dialog. Returns selected item map or nil on Esc.
   `items` is a vec of {:label str, ...} maps."
  [^TerminalScreen screen title items]
  (let [selected (atom 0)
        scroll (atom 0)
        ch (count items)]
    (loop []
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title ch)
            {:keys [left inner-w]} bounds
            total (count items)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
            visible (min total content-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % content-h total))]
        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]
            (when (< idx total)
              (draw-list-item! g
                left
                row
                (if (> total content-h) (dec inner-w) inner-w)
                (= idx @selected)
                (:label (nth items idx))))))
        (scrollbar/draw! g
          {:col (+ left inner-w),
           :top content-top,
           :track-h content-h,
           :total-h total,
           :inner-h content-h,
           :scroll @scroll})
        (draw-hint-bar! g
          left
          hint-row
          inner-w
          [["↑/↓" "move"] ["Enter" "select"] ["Esc" "cancel"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (if-let [wheel-step (modal-wheel-step key)]
              (do (swap! selected #(clamp (+ % wheel-step) 0 (max 0 (dec total)))) (recur))
              (condp = (.getKeyType key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                    (recur))
                KeyType/Enter (when (pos? total) (nth items @selected))
                (recur)))))))))
;;; ── Resources dialog ────────────────────────────────────────────────────────
(defn- markdown-link-row [{:keys [text url]}] (str "- [" (or text "Resource") "](" (or url "") ")"))
(defn- resource-open-action?
  [action]
  (or (= MouseActionType/CLICK_DOWN action) (= MouseActionType/CLICK_RELEASE action)))
(defn- resource-row-label
  [selected? title max-w]
  (ellipsize (str (p/selection-prefix selected?) "[" (or title "Resource") "]") max-w))
(defn resource-dialog-items
  "Build selectable rows for a resources popup from renderer refs.
   Keeps the original ref fields so callers can open `:url` after
   selection, and also preserves a Markdown link row for copy/readback."
  [refs]
  (mapv (fn [{:keys [display text url], :as ref}]
          (assoc ref
            :markdown (markdown-link-row ref)
            :label (or display
                     (str (or text "Resource") (when-not (str/blank? url) (str " -> " url))))))
    refs))
(defn- draw-resource-item!
  ;; Selection visual: leading `> ` glyph from `resource-row-label`,
  ;; anchored at `(inc left)` so the marker lands right at the
  ;; dialog's inner edge (see `draw-list-item!` for the layout map).
  ;; Link-chrome colors are kept on every row so URL/badge stays
  ;; readable; previously the inverse-on-accent path squashed both
  ;; into `dialog-bg` and the URL hint vanished.
  [g left row inner-w selected? {:keys [text url kind]}]
  (let [title (or text "Resource")
        target (or url "")
        badge (case kind
                :image "image"
                :file "file"
                :info "detail"
                "link")
        body-w (max 1 (- inner-w 4))
        target-w (min 32 (max 0 (- body-w (count title) 10)))
        target-txt (when (pos? target-w) (ellipsize target target-w))
        label (resource-row-label selected? title body-w)]
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (let [x (inc left)]
      ;; Bracketed title in link color (BOLD on selected for cursor cue).
      (p/set-colors! g t/link-chrome-fg t/dialog-bg)
      (if selected? (p/styled g [p/BOLD] (p/put-str! g x row label)) (p/put-str! g x row label))
      ;; Badge + URL tail in dimmed hint color.
      (when target-txt
        (p/set-colors! g t/dialog-hint t/dialog-bg)
        (p/put-str! g (+ x (count label) 1) row (str "(" badge ") " target-txt))))))
(defn resources-dialog!
  "Show a Markdown-style clickable resources popup and return the
   selected ref, or nil on Esc. Enter opens the highlighted row;
   mouse click opens the row under the cursor."
  [^TerminalScreen screen refs]
  (let [items (resource-dialog-items refs)
        selected (atom 0)
        scroll (atom 0)]
    (when (seq items)
      (loop []
        (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
              cols (.getColumns size)
              rows (.getRows size)
              g (.newTextGraphics screen)
              bounds (draw-dialog-chrome! g cols rows "Resources" (count items))
              {:keys [left inner-w]} bounds
              total (count items)
              {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
              visible (min total content-h)
              _ (swap! selected #(clamp % 0 (max 0 (dec total))))
              _ (swap! scroll #(visible-window-start @selected % content-h total))]
          (dotimes [i visible]
            (let [idx (+ @scroll i)
                  row (+ content-top i)]
              (when (< idx total)
                (draw-resource-item! g left row inner-w (= idx @selected) (nth items idx)))))
          (draw-hint-bar! g
            left
            hint-row
            inner-w
            [["↑/↓" "move"] ["Enter/click" "open"] ["Esc" "cancel"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (.refresh screen Screen$RefreshType/DELTA)
          (let [key (read-modal-key! screen)]
            (when key
              (cond (= KeyType/Escape (.getKeyType key)) nil
                (= KeyType/ArrowUp (.getKeyType key))
                (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                (= KeyType/ArrowDown (.getKeyType key))
                (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
                (= KeyType/Enter (.getKeyType key)) (nth items @selected)
                (instance? MouseAction key)
                (let [^MouseAction m key
                      pos (.getPosition m)
                      mx (.getColumn pos)
                      my (.getRow pos)
                      hit (when (and (<= content-top my)
                                  (< my (+ content-top visible))
                                  (<= (inc left) mx)
                                  (< mx (+ left inner-w)))
                            (+ @scroll (- my content-top)))]
                  (if (and hit (< hit total) (resource-open-action? (.getActionType m)))
                    (nth items hit)
                    (do (when (and hit (< hit total)) (reset! selected hit)) (recur))))
                :else (recur)))))))))
;;; ── Read-only text viewer dialog ────────────────────────────────────────────
(defn text-view-dialog!
  "Show read-only lines in a scrollable modal. Returns nil after close."
  [^TerminalScreen screen title lines]
  (let [scroll (atom 0)]
    (loop []
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title (max 8 (count lines)))
            {:keys [left inner-w]} bounds
            text-w (max 1 (- inner-w 2))
            wrapped
            (vec (mapcat (fn [line]
                           (if (str/blank? (str line)) [""] (render/wrap-text (str line) text-w)))
                   (or lines [])))
            total (count wrapped)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
            visible (min total content-h)
            max-scroll (max 0 (- total visible))
            _ (swap! scroll #(clamp % 0 max-scroll))]
        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]
            (when (< idx total)
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/put-str! g (+ left 2) row (ellipsize (nth wrapped idx) text-w)))))
        (scrollbar/draw! g
          {:col (+ left inner-w),
           :top content-top,
           :track-h content-h,
           :total-h total,
           :inner-h content-h,
           :scroll @scroll})
        (draw-hint-bar! g left hint-row inner-w [["↑/↓" "scroll"] ["Enter/Esc" "close"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape nil
              KeyType/Enter nil
              KeyType/ArrowUp (do (swap! scroll dec) (recur))
              KeyType/ArrowDown (do (swap! scroll inc) (recur))
              (recur))))))))
;;; ── File picker dialog ──────────────────────────────────────────────────────
(defn- open-picker-item!
  ([item] (open-picker-item! item workspace/*workspace-root*))
  ([{:keys [path]} workspace-root]
   (vis/worker-future "vis-tui-open-picker-item"
     #(binding [workspace/*workspace-root* workspace-root]
        (try (opener/open! path)
          (catch Throwable t
            {:status :spawn-failed,
             :command nil,
             :scheme nil,
             :target path,
             :error (.getMessage t)}))))))
(def ^:private file-picker-table-headers ["Status" "File" "Size" "Modified"])
(defn- file-picker-table-widths
  [table-w]
  (let [overhead (inc (* 3 (count file-picker-table-headers)))
        available (max (count file-picker-table-headers) (- table-w overhead))]
    (if (>= available 32)
      (let [status-w 9
            size-w 7
            modified-w 8
            file-w (max 1 (- available status-w size-w modified-w))]
        [status-w file-w size-w modified-w])
      (let [status-w (max 1 (min 6 (quot available 4)))
            size-w 1
            modified-w 1
            file-w (max 1 (- available status-w size-w modified-w))]
        [status-w file-w size-w modified-w]))))
(defn- file-picker-table-cells
  [{:keys [status-label path size-label age-label]}]
  [status-label path size-label age-label])
(defn- reset-picker-cursor!
  "Reset selection + scroll to top. Used after filter/sort/ignore changes
   so cursor doesn't dangle past the new result set."
  [selected scroll]
  (reset! selected 0)
  (reset! scroll 0))
(defn file-picker-dialog!
  "Interactive `@` file picker. Type to filter repo files, Enter inserts
   the selected relative path, Esc cancels. `Alt+I` toggles ignored files;
   `Alt+S` cycles sort mode; `Alt+O` opens the selection externally."
  [^TerminalScreen screen]
  (let [workspace-root workspace/*workspace-root*
        entries (picker/collect-file-picker-entries)
        query (atom "")
        include-ignored? (atom false)
        sort-mode (atom :auto)
        selected (atom 0)
        scroll (atom 0)]
    (loop []
      (let [items (picker/file-picker-items entries
                    @query
                    {:include-ignored? @include-ignored?,
                     :sort-mode @sort-mode})
            total (count items)
            size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows "Attach File" nil)
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            table-top (+ content-top 4)
            list-h (max 1 (- content-h 8))
            _ (swap! scroll #(visible-window-start @selected % list-h total))
            geom (boxed-table/layout bounds)
            {:keys [table-content-w]} geom
            widths (file-picker-table-widths table-content-w)]
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc left) content-top inner-w content-h)
        (let [cursor-pos (draw-text-input-field! g left content-top inner-w @query (count @query))]
          (boxed-table/draw! g
            {:bounds      bounds
             :top         table-top
             :body-h      list-h
             :headers     file-picker-table-headers
             :widths      widths
             :total       total
             :scroll      @scroll
             :selected    @selected
             :closed?     true
             :cell-fn     (fn [idx] (file-picker-table-cells (nth items idx)))
             :empty-cells ["" "No matching files." "" ""]})
          ;; Footer carries every modifier so the dialog body stays
          ;; pure table. Pairs follow navigator's pattern: the key in
          ;; bold + the CURRENT effective action label, so the user
          ;; can see at a glance whether ignored is on/off and which
          ;; sort mode is active.
          (draw-hint-bar! g
            left
            hint-row
            inner-w
            [["type" "filter"] ["↑/↓" "move"] ["Enter" "attach"]
             ["Alt+I" (str "ignored " (if @include-ignored? "on" "off"))]
             ["Alt+S" (str "sort " (picker/sort-label @sort-mode @query))]
             ["Alt+O" "open"] ["Esc" "cancel"]])
          (.setCursorPosition screen cursor-pos))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (cond
              (instance? MouseAction key)
              (let [^MouseAction ma key
                    atype (.getActionType ma)
                    pos (.getPosition ma)
                    mx (.getColumn pos)
                    my (.getRow pos)
                    hit-idx (boxed-table/hit-row geom table-top list-h @scroll mx my)]
                (cond (= atype MouseActionType/SCROLL_UP)
                  (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                  (= atype MouseActionType/SCROLL_DOWN)
                  (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
                  (and (= atype MouseActionType/CLICK_DOWN) (some? hit-idx) (< hit-idx total))
                  (do (reset! selected hit-idx) (recur))
                  (and (= atype MouseActionType/CLICK_RELEASE)
                    (some? hit-idx)
                    (< hit-idx total))
                  (:path (nth items hit-idx))
                  :else (recur)))

              ;; Clipboard paste → append into the filter query (collapse
              ;; whitespace; markers never reach the query string).
              (input/paste-start? key)
              (do (let [pasted (drain-modal-paste! screen)]
                    (when (seq pasted)
                      (swap! query str (str/trim (str/replace pasted #"\s+" " ")))
                      (reset-picker-cursor! selected scroll)))
                (recur))

              :else (condp = (.getKeyType key)
                      KeyType/Escape nil
                      KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total))))
                                        (recur))
                      KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                          (recur))
                      KeyType/Backspace (do (swap! query #(if (seq %) (subs % 0 (dec (count %))) %))
                                          (reset-picker-cursor! selected scroll)
                                          (recur))
                      KeyType/Enter (when (pos? total) (:path (nth items @selected)))
                      KeyType/Character
                      (let [raw-c (.getCharacter key)]
                        (cond (input/alt-char? key \i) (do (swap! include-ignored? not)
                                                         (reset-picker-cursor! selected scroll)
                                                         (recur))
                          (input/alt-char? key \s) (do (swap! sort-mode picker/cycle-sort-mode)
                                                     (reset-picker-cursor! selected scroll)
                                                     (recur))
                          (input/alt-char? key \o)
                          (do (when (pos? total)
                                (open-picker-item! (nth items @selected) workspace-root))
                            (recur))
                          (Character/isISOControl raw-c) (recur)
                          :else (do (swap! query str raw-c)
                                  (reset-picker-cursor! selected scroll)
                                  (recur))))
                      (recur)))))))))
;;; ── Text input dialog ───────────────────────────────────────────────────────
(defn- text-input-body-lines
  [body]
  (cond (nil? body) []
    (string? body) (str/split-lines body)
    (sequential? body) (mapv str body)
    :else [(str body)]))
(defn text-input-dialog!
  "Show a text input dialog. Returns string or nil on Esc.
   Options: :mask char (e.g. \\* for passwords), :initial string,
   :body string-or-lines rendered above the input label."
  [^TerminalScreen screen title label & {:keys [mask initial body], :or {initial ""}}]
  (let [text (atom (vec initial))
        cursor (atom (count initial))
        body-lines (text-input-body-lines body)
        paste-buffer (volatile! nil)]
    (loop []
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            ;; Content: optional body rows + label row + spacer + 3-row bordered input box.
            bounds (draw-dialog-chrome! g cols rows title 5)
            {:keys [left inner-w]} bounds
            text-w (max 1 (- inner-w 2))
            wrapped-body (->> body-lines
                           (mapcat (fn [line]
                                     (if (str/blank? line) [""] (render/wrap-text line text-w))))
                           vec)
            body-gap (if (seq wrapped-body) 1 0)
            content-count (+ 4 body-gap (count wrapped-body))
            {:keys [content-top content-h hint-row]} (dialog-layout bounds content-count)
            max-body-lines (max 0 (- content-h 4 body-gap))
            visible-body (if (<= (count wrapped-body) max-body-lines)
                           wrapped-body
                           (conj (vec (take (max 0 (dec max-body-lines)) wrapped-body)) "..."))
            label-row (+ content-top (count visible-body) body-gap)
            input-row (inc label-row)
            txt (apply str @text)
            display (if mask (apply str (repeat (count txt) mask)) txt)
            cursor-pos (draw-text-input-field! g left input-row inner-w display @cursor)]
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[idx line] (map-indexed vector visible-body)]
          (let [row (+ content-top idx)]
            (p/fill-rect! g (inc left) row inner-w 1)
            (p/put-str! g (+ left 2) row (ellipsize line text-w))))
        (p/fill-rect! g (inc left) label-row inner-w 1)
        (p/put-str! g (+ left 2) label-row (ellipsize label (max 0 (- inner-w 2))))
        (draw-hint-bar! g
          left
          hint-row
          inner-w
          [["<-/->" "move"] ["Enter" "confirm"] ["Esc" "cancel"]])
        (.setCursorPosition screen cursor-pos)
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (cond
              ;; ── Bracketed paste ──────────────────────────────
              ;; Three-state machine matching the main input loop.
              ;; START -> open buffer; END -> flush into text.
              ;; Prevents PUA marker chars (\uE200, \uE201) from
              ;; leaking into the dialog value - they break HTTP
              ;; Authorization headers when pasted API keys carry
              ;; them into the Bearer token.
              (input/paste-start? key) (do (vreset! paste-buffer (StringBuilder.)) (recur))
              (input/paste-end? key) (let [^StringBuilder sb @paste-buffer]
                                       (when sb
                                         (let [payload (.toString sb)
                                               chars (vec payload)]
                                           (vreset! paste-buffer nil)
                                           (when-not (.isEmpty payload)
                                             (swap! text (fn [t]
                                                           (into (subvec t 0 @cursor)
                                                             (concat chars
                                                               (subvec t @cursor)))))
                                             (swap! cursor + (count chars)))))
                                       (recur))
              ;; Accumulate chars into the paste buffer while open.
              (some? @paste-buffer) (do (when-let [ch (input/keystroke->paste-char key)]
                                          (.append ^StringBuilder @paste-buffer ch))
                                      (recur))
              ;; ── Regular key dispatch ─────────────────────────
              :else (condp = (.getKeyType key)
                      KeyType/Escape nil
                      KeyType/Enter (str/trim (apply str @text))
                      KeyType/Character (let [c (.getCharacter key)]
                                          (swap! text #(into (subvec % 0 @cursor)
                                                         (cons c (subvec % @cursor))))
                                          (swap! cursor inc)
                                          (recur))
                      KeyType/Backspace (do (when (pos? @cursor)
                                              (swap! text #(into (subvec % 0 (dec @cursor))
                                                             (subvec % @cursor)))
                                              (swap! cursor dec))
                                          (recur))
                      KeyType/ArrowLeft (do (swap! cursor #(max 0 (dec %))) (recur))
                      KeyType/ArrowRight (do (swap! cursor #(min (count @text) (inc %))) (recur))
                      (recur)))))))))
;;; ── Confirm dialog ──────────────────────────────────────────────────────────
(defn- draw-button!
  "Draw a button label. Selected = accent bg, normal = dialog bg."
  [g col row label selected?]
  (let [text (str " " label " ")
        w (count text)]
    (if selected?
      (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/put-str! g col row text)
    w))
(defn confirm-dialog!
  "Show Y/N confirmation with side-by-side buttons. Returns true/false, nil on Esc."
  [^TerminalScreen screen title message]
  (let [raw-lines (if (string? message) [message] message)
        btn-yes "Yes"
        btn-no "No"
        btn-w (+ 2 (max (count btn-yes) (count btn-no))) ;; " Yes " / " No  "
        btn-gap 4
        ;; content: message lines + blank + button row = lines + 2
        ch (+ (count raw-lines) 2)
        focus (atom 0)] ;; 0 = Yes, 1 = No
    (loop []
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title ch)
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds ch)
            text-w (max 0 (- inner-w 2))
            lines (vec (mapcat #(render/wrap-text % text-w) raw-lines))
            btn-row (+ content-top (count lines) 1) ;; blank line then buttons
            ;; Center buttons horizontally
            total-btn-w (+ btn-w btn-gap btn-w)
            btn-start (+ left 1 (quot (- inner-w total-btn-w) 2))]
        ;; Message text - centered per line
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[i line] (map-indexed vector lines)]
          (let [row (+ content-top i)]
            (when (< row (+ content-top content-h))
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/draw-centered! g (inc left) row inner-w line))))
        ;; Buttons - side by side
        (p/set-bg! g t/dialog-bg)
        (p/fill-rect! g (inc left) btn-row inner-w 1)
        (draw-button! g btn-start btn-row btn-yes (= @focus 0))
        (draw-button! g (+ btn-start btn-w btn-gap) btn-row btn-no (= @focus 1))
        (draw-hint-bar! g
          left
          hint-row
          inner-w
          [["<-/->" "switch"] ["Enter" "confirm"] ["Esc" "cancel"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape nil
              KeyType/Enter (= @focus 0) ;; true if Yes focused
              KeyType/ArrowLeft (do (reset! focus 0) (recur))
              KeyType/ArrowRight (do (reset! focus 1) (recur))
              KeyType/Tab (do (swap! focus #(if (zero? %) 1 0)) (recur))
              KeyType/Character (let [c (Character/toLowerCase (.getCharacter key))]
                                  (cond (= c \y) true
                                    (= c \n) false
                                    :else (recur)))
              (recur))))))))
;;; ── Settings dialog ─────────────────────────────────────────────────────────
(def ^:private settings-model-no-effort-rows
  [{:type :info,
    :label "Reasoning effort unavailable",
    :description "The active model exposes fixed/binary thinking only."}])
(defn- current-model-info
  []
  (when-let [router (try (vis/get-router) (catch Throwable _ nil))]
    (try (vis/resolve-effective-model router) (catch Throwable _ nil))))
(defn- current-provider-id [] (:provider (current-model-info)))
(defn- reasoning-effort-configurable?
  []
  (let [info (current-model-info)]
    (or (nil? info)
      (and (boolean (:reasoning? info))
        (not= false (:reasoning-effort? info))
        (not= :zai-thinking (:reasoning-style info))))))
(defn- theme-choice-order
  []
  (try (mapv keyword (shared-theme/available-theme-ids))
    (catch Throwable _ [(keyword shared-theme/default-theme-id)])))
(defn- settings-ui-options
  "Local-only settings the TUI still owns (theme + dynamic choices).
   Every former boolean / enum (show-thinking, show-iterations,
   show-silent, show-timestamps, mouse-selection-copy,
   voice/respond?, message-meta, reasoning-level,
   openai-codex-verbosity) was migrated into the toggles registry.
   Host-owned toggles render on General; extension-owned toggles render
   on Extensions so the same switch is not duplicated across tabs."
  []
  [{:key :theme-name,
    :type :choice,
    :choices (theme-choice-order),
    :label "Theme",
    :description
    "Reusable channel theme from com.blockether.vis.internal.theme and extension :ext/theme maps"}])
(defn- general-toggle-spec?
  [{:keys [owner]}]
  (or (nil? owner) (= :vis owner)))

(declare titleize-label)

(defn- registry-toggle-rows
  "Settings rows for registered feature toggles accepted by `include?`.

   Pure projection over `(vis/registered-toggles)`: any toggle
   registered by the host or by an extension shows up in the matching
   tab without a per-row patch. The `:registry-toggle` row type carries
   `:toggle-id` so the apply path flips the registry value (which then
   fan-outs to listeners — persist + render bump — wired in
   `screen/run-chat!`). Returns nil when no matching toggle is
   registered so the section header stays hidden on a bare install."
  ([] (registry-toggle-rows (constantly true)))
  ([include?]
   (let [specs (->> (vis/registered-toggles)
                 (filter include?)
                 (sort-by (juxt #(or (some-> %
                                       :group
                                       name)
                                   "zzz")
                            :id)))]
     (when (seq specs)
       (vec (cons {:type :section, :label "Feature Toggles"}
              (for [{:keys [id label description owner]} specs]
                {:key (keyword (str "toggle::" id)),
                 :type :registry-toggle,
                 :toggle-id id,
                 :label (or label (titleize-label (str (or (namespace id) "") " " (name id)))),
                 :description (str (or description "")
                                (when (and owner (not= owner :vis))
                                  (str "  [" (titleize-label (name owner)) "]")))})))))))
(def ^:private tui-contributor-slots
  #{:tui.slot/header-row :tui.slot/footer-segment :tui.slot/footer-subtitle-segment})
(def ^:private undisableable-tui-contributions
  "Contributions that paint core identity / cannot be hidden from the user.
   The Settings dialog hides their toggle rows; the rendering path in
   footer.clj also bypasses `:contributors-disabled` for them."
  #{:tui.builtin.model/footer})
(defn- contributor-rows
  "Settings-dialog rows for registered TUI channel contributions.
   Each row is a `:set-toggle` against `:contributors-disabled`.
   Builtin core-identity contributions (see `undisableable-tui-contributions`)
   are filtered out so the user can't accidentally hide the model label
   or other critical chrome.
   When no extensions have registered toggleable TUI contributors,
   returns nil so the section stays hidden — don't show an empty band."
  []
  (let [contributions (->> (vis/channel-contributions-for :tui)
                        (filter #(contains? tui-contributor-slots (:slot %)))
                        (remove #(contains? undisableable-tui-contributions (:id %)))
                        (sort-by (juxt #(str (:slot %)) #(str (:id %)))))]
    (when (seq contributions)
      (vec (cons {:type :section, :label "Header / Footer Contributors"}
             (for [{:keys [id slot]} contributions]
               {:key (keyword (str "contrib::" id)),
                :type :set-toggle,
                :set-key :contributors-disabled,
                :item-id id,
                :label (str id),
                :description (str "Toggle this extension's contribution to the TUI "
                               (case slot
                                 :tui.slot/header-row "header subtitle row"
                                 :tui.slot/footer-segment "footer"
                                 :tui.slot/footer-subtitle-segment "footer subtitle"
                                 "chrome"))}))))))
(defn- settings-content-width [cols] (default-content-width cols))
(defn- settings-content-height [rows] (default-content-height rows))
(def ^:private settings-tabs
  [{:id :general, :label "General"} {:id :extensions, :label "Extensions"}])
(defn- settings-tab-index
  [tab-id]
  (let [ids (mapv :id settings-tabs)
        idx (.indexOf ^java.util.List ids tab-id)]
    (if (neg? idx) 0 idx)))
(defn- settings-next-tab
  [tab-id delta]
  (let [n (count settings-tabs)]
    (:id (nth settings-tabs (mod (+ (settings-tab-index tab-id) delta) n)))))
(defn- settings-empty-rows
  [section-label description]
  [{:type :section, :label section-label}
   {:type :info, :label (str "No " (str/lower-case section-label)), :description description}])
(defn- titleize-token
  [s]
  (let [s (str s)]
    (if (str/blank? s) s (str (str/upper-case (subs s 0 1)) (str/lower-case (subs s 1))))))
(defn- titleize-label
  [s]
  (->> (str/split (str s) #"[-_\s]+")
    (remove str/blank?)
    (map titleize-token)
    (str/join " ")))
(def ^:private namespace-noise-segments
  ;; Trailing/marketing segments we drop when deriving a display label
  ;; from a namespace symbol. `core` / `bot` / `main` are the
  ;; vis-extension convention for "the registrar entry point". The
  ;; vendor prefix `com.blockether.vis.ext` (and the per-extension
  ;; family prefixes underneath) carry no information for the user, so
  ;; we strip them too.
  #{"com" "blockether" "vis" "ext" "core" "bot" "main"})
(defn- meaningful-namespace-segment
  "Pick a human-friendly leaf from a fully-qualified namespace symbol.

   The previous implementation called `(name sym)` on a non-aliased
   symbol, which returns the whole dotted string
   (`\"com.blockether.vis.ext.foundation-voice.core\"`). `titleize-label` only
   splits on `[-_\\s]+`, so the whole thing was treated as ONE token and
   the user saw `\"Com.blockether.vis.ext.voice.core\"` in the dialog.

   We split on dots, drop vendor / 'core' / 'bot' / 'main' noise, and
   return the last surviving segment. Caller is expected to feed it
   through `titleize-label` for normal Title-Case rendering, so e.g.
   `voice` -> `Voice`, `goal` -> `Goal`, `channel-tui` ->
   `Channel Tui`. Falls back to the full string if nothing useful
   survives."
  [sym-or-str]
  (let [raw (str sym-or-str)
        segments (->> (str/split raw #"\.")
                   (remove str/blank?)
                   vec)
        cleaned (vec (remove #(contains? namespace-noise-segments %) segments))
        leaf (or (last cleaned) (last (remove #{"com" "blockether"} segments)) (last segments))]
    (or leaf raw)))
(defn- extension-kind
  [ext]
  (cond (seq (:ext/providers ext)) :provider
    (seq (:ext/channels ext)) :channel
    :else :extension))
(defn- extension-display-label
  [ext]
  (let [provider-label (some-> (first (:ext/providers ext))
                         :provider/label
                         (str/replace #"\s+\(.*\)$" ""))
        channel-label (or (some-> (first (:ext/channels ext))
                            :channel/cmd
                            titleize-label)
                        (some-> (first (:ext/channels ext))
                          :channel/id
                          name
                          titleize-label))
        alias-label (some-> (get-in ext [:ext/sci :ext.sci/alias])
                      name
                      titleize-label)
        ;; Take the meaningful tail segment of the namespace (drop
        ;; `com.blockether.vis.ext` vendor prefix and the trailing
        ;; `core` / `bot` / `main` registrar entry-point convention)
        ;; and titleize THAT, so `voice` -> `Voice`, `goal` ->
        ;; `Goal`, `channel-tui` -> `Channel Tui` instead of the
        ;; previous `Com.blockether.vis.ext.voice.core`.
        ns-label (some-> (:ext/name ext)
                   meaningful-namespace-segment
                   titleize-label)]
    (or (not-empty provider-label)
      (not-empty channel-label)
      (not-empty alias-label)
      (not-empty ns-label)
      "Extension")))
(defn- setting-key
  [v]
  (cond (keyword? v) v
    (string? v) (let [s (str/trim v)] (when-not (str/blank? s) (keyword s)))
    :else nil))

(def ^{:private true} retired-extension-setting-keys
  "Old :ext/settings rows now owned by registry toggles. Drop them rather than
   aliasing or rendering duplicates."
  #{:voice/respond? :reasoning-level :vis/reasoning-level
    :openai-codex-verbosity :openai-codex/verbosity})

(defn- extension-setting-declarations
  []
  (->> (vis/registered-extensions)
    (mapcat (fn [ext]
              (let [ext-id (:ext/name ext)
                    ext-kind (extension-kind ext)
                    ext-label (extension-display-label ext)
                    provider-ids (set (keep :provider/id (:ext/providers ext)))]
                (keep-indexed (fn [idx decl]
                                (when-let [k (setting-key (:key decl))]
                                  (when-not (contains? retired-extension-setting-keys k)
                                    (assoc decl
                                      :key k
                                      :extension-id ext-id
                                      :extension-kind ext-kind
                                      :extension-label ext-label
                                      :extension-order idx
                                      :provider-ids provider-ids))))
                  (:ext/settings ext)))))
    (sort-by (juxt :extension-kind :extension-label :extension-order :key))
    vec))
(defn- extension-setting-rows
  []
  (mapv (fn [{:keys [key type choices label description extension-id extension-kind extension-label
                     provider-ids]}]
          {:type (or type :toggle),
           :id [:extension-setting extension-id key],
           :key key,
           :choices choices,
           :label (or label (name key)),
           :extension-id extension-id,
           :extension-kind extension-kind,
           :extension-label extension-label,
           :provider-ids provider-ids,
           :description (or description "Extension setting")})
    (extension-setting-declarations)))
(defn- extension-env-declarations
  []
  (->> (vis/registered-extensions)
    (mapcat (fn [ext]
              (let [ext-id (:ext/name ext)
                    ext-kind (extension-kind ext)
                    ext-label (extension-display-label ext)]
                (for [decl (:ext/env ext)
                      :let [name (some-> (:name decl)
                                   str
                                   str/trim)]
                      :when (not (str/blank? name))]
                  (assoc decl
                    :name name
                    :extension-id ext-id
                    :extension-kind ext-kind
                    :extension-label ext-label)))))
    (sort-by (juxt :extension-kind :extension-label :name))
    vec))
(defn- extension-env-rows
  []
  (mapv (fn [{:keys [name label description extension-id extension-kind extension-label secret?
                     required?]}]
          {:type :env-var,
           :id [:environment name],
           :name name,
           :label (or label name),
           :extension-id extension-id,
           :extension-kind extension-kind,
           :extension-label extension-label,
           :description (or description "Extension environment override"),
           :secret? (boolean secret?),
           :required? (boolean required?)})
    (extension-env-declarations)))
(defn- extension-option-rows [] (vec (concat (extension-setting-rows) (extension-env-rows))))
(defn- provider-row-active?
  [active-provider {:keys [extension-kind provider-ids]}]
  (or (not= :provider extension-kind)
    (nil? active-provider)
    (empty? provider-ids)
    (contains? provider-ids active-provider)))
(defn- extension-rows-of-kind
  [extension-rows kind]
  (filterv #(= kind (:extension-kind %)) extension-rows))
(defn- extension-group-key
  [{:keys [extension-label extension-id]}]
  [(or extension-label "Extension") (str extension-id)])
(defn- settings-extension-groups
  [extension-rows]
  (when (seq extension-rows)
    (mapcat (fn [[[label _] group-rows]]
              (into [{:type :subsection, :label label}]
                (sort-by (juxt :type :label :name) group-rows)))
      (sort-by first (group-by extension-group-key extension-rows)))))
(defn- settings-rows
  ([] (settings-rows :general (extension-option-rows)))
  ([tab-id] (settings-rows tab-id (extension-option-rows)))
  ([tab-id extension-rows]
   (let [active-provider (current-provider-id)
         extension-tab-rows (filterv #(provider-row-active? active-provider %) extension-rows)
         ;; Registry owns migrated host settings; :ext/settings now only
         ;; renders settings still declared by real extensions.
         model-rows (if (reasoning-effort-configurable?) [] settings-model-no-effort-rows)
         extension-toggle-rows (registry-toggle-rows (complement general-toggle-spec?))]
     (vec
       (case tab-id
         :extensions
         (let [rows (concat (or extension-toggle-rows [])
                      (or (contributor-rows) [])
                      (when (seq extension-tab-rows)
                        (concat [{:type :section, :label "Extension Settings"}]
                          (settings-extension-groups extension-tab-rows))))]
           (if (seq rows)
             rows
             (settings-empty-rows
               "Extensions"
               "Installed extensions have not declared configurable settings, env vars, toggles, or TUI contributors")))
         :general (concat [{:type :section, :label "Terminal UI"}]
                    (settings-ui-options)
                    model-rows
                    (or (registry-toggle-rows general-toggle-spec?) []))
         (settings-rows :general extension-rows))))))
(defn- extension-env-status-label
  [source]
  (case source
    :config "set in Vis config"
    :env "set in environment"
    :unset "unset"
    "unset"))
(defn- settings-option-label
  [{:keys [key label type choices set-key item-id toggle-id], env-name :name} values]
  (case type
    :choice (str label ": " (clojure.core/name (or (get values key) (first choices))))
    :env-var
    (str label ": " (extension-env-status-label (:source (vis/extension-env-status env-name))))
    :set-toggle (let [disabled? (boolean (some-> (get values set-key)
                                           (contains? item-id)))]
                  (str label " (" (if disabled? "hidden" "shown") ")"))
    :registry-toggle (let [spec (vis/toggle-spec toggle-id)
                           toggle-val (vis/toggle-value toggle-id)]
                       (if (= :enum (:type spec))
                         (str label ": " (clojure.core/name toggle-val))
                         (str label " (" (if (boolean toggle-val) "on" "off") ")")))
    label))
(defn- cycle-choice
  [choices current]
  (let [choices (vec choices)
        idx (.indexOf ^java.util.List choices current)]
    (nth choices (mod (inc (if (neg? idx) 0 idx)) (count choices)))))
(defn- apply-settings-option
  [values {:keys [key type choices set-key item-id toggle-id]}]
  (case type
    :choice (update values key #(cycle-choice choices %))
    :toggle (update values key not)
    :set-toggle (update values
                  set-key
                  (fn [s]
                    (let [s (or s #{})]
                      (if (contains? s item-id) (disj s item-id) (conj s item-id)))))
    :registry-toggle (do (if (= :enum (:type (vis/toggle-spec toggle-id)))
                           (vis/toggle-cycle-value! toggle-id)
                           (vis/toggle-set-enabled! toggle-id
                             (not (vis/toggle-enabled? toggle-id))))
                       values)
    values))
(defn- notify-settings-change!
  [callbacks values]
  (when-let [f (:on-change callbacks)] (f values))
  values)
(defn- settings-selectable?
  [{:keys [type]}]
  (contains? #{:toggle :choice :action :env-var :set-toggle :registry-toggle} type))
(defn- first-selectable-index
  [rows]
  (or (first (keep-indexed (fn [i row] (when (settings-selectable? row) i)) rows)) 0))
(defn- move-settings-selection
  [rows selected delta]
  (let [n (count rows)]
    (loop [idx (clamp (+ selected delta) 0 (max 0 (dec n)))]
      (cond (= idx selected) idx
        (settings-selectable? (nth rows idx)) idx
        (and (neg? delta) (zero? idx)) selected
        (and (pos? delta) (= idx (dec n))) selected
        :else (recur (clamp (+ idx delta) 0 (max 0 (dec n))))))))
(defn- edit-extension-env-var!
  [^TerminalScreen screen {:keys [name label description secret?]}]
  (let [{:keys [source value]} (vis/extension-env-status name)
        raw (text-input-dialog!
              screen
              "Extension Environment" (str name ":")
              :mask (when secret? \*)
              :initial (if secret? "" (or value ""))
              :body [(str label " - " (extension-env-status-label source)) (or description "")
                     "Blank input clears the Vis config override; OS env still applies."])]
    (when (some? raw) (vis/save-extension-env-var! name raw))))
(defn- theme-display-label
  [theme-id]
  (let [theme-map (shared-theme/theme theme-id)]
    (or (:display-name theme-map)
      (some-> theme-id
        name
        titleize-label)
      (str theme-id))))
(defn- theme-picker-items
  [choices]
  (mapv (fn [theme-id] {:theme-id theme-id, :label (theme-display-label theme-id)}) choices))
(defn- theme-picker-content-width [cols] (settings-content-width cols))
(defn- theme-picker-content-height [rows] (settings-content-height rows))
(defn- theme-picker-dialog!
  "Small theme chooser. Moving selection previews the theme immediately;
   Enter commits the preview, Esc restores the original theme."
  [^TerminalScreen screen choices current preview!]
  (let [items (theme-picker-items choices)
        total (count items)
        original (or current (:theme-id (first items)))
        selected (atom (max 0 (.indexOf ^java.util.List (vec choices) original)))
        scroll (atom 0)
        last-preview (atom ::none)
        preview-selected! (fn []
                            (when-let [theme-id (:theme-id (nth items @selected nil))]
                              (when-not (= theme-id @last-preview)
                                (reset! last-preview theme-id)
                                (preview! theme-id))))]
    (when (pos? total)
      (loop []
        (preview-selected!)
        (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
              cols (.getColumns size)
              rows (.getRows size)
              g (.newTextGraphics screen)
              content-w (theme-picker-content-width cols)
              content-h (theme-picker-content-height rows)
              bounds (draw-dialog-chrome! g cols rows "Theme" content-w content-h)
              {:keys [left inner-w]} bounds
              {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
              visible (min total content-h)
              _ (swap! selected #(clamp % 0 (max 0 (dec total))))
              _ (swap! scroll #(visible-window-start @selected % content-h total))]
          (dotimes [i visible]
            (let [idx (+ @scroll i)
                  row-y (+ content-top i)]
              (when (< idx total)
                (draw-list-item! g
                  left
                  row-y
                  (if (> total content-h) (dec inner-w) inner-w)
                  (= idx @selected)
                  (:label (nth items idx))))))
          (scrollbar/draw! g
            {:col (+ left inner-w),
             :top content-top,
             :track-h content-h,
             :total-h total,
             :inner-h content-h,
             :scroll @scroll})
          (draw-hint-bar! g
            left
            hint-row
            inner-w
            [["↑/↓" "preview"] ["Enter" "choose"] ["Esc" "cancel"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (.refresh screen Screen$RefreshType/DELTA)
          (let [key (read-modal-key! screen)]
            (when key
              (condp = (.getKeyType key)
                KeyType/Escape (do (preview! original) nil)
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                    (recur))
                KeyType/Enter (:theme-id (nth items @selected))
                (recur)))))))))
(defn- activate-theme-row!
  [screen values callbacks {:keys [choices key]}]
  (let [original (get @values key)
        preview! (fn [theme-id]
                   (let [next-values (assoc @values key theme-id)]
                     (reset! values next-values)
                     (notify-settings-change! callbacks next-values)))]
    (if-let [selected (theme-picker-dialog! screen choices original preview!)]
      (preview! selected)
      (preview! original))))
(defn- activate-settings-row!
  [screen values callbacks row]
  (case (:type row)
    :action (when-let [f (get callbacks (:id row))] (f @values))
    :env-var (edit-extension-env-var! screen row)
    (if (= :theme-name (:key row))
      (activate-theme-row! screen values callbacks row)
      (->> (swap! values apply-settings-option row)
        (notify-settings-change! callbacks)))))
(defn- settings-section-text
  [label inner-w]
  (let [prefix (str "── " label " ")
        available (max 0 (- inner-w 2))
        filler (apply str (repeat (max 0 (- available (count prefix))) \─))]
    (ellipsize (str prefix filler) available)))
(defn- settings-option-indent [] t/settings-option-indent)
(defn- settings-subsection-text [label inner-w] (ellipsize (str "◆ " label) (max 0 (- inner-w 2))))
(defn- settings-wrap-lines
  [s w]
  (let [w (max 1 (long w))
        s (str/trim (str (or s "")))]
    (if (str/blank? s) [] (vec (remove str/blank? (render/wrap-text s w))))))
(defn- settings-render-entries
  "Flatten logical settings rows into paint rows. Descriptions wrap under
   their owning option instead of stealing a fixed inline column and
   collapsing to `...` on narrow dialogs / long extension labels."
  [rows option-w desc-w]
  (let [option-w (max 1 (long option-w))
        desc-w (max 1 (long desc-w))]
    (vec (mapcat (fn [idx {:keys [type label description]}]
                   (case type
                     :section [{:row-idx idx, :part :section}]
                     :subsection [{:row-idx idx, :part :subsection}]
                     :info (let [text (str label
                                        (when-not (str/blank? (str (or description "")))
                                          (str "  " description)))
                                 lines (settings-wrap-lines text (max 1 (- option-w 2)))]
                             (mapv (fn [line] {:row-idx idx, :part :info-line, :text line})
                               (or (seq lines) [""])))
                     (let [desc-lines (settings-wrap-lines description desc-w)]
                       (into [{:row-idx idx, :part :option}]
                         (mapv (fn [line] {:row-idx idx, :part :option-desc, :text line})
                           desc-lines)))))
           (range)
           rows))))
(defn- settings-tab-geometry
  [left inner-w]
  (let [n (count settings-tabs)
        gap 1
        total-w (max n (- inner-w 2))
        tab-w (max 1 (quot (max n (- total-w (* gap (dec n)))) n))
        start-x (+ left 2)]
    (mapv (fn [idx tab]
            (let [x (+ start-x (* idx (+ tab-w gap)))
                  w (if (= idx (dec n)) (max 1 (- (+ start-x total-w) x)) tab-w)]
              (assoc tab
                :left x
                :width w)))
      (range)
      settings-tabs)))
(defn- settings-tab-at
  [left inner-w col]
  (some (fn [{:keys [id left width]}] (when (and (>= col left) (< col (+ left width))) id))
    (settings-tab-geometry left inner-w)))
(defn- draw-settings-tabs!
  [g left row inner-w active-tab]
  (let [tabs (settings-tab-geometry left inner-w)]
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (doseq [{:keys [id label left width]} tabs]
      (let [active? (= id active-tab)]
        (if active?
          (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
          (p/set-colors! g t/dialog-hint t/dialog-bg))
        (p/fill-rect! g left row width 1)
        (p/styled g
          (if active? [p/BOLD] [p/ITALIC])
          (p/draw-centered! g left row width (ellipsize label width)))))))
(defn settings-dialog!
  "Show the tabbed settings dialog.

   Tabs: Channels, Providers & Models, Extensions. Toggle rows render `[✓]` /
   `[ ]`. Choice rows render `[->]` and cycle through their allowed values
   with Space or Enter. Action rows render `[↗]` and invoke a callback
   from `callbacks`.

   `settings` is the persisted TUI settings map (see
   `state/default-settings`). Esc closes the dialog and returns the
   current settings map."
  ([^TerminalScreen screen settings] (settings-dialog! screen settings nil))
  ([^TerminalScreen screen settings callbacks]
   (let [extension-rows (extension-option-rows)
         active-tab (atom :general)
         selected (atom (first-selectable-index (settings-rows :general extension-rows)))
         scroll (atom 0)
         values (atom (or settings {}))
         scrollbar-drag-offset (volatile! nil)
         check-w 4
         switch-tab! (fn [tab-id]
                       (let [rows (settings-rows tab-id extension-rows)]
                         (reset! active-tab tab-id)
                         (reset! selected (first-selectable-index rows))
                         (reset! scroll 0)))]
     (loop []
       (let [rows (settings-rows @active-tab extension-rows)
             n (count rows)
             size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
             cols (.getColumns size)
             screen-rows (.getRows size)
             g (.newTextGraphics screen)
             bounds (draw-dialog-chrome! g
                      cols
                      screen-rows
                      "Settings"
                      (settings-content-width cols)
                      (settings-content-height screen-rows))
             {:keys [left right inner-w]} bounds
             {:keys [content-top content-h hint-row]} (dialog-layout bounds)
             tabs-row content-top
             tabs-sep-row (min hint-row (inc tabs-row))
             list-top (min hint-row (+ content-top 2))
             visible-h (max 1 (- content-h 2))
             _ (swap! selected #(clamp % 0 (max 0 (dec n))))
             option-indent (settings-option-indent)
             ;; Reserve `p/SELECTION_WIDTH` cols at the start of the
             ;; option row for the selection gutter (`>` glyph + 1
             ;; col margin). The cursor itself is painted at
             ;; `(inc left)` (the dialog's inner edge) by the row
             ;; loop; option body shifts right by the gutter.
             option-x (+ left 2 option-indent p/SELECTION_WIDTH)
             labels (mapv #(settings-option-label % @values) rows)
             base-paint-w inner-w
             base-option-w (max 1 (- base-paint-w 2 option-indent p/SELECTION_WIDTH))
             base-desc-w (max 1 (- base-option-w check-w))
             base-entries (settings-render-entries rows base-option-w base-desc-w)
             scrollable? (> (count base-entries) visible-h)
             paint-w (if scrollable? (max 1 (dec inner-w)) inner-w)
             option-w (max 1 (- paint-w 2 option-indent p/SELECTION_WIDTH))
             desc-x (+ option-x check-w)
             desc-w (max 1 (- option-w check-w))
             entries (settings-render-entries rows option-w desc-w)
             visual-n (count entries)
             sel-entry-idxs (keep-indexed (fn [entry-idx {:keys [row-idx]}]
                                            (when (= row-idx @selected) entry-idx))
                              entries)
             ;; Option line of the selected row (first non-description entry).
             selected-visual (or (first (keep-indexed (fn [entry-idx {:keys [row-idx part]}]
                                                        (when (and (= row-idx @selected)
                                                                (not= part :option-desc))
                                                          entry-idx))
                                          entries))
                               0)
             ;; Last paint row owned by the selected option, INCLUDING its
             ;; wrapped description rows. The scroll window must be able to
             ;; reach this so the trailing desc lines (and, for the bottom-most
             ;; option, the true content end) come into view — otherwise scroll
             ;; caps short of `visual-n - visible-h` and the scrollbar thumb
             ;; never reaches the bottom (selectable rows < paint rows).
             selected-visual-end (or (last sel-entry-idxs) selected-visual)
             ;; Visual index where the intro rows (section / subsection /
             ;; info-line) that directly precede the selected option begin.
             ;; The scroll window is selection-driven, so without this the
             ;; first option pins itself to the top and its SECTION HEADER
             ;; (a non-selectable row above it) is clipped forever — you can
             ;; scroll to the first setting but never see its header.
             header-start (loop [i (dec selected-visual)]
                            (if (and (>= i 0)
                                  (contains? #{:section :subsection :info-line}
                                    (:part (nth entries i))))
                              (recur (dec i))
                              (inc i)))
             _ (let [start0 (visible-window-start selected-visual @scroll visible-h visual-n)
                     ;; Back UP to reveal those intro headers whenever the
                     ;; option (through its last desc line) still fits in the
                     ;; viewport from `header-start`.
                     start0 (if (and (< header-start start0)
                                  (<= (- selected-visual-end header-start) (dec visible-h)))
                              header-start
                              start0)
                     ;; Pull the window down to reveal the selected row's last
                     ;; desc line, but never so far that the option line itself
                     ;; scrolls out of view (cap at `selected-visual`).
                     start1 (if (>= selected-visual-end (+ start0 visible-h))
                              (min selected-visual (max 0 (- (inc selected-visual-end) visible-h)))
                              start0)]
                 (reset! scroll start1))]
         (draw-settings-tabs! g left tabs-row inner-w @active-tab)
         (p/set-colors! g t/dialog-border t/dialog-bg)
         (p/draw-separator! g left right tabs-sep-row)
         (dotimes [i visible-h]
           (let [entry-idx (+ @scroll i)
                 row-y (+ list-top i)]
             (if (< entry-idx visual-n)
               (let [{:keys [row-idx part text]} (nth entries entry-idx)
                     {:keys [key type label]} (nth rows row-idx)
                     option-label (nth labels row-idx)
                     selected? (= row-idx @selected)
                     state-mark (case type
                                  :action "[↗] "
                                  :env-var "[↗] "
                                  :choice "[->] "
                                  (if (get @values key true) "[✓] " "[ ] "))]
                 (case part
                   :section (do
                              (p/set-colors! g t/dialog-border t/dialog-bg)
                              (p/fill-rect! g (inc left) row-y paint-w 1)
                              (p/put-str! g (+ left 2) row-y (settings-section-text label paint-w))
                              (p/set-fg! g t/dialog-hint-key)
                              (p/styled g [p/BOLD] (p/put-str! g (+ left 5) row-y label)))
                   :subsection (do (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                                 (p/fill-rect! g (inc left) row-y paint-w 1)
                                 (p/styled g
                                   [p/BOLD]
                                   (p/put-str! g
                                     (+ left 2)
                                     row-y
                                     (settings-subsection-text label paint-w))))
                   :info-line
                   (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                     (p/fill-rect! g (inc left) row-y paint-w 1)
                     (p/put-str! g (+ left 2) row-y (ellipsize text (max 1 (- paint-w 2)))))
                   :option-desc (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                                  (p/fill-rect! g (inc left) row-y paint-w 1)
                                  (p/put-str! g desc-x row-y (ellipsize text desc-w)))
                   ;; Selection visual: leading `> ` cursor glyph and
                   ;; BOLD label text. Descriptions wrap beneath the
                   ;; option on dim rows, so long labels no longer force
                   ;; descriptions into an ellipsis-only column.
                   (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                     (p/fill-rect! g (inc left) row-y paint-w 1)
                       ;; Cursor glyph in the dialog padding column.
                     (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                     (p/draw-selection-marker! g (inc left) row-y selected?)
                       ;; Option label.
                     (p/set-colors! g t/dialog-fg t/dialog-bg)
                     (if selected?
                       (p/styled g
                         [p/BOLD]
                         (p/put-str! g
                           option-x
                           row-y
                           (ellipsize (str state-mark option-label) option-w)))
                       (p/put-str! g
                         option-x
                         row-y
                         (ellipsize (str state-mark option-label) option-w))))))
               (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                 (p/fill-rect! g (inc left) row-y paint-w 1)))))
         (scrollbar/draw! g
           {:col (+ left inner-w),
            :top list-top,
            :track-h visible-h,
            :total-h visual-n,
            :inner-h visible-h,
            :scroll @scroll})
         (draw-hint-bar! g
           left
           hint-row
           inner-w
           [["<-/-> Tab" "switch"] ["↑/↓" "move"] ["Space/Enter" "change"]
            ["Esc" "done"]])
         (.setCursorPosition screen (p/cursor-pos 0 0))
         (.refresh screen Screen$RefreshType/DELTA)
         (let [key (read-modal-key! screen)
               selected-row (nth rows @selected)]
           (when key
             (cond
               (instance? MouseAction key)
               (let [^MouseAction ma key
                     action (.getActionType ma)
                     pos (.getPosition ma)
                     mx (.getColumn pos)
                     my (.getRow pos)
                     bar-col (+ left inner-w)
                     geom (scrollbar/geometry visual-n visible-h visible-h @scroll)]
                 (cond
                     ;; Tab click in tab strip.
                   (and (= action MouseActionType/CLICK_DOWN)
                     (= my tabs-row)
                     (settings-tab-at left inner-w mx))
                   (do (switch-tab! (settings-tab-at left inner-w mx)) (recur))
                     ;; Mouse wheel anywhere in the dialog — scroll the
                     ;; list view; selection follows the wheel direction
                     ;; so the cursor stays in the visible window without
                     ;; the user having to chase it with arrow keys.
                   (or (= action MouseActionType/SCROLL_UP)
                     (= action MouseActionType/SCROLL_DOWN))
                   (let [step (or (modal-wheel-step key)
                                (if (= action MouseActionType/SCROLL_UP) -1 1))]
                     (swap! selected #(move-settings-selection rows % step))
                     (recur))
                     ;; CLICK_DOWN on the scrollbar thumb — start drag,
                     ;; preserve the grip so the row under the cursor
                     ;; stays glued to the same point on the thumb.
                   (and (= action MouseActionType/CLICK_DOWN)
                     (some? geom)
                     (scrollbar/on-thumb? mx my {:col bar-col, :top list-top} geom))
                   (let [thumb-top (+ list-top (long (:thumb-top-rel geom)))]
                     (vreset! scrollbar-drag-offset (- my thumb-top))
                     (recur))
                     ;; CLICK_DOWN on the scrollbar TRACK off-thumb —
                     ;; jump-to-position (modern macOS behaviour). Then
                     ;; arm a drag with a centred grip so an immediate
                     ;; follow-up motion tracks naturally.
                   (and (= action MouseActionType/CLICK_DOWN)
                     (some? geom)
                     (scrollbar/on-track? mx
                       my
                       {:col bar-col, :top list-top, :track-h visible-h}))
                   (let [grip (long (quot (long (:thumb-h geom)) 2))]
                     (vreset! scrollbar-drag-offset grip)
                     (reset! scroll (or (scrollbar/scroll-from-mouse-y my
                                          list-top
                                          visible-h
                                          visual-n
                                          visible-h
                                          grip)
                                      0))
                     (recur))
                     ;; DRAG continues to track the cursor while the
                     ;; user holds the button after a thumb grab.
                   (and (= action MouseActionType/DRAG)
                     (some? @scrollbar-drag-offset)
                     (some? geom))
                   (do (reset! scroll (or (scrollbar/scroll-from-mouse-y
                                            my
                                            list-top
                                            visible-h
                                            visual-n
                                            visible-h
                                            (long @scrollbar-drag-offset))
                                        0))
                     (recur))
                   (= action MouseActionType/CLICK_RELEASE)
                   (do (vreset! scrollbar-drag-offset nil) (recur))
                   :else (recur)))
               :else
               (condp = (.getKeyType key)
                 KeyType/Escape @values
                 KeyType/ArrowLeft (do (switch-tab! (settings-next-tab @active-tab -1)) (recur))
                 KeyType/ArrowRight (do (switch-tab! (settings-next-tab @active-tab 1)) (recur))
                 KeyType/Tab (do (switch-tab! (settings-next-tab @active-tab 1)) (recur))
                 KeyType/ArrowUp (do (swap! selected #(move-settings-selection rows % -1))
                                   (recur))
                 KeyType/ArrowDown (do (swap! selected #(move-settings-selection rows % 1))
                                     (recur))
                 KeyType/Character
                 (let [c (.getCharacter key)]
                   (cond (= c \space)
                     (do (activate-settings-row! screen values callbacks selected-row)
                       (recur))
                     (contains? #{\1 \2 \3} c)
                     (do (switch-tab! (:id (nth settings-tabs (- (int c) (int \1)))))
                       (recur))
                     :else (recur)))
                 KeyType/Enter (do (activate-settings-row! screen values callbacks selected-row)
                                 (recur))
                 (recur))))))))))
;;; ── Session picker ─────────────────────────────────────────────────────
(defn- short-session-id [session] (let [id (str (:id session))] (subs id 0 (min 8 (count id)))))
(def ^:private untitled-session-title "Untitled session")
(defn- untitled-session-title?
  [title]
  (or (str/blank? (str title))
    (#{"untitled" "untitled session"} (str/lower-case (str/trim (str title))))))
(defn- empty-untitled-session?
  [{:keys [title turn-count]}]
  (and (not (pos? (long (or turn-count 0))))
    (untitled-session-title? title)))
(defn- session-title
  [session]
  (let [title (:title session)
        base-title (if (untitled-session-title? title) untitled-session-title (str title))
        fork-count (long (or (:fork-count session) 0))]
    (cond-> base-title (pos? fork-count) (str " [forks:" fork-count "]"))))
(def ^:private session-dialog-content-w 96)
(def ^:private session-dialog-content-h 14)
(defn- date->millis
  [v]
  (cond (instance? java.util.Date v) (.getTime ^java.util.Date v)
    (instance? java.time.Instant v) (.toEpochMilli ^java.time.Instant v)
    (number? v) (long v)
    :else nil))
(defn- date-value [v] (when-let [ms (date->millis v)] (java.util.Date. ms)))
(defn- format-session-date
  [v]
  (if-let [date (date-value v)]
    (let [fmt (SimpleDateFormat. "yyyy-MM-dd HH:mm" Locale/ROOT)]
      (.setTimeZone fmt (TimeZone/getTimeZone "UTC"))
      (.format fmt date))
    "-"))
(def ^:private session-table-headers
  ["" "ID" "Title" "Turns" "Created at" "Time" "Modified at" "Time"])
(def ^:private session-table-aligns [:left :left :left :right :left :left :left :left])
(defn- format-session-day
  [v]
  (if-let [date (date-value v)]
    (let [fmt (SimpleDateFormat. "yyyy-MM-dd" Locale/ROOT)]
      (.setTimeZone fmt (TimeZone/getTimeZone "UTC"))
      (.format fmt date))
    "-"))
(defn- format-session-time
  [v]
  (if-let [date (date-value v)]
    (let [fmt (SimpleDateFormat. "HH:mm" Locale/ROOT)]
      (.setTimeZone fmt (TimeZone/getTimeZone "UTC"))
      (.format fmt date))
    "-"))
(defn- session-table-widths
  "Column widths for the boxed session table. Total rendered row width equals
   `table-w`, including side borders, inter-cell separators, and padding."
  [table-w]
  (let [n (count session-table-headers)
        overhead (inc (* 3 n))
        available (max n (- table-w overhead))]
    (if (>= available 70)
      (let [active-w 1
            id-w 8
            title-w (max 10 (- available active-w id-w 5 10 5 11 5))
            turns-w 5
            created-w 10
            modified-w 11
            time-w 5]
        [active-w id-w title-w turns-w created-w time-w modified-w time-w])
      (let [active-w 1
            id-w (max 1 (min 8 (quot available 8)))
            turns-w (max 1 (min 5 (quot available 8)))
            created-w (max 1 (min 10 (quot available 7)))
            modified-w (max 1 (min 11 (quot available 7)))
            time-w (max 1 (min 5 (quot available 12)))
            title-w (max 1 (- available active-w id-w turns-w created-w time-w modified-w time-w))]
        [active-w id-w title-w turns-w created-w time-w modified-w time-w]))))
(defn- session-table-border-line
  [body-w kind]
  (table/boxed-border-line (session-table-widths body-w) kind))
(defn- session-table-row-label
  "Format one fixed-width boxed session table row. Width math is terminal
   columns, not Java chars, so CJK/emoji titles cannot shift later rows."
  [cells body-w]
  (table/boxed-row-line (session-table-widths body-w) cells session-table-aligns))
(defn session-dialog-label
  "Format one fixed-width session table row. Columns are intentionally
   stable so the picker reads as a table inside the shared dialog chrome."
  [{:keys [id turn-count modified-at created-at], :as session} active-id body-w]
  (let [active? (= (str id)
                  (some-> active-id
                    str))]
    (session-table-row-label [(if active? "●" "") (short-session-id session) (session-title session)
                              (str (long (or turn-count 0))) (format-session-day created-at)
                              (format-session-time created-at) (format-session-day modified-at)
                              (format-session-time modified-at)]
      body-w)))
(defn session-dialog-header [body-w] (session-table-row-label session-table-headers body-w))
(defn- session-dialog-sort-key
  [{:keys [modified-at created-at]}]
  [(- (long (or (date->millis modified-at) 0))) (- (long (or (date->millis created-at) 0)))])
(defn session-dialog-items
  "Build table rows for existing sessions only. New/fork stay dialog
   options via the N/F shortcuts and command palette; they are not fake table
   data rows. Rows are sorted by Modified at desc, then Created at desc."
  ([sessions active-id] (session-dialog-items sessions active-id session-dialog-content-w))
  ([sessions active-id body-w]
   (mapv (fn [session]
           {:action :switch,
            :id (str (:id session)), ; downstream (switch-session!) accepts full UUID strings
            :label (session-dialog-label session active-id body-w)})
     (sort-by session-dialog-sort-key sessions))))
(defn- draw-session-row!
  [g left row inner-w selected? label]
  ;; Session picker is a TABLE — cells must NOT shift between selected
  ;; and unselected states, so the dot marker is painted by caller (see
  ;; row loop in `session-picker-dialog!`) at `(inc left)`, inner edge
  ;; of dialog frame. Body label sits two cols further in (gutter for
  ;; marker + margin) and uses normal palette, BOLD on selected so row
  ;; text echoes marker cue.
  (p/set-colors! g t/dialog-fg t/dialog-bg)
  (p/fill-rect! g (inc left) row inner-w 1)
  (let [body-x (+ left 1 p/SELECTION_WIDTH)
        body-w (max 0 (- inner-w 1 p/SELECTION_WIDTH))]
    (if selected?
      (p/styled g [p/BOLD] (p/put-str! g body-x row (ellipsize label body-w)))
      (p/put-str! g body-x row (ellipsize label body-w)))))
(defn session-picker-dialog!
  "Show recent TUI sessions in a fixed-size table. Returns
   `{:action :new}`, `{:action :fork}`, `{:action :switch :id <session-id>}`,
   or nil on Esc."
  [^TerminalScreen screen sessions active-id]
  (let [selected (atom 0)
        scroll (atom 0)]
    (loop []
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows "Sessions" session-dialog-content-h)
            {:keys [left inner-w]} bounds
            ;; Reserve `p/SELECTION_WIDTH` cols at start of inner area
            ;; for dot marker gutter. Table itself is boxed; marker stays
            ;; outside table so columns never shift.
            body-w (max 1 (- inner-w 4 p/SELECTION_WIDTH))
            items (session-dialog-items sessions active-id body-w)
            total (count items)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds)
            table-x (+ left 1 p/SELECTION_WIDTH)
            table-top content-top
            header-row (inc table-top)
            sep-row (inc header-row)
            body-top (inc sep-row)
            body-h (max 1 (- content-h 4))
            bottom-row (+ body-top body-h)
            visible (min total body-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % body-h total))]
        (p/set-colors! g t/dialog-border t/dialog-bg)
        (p/fill-rect! g (inc left) table-top inner-w 1)
        (p/put-str! g table-x table-top (session-table-border-line body-w :top))
        (p/set-colors! g t/dialog-hint-key t/dialog-bg)
        (p/styled g
          [p/BOLD]
          (p/fill-rect! g (inc left) header-row inner-w 1)
          (p/put-str! g table-x header-row (session-dialog-header body-w)))
        (p/set-colors! g t/dialog-border t/dialog-bg)
        (p/fill-rect! g (inc left) sep-row inner-w 1)
        (p/put-str! g table-x sep-row (session-table-border-line body-w :middle))
        (dotimes [i body-h]
          (let [idx (+ @scroll i)
                row (+ body-top i)]
            (if (< idx total)
              (do (draw-session-row! g left row inner-w (= idx @selected) (:label (nth items idx)))
                (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                (p/draw-selection-marker! g (inc left) row (= idx @selected)))
              (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g (inc left) row inner-w 1)))))
        (p/set-colors! g t/dialog-border t/dialog-bg)
        (p/fill-rect! g (inc left) bottom-row inner-w 1)
        (p/put-str! g table-x bottom-row (session-table-border-line body-w :bottom))
        (draw-hint-bar! g
          left
          hint-row
          inner-w
          [["↑/↓" "move"] ["Enter" "select"] ["N" "new"] ["F" "fork"]
           ["Esc" "cancel"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (if-let [wheel-step (modal-wheel-step key)]
              (do (swap! selected #(clamp (+ % wheel-step) 0 (max 0 (dec total)))) (recur))
              (condp = (.getKeyType key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                    (recur))
                KeyType/Enter (when (pos? total) (select-keys (nth items @selected) [:action :id]))
                KeyType/Character (let [raw-c (.getCharacter key)
                                        c (when raw-c (Character/toLowerCase raw-c))]
                                    (case c
                                      \n {:action :new}
                                      \f {:action :fork}
                                      (recur)))
                (recur)))))))))
;;; ── Global navigator (Ctrl+G) ───────────────────────────────────────────────
;; One row per session. Per the locked 1:1 session<->workspace model a
;; session IS its workspace, so the navigator shows a single unified list:
;; no "Kind" column and no session/workspace mode split. The old design
;; emitted both a session row AND a workspace row per entry, so every
;; entry showed up twice with a contradictory "Kind".
(def ^:private navigator-columns
  [{:id :title, :label "Title", :flex 1}
   {:id :session, :label "Session", :width 8}
   {:id :status, :label "Status", :width 10}
   {:id :created, :label "Created", :width 12}
   {:id :modified, :label "Modified", :width 12}])
(defn- navigator-content-w
  "Navigator is wider than the default modal footprint — five columns need
   room for the Title to breathe — but still clamps to the terminal."
  [cols]
  (max (default-content-width cols) (min (max 1 (- cols 6)) 104)))
(defn- navigator-stamp
  "Compact `MM-dd HH:mm` timestamp (year dropped — these are recent
   sessions), or `-` when absent."
  [v]
  (let [day (format-session-day v)]
    (if (= day "-") "-" (str (subs day 5) " " (format-session-time v)))))
(defn- navigator-session-row
  "One unified row per session — a session IS its workspace (locked 1:1),
   so there is no separate workspace row or `kind`. Columns mirror the
   session picker: title, short id, activity, created/modified stamps.

   `:focused?` marks the session you are CURRENTLY in (the active
   workspace). The render loop pins it to the top and paints it in the
   dialog accent + bold so it reads as 'you are here', visually distinct
   from the switch-to-other rows; its status shows `● focused`."
  [active-session-id session]
  (let [id (:id session)
        active? (= (str id) (some-> active-session-id str))]
    {:id (str "session:" id),
     :focused? active?,
     :title (session-title session),
     :session (short-session-id session),
     :status (if active? "● focused" (str (long (or (:turn-count session) 0)) " turns")),
     :created (navigator-stamp (:created-at session)),
     :modified (navigator-stamp (:modified-at session)),
     :target {:action :switch, :id id}}))
(defn- navigator-all-rows
  "Sessions arrive newest-modified-first from `tui-session-summaries`; keep
   that order so the navigator reads top-to-bottom by recency. Empty untitled
   shells are hidden by default; Alt+U in the navigator reveals them.

   No synthetic `+ New Session` row — creating a session is the `N`
   modifier (shown in the hint bar), not a list entry. A real-looking
   action row mixed in with actual sessions read as just another session
   and pushed the newest real one down."
  [{:keys [sessions active-session-id show-empty-untitled?]}]
  (let [focused-id (some-> active-session-id str)
        focused?   (fn [s] (= (str (:id s)) focused-id))
        rows (->> sessions
               ;; Keep the FOCUSED session even when it is an empty
               ;; untitled shell — you must always see "you are here".
               (remove #(and (not show-empty-untitled?)
                          (empty-untitled-session? %)
                          (not (focused? %))))
               (mapv #(navigator-session-row active-session-id %)))]
    ;; Focused row pinned to the top; the rest keep their recency order
    ;; and read as the "switch to" list below it.
    (vec (concat (filter :focused? rows) (remove :focused? rows)))))
(defn- navigator-visible-rows
  [rows query]
  (vec (filter #(table/row-matches? % query) rows)))
(defn- navigator-cell-spans
  "[[x-offset col-width] …] for each column inside a `boxed-row-line`, so
   cell text can be overlaid on a border-colored frame."
  [widths]
  (first
    (reduce (fn [[acc off] w] [(conj acc [off (long w)]) (+ off (long w) 3)])
      [[] 2] widths)))
(defn- navigator-with-selection-gutter
  "Reserve the shared selection-prefix gutter at the head of the first cell.
   `p/draw-selection-marker!` paints into that gutter on selected rows."
  [cells]
  (update (vec cells) 0 #(str (p/selection-prefix false) %)))
(defn- draw-navigator-row!
  "Draw one boxed row with frame + separators in the shared dialog border
   color and cell text in `text-fg` (bolded when `bold?`). Painting every
   box character one consistent color fixes the gray/black border flicker
   that came from drawing body rows in the text color."
  [g x row widths cells aligns text-fg bold?]
  (p/set-colors! g t/dialog-border t/dialog-bg)
  (p/put-str! g x row (table/boxed-row-line widths (vec (repeat (count widths) "")) aligns))
  (p/set-colors! g text-fg t/dialog-bg)
  (let [draw (fn []
               (doseq [[i [off w]] (map-indexed vector (navigator-cell-spans widths))]
                 (p/put-str! g (+ x off) row
                   (table/fit-cell (nth cells i "") w (nth aligns i :left)))))]
    (if bold? (p/styled g [p/BOLD] (draw)) (draw))))
(defn navigator-dialog!
  "Global Ctrl+G picker. Returns a target action map or nil on Esc."
  [^TerminalScreen screen opts]
  (let [query (atom "")
        selected (atom 0)
        scroll (atom 0)
        ;; Rows between the thumb top and the user's grip point, saved on
        ;; CLICK_DOWN so a drag keeps the cursor glued to the same spot on
        ;; the thumb. nil = not dragging.
        scrollbar-drag-offset (volatile! nil)
        show-empty-untitled? (atom (boolean (:show-empty-untitled? opts)))]
    (loop []
      (let [rows (navigator-all-rows (assoc opts :show-empty-untitled? @show-empty-untitled?))
            visible-rows (navigator-visible-rows rows @query)
            total (count visible-rows)
            size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows-n (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows-n "Sessions"
                     (navigator-content-w cols) (default-content-height rows-n))
            {:keys [left inner-w]} bounds
            ;; Input is pinned to a FIXED top position (no vertical
            ;; centering) so it never jumps as the row count changes
            ;; while typing. The table grows DOWN from there.
            {:keys [content-top content-h hint-row]} (dialog-layout bounds)
            table? (pos? total)
            query-row content-top
            ;; Reserve a right-side scrollbar gutter so the bar is a
            ;; SEPARATE element, not painted on top of the table's right
            ;; border. `content-w` shrinks BOTH the query box and the
            ;; table by the gutter so their right edges stay aligned; the
            ;; bar then floats one blank column clear of that edge.
            sb-gutter 2
            content-w (max 1 (- inner-w sb-gutter))
            ;; Table is inset one column on each side, exactly matching the
            ;; query box; the selection marker lives INSIDE the left
            ;; padding, so there is no external gutter.
            table-x (+ left 2)
            table-w (max 1 (- content-w 2))
            table-body-w (max 1 (- table-w 2))
            ;; table-x+table-w-1 is the table's right border; +1 leaves a
            ;; blank separator column before the bar.
            scrollbar-col (+ table-x table-w 1)
            table-widths (table/column-widths navigator-columns (max 1 table-body-w))
            aligns (mapv #(or (:align %) :left) navigator-columns)
            top-row (+ content-top 4)
            header-row (inc top-row)
            sep-row (inc header-row)
            body-top (inc sep-row)
            ;; Height = actual row count (capped) so the bottom border is
            ;; glued to the last row instead of floating below blanks.
            body-h (clamp total 1 (max 1 (- content-h 8)))
            bottom-row (+ body-top body-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % body-h total))]
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc left) content-top inner-w content-h)
        (let [cursor-pos (draw-text-input-field! g left query-row content-w @query (count @query))]
          (if-not table?
            ;; Empty state: no skeleton table, just a quiet line below input.
            (let [hidden-count (count (filter empty-untitled-session? (:sessions opts)))
                  msg (cond
                        (not (str/blank? @query)) "No matches"
                        (and (pos? hidden-count) (not @show-empty-untitled?)) "Only empty untitled sessions hidden"
                        :else "No sessions yet")
                  msg-x (+ table-x (max 0 (quot (- table-w (count msg)) 2)))]
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g msg-x (+ content-top 5) msg))
            (do
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/put-str! g table-x top-row (table/boxed-border-line table-widths :top))
              (draw-navigator-row! g table-x header-row table-widths
                (navigator-with-selection-gutter (mapv #(or (:label %) "") navigator-columns))
                aligns t/dialog-hint-key true)
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/put-str! g table-x sep-row (table/boxed-border-line table-widths :middle))
              (dotimes [i body-h]
                (let [idx (+ @scroll i)
                      row (+ body-top i)
                      selected? (= idx @selected)
                      entry (nth visible-rows idx)
                      ;; The currently-focused session paints in the dialog
                      ;; accent + bold (always), so it pops as "you are
                      ;; here" against the plain switch-to rows.
                      focused? (:focused? entry)
                      row-fg   (if focused? t/dialog-hint-key t/dialog-fg)
                      cells (navigator-with-selection-gutter
                              (mapv (fn [{:keys [id]}] (str (get entry id ""))) navigator-columns))]
                  (draw-navigator-row! g table-x row table-widths cells aligns row-fg (or selected? focused?))
                  ;; Shared cursor marker, painted by the project-wide primitive into
                  ;; the reserved `p/selection-prefix` first-cell gutter.
                  (p/draw-selection-marker! g (+ table-x 2) row selected? t/dialog-hint-key)))
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/put-str! g table-x bottom-row (table/boxed-border-line table-widths :bottom))
              (when (> total body-h)
                (scrollbar/draw! g
                  {:col scrollbar-col,
                   :top body-top,
                   :track-h body-h,
                   :total-h total,
                   :inner-h body-h,
                   :scroll @scroll}))))
          (draw-hint-bar! g
            left
            hint-row
            inner-w
            [["↑/↓" "move"] ["Enter" "open"] ["Ctrl+N" "new"]
             ["Alt+U" (if @show-empty-untitled? "hide empty" "show empty")]
             ["Esc" "cancel"]])
          (.setCursorPosition screen cursor-pos)
          (.refresh screen Screen$RefreshType/DELTA))
        (let [key (read-modal-key! screen)
              reset-list! (fn [] (reset! selected 0) (reset! scroll 0))]
          (when key
            (cond
              (modal-wheel-step key)
              (do (swap! selected #(clamp (+ % (modal-wheel-step key)) 0 (max 0 (dec total))))
                (recur))

              ;; Scrollbar mouse: grab the thumb, jump on a track click, and
              ;; follow drags. The bar is the only mouse target here; row
              ;; clicks fall through to the keyboard-driven flow. Dragging
              ;; also pulls the SELECTION into the new window so the next
              ;; render's `visible-window-start` doesn't yank scroll back to
              ;; the (now off-screen) selected row.
              (and (instance? MouseAction key) (> total body-h)
                (let [action (.getActionType ^MouseAction key)]
                  (or (= action MouseActionType/DRAG)
                    (= action MouseActionType/CLICK_RELEASE)
                    (and (= action MouseActionType/CLICK_DOWN)
                      (let [pos (.getPosition ^MouseAction key)]
                        (scrollbar/on-track? (.getColumn pos) (.getRow pos)
                          {:col scrollbar-col, :top body-top, :track-h body-h, :x-band 2}))))))
              (let [^MouseAction ma key
                    action (.getActionType ma)
                    pos (.getPosition ma)
                    mx (.getColumn pos)
                    my (.getRow pos)
                    geom (scrollbar/geometry total body-h body-h @scroll)
                    apply-scroll! (fn [grip]
                                    (let [ns (or (scrollbar/scroll-from-mouse-y
                                                   my body-top body-h total body-h grip)
                                               0)]
                                      (reset! scroll ns)
                                      (swap! selected
                                        #(clamp % ns (min (dec total) (+ ns (dec body-h)))))))]
                (cond
                  (= action MouseActionType/CLICK_RELEASE)
                  (do (vreset! scrollbar-drag-offset nil) (recur))

                  (and (= action MouseActionType/CLICK_DOWN) (some? geom)
                    (scrollbar/on-thumb? mx my {:col scrollbar-col, :top body-top, :x-band 2} geom))
                  (let [thumb-top (+ body-top (long (:thumb-top-rel geom)))]
                    (vreset! scrollbar-drag-offset (- my thumb-top))
                    (recur))

                  ;; Track click off the thumb: jump-to-position, then arm a
                  ;; centred grip so an immediate drag tracks naturally.
                  (= action MouseActionType/CLICK_DOWN)
                  (let [grip (long (quot (long (or (:thumb-h geom) 1)) 2))]
                    (vreset! scrollbar-drag-offset grip)
                    (apply-scroll! grip)
                    (recur))

                  (and (= action MouseActionType/DRAG) (some? @scrollbar-drag-offset))
                  (do (apply-scroll! (long @scrollbar-drag-offset)) (recur))

                  :else (recur)))

              ;; New session is a MODIFIER (Ctrl+N), not a list row and not a
              ;; bare key — so plain typing (incl. the letter `n`) filters.
              (and (input/ctrl-modifier? key)
                (= KeyType/Character (.getKeyType key))
                (some-> (.getCharacter key) Character/toLowerCase (= \n)))
              {:action :new}

              (input/alt-char? key \u)
              (do (swap! show-empty-untitled? not) (reset-list!) (recur))

              ;; Clipboard paste → append into the query filter.
              (input/paste-start? key)
              (do (let [pasted (drain-modal-paste! screen)]
                    (when (seq pasted)
                      (swap! query str (str/replace pasted #"\s+" " "))
                      (reset-list!)))
                (recur))

              :else
              (condp = (.getKeyType key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
                KeyType/Enter (when (pos? total) (:target (nth visible-rows @selected)))
                KeyType/Backspace (do (swap! query #(if (seq %) (subs % 0 (dec (count %))) %))
                                    (reset-list!) (recur))
                ;; Plain printable character → filter query. Skip control
                ;; chars and Alt/Ctrl-modified keys (those are commands).
                KeyType/Character (let [c (.getCharacter key)]
                                    (when (and c
                                            (not (input/alt-modifier? key))
                                            (not (input/ctrl-modifier? key))
                                            (not (Character/isISOControl c)))
                                      (swap! query str c)
                                      (reset-list!))
                                    (recur))
                (recur)))))))))
;;; ── Command palette ─────────────────────────────────────────────────────────
(def palette-commands
  "Command palette entries. Each is {:id keyword :label str}.
   Quit is intentionally NOT here - use Ctrl+C to quit.

   `:providers` opens router/model/auth configuration. Provider-owned
   knobs still live in Settings -> Providers & Models.

   Whole-session Markdown copy lives in the header as an icon,
   not in Ctrl+K."
  ;; In-session search lives in the upper bar (above messages), not in the command palette —
  ;; keystroke F3 / Shift+F3 / in-place input field. Removing it from this menu avoids two
  ;; entry points for the same action. Workspace palette entries (`:workspace`,
  ;; `:apply-workspace-to-trunk`, `:discard-workspace-{soft,hard}`)
  ;; are GONE. Workspace ops are slash-only now
  ;; (`/workspace new|apply|discard|...`) and surface through
  ;; `menu-commands` which aggregates them from the engine slash registry.
  [{:id :new-session, :label "New Session"} {:id :fork-session, :label "Fork Session"}
   {:id :switch-session, :label "Switch Session"} {:id :providers, :label "Configure Providers"}
   {:id :settings, :label "Settings"}])
(defn command-palette!
  "Show a command palette dialog. Returns the :id of the chosen command, or nil on Esc.
   No bespoke padding - `select-dialog!` runs at the shared default modal
   footprint, and `draw-list-item!` already fills the highlight stripe
   across the full inner width regardless of label length."
  ([^TerminalScreen screen] (command-palette! screen []))
  ([^TerminalScreen screen extra-commands]
   (let [commands (vec (concat palette-commands extra-commands))
         items (mapv (fn [cmd] {:label (:label cmd)}) commands)]
     (when-let [choice (select-dialog! screen "Commands" items)]
       (:id (nth commands (.indexOf ^java.util.List (mapv :label items) (:label choice))))))))
;;; ── Text viewer dialog ─────────────────────────────────────────────────────────
(defn text-viewer-dialog!
  "Show a scrollable read-only text viewer dialog.
   `title` is the dialog header. `text` is a string (may contain newlines)
   that is rendered VERBATIM - same content the LLM receives, only soft-
   wrapped to fit the dialog width. No markdown, no truncation, no
   reformatting.
   Returns nil on Esc. Supports keyboard scrolling."
  [^TerminalScreen screen title text]
  (let [scroll (atom 0)]
    (loop []
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            ;; Text viewer is the only dialog that should consume the
            ;; vertical room it can get - it scrolls long content. Ask
            ;; for terminal-bound height so the viewport is generous,
            ;; while still sharing the standard width.
            bounds (draw-dialog-chrome! g cols rows title (max 12 (- rows 8)))
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds)
            ;; Reserve the last inner column for a scrollbar that matches
            ;; the chat area's track+thumb style. Text wraps into the
            ;; remaining width so nothing collides with the bar.
            scroll-col (+ left inner-w)
            text-w (max 1 (- inner-w 3))
            lines (vec (mapcat #(render/wrap-text % text-w) (str/split-lines (or text "(empty)"))))
            total (count lines)
            max-scroll (max 0 (- total content-h))
            _ (swap! scroll #(clamp % 0 max-scroll))
            visible (subvec lines @scroll (min total (+ @scroll content-h)))]
        ;; Body - verbatim line render, no ellipsization (wrap-text
        ;; already produced lines that fit `text-w`).
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[i line] (map-indexed vector visible)]
          (let [row (+ content-top i)]
            (when (< row (+ content-top content-h))
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/put-str! g (+ left 2) row line))))
        ;; Clear remaining rows in the content area
        (doseq [row (range (+ content-top (count visible)) (+ content-top content-h))]
          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (p/fill-rect! g (inc left) row inner-w 1))
        ;; Scrollbar - same style as the chat messages area: a vertical
        ;; track of │ plus a solid █ thumb sized proportionally to the
        ;; visible window. Drawn over the content's right margin, on the
        ;; dialog background so it visually blends with the dialog frame.
        (when (> total content-h)
          (let [track-h content-h
                ratio (/ (double content-h) total)
                thumb-h (max 1 (int (* track-h ratio)))
                den (max 1 max-scroll)
                thumb-pos (int (* (- track-h thumb-h) (/ (double @scroll) den)))]
            (doseq [r (range track-h)]
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/set-char! g scroll-col (+ content-top r) Symbols/SINGLE_LINE_VERTICAL))
            (doseq [r (range thumb-h)]
              (p/set-colors! g t/dialog-hint-key t/dialog-bg)
              (p/set-char! g scroll-col (+ content-top thumb-pos r) \█))))
        (draw-hint-bar! g left hint-row inner-w [["↑/↓" "scroll"] ["Esc" "close"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape nil
              KeyType/ArrowUp (do (swap! scroll #(max 0 (dec %))) (recur))
              KeyType/ArrowDown (do (swap! scroll #(min max-scroll (inc %))) (recur))
              KeyType/Character (recur)
              (recur))))))))
;;; ── Copy dialog ─────────────────────────────────────────────────────────────
(defn- role-label [role] (name (or role :assistant)))
(defn- message-preview
  [{:keys [role text]}]
  (str (role-label role)
    ": "
    (-> (or text "")
      (str/replace #"\r?\n+" " ")
      str/trim)))
(defn- format-selected-messages
  [messages selected]
  (->> (range (count messages))
    (filter #(contains? selected %))
    (map (fn [idx]
           (let [{:keys [role text]} (nth messages idx)]
             (str (role-label role) ": " (or text "")))))
    (str/join "\n\n")))
(defn copy-dialog!
  "Show copy dialog for chat messages.
   Space toggles, A toggles all, Enter copies selected, Esc cancels."
  [^TerminalScreen screen messages]
  (let [items (vec messages)
        selected (atom 0)
        scroll (atom 0)
        checked (atom #{})
        ch (count items)]
    (loop [status [["Space" "toggle"] ["A" "all"] ["Enter" "copy"] ["Esc" "cancel"]]]
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows "Copy Messages" ch)
            {:keys [left inner-w]} bounds
            total (count items)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
            visible (min total content-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % content-h total))]
        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]
            (when (< idx total)
              (draw-checkbox-item! g
                left
                row
                inner-w
                (= idx @selected)
                (contains? @checked idx)
                (message-preview (nth items idx))))))
        (draw-hint-bar! g left hint-row inner-w status)
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (let [ktype (.getKeyType key)]
              (condp = ktype
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total))))
                                  (recur status))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                    (recur status))
                KeyType/Character
                (let [c (Character/toLowerCase (.getCharacter key))]
                  (cond (= c \space) (do (when (pos? total)
                                           (swap! checked (fn [s]
                                                            (if (contains? s @selected)
                                                              (disj s @selected)
                                                              (conj s @selected)))))
                                       (recur status))
                    (= c \a) (do (swap! checked
                                   (fn [s] (if (= (count s) total) #{} (set (range total)))))
                               (recur status))
                    :else (recur status)))
                KeyType/Enter (let [payload (format-selected-messages items @checked)]
                                (if (seq payload)
                                  (do (input/clipboard-copy! payload) true)
                                  (recur "No messages selected")))
                (recur status)))))))))
