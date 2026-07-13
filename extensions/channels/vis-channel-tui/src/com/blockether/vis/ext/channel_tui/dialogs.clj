(ns com.blockether.vis.ext.channel-tui.dialogs
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.magit :as magit]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.theme :as shared-theme]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna Symbols TerminalPosition]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
           [java.text SimpleDateFormat]
           [java.util Locale TimeZone]))
;;; ── Shared dialog chrome & components ───────────────────────────────────────

(defn- abbreviate-home
  "Shorten an absolute path for DISPLAY by replacing the user's home dir with
   `~`, matching the footer/navigator and web filesystem-roots rail."
  [^String path]
  (let [path
        (str path)

        home
        (System/getProperty "user.home")]

    (if (and (seq path) home (str/starts-with? path home))
      (str "~" (subs path (count home)))
      path)))
;;; ── Default modal footprint ─────────────────────────────────────────────────
;;
;; Every modal in the TUI shares ONE default WIDTH. HEIGHT is now ADAPTIVE:
;; the default arity of `draw-dialog-chrome!` sizes each box to the caller's
;; content height (clamped to a small floor and the terminal), so a 2-line
;; confirm is a compact card while a long list grows and then scrolls. That
;; kills the wasted whitespace of the old uniform footprint without bringing
;; back the "breathing" — the box tracks its content, not the cursor.
;;
;; Callers that genuinely want the shared FULL-height footprint (spacious
;; logo / welcome screens, long scrollable browsers) pass `nil` as the
;; content height to opt back in; the fully explicit width+height arity is
;; still there for a bespoke size.
(defn default-content-width
  "Shared content width every dialog uses, derived from `cols`. Clamped
   between the theme's dialog min/max widths and bounded by the terminal so
   the box never paints off-screen."
  [cols]
  (let [terminal-w
        (max 40 (- cols 4))

        min-w
        (min t/dialog-min-width terminal-w)

        box-w
        (-> (int (* cols t/dialog-width-ratio))
            (max min-w)
            (min t/dialog-max-width)
            (min terminal-w))]

    (max 1 (- box-w t/dialog-chrome-w))))
(defn default-content-height
  "Shared content height every dialog uses, derived from `rows`.
   Clamped to a common modal footprint so dialogs keep equal height."
  [rows]
  (let [terminal-h
        (max 8 (- rows 4))

        min-h
        (min t/dialog-min-height terminal-h)

        box-h
        (-> (int (* rows t/dialog-height-ratio))
            (max min-h)
            (min t/dialog-max-height)
            (min terminal-h))]

    (max 1 (- box-h t/dialog-chrome-h))))
(defn clear-screen!
  "Fill the entire screen with terminal background. Call before sub-dialogs
   to cleanly replace the current dialog (wizard step pattern)."
  [^TerminalScreen screen]
  (let [size
        (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

        cols
        (.getColumns size)

        rows
        (.getRows size)

        g
        (.newTextGraphics screen)]

    (p/set-bg! g t/terminal-bg)
    (p/fill-rect! g 0 0 cols rows)
    (.refresh screen Screen$RefreshType/DELTA)))
(defn clamp [x lo hi] (max lo (min hi x)))
(defn ellipsize
  [s max-w]
  (let [txt
        (or s "")

        max-w
        (long max-w)]

    (cond (<= max-w 0) ""
          (<= (p/display-width txt) max-w) txt
          (= max-w 1) "..."
          :else (str (p/truncate-cols txt (dec max-w)) "..."))))
(def ^:private min-adaptive-content-h
  "Content-height floor for adaptive dialogs — the box never shrinks below
   this many content rows (≈ this + chrome tall), so a tiny popup still reads
   as a comfortable card instead of a cramped sliver."
  3)
(defn adaptive-content-height
  "Clamp a dialog's REQUESTED content height so the box sizes to its own
   content instead of the shared footprint.

   - `nil` requested -> the shared full-height footprint (`default-content-height`).
     Spacious logo / welcome screens and long browsers opt in this way.
   - a number -> clamped between `min-adaptive-content-h` and the terminal-bounded
     `dialog-max-height`, so short dialogs are compact and long ones still scroll."
  [rows requested]
  (if (nil? requested)
    (default-content-height rows)
    (let [terminal-box
          (max 8 (- rows 4))

          max-h
          (max 1 (- (min t/dialog-max-height terminal-box) t/dialog-chrome-h))

          floor
          (min min-adaptive-content-h max-h)]

      (clamp (long requested) floor max-h))))
(defn dialog-layout
  "Compute content area layout. When `content-count` is provided and smaller than
   the available height, content is vertically centered within the frame.
   Layout: border -> title bar -> top separator -> CONTENT -> bottom separator -> hint -> border."
  ([bounds] (dialog-layout bounds nil))
  ([{:keys [top bottom]} content-count]
   (let [raw-top
         (+ top 3)

         hint-row
         (- bottom 1)

         bot-sep-row
         (- bottom 2)

         content-bot
         (dec bot-sep-row)

         full-h
         (max 1 (inc (- content-bot raw-top)))

         v-offset
         (if (and content-count (< content-count full-h)) (quot (- full-h content-count) 2) 0)

         content-top
         (+ raw-top v-offset)

         ;; Usable height from centered top - never exceeds content-bot
         content-h
         (max 1 (inc (- content-bot content-top)))]

     {:content-top content-top
      :content-bottom content-bot
      :content-h content-h
      :hint-row hint-row})))
(defn visible-window-start
  [idx current-start visible-count total-count]
  (let [last-start
        (max 0 (- total-count visible-count))

        start
        (clamp current-start 0 last-start)]

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
(defn- key-type [key] (when (instance? KeyStroke key) (.getKeyType ^KeyStroke key)))

(defn- key-character [key] (when (instance? KeyStroke key) (.getCharacter ^KeyStroke key)))

(defn- lower-character [^Character c] (when c (Character/toLowerCase (.charValue c))))

(defn- lower-key-character [key] (lower-character (key-character key)))

(defn- iso-control-character?
  [^Character c]
  (boolean (and c (Character/isISOControl (.charValue c)))))

(def ^:private modal-pending-key (ThreadLocal/withInitial #(atom nil)))
(defn normalize-modal-key
  "Normalize raw terminal CR/LF/ESC character keystrokes to Lanterna
   Enter/Escape key types. Some terminals surface modal Enter/Escape as
   `KeyType/Character`; modal code should not need to special-case that."
  [key]
  (if (and key (not (instance? MouseAction key)) (= KeyType/Character (key-type key)))
    (case (key-character key)
      (\newline \return)
      (KeyStroke. KeyType/Enter)

      \u001B
      (KeyStroke. KeyType/Escape)

      key)
    key))
(defn modal-enter-key?
  [key]
  (let [key (normalize-modal-key key)]
    (and key (not (instance? MouseAction key)) (= KeyType/Enter (key-type key)))))
(defn modal-escape-key?
  [key]
  (let [key (normalize-modal-key key)]
    (and key (not (instance? MouseAction key)) (= KeyType/Escape (key-type key)))))
(def ^:private modal-close-bounds (ThreadLocal/withInitial #(atom nil)))
(defn modal-close-click?
  "True when `key` is a mouse click on the dialog close (✕) button."
  [key]
  (when (instance? MouseAction key)
    (let [a (.getActionType ^MouseAction key)]
      (when (= a MouseActionType/CLICK_RELEASE)
        (when-let [b @(.get ^ThreadLocal modal-close-bounds)]
          (let [pos (.getPosition ^MouseAction key)
                cx (.getColumn pos)
                cy (.getRow pos)]

            (and (= cy (:y b)) (>= cx (:x0 b)) (<= cx (:x1 b)))))))))

(def ^:private modal-close-hover (ThreadLocal/withInitial #(atom false)))

(defn update-modal-close-hover!
  "On a MOVE/DRAG event, set the thread-local close-hover flag when the cursor
   is within the recorded close-button bounds, clear it otherwise. Lets the
   modal close (X) button light up on hover like the header/overlay buttons."
  [key]
  (when (instance? MouseAction key)
    (let [a (.getActionType ^MouseAction key)]
      (when (or (= a MouseActionType/MOVE) (= a MouseActionType/DRAG))
        (let [b @(.get ^ThreadLocal modal-close-bounds)
              pos (.getPosition ^MouseAction key)
              cx (.getColumn pos)
              cy (.getRow pos)
              hit? (boolean (and b (= cy (:y b)) (>= cx (:x0 b)) (<= cx (:x1 b))))
              cell (.get ^ThreadLocal modal-close-hover)]

          (when (not= @cell hit?) (clojure.core/reset! cell hit?) true))))))

(defn read-modal-input!
  "Read one modal input event. Consecutive pending wheel events are drained
   and returned as one `:scroll-delta`, so a wheel flood costs one redraw.
   The first non-wheel event encountered while draining is held for the next
   modal read on this thread. MOVE/DRAG events also refresh the close (X)
   hover flag so the button can light up under the cursor."
  [^TerminalScreen screen]
  (let [pending-key
        (.get ^ThreadLocal modal-pending-key)

        key
        (normalize-modal-key (or @pending-key (.readInput screen)))]

    (reset! pending-key nil)
    (update-modal-close-hover! key)
    (cond (modal-close-click? key) {:key (KeyStroke. KeyType/Escape)}
          :else (if-let [delta (modal-wheel-delta key)]
                  (loop [acc (long delta)]
                    (if-let [next-key (some-> (.pollInput screen)
                                              normalize-modal-key)]
                      (if-let [next-delta (modal-wheel-delta next-key)]
                        (recur (+ acc (long next-delta)))
                        (do (reset! pending-key next-key) {:scroll-delta acc}))
                      {:scroll-delta acc}))
                  {:key key}))))
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
        (cond (nil? k) (.toString sb)
              (input/paste-end? k) (.toString sb)
              :else (do (when-let [ch (input/keystroke->paste-char k)]
                          (.append sb ^String ch))
                        (recur)))))))
(defn fit-hint-pairs
  "Longest prefix of `[key action]` hint pairs whose rendered width (with
   '  \u00b7  ' separators) fits in `text-w` columns. `put-str!` clips to the
   SCREEN, not the dialog box, so a footer wider than the content area must
   drop whole trailing chords instead of painting across the border."
  [hint text-w]
  (let [sep-w
        (p/display-width "  \u00b7  ")

        seg-w
        (fn [[k a]]
          (+ (p/display-width k) 1 (p/display-width a)))

        pairs
        (vec hint)]

    (loop [i
           0

           used
           0]

      (if (>= i (count pairs))
        pairs
        (let [w (+ (seg-w (nth pairs i)) (if (pos? i) sep-w 0))]
          (if (> (+ used w) (long text-w)) (subvec pairs 0 i) (recur (inc i) (+ used w))))))))

(defn draw-hint-bar!
  "Draw hint bar. `hint` can be:
   - a string: rendered as-is, left-aligned
   - a vec of strings: centered, dim italic, joined with ' \u00b7 '
   - a vec of [key action] pairs: key bold, action dim italic, the whole
     run centered with thin ' \u00b7 ' separators between pairs

   Hints are CENTERED (not full-width justified) so short hint sets read as
   one tidy line instead of being stretched ragged across the dialog.
   Examples:
     \"simple hint\"
     [\"move\" \"select\" \"cancel\"]
     [[\"Up/Dn\" \"move\"] [\"Enter\" \"select\"] [\"Esc\" \"cancel\"]]"
  [g left row inner-w hint]
  (let [text-w
        (max 0 (- inner-w 2))

        text-x
        (+ left 2)

        sep
        "  \u00b7  "

        sep-w
        (p/display-width sep)]

    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (cond
      ;; Plain string
      (string? hint) (p/put-str! g text-x row (ellipsize hint text-w))
      ;; Vec of [key action] pairs - key bold, action dim italic, centered.
      ;; Clipped to whole pairs that fit `text-w` (see `fit-hint-pairs`).
      (and (vector? hint) (seq hint) (vector? (first hint)))
      (let [pairs
            (fit-hint-pairs hint text-w)

            n
            (count pairs)

            seg-w
            (fn [[k a]]
              (+ (p/display-width k) 1 (p/display-width a)))

            total
            (+ (reduce + (map seg-w pairs)) (* sep-w (max 0 (dec n))))

            start
            (+ text-x (max 0 (quot (- text-w total) 2)))]

        (loop [i
               0

               col
               start]

          (when (< i n)
            (let [[k a]
                  (nth pairs i)

                  next-col
                  (+ col (seg-w (nth pairs i)))]

              ;; Key part - bold, stronger color
              (p/set-fg! g t/dialog-hint-key)
              (p/styled g [p/BOLD] (p/put-str! g col row k))
              ;; Action part - dim hint color, italic
              (p/set-fg! g t/dialog-hint)
              (p/styled g [p/ITALIC] (p/put-str! g (+ col (p/display-width k)) row (str " " a)))
              ;; Separator between pairs
              (when (< i (dec n)) (p/set-fg! g t/dialog-hint) (p/put-str! g next-col row sep))
              (recur (inc i) (+ next-col sep-w))))))
      ;; Vec of strings - centered, dim italic, dot-joined, clipped to fit.
      (vector? hint) (let [joined
                           (ellipsize (apply str (interpose sep hint)) text-w)

                           start
                           (+ text-x (max 0 (quot (- text-w (p/display-width joined)) 2)))]

                       (p/set-fg! g t/dialog-hint)
                       (p/styled g [p/ITALIC] (p/put-str! g start row joined))))))
(defn hint-bar-width
  "Natural rendered width (chars) of a `draw-hint-bar!` hint — a plain string,
   a vec of strings, or a vec of `[key action]` pairs — using the SAME segment
   and separator math the hint bar paints with. Lets a dialog size its box to
   the footer instead of a fixed terminal ratio."
  [hint]
  (let [sep-w (p/display-width "  \u00b7  ")]
    (cond (string? hint) (p/display-width hint)
          (and (vector? hint) (seq hint) (vector? (first hint)))
          (+ (reduce +
                     (map (fn [[k a]]
                            (+ (p/display-width k) 1 (p/display-width a)))
                          hint))
             (* sep-w (max 0 (dec (count hint)))))
          (vector? hint) (+ (reduce + (map p/display-width hint))
                            (* sep-w (max 0 (dec (count hint)))))
          :else 0)))

(defn footer-content-width
  "Content width for an action-footer dialog: sized so the box is EXACTLY the
   footer's natural width plus two columns of padding on each side, never
   narrower than `min-content` (the widest content line) nor wider than the
   terminal. The `+2` supplies the extra pad beyond the single-column gutter
   `draw-dialog-chrome!` already reserves inside the border, so a footer of
   width W yields 2 blank columns between the frame and the hints on each side."
  ([cols hint] (footer-content-width cols hint 0))
  ([cols hint min-content]
   (-> (+ (hint-bar-width hint) 2)
       (max (long min-content))
       (min (max 1 (- cols 8))))))
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
  ([g left row inner-w selected? label] (draw-list-item! g left row inner-w selected? label nil))
  ([g left row inner-w selected? label hint]
   ;; `hint` (optional) is a dim, right-aligned chip — e.g. a command's keybind
   ;; — drawn opposite the label (opencode's justify-between rows). The label is
   ;; truncated so it never collides with the hint.
   (let [prefix
         (p/selection-prefix selected?)

         hint
         (some-> hint
                 str
                 not-empty)

         hint-w
         (if hint (+ 2 (long (p/display-width hint))) 0)

         draw-text
         (ellipsize (str prefix label) (max 0 (- inner-w 2 hint-w)))]

     (p/set-colors! g t/dialog-fg t/dialog-bg)
     (p/fill-rect! g (inc left) row inner-w 1)
     (if selected?
       (p/styled g [p/BOLD] (p/put-str! g (inc left) row draw-text))
       (p/put-str! g (inc left) row draw-text))
     (when hint
       (p/set-colors! g t/dialog-hint t/dialog-bg)
       (p/put-str! g (- (+ left inner-w) (long (p/display-width hint))) row hint)
       (p/set-colors! g t/dialog-fg t/dialog-bg)))))
(defn- draw-checkbox-item!
  ;; `> [✓] label` when selected, `  [✓] label` otherwise. The cursor
  ;; glyph and the checkbox glyph carry independent meaning: the
  ;; first says "this row is the cursor", the second says "this
  ;; option is currently on". Drop the accent-bg highlight — the `>`
  ;; alone is the selection cue. Anchored at `(inc left)` so the
  ;; marker sits right at the dialog's inner edge (see `draw-list-item!`).
  [g left row inner-w selected? checked? label]
  (let [mark
        (if checked? "✓" " ")

        prefix
        (str (p/selection-prefix selected?) "[" mark "] ")

        draw-text
        (ellipsize (str prefix label) (max 0 (- inner-w 2)))]

    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (if selected?
      (p/styled g [p/BOLD] (p/put-str! g (inc left) row draw-text))
      (p/put-str! g (inc left) row draw-text))))
(defn- draw-text-input-field!
  ;; BORDERLESS query field (opencode-style dialog input): a single prompt line,
  ;; no box. A dim "›" leads it; `placeholder` fills it while the text is empty.
  ;; Drawn on `row`; the caller reserves the surrounding rows as margin.
  ([g left row inner-w text cursor] (draw-text-input-field! g left row inner-w text cursor nil))
  ([g left row inner-w text cursor placeholder]
   (let [prompt
         "› "

         pw
         (count prompt)

         field-left
         (+ left 1)

         text-left
         (+ field-left pw)

         text-w
         (max 1 (- inner-w 2 pw 1))

         h-off
         (max 0 (- cursor (dec text-w)))

         visible
         (subs text h-off (min (count text) (+ h-off text-w)))]

     (p/set-colors! g t/dialog-fg t/dialog-bg)
     (p/fill-rect! g field-left row (max 1 (- inner-w 2)) 1)
     (p/set-colors! g t/dialog-hint t/dialog-bg)
     (p/put-str! g field-left row prompt)
     (if (and placeholder (zero? (count text)))
       (do (p/set-colors! g t/dialog-hint t/dialog-bg)
           (p/put-str! g text-left row (ellipsize (str placeholder) text-w)))
       (do (p/set-colors! g t/dialog-fg t/dialog-bg) (p/put-str! g text-left row visible)))
     (p/cursor-pos (+ text-left (- cursor h-off)) row))))
(defn draw-dialog-close-button!
  "Paint a clickable X close button at a dialog's top-right title row and
   record its click bounds (thread-local) so `read-modal-input!` can turn a
   click into Escape. Every dialog inherits it via `draw-dialog-chrome!`.
   Lights up to the red pill (`close-button-hover-fg` + bold) when the
   thread-local close-hover flag is set - the same affordance the header and
   help/tasks overlay close buttons use - so modal X buttons are no longer
   static."
  [g box-right title-row]
  (let [label
        " \u2715 "

        x1
        (- box-right 1)

        x0
        (- x1 (dec (count label)))

        hovered?
        @(.get ^ThreadLocal modal-close-hover)]

    (p/clear-styles! g)
    (p/set-colors! g
                   (if hovered? t/header-active-tab-fg t/dialog-title-bg)
                   (if hovered? t/close-button-hover-fg t/dialog-title-fg))
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g x0 title-row label)
    (p/clear-styles! g)
    (reset! (.get ^ThreadLocal modal-close-bounds) {:x0 x0 :x1 x1 :y title-row})))
(defn draw-dialog-chrome!
  "Draw dialog background, shadow, border, and title.

   Three arities:
   - `(g cols rows title content-h)` - shared default width; the box HEIGHT is
     sized to `content-h` via `adaptive-content-height`. Pass `nil` as
     `content-h` for the shared full-height footprint.
   - `(g cols rows title content-w content-h)` - fully explicit. Use
     only when a dialog genuinely needs a non-default width.

   Returns {:left :top :right :bottom :inner-w :inner-h}."
  ([g cols rows title content-h]
   (draw-dialog-chrome! g
                        cols
                        rows
                        title
                        (default-content-width cols)
                        (adaptive-content-height rows content-h)))
  ([g cols rows title content-w content-h]
   (let [[box-w box-h]
         (render/golden-dialog-size cols rows content-w content-h)

         box-left
         (quot (- cols box-w) 2)

         box-top
         (quot (- rows box-h) 2)

         box-right
         (+ box-left box-w -1)

         box-bottom
         (+ box-top box-h -1)

         inner-w
         (- box-w 2)]

     ;; Shadow - clipped to terminal bounds
     (let [shd-left
           (+ box-left 2)

           shd-top
           (inc box-top)

           shd-w
           (min box-w (- cols shd-left))

           shd-h
           (min box-h (- rows shd-top))]

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
     (let [title-row
           (inc box-top)

           title-text
           (ellipsize (or title "") (max 0 (- inner-w 2)))

           tx
           (+ box-left 1 (quot (- inner-w (count title-text)) 2))]

       ;; Accent bar background
       (p/set-bg! g t/dialog-title-bg)
       (p/fill-rect! g (inc box-left) title-row inner-w 1)
       ;; Title text - BOLD, matching the spel/blockether 700-weight header
       (p/set-fg! g t/dialog-title-fg)
       (p/styled g [p/BOLD] (p/put-str! g tx title-row title-text))
       (draw-dialog-close-button! g box-right title-row)
       ;; Top separator - below title bar
       (p/set-colors! g t/dialog-border t/dialog-bg)
       (p/draw-separator! g box-left box-right (inc title-row))
       ;; Bottom separator - above hint bar
       (let [bot-sep (- box-bottom 2)]
         (when (> bot-sep (+ box-top 3)) (p/draw-separator! g box-left box-right bot-sep))))
     {:left box-left
      :top box-top
      :right box-right
      :bottom box-bottom
      :inner-w inner-w
      :inner-h (- box-h 2)})))

(def vis-logo-lines
  "ASCII rendition of the real vis emblem (logo.png) followed by the
   wordmark. Drawn at the top of branded dialogs (the provider auth
   gate). Centered + accent-colored by the caller; auto-dropped when
   the terminal is too short."
  ["              .." "           ...  ..." "      .:-----:  :-----:." "   .-=-:.  .::--::.  .:-=-."
   "  -=-.   .--*###-.--.   .-=-" " -+:     :-*%#%%***-:     :+-" "  :=-.   .-+#*++*%+-:   .-=-"
   "    :==-...:-=+++-:...-==:" "   ....:----..::..----:...." "     .:.   :=-..-=:   .:."
   "             -===." "" "v i s"])

(defn draw-flat-dialog-chrome!
  "Flat variant of `draw-dialog-chrome!`: no drop shadow, no accent title
   stripe, no separators - one thin-bordered rect on the dialog background
   with the title inline on the top border. Same default footprint and the
   same bounds map as the boxed chrome, so `dialog-layout` works unchanged."
  [g cols rows title]
  (let [content-w
        (default-content-width cols)

        content-h
        (default-content-height rows)

        [box-w box-h]
        (render/golden-dialog-size cols rows content-w content-h)

        box-left
        (quot (- cols box-w) 2)

        box-top
        (quot (- rows box-h) 2)

        box-right
        (+ box-left box-w -1)

        box-bottom
        (+ box-top box-h -1)

        inner-w
        (- box-w 2)]

    (p/set-bg! g t/dialog-bg)
    (p/fill-rect! g box-left box-top box-w box-h)
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/draw-box! g box-left box-top box-w box-h)
    ;; Title sits flat ON the top border - no stripe row.
    (when (seq (str title))
      (let [txt (str " " (ellipsize title (max 0 (- inner-w 6))) " ")]
        (p/set-colors! g t/dialog-title-bg t/dialog-bg)
        (p/enable! g p/BOLD)
        (p/put-str! g (+ box-left 2) box-top txt)
        (p/clear-styles! g)))
    (draw-dialog-close-button! g box-right box-top)
    {:left box-left
     :top box-top
     :right box-right
     :bottom box-bottom
     :inner-w inner-w
     :inner-h (- box-h 2)}))
;;; ── Selection dialog ────────────────────────────────────────────────────────
(defn dialog-bounds
  "Pure geometry twin of `draw-dialog-chrome!` (explicit width+height arity):
   the box rectangle a `content-w`×`content-h` dialog occupies, computed WITHOUT
   painting. Lets a component measure its full layout — and reconcile a scroll
   window — before any drawing happens. Returns the SAME shape the chrome does
   ({:left :top :right :bottom :inner-w :inner-h}), from the same golden math."
  [cols rows content-w content-h]
  (let [[box-w box-h]
        (render/golden-dialog-size cols rows content-w content-h)

        box-left
        (quot (- cols box-w) 2)

        box-top
        (quot (- rows box-h) 2)]

    {:left box-left
     :top box-top
     :right (+ box-left box-w -1)
     :bottom (+ box-top box-h -1)
     :inner-w (- box-w 2)
     :inner-h (- box-h 2)}))

(defn run-modal!
  "Shared modal driver — the ONE event loop every ported dialog reuses instead
   of hand-rolling its own `loop/recur`. `component` is a map of PURE fns (they
   never touch the screen) plus one impure paint fn:

     :init      immutable start state (a map), or a 0-arg fn returning it
     :measure   (fn [state cols rows] -> geom)     — geometry, screen-free, TESTABLE
     :reconcile (fn [state geom] -> state)         — optional clamp (e.g. scroll window)
     :paint     (fn [g state geom] -> cursor|nil)  — the only impure piece; draws to `g`
     :on-key    (fn [state key geom] -> state | {::done result})  — screen-free, TESTABLE

   run-modal! owns everything the old dialogs copy-pasted: terminal sizing, the
   `TextGraphics`, wheel/close/Esc normalization (via `read-modal-key!`), the
   cursor + DELTA refresh, and the recur loop. A key handler returns the next
   state to continue, or `{::done v}` to close the modal with value `v` (nil on
   Esc/close). Because `:measure`/`:reconcile`/`:on-key` are pure functions of
   data, a dialog's geometry and key logic can be unit-tested with no live
   terminal at all — the React-like win."
  [^TerminalScreen screen {:keys [init measure reconcile paint on-key]}]
  (loop [state (if (fn? init) (init) init)]
    (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
          cols (.getColumns size)
          rows (.getRows size)
          geom (measure state cols rows)
          state (if reconcile (reconcile state geom) state)
          g (.newTextGraphics screen)
          cursor (paint g state geom)]

      ;; nil cursor HIDES the hardware cursor (no parked top-left blink — the
      ;; same fix applied to the magit buffer); a text field returns its cell.
      (.setCursorPosition screen cursor)
      (.refresh screen Screen$RefreshType/DELTA)
      (let [key (read-modal-key! screen)]
        (if (nil? key)
          (recur state)
          (let [r (on-key state key geom)]
            (if (and (map? r) (contains? r ::done)) (::done r) (recur r))))))))

(defn select-modal-component
  "Build the `run-modal!` component behind `list-dialog!` — a scrollable,
   selectable, optionally type-to-filter list. This is the pure-fn heart of the
   dialog: its `:measure` (geometry), `:reconcile` (scroll window) and `:on-key`
   (navigation / filtering / select) are plain functions of immutable state, so
   they can be exercised in tests WITHOUT a terminal. Only `:paint` touches the
   screen. `items`/opts match `list-dialog!`."
  [title items {:keys [filter? placeholder enter-label height]}]
  (let [items
        (vec items)

        content?
        (= height :content)

        head-rows
        (if filter? 2 0)]

    {:init {:query "" :selected 0 :scroll 0}
     :measure
     (fn [{:keys [query]} cols rows]
       (let [q
             (str/lower-case query)

             filtered
             (if (and filter? (not (str/blank? q)))
               (filterv #(str/includes? (str/lower-case (str (:label %))) q) items)
               items)

             total
             (count filtered)

             footer
             (cond-> []
               filter?
               (conj ["type" "filter"])

               true
               (conj ["↑/↓" "move"] ["Enter" (or enter-label "select")] ["Esc" "cancel"]))

             item-w
             (+ 4
                (reduce max
                        0
                        (map (fn [it]
                               (+ (p/display-width (str (:label it)))
                                  (if (:hint it) (+ 2 (p/display-width (str (:hint it)))) 0)))
                             items)))

             content-w
             (footer-content-width cols footer item-w)

             content-h-req
             (if content? (+ head-rows (min (count items) 16) 1) (adaptive-content-height rows nil))

             bounds
             (dialog-bounds cols rows content-w content-h-req)

             {:keys [content-top content-h hint-row]}
             (dialog-layout bounds)

             list-top
             (+ content-top head-rows)

             list-h
             (max 1 (- content-h head-rows 1))]

         {:cols cols
          :rows rows
          :title title
          :filtered filtered
          :total total
          :footer footer
          :content-w content-w
          :content-h-req content-h-req
          :bounds bounds
          :content-top content-top
          :content-h content-h
          :hint-row hint-row
          :list-top list-top
          :list-h list-h
          :filter? filter?
          :placeholder placeholder}))
     :reconcile (fn [state {:keys [total list-h]}]
                  (let [selected (clamp (:selected state) 0 (max 0 (dec total)))]
                    (assoc state
                      :selected selected
                      :scroll (visible-window-start selected (:scroll state) list-h total))))
     :paint (fn [g {:keys [selected scroll query]}
                 {:keys [cols rows title filtered total footer content-w content-h-req bounds
                         content-top content-h hint-row list-top list-h filter? placeholder]}]
              (let [{:keys [left right inner-w]} bounds]
                (draw-dialog-chrome! g cols rows title content-w content-h-req)
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g (inc left) content-top inner-w content-h)
                (let [cursor (when filter?
                               (draw-text-input-field! g
                                                       left
                                                       content-top
                                                       inner-w
                                                       query
                                                       (count query)
                                                       placeholder))]
                  (when filter?
                    (p/set-colors! g t/dialog-border t/dialog-bg)
                    (p/draw-separator! g left right (inc content-top)))
                  (dotimes [i (min list-h total)]
                    (let [idx (+ scroll i)
                          row (+ list-top i)]

                      (when (< idx total)
                        (let [item (nth filtered idx)]
                          (draw-list-item! g
                                           left
                                           row
                                           (if (> total list-h) (dec inner-w) inner-w)
                                           (= idx selected)
                                           (:label item)
                                           (:hint item))))))
                  (when (> total list-h)
                    (scrollbar/draw! g
                                     {:col (+ left inner-w)
                                      :top list-top
                                      :track-h list-h
                                      :total-h total
                                      :inner-h list-h
                                      :scroll scroll}))
                  (draw-hint-bar! g left hint-row inner-w footer)
                  cursor)))
     :on-key
     (fn [{:keys [selected query] :as state} key {:keys [total filtered]}]
       (let [clampf #(clamp % 0 (max 0 (dec total)))]
         (if-let [wheel (modal-wheel-step key)]
           (assoc state :selected (clampf (+ selected wheel)))
           (condp = (key-type key)
             KeyType/Escape {::done nil}
             KeyType/ArrowUp (assoc state :selected (clampf (dec selected)))
             KeyType/ArrowDown (assoc state :selected (clampf (inc selected)))
             KeyType/Enter {::done (when (pos? total) (nth filtered selected))}
             KeyType/Backspace (if filter?
                                 (assoc state
                                   :query (if (seq query) (subs query 0 (dec (count query))) query)
                                   :selected 0)
                                 state)
             KeyType/Character (if filter?
                                 (let [c (key-character key)]
                                   (if (and c (not (.isCtrlDown key)) (not (.isAltDown key)))
                                     (assoc state
                                       :query (str query c)
                                       :selected 0)
                                     state))
                                 state)
             state))))}))

(defn list-dialog!
  "Reusable scrollable, selectable list dialog — the SINGLE implementation
   behind `select-dialog!` (plain) and `searchable-select!` (type-to-filter).
   Now a THIN driver: `select-modal-component` supplies the pure geometry /
   scroll / key logic and `run-modal!` owns the loop. Returns the chosen item
   map (the full map, so callers recover `:id`/slash keys), or nil on Esc.

   `items` is a vec of maps with at least `:label`. opts:
     :filter?      type-to-filter on `:label`, case-insensitive (default false)
     :placeholder  query placeholder shown while the filter is empty
     :enter-label  hint-bar verb for Enter (default \"select\")
     :height       `:content` sizes the box to the item count (+ the query
                   field), capped; nil uses the shared (tall) footprint."
  [^TerminalScreen screen title items opts]
  (run-modal! screen (select-modal-component title items opts)))
(defn select-dialog!
  "Show a selection list dialog. Returns the selected item map or nil on Esc.
   `items` is a vec of `{:label str, …}` maps. Thin wrapper over `list-dialog!`."
  [^TerminalScreen screen title items]
  (list-dialog! screen title items {}))
;;; ── Multi-select dialog ─────────────────────────────────────────────────────
(defn multi-select-dialog!
  "Checkbox multi-select over `items` (vec of strings). Space toggles the
   cursor row, `a` toggles all, Enter confirms, Esc cancels. Returns the vec
   of selected strings (possibly empty) on confirm, nil on Esc. Mirrors the
   web modal's alias chips — same proposed options, multi-pick semantics."
  [^TerminalScreen screen title items]
  (let [items
        (vec items)

        total
        (count items)

        selected
        (atom 0)

        scroll
        (atom 0)

        checked
        (atom #{})]

    (loop []

      (let [size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows
            (.getRows size)

            g
            (.newTextGraphics screen)

            footer
            [["↑/↓" "move"] ["Space" "toggle"] ["a" "all"] ["Enter" "start"] ["Esc" "cancel"]]

            item-w
            (+ 6 (reduce max 0 (map #(p/display-width (str %)) items)))

            bounds
            (draw-dialog-chrome! g
                                 cols
                                 rows
                                 title
                                 (footer-content-width cols footer item-w)
                                 (adaptive-content-height rows (max 1 total)))

            {:keys [left inner-w]}
            bounds

            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds (max 1 total))

            visible
            (min total content-h)

            _
            (swap! selected #(clamp % 0 (max 0 (dec total))))

            _
            (swap! scroll #(visible-window-start @selected % content-h total))]

        (if (zero? total)
          (draw-list-item! g left content-top inner-w false "  (no options)")
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
                                     (nth items idx))))))
        (draw-hint-bar! g left hint-row inner-w footer)
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (if (nil? key)
            (recur)
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
              KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
              KeyType/Enter (mapv #(nth items %) (sort @checked))
              KeyType/Character
              (let [c (lower-key-character key)]
                (cond (= c \space) (do (when (pos? total)
                                         (swap! checked #(if (contains? % @selected)
                                                           (disj % @selected)
                                                           (conj % @selected))))
                                       (recur))
                      (= c \a) (do (swap! checked #(if (= (count %) total) #{} (set (range total))))
                                   (recur))
                      :else (recur)))
              (recur))))))))
;;; ── Managed-resource dialog (stop / restart by id) ─────────────────────────
(defn- startable-fields-form!
  "Single inline card form for a startable's declared `:fields` — the TUI twin of
   the web modal's inline form (replacing the old one-modal-per-field sequence).
   Every field shows at once as a label + input line; ↑/↓/Tab move between the
   fields and the Start action, typing edits the FOCUSED field, Enter submits
   (validating required fields), Esc cancels. A `*`-marked field is required; an
   empty field shows its placeholder dim as a hint. Returns `{(keyword name)
   value}` or ::cancel."
  [^TerminalScreen screen sr]
  (let [fields
        (vec (:fields sr))

        n
        (count fields)

        states
        (mapv (fn [f]
                (let [d (vec (str (or (:default f) "")))]
                  {:text (atom d) :cursor (atom (count d))}))
              fields)

        focus
        (atom 0)

        ;; 0..n-1 = fields, n = Start button
        paste
        (volatile! nil)

        val-of
        (fn [i]
          (str/trim (apply str @(:text (nth states i)))))

        title
        (str "Start " (:label sr))]

    (loop []

      (let [size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows
            (.getRows size)

            g
            (.newTextGraphics screen)

            ;; each field = label row + input row + gap row (3); + 1 Start row
            content-h
            (+ (* n 3) 1)

            bounds
            (draw-dialog-chrome! g cols rows title content-h)

            {:keys [left inner-w]}
            bounds

            {:keys [content-top hint-row]}
            (dialog-layout bounds content-h)

            body-w
            (max 1 (- inner-w 4))

            cursor-screen
            (atom nil)]

        (dotimes [i n]
          (let [f (nth fields i)
                st (nth states i)
                fy (+ content-top (* i 3))
                iy (inc fy)
                focused? (= @focus i)
                val (apply str @(:text st))]

            ;; label (accent when focused), with a * for required
            (p/set-colors! g (if focused? t/dialog-hint-key t/dialog-hint) t/dialog-bg)
            (p/fill-rect! g (inc left) fy inner-w 1)
            (p/put-str! g
                        (+ left 2)
                        fy
                        (ellipsize (str (or (:label f)
                                            (some-> (:name f)
                                                    name)
                                            "field")
                                        (when (:required f) " *"))
                                   body-w))
            ;; input line
            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/fill-rect! g (inc left) iy inner-w 1)
            (if focused?
              (reset! cursor-screen
                (draw-text-input-field! g (inc left) iy inner-w val @(:cursor st)))
              (if (str/blank? val)
                (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                    (p/put-str! g (+ left 3) iy (ellipsize (str (or (:placeholder f) "")) body-w)))
                (p/put-str! g (+ left 3) iy (ellipsize val body-w))))))
        ;; Add button — the primary action, centered and framed so it reads
        ;; as a button: a solid accent block when focused, accent-inked when not.
        (let [by
              (+ content-top (* n 3))

              focused?
              (= @focus n)

              label
              "  Add  "

              bw
              (count label)

              bx
              (+ (inc left) (max 0 (quot (- inner-w bw) 2)))]

          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (p/fill-rect! g (inc left) by inner-w 1)
          (if focused?
            (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
            (p/set-colors! g t/dialog-hint-key t/dialog-bg))
          (p/put-str! g bx by label))
        (draw-hint-bar! g
                        left
                        hint-row
                        inner-w
                        [["↑/↓/Tab" "field"] ["Enter" "add"] ["Esc" "cancel"]])
        (.setCursorPosition screen (or @cursor-screen (p/cursor-pos 0 0)))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [submit!
              (fn []
                (if-let [missing (first (filter #(and (:required (nth fields %))
                                                      (str/blank? (val-of %)))
                                                (range n)))]
                  (do (vis/notify! (str (or (:label (nth fields missing))
                                            (some-> (:name (nth fields missing))
                                                    name))
                                        " is required")
                                   :level :warn
                                   :ttl-ms 3000)
                      (reset! focus missing)
                      nil)
                  (into {}
                        (map (fn [i]
                               [(keyword (:name (nth fields i))) (val-of i)])
                             (range n)))))

              cur-field
              (when (< @focus n) (nth states @focus))

              key
              (read-modal-key! screen)]

          (cond (nil? key) (recur)
                ;; bracketed paste → focused field only
                (input/paste-start? key) (do (vreset! paste (StringBuilder.)) (recur))
                (input/paste-end? key) (let [^StringBuilder sb @paste]
                                         (when (and sb cur-field (pos? (.length sb)))
                                           (let [chars (vec (.toString sb))]
                                             (swap! (:text cur-field)
                                               (fn [t]
                                                 (into (subvec t 0 @(:cursor cur-field))
                                                       (concat chars
                                                               (subvec t @(:cursor cur-field))))))
                                             (swap! (:cursor cur-field) + (count chars))))
                                         (vreset! paste nil)
                                         (recur))
                (some? @paste) (do (when-let [ch (input/keystroke->paste-char key)]
                                     (.append ^StringBuilder @paste ch))
                                   (recur))
                :else (condp = (key-type key)
                        KeyType/Escape ::cancel
                        KeyType/Enter (or (submit!) (recur))
                        KeyType/Tab (do (swap! focus #(mod (inc %) (inc n))) (recur))
                        KeyType/ArrowDown (do (swap! focus #(mod (inc %) (inc n))) (recur))
                        KeyType/ArrowUp (do (swap! focus #(mod (+ % n) (inc n))) (recur))
                        KeyType/Character (do (when cur-field
                                                (let [c (key-character key)]
                                                  (swap! (:text cur-field)
                                                    #(into (subvec % 0 @(:cursor cur-field))
                                                           (cons c
                                                                 (subvec % @(:cursor cur-field)))))
                                                  (swap! (:cursor cur-field) inc)))
                                              (recur))
                        KeyType/Backspace (do (when (and cur-field (pos? @(:cursor cur-field)))
                                                (swap! (:text cur-field)
                                                  #(into (subvec % 0 (dec @(:cursor cur-field)))
                                                         (subvec % @(:cursor cur-field))))
                                                (swap! (:cursor cur-field) dec))
                                              (recur))
                        KeyType/ArrowLeft
                        (do (when cur-field (swap! (:cursor cur-field) #(max 0 (dec %)))) (recur))
                        KeyType/ArrowRight (do (when cur-field
                                                 (swap! (:cursor cur-field)
                                                   #(min (count @(:text cur-field)) (inc %))))
                                               (recur))
                        (recur))))))))

(defn- resource-status-mark
  "Leading status glyph + color for a managed-resource row — same ● language as
   the footer and the settings rows: ● status-ok = live/healthy, ● status-bad =
   errored, ○ dim = otherwise. Returns `[glyph color]`."
  [status]
  (case (some-> status
                name
                keyword)
    (:up :running :ok :active :ready :live :started)
    [p/STATUS_ON t/status-ok]

    (:error :failed :dead :crashed)
    [p/STATUS_ON t/status-bad]

    [p/STATUS_OFF t/dialog-hint]))

(defn start-resource-flow!
  "Generic 'start a new resource' flow. The daemon owns BOTH the startable
   registry and the session env, so we FETCH the startable descriptors over the
   gateway (each already carrying the options the daemon proposed from its env),
   render the pick / dir / options dialogs locally, then POST the collected choice
   back — the daemon runs the `:start-fn` so the spawned background registers in
   ITS registry and shows up in this same Backgrounds list (a locally-started one
   never would). No resource type is hardcoded here. Returns nil."
  [^TerminalScreen screen session-id]
  (let [startables (vec (try (vis/gateway-list-startables session-id)
                             (catch Throwable t
                               (tel/log! :warn
                                         ["dialogs: gateway-list-startables failed" (ex-message t)])
                               nil)))]
    (cond (empty? startables)
          (vis/notify! "No startable resources registered" :level :warn :ttl-ms 3000)
          :else
          (let [sr (if (= 1 (count startables))
                     (first startables)
                     (select-dialog! screen
                                     "Start resource"
                                     (mapv #(assoc %
                                              :label (str "Start "
                                                          (:label %)
                                                          (when-let [v (get-in % [:variant :label])]
                                                            (str " · " v))))
                                           startables)))]
            (when sr
              (let [root (str (:root sr))
                    dir (when (:dir? sr)
                          (let [r (startable-fields-form! screen
                                                          (assoc sr
                                                            :label (str (:label sr) " directory")
                                                            :fields [{:name "dir"
                                                                      :label "Directory"
                                                                      :default root
                                                                      :placeholder root}]))]
                            (if (= ::cancel r)
                              ::cancel
                              (let [d (str/trim (str (:dir r)))]
                                (if (str/blank? d) root d)))))]

                (when-not (= ::cancel dir)
                  (let [selected (cond (seq (:fields sr)) (startable-fields-form! screen sr)
                                       (:options? sr)
                                       (multi-select-dialog!
                                         screen
                                         (str (:label sr) " — " (or (:options-label sr) "options"))
                                         (or (:options sr) []))
                                       :else [])]
                    (when (and (some? selected) (not= ::cancel selected))
                      (let [res (try (vis/gateway-start-resource!
                                       session-id
                                       {:kind (:kind sr) :dir dir :selected (not-empty selected)})
                                     (catch Throwable t {:result "error" :message (ex-message t)}))]
                        (cond (= "error" (:result res)) (vis/notify! (str "Start failed: "
                                                                          (or (:message res)
                                                                              "unknown error"))
                                                                     :level :warn
                                                                     :ttl-ms 4000)
                              (= "started" (:result res))
                              (vis/notify! (str "Started " (or (:label res) (:label sr)))
                                           :level :info
                                           :ttl-ms 3000)
                              ;; The daemon boots the resource OFF the request thread
                              ;; (a REPL/nREPL start is slow) and answers "starting"
                              ;; at once; it appears in this list as it comes up.
                              (= "starting" (:result res))
                              (vis/notify! (str "Starting " (or (:label res) (:label sr)) "…")
                                           :level :info
                                           :ttl-ms 3000)))))))))))
  nil)
(declare text-view-dialog!)

(defn resources-dialog!
  "Modal list of THIS session's vis-managed resources (nREPLs, daemons, …).
   ↑/↓ move · s = stop · r = restart · Esc = close. Stop/restart go through
   `vis/stop-resource!` / `vis/restart-resource!` — the SAME canonical path the
   agent's `resource_stop`/`resource_restart` tools use, so footer, agent and
   this dialog all drive one definition. The list re-reads every loop, so an
   item vanishes the instant it's stopped."
  [^TerminalScreen screen session-id]
  (let [selected (atom 0)]
    (loop []

      ;; Cached read (750ms TTL): the dialog re-reads every loop iteration, so an
      ;; UNcached fetch would round-trip the daemon — with per-resource health
      ;; probes — on EVERY keystroke, making the list feel like it hangs. The
      ;; footer uses the same cached path.
      (let [items (vec (try (vis/gateway-list-resources-cached session-id)
                            (catch Throwable t
                              (tel/log! :warn
                                        ["dialogs: list-resources failed" session-id
                                         (ex-message t)])
                              nil)))
            total (count items)
            size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            ;; Fixed-size box (the 5-arg chrome ignores item count) drawn over the
            ;; live screen with a shadow — exactly like select-dialog! — so the
            ;; chat shows around it instead of a blank wipe. The box's own bg fill
            ;; clears its interior each frame, so a shrinking list leaves nothing.
            footer [["↑/↓" "move"] ["a" "add"] ["l" "logs"] ["s" "stop"] ["r" "restart"]
                    ["Esc" "close"]]
            bounds (draw-dialog-chrome! g
                                        cols
                                        rows
                                        "Backgrounds"
                                        (footer-content-width cols footer)
                                        (max 1 total))
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds (max 1 total))]

        (swap! selected #(clamp % 0 (max 0 (dec total))))
        (if (zero? total)
          (draw-list-item! g left content-top inner-w false "  (no managed resources)")
          (dotimes [i (min total content-h)]
            (let [r (nth items i)
                  row-y (+ content-top i)
                  selected? (= i @selected)
                  ;; `list-resources` returns STRING-keyed data maps (strings-only
                  ;; boundary); `detail` is a string-keyed sub-map too.
                  port (get-in r ["detail" "port"])
                  [glyph gcolor] (resource-status-mark (get r "status"))
                  ;; kind reads as a TYPE prefix, then the readable name, then
                  ;; the dim port + status — the ● glyph already says live/errored.
                  label (str (get r "kind")
                             "  "
                             (get r "label")
                             (when port (str "  :" port))
                             "  "
                             (get r "status"))
                  actions (str (when (get r "can_logs") "[l] logs  ")
                               (if (get r "can_restart") "[r] restart  [s] stop" "[s] stop"))
                  action-w (count actions)]

              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc left) row-y inner-w 1)
              (p/set-colors! g t/dialog-hint-key t/dialog-bg)
              (p/draw-selection-marker! g (inc left) row-y selected?)
              (let [label-x (p/status-mark! g (+ left 3) row-y glyph gcolor t/dialog-bg)
                    action-x (max label-x (- (+ left inner-w) action-w 1))
                    label-w (max 1 (- action-x label-x 2))
                    lbl (ellipsize label label-w)]

                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (if selected?
                  (p/styled g [p/BOLD] (p/put-str! g label-x row-y lbl))
                  (p/put-str! g label-x row-y lbl))
                (p/set-colors! g t/dialog-hint t/dialog-bg)
                (p/put-str! g action-x row-y actions)))))
        (draw-hint-bar! g left hint-row inner-w footer)
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (if (nil? key)
            (recur)
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
              KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
              KeyType/Character
              (let [c (lower-key-character key)]
                (if (= c \a)
                  ;; start a NEW resource — available even with 0 resources.
                  ;; Fully generic: drives the declarative startable registry
                  ;; (pick type if >1, propose options, call its start-fn).
                  (do (start-resource-flow! screen session-id) (recur))
                  (do
                    (when (pos? total)
                      (let [r (nth items (clamp @selected 0 (dec total)))]
                        (cond
                          (= c \s) (do (vis/gateway-stop-resource! session-id (get r "id"))
                                       (vis/notify! (str "Stopped " (get r "label"))
                                                    :level :info
                                                    :ttl-ms 3000))
                          (= c \l)
                          (when (get r "can_logs")
                            (let
                              [rid (get r "id")
                               ;; logs-fn runs OFF the UI thread under a hard
                               ;; deadline — a wedged resource can never hang F4.
                               fetch
                               #(let [f (future (vis/gateway-resource-logs session-id rid))
                                      v (deref f 3000 ::log-fetch-timeout)]

                                  (if (= ::log-fetch-timeout v)
                                    ["… log fetch timed out (3s) — the resource's logs-fn is stuck …"]
                                    (seq v)))
                               lines (fetch)]

                              (if lines
                                (text-view-dialog! screen
                                                   (str "Logs — " (get r "label"))
                                                   lines
                                                   :refresh-fn fetch
                                                   :tail? true)
                                (vis/notify! "No output captured yet" :level :info :ttl-ms 3000))))
                          (= c \r) (when (get r "can_restart")
                                     (vis/gateway-restart-resource! session-id (get r "id"))
                                     (vis/notify! (str "Restarted " (get r "label"))
                                                  :level :info
                                                  :ttl-ms 3000)))))
                    (recur))))
              (recur))))))))
;;; ── Read-only text viewer dialog ────────────────────────────────────────────
(defn text-view-dialog!
  "Show read-only lines in a scrollable modal. Returns nil after close.

   Keys: ↑/↓ line, PgUp/PgDn page, Home/End jump, mouse-wheel scroll,
   Enter/Esc close. Options:
   - :refresh-fn  thunk returning fresh lines — enables [r] refresh so a live
                  buffer (e.g. background logs) can be re-pulled in place.
   - :tail?       start pinned to the newest line and re-follow the bottom on
                  refresh (log-tail behaviour); scrolling up releases the pin."
  [^TerminalScreen screen title lines & {:keys [refresh-fn tail?]}]
  (let [lines*
        (atom (vec lines))

        scroll
        (atom 0)

        follow
        (atom (boolean tail?))]

    (loop []

      (let [size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows
            (.getRows size)

            g
            (.newTextGraphics screen)

            cur-lines
            @lines*

            bounds
            (draw-dialog-chrome! g cols rows title (max 8 (count cur-lines)))

            {:keys [left inner-w]}
            bounds

            text-w
            (max 1 (- inner-w 2))

            wrapped
            (vec (mapcat (fn [line]
                           (if (str/blank? (str line)) [""] (render/wrap-text (str line) text-w)))
                         (or cur-lines [])))

            total
            (count wrapped)

            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds total)

            visible
            (min total content-h)

            max-scroll
            (max 0 (- total visible))

            _
            (when @follow (reset! scroll max-scroll))

            _
            (swap! scroll #(clamp % 0 max-scroll))]

        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]

            (when (< idx total)
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/put-str! g (+ left 2) row (ellipsize (nth wrapped idx) text-w)))))
        (scrollbar/draw! g
                         {:col (+ left inner-w)
                          :top content-top
                          :track-h content-h
                          :total-h total
                          :inner-h content-h
                          :scroll @scroll})
        (draw-hint-bar! g
                        left
                        hint-row
                        inner-w
                        (cond-> [["↑/↓" "scroll"] ["PgUp/PgDn" "page"]]
                          refresh-fn
                          (conj ["r" (if @follow "tailing" "refresh")])

                          :always
                          (conj ["Enter/Esc" "close"])))
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key
              (read-modal-key! screen)

              wheel
              (modal-wheel-step key)

              move!
              (fn [f]
                (reset! follow false)
                (swap! scroll #(clamp (f %) 0 max-scroll)))]

          (cond (nil? key) (recur)
                wheel (do (move! #(+ % wheel)) (recur))
                :else (condp = (key-type key)
                        KeyType/Escape nil
                        KeyType/Enter nil
                        KeyType/ArrowUp (do (move! dec) (recur))
                        KeyType/ArrowDown (do (move! inc) (recur))
                        KeyType/PageUp (do (move! #(- % (max 1 content-h))) (recur))
                        KeyType/PageDown (do (move! #(+ % (max 1 content-h))) (recur))
                        KeyType/Home (do (reset! follow false) (reset! scroll 0) (recur))
                        KeyType/End
                        (do (reset! follow (boolean tail?)) (reset! scroll max-scroll) (recur))
                        KeyType/Character (do (when (and refresh-fn
                                                         (= (lower-key-character key) \r))
                                                (reset! lines* (vec (refresh-fn)))
                                                (when tail? (reset! follow true)))
                                              (recur))
                        (recur))))))))
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
   :body string-or-lines rendered above the input label,
   :logo lines drawn centered in the accent color above the body,
   :flat? true for the flat chrome (no shadow / title stripe)."
  [^TerminalScreen screen title label & {:keys [mask initial body logo flat?] :or {initial ""}}]
  (let [text
        (atom (vec initial))

        cursor
        (atom (count initial))

        body-lines
        (text-input-body-lines body)

        paste-buffer
        (volatile! nil)]

    (loop []

      (let [size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows
            (.getRows size)

            g
            (.newTextGraphics screen)

            ;; Content: optional logo + body rows + label row + spacer + 3-row bordered input box.
            ;; Pre-estimate the content height (at the default width) so the box
            ;; adapts to a plain prompt; a logo keeps the spacious footprint (nil).
            est-w
            (max 1 (- (default-content-width cols) 2))

            est-body
            (->> body-lines
                 (mapcat (fn [line]
                           (if (str/blank? line) [""] (render/wrap-text line est-w))))
                 vec)

            req-h
            (when-not (seq logo) (+ 4 (if (seq est-body) 1 0) (count est-body)))

            bounds
            (if flat?
              (draw-flat-dialog-chrome! g cols rows title)
              (draw-dialog-chrome! g cols rows title req-h))

            {:keys [left inner-w]}
            bounds

            text-w
            (max 1 (- inner-w 2))

            wrapped-body
            (->> body-lines
                 (mapcat (fn [line]
                           (if (str/blank? line) [""] (render/wrap-text line text-w))))
                 vec)

            body-gap
            (if (seq wrapped-body) 1 0)

            full-h
            (:content-h (dialog-layout bounds))

            ;; Drop the logo before clipping the body when the terminal is short.
            logo-lines
            (if (and (seq logo) (<= (+ 5 body-gap (count logo)) full-h)) (mapv str logo) [])

            logo-block
            (if (seq logo-lines) (inc (count logo-lines)) 0)

            content-count
            (+ 4 body-gap logo-block (count wrapped-body))

            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds content-count)

            max-body-lines
            (max 0 (- content-h 4 body-gap logo-block))

            visible-body
            (if (<= (count wrapped-body) max-body-lines)
              wrapped-body
              (conj (vec (take (max 0 (dec max-body-lines)) wrapped-body)) "..."))

            body-top
            (+ content-top logo-block)

            label-row
            (+ body-top (count visible-body) body-gap)

            input-row
            (inc label-row)

            txt
            (apply str @text)

            display
            (if mask (apply str (repeat (count txt) mask)) txt)

            cursor-pos
            (draw-text-input-field! g (inc left) input-row inner-w display @cursor)]

        (when (seq logo-lines)
          ;; Brand accent for the `v i s` emblem — visible on every theme's
          ;; dialog body (indigo on light, sky on dark). Matches show-welcome!.
          (p/set-colors! g t/header-active-tab-accent t/dialog-bg)
          (p/enable! g p/BOLD)
          (doseq [[idx line] (map-indexed vector logo-lines)]
            (let [row (+ content-top idx)
                  lx (+ left 1 (max 0 (quot (- inner-w (p/display-width line)) 2)))]

              (p/put-str! g lx row (ellipsize line text-w))))
          (p/clear-styles! g))
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[idx line] (map-indexed vector visible-body)]
          (let [row (+ body-top idx)]
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
              ;; -- Bracketed paste ------------------------------
              ;; Three-state machine matching the main input loop.
              ;; START -> open buffer; END -> flush into text.
              ;; Prevents PUA marker chars (\uE200, \uE201) from
              ;; leaking into the dialog value - they break HTTP
              ;; Authorization headers when pasted API keys carry
              ;; them into the Bearer token.
              (input/paste-start? key) (do (vreset! paste-buffer (StringBuilder.)) (recur))
              (input/paste-end? key)
              (let [^StringBuilder sb @paste-buffer]
                (when sb
                  (let [payload (.toString sb)
                        chars (vec payload)]

                    (vreset! paste-buffer nil)
                    (when-not (.isEmpty payload)
                      (swap! text (fn [t]
                                    (into (subvec t 0 @cursor) (concat chars (subvec t @cursor)))))
                      (swap! cursor + (count chars)))))
                (recur))
              ;; Accumulate chars into the paste buffer while open.
              (some? @paste-buffer) (do (when-let [ch (input/keystroke->paste-char key)]
                                          (.append ^StringBuilder @paste-buffer ch))
                                        (recur))
              ;; -- Regular key dispatch -------------------------
              :else (condp = (key-type key)
                      KeyType/Escape nil
                      KeyType/Enter (str/trim (apply str @text))
                      KeyType/Character (let [c (key-character key)]
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
  (let [text
        (str " " label " ")

        w
        (count text)]

    (if selected?
      (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/put-str! g col row text)
    w))
(defn confirm-dialog!
  "Show Y/N confirmation with side-by-side buttons. Returns true/false, nil on Esc."
  [^TerminalScreen screen title message]
  (let [raw-lines
        (if (string? message) [message] message)

        btn-yes
        "Yes"

        btn-no
        "No"

        btn-w
        (+ 2 (max (count btn-yes) (count btn-no)))

        ;; " Yes " / " No  "
        btn-gap
        4

        ;; content: message lines + blank + button row = lines + 2
        ch
        (+ (count raw-lines) 2)

        focus
        (atom 0)]

    ;; 0 = Yes, 1 = No
    (loop []

      (let [size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows
            (.getRows size)

            g
            (.newTextGraphics screen)

            bounds
            (draw-dialog-chrome! g cols rows title ch)

            {:keys [left inner-w]}
            bounds

            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds ch)

            text-w
            (max 0 (- inner-w 2))

            lines
            (vec (mapcat #(render/wrap-text % text-w) raw-lines))

            btn-row
            (+ content-top (count lines) 1)

            ;; blank line then buttons
            ;; Center buttons horizontally
            total-btn-w
            (+ btn-w btn-gap btn-w)

            btn-start
            (+ left 1 (quot (- inner-w total-btn-w) 2))]

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
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/Enter (= @focus 0) ;; true if Yes focused
              KeyType/ArrowLeft (do (reset! focus 0) (recur))
              KeyType/ArrowRight (do (reset! focus 1) (recur))
              KeyType/Tab (do (swap! focus #(if (zero? %) 1 0)) (recur))
              KeyType/Character (let [c (lower-key-character key)]
                                  (cond (= c \y) true
                                        (= c \n) false
                                        :else (recur)))
              (recur))))))))

;;; ── Magit status dialog (C-x g / footer git button) ─────────────────────────

(def ^:private magit-hints
  "Hint-bar chords for the magit status buffer — the most important magit
   verbs, one key each, faithful to magit's own bindings."
  [["↑/↓" "move"] ["TAB" "diff"] ["s/u" "±stage"] ["S/U" "all"] ["x" "discard"] ["c" "commit"]
   ["P" "push"] ["F" "pull"] ["f" "fetch"] ["b" "branch"] ["z" "stash"] ["g" "refresh"]
   ["Esc" "close"]])

(defn- magit-row-style
  "`[fg bold? bg]` for one status-buffer row — foreground, bold?, and an optional
   background (diff add/remove lines get the subtle green/red band magit uses)."
  [{:keys [kind text]}]
  (case kind
    :repo
    [t/dialog-fg true]

    :section
    [t/dialog-hint-key true]

    :diff
    (let [t (str/triml (str text))]
      (cond
        ;; diff/file headers read as plain hints, never as add/remove lines
        (or (str/starts-with? t "+++")
            (str/starts-with? t "---")
            (str/starts-with? t "diff ")
            (str/starts-with? t "index ")
            (str/starts-with? t "new file")
            (str/starts-with? t "deleted file")
            (str/starts-with? t "rename ")
            (str/starts-with? t "similarity "))
        [t/dialog-hint false]
        (str/starts-with? t "@@") [t/dialog-hint-key false t/code-block-bg]
        (str/starts-with? t "+") [t/code-success-fg false t/code-ok-bg]
        (str/starts-with? t "-") [t/code-error-fg false t/code-err-bg]
        :else [t/dialog-hint false]))

    :commit
    [t/dialog-hint false]

    [t/dialog-fg false]))

(defn- magit-diff-filename-split
  "Char index at which the FILE PATH begins on a rendered diff HEADER line
   (`diff --git …`, `--- …`, `+++ …`, `rename`/`copy from|to …`) — so the caller
   paints the `diff --git`/`a/`/`b/` scaffolding dim and the filename that follows
   in the path colour magit gives it. nil for lines that name no file (context,
   hunk `@@`, +/- body), which stay flat."
  [text]
  (let [full
        (str text)

        t
        (str/triml full)

        lead
        (- (count full) (count t))

        after
        (fn [marker]
          (when (str/starts-with? t marker) (+ lead (count marker))))]

    (or (after "diff --git ")
        (after "--- ")
        (after "+++ ")
        (after "rename from ")
        (after "rename to ")
        (after "copy from ")
        (after "copy to "))))

(defn- run-async-with-ticker!
  "Run blocking `thunk` on a BACKGROUND thread so the caller's UI thread stays
   live. Between polls it calls `(tick!)` (paint a spinner frame, drain input)
   every `poll-ms` until the work settles, then returns `thunk`'s value. A thrown
   `thunk` becomes `{:ok? false :msg <message>}` — a network verb never escapes
   as an exception onto the modal loop. Screen-free control flow, so it is
   unit-testable with a plain counting `tick!`."
  [thunk tick! poll-ms]
  (let [fut (future (try (thunk)
                         (catch Throwable t
                           {:ok? false :msg (or (not-empty (ex-message t)) (str t))})))]
    (loop []

      (if (realized? fut) @fut (do (tick!) (Thread/sleep (long poll-ms)) (recur))))))

(defn- run-network!
  "Run a blocking git network `thunk` (push/pull/fetch) on a background thread
   while `busy!` repaints the magit buffer's OWN footer with `label` progress
   every tick, returning the thunk's `{:ok? :msg}`. There is NO modal overlay —
   the spinner lives in the status buffer's footer (the hint-bar row), so the
   buffer stays fully visible while the network round-trip runs off the
   render/input loop."
  [busy! label thunk]
  (run-async-with-ticker! thunk #(busy! label) 80))


(defn- magit-section-action!
  "Apply `f` (a `path → result` action) to every file row under the `:section`
   header at `idx`. Returns the first failure, or a rolled-up success."
  [rows idx f]
  (let [files (magit/section-of rows idx)]
    (if (empty? files)
      {:ok? false :msg "Nothing here"}
      (let [results (mapv #(f (:path %)) files)]
        (or (first (remove :ok? results))
            {:ok? true :msg (str "Done — " (count files) " file(s)")})))))

(defn- magit-stage-action!
  [root rows idx {:keys [kind area path]}]
  (cond (and (= :file kind) (= :staged area)) {:ok? false :msg "Already staged"}
        (= :file kind) (magit/stage-file! root path)
        (and (= :section kind) (contains? #{:untracked :unstaged :unmerged} area))
        (magit-section-action! rows idx #(magit/stage-file! root %))
        :else nil))

(defn- magit-unstage-action!
  [root rows idx {:keys [kind area path]}]
  (cond (and (= :file kind) (= :staged area)) (magit/unstage-file! root path)
        (and (= :section kind) (= :staged area))
        (magit-section-action! rows idx #(magit/unstage-file! root %))
        (= :file kind) {:ok? false :msg "Not staged"}
        :else nil))

(defn- magit-discard-flow!
  [^TerminalScreen screen root {:keys [kind area path] :as row}]
  (when (= :file kind)
    (when (confirm-dialog! screen
                           "Discard"
                           (if (= :untracked area)
                             (str "Delete untracked file " path "?")
                             (str "Discard changes in " path "?")))
      (magit/discard-file! root row))))

(defn- magit-commit-flow!
  [^TerminalScreen screen root model]
  (when-let [choice (select-dialog! screen
                                    "Commit"
                                    [{:label "Commit staged changes" :id :commit}
                                     {:label "Amend last commit" :id :amend}])]
    (let [amend? (= :amend (:id choice))]
      (if (and (not amend?) (empty? (:staged model)))
        {:ok? false :msg "Nothing staged — stage with s/S first"}
        (when-let [msg (text-input-dialog! screen
                                           (if amend? "Amend commit" "Commit")
                                           "Message"
                                           :initial
                                           (if amend? (or (magit/last-commit-message root) "") ""))]
          (magit/commit! root msg {:amend? amend?}))))))

(defn transient-item-by-key
  "PURE: the transient spec item bound to single character `ch` (a Character or
   string), scanning every group in order. nil when nothing is bound."
  [spec ch]
  (let [k (str ch)]
    (some (fn [{:keys [items]}]
            (some #(when (= k (:key %)) %) items))
          (:groups spec))))

(defn transient-toggle
  "PURE reducer for ONE keystroke against a magit transient `state`
   (`{:switches #{ids} :options {id val}}`). Returns a map whose `:kind` tells
   the impure caller what to do next:
     {:kind :continue :state s'}  a SWITCH flipped (or an unbound key — no-op)
     {:kind :option   :item it}   an OPTION was hit; caller reads a value then
                                   re-enters with it stored under [:options id]
     {:kind :action   :item it}   an ACTION fires; caller runs it and closes.
   Switches are the only kind this fn mutates; options/actions leave `state`
   untouched (the impure loop finishes their job)."
  [spec state ch]
  (if-let [{:keys [type id] :as it} (transient-item-by-key spec ch)]
    (case type
      :switch
      {:kind :continue
       :state (update state :switches #(if (contains? % id) (disj % id) (conj (or % #{}) id)))}

      :option
      {:kind :option :item it}

      :action
      {:kind :action :item it}

      {:kind :continue :state state})
    {:kind :continue :state state}))

(defn- draw-transient-item!
  "One transient row: `key` glyph (accented, BOLD when active) + label. An active
   switch or a set option shows in full `dialog-fg`; inactive rows dim to
   `dialog-hint`, so live switches visibly pop. An option's value trails in parens."
  [g left row inner-w {:keys [key label]} active? value]
  (let [keytxt
        (str key)

        x
        (+ left 2)

        lx
        (+ x (p/display-width keytxt) 2)

        has-val?
        (and value (not (str/blank? (str value))))

        body
        (str label (when has-val? (str "  (" value ")")))

        shown
        (ellipsize body (max 0 (- (+ left inner-w) lx 1)))]

    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (if active? (p/styled g [p/BOLD] (p/put-str! g x row keytxt)) (p/put-str! g x row keytxt))
    (p/set-colors! g (if active? t/dialog-fg t/dialog-hint) t/dialog-bg)
    (if active? (p/styled g [p/BOLD] (p/put-str! g lx row shown)) (p/put-str! g lx row shown))
    (p/set-colors! g t/dialog-fg t/dialog-bg)))

(defn magit-transient!
  "Magit-style transient overlay: a keymap of toggleable SWITCHES / value OPTIONS
   and fire-once ACTIONS, grouped under headers. There is NO moving cursor — you
   press an item's `:key` directly (magit's transient model). Switches highlight
   while active; options display their value inline. `spec` is
   `{:groups [{:title str :items [{:key :type :id :label}]}]}` where `:type` is
   `:switch` | `:option` | `:action`. `read-option` (impure) fetches an option's
   value: `(read-option item current) -> str|nil` — nil (Esc) leaves it unchanged.
   Returns `{:action id :switches #{…} :options {id val}}` when an action fires,
   or nil on Esc."
  [^TerminalScreen screen title spec read-option]
  (let [display-rows
        (vec (butlast (into []
                            (mapcat (fn [{:keys [title items]}]
                                      (concat [{:kind :header :text title}]
                                              (map (fn [it]
                                                     {:kind :item :item it})
                                                   items)
                                              [{:kind :blank}])))
                            (:groups spec))))

        total
        (count display-rows)

        item-w
        (+ 8
           (reduce max
                   16
                   (map (fn [r]
                          (case (:kind r)
                            :item
                            (+ 4 (p/display-width (str (:label (:item r)))))

                            :header
                            (p/display-width (str (:text r)))

                            0))
                        display-rows)))

        footer
        [["key" "toggle/run"] ["Esc" "cancel"]]]

    (loop [state {:switches #{} :options {}}]
      (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols (.getColumns size)
            rows (.getRows size)
            g (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g
                                        cols
                                        rows
                                        title
                                        (footer-content-width cols footer item-w)
                                        (adaptive-content-height rows (max 1 total)))
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds (max 1 total))]

        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc left) content-top inner-w content-h)
        (dotimes [i (min total content-h)]
          (let [r (nth display-rows i)
                row (+ content-top i)]

            (case (:kind r)
              :header
              (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                  (p/styled g [p/BOLD] (p/put-str! g (+ left 1) row (str (:text r)))))

              :blank
              nil

              :item
              (let [{:keys [type id] :as it} (:item r)
                    active? (case type
                              :switch
                              (contains? (:switches state) id)

                              :option
                              (contains? (:options state) id)

                              false)
                    value (when (= type :option) (get (:options state) id))]

                (draw-transient-item! g left row inner-w it active? value)))))
        (draw-hint-bar! g left hint-row inner-w footer)
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (if (nil? key)
            (recur state)
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/Character
              (let [ch (lower-key-character key)
                    r (transient-toggle spec state ch)]

                (case (:kind r)
                  :continue
                  (recur (:state r))

                  :option
                  (let [{:keys [id] :as it} (:item r)
                        v (read-option it (get (:options state) id))]

                    (recur (if (nil? v) state (assoc-in state [:options id] v))))

                  :action
                  {:action (:id (:item r)) :switches (:switches state) :options (:options state)}))
              (recur state))))))))


(defn- magit-push-flow!
  "Magit-style push transient (`magit-transient!`). SWITCHES (all optional):
   force-with-lease, dry-run, disable hooks (--no-verify), set-upstream. When the
   repo targets Gerrit a `t Topic` OPTION appears and the primary `p` push lands
   on `refs/for/<branch>` carrying that topic. Every OTHER configured remote is
   listed INLINE as its own `Push to <remote>` action in the SAME overlay (magit
   lists push targets in the transient — no second dialog)."
  [^TerminalScreen screen busy! root]
  (let [upstream
        (magit/upstream-name root)

        remotes
        (magit/remotes root)

        g-remote
        (magit/gerrit-remote root)

        g-branch
        (magit/gerrit-target-branch root)

        branch
        (magit/current-branch root)

        gerrit?
        (some? g-remote)

        other-remotes
        (->> remotes
             (map :name)
             (remove #(or (= "origin" %) (= g-remote %)))
             (take 9)
             vec)

        arg-items
        (cond-> [{:key "f" :type :switch :id :force :label "Force with lease (--force-with-lease)"}
                 {:key "n" :type :switch :id :dry-run :label "Dry run (--dry-run)"}
                 {:key "h" :type :switch :id :no-verify :label "Disable hooks (--no-verify)"}
                 {:key "u" :type :switch :id :set-upstream :label "Set upstream (-u)"}]
          gerrit?
          (conj {:key "t" :type :option :id :topic :label "Topic"}))

        primary
        (if gerrit?
          {:key "p" :type :action :id :review :label (str "Push for review → refs/for/" g-branch)}
          {:key "p"
           :type :action
           :id :push
           :label (str "Push" (when upstream (str " to " upstream)))})

        ;; Each remaining remote becomes its own inline action row keyed by a
        ;; digit — magit lists push targets in the SAME transient, never a
        ;; second dialog.
        remote-rows
        (map-indexed (fn [i name]
                       {:key (str (inc i))
                        :type :action
                        :id (keyword "remote" name)
                        :label (str "Push to " name)})
                     other-remotes)

        remote-action->name
        (into {}
              (map (fn [name]
                     [(keyword "remote" name) name]))
              other-remotes)

        push-items
        (vec (cons primary remote-rows))

        spec
        {:groups [{:title "Arguments" :items arg-items} {:title "Push" :items push-items}]}

        read-option
        (fn [{:keys [id]} current]
          (when (= id :topic)
            (text-input-dialog! screen
                                "Topic" "Topic (optional)"
                                :initial (or current
                                             (when (and branch (not= branch g-branch)) branch)
                                             ""))))]

    (when-let [{:keys [action switches options]} (magit-transient! screen "Push" spec read-option)]
      (let [base {:set-upstream? (contains? switches :set-upstream)
                  :force? (contains? switches :force)
                  :dry-run? (contains? switches :dry-run)
                  :no-verify? (contains? switches :no-verify)}
            topic (:topic options)]

        (cond (= action :review) (run-network! busy!
                                               "Pushing for review"
                                               #(magit/gerrit-push!
                                                  root
                                                  (merge
                                                    {:remote g-remote :branch g-branch :topic topic}
                                                    (select-keys base [:dry-run? :no-verify?]))))
              (= action :push) (run-network! busy! "Pushing" #(magit/push! root base))
              (contains? remote-action->name action)
              (let [rname (get remote-action->name action)]
                (run-network! busy!
                              (str "Pushing to " rname)
                              #(magit/push! root (assoc base :remote rname))))
              :else nil)))))

(defn- magit-branch-flow!
  [^TerminalScreen screen root]
  (when-let [c (select-dialog! screen
                               "Branch"
                               [{:label "Checkout…" :id :checkout}
                                {:label "Create & checkout…" :id :create}
                                {:label "Delete…" :id :delete}])]
    (case (:id c)
      :checkout
      (when-let [b (select-dialog! screen
                                   "Checkout branch"
                                   (mapv (fn [{:keys [name current?]}]
                                           {:label (str name (when current? "  (current)"))
                                            :name name})
                                         (magit/local-branches root)))]
        (magit/checkout-branch! root (:name b)))

      :create
      (when-let [nm (text-input-dialog! screen "Create branch" "Name")]
        (when-not (str/blank? nm) (magit/create-branch! root (str/trim nm))))

      :delete
      (when-let [b (select-dialog! screen
                                   "Delete branch"
                                   (into []
                                         (keep (fn [{:keys [name current?]}]
                                                 (when-not current? {:label name :name name})))
                                         (magit/local-branches root)))]
        (when (confirm-dialog! screen "Delete branch" (str "Delete " (:name b) "?"))
          (let [r (magit/delete-branch! root (:name b) {})]
            (if (:ok? r)
              r
              (if (confirm-dialog! screen
                                   "Force delete?"
                                   [(str (:msg r)) (str "Force delete " (:name b) "?")])
                (magit/delete-branch! root (:name b) {:force? true})
                r))))))))

(defn- magit-stash-flow!
  "`selected-ref` is the stash under the cursor (when the cursor sits on a
   stash row), else the newest stash is the target of pop/apply/drop."
  [^TerminalScreen screen root selected-ref model]
  (let [ref (or selected-ref (:ref (first (:stashes model))))]
    (when-let [c (select-dialog! screen
                                 "Stash"
                                 (cond-> [{:label "Stash working tree…" :id :push}]
                                   ref
                                   (into [{:label (str "Pop " ref) :id :pop}
                                          {:label (str "Apply " ref) :id :apply}
                                          {:label (str "Drop " ref) :id :drop}])))]
      (case (:id c)
        :push
        (when-some [m (text-input-dialog! screen "Stash" "Message (optional)")]
          (magit/stash-push! root m))

        :pop
        (magit/stash-pop! root ref)

        :apply
        (magit/stash-apply! root ref)

        :drop
        (when (confirm-dialog! screen "Drop stash" (str "Drop " ref "?"))
          (magit/stash-drop! root ref))))))

(defn- magit-char-action!
  "Run the magit verb for character `c` against the row under the cursor.
   Returns an action result `{:ok? :msg}`, or nil when the key did nothing.
   Case-SENSITIVE — `s`≠`S`, `u`≠`U`, `f`≠`F`, exactly like magit."
  [^TerminalScreen screen busy! root model rows idx row c]
  (case c
    \s
    (magit-stage-action! root rows idx row)

    \S
    (magit/stage-all! root)

    \u
    (magit-unstage-action! root rows idx row)

    \U
    (magit/unstage-all! root)

    (\x \k)
    (magit-discard-flow! screen root row)

    \c
    (magit-commit-flow! screen root model)

    \P
    (magit-push-flow! screen busy! root)

    \F
    (run-network! busy! "Pulling" #(magit/pull! root))

    \f
    (run-network! busy! "Fetching" #(magit/fetch! root))

    \b
    (magit-branch-flow! screen root)

    \z
    (magit-stash-flow! screen root (when (= :stash (:kind row)) (:ref row)) model)

    (\g \r)
    {:ok? true :msg "Refreshed"}

    nil))

(defn magit-dialog!
  "Magit-style status buffer over the git CLI — the C-x g / footer-git modal.

   `root-or-repos` is either ONE root (string/File — the classic single-repo
   buffer) or a vector of repo entries `{:root :label :draft?}` from
   `magit/workspace-roots` — the session's primary workspace root plus every
   extra filesystem root (`/fs`, the dir picker). With several roots each repo
   gets a `Repository <label> — <path>` header and its own full section stack;
   for a DRAFT session the entries already point at the CLONES the session
   edits, so the buffer shows the draft's git state, not the trunk's.

   Sections: head/upstream facts, untracked, unmerged, unstaged, staged,
   stashes and recent commits. TAB (or Enter) folds a file's diff open under
   its row. Verbs mirror magit and route to the repo UNDER THE CURSOR:
   `s`/`u` stage/unstage the file or the whole section, `S`/`U` all, `x` (or
   `k`) discard with a confirm, `c` commit/amend (message prompt), `P` push
   (plain / -u / --force-with-lease), `F` pull, `f` fetch, `b` branch
   (checkout / create / delete), `z` stash (push / pop / apply / drop), `g`
   refresh, `q`/Esc close. Every verb shells to the real `git` binary via
   `internal.git`, and the buffer re-reads every repo after each action, so
   what you see is always `git status` truth. Returns nil."
  [^TerminalScreen screen root-or-repos]
  (let [repo-entries
        (if (sequential? root-or-repos)
          (vec root-or-repos)
          (magit/workspace-roots nil root-or-repos))

        primary-root
        (:root (first repo-entries))

        multi?
        (> (count repo-entries) 1)

        repos*
        (atom (magit/load-repos repo-entries))

        expanded
        (atom #{})

        diff-cache
        (atom {})

        sel
        (atom 0)

        scroll
        (atom 0)

        echo
        (atom nil)

        refresh!
        (fn []
          (reset! diff-cache {})
          (reset! repos* (magit/load-repos repo-entries)))

        run-action!
        (fn [result]
          (when result (reset! echo result) (refresh!))
          nil)

        model-for
        (fn [root]
          (or (some #(when (= (:root %) root) (:model %)) @repos*) (:model (first @repos*))))

        diff-fn
        (fn [row]
          (let [root
                (or (:root row) primary-root)

                commit?
                (= :commit (:kind row))

                k
                [root (:area row) (if commit? (:sha row) (:path row))]]

            (or (get @diff-cache k)
                (let [lines (or (not-empty (if commit?
                                             (magit/commit-diff-lines root row)
                                             (magit/file-diff-lines root row)))
                                ["(no diff)"])]
                  (swap! diff-cache assoc k lines)
                  lines))))]

    (loop []

      (let [repos
            @repos*

            size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            term-rows
            (.getRows size)

            g
            (.newTextGraphics screen)

            buf-rows
            (magit/multi-status-rows repos @expanded diff-fn)

            total
            (count buf-rows)

            title
            (if multi?
              (str "Git — " (count repos) " roots")
              (let [model (:model (first repos))]
                (str "Git — "
                     (cond (nil? model) "?"
                           (:detached? model) "detached HEAD"
                           :else (:branch model)))))

            content-w
            (max (footer-content-width cols magit-hints)
                 (default-content-width cols)
                 (min 110 (max 40 (- cols 10))))

            bounds
            (draw-dialog-chrome! g
                                 cols
                                 term-rows
                                 title
                                 content-w
                                 (default-content-height term-rows))

            {:keys [left inner-w]}
            bounds

            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds)

            echo-h
            (if @echo 1 0)

            list-h
            (max 1 (- content-h echo-h))

            _
            (reset! sel (or (magit/first-selectable buf-rows (min @sel (max 0 (dec total)))) 0))

            visible
            (min total list-h)

            max-start
            (max 0 (- total visible))

            start
            (clamp @scroll 0 max-start)

            _
            (reset! scroll start)

            text-w
            (max 1 (- inner-w 3))

            busy!
            (fn [label]
              ;; Progress lives in the buffer's OWN footer, not a modal overlay:
              ;; drain keystrokes typed mid-op (so they can't misfire once the
              ;; verb returns), then paint the spinner + `label…` over the
              ;; hint-bar row. The rest of the status buffer stays untouched.
              (try (loop []

                     (when (.pollInput screen) (recur)))
                   (p/set-colors! g t/dialog-fg t/dialog-bg)
                   (p/fill-rect! g (inc left) hint-row inner-w 1)
                   (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                   (p/put-str!
                     g
                     (+ left 2)
                     hint-row
                     (ellipsize
                       (str (render/spinner-frame (System/currentTimeMillis)) "  " label "…")
                       text-w))
                   (.setCursorPosition screen nil)
                   (.refresh screen Screen$RefreshType/DELTA)
                   (catch Throwable _ nil)))]

        (dotimes [i visible]
          (let [idx (+ start i)
                row-y (+ content-top i)]

            (when (< idx total)
              (let [row (nth buf-rows idx)
                    [fg bold? row-bg] (magit-row-style row)
                    bg (or row-bg t/dialog-bg)
                    selected? (and (= idx @sel) (magit/selectable? row))]

                (p/set-colors! g t/dialog-fg bg)
                (p/fill-rect! g (inc left) row-y inner-w 1)
                (p/set-colors! g t/dialog-hint-key bg)
                (p/draw-selection-marker! g (inc left) row-y selected?)
                (p/set-colors! g fg bg)
                (let [txt (ellipsize (:text row) text-w)
                      ;; On a diff HEADER row, split off the filename so it pops
                      ;; in the path colour while the `diff --git`/a-/b- scaffolding
                      ;; stays dim — magit's file-heading look.
                      split (when (= :diff (:kind row)) (magit-diff-filename-split (:text row)))]

                  (cond (and split (< (long split) (count txt)))
                        (let [pre (subs txt 0 split)]
                          (p/put-str! g (+ left 3) row-y pre)
                          (p/set-colors! g t/result-path-fg bg)
                          (p/styled g
                                    [p/BOLD]
                                    (p/put-str! g (+ left 3 (long split)) row-y (subs txt split))))
                        (or bold? selected?)
                        (p/styled g [p/BOLD] (p/put-str! g (+ left 3) row-y txt))
                        :else (p/put-str! g (+ left 3) row-y txt)))))))
        (scrollbar/draw! g
                         {:col (+ left inner-w)
                          :top content-top
                          :track-h list-h
                          :total-h total
                          :inner-h list-h
                          :scroll start})
        (when-let [{:keys [ok? msg]} @echo]
          (let [row-y (+ content-top list-h)]
            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/fill-rect! g (inc left) row-y inner-w 1)
            (p/set-colors! g (if ok? t/code-success-fg t/code-error-fg) t/dialog-bg)
            (p/put-str! g (+ left 2) row-y (ellipsize (str msg) text-w))))
        (draw-hint-bar! g left hint-row inner-w magit-hints)
        ;; No text-input field in the magit buffer — hide the terminal cursor
        ;; entirely (nil) instead of parking it at 0,0, where it blinks in the
        ;; top-left corner.
        (.setCursorPosition screen nil)
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key
              (read-modal-key! screen)

              wheel
              (modal-wheel-step key)

              row
              (when (and (pos? total) (< (long @sel) total)) (nth buf-rows @sel))

              row-root
              (or (:root row) primary-root)

              move!
              (fn [dir]
                ;; Move the selection AND keep it in view (arrows follow point).
                (let [n (magit/next-selectable buf-rows @sel dir)]
                  (reset! sel n)
                  (reset! scroll (visible-window-start n @scroll visible total))))

              scroll-view!
              (fn [delta]
                ;; Scroll the VIEWPORT independently of the selection, so a long
                ;; expanded diff (whose lines aren't selectable) can be read.
                (swap! scroll #(clamp (+ (long %) (long delta)) 0 max-start)))

              reconcile-sel!
              (fn []
                ;; After a pure viewport scroll, pull the point onto the nearest
                ;; selectable row still on screen so verbs act on something visible.
                (let [s
                      @scroll

                      hi
                      (min total (+ s visible))]

                  (when (or (< (long @sel) s) (>= (long @sel) hi))
                    (when-let [n (some #(when (magit/selectable? (nth buf-rows %)) %) (range s hi))]
                      (reset! sel n)))))

              toggle-diff!
              (fn []
                (when (contains? #{:file :commit} (:kind row))
                  (let [id
                        (if (= :commit (:kind row)) (:sha row) (:path row))

                        k
                        [(or (:root row) primary-root) (:area row) id]]

                    (swap! expanded #(if (contains? % k) (disj % k) (conj % k))))))]

          (cond (nil? key) (recur)
                wheel (do (scroll-view! wheel) (reconcile-sel!) (recur))
                :else
                (condp = (key-type key)
                  KeyType/Escape nil
                  KeyType/ArrowUp (do (move! -1) (recur))
                  KeyType/ArrowDown (do (move! 1) (recur))
                  KeyType/PageUp
                  (do (scroll-view! (- (max 1 (dec list-h)))) (reconcile-sel!) (recur))
                  KeyType/PageDown (do (scroll-view! (max 1 (dec list-h))) (reconcile-sel!) (recur))
                  KeyType/Home (do (reset! sel (or (magit/first-selectable buf-rows 0) 0))
                                   (reset! scroll 0)
                                   (recur))
                  KeyType/End (do (reset! scroll max-start) (reconcile-sel!) (recur))
                  KeyType/Tab (do (toggle-diff!) (recur))
                  KeyType/Enter (do (toggle-diff!) (recur))
                  KeyType/Character (let [c (key-character key)]
                                      (if (= c \q)
                                        nil
                                        (do (reset! echo nil)
                                            (run-action! (magit-char-action! screen
                                                                             busy!
                                                                             row-root
                                                                             (model-for row-root)
                                                                             buf-rows
                                                                             @sel
                                                                             row
                                                                             c))
                                            (recur))))
                  (recur))))))))

(defn- current-model-info
  []
  (when-let [router (try (vis/get-router)
                         (catch Throwable t
                           (tel/log! :warn ["dialogs: get-router failed" (ex-message t)])
                           nil))]
    (try (vis/resolve-effective-model router)
         (catch Throwable t
           (tel/log! :warn ["dialogs: resolve-effective-model failed" (ex-message t)])
           nil))))
(defn- current-provider-id [] (:provider (current-model-info)))

(defn- theme-choice-order
  []
  (try (mapv keyword (shared-theme/available-theme-ids))
       (catch Throwable t
         (tel/log! :warn ["dialogs: available-theme-ids failed" (ex-message t)])
         [(keyword shared-theme/default-theme-id)])))
(defn- settings-ui-options
  "Terminal-UI-owned settings in the Terminal UI section: currently just the
   theme picker. Feature toggles (mouse-selection auto-copy, etc.) live in the
   toggles registry, not here."
  []
  [{:key :theme-name
    :type :choice
    :choices (theme-choice-order)
    :label "Theme"
    :description
    "Reusable channel theme from com.blockether.vis.internal.theme and extension :ext/theme maps"}])
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
   ;; `toggles-for-channel` drops provider-specific knobs whose provider
   ;; isn't configured (`:visible-fn`) AND toggles scoped to OTHER channels
   ;; (`:channels`) — e.g. the web theme never shows in the TUI dialog.
   ;; Grouped by `:group` into one section per group — the SAME flat, grouped
   ;; shape the web settings modal uses (no tabs, no single "Feature Toggles"
   ;; bucket). Returns nil when nothing matches so no empty header shows.
   (let [specs (->> (vis/toggles-for-channel :tui)
                    (filter include?))]
     (when (seq specs)
       (vec (mapcat (fn [[group group-specs]]
                      (cons
                        {:type :section :label (titleize-label (name (or group :other)))}
                        (for [{:keys [id label description owner]} (sort-by :id group-specs)]
                          {:key (keyword (str "toggle::" id))
                           :type :registry-toggle
                           :toggle-id id
                           :label (or label
                                      (titleize-label (str (or (namespace id) "") " " (name id))))
                           :description (str (or description "")
                                             (when (and owner (not= owner :vis))
                                               (str "  [" (titleize-label (name owner)) "]")))})))
                    (sort-by (comp str key) (group-by #(or (:group %) :other) specs))))))))
(def ^:private tui-contributor-slots #{:tui.slot/header-row :tui.slot/footer-segment})
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
      (vec (cons {:type :section :label "Header / Footer Contributors"}
                 (for [{:keys [id slot]} contributions]
                   {:key (keyword (str "contrib::" id))
                    :type :set-toggle
                    :set-key :contributors-disabled
                    :item-id id
                    :label (str id)
                    :description (str "Toggle this extension's contribution to the TUI "
                                      (case slot
                                        :tui.slot/header-row
                                        "header subtitle row"

                                        :tui.slot/footer-segment
                                        "footer"

                                        "chrome"))}))))))
(defn- settings-content-width [cols] (default-content-width cols))
(defn- settings-content-height [rows] (default-content-height rows))
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
  (let [raw
        (str sym-or-str)

        segments
        (->> (str/split raw #"\.")
             (remove str/blank?)
             vec)

        cleaned
        (vec (remove #(contains? namespace-noise-segments %) segments))

        leaf
        (or (last cleaned) (last (remove #{"com" "blockether"} segments)) (last segments))]

    (or leaf raw)))
(defn- extension-kind
  [ext]
  (cond (seq (:ext/providers ext)) :provider
        (seq (:ext/channels ext)) :channel
        :else :extension))
(defn- extension-display-label
  [ext]
  (let [provider-label
        (some-> (first (:ext/providers ext))
                :provider/label
                (str/replace #"\s+\(.*\)$" ""))

        channel-label
        (or (some-> (first (:ext/channels ext))
                    :channel/cmd
                    titleize-label)
            (some-> (first (:ext/channels ext))
                    :channel/id
                    name
                    titleize-label))

        alias-label
        (some-> (get-in ext [:ext/engine :ext.engine/alias])
                name
                titleize-label)

        ;; Take the meaningful tail segment of the namespace (drop
        ;; `com.blockether.vis.ext` vendor prefix and the trailing
        ;; `core` / `bot` / `main` registrar entry-point convention)
        ;; and titleize THAT, so `voice` -> `Voice`, `goal` ->
        ;; `Goal`, `channel-tui` -> `Channel Tui` instead of the
        ;; previous `Com.blockether.vis.ext.voice.core`.
        ns-label
        (some-> (:ext/name ext)
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
        (string? v) (let [s (str/trim v)]
                      (when-not (str/blank? s) (keyword s)))
        :else nil))

(def ^{:private true} retired-extension-setting-keys
  "Old :ext/settings rows now owned by registry toggles. Drop them rather than
   aliasing or rendering duplicates."
  #{:voice/respond? :reasoning-level :vis/reasoning-level :openai-codex-verbosity
    :openai-codex/verbosity})

(defn- extension-setting-declarations
  []
  (->> (vis/registered-extensions)
       (mapcat
         (fn [ext]
           (let [ext-id
                 (:ext/name ext)

                 ext-kind
                 (extension-kind ext)

                 ext-label
                 (extension-display-label ext)

                 provider-ids
                 (set (keep :provider/id (:ext/providers ext)))]

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
          {:type (or type :toggle)
           :id [:extension-setting extension-id key]
           :key key
           :choices choices
           :label (or label (name key))
           :extension-id extension-id
           :extension-kind extension-kind
           :extension-label extension-label
           :provider-ids provider-ids
           :description (or description "Extension setting")})
        (extension-setting-declarations)))
(defn- extension-env-declarations
  []
  (->> (vis/registered-extensions)
       (mapcat
         (fn [ext]
           (let [ext-id
                 (:ext/name ext)

                 ext-kind
                 (extension-kind ext)

                 ext-label
                 (extension-display-label ext)]

             (for [decl
                   (:ext/env ext)

                   :let [name
                         (some-> (:name decl)
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
          {:type :env-var
           :id [:environment name]
           :name name
           :label (or label name)
           :extension-id extension-id
           :extension-kind extension-kind
           :extension-label extension-label
           :description (or description "Extension environment override")
           :secret? (boolean secret?)
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
              (into [{:type :subsection :label label}]
                    (sort-by (juxt :type :label :name) group-rows)))
            (sort-by first (group-by extension-group-key extension-rows)))))
(defn- settings-rows
  "Every settings row in ONE flat, grouped list — no tabs (mirrors the web
   settings modal): Terminal-UI chrome, then all feature toggles grouped by
   `:group`, then Models, then channel / provider / extension knobs. Sections
   with nothing to show drop out."
  ([] (settings-rows (extension-option-rows)))
  ([extension-rows]
   (let [active-provider
         (current-provider-id)

         all-extension-rows
         (filterv #(provider-row-active? active-provider %) extension-rows)

         provider-rows
         (extension-rows-of-kind all-extension-rows :provider)

         channel-rows
         (extension-rows-of-kind all-extension-rows :channel)

         generic-rows
         (extension-rows-of-kind all-extension-rows :extension)]

     (vec (concat [{:type :section :label "Terminal UI"}]
                  (settings-ui-options)
                  ;; ALL feature toggles, grouped by :group like the web.
                  (or (registry-toggle-rows) [])
                  (or (contributor-rows) [])
                  ;; Reasoning-effort moved OUT of Settings (own control: Ctrl+R); the
                  ;; Models section only ever carried it, so it's gone too.
                  (when (seq channel-rows)
                    (concat [{:type :section :label "Channel Settings"}]
                            (settings-extension-groups channel-rows)))
                  (when (seq provider-rows)
                    (concat [{:type :section :label "Provider Settings"}]
                            (settings-extension-groups provider-rows)))
                  (when (seq generic-rows)
                    (concat [{:type :section :label "Extension Settings"}]
                            (settings-extension-groups generic-rows))))))))
(defn- extension-env-status-label
  [source]
  (case source
    :config
    "set in Vis config"

    :env
    "set in environment"

    :unset
    "unset"

    "unset"))
(defn- settings-option-label
  [{:keys [key label type choices toggle-id] env-name :name} values]
  (case type
    :choice
    (str label ": " (clojure.core/name (or (get values key) (first choices))))

    :env-var
    (str label ": " (extension-env-status-label (:source (vis/extension-env-status env-name))))

    ;; Boolean state is carried by the leading ●/○ glyph (see settings-row-mark),
    ;; so the label stays clean — no redundant "(on)/(off)/(shown/hidden)" text.
    :set-toggle
    label

    :registry-toggle
    (let [spec
          (vis/toggle-spec toggle-id)

          toggle-val
          (vis/toggle-value toggle-id)]

      (if (= :enum (:type spec)) (str label ": " (clojure.core/name toggle-val)) label))

    label))

(defn- settings-row-mark
  "Leading status glyph + its color for a settings row — the visual that
   replaces the old `[ON]/[off]` text. Consistent with the footer's ● active
   glyph and the resource status dots: ● (status-ok) = on, ○ (dim) = off,
   ◆ (accent) = a value/enum to cycle, ▸ (accent) = an action. Returns
   `[glyph fg-color]`."
  [{:keys [key type set-key item-id toggle-id]} values]
  (let [on
        [p/STATUS_ON t/status-ok]

        ;; enabled
        off
        [p/STATUS_OFF t/dialog-hint]

        ;; disabled
        val
        [p/MARK_VALUE t/header-active-tab-accent]

        ;; cycles a value
        act
        [p/MARK_ACTION t/header-active-tab-accent]]

    ;; runs an action
    (case type
      :action
      act

      :env-var
      act

      :choice
      val

      :set-toggle
      (if (some-> (get values set-key)
                  (contains? item-id))
        off
        on)

      ;; in disabled-set → off
      :registry-toggle
      (let [spec
            (vis/toggle-spec toggle-id)

            tv
            (vis/toggle-value toggle-id)]

        (cond (= :enum (:type spec)) val
              (boolean tv) on
              :else off))

      :toggle
      (if (get values key false) on off)

      [" " t/dialog-fg])))
(defn- cycle-choice
  [choices current]
  (let [choices
        (vec choices)

        idx
        (.indexOf ^java.util.List choices current)]

    (nth choices (mod (inc (if (neg? idx) 0 idx)) (count choices)))))
(defn- apply-settings-option
  [values {:keys [key type choices set-key item-id toggle-id]}]
  (case type
    :choice
    (update values key #(cycle-choice choices %))

    :toggle
    (update values key not)

    :set-toggle
    (update values
            set-key
            (fn [s]
              (let [s (or s #{})]
                (if (contains? s item-id) (disj s item-id) (conj s item-id)))))

    :registry-toggle
    (do (if (= :enum (:type (vis/toggle-spec toggle-id)))
          (vis/toggle-cycle-value! toggle-id)
          (vis/toggle-set-enabled! toggle-id (not (vis/toggle-enabled? toggle-id))))
        values)

    values))
(defn- notify-settings-change!
  [callbacks values]
  (when-let [f (:on-change callbacks)]
    (f values))
  values)
(defn- settings-selectable?
  [{:keys [type]}]
  (contains? #{:toggle :choice :action :env-var :set-toggle :registry-toggle} type))
(defn- first-selectable-index
  [rows]
  (or (first (keep-indexed (fn [i row]
                             (when (settings-selectable? row) i))
                           rows))
      0))
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
  (let [{:keys [source value]}
        (vis/extension-env-status name)

        raw
        (text-input-dialog! screen
                            "Extension Environment" (str name ":")
                            :mask (when secret? \*)
                            :initial (if secret? "" (or value ""))
                            :body
                            [(str label " - " (extension-env-status-label source))
                             (or description "")
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
  (mapv (fn [theme-id]
          {:theme-id theme-id :label (theme-display-label theme-id)})
        choices))
(defn- theme-picker-content-width [cols] (settings-content-width cols))
(defn- theme-picker-content-height [rows] (settings-content-height rows))
(defn- theme-picker-dialog!
  "Small theme chooser. Moving selection previews the theme immediately;
   Enter commits the preview, Esc restores the original theme."
  [^TerminalScreen screen choices current preview!]
  (let [items
        (theme-picker-items choices)

        total
        (count items)

        original
        (or current (:theme-id (first items)))

        selected
        (atom (max 0 (.indexOf ^java.util.List (vec choices) original)))

        scroll
        (atom 0)

        last-preview
        (atom ::none)

        preview-selected!
        (fn []
          (when-let [theme-id (:theme-id (nth items @selected nil))]
            (when-not (= theme-id @last-preview)
              (reset! last-preview theme-id)
              (preview! theme-id))))]

    (when (pos? total)
      (loop []

        (preview-selected!)
        (let [size
              (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

              cols
              (.getColumns size)

              rows
              (.getRows size)

              g
              (.newTextGraphics screen)

              content-w
              (theme-picker-content-width cols)

              content-h
              (theme-picker-content-height rows)

              bounds
              (draw-dialog-chrome! g cols rows "Theme" content-w content-h)

              {:keys [left inner-w]}
              bounds

              {:keys [content-top content-h hint-row]}
              (dialog-layout bounds total)

              visible
              (min total content-h)

              _
              (swap! selected #(clamp % 0 (max 0 (dec total))))

              _
              (swap! scroll #(visible-window-start @selected % content-h total))]

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
                           {:col (+ left inner-w)
                            :top content-top
                            :track-h content-h
                            :total-h total
                            :inner-h content-h
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
              (condp = (key-type key)
                KeyType/Escape (do (preview! original) nil)
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                      (recur))
                KeyType/Enter (:theme-id (nth items @selected))
                (recur)))))))))
(defn- activate-theme-row!
  [screen values callbacks {:keys [choices key]}]
  (let [original
        (get @values key)

        preview!
        (fn [theme-id]
          (let [next-values (assoc @values key theme-id)]
            (reset! values next-values)
            (notify-settings-change! callbacks next-values)))]

    (if-let [selected (theme-picker-dialog! screen choices original preview!)]
      (preview! selected)
      (preview! original))))
(defn- activate-settings-row!
  [screen values callbacks row]
  (case (:type row)
    :action
    (when-let [f (get callbacks (:id row))]
      (f @values))

    :env-var
    (edit-extension-env-var! screen row)

    (if (= :theme-name (:key row))
      (activate-theme-row! screen values callbacks row)
      (->> (swap! values apply-settings-option row)
           (notify-settings-change! callbacks)))))
(defn- settings-section-text
  [label inner-w]
  (let [prefix
        (str "── " label " ")

        available
        (max 0 (- inner-w 2))

        filler
        (apply str (repeat (max 0 (- available (count prefix))) \─))]

    (ellipsize (str prefix filler) available)))
(defn- settings-option-indent [] t/settings-option-indent)
(defn- settings-subsection-text [label inner-w] (ellipsize (str "◆ " label) (max 0 (- inner-w 2))))
(defn- settings-wrap-lines
  [s w]
  (let [w
        (max 1 (long w))

        s
        (str/trim (str (or s "")))]

    (if (str/blank? s) [] (vec (remove str/blank? (render/wrap-text s w))))))
(defn- settings-render-entries
  "Flatten logical settings rows into paint rows. Descriptions wrap under
   their owning option instead of stealing a fixed inline column and
   collapsing to `...` on narrow dialogs / long extension labels."
  [rows option-w desc-w]
  (let [option-w
        (max 1 (long option-w))

        desc-w
        (max 1 (long desc-w))]

    (vec
      (mapcat (fn [idx {:keys [type label description]}]
                (case type
                  :section
                  [{:row-idx idx :part :section}]

                  :subsection
                  [{:row-idx idx :part :subsection}]

                  :info
                  (let [text
                        (str label
                             (when-not (str/blank? (str (or description "")))
                               (str "  " description)))

                        lines
                        (settings-wrap-lines text (max 1 (- option-w 2)))]

                    (mapv (fn [line]
                            {:row-idx idx :part :info-line :text line})
                          (or (seq lines) [""])))

                  (let [desc-lines (settings-wrap-lines description desc-w)]
                    (into [{:row-idx idx :part :option}]
                          (mapv (fn [line]
                                  {:row-idx idx :part :option-desc :text line})
                                desc-lines)))))
              (range)
              rows))))
(defn- settings-header-row? [{:keys [type]}] (contains? #{:section :subsection} type))
(defn- settings-row-search-text
  "Lowercased haystack for a row's search match: its label + description."
  [{:keys [label description]}]
  (str/lower-case (str label " " description)))
(defn- filter-settings-rows
  "Live-filter settings `rows` by `query` (case-insensitive substring over
   label + description). Section / subsection headers survive only when a
   matching option remains beneath them, so the grouped shape is preserved.
   A blank query returns `rows` unchanged."
  [rows query]
  (let [rows
        (vec rows)

        q
        (str/lower-case (str/trim (str query)))]

    (if (str/blank? q)
      rows
      (let [n
            (count rows)

            match?
            (fn [i]
              (let [row (nth rows i)]
                (and (settings-selectable? row) (str/includes? (settings-row-search-text row) q))))

            matched
            (into #{} (filter match? (range n)))

            next-idx
            (fn [i pred]
              (or (first (filter #(and (> % i) (pred (nth rows %))) (range n))) n))

            headers
            (for [i
                  (range n)

                  :let [row
                        (nth rows i)]
                  :when (case (:type row)
                          :section
                          (some matched (range (inc i) (next-idx i #(= :section (:type %)))))

                          :subsection
                          (some matched (range (inc i) (next-idx i settings-header-row?)))

                          false)]

              i)

            keep
            (into matched headers)]

        (vec (keep-indexed (fn [i row]
                             (when (contains? keep i) row))
                           rows))))))
(defn- settings-toc
  "Table-of-contents entries for the VS Code-style left sidebar: one per
   top-level `:section`, each with the count of selectable rows beneath it
   and whether it owns the currently-selected row. `rows` is the (already
   filtered) flat settings list; `selected` is the selected row index."
  [rows selected]
  (let [rows
        (vec rows)

        n
        (count rows)

        sec-idxs
        (filterv #(= :section (:type (nth rows %))) (range n))]

    (vec (map-indexed (fn [k start]
                        (let [end
                              (long (or (get sec-idxs (inc k)) n))

                              cnt
                              (count (filter settings-selectable? (subvec rows start end)))]

                          {:label (:label (nth rows start))
                           :count cnt
                           :start start
                           :active? (and (>= selected start) (< selected end))}))
                      sec-idxs))))

(defn settings-dialog!
  "Show the settings dialog.

   ONE flat, grouped, scrollable list (mirrors the web settings modal), laid
   out VS Code-style: a left Table-of-Contents sidebar rail lists the sections
   with per-section counts and highlights the one owning the selection, while
   the right pane shows the settings themselves. Toggle rows render a leading
   status glyph; choice rows cycle their value with Enter; action rows invoke
   a callback. The rail is a passive locator — arrow keys still move through
   the right pane and the rail tracks where you are.

   `settings` is the persisted TUI settings map (see
   `state/default-settings`). Esc clears an active search first, then closes
   and returns the current settings map."
  ([^TerminalScreen screen settings] (settings-dialog! screen settings nil))
  ([^TerminalScreen screen settings callbacks]
   (let [extension-rows
         (extension-option-rows)

         selected
         (atom (first-selectable-index (settings-rows extension-rows)))

         scroll
         (atom 0)

         values
         (atom (or settings {}))

         scrollbar-drag-offset
         (volatile! nil)

         query
         (atom "")

         ;; Mark gutter = a single status glyph (●/○/◆/▸) + 1-col gap; wrapped
         ;; option descriptions indent to this so they sit under the label.
         check-w
         2]

     (loop []

       (let [all-rows
             (settings-rows extension-rows)

             rows
             (filter-settings-rows all-rows @query)

             n
             (count rows)

             size
             (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

             cols
             (.getColumns size)

             screen-rows
             (.getRows size)

             g
             (.newTextGraphics screen)

             bounds
             (draw-dialog-chrome! g
                                  cols
                                  screen-rows
                                  "Settings"
                                  (settings-content-width cols)
                                  (settings-content-height screen-rows))

             {:keys [left inner-w]}
             bounds

             ;; VS Code split: a left sidebar rail (the section Table of
             ;; Contents) + a vertical divider + the right settings pane.
             ;; `lleft`/`linner` are the right pane's own left/inner-w, so the
             ;; whole list-painting block below reuses the single-pane math
             ;; unchanged — only the search bar and hint bar stay full width.
             rail-w
             (clamp (quot inner-w 4) 14 22)

             lleft
             (+ left rail-w 1)

             linner
             (max 1 (- inner-w rail-w 1))

             {:keys [content-top content-h hint-row]}
             (dialog-layout bounds)

             search-row
             content-top

             list-top
             (+ content-top 2)

             visible-h
             (max 1 (- content-h 2))

             _
             (swap! selected #(clamp % 0 (max 0 (dec n))))

             option-indent
             (settings-option-indent)

             ;; Reserve `p/SELECTION_WIDTH` cols at the start of the
             ;; option row for the selection gutter (`>` glyph + 1
             ;; col margin). The cursor itself is painted at
             ;; `(inc lleft)` (the pane's inner edge) by the row
             ;; loop; option body shifts right by the gutter.
             option-x
             (+ lleft 2 option-indent p/SELECTION_WIDTH)

             labels
             (mapv #(settings-option-label % @values) rows)

             base-paint-w
             linner

             base-option-w
             (max 1 (- base-paint-w 2 option-indent p/SELECTION_WIDTH))

             base-desc-w
             (max 1 (- base-option-w check-w))

             base-entries
             (settings-render-entries rows base-option-w base-desc-w)

             scrollable?
             (> (count base-entries) visible-h)

             paint-w
             (if scrollable? (max 1 (dec linner)) linner)

             option-w
             (max 1 (- paint-w 2 option-indent p/SELECTION_WIDTH))

             desc-x
             (+ option-x check-w)

             desc-w
             (max 1 (- option-w check-w))

             entries
             (settings-render-entries rows option-w desc-w)

             visual-n
             (count entries)

             sel-entry-idxs
             (keep-indexed (fn [entry-idx {:keys [row-idx]}]
                             (when (= row-idx @selected) entry-idx))
                           entries)

             ;; Option line of the selected row (first non-description entry).
             selected-visual
             (or (first (keep-indexed (fn [entry-idx {:keys [row-idx part]}]
                                        (when (and (= row-idx @selected) (not= part :option-desc))
                                          entry-idx))
                                      entries))
                 0)

             ;; Last paint row owned by the selected option, INCLUDING its
             ;; wrapped description rows. The scroll window must be able to
             ;; reach this so the trailing desc lines (and, for the bottom-most
             ;; option, the true content end) come into view — otherwise scroll
             ;; caps short of `visual-n - visible-h` and the scrollbar thumb
             ;; never reaches the bottom (selectable rows < paint rows).
             selected-visual-end
             (or (last sel-entry-idxs) selected-visual)

             ;; Visual index where the intro rows (section / subsection /
             ;; info-line) that directly precede the selected option begin.
             ;; The scroll window is selection-driven, so without this the
             ;; first option pins itself to the top and its SECTION HEADER
             ;; (a non-selectable row above it) is clipped forever — you can
             ;; scroll to the first setting but never see its header.
             header-start
             (loop [i (dec selected-visual)]
               (if (and (>= i 0)
                        (contains? #{:section :subsection :info-line} (:part (nth entries i))))
                 (recur (dec i))
                 (inc i)))

             _
             (let [start0
                   (visible-window-start selected-visual @scroll visible-h visual-n)

                   ;; Back UP to reveal those intro headers whenever the
                   ;; option (through its last desc line) still fits in the
                   ;; viewport from `header-start`.
                   start0
                   (if (and (< header-start start0)
                            (<= (- selected-visual-end header-start) (dec visible-h)))
                     header-start
                     start0)

                   ;; Pull the window down to reveal the selected row's last
                   ;; desc line, but never so far that the option line itself
                   ;; scrolls out of view (cap at `selected-visual`).
                   start1
                   (if (>= selected-visual-end (+ start0 visible-h))
                     (min selected-visual (max 0 (- (inc selected-visual-end) visible-h)))
                     start0)]

               (reset! scroll start1))

             ;; Frame 1 search bar: borderless full-width query field sitting
             ;; above the split — identical to the command palette
             ;; (`list-dialog!`) and the session switcher (`navigator-dialog!`),
             ;; which draw no count on the query row. Returns the cursor pos.
             search-cursor
             (draw-text-input-field! g
                                     left
                                     search-row
                                     inner-w
                                     @query
                                     (count @query)
                                     "Search settings…")]

         ;; Full-width rule under the search bar — the same framed-input
         ;; compartment the command palette (`list-dialog!`) and the session
         ;; switcher (`navigator-dialog!`) draw under their query fields, so
         ;; every searchable surface reads the same. `┬` joins the rail
         ;; divider that begins on the row below it.
         (p/set-colors! g t/dialog-border t/dialog-bg)
         (p/draw-separator! g left (+ left inner-w 1) (inc content-top))
         (p/put-str! g lleft (inc content-top) "┬")
         (dotimes [i visible-h]
           (let [entry-idx (+ @scroll i)
                 row-y (+ list-top i)]

             (if (< entry-idx visual-n)
               (let [{:keys [row-idx part text]} (nth entries entry-idx)
                     {:keys [label]} (nth rows row-idx)
                     option-label (nth labels row-idx)
                     selected? (= row-idx @selected)
                     [mark mark-color] (settings-row-mark (nth rows row-idx) @values)]

                 (case part
                   :section
                   (do (p/set-colors! g t/dialog-border t/dialog-bg)
                       (p/fill-rect! g (inc lleft) row-y paint-w 1)
                       (p/put-str! g (+ lleft 2) row-y (settings-section-text label paint-w))
                       (p/set-fg! g t/dialog-hint-key)
                       (p/styled g [p/BOLD] (p/put-str! g (+ lleft 5) row-y label)))

                   :subsection
                   (do (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                       (p/fill-rect! g (inc lleft) row-y paint-w 1)
                       (p/styled
                         g
                         [p/BOLD]
                         (p/put-str! g (+ lleft 2) row-y (settings-subsection-text label paint-w))))

                   :info-line
                   (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                       (p/fill-rect! g (inc lleft) row-y paint-w 1)
                       (p/put-str! g (+ lleft 2) row-y (ellipsize text (max 1 (- paint-w 2)))))

                   :option-desc
                   (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                       (p/fill-rect! g (inc lleft) row-y paint-w 1)
                       (p/put-str! g desc-x row-y (ellipsize text desc-w)))

                   ;; Selection visual: leading `> ` cursor glyph and
                   ;; BOLD label text. Descriptions wrap beneath the
                   ;; option on dim rows, so long labels no longer force
                   ;; descriptions into an ellipsis-only column.
                   (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                       (p/fill-rect! g (inc lleft) row-y paint-w 1)
                       ;; Cursor glyph in the dialog padding column.
                       (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                       (p/draw-selection-marker! g (inc lleft) row-y selected?)
                       ;; Leading status glyph (●/○/◆/▸) via the shared component,
                       ;; which returns the col to start the label at.
                       (let [label-x (p/status-mark! g option-x row-y mark mark-color t/dialog-bg)
                             lbl (ellipsize option-label (max 1 (- option-w p/STATUS_WIDTH)))]

                         (p/set-colors! g t/dialog-fg t/dialog-bg)
                         (if selected?
                           (p/styled g [p/BOLD] (p/put-str! g label-x row-y lbl))
                           (p/put-str! g label-x row-y lbl))))))
               (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                   (p/fill-rect! g (inc lleft) row-y paint-w 1)))))
         ;; Left Table-of-Contents rail (the VS Code settings sidebar): the
         ;; section list with per-section counts; the section owning the
         ;; selected row gets an accent bar. Painted AFTER the right pane so
         ;; the divider never gets overwritten by a pane fill.
         (let [toc (settings-toc rows @selected)]
           (p/set-colors! g t/dialog-border t/dialog-bg)
           (doseq [ry (range list-top (+ content-top content-h))]
             (p/put-str! g lleft ry "│"))
           (dotimes [i (min (count toc) visible-h)]
             (let [{lbl :label cnt :count active? :active?} (nth toc i)
                   ry (+ list-top i)
                   rail-x (inc left)
                   cstr (str cnt)
                   lbl-w (max 1 (- rail-w 2 (count cstr) 1))
                   bg (if active? t/header-active-tab-accent t/dialog-bg)
                   fg (if active? t/dialog-bg t/dialog-fg)]

               (p/set-colors! g fg bg)
               (p/fill-rect! g rail-x ry rail-w 1)
               (if active?
                 (p/styled g [p/BOLD] (p/put-str! g (inc rail-x) ry (ellipsize lbl lbl-w)))
                 (p/put-str! g (inc rail-x) ry (ellipsize lbl lbl-w)))
               (p/set-colors! g (if active? t/dialog-bg t/dialog-hint) bg)
               (p/put-str! g (- (+ rail-x rail-w) (count cstr) 1) ry cstr))))
         (scrollbar/draw! g
                          {:col (+ lleft linner)
                           :top list-top
                           :track-h visible-h
                           :total-h visual-n
                           :inner-h visible-h
                           :scroll @scroll})
         (draw-hint-bar! g
                         left
                         hint-row
                         inner-w
                         [["type" "search"] ["↑/↓" "move"] ["Enter" "change"]
                          ["Esc" "clear/close"]])
         (.setCursorPosition screen search-cursor)
         (.refresh screen Screen$RefreshType/DELTA)
         (let [key
               (read-modal-key! screen)

               selected-row
               (when (pos? n) (nth rows (clamp @selected 0 (dec n))))]

           (when key
             (cond
               (instance? MouseAction key)
               (let [^MouseAction ma
                     key

                     action
                     (.getActionType ma)

                     pos
                     (.getPosition ma)

                     mx
                     (.getColumn pos)

                     my
                     (.getRow pos)

                     bar-col
                     (+ lleft linner)

                     geom
                     (scrollbar/geometry visual-n visible-h visible-h @scroll)]

                 (cond
                   ;; Mouse wheel anywhere in the dialog — scroll the
                   ;; list view; selection follows the wheel direction
                   ;; so the cursor stays in the visible window without
                   ;; the user having to chase it with arrow keys.
                   (or (= action MouseActionType/SCROLL_UP) (= action MouseActionType/SCROLL_DOWN))
                   (let [step (or (modal-wheel-step key)
                                  (if (= action MouseActionType/SCROLL_UP) -1 1))]
                     (swap! selected #(move-settings-selection rows % step))
                     (recur))
                   ;; CLICK_DOWN on the scrollbar thumb — start drag,
                   ;; preserve the grip so the row under the cursor
                   ;; stays glued to the same point on the thumb.
                   (and (= action MouseActionType/CLICK_DOWN)
                        (some? geom)
                        (scrollbar/on-thumb? mx my {:col bar-col :top list-top} geom))
                   (let [thumb-top (+ list-top (long (:thumb-top-rel geom)))]
                     (vreset! scrollbar-drag-offset (- my thumb-top))
                     (recur))
                   ;; CLICK_DOWN on the scrollbar TRACK off-thumb —
                   ;; jump-to-position (modern macOS behaviour). Then
                   ;; arm a drag with a centred grip so an immediate
                   ;; follow-up motion tracks naturally.
                   (and (= action MouseActionType/CLICK_DOWN)
                        (some? geom)
                        (scrollbar/on-track? mx my {:col bar-col :top list-top :track-h visible-h}))
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
                   (and (= action MouseActionType/DRAG) (some? @scrollbar-drag-offset) (some? geom))
                   (do (reset! scroll (or (scrollbar/scroll-from-mouse-y my
                                                                         list-top
                                                                         visible-h
                                                                         visual-n
                                                                         visible-h
                                                                         (long
                                                                           @scrollbar-drag-offset))
                                          0))
                       (recur))
                   (= action MouseActionType/CLICK_RELEASE) (do (vreset! scrollbar-drag-offset nil)
                                                                (recur))
                   :else (recur)))
               :else
               (condp = (key-type key)
                 ;; Esc clears an active search first, then closes on the next press.
                 KeyType/Escape (if (str/blank? @query)
                                  @values
                                  (do (reset! query "")
                                      (reset! selected (first-selectable-index all-rows))
                                      (reset! scroll 0)
                                      (recur)))
                 KeyType/ArrowUp (do (swap! selected #(move-settings-selection rows % -1)) (recur))
                 KeyType/ArrowDown (do (swap! selected #(move-settings-selection rows % 1)) (recur))
                 ;; Backspace edits the live search query.
                 KeyType/Backspace (do (when (seq @query)
                                         (swap! query #(subs % 0 (dec (count %))))
                                         (reset! selected (first-selectable-index
                                                            (filter-settings-rows all-rows @query)))
                                         (reset! scroll 0))
                                       (recur))
                 ;; Any printable character types into the search query (VS Code feel);
                 ;; Enter is the only key that toggles/activates the selected row.
                 KeyType/Character (let [c (key-character key)]
                                     (if (and c (>= (int c) 32))
                                       (do (swap! query str c)
                                           (reset! selected (first-selectable-index
                                                              (filter-settings-rows all-rows
                                                                                    @query)))
                                           (reset! scroll 0)
                                           (recur))
                                       (recur)))
                 KeyType/Enter (do (when selected-row
                                     (activate-settings-row! screen values callbacks selected-row))
                                   (recur))
                 (recur))))))))))
;;; ── Session picker ─────────────────────────────────────────────────────
(defn- short-session-id
  [session]
  (let [id (str (:id session))]
    (subs id 0 (min 8 (count id)))))
(def ^:private untitled-session-title "Untitled session")
(defn- untitled-session-title?
  [title]
  (or (str/blank? (str title))
      (#{"untitled" "untitled session"} (str/lower-case (str/trim (str title))))))
(defn- empty-untitled-session?
  [{:keys [title turn-count]}]
  (and (not (pos? (long (or turn-count 0)))) (untitled-session-title? title)))
(defn- session-title
  [session]
  (let [title
        (:title session)

        base-title
        (if (untitled-session-title? title) untitled-session-title (str title))

        fork-count
        (long (or (:fork-count session) 0))]

    (cond-> base-title
      (pos? fork-count)
      (str " [forks:" fork-count "]"))))
(def ^:private session-dialog-content-w 96)
(defn- date->millis
  [v]
  (cond (instance? java.util.Date v) (.getTime ^java.util.Date v)
        (instance? java.time.Instant v) (.toEpochMilli ^java.time.Instant v)
        (number? v) (long v)
        :else nil))
(defn- date-value
  [v]
  (when-let [ms (date->millis v)]
    (java.util.Date. ms)))

(def ^:private session-table-headers
  ["" "ID" "Title" "Turns" "Created at" "Time" "Modified at" "Time"])
(def ^:private session-table-aligns [:left :left :left :right :left :left :left :left])
(defn- format-session-day
  [v]
  (if-let [date (date-value v)]
    (let [^SimpleDateFormat fmt (SimpleDateFormat. "yyyy-MM-dd" Locale/ROOT)]
      (.setTimeZone fmt (TimeZone/getTimeZone "UTC"))
      (.format fmt date))
    "-"))
(defn- format-session-time
  [v]
  (if-let [date (date-value v)]
    (let [^SimpleDateFormat fmt (SimpleDateFormat. "HH:mm" Locale/ROOT)]
      (.setTimeZone fmt (TimeZone/getTimeZone "UTC"))
      (.format fmt date))
    "-"))
(defn- session-table-widths
  "Column widths for the boxed session table. Total rendered row width equals
   `table-w`, including side borders, inter-cell separators, and padding."
  [table-w]
  (let [n
        (count session-table-headers)

        overhead
        (inc (* 3 n))

        available
        (max n (- table-w overhead))]

    (if (>= available 70)
      (let [active-w
            1

            id-w
            8

            title-w
            (max 10 (- available active-w id-w 5 10 5 11 5))

            turns-w
            5

            created-w
            10

            modified-w
            11

            time-w
            5]

        [active-w id-w title-w turns-w created-w time-w modified-w time-w])
      (let [active-w
            1

            id-w
            (max 1 (min 8 (quot available 8)))

            turns-w
            (max 1 (min 5 (quot available 8)))

            created-w
            (max 1 (min 10 (quot available 7)))

            modified-w
            (max 1 (min 11 (quot available 7)))

            time-w
            (max 1 (min 5 (quot available 12)))

            title-w
            (max 1 (- available active-w id-w turns-w created-w time-w modified-w time-w))]

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
  [{:keys [id turn-count modified-at created-at] :as session} active-id body-w]
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
           {:action :switch
            :id (str (:id session)) ; downstream (switch-session!) accepts full UUID strings
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
  (let [body-x
        (+ left 1 p/SELECTION_WIDTH)

        body-w
        (max 0 (- inner-w 1 p/SELECTION_WIDTH))]

    (if selected?
      (p/styled g [p/BOLD] (p/put-str! g body-x row (ellipsize label body-w)))
      (p/put-str! g body-x row (ellipsize label body-w)))))
(defn session-picker-dialog!
  "Show recent TUI sessions in a fixed-size table. Returns
   `{:action :new}`, `{:action :fork}`, `{:action :switch :id <session-id>}`,
   or nil on Esc."
  [^TerminalScreen screen sessions active-id]
  (let [selected
        (atom 0)

        scroll
        (atom 0)]

    (loop []

      (let [size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows
            (.getRows size)

            g
            (.newTextGraphics screen)

            ;; nil content-h -> shared full-height footprint, matching the
            ;; directory picker (both are long, scrollable browsers)
            bounds
            (draw-dialog-chrome! g cols rows "Sessions" nil)

            {:keys [left inner-w]}
            bounds

            ;; Reserve `p/SELECTION_WIDTH` cols at start of inner area
            ;; for dot marker gutter. Table itself is boxed; marker stays
            ;; outside table so columns never shift.
            body-w
            (max 1 (- inner-w 4 p/SELECTION_WIDTH))

            items
            (session-dialog-items sessions active-id body-w)

            total
            (count items)

            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds)

            table-x
            (+ left 1 p/SELECTION_WIDTH)

            table-top
            content-top

            header-row
            (inc table-top)

            sep-row
            (inc header-row)

            body-top
            (inc sep-row)

            body-h
            (max 1 (- content-h 4))

            bottom-row
            (+ body-top body-h)

            _visible
            (min total body-h)

            _
            (swap! selected #(clamp % 0 (max 0 (dec total))))

            _
            (swap! scroll #(visible-window-start @selected % body-h total))]

        (p/set-colors! g t/dialog-border t/dialog-bg)
        (p/fill-rect! g (inc left) table-top inner-w 1)
        (p/put-str! g table-x table-top (session-table-border-line body-w :top))
        (p/set-colors! g t/dialog-hint-key t/dialog-bg)
        (p/styled g
                  [p/BOLD]
                  (p/fill-rect! g (inc left) header-row inner-w 1)
                  (p/put-str! g table-x header-row (session-dialog-header body-w)))
        ;; Re-paint the header's side `│` borders in the border color: the
        ;; header row was painted in dialog-hint-key, which would otherwise
        ;; leave the vertical edges a different color than the top/separator/
        ;; bottom chrome (same fix as the body rows + boxed-table).
        (p/set-colors! g t/dialog-border t/dialog-bg)
        (p/put-str! g table-x header-row "│")
        (p/put-str! g (+ table-x (dec body-w)) header-row "│")
        (p/fill-rect! g (inc left) sep-row inner-w 1)
        (p/put-str! g table-x sep-row (session-table-border-line body-w :middle))
        (dotimes [i body-h]
          (let [idx (+ @scroll i)
                row (+ body-top i)]

            (if (< idx total)
              (do (draw-session-row! g left row inner-w (= idx @selected) (:label (nth items idx)))
                  ;; Re-paint the side `│` borders in the border color: draw-session-row!
                  ;; painted the whole boxed row (borders included) in dialog-fg, which
                  ;; would otherwise leave the vertical edges (and the active `●` row's
                  ;; frame) a different color than the top/separator/bottom chrome.
                  (p/set-colors! g t/dialog-border t/dialog-bg)
                  (p/put-str! g table-x row "│")
                  (p/put-str! g (+ table-x (dec body-w)) row "│")
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
              (condp = (key-type key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                      (recur))
                KeyType/Enter (when (pos? total) (select-keys (nth items @selected) [:action :id]))
                KeyType/Character (let [raw-c (key-character key)
                                        c (lower-character raw-c)]

                                    (case c
                                      \n
                                      {:action :new}

                                      \f
                                      {:action :fork}

                                      (recur)))
                (recur)))))))))
;;; ── Global navigator (Ctrl+G) ───────────────────────────────────────────────
;; One row per session. Per the locked 1:1 session<->workspace model a
;; session IS its workspace, so the navigator shows a single unified list:
;; no "Kind" column and no session/workspace mode split. The old design
;; emitted both a session row AND a workspace row per entry, so every
;; entry showed up twice with a contradictory "Kind".
(def ^:private navigator-columns
  [{:id :title :label "Title" :flex 1} {:id :session :label "Session" :width 8}
   {:id :draft :label "Draft" :width 8} {:id :dir :label "Project" :width 22}
   {:id :status :label "Status" :width 10}])
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
  (let [id
        (:id session)

        active?
        (= (str id)
           (some-> active-session-id
                   str))]

    {:id (str "session:" id)
     :focused? active?
     :title (session-title session)
     :session (short-session-id session)
     ;; Enriched by the caller (screen/show-sessions!) from each session's
     ;; pinned workspace: whether it's in a draft, and the directory it works
     ;; in. `—` = trunk / no draft.
     :draft (or (not-empty (:draft-label session)) "—")
     ;; The persistent Project wins as the cluster/label; sessions with no
     ;; project fall back to their project dir (the pre-project behaviour).
     :group (not-empty (:project_name session))
     ;; Manual order within the project (movable tabs); nil for projectless.
     :position (:project_position session)
     :dir (or (not-empty (:project_name session)) (not-empty (:work-dir session)) "—")
     :status (if active? "● focused" (str (long (or (:turn-count session) 0)) " turns"))
     :created (navigator-stamp (:created-at session))
     :modified (navigator-stamp (:modified-at session))
     :target {:action :switch :id id}}))
(defn- group-rows-by-dir
  "Regroup navigator rows so sessions of the SAME project (`:dir`) sit
   adjacent. Rows arrive newest-first, so first-appearance group order =
   projects by their most recent session; within a group the recency order
   is preserved."
  [rows]
  (let [order
        (distinct (map :dir rows))

        by-dir
        (group-by :dir rows)]

    ;; Within each project group, honour the persisted manual order
    ;; (`:position`); projectless rows (nil position) keep recency order.
    (vec (mapcat (fn [d]
                   (sort-by #(or (:position %) Long/MAX_VALUE) (by-dir d)))
                 order))))
(defn- navigator-all-rows
  "Sessions arrive newest-modified-first from `tui-session-summaries`. The
   focused session pins to the top; the rest are grouped by PROJECT
   (`:work-dir`), groups ordered by their most recent session, rows inside a
   group keeping recency order. Empty untitled
   shells are hidden by default; Ctrl+U in the navigator reveals them.

   No synthetic `+ New Session` row — creating a session is the `N`
   modifier (shown in the hint bar), not a list entry. A real-looking
   action row mixed in with actual sessions read as just another session
   and pushed the newest real one down."
  [{:keys [sessions active-session-id show-empty-untitled?]}]
  (let [focused-id
        (some-> active-session-id
                str)

        focused?
        (fn [s]
          (= (str (:id s)) focused-id))

        rows
        (->> sessions
             ;; Keep the FOCUSED session even when it is an empty
             ;; untitled shell — you must always see "you are here".
             (remove
               #(and (not show-empty-untitled?) (empty-untitled-session? %) (not (focused? %))))
             (mapv #(navigator-session-row active-session-id %)))]

    ;; Focused row pinned to the top; the rest regrouped by project, each
    ;; group ordered by its most recent session (see `group-rows-by-dir`).
    (vec (concat (filter :focused? rows) (group-rows-by-dir (remove :focused? rows))))))
(defn- navigator-visible-rows [rows query] (vec (filter #(table/row-matches? % query) rows)))
(defn- navigator-cell-spans
  "[[x-offset col-width] …] for each column inside a `boxed-row-line`, so
   cell text can be overlaid on a border-colored frame."
  [widths]
  (first (reduce (fn [[acc off] w]
                   [(conj acc [off (long w)]) (+ off (long w) 3)])
                 [[] 2]
                 widths)))
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
  (let [draw
        (fn []
          (doseq [[i [off w]] (map-indexed vector (navigator-cell-spans widths))]
            (p/put-str! g (+ x off) row (table/fit-cell (nth cells i "") w (nth aligns i :left)))))]
    (if bold? (p/styled g [p/BOLD] (draw)) (draw))))
(defn- dir-canon
  ^java.io.File [^java.io.File f]
  (try (.getCanonicalFile f)
       (catch Throwable t
         (tel/log! :debug ["dialogs: getCanonicalFile failed" (str f) (ex-message t)])
         (.getAbsoluteFile f))))

(defn- list-subdirs
  "Case-insensitively sorted names of the visible child directories of `dir`,
   INCLUDING dot-directories (`.git`, `.config`, …) — a filesystem browser
   that silently drops hidden folders is worse than useless when the thing
   you're looking for lives in one.
   Unreadable dirs (permission denied → nil listing) yield an empty vec."
  [^java.io.File dir]
  (->> (try (.listFiles dir)
            (catch Throwable t
              (tel/log! :debug ["dialogs: listFiles failed" (str dir) (ex-message t)])
              nil))
       (filter (fn [^java.io.File f]
                 (.isDirectory f)))
       (map (fn [^java.io.File f]
              (.getName f)))
       (sort String/CASE_INSENSITIVE_ORDER)
       vec))

(defn- ctrl-letter?
  "True when `key` is Ctrl+<letter> (case-insensitive)."
  [key letter]
  (and (input/ctrl-modifier? key)
       (= KeyType/Character (key-type key))
       (= (lower-key-character key) letter)))

(defn directory-picker-dialog!
  "Browse the filesystem like a file explorer and manage this session's
   filesystem permissions. Returns the chosen absolute path (string) to OPEN
   IN A NEW TAB, or nil on Esc.

   The picker has TWO clearly separated zones. The read-only HEADER shows the
   current path, the session's filesystem roots (when `:db-info` + `:workspace-id`
   are supplied), and whether the current folder is itself a root. The navigable
   LIST below holds ONLY filesystem entries: `..`, subfolders (navigable), and
   files (dimmed, for context). Nothing in the list is an \"action\" — actions
   are modifier keys, exactly like the session navigator:

     Up/Down       move          Enter / →   open folder (·· ascends)
     ←             up a level    Tab         open current folder in a new tab
     Ctrl+A        add the HIGHLIGHTED subfolder as a filesystem root (or the
                   current folder when a file / `..` is highlighted); removes it
                   if already a root (no-op without a session)
     Ctrl+R        make the HIGHLIGHTED folder (or the current one) the
                   session's ROOT — shell, file tools, and search work there
                   from the next turn (needs `:session-state-id`)
     Ctrl+N        create a new folder here, then enter it
     type / Bksp   filter the list incrementally     Esc   close

   `:purpose` only sets the dialog title (`:add-root` → \"Add a filesystem
   directory\", `:new-folder` → \"Create a directory\", `:remove` → \"Remove a
   filesystem directory\"); the controls are identical regardless."
  [^TerminalScreen screen start-path & {:keys [sid workspace-info purpose]}]
  (let [path
        (atom (dir-canon (java.io.File. ^String (or start-path "."))))

        selected
        (atom 0)

        scroll
        (atom 0)

        query
        (atom "")

        ;; Last C-a / C-r outcome, so a FAILED mutation is visible instead of
        ;; silently swallowed. `[dir-canon-str level text]` — shown only while the
        ;; picker still sits on the same folder (navigating away clears it).
        notice
        (atom nil)

        ;; C-r (change root) repoints the session to a NEW workspace row — the
        ;; picker tracks the live id so the header roots stay truthful after.
        wid
        (atom (:id workspace-info))

        ;; Live gateway workspace snapshot (root + filesystem-roots). Seeded from
        ;; the caller and REFRESHED from every mutation's return value, so the
        ;; header reflects the DAEMON's authoritative roots — the local db-info
        ;; index the picker used to read/write never reached the running session.
        ws-info
        (atom workspace-info)

        manager?
        (boolean (and sid workspace-info))

        title
        (case purpose
          :add-root
          "Add a filesystem directory"

          :new-folder
          "Create a directory"

          :remove
          "Remove a filesystem directory"

          "Filesystem Permissions")

        norm
        (fn [p]
          (some-> p
                  str
                  not-empty
                  (#(try (str (workspace/normalize-root %))
                         (catch Throwable t
                           (tel/log! :debug
                                     ["dialogs: normalize-root failed" (str %) (ex-message t)])
                           %)))))]

    (loop []

      (let [^java.io.File dir
            @path

            up?
            (some? (.getParentFile dir))

            ws
            (when manager? @ws-info)

            base
            (:root ws)

            extras
            (:filesystem-roots ws)

            dir-canon-str
            (norm (.getPath dir))

            base-canon
            (norm base)

            extra-set
            (set (keep #(norm (:trunk %)) extras))

            q
            (str/lower-case (str/trim @query))

            match?
            (fn [n]
              (or (str/blank? q) (str/includes? (str/lower-case n) q)))

            subdirs
            (filterv match? (list-subdirs dir))

            ;; The list is PURELY filesystem navigation — subfolders only.
            ;; Files are hidden (they were inert noise); every folder shows a
            ;; ●/○ membership mark (manager mode) so you can tell, before you
            ;; act, whether it is already a filesystem root.
            root-of?
            (fn [n]
              (let [s (norm (.getPath (dir-canon (java.io.File. dir ^String n))))]
                (or (= s base-canon) (contains? extra-set s))))

            entries
            (vec (concat (when up? [{:kind :up}])
                         (map (fn [n]
                                {:kind :into :name n :root? (boolean (and manager? (root-of? n)))})
                              subdirs)))

            total
            (count entries)

            ;; C-a acts on the HIGHLIGHTED entry when it's a subfolder, so you
            ;; can add `foo/` straight from the list without stepping into it;
            ;; with a file or `..` highlighted it falls back to THIS folder.
            sel-idx
            (clamp @selected 0 (max 0 (dec total)))

            sel-entry
            (when (pos? total) (nth entries sel-idx))

            target-dir
            (if (= :into (:kind sel-entry))
              (dir-canon (java.io.File. dir ^String (:name sel-entry)))
              dir)

            target-str
            (norm (.getPath target-dir))

            target-base?
            (= target-str base-canon)

            target-already?
            (or target-base? (contains? extra-set target-str))

            target-name
            (if (= :into (:kind sel-entry)) (str (:name sel-entry) "/") "this folder")

            size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows-n
            (.getRows size)

            g
            (.newTextGraphics screen)

            bounds
            (draw-dialog-chrome! g cols rows-n title (default-content-height rows-n))

            {:keys [left inner-w]}
            bounds

            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds)

            list-x
            (+ left 2)

            list-w
            (max 1 (- inner-w 3))

            ;; ── Header zone ────────────────────────────────────────────────
            ;; Manager mode promotes this session's filesystem roots to a real
            ;; boxed section (each root a row: ● mark + abbreviated path + a
            ;; tag), then a BROWSE line for the current folder. Non-manager
            ;; mode is just the breadcrumb.
            notice-here
            (when (and manager? @notice (= (first @notice) dir-canon-str)) @notice)

            tag-w
            12

            path-w
            (max 1 (- list-w tag-w 7))

            roots
            (when manager?
              (vec (concat (when base [{:path (str base) :tag "root"}])
                           (map (fn [e]
                                  {:path (:trunk e) :tag "added"})
                                extras))))

            root-row
            (fn [{:keys [path tag]}]
              (table/boxed-row-line [path-w tag-w]
                                    [(str "● " (abbreviate-home path)) tag]
                                    [:left :left]))

            filter-suffix
            (when-not (str/blank? @query) (str "   [filter: " @query "]"))

            header-rows
            (if manager?
              (vec
                (concat [{:kind :blank :text ""}
                         {:kind :section :text (str "FILESYSTEM (" (count roots) ")")}]
                        (if (seq roots)
                          (concat [{:kind :box :text (table/boxed-border-line [path-w tag-w] :top)}]
                                  (map (fn [r]
                                         {:kind :root :text (root-row r)})
                                       roots)
                                  [{:kind :box
                                    :text (table/boxed-border-line [path-w tag-w] :bottom)}])
                          [{:kind :hint :text "  none yet — C-a on a folder below adds it"}])
                        [{:kind :rule :text (apply str (repeat list-w "─"))} {:kind :blank :text ""}
                         {:kind :browse
                          :text (str "BROWSE  " (abbreviate-home (.getPath dir)) filter-suffix)}]
                        (when notice-here
                          [{:kind :notice
                            :text (str (case (second notice-here)
                                         :error
                                         "✖ "

                                         :ok
                                         "✔ "

                                         "")
                                       (nth notice-here 2))
                            :error? (= :error (second notice-here))}])
                        [{:kind :blank :text ""}]))
              [{:kind :blank :text ""}
               {:kind :browse :text (str (abbreviate-home (.getPath dir)) filter-suffix)}
               {:kind :blank :text ""}])

            header-n
            (count header-rows)

            hdr-h
            (min header-n (max 1 (dec content-h)))

            list-top
            (+ content-top hdr-h)

            body-h
            (clamp total 1 (max 1 (- content-h hdr-h)))

            _
            (swap! selected #(clamp % 0 (max 0 (dec total))))

            _
            (swap! scroll #(visible-window-start @selected % body-h total))

            ascend!
            (fn []
              (when up?
                (reset! path (dir-canon (.getParentFile dir)))
                (reset! query "")
                (reset! selected 0)
                (reset! scroll 0)))

            reset-list!
            (fn []
              (reset! selected 0)
              (reset! scroll 0))

            toggle-root!
            (fn []
              (when manager?
                (cond target-base?
                      (reset! notice
                        [dir-canon-str :error
                         "This is the session's root — C-r on another folder to change it"])
                      :else
                      (try (let [updated
                                 (if target-already?
                                   (vis/gateway-remove-filesystem-root! sid (.getPath target-dir))
                                   (vis/gateway-add-filesystem-root! sid (.getPath target-dir)))]
                             (reset! ws-info updated)
                             (reset! wid (:id updated)))
                           (reset! notice [dir-canon-str :ok
                                           (str (if target-already? "Removed " "Added ")
                                                target-name)])
                           (catch Throwable t
                             (reset! notice [dir-canon-str :error (or (ex-message t) (str t))]))))))

            ;; C-r: make the highlighted (or current) folder the session's
            ;; PRIMARY root — repoints the session workspace; shell/file
            ;; tools/search follow from the next turn.
            set-root!
            (fn []
              (when manager?
                (cond (nil? sid) (reset! notice
                                   [dir-canon-str :error
                                    "No session yet — send a message first, then set a root"])
                      target-base? (reset! notice [dir-canon-str :ok "Already the session's root"])
                      :else
                      (try (let [new-ws (vis/gateway-change-root! sid (.getPath target-dir))]
                             (reset! ws-info new-ws)
                             (reset! wid (:id new-ws))
                             ;; Synchronously warm the git status cache for the
                             ;; NEW root so the footer paints the new repo on
                             ;; the very next frame instead of "No git" for a
                             ;; refresh cycle (stale-while-revalidate miss on
                             ;; the unseen cwd). The TUI C-r path bypasses
                             ;; turns, so the turn-completion seed can't cover
                             ;; it — seed right here, at the change site.
                             (when-let [nr (:root new-ws)]
                               (try (git/seed-working-tree-status! (java.io.File. nr))
                                    (catch Throwable t
                                      (tel/log! :warn
                                                ["dialogs: git seed-working-tree-status! failed" nr
                                                 (ex-message t)])
                                      nil)))
                             (reset! notice [dir-canon-str :ok
                                             (str "Root changed — session now works in "
                                                  target-name)]))
                           (catch Throwable t
                             (reset! notice [dir-canon-str :error (or (ex-message t) (str t))]))))))

            enter!
            (fn []
              ;; Pure navigation: mutate state, then return nil so the
              ;; loop RECURS. Tab is the "choose this folder" action (it
              ;; returns the path string); Enter must NEVER return a value
              ;; — `ascend!` / `reset-list!` end in `reset!`, which yields a
              ;; Long, and `(or (enter!) (recur))` would mis-return that as
              ;; the chosen path → `(File. ^String <Long>)` ClassCastException.
              (when (pos? total)
                (let [entry (nth entries @selected)]
                  (case (:kind entry)
                    :up
                    (ascend!)

                    :into
                    (do (reset! path (dir-canon (java.io.File. dir ^String (:name entry))))
                        (reset! query "")
                        (reset-list!))

                    nil)
                  nil)))]

        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc left) content-top inner-w content-h)
        ;; Header zone (read-only): section label, boxed roots, browse line.
        (dotimes [i hdr-h]
          (let [{:keys [kind text error?]} (nth header-rows i)
                row (+ content-top i)
                fg (case kind
                     :section
                     t/dialog-hint-key

                     :root
                     t/dialog-fg

                     :box
                     t/dialog-border

                     :browse
                     t/dialog-hint-key

                     :rule
                     t/dialog-border

                     :notice
                     (if error? t/code-error-fg t/dialog-hint-key)

                     t/dialog-hint)
                rendered (ellipsize (str text) list-w)]

            (p/set-colors! g fg t/dialog-bg)
            (p/fill-rect! g (inc left) row inner-w 1)
            (p/put-str! g list-x row rendered)
            ;; Re-paint the boxed root row's side `│` in the border color so the
            ;; vertical edges match the top/bottom chrome instead of reading
            ;; brighter (dialog-fg) than it — same trick as `boxed-table/render!`.
            (when (= kind :root)
              (p/set-colors! g t/dialog-border t/dialog-bg)
              ;; Re-paint EVERY box glyph (outer edges AND the inner column
              ;; separator) in the border color so none reads brighter than
              ;; the top/bottom chrome.
              (dotimes [ci (count rendered)]
                (when (= (.charAt ^String rendered ci) \│) (p/put-str! g (+ list-x ci) row "│"))))))
        ;; Navigable list zone — subfolders only, each with a ●/○ root mark.
        (dotimes [i body-h]
          (let [idx (+ @scroll i)]
            (when (< idx total)
              (let [row (+ list-top i)
                    entry (nth entries idx)
                    selected? (= idx @selected)
                    bg (if selected? t/dialog-title-bg t/dialog-bg)
                    ;; On the selected slab the text must jump to the title
                    ;; FG (near-white) — dialog-fg on title-bg is base0-on-base01
                    ;; in Solarized: too low contrast to read.
                    fg (if selected? t/dialog-title-fg t/dialog-fg)
                    mark (when (and manager? (= :into (:kind entry))) (if (:root? entry) "●" "○"))
                    mark-fg (cond selected? t/dialog-title-fg
                                  (:root? entry) t/dialog-hint-key
                                  :else t/dialog-hint)
                    up? (= :up (:kind entry))
                    label (if up? "↑ Go up" (str (:name entry) "/"))
                    ;; ".." stays flush-left; browsable folders indent UNDER
                    ;; it so the ●/○ marks + names form their own column:
                    ;;   ↑ Go up
                    ;;     ● foo/
                    ;;     ○ bar/
                    mark-x (+ list-x 2)
                    name-x (cond (not manager?) list-x
                                 up? list-x
                                 :else (+ list-x 4))]

                (p/set-colors! g fg bg)
                (p/fill-rect! g (inc left) row inner-w 1)
                ;; Paint the selected-row marker BEFORE the label: the marker is
                ;; two columns wide, and the up row starts at list-x, so painting
                ;; it last overwrote the ↑ with the marker's trailing space.
                (p/draw-selection-marker! g
                                          (inc left)
                                          row
                                          selected?
                                          (if selected? t/dialog-title-fg t/dialog-hint-key))
                (when mark (p/set-colors! g mark-fg bg) (p/put-str! g mark-x row mark))
                (p/set-colors! g fg bg)
                ;; ↑ "Go up" arrow: accent when unselected (visible on the light
                ;; and dark dialog bg) and WHITE when selected/hovered (visible
                ;; on the title slab in every theme).
                (let [label-fg
                      (if up? (if selected? t/dialog-title-fg t/header-active-tab-accent) fg)]
                  (p/set-colors! g label-fg bg)
                  (p/put-str! g
                              name-x
                              row
                              (ellipsize label (max 1 (- list-w (- name-x list-x))))))))))
        (draw-hint-bar! g
                        left
                        hint-row
                        inner-w
                        (if manager?
                          [["↑/↓" "move"] ["Enter" "open"] ["C-a" "add/remove root"]
                           ["C-r" "set as root"] ["C-n" "new folder"] ["Tab" "open tab"]
                           ["Esc" "close"]]
                          [["↑/↓" "move"] ["Enter" "open"] ["C-n" "new folder"] ["Tab" "open tab"]
                           ["Esc" "close"]]))
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (cond
              (modal-escape-key? key) nil
              (modal-wheel-step key)
              (do (swap! selected #(clamp (+ % (modal-wheel-step key)) 0 (max 0 (dec total))))
                  (recur))
              ;; Actions are MODIFIER keys, never list rows, so plain typing
              ;; (incl. the letters a/n) keeps filtering the list.
              (ctrl-letter? key \a) (do (toggle-root!) (recur))
              (ctrl-letter? key \r) (do (set-root!) (recur))
              (ctrl-letter? key \n)
              (let [nm (text-input-dialog! screen
                                           "New folder" (str "Create under " (.getPath dir))
                                           :flat? true)]
                (when (and nm (seq (str/trim nm)))
                  (try (workspace/create-dir! (.getPath dir) nm)
                       (reset! path (dir-canon (java.io.File. dir ^String (str/trim nm))))
                       (reset! query "")
                       (reset-list!)
                       (catch Throwable t
                         (tel/log! :warn
                                   ["dialogs: create-dir! failed" (.getPath dir) nm (ex-message t)])
                         nil)))
                (recur))
              (input/paste-start? key) (do (let [pasted (drain-modal-paste! screen)]
                                             (when (seq pasted)
                                               (swap! query str (str/replace pasted #"\s+" " "))
                                               (reset-list!)))
                                           (recur))
              (modal-enter-key? key) (or (enter!) (recur))
              :else
              (condp = (key-type key)
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                      (recur))
                KeyType/ArrowRight (or (enter!) (recur))
                KeyType/ArrowLeft (do (ascend!) (recur))
                KeyType/Tab (.getPath dir)
                KeyType/Backspace
                (do (swap! query #(if (seq %) (subs % 0 (dec (count %))) %)) (reset-list!) (recur))
                KeyType/Character (let [c (key-character key)]
                                    (when (and c
                                               (not (input/alt-modifier? key))
                                               (not (input/ctrl-modifier? key))
                                               (not (iso-control-character? c)))
                                      (swap! query str c)
                                      (reset-list!))
                                    (recur))
                (recur)))))))))

(defn navigator-dialog!
  "Global C-g picker. Returns a target action map or nil on Esc."
  [^TerminalScreen screen opts]
  (let [query
        (atom "")

        selected
        (atom 0)

        scroll
        (atom 0)

        ;; Rows between the thumb top and the user's grip point, saved on
        ;; CLICK_DOWN so a drag keeps the cursor glued to the same spot on
        ;; the thumb. nil = not dragging.
        scrollbar-drag-offset
        (volatile! nil)

        show-empty-untitled?
        (atom (boolean (:show-empty-untitled? opts)))]

    (loop []

      (let [rows
            (navigator-all-rows (assoc opts :show-empty-untitled? @show-empty-untitled?))

            visible-rows
            (navigator-visible-rows rows @query)

            total
            (count visible-rows)

            size
            (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

            cols
            (.getColumns size)

            rows-n
            (.getRows size)

            g
            (.newTextGraphics screen)

            bounds
            (draw-dialog-chrome! g
                                 cols
                                 rows-n
                                 "Sessions"
                                 (navigator-content-w cols)
                                 (default-content-height rows-n))

            {:keys [left right inner-w]}
            bounds

            ;; Input is pinned to a FIXED top position (no vertical
            ;; centering) so it never jumps as the row count changes
            ;; while typing. The table grows DOWN from there.
            {:keys [content-top content-h hint-row]}
            (dialog-layout bounds)

            table?
            (pos? total)

            query-row
            content-top

            ;; Reserve a right-side scrollbar gutter so the bar is a
            ;; SEPARATE element, not painted on top of the table's right
            ;; border. `content-w` shrinks BOTH the query box and the
            ;; table by the gutter so their right edges stay aligned; the
            ;; bar then floats one blank column clear of that edge.
            sb-gutter
            2

            content-w
            (max 1 (- inner-w sb-gutter))

            ;; Table is inset one column on each side, exactly matching the
            ;; query box; the selection marker lives INSIDE the left
            ;; padding, so there is no external gutter.
            table-x
            (+ left 2)

            table-w
            (max 1 (- content-w 2))

            table-body-w
            (max 1 (- table-w 2))

            ;; table-x+table-w-1 is the table's right border; +1 leaves a
            ;; blank separator column before the bar.
            scrollbar-col
            (+ table-x table-w 1)

            table-widths
            (table/column-widths navigator-columns (max 1 table-body-w))

            aligns
            (mapv #(or (:align %) :left) navigator-columns)

            top-row
            (+ content-top 2)

            header-row
            (inc top-row)

            sep-row
            (inc header-row)

            body-top
            (inc sep-row)

            ;; Height = actual row count (capped) so the bottom border is
            ;; glued to the last row instead of floating below blanks.
            body-h
            (clamp total 1 (max 1 (- content-h 6)))

            bottom-row
            (+ body-top body-h)

            _
            (swap! selected #(clamp % 0 (max 0 (dec total))))

            _
            (swap! scroll #(visible-window-start @selected % body-h total))]

        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc left) content-top inner-w content-h)
        (let [cursor-pos
              (draw-text-input-field! g (inc left) query-row content-w @query (count @query))]
          ;; Border rule under the search field, T-joined to the dialog frame —
          ;; the same framed-input look the C-x p command palette uses
          ;; (`list-dialog!`), so the session switcher reads as one cohesive box.
          (p/set-colors! g t/dialog-border t/dialog-bg)
          (p/draw-separator! g left right (inc content-top))
          (if-not table?
            ;; Empty state: no skeleton table, just a quiet line below input.
            (let [hidden-count (count (filter empty-untitled-session? (:sessions opts)))
                  msg (cond (not (str/blank? @query)) "No matches"
                            (and (pos? hidden-count) (not @show-empty-untitled?))
                            "Only empty untitled sessions hidden"
                            :else "No sessions yet")
                  msg-x (+ table-x (max 0 (quot (- table-w (count msg)) 2)))]

              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g msg-x (+ content-top 3) msg))
            (do (p/set-colors! g t/dialog-border t/dialog-bg)
                (p/put-str! g table-x top-row (table/boxed-border-line table-widths :top))
                (draw-navigator-row! g
                                     table-x
                                     header-row
                                     table-widths
                                     (navigator-with-selection-gutter (mapv #(or (:label %) "")
                                                                            navigator-columns))
                                     aligns
                                     t/dialog-hint-key
                                     true)
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
                        row-fg (if focused? t/dialog-hint-key t/dialog-fg)
                        cells (navigator-with-selection-gutter (mapv (fn [{:keys [id]}]
                                                                       (str (get entry id "")))
                                                                     navigator-columns))]

                    (draw-navigator-row! g
                                         table-x
                                         row
                                         table-widths
                                         cells
                                         aligns
                                         row-fg
                                         (or selected? focused?))
                    ;; Shared cursor marker, painted by the project-wide primitive into
                    ;; the reserved `p/selection-prefix` first-cell gutter.
                    (p/draw-selection-marker! g (+ table-x 2) row selected? t/dialog-hint-key)))
                (p/set-colors! g t/dialog-border t/dialog-bg)
                (p/put-str! g table-x bottom-row (table/boxed-border-line table-widths :bottom))
                (when (> total body-h)
                  (scrollbar/draw! g
                                   {:col scrollbar-col
                                    :top body-top
                                    :track-h body-h
                                    :total-h total
                                    :inner-h body-h
                                    :scroll @scroll}))))
          (draw-hint-bar! g
                          left
                          hint-row
                          inner-w
                          [["↑/↓" "move"] ["Enter" "open"] ["C-n" "new"] ["C-f" "fork"]
                           ["C-d" "delete"] ["C-b" "project"]
                           [(keymap/chord \u) (if @show-empty-untitled? "hide empty" "show empty")]
                           ["Esc" "cancel"]])
          (.setCursorPosition screen cursor-pos)
          (.refresh screen Screen$RefreshType/DELTA))
        (let [key
              (read-modal-key! screen)

              reset-list!
              (fn []
                (reset! selected 0)
                (reset! scroll 0))]

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
              (and (instance? MouseAction key)
                   (> total body-h)
                   (let [action (.getActionType ^MouseAction key)]
                     (or (= action MouseActionType/DRAG)
                         (= action MouseActionType/CLICK_RELEASE)
                         (and (= action MouseActionType/CLICK_DOWN)
                              (let [pos (.getPosition ^MouseAction key)]
                                (scrollbar/on-track?
                                  (.getColumn pos)
                                  (.getRow pos)
                                  {:col scrollbar-col :top body-top :track-h body-h :x-band 2}))))))
              (let [^MouseAction ma
                    key

                    action
                    (.getActionType ma)

                    pos
                    (.getPosition ma)

                    mx
                    (.getColumn pos)

                    my
                    (.getRow pos)

                    geom
                    (scrollbar/geometry total body-h body-h @scroll)

                    apply-scroll!
                    (fn [grip]
                      (let [ns
                            (or (scrollbar/scroll-from-mouse-y my body-top body-h total body-h grip)
                                0)]
                        (reset! scroll ns)
                        (swap! selected #(clamp % ns (min (dec total) (+ ns (dec body-h)))))))]

                (cond (= action MouseActionType/CLICK_RELEASE)
                      (do (vreset! scrollbar-drag-offset nil) (recur))
                      (and (= action MouseActionType/CLICK_DOWN)
                           (some? geom)
                           (scrollbar/on-thumb? mx
                                                my
                                                {:col scrollbar-col :top body-top :x-band 2}
                                                geom))
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
                   (= KeyType/Character (key-type key))
                   (= (lower-key-character key) \n))
              {:action :new}
              ;; Ctrl+F → fork the highlighted session into a new tab.
              (and (input/ctrl-modifier? key)
                   (= KeyType/Character (key-type key))
                   (= (lower-key-character key) \f))
              (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                {:action :fork :id id}
                (recur))
              ;; Ctrl+D → delete the highlighted session.
              (and (input/ctrl-modifier? key)
                   (= KeyType/Character (key-type key))
                   (= (lower-key-character key) \d))
              (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                {:action :delete :id id}
                (recur))
              ;; Ctrl+B → move the highlighted session to a project.
              (and (input/ctrl-modifier? key)
                   (= KeyType/Character (key-type key))
                   (= (lower-key-character key) \b))
              (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                {:action :project :id id}
                (recur))
              (input/ctrl-char? key \u) (do (swap! show-empty-untitled? not) (reset-list!) (recur))
              ;; Clipboard paste → append into the query filter.
              (input/paste-start? key) (do (let [pasted (drain-modal-paste! screen)]
                                             (when (seq pasted)
                                               (swap! query str (str/replace pasted #"\s+" " "))
                                               (reset-list!)))
                                           (recur))
              :else (condp = (key-type key)
                      KeyType/Escape nil
                      KeyType/ArrowUp (if (and (input/reorder-modifier? key) (pos? total))
                                        (if-let [id (:id (:target (nth visible-rows @selected)))]
                                          {:action :reorder :id id :dir :up}
                                          (recur))
                                        (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total))))
                                            (recur)))
                      KeyType/ArrowDown
                      (if (and (input/reorder-modifier? key) (pos? total))
                        (if-let [id (:id (:target (nth visible-rows @selected)))]
                          {:action :reorder :id id :dir :down}
                          (recur))
                        (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur)))
                      KeyType/Enter (when (pos? total) (:target (nth visible-rows @selected)))
                      KeyType/Backspace (do (swap! query #(if (seq %) (subs % 0 (dec (count %))) %))
                                            (reset-list!)
                                            (recur))
                      ;; Plain printable character → filter query. Skip control
                      ;; chars and Alt/Ctrl-modified keys (those are commands).
                      KeyType/Character (let [c (key-character key)]
                                          (when (and c
                                                     (not (input/alt-modifier? key))
                                                     (not (input/ctrl-modifier? key))
                                                     (not (iso-control-character? c)))
                                            (swap! query str c)
                                            (reset-list!))
                                          (recur))
                      (recur)))))))))
;;; ── Command palette ─────────────────────────────────────────────────────────
(def palette-commands
  "Command palette entries. Each is {:id keyword :label str}. The `:id` is the
   action the screen's `run-command!` executes. Quit is intentionally NOT here
   — use Ctrl+C to quit.

   The palette is THE discoverable entry point for every app verb: opened with
   Ctrl+P (reliable on every terminal, unlike Alt/Option chords on macOS) and
   filtered by typing. Mirrors the web command palette so both channels expose
   the same command set."
  ;; Whole-session Markdown copy lives in the header as an icon. Workspace ops
  ;; are slash-only (`/workspace …`) and surface through `menu-commands` which
  ;; aggregates them from the engine slash registry (passed as extra-commands).
  [{:id :cycle-model :label "Cycle Model"} {:id :pick-model :label "Choose Model…"}
   {:id :cycle-reasoning :label "Cycle Reasoning Effort"}
   {:id :cycle-verbosity :label "Cycle Answer Length"} {:id :search-open :label "Search in Session"}
   {:id :open-resources :label "Backgrounds"} {:id :show-sessions :label "Switch Session"}
   {:id :open-magit :label "Git Status (Magit)"} {:id :open-dirs :label "Filesystem Permissions"}
   {:id :pick-file :label "Attach File"} {:id :toggle-voice-recording :label "Voice Recording"}
   {:id :new-session :label "New Session"} {:id :fork-session :label "Fork Session"}
   {:id :close-tab :label "Close Tab"} {:id :providers :label "Configure Providers"}
   {:id :settings :label "Settings"} {:id :toggle-all-details :label "Fold / Unfold All"}
   {:id :toggle-detail-labels :label "Label Folds — jump to one"}
   {:id :toggle-help :label "Keyboard Shortcuts"}])
(defn searchable-select!
  "Type-to-filter selection list — the searchable spine of the command palette.
   Thin wrapper over `list-dialog!` (filter on, content-sized, palette
   placeholder). Returns the FULL chosen item map (so callers recover
   `:id` / slash keys), or nil on Esc."
  [^TerminalScreen screen title items]
  (list-dialog! screen
                title
                items
                {:filter? true :placeholder "Type a command…" :enter-label "run" :height :content}))
(defn command-palette!
  "Show the searchable command palette. Returns the FULL chosen command map
   (so the caller's `run-command!` can read `:id` and any slash keys), or nil
   on Esc. `extra-commands` are the engine slash roots appended after the
   built-ins. Opened with C-x C-p (Emacs C-x prefix + Ctrl+P)."
  ([^TerminalScreen screen] (command-palette! screen []))
  ([^TerminalScreen screen extra-commands]
   ;; Each built-in carries its direct keybind as a dim right-aligned `:hint`
   ;; (opencode-style), so the palette doubles as a live keymap reference;
   ;; palette-only verbs and slash roots have no chord, so no hint.
   (let [with-hints (mapv (fn [c]
                            (assoc c :hint (keymap/label-for (:id c))))
                          palette-commands)]
     (searchable-select! screen "Command Palette" (vec (concat with-hints extra-commands))))))
(defn model-picker!
  "Searchable per-session model picker — TUI parity with the web footer
   chooser. Lists every configured model as a row (`<provider> / <model>`,
   the active one marked `● current`) plus a top `★ router default` row
   that CLEARS the per-session override. `current` is the session's stored
   model preference (`{:provider <str|kw> :model <str>}`) or nil; it marks
   the active row exactly like the web picker. Returns the chosen item map
   — `{:reset? true}` for the router-default row, else `{:provider <str>
   :model <str>}` — or nil on Esc."
  [^TerminalScreen screen current]
  (let [providers
        (try (vis/configured-providers)
             (catch Throwable t
               (tel/log! :warn ["dialogs: configured-providers failed" (ex-message t)])
               nil))

        cur-provider
        (some-> (:provider current)
                name)

        cur-model
        (:model current)

        model-rows
        (for [p
              providers

              :let [pid
                    (name (:id p))

                    plabel
                    (vis/display-label (:id p))]
              m
              (:models p)

              :let [nm
                    (vis/model-name m)]
              :when nm]

          {:label (str plabel " / " nm)
           :hint (when (and (= nm cur-model) (= pid cur-provider)) "● current")
           :provider pid
           :model nm})

        items
        (vec (cons {:label "★ router default"
                    :hint (when (and (nil? cur-provider) (nil? cur-model)) "● current")
                    :reset? true}
                   model-rows))]

    (list-dialog! screen
                  "Session model"
                  items
                  {:filter? true
                   :placeholder "Type to filter models…"
                   :enter-label "choose"
                   :height :content})))
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
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/ArrowUp (do (swap! scroll #(max 0 (dec %))) (recur))
              KeyType/ArrowDown (do (swap! scroll #(min max-scroll (inc %))) (recur))
              KeyType/Character (recur)
              (recur))))))))
;;; ── Markdown viewer dialog ──────────────────────────────────────────────────
(defn- md-run-paint!
  "Paint one styled IR run at column `x`; returns the next x. Style →
   dialog-palette mapping: headings title-accent bold, code/links/list
   markers hint-key accent, dim/quote hint, **bold**/_italic_ as SGR."
  [g x row {:keys [text style]}]
  (let [style
        (or style #{})

        head?
        (contains? style :heading)

        code?
        (or (contains? style :code) (contains? style :link))

        ;; Headings paint dialog-fg + BOLD, NOT dialog-title-fg: the
        ;; title token is white in BOTH palettes (it sits on the title
        ;; bar), so on the light dialog body it was invisible.
        fg
        (cond code? t/dialog-hint-key
              (contains? style :marker) t/dialog-hint-key
              (or (contains? style :dim) (contains? style :quote)) t/dialog-hint
              :else t/dialog-fg)

        bold?
        (or head? (contains? style :bold))

        italic?
        (contains? style :italic)]

    (p/set-colors! g fg t/dialog-bg)
    (cond (and bold? italic?) (p/styled g [p/BOLD p/ITALIC] (p/put-str! g x row text))
          bold? (p/styled g [p/BOLD] (p/put-str! g x row text))
          italic? (p/styled g [p/ITALIC] (p/put-str! g x row text))
          :else (p/put-str! g x row text))
    (+ x (p/display-width text))))
(defn markdown-viewer-dialog!
  "Scrollable read-only MARKDOWN viewer: `md` is lifted to canonical IR
   (`vis/markdown->ir`) and painted styled — headings, bold, code
   accents, tables — through the SAME IR walker the chat uses
   (`render-ir/ir->lines`). The rich twin of `text-viewer-dialog!`.
   Returns nil on Esc. Supports keyboard scrolling."
  [^TerminalScreen screen title md]
  (let [scroll
        (atom 0)

        ir
        (try
          (vis/markdown->ir (str md))
          (catch Throwable t (tel/log! :warn ["dialogs: markdown->ir failed" (ex-message t)]) nil))]

    (if (nil? ir)
      (text-viewer-dialog! screen title (str md))
      (loop []

        (let [size
              (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

              cols
              (.getColumns size)

              rows
              (.getRows size)

              g
              (.newTextGraphics screen)

              bounds
              (draw-dialog-chrome! g cols rows title (max 12 (- rows 8)))

              {:keys [left inner-w]}
              bounds

              {:keys [content-top content-h hint-row]}
              (dialog-layout bounds)

              scroll-col
              (+ left inner-w)

              text-w
              (max 1 (- inner-w 3))

              lines
              (try (ir-tui/ir->lines ir text-w)
                   (catch Throwable t
                     (tel/log! :warn ["dialogs: ir->lines failed" (ex-message t)])
                     []))

              total
              (count lines)

              max-scroll
              (max 0 (- total content-h))

              _
              (swap! scroll #(clamp % 0 max-scroll))

              visible
              (subvec (vec lines) @scroll (min total (+ @scroll content-h)))]

          (doseq [[i line] (map-indexed vector visible)]
            (let [row (+ content-top i)]
              (when (< row (+ content-top content-h))
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g (inc left) row inner-w 1)
                (reduce (fn [x run]
                          (md-run-paint! g x row run))
                        (+ left 2)
                        (:runs line)))))
          (doseq [row (range (+ content-top (count visible)) (+ content-top content-h))]
            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/fill-rect! g (inc left) row inner-w 1))
          (when (> total content-h)
            (let [track-h
                  content-h

                  ratio
                  (/ (double content-h) total)

                  thumb-h
                  (max 1 (int (* track-h ratio)))

                  den
                  (max 1 max-scroll)

                  thumb-pos
                  (int (* (- track-h thumb-h) (/ (double @scroll) den)))]

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
              (condp = (key-type key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! scroll #(max 0 (dec %))) (recur))
                KeyType/ArrowDown (do (swap! scroll #(min max-scroll (inc %))) (recur))
                KeyType/Character (recur)
                (recur)))))))))
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
  (let [items
        (vec messages)

        selected
        (atom 0)

        scroll
        (atom 0)

        checked
        (atom #{})

        ch
        (count items)]

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
            (let [ktype (key-type key)]
              (condp = ktype
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total))))
                                    (recur status))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                                      (recur status))
                KeyType/Character
                (let [c (lower-key-character key)]
                  (cond (= c \space) (do (when (pos? total)
                                           (swap! checked (fn [s]
                                                            (if (contains? s @selected)
                                                              (disj s @selected)
                                                              (conj s @selected)))))
                                         (recur status))
                        (= c \a)
                        (do (swap! checked (fn [s]
                                             (if (= (count s) total) #{} (set (range total)))))
                            (recur status))
                        :else (recur status)))
                KeyType/Enter (let [payload (format-selected-messages items @checked)]
                                (if (seq payload)
                                  (do (input/clipboard-copy! payload) true)
                                  (recur "No messages selected")))
                (recur status)))))))))
