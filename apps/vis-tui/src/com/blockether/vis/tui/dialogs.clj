(ns com.blockether.vis.tui.dialogs
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.markdown-layout :as layout]
            [com.blockether.vis.tui.table :as table]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.transient :as tr]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.mcp-model :as mcp-model]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [taoensso.telemere :as tel])
  (:import
    [com.googlecode.lanterna Symbols TerminalPosition TerminalRectangle TerminalSize TextCharacter]
    [com.googlecode.lanterna.graphics TextGraphics]
    [com.googlecode.lanterna.gui2 Direction HitRegionMap ScrollBar ScrollBar$DragResult]
    [com.googlecode.lanterna.input InputCoalescer KeyStroke KeyType MouseAction MouseActionType]
    [com.googlecode.lanterna.screen TerminalScreen]
    [java.text Normalizer Normalizer$Form SimpleDateFormat]
    [java.util Locale TimeZone]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Shared dialog chrome & components ───────────────────────────────────────

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
  ^long [^long cols]
  (let [terminal-w
        (max 40 (- cols 4))

        min-w
        (min (long t/dialog-min-width) terminal-w)

        box-w
        (-> (long (* cols (double t/dialog-width-ratio)))
            (max min-w)
            (min (long t/dialog-max-width))
            (min terminal-w))]

    (max 1 (- box-w (long t/dialog-chrome-w)))))

(defn default-content-height
  "Shared content height every dialog uses, derived from `rows`.
   Clamped to a common modal footprint so dialogs keep equal height."
  ^long [^long rows]
  (let [terminal-h
        (max 8 (- rows 4))

        min-h
        (min (long t/dialog-min-height) terminal-h)

        box-h
        (-> (long (* rows (double t/dialog-height-ratio)))
            (max min-h)
            (min (long t/dialog-max-height))
            (min terminal-h))]

    (max 1 (- box-h (long t/dialog-chrome-h)))))

(defn- clear-screen-buffer!
  "Fill the resized back buffer without flushing an intermediate blank frame."
  [^TerminalScreen screen ^TerminalSize size]
  (let [cols
        (.getColumns size)

        rows
        (.getRows size)

        g
        (frame/surface-graphics screen cols rows)]

    (p/set-bg! g t/terminal-bg)
    (p/fill-rect! g 0 0 cols rows)
    size))

(def ^:private ^:dynamic *modal-backgrounds* [])

(defn- invalidate-modal-backgrounds!
  "Discard every active backdrop for this screen after a terminal resize."
  [screen]
  (doseq [background
          *modal-backgrounds*

          :when (identical? screen (:screen background))]

    (swap! (:state background) assoc :restore! nil :footprint nil :cursor nil)))

(defn- modal-size!
  "Apply a pending resize and clear the old back buffer before the next paint."
  ^TerminalSize [^TerminalScreen screen]
  (if-let [size (frame/resize! screen)]
    (do (clear-screen-buffer! screen size) (invalidate-modal-backgrounds! screen) size)
    (.getTerminalSize screen)))

(defn frame-restorer
  "Snapshot the screen's back buffer and return a function that restores its cells.

   Call it with no arguments for the whole frame, `[from to]` for a row band,
   or `[left top right bottom]` for a rectangle. Bounds are inclusive physical
   screen coordinates, clipped to the snapshot. Characters, colors and styles
   are preserved. This keeps the host readable when a smaller transient band
   or dialog replaces a larger one without repainting the host.

   Returns nil when there is no screen: unit tests redefine every dialog away."
  [^TerminalScreen screen]
  (when screen
    (let [size
          (modal-size! screen)

          cols
          (.getColumns size)

          rows
          (.getRows size)

          snapshot
          (mapv (fn [row]
                  (mapv (fn [col]
                          (.getBackCharacter screen (int col) (int row)))
                        (range cols)))
                (range rows))]

      (fn restore! ([] (restore! 0 0 (dec (long cols)) (dec (long rows)))) ([from to] (restore! 0
                                                                                        from
                                                                                        (dec
                                                                                          (long
                                                                                            cols))
                                                                                        to))
        ([left top right bottom] (doseq [row
                                         (range (max 0 (long top))
                                                (min (long rows) (inc (long bottom))))

                                         col
                                         (range (max 0 (long left))
                                                (min (long cols) (inc (long right))))]

                                   (.setCharacter screen
                                                  (int col)
                                                  (int row)
                                                  ^TextCharacter (get-in snapshot [row col]))))))))

(defn- capture-modal-background
  "Capture the host's cells and cursor before a modal starts painting."
  [^TerminalScreen screen]
  (when screen
    (let [restore! (frame-restorer screen)]
      {:screen screen
       :state (atom {:restore! restore! :footprint nil :cursor (.getCursorPosition screen)})})))

(defn- prepare-modal-background!
  "Restore the previous footprint and record the next physical paint rectangle."
  [left top right bottom]
  (when-let [background (peek *modal-backgrounds*)]
    (let [state (:state background)
          {:keys [restore! footprint]} @state
          restore! (or restore! (frame-restorer (:screen background)))]

      (when footprint (apply restore! footprint))
      (swap! state assoc :restore! restore! :footprint [left top right bottom]))))

(defn- finish-modal-background!
  "Restore the modal's footprint and cursor without flushing an extra frame."
  [{:keys [screen state]}]
  (when screen
    (let [{:keys [restore! footprint cursor]} @state]
      (when (and restore! footprint) (apply restore! footprint))
      (.setCursorPosition ^TerminalScreen screen cursor))))

(defmacro ^:private with-modal-background
  "Keep each modal's backdrop separate, including nested dialogs and failures."
  [screen & body]
  `(let [background# (capture-modal-background ~screen)]
     (binding [*modal-backgrounds* (cond-> *modal-backgrounds*
                                     background#
                                     (conj background#))]
       (try ~@body (finally (finish-modal-background! background#))))))

(defn clear-screen!
  "Fill the entire screen with terminal background. Call before sub-dialogs
   to cleanly replace the current dialog (wizard step pattern)."
  [^TerminalScreen screen]
  (let [size
        (modal-size! screen)

        cols
        (.getColumns size)

        rows
        (.getRows size)]

    (prepare-modal-background! (frame/screen-column 0)
                               0
                               (frame/screen-column (dec cols))
                               (dec rows))
    (clear-screen-buffer! screen size)
    (frame/refresh! screen)))

(defn ellipsize
  "Right-truncate `s` to `max-w` columns with a trailing `…`.
   Thin delegate over the canonical `p/ellipsize` (lanterna-backed)."
  [s max-w]
  (p/ellipsize s max-w))

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
  ^long [^long rows requested]
  (if (nil? requested)
    (default-content-height rows)
    (let [terminal-box
          (max 8 (- rows 4))

          max-h
          (max 1 (- (min (long t/dialog-max-height) terminal-box) (long t/dialog-chrome-h)))

          floor
          (min (long min-adaptive-content-h) max-h)]

      (p/clamp (long requested) floor max-h))))

(defn dialog-layout
  "Compute content area layout. When `content-count` is provided and smaller than
   the available height, content is vertically centered within the frame.
   Layout: border -> title bar -> top separator -> CONTENT -> bottom separator -> hint -> border."
  ([bounds] (dialog-layout bounds nil))
  ([{:keys [top bottom]} content-count]
   (let [top
         (long top)

         bottom
         (long bottom)

         raw-top
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
         (long (if (and content-count (< (long content-count) full-h))
                 (quot (- full-h (long content-count)) 2)
                 0))

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
  ^long [^long idx ^long current-start ^long visible-count ^long total-count]
  (let [last-start
        (max 0 (- total-count visible-count))

        start
        (p/clamp current-start 0 last-start)]

    (cond (< idx start) idx
          (>= idx (+ start visible-count)) (max 0 (- idx (dec visible-count)))
          :else start)))

(defn page-selected-index
  "Move a selection by painted rows, skipping non-selectable headings."
  [display selected list-h direction selectable?]
  (let [indices
        (vec (keep-indexed (fn [idx row]
                             (when (selectable? row) idx))
                           display))

        current
        (long (or (nth indices selected nil) 0))

        target
        (+ current (* (long direction) (long list-h)))]

    (if (pos? (long direction))
      (or (first (keep-indexed (fn [idx visual]
                                 (when (>= (long visual) target) idx))
                               indices))
          (max 0 (dec (count indices))))
      (or (last (keep-indexed (fn [idx visual]
                                (when (<= (long visual) target) idx))
                              indices))
          0))))

(defn- key-type [key] (when (instance? KeyStroke key) (.getKeyType ^KeyStroke key)))

(defn- key-character [key] (when (instance? KeyStroke key) (.getCharacter ^KeyStroke key)))

(defn- lower-character [^Character c] (when c (Character/toLowerCase (.charValue c))))

(defn- lower-key-character [key] (lower-character (key-character key)))

(defn- iso-control-character?
  [^Character c]
  (boolean (and c (Character/isISOControl (.charValue c)))))

(def ^:private modal-input-coalescer (ThreadLocal/withInitial #(InputCoalescer.)))

(defn normalize-modal-key
  "Apply the app-wide C-g abort policy to input already decoded by Lanterna."
  [key]
  (input/normalize-abort-key key))

(defn modal-enter-key?
  [key]
  (let [key (normalize-modal-key key)]
    (and key (not (instance? MouseAction key)) (= KeyType/Enter (key-type key)))))

(defn modal-escape-key?
  [key]
  (let [key (normalize-modal-key key)]
    (and key (not (instance? MouseAction key)) (= KeyType/Escape (key-type key)))))

(def ^:private modal-close-target :modal-close)

(def ^:private modal-close-regions (ThreadLocal/withInitial #(HitRegionMap.)))

(defn modal-close-click?
  "True when Lanterna resolves `key` to the dialog close target."
  [key]
  (when (and (instance? MouseAction key)
             (= MouseActionType/CLICK_RELEASE (.getActionType ^MouseAction key)))
    (= modal-close-target
       (.lookup ^HitRegionMap (.get ^ThreadLocal modal-close-regions) ^MouseAction key))))

(defn- mouse-row-offset
  "Return the zero-based row under a primary click inside a list rectangle."
  [key left top width height]
  (when (and (instance? MouseAction key)
             (let [action (.getActionType ^MouseAction key)]
               (or (= action MouseActionType/CLICK_DOWN) (= action MouseActionType/CLICK_RELEASE))))
    (let [bounds
          (TerminalRectangle. (int left) (int top) (int width) (int height))

          relative
          (.relativePosition ^TerminalRectangle bounds
                             ^TerminalPosition (.getPosition ^MouseAction key))]

      (when relative (long (.getRow ^TerminalPosition relative))))))

(defn update-modal-close-hover!
  "Publish MOVE/DRAG hover through the modal's Lanterna hit map."
  [key]
  (when (instance? MouseAction key)
    (let [action (.getActionType ^MouseAction key)]
      (when (or (= action MouseActionType/MOVE) (= action MouseActionType/DRAG))
        (.updateHovered ^HitRegionMap (.get ^ThreadLocal modal-close-regions) ^MouseAction key)))))

(defn- poll-modal-key!
  "Return queued input, or one harmless wake key after applying a resize."
  ^KeyStroke [^TerminalScreen screen]
  (or (.pollInput screen)
      (when-let [size (frame/resize! screen)]
        (clear-screen-buffer! screen size)
        (invalidate-modal-backgrounds! screen)
        (KeyStroke. KeyType/Unknown))))

(defn- await-modal-key!
  ^KeyStroke [^TerminalScreen screen]
  (loop []

    (if-let [key (poll-modal-key! screen)]
      key
      (do (Thread/sleep 16) (recur)))))

(defn read-modal-input!
  "Wait for one canonical modal event through Lanterna's stateful input coalescer.
   Canceled wheel bursts are ignored, not returned as end-of-input.
   Resize wakes remain `KeyType/Unknown`; MOVE/DRAG refreshes close-button hover."
  [^TerminalScreen screen]
  (let [^InputCoalescer coalescer (.get ^ThreadLocal modal-input-coalescer)]
    (loop []

      (if-let [key (.next coalescer
                          (reify
                            java.util.function.Supplier
                              (get [_] (normalize-modal-key (await-modal-key! screen))))
                          (reify
                            java.util.function.Supplier
                              (get [_]
                                (some-> (.pollInput screen)
                                        normalize-modal-key))))]
        (do (update-modal-close-hover! key)
            {:key (if (modal-close-click? key) (KeyStroke. KeyType/Escape) key)})
        (recur)))))

(defn modal-input-pending?
  "True when another modal input is already queued. Lanterna retains the peeked event
   so debounce checks never consume it."
  [^TerminalScreen screen]
  (.inputPending ^InputCoalescer (.get ^ThreadLocal modal-input-coalescer)
                 (reify
                   java.util.function.Supplier
                     (get [_] (poll-modal-key! screen)))))

(defn read-modal-key!
  "Like `Screen/readInput`, with pointer bursts canonicalized by Lanterna."
  ^KeyStroke [^TerminalScreen screen]
  (:key (read-modal-input! screen)))

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
              (= KeyType/PasteEnd (.getKeyType ^KeyStroke k)) (.toString sb)
              :else (do (when-let [text (.getText ^KeyStroke k)]
                          (.append sb ^String text))
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
        (let [w (+ (long (seg-w (nth pairs i))) (long (if (pos? i) sep-w 0)))]
          (if (> (+ (long used) w) (long text-w))
            (subvec pairs 0 i)
            (recur (inc i) (+ (long used) w))))))))

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
        (max 0 (- (long inner-w) 2))

        text-x
        (+ (long left) 2)

        sep
        "  \u00b7  "

        sep-w
        (p/display-width sep)]

    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/fill-rect! g (inc (long left)) row inner-w 1)
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
            (+ (long (reduce + (map seg-w pairs))) (long (* sep-w (max 0 (dec n)))))

            start
            (+ (long text-x) (max 0 (quot (- (long text-w) (long total)) 2)))]

        (loop [i
               0

               col
               start]

          (when (< i n)
            (let [[k a]
                  (nth pairs i)

                  next-col
                  (+ (long col) (long (seg-w (nth pairs i))))]

              ;; Key part - bold, stronger color
              (p/set-fg! g t/dialog-hint-key)
              (p/styled g [p/BOLD] (p/put-str! g col row k))
              ;; Action part - dim hint color, italic
              (p/set-fg! g t/dialog-hint)
              (p/styled g
                        [p/ITALIC]
                        (p/put-str! g (+ (long col) (p/display-width k)) row (str " " a)))
              ;; Separator between pairs
              (when (< i (dec n)) (p/set-fg! g t/dialog-hint) (p/put-str! g next-col row sep))
              (recur (inc i) (+ (long next-col) sep-w))))))
      ;; Vec of strings - centered, dim italic, dot-joined, clipped to fit.
      (vector? hint) (let [joined
                           (ellipsize (apply str (interpose sep hint)) text-w)

                           start
                           (+ (long text-x)
                              (max 0 (quot (- (long text-w) (p/display-width joined)) 2)))]

                       (p/set-fg! g t/dialog-hint)
                       (p/styled g [p/ITALIC] (p/put-str! g start row joined))))))

(defn transient-host
  "The standard modal HOST for `tr/run!` — the one adapter between a Lanterna
   `screen` and the host-agnostic transient component. It paints through `g`,
   flushes with the modal cursor hidden, borrows this namespace's hint bar, and
   normalizes one modal keystroke into what the component understands: `:esc`,
   a Character, or nil for \"nothing actionable, just repaint\".

   Any surface holding a screen and a `TextGraphics` embeds a transient with
   this — the settings dialog, the provider dialog, `transient-dialog!`."
  [^TerminalScreen screen g]
  {:g g
   :hint-bar! draw-hint-bar!
   :refresh! (fn []
               (.setCursorPosition screen nil)
               (frame/refresh! screen))
   :read-key!
   (fn []
     (let [key (read-modal-key! screen)]
       (condp = (key-type key) KeyType/Escape :esc KeyType/Character (key-character key) nil)))})

(defn hint-bar-width
  "Natural rendered width (chars) of a `draw-hint-bar!` hint — a plain string,
   a vec of strings, or a vec of `[key action]` pairs — using the SAME segment
   and separator math the hint bar paints with. Lets a dialog size its box to
   the footer instead of a fixed terminal ratio."
  [hint]
  (let [sep-w (p/display-width "  \u00b7  ")]
    (cond (string? hint) (p/display-width hint)
          (and (vector? hint) (seq hint) (vector? (first hint)))
          (+ (long (reduce +
                           (map (fn [[k a]]
                                  (+ (p/display-width k) 1 (p/display-width a)))
                                hint)))
             (* sep-w (long (max 0 (dec (count hint))))))
          (vector? hint) (+ (long (reduce + (map p/display-width hint)))
                            (* sep-w (long (max 0 (dec (count hint))))))
          :else 0)))

(defn footer-content-width
  "Content width for an action-footer dialog: sized so the box is EXACTLY the
   footer's natural width plus two columns of padding on each side, never
   narrower than `min-content` (the widest content line) nor wider than the
   terminal. The `+2` supplies the extra pad beyond the single-column gutter
   `draw-dialog-chrome!` already reserves inside the border, so a footer of
   width W yields 2 blank columns between the frame and the hints on each side."
  (^long [cols hint] (footer-content-width cols hint 0))
  (^long [^long cols hint ^long min-content]
   (-> (+ (long (hint-bar-width hint)) 2)
       (max min-content)
       (min (max 1 (- cols 8))))))

(defn- draw-list-item!
  ;; Selected rows use bold, reversed colors across the label, hints and padding.
  ([g left row inner-w selected? label] (draw-list-item! g left row inner-w selected? label nil))
  ([g left row inner-w selected? label hint]
   ;; `hint` (optional) is a dim, right-aligned chip — e.g. a command's keybind
   ;; — drawn opposite the label (opencode's justify-between rows). The label is
   ;; truncated so it never collides with the hint.
   (let [hint
         (some-> hint
                 str
                 not-empty)

         hint-w
         (if hint (+ 2 (p/display-width hint)) 0)

         draw-text
         (ellipsize label (max 0 (- (long inner-w) 2 (long hint-w))))]

     (p/set-colors! g t/dialog-fg t/dialog-bg)
     (p/styled g
               (p/selection-styles selected?)
               (p/fill-rect! g (inc (long left)) row inner-w 1)
               (p/put-str! g (inc (long left)) row draw-text)
               (when hint
                 (p/set-colors! g t/dialog-hint t/dialog-bg)
                 (p/put-str! g (- (+ (long left) (long inner-w)) (p/display-width hint)) row hint)
                 (p/set-colors! g t/dialog-fg t/dialog-bg))))))

(defn draw-selectable-row!
  "Paint a list row with bold, reversed colors while selected.
   Checkbox rows share this painter; their status remains independent of focus."
  [g left row inner-w selected? text]
  (draw-list-item! g left row inner-w selected? text))

(defn choice-mark
  "The status glyph a choice row wears in front of its label. An EXCLUSIVE choice
   takes the shared ●/○ pair the settings rows and the footer already speak — pick
   one and the other drops; an INCLUSIVE one takes the `[✓]`/`[ ]` box — pick as
   many as apply. One place, so \"choose one\" and \"choose any\" can never end up
   looking alike."
  [exclusive? checked?]
  (if exclusive?
    (str (if checked? p/STATUS_ON p/STATUS_OFF) " ")
    (str "[" (if checked? "✓" " ") "] ")))

(defn draw-checkbox-item!
  "Paint a checkbox and label with row selection styling.
   The checkbox shows whether the option is on; bold, reversed colors show focus."
  [g left row inner-w selected? checked? label]
  (draw-selectable-row! g left row inner-w selected? (str (choice-mark false checked?) label)))

(def ^:private field-pad
  "Columns of breathing room inside a form field's surface, one on each side —
   the same inner padding the find bar's query field carries. A border cannot
   give it, and text jammed against a coloured field edge reads as a bug."
  1)

(defn field-content-w
  "Columns a form field's TEXT gets on an `inner-w`-wide dialog row: the focus
   ring and the field's own padding come off the top. Public because paint and
   cursor placement have to measure the very same field."
  ^long [inner-w]
  (max 1 (- (long inner-w) 2 1 (* 2 (long field-pad)))))

(defn- draw-row-surface!
  "The shared geometry of EVERY focusable form row, typed or toggled. A form has
   ONE text column: the label, the prose, an option, a checkbox and an input box
   all land on it, and focus costs the text no indent — the accent ring `▎` a
   focused row wears lives in the GUTTER beside that column.

   The row owns the frame's INNER columns and nothing else. `left` is the frame's
   own border column, exactly as every other painter here reads it, so the gutter
   is the first column INSIDE it and the text column the one after. A ring painted
   ON the border column erased the frame's rail on precisely the row the keyboard
   was in: the focused field looked like it had escaped the box, and in a transient
   band it read as a rail hanging outside the border. A gutter carved out of the
   TEXT column instead indented every toggle away from its label.

   The dialog's own paper is cleared across the inner columns first — anything
   past the row's right edge belongs to the body — then `bg` paints the row's
   surface. A typed row's surface OPENS ON the gutter, so the ring is the field's
   own left edge and `pad` is the space between that edge and the text; a toggle
   paints no surface, and its gutter stays empty until it is focused. A focused
   row takes the ink (`box-fg`, bold) while an unfocused one recedes to
   `dialog-hint`. Returns the column the text started at.

   Geometry is shared so a form's rows line up whatever they are; the SURFACE is
   the caller's, because only a row you can type into is an input."
  [g left row inner-w focused? bg pad content]
  (let [content-w
        (field-content-w inner-w)

        ;; the gutter is the first column inside the frame; the text column is the
        ;; next one, and every row of the form shares it.
        ring-col
        (inc (long left))

        text-left
        (+ (long left) 2)

        ;; a surface opens ON the gutter, so its own padding IS the ring's column
        field-left
        (- text-left (long pad))

        shown
        (ellipsize (str content) content-w)]

    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g ring-col row inner-w 1)
    (p/set-colors! g (if focused? t/box-fg t/dialog-hint) bg)
    (p/fill-rect! g field-left row (+ content-w 1 (* 2 (long pad))) 1)
    (if focused?
      (p/styled g [p/BOLD] (p/put-str! g text-left row shown))
      (p/put-str! g text-left row shown))
    (when focused?
      ;; the ring rides the row's OWN paper — on a typed row that IS its surface
      (p/set-colors! g t/header-active-tab-accent bg)
      (p/put-str! g ring-col row "▎"))
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    text-left))

(defn draw-field-row!
  "TYPED row — the painter for a form row text is entered into: a line, a
   password, an OTP's boxes. An input is drawn as an INPUT: `input-field-bg`,
   padded a space each side, the very control `components/find-bar!` paints its
   query box with — so an empty field is still visibly a field and every place
   the TUI takes typing is the same object. It starts at the dialog's own inner
   edge, directly under its label.

   Focus is the other half, and it is said three ways at once: the focused field
   wears the accent ring `▎` down its left edge, keeps the full field surface, and
   takes the ink (`box-fg`, bold). A field the keyboard is NOT in loses the ring,
   recedes to `theme/field-resting-bg` and dims to `dialog-hint`. That contrast IS
   the cursor in a form — there is no `•` gutter, because a marker in front of
   every row says the same thing about all of them.

   A TOGGLE is not typed into and does not wear this paper: see
   [[draw-toggle-row!]].

   `content` is the field's already-rendered text (`ada@example.com`,
   `[1] [2] [ ]`). Returns the column its first cell landed on, so a caller that
   owns the terminal cursor can place it."
  [g left row inner-w focused? content]
  (draw-row-surface! g
                     left
                     row
                     inner-w
                     focused?
                     (if focused? t/input-field-bg (t/field-resting-bg))
                     field-pad
                     content))

(defn draw-toggle-row!
  "TOGGLED row — an option of a `:select`, a checkbox, a slider's track. Exactly
   the geometry of [[draw-field-row!]], so a form's rows line up whatever they
   are, but painted on the dialog's OWN paper: nothing is typed here, so there is
   no input surface to fill. Paper that says \"type here\" under a row that cannot
   take a character is a lie about what the keyboard will do.

   Focus is then the accent ring `▎` and the bold ink alone, and the status glyph
   ([[choice-mark]]) says what the toggle currently IS.

   It also carries NO field padding: the pad keeps typed text off a coloured
   field edge, and there is no edge here — on the dialog's own paper it is only a
   margin. A checkbox IS its own label, so that margin left the one row that must
   line up with the other labels indented away from them; the ring cell is the
   whole gutter a focusable row gets."
  [g left row inner-w focused? content]
  (draw-row-surface! g left row inner-w focused? t/dialog-bg 0 content))

(defn draw-input-item!
  "A form's TYPED row: `draw-field-row!` plus what typing needs — the horizontal
   scroll that keeps the cursor inside the field and the dim `placeholder` an
   empty field shows. Returns the `TerminalPosition` the caller parks the
   terminal cursor at."
  [g left row inner-w focused? text cursor placeholder]
  (let [content-w
        (field-content-w inner-w)

        text
        (str text)

        cursor
        (max 0 (min (long cursor) (count text)))

        h-off
        (max 0 (- cursor (dec content-w)))

        visible
        (subs text h-off (min (count text) (+ h-off content-w)))

        blank?
        (zero? (count text))

        text-left
        (draw-field-row! g left row inner-w focused? (if blank? "" visible))]

    (when (and placeholder blank?)
      ;; The hint rides the field's OWN surface — a resting field must not light
      ;; up just because it is empty.
      (p/set-colors! g t/dialog-hint (if focused? t/input-field-bg (t/field-resting-bg)))
      (p/put-str! g text-left row (ellipsize (str placeholder) content-w))
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/cursor-pos (+ (long text-left) (- cursor h-off)) row)))

(defn draw-text-input-field!
  "Borderless `› text` input row with an optional dim `placeholder`. Returns the
   `TerminalPosition` the caller should park the terminal cursor at."
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
         (+ (long left) 1)

         text-left
         (+ (long field-left) (long pw))

         text-w
         (max 1 (- (long inner-w) 2 (long pw) 1))

         h-off
         (max 0 (- (long cursor) (dec (long text-w))))

         visible
         (subs text h-off (min (count text) (+ (long h-off) (long text-w))))]

     (p/set-colors! g t/dialog-fg t/dialog-bg)
     (p/fill-rect! g field-left row (max 1 (- (long inner-w) 2)) 1)
     (p/set-colors! g t/dialog-hint t/dialog-bg)
     (p/put-str! g field-left row prompt)
     (if (and placeholder (zero? (count text)))
       (do (p/set-colors! g t/dialog-hint t/dialog-bg)
           (p/put-str! g text-left row (ellipsize (str placeholder) text-w)))
       (do (p/set-colors! g t/dialog-fg t/dialog-bg) (p/put-str! g text-left row visible)))
     (p/cursor-pos (+ (long text-left) (- (long cursor) (long h-off))) row))))

(defn draw-dialog-close-button!
  "Paint the dialog close button and publish its target through Lanterna's hit map."
  [g box-right title-row]
  (let [label
        " ✕ "

        x1
        (- (long box-right) 1)

        x0
        (- (long x1) (dec (p/display-width label)))

        ^HitRegionMap regions
        (.get ^ThreadLocal modal-close-regions)

        bounds
        (TerminalRectangle. (int (frame/screen-column x0)) (int title-row) (int (inc (- x1 x0))) 1)

        hovered?
        (= modal-close-target (.hovered regions))]

    (.beginFrame regions)
    (.register regions bounds modal-close-target)
    (.commitFrame regions)
    (p/clear-styles! g)
    (p/set-colors! g
                   (if hovered? t/header-active-tab-fg t/dialog-title-bg)
                   (if hovered? t/close-button-hover-fg t/dialog-title-fg))
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g x0 title-row label)
    (p/clear-styles! g)))

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
   (let [cols
         (long cols)

         rows
         (long rows)

         content-w
         (long content-w)

         content-h
         (long content-h)

         [box-w box-h]
         (render/golden-dialog-size cols rows content-w content-h)

         box-w
         (long box-w)

         box-h
         (long box-h)

         box-left
         (max 3 (- (quot (- cols box-w) 2) 3))

         box-top
         (max 2 (- (quot (- rows box-h) 2) 2))

         box-right
         (+ box-left box-w -1)

         box-bottom
         (+ box-top box-h -1)

         inner-w
         (- box-w 2)]

     (prepare-modal-background! (frame/screen-column box-left)
                                box-top
                                (frame/screen-column (min (dec cols) (+ box-right 2)))
                                (min (dec rows) (inc box-bottom)))
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
     (p/set-colors! g t/dialog-border t/dialog-bg)
     (p/draw-box! g box-left box-top box-w box-h)
     ;; Title bar - full-width accent stripe with centered title
     (let [title-row
           (inc box-top)

           title-text
           (ellipsize (or title "") (max 0 (- inner-w 2)))

           tx
           (+ box-left 1 (quot (- inner-w (p/display-width title-text)) 2))]

       ;; Accent bar background
       (p/set-bg! g t/dialog-title-bg)
       (p/fill-rect! g (inc box-left) title-row inner-w 1)
       ;; Title text - BOLD, matching the Blockether 700-weight header
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

(defn draw-flat-dialog-chrome!
  "Flat variant of `draw-dialog-chrome!`: no drop shadow, no accent title
   stripe, no separators - one thin-bordered rect on the dialog background
   with the title inline on the top border. Same default footprint and the
   same bounds map as the boxed chrome, so `dialog-layout` works unchanged."
  [g ^long cols ^long rows title]
  (let [content-w
        (default-content-width cols)

        content-h
        (default-content-height rows)

        [box-w box-h]
        (render/golden-dialog-size cols rows content-w content-h)

        box-w
        (long box-w)

        box-h
        (long box-h)

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

    (prepare-modal-background! (frame/screen-column box-left)
                               box-top
                               (frame/screen-column (min (dec cols) box-right))
                               (min (dec rows) box-bottom))
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
  [^long cols ^long rows ^long content-w ^long content-h]
  (let [[box-w box-h]
        (render/golden-dialog-size cols rows content-w content-h)

        box-w
        (long box-w)

        box-h
        (long box-h)

        box-left
        (max 3 (- (quot (- cols box-w) 2) 3))

        box-top
        (max 2 (- (quot (- rows box-h) 2) 2))]

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
     :read-key  optional (fn [screen] -> key|nil), for async loading wakeups

   run-modal! owns everything the old dialogs copy-pasted: terminal sizing, the
   `TextGraphics`, wheel/close/Esc normalization (via `read-modal-key!`), the
   cursor, refresh, and the recur loop. Shared chrome restores the previous
   footprint before each paint, including its border and shadow. Closing a
   modal restores the host back buffer and cursor without an extra flush.
   A terminal resize discards all active snapshots for that screen.

   A key handler returns the next state to continue, or `{::done v}` to close
   the modal with value `v` (nil on Esc/close). Because `:measure`, `:reconcile`,
   and `:on-key` are pure functions of data, a dialog's geometry and key logic
   can be unit-tested with no live terminal."
  [^TerminalScreen screen
   {:keys [init measure reconcile paint on-key read-key] :or {read-key read-modal-key!}}]
  (with-modal-background
    screen
    (loop [state (if (fn? init) (init) init)]
      (let [size (modal-size! screen)
            cols (.getColumns size)
            rows (.getRows size)
            geom (measure state cols rows)
            state (if reconcile (reconcile state geom) state)
            g (frame/surface-graphics screen cols rows)
            cursor (paint g state geom)]

        ;; nil cursor hides the hardware cursor; a text field returns its cell.
        (.setCursorPosition screen cursor)
        (frame/refresh! screen)
        (let [key (read-key screen)]
          (if (nil? key)
            (recur state)
            (let [r (on-key state key geom)]
              (if (and (map? r) (contains? r ::done)) (::done r) (recur r)))))))))

(defn- metric-count [n] (if (number? n) (str (long n)) "—"))

(defn- metric-percent [n] (if (number? n) (str n "%") "—"))

(defn- metric-tokens [^long n] (String/format Locale/US "%,d tokens" (object-array [n])))

(defn- metric-token-difference
  [health]
  (when-let [difference (get health "estimate_difference_tokens")]
    (let [signed? (not (zero? (long difference)))
          tokens (String/format Locale/US
                                (if signed? "%+,d tokens" "%,d tokens")
                                (object-array [(long difference)]))
          percent (get health "estimate_difference_percent")]

      (str/replace (str tokens
                        (when (number? percent)
                          (String/format Locale/US
                                         (if signed? " (%+.1f%%)" " (%.1f%%)")
                                         (object-array [(double percent)]))))
                   "-"
                   "−"))))

(defn- session-metric-rows
  [session {:keys [phase usage parts? roots?]}]
  (let [row
        (fn [text]
          {:text text})

        hint
        (fn [text]
          {:text text :tone :hint})

        head
        (fn [text]
          {:text text :tone :heading})

        stat
        (fn [label value]
          (assoc (row (str label "  " value))
            :label label
            :value value))

        nested
        (fn [rows]
          (map #(assoc % :indent 2) rows))

        health
        (get usage "health")

        input
        (get health "last_request_tokens")

        budget
        (get health "budget_tokens")

        reminder
        (get health "reminder_tokens")

        limit
        (get health "model_input_limit")

        percent
        (get health "budget_used_percent")

        budget?
        (some? percent)

        budget-state
        (get health "budget_state")

        tone
        (case budget-state
          ("input-limit" "over-budget")
          :error

          "fold-reminder"
          :warning

          :heading)

        pressure
        (case budget-state
          "input-limit"
          "Input limit reached"

          "over-budget"
          "Over budget"

          "fold-reminder"
          "Fold reminder"

          "within-budget"
          "Within budget"

          "Budget not reported")

        breakdown
        (get health "breakdown")

        estimate
        (get health "estimated_input_tokens")

        prepared?
        (= "prepared-request" (get health "counted_projection"))

        roots
        (get health "roots")

        estimated?
        (get usage "reusable_prefix_estimated")

        samples
        (get usage "prompt_cache_sample_count")]

    (vec
      (case phase
        :loading
        [(hint "Reading session metrics…")]

        :error
        [{:text "Session metrics unavailable. Close and reopen to retry." :tone :error}]

        :ready
        (if-not usage
          [(hint "No measured calls yet. Metrics appear after the first model response.")]
          (concat
            [(head "Session health")]
            (if-not health
              [(hint "Context measurement unavailable")
               (hint "Session totals below do not measure context size.")]
              (concat
                [{:text pressure :tone tone}
                 (hint (str (if (get health "stale") "Earlier measurement" "Last measured call")
                            " · #"
                            (get health "call")))
                 (stat "Context / working budget"
                       (str (metric-count input)
                            " / "
                            (if budget? (metric-count budget) "Not reported")
                            (when budget? (str "  " (metric-percent percent)))))]
                (when-let [ratio (get health "budget_used_ratio")]
                  [{:meter ratio :tone tone}])
                [(hint (if reminder
                         (str "Reminder at " (metric-count reminder))
                         "Reminder not reported"))
                 (hint
                   (cond (some? (get health "budget_remaining_tokens"))
                         (str (metric-count (get health "budget_remaining_tokens")) " budget left")
                         (some? (get health "budget_overage_tokens"))
                         (str (metric-count (get health "budget_overage_tokens")) " over budget")
                         :else "Working budget was not recorded"))
                 (stat "Model input limit" (metric-count limit)) (row "")]
                (if (some? estimate)
                  (concat [{:text (str (if parts? "▾" "▸") " Context breakdown [b]")
                            :tone :heading
                            :toggle :parts?}]
                          (nested
                            (concat [(hint (str (if prepared? "Prepared request" "Logical request")
                                                " · not measured usage"))]
                                    (when parts?
                                      (concat
                                        [(stat "Local estimate" (metric-tokens estimate))
                                         (stat "Provider-reported input" (metric-tokens input))
                                         (stat "Estimate − reported"
                                               (or (metric-token-difference health) "—")) (row "")]
                                        (mapcat (fn [part]
                                                  (cond-> [(stat (get part "label")
                                                                 (str "≈"
                                                                      (metric-count
                                                                        (get part "tokens"))))]
                                                    (get part "path")
                                                    (conj (hint (get part "path")))))
                                                breakdown))))))
                  [(hint "Prompt breakdown unavailable")])
                [(row "")]
                (if (some? roots)
                  (concat
                    [{:text (str (if roots? "▾" "▸") " Linked filesystems [f]")
                      :tone :heading
                      :toggle :roots?}]
                    (nested
                      (concat
                        [(hint (str (metric-count (get health "root_count"))
                                    " available · "
                                    (metric-count (get health "estimated_root_count"))
                                    " with guidance estimates"))]
                        (when roots?
                          (concat
                            (mapcat (fn [item]
                                      (let [guidance (get item "guidance")]
                                        [(row "") (row (get item "path"))
                                         (hint (case (get guidance "status")
                                                 "available"
                                                 (str (get guidance "path")
                                                      " · ≈"
                                                      (metric-count (get guidance "tokens"))
                                                      " tokens on disk")

                                                 "missing"
                                                 "No AGENTS.md or CLAUDE.md"

                                                 "error"
                                                 "Could not read guidance · check file access"

                                                 "Guidance estimate unavailable"))]))
                                    roots)
                            [(hint
                               "Disk estimates do not add to context usage or imply that the agent loaded the file. Main workspace guidance is listed above.")])))))
                  [(hint "Linked filesystem details unavailable")])))
            [(row "") (head "Session totals")
             (hint "Across all calls, including repeated context.")]
            (map (fn [[label field]]
                   (stat label (metric-count (get usage field))))
                 [["Total input" "input_tokens"] ["Total output" "output_tokens"]])
            [(stat "Cost"
                   (if-let [cost (get usage "cost_usd")]
                     (String/format Locale/US "$%.4f" (object-array [(double cost)]))
                     "—"))]
            (map (fn [[label field]]
                   (stat label (metric-count (get usage field))))
                 [["Folds" "fold_count"] ["Turns" "turn_count"] ["Calls" "iteration_count"]
                  ["Tools" "tool_call_count"]])
            [(row "") (head "Prompt cache")
             (stat "Cached input" (metric-percent (get usage "cache_read_share_percent")))
             (hint "Share of all input served from provider cache")
             (stat "Reuse coverage"
                   (str (when (and estimated?
                                   (number? (get usage "reusable_prefix_coverage_percent")))
                          "≈")
                        (metric-percent (get usage "reusable_prefix_coverage_percent"))))
             (hint (str (if estimated? "Estimated share" "Share")
                        " of reusable prior input recovered from cache"
                        (when (number? samples)
                          (str " · "
                               (metric-count samples)
                               " of "
                               (metric-count (get usage "iteration_count"))
                               " calls")))) (row "")
             (stat "Model" (or (not-empty (get usage "model")) (:model session) "—"))
             (stat "Provider" (or (not-empty (get usage "provider")) (:provider session) "—"))
             (stat "Active"
                   (if-let [ms (get usage "duration_ms")]
                     (or (vis/format-duration ms) "0s")
                     "—")) (hint "Time spent inside turns")]))

        []))))

(defn session-metrics-component
  "Render the gateway session-usage metrics without deriving counts, budget states,
   percentages or deltas. Only number formatting and terminal geometry are local.
   Disclosures and keys are deterministic; unknown values never become zero."
  [session snapshot]
  {:init (merge {:scroll 0 :parts? false :roots? false} snapshot)
   :measure
   (fn [state cols rows]
     (let [content-w
           (max 1 (min 120 (- (long cols) 12)))

           width-bounds
           (dialog-bounds cols rows content-w 1)

           text-w
           (max 1 (- (long (:inner-w width-bounds)) 3))

           lines
           (vec
             (mapcat
               (fn [{:keys [text meter label value indent] :as row}]
                 (let [w (long (max 1 (- (long text-w) (long (or indent 0)))))]
                   (cond (some? meter)
                         [(assoc row
                            :text (str (apply str (repeat (long (* (double meter) w)) "━"))
                                       (apply str (repeat (- w (long (* (double meter) w))) "─"))))]
                         (and label (<= (+ (p/display-width label) 2 (p/display-width value)) w))
                         [(assoc row
                            :value-col (- w (p/display-width value))
                            :text (str label
                                       (apply str
                                         (repeat
                                           (- w (p/display-width label) (p/display-width value))
                                           " "))
                                       value))]
                         :else (map #(assoc row :text %)
                                    (if (str/blank? text) [""] (render/wrap-text text w))))))
               (session-metric-rows session state)))

           content-h
           (adaptive-content-height rows (count lines))

           bounds
           (dialog-bounds cols rows content-w content-h)

           layout
           (dialog-layout bounds)]

       (merge layout
              {:cols cols
               :rows rows
               :bounds bounds
               :content-w content-w
               :content-h-req content-h
               :text-w text-w
               :lines lines
               :max-scroll (max 0 (- (count lines) (long (:content-h layout))))})))
   :reconcile (fn [state {:keys [max-scroll]}]
                (update state :scroll #(p/clamp % 0 max-scroll)))
   :paint
   (fn [g {:keys [scroll]}
        {:keys [cols rows bounds content-w content-h-req content-top content-h hint-row text-w
                lines]}]
     (let [{:keys [left inner-w]} bounds]
       (draw-dialog-chrome! g cols rows "Session metrics · C-x u" content-w content-h-req)
       (doseq [[i {:keys [text tone toggle label value value-col indent]}]
               (map-indexed vector (take content-h (drop scroll lines)))]
         (let [y (+ (long content-top) (long i))
               x (+ (long left) 2 (long (or indent 0)))
               w (long (max 1 (- (long text-w) (long (or indent 0)))))]

           (when toggle (draw-toggle-row! g left y (dec (long inner-w)) false text))
           (p/set-colors! g
                          (case tone
                            :heading
                            t/dialog-hint-key

                            :hint
                            t/dialog-hint

                            :warning
                            t/footer-warning-fg

                            :error
                            t/footer-error-fg

                            t/dialog-fg)
                          t/dialog-bg)
           (when value-col (p/set-fg! g t/dialog-hint))
           (if (or label (= :heading tone))
             (p/styled g [p/BOLD] (p/put-str! g x y (ellipsize text w)))
             (p/put-str! g x y (ellipsize text w)))
           (when value-col
             (p/set-fg! g t/dialog-fg)
             (p/styled g [p/BOLD] (p/put-str! g (+ x (long value-col)) y value)))))
       (ScrollBar/draw g
                       Direction/VERTICAL
                       (TerminalPosition. (int (+ (long left) (long inner-w))) (int content-top))
                       (int content-h)
                       (count lines)
                       (int content-h)
                       (Integer/valueOf (int scroll))
                       t/dialog-border
                       t/dialog-bg
                       t/dialog-hint-key
                       t/dialog-bg)
       (draw-hint-bar! g left hint-row inner-w [["↑↓" "scroll"] ["b/f" "details"] ["Esc" "close"]])
       nil))
   :on-key (fn [state key {:keys [max-scroll content-h content-top bounds lines]}]
             (let [move
                   (fn [n]
                     (update state :scroll #(p/clamp (+ (long %) (long n)) 0 max-scroll)))

                   wheel
                   (ScrollBar/wheelStep ^KeyStroke key)

                   click
                   (when (and (instance? MouseAction key)
                              (= MouseActionType/CLICK_RELEASE (.getActionType ^MouseAction key))
                              (= 1 (.getButton ^MouseAction key)))
                     (mouse-row-offset key
                                       (inc (long (:left bounds)))
                                       content-top
                                       (:inner-w bounds)
                                       content-h))]

               (cond wheel (move wheel)
                     (some? click)
                     (if-let [toggle (:toggle
                                       (nth lines (+ (long (:scroll state)) (long click)) nil))]
                       (update state toggle not)
                       state)
                     (modal-escape-key? key) {::done nil}
                     :else (condp = (key-type key)
                             KeyType/ArrowUp (move -1)
                             KeyType/ArrowDown (move 1)
                             KeyType/PageUp (move (- (long content-h)))
                             KeyType/PageDown (move content-h)
                             KeyType/Home (assoc state :scroll 0)
                             KeyType/End (assoc state :scroll max-scroll)
                             KeyType/Character (case (key-character key)
                                                 \b
                                                 (update state :parts? not)

                                                 \f
                                                 (update state :roots? not)

                                                 state)
                             state))))})

(defn session-metrics-dialog!
  "Fetch usage on demand without blocking dismissal; closing cancels the read.
   The captured session id prevents workspace changes from retargeting the sheet."
  [screen sid session]
  (let [task
        (future (vis/session-usage sid))

        component
        (session-metrics-component session {:phase :loading})

        measure
        (:measure component)

        ready?
        (volatile! false)]

    (try (run-modal! screen
                     (assoc component
                       :measure (fn [state cols rows]
                                  (let [snapshot (deref task 0 {:phase :loading})]
                                    (vreset! ready? (not= :loading (:phase snapshot)))
                                    (measure (merge state snapshot) cols rows)))
                       :read-key (fn [screen]
                                   (if (or @ready? (modal-input-pending? screen))
                                     (read-modal-key! screen)
                                     (do (Thread/sleep 16) nil)))))
         (finally (future-cancel task)))))

(defn table-modal-component
  "Pure `run-modal!` component behind `table-view-dialog!` — the spreadsheet view
   of a `vis-table` artifact. `grid` is `table/parse-csv` output (first row is the
   header). Paging, sorting, geometry and the key map are plain functions of
   immutable state, so the whole viewer is testable with no terminal; only
   `:paint` touches the screen.

   The sheet is PAGED, not scrolled: the window always starts on a page boundary
   (`table/page-start`), so a row never straddles two screens and the title says
   which page of how many you are on.

   Keys: ↑/↓ pick a row, PgUp/PgDn turn a whole page, ←/→ pick a column, Enter
   sorts by that column (toggling ascending/descending), Tab yields the row, Esc
   closes."
  [title grid]
  (let [header
        (vec (first grid))

        data
        (vec (rest grid))

        ncols
        (max 1 (count header))

        ;; Column widths are measured against a header carrying its decorations
        ;; (cursor caret + sort arrow) so moving the cursor or re-sorting NEVER
        ;; re-flows the grid — the marks always have room already.
        sizing-grid
        (into [(mapv (fn [h]
                       (str "▸" h " ▲"))
                     header)]
              data)]

    {:init {:selected 0 :scroll 0 :col 0 :sort-idx nil :sort-dir :asc}
     :measure
     (fn [{:keys [sort-idx sort-dir col selected]} cols rows]
       (let [visible
             (cond-> data
               sort-idx
               (table/sort-csv-rows sort-idx sort-dir))

             total
             (count visible)

             footer
             ;; Five hints have to fit an 80-column terminal: a dropped entry is
             ;; always the LAST one, and losing "Esc close" would hide the only way out.
             [["↑/↓" "row"] ["PgUp/PgDn" "page"] ["←/→" "col"] ["Enter" "sort"] ["Esc" "close"]]

             content-w
             (footer-content-width cols footer (table/csv-natural-width sizing-grid))

             ;; Tall enough to page through a big sheet, but a 3-row CSV gets a
             ;; 3-row box: the grid spends 3 rows on its head (top rule, header,
             ;; rule) and 1 on the bottom rule.
             content-h-req
             (min (long (adaptive-content-height rows nil)) (+ 4 (max 1 (count data))))

             bounds
             (dialog-bounds cols rows content-w content-h-req)

             {:keys [content-top content-h hint-row]}
             (dialog-layout bounds)

             grid-top
             (long content-top)

             list-h
             (max 1 (- (long content-h) 4))

             pages
             (long (table/page-count total list-h))

             page
             (long (table/page-index (p/clamp (long selected) 0 (max 0 (dec (long total)))) list-h))

             widths
             (table/csv-stretch-widths (table/csv-widths sizing-grid (:inner-w bounds))
                                       (:inner-w bounds))

             aligns
             (table/csv-aligns grid)

             head-cells
             (mapv (fn [i]
                     (str (when (= (long i) (long col)) "▸")
                          (nth header i "")
                          (when (= sort-idx i) (if (= :desc sort-dir) " ▼" " ▲"))))
                   (range (count widths)))]

         {:cols cols
          :rows rows
          :title (str title
                      "  "
                      total
                      " row"
                      (when-not (= 1 total) "s")
                      " × "
                      ncols
                      " col"
                      (when-not (= 1 ncols) "s")
                      ;; The page counter appears only when there IS a second
                      ;; page — a 3-row sheet must not grow a pager.
                      (when (> pages 1) (str "  page " (inc page) "/" pages)))
          :visible visible
          :total total
          :footer footer
          :content-w content-w
          :content-h-req content-h-req
          :bounds bounds
          :content-top content-top
          :content-h content-h
          :hint-row hint-row
          :grid-top grid-top
          :list-h list-h
          :page page
          :pages pages
          :widths widths
          :aligns aligns
          :head-cells head-cells}))
     :reconcile (fn [state {:keys [total list-h]}]
                  (let [selected (p/clamp (:selected state) 0 (max 0 (dec (long total))))]
                    (assoc state
                      :selected selected
                      ;; Paging, not scrolling: the window snaps to the page holding
                      ;; the cursor.
                      :scroll (table/page-start selected list-h))))
     :paint (fn [g {:keys [selected scroll]}
                 {:keys [cols rows title visible total footer content-w content-h-req bounds
                         content-top content-h hint-row grid-top list-h widths aligns head-cells]}]
              (let [{:keys [left inner-w]}
                    bounds

                    x
                    (inc (long left))]

                (draw-dialog-chrome! g cols rows title content-w content-h-req)
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g x content-top inner-w content-h)
                (table/draw-line! g x grid-top inner-w false (table/boxed-border-line widths :top))
                (table/draw-line! g
                                  x
                                  (+ (long grid-top) 1)
                                  inner-w
                                  true
                                  (table/boxed-row-line widths head-cells (repeat :left)))
                (table/draw-line! g
                                  x
                                  (+ (long grid-top) 2)
                                  inner-w
                                  false
                                  (table/boxed-border-line widths :middle))
                (if (zero? (long total))
                  (table/draw-line! g x (+ (long grid-top) 3) inner-w false "  No rows")
                  (dotimes [i (min (long list-h) (- (long total) (long scroll)))]
                    (let [idx (+ (long scroll) (long i))]
                      (when (< idx (long total))
                        (table/draw-line!
                          g
                          x
                          (+ (long grid-top) 3 (long i))
                          inner-w
                          (= idx (long selected))
                          (table/boxed-row-line widths (nth visible idx) aligns))))))
                (table/draw-line!
                  g
                  x
                  (+ (long grid-top) 3 (max 1 (min (long list-h) (- (long total) (long scroll)))))
                  inner-w
                  false
                  (table/boxed-border-line widths :bottom))
                (draw-hint-bar! g left hint-row inner-w footer)
                nil))
     :on-key
     (fn [{:keys [selected col sort-idx sort-dir] :as state} key {:keys [total visible list-h]}]
       (let [clampf #(p/clamp % 0 (max 0 (dec (long total))))]
         (if-let [wheel (ScrollBar/wheelStep ^KeyStroke key)]
           (assoc state :selected (clampf (+ (long selected) (long wheel))))
           (condp = (key-type key)
             KeyType/Escape {::done nil}
             KeyType/ArrowUp (assoc state :selected (clampf (dec (long selected))))
             KeyType/ArrowDown (assoc state :selected (clampf (inc (long selected))))
             KeyType/ArrowLeft (assoc state :col (p/clamp (dec (long col)) 0 (dec (long ncols))))
             KeyType/ArrowRight (assoc state :col (p/clamp (inc (long col)) 0 (dec (long ncols))))
             ;; A page key moves a WHOLE window, landing the cursor on the first row
             ;; of the neighbouring page — the spreadsheet idiom.
             KeyType/PageUp (assoc state :selected (clampf (- (long selected) (long list-h))))
             KeyType/PageDown (assoc state :selected (clampf (+ (long selected) (long list-h))))
             KeyType/Home (assoc state :selected 0)
             KeyType/End (assoc state :selected (clampf (dec (long total))))
             ;; Enter re-sorts by the column under the cursor; pressing it again
             ;; on the SAME column flips the direction, the spreadsheet idiom.
             KeyType/Enter (assoc state
                             :sort-idx col
                             :sort-dir (if (and (= sort-idx col) (= :asc sort-dir)) :desc :asc)
                             :selected 0
                             :scroll 0)
             KeyType/Tab (if (pos? (long total)) {::done (nth visible selected nil)} state)
             state))))}))

(defn table-view-dialog!
  "Open a `vis-table` artifact — the CSV/TSV fence `attach` writes — as a live
   spreadsheet: PgUp/PgDn to turn a page, ↑/↓ and ←/→ to move the row /
   column cursor, Enter to sort by the current column. `tbl` is the click region's
   `:table` payload (`{:name :csv :cols :rows :title}`). Returns nil, or the
   selected row on Tab."
  [^TerminalScreen screen tbl]
  (let [grid (table/parse-csv (:csv tbl))]
    (when (seq grid)
      (run-modal! screen
                  (table-modal-component
                    (or (not-empty (str (:title tbl))) (not-empty (str (:name tbl))) "Table")
                    grid)))))

(defn filter-select-items
  "Apply the shared picker filter: case-insensitive substring matching, preserving
   source order. Items may provide `:search-text` to include metadata beyond the
   visible label; otherwise `:label` is the haystack. A blank query shows all."
  [items query]
  (let [q (str/lower-case (str query))]
    (if (str/blank? q)
      (vec items)
      (filterv (fn [item]
                 (str/includes? (str/lower-case (str (or (:search-text item) (:label item)))) q))
        items))))

(defn select-modal-component
  "Build the `run-modal!` component behind `list-dialog!` — a scrollable,
   selectable, optionally type-to-filter list. This is the pure-fn heart of the
   dialog: its `:measure` (geometry), `:reconcile` (scroll window) and `:on-key`
   (navigation / filtering / select) are plain functions of immutable state, so
   they can be exercised in tests WITHOUT a terminal. Only `:paint` touches the
   screen. `items`/opts match `list-dialog!`."
  [title items {:keys [filter? placeholder enter-label height column-offset]}]
  (let [items
        (vec items)

        content?
        (= height :content)

        head-rows
        (if filter? 2 0)]

    {:init {:query "" :selected 0 :scroll 0}
     :measure
     (fn [{:keys [query]} cols rows]
       (let [offset
             (p/clamp (if column-offset (column-offset cols rows) 0) 0 (max 0 (dec (long cols))))

             cols
             (- (long cols) (long offset))

             filtered
             (if filter? (filter-select-items items query) items)

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
                (long (reduce max
                              0
                              (map (fn [it]
                                     (+ (p/display-width (str (:label it)))
                                        (if (:hint it) (+ 2 (p/display-width (str (:hint it)))) 0)))
                                   items))))

             content-w
             (footer-content-width cols footer item-w)

             content-h-req
             (if content? (+ head-rows (min (count items) 16) 1) (adaptive-content-height rows nil))

             bounds
             (dialog-bounds cols rows content-w content-h-req)

             {:keys [content-top content-h hint-row]}
             (dialog-layout bounds)

             list-top
             (+ (long content-top) (long head-rows))

             list-h
             (max 1 (- (long content-h) (long head-rows) 1))]

         {:column-offset offset
          :cols cols
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
                  (let [selected (p/clamp (:selected state) 0 (max 0 (dec (long total))))]
                    (assoc state
                      :selected selected
                      :scroll (visible-window-start selected (:scroll state) list-h total))))
     :paint
     (fn [^TextGraphics graphics {:keys [selected scroll query]}
          {:keys [cols rows title filtered total footer content-w content-h-req bounds content-top
                  content-h hint-row list-top list-h filter? placeholder column-offset]}]
       (binding [frame/*column-offset* column-offset]
         (let [g (if (pos? (long column-offset))
                   (frame/view-graphics (.newTextGraphics graphics
                                                          (TerminalPosition. (int column-offset) 0)
                                                          (TerminalSize. (int cols) (int rows)))
                                        cols
                                        rows)
                   graphics)
               {:keys [left right inner-w]} bounds]

           (draw-dialog-chrome! g cols rows title content-w content-h-req)
           (p/set-colors! g t/dialog-fg t/dialog-bg)
           (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
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
               (p/draw-separator! g left right (inc (long content-top))))
             (dotimes [i (min (long list-h) (long total))]
               (let [idx (+ (long scroll) (long i))
                     row (+ (long list-top) (long i))]

                 (when (< (long idx) (long total))
                   (let [item (nth filtered idx)]
                     (draw-list-item!
                       g
                       left
                       row
                       (if (> (long total) (long list-h)) (dec (long inner-w)) inner-w)
                       (= idx selected)
                       (:label item)
                       (:hint item))))))
             (when (> (long total) (long list-h))
               (ScrollBar/draw g
                               Direction/VERTICAL
                               (TerminalPosition. (int (+ (long left) (long inner-w)))
                                                  (int list-top))
                               (int list-h)
                               (int total)
                               (int list-h)
                               (when (some? scroll) (Integer/valueOf (int scroll)))
                               t/dialog-border
                               t/dialog-bg
                               t/dialog-hint-key
                               t/dialog-bg))
             (draw-hint-bar! g left hint-row inner-w footer)
             (when cursor
               (TerminalPosition. (int (frame/screen-column (.getColumn ^TerminalPosition cursor)))
                                  (.getRow ^TerminalPosition cursor)))))))
     :on-key
     (fn [{:keys [selected query] :as state} key {:keys [total filtered list-h]}]
       (let [clampf #(p/clamp % 0 (max 0 (dec (long total))))]
         (if-let [wheel (ScrollBar/wheelStep ^KeyStroke key)]
           (assoc state :selected (clampf (+ (long selected) (long wheel))))
           (condp = (key-type key)
             KeyType/Escape {::done nil}
             KeyType/ArrowUp (assoc state :selected (clampf (dec (long selected))))
             KeyType/ArrowDown (assoc state :selected (clampf (inc (long selected))))
             KeyType/PageUp (assoc state :selected (clampf (- (long selected) (long list-h))))
             KeyType/PageDown (assoc state :selected (clampf (+ (long selected) (long list-h))))
             KeyType/Enter {::done (when (pos? (long total)) (nth filtered selected))}
             KeyType/Backspace (if filter?
                                 (assoc state
                                   :query (if (seq query) (subs query 0 (dec (count query))) query)
                                   :selected 0)
                                 state)
             KeyType/Character
             (if filter?
               (let [c (key-character key)]
                 (if (and c (not (.isCtrlDown ^KeyStroke key)) (not (.isAltDown ^KeyStroke key)))
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
                    field), capped; nil uses the shared (tall) footprint.
     :column-offset (fn [cols rows] -> left) scopes the dialog to columns from
                    `left` to the terminal edge. Recomputed for each paint."
  [^TerminalScreen screen title items opts]
  (run-modal! screen (select-modal-component title items opts)))

(defn select-dialog!
  "Show a selection list dialog. Returns the selected item map or nil on Esc.
   `items` is a vec of `{:label str, …}` maps. Thin wrapper over `list-dialog!`."
  [^TerminalScreen screen title items]
  (list-dialog! screen title items {}))

(defn multi-select-dialog!
  "Checkbox multi-select over `items` (vec of strings). Space toggles the
   cursor row, `a` toggles all, Enter confirms, Esc cancels. Returns the vec
   of selected strings (possibly empty) on confirm, nil on Esc. Mirrors the
   web modal's alias chips — same proposed options, multi-pick semantics."
  ([screen title items] (multi-select-dialog! screen title items #{}))
  ([^TerminalScreen screen title items initial]
   (with-modal-background
     screen
     (let [items
           (vec items)

           total
           (count items)

           selected
           (atom 0)

           scroll
           (atom 0)

           checked
           (atom (into #{}
                       (keep-indexed (fn [idx item]
                                       (when (contains? (set initial) item) idx)))
                       items))]

       (loop []

         (let [size
               (modal-size! screen)

               cols
               (.getColumns size)

               rows
               (.getRows size)

               g
               (frame/surface-graphics screen cols rows)

               footer
               [["↑/↓" "move"] ["Space" "toggle"] ["a" "all"] ["Enter" "start"] ["Esc" "cancel"]]

               item-w
               (+ 6 (long (reduce max 0 (map #(p/display-width (str %)) items))))

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
               (min (long total) (long content-h))

               _
               (swap! selected #(p/clamp % 0 (max 0 (dec total))))

               _
               (swap! scroll #(visible-window-start @selected % content-h total))]

           (if (zero? total)
             (draw-list-item! g left content-top inner-w false "  (no options)")
             (dotimes [i visible]
               (let [idx (+ (long @scroll) (long i))
                     row (+ (long content-top) (long i))]

                 (when (< (long idx) (long total))
                   (draw-checkbox-item! g
                                        left
                                        row
                                        inner-w
                                        (= idx @selected)
                                        (contains? @checked idx)
                                        (nth items idx))))))
           (draw-hint-bar! g left hint-row inner-w footer)
           (.setCursorPosition screen (p/cursor-pos 0 0))
           (frame/refresh! screen)
           (let [key (read-modal-key! screen)]
             (if (nil? key)
               (recur)
               (condp = (key-type key)
                 KeyType/Escape nil
                 KeyType/ArrowUp
                 (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total)))) (recur))
                 KeyType/ArrowDown
                 (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur))
                 KeyType/PageUp
                 (do (swap! selected #(p/clamp (- (long %) (long content-h)) 0 (max 0 (dec total))))
                     (recur))
                 KeyType/PageDown
                 (do (swap! selected #(p/clamp (+ (long %) (long content-h)) 0 (max 0 (dec total))))
                     (recur))
                 KeyType/Enter (mapv #(nth items %) (sort @checked))
                 KeyType/Character
                 (let [c (lower-key-character key)]
                   (cond (= c \space) (do (when (pos? total)
                                            (swap! checked #(if (contains? % @selected)
                                                              (disj % @selected)
                                                              (conj % @selected))))
                                          (recur))
                         (= c \a)
                         (do (swap! checked #(if (= (count %) total) #{} (set (range total))))
                             (recur))
                         :else (recur)))
                 (recur))))))))))

;;; ── Managed-resource dialog (stop by id) ──────────────────────────────────

(declare text-view-dialog!)

(declare log-view-dialog!)

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
  (with-modal-background
    screen
    (let [lines*
          (atom (vec lines))

          scroll
          (atom 0)

          follow
          (atom (boolean tail?))]

      (loop []

        (let [size
              (modal-size! screen)

              cols
              (.getColumns size)

              rows
              (.getRows size)

              g
              (frame/surface-graphics screen cols rows)

              cur-lines
              @lines*

              bounds
              (draw-dialog-chrome! g cols rows title (max 8 (count cur-lines)))

              {:keys [left inner-w]}
              bounds

              text-w
              (max 1 (- (long inner-w) 2))

              wrapped
              (vec (mapcat (fn [line]
                             (if (str/blank? (str line)) [""] (render/wrap-text (str line) text-w)))
                           (or cur-lines [])))

              total
              (count wrapped)

              {:keys [content-top content-h hint-row]}
              (dialog-layout bounds total)

              visible
              (min (long total) (long content-h))

              max-scroll
              (max 0 (- (long total) (long visible)))

              _
              (when @follow (reset! scroll max-scroll))

              _
              (swap! scroll #(p/clamp % 0 max-scroll))]

          (dotimes [i visible]
            (let [idx (+ (long @scroll) (long i))
                  row (+ (long content-top) (long i))]

              (when (< (long idx) (long total))
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g (inc (long left)) row inner-w 1)
                (p/put-str! g (+ (long left) 2) row (ellipsize (nth wrapped idx) text-w)))))
          (ScrollBar/draw g
                          Direction/VERTICAL
                          (TerminalPosition. (int (+ (long left) (long inner-w))) (int content-top))
                          (int content-h)
                          (int total)
                          (int content-h)
                          (when (some? @scroll) (Integer/valueOf (int @scroll)))
                          t/dialog-border
                          t/dialog-bg
                          t/dialog-hint-key
                          t/dialog-bg)
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
          (frame/refresh! screen)
          (let [key
                (read-modal-key! screen)

                wheel
                (ScrollBar/wheelStep ^KeyStroke key)

                move!
                (fn [f]
                  (reset! follow false)
                  (swap! scroll #(p/clamp (f %) 0 max-scroll)))]

            (cond (nil? key) (recur)
                  wheel (do (move! #(+ (long %) (long wheel))) (recur))
                  :else
                  (condp = (key-type key)
                    KeyType/Escape nil
                    KeyType/Enter nil
                    KeyType/ArrowUp (do (move! dec) (recur))
                    KeyType/ArrowDown (do (move! inc) (recur))
                    KeyType/PageUp (do (move! #(- (long %) (max 1 (long content-h)))) (recur))
                    KeyType/PageDown (do (move! #(+ (long %) (max 1 (long content-h)))) (recur))
                    KeyType/Home (do (reset! follow false) (reset! scroll 0) (recur))
                    KeyType/End
                    (do (reset! follow (boolean tail?)) (reset! scroll max-scroll) (recur))
                    KeyType/Character (do (when (and refresh-fn (= (lower-key-character key) \r))
                                            (reset! lines* (vec (refresh-fn)))
                                            (when tail? (reset! follow true)))
                                          (recur))
                    (recur)))))))))

(defn log-view-dialog!
  "FULLSCREEN log viewer — the whole terminal, edge to edge.

   Unlike `text-view-dialog!` (a centered modal box) this owns the entire screen:
   a title strip on the top row, the log body filling every row beneath it, and a
   hint strip on the bottom row. Lines that already carry ANSI (colored tool
   output) are painted through `render/paint-ansi-line!` — the same path that
   carries color on the transcript's code fences.

   Keys: ↑/↓ line, PgUp/PgDn page, Home/End jump, mouse-wheel scroll, `r` refresh
   (when `:refresh-fn`), Enter/Esc close. Options:
   - :refresh-fn  thunk returning fresh lines — enables `r` refresh so a live
                  buffer (e.g. background-shell logs) can be re-pulled in place.
   - :tail?       start pinned to the newest line and re-follow the bottom on
                  refresh (log-tail behaviour); scrolling up releases the pin.
   Returns nil after close."
  [^TerminalScreen screen title lines & {:keys [refresh-fn tail?]}]
  (with-modal-background
    screen
    (let [lines*
          (atom (vec lines))

          scroll
          (atom 0)

          follow
          (atom (boolean tail?))

          scrollbar-drag-offset
          (volatile! nil)]

      (loop []

        (let [size
              (modal-size! screen)

              cols
              (.getColumns size)

              rows
              (.getRows size)

              g
              (frame/surface-graphics screen cols rows)

              cur-lines
              @lines*

              painted
              (mapv str cur-lines)

              total
              (count painted)

              title-row
              0

              body-top
              1

              hint-row
              (dec rows)

              body-h
              (max 1 (- rows 2))

              visible
              (min total body-h)

              max-scroll
              (max 0 (- total body-h))

              _
              (when @follow (reset! scroll max-scroll))

              _
              (swap! scroll #(p/clamp % 0 max-scroll))]

          (prepare-modal-background! (frame/screen-column 0)
                                     0
                                     (frame/screen-column (dec cols))
                                     (dec rows))
          ;; Whole-screen wipe, then the code-block background under the body.
          (render/fill-background! g cols rows)
          (p/set-colors! g t/code-block-fg t/code-block-bg)
          (p/fill-rect! g 0 body-top cols body-h)
          ;; Top strip: title left, tail/position indicator right.
          (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
          (p/fill-rect! g 0 title-row cols 1)
          (let [tag
                (if @follow
                  "  ● tailing  "
                  (str "  " (min (long total) (+ (long @scroll) (long body-h))) "/" total "  "))

                tag-w
                (p/display-width tag)

                tag-x
                (max 0 (- cols tag-w))]

            (p/put-str! g 1 title-row (ellipsize (str " " title) (max 1 (- tag-x 1))))
            (p/put-str! g tag-x title-row tag))
          ;; Body: one source line per row, ANSI runs → theme colors, clipped at
          ;; the right edge (no wrap — log lines stay whole and scroll math simple).
          (dotimes [i visible]
            (let [idx (+ (long @scroll) (long i))
                  y (+ body-top i)]

              (when (< (long idx) (long total))
                (render/paint-ansi-line! g 0 y (nth painted idx) t/code-block-fg t/code-block-bg))))
          ;; Scrollbar last (over the rightmost column) so a wide line can't hide it.
          (ScrollBar/draw g
                          Direction/VERTICAL
                          (TerminalPosition. (int (dec cols)) (int body-top))
                          (int body-h)
                          (int total)
                          (int body-h)
                          (when (some? @scroll) (Integer/valueOf (int @scroll)))
                          t/dialog-border
                          t/dialog-bg
                          t/dialog-hint-key
                          t/dialog-bg)
          ;; Bottom strip: shared hint bar, full width.
          (draw-hint-bar! g
                          0
                          hint-row
                          (dec cols)
                          (cond-> [["↑/↓" "scroll"] ["PgUp/PgDn" "page"] ["Home/End" "jump"]]
                            refresh-fn
                            (conj ["r" (if @follow "tailing" "refresh")])

                            :always
                            (conj ["Enter/Esc" "close"])))
          ;; Read-only viewer — no text field, so hide the terminal cursor (nil)
          ;; instead of parking it at 0,0, where it blinks in the top-left corner.
          (.setCursorPosition screen nil)
          (frame/refresh! screen)
          (let [key
                (read-modal-key! screen)

                wheel
                (ScrollBar/wheelStep ^KeyStroke key)

                move!
                (fn [f]
                  (reset! follow false)
                  (swap! scroll #(p/clamp (f %) 0 max-scroll)))]

            (cond (nil? key) (recur)
                  wheel (do (move! #(+ (long %) (long wheel))) (recur))
                  (instance? MouseAction key)
                  (let [^ScrollBar$DragResult drag
                        (ScrollBar/dragStep ^MouseAction key
                                            Direction/VERTICAL
                                            (TerminalPosition. (int (dec cols)) (int body-top))
                                            (int body-h)
                                            (int total)
                                            (int body-h)
                                            (Integer/valueOf (int @scroll))
                                            (when (some? @scrollbar-drag-offset)
                                              (Integer/valueOf (int @scrollbar-drag-offset)))
                                            1)]
                    (when (and drag (.release drag)) (vreset! scrollbar-drag-offset nil))
                    (when-let [grip (and drag (.gripOffset drag))]
                      (vreset! scrollbar-drag-offset (long grip)))
                    (when-let [s (and drag (.scrollPosition drag))]
                      ;; A deliberate scrollbar drag is a read, not a follow.
                      (reset! follow false)
                      (reset! scroll (long s)))
                    (recur))
                  :else (condp = (key-type key)
                          KeyType/Escape nil
                          KeyType/Enter nil
                          KeyType/ArrowUp (do (move! dec) (recur))
                          KeyType/ArrowDown (do (move! inc) (recur))
                          KeyType/PageUp (do (move! #(- (long %) (max 1 (long body-h)))) (recur))
                          KeyType/PageDown (do (move! #(+ (long %) (max 1 (long body-h)))) (recur))
                          KeyType/Home (do (reset! follow false) (reset! scroll 0) (recur))
                          KeyType/End
                          (do (reset! follow (boolean tail?)) (reset! scroll max-scroll) (recur))
                          KeyType/Character (do (when (and refresh-fn
                                                           (= (lower-key-character key) \r))
                                                  (reset! lines* (vec (refresh-fn)))
                                                  (when tail? (reset! follow true)))
                                                (recur))
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
   :body string-or-lines rendered above the input label,
   :flat? true selects the minimal inline-border chrome."
  [^TerminalScreen screen title label & {:keys [mask initial body flat?] :or {initial ""}}]
  (with-modal-background
    screen
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
              (modal-size! screen)

              cols
              (.getColumns size)

              rows
              (.getRows size)

              g
              (frame/surface-graphics screen cols rows)

              ;; Content: body rows + label row + spacer + 3-row bordered input box.
              ;; Pre-estimate the content height (at the default width) so the box is
              ;; sized to the prompt it actually holds.
              est-w
              (max 1 (- (default-content-width cols) 2))

              est-body
              (->> body-lines
                   (mapcat (fn [line]
                             (if (str/blank? line) [""] (render/wrap-text line est-w))))
                   vec)

              req-h
              (+ 4 (if (seq est-body) 1 0) (count est-body))

              bounds
              (if flat?
                (draw-flat-dialog-chrome! g cols rows title)
                (draw-dialog-chrome! g cols rows title req-h))

              {:keys [left inner-w]}
              bounds

              left
              (long left)

              inner-w
              (long inner-w)

              text-w
              (max 1 (- inner-w 2))

              wrapped-body
              (->> body-lines
                   (mapcat (fn [line]
                             (if (str/blank? line) [""] (render/wrap-text line text-w))))
                   vec)

              body-gap
              (if (seq wrapped-body) 1 0)

              content-count
              (+ 4 body-gap (count wrapped-body))

              {:keys [content-top content-h hint-row]}
              (dialog-layout bounds content-count)

              content-top
              (long content-top)

              content-h
              (long content-h)

              max-body-lines
              (max 0 (- content-h 4 body-gap))

              visible-body
              (if (<= (count wrapped-body) max-body-lines)
                wrapped-body
                (conj (vec (take (max 0 (dec max-body-lines)) wrapped-body)) "..."))

              body-top
              content-top

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

          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (doseq [[idx line] (map-indexed vector visible-body)]
            (let [row (+ body-top (long idx))]
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
          (frame/refresh! screen)
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
                (= KeyType/PasteStart (.getKeyType ^KeyStroke key))
                (do (vreset! paste-buffer (StringBuilder.)) (recur))
                (= KeyType/PasteEnd (.getKeyType ^KeyStroke key))
                (let [^StringBuilder sb @paste-buffer]
                  (when sb
                    (let [payload (.toString sb)
                          chars (vec payload)]

                      (vreset! paste-buffer nil)
                      (when-not (.isEmpty payload)
                        (swap! text (fn [t]
                                      (into (subvec t 0 @cursor)
                                            (concat chars (subvec t @cursor)))))
                        (swap! cursor + (count chars)))))
                  (recur))
                ;; Accumulate chars into the paste buffer while open.
                (some? @paste-buffer) (do (when-let [text (.getText ^KeyStroke key)]
                                            (.append ^StringBuilder @paste-buffer ^String text))
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
                        KeyType/Backspace (do (when (pos? (long @cursor))
                                                (swap! text #(into (subvec % 0 (dec (long @cursor)))
                                                                   (subvec % @cursor)))
                                                (swap! cursor dec))
                                              (recur))
                        KeyType/ArrowLeft (do (swap! cursor #(max 0 (dec (long %)))) (recur))
                        KeyType/ArrowRight (do (swap! cursor #(min (count @text) (inc (long %))))
                                               (recur))
                        (recur))))))))))

;;; ── Confirm dialog ──────────────────────────────────────────────────────────
(defn- draw-button!
  "Draw a confirm-dialog action button in the shared Blockether look, mirroring
   `components/action-button!`: every state is the same
   filled ` label ` pill and only the COLOUR differs. `Yes` is the PRIMARY cap (ink
   fill, cream bold label) and `No` the muted secondary — and whichever one the
   choice sits on takes the ACCENT fill, the same colour the active tab wears. No
   `▏`/`▕` rails and no marker glyph: a button here is a solid pill and focus is a
   colour. Same width in every state, so the row stays put as the choice moves.
   Returns the consumed width."
  [g col row label {:keys [variant is-focused]}]
  (let [col
        (long col)

        w
        (+ 2 (p/display-width label))

        [fg bg]
        (cond is-focused [t/header-active-tab-fg t/header-active-tab-bg]
              (= :primary variant) [t/dialog-bg t/dialog-hint-key]
              :else [t/dialog-bg t/dialog-hint])]

    (p/clear-styles! g)
    (p/set-colors! g fg bg)
    (p/enable! g p/BOLD)
    (p/put-str! g col row (str " " label " "))
    (p/clear-styles! g)
    w))

(defn confirm-dialog!
  "Show Y/N confirmation with side-by-side buttons. Returns true/false, nil on Esc."
  [^TerminalScreen screen title message]
  (with-modal-background
    screen
    (let [raw-lines
          (if (string? message) [message] message)

          btn-yes
          "Yes"

          btn-no
          "No"

          btn-w
          (+ 2 (max (p/display-width btn-yes) (p/display-width btn-no)))

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
              (modal-size! screen)

              cols
              (.getColumns size)

              rows
              (.getRows size)

              g
              (frame/surface-graphics screen cols rows)

              bounds
              (draw-dialog-chrome! g cols rows title ch)

              {:keys [left inner-w]}
              bounds

              {:keys [content-top content-h hint-row]}
              (dialog-layout bounds ch)

              text-w
              (max 0 (- (long inner-w) 2))

              lines
              (vec (mapcat #(render/wrap-text % text-w) raw-lines))

              btn-row
              (+ (long content-top) (count lines) 1)

              ;; blank line then buttons
              ;; Center buttons horizontally
              total-btn-w
              (+ btn-w btn-gap btn-w)

              btn-start
              (+ (long left) 1 (quot (- (long inner-w) (long total-btn-w)) 2))]

          ;; Message text - centered per line
          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (doseq [[i line] (map-indexed vector lines)]
            (let [row (+ (long content-top) (long i))]
              (when (< row (+ (long content-top) (long content-h)))
                (p/fill-rect! g (inc (long left)) row inner-w 1)
                (p/draw-centered! g (inc (long left)) row inner-w line))))
          ;; Buttons - side by side
          (p/set-bg! g t/dialog-bg)
          (p/fill-rect! g (inc (long left)) btn-row inner-w 1)
          (draw-button! g btn-start btn-row btn-yes {:variant :primary :is-focused (= @focus 0)})
          (draw-button! g
                        (+ (long btn-start) (long btn-w) (long btn-gap))
                        btn-row
                        btn-no
                        {:variant :secondary :is-focused (= @focus 1)})
          (draw-hint-bar! g
                          left
                          hint-row
                          inner-w
                          [["<-/->" "switch"] ["Enter" "confirm"] ["Esc" "cancel"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (frame/refresh! screen)
          (let [key (read-modal-key! screen)]
            (when key
              (condp = (key-type key)
                KeyType/Escape nil
                KeyType/Enter (= @focus 0) ;; true if Yes focused
                KeyType/ArrowLeft (do (reset! focus 0) (recur))
                KeyType/ArrowRight (do (reset! focus 1) (recur))
                KeyType/Tab (do (swap! focus #(if (zero? (long %)) 1 0)) (recur))
                KeyType/Character (let [c (lower-key-character key)]
                                    (cond (= c \y) true
                                          (= c \n) false
                                          :else (recur)))
                (recur)))))))))

(defn host-band-region
  "ONE band INSTANCE inside a frame the host already painted: the caller's
   `tr/run!` geometry plus the single frame snapshot the whole flow shares.

   Taken at the FIRST band, the snapshot holds the host exactly as the user last
   saw it — the settings list, the provider cards, the transcript — so every band
   after it can hand back the rows a taller predecessor covered. A host that
   already made one keeps its own."
  [^TerminalScreen screen region]
  (update region :restore! #(or % (frame-restorer screen))))

(defn embed-transient!
  "Run ONE transient (`tr/run!`) INSIDE a frame someone else owns — same box,
   same hint row, no second window. THE band component: the session screen,
   Settings, the provider manager and `transient-dialog!`
   are all separate INSTANCES of it, differing only in the region they hand in.

   `region` is already in `tr/run!` geometry (`:left`, `:inner-w`, `:hint-row`,
   `:text-w`, plus the optional `:min-row` floor and the `:restore!` snapshot
   `host-band-region` takes). Returns `tr/run!`'s `{:action :switches :options}`,
   or nil on Esc.

   With a `title`, the title is inked ON the band's opening rule, so the first
   row is chrome and every row under it is the column grid.

   This is THE seam between a Lanterna surface and the host-agnostic transient
   component. Nothing else calls `tr/run!` with a `transient-host`."
  ([^TerminalScreen screen g region spec] (tr/run! (transient-host screen g) region spec))
  ([^TerminalScreen screen g region title spec]
   (embed-transient! screen g region (assoc spec :title title))))

(defn- band-question-frame!
  "Repaint `region` as a band holding ONE question: the host rows a taller band
   covered are handed back, the chrome is redrawn, `title` is the band's own bold
   title and `hints` its hint bar. Returns the first body row."
  ([g region title hints] (band-question-frame! g region title hints 1))
  ([g {:keys [left inner-w text-w restore!] :as region} title hints n]
   (let [{:keys [sep-row title-row title-rule-row body-top foot-rule-row foot-row wipe-top
                 top-limit]}
         (tr/band-geometry region n true)]
     (when restore! (restore! top-limit (dec (long wipe-top))))
     (tr/clear-rows! g region (max (long top-limit) (long wipe-top)) foot-row)
     (when (>= (long sep-row) (long top-limit)) (tr/draw-rule! g region sep-row))
     (when (> (long title-rule-row) (long title-row)) (tr/draw-rule! g region title-rule-row))
     (when (> (long foot-rule-row) (max (long sep-row) (long top-limit)))
       (tr/draw-rule! g region foot-rule-row))
     (p/set-colors! g t/dialog-hint-key t/dialog-bg)
     (p/styled g [p/BOLD] (p/put-str! g (+ (long left) 2) title-row (ellipsize (str title) text-w)))
     (draw-hint-bar! g left foot-row inner-w hints)
     body-top)))

(defn mini-read!
  "Ask ONE typed question in the band's own frame: `label` becomes the band's
   title and the answer is typed into a real input row under it, so nothing the
   keyboard no longer owns stays advertised. Enter submits the trimmed string
   (may be empty), Esc returns nil. Opts: :initial (seed text), :mask (echo
   char), :placeholder (dim hint while the field is empty)."
  [^TerminalScreen screen g {:keys [left inner-w] :as region} label
   {:keys [initial mask placeholder]}]
  (let [text
        (atom (vec (or initial "")))

        cursor
        (atom (count (or initial "")))]

    (loop []

      (let [row
            (band-question-frame! g region label [["Enter" "submit"] ["Esc" "cancel"]])

            txt
            (apply str @text)

            display
            (if mask (apply str (repeat (count txt) mask)) txt)

            ;; The answer sits on the band's own body lead — one column inside the
            ;; frame, the very inset a form's rows take — so the field breathes off
            ;; both rails instead of opening flush against the left one.
            pos
            (draw-input-item! g
                              (inc (long left))
                              row
                              (dec (long inner-w))
                              true
                              display
                              @cursor
                              placeholder)]

        (.setCursorPosition screen pos)
        (frame/refresh! screen)
        (let [key (read-modal-key! screen)]
          (if (nil? key)
            (recur)
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/Enter (str/trim (apply str @text))
              KeyType/Character (let [c (key-character key)]
                                  (swap! text #(into (subvec % 0 @cursor)
                                                     (cons c (subvec % @cursor))))
                                  (swap! cursor inc)
                                  (recur))
              KeyType/Backspace (do (when (pos? (long @cursor))
                                      (swap! text #(into (subvec % 0 (dec (long @cursor)))
                                                         (subvec % @cursor)))
                                      (swap! cursor dec))
                                    (recur))
              KeyType/ArrowLeft (do (swap! cursor #(max 0 (dec (long %)))) (recur))
              KeyType/ArrowRight (do (swap! cursor #(min (count @text) (inc (long %)))) (recur))
              (recur))))))))

(defn region-option-reader
  "A `:read-option` for a transient EMBEDDED in someone else's frame.

   `transient-dialog!` builds this for its own modal; a band painted into a host
   region (Settings, the provider manager) needs the same question, in the same
   frame, so an OPTION is typed without opening a second window."
  [^TerminalScreen screen g region]
  (fn [{:keys [label prompt mask]} current]
    (mini-read! screen g region (or prompt (str label ":")) {:initial current :mask mask})))

(defn- mini-choose!
  "Ask WHICH one in the band's own frame. `choices` is a vec of
   {:key char :label str :id kw}, painted as the band's OWN rows under the
   question — a band paints no title row, so the question is inked ON the band's
   opening rule. Returns the chosen `:id`, or nil on Esc."
  [^TerminalScreen screen g region title choices]
  (:action (embed-transient! screen
                             g
                             region
                             {:title title
                              :groups [{:items
                                        (mapv (fn [{:keys [key label id]}]
                                                {:key (str key) :type :action :id id :label label})
                                              choices)}]})))

(defn- mini-confirm!
  "Ask y/n in the band's own frame: the question is inked ON the band's opening
   rule and `Yes` / `No` are the only rows under it. Returns true / false / nil (Esc).

   A DESTRUCTIVE question owes the reader more than the word `Yes`, so `opts`
   carries what the companion's own confirm row carries: `:cost` — ONE line
   saying what agreeing costs — becomes the heading over the two answers, and
   `:yes-label` / `:no-label` name them in the caller's verb (`Yes, remove` /
   `Keep it`) instead of making the reader remember what was asked."
  ([^TerminalScreen screen g region question] (mini-confirm! screen g region question nil))
  ([^TerminalScreen screen g region question {:keys [cost yes-label no-label]}]
   (case (:action (embed-transient!
                    screen
                    g
                    region
                    {:title question
                     :groups
                     [(cond-> {:items [{:key "y" :type :action :id :yes :label (or yes-label "Yes")}
                                       {:key "n" :type :action :id :no :label (or no-label "No")}]}
                        (not (str/blank? (str cost)))
                        (assoc :title (str cost)))]}))
     :yes
     true

     :no
     false

     nil)))

(defn- mini-note!
  "SAY one line in the band's own frame — the refusal a verb came back with. The
   message is the band's heading, `q` dismisses it, and the list it was fired from
   stays on screen: a failure reached from a transient must not answer with a
   window on top of it either. Returns nil."
  [^TerminalScreen screen g region title line]
  (embed-transient! screen
                    g
                    region
                    {:title title
                     :groups [{:title (str line)
                               :items [{:key "q" :type :action :id :dismiss :label "Dismiss"}]}]})
  nil)

(defn- mini-wait!
  "HOLD the band while something else finishes — a browser round-trip the daemon
   is polling for. `line-fn` renders the ONE status line on every tick (elapsed
   seconds), `done?` says the wait is over, and Esc gives up: a flow that has to
   wait must not blank the list it was fired from either, so the wait paints in
   the same frame as the question that started it. Returns true when `done?` won,
   nil when the user pressed Esc."
  [^TerminalScreen screen g {:keys [left text-w] :as region} title line-fn done?]
  (loop []

    (if (done?)
      true
      (let [row (band-question-frame! g region title [["Esc" "cancel"]])]
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/put-str! g (+ (long left) 2) row (ellipsize (str (line-fn)) text-w))
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (frame/refresh! screen)
        (if (some-> (.pollInput screen)
                    modal-escape-key?)
          nil
          (do (Thread/sleep 120) (recur)))))))

;;; ── One band, every question it can ask ─────────────────────────────────────
;; A band IS a region — six coordinates — plus the screen it paints on. They are
;; bound ONCE, here, and every host COMPOSES the result: the provider
;; manager, Settings, the session band and
;; `transient-dialog!` open their sub-transients through the same host and ask
;; their follow-up questions on their own hint row through the same minibuffers.
;; A caller that unpacks `:left`/`:inner-w`/`:hint-row`/`:text-w` again, or
;; reaches for `tr/run!` plus `transient-host` itself, is how two bands drift
;; apart.

(defn- mini-view!
  "Read and scroll text inside the caller's band, without opening another frame."
  [^TerminalScreen screen g {:keys [left inner-w hint-row min-row] :as region} title lines]
  (let [width
        (max 1 (- (long inner-w) 4))

        lines
        (vec (mapcat #(p/fold-cols % width) lines))

        height
        (max 1 (min (count lines) (- (long hint-row) (long (or min-row 0)) 4)))

        limit
        (max 0 (- (count lines) height))]

    (loop [offset 0]
      (let [top (band-question-frame! g
                                      region
                                      title
                                      [["q/Esc" "back"] ["↑/↓" "scroll"] ["PgUp/PgDn" "page"]]
                                      height)]
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[i line] (map-indexed vector (take height (drop offset lines)))]
          (p/put-str! g (+ (long left) 2) (+ (long top) (long i)) line))
        (.setCursorPosition screen nil)
        (frame/refresh! screen)
        (let [key (read-modal-key! screen)
              kt (when key (key-type key))
              c (when key (key-character key))
              wheel (when key (ScrollBar/wheelStep ^KeyStroke key))]

          (when-not (or (= kt KeyType/Escape) (= kt KeyType/Enter) (= c \q))
            (recur (long (p/clamp (if wheel
                                    (+ (long offset) (long wheel))
                                    (condp = kt
                                      KeyType/ArrowUp (dec (long offset))
                                      KeyType/ArrowDown (inc (long offset))
                                      KeyType/PageUp (- (long offset) height)
                                      KeyType/PageDown (+ (long offset) height)
                                      KeyType/Home 0
                                      KeyType/End limit
                                      offset))
                                  0
                                  limit)))))))))

(defn band-questions
  "Everything a band can ASK, bound to its own `region` once:

     `:read!`        one typed answer, in the band's own frame — `[label]` / `[label opts]`
     `:choose!`      WHICH one, single-key — `[title choices]`, returns the `:id`
     `:confirm!`     y/n — `[question]` / `[question {:cost … :yes-label … :no-label …}]`
     `:note!`        SAY one line back — `[title line]`, dismissed with `q`
     `:view!`        read-only scrollable text in this band — `[title lines]`
     `:wait!`        HOLD the band while something else finishes — `[title line-fn done?]`
     `:transient!`   ANOTHER transient over the SAME band region — `[spec]`, its
                     `:read-option` already bound, so an OPTION item inside it
                     is typed on this band's own hint row
     `:read-option`  the `:read-option` a spec with OPTION items hands `tr/run!`

   A transient that opens a transient, and a question that REPLACES the commands
   that led to it instead of opening a second frame, is how a band asks a second
   thing — every band in the TUI does both through this map."
  [^TerminalScreen screen g region]
  (let [read-option (region-option-reader screen g region)]
    {:read! (fn read! ([label] (read! label {})) ([label opts] (mini-read! screen g region label
                                                                 opts)))
     :choose! (fn [title choices]
                (mini-choose! screen g region title choices))
     :confirm! (fn confirm! ([question] (confirm! question nil)) ([question opts] (mini-confirm!
                                                                                    screen g region
                                                                                    question opts)))
     :note! (fn [title line]
              (mini-note! screen g region title line))
     :view! (fn [title lines]
              (mini-view! screen g region title lines))
     :wait! (fn [title line-fn done?]
              (mini-wait! screen g region title line-fn done?))
     :transient! (fn [spec]
                   (embed-transient! screen
                                     g
                                     region
                                     (assoc spec
                                       :read-option (or (:read-option spec) read-option))))
     :read-option read-option}))

(defn transient-dialog!
  "Host ONE transient in its OWN modal — the popup for flows that have no
   status buffer to sit in (provider authentication). `body` (a string or lines)
   is the caller's guidance, painted once at the top of the content area; the
   transient owns every row under it and its hint bar lands on the dialog's own
   hint row. The box is sized to what it actually holds, so a two-line prompt
   opens a small dialog instead of a half-screen one.

   OPTION items are read INLINE on that hint row (`mini-read!`), honouring
   the item's `:prompt` (default `<label>:`) and `:mask` (`\\*` for a credential);
   mark such an item `:secret? true` and its value renders as dots, never as
   text. `spec` may carry a `:title` for the popup itself when the frame's title
   would read redundantly.

   Returns `tr/run!`'s `{:action :switches :options}`, or nil on Esc."
  [^TerminalScreen screen title body spec]
  (with-modal-background
    screen
    (let [size
          (modal-size! screen)

          cols
          (.getColumns size)

          rows
          (.getRows size)

          g
          (frame/surface-graphics screen cols rows)

          est-w
          (max 1 (- (default-content-width cols) 2))

          wrapped
          (->> (text-input-body-lines body)
               (mapcat (fn [line]
                         (if (str/blank? line) [""] (render/wrap-text line est-w))))
               vec)

          ;; The popup's own footprint — the component knows it (`tr/height`), so the
          ;; box is sized by what the transient will actually paint — a heading
          ;; wraps, so the WIDTH the box will give it is part of that answer.
          popup-h
          (tr/height spec {:inner-w est-w})

          body-gap
          (if (seq wrapped) 1 0)

          content-count
          (+ (count wrapped) (long body-gap) (long popup-h))

          bounds
          (draw-dialog-chrome! g cols rows title content-count)

          {:keys [left inner-w]}
          bounds

          left
          (long left)

          inner-w
          (long inner-w)

          text-w
          (max 1 (- inner-w 2))

          {:keys [content-top hint-row]}
          (dialog-layout bounds content-count)

          content-top
          (long content-top)]

      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (doseq [[idx line] (map-indexed vector wrapped)]
        (let [row (+ content-top (long idx))]
          (p/fill-rect! g (inc left) row inner-w 1)
          (p/put-str! g (+ left 2) row (ellipsize line text-w))))
      (let [region {:left left
                    :inner-w inner-w
                    :hint-row hint-row
                    :text-w text-w
                    :min-row (+ content-top (count wrapped) (long body-gap))}]
        (embed-transient! screen
                          g
                          region
                          (assoc spec
                            :title (or (:title spec) title)
                            :read-option (region-option-reader screen g region)))))))

(defn- theme-choice-order
  []
  (try (mapv keyword (shared-theme/available-theme-ids))
       (catch Throwable t
         (tel/log! :warn ["dialogs: available-theme-ids failed" (ex-message t)])
         [(keyword shared-theme/default-theme-id)])))

(defn- settings-ui-options
  "Terminal-local response and theme preferences, grouped like the app's Settings.
   Engine settings use the registry."
  []
  [{:type :section :label "Responses"}
   {:key :show-python-code
    :type :toggle
    :label "Show Python code and results"
    :description
    "Show source code and raw results before Activity. Turn off to show only Activity."}
   {:key :summarize-steps
    :type :toggle
    :label "Summarize steps between notes"
    :description
    "Combine the steps between progress notes into one Activity, during and after a turn. Turn off to show Activity for each step."}
   {:type :section :label "Theme"}
   {:key :theme-name
    :type :choice
    :choices (theme-choice-order)
    :label "Theme"
    :description
    "Reusable channel theme from com.blockether.vis.tui.shared-theme and extension :ext/theme maps"}])

(declare titleize-label)

(def ^:private settings-inventory
  "Cached gateway settings catalog rendered INSIDE Settings.

   Stays `:unloaded` until a dialog asks for it, so `settings-rows` keeps
   working — and stays gateway-free — for callers and tests without one."
  (atom {:status :unloaded :groups [] :error nil}))

(def ^:dynamic *settings-target* nil)

(def ^:dynamic *settings-context*
  "The session whose project, group and session settings may decide a row, or nil."
  nil)

(def ^:dynamic *local-settings-inventory* nil)

(defn- settings-inventory-atom [] (or *local-settings-inventory* settings-inventory))

(defn- mirror-setting-value!
  "Mirror ONE catalog row's value onto the process registry when this binary
   registers that id itself, so local reads (`plans` in the annotator,
   `codex_fast_mode` in the footer) honour what the daemon holds. Ids only the
   daemon knows stay OUT of the registry: their rows render from the catalog."
  [row]
  (let [id
        (get row "id")

        value
        (if (contains? row "enabled") (get row "enabled") (get row "value"))]

    (when (and (some? value) (vis/toggle-spec id))
      (try (vis/toggle-set-value! id value) (catch Throwable _ nil)))))

(defn load-settings-inventory!
  "Refresh the cached settings catalog from the gateway. Never throws: a daemon
   that cannot answer keeps the catalog Settings last read — and, before the
   first answer, the process-registry projection — instead of a blank pane."
  []
  (let [answer (try (let [response (vis/gateway-settings :tui *settings-target* *settings-context*)]
                      {:status :ok
                       :groups (vec (get response "groups"))
                       ;; The gateway names a group or project; the caller only has its id.
                       :label (get response "label")
                       :error nil})
                    (catch Exception e {:status :error :error (ex-message e)}))]
    (if (= :ok (:status answer))
      (do (when-not *settings-target*
            (run! mirror-setting-value! (mapcat #(get % "toggles") (:groups answer))))
          (reset! (settings-inventory-atom) answer))
      (swap! (settings-inventory-atom) assoc :status :error :error (:error answer)))))

(defn- cache-setting-row!
  "Fold ONE refreshed gateway row back into the cached catalog, so the frame
   after a flip renders the value the daemon just confirmed without a re-read."
  [row]
  (when-let [id (get row "id")]
    (swap! (settings-inventory-atom) update
      :groups
      (fn [groups]
        (mapv (fn [group]
                (update group
                        "toggles"
                        (fn [rows]
                          (mapv #(if (= id (get % "id")) (merge % row) %) rows))))
              (or groups []))))))

(defn- catalog-toggle-rows
  "Project the gateway catalog without repeating metadata or reset actions in the list.
   Global settings are the root scope: an explicit value there overrides nothing, so only a
   scoped target marks overrides and offers their reset."
  [groups]
  (vec
    (mapcat (fn [group]
              (let [rows (filterv #(not= "agent_name" (get % "id")) (get group "toggles"))]
                (when (seq rows)
                  (cons {:type :section :label (str (get group "title"))}
                        (mapv (fn [row]
                                (let [type (get row "type")
                                      id (get row "id")
                                      override? (and *settings-target* (get row "is_override"))]

                                  {:key (keyword (str "toggle::" id))
                                   :type (case type
                                           "string"
                                           :text-setting

                                           "number"
                                           :number-setting

                                           ("array" "object")
                                           :structured-setting

                                           :registry-toggle)
                                   :toggle-id id
                                   :toggle-type (keyword type)
                                   :toggle-value (if (= "boolean" type)
                                                   (boolean (get row "enabled"))
                                                   (get row "value"))
                                   :setting row
                                   :choices (vec (get row "choices"))
                                   :experimental? (boolean (get row "is_experimental"))
                                   :source (get row "source")
                                   :is-override? (boolean override?)
                                   :label (str (get row "label"))
                                   :description (str (get row "description"))}))
                              rows)))))
            (or groups []))))

(defn- override-note
  "Explain why a more specific scope decides this catalog row for the session
   Settings opened from, or nil. The gateway marks such rows only when the
   request names that session."
  [row]
  (when-let [{:strs [scope enabled value]} (get row "overridden_by")]
    (let [where (str (str/capitalize (str scope)) " settings")]
      (str where
           (cond (some? enabled) (if enabled " turn this on" " turn this off")
                 (= "enum" (get row "type")) (str " set this to " value)
                 :else " set this")
           " for this session. Change it in "
           where
           "."))))

(defn- lock-overridden-rows
  "Lock each catalog row that a more specific scope decides for the session
   Settings opened from; its description says where to change it instead."
  [groups rows]
  (let [notes (into {}
                    (keep #(when-let [note (override-note %)] [(get % "id") note]))
                    (mapcat #(get % "toggles") groups))]
    (mapv (fn [{:keys [toggle-id] :as row}]
            (if-let [note (get notes toggle-id)]
              (assoc row :locked note)
              row))
          rows)))

(defn- registry-toggle-rows
  "Settings rows for the feature toggles this channel shows.

   The gateway owns the catalog: once `load-settings-inventory!` has answered,
   every row is a projection of `GET /v1/settings?channel=tui` — the SAME
   groups, order and rows the companion app renders, so a toggle the engine or
   an extension registers shows up here without a mirrored registration in this
   binary. Until that first answer, and whenever the daemon cannot be reached,
   the process registry renders the pane instead of leaving it blank."
  []
  (let [groups (:groups @(settings-inventory-atom))]
    (if (or *settings-target* (seq groups))
      (lock-overridden-rows groups (catalog-toggle-rows groups))
      ;; `toggles-for-channel` drops provider-specific knobs whose provider
      ;; isn't configured (`:visible-fn`) AND toggles scoped to OTHER channels
      ;; (`:channels`) — e.g. the web theme never shows in the TUI dialog.
      (let [specs (vis/toggles-for-channel :tui)]
        (when (seq specs)
          (vec (mapcat
                 (fn [[group group-specs]]
                   (cons {:type :section :label (titleize-label (name (or group :other)))}
                         (for [{:keys [id label description owner]} (sort-by :id group-specs)]
                           {:key (keyword (str "toggle::" id))
                            :type :registry-toggle
                            :toggle-id id
                            :label (or label
                                       (titleize-label (str (or (namespace id) "") " " (name id))))
                            :description (str (or description "")
                                              (when (and owner (not= owner :vis))
                                                (str "  [" (titleize-label (name owner)) "]")))})))
                 (sort-by (comp str key) (group-by #(or (:group %) :other) specs)))))))))

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

(def ^:private provider-inventory
  "Cached provider fleet plus each provider's gateway auth verdict, rendered
   INSIDE Settings.

   Stays `:unloaded` until a dialog asks for it, so `settings-rows` keeps
   working — and stays gateway-free — for callers and tests without one."
  (atom {:status :unloaded :providers [] :error nil}))

(defn- provider-fleet
  "The fleet the Providers section shows: configured providers first, then every
   preset the gateway already holds credentials for — the same list the full
   provider manager builds, so both surfaces never disagree."
  [config]
  (let [base
        (vec (or (:providers config) []))

        configured-ids
        (into #{} (map :id) base)]

    (into base
          (remove #(contains? configured-ids (:id %)))
          (try (vis/authenticated-preset-providers) (catch Throwable _ nil)))))

(defn router-primary
  "PURE: the PRIMARY router root a surface should SHOW for `fleet` — the pair the
   router itself resolves (`vis/resolve-default-selection`: an explicit tag wins,
   an unset or dangling one degrades to the fleet's first provider), falling back
   to the literal config tag when no fleet entry carries a model at all.

   Surfaces must not read `:default-provider` raw. A machine that just added its
   only provider, or removed the tagged one, has a config the ROUTER already reads
   as pointing somewhere — showing the raw key instead marked nothing as default
   and made a fresh fleet look unusable."
  [config fleet]
  (or (vis/resolve-default-selection config fleet)
      (when-let [pid (:default-provider config)]
        {:provider-id (keyword (name pid)) :model (:default-model config)})))

(defn- gateway-auth-index
  "ONE gateway round trip for the WHOLE fleet. `GET /v1/router` already carries
   every provider's daemon-classified auth state, so Settings asks once instead
   of once per provider. Returns `{provider-id-string status-map}`."
  []
  (try (into {}
             (keep (fn [entry]
                     (when-let [id (get entry "id")]
                       [id (or (get entry "status") {})])))
             (vis/gateway-router-fleet))
       (catch Throwable _ nil)))

(defn- provider-auth-state
  "The gateway's four-state auth verdict, refined only for display: a provider
   with no credential is `:off`, while a reachable local runtime is `:local`.
   The TUI never reads credential files itself."
  [provider auth-index]
  (let [status
        (get auth-index
             (some-> (:id provider)
                     name))

        state
        (some-> (get status "auth_state")
                keyword)

        authenticated?
        (true? (get status "is_authenticated"))]

    (cond (and (contains? vis/provider-local-no-auth-ids (:id provider)) (= :verified state)) :local
          (contains? #{:verified :rejected :degraded} state) state
          authenticated? :unverified
          :else :off)))

(defn load-provider-inventory!
  "Refresh the cached provider fleet from config + gateway. Never throws: a
   gateway that is down becomes an inline row instead of yet another modal. The
   auth verdicts come from a SINGLE `/v1/router` read covering every provider,
   so opening Settings costs one round trip whatever the fleet size.

   Router selection is part of the fleet, not a separate lookup: each entry
   carries whether it is the default or the fallback AND the model that choice
    picked, so Settings can SHOW what `d`/`f` just did — through `router-primary`,
    which resolves the tag the way the router does, so the machine's only provider
    reads as the default it already is."
  []
  (reset! provider-inventory
    (try
      (let [config
            (vis/load-config)

            fleet
            (provider-fleet config)

            primary
            (router-primary config fleet)

            default-id
            (some-> (:provider-id primary)
                    name)

            fallback-id
            (some-> (:fallback-provider config)
                    name)

            auth-index
            (gateway-auth-index)]

        {:status :ok
         :providers (mapv (fn [provider]
                            (let [pid (some-> (:id provider)
                                              name)]
                              {:provider provider
                               :auth (provider-auth-state provider auth-index)
                               :default? (= default-id pid)
                               :default-model (when (= default-id pid) (:model primary))
                               :fallback? (= fallback-id pid)
                               :fallback-model (when (= fallback-id pid)
                                                 (some-> (:fallback-model config)
                                                         name))}))
                          fleet)
         :error nil})
      (catch Exception e {:status :error :providers [] :error (ex-message e)}))))

(defn- provider-settings-status
  "One provider row's description: the ROUTER TAGS first, then the auth verdict,
   then the provider's own model.

   A router tag names the model it selected (`default → sonnet`), because
   \"default\" alone never told you which model `d` had just bound — and it leads
   the line because a narrow pane truncates the TAIL, while auth already has a
   glyph of its own on the row."
  [{:keys [provider auth default? default-model fallback? fallback-model]}]
  (let [tag (fn [label model]
              (if (str/blank? (str model))
                label
                (str label " " (char 0x2192) " " (vis/model-name model))))]
    (str/join " · "
              (remove str/blank?
                [(when default? (tag "default" default-model))
                 (when fallback? (tag "fallback" fallback-model))
                 (case auth
                   :verified
                   "verified"

                   :rejected
                   "credentials rejected"

                   :degraded
                   "live check unavailable"

                   :unverified
                   "saved, not verified"

                   :local
                   "local, reachable"

                   "not signed in")
                 (str (some-> provider
                              :models
                              first
                              vis/model-name))]))))

(defn- provider-settings-rows
  "The `Providers` settings section: one row per provider — auth state, model,
   default tag on the same line — opening that provider's own transient
   INSIDE this frame, plus one row that adds a new provider. Empty until
   `load-provider-inventory!` has run."
  []
  (let [{:keys [status providers error]} @provider-inventory]
    (when-not (= :unloaded status)
      (vec
        (concat
          [{:type :section :label "Providers"}]
          (mapv (fn [{:keys [provider auth] :as entry}]
                  {:type :provider
                   :label (vis/display-label (:id provider))
                   :description (provider-settings-status entry)
                   :inline-description true
                   :provider provider
                   :auth auth})
                providers)
          (when (seq (str error))
            [{:type :info :tone :bad :label "Providers unavailable" :description (str error)}])
          (when (and (empty? providers) (empty? (str error)))
            (if (= :loading status)
              [{:type :info
                :label "Loading providers…"
                :description "Reading the fleet from the gateway"}]
              [{:type :info
                :label "No providers yet"
                :description "Add one below, or declare them under providers: in vis.yml."}]))
          [{:type :action
            :id :provider-add
            :label "Add provider…"
            :description "Sign in and configure a new one"}])))))

(def ^:private mcp-inventory
  "Cached gateway MCP inventory rendered INSIDE Settings.

   Stays `:unloaded` until a dialog asks for it, so `settings-rows` keeps
   working — and stays MCP-free — for callers and tests without a gateway."
  (atom {:status :unloaded :servers [] :error nil}))

(def ^:dynamic *local-mcp-inventory* nil)

(defn- mcp-inventory-atom [] (or *local-mcp-inventory* mcp-inventory))

(defn load-mcp-inventory!
  "Refresh the cached MCP inventory from the gateway. Never throws: a gateway
   that is down, or a rejected verb, becomes an inline row instead of yet
   another modal."
  []
  (reset! (mcp-inventory-atom) (try {:status :ok
                                     :servers (vec (if *settings-target*
                                                     (vis/gateway-mcp-servers *settings-target*)
                                                     (vis/gateway-mcp-servers)))
                                     :error nil}
                                    (catch Exception e
                                      {:status :error :servers [] :error (ex-message e)}))))

(defn- mcp-settings-rows
  "The `MCP Servers` settings section: one row per server — its live status
   riding the same line — opening that server's own transient INSIDE this
   frame, plus one row that adds a new server. Empty until `load-mcp-inventory!`
   has run."
  []
  (let [{:keys [status servers error]} @(mcp-inventory-atom)]
    (when-not (= :unloaded status)
      (vec
        (concat [{:type :section :label "MCP Servers"}]
                (mapv (fn [row]
                        {:type :mcp
                         :label (str (get row "name"))
                         :description (mcp-model/server-status row)
                         :inline-description true
                         :server (cond-> row
                                   *settings-target*
                                   (assoc "is_scoped" true))})
                      servers)
                (when (seq (str error))
                  [{:type :info :tone :bad :label "MCP unavailable" :description (str error)}])
                (when (and (= :loading status) (empty? servers) (empty? (str error)))
                  [{:type :info
                    :label "Loading MCP servers…"
                    :description "Reading them from the gateway"}])
                [{:type :action
                  :id :mcp-add
                  :label "Add MCP server…"
                  :description "Register a new one with the gateway"}])))))

(defonce ^:private agent-name-setting (atom nil))

(defn- load-agent-name!
  []
  (reset! agent-name-setting (try (vis/setting "agent_name")
                                  (catch Exception e {"error" (ex-message e)}))))

(defn- mark-inventories-loading!
  "Arm every gateway-backed inventory for a refresh WITHOUT clearing what they
   already hold: a re-opened Settings shows the fleet it last read and refreshes
   it in place, and a first open shows a `Loading…` row — never a blank pane and
   never a wait before the frame."
  []
  (swap! provider-inventory assoc :status :loading)
  (swap! (mcp-inventory-atom) assoc :status :loading)
  (swap! (settings-inventory-atom) assoc :status :loading))

(defn- load-inventories!
  "Read the gateway name, settings catalog, MCP inventory and provider fleet in
   parallel. Called only AFTER the settings frame is on the terminal."
  []
  (if *settings-target*
    (do (load-settings-inventory!) (load-mcp-inventory!))
    (let [mcp
          (vis/worker-future "vis-tui-settings-mcp-inventory" load-mcp-inventory!)

          catalog
          (vis/worker-future "vis-tui-settings-catalog" (bound-fn* load-settings-inventory!))]

      (let [agent (vis/worker-future "vis-tui-settings-agent-name" load-agent-name!)]
        (load-provider-inventory!)
        @agent)
      @mcp
      @catalog
      nil)))

(defn- settings-rows
  "Every setting in one flat grouped list: response and theme preferences,
   toggles, providers, and MCP servers. Empty sections are omitted."
  []
  (if *settings-target*
    (vec (concat (or (registry-toggle-rows) [])
                 (when-let [error (:error @(settings-inventory-atom))]
                   [{:type :info :tone :bad :label "Settings unavailable" :description error}])
                 (or (mcp-settings-rows) [])))
    (vec (concat (settings-ui-options)
                 [{:type :section :label "Agent"}
                  {:type :agent-name
                   :label "Agent name"
                   :description (or (get @agent-name-setting "error")
                                    "Shared by all gateway clients. Overrides project names.")}]
                 (or (registry-toggle-rows) [])
                 (or (provider-settings-rows) [])
                 (or (mcp-settings-rows) [])))))

(defn- settings-option-label
  [{:keys [label type toggle-id experimental? locked is-override?]} _values]
  (if (contains? #{:registry-toggle :text-setting :number-setting :structured-setting} type)
    (let [spec (vis/toggle-spec toggle-id)]
      (str label
           (when is-override? "  [Override]")
           (when (if (some? experimental?) experimental? (:experimental? spec)) "  [Experimental]")
           (when locked "  [Locked]")))
    label))

(defn- settings-option-value
  "The current value or short service status, separate from the setting's label."
  [{:keys [key type choices toggle-id toggle-type toggle-value set-key item-id inline-description
           description]} values]
  (case type
    :agent-name
    (or (get @agent-name-setting "value") "Unavailable")

    :choice
    (name (or (get values key) (first choices)))

    (:text-setting :number-setting)
    (str toggle-value)

    :structured-setting
    (let [n (count toggle-value)]
      (str n
           (if (= :array toggle-type)
             (if (= 1 n) " entry" " entries")
             (if (= 1 n) " field" " fields"))))

    :registry-toggle
    (let [spec
          (vis/toggle-spec toggle-id)

          current
          (if (some? toggle-value) toggle-value (vis/toggle-value toggle-id))]

      (if (= :enum (or toggle-type (:type spec)))
        (let [text (or (some-> current
                               name)
                       "")]
          (get {"auto" "Auto" "on" "On" "off" "Off"} text text))
        (if current "On" "Off")))

    :toggle
    (if (get values key false) "On" "Off")

    :set-toggle
    (if (contains? (get values set-key #{}) item-id) "Off" "On")

    (when inline-description (str description))))

(defn- settings-row-mark
  "Leading status glyph + its color for a settings row. Provider rows use
   the daemon's four auth states: green verified, red rejected, yellow degraded,
   and a neutral hollow dot when unverified/off. Returns `[glyph fg-color]`."
  [{:keys [key type set-key item-id toggle-id toggle-type toggle-value server auth]} values]
  (let [on
        [p/STATUS_ON t/status-ok]

        off
        [p/STATUS_OFF t/dialog-hint]

        bad
        [p/STATUS_ON t/status-bad]

        warn
        [p/STATUS_ON t/warning-fg]

        val
        [p/MARK_VALUE t/header-active-tab-accent]

        act
        [p/MARK_ACTION t/header-active-tab-accent]]

    ;; runs an action
    (case type
      :action
      act

      :env-var
      [" " t/dialog-fg]

      :agent-name
      val

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
            (if (some? toggle-value) toggle-value (vis/toggle-value toggle-id))]

        (cond (= :enum (or toggle-type (:type spec))) val
              (boolean tv) on
              :else off))

      ;; an MCP server reads its on/off off the live gateway row
      :mcp
      (if (mcp-model/server-on? server) on off)

      ;; a provider's dot is the GATEWAY's auth verdict, never a local guess
      :provider
      (case auth
        (:verified :local)
        on

        :rejected
        bad

        :degraded
        warn

        off)

      :toggle
      (if (get values key false) on off)

      [" " t/dialog-fg])))

(defn- cycle-choice
  [choices current]
  (let [choices
        (vec choices)

        idx
        (.indexOf ^java.util.List choices current)]

    (nth choices (mod (inc (long (if (neg? idx) 0 idx))) (count choices)))))

(defn- apply-registry-toggle
  [values {:keys [toggle-id toggle-type value]}]
  (try
    (let [kind
          (or toggle-type (:type (vis/toggle-spec toggle-id)))

          remote-row
          (case kind
            :boolean
            (vis/gateway-toggle-setting! toggle-id *settings-target*)

            :enum
            (vis/gateway-set-setting-value! toggle-id value *settings-target*)

            (throw (ex-info "Unsupported registry setting type" {:toggle-id toggle-id :type kind})))

          remote-value
          (case kind
            :boolean
            (if (and (= "boolean" (get remote-row "type")) (contains? remote-row "enabled"))
              (get remote-row "enabled")
              (throw (ex-info "Gateway returned an invalid boolean setting row"
                              {:toggle-id toggle-id})))

            :enum
            (if (and (= "enum" (get remote-row "type")) (contains? remote-row "value"))
              (get remote-row "value")
              (throw (ex-info "Gateway returned an invalid enum setting row"
                              {:toggle-id toggle-id}))))]

      ;; The daemon owns the effective value and atomically changes it. Mirror its
      ;; answer only after success so the Settings glyph never promises a local
      ;; preference the session runtime did not receive: into the catalog every
      ;; row renders from, and into the process registry when this binary reads
      ;; that toggle itself.
      (cache-setting-row! remote-row)
      (when (and (not *settings-target*) (vis/toggle-spec toggle-id))
        (vis/toggle-set-value! toggle-id remote-value))
      values)
    (catch Throwable t
      (vis/notify! (str "Setting was not changed: " (or (ex-message t) "gateway request failed"))
                   :level :error
                   :ttl-ms 5000)
      values)))

(defn- apply-settings-option
  [values {:keys [key type choices set-key item-id] :as row}]
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
    (apply-registry-toggle values row)

    values))

(defn- notify-settings-change!
  [callbacks values]
  (when-let [f (:on-change callbacks)]
    (f values))
  values)

(defn- settings-selectable?
  [{:keys [type]}]
  (contains? #{:toggle :choice :action :agent-name :text-setting :number-setting :structured-setting
               :set-toggle :registry-toggle :mcp :provider}
             type))

(defn- first-selectable-index
  [rows]
  (or (first (keep-indexed (fn [i row]
                             (when (settings-selectable? row) i))
                           rows))
      0))

(defn- settings-initial-index
  "Focus a section's first setting, or its header when the section is empty."
  [rows section]
  (if-let [start (first (keep-indexed
                          (fn [i row]
                            (when (and (= :section (:type row)) (= section (:label row))) i))
                          rows))]
    (let [end (or (first (filter #(= :section (:type (nth rows %)))
                                 (range (inc (long start)) (count rows))))
                  (count rows))]
      (or (first (filter #(settings-selectable? (nth rows %)) (range (inc (long start)) end)))
          start))
    (first-selectable-index rows)))

(defn- move-settings-selection
  [rows ^long selected ^long delta]
  (let [n (count rows)]
    (if (zero? n)
      0
      (loop [idx (p/clamp (+ selected delta) 0 (dec n))]
        (cond (= idx selected) idx
              (settings-selectable? (nth rows idx)) idx
              (and (neg? delta) (zero? idx)) selected
              (and (pos? delta) (= idx (dec n))) selected
              :else (recur (p/clamp (+ idx delta) 0 (dec n))))))))

(defn- settings-page-selection
  "Move by painted settings lines, skipping headings and wrapped descriptions."
  [rows entries selected visible-h direction]
  (let [selectable?
        (fn [{:keys [row-idx part]}]
          (and (= part :option) (settings-selectable? (nth rows row-idx))))

        options
        (filterv selectable? entries)

        selected-idx
        (or (first (keep-indexed (fn [idx entry]
                                   (when (= selected (:row-idx entry)) idx))
                                 options))
            0)

        page-idx
        (page-selected-index entries selected-idx visible-h direction selectable?)]

    (or (:row-idx (nth options page-idx nil)) selected)))

(defn- settings-selection-for-window
  "The row the cursor must take when a scrollbar drag scrolls the settings list to
   paint row `start`. That window is SELECTION-DRIVEN - every paint recomputes
   `scroll` from the selected row - so a drag that moved `scroll` alone is snapped
   straight back by the next frame. Returns the first selectable row whose option
   line lands inside the new window, or nil when the window holds none."
  [rows entries ^long start ^long visible-h]
  (let [n
        (count entries)

        from
        (long (p/clamp start 0 n))

        to
        (long (p/clamp (+ start visible-h) from n))]

    (first (keep (fn [{:keys [row-idx part]}]
                   (when (and (= part :option) (settings-selectable? (nth rows row-idx))) row-idx))
                 (subvec entries from to)))))

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

(defn- theme-picker!
  "Apply themes immediately on single keys in Settings' own transient band.
   Esc closes without reverting the choice. Long catalogs page with n/p.
   apply! repaints the host; capture that fresh frame before the next band
   restores exposed rows, never resurrecting cells from the previous theme."
  [screen g region choices current apply!]
  (let [restore
        (atom (:restore! (host-band-region screen region)))

        region
        (assoc region
          :grid? true
          :restore! (fn [from to]
                      (when-let [f @restore]
                        (f from to))))

        bindings
        "abcdefghijklmoqrstuvwxyz"

        page-size
        (long (p/clamp (- (long (:hint-row region)) (long (or (:min-row region) 0)) 10)
                       1
                       (count bindings)))

        pages
        (max 1 (quot (+ (count choices) (dec page-size)) page-size))]

    (when (seq choices)
      (loop [page
             0

             selected
             current]

        (let [items
              (mapv (fn [i {:keys [theme-id label]}]
                      {:key (str (nth bindings i))
                       :type :action
                       :id theme-id
                       :label (str label (when (= theme-id selected) "  ● current"))})
                    (range)
                    (theme-picker-items (take page-size (drop (* (long page) page-size) choices))))

              spec
              {:title "Theme"
               :escape-label "close"
               :groups
               (cond-> (let [cell-w
                             (+ 9 (long (reduce max 0 (map #(p/display-width (:label %)) items))))

                             columns
                             (max 1 (quot (max 1 (- (long (:inner-w region)) 24)) cell-w))

                             per-column
                             (max 1 (quot (+ (count items) (dec columns)) columns))]

                         (mapv (fn [i column]
                                 {:title (if (zero? (long i))
                                           (str "Themes  " (inc (long page)) "/" pages)
                                           "")
                                  :items (vec column)})
                               (range)
                               (partition-all per-column items)))
                 (> pages 1)
                 (conj
                   {:title "Commands"
                    :items
                    [{:key "n" :type :action :id ::next-theme-page :label "Next page"}
                     {:key "p" :type :action :id ::previous-theme-page :label "Previous page"}]}))}

              action
              (:action (embed-transient! screen g region spec))]

          (cond (nil? action) nil
                (= action ::next-theme-page) (recur (mod (inc (long page)) pages) selected)
                (= action ::previous-theme-page) (recur (mod (dec (long page)) pages) selected)
                :else (do (apply! action)
                          (reset! restore (frame-restorer screen))
                          (recur page action))))))))

(defn- activate-theme-row!
  [screen g region values callbacks {:keys [choices key]}]
  (let [apply! (fn [theme-id]
                 (let [next-values (assoc @values key theme-id)]
                   (reset! values next-values)
                   (notify-settings-change! callbacks next-values)))]
    (theme-picker! screen g region choices (get @values key) apply!)))

(defn- settings-config-property
  "One property's schema in the config document, such as a workspace entry's `access`."
  [definition property]
  (get-in (document/schema-document "config") ["$defs" definition "properties" property]))

(defn- settings-save-key?
  [^KeyStroke key]
  (or (= KeyType/F2 (key-type key))
      (and (= KeyType/Character (key-type key)) (.isCtrlDown key) (= \s (key-character key)))))

(defn- settings-text-editor!
  "Edit real multiline text from its end; read-only text opens at the top.
   F2 accepts it, and Escape protects unsaved text."
  [screen title initial {:keys [read-only? changed? submit-label]}]
  (let [initial
        (or initial "")

        footer
        [["↑/↓" "move"] ["Enter" "new line"] ["F2/Ctrl+S" (or submit-label "Use text")]
         ["Esc" "back"]]]

    (run-modal!
      screen
      {:init {:editor (let [lines (vec (str/split initial #"\n" -1))]
                        (if read-only?
                          {:lines lines :crow 0 :ccol 0}
                          {:lines lines :crow (dec (count lines)) :ccol (count (peek lines))}))
              :scroll 0}
       :measure (fn [_ cols rows]
                  (let [content-w
                        (default-content-width cols)

                        bounds
                        (dialog-bounds cols rows content-w 14)]

                    (merge (dialog-layout bounds)
                           {:cols cols :rows rows :content-w content-w :bounds bounds})))
       :reconcile (fn [{:keys [editor] :as state} {:keys [content-h]}]
                    (let [crow
                          (long (:crow editor))

                          h
                          (long content-h)]

                      (update state
                              :scroll
                              #(cond (< crow (long %)) crow
                                     (>= crow (+ (long %) h)) (max 0 (inc (- crow h)))
                                     :else %))))
       :paint (fn [g {:keys [editor scroll]}
                   {:keys [cols rows content-w bounds content-top content-h hint-row]}]
                (let [{:keys [left inner-w]}
                      bounds

                      w
                      (max 1 (- (long inner-w) 2))

                      crow
                      (long (:crow editor))

                      ccol
                      (long (:ccol editor))

                      offset
                      (max 0 (- ccol (dec w)))

                      current
                      (get (:lines editor) crow "")]

                  (draw-dialog-chrome! g cols rows title content-w 14)
                  (p/set-colors! g t/dialog-fg t/dialog-bg)
                  (doseq [[i line] (map-indexed vector
                                                (take content-h (drop scroll (:lines editor))))]
                    (let [start (min (count line) offset)]
                      (p/put-str! g
                                  (+ (long left) 2)
                                  (+ (long content-top) (long i))
                                  (ellipsize (subs line start) w))))
                  (draw-hint-bar! g left hint-row inner-w footer)
                  (TerminalPosition. (int (+ (long left)
                                             2
                                             (p/display-width (subs current
                                                                    (min offset (count current))
                                                                    (min ccol (count current))))))
                                     (int (+ (long content-top) (- crow (long scroll)))))))
       :on-key (fn [state key _]
                 (let [editor
                       (:editor state)

                       type
                       (key-type key)]

                   (cond (settings-save-key? key) {::done (input/input->text editor)}
                         (= type KeyType/Escape)
                         (if (or read-only?
                                 (and (not changed?) (= initial (input/input->text editor)))
                                 (confirm-dialog! screen
                                                  "Discard text?"
                                                  "The edited text has not been saved."))
                           {::done nil}
                           state)
                         :else (let [edit
                                     (condp = type
                                       KeyType/ArrowUp input/move-up
                                       KeyType/ArrowDown input/move-down
                                       KeyType/ArrowLeft input/move-left
                                       KeyType/ArrowRight input/move-right
                                       KeyType/Home input/move-line-start
                                       KeyType/End input/move-line-end
                                       KeyType/Backspace (when-not read-only? input/delete-backward)
                                       KeyType/Delete (when-not read-only? input/delete-forward)
                                       KeyType/Enter (when-not read-only? input/insert-newline)
                                       nil)]
                                 (cond edit (update state :editor edit)
                                       (and (= type KeyType/Character) (not read-only?))
                                       (assoc state :editor (:state (input/handle-key key editor)))
                                       :else state)))))})))

(defn- settings-list-editor!
  [screen label values]
  (when-let [text (settings-text-editor! screen
                                         (str label " · one entry per line")
                                         (str/join "\n" values)
                                         {})]
    (vec (remove str/blank? (map str/trim (str/split-lines text))))))

(defn- settings-json-editor!
  [screen g region label value]
  (let [original (wire/json-str value)]
    (loop [raw original]
      (when-let [text (settings-text-editor! screen
                                             (str label " · Advanced JSON")
                                             raw
                                             {:changed? (not= raw original)})]
        (let [parsed (wire/parse-json text)]
          (if (some? parsed)
            parsed
            (do (mini-note! screen g region "Invalid JSON" "Keep editing or discard this text.")
                (recur text))))))))

(defn- settings-pick!
  [screen title items]
  (:value (run-modal! screen (select-modal-component title items {:height :content}))))

(defn- settings-path-editor!
  [screen g region original]
  (loop [entries (vec original)]
    (let [items (concat (map-indexed (fn [i entry]
                                       {:label (str (get entry "id") " · " (get entry "path"))
                                        :value i})
                                     entries)
                        [{:label "Add workspace path" :value :add}
                         {:label "Advanced JSON" :value :advanced}
                         {:label "Save these paths" :value :done}])
          selected (settings-pick! screen "Workspace paths" (vec items))]

      (case selected
        nil
        nil

        :done
        entries

        :advanced
        (settings-json-editor! screen g region "Workspace paths" entries)

        :add
        (when-let [path (mini-read! screen
                                    g
                                    region
                                    "Absolute workspace path"
                                    {:placeholder "~/project"})]
          (recur (conj entries {"id" (str "workspace_" (inc (count entries))) "path" path})))

        (let [entry (nth entries selected)
              field (settings-pick! screen
                                    (str "Workspace path · " (get entry "id"))
                                    [{:label "Name" :value "id"} {:label "Path" :value "path"}
                                     {:label "Python name (optional)" :value "python_name"}
                                     {:label "Access" :value "access"}
                                     {:label "Draft policy" :value "draft"}
                                     {:label "Search this path" :value "search"}
                                     {:label "Remove this path" :value :remove}])]

          (cond (= field :remove) (if (mini-confirm! screen
                                                     g
                                                     region
                                                     "Remove this workspace path?"
                                                     {:cost
                                                      "Nothing is saved until you save these paths."
                                                      :yes-label "Remove path"})
                                    (recur (vec (concat (subvec entries 0 selected)
                                                        (subvec entries (inc (long selected))))))
                                    (recur entries))
                (nil? field) (recur entries)
                :else (let [current (get entry field)
                            value (cond (= field "search") (not (if (nil? current) true current))
                                        (#{"access" "draft"} field)
                                        (settings-pick! screen
                                                        (str "Workspace " field)
                                                        (mapv #(hash-map :label % :value %)
                                                              (get (settings-config-property
                                                                     "workspaceEntry"
                                                                     field)
                                                                   "enum")))
                                        :else (mini-read! screen
                                                          g
                                                          region
                                                          (str "Workspace " field)
                                                          {:initial (str current)}))]

                        (if (nil? value)
                          (recur entries)
                          (recur (assoc entries
                                   selected (if (and (= field "python_name") (str/blank? value))
                                              (dissoc entry field)
                                              (assoc entry field value))))))))))))

(defn- settings-records-editor!
  [screen g region label definition summary-key fields value]
  (loop [entries (vec (or value []))]
    (let [items (concat (map-indexed (fn [index entry]
                                       {:label (str (inc (long index)) ". " (get entry summary-key))
                                        :value index})
                                     entries)
                        [{:label (str "Add " (str/lower-case label)) :value :add}
                         {:label "Advanced JSON" :value :advanced}
                         {:label "Use these rules" :value :done}])
          selected (settings-pick! screen label (vec items))]

      (case selected
        nil
        nil

        :done
        entries

        :advanced
        (settings-json-editor! screen g region label entries)

        :add
        (when-let [text (mini-read! screen g region (str label " · " summary-key) {})]
          (recur (conj entries {summary-key text})))

        (let [entry (nth entries selected)
              field (settings-pick! screen
                                    label
                                    (conj fields {:label "Remove this rule" :value :remove}))]

          (cond (nil? field) (recur entries)
                (= field :remove) (if (mini-confirm!
                                        screen
                                        g
                                        region
                                        "Remove this rule?"
                                        {:cost "Nothing is saved until you save this configuration."
                                         :yes-label "Remove rule"})
                                    (recur (vec (concat (subvec entries 0 selected)
                                                        (subvec entries (inc (long selected))))))
                                    (recur entries))
                :else
                (let [property (settings-config-property definition field)
                      old (get entry field)
                      next-value
                      (cond
                        (get property "enum") (settings-pick! screen
                                                              (str label " · " field)
                                                              (mapv #(hash-map :label % :value %)
                                                                    (get property "enum")))
                        (= field "allow") (settings-records-editor!
                                            screen
                                            g
                                            region
                                            "Allowed requests"
                                            "networkRuleAllow"
                                            "method"
                                            [{:label "Method" :value "method"}
                                             {:label "Path (optional)" :value "path"}]
                                            old)
                        (= field "ports")
                        (when-let [ports (settings-list-editor! screen "Host ports" (map str old))]
                          (let [parsed (mapv wire/parse-json ports)
                                bounds (get property "items")]

                            (if (every? #(and (integer? %)
                                              (<= (get bounds "minimum") % (get bounds "maximum")))
                                        parsed)
                              parsed
                              (do (mini-note! screen
                                              g
                                              region
                                              "Invalid ports"
                                              "Use one valid integer port per line.")
                                  nil))))
                        (= "array" (get property "type"))
                        (settings-list-editor! screen (str label " · " field) (or old []))
                        :else
                        (mini-read! screen g region (str label " · " field) {:initial (str old)}))]

                  (if (nil? next-value)
                    (recur entries)
                    (recur (assoc entries
                             selected (if (and (string? next-value)
                                               (str/blank? next-value)
                                               (not= field summary-key))
                                        (dissoc entry field)
                                        (assoc entry field next-value))))))))))))

(defn- settings-object-editor!
  [screen g region row]
  (let [network?
        (= "network" (get-in row [:setting "editor"]))

        fields
        (if network?
          [{:label "Allowed domains" :value "allowed_domains"}
           {:label "Denied domains" :value "denied_domains"}
           {:label "Domains outside the proxy" :value "exclude_domains"}
           {:label "Private network access" :value "allow_private"}
           {:label "Inbound ports" :value "inbound_ports"} {:label "Host rules" :value "rules"}]
          [{:label "Allowed paths" :value "allow"} {:label "Blocked reads" :value "deny_read"}
           {:label "Blocked writes" :value "deny_write"}])]

    (loop [value (or (:toggle-value row) {})]
      (let [field (settings-pick! screen
                                  (:label row)
                                  (vec (concat fields
                                               [{:label "Advanced JSON" :value :advanced}
                                                {:label "Save this configuration" :value :done}])))]
        (case field
          nil
          nil

          :done
          value

          :advanced
          (settings-json-editor! screen g region (:label row) value)

          (let [old (get value field)
                next-value
                (cond (= field "allow_private") (not old)
                      (= field "rules")
                      (settings-records-editor!
                        screen
                        g
                        region
                        "Host rules"
                        "networkRule"
                        "host"
                        [{:label "Host" :value "host"} {:label "Access" :value "access"}
                         {:label "Methods" :value "methods"} {:label "Ports" :value "ports"}
                         {:label "Allowed requests" :value "allow"}]
                        old)
                      (= field "inbound_ports")
                      (when-let [ports (settings-list-editor! screen "Inbound ports" (map str old))]
                        (let [parsed (mapv wire/parse-json ports)]
                          (if (every? integer? parsed)
                            parsed
                            (do (mini-note! screen
                                            g
                                            region
                                            "Invalid ports"
                                            "Use one integer port per line.")
                                nil))))
                      :else (settings-list-editor! screen (str/replace field "_" " ") (or old [])))]

            (recur (if (some? next-value) (assoc value field next-value) value))))))))

(defn- settings-structured-editor!
  [screen g region row]
  (case (get-in row [:setting "editor"])
    "paths"
    (settings-path-editor! screen g region (:toggle-value row))

    ("filesystem" "network")
    (settings-object-editor! screen g region row)

    "list"
    (settings-list-editor! screen (:label row) (:toggle-value row))

    (settings-json-editor! screen g region (:label row) (:toggle-value row))))

(defn- pick-setting-value!
  "Open the enum's choice list on its saved value; Escape leaves it unchanged."
  [screen {:keys [label toggle-id choices toggle-value]}]
  (let [choices
        (vec (or (seq choices) (:choices (vis/toggle-spec toggle-id))))

        current
        (if (some? toggle-value) toggle-value (vis/toggle-value toggle-id))

        items
        (mapv (fn [choice]
                (cond-> {:label choice :value choice}
                  (= current choice)
                  (assoc :hint "current")))
              choices)

        component
        (select-modal-component label items {:height :content})]

    (:value (run-modal! screen
                        (assoc-in component
                          [:init :selected]
                          (max 0 (.indexOf ^java.util.List choices current)))))))

(defn- save-setting-value!
  "Save one value for the open owner, then show the row the gateway confirmed."
  [screen g region id value]
  (try (cache-setting-row! (vis/set-setting-value! id value *settings-target*))
       (load-settings-inventory!)
       (catch Exception e (mini-note! screen g region "Setting not saved" (ex-message e)))))

(defn- setting-number
  "The finite number `text` names, or nil. A whole number stays an integer."
  [text]
  (let [text (str/trim (str text))]
    (or (parse-long text)
        (when-let [number (parse-double text)]
          (when (Double/isFinite (double number)) number)))))

(defn- activate-unlocked-row!
  [^TerminalScreen screen g region values callbacks row]
  (case (:type row)
    :text-setting
    (when-let [value (mini-read! screen
                                 g
                                 (host-band-region screen region)
                                 (:label row)
                                 {:initial (:toggle-value row)})]
      (save-setting-value! screen g region (:toggle-id row) value))

    :number-setting
    (loop [text (str (:toggle-value row))]
      (when-let [value (mini-read! screen
                                   g
                                   (host-band-region screen region)
                                   (:label row)
                                   {:initial text})]
        (if-let [number (setting-number value)]
          (save-setting-value! screen g region (:toggle-id row) number)
          (do (mini-note! screen
                          g
                          region
                          "Invalid number"
                          "Enter a finite number. Your text is kept.")
              (recur value)))))

    :structured-setting
    (when-let [value (settings-structured-editor! screen g region row)]
      (save-setting-value! screen g region (:toggle-id row) value))

    :inherit
    (try (cache-setting-row! (vis/inherit-setting! (:toggle-id row) *settings-target*))
         (load-settings-inventory!)
         (catch Exception e (mini-note! screen g region "Setting not reset" (ex-message e))))

    :agent-name
    (let [region (host-band-region screen region)]
      (try (let [current (vis/setting "agent_name")]
             (reset! agent-name-setting current)
             (when-let [value (mini-read! screen
                                          g
                                          region
                                          "Agent name (all gateway clients):"
                                          {:initial (get current "value")})]
               (reset! agent-name-setting (vis/set-setting-value! "agent_name" value))))
           (catch Exception e (mini-note! screen g region "Agent name not saved" (ex-message e)))))

    :registry-toggle
    (let [enum?
          (= :enum (or (:toggle-type row) (:type (vis/toggle-spec (:toggle-id row)))))

          current
          (if (some? (:toggle-value row)) (:toggle-value row) (vis/toggle-value (:toggle-id row)))

          value
          (when enum? (pick-setting-value! screen row))]

      (when (or (not enum?) (and value (not= value current)))
        (->> (swap! values apply-settings-option (assoc row :value value))
             (notify-settings-change! callbacks))
        ;; A feature flag can reveal or hide dependent rows — Improve mode shows
        ;; only while Improve is on — exactly as it does in the app, so re-read
        ;; the catalog it just changed instead of waiting for the next open.
        (when (and (:experimental? row) (seq (:groups @(settings-inventory-atom))))
          (load-settings-inventory!))))

    :action
    (when-let [f (get callbacks (:id row))]
      ;; An action gets the SAME frame handle a provider row gets, so it can
      ;; paint its own transient band inside Settings instead of stacking a
      ;; dialog on top of it.
      (let [result (f {:values @values :g g :region region})]
        ;; Adding an entry changes what every row under it says; re-read that
        ;; inventory instead of trusting the cached one.
        (when (= :mcp-add (:id row)) (load-mcp-inventory!))
        (when (= :provider-add (:id row)) (load-provider-inventory!))
        result))

    ;; An MCP row IS its verbs — start, kill, enable, disable, sign in, edit,
    ;; remove — offered as a transient band in THIS frame, each on the key
    ;; the verb itself carries, and every question that verb then asks (a field,
    ;; the save confirm, a gateway refusal) lands in the SAME band, on the region
    ;; snapshotted here BEFORE anything paints over the settings list.
    :mcp
    (let [region (host-band-region screen region)]
      (when-let [action (:action (embed-transient! screen
                                                   g
                                                   region
                                                   (mcp-model/server-transient-spec (:server
                                                                                      row))))]
        (when-let [f (:mcp-action callbacks)]
          (f {:server (:server row) :action action :g g :region region}))
        (load-mcp-inventory!))
      @values)

    ;; Same for a provider row: default, fallback, sign in, status, remove are
    ;; a transient here, and picking a model is a second one — never a dialog.
    :provider
    (do (when-let [f (:provider-transient callbacks)]
          (f {:provider-id (:id (:provider row)) :g g :region region})
          (load-provider-inventory!))
        @values)

    (if (= :theme-name (:key row))
      (activate-theme-row! screen g region values callbacks row)
      (->> (swap! values apply-settings-option row)
           (notify-settings-change! callbacks)))))

(defn- activate-settings-row!
  "Activate one Settings row. A row that a more specific scope decides for the
   session Settings opened from explains where to change it instead."
  [^TerminalScreen screen g region values callbacks row]
  (if-let [note (:locked row)]
    (mini-note! screen g region "Locked" note)
    (activate-unlocked-row! screen g region values callbacks row)))

(defn- settings-section-text
  [label inner-w]
  (let [prefix
        (str "── " label " ")

        available
        (max 0 (- (long inner-w) 2))

        filler
        (apply str (repeat (max 0 (- (long available) (p/display-width prefix))) \─))]

    (ellipsize (str prefix filler) available)))

(defn- settings-option-indent [] t/settings-option-indent)

(defn- settings-subsection-text
  [label inner-w]
  (ellipsize (str "◆ " label) (max 0 (- (long inner-w) 2))))

(defn- settings-wrap-lines
  [s w]
  (let [w
        (max 1 (long w))

        s
        (str/trim (str (or s "")))]

    (if (str/blank? s) [] (vec (remove str/blank? (render/wrap-text s w))))))

(defn- settings-render-entries
  "One line per setting. Only section guidance, empty states and errors wrap."
  [rows desc-w]
  (vec
    (mapcat (fn [idx {:keys [type label description]}]
              (case type
                :section
                (into [{:row-idx idx :part :section}]
                      (when (= "Extension engines" label)
                        (mapv (fn [line]
                                {:row-idx idx :part :info-line :text line})
                              (settings-wrap-lines
                                "Auto — when applicable. On — always active. Off — tools disabled."
                                desc-w))))

                :subsection
                [{:row-idx idx :part :subsection}]

                :info
                (into (mapv (fn [line]
                              {:row-idx idx :part :info-line :text line :head? true})
                            (or (seq (settings-wrap-lines label desc-w)) [""]))
                      (mapv (fn [line]
                              {:row-idx idx :part :info-line :text line})
                            (settings-wrap-lines description desc-w)))

                [{:row-idx idx :part :option}]))
            (range)
            rows)))

(defn- settings-header-row? [{:keys [type]}] (contains? #{:section :subsection} type))

(defn- settings-row-search-text
  "Lowercased haystack for a row's search match: its label + description."
  [{:keys [label description source]}]
  (str/lower-case (str label " " description " " source)))

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
              (or (first (filter #(and (> (long %) (long i)) (pred (nth rows %))) (range n))) n))

            headers
            (for [i
                  (range n)

                  :let [row
                        (nth rows i)]
                  :when
                  (case (:type row)
                    :section
                    (some matched (range (inc (long i)) (next-idx i #(= :section (:type %)))))

                    :subsection
                    (some matched (range (inc (long i)) (next-idx i settings-header-row?)))

                    false)]

              i)

            keep
            (into matched headers)]

        (vec (keep-indexed (fn [i row]
                             (when (contains? keep i) row))
                           rows))))))

(defn- settings-toc
  "Sections in catalog order, with their bounds, setting counts and active state."
  [rows selected]
  (let [rows
        (vec rows)

        starts
        (filterv #(= :section (:type (nth rows %))) (range (count rows)))]

    (mapv (fn [k start]
            (let [end
                  (or (get starts (inc (long k))) (count rows))

                  label
                  (:label (nth rows start))]

              {:label label
               :count (count (filter settings-selectable? (subvec rows start end)))
               :start start
               :end end
               :active? (<= start selected (dec (long end)))}))
          (range)
          starts)))

(defn- settings-details-lines
  [row values]
  (vec (concat (when-some [value (settings-option-value row values)]
                 [(str "Value: " value)])
               [(str "Source: "
                     (or (:source row)
                         (when (= :agent-name (:type row)) "gateway")
                         (when (:toggle-id row) "unavailable")
                         "this terminal")) "" (:description row)]
               (when (:is-override? row) ["" "This scope overrides the inherited value."])
               (when-let [note (:locked row)]
                 ["" note]))))

(defn- settings-details-dialog!
  "Show full metadata on demand. Return :change, :inherit, or nil without saving."
  [screen row values]
  (let [lines
        (settings-details-lines row values)

        title
        (str (:label row) " · Details")

        editable?
        (not (:locked row))

        inherit?
        (and editable? (:is-override? row))

        footer
        (cond-> [["↑/↓" "scroll"]]
          editable?
          (conj ["Enter" "change"])

          inherit?
          (conj ["i" "inherit"])

          true
          (conj ["Esc" "back"]))]

    (run-modal!
      screen
      {:init {:scroll 0}
       :measure (fn [_ cols rows]
                  (let [content-w
                        (footer-content-width cols footer 52)

                        text-w
                        (max 1 (- (long (:inner-w (dialog-bounds cols rows content-w 8))) 3))

                        wrapped
                        (vec (mapcat #(if (str/blank? %) [""] (render/wrap-text % text-w)) lines))

                        content-h-req
                        (max 8 (count wrapped))

                        bounds
                        (dialog-bounds cols rows content-w content-h-req)

                        layout
                        (dialog-layout bounds)]

                    (merge {:cols cols
                            :rows rows
                            :content-w content-w
                            :content-h-req content-h-req
                            :bounds bounds
                            :lines wrapped
                            :text-w text-w}
                           layout)))
       :reconcile
       (fn [state {:keys [lines content-h]}]
         (update state :scroll #(p/clamp % 0 (max 0 (- (count lines) (long content-h))))))
       :paint (fn [g {:keys [scroll]}
                   {:keys [cols rows content-w content-h-req bounds lines text-w content-top
                           content-h hint-row]}]
                (let [{:keys [left inner-w]} bounds]
                  (draw-dialog-chrome! g cols rows title content-w content-h-req)
                  (p/set-colors! g t/dialog-fg t/dialog-bg)
                  (doseq [[i line] (map-indexed vector (take content-h (drop scroll lines)))]
                    (p/put-str! g
                                (+ (long left) 2)
                                (+ (long content-top) (long i))
                                (ellipsize line text-w)))
                  (ScrollBar/draw g
                                  Direction/VERTICAL
                                  (TerminalPosition. (int (+ (long left) (long inner-w)))
                                                     (int content-top))
                                  (int content-h)
                                  (int (count lines))
                                  (int content-h)
                                  (Integer/valueOf (int scroll))
                                  t/dialog-border
                                  t/dialog-bg
                                  t/dialog-hint-key
                                  t/dialog-bg)
                  (draw-hint-bar! g left hint-row inner-w footer))
                nil)
       :on-key (fn [state key {:keys [lines content-h]}]
                 (let [max-scroll
                       (max 0 (- (count lines) (long content-h)))

                       move
                       (fn [delta]
                         (update state :scroll #(p/clamp (+ (long %) (long delta)) 0 max-scroll)))]

                   (if-let [step (ScrollBar/wheelStep ^KeyStroke key)]
                     (move step)
                     (condp = (key-type key)
                       KeyType/Escape {::done nil}
                       KeyType/Enter (if editable? {::done :change} state)
                       KeyType/ArrowUp (move -1)
                       KeyType/ArrowDown (move 1)
                       KeyType/PageUp (move (- (long content-h)))
                       KeyType/PageDown (move content-h)
                       KeyType/Home (assoc state :scroll 0)
                       KeyType/End (assoc state :scroll max-scroll)
                       KeyType/Character
                       (if (and inherit? (= \i (key-character key))) {::done :inherit} state)
                       state))))})))

(defn- settings-pane-geometry
  "Use a TOC rail only when its 14 columns, divider, and a useful 17-column
   settings pane all fit. Narrow dialogs become one pane instead of painting
   a forced sidebar through their right edge."
  [left inner-w]
  (let [left
        (long left)

        inner-w
        (max 1 (long inner-w))

        split?
        (>= inner-w (+ 14 1 17))

        rail-w
        (if split? (p/clamp (quot inner-w 3) 14 22) 0)]

    {:split? split?
     :rail-w rail-w
     :pane-left (if split? (+ left rail-w 1) left)
     :pane-width (if split? (- inner-w rail-w 1) inner-w)}))

(defn- settings-pointer-target
  "Map a primary pointer press to a painted setting or section scroll target."
  [key rows entries scroll {:keys [split? left rail-w pane-left pane-width list-top visible-h toc]}]
  (or (when split?
        (when-let [offset (mouse-row-offset key
                                            (inc (long left))
                                            list-top
                                            rail-w
                                            (min (count toc) (long visible-h)))]
          {:kind :toc :row-idx (settings-initial-index rows (:label (nth toc offset)))}))
      (when-let [offset (mouse-row-offset key (inc (long pane-left)) list-top pane-width visible-h)]
        (let [entry-idx (+ (long scroll) (long offset))]
          (when-let [{:keys [row-idx]} (get entries entry-idx)]
            (when (settings-selectable? (nth rows row-idx)) {:kind :setting :row-idx row-idx}))))))

(defn settings-dialog!
  "Show the settings dialog.

   Show the full catalog as one scrollable list, with a section sidebar when space
   permits. Each setting uses one line with its current value. Arrows, the mouse
   wheel and paging cross section boundaries. Sidebar clicks scroll to a section.
   Enter or a primary pointer click changes a setting. F1 opens its description,
   source and inherited-value action. Search matches the whole catalog, including
   descriptions hidden from the list.

   `settings` is the persisted TUI settings map (see
   `state/default-settings`). `callbacks` also carries `:focus-section` (a
   section label to park the cursor on, e.g. `MCP Servers` or `Providers`),
   `:mcp-add` / `:provider-add` (the add row of each section), `:mcp-action`
   (the verb a server's transient fired) and `:provider-transient` (one
   provider's transient, handed the graphics and the region it paints into).
   `:context-session-id` names the session whose more specific settings lock the
   rows they decide.
   Esc clears an active search first, then closes and returns the
   current settings map."
  ([^TerminalScreen screen settings] (settings-dialog! screen settings nil))
  ([^TerminalScreen screen settings callbacks]
   (with-modal-background
     screen
     (binding [*settings-target*
               (:settings-target callbacks)

               *settings-context*
               (:context-session-id callbacks)

               *local-settings-inventory*
               (when (:settings-target callbacks) (atom {:status :unloaded :groups [] :error nil}))

               *local-mcp-inventory*
               (when (:settings-target callbacks)
                 (atom {:status :unloaded :servers [] :error nil}))]

       (let [;; MCP servers and providers are settings sections now, so both
             ;; inventories are read once per open instead of from behind dialogs of
             ;; their own — but NOT here. Opening Settings costs one paint, never a
             ;; gateway round trip (a daemon that still has to start takes seconds; a
             ;; gateway on another machine costs an RTT per provider). The loop reads
             ;; them once its first frame is on the terminal.
             _
             (mark-inventories-loading!)

             inventories-pending
             (volatile! true)

             selected
             (atom (settings-initial-index (settings-rows) (:focus-section callbacks)))

             toc-scroll
             (atom 0)

             scroll
             (atom 0)

             values
             (atom (or settings {}))

             scrollbar-drag-offset
             (volatile! nil)

             pointer-down-target
             (volatile! nil)

             query
             (atom "")

             ;; One status glyph and a gap precede each compact setting label.
             check-w
             2]

         ;; A live change can repaint the chat behind this modal. Reuse the SAME
         ;; settings paint without reading input or flushing a band-less frame.
         ((fn paint-settings! [paint-only?]
            (loop []

              (let [filtered
                    (filter-settings-rows (settings-rows) @query)

                    rows
                    (if (and (empty? filtered) (not (str/blank? @query)))
                      [{:type :info
                        :label "No matching settings"
                        :description "Try another search."}]
                      filtered)

                    _
                    (swap! selected (fn [current]
                                      (let [index (p/clamp current 0 (max 0 (dec (count rows))))]
                                        (if (and (seq rows)
                                                 (let [row (nth rows index)]
                                                   (or (settings-selectable? row)
                                                       (and (= :section (:type row))
                                                            (= index
                                                               (settings-initial-index rows
                                                                                       (:label
                                                                                         row)))))))
                                          index
                                          (first-selectable-index rows)))))

                    toc
                    (settings-toc rows @selected)

                    size
                    (modal-size! screen)

                    cols
                    (.getColumns size)

                    screen-rows
                    (.getRows size)

                    g
                    (frame/surface-graphics screen cols screen-rows)

                    bounds
                    (draw-dialog-chrome! g
                                         cols
                                         screen-rows
                                         (if *settings-target*
                                           (str (titleize-label (:scope *settings-target*))
                                                " settings: "
                                                (or (:label @(settings-inventory-atom))
                                                    (:label *settings-target*)
                                                    (:target-id *settings-target*)))
                                           "Settings")
                                         (settings-content-width cols)
                                         (settings-content-height screen-rows))

                    {:keys [left inner-w]}
                    bounds

                    left
                    (long left)

                    inner-w
                    (long inner-w)

                    ;; Wide Settings is a TOC rail + divider + settings pane. Narrow
                    ;; Settings collapses to one pane; forcing the 14-column rail was
                    ;; what let content cross the dialog's right border.
                    {:keys [split? rail-w pane-left pane-width]}
                    (if (seq toc)
                      (settings-pane-geometry left inner-w)
                      {:split? false :rail-w 0 :pane-left left :pane-width inner-w})

                    rail-w
                    (long rail-w)

                    lleft
                    (long pane-left)

                    linner
                    (long pane-width)

                    {:keys [content-top content-h hint-row]}
                    (dialog-layout bounds)

                    content-top
                    (long content-top)

                    content-h
                    (long content-h)

                    search-row
                    content-top

                    list-top
                    (+ content-top 2)

                    visible-h
                    (max 1 (- content-h 2))

                    _
                    (swap! toc-scroll #(visible-window-start
                                         (or (first (keep-indexed (fn [i entry]
                                                                    (when (:active? entry) i))
                                                                  toc))
                                             0)
                                         %
                                         visible-h
                                         (count toc)))

                    visible-toc
                    (subvec toc @toc-scroll (min (count toc) (+ (long @toc-scroll) visible-h)))

                    option-indent
                    (long (settings-option-indent))

                    option-x
                    (+ lleft 2 option-indent)

                    labels
                    (mapv #(settings-option-label % @values) rows)

                    option-values
                    (mapv #(settings-option-value % @values) rows)

                    base-paint-w
                    linner

                    base-option-w
                    (max 1 (- base-paint-w 2 option-indent))

                    base-desc-w
                    (max 1 (- base-option-w check-w))

                    base-entries
                    (settings-render-entries rows base-desc-w)

                    scrollable?
                    (> (count base-entries) visible-h)

                    paint-w
                    (if scrollable? (max 1 (dec linner)) linner)

                    option-w
                    (max 1 (- paint-w 2 option-indent))

                    desc-x
                    (+ option-x check-w)

                    desc-w
                    (max 1 (- option-w check-w))

                    ;; Keep short labels readable without dropping meaningful service status.
                    value-w
                    (min (max 0
                              (- option-w
                                 p/STATUS_WIDTH
                                 2
                                 (min 16 (long (reduce max 0 (map p/display-width labels))))))
                         (long (reduce max 0 (map #(p/display-width (str %)) option-values))))

                    entries
                    (settings-render-entries rows desc-w)

                    visual-n
                    (count entries)

                    sel-entry-idxs
                    (keep-indexed (fn [entry-idx {:keys [row-idx]}]
                                    (when (= row-idx @selected) entry-idx))
                                  entries)

                    selected-visual
                    (long (or (first sel-entry-idxs) 0))

                    ;; Section guidance can wrap; selectable rows remain single-line.
                    selected-visual-end
                    (long (or (last sel-entry-idxs) selected-visual))

                    ;; Visual index where the intro rows (section / subsection /
                    ;; info-line) that directly precede the selected option begin.
                    ;; The scroll window is selection-driven, so without this the
                    ;; first option pins itself to the top and its SECTION HEADER
                    ;; (a non-selectable row above it) is clipped forever — you can
                    ;; scroll to the first setting but never see its header.
                    header-start
                    (long (loop [i (dec selected-visual)]
                            (if (and (>= i 0)
                                     (contains? #{:section :subsection :info-line}
                                                (:part (nth entries i))))
                              (recur (dec i))
                              (inc i))))

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
                ;; switcher (`navigator-dialog!`) draw under their query fields. On a
                ;; split layout, `┬` joins the rail divider beginning below it.
                (p/set-colors! g t/dialog-border t/dialog-bg)
                (p/draw-separator! g left (+ left inner-w 1) (inc content-top))
                (when split? (p/put-str! g lleft (inc content-top) "┬"))
                (dotimes [i visible-h]
                  (let [entry-idx (+ (long @scroll) i)
                        row-y (+ list-top i)]

                    (if (< entry-idx visual-n)
                      (let [{:keys [row-idx part text head?]} (nth entries entry-idx)
                            {:keys [label tone]} (nth rows row-idx)
                            option-label (nth labels row-idx)
                            selected? (= row-idx @selected)
                            [mark mark-color] (settings-row-mark (nth rows row-idx) @values)]

                        (case part
                          :section
                          (do (p/set-colors! g t/dialog-border t/dialog-bg)
                              (p/fill-rect! g (inc lleft) row-y paint-w 1)
                              (p/put-str! g (+ lleft 2) row-y (settings-section-text label paint-w))
                              (p/set-fg! g t/dialog-hint-key)
                              (p/styled g
                                        [p/BOLD]
                                        (p/put-str! g
                                                    (+ lleft 5)
                                                    row-y
                                                    (ellipsize label (max 0 (- paint-w 4))))))

                          :subsection
                          (do (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                              (p/fill-rect! g (inc lleft) row-y paint-w 1)
                              (p/styled g
                                        [p/BOLD]
                                        (p/put-str! g
                                                    (+ lleft 2)
                                                    row-y
                                                    (settings-subsection-text label paint-w))))

                          ;; Prose ABOUT the section (empty state, gateway error): a
                          ;; bold head line plus its own wrapped body, both in the
                          ;; description column so the block hangs off the section
                          ;; instead of running along the pane edge as one sentence.
                          :info-line
                          (do (p/set-colors! g
                                             (cond (and head? (= :bad tone)) t/status-bad
                                                   head? t/dialog-fg
                                                   :else t/dialog-hint)
                                             t/dialog-bg)
                              (p/fill-rect! g (inc lleft) row-y paint-w 1)
                              (if head?
                                (p/styled g
                                          [p/BOLD]
                                          (p/put-str! g desc-x row-y (ellipsize text desc-w)))
                                (p/put-str! g desc-x row-y (ellipsize text desc-w))))

                          ;; Highlight the setting while its status and value keep their meaning.
                          (p/styled
                            g
                            (p/selection-styles selected?)
                            (p/set-colors! g t/dialog-fg t/dialog-bg)
                            (p/fill-rect! g (inc lleft) row-y paint-w 1)
                            ;; The leading status glyph reports the current setting value.
                            (let [label-x
                                  (p/status-mark! g option-x row-y mark mark-color t/dialog-bg)
                                  value (nth option-values row-idx)
                                  label-w
                                  (max 1
                                       (- option-w
                                          p/STATUS_WIDTH
                                          (if (and (some? value) (pos? value-w)) (+ value-w 2) 0)))
                                  lbl (ellipsize option-label label-w)]

                              (p/set-colors! g t/dialog-fg t/dialog-bg)
                              (p/put-str! g label-x row-y lbl)
                              (when (and (some? value) (pos? value-w))
                                (let [text (ellipsize value value-w)
                                      dx (- (+ lleft paint-w) (p/display-width text))]

                                  (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                                  (p/put-str! g dx row-y text)))))))
                      (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                          (p/fill-rect! g (inc lleft) row-y paint-w 1)))))
                ;; Wide-only Table-of-Contents rail. Painted AFTER the settings pane so
                ;; its divider cannot be overwritten by a pane fill.
                (when split?
                  (let [toc visible-toc]
                    (p/set-colors! g t/dialog-border t/dialog-bg)
                    (doseq [ry (range list-top (+ content-top content-h))]
                      (p/put-str! g lleft ry "│"))
                    (dotimes [i (min (count toc) visible-h)]
                      (let [{lbl :label cnt :count active? :active?} (nth toc i)
                            ry (+ list-top i)
                            rail-x (inc left)
                            cstr (str cnt)
                            lbl-w (max 1 (- rail-w 2 (count cstr) 1))
                            bg (if active? t/header-active-tab-bg t/dialog-bg)
                            fg (if active? t/header-active-tab-fg t/dialog-fg)]

                        (p/set-colors! g fg bg)
                        (p/fill-rect! g rail-x ry rail-w 1)
                        (if active?
                          (p/styled g [p/BOLD] (p/put-str! g (inc rail-x) ry (ellipsize lbl lbl-w)))
                          (p/put-str! g (inc rail-x) ry (ellipsize lbl lbl-w)))
                        (p/set-colors! g (if active? t/header-active-tab-fg t/dialog-hint) bg)
                        (p/put-str! g (- (+ rail-x rail-w) (count cstr) 1) ry cstr)))))
                (ScrollBar/draw g
                                Direction/VERTICAL
                                (TerminalPosition. (int (+ lleft linner)) (int list-top))
                                (int visible-h)
                                (int visual-n)
                                (int visible-h)
                                (when (some? @scroll) (Integer/valueOf (int @scroll)))
                                t/dialog-border
                                t/dialog-bg
                                t/dialog-hint-key
                                t/dialog-bg)
                (draw-hint-bar! g
                                left
                                hint-row
                                inner-w
                                (if (< inner-w 50)
                                  [["↑/↓" "scroll"] ["F1" "details"] ["Esc" "clear/close"]]
                                  [["↑/↓" "scroll"] ["PgUp/PgDn" "scroll"] ["Enter" "change"]
                                   ["F1" "details"] ["Esc" "clear/close"]]))
                (when-not paint-only?
                  (.setCursorPosition screen search-cursor)
                  (frame/refresh! screen))
                (when-not paint-only?
                  (if @inventories-pending
                    ;; The frame is ON the terminal now — only then pay for the gateway,
                    ;; and repaint into the dialog the user is already looking at.
                    ;; Refocus the requested section after the first inventory answer.
                    (do (vreset! inventories-pending false)
                        (load-inventories!)
                        (reset! selected (settings-initial-index (settings-rows)
                                                                 (:focus-section callbacks)))
                        (reset! scroll 0)
                        (recur))
                    (let [key
                          (read-modal-key! screen)

                          selected-row
                          (let [row (get rows @selected)]
                            (when (settings-selectable? row) row))

                          activate-row!
                          (fn [row]
                            (activate-settings-row! screen
                                                    g
                                                    {:left left
                                                     :inner-w inner-w
                                                     :hint-row hint-row
                                                     :text-w (max 1 (- (long inner-w) 2))
                                                     :min-row list-top
                                                     ;; One snapshot per activation: a shorter band gives the
                                                     ;; rows a taller one covered back to the list itself.
                                                     :restore! (frame-restorer screen)}
                                                    values
                                                    (assoc callbacks
                                                      :on-change
                                                      (fn [settings]
                                                        (notify-settings-change! callbacks settings)
                                                        (paint-settings! true)))
                                                    row))]

                      (when key
                        (cond
                          (instance? MouseAction key)
                          (if-let [step (ScrollBar/wheelStep ^KeyStroke key)]
                            ;; Mouse wheel anywhere in the dialog — selection follows
                            ;; the wheel direction so the cursor stays in the visible
                            ;; window without having to chase it with arrow keys.
                            (do (vreset! pointer-down-target nil)
                                (swap! selected #(move-settings-selection rows % step))
                                (recur))
                            (let [was-dragging? (some? @scrollbar-drag-offset)
                                  ^ScrollBar$DragResult drag
                                  (ScrollBar/dragStep
                                    ^MouseAction key
                                    Direction/VERTICAL
                                    (TerminalPosition. (int (+ lleft linner)) (int list-top))
                                    (int visible-h)
                                    (int visual-n)
                                    (int visible-h)
                                    (Integer/valueOf (int @scroll))
                                    (when (some? @scrollbar-drag-offset)
                                      (Integer/valueOf (int @scrollbar-drag-offset)))
                                    1)
                                  action (.getActionType ^MouseAction key)
                                  pointer-target (settings-pointer-target key
                                                                          rows
                                                                          entries
                                                                          @scroll
                                                                          {:split? split?
                                                                           :left left
                                                                           :rail-w rail-w
                                                                           :pane-left lleft
                                                                           ;; `paint-w` excludes the scrollbar cell.
                                                                           :pane-width paint-w
                                                                           :list-top list-top
                                                                           :visible-h visible-h
                                                                           :toc visible-toc})
                                  scrollbar-interaction? (or was-dragging?
                                                             (and drag (not (.release drag))))]

                              ;; A release belongs to the scrollbar only when a drag was armed.
                              (when (and drag (.release drag)) (vreset! scrollbar-drag-offset nil))
                              (when-let [grip (and drag (.gripOffset drag))]
                                (vreset! scrollbar-drag-offset (long grip)))
                              (when-let [s (and drag (.scrollPosition drag))]
                                (reset! scroll (long s))
                                ;; The window is selection-driven, so the cursor rides along
                                ;; with the drag instead of snapping back on the next paint.
                                (when-let [row (settings-selection-for-window rows
                                                                              entries
                                                                              (long s)
                                                                              visible-h)]
                                  (reset! selected row)))
                              (cond scrollbar-interaction? (do (vreset! pointer-down-target nil)
                                                               (recur))
                                    (= action MouseActionType/CLICK_DOWN)
                                    ;; Keep the painted frame stable between down/release. Moving
                                    ;; selection here could scroll the row away before release.
                                    (do (vreset! pointer-down-target pointer-target) (recur))
                                    (= action MouseActionType/CLICK_RELEASE)
                                    (let [pressed @pointer-down-target]
                                      (vreset! pointer-down-target nil)
                                      (when (and pressed (= pressed pointer-target))
                                        (let [row-idx (:row-idx pressed)]
                                          (reset! selected row-idx)
                                          (when (= :setting (:kind pressed))
                                            (activate-row! (nth rows row-idx)))))
                                      (recur))
                                    :else (do (when (= action MouseActionType/DRAG)
                                                (vreset! pointer-down-target nil))
                                              (recur)))))
                          :else
                          (condp = (key-type key)
                            ;; Esc clears an active search first, then closes on the next press.
                            KeyType/Escape
                            (if (str/blank? @query)
                              @values
                              (do (reset! query "") (reset! selected 0) (reset! scroll 0) (recur)))
                            KeyType/F1
                            (do (when selected-row
                                  (let [restore!
                                        (frame-restorer screen)

                                        action
                                        (settings-details-dialog! screen selected-row @values)]

                                    (restore!)
                                    (case action
                                      :change
                                      (activate-row! selected-row)

                                      :inherit
                                      (activate-row! (assoc selected-row :type :inherit))

                                      nil)))
                                (recur))
                            KeyType/ArrowUp
                            (do (swap! selected #(move-settings-selection rows % -1)) (recur))
                            KeyType/ArrowDown
                            (do (swap! selected #(move-settings-selection rows % 1)) (recur))
                            KeyType/PageUp
                            (do (swap! selected
                                  #(settings-page-selection rows entries % visible-h -1))
                                (recur))
                            KeyType/PageDown
                            (do (swap! selected
                                  #(settings-page-selection rows entries % visible-h 1))
                                (recur))
                            KeyType/Home (do (reset! selected (first-selectable-index rows))
                                             (recur))
                            KeyType/End (do (reset! selected
                                              (or (last (keep-indexed
                                                          (fn [i row]
                                                            (when (settings-selectable? row) i))
                                                          rows))
                                                  0))
                                            (recur))
                            ;; Backspace edits the live search query.
                            KeyType/Backspace (do (when (seq @query)
                                                    (swap! query #(subs % 0 (dec (count %))))
                                                    (reset! selected 0)
                                                    (reset! scroll 0))
                                                  (recur))
                            ;; Any printable character types into the search query (VS Code feel);
                            ;; Enter is the only key that toggles/activates the selected row.
                            KeyType/Character (let [c (key-character key)]
                                                (if (and c (>= (int c) 32))
                                                  (do (swap! query str c)
                                                      (reset! selected 0)
                                                      (reset! scroll 0)
                                                      (recur))
                                                  (recur)))
                            KeyType/Enter (do (when selected-row (activate-row! selected-row))
                                              (recur))
                            (recur))))))))))
           false))))))

;;; ── Session picker ─────────────────────────────────────────────────────
(defn- short-session-id
  [session]
  (let [id (str (get session "id"))]
    (subs id 0 (min 8 (count id)))))

(def ^:private untitled-session-title "Untitled session")

(defn- untitled-session-title?
  [title]
  (or (str/blank? (str title))
      (#{"untitled" "untitled session"} (str/lower-case (str/trim (str title))))))

(defn- empty-untitled-session?
  [s]
  (and (not (pos? (long (or (get s "turn_count") 0)))) (untitled-session-title? (get s "title"))))

(defn- session-title
  [session]
  (let [title
        (get session "title")

        base-title
        (if (untitled-session-title? title) untitled-session-title (str title))

        fork-count
        (long (or (get session "fork_count") 0))]

    (cond-> base-title
      (pos? fork-count)
      (str " [forks:" fork-count "]"))))

(def ^:private session-dialog-content-w 96)

(defn date->millis
  "Epoch ms out of whatever a session row carries in a timestamp slot — a
   `java.util.Date`, an `Instant`, or a number already — and nil when it carries
   nothing. The picker and the tab strip sort on the SAME reading."
  [v]
  (cond (instance? java.util.Date v) (.getTime ^java.util.Date v)
        (instance? java.time.Instant v) (.toEpochMilli ^java.time.Instant v)
        (number? v) (long v)
        :else nil))

(defn- date-value
  [v]
  (when-let [ms (date->millis v)]
    (java.util.Date. (long ms))))

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
  [^long table-w]
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
  [session active-id body-w]
  (let [id
        (get session "id")

        turn-count
        (get session "turn_count")

        modified-at
        (get session "modified_at")

        created-at
        (get session "created_at")

        active?
        (= (str id)
           (some-> active-id
                   str))]

    (session-table-row-label [(if active? "●" "") (short-session-id session) (session-title session)
                              (str (long (or turn-count 0))) (format-session-day created-at)
                              (format-session-time created-at) (format-session-day modified-at)
                              (format-session-time modified-at)]
                             body-w)))

(defn session-dialog-header [body-w] (session-table-row-label session-table-headers body-w))

(defn- session-dialog-sort-key
  [session]
  [(- (long (or (date->millis (get session "modified_at")) 0)))
   (- (long (or (date->millis (get session "created_at")) 0)))])

(defn session-dialog-items
  "Build table rows for existing sessions only. New/fork stay dialog
   options via the N/F shortcuts and command palette; they are not fake table
   data rows. Rows are sorted by Modified at desc, then Created at desc."
  ([sessions active-id] (session-dialog-items sessions active-id session-dialog-content-w))
  ([sessions active-id body-w]
   (mapv (fn [session]
           {:action :switch
            :id (str (get session "id")) ; downstream (switch-session!) accepts full UUID strings
            :label (session-dialog-label session active-id body-w)})
         (sort-by session-dialog-sort-key sessions))))

(defn- draw-session-row!
  [g left row inner-w selected? label]
  (p/set-colors! g t/dialog-fg t/dialog-bg)
  (p/styled g
            (p/selection-styles selected?)
            (p/fill-rect! g (inc (long left)) row inner-w 1)
            (p/put-str! g (inc (long left)) row (ellipsize label (max 0 (- (long inner-w) 1))))))

(defn session-picker-dialog!
  "Show recent TUI sessions in a fixed-size table. Returns
   `{:action :new}`, `{:action :fork}`, `{:action :switch :id <session-id>}`,
   or nil on Esc."
  [^TerminalScreen screen sessions active-id]
  (with-modal-background
    screen
    (let [selected
          (atom 0)

          scroll
          (atom 0)]

      (loop []

        (let [size
              (modal-size! screen)

              cols
              (.getColumns size)

              rows
              (.getRows size)

              g
              (frame/surface-graphics screen cols rows)

              ;; nil content-h -> shared full-height footprint, matching the
              ;; directory picker (both are long, scrollable browsers)
              bounds
              (draw-dialog-chrome! g cols rows "Sessions" (- cols 4) (- rows 4))

              {:keys [left inner-w]}
              bounds

              body-w
              (long (max 1 (- (long inner-w) 4)))

              items
              (session-dialog-items sessions active-id body-w)

              total
              (count items)

              {:keys [content-top content-h hint-row]}
              (dialog-layout bounds)

              table-x
              (inc (long left))

              table-top
              (long content-top)

              header-row
              (inc table-top)

              sep-row
              (inc header-row)

              body-top
              (inc sep-row)

              body-h
              (long (max 1 (- (long content-h) 4)))

              bottom-row
              (+ body-top body-h)

              _visible
              (min total body-h)

              _
              (swap! selected #(p/clamp % 0 (max 0 (dec total))))

              _
              (swap! scroll #(visible-window-start @selected % body-h total))]

          (p/set-colors! g t/dialog-border t/dialog-bg)
          (p/fill-rect! g (inc (long left)) table-top inner-w 1)
          (p/put-str! g table-x table-top (session-table-border-line body-w :top))
          (p/set-colors! g t/dialog-hint-key t/dialog-bg)
          (p/styled g
                    [p/BOLD]
                    (p/fill-rect! g (inc (long left)) header-row inner-w 1)
                    (p/put-str! g table-x header-row (session-dialog-header body-w)))
          ;; Re-paint the header's side `│` borders in the border color: the
          ;; header row was painted in dialog-hint-key, which would otherwise
          ;; leave the vertical edges a different color than the top/separator/
          ;; bottom chrome (same fix as the body rows + boxed-table).
          (p/set-colors! g t/dialog-border t/dialog-bg)
          (p/put-str! g table-x header-row "│")
          (p/put-str! g (+ table-x (dec body-w)) header-row "│")
          (p/fill-rect! g (inc (long left)) sep-row inner-w 1)
          (p/put-str! g table-x sep-row (session-table-border-line body-w :middle))
          (dotimes [i body-h]
            (let [idx (+ (long @scroll) i)
                  row (+ body-top i)]

              (if (< idx total)
                (do
                  (draw-session-row! g left row inner-w (= idx @selected) (:label (nth items idx)))
                  ;; Re-paint the side `│` borders in the border color: draw-session-row!
                  ;; painted the whole boxed row (borders included) in dialog-fg, which
                  ;; would otherwise leave the vertical edges (and the active `●` row's
                  ;; frame) a different color than the top/separator/bottom chrome.
                  (p/set-colors! g t/dialog-border t/dialog-bg)
                  (p/put-str! g table-x row "│")
                  (p/put-str! g (+ table-x (dec body-w)) row "│"))
                (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                    (p/fill-rect! g (inc (long left)) row inner-w 1)))))
          (p/set-colors! g t/dialog-border t/dialog-bg)
          (p/fill-rect! g (inc (long left)) bottom-row inner-w 1)
          (p/put-str! g table-x bottom-row (session-table-border-line body-w :bottom))
          (draw-hint-bar! g
                          left
                          hint-row
                          inner-w
                          [["↑/↓" "move"] ["Enter" "select"] ["N" "new"] ["F" "fork"]
                           ["Esc" "cancel"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (frame/refresh! screen)
          (let [key (read-modal-key! screen)]
            (when key
              (if-let [wheel-step (ScrollBar/wheelStep ^KeyStroke key)]
                (do (swap! selected #(p/clamp (+ (long %) (long wheel-step)) 0 (max 0 (dec total))))
                    (recur))
                (condp = (key-type key)
                  KeyType/Escape nil
                  KeyType/ArrowUp
                  (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total)))) (recur))
                  KeyType/ArrowDown
                  (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur))
                  KeyType/PageUp
                  (do (swap! selected #(p/clamp (- (long %) body-h) 0 (max 0 (dec total)))) (recur))
                  KeyType/PageDown
                  (do (swap! selected #(p/clamp (+ (long %) body-h) 0 (max 0 (dec total)))) (recur))
                  KeyType/Enter (when (pos? total)
                                  (select-keys (nth items @selected) [:action :id]))
                  KeyType/Character (let [raw-c (key-character key)
                                          c (lower-character raw-c)]

                                      (case c
                                        \n
                                        {:action :new}

                                        \f
                                        {:action :fork}

                                        (recur)))
                  (recur))))))))))

;;; ── Global navigator (Ctrl+G) ───────────────────────────────────────────────
;; One row per session. Per the locked 1:1 session<->workspace model a
;; session IS its workspace, so the navigator shows a single unified list:
;; no "Kind" column and no session/workspace mode split. The old design
;; emitted both a session row AND a workspace row per entry, so every
;; entry showed up twice with a contradictory "Kind".
(def ^:private navigator-search-delay-ms 180)

(defn- schedule-navigator-search!
  "Debounce transcript lookup off the modal paint/input thread. Replacing a
   query cancels its sleeping predecessor; generation guards discard any stale
   request that was already in flight."
  [task generation result query search-fn]
  (let [q
        (str/trim (or query ""))

        token
        (swap! generation inc)]

    (when-let [running @task]
      (future-cancel running))
    (reset! result nil)
    (if (or (empty? q) (nil? search-fn))
      (do (reset! task nil) token)
      (let [next-task (future (try (Thread/sleep (long navigator-search-delay-ms))
                                   (when (= token @generation)
                                     (let [matches (or (search-fn q) {})]
                                       (when (= token @generation)
                                         (reset! result {:token token :query q :matches matches}))))
                                   (catch InterruptedException _)
                                   (catch Throwable _
                                     (when (= token @generation)
                                       (reset! result {:token token :query q :matches {}})))))]
        (reset! task next-task)
        token))))

(def ^:private navigator-live-poll-ms
  "How often the picker looks for a keystroke while it is WATCHING the fleet stream.
   Frames land on the reader thread, so the paint loop must not park on a blocking
   read — a session would go live behind an unchanged screen. This is both the input
   latency a reader could feel and the ceiling on how long a status change waits to be
   painted."
  60)

(defn- read-navigator-key!
  "Wait for input or a background page, search result or fleet frame. `pending?`
   reports background updates; while present it also keeps the modal read polling."
  [^TerminalScreen screen task result pending?]
  (loop []

    (cond (some? @result) nil
          (and pending? (pending?)) nil
          (modal-input-pending? screen) (read-modal-key! screen)
          (or pending? (and @task (not (future-done? @task))))
          (do (Thread/sleep (if pending? (long navigator-live-poll-ms) 12)) (recur))
          :else (read-modal-key! screen))))

(defn- navigator-stamp
  "Compact `MM-dd HH:mm` timestamp (year dropped — these are recent
   sessions), or `-` when absent."
  [v]
  (let [day (format-session-day v)]
    (if (= day "-") "-" (str (subs day 5) " " (format-session-time v)))))

(defn- navigator-session-row
  "Normalize a session row with explicit project and group names.
   Group metadata never determines the status or timestamp ink."
  [active-session-id groups session]
  (let [id
        (get session "id")

        active?
        (= (str id)
           (some-> active-session-id
                   str))

        work-dir
        (or (not-empty (:work-dir session)) "No work dir")

        ;; The gateway's own fleet fact (`state/soul` -> `bus/waiting-requests`),
        ;; never a local guess: a run parked on an unanswered human-input request
        ;; is normally parked in ANOTHER process, and this list is where its
        ;; operator goes looking for it.
        awaiting-input?
        (true? (get session "is_awaiting_input"))

        ;; HOW MANY requests it is parked on, so answering one of two visibly
        ;; drops the row from ×2 to ×1 instead of leaving the same badge lit.
        awaiting-count
        (long (or (get session "awaiting_input_count") (if awaiting-input? 1 0)))

        live?
        (true? (get session "live"))

        ;; The gateway's own NEW, painted rather than re-derived: this picker lists
        ;; sessions no tab in this terminal holds, so a local guess reads every one
        ;; of them as read.
        unread
        (long (or (get session "unread_answers") 0))

        ;; STOPPED is bounded by that mark, exactly as the app bounds it
        ;; (`SessionList`): it reports a run that was cut off and not yet read,
        ;; instead of sitting on an abandoned session for good.
        stopped?
        (and (not live?)
             (or (true? (get session "was_interrupted")) (true? (get session "was_failed")))
             (pos? unread))

        gid
        (not-empty (str (get session "group_id")))

        group
        (get groups gid)]

    {:id (str "session:" id)
     :focused? active?
     :awaiting-input? awaiting-input?
     :unread? (pos? unread)
     :stopped? stopped?
     :title (session-title session)
     :session (short-session-id session)
     :group (or (not-empty (get session "project_name"))
                (not-empty (str (get session "project_id")))
                "No project")
     :project-id (not-empty (str (get session "project_id")))
     :session-group-id gid
     :session-group (or (not-empty (str (get group "name"))) gid "No group")
     ;; Keep the gateway's star for Ctrl+S without changing recency or row fields.
     :favorite-rank (get session "favorite_rank")
     :favorite? (some? (get session "favorite_rank"))
     :dir work-dir
     :work-dir work-dir
     :status (cond awaiting-input?
                   (if (> awaiting-count 1) (str "! HITL ×" awaiting-count) "! HITL")
                   live? "● LIVE"
                   stopped? "⨯ STOPPED"
                   (pos? unread) (if (> unread 1) (str unread " NEW") "NEW")
                   :else "IDLE")
     :created (navigator-stamp (get session "created_at"))
     :modified (navigator-stamp (or (get session "modified_at") (get session "created_at")))
     :target {:action :switch :id id}}))

(defn- navigator-row-in-scope?
  "Project scopes intersect the union of the selected groups."
  [row {:keys [project-id root group-ids]}]
  (and (or (nil? project-id) (= (str project-id) (:project-id row)))
       (or (nil? root) (if (str/blank? root) (nil? (:project-id row)) (= root (:work-dir row))))
       (or (nil? group-ids) (contains? (set group-ids) (:session-group-id row)))))

(defn- navigator-scope-group?
  "Whether the search scope offers this group to narrow to: one of the chosen project's
   groups, or an unfiled one under No project. All projects offers none, because it
   already means all groups."
  [scope group]
  (cond (:project-id scope) (= (:project-id scope) (str (get group "project_id")))
        (contains? scope :root) (nil? (get group "project_id"))
        :else false))

(defn- navigator-scope-controls
  "The row below the search field says where the search looks. Each control is its key
   in hint ink and its current choice in text ink, at its own width, so only a long
   project, then a long groups label, gives up columns. Groups appear only while the
   chosen project has groups to narrow to."
  [width project group-count groups?]
  (let [gap
        3

        controls
        (cond-> [{:action :project :key "C-p" :text (str "Project: " project)}]
          groups?
          (conj {:action :groups
                 :key "C-g"
                 :text
                 (str "Groups: "
                      (if (pos? (long group-count)) (str group-count " selected") "All groups"))}))

        natural
        (mapv #(+ (count (:key %)) 1 (count (:text %))) controls)

        spare
        (- (long width) (* gap (dec (count controls))))

        widths
        (loop [idx
               0

               widths
               natural

               over
               (max 0 (- (long (reduce + natural)) spare))]

          (if (or (zero? over) (= idx (count widths)))
            widths
            (let [w
                  (long (widths idx))

                  give
                  (min over (max 0 (- w (+ (count (:key (controls idx))) 9))))]

              (recur (inc idx) (assoc widths idx (- w give)) (- over give)))))]

    (loop [idx
           0

           x
           0

           out
           []]

      (if (= idx (count controls))
        out
        (let [control
              (controls idx)

              w
              (min (long (widths idx)) (- (long width) x))

              text-w
              (- w (count (:key control)) 1)]

          (if (< text-w 1)
            out
            (recur (inc idx)
                   (+ x w gap)
                   (conj out
                         (assoc control
                           :x x
                           :width w
                           :label (p/ellipsize (:text control) text-w))))))))))

(defn- navigator-selected-index
  "Keep the selected session when recency, pages or search results move its row.
   On opening, `selection` names the current session. Later it holds the previous
   visible rows, so keyboard movement is resolved before the next list changes."
  [rows selected selection]
  (let [id (if-let [previous (:rows selection)]
             (:id (:target (nth previous (long selected) nil)))
             (:id selection))]
    (or (when id
          (some (fn [[idx row]]
                  (when (= (str id) (str (:id (:target row)))) idx))
                (map-indexed vector rows)))
        (p/clamp selected 0 (max 0 (dec (count rows)))))))

(defn- navigator-all-rows
  "Build one newest-first list across projects, groups and stars. Empty untitled
   shells stay hidden by default, but the current session always survives."
  [{:keys [sessions active-session-id show-empty-untitled? groups]}]
  (->> sessions
       (remove #(and (not show-empty-untitled?)
                     (empty-untitled-session? %)
                     (not= (str (get % "id"))
                           (some-> active-session-id
                                   str))))
       (sort-by (juxt
                  #(or (date->millis (get % "modified_at")) (date->millis (get % "created_at")) 0)
                  #(str (get % "id")))
                #(compare %2 %1))
       (mapv #(navigator-session-row active-session-id groups %))))

(def ^:private navigator-page-slack
  "How near the end of the rows it holds the reader may come before the picker asks the
   gateway for its next page. Wide enough that the page lands before the list runs out
   under a held arrow key."
  8)

(defn- navigator-page-in?
  "Pull the next scoped gateway window near the end of the held rows."
  [{:keys [selected total next-cursor]}]
  (boolean (and (seq (str next-cursor))
                (>= (+ (long selected) (long navigator-page-slack)) (long total)))))

(defn- navigator-merge-sessions
  "`held` plus the rows it does not already carry, in the order they arrived. A page and a
   search answer can name the same session, and the picker paints it once."
  [held incoming]
  (first (reduce (fn [[rows seen] row]
                   (let [id (str (get row "id"))]
                     (if (contains? seen id) [rows seen] [(conj rows row) (conj seen id)])))
                 [[] #{}]
                 (concat held incoming))))

(defn- navigator-apply-fleet-frame
  "PURE fold of ONE fleet frame into the rows the picker holds. `session.status`
   re-stamps that row's fleet marks (the frame speaks the wire's `is_live`; a list row
   spells the same fact `live`, so the row keeps ITS vocabulary), `session.title_updated`
   its title. A frame naming a session outside the window is dropped: the picker paints
   what it holds, and a row it has not walked to yet arrives already current."
  [held frame]
  (let [sid
        (str (get frame "session_id"))

        type
        (str (get frame "type"))]

    (if (or (str/blank? sid) (not (#{"session.status" "session.title_updated"} type)))
      held
      (mapv (fn [row]
              (if-not (= sid (str (get row "id")))
                row
                (if (= "session.title_updated" type)
                  (assoc row "title" (str (get frame "title")))
                  (assoc row
                    "live" (true? (get frame "is_live"))
                    "is_awaiting_input" (true? (get frame "is_awaiting_input"))
                    "awaiting_input_count" (long (or (get frame "awaiting_input_count") 0))
                    "current_turn_id" (get frame "current_turn_id")))))
            held))))

(defn- navigator-row-matches?
  [row query]
  (let [needle (str/lower-case (str/trim (or query "")))]
    (or (empty? needle)
        (some #(str/includes? (str/lower-case (str (get row % ""))) needle)
              [:title :session :dir :work-dir :status]))))

(defn- navigator-visible-rows
  "Union local metadata and gateway transcript matches without changing recency
   or replacing a session's status with the location of a search hit."
  [rows query transcript-ids]
  (let [q (str/trim (or query ""))]
    (into []
          (keep (fn [row]
                  (let [local-hit? (navigator-row-matches? row query)
                        match (get transcript-ids (str (:id (:target row))))
                        body-hit? (some? match)]

                    (when (or local-hit? body-hit?)
                      (cond-> row
                        (and body-hit? (not local-hit?) (seq q))
                        (assoc :transcript-match? true)

                        (and body-hit? (map? match) (seq q))
                        (assoc :transcript-match (assoc match :title (:title row))))))))
          rows)))

(defn- navigator-fold
  "Fold `s` the way the gateway's `unicode61` index compares words: lower case and
   without diacritics. Each char folds to ONE char, so an index into the result is
   an index into `s`."
  ^String [s]
  (let [^String s
        (str (or s ""))

        sb
        (StringBuilder. (.length s))]

    (dotimes [i (.length s)]
      (let [c (.charAt s i)
            base (if (< (int c) 128)
                   c
                   (.charAt (Normalizer/normalize (String/valueOf c) Normalizer$Form/NFD) 0))]

        (.append sb (Character/toLowerCase (char base)))))
    (.toString sb)))

(defn- navigator-search-terms
  "The words of a search, folded like `navigator-fold`, longest first. The gateway
   matches each word on its own at the start of a word in the text, so the message
   pane marks every word and not the whole query."
  [query]
  (->> (str/split (navigator-fold (str/trim (str (or query "")))) #"[^\p{L}\p{N}]+")
       (remove str/blank?)
       distinct
       (sort-by count #(compare %2 %1))
       vec))

(defn- navigator-word-start?
  [^String folded i]
  (let [i (long i)]
    (or (zero? i) (not (Character/isLetterOrDigit (.charAt folded (dec i)))))))

(defn- navigator-highlight-segments
  "Split `s` into `[text match?]` segments. A segment matches where a word of
   `query` starts a word of `s`, compared without case or diacritics, so the
   words the gateway matched stand out in its snippet."
  [s query]
  (let [s
        (str (or s ""))

        terms
        (navigator-search-terms query)]

    (if (or (empty? terms) (str/blank? s))
      [[s false]]
      (let [folded
            (navigator-fold s)

            n
            (count s)]

        (loop [i
               0

               from
               0

               acc
               []]

          (if (>= i n)
            (cond-> acc
              (< from n)
              (conj [(subs s from) false]))
            (if-let [^String term (when (navigator-word-start? folded i)
                                    (some (fn [^String term]
                                            (when (.startsWith folded term (int i)) term))
                                          terms))]
              (let [end (+ i (.length term))]
                (recur end
                       end
                       (cond-> acc
                         (< from i)
                         (conj [(subs s from i) false])

                         :always
                         (conj [(subs s i end) true]))))
              (recur (inc i) from acc))))))))

(defn- navigator-preview-entries
  "Message rows for a body match: ONE row per MATCH HIT, newest first — `You`
   for a hit in the user's own request, `Vis` for one in the assistant's reply or
   its thinking (`:side`). `:at` is the message time in epoch ms when the gateway
   sent it.

   Falls back to the legacy single request/reply snippet pair when the caller
   supplied no `:hits`."
  [match]
  (when (map? match)
    (let [hits (into []
                     (comp (filter #(not (str/blank? (:snippet %))))
                           (map (fn [h]
                                  (let [side (or (:side h) :reply)]
                                    {:label (if (= :request side) "You" "Vis")
                                     :role (if (= :request side) :user :ai)
                                     :side side
                                     :at (:at h)
                                     :text (:snippet h)}))))
                     (:hits match))]
      (if (seq hits)
        hits
        (cond-> []
          (not (str/blank? (:request-snippet match)))
          (conj {:label "You" :role :user :side :request :text (:request-snippet match)})

          (not (str/blank? (:reply-snippet match)))
          (conj {:label "Vis" :role :ai :side :reply :text (:reply-snippet match)}))))))

(defn- navigator-block-heights
  "Two terminal lines per session: title/status, then explicit location."
  [visible-rows]
  (vec (repeat (count visible-rows) 2)))

(defn- navigator-scroll-start
  "First visible row index: the smallest scroll that still fits the selected
   row's own line inside `budget` painted lines, then pulled back so the window
   is never scrolled past the end (otherwise a tall block's window keeps its
   inherited scroll and paints one lonely row with dead space under it)."
  [heights selected scroll budget]
  (let [n
        (count heights)

        selected
        (long (max 0 (long selected)))

        budget
        (max 1 (long budget))

        ;; Smallest index whose remaining blocks all fit — scrolling past it only
        ;; wastes lines, so it is the hard upper bound for the window start.
        tail-start
        (loop [i
               (dec n)

               used
               0

               k
               (max 0 (dec n))]

          (if (neg? i)
            k
            (let [u (+ (long used) (long (nth heights i)))]
              (if (> u budget) k (recur (dec i) u i)))))]

    (min (long tail-start)
         (long (loop [s (min (max 0 (long scroll)) selected)]
                 (if (and (< s selected)
                          (> (long (reduce +
                                           (long (get heights selected 1))
                                           (subvec heights s selected)))
                             budget))
                   (recur (inc s))
                   s))))))

(defn- navigator-visible-blocks
  "Paint complete two-line session rows, clipped to the list budget."
  [visible-rows start budget]
  (let [start
        (long (max 0 (long start)))

        end
        (min (count visible-rows) (+ start (max 1 (quot (long budget) 2))))]

    (mapv (fn [idx]
            {:idx idx :entry (nth visible-rows idx)})
          (range start end))))

(def ^:private navigator-min-height
  "Box height the picker asks for at least, so a search has room for the
   matching messages even when there are only a few sessions."
  24)

(defn- navigator-pane-layout
  "Always split the body below the query into a session list on the left and
   a message preview on the right, including narrow terminals and blank queries."
  [{:keys [left right inner-w]} content-top content-h]
  (let [left
        (long left)

        right
        (long right)

        inner-w
        (long inner-w)

        body-top
        (+ (long content-top) 3)

        avail
        (max 1 (- (long content-h) 3))

        list-inner
        (long (p/clamp (quot (* 60 inner-w) 100) 4 (max 4 (- inner-w 4))))

        divider
        (+ left 1 list-inner)]

    {:mode :side
     :body-x (+ left 2)
     :body-w (max 1 (- list-inner 4))
     :scrollbar-col (- divider 2)
     :body-top body-top
     :list-budget avail
     :divider divider
     :preview-x (+ divider 2)
     :preview-w (max 1 (- right divider 3))
     :preview-top body-top
     :preview-h avail}))

(defn- navigator-preview-lines
  "Paint plan of the message pane for the selected `entry`, at most `height`
   lines of `width` columns. Each line is a map by `:kind`: `:title` the
   session's title; `:label` one message's author (`:label`, `:role`), place and
   time; `:text` one wrapped line of its snippet as `navigator-highlight-segments`;
   `:note` why no message shows; `:more` the matched messages that did not fit;
   `:blank` a spacer. Whole messages fit first: only a first message taller than
   the pane is clipped."
  [entry query {:keys [width height pending?]}]
  (let [width
        (max 1 (long width))

        height
        (max 0 (long height))

        match
        (:transcript-match entry)

        head
        [{:kind :title :text (str (:title entry))} {:kind :blank}]

        more
        (fn [n]
          {:kind :more
           :count n
           :text (str "+" n (if (= 1 (long n)) " more message" " more messages"))})

        groups
        (mapv (fn [{:keys [label role side at text]}]
                (into [{:kind :label
                        :label label
                        :role role
                        :place (when (= :thinking side) "thinking")
                        :stamp (when (some? at) (navigator-stamp at))}]
                      (map (fn [line]
                             {:kind :text :segments (navigator-highlight-segments line query)}))
                      (p/word-wrap (str/trim (str/replace (str text) #"\s+" " ")) width)))
              (navigator-preview-entries match))

        lines
        (cond (nil? entry) (if pending? [{:kind :note :text "Searching messages…"}] [])
              (seq groups) (loop [acc
                                  head

                                  groups
                                  groups

                                  shown
                                  0]

                             (if-let [group (first groups)]
                               (let [gap (if (pos? shown) [{:kind :blank}] [])
                                     later (dec (count groups))
                                     ;; While more messages follow, one line stays free for the
                                     ;; note that counts them.
                                     room (- height (if (pos? later) 1 0))]

                                 (cond (<= (+ (count acc) (count gap) (count group)) room)
                                       (recur (into (into acc gap) group) (rest groups) (inc shown))
                                       (zero? shown)
                                       (cond-> (into acc (take (max 0 (- room (count acc))) group))
                                         (pos? later)
                                         (conj (more later)))
                                       :else (conj acc (more (count groups)))))
                               acc))
              (str/blank? (str query)) (conj head
                                             {:kind :note :text "Type to find matching messages."})
              pending? (conj head {:kind :note :text "Searching messages…"})
              (= :title (:kind match))
              (conj head {:kind :note :text "The title matches. No message matches."})
              :else (conj head {:kind :note :text "No message matches."}))]

    (vec (take height lines))))

(defn- draw-navigator-session!
  [g x row width entry selected?]
  (let [focused?
        (:focused? entry)

        status-color
        (cond (:awaiting-input? entry) t/warning-fg
              (:stopped? entry) t/cancelled-fg
              (or focused? (:unread? entry)) t/dialog-hint-key
              :else t/dialog-hint)

        title-color
        (cond (:awaiting-input? entry) t/warning-fg
              focused? t/dialog-hint-key
              :else t/dialog-fg)

        date-color
        t/dialog-hint

        fields
        [[(str (:modified entry)) date-color true] [" / " t/dialog-hint false]
         [(str (:status entry)) status-color false] [" / " t/dialog-hint false]
         [(str (:title entry)) title-color (or selected? focused?)]]]

    (p/styled
      g
      (p/selection-styles selected?)
      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/fill-rect! g x row width 2)
      (loop [fields
             fields

             cx
             (long x)

             remaining
             (max 0 (long width))]

        (when (and (seq fields) (pos? remaining))
          (let [[text color bold?]
                (first fields)

                text
                (if (next fields) (p/truncate-cols text remaining) (p/ellipsize text remaining))

                used
                (long (p/display-width text))]

            (p/set-colors! g color t/dialog-bg)
            (if bold? (p/styled g [p/BOLD] (p/put-str! g cx row text)) (p/put-str! g cx row text))
            (recur (rest fields) (+ cx used) (- remaining used)))))
      (p/set-colors! g t/dialog-hint t/dialog-bg)
      (p/put-str! g
                  x
                  (inc (long row))
                  (p/ellipsize (str "Project: " (:group entry) " / Group: " (:session-group entry))
                               width)))))

(defn- draw-navigator-segments!
  "Paint `navigator-highlight-segments` from `x`, clipped to `width` columns. A
   matched segment is marked in the accent ink, like a highlighter pen."
  [g x row width segments]
  (loop [segments
         segments

         cx
         (long x)

         remaining
         (long width)]

    (when (and (seq segments) (pos? remaining))
      (let [[segment match?]
            (first segments)

            segment
            (p/truncate-cols segment remaining)

            segment-w
            (long (p/display-width segment))]

        (if match?
          (do (p/set-colors! g t/dialog-bg t/dialog-hint-key)
              (p/styled g [p/BOLD] (p/put-str! g cx row segment)))
          (do (p/set-colors! g t/dialog-fg t/dialog-bg) (p/put-str! g cx row segment)))
        (recur (rest segments) (+ cx segment-w) (- remaining segment-w))))))

(defn- draw-navigator-preview!
  "Paint the message pane from its `navigator-preview-lines` plan: the selected
   session's title, then for each matching message its author, place and time
   over its snippet."
  [g x top width lines]
  (let [width (long width)]
    (doseq [[i line] (map-indexed vector lines)]
      (let [row (+ (long top) (long i))]
        (case (:kind line)
          :title
          (do (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/styled g [p/BOLD] (p/put-str! g x row (p/ellipsize (:text line) width))))

          :label
          (let [label (str (:label line))
                label-w (long (p/display-width label))
                details (str/join "  ·  " (remove nil? [(:place line) (:stamp line)]))]

            (p/set-colors! g (if (= :user (:role line)) t/user-role-fg t/ai-role-fg) t/dialog-bg)
            (p/styled g [p/BOLD] (p/put-str! g x row (p/ellipsize label width)))
            (when (and (seq details) (< (+ label-w 2) width))
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g (+ (long x) label-w 2) row (p/ellipsize details (- width label-w 2)))))

          :text
          (draw-navigator-segments! g x row width (:segments line))

          (:note :more)
          (do (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g x row (p/ellipsize (:text line) width)))

          nil)))))

(defn- draw-navigator-divider!
  "Paint the border between the list and the message pane, from the query
   separator down to the footer separator and joined to both."
  [g col content-top content-h]
  (let [top
        (inc (long content-top))

        bottom
        (+ (long content-top) (long content-h))]

    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/set-char! g col top p/BOX_T_DOWN)
    (doseq [row (range (inc top) bottom)]
      (p/set-char! g col row p/BOX_V))
    (p/set-char! g col bottom p/BOX_T_UP)))

(defn navigator-dialog!
  "C-x s session picker. `:load-initial` and `:load-more` fetch pages off the
   paint/input thread; the latter takes a cursor. Loading and failure retain the
   current rows and keyboard input; C-r retries a failed page. Transcript search
   is debounced. Closing cancels outstanding page and search work."
  [^TerminalScreen screen opts]
  (with-modal-background
    screen
    (let [query
          (atom "")

          selected
          (atom 0)

          selection
          (atom {:id (some-> (:active-session-id opts)
                             str)})

          scroll
          (atom 0)

          scrollbar-drag-offset
          (volatile! nil)

          show-empty-untitled?
          (atom (boolean (:show-empty-untitled? opts)))

          ;; The rows the picker HOLDS. It opens on ONE gateway window and grows from
          ;; there: a page as the reader nears the end (`page-in!`), and the rows a
          ;; server search named that this window does not have.
          loaded-sessions
          (atom (vec (:sessions opts)))

          ;; Catalog reads stay off the input thread. Rows name a group even while it loads.
          groups-index
          (atom (or (:groups opts) {}))

          scope-projects
          (atom (vec (:projects opts)))

          search-scope
          (atom {})

          scope-project-label
          (atom "All projects")

          page-generation
          (atom 0)

          groups-task
          (atom nil)

          page-cursor
          (atom (:next-cursor opts))

          page-task
          (atom nil)

          page-result
          (atom nil)

          page-error
          (atom nil)

          load-more
          (:load-more opts)

          ;; A worker returns data only. The UI adopts it after checking query and scope.
          search-sessions
          (:search-sessions opts)

          transcript-ids
          (atom {})

          transcript-query
          (atom nil)

          search-task
          (atom nil)

          search-generation
          (atom 0)

          search-result
          (atom nil)

          ;; What the FLEET stream said since the last paint. The picker holds a window and
          ;; never re-reads a row, so this delta feed is how a session that went live, parked
          ;; on a human or was renamed reaches the list at all.
          fleet-frames
          (atom [])

          stop-fleet!
          (when-let [watch (:watch-fleet opts)]
            (try (watch (fn [frame]
                          (swap! fleet-frames conj frame)))
                 (catch Throwable _ nil)))]

      (letfn
        [(start-search! []
           (let [q
                 (str/trim @query)

                 scope
                 @search-scope]

             (reset! transcript-ids {})
             (reset! transcript-query nil)
             (reset! page-cursor nil)
             (swap! page-generation inc)
             (when-let [running @page-task]
               (future-cancel running))
             (reset! page-task nil)
             (reset! page-result nil)
             (if (empty? q)
               (do (swap! search-generation inc)
                   (when-let [running @search-task]
                     (future-cancel running))
                   (reset! search-task nil)
                   (reset! search-result nil)
                   (when search-sessions (start-page! #(search-sessions q scope))))
               (schedule-navigator-search! search-task
                                           search-generation
                                           search-result
                                           q
                                           (when search-sessions
                                             (fn [needle]
                                               (assoc (search-sessions needle scope)
                                                 :scope scope)))))))
         (reset-list! [search?]
           (reset! selected 0)
           (reset! scroll 0)
           (reset! selection (when (str/blank? @query)
                               {:id (some-> (:active-session-id opts)
                                            str)}))
           (when search? (start-search!)))
         (start-page! [load!]
           (let [token (swap! page-generation inc)]
             (reset! page-error nil)
             (when-let [running @page-task]
               (future-cancel running))
             (reset! page-task (future (try (let [page (load!)]
                                              (when (= token @page-generation)
                                                (reset! page-result {:token token :page page})))
                                            (catch InterruptedException _ nil)
                                            (catch Throwable _
                                              (when (= token @page-generation)
                                                (reset! page-result {:token token
                                                                     :retry load!}))))))))
         (page-in! [total]
           (let [q
                 (str/trim @query)

                 scope
                 @search-scope

                 cursor
                 @page-cursor]

             (when (and (or load-more search-sessions)
                        (nil? @page-task)
                        (nil? @search-task)
                        (nil? @page-error)
                        (or (empty? q) (= q @transcript-query))
                        (navigator-page-in? {:selected @selected :total total :next-cursor cursor}))
               (start-page! (if (and load-more (empty? q))
                              #(load-more cursor scope)
                              #(search-sessions q (assoc scope :after cursor)))))))
         (change-scope! [scope label]
           (reset! search-scope scope)
           (reset! scope-project-label label)
           (reset! loaded-sessions [])
           (reset! selection nil)
           (reset-list! true))
         (choose-project! []
           (when-let [chosen (select-dialog! screen
                                             "Search project"
                                             (into [{:label "All projects" :scope {}}
                                                    {:label "No project" :scope {:root ""}}]
                                                   (map (fn [project]
                                                          {:label (get project "name")
                                                           :scope {:project-id (str (get project
                                                                                         "id"))}})
                                                        @scope-projects)))]
             (change-scope! (:scope chosen) (:label chosen))))
         (choose-groups! []
           (let [choices
                 (->> (vals @groups-index)
                      (filter #(navigator-scope-group? @search-scope %))
                      (sort-by (juxt #(str (get % "name")) #(str (get % "id"))))
                      (mapv (fn [group]
                              {:id (str (get group "id"))
                               :label (str (get group "name") " · " (short-session-id group))})))

                 initial
                 (into #{}
                       (keep #(when (contains? (set (:group-ids @search-scope)) (:id %))
                                (:label %)))
                       choices)]

             (when-let [chosen (and (seq choices)
                                    (multi-select-dialog!
                                      screen
                                      "Search groups (any selected; empty means all)"
                                      (mapv :label choices)
                                      initial))]
               (let [ids
                     (into #{} (keep #(when (contains? (set chosen) (:label %)) (:id %))) choices)]
                 (change-scope! (cond-> (dissoc @search-scope :group-ids)
                                  (seq ids)
                                  (assoc :group-ids ids))
                                @scope-project-label)))))
         (scope-action! [action]
           (case action
             :project
             (choose-project!)

             :groups
             (choose-groups!)

             nil))]
        (try
          (when-let [load-initial (:load-initial opts)]
            (start-page! load-initial))
          (when-let [load-catalog (:load-catalog opts)]
            (reset! groups-task (future (try (when-let [catalog (load-catalog)]
                                               (reset! groups-index (:groups catalog))
                                               (reset! scope-projects (:projects catalog)))
                                             (catch InterruptedException _ nil)
                                             (catch Throwable _ nil)))))
          (loop []

            (when (and @groups-task (future-done? @groups-task)) (reset! groups-task nil))
            (when-let [{:keys [token page retry]} (first (swap-vals! page-result (constantly nil)))]
              (when (= token @page-generation)
                (reset! page-task nil)
                (reset! page-error retry)
                (when-not retry
                  (reset! page-cursor (:next-cursor page))
                  (swap! loaded-sessions navigator-merge-sessions (:sessions page))
                  (when (seq (:matches page)) (swap! transcript-ids merge (:matches page))))))
            (when (seq @fleet-frames)
              (let [frames (first (swap-vals! fleet-frames empty))]
                (swap! loaded-sessions #(reduce navigator-apply-fleet-frame % frames))))
            (when-let [{:keys [token query matches]} @search-result]
              (reset! search-result nil)
              (when (and (= token @search-generation) (= (:scope matches) @search-scope))
                (swap! loaded-sessions navigator-merge-sessions (:sessions matches))
                (reset! page-cursor (:next-cursor matches))
                (reset! transcript-query query)
                (reset! transcript-ids (or (:matches matches) {}))
                (reset! search-task nil)))
            (let [rows
                  (filterv #(navigator-row-in-scope? % @search-scope)
                    (navigator-all-rows (assoc opts
                                          :sessions @loaded-sessions
                                          :groups @groups-index
                                          :show-empty-untitled? @show-empty-untitled?)))

                  visible-rows
                  (navigator-visible-rows rows @query @transcript-ids)

                  total
                  (count visible-rows)

                  size
                  (modal-size! screen)

                  cols
                  (.getColumns size)

                  rows-n
                  (.getRows size)

                  g
                  (frame/surface-graphics screen cols rows-n)

                  unfiltered
                  (navigator-visible-rows rows "" {})

                  desired-lines
                  (reduce + 0 (navigator-block-heights unfiltered))

                  bounds
                  (draw-dialog-chrome! g
                                       cols
                                       rows-n
                                       "Sessions"
                                       (- cols 4)
                                       (max (long navigator-min-height) (+ (long desired-lines) 4)))

                  {:keys [left right inner-w]}
                  bounds

                  {:keys [content-top content-h hint-row]}
                  (dialog-layout bounds)

                  query-row
                  content-top

                  content-w
                  (long (max 1 (- (long inner-w) 2)))

                  scope-controls
                  (navigator-scope-controls content-w
                                            @scope-project-label
                                            (count (:group-ids @search-scope))
                                            (boolean (some #(navigator-scope-group? @search-scope %)
                                                           (vals @groups-index))))

                  block-heights
                  (navigator-block-heights visible-rows)

                  {:keys [divider preview-x preview-top preview-w preview-h] :as panes}
                  (navigator-pane-layout bounds content-top content-h)

                  body-x
                  (long (:body-x panes))

                  body-w
                  (long (:body-w panes))

                  scrollbar-col
                  (long (:scrollbar-col panes))

                  body-top
                  (long (:body-top panes))

                  list-budget
                  (long (:list-budget panes))

                  _
                  (reset! selected (navigator-selected-index visible-rows @selected @selection))

                  _
                  (when (seq visible-rows) (reset! selection {:rows visible-rows}))

                  _
                  (page-in! total)

                  _
                  (swap! scroll #(navigator-scroll-start block-heights @selected % list-budget))

                  blocks
                  (navigator-visible-blocks visible-rows @scroll list-budget)

                  page-rows
                  (max 1 (count blocks))

                  page-status
                  (cond @page-error "Could not load sessions · C-r retry"
                        @page-task
                        (if (seq @loaded-sessions) "Loading more sessions…" "Loading sessions…"))]

              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
              (let [cursor-pos (draw-text-input-field! g
                                                       (inc (long left))
                                                       query-row
                                                       content-w
                                                       @query
                                                       (count @query))]
                (p/set-colors! g t/dialog-border t/dialog-bg)
                (doseq [control scope-controls]
                  (let [x (+ (inc (long left)) (long (:x control)))]
                    (p/set-colors! g t/dialog-hint t/dialog-bg)
                    (p/put-str! g x (inc (long content-top)) (:key control))
                    (p/set-colors! g t/dialog-fg t/dialog-bg)
                    (p/put-str! g
                                (+ x (count (:key control)) 1)
                                (inc (long content-top))
                                (:label control))))
                (p/set-colors! g t/dialog-border t/dialog-bg)
                (p/draw-separator! g left right (+ (long content-top) 2))
                (when (and page-status (pos? total))
                  (p/set-colors! g t/dialog-hint t/dialog-bg)
                  (p/put-str! g body-x (+ (long content-top) 2) (ellipsize page-status body-w)))
                (if (zero? total)
                  (let [hidden-count (count (filter empty-untitled-session? @loaded-sessions))
                        message (cond page-status page-status
                                      (not (str/blank? @query)) "No matches"
                                      (and (pos? hidden-count) (not @show-empty-untitled?))
                                      "Only empty untitled sessions hidden"
                                      :else "No sessions yet")
                        message-x (+ body-x (long (max 0 (quot (- body-w (count message)) 2))))]

                    (p/set-colors! g t/dialog-hint t/dialog-bg)
                    (p/put-str! g message-x (+ body-top 1) (ellipsize message body-w)))
                  (loop [remaining blocks
                         row body-top]

                    (when-let [{:keys [idx entry]} (first remaining)]
                      (draw-navigator-session! g body-x row body-w entry (= idx @selected))
                      (recur (rest remaining) (+ (long row) 2)))))
                ;; The list and the selected session's messages always stay side by side.
                (draw-navigator-divider! g divider (inc (long content-top)) (dec (long content-h)))
                (draw-navigator-preview!
                  g
                  preview-x
                  preview-top
                  preview-w
                  (navigator-preview-lines
                    (when (pos? total) (nth visible-rows @selected))
                    (or @transcript-query @query)
                    {:width preview-w :height preview-h :pending? (some? @search-task)}))
                (when (> total page-rows)
                  (ScrollBar/draw g
                                  Direction/VERTICAL
                                  (TerminalPosition. (int scrollbar-col) (int body-top))
                                  (int list-budget)
                                  (int total)
                                  (int page-rows)
                                  (when (some? @scroll) (Integer/valueOf (int @scroll)))
                                  t/dialog-border
                                  t/dialog-bg
                                  t/dialog-hint-key
                                  t/dialog-bg))
                (draw-hint-bar! g
                                left
                                hint-row
                                inner-w
                                [["↑/↓" "move"] ["Enter" "open"] ["C-n" "new"] ["C-f" "fork"]
                                 ["C-s" "star"] ["C-d" "delete"] ["C-b" "project"]
                                 [(keymap/chord \u)
                                  (if @show-empty-untitled? "hide empty" "show empty")]
                                 ["Esc" "cancel"]])
                (.setCursorPosition screen cursor-pos)
                (frame/refresh! screen))
              (let [key (read-navigator-key!
                          screen
                          search-task
                          search-result
                          (when (or stop-fleet! @page-task @page-result @groups-task)
                            #(or (seq @fleet-frames)
                                 @page-result
                                 (and @groups-task (future-done? @groups-task)))))]
                (if-not key
                  (recur)
                  (cond
                    (input/ctrl-char? key \p) (do (scope-action! :project) (recur))
                    (input/ctrl-char? key \g) (do (scope-action! :groups) (recur))
                    (and (instance? MouseAction key)
                         (= MouseActionType/CLICK_DOWN (.getActionType ^MouseAction key))
                         (= (inc (long content-top)) (.getRow (.getPosition ^MouseAction key))))
                    (do (let [column (- (.getColumn (.getPosition ^MouseAction key))
                                        (inc (long left)))
                              control (some #(when (<= (long (:x %))
                                                       column
                                                       (dec (+ (long (:x %)) (long (:width %)))))
                                               %)
                                            scope-controls)]

                          (scope-action! (:action control)))
                        (recur))
                    (some? (ScrollBar/wheelStep ^KeyStroke key))
                    (do (swap! selected #(p/clamp (+ (long %)
                                                     (long (ScrollBar/wheelStep ^KeyStroke key)))
                                                  0
                                                  (max 0 (dec total))))
                        (recur))
                    (and (instance? MouseAction key)
                         (> total page-rows)
                         (let [action (.getActionType ^MouseAction key)]
                           (or (= action MouseActionType/DRAG)
                               (= action MouseActionType/CLICK_RELEASE)
                               (and (= action MouseActionType/CLICK_DOWN)
                                    (let [pos (.getPosition ^MouseAction key)]
                                      (ScrollBar/isOnTrack Direction/VERTICAL
                                                           (.getColumn pos)
                                                           (.getRow pos)
                                                           (TerminalPosition. (int scrollbar-col)
                                                                              (int body-top))
                                                           (int list-budget)
                                                           2))))))
                    (let [^ScrollBar$DragResult drag
                          (ScrollBar/dragStep ^MouseAction key
                                              Direction/VERTICAL
                                              (TerminalPosition. (int scrollbar-col) (int body-top))
                                              (int list-budget)
                                              (int total)
                                              (int page-rows)
                                              (Integer/valueOf (int @scroll))
                                              (when (some? @scrollbar-drag-offset)
                                                (Integer/valueOf (int @scrollbar-drag-offset)))
                                              2)]
                      (when (and drag (.release drag)) (vreset! scrollbar-drag-offset nil))
                      (when-let [grip (and drag (.gripOffset drag))]
                        (vreset! scrollbar-drag-offset (long grip)))
                      (when-let [next-scroll (and drag (.scrollPosition drag))]
                        (let [next-scroll (long next-scroll)]
                          (reset! scroll next-scroll)
                          (swap! selected #(p/clamp %
                                                    next-scroll
                                                    (min (dec total)
                                                         (+ next-scroll (dec page-rows)))))))
                      (recur))
                    (and (input/ctrl-modifier? key)
                         (= KeyType/Character (key-type key))
                         (= (lower-key-character key) \n))
                    {:action :new}
                    (and (input/ctrl-modifier? key)
                         (= KeyType/Character (key-type key))
                         (= (lower-key-character key) \f))
                    (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                      {:action :fork :id id}
                      (recur))
                    (and (input/ctrl-modifier? key)
                         (= KeyType/Character (key-type key))
                         (= (lower-key-character key) \s))
                    ;; Ctrl+S toggles the human's star. The row already carries the
                    ;; gateway's rank, so the intent is simply its opposite.
                    (let [entry (and (pos? total) (nth visible-rows @selected))]
                      (if-let [id (:id (:target entry))]
                        {:action :favorite :id id :favorite? (not (:favorite? entry))}
                        (recur)))
                    (and (input/ctrl-modifier? key)
                         (= KeyType/Character (key-type key))
                         (= (lower-key-character key) \d))
                    (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                      {:action :delete :id id}
                      (recur))
                    (and (input/ctrl-modifier? key)
                         (= KeyType/Character (key-type key))
                         (= (lower-key-character key) \o))
                    (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                      {:action :group :id id}
                      (recur))
                    (and (input/ctrl-modifier? key)
                         (= KeyType/Character (key-type key))
                         (= (lower-key-character key) \b))
                    (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                      {:action :project :id id}
                      (recur))
                    (and (input/ctrl-char? key \r) @page-error) (do (start-page! @page-error)
                                                                    (recur))
                    (input/ctrl-char? key \u)
                    (do (swap! show-empty-untitled? not) (reset-list! false) (recur))
                    (= KeyType/PasteStart (.getKeyType ^KeyStroke key))
                    (do (let [pasted (drain-modal-paste! screen)]
                          (when (seq pasted)
                            (swap! query str (str/replace pasted #"\s+" " "))
                            (reset-list! true)))
                        (recur))
                    :else
                    (condp = (key-type key)
                      KeyType/Escape nil
                      KeyType/ArrowUp
                      (if (and (input/reorder-modifier? key) (pos? total))
                        (if-let [id (:id (:target (nth visible-rows @selected)))]
                          {:action :reorder :id id :dir :up}
                          (recur))
                        (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total))))
                            (recur)))
                      KeyType/ArrowDown
                      (if (and (input/reorder-modifier? key) (pos? total))
                        (if-let [id (:id (:target (nth visible-rows @selected)))]
                          {:action :reorder :id id :dir :down}
                          (recur))
                        (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total))))
                            (recur)))
                      KeyType/PageUp
                      (do (swap! selected #(p/clamp (- (long %) page-rows) 0 (max 0 (dec total))))
                          (recur))
                      KeyType/PageDown
                      (do (swap! selected #(p/clamp (+ (long %) page-rows) 0 (max 0 (dec total))))
                          (recur))
                      KeyType/Enter (if (pos? total) (:target (nth visible-rows @selected)) (recur))
                      KeyType/Backspace (do (swap! query #(if (seq %) (subs % 0 (dec (count %))) %))
                                            (reset-list! true)
                                            (recur))
                      KeyType/Character (let [character (key-character key)]
                                          (when (and character
                                                     (not (input/alt-modifier? key))
                                                     (not (input/ctrl-modifier? key))
                                                     (not (iso-control-character? character)))
                                            (swap! query str character)
                                            (reset-list! true))
                                          (recur))
                      (recur)))))))
          (finally (swap! search-generation inc)
                   (when-let [running @search-task]
                     (future-cancel running))
                   (when-let [running @page-task]
                     (future-cancel running))
                   (when-let [running @groups-task]
                     (future-cancel running))
                   (when stop-fleet! (stop-fleet!))))))))

;;; ── Command palette ─────────────────────────────────────────────────────────

(defn- band-frame!
  "Paint ONE frame of a band that will never read a key. A slash already said
   which command it is, so the band is the FRAME its follow-up question is asked
   in — the title, the rows and the hint bar the human would have seen — and not
   a menu to pick from."
  [^TerminalScreen screen g region spec]
  (let [{:keys [refresh!] :as host} (transient-host screen g)]
    (tr/paint! host region spec {:switches #{} :options {}})
    (refresh!)))

(defn- session-band-instance!
  "ONE band INSTANCE in the LIVE SESSION frame, opened around `body`.

   `anchor` is `state/band-anchor`: `:content-top` is the first row the band may
   touch, `:prompt-h` the live height of the prompt it sits above, and
   `:chat-left` the start of the chat pane. The band is clipped to that pane,
   leaving a docked project rail untouched. The frame is snapshotted before the
   band paints and put back on the way out — the transcript underneath is never
   repainted from scratch and never blanked.

   `body` is called with `[g region]`, the same two handles every other host of
   `embed-transient!` composes. This is the only place the session screen turns
   an anchor into a band region: a second one is how two bands drift apart."
  [^TerminalScreen screen {:keys [content-top prompt-h chat-left]} body]
  (let [size
        (modal-size! screen)

        left
        (min (max 0 (long (or chat-left 0))) (max 0 (dec (.getColumns size))))

        cols
        (- (.getColumns size) left)

        g
        (binding [frame/*column-offset* left]
          (frame/surface-graphics screen cols (.getRows size)))

        restore!
        (frame-restorer screen)

        region
        (assoc (tr/band-region cols (.getRows size) (or content-top 1) (or prompt-h tr/prompt-rows))
          :restore! restore!)]

    ;; The band owns the keyboard while it is up, so it owns the CURSOR: left
    ;; where the last session paint parked it, the hardware caret went on
    ;; blinking inside the prompt behind the hydra, as if the band were not
    ;; there. Anything inside the band that reads typed text (`band-questions`)
    ;; places it again for itself.
    (.setCursorPosition screen nil)
    (try (binding [frame/*column-offset* left]
           (body g region))
         (finally (when restore! (restore!))
                  (.setCursorPosition screen nil)
                  (frame/refresh! screen)))))

(defn session-band!
  "Run ONE transient as a BAND inside the LIVE SESSION frame — the same
   `embed-transient!` component Settings and the
   provider manager embed, instanced here over the session's own region
   (`session-band-instance!`) instead of in a window of its own.

   `f` is called with `{:screen :g :region :result}` ONLY when the transient
   produced an action, on the band's own rows: that is where an inline
   minibuffer (`band-questions`) asks its follow-up question, on the hint row,
   instead of opening a modal. Returns `f`'s value, or nil on Esc.

   `pressed` is an action ALREADY chosen by the caller. The band paints itself
   and goes straight to that action's question instead of waiting for a duplicate
   keystroke."
  ([^TerminalScreen screen anchor spec f] (session-band! screen anchor spec f nil))
  ([^TerminalScreen screen anchor spec f pressed]
   (session-band-instance! screen
                           anchor
                           (fn [g region]
                             (when-let [result (if pressed
                                                 (do (band-frame! screen g region spec)
                                                     {:action pressed :switches #{} :options {}})
                                                 (embed-transient! screen g region spec))]
                               (f {:screen screen :g g :region region :result result}))))))

(defn- pointer-drift?
  "Is `key` pure POINTER TRAFFIC — a move, a drag or a wheel notch — rather than
   an answer to a chord?

   With SGR mouse reporting on, the terminal sends a MouseAction for every cell
   the cursor crosses. A band that treats one of those as its second key is a
   band that vanishes when the hand on the desk nudges the mouse."
  [key]
  (and (instance? MouseAction key)
       (let [a (.getActionType ^MouseAction key)]
         (or (= a MouseActionType/MOVE)
             (= a MouseActionType/DRAG)
             (= a MouseActionType/SCROLL_UP)
             (= a MouseActionType/SCROLL_DOWN)))))

(defn- read-chord-key!
  "The next event that can ANSWER a chord: keep reading past pointer drift.

   `read-modal-key!` hands back whatever the terminal sent, wheel notches
   included. `input/resolve-prefix-key` reads anything that is not a key it
   knows as an abort, so one mouse MOVE used to close the band mid-chord."
  ^KeyStroke [^TerminalScreen screen]
  (loop []

    (let [key (read-modal-key! screen)]
      (if (pointer-drift? key) (recur) key))))

(defn prefix-band!
  "The C-x HYDRA: paint `spec` as a band in the LIVE SESSION frame, read the ONE
   keystroke that answers the chord, restore the frame and hand it BACK raw.

   Same band instance as `session-band!` (`session-band-instance!`), but
   deliberately not a `tr/run!`: the band advertises the chord, it does not own
   it. `input/resolve-prefix-key` still decides what the second key means, so
   C-x TAB, C-x ←/→ and C-x 1…9 keep working even though no row lists them, and a
   verb can never be reachable in the band but dead from the keyboard.

   POINTER DRIFT IS NOT AN ANSWER: the band waits through moves, drags and wheel
   notches (`read-chord-key!`) instead of letting the resolver read them as an
   abort. A CLICK still is one — that is a gesture, and it dismisses the band.

   Returns the `KeyStroke` (Esc included — the resolver reads it as an abort), or
   nil when the terminal had nothing to give."
  [^TerminalScreen screen anchor spec]
  (session-band-instance! screen
                          anchor
                          (fn [g region]
                            (band-frame! screen g region spec)
                            (read-chord-key! screen))))

(def palette-commands
  "Command palette entries. Each is {:id keyword :label str}. The `:id` is the
   action the screen's `run-command!` executes. Quit is intentionally NOT here
   — use Ctrl+C to quit.

   The palette is THE discoverable entry point for every app verb: opened with
   C-x p (reliable on every terminal, unlike Alt/Option chords on macOS) and
   filtered by typing."
  ;; Whole-session Markdown copy lives in the header as an icon.
  [{:id :search-open :label "Search in Session"} {:id :show-sessions :label "Switch Session"}
   {:id :session-metrics :label "Session Metrics"} {:id :pick-file :label "Attach File"}
   {:id :toggle-voice-recording :label "Voice Recording"} {:id :new-session :label "New Session"}
   ;; Both fork verbs are `:has-turns`-gated: a session with no turns has
   ;; nothing to fork, so the palette must not even offer them.
   {:id :fork-session :label "Fork Session" :show-when :has-turns}
   {:id :fork-at-turn :label "Fork Session at Turn…" :show-when :has-turns}
   {:id :close-tab :label "Close Tab"} {:id :providers :label "Providers"}
   {:id :mcp :label "MCP Servers"} {:id :settings :label "Settings"}
   {:id :session-settings :label "Session settings"} {:id :group-settings :label "Group settings"}
   {:id :project-settings :label "Project settings"}
   {:id :toggle-all-details :label "Fold / Unfold All"}
   {:id :toggle-detail-labels :label "Label Folds — jump to one"}
   {:id :toggle-help :label "Keyboard Shortcuts"}
   ;; Both views stay hidden while Improve is off. Settings → Experimental
   ;; keeps the opt-in flag and, when enabled, its mode control available.
   {:id :improve :label "Improve — Projects and Issues" :show-when :improve}
   {:id :improve-settings :label "Improve Mode…" :show-when :improve}])

(defn fork-turn-items
  "Rows for the fork-at-turn palette (`searchable-select!`), one per turn of the
   current session (from `db-list-session-turns`), top-to-bottom. Each row's
   `:label` is the turn's user message (whitespace-collapsed, truncated) and
   `:hint` its ordinal `tN`; `:turn-id` carries the `session_turn_soul` id the
   fork copies THROUGH — selecting a row forks the session keeping every turn up
   to and INCLUDING it. Type to filter by message text."
  [turns]
  (mapv (fn [i turn]
          (let [n
                (or (:position turn) (inc (long i)))

                req
                (some-> (:user-request turn)
                        str
                        str/trim)

                req
                (if (or (nil? req) (str/blank? req)) "(no message)" req)

                one-line
                (str/replace req #"\s+" " ")

                label
                (if (> (count one-line) 72) (str (subs one-line 0 71) "…") one-line)]

            {:label label :hint (str "t" n) :turn-id (:id turn)}))
        (range)
        turns))

(defn searchable-select!
  "Type-to-filter selection list — the searchable spine of the command palette.
   Thin wrapper over `list-dialog!` (filter on, content-sized, palette
   placeholder). Returns the FULL chosen item map (so callers recover
   `:id` / slash keys), or nil on Esc.

   The optional `opts` map overrides the filter field's `:placeholder` and the
   `:enter-label` — so callers other than the command palette (e.g. the project
   switcher) show a fitting prompt instead of \"Type a command…\"."
  ([^TerminalScreen screen title items] (searchable-select! screen title items nil))
  ([^TerminalScreen screen title items {:keys [placeholder enter-label]}]
   (list-dialog! screen
                 title
                 items
                 {:filter? true
                  :placeholder (or placeholder "Type a command…")
                  :enter-label (or enter-label "run")
                  :height :content})))

(defn palette-commands-for
  "`palette-commands` filtered to the entries that can ACT in `ctx`. Mirrors the
   which-key strip's `:show-when` gating: an entry tagged `:has-turns` (both
   Fork Session verbs) is DROPPED in a session with no turns — forking a
   turnless session is prohibited, so it must not even be discoverable.

   `ctx` is `{:has-turns? bool :improve? bool}`; a missing/nil ctx is the
   conservative case — turnless, and with Improve off. Untagged entries always
   survive."
  [{:keys [has-turns? improve?]}]
  (filterv (fn [{:keys [show-when]}]
             (case show-when
               :has-turns
               (boolean has-turns?)

               :improve
               (boolean improve?)

               true))
    palette-commands))

(defn command-palette!
  "Show the searchable command palette. Returns the FULL chosen command map
   (so the caller's `run-command!` can read `:id` and any slash keys), or nil
   on Esc. `extra-commands` are the engine slash roots appended after the
   built-ins. Opened with C-x C-p (Emacs C-x prefix + Ctrl+P).

   `ctx` (`{:has-turns? bool}`) gates context-only verbs via
   [[palette-commands-for]] — without turns the Fork Session entries are not
   listed at all."
  ([^TerminalScreen screen] (command-palette! screen [] nil))
  ([^TerminalScreen screen extra-commands] (command-palette! screen extra-commands nil))
  ([^TerminalScreen screen extra-commands ctx]
   ;; Each built-in carries its direct keybind as a dim right-aligned `:hint`
   ;; (opencode-style), so the palette doubles as a live keymap reference;
   ;; palette-only verbs and slash roots have no chord, so no hint.
   (let [with-hints (mapv (fn [c]
                            (assoc c :hint (keymap/label-for (:id c))))
                          (palette-commands-for ctx))]
     (searchable-select! screen "Command Palette" (vec (concat with-hints extra-commands))))))

(defn model-picker!
  "Searchable per-session model picker — TUI parity with the web footer
   chooser. Lists every configured model as a row (`<provider> / <model>`,
   the active one marked `● current`) plus a top `* router default` row
   that CLEARS the per-session override. `current` is the session's stored
   model preference (`{:provider <str|kw> :model <str>}`) or nil; it marks
   the active row exactly like the web picker. Returns the chosen item map
   — `{:reset? true}` for the router-default row, else `{:provider <str>
   :model <str>}` — or nil on Esc. Optional `opts` passes the pane's
   `:column-offset` resolver to `list-dialog!`. Without it, the picker is global."
  ([screen current] (model-picker! screen current {}))
  ([^TerminalScreen screen current opts]
   (let [providers
         (try
           (vis/picker-fleet)
           (catch Throwable t (tel/log! :warn ["dialogs: picker-fleet failed" (ex-message t)]) nil))

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
         (vec (cons {:label "* router default"
                     :hint (when (and (nil? cur-provider) (nil? cur-model)) "● current")
                     :reset? true}
                    model-rows))]

     (list-dialog! screen
                   "Session model"
                   items
                   (assoc opts
                     :filter? true
                     :placeholder "Type to filter models…"
                     :enter-label "choose"
                     :height :content)))))

(defn text-viewer-dialog!
  "Show a scrollable read-only text viewer dialog.
   `title` is the dialog header. `text` is a string (may contain newlines)
   that is rendered VERBATIM - same content the LLM receives, only soft-
   wrapped to fit the dialog width. No markdown, no truncation, no
   reformatting.
   Returns nil on Esc. Supports keyboard scrolling."
  [^TerminalScreen screen title text]
  (with-modal-background
    screen
    (let [scroll (atom 0)]
      (loop []

        (let [size (modal-size! screen)
              cols (.getColumns size)
              rows (.getRows size)
              g (frame/surface-graphics screen cols rows)
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
              scroll-col (+ (long left) (long inner-w))
              text-w (max 1 (- (long inner-w) 3))
              lines (vec (mapcat #(render/wrap-text % text-w)
                                 (str/split-lines (or text "(empty)"))))
              total (count lines)
              max-scroll (long (max 0 (- total (long content-h))))
              _ (swap! scroll #(p/clamp % 0 max-scroll))
              visible (subvec lines @scroll (min total (+ (long @scroll) (long content-h))))]

          ;; Body - verbatim line render, no ellipsization (wrap-text
          ;; already produced lines that fit `text-w`).
          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (doseq [[i line] (map-indexed vector visible)]
            (let [row (+ (long content-top) (long i))]
              (when (< row (+ (long content-top) (long content-h)))
                (p/fill-rect! g (inc (long left)) row inner-w 1)
                (p/put-str! g (+ (long left) 2) row line))))
          ;; Clear remaining rows in the content area
          (doseq [row (range (+ (long content-top) (count visible))
                             (+ (long content-top) (long content-h)))]
            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/fill-rect! g (inc (long left)) row inner-w 1))
          ;; Scrollbar - same style as the chat messages area: a vertical
          ;; track of │ plus a solid █ thumb sized proportionally to the
          ;; visible window. Drawn over the content's right margin, on the
          ;; dialog background so it visually blends with the dialog frame.
          (when (> total (long content-h))
            (let [track-h (long content-h)
                  ratio (/ (double content-h) total)
                  thumb-h (long (max 1 (int (* track-h ratio))))
                  den (long (max 1 max-scroll))
                  thumb-pos (int (* (- track-h thumb-h) (/ (double @scroll) den)))]

              (doseq [r (range track-h)]
                (p/set-colors! g t/dialog-border t/dialog-bg)
                (p/set-char! g
                             scroll-col
                             (+ (long content-top) (long r))
                             Symbols/SINGLE_LINE_VERTICAL))
              (doseq [r (range thumb-h)]
                (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                (p/set-char! g scroll-col (+ (long content-top) (long thumb-pos) (long r)) \█))))
          (draw-hint-bar! g left hint-row inner-w [["↑/↓" "scroll"] ["Esc" "close"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (frame/refresh! screen)
          (let [key (read-modal-key! screen)]
            (when key
              (condp = (key-type key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! scroll #(max 0 (dec (long %)))) (recur))
                KeyType/ArrowDown (do (swap! scroll #(min max-scroll (inc (long %)))) (recur))
                KeyType/PageUp (do (swap! scroll #(max 0 (- (long %) (long content-h)))) (recur))
                KeyType/PageDown (do (swap! scroll #(min max-scroll (+ (long %) (long content-h))))
                                     (recur))
                KeyType/Character (recur)
                (recur)))))))))

;;; ── Markdown viewer dialog ──────────────────────────────────────────────────
(defn md-run-paint!
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
    (+ (long x) (p/display-width text))))

(defn markdown-viewer-dialog!
  "Scrollable read-only MARKDOWN viewer: `md` is lifted to canonical IR
   (`vis/markdown->ast`) and painted with styled headings, bold, and code
   accents, tables — through the SAME IR walker the chat uses
   (`layout/ast->lines`). The rich twin of `text-viewer-dialog!`.
   Returns nil on Esc. Supports keyboard scrolling."
  [^TerminalScreen screen title md]
  (with-modal-background
    screen
    (let [scroll
          (atom 0)

          ir
          (try (vis/markdown->ast (str md))
               (catch Throwable t
                 (tel/log! :warn ["dialogs: markdown->ast failed" (ex-message t)])
                 nil))]

      (if (nil? ir)
        (text-viewer-dialog! screen title (str md))
        (loop []

          (let [size
                (modal-size! screen)

                cols
                (.getColumns size)

                rows
                (.getRows size)

                g
                (frame/surface-graphics screen cols rows)

                bounds
                (draw-dialog-chrome! g cols rows title (max 12 (- rows 8)))

                {:keys [left inner-w]}
                bounds

                {:keys [content-top content-h hint-row]}
                (dialog-layout bounds)

                scroll-col
                (+ (long left) (long inner-w))

                text-w
                (max 1 (- (long inner-w) 3))

                lines
                (try (layout/ast->lines ir text-w)
                     (catch Throwable t
                       (tel/log! :warn ["dialogs: ast->lines failed" (ex-message t)])
                       []))

                total
                (count lines)

                max-scroll
                (long (max 0 (- total (long content-h))))

                _
                (swap! scroll #(p/clamp % 0 max-scroll))

                visible
                (subvec (vec lines) @scroll (min total (+ (long @scroll) (long content-h))))]

            (doseq [[i line] (map-indexed vector visible)]
              (let [row (+ (long content-top) (long i))]
                (when (< row (+ (long content-top) (long content-h)))
                  (p/set-colors! g t/dialog-fg t/dialog-bg)
                  (p/fill-rect! g (inc (long left)) row inner-w 1)
                  (reduce (fn [x run]
                            (md-run-paint! g x row run))
                          (+ (long left) 2)
                          (:runs line)))))
            (doseq [row (range (+ (long content-top) (count visible))
                               (+ (long content-top) (long content-h)))]
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc (long left)) row inner-w 1))
            (when (> total (long content-h))
              (let [track-h
                    (long content-h)

                    ratio
                    (/ (double content-h) total)

                    thumb-h
                    (long (max 1 (int (* track-h ratio))))

                    den
                    (long (max 1 max-scroll))

                    thumb-pos
                    (int (* (- track-h thumb-h) (/ (double @scroll) den)))]

                (doseq [r (range track-h)]
                  (p/set-colors! g t/dialog-border t/dialog-bg)
                  (p/set-char! g
                               scroll-col
                               (+ (long content-top) (long r))
                               Symbols/SINGLE_LINE_VERTICAL))
                (doseq [r (range thumb-h)]
                  (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                  (p/set-char! g scroll-col (+ (long content-top) (long thumb-pos) (long r)) \█))))
            (draw-hint-bar! g left hint-row inner-w [["↑/↓" "scroll"] ["Esc" "close"]])
            (.setCursorPosition screen (p/cursor-pos 0 0))
            (frame/refresh! screen)
            (let [key (read-modal-key! screen)]
              (when key
                (condp = (key-type key)
                  KeyType/Escape nil
                  KeyType/ArrowUp (do (swap! scroll #(max 0 (dec (long %)))) (recur))
                  KeyType/ArrowDown (do (swap! scroll #(min max-scroll (inc (long %)))) (recur))
                  KeyType/PageUp (do (swap! scroll #(max 0 (- (long %) (long content-h)))) (recur))
                  KeyType/PageDown
                  (do (swap! scroll #(min max-scroll (+ (long %) (long content-h)))) (recur))
                  KeyType/Character (recur)
                  (recur))))))))))
