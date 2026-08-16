(ns com.blockether.vis.ext.channel-tui.dialogs
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.drafts :as drafts]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.magit :as magit]
            [com.blockether.vis.ext.channel-tui.highlight :as highlight]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.markdown-layout :as layout]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.channel-tui.transient :as tr]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.mcp-model :as mcp-model]
            [com.blockether.vis.internal.theme :as shared-theme]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna Symbols TerminalPosition TextCharacter]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]
           [java.text SimpleDateFormat]
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
  (let
    [terminal-w
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
  (let
    [terminal-h
     (max 8 (- rows 4))

     min-h
     (min (long t/dialog-min-height) terminal-h)

     box-h
     (-> (long (* rows (double t/dialog-height-ratio)))
         (max min-h)
         (min (long t/dialog-max-height))
         (min terminal-h))]

    (max 1 (- box-h (long t/dialog-chrome-h)))))

(defn clear-screen!
  "Fill the entire screen with terminal background. Call before sub-dialogs
   to cleanly replace the current dialog (wizard step pattern)."
  [^TerminalScreen screen]
  (let
    [size
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

(defn open-nested!
  "Run `f` with the screen cleared before AND after it, for a dialog opened
   from inside another dialog.

   Modals float over the CHAT on purpose, but a modal opened over another modal
   paints a SMALLER box on top of the parent's, so the parent's border, ✕ and
   hint bar keep framing it and the user sees two stacked popups. Erasing on the
   way in gives the nested flow a clean surface; erasing on the way out lets the
   caller repaint its own frame from scratch."
  [^TerminalScreen screen f]
  ;; `screen` is nil in unit tests that redefine every dialog away.
  (some-> screen
          clear-screen!)
  (try (f)
       (finally (some-> screen
                        clear-screen!))))

(defn frame-restorer
  "Snapshot the screen's back buffer NOW and return `(fn [] …)` / `(fn [from to])`
   that puts those rows back exactly as they look at this moment.

   A magit band paints over the host's rows, and when a SHORTER band replaces a
   taller one the rows between them belong to the HOST again. Blanking them
   punched a hole in the settings list behind the popup — the whole point of a
   band is that the buffer it is about stays readable. The host is not repainted
   while a band flow runs, so restoring the snapshot is the only honest answer.

   Returns nil when there is no screen: unit tests redefine every dialog away."
  [^TerminalScreen screen]
  (when screen
    (let
      [size
       (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

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

      (fn restore! ([] (restore! 0 (dec (long rows))))
        ([from to]
         (doseq [row (range (max 0 (long from)) (min (long rows) (inc (long to))))]
           (dotimes [col cols]
             (.setCharacter screen
                            (int col)
                            (int row)
                            ^TextCharacter (get-in snapshot [row col])))))))))

(defn with-frame-restored!
  "Run `f` — typically a nested dialog or a full-screen prompt — and put THIS
   frame back exactly as it was.

   `open-nested!` erases the screen on the way OUT, which is right when the
   caller repaints itself afterwards and catastrophic in the middle of a magit
   band flow: the next band lands on blank paper and everything above it is
   gone. Snapshotting the back buffer and writing it back costs one delta
   refresh and keeps the host frame on screen across the detour."
  [^TerminalScreen screen f]
  ;; `screen` is nil in unit tests that redefine every dialog away.
  (if-let [restore! (frame-restorer screen)]
    (try (f) (finally (restore!) (.refresh screen Screen$RefreshType/DELTA)))
    (f)))

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
    (let
      [terminal-box
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
   (let
     [top
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
  (let
    [last-start
     (max 0 (- total-count visible-count))

     start
     (p/clamp current-start 0 last-start)]

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
   Enter/Escape key types, and C-g to Escape. Some terminals surface modal
   Enter/Escape as `KeyType/Character`; modal code should not need to
   special-case that.

   C-g (Emacs `keyboard-quit`) is rewritten here too, so EVERY dialog closes on
   it through the `KeyType/Escape` branch it already has - one rewrite instead of
   an abort clause per key loop."
  [key]
  (if (and key (not (instance? MouseAction key)) (= KeyType/Character (key-type key)))
    (cond (input/ctrl-abort-key? key) (KeyStroke. KeyType/Escape)
          :else (case (key-character key)
                  (\newline \return)
                  (KeyStroke. KeyType/Enter)

                  \u001B
                  (KeyStroke. KeyType/Escape)

                  key))
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
          (let
            [pos (.getPosition ^MouseAction key)
             cx (.getColumn pos)
             cy (.getRow pos)]

            (and (= cy (:y b)) (>= (long cx) (long (:x0 b))) (<= (long cx) (long (:x1 b))))))))))

(def ^:private modal-close-hover (ThreadLocal/withInitial #(atom false)))

(defn update-modal-close-hover!
  "On a MOVE/DRAG event, set the thread-local close-hover flag when the cursor
   is within the recorded close-button bounds, clear it otherwise. Lets the
   modal close (X) button light up on hover like the header/overlay buttons."
  [key]
  (when (instance? MouseAction key)
    (let [a (.getActionType ^MouseAction key)]
      (when (or (= a MouseActionType/MOVE) (= a MouseActionType/DRAG))
        (let
          [b @(.get ^ThreadLocal modal-close-bounds)
           pos (.getPosition ^MouseAction key)
           cx (.getColumn pos)
           cy (.getRow pos)
           hit? (boolean
                  (and b (= cy (:y b)) (>= (long cx) (long (:x0 b))) (<= (long cx) (long (:x1 b)))))
           cell (.get ^ThreadLocal modal-close-hover)]

          (when (not= @cell hit?) (clojure.core/reset! cell hit?) true))))))

(defn read-modal-input!
  "Read one modal input event. Consecutive pending wheel events are drained
   and returned as one `:scroll-delta`, so a wheel flood costs one redraw.
   The first non-wheel event encountered while draining is held for the next
   modal read on this thread. MOVE/DRAG events also refresh the close (X)
   hover flag so the button can light up under the cursor."
  [^TerminalScreen screen]
  (let
    [pending-key
     (.get ^ThreadLocal modal-pending-key)

     key
     (normalize-modal-key (or @pending-key (.readInput screen)))]

    (reset! pending-key nil)
    (update-modal-close-hover! key)
    (cond (modal-close-click? key) {:key (KeyStroke. KeyType/Escape)}
          :else (if-let [delta (modal-wheel-delta key)]
                  (loop [acc (long delta)]
                    (if-let
                      [next-key (some-> (.pollInput screen)
                                        normalize-modal-key)]
                      (if-let [next-delta (modal-wheel-delta next-key)]
                        (recur (+ acc (long next-delta)))
                        (do (reset! pending-key next-key) {:scroll-delta acc}))
                      {:scroll-delta acc}))
                  {:key key}))))

(defn modal-input-pending?
  "True when another keystroke is ALREADY queued for this modal loop. The peeked
   event is stashed in the same thread-local slot `read-modal-input!` drains, so
   nothing is lost.

   This is the TUI's DEBOUNCE primitive: an expensive per-keystroke effect (the
   gateway transcript search) can skip itself while the user is still typing and
   run once on the keystroke that lands in a pause — no threads, no timers, and
   no repaint problem from an async result arriving while the loop blocks in
   `readInput`."
  [^TerminalScreen screen]
  (let [pending (.get ^ThreadLocal modal-pending-key)]
    (boolean (or (some? @pending)
                 (when-let [k (.pollInput screen)]
                   (reset! pending k)
                   true)))))

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
  (let
    [sep-w
     (p/display-width "  \u00b7  ")

     seg-w
     (fn [[k a]]
       (+ (p/display-width k) 1 (p/display-width a)))

     pairs
     (vec hint)]

    (loop
      [i
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
  (let
    [text-w
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
      (let
        [pairs
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

        (loop
          [i
           0

           col
           start]

          (when (< i n)
            (let
              [[k a]
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
      (vector? hint) (let
                       [joined
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
   this — the magit status buffer, the provider dialog, `transient-dialog!`."
  [^TerminalScreen screen g]
  {:g g
   :hint-bar! draw-hint-bar!
   :refresh! (fn []
               (.setCursorPosition screen nil)
               (.refresh screen Screen$RefreshType/DELTA))
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
  ;; Selection visual:
  ;;   col left   : │ (frame, painted by chrome)
  ;;   col left+1 : `•` cursor glyph (or blank if not selected)
  ;;   col left+2 : ` ` margin between marker and body
  ;;   col left+3+: body label (BOLD on selected)
  ;;
  ;; The 2-col `selection-prefix` (`• ` / `  `) is concatenated to the
  ;; label and the whole string is drawn at `(inc left)` so the marker
  ;; lands RIGHT AT the inner edge of the dialog (no padding column
  ;; between the frame and the marker), then a 1-col margin, then the
  ;; label — matching the project-wide `•`-cursor convention.
  ([g left row inner-w selected? label] (draw-list-item! g left row inner-w selected? label nil))
  ([g left row inner-w selected? label hint]
   ;; `hint` (optional) is a dim, right-aligned chip — e.g. a command's keybind
   ;; — drawn opposite the label (opencode's justify-between rows). The label is
   ;; truncated so it never collides with the hint.
   (let
     [prefix
      (p/selection-prefix selected?)

      hint
      (some-> hint
              str
              not-empty)

      hint-w
      (if hint (+ 2 (p/display-width hint)) 0)

      draw-text
      (ellipsize (str prefix label) (max 0 (- (long inner-w) 2 (long hint-w))))]

     (p/set-colors! g t/dialog-fg t/dialog-bg)
     (p/fill-rect! g (inc (long left)) row inner-w 1)
     (if selected?
       (p/styled g [p/BOLD] (p/put-str! g (inc (long left)) row draw-text))
       (p/put-str! g (inc (long left)) row draw-text))
     (when hint
       (p/set-colors! g t/dialog-hint t/dialog-bg)
       (p/put-str! g (- (+ (long left) (long inner-w)) (p/display-width hint)) row hint)
       (p/set-colors! g t/dialog-fg t/dialog-bg)))))

(defn draw-selectable-row!
  "The ONE focusable-row painter for LIST dialogs: `p/selection-prefix`'s cursor
   glyph, the text, bold while the cursor is on it.

   `draw-checkbox-item!` is this row with a status glyph in front of the label,
   and any new focusable LIST row joins them here instead of inventing a second
   way to look selected. A form is the other family: its rows are drawn as
   INPUTS on their own surface (`draw-field-row!`) and wear no cursor glyph at
   all, because a `•` in front of every row says the same thing about all of
   them."
  [g left row inner-w selected? text]
  (let
    [draw-text (ellipsize (str (p/selection-prefix selected?) text) (max 0 (- (long inner-w) 2)))]
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc (long left)) row inner-w 1)
    (if selected?
      (p/styled g [p/BOLD] (p/put-str! g (inc (long left)) row draw-text))
      (p/put-str! g (inc (long left)) row draw-text))))

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
  "MULTI-choice LIST row — cursor glyph, a `[✓]`/`[ ]` box, then the label. The
   cursor glyph and the checkbox glyph carry independent meaning: the first says
   \"this row is the cursor\", the second says \"this option is currently on\".
   Anchored at `(inc left)` so the marker sits right at the dialog's inner edge
   (see `draw-list-item!`)."
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
   was in: the focused field looked like it had escaped the box, and in a magit
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
  (let
    [content-w
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
  (let
    [content-w
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
   (let
     [prompt
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
  "Paint a clickable X close button at a dialog's top-right title row and
   record its click bounds (thread-local) so `read-modal-input!` can turn a
   click into Escape. Every dialog inherits it via `draw-dialog-chrome!`.
   Lights up to the red pill (`close-button-hover-fg` + bold) when the
   thread-local close-hover flag is set - the same affordance the header and
   help/tasks overlay close buttons use - so modal X buttons are no longer
   static."
  [g box-right title-row]
  (let
    [label
     " \u2715 "

     x1
     (- (long box-right) 1)

     x0
     (- (long x1) (dec (count label)))

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
   (let
     [cols
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

     ;; Shadow - clipped to terminal bounds
     (let
       [shd-left
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
     (let
       [title-row
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

(defn draw-flat-dialog-chrome!
  "Flat variant of `draw-dialog-chrome!`: no drop shadow, no accent title
   stripe, no separators - one thin-bordered rect on the dialog background
   with the title inline on the top border. Same default footprint and the
   same bounds map as the boxed chrome, so `dialog-layout` works unchanged."
  [g ^long cols ^long rows title]
  (let
    [content-w
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
  (let
    [[box-w box-h]
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

   run-modal! owns everything the old dialogs copy-pasted: terminal sizing, the
   `TextGraphics`, wheel/close/Esc normalization (via `read-modal-key!`), the
   cursor + DELTA refresh, and the recur loop. A key handler returns the next
   state to continue, or `{::done v}` to close the modal with value `v` (nil on
   Esc/close). Because `:measure`/`:reconcile`/`:on-key` are pure functions of
   data, a dialog's geometry and key logic can be unit-tested with no live
   terminal at all — the React-like win."
  [^TerminalScreen screen {:keys [init measure reconcile paint on-key]}]
  (loop [state (if (fn? init) (init) init)]
    (let
      [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
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
  (let
    [header
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
       (let
         [visible
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
     :paint (fn
              [g {:keys [selected scroll]}
               {:keys [cols rows title visible total footer content-w content-h-req bounds
                       content-top content-h hint-row grid-top list-h widths aligns head-cells]}]
              (let
                [{:keys [left inner-w]}
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
         (if-let [wheel (modal-wheel-step key)]
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
  [title items {:keys [filter? placeholder enter-label height]}]
  (let
    [items
     (vec items)

     content?
     (= height :content)

     head-rows
     (if filter? 2 0)]

    {:init {:query "" :selected 0 :scroll 0}
     :measure
     (fn [{:keys [query]} cols rows]
       (let
         [filtered
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
                  (let [selected (p/clamp (:selected state) 0 (max 0 (dec (long total))))]
                    (assoc state
                      :selected selected
                      :scroll (visible-window-start selected (:scroll state) list-h total))))
     :paint
     (fn
       [g {:keys [selected scroll query]}
        {:keys [cols rows title filtered total footer content-w content-h-req bounds content-top
                content-h hint-row list-top list-h filter? placeholder]}]
       (let [{:keys [left right inner-w]} bounds]
         (draw-dialog-chrome! g cols rows title content-w content-h-req)
         (p/set-colors! g t/dialog-fg t/dialog-bg)
         (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
         (let
           [cursor
            (when filter?
              (draw-text-input-field! g left content-top inner-w query (count query) placeholder))]
           (when filter?
             (p/set-colors! g t/dialog-border t/dialog-bg)
             (p/draw-separator! g left right (inc (long content-top))))
           (dotimes [i (min (long list-h) (long total))]
             (let
               [idx (+ (long scroll) (long i))
                row (+ (long list-top) (long i))]

               (when (< (long idx) (long total))
                 (let [item (nth filtered idx)]
                   (draw-list-item! g
                                    left
                                    row
                                    (if (> (long total) (long list-h)) (dec (long inner-w)) inner-w)
                                    (= idx selected)
                                    (:label item)
                                    (:hint item))))))
           (when (> (long total) (long list-h))
             (scrollbar/draw! g
                              {:col (+ (long left) (long inner-w))
                               :top list-top
                               :track-h list-h
                               :total-h total
                               :inner-h list-h
                               :scroll scroll}))
           (draw-hint-bar! g left hint-row inner-w footer)
           cursor)))
     :on-key
     (fn [{:keys [selected query] :as state} key {:keys [total filtered]}]
       (let [clampf #(p/clamp % 0 (max 0 (dec (long total))))]
         (if-let [wheel (modal-wheel-step key)]
           (assoc state :selected (clampf (+ (long selected) (long wheel))))
           (condp = (key-type key)
             KeyType/Escape {::done nil}
             KeyType/ArrowUp (assoc state :selected (clampf (dec (long selected))))
             KeyType/ArrowDown (assoc state :selected (clampf (inc (long selected))))
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
                   field), capped; nil uses the shared (tall) footprint."
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
  [^TerminalScreen screen title items]
  (let
    [items
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

      (let
        [size
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
            (let
              [idx (+ (long @scroll) (long i))
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
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (if (nil? key)
            (recur)
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/ArrowUp (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total))))
                                  (recur))
              KeyType/ArrowDown (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total))))
                                    (recur))
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
  (let
    [lines*
     (atom (vec lines))

     scroll
     (atom 0)

     follow
     (atom (boolean tail?))]

    (loop []

      (let
        [size
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
          (let
            [idx (+ (long @scroll) (long i))
             row (+ (long content-top) (long i))]

            (when (< (long idx) (long total))
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc (long left)) row inner-w 1)
              (p/put-str! g (+ (long left) 2) row (ellipsize (nth wrapped idx) text-w)))))
        (scrollbar/draw! g
                         {:col (+ (long left) (long inner-w))
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
        (let
          [key
           (read-modal-key! screen)

           wheel
           (modal-wheel-step key)

           move!
           (fn [f]
             (reset! follow false)
             (swap! scroll #(p/clamp (f %) 0 max-scroll)))]

          (cond (nil? key) (recur)
                wheel (do (move! #(+ (long %) (long wheel))) (recur))
                :else (condp = (key-type key)
                        KeyType/Escape nil
                        KeyType/Enter nil
                        KeyType/ArrowUp (do (move! dec) (recur))
                        KeyType/ArrowDown (do (move! inc) (recur))
                        KeyType/PageUp (do (move! #(- (long %) (max 1 (long content-h)))) (recur))
                        KeyType/PageDown (do (move! #(+ (long %) (max 1 (long content-h)))) (recur))
                        KeyType/Home (do (reset! follow false) (reset! scroll 0) (recur))
                        KeyType/End
                        (do (reset! follow (boolean tail?)) (reset! scroll max-scroll) (recur))
                        KeyType/Character (do (when (and refresh-fn
                                                         (= (lower-key-character key) \r))
                                                (reset! lines* (vec (refresh-fn)))
                                                (when tail? (reset! follow true)))
                                              (recur))
                        (recur))))))))

(defn log-view-dialog!
  "FULLSCREEN, syntax-highlighted log viewer — the whole terminal, edge to edge.

   Unlike `text-view-dialog!` (a centered modal box) this owns the entire screen:
   a title strip on the top row, the log body filling every row beneath it, and a
   hint strip on the bottom row. Each line is colorized by parsing the WHOLE
   buffer with tree-sitter (`highlight/highlight`, default the `bash` grammar) and
   painting the resulting ANSI runs through `render/paint-ansi-line!` — the same
   path that carries syntax color on the transcript's code fences. Fails open to
   plain text when the native grammar pack isn't loadable.

   Keys: ↑/↓ line, PgUp/PgDn page, Home/End jump, mouse-wheel scroll, `r` refresh
   (when `:refresh-fn`), Enter/Esc close. Options:
   - :refresh-fn  thunk returning fresh lines — enables `r` refresh so a live
                  buffer (e.g. background-shell logs) can be re-pulled in place.
   - :tail?       start pinned to the newest line and re-follow the bottom on
                  refresh (log-tail behaviour); scrolling up releases the pin.
   - :grammar     tree-sitter grammar for coloring (default \"bash\"); nil = plain.
   Returns nil after close."
  [^TerminalScreen screen title lines & {:keys [refresh-fn tail? grammar] :or {grammar "bash"}}]
  (let
    [lines*
     (atom (vec lines))

     scroll
     (atom 0)

     follow
     (atom (boolean tail?))]

    (loop []

      (let
        [size
         (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

         cols
         (.getColumns size)

         rows
         (.getRows size)

         g
         (.newTextGraphics screen)

         cur-lines
         @lines*

         ;; Colorize the WHOLE buffer at once (cached by [grammar source]) so
         ;; multi-line shell constructs classify correctly and identical
         ;; buffers aren't re-parsed on every scroll keystroke. nil = plain.
         colored
         (when (and grammar (seq cur-lines))
           (some-> (highlight/highlight grammar (str/join "\n" (map str cur-lines)))
                   str/split-lines))

         painted
         (if (and colored (= (count colored) (count cur-lines))) (vec colored) (mapv str cur-lines))

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

        ;; Whole-screen wipe, then the code-block background under the body.
        (render/fill-background! g cols rows)
        (p/set-colors! g t/code-block-fg t/code-block-bg)
        (p/fill-rect! g 0 body-top cols body-h)
        ;; Top strip: title left, tail/position indicator right.
        (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
        (p/fill-rect! g 0 title-row cols 1)
        (let
          [tag
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
          (let
            [idx (+ (long @scroll) (long i))
             y (+ body-top i)]

            (when (< (long idx) (long total))
              (render/paint-ansi-line! g 0 y (nth painted idx) t/code-block-fg t/code-block-bg))))
        ;; Scrollbar last (over the rightmost column) so a wide line can't hide it.
        (scrollbar/draw! g
                         {:col (dec cols)
                          :top body-top
                          :track-h body-h
                          :total-h total
                          :inner-h body-h
                          :scroll @scroll})
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
        (.refresh screen Screen$RefreshType/DELTA)
        (let
          [key
           (read-modal-key! screen)

           wheel
           (modal-wheel-step key)

           move!
           (fn [f]
             (reset! follow false)
             (swap! scroll #(p/clamp (f %) 0 max-scroll)))]

          (cond (nil? key) (recur)
                wheel (do (move! #(+ (long %) (long wheel))) (recur))
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
   :flat? true selects the minimal inline-border chrome."
  [^TerminalScreen screen title label & {:keys [mask initial body flat?] :or {initial ""}}]
  (let
    [text
     (atom (vec initial))

     cursor
     (atom (count initial))

     body-lines
     (text-input-body-lines body)

     paste-buffer
     (volatile! nil)]

    (loop []

      (let
        [size
         (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

         cols
         (.getColumns size)

         rows
         (.getRows size)

         g
         (.newTextGraphics screen)

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
                  (let
                    [payload (.toString sb)
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
                      KeyType/Backspace (do (when (pos? (long @cursor))
                                              (swap! text #(into (subvec % 0 (dec (long @cursor)))
                                                                 (subvec % @cursor)))
                                              (swap! cursor dec))
                                            (recur))
                      KeyType/ArrowLeft (do (swap! cursor #(max 0 (dec (long %)))) (recur))
                      KeyType/ArrowRight (do (swap! cursor #(min (count @text) (inc (long %))))
                                             (recur))
                      (recur)))))))))

;;; ── Confirm dialog ──────────────────────────────────────────────────────────
(defn- draw-button!
  "Draw a confirm-dialog action button in the shared blockether look (mirroring
   `components/action-button!` and the spel-bridge modal): every state is the same
   filled ` label ` pill and only the COLOUR differs. `Yes` is the PRIMARY cap (ink
   fill, cream bold label) and `No` the muted secondary — and whichever one the
   choice sits on takes the ACCENT fill, the same colour the active tab wears. No
   `▏`/`▕` rails and no marker glyph: a button here is a solid pill and focus is a
   colour. Same width in every state, so the row stays put as the choice moves.
   Returns the consumed width."
  [g col row label {:keys [variant is-focused]}]
  (let
    [col
     (long col)

     w
     (+ 2 (count label))

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
  (let
    [raw-lines
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

      (let
        [size
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
        (.refresh screen Screen$RefreshType/DELTA)
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
              (recur))))))))

;;; ── Magit status dialog (C-x g / footer git button) ─────────────────────────

(def ^:private magit-hints
  "Hint-bar chords for the magit status buffer — the most important magit
   verbs, one key each. Keys are CASE-SENSITIVE, exactly like Emacs magit
   (`s` ≠ `S`, `u` ≠ `U`, `f` ≠ `F`). See the full key-for-key reference banner
   atop `magit.clj`; the same usage is summarised here:

   Navigation  ↑/↓ move a row · n/p next/prev SECTION · PageUp/Down page
               Home/End top/bottom · TAB fold diff · RET visit · q/Esc close
   Staging     s/u stage/unstage at point · S/U stage/unstage ALL
   Discard     x/k discard at point (asks first)
   Commit      c commit transient (flag -h --no-verify · c commit · a amend)
   History     l log graph · C-w copy sha/path/ref
   Remote      P push · F pull · f fetch
   Branch      b branch flow      Stash  z stash flow
   Buffer      g refresh

   Magit-compatible bindings use Magit's own verbs, with `x` as an additional
   discard alias alongside Magit's `k`.
   Unimplemented Magit keys stay FREE:
   r rebase · y show-refs · d/D diff · m merge · V revert ·
   A cherry-pick · t tag · G refresh-all · SPC/DEL scroll · M-n/M-p sibling."
  [["↑/↓" "move"] ["n/p" "section"] ["TAB" "diff"] ["RET" "visit"] ["s/u" "±stage"] ["S/U" "all"]
   ["x/k" "discard"] ["c" "commit"] ["l" "log"] ["C-w" "copy"] ["P" "push"] ["F" "pull"]
   ["f" "fetch"] ["b" "branch"] ["z" "stash"] ["g" "refresh"] ["Esc" "close"]])

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
  (let
    [full
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
  (let
    [fut (future (try (thunk)
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
  [root rows idx {:keys [kind area path hunk]}]
  (cond (and (= :diff kind) (= :unstaged area)) (magit/stage-hunk! root {:path path :hunk hunk})
        (and (= :diff kind) (= :staged area)) {:ok? false :msg "Hunk already staged (u to unstage)"}
        (and (= :file kind) (= :staged area)) {:ok? false :msg "Already staged"}
        (= :file kind) (magit/stage-file! root path)
        (and (= :section kind) (contains? #{:untracked :unstaged :unmerged} area))
        (magit-section-action! rows idx #(magit/stage-file! root %))
        :else nil))

(defn- magit-unstage-action!
  [root rows idx {:keys [kind area path hunk]}]
  (cond (and (= :diff kind) (= :staged area)) (magit/unstage-hunk! root {:path path :hunk hunk})
        (and (= :diff kind) (= :unstaged area)) {:ok? false :msg "Hunk not staged (s to stage)"}
        (and (= :file kind) (= :staged area)) (magit/unstage-file! root path)
        (and (= :section kind) (= :staged area))
        (magit-section-action! rows idx #(magit/unstage-file! root %))
        (= :file kind) {:ok? false :msg "Not staged"}
        :else nil))

;;; ── One question, in the band's own frame ───────────────────────────────────
;; A band asks its follow-up question by REPLACING the commands that led to it.
;; Painting `Name the draft:` over the hint row while `c`/`d`/`s`/`k` were still
;; listed above it advertised commands the reader had already stolen: every
;; letter typed into the name looked like a command that did nothing. So a
;; question is a BAND, like everything else here — the prompt is its bold title,
;; and under it sits the ONE thing that can answer it: a real input row for typed
;; text, the answers themselves (`y`/`n`, a list of choices) for a keyed one.

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
   same hint row, no second window. THE band component: the session screen, the
   magit status buffer, Settings, the provider manager and `transient-dialog!`
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
   title and `hints` its hint bar. Returns the single body row the answer is
   painted on."
  [g {:keys [left inner-w text-w restore!] :as region} title hints]
  (let
    [{:keys [sep-row title-row title-rule-row body-top foot-rule-row foot-row wipe-top top-limit]}
     (tr/band-geometry region 1 true)]
    (when restore! (restore! top-limit (dec (long wipe-top))))
    (tr/clear-rows! g region (max (long top-limit) (long wipe-top)) foot-row)
    (when (>= (long sep-row) (long top-limit)) (tr/draw-rule! g region sep-row))
    (when (> (long title-rule-row) (long title-row)) (tr/draw-rule! g region title-rule-row))
    (when (> (long foot-rule-row) (max (long sep-row) (long top-limit)))
      (tr/draw-rule! g region foot-rule-row))
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/styled g [p/BOLD] (p/put-str! g (+ (long left) 2) title-row (ellipsize (str title) text-w)))
    (draw-hint-bar! g left foot-row inner-w hints)
    body-top))

(defn magit-mini-read!
  "Ask ONE typed question in the band's own frame: `label` becomes the band's
   title and the answer is typed into a real input row under it, so nothing the
   keyboard no longer owns stays advertised. Enter submits the trimmed string
   (may be empty), Esc returns nil. Opts: :initial (seed text), :mask (echo
   char), :placeholder (dim hint while the field is empty)."
  [^TerminalScreen screen g {:keys [left inner-w] :as region} label
   {:keys [initial mask placeholder]}]
  (let
    [text
     (atom (vec (or initial "")))

     cursor
     (atom (count (or initial "")))]

    (loop []

      (let
        [row
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
        (.refresh screen Screen$RefreshType/DELTA)
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
    (magit-mini-read! screen g region (or prompt (str label ":")) {:initial current :mask mask})))

(defn- magit-mini-choose!
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

(defn- magit-mini-confirm!
  "Ask y/n in the band's own frame: the question is inked ON the band's opening
   rule and `Yes` / `No` are the only rows under it. Returns true / false / nil (Esc)."
  [^TerminalScreen screen g region question]
  (case
    (:action (embed-transient! screen
                               g
                               region
                               {:title question
                                :groups [{:items [{:key "y" :type :action :id :yes :label "Yes"}
                                                  {:key "n" :type :action :id :no :label "No"}]}]}))
    :yes
    true

    :no
    false

    nil))

(defn- magit-discard-flow!
  [mini root {:keys [kind area path] :as row}]
  (when (= :file kind)
    (when ((:confirm! mini)
            (if (= :untracked area)
              (str "Delete untracked file " path "?")
              (str "Discard changes in " path "?")))
      (magit/discard-file! root row))))

(defn- magit-commit-flow!
  "Magit's `c` commit transient (`tr/run!`), keyed exactly like Emacs
   magit's commit popup. FLAGS: `-h` Disable hooks (`--no-verify`) — a TOGGLE (press
   `h` to arm it, press it again to disarm it), the escape hatch for a repo whose
   pre-commit/commit-msg githook is broken or irrelevant. COMMANDS: `c` commit the
   staged index, `a` amend the last commit (magit's `c c` / `c a`). The message is
   then read INLINE. Commit verification runs off the render/input thread while
   `busy!` repaints progress in the status buffer's footer."
  [busy! mini root model]
  (when-let
    [{:keys [action switches]}
     ((:transient! mini)
       {:title "Commit"
        :groups
        [{:title "Arguments"
          :items
          [{:key "h" :type :switch :id :no-verify :label "Disable hooks" :arg "--no-verify"}]}
         {:title "Commands"
          :items [{:key "c" :type :action :id :commit :label "Commit staged"}
                  {:key "a" :type :action :id :amend :label "Amend last commit"}]}]})]
    (let
      [amend? (= :amend action)
       no-verify? (contains? switches :no-verify)]

      (if (and (not amend?) (empty? (:staged model)))
        {:ok? false :msg "Nothing staged — stage with s/S first"}
        (when-let
          [msg ((:read! mini)
                 (if amend? "Amend message:" "Commit message:")
                 {:initial (if amend? (or (magit/last-commit-message root) "") "")})]
          (if (str/blank? msg)
            {:ok? false :msg "Empty message — commit aborted"}
            (run-async-with-ticker!
              #(magit/commit! root msg {:amend? amend? :no-verify? no-verify?})
              #(busy! "Verifying and committing")
              80)))))))

;;; ── One band, every question it can ask ─────────────────────────────────────
;; A band IS a region — six coordinates — plus the screen it paints on. They are
;; bound ONCE, here, and every host COMPOSES the result: the magit status
;; buffer, the provider manager, Settings, the session band and
;; `transient-dialog!` open their sub-transients through the same host and ask
;; their follow-up questions on their own hint row through the same minibuffers.
;; A caller that unpacks `:left`/`:inner-w`/`:hint-row`/`:text-w` again, or
;; reaches for `tr/run!` plus `transient-host` itself, is how two bands drift
;; apart.

(defn band-questions
  "Everything a band can ASK, bound to its own `region` once:

     `:read!`        one typed answer, in the band's own frame — `[label]` / `[label opts]`
     `:choose!`      WHICH one, single-key — `[title choices]`, returns the `:id`
     `:confirm!`     y/n — `[question]`
     `:transient!`   ANOTHER transient over the SAME band region — `[spec]`
     `:read-option`  the `:read-option` a spec with OPTION items hands `tr/run!`

   A transient that opens a transient, and a question that REPLACES the commands
   that led to it instead of opening a second frame, is how magit asks a second
   thing — every band in the TUI does both through this map."
  [^TerminalScreen screen g region]
  {:read! (fn read! ([label] (read! label {}))
            ([label opts] (magit-mini-read! screen g region label opts)))
   :choose! (fn [title choices]
              (magit-mini-choose! screen g region title choices))
   :confirm! (fn [question]
               (magit-mini-confirm! screen g region question))
   :transient! (fn [spec]
                 (embed-transient! screen g region spec))
   :read-option (region-option-reader screen g region)})

(defn transient-dialog!
  "Host ONE magit transient in its OWN modal — the popup for flows that have no
   status buffer to sit in (provider authentication). `body` (a string or lines)
   is the caller's guidance, painted once at the top of the content area; the
   transient owns every row under it and its hint bar lands on the dialog's own
   hint row. The box is sized to what it actually holds, so a two-line prompt
   opens a small dialog instead of a half-screen one.

   OPTION items are read INLINE on that hint row (`magit-mini-read!`), honouring
   the item's `:prompt` (default `<label>:`) and `:mask` (`\\*` for a credential);
   mark such an item `:secret? true` and its value renders as dots, never as
   text. `spec` may carry a `:title` for the popup itself when the frame's title
   would read redundantly.

   Returns `tr/run!`'s `{:action :switches :options}`, or nil on Esc."
  [^TerminalScreen screen title body spec]
  (let
    [size
     (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

     cols
     (.getColumns size)

     rows
     (.getRows size)

     g
     (.newTextGraphics screen)

     est-w
     (max 1 (- (default-content-width cols) 2))

     wrapped
     (->> (text-input-body-lines body)
          (mapcat (fn [line]
                    (if (str/blank? line) [""] (render/wrap-text line est-w))))
          vec)

     ;; The popup's own footprint — the component knows it (`tr/height`), so the
     ;; box is sized by what the transient will actually paint.
     popup-h
     (tr/height spec)

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
    (let
      [region {:left left
               :inner-w inner-w
               :hint-row hint-row
               :text-w text-w
               :min-row (+ content-top (count wrapped) (long body-gap))}]
      (embed-transient! screen
                        g
                        region
                        (assoc spec
                          :title (or (:title spec) title)
                          :read-option (region-option-reader screen g region))))))

(defn- magit-push-flow!
  "Magit-style push transient (`tr/run!`). SWITCHES (all optional):
   force-with-lease, dry-run, disable hooks (--no-verify), set-upstream. `p` is
   ALWAYS the direct push — to the branch's upstream, or to the repo's push
   remote when it has none. A Gerrit repo ADDS `r Push for review →
   refs/for/<branch>` and a `t Topic` OPTION BESIDE it instead of replacing it:
   magit binds the review push as an EXTRA key, and a repo whose refs/heads are
   pushable directly keeps both paths. Every remote `p` does not already target
   is listed INLINE as its own `Push to <remote>` action in the SAME overlay
   (magit lists push targets in the transient — no second dialog)."
  [busy! mini root]
  (let
    [upstream
     (magit/upstream-name root)

     remotes
     (magit/remotes root)

     push-target
     (magit/push-remote root)

     g-remote
     (magit/gerrit-remote root)

     g-branch
     (magit/gerrit-target-branch root)

     branch
     (magit/current-branch root)

     gerrit?
     (some? g-remote)

     ;; `p` already lands on the push remote — every OTHER remote, the Gerrit one
     ;; included (a refs/heads push to it is a legitimate target), gets its own row.
     other-remotes
     (->> remotes
          (map :name)
          (remove #(= push-target %))
          (take 9)
          vec)

     arg-items
     (cond->
       [{:key "f" :type :switch :id :force :label "Force with lease" :arg "--force-with-lease"}
        {:key "n" :type :switch :id :dry-run :label "Dry run" :arg "--dry-run"}
        {:key "h" :type :switch :id :no-verify :label "Disable hooks" :arg "--no-verify"}
        {:key "u" :type :switch :id :set-upstream :label "Set upstream" :arg "-u"}]
       gerrit?
       (conj {:key "t" :type :option :id :topic :label "Topic" :arg "%topic="}))

     primary
     {:key "p"
      :type :action
      :id :push
      :label (str "Push"
                  (when-let [target (or upstream push-target)]
                    (str " to " target)))}

     review-rows
     (if gerrit?
       [{:key "r" :type :action :id :review :label (str "Push for review → refs/for/" g-branch)}]
       [])

     ;; Each remaining remote becomes its own inline action row keyed by a
     ;; digit — magit lists push targets in the SAME transient, never a
     ;; second dialog.
     remote-rows
     (map-indexed (fn [i name]
                    {:key (str (inc (long i)))
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
     (vec (concat [primary] review-rows remote-rows))

     spec
     {:title "Push"
      :groups [{:title "Arguments" :items arg-items} {:title "Commands" :items push-items}]
      :read-option (fn [{:keys [id]} current]
                     (when (= id :topic)
                       ((:read! mini)
                         "Topic:"
                         {:initial
                          (or current (when (and branch (not= branch g-branch)) branch) "")})))}]

    (when-let [{:keys [action switches options]} ((:transient! mini) spec)]
      (let
        [base {:set-upstream? (contains? switches :set-upstream)
               :force? (contains? switches :force)
               :dry-run? (contains? switches :dry-run)
               :no-verify? (contains? switches :no-verify)}
         topic (:topic options)
         ;; With an upstream a bare `git push` follows it (magit's `p`); without one
         ;; the target is spelled out, or git would fall back to an absent `origin`.
         push-opts (cond-> base
                     (and (nil? upstream) push-target)
                     (assoc :remote push-target))]

        ;; The review push gets every armed switch — `gerrit-push!` refuses the ones
        ;; a refs/for ref cannot carry instead of dropping them without a word.
        (cond (= action :review)
              (run-network! busy!
                            "Pushing for review"
                            #(magit/gerrit-push!
                               root
                               (merge base {:remote g-remote :branch g-branch :topic topic})))
              (= action :push) (run-network! busy! "Pushing" #(magit/push! root push-opts))
              (contains? remote-action->name action)
              (let [rname (get remote-action->name action)]
                (run-network! busy!
                              (str "Pushing to " rname)
                              #(magit/push! root (assoc base :remote rname))))
              :else nil)))))

(defn- magit-branch-flow!
  [mini root]
  (when-let
    [id ((:choose! mini)
          "Branch:"
          [{:key \b :label "checkout" :id :checkout}
           {:key \c :label "create & checkout" :id :create} {:key \k :label "delete" :id :delete}])]
    (case id
      :checkout
      (when-let [nm ((:read! mini) "Checkout branch:" {})]
        (when-not (str/blank? nm) (magit/checkout-branch! root (str/trim nm))))

      :create
      (when-let [nm ((:read! mini) "Create branch:" {})]
        (when-not (str/blank? nm) (magit/create-branch! root (str/trim nm))))

      :delete
      (when-let [nm ((:read! mini) "Delete branch:" {})]
        (when-not (str/blank? nm)
          (let
            [nm (str/trim nm)
             r (magit/delete-branch! root nm {})]

            (if (:ok? r)
              r
              (if ((:confirm! mini) (str (:msg r) " — force delete " nm "?"))
                (magit/delete-branch! root nm {:force? true})
                r))))))))

(defn- magit-stash-flow!
  "`selected-ref` is the stash under the cursor (when the cursor sits on a
   stash row), else the newest stash is the target of pop/apply/drop."
  [mini root selected-ref model]
  (let
    [ref
     (or selected-ref (:ref (first (:stashes model))))

     choices
     (cond-> [{:key \z :label "stash working tree" :id :push}]
       ref
       (into [{:key \p :label (str "pop " ref) :id :pop}
              {:key \a :label (str "apply " ref) :id :apply}
              {:key \k :label (str "drop " ref) :id :drop}]))]

    (when-let [id ((:choose! mini) "Stash:" choices)]
      (case id
        :push
        (when-some [m ((:read! mini) "Stash message:" {})]
          (magit/stash-push! root m))

        :pop
        (magit/stash-pop! root ref)

        :apply
        (magit/stash-apply! root ref)

        :drop
        (when ((:confirm! mini) (str "Drop " ref "?")) (magit/stash-drop! root ref))))))

(defn- magit-log-flow!
  "Magit's `l` log transient, TUI-style. An inline `:choose!` picks the scope —
   the current branch's history or every ref — then the fullscreen ANSI log
   viewer opens on git's own colored `--graph` output. `r` inside re-pulls it.
   Returns nil (the log viewer is its own screen; nothing to echo/refresh)."
  [^TerminalScreen screen mini root]
  (when-let
    [id ((:choose! mini)
          "Log:"
          [{:key \l :label "current branch" :id :current}
           {:key \a :label "all branches" :id :all}])]
    (let
      [all? (= id :all)
       reload #(magit/log-graph-lines root {:all? all?})
       title (str "Git log — " (if all? "all branches" (or (magit/current-branch root) "HEAD")))]

      (log-view-dialog! screen title (reload) :grammar nil :refresh-fn reload)
      (clear-screen! screen)))
  nil)

(defn- magit-copy-action!
  "Magit's `y`: copy the identifier under the cursor onto the system clipboard —
   a commit sha, a stash ref, a file path, or a repo root — via the shared
   `input/clipboard-copy!` shell helpers. Returns the `{:ok? :msg}` echo
   contract so the action lands in the buffer's footer like every other verb."
  [root {:keys [kind sha ref path]}]
  (let
    [payload (case kind
               :commit
               sha

               :stash
               ref

               :file
               path

               :repo
               (str root)

               nil)]
    (if (seq (str payload))
      (do (input/clipboard-copy! (str payload)) {:ok? true :msg (str "Copied " payload)})
      {:ok? false :msg "Nothing to copy here"})))

(defn- magit-path-grammar
  "Tree-sitter grammar name for a file `path`'s extension (for the RET-visit
   viewer's syntax coloring), or nil when we don't colorize that language."
  [path]
  (let
    [s
     (str path)

     dot
     (str/last-index-of s ".")]

    (when (and dot (< (inc (long dot)) (count s)))
      (highlight/grammar-for (subs s (inc (long dot)))))))

(defn- magit-char-action!
  "Run the magit verb for character `c` against the row under the cursor.
   Returns an action result `{:ok? :msg}`, or nil when the key did nothing.
   Case-SENSITIVE — `s`≠`S`, `u`≠`U`, `f`≠`F`, exactly like magit. All prompts
   render inline in the buffer's bottom row via `mini` (never a modal box)."
  [^TerminalScreen screen busy! mini root model rows idx row c]
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
    (magit-discard-flow! mini root row)

    \c
    (magit-commit-flow! busy! mini root model)

    \l
    (magit-log-flow! screen mini root)

    \P
    (magit-push-flow! busy! mini root)

    \F
    (run-network! busy! "Pulling" #(magit/pull! root))

    \f
    (run-network! busy! "Fetching" #(magit/fetch! root))

    \b
    (magit-branch-flow! mini root)

    \z
    (magit-stash-flow! mini root (when (= :stash (:kind row)) (:ref row)) model)

    \g
    {:ok? true :msg "Refreshed"}

    nil))

(defn magit-dialog!
  "Magit-style status buffer over the git CLI — the C-x g / footer-git modal.

   `root-or-repos` is either ONE root (string/File — the classic single-repo
   buffer) or a vector of repo entries `{:root :label :draft?}` from
   `magit/session-roots` — the session's primary workspace root, every Git
   repository nested below it (a mega-repo's `repositories/` clones) and every
   repository declared in `vis.yml`'s `workspace.filesystem` catalog. Each
   entry gets a header row carrying its branch and dirty counts, and its own
   full section stack; a clean repo opens FOLDED to that one line and TAB
   unfolds it. For a DRAFT session the entries already point at the CLONES the
   session edits, so the buffer shows the draft's git state, not the trunk's.

   Sections: head/upstream facts, untracked, unmerged, unstaged, staged,
   stashes and recent commits. TAB folds a file's diff peek open under
   its row; RET visits it FULLSCREEN (a file's syntax-highlighted body, a
   commit's or stash's full patch). Verbs mirror magit and route to the repo
   UNDER THE CURSOR:
   `s`/`u` stage/unstage the file or the whole section, `S`/`U` all, `k`
   discard with a confirm, `c` commit/amend (message prompt), `P` push
   (plain / -u / --force-with-lease), `F` pull, `f` fetch, `b` branch
   (checkout / create / delete), `z` stash (push / pop / apply / drop), `g`
   refresh, `C-w` copy the sha/path/ref under point, `q`/Esc close. Every verb
   shells to the real `git` binary via
   `internal.git`, and the buffer re-reads every repo after each action, so
   what you see is always `git status` truth. Returns nil."
  [^TerminalScreen screen root-or-repos]
  (let
    [repo-entries
     (if (sequential? root-or-repos) (vec root-or-repos) (magit/workspace-roots nil root-or-repos))

     repos*
     (atom (magit/load-repos repo-entries))

     ;; primary-root / multi? read the LOADED repos so non-git roots that
     ;; load-repos dropped never skew the title or the fallback root.
     primary-root
     (:root (first @repos*))

     multi?
     (> (count @repos*) 1)

     ;; Discovery bound its own walk? Then a repository exists that this buffer
     ;; is NOT showing, and the title says so — a short list must never be
     ;; mistaken for the whole fleet.
     scan-truncated?
     (and multi? (magit/nested-scan-truncated? primary-root))

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
       (let
         [root
          (or (:root row) primary-root)

          kind
          (:kind row)

          id
          (case kind
            :commit
            (:sha row)

            :stash
            (:ref row)

            (:path row))

          k
          [root (:area row) id]]

         (or (get @diff-cache k)
             (let
               [lines (or (not-empty (case kind
                                       :commit
                                       (magit/commit-diff-lines root row)

                                       :stash
                                       (magit/stash-diff-lines root row)

                                       (magit/file-diff-lines root row)))
                          ["(no diff)"])]
               (swap! diff-cache assoc k lines)
               lines))))]

    (loop []

      (let
        [repos
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
           (str "Git — " (count repos) " roots" (when scan-truncated? " · scan truncated"))
           (let [model (:model (first repos))]
             (str "Git — "
                  (cond (nil? model) "?"
                        (:detached? model) "detached HEAD"
                        :else (:branch model)))))

         ;; Full-screen overlay that still spares the app's two-row footer.
         ;; Center within `term-rows - 1` so the frame stays above it.
         content-w
         (max (footer-content-width cols magit-hints) (- cols 4))

         bounds
         (draw-dialog-chrome! g cols (dec term-rows) title content-w (max 1 (- term-rows 5)))

         {:keys [left inner-w]}
         bounds

         left
         (long left)

         inner-w
         (long inner-w)

         {:keys [content-top content-h hint-row]}
         (dialog-layout bounds)

         content-top
         (long content-top)

         content-h
         (long content-h)

         echo-h
         (if @echo 1 0)

         list-h
         (long (max 1 (- content-h echo-h)))

         _
         (reset! sel (or (magit/first-selectable buf-rows (min (long @sel) (max 0 (dec total)))) 0))

         visible
         (long (min total list-h))

         max-start
         (long (max 0 (- total visible)))

         start
         (p/clamp @scroll 0 max-start)

         _
         (reset! scroll start)

         text-w
         (long (max 1 (- inner-w 3)))

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
                (p/put-str! g
                            (+ left 2)
                            hint-row
                            (ellipsize
                              (str (render/spinner-frame (System/currentTimeMillis)) "  " label "…")
                              text-w))
                (.setCursorPosition screen nil)
                (.refresh screen Screen$RefreshType/DELTA)
                (catch Throwable _ nil)))

         mini
         (band-questions
           screen
           g
           ;; The band opens INSIDE the status buffer's own frame and
           ;; may never climb over the box's top border or the rows it
           ;; keeps visible.
           {:left left :inner-w inner-w :hint-row hint-row :text-w text-w :min-row content-top})]

        (dotimes [i visible]
          (let
            [idx (+ start i)
             row-y (+ content-top i)]

            (when (< idx total)
              (let
                [row (nth buf-rows idx)
                 [fg bold? row-bg] (magit-row-style row)
                 bg (or row-bg t/dialog-bg)
                 selected? (and (= idx @sel) (magit/selectable? row))]

                (p/set-colors! g t/dialog-fg bg)
                (p/fill-rect! g (inc left) row-y inner-w 1)
                (p/set-colors! g t/dialog-hint-key bg)
                (p/draw-selection-marker! g (inc left) row-y selected?)
                (p/set-colors! g fg bg)
                (let
                  [txt (ellipsize (:text row) text-w)
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
        (let
          [key
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
             (swap! scroll #(p/clamp (+ (long %) (long delta)) 0 max-start)))

           reconcile-sel!
           (fn []
             ;; After a pure viewport scroll, pull the point onto the nearest
             ;; selectable row still on screen so verbs act on something visible.
             (let
               [s
                (long @scroll)

                hi
                (long (min total (+ s visible)))]

               (when (or (< (long @sel) s) (>= (long @sel) hi))
                 (when-let [n (some #(when (magit/selectable? (nth buf-rows %)) %) (range s hi))]
                   (reset! sel n)))))

           toggle-diff!
           (fn []
             (let
               [kind
                (:kind row)

                root
                (or (:root row) primary-root)

                k
                (case kind
                  ;; TAB on a section header folds/unfolds the whole
                  ;; section (magit's section visibility toggle).
                  :section
                  (when (:collapsible? row) [root :section (:area row)])

                  ;; TAB on a repo header folds/unfolds that repository's whole
                  ;; section stack in a multi-root buffer.
                  :repo
                  [root :repo nil]

                  :commit
                  [root (:area row) (:sha row)]

                  :stash
                  [root :stashes (:ref row)]

                  :file
                  [root (:area row) (:path row)]

                  nil)]

               (when k (swap! expanded #(if (contains? % k) (disj % k) (conj % k))))))

           visit!
           (fn [target]
             ;; magit's RET: VISIT the thing under point in the FULLSCREEN
             ;; viewer (TAB still folds the inline diff peek). A file opens
             ;; its working-tree body, syntax-highlighted by extension and
             ;; falling back to its diff for a deleted/renamed entry; a
             ;; commit or stash opens its full patch.
             (when target
               (let [vroot (or (:root target) primary-root)]
                 (case (:kind target)
                   :file
                   (let
                     [{:keys [path]} target
                      body (magit/visit-file-lines vroot path)]

                     (if (seq body)
                       (do (log-view-dialog! screen
                                             (str path)
                                             body
                                             :grammar
                                             (magit-path-grammar path))
                           (clear-screen! screen))
                       (when-let [d (not-empty (magit/file-diff-lines vroot target))]
                         (log-view-dialog! screen (str path "  (diff)") d :grammar nil)
                         (clear-screen! screen))))

                   :commit
                   (when-let [d (not-empty (magit/commit-diff-lines vroot target))]
                     (log-view-dialog! screen (str "commit " (:sha target)) d :grammar nil)
                     (clear-screen! screen))

                   :stash
                   (when-let [d (not-empty (magit/stash-diff-lines vroot target))]
                     (log-view-dialog! screen (str (:ref target)) d :grammar nil)
                     (clear-screen! screen))

                   nil))))]

          (cond (nil? key) (recur)
                wheel (do (scroll-view! wheel) (reconcile-sel!) (recur))
                :else (condp = (key-type key)
                        KeyType/Escape nil
                        KeyType/ArrowUp (do (move! -1) (recur))
                        KeyType/ArrowDown (do (move! 1) (recur))
                        KeyType/PageUp
                        (do (scroll-view! (- (long (max 1 (dec list-h))))) (reconcile-sel!) (recur))
                        KeyType/PageDown
                        (do (scroll-view! (long (max 1 (dec list-h)))) (reconcile-sel!) (recur))
                        KeyType/Home (do (reset! sel (or (magit/first-selectable buf-rows 0) 0))
                                         (reset! scroll 0)
                                         (recur))
                        KeyType/End (do (reset! scroll max-start) (reconcile-sel!) (recur))
                        KeyType/Tab (do (toggle-diff!) (recur))
                        KeyType/Enter (do (visit! row) (recur))
                        KeyType/Character
                        (let [c (key-character key)]
                          (cond (= c \q) nil
                                ;; n/p jump section-to-section (magit's section motion)
                                (contains? #{\n \p} c)
                                (let [i (magit/next-section buf-rows @sel (if (= c \n) 1 -1))]
                                  (reset! sel i)
                                  (reset! scroll (visible-window-start i @scroll visible total))
                                  (recur))
                                ;; C-w copies the sha/path/ref at point (magit-copy-section-value)
                                (and (.isCtrlDown ^KeyStroke key) (= c \w))
                                (do (reset! echo nil)
                                    (run-action! (magit-copy-action! row-root row))
                                    (recur))
                                :else (do (reset! echo nil)
                                          (run-action! (magit-char-action! screen
                                                                           busy!
                                                                           mini
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
  (when-let
    [router
     (try (vis/get-router)
          (catch Throwable t (tel/log! :warn ["dialogs: get-router failed" (ex-message t)]) nil))]
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
   (let
     [specs (->> (vis/toggles-for-channel :tui)
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
  (let
    [contributions (->> (vis/channel-contributions-for :tui)
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
  (let
    [raw
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
  (let
    [provider-label
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
  "Old extension setting rows now owned by registry toggles. Drop them rather
   than aliasing or rendering duplicates. These are extension-setting keys,
   not toggle ids."
  #{:reasoning-level :verbosity})

(defn- extension-setting-declarations
  []
  (->> (vis/registered-extensions)
       (mapcat
         (fn [ext]
           (let
             [ext-id
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
  (mapv (fn
          [{:keys [key type choices label description extension-id extension-kind extension-label
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
           (let
             [ext-id
              (:ext/name ext)

              ext-kind
              (extension-kind ext)

              ext-label
              (extension-display-label ext)]

             (for
               [decl
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
  (mapv (fn
          [{:keys [name label description extension-id extension-kind extension-label secret?
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
  (let
    [base
     (vec (or (:providers config) []))

     configured-ids
     (into #{} (map :id) base)]

    (into base
          (remove #(contains? configured-ids (:id %)))
          (try (vis/authenticated-preset-providers) (catch Throwable _ nil)))))

(defn- gateway-auth-index
  "ONE gateway round trip for the WHOLE fleet. `GET /v1/router` already carries
   every provider's `status`, so Settings asks once instead of once per provider
   — a fleet of eight used to cost eight requests to the daemon, in parallel but
   each with its own connection and its own tail latency.

   Returns `{provider-id-string is-authenticated?}`, or nil when the gateway
   refused: a read that failed is an `:off` row, never a throw into the loop."
  []
  (try (into {}
             (keep (fn [entry]
                     (when-let [id (get entry "id")]
                       [id (boolean (get-in entry ["status" "is_authenticated"]))])))
             (vis/gateway-router-fleet))
       (catch Throwable _ nil)))

(defn- provider-auth-state
  "`:local` for a provider that needs no credentials at all, otherwise the
   GATEWAY's verdict, read out of the one fleet-wide `auth-index`: `:on`
   authenticated, `:off` not. The TUI must not read the auth file itself — the
   gateway may be on another machine and it is the one process that owns
   credential resolution."
  [provider auth-index]
  (cond (contains? vis/provider-local-no-auth-ids (:id provider)) :local
        (get auth-index
             (some-> (:id provider)
                     name))
        :on
        :else :off))

(defn load-provider-inventory!
  "Refresh the cached provider fleet from config + gateway. Never throws: a
   gateway that is down becomes an inline row instead of yet another modal. The
   auth verdicts come from a SINGLE `/v1/router` read covering every provider,
   so opening Settings costs one round trip whatever the fleet size.

   Router selection is part of the fleet, not a separate lookup: each entry
   carries whether it is the default or the fallback AND the model that choice
   picked, so Settings can SHOW what `d`/`f` just did."
  []
  (reset! provider-inventory (try
                               (let
                                 [config
                                  (vis/load-config)

                                  fleet
                                  (provider-fleet config)

                                  default-id
                                  (some-> (:default-provider config)
                                          name)

                                  fallback-id
                                  (some-> (:fallback-provider config)
                                          name)

                                  auth-index
                                  (gateway-auth-index)]

                                 {:status :ok
                                  :providers
                                  (mapv (fn [provider]
                                          (let
                                            [pid (some-> (:id provider)
                                                         name)]
                                            {:provider provider
                                             :auth (provider-auth-state provider auth-index)
                                             :default? (= default-id pid)
                                             :default-model (when (= default-id pid)
                                                              (some-> (:default-model config)
                                                                      name))
                                             :fallback? (= fallback-id pid)
                                             :fallback-model (when (= fallback-id pid)
                                                               (some-> (:fallback-model config)
                                                                       name))}))
                                        fleet)
                                  :error nil})
                               (catch Exception e
                                 {:status :error :providers [] :error (ex-message e)}))))

(defn- provider-settings-status
  "One provider row's description: the ROUTER TAGS first, then the auth verdict,
   then the provider's own model.

   A router tag names the model it selected (`default → sonnet`), because
   \"default\" alone never told you which model `d` had just bound — and it leads
   the line because a narrow pane truncates the TAIL, while auth already has a
   glyph of its own on the row."
  [{:keys [provider auth default? default-model fallback? fallback-model]}]
  (let
    [tag (fn [label model]
           (if (str/blank? (str model))
             label
             (str label " " (char 0x2192) " " (vis/model-name model))))]
    (str/join " · "
              (remove str/blank?
                [(when default? (tag "default" default-model))
                 (when fallback? (tag "fallback" fallback-model))
                 (case auth
                   :on
                   "signed in"

                   :local
                   "local, no sign-in"

                   "not signed in")
                 (str (some-> provider
                              :models
                              first
                              vis/model-name))]))))

(defn- provider-settings-rows
  "The `Providers` settings section: one row per provider — auth state, model,
   default tag on the same line — opening that provider's own magit transient
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

(defn load-mcp-inventory!
  "Refresh the cached MCP inventory from the gateway. Never throws: a gateway
   that is down, or a rejected verb, becomes an inline row instead of yet
   another modal."
  []
  (reset! mcp-inventory (try {:status :ok :servers (vec (vis/gateway-mcp-servers)) :error nil}
                             (catch Exception e
                               {:status :error :servers [] :error (ex-message e)}))))

(defn- mcp-settings-rows
  "The `MCP Servers` settings section: one row per server — its live status
   riding the same line — opening that server's own magit transient INSIDE this
   frame, plus one row that adds a new server. Empty until `load-mcp-inventory!`
   has run."
  []
  (let [{:keys [status servers error]} @mcp-inventory]
    (when-not (= :unloaded status)
      (vec
        (concat [{:type :section :label "MCP Servers"}]
                (mapv (fn [row]
                        {:type :mcp
                         :label (str (get row "name"))
                         :description (mcp-model/server-status row)
                         :inline-description true
                         :server row})
                      servers)
                (when (seq (str error))
                  [{:type :info :tone :bad :label "MCP unavailable" :description (str error)}])
                (when (and (empty? servers) (empty? (str error)))
                  (if (= :loading status)
                    [{:type :info
                      :label "Loading MCP servers…"
                      :description "Reading them from the gateway"}]
                    [{:type :info
                      :label "No MCP servers yet"
                      :description "Add one below, or declare them under mcp: in vis.yml."}]))
                [{:type :action
                  :id :mcp-add
                  :label "Add MCP server…"
                  :description "Register a new one with the gateway"}])))))

(defn- mark-inventories-loading!
  "Arm both gateway-backed inventories for a refresh WITHOUT clearing what they
   already hold: a re-opened Settings shows the fleet it last read and refreshes
   it in place, and a first open shows a `Loading…` row — never a blank pane and
   never a wait before the frame."
  []
  (swap! provider-inventory assoc :status :loading)
  (swap! mcp-inventory assoc :status :loading))

(defn- load-inventories!
  "Read both gateway inventories, in PARALLEL: the MCP list and the provider
   fleet are independent round trips, so one open costs the SLOWER of the two
   instead of their sum. Called only AFTER the settings frame is on the
   terminal."
  []
  (let [mcp (vis/worker-future "vis-tui-settings-mcp-inventory" load-mcp-inventory!)]
    (load-provider-inventory!)
    @mcp
    nil))

(defn- settings-rows
  "Every settings row in ONE flat, grouped list — no tabs (mirrors the web
   settings modal): Terminal-UI chrome, then all feature toggles grouped by
   `:group`, then providers, then MCP servers, then channel / provider /
   extension knobs.
   Sections with nothing to show drop out."
  ([] (settings-rows (extension-option-rows)))
  ([extension-rows]
   (let
     [active-provider
      (current-provider-id)

      all-extension-rows
      (filterv #(provider-row-active? active-provider %) extension-rows)

      provider-rows
      (extension-rows-of-kind all-extension-rows :provider)

      channel-rows
      (extension-rows-of-kind all-extension-rows :channel)

      generic-rows
      (extension-rows-of-kind all-extension-rows :extension)]

     (vec
       (concat [{:type :section :label "Terminal UI"}]
               (settings-ui-options)
               ;; ALL feature toggles, grouped by :group like the web.
               (or (registry-toggle-rows) [])
               (or (contributor-rows) [])
               ;; Providers live here too — one row per provider, straight into
               ;; its own menu, mirroring the web Settings providers panel.
               (or (provider-settings-rows) [])
               ;; MCP servers live here too — one toggle per server instead
               ;; of a stack of separate dialogs.
               (or (mcp-settings-rows) [])
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
    :env
    "set in environment"

    :keychain
    "read from keychain"

    :command
    "read from command"

    :dotenv
    "set in .env"

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
    (let
      [spec
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
  [{:keys [key type set-key item-id toggle-id server auth]} values]
  (let
    [on
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
      [" " t/dialog-fg]

      :choice
      val

      :set-toggle
      (if (some-> (get values set-key)
                  (contains? item-id))
        off
        on)

      ;; in disabled-set → off
      :registry-toggle
      (let
        [spec
         (vis/toggle-spec toggle-id)

         tv
         (vis/toggle-value toggle-id)]

        (cond (= :enum (:type spec)) val
              (boolean tv) on
              :else off))

      ;; an MCP server reads its on/off off the live gateway row
      :mcp
      (if (mcp-model/server-on? server) on off)

      ;; a provider's dot is the GATEWAY's auth verdict, never a local guess
      :provider
      (case auth
        :on
        on

        :local
        val

        off)

      :toggle
      (if (get values key false) on off)

      [" " t/dialog-fg])))

(defn- cycle-choice
  [choices current]
  (let
    [choices
     (vec choices)

     idx
     (.indexOf ^java.util.List choices current)]

    (nth choices (mod (inc (long (if (neg? idx) 0 idx))) (count choices)))))

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
  (contains? #{:toggle :choice :action :set-toggle :registry-toggle :mcp :provider} type))

(defn- first-selectable-index
  [rows]
  (or (first (keep-indexed (fn [i row]
                             (when (settings-selectable? row) i))
                           rows))
      0))

(defn- settings-initial-index
  "Where the cursor starts. `section` (a section label such as `MCP Servers`)
   opens Settings already parked on that section, so a palette entry can point
   straight at its rows instead of opening a dialog of its own."
  [rows section]
  (let
    [head (when (seq (str section))
            (first (keep-indexed (fn [i {:keys [type label]}]
                                   (when (and (= :section type) (= (str label) (str section))) i))
                                 rows)))]
    (or (when head
          (first (keep-indexed (fn [i row]
                                 (when (and (> (long i) (long head)) (settings-selectable? row)) i))
                               rows)))
        (first-selectable-index rows))))

(defn- move-settings-selection
  [rows ^long selected ^long delta]
  (let [n (count rows)]
    (loop [idx (p/clamp (+ selected delta) 0 (max 0 (dec n)))]
      (cond (= idx selected) idx
            (settings-selectable? (nth rows idx)) idx
            (and (neg? delta) (zero? idx)) selected
            (and (pos? delta) (= idx (dec n))) selected
            :else (recur (p/clamp (+ idx delta) 0 (max 0 (dec n))))))))


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

(defn- theme-picker-dialog!
  "Small theme chooser. Moving selection previews the theme immediately;
   Enter commits the preview, Esc restores the original theme."
  [^TerminalScreen screen choices current preview!]
  (let
    [items
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
         (when-not (= theme-id @last-preview) (reset! last-preview theme-id) (preview! theme-id))))]

    (when (pos? total)
      (loop []

        (preview-selected!)
        (let
          [size
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
           ;; Size the box to the ACTUAL theme count (floored, terminal-clamped)
           ;; so a short list gets a compact chooser instead of a full-height
           ;; frame with the rows marooned in the vertical center.
           (adaptive-content-height rows total)

           bounds
           (draw-dialog-chrome! g cols rows "Theme" content-w content-h)

           {:keys [left inner-w]}
           bounds

           {:keys [content-top content-h hint-row]}
           (dialog-layout bounds total)

           visible
           (min (long total) (long content-h))

           _
           (swap! selected #(p/clamp % 0 (max 0 (dec total))))

           _
           (swap! scroll #(visible-window-start @selected % content-h total))]

          (dotimes [i visible]
            (let
              [idx (+ (long @scroll) (long i))
               row-y (+ (long content-top) (long i))]

              (when (< (long idx) (long total))
                (draw-list-item! g
                                 left
                                 row-y
                                 (if (> (long total) (long content-h)) (dec (long inner-w)) inner-w)
                                 (= idx @selected)
                                 (:label (nth items idx))))))
          (scrollbar/draw! g
                           {:col (+ (long left) (long inner-w))
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
                KeyType/ArrowUp (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total))))
                                    (recur))
                KeyType/ArrowDown
                (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur))
                KeyType/Enter (:theme-id (nth items @selected))
                (recur)))))))))

(defn- activate-theme-row!
  [screen values callbacks {:keys [choices key]}]
  (let
    [original
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
  [^TerminalScreen screen g region values callbacks row]
  (case (:type row)
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
    ;; remove — offered as a magit transient band in THIS frame, each on the key
    ;; the verb itself carries. No manager dialog stacked on top of Settings.
    :mcp
    (do (when-let
          [action
           (:action
             (embed-transient! screen g region (mcp-model/server-transient-spec (:server row))))]
          (when-let [f (:mcp-action callbacks)]
            (f {:server (:server row) :action action}))
          (load-mcp-inventory!))
        @values)

    ;; Same for a provider row: default, fallback, sign in, status, log out are
    ;; a transient here, and picking a model is a second one — never a dialog.
    :provider
    (do (when-let [f (:provider-transient callbacks)]
          (f {:provider-id (:id (:provider row)) :g g :region region})
          (load-provider-inventory!))
        @values)

    (if (= :theme-name (:key row))
      (activate-theme-row! screen values callbacks row)
      (->> (swap! values apply-settings-option row)
           (notify-settings-change! callbacks)))))

(defn- settings-section-text
  [label inner-w]
  (let
    [prefix
     (str "── " label " ")

     available
     (max 0 (- (long inner-w) 2))

     filler
     (apply str (repeat (max 0 (- (long available) (count prefix))) \─))]

    (ellipsize (str prefix filler) available)))

(defn- settings-option-indent [] t/settings-option-indent)

(defn- settings-subsection-text
  [label inner-w]
  (ellipsize (str "◆ " label) (max 0 (- (long inner-w) 2))))

(defn- settings-wrap-lines
  [s w]
  (let
    [w
     (max 1 (long w))

     s
     (str/trim (str (or s "")))]

    (if (str/blank? s) [] (vec (remove str/blank? (render/wrap-text s w))))))

(defn- settings-render-entries
  "Flatten logical settings rows into paint rows. Descriptions wrap under
   their owning option instead of stealing a fixed inline column and
   collapsing to `...` on narrow dialogs / long extension labels — except
   rows that ask for `:inline-description` (a short STATE, not prose), which
   keep it on the option line and emit no wrap rows at all.

   An `:info` row is prose ABOUT its section (empty state, gateway error), so
   its label and description are separate wrapped blocks — a bold head line
   plus a dim body — never one run-on sentence."
  [rows desc-w]
  (let [desc-w (max 1 (long desc-w))]
    (vec
      (mapcat (fn [idx {:keys [type label description inline-description]}]
                (case type
                  :section
                  [{:row-idx idx :part :section}]

                  :subsection
                  [{:row-idx idx :part :subsection}]

                  :info
                  (into (mapv (fn [line]
                                {:row-idx idx :part :info-line :text line :head? true})
                              (or (seq (settings-wrap-lines label desc-w)) [""]))
                        (mapv (fn [line]
                                {:row-idx idx :part :info-line :text line})
                              (settings-wrap-lines description desc-w)))

                  (if inline-description
                    [{:row-idx idx :part :option}]
                    (let [desc-lines (settings-wrap-lines description desc-w)]
                      (into [{:row-idx idx :part :option}]
                            (mapv (fn [line]
                                    {:row-idx idx :part :option-desc :text line})
                                  desc-lines))))))
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
  (let
    [rows
     (vec rows)

     q
     (str/lower-case (str/trim (str query)))]

    (if (str/blank? q)
      rows
      (let
        [n
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
         (for
           [i
            (range n)

            :let [row
                  (nth rows i)]
            :when (case (:type row)
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
  "Table-of-contents entries for the VS Code-style left sidebar: one per
   top-level `:section`, each with the count of selectable rows beneath it
   and whether it owns the currently-selected row. `rows` is the (already
   filtered) flat settings list; `selected` is the selected row index."
  [rows selected]
  (let
    [rows
     (vec rows)

     n
     (count rows)

     sec-idxs
     (filterv #(= :section (:type (nth rows %))) (range n))]

    (vec (map-indexed (fn [k start]
                        (let
                          [end
                           (long (or (get sec-idxs (inc (long k))) n))

                           cnt
                           (count (filter settings-selectable? (subvec rows start end)))]

                          {:label (:label (nth rows start))
                           :count cnt
                           :start start
                           :active? (and (>= (long selected) (long start))
                                         (< (long selected) end))}))
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
   `state/default-settings`). `callbacks` also carries `:focus-section` (a
   section label to park the cursor on, e.g. `MCP Servers` or `Providers`),
   `:mcp-add` / `:provider-add` (the add row of each section), `:mcp-action`
   (the verb a server's transient fired) and `:provider-transient` (one
   provider's transient, handed the graphics and the region it paints into).
   Esc clears an active search first, then closes and returns the
   current settings map."
  ([^TerminalScreen screen settings] (settings-dialog! screen settings nil))
  ([^TerminalScreen screen settings callbacks]
   (let
     [extension-rows
      (extension-option-rows)

      ;; MCP servers and providers are settings sections now, so both
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
      (atom (settings-initial-index (settings-rows extension-rows) (:focus-section callbacks)))

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

       (let
         [all-rows
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

          left
          (long left)

          inner-w
          (long inner-w)

          ;; VS Code split: a left sidebar rail (the section Table of
          ;; Contents) + a vertical divider + the right settings pane.
          ;; `lleft`/`linner` are the right pane's own left/inner-w, so the
          ;; whole list-painting block below reuses the single-pane math
          ;; unchanged — only the search bar and hint bar stay full width.
          rail-w
          (p/clamp (quot inner-w 4) 14 22)

          lleft
          (+ left rail-w 1)

          linner
          (max 1 (- inner-w rail-w 1))

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
          (swap! selected #(p/clamp % 0 (max 0 (dec n))))

          option-indent
          (long (settings-option-indent))

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
          (settings-render-entries rows base-desc-w)

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

          ;; Rows carrying an inline description (MCP / provider status) share
          ;; ONE column, so those states line up as a table instead of ragging
          ;; after names of different length.
          inline-desc-x
          (+ option-x
             p/STATUS_WIDTH
             2
             (long (reduce max
                           0
                           (keep (fn [[row lbl]]
                                   (when (:inline-description row) (count lbl)))
                                 (map vector rows labels)))))

          entries
          (settings-render-entries rows desc-w)

          visual-n
          (count entries)

          sel-entry-idxs
          (keep-indexed (fn [entry-idx {:keys [row-idx]}]
                          (when (= row-idx @selected) entry-idx))
                        entries)

          ;; Option line of the selected row (first non-description entry).
          selected-visual
          (long (or (first (keep-indexed (fn [entry-idx {:keys [row-idx part]}]
                                           (when (and (= row-idx @selected)
                                                      (not= part :option-desc))
                                             entry-idx))
                                         entries))
                    0))

          ;; Last paint row owned by the selected option, INCLUDING its
          ;; wrapped description rows. The scroll window must be able to
          ;; reach this so the trailing desc lines (and, for the bottom-most
          ;; option, the true content end) come into view — otherwise scroll
          ;; caps short of `visual-n - visible-h` and the scrollbar thumb
          ;; never reaches the bottom (selectable rows < paint rows).
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
                           (contains? #{:section :subsection :info-line} (:part (nth entries i))))
                    (recur (dec i))
                    (inc i))))

          _
          (let
            [start0
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
           (let
             [entry-idx (+ (long @scroll) i)
              row-y (+ list-top i)]

             (if (< entry-idx visual-n)
               (let
                 [{:keys [row-idx part text head?]} (nth entries entry-idx)
                  {:keys [label tone description inline-description]} (nth rows row-idx)
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
                         (p/styled g [p/BOLD] (p/put-str! g desc-x row-y (ellipsize text desc-w)))
                         (p/put-str! g desc-x row-y (ellipsize text desc-w))))

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
                       ;; Cursor glyph sits immediately LEFT of the row body, so
                       ;; a selected row reads as one unit instead of an orphan
                       ;; bullet parked against the pane divider.
                       (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                       (p/draw-selection-marker! g (- option-x p/SELECTION_WIDTH) row-y selected?)
                       ;; Leading status glyph (●/○/◆/▸) via the shared component,
                       ;; which returns the col to start the label at.
                       (let
                         [label-x (p/status-mark! g option-x row-y mark mark-color t/dialog-bg)
                          lbl (ellipsize option-label (max 1 (- option-w p/STATUS_WIDTH)))]

                         (p/set-colors! g t/dialog-fg t/dialog-bg)
                         (if selected?
                           (p/styled g [p/BOLD] (p/put-str! g label-x row-y lbl))
                           (p/put-str! g label-x row-y lbl))
                         ;; A short STATE (an MCP server / provider status) rides
                         ;; the option line in one shared column instead of
                         ;; costing a whole wrapped row per entry.
                         (when (and inline-description (seq (str description)))
                           (let
                             [dx (max (+ (long label-x) (long (count lbl)) 2) (long inline-desc-x))
                              avail (- (+ lleft paint-w) dx)]

                             (when (pos? avail)
                               (p/set-colors! g t/dialog-hint t/dialog-bg)
                               (p/put-str! g dx row-y (ellipsize (str description) avail)))))))))
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
             (let
               [{lbl :label cnt :count active? :active?} (nth toc i)
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
         (if @inventories-pending
           ;; The frame is ON the terminal now — only then pay for the gateway,
           ;; and repaint into the dialog the user is already looking at.
           ;; `focus-section` is re-parked because the rows the read added sit
           ;; under its own section header.
           (do (vreset! inventories-pending false)
               (load-inventories!)
               (reset! selected (settings-initial-index (settings-rows extension-rows)
                                                        (:focus-section callbacks)))
               (recur))
           (let
             [key
              (read-modal-key! screen)

              selected-row
              (when (pos? n) (nth rows (p/clamp @selected 0 (dec n))))]

             (when key
               (cond
                 (instance? MouseAction key)
                 (let
                   [^MouseAction ma
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
                     (or (= action MouseActionType/SCROLL_UP)
                         (= action MouseActionType/SCROLL_DOWN))
                     (let
                       [step (or (modal-wheel-step key)
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
                          (scrollbar/on-track? mx
                                               my
                                               {:col bar-col :top list-top :track-h visible-h}))
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
                 :else (condp = (key-type key)
                         ;; Esc clears an active search first, then closes on the next press.
                         KeyType/Escape (if (str/blank? @query)
                                          @values
                                          (do (reset! query "")
                                              (reset! selected (first-selectable-index all-rows))
                                              (reset! scroll 0)
                                              (recur)))
                         KeyType/ArrowUp (do (swap! selected #(move-settings-selection rows % -1))
                                             (recur))
                         KeyType/ArrowDown (do (swap! selected #(move-settings-selection rows % 1))
                                               (recur))
                         ;; Backspace edits the live search query.
                         KeyType/Backspace (do (when (seq @query)
                                                 (swap! query #(subs % 0 (dec (count %))))
                                                 (reset! selected (first-selectable-index
                                                                    (filter-settings-rows all-rows
                                                                                          @query)))
                                                 (reset! scroll 0))
                                               (recur))
                         ;; Any printable character types into the search query (VS Code feel);
                         ;; Enter is the only key that toggles/activates the selected row.
                         KeyType/Character
                         (let [c (key-character key)]
                           (if (and c (>= (int c) 32))
                             (do (swap! query str c)
                                 (reset! selected (first-selectable-index
                                                    (filter-settings-rows all-rows @query)))
                                 (reset! scroll 0)
                                 (recur))
                             (recur)))
                         KeyType/Enter (do (when selected-row
                                             (activate-settings-row!
                                               screen
                                               g
                                               {:left left
                                                :inner-w inner-w
                                                :hint-row hint-row
                                                :text-w (max 1 (- (long inner-w) 2))
                                                :min-row list-top
                                                ;; One snapshot per
                                                ;; activation: a
                                                ;; shorter band gives
                                                ;; the rows a taller
                                                ;; one covered back to
                                                ;; the list itself.
                                                :restore! (frame-restorer screen)}
                                               values
                                               callbacks
                                               selected-row))
                                           (recur))
                         (recur)))))))))))

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
  (let
    [title
     (get session "title")

     base-title
     (if (untitled-session-title? title) untitled-session-title (str title))

     fork-count
     (long (or (get session "fork_count") 0))]

    (cond-> base-title
      (pos? fork-count)
      (str " [forks:" fork-count "]"))))

(def ^:private navigator-opening-max 140)

(defn- navigator-opening
  "First SENTENCE of what a session opened with (`first_request` from the
   gateway summary), collapsed to one line and bounded. Titles are generated
   and often vague; this is the actual ask, painted dim after the title."
  [text]
  (some-> text
          str
          (str/replace #"\s+" " ")
          str/trim
          not-empty
          (as-> s (let [s (or (not-empty (str (re-find #"^.*?[.!?\u2026](?=\s|$)" s))) s)]
                    (if (> (count s) (long navigator-opening-max))
                      (str (subs s 0 (long navigator-opening-max)) "\u2026")
                      s)))))

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
  (let
    [n
     (count session-table-headers)

     overhead
     (inc (* 3 n))

     available
     (max n (- table-w overhead))]

    (if (>= available 70)
      (let
        [active-w
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
      (let
        [active-w
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
  (let
    [id
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
  ;; Session picker is a TABLE — cells must NOT shift between selected
  ;; and unselected states, so the dot marker is painted by caller (see
  ;; row loop in `session-picker-dialog!`) at `(inc left)`, inner edge
  ;; of dialog frame. Body label sits two cols further in (gutter for
  ;; marker + margin) and uses normal palette, BOLD on selected so row
  ;; text echoes marker cue.
  (p/set-colors! g t/dialog-fg t/dialog-bg)
  (p/fill-rect! g (inc (long left)) row inner-w 1)
  (let
    [body-x
     (+ (long left) 1 p/SELECTION_WIDTH)

     body-w
     (max 0 (- (long inner-w) 1 p/SELECTION_WIDTH))]

    (if selected?
      (p/styled g [p/BOLD] (p/put-str! g body-x row (ellipsize label body-w)))
      (p/put-str! g body-x row (ellipsize label body-w)))))

(defn session-picker-dialog!
  "Show recent TUI sessions in a fixed-size table. Returns
   `{:action :new}`, `{:action :fork}`, `{:action :switch :id <session-id>}`,
   or nil on Esc."
  [^TerminalScreen screen sessions active-id]
  (let
    [selected
     (atom 0)

     scroll
     (atom 0)]

    (loop []

      (let
        [size
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
         (draw-dialog-chrome! g cols rows "Sessions" (- cols 4) (- rows 4))

         {:keys [left inner-w]}
         bounds

         ;; Reserve `p/SELECTION_WIDTH` cols at start of inner area
         ;; for dot marker gutter. Table itself is boxed; marker stays
         ;; outside table so columns never shift.
         body-w
         (long (max 1 (- (long inner-w) 4 p/SELECTION_WIDTH)))

         items
         (session-dialog-items sessions active-id body-w)

         total
         (count items)

         {:keys [content-top content-h hint-row]}
         (dialog-layout bounds)

         table-x
         (+ (long left) 1 p/SELECTION_WIDTH)

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
          (let
            [idx (+ (long @scroll) i)
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
                  (p/draw-selection-marker! g (inc (long left)) row (= idx @selected)))
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
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (if-let [wheel-step (modal-wheel-step key)]
              (do (swap! selected #(p/clamp (+ (long %) (long wheel-step)) 0 (max 0 (dec total))))
                  (recur))
              (condp = (key-type key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total))))
                                    (recur))
                KeyType/ArrowDown
                (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur))
                KeyType/Enter (when (pos? total) (select-keys (nth items @selected) [:action :id]))
                KeyType/Character (let
                                    [raw-c (key-character key)
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
(def ^:private navigator-search-delay-ms 180)

(defn- schedule-navigator-search!
  "Debounce transcript lookup off the modal paint/input thread. Replacing a
   query cancels its sleeping predecessor; generation guards discard any stale
   request that was already in flight."
  [task generation result query search-fn]
  (let
    [q
     (str/trim (or query ""))

     token
     (swap! generation inc)]

    (when-let [running @task]
      (future-cancel running))
    (reset! result nil)
    (if (or (empty? q) (nil? search-fn))
      (do (reset! task nil) token)
      (let
        [next-task (future (try (Thread/sleep (long navigator-search-delay-ms))
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

(defn- read-navigator-key!
  "Keep input responsive while a transcript lookup runs. Poll only during that
   short async window so its completed result can repaint without waiting for
   another keystroke; otherwise use Lanterna's blocking modal read."
  [^TerminalScreen screen task result]
  (loop []

    (cond (some? @result) nil
          (nil? @task) (read-modal-key! screen)
          (modal-input-pending? screen) (read-modal-key! screen)
          (future-done? @task) (read-modal-key! screen)
          :else (do (Thread/sleep 12) (recur)))))

(defn- navigator-stamp
  "Compact `MM-dd HH:mm` timestamp (year dropped — these are recent
   sessions), or `-` when absent."
  [v]
  (let [day (format-session-day v)]
    (if (= day "-") "-" (str (subs day 5) " " (format-session-time v)))))

(defn- navigator-session-row
  "Normalize one session for the full-width navigator list. The working directory
   owns the hierarchy; project names stay metadata and never split a directory."
  [active-session-id session]
  (let
    [id
     (get session "id")

     active?
     (= (str id)
        (some-> active-session-id
                str))

     work-dir
     (or (not-empty (:work-dir session)) "No work dir")]

    {:id (str "session:" id)
     :focused? active?
     :title (session-title session)
     :opening (navigator-opening (get session "first_request"))
     :session (short-session-id session)
     :draft (or (not-empty (:draft-label session)) "trunk")
     :group (not-empty (get session "project_name"))
     :position (get session "project_position")
     :dir work-dir
     :work-dir work-dir
     :status (if active? "● focused" (str (long (or (get session "turn_count") 0)) " turns"))
     :created (navigator-stamp (get session "created_at"))
     :modified (navigator-stamp (or (get session "modified_at")
                                    (get session "last_active_at")
                                    (get session "created_at")))
     :target {:action :switch :id id}}))

(defn- group-rows-by-dir
  "Keep each working directory contiguous. The focused directory comes first
   and its focused session leads that group; persisted order remains intact
   for every other row."
  [rows]
  (let
    [order
     (distinct (map :dir rows))

     by-dir
     (group-by :dir rows)]

    (vec (mapcat (fn [dir]
                   (let
                     [group-rows
                      (get by-dir dir)

                      focused
                      (filter :focused? group-rows)

                      others
                      (remove :focused? group-rows)]

                     (concat focused (sort-by #(or (:position %) Long/MAX_VALUE) others))))
                 order))))

(defn- navigator-all-rows
  "Build the project-grouped session list. Empty untitled shells stay hidden by
   default, but the focused session always survives and its project is first."
  [{:keys [sessions active-session-id show-empty-untitled?]}]
  (let
    [focused-id
     (some-> active-session-id
             str)

     focused?
     #(= (str (get % "id")) focused-id)

     kept
     (remove #(and (not show-empty-untitled?) (empty-untitled-session? %) (not (focused? %)))
       sessions)

     focused-first
     (concat (filter focused? kept) (remove focused? kept))]

    (group-rows-by-dir (mapv #(navigator-session-row active-session-id %) focused-first))))

(defn- navigator-row-matches?
  [row query]
  (let [needle (str/lower-case (str/trim (or query "")))]
    (or (empty? needle)
        (some #(str/includes? (str/lower-case (str (get row % ""))) needle)
              [:title :session :draft :dir :work-dir :status]))))

(def ^:private navigator-local-only-rank
  "Band of a row only the LOCAL list could match — its project path, its work dir,
   its status, an unsent draft. The gateway ranks everything it can see, so these
   sit after every ranked row instead of competing on a relevance this side is in
   no position to judge."
  100)

(defn- navigator-search-rank
  "Where a matched row sits under a live query, best first — the GATEWAY decided.

   Session search is ranked ONCE, on the server (`db-search-session-matches`), and
   every surface paints that one order: `:rank` is 0 for a hit in the session's own
   TITLE, 1 for the words the USER typed, 2 for the assistant's ANSWER, 3 for its
   THINKING. The picker adds only the two facts a server cannot know — the row the
   keyboard is already on keeps the top of its project, and a row only local
   metadata matched sits last (`navigator-local-only-rank`). With no query every
   row is level, so the list keeps the order it was built in."
  [row query match]
  (cond (str/blank? (str query)) 0
        (:focused? row) -1
        (some? (:rank match)) (long (:rank match))
        :else navigator-local-only-rank))

(defn- navigator-visible-rows
  "Union instant local metadata matches with the gateway's async transcript
   matches, order them inside each project by the RANK the gateway sent
   (`navigator-search-rank`), then tag the first visible row of each project so
   rendering can emit one group header."
  [rows query transcript-ids]
  (let
    [q
     (str/trim (or query ""))

     matched
     (keep
       (fn [row]
         (let
           [local-hit?
            (navigator-row-matches? row query)

            match
            (get transcript-ids (str (:id (:target row))))

            body-hit?
            (some? match)

            hits
            (when (and body-hit? (map? match) (seq q)) (assoc match :title (:title row)))

            rank
            (navigator-search-rank row query match)]

           (cond (and body-hit? (not local-hit?) (seq q) (not (:focused? row)))
                 (assoc row
                   :transcript-match? true
                   :transcript-match hits
                   :search-rank rank
                   :status (case (:kind match)
                             :request
                             "in request"

                             :reply
                             "in reply"

                             :thinking
                             "in thinking"

                             "in chat"))
                 (or local-hit? body-hit?) (cond-> (assoc row :search-rank rank)
                                             hits
                                             (assoc :transcript-match hits))
                 :else nil)))
       rows)]

    (vec (mapcat (fn [group]
                   (let
                     [group
                      (vec (sort-by :search-rank (vec group)))

                      n
                      (count group)]

                     (map-indexed (fn [idx row]
                                    (assoc row
                                      :group-start? (zero? (long idx))
                                      :group-count n))
                                  group)))
                 (partition-by :dir matched)))))

(defn- navigator-highlight-segments
  "Split `s` into `[text bold?]` segments, bolding case-insensitive occurrences
   of `needle` so the matched search term stands out in a plain snippet line."
  [s needle]
  (let
    [s
     (str (or s ""))

     needle
     (str/trim (str (or needle "")))]

    (if (str/blank? needle)
      [[s false]]
      (let
        [ls
         (str/lower-case s)

         ln
         (str/lower-case needle)

         n
         (count needle)]

        (loop
          [from
           0

           acc
           []]

          (let [i (str/index-of ls ln from)]
            (if (nil? i)
              (conj acc [(subs s from) false])
              (recur (+ (long i) n)
                     (cond-> acc
                       (> (long i) (long from))
                       (conj [(subs s from i) false])

                       :always
                       (conj [(subs s i (+ (long i) n)) true]))))))))))

(def ^:private navigator-inline-hits
  "Snippet lines painted INLINE under one matching session row. The companion
   app renders every hit under every match, so the TUI does the same; the
   server already caps its payload at six hits per session."
  6)

(defn- navigator-preview-entries
  "Transcript-style preview rows for a selected body match: ONE row per MATCH
   HIT, newest first — `You` for a hit in the user's own request, `Vis` for one
   in the LLM reply. The server sends several hits per session, so a session
   that matched twenty times no longer shows a single arbitrary line.

   Falls back to the legacy single request/reply snippet pair when the caller
   supplied no `:hits`."
  [match]
  (when (map? match)
    (let
      [hits (into []
                  (comp (filter #(not (str/blank? (:snippet %))))
                        (map (fn [h]
                               (if (= :request (:side h))
                                 {:label "You" :role :user :text (:snippet h)}
                                 {:label "Vis" :role :ai :text (:snippet h)}))))
                  (:hits match))]
      (if (seq hits)
        hits
        (cond-> []
          (not (str/blank? (:request-snippet match)))
          (conj {:label "You" :role :user :text (:request-snippet match)})

          (not (str/blank? (:reply-snippet match)))
          (conj {:label "Vis" :role :ai :text (:reply-snippet match)}))))))

(defn- navigator-hit-entries
  "Inline snippet rows for one visible list row — empty when the row matched by
   title/project only and carries no transcript hits."
  [entry]
  (vec (take navigator-inline-hits (navigator-preview-entries (:transcript-match entry)))))

(defn- navigator-block-heights
  "Painted line count per session: optional project heading and top margin,
   title row, compact metadata row, transcript snippets, then one blank line."
  [visible-rows]
  (mapv (fn [entry]
          (+ 3 (if (:group-start? entry) 2 0) (count (navigator-hit-entries entry))))
        visible-rows))

(defn- navigator-scroll-start
  "First visible row index: the smallest scroll that still fits the selected
   row's own line inside `budget` painted lines, then pulled back so the window
   is never scrolled past the end (otherwise a tall block's window keeps its
   inherited scroll and paints one lonely row with dead space under it)."
  [heights selected scroll budget]
  (let
    [n
     (count heights)

     selected
     (long (max 0 (long selected)))

     budget
     (max 1 (long budget))

     ;; Smallest index whose remaining blocks all fit — scrolling past it only
     ;; wastes lines, so it is the hard upper bound for the window start.
     tail-start
     (loop
       [i
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
                 (if (and (< s selected) (> (long (reduce + 1 (subvec heights s selected))) budget))
                   (recur (inc s))
                   s))))))

(defn- navigator-visible-blocks
  "Paint plan from `start`, clipped by terminal lines. Every emitted session
   keeps its hierarchy/title/metadata base. Overflow snippets are clipped first;
   the blank spacer is omitted only when that base exactly fills the viewport."
  [visible-rows start budget]
  (let
    [n
     (count visible-rows)

     budget
     (long budget)]

    (loop
      [i
       (long (max 0 (long start)))

       used
       0

       acc
       []]

      (if (or (>= i n) (>= (long used) budget))
        acc
        (let
          [entry
           (nth visible-rows i)

           base
           (+ 2 (if (:group-start? entry) 2 0))]

          (if (> (+ (long used) base) budget)
            acc
            (let
              [remaining
               (max 0 (- budget (long used) base))

               spacer?
               (pos? remaining)

               hit-capacity
               (max 0 (- remaining (if spacer? 1 0)))

               hits
               (vec (take hit-capacity (navigator-hit-entries entry)))]

              (recur (inc i)
                     (+ (long used) base (count hits) (if spacer? 1 0))
                     (conj acc {:idx i :entry entry :hits hits :spacer? spacer?})))))))))

(defn- draw-navigator-group!
  [g x row width {:keys [dir work-dir group-count]}]
  (let
    [count-label
     (str group-count " " (if (= 1 group-count) "session" "sessions"))

     root-label
     (when (and (seq work-dir) (not= dir work-dir)) work-dir)

     label
     (str dir (when root-label (str "  ·  " root-label)) "  ·  " count-label)]

    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/styled g [p/BOLD] (p/put-str! g x row (p/ellipsize label (max 1 (long width)))))))

(defn- draw-navigator-session!
  [g x row width entry selected?]
  (let
    [focused?
     (:focused? entry)

     status
     (str (:status entry))

     status-w
     (p/display-width status)

     content-x
     (+ (long x) 2)

     title-w
     (max 1 (- (long width) 2 status-w 2))

     title
     (p/ellipsize (:title entry) title-w)

     title-shown-w
     (p/display-width title)

     ;; `title - opening ask`: a generated title rarely says what the session
     ;; actually opened with, so the first sentence of its first request rides
     ;; the SAME row, dimmed, in whatever width the title left over.
     opening-w
     (- title-w title-shown-w 3)

     opening
     (when (>= opening-w 8)
       (some-> (:opening entry)
               str
               not-empty
               (p/ellipsize opening-w)))

     status-x
     (+ (long x) (max 2 (- (long width) status-w)))

     metadata
     (str (:session entry) "  ·  " (:draft entry) "  ·  " (:modified entry))]

    (p/draw-selection-marker! g x row selected? t/dialog-hint-key)
    (p/set-colors! g (if focused? t/dialog-hint-key t/dialog-fg) t/dialog-bg)
    (if (or selected? focused?)
      (p/styled g [p/BOLD] (p/put-str! g content-x row title))
      (p/put-str! g content-x row title))
    (when opening
      (p/set-colors! g t/dialog-hint t/dialog-bg)
      (p/put-str! g (+ (long content-x) title-shown-w) row (str " - " opening)))
    (p/set-colors! g (if focused? t/dialog-hint-key t/dialog-hint) t/dialog-bg)
    (p/put-str! g status-x row status)
    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/put-str! g content-x (inc (long row)) (p/ellipsize metadata (max 1 (- (long width) 2))))))

(defn- draw-navigator-hit-line!
  "Paint one compact full-width transcript hit beneath its owning session."
  [g x row width query {:keys [label role text]}]
  (let
    [label
     (str label)

     text-x
     (+ (long x) 2 (count label) 2)

     available
     (max 0 (- (long width) 4 (count label)))]

    (p/set-colors! g (if (= role :user) t/user-role-fg t/ai-role-fg) t/dialog-bg)
    (p/styled g [p/BOLD] (p/put-str! g (+ (long x) 2) row label))
    (loop
      [segments
       (navigator-highlight-segments text query)

       cx
       text-x

       remaining
       available]

      (when (and (seq segments) (pos? (long remaining)))
        (let
          [[segment bold?]
           (first segments)

           segment
           (p/truncate-cols segment remaining)

           segment-w
           (p/display-width segment)]

          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (if bold?
            (p/styled g [p/BOLD] (p/put-str! g cx row segment))
            (p/put-str! g cx row segment))
          (recur (rest segments) (+ (long cx) segment-w) (- (long remaining) segment-w)))))))

(defn navigator-dialog!
  "Global C-g session picker. Full-width project/session hierarchy; transcript
   lookup is debounced and asynchronous so typing never waits on the gateway."
  [^TerminalScreen screen opts]
  (let
    [query
     (atom "")

     selected
     (atom 0)

     scroll
     (atom 0)

     scrollbar-drag-offset
     (volatile! nil)

     show-empty-untitled?
     (atom (boolean (:show-empty-untitled? opts)))

     search-transcript-ids
     (:search-transcript-ids opts)

     transcript-ids
     (atom {})

     transcript-query
     (atom nil)

     search-task
     (atom nil)

     search-generation
     (atom 0)

     search-result
     (atom nil)]

    (letfn
      [(start-search! []
         (let [q (str/trim @query)]
           (reset! transcript-ids {})
           (reset! transcript-query nil)
           (if (empty? q)
             (do (swap! search-generation inc)
                 (when-let [running @search-task]
                   (future-cancel running))
                 (reset! search-task nil)
                 (reset! search-result nil))
             (schedule-navigator-search! search-task
                                         search-generation
                                         search-result
                                         q
                                         search-transcript-ids))))
       (reset-list! [search?] (reset! selected 0) (reset! scroll 0) (when search? (start-search!)))]
      (try
        (loop []

          (when-let [{:keys [token query matches]} @search-result]
            (reset! search-result nil)
            (when (= token @search-generation)
              (reset! transcript-query query)
              (reset! transcript-ids matches)
              (reset! search-task nil)))
          (let
            [rows
             (navigator-all-rows (assoc opts :show-empty-untitled? @show-empty-untitled?))

             visible-rows
             (navigator-visible-rows rows @query @transcript-ids)

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
                                  (+ (long desired-lines) 4 (long navigator-inline-hits)))

             {:keys [left right inner-w]}
             bounds

             {:keys [content-top content-h hint-row]}
             (dialog-layout bounds)

             query-row
             content-top

             sb-gutter
             2

             content-w
             (long (max 1 (- (long inner-w) sb-gutter)))

             body-x
             (+ (long left) 2)

             body-w
             (long (max 1 (- content-w 2)))

             scrollbar-col
             (+ body-x body-w 1)

             body-top
             (+ (long content-top) 2)

             list-budget
             (max 2 (- (long content-h) 2))

             _
             (swap! selected #(p/clamp % 0 (max 0 (dec total))))

             block-heights
             (navigator-block-heights visible-rows)

             _
             (swap! scroll #(navigator-scroll-start block-heights @selected % list-budget))

             blocks
             (navigator-visible-blocks visible-rows @scroll list-budget)

             page-rows
             (max 1 (count blocks))]

            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
            (let
              [cursor-pos (draw-text-input-field! g
                                                  (inc (long left))
                                                  query-row
                                                  content-w
                                                  @query
                                                  (count @query))]
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/draw-separator! g left right (inc (long content-top)))
              (if (zero? total)
                (let
                  [hidden-count (count (filter empty-untitled-session? (:sessions opts)))
                   message (cond (not (str/blank? @query)) "No matches"
                                 (and (pos? hidden-count) (not @show-empty-untitled?))
                                 "Only empty untitled sessions hidden"
                                 :else "No sessions yet")
                   message-x (+ body-x (long (max 0 (quot (- body-w (count message)) 2))))]

                  (p/set-colors! g t/dialog-hint t/dialog-bg)
                  (p/put-str! g message-x (+ body-top 1) message))
                (loop
                  [remaining blocks
                   row body-top]

                  (when-let [{:keys [idx entry hits spacer?]} (first remaining)]
                    (let
                      [row (long row)
                       row (if (:group-start? entry)
                             (do (draw-navigator-group! g body-x row body-w entry) (+ row 2))
                             row)
                       spacer-row (+ row 2 (count hits))]

                      (when (< row (+ body-top list-budget))
                        (draw-navigator-session! g body-x row body-w entry (= idx @selected)))
                      (doseq [[hit-idx hit] (map-indexed vector hits)]
                        (let [hit-row (+ row 2 (long hit-idx))]
                          (when (< hit-row (+ body-top list-budget))
                            (draw-navigator-hit-line! g body-x hit-row body-w @query hit))))
                      (recur (rest remaining) (+ spacer-row (if spacer? 1 0)))))))
              (when (> total page-rows)
                (scrollbar/draw! g
                                 {:col scrollbar-col
                                  :top body-top
                                  :track-h list-budget
                                  :total-h total
                                  :inner-h page-rows
                                  :scroll @scroll}))
              (draw-hint-bar! g
                              left
                              hint-row
                              inner-w
                              [["↑/↓" "move"] ["Enter" "open"] ["C-n" "new"] ["C-f" "fork"]
                               ["C-d" "delete"] ["C-b" "project"]
                               [(keymap/chord \u)
                                (if @show-empty-untitled? "hide empty" "show empty")]
                               ["Esc" "cancel"]])
              (.setCursorPosition screen cursor-pos)
              (.refresh screen Screen$RefreshType/DELTA))
            (let [key (read-navigator-key! screen search-task search-result)]
              (if-not key
                (recur)
                (cond
                  (modal-wheel-step key)
                  (do (swap! selected #(p/clamp (+ (long %) (long (modal-wheel-step key)))
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
                                    (scrollbar/on-track? (.getColumn pos)
                                                         (.getRow pos)
                                                         {:col scrollbar-col
                                                          :top body-top
                                                          :track-h list-budget
                                                          :x-band 2}))))))
                  (let
                    [^MouseAction mouse key
                     action (.getActionType mouse)
                     pos (.getPosition mouse)
                     mouse-x (.getColumn pos)
                     mouse-y (.getRow pos)
                     geometry (scrollbar/geometry total page-rows list-budget @scroll)
                     apply-scroll! (fn [grip]
                                     (let
                                       [next-scroll (or (scrollbar/scroll-from-mouse-y mouse-y
                                                                                       body-top
                                                                                       list-budget
                                                                                       total
                                                                                       page-rows
                                                                                       grip)
                                                        0)]
                                       (reset! scroll next-scroll)
                                       (swap! selected #(p/clamp %
                                                                 next-scroll
                                                                 (min (dec total)
                                                                      (+ (long next-scroll)
                                                                         (dec page-rows)))))))]

                    (cond (= action MouseActionType/CLICK_RELEASE)
                          (do (vreset! scrollbar-drag-offset nil) (recur))
                          (and (= action MouseActionType/CLICK_DOWN)
                               (some? geometry)
                               (scrollbar/on-thumb? mouse-x
                                                    mouse-y
                                                    {:col scrollbar-col :top body-top :x-band 2}
                                                    geometry))
                          (let [thumb-top (+ body-top (long (:thumb-top-rel geometry)))]
                            (vreset! scrollbar-drag-offset (- (long mouse-y) thumb-top))
                            (recur))
                          (= action MouseActionType/CLICK_DOWN)
                          (let [grip (long (quot (long (or (:thumb-h geometry) 1)) 2))]
                            (vreset! scrollbar-drag-offset grip)
                            (apply-scroll! grip)
                            (recur))
                          (and (= action MouseActionType/DRAG) (some? @scrollbar-drag-offset))
                          (do (apply-scroll! (long @scrollbar-drag-offset)) (recur))
                          :else (recur)))
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
                       (= (lower-key-character key) \d))
                  (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                    {:action :delete :id id}
                    (recur))
                  (and (input/ctrl-modifier? key)
                       (= KeyType/Character (key-type key))
                       (= (lower-key-character key) \b))
                  (if-let [id (and (pos? total) (:id (:target (nth visible-rows @selected))))]
                    {:action :project :id id}
                    (recur))
                  (input/ctrl-char? key \u)
                  (do (swap! show-empty-untitled? not) (reset-list! false) (recur))
                  (input/paste-start? key) (do (let [pasted (drain-modal-paste! screen)]
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
                      (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total)))) (recur)))
                    KeyType/ArrowDown
                    (if (and (input/reorder-modifier? key) (pos? total))
                      (if-let [id (:id (:target (nth visible-rows @selected)))]
                        {:action :reorder :id id :dir :down}
                        (recur))
                      (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur)))
                    KeyType/Enter (when (pos? total) (:target (nth visible-rows @selected)))
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
                   (future-cancel running)))))))

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
   touch and `:prompt-h` the live height of the prompt it sits above, so the
   band always lands ABOVE the input box and under the header. The frame is
   snapshotted before the band paints and put back on the way out — the
   transcript underneath is never repainted from scratch and never blanked.

   `body` is called with `[g region]`, the same two handles every other host of
   `embed-transient!` composes. This is the only place the session screen turns
   an anchor into a band region: a second one is how two bands drift apart."
  [^TerminalScreen screen {:keys [content-top prompt-h]} body]
  (let
    [size
     (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

     g
     (.newTextGraphics screen)

     restore!
     (frame-restorer screen)

     region
     (assoc (tr/band-region (.getColumns size)
                            (.getRows size)
                            (or content-top 1)
                            (or prompt-h tr/prompt-rows))
       :restore! restore!)]

    ;; The band owns the keyboard while it is up, so it owns the CURSOR: left
    ;; where the last session paint parked it, the hardware caret went on
    ;; blinking inside the prompt behind the hydra, as if the band were not
    ;; there. Anything inside the band that reads typed text (`band-questions`)
    ;; places it again for itself.
    (.setCursorPosition screen nil)
    (try (body g region)
         (finally (when restore! (restore!))
                  (.setCursorPosition screen nil)
                  (.refresh screen Screen$RefreshType/DELTA)))))

(defn session-band!
  "Run ONE transient as a magit BAND inside the LIVE SESSION frame — the same
   `embed-transient!` component the magit status buffer, Settings and the
   provider manager embed, instanced here over the session's own region
   (`session-band-instance!`) instead of in a window of its own.

   `f` is called with `{:screen :g :region :result}` ONLY when the transient
   produced an action, on the band's own rows: that is where an inline
   minibuffer (`band-questions`) asks its follow-up question, on the hint row,
   instead of opening a modal. Returns `f`'s value, or nil on Esc.

   `pressed` is that action ALREADY chosen — a slash that names one command of
   this band (`/draft new`) is exactly that key, pre-pressed, so the band paints
   itself and goes straight to the question instead of waiting for a keystroke
   the human already typed."
  ([^TerminalScreen screen anchor spec f] (session-band! screen anchor spec f nil))
  ([^TerminalScreen screen anchor spec f pressed]
   (session-band-instance! screen
                           anchor
                           (fn [g region]
                             (when-let
                               [result (if pressed
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

;;; ── Questions a band asks on its OWN hint row ───────────────────────────────
;; `ctx` is what `session-band!` hands its `f`: the screen, its graphics and the
;; band's region. These wrappers are the whole reason a band's follow-up
;; question reads as a QUESTION at the call site — they name it, while
;; `band-questions` is what binds it to the band's own coordinates.

(defn- band-ask
  "The questions THIS band can ask, bound to the region `session-band!` handed
   `f` — one composed map instead of six coordinates unpacked again in every
   branch that has something to ask."
  [{:keys [screen g region]}]
  (band-questions screen g region))

(defn- band-read!
  "Ask for one line of text on the band's hint row; nil on Esc."
  ([ctx label] (band-read! ctx label {}))
  ([ctx label opts] ((:read! (band-ask ctx)) label opts)))

(defn- band-choose!
  "Ask WHICH one, single-key, on the band's hint row; returns the chosen `:id`."
  [ctx title choices]
  ((:choose! (band-ask ctx)) title choices))

(defn- band-confirm!
  "Ask y/n on the band's hint row."
  [ctx question]
  ((:confirm! (band-ask ctx)) question))

(defn- band-run!
  "Run ANOTHER transient over the SAME band region — a transient that opens a
   transient, which is how magit asks a second question without a second
   window. The frame is snapshotted and restored once, by the `session-band!`
   that owns this region."
  [ctx spec]
  ((:transient! (band-ask ctx)) spec))

;;; ── Drafts ──────────────────────────────────────────────────────────────────

(defn- draft-name-prompt
  "Which working tree the draft forks, said in the prompt itself: the band has
   no armed flag to read it off any more, so the question has to carry it."
  [clean?]
  (if clean? "Name the draft (from committed HEAD):" "Name the draft (with my changes):"))

(defn- name-new-draft!
  "`c` / `d`: name the draft inline, or back out. An empty name is a cancelled
   prompt, never an unnamed draft."
  [ctx {:keys [clean?] :as choice}]
  (when-let [label (band-read! ctx (draft-name-prompt clean?))]
    (when-let [label (not-empty (str/trim label))]
      (assoc choice :label label))))

(defn- switch-draft!
  "`s`: the SWITCH band — trunk and every draft, `●` on the one we are in —
   resolved to the choice the screen's draft executor speaks."
  [ctx draft-rows]
  (drafts/switch-choice draft-rows (band-run! ctx (drafts/switch-spec draft-rows))))

(defn- abandon-draft!
  "`k`: WHICH draft (asked only when more than one exists), then a y/n that
   names the draft it is about. Both on the hint row."
  [ctx draft-rows]
  (let
    [choices
     (drafts/abandon-choices draft-rows)

     ws-id
     (if (= 1 (count choices)) (:id (first choices)) (band-choose! ctx "Abandon draft:" choices))

     row
     (drafts/row-by-id (drafts/rows draft-rows) ws-id)]

    (when (and row
               (band-confirm!
                 ctx
                 (str "Permanently discard '" (:label row) "' and its isolated files?")))
      {:action :abandon
       :workspace-id ws-id
       :label (:label row)
       :reason "abandoned from the TUI draft transient"})))

(defn- draft-band-choice
  "ONE finished keystroke of the draft band, answered: create asks for a name,
   switch opens the switch band, abandon asks which and then whether."
  [ctx draft-rows]
  (let [choice (drafts/choice (:result ctx))]
    (case (:action choice)
      :new
      (name-new-draft! ctx choice)

      :switch
      (switch-draft! ctx draft-rows)

      :abandon
      (abandon-draft! ctx draft-rows)

      choice)))

(defn draft-transient!
  "The DRAFT band: create, switch and abandon a draft workspace without ever
   leaving the session. Returns the choice the screen executes — `:trunk`,
   `:draft`, `:new` (with the typed `:label` and `:clean?`) or `:abandon` (with
   `:workspace-id` and `:reason`) — or nil when the human backed out at any
   step.

   Creating, switching and abandoning are three separate keys: `c`/`d` fork a
   draft and name it inline, `s` opens the switch band, `k` abandons one. No
   step opens a window.

   `pressed` is one of those commands named by a SLASH instead of by a key
   (`drafts/slash-band`): `/draft new` IS `d`, already pressed. A command this
   band does not offer right now (`/draft resume` with no drafts) opens the band
   itself rather than firing something the human was never shown."
  ([^TerminalScreen screen anchor draft-rows] (draft-transient! screen anchor draft-rows nil))
  ([^TerminalScreen screen anchor draft-rows pressed]
   (let [spec (drafts/spec draft-rows)]
     (session-band! screen
                    anchor
                    spec
                    #(draft-band-choice % draft-rows)
                    (when (tr/item-by-id spec pressed) pressed)))))

;;; ── Where a NEW session starts ──────────────────────────────────────────────

(defn- start-in-band-choice
  "ONE finished keystroke of the start-in band: the project itself is the whole
   answer, a draft still needs its name."
  [ctx]
  (let [choice (drafts/start-in-choice (:result ctx))]
    (cond (nil? choice) nil
          (= :trunk (:start-in choice)) choice
          :else (when-let [label (band-read! ctx (draft-name-prompt (:clean? choice)))]
                  (when-let [draft (drafts/draft-spec choice label)]
                    (assoc choice :draft draft))))))

(defn start-in-transient!
  "Ask WHERE a new session starts, as a band in the current session's frame:
   `t` the project itself, `c` a draft forked from the committed HEAD, `d` a
   draft carrying the uncommitted working tree. A draft is named inline on the
   hint row.

   Returns `{:start-in :trunk}`, `{:start-in :draft :clean? bool :draft {:label :clean?}}`,
   or nil."
  [^TerminalScreen screen anchor]
  (session-band! screen anchor drafts/start-in-spec start-in-band-choice))

(def palette-commands
  "Command palette entries. Each is {:id keyword :label str}. The `:id` is the
   action the screen's `run-command!` executes. Quit is intentionally NOT here
   — use Ctrl+C to quit.

   The palette is THE discoverable entry point for every app verb: opened with
   C-x p (reliable on every terminal, unlike Alt/Option chords on macOS) and
   filtered by typing."
  ;; Whole-session Markdown copy lives in the header as an icon. Draft ops
  ;; are slash-only (`/draft …`) and surface through `menu-commands` which
  ;; aggregates them from the engine slash registry (passed as extra-commands).
  [{:id :search-open :label "Search in Session"} {:id :show-sessions :label "Switch Session"}
   {:id :pick-file :label "Attach File"} {:id :toggle-voice-recording :label "Voice Recording"}
   {:id :new-session :label "New Session"} {:id :new-session-in :label "New Session in a Draft…"}
   ;; Both fork verbs are `:has-turns`-gated: a session with no turns has
   ;; nothing to fork, so the palette must not even offer them.
   {:id :fork-session :label "Fork Session" :show-when :has-turns}
   {:id :fork-at-turn :label "Fork Session at Turn…" :show-when :has-turns}
   {:id :close-tab :label "Close Tab"} {:id :providers :label "Providers"}
   {:id :mcp :label "MCP Servers"} {:id :settings :label "Settings"}
   {:id :toggle-all-details :label "Fold / Unfold All"}
   {:id :toggle-detail-labels :label "Label Folds — jump to one"}
   {:id :toggle-help :label "Keyboard Shortcuts"}])

(defn fork-turn-items
  "Rows for the fork-at-turn palette (`searchable-select!`), one per turn of the
   current session (from `db-list-session-turns`), top-to-bottom. Each row's
   `:label` is the turn's user message (whitespace-collapsed, truncated) and
   `:hint` its ordinal `tN`; `:turn-id` carries the `session_turn_soul` id the
   fork copies THROUGH — selecting a row forks the session keeping every turn up
   to and INCLUDING it. Type to filter by message text."
  [turns]
  (mapv (fn [i turn]
          (let
            [n
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

   `ctx` is `{:has-turns? bool}`; a missing/nil ctx is the conservative
   turnless case. Untagged entries always survive."
  [{:keys [has-turns?]}]
  (filterv (fn [{:keys [show-when]}]
             (case show-when
               :has-turns
               (boolean has-turns?)

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
   (let
     [with-hints (mapv (fn [c]
                         (assoc c :hint (keymap/label-for (:id c))))
                       (palette-commands-for ctx))]
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
  (let
    [providers
     (try (vis/picker-fleet)
          (catch Throwable t (tel/log! :warn ["dialogs: picker-fleet failed" (ex-message t)]) nil))

     cur-provider
     (some-> (:provider current)
             name)

     cur-model
     (:model current)

     model-rows
     (for
       [p
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

      (let
        [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
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
         scroll-col (+ (long left) (long inner-w))
         text-w (max 1 (- (long inner-w) 3))
         lines (vec (mapcat #(render/wrap-text % text-w) (str/split-lines (or text "(empty)"))))
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
        (doseq
          [row (range (+ (long content-top) (count visible))
                      (+ (long content-top) (long content-h)))]
          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (p/fill-rect! g (inc (long left)) row inner-w 1))
        ;; Scrollbar - same style as the chat messages area: a vertical
        ;; track of │ plus a solid █ thumb sized proportionally to the
        ;; visible window. Drawn over the content's right margin, on the
        ;; dialog background so it visually blends with the dialog frame.
        (when (> total (long content-h))
          (let
            [track-h (long content-h)
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
        (.refresh screen Screen$RefreshType/DELTA)
        (let [key (read-modal-key! screen)]
          (when key
            (condp = (key-type key)
              KeyType/Escape nil
              KeyType/ArrowUp (do (swap! scroll #(max 0 (dec (long %)))) (recur))
              KeyType/ArrowDown (do (swap! scroll #(min max-scroll (inc (long %)))) (recur))
              KeyType/Character (recur)
              (recur))))))))

;;; ── Markdown viewer dialog ──────────────────────────────────────────────────
(defn- md-run-paint!
  "Paint one styled IR run at column `x`; returns the next x. Style →
   dialog-palette mapping: headings title-accent bold, code/links/list
   markers hint-key accent, dim/quote hint, **bold**/_italic_ as SGR."
  [g x row {:keys [text style]}]
  (let
    [style
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
  (let
    [scroll
     (atom 0)

     ir
     (try
       (vis/markdown->ast (str md))
       (catch Throwable t (tel/log! :warn ["dialogs: markdown->ast failed" (ex-message t)]) nil))]

    (if (nil? ir)
      (text-viewer-dialog! screen title (str md))
      (loop []

        (let
          [size
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
           (+ (long left) (long inner-w))

           text-w
           (max 1 (- (long inner-w) 3))

           lines
           (try
             (layout/ast->lines ir text-w)
             (catch Throwable t (tel/log! :warn ["dialogs: ast->lines failed" (ex-message t)]) []))

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
          (doseq
            [row (range (+ (long content-top) (count visible))
                        (+ (long content-top) (long content-h)))]
            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/fill-rect! g (inc (long left)) row inner-w 1))
          (when (> total (long content-h))
            (let
              [track-h
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
          (.refresh screen Screen$RefreshType/DELTA)
          (let [key (read-modal-key! screen)]
            (when key
              (condp = (key-type key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! scroll #(max 0 (dec (long %)))) (recur))
                KeyType/ArrowDown (do (swap! scroll #(min max-scroll (inc (long %)))) (recur))
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
  (let
    [items
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
      (let
        [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
         cols (.getColumns size)
         rows (.getRows size)
         g (.newTextGraphics screen)
         bounds (draw-dialog-chrome! g cols rows "Copy Messages" ch)
         {:keys [left inner-w]} bounds
         total (count items)
         {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
         visible (min total (long content-h))
         _ (swap! selected #(p/clamp % 0 (max 0 (dec total))))
         _ (swap! scroll #(visible-window-start @selected % content-h total))]

        (dotimes [i visible]
          (let
            [idx (+ (long @scroll) i)
             row (+ (long content-top) i)]

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
                KeyType/ArrowUp (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total))))
                                    (recur status))
                KeyType/ArrowDown
                (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur status))
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
