(ns com.blockether.vis.ext.channel-tui.dialogs
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.file-picker :as picker])
  (:import [com.googlecode.lanterna TextColor$RGB]
           [com.googlecode.lanterna Symbols]
           [com.googlecode.lanterna.input KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]))

;;; ── Shared dialog chrome & components ───────────────────────────────────────

(def ^:private input-field-bg (TextColor$RGB. 255 255 252))

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

(def ^:const DEFAULT_DIALOG_WIDTH_RATIO
  "Fraction of terminal width every modal occupies by default."
  0.7)

(def ^:const DEFAULT_DIALOG_MIN_WIDTH 50)
(def ^:const DEFAULT_DIALOG_MAX_WIDTH 100)

(def ^:const DEFAULT_DIALOG_HEIGHT_RATIO
  "Fraction of terminal height every modal occupies by default. Shared
   across the whole ESC stack so dialogs keep the same footprint."
  0.55)

(def ^:const DEFAULT_DIALOG_MIN_HEIGHT
  "Minimum dialog box height in rows. Keeps the shared modal footprint
   deliberate even on shorter terminals."
  14)

(def ^:const DEFAULT_DIALOG_MAX_HEIGHT
  "Maximum default dialog box height in rows. Leaves enough surrounding
   chat visible that a modal still feels like an overlay instead of a
   screen takeover."
  24)

(def ^:const DIALOG_CHROME_W 4)  ;; border(1) + pad(1) each side
(def ^:const DIALOG_CHROME_H 6)  ;; top + title + sep + body + sep + hint + bot

(defn default-content-width
  "Shared content width every dialog uses, derived from `cols`. Clamped
   between `DEFAULT_DIALOG_MIN_WIDTH` and `DEFAULT_DIALOG_MAX_WIDTH` and
   bounded by the terminal so the box never paints off-screen."
  [cols]
  (let [box-w (-> (int (* cols DEFAULT_DIALOG_WIDTH_RATIO))
                (max DEFAULT_DIALOG_MIN_WIDTH)
                (min DEFAULT_DIALOG_MAX_WIDTH)
                (min (max DEFAULT_DIALOG_MIN_WIDTH (- cols 4))))]
    (max 1 (- box-w DIALOG_CHROME_W))))

(defn default-content-height
  "Shared content height every dialog uses, derived from `rows`.
   Clamped to a common modal footprint so dialogs keep equal height."
  [rows]
  (let [box-h (-> (int (* rows DEFAULT_DIALOG_HEIGHT_RATIO))
                (max DEFAULT_DIALOG_MIN_HEIGHT)
                (min DEFAULT_DIALOG_MAX_HEIGHT)
                (min (max DEFAULT_DIALOG_MIN_HEIGHT (- rows 4))))]
    (max 1 (- box-h DIALOG_CHROME_H))))

(defn clear-screen!
  "Fill the entire screen with terminal background. Call before sub-dialogs
   to cleanly replace the current dialog (wizard step pattern)."
  [^TerminalScreen screen]
  (let [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
        cols (.getColumns size)
        rows (.getRows size)
        g    (.newTextGraphics screen)]
    (p/set-bg! g t/terminal-bg)
    (p/fill-rect! g 0 0 cols rows)
    (.refresh screen Screen$RefreshType/DELTA)))

(defn clamp [x lo hi]
  (max lo (min hi x)))

(defn ellipsize [s max-w]
  (let [txt (or s "")]
    (cond
      (<= max-w 0) ""
      (<= (count txt) max-w) txt
      (= max-w 1) "…"
      :else (str (subs txt 0 (dec max-w)) "…"))))

(defn dialog-layout
  "Compute content area layout. When `content-count` is provided and smaller than
   the available height, content is vertically centered within the frame.
   Layout: border → title bar → top separator → CONTENT → bottom separator → hint → border."
  ([bounds] (dialog-layout bounds nil))
  ([{:keys [top bottom]} content-count]
   (let [raw-top      (+ top 3)
         hint-row     (- bottom 1)
         bot-sep-row  (- bottom 2)
         content-bot  (dec bot-sep-row)
         full-h      (max 1 (inc (- content-bot raw-top)))
         v-offset    (if (and content-count (< content-count full-h))
                       (quot (- full-h content-count) 2)
                       0)
         content-top (+ raw-top v-offset)
         ;; Usable height from centered top — never exceeds content-bot
         content-h   (max 1 (inc (- content-bot content-top)))]
     {:content-top content-top
      :content-bottom content-bot
      :content-h content-h
      :hint-row hint-row})))

(defn visible-window-start
  [idx current-start visible-count total-count]
  (let [last-start (max 0 (- total-count visible-count))
        start      (clamp current-start 0 last-start)]
    (cond
      (< idx start) idx
      (>= idx (+ start visible-count)) (max 0 (- idx (dec visible-count)))
      :else start)))

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
  (let [text-w  (max 0 (- inner-w 2))
        text-x  (+ left 2)]
    (p/set-colors! g t/dialog-hint t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (cond
      ;; Plain string
      (string? hint)
      (p/put-str! g text-x row (ellipsize hint text-w))

      ;; Vec of [key action] pairs — render keys in italic
      (and (vector? hint) (seq hint) (vector? (first hint)))
      (let [labels    (mapv (fn [[k a]] (str k " " a)) hint)
            ;; Walk through the laid-out string to find each key's position
            ;; Simpler: compute column offsets from space-between math
            n         (count labels)
            total-txt (reduce + (map count labels))
            total-gap (- text-w total-txt)
            gap-count (max 1 (dec n))
            base-gap  (max 1 (quot total-gap gap-count))
            extra     (- total-gap (* base-gap gap-count))]
        (loop [i 0 col text-x]
          (when (< i n)
            (let [[k a]   (nth hint i)
                  gap     (if (< i (dec n))
                            (+ base-gap (if (< i extra) 1 0))
                            0)]
              ;; Key part — bold, stronger color
              (p/set-fg! g t/dialog-hint-key)
              (p/styled g [p/BOLD]
                (p/put-str! g col row k))
              ;; Action part — normal hint color, italic
              (p/set-fg! g t/dialog-hint)
              (p/styled g [p/ITALIC]
                (p/put-str! g (+ col (count k)) row (str " " a)))
              (recur (inc i) (+ col (count k) 1 (count a) gap))))))

      ;; Vec of strings — space-between, all italic
      (vector? hint)
      (p/styled g [p/ITALIC]
        (p/draw-space-between! g text-x row text-w hint)))))

(defn- draw-list-item!
  [g left row inner-w selected? label]
  (let [prefix    (if selected? "▸ " "  ")
        draw-text (ellipsize (str prefix label) (max 0 (- inner-w 2)))]
    (if selected?
      (p/set-colors! g t/dialog-bg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/fill-rect! g (inc left) row inner-w 1)
    (p/put-str! g (+ left 2) row draw-text)))

(defn- draw-checkbox-item!
  [g left row inner-w selected? checked? label]
  (let [mark      (if checked? "✓" " ")
        prefix    (str "[" mark "] ")
        draw-text (ellipsize (str prefix label) (max 0 (- inner-w 2)))]
    (if selected?
      (p/set-colors! g t/dialog-bg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/fill-rect! g (inc left) row inner-w 1)
    (p/put-str! g (+ left 2) row draw-text)))

(defn- draw-text-input-field!
  [g left row inner-w text cursor]
  (let [box-left   (+ left 2)
        box-w      (max 3 (- inner-w 2))
        input-left (inc box-left)
        input-w    (max 1 (- box-w 2))
        h-off      (max 0 (- cursor (dec input-w)))
        visible    (subs text h-off (min (count text) (+ h-off input-w)))]
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/draw-box! g box-left row box-w 3)
    (p/set-colors! g t/box-fg input-field-bg)
    (p/fill-rect! g input-left (inc row) input-w 1)
    (p/put-str! g input-left (inc row) visible)
    (p/cursor-pos (+ input-left (- cursor h-off)) (inc row))))

(defn draw-dialog-chrome!
  "Draw dialog background, shadow, border, and title.

   Three arities:
   - `(g cols rows title content-h)` — shared default width and height.
     Caller-supplied `content-h` is ignored in this arity on purpose;
     the whole TUI dialog stack now uses one common footprint.
   - `(g cols rows title content-w content-h)` — fully explicit. Use
     only when a dialog genuinely needs a non-default width.

   Returns {:left :top :right :bottom :inner-w :inner-h}."
  ([g cols rows title _content-h]
   (draw-dialog-chrome! g cols rows title
     (default-content-width cols)
     (default-content-height rows)))
  ([g cols rows title content-w content-h]
   (let [[box-w box-h] (render/golden-dialog-size cols rows content-w content-h)
         box-left      (quot (- cols box-w) 2)
         box-top       (quot (- rows box-h) 2)
         box-right     (+ box-left box-w -1)
         box-bottom    (+ box-top box-h -1)
         inner-w       (- box-w 2)]

    ;; Shadow — clipped to terminal bounds
     (let [shd-left (+ box-left 2)
           shd-top  (inc box-top)
           shd-w    (min box-w (- cols shd-left))
           shd-h    (min box-h (- rows shd-top))]
       (when (and (pos? shd-w) (pos? shd-h))
         (p/set-bg! g t/dialog-shadow)
         (p/fill-rect! g shd-left shd-top shd-w shd-h)))

    ;; Background
     (p/set-bg! g t/dialog-bg)
     (p/fill-rect! g box-left box-top box-w box-h)

    ;; Border
     (p/set-colors! g t/dialog-border t/dialog-bg)
     (p/draw-box! g box-left box-top box-w box-h)

    ;; Title bar — full-width accent stripe with centered title
     (let [title-row  (inc box-top)
           title-text (ellipsize (or title "") (max 0 (- inner-w 2)))
           tx         (+ box-left 1 (quot (- inner-w (count title-text)) 2))]
      ;; Accent bar background
       (p/set-bg! g t/dialog-title-bg)
       (p/fill-rect! g (inc box-left) title-row inner-w 1)
      ;; Title text
       (p/set-fg! g t/dialog-title-fg)
       (p/put-str! g tx title-row title-text)
      ;; Top separator — below title bar
       (p/set-colors! g t/dialog-border t/dialog-bg)
       (p/draw-separator! g box-left box-right (inc title-row))
      ;; Bottom separator — above hint bar
       (let [bot-sep (- box-bottom 2)]
         (when (> bot-sep (+ box-top 3))
           (p/draw-separator! g box-left box-right bot-sep))))

     {:left box-left :top box-top :right box-right :bottom box-bottom
      :inner-w inner-w :inner-h (- box-h 2)})))

;;; ── Selection dialog ────────────────────────────────────────────────────────

(defn select-dialog!
  "Show a selection list dialog. Returns selected item map or nil on Esc.
   `items` is a vec of {:label str, ...} maps."
  [^TerminalScreen screen title items]
  (let [selected  (atom 0)
        scroll    (atom 0)
        ch        (count items)]
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title ch)
            {:keys [left inner-w]} bounds
            total  (count items)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
            visible (min total content-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % content-h total))]

        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]
            (when (< idx total)
              (draw-list-item! g left row inner-w (= idx @selected)
                (:label (nth items idx))))))

        (draw-hint-bar! g left hint-row inner-w [["↑/↓" "move"] ["Enter" "select"] ["Esc" "cancel"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape    nil
              KeyType/ArrowUp   (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
              KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
              KeyType/Enter     (when (pos? total) (nth items @selected))
              (recur))))))))

;;; ── File picker dialog ──────────────────────────────────────────────────────

(def ^:private file-picker-max-visible 10)

(defn- open-picker-item!
  [{:keys [path]}]
  (future
    (try
      (opener/open! path)
      (catch Throwable t
        {:status :spawn-failed
         :command nil
         :scheme nil
         :target path
         :error (.getMessage t)}))))

(defn- draw-file-picker-item!
  [g left row inner-w selected? {:keys [status-label name parent size-label age-label]}]
  (let [meta-text (str size-label "  " age-label)
        text-x    (+ left 2)
        text-w    (max 1 (- inner-w 2))
        right-w   (count meta-text)
        gap-w     (if (str/blank? meta-text) 0 2)
        left-w    (max 1 (- text-w right-w gap-w))
        path-text (if (= parent ".")
                    (str "[" status-label "] " name)
                    (str "[" status-label "] " name " · " parent))]
    (if selected?
      (p/set-colors! g t/dialog-bg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/fill-rect! g (inc left) row inner-w 1)
    (p/put-str! g text-x row (ellipsize path-text left-w))
    (when-not (str/blank? meta-text)
      (p/put-str! g (+ text-x (- text-w right-w)) row meta-text))))

(defn file-picker-dialog!
  "Interactive `@` file picker. Type to filter repo files, Enter inserts
   the selected relative path, Esc cancels. `Alt+I` toggles ignored files;
   `Alt+S` cycles sort mode; `Alt+O` opens the selection externally."
  [^TerminalScreen screen]
  (let [entries          (picker/collect-file-picker-entries)
        query            (atom "")
        include-ignored? (atom false)
        sort-mode        (atom :auto)
        selected         (atom 0)
        scroll           (atom 0)]
    (loop []
      (let [items         (picker/file-picker-items entries @query {:include-ignored? @include-ignored?
                                                                    :sort-mode @sort-mode})
            total         (count items)
            content-lines (+ 5 (max 1 (min total file-picker-max-visible)))
            size          (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols          (.getColumns size)
            rows          (.getRows size)
            g             (.newTextGraphics screen)
            bounds        (draw-dialog-chrome! g cols rows "Attach File" content-lines)
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds content-lines)
            _             (swap! selected #(clamp % 0 (max 0 (dec total))))
            mode-row      (+ content-top 4)
            list-top      (+ content-top 5)
            list-h        (max 1 (- content-h 5))
            visible       (min total list-h)
            _             (swap! scroll #(visible-window-start @selected % list-h total))
            mode-line     (str "Ignored: " (if @include-ignored? "on" "off")
                            "   Sort: " (picker/sort-label @sort-mode @query))]

        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc left) content-top inner-w content-h)
        (let [cursor-pos (draw-text-input-field! g left content-top inner-w @query (count @query))]
          (p/set-colors! g t/dialog-hint t/dialog-bg)
          (p/put-str! g (+ left 2) mode-row (ellipsize mode-line (max 1 (- inner-w 2))))

          (if (zero? total)
            (do
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g (+ left 2) list-top "No matching files."))
            (dotimes [i visible]
              (let [idx (+ @scroll i)
                    row (+ list-top i)]
                (when (< idx total)
                  (draw-file-picker-item! g left row inner-w (= idx @selected)
                    (nth items idx))))))

          (draw-hint-bar! g left hint-row inner-w
            [["type" "filter"] ["Alt+I" "ignored"] ["Alt+S" "sort"] ["Alt+O" "open"] ["↑/↓" "move"] ["Enter" "attach"] ["Esc" "cancel"]])
          (.setCursorPosition screen cursor-pos))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (cond
              (instance? MouseAction key)
              (let [^MouseAction ma key
                    atype (.getActionType ma)
                    pos   (.getPosition ma)
                    mx    (.getColumn pos)
                    my    (.getRow pos)
                    hit-idx (when (and (>= mx (inc left)) (< mx (+ left 1 inner-w))
                                    (>= my list-top) (< my (+ list-top visible)))
                              (+ @scroll (- my list-top)))]
                (cond
                  (and (= atype MouseActionType/CLICK_DOWN) (some? hit-idx) (< hit-idx total))
                  (do (reset! selected hit-idx)
                    (recur))

                  (and (= atype MouseActionType/CLICK_RELEASE) (some? hit-idx) (< hit-idx total))
                  (:path (nth items hit-idx))

                  :else
                  (recur)))

              :else
              (condp = (.getKeyType key)
                KeyType/Escape nil
                KeyType/ArrowUp (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total)))) (recur))
                KeyType/ArrowDown (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total)))) (recur))
                KeyType/Backspace (do (swap! query #(if (seq %) (subs % 0 (dec (count %))) %))
                                    (reset! selected 0)
                                    (reset! scroll 0)
                                    (recur))
                KeyType/Enter (when (pos? total) (:path (nth items @selected)))
                KeyType/Character
                (let [raw-c (.getCharacter key)
                      c     (Character/toLowerCase raw-c)]
                  (cond
                    (and (.isAltDown key) (= c \i))
                    (do (swap! include-ignored? not)
                      (reset! selected 0)
                      (reset! scroll 0)
                      (recur))

                    (and (.isAltDown key) (= c \s))
                    (do (swap! sort-mode picker/cycle-sort-mode)
                      (reset! selected 0)
                      (reset! scroll 0)
                      (recur))

                    (and (.isAltDown key) (= c \o))
                    (do
                      (when (pos? total)
                        (open-picker-item! (nth items @selected)))
                      (recur))

                    (Character/isISOControl raw-c)
                    (recur)

                    :else
                    (do (swap! query str raw-c)
                      (reset! selected 0)
                      (reset! scroll 0)
                      (recur))))
                (recur)))))))))

;;; ── Text input dialog ───────────────────────────────────────────────────────

(defn- text-input-body-lines
  [body]
  (cond
    (nil? body) []
    (string? body) (str/split-lines body)
    (sequential? body) (mapv str body)
    :else [(str body)]))

(defn text-input-dialog!
  "Show a text input dialog. Returns string or nil on Esc.
   Options: :mask char (e.g. \\* for passwords), :initial string,
   :body string-or-lines rendered above the input label."
  [^TerminalScreen screen title label & {:keys [mask initial body] :or {initial ""}}]
  (let [text         (atom (vec initial))
        cursor       (atom (count initial))
        body-lines   (text-input-body-lines body)
        paste-buffer (volatile! nil)]
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            ;; Content: optional body rows + label row + spacer + 3-row bordered input box.
            bounds (draw-dialog-chrome! g cols rows title 5)
            {:keys [left inner-w]} bounds
            text-w     (max 1 (- inner-w 2))
            wrapped-body (->> body-lines
                           (mapcat (fn [line]
                                     (if (str/blank? line)
                                       [""]
                                       (render/wrap-text line text-w))))
                           vec)
            body-gap   (if (seq wrapped-body) 1 0)
            content-count (+ 4 body-gap (count wrapped-body))
            {:keys [content-top content-h hint-row]} (dialog-layout bounds content-count)
            max-body-lines (max 0 (- content-h 4 body-gap))
            visible-body (if (<= (count wrapped-body) max-body-lines)
                           wrapped-body
                           (conj (vec (take (max 0 (dec max-body-lines)) wrapped-body)) "…"))
            label-row  (+ content-top (count visible-body) body-gap)
            input-row  (inc label-row)
            txt        (apply str @text)
            display    (if mask (apply str (repeat (count txt) mask)) txt)
            cursor-pos (draw-text-input-field! g left input-row inner-w display @cursor)]

        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[idx line] (map-indexed vector visible-body)]
          (let [row (+ content-top idx)]
            (p/fill-rect! g (inc left) row inner-w 1)
            (p/put-str! g (+ left 2) row (ellipsize line text-w))))
        (p/fill-rect! g (inc left) label-row inner-w 1)
        (p/put-str! g (+ left 2) label-row (ellipsize label (max 0 (- inner-w 2))))

        (draw-hint-bar! g left hint-row inner-w [["←/→" "move"] ["Enter" "confirm"] ["Esc" "cancel"]])
        (.setCursorPosition screen cursor-pos)
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (cond
              ;; ── Bracketed paste ──────────────────────────────
              ;; Three-state machine matching the main input loop.
              ;; START → open buffer; END → flush into text.
              ;; Prevents PUA marker chars (\uE200, \uE201) from
              ;; leaking into the dialog value — they break HTTP
              ;; Authorization headers when pasted API keys carry
              ;; them into the Bearer token.
              (input/paste-start? key)
              (do (vreset! paste-buffer (StringBuilder.))
                (recur))

              (input/paste-end? key)
              (let [^StringBuilder sb @paste-buffer]
                (when sb
                  (let [payload (.toString sb)
                        chars   (vec payload)]
                    (vreset! paste-buffer nil)
                    (when-not (.isEmpty payload)
                      (swap! text
                        (fn [t]
                          (into (subvec t 0 @cursor)
                            (concat chars (subvec t @cursor)))))
                      (swap! cursor + (count chars)))))
                (recur))

              ;; Accumulate chars into the paste buffer while open.
              (some? @paste-buffer)
              (do (when-let [ch (input/keystroke->paste-char key)]
                    (.append ^StringBuilder @paste-buffer ch))
                (recur))

              ;; ── Regular key dispatch ─────────────────────────
              :else
              (condp = (.getKeyType key)
                KeyType/Escape nil
                KeyType/Enter  (str/trim (apply str @text))

                KeyType/Character
                (let [c (.getCharacter key)]
                  (swap! text #(into (subvec % 0 @cursor) (cons c (subvec % @cursor))))
                  (swap! cursor inc)
                  (recur))

                KeyType/Backspace
                (do (when (pos? @cursor)
                      (swap! text #(into (subvec % 0 (dec @cursor)) (subvec % @cursor)))
                      (swap! cursor dec))
                  (recur))

                KeyType/ArrowLeft  (do (swap! cursor #(max 0 (dec %))) (recur))
                KeyType/ArrowRight (do (swap! cursor #(min (count @text) (inc %))) (recur))
                (recur)))))))))

;;; ── Confirm dialog ──────────────────────────────────────────────────────────

(defn- draw-button!
  "Draw a button label. Selected = accent bg, normal = dialog bg."
  [g col row label selected?]
  (let [text (str " " label " ")
        w    (count text)]
    (if selected?
      (p/set-colors! g t/dialog-title-fg t/dialog-title-bg)
      (p/set-colors! g t/dialog-fg t/dialog-bg))
    (p/put-str! g col row text)
    w))

(defn confirm-dialog!
  "Show Y/N confirmation with side-by-side buttons. Returns true/false, nil on Esc."
  [^TerminalScreen screen title message]
  (let [raw-lines  (if (string? message) [message] message)
        btn-yes    "Yes"
        btn-no     "No"
        btn-w      (+ 2 (max (count btn-yes) (count btn-no))) ;; " Yes " / " No  "
        btn-gap    4
        ;; content: message lines + blank + button row = lines + 2
        ch         (+ (count raw-lines) 2)
        focus      (atom 0)] ;; 0 = Yes, 1 = No
    (loop []
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows title ch)
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds ch)
            text-w (max 0 (- inner-w 2))
            lines  (vec (mapcat #(render/wrap-text % text-w) raw-lines))
            btn-row (+ content-top (count lines) 1) ;; blank line then buttons
            ;; Center buttons horizontally
            total-btn-w (+ btn-w btn-gap btn-w)
            btn-start   (+ left 1 (quot (- inner-w total-btn-w) 2))]

        ;; Message text — centered per line
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[i line] (map-indexed vector lines)]
          (let [row (+ content-top i)]
            (when (< row (+ content-top content-h))
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/draw-centered! g (inc left) row inner-w line))))

        ;; Buttons — side by side
        (p/set-bg! g t/dialog-bg)
        (p/fill-rect! g (inc left) btn-row inner-w 1)
        (draw-button! g btn-start btn-row btn-yes (= @focus 0))
        (draw-button! g (+ btn-start btn-w btn-gap) btn-row btn-no (= @focus 1))

        (draw-hint-bar! g left hint-row inner-w [["←/→" "switch"] ["Enter" "confirm"] ["Esc" "cancel"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape     nil
              KeyType/Enter      (= @focus 0) ;; true if Yes focused
              KeyType/ArrowLeft  (do (reset! focus 0) (recur))
              KeyType/ArrowRight (do (reset! focus 1) (recur))
              KeyType/Tab        (do (swap! focus #(if (zero? %) 1 0)) (recur))
              KeyType/Character
              (let [c (Character/toLowerCase (.getCharacter key))]
                (cond (= c \y) true (= c \n) false :else (recur)))
              (recur))))))))

;;; ── Settings dialog ─────────────────────────────────────────────────────────

(def ^:private reasoning-choice-order
  [:quick :balanced :deep])

(def ^:private codex-verbosity-choice-order
  [:low :medium :high])

(def ^:private settings-sections
  [{:id :extensions :label "Extensions"}
   {:id :ui         :label "UI"}])

(def ^:private settings-options-by-section
  {:extensions [{:key :reasoning-level
                 :type :choice
                 :choices reasoning-choice-order
                 :label "Reasoning effort"
                 :description "Base thinking depth for reasoning-capable models: quick / balanced / deep"}
                {:key :openai-codex-verbosity
                 :type :choice
                 :choices codex-verbosity-choice-order
                 :label "OpenAI Codex verbosity"
                 :description "Output detail for OpenAI Codex only: low / medium / high"}]
   :ui         [{:key :show-thinking
                 :type :toggle
                 :label "Show thinking / reasoning"
                 :description "LLM's chain-of-thought reasoning above each iteration"}
                {:key :show-iterations
                 :type :toggle
                 :label "Show full execution trace"
                 :description "Blocks, eval results, stdout, errors — the whole iteration history"}
                {:key :show-timestamps
                 :type :toggle
                 :label "Show per-message timestamps"
                 :description "Date+time next to every 'You' / 'Vis' label"}
                {:key :mouse-selection-copy
                 :type :toggle
                 :label "Mouse selection auto-copy"
                 :description "Drag-select visible text; copied automatically on mouse release"}]})

(defn- settings-rows
  []
  (->> settings-sections
    (mapcat (fn [{:keys [id label]}]
              (into [{:type :section
                      :label label}]
                (get settings-options-by-section id []))))
    vec))

(defn- settings-option-label
  [{:keys [key label type choices]} values]
  (case type
    :choice (str label ": " (name (or (get values key) (first choices))))
    label))

(defn- cycle-choice
  [choices current]
  (let [choices (vec choices)
        idx     (.indexOf ^java.util.List choices current)]
    (nth choices (mod (inc (if (neg? idx) 0 idx)) (count choices)))))

(defn- apply-settings-option
  [values {:keys [key type choices]}]
  (case type
    :choice (update values key #(cycle-choice choices %))
    :toggle (update values key not)
    values))

(defn- settings-selectable?
  [{:keys [type]}]
  (contains? #{:toggle :choice :action} type))

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
      (cond
        (= idx selected) idx
        (settings-selectable? (nth rows idx)) idx
        (and (neg? delta) (zero? idx)) selected
        (and (pos? delta) (= idx (dec n))) selected
        :else (recur (clamp (+ idx delta) 0 (max 0 (dec n))))))))

(defn- activate-settings-row!
  [values callbacks row]
  (case (:type row)
    :action (when-let [f (get callbacks (:id row))]
              (f @values))
    (swap! values apply-settings-option row)))

(defn- settings-section-text
  [label inner-w]
  (let [prefix    (str "── " label " ")
        available (max 0 (- inner-w 2))
        filler    (apply str (repeat (max 0 (- available (count prefix))) \─))]
    (ellipsize (str prefix filler) available)))

(defn settings-dialog!
  "Show one settings dialog with category headers for Extensions and UI.

   Toggle rows render `[✓]` / `[ ]`. Choice rows render `[→]` and cycle
   through their allowed values with Space or Enter. Action rows render
   `[↗]` and invoke a callback from `callbacks`.

   `settings` is the persisted TUI settings map (see
   `state/default-settings`). Esc closes the dialog and returns the
   current settings map."
  ([^TerminalScreen screen settings]
   (settings-dialog! screen settings nil))
  ([^TerminalScreen screen settings callbacks]
   (let [rows     (settings-rows)
         n        (count rows)
         selected (atom (first-selectable-index rows))
         values   (atom (or settings {}))
         check-w  4
         gap      "  "
         gap-w    (count gap)]
     (loop []
       (let [size          (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
             cols          (.getColumns size)
             screen-rows   (.getRows size)
             g             (.newTextGraphics screen)
             bounds        (draw-dialog-chrome! g cols screen-rows "Settings" n)
             {:keys [left inner-w]} bounds
             {:keys [content-top content-h hint-row]} (dialog-layout bounds n)
             labels        (mapv #(settings-option-label % @values) rows)
             label-w       (apply max (map count labels))
             actual-desc-w (max 1 (- inner-w 2 check-w label-w gap-w))]

         (dotimes [i n]
           (let [{:keys [key type description]} (nth rows i)
                 row-y      (+ content-top i)
                 label      (nth labels i)
                 selected?  (= i @selected)
                 state-mark (case type
                              :section "    "
                              :action  "[↗] "
                              :choice  "[→] "
                              (if (get @values key true) "[✓] " "[ ] "))
                 label-pad  (str label
                              (apply str (repeat (max 0 (- label-w (count label))) \space)))
                 desc       (or description "")
                 desc-trunc (if (<= (count desc) actual-desc-w)
                              desc
                              (str (subs desc 0 (max 0 (dec actual-desc-w))) "…"))]
             (when (< row-y (+ content-top content-h))
               (if (= :section type)
                 (do
                   (p/set-colors! g t/dialog-border t/dialog-bg)
                   (p/fill-rect! g (inc left) row-y inner-w 1)
                   (p/put-str! g (+ left 2) row-y (settings-section-text label inner-w))
                   (p/set-fg! g t/dialog-hint-key)
                   (p/styled g [p/BOLD]
                     (p/put-str! g (+ left 5) row-y label)))
                 (if selected?
                   (do
                     (p/set-colors! g t/dialog-bg t/dialog-title-bg)
                     (p/fill-rect! g (inc left) row-y inner-w 1)
                     (p/put-str! g (+ left 2) row-y
                       (str state-mark label-pad gap desc-trunc)))
                   (do
                     (p/set-colors! g t/dialog-fg t/dialog-bg)
                     (p/fill-rect! g (inc left) row-y inner-w 1)
                     (p/put-str! g (+ left 2) row-y (str state-mark label-pad))
                     (p/set-colors! g t/dialog-hint t/dialog-bg)
                     (p/put-str! g (+ left 2 check-w label-w gap-w) row-y desc-trunc)))))))

         (draw-hint-bar! g left hint-row inner-w [["↑/↓" "move"] ["Space/Enter" "toggle/select"] ["Esc" "done"]])
         (.setCursorPosition screen (p/cursor-pos 0 0))
         (.refresh screen Screen$RefreshType/DELTA)

         (let [key          (.readInput screen)
               selected-row (nth rows @selected)]
           (when key
             (condp = (.getKeyType key)
               KeyType/Escape @values

               KeyType/ArrowUp
               (do
                 (swap! selected #(move-settings-selection rows % -1))
                 (recur))

               KeyType/ArrowDown
               (do
                 (swap! selected #(move-settings-selection rows % 1))
                 (recur))

               KeyType/Character
               (let [c (.getCharacter key)]
                 (if (= c \space)
                   (do
                     (activate-settings-row! values callbacks selected-row)
                     (recur))
                   (recur)))

               KeyType/Enter
               (do
                 (activate-settings-row! values callbacks selected-row)
                 (recur))

               (recur)))))))))

;;; ── Command palette ─────────────────────────────────────────────────────────

(def palette-commands
  "Command palette entries. Each is {:id keyword :label str}.
   Quit is intentionally NOT here — use Ctrl+C to quit.

   `:providers` opens router/model/auth configuration directly from the
   palette instead of nesting it under Settings.

   `:copy` is the per-message picker (Space toggles, Enter copies
   selected as plain `role: text`). Whole-conversation Markdown copy
   lives in the header as an icon, not in Ctrl+K."
  [{:id :providers :label "Providers"}
   {:id :settings  :label "Settings"}
   {:id :copy      :label "Copy Messages"}])

(defn command-palette!
  "Show a command palette dialog. Returns the :id of the chosen command, or nil on Esc.
   No bespoke padding — `select-dialog!` runs at the shared default modal
   footprint, and `draw-list-item!` already fills the highlight stripe
   across the full inner width regardless of label length."
  [^TerminalScreen screen]
  (let [items (mapv (fn [cmd] {:label (:label cmd)}) palette-commands)]
    (when-let [choice (select-dialog! screen "Commands" items)]
      (:id (nth palette-commands (.indexOf ^java.util.List (mapv :label items) (:label choice)))))))

;;; ── Text viewer dialog ─────────────────────────────────────────────────────────

(defn text-viewer-dialog!
  "Show a scrollable read-only text viewer dialog.
   `title` is the dialog header. `text` is a string (may contain newlines)
   that is rendered VERBATIM — same content the LLM receives, only soft-
   wrapped to fit the dialog width. No markdown, no truncation, no
   reformatting.
   Returns nil on Esc. Supports keyboard scrolling."
  [^TerminalScreen screen title text]
  (let [scroll (atom 0)]
    (loop []
      (let [size    (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols    (.getColumns size)
            rows    (.getRows size)
            g       (.newTextGraphics screen)
            ;; Text viewer is the only dialog that should consume the
            ;; vertical room it can get — it scrolls long content. Ask
            ;; for terminal-bound height so the viewport is generous,
            ;; while still sharing the standard width.
            bounds  (draw-dialog-chrome! g cols rows title (max 12 (- rows 8)))
            {:keys [left inner-w]} bounds
            {:keys [content-top content-h hint-row]} (dialog-layout bounds)
            ;; Reserve the last inner column for a scrollbar that matches
            ;; the chat area's track+thumb style. Text wraps into the
            ;; remaining width so nothing collides with the bar.
            scroll-col (+ left inner-w)
            text-w  (max 1 (- inner-w 3))
            lines   (vec (mapcat #(render/wrap-text % text-w)
                           (str/split-lines (or text "(empty)"))))
            total   (count lines)
            max-scroll (max 0 (- total content-h))
            _       (swap! scroll #(clamp % 0 max-scroll))
            visible (subvec lines @scroll
                      (min total (+ @scroll content-h)))]

        ;; Body — verbatim line render, no ellipsization (wrap-text
        ;; already produced lines that fit `text-w`).
        (p/set-colors! g t/dialog-fg t/dialog-bg)
        (doseq [[i line] (map-indexed vector visible)]
          (let [row (+ content-top i)]
            (when (< row (+ content-top content-h))
              (p/fill-rect! g (inc left) row inner-w 1)
              (p/put-str! g (+ left 2) row line))))
        ;; Clear remaining rows in the content area
        (doseq [row (range (+ content-top (count visible))
                      (+ content-top content-h))]
          (p/set-colors! g t/dialog-fg t/dialog-bg)
          (p/fill-rect! g (inc left) row inner-w 1))

        ;; Scrollbar — same style as the chat messages area: a vertical
        ;; track of │ plus a solid █ thumb sized proportionally to the
        ;; visible window. Drawn over the content's right margin, on the
        ;; dialog background so it visually blends with the dialog frame.
        (when (> total content-h)
          (let [track-h  content-h
                ratio    (/ (double content-h) total)
                thumb-h  (max 1 (int (* track-h ratio)))
                den      (max 1 max-scroll)
                thumb-pos (int (* (- track-h thumb-h) (/ (double @scroll) den)))]
            (doseq [r (range track-h)]
              (p/set-colors! g t/dialog-border t/dialog-bg)
              (p/set-char! g scroll-col (+ content-top r) Symbols/SINGLE_LINE_VERTICAL))
            (doseq [r (range thumb-h)]
              (p/set-colors! g t/dialog-hint-key t/dialog-bg)
              (p/set-char! g scroll-col (+ content-top thumb-pos r) \█))))

        (draw-hint-bar! g left hint-row inner-w
          [["↑/↓" "scroll"] ["PgUp/PgDn" "page"] ["Esc" "close"]])
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (condp = (.getKeyType key)
              KeyType/Escape nil

              KeyType/ArrowUp
              (do (swap! scroll #(max 0 (dec %))) (recur))

              KeyType/ArrowDown
              (do (swap! scroll #(min max-scroll (inc %))) (recur))

              KeyType/PageUp
              (do (swap! scroll #(max 0 (- % content-h))) (recur))

              KeyType/PageDown
              (do (swap! scroll #(min max-scroll (+ % content-h))) (recur))

              KeyType/Character (recur)

              (recur))))))))

;;; ── Copy dialog ─────────────────────────────────────────────────────────────

(defn- role-label [role] (name (or role :assistant)))

(defn- message-preview [{:keys [role text]}]
  (str (role-label role) ": "
    (-> (or text "") (str/replace #"\r?\n+" " ") str/trim)))

(defn- format-selected-messages [messages selected]
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
  (let [items       (vec messages)
        selected    (atom 0)
        scroll      (atom 0)
        checked     (atom #{})
        ch          (count items)]
    (loop [status [["Space" "toggle"] ["A" "all"] ["Enter" "copy"] ["Esc" "cancel"]]]
      (let [size   (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
            cols   (.getColumns size)
            rows   (.getRows size)
            g      (.newTextGraphics screen)
            bounds (draw-dialog-chrome! g cols rows "Copy Messages" ch)
            {:keys [left inner-w]} bounds
            total  (count items)
            {:keys [content-top content-h hint-row]} (dialog-layout bounds total)
            visible (min total content-h)
            _ (swap! selected #(clamp % 0 (max 0 (dec total))))
            _ (swap! scroll #(visible-window-start @selected % content-h total))]

        (dotimes [i visible]
          (let [idx (+ @scroll i)
                row (+ content-top i)]
            (when (< idx total)
              (draw-checkbox-item! g left row inner-w (= idx @selected)
                (contains? @checked idx)
                (message-preview (nth items idx))))))

        (draw-hint-bar! g left hint-row inner-w status)
        (.setCursorPosition screen (p/cursor-pos 0 0))
        (.refresh screen Screen$RefreshType/DELTA)

        (let [key (.readInput screen)]
          (when key
            (let [ktype (.getKeyType key)]
              (condp = ktype
                KeyType/Escape nil

                KeyType/ArrowUp
                (do (swap! selected #(clamp (dec %) 0 (max 0 (dec total))))
                  (recur status))

                KeyType/ArrowDown
                (do (swap! selected #(clamp (inc %) 0 (max 0 (dec total))))
                  (recur status))

                KeyType/Character
                (let [c (Character/toLowerCase (.getCharacter key))]
                  (cond
                    (= c \space)
                    (do (when (pos? total)
                          (swap! checked (fn [s] (if (contains? s @selected)
                                                   (disj s @selected)
                                                   (conj s @selected)))))
                      (recur status))

                    (= c \a)
                    (do (swap! checked (fn [s] (if (= (count s) total) #{} (set (range total)))))
                      (recur status))

                    :else (recur status)))

                KeyType/Enter
                (let [payload (format-selected-messages items @checked)]
                  (if (seq payload)
                    (do (input/clipboard-copy! payload) true)
                    (recur "No messages selected")))

                (recur status)))))))))
