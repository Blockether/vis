(ns com.blockether.vis.ext.channel-tui.human-input
  "TUI rendering for `:human-input/request` — the terminal half of the engine's
   typed human-input pause primitive (`com.blockether.vis.internal.human-input`).

   The engine parks an extension thread and publishes a request VIEW; this
   namespace turns that view into a form the operator can fill in, and hands
   the collected values back through `submit-human-input!` / `cancel-human-input!`.

   Everything except [[paint!]] is PURE: [[init-form]] builds the form model
   from a request view, [[handle-event]] is a reducer over one normalized
   keystroke, and [[form-rows]] is the paint plan. The Lanterna surface only
   shows up in [[key->event]] (decoding) and [[paint!]] (drawing), so the whole
   interaction is testable without a terminal.

   Navigation is a flat list of STOPS — one per text field, one per checkbox,
   and one per select/multiselect OPTION — so ↑/↓/Tab walks the form the way
   the settings dialog walks its rows."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.theme :as t])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

(set! *warn-on-reflection* true)

(def ^:private text-types "Field types edited as free text." #{:plaintext :password :multiline})

(def ^:private choice-types
  "Field types whose stops are individual options."
  #{:select :multiselect})

(def ^:private mask-char "Password fields never render their plaintext." \u2022)

;; =============================================================================
;; Form model
;; =============================================================================

(defn- field-options [field] (vec (:options field)))

(defn default-value
  "The value a field starts with: its declared `:default` coerced to the shape
   the field type submits, else that type's empty value."
  [{:keys [type default] :as field}]
  (cond (contains? text-types type) (if (some? default) (str default) "")
        (= :checkbox type) (boolean default)
        (= :multiselect type) (cond (sequential? default) (vec default)
                                    (some? default) [default]
                                    :else [])
        (= :select type) (if (some? default) default (:value (first (field-options field))))
        :else nil))

(defn stops
  "Flat vector of focus stops for `request` — the linear order ↑/↓/Tab walks."
  [request]
  (into []
        (mapcat (fn [{:keys [id type] :as field}]
                  (cond (contains? text-types type) [{:kind :text :field-id id}]
                        (= :checkbox type) [{:kind :checkbox :field-id id}]
                        (contains? choice-types type)
                        (mapv (fn [{:keys [value]}]
                                {:kind (if (= :multiselect type) :multi-option :select-option)
                                 :field-id id
                                 :value value})
                              (field-options field))
                        :else [])))
        (:fields request)))

(defn init-form
  "Build the form model for a request VIEW. Text cursors start at end-of-text
   and focus starts on the first stop."
  [request]
  (let [values (into {} (map (juxt :id default-value)) (:fields request))]
    {:request request
     :values values
     :cursors (into {}
                    (keep (fn [{:keys [id type]}]
                            (when (contains? text-types type) [id (count (str (get values id)))])))
                    (:fields request))
     :stops (stops request)
     :focus 0
     :errors {}}))

(defn request-id "The engine request id this form answers." [form] (get-in form [:request :id]))

(defn submit-values
  "The values map handed to `submit-human-input!` — keyed by field id."
  [form]
  (:values form))

(defn- field-by-id
  [form field-id]
  (first (filter #(= field-id (:id %)) (get-in form [:request :fields]))))

(defn focused-stop
  "The stop under the cursor, or nil for a form with no stops."
  [{:keys [stops focus]}]
  (get stops (or focus 0)))

(defn- move-focus
  [{:keys [stops] :as form} delta]
  (let [n (count stops)]
    (if (zero? n) form (assoc form :focus (mod (+ (long (:focus form 0)) (long delta)) n)))))

(defn- clamp ^long [^long v ^long lo ^long hi] (max lo (min hi v)))

;; =============================================================================
;; Text editing
;; =============================================================================

(defn- cursor-of
  ^long [form field-id]
  (let [text (str (get-in form [:values field-id]))]
    (clamp (long (get-in form [:cursors field-id] (count text))) 0 (count text))))

(defn- put-text
  [form field-id text cursor]
  (-> form
      (assoc-in [:values field-id] text)
      (assoc-in [:cursors field-id] (clamp (long cursor) 0 (count text)))
      (update :errors dissoc field-id)))

(defn- insert-text
  [form field-id s]
  (let
    [field
     (field-by-id form field-id)

     text
     (str (get-in form [:values field-id]))

     cursor
     (cursor-of form field-id)

     max-length
     (:max-length field)

     room
     (if max-length (max 0 (- (long max-length) (count text))) (count s))

     s
     (subs s 0 (min (count s) room))]

    (if (zero? (count s))
      form
      (put-text form
                field-id
                (str (subs text 0 cursor) s (subs text cursor))
                (+ cursor (count s))))))

(defn- delete-back
  [form field-id]
  (let
    [text
     (str (get-in form [:values field-id]))

     cursor
     (cursor-of form field-id)]

    (if (zero? cursor)
      form
      (put-text form field-id (str (subs text 0 (dec cursor)) (subs text cursor)) (dec cursor)))))

(defn- delete-forward
  [form field-id]
  (let
    [text
     (str (get-in form [:values field-id]))

     cursor
     (cursor-of form field-id)]

    (if (>= cursor (count text))
      form
      (put-text form field-id (str (subs text 0 cursor) (subs text (inc cursor))) cursor))))

(defn- move-cursor
  [form field-id delta]
  (let [text (str (get-in form [:values field-id]))]
    (assoc-in form
      [:cursors field-id]
      (clamp (+ (cursor-of form field-id) (long delta)) 0 (count text)))))

;; =============================================================================
;; Toggling
;; =============================================================================

(defn- toggle-stop
  [form {:keys [kind field-id value]}]
  (case kind
    :checkbox
    (-> form
        (update-in [:values field-id] not)
        (update :errors dissoc field-id))

    :select-option
    (-> form
        (assoc-in [:values field-id] value)
        (update :errors dissoc field-id))

    :multi-option
    (let
      [order
       (mapv :value (field-options (field-by-id form field-id)))

       chosen
       (set (get-in form [:values field-id]))

       chosen
       (if (contains? chosen value) (disj chosen value) (conj chosen value))]

      (-> form
          (assoc-in [:values field-id] (filterv chosen order))
          (update :errors dissoc field-id)))

    form))

;; =============================================================================
;; Key handling
;; =============================================================================

(defn key->event
  "Normalize one Lanterna keystroke into the event map [[handle-event]] takes,
   or nil when the keystroke means nothing to a form (mouse, unknown chords)."
  [^KeyStroke key]
  (when key
    (let [kt (.getKeyType key)]
      (condp = kt
        KeyType/Escape {:kind :cancel}
        KeyType/Enter (if (or (.isAltDown key) (.isCtrlDown key)) {:kind :submit} {:kind :enter})
        KeyType/Tab {:kind :next}
        KeyType/ReverseTab {:kind :prev}
        KeyType/ArrowDown {:kind :next}
        KeyType/ArrowUp {:kind :prev}
        KeyType/ArrowLeft {:kind :left}
        KeyType/ArrowRight {:kind :right}
        KeyType/Home {:kind :home}
        KeyType/End {:kind :end}
        KeyType/Backspace {:kind :backspace}
        KeyType/Delete {:kind :delete}
        KeyType/Character (let [c (.getCharacter key)]
                            (cond (and (.isCtrlDown key) (contains? #{\s \S} c)) {:kind :submit}
                                  (.isCtrlDown key) nil
                                  (nil? c) nil
                                  (Character/isISOControl (char c)) nil
                                  :else {:kind :char :char (char c)}))
        nil))))

(defn- multiline-focus?
  [form]
  (let [stop (focused-stop form)]
    (and (= :text (:kind stop)) (= :multiline (:type (field-by-id form (:field-id stop)))))))

(defn handle-event
  "PURE reducer: apply ONE normalized event to `form`.

   Returns `{:form form' :action action}` where `action` is nil (stay open),
   `:submit` (ask the engine to accept `submit-values`), or `:cancel`."
  [form {:keys [kind char] :as _event}]
  (let
    [stop
     (focused-stop form)

     text-stop?
     (= :text (:kind stop))

     field-id
     (:field-id stop)

     stay
     (fn [f]
       {:form f :action nil})]

    (case kind
      :cancel
      (if (get-in form [:request :is-cancellable] true) {:form form :action :cancel} (stay form))

      :submit
      {:form form :action :submit}

      :enter
      (cond (multiline-focus? form) (stay (insert-text form field-id "\n"))
            ;; Enter on an option/checkbox both picks it AND is the
            ;; natural "I'm done" key — toggling first would make a
            ;; single-option form impossible to accept, so options
            ;; toggle with Space and Enter always submits.
            :else {:form form :action :submit})

      :next
      (stay (move-focus form 1))

      :prev
      (stay (move-focus form -1))

      :left
      (stay (if text-stop? (move-cursor form field-id -1) form))

      :right
      (stay (if text-stop? (move-cursor form field-id 1) form))

      :home
      (stay (if text-stop? (assoc-in form [:cursors field-id] 0) form))

      :end
      (stay (if text-stop?
              (assoc-in form [:cursors field-id] (count (str (get-in form [:values field-id]))))
              form))

      :backspace
      (stay (if text-stop? (delete-back form field-id) form))

      :delete
      (stay (if text-stop? (delete-forward form field-id) form))

      :char
      (cond (nil? stop) (stay form)
            text-stop? (stay (insert-text form field-id (str char)))
            (= \space char) (stay (toggle-stop form stop))
            :else (stay form))

      (stay form))))

(defn set-errors
  "Attach the engine's per-field rejection messages and move focus to the first
   offending field so the operator lands on what needs fixing."
  [form errors]
  (let
    [errors
     (or errors {})

     bad
     (set (keys errors))

     idx
     (first (keep-indexed (fn [i {:keys [field-id]}]
                            (when (contains? bad field-id) i))
                          (:stops form)))]

    (cond-> (assoc form :errors errors)
      idx
      (assoc :focus idx))))

;; =============================================================================
;; Paint plan
;; =============================================================================

(defn- cursor-line-col
  "`[line col]` of `cursor` inside `text`."
  [text ^long cursor]
  (let
    [before
     (subs (str text) 0 (clamp cursor 0 (count (str text))))

     lines
     (str/split before #"\n" -1)]

    [(dec (count lines)) (count (last lines))]))

(defn- display-text
  [{:keys [type]} value]
  (if (= :password type) (apply str (repeat (count (str value)) mask-char)) (str value)))

(def ^:private required-marker
  "Said in full, next to the label. A lone `*` is a footnote nobody reads, and the
   engine REFUSES a submission that leaves one of these blank — so the dialog has
   to name the fields that will bounce it before the operator hits enter."
  "REQUIRED")

(defn- label-text
  [{:keys [label is-required]}]
  (str label (when is-required (str "  " required-marker))))

(defn- description-rows
  "`text` as `:description` rows, WORD-WRAPPED to `text-w` columns.

   Prose is the one thing in this dialog that is a sentence, not a token: the
   request's own description says what the whole ask is about, and a field's
   explains that field. Clipping it to a single `…` row loses exactly the part
   that was worth reading, so it wraps onto as many rows as it needs. `text-w`
   nil means \"do not wrap\" — the pure plan a caller measures without a
   terminal."
  [text text-w]
  (let
    [text
     (some-> text
             str
             not-empty)

     width
     (long (or text-w 0))]

    (when text
      (mapv (fn [line]
              {:kind :description :text line})
            (if (pos? width) (render/wrap-text text width) [text])))))

(defn- field-rows
  [form focus text-w {:keys [id type] :as field}]
  (let
    [stop-index
     (fn [pred]
       (first (keep-indexed (fn [i s]
                              (when (pred s) i))
                            (:stops form))))

     value
     (get-in form [:values id])

     description
     (description-rows (:description field) text-w)

     rows
     (cond (contains? text-types type)
           (let
             [idx
              (stop-index #(= id (:field-id %)))

              focused?
              (= idx focus)

              shown
              (display-text field value)

              cursor
              (cursor-of form id)]

             (if (= :multiline type)
               (let [[line col] (cursor-line-col shown cursor)]
                 (vec (map-indexed (fn [i l]
                                     {:kind :input
                                      :field-id id
                                      :text l
                                      :cursor (if (= i line) col 0)
                                      :is-focused (and focused? (= i line))
                                      :placeholder (when (and (zero? i) (zero? (count shown)))
                                                     (:placeholder field))})
                                   (str/split shown #"\n" -1))))
               [{:kind :input
                 :field-id id
                 :text shown
                 :cursor cursor
                 :is-focused focused?
                 :placeholder (:placeholder field)}]))
           (= :checkbox type) (let [idx (stop-index #(= id (:field-id %)))]
                                [{:kind :checkbox
                                  :field-id id
                                  :text (label-text field)
                                  :is-checked (boolean value)
                                  :is-focused (= idx focus)}])
           (contains? choice-types type)
           (let [chosen (if (= :multiselect type) (set value) #{value})]
             (mapv (fn [{:keys [value label]}]
                     (let [idx (stop-index #(and (= id (:field-id %)) (= value (:value %))))]
                       {:kind :option
                        :field-id id
                        :text (or label (str value))
                        :is-checked (contains? chosen value)
                        :is-focused (= idx focus)}))
                   (field-options field)))
           :else [])]

    ;; Label, then description, then the input: the italic prose explains the
    ;; field you are about to fill, so it has to be readable BEFORE it, not
    ;; discovered underneath it.
    ;;
    ;; A checkbox row already carries its own label — a separate bold label row
    ;; above it would say the same word twice, which no other dialog does — so
    ;; there its description follows the box instead.
    (cond->
      (if (= :checkbox type)
        (into (vec rows) description)
        (into (into [{:kind :label :text (label-text field)}] description) rows))
      (get-in form [:errors id])
      (conj {:kind :error :text (get-in form [:errors id])})

      true
      (conj {:kind :blank}))))

(defn form-rows
  "PURE paint plan: the ordered rows the dialog body draws for `form`.

   `text-w` is the column budget prose gets — the request's description and
   every field's wrap to it. Omit it (or pass nil) for the unwrapped plan."
  ([form] (form-rows form nil))
  ([{:keys [request focus] :as form} text-w]
   (let
     [head (when-let [rows (seq (description-rows (:description request) text-w))]
             (conj (vec rows) {:kind :blank}))]
     (into (vec head) (mapcat #(field-rows form (or focus 0) text-w %)) (:fields request)))))

(defn focused-row
  "Index of the row carrying the focused stop, or 0."
  [rows]
  (or (first (keep-indexed (fn [i r]
                             (when (:is-focused r) i))
                           rows))
      0))

(defn window-start
  "First plan row to draw so the focused field stays usable: the minimal scroll
   that keeps the focused row on screen, pulled up to that field's LABEL row when
   the window is too short to hold both — a bare unlabelled input row would not
   say which field is being edited."
  ^long [rows ^long visible]
  (if (empty? rows)
    0
    (let
      [focus-idx
       (long (focused-row rows))

       label-idx
       (long (or (first (filter #(= :label (:kind (nth rows %))) (range focus-idx -1 -1))) 0))

       start
       (long (dialogs/visible-window-start focus-idx 0 visible (count rows)))]

      (min start label-idx))))

(defn hint
  "Hint-bar pairs for `form` — the chord list changes with the focused field.
   Spelled the canonical dialog way: `↑/↓` chords and lowercase actions."
  [form]
  (let
    [stop
     (focused-stop form)

     multi?
     (multiline-focus? form)

     action
     (fn [label fallback]
       (str/lower-case (or (not-empty (str label)) fallback)))]

    (cond-> [["↑/↓" "move"]]
      (contains? #{:checkbox :select-option :multi-option} (:kind stop))
      (conj ["Space" "toggle"])

      multi?
      (conj ["Enter" "newline"])

      true
      (conj [(if multi? "^S" "Enter") (action (get-in form [:request :submit-label]) "submit")])

      (get-in form [:request :is-cancellable] true)
      (conj ["Esc" (action (get-in form [:request :cancel-label]) "cancel")]))))

;; =============================================================================
;; Painting
;; =============================================================================

(defn- paint-plain!
  [g left row inner-w fg text]
  (p/set-colors! g fg t/dialog-bg)
  (p/fill-rect! g (inc (long left)) row inner-w 1)
  (p/put-str! g (inc (long left)) row (dialogs/ellipsize (str text) (max 0 (- (long inner-w) 2)))))

(defn- paint-italic!
  "Prose rows — the request's description and each field's — paint in the same
   dim ITALIC voice the rest of the TUI uses for explanatory text, so a
   description can never be mistaken for a label or a value."
  [g left row inner-w fg text]
  (p/set-colors! g fg t/dialog-bg)
  (p/fill-rect! g (inc (long left)) row inner-w 1)
  (p/styled g
            [p/ITALIC]
            (p/put-str! g
                        (inc (long left))
                        row
                        (dialogs/ellipsize (str text) (max 0 (- (long inner-w) 2))))))



(defn- paint-row!
  "Paint one plan row. Selectable rows go through the SHARED dialog painters
   (`dialogs/draw-checkbox-item!`, `dialogs/draw-radio-item!`,
   `dialogs/draw-text-input-field!`) so this dialog cannot drift from the rest
   of the TUI. Returns the terminal cursor position when the row owns it."
  [g left row inner-w entry]
  (case (:kind entry)
    :blank
    (do (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc (long left)) row inner-w 1)
        nil)

    :description
    (do (paint-italic! g left row inner-w t/dialog-hint (:text entry)) nil)

    :label
    (do (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc (long left)) row inner-w 1)
        (p/styled g
                  [p/BOLD]
                  (p/put-str! g
                              (inc (long left))
                              row
                              (dialogs/ellipsize (str (:text entry)) (max 0 (- (long inner-w) 2)))))
        nil)

    :error
    (do (paint-plain! g left row inner-w t/footer-error-fg (:text entry)) nil)

    :checkbox
    (do (dialogs/draw-checkbox-item! g
                                     left
                                     row
                                     inner-w
                                     (:is-focused entry)
                                     (:is-checked entry)
                                     (:text entry))
        nil)

    :option
    (do (dialogs/draw-radio-item! g
                                  left
                                  row
                                  inner-w
                                  (:is-focused entry)
                                  (:is-checked entry)
                                  (:text entry))
        nil)

    :input
    (let
      [text
       (str (:text entry))

       cursor
       (clamp (long (or (:cursor entry) 0)) 0 (count text))

       pos
       (dialogs/draw-text-input-field! g left row inner-w text cursor (:placeholder entry))]

      (when (:is-focused entry) pos))

    nil))

(defn paint!
  "Draw the human-input dialog for `form` over the whole screen. Returns the
   `TerminalPosition` the caller should place the terminal cursor at (the
   focused text field), or nil when no text field has focus.

   Sized and decorated like every other dialog: shared chrome, a box wide
   enough for the whole hint bar, and a scrollbar in the right gutter once the
   form is taller than the content area."
  [g ^long cols ^long rows form]
  (let
    [bar
     (hint form)

     content-w
     (dialogs/footer-content-width cols bar (dialogs/default-content-width cols))

     plan
     ;; Wrap prose to what the narrowest painted row can hold: the painter
     ;; insets every row by 2 columns and gives one more to the scrollbar lane
     ;; once the form overflows, so a wrapped line still fits after the plan
     ;; itself decided the box is scrolling.
     (form-rows form (max 1 (- content-w 3)))

     content-h
     (dialogs/adaptive-content-height rows (count plan))

     bounds
     (dialogs/draw-dialog-chrome! g cols rows (get-in form [:request :title]) content-w content-h)

     layout
     (dialogs/dialog-layout bounds)

     left
     (long (:left bounds))

     inner-w
     (long (:inner-w bounds))

     content-top
     (long (:content-top layout))

     visible
     (max 1 (long (:content-h layout)))

     total
     (count plan)

     start
     (window-start plan visible)

     is-overflowing
     (> total visible)

     row-w
     (if is-overflowing (dec inner-w) inner-w)

     shown
     (subvec (vec plan) (min start total) (min total (+ start visible)))

     cursor
     (reduce (fn [acc [i entry]]
               (or (paint-row! g left (+ content-top (long i)) row-w entry) acc))
             nil
             (map-indexed vector shown))]

    (doseq [i (range (count shown) visible)]
      (paint-row! g left (+ content-top (long i)) row-w {:kind :blank}))
    (when is-overflowing
      (scrollbar/draw! g
                       {:col (+ left inner-w)
                        :top content-top
                        :track-h visible
                        :total-h total
                        :inner-h visible
                        :scroll start}))
    (dialogs/draw-hint-bar! g left (:hint-row layout) inner-w bar)
    (p/clear-styles! g)
    cursor))
