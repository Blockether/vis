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
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.channel-tui.transient :as tr]
            [com.blockether.vis.internal.human-input :as engine])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

(set! *warn-on-reflection* true)

(def ^:private text-types
  "Field types edited as free text — `:otp` among them: it is typed, erased and
   walked exactly like a text field, it only LOOKS like a row of boxes."
  #{:plaintext :password :multiline :otp})

(def ^:private choice-types
  "Field types whose stops are individual options."
  #{:select :multiselect})

(def ^:private range-defaults
  "Bounds a `:range` field falls back to — the engine's own defaults, so a
   hand-made request view renders the same slider a normalized one does."
  {:min 0 :max 100 :step 1})

(defn- range-bounds
  "`{:lo :hi :st}` for a `:range` field. Named away from `min`/`max` on purpose:
   destructuring those would shadow the two core fns this file rounds with."
  [field]
  (let
    [num (fn [k]
           (let [v (get field k)]
             (if (number? v) v (get range-defaults k))))]
    {:lo (num :min) :hi (num :max) :st (num :step)}))

(defn- range-integral? [{:keys [lo hi st]}] (every? integer? [lo hi st]))

(defn- range-snap
  "`v` pulled inside the bounds and given the type the field submits — a long
   for an all-integer slider, so ↑/↓ can never produce `42.0000001`."
  [{:keys [lo hi] :as bounds} v]
  (let [v (max (double lo) (min (double hi) (double v)))]
    (if (range-integral? bounds) (long (Math/round v)) v)))

(def ^:private range-track-w
  "Cells the slider track gets. Fixed, so the knob does not jump between two
   fields whose bounds differ and the number beside it stays where the eye is."
  20)

(defn- range-text
  "`━━━━●─────────  42  (0–100)` — track, knob, the value, and the bounds. A bar
   alone never says WHAT is about to be submitted, and a number alone never says
   how much room is left."
  [{:keys [lo hi]} v]
  (let
    [span
     (- (double hi) (double lo))

     frac
     (if (pos? span) (/ (- (double v) (double lo)) span) 0.0)

     w
     (long range-track-w)

     knob
     (max 0 (min (dec w) (long (Math/round (* frac (dec w))))))]

    (str (apply str (repeat knob \━))
         "●"
         (apply str (repeat (- (dec w) knob) \─))
         "  "
         v
         "  ("
         lo
         "–"
         hi
         ")")))

(def ^:private otp-defaults
  "Digits an `:otp` field falls back to — the engine's own default, so a view
   that somehow arrives without bounds still draws the six boxes everyone
   expects from a one-time code."
  {:min-length 6 :max-length 6})

(defn- otp-slots
  "`{:lo :hi}` — the fewest and the most digits this `:otp` field accepts."
  [field]
  (let
    [hi
     (max 1 (long (or (:max-length field) (:max-length otp-defaults))))

     lo
     (max 1 (min hi (long (or (:min-length field) (:min-length otp-defaults)))))]

    {:lo lo :hi hi}))

(def ^:private otp-cell-w "`[7] ` — one box, its digit, and the gap to the next box." 4)

(defn- otp-text
  "`[1] [2] [3] [ ] [ ] [ ]` — one box per digit the field takes, filled left to
   right. A field that accepts a RANGE of lengths says so after the boxes: eight
   empty boxes cannot show that six of them are already enough."
  [{:keys [lo hi]} value]
  (let [digits (str value)]
    (str (str/join " "
                   (map (fn [i]
                          (str "[" (if (< (long i) (count digits)) (nth digits i) \space) "]"))
                        (range hi)))
         (when (not= (long lo) (long hi)) (str "  (" lo "–" hi " digits)")))))

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
        (= :range type) (let [{:keys [lo] :as bounds} (range-bounds field)]
                          (range-snap bounds (if (number? default) default lo)))
        :else nil))

(defn- field-stops
  [{:keys [id type] :as field}]
  (cond (contains? text-types type) [{:kind :text :field-id id}]
        (= :checkbox type) [{:kind :checkbox :field-id id}]
        (= :range type) [{:kind :range :field-id id}]
        (contains? choice-types type)
        (mapv (fn [{:keys [value]}]
                {:kind (if (= :multiselect type) :multi-option :select-option)
                 :field-id id
                 :value value})
              (field-options field))
        :else []))

(defn action-stops
  "The request's own confirm/cancel buttons, as focus stops of their own.

   A transient that can only be accepted by a chord is a transient nobody
   accepts: these are the LAST stops, so ↓ walks off the final field straight
   onto `[ Submit ]` and Enter presses it. Cancel only exists when the request
   allows it — an uncancellable ask must not offer a button that does nothing."
  [request]
  (let
    [labelled (fn [k fallback]
                (or (not-empty (str/trim (str (get request k)))) fallback))]
    (cond-> [{:kind :action :action :submit :label (labelled :submit-label "Submit")}]
      (get request :is-cancellable true)
      (conj {:kind :action :action :cancel :label (labelled :cancel-label "Cancel")}))))

(defn stops
  "Flat vector of focus stops for `request` — the linear order ↑/↓/Tab walks."
  [request]
  (into (into [] (mapcat field-stops) (:fields request)) (action-stops request)))

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
     ;; Pristine: nothing has been touched, nothing has been submitted, so
     ;; nothing is allowed to be red yet.
     :touched #{}
     :is-submit-attempted false
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
  (let
    [n
     (count stops)

     left
     (:field-id (focused-stop form))]

    (if (zero? n)
      form
      ;; Leaving a field is what BLURS it, and a blurred field has earned the
      ;; right to complain — see [[visible-errors]].
      (cond-> (assoc form :focus (mod (+ (long (:focus form 0)) (long delta)) n))
        left
        (update :touched (fnil conj #{}) left)))))

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

     s
     ;; An `:otp` field takes DIGITS, and that filter IS the paste handler:
     ;; pasting `123-456`, or a whole SMS line, fills the boxes with the code
     ;; that was in it instead of refusing the paste.
     (if (= :otp (:type field)) (str/replace (str s) #"\D" "") (str s))

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

(defn- nudge-range
  "Move a slider one `:step` — the ←/→ the operator expects on a track, clamped
   to the bounds instead of wrapping, because a volume knob that jumps from max
   to min is a bug in every UI that ever shipped one."
  [form field-id ^long delta]
  (let
    [bounds
     (range-bounds (field-by-id form field-id))

     current
     (let [v (get-in form [:values field-id])]
       (if (number? v) v (:lo bounds)))]

    (-> form
        (assoc-in [:values field-id]
                  (range-snap bounds (+ (double current) (* delta (double (:st bounds))))))
        (update :errors dissoc field-id))))

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

;; =============================================================================
;; Validation
;; =============================================================================

(defn live-errors
  "Every complaint the ENGINE has about the values `form` holds right now, as
   `{field-id message}`. Computed HERE — the same coercion and the same rules,
   with no round trip — and PURE, so the painter may ask on every keystroke."
  [form]
  (let
    [{:keys [is-accepted errors]} (engine/validate-values (get-in form [:request :fields])
                                                          (:values form))]
    (if is-accepted {} (or errors {}))))

(defn visible-errors
  "The complaints the operator is actually SHOWN — Formik's rule, for Formik's
   reason: a PRISTINE field never nags. A field speaks up once it has been left
   (touched, i.e. blurred) or once a submit has been attempted; a message the
   engine itself sent back always shows."
  [form]
  (let
    [shown? (fn [field-id]
              (or (boolean (:is-submit-attempted form)) (contains? (:touched form) field-id)))]
    (merge (into {} (filter (comp shown? key)) (live-errors form)) (:errors form))))

(defn- refuse-submit
  "A refused submission: every field may now complain, and the cursor lands on
   the first one that does."
  [form errors]
  (let
    [bad
     (set (keys errors))

     idx
     (first (keep-indexed (fn [i {:keys [field-id]}]
                            (when (contains? bad field-id) i))
                          (:stops form)))]

    (cond-> (assoc form :is-submit-attempted true)
      idx
      (assoc :focus idx))))

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

     range-stop?
     (= :range (:kind stop))

     button
     (when (= :action (:kind stop)) (:action stop))

     field-id
     (:field-id stop)

     stay
     (fn [f]
       {:form f :action nil})

     ;; ←/→ mean three different things depending on what the cursor is on, and
     ;; all three are the web habit: move the caret in text, slide a track, and
     ;; nothing at all on a row that has no horizontal axis.
     horizontal
     (fn [delta]
       (stay (cond text-stop? (move-cursor form field-id delta)
                   range-stop? (nudge-range form field-id delta)
                   :else form)))

     submit
     ;; The form refuses ITSELF before the engine is asked: every rule the view
     ;; carries runs against the values as they stand, and a failure only turns
     ;; the form red and parks the cursor on the first complaint.
     (fn []
       (let [errors (live-errors form)]
         (if (seq errors) (stay (refuse-submit form errors)) {:form form :action :submit})))

     press
     (fn []
       (if (= :cancel button)
         (if (get-in form [:request :is-cancellable] true) {:form form :action :cancel} (stay form))
         (submit)))]

    (case kind
      :cancel
      (if (get-in form [:request :is-cancellable] true) {:form form :action :cancel} (stay form))

      :submit
      (submit)

      :enter
      (cond (multiline-focus? form) (stay (insert-text form field-id "\n"))
            ;; Enter on an option/checkbox both picks it AND is the
            ;; natural "I'm done" key — toggling first would make a
            ;; single-option form impossible to accept, so options
            ;; toggle with Space and Enter always submits. On a BUTTON
            ;; it presses that button, which is the only way `[ Cancel ]`
            ;; is reachable without knowing Esc.
            :else (press))

      :next
      (stay (move-focus form 1))

      :prev
      (stay (move-focus form -1))

      :left
      (horizontal -1)

      :right
      (horizontal 1)

      :home
      (stay (cond text-stop? (assoc-in form [:cursors field-id] 0)
                  range-stop? (nudge-range form field-id Long/MIN_VALUE)
                  :else form))

      :end
      (stay (cond text-stop?
                  (assoc-in form [:cursors field-id] (count (str (get-in form [:values field-id]))))
                  range-stop? (nudge-range form field-id Long/MAX_VALUE)
                  :else form))

      :backspace
      (stay (if text-stop? (delete-back form field-id) form))

      :delete
      (stay (if text-stop? (delete-forward form field-id) form))

      :char
      (cond (nil? stop) (stay form)
            text-stop? (stay (insert-text form field-id (str char)))
            (not= \space char) (stay form)
            button (press)
            :else (stay (toggle-stop form stop)))

      (stay form))))

(defn set-errors
  "Attach the engine's per-field rejection messages and move focus to the first
   offending field so the operator lands on what needs fixing."
  [form errors]
  (let [errors (or errors {})]
    (assoc (refuse-submit form errors) :errors errors)))

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
   terminal.

   Whitespace is not prose: a blank description is NO rows, never an empty one,
   so `description: \"   \"` cannot open a hole above the first field."
  [text text-w]
  (let
    [text
     (when-not (str/blank? (some-> text
                                   str))
       (str text))

     width
     (long (or text-w 0))]

    (when text
      (mapv (fn [line]
              {:kind :description :text line})
            (if (pos? width) (render/wrap-text text width) [text])))))

(defn- field-rows
  [form errors focus text-w {:keys [id type] :as field}]
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
     (cond (= :otp type) (let
                           [idx
                            (stop-index #(= id (:field-id %)))

                            slots
                            (otp-slots field)]

                           [{:kind :otp
                             :field-id id
                             :text (otp-text slots value)
                             :cursor (min (cursor-of form id) (max 0 (dec (long (:hi slots)))))
                             :is-focused (= idx focus)}])
           (contains? text-types type)
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
           (= :range type) (let
                             [idx
                              (stop-index #(= id (:field-id %)))

                              bounds
                              (range-bounds field)

                              v
                              (range-snap bounds (if (number? value) value (:lo bounds)))]

                             [{:kind :range
                               :field-id id
                               :value v
                               :text (range-text bounds v)
                               :is-focused (= idx focus)}])
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
      (get errors id)
      (conj {:kind :error :text (get errors id)})

      true
      (conj {:kind :blank}))))

(defn action-bar
  "The request's own buttons as ONE row: `[ Submit ]  [ Cancel ]`.

   PINNED by the painter under the scrolling body instead of trailing it, so a
   form taller than the band can never push the two controls that END the pause
   off the screen — the same reason the companion pins them in its footer."
  [{:keys [stops focus]}]
  (let
    [buttons (into []
                   (keep-indexed (fn [i {:keys [kind action label]}]
                                   (when (= :action kind)
                                     {:action action :label label :is-focused (= i (or focus 0))})))
                   stops)]
    (when (seq buttons) {:kind :action :buttons buttons})))

(defn form-rows
  "PURE paint plan: the ordered rows the dialog body draws for `form`.

   Fields only — the confirm/cancel buttons live in [[action-bar]], pinned below
   this body, so they are never scrolled away.

   `text-w` is the column budget prose gets — the request's description and
   every field's wrap to it. Omit it (or pass nil) for the unwrapped plan."
  ([form] (form-rows form nil))
  ([{:keys [request focus] :as form} text-w]
   (let
     [head
      (when-let [rows (seq (description-rows (:description request) text-w))]
        (conj (vec rows) {:kind :blank}))

      errors
      (visible-errors form)]

     (into (vec head) (mapcat #(field-rows form errors (or focus 0) text-w %)) (:fields request)))))

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

     otp?
     (= :otp (:type (field-by-id form (:field-id stop))))

     action
     (fn [label fallback]
       (str/lower-case (or (not-empty (str label)) fallback)))]

    (cond-> [["↑/↓" "move"]]
      (contains? #{:checkbox :select-option :multi-option} (:kind stop))
      (conj ["Space" "toggle"])

      otp?
      (conj ["0–9" "fill"])

      (= :range (:kind stop))
      (conj ["←/→" "adjust"])

      multi?
      (conj ["Enter" "newline"])

      true
      (conj [(if multi? "^S" "Enter")
             (if (= :action (:kind stop))
               "press"
               (action (get-in form [:request :submit-label]) "submit"))])

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


(defn- paint-actions!
  "The PINNED action bar: every button the request offers on ONE row, the
   focused one bold behind the shared cursor glyph.

   Pinned rather than scrolled with the fields, because the two controls that
   END the pause are exactly the ones a long form must never push out of view."
  [g left row inner-w buttons]
  (let
    [left
     (long left)

     inner-w
     (long inner-w)

     right
     (+ left inner-w -1)]

    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) row inner-w 1)
    (reduce (fn [^long col {:keys [label is-focused]}]
              (let
                [text
                 (str (p/selection-prefix is-focused) "[ " label " ]")

                 w
                 (long (p/display-width text))]

                (when (<= (+ col w) right)
                  (if is-focused
                    (p/styled g [p/BOLD] (p/put-str! g col row text))
                    (p/put-str! g col row text)))
                (+ col w 1)))
            (inc left)
            buttons)
    nil))


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

    :range
    (do (dialogs/draw-selectable-row! g left row inner-w (:is-focused entry) (:text entry)) nil)

    ;; The boxes are a selectable row like any other; what is special is that the
    ;; TERMINAL cursor is parked inside the active box, so the operator sees
    ;; where the next digit lands instead of guessing.
    :otp
    (do (dialogs/draw-selectable-row! g left row inner-w (:is-focused entry) (:text entry))
        (when (:is-focused entry)
          (p/cursor-pos (min (+ (long left) (long inner-w))
                             (+ (long left)
                                2
                                (long p/SELECTION_WIDTH)
                                (* (long otp-cell-w) (long (:cursor entry 0)))))
                        row)))

    :action
    (do (paint-actions! g left row inner-w (:buttons entry)) nil)

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

(defn- prose-width
  "Columns a plan row's TEXT actually gets out of a `row-w`-wide row.

   `paint-row!` starts every row one column inside the frame and `ellipsize`s
   it two columns short of `row-w`, so prose wrapped any wider than this is
   wrapped to a width the painter then CLIPS — and a hard-broken token (a URL,
   a path) silently loses the characters that fall past the cut."
  ^long [^long row-w]
  (max 1 (- row-w 2)))

(def ^:private band-pad
  "Columns of empty space on each end of the band's rules — the SAME inset
   `render/draw-input-box!` gives the prompt's own top and bottom rules, so the
   band lines up with the chrome it takes over instead of floating beside it."
  2)

(defn band-region
  "PURE: the rectangle the in-session band paints into on a `cols`×`rows`
   terminal whose transcript starts at `content-top`.

   The session frame is SIDELESS — the prompt is two horizontal rules with no
   `│` rails — so the band borrows exactly that: rules inset [[band-pad]]
   columns and text one column further in. `:hint-row` is the prompt box's own
   closing rule (always `rows - 3`, whatever height the editor grew to), which
   is what keeps the echo area's two footer rows below the band alive.
   `:min-row` is the floor: however tall the form, the header and the top of
   the transcript stay on screen."
  [^long cols ^long rows ^long content-top]
  (let
    [pad
     (long band-pad)

     min-row
     (max 0 content-top)]

    {:left (dec pad)
     :inner-w (max 4 (- cols (* 2 pad)))
     :hint-row (max (+ min-row 3) (- rows 3))
     :min-row min-row}))

(defn- clear-band-row!
  "Blank one band row across the FULL terminal width. The band sits ON the live
   transcript, not on a modal's own paper: anything it does not repaint would
   show through between its rules."
  [g ^long cols ^long row]
  (p/set-colors! g t/dialog-fg t/dialog-bg)
  (p/fill-rect! g 0 row cols 1))

(defn- draw-rule!
  "One of the band's horizontal rules, inset to the prompt's own columns.
   Sideless, so no `├`/`┤` junctions: there are no rails for them to join."
  [g ^long left ^long inner-w ^long row]
  (p/set-colors! g t/border-fg t/dialog-bg)
  (p/put-str! g (inc left) row (p/horiz-line inner-w)))

(defn paint!
  "Draw the human-input band for `form` INSIDE the session's own frame. Returns
   the `TerminalPosition` the caller should place the terminal cursor at (the
   focused text field), or nil when no text field has focus.

   A magit-style TRANSIENT, not a modal. The band is bottom-anchored on the
   session's bottom chrome — it takes over the prompt's rows and grows UPWARD
   over the transcript, never past `content-top` — and reads `───` / bold title /
   `───` / the fields / the action bar / `───` / hint bar, the same chrome every
   other transient in the TUI wears. The rule directly above the hint bar is the
   host's closing rule, so the footer below the band is never swallowed.

   The action bar is PINNED: only the fields scroll under it, so `[ Submit ]`
   and `[ Cancel ]` stay on screen for a form of any length.

   TWO passes over the plan, because a scrollbar costs a column: the first plan
   sizes the band, and an overflowing one re-wraps one column narrower."
  ([g cols rows form] (paint! g cols rows form 1))
  ;; NOTE: no primitive hints on this arity — Clojure caps primitive-taking fns
  ;; at four arguments — so the sizes are coerced inside the `let` instead.
  ([g cols rows form content-top]
   (let
     [cols
      (long cols)

      {:keys [left inner-w] :as region}
      (band-region cols (long rows) (long content-top))

      left
      (long left)

      inner-w
      (long inner-w)

      bar
      (hint form)

      actions
      (action-bar form)

      draft
      ;; Sizing pass: how many rows the form wants decides where the band's
      ;; top rule lands. The pinned action bar asks for one row of its own.
      (form-rows form (prose-width inner-w))

      {:keys [sep-row title-row title-rule-row body-top foot-rule-row foot-row visible top-limit]}
      (tr/band-geometry region (+ (count draft) (if actions 1 0)))

      visible
      (long visible)

      body-visible
      (max 0 (- visible (if actions 1 0)))

      is-overflowing
      (> (count draft) body-visible)

      row-w
      (if is-overflowing (dec inner-w) inner-w)

      plan
      ;; A scrollbar eats one column, so overflowing prose re-wraps one
      ;; narrower — still overflowing, so this settles in one step.
      (if is-overflowing (form-rows form (prose-width row-w)) draft)

      total
      (count plan)

      start
      (long (if (= :action (:kind (focused-stop form)))
              ;; Focus is on a pinned button: the fields stay where the operator
              ;; left them — at the end — instead of snapping back to row 0.
              (max 0 (- total body-visible))
              (window-start plan body-visible)))

      shown
      (subvec (vec plan) (min start total) (min total (+ start body-visible)))]

     (doseq [row (range (max 0 (long sep-row)) (inc (long foot-row)))]
       (clear-band-row! g cols row))
     (when (>= (long sep-row) (long top-limit)) (draw-rule! g left inner-w sep-row))
     (when (> (long title-rule-row) (long title-row)) (draw-rule! g left inner-w title-rule-row))
     (when (> (long foot-rule-row) (max (long sep-row) (long top-limit)))
       (draw-rule! g left inner-w foot-rule-row))
     (p/set-colors! g t/dialog-hint-key t/dialog-bg)
     (p/styled g
               [p/BOLD]
               (p/put-str! g
                           (inc left)
                           title-row
                           (dialogs/ellipsize (str (get-in form [:request :title]))
                                              (max 0 (- inner-w 2)))))
     (let
       [cursor (reduce (fn [acc [i entry]]
                         (or (paint-row! g left (+ (long body-top) (long i)) row-w entry) acc))
                       nil
                       (map-indexed vector shown))]
       (doseq [i (range (count shown) body-visible)]
         (paint-row! g left (+ (long body-top) (long i)) row-w {:kind :blank}))
       (when is-overflowing
         (scrollbar/draw! g
                          {:col (+ left inner-w)
                           :top body-top
                           :track-h body-visible
                           :total-h total
                           :inner-h body-visible
                           :scroll start}))
       (when actions (paint-row! g left (+ (long body-top) (long body-visible)) inner-w actions))
       (dialogs/draw-hint-bar! g left foot-row inner-w bar)
       (p/clear-styles! g)
       cursor))))
