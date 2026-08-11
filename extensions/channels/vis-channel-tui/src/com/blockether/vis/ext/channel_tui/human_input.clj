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
            [com.blockether.vis.ext.channel-tui.components :as components]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.channel-tui.transient :as tr]
            [com.blockether.vis.internal.human-input :as engine]
            [com.blockether.vis.internal.human-input.spec :as hi-spec])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

(set! *warn-on-reflection* true)

(def ^:private text-types
  "Field types edited as free text — the engine's own set plus `:otp`: a code is
   typed, erased and walked exactly like a text field, it only LOOKS like a row
   of boxes."
  (conj hi-spec/text-types :otp))

(defn- range-bounds
  "`{:lo :hi :st}` for a `:range` field. Named away from `min`/`max` on purpose:
   destructuring those would shadow the two core fns this file rounds with."
  [field]
  (let
    [num (fn [k]
           (let [v (get field k)]
             (if (number? v) v (get hi-spec/range-defaults k))))]
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

(def ^:private otp-slot-default
  "Digits an `:otp` field falls back to — the engine's own default, so a view
   that somehow arrives without bounds still draws the six boxes everyone
   expects from a one-time code."
  (long (:length hi-spec/otp-defaults)))

(defn- otp-slots
  "`{:lo :hi}` — the fewest and the most digits this `:otp` field accepts."
  [field]
  (let
    [hi
     (max 1 (long (or (:max-length field) otp-slot-default)))

     lo
     (max 1 (min hi (long (or (:min-length field) otp-slot-default))))]

    {:lo lo :hi hi}))

(def ^:private otp-cell-w "`[7] ` — one box, its digit, and the gap to the next box." 4)

(def ^:private mask-char "Secret fields never render their plaintext." (char 0x2022))

(defn- otp-text
  "`[•] [•] [•] [ ] [ ] [ ]` — one box per digit the field takes, filled left to
   right. A one-time code is a credential, so the boxes report HOW MANY digits
   landed and never which ones. A field that accepts a RANGE of lengths says so
   after the boxes: eight empty boxes cannot show that six of them are already
   enough."
  [{:keys [lo hi]} value]
  (let [digits (str value)]
    (str (str/join " "
                   (map (fn [i]
                          (str "[" (if (< (long i) (count digits)) mask-char \space) "]"))
                        (range hi)))
         (when (not= (long lo) (long hi)) (str "  (" lo "–" hi " digits)")))))

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
        (contains? hi-spec/choice-types type)
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
   onto the `Submit` cap and Enter presses it. Cancel only exists when the request
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
  (into (into [] (mapcat field-stops) (engine/input-fields (:fields request)))
        (action-stops request)))

(defn init-form
  "Build the form model for a request VIEW. Text cursors start at end-of-text
   and focus starts on the first stop."
  [request]
  (let
    [answerable
     (engine/input-fields (:fields request))

     values
     (into {} (map (juxt :id default-value)) answerable)]

    {:request request
     :values values
     :cursors (into {}
                    (keep (fn [{:keys [id type]}]
                            (when (contains? text-types type) [id (count (str (get values id)))])))
                    answerable)
     :stops (stops request)
     :focus 0
     ;; PRISTINE: a form complains only after the engine REFUSED a confirmation,
     ;; and the next touch of a field clears that field's message.
     :errors {}}))

(defn request-id "The engine request id this form answers." [form] (get-in form [:request :id]))

(defn request<-wire
  "Rehydrate a request VIEW from the canonical snake_case wire map a
   `human_input.request` SESSION event carries — the only shape a request takes
   when the parked run lives in the serve daemon instead of this process. The
   ENGINE owns that inverse (`view<-wire`); the terminal never keeps a second
   field vocabulary."
  [wire]
  (engine/view<-wire wire))

(defn session-id
  "The gateway session whose run this form parks, or nil. A form built from a
   session event must be answered over the gateway that owns it."
  [form]
  (some-> (get-in form [:request :session-id])
          str
          str/trim
          not-empty))

(defn submit-values
  "The values map handed to `submit-human-input!` — keyed by field id."
  [form]
  (:values form))

(defn- field-by-id
  [form field-id]
  (first (filter #(= field-id (:id %)) (engine/input-fields (get-in form [:request :fields])))))

(defn focused-stop
  "The stop under the cursor, or nil for a form with no stops."
  [{:keys [stops focus]}]
  (get stops (or focus 0)))

(defn- move-focus
  [{:keys [stops] :as form} delta]
  (let [n (count stops)]
    (if (zero? n)
      form
      ;; Moving is not touching: walking off a field neither validates it nor
      ;; clears what the engine already said about it.
      (assoc form :focus (mod (+ (long (:focus form 0)) (long delta)) n)))))

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
    ;; C-g is Esc: Emacs `keyboard-quit` cancels the form, and the chat loop
    ;; feeds this decoder RAW keystrokes, so the rewrite belongs here.
    (let
      [^KeyStroke key
       (input/normalize-abort-key key)

       kt
       (.getKeyType key)]

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
;; Errors
;; =============================================================================

(defn- focus-first-error
  "Park the cursor on the first stop the engine complained about, so a refused
   confirmation lands the operator on what needs fixing."
  [form errors]
  (let
    [bad
     (set (keys errors))

     idx
     (first (keep-indexed (fn [i {:keys [field-id]}]
                            (when (contains? bad field-id) i))
                          (:stops form)))]

    (cond-> form
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
     ;; Confirmation is the ONLY moment anything is validated, and the ENGINE
     ;; validates: the form never keeps a second copy of the rules to
     ;; second-guess it with. A rejection comes back through [[set-errors]].
     (fn []
       {:form form :action :submit})

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
            ;; it presses that button, which is the only way `Cancel`
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
  "The ONE way a form turns red: the engine's per-field verdict on a CONFIRMATION.
   Focus lands on the first offending field, and the next touch of that field
   clears its message (see [[put-text]], [[nudge-range]], [[toggle-stop]]) — so
   the form is pristine again until the next confirmation."
  [form errors]
  (let [errors (or errors {})]
    (assoc (focus-first-error form errors) :errors errors)))

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
  "The web's own mark for a field that cannot be left blank, and the terminal wears
   the same one: a `*` right after the label, in `t/footer-error-fg`. Spelling
   `REQUIRED` out beside every label shouted the same word down the whole form and
   shoved the labels apart; one red cell says it without competing with the label it
   annotates — and the engine REFUSES a submission that leaves such a field blank,
   so the warning colour is the honest one.

   The leading space belongs to the marker: it is the gap from the label, and it is
   what lets a painter find the marker at the END of an already-ellipsized row."
  " *")

(defn- label-text [{:keys [label is-required]}] (str label (when is-required required-marker)))

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

(defn- decor-rows
  "Rows for a DECORATION — a heading or a paragraph. Neither holds a value and
   neither is a focus stop: these are the section titles and the sentences that
   make a long form readable, so they are planned as plain ink the keyboard walks
   straight past. A paragraph wraps like any other prose in this dialog; a
   heading is one line, ellipsized if the dialog is narrow."
  [text-w {:keys [type text]}]
  (if (= :heading type)
    ;; No gap of its own: a heading is read together with the paragraph under it,
    ;; and the field it introduces already opens with the blank its label owns.
    [{:kind :heading :text text}]
    (mapv #(assoc % :kind :paragraph) (description-rows text text-w))))

(declare field-rows)

(def ^:private column-gutter
  "Columns between two fields sitting side by side — enough that a focused row's
   selection marker never touches the field to its left."
  2)

(defn- zip-columns
  "Zip one row-plan per COLUMN into one plan row per LINE: cell `i` of line `n`
   is column `i`'s `n`-th row, or nothing when that column already ran out. The
   painter divides the row's width by the number of cells, so a `row` group is
   laid out at paint time and the plan stays pure."
  [cells]
  (let [height (long (reduce max 0 (map count cells)))]
    (mapv (fn [i]
            (let [entries (mapv #(nth % i nil) cells)]
              {:kind :columns
               :cells entries
               ;; A composite row is a LABEL row when its columns start with
               ;; their labels, so scrolling still lands on the words that say
               ;; which fields are being edited.
               :is-label (boolean (some #(= :label (:kind %)) entries))
               :is-focused (boolean (some :is-focused entries))}))
          (range height))))

(def ^:private option-stop-kinds
  "Stop kinds that carry a `:value`: one focus stop per option, not per field."
  #{:select-option :multi-option})

(defn- stop-index
  "Focus lookup for ONE paint: stop identity → its index in the flat `stops`
   vector, keyed by field id — or by `[field-id value]` for the option kinds,
   which put several stops on the same field.

   Built once with a transient and threaded through the plan, because every row
   asks \"is this the focused stop?\": scanning `stops` per row made painting a
   form quadratic in its field count. Earlier stops win, so a duplicate id keeps
   the first stop ↑/↓ would reach."
  [stops]
  (loop
    [i
     (dec (count stops))

     acc
     (transient {})]

    (if (neg? i)
      (persistent! acc)
      (let [{:keys [kind field-id value]} (nth stops i)]
        (recur (dec i)
               (cond-> acc
                 field-id
                 (assoc! (if (contains? option-stop-kinds kind) [field-id value] field-id) i)))))))

(defn- group-rows
  "Rows for a layout group: its optional heading, then its children — stacked
   when the group is a `:column`, side by side when it is a `:row`. A child may
   itself be a group, so the two directions compose without another rule."
  [ctx text-w {:keys [direction fields] :as group}]
  (let
    [heading
     (into (if (:label group) [{:kind :label :text (:label group)}] [])
           (description-rows (:description group) text-w))

     n
     (max 1 (count fields))

     body
     (if (= :row direction)
       (let [cell-w (when text-w (max 4 (- (quot (+ (long text-w) 2) n) 2 (long column-gutter))))]
         (zip-columns (mapv #(vec (field-rows ctx cell-w %)) fields)))
       (into [] (mapcat #(field-rows ctx text-w %)) fields))]

    (into (if (seq heading) (conj (vec heading) {:kind :blank}) []) body)))

(defn- field-rows
  "Rows for ONE node of the field tree. `ctx` is the per-paint context built by
   [[form-rows]] — the form, its visible errors, the focused stop and the
   [[stop-index]] — and `text-w` is the prose budget of the column this node
   lands in, which a `:row` group narrows for its children."
  [{:keys [form errors focus index] :as ctx} text-w {:keys [id type] :as field}]
  (cond
    (= :group type) (group-rows ctx text-w field)
    (hi-spec/decoration? field) (decor-rows text-w field)
    :else
    (let
      [idx
       (get index id)

       value
       (get-in form [:values id])

       ;; The keyboard is in this FIELD when the focused stop belongs to it — its
       ;; own stop, or, for a choice field, any one of its option stops. The whole
       ;; SECTION reads off this: its label takes the ink and its prose becomes
       ;; readable, while every other field recedes. A form that paints all five
       ;; at full strength has said nothing about which one is being filled.
       is-active-field
       (= id (:field-id (focused-stop form)))

       description
       (mapv #(assoc % :is-active-field is-active-field)
             (description-rows (:description field) text-w))

       rows
       (cond (= :otp type) (let [slots (otp-slots field)]
                             [{:kind :otp
                               :field-id id
                               :text (otp-text slots value)
                               :cursor (min (cursor-of form id) (max 0 (dec (long (:hi slots)))))
                               :is-focused (= idx focus)
                               :is-active-field (= idx focus)}])
             (contains? text-types type)
             (let
               [focused?
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
                                        ;; The FIELD is lit whole while the keyboard
                                        ;; is in it; only the line being typed wears
                                        ;; the cursor glyph and the terminal caret.
                                        :is-focused (and focused? (= i line))
                                        :is-active-field focused?
                                        :placeholder (when (and (zero? i) (zero? (count shown)))
                                                       (:placeholder field))})
                                     (str/split shown #"\n" -1))))
                 [{:kind :input
                   :field-id id
                   :text shown
                   :cursor cursor
                   :is-focused focused?
                   :is-active-field focused?
                   :placeholder (:placeholder field)}]))
             (= :range type) (let
                               [bounds
                                (range-bounds field)

                                v
                                (range-snap bounds (if (number? value) value (:lo bounds)))]

                               [{:kind :range
                                 :field-id id
                                 :value v
                                 :text (range-text bounds v)
                                 :is-focused (= idx focus)}])
             (= :checkbox type) [{:kind :checkbox
                                  :field-id id
                                  :text (label-text field)
                                  :is-required (boolean (:is-required field))
                                  :is-checked (boolean value)
                                  :is-focused (= idx focus)}]
             (contains? hi-spec/choice-types type)
             (let [chosen (if (= :multiselect type) (set value) #{value})]
               (mapv (fn [{:keys [value label]}]
                       {:kind :option
                        :field-id id
                        ;; EXCLUSIVE (`:select`) or INCLUSIVE (`:multiselect`): the row
                        ;; carries the ANSWER SHAPE, and the painter answers it with the
                        ;; radio dot or the checkbox box — so the option itself says
                        ;; whether picking a second one replaces the first or adds to it.
                        :is-exclusive (= :select type)
                        :text (or label (str value))
                        :is-checked (contains? chosen value)
                        :is-focused (= (get index [id value]) focus)})
                     (field-options field)))
             :else [])]

      ;; Label, then a BLANK, then description, then the input: the label is the
      ;; headline of its own section, and everything it introduces — the italic
      ;; prose that explains the field, the options, the input itself — starts one
      ;; row below it. Butted straight against the label, a form of five fields
      ;; painted as one unbroken column and nothing said where a field began.
      ;;
      ;; A checkbox row already carries its own label — a separate bold label row
      ;; above it would say the same word twice, which no other dialog does — so
      ;; there its description follows the box instead, with no gap to open.
      ;;
      ;; The prose is its own block too: when a field has one, another blank
      ;; follows it, so the sentence that explains the field is not butted against
      ;; the box it explains and cannot be read as part of it.
      (cond->
        (if (= :checkbox type)
          (into (vec rows) description)
          (into (into (into [{:kind :label
                              :text (label-text field)
                              :is-required (boolean (:is-required field))
                              :is-active-field is-active-field} {:kind :blank}]
                            description)
                      (when (seq description) [{:kind :blank}]))
                rows))
        (get errors id)
        (conj {:kind :error :text (get errors id)})

        true
        (conj {:kind :blank})))))

(defn action-bar
  "The request's own buttons as ONE row: the solid ` Submit ` pill, then the muted
   ` Cancel ` one.

   PRIMARY vs SECONDARY is a SEMANTIC ranking, not a focus state — submit is the
   confirming action and keeps the ink fill wherever the cursor is, so walking the
   caps never promotes Cancel to look like the default. `:is-focused` says which
   cap the cursor sits on, and the painter answers it with the project-wide `•`
   marker instead of a second colour.

   No chord travels with a button: a cap is a focus stop reached with ↑/↓ like
   every other row of the form. The hint bar one row below stays silent about
   submit and cancel too ([[hint]]) — it used to reprint these very labels as
   `Enter submit · Esc cancel`, two rows of chrome for one meaning.

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
  ([{:keys [request focus stops] :as form} text-w]
   (let
     [head
      (when-let [rows (seq (description-rows (:description request) text-w))]
        (conj (vec rows) {:kind :blank}))

      ;; ONE context per paint — including the stop index the whole tree shares,
      ;; so nested groups cost lookups, not rescans.
      ctx
      {:form form :errors (:errors form) :focus (or focus 0) :index (stop-index stops)}]

     (into (vec head) (mapcat #(field-rows ctx text-w %)) (:fields request)))))

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
       (long (or (first (filter #(let
                                   [r
                                    (nth rows %)]

                                   (or (= :label (:kind r)) (:is-label r)))
                                (range focus-idx -1 -1)))
                 0))

       start
       (long (dialogs/visible-window-start focus-idx 0 visible (count rows)))]

      (min start label-idx))))

(defn hint
  "Hint-bar pairs for `form` — TYPING chords only, spelled the canonical dialog
   way: chord, then a lowercase action.

   NAVIGATION IS NOT A HINT. `↑/↓ move` used to lead this list on every single
   pause: a permanent row of chrome teaching the one thing every terminal
   operator already knows, printed under a form whose focus ring already shows
   where the cursor is. So the bar is EMPTY unless the FOCUSED stop accepts a
   chord the band itself cannot show — Space on a toggle, digits in an OTP, ←/→
   on a slider, Enter for a newline.

   Submit and cancel are not here either: [[action-bar]] paints those two
   controls one row above with their own chords ON the caps, so a hint pair for
   either would print the same verb twice, a row apart. The one Enter that
   belongs here is the multiline NEWLINE — that is a typing chord, and it is also
   why the submit cap switches to `^S`."
  [form]
  (let
    [stop
     (focused-stop form)

     otp?
     (= :otp (:type (field-by-id form (:field-id stop))))]

    (cond-> []
      (contains? #{:checkbox :multi-option} (:kind stop))
      (conj ["Space" "toggle"])

      ;; An EXCLUSIVE option has no off state — Space moves the single choice onto
      ;; this row. Calling that "toggle" promised an off switch the field does not
      ;; have, and the same word then meant two different things one field apart.
      (= :select-option (:kind stop))
      (conj ["Space" "pick"])

      otp?
      (conj ["0–9" "fill"])

      (= :range (:kind stop))
      (conj ["←/→" "adjust"])

      (multiline-focus? form)
      (conj ["Enter" "newline"]))))

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

(defn- paint-required!
  "Re-ink a row's trailing [[required-marker]] in the error colour, on whatever paper
   the row is already wearing. It is the same red the field's error line uses,
   because it names the same refusal — one turn earlier."
  [g col row bg]
  (p/set-colors! g t/footer-error-fg bg)
  (p/put-str! g col row required-marker)
  (p/set-colors! g t/dialog-fg t/dialog-bg))

(defn- paint-actions!
  "The PINNED action bar: every button the request offers on ONE row, drawn as the
   SHARED neobrutalist cap `components/action-button!` — the same control the
   confirm dialog and the spel-bridge modal use:

     Submit   Cancel

   Nothing but COLOUR distinguishes the caps, and colour answers both questions at
   once without a glyph: the cap under the cursor wears the product's ACCENT fill
   (the active tab's own colours), and the caps it is not on keep their RANK —
   `:submit` is the PRIMARY ink fill, every other cap the muted secondary. So the
   accent says where you are and the ink says what matters, and walking ↑/↓ never
   demotes Submit.

   No `•` marker beside a cap: a marker is a second alphabet for something the
   fill already says, and it cost the row two columns of gutter that had to be
   reserved in both states. Nothing stencils a chord either — the caps ARE the
   visible way to end the pause, and `Enter`/`Esc` printed next to them only named
   a shortcut for the control already under the cursor. Every state measures the
   same, so the row never shifts as focus moves.

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
    (reduce (fn [^long col {:keys [action label is-focused]}]
              (let [w (+ 2 (long (p/display-width label)))]
                (when (<= (+ col w) right)
                  (components/action-button! g
                                             col
                                             row
                                             label
                                             {:variant (if (= :submit action) :primary :secondary)
                                              :is-focused is-focused}))
                (+ col w 2)))
            (inc left)
            buttons)
    nil))


(defn- paint-row!
  "Paint one plan row. Every focusable row of the form — typed line, digit boxes,
   option, checkbox, slider — goes through the SHARED field painters
   (`dialogs/draw-field-row!`, `dialogs/draw-input-item!`) so it looks like an
   input and cannot drift from the rest of the TUI.

   Every row of a field also paints its FOCUS: the section the keyboard is in
   takes the ink — bold label, readable prose, an inked field — and the ones it
   is not in dim to `dialog-hint`. Returns the terminal cursor position when the
   row owns it."
  [g left row inner-w entry]
  (case (:kind entry)
    :blank
    (do (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc (long left)) row inner-w 1)
        nil)

    ;; The request's own prose has no field to belong to, so it stays dim; a
    ;; field's description brightens with the field, and fades back with it.
    :description
    (do (paint-italic! g
                       left
                       row
                       inner-w
                       (if (:is-active-field entry) t/dialog-fg t/dialog-hint)
                       (:text entry))
        nil)

    :label
    (let
      [fg
       (if (:is-active-field entry) t/dialog-fg t/dialog-hint)

       shown
       (dialogs/ellipsize (str (:text entry)) (max 0 (- (long inner-w) 2)))

       ;; The `*` is the last thing on the row, so it is inked red only when it
       ;; actually survived the ellipsis — a truncated label must not stain its
       ;; own tail.
       mark?
       (boolean (and (:is-required entry) (str/ends-with? shown required-marker)))

       head
       (if mark? (subs shown 0 (- (count shown) (count required-marker))) shown)]

      (p/set-colors! g fg t/dialog-bg)
      (p/fill-rect! g (inc (long left)) row inner-w 1)
      (p/styled g [p/BOLD] (p/put-str! g (inc (long left)) row head))
      (when mark? (paint-required! g (+ (inc (long left)) (count head)) row t/dialog-bg))
      nil)

    :error
    (do (paint-plain! g left row inner-w t/footer-error-fg (:text entry)) nil)

    ;; A DECORATION answers nothing and is never focused, so it wears neither a
    ;; field surface nor focus ink: a heading is the bold section title that
    ;; breaks a long form into parts, a paragraph the prose that explains one.
    ;; They read exactly the same whatever the keyboard is doing, which is what
    ;; makes them safe to hang a form's structure on.
    :heading
    (do (p/set-colors! g t/dialog-fg t/dialog-bg)
        (p/fill-rect! g (inc (long left)) row inner-w 1)
        (p/styled g
                  [p/BOLD]
                  (p/put-str! g
                              (inc (long left))
                              row
                              (dialogs/ellipsize (str (:text entry)) (max 0 (- (long inner-w) 2)))))
        nil)

    :paragraph
    (do (paint-italic! g left row inner-w t/dialog-hint (:text entry)) nil)

    ;; A checkbox, an option and a slider are TOGGLED, not typed: they keep a
    ;; typed row's geometry so the form lines up, but they are painted on the
    ;; dialog's own paper. A filled input surface under a row that cannot take a
    ;; character promised typing where only Space toggles. Focus is the ring and
    ;; the bold ink, and the ●/○ or [✓]/[ ] glyph says what the toggle IS.
    :checkbox
    (let
      [content
       (str (dialogs/choice-mark false (:is-checked entry)) (:text entry))

       shown
       (dialogs/ellipsize content (dialogs/field-content-w inner-w))

       text-left
       (dialogs/draw-toggle-row! g left row inner-w (:is-focused entry) content)]

      ;; A checkbox carries its own label, so its `*` is re-inked on the paper the
      ;; box sits on — the dialog's own, in the same red every other label uses.
      (when (and (:is-required entry) (str/ends-with? shown required-marker))
        (paint-required! g
                         (+ (long text-left) (- (count shown) (count required-marker)))
                         row
                         t/dialog-bg))
      nil)

    ;; One row shape, TWO toggle vocabularies. An exclusive `:select` option wears
    ;; the shared ●/○ radio mark — picking one drops the other; an inclusive
    ;; `:multiselect` option wears the `[✓]`/`[ ]` box — pick as many as apply.
    ;; Painting both with the radio dot made "choose any" look exactly like "choose
    ;; one", which is the one thing an option row exists to say. The companion app
    ;; already draws the two marks apart, so this is also what keeps the surfaces
    ;; speaking one vocabulary.
    :option
    (do (dialogs/draw-toggle-row!
          g
          left
          row
          inner-w
          (:is-focused entry)
          (str (dialogs/choice-mark (:is-exclusive entry) (:is-checked entry)) (:text entry)))
        nil)

    :range
    (do (dialogs/draw-toggle-row! g left row inner-w (:is-focused entry) (:text entry)) nil)

    ;; The digit boxes are an INPUT, so they ride the same field surface a typed
    ;; row does; what is special is that the TERMINAL cursor is parked inside the
    ;; active box, so the operator sees where the next digit lands instead of
    ;; guessing.
    :otp
    (let
      [text-left
       (dialogs/draw-field-row! g left row inner-w (:is-active-field entry) (:text entry))]
      (when (:is-focused entry)
        (p/cursor-pos (min (+ (long left) (long inner-w))
                           (+ (long text-left) 1 (* (long otp-cell-w) (long (:cursor entry 0)))))
                      row)))

    ;; A `row` group's line: the width is divided between the cells HERE, so the
    ;; plan carries no geometry and every painter below — including another
    ;; `:columns` row from a nested group — just paints into a narrower row.
    :columns
    (let
      [cells
       (:cells entry)

       n
       (max 1 (count cells))

       cell-w
       (max 1 (quot (long inner-w) n))]

      (reduce (fn [pos [i cell]]
                (let
                  [taken
                   (* (long i) cell-w)

                   width
                   (if (= i (dec n)) (- (long inner-w) taken) cell-w)

                   here
                   (paint-row! g (+ (long left) taken) row (max 1 width) (or cell {:kind :blank}))]

                  (or pos here)))
              nil
              (map-indexed vector cells)))

    :action
    (do (paint-actions! g left row inner-w (:buttons entry)) nil)

    :input
    (let
      [text
       (str (:text entry))

       cursor
       (clamp (long (or (:cursor entry) 0)) 0 (count text))

       pos
       (dialogs/draw-input-item! g
                                 left
                                 row
                                 inner-w
                                 (:is-active-field entry)
                                 text
                                 cursor
                                 (:placeholder entry))]

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





(defn paint!
  "Draw the human-input band for `form` INSIDE the session's own frame. Returns
   the `TerminalPosition` the caller should place the terminal cursor at (the
   focused text field), or nil when no text field has focus.

   A magit-style TRANSIENT, not a modal — and the SAME band the C-x hydra paints
   (`transient/paint-layout!`), down to the painter: a closed BOX on the
   terminal's own paper, corner-capped rules with `│` down both edge columns and
   the hint bar fenced INSIDE it under its own rule. It is anchored directly
   ABOVE the session's PROMPT — the input box and the footer stay visible and in
   place underneath — and grows UPWARD over the transcript, never past
   `content-top`.

   No tint: the band wears `terminal-bg` like every other transient, so it is the
   BORDER that says where the band is. A form painted on paper of its own read as
   a third kind of surface next to the hydra it is supposed to BE.

   No title row either: the request's question rides ON the opening rule the way
   magit's `── Commit ──` does, which hands the fields back the two rows a title
   row and its own rule used to cost.

   `prompt-h` is the live height of that input box (`screen`'s `input-box-h`),
   which is what decides where the band's floor is.

   The body is inset by `transient/pane-lead`, the very lead the hydra gives its
   own panes: the accent ring `▎` a focused row wears takes the LAST of those
   columns and the text starts after it, so a focused row is fenced off the rail
   by a clear column instead of painting its ring against it — and the form's
   text column then lands exactly where a transient's items do.

   The scrollbar sits in the LANE every other scrollable dialog uses — the last
   column INSIDE the right rail (`left + inner-w`), painted by the shared
   `scrollbar/draw!`. That lane is the clear column the body's right lead already
   reserves, so the bar costs the prose nothing and needs no second wrap pass. A
   thumb painted ON the rail turned the band's own border into a control: the box
   gained a moving glyph no other dialog's border has, and there was no gutter
   between the frame and the position marker.

   The action bar is PINNED under a blank row of its own: only the fields scroll
   under it, so the `Submit` and `Cancel` caps stay on screen for a form of any
   length, and the row of air above them keeps the caps from growing out of
   whatever field the scroll happens to end on."
  ([g cols rows form] (paint! g cols rows form 1 tr/prompt-rows))
  ([g cols rows form content-top] (paint! g cols rows form content-top tr/prompt-rows))
  ;; NOTE: no primitive hints on these arities — Clojure caps primitive-taking fns
  ;; at four arguments — so the sizes are coerced inside the `let` instead.
  ([g cols rows form content-top prompt-h]
   (let
     [{:keys [left inner-w] :as region}
      (tr/band-region (long cols) (long rows) (long content-top) (long prompt-h))]
     ;; A band is not a dialog: it lies on the LIVE transcript and wears the
     ;; TERMINAL's own paper, body and footer alike. Bound ONCE, so every painter
     ;; under it — rules, labels, toggles, field surfaces, hint bar — follows
     ;; without carrying a colour argument of its own.
     (binding [t/dialog-bg (if (:is-sideless region) t/terminal-bg t/dialog-bg)]
       (let
         [left (long left)
          inner-w (long inner-w)
          ;; The rails own the two edge columns, and the body takes the hydra's
          ;; own pane lead inside them — the ring gutter is the last of those
          ;; columns, so nothing the form paints ever touches a rail. The same
          ;; one clear column answers on the right, so the box breathes equally
          ;; on both sides and the scrollbar's rail keeps its air.
          body-left (+ left (long tr/pane-lead))
          body-w (- inner-w (long tr/pane-lead))
          baz (hint form)
          actions (action-bar form)
          ;; The pinned bar costs its own row plus the blank one above it.
          actions-h (if actions 2 0)
          plan
          ;; Sizing pass: how many rows the form wants decides where the band's
          ;; top rule lands. The pinned action bar asks for its two rows, and the
          ;; fenced hint bar asks for the rule above it.
          (form-rows form (prose-width body-w))
          {:keys [sep-row body-top foot-rule-row foot-row visible top-limit]}
          (tr/band-geometry region (+ (count plan) (long actions-h) 1) false)
          ;; The band CLOSES below its footer, exactly like a sideless transient:
          ;; the hint bar takes the row the closing rule used to own and the rule
          ;; drops onto the host's hint row, so the footer is inside the box.
          hint-at (long foot-rule-row)
          rule-at (long foot-row)
          hint-rule-at (dec hint-at)
          visible (max 1 (dec (long visible)))
          body-visible (max 0 (- visible (long actions-h)))
          total (count plan)
          is-overflowing (> total body-visible)
          start (long (if (= :action (:kind (focused-stop form)))
                        ;; Focus is on a pinned button: the fields stay where the operator
                        ;; left them — at the end — instead of snapping back to row 0.
                        (max 0 (- total body-visible))
                        (window-start plan body-visible)))
          shown (subvec (vec plan) (min start total) (min total (+ start body-visible)))]

         (tr/clear-rows! g region (max 0 (long sep-row)) rule-at)
         ;; The question is the rule's own label — `── Deploy? ──` — so the first
         ;; row is chrome and every row under it is the form.
         (when (>= (long sep-row) (long top-limit))
           (tr/draw-rule! g region sep-row (get-in form [:request :title])))
         (when (> rule-at (max (long sep-row) (long top-limit))) (tr/draw-rule! g region rule-at))
         (when (> (long hint-rule-at) (max (long sep-row) (long top-limit)))
           (tr/draw-rule! g region hint-rule-at))
         (let
           [cursor (reduce (fn [acc [i entry]]
                             (or (paint-row! g body-left (+ (long body-top) (long i)) body-w entry)
                                 acc))
                           nil
                           (map-indexed vector shown))]
           (doseq [i (range (count shown) body-visible)]
             (paint-row! g body-left (+ (long body-top) (long i)) body-w {:kind :blank}))
           (when actions
             (paint-row! g body-left (+ (long body-top) (long body-visible)) body-w {:kind :blank})
             (paint-row! g body-left (+ (long body-top) (long body-visible) 1) body-w actions))
           (dialogs/draw-hint-bar! g left hint-at inner-w baz)
           ;; The gutter lane every scrollable dialog draws its bar in: the last
           ;; column inside the right rail, which the body's own lead keeps clear.
           (when is-overflowing
             (scrollbar/draw! g
                              {:col (+ left inner-w)
                               :top body-top
                               :track-h body-visible
                               :total-h total
                               :inner-h body-visible
                               :scroll start
                               :track-fg t/border-fg}))
           (tr/draw-band-border! g region sep-row rule-at top-limit)
           (p/clear-styles! g)
           cursor))))))
