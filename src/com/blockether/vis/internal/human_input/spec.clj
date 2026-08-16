(ns com.blockether.vis.internal.human-input.spec
  "The executable contract for typed human input: `clojure.spec` over the
   NORMALIZED form of a request, plus the closed vocabulary that form is built
   from.

   Two layers, one contract. `com.blockether.vis.internal.human-input` PARSES —
   an extension (or a Python object arriving as JSON) writes `is_required` or
   `:is-required`, `\"otp\"` or `:otp`, and the normalizer turns whatever came in
   into one internal shape, naming the key its author has to fix when it cannot.
   This namespace DECLARES that shape: every map is CLOSED, `:name` and `:id`
   are the same identity, a `:select` really carries options, an `:otp` really
   fits its boxes, and a `:default` really is a value of the field's own type.

   Both are needed. A parser that only refuses bad INPUT still lets a bug
   INSIDE the engine hand a surface a field with no `:label`, or hand a blocked
   extension an answer with no `:request-id` — and that failure then surfaces as
   a broken dialog three namespaces away from its cause. The specs are checked
   once per request and once per answer, never per keystroke, so the guard sits
   where it costs nothing.

   The functions here only EXPLAIN ([[field-error]], [[group-error]],
   [[request-error]], [[answer-error]] return nil or a one-line reason); the
   refusal itself stays
   in `human-input`, which owns the error envelope every surface already
   handles."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; The closed vocabulary

(def field-types
  "Wire type name -> internal field type. A CLOSED set: an unknown name is
   refused with these listed, never minted into a keyword the surfaces cannot
   paint.

   Only a type that holds an ANSWER belongs here. A layout `group` does not —
   see [[group-type]]."
  {"plaintext" :plaintext
   "password" :password
   "multiline" :multiline
   "select" :select
   "multiselect" :multiselect
   "checkbox" :checkbox
   "range" :range
   "otp" :otp})

(def text-types "Field types whose answer is typed text." #{:plaintext :password :multiline})

(def choice-types
  "Field types answered by picking from `:options` — exclusive, then inclusive."
  #{:select :multiselect})

(def secret-types
  "Field types whose value must never reach a log, an event or a transcript. A
   one-time code is as much a credential as a password — it opens the account
   once — so both answer with a vault handle instead of what the human typed."
  #{:password :otp})

(def group-type
  "The type of a layout GROUP: the one node of a request's field tree that holds
   no answer, and deliberately NOT a member of [[field-types]].

   A group is the control flow ABOVE the fields, not a ninth kind of field. It
   owns the children it arranges and the direction they run in, and nothing that
   describes a value — no default, no options, no rules, no key in the answer
   map. Listing it beside `:otp` said the opposite: every value path had to
   carry a branch for a node that can never take one, and a field spec would
   happily accept a group."
  :group)

(def group-type-name "How a wire spec asks for a [[group-type]] node." "group")

(def group-directions
  "Wire direction name -> internal group direction."
  {"column" :column "row" :row})

(def decor-types
  "Wire type name -> internal type of a DECORATION: a node that asks nothing and
   arranges nothing, and is on the form purely to be READ.

   A `heading` breaks a long form into sections and a `paragraph` explains one.
   Neither is in [[field-types]] and neither is a [[group-type]]: a decoration
   holds no answer — no default, no options, no rules, no key in the answer map,
   no focus stop — and owns no children either. It has no `:name` at all, which
   is the point: there is no identity, so two headings reading the same words are
   two decorations rather than a name collision."
  {"heading" :heading "paragraph" :paragraph})

(def ^:private decor-node-types (set (vals decor-types)))

(defn decoration?
  "True when this normalized node is a [[decor-types]] decoration — ink on the
   form rather than a question. Every surface asks this before it looks for a
   value, and the answer contract never sees such a node at all."
  [{:keys [type]}]
  (contains? decor-node-types type))

(def otp-defaults
  "How many boxes a one-time code gets by default, and the most it may ask for:
   past a dozen the boxes no longer fit a narrow dialog."
  {:length 6 :ceiling 12})

(def range-defaults
  "The track a `:range` field falls back to. A slider with no bounds is a
   PERCENTAGE — the one scale every operator already reads without being told
   what the numbers mean — and every surface fills the same three numbers in, so
   a hand-made request view draws the same knob a normalized one does."
  {:min 0 :max 100 :step 1})

(def secret-handle-prefix
  "What a submitted secret — a `:password` or an `:otp` — becomes before its
   answer leaves the engine. The plaintext stays in a process-local vault; this
   prefix is the whole difference between an answer that is harmless in a log
   and a leaked credential, so the answer contract is declared in terms of it."
  "vis-secret:")

(defn secret-handle?
  "True when `value` is an opaque handle minted for a `secret-types` field."
  [value]
  (and (string? value) (str/starts-with? value secret-handle-prefix)))

(defn contract-vocabulary
  "This vocabulary as DATA, for `com.blockether.vis.contract.python-host` to render
   into `vis_contract/contract.json` — the document every surface that cannot
   require this namespace reads instead. The tables above stay the one definition:
   a Python reader gets a rendering of them, never a transcription."
  []
  {:field-types (vec (sort (keys field-types)))
   :text-types (mapv clojure.core/name (sort text-types))
   :choice-types (mapv clojure.core/name (sort choice-types))
   :secret-types (mapv clojure.core/name (sort secret-types))
   :decor-types (vec (sort (keys decor-types)))
   :group-type group-type-name
   :group-directions (vec (sort (keys group-directions)))
   :otp {:length (:length otp-defaults) :ceiling (:ceiling otp-defaults)}
   :range {:min (:min range-defaults) :max (:max range-defaults) :step (:step range-defaults)}
   :secret-handle-prefix secret-handle-prefix})

;; The keys — one table, and the parser reads it too
;;
;; Every map declared below is CLOSED, so each shape's key set is written down
;; exactly once here, and the snake_case spelling a wire spec may use is derived
;; from these very sets by the parser's `wire-keys`. A key added here reaches
;; both layers with no second table to keep in step.

(def derived-keys
  "Keys the ENGINE stamps on a normalized node, never written in a spec.
   `:is-secret` follows from the type, so a caller offering it is refused."
  #{:is-secret})

(def request-stamp-keys
  "Keys the ENGINE stamps on a pending REQUEST, so every channel sees them on the
   projected view although no spec may write one: the registry's arrival time.
   Same category as [[derived-keys]] one level up — `request-keys` deliberately
   refuses them on the way in, so a reader rebuilding a view that crossed a
   process boundary lifts them across instead of re-parsing them."
  #{:created-at})

(def value-keys
  "Every key an answerable field may carry, whatever its type."
  #{:id :name :type :label :description :placeholder :is-required :is-secret :default :validate})

(def text-keys "Every key a typed field may carry." (into value-keys [:min-length :max-length]))
(def choice-keys "Every key a field answered from `:options` may carry." (conj value-keys :options))
(def range-keys
  "Every key a field answered on a track may carry."
  (into value-keys [:min :max :step]))

(def field-keys
  "Every key a field spec may be WRITTEN with: the union of the per-type sets,
   less what the engine derives. The parser accepts exactly this vocabulary in
   its snake_case spelling, so there is no second table of keys to keep in step."
  (apply disj (into text-keys (concat choice-keys range-keys)) derived-keys))

(def group-keys
  "Every key a layout group may carry. A node that holds no answer has no key
   that describes one."
  #{:id :name :type :label :description :direction :fields})

(def layout-keys
  "The keys only a group has. A field carrying one meant to group and forgot to
   say so, which is worth its own message rather than an unknown-key refusal."
  (apply disj group-keys field-keys))

(def decor-keys
  "Every key a decoration may carry: its own type and the words it paints. A node
   nobody can answer has nothing else to say."
  #{:type :text})

(def option-keys "Every key one `:options` entry may carry." #{:value :label})

(def request-keys
  "Every key a request may carry."
  #{:id :title :description :source :fields :submit-label :cancel-label :is-cancellable :timeout-ms
    :channel-ids :session-id})

(def ^:private answer-keys #{:is-submitted :reason :request-id :values})

;; Predicates

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(defn- closed?
  "True when `m` carries no key outside `allowed`. The internal form is closed
   in both directions: a wire key that survived normalization is a normalizer
   bug, not a harmless extra, and a surface reading the map has one shape to
   paint."
  [allowed m]
  (and (map? m) (every? allowed (keys m))))

(defn- one-identity?
  "`:name` is `:id` under its legacy spelling. Two spellings of one identity
   have to agree, or a value comes back keyed by the name the caller did not
   use."
  [{:keys [id name]}]
  (= id name))

(defn- secret-marked?
  "`:is-secret` is derived from the type, never guessed per field: a password and
   a one-time code are secret and nothing else is silently promoted or demoted."
  [{:keys [type is-secret]}]
  (= is-secret (contains? secret-types type)))

(defn- ordered-lengths?
  [{:keys [min-length max-length]}]
  (or (nil? min-length) (nil? max-length) (<= (long min-length) (long max-length))))

(defn- otp-fits-boxes? [{:keys [max-length]}] (<= (long max-length) (long (:ceiling otp-defaults))))

(defn- ascending-bounds? [{:keys [min max]}] (< (double min) (double max)))

(defn- positive-step?
  [{:keys [min max step]}]
  (and (pos? (double step)) (<= (double step) (- (double max) (double min)))))

(defn- option-values [field] (set (map :value (:options field))))

(defn- non-empty?
  "A collection with something in it. Spelled out rather than left to
   `:min-count`, whose expansion counts in boxed math."
  [coll]
  (boolean (seq coll)))

(defn- all-distinct? [coll] (apply distinct? coll))

(defn- default-in-domain?
  "A `:default` is a value of the field's OWN type, already coerced. This is the
   invariant a surface leans on when it paints a dialog before the human has
   touched it: the slider's knob starts inside its track, the picker starts on
   an option that exists, and a checkbox starts on a boolean rather than on the
   string `\"false\"`."
  [{:keys [type default] :as field}]
  (let
    [{lo :min hi :max :keys [min-length max-length]}
     field

     chosen
     (option-values field)]

    (cond (nil? default) true
          (contains? text-types type) (string? default)
          (= :select type) (contains? chosen default)
          (= :multiselect type)
          (and (vector? default) (every? chosen default) (= (count default) (count (set default))))
          (= :checkbox type) (boolean? default)
          (= :range type) (and (number? default) (<= (double lo) (double default) (double hi)))
          (= :otp type) (and (string? default)
                             (some? (re-matches #"\d+" default))
                             (<= (long min-length) (count default) (long max-length)))
          :else false)))

;; A field — the leaf that holds one answer

(s/def ::id non-blank-string?)
(s/def ::name non-blank-string?)
(s/def ::type (set (vals field-types)))
(s/def ::label non-blank-string?)
(s/def ::description non-blank-string?)
(s/def ::placeholder non-blank-string?)
(s/def ::is-required boolean?)
(s/def ::is-secret boolean?)
(s/def ::default some?)
(s/def ::validate (s/and (s/coll-of ifn? :kind vector?) non-empty?))
(s/def ::min-length pos-int?)
(s/def ::max-length pos-int?)
(s/def ::min number?)
(s/def ::max number?)
(s/def ::step number?)
(s/def ::direction (set (vals group-directions)))
(s/def ::value non-blank-string?)
(s/def ::text non-blank-string?)

(s/def ::option (s/and #(closed? option-keys %) (s/keys :req-un [::value ::label])))

(s/def ::options
  (s/and (s/coll-of ::option :kind vector?)
         non-empty?
         ;; Two options sharing a value make one of them unpickable: whichever
         ;; the human chose, the answer names the first.
         #(apply distinct? (map :value %))))

(s/def ::value-field
  (s/and (s/keys :req-un [::id ::name ::type ::label ::is-required ::is-secret]
                 :opt-un [::description ::placeholder ::default ::validate])
         one-identity?
         secret-marked?
         default-in-domain?))

(def ^:private text-field
  (s/and #(closed? text-keys %)
         ::value-field
         (s/keys :opt-un [::min-length ::max-length])
         ordered-lengths?))

(def ^:private choice-field
  (s/and #(closed? choice-keys %)
         ::value-field
         (s/keys :req-un [::options])))

(def ^:private checkbox-field (s/and #(closed? value-keys %) ::value-field))

(def ^:private range-field
  (s/and #(closed? range-keys %)
         ::value-field
         (s/keys :req-un [::min ::max ::step])
         ascending-bounds?
         positive-step?))

(def ^:private otp-field
  (s/and #(closed? text-keys %)
         ::value-field
         ;; A code's width is not optional: the boxes are painted from it.
         (s/keys :req-un [::min-length ::max-length])
         ordered-lengths?
         otp-fits-boxes?))

(defmulti ^:private field-form "The form one field type must take once normalized." :type)

(defmethod field-form :plaintext [_] text-field)
(defmethod field-form :password [_] text-field)
(defmethod field-form :multiline [_] text-field)
(defmethod field-form :select [_] choice-field)
(defmethod field-form :multiselect [_] choice-field)
(defmethod field-form :checkbox [_] checkbox-field)
(defmethod field-form :range [_] range-field)
(defmethod field-form :otp [_] otp-field)

(s/def ::field (s/multi-spec field-form :type))

;; A group — the control flow above the fields
;;
;; A request carries a TREE of three kinds of node, not a list of ten kinds of
;; field: a group ARRANGES children, a decoration is READ, a field HOLDS one
;; value. [[::node]] is where that choice is made — once, on the way in — and the
;; only place the three contracts meet.

(defn- grouped?
  "A group says so in its own `:type`. Every surface walks the tree by that key,
   so a layout node that lost it is drawn as a field."
  [{:keys [type]}]
  (= group-type type))

(s/def ::group
  (s/and #(closed? group-keys %)
         grouped?
         (s/keys :req-un [::id ::name ::direction ::fields] :opt-un [::label ::description])
         one-identity?))

;; A decoration — ink that answers nothing

(s/def ::decor
  ;; No `:id` and no `:name`: a decoration is never keyed, so it carries its own
  ;; type and the words it paints, and refusing everything else is what keeps a
  ;; disabled-looking field out of the vocabulary of answers.
  (s/and #(closed? decor-keys %)
         decoration?
         (s/keys :req-un [::text])))

;; A node — the one place the three contracts meet

(defmulti ^:private node-form
  "Which of the three contracts one node of the field tree answers to."
  :type)

(defmethod node-form group-type [_] ::group)

;; The decoration vocabulary is [[decor-types]] and lives in ONE place, so the
;; fallback asks it instead of listing `:heading` and `:paragraph` again here.
(defmethod node-form :default [node] (if (decoration? node) ::decor ::field))

(s/def ::node (s/multi-spec node-form :type))

(s/def ::fields (s/and (s/coll-of ::node :kind vector?) non-empty?))

;; A request

(s/def ::title non-blank-string?)
(s/def ::source non-blank-string?)
(s/def ::session-id non-blank-string?)
(s/def ::submit-label non-blank-string?)
(s/def ::cancel-label non-blank-string?)
(s/def ::is-cancellable boolean?)
(s/def ::timeout-ms nat-int?)

(s/def ::channel-ids
  ;; One id twice would open the same dialog twice and answer it once.
  (s/and (s/coll-of keyword? :kind vector?)
         non-empty?
         all-distinct?))

(defn- field-names
  [fields]
  (mapcat (fn [node]
            ;; A decoration has no `:name` — nothing keys it — so it contributes
            ;; nothing to key uniqueness either.
            (cond-> (field-names (:fields node))
              (:name node)
              (conj (:name node))))
          fields))

(defn- distinct-names?
  "Names are the keys of the answer map, across groups too: a collision loses a
   value silently."
  [{:keys [fields]}]
  (let [names (field-names fields)]
    (= (count names) (count (set names)))))

(s/def ::request
  (s/and #(closed? request-keys %)
         (s/keys :req-un [::id ::title ::fields ::submit-label ::cancel-label ::is-cancellable
                          ::timeout-ms ::channel-ids]
                 :opt-un [::description ::source ::session-id])
         distinct-names?))

;; An answer

(s/def ::is-submitted boolean?)
(s/def ::reason non-blank-string?)
(s/def ::request-id non-blank-string?)

(s/def ::answer-value
  (s/nilable (s/or :text string?
                   :flag boolean?
                   :number number?
                   :choices (s/coll-of string? :kind vector?))))

(s/def ::values (s/map-of non-blank-string? ::answer-value))

(defn- values-iff-submitted?
  "`:values` is the proof the human answered. A cancelled or timed-out request
   carries none, so a caller cannot read a half-filled form as a submission."
  [{:keys [is-submitted values]}]
  (if is-submitted (map? values) (nil? values)))

(s/def ::answer
  (s/and #(closed? answer-keys %)
         (s/keys :req-un [::is-submitted ::reason ::request-id] :opt-un [::values])
         values-iff-submitted?))

;; An answered VALUE — the field's own domain
;;
;; [[::answer-value]] says what a value may LOOK like; these say what it may BE
;; for the field that asked the question. Coercion is hand-written per type and
;; nothing declared what it produces, so a `:select` could come back on an
;; option nobody offered, a slider outside its own track, a `:password` as the
;; plaintext a transcript must never hold. Each domain is built FROM the field,
;; so the declaration cannot drift from the question.

(defn- typed-in-domain?
  "Blank is how a surface says the human left a text field alone, and only an
   OPTIONAL field may be left alone. A value that IS there fits the declared
   length bounds — a blank one is not a short one, exactly as the parser reads
   it."
  [{:keys [is-required min-length max-length]} value]
  (cond (nil? value) (not is-required)
        (str/blank? value) (not is-required)
        :else (and (or (nil? min-length) (<= (long min-length) (count value)))
                   (or (nil? max-length) (<= (count value) (long max-length))))))

(defn- secret-in-domain?
  "A `:password` and an `:otp` both answer with a vault HANDLE. The plaintext has
   no business in an answer map at all, and the length bounds — or the digit
   width of a code — describe what the human typed rather than the handle it
   became."
  [{:keys [is-required]} value]
  (if (nil? value) (not is-required) (secret-handle? value)))

(defn- selected-in-domain?
  [{:keys [is-required] :as field} value]
  (if (nil? value) (not is-required) (contains? (option-values field) value)))

(defn- picked-in-domain?
  "Every pick is an option that exists, named once; a required multiselect needs
   at least one of them."
  [{:keys [is-required] :as field} value]
  (let [chosen (option-values field)]
    (and (every? chosen value)
         (= (count value) (count (set value)))
         (or (non-empty? value) (not is-required)))))

(defn- ticked-in-domain?
  "A required checkbox is a consent box: `false` is not an answer to it."
  [{:keys [is-required]} value]
  (or (true? value) (not is-required)))

(defn- slid-in-domain? [{lo :min hi :max} value] (<= (double lo) (double value) (double hi)))

(defmulti ^:private answer-form
  "The form ONE answered value must take, derived from the very field that asked
   for it: its own options, its own track, its own width."
  :type)

(defmethod answer-form :plaintext
  [field]
  (s/and (s/nilable string?) (partial typed-in-domain? field)))

(defmethod answer-form :multiline
  [field]
  (s/and (s/nilable string?) (partial typed-in-domain? field)))

(defmethod answer-form :password
  [field]
  (s/and (s/nilable string?) (partial secret-in-domain? field)))

(defmethod answer-form :select
  [field]
  (s/and (s/nilable string?) (partial selected-in-domain? field)))

(defmethod answer-form :multiselect
  [field]
  (s/and (s/coll-of string? :kind vector?) (partial picked-in-domain? field)))

(defmethod answer-form :checkbox [field] (s/and boolean? (partial ticked-in-domain? field)))

(defmethod answer-form :range [field] (s/and number? (partial slid-in-domain? field)))

(defmethod answer-form :otp [field] (s/and (s/nilable string?) (partial secret-in-domain? field)))

(defn- answerable
  "Every field that holds an ANSWER. A group is control flow: it never appears in
   a values map, its children answer in its place. A decoration answers nothing
   at all and has no children to answer for it."
  [fields]
  (mapcat (fn [{:keys [type] :as node}]
            (cond (= group-type type) (answerable (:fields node))
                  (decoration? node) []
                  :else [node]))
          fields))

;; Explaining a violation

(defn- brief
  [x]
  (let [text (pr-str x)]
    (if (> (count text) 88) (str (subs text 0 88) "…") text)))

(defn- problem-str
  [{:keys [in val pred]}]
  (str (when (seq in) (str (str/join " -> " (map brief in)) ": "))
       (brief val)
       " fails "
       (brief pred)))

(defn- error
  "nil when `x` satisfies `spec`, else a one-line reason naming the first
   couple of problems — short enough to ride in an error message a human reads
   in a dialog."
  [spec x]
  (when-let [problems (::s/problems (s/explain-data spec x))]
    (str/join "; " (map problem-str (take 2 problems)))))

(defn field-error
  "nil when `field` is a legal normalized FIELD — a leaf holding one answer —
   else why it is not. A layout group is not a field ([[group-error]]) and
   neither is a decoration ([[decor-error]])."
  [field]
  (error ::field field))

(defn group-error
  "nil when `group` is a legal normalized layout group, else why it is not. The
   children it arranges are checked as the nodes they are, so one call covers
   the whole subtree."
  [group]
  (error ::group group))

(defn decor-error
  "nil when `decor` is a legal normalized decoration, else why it is not. The
   three node contracts are checked apart, so each refuses the other two: a
   heading carrying a `:default` is a spec that meant to ask something."
  [decor]
  (error ::decor decor))

(defn request-error
  "nil when `request` is a legal normalized request, else why it is not."
  [request]
  (error ::request request))

(defn- values-error
  "nil when `values` answers EXACTLY the answerable `fields` — no field left out,
   none invented — with every value inside the domain its own field declared."
  [fields values]
  (let
    [by-name
     (into {} (map (juxt :name identity)) (answerable fields))

     answered
     (set (keys values))]

    (or (when-let [extra (seq (sort (remove by-name answered)))]
          (str "answers no such field: " (str/join ", " extra)))
        (when-let [missing (seq (sort (remove answered (keys by-name))))]
          (str "leaves unanswered: " (str/join ", " missing)))
        (some (fn [[field-name value]]
                (when-let [why (error (answer-form (get by-name field-name)) value)]
                  (str field-name ": " why)))
              values))))

(defn answer-error
  "nil when `answer` is a legal answer to hand a blocked extension, else why it
   is not.

   `fields` are the fields of the request being answered: a SUBMITTED answer is
   checked against them too, so a value can only reach an extension for a field
   that asked for it and only inside that field's own domain. nil `fields` means
   the request settled while this answer was in flight — nobody is left to read
   it, so its own shape is all there is to check."
  [fields answer]
  (or (error ::answer answer)
      (when (and (seq fields) (:is-submitted answer)) (values-error fields (:values answer)))))
