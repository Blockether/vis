(ns com.blockether.vis.internal.human-input
  "Typed human-input requests — the pause/resume primitive an extension uses to
   ask the operator for structured values in the middle of a run.

   An extension calls [[request!]] with a title and a vector of typed fields.
   The call BLOCKS the calling thread, publishes a `:human-input/request`
   channel event so the mounted channel can draw a dialog, and returns once a
   channel calls [[submit!]] / [[cancel!]], or the request runs out of time.

   A request either carries a DEADLINE (`:timeout-ms`, five minutes by default)
   or waits INDEFINITELY (`:timeout-ms` 0) — an extension that must not guess an
   answer says so and parks until the human is back at the keyboard, while one
   that can carry on alone names the wait it is willing to bill and gets a
   `timeout` answer when nobody came. Even an indefinite request cannot park a
   thread nobody can release: a request that reaches no mounted channel is
   answered `undeliverable` at once, and interrupting the surrounding turn
   cancels it.

   This namespace PARSES: it takes either spelling of every key, looks an
   extension-supplied type name up in a CLOSED vocabulary
   ([[com.blockether.vis.internal.human-input.spec/field-types]]) instead of
   `keyword`-minting it, and names the key an author has to fix when it cannot.
   What it produces is DECLARED by `clojure.spec` in
   `com.blockether.vis.internal.human-input.spec`: every normalized field, every
   normalized request, and every answer handed back to a blocked extension — its
   VALUES included, each against the domain the field that asked for it declared
   — is checked against that contract, so an engine bug surfaces here instead of
   as a half-built dialog three namespaces away. Coercion and validation live in
   one place, so the value an extension receives already matches the declared
   type: a `:checkbox` yields a boolean, a `:multiselect` a vector of declared
   option values, a `:select` one declared option value.

   Secrets never travel as plaintext. A `:password` and an `:otp` field both
   resolve to an opaque
   `vis-secret:<uuid>` handle; the plaintext stays in a process-local vault and
   is readable only through [[reveal-secret]] from the trusted extension side.
   Handles are what land in logs, transcripts and wire payloads, so a leaked
   event is worthless."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as channel-events]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.human-input.live :as live]
            [com.blockether.vis.internal.human-input.live-sink :as live-sink]
            [com.blockether.vis.internal.human-input.spec :as hi-spec]
            [com.blockether.vis.internal.human-input.validation :as validation]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

(def default-timeout-ms
  "How long a request waits when its spec says nothing: five minutes — long
   enough for a human who is reading, short enough that a dialog nobody noticed
   does not park an extension all afternoon. A caller who wants another budget
   names its own `:timeout-ms`, or [[no-timeout-ms]] to wait as long as it takes."
  300000)

(def no-timeout-ms
  "The `:timeout-ms` that means NO deadline at all: [[request!]] parks until a
   human answers, a surface cancels, or the surrounding turn is interrupted.

   Nothing infers it. A spec asks for it explicitly with `timeout_ms` 0, so a
   spec that merely FORGOT the key still expires at [[default-timeout-ms]]
   instead of silently pinning the run on an operator who walked away."
  0)

(defn indefinite-timeout?
  "True when `timeout-ms` is [[no-timeout-ms]]: the request waits indefinitely."
  [timeout-ms]
  (zero? (long (or timeout-ms no-timeout-ms))))

(defonce ^:private pending (atom {}))

(defonce ^:private secrets (atom {}))

;; Secret vault

(def ^:private max-secrets
  "The vault only has to bridge a submitted secret to the extension that asked
   for it. Keeping it bounded means a long session cannot accumulate plaintext
   forever when an extension forgets to call [[forget-secret!]]."
  128)

(defn- evict-oldest
  [vault]
  (if (<= (count vault) (long max-secrets))
    vault
    (dissoc vault (key (apply min-key #(:at (val %)) (seq vault))))))

(defn- stash-secret!
  [value]
  (let [handle (str hi-spec/secret-handle-prefix (random-uuid))]
    (swap! secrets #(evict-oldest (assoc % handle {:value value :at (System/nanoTime)})))
    handle))

(defn reveal-secret
  "Return the plaintext behind a `vis-secret:` handle, or nil when the handle is
   unknown/forgotten. Trusted-side only: never hand the result to a channel, a
   log, or the model."
  [handle]
  (:value (get @secrets handle)))

(defn forget-secret!
  "Drop the plaintext behind `handle`. Returns true when something was dropped."
  [handle]
  (let [[old _] (swap-vals! secrets dissoc handle)]
    (contains? old handle)))

(defn forget-secrets!
  "Empty the vault. Returns how many plaintexts were dropped."
  []
  (let [[old _] (reset-vals! secrets {})]
    (count old)))

;; Normalization — request/field specs

(defn- pick
  "First non-nil value among `ks`. Specs arrive either string-keyed (from the
   Python/wire boundary) or kebab-keyword-keyed (from Clojure callers).

   `false` is a VALUE, not a miss: `some` would treat it as one and fall through
   to the default, which silently turned `:is-cancellable false` — an extension
   demanding an answer — into a dismissable request on every surface."
  [m & ks]
  (reduce (fn [_ k]
            (let [v (get m k)]
              (if (some? v) (reduced v) nil)))
          nil
          ks))

(defn- trimmed
  "`value` as trimmed non-blank text, or nil.

   A collection is never text: a map or vector would `str` into Clojure source
   no operator can read, so a name, label or description that arrives as one is
   dropped exactly like a blank."
  [value]
  (when-not (coll? value)
    (some-> value
            str
            str/trim
            not-empty)))

(defn- invalid-field!
  [field-id message]
  (throw (ex-info (str "Invalid human-input field" (when field-id (str " " field-id)) ": " message)
                  {:type :vis/human-input-invalid-field :field-id field-id :reason message})))

(defn- invalid-request!
  [message]
  (throw (ex-info (str "Invalid human-input request: " message)
                  {:type :vis/human-input-invalid-request :reason message})))

(defn- invalid-answer!
  [request-id message]
  (throw (ex-info (str "Invalid human-input answer for " request-id ": " message)
                  {:type :vis/human-input-invalid-answer :request-id request-id :reason message})))

(defn- checked-field
  "`field` once it satisfies the declared contract, else a refusal naming it. The
   parsing below refuses bad INPUT key by key; this refuses a normalized form no
   surface could paint, whoever built it."
  [field-id field]
  (if-let [why (hi-spec/field-error field)]
    (invalid-field! field-id why)
    field))

(defn- checked-group
  "`group` once it satisfies the declared contract for a LAYOUT node, not a
   field's. A group answers nothing, so what it must have is different: children,
   a direction they run in, and no key that describes a value."
  [field-id group]
  (if-let [why (hi-spec/group-error group)]
    (invalid-field! field-id why)
    group))

(defn- checked-decor
  "`decor` once it satisfies the declared contract for a DECORATION. It answers
   nothing and arranges nothing, so the only thing it must have is the words it
   paints — and it must carry nothing that would make a surface go looking for a
   value it can never hold."
  [decor]
  (if-let [why (hi-spec/decor-error decor)]
    (invalid-field! nil why)
    decor))

(defn- checked-request
  [request]
  (if-let [why (hi-spec/request-error request)]
    (invalid-request! why)
    request))

(defn- checked-answer
  "The answer a blocked extension is about to receive. A caller reads
   `:is-submitted`, `:reason` and `:values` without asking whether they are
   there, so an answer missing one never leaves the engine.

   `fields` are the fields of the request being answered, so a submitted answer
   is also checked against the questions it answers: no field invented, none
   dropped, every value inside its own field's domain, and a `:password` or an
   `:otp` as the vault HANDLE rather than the plaintext."
  [request-id fields answer]
  (if-let [why (hi-spec/answer-error fields answer)]
    (invalid-answer! request-id why)
    answer))

(defn- snake-key
  "`k` as the canonical snake_case name it is reaching for: `:is-required`,
   `\"is-required\"` and `\"isRequired\"` all canonicalize to `\"is_required\"`."
  [k]
  (-> (if (keyword? k) (subs (str k) 1) (str k))
      (str/replace #"([a-z0-9])([A-Z])" "$1_$2")
      (str/replace "-" "_")
      str/lower-case))

(defn- kebab-key [canonical] (str/replace canonical "_" "-"))

(defn- wire-keys
  "The spec vocabulary `ks` in the canonical snake_case spelling a Python/JSON
   spec writes. The keys a parser accepts are exactly the ones
   [[com.blockether.vis.internal.human-input.spec]] declares — deriving them here
   is what keeps the wire from growing a second copy of that table."
  [ks]
  (into #{} (map snake-key) ks))

(def ^:private field-keys "Every key a VALUE field spec may carry." (wire-keys hi-spec/field-keys))

(def ^:private layout-keys
  "The keys only a `group` has. A field that holds an ANSWER carrying one of them
   is a spec that meant to group and forgot to say so: dropping the key in
   silence drew the form flat and sent the author hunting for a layout bug in the
   surfaces, so it is refused with the fix in the message."
  (wire-keys hi-spec/layout-keys))

(def ^:private group-keys "Every key a `group` may carry." (wire-keys hi-spec/group-keys))

(def ^:private decor-keys "Every key a decoration may carry." (wire-keys hi-spec/decor-keys))

(def ^:private option-keys
  "Every key one `:options` entry may carry."
  (wire-keys hi-spec/option-keys))

(def ^:private request-keys
  "Every key a request spec may carry. `channel_id` is the singular spelling of
   `channel_ids` — a one-channel convenience, and the only wire key with no
   counterpart in the normalized form."
  (conj (wire-keys hi-spec/request-keys) "channel_id"))

(defn- accepted-spelling?
  "Two spellings, one meaning: the snake_case STRING a Python/JSON spec writes,
   or the kebab-case KEYWORD a Clojure caller writes. Nothing else."
  [k canonical]
  (cond (string? k) (= k canonical)
        (keyword? k) (and (nil? (namespace k)) (= (name k) (kebab-key canonical)))
        :else false))

(defn- check-keys!
  "Refuse a spec key that is not in `allowed`, or one spelled any way other than
   the snake_case string / kebab-case keyword pair.

   Silence was the bug: `{'isRequired': True}` from a Python extension parsed as
   clean JSON, matched nothing, and left a mandatory field optional on every
   surface — the human simply skipped it. A misspelled key now names its own
   fix instead of disappearing."
  [what allowed m fail!]
  (doseq
    [k
     (keys m)

     :let [canonical
           (snake-key k)]]

    (cond (not (contains? allowed canonical)) (fail! (str "unknown " what
                                                          " key " (pr-str k)
                                                          " — expected one of "
                                                          (str/join ", " (sort allowed))))
          (not (accepted-spelling? k canonical)) (fail! (str what
                                                             " key "
                                                             (pr-str k)
                                                             " is misspelled — write \""
                                                             canonical
                                                             "\" (Python/JSON) or :"
                                                             (kebab-key canonical)
                                                             " (Clojure)")))))

(defn- normalize-option
  [field-id option]
  (when (map? option) (check-keys! "option" option-keys option #(invalid-field! field-id %)))
  (let
    [[value label]
     (if (map? option) [(pick option "value" :value) (pick option "label" :label)] [option option])

     value
     (trimmed value)]

    (when-not value (invalid-field! field-id "option values must be non-blank"))
    {:value value :label (or (trimmed label) value)}))

(defn- normalize-options
  [field-id field-type raw]
  (when-not (sequential? raw)
    (invalid-field! field-id (str (name field-type) " needs an :options sequence")))
  (let [options (mapv #(normalize-option field-id %) raw)]
    (when (empty? options)
      (invalid-field! field-id (str (name field-type) " needs at least one option")))
    (when-not (apply distinct? (map :value options))
      (invalid-field! field-id "option values must be distinct"))
    options))

(defn- bool-value
  [fail! label value fallback]
  (cond (nil? value) fallback
        (boolean? value) value
        (= "true" (str value)) true
        (= "false" (str value)) false
        :else (fail! (str label " must be a boolean"))))

(defn- normalize-bool
  [field-id label value fallback]
  (bool-value #(invalid-field! field-id %) label value fallback))

(defn- normalize-length
  "A `:min_length`/`:max_length` character count. `label` is the key the spec
   actually wrote, so a refusal names the key the author has to fix."
  [field-id label value]
  (when (some? value)
    (let [n (if (number? value) value (parse-long (str value)))]
      (when-not (and (number? n) (integer? n) (pos? (long n)))
        (invalid-field! field-id (str label " must be a positive whole number")))
      (long n))))

(defn- normalize-number
  [field-id label value fallback]
  (cond (nil? value) fallback
        (number? value) value
        :else (let [s (str/trim (str value))]
                (or (parse-long s)
                    (parse-double s)
                    (invalid-field! field-id (str label " must be a number"))))))

(defn- normalize-range
  "The three numbers a slider needs. `:step` is the increment a surface nudges
   by, NOT a validation rule: the engine only refuses a value outside the
   bounds, so a client that types an exact number is never argued with."
  [field-id field]
  (let
    [lo
     (normalize-number field-id ":min" (pick field "min" :min) (:min hi-spec/range-defaults))

     hi
     (normalize-number field-id ":max" (pick field "max" :max) (:max hi-spec/range-defaults))

     step
     (normalize-number field-id ":step" (pick field "step" :step) (:step hi-spec/range-defaults))]

    (when-not (< (double lo) (double hi))
      (invalid-field! field-id ":max must be greater than :min"))
    (when-not (pos? (double step)) (invalid-field! field-id ":step must be positive"))
    {:min lo :max hi :step step}))

(defn- normalize-otp
  "How many digits the boxes hold. `:min_length` defaults to `:max_length`, so a
   plain `otp` field is the fixed six-digit code everybody means; giving both
   makes the code variable-length, which is what a spec asks for when the sender
   is not under its control."
  [field-id field]
  (let
    [hi
     (or (normalize-length field-id ":max_length" (pick field "max_length" :max-length))
         (long (:length hi-spec/otp-defaults)))

     lo
     (or (normalize-length field-id ":min_length" (pick field "min_length" :min-length)) hi)]

    (when (> (long lo) (long hi))
      (invalid-field! field-id ":max_length must be at least :min_length"))
    (when (> (long hi) (long (:ceiling hi-spec/otp-defaults)))
      (invalid-field!
        field-id
        (str ":max_length must be at most " (:ceiling hi-spec/otp-defaults) " digits")))
    {:min-length lo :max-length hi}))

(defn- normalize-direction
  [field-id value]
  (let [name' (str/lower-case (or (trimmed value) "column"))]
    (or (get hi-spec/group-directions name')
        (invalid-field! field-id
                        (str "unknown :direction " (pr-str name')
                             " — expected one of "
                             (str/join ", " (sort (keys hi-spec/group-directions))))))))

(declare coerce-value normalize-node)

(defn- group-node?
  "True when this RAW spec node asks to be a layout group. Decided from the type
   name alone, and decided FIRST: which keys are legal, whether a `:name` is
   required and whether the node may hold children all follow from the answer."
  [node]
  (= hi-spec/group-type-name (str/lower-case (or (trimmed (pick node "type" :type)) ""))))

(defn- decor-type
  "The internal DECORATION type this RAW spec node asks for, or nil when it asks
   for something answerable. Read from the type name alone and read FIRST, like
   [[group-node?]]: a heading is ink on the form, so it never walks a value path."
  [node]
  (get hi-spec/decor-types (str/lower-case (or (trimmed (pick node "type" :type)) ""))))

(defn normalize-field
  "Validate one FIELD spec — a leaf holding exactly one answer — and return its
   internal form. Throws `ex-info` with `:type :vis/human-input-invalid-field`
   on a bad spec.

   Three names, three jobs, and every field ends up with all three:

     - `:name` is how the answer is KEYED — the key the extension reads back out
       of `:values` (`:id` is the historical alias, accepted and still emitted).
     - `:label` is how the field is SHOWN. Never blank: a field without one
       shows its `:name`, so no surface ever draws a bare, unlabelled input.
     - `:description` is the prose under that label, rendered in italic by every
       dialog.

   A layout group is not a field and never arrives here: [[normalize-node]]
   routes it to [[normalize-group]] before a single value key is parsed."
  [field]
  (when-not (map? field) (invalid-field! nil "field must be a map"))
  (when (group-node? field)
    (invalid-field! (trimmed (pick field "name" :name "id" :id))
                    (str "a group is not a field — it holds no answer, only the fields it"
                         " arranges. Normalize a node of the tree with normalize-node.")))
  (when-let [decor (decor-type field)]
    (invalid-field! (trimmed (pick field "name" :name "id" :id))
                    (str "a "
                         (name decor)
                         " is a decoration, not a field — it holds no answer,"
                         " only the words it paints. Normalize a node of the tree with"
                         " normalize-node.")))
  (let
    [field-id
     (trimmed (pick field "name" :name "id" :id))

     _
     ;; Layout keys are refused BEFORE the generic key check so the message can
     ;; say what the author actually meant: `fields` on a `plaintext` is a group
     ;; that forgot its `:type`, not a misspelling.
     (when-let [k (first (sort (filter layout-keys (map snake-key (keys field)))))]
       (invalid-field! field-id
                       (str "key \""
                            k
                            "\" only exists on a group — a field that holds an answer has"
                            " nothing to lay out. Put these fields inside"
                            " {\"type\": \"group\", \"direction\": \"row\"} instead.")))

     _
     (check-keys! "field" field-keys field #(invalid-field! field-id %))

     _
     (when-not field-id (invalid-field! nil "field needs a non-blank :name"))

     type-name
     (or (trimmed (pick field "type" :type)) "plaintext")

     field-type
     (get hi-spec/field-types (str/lower-case type-name))

     _
     (when-not field-type
       (invalid-field! field-id
                       (str "unknown type " (pr-str type-name)
                            " — expected one of " (str/join ", "
                                                            (sort (keys hi-spec/field-types))))))]

    (checked-field
      field-id
      (let
        [description
         (trimmed (pick field "description" :description))

         ;; An `:otp` derives its own lengths from the same two keys — how many
         ;; boxes it draws IS its length — so it must not be length-checked twice.
         min-length
         (when-not (= :otp field-type)
           (normalize-length field-id ":min_length" (pick field "min_length" :min-length)))

         max-length
         (when-not (= :otp field-type)
           (normalize-length field-id ":max_length" (pick field "max_length" :max-length)))

         validate
         (validation/normalize-validators (pick field "validate" :validate)
                                          #(invalid-field! field-id %))

         spec
         (cond->
           {:id field-id
            ;; The same string under both keys: `:name` is the contract a spec
            ;; writes, `:id` is what every surface has always keyed rows and errors
            ;; by. One field identity, two spellings, no drift between them.
            :name field-id
            :type field-type
            :label (or (trimmed (pick field "label" :label)) field-id)
            ;; Optional unless the caller says otherwise — the same default every
            ;; form API has, so a spec never blocks a human on a field the
            ;; extension did not actually need.
            :is-required
            (normalize-bool field-id ":is-required" (pick field "is_required" :is-required) false)
            :is-secret (contains? hi-spec/secret-types field-type)}
           description
           (assoc :description description)

           (trimmed (pick field "placeholder" :placeholder))
           (assoc :placeholder (trimmed (pick field "placeholder" :placeholder)))

           (contains? hi-spec/choice-types field-type)
           (assoc :options (normalize-options field-id field-type (pick field "options" :options)))

           (= :range field-type)
           (merge (normalize-range field-id field))

           (= :otp field-type)
           (merge (normalize-otp field-id field))

           min-length
           (assoc :min-length min-length)

           max-length
           (assoc :max-length max-length)

           (seq validate)
           (assoc :validate validate))

         raw-default
         (pick field "default" :default)

         [status default]
         (coerce-value (assoc spec :is-required false) raw-default)]

        (when (= :error status) (invalid-field! field-id (str "invalid :default — " default)))
        (cond-> spec
          (some? default)
          (assoc :default default))))))

(defn- normalize-group
  "Validate a layout `group` and return its internal form.

   A group answers nothing: it has `:fields` of its own and a `:direction` they
   run in, and because a child may itself be a group, `row` and `column` compose
   into any arrangement without a single new key. Its `:name` is optional; when
   the spec does not give one it is derived from the children, so a surface
   still has a stable key to draw rows under and no author has to invent an
   identifier for a box that only exists to hold two fields side by side."
  [group]
  (let [field-id (trimmed (pick group "name" :name "id" :id))]
    (check-keys! "group" group-keys group #(invalid-field! field-id %))
    (let
      [raw (pick group "fields" :fields)
       _ (when-not (sequential? raw) (invalid-field! field-id "group needs a :fields sequence"))
       children (mapv normalize-node raw)
       _ (when (empty? children) (invalid-field! field-id "group needs at least one field"))
       id (or field-id (str "group:" (str/join "+" (map :name children))))
       description (trimmed (pick group "description" :description))]

      (checked-group id
                     (cond->
                       {:id id
                        :name id
                        :type hi-spec/group-type
                        :direction (normalize-direction field-id
                                                        (pick group "direction" :direction))
                        :fields children}
                       (trimmed (pick group "label" :label))
                       (assoc :label (trimmed (pick group "label" :label)))

                       description
                       (assoc :description description))))))

(defn- normalize-decor
  "Validate a DECORATION — a heading or a paragraph — and return its internal
   form. Throws `ex-info` with `:type :vis/human-input-invalid-field` on a bad
   spec.

   The whole node is two things: which decoration it is, and the words it
   paints. It has no `:name`, which is the point — there is no identity, so it
   is never keyed, never focused, never in an answer map, and two paragraphs
   saying the same thing are two decorations rather than a name collision."
  [decor]
  (let [type (decor-type decor)]
    (check-keys! (name type) decor-keys decor #(invalid-field! nil %))
    (let [text (trimmed (pick decor "text" :text))]
      (when-not text
        (invalid-field! nil (str "a " (name type) " must carry :text — the words it paints")))
      (checked-decor {:type type :text text}))))

(defn normalize-node
  "Validate one node of a request's field TREE and return its internal form.

   This is the fork the whole shape hangs on, and it is taken ONCE, up here: a
   `group` is control flow — it arranges the children below it and holds no
   answer — a `heading` or a `paragraph` is pure decoration that neither asks
   nor arranges, and anything else is a field holding exactly one answer.
   Deciding it above the three normalizers is why [[normalize-field]] never has
   to ask whether it is really layout or ink, and why no value path below
   carries a branch for a node that can never take one."
  [node]
  (when-not (map? node) (invalid-field! nil "field must be a map"))
  (cond (group-node? node) (normalize-group node)
        (decor-type node) (normalize-decor node)
        :else (normalize-field node)))

(defn- leaves!
  "Conjoin every ANSWERABLE field of `fields` onto TRANSIENT vector `acc`, depth
   first. A group is walked through and a decoration is dropped — neither holds
   a value, so neither can key `:values`. The accumulator is threaded through
   the recursion, so a tree of any depth is flattened in one pass onto one array
   — where the `mapcat` shape this replaces allocated a lazy seq and a fresh
   vector for every group it entered."
  [acc fields]
  (reduce (fn [acc {:keys [type] :as field}]
            (cond (= hi-spec/group-type type) (leaves! acc (:fields field))
                  (hi-spec/decoration? field) acc
                  :else (conj! acc field)))
          acc
          fields))

(defn input-fields
  "Every ANSWERABLE field in `fields`, depth-first in the order a surface draws
   them. A group carries no value, so it is walked through and never returned:
   this is the sequence that keys `:values`, and the reason a layout change can
   never change an extension's answer map.

   The hot path of the whole module — every keystroke on every surface
   re-validates through here — so it is a transient walk, not a lazy one."
  [fields]
  (persistent! (leaves! (transient []) fields)))

(defn- nodes!
  "[[leaves!]] for the WHOLE tree: every group is conjoined before the children
   it owns, on the same transient accumulator."
  [acc fields]
  (reduce (fn [acc {:keys [type] :as field}]
            (if (= hi-spec/group-type type)
              (nodes! (conj! acc field) (:fields field))
              (conj! acc field)))
          acc
          fields))

(defn- all-fields
  "Every node in the tree, groups included — what name uniqueness is checked on."
  [fields]
  (persistent! (nodes! (transient []) fields)))

(defn- map-fields
  "Rewrite every node of the tree with `f`, children before their group."
  [f fields]
  (mapv (fn [{:keys [type] :as field}]
          (f (cond-> field
               (= hi-spec/group-type type)
               (update :fields #(map-fields f %)))))
        fields))

(defn- ambient-session-id
  "Session id of the extension environment currently executing, or nil.

   A request raised inside a gateway session has to NAME that session: the
   gateway bridge turns the request into a session event so the companion app
   learns the run is blocked, and a session event with no session has nowhere
   to go. Resolved late and defensively — `human-input` must stay loadable
   (and testable) without the extension runtime."
  []
  (try (when-let [v (resolve 'com.blockether.vis.internal.extension/*current-environment*)]
         (let
           [env (var-get v)
            env (if (instance? clojure.lang.IDeref env) (deref env) env)]

           (when (map? env) (trimmed (:session-id env)))))
       (catch Throwable _ nil)))

(defn- normalize-channel-ids
  [request fail!]
  (let
    [ids
     (pick request "channel_ids" :channel-ids "channel_id" :channel-id)

     ids
     ;; Both surfaces by default: the TUI draws its dialog, and the gateway
     ;; bridge turns the same event into a session event + push alert so a
     ;; companion-app operator is not left staring at a stalled run.
     (cond (nil? ids) [:tui :app]
           (keyword? ids) [ids]
           (sequential? ids) (vec ids)
           :else (fail! ":channel-ids must be a keyword or a sequence of keywords"))]

    (when (empty? ids) (fail! ":channel-ids must not be empty"))
    (when-not (every? keyword? ids) (fail! ":channel-ids must be keywords"))
    ids))

(defn- normalize-timeout
  "The deadline in milliseconds, or [[no-timeout-ms]] when the wait is
   indefinite.

   A missing key means `fallback`: five minutes for a request a human owes an
   answer to, none for a live view whose work takes as long as the work takes.
   `0` is the one way to ask for no deadline; a negative number is refused.
   Nothing is CLAMPED — a caller who wants to wait all day says 0 and means it,
   so quietly shortening a stated budget would only lie about when the answer
   arrives."
  [spec fallback fail!]
  (let
    [raw
     (pick spec "timeout_ms" :timeout-ms)

     ms
     (if (nil? raw)
       fallback
       (or (if (number? raw) (long raw) (parse-long (str raw)))
           (fail! ":timeout-ms must be a number of milliseconds, or 0 to wait indefinitely")))]

    (when (neg? (long ms)) (fail! ":timeout-ms must not be negative — 0 waits indefinitely"))
    (long ms)))

(defn normalize-request
  "Validate a human-input request spec and return its internal form. Throws
   `ex-info` with `:type :vis/human-input-invalid-request` (or
   `:vis/human-input-invalid-field`) on a bad spec."
  [request]
  (when-not (map? request) (invalid-request! "request must be a map"))
  (check-keys! "request" request-keys request invalid-request!)
  (let
    [title
     (trimmed (pick request "title" :title))

     _
     (when-not title (invalid-request! "request needs a non-blank :title"))

     raw-fields
     (pick request "fields" :fields)

     _
     (when-not (sequential? raw-fields) (invalid-request! ":fields must be a sequence"))

     fields
     (mapv normalize-node raw-fields)

     _
     (when (empty? fields) (invalid-request! ":fields must not be empty"))

     _
     (let [names (into [] (keep :name) (all-fields fields))]
       (when-not (or (empty? names) (apply distinct? names))
         (invalid-request! "field names must be distinct")))

     session-id
     (or (trimmed (pick request "session_id" :session-id)) (ambient-session-id))]

    (checked-request
      (cond->
        {:id (or (trimmed (pick request "id" :id)) (str (random-uuid)))
         :title title
         :fields fields
         :submit-label (or (trimmed (pick request "submit_label" :submit-label)) "Submit")
         :cancel-label (or (trimmed (pick request "cancel_label" :cancel-label)) "Cancel")
         :is-cancellable
         (normalize-bool nil ":is-cancellable" (pick request "is_cancellable" :is-cancellable) true)
         :timeout-ms (normalize-timeout request default-timeout-ms invalid-request!)
         :channel-ids (normalize-channel-ids request invalid-request!)}
        session-id
        (assoc :session-id session-id)

        (trimmed (pick request "description" :description))
        (assoc :description (trimmed (pick request "description" :description)))

        (trimmed (pick request "source" :source))
        (assoc :source (trimmed (pick request "source" :source)))))))

;; A live view — the second kind of interaction
;;
;; The same parser, deliberately: closed tables looked up rather than keywords
;; minted, either spelling of every key, blanks dropped before the materializer
;; ever sees them. What differs is TIME. A form is answered once, so its shape is
;; its whole life; a view is DECLARED once and then patched, so every node
;; carries an `:id` that patches ADDRESS it by.

(defn- invalid-live-view!
  [message]
  (throw (ex-info (str "Invalid live view: " message)
                  {:type :vis/human-input-invalid-live-view :reason message})))

(defn- invalid-live-patch!
  [message]
  (throw (ex-info (str "Invalid live-view patch: " message)
                  {:type :vis/human-input-invalid-patch :reason message})))

(defn- checked-live-view
  [view]
  (if-let [why (hi-spec/live-view-error view)]
    (invalid-live-view! why)
    view))

(defn- checked-live-node
  "`node` once it satisfies the node contract, else the reason it does not,
   refused by whoever was declaring it."
  [fail! node]
  (if-let [why (hi-spec/live-node-error node)]
    (fail! why)
    node))

(defn- checked-live-patch
  [patch]
  (if-let [why (hi-spec/live-patch-error patch)]
    (invalid-live-patch! why)
    patch))

(def ^:private live-view-decl-keys
  "Every key a live-view SPEC may write: the view's vocabulary minus the engine's
   own stamps, plus the singular `channel_id` [[normalize-channel-ids]] accepts."
  (conj (wire-keys (reduce disj hi-spec/live-view-keys hi-spec/live-view-stamp-keys)) "channel_id"))

(def ^:private live-node-decl-keys
  "Every key a NODE spec may write. `total_lines` is the engine's stamp on a log:
   the size of the RECORD is counted, never claimed. `direction` and `fields`
   belong to a layout GROUP and are refused here BY NAME, so a `status` written
   with children hears about them instead of having them dropped."
  (wire-keys (reduce disj hi-spec/live-node-keys #{:total-lines :direction :fields})))

(def ^:private live-group-decl-keys
  "Every key a live layout GROUP spec may write: its own vocabulary, so `lines` on
   a `row` is refused by name rather than quietly ignored."
  (wire-keys hi-spec/live-group-keys))

(def ^:private live-patch-decl-keys
  "The one key a patch spec may write. `view_id` and `seq` are stamps: a caller
   who could choose the seq could replay a patch, and one who could choose the
   view could patch somebody else's."
  (wire-keys (reduce disj hi-spec/live-patch-keys #{:view-id :seq})))

(def ^:private live-order-decl-keys
  "Every key a `{:by …}` table order may write."
  (wire-keys hi-spec/live-sorted-keys))

(def ^:private live-op-decl-keys
  "Every key ONE operation may write, per operation — the op's own `:op` chooses
   the set, so a `clear` carrying the lines it meant to `append` is refused
   instead of silently emptying the node."
  (update-vals hi-spec/live-op-key-sets wire-keys))

(def ^:private live-item-keys
  "Every key one item of a keyed collection may write, by the key holding them."
  {:columns (wire-keys hi-spec/live-column-keys)
   :rows (wire-keys hi-spec/live-row-keys)
   :stats (wire-keys hi-spec/live-stat-keys)
   :steps (wire-keys hi-spec/live-step-keys)
   :links (wire-keys hi-spec/live-link-keys)})

(def ^:private live-item-name
  "What one item of each keyed collection is CALLED, so a refusal names the thing
   the author wrote rather than the key it arrived under."
  {:columns "column" :rows "row" :stats "stat" :steps "step" :links "link"})

(defn- pick*
  "The value of canonical key `k` written either legal way: `\"window_lines\"`
   from the wire, `:window-lines` from Clojure."
  [m k]
  (pick m (snake-key k) k))

(defn- live-term
  "The entry `value` names in a CLOSED wire table, in either spelling. A term
   outside the table is refused NAMING the table: a minted keyword reaches a
   surface that has no way to paint it, and nothing downstream would ever say so."
  [fail! what table value]
  (or (when (or (keyword? value) (string? value)) (get table (name value)))
      (fail!
        (str what " must be one of " (str/join ", " (sort (keys table))) ", not " (pr-str value)))))

(defn- live-text
  [fail! what value]
  (or (trimmed value) (fail! (str what " must be non-blank text"))))

(defn- live-shown
  "One value AS SHOWN. A number or a keyword is rendered, because a stat's value
   and a table's cell are text on purpose — the extension chooses the units and
   the engine never formats. A collection is refused: it would `str` into
   Clojure source no human reads."
  [fail! what value]
  (cond (coll? value) (fail! (str what " is one text, not " (pr-str value)))
        (string? value) value
        (nil? value) (fail! (str what " must be text"))
        :else (str value)))

(defn- text-items
  "A sequence of TEXTS. nil is the empty text — a blank line IS a line, and a
   missing cell is an empty cell, not a hole in the row."
  [fail! what values]
  (when-not (sequential? values) (fail! (str what " must be a sequence of texts")))
  (mapv (fn [value]
          (if (nil? value) "" (live-shown fail! what value)))
        values))

(defn- live-long
  [fail! what value]
  (let
    [n (if (number? value)
         (when (== (double value) (Math/floor (double value))) (long value))
         (parse-long (str/trim (str value))))]
    (if (nil? n) (fail! (str what " must be a whole number")) (long n))))

(defn- live-fraction
  "A fraction of one. Absent is INDETERMINATE and stays absent: a job whose size
   nobody knows is not a job at zero percent, and the two paint differently."
  [fail! what value]
  (let [n (if (number? value) value (parse-double (str/trim (str value))))]
    (if (and (number? n) (<= 0 (double n) 1))
      (double n)
      (fail! (str what " must be a fraction from 0 to 1, or absent when the size is unknown")))))

(defn- live-target-kind
  "What a link points AT. Named by the spec, or inferred from a target carrying a
   URL scheme; anything else is refused with the three kinds named, because a
   surface opens each of them differently."
  [fail! declared target]
  (cond (some? declared) (live-term fail! ":target-kind" hi-spec/link-targets declared)
        (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*://" target) :url
        :else (fail! (str ":target-kind must be one of "
                          (str/join ", " (sort (keys hi-spec/link-targets)))
                          " — only a target carrying a scheme names its own kind"))))

(defn- normalize-live-item
  "One item of a keyed collection — a column, row, stat, step or link. All five
   are normalized by the same three rules: closed keys, an `:id` that ADDRESSES
   the item, and every closed vocabulary looked up rather than minted."
  [fail! kind item]
  (let [what (live-item-name kind)]
    (when-not (map? item) (fail! (str "a " what " must be a map")))
    (check-keys! what (live-item-keys kind) item fail!)
    (let
      [id (or (trimmed (pick* item :id))
              (fail! (str "a " what " needs a non-blank :id — a patch addresses it by that id")))
       item-fail! (fn [message]
                    (fail! (str what " " id ": " message)))
       label (trimmed (pick* item :label))
       tone (some->> (pick* item :tone)
                     (live-term item-fail! ":tone" hi-spec/live-tones))]

      (case kind
        :columns
        (cond-> {:id id :label (or label (item-fail! "a column needs a :label"))}
          (some? (pick* item :align))
          (assoc :align (live-term item-fail! ":align" hi-spec/live-aligns (pick* item :align))))

        :rows
        (cond-> {:id id :cells (text-items item-fail! ":cells" (or (pick* item :cells) []))}
          tone
          (assoc :tone tone))

        :stats
        (cond->
          {:id id
           :label (or label (item-fail! "a stat needs a :label"))
           :value-text (live-shown item-fail! ":value-text" (pick* item :value-text))}
          tone
          (assoc :tone tone))

        :steps
        (cond->
          {:id id :label (or label (item-fail! "a step needs a :label")) :tone (or tone :idle)}
          (trimmed (pick* item :detail))
          (assoc :detail (trimmed (pick* item :detail)))

          (some? (pick* item :value))
          (assoc :value (live-fraction item-fail! ":value" (pick* item :value))))

        :links
        (let [target (live-text item-fail! ":target" (pick* item :target))]
          (cond->
            {:id id
             :label (or label (item-fail! "a link needs a :label"))
             :target target
             :target-kind (live-target-kind item-fail! (pick* item :target-kind) target)}
            tone
            (assoc :tone tone)))))))

(defn- normalize-live-items
  [fail! kind values]
  (cond (nil? values) []
        (sequential? values) (mapv #(normalize-live-item fail! kind %) values)
        :else (fail! (str ":" (name kind) " must be a sequence of " (live-item-name kind) "s"))))

(defn- normalize-live-order
  "How a table PAINTS its rows. Absent means insertion order — the order the
   human watched them arrive in, which is the one order no surface can get wrong."
  [fail! order]
  (cond (nil? order) :insertion
        (map? order) (do (check-keys! "order" live-order-decl-keys order fail!)
                         (cond-> {:by (live-text fail! "a `{:by …}` order's :by" (pick* order :by))}
                           (some? (pick* order :dir))
                           (assoc :dir
                             (live-term fail! ":dir" hi-spec/live-sort-dirs (pick* order :dir)))))
        :else (live-term fail! ":order" hi-spec/live-orders order)))

(defn- live-node
  "One declared node of a live view. `fail!` is the refusal of whoever declares
   it — a view spec, or the `add-node` op that introduces one mid-run — so one
   node parser serves both without either learning the other's error type.

   A layout GROUP is parsed here too, by recursion: `row` and `column` are the
   REQUEST's own vocabulary, so a view arranges its work exactly the way a form
   arranges its questions, and a row inside a column costs no second grammar."
  [fail! node]
  (when-not (map? node) (fail! "a node must be a map"))
  (let [is-group (group-node? node)]
    (check-keys! (if is-group "group" "node")
                 (if is-group live-group-decl-keys live-node-decl-keys)
                 node
                 fail!)
    (if is-group
      (let
        [id (or (trimmed (pick* node :id))
                (fail! "a group needs a non-blank :id — `add-node :after` names it too"))
         group-fail! (fn [message]
                       (fail! (str "group " id ": " message)))
         children (pick* node :fields)
         label (trimmed (pick* node :label))]

        (when-not (and (sequential? children) (seq children))
          (group-fail! "a group needs a non-empty :fields — a row arranging nothing is a typo"))
        (checked-live-node group-fail!
                           (cond->
                             {:id id
                              :type hi-spec/group-type
                              :direction (live-term group-fail!
                                                    ":direction"
                                                    hi-spec/group-directions
                                                    (or (pick* node :direction) "column"))
                              :fields (mapv #(live-node group-fail! %) children)}
                             label
                             (assoc :label label))))
      (let
        [id (or (trimmed (pick* node :id))
                (fail! "a node needs a non-blank :id — every patch names the node it speaks to"))
         node-fail! (fn [message]
                      (fail! (str "node " id ": " message)))
         type (live-term node-fail! ":type" hi-spec/live-node-types (pick* node :type))
         label (trimmed (pick* node :label))
         base (cond-> {:id id :type type}
                label
                (assoc :label label))
         items (fn [kind]
                 (normalize-live-items node-fail! kind (pick* node kind)))]

        (checked-live-node
          node-fail!
          (case type
            :status
            (cond->
              (assoc base
                :text (live-text node-fail! "a status' :text" (pick* node :text))
                :tone (or (some->> (pick* node :tone)
                                   (live-term node-fail! ":tone" hi-spec/live-tones))
                          :idle))
              (trimmed (pick* node :detail))
              (assoc :detail (trimmed (pick* node :detail))))

            :progress
            (cond-> base
              (some? (pick* node :value))
              (assoc :value (live-fraction node-fail! ":value" (pick* node :value)))

              (some? (pick* node :done))
              (assoc :done (live-long node-fail! ":done" (pick* node :done)))

              (some? (pick* node :total))
              (assoc :total (live-long node-fail! ":total" (pick* node :total))))

            :stat
            (assoc base :stats (items :stats))

            :steps
            (assoc base :steps (items :steps))

            :log
            (assoc base
              :lines (if-some [lines (pick* node :lines)]
                       (text-items node-fail! ":lines" lines)
                       [])
              :window-lines (if-some [window (pick* node :window-lines)]
                              (live-long node-fail! ":window-lines" window)
                              (long (:window-lines hi-spec/log-defaults))))

            :table
            (assoc base
              :columns (items :columns)
              :rows (items :rows)
              :max-rows (if-some [bound (pick* node :max-rows)]
                          (live-long node-fail! ":max-rows" bound)
                          (long (:max-rows hi-spec/table-defaults)))
              :order (normalize-live-order node-fail! (pick* node :order)))

            :link
            (assoc base :links (items :links))))))))

(defn normalize-live-node
  "One declared node, refused where it was BUILT — the seam the node builders in
   `com.blockether.vis.human-input` validate through, so an unknown key or a
   `:tone` outside the table throws at the line that wrote it rather than in
   front of the human."
  [node]
  (live-node invalid-live-view! node))

(defn live-nodes?
  "Whether `nodes` are the picture a human WATCHES rather than the questions a
   form asks. A layout group is the ONE node both vocabularies share, so a
   builder composing one asks its children which check it is dated against — and
   a group inside a group answers by its own children."
  [nodes]
  (boolean (some (fn [node]
                   (when (map? node)
                     (if (group-node? node)
                       (live-nodes? (pick* node :fields))
                       (contains? hi-spec/live-node-types
                                  (some-> (trimmed (pick* node :type))
                                          str/lower-case)))))
                 nodes)))

(defn normalize-live-view
  "Validate a live-view spec and return the view the materializer holds. Throws
   `ex-info` with `:type :vis/human-input-invalid-live-view` on a bad spec, so a
   view is refused WHERE IT WAS DECLARED, before any surface drew anything.

   Three keys are the ENGINE's stamps and a spec that writes one is refused: the
   view's `:id`, the `:seq` no patch has advanced yet, and `:created-at`. Two
   writers of one id is exactly how a patch lands on the wrong view.

   Unlike a request, a view must name its SESSION here rather than at mount
   time: the session is where the record is kept, so a view without one has
   nowhere to be written down.

   `:timeout-ms` defaults to [[no-timeout-ms]] — a build takes as long as the
   build takes. It is the deadline a surface may show and a driver may honour;
   nothing in the engine kills a view that is still being patched, because the
   work behind it is the extension's, not a human's."
  [view]
  (when-not (map? view) (invalid-live-view! "a live view must be a map"))
  (check-keys! "live view" live-view-decl-keys view invalid-live-view!)
  (let
    [title
     (or (trimmed (pick* view :title)) (invalid-live-view! "a live view needs a non-blank :title"))

     raw-nodes
     (pick* view :nodes)

     _
     (when-not (sequential? raw-nodes) (invalid-live-view! ":nodes must be a sequence"))

     nodes
     (mapv #(live-node invalid-live-view! %) raw-nodes)

     _
     (when (empty? nodes) (invalid-live-view! ":nodes must not be empty"))

     _
     (when-not (apply distinct? (mapv :id nodes))
       (invalid-live-view! (str "node ids must be distinct — got "
                                (str/join ", " (sort (mapv :id nodes))))))

     session-id
     (or (trimmed (pick* view :session-id)) (ambient-session-id))]

    (checked-live-view (cond->
                         {:id (str (random-uuid))
                          :title title
                          :nodes nodes
                          :timeout-ms (normalize-timeout view no-timeout-ms invalid-live-view!)
                          :channel-ids (normalize-channel-ids view invalid-live-view!)
                          :seq 0
                          :created-at (System/currentTimeMillis)}
                         session-id
                         (assoc :session-id session-id)

                         (trimmed (pick* view :description))
                         (assoc :description (trimmed (pick* view :description)))

                         (trimmed (pick* view :source))
                         (assoc :source (trimmed (pick* view :source)))))))

(def ^:private live-op-value
  "How ONE key of a patch operation is normalized, by key. A table rather than a
   branch per operation, so `:tone` means the same thing in every op that carries
   one and no op can grow a private reading of a shared key."
  {:node-id (fn [fail! value]
              (live-text fail! ":node-id" value))
   :after (fn [fail! value]
            (live-text fail! ":after" value))
   :text (fn [fail! value]
           (live-text fail! ":text" value))
   :detail (fn [fail! value]
             (live-text fail! ":detail" value))
   :label (fn [fail! value]
            (live-text fail! ":label" value))
   :tone (fn [fail! value]
           (live-term fail! ":tone" hi-spec/live-tones value))
   :value (fn [fail! value]
            (live-fraction fail! ":value" value))
   :done (fn [fail! value]
           (live-long fail! ":done" value))
   :total (fn [fail! value]
            (live-long fail! ":total" value))
   :lines (fn [fail! value]
            (text-items fail! ":lines" value))
   :item-ids (fn [fail! value]
               (text-items fail! ":item-ids" value))
   :rows (fn [fail! value]
           (normalize-live-items fail! :rows value))
   :stats (fn [fail! value]
            (normalize-live-items fail! :stats value))
   :steps (fn [fail! value]
            (normalize-live-items fail! :steps value))
   :links (fn [fail! value]
            (normalize-live-items fail! :links value))
   :node-spec (fn [fail! value]
                (live-node fail! value))})

(defn- live-op
  "One patch operation, normalized against the closed key set its own `:op`
   chooses."
  [fail! op]
  (when-not (map? op) (fail! "an operation must be a map"))
  (let
    [kind
     (live-term fail! ":op" hi-spec/live-ops (pick* op :op))

     ;; "an append op", "a set op" — the refusal is read out loud by whoever wrote it.
     article
     (if (contains? #{\a \e \i \o \u} (first (name kind))) "an " "a ")

     op-fail!
     (fn [message]
       (fail! (str article (name kind) " op: " message)))]

    (check-keys! (str (name kind) " op") (live-op-decl-keys kind) op op-fail!)
    (reduce (fn [acc k]
              (if-some [value (pick* op k)]
                (assoc acc k ((live-op-value k) op-fail! value))
                acc))
            {:op kind}
            (disj (hi-spec/live-op-key-sets kind) :op))))

(defn normalize-live-op
  "One patch operation, refused where it was BUILT — the seam the `add-node` /
   `remove-node` builders check through. Shape only: whether the node an op
   names EXISTS is the running view's answer, in [[normalize-patch]]."
  [op]
  (live-op invalid-live-patch! op))

(defn normalize-patch
  "Validate `patch` against the `view` it lands on and return its internal form:
   the operations as declared, under the engine's own `:view-id` and the NEXT
   `:seq`.

   The ops may arrive bare or under `:ops`, because the wire carries a map and a
   Clojure caller has a vector — everything below that is one vocabulary. Shape
   only: whether the node an op names EXISTS, and whether it would cross a
   bound, is `live/apply-patch`'s answer."
  [view patch]
  (let
    [ops (cond (map? patch) (do (check-keys! "patch" live-patch-decl-keys patch invalid-live-patch!)
                                (pick* patch :ops))
               (sequential? patch) patch
               :else (invalid-live-patch! "a patch is a sequence of operations"))]
    (when-not (sequential? ops) (invalid-live-patch! ":ops must be a sequence of operations"))
    (when (empty? ops)
      (invalid-live-patch!
        ":ops must not be empty — a patch that changes nothing is a bug, not a state"))
    (checked-live-patch {:view-id (:id view)
                         :seq (inc (long (:seq view)))
                         :ops (mapv #(live-op invalid-live-patch! %) ops)})))

;; Value coercion — one implementation for defaults and submissions

(defn- coerce-text
  [{:keys [type is-required min-length max-length]} value]
  (let
    [text
     (cond (nil? value) ""
           ;; A JSON client can post an object or a list where a dialog can only
           ;; ever type characters. `str` would hand the extension a Clojure
           ;; printing of it, so the app would submit something the TUI cannot —
           ;; reject it instead, like any other malformed value.
           (coll? value) ::invalid
           :else (str value))

     text
     (cond (= ::invalid text) text
           (= :multiline type) text
           :else (str/trim text))]

    (cond (= ::invalid text) [:error "must be text"]
          (and is-required (str/blank? text)) [:error "is required"]
          ;; A blank optional answer is not a short one: length bounds describe
          ;; the shape of a value that IS there, exactly like the rules do.
          (str/blank? text) [:ok (when (= :multiline type) (when-not (empty? text) text))]
          (and min-length (< (count text) (long min-length)))
          [:error (str "must be at least " min-length " characters")]
          (and max-length (> (count text) (long max-length)))
          [:error (str "must be at most " max-length " characters")]
          :else [:ok text])))

(defn- coerce-select
  [{:keys [is-required options]} value]
  (let
    [text
     (trimmed value)

     allowed
     (set (map :value options))]

    (cond (nil? text) (if is-required [:error "is required"] [:ok nil])
          (contains? allowed text) [:ok text]
          :else [:error (str "must be one of " (str/join ", " (sort allowed)))])))

(defn- coerce-multiselect
  [{:keys [is-required options]} value]
  (let
    [values (cond (nil? value) []
                  (string? value) [value]
                  (sequential? value) (vec value)
                  (set? value) (vec value)
                  :else ::invalid)]
    (if (= ::invalid values)
      [:error "must be a list of option values"]
      (let
        [picked (into [] (comp (keep trimmed) (distinct)) values)
         allowed (set (map :value options))
         unknown (remove allowed picked)]

        (cond (seq unknown) [:error (str "unknown option " (str/join ", " (sort unknown)))]
              (and is-required (empty? picked)) [:error "is required"]
              :else [:ok picked])))))

(defn- coerce-checkbox
  [{:keys [is-required]} value]
  (let
    [[status result] (cond (nil? value) [:ok false]
                           (boolean? value) [:ok value]
                           (contains? #{"true" "1"} (str/lower-case (str value))) [:ok true]
                           (contains? #{"false" "0"} (str/lower-case (str value))) [:ok false]
                           :else [:error "must be true or false"])]
    ;; A required checkbox is a consent box — "I agree", "yes, delete it". Leaving
    ;; it unticked is not an answer, so it is refused exactly like a blank
    ;; required text field. Without this the surfaces disagree: the app greys its
    ;; submit button out for an unticked required box while the engine happily
    ;; accepted `false` from anything that posted JSON.
    (if (and (= :ok status) is-required (not result)) [:error "must be checked"] [status result])))

(defn- coerce-range
  [{lo :min hi :max st :step} value]
  (let
    [lo
     (if (number? lo) lo (:min hi-spec/range-defaults))

     hi
     (if (number? hi) hi (:max hi-spec/range-defaults))

     n
     (cond (nil? value) lo
           (number? value) value
           (coll? value) ::invalid
           :else (let [s (str/trim (str value))]
                   (or (parse-long s) (parse-double s) ::invalid)))]

    (cond (= ::invalid n) [:error "must be a number"]
          (or (< (double n) (double lo)) (> (double n) (double hi)))
          [:error (str "must be between " lo " and " hi)]
          ;; The SPEC decides the answer's type, not the keystroke that produced it:
          ;; an all-integer slider always hands the extension a long, so `0`, `"0"`
          ;; and `0.0` cannot reach it as three different things.
          (every? integer? [lo hi (if (number? st) st (:step hi-spec/range-defaults))])
          [:ok (long (Math/round (double n)))]
          :else [:ok (double n)])))

(defn- coerce-otp
  "A one-time code is DIGITS, however the human pasted them. Spaces and dashes
   are how every provider prints a code (`123 456`, `123-456`), so they are
   separators here rather than a typo the operator has to go back and delete."
  [{:keys [is-required min-length max-length]} value]
  (let
    [lo
     (long (or min-length (:length hi-spec/otp-defaults)))

     hi
     (long (or max-length (:length hi-spec/otp-defaults)))

     digits
     (cond (nil? value) ""
           (coll? value) ::invalid
           :else (str/replace (str value) #"[\s-]" ""))]

    (cond (= ::invalid digits) [:error "must be a one-time code"]
          ;; Nothing typed is nothing answered — nil, exactly like an empty text
          ;; field, so an untouched optional code does not become a `""` default.
          (str/blank? digits) (if is-required [:error "is required"] [:ok nil])
          (not (re-matches #"[0-9]+" digits)) [:error "must be digits only"]
          (= lo hi) (if (= (count digits) hi) [:ok digits] [:error (str "must be " hi " digits")])
          (< (count digits) lo) [:error (str "must be at least " lo " digits")]
          (> (count digits) hi) [:error (str "must be at most " hi " digits")]
          :else [:ok digits])))

(defn coerce-value
  "Coerce and validate one raw `value` against normalized `field`. Returns
   `[:ok coerced]` or `[:error message]`."
  [{:keys [type] :as field} value]
  (cond (contains? hi-spec/text-types type) (coerce-text field value)
        (= :select type) (coerce-select field value)
        (= :multiselect type) (coerce-multiselect field value)
        (= :checkbox type) (coerce-checkbox field value)
        (= :range type) (coerce-range field value)
        (= :otp type) (coerce-otp field value)
        :else [:error "unknown field type"]))

(defn- coerce-all
  "Coerce every field's raw value. Pure: `{:values … :errors …}`, no vault.

   Both maps grow as TRANSIENTS through one indexed pass. Every keystroke on
   every surface lands here, and the `assoc-in` this replaces rebuilt two nested
   persistent maps per field for an answer the caller only ever reads once."
  [fields values]
  (let
    [values
     (or values {})

     fields
     (vec fields)

     n
     (long (count fields))]

    (loop
      [i
       0

       out
       (transient {})

       errs
       (transient {})]

      (if-not (< i n)
        {:values (persistent! out) :errors (persistent! errs)}
        (let
          [{:keys [id] :as field}
           (nth fields i)

           raw
           (cond (contains? values id) (get values id)
                 (contains? values (keyword id)) (get values (keyword id))
                 ;; Absent means "the human left it alone" — the field's
                 ;; declared default stands in, then gets validated like
                 ;; any other value.
                 :else (:default field))

           [status result]
           (coerce-value field raw)]

          (if (= :error status)
            (recur (inc i) out (assoc! errs id result))
            ;; Every field id is present in `:values`, so a caller can
            ;; read a field without knowing whether it was filled in.
            (recur (inc i) (assoc! out id result) errs)))))))

(defn- check-all
  "Run each field's `:validate` functions over the COERCED values. A field the
   type already rejected is left alone: one message per field, and the one that
   explains the earliest problem.

   The map [[coerce-all]] just handed over is grown further as a transient
   instead of being rebuilt validator by validator."
  [fields {:keys [values errors]}]
  {:values values
   :errors (persistent! (reduce (fn [acc {:keys [id validate]}]
                                  (if (or (empty? validate) (some? (get acc id)))
                                    acc
                                    (if-let
                                      [message (validation/check validate (get values id) values)]
                                      (assoc! acc id message)
                                      acc)))
                                (transient errors)
                                fields))})

(defn validate-values
  "Coerce and validate a raw `field id -> value` map against a request's
   `fields`. Returns `{:is-accepted true :values …}` or
   `{:is-accepted false :errors {id message}}`.

   `fields` may be the request's TREE: layout groups hold no answer, so they are
   flattened away here and a group can never change what an extension reads.

   Pure — no vault, no state, no side effect of any kind — but not free: it runs
   the extension's own validator FUNCTIONS. So it runs ONCE, when the human
   confirms the form, never on a keystroke; only a real submission goes through
   [[coerce-values]]."
  [fields values]
  (let
    [fields
     (input-fields fields)

     {:keys [values errors]}
     (check-all fields (coerce-all fields values))]

    (if (seq errors) {:is-accepted false :errors errors} {:is-accepted true :values values})))

(defn coerce-values
  "[[validate-values]] for a SUBMISSION: identical answer, except that accepted
   `:password` and `:otp` values are replaced with opaque vault handles."
  [fields values]
  (let [result (validate-values fields values)]
    (if-not (:is-accepted result)
      result
      (assoc result
        :values (persistent! (reduce (fn [acc {:keys [id is-secret]}]
                                       (let [value (get acc id)]
                                         (if (and is-secret (some? value))
                                           (assoc! acc id (stash-secret! value))
                                           acc)))
                                     (transient (:values result))
                                     (input-fields fields)))))))

;; Channel projection

(defn request->view
  "The channel/wire-facing projection of a pending request: the spec a dialog
   needs, and nothing a channel must not see (no promise, no submitted values,
   and no validator — validation is CODE the engine runs when the form is
   confirmed, a function cannot cross the wire, and a surface's job is to render
   the errors it is handed rather than to invent its own).

   The field TREE is projected as a tree: a group crosses the wire with its own
   `:direction` and `:fields`, so both surfaces lay the form out from the same
   data instead of each inventing a layout."
  [request]
  (-> request
      (dissoc :promise :channel-ids :kind)
      (assoc :fields (map-fields #(dissoc % :is-secret :validate) (:fields request)))))

(def ^:private request-stamps
  "The engine's own request stamps, wire spelling -> normalized key."
  (into {} (map (juxt snake-key identity)) hi-spec/request-stamp-keys))

(defn view<-wire
  "Inverse of [[request->view]] for a view that CROSSED A PROCESS BOUNDARY — the
   canonical snake_case map a `human_input.request` session event carries.

   A run parked inside `vis-agent serve` publishes on an in-process channel bus that
   never leaves that JVM, so for every other process the session event IS the
   request. Rebuilding the view goes back through the engine's own parser rather
   than through a second field vocabulary; only the stamps of [[request!]],
   which `normalize-request` refuses by contract, are lifted across unchanged."
  [wire]
  (when (map? wire)
    (let
      [stamps (into {}
                    (keep (fn [[wire-key k]]
                            (when-some [v (get wire wire-key)]
                              [k v])))
                    request-stamps)]
      (merge (request->view (normalize-request (apply dissoc wire (keys request-stamps))))
             stamps))))

(defn- publish!
  "Publish `event` on every channel in `channel-ids` and return how many
   listeners it actually reached across all of them."
  [channel-ids event]
  (transduce (map #(channel-events/publish-channel-event! % event)) + 0 channel-ids))

;; A live view's life — open, patch, close
;;
;; Nothing here parks the caller. A form parks because only a human can answer
;; it; a view is DRIVEN by the extension that opened it, so the engine's job is
;; to keep the record, tell the surfaces, and hand back the one thing the model
;; reads: the finished picture, as data.

(def ^:private live-ending-keys
  "Every key the caller of [[close-live!]] may write. The view id, the completion
   flag and the picture are the ENGINE's: what the human watched is read from the
   record, never claimed by whoever is ending it. So is the human's own stop:
   `:is-from-human` and `:note` are ENGINE stamps too, because no run gets to
   claim a person ended it."
  (wire-keys (reduce disj
                     hi-spec/live-result-keys
                     #{:view-id :is-completed :view :elided :is-from-human :note})))

(defn- live-entry
  "The pending entry of live view `view-id`, or nil when no live view is open
   under that id."
  [view-id]
  (let [entry (get @pending view-id)]
    (when (= :live (:kind entry)) entry)))

(defn- live-entry!
  [view-id]
  (or (live-entry view-id)
      (invalid-live-patch!
        (str "no live view " view-id " is open — it was closed, interrupted, or never opened"))))

(defn live-view
  "What live view `view-id` looks like right now, or nil. This IS what the
   surfaces paint: one materialized map, so the terminal, the phone and the model
   cannot disagree about a row."
  [view-id]
  (some-> (live-entry view-id)
          :view
          deref))

(defn open-live!
  "Mount a live view the human WATCHES and return it materialized, `:id` and all.
   The caller keeps that id: every patch, and the close, name it.

   Nothing blocks. A view nobody has mounted a surface for still runs and still
   ends in the verdict the model reads — the engine says so ONCE, in the log,
   rather than refusing work whose whole product is the picture at the end. That
   is the one place a view differs from a request, which answers `undeliverable`
   at once because a form nobody can see is a thread parked forever."
  [view]
  (let
    [view
     (live/materialize (normalize-live-view view))

     view-id
     (:id view)

     _
     (when-not (trimmed (:session-id view))
       (invalid-live-view! (str "view " view-id
                                " names no session — set :session-id, or open it "
                                "from an extension environment that carries one")))

     entry
     {:kind :live
      :id view-id
      :view (atom view)
      :file (live-sink/open! view)
      :promise (promise)
      :session-id (:session-id view)
      :channel-ids (:channel-ids view)
      :created-at (:created-at view)}]

    (swap! pending assoc view-id entry)
    (tel/log! {:level :debug
               :id ::live-opened
               :data {:view-id view-id :title (:title view) :nodes (mapv :id (:nodes view))}
               :msg "Live view opened"})
    (when (zero? (long (publish! (:channel-ids entry)
                                 {:op :human-input/live-open
                                  :view-id view-id
                                  :session-id (:session-id entry)
                                  :view view})))
      (tel/log! {:level :warn
                 :id ::live-unwatched
                 :data {:view-id view-id :title (:title view) :channel-ids (:channel-ids entry)}
                 :msg (str "Live view reached no channel — nobody is watching it. It still runs, "
                           "and it still ends in the verdict the model reads")}))
    view))

(defn patch-live!
  "Apply `patch` to live view `view-id` and return the view it made.

   One patch at a time per view: the record on disk is the order the engine
   ACCEPTED patches in, and two threads racing would leave a file no replay can
   trust. The line is written BEFORE any surface is told, so a crash keeps what
   the engine accepted rather than what a screen managed to paint."
  [view-id patch]
  (let
    [entry
     (live-entry! view-id)

     cell
     (:view entry)]

    (locking cell
      (let
        [applied
         (normalize-patch @cell patch)

         patched
         (live/apply-patch @cell applied)]

        (reset! cell patched)
        (live-sink/append! (:file entry) applied)
        (publish! (:channel-ids entry)
                  {:op :human-input/live-patch
                   :view-id view-id
                   :session-id (:session-id entry)
                   :patch applied})
        patched))))

(defn- human-note
  "The comment a human left with their stop: trimmed, and cut to
   `hi-spec/note-chars`. A stop is never refused for the length of its note —
   what the person managed to type before pressing stop always reaches the model."
  [note]
  (when-let [text (trimmed note)]
    (subs text 0 (min (count text) (long hi-spec/note-chars)))))

(defn- live-result
  "The verdict of `view`: how it ended, and the whole picture the human watched,
   handed to the model as DATA. `:view` carries the finished nodes — ids, tones,
   values — budgeted exactly as the document is, and `:elided` counts what that
   budget left in the record. It reads the same materialized state both human
   surfaces painted, and says how the story ended before it says anything else.

   `human` is the person who ended it: `{:note …}` from [[interrupt-live!]], nil
   when the run ended itself. Their stop is stamped `:is-from-human`, and their
   words ride along as `:note`, so a run that was cut short reads that a PERSON
   cut it, and why, instead of inferring it from `interrupted`."
  [view ending human fail!]
  (when-not (map? ending) (fail! "an ending must be a map"))
  (check-keys! "ending" live-ending-keys ending fail!)
  (let
    [reason
     (live-term fail! ":reason" hi-spec/live-reasons (or (pick* ending :reason) :completed))

     verdict
     (cond->
       {:view-id (:id view)
        :is-completed (= :completed reason)
        :reason reason
        :is-from-human (some? human)}
       (human-note (:note human))
       (assoc :note (human-note (:note human)))

       (trimmed (pick* ending :summary))
       (assoc :summary (trimmed (pick* ending :summary)))

       (trimmed (pick* ending :error))
       (assoc :error (trimmed (pick* ending :error)))

       (trimmed (pick* ending :artifact-id))
       (assoc :artifact-id (trimmed (pick* ending :artifact-id))))

     picture
     (live/picture view)

     result
     (cond-> (assoc verdict :view (:view picture))
       (seq (:elided picture))
       (assoc :elided (:elided picture)))]

    (if-let [why (hi-spec/live-result-error result)]
      (fail! why)
      result)))

(defn- live-attachment
  "The settled view as ONE row of `db-store-iteration!`'s `:attachments` — the only
   shape the engine stores an artifact in. It IS the artifact, given the two keys a
   row is filed under (`:kind`, `:filename`) and stripped of `:view`: the final
   picture is the VERDICT's, handed to the model as data on the close, and a row is
   a row. Everything a gallery needs to list one — which view it was, how it ended,
   how much record there is and where — travels with it."
  [artifact]
  (-> artifact
      (dissoc :view)
      (assoc :kind "file"
             :filename (str (or (live/slug (:title artifact)) "live-view") ".live.json"))))

(defn- live-artifact
  "The closed view as an ARTIFACT the human can reopen: what they watched,
   ADDRESSED rather than copied.

   Nothing here reads the log. The record has been the store of truth since
   `open`, so the artifact points at that file (`:storage-uri`) and states how much
   of it there is; only a view small enough to survive a session sync
   (`hi-spec/live-artifact-inline-bytes`) also travels as bytes, because holding a
   build log in memory as base64 is the cost this whole design removes. `:view` is
   the final materialized state — the summary a surface opens instantly, and the
   very state `live/->markdown` re-renders the model's document from.

   `:size` and `:line-count` count the RUN — the declared view and every accepted
   patch — because the trailer that seals the record states the verdict, and the
   verdict is already `:reason` and `:view` here."
  [view result ^java.io.File file]
  (let
    [{:keys [size line-count]}
     (live-sink/stats file)

     artifact
     (cond->
       {:id (str (java.util.UUID/randomUUID))
        :view-id (:view-id result)
        :session-id (:session-id view)
        :title (:title view)
        :media-type hi-spec/live-artifact-media-type
        :audience "user"
        :ended-at (System/currentTimeMillis)
        :reason (:reason result)
        :view (:view result)
        :storage-uri (str "file://" (.getAbsolutePath file))
        :size size
        :line-count line-count}
       (<= (long size) (long hi-spec/live-artifact-inline-bytes))
       (assoc :base64
         (.encodeToString (java.util.Base64/getEncoder)
                          (java.nio.file.Files/readAllBytes (.toPath file)))))]

    (if-let [why (hi-spec/live-artifact-error artifact)]
      (invalid-live-view! why)
      artifact)))

(defn close-live!
  "End live view `view-id` and return its verdict — the ONE thing the model
   reads. nil when the view was already closed, so a `finally` closing what an
   interrupt already closed is a no-op rather than a second verdict.

   `ending` says how it ended: `:reason` (`completed` by default), `:summary`,
   `:error`, `:artifact-id`. `human` is the person who stopped it — `{:note …}`,
   which only [[interrupt-live!]] passes, because a run does not get to claim a
   human ended it.

   The close SETTLES the view: the record it has been writing since `open` becomes
   an artifact this session owns, and its id rides back in the verdict — so the
   human can reopen the log after the pane is gone instead of it being dumped into
   the transcript. A close reached from somewhere that holds no artifacts (a
   human's stop arriving on a gateway thread, a unit test) still ends the view and
   still writes the record; the verdict then simply names no artifact, and every
   surface still reaches the log by view id."
  ([view-id] (close-live! view-id {} nil))
  ([view-id ending] (close-live! view-id ending nil))
  ([view-id ending human]
   (when-let [entry (live-entry view-id)]
     ;; The SAME cell `patch-live!` holds: a close racing the last patch waits for
     ;; it, so the verdict renders the picture the record already ends with.
     (let [cell (:view entry)]
       (locking cell
         (let
           [verdict (live-result @cell ending human invalid-live-view!)
            ;; Built BEFORE the registry drops the view, so a refusal leaves the
            ;; view open and nameable rather than stranding whoever is holding it;
            ;; REGISTERED after, inside the branch that won the close, so a second
            ;; close files no second artifact.
            artifact (live-artifact @cell verdict (:file entry))
            [old _] (swap-vals! pending dissoc view-id)]

           (when (contains? old view-id)
             (let
               [artifact-id (when (mpl-capture/record-attachment! (live-attachment artifact))
                              (:id artifact))
                result (cond-> verdict
                         artifact-id
                         (assoc :artifact-id artifact-id))]

               ;; The trailer carries the SAME verdict that is delivered and
               ;; published, artifact and all: `live-ended` reads the record for a
               ;; view the human interrupted, and it must not learn less than the
               ;; thread that was holding it.
               (live-sink/close! (:file entry) result)
               (deliver (:promise entry) result)
               ;; `:session-id` rides on every live event for the same reason it
               ;; rides on a form's close: by the time this one is published the
               ;; entry is out of the registry, and a listener that has to route
               ;; the ending to a session can no longer look it up.
               (publish! (:channel-ids entry)
                         {:op :human-input/live-close
                          :view-id view-id
                          :session-id (:session-id entry)
                          :result result})
               (tel/log! {:level :debug
                          :id ::live-closed
                          :data {:view-id view-id
                                 :reason (:reason result)
                                 :is-from-human (:is-from-human result)
                                 :artifact-id artifact-id}
                          :msg "Live view closed"})
               result))))))))

(defn interrupt-live!
  "End live view `view-id` because a human stopped watching — Escape in the
   terminal, Stop in the app — with `note`, the comment they left, when they left
   one. ALWAYS allowed: a view asks nothing, so stopping it leaves nothing
   unanswered, and work being SHOWN can always be told to stop showing itself.

   The verdict reads `interrupted`, is stamped `:is-from-human true`, carries the
   note, and still carries the picture of everything that had happened: what the
   human saw before they stopped it is exactly what the model has to read, and
   their own words say why it is not the whole story."
  ([view-id] (interrupt-live! view-id nil))
  ([view-id note] (close-live! view-id {:reason :interrupted} {:note note})))

(defn with-live!
  "Open the view `view` declares, hand its id to `body`, and CLOSE it — on a
   throw as well as on a return, because an extension that dies mid-run must
   still leave the model the picture the human watched. Returns the verdict.

   A `body` that closed the view itself WINS: the ending it chose is the one
   that ships, this close is then a no-op and the answer is nil — a run that
   knows why it stopped never has that reason overwritten by its wrapper."
  [view body]
  (let [view-id (:id (open-live! view))]
    (try (body view-id)
         (close-live! view-id)
         (catch Throwable t
           (close-live! view-id {:reason :failed :error (or (ex-message t) (str t))})
           (throw t)))))

(defn- entry->view
  "One pending entry as the surfaces see it, by kind: a request loses its promise
   and its validators, a live view IS its materialized state."
  [entry]
  (if (= :live (:kind entry))
    (some-> (:view entry)
            deref)
    (request->view entry)))

;; Registry

(defn pending-requests
  "The forms a run is BLOCKED on right now, oldest first.

   Live views share this registry — one id space, one cancel path — but they are
   not questions: nothing is parked on them and nobody owes them an answer, so
   they come back from [[live-views]] and a surface listing what WAITS on the
   human never shows one."
  []
  (->> (vals @pending)
       (remove #(= :live (:kind %)))
       (sort-by :created-at)
       (mapv entry->view)))

(defn live-views
  "Every live view open right now, materialized exactly as the surfaces paint
   them, oldest first.

   This is the resync: a client that joined mid-flight, woke from sleep or lost
   its stream reads the picture back instead of replaying every patch it missed."
  []
  (->> (vals @pending)
       (filter #(= :live (:kind %)))
       (sort-by :created-at)
       (mapv entry->view)))

(defn pending-request
  "The pending request `request-id`, as a view, or nil."
  [request-id]
  (some-> (get @pending request-id)
          entry->view))

(defn- settle!
  "Remove `request-id` and deliver `result` to whoever is blocked on it. Returns
   the removed entry, or nil when the request already settled (a late submit
   racing a timeout, a double cancel)."
  [request-id result]
  ;; The one funnel every answer passes through — submitted, cancelled, timed
  ;; out, undeliverable — so the contract is checked once, here, against the very
  ;; request being answered. The fields are read BEFORE the entry is removed: a
  ;; refusal must not strand the thread parked on that promise.
  (checked-answer request-id (:fields (get @pending request-id)) result)
  (let [[old _] (swap-vals! pending dissoc request-id)]
    (when-let [entry (get old request-id)]
      (deliver (:promise entry) result)
      ;; The close event carries `:session-id` because the entry is ALREADY
      ;; gone from `pending` by now: a listener that has to route the close to
      ;; a session can no longer look it up.
      (publish! (:channel-ids entry)
                (cond-> {:op :human-input/close :request-id request-id :reason (:reason result)}
                  (:session-id entry)
                  (assoc :session-id (:session-id entry))))
      entry)))

(defn submit!
  "Resolve pending request `request-id` with a raw `field id -> value` map.

   Returns `{:is-accepted false :errors {field-id message}}` when a value fails
   its field's validation — the request stays pending so the dialog can show the
   errors inline. Returns `{:is-accepted true}` once the waiter is released, and
   `{:is-accepted false :reason \"unknown\"}` for an already-settled request."
  [request-id values]
  (if-let [entry (get @pending request-id)]
    (if (= :live (:kind entry))
      (invalid-answer! request-id
                       (str "this is a live view, not a form — a view ends when the extension that "
                            "opened it closes it, and carries no values to submit"))
      (let [outcome (coerce-values (:fields entry) values)]
        (if (:is-accepted outcome)
          (if (settle! request-id
                       {:is-submitted true
                        :reason "submitted"
                        :request-id request-id
                        :values (:values outcome)})
            {:is-accepted true}
            {:is-accepted false :reason "unknown"})
          outcome)))
    {:is-accepted false :reason "unknown"}))

(defn- force-cancel!
  "Release `request-id` no matter what it declared — a form settles as
   cancelled, a live view closes as interrupted. The shutdown path: a detaching
   channel or a closing session must never leave a thread parked or a view open,
   and the two kinds end differently because they were entered differently."
  [request-id reason]
  (if (live-entry request-id)
    (some? (close-live! request-id {:reason :interrupted}))
    (some? (settle! request-id
                    {:is-submitted false
                     :reason (or (trimmed reason) "cancelled")
                     :request-id request-id}))))

(defn cancel!
  "Cancel pending request `request-id` on the operator's behalf. Returns true
   when it was pending AND dismissable.

   A request declared `:is-cancellable false` refuses here, so EVERY surface is
   refused alike — the TUI dialog, the companion app, any extension API. The
   only ways out of such a request are an accepted answer, its timeout, and
   [[cancel-all!]]."
  ([request-id] (cancel! request-id "cancelled"))
  ([request-id reason]
   (if (false? (:is-cancellable (get @pending request-id)))
     false
     (force-cancel! request-id reason))))

(defn cancel-all!
  "Cancel every pending request. Returns how many were released. Used when a
   channel detaches or the session shuts down, so no thread stays parked — this
   one ignores `:is-cancellable`, because nothing is left to answer with."
  ([] (cancel-all! "cancelled"))
  ([reason] (count (filterv #(force-cancel! % reason) (keys @pending)))))

(defn request!
  "Ask the operator for typed values and BLOCK until they answer.

   `request` is a spec map — `:title`, `:fields`, optional `:description`,
   `:submit-label`, `:cancel-label`, `:is-cancellable`, `:timeout-ms`,
   `:channel-ids` (string keys from the Python boundary work too).

   Every field carries `:name`, `:type`, `:label` and an optional
   `:description`. `:name` keys the answer in `:values`, `:label` is what the
   dialog shows above the input, and `:description` is the italic line under
   that label — see [[normalize-field]].

   Publishes a `:human-input/request` channel event, waits for [[submit!]] /
   [[cancel!]], and always returns a map, either

     :is-submitted true, :reason \"submitted\", plus :request-id and :values

   or

     :is-submitted false, :reason \"cancelled\"/\"timeout\"/\"undeliverable\"/…,
     plus :request-id

   `\"undeliverable\"` is the honest answer when the event reached ZERO
   listeners: no surface is mounted on any channel the request names, so no
   dialog can be drawn and nobody can answer. That returns AT ONCE and logs an
   error naming the request — parking the caller for the full timeout would
   report a run nobody was ever shown as if a human had ignored it.

   `:timeout-ms` is the wait this call is willing to bill: [[default-timeout-ms]]
   when the spec says nothing, or [[no-timeout-ms]] (0) to wait INDEFINITELY. A
   finite wait that runs out settles the request itself — the dialog closes on
   every surface and the answer reads `timeout`, so the extension resumes with one
   clear fixed outcome instead of a half-open form nobody can answer. An
   indefinite wait never gives up on the human: only an answer, a cancel or an
   interrupt releases it.

   `:password` and `:otp` values in `:values` are opaque handles — see
   [[reveal-secret]].

   A request MUST name the session it parks — `:session-id`, or the session of
   the extension environment currently executing. A run nobody can attribute is
   a run the companion app is never told about: the gateway bridge turns the
   request into a session event, and a session event with no session has nowhere
   to go, so only a surface mounted in this very process could ever answer it.
   That is refused here, before anything blocks."
  [request]
  (let
    [entry
     (assoc (normalize-request request)
       :kind :form
       :promise (promise)
       :created-at (System/currentTimeMillis))

     request-id
     (:id entry)]

    (when-not (trimmed (:session-id entry))
      (invalid-request! (str "request " request-id
                             " names no session — set :session-id, or raise it "
                             "from an extension environment that carries one")))
    (when (contains? @pending request-id)
      (invalid-request! (str "request id " request-id " is already pending")))
    (swap! pending assoc request-id entry)
    (tel/log! {:level :debug
               :id ::request-opened
               :data {:request-id request-id
                      :fields (mapv :id (input-fields (:fields entry)))
                      :timeout-ms (:timeout-ms entry)}
               :msg "Human-input request opened"})
    (if (zero? (long (publish! (:channel-ids entry)
                               {:op :human-input/request
                                :request-id request-id
                                :request (request->view entry)})))
      (do (tel/log! {:level :error
                     :id ::request-undeliverable
                     :data {:request-id request-id
                            :title (:title entry)
                            :channel-ids (:channel-ids entry)
                            :session-id (:session-id entry)}
                     :msg (str "Human-input request reached no channel — nothing is mounted on "
                               (pr-str (:channel-ids entry))
                               ", so no dialog can be drawn and nobody can answer")})
          (settle! request-id {:is-submitted false :reason "undeliverable" :request-id request-id})
          ;; `settle!` delivered, unless a racing submit! got there first.
          @(:promise entry))
      (let
        [timeout-ms
         (:timeout-ms entry)

         result
         ;; Waiting on a human is NOT wall-clock work an enclosing timeout may
         ;; bill: park every enclosing wall (Python eval watchdog, native-tool
         ;; wall) for as long as the operator takes. Without this the surrounding
         ;; wall kills the thread at `Timeout` with the dialog still up.
         (rt/park-blocking-wall (fn []
                                  (try (if (indefinite-timeout? timeout-ms)
                                         ;; No deadline at all: only an answer, a cancel or an
                                         ;; interrupt gets this thread back.
                                         @(:promise entry)
                                         (deref (:promise entry) timeout-ms ::timeout))
                                       (catch Throwable t
                                         ;; Interrupt/cancel of the surrounding turn: release the entry
                                         ;; and close the dialog, never leave a zombie pending request.
                                         (force-cancel! request-id "interrupted")
                                         (throw t)))))]

        (if (identical? ::timeout result)
          (do (tel/log! {:level :warn
                         :id ::request-timed-out
                         :data {:request-id request-id :title (:title entry) :timeout-ms timeout-ms}
                         :msg
                         "Human-input request timed out — nobody answered, resuming without one"})
              (settle! request-id {:is-submitted false :reason "timeout" :request-id request-id})
              ;; `settle!` delivered, or a submit! that won the race already did.
              @(:promise entry))
          result)))))

;; Strings-only boundary — what a Python extension actually calls

(defn answer->wire
  "Wire projection of a [[request!]] answer: snake_case string keys, JSON-safe
   values. `:password` and `:otp` values stay opaque handles."
  [answer]
  {"is_submitted" (boolean (:is-submitted answer))
   "reason" (:reason answer)
   "request_id" (:request-id answer)
   "values" (or (:values answer) {})})

(defn- attach-validators
  "Put the extension's validator FUNCTIONS back onto the field tree a Python
   extension just sent as JSON.

   A callable is not JSON, so `vis.ask()` pops each field's `validate` functions
   out of the spec, reports `{field name -> how many it declared}`, and hands the
   host one `run` callable that dispatches on that name and index. The fields
   come back carrying real functions and [[normalize-request]] never learns that
   Python was involved. Groups nest, so this walks the tree, not a flat list."
  [fields counts run]
  (mapv (fn [field]
          (let
            [field-name
             (trimmed (or (get field "name") (get field "id")))

             declared
             (get counts field-name)

             children
             (get field "fields")]

            (cond-> field
              (sequential? children)
              (assoc "fields" (attach-validators children counts run))

              (and (number? declared) (pos? (long declared)))
              (assoc "validate"
                (mapv (fn [index]
                        (fn [value values]
                          (run field-name index value values)))
                      (range (long declared)))))))
        fields))

(defn request-json!
  "The strings-only seam a Python extension crosses: a JSON request object in, a
   JSON answer object out. Blocks exactly like [[request!]].

   Channel routing is host-side — a `channel_id`/`channel_ids` key is dropped
   rather than minting keywords from guest data, so a Python extension always
   reaches the channels the host picked.

   Validation is CODE, so it does not travel as JSON either: `validators-json` is
   `{field name -> how many validators that field declared}` and `run` is called
   `(run field-name index value values)` to reach the extension's own function,
   answering the verdict
   [[com.blockether.vis.internal.human-input.validation/check]] understands
   (nil/true, a message string, false, or a throw). Only a name, an index and the
   value being judged ever cross."
  ([request-json] (request-json! request-json nil nil))
  ([request-json validators-json run]
   (let
     [request
      (json/read-json (str request-json) :key-fn identity)

      counts
      (when (and run (not (str/blank? (str validators-json))))
        (json/read-json (str validators-json) :key-fn identity))]

     (when-not (map? request) (invalid-request! "request must be a JSON object"))
     (-> request
         (dissoc "channel_id" "channel_ids")
         (cond->
           (seq counts)
           (update "fields" #(if (sequential? %) (attach-validators % counts run) %)))
         request!
         answer->wire
         json/write-json-str))))

;; -- The Python seam ---------------------------------------------------------

(def ^:private live-ops
  "The live verb's op grammar, the same shape the shell verb's is:
   `open` MOUNTS a view and answers the id every later op names, and each handle
   op drives the view that id stands for.

   `packages/vis-contract/resources/vis-contract/python-host.edn` declares the
   same table for the packaged module and for a host outside Vis, and
   `contract.python-host-test` is what fails when the two drift: an op the engine
   does not know would make an extension that runs inside Vis refuse outside it."
  {:default "open" :spawn #{"open"} :handle #{"patch" "state" "close"}})

(defn- live-op-name
  "The op `opts` asks for, refused BY NAME when it is in neither list. An options
   map with no `op` means the default one, exactly as a shell options map does."
  [opts]
  (let [op (or (trimmed (get opts "op")) (:default live-ops))]
    (when-not (or (contains? (:spawn live-ops) op) (contains? (:handle live-ops) op))
      (throw (ex-info (str "unknown live view op " (pr-str op)
                           " — use " (str/join ", "
                                               (sort (into (:spawn live-ops) (:handle live-ops)))))
                      {:type :vis/human-input-unknown-live-op :op op})))
    op))

(defn- live-handle-id
  "The view a handle op names."
  [opts op]
  (or (trimmed (get opts "view_id"))
      (invalid-live-patch! (str "a live " op " names no view_id — pass the id `open` answered"))))

(defn- live-ended
  "The answer for a view that is no longer open: its verdict read back off the
   RECORD.

   The registry drops a view the moment it ends, so an extension that pushes into
   a view the human interrupted would otherwise learn only that it is gone. The
   sink file still holds the trailer, so it learns WHY — which is the whole
   difference between an unattended loop that stops and one that keeps working
   for a screen nobody is watching."
  [view-id]
  (let [result (live-sink/verdict (live-sink/view-file (ambient-session-id) view-id))]
    (when-not result
      (invalid-live-patch!
        (str "no live view " view-id " is open — it was closed, interrupted, or never opened")))
    {:view-id view-id :is-open false :result result}))

(defn live-dispatch
  "One live-view op: an options map with wire keys in, the answer map out.

   `open` answers the mounted view, `patch` the sequence number the engine
   accepted, `state` what the view looks like right now, and `close` the verdict
   the model reads. Every answer says `:is-open`, so the caller learns from the
   op it was making that the human stopped watching — the record's verdict comes
   back with it."
  [opts]
  (when-not (map? opts) (invalid-live-view! "a live envelope must be a JSON object"))
  (let [op (live-op-name opts)]
    (case op
      "open"
      (let [view (open-live! (get opts "view"))]
        {:view-id (:id view) :is-open true :view view})

      "patch"
      ;; Asked BEFORE patching rather than catching the refusal after: a patch
      ;; the human's interrupt raced is not a spec problem, and an author must
      ;; not have to tell those two apart by reading a message.
      (let [view-id (live-handle-id opts op)]
        (if (live-view view-id)
          {:view-id view-id :is-open true :seq (:seq (patch-live! view-id (get opts "patch")))}
          (live-ended view-id)))

      "state"
      (let [view-id (live-handle-id opts op)]
        (if-let [view (live-view view-id)]
          {:view-id view-id :is-open true :view view}
          (live-ended view-id)))

      "close"
      (let [view-id (live-handle-id opts op)]
        (if-let [result (close-live! view-id (or (get opts "ending") {}))]
          {:view-id view-id :is-open false :result result}
          (live-ended view-id))))))

(defn live-json!
  "The strings-only seam a Python extension crosses for a live view: one JSON
   envelope in, one JSON answer out.

   Channel routing is host-side — a `channel_id`/`channel_ids` key is dropped
   rather than minting keywords from guest data — so an extension always reaches
   the channels the host picked, exactly as [[request-json!]] does.

   Nothing blocks. A form parks the extension because it owes the human a value;
   a view owes nobody anything, so the push crosses, the surfaces learn, and the
   extension carries on."
  [envelope-json]
  (let [envelope (json/read-json (str envelope-json) :key-fn identity)]
    (when-not (map? envelope) (invalid-live-view! "a live envelope must be a JSON object"))
    (-> envelope
        (dissoc "channel_id" "channel_ids")
        live-dispatch
        wire/json-str)))
