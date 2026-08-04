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

   Secrets never travel as plaintext. A `:password` field resolves to an opaque
   `vis-secret:<uuid>` handle; the plaintext stays in a process-local vault and
   is readable only through [[reveal-secret]] from the trusted extension side.
   Handles are what land in logs, transcripts and wire payloads, so a leaked
   event is worthless."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as channel-events]
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

;; =============================================================================
;; Secret vault
;; =============================================================================

(def ^:private max-secrets
  "The vault only has to bridge a submitted password to the extension that asked
   for it. Keeping it bounded means a long session cannot accumulate plaintext
   forever when an extension forgets to call [[forget-secret!]]."
  128)

(defn- evict-oldest
  [vault]
  (if (<= (count vault) max-secrets)
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

;; =============================================================================
;; Normalization — request/field specs
;; =============================================================================

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
   dropped, every value inside its own field's domain, and a `:password` as the
   vault HANDLE rather than the plaintext."
  [request-id fields answer]
  (if-let [why (hi-spec/answer-error fields answer)]
    (invalid-answer! request-id why)
    answer))

(def ^:private field-keys
  "Every key a VALUE field spec may carry, in its canonical snake_case spelling.
   `id` and `help` are the legacy names of `name` and `description`.

   Layout is deliberately NOT in here: `fields` and `direction` belong to a
   `group` — see [[layout-keys]]."
  #{"name" "id" "type" "label" "description" "help" "is_required" "placeholder" "options"
    "min_length" "max_length" "default" "min" "max" "step" "validate"})

(def ^:private layout-keys
  "The two keys only a `group` has. A field that holds an ANSWER carrying one of
   them is a spec that meant to group and forgot to say so: dropping the key in
   silence drew the form flat and sent the author hunting for a layout bug in
   the surfaces, so it is refused with the fix in the message."
  #{"fields" "direction"})

(def ^:private group-keys
  "Every key a `group` may carry. A layout node has no answer, so every key that
   describes ONE value — a default, a placeholder, options, rules — is refused
   here instead of being silently ignored on a node that can never use it."
  #{"name" "id" "type" "label" "description" "help" "fields" "direction"})

(def ^:private option-keys "Every key one `:options` entry may carry." #{"value" "label"})

(def ^:private request-keys
  "Every key a request spec may carry, in its canonical snake_case spelling."
  #{"id" "title" "description" "source" "fields" "submit_label" "cancel_label" "is_cancellable"
    "timeout_ms" "session_id" "channel_id" "channel_ids"})

(defn- snake-key
  "`k` as the canonical snake_case name it is reaching for: `:is-required`,
   `\"is-required\"` and `\"isRequired\"` all canonicalize to `\"is_required\"`."
  [k]
  (-> (if (keyword? k) (subs (str k) 1) (str k))
      (str/replace #"([a-z0-9])([A-Z])" "$1_$2")
      (str/replace "-" "_")
      str/lower-case))

(defn- kebab-key [canonical] (str/replace canonical "_" "-"))

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

(defn- normalize-bool
  [field-id label value fallback]
  (cond (nil? value) fallback
        (boolean? value) value
        (= "true" (str value)) true
        (= "false" (str value)) false
        :else (invalid-field! field-id (str label " must be a boolean"))))

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

(def ^:private range-defaults
  "A `:range` with no bounds is a percentage — the one scale every operator
   already reads without being told what the numbers mean."
  {:min 0 :max 100 :step 1})

(defn- normalize-range
  "The three numbers a slider needs. `:step` is the increment a surface nudges
   by, NOT a validation rule: the engine only refuses a value outside the
   bounds, so a client that types an exact number is never argued with."
  [field-id field]
  (let
    [lo
     (normalize-number field-id ":min" (pick field "min" :min) (:min range-defaults))

     hi
     (normalize-number field-id ":max" (pick field "max" :max) (:max range-defaults))

     step
     (normalize-number field-id ":step" (pick field "step" :step) (:step range-defaults))]

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

(declare coerce-value normalize-group)

(defn normalize-field
  "Validate one field spec and return its internal form. Throws `ex-info` with
   `:type :vis/human-input-invalid-field` on a bad spec.

   Three names, three jobs, and every field ends up with all three:

     - `:name` is how the answer is KEYED — the key the extension reads back out
       of `:values` (`:id` is the historical alias, accepted and still emitted).
     - `:label` is how the field is SHOWN. Never blank: a field without one
       shows its `:name`, so no surface ever draws a bare, unlabelled input.
     - `:description` is the prose under that label, rendered in italic by every
       dialog (`:help` is its legacy alias).

   A `group` is the exception, and the only one: it holds no answer, so it needs
   no name and gets no label unless the spec wants a heading. See
   [[normalize-group]]."
  [field]
  (when-not (map? field) (invalid-field! nil "field must be a map"))
  ;; No key check yet: which keys are legal depends on whether this node holds
  ;; an answer or only lays one out, and that is the `:type` below.
  (let
    [field-id
     (trimmed (pick field "name" :name "id" :id))

     type-name
     (or (trimmed (pick field "type" :type)) "plaintext")

     is-group
     (= "group" (str/lower-case type-name))

     _
     ;; Layout keys are refused BEFORE the generic key check so the message can
     ;; say what the author actually meant: `fields` on a `plaintext` is a group
     ;; that forgot its `:type`, not a misspelling.
     (when-not is-group
       (when-let [k (first (sort (filter layout-keys (map snake-key (keys field)))))]
         (invalid-field! field-id
                         (str "key \""
                              k
                              "\" only exists on a group — a field that holds an answer has"
                              " nothing to lay out. Put these fields inside"
                              " {\"type\": \"group\", \"direction\": \"row\"} instead."))))

     _
     (when-not is-group (check-keys! "field" field-keys field #(invalid-field! field-id %)))

     _
     (when-not (or field-id is-group) (invalid-field! nil "field needs a non-blank :name"))

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
      (if is-group
        (normalize-group field-id field)
        (let
          [description
           (trimmed (pick field "description" :description "help" :help))

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
             (assoc :options
               (normalize-options field-id field-type (pick field "options" :options)))

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
            (assoc :default default)))))))

(defn- normalize-group
  "Validate a `group` — the layout node — and return its internal form.

   A group answers nothing: it has `:fields` of its own and a `:direction` they
   run in, and because a child may itself be a group, `row` and `column` compose
   into any arrangement without a single new key. Its `:name` is optional; when
   the spec does not give one it is derived from the children, so a surface
   still has a stable key to draw rows under and no author has to invent an
   identifier for a box that only exists to hold two fields side by side."
  [field-id field]
  (check-keys! "group" group-keys field #(invalid-field! field-id %))
  (let
    [raw
     (pick field "fields" :fields)

     _
     (when-not (sequential? raw) (invalid-field! field-id "group needs a :fields sequence"))

     children
     (mapv normalize-field raw)

     _
     (when (empty? children) (invalid-field! field-id "group needs at least one field"))

     id
     (or field-id (str "group:" (str/join "+" (map :name children))))

     description
     (trimmed (pick field "description" :description "help" :help))]

    (cond->
      {:id id
       :name id
       :type :group
       :direction (normalize-direction field-id (pick field "direction" :direction))
       :fields children}
      (trimmed (pick field "label" :label))
      (assoc :label (trimmed (pick field "label" :label)))

      description
      (assoc :description description))))

(defn- leaves!
  "Conjoin every ANSWERABLE field of `fields` onto TRANSIENT vector `acc`, depth
   first. The accumulator is threaded through the recursion, so a tree of any
   depth is flattened in one pass onto one array — where the `mapcat` shape this
   replaces allocated a lazy seq and a fresh vector for every group it entered."
  [acc fields]
  (reduce (fn [acc {:keys [type] :as field}]
            (if (= :group type) (leaves! acc (:fields field)) (conj! acc field)))
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
            (if (= :group type) (nodes! (conj! acc field) (:fields field)) (conj! acc field)))
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
               (= :group type)
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
  [request]
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
           :else (invalid-request! ":channel-ids must be a keyword or a sequence of keywords"))]

    (when (empty? ids) (invalid-request! ":channel-ids must not be empty"))
    (when-not (every? keyword? ids) (invalid-request! ":channel-ids must be keywords"))
    ids))

(defn- normalize-timeout
  "The request's deadline in milliseconds, or [[no-timeout-ms]] when it waits
   indefinitely.

   A missing key means [[default-timeout-ms]]; `0` is the one way to ask for no
   deadline; a negative number is refused. Nothing is CLAMPED — a caller who
   wants to wait all day says 0 and means it, so quietly shortening a stated
   budget would only lie about when the answer arrives."
  [request]
  (let
    [raw
     (pick request "timeout_ms" :timeout-ms)

     ms
     (if (nil? raw)
       default-timeout-ms
       (or (if (number? raw) (long raw) (parse-long (str raw)))
           (invalid-request!
             ":timeout-ms must be a number of milliseconds, or 0 to wait indefinitely")))]

    (when (neg? (long ms))
      (invalid-request! ":timeout-ms must not be negative — 0 waits indefinitely"))
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
     (mapv normalize-field raw-fields)

     _
     (when (empty? fields) (invalid-request! ":fields must not be empty"))

     _
     (when-not (apply distinct? (map :name (all-fields fields)))
       (invalid-request! "field names must be distinct"))

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
         :timeout-ms (normalize-timeout request)
         :channel-ids (normalize-channel-ids request)}
        session-id
        (assoc :session-id session-id)

        (trimmed (pick request "description" :description))
        (assoc :description (trimmed (pick request "description" :description)))

        (trimmed (pick request "source" :source))
        (assoc :source (trimmed (pick request "source" :source)))))))

;; =============================================================================
;; Value coercion — one implementation for defaults and submissions
;; =============================================================================

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
     (if (number? lo) lo (:min range-defaults))

     hi
     (if (number? hi) hi (:max range-defaults))

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
          (every? integer? [lo hi (if (number? st) st (:step range-defaults))])
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
   `:password` values are replaced with opaque vault handles."
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

;; =============================================================================
;; Channel projection
;; =============================================================================

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
      (dissoc :promise :channel-ids)
      (assoc :fields (map-fields #(dissoc % :is-secret :validate) (:fields request)))))

(defn- publish!
  "Publish `event` on every channel in `channel-ids` and return how many
   listeners it actually reached across all of them."
  [channel-ids event]
  (transduce (map #(channel-events/publish-channel-event! % event)) + 0 channel-ids))

;; =============================================================================
;; Registry
;; =============================================================================

(defn pending-requests
  "Snapshot of the currently pending requests, oldest first. Views only."
  []
  (->> (vals @pending)
       (sort-by :created-at)
       (mapv request->view)))

(defn pending-request
  "The pending request `request-id`, as a view, or nil."
  [request-id]
  (some-> (get @pending request-id)
          request->view))

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
    (let [outcome (coerce-values (:fields entry) values)]
      (if (:is-accepted outcome)
        (if (settle! request-id
                     {:is-submitted true
                      :reason "submitted"
                      :request-id request-id
                      :values (:values outcome)})
          {:is-accepted true}
          {:is-accepted false :reason "unknown"})
        outcome))
    {:is-accepted false :reason "unknown"}))

(defn- force-cancel!
  "Settle `request-id` as cancelled no matter what the request declared. The
   shutdown path: a detaching channel or a closing session must never leave a
   thread parked, even on a request whose author forbade dismissal."
  [request-id reason]
  (some? (settle!
           request-id
           {:is-submitted false :reason (or (trimmed reason) "cancelled") :request-id request-id})))

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

   `:password` values in `:values` are opaque handles — see [[reveal-secret]]."
  [request]
  (let
    [entry
     (assoc (normalize-request request)
       :promise (promise)
       :created-at (System/currentTimeMillis))

     request-id
     (:id entry)]

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
         ;; wall kills the thread at `Timeout (120s)` with the dialog still up.
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

;; =============================================================================
;; Strings-only boundary — what a Python extension actually calls
;; =============================================================================

(defn answer->wire
  "Wire projection of a [[request!]] answer: snake_case string keys, JSON-safe
   values. `:password` values stay opaque handles."
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
