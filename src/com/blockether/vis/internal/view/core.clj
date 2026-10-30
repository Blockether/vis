(ns com.blockether.vis.internal.view.core
  "The one lifecycle for every operator-facing View.

   A View has a CLOSED semantic document, a stable id, and the same `open`, `patch`
   and `close` rail on every channel. Its `:kind` declares the capability policy:

   - `:input` is Human Input — a typed form that BLOCKS until a `submit` or
     `cancel` action, timeout, or interruption;
   - `:live` is a non-blocking picture driven by its producer and optionally ended
     by an `interrupt` action.

   The distinction belongs in policy, not transport. Both kinds share the pending
   registry, publish `:view/open` / `:view/patch` / `:view/close` envelopes, and
   receive operator intent through [[action!]]. Renderers dispatch on `:kind` and
   never infer behavior from an event name or resource path.

   This namespace PARSES extension data against the CLOSED vocabulary declared by
   [[com.blockether.vis.contract.view]]. Input answers are coerced and checked
   once at the settle seam; live patches are normalized and materialized once before
   any surface sees them. Unknown keys are refused, while every declared key is
   preserved through normalization.

   Secrets never travel as plaintext. A `:password` and an `:otp` field resolve to
   an opaque `vis-secret:<uuid>` handle; plaintext stays in a process-local vault
   and is readable only through [[reveal-secret]] from the trusted extension side."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.channel.events :as channel-events]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.view.materializer :as materializer]
            [com.blockether.vis.internal.view.sink :as sink]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.contract.view :as view-spec]
            [com.blockether.vis.internal.view.validation :as validation]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

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

(def ^:dynamic *block-owner*
  "The current block's owner token: an atom that becomes true when execution ends.
   Host callbacks inherit this binding. Views keep it until they close."
  nil)

(def ^:dynamic *live-owner*
  "The extension whose host door opens or drives a live view, as `{:extension name
   :context ctx :listener f}`, or nil. Only an owner opens a `panel` view. The `python_execution` sandbox has no owner, so it never opens one.

   `:listener` receives each human event as `(listener event session-id)` on a
   thread of its own. The door that made it binds that session for the callback."
  nil)

(defonce ^:private human-stops
  ;; The live views that a PERSON stopped while the block that opened them still ran,
  ;; keyed by that block's owner token. The block takes them once, when it ends.
  (atom {}))

(defn human-stop-text
  "One sentence for the model about the live view that a person stopped: its title,
   their note and what the model must do next."
  [{:keys [title note]}]
  (str "The person stopped the live view \""
       title
       "\""
       (if note (str " with this note: " note) " without a note")
       ". Do not watch it again. Read the note and answer the person."))

(defn take-human-stops!
  "The live views that a person stopped while the block of `block-owner` ran, oldest
   first, as `{:title … :note …}` maps. Each stop is answered once."
  [block-owner]
  (if-not block-owner
    []
    (let [[old _] (swap-vals! human-stops dissoc block-owner)]
      (get old block-owner []))))

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
  (let [handle (str view-spec/secret-handle-prefix (random-uuid))]
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
  "First non-nil value among `ks`: the keys one value may be written under, such
   as a field's `:name` or `:id`.

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
  (throw (ex-info (str "Invalid input View field" (when field-id (str " " field-id)) ": " message)
                  {:type :vis/view-invalid-field :field-id field-id :reason message})))

(defn- invalid-request!
  [message]
  (throw (ex-info (str "Invalid input View request: " message)
                  {:type :vis/view-invalid-request :reason message})))

(defn- invalid-answer!
  [request-id message]
  (throw (ex-info (str "Invalid input View answer for " request-id ": " message)
                  {:type :vis/view-invalid-answer :request-id request-id :reason message})))

(defn- invalid-view-action!
  [view-id message]
  (throw (ex-info (str "Invalid View action for " view-id ": " message)
                  {:type :vis/view-invalid-action :view-id (str view-id) :reason message})))

(defn- unsupported-view-action!
  [view-id kind action]
  (throw
    (ex-info
      (str "View action " (name action) " is not supported by " (name kind) " Views")
      {:type :vis/view-action-not-supported :view-id (str view-id) :kind kind :action action})))

(defn- checked-field
  "`field` once it satisfies the declared contract, else a refusal naming it. The
   parsing below refuses bad INPUT key by key; this refuses a normalized form no
   surface could paint, whoever built it."
  [field-id field]
  (if-let [why (view-spec/field-error field)]
    (invalid-field! field-id why)
    field))

(defn- checked-request
  [request]
  (if-let [why (view-spec/request-error request)]
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
  (if-let [why (view-spec/answer-error fields answer)]
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

(def ^:private field-keys "Every key a VALUE field spec may carry." view-spec/field-keys)

(def ^:private layout-keys
  "The keys only a `group` has. A field that holds an ANSWER carrying one of them
   is a spec that meant to group and forgot to say so: dropping the key in
   silence drew the form flat and sent the author hunting for a layout bug in the
   surfaces, so it is refused with the fix in the message."
  view-spec/layout-keys)

(def ^:private option-keys "Every key one `:options` entry may carry." view-spec/option-keys)

(def ^:private request-keys
  "Every key a request spec may carry. `:channel-id` is the singular spelling of
   `:channel-ids` — a one-channel convenience, and the only spec key with no
   counterpart in the normalized form."
  (conj view-spec/request-keys :channel-id))

(def ^:private view-action-decl-keys
  "Every key each closed operator action may carry."
  view-spec/view-action-key-sets)

(defn- check-keys!
  "Refuse a spec key that is not in `allowed`. A spec is keyword-keyed: a
   Python/JSON spec had its keys converted once by [[spec<-json]], so a key
   written any other way is refused naming the spelling that would match.

   Silence was the bug: `{'isRequired': True}` from a Python extension parsed as
   clean JSON, matched nothing, and left a mandatory field optional on every
   surface — the human simply skipped it. A misspelled key now names its own
   fix instead of disappearing."
  [what allowed m fail!]
  (doseq [k
          (keys m)

          :when (not (contains? allowed k))
          :let [canonical
                (snake-key k)

                engine-key
                (keyword (kebab-key canonical))]]

    (fail! (cond (not (contains? allowed engine-key))
                 (str "unknown " what
                      " key " (pr-str k)
                      " — expected one of " (str/join ", " (sort (map snake-key allowed))))
                 (= k canonical) (str what " key " (pr-str k) " must be the keyword " engine-key)
                 :else (str what
                            " key "
                            (pr-str k)
                            " is misspelled — write \""
                            canonical
                            "\" (Python/JSON) or "
                            engine-key
                            " (Clojure)")))))

(defn spec<-json
  "A spec decoded from JSON — a Python extension's request, live view, patch or
   ending — with its keys converted ONCE to the engine keywords the parser reads.
   Only a canonical snake_case key converts: any other spelling stays the string
   it arrived as, so [[check-keys!]] refuses it naming the fix rather than a
   second spelling quietly meaning the same thing."
  [x]
  (cond (map? x) (persistent! (reduce-kv (fn [m k v]
                                           (assoc! m
                                                   (if (and (string? k) (= k (snake-key k)))
                                                     (wire/engine-key k)
                                                     k)
                                                   (spec<-json v)))
                                         (transient {})
                                         x))
        (sequential? x) (mapv spec<-json x)
        :else x))

(defn- normalize-option
  [field-id option]
  (when (map? option) (check-keys! "option" option-keys option #(invalid-field! field-id %)))
  (let [[value label]
        (if (map? option) [(:value option) (:label option)] [option option])

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
  (let [lo
        (normalize-number field-id ":min" (:min field) (:min view-spec/range-defaults))

        hi
        (normalize-number field-id ":max" (:max field) (:max view-spec/range-defaults))

        step
        (normalize-number field-id ":step" (:step field) (:step view-spec/range-defaults))]

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
  (let [hi
        (or (normalize-length field-id ":max_length" (:max-length field))
            (long (:length view-spec/otp-defaults)))

        lo
        (or (normalize-length field-id ":min_length" (:min-length field)) hi)]

    (when (> (long lo) (long hi))
      (invalid-field! field-id ":max_length must be at least :min_length"))
    (when (> (long hi) (long (:ceiling view-spec/otp-defaults)))
      (invalid-field!
        field-id
        (str ":max_length must be at most " (:ceiling view-spec/otp-defaults) " digits")))
    {:min-length lo :max-length hi}))

(declare coerce-value)

(defn- raw-type-name
  "The lower-case type name of a RAW spec node, from a string or a keyword."
  [node]
  (let [type (:type node)]
    (str/lower-case (or (if (keyword? type) (name type) (trimmed type)) ""))))

(defn- group-node?
  "True when this RAW spec node asks to be a layout group. Decided from the type
   name alone, and decided FIRST: which keys are legal, whether a `:name` is
   required and whether the node may hold children all follow from the answer."
  [node]
  (= view-spec/group-type-name (raw-type-name node)))

(defn normalize-field
  "Validate one FIELD spec — a leaf holding exactly one answer — and return its
   internal form. Throws `ex-info` with `:type :vis/view-invalid-field`
   on a bad spec.

   Three names, three jobs, and every field ends up with all three:

     - `:name` is how the answer is KEYED — the key the extension reads back out
       of `:values` (`:id` is the historical alias, accepted and still emitted).
     - `:label` is how the field is SHOWN. Never blank: a field without one
       shows its `:name`, so no surface ever draws a bare, unlabelled input.
     - `:description` is the prose under that label, rendered in italic by every
       dialog.

   A layout group or a display component is not a field and never arrives
   here: [[normalize-component]] routes it before a single value key is parsed."
  [field]
  (when-not (map? field) (invalid-field! nil "field must be a map"))
  (when (group-node? field)
    (invalid-field! (trimmed (pick field :name :id))
                    (str "a group is not a field — it holds no answer, only the fields it"
                         " arranges. Normalize a node of the tree with normalize-node.")))
  (when-let [display (get view-spec/live-node-types
                          (str/lower-case (or (trimmed (:type field)) "")))]
    (invalid-field! (trimmed (pick field :name :id))
                    (str "a " (name display)
                         " is a display component, not a field — it holds no answer."
                         " Normalize a node of the tree with normalize-node.")))
  (let [field-id
        (trimmed (pick field :name :id))

        _
        ;; Layout keys are refused BEFORE the generic key check so the message can
        ;; say what the author actually meant: `fields` on a `plaintext` is a group
        ;; that forgot its `:type`, not a misspelling.
        (when-let [k (first (sort (filter layout-keys (keys field))))]
          (invalid-field! field-id
                          (str "key \""
                               (snake-key k)
                               "\" only exists on a group — a field that holds an answer has"
                               " nothing to lay out. Put these fields inside"
                               " {\"type\": \"group\", \"direction\": \"row\"} instead.")))

        _
        (check-keys! "field" field-keys field #(invalid-field! field-id %))

        _
        (when-not field-id (invalid-field! nil "field needs a non-blank :name"))

        type-name
        (or (trimmed (:type field)) "plaintext")

        field-type
        (get view-spec/field-types (str/lower-case type-name))

        _
        (when-not field-type
          (invalid-field! field-id
                          (str "unknown type " (pr-str type-name)
                               " — expected one of "
                               (str/join ", " (sort (keys view-spec/field-types))))))]

    (checked-field
      field-id
      (let [description
            (trimmed (:description field))

            ;; An `:otp` derives its own lengths from the same two keys — how many
            ;; boxes it draws IS its length — so it must not be length-checked twice.
            min-length
            (when-not (= :otp field-type)
              (normalize-length field-id ":min_length" (:min-length field)))

            max-length
            (when-not (= :otp field-type)
              (normalize-length field-id ":max_length" (:max-length field)))

            validate
            (validation/normalize-validators (:validate field) #(invalid-field! field-id %))

            spec
            (cond-> {:id field-id
                     ;; The same string under both keys: `:name` is the contract a spec
                     ;; writes, `:id` is what every surface has always keyed rows and errors
                     ;; by. One field identity, two spellings, no drift between them.
                     :name field-id
                     :type field-type
                     :label (or (trimmed (:label field)) field-id)
                     ;; Optional unless the caller says otherwise — the same default every
                     ;; form API has, so a spec never blocks a human on a field the
                     ;; extension did not actually need.
                     :is-required
                     (normalize-bool field-id ":is-required" (:is-required field) false)
                     :is-secret (contains? view-spec/secret-types field-type)}
              description
              (assoc :description description)

              (trimmed (:placeholder field))
              (assoc :placeholder (trimmed (:placeholder field)))

              (contains? view-spec/choice-types field-type)
              (assoc :options (normalize-options field-id field-type (:options field)))

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
            (:default field)

            _
            (when (and (:is-secret spec) (some? raw-default))
              (invalid-field! field-id "secret fields must not have a :default"))

            [status default]
            (coerce-value (assoc spec :is-required false) raw-default)]

        (when (= :error status) (invalid-field! field-id (str "invalid :default — " default)))
        (cond-> spec
          (some? default)
          (assoc :default default))))))

(defn- leaves!
  "Conjoin every ANSWERABLE field of `fields` onto TRANSIENT vector `acc`, depth
   first. A group is walked through and a decoration is dropped — neither holds
   a value, so neither can key `:values`. The accumulator is threaded through
   the recursion, so a tree of any depth is flattened in one pass onto one array
   — where the `mapcat` shape this replaces allocated a lazy seq and a fresh
   vector for every group it entered."
  [acc fields]
  (reduce (fn [acc {:keys [type] :as field}]
            (cond (= view-spec/group-type type) (leaves! acc (:children field))
                  (view-spec/decoration? field) acc
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
            (if (= view-spec/group-type type)
              (nodes! (conj! acc field) (:children field))
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
               (= view-spec/group-type type)
               (update :children #(map-fields f %)))))
        fields))

(defn- ambient-session-id
  "Session id of the extension environment currently executing, or nil.

   A request raised inside a gateway session has to NAME that session: the
   gateway bridge turns the request into a session event so the companion app
   learns the run is blocked, and a session event with no session has nowhere
   to go. A deref failure answers nil."
  []
  (try (let [env
             extension/*current-environment*

             env
             (if (instance? clojure.lang.IDeref env) (deref env) env)]

         (when (map? env) (trimmed (:session-id env))))
       (catch Throwable _ nil)))

(defn- normalize-channel-ids
  [request fail!]
  (let [ids
        (pick request :channel-ids :channel-id)

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
  (let [raw
        (:timeout-ms spec)

        ms
        (if (nil? raw)
          fallback
          (or (if (number? raw) (long raw) (parse-long (str raw)))
              (fail! ":timeout-ms must be a number of milliseconds, or 0 to wait indefinitely")))]

    (when (neg? (long ms)) (fail! ":timeout-ms must not be negative — 0 waits indefinitely"))
    (long ms)))

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
                  {:type :vis/view-invalid-live-view :reason message})))

(defn- invalid-live-patch!
  [message]
  (throw (ex-info (str "Invalid live-view patch: " message)
                  {:type :vis/view-invalid-patch :reason message})))

(defn- checked-live-view
  [view]
  (if-let [why (view-spec/live-view-error view)]
    (invalid-live-view! why)
    view))

(defn- checked-live-node
  "`node` once it satisfies the node contract, else the reason it does not,
   refused by whoever was declaring it."
  [fail! node]
  (if-let [why (view-spec/live-node-error node)]
    (fail! why)
    node))

(defn- checked-live-patch
  [patch]
  (if-let [why (view-spec/live-patch-error patch)]
    (invalid-live-patch! why)
    patch))

(def ^:private live-view-decl-keys
  "Every key a live-view SPEC may write: the view's vocabulary minus the engine's
   own stamps, plus the singular `:channel-id` [[normalize-channel-ids]] accepts."
  (conj (reduce disj view-spec/live-view-keys view-spec/live-view-stamp-keys) :channel-id))

(def ^:private live-node-decl-keys
  "Every key a NODE spec may write. `total_lines` is the engine's stamp on a log:
   the size of the RECORD is counted, never claimed. `direction` and `children`
   belong to a layout GROUP and are refused here BY NAME, so a `status` written
   with children hears about them instead of having them dropped."
  (reduce disj view-spec/live-node-keys #{:total-lines :clicks :direction :children}))

(def ^:private live-group-decl-keys
  "Every key a live layout GROUP spec may write: its own vocabulary, so `lines` on
   a `row` is refused by name rather than quietly ignored."
  view-spec/live-group-keys)

(def ^:private live-patch-decl-keys
  "The one key a patch spec may write. `view_id` and `seq` are stamps: a caller
   who could choose the seq could replay a patch, and one who could choose the
   view could patch somebody else's."
  (reduce disj view-spec/live-patch-keys #{:view-id :seq}))

(def ^:private live-order-decl-keys
  "Every key a `{:by …}` table order may write."
  view-spec/live-sorted-keys)

(def ^:private live-op-decl-keys
  "Every key ONE operation may write, per operation — the op's own `:op` chooses
   the set, so a `clear` carrying the lines it meant to `append` is refused
   instead of silently emptying the node."
  view-spec/live-op-key-sets)

(def ^:private live-item-keys
  "Every key one item of a keyed collection may write, by the key holding them."
  {:columns view-spec/live-column-keys
   :rows view-spec/live-row-keys
   :stats view-spec/live-stat-keys
   :steps view-spec/live-step-keys
   :links view-spec/live-link-keys
   :groups view-spec/live-table-group-keys
   :objects view-spec/live-scene-object-keys
   :object-changes view-spec/live-scene-object-keys
   :meshes view-spec/live-scene-mesh-keys})

(def ^:private live-item-name
  "What one item of each keyed collection is CALLED, so a refusal names the thing
   the author wrote rather than the key it arrived under."
  {:columns "column"
   :rows "row"
   :stats "stat"
   :steps "step"
   :links "link"
   :groups "table group"
   :objects "scene object"
   :object-changes "scene object"
   :meshes "scene mesh"})

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
  (let [n (if (number? value)
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
  (cond (some? declared) (live-term fail! ":target-kind" view-spec/link-targets declared)
        (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*://" target) :url
        :else (fail! (str ":target-kind must be one of "
                          (str/join ", " (sort (keys view-spec/link-targets)))
                          " — only a target carrying a scheme names its own kind"))))

(defn- scene-number
  "A finite number as a double. A scene places, turns and sizes in plain numbers."
  [fail! what value]
  (if (and (number? value) (Double/isFinite (double value)))
    (double value)
    (fail! (str what " must be a finite number"))))

(defn- scene-color
  [fail! what value]
  (let [color (trimmed value)]
    (if (and color (re-matches #"#[0-9A-Fa-f]{6}" color))
      (str/lower-case color)
      (fail! (str what " must be a colour like #33ccff")))))

(def ^:private scene-object-keys
  "The keys of one scene object besides its id, in the order a refusal checks them."
  [:mesh :x :y :z :rx :ry :rz :scale :color :is-emissive])

(defn- scene-object
  "One scene object. A declaration (`full?`) fills every default and needs a mesh. A
   change keeps only the keys it names, so a moved object costs its new place only."
  [fail! item full?]
  (let [given (reduce (fn [acc k]
                        (if-some [value (get item k)]
                          (assoc acc
                            k
                            (case k
                              :mesh (live-text fail! ":mesh" value)
                              :color (scene-color fail! ":color" value)
                              :is-emissive (bool-value fail! ":is-emissive" value false)
                              (scene-number fail! (str k) value)))
                          acc))
                      {}
                      scene-object-keys)]
    (if full?
      (do (when-not (:mesh given) (fail! "a scene object needs a :mesh"))
          (merge (:object view-spec/scene-defaults) given))
      given)))

(defn- scene-mesh
  [fail! {:keys [vertices faces]}]
  (letfn [(triples [what value]
            (when-not (and (sequential? value) (every? #(and (sequential? %) (= 3 (count %))) value))
              (fail! (str what " must be a list of three-number lists")))
            value)]
    {:vertices (mapv (fn [vertex]
                       (mapv #(scene-number fail! "a vertex coordinate" %) vertex))
                     (triples ":vertices" vertices))
     :faces (mapv (fn [face]
                    (mapv #(live-long fail! "a face index" %) face))
                  (triples ":faces" faces))}))

(defn- normalize-live-item
  "One item of a keyed collection — a column, row, stat, step or link. All five
   are normalized by the same three rules: closed keys, an `:id` that ADDRESSES
   the item, and every closed vocabulary looked up rather than minted."
  [fail! kind item]
  (let [what (live-item-name kind)]
    (when-not (map? item) (fail! (str "a " what " must be a map")))
    (check-keys! what (live-item-keys kind) item fail!)
    (let [id (or (trimmed (:id item))
                 (fail! (str "a " what " needs a non-blank :id — a patch addresses it by that id")))
          item-fail! (fn [message]
                       (fail! (str what " " id ": " message)))
          label (trimmed (:label item))
          tone (some->> (:tone item)
                        (live-term item-fail! ":tone" view-spec/live-tones))]

      (case kind
        :columns
        (cond-> {:id id :label (or label (item-fail! "a column needs a :label"))}
          (some? (:align item))
          (assoc :align (live-term item-fail! ":align" view-spec/live-aligns (:align item))))

        :rows
        (cond-> {:id id :cells (text-items item-fail! ":cells" (or (:cells item) []))}
          tone
          (assoc :tone tone)

          (trimmed (:parent item))
          (assoc :parent (trimmed (:parent item))))

        ;; A DECLARED group: the id `:parent` points at, plus what the surfaces
        ;; need to paint a head the rows never carry — a label that may change
        ;; without moving the group, a tone, a place in the order, and whether
        ;; the producer wants it open on arrival.
        :groups
        (cond-> {:id id}
          label
          (assoc :label label)

          tone
          (assoc :tone tone)

          (some? (:order item))
          (assoc :order (live-long item-fail! ":order" (:order item)))

          (some? (:is-open item))
          (assoc :is-open (bool-value item-fail! ":is-open" (:is-open item) false)))

        :stats
        (cond-> {:id id
                 :label (or label (item-fail! "a stat needs a :label"))
                 :value-text (live-shown item-fail! ":value-text" (:value-text item))}
          tone
          (assoc :tone tone))

        :steps
        (cond-> {:id id
                 :label (or label (item-fail! "a step needs a :label"))
                 :tone (or tone :idle)}
          (trimmed (:detail item))
          (assoc :detail (trimmed (:detail item)))

          (some? (:value item))
          (assoc :value (live-fraction item-fail! ":value" (:value item))))

        :links
        (let [target (live-text item-fail! ":target" (:target item))]
          (cond-> {:id id
                   :label (or label (item-fail! "a link needs a :label"))
                   :target target
                   :target-kind (live-target-kind item-fail! (:target-kind item) target)}
            tone
            (assoc :tone tone)))

        :objects
        (assoc (scene-object item-fail! item true) :id id)

        :object-changes
        (assoc (scene-object item-fail! item false) :id id)

        :meshes
        (assoc (scene-mesh item-fail! item) :id id)))))

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
                         (cond-> {:by (live-text fail! "a `{:by …}` order's :by" (:by order))}
                           (some? (:dir order))
                           (assoc :dir
                             (live-term fail! ":dir" view-spec/live-sort-dirs (:dir order)))))
        :else (live-term fail! ":order" view-spec/live-orders order)))

(defn- input-node
  "One input component. A form takes the field as declared. A live view also
   takes its current `:value`, coerced into the field domain; a secret input
   never declares one, because only the host holds its handle."
  [fail! context node]
  (if (= :form context)
    (do (when (contains? node :value)
          (invalid-field! (trimmed (pick node :name :id))
                          "a form input takes :default, not :value"))
        (normalize-field node))
    (let [field
          (try (normalize-field (dissoc node :value))
               (catch clojure.lang.ExceptionInfo e
                 (if (= :vis/view-invalid-field (:type (ex-data e)))
                   (fail! (str "field " (:field-id (ex-data e)) ": " (:reason (ex-data e))))
                   (throw e))))

          raw
          (:value node)]

      (cond (nil? raw) field
            (:is-secret field) (fail! (str "field " (:id field) ": a secret input has no :value"))
            :else (let [[status value] (coerce-value (assoc field :is-required false) raw)]
                    (when (= :error status)
                      (fail! (str "field " (:id field) ": invalid :value — " value)))
                    (cond-> field
                      (some? value)
                      (assoc :value value)))))))

(defn- input-node?
  "True when this RAW spec node asks for an input component: neither a group nor
   a display, data or action component. A node without a type is `plaintext`."
  [node]
  (and (not (group-node? node)) (not (contains? view-spec/live-node-types (raw-type-name node)))))

(defn- grid-columns
  "The `:max-columns` of a grid group, a whole number inside the contract bounds.
   Only a grid has columns to cap, so the key on another direction is refused."
  [fail! node]
  (let [[lo hi]
        view-spec/grid-columns

        raw
        (:max-columns node)

        n
        (if (number? raw) raw (parse-long (str raw)))]

    (when-not (= "grid"
                 (some-> (:direction node)
                         name
                         str/lower-case
                         str/trim))
      (fail! ":max_columns only applies to a group with direction \"grid\""))
    (when-not (and (integer? n) (<= (long lo) (long n) (long hi)))
      (fail! (str ":max_columns must be a whole number from " lo " to " hi)))
    (long n)))

(def ^:private ^:dynamic *host-patch*
  "True while the host writes a patch itself. Its messages keep their authors and it
   may write Send receipts. An owner's patch is never trusted that way."
  false)

(defn- owner-author
  "The author of a message that the current owner writes: its extension by name,
   or the agent of the `python_execution` sandbox."
  []
  (if-let [extension-name (:extension *live-owner*)]
    {:kind "extension" :name extension-name}
    {:kind "agent"}))

(defn- enum-text
  "An enum value of an app map as wire text: a keyword reads as its name."
  [value]
  (if (keyword? value) (name value) value))

(defn- app-anchor
  "One diff line that a thread belongs to: `{:path :side :line}`."
  [fail! anchor]
  (when-not (map? anchor) (fail! "an :anchor is a map of :path, :side and :line"))
  (check-keys! "anchor" #{:path :side :line} anchor fail!)
  {:path (live-text fail! "an anchor's :path" (:path anchor))
   :side (enum-text (:side anchor))
   :line (live-long fail! "an anchor's :line" (:line anchor))})

(defn- app-message
  "One posted message. The host stamps the current owner as its author; only a
   patch that the host writes itself keeps the author it names."
  [fail! message]
  (when-not (map? message) (fail! "a message must be a map"))
  (check-keys! "message" #{:id :author :body :created-at :submission-id} message fail!)
  (when-not (and (string? (:body message)) (not (str/blank? (:body message))))
    (fail! "a message :body must be non-blank text"))
  (cond-> {:id (or (trimmed (:id message)) (str (random-uuid)))
           :author (if *host-patch*
                     (let [author (:author message)]
                       (cond-> {:kind (enum-text (:kind author))}
                         (trimmed (:name author))
                         (assoc :name (trimmed (:name author)))))
                     (owner-author))
           :body (:body message)
           :created-at (if (some? (:created-at message))
                         (live-long fail! "a message's :created-at" (:created-at message))
                         (util/now-ms))}
    (trimmed (:submission-id message))
    (assoc :submission-id (trimmed (:submission-id message)))))

(defn- app-thread
  "One thread: a stable id, an optional anchor, its messages and the saved draft
   of the human. A blank draft is no draft."
  [fail! thread]
  (when-not (map? thread) (fail! "a thread must be a map"))
  (check-keys! "thread" #{:id :anchor :messages :draft :is-resolved} thread fail!)
  (let [id
        (or (trimmed (:id thread)) (fail! "a thread needs a non-blank :id"))

        thread-fail!
        (fn [message]
          (fail! (str "thread " id ": " message)))

        messages
        (or (:messages thread) [])

        draft
        (:draft thread)]

    (when-not (sequential? messages) (thread-fail! ":messages must be a sequence"))
    (when-not (or (nil? draft) (string? draft)) (thread-fail! "a :draft must be text"))
    (cond-> {:id id :messages (mapv #(app-message thread-fail! %) messages)}
      (some? (:anchor thread))
      (assoc :anchor (app-anchor thread-fail! (:anchor thread)))

      (util/non-blank-string? draft)
      (assoc :draft draft)

      (some? (:is-resolved thread))
      (assoc :is-resolved (bool-value thread-fail! ":is-resolved" (:is-resolved thread) false)))))

(defn- app-threads
  "The threads of a diff: each has an anchor, and no two share an id."
  [fail! threads]
  (when-not (sequential? threads) (fail! ":threads must be a sequence of threads"))
  (let [parsed (mapv #(app-thread fail! %) threads)]
    (when-let [loose (first (remove :anchor parsed))]
      (fail! (str "diff thread " (:id loose) " needs an :anchor")))
    (when-not (or (empty? parsed) (apply distinct? (map :id parsed)))
      (fail! "diff thread ids must be distinct"))
    parsed))

(defn- diff-patch
  [fail! patch]
  (if (string? patch) patch (fail! "a diff :patch must be unified diff text")))

(defn- live-node
  "One declared component of a form or a live view. `fail!` is the refusal of
   whoever declares it — a view spec, a request, or the `add-node` op that
   introduces one mid-run — so one parser serves all three without any of them
   learning the others' error type. `context` is `:form` or `:live`.

   A layout GROUP is parsed here too, by recursion: a view arranges its work
   exactly the way a form arranges its questions, and a row inside a column
   costs no second grammar."
  ([fail! node] (live-node fail! :live node))
  ([fail! context node]
   (when-not (map? node) (fail! "a node must be a map"))
   (if (input-node? node)
     (input-node fail! context node)
     (let [is-group (group-node? node)]
       (check-keys! (if is-group "group" "node")
                    (if is-group live-group-decl-keys live-node-decl-keys)
                    node
                    fail!)
       (if is-group
         (let [id (or (trimmed (:id node))
                      (fail!
                        (str "a group needs an id, because patches and `after` name it: "
                             "write vis.ui.group('actions', ...) or vis.ui.wrap('actions', ...)")))
               group-fail! (fn [message]
                             (fail! (str "group " id ": " message)))
               children (:children node)
               label (trimmed (:label node))
               description (trimmed (:description node))]

           (when-not (and (sequential? children) (seq children))
             (group-fail!
               "a group needs a non-empty :children — a row arranging nothing is a typo"))
           (checked-live-node
             group-fail!
             (cond-> {:id id
                      :type view-spec/group-type
                      :direction (live-term group-fail!
                                            ":direction"
                                            view-spec/group-directions
                                            (or (:direction node) "column"))
                      :children (mapv #(live-node group-fail! context %) children)}
               label
               (assoc :label label)

               description
               (assoc :description description)

               (some? (:max-columns node))
               (assoc :max-columns (grid-columns group-fail! node))

               (some? (:is-collapsible node))
               (assoc :is-collapsible
                 (bool-value group-fail! ":is-collapsible" (:is-collapsible node) false))

               (some? (:default-expanded node))
               (assoc :default-expanded
                 (bool-value group-fail! ":default-expanded" (:default-expanded node) false)))))
         (let [id (or (trimmed (:id node))
                      (fail!
                        "a node needs a non-blank :id — every patch names the node it speaks to"))
               node-fail! (fn [message]
                            (fail! (str "node " id ": " message)))
               type (live-term node-fail! ":type" view-spec/live-node-types (:type node))
               label (trimmed (:label node))
               base (cond-> {:id id :type type}
                      label
                      (assoc :label label))
               items (fn [kind]
                       (normalize-live-items node-fail! kind (get node kind)))]

           (checked-live-node
             node-fail!
             (case type
               :divider
               (do (check-keys! "divider" #{:id :type} node node-fail!) base)

               :status
               (cond-> (assoc base
                         :text (live-text node-fail! "a status' :text" (:text node))
                         :tone (or (some->> (:tone node)
                                            (live-term node-fail! ":tone" view-spec/live-tones))
                                   :idle))
                 (trimmed (:detail node))
                 (assoc :detail (trimmed (:detail node))))

               (:paragraph :heading :code)
               (cond-> (assoc base
                         :text
                         (if (= type :code)
                           (let [text (:text node)]
                             (if (string? text) text (node-fail! "code :text must be a string")))
                           (live-text node-fail! ":text" (:text node))))
                 (= type :heading)
                 (assoc :level (live-long node-fail! ":level" (or (:level node) 2)))

                 (and (= type :code) (some? (:language node)))
                 (assoc :language (live-text node-fail! ":language" (:language node))))

               :spinner
               (assoc base
                 :text (live-text node-fail! ":text" (or (:text node) "Working"))
                 :variant (live-term node-fail!
                                     ":variant"
                                     view-spec/spinner-variants
                                     (or (:variant node) "braille"))
                 :is-active (bool-value node-fail! ":is-active" (:is-active node) true))

               :button
               (assoc base
                 :label (live-text node-fail! "button :label" (:label node))
                 :action (live-term node-fail!
                                    ":action"
                                    view-spec/button-actions
                                    (or (:action node) "activate"))
                 :is-disabled (bool-value node-fail! ":is-disabled" (:is-disabled node) false))

               :card
               (cond-> (assoc base :title (live-text node-fail! "a card's :title" (:title node)))
                 (string? (:text node))
                 (assoc :text (:text node))

                 (some? (:tone node))
                 (assoc :tone (live-term node-fail! ":tone" view-spec/live-tones (:tone node)))

                 (trimmed (:meta node))
                 (assoc :meta (trimmed (:meta node)))

                 (some? (:is-selectable node))
                 (assoc :is-selectable
                   (bool-value node-fail! ":is-selectable" (:is-selectable node) false)))

               :progress
               (cond-> base
                 (some? (:value node))
                 (assoc :value (live-fraction node-fail! ":value" (:value node)))

                 (some? (:done node))
                 (assoc :done (live-long node-fail! ":done" (:done node)))

                 (some? (:total node))
                 (assoc :total (live-long node-fail! ":total" (:total node))))

               :stat
               (assoc base :stats (items :stats))

               :steps
               (assoc base :steps (items :steps))

               :log
               (cond-> (assoc base
                         :lines (mapv materializer/log-text
                                      (text-items node-fail! ":lines" (or (:lines node) [])))
                         :window-lines (if-some [window (:window-lines node)]
                                         (live-long node-fail! ":window-lines" window)
                                         (long (:window-lines view-spec/log-defaults))))
                 (some? (:line-tones node))
                 (assoc :line-tones
                   (let [tones (:line-tones node)]
                     (when-not (and (sequential? tones) (= (count tones) (count (:lines node))))
                       (node-fail! ":line-tones must have one tone or nil per line"))
                     (mapv #(when (some? %)
                              (live-term node-fail! ":line-tones" view-spec/live-tones %))
                           tones)))

                 (some? (:default-expanded node))
                 (assoc :default-expanded
                   (bool-value node-fail! ":default-expanded" (:default-expanded node) false)))

               :table
               (cond-> (assoc base
                         :columns (items :columns)
                         :rows (items :rows)
                         :max-rows (if-some [bound (:max-rows node)]
                                     (live-long node-fail! ":max-rows" bound)
                                     (long (:max-rows view-spec/table-defaults)))
                         :order (normalize-live-order node-fail! (:order node)))
                 (seq (:groups node))
                 (assoc :groups (items :groups))

                 (some? (:is-selectable node))
                 (assoc :is-selectable
                   (bool-value node-fail! ":is-selectable" (:is-selectable node) false))

                 (some? (:selected-ids node))
                 (assoc :selected-ids (text-items node-fail! ":selected-ids" (:selected-ids node))))

               :link
               (assoc base :links (items :links))

               :scene
               (let [{:keys [background fov is-wrapping tick-ms]} view-spec/scene-defaults]
                 (assoc base
                   :width (scene-number node-fail! ":width" (:width node))
                   :height (scene-number node-fail! ":height" (:height node))
                   :background (scene-color node-fail! ":background" (or (:background node) background))
                   :fov (scene-number node-fail! ":fov" (or (:fov node) fov))
                   :is-wrapping (bool-value node-fail! ":is-wrapping" (:is-wrapping node) is-wrapping)
                   :tick-ms (live-long node-fail! ":tick-ms" (or (:tick-ms node) tick-ms))
                   :meshes (items :meshes)
                   :objects (items :objects)))

               :diff
               (cond-> (assoc base :patch (diff-patch node-fail! (:patch node)))
                 (some? (:old-revision node))
                 (assoc :old-revision (live-text node-fail! ":old-revision" (:old-revision node)))

                 (some? (:new-revision node))
                 (assoc :new-revision (live-text node-fail! ":new-revision" (:new-revision node)))

                 (some? (:threads node))
                 (assoc :threads (app-threads node-fail! (:threads node))))

               :thread
               (merge base
                      (dissoc (app-thread
                                node-fail!
                                (assoc (select-keys node [:anchor :messages :draft :is-resolved])
                                  :id id))
                        :id))))))))))

(defn normalize-live-node
  "Validate one declared live node, so an unknown key or a `:tone` outside the
   table throws where the node was built rather than in front of the human."
  [node]
  (live-node invalid-live-view! node))

(defn- form-ids
  "RAW form nodes with a positional `:id` on each group and display component
   that has none. A form never patches a node, so this id only keys a row."
  [nodes]
  (let [counter (volatile! 0)]
    (letfn [(walk [nodes]
              (mapv (fn [node]
                      (if-not (map? node)
                        node
                        (let [node (cond-> node
                                     (and (group-node? node) (sequential? (:children node)))
                                     (update :children walk))]
                          (if (or (input-node? node) (trimmed (:id node)))
                            node
                            (assoc node
                              :id (str (or (not-empty (raw-type-name node)) "node")
                                       "-"
                                       (vswap! counter #(inc (long %)))))))))
                    nodes))]
      (walk nodes))))

(defn- form-node [node] (live-node #(invalid-field! nil %) :form node))

(defn normalize-node
  "Validate one node of a request's component TREE and return its internal form.
   Throws `ex-info` with `:type :vis/view-invalid-field` on a bad spec.

   A form uses the same component catalog as a live view: an input holds one
   answer, a group arranges its children, and any other component shows text or
   state without an answer."
  [node]
  (form-node (first (form-ids [node]))))

(defn normalize-request
  "Validate an input View request spec and return its internal form. Throws
   `ex-info` with View-owned error data on a bad spec."
  [request]
  (when-not (map? request) (invalid-request! "request must be a map"))
  (check-keys! "request" request-keys request invalid-request!)
  (let [title
        (trimmed (:title request))

        _
        (when-not title (invalid-request! "request needs a non-blank :title"))

        raw-nodes
        (:nodes request)

        _
        (when-not (sequential? raw-nodes) (invalid-request! ":nodes must be a sequence"))

        nodes
        (mapv form-node (form-ids raw-nodes))

        _
        (when (empty? nodes) (invalid-request! ":nodes must not be empty"))

        _
        (let [names (into [] (keep :name) (all-fields nodes))]
          (when-not (or (empty? names) (apply distinct? names))
            (invalid-request! "field names must be distinct")))

        session-id
        (or (trimmed (:session-id request)) (ambient-session-id))]

    (checked-request
      (cond-> {:id (or (trimmed (:id request)) (str (random-uuid)))
               :title title
               :nodes nodes
               :submit-label (or (trimmed (:submit-label request)) "Submit")
               :cancel-label (or (trimmed (:cancel-label request)) "Cancel")
               :is-cancellable (normalize-bool nil ":is-cancellable" (:is-cancellable request) true)
               :timeout-ms (normalize-timeout request default-timeout-ms invalid-request!)
               :channel-ids (normalize-channel-ids request invalid-request!)}
        session-id
        (assoc :session-id session-id)

        (trimmed (:description request))
        (assoc :description (trimmed (:description request)))

        (trimmed (:source request))
        (assoc :source (trimmed (:source request)))))))

(defn live-nodes?
  "Whether `nodes` are the picture a human WATCHES rather than the questions a
   form asks. A layout group is the ONE node both vocabularies share, so a
   builder composing one asks its children which check it is dated against — and
   a group inside a group answers by its own children."
  [nodes]
  (boolean (some (fn [node]
                   (when (map? node)
                     (if (group-node? node)
                       (live-nodes? (:children node))
                       (and (some? (:id node))
                            (contains? view-spec/live-node-types
                                       (some-> (trimmed (:type node))
                                               str/lower-case))))))
                 nodes)))

(defn- live-shortcuts
  "The shortcuts of an app spec or a `shortcuts` op, refused by shape here. Whether
   each key is unique and each `node_id` is a button of the app is the answer of
   the view they land on."
  [fail! value]
  (when-not (sequential? value) (fail! ":shortcuts must be a list"))
  (let [shortcuts (vec value)]
    (some-> (some app/shortcut-shape-error shortcuts)
            fail!)
    shortcuts))

(defn normalize-live-view
  "Validate a live-view spec and return the view the materializer holds. Throws
   `ex-info` with `:type :vis/view-invalid-live-view` on a bad spec, so a
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
  (let [title
        (or (trimmed (:title view)) (invalid-live-view! "a live view needs a non-blank :title"))

        raw-nodes
        (:nodes view)

        _
        (when-not (sequential? raw-nodes) (invalid-live-view! ":nodes must be a sequence"))

        nodes
        (mapv #(live-node invalid-live-view! %) raw-nodes)

        _
        (when (empty? nodes) (invalid-live-view! ":nodes must not be empty"))

        _
        (when-not (or (empty? nodes) (apply distinct? (mapv :id nodes)))
          (invalid-live-view! (str "node ids must be distinct — got "
                                   (str/join ", " (sort (mapv :id nodes))))))

        placement
        (live-term invalid-live-view!
                   ":placement"
                   view-spec/placements
                   (or (:placement view) "inline"))

        app?
        (= app/placement placement)

        ;; A panel view needs its extension and a key. An app takes both when they
        ;; exist: the sandbox opens apps too, and a key only makes an app adoptable.
        owned?
        (not (contains? #{:inline app/placement} placement))

        extension-name
        (cond owned? (or (trimmed (:extension *live-owner*))
                         (invalid-live-view! (str "only extension code can open a "
                                                  (name placement)
                                                  " view; the python_execution sandbox cannot")))
              app? (trimmed (:extension *live-owner*)))

        _
        (when (and owned? (not (trimmed (:key view))))
          (invalid-live-view! (str "a " (name placement) " view needs a :key that names it")))

        session-id
        (or (trimmed (:session-id view)) (ambient-session-id))

        shortcuts
        (when (contains? view :shortcuts) (live-shortcuts invalid-live-view! (:shortcuts view)))

        _
        (some-> (app/shortcuts-error (cond-> {:placement placement :nodes nodes}
                                       shortcuts
                                       (assoc :shortcuts shortcuts)))
                invalid-live-view!)

        agent-note
        (when (contains? view :agent-note)
          (some-> (app/agent-note-error {:placement placement} (:agent-note view))
                  invalid-live-view!)
          (trimmed (:agent-note view)))]

    (checked-live-view
      (cond-> {:id (str (random-uuid))
               :title title
               :placement placement
               :nodes nodes
               :timeout-ms (normalize-timeout view no-timeout-ms invalid-live-view!)
               :channel-ids (normalize-channel-ids view invalid-live-view!)
               :seq 0
               :created-at (util/now-ms)}
        (and extension/*current-invocation-id* (not owned?))
        (assoc :owner
          (cond-> {:invocation-id extension/*current-invocation-id*}
            extension/*activity-history-id*
            (assoc :activity-id extension/*activity-history-id*)))

        extension-name
        (assoc :extension extension-name)

        session-id
        (assoc :session-id session-id)

        (trimmed (:description view))
        (assoc :description (trimmed (:description view)))

        (trimmed (:source view))
        (assoc :source (trimmed (:source view)))

        (trimmed (:key view))
        (assoc :key (trimmed (:key view)))

        (seq shortcuts)
        (assoc :shortcuts shortcuts)

        agent-note
        (assoc :agent-note agent-note)))))

(def ^:private live-op-value
  "How ONE key of a patch operation is normalized, by key. A table rather than a
   branch per operation, so `:tone` means the same thing in every op that carries
   one and no op can grow a private reading of a shared key."
  {:node-id (fn [fail! value]
              (live-text fail! ":node-id" value))
   :after (fn [fail! value]
            (live-text fail! ":after" value))
   :text (fn [fail! value]
           (if (string? value) value (fail! ":text must be a string")))
   :level (fn [fail! value]
            (live-long fail! ":level" value))
   :language (fn [fail! value]
               (live-text fail! ":language" value))
   :variant (fn [fail! value]
              (live-term fail! ":variant" view-spec/spinner-variants value))
   :is-active (fn [fail! value]
                (bool-value fail! ":is-active" value true))
   :is-disabled (fn [fail! value]
                  (bool-value fail! ":is-disabled" value false))
   :clicks (fn [fail! value]
             (live-long fail! ":clicks" value))
   ;; A BLANK detail clears it: a declaration leaves an empty detail out, so a
   ;; `set` must be able to take one away again instead of refusing the patch.
   :detail (fn [fail! value]
             (if (and (string? value) (str/blank? value)) "" (live-text fail! ":detail" value)))
   :label (fn [fail! value]
            (live-text fail! ":label" value))
   :title (fn [fail! value]
            (live-text fail! ":title" value))
   :meta (fn [fail! value]
           (live-text fail! ":meta" value))
   :is-selectable (fn [fail! value]
                    (bool-value fail! ":is-selectable" value false))
   :tone (fn [fail! value]
           (live-term fail! ":tone" view-spec/live-tones value))
   :value (fn [fail! value]
            (live-fraction fail! ":value" value))
   :done (fn [fail! value]
           (live-long fail! ":done" value))
   :total (fn [fail! value]
            (live-long fail! ":total" value))
   :lines (fn [fail! value]
            (mapv materializer/log-text (text-items fail! ":lines" value)))
   :item-ids (fn [fail! value]
               (text-items fail! ":item-ids" value))
   :selected-ids (fn [fail! value]
                   (text-items fail! ":selected-ids" value))
   :rows (fn [fail! value]
           (normalize-live-items fail! :rows value))
   :groups (fn [fail! value]
             (normalize-live-items fail! :groups value))
   :stats (fn [fail! value]
            (normalize-live-items fail! :stats value))
   :steps (fn [fail! value]
            (normalize-live-items fail! :steps value))
   :links (fn [fail! value]
            (normalize-live-items fail! :links value))
   :objects (fn [fail! value]
              (normalize-live-items fail! :object-changes value))
   :meshes (fn [fail! value]
             (normalize-live-items fail! :meshes value))
   :background (fn [fail! value]
                 (scene-color fail! ":background" value))
   :fov (fn [fail! value]
          (scene-number fail! ":fov" value))
   :is-wrapping (fn [fail! value]
                  (bool-value fail! ":is-wrapping" value false))
   :tick-ms (fn [fail! value]
              (live-long fail! ":tick-ms" value))
   :patch (fn [fail! value]
            (diff-patch fail! value))
   :old-revision (fn [fail! value]
                   (live-text fail! ":old-revision" value))
   :new-revision (fn [fail! value]
                   (live-text fail! ":new-revision" value))
   :thread (fn [fail! value]
             (app-thread fail! value))
   :submission (fn [fail! value]
                 (if (map? value) value (fail! ":submission must be a receipt map")))
   :shortcuts (fn [fail! value]
                (live-shortcuts fail! value))
   :node-spec (fn [fail! value]
                (live-node fail! value))})

(defn- live-op
  "One patch operation, normalized against the closed key set its own `:op`
   chooses."
  [fail! op]
  (when-not (map? op) (fail! "an operation must be a map"))
  (let [kind
        (live-term fail! ":op" view-spec/live-ops (:op op))

        ;; "an append op", "a set op" — the refusal is read out loud by whoever wrote it.
        article
        (if (contains? #{\a \e \i \o \u} (first (name kind))) "an " "a ")

        op-fail!
        (fn [message]
          (fail! (str article (name kind) " op: " message)))]

    (check-keys! (str (name kind) " op") (live-op-decl-keys kind) op op-fail!)
    (when (and (= :replace kind) (not (sequential? (:rows op))))
      (op-fail! "needs :rows, the whole row set — an empty list empties the table"))
    (if (= :set-value kind)
      ;; The value domain belongs to the input it lands on, so the materializer
      ;; checks it against that node. Here a nil value is a clear, not a gap.
      (do (when-not (contains? op :value) (op-fail! "needs :value — nil clears the input"))
          {:op kind :node-id ((live-op-value :node-id) op-fail! (:node-id op)) :value (:value op)})
      (reduce (fn [acc k]
                (if-some [value (get op k)]
                  (assoc acc k ((live-op-value k) op-fail! value))
                  acc))
              {:op kind}
              (disj (view-spec/live-op-key-sets kind) :op)))))

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
   bound, is `materializer/apply-patch`'s answer."
  [view patch]
  (let [ops (cond (map? patch)
                  (do (check-keys! "patch" live-patch-decl-keys patch invalid-live-patch!)
                      (:ops patch))
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
  (let [text
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
  (let [text
        (trimmed value)

        allowed
        (set (map :value options))]

    (cond (nil? text) (if is-required [:error "is required"] [:ok nil])
          (contains? allowed text) [:ok text]
          :else [:error (str "must be one of " (str/join ", " (sort allowed)))])))

(defn- coerce-multiselect
  [{:keys [is-required options]} value]
  (let [values (cond (nil? value) []
                     (string? value) [value]
                     (sequential? value) (vec value)
                     (set? value) (vec value)
                     :else ::invalid)]
    (if (= ::invalid values)
      [:error "must be a list of option values"]
      (let [picked (into [] (comp (keep trimmed) (distinct)) values)
            allowed (set (map :value options))
            unknown (remove allowed picked)]

        (cond (seq unknown) [:error (str "unknown option " (str/join ", " (sort unknown)))]
              (and is-required (empty? picked)) [:error "is required"]
              :else [:ok picked])))))

(defn- coerce-checkbox
  [{:keys [is-required]} value]
  (let [[status result] (cond (nil? value) [:ok false]
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
  (let [lo
        (if (number? lo) lo (:min view-spec/range-defaults))

        hi
        (if (number? hi) hi (:max view-spec/range-defaults))

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
          (every? integer? [lo hi (if (number? st) st (:step view-spec/range-defaults))])
          [:ok (long (Math/round (double n)))]
          :else [:ok (double n)])))

(defn- coerce-otp
  "A one-time code is DIGITS, however the human pasted them. Spaces and dashes
   are how every provider prints a code (`123 456`, `123-456`), so they are
   separators here rather than a typo the operator has to go back and delete."
  [{:keys [is-required min-length max-length]} value]
  (let [lo
        (long (or min-length (:length view-spec/otp-defaults)))

        hi
        (long (or max-length (:length view-spec/otp-defaults)))

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
  (cond (contains? view-spec/text-types type) (coerce-text field value)
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
  (let [values
        (or values {})

        fields
        (vec fields)

        n
        (long (count fields))]

    (loop [i
           0

           out
           (transient {})

           errs
           (transient {})]

      (if-not (< i n)
        {:values (persistent! out) :errors (persistent! errs)}
        (let [{:keys [id] :as field}
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
                                    (if-let [message
                                             (validation/check validate (get values id) values)]
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
  [fields raw-values]
  (let [fields
        (input-fields fields)

        coerced
        (coerce-all fields raw-values)

        {:keys [values errors]}
        (check-all fields coerced)

        ;; Only multiselect coercion echoes submitted values. The other built-in
        ;; messages describe the spec: a one-letter password must not erase that
        ;; letter from "must be one of ...". Custom validator errors are untrusted.
        static-errors
        (into #{}
              (keep (fn [{:keys [id type]}]
                      (when (and (contains? (:errors coerced) id) (not= :multiselect type)) id)))
              fields)

        secrets
        (mapcat (fn [{:keys [id]}]
                  [(get raw-values id) (get raw-values (keyword id)) (get values id)])
                (filter :is-secret fields))]

    (if (seq errors)
      {:is-accepted false
       :errors
       (into {}
             (map (fn [[id message]]
                    [id (util/redact-secret-text message (when-not (static-errors id) secrets))]))
             errors)}
      {:is-accepted true :values values})))

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
   `:direction` and `:children`, so both surfaces lay the form out from the same
   data instead of each inventing a layout."
  [request]
  (-> request
      (dissoc :promise :channel-ids :kind)
      (assoc :nodes (map-fields (fn [field]
                                  (cond-> (dissoc field :is-secret :validate)
                                    (contains? view-spec/secret-types (:type field))
                                    (dissoc :default)))
                                (:nodes request)))))

(def ^:private request-stamps
  "The engine's own request stamps, wire spelling -> normalized key."
  (into {} (map (juxt snake-key identity)) view-spec/request-stamp-keys))

(defn view<-wire
  "Inverse of [[request->view]] for a view that CROSSED A PROCESS BOUNDARY — the
   canonical snake_case map a `view.open` session event carries.

   A run parked inside `vis-agent serve` publishes on an in-process channel bus that
   never leaves that JVM, so for every other process the session event IS the
   request. Rebuilding the view goes back through the engine's own parser rather
   than through a second field vocabulary; only the stamps of [[request!]],
   which `normalize-request` refuses by contract, are lifted across unchanged."
  [wire]
  (when (map? wire)
    (let [stamps (into {}
                       (keep (fn [[wire-key k]]
                               (when-some [v (get wire wire-key)]
                                 ;; A map stamp such as `origin` takes engine keys too.
                                 [k (if (map? v) (wire/->engine v) v)])))
                       request-stamps)]
      (merge (request->view (normalize-request (spec<-json
                                                 (apply dissoc wire (keys request-stamps)))))
             stamps))))

;; A live view crossing a process boundary
;;
;; The request inverse above RE-PARSES, because a request is a declaration and the
;; parser is the only reader of that vocabulary. A materialized view cannot be: it
;; carries what the ENGINE stamped (its own id, the `:seq` every surface orders by,
;; a log's `:total-lines`) and everything the patches have already put in it, none
;; of which a spec may write — re-parsing one would mint a second id and drop the
;; counters. So this inverse is MECHANICAL: the closed key sets give the spelling
;; back, the closed term tables give the keywords back, and what comes out is
;; checked against the same spec the materializer itself answers to.

(def ^:private live-wire-keys
  "Wire spelling -> engine key, for every key a live view, a patch or a verdict may
   carry. Derived from the closed sets `hi-spec` declares: a second copy of the
   vocabulary is a copy that drifts."
  (into {}
        (map (juxt snake-key identity))
        (reduce into
                #{}
                [view-spec/live-view-keys view-spec/live-node-keys view-spec/live-group-keys
                 view-spec/live-column-keys view-spec/live-table-group-keys view-spec/live-row-keys
                 view-spec/live-stat-keys view-spec/live-step-keys view-spec/live-link-keys
                 view-spec/live-sorted-keys view-spec/live-patch-keys view-spec/live-op-keys
                 view-spec/live-result-keys view-spec/live-picture-keys
                 view-spec/live-elided-keys])))

(def ^:private live-wire-terms
  "The CLOSED table each keyword-valued key takes its value from. A term outside its
   table stays the text it arrived as, so the spec check refuses the frame instead
   of minting a keyword no surface knows how to paint."
  {:type view-spec/component-types
   :tone view-spec/live-tones
   :line-tones view-spec/live-tones
   :op view-spec/live-ops
   :variant view-spec/spinner-variants
   :order view-spec/live-orders
   :dir view-spec/live-sort-dirs
   :align view-spec/live-aligns
   :direction view-spec/group-directions
   :target-kind view-spec/link-targets
   :placement view-spec/placements
   :reason view-spec/live-reasons
   ;; A replayed button must stay a Send: `:submit` is compared as a keyword.
   :action view-spec/button-actions})

(defn- live<-wire
  "One decoded live value as the engine holds it: canonical keys back to their
   engine keywords, closed vocabularies back to their terms, everything else the
   data it already is."
  [x]
  (cond (map? x) (persistent!
                   (reduce-kv
                     (fn [m k v]
                       (let [canonical
                             (snake-key k)

                             k*
                             (or (get live-wire-keys canonical) (keyword (kebab-key canonical)))

                             table
                             (get live-wire-terms k*)]

                         (assoc! m
                                 k*
                                 (cond (and table (string? v)) (get table v v)
                                       (and table (sequential? v)) (mapv #(get table % %) v)
                                       ;; The channels the view was published on: keywords on
                                       ;; both sides, rendered as their names by the encoder.
                                       (and (= :channel-ids k*) (sequential? v)) (mapv keyword v)
                                       :else (live<-wire v)))))
                     (transient {})
                     x))
        (sequential? x) (mapv live<-wire x)
        :else x))

(defn- live<-wire-checked
  "`wire` as the engine's own `what`, or nil when it is not one this engine could
   have published. A frame a surface cannot paint is dropped SAYING SO: half a
   frame is how a pane starts disagreeing with the phone about a row."
  [what error-of wire]
  (when (map? wire)
    (let [value (live<-wire wire)]
      (if-let [why (error-of value)]
        (do (tel/log! {:level :warn
                       :id ::live-frame-unreadable
                       :data {:what what :reason why}
                       :msg (str "Dropped an unreadable live-view " what " from another process")})
            nil)
        value))))

(defn live-view<-wire
  "Inverse of the wire projection for a MATERIALIZED live view — the canonical
   snake_case map a `view.open` session event carries, and the shape
   the live-views resync answers with.

   A run SHOWING its work inside `vis-agent serve` publishes on an in-process channel
   bus that never leaves that JVM, so for every other process this event IS the
   view. [[view<-wire]] is the same door for a form."
  [wire]
  (live<-wire-checked "view" view-spec/live-view-error wire))

(defn live-patch<-wire
  "Inverse of the wire projection for one accepted patch — what a
   `view.patch` session event carries. The gateway COALESCES patches
   before it journals them, so `:seq` names the last engine patch the frame folded
   in, and a surface that already applied it can tell."
  [wire]
  (live<-wire-checked "patch" view-spec/live-patch-error wire))

(defn live-result<-wire
  "Inverse of the wire projection for a view's VERDICT — what a `view.close`
   session event carries."
  [wire]
  (live<-wire-checked "verdict" view-spec/live-result-error wire))

(defn- publish!
  "Publish `event` on every channel in `channel-ids` and return how many
   listeners it actually reached across all of them."
  [channel-ids event]
  (transduce (map #(channel-events/publish-channel-event! % event)) + 0 channel-ids))

(defn- lifecycle-event
  "Canonical channel envelope for either View kind."
  [op entry payload]
  (merge {:op op :kind (:kind entry) :view-id (:id entry) :session-id (:session-id entry)} payload))

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
   claim a person ended it. `:model-result` is the optional compact string returned
   to the model while the complete verdict remains human-facing."
  (into (reduce disj
                view-spec/live-result-keys
                #{:view-id :is-completed :view :elided :is-from-human :note})
        #{:selection-snapshots :model-result}))

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

;; Owned views — a panel view that its extension keeps open
;;
;; An extension and a key own one view. It outlives the call that opened it and
;; closes on an explicit close or when its extension unloads. The store keeps the
;; last picture, so after a gateway restart the view shows as stale until its
;; extension loads again. A load that does not open the same key closes it.

(defonce ^:private stale-views
  ;; View id -> the saved picture of an owned view that no extension opened again
  ;; since this gateway started. It paints as stale and accepts only a dismiss.
  (atom {}))

(defn- owned-key
  "`[extension key]` of an owned panel view, or nil for an inline view or an app."
  [view]
  (when (and (:extension view) (not (app/app? view))) [(:extension view) (:key view)]))

(defonce ^:private owned-saver
  (delay (Executors/newSingleThreadScheduledExecutor (util/daemon-thread-factory
                                                       "vis-owned-view-save"))))

(defonce ^:private save-due (atom false))

(defonce ^:private ended-owned
  ;; View id -> session id of each owned view that closed in this process, so a
  ;; late push still reads its verdict. An owned view has no ambient session.
  (atom {}))

(defn- owned-pictures
  "The current picture of each open owned view, then each stale one."
  []
  (into (->> (vals @pending)
             (filter :owned-key)
             (sort-by :created-at)
             (mapv #(deref (:view %))))
        (map #(dissoc % :is-stale))
        (sort-by :created-at (vals @stale-views))))

(defn flush-owned!
  "Save the owned pictures now when a change waits for a save. Call it before the
   process stops, so the last 500 ms of patches are not lost."
  []
  (when (compare-and-set! save-due true false)
    (try (sink/save-owned! (owned-pictures))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::owned-save-failed
                      :data {:error (ex-message t)}
                      :msg "Vis could not save the pictures of owned live views"}))))
  nil)

(defn- save-owned-soon!
  "Save the owned pictures within 500 ms. Many patches in that time make one write."
  []
  (when (compare-and-set! save-due false true)
    (.schedule ^ScheduledExecutorService @owned-saver
               ^Runnable flush-owned!
               500
               TimeUnit/MILLISECONDS))
  nil)

(defn- stale-close!
  "Forget stale view `view` and tell the surfaces that it closed with `reason`."
  [view reason]
  (when (contains? (first (swap-vals! stale-views dissoc (:id view))) (:id view))
    (publish! (:channel-ids view)
              {:op :view/close
               :kind :live
               :view-id (:id view)
               :session-id (:session-id view)
               :result {:view-id (:id view)
                        :reason reason
                        :is-completed false
                        :view (select-keys view view-spec/live-picture-keys)}})
    true))

(defn restore-owned!
  "Load the saved pictures of owned views as stale views. A key that is open
   already keeps its live view. Returns the number of stale views."
  []
  (let [open-keys (set (keep :owned-key (vals @pending)))]
    (doseq [saved (sink/load-owned)
            :let [view (assoc (live<-wire saved) :is-stale true)
                  why (view-spec/live-view-error view)]
            :when (and (string? (:id view)) (owned-key view) (not (open-keys (owned-key view))))]

      (if why
        ;; An old picture, such as a removed placement, does not come back.
        (tel/log! {:level :warn
                   :id ::owned-restore-dropped
                   :data {:view-id (:id view) :extension (:extension view) :error why}
                   :msg "Vis dropped a saved owned live view that is no longer valid"})
        (swap! stale-views assoc (:id view) view)))
    (count @stale-views)))

;; Apps — live views with the `attachment` placement
;;
;; An app outlives the block and the extension that opened it. Its record is the
;; store of truth: a restart replays it, and a closed app stays readable. Each
;; session lists its apps next to the records, so a key adopts the app again.

(defonce ^:private closed-apps
  ;; View id -> the last picture of each closed app that this process read.
  (atom {}))

(defn- app-key
  "What adopts app `view` again: its session, its extension and its key. nil
   without a key."
  [view]
  (when-let [k (:key view)]
    [(:session-id view) (:extension view) k]))

(defn- closed-picture [view] (assoc view :is-closed true))

(defn- patch-node-seqs
  "Node id -> seq, for each node that `patch` writes."
  [patch]
  (into {}
        (keep #(when-let [node-id (or (:node-id %) (get-in % [:node-spec :id]))] [node-id
                                                                                  (:seq patch)]))
        (:ops patch)))

(defn- replay-app
  "The picture of the app that `file` records, with the seq of the last write to
   each node. nil when the record holds no app."
  [file]
  (sink/fold-lines file
                   (fn [acc line]
                     (case (str (:kind line))
                       "open"
                       (let [view (materializer/materialize (live<-wire (:view line)))]
                         (if (app/app? view)
                           ;; A compacted record keeps the node seqs as pairs.
                           {:view view :node-seqs (into {} (:node-seqs line))}
                           (reduced nil)))

                       "patch"
                       (let [patch (live<-wire (:patch line))]
                         (-> acc
                             (update :view materializer/apply-patch patch)
                             (update :node-seqs merge (patch-node-seqs patch))))

                       "close"
                       (update acc :view closed-picture)

                       "archive"
                       (assoc-in acc [:view :is-archived] true)

                       acc))
                   nil))

(defn- app-entry
  "The registry entry of open app `view`, recorded in `file`. `node-seqs` maps
   each node id to the seq of the last patch that wrote it."
  [view file node-seqs]
  {:kind :live
   :id (:id view)
   :view (atom view)
   :events (atom {:cursor 0 :items []})
   :file file
   :promise (promise)
   :session-id (:session-id view)
   :channel-ids (:channel-ids view)
   :created-at (:created-at view)
   :is-app true
   :app-key (app-key view)
   :node-seqs (atom node-seqs)
   ;; The size of the record after its last compaction.
   :compacted-bytes (atom 0)
   ;; The Sends on their way to a listener of this process.
   :delivering (atom #{})})

(defn- owner-hold
  "The callback fields of the current owner: its context, its listener, and the
   agent that runs the listener calls in order."
  []
  {:owner-context (:context *live-owner*)
   :listener (:listener *live-owner*)
   :listener-agent (when (:listener *live-owner*) (agent nil))
   ;; An app that its owner opened or adopted in this process.
   :is-held true})

(defn- app-stale?
  "True for open app `entry` of an extension or an SDK program that no owner of
   this process holds yet: an app that a restart read back from its record."
  [entry]
  (boolean (and (:is-app entry) (not (:is-held entry)) (:extension @(:view entry)))))

(defn- app-picture
  "The picture of live `entry` as the surfaces paint it: an app that waits for its
   owner says `:is-stale` true."
  [entry]
  (some-> (:view entry)
          deref
          (cond->
            (app-stale? entry)
            (assoc :is-stale true))))

(defonce ^:private session-sends
  ;; Offers the Sends of sandbox apps to their sessions in order, away from the app lock.
  (agent nil :error-mode :continue))

(defn- offer-send!
  "Offer Send `receipt` of open app `entry` to the session that owns the app. Only
   an app of the python_execution sandbox needs it: an extension gets its Sends
   through its listener or its events. The gateway queues one notification turn
   and marks the receipt delivered. Without a gateway, the receipt stays accepted."
  [entry receipt]
  (let [view @(:view entry)]
    (when (and (nil? (:extension view)) (nil? (:listener entry)))
      (send-off
        session-sends
        (fn [_]
          (try (publish!
                 (:channel-ids entry)
                 (lifecycle-event :view/send entry {:title (:title view) :submission receipt}))
               (catch Throwable t
                 (tel/log!
                   {:level :warn
                    :id ::send-offer-failed
                    :data {:view-id (:id entry) :submission-id (:id receipt) :error (ex-message t)}
                    :msg "Vis could not offer an app Send to its session"})))
          nil)))))

(defn- offer-sends!
  "Offer each accepted Send of restored app `entry` to its session again: a Send
   that a restart cut off before the session took it."
  [entry]
  (doseq [receipt
          (:submissions @(:view entry))

          :when (= "accepted" (:status receipt))]

    (offer-send! entry receipt)))

(defn- replayed-app
  "[[replay-app]] of `file`, the record of app `view-id` of session `sid`. A record
   that does not replay is logged with its cause and answers nil, so it never hides
   the other apps of the session."
  [sid view-id file]
  (try (replay-app file)
       (catch Exception e
         (tel/log! {:level :warn
                    :id ::app-record-unreadable
                    :data {:session-id (str sid)
                           :view-id (str view-id)
                           :file (str file)
                           :error (ex-message e)
                           :cause (some-> (ex-cause e)
                                          ex-message)}
                    :msg "Skipped an app whose record does not replay"})
         nil)))

(defn- restore-app!
  "Read app `view-id` of session `sid` back from its record. An open app goes back
   to the registry without an owner callback, and a closed app to [[closed-apps]].
   Answers the picture, or nil when the record holds no app of that session. The
   call that restores an open app offers its accepted Sends to the session again."
  [sid view-id]
  (when (and (parse-uuid (str sid)) (parse-uuid (str view-id)))
    (let [file (sink/view-file sid view-id)]
      (when-let [{:keys [view node-seqs]} (replayed-app sid view-id file)]
        (when (= (str sid) (:session-id view))
          (if (:is-closed view)
            (get (swap! closed-apps #(cond-> % (not (contains? % view-id)) (assoc view-id view)))
                 view-id)
            (let [[before after] (swap-vals! pending
                                             #(cond-> % (not (contains? % view-id))
                                                (assoc view-id (app-entry view file node-seqs))))
                  entry (get after view-id)]

              (when (and entry (not (contains? before view-id)))
                (offer-sends! entry)
                ;; The surfaces learn that the app waits for its owner.
                (when (app-stale? entry)
                  (publish! (:channel-ids entry)
                            (lifecycle-event :view/open entry {:view (app-picture entry)}))))
              (some-> entry
                      app-picture))))))))

(defn app-view
  "App `view-id` of session `sid` as the surfaces paint it, open or closed, or
   nil. After a restart, the first read replays its record."
  [sid view-id]
  (let [sid
        (str sid)

        view-id
        (str view-id)

        mine
        (fn [view]
          (when (and view (app/app? view) (= sid (:session-id view))) view))]

    (or (mine (some-> (live-entry view-id)
                      app-picture))
        (mine (get @closed-apps view-id))
        (when-not (or (live-entry view-id) (contains? @closed-apps view-id))
          (restore-app! sid view-id)))))

(defn session-apps
  "Every app of session `sid`, open or closed, in the order they opened."
  [sid]
  (into [] (comp (map :view-id) (distinct) (keep #(app-view sid %))) (sink/app-index (str sid))))

(defn agent-notes
  "What the open apps and the HTML pages of session `sid` tell the model: one map
   of `id`, `kind`, `title` and `agent_note` each. Apps come first, in the order
   they opened, then pages, oldest note first. A closed or archived app tells
   nothing."
  [sid]
  (into (vec
          (for [view
                (session-apps sid)

                :when (and (:agent-note view) (not (:is-closed view)) (not (:is-archived view)))]

            {"id" (:id view) "kind" "app" "title" (:title view) "agent_note" (:agent-note view)}))
        (for [{:keys [id title agent-note]}
              (sink/page-notes sid)

              :when (and (string? id) (string? agent-note))]

          (cond-> (array-map "id" id "kind" "page")
            (string? title)
            (assoc "title" title)

            true
            (assoc "agent_note" agent-note)))))

(defn- adoptable-app
  "The open entry of the app that `record` adopts: the newest app of its session
   with the same extension and key. nil when that app is closed or missing."
  [record]
  (when-let [k (app-key record)]
    (or (some #(when (= k (:app-key %)) %) (vals @pending))
        (when-let [newest (->> (sink/app-index (:session-id record))
                               (filter #(= k [(:session-id record) (:extension %) (:key %)]))
                               last)]
          (app-view (:session-id record) (:view-id newest))
          (live-entry (:view-id newest))))))

(defn- adopt-app!
  "Give the open app that `record` names by key to the current owner. Answers its
   picture, or nil when no open app holds that key. The app keeps its state; the
   owner patches it to change it."
  [record]
  (when-let [entry (adoptable-app record)]
    (let [[before after]
          (swap-vals! pending
                      #(cond-> % (contains? % (:id entry)) (update (:id entry) merge (owner-hold))))
          adopted (get after (:id entry))]

      ;; The surfaces learn that a disconnected app has its owner again.
      (when (and adopted
                 (some-> (get before (:id entry))
                         app-stale?))
        (publish! (:channel-ids adopted)
                  (lifecycle-event :view/open adopted {:view (app-picture adopted)})))
      (some-> adopted
              app-picture))))

(defn- file-app!
  "List new app `view` in its session and file its attachment row in the block
   that opened it. An app opened outside a block has no row; its session still
   lists it."
  [view]
  (sink/index-app! (:session-id view)
                   (cond-> {:view-id (:id view) :created-at (:created-at view)}
                     (:key view)
                     (assoc :key (:key view))

                     (:extension view)
                     (assoc :extension (:extension view))))
  (let [picture (.getBytes ^String (wire/json-str view) java.nio.charset.StandardCharsets/UTF_8)]
    (mpl-capture/record-attachment! {:kind "file"
                                     :media-type app/media-type
                                     :filename (str (or (materializer/slug (:title view)) "app")
                                                    ".app.json")
                                     :base64 (.encodeToString (java.util.Base64/getEncoder) picture)
                                     :size (alength picture)
                                     :view-id (:id view)
                                     :audience "user"})))

(defn- mount-live!
  "Register `record`, the normalized declaration of a new live view, and tell the
   surfaces. Answers the materialized view."
  [record]
  (let [view-id
        (:id record)

        app?
        (app/app? record)

        owned
        (owned-key record)

        _
        (when-not (trimmed (:session-id record))
          (invalid-live-view! (str "view " view-id
                                   " names no session — set :session-id, or open it "
                                   "from an extension environment that carries one")))

        file
        (sink/open! record)

        view
        (materializer/materialize record)

        entry
        {:kind :live
         :id view-id
         :view (atom view)
         ;; What the human DID in this view, in order: changes, submits and
         ;; activations. The owner reads them with `events`, waiting when none is new.
         :events (atom {:cursor 0 :items []})
         :file file
         :promise (promise)
         :session-id (:session-id view)
         :channel-ids (:channel-ids view)
         :created-at (:created-at view)
         ;; The collector of the block that OPENED the view. A human's stop arrives on a
         ;; gateway thread, which holds no artifacts of its own, and the record still
         ;; belongs to the block whose run produced it.
         :attachment-sink (when-not (or owned app?) mpl-capture/*attachment-sink*)
         :block-owner (when-not (or owned app?) *block-owner*)
         ;; The eval wall of the block that opened it, LIFTED for as long as the
         ;; view is open. A run SHOWING its work is never billed a deadline: a CI
         ;; run takes fifteen minutes and the Python watchdog around the block
         ;; billed five, killing the watch mid-picture. Released at the close,
         ;; whoever closes it. An owned view outlives every block, so it holds none.
         :wall-hold (when-not (or owned app?) (rt/hold-blocking-wall!))
         :owned-key owned
         :owner-context (when owned (:context *live-owner*))
         :listener (when owned (:listener *live-owner*))
         ;; Callbacks of one view run in order, one at a time.
         :listener-agent (when (and owned (:listener *live-owner*)) (agent nil))}

        ;; An app also outlives every block, and its owner can hand it on.
        entry
        (cond-> entry
          app?
          (merge (app-entry view file {}) (owner-hold)))]

    (swap! pending assoc view-id entry)
    (when owned (save-owned-soon!))
    (when app? (file-app! view))
    (tel/log! {:level :debug
               :id ::live-opened
               :data {:view-id view-id :title (:title view) :nodes (mapv :id (:nodes view))}
               :msg "Live view opened"})
    (when (zero? (long (publish! (:channel-ids entry)
                                 (lifecycle-event :view/open entry {:view view}))))
      (tel/log! {:level :warn
                 :id ::live-unwatched
                 :data {:view-id view-id :title (:title view) :channel-ids (:channel-ids entry)}
                 :msg (str "Live view reached no channel — nobody is watching it. It still runs, "
                           "and it still ends in the verdict the model reads")}))
    view))

(defn open-live!
  "Mount a live view the human WATCHES and return it materialized, `:id` and all.
   The caller keeps that id: every patch, and the close, name it.

   Nothing blocks. A view nobody has mounted a surface for still runs and still
   ends in the verdict the model reads — the engine says so ONCE, in the log,
   rather than refusing work whose whole product is the picture at the end. That
   is the one place a view differs from a request, which answers `undeliverable`
   at once because a form nobody can see is a thread parked forever."
  [view]
  ;; A person's stop ends the watching of the whole block. Without this, a model loop
  ;; around a watch tool opened the same view again at once, and the note never
  ;; reached the model.
  (when-let [stop (some->> *block-owner*
                           (get @human-stops)
                           peek)]
    (throw (ex-info (str "Vis refused a new live view. " (human-stop-text stop))
                    {:type :vis/live-stopped-by-human :title (:title stop) :note (:note stop)})))
  (let [record (-> view
                   normalize-live-view
                   materializer/redact-presentation
                   checked-live-view
                   materializer/record-view)]
    (or (when (app/app? record) (adopt-app! record)) (mount-live! record))))

(defn- app-node
  "Node `node-id` anywhere in `view`, or nil."
  [view node-id]
  (letfn [(walk [nodes] (some #(if (= node-id (:id %)) % (walk (:children %))) nodes))]
    (walk (:nodes view))))

(defn- owner-op
  "One op of an owner's patch as it lands on `view`. Posted messages and the saved
   draft of the human stay as the host stored them. Only the host writes a Send
   receipt."
  [view op]
  (case (:op op)
    :submission
    (invalid-live-patch! "a Send receipt changes only through the live `submission` op")

    :thread
    (let [before
          (some-> (app-node view (:node-id op))
                  (app/find-thread (:id (:thread op))))

          posted
          (into {} (map (juxt :id identity)) (:messages before))]

      (update op
              :thread
              (fn [thread]
                (cond-> (-> thread
                            (update :messages
                                    (fn [messages]
                                      (mapv #(get posted (:id %) %) messages)))
                            (dissoc :draft))
                  (:draft before)
                  (assoc :draft (:draft before))))))

    op))

(defn- compact-app-record!
  "Compact the record of app `entry` when it grew past twice its last compacted
   size. The doubling keeps the cost of a rewrite in step with the lines that it
   drops. A failed rewrite keeps the whole record and waits for the next doubling."
  [entry]
  (when-let [floor (:compacted-bytes entry)]
    (let [^java.io.File file (:file entry)
          size (.length file)]

      (when (> size (max (long sink/compact-floor-bytes) (* 2 (long @floor))))
        (try (reset! floor (sink/compact! file @(:view entry) @(:node-seqs entry)))
             (catch Exception e
               (reset! floor size)
               (tel/log! {:level :warn
                          :id ::app-record-compaction-failed
                          :data {:view-id (:id entry) :file (str file) :error (ex-message e)}
                          :msg
                          "Vis kept the whole record of an app that it could not compact"})))))))

(defn patch-live!
  "Apply `patch` to live view `view-id` and return the view it made.

   One patch at a time per view: the record on disk is the order the engine
   ACCEPTED patches in, and two threads racing would leave a file no replay can
   trust. The line is written BEFORE any surface is told, so a crash keeps what
   the engine accepted rather than what a screen managed to paint."
  [view-id patch]
  (let [entry
        (live-entry! view-id)

        cell
        (:view entry)]

    (locking cell
      ;; A close may win while this patch waits for the view lock.
      (live-entry! view-id)
      (let [applied
            (-> (normalize-patch @cell patch)
                (cond->
                  (not *host-patch*)
                  (update :ops
                          (fn [ops]
                            (mapv #(owner-op @cell %) ops))))
                materializer/redact-presentation
                checked-live-patch)

            patched
            (materializer/apply-patch @cell applied)]

        (reset! cell patched)
        (sink/append! (:file entry) applied)
        (some-> (:node-seqs entry)
                (swap! merge (patch-node-seqs applied)))
        (compact-app-record! entry)
        (when (:owned-key entry) (save-owned-soon!))
        ;; An app patch is marked so the gateway keeps it out of the session journal.
        (publish! (:channel-ids entry)
                  (lifecycle-event :view/patch
                                   entry
                                   (cond-> {:patch applied}
                                     (:is-app entry)
                                     (assoc :is-app true))))
        (.notifyAll ^Object cell)
        patched))))

(defn select-live!
  "Select `item-ids` in selectable table `node-id` of open view `view-id`.

   This is an ordinary durable `set` patch, not app-local selection: the writer
   sees the change through `state`, and every TUI or Companion painting the view
   receives the same new selection. Stale row ids and non-selectable tables are
   refused by the materializer without changing the view."
  [view-id node-id item-ids]
  (patch-live! view-id [{:op :set :node-id node-id :selected-ids item-ids}]))

(defn- receipt-move
  "Receipt `receipt` moved forward to `status`, with an optional `note`, or nil
   when it does not move: a receipt never moves back, and a settled one stays."
  [receipt status note]
  (let [rank (zipmap app/submission-statuses (range))]
    (when-not (contains? rank status)
      (invalid-live-patch! (str "unknown submission status " (pr-str status)
                                " — use " (str/join ", " app/submission-statuses))))
    (when-not (or (nil? note) (and (string? note) (<= 1 (count note) (long (:note app/bounds)))))
      (invalid-live-patch!
        (str "a submission note is text of 1 to " (:note app/bounds) " characters")))
    (when (and (not (app/settled-statuses (:status receipt)))
               (> (long (rank status)) (long (get rank (:status receipt) 0))))
      (cond-> (assoc receipt
                :status status
                :updated-at (max (util/now-ms) (long (:updated-at receipt))))
        note
        (assoc :note note)))))

(defn- advance-receipt!
  "Move Send receipt `submission-id` of open app `view-id` forward to `status`.
   Answers `{:submission … :seq …}`; a receipt that does not move answers as it is."
  [view-id submission-id status note]
  (let [cell (:view (live-entry! view-id))]
    (locking cell
      (let [view @cell
            receipt (or (app/find-submission view submission-id)
                        (invalid-live-patch! (str "app " view-id
                                                  " has no submission " submission-id)))]

        (if-let [moved (receipt-move receipt status note)]
          {:submission moved
           :seq (:seq (binding [*host-patch* true]
                        (patch-live! view-id [{:op :submission :submission moved}])))}
          {:submission receipt :seq (:seq view)})))))

(defn mark-send-delivered!
  "Mark Send `submission-id` of open app `view-id` delivered: the session that owns
   the app has it now. A receipt that has moved on stays as it is. Answers it."
  [view-id submission-id]
  (:submission (advance-receipt! view-id submission-id "delivered" nil)))

(defn- human-note
  "The comment a human left with their stop: trimmed, and cut to
   `view-spec/note-chars`. A stop is never refused for the length of its note —
   what the person managed to type before pressing stop always reaches the model."
  [note]
  (when-let [text (some-> (trimmed note)
                          util/redact-secret-text)]
    (subs text 0 (min (count text) (long view-spec/note-chars)))))

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
  (let [reason
        (live-term fail! ":reason" view-spec/live-reasons (or (:reason ending) :completed))

        verdict
        (cond-> {:view-id (:id view)
                 :is-completed (= :completed reason)
                 :reason reason
                 :is-from-human (some? human)}
          (human-note (:note human))
          (assoc :note (human-note (:note human)))

          (trimmed (:summary ending))
          (assoc :summary (trimmed (:summary ending)))

          (trimmed (:error ending))
          (assoc :error (trimmed (:error ending)))

          (trimmed (:artifact-id ending))
          (assoc :artifact-id (trimmed (:artifact-id ending))))

        picture
        (materializer/picture view)

        result
        (cond-> (assoc verdict :view (:view picture))
          (seq (:elided picture))
          (assoc :elided (:elided picture)))]

    (when-let [why (view-spec/live-result-error result)]
      (fail! why))
    (let [public (materializer/redact-presentation result)]
      (if-let [why (view-spec/live-result-error public)]
        (fail! why)
        public))))

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
             :filename (str (or (materializer/slug (:title artifact)) "live-view")
                            ".live.ndjson"))))

(defn- live-artifact
  "The closed view as an ARTIFACT the human can reopen: what they watched,
   ADDRESSED rather than copied.

   Nothing here reads the log. The record has been the store of truth since
   `open`, so the artifact points at that file (`:storage-uri`) and states how much
   of it there is; only a view small enough to survive a session sync
   (`view-spec/live-artifact-inline-bytes`) also travels as bytes, because holding a
   build log in memory as base64 is the cost this whole design removes. `:view` is
   the final materialized state — the summary a surface opens instantly, and the
   very state `materializer/->markdown` re-renders the model's document from.

   `:size` and `:line-count` count the RUN — the declared view and every accepted
   patch — because the trailer that seals the record states the verdict, and the
   verdict is already `:reason` and `:view` here."
  [view result ^java.io.File file]
  (let [{:keys [size line-count]}
        (sink/stats file)

        artifact
        (cond-> {:id (str (java.util.UUID/randomUUID))
                 :view-id (:view-id result)
                 :session-id (:session-id view)
                 :title (:title view)
                 :media-type view-spec/live-artifact-media-type
                 :audience "user"
                 :ended-at (util/now-ms)
                 :reason (:reason result)
                 :view (:view result)
                 :storage-uri (sink/record-uri (:session-id view) (:view-id result))
                 :size size
                 :line-count line-count}
          (:owner view)
          (assoc :owner (:owner view))

          (<= (long size) (long view-spec/live-artifact-inline-bytes))
          (assoc :base64
            (.encodeToString (java.util.Base64/getEncoder)
                             (java.nio.file.Files/readAllBytes (.toPath file)))))]

    (if-let [why (view-spec/live-artifact-error artifact)]
      (invalid-live-view! why)
      artifact)))

(defonce ^:private late-artifact-filer
  ;; The ONE way an artifact reaches durable storage from OUTSIDE a running block.
  ;; The layer that owns the database installs it; this namespace cannot reach that
  ;; layer (the turn loop sits between the two), and there is deliberately no
  ;; second door.
  (atom nil))

(defn set-late-artifact-filer!
  "Install what a LATE close files its artifact through: called
   `[iteration-id attachment]`, answering the stored row or nil.

   Until one is installed a late close still seals its record and still answers
   its verdict — only the row that LISTS the artifact is missing."
  [f]
  (reset! late-artifact-filer f))

(defn- file-late-artifact!
  "File `attachment` on iteration `home`, for a view whose block is already over.
   True when a row was stored. Never throws: a close owes a verdict first."
  [home attachment]
  (boolean (when-let [f @late-artifact-filer]
             (try (some? (f home attachment))
                  (catch Throwable t
                    (tel/log! {:level :warn
                               :id ::live-artifact-not-filed
                               :data {:iteration-id (str home) :error (ex-message t)}
                               :msg "Late live-view artifact could not be filed"})
                    false)))))

(defn close-live!
  "End live view `view-id` and return what the model should read. nil when the
   view was already closed, so a `finally` closing what an interrupt already
   closed is a no-op rather than a second result.

   `ending` says how it ended: `:reason` (`completed` by default), `:summary`,
   `:error`, `:artifact-id`, optional archive-only `:selection-snapshots`, and optional
   compact string `:model-result`. A snapshot's PICTURE goes back through the wire
   inverse first — the only view an extension holds is the one `state` answered it,
   in the JSON it crossed with. The full verdict always remains in the artifact
   and human-facing close event. `human` is the person who stopped it — `{:note …}`,
   which only [[interrupt-live!]] passes, because a run does not get to claim a
   human ended it.

   The close SETTLES the view: the record it has been writing since `open` becomes
   an artifact this session owns, and its id rides back in the verdict — so the
   human can reopen the log after the pane is gone instead of it being dumped into
   the transcript. A close reached from somewhere that holds no artifacts (a human's
   stop arriving on a gateway thread) still files one, into the collector the OPENING
   block captured. Only a view opened outside any block names no artifact; the record
   is sealed either way, and every surface still reaches the log by view id."
  ([view-id] (close-live! view-id {} nil))
  ([view-id ending] (close-live! view-id ending nil))
  ([view-id ending human]
   (when-let [entry (live-entry view-id)]
     ;; The SAME cell `patch-live!` holds: a close racing the last patch waits for
     ;; it, so the verdict renders the picture the record already ends with.
     (let [cell (:view entry)]
       (locking cell
         (let [snapshots (mapv
                           (fn [snapshot]
                             (let [node-id (trimmed (:node-id snapshot))
                                   selected-ids (mapv str (or (:selected-ids snapshot) []))
                                   ;; The picture an extension archives is the one
                                   ;; `state` ANSWERED it: snake_case keys, wire terms,
                                   ;; JSON both ways. Read it back the same mechanical
                                   ;; way every other frame crossing a process boundary
                                   ;; is read — a no-op for an engine-shaped view — then
                                   ;; hold it to the spec the materializer answers to.
                                   snapshot-view (live<-wire (:view snapshot))]

                               (when-not (and node-id (seq selected-ids) (map? snapshot-view))
                                 (invalid-live-view!
                                   "each selection snapshot needs node_id, selected_ids and view"))
                               (when-let [why (view-spec/live-view-error snapshot-view)]
                                 (invalid-live-view! (str "invalid selection snapshot: " why)))
                               {:node-id node-id
                                :selected-ids selected-ids
                                :view (checked-live-view (materializer/redact-presentation
                                                           snapshot-view))}))
                           (or (:selection-snapshots ending) []))
               _ (when (> (count snapshots) 500)
                   (invalid-live-view! "an artifact holds at most 500 selection snapshots"))
               _ (when (> (count (.getBytes (wire/json-str snapshots)
                                            java.nio.charset.StandardCharsets/UTF_8))
                          (long view-spec/live-selection-snapshot-bytes))
                   (invalid-live-view!
                     "selection snapshots exceed the 1000000-byte artifact limit"))
               verdict (live-result @cell
                                    (dissoc ending :selection-snapshots :model-result)
                                    human
                                    invalid-live-view!)
               compact-result (when (contains? ending :model-result)
                                (or (some-> (trimmed (:model-result ending))
                                            util/redact-secret-text)
                                    (invalid-live-view! "model_result must be a non-blank string")))
               ;; Built BEFORE the registry drops the view, so a refusal leaves the
               ;; view open and nameable rather than stranding whoever is holding it;
               ;; REGISTERED after, inside the branch that won the close, so a second
               ;; close files no second artifact.
               surface-verdict (assoc (dissoc verdict :elided)
                                 :view (select-keys @cell view-spec/live-picture-keys))
               ;; No block owns an extension's view, so it files no session artifact.
               artifact (when-not (or (:owned-key entry) (:is-app entry))
                          (live-artifact @cell surface-verdict (:file entry)))
               ;; A closed app stays readable. It is kept BEFORE the registry drops
               ;; it, so a reader never falls back to a record without its close.
               _ (when (:is-app entry) (swap! closed-apps assoc view-id (closed-picture @cell)))
               [old _] (swap-vals! pending dissoc view-id)]

           (when (contains? old view-id)
             (let [;; WHERE the row goes. A block collecting HERE — the view opened and
                   ;; closed inside one run, or carried into a later block — takes it out
                   ;; with its own artifacts. A human's stop arriving after that block
                   ;; ENDED reaches a collector nobody will ever read again, so the row
                   ;; goes to the iteration that block became: the turn the human is
                   ;; already reading, where every other artifact of that run is listed.
                   attachment (some-> artifact
                                      live-attachment)
                   filed? (and attachment
                               (if-let [home (and (nil? mpl-capture/*attachment-sink*)
                                                  (:late-home entry))]
                                 (file-late-artifact! home attachment)
                                 (some? (binding [mpl-capture/*attachment-sink*
                                                  (or mpl-capture/*attachment-sink*
                                                      (:attachment-sink entry))]
                                          (mpl-capture/record-attachment! attachment)))))
                   artifact-id (when filed? (:id artifact))
                   artifact-result (cond-> surface-verdict
                                     artifact-id
                                     (assoc :artifact-id artifact-id))
                   model-result (or compact-result
                                    (cond-> verdict
                                      artifact-id
                                      (assoc :artifact-id artifact-id)))]

               ;; Human readers retain the layout and full hot windows. The producer
               ;; receives the budgeted model picture, or its explicit compact result.
               (when-let [release (:wall-hold entry)]
                 (release))
               (when-let [owner (and human (:block-owner entry))]
                 (when-not @owner
                   (swap! human-stops update
                     owner
                     (fnil conj [])
                     (cond-> {:title (:title @cell)}
                       (:note verdict)
                       (assoc :note (:note verdict))))))
               (sink/close! (:file entry)
                            (cond-> artifact-result
                              (seq snapshots)
                              (assoc :selection-snapshots snapshots)))
               (deliver (:promise entry) model-result)
               (.notifyAll ^Object cell)
               (when (or (:owned-key entry) (:is-app entry))
                 (swap! ended-owned assoc view-id (:session-id entry)))
               (when (:owned-key entry) (save-owned-soon!))
               (publish! (:channel-ids entry)
                         (lifecycle-event :view/close entry {:result artifact-result}))
               (tel/log! {:level :debug
                          :id ::live-closed
                          :data {:view-id view-id
                                 :reason (:reason artifact-result)
                                 :is-from-human (:is-from-human artifact-result)
                                 :artifact-id artifact-id}
                          :msg "Live view closed"})
               model-result))))))))

(defn interrupt-live!
  "End an extension-owned live view because a human stopped watching."
  ([view-id] (interrupt-live! view-id nil))
  ([view-id note]
   (when (live-entry view-id) (close-live! view-id {:reason :interrupted} {:note note}))))

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
  (if (= :live (:kind entry)) (app-picture entry) (request->view entry)))

(defn open-view
  "The open View `view-id` as `{:kind … :view …}`, or nil. The descriptor is for
   capability policy and routing; renderers still receive the document itself."
  [view-id]
  (if-let [entry (get @pending (str view-id))]
    {:kind (:kind entry) :view (entry->view entry)}
    (when-let [stale (get @stale-views (str view-id))]
      {:kind :live :view stale})))

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
   its stream reads the picture back instead of replaying every patch it missed.
   The stale pictures of owned views come with them, each with `:is-stale` true."
  []
  (->> (vals @pending)
       (filter #(and (= :live (:kind %)) (not (:is-app %))))
       (map entry->view)
       (concat (vals @stale-views))
       (sort-by (juxt :created-at :id))
       vec))

(defn open-live-ids
  "The id of every live view open right now.

   The cheap half of [[live-views]]: a caller that only has to notice a view
   APPEARING — the run that opened it, on its way out — never materializes one."
  []
  (into #{}
        (keep (fn [[view-id entry]]
                (when (= :live (:kind entry)) view-id)))
        @pending))

(defn dismiss-stale!
  "Remove stale view `view-id` for good. Answers true when it was stale."
  [view-id]
  (when-let [view (get @stale-views (str view-id))]
    (when (stale-close! view :interrupted) (save-owned-soon!) true)))

(defn close-unclaimed-stale!
  "Close each stale view of `extension` after the extension loaded again. The load
   opens every key that the extension still uses, so a stale view that is left has
   no owner. Answers the closed view ids."
  [extension]
  (let [closed (->> (vals @stale-views)
                    (filter #(= (str extension) (:extension %)))
                    (sort-by :created-at)
                    (keep #(when (stale-close! % :completed) (:id %)))
                    vec)]
    (when (seq closed) (save-owned-soon!))
    closed))

(defn close-owned-by!
  "Close each owned view that extension context `ctx` opened, because the
   extension unloads. Answers the closed view ids."
  [ctx]
  ;; An app outlives its extension. It only loses the callback, and its Sends wait
  ;; as accepted until the extension opens the same key again.
  (let [released?
        #(and (:is-app %) (identical? ctx (:owner-context %)))

        [before after]
        (swap-vals! pending
                    (fn [entries]
                      (into {}
                            (map
                              (fn [[id entry]]
                                [id
                                 (if (released? entry)
                                   (dissoc entry :owner-context :listener :listener-agent :is-held)
                                   entry)]))
                            entries)))]

    ;; The surfaces show each released app as disconnected.
    (doseq [[id entry]
            before

            :let [now
                  (get after id)]
            :when (and (released? entry) now (app-stale? now))]

      (publish! (:channel-ids now) (lifecycle-event :view/open now {:view (app-picture now)}))))
  (->> (vals @pending)
       (filter #(and (:owned-key %) (identical? ctx (:owner-context %))))
       (sort-by :created-at)
       (keep #(when (close-live! (:id %) {:reason :completed :summary "The extension unloaded."})
                (:id %)))
       vec))

(defn owner-context-open?
  "True when extension context `ctx` drives an open owned view or holds an open
   app. Disposal of that context closes the view or disconnects the app, so the
   idle sandbox reaper keeps its session."
  [ctx]
  (boolean (and ctx
                (some #(and (or (:owned-key %) (:is-app %)) (identical? ctx (:owner-context %)))
                      (vals @pending)))))

(defn- supersede-owned!
  "Close every other view of the key that the new owned `view` holds, open or
   stale: an extension and a key own one view."
  [view]
  (when-let [k (owned-key view)]
    (doseq [entry (vals @pending)
            :when (and (= k (:owned-key entry)) (not= (:id view) (:id entry)))]

      (close-live! (:id entry) {:reason :superseded}))
    (doseq [stale (vals @stale-views)
            :when (= k (owned-key stale))]

      (stale-close! stale :superseded))
    (save-owned-soon!))
  view)

(defn adopt-open-views!
  "Give ended blocks' unhomed live views in `session-id` the iteration `home`.
   Running blocks and other sessions keep their own views."
  [session-id home]
  (if-not (and (trimmed session-id) home)
    []
    (let [eligible?
          (fn [entry]
            (and (= :live (:kind entry))
                 (not (:is-app entry))
                 (= session-id (:session-id entry))
                 (nil? (:late-home entry))
                 (or (nil? (:block-owner entry)) @(:block-owner entry))))

          [before after]
          (swap-vals! pending
                      (fn [entries]
                        (into {}
                              (map (fn [[id entry]]
                                     [id
                                      (if (eligible? entry) (assoc entry :late-home home) entry)]))
                              entries)))]

      (->> before
           (keep (fn [[id entry]]
                   (when (and (eligible? entry) (= home (:late-home (get after id)))) id)))
           sort
           vec))))

(defn close-abandoned!
  "Close live views opened by `block-owner` with `ending`.
   A block's failure never closes another block's view, including carried views."
  [block-owner ending]
  (if-not block-owner
    []
    (->> @pending
         vals
         (filter #(and (= :live (:kind %)) (identical? block-owner (:block-owner %))))
         (sort-by (juxt :created-at :id))
         (keep #(close-live! (:id %) ending))
         vec)))

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
  (checked-answer request-id (:nodes (get @pending request-id)) result)
  (let [[old _] (swap-vals! pending dissoc request-id)]
    (when-let [entry (get old request-id)]
      ;; Close reaches every channel before the resumed caller can finish its turn.
      ;; Routing comes from the removed entry, not the pending registry.
      (try (publish! (:channel-ids entry)
                     (lifecycle-event :view/close entry {:result {:reason (:reason result)}}))
           (finally (deliver (:promise entry) result)))
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
      (let [outcome (coerce-values (:nodes entry) values)]
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

(def ^:private max-live-events
  "How many events one live view keeps for its owner. The oldest event drops first."
  256)

(defn- push-live-event!
  "Append `event` to the event queue of live `entry` and wake its readers."
  [entry event]
  (let [cell (:view entry)]
    (locking cell
      (let [{:keys [items]}
            (swap! (:events entry) (fn [{:keys [cursor items]}]
                                     (let [next-cursor (inc (long cursor))
                                           items (conj items
                                                       (assoc event
                                                         :seq next-cursor
                                                         :at (util/now-ms)))
                                           extra (- (count items) max-live-events)]

                                       {:cursor next-cursor
                                        :items (if (pos? extra) (subvec items extra) items)})))]
        (when-let [listener (:listener entry)]
          (let [recorded (peek items)
                submission-id (:submission-id recorded)]

            (when submission-id
              (some-> (:delivering entry)
                      (swap! conj submission-id)))
            (send-off (:listener-agent entry)
                      (fn [_]
                        (when (try (listener recorded (:session-id entry))
                                   true
                                   (catch Throwable t
                                     (tel/log! {:level :warn
                                                :id ::live-callback-failed
                                                :data {:view-id (:id entry) :error (ex-message t)}
                                                :msg "A live view callback failed"})
                                     false))
                          ;; The owner has the Send now.
                          (when submission-id
                            (try (advance-receipt! (:id entry) submission-id "delivered" nil)
                                 (catch Throwable t
                                   (tel/log! {:level :warn
                                              :id ::send-delivery-not-recorded
                                              :data {:view-id (:id entry)
                                                     :submission-id submission-id
                                                     :error (ex-message t)}
                                              :msg "Vis could not mark a delivered Send"})))))
                        ;; A failed call leaves the Send accepted for the next owner.
                        (when submission-id
                          (some-> (:delivering entry)
                                  (swap! disj submission-id)))
                        nil)))))
      (.notifyAll ^Object cell))))

(defn live-events
  "The events of live view `view-id` after cursor `after`. Waits up to
   `timeout-ms` for a new one. Returns `{:events [...] :cursor n :is-open b}`;
   `:is-open` is false once the view is closed, so a reader can stop."
  [view-id after timeout-ms]
  (let [deadline (+ (util/now-ms) (long timeout-ms))]
    (loop []

      (if-let [entry (live-entry (str view-id))]
        (let [cell (:view entry)
              {:keys [cursor items]} @(:events entry)
              fresh (filterv #(> (long (:seq %)) (long after)) items)
              left (- deadline (util/now-ms))]

          (if (or (seq fresh) (<= left 0))
            {:events fresh :cursor cursor :is-open true}
            (do (locking cell (.wait ^Object cell (long (min left 1000)))) (recur))))
        {:events [] :cursor (long after) :is-open false}))))

(defn- view-node
  "The node `node-id` anywhere in the component tree `nodes`, or nil."
  [nodes node-id]
  (first (filter #(= node-id (:id %)) (tree-seq #(seq (:children %)) :children {:children nodes}))))

(defn- submit-scope
  "The components a submit button `node-id` collects: the children of its
   nearest group, or the whole view when no group holds it."
  [nodes node-id]
  (letfn [(walk [scope nodes]
            (some (fn [node]
                    (cond (= node-id (:id node)) scope
                          (seq (:children node)) (walk (:children node) (:children node))))
                  nodes))]
    (walk nodes nodes)))

(defn- live-change!
  "Set the value of input `node-id` in live `entry` and queue one change event.
   A secret input keeps no value: its event says only whether it is set."
  [entry view-id node-id value]
  (let [node (view-node (:nodes @(:view entry)) node-id)]
    (when-not (contains? (set (vals view-spec/field-types)) (:type node))
      (invalid-view-action! view-id "change requires an input node"))
    (if (:is-secret node)
      (do (push-live-event! entry {:type :change :node-id node-id :is-set (some? value)})
          {:is-accepted true :node-id node-id})
      (let [[status coerced] (coerce-value (assoc node :is-required false) value)]
        (if (= :error status)
          {:is-accepted false :reason "invalid" :errors {node-id coerced}}
          (do (patch-live! view-id [{:op :set-value :node-id node-id :value coerced}])
              (push-live-event! entry {:type :change :node-id node-id :value coerced})
              {:is-accepted true :node-id node-id}))))))

(defn- live-submit!
  "Submit the inputs that submit button `node-id` collects in live `entry`.
   Valid values update the view and queue one submit event; an invalid value
   gives field errors and changes nothing."
  [entry view-id node-id values]
  (let [view
        @(:view entry)

        button
        (view-node (:nodes view) node-id)]

    (when-not (and (= :button (:type button)) (= :submit (:action button)))
      (invalid-view-action! view-id "submit requires the node_id of a submit button"))
    (if (:is-disabled button)
      {:is-accepted false :reason "disabled"}
      (let [scope
            (input-fields (submit-scope (:nodes view) node-id))

            stored
            (into {}
                  (keep (fn [field]
                          (when (contains? field :value) [(:id field) (:value field)])))
                  scope)

            outcome
            (coerce-values scope (merge stored (select-keys values (map :id scope))))]

        (if-not (:is-accepted outcome)
          {:is-accepted false :reason "invalid" :errors (:errors outcome)}
          (let [shown (remove :is-secret scope)]
            (patch-live!
              view-id
              (into [{:op :set :node-id node-id :clicks (inc (long (or (:clicks button) 0)))}]
                    (map (fn [{:keys [id]}]
                           {:op :set-value :node-id id :value (get (:values outcome) id)}))
                    shown))
            (push-live-event! entry {:type :submit :node-id node-id :values (:values outcome)})
            {:is-accepted true :node-id node-id}))))))

(defn- app-locked
  "Call `f` with the current entry of open app `entry`, under its view lock. An
   action that loses the race to a close is refused."
  [entry view-id f]
  (let [cell (:view entry)]
    (locking cell
      (let [current (get @pending view-id)]
        (if (identical? cell (:view current))
          (f current)
          {:is-accepted false :reason (if (contains? @closed-apps view-id) "closed" "unknown")})))))

(defn- app-checked
  "Call `f`. An app rule that it breaks throws again with `prefix` before its
   message and `data` with its reason, and the broken rule as the cause."
  [prefix data f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (if (= :app/invalid (:type (ex-data e)))
           (throw (ex-info (str prefix (ex-message e)) (assoc data :reason (ex-message e)) e))
           (throw e)))))

(defn- app-nodes
  "Every node of the component tree `nodes`, children included."
  [nodes]
  (rest (tree-seq #(seq (:children %)) :children {:children nodes})))

(defn- app-refusal
  "Why an action on the nodes `node-ids` of open app `entry` is too late, or nil.
   A `:revision` that no diff of `diffs` shows, or else no diff of the app, is a
   stale revision. A `:base-seq` older than a write to one of the nodes is a
   conflict."
  [entry {:keys [base-seq revision]} node-ids diffs]
  (let [view
        @(:view entry)

        revised
        (filter app/revision
                (or (seq diffs) (filter #(= :diff (:type %)) (app-nodes (:nodes view)))))

        seqs
        @(:node-seqs entry)]

    (cond (and revision (seq revised) (not-any? #(= revision (app/revision %)) revised))
          {:is-accepted false
           :reason "stale_revision"
           :revision (app/revision (first revised))
           :seq (:seq view)}
          base-seq (when-let [node-id (some #(when (> (long (get seqs % 0)) (long base-seq)) %)
                                            node-ids)]
                     {:is-accepted false :reason "conflict" :node-id node-id :seq (:seq view)}))))

(defn- send-event
  "The submit event that gives Send `receipt` to the owner of its app, with the
   values by node id and the comments that the Send posted."
  [receipt values comments]
  (cond-> {:type :submit :submission-id (:id receipt) :values values :comments (vec comments)}
    (:node-id receipt)
    (assoc :node-id (:node-id receipt))))

(defn- app-draft!
  "Save the draft of the human in a thread of node `node-id` of open app `entry`.
   Answers the seq of the app after the write, or a refusal when the client
   edited an older state."
  [entry view-id {:keys [node-id thread-id] :as action}]
  (app-locked entry
              view-id
              (fn [current]
                (let [view
                      @(:view current)

                      node
                      (or (app-node view node-id)
                          (invalid-view-action! view-id (str "the app has no node " node-id)))]

                  (or (app-refusal current action [node-id] (when (= :diff (:type node)) [node]))
                      (let [op
                            (app-checked (str "Invalid View action for " view-id ": ")
                                         {:type :vis/view-invalid-action :view-id view-id}
                                         #(app/draft-op node action (str (random-uuid))))

                            thread-id
                            (or (get-in op [:thread :id]) thread-id)]

                        (cond-> {:is-accepted true
                                 :node-id node-id
                                 :seq (if op
                                        (:seq (binding [*host-patch* true]
                                                (patch-live! view-id [op])))
                                        (:seq view))}
                          thread-id
                          (assoc :thread-id thread-id))))))))

(defn- app-send
  "The receipt, the comments and the patch of one new Send. `collected` holds its
   submit button, the inputs and thread nodes that it collects, and the checked
   values."
  [{:keys [node-id idempotency-key revision]} {:keys [button inputs threaded outcome]}]
  (let [now
        (util/now-ms)

        submission-id
        (str (random-uuid))

        posted
        (app/post-drafts threaded
                         {:author {:kind "human"}
                          :at now
                          :submission-id submission-id
                          :new-id #(str (random-uuid))})

        shown
        (remove :is-secret inputs)

        receipt
        (cond-> {:id submission-id
                 :idempotency-key (or idempotency-key (str (random-uuid)))
                 :status "accepted"
                 :created-at now
                 :updated-at now
                 :values (into []
                               (comp (keep (fn [{:keys [id]}]
                                             (when-some [value (get (:values outcome) id)]
                                               {:node-id id :value value})))
                                     (take (:values app/bounds)))
                               shown)
                 :comments (vec (take (:comments app/bounds) (:comments posted)))}
          node-id
          (assoc :node-id node-id)

          revision
          (assoc :revision revision))]

    {:receipt receipt
     :comments (:comments posted)
     :patch
     (-> (if button [{:op :set :node-id node-id :clicks (inc (long (or (:clicks button) 0)))}] [])
         (into (map (fn [{:keys [id]}]
                      {:op :set-value :node-id id :value (get (:values outcome) id)}))
               shown)
         (into (:ops posted))
         (conj {:op :submission :submission receipt}))}))

(defn- app-submit!
  "Send what the human wrote in open app `entry`: the inputs and the thread drafts
   in the scope of `node-id`, or in the whole app without one. A submit button
   collects its group, and a diff or a thread node sends only its own drafts. The
   host writes one receipt and gives the owner one submit event. A retry with the
   same idempotency key answers the first receipt."
  [entry view-id {:keys [node-id values idempotency-key] :as action}]
  (app-locked
    entry
    view-id
    (fn [current]
      (let [view
            @(:view current)

            earlier
            (some->> idempotency-key
                     (app/submission-for-key view))

            node
            (when (and node-id (nil? earlier))
              (or (app-node view node-id)
                  (invalid-view-action! view-id (str "the app has no node " node-id))))

            button
            (when (and (= :button (:type node)) (= :submit (:action node))) node)]

        (when (and node (not button) (not (app/thread-types (:type node))))
          (invalid-view-action! view-id "submit names a submit button, a diff or a thread node"))
        (cond earlier (cond-> {:is-accepted true :submission earlier :seq (:seq view)}
                        (:node-id earlier)
                        (assoc :node-id (:node-id earlier)))
              (:is-disabled button) {:is-accepted false :reason "disabled"}
              :else
              (let [scope
                    (cond button (submit-scope (:nodes view) node-id)
                          node [node]
                          :else (:nodes view))

                    inputs
                    (if (and node (not button)) [] (input-fields scope))

                    threaded
                    (filterv (comp app/thread-types :type) (app-nodes scope))

                    stored
                    (into {}
                          (keep (fn [field]
                                  (when (contains? field :value) [(:id field) (:value field)])))
                          inputs)

                    outcome
                    (coerce-values inputs (merge stored (select-keys values (map :id inputs))))]

                (or (app-refusal current
                                 action
                                 (map :id (concat inputs threaded))
                                 (filter #(= :diff (:type %)) threaded))
                    (if-not (:is-accepted outcome)
                      {:is-accepted false :reason "invalid" :errors (:errors outcome)}
                      (let [{:keys [receipt comments patch]}
                            (app-send
                              action
                              {:button button :inputs inputs :threaded threaded :outcome outcome})

                            patched
                            (binding [*host-patch* true]
                              (patch-live! view-id patch))]

                        (push-live-event! current (send-event receipt (:values outcome) comments))
                        (offer-send! current receipt)
                        (cond-> {:is-accepted true :submission receipt :seq (:seq patched)}
                          node-id
                          (assoc :node-id node-id)))))))))))

(defn- base-seq
  [fail! value]
  (if (and (integer? value) (not (neg? (long value))))
    (long value)
    (fail! "base_seq must be a whole number of 0 or more")))

(defn- normalize-view-action
  "Normalize one closed operator action. Its keys are engine keywords: the gateway
   converts a decoded action's keys once, and the `:values` it carries stay keyed
   by field name."
  [view-id raw]
  (when-not (map? raw) (invalid-view-action! view-id "an action must be an object"))
  (let [action
        (live-term #(invalid-view-action! view-id %) ":action" view-spec/view-actions (:action raw))

        fail!
        #(invalid-view-action! view-id %)]

    (check-keys! (str (name action) " action") (get view-action-decl-keys action) raw fail!)
    (case action
      :submit
      (let [values
            (:values raw)

            k
            (:idempotency-key raw)]

        (when-not (map? values) (fail! "submit values must be an object"))
        (when (and (some? k) (not (app/idempotency-key? k)))
          (fail! "submit idempotency_key is not a valid key"))
        (cond-> {:action action :values values}
          (some? (:node-id raw))
          (assoc :node-id (live-text fail! "submit node_id" (:node-id raw)))

          (some? k)
          (assoc :idempotency-key k)

          (some? (:base-seq raw))
          (assoc :base-seq (base-seq fail! (:base-seq raw)))

          (some? (:revision raw))
          (assoc :revision (live-text fail! "submit revision" (:revision raw)))))

      :change
      (do (when-not (contains? raw :value) (fail! "change needs a value — null clears the input"))
          (cond-> {:action action
                   :node-id (live-text fail! "change node_id" (:node-id raw))
                   :value (:value raw)}
            (some? (:base-seq raw))
            (assoc :base-seq (base-seq fail! (:base-seq raw)))))

      :draft
      (let [body
            (:body raw)

            anchor
            (:anchor raw)]

        (when-not (string? body) (fail! "draft body must be text"))
        (when (> (count body) (long (:body app/bounds)))
          (fail! (str "a draft holds at most " (:body app/bounds) " characters")))
        (cond-> {:action action
                 :node-id (live-text fail! "draft node_id" (:node-id raw))
                 :body body}
          (some? (:thread-id raw))
          (assoc :thread-id (live-text fail! "draft thread_id" (:thread-id raw)))

          (some? anchor)
          (assoc :anchor
            (app-anchor fail!
                        (cond-> anchor
                          (map? anchor)
                          (update-keys #(wire/engine-key (name %))))))

          (some? (:base-seq raw))
          (assoc :base-seq (base-seq fail! (:base-seq raw)))

          (some? (:revision raw))
          (assoc :revision (live-text fail! "draft revision" (:revision raw)))))

      :cancel
      {:action action}

      :activate
      {:action action :node-id (live-text fail! "activate node_id" (:node-id raw))}

      :shortcut
      (let [k (:key raw)]
        (when-not (app/shortcut-key? k)
          (fail! (str "shortcut key " (pr-str k) " is not valid — use " app/shortcut-key-text)))
        {:action action :key k})

      :select
      (let [node-id
            (live-text fail! "select node_id" (:node-id raw))

            item-ids
            (:item-ids raw)]

        (when-not (sequential? item-ids) (fail! "select item_ids must be an array"))
        {:action action
         :node-id node-id
         :item-ids (mapv #(live-text fail! "each selected item id" %) item-ids)})

      :interrupt
      (let [note (:note raw)]
        (when (coll? note) (fail! "interrupt note must be text"))
        (cond-> {:action action}
          (some? note)
          (assoc :note note))))))

(def ^:private validate-wait-ms "How long a submit waits for the owner to judge its inputs." 5000)

(defonce ^:private validator-ids
  ;; Open view id -> the input ids whose validators the owner runs. Code does not
  ;; persist, so every open and adoption declares them again.
  (atom {}))

(defonce ^:private validations
  ;; Request id -> {:view-id :answer promise :ids}: one submit that waits for its owner.
  (atom {}))

(defn- declared-validators
  "The input ids that the `validators` of open or patch envelope `opts` names, or
   nil. A keyless open names only nodes of its own view; a keyed open can adopt an
   app that has more nodes, so [[declare-validators!]] checks it after the open."
  [opts]
  (let [ids (get opts "validators")]
    (when (some? ids)
      (when-not (and (sequential? ids) (<= (count ids) 64) (every? util/non-blank-string? ids))
        (invalid-live-view! "validators must be a list of at most 64 input ids"))
      (when (and (= "open" (get opts "op")) (nil? (get-in opts ["view" "key"])))
        (let [named (into #{}
                          (comp (mapcat #(tree-seq map?
                                                   (fn [node]
                                                     (get node "children"))
                                                   %))
                                (keep #(or (get % "id") (get % "name"))))
                          (get-in opts ["view" "nodes"]))]
          (when-let [missing (first (remove named ids))]
            (invalid-live-view!
              (str "validators names " missing ", which is no node of the view")))))
      (set ids))))

(defn- node-key
  "The id of live `node`: an input can name itself with `:name` alone."
  [node]
  (or (:id node) (:name node)))

(defn- declare-validators!
  "Keep the validator `ids` of open view `view-id` that name a node of `nodes`.
   nil ids keep the declared ids that still name a node: a removed input loses
   its validator."
  [view-id ids nodes]
  (let [named
        (into #{} (comp (mapcat #(tree-seq map? :children %)) (keep node-key)) nodes)

        ids
        (or ids (get @validator-ids view-id) #{})

        kept
        (into #{} (filter named) ids)]

    (when-let [missing (seq (remove named ids))]
      (tel/log! {:level :warn
                 :id ::validator-without-node
                 :data {:view-id view-id :input-ids (vec missing)}
                 :msg "Vis dropped validators that name no node of the live view"}))
    (if (seq kept) (swap! validator-ids assoc view-id kept) (swap! validator-ids dissoc view-id))))

(defn- validated-inputs
  "The inputs that a submit of `node-id` in live `entry` collects."
  [entry node-id]
  (let [nodes
        (:nodes @(:view entry))

        node
        (when node-id (view-node nodes node-id))]

    (cond (nil? node-id) (if (:is-app entry) (input-fields nodes) [])
          (and (= :button (:type node)) (= :submit (:action node))) (input-fields
                                                                      (submit-scope nodes node-id))
          :else [])))

(defn- owner-check
  "nil when the owner of live `entry` accepts the submit of `node-id`, else the
   refusal with field errors. The owner judges the inputs that it gave validators:
   the host queues one `validate` event and waits for the `validated` answer. It
   waits outside the view lock, so the owner can read and patch the view."
  [entry view-id node-id values]
  (let [ids
        (get @validator-ids view-id)

        inputs
        (when (seq ids) (validated-inputs entry node-id))

        checked
        (filterv #(contains? ids (node-key %)) inputs)]

    (when (seq checked)
      (let [stored
            (into {}
                  (keep (fn [field]
                          (when (contains? field :value) [(:id field) (:value field)])))
                  inputs)

            outcome
            (validate-values inputs (merge stored (select-keys values (map :id inputs))))]

        ;; A value that its type refuses is refused by the submit itself.
        (when (:is-accepted outcome)
          (let [request-id
                (str (random-uuid))

                answer
                (promise)]

            (swap! validations assoc
              request-id
              {:view-id view-id :answer answer :ids (set (map node-key checked))})
            (try (push-live-event!
                   entry
                   (cond-> {:type :validate :request-id request-id :values (:values outcome)}
                     node-id
                     (assoc :node-id node-id)))
                 (let [errors (deref answer validate-wait-ms ::no-answer)]
                   (cond (= ::no-answer errors)
                         (do (tel/log! {:level :warn
                                        :id ::owner-validation-timeout
                                        :data {:view-id view-id :node-id node-id}
                                        :msg
                                        "The owner of a live view did not judge a submit in time"})
                             {:is-accepted false
                              :reason "invalid"
                              :errors {(or node-id (node-key (first checked)))
                                       "The app did not answer. Try again."}})
                         (seq errors) {:is-accepted false :reason "invalid" :errors errors}))
                 (finally (swap! validations dissoc request-id)
                          ;; The question is answered; later readers do not see it.
                          (swap! (:events entry) update
                            :items
                            (fn [items]
                              (filterv #(not= request-id (:request-id %)) items)))))))))))

(defn- validated!
  "Deliver the owner's answer `opts` to its waiting submit. Answers whether a
   submit took it: a late or repeated answer changes nothing."
  [view-id opts]
  (let [request-id
        (live-text invalid-live-patch! "a validated op's request_id" (get opts "request_id"))

        errors
        (get opts "errors")

        {:keys [answer ids] :as waiting}
        (get @validations request-id)]

    (when-not (or (nil? errors) (map? errors))
      (invalid-live-patch! "validated errors must be an object of input id to message"))
    {:view-id view-id
     :is-open (some? (live-entry view-id))
     :is-accepted (boolean (and waiting
                                (= view-id (:view-id waiting))
                                (deliver answer
                                         (into {}
                                               (keep (fn [[k v]]
                                                       (when (and (contains? ids (str k))
                                                                  (string? v)
                                                                  (not (str/blank? v)))
                                                         [(str k) (subs v 0 (min 500 (count v)))])))
                                               errors))))}))

(defn- press-node!
  "Press button or selectable card `node-id` of live `entry`, as a surface does:
   one more click and one owner `activate` event. `base` starts the answer."
  [entry view-id node-id base]
  (let [cell (:view entry)]
    (locking cell
      (if-not (identical? entry (get @pending view-id))
        (assoc base
          :is-accepted false
          :reason "unknown")
        (let [node (first (filter
                            #(= node-id (:id %))
                            (tree-seq #(seq (:children %)) :children {:children (:nodes @cell)})))]
          (when-not (or (= :button (:type node)) (and (= :card (:type node)) (:is-selectable node)))
            (invalid-view-action! view-id "activate requires a button or a selectable card node"))
          (if (:is-disabled node)
            (assoc base
              :is-accepted false
              :reason "disabled")
            (do (patch-live!
                  view-id
                  [{:op :set :node-id node-id :clicks (inc (long (or (:clicks node) 0)))}])
                (push-live-event! entry {:type :activate :node-id node-id})
                (assoc base
                  :is-accepted true
                  :node-id node-id))))))))

(defn action!
  "Apply one operator action to open View `view-id` through the shared action seam.

   `submit` and `cancel` are input policy; `select`, `activate` and `interrupt` are live policy.
   The action map is closed and accepts snake_case string keys or kebab keywords.
   Every outcome carries `:action`, `:view-id`, and `:is-accepted`; malformed or
   kind-incompatible actions are refused before they can mutate the View."
  [view-id raw]
  (let [view-id
        (str view-id)

        {:keys [action values node-id item-ids note value] :as normalized}
        (normalize-view-action view-id raw)

        entry
        (get @pending view-id)

        base
        {:action action :view-id view-id}]

    (cond
      (and (nil? entry) (contains? @closed-apps view-id)) (assoc base
                                                            :is-accepted false
                                                            :reason "closed")
      (and (nil? entry) (contains? @stale-views view-id))
      (if (= :interrupt action)
        (assoc base :is-accepted (boolean (dismiss-stale! view-id)))
        (assoc base
          :is-accepted false
          :reason "stale"))
      (nil? entry) (assoc base
                     :is-accepted false
                     :reason "unknown")
      ;; Nobody hears a press until the owner of a restored app opens it again.
      (and (app-stale? entry) (#{:activate :select :change :shortcut} action))
      (cond-> (assoc base
                :is-accepted false
                :reason "disconnected")
        node-id
        (assoc :node-id node-id))
      :else
      (let [kind
            (:kind entry)

            supported?
            (case action
              :submit
              (or (= :input kind) (and (= :live kind) (or (:is-app entry) (some? node-id))))

              :cancel
              (= :input kind)

              (:draft :shortcut)
              (boolean (:is-app entry))

              :interrupt
              (and (= :live kind) (not (:is-app entry)))

              (:select :activate :change)
              (= :live kind))]

        (when-not supported? (unsupported-view-action! view-id kind action))
        (case action
          :submit
          (merge base
                 (or (when (and (= :live kind)
                                (not (and (:is-app entry)
                                          (:idempotency-key normalized)
                                          (app/submission-for-key @(:view entry)
                                                                  (:idempotency-key normalized)))))
                       (owner-check entry view-id node-id values))
                     (cond (:is-app entry) (app-submit! entry view-id normalized)
                           (= :live kind) (live-submit! entry view-id node-id values)
                           :else (submit! view-id values))))

          :draft
          (merge base (app-draft! entry view-id normalized))

          :change
          (merge base
                 (if (:is-app entry)
                   (app-locked entry
                               view-id
                               (fn [current]
                                 (or (app-refusal current normalized [node-id] nil)
                                     (assoc (live-change! current view-id node-id value)
                                       :seq (:seq @(:view current))))))
                   (live-change! entry view-id node-id value)))

          :cancel
          (if (false? (:is-cancellable entry))
            (assoc base
              :is-accepted false
              :reason "not_cancellable")
            (let [accepted? (boolean (cancel! view-id))]
              (cond-> (assoc base :is-accepted accepted?)
                (not accepted?)
                (assoc :reason "unknown"))))

          :activate
          (press-node! entry view-id node-id base)

          :shortcut
          (let [cell
                (:view entry)

                k
                (:key normalized)]

            (locking cell
              (let [shortcut (app/find-shortcut @cell k)]
                (cond (not (identical? entry (get @pending view-id))) (assoc base
                                                                        :is-accepted false
                                                                        :reason "closed")
                      (nil? shortcut) (assoc base
                                        :is-accepted false
                                        :reason "unbound"
                                        :key k)
                      ;; A node shortcut presses its node, as a surface does.
                      (:node-id shortcut)
                      (assoc (press-node! entry view-id (:node-id shortcut) base) :key k)
                      :else (do (push-live-event!
                                  entry
                                  {:type :shortcut :action (:action shortcut) :key k})
                                (assoc base
                                  :is-accepted true
                                  :key k
                                  :shortcut (:action shortcut)))))))

          :select
          (let [patched
                (select-live! view-id node-id item-ids)

                selected
                (vec (:selected-ids (first (filter #(= node-id (:id %))
                                                   (tree-seq #(seq (:children %))
                                                             :children
                                                             {:children (:nodes patched)})))))]

            ;; The owner hears each accepted selection, as it hears a press.
            (push-live-event! entry {:type :select :node-id node-id :item-ids selected})
            (assoc base
              :is-accepted true
              :node-id node-id
              :item-ids selected))

          :interrupt
          (let [accepted? (some? (interrupt-live! view-id note))]
            (cond-> (assoc base :is-accepted accepted?)
              (not accepted?)
              (assoc :reason "unknown"))))))))

(defn cancel-all!
  "Cancel every pending request. Returns how many were released. Used when a
   channel detaches or the session shuts down, so no thread stays parked — this
   one ignores `:is-cancellable`, because nothing is left to answer with."
  ([] (cancel-all! "cancelled"))
  ([reason]
   ;; An app is no wait: it outlives a detaching channel and a closing session.
   (count (filterv #(force-cancel! % reason)
            (keep (fn [[id entry]]
                    (when-not (:is-app entry) id))
                  @pending)))))

(defonce ^:private ask-gate
  ;; Decides whether a session may ask the person at all. The layer that knows the
  ;; session tree installs it; this namespace cannot reach that layer.
  (atom nil))

(defn set-ask-gate!
  "Install what decides whether a session may ask the person: called
   `[session-id]`, truthy when it may. Without a gate, every session may ask.

   The composition root installs a gate that refuses a managed subagent. Its
   leader answers its questions and decides its grants, so the person never sees
   a form that a subagent raises."
  [f]
  (reset! ask-gate f))

(defn- may-ask?
  "Whether session `session-id` may ask the person. A gate that throws allows the
   request and logs the failure: a broken lookup must not take a form from a
   session that may ask."
  [session-id]
  (if-let [f @ask-gate]
    (try (boolean (f session-id))
         (catch Throwable t
           (tel/log! {:level :error
                      :id ::ask-gate-failed
                      :data {:session-id session-id :error (ex-message t)}
                      :msg "The ask gate failed, so the human-input request goes out unchecked"})
           true))
    true))

(defn- refused
  "The answer of a request that its session may not raise. Nothing was published,
   so no surface shows it and nobody can answer it."
  [entry]
  (tel/log! {:level :warn
             :id ::request-refused
             :data {:request-id (:id entry) :title (:title entry) :session-id (:session-id entry)}
             :msg "Human-input request refused: this session may not ask the person"})
  {:is-submitted false :reason "undeliverable" :request-id (:id entry)})

(defn request!
  "Ask the operator for typed values and BLOCK until they answer.

   `request` is a spec map — `:title`, `:nodes`, optional `:description`,
   `:submit-label`, `:cancel-label`, `:is-cancellable`, `:timeout-ms`,
   `:channel-ids` (string keys from the Python boundary work too).

   Every field carries `:name`, `:type`, `:label` and an optional
   `:description`. `:name` keys the answer in `:values`, `:label` is what the
   dialog shows above the input, and `:description` is the italic line under
   that label — see [[normalize-field]].

   Publishes a `:view/open` channel event, waits for [[submit!]] /
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

   A session that may not ask the person also gets `\"undeliverable\"` at once.
   No surface ever sees its request; [[set-ask-gate!]] decides it.

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
  (let [entry
        (assoc (normalize-request request)
          :kind :input
          :promise (promise)
          :created-at (util/now-ms))

        request-id
        (:id entry)]

    (when-not (trimmed (:session-id entry))
      (invalid-request! (str "request " request-id
                             " names no session — set :session-id, or raise it "
                             "from an extension environment that carries one")))
    (when (contains? @pending request-id)
      (invalid-request! (str "request id " request-id " is already pending")))
    (if-not (may-ask? (:session-id entry))
      (refused entry)
      (do
        (swap! pending assoc request-id entry)
        (tel/log! {:level :debug
                   :id ::request-opened
                   :data {:request-id request-id
                          :fields (mapv :id (input-fields (:nodes entry)))
                          :timeout-ms (:timeout-ms entry)}
                   :msg "Human-input request opened"})
        (if (zero? (long (publish!
                           (:channel-ids entry)
                           (lifecycle-event :view/open entry {:view (request->view entry)}))))
          (do (tel/log! {:level :error
                         :id ::request-undeliverable
                         :data {:request-id request-id
                                :title (:title entry)
                                :channel-ids (:channel-ids entry)
                                :session-id (:session-id entry)}
                         :msg (str "Human-input request reached no channel — nothing is mounted on "
                                   (pr-str (:channel-ids entry))
                                   ", so no dialog can be drawn and nobody can answer")})
              (settle! request-id
                       {:is-submitted false :reason "undeliverable" :request-id request-id})
              ;; `settle!` delivered, unless a racing submit! got there first.
              @(:promise entry))
          (let [timeout-ms
                (:timeout-ms entry)

                result
                ;; Waiting on a human is NOT wall-clock work an enclosing timeout may
                ;; bill: park the Python evaluation watchdog for as long as the operator
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
              (do (tel/log!
                    {:level :warn
                     :id ::request-timed-out
                     :data {:request-id request-id :title (:title entry) :timeout-ms timeout-ms}
                     :msg "Human-input request timed out — nobody answered, resuming without one"})
                  (settle! request-id
                           {:is-submitted false :reason "timeout" :request-id request-id})
                  ;; `settle!` delivered, or a submit! that won the race already did.
                  @(:promise entry))
              result)))))))

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
          (let [field-name
                (trimmed (pick field :name :id))

                declared
                (get counts field-name)

                children
                (:children field)]

            (cond-> field
              (sequential? children)
              (assoc :children (attach-validators children counts run))

              (and (number? declared) (pos? (long declared)))
              (assoc :validate
                (mapv (fn [index]
                        (fn [value values]
                          (run field-name index value values)))
                      (range (long declared)))))))
        fields))

(defn request-json!
  "The strings-only seam a Python extension crosses: a JSON request object in, a
   JSON answer object out. Blocks exactly like [[request!]]. The request's keys
   become engine keywords here, once, before [[normalize-request]] reads them.

   Channel routing is host-side — a `channel_id`/`channel_ids` key is dropped
   rather than minting keywords from guest data, so a Python extension always
   reaches the channels the host picked.

   Validation is CODE, so it does not travel as JSON either: `validators-json` is
   `{field name -> how many validators that field declared}` and `run` is called
   `(run field-name index value values)` to reach the extension's own function,
   answering the verdict
   [[com.blockether.vis.internal.view.validation/check]] understands
   (nil/true, a message string, false, or a throw). Only a name, an index and the
   value being judged ever cross."
  ([request-json] (request-json! request-json nil nil))
  ([request-json validators-json run]
   (let [request
         (json/read-json (str request-json) :key-fn identity)

         counts
         (when (and run (not (str/blank? (str validators-json))))
           (json/read-json (str validators-json) :key-fn identity))]

     (when-not (map? request) (invalid-request! "request must be a JSON object"))
     (-> request
         (dissoc "channel_id" "channel_ids")
         spec<-json
         (cond->
           (seq counts)
           (update :nodes #(if (sequential? %) (attach-validators % counts run) %)))
         request!
         answer->wire
         json/write-json-str))))

(def ^:private live-ops
  "Operations implemented by the live view host door."
  {:default "open"
   :spawn #{"open"}
   :handle #{"patch" "state" "events" "close" "reply" "submission" "archive" "validated"}})

(defn- live-op-name
  "The op `opts` asks for, refused BY NAME when it is in neither list. An options
   map with no `op` means the default one, exactly as a shell options map does."
  [opts]
  (let [op (or (trimmed (get opts "op")) (:default live-ops))]
    (when-not (or (contains? (:spawn live-ops) op) (contains? (:handle live-ops) op))
      (throw (ex-info (str "unknown live view op " (pr-str op)
                           " — use " (str/join ", "
                                               (sort (into (:spawn live-ops) (:handle live-ops)))))
                      {:type :vis/view-unknown-live-op :op op})))
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
  (let [closed
        (get @closed-apps view-id)

        sid
        (cond closed (:session-id closed)
              (contains? @ended-owned view-id) (get @ended-owned view-id)
              :else (ambient-session-id))

        result
        (sink/verdict (sink/view-file sid view-id))]

    (when-not result
      (invalid-live-patch!
        (str "no live view " view-id " is open — it was closed, interrupted, or never opened")))
    ;; A closed app stays readable: its owner still reads the receipts.
    (cond-> {:view-id view-id :is-open false :result result}
      closed
      (assoc :view closed))))

(defn- live-state
  "Read a snapshot, or wait on its monitor until its sequence changes or it closes.
   Check and wait share the patch/close lock, so no notification can be lost.
   A timeout carries no picture and publishes no activity or view event."
  [view-id opts]
  (let [timeout
        (get opts "timeout_ms" 0)

        after
        (get opts "after_seq")]

    (when-not (and (integer? timeout)
                   (<= 0 timeout 86400000)
                   (or (zero? (long timeout)) (and (integer? after) (<= 0 after Long/MAX_VALUE))))
      (invalid-live-patch!
        "state timeout_ms must be 0..86400000; a wait requires nonnegative after_seq"))
    (if-let [entry (live-entry view-id)]
      (let [cell (:view entry)
            began (System/nanoTime)]

        (locking cell
          (loop []

            (let [view @cell
                  remaining (- (long timeout) (quot (- (System/nanoTime) began) 1000000))]

              (cond (realized? (:promise entry))
                    {:view-id view-id :is-open false :result @(:promise entry)}
                    (or (zero? (long timeout)) (not= after (:seq view)))
                    {:view-id view-id :is-open true :view view}
                    (<= remaining 0) {:view-id view-id :is-open true :timed-out true}
                    :else (do (.wait ^Object cell (long remaining)) (recur)))))))
      (live-ended view-id))))

(defn- recorded-app!
  "Read app `view-id` back from its record when this process has not read it yet,
   so that a handle op after a restart finds it."
  [view-id]
  (when-not (or (contains? @pending view-id) (contains? @closed-apps view-id))
    (let [sid
          (or (get @ended-owned view-id) (ambient-session-id))

          own
          (when sid (sink/view-file sid view-id))

          file
          (if (and own (.isFile ^java.io.File own)) own (sink/find-view-file view-id))]

      (when file (restore-app! (.getName (.getParentFile ^java.io.File file)) view-id)))))

(defn- owned-app
  "App `view-id` that the current owner drives, open or closed. nil for a view
   that is no app. The sandbox drives only the apps of its own session."
  [view-id]
  (let [app
        (some #(when (app/app? %) %)
              [(some-> (live-entry view-id)
                       :view
                       deref) (get @closed-apps view-id)])

        sid
        (when (and app (nil? (:extension app))) (ambient-session-id))]

    (when (and app (not= (:extension app) (:extension *live-owner*)))
      (invalid-live-patch! (str "app " view-id
                                " belongs to " (if-let [owner (:extension app)]
                                                 (str "extension " owner)
                                                 "the python_execution sandbox"))))
    (when (and sid (:session-id app) (not= sid (:session-id app)))
      (invalid-live-patch! (str "app " view-id " belongs to another session")))
    app))

(defn- app-only!
  [view-id op]
  (when-not (owned-app view-id)
    (invalid-live-patch! (str "a live " op " needs an app — " view-id " is no app"))))

(defn- closed-app!
  "Refuse `op` on closed app `view-id`. A closed app is read-only, so a write that
   the owner expects to land fails loudly instead of answering the ended picture."
  [view-id op]
  (invalid-live-patch!
    (str "a live " op " needs an open app — app " view-id " is closed and read-only")))

(defn- owner-checked
  "Call `f`. An app rule that it breaks refuses the owner's op as an invalid patch."
  [f]
  (app-checked "Invalid live-view patch: " {:type :vis/view-invalid-patch} f))

(defn- app-reply!
  "Post the reply of the owner of open app `view-id` in a thread of node
   `node_id`. Without `thread_id`, a diff starts a thread at `anchor`. With
   `submission_id`, the Send that the reply answers moves to `answered`."
  [view-id opts]
  (let [node-id
        (live-text invalid-live-patch! "a reply's node_id" (get opts "node_id"))

        body
        (get opts "body")

        submission-id
        (trimmed (get opts "submission_id"))

        cell
        (:view (live-entry! view-id))]

    (when-not (string? body) (invalid-live-patch! "a reply's body must be text"))
    (locking cell
      (live-entry! view-id)
      (let [view
            @cell

            node
            (or (app-node view node-id)
                (invalid-live-patch! (str "app " view-id " has no node " node-id)))

            receipt
            (when submission-id
              (or (app/find-submission view submission-id)
                  (invalid-live-patch! (str "app " view-id " has no submission " submission-id))))

            message
            (cond-> {:id (str (random-uuid))
                     :author (owner-author)
                     :body body
                     :created-at (util/now-ms)}
              submission-id
              (assoc :submission-id submission-id))

            op
            (owner-checked #(app/reply-op node
                                          {:thread-id (trimmed (get opts "thread_id"))
                                           :anchor (some->> (get opts "anchor")
                                                            spec<-json
                                                            (app-anchor invalid-live-patch!))
                                           :new-thread-id (str (random-uuid))}
                                          message))

            moved
            (some-> receipt
                    (receipt-move "answered" nil))

            patched
            (binding [*host-patch* true]
              (patch-live! view-id
                           (cond-> [op]
                             moved
                             (conj {:op :submission :submission moved}))))]

        (cond-> {:view-id view-id
                 :is-open true
                 :node-id node-id
                 :thread-id (get-in op [:thread :id])
                 :message-id (:id message)
                 :seq (:seq patched)}
          receipt
          (assoc :submission (or moved receipt)))))))

(defn- archive-app!
  "Close app `view-id` when it is open and mark it archived. An archived app
   leaves the default list of its session and refuses every action."
  [view-id]
  (let [view (owned-app view-id)]
    (when-not (:is-archived view)
      (when (live-entry view-id) (close-live! view-id {:summary "The owner archived the app."}))
      (sink/archive! (sink/view-file (:session-id view) view-id))
      (swap! closed-apps update view-id #(assoc (or % (closed-picture view)) :is-archived true)))
    {:view-id view-id :is-open false :is-closed true :is-archived true}))

(defn- redeliver-sends!
  "Give the owner of open app `view-id` each Send that it did not get yet: a Send
   from before a restart, or one that came while no owner was there. An owner
   without a listener reads them after the events cursor of its open."
  [view-id]
  (when-let [cell (:view (live-entry view-id))]
    (locking cell
      (let [entry (live-entry view-id)]
        (when (and (:is-app entry) (or (:listener entry) (:extension @cell)))
          (doseq [receipt (:submissions @cell)
                  :when (and (= "accepted" (:status receipt))
                             (not (contains? @(:delivering entry) (:id receipt))))]

            (push-live-event! entry
                              (send-event receipt
                                          (into {} (map (juxt :node-id :value)) (:values receipt))
                                          (:comments receipt)))))))))

(defn- read-sends!
  "Mark each Send in `events` delivered: the owner of app `view-id` without a
   listener reads its events itself."
  [view-id events]
  (doseq [submission-id (keep :submission-id events)]
    (try (advance-receipt! view-id submission-id "delivered" nil)
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::send-read-not-recorded
                      :data {:view-id view-id :submission-id submission-id :error (ex-message t)}
                      :msg "Vis could not mark a Send that its owner read"})))))

(defn live-dispatch
  "One live-view op: an options map with wire keys in, the answer map out. The
   view, patch or ending it carries becomes engine keywords here, once.

   `open` answers the mounted view, `patch` the sequence number the engine
   accepted, `state` what the view looks like right now, and `close` the verdict
   the model reads. Every answer says `:is-open`, so the caller learns from the
   op it was making that the human stopped watching — the record's verdict comes
   back with it."
  [opts]
  (when-not (map? opts) (invalid-live-view! "a live envelope must be a JSON object"))
  (let [op (live-op-name opts)]
    (when-not (= "open" op)
      ;; Only its own extension drives an owned view, and only its owner an app.
      (let [view-id (live-handle-id opts op)
            _ (recorded-app! view-id)
            owner (first (:owned-key (live-entry view-id)))]

        (when (and owner (not= owner (:extension *live-owner*)))
          (invalid-live-patch! (str "live view " view-id " belongs to extension " owner)))
        (owned-app view-id)))
    (case op
      "open"
      (let [ids (declared-validators opts)
            view (supersede-owned! (open-live! (spec<-json (get opts "view"))))]

        (swap! validator-ids dissoc (:id view))
        (declare-validators! (:id view) (or ids #{}) (:nodes view))
        ;; The owner reads from here: older events are not its own, and the Sends
        ;; that it still owes come after this cursor.
        (let [cursor (or (some-> (live-entry (:id view))
                                 :events
                                 deref
                                 :cursor)
                         0)]
          (when (app/app? view) (redeliver-sends! (:id view)))
          {:view-id (:id view) :is-open true :view view :events-cursor cursor}))

      "patch"
      ;; Asked BEFORE patching rather than catching the refusal after: a patch
      ;; the human's interrupt raced is not a spec problem, and an author must
      ;; not have to tell those two apart by reading a message.
      (let [view-id (live-handle-id opts op)
            ids (declared-validators opts)]

        (if-let [entry (live-entry view-id)]
          (let [cell (:view entry)]
            (locking cell
              (if (live-entry view-id)
                (let [patched (patch-live! view-id (spec<-json (get opts "patch")))]
                  ;; Validators follow the node tree: a patch declares new ones.
                  (declare-validators! view-id ids (:nodes patched))
                  {:view-id view-id :is-open true :seq (:seq patched)})
                (live-ended view-id))))
          (live-ended view-id)))

      "state"
      (live-state (live-handle-id opts op) opts)

      "events"
      (let [view-id (live-handle-id opts op)
            after (get opts "after" 0)
            timeout (get opts "timeout_ms" 0)]

        (when-not (and (integer? after)
                       (<= 0 (long after))
                       (integer? timeout)
                       (<= 0 (long timeout) 86400000))
          (invalid-live-patch!
            "events after must be a nonnegative integer; timeout_ms must be 0..86400000"))
        (if-let [entry (live-entry view-id)]
          (if (app-stale? entry)
            ;; A restart dropped the owner: it opens the app again with its key.
            {:view-id view-id :is-open true :is-stale true :events [] :cursor 0}
            (let [answer (live-events view-id after timeout)]
              (when (and (:is-app entry) (nil? (:listener entry)))
                (read-sends! view-id (:events answer)))
              (assoc answer :view-id view-id)))
          (assoc (live-ended view-id)
            :events []
            :cursor after)))

      "reply"
      (let [view-id (live-handle-id opts op)]
        (app-only! view-id op)
        (if (live-entry view-id) (app-reply! view-id opts) (closed-app! view-id op)))

      "submission"
      (let [view-id (live-handle-id opts op)]
        (app-only! view-id op)
        (if (live-entry view-id)
          (assoc (advance-receipt! view-id
                                   (live-text invalid-live-patch!
                                              "a submission op's submission_id"
                                              (get opts "submission_id"))
                                   (get opts "status")
                                   (get opts "note"))
            :view-id view-id
            :is-open true)
          (closed-app! view-id op)))

      "archive"
      (let [view-id (live-handle-id opts op)]
        (app-only! view-id op)
        (archive-app! view-id))

      "validated"
      (validated! (live-handle-id opts op) opts)

      "close"
      (let [view-id (live-handle-id opts op)]
        (if-let [result (close-live! view-id (spec<-json (or (get opts "ending") {})))]
          {:view-id view-id :is-open false :result result}
          (live-ended view-id))))))

(defn live-json!
  "The strings-only seam a Python extension crosses for a live view: one JSON
   envelope in, one JSON answer out.

   Channel routing is host-side — a `channel_id`/`channel_ids` key is dropped
   rather than minting keywords from guest data — so an extension always reaches
   the channels the host picked, exactly as [[request-json!]] does.

   Only a `state` with `timeout_ms` blocks: it waits for a change or close,
   without periodic guest callbacks or publishing a view event.

   `owner` is the [[*live-owner*]] of the extension door that crosses."
  ([envelope-json] (live-json! envelope-json nil))
  ([envelope-json owner]
   (let [envelope (json/read-json (str envelope-json) :key-fn identity)]
     (when-not (map? envelope) (invalid-live-view! "a live envelope must be a JSON object"))
     (binding [*live-owner* owner]
       (-> envelope
           (dissoc "channel_id" "channel_ids")
           live-dispatch
           wire/json-str)))))

(defn app-json!
  "The app seam of the `python_execution` sandbox: [[live-json!]] without an owner.
   The sandbox opens only apps, and it drives only the apps of its own session."
  [envelope-json]
  (let [envelope (json/read-json (str envelope-json) :key-fn identity)]
    (when-not (map? envelope) (invalid-live-view! "a live envelope must be a JSON object"))
    (binding [*live-owner* nil]
      (let [op (live-op-name envelope)]
        (if (= "open" op)
          (when-not (= (name app/placement) (get-in envelope ["view" "placement"]))
            (invalid-live-view!
              "the python_execution sandbox opens only apps: attach(ui.app(...))"))
          (let [view-id (live-handle-id envelope op)]
            (recorded-app! view-id)
            (app-only! view-id op))))
      (-> envelope
          (dissoc "channel_id" "channel_ids")
          live-dispatch
          wire/json-str))))

(def ^:private sdk-owner-pattern
  "The owner name that an SDK program gives the gateway app door."
  #"[a-z0-9][a-z0-9._-]{0,63}")

(def ^:private sdk-events-wait-ms
  "The longest `events` wait of the gateway app door, so that one HTTP request
   never holds a connection for minutes."
  30000)

(defn- sdk-refusal!
  [status message]
  (throw (ex-info message {:type :vis/view-app-door-refused :status status :reason message})))

(defn owner-app-json!
  "The gateway app door of an SDK program: [[live-json!]] for owner `owner` in
   session `sid`. The owner is bound as extension `sdk/<owner>`.

   The door opens only apps of session `sid`, and it drives only the apps of that
   owner in that session. A refusal of the owner throws `ex-info` with `:status`
   403; every other refusal is the normal seam refusal."
  [sid owner envelope-json]
  (when-not (and (string? owner) (re-matches sdk-owner-pattern owner))
    (sdk-refusal! 400
                  (str "owner must match "
                       sdk-owner-pattern
                       ": a lowercase letter or digit, then up to 63 of a-z 0-9 . _ -")))
  (let [envelope
        (json/read-json (str envelope-json) :key-fn identity)

        extension-name
        (str "sdk/" owner)]

    (when-not (map? envelope) (invalid-live-view! "a live envelope must be a JSON object"))
    (binding [*live-owner*
              {:extension extension-name}

              extension/*current-environment*
              {:session-id sid}]

      (let [op (live-op-name envelope)]
        (if (= "open" op)
          (let [view (get envelope "view")]
            (when-not (and (map? view) (= (name app/placement) (get view "placement")))
              (invalid-live-view! "the gateway app door opens only apps: placement \"attachment\""))
            (when-not (contains? #{nil sid} (trimmed (get view "session_id")))
              (sdk-refusal! 403 (str "the gateway app door opens apps only in session " sid))))
          (let [view-id (live-handle-id envelope op)
                _ (recorded-app! view-id)
                app (some #(when (app/app? %) %)
                          [(some-> (live-entry view-id)
                                   :view
                                   deref) (get @closed-apps view-id)])]

            (when-not app
              (invalid-live-patch! (str "a live " op " needs an app — " view-id " is no app")))
            (when-not (= sid (:session-id app))
              (sdk-refusal! 403 (str "app " view-id " belongs to another session")))
            (when-not (= extension-name (:extension app))
              (sdk-refusal! 403
                            (str "app " view-id
                                 " belongs to " (if-let [other (:extension app)]
                                                  (str "extension " other)
                                                  "the python_execution sandbox"))))
            (when (and (= "events" op)
                       (let [timeout (get envelope "timeout_ms" 0)]
                         (and (integer? timeout) (< sdk-events-wait-ms (long timeout)))))
              (sdk-refusal! 400
                            (str "events timeout_ms must be at most "
                                 sdk-events-wait-ms
                                 " on the gateway app door"))))))
      (-> envelope
          (dissoc "channel_id" "channel_ids")
          (cond->
            (= "open" (live-op-name envelope))
            (assoc-in ["view" "session_id"] sid))
          live-dispatch
          wire/json-str))))
