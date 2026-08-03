(ns com.blockether.vis.internal.human-input
  "Typed human-input requests — the pause/resume primitive an extension uses to
   ask the operator for structured values in the middle of a run.

   An extension calls [[request!]] with a title and a vector of typed fields.
   The call BLOCKS the calling thread, publishes a `:human-input/request`
   channel event so the mounted channel can draw a dialog, and returns once a
   channel calls [[submit!]] / [[cancel!]] or the request times out. Every
   request carries a finite `:timeout-ms` — a headless or wedged channel can
   never pin an extension thread forever.

   Field types are a CLOSED set (see [[field-types]]): extension-supplied type
   names are looked up, never `keyword`-minted. Coercion and validation live in
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
            [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

(def field-types
  "Wire type name -> internal field-type keyword. Closed set."
  {"plaintext" :plaintext
   "password" :password
   "multiline" :multiline
   "select" :select
   "multiselect" :multiselect
   "checkbox" :checkbox})

(def ^:private text-types #{:plaintext :password :multiline})

(def ^:private choice-types #{:select :multiselect})

(def ^:private secret-types #{:password})

(def default-timeout-ms
  "Requests wait five minutes unless the caller asks for something else."
  300000)

(def max-timeout-ms
  "Upper bound on `:timeout-ms`. One hour — long enough for a human who stepped
   away, short enough that a forgotten dialog cannot pin a thread for a day."
  3600000)

(def ^:private secret-handle-prefix "vis-secret:")

(defonce ^:private pending (atom {}))

(defonce ^:private secrets (atom {}))

;; =============================================================================
;; Secret vault
;; =============================================================================

(defn secret-handle?
  "True when `value` is an opaque handle minted by a `:password` field."
  [value]
  (and (string? value) (str/starts-with? value secret-handle-prefix)))

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
  (let [handle (str secret-handle-prefix (random-uuid))]
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
   Python/wire boundary) or kebab-keyword-keyed (from Clojure callers)."
  [m & ks]
  (some (fn [k]
          (let [v (get m k)]
            (when (some? v) v)))
        ks))

(defn- trimmed
  [value]
  (some-> value
          str
          str/trim
          not-empty))

(defn- invalid-field!
  [field-id message]
  (throw (ex-info (str "Invalid human-input field" (when field-id (str " " field-id)) ": " message)
                  {:type :vis/human-input-invalid-field :field-id field-id :reason message})))

(defn- invalid-request!
  [message]
  (throw (ex-info (str "Invalid human-input request: " message)
                  {:type :vis/human-input-invalid-request :reason message})))

(defn- normalize-option
  [field-id option]
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

(defn- normalize-max-length
  [field-id value]
  (when (some? value)
    (let [n (long (if (number? value) value (parse-long (str value))))]
      (when-not (pos? n) (invalid-field! field-id ":max-length must be positive"))
      n)))

(declare coerce-value)

(defn normalize-field
  "Validate one field spec and return its internal form. Throws `ex-info` with
   `:type :vis/human-input-invalid-field` on a bad spec."
  [field]
  (when-not (map? field) (invalid-field! nil "field must be a map"))
  (let
    [field-id
     (trimmed (pick field "id" :id))

     _
     (when-not field-id (invalid-field! nil "field needs a non-blank :id"))

     type-name
     (or (trimmed (pick field "type" :type)) "plaintext")

     field-type
     (get field-types (str/lower-case type-name))

     _
     (when-not field-type
       (invalid-field! field-id
                       (str "unknown type " (pr-str type-name)
                            " — expected one of " (str/join ", " (sort (keys field-types))))))

     spec
     (cond->
       {:id field-id
        :type field-type
        :label (or (trimmed (pick field "label" :label)) field-id)
        ;; Optional unless the caller says otherwise — the same default every
        ;; form API has, so a spec never blocks a human on a field the
        ;; extension did not actually need.
        :is-required
        (normalize-bool field-id ":is-required" (pick field "is_required" :is-required) false)
        :is-secret (contains? secret-types field-type)}
       (trimmed (pick field "help" :help))
       (assoc :help (trimmed (pick field "help" :help)))

       (trimmed (pick field "placeholder" :placeholder))
       (assoc :placeholder (trimmed (pick field "placeholder" :placeholder)))

       (contains? choice-types field-type)
       (assoc :options (normalize-options field-id field-type (pick field "options" :options)))

       (normalize-max-length field-id (pick field "max_length" :max-length))
       (assoc :max-length (normalize-max-length field-id (pick field "max_length" :max-length))))

     raw-default
     (pick field "default" :default)

     [status default]
     (coerce-value (assoc spec :is-required false) raw-default)]

    (when (= :error status) (invalid-field! field-id (str "invalid :default — " default)))
    (cond-> spec
      (some? default)
      (assoc :default default))))

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
  [request]
  (let
    [raw
     (pick request "timeout_ms" :timeout-ms)

     ms
     (if (nil? raw)
       default-timeout-ms
       (or (if (number? raw) (long raw) (parse-long (str raw)))
           (invalid-request! ":timeout-ms must be a number")))]

    (when-not (pos? ms) (invalid-request! ":timeout-ms must be positive"))
    (min (long ms) max-timeout-ms)))

(defn normalize-request
  "Validate a human-input request spec and return its internal form. Throws
   `ex-info` with `:type :vis/human-input-invalid-request` (or
   `:vis/human-input-invalid-field`) on a bad spec."
  [request]
  (when-not (map? request) (invalid-request! "request must be a map"))
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
     (when-not (apply distinct? (map :id fields)) (invalid-request! "field ids must be distinct"))

     session-id
     (or (trimmed (pick request "session_id" :session-id)) (ambient-session-id))]

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
      (assoc :source (trimmed (pick request "source" :source))))))

;; =============================================================================
;; Value coercion — one implementation for defaults and submissions
;; =============================================================================

(defn- coerce-text
  [{:keys [type is-required max-length]} value]
  (let
    [text
     (if (nil? value) "" (str value))

     text
     (if (= :multiline type) text (str/trim text))]

    (cond (and is-required (str/blank? text)) [:error "is required"]
          (and max-length (> (count text) (long max-length)))
          [:error (str "must be at most " max-length " characters")]
          (str/blank? text) [:ok (when (= :multiline type) (when-not (empty? text) text))]
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
  [_field value]
  (cond (nil? value) [:ok false]
        (boolean? value) [:ok value]
        (contains? #{"true" "1"} (str/lower-case (str value))) [:ok true]
        (contains? #{"false" "0"} (str/lower-case (str value))) [:ok false]
        :else [:error "must be true or false"]))

(defn coerce-value
  "Coerce and validate one raw `value` against normalized `field`. Returns
   `[:ok coerced]` or `[:error message]`."
  [{:keys [type] :as field} value]
  (cond (contains? text-types type) (coerce-text field value)
        (= :select type) (coerce-select field value)
        (= :multiselect type) (coerce-multiselect field value)
        (= :checkbox type) (coerce-checkbox field value)
        :else [:error "unknown field type"]))

(defn coerce-values
  "Coerce a raw `field id -> value` map against a request's `fields`. Returns
   `{:is-accepted true :values …}` or `{:is-accepted false :errors {id msg}}`.
   `:password` values are replaced with opaque vault handles."
  [fields values]
  (let
    [values
     (or values {})

     results
     (reduce (fn [acc {:keys [id] :as field}]
               (let
                 [raw
                  (cond (contains? values id) (get values id)
                        (contains? values (keyword id)) (get values (keyword id))
                        ;; Absent means "the human left it alone" — the field's
                        ;; declared default stands in, then gets validated like
                        ;; any other value.
                        :else (:default field))

                  [status result]
                  (coerce-value field raw)]

                 (if (= :error status)
                   (assoc-in acc [:errors id] result)
                   ;; Every field id is present in `:values`, so a caller can
                   ;; read a field without knowing whether it was filled in.
                   (assoc-in acc
                     [:values id]
                     (if (and (:is-secret field) (some? result)) (stash-secret! result) result)))))
             {:values {} :errors {}}
             fields)]

    (if (seq (:errors results))
      {:is-accepted false :errors (:errors results)}
      {:is-accepted true :values (:values results)})))

;; =============================================================================
;; Channel projection
;; =============================================================================

(defn request->view
  "The channel/wire-facing projection of a pending request: the spec a dialog
   needs, and nothing a channel must not see (no promise, no submitted values)."
  [request]
  (-> request
      (dissoc :promise :channel-ids)
      (assoc :fields (mapv #(dissoc % :is-secret) (:fields request)))))

(defn- publish!
  [channel-ids event]
  (doseq [channel-id channel-ids]
    (channel-events/publish-channel-event! channel-id event)))

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

(defn cancel!
  "Cancel pending request `request-id`. Returns true when it was pending."
  ([request-id] (cancel! request-id "cancelled"))
  ([request-id reason]
   (some? (settle! request-id
                   {:is-submitted false
                    :reason (or (trimmed reason) "cancelled")
                    :request-id request-id}))))

(defn cancel-all!
  "Cancel every pending request. Returns how many were released. Used when a
   channel detaches or the session shuts down, so no thread stays parked."
  ([] (cancel-all! "cancelled"))
  ([reason] (count (filterv #(cancel! % reason) (keys @pending)))))

(defn request!
  "Ask the operator for typed values and BLOCK until they answer.

   `request` is a spec map — `:title`, `:fields`, optional `:description`,
   `:submit-label`, `:cancel-label`, `:is-cancellable`, `:timeout-ms`,
   `:channel-ids` (string keys from the Python boundary work too). Publishes a
   `:human-input/request` channel event, waits for [[submit!]] / [[cancel!]],
   and always returns a map:

     {:is-submitted true  :reason \"submitted\" :request-id … :values {…}}
     {:is-submitted false :reason \"cancelled\"|\"timeout\"|… :request-id …}

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
                      :fields (mapv :id (:fields entry))
                      :timeout-ms (:timeout-ms entry)}
               :msg "Human-input request opened"})
    (publish! (:channel-ids entry)
              {:op :human-input/request :request-id request-id :request (request->view entry)})
    (let [result (deref (:promise entry) (:timeout-ms entry) ::timeout)]
      (if (identical? ::timeout result)
        (do (settle! request-id {:is-submitted false :reason "timeout" :request-id request-id})
            ;; `settle!` delivered, or a submit! that won the race already did.
            @(:promise entry))
        result))))

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

(defn request-json!
  "The strings-only seam a Python extension crosses: a JSON request object in, a
   JSON answer object out. Blocks exactly like [[request!]].

   Channel routing is host-side — a `channel_id`/`channel_ids` key is dropped
   rather than minting keywords from guest data, so a Python extension always
   reaches the channels the host picked."
  [request-json]
  (let [request (json/read-json (str request-json) :key-fn identity)]
    (when-not (map? request) (invalid-request! "request must be a JSON object"))
    (-> request
        (dissoc "channel_id" "channel_ids")
        request!
        answer->wire
        json/write-json-str)))
