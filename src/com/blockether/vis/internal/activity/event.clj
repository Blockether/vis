(ns com.blockether.vis.internal.activity.event
  "Versioned, bounded lifecycle events for host-observed tool invocations.

   Events are presentation input, never operation control. Construction redacts
   before measuring, collectors reject broken lifecycle order, and a sink failure
   must never change the value or exception the Python caller observes."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import [java.nio.charset StandardCharsets]
           [java.util UUID]
           [java.util.concurrent.atomic AtomicLong]))

(def schema-version 1)
(def max-event-bytes (* 16 1024))
(def max-summary-bytes 512)
(def max-detail-bytes (* 2 1024))
(def max-resources 8)
(def ^:private max-summary-nodes 128)

(def ^:private secret-key?
  #(boolean (re-find #"(?i)(password|passwd|secret|token|authorization|cookie|otp|api[_-]?key)"
                     (name %))))

(defn utf8-bytes ^long [x] (long (alength (.getBytes (str x) StandardCharsets/UTF_8))))

(defn bounded-text
  "UTF-8 truncate text to at most `limit`, appending an omission marker."
  [value limit]
  (let [s
        (str value)

        limit
        (long limit)]

    (if (<= (utf8-bytes s) limit)
      s
      (let [marker
            "…"

            room
            (Math/max 0 (- limit (utf8-bytes marker)))]

        (loop [end (long (count s))]
          (let [candidate (subs s 0 end)]
            (if (or (zero? end) (<= (utf8-bytes candidate) room))
              (str candidate marker)
              (recur (dec end)))))))))

(defn redact
  "Remove credential-bearing values recursively before summaries or sizes exist."
  [value]
  (cond (map? value) (into (empty value)
                           (map (fn [[k v]]
                                  [k (if (secret-key? k) "[REDACTED]" (redact v))]))
                           value)
        (vector? value) (mapv redact value)
        (set? value) (into #{} (map redact) value)
        (sequential? value) (mapv redact value)
        (and (string? value) (str/starts-with? value "vis-secret:")) "[SECRET HANDLE]"
        :else value))

(defn- bounded-redact-result
  "Redact a representative prefix without traversing an unbounded result graph."
  [value]
  (let [remaining
        (volatile! max-summary-nodes)

        truncated?
        (volatile! false)

        omitted
        "…"]

    (letfn
      [(visit [x]
         (if-not (pos? (long @remaining))
           (do (vreset! truncated? true) omitted)
           (do (vswap! remaining #(unchecked-dec (long %)))
               (cond (map? x)
                     (loop [entries
                            (seq x)

                            result
                            (transient {})]

                       (cond (nil? entries) (persistent! result)
                             (not (pos? (long @remaining)))
                             (do (vreset! truncated? true)
                                 (persistent! (assoc! result omitted "omitted")))
                             :else (let [[k v] (first entries)]
                                     (recur (next entries)
                                            (assoc! result
                                                    k
                                                    (if (secret-key? k) "[REDACTED]" (visit v)))))))
                     (or (vector? x) (set? x) (sequential? x))
                     (loop [items
                            (seq x)

                            result
                            (transient [])]

                       (cond (nil? items) (persistent! result)
                             (not (pos? (long @remaining)))
                             (do (vreset! truncated? true) (persistent! (conj! result omitted)))
                             :else (recur (next items) (conj! result (visit (first items))))))
                     (and (string? x) (str/starts-with? x "vis-secret:")) "[SECRET HANDLE]"
                     (string? x) (bounded-text x max-detail-bytes)
                     :else x))))]
      {:value (visit value) :is-truncated @truncated?})))

(defn- bounded-rendered
  [rendered limit]
  (let [bounded (bounded-text rendered limit)]
    {:text bounded :is-truncated (not= rendered bounded)}))

(defn- bounded-summary
  [value limit]
  (let [{:keys [value is-truncated]}
        (bounded-redact-result value)

        bounded
        (bounded-rendered (pr-str value) limit)]

    (update bounded :is-truncated #(or % is-truncated))))

(defn safe-summary [value] (:text (bounded-summary value max-summary-bytes)))

(defn context
  "Create one concurrency-safe event context for an evaluation/form anchor."
  [{:keys [evaluation-id form-index] :or {evaluation-id (str (random-uuid)) form-index 0}}]
  {:evaluation-id (str evaluation-id)
   :form-index (long form-index)
   :invocation-counter (AtomicLong. 0)
   :event-counter (AtomicLong. 0)})

(defn invocation
  "Allocate stable identity and wrapper-entry sequence from `ctx`."
  [ctx parent-id]
  (let [sequence (.incrementAndGet ^AtomicLong (:invocation-counter ctx))]
    (cond-> {:invocation-id (str (UUID/randomUUID)) :invocation-sequence sequence}
      parent-id
      (assoc :parent-invocation-id (str parent-id)))))

(defn- valid-id? [x] (try (UUID/fromString (str x)) true (catch Throwable _ false)))

(defn event-error
  "Nil for a valid v1 event, otherwise the lifecycle contract violation."
  [event]
  (let [required
        [:schema-version :evaluation-id :invocation-id :form-index :invocation-sequence
         :event-sequence :operation :presenter :phase :observed-at]

        terminal?
        (= :terminal (:phase event))

        outcomes
        (select-keys event [:succeeded :failed :cancelled])]

    (cond (not= schema-version (:schema-version event)) "unknown schema version"
          (some #(not (contains? event %)) required) "missing required event field"
          (not (valid-id? (:evaluation-id event))) "malformed evaluation id"
          (not (valid-id? (:invocation-id event))) "malformed invocation id"
          (and (:parent-invocation-id event) (not (valid-id? (:parent-invocation-id event))))
          "malformed parent invocation id"
          (not (contains? #{:start :terminal} (:phase event))) "unknown lifecycle phase"
          (and terminal? (not= 1 (count outcomes))) "terminal must have exactly one outcome"
          (and (not terminal?) (seq outcomes)) "start cannot carry an outcome"
          (and terminal? (not (number? (:duration-ms event)))) "terminal requires duration"
          (> (utf8-bytes (wire/json-str event)) (long max-event-bytes)) "event exceeds 16 KiB"
          :else nil)))

(defn checked
  [event]
  (if-let [reason (event-error event)]
    (throw (ex-info (str "Invalid Activity event: " reason)
                    {:type :activity/invalid-event :reason reason :event event}))
    event))

(defn collector
  "Create a strict lifecycle collector. `accept!` returns each accepted event."
  []
  (atom {:starts #{} :terminals #{}}))

(defn accept!
  "Validate and append one event, rejecting duplicate or orphan lifecycle edges."
  [state event]
  (let [event
        (checked event)

        id
        (:invocation-id event)]

    (swap! state (fn [{:keys [starts terminals] :as current}]
                   (case (:phase event)
                     :start
                     (do (when (starts id)
                           (throw (ex-info "Duplicate Activity start"
                                           {:type :activity/duplicate-start :invocation-id id})))
                         (-> current
                             (update :starts conj id)))

                     :terminal
                     (do (when-not (starts id)
                           (throw (ex-info "Activity terminal without start"
                                           {:type :activity/orphan-terminal :invocation-id id})))
                         (when (terminals id)
                           (throw (ex-info "Duplicate Activity terminal"
                                           {:type :activity/duplicate-terminal :invocation-id id})))
                         (-> current
                             (update :terminals conj id))))))
    event))

(defn- base-event
  [ctx invocation operation presenter phase]
  {:schema-version schema-version
   :evaluation-id (:evaluation-id ctx)
   :invocation-id (:invocation-id invocation)
   :form-index (:form-index ctx)
   :invocation-sequence (:invocation-sequence invocation)
   :event-sequence (.incrementAndGet ^AtomicLong (:event-counter ctx))
   :operation operation
   :presenter presenter
   :phase phase
   :observed-at (System/currentTimeMillis)})

(defn- map-value [m k] (when (map? m) (or (get m k) (get m (name k)))))

(defn- explicit-group-token
  [value]
  (let [token (or (map-value value :activity/group-token)
                  (get-in value [:metadata :activity/group-token])
                  (get-in value ["metadata" "activity/group-token"]))]
    (when (some? token) (bounded-text token max-summary-bytes))))

(defn- declared-resources
  [result]
  (let [value (when (map? result)
                (or (:activity/resources result)
                    (get result "activity/resources")
                    (get-in result [:metadata :activity/resources])
                    (get-in result ["metadata" "activity/resources"])))]
    (when (sequential? value)
      (keep (fn [resource]
              (let [type (or (:type resource) (get resource "type"))
                    id (or (:id resource) (get resource "id"))]

                (when (and type id) {:type type :id (bounded-text id max-summary-bytes)})))
            value))))

(defn- shell-presenter?
  [operation declared]
  (= :shell (presenter/presenter-for operation declared)))

(defn- follow-up-shell-operation?
  [operation]
  (boolean (re-find #"shell[_./-](?:logs|wait|type|stop|send)$"
                    (str/lower-case (if-let [ns (namespace operation)]
                                      (str ns "/" (name operation))
                                      (name operation))))))

(defn- shell-id-from-args
  [operation args]
  (when (follow-up-shell-operation? operation)
    (let [first-arg
          (first args)

          id
          (if (map? first-arg) (map-value first-arg :id) first-arg)]

      (when (some? id) (bounded-text id max-summary-bytes)))))

(defn- resource-refs
  [{:keys [operation presenter args result]}]
  (let [shell-id
        (when (shell-presenter? operation presenter)
          (or (when (map? result) (map-value result :id)) (shell-id-from-args operation args)))

        refs
        (concat (declared-resources result)
                (when shell-id
                  [{:type :shell-handle :id (bounded-text shell-id max-summary-bytes)}]))]

    (->> refs
         distinct
         (take max-resources)
         vec
         not-empty)))

(defn start-event
  [ctx invocation
   {:keys [operation presenter extension symbol label phrase args classification group-token]
    :as details}]
  (let [refs
        (resource-refs details)

        token
        (or group-token (some explicit-group-token args))

        argument
        (bounded-summary args max-summary-bytes)]

    (checked
      (cond-> (merge (base-event ctx invocation operation presenter :start)
                     (select-keys invocation [:parent-invocation-id])
                     {:status :running :argument-summary (:text argument)})
        extension
        (assoc :extension extension)

        symbol
        (assoc :symbol symbol)

        label
        (assoc :label (bounded-text label max-summary-bytes))

        phrase
        (assoc :phrase (bounded-text phrase max-summary-bytes))

        classification
        (assoc :classification classification)

        token
        (assoc :group-token (bounded-text token max-summary-bytes))

        refs
        (assoc :resources refs)

        (:is-truncated argument)
        (assoc :argument-truncated true)))))

(defn terminal-event
  [ctx invocation
   {:keys [operation presenter started-at-ms outcome result error classification group-token]
    :as details}]
  (let [duration
        (max 0 (- (System/currentTimeMillis) (long started-at-ms)))

        error*
        (redact error)

        details*
        (assoc details
          :result result
          :error error*)

        refs
        (resource-refs details*)

        token
        (or group-token (explicit-group-token result))

        summary
        (if (= outcome :succeeded)
          (bounded-summary result max-detail-bytes)
          (bounded-rendered (or (some-> error*
                                        ex-message)
                                (str error*))
                            max-detail-bytes))]

    (checked
      (cond-> (merge (base-event ctx invocation operation presenter :terminal)
                     (select-keys invocation [:parent-invocation-id])
                     {outcome true
                      :status (case outcome
                                :succeeded
                                :succeeded

                                :cancelled
                                :cancelled

                                :failed)
                      :duration-ms duration})
        (= outcome :succeeded)
        (assoc :result-summary (:text summary))

        (not= outcome :succeeded)
        (assoc :error-summary (:text summary))

        classification
        (assoc :classification classification)

        token
        (assoc :group-token (bounded-text token max-summary-bytes))

        refs
        (assoc :resources refs)

        (:is-truncated summary)
        (assoc :result-truncated true)))))
