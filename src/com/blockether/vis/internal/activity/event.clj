(ns com.blockether.vis.internal.activity.event
  "Bounded, ownerless lifecycle events for host-observed tool invocations.

   Events are presentation input, never operation control. Construction redacts
   before measuring, collectors reject broken lifecycle order, and a sink failure
   must never change the value or exception the Python caller observes."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util])
  (:import [java.nio.charset StandardCharsets]
           [java.util UUID]
           [java.util.concurrent.atomic AtomicLong]))

(def max-event-bytes (* 256 1024))

(def max-summary-bytes 512)

(def max-detail-bytes (* 2 1024))

(def max-resources 8)

(def max-diff-line-bytes 512)

(def ^:private max-summary-nodes 128)

(def ^:private secret-key?
  #(boolean (re-find #"(?i)(password|passwd|secret|token|authorization|cookie|otp|api[_-]?key)"
                     (name %))))

(defn utf8-bytes ^long [x] (long (alength (.getBytes (str x) StandardCharsets/UTF_8))))

(defn bounded-text
  "UTF-8 truncate text to at most `limit` bytes, appending an omission marker.

   ONE cut, on a code-point boundary, over a prefix of at most `limit`
   CHARACTERS — UTF-8 spends at least one byte per character, so nothing past
   that prefix can survive the limit anyway. The walk this replaced moved the cut
   down ONE character at a time, re-`subs`ing and re-encoding the WHOLE string on
   every pass: quadratic, and event construction runs on the CALLING thread with
   no interrupt point in it, so one tool result carrying a 3.2 MB single line (a
   minified JSON blob under `cat`/`grep`) burned an hour of CPU — the block's
   timeout fired while that thread kept running and every later block queued
   behind it."
  [value limit]
  (let [s
        (str value)

        limit
        (long limit)

        char-count
        (long (count s))]

    ;; bytes >= chars in UTF-8, so a string longer than `limit` CHARS cannot fit
    ;; and never has to be encoded whole just to find that out.
    (if (and (<= char-count limit) (<= (utf8-bytes s) limit))
      s
      (let [marker
            "…"

            room
            (long (max 0 (- limit (utf8-bytes marker))))

            ^bytes head
            (.getBytes ^String (subs s 0 (int (min char-count room))) StandardCharsets/UTF_8)

            head-len
            (long (alength head))

            ;; step back over any UTF-8 continuation byte (0b10xxxxxx) so the cut
            ;; lands between code points and never splits one into U+FFFD
            end
            (loop [i (long (min head-len room))]
              (cond (not (pos? i)) 0
                    (>= i head-len) head-len
                    (= 0x80 (bit-and 0xC0 (aget head (int i)))) (recur (dec i))
                    :else i))]

        (str (String. head 0 (int end) StandardCharsets/UTF_8) marker)))))

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

(defn context
  "Create one concurrency-safe counter context for a single block's tool calls.

   Ownerless by contract: the form this block becomes is the snapshot's only
   identity, so no evaluation, iteration or form coordinate is carried here."
  []
  {:invocation-counter (AtomicLong. 0) :event-counter (AtomicLong. 0)})

(defn invocation
  "Allocate stable identity and wrapper-entry sequence from `ctx`."
  [ctx parent-id]
  (let [sequence (.incrementAndGet ^AtomicLong (:invocation-counter ctx))]
    (cond-> {:invocation-id (str (UUID/randomUUID)) :invocation-sequence sequence}
      parent-id
      (assoc :parent-invocation-id (str parent-id)))))

(defn- valid-id? [x] (try (UUID/fromString (str x)) true (catch Throwable _ false)))

(defn event-error
  "Nil for a valid event, otherwise the lifecycle contract violation."
  [event]
  (let [required
        [:invocation-id :invocation-sequence :event-sequence :operation :presenter :phase
         :observed-at]

        terminal?
        (= :terminal (:phase event))

        outcomes
        (select-keys event [:succeeded :failed :cancelled])]

    (cond (some #(not (contains? event %)) required) "missing required event field"
          (not (valid-id? (:invocation-id event))) "malformed invocation id"
          (and (:parent-invocation-id event) (not (valid-id? (:parent-invocation-id event))))
          "malformed parent invocation id"
          (not (contains? #{:start :terminal} (:phase event))) "unknown lifecycle phase"
          (and terminal? (not= 1 (count outcomes))) "terminal must have exactly one outcome"
          (and (not terminal?) (seq outcomes)) "start cannot carry an outcome"
          (and terminal? (not (number? (:duration-ms event)))) "terminal requires duration"
          (> (utf8-bytes (wire/json-str event)) (long max-event-bytes))
          (str "event exceeds " (quot (long max-event-bytes) 1024) " KiB")
          :else nil)))

(defn checked
  [event]
  (if-let [reason (event-error event)]
    (throw (ex-info (str "Invalid Activity event: " reason)
                    {:type :activity/invalid-event :reason reason :event event}))
    event))

(defn- heaviest-diff-body
  "Index of the diff evidence whose lines weigh the most, nil when none carries any."
  [diffs]
  (->> (map-indexed vector diffs)
       (filter (fn [[_ d]]
                 (seq (:lines d))))
       (sort-by (fn [[_ d]]
                  (- (utf8-bytes (wire/json-str (:lines d))))))
       ffirst))

(defn fit-event
  "The event, shrunk to `max-event-bytes` by dropping WHOLE diff bodies, heaviest first.

   A patch is evidence, and half of one is worse than none: a reader who meets a hunk
   that stops mid-file cannot tell the cut from the change. So the ceiling that keeps a
   pathological payload out of SSE and `/poll` replay is paid in whole bodies - the file
   keeps its name, its counts and `:is-truncated` - never in a partial diff. Everything
   else an event carries is already bounded to bytes where it is built."
  [event]
  (loop [event event]
    (if (<= (utf8-bytes (wire/json-str event)) (long max-event-bytes))
      event
      (if-let [idx (heaviest-diff-body (:diff-evidence event))]
        (recur (update-in event [:diff-evidence idx] assoc :lines [] :is-truncated true))
        (dissoc event :diff-evidence)))))

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
  {:invocation-id (:invocation-id invocation)
   :invocation-sequence (:invocation-sequence invocation)
   :event-sequence (.incrementAndGet ^AtomicLong (:event-counter ctx))
   :operation operation
   :presenter presenter
   :phase phase
   :observed-at (util/now-ms)})

(defn- map-value [m k] (when (map? m) (or (get m k) (get m (name k)))))

(def ^:private sensitive-diff-text?
  #(boolean (re-find #"(?i)(password|passwd|secret|token|authorization|cookie|otp|api[_-]?key)"
                     (str %))))

(defn- diff-line
  [line]
  (let [line
        (str line)

        kind
        (cond (str/starts-with? line "@@") :hunk
              (str/starts-with? line "--- (") :header
              (str/starts-with? line "+") :addition
              (str/starts-with? line "-") :deletion
              :else :context)

        text
        (if (and (contains? #{:addition :deletion :context} kind)
                 (contains? #{\+ \- \space} (first line)))
          (subs line 1)
          line)

        redacted?
        (sensitive-diff-text? text)]

    (cond-> {:kind kind :text (if redacted? "[REDACTED]" (bounded-text text max-diff-line-bytes))}
      redacted?
      (assoc :is-redacted true))))

(defn- diff-lines
  "EVERY line of the file's diff, in order.

   A patch is read whole or not at all: a receipt that stops in the middle of a hunk
   sends the reader to find the rest somewhere else, which is the one thing the
   receipt exists to spare them. Size is answered at the transport's own edge by
   `fit-event`, which drops a WHOLE body rather than half a patch; the only clamp
   here is `diff-line`'s, on a single monstrous line, in place."
  [source]
  (mapv diff-line (str/split-lines (str source))))

(defn- file-diff-evidence
  "ONE file's diff, named by that file and carried WHOLE."
  [unit]
  (let [lines
        (diff-lines (map-value unit :diff))

        counts
        (map-value unit :lines)

        target
        (map-value unit :target)

        path
        (or (map-value target :resolved) (map-value target :requested))

        upstream-truncated?
        (some #(and (= :context (:kind %)) (str/includes? (:text %) "omitted")) lines)]

    {:kind :diff
     :text (str (or path "diff"))
     :lines lines
     :additions (long (or (map-value counts :added) 0))
     :deletions (long (or (map-value counts :removed) 0))
     :modifications (long (or (map-value counts :modified) 0))
     :is-truncated (boolean upstream-truncated?)
     :is-redacted (boolean (some :is-redacted lines))}))

(defn- result-diff-evidence
  "The diffs a row SHOWS, ONE PER FILE, for ANY result envelope carrying them. A file's diff
   is `{:diff … :lines … :target …}`: `patch` edits one file and reports that map itself, a
   code block that wrote eleven files reports eleven of them under `:diffs`, and both arrive
   as the same evidence through the same renderer. A producer earns a diff by carrying that
   vocabulary, never by being a particular tool.

   Answers a VECTOR, one whole diff per file. Nothing is divided between them here:
   the transport's ceiling is answered once, at the event's own edge, by `fit-event`."
  [{:keys [result-envelope]}]
  (let [metadata
        (map-value result-envelope :metadata)

        declared
        (map-value metadata :diffs)

        carried
        (filterv #(util/non-blank-string? (map-value % :diff))
          (if (sequential? declared) (vec declared) [metadata]))]

    (when (seq carried) (mapv file-diff-evidence carried))))

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

(def ^:private activity-declaration-keys
  "Keys a caller puts in a result to DECLARE something to Activity instead of reporting a
   value: resources to name, a token to group by. `declared-resources` and
   `explicit-group-token` are their only readers."
  [:activity/resources "activity/resources" :activity/group-token "activity/group-token"])

(defn- displayable-result
  "The part of a result a row can SHOW. A declaration is addressed to Activity, not to the
   reader, so a result made only of declarations summarizes to nothing — printing it back
   would put engine data structures where the row's own words belong."
  [result]
  (if (map? result) (not-empty (apply dissoc result activity-declaration-keys)) result))

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
   {:keys [operation presenter extension symbol label phrase args classification group-token
           group-head summary-format]
    :as details}]
  (let [refs
        (resource-refs details)

        token
        (or group-token (some explicit-group-token args))

        phrase
        (or phrase
            (when (and (shell-presenter? operation presenter)
                       (not (follow-up-shell-operation? operation))
                       (string? (first args)))
              (first args)))

        argument
        (when (seq args) (bounded-summary args max-summary-bytes))]

    (checked
      (cond-> (merge (base-event ctx invocation operation presenter :start)
                     (select-keys invocation [:parent-invocation-id])
                     {:status :running})
        argument
        (assoc :argument-summary (:text argument))

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

        group-head
        (assoc :group-head group-head)

        summary-format
        (assoc :summary-format summary-format)

        refs
        (assoc :resources refs)

        (:is-truncated argument)
        (assoc :argument-truncated true)))))

(defn terminal-event
  [ctx invocation
   {:keys [operation presenter started-at-ms outcome result error classification group-token
           result-format]
    :as details}]
  (let [duration
        (max 0 (- (util/now-ms) (long started-at-ms)))

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
          (some-> (displayable-result result)
                  (bounded-summary max-detail-bytes))
          (bounded-rendered (or (some-> error*
                                        ex-message)
                                (str error*))
                            max-detail-bytes))

        diff-evidence
        (when (= outcome :succeeded) (result-diff-evidence details))]

    (checked
      (fit-event
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
          (and (= outcome :succeeded) (:text summary))
          (assoc :result-summary (:text summary))

          (not= outcome :succeeded)
          (assoc :error-summary (:text summary))

          classification
          (assoc :classification classification)

          token
          (assoc :group-token (bounded-text token max-summary-bytes))

          result-format
          (assoc :result-format result-format)

          refs
          (assoc :resources refs)

          (seq diff-evidence)
          (assoc :diff-evidence diff-evidence)

          (:is-truncated summary)
          (assoc :result-truncated true))))))
