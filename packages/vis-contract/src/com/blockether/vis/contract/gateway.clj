(ns com.blockether.vis.contract.gateway
  "Gateway payload schemas and their HTTP transport metadata."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]))

(def ^:private source (delay (document/schema-document "gateway")))

(def protocol-version
  "Wire protocol spoken by this schema."
  (get-in @source ["$defs" "handshake" "properties" "protocol" "const"]))

(def minimum-client-protocol
  "Oldest client protocol served by this gateway."
  (get-in @source ["$defs" "handshake" "properties" "min_client" "const"]))

(def minimum-gateway-protocol
  "Oldest gateway protocol accepted by this client."
  (get-in @source ["$defs" "handshake" "properties" "min_gateway" "const"]))

(def client-lease
  "Remote lease lifetime and keepalive policy."
  (wire/->engine (get @source "x-vis-client-lease")))

(def route-table
  "Complete built-in gateway route table, one record per path."
  (mapv (fn [{:keys [path audience operations]}]
          {:path path
           :audience (keyword audience)
           :operations (into {}
                             (map (fn [[method operation]]
                                    [method (update-vals operation keyword)]))
                             operations)})
        (wire/->engine (get @source "x-vis-routes"))))

(def route-operations
  "Complete `[method path]` to request/response transport declaration."
  (into {}
        (mapcat (fn [{:keys [path operations]}]
                  (map (fn [[method operation]]
                         [[method path] operation])
                       operations)))
        route-table))

(defn operation
  "Request/response transport declaration for built-in `method` and `path`, or nil."
  [method path]
  (get route-operations [method path]))

(def session-goal-labels
  "Display titles of the canonical goal status schema alternatives."
  (into {}
        (map (juxt #(get % "const") #(get % "title")))
        (get-in @source ["$defs" "session_goal" "properties" "status" "oneOf"])))

(defn valid-session-goal?
  "True for a canonical wire goal. A session without a goal carries nil instead."
  [goal]
  (document/valid-json? "gateway" "session_goal" goal))

(defn newer-session-goal
  "Keep the highest valid session-wide revision across HTTP snapshots and event replay.
   Goal replacement increments revision too; invalid, absent and older snapshots cannot rewind it."
  [previous incoming]
  (let [previous (when (valid-session-goal? previous) previous)]
    (if (and (valid-session-goal? incoming)
             (> (long (get incoming "revision")) (long (get previous "revision" 0))))
      incoming
      previous)))

(def session-group-colors
  "Closed palette tokens a session group may be painted with, in
   the order a picker offers them. A channel maps a token to its own theme, so a
   group reads the same in the TUI, the companion app and the web UI."
  (vec (get-in @source ["$defs" "session_group_color" "enum"])))

(def default-session-group-color
  "Palette token a group takes when the client names none."
  (first session-group-colors))

(defn session-group-color?
  "True when `color` is one of the closed group palette tokens."
  [color]
  (contains? (set session-group-colors) color))

(def ^:private session-event-variants (get-in @source ["$defs" "session_event_type" "oneOf"]))

(def session-event-types
  "Closed built-in vocabulary carried by the session journal and multiplexed SSE stream."
  (set (map #(get % "const") session-event-variants)))

(def job-events
  "Directional event names carried by dedicated speech job streams."
  (into {}
        (map (juxt #(keyword (get % "x-vis-direction")) #(get % "const")))
        (get-in @source ["$defs" "job_event_type" "oneOf"])))

(def job-event-types "All dedicated job-stream event names." (set (vals job-events)))

(def voice-job-event "Transcription job stream event name." (:transcribe job-events))

(def speech-job-event "Speech synthesis job stream event name." (:synthesize job-events))

(def push-event-types
  "Event names used by relay push payloads."
  (set (get-in @source ["$defs" "push_event_type" "enum"])))

(def turn-terminal-event-types
  "Every built-in event type that ends a turn."
  (into #{}
        (comp (filter #(get % "x-vis-turn-terminal")) (map #(get % "const")))
        session-event-variants))

(def queue-mirror-event-types
  "Queue lifecycle events mirrored by attached channels."
  (into #{}
        (comp (filter #(get % "x-vis-queue-mirror")) (map #(get % "const")))
        session-event-variants))

(def view-events
  "Open, patch and close event names for both View kinds."
  (into {}
        (keep (fn [event]
                (when (str/starts-with? event "view.") [(keyword (subs event 5)) event])))
        session-event-types))

(def view-open-event "Session event that mounts either View kind." (:open view-events))

(def view-patch-event "Session event carrying accepted View operations." (:patch view-events))

(def view-close-event "Session event that ends either View kind." (:close view-events))

(def session-event-schema
  "Schema number stamped onto every session event."
  (get-in @source ["$defs" "session_event" "properties" "schema" "const"]))

(def turn-meta-keys
  "Wire fields copied from a settled turn into blocking submit/attach results."
  (vec (keys (get-in @source ["$defs" "turn_metadata" "properties"]))))

(defn handshake
  "Build the engine handshake from schema protocol numbers and runtime release identity."
  [{:keys [version build]}]
  {:protocol protocol-version
   :min-client minimum-client-protocol
   :min-gateway minimum-gateway-protocol
   :version version
   :build build})

(defn error-body
  "Build the canonical JSON error body; caller extras retain their existing override semantics."
  [type message extra]
  {"error" (merge {"type" (name type) "message" message} (wire/->wire extra))})

(defn stamp-session-event
  "Apply the schema identity stamp after `payload`, so payload keys cannot spoof it."
  [payload session-id sequence timestamp type]
  (assoc payload
    "schema" session-event-schema
    "seq" sequence
    "ts" timestamp
    "session_id" (str session-id)
    "type" type))

(defn stamp-journal-line
  "Add private producer metadata to one already-stamped session event."
  [event producer pid store?]
  (assoc event
    "_producer" producer
    "_pid" pid
    "_store" (boolean store?)))

(defn journal-producer [event] (get event "_producer"))

(defn journal-pid [event] (get event "_pid"))

(defn journal-stored? [event] (boolean (get event "_store")))

(defn strip-journal-metadata
  "Remove private journal metadata before an event reaches a session consumer."
  [event]
  (apply dissoc event (keys (get-in @source ["$defs" "journal_metadata" "properties"]))))

(defn subscription-ready-event
  "Build the canonical first frame for one session subscription."
  [{:keys [session-id cursor current-turn-id is-live server-time-ms latest-iteration goal
           agent-name]}]
  (cond-> {"type" (get-in @source ["$defs" "subscription_ready" "properties" "type" "const"])
           "session_id" (str session-id)
           "cursor" cursor
           "current_turn_id" (some-> current-turn-id
                                     str)
           "is_live" (boolean is-live)
           "server_time_ms" server-time-ms
           "goal" goal}
    (some? agent-name)
    (assoc "agent_name" agent-name)

    (some? latest-iteration)
    (assoc "latest_iteration" latest-iteration)))

(def event-types
  "All event names on session, dedicated-job and relay-push streams."
  (into session-event-types (concat job-event-types push-event-types)))

(defn route-methods
  "Exact built-in `[method path]` pairs declared by the schema's HTTP metadata."
  []
  (set (keys route-operations)))

(defn session-event-type?
  "True when `event-type` belongs to the closed session-stream vocabulary."
  [event-type]
  (contains? session-event-types event-type))

(defn- ->protocol-number
  [x]
  (cond (integer? x) (long x)
        (number? x) (long x)
        (string? x) (try (Long/parseLong (str/trim x)) (catch Exception _ nil))
        :else nil))

(defn wire->handshake
  "Read a peer's advertised handshake from its canonical string-keyed wire map.
   Missing fields remain nil so [[verdict]] rejects an unversioned peer explicitly."
  [m]
  {:protocol (->protocol-number (get m "protocol"))
   :min-client (->protocol-number (get m "min_client"))
   :min-gateway (->protocol-number (get m "min_gateway"))
   :version (some-> (get m "version")
                    str
                    not-empty)
   :build (some-> (get m "build")
                  str
                  not-empty)})

(defn verdict
  "Pure compatibility verdict between a gateway and a client.

   Reasons are `ok`, `client-too-old`, `gateway-too-old`, or `unknown` when a peer
   did not advertise a protocol. `:upgrade` names the half that must be updated."
  [{:keys [gateway-protocol gateway-min-client gateway-version client-protocol client-min-gateway
           client-version client-name]}]
  (let [gp
        (->protocol-number gateway-protocol)

        cp
        (->protocol-number client-protocol)

        gmin
        (or (->protocol-number gateway-min-client) gp)

        cmin
        (or (->protocol-number client-min-gateway) cp)

        reason
        (cond (or (nil? gp) (nil? cp)) "unknown"
              (< (long cp) (long gmin)) "client-too-old"
              (< (long gp) (long cmin)) "gateway-too-old"
              :else "ok")]

    {:is-compatible (= "ok" reason)
     :reason reason
     :upgrade (case reason
                "client-too-old"
                "client"

                "gateway-too-old"
                "gateway"

                "unknown"
                (cond (nil? cp) "client"
                      (nil? gp) "gateway"
                      :else nil)

                nil)
     :gateway-protocol gp
     :gateway-min-client gmin
     :gateway-version gateway-version
     :client-protocol cp
     :client-min-gateway cmin
     :client-version client-version
     :client-name (or client-name "client")}))
