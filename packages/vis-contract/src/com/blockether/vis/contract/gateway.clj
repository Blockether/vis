(ns com.blockether.vis.contract.gateway
  "Gateway declarations loaded from the validated JSON contract."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]))

(def ^:private source (delay (document/load! "gateway")))
(def ^:private engine-source (delay (wire/->engine @source)))

(def version "Gateway contract document version." (get @source "version"))
(def protocol "Canonical gateway compatibility numbers." (:protocol @engine-source))
(def protocol-version "Wire protocol spoken by this contract." (:version protocol))
(def minimum-client-protocol
  "Oldest client protocol served by this gateway contract."
  (:minimum-client protocol))
(def minimum-gateway-protocol
  "Oldest gateway protocol accepted by this client contract."
  (:minimum-gateway protocol))
(def headers "Semantic header key to canonical lower-case spelling." (:headers @engine-source))

(def route-table
  "Complete built-in gateway route table, one record per path."
  (mapv (fn [{:keys [path audience operations]}]
          {:path path
           :audience (keyword audience)
           :operations (into {}
                             (map (fn [[method operation]]
                                    [method (update-vals operation keyword)]))
                             operations)})
        (:routes @engine-source)))
(def route-operations
  "Complete `[method path]` to request/response transport declaration."
  (into {}
        (mapcat (fn [{:keys [path operations]}]
                  (map (fn [[method operation]]
                         [[method path] operation])
                       operations))
                route-table)))

(defn operation
  "Request/response transport declaration for built-in `method` and `path`, or nil."
  [method path]
  (get route-operations [method path]))
(def session-event-types
  "Closed built-in vocabulary carried by the session journal and multiplexed session SSE stream."
  (set (get-in @source ["events" "session"])))
(def job-events
  "Directional event names carried by dedicated speech job streams."
  (get-in @engine-source [:events :jobs]))
(def job-event-types "All dedicated job-stream event names." (set (vals job-events)))
(def voice-job-event "Transcription job stream event name." (:transcribe job-events))
(def speech-job-event "Speech synthesis job stream event name." (:synthesize job-events))
(def push-event-types
  "Event names used by relay push payloads."
  (set (get-in @source ["events" "push"])))
(def turn-terminal-event-types
  "Every built-in event type that ends a turn."
  (set (get-in @source ["events" "turn_terminal"])))
(def queue-mirror-event-types
  "Queue lifecycle events mirrored by attached channels."
  (set (get-in @source ["events" "queue_mirror"])))
(def view-events
  "Open, patch and close event names for both View kinds."
  (get-in @engine-source [:events :view]))
(def view-open-event "Session event that mounts either View kind." (:open view-events))
(def view-patch-event "Session event carrying accepted View operations." (:patch view-events))
(def view-close-event "Session event that ends either View kind." (:close view-events))
(def envelopes "Canonical gateway envelope declarations." (:envelopes @engine-source))
(def handshake-envelope "Gateway identity handshake declaration." (:handshake envelopes))
(def handshake-keys "Semantic handshake key to canonical wire spelling." (:keys handshake-envelope))
(def error-response-envelope "Shared JSON error response declaration." (:error-response envelopes))
(def error-response-body-keys
  "Semantic error body key to canonical wire spelling."
  (:body-keys error-response-envelope))
(def error-response-error-keys
  "Semantic error detail key to canonical wire spelling."
  (:error-keys error-response-envelope))
(def session-event-envelope "Stamped session stream event declaration." (:session-event envelopes))
(def session-event-schema
  "Schema number stamped onto every session event."
  (:schema session-event-envelope))
(def session-event-keys
  "Semantic session stamp key to canonical wire spelling."
  (:stamp-keys session-event-envelope))
(def journal-line-envelope
  "Private cross-process journal metadata declaration."
  (:journal-line envelopes))
(def journal-metadata-keys
  "Semantic journal metadata key to canonical wire spelling."
  (:metadata-keys journal-line-envelope))
(def journal-pid-key (:pid journal-metadata-keys))
(def journal-producer-key (:producer journal-metadata-keys))
(def journal-store-key (:store journal-metadata-keys))
(def subscription-ready-envelope
  "First-frame session subscription declaration."
  (:subscription-ready envelopes))
(def subscription-ready-required-keys (:required-keys subscription-ready-envelope))
(def subscription-ready-optional-keys (:optional-keys subscription-ready-envelope))
(def turn-meta-keys
  "Wire keys copied from a settled turn row into blocking submit/attach results."
  (get-in envelopes [:settled-turn :meta-keys]))

(defn handshake
  "Build the engine handshake from contract protocol numbers and runtime release identity."
  [{:keys [version build]}]
  {:protocol protocol-version
   :min-client minimum-client-protocol
   :min-gateway minimum-gateway-protocol
   :version version
   :build build})

(defn error-body
  "Build the canonical JSON error body; caller extras retain their existing override semantics."
  [type message extra]
  {(get error-response-body-keys :error) (merge {(get error-response-error-keys :type) (name type)
                                                 (get error-response-error-keys :message) message}
                                                (wire/->wire extra))})

(defn stamp-session-event
  "Apply the contract-owned identity stamp after `payload`, so payload keys cannot spoof it."
  [payload session-id sequence timestamp type]
  (assoc payload
    (:schema session-event-keys) session-event-schema
    (:sequence session-event-keys) sequence
    (:timestamp session-event-keys) timestamp
    (:session-id session-event-keys) (str session-id)
    (:type session-event-keys) type))

(defn stamp-journal-line
  "Add private producer metadata to one already-stamped session event."
  [event producer pid store?]
  (assoc event
    journal-producer-key producer
    journal-pid-key pid
    journal-store-key (boolean store?)))

(defn journal-producer [event] (get event journal-producer-key))
(defn journal-pid [event] (get event journal-pid-key))
(defn journal-stored? [event] (boolean (get event journal-store-key)))
(defn strip-journal-metadata
  "Remove private journal metadata before an event reaches a session consumer."
  [event]
  (apply dissoc event (vals journal-metadata-keys)))

(defn subscription-ready-event
  "Build the canonical first frame for one session subscription."
  [{:keys [session-id cursor current-turn-id is-live server-time-ms latest-iteration]}]
  (let [required
        subscription-ready-required-keys

        optional
        subscription-ready-optional-keys]

    (cond-> {(get required :type) (:event subscription-ready-envelope)
             (get required :session-id) (str session-id)
             (get required :cursor) cursor
             (get required :current-turn-id) (some-> current-turn-id
                                                     str)
             (get required :is-live) (boolean is-live)
             (get required :server-time-ms) server-time-ms}
      (some? latest-iteration)
      (assoc (get optional :latest-iteration) latest-iteration))))
(def replay "Cursor and generation anchors for session replay." (:replay @engine-source))
(def event-types
  "All event names on session, dedicated-job and relay-push streams."
  (into session-event-types (concat job-event-types push-event-types)))

(defn header "Canonical lower-case spelling for semantic header `k`, or nil." [k] (get headers k))

(defn route-methods
  "Exact built-in `[method path]` pairs declared by the contract."
  []
  (set (keys route-operations)))

(defn package-document "The validated language-neutral gateway document." [] @source)

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
  {:protocol (->protocol-number (get m (:protocol handshake-keys)))
   :min-client (->protocol-number (get m (:min-client handshake-keys)))
   :min-gateway (->protocol-number (get m (:min-gateway handshake-keys)))
   :version (some-> (get m (:version handshake-keys))
                    str
                    not-empty)
   :build (some-> (get m (:build handshake-keys))
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
