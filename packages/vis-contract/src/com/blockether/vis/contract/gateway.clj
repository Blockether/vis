(ns com.blockether.vis.contract.gateway
  "Canonical gateway declarations shared by Core, the server and every language SDK.

   `vis-contract/gateway.edn` is the source: the complete built-in method/path table,
   protocol numbers and headers, built-in event vocabularies, terminal/queue semantics,
   and cursor replay anchors. This namespace reads and validates those declarations;
   it performs no transport, lifecycle or daemon work."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]))

(def ^:private http-methods #{:delete :get :patch :post :put})
(def ^:private audiences #{:administration :public :sdk})

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- closed-map? [m expected-keys] (and (map? m) (= expected-keys (set (keys m)))))
(defn- event-set? [x] (and (set? x) (seq x) (every? non-blank-string? x)))
(defn- wire-key-map?
  [m expected-keys]
  (and (closed-map? m expected-keys)
       (every? non-blank-string? (vals m))
       (= (count m) (count (set (vals m))))))

(defn- valid-route?
  [{:keys [path methods audience] :as route}]
  (and (closed-map? route #{:path :methods :audience})
       (non-blank-string? path)
       (str/starts-with? path "/")
       (set? methods)
       (seq methods)
       (every? http-methods methods)
       (contains? audiences audience)))

(defn- valid-document?
  [{:contract/keys [version]
    :gateway/keys [protocol headers routes events envelopes replay]
    :as document}]
  (let [{:keys [session jobs push turn-terminal queue-mirror view]}
        events

        {:keys [session-event journal-line subscription-ready settled-turn]}
        envelopes]

    (and (closed-map? document
                      #{:contract/version :gateway/protocol :gateway/headers :gateway/routes
                        :gateway/events :gateway/envelopes :gateway/replay})
         (pos-int? version)
         (closed-map? protocol #{:version :minimum-client :minimum-gateway})
         (every? pos-int? (vals protocol))
         (map? headers)
         (seq headers)
         (every? keyword? (keys headers))
         (every? non-blank-string? (vals headers))
         (= (count headers) (count (set (vals headers))))
         (vector? routes)
         (seq routes)
         (every? valid-route? routes)
         (= (count routes) (count (set (map :path routes))))
         (closed-map? events #{:session :jobs :push :turn-terminal :queue-mirror :view})
         (closed-map? jobs #{:transcribe :synthesize})
         (every? non-blank-string? (vals jobs))
         (every? event-set? [session push turn-terminal queue-mirror])
         (every? session turn-terminal)
         (every? session queue-mirror)
         (closed-map? view #{:open :patch :close})
         (every? session (vals view))
         (closed-map? envelopes #{:session-event :journal-line :subscription-ready :settled-turn})
         (closed-map? session-event #{:schema :stamp-keys})
         (pos-int? (:schema session-event))
         (wire-key-map? (:stamp-keys session-event)
                        #{:schema :sequence :session-id :timestamp :type})
         (closed-map? journal-line #{:metadata-keys})
         (wire-key-map? (:metadata-keys journal-line) #{:pid :producer :store})
         (closed-map? subscription-ready #{:event :required-keys :optional-keys})
         (contains? session (:event subscription-ready))
         (wire-key-map? (:required-keys subscription-ready)
                        #{:cursor :current-turn-id :is-live :server-time-ms :session-id :type})
         (wire-key-map? (:optional-keys subscription-ready) #{:latest-iteration})
         (not-any? (set (vals (:required-keys subscription-ready)))
                   (vals (:optional-keys subscription-ready)))
         (closed-map? settled-turn #{:meta-keys})
         (vector? (:meta-keys settled-turn))
         (seq (:meta-keys settled-turn))
         (every? non-blank-string? (:meta-keys settled-turn))
         (= (count (:meta-keys settled-turn)) (count (set (:meta-keys settled-turn))))
         (closed-map? replay #{:cursor-header :cursor-key :ready-event :generation-start-event})
         (contains? headers (:cursor-header replay))
         (non-blank-string? (:cursor-key replay))
         (contains? session (:ready-event replay))
         (contains? session (:generation-start-event replay))
         (= (:event subscription-ready) (:ready-event replay)))))

(s/def :contract/gateway valid-document?)

(def ^:private resource-path "vis-contract/gateway.edn")

(def ^:private document
  (delay
    (let [resource
          (io/resource resource-path)

          _
          (when-not resource
            (throw (ex-info (str "the gateway contract is missing from the classpath: "
                                 resource-path)
                            {:type :vis/contract-missing :resource resource-path})))

          parsed
          (edn/read-string (slurp resource))]

      (when-not (s/valid? :contract/gateway parsed)
        (throw (ex-info (str resource-path " is not a valid gateway contract")
                        {:type :vis/contract-invalid
                         :resource resource-path
                         :explain (s/explain-str :contract/gateway parsed)})))
      parsed)))

(def version "Gateway contract document version." (:contract/version @document))
(def protocol "Canonical gateway compatibility numbers." (:gateway/protocol @document))
(def protocol-version "Wire protocol spoken by this contract." (:version protocol))
(def minimum-client-protocol
  "Oldest client protocol served by this gateway contract."
  (:minimum-client protocol))
(def minimum-gateway-protocol
  "Oldest gateway protocol accepted by this client contract."
  (:minimum-gateway protocol))
(def headers "Semantic header key to canonical lower-case spelling." (:gateway/headers @document))
(def route-table
  "Complete built-in gateway route table, one record per path."
  (:gateway/routes @document))
(def session-event-types
  "Closed built-in vocabulary carried by the session journal and multiplexed session SSE stream."
  (get-in @document [:gateway/events :session]))
(def job-events
  "Directional event names carried by dedicated speech and voice job streams."
  (get-in @document [:gateway/events :jobs]))
(def job-event-types "All dedicated job-stream event names." (set (vals job-events)))
(def voice-job-event "Transcription job stream event name." (:transcribe job-events))
(def speech-job-event "Speech synthesis job stream event name." (:synthesize job-events))
(def push-event-types
  "Event names used by relay push payloads."
  (get-in @document [:gateway/events :push]))
(def turn-terminal-event-types
  "Every built-in event type that ENDS a turn for every blocking reader.
   `turn.cancelled` is terminal: a user stop or stall force-cancel lands exactly like
   completion/failure, so omitting it leaves the reader and its live stream parked."
  (get-in @document [:gateway/events :turn-terminal]))
(def queue-mirror-event-types
  "Queue lifecycle events attached channels mirror for a DIFFERENT turn of the same
   session. `turn.queued.drained` removes the head when Core starts it; pause/resume
   events keep every sibling channel on the same held-backlog state."
  (get-in @document [:gateway/events :queue-mirror]))
(def view-events
  "Open, patch and close event names for both View kinds."
  (get-in @document [:gateway/events :view]))
(def view-open-event "Session event that mounts either View kind." (:open view-events))
(def view-patch-event "Session event carrying accepted View operations." (:patch view-events))
(def view-close-event "Session event that ends either View kind." (:close view-events))
(def envelopes "Canonical gateway envelope declarations." (:gateway/envelopes @document))
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
(def replay "Cursor and generation anchors for session replay." (:gateway/replay @document))
(def event-types
  "All event names on session, dedicated-job and relay-push streams."
  (into session-event-types (concat job-event-types push-event-types)))

(defn header "Canonical lower-case spelling for semantic header `k`, or nil." [k] (get headers k))

(defn route-methods
  "Exact built-in `[method path]` pairs declared by the contract."
  []
  (into #{}
        (mapcat (fn [{:keys [path methods]}]
                  (map #(vector % path) methods))
                route-table)))

(defn- ->package-data
  [value]
  (cond (map? value) (into (sorted-map)
                           (map (fn [[k v]]
                                  [(wire/wire-key k) (->package-data v)]))
                           value)
        (set? value) (->> value
                          (map ->package-data)
                          (sort-by pr-str)
                          vec)
        (sequential? value) (mapv ->package-data value)
        :else (wire/->wire value)))

(defn package-document
  "Deterministic string-keyed gateway data for generated Python and JavaScript inputs.
   Sets become sorted vectors; route order remains the owning EDN's order."
  []
  (array-map "version" version
             "protocol" (->package-data protocol)
             "headers" (->package-data headers)
             "routes" (->package-data route-table)
             "events" (->package-data (:gateway/events @document))
             "envelopes" (->package-data envelopes)
             "replay" (->package-data replay)))

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
