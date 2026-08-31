(ns com.blockether.vis.contract.gateway
  "Canonical gateway declarations shared by Core, the server and every language SDK.

   `vis-contract/gateway.edn` is the source: the complete built-in method/path table,
   protocol numbers and headers, built-in event vocabularies, terminal/queue semantics,
   and cursor replay anchors. This namespace reads and validates those declarations;
   it performs no transport, lifecycle or daemon work."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def ^:private http-methods #{:delete :get :patch :post :put})
(def ^:private audiences #{:administration :public :sdk})

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- closed-map? [m expected-keys] (and (map? m) (= expected-keys (set (keys m)))))
(defn- event-set? [x] (and (set? x) (seq x) (every? non-blank-string? x)))

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
  [{:contract/keys [version] :gateway/keys [protocol headers routes events replay] :as document}]
  (let [{:keys [session jobs push turn-terminal queue-mirror view]} events]
    (and (closed-map? document
                      #{:contract/version :gateway/protocol :gateway/headers :gateway/routes
                        :gateway/events :gateway/replay})
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
         (every? event-set? [session jobs push turn-terminal queue-mirror])
         (every? session turn-terminal)
         (every? session queue-mirror)
         (closed-map? view #{:open :patch :close})
         (every? session (vals view))
         (closed-map? replay #{:cursor-header :cursor-key :ready-event :generation-start-event})
         (contains? headers (:cursor-header replay))
         (non-blank-string? (:cursor-key replay))
         (contains? session (:ready-event replay))
         (contains? session (:generation-start-event replay)))))

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
(def job-event-types
  "Event names used by dedicated speech and voice job streams."
  (get-in @document [:gateway/events :jobs]))
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
