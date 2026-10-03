(ns com.blockether.vis.native-rooms-test
  "Owned gateways exchange Council traffic through a local or deployed Worker and D1."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io File]
           [java.net InetSocketAddress ServerSocket]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent CountDownLatch Executors TimeUnit]))

(set! *warn-on-reflection* true)

(def ^:dynamic *deployment*
  "Optional live fixture: `{:relay-url :admin-token :guest-port :guest-root :provider-port}`.
   Set it with `alter-var-root` in a REPL, then run the var with `lazytest.repl/run-test-var`.
   A namespace reload resets it. Use only owned, isolated gateways and an authenticated relay."
  nil)

(defn- request!
  [port method path body]
  (with-redefs-fn {#'gateway-client/ensure-gateway! (constantly
                                                      {:host "127.0.0.1" :port port :remote? true})
                   #'gateway-client/client-id (atom nil)
                   #'gateway-client/release-hook-installed? (atom true)}
    (fn []
      (let [r (gateway-client/request! method
                                       path
                                       (cond-> {:timeout-ms 10000}
                                         body
                                         (assoc :body body)))]
        {:status (:status r) :body (when (seq (:body r)) (wire/parse-json (:body r)))}))))

(defn- ok!
  [port method path body]
  (let [r (request! port method path body)]
    (when-not (<= 200 (:status r) 299)
      (throw (ex-info (str "Native Rooms request failed: " (name method)
                           " " path
                           " " (:status r)
                           " " (select-keys (get-in r [:body "error"]) ["type" "code" "message"]))
                      {:method method :path path :status (:status r)})))
    (:body r)))

(defn- log-tail
  "Return the last 40 lines of `file`. The suite deletes its directory, so a failure carries the log."
  [^File file]
  (if (.isFile file) (str/join "\n" (take-last 40 (str/split-lines (slurp file)))) ""))

(defn- wait-health!
  [port ^Process process]
  (loop [left 120]
    (cond (= 200 (:status (try (request! port :get "/healthz" nil) (catch Exception _ {})))) true
          (or (zero? left) (not (.isAlive process)))
          (throw (ex-info "Owned Rooms gateway failed to start" {}))
          :else (do (Thread/sleep 250) (recur (dec left))))))

(defn- setting!
  [port scope sid id value]
  (let [catalog
        (ok! port :get (str "/v1/settings?scope=" scope (when sid (str "&target_id=" sid))) nil)]
    (ok! port
         :patch
         "/v1/settings"
         (cond-> {:scope scope
                  :revision (get catalog "revision")
                  :changes [{:id id :action "value" :value value}]}
           sid
           (assoc :target_id
             sid :context_session_id
             sid)))))

(defn- provider!
  []
  (let [started
        (CountDownLatch. 2)

        release
        (CountDownLatch. 1)

        pool
        (Executors/newCachedThreadPool)

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1"
                                               (int (or (:provider-port *deployment*) 0)))
                           0)]

    (.createContext
      server
      "/"
      (reify
        HttpHandler
          (handle [_ exchange]
            (let [^HttpExchange exchange
                  exchange

                  catalog?
                  (= "GET" (.getRequestMethod exchange))

                  body
                  (slurp (.getRequestBody exchange))]

              (when-not catalog? (.countDown started) (.await release 120 TimeUnit/SECONDS))
              (let [payload
                    (if catalog?
                      "{\"data\":[]}"
                      (if (str/includes? (str/replace body " " "") "\"stream\":true")
                        (#'binary/stream-body "Rooms verified")
                        (#'binary/whole-body "Rooms verified")))

                    data
                    (.getBytes ^String payload StandardCharsets/UTF_8)]

                (.add (.getResponseHeaders exchange)
                      "Content-Type"
                      (if catalog? "application/json" "text/event-stream"))
                (.sendResponseHeaders exchange 200 (alength data))
                (with-open [out (.getResponseBody exchange)]
                  (.write out data))))
            nil)))
    (.setExecutor server pool)
    (.start server)
    {:port (.getPort (.getAddress server))
     :started started
     :stop! #(do (.countDown release) (.stop server 0) (.shutdownNow pool))}))

(defn- gateway!
  [^File dir provider-port]
  (#'binary/overlay! dir provider-port)
  (let [port
        (with-open [socket (ServerSocket. 0)]
          (.getLocalPort socket))

        pb
        (doto (ProcessBuilder. ^"[Ljava.lang.String;"
                               (into-array String
                                           [(.getAbsolutePath ^File (#'binary/require-binary))
                                            (str "-Duser.home=" (.getAbsolutePath dir))
                                            (str "-Dvis.rooms.home=" (.getAbsolutePath dir))
                                            "gateway" "start" "--host" "127.0.0.1" "--port"
                                            (str port) "--db"
                                            (.getAbsolutePath (io/file dir "sessions"))]))
          (.directory dir)
          (.redirectErrorStream true)
          (.redirectOutput (io/file dir "gateway.log")))

        environment
        (.environment pb)]

    (.putAll environment (#'binary/native-environment))
    (doseq [key ["VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "VIS_GATEWAY_MANAGED" "VIS_HOME"]]
      (.remove environment key))
    (let [process (.start pb)]
      (try (wait-health! port process)
           {:port port :process process}
           (catch Exception e
             (#'binary/kill-tree! process)
             (throw
               (ex-info (str (ex-message e) "\n" (log-tail (io/file dir "gateway.log"))) {} e)))))))

(defdescribe
  native-rooms-boundary
  (it
    "joins without sharing, exchanges a required reply and enforces parent restrictions"
    (let [^File dir
          (#'binary/temp-dir "vis-native-rooms-")

          owner-dir
          (doto (io/file dir "owner") .mkdirs)

          guest-dir
          (doto (io/file dir "guest") .mkdirs)

          processes
          (atom [])

          machines
          (atom [])

          provider
          (provider!)

          admin
          (or (:admin-token *deployment*) (apply str (repeat 43 "a")))]

      (try
        (let [relay-pb
              (doto (ProcessBuilder. ^"[Ljava.lang.String;"
                                     (into-array String ["node" "test/native-relay.mjs"]))
                (.directory (io/file "apps/vis-companion-relay"))
                (.redirectError (io/file dir "relay.log")))

              _
              (.put (.environment relay-pb) "ROOMS_ADMIN_TOKEN" admin)

              relay
              (.start relay-pb)

              _
              (swap! processes conj relay)

              relay-url
              (or (:relay-url *deployment*)
                  (with-open [reader (io/reader (.getInputStream relay))]
                    (deref (future (.readLine ^java.io.BufferedReader reader)) 60000 nil)))

              _
              (when-not (and relay-url
                             (or (:relay-url *deployment*)
                                 (str/starts-with? relay-url "http://127.0.0.1:")))
                (throw (ex-info (str "Local Rooms Worker did not start\n"
                                     (log-tail (io/file dir "relay.log")))
                                {})))

              owner
              (gateway! owner-dir (:port provider))

              _
              (swap! processes conj (:process owner))

              guest
              (if-let [port (:guest-port *deployment*)]
                {:port port}
                (gateway! guest-dir (:port provider)))

              _
              (when-let [process (:process guest)]
                (swap! processes conj process))

              a
              (:port owner)

              b
              (:port guest)

              _
              (swap! machines conj a)

              _
              (ok! a
                   :post
                   "/v1/council/rooms/register"
                   {:relay_url relay-url :name "Owner" :admin_token admin})

              room
              (ok! a :post "/v1/council/rooms" {:name "Native verification"})

              rid
              (get room "room_id")

              invite
              (ok! a :post (str "/v1/council/rooms/" rid "/invites") {})

              _
              (swap! machines conj b)

              joined
              (ok! b
                   :post
                   "/v1/council/rooms/join"
                   {:invite_url (get invite "invite_url") :machine_name "Guest"})

              sa
              (get (ok! a :post "/v1/sessions" {:channel "api" :root (.getAbsolutePath owner-dir)})
                   "id")

              sb
              (get (ok! b
                        :post
                        "/v1/sessions"
                        {:channel "api"
                         :root (or (:guest-root *deployment*) (.getAbsolutePath guest-dir))})
                   "id")

              ca
              (str "/v1/sessions/" sa "/council")

              cb
              (str "/v1/sessions/" sb "/council")]

          (expect (not= rid (get (ok! b :get cb nil) "default_group_id")))
          (setting! a "session" sa "council_room" rid)
          (setting! b "session" sb "council_room" rid)
          (ok! a
               :post
               (str "/v1/sessions/" sa "/turns")
               {:request "Wait for the native Rooms verification"})
          (ok! b
               :post
               (str "/v1/sessions/" sb "/turns")
               {:request "Wait for the native Rooms verification"})
          (expect (.await ^CountDownLatch (:started provider) 60 TimeUnit/SECONDS))
          (ok! b :get (str cb "/members") nil)
          (let [question
                (ok! a
                     :post
                     (str ca "/entries")
                     {:kind "coordination"
                      :content "Verify the room"
                      :ping [sb]
                      :reply_required true
                      :activation_id (get (ok! a :get ca nil) "activation_id")})

                eid
                (get question "entry_id")

                received
                (ok! b :get (str cb "/entries/" eid) nil)

                reply
                (ok! b
                     :post
                     (str cb "/entries")
                     {:kind "informational"
                      :content "Room verified"
                      :reply_to eid
                      :activation_id (get (ok! b :get cb nil) "activation_id")})

                answered
                (ok! a :get (str ca "/entries/" eid) nil)]

            (expect (= "Verify the room" (get received "content")))
            (expect (= "replied" (get-in answered ["replies" 0 "state"])))
            (expect (= (get reply "entry_id") (get-in answered ["replies" 0 "reply_entry_id"]))))
          (let [access (str "council_room_" (str/replace rid "-" "") "_access")]
            (setting! b "global" nil access false)
            (setting! b "session" sb access true)
            (expect (not= rid (get (ok! b :get cb nil) "default_group_id")))
            (expect (not= 200 (:status (request! b :get (str cb "/entries?group_id=" rid) nil))))
            (setting! b "global" nil access true))
          (ok! a
               :delete
               (str "/v1/council/rooms/" rid "/members/" (get-in joined ["machine" "machine_id"]))
               nil)
          (expect (= [] (get (ok! b :get "/v1/council/rooms" nil) "rooms")))
          (expect (not= rid (get (ok! b :get cb nil) "default_group_id")))
          (ok! a :delete (str "/v1/council/rooms/" rid) nil)
          (doseq [port [a b]]
            (expect (false? (get (ok! port :delete "/v1/council/rooms" nil) "configured")))
            (expect (false? (get (ok! port :get "/v1/council/rooms" nil) "configured"))))
          (reset! machines []))
        (finally (doseq [port @machines]
                   (try (request! port :delete "/v1/council/rooms" nil) (catch Exception _ nil)))
                 ((:stop! provider))
                 (doseq [process (reverse @processes)]
                   (#'binary/kill-tree! process))
                 (#'binary/delete-tree! dir))))))
