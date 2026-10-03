(ns com.blockether.vis.internal.automation.relay-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.automation.relay :as relay]
            [com.blockether.vis.internal.automation.runner :as runner]
            [com.blockether.vis.internal.automation.webhook :as webhook]
            [com.blockether.vis.internal.persistance.core :as ps]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io File)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util Base64)))

(def ^:private inbox-id "AbCdEfGhIjKlMnOpQrStUv")

(def ^:private token (apply str (repeat 43 "t")))

(defn- utf8 ^bytes [^String text] (.getBytes text StandardCharsets/UTF_8))

(defn- respond!
  [^HttpExchange exchange status value]
  (let [data (utf8 (wire/json-str value))]
    (.add (.getResponseHeaders exchange) "content-type" "application/json")
    (.sendResponseHeaders exchange (int status) (alength data))
    (with-open [out (.getResponseBody exchange)]
      (.write out data))))

(defn- acknowledge
  [relay-state ids]
  (-> relay-state
      (update :acked into ids)
      (update :requests
              (fn [requests]
                (filterv #(not (contains? (set ids) (get % "id"))) requests)))))

(defn- fake-relay
  "A relay with one inbox. Its `:state` atom holds :requests, :acked, :creates and :lost?."
  []
  (let [state
        (atom {:requests [] :acked [] :creates 0 :lost? false})

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext
      server
      "/"
      (reify
        HttpHandler
          (handle [_ exchange]
            (let [path
                  (.getPath (.getRequestURI exchange))

                  authorized?
                  (and (= (str "Bearer " token)
                          (.getFirst (.getRequestHeaders exchange) "authorization"))
                       (not (:lost? @state)))]

              (cond (= "/v1/hooks/inboxes" path)
                    (do (swap! state update :creates inc)
                        (respond! exchange 201 {"inbox_id" inbox-id "token" token}))
                    (not authorized?)
                    (respond! exchange 401 {"error" {"code" "unauthorized" "message" "no"}})
                    (= "/v1/hooks/inbox" path)
                    (respond! exchange 200 {"requests" (:requests @state)})
                    :else (let [ids (get (wire/parse-json (slurp (.getRequestBody exchange)))
                                         "ids")]
                            (swap! state acknowledge ids)
                            (respond! exchange 200 {"acked" (count ids)})))))))
    (.start server)
    {:state state :server server :url (str "http://127.0.0.1:" (.getPort (.getAddress server)))}))

(defn- with-relay
  [f]
  (let [relay-server (fake-relay)]
    (try (f relay-server) (finally (.stop ^HttpServer (:server relay-server) 0)))))

(defn- with-home
  "Run `f` with a fresh automation home directory."
  [f]
  (let [home
        (.toFile (Files/createTempDirectory "vis-relay-" (make-array FileAttribute 0)))

        previous
        (System/getProperty "vis.automations.home")]

    (System/setProperty "vis.automations.home" (str home))
    (try (f home)
         (finally (if previous
                    (System/setProperty "vis.automations.home" previous)
                    (System/clearProperty "vis.automations.home"))
                  (automation/set-relay-base! nil)
                  (run! #(.delete ^File %) (reverse (file-seq home)))))))

(defn- with-store
  "Run `f` with a fresh store, a fake runtime and automations turned on."
  [f]
  (let [db (ps/db-create-connection! :memory)]
    (runner/install-runtime! {:submit! (constantly {"status" "done" "turn_id" "t-1" "content" []})
                              :create-session! (constantly {"id" "s-new"})
                              :delete-session! (constantly nil)
                              :session? (constantly false)
                              :notify! (constantly nil)})
    (try (with-redefs [runner/allowed? (constantly true)
                       runner/globally-enabled? (constantly true)]

           (f db))
         (finally (ps/db-dispose-connection! db)))))

(defn- create!
  [db signature]
  (get (automation/create! db
                           {"name" (str "Relay " signature)
                            "triggers" [{"kind" "webhook" "signature" signature}]
                            "prompt" "Check {action}."
                            "target" {"mode" "temporary"}}
                           (System/currentTimeMillis))
       "id"))

(defn- run-count
  [db automation-id]
  (count (ps/db-automation-runs db {:automation-id automation-id :limit 10})))

(defn- stored
  [request-id automation-id received-at headers ^String text]
  {"id" request-id
   "automation_id" automation-id
   "received_at" received-at
   "headers" headers
   "body" (.encodeToString (Base64/getEncoder) (utf8 text))})

(defdescribe
  relay-inbox-test
  (it "creates one private inbox, reuses it and replaces it for another relay"
      (with-home
        (fn [home]
          (with-relay
            (fn [a]
              (with-relay
                (fn [b]
                  (let [state
                        (relay/ensure-inbox! (:url a))

                        file
                        (io/file home "automations" "relay-inbox.json")]

                    (expect (= {"relay_url" (:url a) "inbox_id" inbox-id "token" token} state))
                    (expect (= state (relay/ensure-inbox! (:url a))))
                    (expect (= 1 (:creates @(:state a))))
                    (expect (= "rw-------"
                               (PosixFilePermissions/toString (Files/getPosixFilePermissions
                                                                (.toPath file)
                                                                (make-array LinkOption 0)))))
                    (expect (= (:url b) (get (relay/ensure-inbox! (:url b)) "relay_url")))
                    (expect (= (:url b) (get (relay/read-state) "relay_url")))))))))))
  (it
    "checks stored requests on the webhook path and acknowledges them"
    (with-home
      (fn [_]
        (with-store
          (fn [db]
            (with-relay
              (fn [relay-server]
                (let [standard
                      (create! db "standard")

                      standard-secret
                      (get (automation/rotate-secret! db standard "webhook" 1) "secret")

                      received
                      (- (System/currentTimeMillis) 600000)

                      timestamp
                      (str (quot received 1000))

                      body
                      "{\"action\":\"opened\"}"

                      signed
                      {"webhook-id" "msg-1"
                       "webhook-timestamp" timestamp
                       "webhook-signature"
                       (webhook/standard-signature standard-secret "msg-1" timestamp (utf8 body))}

                      state
                      (relay/ensure-inbox! (:url relay-server))]

                  ;; The relay received the request ten minutes ago: its time, not
                  ;; the time of collection, sets the signature window.
                  (swap! (:state relay-server) assoc
                    :requests
                    [(stored "r00000000000000000001a" standard received signed body)
                     (stored "r00000000000000000002a"
                             standard
                             received
                             (assoc signed "webhook-signature" "v1,AAAA")
                             body)])
                  (expect (= 2 (relay/collect! db state)))
                  (expect (= ["r00000000000000000001a" "r00000000000000000002a"]
                             (:acked @(:state relay-server))))
                  (expect (= 1 (run-count db standard)))))))))))
  (it "starts one run when the relay repeats a request without a delivery header"
      (with-home
        (fn [_]
          (with-store
            (fn [db]
              (with-relay
                (fn [relay-server]
                  (let [id
                        (create! db "token")

                        secret
                        (get (automation/rotate-secret! db id "webhook" 1) "secret")

                        request
                        (stored "r00000000000000000003a"
                                id
                                (System/currentTimeMillis)
                                {"x-webhook-token" secret}
                                "{\"action\":\"closed\"}")

                        state
                        (relay/ensure-inbox! (:url relay-server))]

                    (swap! (:state relay-server) assoc :requests [request])
                    (expect (= 1 (relay/collect! db state)))
                    ;; The acknowledgement was lost: the relay sends the request again.
                    (swap! (:state relay-server) assoc :requests [request])
                    (expect (= 1 (relay/collect! db state)))
                    (expect (= 1 (run-count db id)))))))))))
  (it "forgets an inbox that the relay no longer knows"
      (with-home (fn [_]
                   (with-store (fn [db]
                                 (with-relay (fn [relay-server]
                                               (let [state (relay/ensure-inbox! (:url
                                                                                  relay-server))]
                                                 (swap! (:state relay-server) assoc :lost? true)
                                                 (expect (nil? (relay/collect! db state)))
                                                 (expect (nil? (relay/read-state)))))))))))
  (it "adds the public relay address to webhook automations"
      (with-home
        (fn [_]
          (with-store (fn [db]
                        (let [id
                              (create! db "github")

                              base
                              (str "https://relay.example.com/hooks/" inbox-id)

                              _
                              (automation/set-relay-base! base)

                              described
                              (automation/describe db id (System/currentTimeMillis))]

                          (expect (= {"path" (str "/v1/hooks/" id) "url" (str base "/" id)}
                                     (get described "webhook")))
                          (expect
                            (document/valid-json? "automations" "automation" described)))))))))
