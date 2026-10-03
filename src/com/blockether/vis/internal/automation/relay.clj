(ns com.blockether.vis.internal.automation.relay
  "The relay inbox: webhook requests for a gateway without a public address.

   The gateway creates one inbox at the relay and keeps the inbox token in a
   private state file. Another service sends its webhook to the public inbox
   address. The gateway collects the stored requests, checks each one on the
   normal webhook path and acknowledges the requests that it processed. The
   relay never holds an automation secret, so it cannot forge an accepted
   request. The gateway contacts the relay only while automations are on and
   an enabled webhook automation has a secret."
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.automation.runner :as runner]
            [com.blockether.vis.internal.persistance.core :as ps]
            [taoensso.telemere :as tel])
  (:import (java.nio.file CopyOption Files LinkOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util Base64)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)
           (java.util.concurrent.atomic AtomicBoolean)))

(set! *warn-on-reflection* true)

(def ^:private min-wait-ms
  "The wait after a page with requests, and the start of the backoff after a wake. The
   poller collects a full page at once."
  5000)

(def ^:private max-wait-ms "The longest wait between two polls, also after a failure." 60000)

(defonce ^:private state-lock (Object.))

(defonce ^:private runtime (atom nil))

(defonce ^:private http-client (delay (http/client {:connect-timeout 10000})))

(defn- state-path
  ^Path []
  (.toPath (io/file (or (System/getProperty "vis.automations.home")
                        (System/getenv "VIS_HOME")
                        (str (System/getProperty "user.home") "/.vis"))
                    "automations"
                    "relay-inbox.json")))

(defn- unsafe?
  [^Path path]
  (or (Files/isSymbolicLink path) (Files/isSymbolicLink (.getParent path))))

(defn read-state
  "The saved inbox `{relay_url inbox_id token}`, or nil when it is missing,
   unsafe or invalid."
  []
  (let [path (state-path)]
    (when (and (not (unsafe? path)) (Files/exists path (make-array LinkOption 0)))
      (let [value (try (wire/parse-json (slurp (.toFile path))) (catch Throwable _ nil))]
        (when (and (map? value)
                   (string? (get value "relay_url"))
                   (document/valid-json? "automations" "relay_inbox" (dissoc value "relay_url")))
          value)))))

(defn- save-state!
  "Write the inbox state atomically. Only the owner can read it."
  [value]
  (let [path
        (state-path)

        parent
        (.getParent path)]

    (when (unsafe? path)
      (throw (ex-info "The relay inbox state path is a symbolic link" {:path (str path)})))
    (Files/createDirectories parent
                             (into-array FileAttribute
                                         [(PosixFilePermissions/asFileAttribute
                                            (PosixFilePermissions/fromString "rwx------"))]))
    (let [temp (Files/createTempFile parent
                                     "relay-inbox-"
                                     ".json"
                                     (into-array FileAttribute
                                                 [(PosixFilePermissions/asFileAttribute
                                                    (PosixFilePermissions/fromString
                                                      "rw-------"))]))]
      (try (spit (.toFile temp) (wire/json-str value))
           (Files/move temp
                       path
                       (into-array CopyOption
                                   [StandardCopyOption/ATOMIC_MOVE
                                    StandardCopyOption/REPLACE_EXISTING]))
           value
           (finally (Files/deleteIfExists temp))))))

(defn- forget-state!
  "Delete the saved inbox when it is still `state`."
  [state]
  (locking state-lock (when (= state (read-state)) (Files/deleteIfExists (state-path)))))

(defn- publish!
  "Show the inbox address in automation webhook URLs, or remove it. Answers `state`."
  [state]
  (automation/set-relay-base!
    (some->> state
             ((juxt #(get % "relay_url") (constantly "/hooks/") #(get % "inbox_id")))
             (apply str)))
  state)

(defn- call!
  "One relay request. Answers {:status … :body …}. A transport failure has status 0."
  [method uri {:keys [token body]}]
  (try (let [resp (http/request (cond-> {:uri uri
                                         :method method
                                         :client @http-client
                                         :headers (cond-> {"accept" "application/json"}
                                                    token
                                                    (assoc "authorization" (str "Bearer " token))

                                                    body
                                                    (assoc "content-type" "application/json"))
                                         :timeout 15000
                                         :throw false
                                         :as :string}
                                  body
                                  (assoc :body (wire/json-str body))))]
         {:status (:status resp)
          :body (try (wire/parse-json (:body resp)) (catch Throwable _ nil))})
       (catch Throwable t {:status 0 :body nil :error (ex-message t)})))

(defn- refused!
  [id status body]
  (tel/log! {:level :warn :id id :data {:status status :code (get-in body ["error" "code"])}})
  nil)

(defn ensure-inbox!
  "The saved inbox at relay `url`. The relay creates a new inbox when none is
   saved or the saved one belongs to another relay. Answers nil when the relay
   refuses."
  [url]
  (locking state-lock
    (let [state (read-state)]
      (if (= url (get state "relay_url"))
        state
        (let [{:keys [status body]} (call! :post (str url "/v1/hooks/inboxes") {})]
          (if (and (= 201 status) (document/valid-json? "automations" "relay_inbox" body))
            (save-state! (assoc body "relay_url" url))
            (refused! ::inbox-refused status body)))))))

(defn- wanted?
  "True when automations are on and an enabled webhook automation has a secret."
  [db]
  (and (runner/globally-enabled? db)
       (boolean (some (fn [row]
                        (and (:webhook_secret row)
                             (get-in row [:definition "enabled"])
                             (automation/webhook-trigger (:definition row))))
                      (ps/db-automation-list db)))))

(defn- deliver!
  "Hand one stored request to the webhook path. True when the relay can delete
   it. A refused request is done too; a failure of this gateway is not."
  [db {:strs [id automation_id received_at headers body]}]
  (if-let [data (try (.decode (Base64/getDecoder) ^String body)
                     (catch IllegalArgumentException _ nil))]
    (try (let [{:keys [status error]}
               (runner/accept-webhook!
                 db
                 automation_id
                 {:headers headers :body data :received-at received_at :relay-id id})]
           (tel/log! {:level :info
                      :id ::delivered
                      :data {:automation-id automation_id :status status :code (first error)}})
           true)
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::deliver-failed
                      :data {:automation-id automation_id :error (ex-message t)}})
           false))
    (do (tel/log! {:level :warn :id ::invalid-body :data {:automation-id automation_id}}) true)))

(defn collect!
  "Collect one page from the inbox, hand each request to the webhook path and
   acknowledge the processed requests. Answers the number of requests in the
   page, or nil when the relay failed. A refused token deletes the saved inbox."
  [db state]
  (let [{relay "relay_url" token "token"}
        state

        {:keys [status body]}
        (call! :get (str relay "/v1/hooks/inbox") {:token token})]

    (cond (= 401 status) (do (tel/log! {:level :warn :id ::inbox-lost :data {:relay relay}})
                             (forget-state! state)
                             (publish! nil)
                             nil)
          (not= 200 status) (refused! ::poll-failed status body)
          (not (document/valid-json? "automations" "relay_page" body))
          (refused! ::invalid-page status nil)
          :else (let [requests
                      (get body "requests")

                      done
                      (into [] (comp (filter #(deliver! db %)) (map #(get % "id"))) requests)]

                  (when (seq done)
                    (let [{:keys [status body]} (call! :post
                                                       (str relay "/v1/hooks/inbox/ack")
                                                       {:token token :body {"ids" done}})]
                      (when-not (= 200 status) (refused! ::ack-failed status body))))
                  (count requests)))))

(defn- next-wait
  "The wait after a poll that collected `collected` requests, or nil on failure."
  ^long [^long wait collected]
  (cond (nil? collected) max-wait-ms
        (>= (long collected) (automation/limit :relay_page_requests)) 0
        (pos? (long collected)) min-wait-ms
        :else (min (long max-wait-ms) (* 2 (max (long min-wait-ms) wait)))))

(defn- step!
  "One poller step. Answers the wait before the next step."
  ^long [db relay-url ^long wait]
  (let [url (relay-url)]
    (cond (nil? url) (do (publish! nil) max-wait-ms)
          (wanted? db) (if-let [state (publish! (ensure-inbox! url))]
                         (next-wait wait (collect! db state))
                         max-wait-ms)
          :else (let [saved (read-state)]
                  (publish! (when (= url (get saved "relay_url")) saved))
                  max-wait-ms))))

(defn refresh!
  "Prepare the inbox now and wake the poller, for example after a new webhook
   secret. Never throws."
  [db]
  (try (when-let [{:keys [relay-url ^LinkedBlockingQueue wake]} @runtime]
         (when-let [url (relay-url)]
           (when (wanted? db) (publish! (ensure-inbox! url))))
         (.offer wake :wake))
       (catch Throwable t
         (tel/log! {:level :warn :id ::refresh-failed :data {:error (ex-message t)}}))))

(defn start!
  "Start the inbox poller. `relay-url` answers the usable relay base URL, or nil
   when relaying is off. Answers a function that stops the poller."
  [db-spec relay-url]
  (let [db
        (ps/db-shared-connection! db-spec)

        running
        (AtomicBoolean. true)

        wake
        (LinkedBlockingQueue.)

        thread
        (Thread. ^Runnable
                 (fn []
                   (loop [wait min-wait-ms]
                     (when (.get running)
                       (let [next (try (step! db relay-url wait)
                                       (catch Throwable t
                                         (tel/log! {:level :warn
                                                    :id ::step-failed
                                                    :data {:error (ex-message t)}})
                                         max-wait-ms))
                             woken (when (pos? next)
                                     (try (.poll wake next TimeUnit/MILLISECONDS)
                                          (catch InterruptedException _ nil)))]

                         (.clear wake)
                         ;; After a wake, a request can arrive soon, so the backoff starts again.
                         (recur (if woken min-wait-ms next))))))
                 "vis-automation-relay")]

    (reset! runtime {:relay-url relay-url :wake wake})
    (.setDaemon thread true)
    (.start thread)
    (fn stop! []
      (.set running false)
      (reset! runtime nil)
      (.interrupt thread))))
