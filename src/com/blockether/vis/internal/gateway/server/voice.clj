(ns com.blockether.vis.internal.gateway.server.voice
  "Authenticated live voice lifecycle and origin-bound WebSocket upgrades."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.voice :as voice]
            [com.blockether.vis.internal.util :as util]
            [ring.websocket :as ws]
            [taoensso.telemere :as tel]))

(def ^:private media-pattern #"/v1/sessions/([^/]+)/voice/live/([^/]+)/media")

(defn media-upgrade?
  "The dedicated media version replaces the HTTP protocol header only on this upgrade."
  [request]
  (and (= :get (:request-method request))
       (re-matches media-pattern (str (:uri request)))
       (ws/upgrade-request? request)
       (some #{"vis-voice.v1"} (ws/request-protocols request))))

(defn- media-ticket
  [request]
  (some #(when (str/starts-with? % "vis-ticket.") (subs % 11)) (ws/request-protocols request)))

(defn authenticated-upgrade
  "Consume a one-use ticket before the ordinary token gate. Return a scoped request or nil."
  [request]
  (when (media-upgrade? request)
    (let [[_ sid id]
          (re-matches media-pattern (:uri request))

          ticket
          (media-ticket request)]

      (when-let [runtime (voice/claim! sid id ticket (get-in request [:headers "origin"]))]
        (assoc request ::runtime runtime)))))

(defn authenticated? [request] (boolean (::runtime request)))

(defn wrap-media-auth
  "Authenticate media before the bearer gate. Require a ticket on loopback too."
  [handler]
  (fn [request]
    (if (media-upgrade? request)
      (if-let [scoped (authenticated-upgrade request)]
        (handler scoped)
        (http/error-response 401 :invalid-voice-ticket "A valid one-use voice ticket is required"))
      (handler request))))

(defn- start-handler
  [request]
  (let [sid (http/path-sid request)]
    (cond (or (nil? sid) (nil? (state/soul sid))) (http/session-404 (get-in request
                                                                            [:path-params :sid]))
          (state/session-archived? sid) (http/session-archived-409 sid)
          :else
          (let [{:keys [status data error]} (voice/start! sid
                                                          (wire/->engine (http/body-json request))
                                                          (get-in request [:headers "origin"]))]
            (if data
              (assoc-in (http/json-response status data) [:headers "Cache-Control"] "no-store")
              (http/error-response status
                                   (keyword error)
                                   "Live voice is not available for this request"))))))

(defn- with-runtime
  [request f]
  (if-let [runtime (voice/runtime-for (http/path-sid request) (get-in request [:path-params :vid]))]
    (f runtime)
    (http/error-response 404 :voice-not-found "This voice session is no longer available")))

(defn- snapshot-handler
  [request]
  (if-let [snapshot (voice/snapshot-for (http/path-sid request)
                                        (get-in request [:path-params :vid]))]
    (http/json-response 200 snapshot)
    (http/error-response 404 :voice-not-found "This voice session is no longer available")))

(defn- close-handler
  [request]
  (with-runtime request
                (fn [runtime]
                  (voice/close! runtime "user_ended")
                  (http/json-response 200 {:closed true}))))

(defn- media-error-details
  [error known-secrets]
  (let [{:keys [via trace]} (Throwable->map error)]
    (util/redact-secret-text (pr-str {:causes (mapv #(select-keys % [:type :message :at]) via)
                                      :trace trace})
                             known-secrets)))

(defn- peer-left?
  "True when the connection ended: an I/O failure or a timeout, not a fault in the media."
  [error]
  (boolean (some #(or (instance? java.io.IOException %)
                      (str/ends-with? (.getName (class %)) "TimeoutException"))
                 (take-while some? (iterate ex-cause error)))))

(defn- log-media-error!
  [error ticket]
  (if (peer-left? error)
    ;; A lost connection is a usual end of a voice session, not an error.
    (tel/log! :info
              [(str "Live voice media closed: "
                    (util/redact-secret-text (str (ex-message error)) [ticket]))])
    (let [message (str "Live voice media failed: " (media-error-details error [ticket]))]
      (tel/log! :error [message])
      (.println System/err message))))

(defn- media-handler
  [request]
  (if-let [runtime (::runtime request)]
    (let [ticket (media-ticket request)]
      {:status 101
       :headers {"Cache-Control" "no-store"}
       :ring.websocket/protocol "vis-voice.v1"
       :ring.websocket/listener {:on-open #(voice/open-socket! runtime %)
                                 :on-message (fn [_ value]
                                               (voice/message! runtime value))
                                 :on-error (fn [_ error]
                                             (try (log-media-error! error ticket)
                                                  (finally (voice/close! runtime
                                                                         "connection_failed"))))
                                 :on-close (fn [_ _ _]
                                             (voice/close! runtime "connection_closed"))}})
    (http/error-response 401 :invalid-voice-ticket "A valid one-use voice ticket is required")))

(defn shutdown! [] (voice/shutdown!))

(def handlers
  {[:get "/v1/voice/live/capabilities"] (fn [_]
                                          (http/json-response 200 (voice/capabilities)))
   [:post "/v1/sessions/:sid/voice/live"] start-handler
   [:get "/v1/sessions/:sid/voice/live/:vid"] snapshot-handler
   [:delete "/v1/sessions/:sid/voice/live/:vid"] close-handler
   [:get "/v1/sessions/:sid/voice/live/:vid/media"] media-handler})
