(ns com.blockether.vis.internal.gateway.server.devices
  "Device routes: push-device registration and tests, and the durable machine
   order."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.server.instance :as instance]))

(defn- machine-order-handler
  "POST /v1/machines/order {machine_ids: [...]} — register stable gateway ids and
   return their durable order. This gateway leads; new ids append in lexical order.
   Subset requests never delete or move existing ids. Clients send no credentials
   or addresses and retain this answer when the primary gateway is unavailable."
  [request]
  (let [ids (get (try (http/body-json request) (catch Throwable _ nil)) "machine_ids")]
    (if-not (and (vector? ids)
                 (<= (count ids) 256)
                 (every? #(and (string? %) (<= 1 (count %) 200) (re-matches #"[A-Za-z0-9_-]+" %))
                         ids))
      (http/error-response 400
                           :invalid-machine-ids
                           "machine_ids must contain at most 256 gateway ids.")
      (let [{:keys [db host port]} @instance/server-state
            own-id (instance/gateway-instance-id db host port)
            saved (config/update-machine-config!
                    (fn [raw]
                      (let [known (vec (distinct (cons own-id (get raw "machine_order" []))))
                            added (sort (remove (set known) (distinct ids)))]

                        (assoc raw "machine_order" (into known added)))))]

        (http/json-response {:machine-ids (get saved "machine_order")})))))

(defn- device-wire
  "One registered device in wire shape — the raw token and the relay grant NEVER
   leave the gateway."
  [device]
  (push/public-device device))

(defn- list-devices-handler
  "GET /v1/devices — registered push devices (tokens masked) plus this gateway's
   APNs readiness, so a client can tell \"push impossible here\" apart from
   \"push possible, this device just isn't registered\"."
  [_]
  (http/json-response {:devices (push/list-devices) :push (push/status)}))

(defn- register-device-handler
  "POST /v1/devices — idempotently register one device for push.
   `{token?, grant?, platform?, environment?, client?, client_version?, label?,
   bundle_id?}`. Exactly one identifier is required: a raw APNs/FCM `token`
   this gateway pushes to with its OWN credentials, or a relay `grant` the
   device obtained from the push relay — the relay holds the signing key, so a
   gateway that is not the app's publisher can wake it without ever learning
   the device token. Re-registering refreshes instead of duplicating."
  [request]
  (let [body
        (http/body-json request)

        token
        (some-> (get body "token")
                str
                str/trim)

        grant
        (some-> (get body "grant")
                str
                str/trim)]

    (if (and (str/blank? token) (str/blank? grant))
      (http/error-response 400 :bad-request "token or grant is required")
      (if-let [device (push/register-device! {:token token
                                              :grant grant
                                              :platform (get body "platform")
                                              :environment (get body "environment")
                                              :client (get body "client")
                                              :client-version (get body "client_version")
                                              :label (get body "label")
                                              :bundle-id (get body "bundle_id")
                                              :relay-url (get body "relay_url")})]
        (http/json-response {:device (device-wire device) :push (push/status)})
        (http/error-response 400 :bad-request "unusable device token")))))

(defn- delete-device-handler
  "DELETE /v1/devices/:token — stop pushing to this device (logout, permission
   revoked, app uninstalled)."
  [request]
  (let [token (str (get-in request [:path-params :token]))]
    (http/json-response {:is_removed (push/unregister-device! token)})))

(defn- test-device-handler
  "POST /v1/devices/actions/test — send one test alert to every registered device
   and report APNs' per-device verdict. The ONLY way to prove the whole chain
   (key, topic, environment, token) without waiting for a real turn."
  [_]
  (if-not (push/any-configured?)
    (http/error-response 503
                         :push-unavailable "push is not configured on this gateway"
                         :push (push/status))
    (http/json-response {:results (push/broadcast! {:title "Vis"
                                                    :body "Push notifications are working."
                                                    :data {:type "test"}})
                         :push (push/status)})))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/devices"] list-devices-handler
   [:post "/v1/devices"] register-device-handler
   [:post "/v1/devices/actions/test"] test-device-handler
   [:delete "/v1/devices/:token"] delete-device-handler
   [:post "/v1/machines/order"] machine-order-handler})
