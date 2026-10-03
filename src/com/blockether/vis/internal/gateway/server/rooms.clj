(ns com.blockether.vis.internal.gateway.server.rooms
  "Human-controlled machine membership. The gateway owns credentials, not the Companion."
  (:require [clojure.walk :as walk]
            [com.blockether.vis.internal.council.rooms :as rooms]
            [com.blockether.vis.internal.gateway.server.http :as http]))

(defn- handler
  [operation]
  (fn [request]
    (try (let [params
               (:path-params request)

               body
               (when (#{:register :join :create :invite} operation)
                 (walk/keywordize-keys (http/body-json request)))

               result
               (case operation
                 :status
                 (rooms/status!)

                 :register
                 (rooms/register! body)

                 :join
                 (rooms/join! body)

                 :create
                 (rooms/create! body)

                 :disconnect
                 (rooms/delete-machine!)

                 (rooms/manage! operation
                                (:room-id params)
                                (or (:machine-id params) (:invite-id params))
                                body))]

           (http/json-response result))
         (catch clojure.lang.ExceptionInfo e
           (http/error-response (or (:status (ex-data e)) 400)
                                :rooms-error
                                "Council Rooms request failed"))
         (catch Exception _
           (http/error-response 503 :rooms-error "Council Rooms is unavailable")))))

(def handlers
  {[:get "/v1/council/rooms"] (handler :status)
   [:post "/v1/council/rooms"] (handler :create)
   [:delete "/v1/council/rooms"] (handler :disconnect)
   [:post "/v1/council/rooms/register"] (handler :register)
   [:post "/v1/council/rooms/join"] (handler :join)
   [:delete "/v1/council/rooms/:room-id"] (handler :delete)
   [:post "/v1/council/rooms/:room-id/invites"] (handler :invite)
   [:delete "/v1/council/rooms/:room-id/invites/:invite-id"] (handler :revoke)
   [:get "/v1/council/rooms/:room-id/members"] (handler :members)
   [:delete "/v1/council/rooms/:room-id/members/:machine-id"] (handler :remove)})
