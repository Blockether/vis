(ns com.blockether.vis.internal.gateway.server.rooms
  "Human-controlled machine membership. The gateway owns credentials, not the Companion."
  (:require [clojure.walk :as walk]
            [com.blockether.vis.internal.council.rooms :as rooms]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.loop :as lp]))

(defn- handler
  [operation]
  (fn [request]
    (try (let [params
               (:path-params request)

               body
               (when (#{:register :join :create :invite :disconnect} operation)
                 (walk/keywordize-keys (http/body-json request)))

               result
               (case operation
                 :status
                 (rooms/status! (lp/db-info))

                 :register
                 (rooms/register! (lp/db-info) body)

                 :join
                 (rooms/join! (lp/db-info) body)

                 :create
                 (rooms/create! (lp/db-info) body)

                 :disconnect
                 (rooms/delete-machine! (lp/db-info) body)

                 (rooms/manage! (lp/db-info)
                                operation
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
   [:post "/v1/council/rooms/disconnect"] (handler :disconnect)
   [:post "/v1/council/rooms/register"] (handler :register)
   [:post "/v1/council/rooms/join"] (handler :join)
   [:delete "/v1/council/rooms/:room-id"] (handler :delete)
   [:post "/v1/council/rooms/:room-id/invites"] (handler :invite)
   [:delete "/v1/council/rooms/:room-id/invites/:invite-id"] (handler :revoke)
   [:get "/v1/council/rooms/:room-id/members"] (handler :members)
   [:delete "/v1/council/rooms/:room-id/members/:machine-id"] (handler :remove)})
