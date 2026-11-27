(ns com.blockether.vis.internal.gateway.server.tasks
  "List, read, cancel and acknowledge session Tasks."
  (:require [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.task :as task]))

(defn- task-404 []
  (http/error-response 404 :task-not-found "unknown Task"))

(defn- task-read [session id request]
  (case (http/query-str request "include_record")
    "true" (if-let [detail (task/status session id true)]
             (http/json-response detail)
             (task-404))
    (nil "false") (if-let [row (task/wire-state session id)]
                    (http/json-response {:task row})
                    (task-404))
    (http/error-response 400 :invalid-request "include_record must be true or false")))

(defn- task-handler [operation]
  (fn [request]
    (let [{:keys [sid task-id]} (:path-params request)
          session (http/path-sid request)]
      (if-not (and session (state/session-exists? session))
        (http/session-404 sid)
        (case operation
          :list (http/json-response {:tasks (task/wire-tasks session)})
          :read (task-read session task-id request)
          :cancel (if-let [result (task/cancel-result! session task-id :person)]
                     (http/json-response result)
                     (task-404))
          :acknowledge (if (task/wire-state session task-id)
                         (http/json-response {:acknowledged (boolean (task/acknowledge! session task-id))})
                         (task-404)))))))

(def handlers
  "Handlers for the session Task routes."
  {[:get "/v1/sessions/:sid/tasks"] (task-handler :list)
   [:get "/v1/sessions/:sid/tasks/:task-id"] (task-handler :read)
   [:post "/v1/sessions/:sid/tasks/:task-id/cancel"] (task-handler :cancel)
   [:post "/v1/sessions/:sid/tasks/:task-id/acknowledge"] (task-handler :acknowledge)})
