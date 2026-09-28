(ns com.blockether.vis.internal.gateway.server.mcp
  "MCP server routes: configuration, lifecycle and authentication."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [com.blockether.vis.internal.gateway.server.http :as http]))

(defn- mcp-error-response
  [e]
  (let [{:keys [type]}
        (ex-data e)

        status
        (case type
          :mcp/not-found
          404

          :mcp/invalid-name
          400

          :mcp/invalid-server
          400

          :mcp/not-managed
          409

          ;; An auth flow the gateway no longer has: abandoned, cancelled, spent, or
          ;; swept after its TTL. The client must start a new one.
          :mcp/oauth-flow-not-found
          404

          (or (:status (ex-data e)) 400))]

    (http/error-response status (or type :mcp/invalid-request) (ex-message e))))

(defn- settings-target
  [request body]
  (let [params (merge (:query-params request) body)]
    (scoped/target (lp/db-info) (get params "scope") (get params "target_id"))))

(defn- mcp-servers-handler
  [request]
  (try (http/json-response (mcp/scoped-servers (lp/db-info) (settings-target request nil)))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- save-mcp-server-handler
  [request]
  (try (let [body
             (http/body-json request)

             name
             (or (get-in request [:path-params :name]) (get body "name"))

             server
             (or (get body "server") (dissoc body "scope" "target_id"))]

         (http/json-response
           (mcp/save-scoped-server! (lp/db-info) (settings-target request body) name server)))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (http/error-response 400 :mcp/invalid-request (ex-message e)))))

(defn- set-mcp-server-enabled-handler
  [request]
  (try (let [body
             (http/body-json request)

             enabled
             (get body "enabled")]

         (if (boolean? enabled)
           (http/json-response (mcp/set-scoped-server-enabled! (lp/db-info)
                                                               (settings-target request body)
                                                               (get-in request [:path-params :name])
                                                               enabled))
           (http/error-response 400 :mcp/invalid-request "enabled must be a boolean")))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- delete-mcp-server-handler
  [request]
  (try (http/json-response (mcp/delete-scoped-server! (lp/db-info)
                                                      (settings-target request nil)
                                                      (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- test-mcp-server-handler
  [request]
  (try (let [body (http/body-json request)]
         (http/json-response (mcp/test-gateway-server! (get body "name")
                                                       (or (get body "server") body))))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (http/error-response 400 :mcp/test-failed (ex-message e)))))

(defn- kill-mcp-server-handler
  "Stop a server NOW and hold it down. Not a config edit — works for hand-written
   servers too, because killing a runaway process is not rewriting the user's file."
  [request]
  (try (http/json-response (mcp/kill-gateway-server! (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- start-mcp-server-handler
  [request]
  (try (http/json-response (mcp/start-gateway-server! (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- mcp-auth-start-handler
  "Begin headless MCP OAuth. Optional callback_mode selects loopback or direct app
   return; the client cannot supply an arbitrary redirect URI."
  [request]
  (try (http/json-response (mcp/start-gateway-server-auth!
                             (get-in request [:path-params :name])
                             {:callback-mode (get (http/body-json request) "callback_mode")}))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (http/error-response 400 :mcp/oauth-failed (ex-message e)))))

(defn- mcp-auth-flow-id
  [body]
  (let [flow-id (get body "flow_id")]
    (when (and (string? flow-id) (seq (str/trim flow-id))) (str/trim flow-id))))

(defn- mcp-auth-complete-handler
  [request]
  (try (let [body
             (http/body-json request)

             flow-id
             (mcp-auth-flow-id body)

             input
             (or (get body "input") (get body "redirect_url") (get body "code"))]

         (if (and flow-id (string? input) (seq (str/trim ^String input)))
           (http/json-response (mcp/complete-gateway-server-auth! flow-id input))
           (http/error-response 400
                                :mcp/invalid-request
                                "flow_id and input (redirect URL or code) are required")))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (http/error-response 400 :mcp/oauth-failed (ex-message e)))))

(defn- mcp-auth-poll-handler
  [request]
  (try (if-let [flow-id (mcp-auth-flow-id (http/body-json request))]
         (http/json-response (mcp/poll-gateway-server-auth! flow-id))
         (http/error-response 400 :mcp/invalid-request "flow_id is required"))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- mcp-auth-cancel-handler
  [request]
  (try (if-let [flow-id (mcp-auth-flow-id (http/body-json request))]
         (http/json-response (mcp/cancel-gateway-server-auth! flow-id))
         (http/error-response 400 :mcp/invalid-request "flow_id is required"))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- mcp-auth-logout-handler
  [request]
  (try (http/json-response (mcp/logout-gateway-server-auth! (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- global-only
  [handler]
  (fn [request]
    (try
      (let [body
            (some-> (:body request)
                    slurp)

            decoded
            (when (seq body) (http/body-json (assoc request :body (java.io.StringReader. body))))

            params
            (merge (:query-params request) decoded)]

        (if (or (not= "global" (get params "scope" "global")) (some? (get params "target_id")))
          (http/error-response 400 :mcp/global-only "Lifecycle and authentication are global-only")
          (handler (cond-> request
                     body
                     (assoc :body (java.io.StringReader. body))))))
      (catch Exception e (mcp-error-response e)))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/mcp/servers"] mcp-servers-handler
   [:post "/v1/mcp/servers"] save-mcp-server-handler
   [:post "/v1/mcp/servers/actions/test"] (global-only test-mcp-server-handler)
   [:put "/v1/mcp/servers/:name"] save-mcp-server-handler
   [:delete "/v1/mcp/servers/:name"] delete-mcp-server-handler
   [:post "/v1/mcp/servers/:name/actions/enable"] set-mcp-server-enabled-handler
   [:post "/v1/mcp/servers/:name/actions/kill"] (global-only kill-mcp-server-handler)
   [:post "/v1/mcp/servers/:name/actions/start"] (global-only start-mcp-server-handler)
   [:post "/v1/mcp/servers/:name/auth/start"] (global-only mcp-auth-start-handler)
   [:post "/v1/mcp/servers/:name/auth/complete"] (global-only mcp-auth-complete-handler)
   [:post "/v1/mcp/servers/:name/auth/poll"] (global-only mcp-auth-poll-handler)
   [:post "/v1/mcp/servers/:name/auth/cancel"] (global-only mcp-auth-cancel-handler)
   [:post "/v1/mcp/servers/:name/auth/logout"] (global-only mcp-auth-logout-handler)})
