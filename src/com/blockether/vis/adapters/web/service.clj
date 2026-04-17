(ns com.blockether.vis.adapters.web.service
  "Managed web service lifecycle with health endpoints."
  (:require [com.blockether.vis.loop.conversations.core :as conversations]
            [com.blockether.vis.adapters.web.conversations :as web-conversations]
            [com.blockether.vis.adapters.web.executor :as executor]
            [com.blockether.vis.adapters.web.routes :as routes]
            [ring.adapter.jetty :as jetty])
  (:import [org.eclipse.jetty.server Server]
           [java.time Instant]))

(defonce ^:private service-state
  (atom {:server nil :started-at nil :port nil}))

(defn status []
  (let [{:keys [server started-at port]} @service-state]
    {:running? (some? server)
     :port     port
     :started-at started-at}))

(defn health-handler [_req]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body "{\"status\":\"ok\"}"})

(defn readiness-handler [_req]
  (let [{:keys [server port started-at]} @service-state
        ok? (and (some? server) (.isRunning ^Server server))]
    {:status (if ok? 200 503)
     :headers {"Content-Type" "application/json"}
     :body (format "{\"ready\":%s,\"port\":%s,\"startedAt\":\"%s\"}"
             (if ok? "true" "false")
             (or port "null")
             (or (str started-at) "null"))}))

(defn wrap-health [handler]
  (fn [req]
    (let [uri  (:uri req)
          meth (:request-method req)]
      (cond
        (and (= meth :get) (= uri "/health"))        (health-handler req)
        (and (= meth :get) (= uri "/health/ready"))   (readiness-handler req)
        :else (handler req)))))

(defn start!
  ([] (start! {}))
  ([{:keys [port] :or {port 3000}}]
   (when-let [^Server s (:server @service-state)]
     (try (.stop s) (catch Exception _)))
   (executor/start!)
   (let [handler (wrap-health #'routes/handler)
         srv     (jetty/run-jetty handler
                   {:port port :join? false :host "0.0.0.0"})]
     (swap! service-state assoc
       :server srv :started-at (Instant/now) :port port)
     (println (str "[vis] web service started on http://0.0.0.0:" port))
     (println (str "[vis] health: http://0.0.0.0:" port "/health"))
     (println (str "[vis] conversations: " (count (web-conversations/conversations-list))))
     srv)))

(defn stop! []
  (executor/stop!)
  (when-let [^Server s (:server @service-state)]
    (try (.stop s) (catch Exception _)))
  (swap! service-state assoc :server nil :started-at nil :port nil)
  (conversations/close-all!)
  (println "[vis] web service stopped."))
