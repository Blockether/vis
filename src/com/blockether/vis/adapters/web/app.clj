(ns com.blockether.vis.adapters.web.app
  "Web server boot/shutdown for the web adapter."
  (:require [com.blockether.vis.rlm.conversations.core :as conversations]
            [com.blockether.vis.adapters.web.conversations :as web-conversations]
            [com.blockether.vis.adapters.web.executor :as executor]
            [com.blockether.vis.adapters.web.routes :as routes]
            [ring.adapter.jetty :as jetty]))

;;; ── Jetty ──────────────────────────────────────────────────────────

(defonce server (atom nil))

(defn start! [& [{:keys [port] :or {port 3000}}]]
  (when @server (.stop @server))
  (executor/start!)
  (println (str "Starting vis web on http://0.0.0.0:" port))
  (println (str "Conversations: " (count (web-conversations/conversations-list))))
  (reset! server (jetty/run-jetty #'routes/handler
                   {:port port :join? false :host "0.0.0.0"})))

(defn stop! []
  (executor/stop!)
  (when @server (.stop @server) (reset! server nil))
  (conversations/close-all!))

(defn -main [& args]
  (let [port (if (seq args) (parse-long (first args)) 3000)]
    (.addShutdownHook (Runtime/getRuntime)
      (Thread. ^Runnable (fn []
                           (println "Shutting down vis web…")
                           (stop!))))
    (start! {:port port})
    @(promise)))
