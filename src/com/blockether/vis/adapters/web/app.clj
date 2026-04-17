(ns com.blockether.vis.adapters.web.app
  "Web server boot/shutdown — delegates to web.service."
  (:require [com.blockether.vis.adapters.web.service :as service]))

(defn -main [& args]
  (let [port (if (seq args) (parse-long (first args)) 3000)]
    (.addShutdownHook (Runtime/getRuntime)
      (Thread. ^Runnable (fn []
                           (println "Shutting down vis web…")
                           (service/stop!))))
    (service/start! {:port port})
    @(promise)))
