(ns com.blockether.vis.adapters.web.app
  "Web server boot/shutdown — delegates to web.service."
  (:require [com.blockether.vis.adapters.web.service :as service]
            [taoensso.trove :as trove]))

(defn -main [& args]
  (let [port (if (seq args) (parse-long (first args)) 3000)]
    (.addShutdownHook (Runtime/getRuntime)
      (Thread. ^Runnable (fn []
                           (trove/log! {:level :info :id ::shutdown :msg "shutting down vis web"})
                           (service/stop!))))
    (service/start! {:port port})
    @(promise)))
