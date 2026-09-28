(ns com.blockether.vis.internal.gateway.machine-order-test
  (:require [lazytest.core :refer [defdescribe expect it]]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.server.devices]
            [com.blockether.vis.internal.gateway.server.instance]
            [com.blockether.vis.contract.wire :as wire]))

(defdescribe
  machine-order-is-durable-and-independent-of-request-order
  (it "machine order is durable and independent of request order"
      ;; Regression: machine connectivity and client pairing order moved tabs.
      (let [dir
            (.toFile (java.nio.file.Files/createTempDirectory
                       "vis-machine-order"
                       (make-array java.nio.file.attribute.FileAttribute 0)))

            handler
            (ns-resolve 'com.blockether.vis.internal.gateway.server.devices 'machine-order-handler)

            identity
            (ns-resolve 'com.blockether.vis.internal.gateway.server.instance 'gateway-instance-id)

            request
            (fn [ids]
              (handler {:body (java.io.StringReader. (wire/json-str {:machine-ids ids}))}))]

        (try (with-redefs-fn {#'config/config-dir (constantly (.getPath dir))
                              identity (fn [& _]
                                         "primary")}
               (fn []
                 (expect (= 200 (:status (request ["z" "b" "primary" "b"]))))
                 (expect (= ["primary" "b" "z"]
                            (get (config/load-global-config-raw) "machine_order")))
                 (expect (= (:body (request ["b" "z"])) (:body (request ["z" "b"]))))
                 (expect (= 200 (:status (request ["a"]))))
                 (expect (= ["primary" "b" "z" "a"]
                            (get (config/load-global-config-raw) "machine_order")))
                 (doseq [invalid [nil "id" [""] [12] ["https://gateway.example.com"]
                                  (vec (repeat 257 "x"))]]
                   (expect (= 400 (:status (request invalid)))))
                 (expect (= ["primary" "b" "z" "a"]
                            (get (config/load-global-config-raw) "machine_order")))))
             (finally (doseq [f (reverse (file-seq dir))]
                        (io/delete-file f true)))))))
