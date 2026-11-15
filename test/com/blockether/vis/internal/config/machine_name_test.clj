(ns com.blockether.vis.internal.config.machine-name-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.gateway.server.settings :as settings-api]
            [com.blockether.vis.internal.gateway.server.web :as web]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]))

(defn- with-store!
  "Run `f` with an empty Vis home. `f` gets the home folder."
  [f]
  (let [dir (.toFile (Files/createTempDirectory "vis-machine-name"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0)))]
    (try (with-redefs [config/config-dir (constantly (.getPath dir))
                       lp/db-info (constantly {})]

           (f dir))
         (finally (doseq [file (reverse (file-seq dir))]
                    (.delete ^java.io.File file))))))

(defn- with-env!
  "Run `f` while the environment variable VIS_MACHINE_NAME holds `value`."
  [value f]
  (with-redefs [util/env-val (fn [k]
                               (when (= k "VIS_MACHINE_NAME") (util/non-blank value)))]
    (f)))

(defn- answer [response] (wire/parse-json (:body response)))

(defn- write!
  [params]
  (#'settings-api/set-setting-handler {:query-params (assoc params "id" "machine_name")}))

(defn- setting [] (answer (#'settings-api/get-setting-handler {:path-params {:id "machine_name"}})))

(defn- health
  []
  (with-redefs-fn {#'server/ensure-self-registered! (constantly nil)}
    #(answer (#'server/health-handler {:headers {}}))))

(defn- grant
  []
  (let [{:keys [code]} (web/mint-launch!)]
    (answer (#'web/redeem-handler
             {:body (io/input-stream (.getBytes (str "{\"code\":\"" code "\"}") "UTF-8"))}))))

(defdescribe
  machine-name-sources
  (it "takes the environment before the configuration, and none without both"
      (with-store!
        (fn [dir]
          (with-env! nil
                     (fn []
                       (expect (nil? (config/machine-name)))
                       (spit (io/file dir "config.yml") "machine_name: Written by hand\n")
                       (expect (= "Written by hand" (config/machine-name)))
                       (spit (io/file dir "state.yml") "machine_name: '  Studio  '\n")
                       (expect (= "Studio" (config/machine-name)))))
          (with-env! "Lambda EU" #(expect (= "Lambda EU" (config/machine-name))))
          (with-env! "   " #(expect (= "Studio" (config/machine-name))))
          (with-env! (apply str (repeat 65 "a")) #(expect (= "Studio" (config/machine-name))))
          (with-env! "two\nlines" #(expect (= "Studio" (config/machine-name))))))))

(defdescribe
  machine-name-setting
  (it "saves, replaces and removes the name"
      (with-store!
        (fn [dir]
          (with-env!
            nil
            (fn []
              (spit (io/file dir "state.yml") "toggles:\n  shell: true\n")
              (expect (= "" (get (setting) "value")))
              (expect (= 200 (:status (write! {"action" "value" "value" "  Studio  "}))))
              (expect (= {"toggles" {"shell" true} "machine_name" "Studio"}
                         (config/load-global-config-raw)))
              (expect (= "Studio" (get (setting) "value")))
              (expect (document/valid-json? "gateway" "setting" (setting)))
              (expect (some #(= "machine_name" (get % "id"))
                            (tree-seq coll?
                                      #(if (map? %) (vals %) %)
                                      (answer (#'settings-api/list-settings-handler {})))))
              (doseq [value ["two\nlines" 42 (apply str (repeat 65 "a"))]]
                (expect (= 400 (:status (write! {"action" "value" "value" value})))))
              (expect (= 400 (:status (write! {"action" "toggle"}))))
              (expect (= 400 (:status (write! {"action" "value" "value" "X" "scope" "project"}))))
              (expect (= "Studio" (config/machine-name)))
              (expect (= 200 (:status (write! {"action" "value" "value" "  "}))))
              (expect (= {"toggles" {"shell" true}} (config/load-global-config-raw)))
              (write! {"action" "value" "value" "Again"})
              (expect (= 200 (:status (write! {"action" "inherit"}))))
              (expect (nil? (config/machine-name))))))))
  (it "shows the environment name and keeps a saved name for later"
      (with-store! (fn [_]
                     (with-env!
                       "Lambda EU"
                       (fn []
                         (expect (= 200 (:status (write! {"action" "value" "value" "Studio"}))))
                         (let [row (setting)]
                           (expect (= "Lambda EU" (get row "value")))
                           (expect (= "Studio" (get row "own_value")))
                           (expect (re-find #"VIS_MACHINE_NAME" (get row "description"))))))
                     (with-env! nil #(expect (= "Studio" (get (setting) "value"))))))))

(defdescribe
  machine-name-report
  (it "reports the name beside the gateway id, and omits it without a name"
      (with-store!
        (fn [_]
          (reset! instance/server-state {:host "127.0.0.1" :port 7777 :token "secret"})
          (try (with-env! nil
                          (fn []
                            (let [body (health)]
                              (expect (document/valid-json? "gateway" "health" body))
                              (expect (not (contains? body "machine_name"))))
                            (let [body (grant)]
                              (expect (document/valid-json? "gateway" "web_launch_grant" body))
                              (expect (not (contains? body "machine_name"))))
                            (config/set-machine-name! "Studio")
                            (let [body (health)]
                              (expect (document/valid-json? "gateway" "health" body))
                              (expect (= "Studio" (get body "machine_name")))
                              (expect (string? (get body "id"))))
                            (let [body (grant)]
                              (expect (document/valid-json? "gateway" "web_launch_grant" body))
                              (expect (= "Studio" (get body "machine_name"))))))
               (with-env! "Lambda EU" #(expect (= "Lambda EU" (get (health) "machine_name"))))
               (finally (reset! instance/server-state nil))))))
  (it "refuses a report with an invalid name"
      (let [body {"status" "ok"
                  "id" "abc"
                  "protocol" (wire/parse-json (wire/json-str (:protocol (#'server/status-map))))}]
        (expect (document/valid-json? "gateway" "health" body))
        (expect (not (document/valid-json? "gateway" "health" (assoc body "machine_name" ""))))
        (expect (not (document/valid-json? "gateway"
                                           "health"
                                           (assoc body
                                             "machine_name" (apply str (repeat 65 "a")))))))))
