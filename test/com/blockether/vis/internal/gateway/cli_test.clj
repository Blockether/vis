(ns com.blockether.vis.internal.gateway.cli-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.cli :as gateway-cli]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.pairing :as pairing]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  gateway-status-staleness-test
  ;; `gateway status` is what gets asked when an update looks like it did nothing,
  ;; so it must answer with the same verdict an attach would reach - never promise
  ;; a replacement the next client would refuse to make.
  (let [ours
        {:version "0.1.40" :build "aaaaaaaaaaaa"}

        note
        (fn [status]
          (#'gateway-cli/stale-daemon-note status ours))

        running
        (fn [m]
          (merge {"status" "running" "managed" true "clients" 0 "running_turns" 0} m))]

    (it "is silent about a daemon this build does not replace"
        (expect (nil? (note (running {"protocol" {"version" "0.1.40" "build" "aaaaaaaaaaaa"}}))))
        (expect (nil? (note (running {"protocol" {"version" "0.1.41" "build" "cccccccccccc"}}))))
        (expect (nil? (note (running {"protocol" {"version" "dev"}})))))))

;; Regression: a daemon running a NEWER release than this build was reported by
;; nothing at all - no bounce, no note, no mismatch screen - so `gateway status` let
;; somebody sit on the older half with no sign that the update was already serving
;; them.
(defdescribe gateway-status-newer-daemon-test
             (let [ours
                   {:version "0.1.40" :build "aaaaaaaaaaaa"}

                   note
                   (fn [status]
                     (#'gateway-cli/newer-daemon-note status ours))

                   running
                   (fn [m]
                     (merge {"status" "running" "managed" true "clients" 0 "running_turns" 0} m))]

               (it "is silent unless the daemon is strictly newer"
                   (expect (nil? (note (running {"protocol" {"version" "0.1.40"}}))))
                   (expect (nil? (note (running {"protocol" {"version" "0.1.39"}}))))
                   (expect (nil? (note (running {"protocol" {"version" "dev"}}))))
                   (expect (nil? (note (running {})))))))

(defdescribe
  gateway-advertise-option-test
  (it "prefers the flag, then VIS_GATEWAY_ADVERTISE, then config, ignoring blanks"
      (expect (= "10.0.0.5" (#'gateway-cli/advertise-option {"advertise" "10.0.0.5"} nil nil)))
      (expect (= "10.0.0.5"
                 (#'gateway-cli/advertise-option
                  {"advertise" "10.0.0.5"}
                  "gateway.example.com"
                  "192.0.2.7")))
      (expect (= "gateway.example.com"
                 (#'gateway-cli/advertise-option {} "gateway.example.com" "192.0.2.7")))
      (expect (= "192.0.2.7" (#'gateway-cli/advertise-option {} nil "192.0.2.7")))
      (expect (= "192.0.2.7" (#'gateway-cli/advertise-option {"advertise" "   "} "" "192.0.2.7")))
      (expect (= "10.0.0.5" (#'gateway-cli/advertise-option {} " 10.0.0.5 " nil)))
      (expect (nil? (#'gateway-cli/advertise-option {} nil nil)))
      (expect (nil? (#'gateway-cli/advertise-option {"advertise" ""} "   " nil)))))

(defdescribe
  gateway-advertise-config-file-test
  (it "takes `gateway: advertise:` off the config file when nothing else names an address"
      (let [dir
            (java.io.File. (System/getProperty "java.io.tmpdir")
                           (str "vis-advertise-" (System/nanoTime)))

            store
            (java.io.File. dir ".vis")

            old-home
            (System/getProperty "user.home")]

        (try (.mkdirs store)
             (spit (java.io.File. store "config.yml") "gateway:\n  advertise: 10.0.0.5\n")
             (System/setProperty "user.home" (.getPath dir))
             (config/invalidate-config-cache!)
             ;; Read from the RAW merged config, never `current-config`: a machine with no
             ;; saved providers still has to pair against the address its network allows.
             (expect (= (or (not-empty (str (System/getenv "VIS_GATEWAY_ADVERTISE"))) "10.0.0.5")
                        (#'gateway-cli/advertise-option {})))
             (finally (System/setProperty "user.home" old-home)
                      (config/invalidate-config-cache!))))))

(defdescribe
  web-command-test
  (it "takes a TCP port and refuses anything else"
      (expect (nil? (#'gateway-cli/web-port nil)))
      (expect (= 8080 (#'gateway-cli/web-port " 8080 ")))
      (doseq [bad ["0" "65536" "http" "-1"]]
        (expect (try (#'gateway-cli/web-port bad)
                     false
                     (catch clojure.lang.ExceptionInfo e
                       (and (:vis/user-error (ex-data e))
                            (str/includes? (ex-message e) "1 to 65535"))))
                bad)))
  (it "refuses --host or --port together with --gateway"
      (with-redefs-fn {#'config/init-cli! (constantly nil)
                       #'gateway-client/remote-gateway
                       (constantly {:host "10.0.0.5" :port 7890 :remote? true})}
        (fn []
          (expect (try (#'gateway-cli/cli-web! {"port" "8080"} [])
                       false
                       (catch clojure.lang.ExceptionInfo e
                         (str/includes? (ex-message e) "--gateway already names one")))))))
  (it "leaves an error it cannot explain unchanged"
      (let [e (ex-info "boom" {:type :gateway/start-failed})]
        (expect (identical? e (#'gateway-cli/web-error e)))))
  (it "binds a phone-reachable host for --pair unless --host or --gateway decides"
      (with-redefs-fn {#'pairing/pair-bind-host (constantly "0.0.0.0")}
        (fn []
          (expect (= "0.0.0.0" (#'gateway-cli/web-host {"pair" true} false)))
          (expect (= "10.0.0.5" (#'gateway-cli/web-host {"pair" true "host" " 10.0.0.5 "} false)))
          (expect (nil? (#'gateway-cli/web-host {"pair" true} true)))
          (expect (nil? (#'gateway-cli/web-host {} false))))))
  (it "prints the gateway pairing block for --pair"
      (let [lines
            (atom [])

            pairs
            (atom [])]

        (with-redefs-fn {#'commandline/stdout! #(swap! lines conj %)
                         #'pairing/candidate-hosts (constantly ["10.0.0.5"])
                         #'gateway-client/pairing-info (constantly {:running? true :token "secret"})
                         #'pairing/print-pairing! #(swap! pairs conj (dissoc % :emit))}
          (fn []
            (#'gateway-cli/web-ready!
             {"no-open" true "pair" true "advertise" "gateway.example.com"}
             {:url "http://127.0.0.1:7890/" :host "0.0.0.0" :port 7890 :remote? false})))
        (expect (= [{:host "0.0.0.0"
                     :port 7890
                     :token "secret"
                     :require-token? true
                     :advertise "gateway.example.com"}]
                   @pairs)))))
