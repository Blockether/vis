(ns com.blockether.vis.internal.gateway.cli-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.cli :as gateway-cli]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
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

    (it "names a dev build by its commit, because \"dev\" alone names no code"
        (expect (= "dev (abc123abc123)"
                   (#'gateway-cli/build-label {:version "dev" :build "abc123abc123"})))
        (expect (= "0.1.40" (#'gateway-cli/build-label {:version "0.1.40" :build "abc123abc123"}))))
    (it "says what picks the new build up when nothing is using the old daemon"
        (let [s (note (running {"protocol" {"version" "0.1.39" "build" "bbbbbbbbbbbb"}}))]
          (expect (str/includes? s "this build is 0.1.40"))
          (expect (str/includes? s "next session starts on it"))))
    (it "counts what is holding the old daemon instead of promising a replacement"
        (let [s (note (running {"clients" 2 "running_turns" 1 "protocol" {"version" "0.1.39"}}))]
          (expect (str/includes? s "2 clients"))
          (expect (str/includes? s "1 running turn"))
          (expect (not (str/includes? s "next session")))))
    (it "hands a user-owned daemon back to whoever started it"
        (let [s (note (running {"managed" false "pid" 4242 "protocol" {"version" "0.1.39"}}))]
          (expect (str/includes? s "user-owned"))
          (expect (str/includes? s "4242"))))
    (it "counts nothing it could not read"
        (let [s (note (running {"clients" :two "protocol" {"version" "0.1.39"}}))]
          (expect (str/includes? s "no longer in use"))))
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

               (it "names the newer release the running daemon already serves"
                   (let [s (note (running {"protocol" {"version" "0.1.41"
                                                       "build" "cccccccccccc"}}))]
                     (expect (str/includes? s "0.1.41"))
                     (expect (str/includes? s "this build is 0.1.40"))
                     (expect (str/includes? s "vis-agent update"))))
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

;; Regression (reported: `vis-agent gateway stop` printed
;; `gateway stop requested: {"stopping" true, "status" {...}}`): the daemon's
;; acknowledgement has to read as a sentence, never as a wire map.
(defdescribe
  cli-gateway-stop-vocabulary-test
  (it "says the gateway is stopping and what that releases"
      (let [lines (atom [])]
        (with-redefs-fn
          {#'commandline/stdout! #(swap! lines conj %)
           #'config/init-cli! (constantly nil)
           #'gateway-client/stop-daemon!
           (constantly {:stopping true :status "stopping" :pid 32379 :clients 4 :running-turns 0})}
          (fn []
            (#'gateway-cli/cli-gateway-stop! {} [])))
        (expect (= ["gateway stopping (pid 32379) - releasing 4 clients"] @lines))))
  (it "counts a drained turn beside the clients it releases"
      (let [lines (atom [])]
        (with-redefs-fn {#'commandline/stdout! #(swap! lines conj %)
                         #'config/init-cli! (constantly nil)
                         #'gateway-client/stop-daemon!
                         (constantly
                           {:stopping true :status "stopping" :clients 1 :running-turns 2})}
          (fn []
            (#'gateway-cli/cli-gateway-stop! {} [])))
        (expect (= ["gateway stopping - releasing 1 client, draining 2 running turns"] @lines))))
  (it "never answers a human with a map"
      (let [lines (atom [])]
        (with-redefs-fn {#'commandline/stdout! #(swap! lines conj %)
                         #'config/init-cli! (constantly nil)
                         #'gateway-client/stop-daemon! (constantly {})}
          (fn []
            (#'gateway-cli/cli-gateway-stop! {} [])))
        (expect (str/includes? (first @lines) "no final state"))
        (expect (not (str/includes? (first @lines) "{"))))))
