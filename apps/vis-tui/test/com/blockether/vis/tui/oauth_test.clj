(ns com.blockether.vis.tui.oauth-test
  (:require [babashka.http-client :as http]
            [clojure.string]
            [com.blockether.vis.tui.oauth :as oauth]
            [com.blockether.vis.tui.external-opener :as opener]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.net ServerSocket URLEncoder]
           [java.nio.charset StandardCharsets]))

(defn- flow
  []
  (let [port
        (with-open [socket (ServerSocket. 0)]
          (.getLocalPort socket))

        redirect
        (str "http://127.0.0.1:" port "/callback")]

    {"flow_id" "client-flow"
     "redirect_uri" redirect
     "url" (str "https://gateway.example.com/authorize?state=test-state&redirect_uri="
                (URLEncoder/encode redirect StandardCharsets/UTF_8))}))

(defn- wait-until
  [_ _ done?]
  (loop [tries 200]
    (cond (done?) true
          (zero? tries) false
          :else (do (Thread/sleep 10) (recur (dec tries))))))

(defdescribe
  browser-return-test
  (it "receives on the browser machine and forwards exactly once to the paired remote gateway"
      (let [started
            (flow)

            seen
            (atom [])

            cancelled
            (atom 0)

            input
            (str (get started "redirect_uri") "?code=test-code&state=test-state")]

        (with-redefs [opener/open! (fn [_]
                                     (expect (= 400
                                                (:status (http/get (str
                                                                     (get started "redirect_uri")
                                                                     "?code=test-code&state=other")
                                                                   {:throw false}))))
                                     (expect (= 200 (:status (http/get input)))))]
          (expect (= {"status" "ok"}
                     (oauth/login! {:wait! wait-until}
                                   "Provider"
                                   started
                                   #(do (swap! seen conj %) {"status" "ok"})
                                   (constantly {"status" "pending"})
                                   #(swap! cancelled inc))))
          (expect (= [input] @seen))
          (expect (zero? @cancelled)))))
  (it "Escape cancels the gateway flow and closes the local port"
      (let [started
            (flow)

            cancelled
            (atom 0)

            completed
            (atom 0)]

        (with-redefs [opener/open! (constantly nil)]
          (expect (nil? (oauth/login! {:wait! (fn [& _]
                                                false)}
                                      "MCP"
                                      started
                                      (fn [_]
                                        (swap! completed inc))
                                      (constantly {"status" "pending"})
                                      #(swap! cancelled inc)))))
        (expect (= 1 @cancelled))
        (expect (zero? @completed))
        (expect (try (http/get (str (get started "redirect_uri") "?code=a&state=test-state")
                               {:timeout 500})
                     false
                     (catch Exception _ true)))))
  (it "does not open a receiver for a non-loopback destination"
      (let [completed
            (atom [])

            started
            {"url" "https://gateway.example.com/authorize"}]

        (with-redefs [opener/open! (constantly nil)]
          (expect (= {"status" "ok"}
                     (oauth/login! {:read! (fn [& _]
                                             "manual-code")}
                                   "Provider"
                                   started
                                   #(do (swap! completed conj %) {"status" "ok"})
                                   (constantly {"status" "pending"})
                                   (constantly nil))))
          (expect (= ["manual-code"] @completed))))))

(defdescribe
  device-return-test
  (it
    "shows the code while polling immediately, without asking for a pasted URL or another confirmation"
    (let [opened
          (atom nil)

          lines
          (atom [])

          polls
          (atom 0)

          started
          {"kind" "device"
           "url" "https://gateway.example.com/device"
           "user_code" "ABCD-EFGH"
           "interval_ms" 1}]

      (with-redefs [opener/open! #(reset! opened %)]
        (expect (= {"status" "ok"}
                   (oauth/login! {:wait! (fn [title line done?]
                                           (swap! lines conj (line))
                                           (wait-until title line done?))
                                  :read! (fn [& _]
                                           (throw (ex-info "Device flow asked for a paste" {})))}
                                 "Device"
                                 started
                                 (fn [_]
                                   (throw (ex-info "Device flow exchanged on the client" {})))
                                 #(do (swap! polls inc) {"status" "ok"})
                                 (fn []
                                   (throw (ex-info "Successful flow was cancelled" {}))))))
        (expect (= "https://gateway.example.com/device" @opened))
        (expect (= 1 @polls))
        (expect (clojure.string/includes? (first @lines) "ABCD-EFGH")))))
  (it "bounds device waiting by the gateway expiry and cancels on timeout"
      (let [cancelled (atom 0)]
        (with-redefs [opener/open! (constantly nil)]
          (expect (= "error"
                     (get (oauth/login! {:wait! wait-until}
                                        "Device"
                                        {"kind" "device" "expires_at" 0}
                                        (constantly nil)
                                        (constantly {"status" "pending"})
                                        #(swap! cancelled inc))
                          "status")))
          (expect (= 1 @cancelled))))))
