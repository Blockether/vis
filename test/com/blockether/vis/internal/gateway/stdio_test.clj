(ns com.blockether.vis.internal.gateway.stdio-test
  (:require [clojure.string]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [com.blockether.vis.internal.gateway.stdio :as stdio]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.view :as view])
  (:import [java.io StringReader StringWriter]))

(deftest framed-requests-use-the-handler-and-preserve-binary-data
  (let [seen
        (atom nil)

        out
        (StringWriter.)]

    (stdio/serve! (StringReader.
                    (str (wire/json-str {:method "POST" :route "/v1/voice" :content "AP8="}) "\n"))
                  out
                  (fn [request]
                    (reset! seen request)
                    {:status 200
                     :headers {"Content-Type" "application/octet-stream"}
                     :body (byte-array [0 -1])}))
    (is (= :post (:request-method @seen)))
    (is (= "/v1/voice" (:uri @seen)))
    (is (= [0 -1] (vec (.readAllBytes ^java.io.InputStream (:body @seen)))))
    (let [reply (wire/parse-json (second (clojure.string/split-lines (str out))))]
      (is (= 200 (get reply "status")))
      (is (= "AP8=" (get reply "content"))))))

(deftest refuses-non-sdk-and-streaming-routes-without-handler-io
  (doseq [route ["/v1/admin/stop" "/v1/events" "/not-a-route"]]
    (let [out (StringWriter.)
          calls (atom 0)]

      (stdio/serve! (StringReader. (str (wire/json-str {:method "GET" :route route}) "\n"))
                    out
                    (fn [_]
                      (swap! calls inc)))
      (is (zero? @calls))
      (is (= 400
             (get (wire/parse-json (second (clojure.string/split-lines (str out)))) "status"))))))

(deftest stdio-owns-view-bridge-through-eof-and-malformed-input
  ;; Full installed SDK flow previously returned undeliverable: only HTTP installed
  ;; the channel bridge, so an input View never reached the session event journal.
  (doseq [input ["" "not-json\n"]]
    (let [seen (atom [])]
      (with-redefs [view/install! #(swap! seen conj :install)
                    view/uninstall! #(swap! seen conj :uninstall)]

        (try (stdio/serve! (StringReader. input) (StringWriter.) (constantly nil))
             (catch Exception _ nil)))
      (is (= [:install :uninstall] @seen)))))
