(ns com.blockether.vis.internal.gateway.stdio-test
  (:require [clojure.string]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.extension.client :as client]
            [com.blockether.vis.internal.gateway.stdio :as stdio]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.view :as view])
  (:import [java.io StringReader StringWriter]))

(defdescribe framed-requests-use-the-handler-and-preserve-binary-data
             (it "framed requests use the handler and preserve binary data"
                 (let [seen
                       (atom nil)

                       out
                       (StringWriter.)]

                   (stdio/serve! (StringReader. (str (wire/json-str {:method "POST"
                                                                     :route "/v1/voice"
                                                                     :content "AP8="})
                                                     "\n"))
                                 out
                                 (fn [request]
                                   (reset! seen request)
                                   {:status 200
                                    :headers {"Content-Type" "application/octet-stream"}
                                    :body (byte-array [0 -1])}))
                   (expect (= :post (:request-method @seen)))
                   (expect (= "/v1/voice" (:uri @seen)))
                   (expect (= [0 -1] (vec (.readAllBytes ^java.io.InputStream (:body @seen)))))
                   (let [reply (wire/parse-json (second (clojure.string/split-lines (str out))))]
                     (expect (= 200 (get reply "status")))
                     (expect (= "AP8=" (get reply "content")))))))

(defdescribe
  refuses-non-sdk-and-streaming-routes-without-handler-io
  (it "refuses non sdk and streaming routes without handler io"
      (doseq [route ["/v1/admin/stop" "/v1/events" "/not-a-route"]]
        (let [out (StringWriter.)
              calls (atom 0)]

          (stdio/serve! (StringReader. (str (wire/json-str {:method "GET" :route route}) "\n"))
                        out
                        (fn [_]
                          (swap! calls inc)))
          (expect (zero? @calls))
          (expect (= 400
                     (get (wire/parse-json (second (clojure.string/split-lines (str out))))
                          "status")))))))

(defdescribe stdio-owns-view-bridge-through-eof-and-malformed-input
             (it "stdio owns view bridge through eof and malformed input"
                 ;; Full installed SDK flow previously returned undeliverable: only HTTP installed
                 ;; the channel bridge, so an input View never reached the session event journal.
                 (doseq [input ["" "not-json\n"]]
                   (let [seen (atom [])]
                     (with-redefs [view/install! #(swap! seen conj :install)
                                   view/uninstall! #(swap! seen conj :uninstall)]

                       (try (stdio/serve! (StringReader. input) (StringWriter.) (constantly nil))
                            (catch Exception _ nil)))
                     (expect (= [:install :uninstall] @seen))))))

(defdescribe client-header-is-whitelisted-and-owner-detaches-on-eof-or-malformed-input
             (it "client header is whitelisted and owner detaches on eof or malformed input"
                 (doseq [tail ["" "not-json\n"]]
                   (let [seen (atom nil)
                         detached (atom [])
                         frame {:method "GET"
                                :route "/v1/sessions"
                                :headers {"x-vis-client-id" "sdk-owner"
                                          "authorization" "ignored"
                                          "x-arbitrary" "ignored"}}]

                     (with-redefs [client/detach-owner! #(swap! detached conj %)]
                       (try (stdio/serve! (StringReader. (str (wire/json-str frame) "\n" tail))
                                          (StringWriter.)
                                          (fn [request]
                                            (reset! seen (:headers request))
                                            {:status 200 :body {}}))
                            (catch Exception _ nil)))
                     (expect (= "sdk-owner" (get @seen "x-vis-client-id")))
                     (expect (nil? (get @seen "authorization")))
                     (expect (nil? (get @seen "x-arbitrary")))
                     (expect (= ["sdk-owner"] @detached))))))
