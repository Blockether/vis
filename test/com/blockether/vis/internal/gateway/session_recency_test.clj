(ns com.blockether.vis.internal.gateway.session-recency-test
  (:require [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.server.sessions :as sessions]
            [com.blockether.vis.internal.gateway.state :as state]
            [lazytest.core :refer [defdescribe describe it expect]])
  (:import [java.io ByteArrayInputStream]
           [java.util Date]))

(defdescribe session-open-recency-test
             (describe
               "explicit session openings"
               (it "puts an older conversation opened most recently before newer content"
                   (let [rows
                         [{"id" "newer" "modified_at" (Date. 4000)}
                          {"id" "opened" "modified_at" (Date. 1000) "last_opened_at" (Date. 5000)}]]
                     (expect (= ["opened" "newer"]
                                (mapv #(get % "id") (#'state/order-session-summaries rows))))))
               (it "uses durable opening time for the database ranking"
                   (expect (= 5000
                              (#'state/record-recency-ms
                               {:created-at (Date. 1000) :last-opened-at (Date. 5000)}
                               {:latest-turn-at (Date. 2000)}))))
               (it "allows later conversation activity to become more recent than an opening"
                   (expect (= 6000
                              (#'state/record-recency-ms
                               {:created-at (Date. 1000) :last-opened-at (Date. 5000)}
                               {:latest-turn-at (Date. 6000)}))))))

(defn- patch-opening
  [sid opened]
  (#'sessions/patch-session-handler
   {:request-method :patch
    :path-params {:sid (str sid)}
    :body (ByteArrayInputStream. (.getBytes (wire/json-str {:opened opened}) "UTF-8"))}))

(defdescribe
  session-opening-route-test
  (it "records an explicit opening and returns the allocated timestamp"
      (let [sid
            (random-uuid)

            asked
            (atom [])]

        (with-redefs [state/mark-session-opened! (fn [selected]
                                                   (swap! asked conj selected)
                                                   {"id" (str selected)
                                                    "last_opened_at" (Date. 5000)})]
          (let [response (patch-opening sid true)]
            (expect (= 200 (:status response)))
            (expect (= [sid] @asked))
            (expect (= 5000 (get (wire/parse-json (:body response)) "last_opened_at")))))))
  (it "rejects anything other than an explicit true opening without touching recency"
      (with-redefs [state/mark-session-opened! (fn [_]
                                                 (throw (ex-info "Unexpected opening" {})))]
        (doseq [value [false nil "true" 1]]
          (expect (= 400 (:status (patch-opening (random-uuid) value)))))))
  (it "reports a missing session instead of claiming to record its opening"
      (with-redefs [state/mark-session-opened! (constantly nil)]
        (expect (= 404 (:status (patch-opening (random-uuid) true)))))))
