(ns com.blockether.vis.internal.gateway.session-recency-test
  (:require [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.server.sessions :as sessions]
            [com.blockether.vis.internal.gateway.state :as state]
            [lazytest.core :refer [defdescribe describe it expect]])
  (:import [java.io ByteArrayInputStream]
           [java.util Date]))

(defn- clock-ms
  "Epoch ms of the `:latest-turn-at` clock in turn stats `st`, or nil."
  [st]
  (some-> ^Date (:latest-turn-at st)
          .getTime))

(defn- with-registry
  "Run `f` with the gateway registry replaced by `entries`."
  [entries f]
  (with-redefs-fn {#'state/registry (atom entries)} f))

;; Regression: opening a session from the list moved it to the top of the recents.
;; Recency must follow the newest sent message alone.
(defdescribe
  session-recency-test
  (describe "conversation activity"
            (it "orders sessions by their newest message, not by an opening"
                (let [rows
                      [{"id" "newer" "modified_at" (Date. 4000)}
                       {"id" "opened" "modified_at" (Date. 1000) "last_opened_at" (Date. 5000)}]]
                  (expect (= ["newer" "opened"]
                             (mapv #(get % "id") (#'state/order-session-summaries rows))))))
            (it "ranks a stored session by its newest turn, else by its creation"
                (expect (= 2000
                           (#'state/record-recency-ms
                            {:created-at (Date. 1000) :last-opened-at (Date. 5000)}
                            {:latest-turn-at (Date. 2000)})))
                (expect (= 1000 (#'state/record-recency-ms {:created-at (Date. 1000)} nil)))))
  (describe
    "a sent message"
    (it "moves the session at the send, before the worker stores its turn row"
        (with-registry {"running" {:turns {"t1" {:status "running" :started_at 9000}}}
                        "queued" {:turns {"t0" {:status "completed" :started_at 1500}
                                          "t2" {:status "queued" :queued_at 7000}}}
                        "settled" {:turns {"t3" {:status "completed" :started_at 9999}}}}
                       (fn []
                         (let [stats (#'state/with-sent-turns
                                      {"running" {:latest-turn-at (Date. 2000) :turn-count 3}
                                       "settled" {:latest-turn-at (Date. 3000) :turn-count 1}})]
                           (expect (= 9000 (clock-ms (get stats "running"))))
                           (expect (= 3 (:turn-count (get stats "running"))))
                           (expect (= 7000 (clock-ms (get stats "queued"))))
                           (expect (= 7000
                                      (#'state/record-recency-ms
                                       {:created-at (Date. 1000)}
                                       (get stats "queued"))))
                           (expect (= 3000 (clock-ms (get stats "settled"))))))))
    (it "keeps the send time of a queued turn after it starts"
        (expect (= 7000
                   (#'state/sent-turn-ms
                    {:turns {"t2" {:status "running" :queued_at 7000 :started_at 8000}}}))))
    (it "keeps a stored turn that is newer than the send"
        (expect (= 9000 (clock-ms (#'state/with-sent-turn {:latest-turn-at (Date. 9000)} 7000)))))))

(defn- patch-session
  [sid body]
  (#'sessions/patch-session-handler
   {:request-method :patch
    :path-params {:sid (str sid)}
    :body (ByteArrayInputStream. (.getBytes (wire/json-str body) "UTF-8"))}))

(defdescribe session-opening-route-test
             (it "refuses an opening write, so viewing a session cannot change its order"
                 (expect (= 400 (:status (patch-session (random-uuid) {:opened true}))))))
