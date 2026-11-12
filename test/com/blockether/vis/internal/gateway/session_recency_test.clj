(ns com.blockether.vis.internal.gateway.session-recency-test
  (:require [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.server.sessions :as sessions]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [lazytest.core :refer [defdescribe describe it expect]])
  (:import [java.io ByteArrayInputStream]
           [java.util Date]))

;; Regression: opening a session from the list moved it to the top of the recents.
;; Recency must follow the newest agent reply alone.
(defdescribe
  session-recency-test
  (describe "conversation activity"
            (it "orders sessions by their newest message, not by an opening"
                (let [rows
                      [{"id" "newer" "modified_at" (Date. 4000)}
                       {"id" "opened" "modified_at" (Date. 1000) "last_opened_at" (Date. 5000)}]]
                  (expect (= ["newer" "opened"]
                             (mapv #(get % "id") (#'state/order-session-summaries rows))))))
            (it "ranks a stored session by its newest agent reply, else by its creation"
                (expect (= 2000
                           (#'state/record-recency-ms
                            {:created-at (Date. 1000) :last-opened-at (Date. 5000)}
                            {:latest-turn-at (Date. 2000)})))
                (expect (= 1000 (#'state/record-recency-ms {:created-at (Date. 1000)} nil)))))
  ;; Regression, user report (paraphrased: "sort by the newest AGENT REPLY, not by our
  ;; own interaction"): a queued message lifted its session over one the agent had
  ;; answered later.
  (describe "a sent message"
            (it "leaves the session where its newest agent reply put it"
                (with-redefs [state/registry
                              (atom {"sent" {:turns {"t1" {:status "queued" :queued_at 9000}}}})

                              lp/db-info
                              (constantly nil)

                              persistance/db-session-turn-stats
                              (constantly nil)

                              lp/by-channel
                              (constantly [{:id "sent" :title "sent" :created-at 3000}
                                           {:id "answered" :title "answered" :created-at 5000}])

                              bus/live-turns
                              (constantly {})

                              bus/waiting-requests
                              (constantly {})

                              state/soul
                              (fn [sid]
                                {"id" (str sid)})]

                  (expect (= ["answered" "sent"]
                             (mapv #(get % "id")
                                   (:sessions (state/list-sessions-page :all {})))))))))

(defn- patch-session
  [sid body]
  (#'sessions/patch-session-handler
   {:request-method :patch
    :path-params {:sid (str sid)}
    :body (ByteArrayInputStream. (.getBytes (wire/json-str body) "UTF-8"))}))

(defdescribe session-opening-route-test
             (it "refuses an opening write, so viewing a session cannot change its order"
                 (expect (= 400 (:status (patch-session (random-uuid) {:opened true}))))))
