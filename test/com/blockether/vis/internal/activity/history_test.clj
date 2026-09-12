(ns com.blockether.vis.internal.activity.history-test
  (:require [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.internal.persistance.core :as db]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defn- write-operation!
  [store sid aid ctx n]
  (let [inv
        (event/invocation ctx nil)

        details
        {:operation :read-record
         :presenter :generic
         :args [{:path (str "record-" n) :password "private-test-value"}]
         :activity {:headline "Read record"}}

        content
        {"headline" "Read record"
         "summary" (str "Record " n)
         "content" [{"type" "text" "text" (str "record " n " " (apply str (repeat 4000 "界")))}]}]

    (db/db-activity-apply! store sid aid (event/start-event ctx inv details))
    (db/db-activity-apply! store sid aid (event/content-event ctx inv details content))
    (db/db-activity-apply! store
                           sid
                           aid
                           (event/terminal-event ctx
                                                 inv
                                                 (assoc details
                                                   :outcome (cond (= n 298) :failed
                                                                  (= n 299) :cancelled
                                                                  :else :succeeded)
                                                   :started-at-ms (System/currentTimeMillis)
                                                   :result (str "Record " n)
                                                   :error (ex-info (str "Outcome " n) {}))))
    (:invocation-id inv)))

(defdescribe
  durable-history-test
  ;; Regression #212: pages bound transport, not retained rows or detail.
  (it
    "reads all 300 operations and multi-megabyte details in bounded ordered pages"
    (let [store
          (h/store)

          sid
          (h/store-session! store {})

          aid
          (str (random-uuid))

          ctx
          (event/context)

          ids
          (mapv #(write-operation! store sid aid ctx %) (range 300))

          pages
          (loop [after
                 0

                 pages
                 []]

            (let [page (db/db-activity-page store sid aid {:after after})]
              (if-let [next-after (get-in page [:history :next-after])]
                (recur next-after (conj pages page))
                (conj pages page))))

          rows
          (mapcat :rows pages)]

      (expect (= ids (mapv :id rows)))
      (expect (= 300 (count rows)))
      (expect (every? contract/valid-projection? pages))
      (expect (every? #(<= (activity/byte-size %) 1048576) pages))
      (expect (every? #(= 300 (get-in % [:history :total])) pages))
      (expect (every? #(= 900 (get-in % [:history :revision])) pages))
      (expect (every? #(zero? (get-in % [:omitted :rows])) pages))
      (expect (every? #(> (count (get-in % [:presentation "content" 0 "text"])) 4000) rows))
      (expect (= ["failed" "cancelled"] (mapv :state (take-last 2 rows))))
      (expect (nil? (db/db-activity-page store (random-uuid) aid {})))
      (expect (empty? (:rows (db/db-activity-page store sid aid {:q "private-test-value"}))))
      (expect (= [(last ids)]
                 (mapv :id (:rows (db/db-activity-page store sid aid {:q "Outcome 299"})))))))
  (it
    "validates lifecycle durably and settles hidden live calls once"
    (let [store
          (h/store)

          sid
          (h/store-session! store {})

          aid
          (str (random-uuid))

          ctx
          (event/context)

          inv
          (event/invocation ctx nil)

          start
          (event/start-event ctx
                             inv
                             {:operation :cat
                              :presenter :generic
                              :activity {:headline "Read file" :show-start false}})]

      (db/db-activity-apply! store sid aid start)
      (expect (empty? (:rows (db/db-activity-page store sid aid {}))))
      (expect (= :activity/lifecycle
                 (try (db/db-activity-apply! store sid aid start)
                      nil
                      (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
      (db/db-activity-settle! store aid :cancelled "Stopped")
      (let [page (db/db-activity-page store sid aid {})]
        (expect (= ["cancelled"] (mapv :state (:rows page))))
        (expect (= 0 (get-in page [:counts :running])))
        (db/db-activity-settle! store aid :failed "Late failure")
        (expect (= page (db/db-activity-page store sid aid {})))))))

(defdescribe
  byte-bounded-grouped-pages-test
  ;; Regression #212: a page ends earlier rather than shortening any retained detail.
  (it
    "pages large grouped invocations losslessly, including tail failure and cancellation"
    (let [store
          (h/store)

          sid
          (h/store-session! store {})

          aid
          (str (random-uuid))

          ctx
          (event/context)

          text
          (apply str (repeat 8100 "x"))

          ids
          (mapv
            (fn [n]
              (let [inv
                    (event/invocation ctx nil)

                    details
                    {:operation :read-record
                     :presenter :observation
                     :group-token "large-batch"
                     :activity {:headline "Read record"}}]

                (db/db-activity-apply! store sid aid (event/start-event ctx inv details))
                (db/db-activity-apply! store
                                       sid
                                       aid
                                       (event/content-event
                                         ctx
                                         inv
                                         details
                                         {"headline" "Read record"
                                          "summary" (str "Record " n)
                                          "content" (vec (repeat 4 {"type" "text" "text" text}))}))
                (db/db-activity-apply! store
                                       sid
                                       aid
                                       (event/terminal-event ctx
                                                             inv
                                                             (assoc details
                                                               :outcome (case n
                                                                          68
                                                                          :failed

                                                                          69
                                                                          :cancelled

                                                                          :succeeded)
                                                               :started-at-ms
                                                               (System/currentTimeMillis))))
                (:invocation-id inv)))
            (range 70))

          pages
          (loop [after
                 0

                 pages
                 []]

            (let [page (db/db-activity-page store sid aid {:after after})]
              (if-let [cursor (get-in page [:history :next-after])]
                (recur cursor (conj pages page))
                (conj pages page))))

          leaves
          (fn [page]
            (filter #(empty? (:children %))
                    (mapcat #(tree-seq (comp seq :children) :children %) (:rows page))))

          rows
          (mapcat leaves pages)]

      (expect (< (count (leaves (first pages))) 32))
      (expect (seq (:children (first (:rows (first pages))))))
      (expect (= ids (mapv :id rows)))
      (expect (every? #(= (repeat 4 text)
                          (map (fn [block]
                                 (get block "text"))
                               (get-in % [:presentation "content"])))
                      rows))
      (expect (= ["failed" "cancelled"] (mapv :state (take-last 2 rows))))
      (expect (every? contract/valid-projection? pages))
      (expect (every? #(<= (activity/byte-size %) 1048576) pages))
      (expect (= (first pages) (db/db-activity-page store sid aid {}))))))
