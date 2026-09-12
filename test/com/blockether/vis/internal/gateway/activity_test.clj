(ns com.blockether.vis.internal.gateway.activity-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.gateway.server]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as db]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]
            [ring.core.protocols :as ring-protocols])
  (:import [java.io ByteArrayOutputStream IOException]))

(h/use-mem-store!)

(defn- add-operation!
  [store sid aid ctx n]
  (let [inv
        (event/invocation ctx nil)

        details
        {:operation :read-record
         :presenter :generic
         :args [{:path (str "record-" n) :password "private-export-value"}]
         :activity {:headline "Read record"}}]

    (db/db-activity-apply! store sid aid (event/start-event ctx inv details))
    (db/db-activity-apply!
      store
      sid
      aid
      (event/content-event
        ctx
        inv
        details
        {"headline" "Read record"
         "summary" (str "Record " n)
         "content" [{"type" "text" "text" (str "Record " n " " (apply str (repeat 1000 "界")))}]}))
    (db/db-activity-apply! store
                           sid
                           aid
                           (event/terminal-event ctx
                                                 inv
                                                 (assoc details
                                                   :outcome (if (= n 144) :failed :succeeded)
                                                   :started-at-ms (System/currentTimeMillis)
                                                   :error (ex-info "Tail failure" {}))))
    (:invocation-id inv)))

(defn- handler [symbol] (ns-resolve 'com.blockether.vis.internal.gateway.server symbol))

(defdescribe
  activity-history-routes-test
  ;; Regression #212: an embedded page is not a complete copy of a large history.
  (it
    "serves every admitted row, searches the tail, and streams all details"
    (let [store
          (h/store)

          sid
          (h/store-session! store {})

          aid
          (str (random-uuid))

          ctx
          (event/context)

          ids
          (mapv #(add-operation! store sid aid ctx %) (range 145))

          request
          {:path-params {:sid (str sid) :aid aid}}

          page-handler
          (handler 'activity-page-handler)

          export-handler
          (handler 'activity-export-handler)]

      (with-redefs [lp/db-info (constantly store)]
        (let [pages (loop [after 0
                           pages []]

                      (let [response (page-handler (assoc request
                                                     :query-params {"after" (str after)}))
                            page (contract/from-wire (wire/parse-json (:body response)))]

                        (expect (= 200 (:status response)))
                        (expect (some? page))
                        (if-let [next-after (get-in page [:history :next-after])]
                          (recur next-after (conj pages page))
                          (conj pages page))))]
          (expect (= ids (mapv :id (mapcat :rows pages)))))
        (let [response (page-handler (assoc request :query-params {"q" "Record 144 "}))
              page (contract/from-wire (wire/parse-json (:body response)))]

          (expect (= [(last ids)] (mapv :id (:rows page)))))
        (let [response (export-handler request)]
          (expect (= 200 (:status response)))
          (expect (= "text/plain; charset=utf-8" (get-in response [:headers "Content-Type"])))
          (with-open [out (ByteArrayOutputStream.)]
            (ring-protocols/write-body-to-stream (:body response) response out)
            (let [text (.toString out "UTF-8")]
              (expect (> (.size out) 65536))
              (expect (= 1 (count (re-seq #"ACTIVITY" text))))
              (expect (not (str/includes? text "private-export-value")))
              (doseq [n (range 145)]
                (expect (str/includes? text (str "Record " n " ")))))))
        (doseq [invalid [{"after" "no"} {"after" "-1"} {"limit" "0"} {"limit" "33"}
                         {"q" (apply str (repeat 513 "x"))} {"revision" "no"}]]
          (expect (= 400 (:status (page-handler (assoc request :query-params invalid))))))
        (expect (= 409 (:status (export-handler (assoc request :query-params {"revision" "0"})))))
        (doseq [route [page-handler export-handler]]
          (expect (= 404
                     (:status (route (assoc-in request [:path-params :sid] (str (random-uuid)))))))
          (expect (= 404 (:status (route (assoc-in request [:path-params :aid] "bad")))))))))
  (it "marks and aborts a streaming export instead of silently mixing revisions"
      (let [store
            (h/store)

            sid
            (h/store-session! store {})

            aid
            (str (random-uuid))

            ctx
            (event/context)]

        (doseq [n (range 33)]
          (add-operation! store sid aid ctx n))
        (with-redefs [lp/db-info (constantly store)]
          (let [response ((handler 'activity-export-handler)
                           {:path-params {:sid (str sid) :aid aid}})]
            (add-operation! store sid aid ctx 33)
            (with-open [out (ByteArrayOutputStream.)]
              (expect (= "Activity changed during export"
                         (try (ring-protocols/write-body-to-stream (:body response) response out)
                              nil
                              (catch IOException error (ex-message error)))))
              (expect (str/includes? (.toString out "UTF-8") "INCOMPLETE EXPORT"))
              (expect (not (str/includes? (.toString out "UTF-8") "Record 33 ")))))))))
