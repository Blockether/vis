(ns com.blockether.vis.internal.activity.history-integration-test
  (:require [babashka.fs :as fs]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as db]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defn- emit-operation!
  [ctx n]
  (let [inv
        (event/invocation ctx nil)

        details
        {:operation :read-record
         :presenter :generic
         :args [{:path (str "record-" n)}]
         :activity {:headline "Read record" :show-start false}}]

    (extension/*tool-event-sink* (event/start-event ctx inv details))
    (extension/*tool-event-sink*
      (event/content-event
        ctx
        inv
        details
        {"headline" "Read record"
         "summary" (str "Record " n)
         "content" [{"type" "text" "text" (str "Record " n " " (apply str (repeat 2000 "界")))}]}))
    (extension/*tool-event-sink* (event/terminal-event ctx
                                                       inv
                                                       (assoc details
                                                         :outcome :succeeded
                                                         :started-at-ms
                                                         (System/currentTimeMillis))))
    (:invocation-id inv)))

(defn- all-rows
  [store sid aid]
  (loop [after
         0

         rows
         []]

    (let [page
          (db/db-activity-page store sid aid {:after after})

          rows
          (into rows (:rows page))]

      (if-let [next-after (get-in page [:history :next-after])]
        (recur next-after rows)
        rows))))

(defdescribe
  durable-python-activity-test
  ;; Regression #212: the Python collector and saved form used to discard rows/details.
  (it
    "keeps the evaluation collector bounded and stores complete history outside the form"
    (let [store
          (h/store)

          sid
          (h/store-session! store {})

          ids
          (atom [])

          snapshots
          (atom [])]

      (tpc/with-own
        [pc {}]
        (with-redefs [event/collector
                      (fn []
                        (throw (ex-info "Unbounded collector used" {})))

                      env/run-python-block
                      (fn [_ _ _]
                        (let [ctx (event/context)]
                          (reset! ids (mapv #(emit-operation! ctx %) (range 300))))
                        {:stdout "done"})]

          (let [result
                (#'lp/run-python-code
                 pc
                 "pass"
                 :env
                 {:db-info store :session-id sid :activity/on-snapshot #(swap! snapshots conj %)})

                page
                (:activity result)

                rows
                (all-rows store sid (get-in page [:history :id]))]

            (expect (nil? (:error result)))
            (expect (= "done" (:stdout result)))
            (expect (= 32 (count (:rows page))))
            (expect (= 300 (get-in page [:history :total])))
            (expect (= @ids (mapv :id rows)))
            (expect (every? #(> (count (get-in % [:presentation "content" 0 "text"])) 2000) rows))
            (expect (every? contract/valid-projection? (conj @snapshots page)))
            (expect (every? #(<= (activity/byte-size %) 1048576) (conj @snapshots page))))))))
  (it "records actual Python shim calls beyond the old 128 row limit"
      (let [store
            (h/store)

            sid
            (h/store-session! store {})]

        (tpc/with-own [pc {}]
                      (let [result
                            (#'lp/run-python-code
                             pc
                             "for i in range(160):\n    ls('resources/vis-shims')\nprint('done')"
                             :env
                             {:db-info store :session-id sid})

                            page
                            (:activity result)

                            rows
                            (all-rows store sid (get-in page [:history :id]))]

                        (expect (nil? (:error result)))
                        (expect (= "done\n" (:stdout result)))
                        (expect (= 160 (count rows)))
                        (expect (= 160 (get-in page [:counts :succeeded])))
                        (expect (zero? (get-in page [:omitted :rows]))))))))

(defdescribe
  activity-disk-reopen-test
  (it
    "keeps every page and detail after disposing and reopening the disk store"
    (let [dir
          (fs/create-temp-dir {:prefix "vis-activity-reopen-"})

          sid
          (atom nil)

          aid
          (atom nil)

          original
          (atom nil)]

      (try (let [store (vis/db-create-connection! (str dir))]
             (try (reset! sid (h/store-session! store {}))
                  (tpc/with-own
                    [pc {}]
                    (with-redefs [env/run-python-block (fn [_ _ _]
                                                         (let [ctx (event/context)]
                                                           (doseq [n (range 145)]
                                                             (emit-operation! ctx n)))
                                                         {:stdout "saved"})]
                      (let [result
                            (#'lp/run-python-code pc "pass" :env {:db-info store :session-id @sid})]
                        (expect (nil? (:error result)))
                        (reset! aid (get-in result [:activity :history :id]))
                        (reset! original (all-rows store @sid @aid)))))
                  (finally (vis/db-dispose-connection! store))))
           (let [reopened (vis/db-create-connection! (str dir))]
             (try (expect (= 145 (count @original)))
                  (expect (= @original (all-rows reopened @sid @aid)))
                  (finally (vis/db-dispose-connection! reopened))))
           (finally (fs/delete-tree dir))))))

(defdescribe
  activity-backpressure-test
  ;; Regression #212: a durable store must not move an unbounded history into its dispatch queue.
  (it "backs off producers rather than dropping events or accumulating an unbounded queue"
      (let [[dispatch! shutdown!]
            (#'lp/serial-activity-dispatcher)

            entered
            (promise)

            release
            (promise)

            crossed-bound
            (promise)]

        (dispatch! (fn []
                     (deliver entered true)
                     @release))
        (let [producer (future (dotimes [n 256]
                                 (dispatch! (fn []
                                              nil))
                                 (when (= n 64) (deliver crossed-bound true))))]
          (try (expect (deref entered 2000 false))
               (expect (nil? (deref crossed-bound 500 nil)))
               (finally (deliver release true) (deref producer 5000 nil) (shutdown!))))))
  (it "reports a durable write failure instead of returning a successful incomplete receipt"
      (let [store
            (h/store)

            sid
            (h/store-session! store {})]

        (tpc/with-own
          [pc {}]
          (with-redefs [db/db-activity-apply!
                        (fn [& _]
                          (throw (ex-info "Storage unavailable" {})))

                        env/run-python-block
                        (fn [_ _ _]
                          (emit-operation! (event/context) 0)
                          {:stdout "work completed"})]

            (let [result (#'lp/run-python-code pc "pass" :env {:db-info store :session-id sid})]
              (expect (= "work completed" (:stdout result)))
              (expect (= :activity/persistence (get-in result [:error :type])))))))))

(defdescribe
  activity-read-failure-test
  ;; #212: a failed final page read must not erase completed Python output or its error.
  (it "preserves the block result when the saved Activity page cannot be read"
      (doseq [original-error [nil {:type :python/error :message "The block failed"}]]
        (let [store (h/store)
              sid (h/store-session! store {})]

          (tpc/with-own
            [pc {}]
            (with-redefs [db/db-activity-page
                          (fn [& _]
                            (throw (ex-info "Storage unavailable during final read" {})))
                          env/run-python-block (fn [_ _ _]
                                                 (emit-operation! (event/context) 0)
                                                 (cond-> {:stdout "work completed"}
                                                   original-error
                                                   (assoc :error original-error)))]

              (let [result (#'lp/run-python-code pc "pass" :env {:db-info store :session-id sid})]
                (expect (= "work completed" (:stdout result)))
                (expect (nil? (:activity result)))
                (if original-error
                  (expect (= original-error (:error result)))
                  (expect (= :activity/persistence (get-in result [:error :type])))))))))))
