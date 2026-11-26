(ns com.blockether.vis.internal.activity.block-test
  (:require [com.blockether.vis.internal.activity.block :as block]
            [com.blockether.vis.internal.activity.event :as event]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private details
  {:operation :read-record
   :presenter :generic
   :args [{:path "record-1"}]
   :activity {:headline "Read record" :show-start false}})

(defn- run-operation!
  "Record one succeeded operation through the block and answer its events."
  ([activity] (run-operation! activity details))
  ([activity details]
   (let [ctx
         (:context activity)

         inv
         (event/invocation ctx nil)

         events
         [(event/start-event ctx inv details)
          (event/terminal-event ctx
                                inv
                                (assoc details
                                  :outcome :succeeded
                                  :started-at-ms (System/currentTimeMillis)))]]

     (run! #(block/record! activity %) events)
     events)))

(def ^:private team-details
  "Subagent calls of a leader and a child, built in and through the host door."
  (mapv #(assoc details :operation %)
        [:council.run :council.wait :council.cancel :council.ask :council.result
         :vis.council.wait]))

(def ^:private publish-details
  "Council publications, built in and through the host door. User request: the team
   notes already show each message, so Activity must not repeat it."
  (mapv #(assoc details :operation %) [:council.publish :vis.council.publish]))

(defdescribe
  block-activity-test
  (it "records events in order and settles them into the block's envelope"
      (let [seen
            (atom [])

            activity
            (block/start! {:on-event #(swap! seen conj %)})

            events
            (run-operation! activity)

            result
            (block/settle! activity {:stdout "done"})]

        (expect (= events @seen))
        (expect (= "done" (:stdout result)))
        (expect (nil? (:error result)))
        (expect (= "succeeded" (get-in result [:activity :rows 0 :state])))))
  (it "leaves team calls and publications out and still passes their events on"
      (let [seen
            (atom [])

            activity
            (block/start! {:on-event #(swap! seen conj %)})

            events
            (into
              (into [] (mapcat #(run-operation! activity %)) (concat team-details publish-details))
              (mapcat #(run-operation! activity %))
              [(assoc details :operation :council.read) details])

            result
            (block/settle! activity {})]

        (expect (= events @seen))
        (expect (= ["council.read" "read-record"]
                   (map :operation (get-in result [:activity :rows]))))
        (expect (= 2 (get-in result [:activity :counts :succeeded])))))
  (it "keeps team calls and publications out of durable Activity"
      (let [applied
            (atom [])

            activity
            (block/start! {:store {:history-id "history-1"
                                   :apply! (fn [event]
                                             (swap! applied conj event)
                                             {})
                                   :settle! (fn [_ _ _]
                                              nil)
                                   :page (fn []
                                           nil)}})]

        (run! #(run-operation! activity %) (concat team-details publish-details))
        (expect (= {} (block/settle! activity {})))
        (expect (= [] @applied))))
  (it "ends ordinary calls but records Task events after the block ends"
      (let [settled (promise)
            activity (block/start! {:on-late-snapshot #(when (= 1 (get-in % [:counts :succeeded]))
                                                        (deliver settled %))})
            owner ["session" "t1"]
            release! (block/hold! activity owner)
            ctx (:context activity)
            watch (event/invocation ctx nil)
            ordinary (event/invocation ctx nil)]
        (try
          (block/record! activity owner (event/start-event ctx watch (assoc-in details [:activity :show-start] true)))
          (block/record! activity (event/start-event ctx ordinary details))
          (let [r (:activity (block/settle! activity {}))]
            (expect (= [{:type "task" :id "t1"}] (get-in r [:rows 0 :resources])))
             (expect (empty? (get-in r [:rows 1 :resources])))
             (expect (= {:running 1 :succeeded 0 :failed 0 :cancelled 1} (:counts r))))
          (block/record! activity owner (event/terminal-event ctx watch
                                                             (assoc details :outcome :succeeded
                                                                    :started-at-ms 0)))
          (release! {:state :done})
          (expect (= {:running 0 :succeeded 1 :failed 0 :cancelled 1}
                     (:counts (deref settled 3000 nil))))
          (finally (release! {:state :cancelled})))))
  (it "settles a Task without a tool terminal when the session sweeps it"
      (let [settled (promise)
            activity (block/start! {:on-late-snapshot #(deliver settled %)})
            owner ["session" "t1"]
            release! (block/hold! activity owner)
            ctx (:context activity)
            watch (event/invocation ctx nil)]
        (block/record! activity owner (event/start-event ctx watch details))
        (expect (= "running" (get-in (block/settle! activity {}) [:activity :state])))
        (release! {:state :cancelled})
        (expect (= "cancelled" (:state (deref settled 3000 nil))))))
  (it "drops an event that arrives after the block settled"
      (let [seen
            (atom [])

            activity
            (block/start! {:on-event #(swap! seen conj %)})]

        (block/settle! activity {})
        (run-operation! activity)
        (expect (= [] @seen))))
  (it "reports a failed durable write only when the block has no error of its own"
      (doseq [own-error [nil {:type :python/error :message "The block failed"}]]
        (let [activity (block/start! {:store {:history-id "history-1"
                                              :apply! (fn [_]
                                                        (throw (ex-info "Storage unavailable" {})))
                                              :settle! (fn [_ _ _]
                                                         nil)
                                              :page (fn []
                                                      nil)}})]
          (run-operation! activity)
          (let [result (block/settle! activity
                                      (cond-> {}
                                        own-error
                                        (assoc :error own-error)))]
            (expect (= "history-1" (:history-id activity)))
            (if own-error
              (expect (= own-error (:error result)))
              (expect (= :activity/persistence (get-in result [:error :type])))))))))

(defdescribe serial-dispatcher-test
             ;; Regression, issue td-74427c: concurrent callbacks could reduce S2 before
             ;; publishing stale S1, while a slow listener blocked the tool callback itself.
             (it
               "publishes snapshots FIFO without blocking their callback threads"
               (let [[dispatch! shutdown!]
                     (#'block/serial-dispatcher)

                     release-first
                     (promise)

                     entered-first
                     (promise)

                     snapshots
                     (atom [])

                     ;; `dispatch!` answers the Future the settler waits on, so the
                     ;; drain is deref-ing what was submitted rather than a third
                     ;; function the dispatcher no longer hands out.
                     submitted
                     (atom [])

                     submit
                     (fn [n]
                       (swap! submitted conj
                         (dispatch! (fn []
                                      (when (= 1 n) (deliver entered-first true) @release-first)
                                      (swap! snapshots conj n)))))]

                 (try (submit 1)
                      @entered-first
                      (let [started (System/nanoTime)]
                        (submit 2)
                        (submit 3)
                        (expect (< (/ (- (System/nanoTime) started) 1e6) 100.0)))
                      (deliver release-first true)
                      (run! deref @submitted)
                      (expect (= [1 2 3] @snapshots))
                      (finally (shutdown!)))))
             ;; Regression #212: a durable store must not move an unbounded history into its dispatch queue.
             (it
               "backs off producers rather than dropping events or accumulating an unbounded queue"
               (let [[dispatch! shutdown!]
                     (#'block/serial-dispatcher)

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
                        (finally (deliver release true) (deref producer 5000 nil) (shutdown!)))))))
