(ns com.blockether.vis.internal.janitor-test
  "The one maintenance worker: one thread, one task at a time, and a line only for
   a run that did work.

   The worker belongs to the whole process, so other tests can have tasks on it.
   Each test here uses its own ids and waits only for its own tasks."
  (:require [com.blockether.vis.internal.bus :as bus]
            [com.blockether.vis.internal.janitor :as janitor]
            [lazytest.core :refer [defdescribe expect it throws?]]
            [taoensso.telemere :as tel]))

(defn- wait-for
  "The value of promise `p`, or `::timeout` after five seconds."
  [p]
  (deref p 5000 ::timeout))

(defn- eventually?
  "Whether `pred` becomes true in five seconds."
  [pred]
  (loop [tries 250]
    (cond (pred) true
          (zero? tries) false
          :else (do (Thread/sleep 20) (recur (dec tries))))))

(defdescribe
  worker-test
  ;; A second thread lets two walks of one disk run together. A thread that is not a
  ;; daemon keeps a finished command alive.
  (it "runs the tasks one after another on one daemon thread at the lowest priority"
      (let [threads
            (atom #{})

            busy
            (atom 0)

            together
            (atom 0)

            done
            (promise)

            work
            (fn [last?]
              (fn []
                (when (< 1 (long (swap! busy inc))) (swap! together inc))
                (swap! threads conj (Thread/currentThread))
                (Thread/sleep 20)
                (swap! busy dec)
                (when last? (deliver done true))
                nil))]

        (janitor/submit! {:id ::first :run (work false)})
        (janitor/submit! {:id ::second :run (work false)})
        (janitor/submit! {:id ::third :run (work true)})
        (expect (true? (wait-for done)))
        (expect (zero? (long @together)))
        (expect (= 1 (count @threads)))
        (let [^Thread thread (first @threads)]
          (expect (.isDaemon thread))
          (expect (= Thread/MIN_PRIORITY (.getPriority thread))))))
  ;; An executor drops a repeating task at its first throw: one failed walk would end
  ;; the cleanup until the next process start.
  (it "runs a repeating task again after a run that throws, until cancel"
      (let [runs
            (atom 0)

            third
            (promise)]

        (janitor/submit! {:id ::flaky
                          :every-ms 10
                          :run (fn []
                                 (let [n (long (swap! runs inc))]
                                   (when (= 1 n) (throw (ex-info "The first run fails" {})))
                                   (when (= 3 n) (deliver third true))
                                   nil))})
        (try (expect (true? (wait-for third))) (finally (expect (true? (janitor/cancel! ::flaky)))))
        ;; A run in progress at the cancel finishes.
        (Thread/sleep 50)
        (let [stopped-at @runs]
          (Thread/sleep 100)
          (expect (= stopped-at @runs)))
        (expect (false? (janitor/cancel! ::flaky)))))
  (it "replaces a waiting task that has the same id"
      (let [ran
            (atom [])

            done
            (promise)]

        (janitor/submit! {:id ::swap :delay-ms 60000 :run #(swap! ran conj :first)})
        (janitor/submit! {:id ::swap
                          :run (fn []
                                 (swap! ran conj :second)
                                 (deliver done true)
                                 nil)})
        (expect (true? (wait-for done)))
        (expect (= [:second] @ran))
        ;; Nothing waits under the id: the first task is gone, not only hidden.
        (expect (eventually? #(not (contains? (janitor/queued) ::swap))))))
  ;; The executor refuses a repeat interval of zero. A refusal after the waiting task
  ;; left the queue would end that cleanup until the next process start.
  (it "refuses a repeat interval of zero and keeps the waiting task"
      (let [done (promise)]
        (janitor/submit! {:id ::kept
                          :delay-ms 100
                          :run (fn []
                                 (deliver done true)
                                 nil)})
        (expect (throws? IllegalArgumentException
                         #(janitor/submit! {:id ::kept :every-ms 0 :run (constantly nil)})))
        (expect (true? (wait-for done)))))
  ;; Each store open gives the worker one task. A finished task that stayed on the
  ;; queue would hold its store for the life of the process.
  (it "takes a one-time task off the queue when its run ends"
      (let [done (promise)]
        (janitor/submit! {:id ::again :delay-ms 60000 :every-ms 60000 :run (constantly nil)})
        (janitor/submit! {:id ::once
                          :run (fn []
                                 (deliver done true)
                                 nil)})
        (try (expect (true? (wait-for done)))
             (expect (eventually? #(not (contains? (janitor/queued) ::once))))
             (expect (contains? (janitor/queued) ::again))
             (finally (janitor/cancel! ::again)))
        (expect (not (contains? (janitor/queued) ::again)))))
  ;; A task asks many times in a day whether its store is due. A line for each such
  ;; run would fill the gateway log with nothing.
  (it
    "writes a line for a run that did work, a warning for a failed run and nothing for an idle run"
    (let [signals
          (atom [])

          done
          (promise)]

      (tel/add-handler! ::janitor-test
                        (fn ([signal] (swap! signals conj signal)) ([]))
                        {:async nil})
      (try (janitor/submit! {:id ::idle :run (constantly nil)})
           (janitor/submit! {:id ::worked :run (constantly {:deleted 2})})
           (janitor/submit! {:id ::broken :run #(throw (ex-info "The walk failed" {}))})
           (janitor/submit! {:id ::last
                             :run (fn []
                                    (deliver done true)
                                    nil)})
           (expect (true? (wait-for done)))
           (expect (= [[::janitor/ran :info ::worked {:deleted 2}]
                       [::janitor/failed :warn ::broken nil]]
                      (->> @signals
                           (filter #(contains? #{::idle ::worked ::broken} (:task (:data %))))
                           (mapv (juxt :id :level (comp :task :data) (comp :result :data))))))
           (expect (= "The walk failed"
                      (some->> @signals
                               (filter #(= ::broken (:task (:data %))))
                               first
                               :error
                               ex-message)))
           (finally (tel/remove-handler! ::janitor-test))))))

(defn- drain!
  "Wait for the janitor to finish all immediate work already on its queue."
  []
  (let [done (promise)]
    (janitor/submit! {:id (random-uuid) :run #(do (deliver done true) nil)})
    (expect (true? (wait-for done)))))

(defdescribe
  signal-trigger-test
  (it "runs on a matching signal without moving or cancelling the repeating timer"
      (let [runs (atom 0)
            first-run (promise)
            timer-run (promise)
            third-run (promise)
            start (System/nanoTime)]
        (janitor/submit! {:id ::triggered
                          :on "vis.session.*"
                          :delay-ms 1000
                          :every-ms 20
                          :run (fn []
                                 (let [n (swap! runs inc)]
                                   (case n
                                     1 (deliver first-run (System/nanoTime))
                                     2 (deliver timer-run (System/nanoTime))
                                     3 (deliver third-run true)
                                     nil))
                                 nil)})
        (try
          (bus/publish! "vis.session.deleted" {"session_id" "trigger-test"})
          (let [first-at (wait-for first-run)
                timer-at (wait-for timer-run)]
            (expect (and (number? first-at) (< (- (long first-at) start) 900000000)))
            (expect (and (number? timer-at) (<= 1000000000 (- (long timer-at) start)))))
          (expect (true? (wait-for third-run)))
          (finally (janitor/cancel! ::triggered)))))
  (it "coalesces signals while work waits and while it runs on the janitor thread"
      (let [subscribed (promise)
            subscribe! bus/subscribe!
            blocked (promise)
            unblock (promise)
            started (promise)
            release (promise)
            threads (atom [])]
        (janitor/submit! {:id ::blocker
                          :run #(do (deliver blocked (Thread/currentThread)) (wait-for unblock) nil)})
        (try
          (let [janitor-thread (wait-for blocked)]
            (with-redefs [bus/subscribe! (fn [pattern receive]
                                          (deliver subscribed receive)
                                          (subscribe! {:owner :vis} pattern receive))]
              (janitor/submit! {:id ::batch
                                :on "vis.session.deleted"
                                :delay-ms 60000
                                :run (fn []
                                       (when (= 1 (count (swap! threads conj (Thread/currentThread))))
                                         (deliver started true)
                                         (wait-for release))
                                       nil)}))
            (let [receive (deref subscribed 0 nil)]
              (expect (ifn? receive))
              (when receive (dotimes [_ 20] (receive nil)))
              (expect (empty? @threads))
              (deliver unblock true)
              (expect (true? (wait-for started)))
              (when receive (dotimes [_ 20] (receive nil)))
              (deliver release true)
              (drain!)
              (expect (= [janitor-thread janitor-thread] @threads))))
          (finally
            (deliver unblock true)
            (deliver release true)
            (janitor/cancel! ::batch)))))
  (it "removes triggers on cancel, replacement and completion of a one-time task"
      (let [callbacks (atom [])
            subscribe! bus/subscribe!
            topic "vis.session.deleted"
            runs (atom [])
            blocked (promise)
            unblock (promise)]
        (janitor/submit! {:id ::block-cancel
                          :run #(do (deliver blocked true) (wait-for unblock) nil)})
        (try
          (expect (true? (wait-for blocked)))
          (with-redefs [bus/subscribe! (fn [pattern receive]
                                        (let [id (subscribe! {:owner :vis} pattern receive)]
                                          (swap! callbacks conj [id receive])
                                          id))]
            (janitor/submit! {:id ::cancel :on topic :delay-ms 60000
                              :run #(swap! runs conj :cancelled)})
            (janitor/submit! {:id ::replace :on topic :delay-ms 60000
                              :run #(swap! runs conj :replaced)})
            (janitor/submit! {:id ::complete :on topic
                              :run #(do (swap! runs conj :completed) nil)}))
          (expect (= 3 (count @callbacks)))
          (let [[[cancel-id cancel-receive] [replace-id replace-receive] [complete-id complete-receive]] @callbacks]
            ;; Queue work before cancel, then simulate a delivery already in flight.
            (when cancel-receive (cancel-receive nil))
            (when replace-receive (replace-receive nil))
            (expect (true? (janitor/cancel! ::cancel)))
            (janitor/submit! {:id ::replace :delay-ms 60000 :run #(swap! runs conj :replacement)})
            (when cancel-receive (cancel-receive nil))
            (when replace-receive (replace-receive nil))
            (deliver unblock true)
            (drain!)
            (when complete-receive (complete-receive nil))
            (drain!)
            (expect (= [:completed] @runs))
            (doseq [id [cancel-id replace-id complete-id]]
              (when id (expect (false? (bus/unsubscribe! :vis id))))))
          (finally
            (deliver unblock true)
            (janitor/cancel! ::cancel)
            (janitor/cancel! ::replace)
            (janitor/cancel! ::complete))))))
