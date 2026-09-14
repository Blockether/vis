(ns com.blockether.vis.internal.gateway.event-store-test
  (:require [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.turn-archive :as archive]
            [com.blockether.vis.internal.gateway.event-store :as store]
            [taoensso.nippy :as nippy]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn- with-replay
  [f]
  (let [sid
        (str (random-uuid))

        registry
        (atom {})]

    (with-redefs-fn {#'state/registry registry
                     #'state/EVENT_RING_MAX (delay 2)
                     #'bus/publish! (fn [& _])
                     #'bus/hydrate! (fn [& _])}
      (fn []
        (try (f sid registry) (finally (#'state/drop-session! sid)))))))

(deftest reads-disk-and-deletes-rotated-files-test
  (with-replay
    (fn [sid registry]
      (state/append-event! sid "block.output" {:output "original"})
      (let [descriptor
            (first (get-in @registry [sid :events]))

            file
            (::store/file descriptor)

            altered
            (assoc (store/read-event descriptor) "output" "disk value")]

        (with-open [out (java.io.FileOutputStream. ^String file)]
          (.write out ^bytes (nippy/freeze altered)))
        (is (= "disk value" (get (first (state/events-since sid 0)) "output")))
        (state/append-event! sid "block.output" {:output "second"})
        (state/append-event! sid "block.output" {:output "third"})
        (is (not (.exists (java.io.File. ^String file))))
        (is (= 1 (state/replay-floor sid)))
        (is (= [2 3] (mapv #(get % "seq") (state/events-since sid 1))))))))

(deftest failures-declare-gaps-test
  (with-replay
    (fn [sid registry]
      (state/append-event! sid "block.output" {:output "first"})
      (with-redefs [archive/write! (fn [_]
                                     (throw (java.io.IOException. "disk full")))]
        (state/append-event! sid "block.output" {:output "lost"}))
      (is (= 2 (state/replay-floor sid)))
      (is (empty? (state/events-since sid 0)))
      (state/append-event! sid "block.output" {:output "third"})
      (let [file (::store/file (first (get-in @registry [sid :events])))]
        ;; A partial file must never turn into a silently shortened replay.
        (spit file "partial")
        (is (try (state/events-since sid 2)
                 false
                 (catch clojure.lang.ExceptionInfo e
                   (= ::state/replay-unavailable (:type (ex-data e))))))
        (is (= 3 (state/replay-floor sid)))
        (is (not (.exists (java.io.File. ^String file))))))))

(deftest byte-budget-and-live-handoff-test
  (with-replay (fn [sid registry]
                 (binding [store/*max-bytes* 1]
                   (state/append-event! sid "block.output" {:output "too large"}))
                 (is (= 1 (state/replay-floor sid)))
                 (is (empty? (get-in @registry [sid :events])))
                 (state/append-event! sid "block.output" {:output "replayed"})
                 (let [observed
                       (atom [])

                       replay
                       (state/subscribe! sid "sink" #(swap! observed conj %) 1)]

                   (state/append-event! sid "block.output" {:output "live"})
                   (is (= [2 3] (mapv #(get % "seq") (concat replay @observed))))
                   (is (= "live" (get (last (state/events-since sid 1)) "output")))))))

(deftest publication-waits-for-disk-and-subscription-handoff-test
  (with-replay
    (fn [sid _registry]
      (let [entered
            (promise)

            release
            (promise)

            started
            (promise)

            original
            store/write-event!

            observed
            (atom [])]

        (with-redefs [store/write-event! (fn [event]
                                           (deliver entered true)
                                           @release
                                           (original event))]
          (let [writer (future (state/append-event! sid "block.output" {:output "durable"}))]
            (try (is (= true (deref entered 3000 :timeout)))
                 (is (= 0 (state/current-seq sid)))
                 (let [subscriber (future (deliver started true)
                                          (state/subscribe! sid "sink" #(swap! observed conj %) 0))]
                   (try (is (= true (deref started 3000 :timeout)))
                        (is (= :blocked (deref subscriber 50 :blocked)))
                        (deliver release true)
                        (is (map? (deref writer 3000 nil)))
                        (is (= [1] (mapv #(get % "seq") (deref subscriber 3000 []))))
                        (is (empty? @observed))
                        (state/append-event! sid "block.output" {:output "next"})
                        (is (= [2] (mapv #(get % "seq") @observed)))
                        (finally (future-cancel subscriber))))
                 (finally (deliver release true) (future-cancel writer)))))))))

(deftest concurrent-producers-deliver-in-cursor-order-test
  (with-replay (fn [sid _registry]
                 (let [observed (atom [])]
                   (state/subscribe! sid "sink" #(swap! observed conj (get % "seq")) 0)
                   (let [writers (doall (repeatedly 4
                                                    #(future (dotimes [_ 10]
                                                               (state/append-event! sid
                                                                                    "block.output"
                                                                                    {:output
                                                                                     "value"})))))]
                     (try (doseq [writer writers]
                            (is (not= :timeout (deref writer 5000 :timeout))))
                          (is (= (vec (range 1 41)) @observed))
                          (finally (doseq [writer writers]
                                     (future-cancel writer)))))))))

(deftest subscription-reseeds-after-concurrent-forget-test
  (with-replay (fn [sid _registry]
                 (let [observed (atom [])]
                   ;; Forget can win between hydration and the locked replay/live handoff.
                   ;; Recreating the entry must preserve the durable journal's cursor floor.
                   (with-redefs [bus/journal-high-water-seq (constantly 40)
                                 bus/hydrate! (fn [_]
                                                (#'state/drop-session! sid))]

                     (is (empty? (state/subscribe! sid "sink" #(swap! observed conj %) 40)))
                     (is (= 40 (state/current-seq sid)))
                     (state/append-event! sid "block.output" {:output "after forget"})
                     (is (= [41] (mapv #(get % "seq") @observed))))))))

(deftest event-observers-run-outside-replay-lock-test
  (with-replay (fn [sid _registry]
                 (let [tap-id
                       (keyword (str (random-uuid)))

                       lock-held?
                       (atom nil)]

                   (try (state/add-event-tap! tap-id
                                              (fn [event-sid _]
                                                ;; Observers may read another session. Holding this session's lock
                                                ;; across the callback permits cross-session lock inversion.
                                                (reset! lock-held? (Thread/holdsLock
                                                                     (store/lock-for event-sid)))))
                        (state/append-event! sid "block.output" {:output "observed"})
                        (is (false? @lock-held?))
                        (finally (state/remove-event-tap! tap-id)))))))
