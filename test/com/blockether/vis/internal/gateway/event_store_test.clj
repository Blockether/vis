(ns com.blockether.vis.internal.gateway.event-store-test
  (:require [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.turn-archive :as archive]
            [com.blockether.vis.internal.gateway.event-store :as store]
            [taoensso.nippy :as nippy]
            [lazytest.core :refer [defdescribe expect it]]))

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

(defdescribe reads-disk-and-deletes-rotated-files-test
             (it "reads disk and deletes rotated files"
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
                       (expect (= "disk value" (get (first (state/events-since sid 0)) "output")))
                       (state/append-event! sid "block.output" {:output "second"})
                       (state/append-event! sid "block.output" {:output "third"})
                       (expect (not (.exists (java.io.File. ^String file))))
                       (expect (= 1 (state/replay-floor sid)))
                       (expect (= [2 3] (mapv #(get % "seq") (state/events-since sid 1)))))))))

(defdescribe failures-declare-gaps-test
             (it "failures declare gaps"
                 (with-replay
                   (fn [sid registry]
                     (state/append-event! sid "block.output" {:output "first"})
                     (with-redefs [archive/write! (fn [_]
                                                    (throw (java.io.IOException. "disk full")))]
                       (state/append-event! sid "block.output" {:output "lost"}))
                     (expect (= 2 (state/replay-floor sid)))
                     (expect (empty? (state/events-since sid 0)))
                     (state/append-event! sid "block.output" {:output "third"})
                     (let [file (::store/file (first (get-in @registry [sid :events])))]
                       ;; A partial file must never turn into a silently shortened replay.
                       (spit file "partial")
                       (expect (try (state/events-since sid 2)
                                    false
                                    (catch clojure.lang.ExceptionInfo e
                                      (= ::state/replay-unavailable (:type (ex-data e))))))
                       (expect (= 3 (state/replay-floor sid)))
                       (expect (not (.exists (java.io.File. ^String file)))))))))

(defdescribe byte-budget-and-live-handoff-test
             (it "byte budget and live handoff"
                 (with-replay (fn [sid registry]
                                (binding [store/*max-bytes* 1]
                                  (state/append-event! sid "block.output" {:output "too large"}))
                                (expect (= 1 (state/replay-floor sid)))
                                (expect (empty? (get-in @registry [sid :events])))
                                (state/append-event! sid "block.output" {:output "replayed"})
                                (let [observed
                                      (atom [])

                                      replay
                                      (state/subscribe! sid "sink" #(swap! observed conj %) 1)]

                                  (state/append-event! sid "block.output" {:output "live"})
                                  (expect (= [2 3] (mapv #(get % "seq") (concat replay @observed))))
                                  (expect (= "live"
                                             (get (last (state/events-since sid 1)) "output"))))))))

(defdescribe
  publication-waits-for-disk-and-subscription-handoff-test
  (it
    "publication waits for disk and subscription handoff"
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
              (try (expect (= true (deref entered 3000 :timeout)))
                   (expect (= 0 (state/current-seq sid)))
                   (let [subscriber (future
                                      (deliver started true)
                                      (state/subscribe! sid "sink" #(swap! observed conj %) 0))]
                     (try (expect (= true (deref started 3000 :timeout)))
                          (expect (= :blocked (deref subscriber 50 :blocked)))
                          (deliver release true)
                          (expect (map? (deref writer 3000 nil)))
                          (expect (= [1] (mapv #(get % "seq") (deref subscriber 3000 []))))
                          (expect (empty? @observed))
                          (state/append-event! sid "block.output" {:output "next"})
                          (expect (= [2] (mapv #(get % "seq") @observed)))
                          (finally (future-cancel subscriber))))
                   (finally (deliver release true) (future-cancel writer))))))))))

(defdescribe concurrent-producers-deliver-in-cursor-order-test
             (it "concurrent producers deliver in cursor order"
                 (with-replay
                   (fn [sid _registry]
                     (let [observed (atom [])]
                       (state/subscribe! sid "sink" #(swap! observed conj (get % "seq")) 0)
                       (let [writers (doall (repeatedly 4
                                                        #(future (dotimes [_ 10]
                                                                   (state/append-event!
                                                                     sid
                                                                     "block.output"
                                                                     {:output "value"})))))]
                         (try (doseq [writer writers]
                                (expect (not= :timeout (deref writer 5000 :timeout))))
                              (expect (= (vec (range 1 41)) @observed))
                              (finally (doseq [writer writers]
                                         (future-cancel writer))))))))))

(defdescribe subscription-reseeds-after-concurrent-forget-test
             (it "subscription reseeds after concurrent forget"
                 (with-replay
                   (fn [sid _registry]
                     (let [observed (atom [])]
                       ;; Forget can win between hydration and the locked replay/live handoff.
                       ;; Recreating the entry must preserve the durable journal's cursor floor.
                       (with-redefs [bus/journal-high-water-seq (constantly 40)
                                     bus/hydrate! (fn [_]
                                                    (#'state/drop-session! sid))]

                         (expect (empty? (state/subscribe! sid "sink" #(swap! observed conj %) 40)))
                         (expect (= 40 (state/current-seq sid)))
                         (state/append-event! sid "block.output" {:output "after forget"})
                         (expect (= [41] (mapv #(get % "seq") @observed)))))))))

(defdescribe event-observers-run-outside-replay-lock-test
             (it "event observers run outside replay lock"
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
                                                                                    (store/lock-for
                                                                                      event-sid)))))
                                       (state/append-event! sid "block.output" {:output "observed"})
                                       (expect (false? @lock-held?))
                                       (finally (state/remove-event-tap! tap-id))))))))

(defdescribe
  settled-turn-replay-test
  (it
    "replays a settled turn without its stream only to a rendering client"
    (with-replay
      (fn [sid registry]
        (with-redefs-fn {#'state/EVENT_RING_MAX (delay 20)}
          (fn []
            (doseq [[type payload]
                    [["turn.started" {:turn_id "settled"}]
                     ["content.block.delta" {:turn_id "settled" :cumulative "Seen"}]
                     ["content.block.delta" {:turn_id "settled" :cumulative "Seen, then more"}]
                     ["block.output" {:turn_id "settled" :output "form"}]
                     ["chunk.response-parse" {:turn_id "settled"}]
                     ["context.updated" {:turn_id "settled"}]
                     ["turn.completed" {:turn_id "settled"}] ["block.output" {:output "unscoped"}]
                     ["turn.started" {:turn_id "running"}]
                     ["content.block.delta" {:turn_id "running" :cumulative "Live"}]]]
              (state/append-event! sid type payload))
            (let [frames
                  #(mapv (juxt (fn [event]
                                 (get event "type"))
                               (fn [event]
                                 (get event "turn_id")))
                         %)

                  resumed
                  [["context.updated" "settled"] ["turn.completed" "settled"] ["block.output" nil]
                   ["turn.started" "running"] ["content.block.delta" "running"]]]

              ;; The client saw the turn start and its first delta, then left.
              (expect (= resumed (frames (state/events-since sid 2 :settled))))
              (expect (= resumed
                         (frames (state/subscribe! sid
                                                   "sink" (fn [_])
                                                   2 :settled))))
              (expect (= (into [["turn.started" "settled"]] resumed)
                         (frames (state/events-since sid 0 :settled))))
              ;; A cursor inside the running turn still gets every frame.
              (expect (= [["content.block.delta" "running"]]
                         (frames (state/events-since sid 9 :settled))))
              ;; A client that follows the stream, such as an SDK poll whose window holds
              ;; a turn's last frames and its terminal, gets every frame.
              (expect (= 8 (count (state/events-since sid 2))))
              (expect (= 8
                         (count (state/subscribe! sid
                                                  "follower" (fn [_])
                                                  2 :full))))
              ;; Only the read is filtered; the ring keeps every stored frame.
              (expect (= 10 (count (get-in @registry [sid :events])))))))))))
