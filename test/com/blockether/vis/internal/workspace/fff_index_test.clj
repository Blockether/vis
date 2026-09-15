(ns com.blockether.vis.internal.workspace.fff-index-test
  (:require [babashka.fs :as fs]
            [com.blockether.fff :as fff]
            [com.blockether.vis.internal.workspace.fff-index :as index]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [taoensso.telemere :as tel]))

(defn- with-pool
  [f]
  (let [pool
        (atom {})

        reaper
        (atom nil)]

    (with-redefs-fn {#'index/pool pool
                     #'index/idle-reaper reaper
                     #'index/idle-reap-interval-ms 10
                     #'index/lifecycle-counters (atom {})}
      (fn []
        (try (f pool reaper)
             (finally (when-let [^Thread runner @reaper]
                        (.interrupt runner)
                        (.join runner 2500))
                      (doseq [entry (vals @pool)]
                        (.set ^java.util.concurrent.atomic.AtomicBoolean (:dead entry) true)
                        (#'index/retire! entry))))))))

(defn- lease [] (index/lease (java.io.File. ".") true))

(deftest idle-index-is-released-without-another-search-test
  (with-pool
    (fn [pool reaper]
      (let [closes
            (atom 0)

            closed
            (promise)

            handle
            (reify
              java.io.Closeable
                (close [_] (swap! closes inc) (deliver closed true)))]

        (with-redefs-fn {#'index/idle-ttl-ms 5
                         #'index/open! (fn [& _]
                                         handle)}
          (fn []
            (index/with-index* (lease) identity)
            (let [^Thread runner @reaper]
              (is (= true (deref closed 2500 false)))
              (when runner (.join runner 2500))
              (is (or (nil? runner) (not (.isAlive runner))))
              (is (nil? @reaper))
              (is (empty? @pool))
              (is (= 1 @closes)))))))))

(deftest idle-sweep-never-closes-a-borrowed-index-test
  (with-pool
    (fn [pool _]
      (let [closes
            (atom 0)

            handle
            (reify
              java.io.Closeable
                (close [_] (swap! closes inc)))]

        (with-redefs-fn {#'index/open! (fn [& _]
                                         handle)}
          (fn []
            (index/with-index* (lease)
                               (fn [_]
                                 (let [entry (first (vals @pool))]
                                   (.set ^java.util.concurrent.atomic.AtomicLong (:last-used entry)
                                         0)
                                   (#'index/sweep! nil)
                                   (is (= 1 (count @pool)))
                                   (is (zero? @closes)))))
            (let [entry (first (vals @pool))]
              ;; Idle age starts after release, not at the beginning of a long search.
              (is (pos? (.get ^java.util.concurrent.atomic.AtomicLong (:last-used entry))))
              (.set ^java.util.concurrent.atomic.AtomicLong (:last-used entry) 0)
              (#'index/sweep! nil)
              (#'index/sweep! nil)
              (is (empty? @pool))
              (is (= 1 @closes)))))))))

(deftest idle-reaper-restarts-after-the-pool-drains-test
  (with-pool (fn [_ reaper]
               (with-redefs-fn {#'index/idle-ttl-ms 0
                                #'index/open! (fn [& _]
                                                (reify
                                                  java.io.Closeable
                                                    (close [_])))}
                 (fn []
                   (dotimes [_ 2]
                     (index/with-index* (lease) identity)
                     (let [^Thread runner @reaper]
                       (when runner (.join runner 2500))
                       (is (or (nil? runner) (not (.isAlive runner))))
                       (is (nil? @reaper)))))))))

(deftest failed-build-does-not-leave-an-idle-worker-test
  (with-pool (fn [pool reaper]
               (with-redefs-fn {#'index/open! (fn [& _]
                                                (throw (ex-info "Index build failed" {})))}
                 (fn []
                   (is (= "Index build failed"
                          (try (index/with-index* (lease) identity)
                               nil
                               (catch clojure.lang.ExceptionInfo e (ex-message e)))))
                   (let [^Thread runner @reaper]
                     (when runner (.join runner 2500))
                     (is (empty? @pool))
                     (is (or (nil? runner) (not (.isAlive runner))))
                     (is (nil? @reaper))))))))

(deftest active-index-remains-shared-under-capacity-pressure-test
  ;; An active worker must not lose its pool entry to another worker's draft scan.
  (with-pool
    (fn [pool _]
      (let [builds
            (atom 0)

            a
            (index/lease (java.io.File. "target/draft-a") true)]

        (with-redefs-fn {#'index/open! (fn [& _]
                                         (swap! builds inc)
                                         (reify
                                           java.io.Closeable
                                             (close [_])))}
          (fn []
            (index/with-index* a
                               (fn [first-handle]
                                 (doseq [n (range 7)]
                                   (index/with-index*
                                     (index/lease (java.io.File. (str "target/draft-" n)) true)
                                     identity))
                                 (is (index/with-index* a #(identical? first-handle %)))
                                 (is (= 8 @builds))))
            (is (<= (count @pool) 6))))))))

(defn- fake-index
  [& _]
  (reify
    java.io.Closeable
      (close [_])))

(deftest writes-only-rescan-overlapping-draft-roots-test
  (with-pool
    (fn [_ _]
      (let [a
            (index/lease (java.io.File. "target/draft-a") true)

            nested
            (index/lease (java.io.File. "target/draft-a/src") true)

            b
            (index/lease (java.io.File. "target/draft-ab") true)

            rescans
            (atom [])]

        (with-redefs-fn {#'index/open! fake-index
                         #'fff/rescan! (fn [idx _]
                                         (swap! rescans conj idx)
                                         true)}
          (fn []
            (let [a-index
                  (index/with-index* a identity)

                  nested-index
                  (index/with-index* nested identity)]

              (index/with-index* b identity)
              ;; Ignore files affect nested roots, not a sibling with a shared prefix.
              (index/note-fs-write! (java.io.File. "target/draft-a/.gitignore"))
              (doseq [lease [a nested b a nested b]]
                (index/with-index* lease identity))
              (is (= [a-index nested-index] @rescans))
              (is (= 2 (get-in (index/pool-stats) [:counters :events :scan]))))))))))

(deftest failed-rescan-does-not-run-search-or-consume-write-test
  (with-pool
    (fn [_ _]
      (let [attempts
            (atom 0)

            searches
            (atom 0)]

        (with-redefs-fn {#'index/open! fake-index
                         #'fff/rescan! (fn [& _]
                                         (> (swap! attempts inc) 1))}
          (fn []
            (index/with-index* (lease) identity)
            (index/note-fs-write! (java.io.File. "marker.txt"))
            (is (= :ext.foundation.editing/fff-scan-timeout
                   (try (index/with-index* (lease)
                                           (fn [_]
                                             (swap! searches inc)))
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
            (is (zero? @searches))
            (index/with-index* (lease)
                               (fn [_]
                                 (swap! searches inc)))
            (is (= 2 @attempts))
            (is (= 1 @searches))))))))

(deftest write-during-rescan-remains-pending-test
  (with-pool (fn [_ _]
               (let [scans (atom 0)]
                 (with-redefs-fn {#'index/open! fake-index
                                  #'fff/rescan! (fn [& _]
                                                  (when (= 1 (swap! scans inc))
                                                    (index/note-fs-write! (java.io.File.
                                                                            "marker.txt")))
                                                  true)}
                   (fn []
                     (index/with-index* (lease) identity)
                     (index/note-fs-write! (java.io.File. "marker.txt"))
                     (dotimes [_ 3]
                       (index/with-index* (lease) identity))
                     (is (= 2 @scans))))))))

(deftest lifecycle-events-and-active-users-test
  (with-pool
    (fn [pool _]
      (with-redefs-fn {#'index/open! fake-index}
        (fn []
          (let [{:keys [signals]}
                (tel/with-signals
                  (index/with-index*
                    (lease)
                    (fn [first-handle]
                      (is (= 1 (:active-users (first (:entries (index/pool-stats))))))
                      (index/with-index*
                        (lease)
                        (fn [second-handle]
                          (is (identical? first-handle second-handle))
                          (is (= 2 (:active-users (first (:entries (index/pool-stats))))))))))
                  (let [entry (first (vals @pool))]
                    (.set ^java.util.concurrent.atomic.AtomicLong (:last-used entry) 0)
                    (#'index/sweep! nil)))

                events
                (mapv :data (filter #(= ::index/lifecycle (:id %)) signals))]

            (is (= 1 (get-in (index/pool-stats) [:counters :events :create])))
            (is (= 1 (get-in (index/pool-stats) [:counters :events :reuse])))
            (is (= 2 (get-in (index/pool-stats) [:counters :events :release])))
            (is (= 1 (get-in (index/pool-stats) [:counters :evictions :idle])))
            (is (some #(and (= :evict (:event %)) (= :idle (:reason %)) (zero? (:active-users %)))
                      events))
            (is (= 1 (count (filter #(= :close (:event %)) events))))))))))

(defn- draft-switching-experiment
  "Real native scans of distinct fixture roots, not clones or user worktrees."
  [root-count rounds]
  (let [base (fs/create-temp-dir {:prefix "vis-fff-switching-"})]
    (try (let [leases (mapv (fn [n]
                              (let [dir (fs/file base (str "draft-" n))]
                                (fs/create-dirs dir)
                                (dotimes [file-number 32]
                                  (spit (fs/file dir (str "marker-" n "-" file-number ".txt"))
                                        (str "draft " n " contents\n")))
                                (index/lease dir true)))
                            (range root-count))]
           (with-pool (fn [_ _]
                        (let [started-at (System/nanoTime)]
                          (dotimes [_ rounds]
                            (doseq [[n lease] (map-indexed vector leases)]
                              ;; Avoid tied millisecond LRU stamps on tiny native fixtures.
                              (Thread/sleep 2)
                              (index/with-index*
                                lease
                                (fn [idx]
                                  (is (= 32
                                         (:total-matched (fff/search idx
                                                                     {:query (str "marker-" n "-")
                                                                      :page-size 100}))))))))
                          (assoc (index/pool-stats)
                            :elapsed-ms (quot (- (System/nanoTime) started-at) 1000000))))))
         (finally (fs/delete-tree base)))))

(deftest native-draft-switching-distinguishes-capacity-from-rescans-test
  (doseq [[roots creates reuses evictions] [[6 6 12 0] [7 21 0 15]]]
    (let [stats (draft-switching-experiment roots 3)]
      (is (= creates (get-in stats [:counters :events :create])))
      (is (= reuses (get-in stats [:counters :events :reuse] 0)))
      (is (= evictions (get-in stats [:counters :evictions :capacity] 0)))
      (is (= creates (get-in stats [:counters :events :scan])))
      (is (= creates (get-in stats [:counters :scans :initial :ready])))
      (is (nil? (get-in stats [:counters :scans :write])))
      (is (every? (comp zero? :active-users) (:entries stats))))))

(deftest initial-scan-events-identify-failed-and-successful-builds-test
  (let [dir (fs/create-temp-dir {:prefix "vis-fff-events-"})]
    (try (doseq [outcome [:create-failed :scan-failed :ready]]
           (with-pool
             (fn [_ _]
               (with-redefs [fff/create (fn [_]
                                          (if (= outcome :create-failed)
                                            (throw (java.io.IOException. "fixture creation failed"))
                                            (fake-index)))
                             fff/wait-for-scan (fn [& _]
                                                 (= outcome :ready))]

                 (let [{:keys [signals]} (tel/with-signals
                                           (try (index/with-index* (index/lease (fs/file dir) true)
                                                                   identity)
                                                (catch clojure.lang.ExceptionInfo _ nil)))
                       events (mapv :data (filter #(= ::index/lifecycle (:id %)) signals))
                       creation (first (filter #(= :create (:event %)) events))
                       scans (filter #(= :scan (:event %)) events)
                       scan (first scans)]

                   (is (= 1 (count scans)))
                   (is (= (:index-id creation) (:index-id scan)))
                   (is (= (:pool-key creation) (:pool-key scan)))
                   (is (= :initial (:reason scan)))
                   (is (= (if (= outcome :ready) :ready :failed) (:status scan)))
                   (is (and (number? (:scan-ms scan)) (not (neg? (:scan-ms scan)))))
                   (is (and (number? (:queued-ms scan)) (not (neg? (:queued-ms scan))))))))))
         (finally (fs/delete-tree dir)))))
