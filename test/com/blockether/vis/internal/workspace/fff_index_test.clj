(ns com.blockether.vis.internal.workspace.fff-index-test
  (:require [com.blockether.vis.internal.workspace.fff-index :as index]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn- with-pool
  [f]
  (let [pool
        (atom {})

        reaper
        (atom nil)]

    (with-redefs-fn {#'index/pool pool #'index/idle-reaper reaper #'index/idle-reap-interval-ms 10}
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
