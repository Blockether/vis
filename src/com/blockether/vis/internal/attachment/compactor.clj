(ns com.blockether.vis.internal.attachment.compactor
  "The image compactor: the one background worker that re-compresses stored images.

   A step stores an image as it arrived, with `compaction_pending` set, so that the
   next model request never waits for the optimiser. This worker reads each waiting
   row, compacts its bytes LOSSLESSLY ([[image-convert/compact]]) and writes them
   back. It is not the janitor: a long compaction never delays maintenance, and
   maintenance never delays a compaction.

   One daemon thread at the lowest priority does one image at a time, so a burst of
   screenshots takes one core and no more. The wait lives in the row, not in this
   process: a row that a stopped process left waiting is compacted after the next
   start, and a second run on a compacted row writes nothing.

   [[watch!]] names a store to the worker; [[wake!]] asks for a pass soon after a
   write. A pass also runs every [[poll-ms]], so a wake that came before the write
   committed loses nothing. The thread starts at the first [[watch!]], never at
   namespace load, so the native image holds no thread from its build."
  (:require [com.blockether.vis.internal.attachment.image-convert :as image-convert]
            [com.blockether.vis.internal.perf :as perf]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (com.zaxxer.hikari HikariDataSource)
           (java.util.concurrent ScheduledThreadPoolExecutor ThreadFactory TimeUnit)
           (java.util.concurrent.atomic AtomicBoolean)))

(def ^:private settle-ms
  "The time from a wake to its pass: the write that woke the worker commits first."
  1000)

(def ^:private poll-ms
  "The time between two passes without a wake."
  60000)

(defn- thread-factory
  "Daemon threads at the lowest priority, named `vis-image-compactor-N`."
  ^ThreadFactory []
  (let [daemons (util/daemon-thread-factory "vis-image-compactor")]
    (reify
      ThreadFactory
        (newThread [_ runnable]
          (doto (.newThread daemons runnable) (.setPriority Thread/MIN_PRIORITY))))))

(defonce ^{:private true :doc "The one worker thread."}
         executor
  (delay (doto (ScheduledThreadPoolExecutor. 1 (thread-factory)) (.setRemoveOnCancelPolicy true))))

(defonce ^{:private true :doc "DataSource -> the store the worker drains."}
         stores
  (atom {}))

(defonce ^{:private true :doc "True while a pass waits to start."}
         pass-waits
  (AtomicBoolean.))

(defn- compact-row!
  "Compact the bytes of ONE waiting row and end its wait."
  [db {:keys [id] :as row}]
  (let [^bytes data
        (:bytes row)

        ^bytes out
        (perf/timed :attachment/compact (image-convert/compact data))]

    (persistance/db-finish-attachment-compaction!
      db
      id
      data
      (when (< (alength out) (alength data)) out))))

(defn drain!
  "Compact every waiting row of `db`, one at a time, and answer how many it read."
  [db]
  (loop [n 0]
    (if-let [row (first (persistance/db-pending-attachment-compactions db 1))]
      (do (compact-row! db row) (recur (inc n)))
      n)))

(defn- closed?
  "True when the DataSource `k` of a store is a closed pool. Only a pool tells it:
   a failed connection can also be a passing failure."
  [k]
  (and (instance? HikariDataSource k) (.isClosed ^HikariDataSource k)))

(defn- pass!
  "Drain each watched store. A closed store leaves the worker until its next
   [[watch!]]; a store with a passing failure stays for the next pass."
  []
  (.set ^AtomicBoolean pass-waits false)
  (doseq [[k db] @stores]
    (try (let [started (util/now-ms)
               n (drain! db)]
           (when (pos? (long n))
             (tel/log! {:level :info
                        :id ::compacted
                        :data {:images n :ms (- (util/now-ms) started)}
                        :msg (str "Compacted " n " stored images")})))
         (catch Throwable t
           (if (closed? k)
             (swap! stores dissoc k)
             (tel/log! {:level :warn :id ::pass-failed :error t :msg "Image compaction failed"}))))))

(defn wake!
  "Ask for a pass soon. Many wakes before the pass start make one pass."
  []
  (when (and (seq @stores) (.compareAndSet ^AtomicBoolean pass-waits false true))
    (.schedule ^ScheduledThreadPoolExecutor @executor ^Runnable pass! (long settle-ms) TimeUnit/MILLISECONDS)))

(defonce ^{:private true :doc "The repeating pass, started at the first watch."}
         polling
  (delay (.scheduleWithFixedDelay ^ScheduledThreadPoolExecutor @executor
                                  ^Runnable pass!
                                  (long poll-ms)
                                  (long poll-ms)
                                  TimeUnit/MILLISECONDS)))

(defn watch!
  "Give the store `db` to the worker and drain its waiting rows soon. A store that
   the worker already drains changes nothing."
  [db]
  (when-let [k (:datasource db)]
    (when-not (contains? @stores k)
      (swap! stores assoc k db)
      (force polling)
      (wake!)))
  nil)
