(ns com.blockether.vis.internal.attachment.compactor
  "The attachment compactor: janitor tasks that shrink stored payloads LOSSLESSLY.

   A step stores each payload as it arrived, with `compaction_pending` set, so that
   no step waits for an optimiser. A pass reads each waiting row and makes it
   smaller in two steps: [[image-convert/compact]] gives a PNG the same picture in
   fewer bytes, then [[codec/pack]] stores any payload as a zstd frame when that
   saves enough. Every read gives the same bytes back as before the pass, except
   for a compacted PNG, which shows the same pixels and metadata.

   The work runs on the janitor ([[janitor/submit!]]), so it never runs at the same
   time as a vacuum or a sweep of the same store. A pass stops after [[slice-ms]]
   and asks for the next pass at once, so a long backlog never holds the other
   chores for more than one slice.

   A pass takes a row ([[persistance/db-claim-attachment-compaction!]]) before it
   reads the bytes with the optimiser. A payload that stops the process stays
   `raw` and is never taken again, so it cannot stop each next start too.

   [[watch!]] names a store to the janitor; [[wake!]] asks for a pass soon after a
   write. A pass also runs every [[poll-ms]], so a wake that came before the write
   committed loses nothing."
  (:require [com.blockether.vis.internal.attachment.codec :as codec]
            [com.blockether.vis.internal.attachment.image-convert :as image-convert]
            [com.blockether.vis.internal.janitor :as janitor]
            [com.blockether.vis.internal.perf :as perf]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (com.zaxxer.hikari HikariDataSource)))

(def ^:private settle-ms
  "The time from a wake to its pass: the write that woke the worker commits first."
  1000)

(def ^:private poll-ms "The time between two passes without a wake." 60000)

(def ^:private slice-ms
  "The longest time that one pass compacts before it gives the janitor back."
  2000)

(defonce ^{:private true :doc "DataSource -> the store that the janitor drains."} stores (atom {}))

(defn- smaller
  "`{:bytes :codec :size}` for the payload `data`, or nil when the optimisers win
   nothing. An optimiser failure goes to the log and keeps `data`: it must not stop
   the other rows of the pass."
  [id ^bytes data]
  (try (let [^bytes picture
             (perf/timed :attachment/compact (image-convert/compact data))

             {:keys [^bytes bytes codec]}
             (perf/timed :attachment/pack (codec/pack picture))]

         (when (or (not (identical? picture data)) (not= codec/raw codec))
           {:bytes bytes :codec codec :size (alength picture)}))
       (catch Exception e
         (tel/log! {:level :warn
                    :id ::row-failed
                    :data {:attachment id}
                    :error e
                    :msg (str "Attachment " id " stays as it is: its compaction failed")})
         nil)))

(defn drain!
  "Compact the waiting rows of `db`, one at a time, until none waits or
   `budget-ms` has passed (nil: no limit). The time is checked before each row,
   so one row can take longer than the budget: a large PNG takes seconds. Answer
   `{:rows n :more? bool}`: how many rows it took, and whether rows still wait."
  ([db] (drain! db nil))
  ([db budget-ms]
   (let [deadline (some->> budget-ms
                           long
                           (+ (util/now-ms)))]
     (loop [n 0]
       (let [id (:id (first (persistance/db-pending-attachment-compactions db 1)))]
         (cond (nil? id) {:rows n :more? false}
               ;; Each pass takes one row at least, so a pass always moves on.
               (and deadline (pos? n) (>= (util/now-ms) (long deadline))) {:rows n :more? true}
               :else (let [data (persistance/db-claim-attachment-compaction! db id)]
                       (when-let [out (some->> data
                                               (smaller id))]
                         (persistance/db-finish-attachment-compaction! db id data out))
                       (recur (inc n)))))))))

(defn- closed?
  "True when the DataSource `k` of a store is a closed pool. Only a pool tells it:
   a failed connection can also be a passing failure."
  [k]
  (and (instance? HikariDataSource k) (.isClosed ^HikariDataSource k)))

(defn- task-id "The janitor id of the `kind` task of the store `k`." [k kind] [::compaction kind k])

(defn- pass!
  "Drain one slice of the store `k`. Answer `{:rows n}` for the janitor line, or nil
   when no row waited. A closed store leaves the janitor until its next [[watch!]];
   another failure goes to the janitor log and the store stays."
  [k db]
  (try (let [{:keys [rows more?]} (drain! db slice-ms)]
         ;; Not [[wake-store!]]: this task is still queued while it runs, so that
         ;; guard would drop the next slice.
         (when more? (janitor/submit! {:id (task-id k :wake) :run #(pass! k db)}))
         (when (pos? (long rows)) {:rows rows}))
       (catch Throwable t
         (if (closed? k)
           (do (swap! stores dissoc k)
               (janitor/cancel! (task-id k :poll))
               (janitor/cancel! (task-id k :wake))
               nil)
           (throw t)))))

(defn- wake-store!
  "Give the janitor one pass of the store `k` after `delay-ms`, unless one waits."
  [k delay-ms]
  (when-let [db (get @stores k)]
    (let [id (task-id k :wake)]
      (when-not (contains? (janitor/queued) id)
        (janitor/submit! {:id id :delay-ms delay-ms :run #(pass! k db)})))))

(defn wake!
  "Ask for a pass of each watched store soon. Many wakes before the pass start make
   one pass."
  []
  (doseq [k (keys @stores)]
    (wake-store! k settle-ms)))

(defn watch!
  "Give the store `db` to the janitor and drain its waiting rows soon. A store that
   the janitor already drains changes nothing."
  [db]
  (when-let [k (:datasource db)]
    (when-not (contains? @stores k)
      (swap! stores assoc k db)
      (janitor/submit!
        {:id (task-id k :poll) :delay-ms poll-ms :every-ms poll-ms :run #(pass! k db)})
      (wake-store! k settle-ms)))
  nil)
