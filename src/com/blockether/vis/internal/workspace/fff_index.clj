(ns com.blockether.vis.internal.workspace.fff-index
  "THE canonical way vis talks to fff.

   Every fff instance in this process is born here and lives in ONE pool keyed
   by `[canonical-root respect-ignore-files? ignore-overlay]`. Nothing else may call
   `fff/create` — a second, unpooled instance would duplicate a whole tree's
   native path+content index, spin its own watcher threads, and go stale on our
   own writes.

   Contract for callers:

     (fff-index/with-index [idx (fff-index/lease root respect-ignore-files? overlay)]
       (fff/search idx …) (fff/grep idx …))

   - the index is watcher-live (`:watch? true`) and resynced before the body
     runs when this process wrote inside its tree since it was last synced,
   - all gateway workers share the pool and the same root/policy index,
   - the body must NOT close `idx`; borrowed indexes cannot be evicted,
   - filesystem mutations call `note-fs-write!` with the changed path so the
     next search reads its own writes without rescanning unrelated drafts."
  (:require [clojure.string :as str]
            [com.blockether.fff :as fff]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [java.io File]))

(def ^:private scan-timeout-ms
  "Ceiling on how long `open!` blocks for fff's initial scan (paths +
   content index) to COMPLETE before the instance is usable. wait-for-scan
   returns false on timeout; a half-built index silently under-reports
   grep/search hits, so past this ceiling we fail loud instead of searching a
   partial index."
  30000)

;; rg is fff-first: fff owns workspace discovery/ranking, this namespace only
;; re-reads returned candidate files to preserve exact line semantics + patch
;; anchors. Instances are POOLED per (root, ignore-policy) by `with-index`
;; and kept current by fff's own filesystem watcher (`:watch? true`) — the
;; earlier "never cache" rule existed only because a `:watch? false` snapshot
;; silently went stale (the `rg returns nothing that should not be empty` bug).
;; A cold build + first grep of this repo costs ~450ms; the SAME grep on the
;; pooled, page-cache-warm instance costs ~11ms, so pooling is the difference
;; between a third of a second and instant on every search after the first.
;; fff's on-disk mmap cache stays OFF (`:enable-mmap-cache? false`): that one
;; persists ACROSS processes with no watcher behind it.

(def ^:private fff-scan-pool-width
  "How many fresh fff scans can actually be RUNNING at once on this machine: the
   width of fff's `BACKGROUND_THREAD_POOL`, the ONE rayon pool every fff instance
   in this process scans on (`crates/fff-core/src/parallelism.rs`:
   `bg_threads = max(total / 2, 2)` over `available_parallelism`, which is what
   `availableProcessors` answers here).

   Queue time and scan time are NOT the same thing, and fff cannot tell them
   apart: `ScanJob::spawn` (`crates/fff-core/src/scan.rs`) stores
   `scanning = true` on the CALLING thread and only then hands the job to the
   pool, so `wait-for-scan` reports a job that has not started yet as scanning
   and `scan-timeout-ms` is burned WAITING FOR A THREAD. Measured on a 14-core
   machine (pool width 7), 10 concurrent scans of one 25k-file tree: 7 finished
   in ~200 ms and the last 3 in ~1135 ms, while a 3-file directory whose own
   scan costs 12 ms took 990 ms — all of it queue."
  (max 2 (quot (.availableProcessors (Runtime/getRuntime)) 2)))

(def ^:private scan-max-concurrency
  "Permit count for `scan-semaphore`: the max number of initial fff index
   scans and explicit read-your-writes rescans allowed to run at once.
   A scan spins fff's own worker
   threads over the whole tree — cheap for a small repo (~11ms), but up to the
   `scan-timeout-ms` (~30s) ceiling for a large one — so an UNBOUNDED
   `gather(rg, rg, …)` of N searches could fan out into N simultaneous
   full-tree scans, N CPU-heavy scan groups grinding at once (the orphan-CPU
   shape). A small bound caps that blast radius while still overlapping enough
   scans to keep `gather` worthwhile. Cheap reads (cat / index) never spin an
   index and are NEVER bounded — they don't pass through here.

   Never wider than `fff-scan-pool-width`: a permit handed out past fff's own
   pool width does not start a scan, it PARKS one in fff's queue while our 30s
   scan budget runs down, and the timeout then blames the tree for a scheduling
   wait. On a 4-core laptop this is 2 permits, not 4."
  (min 4 (long fff-scan-pool-width)))

(defonce ^:private ^java.util.concurrent.Semaphore scan-semaphore
  ;; FAIR (true) so a queued burst of scans drains in arrival order — no scan
  ;; starves behind a steady stream of later arrivals.
  (java.util.concurrent.Semaphore. scan-max-concurrency true))

(defn- with-scan-permit*
  "Run `thunk` holding ONE fff-scan permit: block (interruptibly) until a permit
   is free, then ALWAYS release it — even when `thunk` throws, or the waiting
   thread is interrupted (turn `cancel!` / eval timeout, which surfaces as an
   `InterruptedException` from `.acquire` and propagates, releasing nothing it
   never took). Guards initial builds and explicit rescans; searching an
   already-synchronized index is cheap and needs no permit. The permit is
   dropped as soon as the scan is ready, maximizing scan overlap."
  [thunk]
  (.acquire ^java.util.concurrent.Semaphore scan-semaphore)
  (try (thunk) (finally (.release ^java.util.concurrent.Semaphore scan-semaphore))))

(def max-content-file-size
  "Largest file whose CONTENT the pooled index will read, in bytes (256 MiB).

   fff's own default is 10 MB (`MAX_FFFILE_SIZE`), and everything above it was
   skipped SILENTLY: a needle sitting in a 20 MB log, dump or generated source
   made `grep` answer \"No file NAME or CONTENT matched\". A silent false
   negative is strictly worse than the slow scan this discovery path replaced,
   so the budget is raised HERE and at the `fff/grep` call site — the index's
   own content budget wins, so moving only one of the two still reads nothing.

   Cost is page cache, not heap: fff reads these files itself, and vis streams
   them line by line (`search-file-content`) rather than slurping them."
  (* 256 1024 1024))

(defonce ^:private lifecycle-counters (atom {}))

(defn- record-event!
  "Bounded process totals plus structured lifecycle logs; never retain event history."
  [event data]
  (swap! lifecycle-counters (fn [counters]
                              (cond-> (update-in counters [:events event] (fnil inc 0))
                                (= event :evict)
                                (update-in [:evictions (:reason data)] (fnil inc 0))

                                (= event :scan)
                                (update :scan-ms (fnil + 0) (:scan-ms data))

                                (= event :scan)
                                (update :queued-ms (fnil + 0) (or (:queued-ms data) 0))

                                (= event :scan)
                                (update-in [:scans (:reason data) (:status data)] (fnil inc 0)))))
  (tel/log! {:level (if (#{:acquire :reuse :release :invalidate} event) :debug :info)
             :id ::lifecycle
             :data (assoc data :event event)}
            "FFF index lifecycle"))

(defn- open!
  "Create a FRESH fff instance scoped to `root`, blocking until its initial
   scan completes. The caller owns the instance and must close it. The
   CPU-heavy build (create + scan) runs under `with-scan-permit*`, so no
   more than `scan-max-concurrency` fresh scans ever run at once — a
   `gather(rg, …)` fan-out queues past the bound instead of stampeding, and that
   bound never exceeds fff's own scan-pool width, so a permit buys a RUNNING
   scan instead of a place in fff's queue. A scan that still misses the ceiling
   reports the two costs APART (`:queued-ms` waiting for a permit, `:scan-ms`
   inside fff), because `your tree is slow` and `no thread was free` are
   different problems.

   `overlay` (optional) is the caller's ignore overlay —
   `{:custom-ignore-filenames [\".rgignore\"] :exclude-globs [\"…\"] :unignore-globs [\"…\"]}` —
   handed straight to fff, which applies it in the native walker AND in the
   watcher's filter.

   `respect-ignore-files?` (default true) is handed STRAIGHT to fff: false makes
   fff's own native walker skip `.gitignore`/`.ignore`/`.git/info/exclude`/global
   ignores. vis' search tools ALWAYS pass true — gitignore is honored
   unconditionally and only the `vis.yml` `:grep` overlay widens it.

   NO `:frecency-db-path` / `:history-db-path` — deliberately. Those are the
   ONLY things that make fff open an LMDB env, and fff opens it in heed's
   default `WithTls` mode, where a reader-lock slot is pinned to every OS
   thread that ever ran a read txn and is never released for the process
   lifetime (dmtrKovalenko/fff#664: ~12-18 slots leaked per long-lived
   process, `maxreaders` 126, then `MDB_READERS_FULL` -> SIGSEGV for the NEXT
   process to open it — i.e. one long-running gateway would poison the user's
   nvim). With both paths nil, fff opens no env, writes nothing to disk and
   shares NO cross-process state: pooled indexes are purely in-memory, so many
   sessions (and other fff consumers on the same machine) cannot clash."
  (^java.io.Closeable [^File root] (open! root true nil))
  (^java.io.Closeable [^File root respect-ignore-files?] (open! root respect-ignore-files? nil))
  (^java.io.Closeable [^File root respect-ignore-files? overlay]
   (open! root respect-ignore-files? overlay nil))
  (^java.io.Closeable [^File root respect-ignore-files? overlay event-data]
   (when-not (.isDirectory root)
     (throw (ex-info "rg fff index root must be a directory"
                     {:type :ext.foundation.editing/invalid-rg-root :path (.getPath root)})))
   (let [requested-at (System/nanoTime)]
     (with-scan-permit*
       (fn []
         (let [queued-ms (quot (- (System/nanoTime) requested-at) 1000000)
               k (.getCanonicalPath root)
               scan-started-at (System/nanoTime)
               idx (try
                     (fff/create
                       {:base-path k
                        :watch? true
                        :ai-mode? true
                        :enable-content-indexing? true
                        :enable-mmap-cache? false
                        ;; content budget: fff skips files past this SILENTLY,
                        ;; and its 10 MB default made grep miss needles that
                        ;; live in big logs/dumps (issue #63 follow-up).
                        :cache-budget-max-file-size max-content-file-size
                        ;; see docstring — never open fff's LMDB dbs.
                        :frecency-db-path nil
                        :history-db-path nil
                        :respect-ignore-files? (boolean respect-ignore-files?)
                        ;; ignore overlay — fff honors it in BOTH the scan
                        ;; walk and the live watcher, which is why vis no
                        ;; longer walks trees in Clojure for `.rgignore` or
                        ;; the `:grep` config overlay.
                        :custom-ignore-filenames (:custom-ignore-filenames overlay)
                        :exclude-globs (:exclude-globs overlay)
                        :unignore-globs (:unignore-globs overlay)})
                     (catch Throwable t
                       (record-event! :scan
                                      (merge event-data
                                             {:root k
                                              :reason :initial
                                              :status :failed
                                              :queued-ms queued-ms
                                              :scan-ms (quot (- (System/nanoTime) scan-started-at)
                                                             1000000)}))
                       (throw (ex-info
                                (str "rg requires fff for directory search, but fff failed for " k)
                                {:type :ext.foundation.editing/fff-unavailable :path k}
                                t))))]

           (try (when-not (fff/wait-for-scan idx scan-timeout-ms)
                  (let [scan-ms (quot (- (System/nanoTime) scan-started-at) 1000000)
                        in-flight (- (long scan-max-concurrency)
                                     (long (.availablePermits ^java.util.concurrent.Semaphore
                                                              scan-semaphore)))]

                    (throw
                      (ex-info (str "rg fff scan did not complete in time for "
                                    k
                                    " — queued "
                                    queued-ms
                                    "ms for one of "
                                    scan-max-concurrency
                                    " scan permits, then "
                                    scan-ms
                                    "ms inside fff with "
                                    in-flight
                                    " scan(s) in flight")
                               {:type :ext.foundation.editing/fff-scan-timeout
                                :path k
                                :timeout-ms scan-timeout-ms
                                :queued-ms queued-ms
                                :scan-ms scan-ms
                                :scans-in-flight in-flight}))))
                (record-event! :scan
                               (merge event-data
                                      {:root k
                                       :reason :initial
                                       :status :ready
                                       :queued-ms queued-ms
                                       :scan-ms (quot (- (System/nanoTime) scan-started-at)
                                                      1000000)}))
                idx
                (catch Throwable t
                  (.close ^java.io.Closeable idx)
                  (record-event! :scan
                                 (merge event-data
                                        {:root k
                                         :reason :initial
                                         :status :failed
                                         :queued-ms queued-ms
                                         :scan-ms (quot (- (System/nanoTime) scan-started-at)
                                                        1000000)}))
                  (throw t)))))))))

(def ^:private pool-size
  "Idle retention budget for root × ignore-policy indexes. Borrowed entries stay
   addressable even above the budget: evicting them would let another worker build
   a duplicate native index. Release trims the least-recently-used idle entries.

   Sized to the working set a gateway really holds, because every capacity eviction
   is repaid as a full rescan: the next search of that root blocks until its index
   rebuilds, which for this repository is 681 ms at the median and 2855 ms at worst.
   Every search SCOPE is its own key, not every repository, so that set is wider than
   it looks: one measured three-hour gateway touched 70 distinct keys, 9 of them
   inside a median idle-TTL window and 27 inside the busiest, and evicted 111 entries
   for capacity against 26 for idleness — at six slots the budget, not the TTL, was
   retiring live indexes. Retention is the cheap side of the trade: 23 concurrent
   indexes (14 repositories plus 9 subdirectory scopes) cost 13 MB of RSS and about
   four native watcher threads each, all returned on close."
  24)

(def ^:private idle-ttl-ms
  "Retire a pooled index untouched for this long. A watcher thread per live
   index is cheap but not free, and a workspace root searched once an hour has
   no business holding a whole tree's content index."
  (* 10 60 1000))

(defonce ^:private pool
  ;; Claiming a slot/lease and selecting eviction victims share this monitor.
  ;; Native construction, scanning and closing always happen outside it.
  (atom {}))

(defonce ^:private index-sequence (java.util.concurrent.atomic.AtomicLong. 0))

(defn- entry-data
  [entry]
  {:index-id (:index-id entry)
   :pool-key (:key entry)
   :active-users (.get ^java.util.concurrent.atomic.AtomicInteger (:leases entry))})

(defn pool-stats
  "Process-wide lifecycle totals and current leases, without forcing a cold index.
   Counters distinguish initial scans, reuse, invalidation and eviction reasons;
   structured `::lifecycle` logs carry per-root scan duration and index identity."
  []
  {:limit pool-size
   :idle-ttl-ms idle-ttl-ms
   :counters @lifecycle-counters
   :entries (mapv (fn [entry]
                    (assoc (entry-data entry) :ready? (realized? (:idx entry))))
                  (vals @pool))})

(defn- retire!
  "Close a retired entry once, after its last lease returns. Eviction and lease
   acquisition share the pool monitor; the closed CAS also protects concurrent
   release/failure cleanup. Never force an unrealized index just to close it."
  [entry]
  (let [^java.util.concurrent.atomic.AtomicBoolean lock (:closed entry)]
    (when (locking lock
            (and (zero? (.get ^java.util.concurrent.atomic.AtomicInteger (:leases entry)))
                 (.compareAndSet lock false true)))
      (let [d (:idx entry)]
        (when (realized? d)
          (try (.close ^java.io.Closeable @d)
               (record-event! :close (entry-data entry))
               (catch Throwable _ nil)))))))

(defn- literal-glob
  "Anchored gitignore pattern matching exactly `rel`, a `/`-separated path under
   the index base."
  [^String rel]
  (str "/" (str/replace rel #"[\\*?\[\]]|\s$" #(str "\\" %))))

(defn- held-globs
  "Exclude globs for every file under `canonical-root` this process holds OS locks
   on (`paths/held-file?`). Scanning, grepping or watching such a file opens it,
   and closing that descriptor releases the locks."
  [^String canonical-root]
  (let [root
        (paths/unixify canonical-root)

        prefix
        (if (str/ends-with? root "/") root (str root "/"))]

    (into []
          (comp (map paths/unixify)
                (filter #(str/starts-with? % prefix))
                (map #(literal-glob (subs % (count prefix)))))
          (sort (paths/held-files)))))

(defn- unguarded?
  "True when the index under pool key `k` may still open a file this process has
   since started to hold locks on: its exclude globs predate that hold."
  [[root _ overlay-key]]
  (let [globs (held-globs root)]
    (boolean (and (seq globs) (not (every? (set (second overlay-key)) globs))))))

(defn- sweep!
  "Retire idle and over-budget entries, and ones whose excludes miss a file this
   process has since started to hold locks on — never borrowed entries or `keep-key`.
   Selection is atomic with taking a lease, so a live index stays shared even
   when workers simultaneously acquire roots under capacity pressure."
  [keep-key]
  (let [now
        (util/now-ms)

        victims
        (locking pool
          (let [available
                (->> @pool
                     (remove (fn [[k e]]
                               (or (= k keep-key)
                                   (pos? (.get ^java.util.concurrent.atomic.AtomicInteger
                                               (:leases e))))))
                     (sort-by (fn [[_ e]]
                                (.get ^java.util.concurrent.atomic.AtomicLong (:last-used e)))))

                unguarded
                (filterv (fn [[k _]]
                           (unguarded? k))
                  available)

                unguarded-keys
                (set (map key unguarded))

                expired
                (filterv (fn [[k e]]
                           (and (not (contains? unguarded-keys k))
                                (> (- now
                                      (.get ^java.util.concurrent.atomic.AtomicLong (:last-used e)))
                                   (long idle-ttl-ms))))
                  available)

                retired-keys
                (into unguarded-keys (map key) expired)

                over
                (- (count @pool) (count retired-keys) (long pool-size))

                capacity
                (take (max 0 over) (remove #(contains? retired-keys (key %)) available))

                victims
                (-> (mapv (fn [[k e]]
                            [k e :held-files])
                          unguarded)
                    (into (map (fn [[k e]]
                                 [k e :idle]))
                          expired)
                    (into (map (fn [[k e]]
                                 [k e :capacity]))
                          capacity))]

            (swap! pool #(apply dissoc % (map first victims)))
            victims))]

    (doseq [[_ entry reason] victims]
      (.set ^java.util.concurrent.atomic.AtomicBoolean (:dead entry) true)
      (record-event! :evict (assoc (entry-data entry) :reason reason))
      (retire! entry))))

(def ^:private idle-reap-interval-ms
  "Maximum scheduling delay after an idle index's TTL expires."
  1000)

(defonce ^:private idle-reaper (atom nil))

(defn- start-idle-reaper!
  "Start one runtime-only daemon for the process-owned pool. It releases idle
   indexes without another search and exits when the pool becomes empty."
  []
  (locking idle-reaper
    (when-not (some-> ^Thread @idle-reaper
                      .isAlive)
      (let [runner (Thread. ^Runnable
                            (fn []
                              (try
                                (loop []

                                  (Thread/sleep (long idle-reap-interval-ms))
                                  (sweep! nil)
                                  (when (locking idle-reaper
                                          (if (seq @pool) true (do (reset! idle-reaper nil) false)))
                                    (recur)))
                                (catch InterruptedException _ nil)
                                (finally (locking idle-reaper
                                           (when (identical? @idle-reaper (Thread/currentThread))
                                             (reset! idle-reaper nil))))))
                            "vis-fff-idle-reaper")]
        (.setDaemon runner true)
        (reset! idle-reaper runner)
        (.start runner))))
  nil)

(defn note-fs-write!
  "Invalidate pooled trees overlapping the changed path's directory. Taking the
   parent also covers ancestor ignore-file changes and directory replacement.
   Unrelated draft roots keep their read-your-writes epoch and do not rescan."
  [^File path]
  (let [canonical
        (.getCanonicalFile path)

        changed
        (.toPath (or (.getParentFile canonical) canonical))]

    (locking pool
      (doseq [[[root-path] entry]
              @pool

              :let [indexed
                    (.toPath (File. ^String root-path))]
              :when (or (.startsWith indexed changed) (.startsWith changed indexed))]

        (.incrementAndGet ^java.util.concurrent.atomic.AtomicLong (:write-epoch entry))
        (record-event! :invalidate (entry-data entry)))))
  nil)

(defn- resync!
  "Serialize read-your-writes rescans per shared index. A write during a rescan
   remains pending; failed scans never advance the synchronized epoch."
  [entry idx]
  (let [^java.util.concurrent.atomic.AtomicLong synced
        (:synced-epoch entry)

        ^java.util.concurrent.atomic.AtomicLong written
        (:write-epoch entry)]

    (when (< (.get synced) (.get written))
      (locking synced
        (let [now (.get written)]
          (when (< (.get synced) now)
            (let [requested-at (System/nanoTime)]
              (with-scan-permit*
                (fn []
                  (let [started-at (System/nanoTime)
                        data (assoc (entry-data entry)
                               :reason :write
                               :queued-ms (quot (- started-at requested-at) 1000000))]

                    (try (when-not (fff/rescan! idx scan-timeout-ms)
                           (throw (ex-info "FFF read-your-writes rescan timed out"
                                           {:type :ext.foundation.editing/fff-scan-timeout
                                            :path (first (:key entry))
                                            :timeout-ms scan-timeout-ms})))
                         (.set synced now)
                         (record-event! :scan
                                        (assoc data
                                          :status :ready
                                          :scan-ms (quot (- (System/nanoTime) started-at) 1000000)))
                         (catch Throwable t
                           (record-event! :scan
                                          (assoc data
                                            :status :failed
                                            :scan-ms (quot (- (System/nanoTime) started-at)
                                                           1000000)))
                           (throw t)))))))))))))

(defn- pool-key
  "The pool identity of a lease: canonical root path, ignore policy and the
   ignore overlay — two overlays index DIFFERENT file universes, so they must
   never share one instance."
  [{:keys [^File root respect-ignore-files? overlay]}]
  [(.getCanonicalPath root) (boolean respect-ignore-files?)
   (when overlay
     (mapv (fn [k]
             (vec (get overlay k)))
           [:custom-ignore-filenames :exclude-globs :unignore-globs]))])

(defn- new-entry
  [lease k]
  (let [users
        (java.util.concurrent.atomic.AtomicInteger. 0)

        id
        (.incrementAndGet ^java.util.concurrent.atomic.AtomicLong index-sequence)]

    {:key k
     :index-id id
     :idx (delay (let [data {:pool-key k :index-id id :active-users (.get users)}]
                   (record-event! :create data)
                   (open! (:root lease) (:respect-ignore-files? lease) (:overlay lease) data)))
     :leases users
     :last-used (java.util.concurrent.atomic.AtomicLong. (util/now-ms))
     :closed (java.util.concurrent.atomic.AtomicBoolean. false)
     :dead (java.util.concurrent.atomic.AtomicBoolean. false)
     :write-epoch (java.util.concurrent.atomic.AtomicLong. 0)
     :synced-epoch (java.util.concurrent.atomic.AtomicLong. 0)}))

(defn with-index*
  "Call `f` with the process-wide index shared by every worker for this root and
   ignore policy. Borrowed indexes remain in the pool under capacity pressure.
   Failed builds are removed so a later call can retry."
  [lease f]
  (let [k
        (pool-key lease)

        [entry reused?]
        (locking pool
          (let [existing
                (get @pool k)

                entry
                (or existing (new-entry lease k))]

            (when-not existing (swap! pool assoc k entry))
            (.set ^java.util.concurrent.atomic.AtomicLong (:last-used entry) (util/now-ms))
            (.incrementAndGet ^java.util.concurrent.atomic.AtomicInteger (:leases entry))
            [entry (some? existing)]))]

    (try (record-event! :acquire (entry-data entry))
         (when reused? (record-event! :reuse (entry-data entry)))
         (start-idle-reaper!)
         (let [idx (try
                     @(:idx entry)
                     (catch Throwable t
                       (let [removed?
                             (locking pool
                               (when (identical? (get @pool k) entry) (swap! pool dissoc k) true))]
                         (.set ^java.util.concurrent.atomic.AtomicBoolean (:dead entry) true)
                         (when removed?
                           (record-event! :evict (assoc (entry-data entry) :reason :build-failed))))
                       (throw t)))]
           (sweep! k)
           (resync! entry idx)
           (f idx))
         (finally (.set ^java.util.concurrent.atomic.AtomicLong (:last-used entry) (util/now-ms))
                  (.decrementAndGet ^java.util.concurrent.atomic.AtomicInteger (:leases entry))
                  (record-event! :release (entry-data entry))
                  (when (.get ^java.util.concurrent.atomic.AtomicBoolean (:dead entry))
                    (retire! entry))
                  (sweep! nil)))))

(defn lease
  "One pool key: which root, under which ignore policy, with which ignore
   overlay. Bundled into a single value so `with-index` keeps a plain
   `[binding init]` shape. Files under `root` that this process holds OS locks
   on (`paths/held-file?`) join the overlay's exclude globs, so no index opens
   them."
  ([^File root respect-ignore-files?] (lease root respect-ignore-files? nil))
  ([^File root respect-ignore-files? overlay]
   (let [held
         (held-globs (.getCanonicalPath root))

         overlay
         (cond-> overlay
           (seq held)
           (update :exclude-globs #(vec (distinct (concat % held)))))]

     {:root root
      :respect-ignore-files? (boolean respect-ignore-files?)
      :overlay (when (some seq (vals overlay)) overlay)})))

(defmacro with-index
  "`(with-index [idx (lease root respect?)] body…)` — body runs with a
   POOLED, watcher-live fff index, resynced before use. Do NOT close `idx`: the
   pool owns it and retires it on idle/LRU eviction."
  [[sym lease] & body]
  `(with-index* ~lease
                (fn [~sym]
                  ~@body)))

(defn warm?
  "True when this lease's index is ALREADY built — i.e. `with-index` on it will
   only pay for a search, never for a tree scan. Lets a latency-sensitive caller
   (the TUI `@` popup on the render thread) skip the first, blocking build."
  [lease]
  (boolean (when-let [e (get @pool (pool-key lease))]
             (and (realized? (:idx e))
                  (not (.get ^java.util.concurrent.atomic.AtomicBoolean (:closed e)))))))

(defn prewarm!
  "Build this lease's index OFF the calling thread, at most one build in flight
   per key (the pool's `delay` guarantees that). Returns nil immediately; poll
   `warm?` to find out when searching became cheap."
  [lease]
  (future (try (with-index* lease
                            (fn [_]
                              nil))
               (catch Throwable _ nil)))
  nil)
