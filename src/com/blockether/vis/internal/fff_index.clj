(ns com.blockether.vis.internal.fff-index
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
     runs when this process wrote anything since it was last synced,
   - the body must NOT close `idx`; the pool owns it (LRU + idle TTL),
   - every filesystem mutation this process performs must call `note-fs-write!`
     so the next search reads its own writes."
  (:require [com.blockether.fff :as fff])
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
  "Permit count for `scan-semaphore`: the max number of FRESH fff index
   scans (grep / find_files — everything that goes through
   `open!`) allowed to run at once. A fresh scan spins fff's own worker
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
   never took). Guards ONLY the index BUILD (create + `wait-for-scan`);
   searching an already-scanned index is cheap and needs no permit, so the
   permit is dropped the moment the scan is ready — maximizing scan overlap."
  [thunk]
  (.acquire scan-semaphore)
  (try (thunk) (finally (.release scan-semaphore))))

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
   (when-not (.isDirectory root)
     (throw (ex-info "rg fff index root must be a directory"
                     {:type :ext.foundation.editing/invalid-rg-root :path (.getPath root)})))
   (let [requested-at (System/nanoTime)]
     (with-scan-permit*
       (fn []
         (let [queued-ms (quot (- (System/nanoTime) requested-at) 1000000)
               k (.getCanonicalPath root)
               idx (try (fff/create
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
                          (throw (ex-info
                                   (str "rg requires fff for directory search, but fff failed for "
                                        k)
                                   {:type :ext.foundation.editing/fff-unavailable :path k}
                                   t))))
               scan-started-at (System/nanoTime)]

           (when-not (fff/wait-for-scan idx scan-timeout-ms)
             (.close ^java.io.Closeable idx)
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
           idx))))))

(def ^:private pool-size
  "How many pooled fff indexes (root × ignore-policy) stay live at once. Each
   holds a native path+content index for a whole tree, so this is a memory
   budget: past it the least-recently-used entry is retired."
  6)

(def ^:private idle-ttl-ms
  "Retire a pooled index untouched for this long. A watcher thread per live
   index is cheap but not free, and a workspace root searched once an hour has
   no business holding a whole tree's content index."
  (* 10 60 1000))

(defonce ^:private pool
  ;; key [canonical-root respect-ignore-files?] -> entry. The index itself is a
  ;; `delay`, so the pool slot is claimed ATOMICALLY (one builder per key, no
  ;; stampede) while the expensive build happens outside the swap.
  (atom {}))

(defn- retire!
  "Close a pooled entry's index — ONCE, and only when no lease still holds it.
   An unrealized delay is never forced (that would build an index just to close
   it); the losing racer no-ops on the `:closed` CAS. Runs under the entry's
   monitor so it cannot interleave with a lease being TAKEN in
   `with-index*` — otherwise an eviction could observe `leases=0` and
   close an index a just-arrived searcher is about to use."
  [entry]
  (let [^java.util.concurrent.atomic.AtomicBoolean lock (:closed entry)]
    (when (locking lock
            (and (zero? (.get ^java.util.concurrent.atomic.AtomicInteger (:leases entry)))
                 (.compareAndSet lock false true)))
      (let [d (:idx entry)]
        (when (realized? d) (try (.close ^java.io.Closeable @d) (catch Throwable _ nil)))))))

(defn- sweep!
  "Evict idle + over-budget pool entries, never `keep-key` (the caller's). Uses
   `swap-vals!` so the victim set is derived from the map that actually landed,
   not from a swap body that may have been retried."
  [keep-key]
  (let [now
        (System/currentTimeMillis)

        [old new]
        (swap-vals!
          pool
          (fn [m]
            (let [live
                  (reduce-kv (fn [acc k e]
                               (if (and (not= k keep-key)
                                        (> (- now
                                              (.get ^java.util.concurrent.atomic.AtomicLong
                                                    (:last-used e)))
                                           (long idle-ttl-ms)))
                                 acc
                                 (assoc acc k e)))
                             {}
                             m)

                  over
                  (- (count live) (long pool-size))]

              (if (pos? over)
                (->> (dissoc live keep-key)
                     (sort-by (fn [[_ e]]
                                (.get ^java.util.concurrent.atomic.AtomicLong (:last-used e))))
                     (take over)
                     (map key)
                     (apply dissoc live))
                live))))]

    (doseq [[k e]
            old

            :when (not (contains? new k))]

      (.set ^java.util.concurrent.atomic.AtomicBoolean (:dead e) true)
      (retire! e))))

(def ^:private write-epoch
  "Bumped by `note-fs-write!` on EVERY mutation this process performs. A pooled
   index only pays for a rescan when this moved past the epoch it last synced to,
   so the steady state (search after search, nothing written) costs zero syscalls."
  (java.util.concurrent.atomic.AtomicLong. 0))

(defn note-fs-write!
  "Tell the fff index pool that THIS process just mutated the filesystem. Cheap
   (one atomic increment); call it from every write/copy/move/delete path so the
   next search sees your write."
  []
  (.incrementAndGet ^java.util.concurrent.atomic.AtomicLong write-epoch))

(defn- resync!
  "Pull a POOLED index up to date before searching it. fff's watcher is live but
   ASYNCHRONOUS — a file written <50ms ago may not be indexed yet, and \"write a
   file, then immediately grep for what you wrote\" is a normal move. `rescan!`
   is the deterministic, read-your-writes rebuild (file index AND content index),
   but it costs 25ms-600ms depending on tree size, so it runs ONLY when
   `write-epoch` moved since this entry last synced. Untouched tree =>
   nothing to do.

   Serialized per entry: two concurrent searches on the same index would
   otherwise each pay a full rebuild for the SAME write. The epoch is read
   BEFORE the rescan, so a write that lands mid-rescan still forces the next
   one."
  [entry idx]
  (let [^java.util.concurrent.atomic.AtomicLong synced (:synced-epoch entry)]
    (when (< (.get synced) (.get ^java.util.concurrent.atomic.AtomicLong write-epoch))
      (locking synced
        (let [now (.get ^java.util.concurrent.atomic.AtomicLong write-epoch)]
          (when (< (.get synced) now) (fff/rescan! idx scan-timeout-ms) (.set synced now)))))))

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

(defn with-index*
  "Call `f` with a POOLED fff index for `lease`'s root + ignore policy. The entry
   is leased for the call, so a concurrent eviction defers its close to the last
   lease holder instead of yanking a live index. A build that THROWS is removed
   from the pool (a poisoned slot would fail every later search)."
  [lease f]
  (let [^File root
        (:root lease)

        respect-ignore-files?
        (:respect-ignore-files? lease)

        k
        (pool-key lease)

        entry
        ;; Claim the slot and TAKE the lease under the entry's monitor, retrying
        ;; when we raced an eviction that already closed this index. Without the
        ;; retry a >TTL-idle entry could be closed between the pool lookup and the
        ;; lease increment, and the search would run against a closed handle.
        (loop []

          (let [e
                (-> (swap! pool
                      (fn [m]
                        (cond-> m
                          (not (contains? m k))
                          (assoc k
                            {:idx (delay (open! root respect-ignore-files? (:overlay lease)))
                             :leases (java.util.concurrent.atomic.AtomicInteger. 0)
                             ;; born "just used": a 0 here would look ancient to a
                             ;; concurrent sweep and evict the entry before its first
                             ;; search.
                             :last-used (java.util.concurrent.atomic.AtomicLong.
                                          (System/currentTimeMillis))
                             :closed (java.util.concurrent.atomic.AtomicBoolean. false)
                             :dead (java.util.concurrent.atomic.AtomicBoolean. false)
                             ;; A fresh index is built AFTER this entry lands, so it
                             ;; already reflects the epoch we record here.
                             :synced-epoch (java.util.concurrent.atomic.AtomicLong.
                                             (.get ^java.util.concurrent.atomic.AtomicLong
                                                   write-epoch))}))))
                    (get k))

                ^java.util.concurrent.atomic.AtomicBoolean lock
                (:closed e)

                taken?
                (locking lock
                  (when-not (.get lock)
                    (.set ^java.util.concurrent.atomic.AtomicLong (:last-used e)
                          (System/currentTimeMillis))
                    (.incrementAndGet ^java.util.concurrent.atomic.AtomicInteger (:leases e))
                    true))]

            (if taken?
              e
              (do (swap! pool (fn [m]
                                (if (identical? (get m k) e) (dissoc m k) m)))
                  (recur)))))]

    (try (let [idx (try @(:idx entry)
                        (catch Throwable t
                          (swap! pool (fn [m]
                                        (if (identical? (get m k) entry) (dissoc m k) m)))
                          (.set ^java.util.concurrent.atomic.AtomicBoolean (:dead entry) true)
                          (throw t)))]
           (sweep! k)
           (resync! entry idx)
           (f idx))
         (finally (when (and (zero? (.decrementAndGet ^java.util.concurrent.atomic.AtomicInteger
                                                      (:leases entry)))
                             (.get ^java.util.concurrent.atomic.AtomicBoolean (:dead entry)))
                    (retire! entry))))))

(defn lease
  "One pool key: which root, under which ignore policy, with which ignore
   overlay. Bundled into a single value so `with-index` keeps a plain
   `[binding init]` shape."
  ([^File root respect-ignore-files?] (lease root respect-ignore-files? nil))
  ([^File root respect-ignore-files? overlay]
   {:root root
    :respect-ignore-files? (boolean respect-ignore-files?)
    :overlay (when (some seq (vals overlay)) overlay)}))

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
