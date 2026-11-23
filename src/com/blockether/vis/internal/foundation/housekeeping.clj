(ns com.blockether.vis.internal.foundation.housekeeping
  "Retention for Vis-owned state: advisory inventory and bounded cleanup.

   ADVISORY (`scan` observes, `purge!` acts, `vis-agent doctor` renders): the
   gateway journals, sandbox snapshots of sessions with no row, and temp files of
   killed writes. `scan` is pure observation (no mutation, never throws) and
   `purge!` is the explicit operator action behind `vis-agent doctor --purge`.

   SELF-DELETING (`sweeps`): diagnostic logs, the gateway journals, the display
   caches and old downloaded Python archives. Those are DERIVED — a log of a
   process that exited, the wire replay of a turn the DB already owns, a picture
   whose bytes are already DB-owned — so they carry a window instead of a report.
   `sweep-targets` is the one list of them. Each sweep has its own interval, and
   the stamp of a root holds that interval for the whole machine (`claim-sweep!`),
   so one process walks a root in each interval. No sweep has its own thread:
   `tasks` gives each sweep to `internal.janitor`, the one maintenance worker of a
   process, and the worker repeats it while the process runs. `sweep-stale!` runs
   the same sweeps one time on the calling thread.
   Journals also self-sweep inside the tailer loop (`gateway.bus/sweep!`) after a
   single idle day, but that is a LIVENESS rule and it only runs while a daemon
   does — journals from crashed or never-restarted daemons used to stay forever.

   Versioned Python runtime and source trees, and the frozen guest trees under
   `python/vis-guest`, are reclaimed by CLAIM rather than by age. Every process
   claims the trees it boots from (`internal.paths/claim-dir!`), so an older
   install still serving another Vis process is kept however old it is, while the
   tree a killed process left behind is free the moment it dies. The pinned and
   the newest install are never candidates at all, and a tree from a build before
   claims existed still waits out the retention window.
   `runtime-retention-plan` previews that classification without deleting.

   The `python/pycache` directory mirrors absolute source paths. The bytecode
   sweep removes entries with no source, then limits bytecode from outside the
   Vis runtime, shared packages and extensions.

   `purge!` removes journal files and sandbox snapshots directly, confined to the
   events or sandbox store."
  (:require [clojure.java.io :as io]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as p]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import [com.blockether.vispython Locations]
           [java.io File]
           [java.nio.file FileVisitResult Files LinkOption Path SimpleFileVisitor]
           [java.nio.file.attribute BasicFileAttributes]))

(def default-stale-days "Age past which unattended state is worth mentioning: two weeks." 14)

(def ^:const day-ms 86400000)

(def ^:private ^:const hour-ms 3600000)

(defn- ms->days ^long [^long ms] (long (quot ms (long day-ms))))

(def
  ^:dynamic
  ^{:doc
    "Test seam for the gateway journal directory. `nil` (production) resolves to
                 `~/.vis/gateway/events`, mirroring the private `gateway.bus/events-dir` —
                 journals are addressed by absolute path from several processes, so that
                 location is a fixed contract rather than a user-facing configurable."}
  *events-home*
  nil)

(def
  ^:dynamic
  ^{:doc
    "Test seam for the sandbox snapshot directory. `nil` (production) resolves to
                 `paths/sandbox-defs-dir`."}
  *sandbox-home*
  nil)

(defn- events-dir
  ^File []
  (io/file (or *events-home* (io/file (paths/vis-home) "gateway" "events"))))

;; Filesystem helpers. Every one of these swallows IO failure: housekeeping is
;; advisory, and a permission-denied subtree must not take `vis-agent doctor` down.

(defn- canonical
  [^File f]
  (try (.getCanonicalPath f)
       (catch Throwable _
         (some-> f
                 .getAbsolutePath))))

(defn- tree-stats
  "Recursive size in bytes plus the newest regular-file mtime, in ONE walk.
   Symlinks are never followed. The mtime is what makes an orphan directory safe
   to judge: a directory's own timestamp only moves when entries are added or
   removed, so a busy tree can look untouched for weeks by that measure alone. A file with
   more than one hard link counts once: a fork shares its sandbox snapshots with
   the source when the walks share one `seen` set. Without the set, nothing is deduplicated."
  ([^File root] (tree-stats root nil))
  ([^File root ^java.util.Set seen]
   (try (let [total
              (java.util.concurrent.atomic.AtomicLong. 0)

              newest
              (java.util.concurrent.atomic.AtomicLong. 0)]

          (Files/walkFileTree (.toPath root)
                              (proxy [SimpleFileVisitor] []
                                (visitFile [_p ^BasicFileAttributes attrs]
                                  (when (.isRegularFile attrs)
                                    (when (let [k (.fileKey attrs)]
                                            (or (nil? seen) (nil? k) (.add seen k)))
                                      (.addAndGet total (.size attrs)))
                                    (.getAndUpdate
                                      newest
                                      (reify
                                        java.util.function.LongUnaryOperator
                                          (applyAsLong [_ cur]
                                            (Math/max cur (.toMillis (.lastModifiedTime attrs)))))))
                                  FileVisitResult/CONTINUE)
                                (visitFileFailed [_p _e] FileVisitResult/CONTINUE)))
          ;; Fall back to the directory's own stamp only for an EMPTY tree: a
          ;; directory's mtime moves when entries are added or removed, so on a
          ;; tree with files it reads as fresh even when nothing was worked on.
          {:bytes (.get total)
           :newest-ms (let [n (.get newest)]
                        (if (pos? n) n (.lastModified root)))})
        (catch Throwable _ {:bytes 0 :newest-ms (.lastModified root)}))))

(defn- delete-tree!
  "Depth-first delete. Returns the number of entries removed."
  ^long [^File root]
  (try (let [removed (java.util.concurrent.atomic.AtomicLong. 0)]
         (Files/walkFileTree (.toPath root)
                             (proxy [SimpleFileVisitor] []
                               (visitFile [^Path p _attrs]
                                 (when (Files/deleteIfExists p) (.incrementAndGet removed))
                                 FileVisitResult/CONTINUE)
                               (visitFileFailed [_p _e] FileVisitResult/CONTINUE)
                               (postVisitDirectory [^Path p _e]
                                 (when (Files/deleteIfExists p) (.incrementAndGet removed))
                                 FileVisitResult/CONTINUE)))
         (.get removed))
       (catch Throwable _ 0)))

(defn- under?
  "True when `child` really sits inside `parent` — the guard that keeps a bad
   or stale `:root` from turning a purge into an arbitrary `rm -rf`."
  [^String parent ^String child]
  (boolean (and parent child (not= parent child) (.startsWith child (str parent File/separator)))))

;; The self-deleting sweep
;;
;; UNLIKE journals, everything below is DERIVED and nothing anyone
;; can recover from: a diagnostic log of a process that exited, a picture whose
;; bytes are already DB-owned. Each of these directories gains an entry per shell
;; command, per rendered figure — forever — so each one needs a window, and
;; `sweep-targets` is the ONE place that lists them.
;;
;; `~/.vis/logs` used to be swept at its TOP LEVEL only, which is precisely
;; where the shell logs are not: `shell` writes `logs/shell/<run>/<id>.log`, one
;; directory per command, and a single week of those outweighed everything the
;; sweep could see.

(def default-retention-days
  "Age past which any self-deleting derived artifact is deleted automatically —
   diagnostic logs, gateway journals, the display caches. Two weeks: longer than
   any plausible debugging window (a bug reported on
   Friday is still readable the Monday after next), short enough that a machine
   which never restarts does not carry a quarter of dead sessions. ONE number for
   every kind on purpose — a per-kind window is a promise nobody audits, and each
   kind is reconstructible from the DB or from nothing at all."
  14)

(def default-log-sweep-interval-ms
  "Delay between diagnostic-log sweeps while a process stays alive: one hour."
  3600000)

(def default-cache-budget-bytes
  "Bytes one display cache may still hold once the age pass is done. Age alone
   does not bound an afternoon that renders thousands of figures, so the newest
   files up to this budget survive and the oldest go first."
  (* 512 1024 1024))

(def
  ^:dynamic
  ^{:doc
    "Test seam for the diagnostic log directory. `nil` (production) resolves to
                 `internal.paths/logs-dir`: the `vis.logs.dir` property, else `~/.vis/logs`."}
  *logs-home*
  nil)

(def
  ^:dynamic
  ^{:doc
    "Test seam for the display cache root. `nil` (production) resolves to
                 `~/.vis/cache`, mirroring `foundation.mpl-capture/display-cache-file` and the
                 TUI channel's terminal-image cache."}
  *cache-home*
  nil)

(def
  ^:dynamic
  ^{:doc
    "Test seam for the embedded Python state root. `nil` (production) resolves to
                 `~/.vis/python`, mirroring `com.blockether.vispython.Locations` — the
                 runtime unpacks each pinned version under `runtime/<version>/<platform>`
                 and extracts its shipped sources under `sources/<version>`."}
  *python-home*
  nil)

(defn- home-dir
  "`<vis-home>/<segs…>` unless a test seam overrides the whole root."
  ^File [^String override segs]
  (if override (io/file override) (apply io/file (paths/vis-home) segs)))

(defn- logs-dir ^File [] (if *logs-home* (io/file *logs-home*) (io/file (paths/logs-dir))))

(defn- cache-dir ^File [^String sub] (io/file (home-dir *cache-home* ["cache"]) sub))

(defn- python-home
  "`<vis-home>/python`, the tree of the runtime's `com.blockether.vispython.Locations`."
  ^File []
  (io/file (or *python-home* (io/file (paths/vis-home) "python"))))

(defn- python-dir ^File [^String sub] (io/file (python-home) sub))

(defn- delete-quietly!
  "Delete one path, answering true when this call removed it. A directory that
   is not empty, or a file another process already took, is not an error here."
  [^Path p]
  (try (Files/deleteIfExists p) (catch Throwable _ false)))

(defn- sweep-files!
  "Delete every regular file under `root` older than `cutoff`, then directories
   those deletions emptied or whose own timestamp is stale — `root` excepted.
   Symlinks are never followed (`walkFileTree` does not by default) and every
   candidate is re-checked with `under?` against `canon`, so a hostile link cannot
   walk deletion out of the tree. Returns `{:file-count :deleted :bytes :dirs-removed}`."
  [^File root ^String canon ^long cutoff]
  (let [files
        (java.util.concurrent.atomic.AtomicLong. 0)

        deleted
        (java.util.concurrent.atomic.AtomicLong. 0)

        bytes
        (java.util.concurrent.atomic.AtomicLong. 0)

        dirs
        (java.util.concurrent.atomic.AtomicLong. 0)

        prunable
        (volatile! #{})]

    (try (Files/walkFileTree (.toPath root)
                             (proxy [SimpleFileVisitor] []
                               (visitFile [^Path p ^BasicFileAttributes attrs]
                                 (when (.isRegularFile attrs)
                                   (.incrementAndGet files)
                                   (let [size (.size attrs)]
                                     (when (and (< (.toMillis (.lastModifiedTime attrs)) cutoff)
                                                (under? canon (canonical (.toFile p)))
                                                (delete-quietly! p))
                                       (vswap! prunable conj (.getParent p))
                                       (.incrementAndGet deleted)
                                       (.addAndGet bytes size))))
                                 FileVisitResult/CONTINUE)
                               (visitFileFailed [_p _e] FileVisitResult/CONTINUE)
                               (postVisitDirectory [^Path p _e]
                                 ;; A fresh empty directory may be waiting for its writer's open.
                                 (when (and (not= (.toFile p) root)
                                            (or (contains? @prunable p)
                                                (< (.lastModified (.toFile p)) cutoff))
                                            ;; Resolve the path only for a candidate; most directories are live.
                                            (under? canon (canonical (.toFile p)))
                                            (delete-quietly! p))
                                   (vswap! prunable conj (.getParent p))
                                   (.incrementAndGet dirs))
                                 FileVisitResult/CONTINUE)))
         (catch Throwable _ nil))
    {:file-count (.get files)
     :deleted (.get deleted)
     :bytes (.get bytes)
     :dirs-removed (.get dirs)}))

(defn- trim-to-budget!
  "Delete the OLDEST immediate children of `root` until it holds at most
   `budget` bytes. The age pass cannot bound a single afternoon that renders
   thousands of pictures; this does. Returns `{:deleted :bytes}`."
  [^File root ^String canon ^long budget]
  (let [entries
        (->> (or (.listFiles root) (make-array File 0))
             (filter (fn [^File f]
                       (and (.isFile f) (not (Files/isSymbolicLink (.toPath f))))))
             (map (fn [^File f]
                    {:file f :ms (.lastModified f) :size (.length f)}))
             (sort-by :ms)
             vec)

        total
        (reduce + 0 (map :size entries))]

    (:report (reduce (fn [acc {:keys [^File file ^long size]}]
                       (if (<= (long (:held acc)) budget)
                         (reduced acc)
                         (if (and (under? canon (canonical file)) (delete-quietly! (.toPath file)))
                           (-> acc
                               (update :held - size)
                               (update-in [:report :deleted] inc)
                               (update-in [:report :bytes] + size))
                           acc)))
                     {:held total :report {:deleted 0 :bytes 0}}
                     entries))))

(defn runtime-retention-plan
  "Preview keeping the newest installed release and this binary's pinned runtime.

   Returns `{:is-dry-run true :runtime-version :targets}`, with `:latest-version`,
   `:retained` and `:candidates` for each runtime/source store. Candidates are NOT
   safe-to-delete findings: this preview tests no claim and changes no file. The
   sweep reclaims a candidate only once its in-use claim is free — or, for
   a tree from a build before claims existed, only after the retention window.

   Numeric three-part releases are ordered numerically, not by mtime or string.
   Unknown version names and symlinked directories are retained. `:runtime-version`
   overrides the current binary's pin for fixture experiments. Missing stores
   report empty vectors; unreadable stores report `:unavailable? true`."
  ([] (runtime-retention-plan nil))
  ([{:keys [runtime-version]}]
   (let [pinned (or runtime-version runtime/version)]
     {:is-dry-run true
      :runtime-version pinned
      :targets (mapv
                 (fn [kind]
                   (let [root (python-dir kind)
                         files (.listFiles root)
                         entries
                         (mapv (fn [^File dir]
                                 (let [version (.getName dir)
                                       parts (some->> (re-matches #"(\d+)\.(\d+)\.(\d+)" version)
                                                      rest
                                                      (mapv parse-long))]

                                   {:version version
                                    :root (.getAbsolutePath dir)
                                    :order (when (and (seq parts) (every? some? parts)) parts)
                                    :linked? (Files/isSymbolicLink (.toPath dir))}))
                               (filter #(.isDirectory ^File %) files))
                         latest (->> entries
                                     (filter #(and (:order %) (not (:linked? %))))
                                     (sort-by :order)
                                     last
                                     :version)
                         rows (mapv (fn [{:keys [version order linked?] :as entry}]
                                      (assoc (dissoc entry :order :linked?)
                                        :reason (cond linked? :linked-directory
                                                      (nil? order) :unrecognized-version
                                                      (= pinned version) :pinned
                                                      (= latest version) :latest
                                                      :else :liveness-unverified)))
                                    (sort-by :version entries))]

                     {:kind kind
                      :root (.getAbsolutePath root)
                      :latest-version latest
                      :unavailable? (and (.exists root) (nil? files))
                      :retained (filterv #(not= :liveness-unverified (:reason %)) rows)
                      :candidates (filterv #(= :liveness-unverified (:reason %)) rows)}))
                 ["runtime" "sources"])})))

(defn- reclaimable?
  "True when a store directory is nobody's to keep. A CLAIM decides it whenever
   there is one: a tree a process still boots from stays no matter how old it is,
   because nothing writes to a tree while serving it and age would call it dead.
   Only an UNCLAIMED directory — one written before claims existed, or one that
   nothing ever used — falls back to `cutoff`."
  [^File dir ^long cutoff]
  (case (paths/claim-state dir)
    :held
    false

    :free
    true

    (< (long (:newest-ms (tree-stats dir))) cutoff)))

(defn- reclaim-dir!
  "Delete one unused directory tree, answering `{:deleted :bytes}`. The `under?`
   guard is the one `purge!` uses: a store root that resolved to something
   unexpected removes nothing."
  [^File dir ^String canon]
  (if-not (under? canon (canonical dir))
    {:deleted 0 :bytes 0}
    (let [{:keys [bytes]}
          (tree-stats dir)

          removed
          (delete-tree! dir)]

      {:deleted removed :bytes (if (pos? removed) (long bytes) 0)})))

(defn- reclaim-unused!
  "Delete `dir` when nobody keeps it, and answer `{:deleted :bytes}`, or nil when
   the directory stays. The judgement and the delete hold the claim lock, so a
   publisher cannot claim the directory between them. With `lock-root`, that
   holds for publishers in other processes that lock the same store root too."
  [^File lock-root ^File dir ^long cutoff ^String canon]
  (let [reclaim (fn []
                  (when (reclaimable? dir cutoff) (reclaim-dir! dir canon)))]
    (if lock-root (paths/with-claim-lock lock-root reclaim) (paths/with-claim-lock reclaim))))

(defn- add-counts
  [a b]
  {:deleted (+ (long (:deleted a)) (long (:deleted b)))
   :bytes (+ (long (:bytes a)) (long (:bytes b)))})

(def ^:private ^"[Ljava.nio.file.LinkOption;" scratch-no-follow
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- scratch-directory? [^File dir]
  (Files/isDirectory (.toPath dir) scratch-no-follow))

(defn- detach-scratch!
  "Rename a free directory under the claim lock. A later sweep retries removal
   if this process stops before it can empty the renamed directory."
  ^File [^File root ^File dir]
  (let [detached (File. root (str ".reclaim-" (random-uuid)))]
    (Files/move (.toPath dir) (.toPath detached)
                (into-array java.nio.file.CopyOption [java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
    detached))

(defn delete-session-scratch!
  "Remove the session scratch after its last user releases its claim."
  [session-id]
  (when-let [dir (paths/scratch-directory session-id)]
    (let [root (paths/scratch-root)]
      (when (scratch-directory? root)
        (when-let [detached (paths/with-claim-lock root
                             (fn []
                               (when (and (scratch-directory? dir) (not= :held (paths/claim-state dir)))
                                 (detach-scratch! root dir))))]
          (reclaim-dir! detached (canonical root)))))))

(defn- older-than? [^long cutoff ^java.util.Date date]
  (and date (< (.getTime date) cutoff)))

(defn- scratch-expired?
  [db-info ^File dir ^long cutoff]
  (let [store (File. dir ".vis-db")]
    (when (and (Files/isRegularFile (.toPath store) scratch-no-follow)
               (= (:db-file db-info) (slurp store)))
      (let [sid (parse-uuid (.getName dir))]
        (if-let [session (p/db-get-session db-info sid)]
          (and (< (.lastModified dir) cutoff)
               (older-than? cutoff (:created-at session))
               (every? #(older-than? cutoff (:created-at %))
                       (p/db-list-session-turns-meta db-info sid)))
          true)))))

(defn sweep-session-scratch!
  "Remove unclaimed scratch for this database after deletion or 24 hours without a turn."
  [{:keys [db-file] :as db-info} now]
  (let [root (paths/scratch-root)
        cutoff (- (long now) day-ms)]
    (when (and db-file (scratch-directory? root))
      (reduce
        (fn [counts ^File dir]
          (let [id (.getName dir)
                detached
                (cond (and (re-matches #"\.reclaim-[a-f0-9-]+" id) (scratch-directory? dir))
                      dir

                      (paths/scratch-directory id)
                      (paths/with-claim-lock root
                        (fn []
                          (when (and (scratch-directory? dir)
                                     (not= :held (paths/claim-state dir))
                                     (scratch-expired? db-info dir cutoff))
                            (detach-scratch! root dir)))))]
            (if detached
              (add-counts counts (reclaim-dir! detached (canonical root)))
              counts)))
        {:deleted 0 :bytes 0}
        (.listFiles root)))))

(defn scratch-size-warning
  "Text for the agent when its scratch files exceed 10 GiB. Never deletes files."
  [dir]
  (when dir
    (let [{:keys [bytes]} (tree-stats (io/file dir))]
      (when (> (long bytes) (* 10 1024 1024 1024))
        (str "Session scratch uses " bytes " bytes at `" dir "`. The warning limit is 10 GiB.")))))

(defn- sweep-guest-store!
  "Reclaim the frozen Python guest trees nobody serves any more.

   `~/.vis/python/vis-guest/<release>-<digest>` holds the guest modules ONE build
   publishes, and every `<generation>/vis-ext-code*` inside it the frozen
   extension snapshots of ONE engine process. Both are claimed while they are in
   use, and a process that dies without running its shutdown hook — a kill, a
   crash, a hard native exit — is exactly what strands them. Loose files in the
   store root are what a release before content identity staged there.

   Returns `{:deleted :bytes}`."
  [^File root ^String canon ^long cutoff]
  (reduce
    (fn [acc ^File child]
      (let [dot? (.startsWith (.getName child) ".")]
        (cond
          (.isFile child)
          (if (and (not dot?) (< (.lastModified child) cutoff) (under? canon (canonical child)))
            (let [size (.length child)]
              (if (delete-quietly! (.toPath child)) (add-counts acc {:deleted 1 :bytes size}) acc))
            acc)
          (or dot? (not (.isDirectory child))) acc
          :else (if-let [counts (reclaim-unused! root child cutoff canon)]
                  (add-counts acc counts)
                  (reduce (fn [acc ^File snapshot]
                            (if-let [counts (and (.isDirectory snapshot)
                                                 (.startsWith (.getName snapshot) "vis-ext-code")
                                                 (reclaim-unused! root snapshot cutoff canon))]
                              (add-counts acc counts)
                              acc))
                          acc
                          (or (.listFiles child) (make-array File 0)))))))
    {:deleted 0 :bytes 0}
    (or (.listFiles root) (make-array File 0))))

(def ^:private ^"[Ljava.nio.file.LinkOption;" no-follow
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- pycache-source-present?
  "Whether the module a `.pyc` caches still exists among `names`, the entries of
   its mirrored source directory. CPython names an entry after the source stem and
   its own tag — `c.py` becomes `c.cpython-314.pyc`, with an `.opt-N` infix for an
   optimized build — so the stem ends where the tag begins."
  [names ^String entry-name]
  (let [tag
        (.indexOf entry-name ".cpython-")

        stem
        (if (neg? tag) entry-name (subs entry-name 0 tag))]

    (or (contains? names (str stem ".py")) (contains? names (str stem ".pyw")))))

(defn- stamps-dir ^File [] (cache-dir "housekeeping"))

(defn- sweep-stamp
  "The file whose timestamp records the last walk of `root` by the sweep `id`.

   The name holds a digest of the root, so two processes share a stamp only when
   they sweep the same directory. A process with its own `vis.logs.dir` and the
   Vis home of another process thus never holds the logs of that process back.
   Stamps live in their own folder, outside the roots they describe, so no other
   sweep counts or lists one."
  ^File [^String id ^File root]
  (io/file (stamps-dir) (str id "-" (subs (util/sha256-hex (canonical root)) 0 12) ".stamp")))

(def ^:private ^:const sweep-lease-ms
  "How long a claimed walk keeps other processes off its root. A walk of a large
   store takes minutes on a daemon thread, and a short-lived command exits before
   it ends; only a FINISHED walk holds the root for its full interval."
  (* 10 60 1000))

(defn- claim-sweep!
  "Whether this process walks the root of `stamp` now, leasing it when it does.

   Every Vis process queues the sweeps, and a live store holds hundreds of
   thousands of entries. The stamp is shared, so a machine walks each root once
   per `interval-ms`, not once per process. The claim holds the root for
   `sweep-lease-ms` only; [[finish-sweep!]] stamps the full interval when the walk
   ends. A stamp from the future counts as stale, so a clock moved back cannot
   stop the sweep."
  [^File stamp ^long now ^long interval-ms]
  (let [age (- now (.lastModified stamp))]
    (when (or (neg? age) (>= age interval-ms))
      (try
        (.mkdirs (.getParentFile stamp))
        (.createNewFile stamp)
        (.setLastModified stamp (- now (max 0 (- interval-ms (long sweep-lease-ms)))))
        (catch Throwable e
          (tel/log!
            {:level :warn :id ::sweep-stamp-not-written :data {:stamp (.getPath stamp)} :error e})))
      true)))

(defn- finish-sweep!
  "Stamp the walk of `stamp` that started at `now` as done, and answer `result`."
  [^File stamp ^long now result]
  (try (.setLastModified stamp now)
       (catch Throwable e
         (tel/log!
           {:level :warn :id ::sweep-stamp-not-written :data {:stamp (.getPath stamp)} :error e})))
  result)

(def ^:private pycache-budget-bytes
  "Maximum bytes of cached modules outside the Vis runtime, packages and extensions."
  (* 1024 1024 1024))

(defn- pycache-hot-roots
  "Absolute source paths of the Vis runtime, shared packages and global extensions."
  []
  (into #{}
        (mapcat (fn [^File dir] [(.getAbsolutePath dir) (canonical dir)]))
        [(python-dir "runtime") (python-dir "sources") (python-dir "vis-guest")
         (if *python-home* (python-dir "packages") (io/file (Locations/packagesDir)))
         (config/global-extensions-dir)]))

(defn- trim-pycache!
  "Remove the oldest cold entries until `held` is within `budget`."
  [entries ^String canon held budget]
  (let [parents (java.util.HashSet.)
        {:keys [report]}
        (reduce (fn [{:keys [held] :as acc} [_ ^File file size]]
                  (cond (<= (long held) (long budget)) (reduced acc)
                        (and (under? canon (canonical file)) (delete-quietly! (.toPath file)))
                        (do (.add parents (.getParentFile file))
                            (-> acc
                                (update :held - size)
                                (update :report add-counts {:deleted 1 :bytes size})))
                        :else acc))
                {:held held :report {:deleted 0 :bytes 0}}
                (sort-by first entries))]
    (reduce (fn [report ^File parent]
              (loop [report report, dir parent]
                (if (and (under? canon (canonical dir))
                         (empty? (.list dir))
                         (delete-quietly! (.toPath dir)))
                  (recur (update report :deleted inc) (.getParentFile dir))
                  report)))
            report
            parents)))

(defn- sweep-pycache!
  "Remove bytecode with no source. Limit other bytecode outside the hot roots.

   The Python cache prefix mirrors the absolute source path: a bytecode file at
   `<prefix>/tmp/x/c.cpython-314.pyc` caches `/tmp/x/c.py`. Remove a mirror directory
   when its source directory does not exist. For each other directory, list its
   sources once. A live entry under a hot root needs no file attributes.

   Python does not change a bytecode file's modification time on import. Keep
   live bytecode under hot roots regardless of age. For other live bytecode,
   read its size and modification time in one call and sort only if over budget.

   Returns `{:deleted :bytes}`."
  [^File root ^String canon budget]
  (let [hot-roots (pycache-hot-roots)
        cold (java.util.ArrayList.)
        held (volatile! 0)]
    (letfn
      [(visit [acc ^File dir ^File source-dir hot?]
         (let [names (delay (set (.list source-dir)))
               hot? (or hot? (contains? hot-roots (.getPath source-dir)))]
           (reduce (fn [acc ^String entry-name]
                     (let [pyc? (.endsWith entry-name ".pyc")
                           live? (and pyc? (pycache-source-present? @names entry-name))]
                       (if (and live? hot?)
                         acc
                         (let [child (File. dir entry-name)
                               ^BasicFileAttributes attrs
                               (try (Files/readAttributes (.toPath child) BasicFileAttributes no-follow)
                                    (catch java.io.IOException e
                                      (tel/log! {:level :debug
                                                 :id ::pycache-entry-unreadable
                                                 :data {:path (.getPath child)}
                                                 :error e})
                                      nil))]
                           (cond (or (nil? attrs) (.isSymbolicLink attrs)) acc
                                 (.isDirectory attrs)
                                 (let [child-source (File. source-dir entry-name)]
                                   (if-not (.isDirectory child-source)
                                     (add-counts acc (reclaim-dir! child canon))
                                     (let [swept (visit acc child child-source hot?)]
                                       (if (and (empty? (.list child))
                                                (delete-quietly! (.toPath child)))
                                         (add-counts swept {:deleted 1 :bytes 0})
                                         swept))))
                                 (not (and pyc? (.isRegularFile attrs))) acc
                                 live?
                                 (let [size (.size attrs)]
                                   (.add cold [(.toMillis (.lastModifiedTime attrs)) child size])
                                   (vswap! held + size)
                                   acc)
                                 (and (under? canon (canonical child)) (delete-quietly! (.toPath child)))
                                 (add-counts acc {:deleted 1 :bytes (.size attrs)})
                                 :else acc)))))
                   acc
                   (.list dir))))]
      (let [report (visit {:deleted 0 :bytes 0} root (File. File/separator) false)]
        (if (<= (long @held) (long budget))
          report
          (add-counts report (trim-pycache! cold canon @held budget)))))))

(defn- prune-version-stores!
  "Reclaim installed interpreter and source versions no process serves any more.
   [[runtime-retention-plan]] decides which versions are candidates at all — the
   pinned one, the newest install, a symlink and an unrecognized name never are —
   and the claim decides whether a candidate is actually free. Returns one report
   row per store."
  [^long cutoff]
  (mapv (fn [{:keys [kind root candidates]}]
          (let [canon
                (canonical (io/file root))

                counts
                (reduce (fn [acc candidate]
                          (let [^File dir (io/file (:root candidate))]
                            (if-let [counts (and (.isDirectory dir)
                                                 (reclaim-unused! nil dir cutoff canon))]
                              (add-counts acc counts)
                              acc)))
                        {:deleted 0 :bytes 0}
                        candidates)]

            (merge {:id (keyword (str "python-" kind)) :root canon} counts)))
        (:targets (runtime-retention-plan))))

(def ^:private sweep-targets
  "Every directory Vis fills on its own that holds nothing anyone can recover —
   the one list, so a new producer is bounded by being added here rather than by
   a second sweep somewhere else.

   A sweep deletes stale FILES anywhere below the root and then the directories
   they emptied. `:budget-bytes` additionally caps what survives the age pass.
   `:every-ms` is the least time between two sweeps of the root. A byte budget
   holds only as often as its sweep runs, so the two caches run each hour, like
   the logs. The other roots run each day."
  [{:id :logs
    :dir logs-dir
    :retention-days default-retention-days
    :every-ms default-log-sweep-interval-ms}
   {:id :gateway-events :dir events-dir :retention-days default-retention-days :every-ms day-ms}
   {:id :display
    :dir #(cache-dir "display")
    :retention-days default-retention-days
    :budget-bytes default-cache-budget-bytes
    :every-ms hour-ms}
   {:id :tui-attachments
    :dir #(cache-dir "tui-attachments")
    :retention-days default-retention-days
    :budget-bytes default-cache-budget-bytes
    :every-ms hour-ms}
   ;; Releases before 0.4 kept the downloaded platform archive beside the tree
   ;; it was unpacked into; today's fetch deletes its archive once unpacked.
   {:id :python-archives
    :dir #(python-dir "archives")
    :retention-days default-retention-days
    :every-ms day-ms}
   ;; Each sweep keeps one stamp for each root. The stamp of a root that is gone ages out.
   {:id :sweep-stamps :dir stamps-dir :retention-days default-retention-days :every-ms day-ms}])

(defn- sandbox-dir ^File [] (io/file (or *sandbox-home* (paths/sandbox-defs-dir))))

(defn- temp-leftover?
  "True for the `.<name>.tmp-<uuid>` file of a write that a killed process left."
  [^String file-name]
  (and (.startsWith file-name ".") (.contains file-name ".tmp-")))

(defn- sandbox-temps
  "The temp files of killed writes inside the snapshot folder `dir`."
  [^File dir]
  (filter #(and (.isFile ^File %) (temp-leftover? (.getName ^File %))) (rest (file-seq dir))))

(defn- temp-written-ms
  "When the write of temp file `f` started. A hard link keeps the mtime of its source,
   so a link temp carries its start time in its name: `.<name>.tmp-<millis>-<uuid>`."
  ^long [^File f]
  (if-let [[_ ms] (re-find #"\.tmp-(\d{13})-" (.getName f))]
    (Long/parseLong ms)
    (.lastModified f)))

(defn- freed-bytes
  "The bytes that a delete of `f` frees. A file with more hard links frees nothing."
  ^long [^File f]
  (let [links (try (long (Files/getAttribute (.toPath f) "unix:nlink" no-follow))
                   (catch Throwable _ 1))]
    (if (> links 1) 0 (.length f))))

(defn- sweep-sandbox-temps!
  "Delete the temp files of killed snapshot writes under `root` that are older than
   `cutoff-ms`. A write takes milliseconds, so an old temp file has no writer."
  [^File root ^long cutoff-ms]
  (if (.isDirectory root)
    (reduce (fn [acc ^File f]
              (let [size (freed-bytes f)]
                (if (and (< (temp-written-ms f) cutoff-ms) (delete-quietly! (.toPath f)))
                  (-> acc
                      (update :deleted inc)
                      (update :bytes + size))
                  acc)))
            {:deleted 0 :bytes 0}
            (sandbox-temps root))
    {:deleted 0 :bytes 0}))

(defn- sweep-window
  "The age window of one pass as `{:days :cutoff-ms}`: `retention-days`, or the
   `:days` of the pass."
  [{:keys [now days]} retention-days]
  (let [days (long (or days retention-days))]
    {:days days :cutoff-ms (- (long now) (* days (long day-ms)))}))

(defn- sweep-target!
  "Sweep the `sweep-targets` entry `target` at `dir`: the files past the age
   window, then the oldest files over the byte budget."
  [{:keys [id retention-days budget-bytes]} ^File dir {budget-override :budget-bytes :as pass}]
  (let [canon
        (canonical dir)

        window
        (sweep-window pass retention-days)

        swept
        (sweep-files! dir canon (:cutoff-ms window))

        trimmed
        (when budget-bytes (trim-to-budget! dir canon (long (or budget-override budget-bytes))))]

    [(merge {:id id :root canon}
            window
            swept
            (when trimmed
              (assoc (add-counts swept trimmed) :over-budget-deleted (:deleted trimmed))))]))

(def ^:private sweeps
  "Every self-deleting sweep, in report order. This is the ONE list: `sweep-stale!`
   runs it and `tasks` gives it to the janitor.

   `:every-ms` is the least time between two runs of a sweep on one root.
   `:rows` names its report rows. `:root` answers the directory that the sweep
   walks: without that directory there is no work. `:run` takes the directory and
   the pass, `{:now :days :budget-bytes}`, and answers the rows."
  (into
    (mapv (fn [{:keys [id dir every-ms] :as target}]
            {:id id
             :every-ms every-ms
             :rows [id]
             :root dir
             :run (fn [dir pass]
                    (sweep-target! target dir pass))})
          sweep-targets)
    [{:id :python-guest
      :every-ms day-ms
      :rows [:python-guest]
      :root #(python-dir "vis-guest")
      :run (fn [^File dir pass]
             (let [canon
                   (canonical dir)

                   window
                   (sweep-window pass default-retention-days)]

               [(merge {:id :python-guest :root canon}
                       window
                       (sweep-guest-store! dir canon (:cutoff-ms window)))]))}
     {:id :python-pycache
      :every-ms day-ms
      :rows [:python-pycache]
      :root #(python-dir "pycache")
      :run (fn [^File dir {:keys [budget-bytes]}]
             (let [canon (canonical dir)]
               [(merge {:id :python-pycache :root canon}
                       (sweep-pycache! dir canon (or budget-bytes pycache-budget-bytes)))]))}
     {:id :sandbox-temp
      :every-ms day-ms
      :rows [:sandbox-temp]
      :root sandbox-dir
      :run (fn [^File dir pass]
             (let [window (sweep-window pass default-retention-days)]
               [(merge {:id :sandbox-temp :root (canonical dir)}
                       window
                       (sweep-sandbox-temps! dir (:cutoff-ms window)))]))}
     {:id :python-versions
      :every-ms day-ms
      :rows [:python-runtime :python-sources]
      :root python-home
      :run (fn [_dir pass]
             (let [window (sweep-window pass default-retention-days)]
               (mapv #(merge window %) (prune-version-stores! (:cutoff-ms window)))))}]))

(defn- run-sweep!
  "Run `sweep` when its directory exists and its interval is over, and answer its
   report rows. Answer nil when there is no work. The stamp is claimed last, so a
   store that does not exist takes no stamp."
  [{:keys [id every-ms root run]} {:keys [now] :as pass}]
  (let [^File dir (root)]
    (when (.isDirectory dir)
      (let [stamp (sweep-stamp (name id) dir)]
        (when (claim-sweep! stamp (long now) (long every-ms))
          (finish-sweep! stamp (long now) (run dir pass)))))))

(defn- totals
  "The entries and bytes that the report `rows` removed."
  [rows]
  {:deleted (reduce + 0 (map :deleted rows)) :bytes (reduce + 0 (map :bytes rows))})

(defn sweep-stale!
  "Run each sweep of `sweeps` that is due, in order, on the calling thread. Returns
   `{:targets [row…] :deleted :bytes}` — `:deleted` counts entries actually
   removed and `:bytes` the space reclaimed. A sweep that ran gives its full rows:
   for a `sweep-targets` entry `{:id :root :days :cutoff-ms :file-count :deleted
   :bytes :dirs-removed :over-budget-deleted}`. A sweep with no work gives
   `{:id :deleted 0 :bytes 0}`. The claim-based rows are `:python-guest`,
   `:python-runtime` and `:python-sources`. `:python-pycache` deletes bytecode
   with no source and keeps other bytecode outside the hot roots within a budget.

   A missing directory is zero work, and a permission-denied subtree is skipped
   rather than allowed to stop the pass.

   Options, all for tests: `:days` (overrides every window), `:budget-bytes`
   (overrides every byte budget) and `:now-ms`."
  ([] (sweep-stale! nil))
  ([{:keys [now-ms] :as opts}]
   (let [pass
         (assoc (select-keys opts [:days :budget-bytes]) :now (long (or now-ms (util/now-ms))))

         all
         (into []
               (mapcat (fn [sweep]
                         (or (run-sweep! sweep pass)
                             (map (fn [id]
                                    {:id id :deleted 0 :bytes 0})
                                  (:rows sweep)))))
               sweeps)]

     (assoc (totals all) :targets all))))

(def ^:private ^:const settle-ms
  "The time a process runs before its first sweep. A short command ends before
   that, so it walks nothing and its start pays nothing."
  30000)

(defn sweep-task
  "A janitor task for the sweep `{:id :every-ms :roots :run}`, as [[tasks]] makes one
   for each sweep of `sweeps`. `:roots` answers the directories to walk at each
   question. Each root has its own stamp, so a root that appears later is walked when
   it is due. A caller whose sweep needs more than this namespace, such as the Python
   extension host, submits the task itself. The task answers nil when no root was due,
   else `{:deleted :bytes}` over the rows of the walked roots."
  [{:keys [id every-ms roots run]}]
  {:id (keyword "housekeeping" (name id))
   :delay-ms settle-ms
   :every-ms (min (quot (long every-ms) 4) (long hour-ms))
   :run (bound-fn* (fn []
                     (let [pass
                           {:now (util/now-ms)}

                           rows
                           (into
                             []
                             (mapcat #(run-sweep!
                                        {:id id :every-ms every-ms :root (constantly %) :run run}
                                        pass))
                             (roots))]

                       (when (seq rows) (totals rows)))))})

(defn tasks
  "The sweeps as tasks for `internal.janitor/submit!`: one task for each sweep, in
   report order.

   Each sweep has its own interval, `:every-ms` in `sweeps`, and the stamp of its
   root holds that interval for the whole machine. A task asks four times in each
   interval, and at least each hour, whether its sweep is due. A timer and a file
   time never agree to the millisecond, so a task that asked only one time in each
   interval would run its sweep in each second interval. A walk that a stopped
   process left unfinished starts again at the next question. A task that is not
   due answers nil, and the janitor writes no line for it. A task that walked
   answers `{:deleted :bytes}`.

   A task keeps the dynamic bindings of the caller, so the home seams of a test
   reach the worker thread."
  []
  (mapv (fn [{:keys [root] :as sweep}]
          (sweep-task (assoc sweep :roots #(vector (root)))))
        sweeps))

;; Gateway journals

(defn- scan-journals
  [^long cutoff-ms ^long now-ms]
  (let [dir
        (events-dir)

        files
        (when (.isDirectory dir)
          (->> (or (.listFiles dir) (make-array File 0))
               (filter #(.endsWith (.getName ^File %) ".ndjson"))))

        stale
        (into []
              (keep (fn [^File f]
                      (when (< (.lastModified f) cutoff-ms)
                        {:kind :journal
                         :root (canonical f)
                         :label (.getName f)
                         :last-activity-ms (.lastModified f)
                         :age-days (ms->days (- now-ms (.lastModified f)))
                         :bytes (.length f)})))
              files)]

    {:root (canonical dir)
     :file-count (count files)
     :reclaimable (vec (sort-by (comp - long #(or (:bytes %) 0)) stale))
     :bytes (reduce + 0 (map #(long (or (:bytes %) 0)) stale))}))

;; Sandbox snapshots

(defn- snapshot-owner
  "The session id in a snapshot entry name: `<id>.py`, or the `<id>.spill` or
   `<id>.turns` directory."
  [^File f]
  (let [file-name (.getName f)]
    (cond (temp-leftover? file-name) nil
          (and (.isFile f) (.endsWith file-name ".py")) (subs file-name 0 (- (count file-name) 3))
          (and (.isDirectory f) (.endsWith file-name ".spill"))
          (subs file-name 0 (- (count file-name) 6))
          (and (.isDirectory f) (.endsWith file-name ".turns"))
          (subs file-name 0 (- (count file-name) 6))
          :else nil)))

(defn- session-row?
  "True when `db-info` holds a row for session `id`, or when that is unknown. A name
   that is not a UUID cannot be a session id: tests write such names."
  [db-info ^String id]
  (if-let [uuid (parse-uuid id)]
    (try (some? (p/db-get-session db-info uuid)) (catch Throwable _ true))
    false))

(defn- scan-sandbox
  "Report snapshot files of sessions with no row, and temp files of killed writes,
   also inside spill and turn state folders. Both must be untouched since the
   cutoff. Without `db-info` no snapshot is reported, because no row cannot be
   proved. A gateway can use another DB, so this is a report for `doctor --purge`,
   never a startup sweep. A hard-linked file counts once over all items."
  [db-info ^long cutoff-ms ^long now-ms]
  (let [dir
        (sandbox-dir)

        entries
        (when (.isDirectory dir) (vec (or (.listFiles dir) (make-array File 0))))

        seen
        (java.util.HashSet.)

        item
        (fn [kind label files]
          (let [stats
                (mapv #(tree-stats % seen) files)

                newest
                (long (reduce max 0 (map :newest-ms stats)))]

            (when (< newest cutoff-ms)
              {:kind kind
               :root (canonical (first files))
               :label label
               :paths (mapv canonical files)
               :last-activity-ms newest
               :age-days (ms->days (- now-ms newest))
               :bytes (reduce + 0 (map :bytes stats))})))

        snapshots
        (when db-info
          (into []
                (keep (fn [[id files]]
                        (when-not (session-row? db-info id) (item :sandbox id files))))
                (group-by snapshot-owner (sort (filter snapshot-owner entries)))))

        ;; A temp file in the folder of a reported snapshot goes with that snapshot.
        orphans
        (into #{} (map :label) snapshots)

        temps
        (concat (filter #(and (.isFile ^File %) (temp-leftover? (.getName ^File %))) entries)
                (mapcat sandbox-temps
                        (filter #(let [owner (snapshot-owner %)] (and (.isDirectory ^File %)
                                                                      owner
                                                                      (not (orphans owner))))
                                entries)))

        relative
        (fn [^File f]
          (str (.relativize (.toPath dir) (.toPath f))))

        temp-item
        (fn [^File f]
          (let [written (temp-written-ms f)]
            (when (< written cutoff-ms)
              {:kind :sandbox-temp
               :root (canonical f)
               :label (relative f)
               :paths [(canonical f)]
               :last-activity-ms written
               :age-days (ms->days (- now-ms written))
               :bytes (freed-bytes f)})))

        reclaimable
        (into (vec snapshots) (keep temp-item) temps)]

    {:root (canonical dir)
     :file-count (count entries)
     :reclaimable (vec (sort-by (comp - long #(or (:bytes %) 0)) reclaimable))
     :bytes (reduce + 0 (map #(long (or (:bytes %) 0)) reclaimable))}))

;; Public surface

(defn scan
  "Observe stale gateway journals and sandbox snapshots. Pure: touches no state
   and never throws — a missing DB or an unreadable subtree degrades to empty
   findings.

   Options: `:db-info` (nil is fine), `:days` (defaults to `default-stale-days`)
   and `:now-ms` for tests."
  [{:keys [db-info days now-ms]}]
  (let [days
        (long (or days default-stale-days))

        now
        (long (or now-ms (util/now-ms)))

        cutoff
        (- now (* days (long day-ms)))

        journals
        (try (scan-journals cutoff now) (catch Throwable _ nil))

        sandbox
        (try (scan-sandbox db-info cutoff now) (catch Throwable _ nil))

        parts
        [journals sandbox]]

    {:days days
     :cutoff-ms cutoff
     :journals journals
     :sandbox sandbox
     :bytes (reduce + 0 (map #(long (or (:bytes %) 0)) parts))
     :count (reduce + 0 (map #(count (:reclaimable %)) parts))}))

(defn- purge-one!
  [events-root sandbox-root {:keys [kind root paths] :as item}]
  (let [ok (case kind
             :journal
             (boolean (and (under? events-root root)
                           (try (Files/deleteIfExists (.toPath (io/file root)))
                                (catch Throwable _ false))))

             ;; A snapshot file, its spill directory or a temp file, each confined
             ;; to the sandbox directory.
             (:sandbox :sandbox-temp)
             (boolean (and (seq paths)
                           (every? #(under? sandbox-root %) paths)
                           (every? #(pos? (delete-tree! (io/file %))) paths)))

             false)]
    (assoc item :is-purged ok)))

(defn purge!
  "Reclaim everything `scan` reported. Returns the scan augmented with a
   `:purged` vec (each item stamped `:is-purged`) and `:reclaimed-bytes`.

   With `:is-dry-run` true nothing is touched: `:purged` still carries the plan
   with every item stamped `:is-purged false`, so operators can look first."
  [{:keys [is-dry-run] :as opts}]
  (let [{:keys [journals sandbox] :as report}
        (scan opts)

        items
        (into (vec (:reclaimable journals)) (:reclaimable sandbox))]

    (if is-dry-run
      (assoc report
        :purged (mapv #(assoc % :is-purged false) items)
        :reclaimed-bytes 0
        :is-dry-run true)
      (let [done (mapv #(purge-one! (:root journals) (:root sandbox) %) items)]
        (assoc report
          :purged done
          :is-dry-run false
          :reclaimed-bytes
          (reduce + 0 (map #(long (or (:bytes %) 0)) (filter :is-purged done))))))))
