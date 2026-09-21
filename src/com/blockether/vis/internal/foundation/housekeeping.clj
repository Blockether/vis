(ns com.blockether.vis.internal.foundation.housekeeping
  "Retention for Vis-owned state: advisory inventory and bounded cleanup.

   ADVISORY (`scan` observes, `purge!` acts, `vis-agent doctor` renders): the
   drafts store (`~/.vis/drafts`). A draft clone is a full copy of a trunk and
   survives until someone applies or abandons it, so a machine that drafts daily
   and never abandons accumulates gigabytes of dead clones. It holds recoverable
   work, so nothing here deletes it on its own: `scan` is pure observation (no
   mutation, never throws) and `purge!` is the explicit operator action behind
   `vis-agent doctor --purge`. `scan` reports the gateway journals the same way,
   because an operator asking what is reclaimable today should see them.

   SELF-DELETING (`sweep-stale!`, once per process at startup): diagnostic logs,
   the gateway journals, the display caches and old downloaded
   Python archives. Those are DERIVED — a log of a process that exited, the wire
   replay of a turn the DB already owns, a picture whose bytes are already DB-owned
   — so they carry a window instead of a report. `sweep-targets` is the one list
   of them. Diagnostic
   logs also sweep hourly while the process runs; the other targets remain startup-only.
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

   `purge!` routes draft rows, including discarded-root retries, through
   `workspace/abandon!` so backend bookkeeping owns primary and extra-root release.
   A failed release never falls back to raw deletion. Only directories with no row
   and journal files are removed directly, confined to the drafts or events store."
  (:require [clojure.java.io :as io]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as p]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace])
  (:import [java.io File]
           [java.nio.file FileVisitResult Files LinkOption Path SimpleFileVisitor]
           [java.nio.file.attribute BasicFileAttributes]))

(def default-stale-days
  "Age past which unattended state is worth mentioning. Two weeks: long enough
   that a draft parked over a holiday is not nagged about, short enough that the
   report still arrives while the operator remembers what the draft was for."
  14)

(def ^:const day-ms 86400000)

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

(defn- events-dir
  ^File []
  (io/file (or *events-home* (io/file (System/getProperty "user.home") ".vis" "gateway" "events"))))

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
   Symlinks are never followed (draft clones may be linked back to the
   trunk; counting them would report the user's whole source tree as
   reclaimable). The mtime is what makes an orphan directory safe to judge:
   a directory's own timestamp only moves when entries are added or removed, so
   a busy clone can look untouched for weeks by that measure alone."
  [^File root]
  (try (let [total
             (java.util.concurrent.atomic.AtomicLong. 0)

             newest
             (java.util.concurrent.atomic.AtomicLong. 0)]

         (Files/walkFileTree (.toPath root)
                             (proxy [SimpleFileVisitor] []
                               (visitFile [_p ^BasicFileAttributes attrs]
                                 (when (.isRegularFile attrs)
                                   (.addAndGet total (.size attrs))
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
       (catch Throwable _ {:bytes 0 :newest-ms (.lastModified root)})))

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

(defn- exists?
  [^File f]
  (try (Files/exists (.toPath f) (into-array LinkOption [])) (catch Throwable _ false)))

;; The self-deleting sweep
;;
;; UNLIKE drafts and journals, everything below is DERIVED and nothing anyone
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
                 `~/.vis/logs`, mirroring `internal.paths/logs-dir` — the location is a
                 fixed contract shared with the sandbox grant, not a configurable."}
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
  "`~/.vis/<segs…>` unless a test seam overrides the whole root."
  ^File [^String override segs]
  (if override (io/file override) (apply io/file (System/getProperty "user.home") ".vis" segs)))

(defn- logs-dir ^File [] (home-dir *logs-home* ["logs"]))

(defn- cache-dir ^File [^String sub] (io/file (home-dir *cache-home* ["cache"]) sub))

(defn- python-dir ^File [^String sub] (io/file (home-dir *python-home* ["python"]) sub))

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
                                            (under? canon (canonical (.toFile p)))
                                            (or (contains? @prunable p)
                                                (< (.lastModified (.toFile p)) cutoff))
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
   startup sweep reclaims a candidate only once its in-use claim is free — or, for
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

(defn- add-counts
  [a b]
  {:deleted (+ (long (:deleted a)) (long (:deleted b)))
   :bytes (+ (long (:bytes a)) (long (:bytes b)))})

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
  (reduce (fn [acc ^File child]
            (let [dot? (.startsWith (.getName child) ".")]
              (cond (.isFile child) (if (and (not dot?)
                                             (< (.lastModified child) cutoff)
                                             (under? canon (canonical child)))
                                      (let [size (.length child)]
                                        (if (delete-quietly! (.toPath child))
                                          (add-counts acc {:deleted 1 :bytes size})
                                          acc))
                                      acc)
                    (or dot? (not (.isDirectory child))) acc
                    (reclaimable? child cutoff) (add-counts acc (reclaim-dir! child canon))
                    :else (reduce (fn [acc ^File snapshot]
                                    (if (and (.isDirectory snapshot)
                                             (.startsWith (.getName snapshot) "vis-ext-code")
                                             (reclaimable? snapshot cutoff))
                                      (add-counts acc (reclaim-dir! snapshot canon))
                                      acc))
                                  acc
                                  (or (.listFiles child) (make-array File 0))))))
          {:deleted 0 :bytes 0}
          (or (.listFiles root) (make-array File 0))))

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
                            (if (and (.isDirectory dir) (reclaimable? dir cutoff))
                              (add-counts acc (reclaim-dir! dir canon))
                              acc)))
                        {:deleted 0 :bytes 0}
                        candidates)]

            (merge {:id (keyword (str "python-" kind)) :root canon} counts)))
        (:targets (runtime-retention-plan))))

(def ^:private sweep-targets
  "Every directory Vis fills on its own that holds nothing anyone can recover —
   the one list, so a new producer is bounded by being added here rather than by
   a second sweep somewhere else.

   `:mode` `:files` deletes stale FILES anywhere below the root and then the
   directories they emptied. `:budget-bytes` additionally caps what survives the
   age pass."
  [{:id :logs :dir logs-dir :retention-days default-retention-days}
   {:id :gateway-events :dir events-dir :retention-days default-retention-days}
   {:id :display
    :dir #(cache-dir "display")
    :retention-days default-retention-days
    :budget-bytes default-cache-budget-bytes}
   {:id :tui-attachments
    :dir #(cache-dir "tui-attachments")
    :retention-days default-retention-days
    :budget-bytes default-cache-budget-bytes}
   ;; Releases before 0.4 kept the downloaded platform archive beside the tree
   ;; it was unpacked into; today's fetch deletes its archive once unpacked.
   {:id :python-archives :dir #(python-dir "archives") :retention-days default-retention-days}])

(defn sweep-stale!
  "Delete the aged-out derived state of every `sweep-targets` entry, then the
   Python store trees no process claims any more. Returns
   `{:targets [{:id :root :days :cutoff-ms :file-count :deleted :bytes
   :dirs-removed :over-budget-deleted}…] :deleted :bytes}` — `:deleted` counts
   entries actually removed and `:bytes` the space reclaimed. The claim-based
   rows are `:python-guest`, `:python-runtime` and `:python-sources`.

   Never throws: a missing directory is zero work, and a permission-denied
   subtree is skipped rather than allowed to take startup down.

   Options, all for tests: `:days` (overrides every target's window),
   `:budget-bytes` (overrides every byte budget) and `:now-ms`."
  ([] (sweep-stale! nil))
  ([{:keys [days now-ms] budget-override :budget-bytes}]
   (let [now
         (long (or now-ms (util/now-ms)))

         store-window
         (long (or days default-retention-days))

         store-cutoff
         (- now (* store-window (long day-ms)))

         reports
         (mapv (fn [{:keys [id dir retention-days budget-bytes]}]
                 (let [^File d
                       (dir)

                       window
                       (long (or days retention-days))

                       cutoff
                       (- now (* window (long day-ms)))

                       base
                       {:id id :root (canonical d) :days window :cutoff-ms cutoff}]

                   (if-not (.isDirectory d)
                     (merge base {:file-count 0 :deleted 0 :bytes 0 :dirs-removed 0})
                     (let [canon
                           (canonical d)

                           swept
                           (sweep-files! d canon cutoff)

                           trimmed
                           (when budget-bytes
                             (trim-to-budget! d canon (long (or budget-override budget-bytes))))]

                       (merge base
                              swept
                              (when trimmed
                                {:deleted (+ (long (:deleted swept)) (long (:deleted trimmed)))
                                 :bytes (+ (long (:bytes swept)) (long (:bytes trimmed)))
                                 :over-budget-deleted (:deleted trimmed)}))))))
               sweep-targets)

         ^File guest-root
         (python-dir "vis-guest")

         guest
         (merge {:id :python-guest
                 :root (canonical guest-root)
                 :days store-window
                 :cutoff-ms store-cutoff}
                (if (.isDirectory guest-root)
                  (sweep-guest-store! guest-root (canonical guest-root) store-cutoff)
                  {:deleted 0 :bytes 0}))

         versions
         (mapv #(merge {:days store-window :cutoff-ms store-cutoff} %)
               (prune-version-stores! store-cutoff))

         all
         (into (conj reports guest) versions)]

     {:targets all :deleted (reduce + 0 (map :deleted all)) :bytes (reduce + 0 (map :bytes all))})))

(defn- sweep-logs!
  "Repeat only diagnostic retention; other derived-state rules remain startup-only."
  [{:keys [days now-ms]}]
  (let [dir
        (logs-dir)

        now
        (long (or now-ms (util/now-ms)))

        window
        (long (or days default-retention-days))]

    (sweep-files! dir (canonical dir) (- now (* window (long day-ms))))))

(defn sweep-stale-async!
  "Start the stale-state sweep on a lowest-priority daemon thread, then repeat
   diagnostic-log cleanup hourly for this process's lifetime. Other targets are
   swept only at startup. All passes are best-effort and off the first-paint path;
   a short-lived CLI may exit before its initial pass finishes.

   Called once per process. Returns the thread; interrupt it to stop. `:interval-ms`
   overrides the hourly delay for tests. The body is a `bound-fn` so ALL home seams
   convey to every pass rather than falling back to the operator's real `~/.vis`."
  ([] (sweep-stale-async! nil))
  ([opts]
   (let [interval-ms (long (or (:interval-ms opts) default-log-sweep-interval-ms))]
     (doto (Thread. ^Runnable
                    (bound-fn []
                              (try (loop [initial? true]
                                     (when-not (.isInterrupted (Thread/currentThread))
                                       (try (if initial? (sweep-stale! opts) (sweep-logs! opts))
                                            (catch InterruptedException e (throw e))
                                            (catch Throwable _ nil))
                                       (Thread/sleep interval-ms)
                                       (recur false)))
                                   (catch InterruptedException _
                                     (.interrupt (Thread/currentThread)))))
                    "vis-stale-sweep")
       (.setDaemon true)
       (.setPriority Thread/MIN_PRIORITY)
       (.start)))))

;; Drafts

(defn- draft-activity-ms
  "Most recent moment the workspace was demonstrably alive. `last-focused-at-ms`
   is the honest signal; a never-focused draft falls back to creation."
  ^long [ws]
  (max (long (or (:last-focused-at-ms ws) 0))
       (long (or (some-> ^java.util.Date (:created-at ws)
                         .getTime)
                 0))))

(defn- draft-rows [db-info] (try (vec (p/db-workspace-list-drafts db-info)) (catch Throwable _ [])))

(defn- draft-dirs
  "Every `<drafts-root>/<repo>/<draft>` directory on disk. Dot-entries are
   Vis-internal (`.fresh-seed`, `.trash`) and are never reported as drafts."
  [^String drafts-root]
  (let [visible (fn [^File dir]
                  (->> (or (.listFiles dir) (make-array File 0))
                       (filter #(and (.isDirectory ^File %)
                                     (not (.startsWith (.getName ^File %) "."))))))]
    (when drafts-root
      (let [root (io/file drafts-root)]
        (when (.isDirectory root) (vec (mapcat visible (visible root))))))))

(defn- scan-drafts
  [db-info ^long cutoff-ms ^long now-ms]
  (let [drafts-root
        (workspace/drafts-store-path)

        rows
        (draft-rows db-info)

        by-root
        (into {}
              (mapcat (fn [ws]
                        (for [path
                              (cons (:root ws)
                                    (keep (fn [{:keys [trunk clone]}]
                                            (when (not= trunk clone) clone))
                                          (workspace/extra-root-entries ws)))

                              :when path]

                          [(canonical (io/file path)) ws])))
              rows)

        entry
        (fn [ws ^String path kind]
          (let [activity (draft-activity-ms ws)]
            (assoc (select-keys (tree-stats (io/file path)) [:bytes])
              :kind kind
              :workspace-id (:id ws)
              :label (:label ws)
              :state (:state ws)
              :root path
              :last-activity-ms (when (pos? activity) activity)
              :age-days (when (pos? activity) (ms->days (- now-ms activity))))))

        ;; Every owned clone keeps its row, even after the primary was removed.
        ;; Discarded leftovers retry backend release rather than becoming orphans.
        from-rows
        (into []
              (keep (fn [[path ws]]
                      (when (and (under? drafts-root path) (exists? (io/file path)))
                        (cond (= :discarded (:state ws)) (entry ws path :discarded)
                              (< (draft-activity-ms ws) cutoff-ms) (entry ws path :stale)
                              :else nil))))
              by-root)

        ;; A directory with no row is either debris from a crashed clone or a
        ;; store written by a different DB. Either way it is only reclaimable once
        ;; nothing inside it has been touched since the cutoff — the same bar the
        ;; DB-backed drafts have to clear.
        orphans
        (into []
              (keep (fn [^File d]
                      (let [path
                            (canonical d)

                            {:keys [bytes newest-ms]}
                            (tree-stats d)]

                        (when (and (not (contains? by-root path)) (< (long newest-ms) cutoff-ms))
                          {:kind :orphan
                           :root path
                           :label (.getName d)
                           :last-activity-ms newest-ms
                           :age-days (ms->days (- now-ms (long newest-ms)))
                           :bytes bytes}))))
              (draft-dirs drafts-root))

        reclaimable
        (into from-rows orphans)]

    {:root drafts-root
     :row-count (count rows)
     :dir-count (count (draft-dirs drafts-root))
     :reclaimable (vec (sort-by (comp - long #(or (:bytes %) 0)) reclaimable))
     :bytes (reduce + 0 (map #(long (or (:bytes %) 0)) reclaimable))}))

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

;; Public surface

(defn scan
  "Observe stale drafts and gateway journals. Pure: touches no state and never
   throws — a missing DB, an absent drafts store, or an unreadable subtree all
   degrade to empty findings.

   Options: `:db-info` (nil is fine, drafts then reduce to on-disk orphans),
   `:days` (defaults to `default-stale-days`) and `:now-ms` for tests."
  [{:keys [db-info days now-ms]}]
  (let [days
        (long (or days default-stale-days))

        now
        (long (or now-ms (util/now-ms)))

        cutoff
        (- now (* days (long day-ms)))

        drafts
        (try (scan-drafts db-info cutoff now) (catch Throwable _ nil))

        journals
        (try (scan-journals cutoff now) (catch Throwable _ nil))]

    {:days days
     :cutoff-ms cutoff
     :drafts drafts
     :journals journals
     :bytes (+ (long (or (:bytes drafts) 0)) (long (or (:bytes journals) 0)))
     :count (+ (count (:reclaimable drafts)) (count (:reclaimable journals)))}))

(defn- purge-one!
  [db-info drafts-root events-root {:keys [kind root workspace-id] :as item}]
  (let [ok (case kind
             ;; Retry discarded rows through the same backend as live drafts;
             ;; raw deletion bypasses backend bookkeeping and cleanup refusals.
             (:stale :discarded)
             (try (let [{:keys [discard-future]} (workspace/abandon! db-info
                                                                     {:workspace-id workspace-id
                                                                      :reason :housekeeping})]
                    (when discard-future (deref discard-future 30000 nil))
                    (Files/notExists (.toPath (io/file root)) (make-array LinkOption 0)))
                  (catch Throwable _ false))

             :orphan
             (boolean (and (under? drafts-root root) (pos? (delete-tree! (io/file root)))))

             :journal
             (boolean (and (under? events-root root)
                           (try (Files/deleteIfExists (.toPath (io/file root)))
                                (catch Throwable _ false))))

             false)]
    (assoc item :is-purged ok)))

(defn purge!
  "Reclaim everything `scan` reported. Returns the scan augmented with a
   `:purged` vec (each item stamped `:is-purged`) and `:reclaimed-bytes`.

   With `:is-dry-run` true nothing is touched: `:purged` still carries the plan
   with every item stamped `:is-purged false`, so operators can look first."
  [{:keys [db-info is-dry-run] :as opts}]
  (let [{:keys [drafts journals] :as report}
        (scan opts)

        items
        (into (vec (:reclaimable drafts)) (:reclaimable journals))]

    (if is-dry-run
      (assoc report
        :purged (mapv #(assoc % :is-purged false) items)
        :reclaimed-bytes 0
        :is-dry-run true)
      (let [done (mapv #(purge-one! db-info (:root drafts) (:root journals) %) items)]
        (assoc report
          :purged done
          :is-dry-run false
          :reclaimed-bytes
          (reduce + 0 (map #(long (or (:bytes %) 0)) (filter :is-purged done))))))))
