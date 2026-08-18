(ns com.blockether.vis.internal.foundation.housekeeping
  "Retention for the Vis-owned directories that grow without bound — the one
   nobody may delete for you, and the five that delete themselves.

   ADVISORY (`scan` observes, `purge!` acts, `vis-agent doctor` renders): the
   drafts store (`~/.vis/drafts`). A draft clone is a full copy of a trunk and
   survives until someone applies or abandons it, so a machine that drafts daily
   and never abandons accumulates gigabytes of dead clones. It holds recoverable
   work, so nothing here deletes it on its own: `scan` is pure observation (no
   mutation, never throws) and `purge!` is the explicit operator action behind
   `vis-agent doctor --purge`. `scan` reports the gateway journals the same way,
   because an operator asking what is reclaimable today should see them.

   SELF-DELETING (`sweep-stale!`, once per process at startup): diagnostic logs,
   the gateway journals, the display caches and the rewind stores. Those are
   DERIVED — a log of a process that exited, the wire replay of a turn the DB
   already owns, a picture whose bytes are already DB-owned, the pre-image of an
   edit nobody will rewind a fortnight later — so they carry a window instead of
   a report. `sweep-targets` is the one list of them. Journals also self-sweep
   inside the tailer loop (`gateway.bus/sweep!`) after a single idle day, but
   that is a LIVENESS rule and it only runs while a daemon does — journals from
   crashed or never-restarted daemons used to stay forever, and startup is
   exactly when no daemon is running.

   `purge!` routes deletions through `workspace/abandon!` for live draft rows so
   the DB transition, hooks, and backend root release all happen exactly as they
   would from `/draft abandon`. Only rows already `:discarded`, directories with
   no row at all, and journal files are removed directly — and every direct
   delete is confined to a path under the drafts store or the events dir."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.persistance :as p]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import [java.io File]
           [java.nio.file FileVisitResult Files LinkOption Path SimpleFileVisitor]
           [java.nio.file.attribute BasicFileAttributes]))

(def default-stale-days
  "Age past which unattended state is worth mentioning. Two weeks: long enough
   that a draft parked over a holiday is not nagged about, short enough that the
   report still arrives while the operator remembers what the draft was for."
  14)

(def ^:const day-ms 86400000)

(defn format-bytes
  "Human byte size. Locale-stable — explicit Locale.US so output is
   deterministic across machines (no `253,1 MB` from a comma-decimal locale)."
  [^long n]
  (cond (< n 1024) (str n " B")
        (< n (* 1024 1024))
        (String/format java.util.Locale/US "%.1f KB" (object-array [(/ (double n) 1024.0)]))
        (< n (* 1024 1024 1024)) (String/format java.util.Locale/US
                                                "%.1f MB"
                                                (object-array [(/ (double n) (* 1024.0 1024.0))]))
        :else (String/format java.util.Locale/US
                             "%.1f GB"
                             (object-array [(/ (double n) (* 1024.0 1024.0 1024.0))]))))

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
;; bytes are already DB-owned, the pre-image of an edit nobody will rewind a
;; month later. Each of these directories gains an entry per shell command, per
;; rendered figure, per edited file — forever — so each one needs a window, and
;; `sweep-targets` is the ONE place that lists them.
;;
;; `~/.vis/logs` used to be swept at its TOP LEVEL only, which is precisely
;; where the shell logs are not: `shell` writes `logs/shell/<run>/<id>.log`, one
;; directory per command, and a single week of those outweighed everything the
;; sweep could see.

(def default-retention-days
  "Age past which any self-deleting derived artifact is deleted automatically —
   diagnostic logs, gateway journals, the display caches, the rewind stores. Two
   weeks: longer than any plausible debugging or rewind window (a bug reported on
   Friday is still readable the Monday after next), short enough that a machine
   which never restarts does not carry a quarter of dead sessions. ONE number for
   every kind on purpose — a per-kind window is a promise nobody audits, and each
   kind is reconstructible from the DB or from nothing at all."
  14)

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
    "Test seam for the rewind store root. `nil` (production) resolves to
                 `~/.vis/rewind`, mirroring `foundation.rewind/*store-root*`."}
  *rewind-home*
  nil)

(defn- home-dir
  "`~/.vis/<segs…>` unless a test seam overrides the whole root."
  ^File [^String override segs]
  (if override (io/file override) (apply io/file (System/getProperty "user.home") ".vis" segs)))

(defn- logs-dir ^File [] (home-dir *logs-home* ["logs"]))

(defn- cache-dir ^File [^String sub] (io/file (home-dir *cache-home* ["cache"]) sub))

(defn- rewind-dir ^File [] (home-dir *rewind-home* ["rewind"]))

(defn- delete-quietly!
  "Delete one path, answering true when this call removed it. A directory that
   is not empty, or a file another process already took, is not an error here."
  [^Path p]
  (try (Files/deleteIfExists p) (catch Throwable _ false)))

(defn- sweep-files!
  "Delete every regular file under `root` older than `cutoff`, then every
   directory those deletions emptied — `root` itself excepted. Symlinks are
   never followed (`walkFileTree` does not by default) and every candidate is
   re-checked with `under?` against `canon`, so a hostile link cannot walk the
   delete out of the tree. Returns `{:file-count :deleted :bytes :dirs-removed}`."
  [^File root ^String canon ^long cutoff]
  (let [files
        (java.util.concurrent.atomic.AtomicLong. 0)

        deleted
        (java.util.concurrent.atomic.AtomicLong. 0)

        bytes
        (java.util.concurrent.atomic.AtomicLong. 0)

        dirs
        (java.util.concurrent.atomic.AtomicLong. 0)]

    (try (Files/walkFileTree (.toPath root)
                             (proxy [SimpleFileVisitor] []
                               (visitFile [^Path p ^BasicFileAttributes attrs]
                                 (when (.isRegularFile attrs)
                                   (.incrementAndGet files)
                                   (let [size (.size attrs)]
                                     (when (and (< (.toMillis (.lastModifiedTime attrs)) cutoff)
                                                (under? canon (canonical (.toFile p)))
                                                (delete-quietly! p))
                                       (.incrementAndGet deleted)
                                       (.addAndGet bytes size))))
                                 FileVisitResult/CONTINUE)
                               (visitFileFailed [_p _e] FileVisitResult/CONTINUE)
                               (postVisitDirectory [^Path p _e]
                                 (when (and (not= (.toFile p) root)
                                            (under? canon (canonical (.toFile p)))
                                            (delete-quietly! p))
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

(defn- sweep-stores!
  "Delete every immediate child DIRECTORY of `root` whose newest file predates
   `cutoff` — a whole per-session store at a time, judged the way `scan` judges
   an orphan draft, because a live session touches its journal constantly.
   Returns `{:file-count :deleted :bytes :dirs-removed}`."
  [^File root ^String canon ^long cutoff]
  (let [stores (->> (or (.listFiles root) (make-array File 0))
                    (filter (fn [^File f]
                              (and (.isDirectory f) (not (.startsWith (.getName f) ".")))))
                    vec)]
    (reduce (fn [acc ^File d]
              (let [{:keys [bytes newest-ms]} (tree-stats d)]
                (if (and (< (long newest-ms) cutoff)
                         (under? canon (canonical d))
                         (pos? (delete-tree! d)))
                  (-> acc
                      (update :deleted inc)
                      (update :dirs-removed inc)
                      (update :bytes + (long bytes)))
                  acc)))
            {:file-count (count stores) :deleted 0 :bytes 0 :dirs-removed 0}
            stores)))

(def ^:private sweep-targets
  "Every directory Vis fills on its own that holds nothing anyone can recover —
   the one list, so a new producer is bounded by being added here rather than by
   a second sweep somewhere else.

   `:mode` `:files` deletes stale FILES anywhere below the root and then the
   directories they emptied; `:stores` deletes a whole per-session subtree at a
   time. `:budget-bytes` additionally caps what survives the age pass."
  [{:id :logs :mode :files :dir logs-dir :retention-days default-retention-days}
   {:id :gateway-events :mode :files :dir events-dir :retention-days default-retention-days}
   {:id :display
    :mode :files
    :dir #(cache-dir "display")
    :retention-days default-retention-days
    :budget-bytes default-cache-budget-bytes}
   {:id :tui-attachments
    :mode :files
    :dir #(cache-dir "tui-attachments")
    :retention-days default-retention-days
    :budget-bytes default-cache-budget-bytes}
   {:id :rewind :mode :stores :dir rewind-dir :retention-days default-retention-days}])

(defn sweep-stale!
  "Delete the aged-out derived state of every `sweep-targets` entry. Returns
   `{:targets [{:id :root :days :cutoff-ms :file-count :deleted :bytes
   :dirs-removed :over-budget-deleted}…] :deleted :bytes}` — `:deleted` counts
   entries actually removed and `:bytes` the space reclaimed.

   Never throws: a missing directory is zero work, and a permission-denied
   subtree is skipped rather than allowed to take startup down.

   Options, all for tests: `:days` (overrides every target's window),
   `:budget-bytes` (overrides every byte budget) and `:now-ms`."
  ([] (sweep-stale! nil))
  ([{:keys [days now-ms] budget-override :budget-bytes}]
   (let [now
         (long (or now-ms (System/currentTimeMillis)))

         reports
         (mapv
           (fn [{:keys [id mode dir retention-days budget-bytes]}]
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
                       (if (= :stores mode)
                         (sweep-stores! d canon cutoff)
                         (sweep-files! d canon cutoff))

                       trimmed
                       (when budget-bytes
                         (trim-to-budget! d canon (long (or budget-override budget-bytes))))]

                   (merge base
                          swept
                          (when trimmed
                            {:deleted (+ (long (:deleted swept)) (long (:deleted trimmed)))
                             :bytes (+ (long (:bytes swept)) (long (:bytes trimmed)))
                             :over-budget-deleted (:deleted trimmed)}))))))
           sweep-targets)]

     {:targets reports
      :deleted (reduce + 0 (map :deleted reports))
      :bytes (reduce + 0 (map :bytes reports))})))

(defn sweep-stale-async!
  "Fire-and-forget `sweep-stale!` on a lowest-priority daemon thread. Called once
   per process at startup: a few thousand `File` stats are trivial but they are
   still disk I/O on the path to first paint, and a sweep that loses the race
   with a short-lived `vis-agent --version` simply runs on the next start.
   Returns the thread.

   The body is a `bound-fn` so the three home seams CONVEY: a new thread
   otherwise sees only root bindings, which would make a test's temp-dir binding
   silently sweep the operator's real `~/.vis`. Production binds nothing, so the
   conveyance is free."
  ([] (sweep-stale-async! nil))
  ([opts]
   (doto (Thread. ^Runnable (bound-fn* #(try (sweep-stale! opts) (catch Throwable _ nil)))
                  "vis-stale-sweep")
     (.setDaemon true)
     (.setPriority Thread/MIN_PRIORITY)
     (.start))))


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
              (keep (fn [ws]
                      (when-let [r (:root ws)]
                        [(canonical (io/file r)) ws]))
                    rows))

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

        ;; Rows whose clone is still on disk and whose last sign of life is
        ;; older than the cutoff. A :discarded row with a surviving directory is
        ;; always reclaimable — the async root release did not finish.
        from-rows
        (into []
              (keep (fn [ws]
                      (let [path (some-> (:root ws)
                                         io/file
                                         canonical)]
                        (when (and path (under? drafts-root path) (exists? (io/file path)))
                          (cond (= :discarded (:state ws)) (entry ws path :discarded)
                                (< (draft-activity-ms ws) cutoff-ms) (entry ws path :stale)
                                :else nil)))))
              rows)

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
        (long (or now-ms (System/currentTimeMillis)))

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
             ;; A live draft row goes out the front door: state transition, hooks,
             ;; and backend-owned root release, identical to `/draft abandon`.
             :stale
             (try (let [{:keys [discard-future]} (workspace/abandon! db-info
                                                                     {:workspace-id workspace-id
                                                                      :reason :housekeeping})]
                    (when discard-future (deref discard-future 30000 nil))
                    (when (exists? (io/file root))
                      (when (under? drafts-root root) (delete-tree! (io/file root))))
                    true)
                  (catch Throwable _ false))

             (:discarded :orphan)
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
