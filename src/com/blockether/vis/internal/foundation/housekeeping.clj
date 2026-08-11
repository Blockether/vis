(ns com.blockether.vis.internal.foundation.housekeeping
  "Stale-state accounting for the two Vis-owned directories that grow without
   bound: the drafts store (`~/.vis/drafts`) and the gateway journals
   (`~/.vis/gateway/events`).

   Both are legitimately unbounded by design. A draft clone is a full copy of a
   trunk and survives until someone applies or abandons it, so a machine that
   drafts daily and never abandons accumulates gigabytes of dead clones. Gateway
   journals self-sweep inside the tailer loop (`gateway.bus/sweep!`), but only
   while a daemon is actually running — journals from crashed or never-restarted
   daemons stay forever.

   Neither is an error, so nothing here deletes on its own: `scan` is pure
   observation (no mutation, never throws) that `vis-agent doctor` renders, and
   `purge!` is the explicit operator action behind `vis-agent doctor --purge`.

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

(defn- ms->days ^long [^long ms] (long (quot ms day-ms)))

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

;; ---------------------------------------------------------------------------
;; Filesystem helpers. Every one of these swallows IO failure: housekeeping is
;; advisory, and a permission-denied subtree must not take `vis-agent doctor` down.
;; ---------------------------------------------------------------------------

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
  (try (let
         [total
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

;; ---------------------------------------------------------------------------
;; Diagnostic logs
;;
;; UNLIKE drafts and journals this one deletes on its own. A log file carries no
;; recoverable work: `~/.vis/logs` is a dedicated write-only sink (nrepl session
;; logs, JFR dumps) whose entries are never referenced again once the process
;; that wrote them is gone, and it grows one file per REPL start forever —
;; hundreds of mostly-empty files accumulate within weeks. Nothing bounded it,
;; because the Telemere rolling handler only bounds the CURRENT process's
;; `~/.vis/logs/vis-<pid>.log`, and every exited process leaves its own behind.
;; ---------------------------------------------------------------------------

(def default-log-retention-days
  "Age past which a diagnostic log is deleted automatically. Three weeks: longer
   than any plausible debugging window (a bug reported on Friday is still
   readable the Monday after next), short enough that the directory stays
   navigable and a `grep` over it does not walk a year of dead sessions."
  21)

(def
  ^:dynamic
  ^{:doc
    "Test seam for the diagnostic log directory. `nil` (production) resolves to
                 `~/.vis/logs`, mirroring `internal.paths/logs-dir` — the location is a
                 fixed contract shared with the sandbox grant, not a configurable."}
  *logs-home*
  nil)

(defn- logs-dir
  ^File []
  (io/file (or *logs-home* (io/file (System/getProperty "user.home") ".vis" "logs"))))

(defn sweep-logs!
  "Delete every stale regular file directly under `~/.vis/logs`. Returns
   `{:root :days :cutoff-ms :file-count :deleted :bytes}` — `:deleted` counts
   files actually removed and `:bytes` the space reclaimed.

   Never throws and never recurses: only immediate children are considered, only
   regular files (a symlink or subdirectory is left alone), and every candidate
   is re-checked with `under?` against the canonical logs root so a hostile
   symlinked entry cannot walk the delete out of the directory.

   Options: `:days` (defaults to `default-log-retention-days`) and `:now-ms` for
   tests."
  ([] (sweep-logs! nil))
  ([{:keys [days now-ms]}]
   (let
     [days
      (long (or days default-log-retention-days))

      now
      (long (or now-ms (System/currentTimeMillis)))

      cutoff
      (- now (* days day-ms))

      dir
      (logs-dir)

      root
      (canonical dir)

      files
      (try (when (.isDirectory dir) (vec (or (.listFiles dir) (make-array File 0))))
           (catch Throwable _ nil))

      result
      (reduce (fn [acc ^File f]
                (try (let
                       [p
                        (.toPath f)

                        regular?
                        (Files/isRegularFile p (into-array LinkOption []))

                        link?
                        (Files/isSymbolicLink p)

                        size
                        (.length f)]

                       (if (and regular?
                                (not link?)
                                (< (.lastModified f) cutoff)
                                (under? root (canonical f))
                                (Files/deleteIfExists p))
                         (-> acc
                             (update :deleted inc)
                             (update :bytes + size))
                         acc))
                     (catch Throwable _ acc)))
              {:deleted 0 :bytes 0}
              files)]

     (assoc result
       :root root
       :days days
       :cutoff-ms cutoff
       :file-count (count files)))))

(defn sweep-logs-async!
  "Fire-and-forget `sweep-logs!` on a lowest-priority daemon thread. Called once
   per process at startup: hundreds of `File` stats are trivial but they are
   still disk I/O on the path to first paint, and a sweep that loses the race
   with a short-lived `vis-agent --version` simply runs on the next start. Returns the
   thread.

   The body is a `bound-fn` so `*logs-home*` CONVEYS: a new thread otherwise
   sees only root bindings, which would make a test's temp-dir binding silently
   sweep the operator's real `~/.vis/logs`. Production binds nothing, so the
   conveyance is free."
  ([] (sweep-logs-async! nil))
  ([opts]
   (doto (Thread. ^Runnable (bound-fn* #(try (sweep-logs! opts) (catch Throwable _ nil)))
                  "vis-log-sweep")
     (.setDaemon true)
     (.setPriority Thread/MIN_PRIORITY)
     (.start))))


;; ---------------------------------------------------------------------------
;; Drafts
;; ---------------------------------------------------------------------------

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
  (let
    [visible (fn [^File dir]
               (->> (or (.listFiles dir) (make-array File 0))
                    (filter #(and (.isDirectory ^File %)
                                  (not (.startsWith (.getName ^File %) "."))))))]
    (when drafts-root
      (let [root (io/file drafts-root)]
        (when (.isDirectory root) (vec (mapcat visible (visible root))))))))

(defn- scan-drafts
  [db-info ^long cutoff-ms ^long now-ms]
  (let
    [drafts-root
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
                   (let
                     [path (some-> (:root ws)
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
                   (let
                     [path
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

;; ---------------------------------------------------------------------------
;; Gateway journals
;; ---------------------------------------------------------------------------

(defn- scan-journals
  [^long cutoff-ms ^long now-ms]
  (let
    [dir
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

;; ---------------------------------------------------------------------------
;; Public surface
;; ---------------------------------------------------------------------------

(defn scan
  "Observe stale drafts and gateway journals. Pure: touches no state and never
   throws — a missing DB, an absent drafts store, or an unreadable subtree all
   degrade to empty findings.

   Options: `:db-info` (nil is fine, drafts then reduce to on-disk orphans),
   `:days` (defaults to `default-stale-days`) and `:now-ms` for tests."
  [{:keys [db-info days now-ms]}]
  (let
    [days
     (long (or days default-stale-days))

     now
     (long (or now-ms (System/currentTimeMillis)))

     cutoff
     (- now (* days day-ms))

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
  (let
    [ok (case kind
          ;; A live draft row goes out the front door: state transition, hooks,
          ;; and backend-owned root release, identical to `/draft abandon`.
          :stale
          (try (let
                 [{:keys [discard-future]}
                  (workspace/abandon! db-info {:workspace-id workspace-id :reason :housekeeping})]
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
  (let
    [{:keys [drafts journals] :as report}
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
