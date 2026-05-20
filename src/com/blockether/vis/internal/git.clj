(ns com.blockether.vis.internal.git
  "Shared JGit workspace inspection.

   UI surfaces and extensions should depend on this namespace instead of
   embedding their own JGit or shell-git calls. It reports small,
   renderable facts about the current repository and exposes read-only
   primitives for status, diff and log. Workspace lifecycle mutations
   stay in `com.blockether.vis.internal.workspace` because git worktree
   porcelain is still shell-backed there."
  (:require [com.blockether.vis.internal.workspace :as workspace])
  (:import [java.io ByteArrayOutputStream File]
           [org.eclipse.jgit.api Git Status]
           [org.eclipse.jgit.diff DiffEntry DiffFormatter Edit RawTextComparator]
           [org.eclipse.jgit.lib BranchTrackingStatus Repository]
           [org.eclipse.jgit.revwalk RevCommit RevWalk]
           [org.eclipse.jgit.storage.file FileRepositoryBuilder]
           [org.eclipse.jgit.treewalk AbstractTreeIterator CanonicalTreeParser FileTreeIterator]))

(set! *warn-on-reflection* true)

(def ^:private default-cache-ms 5000)

(defonce ^:private working-tree-status-cache
  (atom {:cwd nil :expires-at 0 :value nil}))

(defn cwd-file
  "Canonical current working directory as a File. Indirected for tests."
  ^File []
  (.getCanonicalFile (workspace/cwd)))

(defn open-repository
  "Open the git repository containing `start`, or nil when `start` is
   outside git. Caller owns closing the returned Repository."
  ^Repository [^File start]
  (try
    (let [^FileRepositoryBuilder builder (FileRepositoryBuilder.)]
      (.. builder
        (findGitDir start)
        readEnvironment)
      (when (.getGitDir builder)
        (.build builder)))
    (catch Throwable _ nil)))

(defn in-repository?
  "True when `start` is inside a git repository. Uses JGit only."
  [^File start]
  (boolean
    (when-let [^Repository repo (open-repository start)]
      (try true
        (finally
          (try (.close repo) (catch Throwable _ nil)))))))

(defn repo-name
  "Human label for a repository, currently its work-tree directory name."
  [^Repository repo]
  (some-> repo .getWorkTree .getName))

(defn branch-label
  "Current branch label. Detached HEADs are shortened to `detached:<sha>`."
  [^Repository repo]
  (let [branch (.getBranch repo)]
    (if (and branch (re-matches #"(?i)[0-9a-f]{40}" branch))
      (str "detached:" (subs branch 0 8))
      (or branch "unknown"))))

(defn head-id
  "Full HEAD object id, or nil when unavailable."
  [^Repository repo]
  (try
    (some-> (.resolve repo "HEAD") .getName)
    (catch Throwable _ nil)))

(defn count-status-sets
  "Count unique-ish paths across JGit status sets. JGit already de-dupes
   inside each set; callers choose which sets belong to one bucket."
  [& sets]
  (reduce + (map count sets)))

(defn status-counts
  "Detailed, environment-facing counters from JGit Status."
  [^Status status]
  (let [modified    (count (.getModified status))
        untracked   (count (.getUntracked status))
        added       (count (.getAdded status))
        changed     (count (.getChanged status))
        missing     (count (.getMissing status))
        removed     (count (.getRemoved status))
        conflicting (count (.getConflicting status))
        changes?    (boolean (some pos? [modified untracked added changed
                                         missing removed conflicting]))]
    {:clean?      (.isClean status)
     :dirty?      (not (.isClean status))
     :changes?    changes?
     :modified    modified
     :untracked   untracked
     :added       added
     :changed     changed
     :missing     missing
     :removed     removed
     :conflicting conflicting}))

(defn dirty-counts
  "Summarise JGit Status into user-facing dirty buckets."
  [^Status status]
  (if (.isClean status)
    {:modified 0
     :created  0
     :deleted  0}
    {:modified (count-status-sets (.getModified status)
                 (.getChanged status))
     :created  (count-status-sets (.getAdded status)
                 (.getUntracked status)
                 (.getUntrackedFolders status))
     :deleted  (count-status-sets (.getRemoved status)
                 (.getMissing status))}))

(defn tracking-counts
  "Ahead/behind counts for `branch`, or nil when there is no upstream."
  [^Repository repo branch]
  (try
    (when-let [^BranchTrackingStatus tracking (BranchTrackingStatus/of repo branch)]
      {:upstream? true
       :ahead  (.getAheadCount tracking)
       :behind (.getBehindCount tracking)})
    (catch Throwable _ nil)))

(defn- sorted-entries
  [status paths]
  (mapv (fn [path] {:status status :file path}) (sort paths)))

(defn status-entries
  "Porcelain-like status entries from JGit Status.

   Status codes intentionally stay compact (`M`, `A`, `D`, `??`, `UU`)
   because callers render summaries, not exact index/worktree XY state."
  [^Status status]
  (->> (concat
         (sorted-entries "A"  (.getAdded status))
         (sorted-entries "M"  (.getChanged status))
         (sorted-entries "M"  (.getModified status))
         (sorted-entries "D"  (.getMissing status))
         (sorted-entries "D"  (.getRemoved status))
         (sorted-entries "??" (.getUntracked status))
         (sorted-entries "??" (.getUntrackedFolders status))
         (sorted-entries "UU" (.getConflicting status)))
    distinct
    vec))

(defn status-snapshot
  "Read branch, head and porcelain-like entries for `start` with JGit."
  [^File start]
  (when-let [^Repository repo (open-repository start)]
    (try
      (let [^Git git    (Git/wrap repo)
            ^Status st (try
                         (.. git status call)
                         (finally
                           (try (.close git) (catch Throwable _ nil))))]
        {:branch  (branch-label repo)
         :head    (head-id repo)
         :clean?  (.isClean st)
         :entries (status-entries st)})
      (finally
        (try (.close repo) (catch Throwable _ nil))))))

(defn- tree-parser
  ^CanonicalTreeParser [^Repository repo rev]
  (when-let [commit-id (.resolve repo ^String rev)]
    (let [parser (CanonicalTreeParser.)]
      (with-open [reader (.newObjectReader repo)
                  walk   (RevWalk. repo)]
        (let [commit (.parseCommit walk commit-id)]
          (.reset parser reader (.getId (.getTree commit)))))
      parser)))

(defn- diff-path
  [^DiffEntry entry]
  (case (some-> (.getChangeType entry) .name)
    "DELETE" (.getOldPath entry)
    (.getNewPath entry)))

(defn- entry-numstat
  [^DiffFormatter formatter ^DiffEntry entry]
  (let [path (diff-path entry)]
    (try
      (let [edits (.toEditList (.toFileHeader formatter entry))
            adds  (reduce (fn [n ^Edit edit]
                            (+ n (- (.getEndB edit) (.getBeginB edit))))
                    0 edits)
            dels  (reduce (fn [n ^Edit edit]
                            (+ n (- (.getEndA edit) (.getBeginA edit))))
                    0 edits)]
        {:file path :+ adds :- dels})
      (catch Throwable _
        {:file path :+ 0 :- 0}))))

(defn- untracked-paths
  [^Repository repo]
  (try
    (let [^Git git (Git/wrap repo)]
      (try
        (let [^Status status (.. git status call)]
          (set (concat (.getUntracked status) (.getUntrackedFolders status))))
        (finally
          (try (.close git) (catch Throwable _ nil)))))
    (catch Throwable _ #{})))

(defn diff-numstat
  "Return git-numstat-like entries using JGit.

   `old-rev` is required. `new-rev` nil means compare `old-rev` to the
   working tree, matching `git diff <old-rev>` for tracked files."
  [^File start old-rev new-rev]
  (when-let [^Repository repo (open-repository start)]
    (try
      (with-open [out       (ByteArrayOutputStream.)
                  formatter (doto (DiffFormatter. out)
                              (.setRepository repo)
                              (.setDiffComparator RawTextComparator/DEFAULT)
                              (.setDetectRenames true))]
        (if-let [old-tree (tree-parser repo old-rev)]
          (let [^AbstractTreeIterator new-tree (if new-rev
                                                 (tree-parser repo new-rev)
                                                 (FileTreeIterator. repo))
                untracked (when-not new-rev (untracked-paths repo))]
            (if new-tree
              (->> (.scan formatter ^AbstractTreeIterator old-tree new-tree)
                (remove #(contains? untracked (diff-path %)))
                (mapv #(entry-numstat formatter %)))
              []))
          []))
      (finally
        (try (.close repo) (catch Throwable _ nil))))))

(defn recent-commits
  "Recent commits for `start`, newest first, using JGit only."
  [^File start n]
  (when-let [^Repository repo (open-repository start)]
    (try
      (let [^Git git (Git/wrap repo)]
        (try
          (->> (.. git log (setMaxCount (int (max 1 n))) call)
            (mapv (fn [^RevCommit commit]
                    (let [author (.getAuthorIdent commit)]
                      {:sha     (some-> (.getId commit) .getName)
                       :author  (some-> author .getName)
                       :at      (long (.getCommitTime commit))
                       :subject (.getShortMessage commit)}))))
          (finally
            (try (.close git) (catch Throwable _ nil)))))
      (finally
        (try (.close repo) (catch Throwable _ nil))))))

(defn working-tree-status
  "Return git facts for `start` (default cwd).

   Outside git, returns `{:workspace? false}` so callers can explicitly
   render the absence of a workspace. Inside git, returns:

     {:workspace? true
      :repo <repo-name>
      :branch <branch-name>
      :modified 2 :created 1 :deleted 0
      :upstream? true :ahead 4 :behind 0}"
  ([] (working-tree-status (cwd-file)))
  ([^File start]
   (if-let [^Repository repo (open-repository start)]
     (try
       (let [branch   (branch-label repo)
             tracking (or (tracking-counts repo branch)
                        {:upstream? false :ahead 0 :behind 0})
             ^Git git (Git/wrap repo)
             ^Status status (try
                              (.. git status call)
                              (finally
                                (try (.close git) (catch Throwable _ nil))))]
         (merge {:workspace? true
                 :repo (repo-name repo)
                 :branch branch}
           (dirty-counts status)
           tracking))
       (catch Throwable _
         {:workspace? false})
       (finally
         (try (.close repo) (catch Throwable _ nil))))
     {:workspace? false})))

(defn cached-working-tree-status
  "Cached `working-tree-status` for hot render paths.

   The TUI footer repaints often, and other surfaces will need the same
   git workspace facts. Keep the cache here so every caller shares one
   resolved view instead of each UI namespace running JGit independently."
  ([] (cached-working-tree-status (System/currentTimeMillis) default-cache-ms))
  ([^File start]
   (cached-working-tree-status start (System/currentTimeMillis) default-cache-ms))
  ([now-ms ttl-ms]
   (let [cwd (.getPath (cwd-file))
         {:keys [expires-at value]} @working-tree-status-cache]
     (if (and (= cwd (:cwd @working-tree-status-cache))
           (< (long now-ms) (long expires-at)))
       value
       (let [value (working-tree-status)]
         (reset! working-tree-status-cache {:cwd cwd
                                            :expires-at (+ (long now-ms) (long ttl-ms))
                                            :value value})
         value))))
  ([^File start now-ms ttl-ms]
   (let [cwd (.getPath (.getCanonicalFile start))
         {:keys [expires-at value]} @working-tree-status-cache]
     (if (and (= cwd (:cwd @working-tree-status-cache))
           (< (long now-ms) (long expires-at)))
       value
       (let [value (working-tree-status start)]
         (reset! working-tree-status-cache {:cwd cwd
                                            :expires-at (+ (long now-ms) (long ttl-ms))
                                            :value value})
         value)))))
