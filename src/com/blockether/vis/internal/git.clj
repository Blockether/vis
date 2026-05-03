(ns com.blockether.vis.internal.git
  "Shared git workspace inspection.

   UI surfaces should depend on this namespace instead of embedding their
   own JGit calls. It reports small, renderable facts about the current
   repository: whether a git workspace exists, repo/branch identity,
   dirty counts, and ahead/behind counts for the configured upstream."
  (:import [java.io File]
           [org.eclipse.jgit.api Git Status]
           [org.eclipse.jgit.lib BranchTrackingStatus Repository]
           [org.eclipse.jgit.storage.file FileRepositoryBuilder]))

(set! *warn-on-reflection* true)

(defn cwd-file
  "Canonical current working directory as a File. Indirected for tests."
  ^File []
  (.getCanonicalFile (File. ".")))

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

(defn count-status-sets
  "Count unique-ish paths across JGit status sets. JGit already de-dupes
   inside each set; callers choose which sets belong to one bucket."
  [& sets]
  (reduce + (map count sets)))

(defn dirty-counts
  "Summarise JGit Status into user-facing dirty buckets."
  [^Status status]
  {:modified (count-status-sets (.getModified status)
               (.getChanged status))
   :created  (count-status-sets (.getAdded status)
               (.getUntracked status)
               (.getUntrackedFolders status))
   :deleted  (count-status-sets (.getRemoved status)
               (.getMissing status))})

(defn tracking-counts
  "Ahead/behind counts for `branch`, or nil when there is no upstream."
  [^Repository repo branch]
  (try
    (when-let [^BranchTrackingStatus tracking (BranchTrackingStatus/of repo branch)]
      {:ahead  (.getAheadCount tracking)
       :behind (.getBehindCount tracking)})
    (catch Throwable _ nil)))

(defn workspace-status
  "Return git facts for `start` (default cwd).

   Outside git, returns `{:workspace? false}` so callers can explicitly
   render the absence of a workspace. Inside git, returns:

     {:workspace? true
      :repo "vis"
      :branch "main"
      :modified 2 :created 1 :deleted 0
      :ahead 4 :behind 0} ; ahead/behind only when upstream exists"
  ([] (workspace-status (cwd-file)))
  ([^File start]
   (if-let [^Repository repo (open-repository start)]
     (try
       (let [branch   (branch-label repo)
             tracking (tracking-counts repo branch)
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
