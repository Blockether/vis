(ns com.blockether.vis.ext.foundation.environment.git
  "JGit-backed introspection for the environment block.

   Returns a snapshot map for the repository that contains `start`
   (typically the JVM working directory). nil when `start` is not
   inside any git repository. Never throws — every JGit call is
   guarded; on any failure we degrade gracefully to nil or a
   reduced-shape map.

   The expensive call is `git status` (full working-tree walk). It
   has a configurable wall-time guard (`status-timeout-ms`); when
   the deadline trips, the snapshot drops the dirty-status fields
   instead of stalling the system-prompt build."
  (:require
   [clojure.java.io :as io])
  (:import
   (java.io File)
   (java.util.concurrent ExecutionException Future TimeUnit TimeoutException)
   (org.eclipse.jgit.api Git Status)
   (org.eclipse.jgit.lib Repository)
   (org.eclipse.jgit.storage.file FileRepositoryBuilder)))

(set! *warn-on-reflection* true)

(def ^:const default-status-timeout-ms
  "Hard ceiling on the JGit status walk. Two seconds is enough for
   most repos; large monorepos with many untracked files can blow
   past it, in which case the snapshot returns without dirty-status
   fields rather than blocking the LLM turn."
  2000)

(defn- open-repository ^Repository [^File start]
  (try
    (let [^FileRepositoryBuilder builder (FileRepositoryBuilder.)]
      (.. builder
        (findGitDir start)
        readEnvironment)
      (when (.getGitDir builder)
        (.build builder)))
    (catch Throwable _ nil)))

(defn- detached-head?
  "True when the repository HEAD is not on a branch.
   `getFullBranch` returns the full ref name on a branch and the
   commit SHA when detached."
  [^Repository repo]
  (try
    (let [full (.getFullBranch repo)]
      (cond
        (nil? full) true
        (.startsWith ^String full "refs/") false
        :else true))
    (catch Throwable _ false)))

(defn- short-head [^Repository repo]
  (try
    (let [full (.getFullBranch repo)]
      (when full
        (subs full 0 (min 12 (count ^String full)))))
    (catch Throwable _ nil)))

(defn- worktree?
  "A linked worktree has a `.git` FILE (with `gitdir:` pointer)
   instead of a directory at the working-tree root."
  [^File worktree-root]
  (try
    (let [dot-git (io/file worktree-root ".git")]
      (and (.exists dot-git) (.isFile dot-git)))
    (catch Throwable _ false)))

(defn- submodules-present? [^File worktree-root]
  (try
    (.exists (io/file worktree-root ".gitmodules"))
    (catch Throwable _ false)))

(defn- safe-count [coll]
  (if coll (count coll) 0))

(defn- collect-status
  "Run `git status` with a wall-time deadline. Returns a status map
   on success, nil on timeout/failure. Status walks the working
   tree, which can be slow on large dirty trees — the side-thread
   + `Future.get(timeout)` pattern lets us bail out cleanly without
   stalling the system-prompt build."
  [^Repository repo ^long timeout-ms]
  (let [^Git git (Git/wrap repo)
        task     (reify java.util.concurrent.Callable
                   (call [_]
                     (.. git (status) (call))))
        executor (java.util.concurrent.Executors/newSingleThreadExecutor)]
    (try
      (let [^Future fut (.submit executor ^java.util.concurrent.Callable task)]
        (try
          (let [^Status status (.get fut timeout-ms TimeUnit/MILLISECONDS)]
            {:clean?      (.isClean status)
             :modified    (safe-count (.getModified status))
             :untracked   (safe-count (.getUntracked status))
             :added       (safe-count (.getAdded status))
             :changed     (safe-count (.getChanged status))
             :missing     (safe-count (.getMissing status))
             :removed     (safe-count (.getRemoved status))
             :conflicting (safe-count (.getConflicting status))})
          (catch TimeoutException _
            (.cancel fut true)
            nil)
          (catch ExecutionException _ nil)
          (catch InterruptedException _ nil)))
      (finally
        (.shutdownNow executor)))))

(defn snapshot
  "Inspect the repository that contains `start-file`. Returns a map
   of git facts, or nil when `start-file` is not in a repo.

   Options:
     :status?           — when true, run `git status` (slower).
                          Defaults true.
     :status-timeout-ms — wall-time cap on the status walk.
                          Defaults to `default-status-timeout-ms`."
  ([^File start-file] (snapshot start-file nil))
  ([^File start-file {:keys [status? status-timeout-ms]
                      :or   {status?            true
                             status-timeout-ms default-status-timeout-ms}}]
   (when-let [^Repository repo (open-repository start-file)]
     (try
       (let [git-dir       (.getDirectory repo)
             worktree-root (.getWorkTree repo)
             branch        (try (.getBranch repo) (catch Throwable _ nil))
             detached?     (detached-head? repo)
             short-sha     (short-head repo)
             status-map    (when status?
                             (collect-status repo (long status-timeout-ms)))
             base          {:root         (try (.getCanonicalPath worktree-root)
                                            (catch Throwable _ (.getAbsolutePath worktree-root)))
                            :git-dir      (try (.getCanonicalPath git-dir)
                                            (catch Throwable _ (.getAbsolutePath git-dir)))
                            :branch       (when-not detached? branch)
                            :detached?    detached?
                            :detached-sha (when detached? short-sha)
                            :worktree?    (worktree? worktree-root)
                            :submodules?  (submodules-present? worktree-root)}]
         (if status-map
           (merge base status-map)
           (assoc base :status-unavailable? true)))
       (finally
         (try (.close repo) (catch Throwable _ nil)))))))
