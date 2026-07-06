(ns com.blockether.vis.internal.foundation.environment.git
  "Git introspection for the environment block, backed by the native `git`
   binary (via `internal.git`).

   Returns a snapshot map for the repository that contains `start`
   (typically the JVM working directory). nil when `start` is not inside any
   git repository. Never throws — every git call is guarded; on any failure we
   degrade gracefully to nil or a reduced-shape map.

   The expensive call is `git status` (working-tree walk). When it fails or is
   suppressed, the snapshot drops the dirty-status fields instead of stalling
   the system-prompt build."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.git :as vis-git])
  (:import (java.io File)))

(def ^:const default-status-timeout-ms
  "Retained for call-site compatibility. Native `git status` is fast, so this
   is no longer a hard walk deadline — `run-git` applies its own subprocess
   timeout."
  2000)

(defn- git-line
  "Trimmed stdout of `git <args>` in `dir` when it exits 0, else nil."
  [^File dir args]
  (let [{:keys [exit out]} (vis-git/run-git dir args)]
    (when (= 0 exit)
      (let [s (str/trim (or out ""))]
        (when (seq s) s)))))

(defn- worktree?
  "A linked worktree has a `.git` FILE (with `gitdir:` pointer) instead of a
   directory at the working-tree root."
  [^File worktree-root]
  (try (let [dot-git (io/file worktree-root ".git")]
         (and (.exists dot-git) (.isFile dot-git)))
       (catch Throwable _ false)))

(defn- submodules-present?
  [^File worktree-root]
  (try (.exists (io/file worktree-root ".gitmodules")) (catch Throwable _ false)))

(defn- stash-count
  [^File dir]
  (try (if-let [out (git-line dir ["stash" "list"])]
         (count (str/split-lines out))
         0)
       (catch Throwable _ 0)))

(defn- ahead-behind
  "Ahead/behind vs the branch's upstream, or nil when there is none.
   `git rev-list --left-right --count @{u}...HEAD` prints `<behind> <ahead>`."
  [^File dir]
  (try (when-let [out (git-line dir ["rev-list" "--left-right" "--count" "@{upstream}...HEAD"])]
         (let [[behind ahead] (map parse-long (str/split (str/trim out) #"\s+"))]
           (when (and behind ahead) {:ahead ahead :behind behind :stale? (pos? (long behind))})))
       (catch Throwable _ nil)))

(defn snapshot
  "Inspect the repository that contains `start-file`. Returns a map of git
   facts, or nil when `start-file` is not in a repo.

   Options:
     :status? - when true, run `git status` for dirty counters. Defaults true."
  ([^File start-file] (snapshot start-file nil))
  ([^File start-file {:keys [status?] :or {status? true}}]
   (when-let [^File worktree-root (vis-git/repo-work-tree start-file)]
     (try (let [git-dir (git-line worktree-root ["rev-parse" "--absolute-git-dir"])
                branch-ref (git-line worktree-root ["rev-parse" "--abbrev-ref" "HEAD"])
                detached? (or (nil? branch-ref) (= "HEAD" branch-ref))
                short-sha (git-line worktree-root ["rev-parse" "--short=12" "HEAD"])
                porcelain (when status? (vis-git/porcelain-tokens worktree-root nil))
                status-map (when porcelain (vis-git/status-counts porcelain))
                upstream (when-not detached?
                           (git-line worktree-root
                                     ["rev-parse" "--abbrev-ref" "--symbolic-full-name"
                                      "@{upstream}"]))
                sync-map (when-not detached? (ahead-behind worktree-root))
                base (cond-> {:root (.getPath worktree-root)
                              :git-dir (or git-dir (.getPath (io/file worktree-root ".git")))
                              :branch (when-not detached? branch-ref)
                              :detached? detached?
                              :detached-sha (when detached? short-sha)
                              :worktree? (worktree? worktree-root)
                              :submodules? (submodules-present? worktree-root)
                              :stash-count (stash-count worktree-root)}
                       upstream
                       (assoc :upstream upstream)

                       sync-map
                       (merge sync-map))]

            (if status-map (merge base status-map) (assoc base :status-unavailable? true)))
          (catch Throwable _ nil)))))
