(ns com.blockether.vis.ext.foundation-core.workspace-ctx
  "Pre-turn `:session/workspace` CTX block. Canonical workspace + VCS
   shape. Workspace identity and VCS capability are separate axes.

   The model reads `:session/workspace` to know the active root,
   sandbox flag, current VCS ref, mainline, and unmerged commits. The
   block is stamped once per turn at engine start; ctx_renderer
   serialises it into the prompt verbatim. Stale renders must not
   recompute on every iter.

   `render-block` projects a hydrated `{:workspace ... :session-state ...}`
   pair plus optional VCS facts gathered via JGit. Non-VCS sessions render
   a rooted workspace map: `{:workspace/root ... :workspace/sandbox? false
   :vcs/kind :none}`."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   [java.io ByteArrayOutputStream]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.diff DiffEntry DiffFormatter RawTextComparator]
   [org.eclipse.jgit.lib ObjectId Repository]
   [org.eclipse.jgit.revwalk RevWalk]
   [org.eclipse.jgit.storage.file FileRepositoryBuilder]
   [org.eclipse.jgit.treewalk CanonicalTreeParser]))

;; =============================================================================
;; JGit helpers - all read-only, no mutation
;; =============================================================================

(defn- open-git
  ^Git [dir] (Git/open (io/file dir)))

(defn- canonical-path
  [dir]
  (some-> dir io/file .getCanonicalPath))

(defn- discover-git-root
  "Return enclosing git worktree root for `dir`, or nil when unsupported."
  [dir]
  (try
    (let [builder (doto (FileRepositoryBuilder.)
                    (.findGitDir (io/file dir))
                    .readEnvironment)
          git-dir (.getGitDir builder)]
      (when git-dir
        (with-open [repo (.build builder)]
          (some-> repo .getWorkTree canonical-path))))
    (catch Throwable _ nil)))

(defn- current-ref
  "Return current VCS ref name for git worktree `dir`, or nil."
  [dir]
  (try
    (with-open [git (open-git dir)]
      (.getBranch (.getRepository git)))
    (catch Throwable _ nil)))

(defn- non-vcs-block
  [root]
  (cond-> {:workspace/sandbox? false
           :vcs/kind :none}
    root (assoc :workspace/root root)))

(defn- head-sha
  "Resolve HEAD in the worktree at `dir`. nil when not a repo or HEAD
   doesn't resolve."
  [dir]
  (try
    (with-open [git (open-git dir)]
      (some-> (.resolve (.getRepository git) "HEAD") .getName))
    (catch Throwable _ nil)))

(defn- dirty?
  "True when the JGit `Status` for `dir` has any pending changes."
  [dir]
  (try
    (with-open [git (open-git dir)]
      (not (.isClean (.call (.status git)))))
    (catch Throwable _ false)))

(defn- resolve-ref
  "Resolve `ref-name` (e.g. \"main\", \"vis/abc12345\") to an ObjectId
   in `repo`. nil on miss."
  ^ObjectId [^Repository repo ^String ref-name]
  (try
    (or (.resolve repo (str "refs/heads/" ref-name))
      (.resolve repo ref-name))
    (catch Throwable _ nil)))

(defn- ff-possible?
  "True when `trunk` is an ancestor of `branch` in the repo at
   `repo-root`. JGit `RevWalk.isMergedInto` replaces the previous
   `git merge-base --is-ancestor` shell-out."
  [repo-root branch trunk]
  (boolean
    (when (and trunk branch (not= trunk branch))
      (try
        (with-open [git (open-git repo-root)]
          (let [repo  (.getRepository git)
                tid   (resolve-ref repo trunk)
                bid   (resolve-ref repo branch)]
            (when (and tid bid)
              (with-open [walk (RevWalk. repo)]
                (let [tc (.parseCommit walk tid)
                      bc (.parseCommit walk bid)]
                  (.isMergedInto walk tc bc))))))
        (catch Throwable _ false)))))

(defn- commits-ahead
  "Return a vec of `{:sha :message}` commits in `branch` not in
   `trunk`, oldest-first. Hard-capped at 32 entries to keep CTX bounded.
   JGit `LogCommand` with `addRange(trunk, branch)`."
  [repo-root branch trunk]
  (if (or (str/blank? branch) (str/blank? trunk) (= branch trunk))
    []
    (try
      (with-open [git (open-git repo-root)]
        (let [repo (.getRepository git)
              tid  (resolve-ref repo trunk)
              bid  (resolve-ref repo branch)]
          (if (and tid bid)
            (let [iter (-> (.log git)
                         (.addRange tid bid)
                         (.setMaxCount 32)
                         .call)]
              (vec (for [^org.eclipse.jgit.revwalk.RevCommit c iter]
                     {:sha (.getName c)
                      :message (str/trim (or (.getShortMessage c) ""))})))
            [])))
      (catch Throwable _ []))))

(defn- tree-parser
  "Build a CanonicalTreeParser positioned at the tree of `commit-id`.
   Used by the numstat diff helper."
  ^CanonicalTreeParser [^Repository repo ^ObjectId commit-id]
  (let [parser (CanonicalTreeParser.)]
    (with-open [reader (.newObjectReader repo)
                walk   (RevWalk. repo)]
      (let [commit (.parseCommit walk commit-id)]
        (.reset parser reader (.getId (.getTree commit)))))
    parser))

(defn- numstat-stats
  "JGit diff numstat for `trunk..branch`. Returns
   `{path {:added :removed}}` matching the engine spec. JGit
   `DiffFormatter.scan` gives the entry list; per-entry edits provide
   per-file added/removed counts."
  [repo-root branch trunk]
  (if (or (str/blank? branch) (str/blank? trunk) (= branch trunk))
    {}
    (try
      (with-open [git (open-git repo-root)]
        (let [repo (.getRepository git)
              tid  (resolve-ref repo trunk)
              bid  (resolve-ref repo branch)]
          (if-not (and tid bid)
            {}
            (with-open [out (ByteArrayOutputStream.)
                        formatter (doto (DiffFormatter. out)
                                    (.setRepository repo)
                                    (.setDiffComparator RawTextComparator/DEFAULT)
                                    (.setDetectRenames true))]
              (let [entries (.scan formatter
                              (tree-parser repo tid)
                              (tree-parser repo bid))]
                (reduce
                  (fn [acc ^DiffEntry entry]
                    (let [path  (or (.getNewPath entry) (.getOldPath entry))
                          edits (.toEditList (.toFileHeader formatter entry))
                          {:keys [added removed]}
                          (reduce
                            (fn [{:keys [added removed]} ^org.eclipse.jgit.diff.Edit e]
                              {:added   (+ added   (- (.getEndB e) (.getBeginB e)))
                               :removed (+ removed (- (.getEndA e) (.getBeginA e)))})
                            {:added 0 :removed 0}
                            edits)]
                      (if (and path (or (pos? added) (pos? removed)))
                        (assoc acc path {:added added :removed removed})
                        acc)))
                  {}
                  entries))))))
      (catch Throwable _ {}))))

;; =============================================================================
;; Public render
;; =============================================================================

(defn render-block
  "Project a hydrated `{:workspace :session-state}` pair into the
   canonical `:session/workspace` CTX map.

   When `workspace` is nil, falls back to the current cwd as workspace root
   and still emits rooted workspace identity. Bare `{:vcs/kind :none}` is
   never emitted.

   :workspace/*  -- Vis identity (id, root, sandbox?, label)
   :session/*    -- soul/state linkage + title + fork lineage
   :vcs/*        -- VCS facts (ref, mainline, head, dirty?, stats,
                    unmerged-commits, integrable?). The discriminator
                    is `:vcs/kind` (`:none`, `:git`, `:hg`, `:jj`,
                    `:fossil`)."
  [{:keys [workspace session-state]}]
  (let [root     (canonical-path (or (:root workspace) (workspace/cwd)))
        repo-root (or (:repo-root workspace) (discover-git-root root))]
    (if (nil? repo-root)
      (non-vcs-block root)
      (let [ref       (or (:branch workspace) (current-ref root))
            mainline  (workspace/detect-trunk-branch repo-root)
            sandbox?  (= :branch (:kind workspace))
            head      (when root (head-sha root))
            dirty     (when root (dirty? root))
            stats     (numstat-stats repo-root ref mainline)
            unmerged  (commits-ahead repo-root ref mainline)
            integrable? (cond
                          (not sandbox?)       false
                          (and ref mainline)   (ff-possible? repo-root ref mainline)
                          :else                :unknown)]
        (cond-> {:workspace/id       (:id workspace)
                 :workspace/root     root
                 :workspace/sandbox? sandbox?
                 :vcs/kind           :git
                 :vcs/ref            ref
                 :vcs/mainline       mainline
                 :vcs/head           head
                 :vcs/dirty?         dirty
                 :vcs/stats          stats
                 :vcs/unmerged-commits unmerged
                 :vcs/integrable?    integrable?}
          (:parent-id workspace)
          (assoc :workspace/parent-id (:parent-id workspace))

          (:label workspace)
          (assoc :workspace/label (:label workspace))

          session-state
          (merge
            {:session/state-id (:id session-state)
             :session/id       (:session-soul-id session-state)
             :session/title    (or (:title session-state) "Untitled")}
            (when-let [pid (:parent-state-id session-state)]
              {:session/fork-of {:soul         (:session-soul-id session-state)
                                 :parent-state pid}})))))))
