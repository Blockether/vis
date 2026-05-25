(ns com.blockether.vis.ext.foundation-core.workspace-ctx
  "Pre-turn `:session/workspace` CTX block. Canonical `:vcs/*` shape;
   the `:git/*` aliases were retired.

   The model reads `:session/workspace` to know what workspace is
   focused, what trunk it FFs onto, and which commits are ahead. The
   block is stamped once per turn at engine start; ctx_renderer
   serialises it into the prompt verbatim. Stale renders must not
   recompute on every iter.

   `render-block` is a pure projection from a hydrated
   `{:workspace ... :session-state ...}` pair plus optional VCS facts
   gathered via JGit. Empty / non-VCS sessions render `{:vcs/kind :none}`
   (matches engine `empty-ctx`)."
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
   [org.eclipse.jgit.treewalk CanonicalTreeParser]))

;; =============================================================================
;; JGit helpers - all read-only, no mutation
;; =============================================================================

(defn- open-git
  ^Git [dir] (Git/open (io/file dir)))

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

   Returns `{:vcs/kind :none}` when `workspace` is nil -- matches the
   engine's `empty-ctx`.

   :workspace/*  -- Vis identity (id, kind, label)
   :session/*    -- soul/state linkage + title + fork lineage
   :vcs/*        -- git-side facts (branch, trunk, head, dirty?,
                    stats, commits-ahead, ff-possible?). The
                    discriminator is `:vcs/kind` (`:git` for now,
                    `:hg` / `:jj` / `:fossil` plug in later; PLAN
                    section 8)."
  [{:keys [workspace session-state]}]
  (if (nil? workspace)
    {:vcs/kind :none}
    (let [repo-root (:repo-root workspace)
          root      (:root workspace)
          branch    (:branch workspace)
          trunk     (when repo-root (workspace/detect-trunk-branch repo-root))
          head      (when root (head-sha root))
          dirty     (when root (dirty? root))
          stats     (numstat-stats repo-root branch trunk)
          ahead     (commits-ahead repo-root branch trunk)
          ff?       (cond
                      (= :trunk (:kind workspace))            false
                      (and branch trunk)                       (ff-possible? repo-root branch trunk)
                      :else                                    :unknown)]
      (cond-> {:workspace/id    (:id workspace)
               :workspace/kind  (:kind workspace)
               :vcs/kind        :git
               :vcs/branch      branch
               :vcs/trunk       trunk
               :vcs/head        head
               :vcs/dirty?      dirty
               :vcs/stats       stats
               :vcs/commits-ahead ahead
               :vcs/ff-possible? ff?}
        (:label workspace)
        (assoc :workspace/label (:label workspace))

        session-state
        (merge
          {:session/state-id (:id session-state)
           :session/id       (:session-soul-id session-state)
           :session/title    (or (:title session-state) "Untitled")}
          (when-let [pid (:parent-state-id session-state)]
            {:session/fork-of {:soul         (:session-soul-id session-state)
                               :parent-state pid}}))))))
