(ns com.blockether.vis.ext.foundation-core.workspace-ctx
  "Pre-turn `:session/workspace` CTX block (PLAN.md §8 — canonical
   `:vcs/*` shape; legacy `:git/*` aliases were KILLED in step 4).

   The model reads `:session/workspace` to know what workspace is
   focused, what trunk it FFs onto, and which commits are ahead. The
   block is stamped once per turn at engine start; ctx_renderer
   serialises it into the prompt verbatim. Stale renders must not
   recompute on every iter — see PLAN §8 'stamping rules'.

   `render-block` is a pure projection from a hydrated
   `{:workspace … :session-state …}` pair plus optional VCS facts
   gathered via a local `git!` helper. Empty / non-VCS sessions
   render `{:vcs/kind :none}` (matches engine `empty-ctx`)."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Git helpers — all read-only, no mutation
;; =============================================================================

(defn- git-out
  "Run `git -C dir args` and return the trimmed stdout, or nil on failure.
   Read-only by contract; never throws."
  [dir args]
  (try
    (let [{:keys [exit out]}
          (apply sh/sh (into ["git" "-C" (.getCanonicalPath (io/file dir))] args))]
      (when (zero? exit)
        (str/trim (or out ""))))
    (catch Throwable _ nil)))

(defn- head-sha [dir]
  (git-out dir ["rev-parse" "HEAD"]))

(defn- dirty?
  [dir]
  (boolean (some-> (git-out dir ["status" "--porcelain"]) seq)))

(defn- ff-possible?
  "True when `trunk` is an ancestor of `branch` in `repo-root`. Used
   to surface a hint when `/workspace apply` is safe."
  [repo-root branch trunk]
  (boolean
    (when (and trunk branch (not= trunk branch))
      (let [{:keys [exit]} (sh/sh "git" "-C" (.getCanonicalPath (io/file repo-root))
                             "merge-base" "--is-ancestor" trunk branch)]
        (zero? exit)))))

(defn- commits-ahead
  "Return a vec of `{:sha :message}` commits in `branch` not in `trunk`.
   Empty when `branch` = `trunk` or git fails. Hard-capped at 32 entries
   to keep CTX bounded."
  [repo-root branch trunk]
  (if (or (str/blank? branch) (str/blank? trunk) (= branch trunk))
    []
    (let [raw (git-out repo-root
                ["log" "--format=%H%x09%s" "-n" "32"
                 (str trunk ".." branch)])]
      (if (str/blank? raw)
        []
        (mapv (fn [line]
                (let [[sha msg] (str/split line #"\t" 2)]
                  {:sha sha :message (or msg "")}))
          (str/split-lines raw))))))

(defn- numstat-stats
  "Parse `git diff --numstat trunk..branch` into a {path {:added :removed}}
   map. Returns {} on failure."
  [repo-root branch trunk]
  (if (or (str/blank? branch) (str/blank? trunk) (= branch trunk))
    {}
    (let [raw (git-out repo-root ["diff" "--numstat" (str trunk ".." branch)])]
      (if (str/blank? raw)
        {}
        (reduce
          (fn [acc line]
            (let [[added removed path] (str/split line #"\t" 3)]
              (if (and path
                    (re-matches #"\d+" (or added ""))
                    (re-matches #"\d+" (or removed "")))
                (assoc acc path {:added   (Long/parseLong added)
                                 :removed (Long/parseLong removed)})
                acc)))
          {}
          (str/split-lines raw))))))

;; =============================================================================
;; Public render
;; =============================================================================

(defn render-block
  "Project a hydrated `{:workspace :session-state}` pair into the
   canonical `:session/workspace` CTX map (PLAN.md §8).

   Returns `{:vcs/kind :none}` when `workspace` is nil — matches the
   engine's `empty-ctx`.

   :workspace/*  — Vis identity (id, kind, label)
   :session/*    — soul/state linkage + title + fork lineage
   :vcs/*        — git-side facts (branch, trunk, head, dirty?,
                   stats, commits-ahead, ff-possible?). The
                   discriminator is `:vcs/kind` (`:git` for now,
                   `:hg` / `:jj` / `:fossil` plug in later; PLAN §8)."
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
