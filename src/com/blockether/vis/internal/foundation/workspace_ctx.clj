(ns com.blockether.vis.internal.foundation.workspace-ctx
  "Pre-turn `:session/workspace` CTX block.

   Sessions may work directly in trunk or inside an isolated backend
   workspace. That distinction is reported on `:workspace/sandbox?`, NOT
   as a VCS. `:vcs/kind` reports the underlying repository VCS (`:git`
   when the root is inside a git repo, else `:none`) so it matches the
   ctx-spec set and the `git/` extension surface, which activates on the
   same predicate. The model reads `:session/workspace` to know the active
   root and what it has changed since the fork. The block is stamped once
   per turn at engine start; ctx_renderer serialises it verbatim."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.internal.git :as git-core]
   [com.blockether.vis.internal.workspace :as workspace]))

(defn- canonical-path
  [dir]
  (some-> dir io/file .getCanonicalPath))

(def ^:private max-changed
  "Cap the changed-paths list in the prompt block so a large working
   set never blows the CTX budget."
  50)

(defn render-block
  "Project a hydrated `{:workspace :session-state}` pair into the
   canonical `:session/workspace` CTX map.

   :workspace/*  — identity (id, root, sandbox?, label, parent-id)
   :workspace/changed / :workspace/changed-paths — since-fork edits
   :session/*    — soul/state linkage + title + fork lineage"
  [{:keys [workspace session-state]}]
  (let [root    (canonical-path (or (:root workspace) (workspace/cwd)))
        fork-ms (:fork-ms workspace)
        ;; Migration window for rows created before V4__workspace_backend.sql:
        ;; a fork timestamp without a backend still means "isolated copy".
        ;; New rows must persist :workspace-backend explicitly.
        isolated? (not= :live
                    (or (:workspace-backend workspace)
                      (when fork-ms :legacy-isolated)
                      :live))
        changed (when (and root fork-ms (.exists (io/file root)))
                  (try (workspace/changed-paths root fork-ms)
                    (catch Throwable _ nil)))]
    (cond-> {:workspace/root     root
             :workspace/sandbox? isolated?
             :vcs/kind           (git-core/vcs-kind root)}
      (:id workspace)        (assoc :workspace/id (:id workspace))
      (:label workspace)     (assoc :workspace/label (:label workspace))
      (seq (:filesystem-roots workspace))
      (assoc :workspace/filesystem-roots
        ;; Extra dirs the session may also operate on, addressed by their REAL
        ;; path (`:dir`). Edits there are transparently isolated in a draft
        ;; copy when `:isolated?` — they land on /draft apply. Surfaced so the
        ;; model KNOWS it can read/write under these dirs.
        (mapv (fn [{:keys [trunk clone]}]
                (cond-> {:dir trunk}
                  (not= clone trunk) (assoc :isolated? true)))
          (workspace/filesystem-roots workspace)))
      changed                (assoc :workspace/changed (count changed)
                               :workspace/changed-paths (vec (take max-changed (sort changed))))
      session-state
      (merge
        {:session/state-id (:id session-state)
         :session/id       (:session-soul-id session-state)
         :session/title    (or (:title session-state) "Untitled")}
        (when-let [pid (:parent-state-id session-state)]
          {:session/fork-of {:soul         (:session-soul-id session-state)
                             :parent-state pid}})))))
