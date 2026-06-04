(ns com.blockether.vis.internal.foundation.workspace-ctx
  "Pre-turn `:session/workspace` CTX block — git-free rift-clone shape.

   Every session works inside a `rift` CoW clone of the user's cwd
   (trunk). The model reads `:session/workspace` to know the active root
   and what it has changed since the fork. There are no branches /
   commits / merges to report: `apply` lands the clone's file edits back
   into cwd. The block is stamped once per turn at engine start;
   ctx_renderer serialises it into the prompt verbatim."
  (:require
   [clojure.java.io :as io]
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
   canonical `:session/workspace` CTX map (git-free rift model).

   :workspace/*  — identity (id, root, sandbox?=true, label, parent-id)
   :workspace/changed / :workspace/changed-paths — since-fork edits
   :session/*    — soul/state linkage + title + fork lineage"
  [{:keys [workspace session-state]}]
  (let [root    (canonical-path (or (:root workspace) (workspace/cwd)))
        fork-ms (:fork-ms workspace)
        changed (when (and root fork-ms (.exists (io/file root)))
                  (try (workspace/changed-paths root fork-ms)
                    (catch Throwable _ nil)))]
    (cond-> {:workspace/root     root
             :workspace/sandbox? true
             :vcs/kind           :rift}
      (:id workspace)        (assoc :workspace/id (:id workspace))
      (:label workspace)     (assoc :workspace/label (:label workspace))
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
