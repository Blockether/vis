(ns com.blockether.vis.internal.foundation.workspace-ctx
  "Pre-turn `\"session_workspace\"` CTX block (STRING-KEYED — crosses the
   Python boundary as `session[\"workspace\"]`).

   Sessions may work directly in trunk or inside an isolated backend
   workspace. That distinction is reported on `\"sandbox\"`, NOT
   as a VCS. `\"vcs_kind\"` reports the underlying repository VCS (`\"git\"`
   when the root is inside a git repo, else `\"none\"`) so it matches the
   `git/` extension surface, which activates on the same predicate. The
   model reads the workspace block to know the active root and what it has
   changed since the fork. The block is stamped once per turn at engine
   start; ctx_renderer serialises it verbatim."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.git :as git-core]
            [com.blockether.vis.internal.workspace :as workspace]))

(defn- canonical-path
  [dir]
  (some-> dir
          io/file
          .getCanonicalPath))

(def ^:private max-changed
  "Cap the changed-paths list in the prompt block so a large working
   set never blows the CTX budget."
  50)

(defn render-block
  "Project a hydrated `{:workspace :session-state}` pair into the
   canonical `\"session_workspace\"` CTX map. STRING-KEYED — this block
   crosses the Clojure↔Python boundary as `session[\"workspace\"]`, so it
   carries no keyword keys/values at any depth (`\"vcs_kind\"` is the
   stringified `git/vcs-kind`).

   workspace identity — `\"root\"` `\"sandbox\"` `\"id\"` `\"label\"`
     `\"filesystem_roots\"` (the `workspace/` namespace folds away — the
     block IS the workspace)
   `\"changed\"` / `\"changed_paths\"` — since-fork edits
   session linkage — `\"session_state_id\"` `\"session_id\"` `\"session_title\"`
     `\"session_fork_of\"` (foreign namespaces stay folded)"
  [{:keys [workspace session-state]}]
  (let [root
        (canonical-path (or (:root workspace) (workspace/cwd)))

        fork-ms
        (:fork-ms workspace)

        ;; Migration window for rows created before V4__workspace_backend.sql:
        ;; a fork timestamp without a backend still means "isolated copy".
        ;; New rows must persist :workspace-backend explicitly.
        isolated?
        (not= :live (or (:workspace-backend workspace) (when fork-ms :legacy-isolated) :live))

        changed
        (when (and root fork-ms (.exists (io/file root)))
          (try (workspace/changed-paths root fork-ms) (catch Throwable _ nil)))]

    (cond-> {"root" root
             "sandbox" isolated?
             "vcs_kind" (some-> (git-core/vcs-kind root)
                                name)}
      (:id workspace)
      (assoc "id" (:id workspace))

      (:label workspace)
      (assoc "label" (:label workspace))

      (seq (:filesystem-roots workspace))
      (assoc "filesystem_roots"
        ;; Extra dirs the session may also operate on, addressed by their REAL
        ;; path (`"dir"`). Edits there are transparently isolated in a draft
        ;; copy when `"isolated"` — they land on /draft apply. Surfaced so the
        ;; model KNOWS it can read/write under these dirs.
        (mapv (fn [{:keys [trunk clone]}]
                (cond-> {"dir" trunk}
                  (not= clone trunk)
                  (assoc "isolated" true)))
              (workspace/filesystem-roots workspace)))

      changed
      (assoc "changed"
        (count changed) "changed_paths"
        (vec (take max-changed (sort changed))))

      session-state
      (merge {"session_state_id" (:id session-state)
              "session_id" (:session-soul-id session-state)
              "session_title" (or (:title session-state) "Untitled")}
             (when-let [pid (:parent-state-id session-state)]
               {"session_fork_of" {"soul" (:session-soul-id session-state) "parent_state" pid}})))))
