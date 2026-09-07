(ns com.blockether.vis.internal.foundation.workspace-ctx
  "Pre-turn `\"session_workspace\"` CTX block (STRING-KEYED — crosses the
   Python boundary as `session[\"workspace\"]`).

   Sessions may work directly in trunk or inside an isolated backend
   workspace. That distinction is reported on `\"isolated\"` (the word
   `sandbox` names the Python sandbox, and confinement is `jail`), NOT
   as a VCS. `\"vcs_kind\"` reports the underlying repository VCS (`\"git\"`
   when the root is inside a git repo, else `\"none\"`) so it matches the
   `git/` extension surface, which activates on the same predicate. The
   model reads the workspace block to know the active root and what it has
   changed since the fork. The block is stamped once per turn at engine
   start; ctx_renderer serialises it verbatim."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.workspace.git :as git-core]
            [com.blockether.vis.internal.workspace.core :as workspace]))

(defn- canonical-path
  [dir]
  (some-> dir
          io/file
          .getCanonicalPath))

(def ^:private max-changed
  "Cap the changed-paths list in the prompt block so a large working
   set never blows the CTX budget."
  50)

(defn- project-filesystem-roots
  "Merge registrations into filesystem roots, projecting names and cwd onto permitted working copies."
  [root repo-root filesystem-roots project-paths]
  (let [own
        (set [root (canonical-path repo-root)])

        mappings
        (sort-by (comp count :trunk) > filesystem-roots)

        projects
        (keep
          (fn [[alias path]]
            (let [source
                  (.toPath (io/file path))

                  mapping
                  (some #(when (.startsWith source (.toPath (io/file (:trunk %)))) %) mappings)]

              (when-not (or (contains? own path) (:denied? mapping))
                (let [target (if mapping
                               (str (.resolve
                                      (.toPath (io/file (or (:clone mapping) (:trunk mapping))))
                                      (.relativize (.toPath (io/file (:trunk mapping))) source)))
                               path)]
                  (when-not (= root target)
                    (assoc mapping
                      :trunk path
                      :clone target
                      :python-name alias))))))
          (sort-by key project-paths))

        registered
        (into #{} (map :trunk) projects)]

    (mapv (fn [{:keys [trunk clone draft denied? python-name]}]
            (cond-> {"cwd" (if denied? trunk (or clone trunk))
                     "isolated" (boolean (and clone (not= clone trunk)))
                     "draft" (name (or draft :shared))}
              denied?
              (assoc "is_denied" true)

              python-name
              (assoc "python_name" python-name)))
          (concat
            projects
            (remove #(or (:primary? %) (contains? own (:trunk %)) (contains? registered (:trunk %)))
              filesystem-roots)))))

(defn render-block
  "Project a hydrated `{:workspace :session-state}` pair into the
   canonical `\"session_workspace\"` CTX map. STRING-KEYED — this block
   crosses the Clojure↔Python boundary as `session[\"workspace\"]`, so it
   carries no keyword keys/values at any depth (`\"vcs_kind\"` is the
   stringified `git/vcs-kind`).

   workspace identity — `\"root\"` `\"isolated\"` `\"id\"` `\"label\"`
     `\"filesystem_roots\"` — additional roots with their working `\"cwd\"`, `\"draft\"`
       isolation policy and `\"isolated\"` flag. A registered project's `\"python_name\"`
       binds a prebound Python Path to that same cwd; caches and denied roots have
       no automatic name. The session's OWN root stays in `\"root\"`/`\"isolated\"`
       and binds `project_root_path`, never a duplicate filesystem-roots row.
   `\"changed\"` / `\"changed_paths\"` — since-fork edits
   session linkage — `\"session_state_id\"` `\"session_id\"` `\"session_title\"`
     `\"session_fork_of\"` (foreign namespaces stay folded)"
  [{:keys [workspace session-state filesystem-roots project-paths]}]
  (let [root
        (canonical-path (or (:root workspace) (workspace/cwd)))

        fork-ms
        (:fork-ms workspace)

        ;; Migration window for rows created before V4__workspace_backend.sql:
        ;; a fork timestamp without a backend still means "isolated copy".
        ;; New rows must persist :workspace-backend explicitly.
        isolated?
        (not= :live (or (:workspace-backend workspace) (when fork-ms :legacy-isolated) :live))

        roots
        (project-filesystem-roots root (:repo-root workspace) filesystem-roots project-paths)

        changed
        (when (and root fork-ms (.exists (io/file root)))
          (try (workspace/changed-paths root fork-ms) (catch Throwable _ nil)))]

    (cond-> {"root" root
             "isolated" isolated?
             "vcs_kind" (some-> (git-core/vcs-kind root)
                                name)}
      (:id workspace)
      (assoc "id" (:id workspace))

      (:label workspace)
      (assoc "label" (:label workspace))

      (seq roots)
      (assoc "filesystem_roots" roots)

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
