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
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.drafts :as drafts]
            [com.blockether.vis.internal.workspace.git :as git-core]))

(defn- draft-block
  "`\"draft\"` — the landing facts of the draft the session works in: its
   backend, its `vis/<label>` branch and default target branch, how many draft
   commits the target lacks and how many paths still differ from the draft branch.
   Git facts are omitted when they cannot be read; the block never fails the
   turn."
  [workspace]
  (let [{:keys [backend branch target-branch ahead pending]} (try (drafts/status workspace)
                                                                  (catch Throwable _ nil))]
    (cond-> {"label" (:label workspace)}
      backend
      (assoc "backend" backend)

      branch
      (assoc "branch" branch)

      target-branch
      (assoc "target_branch" target-branch)

      ahead
      (assoc "approved_ahead" ahead)

      pending
      (assoc "pending_paths" pending))))

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
  "Merge registrations and access facts into ONE list of filesystem roots, projecting names and
   cwd onto permitted working copies. A process read-only grant of `policy` is in the list with
   `\"mode\" \"read\"`, so is a root that a subagent write grant left readable only. Only
   facts that differ from the default (read/write, shared, searched) are written."
  [root repo-root filesystem-roots project-paths policy]
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
                    (cond-> (assoc mapping
                              :trunk path
                              :clone target
                              :python-name alias)
                      ;; A search setting belongs to its own root, not to a path below it.
                      (not= path (:trunk mapping))
                      (dissoc :no-search?)))))))
          (sort-by key project-paths))

        registered
        (into #{} (map :trunk) projects)

        catalog
        (remove #(or (:primary? %) (contains? own (:trunk %)) (contains? registered (:trunk %)))
          filesystem-roots)

        jail
        (:process-jail policy)

        listed
        (into (into own registered) (map :trunk) catalog)

        no-search
        (set (:no-search jail))

        read?
        (set (:allow-read jail))

        read-only
        (keep (fn [path]
                (when-not (contains? listed path)
                  {:trunk path
                   :draft (get (:draft-policies policy) path)
                   :no-search? (contains? no-search path)}))
              (distinct (:allow-read jail)))

        descriptions
        (:path-descriptions jail)]

    (mapv (fn [{:keys [trunk clone draft denied? no-search? python-name]}]
            (let [draft
                  (some-> draft
                          name)

                  description
                  (get descriptions trunk)]

              (cond-> {"cwd" (paths/abbreviate-home (if denied? trunk (or clone trunk)))}
                python-name
                (assoc "python_name" python-name)

                (read? trunk)
                (assoc "mode" "read")

                description
                (assoc "description" description)

                (and clone (not= clone trunk))
                (assoc "isolated" true)

                (and draft (not= "shared" draft))
                (assoc "draft" draft)

                denied?
                (assoc "is_denied" true)

                no-search?
                (assoc "no_search" true))))
          (concat projects catalog read-only))))

(defn render-block
  "Project a hydrated `{:workspace :session-state}` pair into the
   canonical `\"session_workspace\"` CTX map. STRING-KEYED — this block
   crosses the Clojure↔Python boundary as `session[\"workspace\"]`, so it
   carries no keyword keys/values at any depth (`\"vcs_kind\"` is the
   stringified `git/vcs-kind`).

   Paths under home read as `~/…`, like the access block; Python expands them.

   workspace identity — `\"root\"` `\"isolated\"` `\"id\"` `\"label\"`
     `\"filesystem_roots\"` — the ONE list of other paths: each working `\"cwd\"` with
       its `\"description\"`, `\"mode\" \"read\"` for a process read-only grant,
       `\"no_search\"`, a non-shared `\"draft\"` policy and `\"isolated\"` for a private
       copy. A registered project's `\"python_name\"` binds a prebound Python Path to
       that same cwd; caches and denied roots have no automatic name. The session's OWN
       root stays in `\"root\"`/`\"isolated\"` and binds `project_root_path`, never a
       duplicate filesystem-roots row. `session[\"access\"]` keeps only the rules.
   `\"changed\"` / `\"changed_paths\"` — since-fork edits
   session linkage — `\"session_state_id\"` `\"session_id\"` `\"session_title\"`
     `\"session_fork_of\"` (foreign namespaces stay folded)"
  [{:keys [workspace session-state filesystem-roots project-paths policy]}]
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
        (project-filesystem-roots root (:repo-root workspace) filesystem-roots project-paths policy)

        changed
        (when (and root fork-ms (.exists (io/file root)))
          (try (workspace/changed-paths root fork-ms) (catch Throwable _ nil)))]

    (cond-> {"root" (paths/abbreviate-home root)
             "isolated" isolated?
             "vcs_kind" (some-> (git-core/vcs-kind root)
                                name)}
      (:id workspace)
      (assoc "id" (:id workspace))

      (:label workspace)
      (assoc "label" (:label workspace))

      (and isolated? (workspace/draft? workspace))
      (assoc "draft" (draft-block workspace))

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
