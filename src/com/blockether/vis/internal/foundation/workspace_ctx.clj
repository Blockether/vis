(ns com.blockether.vis.internal.foundation.workspace-ctx
  "Pre-turn `\"session_workspace\"` CTX block (STRING-KEYED — crosses the
   Python boundary as `session[\"workspace\"]`).

   `\"vcs_kind\"` reports the underlying repository VCS (`\"git\"`
   when the root is inside a git repo, else `\"none\"`) so it matches the
   `git/` extension surface, which activates on the same predicate. The
   model reads the workspace block to know the active root. The block is
   stamped once per turn at engine start; ctx_renderer serialises it verbatim."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.git :as git-core]))

(defn- canonical-path
  [dir]
  (some-> dir
          io/file
          .getCanonicalPath))

(defn- project-filesystem-roots
  "Merge registrations and access facts into ONE list of filesystem roots. A process
   read-only grant of `policy` is in the list with `\"mode\" \"read\"`, so is a root that a
   subagent write grant left readable only. Only facts that differ from the default
   (read/write, searched) are written."
  [root filesystem-roots project-paths policy]
  (let [mappings
        (sort-by (comp count :root) > filesystem-roots)

        projects
        (keep (fn [[alias path]]
                (when-not (= root path)
                  (let [source
                        (.toPath (io/file path))

                        mapping
                        (some #(when (.startsWith source (.toPath (io/file (:root %)))) %)
                              mappings)]

                    ;; A search setting belongs to its own root, not to a path below it.
                    (cond-> {:root path :python-name alias}
                      (and (:no-search? mapping) (= path (:root mapping)))
                      (assoc :no-search? true)))))
              (sort-by key project-paths))

        registered
        (into #{} (map :root) projects)

        ;; A disabled jail grants every host root (`/`). `\"is_jailed\": false` already
        ;; says so, and default searches skip them, so the list does not repeat them.
        host-root?
        (fn [entry]
          (and (false? (:jail-enabled policy)) (nil? (.getParent (io/file (:root entry))))))

        catalog
        (remove #(or (host-root? %) (= root (:root %)) (contains? registered (:root %)))
          filesystem-roots)

        jail
        (:process-jail policy)

        listed
        (into (conj registered root) (map :root) catalog)

        no-search
        (set (:no-search jail))

        read?
        (set (:allow-read jail))

        read-only
        (keep (fn [path]
                (when-not (contains? listed path)
                  {:root path :no-search? (contains? no-search path)}))
              (distinct (:allow-read jail)))

        descriptions
        (:path-descriptions jail)]

    (mapv (fn [{:keys [root no-search? python-name]}]
            (let [description (get descriptions root)]
              (cond-> {"cwd" (paths/abbreviate-home root)}
                python-name
                (assoc "python_name" python-name)

                (read? root)
                (assoc "mode" "read")

                description
                (assoc "description" description)

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

   workspace identity — `\"root\"` `\"id\"` `\"label\"`
     `\"filesystem_roots\"` — the ONE list of other paths: each working `\"cwd\"` with
       its `\"description\"`, `\"mode\" \"read\"` for a process read-only grant and
       `\"no_search\"`. A registered project's `\"python_name\"` binds a prebound Python
       Path to that same cwd; caches have no automatic name. The session's OWN
       root stays in `\"root\"` and binds `project_root_path`, never a
       duplicate filesystem-roots row. `session[\"access\"]` keeps only the rules.
   session linkage — `\"session_state_id\"` `\"session_id\"` (the title is for people only)
     `\"session_fork_of\"` (foreign namespaces stay folded)"
  [{:keys [workspace session-state filesystem-roots project-paths policy]}]
  (let [root
        (canonical-path (or (:root workspace) (workspace/cwd)))

        roots
        (project-filesystem-roots root filesystem-roots project-paths policy)]

    (cond-> {"root" (paths/abbreviate-home root)
             "vcs_kind" (some-> (git-core/vcs-kind root)
                                name)}
      (:id workspace)
      (assoc "id" (:id workspace))

      (:label workspace)
      (assoc "label" (:label workspace))

      (seq roots)
      (assoc "filesystem_roots" roots)

      session-state
      (merge {"session_state_id" (:id session-state) "session_id" (:session-soul-id session-state)}
             (when-let [pid (:parent-state-id session-state)]
               {"session_fork_of" {"soul" (:session-soul-id session-state) "parent_state" pid}})))))
