(ns com.blockether.vis.internal.workspace.cli
  "`vis-agent projects` commands: list projects, or delete one, optionally with
   every session in it."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.workspace.core :as workspace]))

(defn- match-projects
  "Projects from `projects` selected by `input`: an exact id wins outright,
   otherwise every id with that prefix (case-insensitive).

   Pure on purpose — this is the resolution RULE, so an ambiguous prefix is
   provable without a database."
  [projects input]
  (let [needle
        (str/lower-case (str/trim (str input)))

        pid
        #(str/lower-case (str (:id %)))]

    (if (str/blank? needle)
      []
      (let [exact (filterv #(= needle (pid %)) projects)]
        (if (seq exact) exact (filterv #(str/starts-with? (pid %) needle) projects))))))

(defn- project-or-exit!
  "Resolve a project by full id or unambiguous prefix, or print why not and exit."
  [input]
  (let [matches (match-projects (lp/projects {:archived :include}) input)]
    (cond (= 1 (count matches)) (first matches)
          (empty? matches) (do (commandline/stdout! (str "Project not found: " input))
                               (commandline/stdout! "")
                               (commandline/stdout! "List existing projects with:")
                               (commandline/stdout! "  vis-agent projects")
                               (shutdown-agents)
                               (System/exit 1))
          :else (do (commandline/stdout!
                      (str "Ambiguous project id: " input " (" (count matches) " matches)"))
                    (commandline/stdout! "")
                    (doseq [p matches]
                      (commandline/stdout! (str "  " (:id p)
                                                "  " (or (not-empty (str (:name p))) "-"))))
                    (shutdown-agents)
                    (System/exit 1)))))

(defn- project-rows
  [projects]
  (mapv (fn [p]
          {:id (str (:id p))
           :name (or (not-empty (str (:name p))) "-")
           :root (or (not-empty (str (:workspace-root p))) "-")
           :sessions (str (or (:session-count p) 0))
           :state (if (:archived-at p) "archived" "active")})
        projects))

(defn- list-projects!
  "List projects (cross-channel, archived included) with their live session counts."
  []
  (let [projects (lp/projects {:archived :include})]
    (if (empty? projects)
      (commandline/stdout! "No projects found.")
      (do (commandline/stdout! "\n  Projects\n")
          (commandline/print-table!
            [{:key :id :label "ID" :width 36 :align :left}
             {:key :name :label "Name" :width 24 :align :left :grow? true}
             {:key :root :label "Workspace Root" :width 28 :align :left :grow? true}
             {:key :sessions :label "Sessions" :width 8 :align :right}
             {:key :state :label "State" :width 8 :align :left}]
            (project-rows projects))
          (commandline/stdout! (str "\n  " (count projects) " project(s)\n"))
          (commandline/stdout! "  Delete the project only:      vis-agent projects delete <ID>")
          (commandline/stdout!
            "  Delete it and its sessions:   vis-agent projects delete <ID> --with-sessions"))))
  (shutdown-agents))

(defn- delete-project-tree!
  "Delete project `project-id` and return a summary; prints nothing.

   Without `:with-sessions` this is the SCATTER delete the schema has always
   had: the row goes and its sessions survive as project-less. With it, every
   MEMBER session tree is deleted FIRST — an interrupted teardown must leave a
   project holding survivors, never orphans with a dead parent. Membership is
   not a client's visible list: untitled and empty conversations are members
   too, and a caller fanning out over what it can see would keep the rest."
  [d project-id {:keys [with-sessions]}]
  (let [pid
        (str project-id)

        member-ids
        (vec (lp/project-session-ids pid))]

    (when with-sessions
      (doseq [sid member-ids]
        ;; Same order as `vis-agent sessions delete`: trash the draft clones
        ;; (primary + auto-cloned roots) before the DB tree, and deref so
        ;; reclamation finishes before this one-shot JVM exits.
        (try (some-> (workspace/discard-session-clones! d sid)
                     deref)
             (catch Throwable _ nil))
        (lp/delete! sid)))
    (lp/delete-project! pid)
    {:project-id pid
     :deleted-session-ids (if with-sessions member-ids [])
     :kept-session-ids (if with-sessions [] member-ids)}))

(defn- cli-delete-project!
  "`vis-agent projects delete <PROJECT-ID> [--with-sessions]` handler."
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        project
        (project-or-exit! (get parsed "project-id"))

        with-sessions
        (boolean (get parsed "with-sessions"))

        {:keys [project-id deleted-session-ids kept-session-ids]}
        (delete-project-tree! d (:id project) {:with-sessions with-sessions})]

    (commandline/stdout! (str "Deleted project "
                              project-id
                              (when-let [n (not-empty (str (:name project)))]
                                (str " (" n ")"))))
    (if with-sessions
      (commandline/stdout! (str "  " (count deleted-session-ids) " session(s) deleted with it."))
      (when (seq kept-session-ids)
        (commandline/stdout!
          (str "  " (count kept-session-ids) " session(s) kept, now project-less."))
        (commandline/stdout! "  Delete one with: vis-agent sessions delete <ID>"))))
  (shutdown-agents))

(defn- cli-projects-list! [_parsed _residual] (config/init-cli!) (list-projects!))

(defn- cli-projects!
  "`vis-agent projects` default handler. Bare `vis-agent projects` lists them;
   every other operation is a canonical subcommand."
  [_parsed residual]
  (config/init-cli!)
  (if (seq residual)
    (do (commandline/stdout! (str "Unknown projects command: " (first residual)))
        (commandline/stdout! "")
        (commandline/stdout! "Run: vis-agent projects --help")
        (shutdown-agents)
        (System/exit 2))
    (list-projects!)))

(def command
  {:cmd/name "projects"
   :cmd/doc "List projects, or delete one (optionally with every session in it)."
   :cmd/usage "vis-agent projects <list|delete> [...]"
   :cmd/examples ["vis-agent projects" "vis-agent projects list"
                  "vis-agent projects delete 9f2c1a44"
                  "vis-agent projects delete 9f2c1a44 --with-sessions"]
   :cmd/subcommands #(registry/registered-under ["projects"])
   :cmd/run-fn cli-projects!})

(def subcommands
  "Subcommands registered under `vis-agent projects`."
  [{:cmd/name "list"
    :cmd/parent ["projects"]
    :cmd/doc "List projects with their live session counts."
    :cmd/usage "vis-agent projects list"
    :cmd/examples ["vis-agent projects list"]
    :cmd/run-fn cli-projects-list!}
   {:cmd/name "delete"
    :cmd/parent ["projects"]
    :cmd/doc "Delete a project; with --with-sessions, every session in it too."
    :cmd/usage "vis-agent projects delete <PROJECT-ID> [--with-sessions]"
    :cmd/args
    [{:name "project-id"
      :kind :positional
      :type :string
      :required true
      :doc "Project id (full UUID or unambiguous prefix)."}
     {:name "with-sessions"
      :kind :flag
      :type :boolean
      :doc "Also delete every session in the project (drafts included), not just the project row."}]
    :cmd/examples ["vis-agent projects delete 9f2c1a44"
                   "vis-agent projects delete 9f2c1a44 --with-sessions"]
    :cmd/run-fn cli-delete-project!}])
