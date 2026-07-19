(ns com.blockether.vis.internal.foundation.workspace-slashes
  "Declarative `/draft …` slash tree.

   Drafts are OPT-IN. By default a session works directly in the user's
   real cwd (trunk). `/draft new <label>` clones cwd into an isolated
   draft (an isolated workspace named `<label>`) and enters it; `/draft apply`
   lands the draft's changes into cwd and leaves the draft; `/draft
   abandon` discards it and leaves. The header shows `<label> (DRAFT)`
   while you're in one.

     /draft                show whether you're on trunk or in a draft
     /draft new <label>    clone cwd into a draft named <label>, enter it
     /draft apply          land the draft's changes into cwd, leave the draft
     /draft abandon [why]  discard the draft, leave it
     /draft-fresh <label>  like /draft new, but the draft starts EMPTY —
                           no files from the current HEAD are carried in

   Filesystem permissions (`/fs`, `/root`) — session-scoped, every channel:

     /root [path]          show / CHANGE the session's filesystem root
     /fs                   list the session's filesystem permissions
     /fs root <path>       same as /root <path>
     /fs add <path>        also let the session operate under <path>
     /fs remove <path>     drop an added directory
     /fs create <path>     mkdir + add it

   Vis owns no git lifecycle — `apply` copies the changed files into the
   user's real cwd, uncommitted. Handlers are PURE w.r.t. the channel."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace]))
;; =============================================================================
;; Helpers
;; =============================================================================
(defn- ctx-session-state-id [ctx] (:session/state-id ctx))
(defn- ctx-db [ctx] (or (:db-info ctx) (:db ctx)))
(defn- session-workspace
  "The workspace (trunk or draft) the current session is in."
  [ctx]
  (let [db (ctx-db ctx)]
    (or (when-let [state-id (ctx-session-state-id ctx)]
          (workspace/for-session db state-id))
        (when-let [wid (:workspace/id ctx)]
          (workspace/get db wid)))))
(defn- err [msg & {:as extras}] (merge {:slash/status :error :slash/title msg} extras))
(defn- sync-confinement!
  "Push the freshly-mutated workspace `ws` into the live sandbox confinement
   pointer (`:workspace-atom`, deref'd by the gateway's `sandbox-roots-fn` on
   every real-fs access) so a `/fs`/`/root` change takes effect THIS turn — not
   only from the next `run-turn!` workspace re-resolve. No-op when the ctx has no
   atom (other channels build their own ctx). Returns `ws` for threading."
  [ctx ws]
  (when ws
    (some-> (:workspace-atom ctx)
            (reset! ws)))
  ws)
(defn- change-line
  [{:keys [status path]}]
  (str (case status
         :add
         "+ "

         :modify
         "~ "

         :delete
         "- "

         "  ")
       path))
;; =============================================================================
;; Handlers
;; =============================================================================
(defn- handle-create
  "Shared `/draft new` + `/draft-fresh` implementation. `fresh?` forks an
   EMPTY draft — nothing from the current HEAD is carried into it."
  [ctx fresh?]
  (let [db
        (ctx-db ctx)

        state-id
        (ctx-session-state-id ctx)

        label
        (some-> (str/join " " (:command/argv ctx))
                str/trim
                not-empty)

        current
        (session-workspace ctx)

        usage
        (if fresh? "/draft-fresh <label>" "/draft new <label>")]

    (cond
      (nil? state-id) (err (str "Send a message first, then " usage " (session not ready yet)"))
      (workspace/draft? current) (err (str "Already in draft '"
                                           (workspace/display-label current)
                                           "' — /draft apply or /draft abandon it first"))
      ;; A draft MUST be named — an unlabeled draft is anonymous and
      ;; indistinguishable in the tab strip / draft list. The TUI prompts for
      ;; the label (see the `:slash/prompt-arg` on this spec); other channels
      ;; get this explicit nudge instead of a silent "draft" default.
      (nil? label) (err (str "Name the draft: " usage))
      (not (workspace/isolated-workspaces-supported? (or (:root current) (workspace/trunk-root))))
      (err "No workspace backend can create an isolated draft here"
           :slash/body "Drafts require isolation, rollback, merge-back, and retained revisions."
           :slash/data {:capability-matrix (workspace/workspace-capability-matrix
                                             (or (:root current) (workspace/trunk-root)))})
      :else
      (let [draft (workspace/create! db {:session-state-id state-id :label label :fresh? fresh?})]
        {:slash/status :ok
         :slash/title (str (if fresh? "Fresh draft '" "Draft '")
                           (workspace/display-label draft)
                           "' — you're in it now")
         :slash/body
         (if fresh?
           "Started EMPTY — nothing from your repo was carried in. /draft apply lands created files into your repo · /draft abandon discards."
           "Edits here stay isolated. /draft apply lands them into your repo · /draft abandon discards.")
         :slash/data {:workspace-id (:id draft) :label (:label draft) :fresh? fresh?}}))))

(defn- handle-new
  "`/draft new <label>` — clone cwd into a draft named <label> and enter it."
  [ctx]
  (handle-create ctx false))

(defn- handle-new-fresh
  "`/draft-fresh <label>` — like /draft new, but the draft starts EMPTY: no
   files from the current HEAD are carried into it."
  [ctx]
  (handle-create ctx true))

(defn- handle-apply
  "`/draft apply` — land the draft's changes into cwd, then leave the draft."
  [ctx]
  (let [db
        (ctx-db ctx)

        state-id
        (ctx-session-state-id ctx)

        current
        (session-workspace ctx)]

    (cond (nil? current) (err "No active workspace")
          (not (workspace/draft? current)) (err "Not in a draft — /draft new <label> to start one")
          :else (let [{:keys [landed changed]}
                      (workspace/apply! db {:workspace-id (:id current)})

                      label
                      (workspace/display-label current)]

                  (workspace/abandon! db {:workspace-id (:id current) :reason "applied"})
                  (when state-id (workspace/exit-to-trunk! db state-id))
                  {:slash/status :ok
                   :slash/title (str "Applied "
                                     landed
                                     " file"
                                     (when (not= 1 landed) "s")
                                     " — left draft '"
                                     label
                                     "', back in your repo")
                   :slash/body (->> changed
                                    (map change-line)
                                    (str/join "\n"))
                   :slash/data {:landed landed :changed changed}}))))
(defn- handle-abandon
  "`/draft abandon [reason]` — discard the draft and leave it."
  [ctx]
  (let [db
        (ctx-db ctx)

        state-id
        (ctx-session-state-id ctx)

        current
        (session-workspace ctx)

        reason
        (some-> (str/join " " (:command/argv ctx))
                str/trim
                not-empty)]

    (cond (nil? current) (err "No active workspace")
          (not (workspace/draft? current)) (err "Not in a draft")
          :else (let [label (workspace/display-label current)]
                  (workspace/abandon! db {:workspace-id (:id current) :reason reason})
                  (when state-id (workspace/exit-to-trunk! db state-id))
                  {:slash/status :ok
                   :slash/title (str "Abandoned draft '" label "' — back in your repo")
                   :slash/body (when reason (str "Reason: " reason))
                   :slash/data {:workspace-id (:id current) :reason reason}}))))
(defn- handle-status
  "Bare `/draft` — are you on trunk or in a draft?"
  [ctx]
  (let [db
        (ctx-db ctx)

        current
        (session-workspace ctx)]

    (cond (workspace/draft? current)
          (let [st (workspace/status db (:id current))]
            {:slash/status :ok
             :slash/title (str "Draft '" (workspace/display-label current) "'")
             :slash/body (str (:workspace/changed st 0)
                              " file(s) changed · "
                              "/draft apply to land them, /draft abandon to discard")
             :slash/data {:workspace-id (:id current)}})
          :else {:slash/status :ok
                 :slash/title "On trunk — your real repo"
                 :slash/body
                 "Editing your repo directly. /draft new <label> to start an isolated draft."})))
(defn- expand-home
  "Expand a leading `~` in a typed path to the user's home dir, so
   `/root ~/code/proj` works the way a shell user expects. Everything else
   passes through untouched."
  [path]
  (let [p (str path)]
    (cond (= p "~") (System/getProperty "user.home")
          (str/starts-with? p "~/") (str (System/getProperty "user.home") (subs p 1))
          :else p)))

(defn- argv-path
  "The handler's whole argv as one `~`-expanded path string, or nil."
  [ctx]
  (some-> (str/join " " (:command/argv ctx))
          str/trim
          not-empty
          expand-home))

(declare handle-fs-list)

(defn- handle-fs
  "Bare `/fs` — show the session's filesystem permissions (the root plus any
   additional granted directories)."
  [ctx]
  (handle-fs-list ctx))

(defn- handle-fs-root
  "`/fs root <path>` (also top-level `/root <path>`) — change the session's
   PRIMARY filesystem root. The session then works in <path>: shell cwd,
   relative paths, file tools, and search all follow. Additional filesystem
   roots carry over. Bare (no path) shows the current root."
  [ctx]
  (let [db
        (ctx-db ctx)

        state-id
        (ctx-session-state-id ctx)

        current
        (session-workspace ctx)

        path
        (argv-path ctx)]

    (cond (nil? path)
          {:slash/status :ok
           :slash/title (str "Root: " (or (:root current) "(none)"))
           :slash/body
           "/root <path> to work in a different directory. Additional filesystem roots carry over."
           :slash/data {:root (:root current)}}
          (nil? state-id) (err "Send a message first, then /root <path> (session not ready yet)")
          :else (try
                  (let [ws
                        (sync-confinement! ctx (workspace/change-root! db state-id path))

                        roots
                        (workspace/filesystem-roots ws)]

                    {:slash/status :ok
                     :slash/title (str "Root changed — now working in " (:root ws))
                     :slash/body
                     (str "Shell, file tools, and search operate here from the next turn."
                          (when (seq roots)
                            (str "\nAdditional filesystem roots kept (" (count roots)
                                 "):\n" (str/join "\n" (map #(str "  " (:trunk %)) roots)))))
                     :slash/data {:root (:root ws) :workspace-id (:id ws) :filesystem-roots roots}})
                  (catch Exception e
                    (err (str "Can't change root to '" path "': " (or (ex-message e) (str e)))))))))

(defn- handle-fs-add
  "`/fs add <path>` - widen the session so it may also operate on files under
   <path>, in addition to its primary filesystem root."
  [ctx]
  (let [db
        (ctx-db ctx)

        current
        (session-workspace ctx)

        path
        (argv-path ctx)]

    (cond (nil? current) (err "No active workspace")
          (nil? path) (err "Give a directory: /fs add <path>")
          :else
          (try (let [ws
                     (sync-confinement! ctx (workspace/add-filesystem-root! db (:id current) path))

                     roots
                     (workspace/filesystem-roots ws)]

                 {:slash/status :ok
                  :slash/title "Added a filesystem directory - the session can work there now"
                  :slash/body
                  (str "Filesystem dirs (" (count roots)
                       "):\n" (str/join
                                "\n"
                                (map #(str "  "
                                           (:trunk %)
                                           (when (and (:fork-ms %) (not= (:clone %) (:trunk %)))
                                             " (isolated draft copy — lands on /draft apply)"))
                                     roots)))
                  :slash/data {:filesystem-roots roots}})
               (catch Exception e
                 (err (str "Can't add '" path "': " (or (ex-message e) (str e)))))))))

(defn- handle-fs-create
  "`/fs create <path>` - make the directory <path> (its last segment, under an
   existing parent), then add it as a filesystem root so the session can work
   there. The parent must already exist; only the final segment is created."
  [ctx]
  (let [db
        (ctx-db ctx)

        current
        (session-workspace ctx)

        path
        (argv-path ctx)]

    (cond (nil? current) (err "No active workspace")
          (nil? path) (err "Give a directory: /fs create <path>")
          :else (try (let [f
                           (io/file path)

                           parent
                           (or (.getParent f) ".")

                           created
                           (workspace/create-dir! parent (.getName f))

                           ws
                           (sync-confinement!
                             ctx
                             (workspace/add-filesystem-root! db (:id current) created))

                           roots
                           (workspace/filesystem-roots ws)]

                       {:slash/status :ok
                        :slash/title (str "Created and added '" created "'")
                        :slash/body (str "Filesystem dirs (" (count roots)
                                         "):\n" (str/join "\n" (map #(str "  " (:trunk %)) roots)))
                        :slash/data {:filesystem-roots roots :created created}})
                     (catch Exception e
                       (err (str "Can't create '" path "': " (or (ex-message e) (str e)))))))))

(defn- handle-fs-remove
  "`/fs remove <path>` - stop letting the session operate under <path>."
  [ctx]
  (let [db
        (ctx-db ctx)

        current
        (session-workspace ctx)

        path
        (argv-path ctx)]

    (cond (nil? current) (err "No active workspace")
          (nil? path) (err "Give a directory: /fs remove <path>")
          :else (let [ws
                      (sync-confinement! ctx
                                         (workspace/remove-filesystem-root! db (:id current) path))

                      roots
                      (workspace/filesystem-roots ws)]

                  {:slash/status :ok
                   :slash/title "Removed a filesystem directory"
                   :slash/body
                   (if (seq roots)
                     (str "Filesystem dirs (" (count roots)
                          "):\n" (str/join
                                   "\n"
                                   (map #(str "  "
                                              (:trunk %)
                                              (when (and (:fork-ms %) (not= (:clone %) (:trunk %)))
                                                " (isolated draft copy — lands on /draft apply)"))
                                        roots)))
                     "No extra filesystem dirs - back to the primary root only.")
                   :slash/data {:filesystem-roots roots}}))))

(defn- handle-fs-list
  "`/fs list` (also bare `/fs`) - show the session's filesystem permissions.
   The ROOT (changeable via /root <path>) is always #1 — vis reads/edits
   there by default; added roots are extra grants. Enumerated so the listing
   matches the web rail's `Filesystem` section (root first)."
  [ctx]
  (let [current
        (session-workspace ctx)

        base
        (:root current)

        roots
        (some-> current
                workspace/filesystem-roots)

        total
        (+ (if base 1 0) (count roots))]

    {:slash/status :ok
     :slash/title "Filesystem"
     :slash/body
     (str
       "Filesystem roots ("
       total
       "):\n"
       (str/join "\n"
                 (remove nil?
                   (cons (when base (str "  " base "   (root — /root <path> to change)"))
                         (map #(str "  "
                                    (:trunk %)
                                    (when (and (:fork-ms %) (not= (:clone %) (:trunk %)))
                                      " (isolated draft copy — lands on /draft apply)"))
                              roots))))
       "\n\n/fs add <path> to widen, /fs remove <path> to drop one, /root <path> to change the root.")
     :slash/data {:filesystem-roots roots :root base}}))
;; =============================================================================
;; Specs vec
;; =============================================================================
(defn- build-specs
  "Slash specs vec. Commands are always discoverable; handlers report runtime
   capability availability for the active workspace."
  []
  (into
    [{:slash/name "draft"
      :slash/doc "Drafts — isolated workspace copies of your repo (opt-in)."
      :slash/usage "/draft <new <label> | apply | abandon>"
      :slash/ui {:kind :navigator}
      :slash/run-fn handle-status}
     {:slash/name "new"
      :slash/parent ["draft"]
      :slash/doc "Clone cwd into an isolated draft named <label> and enter it."
      :slash/usage "/draft new <label>"
      :slash/prompt-arg "Draft label (e.g. feature-x)"
      :slash/requires #{:session}
      :slash/run-fn handle-new}
     {:slash/name "apply"
      :slash/parent ["draft"]
      :slash/doc "Land the draft's changes into your repo and leave the draft."
      :slash/usage "/draft apply"
      :slash/requires #{:session}
      :slash/run-fn handle-apply}
     {:slash/name "abandon"
      :slash/parent ["draft"]
      :slash/doc "Discard the draft and leave it."
      :slash/usage "/draft abandon [reason]"
      :slash/requires #{:session}
      :slash/run-fn handle-abandon}
     {:slash/name "draft-fresh"
      :slash/doc
      "Like /draft new, but the draft starts EMPTY — no files from your current repo (HEAD) are carried in."
      :slash/usage "/draft-fresh <label>"
      :slash/prompt-arg "Draft label (e.g. feature-x)"
      :slash/requires #{:session}
      :slash/run-fn handle-new-fresh}]
    ;; Filesystem permissions — available on EVERY channel: rich channels keep
    ;; their pickers (TUI directory dialog, web rail), but /root and /fs work
    ;; typed anywhere, which is how a web session moves to a different project.
    [{:slash/name "root"
      :slash/doc "Show or change the session's filesystem root (the directory vis works in)."
      :slash/usage "/root [path]"
      :slash/run-fn handle-fs-root}
     {:slash/name "fs"
      :slash/doc "Filesystem permissions — the directories this session may read and edit."
      :slash/usage "/fs"
      ;; Rich channels realize bare `/fs` as their directory picker UI
      ;; (the TUI's Ctrl+G dialog); text channels run the list handler.
      :slash/ui {:kind :dir-picker}
      :slash/run-fn handle-fs}
     {:slash/name "root"
      :slash/parent ["fs"]
      :slash/doc "Change the session's filesystem root to <path>."
      :slash/usage "/fs root <path>"
      :slash/requires #{:session}
      :slash/run-fn handle-fs-root}
     {:slash/name "add"
      :slash/parent ["fs"]
      :slash/doc "Let the session also operate on files under <path>."
      :slash/usage "/fs add <path>"
      :slash/requires #{:session}
      :slash/run-fn handle-fs-add}
     {:slash/name "create"
      :slash/parent ["fs"]
      :slash/doc "Create a new directory and let the session operate under it."
      :slash/usage "/fs create <path>"
      :slash/requires #{:session}
      :slash/run-fn handle-fs-create}
     {:slash/name "remove"
      :slash/parent ["fs"]
      :slash/doc "Stop letting the session operate under <path>."
      :slash/usage "/fs remove <path>"
      :slash/requires #{:session}
      :slash/run-fn handle-fs-remove}
     {:slash/name "list"
      :slash/parent ["fs"]
      :slash/doc "Show the session's filesystem permissions."
      :slash/usage "/fs list"
      :slash/run-fn handle-fs-list}]))
(def specs
  "Declarative slash specs vec hooked onto foundation-core's manifest\n   via `:ext/slash-commands`. Capability checks happen when commands run."
  (build-specs))
