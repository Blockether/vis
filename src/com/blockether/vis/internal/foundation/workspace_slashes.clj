(ns com.blockether.vis.internal.foundation.workspace-slashes
  "Declarative `/draft …` slash tree.

   Drafts are OPT-IN. By default a session works directly in the user's
   real cwd (trunk). `/draft new <label>` clones cwd into an isolated
   draft (a rift clone named `<label>`) and enters it; `/draft apply`
   lands the draft's changes into cwd and leaves the draft; `/draft
   abandon` discards it and leaves. The header shows `<label> (DRAFT)`
   while you're in one.

     /draft                show whether you're on trunk or in a draft
     /draft new <label>    clone cwd into a draft named <label>, enter it
     /draft apply          land the draft's changes into cwd, leave the draft
     /draft abandon [why]  discard the draft, leave it

   Vis owns no git lifecycle — `apply` copies the changed files into the
   user's real cwd, uncommitted. Handlers are PURE w.r.t. the channel."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace]))
;; =============================================================================
;; Helpers
;; =============================================================================
(defn- ctx-session-state-id [ctx] (or (:session/state-id ctx) (:session-state-id ctx)))
(defn- ctx-db [ctx] (or (:db-info ctx) (:db ctx)))
(defn- session-workspace
  "The workspace (trunk or draft) the current session is in."
  [ctx]
  (let [db (ctx-db ctx)]
    (or (when-let [state-id (ctx-session-state-id ctx)] (workspace/for-session db state-id))
      (when-let [wid (:workspace/id ctx)] (workspace/get db wid)))))
(defn- err [msg & {:as extras}] (merge {:slash/status :error, :slash/title msg} extras))
(defn- change-line
  [{:keys [status path]}]
  (str (case status
         :add "+ "
         :modify "~ "
         :delete "- "
         "  ")
    path))
;; =============================================================================
;; Handlers
;; =============================================================================
(defn- handle-new
  "`/draft new <label>` — clone cwd into a draft named <label> and enter it."
  [ctx]
  (let [db (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        label (some-> (str/join " " (:command/argv ctx))
                str/trim
                not-empty)
        current (session-workspace ctx)]
    (cond
      (nil? state-id) (err "Send a message first, then /draft new <label> (session not ready yet)")
      (workspace/draft? current) (err (str "Already in draft '"
                                        (workspace/display-label current)
                                        "' — /draft apply or /draft abandon it first"))
      ;; A draft MUST be named — an unlabeled draft is anonymous and
      ;; indistinguishable in the tab strip / draft list. The TUI prompts for
      ;; the label (see the `:slash/prompt-arg` on this spec); other channels
      ;; get this explicit nudge instead of a silent "draft" default.
      (nil? label) (err "Name the draft: /draft new <label>")
      :else
      (let [draft (workspace/create! db {:session-state-id state-id, :label label})]
        {:slash/status :ok,
         :slash/title (str "Draft '" (workspace/display-label draft) "' — you're in it now"),
         :slash/body
         "Edits here stay isolated. /draft apply lands them into your repo · /draft abandon discards.",
         :slash/data {:workspace-id (:id draft), :label (:label draft)}}))))
(defn- handle-apply
  "`/draft apply` — land the draft's changes into cwd, then leave the draft."
  [ctx]
  (let [db (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        current (session-workspace ctx)]
    (cond (nil? current) (err "No active workspace")
      (not (workspace/draft? current)) (err "Not in a draft — /draft new <label> to start one")
      :else (let [{:keys [landed changed]} (workspace/apply! db {:workspace-id (:id current)})
                  label (workspace/display-label current)]
              (workspace/abandon-lineage! db {:workspace-id (:id current), :reason "applied"})
              (when state-id (workspace/exit-to-trunk! db state-id))
              {:slash/status :ok,
               :slash/title (str "Applied "
                              landed
                              " file"
                              (when (not= 1 landed) "s")
                              " — left draft '"
                              label
                              "', back in your repo"),
               :slash/body (->> changed
                             (map change-line)
                             (str/join "\n")),
               :slash/data {:landed landed, :changed changed}}))))
(defn- handle-abandon
  "`/draft abandon [reason]` — discard the draft and leave it."
  [ctx]
  (let [db (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        current (session-workspace ctx)
        reason (some-> (str/join " " (:command/argv ctx))
                 str/trim
                 not-empty)]
    (cond (nil? current) (err "No active workspace")
      (not (workspace/draft? current)) (err "Not in a draft")
      :else (let [label (workspace/display-label current)]
              (workspace/abandon-lineage! db {:workspace-id (:id current), :reason reason})
              (when state-id (workspace/exit-to-trunk! db state-id))
              {:slash/status :ok,
               :slash/title (str "Abandoned draft '" label "' — back in your repo"),
               :slash/body (when reason (str "Reason: " reason)),
               :slash/data {:workspace-id (:id current), :reason reason}}))))
(defn- handle-status
  "Bare `/draft` — are you on trunk or in a draft?"
  [ctx]
  (let [db (ctx-db ctx)
        current (session-workspace ctx)]
    (cond (workspace/draft? current)
      (let [st (workspace/status db (:id current))]
        {:slash/status :ok,
         :slash/title (str "Draft '" (workspace/display-label current) "'"),
         :slash/body (str (:workspace/changed st 0)
                       " file(s) changed · "
                       "/draft apply to land them, /draft abandon to discard"),
         :slash/data {:workspace-id (:id current)}})
      :else {:slash/status :ok,
             :slash/title "On trunk — your real repo",
             :slash/body
             "Editing your repo directly. /draft new <label> to start an isolated draft."})))
(defn- handle-dir
  "`/dir` is realized by the TUI as a directory picker (`:slash/ui
   {:kind :dir-picker}`) — it opens a session in the chosen directory in its
   own tab. On non-TUI channels it has no effect; say so."
  [_ctx]
  {:slash/status :ok,
   :slash/title "Open a directory",
   :slash/body "Use /dir in the TUI to open a session in another directory, in its own tab."})

(defn- handle-dir-add
  "`/dir add <path>` - widen the session so it may also operate on files under
   <path>, in addition to its primary workspace root."
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        path    (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? current) (err "No active workspace")
      (nil? path)    (err "Give a directory: /dir add <path>")
      :else
      (try
        (let [ws    (workspace/add-context-root! db (:id current) path)
              roots (workspace/context-roots ws)]
          {:slash/status :ok,
           :slash/title  "Added a context directory - the session can work there now",
           :slash/body   (str "Context dirs (" (count roots) "):\n"
                           (str/join "\n" (map #(str "  " (:trunk %)
                                                  (when (and (:fork-ms %) (not= (:clone %) (:trunk %)))
                                                    " (isolated draft copy — lands on /draft apply)"))
                                            roots))),
           :slash/data   {:context-roots roots}})
        (catch Exception e
          (err (str "Can't add '" path "': " (or (ex-message e) (str e)))))))))

(defn- handle-dir-create
  "`/dir create <path>` - make the directory <path> (its last segment, under an
   existing parent), then add it as a context root so the session can work
   there. The parent must already exist; only the final segment is created."
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        path    (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? current) (err "No active workspace")
      (nil? path)    (err "Give a directory: /dir create <path>")
      :else
      (try
        (let [f       (io/file path)
              parent  (or (.getParent f) ".")
              created (workspace/create-dir! parent (.getName f))
              ws      (workspace/add-context-root! db (:id current) created)
              roots   (workspace/context-roots ws)]
          {:slash/status :ok
           :slash/title  (str "Created and added '" created "'")
           :slash/body   (str "Context dirs (" (count roots) "):\n"
                           (str/join "\n" (map #(str "  " (:trunk %)) roots)))
           :slash/data   {:context-roots roots :created created}})
        (catch Exception e
          (err (str "Can't create '" path "': " (or (ex-message e) (str e)))))))))

(defn- handle-dir-remove
  "`/dir remove <path>` - stop letting the session operate under <path>."
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        path    (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? current) (err "No active workspace")
      (nil? path)    (err "Give a directory: /dir remove <path>")
      :else
      (let [ws    (workspace/remove-context-root! db (:id current) path)
            roots (workspace/context-roots ws)]
        {:slash/status :ok,
         :slash/title  "Removed a context directory",
         :slash/body   (if (seq roots)
                         (str "Context dirs (" (count roots) "):\n"
                           (str/join "\n" (map #(str "  " (:trunk %)
                                                  (when (and (:fork-ms %) (not= (:clone %) (:trunk %)))
                                                    " (isolated draft copy — lands on /draft apply)"))
                                            roots)))
                         "No extra context dirs - back to the primary root only."),
         :slash/data   {:context-roots roots}}))))

(defn- handle-dir-list
  "`/dir list` - show the directories the session may operate on. The
   workspace root (the dir vis was started in) is ALWAYS context root #1 —
   vis reads/edits there by default; added roots are extras. Enumerated so
   the listing matches the web rail's `Context roots` (workspace first)."
  [ctx]
  (let [current (session-workspace ctx)
        base    (:root current)
        roots   (some-> current workspace/context-roots)
        total   (+ (if base 1 0) (count roots))]
    {:slash/status :ok,
     :slash/title  "Context directories",
     :slash/body   (str "Context roots (" total "):\n"
                     (str/join "\n"
                       (remove nil?
                         (cons (when base (str "  " base "   (workspace — always set)"))
                           (map #(str "  " (:trunk %)
                                   (when (and (:fork-ms %) (not= (:clone %) (:trunk %)))
                                     " (isolated draft copy — lands on /draft apply)"))
                             roots))))
                     "\n\n/dir add <path> to widen, /dir remove <path> to drop one."),
     :slash/data   {:context-roots roots :root base}}))
;; =============================================================================
;; Specs vec
;; =============================================================================
(defn- build-specs
  "Slash specs vec. `/draft …` is included only when this filesystem can do\n   a real rift CoW clone (`workspace/rift-supported?`); `/dir` is always\n   present. Computed once for `specs` below."
  []
  (into (if (workspace/rift-supported?)
          [{:slash/name "draft",
            :slash/doc "Drafts — isolated rift clones of your repo (opt-in).",
            :slash/usage "/draft <new <label> | apply | abandon>",
            :slash/ui {:kind :navigator},
            :slash/run-fn handle-status}
           {:slash/name "new",
            :slash/parent ["draft"],
            :slash/doc "Clone cwd into an isolated draft named <label> and enter it.",
            :slash/usage "/draft new <label>",
            :slash/prompt-arg "Draft label (e.g. feature-x)",
            :slash/requires #{:session},
            :slash/run-fn handle-new}
           {:slash/name "apply",
            :slash/parent ["draft"],
            :slash/doc "Land the draft's changes into your repo and leave the draft.",
            :slash/usage "/draft apply",
            :slash/requires #{:session},
            :slash/run-fn handle-apply}
           {:slash/name "abandon",
            :slash/parent ["draft"],
            :slash/doc "Discard the draft and leave it.",
            :slash/usage "/draft abandon [reason]",
            :slash/requires #{:session},
            :slash/run-fn handle-abandon}]
          [])
    [{:slash/name "dir",
      :slash/doc "Open a session in another directory, in its own tab.",
      :slash/usage "/dir",
      :slash/ui {:kind :dir-picker},
      :slash/run-fn handle-dir}
     {:slash/name "add",
      :slash/parent ["dir"],
      :slash/doc "Let the session also operate on files under <path>.",
      :slash/usage "/dir add <path>",
      :slash/prompt-arg "Directory to add (e.g. ../other-repo)",
      :slash/requires #{:session},
      :slash/run-fn handle-dir-add}
     {:slash/name "create",
      :slash/parent ["dir"],
      :slash/doc "Create a new directory and let the session operate under it.",
      :slash/usage "/dir create <path>",
      :slash/prompt-arg "New directory to create (e.g. ../new-repo)",
      :slash/requires #{:session},
      :slash/run-fn handle-dir-create}
     {:slash/name "remove",
      :slash/parent ["dir"],
      :slash/doc "Stop letting the session operate under <path>.",
      :slash/usage "/dir remove <path>",
      :slash/prompt-arg "Directory to remove",
      :slash/requires #{:session},
      :slash/run-fn handle-dir-remove}
     {:slash/name "list",
      :slash/parent ["dir"],
      :slash/doc "Show the directories the session may operate on.",
      :slash/usage "/dir list",
      :slash/run-fn handle-dir-list}]))
(def specs
  "Declarative slash specs vec hooked onto foundation-core's manifest\n   via `:ext/slash-commands`. `/draft …` appears only on a CoW-capable\n   filesystem; see `build-specs`."
  (build-specs))
