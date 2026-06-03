(ns com.blockether.vis.ext.foundation-core.workspace-slashes
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
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- ctx-session-state-id [ctx]
  (or (:session/state-id ctx) (:session-state-id ctx)))

(defn- ctx-db [ctx]
  (or (:db-info ctx) (:db ctx)))

(defn- session-workspace
  "The workspace (trunk or draft) the current session is in."
  [ctx]
  (let [db (ctx-db ctx)]
    (or (when-let [state-id (ctx-session-state-id ctx)]
          (workspace/for-session db state-id))
      (when-let [wid (:workspace/id ctx)]
        (workspace/get db wid)))))

(defn- err [msg & {:as extras}]
  (merge {:slash/status :error :slash/title msg} extras))

(defn- change-line [{:keys [status path]}]
  (str (case status :add "+ " :modify "~ " :delete "- " "  ") path))

;; =============================================================================
;; Handlers
;; =============================================================================

(defn- handle-new
  "`/draft new <label>` — clone cwd into a draft named <label> and enter it."
  [ctx]
  (let [db       (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        label    (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)
        current  (session-workspace ctx)]
    (cond
      (nil? state-id)
      (err "Send a message first, then /draft new <label> (session not ready yet)")

      (workspace/draft? current)
      (err (str "Already in draft '" (workspace/display-label current)
             "' — /draft apply or /draft abandon it first"))

      ;; A draft MUST be named — an unlabeled draft is anonymous and
      ;; indistinguishable in the tab strip / draft list. The TUI prompts for
      ;; the label (see the `:slash/prompt-arg` on this spec); other channels
      ;; get this explicit nudge instead of a silent "draft" default.
      (nil? label)
      (err "Name the draft: /draft new <label>")

      :else
      (let [draft (workspace/create! db {:session-state-id state-id :label label})]
        {:slash/status :ok
         :slash/title  (str "Draft '" (workspace/display-label draft) "' — you're in it now")
         :slash/body   "Edits here stay isolated. /draft apply lands them into your repo · /draft abandon discards."
         :slash/data   {:workspace-id (:id draft) :label (:label draft)}}))))

(defn- handle-apply
  "`/draft apply` — land the draft's changes into cwd, then leave the draft."
  [ctx]
  (let [db       (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        current  (session-workspace ctx)]
    (cond
      (nil? current)                    (err "No active workspace")
      (not (workspace/draft? current))  (err "Not in a draft — /draft new <label> to start one")
      :else
      (let [{:keys [landed changed]} (workspace/apply! db {:workspace-id (:id current)})
            label (workspace/display-label current)]
        (workspace/abandon! db {:workspace-id (:id current) :reason "applied"})
        (when state-id (workspace/exit-to-trunk! db state-id))
        {:slash/status :ok
         :slash/title  (str "Applied " landed " file" (when (not= 1 landed) "s")
                         " — left draft '" label "', back in your repo")
         :slash/body   (->> changed (map change-line) (str/join "\n"))
         :slash/data   {:landed landed :changed changed}}))))

(defn- handle-abandon
  "`/draft abandon [reason]` — discard the draft and leave it."
  [ctx]
  (let [db       (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        current  (session-workspace ctx)
        reason   (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? current)                    (err "No active workspace")
      (not (workspace/draft? current))  (err "Not in a draft")
      :else
      (let [label (workspace/display-label current)]
        (workspace/abandon! db {:workspace-id (:id current) :reason reason})
        (when state-id (workspace/exit-to-trunk! db state-id))
        {:slash/status :ok
         :slash/title  (str "Abandoned draft '" label "' — back in your repo")
         :slash/body   (when reason (str "Reason: " reason))
         :slash/data   {:workspace-id (:id current) :reason reason}}))))

(defn- handle-status
  "Bare `/draft` — are you on trunk or in a draft?"
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)]
    (cond
      (workspace/draft? current)
      (let [st (workspace/status db (:id current))]
        {:slash/status :ok
         :slash/title  (str "Draft '" (workspace/display-label current) "'")
         :slash/body   (str (:workspace/changed st 0) " file(s) changed · "
                         "/draft apply to land them, /draft abandon to discard")
         :slash/data   {:workspace-id (:id current)}})
      :else
      {:slash/status :ok
       :slash/title  "On trunk — your real repo"
       :slash/body   "Editing your repo directly. /draft new <label> to start an isolated draft."})))

(defn- handle-dir
  "`/dir` is realized by the TUI as a directory picker (`:slash/ui
   {:kind :dir-picker}`) — it opens a session in the chosen directory in its
   own tab. On non-TUI channels it has no effect; say so."
  [_ctx]
  {:slash/status :ok
   :slash/title  "Open a directory"
   :slash/body   "Use /dir in the TUI to open a session in another directory, in its own tab."})

;; =============================================================================
;; Specs vec
;; =============================================================================

(def specs
  "Declarative slash specs vec hooked onto foundation-core's manifest
   via `:ext/slash-commands`."
  [{:slash/name   "draft"
    :slash/doc    "Drafts — isolated rift clones of your repo (opt-in)."
    :slash/usage  "/draft <new <label> | apply | abandon>"
    :slash/ui     {:kind :navigator}
    :slash/run-fn handle-status}
   {:slash/name       "new"
    :slash/parent     ["draft"]
    :slash/doc        "Clone cwd into an isolated draft named <label> and enter it."
    :slash/usage      "/draft new <label>"
    ;; Typed with no label, the TUI pops a text-input asking for it rather
    ;; than creating an anonymous draft.
    :slash/prompt-arg "Draft label (e.g. feature-x)"
    :slash/requires   #{:session}
    :slash/run-fn     handle-new}
   {:slash/name     "apply"
    :slash/parent   ["draft"]
    :slash/doc      "Land the draft's changes into your repo and leave the draft."
    :slash/usage    "/draft apply"
    :slash/requires #{:session}
    :slash/run-fn   handle-apply}
   {:slash/name     "abandon"
    :slash/parent   ["draft"]
    :slash/doc      "Discard the draft and leave it."
    :slash/usage    "/draft abandon [reason]"
    :slash/requires #{:session}
    :slash/run-fn   handle-abandon}
   {:slash/name   "dir"
    :slash/doc    "Open a session in another directory, in its own tab."
    :slash/usage  "/dir"
    :slash/ui     {:kind :dir-picker}
    :slash/run-fn handle-dir}])
