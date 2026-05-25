(ns com.blockether.vis.ext.foundation-core.workspace-slashes
  "Declarative `/workspace …` slash tree.

   Surface registered on `vis-foundation-core`'s
   `:ext/slash-commands` vec. Engine aggregates declarative slash
   sets from every active extension via `vis/active-slashes`; this
   namespace owns the workspace family.

   Each handler is `(fn [ctx])` returning a `:slash/*` envelope:

     {:slash/status :ok | :error | :nothing-to-commit | :ff-failed
      :slash/title  short headline
      :slash/body   multi-line detail (optional)
      :slash/actions [{:label … :slash …}]   ;; optional follow-ups
      :slash/data    arbitrary payload (workspace-id, sha, …)}

   Handlers are PURE w.r.t. the channel — they never call channel
   APIs directly. The dispatch layer (`internal/slash.clj`) wraps
   the call so the engine + channels render results from a single
   contract."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- ctx-session-state-id
  "Pull the session-state UUID from a slash ctx. Engine stamps both
   `:session/id` (soul id) and `:session/state-id` (state row id); the
   state id is what `workspace/*` core fns need."
  [ctx]
  (or (:session/state-id ctx)
    (:session-state-id ctx)))

(defn- ctx-db [ctx]
  (or (:db-info ctx) (:db ctx)))

(defn- session-workspace
  "Resolve the workspace pinned to the current session-state. Returns
   nil when the session has no pin yet (pre-ensure-trunk!)."
  [ctx]
  (when-let [state-id (ctx-session-state-id ctx)]
    (workspace/for-session (ctx-db ctx) state-id)))

(defn- err [msg & {:as extras}]
  (merge {:slash/status :error :slash/title msg} extras))

;; =============================================================================
;; Handlers (one per command)
;; =============================================================================

(defn- handle-new
  [ctx]
  (let [db        (ctx-db ctx)
        state-id  (ctx-session-state-id ctx)
        current   (session-workspace ctx)
        label     (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? state-id)
      (err "Cannot spawn workspace: no active session")

      ;; Refuse `new` from a branch-kind session. Avoids the
      ;; fork-from-branch ambiguity; user must apply / discard /
      ;; switch first.
      (and current (= :branch (:kind current)))
      (err (str "You are in workspace " (or (:label current) (:branch current))
             "; use /workspace apply, /workspace discard, or /workspace switch first."))

      :else
      (let [spawned (workspace/spawn-branch! db
                      {:from              current
                       :session-state-id  state-id})
            spawned (if label
                      (workspace/set-label! db
                        {:workspace-id (:id spawned) :label label})
                      spawned)]
        {:slash/status :ok
         :slash/title  "Workspace created"
         :slash/body   (str (workspace/display-label spawned) " · " (:branch spawned))
         :slash/data   {:workspace-id (:id spawned)
                        :branch       (:branch spawned)
                        :label        (:label spawned)}}))))

(defn- handle-commit
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        message (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? current)
      (err "No active workspace")

      (= :trunk (:kind current))
      (err "Cannot commit on trunk workspace")

      (str/blank? message)
      (err "Commit message required: /workspace commit <message>")

      :else
      (let [result (workspace/commit! db {:workspace-id (:id current)
                                          :message      message})]
        (case (:status result)
          :ok               {:slash/status :ok
                             :slash/title  "Committed"
                             :slash/body   (str (subs (:sha result) 0 8) " — " (:message result))
                             :slash/data   result}
          :nothing-to-commit {:slash/status :nothing-to-commit
                              :slash/title  "Nothing to commit"
                              :slash/data   result})))))

(defn- handle-apply
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)]
    (cond
      (nil? current)
      (err "No active workspace")

      (= :trunk (:kind current))
      (err "Cannot apply trunk onto itself")

      :else
      (let [result (workspace/ff-apply! db {:workspace-id (:id current)})]
        (case (:status result)
          :ok        {:slash/status :ok
                      :slash/title  "Workspace applied"
                      :slash/body   (str (:branch result) " -> " (:trunk result "trunk")
                                      " (" (subs (:sha result) 0 8) ")")
                      :slash/data   result}
          :ff-failed {:slash/status :ff-failed
                      :slash/title  "FF apply failed"
                      :slash/body   (:reason result)
                      :slash/actions [{:label "Resolve merge"
                                       :slash "/workspace merge-resolve"}
                                      {:label "Discard workspace"
                                       :slash "/workspace discard"}]
                      :slash/data   result})))))

(defn- handle-discard
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        hard?   (boolean (some #{"--hard"} (:command/argv ctx)))]
    (cond
      (nil? current)        (err "No active workspace")
      (= :trunk (:kind current))
      (err "Cannot discard trunk workspace")

      :else
      (let [done (workspace/discard! db
                   {:workspace-id   (:id current)
                    :delete-branch? hard?
                    :force?         hard?})]
        {:slash/status :ok
         :slash/title  (if hard? "Workspace fully discarded" "Workspace worktree removed")
         :slash/body   (str (:branch done) " state=" (:state done))
         :slash/data   {:workspace-id (:id done)
                        :hard?        hard?
                        :state        (:state done)}}))))

(defn- handle-list
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        repo-id (some :repo-id [current (workspace/get db (:workspace/id ctx))])
        active  (when repo-id (workspace/list-active-with-sessions db repo-id))]
    {:slash/status :ok
     :slash/title  (str "Workspaces (" (count active) " active)")
     :slash/body   (->> active
                     (map (fn [{:keys [workspace session-state]}]
                            (str (if (= (:id workspace) (:id current)) "* " "  ")
                              (workspace/display-label nil workspace session-state)
                              "  [" (name (:kind workspace)) " " (:branch workspace) "]")))
                     (str/join "\n"))
     :slash/data   {:active active :current-id (:id current)}}))

(defn- handle-switch
  [ctx]
  (let [db        (ctx-db ctx)
        selector  (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)
        current   (session-workspace ctx)
        repo-id   (:repo-id current)
        candidates (when repo-id (workspace/list-active db repo-id))
        chosen     (when selector
                     (or
                       ;; uuid prefix match
                       (some #(when (str/starts-with? (str (:id %)) selector) %) candidates)
                       ;; label match
                       (some #(when (= selector (:label %)) %) candidates)
                       ;; branch match
                       (some #(when (= selector (:branch %)) %) candidates)))]
    (cond
      (nil? selector)
      (err "/workspace switch <id|label|branch>")

      (nil? chosen)
      (err (str "No workspace matches selector " (pr-str selector)))

      :else
      (do (workspace/focus! db (:id chosen))
        {:slash/status :ok
         :slash/title  "Workspace focused"
         :slash/body   (workspace/display-label chosen)
         :slash/data   {:workspace-id (:id chosen)}}))))

(defn- handle-label
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        argv    (:command/argv ctx)
        clear?  (boolean (some #{"--clear"} argv))
        text    (when-not clear? (some-> (str/join " " argv) str/trim not-empty))]
    (cond
      (nil? current)
      (err "No active workspace")

      (and (not clear?) (nil? text))
      (err "/workspace label <text>  |  /workspace label --clear")

      :else
      (let [done (workspace/set-label! db
                   {:workspace-id (:id current)
                    :label        (when-not clear? text)})]
        {:slash/status :ok
         :slash/title  (if clear? "Label cleared" "Label set")
         :slash/body   (workspace/display-label done)
         :slash/data   {:workspace-id (:id done)
                        :label        (:label done)}}))))

;; =============================================================================
;; Availability predicates
;; =============================================================================

(defn- branch-ws? [ctx]
  (= :branch (:kind (session-workspace ctx))))

(defn- branch-and-not-merged? [ctx]
  (let [ws (session-workspace ctx)]
    (and (= :branch (:kind ws))
      (contains? #{:active :merging} (:state ws)))))

;; =============================================================================
;; Specs vec
;; =============================================================================

(def specs
  "Declarative slash specs vec hooked onto foundation-core's manifest
   via `:ext/slash-commands`."
  [{:slash/name   "workspace"
    :slash/doc    "Workspace operations (see subcommands)."
    :slash/usage  "/workspace <new|commit|apply|discard|list|switch|label> …"
    :slash/run-fn (fn [_ctx]
                    {:slash/status :ok
                     :slash/title  "Workspace commands"
                     :slash/body   (str "/workspace new [label?]\n"
                                     "/workspace commit <message>\n"
                                     "/workspace apply\n"
                                     "/workspace discard [--hard]\n"
                                     "/workspace list\n"
                                     "/workspace switch <id|label|branch>\n"
                                     "/workspace label <text>|--clear")})}
   {:slash/name     "new"
    :slash/parent   ["workspace"]
    :slash/doc      "Spawn a fresh :branch workspace from trunk; pins the current session 1:1."
    :slash/usage    "/workspace new [label?]"
    :slash/requires #{:session}
    :slash/run-fn   handle-new}
   {:slash/name     "commit"
    :slash/parent   ["workspace"]
    :slash/doc      "Stage all worktree changes and commit (refuses trunk + empty diff)."
    :slash/usage    "/workspace commit <message>"
    :slash/requires #{:session}
    :slash/availability-fn branch-ws?
    :slash/run-fn   handle-commit}
   {:slash/name     "apply"
    :slash/parent   ["workspace"]
    :slash/doc      "Fast-forward trunk onto this workspace's branch."
    :slash/usage    "/workspace apply"
    :slash/requires #{:session}
    :slash/availability-fn branch-and-not-merged?
    :slash/run-fn   handle-apply}
   {:slash/name     "discard"
    :slash/parent   ["workspace"]
    :slash/doc      "Drop the worktree (soft) or worktree + branch (--hard)."
    :slash/usage    "/workspace discard [--hard]"
    :slash/requires #{:session}
    :slash/availability-fn branch-and-not-merged?
    :slash/run-fn   handle-discard}
   {:slash/name     "list"
    :slash/parent   ["workspace"]
    :slash/doc      "List active workspaces for the current repo."
    :slash/usage    "/workspace list"
    :slash/requires #{:channel}
    :slash/run-fn   handle-list}
   {:slash/name     "switch"
    :slash/parent   ["workspace"]
    :slash/doc      "Focus another workspace by id-prefix, label, or branch."
    :slash/usage    "/workspace switch <id|label|branch>"
    :slash/requires #{:channel}
    :slash/run-fn   handle-switch}
   {:slash/name     "label"
    :slash/parent   ["workspace"]
    :slash/doc      "Set or clear the human-friendly workspace label."
    :slash/usage    "/workspace label <text>|--clear"
    :slash/requires #{:session}
    :slash/run-fn   handle-label}])
