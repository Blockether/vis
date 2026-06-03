(ns com.blockether.vis.ext.foundation-core.workspace-slashes
  "Declarative `/draft …` slash tree.

   A session IS its draft — a single `rift` CoW clone of cwd, locked 1:1.
   Exactly one draft is active at a time; there is no parallel/multi-draft
   juggling (parallelism comes from running multiple sessions, each with
   its own draft). So the family is tiny:

     /draft            show what's in the current draft (or the navigator)
     /draft apply      land the draft's since-fork changes into cwd
     /draft abandon [reason]   discard the draft's work, start a fresh clone

   Vis owns no git lifecycle — `apply` copies the changed files (adds,
   edits, deletions) into the user's real cwd, uncommitted; the user
   commits with their own tools. Handlers are PURE w.r.t. the channel."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- ctx-session-state-id
  "Session-state UUID from a slash ctx (engine stamps `:session/state-id`)."
  [ctx]
  (or (:session/state-id ctx)
    (:session-state-id ctx)))

(defn- ctx-db [ctx]
  (or (:db-info ctx) (:db ctx)))

(defn- session-workspace
  "The draft pinned to the current session-state, or nil."
  [ctx]
  (when-let [state-id (ctx-session-state-id ctx)]
    (workspace/for-session (ctx-db ctx) state-id)))

(defn- err [msg & {:as extras}]
  (merge {:slash/status :error :slash/title msg} extras))

(defn- change-line [{:keys [status path]}]
  (str (case status :add "+ " :modify "~ " :delete "- " "  ") path))

(def ^:private help-body
  (str "/draft            show the current draft\n"
    "/draft apply      land the draft's changes into cwd\n"
    "/draft abandon [reason]   discard the draft, start fresh"))

;; =============================================================================
;; Handlers
;; =============================================================================

(defn- handle-status
  "Bare `/draft` — summarise the current draft (root + since-fork change
   count). Picker-capable channels open the navigator instead."
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)]
    (if (nil? current)
      {:slash/status :ok :slash/title "Draft" :slash/body help-body}
      (let [st (workspace/status db (:id current))]
        {:slash/status :ok
         :slash/title  (str "Draft · " (workspace/display-label current))
         :slash/body   (str (:workspace/root st) "\n"
                         (:workspace/changed st 0) " file(s) changed since fork")
         :slash/data   {:workspace-id (:id current)
                        :changed      (:workspace/changed st 0)}}))))

(defn- handle-apply
  "`/draft apply` — land the draft's since-fork changes into cwd."
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)]
    (if (nil? current)
      (err "No active draft")
      (let [{:keys [landed changed]} (workspace/apply! db {:workspace-id (:id current)})]
        {:slash/status :ok
         :slash/title  (str "Applied " landed " file" (when (not= 1 landed) "s") " to cwd")
         :slash/body   (->> changed (map change-line) (str/join "\n"))
         :slash/data   {:workspace-id (:id current) :landed landed :changed changed}}))))

(defn- handle-abandon
  "`/draft abandon [reason]` — discard the current draft's work and start
   a fresh clone of cwd. The session always has exactly one active draft,
   so abandoning immediately re-mints a clean one. `reason` is logged into
   the abandoned draft's lineage record."
  [ctx]
  (let [db       (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        current  (session-workspace ctx)
        reason   (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (cond
      (nil? state-id) (err "No active session")
      (nil? current)  (err "No active draft")
      :else
      (let [done  (workspace/abandon! db {:workspace-id (:id current) :reason reason})
            fresh (workspace/create! db {:session-state-id state-id})]
        {:slash/status :ok
         :slash/title  "Draft discarded — fresh draft started"
         :slash/body   (str "Discarded " (workspace/display-label done)
                         (when reason (str " — " reason)))
         :slash/data   {:abandoned-id (:id done)
                        :active-id    (:id fresh)
                        :reason       reason}}))))

(defn- handle-label
  "`/draft label <text>` — name the current draft (or `--clear`)."
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        argv    (:command/argv ctx)
        clear?  (boolean (some #{"--clear"} argv))
        text    (when-not clear? (some-> (str/join " " argv) str/trim not-empty))]
    (cond
      (nil? current)
      (err "No active draft")

      (and (not clear?) (nil? text))
      (err "/draft label <text>  |  /draft label --clear")

      :else
      (let [done (workspace/set-label! db {:workspace-id (:id current)
                                           :label        (when-not clear? text)})]
        {:slash/status :ok
         :slash/title  (if clear? "Label cleared" "Draft labelled")
         :slash/body   (workspace/display-label done)
         :slash/data   {:workspace-id (:id done) :label (:label done)}}))))

;; =============================================================================
;; Specs vec
;; =============================================================================

(def specs
  "Declarative slash specs vec hooked onto foundation-core's manifest
   via `:ext/slash-commands`."
  [{:slash/name   "draft"
    :slash/doc    "The session's draft (CoW workspace) — see subcommands."
    :slash/usage  "/draft <apply | abandon | label> …"
    ;; Bare `/draft` opens the session/draft navigator in picker-capable
    ;; channels (session == draft, 1:1); elsewhere it prints draft status.
    :slash/ui     {:kind :navigator}
    :slash/run-fn handle-status}
   {:slash/name     "apply"
    :slash/parent   ["draft"]
    :slash/doc      "Land the draft's since-fork changes into cwd (uncommitted)."
    :slash/usage    "/draft apply"
    :slash/requires #{:session}
    :slash/run-fn   handle-apply}
   {:slash/name     "abandon"
    :slash/parent   ["draft"]
    :slash/doc      "Discard the draft's work and start a fresh clone of cwd."
    :slash/usage    "/draft abandon [reason]"
    :slash/requires #{:session}
    :slash/run-fn   handle-abandon}
   {:slash/name     "label"
    :slash/parent   ["draft"]
    :slash/doc      "Name the current draft (or --clear to reset)."
    :slash/usage    "/draft label <text>|--clear"
    :slash/requires #{:session}
    :slash/run-fn   handle-label}])
