(ns com.blockether.vis.ext.foundation-core.workspace-slashes
  "Declarative `/draft …` slash tree.

   Surface registered on `vis-foundation-core`'s `:ext/slash-commands`
   vec. A session IS its draft (a `rift` CoW clone of cwd), locked 1:1,
   so the family is small:

     /draft [label]        fork a new draft from the current one
     /draft apply          land this draft's since-fork edits into cwd
     /draft list           list drafts (== sessions)
     /draft abandon [sel] <reason>   trash a draft (current or by selector)

   Vis owns no git lifecycle — `apply` copies changed files into the
   user's real cwd, uncommitted, and the user commits with their own
   tools. Each handler is `(fn [ctx])` returning a `:slash/*` envelope;
   handlers are PURE w.r.t. the channel."
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

;; =============================================================================
;; Handlers
;; =============================================================================

(defn- handle-new
  "`/draft [label]` — fork a fresh draft from the current one (clone-of-a-
   clone) and pin the current session to it. With no current draft (first
   call in a fresh session) clones cwd directly."
  [ctx]
  (let [db       (ctx-db ctx)
        state-id (ctx-session-state-id ctx)
        current  (session-workspace ctx)
        label    (some-> (str/join " " (:command/argv ctx)) str/trim not-empty)]
    (if (nil? state-id)
      (err "Cannot fork: no active session")
      (let [forked (workspace/create! db {:from             current
                                          :session-state-id state-id
                                          :label            label})]
        {:slash/status :ok
         :slash/title  "Draft created"
         :slash/body   (str (workspace/display-label forked)
                         (when current
                           (str " · forked from " (workspace/display-label current))))
         :slash/data   {:workspace-id (:id forked)
                        :label        (:label forked)
                        :parent-id    (:parent-id forked)}}))))

(defn- handle-apply
  "`/draft apply` — land the draft's since-fork edits into cwd."
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

(defn- handle-list
  "`/draft list` — drafts for the current repo (== sessions, 1:1)."
  [ctx]
  (let [db      (ctx-db ctx)
        current (session-workspace ctx)
        repo-id (or (:repo-id current)
                  (some-> (workspace/get db (:workspace/id ctx)) :repo-id))
        active  (when repo-id (workspace/list-active-with-sessions db repo-id))]
    {:slash/status :ok
     :slash/title  (str "Drafts (" (count active) ")")
     :slash/body   (->> active
                     (map (fn [{:keys [workspace session-state]}]
                            (str (if (= (:id workspace) (:id current)) "* " "  ")
                              (workspace/display-label nil workspace session-state))))
                     (str/join "\n"))
     :slash/data   {:active active :current-id (:id current)}}))

(defn- handle-abandon
  "`/draft abandon [selector] <reason>` — trash a draft. With a leading
   id-prefix / label / clone-name selector, targets that draft; otherwise
   the current one. Remaining words are the reason (logged into lineage)."
  [ctx]
  (let [db         (ctx-db ctx)
        current    (session-workspace ctx)
        repo-id    (:repo-id current)
        argv       (vec (:command/argv ctx))
        candidates (when repo-id (workspace/list-active db repo-id))
        selector   (first argv)
        match      (when selector
                     (some #(when (or (str/starts-with? (str (:id %)) selector)
                                    (= selector (:label %))
                                    (= selector (:branch %)))
                              %)
                       candidates))
        target     (or match current)
        reason     (str/trim (str/join " " (if match (rest argv) argv)))]
    (if (nil? target)
      (err "No draft to abandon")
      (let [done (workspace/abandon! db {:workspace-id (:id target)
                                         :reason       (not-empty reason)})]
        {:slash/status :ok
         :slash/title  "Draft abandoned"
         :slash/body   (str (workspace/display-label done)
                         (when (not-empty reason) (str " — " reason)))
         :slash/data   {:workspace-id (:id done) :reason (not-empty reason)}}))))

;; =============================================================================
;; Specs vec
;; =============================================================================

(def specs
  "Declarative slash specs vec hooked onto foundation-core's manifest
   via `:ext/slash-commands`."
  [{:slash/name   "draft"
    :slash/doc    "Draft (CoW workspace) operations — see subcommands."
    :slash/usage  "/draft <[label] | apply | list | abandon> …"
    ;; Bare `/draft` opens the session/draft navigator in picker-capable
    ;; channels (session == draft, 1:1); `/draft <label>` forks a new one.
    :slash/ui     {:kind :navigator}
    :slash/run-fn (fn [ctx]
                    (if (seq (:command/argv ctx))
                      (handle-new ctx)
                      {:slash/status :ok
                       :slash/title  "Draft commands"
                       :slash/body   (str "/draft [label]        fork a new draft from the current one\n"
                                       "/draft apply          land this draft's changes into cwd\n"
                                       "/draft list           list drafts\n"
                                       "/draft abandon [sel] <reason>   trash a draft")}))}
   {:slash/name     "apply"
    :slash/parent   ["draft"]
    :slash/doc      "Land this draft's since-fork edits into cwd (uncommitted)."
    :slash/usage    "/draft apply"
    :slash/requires #{:session}
    :slash/run-fn   handle-apply}
   {:slash/name     "list"
    :slash/parent   ["draft"]
    :slash/doc      "List drafts / sessions for the current repo."
    :slash/usage    "/draft list"
    :slash/requires #{:channel}
    :slash/ui       {:kind :navigator}
    :slash/run-fn   handle-list}
   {:slash/name     "abandon"
    :slash/parent   ["draft"]
    :slash/doc      "Trash a draft (current, or by selector) with an optional reason."
    :slash/usage    "/draft abandon [id|label] <reason>"
    :slash/requires #{:session}
    :slash/run-fn   handle-abandon}])
