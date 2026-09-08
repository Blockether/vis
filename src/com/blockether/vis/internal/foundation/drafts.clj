(ns com.blockether.vis.internal.foundation.drafts
  "Drafts as the user and the model reach them: the `/draft`, `/approve` and
   `/discard` slash commands, the `draft_status` / `draft_approve` sandbox
   symbols and the `draft_backend` toggle. Each is a thin layer over
   `workspace.drafts`, the boundary the daemon's HTTP routes use too, so an
   extension hook on `:draft/*` sees every surface alike."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.drafts :as drafts]))

(vis/register-toggle!
  {:id workspace/draft-backend-toggle-id
   :label "Draft backend"
   ;; One line for the Settings row (100 chars max); `doc("drafts")` has the rest.
   :description "How /draft isolates work: git worktree, Rift clone, auto (first fit) or off."
   :type :enum
   :choices ["auto" "worktree" "rift" "off"]
   :default "auto"
   :owner :vis
   :persist? true
   :group :sandbox})

;; Shared lookups: a slash ctx carries `:session/state-id` + `:session/id`, a
;; symbol env carries `:workspace/id` + `:session-id`; both carry `:db-info`.

(defn- db-of [m] (or (:db-info m) (:db m)))

(defn- boundary-env [m] {:db-info (db-of m) :session-id (or (:session-id m) (:session/id m))})

(defn- current-workspace
  "The workspace the session works in right now."
  [m]
  (when-let [db (db-of m)]
    (or (when-let [state-id (:session/state-id m)]
          (workspace/for-session db state-id))
        (when-let [wid (:workspace/id m)]
          (workspace/get db wid)))))

(defn- message-arg
  "`draft_approve(\"subject\")` or `draft_approve({\"message\": ...})` — the subject, or nil."
  [x]
  (some-> (cond (string? x) x
                (map? x) (or (get x "message") (:message x))
                :else nil)
          str
          str/trim
          not-empty))

;; Sandbox symbols

(defn draft-status
  "Status of the session's current draft."
  [env]
  (let [ws (current-workspace env)]
    (extension/success {:op :draft-status
                        :result (wire/canonical (if (workspace/draft? ws)
                                                  (assoc (drafts/status ws) :in-draft true)
                                                  {:in-draft false
                                                   :root (:root ws)
                                                   :backend-setting
                                                   (name (workspace/draft-backend-setting))}))})))

(defn draft-approve
  "Land the session's current draft on its `vis/<label>` branch."
  [env & [message]]
  (let [ws (current-workspace env)]
    (if-not (workspace/draft? ws)
      (extension/failure
        {:error {:message
                 (str "Not in a draft: draft_approve lands the session's current draft and this "
                      "session works on its trunk. The user opens one with /draft <name>.")}})
      (try (extension/success {:op :draft-approve
                               :result (wire/canonical (dissoc (drafts/approve!
                                                                 (boundary-env env)
                                                                 {:workspace-id (:id ws)
                                                                  :message (message-arg message)})
                                                         :workspace))})
           (catch clojure.lang.ExceptionInfo e (extension/failure {:throwable e}))))))

(def draft-status-symbol
  (vis/symbol
    #'draft-status
    {:inject-env? true
     :tag :observation
     :description
     (str
       "Where this session's work lands — `draft_status()` says whether the session is inside a "
       "draft (an isolated working copy the user opened with /draft) and, if so, which backend "
       "holds it, the `vis/<name>` branch approvals commit to, how many approved commits the trunk "
       "lacks (`ahead`) and how many paths still differ from that branch (`pending`). Outside a "
       "draft it reports the trunk root and the `draft_backend` setting.")
     :result
     (str "String-keyed `{in_draft, root, ...}`; in a draft also `{workspace_id, label, repo_root, "
          "backend, mechanism, branch, ahead, pending}`.")}))

(def draft-approve-symbol
  (vis/symbol
    #'draft-approve
    {:inject-env? true
     :tag :mutation
     :description
     (str
       "Land the session's current draft as ONE commit on its `vis/<name>` branch — every changed "
       "and untracked path is staged, the commit carries `Vis-Session`/`Vis-Draft` trailers, and "
       "the trunk checkout is left untouched (the branch is reachable from it). "
       "`draft_approve()` uses the default subject, `draft_approve(\"subject\")` yours. The draft "
       "stays open, so later work can be approved again. Only meaningful inside a draft; the user "
       "opens one with /draft and reviews the branch afterwards. Extension hooks on `draft/approve` "
       "may veto the landing.")
     :params [{:name "message" :note "commit subject; default `draft(<name>): approve`"}]
     :call {:lead-opt "message" :rest :never}
     :result (str "String-keyed `{status: approved|nothing_to_approve, branch, commit, files}`; "
                  "`files` lists the paths that landed.")}))

(def symbols [draft-status-symbol draft-approve-symbol])

;; Slash commands

(defn- err [msg & {:as extras}] (merge {:slash/status :error :slash/title msg} extras))

(defn- refusal
  "A refused draft operation as a slash error: the thrown message, and the
   canonical `:hint` when the refusal carries one."
  [^clojure.lang.ExceptionInfo e]
  (let [hint (:hint (ex-data e))]
    (cond-> (err (ex-message e))
      hint
      (assoc :slash/body hint))))

(defn- sync-confinement!
  "Push `ws` into the live sandbox confinement pointer for this turn."
  [ctx ws]
  (when ws
    (some-> (:workspace-atom ctx)
            (reset! ws)))
  ws)

(defn- draft-args
  "`/draft <name...> [--clean]` — words become the label, joined by `-`."
  [argv]
  (let [words
        (remove str/blank? (map str argv))

        clean?
        (boolean (some #{"--clean"} words))]

    {:label (not-empty (str/join "-" (remove #{"--clean"} words))) :clean? clean?}))

(defn- handle-draft
  "`/draft <name> [--clean]` — open a draft from the trunk and work inside it.
   A draft already open is parked, never lost."
  [ctx]
  (let [db
        (db-of ctx)

        state-id
        (:session/state-id ctx)

        {:keys [label clean?]}
        (draft-args (:command/argv ctx))]

    (cond (or (nil? db) (nil? state-id)) (err "Drafts need a persisted session")
          (nil? label) (err "Name the draft" :slash/body "Usage: /draft <name> [--clean]")
          :else
          (let [current
                (current-workspace ctx)

                repo-root
                (or (:repo-root current) (:root current) (workspace/trunk-root))]

            (if-not (workspace/isolated-workspaces-supported? repo-root)
              (err "Drafts are not available here"
                   :slash/body
                   (workspace/isolation-unavailable-hint repo-root))
              (try (when (workspace/draft? current) (workspace/stash! db state-id))
                   (let [trunk
                         (workspace/for-session db state-id)

                         ws
                         (drafts/create!
                           (boundary-env ctx)
                           {:session-state-id state-id :label label :from trunk :clean? clean?})

                         {:keys [backend branch] :as status}
                         (drafts/status ws)]

                     (sync-confinement! ctx ws)
                     {:slash/status :ok
                      :slash/title (str "Draft '"
                                        (:label ws)
                                        "' open ("
                                        backend
                                        (when branch (str ", branch " branch))
                                        ")")
                      :slash/body (str "Working copy: "
                                       (:root ws)
                                       (if clean?
                                         ". Seeded from HEAD; pending trunk changes stayed behind."
                                         ". Pending trunk changes came along.")
                                       (if branch
                                         (str " /approve lands the work on "
                                              branch
                                              "; /discard throws the working copy away.")
                                         " /discard throws the working copy away."))
                      :slash/data (wire/canonical status)})
                   (catch clojure.lang.ExceptionInfo e (refusal e))))))))

(defn- handle-approve
  "`/approve [subject]` — land the current draft on its `vis/<name>` branch."
  [ctx]
  (let [ws
        (current-workspace ctx)

        message
        (message-arg (str/join " " (:command/argv ctx)))]

    (if-not (workspace/draft? ws)
      (err "Not in a draft" :slash/body "/draft <name> opens one; /approve lands it.")
      (try (let [{:keys [status branch commit files]}
                 (drafts/approve! (boundary-env ctx) {:workspace-id (:id ws) :message message})]
             (if (= :nothing-to-approve status)
               {:slash/status :ok
                :slash/title "Nothing to approve"
                :slash/body (str "The draft already matches " branch ".")}
               {:slash/status :ok
                :slash/title (str "Approved " (count files) " path(s) on " branch)
                :slash/body
                (str "Commit "
                     (subs (str commit) 0 (min 12 (count (str commit))))
                     ". The draft stays open; the trunk checkout is untouched — merge or review "
                     branch
                     " there. /discard removes the working copy, not the branch.")
                :slash/data (wire/canonical
                              {:status status :branch branch :commit commit :files files})}))
           (catch clojure.lang.ExceptionInfo e (refusal e))))))

(defn- handle-discard
  "`/discard` — leave the current draft and remove its working copy. Approved
   commits stay on the `vis/<name>` branch."
  [ctx]
  (let [db
        (db-of ctx)

        state-id
        (:session/state-id ctx)

        ws
        (current-workspace ctx)]

    (cond (or (nil? db) (nil? state-id)) (err "Drafts need a persisted session")
          (not (workspace/draft? ws))
          (err "Not in a draft" :slash/body "There is nothing to discard on the trunk.")
          :else (try (let [{:keys [branch ahead]}
                           (drafts/status ws)

                           trunk
                           (workspace/exit-to-trunk! db state-id (:repo-root ws))]

                       (drafts/discard! (boundary-env ctx)
                                        {:workspace-id (:id ws) :reason "discarded with /discard"})
                       (sync-confinement! ctx trunk)
                       {:slash/status :ok
                        :slash/title (str "Discarded draft '" (:label ws) "'")
                        :slash/body
                        (str "Back on " (:root trunk)
                             ". " (if (and branch (pos? (or ahead 0)))
                                    (str "Its " ahead " approved commit(s) stay on " branch ".")
                                    "Nothing had been approved from it."))})
                     (catch clojure.lang.ExceptionInfo e (refusal e))))))

(def specs
  "Declarative slash specs hooked onto foundation-core's manifest."
  [{:slash/name "draft"
    :slash/doc "Open an isolated draft of this repository and work inside it."
    :slash/usage "/draft <name> [--clean]"
    :slash/run-fn handle-draft}
   {:slash/name "approve"
    :slash/doc "Land the current draft as a commit on its vis/<name> branch."
    :slash/usage "/approve [subject]"
    :slash/run-fn handle-approve}
   {:slash/name "discard"
    :slash/doc "Leave the current draft and remove its working copy; approved commits stay."
    :slash/usage "/discard"
    :slash/run-fn handle-discard}])
