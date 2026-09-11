(ns com.blockether.vis.internal.foundation.drafts
  "Drafts as the model reaches them: the `draft_create`, `draft_status`,
   `draft_approve` and `draft_discard` sandbox symbols and the `draft_backend`
   toggle. Only the agent manages drafts — no channel offers a slash command
   or a picker for them. Each symbol is a thin layer over `workspace.drafts`,
   the boundary the daemon's HTTP routes use too, so an extension hook on
   `:draft/*` sees every surface alike."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.drafts :as drafts]))

(vis/register-toggle!
  {:id workspace/draft-backend-toggle-id
   :label "Draft backend"
   ;; One line for the Settings row (100 chars max); `doc("drafts")` has the rest.
   :description "How the agent isolates a draft: git worktree, Rift clone, auto (first fit) or off."
   :type :enum
   :choices ["auto" "worktree" "rift" "off"]
   :default "auto"
   :owner :vis
   :persist? true
   :group :sandbox})

;; The injected env carries `:db-info`, `:session-id`, `:session/state-id`,
;; `:workspace/id` and `:workspace-atom`, the live sandbox confinement pointer.

(defn- db-of [env] (or (:db-info env) (:db env)))

(defn- boundary-env [env] {:db-info (db-of env) :session-id (:session-id env)})

(defn- state-id-of
  "The session state the draft pins to: the env's, else the session's latest."
  [env]
  (or (:session/state-id env)
      (when-let [db (db-of env)]
        (some->> (:session-id env)
                 (persistance/db-latest-session-state-id db)))))

(defn- current-workspace
  "The workspace the session works in right now."
  [env]
  (when-let [db (db-of env)]
    (or (when-let [state-id (state-id-of env)]
          (workspace/for-session db state-id))
        (when-let [wid (:workspace/id env)]
          (workspace/get db wid)))))

(defn- sync-confinement!
  "Push `ws` into the live sandbox confinement pointer, so the rest of this
   turn reads and writes inside it."
  [env ws]
  (when ws
    (some-> (:workspace-atom env)
            (reset! ws)))
  ws)

(defn- message-arg
  "`draft_approve(\"subject\")` or `draft_approve({\"message\": ...})` — the subject, or nil."
  [x]
  (some-> (cond (string? x) x
                (map? x) (or (get x "message") (:message x))
                :else nil)
          str
          str/trim
          not-empty))

(defn- clean-arg
  "`draft_create(name, clean=True)` or `draft_create(name, {\"clean\": true})`."
  [x]
  (not (false? (if (map? x) (get x "clean" (get x :clean true)) x))))

(defn- failure [message] (extension/failure {:error {:message message}}))

(defn- not-in-draft
  [tool]
  (failure (str tool ": not in a draft; use draft_create(\"name\") first.")))

(defn- refusal
  "A refused draft operation: the thrown message, plus the canonical `:hint`
   when the refusal carries one."
  [^clojure.lang.ExceptionInfo e]
  (let [hint (:hint (ex-data e))]
    (failure (if (str/blank? (str hint)) (ex-message e) (str (ex-message e) " " hint)))))

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

(defn draft-create
  "Open a draft of the trunk and move the session into it."
  [env label & [clean]]
  (let [db
        (db-of env)

        state-id
        (state-id-of env)

        label
        (some-> label
                str
                str/trim
                not-empty)

        clean?
        (clean-arg clean)

        current
        (current-workspace env)

        repo-root
        (or (:repo-root current) (:root current) (workspace/trunk-root))]

    (cond (or (nil? db) (nil? state-id)) (failure "Drafts need a persisted session.")
          (nil? label) (failure "Name the draft: draft_create(\"name\").")
          (workspace/draft? current) (failure (str "Already in draft '" (:label current)
                                                   "': draft_approve() what should land, "
                                                   "then draft_discard() before opening another."))
          (not (workspace/isolated-workspaces-supported? repo-root))
          (failure (str "Drafts are not available here. "
                        (workspace/isolation-unavailable-hint repo-root)))
          :else
          (try (let [ws (drafts/create!
                          (boundary-env env)
                          {:session-state-id state-id :label label :from current :clean? clean?})]
                 (sync-confinement! env ws)
                 (extension/success {:op :draft-create
                                     :result (wire/canonical (assoc (drafts/status ws)
                                                               :in-draft true
                                                               :clean clean?))}))
               (catch clojure.lang.ExceptionInfo e (refusal e))))))

(defn draft-approve
  "Commit the session draft and merge it into the repository's local default branch."
  [env & [message]]
  (let [ws (current-workspace env)]
    (if-not (workspace/draft? ws)
      (not-in-draft "draft_approve()")
      (try (extension/success {:op :draft-approve
                               :result (wire/canonical (dissoc (drafts/approve!
                                                                 (boundary-env env)
                                                                 {:workspace-id (:id ws)
                                                                  :message (message-arg message)})
                                                         :workspace))})
           (catch clojure.lang.ExceptionInfo e (refusal e))))))

(defn draft-discard
  "Leave the session's current draft and remove its working copy. Approved
   commits stay on the `vis/<label>` branch."
  [env]
  (let [db
        (db-of env)

        state-id
        (state-id-of env)

        ws
        (current-workspace env)]

    (cond (or (nil? db) (nil? state-id)) (failure "Drafts need a persisted session.")
          (not (workspace/draft? ws)) (not-in-draft "draft_discard()")
          :else (try (let [{:keys [branch ahead]}
                           (drafts/status ws)

                           [_discarded trunk]
                           (drafts/discard! (boundary-env env)
                                            {:workspace-id (:id ws)
                                             :reason "discarded with draft_discard()"
                                             :session-state-id state-id})]

                       (sync-confinement! env trunk)
                       (extension/success {:op :draft-discard
                                           :result (wire/canonical {:status :discarded
                                                                    :label (:label ws)
                                                                    :root (:root trunk)
                                                                    :branch branch
                                                                    :approved-ahead (or ahead
                                                                                        0)})}))
                     (catch clojure.lang.ExceptionInfo e (refusal e))))))

(def draft-status-symbol
  (vis/symbol
    #'draft-status
    {:activity (presenter/for-tool :draft_status)
     :inject-env? true
     :tag :observation
     :description
     (str
       "Where this session's work lands — `draft_status()` says whether the session is inside a "
       "draft (an isolated working copy opened with `draft_create`) and, if so, which backend "
       "holds it, its `vis/<name>` branch and the default `target_branch`, how many draft commits "
       "the target lacks (`ahead`) and how many paths still differ from the draft branch (`pending`). "
       "draft it reports the trunk root and the `draft_backend` setting.")
     :result
     (str "String-keyed `{in_draft, root, ...}`; in a draft also `{workspace_id, label, repo_root, "
          "backend, mechanism, branch, target_branch, ahead, pending}`.")}))

(def draft-create-symbol
  (vis/symbol
    #'draft-create
    {:activity (presenter/for-tool :draft_create)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Open a draft — an isolated working copy of this repository — and move the session into it. "
       "The trunk checkout is left alone until approval. Drafts default to committed HEAD (clean=True); "
       "clean=False explicitly copies pending trunk work and can cause overlap refusals on approval. "
       "Approval preserves unrelated local work. Discard the current draft before opening another. "
       "Extension hooks on `draft/create` may refuse.")
     :params [{:name "label" :note "draft name; also the `vis/<label>` branch"}
              {:name "clean" :note "False copies pending changes; default True"}]
     :call {:pos ["label"] :opt-pos ["clean"]}
     :result
     (str
       "String-keyed `{in_draft: true, workspace_id, label, root, repo_root, backend, mechanism, "
       "branch, target_branch, ahead, pending, clean}`. Work under `root`: the sandbox is confined to it at once, "
       "and `session[\"workspace\"]` / `project_root_path` follow from the next block on.")}))

(def draft-approve-symbol
  (vis/symbol
    #'draft-approve
    {:activity (presenter/for-tool :draft_approve)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Commit the draft and fast-forward the default branch (origin/HEAD, otherwise main or master). "
       "Fetch origin when configured; the draft must contain both local and origin target commits. "
       "If synchronization is required, merge or rebase in the draft, resolve conflicts and retry. "
       "Approval never merges target history. All pending draft paths are staged; commits carry "
       "Vis-Session/Vis-Draft trailers. Overlapping local paths are refused before landing. "
       "Unrelated local changes are stashed including untracked files, then restored with --index. "
       "Push to origin without force only after restoration succeeds. A failed restore retains the stash; "
       "a failed push reports landed locally and permits retry. No origin means local-only approval. "
       "The draft stays open. Hooks on draft/approve and git/commit may veto.")
     :params [{:name "message" :note "commit subject; default `draft(<name>): approve`"}]
     :call {:lead-opt "message" :rest :never}
     :result
     (str
       "String-keyed `{status: approved|nothing-to-approve, published, branch, target_branch, commit, files}`; "
       "`files` lists the paths that landed.")}))

(def draft-discard-symbol
  (vis/symbol
    #'draft-discard
    {:activity (presenter/for-tool :draft_discard)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Leave the session's current draft and remove its working copy; the session is back on the "
       "trunk at once. Approved work stays on the default branch; the merged draft branch may be "
       "removed. Call `draft_approve()` first to keep the work. Unapproved changes are lost. Only meaningful "
       "inside a draft. Extension hooks on `draft/discard` may refuse.")
     :result (str
               "String-keyed `{status: discarded, label, root, branch, approved_ahead}`; `root` is "
               "the trunk the session works in again.")}))

(def symbols [draft-status-symbol draft-create-symbol draft-approve-symbol draft-discard-symbol])
