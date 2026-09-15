(ns com.blockether.vis.internal.foundation.drafts
  "Drafts as the model reaches them: the `draft_create`, `draft_status`,
   `draft_approve` and `draft_discard` sandbox symbols and the `draft_backend`
   toggle. Only the agent manages drafts — no channel offers a slash command
   or a picker for them. Each symbol is a thin layer over `workspace.drafts`,
   the boundary the daemon's HTTP routes use too, so an extension hook on
   `:draft/*` sees every surface alike."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as capture]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.drafts :as drafts]))

(vis/register-toggle! {:id workspace/draft-backend-toggle-id
                       :label "Draft backend"
                       ;; One line for the Settings row (100 chars max); `doc("drafts")` has the rest.
                       :description
                       "Require drafts for changes: auto-select, git worktree, Rift clone, or off."
                       :type :enum
                       :choices ["auto" "worktree" "rift" "off"]
                       :default "auto"
                       :owner :vis
                       :persist? true
                       :group :sandbox})

(def ^:private DRAFT_WORKFLOW_PROMPT
  (str
    "## Draft workflow\n"
    "- `draft_backend` is enabled: this is standing authorization to create drafts without asking. Isolate every change-making task (code, tests, documentation and configuration) in its own session-owned draft. Read-only questions, analysis and diff previews do not require a draft.\n"
    "- Check `session[\"workspace\"]` or `draft_status()`. Use `draft_create(\"task-name\")` before editing, or continue this session's draft for the same task. For an added read/write root, pass `root=<its Path variable>`; a shared root can be isolated for this task without changing configuration. Never edit the shared checkout or another session's draft. Creation defaults to committed HEAD; use `clean=False` only when the task includes the pending checkout changes.\n"
    "- For a new clean task, fetch origin and base the draft on `origin/<target_branch>` before editing (create, then fast-forward the clean draft if needed). Without origin, use the committed local target. Never reset or overwrite unrelated checkout work; report divergent history instead.\n"
    "- After creation, use `project_root_path` and `session[\"workspace\"]` from the next block, not cached checkout paths. Keep edits, formatting and verification in the draft.\n"
    "- If drafts are unavailable or blocked, report the blocker; never silently fall back to shared-checkout edits or bypass the draft tools with an ad-hoc worktree or clone.\n"
    "- Use `draft_diff()` for a requested review. `draft_approve()` commits and may push: call it only after relevant checks pass and the user or applicable project instructions authorize commit and push. Review-first, local-only and no-commit/push requests leave the draft unapproved.\n"
    "- When a task is complete, use `draft_status()` and Git to verify no pending changes, all draft commits merged into the target, and required publication succeeded. Then call `draft_discard()` without asking: cleanup of a completed, merged draft is already authorized. Do not discard an active task's draft merely because it is clean.\n"
    "- Confirm destructive discard only when unapproved changes or unmerged commits would be lost. Keep incomplete drafts and drafts awaiting review or required publication. Read `doc(\"drafts\")` for backend limits and approval recovery.\n"))

(defn prompt
  "Contribute the draft workflow for enabled backends; off contributes nothing."
  [_env]
  (when-not (= :off (workspace/draft-backend-setting)) DRAFT_WORKFLOW_PROMPT))

;; The injected env carries `:db-info`, `:session-id`, `:session/state-id`,
;; `:workspace/id` and `:workspace-atom`, the live sandbox confinement pointer.

(defn- db-of [env] (or (:db-info env) (:db env)))

(defn- boundary-env
  [env]
  (assoc (select-keys env [:session-id :security-policy :security/filesystem-roots])
    :db-info (db-of env)))

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

(defn- create-args
  "Normalize positionals and the map that carries Python keyword arguments."
  [label clean root]
  (let [options (into {}
                      (comp (filter map?)
                            cat
                            (map (fn [[k v]]
                                   [(keyword k) v])))
                      [label clean root])]
    {:label (if (map? label) (:label options) label)
     :clean? (not (false? (get options :clean clean)))
     :root (if (map? root) (:root options) (or root (:root options)))}))

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

(defn draft-diff
  "Attach the active draft's immutable patch and return its reusable checkpoint."
  [env & [filename since]]
  (let [ws
        (current-workspace env)

        filename
        (or filename (str "DIFF-" (:label ws) ".json"))]

    (cond (not (workspace/draft? ws)) (not-in-draft "draft_diff()")
          (nil? capture/*attachment-sink*) (failure
                                             "draft_diff requires an active attachment collector.")
          (or (not (string? filename))
              (str/blank? filename)
              (re-find #"[/\\]" filename)
              (not (str/ends-with? filename ".json")))
          (failure "Use a stable JSON filename, such as DIFF-feature.json.")
          :else
          (try (let [{:keys [patch source checkpoint]}
                     (drafts/diff (boundary-env env) {:workspace-id (:id ws) :since since})

                     text
                     (diff/render {"schema_version" 1 "patch" patch "source" source "comments" []})

                     bytes
                     (.getBytes ^String text java.nio.charset.StandardCharsets/UTF_8)

                     recorded
                     (capture/record-attachment!
                       {:kind "diff"
                        :media-type diff/media-type
                        :filename filename
                        :audience "user"
                        :commentable true
                        :size (alength bytes)
                        :base64 (.encodeToString (java.util.Base64/getEncoder) bytes)})]

                 (if recorded
                   (extension/success {:op :draft-diff
                                       :result (wire/canonical (assoc (dissoc recorded :base64)
                                                                 :checkpoint checkpoint
                                                                 :empty (empty? patch)))})
                   (failure "The draft diff could not be attached. Retry the capture.")))
               (catch clojure.lang.ExceptionInfo e (refusal e))))))

(defn draft-create
  "Open a draft of the selected source and move the session into it."
  [env label & [clean root]]
  (let [db
        (db-of env)

        state-id
        (state-id-of env)

        {:keys [label clean? root]}
        (create-args label clean root)

        label
        (some-> label
                str
                str/trim
                not-empty)

        current
        (current-workspace env)]

    (cond (or (nil? db) (nil? state-id)) (failure "Drafts need a persisted session.")
          (nil? label) (failure "Name the draft: draft_create(\"name\").")
          (workspace/draft? current) (failure (str "Already in draft '" (:label current)
                                                   "': draft_approve() what should land, "
                                                   "then draft_discard() before opening another."))
          :else (try (let [ws (drafts/create! (boundary-env env)
                                              {:session-state-id state-id
                                               :label label
                                               :from current
                                               :clean? clean?
                                               :root root})]
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

(def draft-diff-symbol
  (vis/symbol
    #'draft-diff
    {:activity (presenter/for-tool :draft_diff)
     :inject-env? true
     :tag :observation
     :description
     (str
       "Attach the active draft's exact changes as a reviewable diff. Defaults to changes since "
       "the seeded fork; inherited pending work is excluded. Pass since=<checkpoint> from a "
       "previous result for task-only changes. Neither the working index nor commits are changed. "
       "Works with worktree and Rift drafts, including non-Git Rift directories; Git is required. "
       "The same filename stores the next attachment version. Old drafts without a baseline refuse.")
     :params [{:name "filename" :note "stable JSON filename; default DIFF-<draft-label>.json"}
              {:name "since" :note "snapshot checkpoint; omit for cumulative diff"}]
     :call {:opt-pos ["filename" "since"]}
     :result
     "Attachment descriptor plus `checkpoint` and `empty`. Patch bytes stay in the attachment."}))

(def draft-create-symbol
  (vis/symbol
    #'draft-create
    {:activity (presenter/for-tool :draft_create)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Open a draft — an isolated working copy of this repository, or an added read/write root — and move the session into it. "
       "The trunk checkout is left alone until approval. Drafts default to committed HEAD (clean=True); "
       "clean=False explicitly copies pending trunk work and can cause overlap refusals on approval. "
       "Pass root=<Path> to select a session filesystem root without changing its configured draft policy. "
       "Read-only, copy-only, not-allowed and overlapping denied paths cannot be selected. "
       "Approval lands in the selected repository; discard returns to the original project. "
       "Approval preserves unrelated local work. Discard the current draft before opening another. "
       "Extension hooks on `draft/create` may refuse.")
     :params [{:name "label" :note "draft name; also the `vis/<label>` branch"}
              {:name "clean" :note "False copies pending changes; default True"}
              {:name "root" :note "existing read/write session root; omit for the current project"}]
     :call {:pos ["label"] :opt-pos ["clean" "root"]}
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

(def symbols
  [draft-status-symbol draft-diff-symbol draft-create-symbol draft-approve-symbol
   draft-discard-symbol])
