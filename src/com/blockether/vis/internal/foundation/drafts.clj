(ns com.blockether.vis.internal.foundation.drafts
  "Draft creation, review, synchronization, approval and discard as sandbox symbols.
   Only the agent manages drafts; channels show state but do not expose a draft
   picker. Lifecycle mutations cross workspace.drafts extension operation hooks."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.extension :as ext]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as capture]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.drafts :as drafts]))

(ext/register-toggle!
  {:id workspace/draft-backend-toggle-id
   :label "Draft backend"
   ;; One line for the Settings row (100 chars max); `doc("drafts")` has the rest.
   :description "Off by default. Enable drafts for changes with auto, git worktree or Rift clone."
   :type :enum
   :choices ["auto" "worktree" "rift" "off"]
   :default "off"
   :experimental? true
   :owner :vis
   :persist? true
   :group :experimental})

(def ^:private DRAFT_WORKFLOW_PROMPT
  (str
    "## Draft workflow\n"
    "- `draft_backend` is enabled: this is standing authorization to create drafts without asking. Isolate every change-making task (code, tests, documentation and configuration) in its own session-owned draft. Read-only questions, analysis and diff previews do not require a draft.\n"
    "- Check `session[\"workspace\"]` or `draft_status()`. Use `draft_create(\"task-name\")` before editing, or continue this session's draft for the same task. For several repositories, pass `roots=[project_root_path, sibling_path]`; the first is primary and every selected source gets its own working copy. Never edit the shared checkout or another session's draft. Creation defaults to committed HEAD; use `clean=False` only when the task includes the pending checkout changes.\n"
    "- For a new clean task, use `draft_sync()` to fetch origin and incorporate `origin/<target_branch>` before editing. Without origin it uses the committed local target. Synchronization can checkpoint changes or create merge commits, so it needs commit authorization. Resolve reported conflicts in draft files and call `draft_sync(action=\"continue\")`, or abort this owned merge with `draft_sync(action=\"abort\")`. Never reset or overwrite unrelated checkout work.\n"
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
  (assoc (select-keys env
                      [:session-id :workspace-atom :security-policy :security/filesystem-roots
                       :workspace/drafts-home :workspace/draft-protected-roots])
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
  [label clean roots]
  (let [options (into {}
                      (comp (filter map?)
                            cat
                            (map (fn [[k v]]
                                   [(keyword k) v])))
                      [label clean roots])]
    {:label (if (map? label) (:label options) label)
     :clean? (not (false? (get options :clean clean)))
     :roots (if (map? roots) (:roots options) (or roots (:roots options)))}))

(defn- failure [message] (extension/failure {:error {:message message}}))

(defn- not-in-draft
  [tool]
  (failure (str tool ": not in a draft; use draft_create(\"name\") first.")))

(defn- refusal
  "Preserve a lifecycle refusal's actionable message and structured recovery data."
  [^clojure.lang.ExceptionInfo e]
  (let [data
        (ex-data e)

        hint
        (:hint data)]

    (extension/failure
      {:error {:message (if (str/blank? (str hint)) (ex-message e) (str (ex-message e) " " hint))
               :details (wire/canonical (dissoc data :workspace))}})))

;; Sandbox symbols

(defn draft-status
  "Status of the current draft, including recovery for a draft-rooted trunk row."
  [env]
  (let [ws
        (current-workspace env)

        recovery
        (when-not (workspace/draft? ws) (workspace/draft-location (db-of env) (:root ws)))]

    (extension/success
      {:op :draft-status
       :result
       (wire/canonical
         (cond
           (workspace/draft? ws) (assoc (drafts/status ws) :in-draft true)
           recovery
           {:in-draft true
            :recovery-required true
            :root (:root ws)
            :label (:label recovery)
            :repo-root (:source-root recovery)
            :managed (boolean (:id recovery))
            :recovery-hint
            (if (:source-root recovery)
              "Use draft_discard() to return to the source checkout; the existing draft will be preserved."
              "Use /cd <original-checkout> to recover; ownership is unavailable and no draft files will be removed.")}
           :else {:in-draft false
                  :root (:root ws)
                  :backend-setting (name (workspace/draft-backend-setting))}))})))

(defn- diff-arguments
  "Normalize optional Python keywords without treating a checkpoint map as keywords."
  [filename since]
  (let [keyword-map?
        #(and (map? %) (some (set (keys %)) ["filename" :filename "since" :since]))

        options
        (into {}
              (map (fn [[k v]]
                     [(keyword k) v]))
              (mapcat seq (filter keyword-map? [filename since])))]

    {:filename (if (map? filename) (:filename options) filename)
     :since (if (keyword-map? since) (:since options) (or since (:since options)))}))

(defn- diff-checkpoints
  "Require a complete repository checkpoint map before reading or attaching any diff."
  [repositories since]
  (let [sources (set (map :repo-root repositories))]
    (cond
      (nil? since) {}
      (and (= 1 (count repositories)) (string? since)) {(first sources) since}
      (and (map? since) (= sources (set (keys since))) (every? string? (vals since))) since
      :else
      (throw
        (ex-info
          "Pass the complete checkpoint returned by this draft_diff call."
          {:type :draft/diff-invalid-checkpoint
           :hint
           "For several repositories, keep the entire checkpoint map; do not omit or replace members."})))))

(defn- diff-attachment
  "Render one repository's canonical artifact without recording a partial review."
  [filename repository {:keys [patch source checkpoint]}]
  (let [text
        (diff/render {"schema_version" 1 "patch" patch "source" source "comments" []})

        bytes
        (.getBytes ^String text java.nio.charset.StandardCharsets/UTF_8)]

    {:repo-root (:repo-root repository)
     :checkpoint checkpoint
     :empty (empty? patch)
     :attachment {:kind "diff"
                  :media-type diff/media-type
                  :filename filename
                  :audience "user"
                  :commentable true
                  :size (alength bytes)
                  :base64 (.encodeToString (java.util.Base64/getEncoder) bytes)}}))

(defn draft-diff
  "Attach every owned repository's immutable diff and return reusable checkpoints."
  [env & [filename since]]
  (let [ws
        (current-workspace env)

        {:keys [filename since]}
        (diff-arguments filename since)

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
          (try
            (let [repositories
                  (workspace/draft-roots ws)

                  multiple?
                  (> (count repositories) 1)

                  checkpoints
                  (diff-checkpoints repositories since)

                  ;; Validate every checkpoint and render every document before the first attachment.
                  prepared
                  (mapv (fn [index repository]
                          (let [review
                                (drafts/diff (boundary-env env)
                                             {:workspace-id (:id ws)
                                              :root (:repo-root repository)
                                              :since (get checkpoints (:repo-root repository))})

                                review
                                (cond-> review
                                  multiple?
                                  (assoc-in [:source "label"]
                                    (str (:label ws) " — " (:repo-root repository))))

                                name
                                (if multiple?
                                  (str (subs filename 0 (- (count filename) 5))
                                       "-repo-"
                                       (inc (long index))
                                       ".json")
                                  filename)]

                            (diff-attachment name repository review)))
                        (range)
                        repositories)

                  recorded
                  (mapv (fn [{:keys [attachment] :as item}]
                          (when-let [descriptor (capture/record-attachment! attachment)]
                            (merge (dissoc descriptor :base64) (dissoc item :attachment))))
                        prepared)]

              (if (every? some? recorded)
                (extension/success
                  {:op :draft-diff
                   :result (wire/canonical (if multiple?
                                             {:filename filename
                                              :attachments recorded
                                              :repository-count (count recorded)
                                              :checkpoint
                                              (into {} (map (juxt :repo-root :checkpoint)) recorded)
                                              :empty (every? :empty recorded)}
                                             (first recorded)))})
                (failure "The draft diff could not be attached. Retry the capture.")))
            (catch clojure.lang.ExceptionInfo e (refusal e))))))

(defn draft-create
  "Open one draft across the selected repositories and move the session into it."
  [env label & [clean roots]]
  (let [db
        (db-of env)

        state-id
        (state-id-of env)

        {:keys [label clean? roots]}
        (create-args label clean roots)

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
                                               :roots roots})]
                       (sync-confinement! env ws)
                       (extension/success {:op :draft-create
                                           :result (wire/canonical (assoc (drafts/status ws)
                                                                     :in-draft true
                                                                     :clean clean?))}))
                     (catch clojure.lang.ExceptionInfo e (refusal e))))))

(defn draft-sync
  "Synchronize selected owned copies, or continue/abort merges this draft started."
  [env & [action message roots]]
  (let [options
        (into {}
              (comp (filter map?)
                    cat
                    (map (fn [[k v]]
                           [(keyword k) v])))
              [action message roots])

        action
        (or (:action options) (when-not (map? action) action) "start")

        message
        (or (:message options) (when-not (map? message) message))

        roots
        (or (:roots options) (when-not (map? roots) roots))

        ws
        (current-workspace env)]

    (cond (not (workspace/draft? ws)) (not-in-draft "draft_sync()")
          (not (contains? #{"start" "continue" "abort" :start :continue :abort} action))
          (failure "Use draft_sync(action=\"start\"), \"continue\" or \"abort\".")
          :else
          (try (let [result (dissoc (drafts/sync! (boundary-env env)
                                                  {:workspace-id (:id ws)
                                                   :action (keyword action)
                                                   :message (message-arg message)
                                                   :roots roots})
                              :workspace)]
                 (if (= :partial (:status result))
                   (extension/failure
                     {:error
                      {:message
                       "Some repositories could not synchronize; inspect details before retrying."
                       :details (wire/canonical result)}})
                   (extension/success {:op :draft-sync :result (wire/canonical result)})))
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
  "Leave an owned draft and remove its working copy, or recover an inherited
   draft path without removing another workspace's files. Approved commits stay."
  [env]
  (let [db
        (db-of env)

        state-id
        (state-id-of env)

        ws
        (current-workspace env)

        recovery
        (when-not (workspace/draft? ws) (workspace/draft-location db (:root ws)))]

    (cond (or (nil? db) (nil? state-id)) (failure "Drafts need a persisted session.")
          (not (or (workspace/draft? ws) recovery)) (not-in-draft "draft_discard()")
          :else (try (let [{:keys [branch ahead]}
                           (when-not recovery (drafts/status ws))

                           [discarded trunk]
                           (drafts/discard! (boundary-env env)
                                            {:workspace-id (:id ws)
                                             :reason "discarded with draft_discard()"
                                             :session-state-id state-id})]

                       (extension/success {:op :draft-discard
                                           :result (wire/canonical (if recovery
                                                                     (assoc discarded
                                                                       :root (:root trunk)
                                                                       :label (:label recovery))
                                                                     {:status :discarded
                                                                      :label (:label ws)
                                                                      :root (:root trunk)
                                                                      :branch branch
                                                                      :approved-ahead (or ahead
                                                                                          0)}))}))
                     (catch clojure.lang.ExceptionInfo e (refusal e))))))

(def draft-status-symbol
  (ext/symbol
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
       "It reports every participating repository and task-only review counts. Outside a "
       "draft it reports the trunk root and the `draft_backend` setting. A draft-rooted session "
       "with missing ownership reports recovery_required and a safe recovery_hint instead.")
     :result
     (str
       "String-keyed `{in_draft, root, ...}`; in a draft also `{workspace_id, label, repo_root, "
       "backend, mechanism, branch, target_branch, ahead, pending, repositories, draft_changes}`. "
       "Recovery adds `{recovery_required, managed, recovery_hint}`; it never adopts another session's draft.")}))

(def draft-diff-symbol
  (ext/symbol
    #'draft-diff
    {:activity (presenter/for-tool :draft_diff)
     :inject-env? true
     :tag :observation
     :description
     (str
       "Attach every owned repository's exact changes as a separate reviewable diff. Defaults to "
       "changes since each seeded fork; copied pending work is excluded from this task-only review. "
       "Pass since=<checkpoint> from a previous result; for several repositories keep the complete map. "
       "All checkpoints are validated before any attachment is recorded. Neither working indexes nor commits change. "
       "Shared roots are excluded. Stable numbered filenames distinguish repositories, including identical relative paths. "
       "Works with worktree and Rift drafts; old drafts without a review baseline refuse.")
     :params [{:name "filename" :note "stable JSON filename; default DIFF-<draft-label>.json"}
              {:name "since" :note "snapshot checkpoint; omit for cumulative diff"}]
     :call {:opt-pos ["filename" "since"]}
     :result
     "Single repository: attachment descriptor with `{checkpoint, empty}` and a checkpoint string. Several: `{attachments, checkpoint, repository_count, empty}` with a checkpoint map keyed by source. Patch bytes stay in attachments."}))

(def draft-create-symbol
  (ext/symbol
    #'draft-create
    {:activity (presenter/for-tool :draft_create)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Open one draft across selected repositories and move the session into its first working copy. "
       "Original checkouts are left alone until approval. Drafts default to committed HEAD (clean=True); "
       "clean=False explicitly copies pending source work, which remains part of approval scope. "
       "Pass roots=[project_root_path, sibling_path] to select catalog repositories without changing configuration; omit roots for defaults. "
       "The nonempty list must contain distinct, nonoverlapping read/write Git repository roots. "
       "Read-only, copy-only, not-allowed and denied paths cannot be selected. "
       "All selections are validated before creation; failure preserves the original session. "
       "Approval addresses every participating repository; discard returns to the original project. "
       "Approval preserves unrelated local work. Discard the current draft before opening another. "
       "Extension hooks on `draft/create` may refuse.")
     :params [{:name "label" :note "draft name; also the `vis/<label>` branch"}
              {:name "clean" :note "False copies pending changes; default True"}
              {:name "roots" :note "read/write catalog repository Paths; primary first"}]
     :call {:pos ["label"] :opt-pos ["clean" "roots"]}
     :result
     (str
       "String-keyed `{in_draft: true, workspace_id, label, root, repo_root, backend, mechanism, "
       "branch, target_branch, ahead, pending, repositories, clean}`. Every selected Path alias points into its working copy, "
       "and `session[\"workspace\"]` / `project_root_path` follow from the next block on.")}))

(def draft-sync-symbol
  (ext/symbol
    #'draft-sync
    {:activity (presenter/for-tool :draft_sync)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Fetch and merge local/origin targets into owned draft repositories without changing originals or pushing. "
       "May checkpoint pending changes and commit merges: requires commit authorization and runs Git/extension hooks. "
       "Resolve reported conflict files with editing tools, then call action='continue' to stage resolutions and finish; "
       "unresolved conflict markers refuse. action='abort' cancels only merges this tool owns, retaining pre-sync checkpoints. "
       "Optional roots selects participating source or clone Paths, never arbitrary repositories.")
     :params [{:name "action" :note "start (default), continue, or abort"}
              {:name "message" :note "commit subject"}
              {:name "roots" :note "participating source or clone Paths"}]
     :call {:opt-pos ["action" "message" "roots"]}
     :result
     "String-keyed `{status, repositories}` with per-repository conflicts/commit/error. Conflicts require resolution, not approval."}))

(def draft-approve-symbol
  (ext/symbol
    #'draft-approve
    {:activity (presenter/for-tool :draft_approve)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Commit the draft and fast-forward the default branch (origin/HEAD, otherwise main or master). "
       "Fetch origin when configured; the draft must contain both local and origin target commits. "
       "If synchronization is required, use draft_sync(), resolve its conflicts and retry. "
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
       "`repositories` reports every participant. All targets are preflighted; late publication failures may leave some repositories landed and must be retried without discarding copies.")}))

(def draft-discard-symbol
  (ext/symbol
    #'draft-discard
    {:activity (presenter/for-tool :draft_discard)
     :inject-env? true
     :tag :mutation
     :description
     (str
       "Leave the session's current draft and remove its working copy; the session is back on the "
       "trunk at once. Approved work stays on the default branch; the merged draft branch may be "
       "removed. Call `draft_approve()` first to keep the work. Unapproved changes are lost. "
       "For an inherited draft path without ownership, return to the source checkout without deleting "
       "the existing draft. Unknown ownership requires /cd <original-checkout> instead. "
       "Extension hooks on `draft/discard` may refuse.")
     :result
     (str
       "String-keyed `{status: discarded, label, root, branch, approved_ahead}`. Recovery returns "
       "`{status: recovered, label, root, preserved_root}` without deleting files. "
       "`root` is the checkout the session works in again.")}))

(def symbols
  [draft-status-symbol draft-diff-symbol draft-create-symbol draft-sync-symbol draft-approve-symbol
   draft-discard-symbol])
