(ns com.blockether.vis.ext.foundation-git.merge-ops
  "Merge-resolve op family.

   Lives under the `git/` alias alongside the rest of the JGit
   observation surface (git_diff, git_status, git_log, ...):

     git_merge_status()                             read the current merge state
     git_merge_accept_ours(\"foo.txt\")               keep the trunk side of a conflict
     git_merge_accept_theirs(\"foo.txt\")             keep the branch side
     git_merge_mark_resolved(\"foo.txt\")             stage a manually-edited file
     git_merge_continue({\"message\": ...})           commit the merge (default msg 'merge-resolve')
     git_merge_abort()                              restore HEAD via `git reset --merge`

   Operations run against `(workspace/cwd)` via JGit (`Git/open`).
   The merge-resolve sub-session pins to the parent workspace whose
   `:root` is the conflicting tree (trunk is the merge target).

   All git access goes through JGit — no `sh/sh` shell-outs. Read-only
   `(mr/status)` is safe to call as an introspection probe regardless
   of merge state. Mutators throw structured ex-info when no merge is
   in progress (JGit raises its own checked exceptions on conflicting
   ref state)."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.ext.foundation-git.render :as render]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   [java.io File]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.api CheckoutCommand$Stage MergeCommand$FastForwardMode ResetCommand$ResetType]
   [org.eclipse.jgit.lib RepositoryState]))

;; =============================================================================
;; JGit handle
;; =============================================================================

(defn- open-git
  "Open the JGit repository discovered upward from `(workspace/cwd)`.
   Caller owns closing via `with-open` (Git is AutoCloseable)."
  ^Git []
  (Git/open ^File (workspace/cwd)))

(defn- merge-in-progress?*
  [^Git git]
  (let [state (.getRepositoryState (.getRepository git))]
    (boolean (#{RepositoryState/MERGING RepositoryState/MERGING_RESOLVED} state))))

(defn- conflict-paths*
  [^Git git]
  ;; `Status.getConflicting` returns a Set<String> of paths whose index
  ;; entries are in the conflict (UU/AA/DD/...) state.
  (set (.getConflicting (.call (.status git)))))

(defn- conflicts*
  [^Git git]
  (mapv (fn [path] {:path path :state "UU"}) (sort (conflict-paths* git))))

(defn- sha
  "Resolve `rev` to its sha string, or nil when the ref doesn't exist
   (e.g. MERGE_HEAD when not merging)."
  [^Git git rev]
  (some-> (.resolve (.getRepository git) ^String rev) .getName))

;; =============================================================================
;; Public Clojure helpers (each opens / closes one JGit handle)
;; =============================================================================

(defn status
  "Return a plain map describing the active merge.
   `:in-progress?` false when nothing to resolve."
  []
  (with-open [git (open-git)]
    (let [active? (merge-in-progress?* git)]
      (cond-> {:in-progress? active?}
        active? (assoc :head       (sha git "HEAD")
                  :merge-head (sha git "MERGE_HEAD")
                  :branch     (.getBranch (.getRepository git))
                  :conflicts  (conflicts* git))))))

(defn accept-ours
  "Resolve `path` by keeping the trunk side (HEAD) of the conflict.
   JGit `CheckoutCommand.setStage(OURS)` rewrites the working tree
   from the OURS stage of the index; `AddCommand` then advances the
   index so the merge progresses."
  [path]
  (with-open [git (open-git)]
    (.. git checkout (setStage CheckoutCommand$Stage/OURS) (addPath path) call)
    (.. git add (addFilepattern path) call))
  {:path path :op :git/merge-accept-ours})

(defn accept-theirs
  "Resolve `path` by keeping the branch side (MERGE_HEAD). JGit
   `CheckoutCommand.setStage(THEIRS)` + `AddCommand`."
  [path]
  (with-open [git (open-git)]
    (.. git checkout (setStage CheckoutCommand$Stage/THEIRS) (addPath path) call)
    (.. git add (addFilepattern path) call))
  {:path path :op :git/merge-accept-theirs})

(defn mark-resolved
  "Stage a path the model already edited by hand (e.g. via `patch`).
   Pure `AddCommand`; no checkout."
  [path]
  (with-open [git (open-git)]
    (.. git add (addFilepattern path) call))
  {:path path :op :git/merge-mark-resolved})

(defn continue!
  "Commit the merge with `:message` (default: 'merge-resolve').
   Refuses when conflicts are still outstanding. Publishes a
   `:session/merge-resolve-finished` event on success."
  [{:keys [message channel-id session-id]}]
  (with-open [git (open-git)]
    (let [outstanding (conflicts* git)]
      (when (seq outstanding)
        (throw (ex-info "merge has unresolved conflicts; resolve them before continue!"
                 {:type :merge-ops/unresolved-conflicts
                  :conflicts outstanding}))))
    (let [msg    (or (some-> message str str/trim not-empty) "merge-resolve")
          commit (.. git commit (setMessage msg) call)
          new-sha (.getName commit)]
      (when channel-id
        (try (vis/publish-channel-event! channel-id
               {:type :session/merge-resolve-finished
                :session-id session-id
                :result :continued
                :head new-sha
                :message msg})
          (catch Throwable _ nil)))
      {:result :continued :head new-sha :message msg})))

(defn abort!
  "Restore HEAD via JGit `ResetCommand` with `ResetType/HARD` — the
   programmatic equivalent of `git merge --abort` (which resets the
   worktree + index, then clears MERGE_HEAD / MERGE_MSG). `ResetType/MERGE`
   is fragile in JGit 7.x: it throws UnsupportedOperationException when
   applied outside specific conflict-state preconditions, so HARD is used.

   Reset target: `ORIG_HEAD` when present, else `HEAD`. The CLI `git merge`
   writes ORIG_HEAD at merge start, but JGit's `MergeCommand` does NOT — so
   a merge vis itself started (always via JGit) has no ORIG_HEAD. On an
   uncommitted (conflicted) merge HEAD is still the pre-merge commit, so a
   HARD reset to HEAD restores the worktree and `ResetCommand` clears
   MERGE_HEAD regardless of target. Publishes
   `:session/merge-resolve-finished` with `:result :aborted`."
  [{:keys [channel-id session-id]}]
  (with-open [git (open-git)]
    (let [ref (if (.resolve (.getRepository git) "ORIG_HEAD") "ORIG_HEAD" "HEAD")]
      (.. git reset
        (setMode ResetCommand$ResetType/HARD)
        (setRef ref)
        call)))
  (when channel-id
    (try (vis/publish-channel-event! channel-id
           {:type :session/merge-resolve-finished
            :session-id session-id
            :result :aborted})
      (catch Throwable _ nil)))
  {:result :aborted}) (defn- merge-hint
                        "Actionable next-step for a non-trivial merge `status` name, or nil."
                        [status-name]
                        (case status-name
                          "CONFLICTING" "Merge hit conflicts (see :conflicts). Resolve each with git_merge_accept_ours / git_merge_accept_theirs / patch then git_merge_mark_resolved, then git_merge_continue(). Bail with git_merge_abort()."
                          "FAILED" "Merge could not start (see :failing-paths) - usually dirty/uncommitted tracked files. Commit or stash them, then retry."
                          "CHECKOUT_CONFLICT" "Local changes would be overwritten by the merge checkout (see :conflicts). Commit or stash them, then retry."
                          "ABORTED" "Merge aborted before applying."
                          "MERGED_NOT_COMMITTED" "Merge staged but not committed (is_no_commit/is_squash). Review the index, then git_commit({...})."
                          "MERGED_SQUASHED_NOT_COMMITTED" "Squash merge staged but not committed. Review the index, then git_commit({...})."
                          nil)) (defn merge!
                                  "Merge a branch/ref INTO the current branch via JGit MergeCommand - the
   operation the resolve ops below could only FINISH, never START. `:branch`
   (or `:ref`) names what to merge in (branch name, tag, or sha). A clean
   merge commits or fast-forwards; conflicts leave the worktree MERGING so
   git_merge_status / git_merge_accept_* / git_merge_continue! drive the
   resolution - the same sub-session those ops already expect.

   opts: {:branch / :ref   what to merge in (required)
          :message         merge commit message (default: JGit's 'Merge ...')
          :is_no_ff        force a merge commit even when a fast-forward is possible
          :is_ff_only      refuse anything but a fast-forward
          :is_squash       squash merged history into the index (no commit)
          :is_no_commit    leave the merge staged but uncommitted}

   Returns {:op :git/merge :status <MergeStatus> :merged? bool :branch
            :head :merge-head :conflicts [{:path :state}] :failing-paths {}
            :hint}."
                                  [{:keys [branch ref message is_no_ff is_ff_only is_squash is_no_commit]}]
                                  (with-open [git (open-git)]
                                    (let [target (or (some-> (or branch ref) str str/trim not-empty)
                                                   (throw (ex-info "git_merge requires a branch/ref to merge in"
                                                            {:type :merge-ops/no-branch})))
                                          repo   (.getRepository git)
                                          obj    (or (.resolve repo target)
                                                   (throw (ex-info (str "git_merge could not resolve ref: " target)
                                                            {:type :merge-ops/bad-ref :ref target})))
                                          named  (.findRef repo target)
                                          cmd    (.merge git)]
                                      (if named (.include cmd named) (.include cmd obj))
                                      (when is_no_ff (.setFastForward cmd MergeCommand$FastForwardMode/NO_FF))
                                      (when is_ff_only (.setFastForward cmd MergeCommand$FastForwardMode/FF_ONLY))
                                      (when is_squash (.setSquash cmd true))
                                      (when is_no_commit (.setCommit cmd false))
                                      (when-let [m (some-> message str str/trim not-empty)] (.setMessage cmd m))
                                      (let [res    (.call cmd)
                                            status (.getMergeStatus res)
                                            sname  (.name status)
                                            head   (some-> (.getNewHead res) .getName)
                                            failing (.getFailingPaths res)]
                                        (cond-> {:op :git/merge
                                                 :status sname
                                                 :merged? (.isSuccessful status)
                                                 :branch (.getBranch repo)
                                                 :merge-head (.getName obj)}
                                          head (assoc :head head)
                                          (seq (conflicts* git)) (assoc :conflicts (conflicts* git))
                                          failing (assoc :failing-paths
                                                    (into {} (map (fn [[p r]] [p (.name r)])) failing))
                                          (merge-hint sname) (assoc :hint (merge-hint sname)))))))

;; =============================================================================
;; Python-facing tool wrappers (envelope contract)
;; =============================================================================

(defn- ok [v] (extension/success {:result v}))

;; Tool entry points — each `defn` carries its doc + arglists via
;; var metadata; `vis/symbol` reads both straight off the var.

(defn merge-status-tool
  "Read the active merge-resolve state: {:in-progress? :head :merge-head :branch :conflicts [...]}. Pure JGit read; safe to call regardless of merge state."
  []
  (ok (status)))

(defn merge-accept-ours-tool
  "Resolve a conflict by keeping the trunk side (HEAD). JGit CheckoutCommand(stage=OURS) + AddCommand."
  [path]
  (ok (accept-ours path)))

(defn merge-accept-theirs-tool
  "Resolve a conflict by keeping the branch side (MERGE_HEAD). JGit CheckoutCommand(stage=THEIRS) + AddCommand."
  [path]
  (ok (accept-theirs path)))

(defn merge-mark-resolved-tool
  "Stage a path the model already edited by hand (e.g. via patch). JGit AddCommand."
  [path]
  (ok (mark-resolved path)))

(defn merge-continue!-tool
  "Commit the merge via JGit CommitCommand. Opts: {:message :channel-id :session-id}."
  ([] (merge-continue!-tool {}))
  ([opts] (ok (continue! opts))))

(defn merge-abort!-tool
  "Restore HEAD via JGit ResetCommand to ORIG_HEAD (HARD) -- equivalent to `git merge --abort`."
  ([] (merge-abort!-tool {}))
  ([opts] (ok (abort! opts)))) (defn merge!-tool
                                 "Merge a branch/ref INTO the current branch (JGit MergeCommand). Opts: {:branch / :ref <what to merge in>, :message, :is_no_ff, :is_ff_only, :is_squash, :is_no_commit}. A clean merge commits or fast-forwards; conflicts leave the tree MERGING for git_merge_status / git_merge_accept_* / git_merge_continue!."
                                 [opts]
                                 (ok (merge! opts)))

(def merge-status-symbol
  (extension/symbol #'merge-status-tool
    {:symbol 'merge-status
     :tag :observation
     :render render/render-merge-status
     :color-role :tool-color/read}))

(def merge-accept-ours-symbol
  (extension/symbol #'merge-accept-ours-tool
    {:symbol 'merge-accept-ours
     :tag :mutation
     :render render/render-merge-accept-ours
     :color-role :tool-color/edit}))

(def merge-accept-theirs-symbol
  (extension/symbol #'merge-accept-theirs-tool
    {:symbol 'merge-accept-theirs
     :tag :mutation
     :render render/render-merge-accept-theirs
     :color-role :tool-color/edit}))

(def merge-mark-resolved-symbol
  (extension/symbol #'merge-mark-resolved-tool
    {:symbol 'merge-mark-resolved
     :tag :mutation
     :render render/render-merge-mark-resolved
     :color-role :tool-color/edit}))

(def merge-continue!-symbol
  (extension/symbol #'merge-continue!-tool
    {:symbol 'merge-continue!
     :tag :mutation
     :render render/render-merge-continue
     :color-role :tool-color/edit}))

(def merge-abort!-symbol
  (extension/symbol #'merge-abort!-tool
    {:symbol 'merge-abort!
     :tag :mutation
     :render render/render-merge-abort
     :color-role :tool-color/edit}))

(def merge!-symbol
  (extension/symbol #'merge!-tool
    {:symbol 'merge!
     :tag :mutation
     :render render/render-merge
     :color-role :tool-color/edit}))

(def merge-ops-symbols
  [merge-status-symbol merge-accept-ours-symbol merge-accept-theirs-symbol
   merge-mark-resolved-symbol merge-continue!-symbol merge-abort!-symbol
   merge!-symbol])
