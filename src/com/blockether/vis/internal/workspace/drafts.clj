(ns com.blockether.vis.internal.workspace.drafts
  "Draft lifecycle above the workspace primitives: create, approve, discard and
   status. Each mutation crosses the extension op-hook boundary (`:draft/create`,
   `:draft/approve`, `:draft/discard`), so an extension can veto it with a
   `:before` guard or observe it with an `:after` hook.

   Approval requires the draft to contain the local and fetched origin target.
   It fast-forwards locally, restores saved local work, then pushes to origin
   when configured. Conflicts must be resolved in the draft before approval.
   Every new commit crosses the `:git/commit` boundary."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.git :as git])
  (:import [java.io File]))

(def ^:private git-timeout {:timeout-secs 120})

(defn- git-error
  [^File dir args {:keys [out err]}]
  (ex-info (str "git " (str/join " " args)
                " failed in " (.getPath dir)
                ": " (str/trim (str (if (str/blank? (str err)) out err))))
           {:type :draft/git-failed
            :dir (.getPath dir)
            :args (mapv str args)
            :details (str/trim (str err))}))

(defn- git!
  "`git <args>` in `dir`, throwing `:draft/git-failed` unless it exits 0. Returns
   the trimmed stdout."
  ^String [^File dir args]
  (let [{:keys [exit out] :as result} (git/run-git dir args git-timeout)]
    (when-not (= 0 exit) (throw (git-error dir args result)))
    (str/trim (str out))))

(defn- git-lines [^File dir args] (into [] (remove str/blank?) (str/split-lines (git! dir args))))

(defn- current-branch
  "The branch `dir` has checked out, nil when detached."
  [^File dir]
  (let [{:keys [exit out]}
        (git/run-git dir ["symbolic-ref" "--short" "--quiet" "HEAD"] git-timeout)]
    (when (= 0 exit) (not-empty (str/trim (str out))))))

(defn- branch-taken?
  [^File repo branch]
  (= 0
     (:exit (git/run-git repo
                         ["rev-parse" "--verify" "--quiet" (str "refs/heads/" branch)]
                         git-timeout))))

(defn- free-branch-name
  "`vis/<label>`, numerically suffixed while `repo` already has that branch."
  [^File repo label]
  (let [base (str workspace/draft-branch-prefix label)]
    (loop [branch base
           i 2]

      (if (branch-taken? repo branch) (recur (str base "-" i) (inc i)) branch))))

(defn- draft-branch
  "The draft branch, if `branch` names one."
  [branch]
  (and branch (str/starts-with? branch workspace/draft-branch-prefix) branch))

(defn- worktree? [ws] (= :worktree (workspace/backend-id (:workspace-backend ws))))

(defn- require-draft
  [db-info workspace-id]
  (let [ws (workspace/get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:type :draft/unknown :workspace-id workspace-id})))
    (when-not (workspace/draft? ws)
      (throw (ex-info "Not a draft workspace"
                      {:type :draft/not-a-draft :workspace-id workspace-id})))
    (when-not (= :active (:state ws))
      (throw (ex-info (str "Draft is " (name (:state ws)))
                      {:type :draft/not-active :workspace-id workspace-id :state (:state ws)})))
    ws))

(defn- hook-ctx
  "Hook payload for `ws` — plain data, safe to stringify for Python."
  [ws extra]
  (merge {:workspace-id (:id ws)
          :label (:label ws)
          :root (:root ws)
          :repo-root (:repo-root ws)
          :backend (some-> (:workspace-backend ws)
                           workspace/backend-id
                           name)}
         extra))

(defn- through-hooks
  "Run `f` through the `op` extension boundary with `ctx` as the hook argument.
   A vetoed operation throws `:draft/blocked` with the extension's reason; the
   result of `f` otherwise flows back unchanged."
  [op env ctx f]
  (let [res (extension/invoke-operation op
                                        env
                                        (fn [_ctx]
                                          (extension/success {:result (f)}))
                                        [ctx])]
    (if (extension/envelope-success? res)
      (:result res)
      (throw (ex-info (or (some-> res
                                  :error
                                  :message
                                  not-empty)
                          (str "Refused: " (name op)))
                      {:type :draft/blocked :op op :ctx ctx :error (:error res)})))))

;; Lifecycle

(defn create!
  "Create a draft through the `:draft/create` boundary; `opts` are
   `workspace/create!`'s. `env` carries `:db-info` and `:session-id`."
  [env opts]
  (through-hooks :draft/create
                 env
                 {:label (:label opts)
                  :clean (boolean (:clean? opts))
                  :repo-root (some-> (or (:repo-root (:from opts)) (workspace/trunk-root))
                                     str)}
                 #(workspace/create! (:db-info env) opts)))

(defn discard!
  "Discard `workspace-id` through the `:draft/discard` boundary. With
   `:session-state-id`, the session is repointed to the trunk inside the
   boundary — a veto leaves it pinned to its draft — and `workspace/abandon!`
   then removes the draft. Returns `[abandon-result trunk]`."
  [env {:keys [workspace-id reason session-state-id]}]
  (let [ws (require-draft (:db-info env) workspace-id)]
    (through-hooks
      :draft/discard
      env
      (hook-ctx ws {:reason reason})
      (fn []
        (let [trunk (when session-state-id
                      (workspace/exit-to-trunk! (:db-info env) session-state-id (:repo-root ws)))]
          [(dissoc (workspace/abandon! (:db-info env) {:workspace-id workspace-id :reason reason})
             :discard-future) trunk])))))

(defn- ensure-draft-branch!
  "Keep the worktree branch; give a Rift clone its own vis/ branch."
  [^File root ^File trunk label]
  (or (draft-branch (current-branch root))
      (let [branch (free-branch-name trunk label)]
        (git! root ["switch" "-c" branch])
        branch)))

(defn- require-idle!
  [^File root]
  (doseq [operation
          ["MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD" "rebase-merge" "rebase-apply"]

          :when (.exists (io/file (git! root
                                        ["rev-parse" "--path-format=absolute" "--git-path"
                                         operation])))]

    (throw (ex-info "Finish or abort the checkout's existing Git operation before approving."
                    {:type :draft/in-progress :operation operation :dir (.getPath root)}))))

(defn- stage-all!
  "Stage draft changes except backend bookkeeping; return staged paths."
  [^File root]
  (require-idle! root)
  (git! root ["add" "-A" "--" "."])
  (git! root ["rm" "-r" "-q" "--cached" "--ignore-unmatch" "--" ".rift" ".trash"])
  (git-lines root ["diff" "--cached" "--name-only"]))

(defn- commit-message
  [label message session-id]
  (str (if (str/blank? (str message)) (str "draft(" label "): approve") (str/trim (str message)))
       "\n\n"
       (when-not (str/blank? (str session-id)) (str "Vis-Session: " session-id "\n"))
       "Vis-Draft: "
       label
       "\n"))

(defn- commit!
  [^File root message]
  (let [{:keys [exit] :as result} (git/commit! root ["commit" "--quiet" "-m" message] git-timeout)]
    (when-not (= 0 exit) (throw (git-error root ["commit"] result)))
    (git! root ["rev-parse" "HEAD"])))

(defn- target-branch
  "The local branch named by origin/HEAD, falling back to main or master."
  [^File trunk]
  (let [{:keys [exit out]}
        (git/run-git trunk ["symbolic-ref" "--quiet" "refs/remotes/origin/HEAD"] git-timeout)

        remote-ref
        (when (= 0 exit) (str/trim (str out)))

        target
        (or (when (and remote-ref (str/starts-with? remote-ref "refs/remotes/origin/"))
              (subs remote-ref (count "refs/remotes/origin/")))
            (some #(when (branch-taken? trunk %) %) ["main" "master"]))]

    (when-not (and target (branch-taken? trunk target) (not (draft-branch target)))
      (throw (ex-info "Approval needs a local default branch: origin/HEAD, main or master."
                      {:type :draft/no-target-branch :target-branch target})))
    target))

(defn- target-checkout
  "The checkout holding `target`, nil when the branch is not checked out."
  [^File trunk target]
  (let [separator (str (char 0))]
    (some (fn [entry]
            (let [fields (str/split entry (re-pattern separator))]
              (when (some #{(str "branch refs/heads/" target)} fields)
                (some #(when (str/starts-with? % "worktree ") (io/file (subs % 9))) fields))))
          (str/split (git! trunk ["worktree" "list" "--porcelain" "-z"])
                     (re-pattern (str separator separator))))))

(defn- require-target-branch!
  [^File checkout target]
  (when-not (= target (current-branch checkout))
    (throw (ex-info "The target checkout changed branches; retry approval."
                    {:type :draft/target-moved :target-branch target}))))

(defn- ancestor?
  [^File root ancestor descendant]
  (let [{:keys [exit] :as result}
        (git/run-git root ["merge-base" "--is-ancestor" ancestor descendant] git-timeout)]
    (case exit
      0
      true

      1
      false

      (throw (git-error root ["merge-base" "--is-ancestor" ancestor descendant] result)))))

(defn- require-synced!
  [^File root target sha]
  (when-not (ancestor? root sha "HEAD")
    (throw (ex-info "Synchronize the target into the draft and resolve conflicts before approval."
                    {:type :draft/sync-required
                     :target-branch target
                     :target-commit sha
                     :hint
                     (str "In the draft, merge or rebase onto " sha ", then retry approval.")}))))

(defn- fetch-origin!
  [^File trunk ^File root target]
  (when (some #{"origin"} (git-lines trunk ["remote"]))
    (git! trunk ["fetch" "--quiet" "--no-tags" "origin" (str "refs/heads/" target)])
    (let [sha (git! trunk ["rev-parse" "FETCH_HEAD"])]
      (when-not (= trunk root) (git! root ["fetch" "--quiet" "--no-tags" (.getPath trunk) sha]))
      sha)))

(defn- require-recovered!
  [^File trunk]
  (when (some #(str/includes? % "vis-approve-") (git-lines trunk ["stash" "list" "--format=%gs"]))
    (throw
      (ex-info
        "An approval stash still needs recovery; approval and push are blocked."
        {:type :draft/recovery-required
         :hint
         "Inspect git stash list, restore the saved vis-approve stash and drop it only after verifying your local work."}))))

(defn- publish!
  [^File trunk target sha origin?]
  (when origin?
    (try
      (git! trunk ["push" "--porcelain" "origin" (str sha ":refs/heads/" target)])
      (catch Exception e
        (throw
          (ex-info
            "Draft landed locally, but origin rejected publication."
            {:type :draft/push-failed
             :status :landed-locally
             :target-branch target
             :commit sha
             :hint
             "Local work has been restored. Fetch origin, synchronize the draft and retry approval; no force push was used."}
            e)))))
  (boolean origin?))

(defn- restore-stash!
  [^File checkout target stash]
  (try (require-target-branch! checkout target)
       (git! checkout ["stash" "apply" "--index" stash])
       (catch Exception e
         (throw (ex-info (str "Local work could not be restored; saved stash "
                              stash
                              " was retained. No push was attempted.")
                         {:type :draft/restore-failed
                          :stash stash
                          :hint (str "Resolve the checkout manually using saved stash "
                                     stash
                                     "; do not approve again until recovery is complete.")}
                         e))))
  ;; Another session can add a stash. Never drop its entry by assuming stash@{0}.
  (when-let [entry (some (fn [line]
                           (let [[sha ref] (str/split line #" " 2)]
                             (when (= sha stash) ref)))
                         (git-lines checkout ["stash" "list" "--format=%H %gd"]))]
    (git! checkout ["stash" "drop" entry])))

(defn- land!
  [^File trunk target target-sha sha]
  (if-let [checkout (target-checkout trunk target)]
    (do
      (require-target-branch! checkout target)
      (require-idle! checkout)
      (when-not (= target-sha (git! checkout ["rev-parse" "HEAD"]))
        (throw (ex-info "Target moved during approval; retry." {:type :draft/target-moved})))
      ;; Conservative preflight: never stash local paths touched by the landing.
      ;; This also covers copied dirty drafts and avoids post-landing stash conflicts.
      (let [changed (set (str/split (git! checkout
                                          ["diff" "--name-only" "--no-renames" "-z" target-sha sha])
                                    #"\u0000"))
            local (concat
                    (str/split (git! checkout ["diff" "--cached" "--name-only" "--no-renames" "-z"])
                               #"\u0000")
                    (str/split (git! checkout ["diff" "--name-only" "--no-renames" "-z"]) #"\u0000")
                    (str/split (git! checkout ["ls-files" "--others" "-z"]) #"\u0000"))]

        (when (some (fn [path]
                      (and (not (str/blank? path))
                           (some #(or (= path %)
                                      (str/starts-with? path (str % "/"))
                                      (str/starts-with? % (str path "/")))
                                 (remove str/blank? changed))))
                    local)
          (throw
            (ex-info
              "Local work overlaps draft paths; move or resolve it before approval. Nothing was stashed or landed."
              {:type :draft/git-failed :hint "Approval does not merge local working changes."}))))
      (let [dirty? (not (str/blank? (git! checkout
                                          ["status" "--porcelain" "--untracked-files=all"])))
            marker (str "vis-approve-" (random-uuid))
            stash
            (when dirty?
              (git! checkout ["stash" "push" "--include-untracked" "-m" marker])
              (or
                (some (fn [line]
                        (when (str/ends-with? line marker) (first (str/split line #" " 2))))
                      (git-lines checkout ["stash" "list" "--format=%H %gs"]))
                (throw
                  (ex-info
                    "Approval stash could not be identified; inspect the checkout before retrying."
                    {:type :draft/recovery-required}))))]

        (try (require-target-branch! checkout target)
             (git! checkout ["merge" "--ff-only" "--no-autostash" "--no-overwrite-ignore" sha])
             (finally (when stash (restore-stash! checkout target stash))))))
    (git! trunk ["update-ref" (str "refs/heads/" target) sha target-sha])))

(defn approve!
  "Commit the draft, require synchronized local/origin history, fast-forward the
   target, restore local changes with their index, then push without force when
   origin exists. Overlapping local paths are refused before stashing. Failed
   restore retains its stash and prevents push; failed push reports local landing.
   The draft stays active. Retry also publishes an already-landed commit.
   opts: :workspace-id and optional :message. Returns :approved or
   :nothing-to-approve with :published indicating whether origin was pushed."
  [env {:keys [workspace-id message]}]
  (let [ws
        (require-draft (:db-info env) workspace-id)

        root
        (io/file (:root ws))

        trunk
        (io/file (:repo-root ws))

        _
        (when-not (workspace/git-managed? trunk)
          (throw (ex-info "Approval needs a Git-managed project."
                          {:type :draft/not-git-managed :workspace-id workspace-id})))

        target
        (target-branch trunk)

        branch
        (ensure-draft-branch! root trunk (:label ws))

        target-sha
        (if (worktree? ws)
          (git! trunk ["rev-parse" (str "refs/heads/" target)])
          (do (git! root
                    ["fetch" "--quiet" "--no-tags" (.getPath trunk) (str "refs/heads/" target)])
              (git! root ["rev-parse" "FETCH_HEAD"])))

        _
        (do (require-recovered! trunk)
            (when-let [checkout (target-checkout trunk target)]
              (require-idle! checkout)))

        origin-sha
        (fetch-origin! trunk root target)

        staged
        (stage-all! root)

        files
        (vec (distinct (concat (git-lines root ["diff" "--name-only" (str target-sha "...HEAD")])
                               staged)))]

    (if (and (empty? staged) (= (git! root ["rev-parse" "HEAD"]) target-sha))
      (do (when origin-sha (require-synced! root target origin-sha))
          (through-hooks
            :draft/approve
            env
            (hook-ctx ws {:branch branch :target-branch target :files [] :message message})
            #(hash-map :status :nothing-to-approve
                       :branch branch
                       :target-branch target
                       :published (publish! trunk target target-sha origin-sha)
                       :files []
                       :workspace ws)))
      (through-hooks
        :draft/approve
        env
        (hook-ctx ws {:branch branch :target-branch target :files files :message message})
        (fn []
          (when-let [checkout (target-checkout trunk target)]
            (require-target-branch! checkout target))
          (when (seq staged) (commit! root (commit-message (:label ws) message (:session-id env))))
          (require-synced! root target target-sha)
          (when origin-sha (require-synced! root target origin-sha))
          (let [sha (git! root ["rev-parse" "HEAD"])]
            ;; Import objects only: a refused draft may have been rebased before retry.
            ;; Do not maintain a second local branch that would require a forced update.
            (when-not (worktree? ws)
              (git! trunk ["fetch" "--quiet" "--no-tags" (.getPath root) sha]))
            (land! trunk target target-sha sha)
            (let [result {:status :approved
                          :published (publish! trunk target sha origin-sha)
                          :branch branch
                          :target-branch target
                          :commit sha
                          :files files
                          :workspace ws}]
              (workspace/fire-hook! :on-approve ws (dissoc result :workspace))
              result)))))))

(defn status
  "Landing status: draft :branch, :target-branch, commits the target lacks
   (:ahead) and pending paths (:pending). Git facts are nil outside a repository."
  [ws]
  (let [root
        (io/file (:root ws))

        trunk
        (io/file (:repo-root ws))

        git?
        (and (.isDirectory root) (workspace/git-managed? trunk) (workspace/git-managed? root))

        branch
        (when git? (draft-branch (current-branch root)))

        target
        (when git? (try (target-branch trunk) (catch clojure.lang.ExceptionInfo _ nil)))]

    {:workspace-id (:id ws)
     :label (:label ws)
     :root (:root ws)
     :repo-root (:repo-root ws)
     :state (:state ws)
     :backend (some-> (:workspace-backend ws)
                      workspace/backend-id
                      name)
     :mechanism (some-> (:workspace-mechanism ws)
                        workspace/mechanism-id
                        name)
     :branch branch
     :target-branch target
     :ahead (when (and branch target)
              (let [{target-exit :exit target-out :out}
                    (git/run-git trunk ["rev-parse" (str "refs/heads/" target)] git-timeout)]
                (when (= 0 target-exit)
                  (let [{:keys [exit out]} (git/run-git root
                                                        ["rev-list" "--count"
                                                         (str (str/trim (str target-out)) "..HEAD")]
                                                        git-timeout)]
                    (when (= 0 exit) (parse-long (str/trim (str out))))))))
     :pending
     (when git?
       (let [{:keys [exit out]}
             (git/run-git root ["status" "--porcelain" "--untracked-files=all"] git-timeout)]
         (when (= 0 exit) (count (remove str/blank? (str/split-lines (str out)))))))}))
