(ns com.blockether.vis.internal.workspace.drafts
  "Draft lifecycle above the workspace primitives: create, approve, discard and
   status. Each mutation crosses the extension op-hook boundary (`:draft/create`,
   `:draft/approve`, `:draft/discard`), so an extension can veto it with a
   `:before` guard or observe it with an `:after` hook.

   Approval commits the draft on `vis/<label>` and merges it into the local
   default branch: `origin/HEAD`, otherwise `main` or `master`. Divergence is
   merged inside the draft before fast-forwarding the target; target checkouts
   must be clean. No remote is pushed. Every new commit crosses the
   `:git/commit` boundary through `workspace.git/commit!`."
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
  "Discard `workspace-id` through the `:draft/discard` boundary — `workspace/abandon!`
   plus the hook round-trip. The caller has already moved the session off it."
  [env {:keys [workspace-id reason]}]
  (let [ws (require-draft (:db-info env) workspace-id)]
    (through-hooks :draft/discard
                   env
                   (hook-ctx ws {:reason reason})
                   #(dissoc (workspace/abandon! (:db-info env)
                                                {:workspace-id workspace-id :reason reason})
                      :discard-future))))

(defn- ensure-draft-branch!
  "Put the draft at `root` on a `vis/…` branch: a worktree already is; a Rift clone
   still sits on the trunk's branch and gets a fresh one, named free in `trunk`
   so the fetch below lands it without a clash."
  [^File root ^File trunk label]
  (or (draft-branch (current-branch root))
      (let [branch (free-branch-name trunk label)]
        (git! root ["switch" "-c" branch])
        branch)))

(defn- stage-all!
  "Stage every change in the draft except backend bookkeeping; returns the
   staged paths."
  [^File root]
  (doseq [operation
          ["MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD" "rebase-merge" "rebase-apply"]

          :when (.exists (io/file (git! root
                                        ["rev-parse" "--path-format=absolute" "--git-path"
                                         operation])))]

    (throw (ex-info "Finish or abort the draft's existing Git operation before approving."
                    {:type :draft/in-progress :operation operation})))
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

(defn- require-clean-target!
  [^File checkout target]
  (when-not (= target (current-branch checkout))
    (throw (ex-info "The target checkout changed branches; retry approval."
                    {:type :draft/target-moved :target-branch target})))
  (when-not (str/blank? (git! checkout ["status" "--porcelain" "--untracked-files=all"]))
    (throw (ex-info (str "Approval needs a clean "
                         target
                         " checkout at "
                         (.getPath checkout)
                         "; commit or stash its changes first. No changes were overwritten.")
                    {:type :draft/dirty-target :target-branch target :root (.getPath checkout)}))))

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

(defn- merge-target!
  "Merge target history in the draft, never in the user's checkout."
  [^File root target-sha message]
  (when-not (ancestor? root target-sha "HEAD")
    (try
      (git! root
            ["merge" "--no-ff" "--no-commit" "--no-autostash" "--no-overwrite-ignore" target-sha])
      (commit! root message)
      (catch Exception e
        ;; The draft was committed before this merge. Restore that commit on
        ;; conflicts or commit vetoes so the agent can resolve and retry safely.
        (when (= 0
                 (:exit
                   (git/run-git root ["rev-parse" "--verify" "--quiet" "MERGE_HEAD"] git-timeout)))
          (git! root ["merge" "--abort"]))
        (throw e)))))

(defn- land!
  [^File trunk target target-sha sha]
  (if-let [checkout (target-checkout trunk target)]
    (do (require-clean-target! checkout target)
        (git! checkout ["merge" "--ff-only" "--no-autostash" "--no-overwrite-ignore" sha]))
    ;; Compare-and-swap refuses a concurrent update; never reset another branch
    ;; or switch the user's checkout just to move an unchecked-out target.
    (git! trunk ["update-ref" (str "refs/heads/" target) sha target-sha])))

(defn approve!
  "Commit the draft and merge it into the local default branch (origin/HEAD,
   otherwise main or master). A diverged target is merged inside the draft,
   then the target is fast-forwarded. Dirty target checkouts refuse; conflicts
   leave the draft commit available for resolution and retry. No push, stash,
   force update or checkout switch. The draft stays active.

   Crosses :draft/approve and, for each new commit, :git/commit. Returns
   {:status :approved :branch … :target-branch … :commit … :files …}, or
   :nothing-to-approve only when the target already includes the draft and
   there are no pending changes. Existing draft commits can be retried without
   creating another commit. opts: :workspace-id and optional :message;
   Vis-Session/Vis-Draft trailers are appended to new commits."
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

        staged
        (stage-all! root)

        files
        (vec (distinct (concat (git-lines root ["diff" "--name-only" (str target-sha "...HEAD")])
                               staged)))]

    (if (and (empty? staged) (ancestor? root "HEAD" target-sha))
      {:status :nothing-to-approve :branch branch :target-branch target :files [] :workspace ws}
      (through-hooks
        :draft/approve
        env
        (hook-ctx ws {:branch branch :target-branch target :files files :message message})
        (fn []
          (when-let [checkout (target-checkout trunk target)]
            (require-clean-target! checkout target))
          (when (seq staged) (commit! root (commit-message (:label ws) message (:session-id env))))
          (merge-target! root
                         target-sha
                         (commit-message (:label ws)
                                         (str "merge: approve " (:label ws) " into " target)
                                         (:session-id env)))
          (let [sha (git! root ["rev-parse" "HEAD"])]
            (when-not (worktree? ws)
              (git! trunk
                    ["fetch" "--quiet" "--no-tags" (.getPath root)
                     (str "refs/heads/" branch ":refs/heads/" branch)]))
            (land! trunk target target-sha sha)
            (let [result {:status :approved
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
              (let [{:keys [exit out]} (git/run-git trunk
                                                    ["rev-list" "--count"
                                                     (str "refs/heads/" target ".." branch)]
                                                    git-timeout)]
                (when (= 0 exit) (parse-long (str/trim (str out))))))
     :pending
     (when git?
       (let [{:keys [exit out]}
             (git/run-git root ["status" "--porcelain" "--untracked-files=all"] git-timeout)]
         (when (= 0 exit) (count (remove str/blank? (str/split-lines (str out)))))))}))
