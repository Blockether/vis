(ns com.blockether.vis.internal.workspace.drafts
  "Draft lifecycle above the workspace primitives: create, approve, discard and
   status. Each mutation crosses the extension op-hook boundary (`:draft/create`,
   `:draft/approve`, `:draft/discard`), so an extension can veto it with a
   `:before` guard or observe it with an `:after` hook.

   Approval requires the draft to contain the local and fetched origin target.
   It fast-forwards locally, restores saved local work, then pushes to origin
   when configured. Conflicts must be resolved in the draft before approval.
   Every new commit crosses the `:git/commit` boundary."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.paths :as paths]
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

(defn- select-roots
  [ws requested]
  (let [owned (workspace/draft-roots ws)]
    (if (nil? requested)
      owned
      (do (when-not (and (sequential? requested) (seq requested))
            (throw (ex-info "Choose a nonempty list of participating repository roots."
                            {:type :draft/invalid-roots})))
          (let [selected
                (mapv (fn [path]
                        (let [root (workspace/normalize-root path)]
                          (or (some #(when (or (= root (:root %)) (= root (:repo-root %))) %) owned)
                              (throw (ex-info "This repository does not belong to the draft."
                                              {:type :draft/root-unavailable :root root})))))
                      requested)]
            (when-not (= (count selected) (count (distinct (map :repo-root selected))))
              (throw (ex-info "Choose each participating repository only once."
                              {:type :draft/invalid-roots})))
            selected)))))

(defn diff
  "Read one owned repository's exact patch from its immutable fork or checkpoint."
  [env {:keys [workspace-id since root]}]
  (let [ws
        (require-draft (:db-info env) workspace-id)

        selected
        (if root (first (select-roots ws [root])) ws)]

    (workspace/review-diff selected since)))

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

(defn- through-group-hooks
  "Nest root-specific guards before the one group mutation."
  [op env contexts f]
  ((reduce (fn [next ctx]
             #(through-hooks op env ctx next))
           f
           (reverse contexts))))

;; Lifecycle

(defn- selected-root
  "Resolve an explicitly selected root without expanding the session's access."
  [env from requested]
  (let [base
        (or (:root from) (workspace/trunk-root))

        path
        (when-not (str/blank? (str requested)) (io/file (paths/expand-home (str requested))))

        root
        (when path
          (workspace/normalize-root (if (.isAbsolute ^File path) path (io/file base path))))

        writable
        (into #{(workspace/normalize-root base)}
              (keep workspace/normalize-root)
              (:security/filesystem-roots env))

        allowed
        (into writable
              (comp (keep workspace/normalize-root)
                    (filter (fn [path]
                              (let [^java.nio.file.Path candidate (.toPath (io/file path))]
                                (some #(.startsWith candidate (.toPath (io/file %))) writable)))))
              (vals (get-in env [:security-policy :project-paths])))]

    (when-not (and root (contains? allowed root) (.isDirectory (io/file root)))
      (throw
        (ex-info
          "Choose an existing read/write root from session[\"workspace\"][\"filesystem_roots\"]."
          {:type :draft/root-unavailable :root root})))
    (let [^java.nio.file.Path selected
          (.toPath (io/file root))

          policy
          (:security-policy env)

          restricted
          (concat (get-in policy [:process-jail :deny-read])
                  (get-in policy [:process-jail :deny-write])
                  (get-in policy [:process-jail :deny-exec])
                  (keep (fn [[path policy]]
                          (when (contains? #{:copy-only :not-allowed}
                                           (workspace/draft-policy-id policy))
                            path))
                        (:draft-policies policy)))]

      (when (some (fn [path]
                    (let [^java.nio.file.Path denied (.toPath (io/file (workspace/normalize-root
                                                                         path)))]
                      (or (.startsWith selected denied) (.startsWith denied selected))))
                  restricted)
        (throw
          (ex-info
            "This root overlaps a filesystem restriction or a copy-only/not-allowed draft policy."
            {:type :draft/root-denied :root root}))))
    root))

(defn create!
  "Validate every selected catalog repository before creating one owned draft group."
  [env opts]
  (when (contains? opts :root)
    (throw (ex-info "Use :roots with a list of catalog repositories, not the singular :root option."
                    {:type :draft/invalid-roots})))
  (let
    [requested
     (:roots opts)

     _
     (when (and (some? requested) (not (and (sequential? requested) (seq requested))))
       (throw (ex-info "Choose a nonempty list of repository roots." {:type :draft/invalid-roots})))

     roots
     (when requested (mapv #(selected-root env (:from opts) %) requested))

     _
     (doseq [root roots]
       (when-not (workspace/git-managed? root)
         (throw (ex-info "Selected roots must be Git repository roots."
                         {:type :draft/not-git-managed :root root}))))

     _
     (doseq [[i root]
             (map-indexed vector roots)

             other
             (drop (inc (long i)) roots)]

       (let [a
             (.toPath (io/file root))

             b
             (.toPath (io/file other))]

         (when (or (.startsWith a b) (.startsWith b a))
           (throw (ex-info "Selected repositories must be distinct and non-overlapping."
                           {:type :draft/overlapping-roots :roots [root other]})))))

     primary
     (first roots)

     source
     (or primary (:repo-root (:from opts)) (workspace/trunk-root))

     plan
     (concat (map #(hash-map :trunk % :policy :copy-and-apply) (rest roots))
             (or (:filesystem-roots opts) (workspace/draft-isolation-plan)))

     plan
     (reduce (fn [acc entry]
               (let [root (workspace/normalize-root (:trunk entry))]
                 (if (or (= root (workspace/normalize-root source)) (some #(= root (:trunk %)) acc))
                   acc
                   (conj acc (assoc entry :trunk root)))))
             []
             plan)

     approving
     (cons source
           (map :trunk (filter #(= :copy-and-apply (workspace/draft-policy-id (:policy %))) plan)))

     _
     (when (and (contains? env :workspace/draft-protected-roots)
                (not= :off (workspace/draft-backend-setting)))
       (doseq [root (cons source (map :trunk plan))]
         (let [path (.toPath (io/file (workspace/normalize-root root)))]
           (when-not (some #(.startsWith path (.toPath (io/file (workspace/normalize-root %))))
                           (:workspace/draft-protected-roots env))
             (throw
               (ex-info
                 "This repository was not protected when the Python context started. Start a new turn to rebuild the context before creating its draft; existing handles cannot be safely migrated."
                 {:type :draft/policy-expanded :root root}))))))

     identities
     (mapv #(workspace/normalize-root
              (git! (io/file %) ["rev-parse" "--path-format=absolute" "--git-common-dir"]))
           (filter workspace/git-managed? approving))

     _
     (when-not (= (count identities) (count (distinct identities)))
       (throw (ex-info "Draft roots include multiple worktrees of the same repository."
                       {:type :draft/duplicate-repository :roots (vec approving)})))

     opts
     (cond-> (assoc (dissoc opts :roots) :filesystem-roots (vec plan))
       primary
       (assoc :root primary)

       (:workspace/drafts-home env)
       (assoc :drafts-home (:workspace/drafts-home env)))]

    (through-group-hooks :draft/create
                         env
                         (mapv (fn [root]
                                 {:label (:label opts)
                                  :clean (boolean (:clean? opts))
                                  :repo-root (str root)
                                  :roots (vec (cons source (map :trunk plan)))})
                               (cons source (map :trunk plan)))
                         #(workspace/create! (:db-info env) opts))))

(defn discard!
  "Discard through `:draft/discard`, repointing persistence and live confinement
   before backend release. A trunk row incorrectly rooted in a managed draft is
   detached without deleting that draft. Unrecognized draft-store paths require
   an explicit `/cd` recovery. Returns `[discard-result trunk]`; hooks may veto."
  [env {:keys [workspace-id reason session-state-id]}]
  (let [db
        (:db-info env)

        current
        (workspace/get db workspace-id)

        recovery
        (when-not (workspace/draft? current) (workspace/draft-location db (:root current)))

        ws
        (if recovery current (require-draft db workspace-id))

        parent
        (when-let [id (:parent-workspace-id ws)]
          (workspace/get db id))

        return-root
        (if recovery
          (workspace/source-root db (:root ws))
          (or (when-not (workspace/draft? parent) (:root parent)) (:repo-root ws)))]

    (when (and recovery (nil? session-state-id))
      (throw (ex-info "Draft recovery needs a persisted session." {:type :draft/session-required})))
    (through-hooks :draft/discard
                   env
                   (hook-ctx ws
                             (cond-> {:reason reason}
                               recovery
                               (assoc :recovery true)))
                   (fn []
                     (let [trunk (when session-state-id
                                   (workspace/exit-to-trunk! db session-state-id return-root))]
                       (when (and trunk (:workspace-atom env)) (reset! (:workspace-atom env) trunk))
                       [(if recovery
                          {:status :recovered :preserved-root (:root ws)}
                          (dissoc (workspace/abandon! db
                                                      {:workspace-id workspace-id :reason reason})
                            :discard-future)) trunk])))))

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
    (throw
      (ex-info
        "Synchronize the target into the draft and resolve conflicts before approval."
        {:type :draft/sync-required
         :target-branch target
         :target-commit sha
         :hint
         "Call draft_sync(), resolve any reported conflicts in the draft, then retry draft_approve()."}))))

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

(defn- preflight-land!
  [^File trunk target target-sha sha]
  (when-not (= target-sha (git! trunk ["rev-parse" (str "refs/heads/" target)]))
    (throw (ex-info "Target moved during approval; retry." {:type :draft/target-moved})))
  (when-let [checkout (target-checkout trunk target)]
    (require-target-branch! checkout target)
    (require-idle! checkout)
    (let [changed (remove str/blank?
                    (str/split (git! checkout
                                     ["diff" "--name-only" "--no-renames" "-z" target-sha sha])
                               #"\u0000"))
          local (remove str/blank?
                  (mapcat #(str/split (git! checkout %) #"\u0000")
                          [["diff" "--cached" "--name-only" "--no-renames" "-z"]
                           ["diff" "--name-only" "--no-renames" "-z"]
                           ["ls-files" "--others" "-z"]]))
          overlaps (filterv (fn [path]
                              (some #(or (= path %)
                                         (str/starts-with? path (str % "/"))
                                         (str/starts-with? % (str path "/")))
                                    changed))
                     local)]

      (when (seq overlaps)
        (throw
          (ex-info
            "Local work overlaps draft paths. Nothing was stashed or landed."
            {:type :draft/git-failed
             :paths overlaps
             :repo-root (.getPath trunk)
             :hint
             "Preserve and commit the original work through your authorized Git workflow, then use draft_sync() and retry draft_approve(). Copied pending work is not transferred out of the original checkout."}))))))

(defn- land!
  [^File trunk target target-sha sha]
  (preflight-land! trunk target target-sha sha)
  (if-let [checkout (target-checkout trunk target)]
    (let [dirty? (not (str/blank? (git! checkout ["status" "--porcelain" "--untracked-files=all"])))
          marker (str "vis-approve-" (random-uuid))
          stash
          (when dirty?
            (git! checkout ["stash" "push" "--include-untracked" "-m" marker])
            (or (some (fn [line]
                        (when (str/ends-with? line marker) (first (str/split line #" " 2))))
                      (git-lines checkout ["stash" "list" "--format=%H %gs"]))
                (throw
                  (ex-info
                    "Approval stash could not be identified; inspect the checkout before retrying."
                    {:type :draft/recovery-required}))))]

      (try (require-target-branch! checkout target)
           (when-not (= target-sha (git! checkout ["rev-parse" "HEAD"]))
             (throw (ex-info "Target moved during approval; retry." {:type :draft/target-moved})))
           (git! checkout ["merge" "--ff-only" "--no-autostash" "--no-overwrite-ignore" sha])
           (finally (when stash (restore-stash! checkout target stash)))))
    (git! trunk ["update-ref" (str "refs/heads/" target) sha target-sha])))

(defn- target-state!
  [ws]
  (let [root
        (io/file (:root ws))

        trunk
        (io/file (:repo-root ws))]

    (when-not (and (workspace/git-managed? trunk) (workspace/git-managed? root))
      (throw (ex-info "Approval and synchronization need Git-managed repositories."
                      {:type :draft/not-git-managed :repo-root (:repo-root ws)})))
    (let [target
          (target-branch trunk)

          target-sha
          (if (worktree? ws)
            (git! trunk ["rev-parse" (str "refs/heads/" target)])
            (do (git! root
                      ["fetch" "--quiet" "--no-tags" (.getPath trunk) (str "refs/heads/" target)])
                (git! root ["rev-parse" "FETCH_HEAD"])))]

      {:root root
       :trunk trunk
       :target target
       :target-sha target-sha
       :origin-sha (fetch-origin! trunk root target)})))

(defn- sync-marker
  ^File [^File root]
  (io/file (git! root ["rev-parse" "--path-format=absolute" "--git-path" "vis-draft-sync.edn"])))

(defn- conflict-paths
  [^File root]
  (vec (remove str/blank?
         (str/split (git! root ["diff" "--name-only" "--diff-filter=U" "-z"]) #"\u0000"))))

(defn- sync-error
  [^Exception e]
  (merge {:message (ex-message e)} (select-keys (ex-data e) [:type :hint :paths :operation])))

(defn- owned-sync!
  [ws]
  (let [root
        (io/file (:root ws))

        marker
        (sync-marker root)]

    (when-not (.isFile marker)
      (throw (ex-info "No synchronization started by draft_sync is active in this repository."
                      {:type :draft/sync-not-active :repo-root (:repo-root ws)})))
    (let [state
          (edn/read-string (slurp marker :encoding "UTF-8"))

          head
          (git! root ["rev-parse" "HEAD"])

          merge-file
          (io/file (git! root ["rev-parse" "--path-format=absolute" "--git-path" "MERGE_HEAD"]))]

      (when-not (and (= (:id ws) (:workspace-id state))
                     (= head (:head state))
                     (or (not (.exists merge-file))
                         (= (:target state) (str/trim (slurp merge-file :encoding "UTF-8")))))
        (throw
          (ex-info
            "The checkout no longer matches this draft's synchronization; it was left untouched."
            {:type :draft/sync-ownership-changed})))
      (doseq [operation ["CHERRY_PICK_HEAD" "REVERT_HEAD" "rebase-merge" "rebase-apply"]]
        (when (.exists (io/file
                         (git! root ["rev-parse" "--path-format=absolute" "--git-path" operation])))
          (throw (ex-info "Finish the unrelated Git operation first."
                          {:type :draft/in-progress :operation operation}))))
      (assoc state :merging? (.exists merge-file)))))

(defn- sync-merges!
  [env ws targets message]
  (let [root
        (io/file (:root ws))

        marker
        (sync-marker root)]

    (loop [remaining (seq targets)]
      (if-let [sha (first remaining)]
        (if (ancestor? root sha "HEAD")
          (recur (next remaining))
          (let [state {:workspace-id (:id ws)
                       :head (git! root ["rev-parse" "HEAD"])
                       :target sha
                       :remaining (vec (next remaining))
                       :message message}
                _ (spit marker (pr-str state) :encoding "UTF-8")
                args ["merge" "--no-commit" "--no-ff" "--no-autostash" "--no-overwrite-ignore" sha]
                {:keys [exit] :as result} (git/run-git root args git-timeout)
                conflicts (conflict-paths root)]

            (cond
              (seq conflicts)
              (do
                (spit marker (pr-str (assoc state :conflicts conflicts)) :encoding "UTF-8")
                {:status :conflicts
                 :conflicts conflicts
                 :hint
                 "Resolve these files in the draft, then call draft_sync(action=\"continue\"); draft_sync(action=\"abort\") keeps the pre-sync checkpoint."})
              (not= 0 exit) (throw (git-error root args result))
              :else (do (commit! root
                                 (commit-message (:label ws)
                                                 (or message
                                                     "chore(drafts): synchronize target history")
                                                 (:session-id env)))
                        (java.nio.file.Files/deleteIfExists (.toPath marker))
                        (recur (next remaining))))))
        {:status :synced :conflicts [] :commit (git! root ["rev-parse" "HEAD"])}))))

(defn- sync-one!
  [env ws action message]
  (let [root
        (io/file (:root ws))

        marker
        (sync-marker root)]

    (case action
      :start
      (do
        (require-idle! root)
        (when (.exists marker)
          (throw (ex-info
                   "A previous draft synchronization needs continue or abort."
                   {:type :draft/sync-in-progress
                    :hint "Use draft_sync(action=\"continue\") or draft_sync(action=\"abort\")."})))
        (ensure-draft-branch! root (io/file (:repo-root ws)) (:label ws))
        (let [{:keys [target-sha origin-sha]} (target-state! ws)]
          (when (seq (stage-all! root))
            (commit! root
                     (commit-message (:label ws)
                                     (or message "chore(drafts): checkpoint before synchronization")
                                     (:session-id env))))
          (sync-merges! env ws (distinct (remove nil? [target-sha origin-sha])) message)))

      :continue
      (let [{:keys [merging? conflicts remaining] :as state} (owned-sync! ws)]
        (when-not merging?
          (throw (ex-info
                   "The merge did not start. Abort its draft marker, then retry synchronization."
                   {:type :draft/sync-not-merging})))
        (let [unresolved (filterv (fn [path]
                                    (let [file (io/file root path)]
                                      (and (.startsWith (.toPath (.getCanonicalFile file))
                                                        (.toPath (.getCanonicalFile root)))
                                           (.isFile file)
                                           (not (java.nio.file.Files/isSymbolicLink (.toPath file)))
                                           (re-find #"(?m)^(<<<<<<< |=======$|>>>>>>> )"
                                                    (slurp file :encoding "UTF-8")))))
                           (distinct (concat conflicts (conflict-paths root))))]
          (when (seq unresolved)
            (throw (ex-info "Resolve conflict markers before continuing synchronization."
                            {:type :draft/unresolved-conflicts :paths unresolved}))))
        (git! root ["add" "-A" "--" "."])
        (git! root ["rm" "-r" "-q" "--cached" "--ignore-unmatch" "--" ".rift" ".trash"])
        (when (seq (conflict-paths root))
          (throw (ex-info "Unresolved index entries remain." {:type :draft/unresolved-conflicts})))
        (commit! root
                 (commit-message
                   (:label ws)
                   (or message (:message state) "chore(drafts): resolve synchronization")
                   (:session-id env)))
        (java.nio.file.Files/deleteIfExists (.toPath marker))
        (sync-merges! env ws remaining message))

      :abort
      (let [{:keys [merging?]} (owned-sync! ws)]
        (when merging? (git! root ["merge" "--abort"]))
        (java.nio.file.Files/deleteIfExists (.toPath marker))
        {:status :aborted :conflicts [] :commit (git! root ["rev-parse" "HEAD"])}))))

(defn sync!
  "Checkpoint and merge target history in owned drafts only; continue/abort owns its Git operation."
  [env {:keys [workspace-id action roots message] :or {action :start}}]
  (when-not (contains? #{:start :continue :abort} action)
    (throw (ex-info "Use start, continue or abort for draft synchronization."
                    {:type :draft/invalid-action})))
  (let [ws
        (require-draft (:db-info env) workspace-id)

        selected
        (let [selected (select-roots ws roots)]
          (when (and roots (some #(= :copy-only (:policy %)) selected))
            (throw (ex-info "Copied dependency roots are review-only, not synchronization targets."
                            {:type :draft/root-not-approvable})))
          (filterv #(not= :copy-only (:policy %)) selected))

        selected
        (let [active (when (and (nil? roots) (not= :start action))
                       (filterv #(.exists (sync-marker (io/file (:root %)))) selected))]
          (if (seq active) active selected))]

    (through-group-hooks
      :draft/sync
      env
      (mapv #(hook-ctx % {:action action :roots (mapv :repo-root selected)}) selected)
      (fn []
        (let [results (mapv (fn [repo]
                              (merge (select-keys repo [:root :repo-root])
                                     (try (sync-one! env repo action message)
                                          (catch Exception e
                                            {:status :failed :error (sync-error e)}))))
                            selected)]
          {:status (cond (some #(= :failed (:status %)) results) :partial
                         (some #(= :conflicts (:status %)) results) :conflicts
                         (= :abort action) :aborted
                         :else :synced)
           :repositories results
           :workspace ws})))))

(defn- prepare-approval!
  [ws]
  (let [root
        (io/file (:root ws))

        trunk
        (io/file (:repo-root ws))]

    (require-idle! root)
    (require-recovered! trunk)
    (when-let [checkout (target-checkout trunk (target-branch trunk))]
      (require-idle! checkout))
    (let [state
          (target-state! ws)

          branch
          (ensure-draft-branch! root trunk (:label ws))

          staged
          (stage-all! root)

          files
          (vec (distinct
                 (concat (git-lines root ["diff" "--name-only" (str (:target-sha state) "...HEAD")])
                         staged)))]

      (assoc state
        :ws ws
        :branch branch
        :staged staged
        :files files))))

(defn- approval-result
  [{:keys [ws branch target sha files]} status]
  {:root (:root ws)
   :repo-root (:repo-root ws)
   :branch branch
   :target-branch target
   :commit sha
   :files files
   :status status})

(defn- through-approval-hooks
  [env plans message f]
  (through-group-hooks :draft/approve
                       env
                       (mapv (fn [plan]
                               (hook-ctx (:ws plan)
                                         {:branch (:branch plan)
                                          :target-branch (:target plan)
                                          :files (:files plan)
                                          :message message
                                          :repositories (mapv #(approval-result % :prepared)
                                                              plans)}))
                             plans)
                       f))

(defn approve!
  "Preflight every approving repository before landing any; late failures retain truthful per-root results."
  [env {:keys [workspace-id message]}]
  (let [ws
        (require-draft (:db-info env) workspace-id)

        repos
        (remove #(= :copy-only (:policy %)) (workspace/draft-roots ws))

        plans
        (mapv prepare-approval! repos)]

    (through-approval-hooks
      env
      plans
      message
      (fn []
        (let [prepared
              (mapv (fn [{:keys [root trunk staged ws target target-sha origin-sha] :as plan}]
                      (when (seq staged)
                        (commit! root (commit-message (:label ws) message (:session-id env))))
                      (require-synced! root target target-sha)
                      (when origin-sha (require-synced! root target origin-sha))
                      (let [sha (git! root ["rev-parse" "HEAD"])]
                        (when-not (worktree? ws)
                          (git! trunk ["fetch" "--quiet" "--no-tags" (.getPath ^File root) sha]))
                        (assoc plan :sha sha)))
                    plans)]
          ;; All clones may acquire commits, but no original target moves before ALL pass.
          (doseq [{:keys [trunk target target-sha sha]} prepared]
            (preflight-land! trunk target target-sha sha))
          (loop [remaining (seq prepared)
                 results []]

            (if-let [{:keys [trunk target target-sha sha origin-sha] :as plan} (first remaining)]
              (let [result (try (when-not (= target-sha sha) (land! trunk target target-sha sha))
                                (assoc (approval-result
                                         plan
                                         (if (= target-sha sha) :nothing-to-approve :approved))
                                  :published (publish! trunk target sha origin-sha))
                                (catch Exception e
                                  (throw (ex-info
                                           (ex-message e)
                                           (assoc (or (ex-data e) {:type :draft/git-failed})
                                             :repositories
                                             (into results
                                                   (cons (merge (approval-result plan :failed)
                                                                (select-keys (ex-data e) [:status])
                                                                {:error (sync-error e)})
                                                         (map #(approval-result % :not-attempted)
                                                              (next remaining)))))
                                           e))))]
                (recur (next remaining) (conj results result)))
              (let [result (assoc (first results)
                             :status (if (some #(= :approved (:status %)) results)
                                       :approved
                                       :nothing-to-approve)
                             :published (every? :published results)
                             :repositories results
                             :workspace ws)]
                (when (= :approved (:status result))
                  (workspace/fire-hook! :on-approve ws (dissoc result :workspace)))
                result))))))))

(defn- repository-status
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
        (when git? (try (target-branch trunk) (catch clojure.lang.ExceptionInfo _ nil)))

        summary
        (try {:draft-changes (workspace/review-summary ws)}
             (catch Exception e {:draft-error (ex-message e)}))]

    (merge {:workspace-id (:id ws)
            :label (:label ws)
            :root (:root ws)
            :repo-root (:repo-root ws)
            :state (:state ws)
            :policy (:policy ws)
            :primary? (:primary? ws)
            :approval? (not= :copy-only (:policy ws))
            :backend (some-> (:workspace-backend ws)
                             workspace/backend-id
                             name)
            :mechanism (some-> (:workspace-mechanism ws)
                               workspace/mechanism-id
                               name)
            :branch branch
            :target-branch target
            :ahead (when (and branch target)
                     (let [{:keys [exit out]}
                           (git/run-git trunk ["rev-parse" (str "refs/heads/" target)] git-timeout)]
                       (when (= 0 exit)
                         (let [{:keys [exit out]} (git/run-git root
                                                               ["rev-list" "--count"
                                                                (str (str/trim (str out)) "..HEAD")]
                                                               git-timeout)]
                           (when (= 0 exit) (parse-long (str/trim (str out))))))))
            :pending
            (when git?
              (let [{:keys [exit out]}
                    (git/run-git root ["status" "--porcelain" "--untracked-files=all"] git-timeout)]
                (when (= 0 exit) (count (remove str/blank? (str/split-lines (str out)))))))}
           summary)))

(defn status
  "Per-repository landing facts and aggregate task-diff counts; unavailable values remain nil."
  [ws]
  (let [repositories
        (mapv repository-status (workspace/draft-roots ws))

        summary
        (when (every? :draft-changes repositories)
          (apply merge-with
            +
            {:modified 0 :created 0 :deleted 0}
            (map :draft-changes repositories)))

        approving
        (filterv :approval? repositories)

        total
        (fn [key]
          (when (every? #(number? (key %)) approving) (reduce + (map key approving))))]

    (cond-> (assoc (first repositories)
              :repositories repositories
              :pending (total :pending)
              :ahead (total :ahead)
              :draft-changes summary)
      (some :draft-error repositories)
      (assoc :draft-error "Draft change summary unavailable for one or more repositories."))))
