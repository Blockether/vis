(ns com.blockether.vis.internal.workspace.drafts-test
  "Draft lifecycle and op hooks, with real Git repositories and an in-memory
   store. Approval commits and merges into the default branch; discard removes
   the working copy without losing approved work. The drafts home is rebound
   so ~/.vis is never touched."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.workspace.core :as ws]
            [com.blockether.vis.internal.workspace.drafts :as drafts]
            [com.blockether.vis.internal.workspace.git :as workspace-git]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (.delete ^java.io.File f)))

(defn- git!
  "Run git in `root`; the trimmed output, or a throw naming the failure."
  [root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb (io/file root))
    (.redirectErrorStream pb true)
    (let [p (.start pb)
          out (slurp (.getInputStream p))
          exit (.waitFor p)]

      (when-not (zero? exit) (throw (ex-info "git failed" {:args args :out out})))
      (.trim ^String out))))

(defn- init-repo!
  "A real repository with one commit and a pending tracked edit plus an
   untracked file, so a draft has something to carry and to approve."
  [root]
  (git! root "init" "-q" "-b" "main")
  (git! root "config" "user.name" "Vis Test")
  (git! root "config" "user.email" "vis-test@example.invalid")
  (git! root "config" "commit.gpgsign" "false")
  (spit (io/file root "a.txt") "x\n")
  (git! root "add" "a.txt")
  (git! root "commit" "-q" "-m" "init")
  (spit (io/file root "a.txt") "x\npending\n")
  (spit (io/file root "new.txt") "untracked\n"))

(defn- with-repo
  "Run `(f store base env)` against a fresh repo, an in-memory store and a
   throwaway drafts home; everything is deleted afterwards."
  [prefix f]
  (let [base
        (temp-dir prefix)

        store
        (assoc (ps/db-open! :memory) :backend :sqlite)]

    (try (init-repo! base)
         (binding [ws/*drafts-home* (str base "-store")]
           (f store base {:db-info store :session-id "sess-drafts"}))
         (finally (ps/db-close! store) (delete-tree! (str base "-store")) (delete-tree! base)))))

(defn- seed-trunk!
  [store base]
  (ps/db-workspace-insert!
    store
    {:id (str (random-uuid)) :repo-id "rt" :repo-root base :root base :state :active :fork-ms 1}))

(defn- wait-until
  "True once `pred` holds, polling for at most five seconds; clone removal
   after a discard runs in the background."
  [pred]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (loop []

      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 50) (recur))))))

(defn- rift-available?
  [base]
  (boolean (some #(and (= :rift (:backend %)) (:available? %))
                 (ws/workspace-capability-matrix base))))

(defn- approve-roundtrip!
  "Create, approve twice and discard one draft; the observations each step
   makes, for either backend."
  [store base env]
  (let [seed
        (seed-trunk! store base)

        draft
        (drafts/create! env {:session-state-id (str (random-uuid)) :label "feature x" :from seed})

        droot
        (:root draft)

        worktree-listed?
        (str/includes? (git! base "worktree" "list") droot)

        carried?
        (and (= "x\npending\n" (slurp (io/file droot "a.txt"))) (.exists (io/file droot "new.txt")))

        _
        (do (spit (io/file droot "b.txt") "added in the draft\n")
            ;; The copied pending work now belongs to the draft. Clear only the
            ;; fixture's duplicate files so the landing checkout is clean.
            (spit (io/file base "a.txt") "x\n")
            (.delete (io/file base "new.txt")))

        first-pass
        (drafts/approve! env {:workspace-id (:id draft) :message "feat: land b"})

        second-pass
        (drafts/approve! env {:workspace-id (:id draft)})

        branch
        (:branch first-pass)

        subject
        (git! base "log" "-1" "--format=%s" (:commit first-pass))

        body
        (git! base "log" "-1" "--format=%b" (:commit first-pass))

        landed
        (str/split-lines (git! base "show" "--name-only" "--format=" (:commit first-pass)))

        trunk-head-subject
        (git! base "log" "-1" "--format=%s" "HEAD")

        status
        (drafts/status draft)

        _
        (drafts/discard! env {:workspace-id (:id draft) :reason "test"})

        removed?
        (wait-until #(not (.exists (io/file droot))))]

    {:draft draft
     :worktree-listed? worktree-listed?
     :carried? carried?
     :first first-pass
     :second second-pass
     :branch branch
     :subject subject
     :body body
     :landed (set landed)
     :trunk-head-subject trunk-head-subject
     :trunk-pending (slurp (io/file base "a.txt"))
     :status status
     :discarded-state (:state (ws/get store (:id draft)))
     :branch-after-discard (git! base "branch" "--list" branch)
     :root-after-discard (not removed?)}))

(defn- expect-roundtrip!
  [{:keys [draft carried? first second branch subject body landed trunk-head-subject trunk-pending
           status discarded-state branch-after-discard root-after-discard]}]
  (expect (= "feature-x" (:label draft)))
  (expect carried?)
  (expect (= :approved (:status first)))
  (expect (= "vis/feature-x" branch))
  (expect (= #{"a.txt" "new.txt" "b.txt"} (set (:files first))))
  (expect (= "feat: land b" subject))
  (expect (str/includes? body "Vis-Session: sess-drafts"))
  (expect (str/includes? body "Vis-Draft: feature-x"))
  (expect (= #{"a.txt" "new.txt" "b.txt"} landed))
  (expect (= :nothing-to-approve (:status second)))
  ;; Approval updates the default branch and its checkout, not just vis/<label>.
  (expect (= "main" (:target-branch first)))
  (expect (= "feat: land b" trunk-head-subject))
  (expect (= "x\npending\n" trunk-pending))
  (expect (= 0 (:ahead status)))
  (expect (= 0 (:pending status)))
  (expect (= :discarded discarded-state))
  ;; The merged draft branch may be cleaned up; the approved work stays on main.
  (expect (or (str/blank? branch-after-discard)
              (str/includes? branch-after-discard "vis/feature-x")))
  (expect (false? root-after-discard)))

(defdescribe
  approve-roundtrip-test
  (it "worktree approval merges carried changes into main and discard keeps the approved work"
      (with-repo "vis-drafts-wt"
                 (fn [store base env]
                   (binding [ws/*draft-backend* :worktree]
                     (let [out (approve-roundtrip! store base env)]
                       (expect (= :worktree (:workspace-backend (:draft out))))
                       (expect (:worktree-listed? out))
                       (expect-roundtrip! out))))))
  (it "rift approval fetches and merges the clone commit into the default branch"
      (with-repo "vis-drafts-rift"
                 (fn [store base env]
                   ;; Rift needs its native library; a host without it cannot exercise this backend.
                   (if (rift-available? base)
                     (binding [ws/*draft-backend* :rift]
                       (let [out (approve-roundtrip! store base env)]
                         (expect (= :rift (:workspace-backend (:draft out))))
                         (expect-roundtrip! out)))
                     (expect (not (rift-available? base)))))))
  (it "auto prefers the worktree backend inside a committed git repository"
      (with-repo "vis-drafts-auto"
                 (fn [_store base _env]
                   (binding [ws/*draft-backend* :auto]
                     (expect (= :worktree (ws/draft-backend-for base)))
                     (expect (ws/isolated-workspaces-supported? base))))))
  (it "clean drafts seed from HEAD and leave pending trunk work behind"
      (with-repo "vis-drafts-clean"
                 (fn [store base env]
                   (binding [ws/*draft-backend* :worktree]
                     (let [draft (drafts/create! env
                                                 {:session-state-id (str (random-uuid))
                                                  :label "clean"
                                                  :from (seed-trunk! store base)
                                                  :clean? true})]
                       (expect (= "x\n" (slurp (io/file (:root draft) "a.txt"))))
                       (expect (false? (.exists (io/file (:root draft) "new.txt"))))
                       (expect (= :nothing-to-approve
                                  (:status (drafts/approve! env {:workspace-id (:id draft)}))))
                       ;; what the checkout left out is not an agent deletion: apply! keeps
                       ;; trunk's untracked file and its pending edit
                       (spit (io/file (:root draft) "c.txt") "made\n")
                       (let [{:keys [changed]} (ws/apply! store {:workspace-id (:id draft)})]
                         (expect (= [["c.txt" :add]] (mapv (juxt :path :status) changed)))
                         (expect (= "untracked\n" (slurp (io/file base "new.txt"))))
                         (expect (= "x\npending\n" (slurp (io/file base "a.txt"))))))))))
  ;; Regression: a worktree never receives what trunk ignores, and reading that
  ;; absence as a deletion would erase the user's local files on apply.
  (it "apply! never deletes trunk's ignored files, which no worktree draft receives"
      (with-repo "vis-drafts-ignored"
                 (fn [store base env]
                   (spit (io/file base ".gitignore") "secret.env\n")
                   (spit (io/file base "secret.env") "TOKEN=1\n")
                   (git! base "add" ".gitignore")
                   (git! base "commit" "-q" "-m" "ignore")
                   (binding [ws/*draft-backend* :worktree]
                     (let [draft (drafts/create! env
                                                 {:session-state-id (str (random-uuid))
                                                  :label "ignored"
                                                  :from (seed-trunk! store base)})]
                       (expect (false? (.exists (io/file (:root draft) "secret.env"))))
                       (expect (empty? (ws/changed-paths (:root draft) (:fork-ms draft)))
                               "copied pending work is older than the apply baseline")
                       (let [file (io/file (:root draft) "new.txt")]
                         (spit file "edited in the draft\n")
                         ;; Deterministic regression for writes within the fork's millisecond.
                         (expect (.setLastModified file
                                                   (long (or (:apply-fork-ms draft)
                                                             (:fork-ms draft))))))
                       (.delete (io/file (:root draft) "a.txt"))
                       (let [{:keys [changed]} (ws/apply! store {:workspace-id (:id draft)})]
                         (expect (= {"new.txt" :modify "a.txt" :delete}
                                    (into {} (map (juxt :path :status)) changed)))
                         (expect (= "edited in the draft\n" (slurp (io/file base "new.txt"))))
                         (expect (= "TOKEN=1\n" (slurp (io/file base "secret.env"))))
                         (expect (false? (.exists (io/file base "a.txt")))))))))))

(defdescribe
  draft-op-hooks-test
  (it
    "an around hook on :draft/approve can veto the landing and an after hook observes it"
    (with-repo
      "vis-drafts-hooks"
      (fn [store base env]
        (let [seen (atom [])]
          (try (extension/register-op-hook! {:op :draft/approve
                                             :phase :around
                                             :owner :ext/drafts-test
                                             :fn (fn [_env _op args next]
                                                   (if (= "veto me" (:message (first args)))
                                                     (extension/failure {:error {:message
                                                                                 "policy says no"}})
                                                     (next args)))})
               (extension/register-op-hook! {:op :draft/approve
                                             :phase :after
                                             :owner :ext/drafts-test
                                             :fn (fn [_env op _args result]
                                                   (swap! seen conj [op (:status (:result result))])
                                                   result)})
               (binding [ws/*draft-backend* :worktree]
                 (let [draft (drafts/create! env
                                             {:session-state-id (str (random-uuid))
                                              :label "guarded"
                                              :from (seed-trunk! store base)})
                       _ (do (spit (io/file base "a.txt") "x\n") (.delete (io/file base "new.txt")))
                       vetoed (try (drafts/approve! env
                                                    {:workspace-id (:id draft) :message "veto me"})
                                   nil
                                   (catch clojure.lang.ExceptionInfo e
                                     (assoc (ex-data e) :message (ex-message e))))
                       allowed (drafts/approve! env {:workspace-id (:id draft) :message "fine"})]

                   (expect (= :draft/blocked (:type vetoed)))
                   (expect (= "policy says no" (:message vetoed)))
                   ;; nothing landed while vetoed: the first real commit is the allowed one
                   (expect (= :approved (:status allowed)))
                   (expect (= "fine" (git! base "log" "-1" "--format=%s" (:branch allowed))))
                   (expect (= 0
                              (parse-long
                                (git! base "rev-list" "--count" (str "HEAD.." (:branch allowed))))))
                   ;; after hooks observe every outcome, the refused one included
                   (expect (= [[:draft/approve nil] [:draft/approve :approved]] @seen))))
               (finally (extension/unregister-op-hooks-for-owner! :ext/drafts-test))))))))

(defdescribe draft-backend-off-test
             (it "the off setting refuses to create drafts and says which toggle to flip"
                 (with-repo
                   "vis-drafts-off"
                   (fn [store base env]
                     (binding [ws/*draft-backend* :off]
                       (expect (nil? (ws/draft-backend-for base)))
                       (expect (false? (ws/isolated-workspaces-supported? base)))
                       (expect (str/includes? (ws/isolation-unavailable-hint base) "draft_backend"))
                       (let [thrown (try (drafts/create! env
                                                         {:session-state-id (str (random-uuid))
                                                          :label "nope"
                                                          :from (seed-trunk! store base)})
                                         nil
                                         (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                         (expect (= :workspace/drafts-disabled (:type thrown)))))))))

(defn- with-clean-draft
  "Run f with a clean target checkout and a worktree draft."
  [f]
  (with-repo "vis-drafts-target"
             (fn [store base env]
               (git! base "add" "-A")
               (git! base "commit" "-q" "-m" "pending work")
               (binding [ws/*draft-backend* :worktree]
                 (let [draft (drafts/create! env
                                             {:session-state-id (str (random-uuid))
                                              :label "land"
                                              :from (seed-trunk! store base)})]
                   (f base env draft))))))

(defn- approval-error
  [env draft]
  (try (drafts/approve! env {:workspace-id (:id draft)})
       nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe
  approve-target-test
  (it "approve commits and merges into main or master without a separate manual merge"
      (doseq [target ["main" "master"]]
        (with-clean-draft
          (fn [base env draft]
            (git! base "branch" "-m" target)
            (spit (io/file (:root draft) "b.txt") "approved\n")
            (let [result (drafts/approve! env
                                          {:workspace-id (:id draft) :message "feat: approve b"})]
              (expect (= :approved (:status result)))
              (expect (= (:commit result) (git! base "rev-parse" target)))
              (expect (= target (:target-branch result)))
              (expect (= "feat: approve b" (git! base "log" "-1" "--format=%s" target)))
              (expect (= "approved\n" (slurp (io/file base "b.txt")))))))))
  (it "origin/HEAD wins over main, including a custom default that is not checked out"
      (doseq [target ["master" "trunk"]]
        (with-clean-draft
          (fn [base env draft]
            (let [head (git! base "rev-parse" "HEAD")]
              (git! base "branch" target)
              (git! base "update-ref" (str "refs/remotes/origin/" target) head)
              (git! base
                    "symbolic-ref"
                    "refs/remotes/origin/HEAD"
                    (str "refs/remotes/origin/" target))
              (spit (io/file (:root draft) "b.txt") "approved\n")
              (let [result (drafts/approve! env {:workspace-id (:id draft)})]
                (expect (= target (:target-branch result)))
                (expect (= (:commit result) (git! base "rev-parse" target)))
                (expect (= head (git! base "rev-parse" "HEAD")))
                (expect (= "main" (git! base "branch" "--show-current")))
                (expect (= head (git! base "rev-parse" (str "refs/remotes/origin/" target))))
                (expect (= 0 (:ahead (drafts/status draft))))))))))
  (it "updates the target's linked checkout without switching the source feature branch"
      (with-clean-draft (fn [base env draft]
                          (let [head
                                (git! base "rev-parse" "HEAD")

                                checkout
                                (str ws/*drafts-home* "/main-checkout")]

                            (git! base "switch" "-q" "-c" "feature")
                            (git! base "worktree" "add" "-q" checkout "main")
                            (spit (io/file (:root draft) "b.txt") "approved\n")
                            (let [result (drafts/approve! env {:workspace-id (:id draft)})]
                              (expect (= (:commit result) (git! checkout "rev-parse" "HEAD")))
                              (expect (= "approved\n" (slurp (io/file checkout "b.txt"))))
                              (expect (= head (git! base "rev-parse" "HEAD")))
                              (expect (= "feature" (git! base "branch" "--show-current"))))))))
  (it "refuses a diverged target until synchronization in the draft and supports later approvals"
      (with-clean-draft
        (fn [base env draft]
          (spit (io/file base "main-only.txt") "main work\n")
          (git! base "add" "main-only.txt")
          (git! base "commit" "-q" "-m" "advance main")
          (spit (io/file (:root draft) "b.txt") "first\n")
          (expect (= :draft/sync-required (:type (approval-error env draft))))
          (git! (:root draft) "merge" "--no-edit" "main")
          (let [first-pass (drafts/approve! env {:workspace-id (:id draft)})]
            (expect (= (:commit first-pass) (git! base "rev-parse" "main")))
            (expect (= 3 (count (str/split (git! base "rev-list" "--parents" "-1" "HEAD") #" "))))
            (expect (= "main work\n" (slurp (io/file (:root draft) "main-only.txt"))))
            (spit (io/file (:root draft) "b.txt") "second\n")
            (let [second-pass (drafts/approve! env {:workspace-id (:id draft)})]
              (expect (= (:commit second-pass) (git! base "rev-parse" "main")))
              (expect (= "second\n" (slurp (io/file base "b.txt"))))
              (expect (= 0 (:ahead (drafts/status draft)))))))))
  (it "lands an already committed draft instead of incorrectly reporting nothing to approve"
      (with-clean-draft (fn [base env draft]
                          (spit (io/file (:root draft) "b.txt") "committed\n")
                          (git! (:root draft) "add" "b.txt")
                          (git! (:root draft) "commit" "-q" "-m" "existing draft commit")
                          (let [head
                                (git! (:root draft) "rev-parse" "HEAD")

                                result
                                (drafts/approve! env {:workspace-id (:id draft)})]

                            (expect (= :approved (:status result)))
                            (expect (= head (:commit result) (git! base "rev-parse" "main")))
                            (expect (= ["b.txt"] (:files result)))
                            (expect (= :nothing-to-approve
                                       (:status (drafts/approve! env
                                                                 {:workspace-id (:id draft)}))))))))
  (it "refuses a repository with no default branch instead of committing onto an arbitrary branch"
      (with-clean-draft (fn [base env draft]
                          (git! base "branch" "-m" "feature")
                          (spit (io/file (:root draft) "b.txt") "pending\n")
                          (expect (= :draft/no-target-branch
                                     (:type (approval-error env draft))))))))

(defdescribe
  approve-safety-test
  (it "approves with unrelated staged, unstaged, partially staged and untracked target changes"
      ;; A dirty main must not block approval of unrelated draft paths.
      (doseq [kind [:staged :unstaged :partially-staged :untracked]]
        (with-clean-draft
          (fn [base env draft]
            (let [path (if (= :untracked kind) "local.txt" "a.txt")]
              (spit (io/file base path) "local work\n")
              (when (#{:staged :partially-staged} kind) (git! base "add" path))
              (when (= :partially-staged kind) (spit (io/file base path) "more local work\n"))
              (spit (io/file (:root draft) "b.txt") "draft work\n")
              (let [status (git! base "status" "--porcelain")
                    staged (git! base "diff" "--cached")
                    unstaged (git! base "diff")
                    contents (slurp (io/file base path))
                    result (drafts/approve! env {:workspace-id (:id draft)})]

                (expect (= :approved (:status result)))
                (expect (= (:commit result) (git! base "rev-parse" "HEAD")))
                (expect (= "draft work\n" (slurp (io/file base "b.txt"))))
                (expect (= status (git! base "status" "--porcelain")))
                (expect (= staged (git! base "diff" "--cached")))
                (expect (= unstaged (git! base "diff")))
                (expect (= contents (slurp (io/file base path))))))))))
  (it "refuses to overwrite overlapping local work and allows retry without another draft commit"
      (doseq [kind [:staged :unstaged :untracked :ignored]]
        (with-clean-draft
          (fn [base env draft]
            (let [path (if (#{:untracked :ignored} kind) "local.txt" "a.txt")]
              (spit (io/file base path) "local work\n")
              (when (= :staged kind) (git! base "add" path))
              (when (= :ignored kind) (spit (io/file base ".git/info/exclude") "local.txt\n"))
              (spit (io/file (:root draft) path) "draft work\n")
              (when (= :ignored kind) (git! (:root draft) "add" "-f" path))
              (let [head (git! base "rev-parse" "HEAD")
                    index (git! base "write-tree")
                    status (git! base "status" "--porcelain")]

                (expect (= :draft/git-failed (:type (approval-error env draft))))
                (expect (= head (git! base "rev-parse" "HEAD")))
                (expect (= index (git! base "write-tree")))
                (expect (= status (git! base "status" "--porcelain")))
                (expect (= "local work\n" (slurp (io/file base path))))
                (expect (= "draft work" (git! (:root draft) "show" (str "HEAD:" path))))
                (expect (= "" (git! (:root draft) "status" "--porcelain")))
                (let [draft-head (git! (:root draft) "rev-parse" "HEAD")]
                  ;; Clear only this fixture's collision; approval reuses its saved commit.
                  (if (#{:untracked :ignored} kind)
                    (.delete (io/file base path))
                    (git! base "restore" "--source=HEAD" "--staged" "--worktree" "--" path))
                  (let [result (drafts/approve! env {:workspace-id (:id draft)})]
                    (expect (= :approved (:status result)))
                    (expect (= draft-head (:commit result)))
                    (expect (= "draft work\n" (slurp (io/file base path))))))))))))
  (it
    "conflicts leave the target untouched and retain a clean draft commit for resolution and retry"
    (with-clean-draft
      (fn [base env draft]
        (spit (io/file base "a.txt") "target version\n")
        (git! base "add" "a.txt")
        (git! base "commit" "-q" "-m" "target change")
        (spit (io/file (:root draft) "a.txt") "draft version\n")
        (let [head (git! base "rev-parse" "HEAD")]
          (expect (= :draft/sync-required (:type (approval-error env draft))))
          (expect (= head (git! base "rev-parse" "HEAD")))
          (expect (= "target version\n" (slurp (io/file base "a.txt"))))
          (expect (= "draft version\n" (slurp (io/file (:root draft) "a.txt"))))
          (expect (= "" (git! (:root draft) "status" "--porcelain")))
          (expect (= "" (git! (:root draft) "ls-files" "--unmerged")))
          (git! (:root draft) "merge" "-s" "ours" "--no-edit" "main")
          (spit (io/file (:root draft) "a.txt") "target version\n")
          (expect (= :approved (:status (drafts/approve! env {:workspace-id (:id draft)}))))))))
  (it "refuses to finish a merge already in progress in the draft"
      (with-clean-draft
        (fn [base env draft]
          (spit (io/file base "main-only.txt") "main work\n")
          (git! base "add" "main-only.txt")
          (git! base "commit" "-q" "-m" "advance main")
          (git! (:root draft) "merge" "--no-ff" "--no-commit" "main")
          (let [head
                (git! (:root draft) "rev-parse" "HEAD")

                index
                (git! (:root draft) "write-tree")

                merge-head
                (git! (:root draft) "rev-parse" "MERGE_HEAD")]

            (expect (= :draft/in-progress (:type (approval-error env draft))))
            (expect (= head (git! (:root draft) "rev-parse" "HEAD")))
            (expect (= index (git! (:root draft) "write-tree")))
            (expect (= merge-head (git! (:root draft) "rev-parse" "MERGE_HEAD")))))))
  (it "git/commit hooks can veto the draft commit before landing"
      (doseq [blocked-call [1]]
        (with-clean-draft
          (fn [base env draft]
            (spit (io/file (:root draft) "b.txt") "draft\n")
            (let [head (git! base "rev-parse" "HEAD")
                  calls (atom 0)]

              (try (extension/register-op-hook! {:op :git/commit
                                                 :phase :around
                                                 :owner :ext/draft-commit-test
                                                 :fn (fn [_env _op args next]
                                                       (if (= blocked-call (swap! calls inc))
                                                         {:exit 1 :out "" :err "commit blocked"}
                                                         (next args)))})
                   (expect (= :draft/git-failed (:type (approval-error env draft))))
                   (expect (= blocked-call @calls))
                   (expect (= head (git! base "rev-parse" "HEAD")))
                   (expect (= "" (git! base "status" "--porcelain")))
                   (expect (= "draft\n" (slurp (io/file (:root draft) "b.txt"))))
                   (finally (extension/unregister-op-hooks-for-owner!
                              :ext/draft-commit-test)))))))))

(defn- with-origin-draft
  "Real local origin and a clean draft, for either backend and target name."
  [backend target f]
  (with-repo "vis-drafts-origin"
             (fn [store base env]
               (when (or (= :worktree backend) (rift-available? base))
                 (git! base "add" "-A")
                 (git! base "commit" "-q" "-m" "baseline")
                 (git! base "branch" "-m" target)
                 (let [origin (str ws/*drafts-home* "/origin.git")]
                   (.mkdirs (io/file ws/*drafts-home*))
                   (git! base "clone" "--bare" "--quiet" base origin)
                   (git! base "remote" "add" "origin" origin)
                   (git! base "fetch" "--quiet" "origin")
                   (git! base "remote" "set-head" "origin" target)
                   (binding [ws/*draft-backend* backend]
                     (let [draft (drafts/create! env
                                                 {:session-state-id (str (random-uuid))
                                                  :label "remote"
                                                  :clean? true
                                                  :from (seed-trunk! store base)})]
                       (f base origin env draft))))))))

(defn- local-state
  "Index, worktree patch, porcelain state and existing stash identities."
  [base]
  (mapv #(apply git! base %)
        [["diff" "--cached"] ["diff"] ["status" "--porcelain"] ["stash" "list" "--format=%H"]]))

(defn- advanced-peer!
  "Create a peer with one unpublished commit beyond origin."
  [base origin]
  (let [peer (str ws/*drafts-home* "/peer")]
    (git! base "clone" "--quiet" origin peer)
    (git! peer "config" "user.name" "Vis Test")
    (git! peer "config" "user.email" "vis-test@example.invalid")
    (git! peer "config" "commit.gpgsign" "false")
    (spit (io/file peer "remote.txt") "remote\n")
    (git! peer "add" "remote.txt")
    (git! peer "commit" "-q" "-m" "advance origin")
    peer))

(defdescribe
  approve-origin-test
  (it
    "publishes only after restoring all unrelated local states, preserving existing stashes"
    (doseq [backend
            [:worktree :rift]

            target
            ["main" "master"]

            kind
            [:clean :staged :unstaged :partial :untracked :delete :rename]]

      (with-origin-draft
        backend
        target
        (fn [base origin env draft]
          (spit (io/file base "saved.txt") "old stash\n")
          (git! base "stash" "push" "-u" "-m" "user stash")
          (case kind
            :clean
            nil

            :untracked
            (spit (io/file base "local space\nfile.txt") "local\n")

            :delete
            (.delete (io/file base "a.txt"))

            :rename
            (git! base "mv" "a.txt" "renamed.txt")

            (do (spit (io/file base "a.txt") "staged\n")
                (when (#{:staged :partial} kind) (git! base "add" "a.txt"))
                (when (= :partial kind) (spit (io/file base "a.txt") "unstaged\n"))))
          (spit (io/file (:root draft) "b.txt") "draft\n")
          (let [before
                (local-state base)

                result
                (drafts/approve! env {:workspace-id (:id draft)})]

            (expect (= :approved (:status result)))
            (expect (true? (:published result)))
            (expect (= before (local-state base)))
            (expect (= (:commit result) (git! origin "rev-parse" target)))
            (when (= :untracked kind)
              (expect (= "local\n" (slurp (io/file base "local space\nfile.txt")))))
            (expect (= :nothing-to-approve
                       (:status (drafts/approve! env {:workspace-id (:id draft)})))))))))
  (it
    "refuses stale origin even when local target has not moved, then accepts explicit synchronization"
    (doseq [backend
            [:worktree :rift]

            target
            ["main" "master"]]

      (with-origin-draft
        backend
        target
        (fn [base origin env draft]
          (let [peer
                (advanced-peer! base origin)

                head
                (git! base "rev-parse" target)]

            (git! peer "push" "origin" target)
            (spit (io/file (:root draft) "b.txt") "draft\n")
            (expect (= :draft/sync-required (:type (approval-error env draft))))
            (expect (= head (git! base "rev-parse" target)))
            (git! (:root draft) "merge" "--no-edit" (git! origin "rev-parse" target))
            (let [result (drafts/approve! env {:workspace-id (:id draft)})]
              (expect (true? (:published result)))
              (expect (= (:commit result) (git! origin "rev-parse" target)))
              (expect (= "remote\n" (slurp (io/file base "remote.txt"))))))))))
  (it
    "reports rejected push after local restoration and retries without a duplicate commit"
    (doseq [backend
            [:worktree :rift]

            target
            ["main" "master"]]

      (with-origin-draft backend
                         target
                         (fn [base origin env draft]
                           (let [hook
                                 (io/file origin "hooks" "pre-receive")

                                 head
                                 (git! origin "rev-parse" target)]

                             (spit hook "#!/bin/sh\nexit 1\n")
                             (.setExecutable hook true)
                             (spit (io/file base "a.txt") "local\n")
                             (git! base "add" "a.txt")
                             (spit (io/file (:root draft) "b.txt") "draft\n")
                             (let [before
                                   (local-state base)

                                   error
                                   (approval-error env draft)

                                   landed
                                   (git! base "rev-parse" target)]

                               (expect (= :draft/push-failed (:type error)))
                               (expect (= :landed-locally (:status error)))
                               (expect (= before (local-state base)))
                               (expect (= head (git! origin "rev-parse" target)))
                               (expect (not= head landed))
                               (.delete hook)
                               (let [retry (drafts/approve! env {:workspace-id (:id draft)})]
                                 (expect (= :nothing-to-approve (:status retry)))
                                 (expect (true? (:published retry)))
                                 (expect (= landed (git! origin "rev-parse" target)))
                                 (expect (= before (local-state base))))))))))
  (it "unreachable origin refuses before committing, stashing or landing"
      (with-origin-draft
        :worktree
        "main"
        (fn [base _origin env draft]
          (git! base "remote" "set-url" "origin" (str ws/*drafts-home* "/missing.git"))
          (spit (io/file base "a.txt") "local\n")
          (spit (io/file (:root draft) "b.txt") "draft\n")
          (let [before
                (local-state base)

                head
                (git! (:root draft) "rev-parse" "HEAD")]

            (expect (= :draft/git-failed (:type (approval-error env draft))))
            (expect (= before (local-state base)))
            (expect (= head (git! (:root draft) "rev-parse" "HEAD")))))))
  (it "retained approval stashes block even an already-landed retry from publishing"
      (with-origin-draft
        :worktree
        "main"
        (fn [base origin env draft]
          (let [head (git! origin "rev-parse" "main")]
            (spit (io/file base "a.txt") "recover me\n")
            (git! base "stash" "push" "-m" "vis-approve-interrupted")
            (expect (= :draft/recovery-required (:type (approval-error env draft))))
            (expect (= head (git! origin "rev-parse" "main")))
            (expect (str/includes? (git! base "stash" "list") "vis-approve-interrupted")))))))

(defdescribe
  approve-failure-recovery-test
  (it
    "a failed landing restores local work; a failed restore retains its stash and blocks push and retry"
    (doseq [phase [:land :restore]]
      (with-origin-draft
        :worktree
        "main"
        (fn [base origin env draft]
          (spit (io/file base "a.txt") "local\n")
          (git! base "add" "a.txt")
          (spit (io/file base "a.txt") "more local\n")
          (spit (io/file (:root draft) "b.txt") "draft\n")
          (let [before (local-state base)
                head (git! origin "rev-parse" "main")
                run workspace-git/run-git
                error (with-redefs [workspace-git/run-git
                                    (fn [dir args timeout]
                                      (if (or (and (= :land phase)
                                                   (= ["merge" "--ff-only"] (vec (take 2 args))))
                                              (and (= :restore phase)
                                                   (= ["stash" "apply"] (vec (take 2 args)))))
                                        {:exit 1 :out "" :err "injected failure"}
                                        (run dir args timeout)))]
                        (approval-error env draft))]

            (expect (= head (git! origin "rev-parse" "main")))
            (if (= :land phase)
              (do (expect (= :draft/git-failed (:type error)))
                  (expect (= head (git! base "rev-parse" "main")))
                  (expect (= before (local-state base))))
              (do (expect (= :draft/restore-failed (:type error)))
                  (expect (= (:stash error) (git! base "rev-parse" "refs/stash")))
                  (expect (= :draft/recovery-required (:type (approval-error env draft))))
                  (expect (= head (git! origin "rev-parse" "main")))
                  (git! base "stash" "apply" "--index" (:stash error))
                  (git! base "stash" "drop")
                  (expect (= before (local-state base)))))
            (expect (true? (:published (drafts/approve! env {:workspace-id (:id draft)})))))))))
  (it "staged work cancelled in the worktree still counts as overlapping local work"
      (with-clean-draft (fn [base env draft]
                          (let [original
                                (slurp (io/file base "a.txt"))

                                head
                                (git! base "rev-parse" "HEAD")]

                            (spit (io/file base "a.txt") "staged\n")
                            (git! base "add" "a.txt")
                            (spit (io/file base "a.txt") original)
                            (spit (io/file (:root draft) "a.txt") "draft\n")
                            (let [before (local-state base)]
                              (expect (= :draft/git-failed (:type (approval-error env draft))))
                              (expect (= before (local-state base)))
                              (expect (= head (git! base "rev-parse" "HEAD"))))))))
  (it "origin advancing after fetch rejects push without overwriting remote history"
      (with-origin-draft
        :worktree
        "main"
        (fn [base origin env draft]
          (let [peer (advanced-peer! base origin)]
            (spit (io/file (:root draft) "b.txt") "draft\n")
            (spit (io/file base "a.txt") "local\n")
            (let [before (local-state base)]
              (try (extension/register-op-hook! {:op :draft/approve
                                                 :phase :around
                                                 :owner :ext/draft-race-test
                                                 :fn (fn [_env _op args next]
                                                       (git! peer "push" "origin" "main")
                                                       (next args))})
                   (expect (= :draft/push-failed (:type (approval-error env draft))))
                   (expect (= (git! peer "rev-parse" "HEAD") (git! origin "rev-parse" "main")))
                   (expect (= before (local-state base)))
                   (finally (extension/unregister-op-hooks-for-owner! :ext/draft-race-test)))
              (expect (= :draft/sync-required (:type (approval-error env draft))))
              (git! (:root draft) "merge" "--no-edit" (git! origin "rev-parse" "main"))
              (expect (true? (:published (drafts/approve! env {:workspace-id (:id draft)}))))))))))

(defdescribe approve-rebase-retry-test
             (it "retries a refused landing after rebasing the draft onto an advanced local target"
                 (doseq [backend [:worktree :rift]]
                   (with-origin-draft
                     backend
                     "main"
                     (fn [base _origin env draft]
                       (spit (io/file base "b.txt") "local\n")
                       (spit (io/file (:root draft) "b.txt") "draft\n")
                       (expect (= :draft/git-failed (:type (approval-error env draft))))
                       (.delete (io/file base "b.txt"))
                       (spit (io/file base "main-only.txt") "main\n")
                       (git! base "add" "main-only.txt")
                       (git! base "commit" "-q" "-m" "advance main")
                       (git! (:root draft) "fetch" "--quiet" base "main")
                       (git! (:root draft) "rebase" "FETCH_HEAD")
                       (expect (true? (:published
                                        (drafts/approve! env {:workspace-id (:id draft)})))))))))
