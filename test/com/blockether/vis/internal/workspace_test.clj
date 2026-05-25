(ns com.blockether.vis.internal.workspace-test
  "Workspace facade tests. The DB-backed paths use an in-memory SQLite
   store through the sqlite extension; git-backed paths exercise the
   live vis repository (kind=:trunk, no worktree to clean up).

   Branch-kind operations use temporary git repositories so apply/discard
   regressions stay covered without touching the live repository."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [com.blockether.vis.ext.persistance-sqlite.core :as ps]
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.workspace :as ws]
   [lazytest.core :refer [defdescribe describe expect it]]
   [next.jdbc :as jdbc]))

(defn- with-store
  "Open an :memory sqlite store, run `f` with it, dispose."
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store)
      (finally (ps/db-close! store)))))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath
    (.toFile
      (java.nio.file.Files/createTempDirectory
        prefix
        (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- git!
  [dir args]
  (let [argv   (into ["git" "-C" (.getCanonicalPath (io/file dir))] args)
        result (apply sh/sh argv)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "git failed: " (str/join " " argv))
               (assoc result :argv argv))))
    (str/trim (or (:out result) ""))))

(defn- init-git-repo!
  [repo-root]
  (git! repo-root ["init"])
  (git! repo-root ["config" "user.name" "Vis Test"])
  (git! repo-root ["config" "user.email" "vis-test@example.invalid"])
  (spit (io/file repo-root "note.txt") "base\n")
  (git! repo-root ["add" "note.txt"])
  (git! repo-root ["commit" "-m" "base"]))

(defn- branch-workspace!
  [store repo-root worktree-root]
  (let [branch (str "vis/apply-test-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
    (git! repo-root ["worktree" "add" "-b" branch worktree-root "HEAD"])
    (ps/db-workspace-insert! store
      {:id        (str (java.util.UUID/randomUUID))
       :repo-id   "apply-test"
       :repo-root repo-root
       :kind      :branch
       :branch    branch
       :root      worktree-root
       :state     :active
       :commit-id (git! worktree-root ["rev-parse" "HEAD"])})))

(defdescribe cwd-binding-test
  (it "falls back to process cwd when *workspace-root* is unbound (REPL/test convenience)"
    (let [process-cwd (System/getProperty "user.dir")]
      (expect (= process-cwd (.getPath (ws/cwd))))))

  (it "returns the bound root inside a binding"
    (binding [ws/*workspace-root* "/tmp"]
      (expect (= "/private/tmp" (.getCanonicalPath (ws/cwd))))))

  (it "workspace-root reads :workspace/root from an env map"
    (expect (= "/private/tmp"
              (ws/workspace-root {:workspace/root "/tmp"}))))

  (it "workspace-root accepts a raw string and canonicalises it"
    (expect (= "/private/tmp" (ws/workspace-root "/tmp"))))

  (it "workspace-root returns nil for blank input"
    (expect (nil? (ws/workspace-root "   ")))
    (expect (nil? (ws/workspace-root nil)))))

(defdescribe trunk-test
  (describe "ensure-trunk! against the live git repo"
    (it "creates a trunk-kind row with branch + commit captured"
      (with-store
        (fn [store]
          (let [ws (ws/ensure-trunk! store {})]
            (expect (= :trunk (:kind ws)))
            (expect (= :active (:state ws)))
            (expect (string? (:branch ws)))
            (expect (string? (:commit-id ws)))
            (expect (string? (:repo-root ws)))
            (expect (= (:repo-root ws) (:root ws)))))))

    (it "is idempotent when called with the same pinned session-state"
      (with-store
        (fn [store]
          (let [trunk (ws/ensure-trunk! store {})
                ;; Simulate a session_state row pinned to the trunk.
                ds    (:datasource store)
                _ (jdbc/execute! ds
                    ["INSERT INTO session_soul (id, created_at) VALUES ('s1', 1)"])
                _ (jdbc/execute! ds
                    ["INSERT INTO session_state (id, session_soul_id, workspace_id, version, created_at)
                      VALUES ('ss1', 's1', ?, 0, 1)" (str (:id trunk))])
                again (ws/ensure-trunk! store {:session-state-id "ss1"})]
            (expect (= (:id trunk) (:id again)))))))

    (it "for-session returns nil for an unbound session-state"
      (with-store
        (fn [store]
          (expect (nil? (ws/for-session store "nonexistent"))))))))

(defdescribe trunk-refuses-mutations-test
  (it "ff-apply! throws :workspace/trunk-merge for trunk-kind"
    (with-store
      (fn [store]
        (let [trunk (ws/ensure-trunk! store {})]
          (try (ws/ff-apply! store {:workspace-id (:id trunk)})
            (expect false)
            (catch clojure.lang.ExceptionInfo e
              (expect (= :workspace/trunk-merge (:type (ex-data e))))))))))

  (it "commit! throws :workspace/trunk-commit for trunk-kind"
    (with-store
      (fn [store]
        (let [trunk (ws/ensure-trunk! store {})]
          (try (ws/commit! store {:workspace-id (:id trunk) :message "x"})
            (expect false)
            (catch clojure.lang.ExceptionInfo e
              (expect (= :workspace/trunk-commit (:type (ex-data e))))))))))

  (it "discard! throws :workspace/trunk-discard for trunk-kind"
    (with-store
      (fn [store]
        (let [trunk (ws/ensure-trunk! store {})]
          (try (ws/discard! store {:workspace-id (:id trunk)})
            (expect false)
            (catch clojure.lang.ExceptionInfo e
              (expect (= :workspace/trunk-discard (:type (ex-data e)))))))))))

(defdescribe commit-and-ff-apply-test
  (it "commit! returns :nothing-to-commit on clean worktree"
    (with-store
      (fn [store]
        (let [base          (temp-dir "vis-workspace-commit-clean")
              repo-root     (str (io/file base "repo"))
              worktree-root (str (io/file base "worktree"))]
          (try
            (.mkdirs (io/file repo-root))
            (init-git-repo! repo-root)
            (let [workspace (branch-workspace! store repo-root worktree-root)
                  result    (ws/commit! store {:workspace-id (:id workspace)
                                               :message      "empty"})]
              (expect (= :nothing-to-commit (:status result))))
            (finally (delete-tree! base)))))))

  (it "commit! + ff-apply! fast-forwards trunk to the new branch HEAD"
    (with-store
      (fn [store]
        (let [base          (temp-dir "vis-workspace-ff-apply")
              repo-root     (str (io/file base "repo"))
              worktree-root (str (io/file base "worktree"))]
          (try
            (.mkdirs (io/file repo-root))
            (init-git-repo! repo-root)
            (let [trunk-head-before (git! repo-root ["rev-parse" "HEAD"])
                  workspace         (branch-workspace! store repo-root worktree-root)]
              (spit (io/file (:root workspace) "note.txt") "changed-via-ff\n")
              (spit (io/file (:root workspace) "new.txt") "new-via-ff\n")
              (let [c-result  (ws/commit! store {:workspace-id (:id workspace)
                                                 :message      "branch work"})]
                (expect (= :ok (:status c-result)))
                (expect (string? (:sha c-result)))
                (expect (not= trunk-head-before (:sha c-result))))
              (let [ff (ws/ff-apply! store {:workspace-id (:id workspace)})]
                (expect (= :ok (:status ff)))
                (expect (= :merged (get-in ff [:workspace :state])))
                (expect (= "changed-via-ff\n" (slurp (io/file repo-root "note.txt"))))
                (expect (= "new-via-ff\n" (slurp (io/file repo-root "new.txt"))))))
            (finally (delete-tree! base)))))))

  (it "ff-apply! reports :ff-failed when trunk has diverged history"
    (with-store
      (fn [store]
        (let [base          (temp-dir "vis-workspace-ff-failed")
              repo-root     (str (io/file base "repo"))
              worktree-root (str (io/file base "worktree"))]
          (try
            (.mkdirs (io/file repo-root))
            (init-git-repo! repo-root)
            (let [workspace (branch-workspace! store repo-root worktree-root)]
              ;; Commit divergent work on the trunk so FF is impossible.
              (spit (io/file repo-root "trunk-only.txt") "trunk-only\n")
              (git! repo-root ["add" "trunk-only.txt"])
              (git! repo-root ["commit" "-m" "trunk diverges"])
              ;; Commit on the workspace branch.
              (spit (io/file (:root workspace) "note.txt") "branch-edit\n")
              (ws/commit! store {:workspace-id (:id workspace)
                                 :message      "branch work"})
              (let [ff (ws/ff-apply! store {:workspace-id (:id workspace)})]
                (expect (= :ff-failed (:status ff)))
                (expect (some? (:reason ff)))
                ;; Workspace stays in :merging so the engine can hand off
                ;; to start-merge-resolve!.
                (expect (= :merging
                          (:state (ws/get store (:id workspace)))))))
            (finally (delete-tree! base))))))))

(defdescribe start-merge-resolve-test
  (it "refuses :trunk-kind workspaces"
    (with-store
      (fn [store]
        (let [trunk (ws/ensure-trunk! store {})]
          (try (ws/start-merge-resolve! store
                 {:workspace-id (:id trunk)
                  :parent-session-state-id "ss"})
            (expect false)
            (catch clojure.lang.ExceptionInfo e
              (expect (= :workspace/trunk-merge-resolve (:type (ex-data e))))))))))

  (it "refuses missing :parent-session-state-id"
    (let [base (temp-dir "vis-merge-resolve-missing")]
      (try
        (with-store
          (fn [store]
            (.mkdirs (io/file base))
            (init-git-repo! base)
            (let [workspace (branch-workspace! store base
                              (str (io/file base "-wt")))]
              (try (ws/start-merge-resolve! store
                     {:workspace-id (:id workspace)})
                (expect false)
                (catch clojure.lang.ExceptionInfo e
                  (expect (= :workspace/missing-parent-session-state
                            (:type (ex-data e)))))))))
        (finally (delete-tree! base)))))

  (it "spawns a merge-resolve sub-session pinned to the parent workspace on conflict"
    (let [base (temp-dir "vis-merge-resolve-conflict")]
      (try
        (with-store
          (fn [store]
            ;; trunk repo with two diverging commits  — same file edited
            ;; on both sides so `git merge` lands on a conflict.
            (.mkdirs (io/file base))
            (init-git-repo! base)
            (let [trunk-branch (git! base ["rev-parse" "--abbrev-ref" "HEAD"])
                  workspace    (branch-workspace! store base
                                 (str (io/file base "-wt")))]
              ;; Trunk-side divergent commit on the same file.
              (spit (io/file base "note.txt") "trunk-side\n")
              (git! base ["commit" "-am" "trunk-divergence"])
              ;; Branch-side divergent commit on the same file.
              (spit (io/file (:root workspace) "note.txt") "branch-side\n")
              (git! (:root workspace) ["commit" "-am" "branch-divergence"])
              ;; Stage a parent session_state pinned to the workspace.
              (let [ds  (:datasource store)
                    sid (str (java.util.UUID/randomUUID))
                    st  (str (java.util.UUID/randomUUID))]
                (jdbc/execute! ds ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)"
                                   sid "tui" 1])
                (jdbc/execute! ds [(str "INSERT INTO session_state "
                                     "(id, session_soul_id, workspace_id, version, created_at) "
                                     "VALUES (?,?,?,?,?)")
                                   st sid (str (:id workspace)) 0 1])
                (let [result (ws/start-merge-resolve! store
                               {:workspace-id (:id workspace)
                                :parent-session-state-id st})]
                  (expect (= :ok (:status result)))
                  (expect (some? (:sub-session-state-id result)))
                  (expect (= trunk-branch (:trunk result)))
                  (expect (some #(= "note.txt" (:path %)) (:conflicts result)))
                  ;; Sub-session is queryable and carries the parent
                  ;; pointer; the partial UNIQUE index lets it share
                  ;; workspace_id with the parent.
                  (let [parent-id (persistance/db-session-state-merge-resolve-parent
                                    store (:sub-session-state-id result))]
                    (expect (= st (str parent-id)))))))))
        (finally (delete-tree! base))))))

(defdescribe lookup-errors-test
  (it "ff-apply! reports unknown workspace-id in ex-data"
    (with-store
      (fn [store]
        (try (ws/ff-apply! store {:workspace-id "nope"})
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= "nope" (:workspace-id (ex-data e)))))))))

  (it "commit! reports unknown workspace-id in ex-data"
    (with-store
      (fn [store]
        (try (ws/commit! store {:workspace-id "nope"})
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= "nope" (:workspace-id (ex-data e)))))))))

  (it "discard! reports unknown workspace-id in ex-data"
    (with-store
      (fn [store]
        (try (ws/discard! store {:workspace-id "nope"})
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= "nope" (:workspace-id (ex-data e))))))))))

(defdescribe trunk-info-test
  (it "returns repo-root, branch and head from live git"
    (let [info (ws/trunk-info)]
      (expect (string? (:repo-root info)))
      (expect (string? (:branch info)))
      (expect (string? (:head info))))))

(defdescribe hooks-test
  (it "register-hook! returns the id and exceptions are swallowed"
    (let [fired (atom 0)
          throws (atom 0)]
      (ws/register-hook! :on-spawn
        (fn [_] (swap! fired inc)))
      (ws/register-hook! :on-spawn
        (fn [_] (swap! throws inc) (throw (Exception. "boom"))))
      (expect (= :on-spawn
                (ws/register-hook! :on-spawn (fn [_])))))))
