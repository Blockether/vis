(ns com.blockether.vis.internal.workspace-test
  "Workspace facade tests. The DB-backed paths use an in-memory SQLite
   store through the sqlite extension; git-backed paths exercise the
   live vis repository (kind=:trunk, no worktree to clean up).

   Branch-kind operations (spawn-branch!, apply-to-trunk!, discard! of
   real worktrees) require disk side-effects under
   `~/.vis/workspaces/<repo-id>/<id>/` and are exercised through smoke
   tests in the REPL during development. They are out of scope for the
   unit suite to keep CI fast and cleanup-free."
  (:require
   [com.blockether.vis.ext.persistance-sqlite.core :as ps]
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.internal.workspace :as ws]
   [lazytest.core :refer [defdescribe describe expect it]]
   [next.jdbc :as jdbc]))

(defn- with-store
  "Open an :memory sqlite store, run `f` with it, dispose."
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store)
      (finally (ps/db-close! store)))))

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
  (it "apply-to-trunk! throws :workspace/trunk-merge for trunk-kind"
    (with-store
      (fn [store]
        (let [trunk (ws/ensure-trunk! store {})]
          (try (ws/apply-to-trunk! store {:workspace-id (:id trunk)})
            (expect false)
            (catch clojure.lang.ExceptionInfo e
              (expect (= :workspace/trunk-merge (:type (ex-data e))))))))))

  (it "discard! throws :workspace/trunk-discard for trunk-kind"
    (with-store
      (fn [store]
        (let [trunk (ws/ensure-trunk! store {})]
          (try (ws/discard! store {:workspace-id (:id trunk)})
            (expect false)
            (catch clojure.lang.ExceptionInfo e
              (expect (= :workspace/trunk-discard (:type (ex-data e)))))))))))

(defdescribe lookup-errors-test
  (it "apply-to-trunk! reports unknown workspace-id in ex-data"
    (with-store
      (fn [store]
        (try (ws/apply-to-trunk! store {:workspace-id "nope"})
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
