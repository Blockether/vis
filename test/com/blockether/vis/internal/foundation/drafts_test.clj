(ns com.blockether.vis.internal.foundation.drafts-test
  "The user's and the model's draft surface: `/draft`, `/approve`, `/discard`
   move a pinned session through one draft, and `draft_status` /
   `draft_approve` see the same draft from the sandbox. A real git repository
   under a temp dir, an in-memory store and a rebound drafts home."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.drafts :as drafts]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.workspace.core :as ws]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]
            [next.jdbc :as jdbc]))

;; Sandbox envelopes name their op, which needs the registration the manifest supplies.
(set-ns-context! [(around-each [f]
                               (let [registered? (some #(= "foundation-core" (:ext/name %))
                                                       (extension/registered-extensions))]
                                 (when-not registered? (foundation/register!))
                                 (try (f)
                                      (finally (when-not registered?
                                                 (extension/deregister-extension!
                                                   "foundation-core"))))))])

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
  [root]
  (git! root "init" "-q" "-b" "main")
  (git! root "config" "user.name" "Vis Test")
  (git! root "config" "user.email" "vis-test@example.invalid")
  (git! root "config" "commit.gpgsign" "false")
  (spit (io/file root "a.txt") "x\n")
  (git! root "add" "a.txt")
  (git! root "commit" "-q" "-m" "init")
  (spit (io/file root "a.txt") "x\npending\n"))

(defn- pin-session!
  "A session soul + state pinned to `workspace-id`; answers the state id."
  [store soul-id workspace-id]
  (let [state-id (str (random-uuid))]
    (jdbc/execute! (:datasource store)
                   ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)" soul-id
                    "tui" 1])
    (jdbc/execute!
      (:datasource store)
      [(str "INSERT INTO session_state (id, session_soul_id, workspace_id, version, created_at) "
            "VALUES (?,?,?,?,?)") state-id soul-id workspace-id 0 1])
    state-id))

(defn- run-slash
  [name ctx]
  ((:slash/run-fn (first (filter #(= name (:slash/name %)) drafts/specs))) ctx))

(defn- with-session
  "Run `(f base ctx-fn env-fn)`: a pinned trunk session over a fresh repo.
   `ctx-fn` builds a slash ctx from argv, `env-fn` the symbol env, both
   following the live confinement pointer."
  [prefix f]
  (let [base
        (temp-dir prefix)

        store
        (assoc (ps/db-open! :memory) :backend :sqlite)]

    (try (init-repo! base)
         (binding [ws/*drafts-home*
                   (str base "-store")

                   ws/*workspace-root*
                   base

                   ws/*draft-backend*
                   :worktree]

           (let [trunk
                 (ws/create-trunk-at! store base)

                 soul
                 (str (random-uuid))

                 state-id
                 (pin-session! store soul (:id trunk))

                 pointer
                 (atom trunk)]

             (f base
                (fn [& argv]
                  {:db-info store
                   :session/id soul
                   :session/state-id state-id
                   :workspace-atom pointer
                   :command/argv (vec argv)})
                (fn []
                  {:db-info store :session-id soul :workspace/id (:id @pointer)}))))
         (finally (ps/db-close! store) (delete-tree! (str base "-store")) (delete-tree! base)))))

(defdescribe
  draft-slash-roundtrip-test
  (it
    "/draft opens a worktree draft the session then works in, /approve lands it, /discard returns to trunk"
    (with-session
      "vis-fdrafts"
      (fn [base ctx env]
        (let [opened
              (run-slash "draft" (ctx "feature" "x"))

              draft-status
              (:result (drafts/draft-status (env)))

              draft-root
              (get draft-status "root")

              _
              (spit (io/file draft-root "b.txt") "new\n")

              approved
              (run-slash "approve" (ctx "feat:" "add" "b"))

              again
              (:result (drafts/draft-approve (env) "again"))

              discarded
              (run-slash "discard" (ctx))

              trunk-status
              (:result (drafts/draft-status (env)))]

          (expect (= :ok (:slash/status opened)))
          (expect (str/includes? (:slash/title opened) "feature-x"))
          (expect (= "vis/feature-x" (get-in opened [:slash/data "branch"])))
          (expect (true? (get draft-status "in_draft")))
          (expect (= "worktree" (get draft-status "backend")))
          (expect (not= base draft-root))
          ;; the pending trunk edit came along
          (expect (= "x\npending\n" (slurp (io/file draft-root "a.txt"))))
          (expect (= :ok (:slash/status approved)))
          (expect (str/starts-with? (:slash/title approved) "Approved 2 path(s) on vis/feature-x"))
          (expect (= "feat: add b" (git! base "log" "-1" "--format=%s" "vis/feature-x")))
          (expect (= "init" (git! base "log" "-1" "--format=%s" "HEAD")))
          (expect (= "nothing-to-approve" (get again "status")))
          (expect (= :ok (:slash/status discarded)))
          (expect (str/includes? (:slash/body discarded) "vis/feature-x"))
          (expect (false? (get trunk-status "in_draft")))
          (expect (= base (get trunk-status "root")))
          (expect (str/includes? (git! base "branch" "--list" "vis/*") "vis/feature-x"))))))
  (it "refuses what makes no sense: an unnamed draft, approving or discarding on trunk"
      (with-session "vis-fdrafts-refuse"
                    (fn [_base ctx env]
                      (expect (= :error (:slash/status (run-slash "draft" (ctx)))))
                      (expect (= :error (:slash/status (run-slash "approve" (ctx)))))
                      (expect (= :error (:slash/status (run-slash "discard" (ctx)))))
                      (expect (false? (extension/envelope-success? (drafts/draft-approve
                                                                     (env)))))))))
