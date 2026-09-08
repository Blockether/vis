(ns com.blockether.vis.internal.foundation.drafts-test
  "The model's draft surface: `draft_create`, `draft_status`, `draft_approve`
   and `draft_discard` move a pinned session through one draft from the
   sandbox, and the foundation ctx block follows the live confinement pointer.
   A real git repository under a temp dir, an in-memory store and a rebound
   drafts home."
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

(defn- with-session
  "Run `(f base env)`: a pinned trunk session over a fresh repo. `env` is the
   symbol env a turn injects — store, session, state and the live confinement
   pointer the symbols move."
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
                 (pin-session! store soul (:id trunk))]

             (f base
                {:db-info store
                 :session-id soul
                 :session/state-id state-id
                 :workspace/id (:id trunk)
                 :workspace-atom (atom trunk)})))
         (finally (ps/db-close! store) (delete-tree! (str base "-store")) (delete-tree! base)))))

(defn- ctx-root
  "The root the foundation ctx block reports for `env` right now."
  [env]
  (get-in ((:ext/ctx-fn foundation/vis-extension) env) ["session_workspace" "root"]))

(defdescribe
  draft-symbol-roundtrip-test
  (it
    "draft_create opens a worktree draft the session then works in, draft_approve lands it, draft_discard returns to trunk"
    (with-session
      "vis-fdrafts"
      (fn [base env]
        (let [opened
              (drafts/draft-create env "feature-x")

              draft-root
              (or (get (:result opened) "root")
                  (throw (ex-info "draft_create refused" {:opened opened})))

              ctx-in-draft
              (ctx-root env)

              ctx-target
              (get-in ((:ext/ctx-fn foundation/vis-extension) env)
                      ["session_workspace" "draft" "target_branch"])

              draft-status
              (:result (drafts/draft-status env))

              _
              (do (spit (io/file draft-root "b.txt") "new\n") (spit (io/file base "a.txt") "x\n"))

              draft-content
              (slurp (io/file draft-root "a.txt"))

              approved
              (:result (drafts/draft-approve env "feat: add b"))

              again
              (:result (drafts/draft-approve env "again"))

              discarded
              (:result (drafts/draft-discard env))

              trunk-status
              (:result (drafts/draft-status env))]

          (expect (true? (extension/envelope-success? opened)))
          (expect (= "feature-x" (get (:result opened) "label")))
          (expect (= "vis/feature-x" (get (:result opened) "branch")))
          (expect (= "worktree" (get (:result opened) "backend")))
          (expect (false? (get (:result opened) "clean")))
          (expect (not= base draft-root))
          ;; the ctx block follows the live pointer the same turn
          (expect (= draft-root ctx-in-draft))
          (expect (= "main" (get draft-status "target_branch")))
          (expect (= "main" ctx-target))
          (expect (true? (get draft-status "in_draft")))
          (expect (= draft-root (get draft-status "root")))
          ;; the pending trunk edit came along
          (expect (= "x\npending\n" draft-content))
          (expect (= "approved" (get approved "status")))
          (expect (= 2 (count (get approved "files"))))
          (expect (= "main" (get approved "target_branch")))
          (expect (= "feat: add b" (git! base "log" "-1" "--format=%s" "main")))
          (expect (= (get approved "commit") (git! base "rev-parse" "main")))
          (expect (= "nothing-to-approve" (get again "status")))
          (expect (= "discarded" (get discarded "status")))
          (expect (= "vis/feature-x" (get discarded "branch")))
          (expect (= 0 (get discarded "approved_ahead")))
          (expect (= base (get discarded "root")))
          (expect (= base (ctx-root env)))
          (expect (false? (get trunk-status "in_draft")))
          (expect (= base (get trunk-status "root")))
          (expect (= "new\n" (slurp (io/file base "b.txt"))))))))
  (it "clean=True seeds from HEAD and leaves pending trunk work behind"
      (with-session "vis-fdrafts-clean"
                    (fn [_base env]
                      (let [opened (drafts/draft-create env "clean-x" {"clean" true})]
                        (expect (true? (extension/envelope-success? opened)))
                        (expect (true? (get (:result opened) "clean")))
                        (expect (= "x\n"
                                   (slurp (io/file (get (:result opened) "root") "a.txt"))))))))
  (it
    "refuses what makes no sense: an unnamed draft, a second draft, approving or discarding on trunk"
    (with-session "vis-fdrafts-refuse"
                  (fn [_base env]
                    (let [failed? (complement extension/envelope-success?)]
                      (expect (failed? (drafts/draft-create env "")))
                      (expect (failed? (drafts/draft-approve env)))
                      (expect (failed? (drafts/draft-discard env)))
                      (expect (true? (extension/envelope-success? (drafts/draft-create env "one"))))
                      (let [second (drafts/draft-create env "two")]
                        (expect (failed? second))
                        (expect (str/includes? (get-in second [:error :message]) "one")))
                      (expect (true? (extension/envelope-success? (drafts/draft-discard env))))
                      (expect (failed? (binding [ws/*draft-backend* :off]
                                         (drafts/draft-create env "three")))))))))

(defdescribe
  draft-symbols-test
  (it
    "the sandbox names draft_create, draft_status, draft_approve and draft_discard, and nothing else manages drafts"
    (expect (= ["draft-status" "draft-create" "draft-approve" "draft-discard"]
               (mapv (comp name :ext.symbol/symbol) drafts/symbols)))
    (expect (empty? (filter #(#{"draft" "approve" "discard"} (:slash/name %))
                            (:ext/slash-commands foundation/vis-extension))))))
