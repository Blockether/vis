(ns com.blockether.vis.internal.foundation.drafts-test
  "The model's draft surface: `draft_create`, `draft_status`, `draft_approve`
   and `draft_discard` move a pinned session through one draft from the
   sandbox, and the foundation ctx block follows the live confinement pointer.
   A real git repository under a temp dir, an in-memory store and a rebound
   drafts home."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.drafts :as drafts]
            [com.blockether.vis.internal.foundation.mpl-capture :as capture]
            [com.blockether.vis.internal.foundation.workspace-slashes :as workspace-slashes]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.workspace.core :as ws]
            [com.blockether.vis.internal.workspace.drafts :as lifecycle]
            [com.blockether.vis.test-python-context :as tpc]
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
        (ps/db-open! :memory)]

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

;; Draft recovery reports #242 and #243: isolation must be an explicit opt-in.
(defdescribe draft-backend-setting-test
             (it "is an experimental opt-in that defaults to off in both clients"
                 (let [spec (toggles/toggle-spec ws/draft-backend-toggle-id)]
                   (expect (= "off" (:default spec)))
                   (expect (= :enum (:type spec)))
                   (expect (= ["auto" "worktree" "rift" "off"] (:choices spec)))
                   (expect (true? (:persist? spec)))
                   (expect (true? (:experimental? spec)))
                   (expect (= :experimental (:group spec)))
                   (doseq [channel [:tui :web]]
                     (expect (some #(= ws/draft-backend-toggle-id (:id %))
                                   (toggles/toggles-for-channel channel))))))
             (it "keeps missing or invalid settings off while honoring explicit backend choices"
                 (binding [ws/*draft-backend* nil]
                   (doseq [configured [nil "" "unknown" false "off" "auto" "worktree" "rift"]]
                     (with-redefs [toggles/value-of (constantly configured)]
                       (expect (= (if (contains? #{"auto" "worktree" "rift"} configured)
                                    (keyword configured)
                                    :off)
                                  (ws/draft-backend-setting)))))))
             (it "honors an explicit backend override even when the saved setting is off"
                 (with-redefs [toggles/value-of (constantly "off")]
                   (binding [ws/*draft-backend* :worktree]
                     (expect (= :worktree (ws/draft-backend-setting)))))))

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
              (do (spit (io/file draft-root "b.txt") "new\n")
                  (spit (io/file base "local.txt") "keep local work\n"))

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
          (expect (true? (get (:result opened) "clean")))
          (expect (not= base draft-root))
          ;; the ctx block follows the live pointer the same turn
          (expect (= draft-root ctx-in-draft))
          (expect (= "main" (get draft-status "target_branch")))
          (expect (= "main" ctx-target))
          (expect (true? (get draft-status "in_draft")))
          (expect (= draft-root (get draft-status "root")))
          ;; Default drafts exclude pending trunk work.
          (expect (= "x\n" draft-content))
          (expect (= "approved" (get approved "status")))
          (expect (= 1 (count (get approved "files"))))
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
          (expect (= "keep local work\n" (slurp (io/file base "local.txt"))))
          (expect (= "x\npending\n" (slurp (io/file base "a.txt"))))
          (expect (= "M a.txt\n?? local.txt" (git! base "status" "--porcelain")))
          (expect (= "new\n" (slurp (io/file base "b.txt"))))))))
  (it "clean=True seeds from HEAD and leaves pending trunk work behind"
      (with-session "vis-fdrafts-clean"
                    (fn [_base env]
                      (let [opened (drafts/draft-create env "clean-x" {"clean" true})]
                        (expect (true? (extension/envelope-success? opened)))
                        (expect (true? (get (:result opened) "clean")))
                        (expect (= "x\n"
                                   (slurp (io/file (get (:result opened) "root") "a.txt"))))))))
  (it "explicit clean=False still copies pending work, for positional and map arguments"
      (doseq [arg [false {"clean" false} {:clean false}]]
        (with-session "vis-fdrafts-dirty"
                      (fn [_base env]
                        (let [opened (drafts/draft-create env "dirty-x" arg)]
                          (expect (true? (extension/envelope-success? opened)))
                          (expect (false? (get (:result opened) "clean")))
                          (expect (= "x\npending\n"
                                     (slurp (io/file (get (:result opened) "root") "a.txt")))))))))
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
  draft-selected-root-test
  (it
    "drafts an added shared repository, lands only there and returns to the original project"
    (with-session
      "vis-fdraft-root"
      (fn [base env]
        (let [sibling (temp-dir "vis-fdraft-sibling")]
          (try (init-repo! sibling)
               (spit (io/file sibling "sibling.txt") "sibling repository\n")
               (git! sibling "add" "sibling.txt")
               (git! sibling "commit" "-q" "-m" "add sibling marker")
               (let [env (assoc env
                           :security-policy {:jail-enabled true
                                             :process-jail {:allow-read-write [base sibling]}}
                           :security/filesystem-roots [base sibling])
                     original-head (git! base "rev-parse" "HEAD")
                     opened (drafts/draft-create env "sibling-fix" true [sibling])
                     result (:result opened)
                     draft-root (get result "root")]

                 (expect (true? (extension/envelope-success? opened)))
                 (expect (= sibling (get result "repo_root")))
                 (expect (not= sibling draft-root))
                 (expect (= draft-root (ctx-root env)))
                 (expect (= "sibling repository\n" (slurp (io/file draft-root "sibling.txt"))))
                 (expect (= "x\n" (slurp (io/file draft-root "a.txt"))))
                 (spit (io/file draft-root "fix.txt") "isolated fix\n")
                 (expect (not (.exists (io/file sibling "fix.txt"))))
                 (expect (not (.exists (io/file base "fix.txt"))))
                 (let [approved (:result (drafts/draft-approve env "fix: update sibling"))
                       discarded (:result (drafts/draft-discard env))]

                   (expect (= "approved" (get approved "status")))
                   (expect (= "isolated fix\n" (slurp (io/file sibling "fix.txt"))))
                   (expect (= original-head (git! base "rev-parse" "HEAD")))
                   (expect (= "M a.txt" (git! base "status" "--porcelain")))
                   (expect (= "M a.txt" (git! sibling "status" "--porcelain")))
                   (expect (= base (get discarded "root")))
                   (expect (= base (ctx-root env)))))
               (finally (delete-tree! sibling))))))))

(defdescribe
  draft-selected-root-from-directory-test
  (it "uses the selected repository's backend even when the original project has no Git history"
      (with-session "vis-fdraft-from-directory"
                    (fn [_base env]
                      (let [sibling
                            (temp-dir "vis-fdraft-git-source")

                            directory
                            (temp-dir "vis-fdraft-plain-project")]

                        (try (init-repo! sibling)
                             (reset! (:workspace-atom env) (ws/change-root! (:db-info env)
                                                                            (:session/state-id env)
                                                                            directory))
                             (let [env
                                   (assoc env :security/filesystem-roots [sibling])

                                   opened
                                   (drafts/draft-create env "from-directory" true [sibling])]

                               (expect (extension/envelope-success? opened))
                               (expect (= sibling (get-in opened [:result "repo_root"])))
                               (expect (= "worktree" (get-in opened [:result "backend"])))
                               (expect (= directory
                                          (get-in (drafts/draft-discard env) [:result "root"])))
                               (expect (= directory (ctx-root env))))
                             (finally (delete-tree! sibling) (delete-tree! directory))))))))

(defdescribe
  draft-selected-root-refusal-test
  (it
    "rejects unavailable or restricted roots before changing the session or either repository"
    (with-session
      "vis-fdraft-root-refuse"
      (fn [base env]
        (let [sibling
              (temp-dir "vis-fdraft-root-candidate")

              nested
              (str (io/file sibling "nested"))

              missing
              (str (io/file sibling "missing"))]

          (try
            (init-repo! sibling)
            (.mkdirs (io/file nested))
            (let [allowed-env
                  (assoc env
                    :security/filesystem-roots [base sibling missing]
                    :security-policy {:jail-enabled true
                                      :process-jail {:allow-read-write [base sibling]}})

                  cases
                  (concat
                    [["blank" "" allowed-env] ["missing" missing allowed-env]
                     ["unlisted directory" nested allowed-env]
                     ["read-only" sibling
                      (-> allowed-env
                          (assoc :security/filesystem-roots [base])
                          (assoc-in [:security-policy :project-paths] {"sibling_path" sibling})
                          (assoc-in [:security-policy :process-jail :allow-read-write] [base])
                          (assoc-in [:security-policy :process-jail :allow-read] [sibling]))]]
                    (for [kind
                          [:deny-read :deny-write :deny-exec]

                          path
                          [sibling nested]]

                      [(str kind " " path) sibling
                       (assoc-in allowed-env [:security-policy :process-jail kind] [path])])
                    (for [policy
                          [:copy-only :not-allowed]

                          path
                          [sibling nested]]

                      [(str policy " " path) sibling
                       (assoc-in allowed-env [:security-policy :draft-policies] {path policy})]))]

              (doseq [[label requested selected-env] cases]
                (let [refused (drafts/draft-create selected-env "refused" true [requested])]
                  (expect (false? (extension/envelope-success? refused)) label)
                  (expect (= base (ctx-root env)))
                  (expect (= base (:root (ws/for-session (:db-info env) (:session/state-id env)))))
                  (expect (= 1
                             (count (re-seq #"(?m)^worktree "
                                            (git! sibling "worktree" "list" "--porcelain")))))))
              (expect (= "M a.txt" (git! base "status" "--porcelain")))
              (expect (= "M a.txt" (git! sibling "status" "--porcelain"))))
            (finally (delete-tree! sibling))))))))

(defdescribe
  draft-selected-root-policy-test
  (it
    "selects a registered project under a broad writable grant and never forks it twice"
    (with-session
      "vis-fdraft-root-policy"
      (fn [base env]
        (let [catalog
              (temp-dir "vis-fdraft-catalog")

              sibling
              (str (io/file catalog "sibling"))]

          (try (.mkdirs (io/file sibling))
               (init-repo! sibling)
               (let [env
                     (assoc env
                       :security/filesystem-roots [base catalog]
                       :security-policy {:jail-enabled true
                                         :project-paths {"sibling_path" sibling}
                                         :draft-policies {sibling :copy-and-apply}})

                     opened
                     (binding [ws/*filesystem-roots*
                               [{:trunk sibling :clone sibling :draft :copy-and-apply}]]
                       (drafts/draft-create env "registered" true [sibling]))]

                 (expect (extension/envelope-success? opened))
                 (expect (= sibling (get-in opened [:result "repo_root"])))
                 (expect (empty? (ws/extra-root-entries (ws/for-session (:db-info env)
                                                                        (:session/state-id env)))))
                 (expect (= 2
                            (count (re-seq #"(?m)^worktree "
                                           (git! sibling "worktree" "list" "--porcelain")))))
                 (expect (extension/envelope-success? (drafts/draft-discard env)))
                 (expect (= base (ctx-root env))))
               (finally (delete-tree! catalog))))))))

(defdescribe
  draft-selected-root-python-test
  (it
    "accepts lists of repository Paths through local and worker Python argument styles"
    (extension/sandbox-symbol-signatures)
    (doseq [worker? [false true]]
      (with-session
        "vis-fdraft-root-python"
        (fn [base env]
          (let [sibling (temp-dir "vis-fdraft-python-sibling")]
            (try
              (init-repo! sibling)
              (let [env (assoc env :security/filesystem-roots [base sibling])
                    ext {:ext/name "foundation-core"}
                    bindings (into {}
                                   (map
                                     (fn [entry]
                                       [(:ext.symbol/symbol entry)
                                        (fn [& args]
                                          (extension/invoke-symbol-wrapper ext entry args env))]))
                                   drafts/symbols)]

                (tpc/with-own
                  [ctx bindings nil {:worker? worker?}]
                  (let
                    [answer
                     (ep/run-python-block
                       ctx
                       (str
                         "import inspect\nfrom pathlib import Path\n"
                         "assert 'roots' in inspect.signature(draft_create).parameters\n"
                         "source = Path("
                         (pr-str sibling)
                         ")\n"
                         "sources = [source, Path(" (pr-str base)
                         ")]\n" "for create, clean in [\n"
                         "    (lambda: draft_create('python-root', roots=sources), True),\n"
                         "    (lambda: draft_create(label='python-root', roots=sources), True),\n"
                         "    (lambda: draft_create('python-root', clean=False, roots=sources), False),\n"
                         "    (lambda: draft_create('python-root', False, roots=sources), False),\n"
                         "    (lambda: draft_create('python-root', True, sources), True),\n" "]:\n"
                         "    opened = create()\n" "    try:\n"
                         "        assert opened['repo_root'] == str(source), dict(opened)\n"
                         "        assert len(opened['repositories']) == 2, dict(opened)\n"
                         "        assert opened['clean'] is clean\n"
                         "        assert draft_status()['root'] == opened['root']\n"
                         "        if clean:\n"
                         "            for sync in [lambda: draft_sync('start', 'sync test', sources), lambda: draft_sync(action='start', message='sync test', roots=sources), lambda: draft_sync('start', message='sync test', roots=sources)]:\n"
                         "                synced = sync()\n"
                         "                assert synced['status'] == 'synced', dict(synced)\n"
                         "                assert len(synced['repositories']) == 2\n"
                         "    finally:\n"
                         "        assert draft_discard()['root'] == " (pr-str base)
                         "\n" "print('selected root round trip')\n"))]
                    (expect (nil? (:error answer)) (pr-str answer))
                    (expect (= "selected root round trip\n" (:stdout answer)))))
                (expect (= base (ctx-root env))))
              (finally (delete-tree! sibling)))))))))

;; #246: recover a session mislabeled as trunk without deleting another session's draft.
(defdescribe
  draft-root-recovery-test
  (it
    "reports an inherited draft path and detaches only the affected session when drafts are off"
    (with-session
      "vis-fdrafts-recovery"
      (fn [base owner]
        (let [opened
              (:result (drafts/draft-create owner "owned"))

              draft-root
              (get opened "root")

              store
              (:db-info owner)

              wrong
              (ps/db-workspace-insert! store
                                       {:repo-id "wrong"
                                        :repo-root draft-root
                                        :root draft-root
                                        :workspace-kind :trunk
                                        :workspace-backend :live
                                        :state :active})

              soul
              (str (random-uuid))

              state-id
              (pin-session! store soul (:id wrong))

              inherited
              (assoc owner
                :session-id soul
                :session/state-id state-id
                :workspace/id (:id wrong)
                :workspace-atom (atom wrong))]

          (spit (io/file draft-root "keep.txt") "owner work")
          (binding [ws/*draft-backend* :off]
            (let [status (:result (drafts/draft-status inherited))]
              (expect (true? (get status "in_draft")))
              (expect (true? (get status "recovery_required")))
              (expect (= base (get status "repo_root")))
              (expect (= "owned" (get status "label"))))
            (let [result (drafts/draft-discard inherited)]
              (expect (extension/envelope-success? result))
              (expect (= "recovered" (get-in result [:result "status"])))
              (expect (= base (get-in result [:result "root"])))
              (expect (= draft-root (get-in result [:result "preserved_root"])))
              (expect (= base (ctx-root inherited)))
              (expect (= base (:root (ws/for-session store state-id))))
              (expect (false? (get-in (drafts/draft-status inherited) [:result "in_draft"])))))
          (expect (= draft-root (ctx-root owner)))
          (expect (= "owner work" (slurp (io/file draft-root "keep.txt"))))))))
  (it
    "allows discard hooks to veto recovery without repointing confinement or persistence"
    (with-session
      "vis-fdrafts-recovery-veto"
      (fn [_base env]
        (let [opened
              (:result (drafts/draft-create env "veto-recovery"))

              draft-root
              (get opened "root")

              store
              (:db-info env)

              wrong
              (ps/db-workspace-insert! store
                                       {:repo-id "wrong"
                                        :repo-root draft-root
                                        :root draft-root
                                        :workspace-kind :trunk
                                        :workspace-backend :live
                                        :state :active})]

          (ps/db-session-state-set-workspace! store (:session/state-id env) (:id wrong))
          (reset! (:workspace-atom env) wrong)
          (try (extension/register-op-hook!
                 {:op :draft/discard
                  :phase :around
                  :owner :ext/draft-recovery-veto-test
                  :fn (fn [_env _op _args _next]
                        (extension/failure {:error {:message "recovery vetoed for test"}}))})
               (let [result (drafts/draft-discard env)]
                 (expect (false? (extension/envelope-success? result)))
                 (expect (str/includes? (get-in result [:error :message]) "recovery vetoed"))
                 (expect (= draft-root (ctx-root env)))
                 (expect (= (:id wrong) (:id (ws/for-session store (:session/state-id env))))))
               (finally (extension/unregister-op-hooks-for-owner!
                          :ext/draft-recovery-veto-test))))))))

;; #246: unknown ownership must never authorize deleting an arbitrary directory.
(defdescribe
  unrecognized-draft-recovery-test
  (it
    "preserves unknown draft files and offers the existing session-scoped /cd recovery"
    (with-session
      "vis-fdrafts-orphan"
      (fn [base env]
        (let [directory
              (io/file ws/*drafts-home* "source" "orphan")

              _
              (.mkdirs directory)

              draft-root
              (.getCanonicalPath directory)

              store
              (:db-info env)

              wrong
              (ps/db-workspace-insert! store
                                       {:repo-id "orphan"
                                        :repo-root draft-root
                                        :root draft-root
                                        :workspace-kind :trunk
                                        :workspace-backend :live
                                        :state :active})]

          (spit (io/file directory "keep.txt") "unowned work")
          (ps/db-session-state-set-workspace! store (:session/state-id env) (:id wrong))
          (reset! (:workspace-atom env) wrong)
          (let [status
                (:result (drafts/draft-status env))

                result
                (drafts/draft-discard env)]

            (expect (true? (get status "in_draft")))
            (expect (true? (get status "recovery_required")))
            (expect (false? (get status "managed")))
            (expect (str/includes? (get-in result [:error :message]) "/cd <original-checkout>"))
            (expect (= draft-root (ctx-root env))))
          (expect (= :workspace/unrecognized-draft
                     (try (ws/create-trunk-at! store draft-root)
                          nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
          (let [cd
                (:slash/run-fn (first workspace-slashes/specs))

                result
                (cd (assoc env :command/argv [base]))]

            (expect (= :ok (:slash/status result)))
            (expect (= base (ctx-root env)))
            (expect (= base (:root (ws/for-session store (:session/state-id env))))))
          (expect (= "unowned work" (slurp (io/file directory "keep.txt")))))))))

(defdescribe
  draft-root-python-recovery-test
  (it
    "exposes recovery status and receipt through both local and worker Python"
    (extension/sandbox-symbol-signatures)
    (doseq [worker? [false true]]
      (with-session
        "vis-fdrafts-recovery-python"
        (fn [base env]
          (let [opened (:result (drafts/draft-create env "python-recovery"))
                draft-root (get opened "root")
                store (:db-info env)
                wrong (ps/db-workspace-insert! store
                                               {:repo-id "wrong"
                                                :repo-root draft-root
                                                :root draft-root
                                                :workspace-kind :trunk
                                                :workspace-backend :live
                                                :state :active})
                bindings (into {}
                               (map (fn [entry]
                                      [(:ext.symbol/symbol entry)
                                       (fn [& args]
                                         (extension/invoke-symbol-wrapper {:ext/name
                                                                           "foundation-core"}
                                                                          entry
                                                                          args
                                                                          env))]))
                               drafts/symbols)]

            (ps/db-session-state-set-workspace! store (:session/state-id env) (:id wrong))
            (reset! (:workspace-atom env) wrong)
            (tpc/with-own [ctx bindings nil {:worker? worker?}]
                          (let [answer
                                (ep/run-python-block
                                  ctx
                                  (str "status = draft_status()\n"
                                       "assert status['in_draft'] and status['recovery_required']\n"
                                       "receipt = draft_discard()\n"
                                       "assert receipt['status'] == 'recovered'\n"
                                       "assert receipt['root'] == "
                                       (pr-str base)
                                       "\n"
                                       "assert receipt['preserved_root'] == "
                                       (pr-str draft-root)
                                       "\n"
                                       "assert not draft_status()['in_draft']\n"
                                       "print('recovered')\n"))]
                            (expect (nil? (:error answer)) (pr-str answer))
                            (expect (= "recovered\n" (:stdout answer)))))
            (expect (= base (ctx-root env)))
            (expect (.isDirectory (io/file draft-root)))))))))

(defdescribe
  draft-discard-veto-test
  (it
    "a vetoed discard keeps the persisted session pinned to its draft"
    (with-session
      "vis-fdrafts-discard-veto"
      (fn [_base env]
        (let [opened
              (drafts/draft-create env "guarded-discard")

              draft-id
              (get-in opened [:result "workspace_id"])

              draft-root
              (get-in opened [:result "root"])]

          (expect (true? (extension/envelope-success? opened)))
          (expect (= draft-id (str (:id (ws/for-session (:db-info env) (:session/state-id env))))))
          (try (extension/register-op-hook!
                 {:op :draft/discard
                  :phase :around
                  :owner :ext/draft-discard-veto-test
                  :fn (fn [_env _op _args _next]
                        (extension/failure {:error {:message "discard vetoed for test"}}))})
               (let [refused
                     (drafts/draft-discard env)

                     persisted
                     (ws/for-session (:db-info env) (:session/state-id env))]

                 (expect (false? (extension/envelope-success? refused)))
                 (expect (= draft-root (ctx-root env)))
                 (expect (.exists (io/file draft-root)))
                 (expect (= draft-id (str (:id persisted)))))
               (finally (extension/unregister-op-hooks-for-owner!
                          :ext/draft-discard-veto-test))))))))

(defdescribe draft-discard-confinement-order-test
             (it "repoints persistence and live confinement before backend release starts"
                 (with-session
                   "vis-fdrafts-discard-order"
                   (fn [base env]
                     (let [opened
                           (drafts/draft-create env "ordered-discard")

                           observed
                           (atom nil)

                           release
                           (atom nil)

                           abandon!
                           ws/abandon!]

                       (expect (extension/envelope-success? opened))
                       (with-redefs [ws/abandon! (fn [db opts]
                                                   (reset! observed
                                                     {:persisted (:root (ws/for-session
                                                                          db
                                                                          (:session/state-id env)))
                                                      :confined (:root @(:workspace-atom env))})
                                                   (let [result (abandon! db opts)]
                                                     (reset! release (:discard-future result))
                                                     result))]
                         (expect (extension/envelope-success? (drafts/draft-discard env))))
                       (when-let [pending @release]
                         (deref pending 30000 nil))
                       (expect (= {:persisted base :confined base} @observed)))))))

(defdescribe draft-symbols-test
             (it "the sandbox names its six draft tools, without a draft slash command"
                 (expect (= ["draft-status" "draft-diff" "draft-create" "draft-sync" "draft-approve"
                             "draft-discard"]
                            (mapv (comp name :ext.symbol/symbol) drafts/symbols)))
                 (expect (empty? (filter #(#{"draft" "approve" "discard"} (:slash/name %))
                                         (:ext/slash-commands foundation/vis-extension))))))

(defdescribe
  draft-diff-symbol-test
  (it
    "captures durable versioned diff bytes and returns only their descriptor and checkpoint"
    (with-session
      "vis-fdraft-diff"
      (fn [_base env]
        (let [opened
              (drafts/draft-create env "review")

              root
              (get-in opened [:result "root"])

              sink
              (atom [])]

          (binding [capture/*attachment-sink* sink]
            (let [initial (drafts/draft-diff env "DIFF-feature.json")
                  _ (spit (io/file root "a.txt") "changed\n")
                  next (drafts/draft-diff env
                                          "DIFF-feature.json"
                                          (get-in initial [:result "checkpoint"]))
                  recorded (last @sink)
                  document (diff/parse! (String. (.decode (java.util.Base64/getDecoder)
                                                          ^String (:base64 recorded))
                                                 java.nio.charset.StandardCharsets/UTF_8))]

              (expect (extension/envelope-success? initial))
              (expect (true? (get-in initial [:result "empty"])))
              (expect (extension/envelope-success? next))
              (expect (= 2 (get-in next [:result "version"])))
              (expect (= "diff" (:kind recorded)))
              (expect (true? (:commentable recorded)))
              (expect (= diff/media-type (:media-type recorded)))
              (expect (str/includes? (get document "patch") "+changed"))
              (expect (not (contains? (:result next) "base64")))
              (expect (= [] (get document "comments")))
              (expect (= (get-in next [:result "checkpoint"])
                         (get-in document ["source" "head_revision"])))))))))
  (it "refuses calls without a draft, collector or a safe attachment filename"
      (with-session "vis-fdraft-diff-refuse"
                    (fn [_base env]
                      (expect (not (extension/envelope-success? (drafts/draft-diff env))))
                      (drafts/draft-create env "review")
                      (expect (not (extension/envelope-success? (drafts/draft-diff env))))
                      (binding [capture/*attachment-sink* (atom [])]
                        (expect (not (extension/envelope-success?
                                       (drafts/draft-diff env "../other.json")))))))))

(defdescribe
  draft-diff-activity-test
  (it
    "declares running progress and preserves successful, empty and failed captures"
    (let [declared (:ext.symbol/activity drafts/draft-diff-symbol)]
      (expect (= "Capture draft diff" (:headline declared)))
      (expect (true? (:show-start declared)))
      (expect (= (:headline (presenter/for-tool :draft_diff)) (:headline declared)))
      (doseq [empty? [false true]]
        (let [ctx (event/context)
              invocation (event/invocation ctx nil)
              details {:operation :draft_diff
                       :presenter :generic
                       :activity declared
                       :started-at-ms (System/currentTimeMillis)}
              start (event/start-event ctx invocation details)
              result {"filename" "DIFF-feature.json"
                      "version" 2
                      "size" 123
                      "checkpoint" "snapshot-tree"
                      "empty" empty?}
              terminal (event/terminal-event ctx
                                             invocation
                                             (assoc details
                                               :outcome :succeeded
                                               :result result))
              projection (activity/presentation (activity/replay [start terminal]))
              row (first (:rows projection))
              rendered (pr-str (:presentation row))]

          (expect (= "Capture draft diff" (get-in start [:presentation "headline"])))
          (expect (= "succeeded" (:state row)))
          (expect (contract/valid-projection? projection))
          (expect (str/includes? rendered "Captured draft diff"))
          (expect (str/includes? rendered "DIFF-feature.json"))
          ;; Routine draft results use the compact Activity summary.
          (expect (str/includes? rendered (if empty? "No changes" "Diff attached")))
          (expect (= [] (get-in row [:presentation "content"])))))
      (let [ctx (event/context)
            invocation (event/invocation ctx nil)
            details {:operation :draft_diff
                     :presenter :generic
                     :activity declared
                     :started-at-ms (System/currentTimeMillis)}
            start (event/start-event ctx invocation details)
            terminal (event/terminal-event ctx
                                           invocation
                                           (assoc details
                                             :outcome :failed
                                             :error (ex-info "No draft is active" {})))
            projection (activity/presentation (activity/replay [start terminal]))
            row (first (:rows projection))]

        (expect (= "failed" (:state row)))
        (expect (= "No draft is active" (:error-summary row)))
        (expect (contract/valid-projection? projection))))))

(defdescribe
  multi-repository-review-test
  (it
    "keeps #241 colliding paths separate and validates all checkpoints before attaching"
    (with-session
      "vis-group-review"
      (fn [base env]
        (let [sibling
              (temp-dir "vis-group-sibling")

              shared
              (temp-dir "vis-group-shared")]

          (try (init-repo! sibling)
               (init-repo! shared)
               (let [env
                     (assoc env
                       :security-policy {:jail-enabled true
                                         :process-jail {:allow-read-write [base sibling shared]}}
                       :security/filesystem-roots [base sibling shared])

                     opened
                     (drafts/draft-create env "group-review" false [base sibling])

                     sink
                     (atom [])]

                 (expect (extension/envelope-success? opened))
                 (binding [capture/*attachment-sink* sink]
                   (let [initial (drafts/draft-diff env)
                         checkpoint (get-in initial [:result "checkpoint"])
                         repositories (ws/draft-roots @(:workspace-atom env))]

                     (expect (extension/envelope-success? initial))
                     (expect (true? (get-in initial [:result "empty"])))
                     (expect (= #{base sibling} (set (keys checkpoint))))
                     (doseq [[index repository] (map-indexed vector repositories)]
                       (spit (io/file (:root repository) "a.txt") (str "task " index "\n")))
                     (let [review (drafts/draft-diff env "DIFF-group.json" checkpoint)
                           documents (mapv #(diff/parse! (String.
                                                           (.decode (java.util.Base64/getDecoder)
                                                                    ^String (:base64 %))
                                                           java.nio.charset.StandardCharsets/UTF_8))
                                           (take-last 2 @sink))
                           next-checkpoint (get-in review [:result "checkpoint"])]

                       (expect (extension/envelope-success? review))
                       (expect (= 2 (get-in review [:result "repository_count"])))
                       (expect (= 2 (count (set (map #(get-in % ["source" "label"]) documents)))))
                       (expect (every? #(str/includes? (get % "patch") "a.txt") documents))
                       (expect (true? (get-in
                                        (drafts/draft-diff env "DIFF-next.json" next-checkpoint)
                                        [:result "empty"])))
                       (doseq [invalid [(dissoc next-checkpoint sibling)
                                        (assoc next-checkpoint sibling "not-a-checkpoint")]]
                         (let [before (count @sink)]
                           (expect (not (extension/envelope-success?
                                          (drafts/draft-diff env "DIFF-invalid.json" invalid))))
                           (expect (= before (count @sink)))))))))
               (finally (delete-tree! sibling) (delete-tree! shared))))))))

(defdescribe draft-sync-activity-test
             (it "declares running progress and preserves sync outcomes, failures and empty results"
                 (let [declared (:ext.symbol/activity drafts/draft-sync-symbol)]
                   (expect (= "Synchronize draft" (:headline declared)))
                   (expect (true? (:show-start declared)))
                   (doseq [[outcome result error expected]
                           [[:succeeded {:status "synced" :repositories [{:status "synced"}]} nil
                             "Synchronized"]
                            [:succeeded {:status "conflicts" :repositories [{:conflicts ["a.txt"]}]}
                             nil "Resolve conflicts"]
                            [:succeeded {:status "aborted" :repositories [{:status "aborted"}]} nil
                             "Synchronization aborted"] [:succeeded nil nil "No draft result"]
                            [:failed nil (ex-info "Resolve the owned merge first" {})
                             "Resolve the owned merge first"]]]
                     (let [ctx (event/context)
                           invocation (event/invocation ctx nil)
                           details {:operation :draft_sync
                                    :presenter :generic
                                    :activity declared
                                    :started-at-ms (System/currentTimeMillis)}
                           start (event/start-event ctx invocation details)
                           terminal (event/terminal-event ctx
                                                          invocation
                                                          (assoc details
                                                            :outcome outcome
                                                            :result result
                                                            :error error))
                           projection (activity/presentation (activity/replay [start terminal]))]

                       (expect (= "Synchronize draft" (get-in start [:presentation "headline"])))
                       (expect (contract/valid-projection? projection))
                       (expect (str/includes? (pr-str projection) expected)))))))

(defdescribe draft-sync-failure-test
             (it "preserves actionable #243 failures and the session pin"
                 (with-session
                   "vis-sync-failure"
                   (fn [_base env]
                     (expect (extension/envelope-success? (drafts/draft-create env "sync-failure")))
                     (let [before (ctx-root env)]
                       (doseq [result [{:status :partial
                                        :repositories
                                        [{:status :failed :repo-root before :error "blocked"}]}
                                       (ex-info "Synchronization refused"
                                                {:type :draft/sync-in-progress
                                                 :hint "Use draft_sync(action=abort)."})]]
                         (with-redefs [lifecycle/sync!
                                       (fn [_ _]
                                         (if (instance? Throwable result) (throw result) result))]
                           (let [refused (drafts/draft-sync env)]
                             (expect (not (extension/envelope-success? refused)))
                             (expect (map? (get-in refused [:error :details])))
                             (expect (= before (ctx-root env)))
                             (expect (= before
                                        (:root (ws/for-session (:db-info env)
                                                               (:session/state-id env)))))))))))))
