(ns com.blockether.vis.ext.foundation-bridge.core-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [bridge.api :as br]
            [bridge.io :as bio]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.env-python :as boundary]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.ext.foundation-bridge.core :as bridge]))

(defn- result-of
  "The tool envelope's `:result` payload VIEWED through the STRINGS-ONLY
   boundary. `boundary-view` passes string-keyed maps through verbatim and
   THROWS on any stray keyword key/value, so reading a bridge result through it
   also guards that the payload crosses to Python string-clean."
  [envelope]
  (boundary/boundary-view (:result envelope)))

(defn- temp-root
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- write-policy!
  [root sandbox]
  (bio/write-data (str root "/.bridge/verification-policy.yaml")
                  {:artifact "verification-policy"
                   :policy-id "sandboxed"
                   :bridge-path-sandbox sandbox
                   :rules []}))

(defn- mark-repository! [root] (.mkdirs (io/file root ".git")) root)

(defn- configure-project!
  [workspace-root project-root]
  (mark-repository! project-root)
  (bridge/init {:workspace/root workspace-root} {"root" project-root})
  (str project-root "/.bridge/profile.yaml"))

(defn- bridge-context
  [root]
  (boundary/boundary-view ((:ext/ctx-fn bridge/vis-extension) {:workspace/root root})))

(defn- protected-env [root] {:workspace/root root :extensions (atom [bridge/vis-extension])})

(def ^:private bridge-commit-gate (var-get #'bridge/bridge-commit-gate))

(defdescribe
  bridge-commit-gate-test
  (it "is declared as lifecycle-owned semantic middleware"
      ;; The commit gate is `:around` middleware; the path sandbox beside it is a
      ;; GATE, which is asked and never wraps, so it declares no phase.
      (expect (= [{:op :git/commit :phase :around :fn bridge-commit-gate}
                  {:op :fs/access :fn (var-get #'bridge/bridge-fs-access-gate)}]
                 (:ext/op-hooks bridge/vis-extension))))
  (it "allows an ordinary commit when Bridge is not configured"
      (let
        [root
         (temp-root "bridge-ext-no-gate")

         called
         (atom nil)

         result
         (bridge-commit-gate {:root root :candidate-tree "candidate" :index-preserving? true}
                             :git/commit
                             []
                             (fn [args]
                               (reset! called args)
                               :committed))]

        (expect (= :committed result))
        (expect (= [] @called))))
  (it
    "approves only the exact semantic candidate supplied by Vis"
    (let
      [root
       (temp-root "bridge-ext-gate-candidate")

       _
       (configure-project! root root)

       seen-opts
       (atom nil)

       context
       {:root root :candidate-tree "candidate" :index-preserving? true}]

      (with-redefs
        [br/check (fn [_profile opts]
                    (reset! seen-opts opts)
                    {:status "clear"
                     :issue-count 0
                     :change-detection {:candidate-tree "candidate"
                                        :approval {:status "approved"}}})]
        (expect (= :committed (bridge-commit-gate context :git/commit [] (constantly :committed))))
        (expect (true? (:index? @seen-opts)))
        (expect (true? (:approve? @seen-opts))))
      (with-redefs
        [br/check (fn [_profile _opts]
                    {:status "clear"
                     :issue-count 0
                     :change-detection {:candidate-tree "different"
                                        :approval {:status "approved"}}})]
        (let
          [error (try (bridge-commit-gate context :git/commit [] (constantly :committed))
                      nil
                      (catch clojure.lang.ExceptionInfo e e))]
          (expect (some? error))
          (expect (str/includes? (ex-message error) "candidate changed"))))))
  (it "ignores configured child repositories when the commit root is unconfigured"
      (let
        [root
         (mark-repository! (temp-root "bridge-ext-parent-commit"))

         child
         (str root "/nested")

         _
         (configure-project! root child)

         called
         (atom nil)]

        (with-redefs
          [br/check (fn [& _]
                      (throw (ex-info "child has unmet obligations"
                                      {:type :test/child-obligations})))]
          (expect (= :committed
                     (bridge-commit-gate
                       {:root root :candidate-tree "parent-candidate" :index-preserving? true}
                       :git/commit
                       []
                       (fn [args]
                         (reset! called args)
                         :committed))))
          (expect (= [] @called)))))
  (it
    "gates commits made directly in a configured child repository"
    (let
      [parent
       (mark-repository! (temp-root "bridge-ext-parent-with-child"))

       child
       (str parent "/nested")

       _
       (configure-project! parent child)

       check-called?
       (atom false)

       commit-called?
       (atom false)]

      (with-redefs
        [br/check (fn [_profile _opts]
                    (reset! check-called? true)
                    {:status "blocked"
                     :issue-count 1
                     :change-detection {:candidate-tree "child-candidate"
                                        :approval {:status "pending"}}})]
        (let
          [error (try (bridge-commit-gate
                        {:root child :candidate-tree "child-candidate" :index-preserving? true}
                        :git/commit
                        []
                        (fn [_args]
                          (reset! commit-called? true)
                          :committed))
                      nil
                      (catch clojure.lang.ExceptionInfo e e))]
          (expect (true? @check-called?))
          (expect (false? @commit-called?))
          (expect (= :vis.bridge/commit-not-approved (:type (ex-data error)))))))))

(defdescribe
  bridge-extension-test
  (it "configures the extension"
      (expect (= 'br (get-in bridge/vis-extension [:ext/engine :ext.engine/alias])))
      (expect (= '#{init profile check list-evidence run-evidence}
                 (set (map :ext.symbol/symbol
                           (get-in bridge/vis-extension [:ext/engine :ext.engine/symbols])))))
      (expect (= #{"init" "profile" "check" "list-evidence" "run-evidence"}
                 (set (map :cmd/name (get-in bridge/vis-extension [:ext/cli 0 :cmd/subcommands])))))
      (expect (fn? (:ext/ctx-fn bridge/vis-extension)))
      (expect (nil? (:ext/hooks bridge/vis-extension)))
      (expect (= :git/commit (get-in bridge/vis-extension [:ext/op-hooks 0 :op])))
      ;; The path sandbox is a GATE hook now: asked before every path the engine
      ;; and the Python interpreter touch, rather than a table the engine reads.
      (expect (= :fs/access (get-in bridge/vis-extension [:ext/op-hooks 1 :op])))
      (expect (= :observation (vis/op-tag :br/check)))
      (expect (= :mutation (vis/op-tag :br/init)))
      (expect (= :mutation (vis/op-tag :br/run-evidence))))
  (it "emits concise routing only in configured workspaces"
      (let
        [root
         (temp-root "bridge-ext-prompt")

         env
         {:workspace/root root}

         prompt-fn
         (:ext/prompt-fn bridge/vis-extension)]

        (expect (nil? (prompt-fn env)))
        (bridge/init env)
        (let [prompt (prompt-fn env)]
          (expect (str/includes? prompt "br_check"))
          (expect (str/includes? prompt "next_action"))
          (expect (not (str/includes? prompt "br_next")))
          (expect (str/includes? prompt "doc(name)"))
          (expect (not (str/includes? prompt "required_obligations")))
          (expect (< (count prompt) 400))))))

(defdescribe bridge-toggle-test
             (it "registers a persistent default-on toggle that controls activation"
                 (let
                   [spec
                    (vis/toggle-spec "bridge")

                    activation-fn
                    (:ext/activation-fn bridge/vis-extension)

                    original
                    (vis/toggle-enabled? "bridge")]

                   (expect (= {:id "bridge"
                               :label "Bridge verification"
                               :default true
                               :persist? true
                               :group :extensions}
                              (select-keys spec [:id :label :default :persist? :group])))
                   (try (expect (true? (activation-fn {})))
                        (vis/toggle-set-enabled! "bridge" false)
                        (expect (false? (activation-fn {})))
                        (finally (vis/toggle-set-enabled! "bridge" original)))))
             ;; Regression, this report: disabling Bridge still left its commit-verification hook active.
             (it "does not run the commit gate while Bridge is disabled"
                 (let
                   [root
                    (temp-root "bridge-ext-disabled-gate")

                    _
                    (configure-project! root root)

                    original
                    (vis/toggle-enabled? "bridge")

                    check-called?
                    (atom false)]

                   (try (vis/toggle-set-enabled! "bridge" false)
                        (with-redefs
                          [br/check (fn [& _]
                                      (reset! check-called? true)
                                      (throw (ex-info "disabled Bridge hook ran" {})))]
                          (expect (= :committed
                                     (extension/invoke-operation :git/commit
                                                                 {:root root
                                                                  :candidate-tree "candidate"
                                                                  :index-preserving? true}
                                                                 (constantly :committed)
                                                                 [])))
                          (expect (false? @check-called?)))
                        (finally (vis/toggle-set-enabled! "bridge" original))))))

;; Regression, issue: bridge silently fell back to `(System/getProperty "user.dir")`
;; when `:workspace/root` was missing from env, instead of failing loud like every
;; other extension (clj/py/ts). A host bug that forgot to thread :workspace/root
;; would silently operate against the JVM process cwd.
(defdescribe bridge-missing-workspace-root-test
             (it "throws instead of falling back to the process cwd"
                 (let [ex (try (bridge/check {}) nil (catch clojure.lang.ExceptionInfo e e))]
                   (expect (some? ex))
                   (expect (= :vis.bridge/missing-workspace-root (:type (ex-data ex)))))))

(defdescribe
  bridge-session-projects-test
  (it "omits the Bridge session slice when no configured project is known"
      (expect (= {} (bridge-context (temp-root "bridge-ext-session-empty")))))
  (it
    "discovers one configured child repository and defaults bare operations to it"
    (let
      [root
       (temp-root "bridge-ext-session-one")

       child
       (str root "/project")

       _
       (mark-repository! child)

       before
       (bridge-context root)

       profile-path
       (configure-project! root child)

       context
       (bridge-context root)

       bridge-slice
       (get-in context ["session_env" "bridge"])

       check-result
       (bridge/check {:workspace/root root})]

      ;; Repository roots are cached, but profile files are probed on every
      ;; contribution, so configuring a known repo is visible immediately.
      (expect (= {} before))
      (expect (= [{"root" child "profile_path" profile-path}] (get bridge-slice "projects")))
      (expect (= profile-path (get bridge-slice "default_profile_path")))
      (expect (not (contains? bridge-slice "discovery_truncated")))
      (expect (true? (:success? check-result)))
      (expect (= profile-path (get-in (result-of check-result) ["profile_path"])))))
  (it
    "requires explicit selection when multiple child repositories are configured"
    (let
      [root
       (temp-root "bridge-ext-session-many")

       alpha
       (str root "/alpha")

       beta
       (str root "/beta")

       _
       (mark-repository! alpha)

       _
       (mark-repository! beta)

       alpha-profile
       (configure-project! root alpha)

       beta-profile
       (configure-project! root beta)

       bridge-slice
       (get-in (bridge-context root) ["session_env" "bridge"])

       ambiguous
       (bridge/check {:workspace/root root})

       guard-result
       (bridge-commit-gate {:root root :candidate-tree "candidate" :index-preserving? true}
                           :git/commit
                           []
                           (constantly :committed))

       selected
       (bridge/check {:workspace/root root} {"profile" beta-profile})]

      (expect (= [alpha-profile beta-profile]
                 (mapv #(get % "profile_path") (get bridge-slice "projects"))))
      (expect (not (contains? bridge-slice "default_profile_path")))
      (expect (false? (:success? ambiguous)))
      (expect (str/includes? (get-in ambiguous [:error :message]) "Multiple Bridge projects"))
      (expect (= :committed guard-result))
      (expect (= 2 (count (get-in ambiguous [:error :details :projects]))))
      (expect (true? (:success? selected)))
      (expect (= beta-profile (get-in (result-of selected) ["profile_path"])))
      (expect (str/includes? ((:ext/prompt-fn bridge/vis-extension) {:workspace/root root})
                             "profile"))))
  (it "gives the active-root profile precedence over configured nested repositories"
      (let
        [root
         (temp-root "bridge-ext-session-root")

         nested
         (str root "/nested")

         _
         (mark-repository! root)

         _
         (mark-repository! nested)

         root-profile
         (configure-project! root root)

         _
         (configure-project! root nested)

         bridge-slice
         (get-in (bridge-context root) ["session_env" "bridge"])

         result
         (bridge/profile {:workspace/root root})]

        (expect (= root-profile (get bridge-slice "default_profile_path")))
        (expect (true? (:success? result)))
        (expect (= root-profile (get-in (result-of result) ["profile_path"])))))
  (it "disables sole-project defaulting when repository discovery is truncated"
      (let
        [root
         (temp-root "bridge-ext-session-truncated")

         child
         (str root "/project")

         profile-path
         (configure-project! root child)

         inventory
         {:root root :count 1 :repositories [{:path "project" :root child}] :truncated? true}]

        (with-redefs [vis/repository-inventory (constantly inventory)]
          (let
            [bridge-slice (get-in (bridge-context root) ["session_env" "bridge"])
             ambiguous (bridge/check {:workspace/root root})
             selected (bridge/check {:workspace/root root} {"profile" profile-path})]

            (expect (= true (get bridge-slice "discovery_truncated")))
            (expect (not (contains? bridge-slice "default_profile_path")))
            (expect (false? (:success? ambiguous)))
            (expect (str/includes? (get-in ambiguous [:error :message]) "truncated"))
            (expect (true? (:success? selected))))))))

(defdescribe bridge-multirepo-init-test
             (it
               "refuses bare initialization in a non-repository parent and accepts an explicit root"
               (let
                 [root
                  (temp-root "bridge-ext-init-parent")

                  child
                  (mark-repository! (str root "/project"))

                  bare
                  (bridge/init {:workspace/root root})

                  explicit
                  (bridge/init {:workspace/root root} {"root" child})]

                 (expect (false? (:success? bare)))
                 (expect (str/includes? (get-in bare [:error :message]) "explicit project root"))
                 (expect (= [child] (get-in bare [:error :details :repository-roots])))
                 (expect (true? (:success? explicit)))
                 (expect (= child (get-in (result-of explicit) ["workspace_root"])))))
             (it "initializes the active repository instead of adopting a configured nested profile"
                 (let
                   [root
                    (temp-root "bridge-ext-init-active")

                    nested
                    (str root "/nested")

                    _
                    (mark-repository! root)

                    _
                    (mark-repository! nested)

                    nested-profile
                    (configure-project! root nested)

                    result
                    (bridge/init {:workspace/root root})

                    root-profile
                    (str root "/.bridge/profile.yaml")]

                   (expect (true? (:success? result)))
                   (expect (= false (get-in (result-of result) ["already_configured"])))
                   (expect (= root-profile (get-in (result-of result) ["profile_path"])))
                   (expect (not= nested-profile (get-in (result-of result) ["profile_path"]))))))

(def ^:private fs-access-gate
  (->> (:ext/op-hooks bridge/vis-extension)
       (some (fn [hook]
               (when (= :fs/access (:op hook)) (:fn hook))))))

(defn- refusal
  "What the gate answers for one operation on one absolute path: the refusal
   sentence, or nil when the Bridge allows it."
  [root operation path]
  (fs-access-gate (protected-env root) :fs/access {:operation operation :path path}))

(defdescribe
  bridge-fs-access-gate-test
  (it "allows everything when Bridge is unconfigured"
      (let [root (temp-root "bridge-ext-protected-unconfigured")]
        (expect (nil? (refusal root "file-write" (str root "/.bridge/profile.yaml"))))))
  (it "allows everything when policy enforcement is disabled"
      (let
        [root
         (temp-root "bridge-ext-protected-disabled")

         env
         {:workspace/root root}]

        (bridge/init env)
        (write-policy! root {:enforce? false :rules [{:path-pattern ".bridge/" :access "none"}]})
        (expect (nil? (refusal root "file-write" (str root "/.bridge/profile.yaml"))))))
  (it "an enforced :none rule refuses the read and the write with the policy's own reason"
      (let
        [root
         (temp-root "bridge-ext-protected")

         env
         {:workspace/root root}

         reason
         "Use br/* tools for Bridge-owned state."]

        (bridge/init env)
        (write-policy! root
                       {:enforce? true
                        :default-access "read-write"
                        :rules [{:path-pattern ".bridge/" :access "none" :reason reason}]})
        (expect (= reason (refusal root "file-write" (str root "/.bridge/profile.yaml"))))
        (expect (= reason (refusal root "file-read" (str root "/.bridge/profile.yaml"))))
        ;; A file the policy says nothing about is not the Bridge's business, and
        ;; neither is a path outside the workspace.
        (expect (nil? (refusal root "file-write" (str root "/src/app.clj"))))
        (expect (nil? (refusal root "file-write" "/tmp/elsewhere.txt")))))
  (it "a :read-only rule refuses the write and allows the read"
      (let
        [root
         (temp-root "bridge-ext-protected-read-only")

         env
         {:workspace/root root}

         hint
         "Policy changes require human approval."]

        (bridge/init env)
        (write-policy! root
                       {:enforce? true
                        :rules [{:path-pattern ".bridge/verification-policy.yaml"
                                 :access "read-only"
                                 :reason hint}]})
        (expect (= hint (refusal root "file-write" (str root "/.bridge/verification-policy.yaml"))))
        (expect (nil? (refusal root "file-read" (str root "/.bridge/verification-policy.yaml"))))))
  (it "prefixes policy patterns when the Bridge profile root is below the workspace"
      (let
        [root
         (temp-root "bridge-ext-protected-subroot")

         project-root
         (str root "/project")

         env
         {:workspace/root root}

         profile-path
         (str root "/.bridge/profile.yaml")]

        (bridge/init env)
        (.mkdirs (java.io.File. project-root ".bridge"))
        (bio/write-data profile-path (assoc (bio/read-data profile-path) :root-path "../project"))
        (bio/write-data (str project-root "/.bridge/verification-policy.yaml")
                        {:artifact "verification-policy"
                         :policy-id "sandboxed-subroot"
                         :bridge-path-sandbox {:enforce? true
                                               :rules [{:path-pattern ".bridge/" :access "none"}]}
                         :rules []})
        (expect
          (=
            "Bridge policy protects this path; use the br/* tool surface instead of direct file IO."
            (refusal root "file-write" (str root "/project/.bridge/state.edn"))))
        (expect (nil? (refusal root "file-write" (str root "/.bridge/state.edn"))))))
  (it "the nested project's rule wins over its ancestor's"
      (let
        [root
         (temp-root "bridge-ext-protected-projects")

         nested
         (str root "/nested")

         _
         (mark-repository! root)

         _
         (mark-repository! nested)

         _
         (configure-project! root root)

         _
         (configure-project! root nested)]

        (write-policy! root
                       {:enforce? true
                        :rules [{:path-pattern ".bridge/" :access "read-only" :reason "ancestor"}]})
        (write-policy! nested
                       {:enforce? true
                        :rules [{:path-pattern ".bridge/" :access "none" :reason "nested"}]})
        (expect (= "nested" (refusal root "file-read" (str root "/nested/.bridge/state.edn"))))
        (expect (= "ancestor" (refusal root "file-write" (str root "/.bridge/state.edn"))))
        (expect (nil? (refusal root "file-read" (str root "/.bridge/state.edn")))))))

(defdescribe
  bridge-unconfigured-workspace-test
  (it
    "can initialize an unconfigured workspace and run the CLI"
    (let
      [root
       (str (java.nio.file.Files/createTempDirectory
              "bridge-ext-test"
              (make-array java.nio.file.attribute.FileAttribute 0)))

       _
       (spit (str root "/deps.edn") "{:aliases {:test {}}}")

       env
       {:workspace/root root}

       init-result
       (bridge/init env)

       profile-result
       (bridge/profile env)

       check-result
       (bridge/check env)

       list-result
       (bridge/list-evidence env)

       run-result
       (bridge/run-evidence env "unit" {"is_dry_run" true})]

      (expect (true? (:success? init-result)))
      (expect (= true (get-in (result-of init-result) ["configured"])))
      (expect (= false (get-in (result-of init-result) ["already_configured"])))
      (expect (= [".bridge/profile.yaml" ".bridge/verification-policy.yaml"]
                 (get-in (result-of init-result) ["created"])))
      (expect (= [".gitignore"] (get-in (result-of init-result) ["updated"])))
      (expect (= "br/check" (get-in (result-of init-result) ["next_step" "op" "tool"])))
      (expect (str/includes? (or (get-in (result-of init-result) ["profile_path"]) "")
                             ".bridge/profile.yaml"))
      (expect (true? (:success? profile-result)))
      (expect (= true (get-in (result-of profile-result) ["configured"])))
      (expect (true? (:success? check-result)))
      (expect (not= "unconfigured" (get-in (result-of check-result) ["status"])))
      (expect (true? (:success? list-result)))
      (expect (vector? (get-in (result-of list-result) ["commands"])))
      (expect (true? (:success? run-result)))
      (expect (= true (get-in (result-of run-result) ["result" "dry_run"]))))))

(defdescribe
  bridge-no-profile-error-test
  (it "returns an error when no profile is configured"
      (let
        [root
         (str (java.nio.file.Files/createTempDirectory
                "bridge-ext-no-profile"
                (make-array java.nio.file.attribute.FileAttribute 0)))

         env
         {:workspace/root root}

         run-result
         (bridge/run-evidence env "unit" {"is_dry_run" true})]

        (expect (false? (:success? run-result)))
        (expect (not (str/includes? (or (get-in run-result [:error :hint]) "") "bb bridge")))
        (expect (str/includes? (or (get-in run-result [:error :hint]) "") "br_init()")))))

(defdescribe bridge-init-idempotent-test
             (it "init is iempotent"
                 (let
                   [env
                    {:workspace/root (str (java.nio.file.Files/createTempDirectory
                                            "bridge-ext-idempotent"
                                            (make-array java.nio.file.attribute.FileAttribute 0)))}

                    first-result
                    (bridge/init env)

                    second-result
                    (bridge/init env)]

                   (expect (true? (:success? first-result)))
                   (expect (= false (get-in (result-of first-result) ["already_configured"])))
                   (expect (true? (:success? second-result)))
                   (expect (= true (get-in (result-of second-result) ["already_configured"])))
                   (expect (= [] (get-in (result-of second-result) ["created"])))
                   (expect (= [] (get-in (result-of second-result) ["updated"])))
                   (expect (str/includes? (get-in (result-of second-result) ["message"])
                                          "already configured")))))

(defdescribe
  bridge-check-flattens-status-test
  (it
    "check returns flattened status summary"
    (let
      [root
       (str (java.nio.file.Files/createTempDirectory
              "bridge-ext-flatten"
              (make-array java.nio.file.attribute.FileAttribute 0)))

       _
       (spit (str root "/deps.edn") "{:aliases {:test {}}}")

       _
       (.mkdirs (java.io.File. root "src"))

       _
       (spit (str root "/src/core.clj") "(ns core)")

       env
       {:workspace/root root}

       _
       (bridge/init env)

       _
       (bio/write-data (str root "/.bridge/ephemeral/evidence/unit.yaml")
                       {:artifact "evidence-run"
                        :evidence-id "unit"
                        :kind "unit-tests"
                        :role "regression"
                        :subject "vis"
                        :evidence-status "failed"
                        :execution-status "execution-failed"
                        :finished-at "2026-05-20T19:01:06.340575Z"
                        :command "clojure -M:test"})

       result
       (bridge/check env {"changed_files" ["src/core.clj"]})

       r
       (result-of result)]

      (expect (true? (:success? result)))
      (expect (= "attention-required" (get-in r ["status"])))
      (expect (= 1 (get-in r ["summary_version"])))
      (expect (= 1 (get-in r ["counts" "required_obligations"])))
      (expect (= 1 (get-in r ["counts" "receipts"])))
      (expect (= "bridge/run-evidence" (get-in r ["next_action" "op"])))
      (expect (= "unit" (get-in r ["next_action" "args" "id"])))
      (expect (= "unit" (get-in r ["next_action" "evidence_id"])))
      (expect (= "failed" (get-in r ["evidence_receipts" 0 "status"])))
      (expect (= "unit-tests" (get-in r ["required_obligations" 0 "evidence_kind"]))))))
