(ns com.blockether.vis.ext.foundation-bridge.core-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
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
  (str (java.nio.file.Files/createTempDirectory prefix
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))))

(defn- write-policy!
  [root sandbox]
  (bio/write-data (str root "/.bridge/verification-policy.yaml")
                  {:artifact "verification-policy"
                   :policy-id "sandboxed"
                   :bridge-path-sandbox sandbox
                   :rules []}))

(defn- protected-env [root] {:workspace/root root :extensions (atom [bridge/vis-extension])})

(defdescribe
  bridge-extension-test
  (it
    "configures the extension"
    (expect (= 'br (get-in bridge/vis-extension [:ext/engine :ext.engine/alias])))
    (expect (= '#{init profile check list-evidence run-evidence}
               (set (map :ext.symbol/symbol
                         (get-in bridge/vis-extension [:ext/engine :ext.engine/symbols])))))
    (expect (= #{"init" "profile" "check" "list-evidence" "run-evidence"}
               (set (map :cmd/name
                         (get-in bridge/vis-extension [:ext/cli 0 :cmd/subcommands])))))
    (expect (fn? (:ext/protected-paths bridge/vis-extension)))
    (expect (nil? (:ext/hooks bridge/vis-extension)))
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

(defdescribe
  bridge-protected-paths-test
  (it "returns no protected path rules when Bridge is unconfigured"
      (let [root (temp-root "bridge-ext-protected-unconfigured")]
        (expect (= [] ((:ext/protected-paths bridge/vis-extension) (protected-env root))))
        (expect (= [] (extension/active-protected-globs (protected-env root))))))
  (it "returns no protected path rules when policy enforcement is disabled"
      (let
        [root
         (temp-root "bridge-ext-protected-disabled")

         env
         {:workspace/root root}]

        (bridge/init env)
        (write-policy! root {:enforce? false :rules [{:path-pattern ".bridge/" :access "none"}]})
        (expect (= [] ((:ext/protected-paths bridge/vis-extension) (protected-env root))))))
  (it
    "maps enforced Bridge path sandbox rules to Vis protected path rules"
    (let
      [root
       (temp-root "bridge-ext-protected")

       env
       {:workspace/root root}

       hint
       "Policy changes require human approval."]

      (bridge/init env)
      (write-policy!
        root
        {:enforce? true
         :default-access "read-write"
         :rules
         [{:path-pattern ".bridge/" :access "none" :reason "Use br/* tools for Bridge-owned state."}
          {:path-pattern ".bridge/verification-policy.yaml" :access "read-only" :reason hint}
          {:path-pattern ".bridge/ephemeral/evidence/**" :access "read-write"}]})
      (expect
        (=
          [{:glob ".bridge/**" :access :none :hint "Use br/* tools for Bridge-owned state."}
           {:glob ".bridge/verification-policy.yaml" :access :read-only :hint hint}
           {:glob ".bridge/ephemeral/evidence/**"
            :access :read-write
            :hint
            "Bridge policy protects this path; use the br/* tool surface instead of direct file IO."}]
          ((:ext/protected-paths bridge/vis-extension) (protected-env root))))
      (expect
        (=
          [{:glob ".bridge/**"
            :access :none
            :hint "Use br/* tools for Bridge-owned state."
            :extension/name "foundation-bridge"}
           {:glob ".bridge/verification-policy.yaml"
            :access :read-only
            :hint hint
            :extension/name "foundation-bridge"}
           {:glob ".bridge/ephemeral/evidence/**"
            :access :read-write
            :hint
            "Bridge policy protects this path; use the br/* tool surface instead of direct file IO."
            :extension/name "foundation-bridge"}]
          (extension/active-protected-globs (protected-env root))))))
  (it
    "prefixes policy patterns when the Bridge profile root is below the workspace"
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
          [{:glob "project/.bridge/**"
            :access :none
            :hint
            "Bridge policy protects this path; use the br/* tool surface instead of direct file IO."}]
          ((:ext/protected-paths bridge/vis-extension) (protected-env root)))))))

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
