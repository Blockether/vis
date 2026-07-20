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

(defn- run-in!
  [root & args]
  (let [pb (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String args))]
    (.directory pb (java.io.File. ^String root))
    (let
      [proc (.start pb)
       exit (.waitFor proc)]

      (expect (zero? exit))
      exit)))

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
    (expect (= '#{init profile check next list-evidence run-evidence}
               (set (map :ext.symbol/symbol
                         (get-in bridge/vis-extension [:ext/engine :ext.engine/symbols])))))
    (expect (fn? (:ext/protected-paths bridge/vis-extension)))
    (expect
      (=
        [{:id :vis.bridge/next
          :doc
          "Hint the model about the next Bridge action when a configured workspace has open evidence work. Silent when Bridge is not configured."
          :phase :turn.iteration/start
          :lifetime :turn
          :fn (get-in bridge/vis-extension [:ext/hooks 0 :fn])}]
        (get-in bridge/vis-extension [:ext/hooks])))
    (expect (= :observation (vis/op-tag :br/check)))
    (expect (= :observation (vis/op-tag :br/next)))
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
          (expect (str/includes? prompt "br_next"))
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
       (str root "/.bridge/profile.edn")]

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

       next-result
       (bridge/next env)

       list-result
       (bridge/list-evidence env)

       run-result
       (bridge/run-evidence env "unit" {"is_dry_run" true})]

      (expect (true? (:success? init-result)))
      (expect (= true (get-in (result-of init-result) ["configured"])))
      (expect (= false (get-in (result-of init-result) ["already_configured"])))
      (expect (= [".bridge/profile.edn" ".bridge/verification-policy.yaml"]
                 (get-in (result-of init-result) ["created"])))
      (expect (= [".gitignore"] (get-in (result-of init-result) ["updated"])))
      (expect (= "br/check" (get-in (result-of init-result) ["next_step" "op" "tool"])))
      (expect (str/includes? (or (get-in (result-of init-result) ["profile_path"]) "")
                             ".bridge/profile.edn"))
      (expect (true? (:success? profile-result)))
      (expect (= true (get-in (result-of profile-result) ["configured"])))
      (expect (true? (:success? check-result)))
      (expect (not= "unconfigured" (get-in (result-of check-result) ["status"])))
      (expect (true? (:success? next-result)))
      (expect (or (nil? (get-in (result-of next-result) ["next_step"]))
                  (= "br/run-evidence" (get-in (result-of next-result) ["next_step" "op" "tool"]))))
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
  bridge-next-suggests-extension-ops-test
  (it
    "next suggests extension ops"
    (let
      [root
       (str (java.nio.file.Files/createTempDirectory
              "bridge-ext-next"
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

       check-result
       (bridge/check env {"changed_files" ["src/core.clj"]})

       next-result
       (bridge/next env {"changed_files" ["src/core.clj"]})

       open-obligation
       (first (get-in (result-of check-result) ["required_obligations"]))

       suggestion
       (get-in (result-of next-result) ["next_step"])]

      (expect (true? (:success? check-result)))
      (expect (= "attention-required" (get-in (result-of check-result) ["status"])))
      (expect (= 1 (get-in (result-of check-result) ["counts" "required_obligations"])))
      (expect (= "open" (get open-obligation "state")))
      (expect (= ["unit-tests"] (get-in open-obligation ["required_evidence"])))
      (expect (true? (:success? next-result)))
      (expect (= "attention-required" (get-in (result-of next-result) ["status"])))
      (expect (= "extension_op" (get suggestion "kind")))
      (expect (= "br/run-evidence" (get-in suggestion ["op" "tool"])))
      (expect (= ["unit"] (get-in suggestion ["op" "args"])))
      (expect (not (str/includes? (or (get-in suggestion ["op" "call"]) "") "bb bridge"))))))

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

(defdescribe
  bridge-hint-hook-test
  (it
    "hinting suggests the next action"
    (let
      [hint-fn
       (get-in bridge/vis-extension [:ext/hooks 0 :fn])

       unconfigured-root
       (str (java.nio.file.Files/createTempDirectory
              "bridge-ext-hint-unconfigured"
              (make-array java.nio.file.attribute.FileAttribute 0)))

       configured-root
       (str (java.nio.file.Files/createTempDirectory
              "bridge-ext-hint-configured"
              (make-array java.nio.file.attribute.FileAttribute 0)))

       _
       (spit (str configured-root "/deps.edn") "{:aliases {:test {}}}")

       _
       (.mkdirs (java.io.File. configured-root "src"))

       _
       (spit (str configured-root "/src/core.clj") "(ns core)\n(def x 1)\n")

       _
       (bridge/init {:workspace/root configured-root})

       _
       (run-in! configured-root "git" "init" "-q")

       _
       (spit (str configured-root "/src/core.clj") "(ns core)\n(def x 2)\n")

       unconfigured-hint
       (hint-fn {:environment {:workspace/root unconfigured-root}})

       configured-hint
       (hint-fn {:environment {:workspace/root configured-root}})]

      ;; Unconfigured workspaces are the normal state, not actionable
      ;; verification work: the hook must stay silent instead of
      ;; emitting a standing warn-task in every non-Bridge repo.
      (expect (nil? unconfigured-hint))
      (expect (= :info (:importance configured-hint)))
      (expect (nil? (:validator-fn configured-hint)))
      (expect (str/includes? (:title configured-hint) "br_next()"))
      ;; The hint is purely informational — it must NOT instruct the model
      ;; to call any removed ctx verb (plan_step / task_set were dropped).
      (expect (not (str/includes? (:title configured-hint) "plan_step")))
      (expect (not (str/includes? (:title configured-hint) "task_set")))
      (expect (not (str/includes? (:title configured-hint) "br_run_evidence")))
      (expect (not (str/includes? (:title configured-hint) "bb bridge"))))))
