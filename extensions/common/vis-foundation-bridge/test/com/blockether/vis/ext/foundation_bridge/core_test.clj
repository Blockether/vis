(ns com.blockether.vis.ext.foundation-bridge.core-test
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [lazytest.core :refer [defdescribe expect it]]
            [bridge.io :as bio]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.ext.foundation-bridge.core :as bridge]
            [com.blockether.vis.ext.foundation-bridge.render :as render]))

(defn- expect-contract
  "Assert `result` conforms to the {:summary :display} render contract and
   that both channels are non-empty."
  [result]
  (expect (extension/render-fn-result? result))
  (expect (some? (:summary result)))
  (expect (some? (:display result)))
  ;; display is a canonical [:ir ...] document with at least one block
  (expect (= :ir (first (:display result))))
  (expect (seq (drop 2 (:display result))))
  ;; summary normalizes to a [:p ...]-bearing [:ir ...]
  (let [ir (extension/summary->ir (:summary result))]
    (expect (= :ir (first ir)))
    (expect (seq (drop 2 ir))))
  result)

(defn- temp-root
  [prefix]
  (str (java.nio.file.Files/createTempDirectory
         prefix
         (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write-policy!
  [root sandbox]
  (bio/write-data (str root "/.bridge/verification-policy.yaml")
    {:artifact "verification-policy"
     :policy-id "sandboxed"
     :bridge-path-sandbox sandbox
     :rules []}))

(defn- protected-env
  [root]
  {:workspace/root root
   :extensions (atom [bridge/vis-extension])})

(defdescribe bridge-extension-test
  (it "configures the extension",
    (let [prompt-text ((get-in bridge/vis-extension [:ext/prompt]) {})
          resource-text (extension/normalize-prompt-text
                          (slurp (io/resource "com/blockether/vis/ext/foundation_bridge/prompt.md")))]
      (expect (= 'br (get-in bridge/vis-extension [:ext/engine :ext.engine/alias])))
      (expect (= '#{init profile check next list-evidence run-evidence}
                (set (map :ext.symbol/symbol
                       (get-in bridge/vis-extension [:ext/engine :ext.engine/symbols])))))
      (expect (= resource-text prompt-text))
      (expect (str/includes? prompt-text "Use `br_check()` first"))
      (expect (str/includes? prompt-text "summarize the returned map instead of pasting it raw"))
      (expect (str/includes? prompt-text "counts"))
      (expect (str/includes? prompt-text "required_obligations"))
      (expect (str/includes? prompt-text "Keep policy obligations and runnable evidence ids distinct"))
      (expect (fn? (:ext/protected-paths bridge/vis-extension)))
      (expect (= #{:policy/obligations} (:ext/capabilities bridge/vis-extension)))

      (expect (nil? (:ext/hooks bridge/vis-extension)))
      (expect (= :observation (vis/op-tag :br/check)))
      (expect (= :observation (vis/op-tag :br/next)))
      (expect (= :mutation (vis/op-tag :br/init)))
      (expect (= :mutation (vis/op-tag :br/run-evidence))))))

(defdescribe bridge-protected-paths-test
  (it "returns no protected path rules when Bridge is unconfigured"
    (let [root (temp-root "bridge-ext-protected-unconfigured")]
      (expect (= [] ((:ext/protected-paths bridge/vis-extension)
                     (protected-env root))))
      (expect (= [] (extension/active-protected-globs
                      (protected-env root))))))

  (it "returns no protected path rules when policy enforcement is disabled"
    (let [root (temp-root "bridge-ext-protected-disabled")
          env {:workspace/root root}]
      (bridge/init env)
      (write-policy! root {:enforce? false
                           :rules [{:path-pattern ".bridge/"
                                    :access "none"}]})
      (expect (= [] ((:ext/protected-paths bridge/vis-extension)
                     (protected-env root))))))

  (it "maps enforced Bridge path sandbox rules to Vis protected path rules"
    (let [root (temp-root "bridge-ext-protected")
          env {:workspace/root root}
          hint "Policy changes require human approval."]
      (bridge/init env)
      (write-policy! root {:enforce? true
                           :default-access "read-write"
                           :rules [{:path-pattern ".bridge/"
                                    :access "none"
                                    :reason "Use br/* tools for Bridge-owned state."}
                                   {:path-pattern ".bridge/verification-policy.yaml"
                                    :access "read-only"
                                    :reason hint}
                                   {:path-pattern ".bridge/ephemeral/evidence/**"
                                    :access "read-write"}]})
      (expect (= [{:glob ".bridge/**"
                   :access :none
                   :hint "Use br/* tools for Bridge-owned state."}
                  {:glob ".bridge/verification-policy.yaml"
                   :access :read-only
                   :hint hint}
                  {:glob ".bridge/ephemeral/evidence/**"
                   :access :read-write
                   :hint "Bridge policy protects this path; use the br/* tool surface instead of direct file IO."}]
                ((:ext/protected-paths bridge/vis-extension)
                 (protected-env root))))
      (expect (= [{:glob ".bridge/**"
                   :access :none
                   :hint "Use br/* tools for Bridge-owned state."
                   :extension/name "foundation-bridge"}
                  {:glob ".bridge/verification-policy.yaml"
                   :access :read-only
                   :hint hint
                   :extension/name "foundation-bridge"}
                  {:glob ".bridge/ephemeral/evidence/**"
                   :access :read-write
                   :hint "Bridge policy protects this path; use the br/* tool surface instead of direct file IO."
                   :extension/name "foundation-bridge"}]
                (extension/active-protected-globs
                  (protected-env root))))))

  (it "prefixes policy patterns when the Bridge profile root is below the workspace"
    (let [root (temp-root "bridge-ext-protected-subroot")
          project-root (str root "/project")
          env {:workspace/root root}
          profile-path (str root "/.bridge/profile.edn")]
      (bridge/init env)
      (.mkdirs (java.io.File. project-root ".bridge"))
      (bio/write-data profile-path
        (assoc (bio/read-data profile-path)
          :root-path "../project"))
      (bio/write-data (str project-root "/.bridge/verification-policy.yaml")
        {:artifact "verification-policy"
         :policy-id "sandboxed-subroot"
         :bridge-path-sandbox {:enforce? true
                               :rules [{:path-pattern ".bridge/"
                                        :access "none"}]}
         :rules []})
      (expect (= [{:glob "project/.bridge/**"
                   :access :none
                   :hint "Bridge policy protects this path; use the br/* tool surface instead of direct file IO."}]
                ((:ext/protected-paths bridge/vis-extension)
                 (protected-env root)))))))

(defdescribe bridge-unconfigured-workspace-test
  (it "can initialize an unconfigured workspace and run the CLI",
    (let [root (str (java.nio.file.Files/createTempDirectory
                      "bridge-ext-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (str root "/deps.edn") "{:aliases {:test {}}}")
          env {:workspace/root root}
          init-result (bridge/init env)
          profile-result (bridge/profile env)
          check-result (bridge/check env)
          next-result (bridge/next env)
          list-result (bridge/list-evidence env)
          run-result (bridge/run-evidence env "unit" {:is_dry_run true})]
      (expect (true? (:success? init-result)))
      (expect (= true (get-in init-result [:result :configured?])))
      (expect (= false (get-in init-result [:result :already-configured?])))
      (expect (= true (get-in init-result [:result :created?])))
      (expect (= [".bridge/profile.edn" ".bridge/verification-policy.yaml"]
                (get-in init-result [:result :created])))
      (expect (= [".gitignore"] (get-in init-result [:result :updated])))
      (expect (= "br/check" (get-in init-result [:result :next-step :op :tool])))
      (expect (str/includes? (or (get-in init-result [:result :profile-path]) "") ".bridge/profile.edn"))

      (expect (true? (:success? profile-result)))
      (expect (= true (get-in profile-result [:result :configured?])))

      (expect (true? (:success? check-result)))
      (expect (not= "unconfigured" (get-in check-result [:result :status])))

      (expect (true? (:success? next-result)))
      (expect (or (nil? (get-in next-result [:result :next-step]))
                (= "br/run-evidence" (get-in next-result [:result :next-step :op :tool]))))

      (expect (true? (:success? list-result)))
      (expect (vector? (get-in list-result [:result :commands])))

      (expect (true? (:success? run-result)))
      (expect (= true (get-in run-result [:result :result :dry-run?]))))))

(defdescribe bridge-no-profile-error-test
  (it "returns an error when no profile is configured"
    (let [root (str (java.nio.file.Files/createTempDirectory
                      "bridge-ext-no-profile"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
          env {:workspace/root root}
          run-result (bridge/run-evidence env "unit" {:is_dry_run true})]
      (expect (false? (:success? run-result)))
      (expect (not (str/includes? (or (get-in run-result [:error :hint]) "") "bb bridge")))
      (expect (str/includes? (or (get-in run-result [:error :hint]) "") "br_init()")))))

(defdescribe bridge-init-idempotent-test
  (it "init is iempotent"
    (let [env {:workspace/root (str (java.nio.file.Files/createTempDirectory
                                      "bridge-ext-idempotent"
                                      (make-array java.nio.file.attribute.FileAttribute 0)))}
          first-result (bridge/init env)
          second-result (bridge/init env)]
      (expect (true? (:success? first-result)))
      (expect (= true (get-in first-result [:result :created?])))
      (expect (true? (:success? second-result)))
      (expect (= true (get-in second-result [:result :already-configured?])))
      (expect (= false (get-in second-result [:result :created?])))
      (expect (= [] (get-in second-result [:result :created])))
      (expect (= [] (get-in second-result [:result :updated])))
      (expect (str/includes? (get-in second-result [:result :message]) "already configured")))))

(defdescribe bridge-next-suggests-extension-ops-test
  (it "next suggests extension ops"
    (let [root (str (java.nio.file.Files/createTempDirectory
                      "bridge-ext-next"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (str root "/deps.edn") "{:aliases {:test {}}}")
          _ (.mkdirs (java.io.File. root "src"))
          _ (spit (str root "/src/core.clj") "(ns core)")
          env {:workspace/root root}
          _ (bridge/init env)
          check-result (bridge/check env {:changed_files ["src/core.clj"]})
          next-result (bridge/next env {:changed_files ["src/core.clj"]})
          open-obligation (first (get-in check-result [:result :required-obligations]))
          suggestion (get-in next-result [:result :next-step])]
      (expect (true? (:success? check-result)))
      (expect (= "attention-required" (get-in check-result [:result :status])))
      (expect (= 1 (get-in check-result [:result :counts :required-obligations])))
      (expect (= "open" (:state open-obligation)))
      (expect (= ["unit-tests"] (get-in open-obligation [:required-evidence])))
      (expect (true? (:success? next-result)))
      (expect (= "attention-required" (get-in next-result [:result :status])))
      (expect (= :extension-op (:kind suggestion)))
      (expect (= "br/run-evidence" (get-in suggestion [:op :tool])))
      (expect (= ["unit"] (get-in suggestion [:op :args])))
      (expect (not (str/includes? (or (get-in suggestion [:op :call]) "") "bb bridge"))))))

(defdescribe bridge-check-flattens-status-test
  (it "check returns flattened status summary"
    (let [root (str (java.nio.file.Files/createTempDirectory
                      "bridge-ext-flatten"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (str root "/deps.edn") "{:aliases {:test {}}}")
          _ (.mkdirs (java.io.File. root "src"))
          _ (spit (str root "/src/core.clj") "(ns core)")
          env {:workspace/root root}
          _ (bridge/init env)
          _ (bio/write-data (str root "/.bridge/ephemeral/evidence/unit.yaml")
              {:artifact "evidence-run"
               :evidence-id "unit"
               :kind "unit-tests"
               :role "regression"
               :subject "vis"
               :evidence-status "failed"
               :execution-status "execution-failed"
               :finished-at "2026-05-20T19:01:06.340575Z"
               :command "clojure -M:test"})
          result (bridge/check env {:changed_files ["src/core.clj"]})]
      (expect (true? (:success? result)))
      (expect (= "attention-required" (get-in result [:result :status])))
      (expect (= 1 (get-in result [:result :summary-version])))
      (expect (= 1 (get-in result [:result :counts :required-obligations])))
      (expect (= 1 (get-in result [:result :counts :receipts])))
      (expect (= "bridge/run-evidence" (get-in result [:result :next-action :op])))
      (expect (= "unit" (get-in result [:result :next-action :args :id])))
      (expect (= "unit" (get-in result [:result :next-action :evidence-id])))
      (expect (= "failed" (get-in result [:result :evidence-receipts 0 :status])))
      (expect (= "unit-tests" (get-in result [:result :required-obligations 0 :evidence-kind]))))))

(defdescribe bridge-hook-noise-test
  (it "does not inject advisory hook tasks for open Bridge work"
    ;; Bridge obligations should become structured graph state when the
    ;; DAG bridge model is enabled. The extension must not inject a
    ;; dismissible `vis.bridge/next` instruction task in every iteration.
    (expect (nil? (:ext/hooks bridge/vis-extension)))))

;; ---------------------------------------------------------------------------
;; Render contract: every `:render-fn` returns {:summary :display}
;; (`::render-fn-result`). We exercise both the populated and the
;; unconfigured/empty branch of each renderer, since the cond path
;; decides the summary shape.
;; ---------------------------------------------------------------------------

(defdescribe bridge-render-contract-test
  (it "render-init conforms across configured / already / unconfigured branches"
    (expect-contract
      (render/render-init {:configured? true :already-configured? true
                           :workspace-root "/ws" :profile-path ".bridge/profile.edn"
                           :message "ready"}))
    (expect-contract
      (render/render-init {:configured? true :already-configured? false
                           :workspace-root "/ws" :profile-path ".bridge/profile.edn"
                           :created [".bridge/profile.edn" ".bridge/verification-policy.yaml"]
                           :updated [".gitignore"]}))
    ;; configured with no created/updated counts
    (expect-contract
      (render/render-init {:configured? true :profile-path ".bridge/profile.edn"
                           :created [] :updated []}))
    (expect-contract
      (render/render-init {:configured? false :workspace-root "/ws"
                           :message "not configured"})))

  (it "render-profile conforms with and without a profile"
    (expect-contract
      (render/render-profile {:configured? false :message "no profile"}))
    (expect-contract
      (render/render-profile {:configured? true
                              :summary {:project "vis" :version "1" :name "vis-profile"}
                              :profile-path ".bridge/profile.edn"
                              :policy-path ".bridge/verification-policy.yaml"
                              :policy-loaded? true}))
    ;; minimal summary (no name/version/project)
    (expect-contract
      (render/render-profile {:configured? true :summary {}
                              :policy-loaded? false})))

  (it "render-check conforms across ok / fail / unconfigured branches"
    (expect-contract
      (render/render-check {:configured? false :message "no profile"}))
    (expect-contract
      (render/render-check {:configured? true :status :ok :issue-count 0
                            :project "vis"
                            :required-obligations [] :recommended-obligations []
                            :evidence-receipts []}))
    (expect-contract
      (render/render-check {:configured? true :status :attention-required
                            :issue-count 2 :project "vis"
                            :required-obligations [{:kind :unit-tests :subject "vis"
                                                    :summary "needs unit tests" :state "open"}]
                            :recommended-obligations [{:kind :lint :artifact "lint"
                                                       :state "open"}]
                            :evidence-receipts [{:status "failed"}]}))
    ;; no :status key -> :center omitted from summary
    (expect-contract
      (render/render-check {:configured? true :issue-count 0
                            :required-obligations [] :recommended-obligations []
                            :evidence-receipts []})))

  (it "render-next conforms across ok / suggestions / unconfigured branches"
    (expect-contract
      (render/render-next {:configured? false :message "no profile"}))
    (expect-contract
      (render/render-next {:configured? true :issue-count 0 :suggestions []
                           :project "vis"}))
    (expect-contract
      (render/render-next {:configured? true :issue-count 1
                           :suggestions [{:op {:call "(br/run-evidence \"unit\")"}
                                          :summary "run unit tests"
                                          :required-evidence ["unit-tests"]}]
                           :next-step {:op {:call "(br/run-evidence \"unit\")"}}
                           :project "vis"})))

  (it "render-list-evidence conforms with and without commands"
    (expect-contract
      (render/render-list-evidence {:configured? false :message "no profile"}))
    (expect-contract
      (render/render-list-evidence {:configured? true :profile-path ".bridge/profile.edn"
                                    :commands []}))
    (expect-contract
      (render/render-list-evidence {:configured? true :profile-path ".bridge/profile.edn"
                                    :commands [{:id "unit" :description "run unit tests"
                                                :command "clojure -M:test" :timeout-seconds 120}
                                               {:id "lint" :command "clj-kondo"}]})))

  (it "render-run-evidence conforms across passed / failed / dry-run branches"
    (expect-contract
      (render/render-run-evidence {:profile-path ".bridge/profile.edn"
                                   :result {:id "unit" :status "passed"
                                            :duration-ms 1234 :exit-code 0
                                            :receipt-path ".bridge/ephemeral/evidence/unit.yaml"
                                            :stdout "all tests passed\n"}}))
    (expect-contract
      (render/render-run-evidence {:profile-path ".bridge/profile.edn"
                                   :result {:id "unit" :status "failed"
                                            :duration-ms 99 :exit-code 1
                                            :stderr "boom\n"}}))
    ;; dry-run: no status / duration / exit -> :right omitted
    (expect-contract
      (render/render-run-evidence {:profile-path ".bridge/profile.edn"
                                   :result {:id "unit"}}))
    ;; empty result map
    (expect-contract
      (render/render-run-evidence {:profile-path ".bridge/profile.edn" :result {}}))))
