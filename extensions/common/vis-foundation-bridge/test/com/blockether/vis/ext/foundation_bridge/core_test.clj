(ns com.blockether.vis.ext.foundation-bridge.core-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [bridge.io :as bio]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-bridge.core :as bridge]))

(defn- run-in!
  [root & args]
  (let [pb (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String args))]
    (.directory pb (java.io.File. root))
    (let [proc (.start pb)
          exit (.waitFor proc)]
      (expect (zero? exit))
      exit)))

(defdescribe bridge-extension-test
  (it "configures the extension",
    (let [prompt-text ((get-in bridge/vis-extension [:ext/prompt]) {})]
      (expect (= 'br (get-in bridge/vis-extension [:ext/sci :ext.sci/alias])))
      (expect (= '#{init profile check next list-evidence run-evidence}
                (set (map :ext.symbol/symbol
                       (get-in bridge/vis-extension [:ext/sci :ext.sci/symbols])))))
      (expect (str/includes? prompt-text "use `(br/check)` first"))
      (expect (str/includes? prompt-text "summarize the returned map instead of pasting it raw"))
      (expect (str/includes? prompt-text ":status-summary"))
      (expect (str/includes? prompt-text ":required-obligations"))
      (expect (str/includes? prompt-text "Keep policy obligations and runnable evidence ids distinct"))

      (expect (= [{:id :vis.bridge/next
                   :doc "Hint the model about the next Bridge action when the workspace is unconfigured or Bridge has open evidence work."
                   :phase :turn.iteration/start
                   :fn (get-in bridge/vis-extension [:ext/hooks 0 :fn])}]
                (get-in bridge/vis-extension [:ext/hooks])))
      (expect (= :op.tag/observation (vis/op-tag :br/check)))
      (expect (= :op.tag/observation (vis/op-tag :br/next)))
      (expect (= :op.tag/mutation (vis/op-tag :br/init)))
      (expect (= :op.tag/mutation (vis/op-tag :br/run-evidence))))))

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
          run-result (bridge/run-evidence env "unit" {:dry-run? true})]
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
          run-result (bridge/run-evidence env "unit" {:dry-run? true})]
      (expect (false? (:success? run-result)))
      (expect (not (str/includes? (or (get-in run-result [:error :hint]) "") "bb bridge")))
      (expect (str/includes? (or (get-in run-result [:error :hint]) "") "(br/init)")))))

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
          check-result (bridge/check env {:changed-files ["src/core.clj"]})
          next-result (bridge/next env {:changed-files ["src/core.clj"]})
          open-obligation (first (get-in check-result [:result :open-obligations]))
          suggestion (get-in next-result [:result :next-step])]
      (expect (true? (:success? check-result)))
      (expect (= "attention-required" (get-in check-result [:result :status])))
      (expect (= 1 (get-in check-result [:result :status-summary :required-obligation-count])))
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
          result (bridge/check env {:changed-files ["src/core.clj"]})]
      (expect (true? (:success? result)))
      (expect (= "attention-required" (get-in result [:result :status])))
      (expect (= 1 (get-in result [:result :status-summary :required-obligation-count])))
      (expect (= 1 (get-in result [:result :status-summary :receipt-count])))
      (expect (= "bridge/run-evidence" (get-in result [:result :next-action :op])))
      (expect (= "unit" (get-in result [:result :next-action :args :id])))
      (expect (= "failed" (get-in result [:result :evidence-receipts 0 :status])))
      (expect (= "unit-tests" (get-in result [:result :required-obligations 0 :evidence-kind]))))))

(defdescribe bridge-hint-hook-test
  (it "hinting suggests the next action"
    (let [hint-fn (get-in bridge/vis-extension [:ext/hooks 0 :fn])
          unconfigured-root (str (java.nio.file.Files/createTempDirectory
                                   "bridge-ext-hint-unconfigured"
                                   (make-array java.nio.file.attribute.FileAttribute 0)))
          configured-root (str (java.nio.file.Files/createTempDirectory
                                 "bridge-ext-hint-configured"
                                 (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (str configured-root "/deps.edn") "{:aliases {:test {}}}")
          _ (.mkdirs (java.io.File. configured-root "src"))
          _ (spit (str configured-root "/src/core.clj") "(ns core)\n(def x 1)\n")
          _ (bridge/init {:workspace/root configured-root})
          _ (run-in! configured-root "git" "init" "-q")
          _ (spit (str configured-root "/src/core.clj") "(ns core)\n(def x 2)\n")
          unconfigured-hint (hint-fn {:environment {:workspace/root unconfigured-root}})
          configured-hint (hint-fn {:environment {:workspace/root configured-root}})]
      (expect (= :high (:importance unconfigured-hint)))
      (expect (str/includes? (:text unconfigured-hint) "(br/init)"))
      (expect (= :low (:importance configured-hint)))
      (expect (str/includes? (:text configured-hint) "(br/next)"))
      (expect (not (str/includes? (:text configured-hint) "(br/run-evidence)")))
      (expect (not (str/includes? (:text configured-hint) "bb bridge"))))))
