(ns com.blockether.vis.test-selection-test
  (:require [com.blockether.vis.test-selection :as selection]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private release 'com.blockether.vis.release-bundle-test)

(defdescribe
  skip-release-test
  (it "skips the launcher script tests only when nothing they run has changed"
      (expect (true? (selection/skip-release? {} ["src/a.clj" "apps/vis-tui/x.clj"])))
      (doseq [changed [["bin/vis-agent"] [".github/workflows/ci.yml"] ["build.clj"]
                       ["packages/vis-agent/deps.edn"] ["VIS_VERSION"]
                       ["test/com/blockether/vis/release_bundle_test.clj"]]]
        (expect (false? (selection/skip-release? {} changed)) (pr-str changed))))
  (it "runs them in CI, on request and when Git cannot tell"
      (doseq [[env changed] [[{"CI" "true"} []] [{"VIS_TEST_ALL" "1"} []] [{} nil]]]
        (expect (false? (selection/skip-release? env changed)) (pr-str env))))
  (it "skips them in CI only when a base commit shows that nothing they run has changed"
      (let [env {"CI" "true" "VIS_TEST_BASE" "abc"}]
        (expect (true? (selection/skip-release? env ["src/a.clj"])))
        (expect (false? (selection/skip-release? env ["bin/vis-agent"])))
        (expect (false? (selection/skip-release? env nil)))
        (expect (false? (selection/skip-release? (assoc env "VIS_TEST_ALL" "1") [])))
        (expect (false? (selection/skip-release? {"CI" "true" "VIS_TEST_BASE" ""} []))))))

(defdescribe
  part-test
  (it "puts each namespace in one part, and runs all of them without a part"
      (doseq [n ['com.blockether.vis.internal.python.cli-environment-test
                 'com.blockether.vis.internal.python.cli-shared-uv-test
                 'com.blockether.vis.internal.python.extensions-test
                 'com.blockether.vis.internal.python.worker-test
                 'com.blockether.vis.internal.loop-test
                 'com.blockether.vis.internal.loop.turn-test
                 'com.blockether.vis.internal.gateway.server-test
                 'com.blockether.vis.contract.gateway-test
                 release]]
        (expect (= 1
                   (count (filter #(selection/in-part? % n)
                            ["python-cli" "python-uv" "python-ext" "python" "core" "other"])))
                (str n))
        (expect (selection/in-part? nil n))
        (expect (selection/in-part? "" n)))
      (expect (selection/in-part? "python" 'com.blockether.vis.internal.python.worker-test))
      (expect (selection/in-part? "other" 'com.blockether.vis.internal.loop.turn-test)))
  (it "checks the gateway answers in the part that also has the contract tests"
      (expect (selection/in-part? "core" 'com.blockether.vis.internal.gateway.server-test))
      (expect (selection/in-part? "core" 'com.blockether.vis.contract.gateway-test)))
  (it "runs the parts of a comma list in one job"
      (expect (selection/in-part? "python-cli,python"
                                  'com.blockether.vis.internal.python.worker-test))
      (expect (not (selection/in-part? "core,other"
                                       'com.blockether.vis.internal.python.worker-test))))
  (it "refuses a part that it does not know, so a wrong name runs no empty job"
      (expect (try (selection/in-part? "pyhton" 'a) false (catch Exception _ true)))
      (expect (try (selection/in-part? "python,othr" 'a) false (catch Exception _ true)))))

(defdescribe
  selected-test
  (it "leaves out the launcher script tests only on request, and only from their part"
      (let [all ['com.blockether.vis.internal.python.worker-test
                 'com.blockether.vis.internal.paths-test release]]
        (expect (= all (selection/selected all nil false)))
        (expect (= (pop all) (selection/selected all nil true)))
        (expect (= [(second all)] (selection/selected all "other" false)))
        (expect (= [release] (selection/selected all "python-cli" false)))
        (expect (= [] (selection/selected all "python-cli" true)))
        (expect (= [(first all)] (selection/selected all "python" false))))))
