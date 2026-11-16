(ns com.blockether.vis.test-selection-test
  (:require [com.blockether.vis.test-selection :as selection]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe skip-release-test
             (it "skips the launcher script tests only when nothing they run has changed"
                 (expect (true? (selection/skip-release? {} {} ["src/a.clj" "apps/vis-tui/x.clj"])))
                 (doseq [changed [["bin/vis-agent"] [".github/workflows/ci.yml"] ["build.clj"]
                                  ["packages/vis-agent/deps.edn"] ["VIS_VERSION"]
                                  ["test/com/blockether/vis/release_bundle_test.clj"]]]
                   (expect (false? (selection/skip-release? {} {} changed)) (pr-str changed))))
             (it "runs them in CI, on request, for an explicit selection and when Git cannot tell"
                 (doseq [[config env changed] [[{} {"CI" "true"} []] [{} {"VIS_TEST_ALL" "1"} []]
                                               [{:ns-filter #{'a}} {} []]
                                               [{:var-filter #{'a/b}} {} []] [{} {} nil]]]
                   (expect (false? (selection/skip-release? config env changed))
                           (pr-str config env)))))

(defdescribe
  part-test
  (it "puts each namespace in one part, and runs all of them without a part"
      (doseq [n ['com.blockether.vis.internal.python.worker-test
                 'com.blockether.vis.internal.loop-test 'com.blockether.vis.release-bundle-test]]
        (expect (= 1 (count (filter #(selection/in-part? % n) ["python" "other"]))) (str n))
        (expect (selection/in-part? nil n))
        (expect (selection/in-part? "" n)))
      (expect (selection/in-part? "python" 'com.blockether.vis.internal.python.worker-test)))
  (it "refuses a part that it does not know, so a wrong name runs no empty job"
      (expect (try (selection/in-part? "pyhton" 'a) false (catch Exception _ true)))))
