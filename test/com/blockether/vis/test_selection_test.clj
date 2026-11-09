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
