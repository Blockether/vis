(ns com.blockether.vis.ext.language-typescript-bun.test-fn-test
  "The bun pack speaks the run_tests CONTRACT's own words. `bun test` prints its
   own summary lines (`N pass`, `N fail`); folding them onto `pass` / `fail`
   happens HERE, in the pack that knows what bun means by them, never in a
   translation table at the language surface."
  (:require [com.blockether.vis.ext.language-typescript-bun.core :as core]
            [com.blockether.vis.ext.language-typescript-bun.runner :as runner]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private bun-counts @#'core/bun-counts)

(defdescribe
  bun-counts-test
  (it "reads bun's summary lines as the contract's pass / fail"
      (expect (= {"pass" 12 "fail" 3}
                 (bun-counts (str " 12 pass\n" " 1 skip\n"
                                  " 3 fail\n" " Ran 16 tests across 4 files. [812.00ms]\n")))))
  (it "names each count ONCE — no pytest/bun spelling rides along"
      (expect (= #{"pass" "fail"} (set (keys (bun-counts (str " 4 pass\n" " 0 fail\n")))))))
  (it "leaves a count bun never printed ABSENT, so UNKNOWN is not reported as zero"
      ;; A crashed run prints no summary: `{}` completes to nil counts, where a
      ;; zero would read as a green run that simply had no failures.
      (expect (= {"pass" 4} (bun-counts " 4 pass\n")))
      (expect (= {} (bun-counts "error: Cannot find module 'nope'\n")))
      (expect (= {} (bun-counts "")))))

(def ^:private test-command @#'core/test-command)

(defn- cmd-tail
  "The `bun test` argv one call builds, minus the machine's own bun binary — the
   grammar, without depending on a bun install."
  [opts]
  (with-redefs
    [runner/resolve-command (fn [_]
                              ["bun"])]
    (vec (drop 1 (test-command "." opts)))))

;; ONE selector in every pack: a path may carry the test name after `::`, so bun
;; gets its `-t` from the same string that names the file. `filter` used to be a
;; second key saying the same thing.
(defdescribe test-command-node-id-test
             (it "splits a node id into bun's path target and its -t name pattern"
                 (expect (= ["test" "-t" "adds" "src/math.test.ts"]
                            (cmd-tail {"paths" ["src/math.test.ts::adds"]}))))
             (it "alternates several names into the ONE pattern bun keeps"
                 (expect (= ["test" "-t" "adds|trims" "src/math.test.ts" "src/str.test.ts"]
                            (cmd-tail {"paths" ["src/math.test.ts::adds"
                                                "src/str.test.ts::trims"]}))))
             (it "passes a PATHLESS ::name as the filter over the whole suite"
                 (expect (= ["test" "-t" "adds"] (cmd-tail {"paths" ["::adds"]}))))
             (it "adds no -t at all when no entry names a test"
                 (expect (= ["test" "src"] (cmd-tail {"paths" ["src"]}))))
             (it "refuses the REMOVED filter key instead of narrowing twice"
                 (let
                   [e (try (core/ts-test-fn {:workspace/root "." :session-id "sid"}
                                            {"paths" ["src"] "filter" "adds"})
                           nil
                           (catch clojure.lang.ExceptionInfo e e))]
                   (expect (= :ts/bad-args (:type (ex-data e))))
                   (expect (re-find #"::adds|node id" (ex-message e))))))
