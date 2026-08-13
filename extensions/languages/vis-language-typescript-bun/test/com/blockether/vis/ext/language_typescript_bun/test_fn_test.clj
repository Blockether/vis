(ns com.blockether.vis.ext.language-typescript-bun.test-fn-test
  "The bun pack speaks the run_tests CONTRACT's own words. `bun test` prints its
   own summary lines (`N pass`, `N fail`); folding them onto `pass` / `fail`
   happens HERE, in the pack that knows what bun means by them, never in a
   translation table at the language surface."
  (:require [com.blockether.vis.ext.language-typescript-bun.core :as core]
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
