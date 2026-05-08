(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.vis.internal.loop :as loop]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe answer-cleanup-test
  (it "does not append removed runtime sections"
    (expect (= "done" (loop/append-runtime-appendices {} "done" {}))))

  (it "does not block final answers with workflow state"
    (expect (nil? (loop/final-answer-gate-error {} 0 [] "done")))))
