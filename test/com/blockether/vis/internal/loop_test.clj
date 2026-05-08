(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.vis.internal.loop :as loop]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe def-display-result-test
  (it "does not deref def forms into implicit observations"
    (let [result (#'loop/def-display-result
                  {:sci-ctx :present}
                  "(def xd \"XDDD\")"
                  {:result :raw-var
                   :stdout ""
                   :stderr ""
                   :error nil})]
      (expect (= :raw-var (:result result)))
      (expect (= :vis/silent (:rendering-kind result))))))

(defdescribe answer-cleanup-test
  (it "does not append removed runtime sections"
    (expect (= "done" (loop/append-runtime-appendices {} "done" {}))))

  (it "does not block final answers with workflow state"
    (expect (nil? (loop/final-answer-gate-error {} 0 [] "done")))))
