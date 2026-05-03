(ns com.blockether.vis.internal.git-test
  (:require [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe count-status-sets-test
  (it "counts paths across status buckets"
    (expect (= 3 (git/count-status-sets #{"a" "b"} #{"c"} nil)))))

(defdescribe workspace-status-shape-test
  (it "reports whether the current directory is inside a git workspace"
    (let [status (git/workspace-status)]
      (expect (contains? status :workspace?))
      (when (:workspace? status)
        (expect (string? (:repo status)))
        (expect (string? (:branch status)))
        (expect (number? (:modified status)))
        (expect (number? (:created status)))
        (expect (number? (:deleted status)))))))
