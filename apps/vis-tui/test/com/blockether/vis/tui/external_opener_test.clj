(ns com.blockether.vis.tui.external-opener-test
  "The detached OS opener needs neither a process library nor terminal IO."
  (:require [com.blockether.vis.tui.external-opener :as opener]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defdescribe
  detached-opener-test
  (it "reports an absent executable without throwing"
      (expect (instance? java.io.IOException (#'opener/spawn! ["/vis-missing-opener/executable"]))))
  (it "closes stdin and discards both output streams"
      (let [path
            (Files/createTempFile "vis-opener-" ".done" (make-array FileAttribute 0))

            task
            (promise)]

        (Files/delete path)
        (try (expect (nil? (#'opener/spawn!
                            ["sh" "-c"
                             "cat >/dev/null; printf output; printf error >&2; printf done > \"$1\""
                             "opener-test" (str path)])))
             ;; The marker is written only after stdin reaches EOF. Bound a broken
             ;; implementation without depending on a fixed scheduling delay.
             (let [deadline (+ (System/nanoTime) 5000000000)]
               (loop []

                 (cond (Files/exists path (make-array java.nio.file.LinkOption 0)) (deliver task
                                                                                            true)
                       (< (System/nanoTime) deadline) (do (Thread/sleep 10) (recur))
                       :else (deliver task false))))
             (expect @task)
             (finally (Files/deleteIfExists path))))))
