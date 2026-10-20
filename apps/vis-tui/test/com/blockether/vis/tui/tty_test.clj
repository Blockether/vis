(ns com.blockether.vis.tui.tty-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.tui.tty :as tty]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; Regression: System/out and System/err kept one descriptor on the log file. A rotation
;; renamed and deleted that file, so every later print went to a deleted inode.
(defdescribe
  log-output-stream-test
  (it "appends to the new file at the path after a rotation renames the old one"
      (let [dir
            (Files/createTempDirectory "vis-tui-log" (make-array FileAttribute 0))

            path
            (str (.resolve dir "tui.log"))

            out
            (java.io.PrintStream. ^java.io.OutputStream (tty/log-output-stream path) true "UTF-8")]

        (try (.println out "before")
             (.renameTo (io/file path) (io/file (str path ".1")))
             (.println out "after")
             (expect (= "after\n" (slurp path)))
             (expect (= "before\n" (slurp (str path ".1"))))
             (finally (doseq [f (reverse (file-seq (.toFile dir)))]
                        (io/delete-file f true)))))))
