(ns com.blockether.vis.internal.foundation.environment.git-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.environment.git :as git]
            [com.blockether.vis.internal.workspace.git :as vis-git]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-env-git-" (into-array FileAttribute []))))

(defn- spit-rel
  [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup
  [^java.io.File root]
  (doseq [^java.io.File f (reverse (file-seq root))]
    (.delete f)))

(defdescribe work-tree-files-test
             (it "lists tracked and untracked files without ignored build output (#422)"
                 (let [root (make-tmp-dir)]
                   (try (vis-git/run-git root ["init" "-q"])
                        (spit-rel root ".gitignore" "DerivedData/\n")
                        (spit-rel root "src/a.clj" "(ns a)")
                        (vis-git/run-git root ["add" "src/a.clj"])
                        (spit-rel root "src/b clj.clj" "(ns b)")
                        (spit-rel root "DerivedData/gen.py" "x = 1")
                        (expect (= #{".gitignore" "src/a.clj" "src/b clj.clj"}
                                   (set (git/work-tree-files root))))
                        (finally (cleanup root)))))
             (it "answers nil outside a work tree"
                 (let [root (make-tmp-dir)]
                   (try (spit-rel root "a.clj" "(ns a)")
                        (expect (nil? (git/work-tree-files root)))
                        (finally (cleanup root))))))
