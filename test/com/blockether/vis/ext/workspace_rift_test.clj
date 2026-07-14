(ns com.blockether.vis.ext.workspace-rift-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.workspace-rift]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree-lax!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(def ^:private delete-tree! @#'com.blockether.vis.ext.workspace-rift/delete-tree!)

(defdescribe
  delete-tree-test
  (it "deletes a temporary tree completely on success"
      (let [root (temp-dir "vis-rift-delete-ok")]
        (io/make-parents (io/file root "nested" "file.txt"))
        (spit (io/file root "nested" "file.txt") "x")
        (delete-tree! root)
        (expect (false? (.exists (io/file root))))))
  (it "throws with failure and remaining-path data when deletion is partial"
      (let [root
            (temp-dir "vis-rift-delete-fail")

            keep
            (io/file root "keep.txt")

            original-delete
            @#'com.blockether.vis.ext.workspace-rift/delete-path!]

        (try (spit keep "keep")
             (with-redefs [com.blockether.vis.ext.workspace-rift/delete-path!
                           (fn [^java.nio.file.Path path]
                             (if (= "keep.txt"
                                    (some-> path
                                            .getFileName
                                            str))
                               (throw (ex-info "simulated delete failure" {:path (str path)}))
                               (original-delete path)))]
               (let [ex (try (delete-tree! root) nil (catch clojure.lang.ExceptionInfo e e))
                     data (ex-data ex)]

                 (expect (some? ex))
                 (expect (= :workspace-rift/delete-tree-failed (:type data)))
                 (expect (= (str (.toPath (io/file root))) (:root data)))
                 (expect (some #(= (str (.toPath keep)) (:path %)) (:failures data)))
                 (expect (some #{(str (.toPath keep))} (:remaining data)))))
             (finally (delete-tree-lax! root))))))
