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

(defdescribe delete-tree-test
             (it "deletes a temporary tree completely on success"
                 (let [root (temp-dir "vis-rift-delete-ok")]
                   (io/make-parents (io/file root "nested" "file.txt"))
                   (spit (io/file root "nested" "file.txt") "x")
                   (delete-tree! root)
                   (expect (false? (.exists (io/file root))))))
             (it "throws with failure and remaining-path data when deletion is partial"
                 (let
                   [root
                    (temp-dir "vis-rift-delete-fail")

                    keep
                    (io/file root "keep.txt")

                    original-delete
                    @#'com.blockether.vis.ext.workspace-rift/delete-path!]

                   (try (spit keep "keep")
                        (with-redefs
                          [com.blockether.vis.ext.workspace-rift/delete-path!
                           (fn [^java.nio.file.Path path]
                             (if (= "keep.txt"
                                    (some-> path
                                            .getFileName
                                            str))
                               (throw (ex-info "simulated delete failure" {:path (str path)}))
                               (original-delete path)))]
                          (let
                            [ex (try (delete-tree! root) nil (catch clojure.lang.ExceptionInfo e e))
                             data (ex-data ex)]

                            (expect (some? ex))
                            (expect (= :workspace-rift/delete-tree-failed (:type data)))
                            (expect (= (str (.toPath (io/file root))) (:root data)))
                            (expect (some #(= (str (.toPath keep)) (:path %)) (:failures data)))
                            (expect (some #{(str (.toPath keep))} (:remaining data)))))
                        (finally (delete-tree-lax! root))))))

(def ^:private prune-dir? @#'com.blockether.vis.ext.workspace-rift/prune-dir?)

(defdescribe prune-dir-test
             (it
               "prunes VCS/build/cache subtrees at ANY depth so the pre-fork perms walk skips them"
               ;; The perms walk is per-file. On a monorepo whose generated app tree holds
               ;; tens of thousands of gitignored files, descending into a NESTED
               ;; node_modules/target is exactly the `/draft new` stall this prune avoids.
               (let
                 [root
                  (java.nio.file.Path/of "/repo" (into-array String []))

                  p
                  (fn [& segs]
                    (java.nio.file.Path/of "/repo" (into-array String segs)))]

                 (expect (prune-dir? root (p ".git")))
                 (expect (prune-dir? root (p "target")))
                 (expect (prune-dir? root (p "apps" "web" "node_modules")))
                 (expect (prune-dir? root (p "apps" "web" "node_modules" "left-pad")))
                 (expect (prune-dir? root (p "extensions" "ext-a" "target" "classes")))
                 (expect (prune-dir? root (p "apps" "web" "ios" ".git")))
                 (expect (prune-dir? root (p "packages" "p" ".clj-kondo" ".cache")))
                 ;; the tracked .clj-kondo dir itself stays walkable
                 (expect (not (prune-dir? root (p "packages" "p" ".clj-kondo"))))
                 (expect (not (prune-dir? root (p "apps" "web" "src"))))
                 (expect (not (prune-dir? root root))))))
