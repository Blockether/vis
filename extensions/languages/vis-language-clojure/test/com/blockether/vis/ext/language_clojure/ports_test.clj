(ns com.blockether.vis.ext.language-clojure.ports-test
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-ports-" (into-array FileAttribute []))))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))] (.delete f))))

(defdescribe discover-all-test
  (it "reads workspace `.nrepl-port`"
    (let [root (tmp-dir)]
      (try
        (spit (io/file root ".nrepl-port") "57321\n")
        (let [hits (ports/discover-all (.getAbsolutePath root))]
          (expect (some #(= 57321 (:port %)) hits))
          (expect (every? string? (map :source hits))))
        (finally (cleanup root)))))

  (it "returns empty when no ports exist anywhere reachable"
    (let [root (tmp-dir)]
      (try
        (let [hits (ports/discover-all (.getAbsolutePath root))]
          ;; Home may still hold ports (real dev box). At minimum the
          ;; workspace-relative `.nrepl-port` must not be present:
          (expect (not (some #(= (.getAbsolutePath (io/file root ".nrepl-port"))
                                (:source %))
                         hits))))
        (finally (cleanup root)))))

  (it "tolerates a malformed port file"
    (let [root (tmp-dir)]
      (try
        (spit (io/file root ".nrepl-port") "not-a-number")
        (let [hits (ports/discover-all (.getAbsolutePath root))]
          (expect (not (some #(= (.getAbsolutePath (io/file root ".nrepl-port"))
                                (:source %))
                         hits))))
        (finally (cleanup root))))))

(defdescribe find-default-test
  (it "returns the workspace port when present"
    (let [root (tmp-dir)]
      (try
        (spit (io/file root ".nrepl-port") "65000")
        (expect (= 65000 (ports/find-default (.getAbsolutePath root))))
        (finally (cleanup root))))))
