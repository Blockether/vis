(ns com.blockether.vis.internal.foundation.environment.repositories-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.environment.repositories :as repositories]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^java.io.File []
  (let [path (Files/createTempDirectory "vis-env-repositories-" (into-array FileAttribute []))]
    (.toFile path)))

(defn- spit-rel
  [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- git!
  [^java.io.File root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let [p (.start pb)]
      (slurp (.getInputStream p))
      (.waitFor p))))

(defn- init-repo!
  [^java.io.File dir]
  (.mkdirs dir)
  (git! dir "init" "-q")
  (git! dir "config" "user.name" "test")
  (git! dir "config" "user.email" "test@example.com")
  (git! dir "config" "commit.gpgsign" "false")
  (spit-rel dir "README.md" "# initial")
  (git! dir "add" "README.md")
  (git! dir "commit" "-q" "-m" "init"))

(defdescribe repositories-snapshot-test
             (it "detects multiple nested Git repositories with prompt-sized summaries"
                 (let [root (make-tmp-dir)]
                   (try (init-repo! root)
                        (init-repo! (io/file root "services/api"))
                        (spit-rel (io/file root "services/api") "untracked.txt" "new")
                        (let
                          [snap (repositories/snapshot root
                                                       {:deadline-ms 2000 :status-timeout-ms 1000})
                           repos (:repositories snap)
                           paths (set (map :path repos))
                           api (first (filter #(= "services/api" (:path %)) repos))]

                          (expect (= 2 (:count snap)))
                          (expect (contains? paths "."))
                          (expect (contains? paths "services/api"))
                          (expect (some? (:branch api)))
                          (expect (true? (:dirty? api)))
                          (expect (true? (:changes? api)))
                          (expect (>= (long (:untracked api)) 1)))
                        (finally (cleanup root)))))
             (it "returns an empty repository list outside Git worktrees"
                 (let [root (make-tmp-dir)]
                   (try (spit-rel root "README.md" "not git")
                        (let [snap (repositories/snapshot root)]
                          (expect (= 0 (:count snap)))
                          (expect (empty? (:repositories snap))))
                        (finally (cleanup root))))))
