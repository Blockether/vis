(ns com.blockether.vis.internal.git-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^File []
  (.toFile (Files/createTempDirectory "vis-internal-git-" (into-array FileAttribute []))))

(defn- spit-rel
  [^File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup
  [^File root]
  (when (.exists root)
    (doseq [^File f (reverse (file-seq root))]
      (.delete f))))

(defn- git!
  [^File root & args]
  ;; Shell out to the real `git` binary — the same implementation the code
  ;; under test uses, so there is no behavioural skew between the fixture and
  ;; the subject.
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let [p (.start pb)]
      (slurp (.getInputStream p))
      (.waitFor p))))

(defn- init-repo!
  [^File root]
  (git! root "init" "-q")
  (git! root "config" "user.name" "Vis Test")
  (git! root "config" "user.email" "vis-test@example.invalid")
  (git! root "config" "commit.gpgsign" "false")
  (spit-rel root "a.txt" "a\n")
  (git! root "add" "a.txt")
  (git! root "commit" "-q" "-m" "base"))

(defdescribe file-dirty?-test
             (it "is false for clean/untracked/repo-less, true once a tracked file is modified"
                 (let [root (make-tmp-dir)]
                   (try (init-repo! root)
                        (let [a (io/file root "a.txt")]
                          (expect (false? (git/file-dirty? a))) ; clean tracked
                          (spit a "a changed\n")
                          (expect (true? (git/file-dirty? a)))  ; modified → dirty
                          (let [n (io/file root "new.txt")]
                            (spit n "fresh\n")
                            (expect (false? (git/file-dirty? n)))) ; untracked is NOT dirty
                          (expect (false? (git/file-dirty? (io/file root "missing.txt")))) ; absent
                          (expect (false? (git/file-dirty? (io/file "/nonexistent/zzz.txt"))))) ; no repo
                        (finally (cleanup root))))))

(defdescribe repository-detection-test
             (it "detects a repository by shelling out to git"
                 (let [root (make-tmp-dir)]
                   (try (expect (false? (git/in-repository? root)))
                        (expect (= :none (git/vcs-kind root)))
                        (init-repo! root)
                        (expect (true? (git/in-repository? root)))
                        (expect (= :git (git/vcs-kind root)))
                        (expect (= (.getName root) (git/repo-name root)))
                        (finally (cleanup root))))))

(defdescribe status-snapshot-test
             (it "reports branch/head and porcelain entries (modified + untracked)"
                 (let [root (make-tmp-dir)]
                   (try (init-repo! root)
                        (spit-rel root "a.txt" "a\nb\n")
                        (spit-rel root "new.txt" "new")
                        (let [{:keys [entries clean? head]} (git/status-snapshot root)]
                          (expect (false? clean?))
                          (expect (string? head))
                          (expect (= [{:status "M" :file "a.txt"} {:status "??" :file "new.txt"}]
                                     (sort-by :file entries))))
                        (finally (cleanup root))))))

(defdescribe working-tree-status-test
             (it "counts modified/created/deleted for the footer and reports the branch"
                 (let [root (make-tmp-dir)]
                   (try (init-repo! root)
                        (spit-rel root "a.txt" "a\nb\n") ; modify tracked
                        (spit-rel root "new.txt" "new")  ; create untracked
                        (let
                          [{:keys [workspace? modified created deleted branch]}
                           (git/working-tree-status root)]
                          (expect (true? workspace?))
                          (expect (= 1 modified))
                          (expect (= 1 created))
                          (expect (= 0 deleted))
                          (expect (string? branch)))
                        (finally (cleanup root)))))
             (it "returns {:workspace? false} outside any repository"
                 (let [root (make-tmp-dir)]
                   (try (expect (false? (:workspace? (git/working-tree-status root))))
                        (finally (cleanup root))))))
