(ns com.blockether.vis.internal.foundation.environment.git-test
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.internal.foundation.environment.git :as git]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir ^java.io.File []
  (let [path (Files/createTempDirectory "vis-env-git-"
               (into-array FileAttribute []))]
    (.toFile path)))

(defn- spit-rel [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- git! [^java.io.File root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let [p (.start pb)]
      (slurp (.getInputStream p))
      (.waitFor p))))

(defn- git-out ^String [^java.io.File root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (let [p   (.start pb)
          out (slurp (.getInputStream p))]
      (.waitFor p)
      (.trim ^String out))))

(defn- init-repo!
  "Initialize a git repository at `dir`, write a single committed
   file, and configure a known author so `commit` succeeds without
   reading the user's global config."
  [^java.io.File dir]
  (git! dir "init" "-q")
  (git! dir "config" "user.name" "test")
  (git! dir "config" "user.email" "test@example.com")
  (git! dir "config" "commit.gpgsign" "false")
  (spit-rel dir "README.md" "# initial")
  (git! dir "add" "README.md")
  (git! dir "commit" "-q" "-m" "init")
  (git-out dir "rev-parse" "--abbrev-ref" "HEAD"))

(defdescribe git-snapshot-test
  (it "returns nil outside any git repository"
    (let [root (make-tmp-dir)]
      (try
        (spit-rel root "README.md" "no git here")
        (expect (nil? (git/snapshot root)))
        (finally (cleanup root)))))

  (it "reports root, branch, and clean status for a fresh repo"
    (let [root (make-tmp-dir)]
      (try
        (let [_branch (init-repo! root)
              snap   (git/snapshot root)]
          (expect (some? snap))
          (expect (string? (:root snap)))
          (expect (string? (:git-dir snap)))
          (expect (false? (:detached? snap)))
          (expect (false? (:worktree? snap)))
          (expect (false? (:submodules? snap)))
          (expect (true? (:clean? snap)))
          (expect (false? (:dirty? snap)))
          (expect (false? (:changes? snap)))
          (expect (= 0 (long (:stash-count snap))))
          (expect (= 0 (long (:modified snap))))
          (expect (= 0 (long (:untracked snap)))))
        (finally (cleanup root)))))

  (it "reports a dirty working tree when files are added or modified"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "untracked.txt" "new file")
        (spit-rel root "README.md"     "# changed")
        (let [snap (git/snapshot root)]
          (expect (false? (:clean? snap)))
          (expect (true? (:dirty? snap)))
          (expect (true? (:changes? snap)))
          (expect (>= (long (:modified snap)) 1))
          (expect (>= (long (:untracked snap)) 1)))
        (finally (cleanup root)))))

  (it "detects submodules from a .gitmodules file"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root ".gitmodules"
          "[submodule \"foo\"]\n  path = foo\n  url = ./foo")
        (let [snap (git/snapshot root)]
          (expect (true? (:submodules? snap))))
        (finally (cleanup root)))))

  (it "reports stash count"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "README.md" "# stashed")
        (git! root "stash" "push" "-m" "test stash")
        (let [snap (git/snapshot root)]
          (expect (= 1 (long (:stash-count snap)))))
        (finally (cleanup root)))))

  (it "marks a branch stale when its configured upstream is ahead"
    (let [root (make-tmp-dir)]
      (try
        (let [branch (init-repo! root)]
          (git! root "branch" "base")
          (git! root "checkout" "-q" "base")
          (spit-rel root "base.txt" "base")
          (git! root "add" "base.txt")
          (git! root "commit" "-q" "-m" "base commit")
          (git! root "checkout" "-q" branch)
          (git! root "config" (str "branch." branch ".remote") ".")
          (git! root "config" (str "branch." branch ".merge") "refs/heads/base")
          (let [snap (git/snapshot root)]
            (expect (= "base" (:upstream snap)))
            (expect (= 0 (long (:ahead snap))))
            (expect (= 1 (long (:behind snap))))
            (expect (true? (:stale? snap)))))
        (finally (cleanup root)))))

  (it "honors the :status? false escape hatch"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [snap (git/snapshot root {:status? false})]
          (expect (some? snap))
          (expect (true? (:status-unavailable? snap)))
          (expect (nil? (:clean? snap))))
        (finally (cleanup root))))))
