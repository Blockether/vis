(ns com.blockether.vis.ext.foundation.environment.git-test
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.foundation.environment.git :as git]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)
   (org.eclipse.jgit.api Git)))

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

(defn- init-repo!
  "Initialize a git repository at `dir`, write a single committed
   file, and configure a known author so `commit` succeeds without
   reading the user's global config."
  [^java.io.File dir]
  (with-open [git (-> (Git/init) (.setDirectory dir) .call)]
    (let [config (.. git getRepository getConfig)]
      (.setString config "user" nil "name"  "test")
      (.setString config "user" nil "email" "test@example.com")
      (.save config))
    (spit-rel dir "README.md" "# initial")
    (-> git .add (.addFilepattern "README.md") .call)
    (-> git .commit (.setMessage "init") .call)
    (.. git getRepository (getBranch))))

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

  (it "honors the :status? false escape hatch"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [snap (git/snapshot root {:status? false})]
          (expect (some? snap))
          (expect (true? (:status-unavailable? snap)))
          (expect (nil? (:clean? snap))))
        (finally (cleanup root))))))
