(ns com.blockether.vis.internal.git-test
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.internal.git :as git]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)
   (org.eclipse.jgit.api Git)))

(defn- make-tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-internal-git-"
             (into-array FileAttribute []))))

(defn- spit-rel [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- init-repo! [^java.io.File root]
  (with-open [g (-> (Git/init) (.setDirectory root) .call)]
    (let [config (.. g getRepository getConfig)]
      (.setString config "user" nil "name" "Vis Test")
      (.setString config "user" nil "email" "vis-test@example.invalid")
      (.save config))
    (spit-rel root "a.txt" "a\n")
    (-> g .add (.addFilepattern "a.txt") .call)
    (-> g .commit (.setMessage "base") .call)))

(defdescribe file-dirty?-test
  (it "is false for clean/untracked/repo-less, true once a tracked file is modified"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [a (io/file root "a.txt")]
          (expect (false? (git/file-dirty? a)))          ; clean tracked
          (spit a "a changed\n")
          (expect (true? (git/file-dirty? a)))           ; modified → dirty
          (let [n (io/file root "new.txt")]
            (spit n "fresh\n")
            (expect (false? (git/file-dirty? n))))        ; untracked is NOT dirty
          (expect (false? (git/file-dirty? (io/file root "missing.txt")))) ; absent
          (expect (false? (git/file-dirty? (io/file "/nonexistent/zzz.txt"))))) ; no repo
        (finally (cleanup root))))))

(defdescribe jgit-shared-test
  (it "detects repositories without shelling out to git"
    (let [root (make-tmp-dir)]
      (try
        (expect (false? (git/in-repository? root)))
        (init-repo! root)
        (expect (true? (git/in-repository? root)))
        (finally (cleanup root)))))

  (it "returns status entries and diff numstat from JGit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "a.txt" "a\nb\n")
        (spit-rel root "new.txt" "new")
        (let [status (git/status-snapshot root)
              diff   (git/diff-numstat root "HEAD" nil)]
          (expect (= [{:status "M" :file "a.txt"}
                      {:status "??" :file "new.txt"}]
                    (:entries status)))
          (expect (= [{:file "a.txt" :add 1 :del 0}]
                    diff)))
        (finally (cleanup root)))))

  (it "returns recent commits from JGit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [commit (first (git/recent-commits root 1))]
          (expect (= "base" (:subject commit)))
          (expect (= "Vis Test" (:author commit)))
          (expect (string? (:sha commit)))
          (expect (integer? (:at commit))))
        (finally (cleanup root))))))
