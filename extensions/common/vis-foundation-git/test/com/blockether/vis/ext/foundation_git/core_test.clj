(ns com.blockether.vis.ext.foundation-git.core-test
  (:require
   [clojure.java.io :as io]
   [clojure.string]
   [com.blockether.vis.ext.foundation-git.core :as git]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)
   (org.eclipse.jgit.api Git)))

(defn- make-tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-foundation-git-"
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
    (spit-rel root "src/a.clj" "(ns a)\n")
    (-> g .add (.addFilepattern "src/a.clj") .call)
    (-> g .commit (.setMessage "base") .call)))

(defdescribe git-diff-test
  (it "accepts optional opts map and reads diff/status through JGit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "src/a.clj" "(ns a)\n(def x 1)\n")
        (let [result (git/git-diff-fn {:workspace/root (.getCanonicalPath root)} {:stat? true})]
          (expect (extension/tool-result? result))
          (expect (= 1 (get-in result [:result :stat :files])))
          (expect (= [{:file "src/a.clj" :+ 1 :- 0}] (get-in result [:result :files])))
          (expect (= [{:status "M" :file "src/a.clj"}] (get-in result [:result :porcelain]))))
        (finally (cleanup root)))))

  (it "rejects non-map opts"
    (try
      (git/git-diff-fn {:workspace/root "/repo"} :bad)
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))
        (expect (clojure.string/includes? (ex-message e)
                  "git/diff expected optional opts map, got :bad"))
        (expect (clojure.string/includes? (ex-message e)
                  "Call (git/diff) or (git/diff {:stat? true})."))))))

(defdescribe git-status-test
  (it "returns branch, head, cleanliness, and entries from JGit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (spit-rel root "new.txt" "new")
        (let [result (git/git-status-fn {:workspace/root (.getCanonicalPath root)})]
          (expect (extension/tool-result? result))
          (expect (string? (get-in result [:result :branch])))
          (expect (string? (get-in result [:result :head])))
          (expect (false? (get-in result [:result :clean?])))
          (expect (some #(= {:status "??" :file "new.txt"} %)
                    (get-in result [:result :entries]))))
        (finally (cleanup root))))))

(defdescribe git-log-test
  (it "returns recent commits from JGit"
    (let [root (make-tmp-dir)]
      (try
        (init-repo! root)
        (let [result (git/git-log-fn {:workspace/root (.getCanonicalPath root)} 1)
              commit (first (get-in result [:result :commits]))]
          (expect (extension/tool-result? result))
          (expect (= 1 (count (get-in result [:result :commits]))))
          (expect (= "base" (:subject commit)))
          (expect (= "Vis Test" (:author commit)))
          (expect (string? (:sha commit)))
          (expect (integer? (:at commit))))
        (finally (cleanup root))))))
