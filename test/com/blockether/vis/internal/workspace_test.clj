(ns com.blockether.vis.internal.workspace-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir
  [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree!
  [^java.io.File root]
  (when (and root (.exists root))
    (doseq [f (reverse (file-seq root))]
      (.delete f))))

(defn- git!
  [dir & args]
  (let [result (apply sh/sh (concat ["git" "-C" (.getCanonicalPath (io/file dir))] args))]
    (when-not (zero? (:exit result))
      (throw (ex-info (:err result) {:args args :result result})))
    (str/trim (or (:out result) ""))))

(defn- init-repo!
  [root]
  (let [init (sh/sh "git" "init" (.getCanonicalPath (io/file root)))]
    (when-not (zero? (:exit init))
      (throw (ex-info (:err init) {:result init}))))
  (git! root "config" "user.name" "Vis Test")
  (git! root "config" "user.email" "vis@example.invalid")
  (spit (io/file root "README.md") "# repo\n")
  (git! root "add" "README.md")
  (git! root "commit" "-m" "init"))

(defdescribe workspace-root-test
  (it "canonicalizes workspace roots and falls back to process cwd"
    (let [cwd (.getCanonicalPath (io/file (System/getProperty "user.dir")))]
      (expect (= cwd (.getCanonicalPath (workspace/cwd))))
      (expect (= cwd (workspace/workspace-root {:workspace/root "."})))
      (binding [workspace/*workspace-root* cwd]
        (expect (= cwd (.getCanonicalPath (workspace/cwd))))))))

(defdescribe workspace-manager-test
  (it "creates a real git worktree under Vis runtime state"
    (let [repo     (temp-dir "vis-workspace-repo-")
          vis-home (temp-dir "vis-workspace-home-")]
      (try
        (init-repo! repo)
        (binding [workspace/*vis-home* vis-home]
          (let [created (workspace/create-worktree! {:repo-root (.getCanonicalPath repo)
                                                     :branch "feature/workspaces"
                                                     :workspace-id "ws-basic"})
                root    (:workspace/root created)
                status  (workspace/workspace-status "ws-basic")
                roots   (workspace/workspace-roots "ws-basic")
                state   (edn/read-string (slurp (io/file vis-home "workspaces.edn")))]
            (expect (= "ws-basic" (:workspace/id created)))
            (expect (= "feature/workspaces" (get-in created [:main :branch])))
            (expect (= (.getCanonicalPath (io/file vis-home "workspaces" (:workspace/repo-id created) "ws-basic"))
                      root))
            (expect (.exists (io/file root "README.md")))
            (expect (= "feature/workspaces" (git! root "rev-parse" "--abbrev-ref" "HEAD")))
            (expect (= created (get-in state [:workspaces "ws-basic"])))
            (expect (= "feature/workspaces" (:git/branch status)))
            (expect (false? (:git/dirty? status)))
            (expect (= root (:workspace/root roots)))
            (expect (= root (:main/root roots)))))
        (finally
          (delete-tree! vis-home)
          (delete-tree! repo))))))
