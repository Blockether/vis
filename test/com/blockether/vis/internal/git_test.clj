(ns com.blockether.vis.internal.git-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe count-status-sets-test
  (it "counts paths across status buckets"
    (expect (= 3 (git/count-status-sets #{"a" "b"} #{"c"} nil)))))

(defdescribe workspace-status-shape-test
  (it "resolves cwd from the active workspace root binding"
    (let [root (-> (java.nio.file.Files/createTempDirectory
                     "vis-git-cwd-"
                     (into-array java.nio.file.attribute.FileAttribute []))
                 .toFile)]
      (try
        (binding [workspace/*workspace-root* (.getCanonicalPath root)]
          (expect (= (.getCanonicalPath root)
                    (.getCanonicalPath (git/cwd-file)))))
        (finally
          (doseq [f (reverse (file-seq root))]
            (.delete f))))))

  (it "reports whether the current directory is inside a git workspace"
    (let [status (git/workspace-status)]
      (expect (contains? status :workspace?))
      (when (:workspace? status)
        (expect (string? (:repo status)))
        (expect (string? (:branch status)))
        (expect (number? (:modified status)))
        (expect (number? (:created status)))
        (expect (number? (:deleted status)))
        (expect (contains? status :upstream?))
        (expect (number? (:ahead status)))
        (expect (number? (:behind status))))))

  (it "does not count ignored-only directories as created files"
    (let [root (-> (java.nio.file.Files/createTempDirectory
                     "vis-git-ignored-only-"
                     (into-array java.nio.file.attribute.FileAttribute []))
                 .toFile)
          spit-rel (fn [rel content]
                     (let [f (io/file root rel)]
                       (when-let [parent (.getParentFile f)]
                         (.mkdirs parent))
                       (spit f content)))
          cleanup! (fn []
                     (when (.exists root)
                       (doseq [f (reverse (file-seq root))]
                         (.delete f))))]
      (try
        (with-open [g (-> (org.eclipse.jgit.api.Git/init)
                        (.setDirectory root)
                        .call)]
          (let [config (.. g getRepository getConfig)]
            (.setString config "user" nil "name" "test")
            (.setString config "user" nil "email" "test@example.com")
            (.setBoolean config "commit" nil "gpgsign" false)
            (.save config))
          (spit-rel "README.md" "# init")
          (spit-rel ".gitignore" ".claude/settings.local.json\n")
          (-> g .add (.addFilepattern "README.md") .call)
          (-> g .add (.addFilepattern ".gitignore") .call)
          (-> g .commit (.setMessage "init") .call))
        (spit-rel ".claude/settings.local.json" "{}")
        (let [status (git/workspace-status root)]
          (expect (true? (:workspace? status)))
          (expect (= 0 (:modified status)))
          (expect (= 0 (:created status)))
          (expect (= 0 (:deleted status))))
        (finally
          (cleanup!)))))

  (it "caches resolved workspace status for hot UI callers"
    (let [calls (atom 0)]
      (with-redefs [git/workspace-status (fn []
                                           (swap! calls inc)
                                           {:workspace? false})
                    git/cwd-file (fn []
                                   (java.io.File. "."))]
        (expect (= {:workspace? false} (git/cached-workspace-status 1000 5000)))
        (expect (= {:workspace? false} (git/cached-workspace-status 2000 5000)))
        (expect (= 1 @calls))))))
