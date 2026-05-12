(ns com.blockether.vis.ext.foundation.environment.core-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.environment.core :as env-core]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)
   (org.eclipse.jgit.api Git)))

(defn- make-tmp-dir ^java.io.File []
  (let [path (Files/createTempDirectory "vis-env-core-"
               (into-array FileAttribute []))]
    (.toFile path)))

(defn- spit-rel [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (when-let [parent (.getParentFile f)]
      (.mkdirs parent))
    (spit f content)))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- init-repo-on-branch!
  [^java.io.File root branch]
  (with-open [git (-> (Git/init) (.setDirectory root) .call)]
    (let [config (.. git getRepository getConfig)]
      (.setString config "user" nil "name" "test")
      (.setString config "user" nil "email" "test@example.com")
      (.setBoolean config "commit" nil "gpgsign" false)
      (.save config))
    (spit-rel root "README.md" "# initial")
    (-> git .add (.addFilepattern "README.md") .call)
    (-> git .commit (.setMessage "init") .call)
    (.. git (branchCreate) (setName branch) (call))
    (.. git (checkout) (setName branch) (call))))

(defdescribe environment-core-test
  (it "exports the expected environment symbol surface"
    (let [syms (set (map :ext.symbol/symbol env-core/environment-symbols))]
      (expect (contains? syms 'snapshot))
      (expect (contains? syms 'git))
      (expect (contains? syms 'repositories))
      (expect (contains? syms 'languages))
      (expect (contains? syms 'monorepo))
      (expect (contains? syms 'refresh!))
      (expect (contains? syms 'render))
      (expect (contains? syms 'main-agent-instructions))
      (expect (not (contains? syms 'load-skill!)))
      (expect (not (contains? syms 'load-skill)))
      (expect (contains? syms 'scan-warnings))
      (expect (contains? syms 'reload-instructions!))
      (expect (contains? syms 'reload-skills!))
      (expect (contains? syms 'reload-extensions!))))

  (it "renders a prompt fragment for the unified v/ alias"
    (let [prompt (env-core/environment-prompt {})]
      (expect (string? prompt))
      (expect (str/includes? prompt "(v/snapshot)"))
      (expect (not (str/includes? prompt "v/load-skill")))
      (expect (str/includes? prompt "(v/reload-extensions!)"))
      (expect (not (str/includes? prompt "`md/`")))))

  (it "renders foundation environment info separately from prompt extras"
    (let [info (env-core/environment-info {})]
      (expect (string? info))
      (expect (str/includes? info "<environment>"))
      (expect (str/includes? info "git.summary"))))

  (it "uses active workspace root instead of JVM cwd for v/git snapshots"
    (let [root   (make-tmp-dir)
          branch "feature/ws"]
      (try
        (init-repo-on-branch! root branch)
        (binding [workspace/*workspace-root* (.getCanonicalPath root)]
          (let [git (:git (env-core/refresh!))]
            (expect (= branch (:branch git)))
            (expect (= (.getCanonicalPath root) (:root git)))))
        (finally
          (binding [workspace/*workspace-root* nil]
            (env-core/refresh!))
          (cleanup root))))))
