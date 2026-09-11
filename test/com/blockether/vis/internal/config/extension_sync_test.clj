(ns com.blockether.vis.internal.config.extension-sync-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.validation :as validation]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest extension-declarations-are-closed-and-explicit
  (is (validation/valid? {"extensions" {"vis-spel" {"source" "https://github.com/Blockether/spel"
                                                    "subdirectory" "extensions/vis-spel"
                                                    "version" "0.1.0"}}}))
  (doseq [entry [{"source" "./tools" "trust" true} {"source" "./tools" "revision" "main"}
                 {"source" "./tools" "version" "1" "revision" (apply str (repeat 40 "a"))} {}]]
    (is (not (validation/valid? {"extensions" {"vis-tools" entry}}))))
  (is (not (validation/valid? {"extensions" {"../outside" {"source" "./tools"}}}))))

(deftest scopes-resolve-relative-paths-and-replace-complete-declarations
  (let [directory
        (.toFile (java.nio.file.Files/createTempDirectory
                   "vis-sync-config"
                   (make-array java.nio.file.attribute.FileAttribute 0)))

        global
        (doto (io/file directory "user/.vis") .mkdirs)

        project
        (doto (io/file directory "project/.vis") .mkdirs)]

    (try
      (spit
        (io/file global "config.yml")
        "extensions:
  shared:
    source: https://github.com/example/global
    version: '1.0.0'
  global-only:
    source: ./global-tools
")
      (spit (io/file global "state.yml") "extensions:
  shared:
    source: ./from-state
")
      (spit
        (io/file (.getParentFile project) "vis.yml")
        "extensions:
  shared:
    source: https://github.com/example/project
    revision: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
")
      (spit (io/file project "config.yml") "extensions:
  shared:
    source: ../local-tools
")
      (with-redefs [config/config-dir
                    (constantly (str global))

                    workspace/cwd
                    (constantly (str (.getParentFile project)))]

        (let [[global-scope project-scope] (config/extension-package-scopes)]
          (is (= #{"shared" "global-only"} (set (keys (:packages global-scope)))))
          (is (= {"source" (.getCanonicalPath (io/file global "from-state"))}
                 (get-in global-scope [:packages "shared"])))
          (is (= {"source" (.getCanonicalPath (io/file project "../local-tools"))}
                 (get-in project-scope [:packages "shared"])))
          (is (= "project" (:scope project-scope)))
          (is (= (str (io/file project "extensions")) (:directory project-scope)))
          (is (= {"source" "../local-tools"}
                 (get-in (config/load-config-raw) ["extensions" "shared"])))))
      (with-redefs [config/config-dir
                    (constantly (str global))

                    workspace/cwd
                    (constantly (str (.getParentFile global)))]

        (is (= 1 (count (config/extension-package-scopes)))))
      (spit (io/file project "config.yml")
            "extensions:
  broken:
    source: ./tools
    trust: true
")
      (with-redefs [config/config-dir
                    (constantly (str global))

                    workspace/cwd
                    (constantly (str (.getParentFile project)))]

        (is
          (try (config/extension-package-scopes) false (catch clojure.lang.ExceptionInfo _ true))))
      (finally (config/invalidate-config-cache!)
               (doseq [file (reverse (file-seq directory))]
                 (io/delete-file file true))))))
