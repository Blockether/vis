(ns com.blockether.vis.internal.config.extension-save-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [yamlstar.core :as yamlstar]))

(defn- with-save-config
  [f]
  (let [directory
        (.toFile (java.nio.file.Files/createTempDirectory
                   "vis-save-config"
                   (make-array java.nio.file.attribute.FileAttribute 0)))

        project
        (doto (io/file directory "project") .mkdirs)

        global
        (doto (io/file directory "user/.vis") .mkdirs)]

    (try (with-redefs [workspace/cwd
                       (constantly (str project))

                       config/config-dir
                       (constantly (str global))]

           (f project global))
         (finally (config/invalidate-config-cache!)
                  (doseq [file (reverse (file-seq directory))]
                    (io/delete-file file true))))))

(def remote {"source" "https://github.com/example/tools" "version" "1.2.3"})

(deftest save-project-creates-config-and-resolves-local-source
  (with-save-config
    (fn [project _]
      (let [path
            (io/file project "vis.yml")

            plan
            (config/prepare-extension-save {:project true})]

        (is (not (.exists path)))
        (is (= (str path)
               (config/save-extension-declaration! plan
                                                   "tools"
                                                   {"source" (str (io/file project "tools"))})))
        (is (= {"extensions" {"tools" {"source" "./tools"}}} (yamlstar/load (slurp path))))
        (is (= #{"vis.yml"} (set (.list ^java.io.File project))))
        (is (= (.getCanonicalPath (io/file project "tools"))
               (get-in (second (config/extension-package-scopes))
                       [:packages "tools" "source"])))))))

(deftest save-project-preserves-comments-fields-and-yaml-spelling
  (with-save-config
    (fn [project _]
      (let [path
            (io/file project "vis.yaml")

            before
            (str "# owner comment\nagent_name: ${API_TOKEN} # placeholder\n"
                 "extensions: # packages\n    keep: # keep this comment\n"
                 "      source: ./keep\n# final comment\n...\n")]

        (spit path before)
        (config/save-extension-declaration! (config/prepare-extension-save {:project true})
                                            "tools"
                                            remote)
        (let [after (slurp path)]
          (is (not (.exists (io/file project "vis.yml"))))
          (is (str/starts-with? after "# owner comment\nagent_name: ${API_TOKEN} # placeholder\n"))
          (is (str/includes? after "    keep: # keep this comment\n      source: ./keep\n"))
          (is (str/ends-with? after "# final comment\n...\n"))
          (is (= remote (get-in (yamlstar/load after) ["extensions" "tools"]))))))))

(deftest save-project-handles-empty-map-and-document-end
  (doseq [before
          ["extensions: {} # packages\r\n" "# empty\n" "---\nagent_name: test\n...\n"
           "extensions: {}" "agent_name: test" "extensions:\n  keep:\n    source: ./keep"
           "\"extensions\": # quoted key\n  # package comment\n    keep:\n      source: ./keep\n"]]
    (with-save-config
      (fn [project _]
        (let [path (io/file project "vis.yml")]
          (spit path before)
          (config/save-extension-declaration! (config/prepare-extension-save {:project true})
                                              "tools"
                                              remote)
          (is (= remote (get-in (yamlstar/load (slurp path)) ["extensions" "tools"])))
          (when (str/includes? before "\r\n") (is (not (re-find #"(?<!\r)\n" (slurp path))))))))))

(deftest save-refuses-unsafe-config-before-admission
  (doseq [before ["[broken" "agent_name: one\n---\nagent_name: two\n"
                  "extensions: {}\nextensions: {}\n" "unknown_key: true\n" "{agent_name: test}\n"
                  "extensions: {keep: {source: ./keep}}\n"]]
    (with-save-config
      (fn [project _]
        (let [path (io/file project "vis.yml")]
          (spit path before)
          (is (try (config/prepare-extension-save {:project true}) false (catch Exception _ true))
              before)
          (is (= before (slurp path))))))))

(deftest save-refuses-concurrent-edits-and-conflicting-declarations
  (with-save-config (fn [project _]
                      (let [path
                            (io/file project "vis.yml")

                            plan
                            (config/prepare-extension-save {:project true})]

                        (spit path "agent_name: changed\n")
                        (is (try (config/save-extension-declaration! plan "tools" remote)
                                 false
                                 (catch clojure.lang.ExceptionInfo _ true)))
                        (is (= "agent_name: changed\n" (slurp path))))))
  (with-save-config (fn [project _]
                      (let [path
                            (io/file project "vis.yml")

                            before
                            "extensions:\n  tools:\n    source: ./other\n"]

                        (spit path before)
                        (is (try (config/save-extension-declaration! (config/prepare-extension-save
                                                                       {:project true})
                                                                     "tools"
                                                                     remote)
                                 false
                                 (catch clojure.lang.ExceptionInfo _ true)))
                        (is (= before (slurp path)))))))

(deftest save-is-idempotent-and-refuses-overlay-conflicts
  (with-save-config
    (fn [project _]
      (let [path
            (io/file project "vis.yml")

            before
            "extensions:\n  tools: # retained\n    source: ./tools\n"]

        (spit path before)
        (config/save-extension-declaration! (config/prepare-extension-save {:project true})
                                            "tools"
                                            {"source" (str (io/file project "tools"))})
        (is (= before (slurp path)))
        (.mkdirs (io/file project ".vis"))
        (spit (io/file project ".vis/config.yml") "extensions:\n  other:\n    source: ./other\n")
        (is (try (config/save-extension-declaration! (config/prepare-extension-save {:project true})
                                                     "other"
                                                     remote)
                 false
                 (catch clojure.lang.ExceptionInfo _ true)))
        (is (= before (slurp path)))))))

(deftest global-save-uses-machine-store-without-copying-handwritten-fields
  (with-save-config
    (fn [_ global]
      (let [manual
            (io/file global "config.yml")

            state
            (io/file global "state.yml")]

        (spit manual "agent_name: handwritten\n")
        (spit state "agent_name: ${API_TOKEN}\n")
        (is (= (str state)
               (config/save-extension-declaration! (config/prepare-extension-save {})
                                                   "tools"
                                                   remote)))
        (is (= "agent_name: handwritten\n" (slurp manual)))
        (is (= "${API_TOKEN}" (get (yamlstar/load (slurp state)) "agent_name")))
        (is (= remote (get-in (yamlstar/load (slurp state)) ["extensions" "tools"])))))))

(deftest project-save-locks-the-write-and-refuses-a-stale-plan
  (with-save-config
    (fn [project global]
      (is (.delete ^java.io.File global))
      (let [first-plan
            (config/prepare-extension-save {:project true})

            stale-plan
            (config/prepare-extension-save {:project true})

            lock-file
            (io/file global "state.yml.lock")

            render
            @#'config/project-extension-text

            locked?
            (atom false)]

        (is (not (.exists lock-file)))
        (with-redefs-fn {#'config/project-extension-text
                         (fn [& args]
                           (with-open [channel (java.nio.channels.FileChannel/open
                                                 (.toPath lock-file)
                                                 (into-array
                                                   java.nio.file.OpenOption
                                                   [java.nio.file.StandardOpenOption/WRITE]))]
                             (try (when-let [lock (.tryLock channel)]
                                    (.release lock))
                                  (catch java.nio.channels.OverlappingFileLockException _
                                    (reset! locked? true))))
                           (apply render args))}
          #(config/save-extension-declaration! first-plan "first" remote))
        (is @locked?)
        (is (try (config/save-extension-declaration! stale-plan "second" remote)
                 false
                 (catch clojure.lang.ExceptionInfo _ true)))
        (is (= {"first" remote}
               (get (yamlstar/load (slurp (io/file project "vis.yml"))) "extensions")))))))
