(ns com.blockether.vis.internal.config.extension-save-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]]
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

(defdescribe save-project-creates-config-and-resolves-local-source
             (it "save project creates config and resolves local source"
                 (with-save-config
                   (fn [project _]
                     (let [path
                           (io/file project "vis.yml")

                           plan
                           (config/prepare-extension-save {:project true})]

                       (expect (not (.exists path)))
                       (expect (= (str path)
                                  (config/save-extension-declaration!
                                    plan
                                    "tools"
                                    {"source" (str (io/file project "tools"))})))
                       (expect (= {"extensions" {"tools" {"source" "./tools"}}}
                                  (yamlstar/load (slurp path))))
                       (expect (= #{"vis.yml"} (set (.list ^java.io.File project))))
                       (expect (= (.getCanonicalPath (io/file project "tools"))
                                  (get-in (second (config/extension-package-scopes))
                                          [:packages "tools" "source"]))))))))

(defdescribe
  save-project-preserves-comments-fields-and-yaml-spelling
  (it "save project preserves comments fields and yaml spelling"
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
              (expect (not (.exists (io/file project "vis.yml"))))
              (expect (str/starts-with?
                        after
                        "# owner comment\nagent_name: ${API_TOKEN} # placeholder\n"))
              (expect (str/includes? after "    keep: # keep this comment\n      source: ./keep\n"))
              (expect (str/ends-with? after "# final comment\n...\n"))
              (expect (= remote (get-in (yamlstar/load after) ["extensions" "tools"])))))))))

(defdescribe
  save-project-handles-empty-map-and-document-end
  (it "save project handles empty map and document end"
      (doseq
        [before
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
              (expect (= remote (get-in (yamlstar/load (slurp path)) ["extensions" "tools"])))
              (when (str/includes? before "\r\n")
                (expect (not (re-find #"(?<!\r)\n" (slurp path)))))))))))

(defdescribe save-refuses-unsafe-config-before-admission
             (it "save refuses unsafe config before admission"
                 (doseq [before ["[broken" "agent_name: one\n---\nagent_name: two\n"
                                 "extensions: {}\nextensions: {}\n" "unknown_key: true\n"
                                 "{agent_name: test}\n" "extensions: {keep: {source: ./keep}}\n"]]
                   (with-save-config (fn [project _]
                                       (let [path (io/file project "vis.yml")]
                                         (spit path before)
                                         (expect (try (config/prepare-extension-save {:project
                                                                                      true})
                                                      false
                                                      (catch Exception _ true))
                                                 before)
                                         (expect (= before (slurp path)))))))))

(defdescribe
  save-refuses-concurrent-edits-and-conflicting-declarations
  (it "save refuses concurrent edits and conflicting declarations"
      (with-save-config (fn [project _]
                          (let [path
                                (io/file project "vis.yml")

                                plan
                                (config/prepare-extension-save {:project true})]

                            (spit path "agent_name: changed\n")
                            (expect (try (config/save-extension-declaration! plan "tools" remote)
                                         false
                                         (catch clojure.lang.ExceptionInfo _ true)))
                            (expect (= "agent_name: changed\n" (slurp path))))))
      (with-save-config (fn [project _]
                          (let [path
                                (io/file project "vis.yml")

                                before
                                "extensions:\n  tools:\n    source: ./other\n"]

                            (spit path before)
                            (expect (try (config/save-extension-declaration!
                                           (config/prepare-extension-save {:project true})
                                           "tools"
                                           remote)
                                         false
                                         (catch clojure.lang.ExceptionInfo _ true)))
                            (expect (= before (slurp path))))))))

(defdescribe save-is-idempotent-and-refuses-overlay-conflicts
             (it "save is idempotent and refuses overlay conflicts"
                 (with-save-config
                   (fn [project _]
                     (let [path
                           (io/file project "vis.yml")

                           before
                           "extensions:\n  tools: # retained\n    source: ./tools\n"]

                       (spit path before)
                       (config/save-extension-declaration!
                         (config/prepare-extension-save {:project true})
                         "tools"
                         {"source" (str (io/file project "tools"))})
                       (expect (= before (slurp path)))
                       (.mkdirs (io/file project ".vis"))
                       (spit (io/file project ".vis/config.yml")
                             "extensions:\n  other:\n    source: ./other\n")
                       (expect (try (config/save-extension-declaration!
                                      (config/prepare-extension-save {:project true})
                                      "other"
                                      remote)
                                    false
                                    (catch clojure.lang.ExceptionInfo _ true)))
                       (expect (= before (slurp path))))))))

(defdescribe
  global-save-uses-machine-store-without-copying-handwritten-fields
  (it "global save uses machine store without copying handwritten fields"
      (with-save-config
        (fn [_ global]
          (let [manual
                (io/file global "config.yml")

                state
                (io/file global "state.yml")]

            (spit manual "agent_name: handwritten\n")
            (spit state "agent_name: ${API_TOKEN}\n")
            (expect (= (str state)
                       (config/save-extension-declaration! (config/prepare-extension-save {})
                                                           "tools"
                                                           remote)))
            (expect (= "agent_name: handwritten\n" (slurp manual)))
            (expect (= "${API_TOKEN}" (get (yamlstar/load (slurp state)) "agent_name")))
            (expect (= remote (get-in (yamlstar/load (slurp state)) ["extensions" "tools"]))))))))

(defdescribe
  project-save-locks-the-write-and-refuses-a-stale-plan
  (it
    "project save locks the write and refuses a stale plan"
    (with-save-config
      (fn [project global]
        (expect (.delete ^java.io.File global))
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

          (expect (not (.exists lock-file)))
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
          (expect @locked?)
          (expect (try (config/save-extension-declaration! stale-plan "second" remote)
                       false
                       (catch clojure.lang.ExceptionInfo _ true)))
          (expect (= {"first" remote}
                     (get (yamlstar/load (slurp (io/file project "vis.yml"))) "extensions"))))))))
