(ns com.blockether.vis.internal.sandbox.policy-test
  (:require [com.blockether.vis.internal.config.validation :as config-validation]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file FileVisitOption Files Path]))

(defn- with-java-installations
  [f]
  (let [root
        (.toRealPath (Files/createTempDirectory "vis-java-policy"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))
                     (make-array java.nio.file.LinkOption 0))

        first-home
        (.resolve root "jdk-one")

        second-home
        (.resolve root "jdk-two")

        launcher
        (.resolve root "launcher")

        current
        (.resolve root "current")]

    (try (doseq [^Path home [first-home second-home launcher]]
           (Files/createDirectories (.resolve home "bin")
                                    (make-array java.nio.file.attribute.FileAttribute 0))
           (Files/createDirectories (.resolve home "lib")
                                    (make-array java.nio.file.attribute.FileAttribute 0))
           (let [executable (.toFile (.resolve home "bin/java"))]
             (spit executable "# Test installation marker; never executed.\n")
             (expect (.setExecutable executable true)))
           (when-not (= home launcher) (spit (.toFile (.resolve home "release")) "test runtime\n")))
         (Files/createSymbolicLink current
                                   first-home
                                   (make-array java.nio.file.attribute.FileAttribute 0))
         (f {:first-home first-home :second-home second-home :launcher launcher :current current})
         (finally (with-open [paths (Files/walk root (make-array FileVisitOption 0))]
                    (doseq [^Path path (reverse (iterator-seq (.iterator paths)))]
                      (Files/deleteIfExists path)))))))

(defn- java-snapshot
  [config roots]
  (with-redefs-fn {#'policy/java-read-roots (constantly roots)} #(policy/snapshot config)))

(defdescribe
  java-runtime-discovery-test
  (it "finds exact installations through JAVA_HOME and PATH without a host JVM"
      (with-java-installations
        (fn [{:keys [first-home second-home current]}]
          (let [first-root
                (str first-home)

                second-root
                (str second-home)

                environment
                {"JAVA_HOME" (str current)
                 "PATH" (str current "/bin" File/pathSeparator second-home "/bin")}]

            (expect (= [first-root] (#'policy/java-read-roots nil environment)))
            (expect (= [first-root] (#'policy/java-read-roots first-root environment)))
            (expect (= [first-root]
                       (#'policy/java-read-roots nil (dissoc environment "JAVA_HOME"))))
            (expect (= [first-root second-root]
                       (#'policy/java-read-roots first-root {"JAVA_HOME" second-root})))))))
  (it "ignores absent or invalid homes and does not infer a runtime behind a launcher"
      (with-java-installations
        (fn [{:keys [first-home launcher]}]
          (expect (= [] (#'policy/java-read-roots nil {})))
          (expect (= [] (#'policy/java-read-roots "relative" {"JAVA_HOME" "" "PATH" "."})))
          (expect (= [] (#'policy/java-read-roots (str (char 0)) {"PATH" (str (char 0))})))
          (expect (= []
                     (#'policy/java-read-roots
                      (str launcher)
                      {"PATH" (str launcher "/bin" File/pathSeparator first-home "/bin")}))))))
  (it "freezes symlink identity until a new snapshot and hashes a runtime change"
      (with-java-installations
        (fn [{:keys [first-home second-home current]}]
          (let [config
                {"jail" {"enabled" true}}

                roots
                #(#'policy/java-read-roots nil {"JAVA_HOME" (str current)})

                before
                (java-snapshot config (roots))]

            (Files/delete ^Path current)
            (Files/createSymbolicLink current
                                      second-home
                                      (make-array java.nio.file.attribute.FileAttribute 0))
            (let [after (java-snapshot config (roots))]
              (expect (= [(str first-home)] (get-in before [:process-jail :allow-read])))
              (expect (= [(str second-home)] (get-in after [:process-jail :allow-read])))
              (expect (not= (:generation before) (:generation after)))
              (expect (= (:generation after) (:generation (java-snapshot config (roots))))))))))
  (it "preserves explicit modes, descriptions and search settings without duplicate grants"
      (with-java-installations
        (fn [{:keys [first-home current]}]
          (let [root
                (str first-home)

                config
                {"workspace" {"filesystem" [{"id" "java"
                                             "path" (str current)
                                             "access" "read-only"
                                             "search" true
                                             "description" "Selected Java runtime"}]}
                 "jail" {"enabled" true "filesystem" {"allow" ["java"]}}}

                read-only
                (java-snapshot config [root])

                writable
                (java-snapshot (assoc-in config ["workspace" "filesystem" 0 "access"] "read-write")
                               [root])]

            (expect (= [root] (get-in read-only [:process-jail :allow-read])))
            (expect (not (some #{root} (policy/no-search-roots read-only))))
            (expect (= "Selected Java runtime"
                       (get-in read-only [:process-jail :path-descriptions root])))
            (expect (some #{root} (policy/read-write-roots writable)))
            (expect (empty? (get-in writable [:process-jail :allow-read])))
            (expect (not (some #{root} (policy/no-search-roots writable))))))))
  (it "does not add process grants when the jail is disabled"
      (let [snapshot (java-snapshot {"jail" {"enabled" false}} ["/toolchains/java"])]
        (expect (empty? (get-in snapshot [:process-jail :allow-read])))
        (expect (not (some #{"/toolchains/java"} (get-in snapshot [:process-jail :no-search])))))))

(defdescribe automatic-java-read-access-test
             (it "admits the running JVM read-only without a workspace grant"
                 (let [java-home
                       (.getCanonicalPath (java.io.File. (System/getProperty "java.home")))

                       snapshot
                       (policy/snapshot {"jail" {"enabled" true "filesystem" {"allow" []}}})

                       view
                       (policy/access-view snapshot [])

                       rendered
                       (policy/home-relative java-home)]

                   (expect (some #{java-home} (get-in snapshot [:process-jail :allow-read])))
                   (expect (some #{rendered} (get-in view ["filesystem" "process_read_only"])))
                   (expect (some #{java-home} (policy/no-search-roots snapshot)))
                   (expect (string? (get-in view ["filesystem" "descriptions" rendered])))
                   (expect (not (some #{java-home} (policy/read-write-roots snapshot))))
                   (expect (not (some #{java-home} (vals (:project-paths snapshot))))))))

(defdescribe
  project-path-registry-test
  (it "names admitted project roots but not caches or non-admitted entries"
      (let [entries
            [{"id" "runtime" "path" "/projects/vis-python-runtime"}
             {"id" "ui" "path" "/projects/MyUI"} {"id" "numeric" "path" "/projects/123-app"}
             {"id" "engine" "path" "/projects/library" "python_name" "engine_path"}
             {"id" "cache" "path" "/projects/cache" "search" false}
             {"id" "hidden" "path" "/projects/hidden"}]

            snapshot
            (policy/snapshot {"workspace" {"filesystem" entries}
                              "jail" {"enabled" true
                                      "filesystem" {"allow" ["runtime" "ui" "numeric" "engine"
                                                             "cache"]}}})]

        (expect (= {"vis_python_runtime_path" "/projects/vis-python-runtime"
                    "my_ui_path" "/projects/MyUI"
                    "project_123_app_path" "/projects/123-app"
                    "engine_path" "/projects/library"}
                   (:project-paths snapshot)))))
  (it "keeps the declared catalog when the jail is off and permits explicit cache aliases"
      (let [snapshot
            (policy/snapshot
              {"workspace"
               {"filesystem"
                [{"id" "reference" "path" "/projects/reference" "access" "read-only"}
                 {"id" "cache" "path" "/projects/cache" "search" false "python_name" "cache_path"}]}
               "jail" {"enabled" false}})]
        (expect (= {"reference_path" "/projects/reference" "cache_path" "/projects/cache"}
                   (:project-paths snapshot)))))
  (it "names normalized absolute and home registrations after resolving their directories"
      (let [snapshot (policy/snapshot {"workspace" {"filesystem" [{"id" "here"
                                                                   "path" "/projects/current/."}
                                                                  {"id" "home" "path" "~"}]}}
                                      {:base-dir "/projects/current" :home "/people/developer"})]
        (expect (= {"current_path" "/projects/current" "developer_path" "/people/developer"}
                   (:project-paths snapshot)))))
  (it "refuses ambiguous or reserved names rather than silently overwriting one"
      (doseq [entries
              [[{"id" "first" "path" "/projects/first/api"}
                {"id" "second" "path" "/projects/second/api"}]
               [{"id" "reserved" "path" "/projects/library" "python_name" "project_root_path"}]]]
        (let [data (try (policy/snapshot {"workspace" {"filesystem" entries}})
                        nil
                        (catch clojure.lang.ExceptionInfo error (ex-data error)))]
          (expect (= :vis/invalid-config (:type data)))
          (expect (seq (:problems data))))))
  (it "includes registry changes in the immutable snapshot generation"
      (let [config
            {"workspace" {"filesystem" [{"id" "library" "path" "/projects/library"}]}}

            before
            (policy/snapshot config)

            after
            (policy/snapshot
              (assoc-in config ["workspace" "filesystem" 0 "python_name"] "engine_path"))]

        (expect (not= (:generation before) (:generation after))))))

(defdescribe
  security-policy-snapshot-test
  (it
    "resolves configured paths once, hashes the policy, and renders HOME-relative access"
    (let [home
          (.getCanonicalFile (.toFile (Files/createTempDirectory
                                        "vis-policy-home"
                                        (make-array java.nio.file.attribute.FileAttribute 0))))

          project
          (doto (java.io.File. home "vis") .mkdirs)

          sibling
          (doto (java.io.File. home "demo") .mkdirs)

          cache
          (doto (java.io.File. home ".m2") .mkdirs)

          cfg
          {"workspace"
           {"filesystem"
            [{"id" "demo" "path" "~/demo" "description" "Sibling repo" "draft" "copy-and-apply"}
             {"id" "ro" "path" "~/read-only" "access" "read-only" "draft" "not-allowed"}
             {"id" "m2" "path" "~/.m2" "search" false "description" "Maven cache"}]}
           "jail" {"enabled" true
                   "filesystem" {"allow" ["demo" "ro" "m2"]}
                   "network" {"allowed_domains" ["example.com"] "inbound_ports" [5273]}}}

          snapshot
          (with-redefs-fn {#'policy/java-read-roots (constantly [])}
            #(policy/snapshot cfg {:base-dir (.getPath project) :home (.getPath home)}))

          view
          (policy/access-view snapshot [(.getPath project)])]

      ;; The implicit session folder (`~/.vis`) is always granted, engine-level.
      (expect (= [(.getCanonicalPath sibling) (.getCanonicalPath cache)
                  (.getPath (java.io.File. home ".vis"))]
                 (policy/read-write-roots snapshot)))
      (expect (= "~/vis" (policy/home-relative (.getPath project) (.getPath home))))
      (expect (= "~/vis/AGENTS.md"
                 (paths/abbreviate-home (.getPath (java.io.File. project "AGENTS.md"))
                                        (.getPath home))))
      (expect (= "~/" (paths/abbreviate-home (.getPath home) (.getPath home))))
      (expect (= "relative/AGENTS.md" (paths/abbreviate-home "relative/AGENTS.md" (.getPath home))))
      (expect (= (str (.getPath home) "-other/AGENTS.md")
                 (paths/abbreviate-home (str (.getPath home) "-other/AGENTS.md") (.getPath home))))
      (expect (= ["~/vis" "~/demo" "~/.m2" "~/.vis"] (get-in view ["filesystem" "read_write"])))
      (expect (= ["~/read-only"] (get-in view ["filesystem" "process_read_only"])))
      (expect (= ["~/.m2" "~/.vis"] (get-in view ["filesystem" "no_search"])))
      (expect (= {"~/demo" "Sibling repo"
                  "~/.m2" "Maven cache"
                  "~/.vis" (get config-validation/vis-home-entry "description")}
                 (get-in view ["filesystem" "descriptions"])))
      ;; Only roots that opt OUT of the default `shared` isolation are named.
      (expect (= {"~/demo" "copy-and-apply" "~/read-only" "not-allowed"}
                 (get-in view ["filesystem" "draft"])))
      (expect (= {(.getCanonicalPath sibling) :copy-and-apply
                  (.getCanonicalPath (java.io.File. home "read-only")) :not-allowed}
                 (policy/draft-policies snapshot)))
      (expect (= [5273] (get-in view ["network" "inbound_ports"])))
      (expect (= "reload" (get view "changes_require")))
      ;; Confinement is named `jail` end to end: the access view says `is_jailed`,
      ;; never `sandboxed` (which read like the Python sandbox).
      (expect (true? (get view "is_jailed")))
      (expect (not (contains? view "sandboxed")))
      (expect (re-matches #"sha256:[0-9a-f]{64}" (get view "generation")))))
  (it
    "grants unrestricted explicit filesystem access when the jail is disabled"
    (let [home
          (.getCanonicalFile (.toFile (Files/createTempDirectory
                                        "vis-policy-open"
                                        (make-array java.nio.file.attribute.FileAttribute 0))))

          project
          (doto (java.io.File. home "vis") .mkdirs)

          base
          (.getPath project)

          snapshot
          (policy/snapshot {"jail" {"enabled" false}} {:base-dir base :home (.getPath home)})

          host-roots
          (->> (java.io.File/listRoots)
               (mapv #(.getCanonicalPath ^java.io.File %)))

          view
          (policy/access-view snapshot [base])]

      (expect (false? (:jail-enabled snapshot)))
      (expect (false? (get view "is_jailed")))
      (expect (= host-roots (policy/read-write-roots snapshot)))
      (expect (= host-roots (policy/no-search-roots snapshot)))
      (expect (= (vec (distinct (concat ["~/vis"] host-roots)))
                 (get-in view ["filesystem" "read_write"])))
      (expect (= host-roots (get-in view ["filesystem" "no_search"])))
      (expect (= (mapv (fn [root]
                         {:trunk root :clone root :draft :shared :no-search? true})
                       host-roots)
                 (workspace/env-filesystem-roots {:security-policy snapshot
                                                  :security/filesystem-roots []
                                                  :security/no-search-roots []})))))
  (it
    "keeps a stable generation for equivalent snapshots and changes it with policy"
    (let [base
          {"jail"
           {"enabled" true "filesystem" {"allow" []} "network" {"allowed_domains" ["example.com"]}}}

          a
          (policy/snapshot base)

          b
          (policy/snapshot base)

          c
          (policy/snapshot (assoc-in base ["jail" "network" "allowed_domains"] ["other.example"]))]

      (expect (= (:generation a) (:generation b)))
      (expect (not= (:generation a) (:generation c)))))
  (it "surfaces config_error in the access view only when the policy carries one"
      (let [snap
            (policy/snapshot {"jail" {"network" {"allowed_domains" ["example.com"]}}})

            clean
            (policy/access-view snap [])

            erred
            (policy/access-view (assoc snap
                                  :config-error
                                  {"source" "vis.yml"
                                   "problems"
                                   ["toggles: unknown top-level config key (config is closed)"]
                                   "hint" "fix it and /reload"})
                                [])]

        (expect (not (contains? clean "config_error")))
        (expect (= "vis.yml" (get-in erred ["config_error" "source"])))
        (expect (= ["toggles: unknown top-level config key (config is closed)"]
                   (get-in erred ["config_error" "problems"]))))))
