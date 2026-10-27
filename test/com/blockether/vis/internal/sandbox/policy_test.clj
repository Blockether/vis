(ns com.blockether.vis.internal.sandbox.policy-test
  (:require [com.blockether.vis.internal.config.validation :as config-validation]
            [com.blockether.vis.internal.foundation.workspace-ctx :as wctx]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file FileVisitOption Files Path]))

(defn- one-list
  "The session's one path list, `session[\"workspace\"][\"filesystem_roots\"]`, for `root`."
  [snapshot root]
  (get (wctx/render-block {:workspace {:root root}
                           :filesystem-roots
                           (workspace/env-filesystem-roots
                             {:security-policy snapshot
                              :security/filesystem-roots (policy/read-write-roots snapshot)
                              :security/no-search-roots (policy/no-search-roots snapshot)})
                           :policy snapshot})
       "filesystem_roots"))

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
  (with-redefs-fn {#'policy/java-read-roots (constantly roots)
                   #'policy/git-config-read-paths (constantly [])}
    #(policy/snapshot config)))

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

                       entry
                       (some #(when (= (paths/abbreviate-home java-home) (get % "cwd")) %)
                             (one-list snapshot (System/getProperty "user.dir")))]

                   (expect (some #{java-home} (get-in snapshot [:process-jail :allow-read])))
                   (expect (= "read" (get entry "mode")))
                   (expect (true? (get entry "no_search")))
                   (expect (some #{java-home} (policy/no-search-roots snapshot)))
                   (expect (string? (get entry "description")))
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
          (policy/access-view snapshot)

          in-home
          #(.getPath (java.io.File. home ^String %))]

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
      ;; User review: the session object listed each path up to three times. Now each path
      ;; is in one list once, with only the facts that differ from the default. A root that
      ;; opts OUT of the default `shared` isolation names its draft policy.
      (expect (= [{"cwd" (in-home "demo") "description" "Sibling repo" "draft" "copy-and-apply"}
                  {"cwd" (in-home ".m2") "description" "Maven cache" "no_search" true}
                  {"cwd" (in-home ".vis")
                   "description" (get config-validation/vis-home-entry "description")
                   "no_search" true}
                  {"cwd" (in-home "read-only") "mode" "read" "draft" "not-allowed"}]
                 (one-list snapshot (.getPath project))))
      ;; The access block keeps only the rules.
      (expect (= {"deny_read" [] "deny_write" []} (get view "filesystem")))
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
  (it "leaves the jail disabled when configuration omits `jail.enabled`"
      ;; Regression: the shipped default omits the key, so the access view must not
      ;; report confinement that configuration never asked the process jail to install.
      (let [home
            (.getCanonicalFile (.toFile (Files/createTempDirectory
                                          "vis-policy-default"
                                          (make-array java.nio.file.attribute.FileAttribute 0))))

            base
            (.getPath (doto (java.io.File. home "vis") .mkdirs))

            snapshot
            (policy/snapshot {} {:base-dir base :home (.getPath home)})

            view
            (policy/access-view snapshot)]

        (expect (true? (get-in snapshot [:process-jail :disabled?])))
        (expect (false? (:jail-enabled snapshot)))
        (expect (false? (get view "is_jailed")))))
  (it "grants unrestricted explicit filesystem access when the jail is disabled"
      ;; Regression: a disabled jail replaced the configured catalog with the host
      ;; roots, so a named project root lost its identity, guidance and search scope.
      (let [home
            (.getCanonicalFile (.toFile (Files/createTempDirectory
                                          "vis-policy-open"
                                          (make-array java.nio.file.attribute.FileAttribute 0))))

            project
            (doto (java.io.File. home "vis") .mkdirs)

            sibling
            (doto (java.io.File. home "sibling") .mkdirs)

            base
            (.getPath project)

            snapshot
            (policy/snapshot {"jail" {"enabled" false}
                              "workspace" {"filesystem" [{"id" "sibling"
                                                          "path" (.getPath sibling)
                                                          "access" "read-write"}]}}
                             {:base-dir base :home (.getPath home)})

            host-roots
            (->> (java.io.File/listRoots)
                 (mapv #(.getCanonicalPath ^java.io.File %)))

            vis-home
            (.getPath (java.io.File. home ".vis"))

            catalog
            [(.getPath sibling) vis-home]

            view
            (policy/access-view snapshot)]

        (expect (false? (:jail-enabled snapshot)))
        (expect (false? (get view "is_jailed")))
        (expect (= (vec (distinct (concat catalog host-roots))) (policy/read-write-roots snapshot)))
        (expect (= (vec (distinct (concat [vis-home] host-roots)))
                   (policy/no-search-roots snapshot)))
        (expect (= (vec (distinct (concat catalog host-roots)))
                   (mapv #(get % "cwd") (one-list snapshot base))))
        (expect (= (vec (distinct (concat [vis-home] host-roots)))
                   (keep #(when (get % "no_search") (get % "cwd")) (one-list snapshot base))))
        (expect (= (into (mapv (fn [root]
                                 (cond-> {:trunk root :clone root :draft :shared}
                                   (= vis-home root)
                                   (assoc :no-search? true)))
                               catalog)
                         (mapv (fn [root]
                                 {:trunk root :clone root :draft :shared :no-search? true})
                               host-roots))
                   (workspace/env-filesystem-roots
                     {:security-policy snapshot
                      :security/filesystem-roots (policy/read-write-roots snapshot)
                      :security/no-search-roots (policy/no-search-roots snapshot)})))))
  (it "keeps `search: false` catalog roots out of the default search when the jail is disabled"
      ;; Regression: a disabled jail replaced the catalog `search: false` roots with the host
      ;; roots, so an unscoped grep crawled caches such as `~/.npm`.
      (let [home
            (.getCanonicalFile (.toFile (Files/createTempDirectory
                                          "vis-policy-no-search"
                                          (make-array java.nio.file.attribute.FileAttribute 0))))

            base
            (.getPath (doto (java.io.File. home "vis") .mkdirs))

            cache
            (.getPath (doto (java.io.File. home "cache") .mkdirs))

            vis-home
            (.getPath (java.io.File. home ".vis"))

            snapshot
            (policy/snapshot {"jail" {"enabled" false}
                              "workspace" {"filesystem"
                                           [{"id" "cache" "path" cache "search" false}]}}
                             {:base-dir base :home (.getPath home)})

            host-roots
            (->> (java.io.File/listRoots)
                 (mapv #(.getCanonicalPath ^java.io.File %)))

            entries
            (workspace/env-filesystem-roots
              {:security-policy snapshot
               :security/filesystem-roots (policy/read-write-roots snapshot)
               :security/no-search-roots (policy/no-search-roots snapshot)})]

        (expect (= (vec (distinct (concat [cache vis-home] host-roots)))
                   (policy/no-search-roots snapshot)))
        (expect (= (vec (distinct (concat [cache vis-home] host-roots)))
                   (keep #(when (get % "no_search") (get % "cwd")) (one-list snapshot base))))
        (expect (true? (:no-search? (first (filter #(= cache (:trunk %)) entries)))))))
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
            (policy/access-view snap)

            erred
            (policy/access-view (assoc snap
                                  :config-error
                                  {"source" "vis.yml"
                                   "problems"
                                   ["toggles: unknown top-level config key (config is closed)"]
                                   "hint" "fix it and /reload"}))]

        (expect (not (contains? clean "config_error")))
        (expect (= "vis.yml" (get-in erred ["config_error" "source"])))
        (expect (= ["toggles: unknown top-level config key (config is closed)"]
                   (get-in erred ["config_error" "problems"]))))))

(defn- with-secret-project
  "A temp project holding a root `.env`, a nested `.env` and one ordinary file."
  [f]
  (let [root
        (.toRealPath (Files/createTempDirectory "vis-deny-policy"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))
                     (make-array java.nio.file.LinkOption 0))

        directory
        (.toFile root)]

    (try (spit (File. directory ".env") "TOKEN=secret")
         (.mkdirs (File. directory "service"))
         (spit (File. directory "service/.env") "TOKEN=secret")
         (spit (File. directory "README.md") "hello")
         (f (.toString root))
         (finally (doseq [^File file (reverse (file-seq directory))]
                    (.delete file))))))

(defdescribe
  filesystem-deny-rule-test
  (it "resolves deny rules against the workspace root and expands globs for the jail (#263)"
      (with-secret-project
        (fn [root]
          (let [snapshot
                (policy/snapshot {"workspace" {"filesystem" [{"id" "project" "path" root}]}
                                  "jail" {"enabled" true
                                          "filesystem" {"allow" ["project"]
                                                        "deny_read" [".env" "**/.env"]
                                                        "deny_write" ["service"]}}}
                                 {:base-dir root})

                jail
                (:process-jail snapshot)]

            ;; The rules stay patterns: the host tools match them live, so a file
            ;; created after startup is still refused by `cat`.
            (expect (= [(str root "/.env") (str root "/**/.env")] (:deny-read-rules jail)))
            (expect (= [(str root "/service")] (:deny-write-rules jail)))
            ;; The snapshot keeps the literal rules only, so it never walks the tree (#396).
            (expect (= [(str root "/.env")] (:deny-read jail)))
            (expect (= [(str root "/service")] (:deny-write jail)))
            ;; A jail that binds paths can only name concrete paths, so the spawn expands the glob.
            (expect (some #{(str root "/service/.env")}
                          (with-redefs [policy/os-binds-deny-paths? (constantly true)]
                            (:deny-read (process-jail/runtime-policy jail)))))
            ;; Diagnostics show the rules an operator wrote, never file contents.
            (expect (= [(str root "/.env") (str root "/**/.env")]
                       (get-in (policy/access-view snapshot) ["filesystem" "deny_read"])))
            (expect (= [(str root "/service")]
                       (get-in (policy/access-view snapshot) ["filesystem" "deny_write"]))))))))

(defn- deny-snapshot
  "A snapshot of a project with glob deny rules, with the jail on or off."
  [root enabled?]
  (policy/snapshot {"workspace" {"filesystem" [{"id" "project" "path" root}]}
                    "jail" {"enabled" enabled?
                            "filesystem" {"allow" ["project"]
                                          "deny_read" [".env" "**/.env" "s*"]
                                          "deny_write" ["*.md"]}}}
                   {:base-dir root}))

(defdescribe
  deny-rule-expansion-cost-test
  ;; Issue #396: each snapshot walked the whole workspace once for each glob deny rule, so a
  ;; new session or a /reload took 10-20 s in a large workspace, with the jail on or off.
  (it "walks nothing when the jail is off or when the OS jail compiles the patterns"
      (with-secret-project
        (fn [root]
          (let [walks (atom 0)]
            (with-redefs [policy/os-binds-deny-paths? (constantly false)
                          policy/walk-entries (fn [& _]
                                                (swap! walks inc))]

              (doseq [enabled? [false true]]
                (let [snapshot (deny-snapshot root enabled?)
                      jail (:process-jail snapshot)
                      env {:security-policy snapshot}]

                  (expect (= [(str root "/.env")] (:deny-read jail)))
                  (expect (= [] (:deny-write jail)))
                  (expect (= [(str root "/.env") (str root "/**/.env") (str root "/s*")]
                             (:deny-read-rules jail)))
                  ;; Seatbelt gets each pattern and also denies files created later.
                  (expect (some #{(str root "/**/.env")}
                                (:deny-read (process-jail/runtime-policy jail))))
                  ;; The host tools still match the rules live.
                  (expect (some?
                            (process-jail/deny-refusal env "file-read" (str root "/service/.env"))))
                  (expect
                    (some? (process-jail/deny-refusal env "file-write" (str root "/README.md")))))))
            (expect (= 0 @walks))))))
  (it
    "starts one shared background walk for every deny list when the OS jail binds paths"
    (with-secret-project
      (fn [root]
        (let [walks
              (atom 0)

              release
              (promise)

              finished
              (atom false)

              walk
              @#'policy/walk-entries]

          (with-redefs [policy/os-binds-deny-paths?
                        (constantly true)

                        policy/walk-entries
                        (fn [& args]
                          (swap! walks inc)
                          (deref release 5000 nil)
                          (apply walk args)
                          (reset! finished true))]

            ;; The snapshot only starts the walk, so a new session does not wait for it.
            (let [jail (:process-jail (deny-snapshot root true))]
              (expect (false? @finished))
              (expect (= [(str root "/.env")] (:deny-read jail)))
              (deliver release true)
              (let [sent (process-jail/runtime-policy jail)]
                ;; A glob without a separator matches a directory too.
                (expect (every? (set (:deny-read sent))
                                [(str root "/.env") (str root "/service/.env")
                                 (str root "/service")]))
                (expect (some #{(str root "/README.md")} (:deny-write sent))))
              ;; A later spawn reuses the walk.
              (process-jail/runtime-policy jail)
              (expect (= 1 @walks)))))))))

(defdescribe
  deny-overlap-test
  ;; Issue #396: the draft root check expanded each glob over the whole workspace.
  (it "tests ancestors without a walk and walks only below the selected root"
      (with-secret-project
        (fn [root]
          (let [starts
                (atom [])

                walk
                @#'policy/walk-entries

                nested
                (str root "/**/.env")]

            (with-redefs [policy/walk-entries (fn [start & args]
                                                (swap! starts conj (str start))
                                                (apply walk start args))]
              (expect (= (str root "/secret.txt")
                         (policy/deny-overlap [nested (str root "/secret.txt")] root)))
              (expect (= (str root "/service")
                         (policy/deny-overlap [(str root "/serv*")] (str root "/service"))))
              (expect (nil? (policy/deny-overlap [(str root "/*.md")] (str root "/service"))))
              (expect (nil? (policy/deny-overlap [nested] (str root "-elsewhere"))))
              (expect (= [] @starts))
              (expect (= (str root "/service/.env")
                         (policy/deny-overlap [nested] (str root "/service"))))
              (expect (= (str root "/service/.env") (policy/deny-overlap [nested] root)))
              (expect (= [(str root "/service") root] @starts))))))))

(defn- with-link-tree
  "A temp tree with `.env` files beside, before and below other entries."
  [f]
  (let [root
        (.toRealPath (Files/createTempDirectory "vis-link-walk"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))
                     (make-array java.nio.file.LinkOption 0))

        directory
        (.toFile root)]

    (try (doseq [path ["a/deep/er" "b" "c"]]
           (.mkdirs (File. directory ^String path)))
         (doseq [path ["f1" "f2" "f3" ".env" "a/f1" "a/f2" "a/deep/er/.env" "b/f1" "b/.env"]]
           (spit (File. directory ^String path) "x"))
         (f (.toString root))
         (finally (doseq [^File file (reverse (file-seq directory))]
                    (.delete file))))))

(defdescribe
  link-count-walk-test
  ;; Issue #396: a cold Linux walk spent half its time on a status call for each file.
  (let [expected
        (fn [root]
          ;; A Java glob `**/` needs one directory, so the root `.env` is not a match.
          (mapv #(str root %) ["/a/deep/er/.env" "/b/.env"]))

        matches
        (fn [root]
          (get (#'policy/glob-matches root [(str root "/**/.env")]) (str root "/**/.env")))]

    (it "finds every match when a reliable link count skips the files after the last subdirectory"
        (with-link-tree
          (fn [root]
            (let [calls
                  (atom 0)

                  ;; A file system with exact link counts, as on ext4, XFS and tmpfs.
                  exact
                  (fn [^Path path _]
                    (swap! calls inc)
                    (when (Files/isDirectory path (make-array java.nio.file.LinkOption 0))
                      (with-open [entries (Files/newDirectoryStream path)]
                        (count (filter #(Files/isDirectory %
                                                           (make-array java.nio.file.LinkOption 0))
                                       entries)))))]

              (with-redefs [policy/subdirectory-count exact]
                (expect (= (expected root) (matches root))))
              ;; 14 entries and the start, but no call for a file after the last subdirectory
              ;; or in a directory without subdirectories.
              (expect (<= @calls 12))))))
    (it "finds every match with the link counts of the real file system"
        (with-link-tree (fn [root]
                          (with-redefs [policy/link-count-reliable? (constantly true)]
                            (expect (= (expected root) (matches root)))))))))

(defdescribe
  subagent-file-grant-narrows-a-snapshot
  (it
    "a subagent file grant narrows a snapshot, also for files created later"
    (let [root
          (.getCanonicalPath (doto (File. (System/getProperty "java.io.tmpdir")
                                          (str "visgrant-" (System/nanoTime)))
                               (.mkdirs)))

          config
          {"workspace" {"filesystem" [{"id" "project" "path" root}]} "jail" {"enabled" true}}

          base
          (policy/snapshot config {:base-dir root})

          narrowed
          (policy/restrict
            base
            {:write ["src/**" "/elsewhere/**"] :read_only ["src/vendor"] :deny [".env"]})

          jail
          (:process-jail narrowed)]

      (expect (= base (policy/restrict base nil)))
      (expect (not= (:generation base) (:generation narrowed)))
      ;; Rules resolve against the root and keep a missing tail. The parent's policy stays
      ;; the ceiling: a rule outside its writable roots grants nothing.
      (expect (= ["/elsewhere/**"] (policy/outside-write-rules base ["/elsewhere/**"] [root])))
      (expect (= [(str root "/src/**")] (:write-rules jail)))
      (expect (true? (:write-limited? jail)))
      (expect (some #{(str root "/.env")} (:deny-read-rules jail)))
      (expect (some #{(str root "/src/vendor")} (:deny-write-rules jail)))
      ;; An enabled jail grants only rule roots inside its writable ceiling.
      (expect (= [(str root "/src")] (:allow-read-write jail)))
      ;; Other writable roots stay readable only.
      (expect (every? (set (:allow-read jail)) (get-in base [:process-jail :allow-read-write])))
      (expect (= [(str root "/src/**")]
                 (get-in (policy/access-view narrowed) ["filesystem" "write_rules"])))
      ;; A grant whose only rule is outside the ceiling writes nothing at all.
      (let [none (:process-jail (policy/restrict base {:write ["/elsewhere/**"]}))]
        (expect (true? (:write-limited? none)))
        (expect (= [] (:write-rules none) (:allow-read-write none))))
      ;; The one path list marks each root that the grant left readable only.
      (let [modes
            (into {} (map (juxt #(get % "cwd") #(get % "mode"))) (one-list narrowed "/elsewhere"))

            former
            (get-in base [:process-jail :allow-read-write])]

        (expect (seq former))
        (expect (every? #(= "read" (get modes (paths/abbreviate-home %))) former)))))
  (it
    "never gives a child more than its parent: the parent's deny rules survive a wide grant"
    (let [root
          (.getCanonicalPath (doto (File. (System/getProperty "java.io.tmpdir")
                                          (str "visgrant-" (System/nanoTime)))
                               (.mkdirs)))

          config
          (fn [jail?]
            {"workspace" {"filesystem" [{"id" "project" "path" root}]}
             "jail" {"enabled" jail?
                     "filesystem"
                     {"allow" ["project"] "deny_read" [".env"] "deny_write" ["src/secret"]}}})]

      (doseq [jail? [true false]]
        (let [narrowed (policy/restrict (policy/snapshot (config jail?) {:base-dir root})
                                        {:write ["**"]})
              jail (:process-jail narrowed)
              env {:security-policy narrowed}]

          (expect (= [(str root "/**")] (:write-rules jail)))
          (expect (some #{(str root "/src/secret")} (:deny-write-rules jail)))
          (expect (some #{(str root "/.env")} (:deny-read-rules jail)))
          ;; The host file tools ask the same rules, so a grant never opens a closed path.
          (expect (some? (process-jail/deny-refusal env "file-write" (str root "/src/secret/key"))))
          (expect (some? (process-jail/deny-refusal env "file-read" (str root "/.env"))))
          (expect (nil? (process-jail/deny-refusal env "file-write" (str root "/src/ok.txt"))))))))
  ;; Council complaint #7930: the docs state this order, so this test pins it.
  (it "deny wins over read_only, read_only wins over write, and empty lists add no rules"
      (let [root
            (.getCanonicalPath (doto (File. (System/getProperty "java.io.tmpdir")
                                            (str "visgrant-" (System/nanoTime)))
                                 (.mkdirs)))

            base
            (policy/snapshot {"workspace" {"filesystem" [{"id" "project" "path" root}]}
                              "jail" {"enabled" true}}
                             {:base-dir root})

            env
            {:security-policy (policy/restrict
                                base
                                {:write ["src/**"] :read_only ["src/vendor"] :deny ["src/secret"]})}

            refused?
            (fn [op path]
              (some? (process-jail/deny-refusal env op (str root path))))]

        (expect (= base (policy/restrict base {:write [] :read_only [] :deny []})))
        (expect (not (refused? "file-write" "/src/ok.txt")))
        (expect (refused? "file-write" "/src/vendor/lib.clj") "read_only wins over write")
        (expect (not (refused? "file-read" "/src/vendor/lib.clj")))
        (expect (refused? "file-write" "/src/secret/key") "deny wins over write")
        (expect (refused? "file-read" "/src/secret/key") "deny closes reads too"))))

;; Regression, issue #394: an extension grant is a toolchain path, never a whole tree
;; that holds the user's files or the Vis credentials.
(defn- grant-home
  []
  (str (Files/createTempDirectory "vis-grant-home"
                                  (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- grant-in
  [home g]
  (policy/extension-grant "tools" g {:home home :vis-home (str home "/.vis")}))

(defn- grant-refusal
  [home g]
  (try (grant-in home g) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defdescribe
  extension-grant-test
  (it "keeps the declared path, expands ~ and maps the mode"
      (let [home (grant-home)]
        (expect (= {:path (str home "/.m2/repository") :mode :read-write :reason "Maven cache."}
                   (grant-in
                     home
                     {"path" "~/.m2/repository" "mode" "read_write" "reason" " Maven cache. "})))
        (expect (= {:path (str home "/.vis/install") :mode :read :reason "Vis launcher."}
                   (grant-in home
                             {"path" "~/.vis/install/" "mode" "read" "reason" "Vis launcher."})))))
  (it "refuses the root, the home, the Vis home and their ancestors"
      (let [home (grant-home)]
        (doseq [path ["/" "~" "~/.vis" (str home "/..")]]
          (expect (some-> (grant-refusal home {"path" path "mode" "read" "reason" "Wide."})
                          (.contains "covers the filesystem root"))
                  path))))
  (it "refuses a relative path, an unknown mode and a missing reason"
      (let [home (grant-home)]
        (expect (some-> (grant-refusal home {"path" "cache" "mode" "read" "reason" "Cache."})
                        (.contains "must be absolute")))
        (expect (some-> (grant-refusal home {"path" "/opt/jdk" "mode" "write" "reason" "JDK."})
                        (.contains "needs mode")))
        (expect (some-> (grant-refusal home {"path" "/opt/jdk" "mode" "read" "reason" " "})
                        (.contains "needs a reason"))))))

(defdescribe
  git-config-grant-test
  ;; Git stops with "unable to access ~/.gitconfig" when the jail denies its global
  ;; configuration, so every git command failed in a jailed session.
  (it
    "grants the global configuration, its includes and XDG files, never credentials"
    (let [home
          (.toRealPath (Files/createTempDirectory "vis-git-policy"
                                                  (make-array java.nio.file.attribute.FileAttribute
                                                              0))
                       (make-array java.nio.file.LinkOption 0))

          file
          (fn [relative text]
            (let [path (.resolve ^Path home ^String relative)]
              (Files/createDirectories (.getParent path)
                                       (make-array java.nio.file.attribute.FileAttribute 0))
              (spit (.toFile path) text)
              (str path)))

          dotfile
          (file "dotfiles/gitconfig"
                (str "[user]\n\tname = A\n[include]\n\tpath = ~/.gitconfig.local\n"
                     "[includeIf \"gitdir:~/work/\"]\n\tpath = work.inc\n"
                     "[core]\n\tpath = not-an-include\n"))

          local
          (file ".gitconfig.local" "[user]\n\temail = a@example.com\n")

          work
          (file "dotfiles/work.inc" "[user]\n\temail = b@example.com\n")

          ignore
          (file ".config/git/ignore" "*.log\n")

          _
          (file ".config/git/credentials" "secret\n")

          _
          (file ".git-credentials" "secret\n")

          link
          (.resolve ^Path home ".gitconfig")]

      (try (Files/createSymbolicLink link
                                     (.resolve ^Path home "dotfiles/gitconfig")
                                     (make-array java.nio.file.attribute.FileAttribute 0))
           (let [paths (#'policy/git-config-read-paths (str home) {})]
             ;; work.inc is relative, so it resolves next to the real file of the link.
             (expect (= #{(str link) dotfile local work ignore} (set paths)))
             (expect (not-any? #(re-find #"credentials" %) paths)))
           ;; GIT_CONFIG_GLOBAL replaces only the config file; git still reads the XDG ignore.
           (expect (= #{local ignore}
                      (set
                        (#'policy/git-config-read-paths (str home) {"GIT_CONFIG_GLOBAL" local}))))
           (finally (with-open [paths (Files/walk home (make-array FileVisitOption 0))]
                      (doseq [^Path path (reverse (iterator-seq (.iterator paths)))]
                        (Files/deleteIfExists path)))))))
  (it "grants nothing without a home or configuration"
      (expect (= nil (#'policy/git-config-read-paths nil {})))
      (let [home (Files/createTempDirectory "vis-git-empty"
                                            (make-array java.nio.file.attribute.FileAttribute 0))]
        (try (expect (= [] (#'policy/git-config-read-paths (str home) {})))
             (finally (Files/deleteIfExists home))))))
