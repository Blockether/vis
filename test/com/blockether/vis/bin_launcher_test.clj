(ns com.blockether.vis.bin-launcher-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(defn- write-executable! [file body] (spit file body) (.setExecutable ^java.io.File file true) file)

(defn- link-command!
  [dir name]
  (let
    [target (first (filter #(.isFile ^java.io.File %) (map #(io/file % name) ["/usr/bin" "/bin"])))]
    (when target
      (Files/createSymbolicLink (.toPath (io/file dir name))
                                (.toPath target)
                                (make-array FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))

(defn- launcher-fixture
  [vendor]
  (let
    [root
     (.toFile (Files/createTempDirectory "vis-launcher-test-" (make-array FileAttribute 0)))

     repo
     (doto (io/file root "repo") .mkdirs)

     bin
     (doto (io/file repo "bin") .mkdirs)

     old-home
     (doto (io/file root "old-jdk") .mkdirs)

     old-bin
     (doto (io/file old-home "bin") .mkdirs)

     pinned-home
     (doto (io/file root "pinned-jdk") .mkdirs)

     pinned-bin
     (doto (io/file pinned-home "bin") .mkdirs)

     tools
     (doto (io/file root "tools") .mkdirs)

     home
     (doto (io/file root "home") .mkdirs)]

    (Files/copy (.toPath (io/file "bin/vis-agent"))
                (.toPath (io/file bin "vis-agent"))
                (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
    (.setExecutable (io/file bin "vis-agent") true)
    (spit (io/file repo "deps.edn") "{}")
    (spit (io/file repo ".graalvm-version")
          (str "GRAAL_EDITION=\"GraalVM CE\"\n"
               "GRAAL_VERSION=\"25.1.3\"\n"
               "GRAAL_VENDOR_VERSION=\"GraalVM CE 25.1.3+9.1\"\n"))
    (write-executable! (io/file bin "require-graalvm")
                       "#!/usr/bin/env bash\nprintf '%s\\n' \"$VIS_TEST_PINNED_HOME\"\n")
    (write-executable!
      (io/file old-bin "java")
      (str "#!/usr/bin/env bash\n" "echo '    java.vendor.version = " vendor "' >&2\n"))
    (write-executable! (io/file pinned-bin "java") "#!/usr/bin/env bash\nexit 0\n")
    (write-executable! (io/file tools "clojure")
                       (str "#!/usr/bin/env bash\n" "printf 'JAVA_HOME=%s\\n' \"$JAVA_HOME\"\n"
                            "printf 'GRAALVM_HOME=%s\\n' \"${GRAALVM_HOME:-}\"\n"
                            "printf 'JAVA_CMD=%s\\n' \"${JAVA_CMD:-}\"\n"))
    ;; Keep the no-Java launch isolated from the host JDK while retaining the
    ;; ordinary POSIX tools the wrapper needs during startup.
    (doseq
      [name ["awk" "basename" "bash" "cat" "chmod" "cp" "cut" "date" "dirname" "env" "find" "grep"
             "head" "ls" "mkdir" "mktemp" "pwd" "readlink" "rm" "sed" "sort" "tail" "tr" "uname"]]
      (link-command! tools name))
    {:root root :repo repo :home home :old-home old-home :pinned-home pinned-home :tools tools}))

(defn- run-launcher
  ([fixture] (run-launcher fixture {}))
  ([{:keys [repo home old-home pinned-home tools]} {:keys [java?] :or {java? true}}]
   (let
     [old-java
      (.getAbsolutePath (io/file old-home "bin/java"))

      pb
      (java.lang.ProcessBuilder. ^java.util.List
                                 (vec ["bash" (.getAbsolutePath (io/file repo "bin/vis-agent")) "--jvm"
                                       "--version"]))

      env
      (.environment pb)]

     (.directory pb repo)
     (.redirectErrorStream pb true)
     (.put env "HOME" (.getAbsolutePath ^java.io.File home))
     (.put env
           "PATH"
           (str (when java? (str (.getAbsolutePath ^java.io.File (io/file old-home "bin")) ":"))
                (.getAbsolutePath ^java.io.File tools)
                (if java? ":/usr/bin:/bin" ":/bin")))
     (if java?
       (do (.put env "JAVA_HOME" (.getAbsolutePath ^java.io.File old-home))
           (.put env "JAVA_CMD" old-java))
       (do (.remove env "JAVA_HOME") (.remove env "JAVA_CMD")))
     (.put env "VIS_TEST_PINNED_HOME" (.getAbsolutePath ^java.io.File pinned-home))
     (.put env "VIS_NO_DEV_CHECKOUT" "1")
     (let
       [process
        (.start pb)

        output
        (slurp (.getInputStream process))

        exit
        (.waitFor process)]

       {:exit exit :output output}))))


(defdescribe
  jvm-launcher-runtime-test
  (it "replaces a stale mismatched GraalVM inherited from the parent runner"
      (let [{:keys [root pinned-home] :as fixture} (launcher-fixture "GraalVM CE 25.2.4+7.1")]
        (try (let
               [{:keys [exit output]} (run-launcher fixture)
                pinned-java (.getAbsolutePath (io/file pinned-home "bin/java"))]

               (expect (= 0 exit))
               (expect (str/includes? output "replaced incompatible GraalVM CE 25.2.4+7.1"))
               (expect (str/includes? output (str "JAVA_HOME=" (.getAbsolutePath pinned-home))))
               (expect (str/includes? output (str "GRAALVM_HOME=" (.getAbsolutePath pinned-home))))
               (expect (str/includes? output (str "JAVA_CMD=" pinned-java))))
             (finally (delete-tree! root)))))
  ;; A missing Java must be provisioned before the JVM launch.
  (it "automatically provisions the pinned JDK when no Java is available"
      (let [{:keys [root pinned-home] :as fixture} (launcher-fixture "GraalVM CE 25.1.3+9.1")]
        (try (let [{:keys [exit output]} (run-launcher fixture {:java? false})]
               (expect (= 0 exit))
               (expect (str/includes? output (str "JAVA_HOME=" (.getAbsolutePath pinned-home))))
               (expect (str/includes?
                         output
                         (str "JAVA_CMD=" (.getAbsolutePath pinned-home) "/bin/java"))))
             (finally (delete-tree! root)))))
  (it "preserves a stock JDK because it has no built-in Truffle to collide"
      (let [{:keys [root old-home] :as fixture} (launcher-fixture "Eclipse Adoptium-25.0.3+8")]
        (try (let [{:keys [exit output]} (run-launcher fixture)]
               (expect (= 0 exit))
               (expect (not (str/includes? output "replaced incompatible")))
               (expect (str/includes? output (str "JAVA_HOME=" (.getAbsolutePath old-home))))
               (expect (str/includes? output
                                      (str "JAVA_CMD="
                                           (.getAbsolutePath (io/file old-home "bin/java"))))))
             (finally (delete-tree! root))))))

(defdescribe
  wrapper-runtime-selection-test
  (it
    "persists the runtime choice and honours one-launch overrides"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-runtime-test-" (make-array FileAttribute 0)))

       install-dir
       (doto (io/file root "install") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       managed-src
       (doto (io/file vis-home "install/src") .mkdirs)

       tools
       (doto (io/file root "tools") .mkdirs)

       launcher
       (io/file install-dir "vis-agent")

       run!
       (fn [args]
         (let
           [pb
            (ProcessBuilder. ^java.util.List (into ["bash" (.getAbsolutePath launcher)] args))

            env
            (.environment pb)]

           (.directory pb root)
           (.redirectErrorStream pb true)
           (.put env "HOME" (.getAbsolutePath home))
           (.put env "VIS_HOME" (.getAbsolutePath vis-home))
           (.put env "VIS_NO_DEV_CHECKOUT" "1")
           (.put env "PATH" (str (.getAbsolutePath tools) ":/usr/bin:/bin"))
           (let
             [process
              (.start pb)

              output
              (slurp (.getInputStream process))]

             {:exit (.waitFor process) :output output})))]

      (try (Files/copy (.toPath (io/file "bin/vis-agent"))
                       (.toPath launcher)
                       (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
           (.setExecutable launcher true)
           (spit (io/file managed-src "deps.edn") "{}")
           (write-executable! (io/file install-dir "vis-agent-native")
                              "#!/usr/bin/env bash\nprintf 'NATIVE:%s\\n' \"$*\"\n")
           (write-executable! (io/file tools "java") "#!/usr/bin/env bash\nexit 0\n")
           (write-executable! (io/file tools "clojure")
                              "#!/usr/bin/env bash\nprintf 'JVM:%s\\n' \"$*\"\n")
           (let [{:keys [exit output]} (run! ["runtime" "show"])]
             (expect (= 0 exit))
             (expect (str/includes? output "Runtime:      native (automatic)")))
           (expect (= 0 (:exit (run! ["runtime" "use" "jvm"]))))
           (let [{:keys [exit output]} (run! ["--version"])]
             (expect (= 0 exit))
             (expect (str/includes? output "JVM:")))
           (let [{:keys [exit output]} (run! ["--native" "--version"])]
             (expect (= 0 exit))
             (expect (str/includes? output "NATIVE:"))
             (expect (str/includes? output "--version")))
           ;; The flag names the runtime for one launch; the persisted choice stays.
           (let [{:keys [output]} (run! ["runtime" "show"])]
             (expect (str/includes? output "Runtime:      jvm"))
             (expect (str/includes? output (.getAbsolutePath managed-src))))
           (finally (delete-tree! root))))))

(defn- release-json
  "Minimal GitHub release payload carrying exactly `asset-names`."
  [& asset-names]
  (str "{\"tag_name\":\"v9.9.9\",\"assets\":["
       (str/join ","
                 (map #(str "{\"name\":\""
                            %
                            "\",\"browser_download_url\":\"https://example.invalid/download/"
                            %
                            "\"}")
                      asset-names))
       "]}"))

(defn- tar-gz!
  [dir out]
  (let
    [pb (java.lang.ProcessBuilder. ^java.util.List
                                  (vec ["tar" "-C" (.getAbsolutePath ^java.io.File dir) "-czf"
                                        (.getAbsolutePath ^java.io.File out) "."]))]
    (.redirectErrorStream pb true)
    (let [process (.start pb)]
      (slurp (.getInputStream process))
      (.waitFor process))
    out))

(defdescribe
  wrapper-release-asset-test
  (it
    "installs the release bundle and reports a missing asset honestly"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-asset-test-" (make-array FileAttribute 0)))

       install-dir
       (doto (io/file root "install") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       tools
       (doto (io/file root "tools") .mkdirs)

       fixtures
       (doto (io/file root "fixtures") .mkdirs)

       launcher
       (io/file install-dir "vis-agent")

       native
       (io/file install-dir "vis-agent-native")

       run-update!
       (fn [json asset-file]
         (let
           [pb
            (ProcessBuilder. ^java.util.List
                             ["bash" (.getAbsolutePath launcher) "update" "--native"])

            env
            (.environment pb)]

           (.directory pb root)
           (.redirectErrorStream pb true)
           (.put env "HOME" (.getAbsolutePath home))
           (.put env "VIS_HOME" (.getAbsolutePath vis-home))
           (.put env "VIS_NO_DEV_CHECKOUT" "1")
           (.put env "PATH" (str (.getAbsolutePath tools) ":/usr/bin:/bin"))
           (.put env "VIS_TEST_RELEASE_JSON" (.getAbsolutePath ^java.io.File json))
           (.put env
                 "VIS_TEST_ASSET_FILE"
                 (str (some-> ^java.io.File asset-file
                              .getAbsolutePath)))
           (let
             [process
              (.start pb)

              output
              (slurp (.getInputStream process))]

             {:exit (.waitFor process) :output output})))]

      (try (Files/copy (.toPath (io/file "bin/vis-agent"))
                       (.toPath launcher)
                       (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
           (.setExecutable launcher true)
           ;; The release API and every download are served from local fixtures, so
           ;; the resolution/install logic is covered without touching the network.
           (write-executable! (io/file tools "uname")
                              (str "#!/usr/bin/env bash\n"
                                   "case \"${1:-}\" in\n" "  -m) echo x86_64 ;;\n"
                                   "  *) echo Linux ;;\n" "esac\n"))
           (write-executable! (io/file tools "curl")
                              (str "#!/usr/bin/env bash\n"
                                   "out=\"\"; url=\"\"\n" "while [[ $# -gt 0 ]]; do\n"
                                   "  case \"$1\" in\n" "    -o) out=\"$2\"; shift 2 ;;\n"
                                   "    -*) shift ;;\n" "    *) url=\"$1\"; shift ;;\n"
                                   "  esac\n" "done\n"
                                   "case \"$url\" in\n"
                                   "  *api.github.com*) cat \"$VIS_TEST_RELEASE_JSON\" ;;\n"
                                   "  *) cp \"$VIS_TEST_ASSET_FILE\" \"$out\" ;;\n" "esac\n"))
           (let [json (io/file fixtures "empty.json")]
             (spit json (release-json))
             (let [{:keys [exit output]} (run-update! json nil)]
               (expect (= 1 exit) output)
               (expect (str/includes? output "vis-agent-linux-x64-community.tar.gz"))))
           (let
             [json
              (io/file fixtures "bundle.json")

              staged
              (doto (io/file fixtures "bundle") .mkdirs)]

             (spit json (release-json "vis-agent-linux-x64-community.tar.gz"))
             (write-executable! (io/file staged "vis-agent")
                                "#!/usr/bin/env bash\necho BUNDLED-LAUNCHER\n")
             (write-executable! (io/file staged "vis-agent-native")
                                "#!/usr/bin/env bash\necho BUNDLED-NATIVE\n")
             (write-executable! (io/file staged "install-vis-agent")
                                "#!/usr/bin/env bash\necho BUNDLED-INSTALLER\n")
             (let
               [tarball
                (tar-gz! staged (io/file fixtures "bundle.tar.gz"))

                {:keys [exit output]}
                (run-update! json tarball)]

               (expect (= 0 exit) output)
               (expect (str/includes? output "installed launcher + native runtime"))
               (expect (str/includes? (slurp native) "BUNDLED-NATIVE"))
               (expect (str/includes? (slurp launcher) "BUNDLED-LAUNCHER"))
               (expect (str/includes? (slurp (io/file install-dir "install-vis-agent"))
                                      "BUNDLED-INSTALLER"))))
           (finally (delete-tree! root))))))

(defn- copy-launcher!
  "Copies the repository launcher to `dest` as an executable."
  [dest]
  (Files/copy (.toPath (io/file "bin/vis-agent"))
              (.toPath ^java.io.File dest)
              (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
  (.setExecutable ^java.io.File dest true)
  dest)

(defn- wrapper-runner
  "Returns `(fn [args] [args env])` running the copied launcher with a sandboxed
   HOME/VIS_HOME/PATH. `env` adds or overrides single variables."
  [{:keys [launcher cwd home vis-home tools]}]
  (fn run! ([args] (run! args {}))
    ([args env-extra]
     (let
       [pb
        (ProcessBuilder. ^java.util.List
                         (into ["bash" (.getAbsolutePath ^java.io.File launcher)] args))

        env
        (.environment pb)]

       (.directory pb ^java.io.File cwd)
       (.redirectErrorStream pb true)
       (.put env "HOME" (.getAbsolutePath ^java.io.File home))
       (.put env "VIS_HOME" (.getAbsolutePath ^java.io.File vis-home))
       (.put env "VIS_NO_DEV_CHECKOUT" "1")
       (.put env "PATH" (str (.getAbsolutePath ^java.io.File tools) ":/usr/bin:/bin"))
       (doseq [[k v] env-extra]
         (.put env (str k) (str v)))
       (let
         [process
          (.start pb)

          output
          (slurp (.getInputStream process))]

         {:exit (.waitFor process) :output output})))))

(defdescribe
  wrapper-clojure-install-test
  (it
    "installs the Clojure CLI into Vis state when a JVM runtime needs it"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-clojure-test-" (make-array FileAttribute 0)))

       install-dir
       (doto (io/file root "install") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       managed-src
       (doto (io/file vis-home "install/src") .mkdirs)

       tools
       (doto (io/file root "tools") .mkdirs)

       launcher
       (io/file install-dir "vis-agent")

       installer
       (io/file root "clojure-installer.sh")]

      (try
        (Files/copy (.toPath (io/file "bin/vis-agent"))
                    (.toPath launcher)
                    (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
        (.setExecutable launcher true)
        (spit (io/file managed-src "deps.edn") "{}")
        (spit
          installer
          (str
            "#!/usr/bin/env bash\n"
            "set -euo pipefail\n" "prefix=''\n"
            "while [[ $# -gt 0 ]]; do\n"
            "  case \"$1\" in --prefix) prefix=\"$2\"; shift 2 ;; *) shift ;; esac\n"
            "done\n" "mkdir -p \"$prefix/bin\"\n"
            "printf '#!/usr/bin/env bash\\nprintf installed-clojure\\n' > \"$prefix/bin/clojure\"\n"
            "chmod +x \"$prefix/bin/clojure\"\n"))
        (.setExecutable installer true)
        (write-executable! (io/file tools "uname") "#!/usr/bin/env bash\nprintf 'Linux\\n'\n")
        (write-executable! (io/file tools "curl")
                           (str "#!/usr/bin/env bash\n" "out=''\n"
                                "while [[ $# -gt 0 ]]; do\n"
                                "  case \"$1\" in -o) out=\"$2\"; shift 2 ;; *) shift ;; esac\n"
                                "done\n" "cp \"$VIS_TEST_CLOJURE_INSTALLER\" \"$out\"\n"))
        (let
          [{:keys [exit output]}
           ((wrapper-runner
              {:launcher launcher :cwd root :home home :vis-home vis-home :tools tools})
             ["runtime" "use" "jvm"]
             {"VIS_TEST_CLOJURE_INSTALLER" (.getAbsolutePath installer)})]
          (expect (= 0 exit) output)
          (expect (str/includes? output "Clojure CLI not found; installing it under") output)
          (expect (.isFile (io/file vis-home "install/clojure/bin/clojure")) output))
        (finally (delete-tree! root))))))

(defn- git!
  "Runs git in `dir` with a deterministic identity; returns exit + merged output."
  [dir & args]
  (let
    [pb
     (ProcessBuilder. ^java.util.List (into ["git"] args))

     env
     (.environment pb)]

    (.directory pb ^java.io.File dir)
    (.redirectErrorStream pb true)
    (.put env "GIT_AUTHOR_NAME" "vis-test")
    (.put env "GIT_AUTHOR_EMAIL" "vis-test@example.invalid")
    (.put env "GIT_COMMITTER_NAME" "vis-test")
    (.put env "GIT_COMMITTER_EMAIL" "vis-test@example.invalid")
    (let
      [process
       (.start pb)

       output
       (slurp (.getInputStream process))]

      {:exit (.waitFor process) :output output})))

(defdescribe
  wrapper-dev-mode-test
  (it
    "follows releases by default and enters the live checkout only in dev mode"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-dev-test-" (make-array FileAttribute 0)))

       install-dir
       (doto (io/file root "install") .mkdirs)

       checkout
       (doto (io/file root "checkout") .mkdirs)

       checkout-bin
       (doto (io/file checkout "bin") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       tools
       (doto (io/file root "tools") .mkdirs)

       launcher
       (io/file install-dir "vis-agent")

       run!
       (wrapper-runner {:launcher launcher :cwd root :home home :vis-home vis-home :tools tools})

       ;; The handoff is what dev mode is FOR, so it must be allowed here.
       dev-env
       {"VIS_DEV_CHECKOUT" (.getAbsolutePath checkout) "VIS_NO_DEV_CHECKOUT" ""}]

      (try (copy-launcher! launcher)
           (copy-launcher! (io/file checkout-bin "vis-agent"))
           (spit (io/file checkout "deps.edn") "{}")
           (write-executable! (io/file install-dir "vis-agent-native")
                              "#!/usr/bin/env bash\nprintf 'NATIVE:%s\\n' \"$*\"\n")
           (write-executable! (io/file tools "clojure")
                              "#!/usr/bin/env bash\nprintf 'JVM:%s\\n' \"$*\"\n")
           (write-executable! (io/file tools "java") "#!/usr/bin/env bash\nexit 0\n")
           ;; A live checkout next door never wins on its own any more.
           (let [{:keys [exit output]} (run! ["--version"] dev-env)]
             (expect (= 0 exit) output)
             (expect (str/includes? output "NATIVE:")))
           (let [{:keys [exit output]} (run! ["--dev" "--version"] dev-env)]
             (expect (= 0 exit) output)
             (expect (str/includes? output "JVM:")))
           (let [{:keys [output]} (run! ["--version"] (assoc dev-env "VIS_RUNTIME" "dev"))]
             (expect (str/includes? output "JVM:")))
           (let [{:keys [exit output]} (run! ["runtime" "use" "dev"] dev-env)]
             (expect (= 0 exit) output)
             (expect (str/includes? output "runtime is now dev")))
           (let [{:keys [output]} (run! ["--version"] dev-env)]
             (expect (str/includes? output "JVM:")))
           (let [{:keys [output]} (run! ["runtime" "show"] dev-env)]
             (expect (str/includes? output "Runtime:      dev")))
           (let [{:keys [exit output]} (run! ["runtime" "use" "auto"] dev-env)]
             (expect (= 0 exit) output)
             (expect (str/includes? output "automatic")))
           (let [{:keys [output]} (run! ["--version"] dev-env)]
             (expect (str/includes? output "NATIVE:")))
           ;; dev names ONE checkout. Pointed at a missing one the wrapper must
           ;; say so, never silently run the checkout it happens to sit in.
           (let
             [run-from-checkout!
              (wrapper-runner {:launcher (io/file checkout-bin "vis-agent")
                               :cwd root
                               :home home
                               :vis-home vis-home
                               :tools tools})

              {:keys [exit output]}
              (run-from-checkout! ["--dev" "--version"]
                                  (assoc dev-env
                                    "VIS_DEV_CHECKOUT" (.getAbsolutePath (io/file root "gone"))))]

             (expect (= 1 exit) output)
             (expect (str/includes? output "is not a checkout") output))
           (finally (delete-tree! root))))))

(defdescribe
  wrapper-native-precedence-test
  (it
    "runs the released native, and a self-built one only when none is installed"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-native-test-" (make-array FileAttribute 0)))

       install-dir
       (doto (io/file root "install") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       managed-src
       (doto (io/file vis-home "install/src") .mkdirs)

       tools
       (doto (io/file root "tools") .mkdirs)

       launcher
       (io/file install-dir "vis-agent")

       ;; What `vis-agent update --native` downloads from the release tag.
       released
       (io/file vis-home "install/vis-agent-native")

       ;; What `clojure -T:build native` leaves in that source tree.
       self-built
       (io/file managed-src "target/vis")

       run!
       (wrapper-runner {:launcher launcher :cwd root :home home :vis-home vis-home :tools tools})]

      (try (copy-launcher! launcher)
           (spit (io/file managed-src "deps.edn") "{}")
           (.mkdirs (io/file managed-src "target"))
           (write-executable! released "#!/usr/bin/env bash\nprintf 'RELEASED:%s\\n' \"$*\"\n")
           (write-executable! self-built "#!/usr/bin/env bash\nprintf 'SELF-BUILT:%s\\n' \"$*\"\n")
           ;; A binary you built yourself must never shadow the published release.
           (let [{:keys [exit output]} (run! ["--native" "--version"])]
             (expect (= 0 exit) output)
             (expect (str/includes? output "RELEASED:") output))
           (let [{:keys [output]} (run! ["runtime" "show"])]
             (expect (str/includes? output (.getAbsolutePath released)) output))
           ;; With no release installed, --native is the build that source made.
           (io/delete-file released)
           (let [{:keys [exit output]} (run! ["--native" "--version"])]
             (expect (= 0 exit) output)
             (expect (str/includes? output "SELF-BUILT:") output))
           (finally (delete-tree! root))))))

(defn- branch-source-fixture
  "A sandboxed wrapper install plus a throwaway origin: v0.1.1, v0.1.9 and
   v0.1.10 — published tags the source must IGNORE — followed by untagged work
   on `main`, which is the commit an update has to land on. `update!` runs the
   wrapper against that origin rather than GitHub; `run!` is the same wrapper
   without it."
  []
  (let
    [root
     (.toFile (Files/createTempDirectory "vis-agent-branch-test-" (make-array FileAttribute 0)))

     origin
     (doto (io/file root "origin") .mkdirs)

     install-dir
     (doto (io/file root "install") .mkdirs)

     home
     (doto (io/file root "home") .mkdirs)

     vis-home
     (doto (io/file home ".vis") .mkdirs)

     tools
     (doto (io/file root "tools") .mkdirs)

     launcher
     (io/file install-dir "vis-agent")

     run!
     (wrapper-runner {:launcher launcher :cwd root :home home :vis-home vis-home :tools tools})

     commit!
     (fn [content tag]
       (spit (io/file origin "f") content)
       (git! origin "add" "-A")
       (git! origin "commit" "-qm" (str "commit " content))
       (when tag (git! origin "tag" tag)))]

    (copy-launcher! launcher)
    (write-executable! (io/file tools "clojure") "#!/usr/bin/env bash\nprintf 'JVM:%s\\n' \"$*\"\n")
    (git! origin "init" "-q" "-b" "main" ".")
    (spit (io/file origin "deps.edn") "{}")
    (commit! "one" "v0.1.1")
    (commit! "two" "v0.1.9")
    (commit! "three" "v0.1.10")
    (commit! "unreleased" nil)
    (.mkdirs (io/file vis-home "install"))
    (spit (io/file vis-home "runtime") "jvm\n")
    {:root root
     :origin origin
     :vis-home vis-home
     :managed-src (io/file vis-home "install/src")
     :head (str/trim (:output (git! origin "rev-parse" "HEAD")))
     :run! run!
     :update! (fn [args]
                (run! args {"VIS_REPO_URL" (.getAbsolutePath origin)}))}))

(defn- moving-refs
  "Every branch and remote-tracking ref in `src` — what a pinned source must not
   have, because each one is a ref `git pull` can follow off the pin."
  [src]
  (str/trim (:output (git! src "for-each-ref" "--format=%(refname)" "refs/heads" "refs/remotes"))))

(defdescribe
  wrapper-source-update-test
  (it
    "pins the managed source to the newest commit on main, or to a named ref"
    (let [{:keys [root origin head vis-home managed-src run! update!]} (branch-source-fixture)]
      (try ;; Nothing is prepared on disk: the wrapper acquires the source itself.
        (let [{:keys [exit output]} (update! ["update"])]
          (expect (= 0 exit) output)
          (expect (str/includes? output (str "source pinned at " head)) output))
        ;; Tags are never consulted: v0.1.10 is published, and the update landed
        ;; on the untagged commit after it.
        (expect (= head (str/trim (slurp (io/file vis-home "install/ref")))))
        (expect (= head (str/trim (:output (git! managed-src "rev-parse" "HEAD")))))
        ;; Pinned means pinned: no branch to step onto, no remote-tracking ref
        ;; to follow, and ONE commit of history instead of the whole repo.
        (expect (= "" (moving-refs managed-src)))
        (expect (= "true"
                   (str/trim (:output (git! managed-src "rev-parse" "--is-shallow-repository")))))
        (expect (= "1" (str/trim (:output (git! managed-src "rev-list" "--count" "HEAD")))))
        ;; The regression: `git checkout main && git pull` in there used to
        ;; walk the source Vis runs a hundred commits past its pin.
        (expect (not= 0 (:exit (git! managed-src "checkout" "main"))))
        (let [{:keys [output]} (run! ["runtime" "show"])]
          (expect (str/includes? output (str "Pinned at:    " head)))
          (expect (str/includes? output "Runtime:      jvm")))
        ;; An exact ref you name still pins source, whatever is effective.
        (let
          [sha (str/trim (:output (git! origin "rev-parse" "v0.1.9^{commit}")))
           {:keys [exit output]} (update! ["update" sha])]

          (expect (= 0 exit) output)
          (expect (str/includes? output (str "source pinned at " sha)))
          (expect (= sha (str/trim (slurp (io/file vis-home "install/ref")))))
          (expect (= sha (str/trim (:output (git! managed-src "rev-parse" "HEAD")))))
          (expect (= "" (moving-refs managed-src))))
        ;; Source moved off its pin by hand is REPORTED, never quietly run.
        (git! managed-src "checkout" "--force" "--detach" head)
        (expect (str/includes? (:output (run! ["runtime" "show"])) "DRIFTED"))
        ;; One runtime per update: naming two is a refusal, not a guess.
        (let [{:keys [exit output]} (run! ["update" "--native" "--dev"])]
          (expect (= 1 exit) output)
          (expect (str/includes? output "name one runtime, not --native and --dev")))
        (finally (delete-tree! root)))))
  (it "repins source an older wrapper left cloned on a branch"
      (let [{:keys [root origin head managed-src run! update!]} (branch-source-fixture)]
        (try ;; What a clone leaves behind: main, its remote-tracking twin, and the
             ;; whole history to fast-forward through.
          (git! root "clone" "-q" (.getAbsolutePath origin) (.getAbsolutePath managed-src))
          (expect (str/includes? (moving-refs managed-src) "refs/heads/main"))
          (let [{:keys [exit output]} (update! ["update"])]
            (expect (= 0 exit) output)
            (expect (str/includes? output (str "source pinned at " head))))
          (expect (= head (str/trim (:output (git! managed-src "rev-parse" "HEAD")))))
          (expect (= "" (moving-refs managed-src)))
          (expect (not= 0 (:exit (git! managed-src "checkout" "main"))))
          (expect (str/includes? (:output (run! ["runtime" "show"])) (str "Pinned at:    " head)))
          (finally (delete-tree! root)))))
  ;; Naming a runtime on `update` IS choosing it: one command installs, updates
  ;; and selects. An update that names nothing must leave the selection alone,
  ;; so automatic stays automatic.
  (it "persists the runtime an update names, and leaves automatic alone"
      (let
        [{:keys [root vis-home update!]}
         (branch-source-fixture)

         runtime-file
         (io/file vis-home "runtime")]

        (try (io/delete-file runtime-file)
             (let [{:keys [exit output]} (update! ["update"])]
               (expect (= 0 exit) output)
               (expect (not (str/includes? output "runtime is now")) output))
             (expect (not (.exists runtime-file)))
             (let [{:keys [exit output]} (update! ["update" "jvm"])]
               (expect (= 0 exit) output)
               (expect (str/includes? output "runtime is now jvm") output))
             (expect (= "jvm" (str/trim (slurp runtime-file))))
             (finally (delete-tree! root))))))

(defn- installer-fixture
  "The real `bin/install-vis-agent` against a throwaway origin whose tip commit
   is untagged, with stub `java`/`clojure`/`curl` on PATH. Nothing here reaches
   GitHub: the runtime comes from that origin, and the wrapper either from this
   checkout (`install!`) or from the stub curl, which records the URL it was
   asked for and answers with this checkout's wrapper (`install-detached!`)."
  []
  (let
    [root
     (.toFile (Files/createTempDirectory "vis-agent-installer-test-" (make-array FileAttribute 0)))

     origin
     (doto (io/file root "origin") .mkdirs)

     install-dir
     (io/file root "bin")

     home
     (doto (io/file root "home") .mkdirs)

     vis-home
     (doto (io/file home ".vis") .mkdirs)

     tools
     (doto (io/file root "tools") .mkdirs)

     curl-log
     (io/file root "curl-args")

     detached-installer
     (io/file root "install-vis-agent")

     env
     {"VIS_INSTALL_DIR" (.getAbsolutePath install-dir) "VIS_REPO_URL" (.getAbsolutePath origin)}

     run!
     (wrapper-runner {:launcher (io/file "bin/install-vis-agent")
                      :cwd root
                      :home home
                      :vis-home vis-home
                      :tools tools})

     run-detached!
     (wrapper-runner
       {:launcher detached-installer :cwd root :home home :vis-home vis-home :tools tools})]

    (write-executable!
      (io/file tools "java")
      "#!/usr/bin/env bash\nprintf 'openjdk version \"25.0.2\" 2026-01-20\\n' >&2\n")
    (write-executable! (io/file tools "clojure") "#!/usr/bin/env bash\nprintf 'JVM:%s\\n' \"$*\"\n")
    (write-executable!
      (io/file tools "curl")
      (str
        "#!/usr/bin/env bash\n"
        "printf '%s\\n' \"$*\" >> "
        (pr-str (.getAbsolutePath curl-log))
        "\n"
        "out=\"\"\n"
        "while [[ $# -gt 0 ]]; do case \"$1\" in -o) out=\"$2\"; shift 2 ;; *) shift ;; esac; done\n"
        "[[ -z \"$out\" ]] || cp "
        (pr-str (.getAbsolutePath (io/file "bin/vis-agent")))
        " \"$out\"\n"))
    ;; A copy outside any checkout: no sibling `vis-agent`, no `../deps.edn`, so
    ;; the installer must download the command instead of copying it.
    (Files/copy (.toPath (io/file "bin/install-vis-agent"))
                (.toPath ^java.io.File detached-installer)
                (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
    (.setExecutable ^java.io.File detached-installer true)
    (git! origin "init" "-q" "-b" "main" ".")
    (spit (io/file origin "deps.edn") "{}")
    (spit (io/file origin "f") "released")
    (git! origin "add" "-A")
    (git! origin "commit" "-qm" "released")
    (git! origin "tag" "v9.9.9")
    (spit (io/file origin "f") "unreleased")
    (git! origin "add" "-A")
    (git! origin "commit" "-qm" "unreleased")
    {:root root
     :vis-home vis-home
     :install-dir install-dir
     :curl-log curl-log
     :head (str/trim (:output (git! origin "rev-parse" "HEAD")))
     :install! (fn [args]
                 (run! args env))
     :install-detached! (fn [args]
                          (run-detached! args env))}))

(defdescribe
  installer-test
  ;; The installer used to default to the native runtime and pin the source one
  ;; to the newest release tag. It installs exactly one runtime now — JVM source
  ;; at the newest commit — because a published tag can be broken source.
  (it "installs the JVM source runtime at the newest commit, and no native one"
      (let [{:keys [root vis-home install-dir head install!]} (installer-fixture)]
        (try (let [{:keys [exit output]} (install! [])]
               (expect (= 0 exit) output)
               (expect (str/includes? output "source pinned at") output)
               ;; No bundle is resolved, downloaded or unpacked.
               (expect (not (str/includes? output "vis-agent-native")) output))
             (expect (.canExecute (io/file install-dir "vis-agent")))
             (expect (.isFile (io/file vis-home "install/src/deps.edn")))
             (expect (not (.exists (io/file install-dir "vis-agent-native"))))
             (expect (= "jvm" (str/trim (slurp (io/file vis-home "runtime")))))
             ;; v9.9.9 is published and is NOT what was installed.
             (expect (= head (str/trim (slurp (io/file vis-home "install/ref")))))
             (finally (delete-tree! root)))))
  (it "has no runtime left to choose"
      (let [{:keys [root install!]} (installer-fixture)]
        (try (let [{:keys [exit output]} (install! ["--runtime" "native"])]
               (expect (= 2 exit) output)
               (expect (str/includes? output "unknown argument: --runtime") output))
             (finally (delete-tree! root)))))
  ;; Regression: the one-liner and the wrapper download were moved to
  ;; raw.githubusercontent.com, which corporate networks block, so the
  ;; documented install could not even fetch the command there. They live on
  ;; the rolling `installer` release now — a release asset that is refreshed by
  ;; every commit on main, so it is as fresh as raw.githubusercontent.com was.
  (it "downloads the command from the rolling installer release, never raw.githubusercontent.com"
      (let [{:keys [root vis-home install-dir head curl-log install-detached!]} (installer-fixture)]
        (try (let [{:keys [exit output]} (install-detached! [])]
               (expect (= 0 exit) output)
               (expect (str/includes? output "source pinned at") output))
             (let [asked (slurp curl-log)]
               (expect (str/includes?
                         asked
                         "https://github.com/Blockether/vis/releases/download/installer/vis-agent")
                       asked)
               (expect (not (str/includes? asked "raw.githubusercontent.com")) asked))
             (expect (.canExecute (io/file install-dir "vis-agent")))
             ;; The asset only bootstraps: the runtime is still the branch tip.
             (expect (= head (str/trim (slurp (io/file vis-home "install/ref")))))
             (finally (delete-tree! root))))))

(defn- dev-checkout-fixture
  "A sandboxed wrapper install plus a throwaway origin whose only branch is
   `main`. The dev checkout is only a PATH here: each test decides whether one
   exists, because acquiring it is the wrapper's job."
  []
  (let
    [root
     (.toFile (Files/createTempDirectory "vis-agent-update-dev-test-" (make-array FileAttribute 0)))

     origin
     (doto (io/file root "origin") .mkdirs)

     install-dir
     (doto (io/file root "install") .mkdirs)

     home
     (doto (io/file root "home") .mkdirs)

     vis-home
     (doto (io/file home ".vis") .mkdirs)

     tools
     (doto (io/file root "tools") .mkdirs)

     checkout
     (io/file root "checkout")

     launcher
     (io/file install-dir "vis-agent")

     run!
     (wrapper-runner {:launcher launcher :cwd root :home home :vis-home vis-home :tools tools})

     commit!
     (fn [content]
       (spit (io/file origin "f") content)
       (git! origin "add" "-A")
       (git! origin "commit" "-qm" (str "commit " content)))]

    (copy-launcher! launcher)
    (git! origin "init" "-q" "-b" "main" ".")
    (spit (io/file origin "deps.edn") "{}")
    (commit! "one")
    {:root root
     :origin origin
     :checkout checkout
     :vis-home vis-home
     :run! run!
     :commit! commit!
     :clone-checkout!
     (fn []
       (git! root "clone" "-q" (.getAbsolutePath origin) (.getAbsolutePath checkout)))
     :dev-env {"VIS_DEV_CHECKOUT" (.getAbsolutePath checkout)
               "VIS_REPO_URL" (.getAbsolutePath origin)}}))

(defdescribe
  wrapper-update-dev-test
  (it "updates the live checkout for the bare word `dev`, not only for `--dev`"
      (let [{:keys [root checkout run! commit! clone-checkout! dev-env]} (dev-checkout-fixture)]
        (try (clone-checkout!)
             (commit! "two")
             ;; `update dev` names the dev runtime. It used to fall through to the
             ;; target case and pin the source Vis owns to a ref called 'dev'.
             (let [{:keys [exit output]} (run! ["update" "dev"] dev-env)]
               (expect (= 0 exit) output)
               (expect (str/includes? output "checkout updated") output)
               (expect (not (str/includes? output "pinning source")) output))
             (expect (= "two" (slurp (io/file checkout "f"))))
             ;; dev follows its branch, so a target beside it is a refusal.
             (let [{:keys [exit output]} (run! ["update" "dev" "v1.2.3"] dev-env)]
               (expect (= 1 exit) output)
               (expect (str/includes? output "takes no target") output))
             ;; A runtime already named makes the word a ref again.
             (let [{:keys [exit output]} (run! ["update" "--native" "dev"] dev-env)]
               (expect (= 1 exit) output)
               (expect (str/includes? output "expected a release tag") output))
             (finally (delete-tree! root)))))
  ;; Regression: switching to dev without a checkout used to be a dead end —
  ;; `vis-agent update` died with "no git checkout at ~/vis" and told the user to
  ;; clone it by hand. Dev means main, and the wrapper clones it itself.
  (it "clones the dev checkout on main when there is none yet"
      (let [{:keys [root checkout run! commit! dev-env]} (dev-checkout-fixture)]
        (try (expect (not (.exists (io/file checkout ".git"))))
             (let [{:keys [exit output]} (run! ["update" "dev"] dev-env)]
               (expect (= 0 exit) output)
               (expect (str/includes? output "cloning") output)
               (expect (not (str/includes? output "no git checkout")) output))
             (expect (.isDirectory (io/file checkout ".git")))
             (expect (.isFile (io/file checkout "deps.edn")))
             ;; Dev means MAIN: a branch to follow, not a detached pin.
             (expect (= "main"
                        (str/trim (:output (git! checkout "rev-parse" "--abbrev-ref" "HEAD")))))
             (expect (= "one" (slurp (io/file checkout "f"))))
             ;; And the clone it just made is one it can fast-forward next time.
             (commit! "two")
             (let [{:keys [exit output]} (run! ["update" "dev"] dev-env)]
               (expect (= 0 exit) output)
               (expect (str/includes? output "checkout updated") output))
             (expect (= "two" (slurp (io/file checkout "f"))))
             (finally (delete-tree! root)))))
  ;; Regression: `update dev` fetched the checkout and then left you on the old
  ;; runtime — running Vis from it needed `vis-agent runtime use dev` as a second
  ;; command. One command is enough.
  (it "selects the runtime it just updated"
      (let [{:keys [root vis-home run! dev-env]} (dev-checkout-fixture)]
        (try (let [{:keys [exit output]} (run! ["update" "dev"] dev-env)]
               (expect (= 0 exit) output)
               (expect (str/includes? output "runtime is now dev") output))
             (expect (= "dev" (str/trim (slurp (io/file vis-home "runtime")))))
             (expect (str/includes? (:output (run! ["runtime" "show"] dev-env))
                                    "Runtime:      dev"))
             (finally (delete-tree! root)))))
  (it "points a switch to dev at the command that fetches the checkout"
      (let [{:keys [root checkout run! dev-env]} (dev-checkout-fixture)]
        (try (let [{:keys [exit output]} (run! ["runtime" "use" "dev"] dev-env)]
               (expect (= 0 exit) output)
               (expect (str/includes? output "vis-agent update --dev") output))
             (expect (not (.exists checkout)))
             (finally (delete-tree! root)))))
  (it "refuses to clone over a directory that is not a checkout"
      (let [{:keys [root checkout run! dev-env]} (dev-checkout-fixture)]
        (try (.mkdirs checkout)
             (spit (io/file checkout "mine.txt") "not a checkout")
             (let [{:keys [exit output]} (run! ["update" "dev"] dev-env)]
               (expect (= 1 exit) output)
               (expect (str/includes? output "is not a git checkout") output))
             (expect (= "not a checkout" (slurp (io/file checkout "mine.txt"))))
             (finally (delete-tree! root))))))

(defdescribe
  wrapper-update-command-test
  (it
    "refreshes the installed vis-agent command from the source a jvm update pinned"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-update-command-test-"
                                           (make-array FileAttribute 0)))

       origin
       (doto (io/file root "origin") .mkdirs)

       install-dir
       (doto (io/file root "install") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       tools
       (doto (io/file root "tools") .mkdirs)

       managed-src
       (io/file vis-home "install/src")

       launcher
       (io/file install-dir "vis-agent")

       run!
       (wrapper-runner {:launcher launcher :cwd root :home home :vis-home vis-home :tools tools})

       marker
       "# VIS_TEST_NEWER_WRAPPER"

       ;; A released wrapper that differs from the installed one, and still works:
       ;; the marker is a comment on the line after the shebang.
       newer-wrapper
       (str/replace-first (slurp "bin/vis-agent") "\n" (str "\n" marker "\n"))

       ;; Every update runs against that origin, never against GitHub.
       repo-env
       {"VIS_REPO_URL" (.getAbsolutePath origin)}]

      (try
        (copy-launcher! launcher)
        (git! origin "init" "-q" "-b" "main" ".")
        (spit (io/file origin "deps.edn") "{}")
        (.mkdirs (io/file origin "bin"))
        (write-executable! (io/file origin "bin/vis-agent") newer-wrapper)
        (git! origin "add" "-A")
        (git! origin "commit" "-qm" "one")
        (git! origin "tag" "v0.3.0")
        (.mkdirs (io/file vis-home "install"))
        (git! root "clone" "-q" (.getAbsolutePath origin) (.getAbsolutePath managed-src))
        (spit (io/file vis-home "runtime") "jvm\n")
        ;; The installed command is a COPY made at install time. Nothing but this
        ;; sync refreshes it, so a source update must carry it along.
        (let [{:keys [exit output]} (run! ["update"] repo-env)]
          (expect (= 0 exit) output)
          (expect (str/includes? output "vis-agent command updated from") output))
        (expect (str/includes? (slurp launcher) marker))
        ;; The replacement is a working command, and an update that changes
        ;; nothing says nothing.
        (let [{:keys [exit output]} (run! ["update"] repo-env)]
          (expect (= 0 exit) output)
          (expect (not (str/includes? output "vis-agent command updated from")) output))
        ;; A wrapper that lives IN a checkout is source: git owns it, never this.
        (let
          [checkout-bin
           (doto (io/file root "checkout/bin") .mkdirs)

           checkout-launcher
           (io/file checkout-bin "vis-agent")

           run-checkout!
           (do
             (spit (io/file root "checkout/deps.edn") "{}")
             (copy-launcher! checkout-launcher)
             (wrapper-runner
               {:launcher checkout-launcher :cwd root :home home :vis-home vis-home :tools tools}))

           {:keys [exit output]}
           (run-checkout! ["update"] repo-env)]

          (expect (= 0 exit) output)
          (expect (not (str/includes? (slurp checkout-launcher) marker)) output))
        (finally (delete-tree! root))))))

(defn- run-wrapper
  "Run the real `bin/vis-agent` from a copy, with HOME/VIS_HOME under `root` and
   a fake native runtime that echoes the argv the app would receive."
  [args & {:keys [vis-home] :as _opts}]
  (let
    [root
     (.toFile (Files/createTempDirectory "vis-agent-argv-test-" (make-array FileAttribute 0)))

     bin
     (doto (io/file root "bin") .mkdirs)

     home
     (doto (io/file root "home") .mkdirs)

     launcher
     (io/file bin "vis-agent")

     native
     (io/file bin "vis-agent-native")]

    (Files/copy (.toPath (io/file "bin/vis-agent"))
                (.toPath launcher)
                (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
    (.setExecutable launcher true)
    (write-executable! native "#!/usr/bin/env bash\nprintf 'ARGV[%s]\\n' \"$@\"\n")
    (let
      [pb
       (ProcessBuilder. ^java.util.List (into ["bash" (.getAbsolutePath launcher)] args))

       env
       (.environment pb)]

      (.directory pb root)
      (.redirectErrorStream pb true)
      (.put env "HOME" (.getAbsolutePath home))
      (.put env "VIS_HOME" (or vis-home (.getAbsolutePath (io/file home ".vis"))))
      (.put env "PATH" "/usr/bin:/bin")
      (.remove env "VIS_RUNTIME")
      (let
        [process
         (.start pb)

         output
         (slurp (.getInputStream process))

         exit
         (.waitFor process)]

        {:exit exit :output output :root root}))))

(defdescribe wrapper-argument-boundaries-test
             (it "lets -- end the WRAPPER's flag parsing, not just the app's"
                 (let [{:keys [exit output]} (run-wrapper ["--" "--dev" "--measure"])]
                   (expect (zero? exit) output)
                   ;; every word after -- reaches the app verbatim, and none of them
                   ;; silently switched runtime or turned measurement on.
                   (expect (str/includes? output "ARGV[--]") output)
                   (expect (str/includes? output "ARGV[--dev]") output)
                   (expect (str/includes? output "ARGV[--measure]") output)
                   (expect (not (str/includes? output "[vis measure]")) output)))
             (it "refuses `runtime use` combined with a one-launch runtime flag"
                 (let [{:keys [exit output]} (run-wrapper ["--dev" "runtime" "use" "jvm"])]
                   (expect (= 2 exit) output)
                   (expect (str/includes? output "runtime use sets the persisted runtime") output)
                   (expect (not (str/includes? output "runtime is now")) output)))
             (it "never claims a runtime it could not persist"
                 (let
                   [root
                    (.toFile (Files/createTempDirectory "vis-agent-home-test-"
                                                        (make-array FileAttribute 0)))

                    vis-home
                    (doto (io/file root "vishome") .mkdirs)

                    _
                    (.mkdirs (io/file vis-home "runtime"))

                    {:keys [exit output]}
                    (run-wrapper ["runtime" "use" "jvm"] :vis-home (.getAbsolutePath vis-home))]

                   (expect (= 1 exit) output)
                   (expect (str/includes? output "is not a regular file") output)
                   (expect (not (str/includes? output "runtime is now")) output))))

(defdescribe
  wrapper-language-resources-test
  (it
    "points the native runtime at the language-resources sidecar it ships with"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-resources-test-"
                                           (make-array FileAttribute 0)))

       install-dir
       (doto (io/file root "install") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       tools
       (doto (io/file root "tools") .mkdirs)

       launcher
       (copy-launcher! (io/file install-dir "vis-agent"))

       run!
       (wrapper-runner {:launcher launcher :cwd root :home home :vis-home vis-home :tools tools})

       resource-path
       (fn [output]
         (second (re-find #"-Dpolyglot\.engine\.resourcePath=(\S+)" output)))

       native-home
       (.getCanonicalPath install-dir)]

      (try (write-executable! (io/file install-dir "vis-agent-native")
                              "#!/usr/bin/env bash\nprintf 'NATIVE:%s\\n' \"$*\"\n")
           ;; Nothing on disk: never point the engine at a directory that does not
           ;; exist — an image with embedded resources must keep working.
           (let [{:keys [exit output]} (run! ["--version"])]
             (expect (= 0 exit))
             (expect (str/includes? output "NATIVE:"))
             (expect (nil? (resource-path output))))
           ;; Release layout: `vis-agent-resources/` beside the runtime.
           (.mkdirs (io/file install-dir "vis-agent-resources"))
           (expect (= (str native-home "/vis-agent-resources")
                      (resource-path (:output (run! ["--version"])))))
           ;; Build-tree layout: native-image itself writes plain `resources/`.
           (.mkdirs (io/file install-dir "resources"))
           (expect (= (str native-home "/vis-agent-resources")
                      (resource-path (:output (run! ["--version"])))))
           (delete-tree! (io/file install-dir "vis-agent-resources"))
           (expect (= (str native-home "/resources")
                      (resource-path (:output (run! ["--version"])))))
           ;; An explicit override always wins, so a shared install can host one copy.
           (let
             [shared
              (doto (io/file root "shared-resources") .mkdirs)

              {:keys [output]}
              (run! ["--version"] {"VIS_NATIVE_RESOURCES" (.getAbsolutePath shared)})]

             (expect (= (.getAbsolutePath shared) (resource-path output))))
           (finally (delete-tree! root))))))
