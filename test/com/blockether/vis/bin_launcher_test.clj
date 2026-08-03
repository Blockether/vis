(ns com.blockether.vis.bin-launcher-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(defn- write-executable! [file body] (spit file body) (.setExecutable ^java.io.File file true) file)

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
    {:root root :repo repo :home home :old-home old-home :pinned-home pinned-home :tools tools}))

(defn- run-launcher
  [{:keys [repo home old-home pinned-home tools]}]
  (let
    [old-java
     (.getAbsolutePath (io/file old-home "bin/java"))

     pb
     (ProcessBuilder. ^java.util.List
                      ["bash" (.getAbsolutePath (io/file repo "bin/vis-agent")) "--jvm"
                       "--version"])

     env
     (.environment pb)]

    (.directory pb repo)
    (.redirectErrorStream pb true)
    (.put env "HOME" (.getAbsolutePath home))
    (.put env
          "PATH"
          (str (.getAbsolutePath (io/file old-home "bin"))
               ":"
               (.getAbsolutePath tools)
               ":/usr/bin:/bin"))
    (.put env "JAVA_HOME" (.getAbsolutePath old-home))
    (.put env "JAVA_CMD" old-java)
    (.put env "VIS_TEST_PINNED_HOME" (.getAbsolutePath pinned-home))
    (.put env "VIS_NO_DEV_CHECKOUT" "1")
    (let
      [process
       (.start pb)

       output
       (slurp (.getInputStream process))

       exit
       (.waitFor process)]

      {:exit exit :output output})))

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
    [pb (ProcessBuilder. ^java.util.List
                         ["tar" "-C" (.getAbsolutePath ^java.io.File dir) "-czf"
                          (.getAbsolutePath ^java.io.File out) "."])]
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

(defdescribe
  wrapper-tagged-source-update-test
  (it
    "pins the managed source to the newest release tag, or to a named ref"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-agent-tag-test-" (make-array FileAttribute 0)))

       origin
       (doto (io/file root "origin") .mkdirs)

       install-dir
       (doto (io/file root "install") .mkdirs)

       home
       (doto (io/file root "home") .mkdirs)

       vis-home
       (doto (io/file home ".vis") .mkdirs)

       managed-src
       (io/file vis-home "install/src")

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

      (try (copy-launcher! launcher)
           (write-executable! (io/file tools "clojure")
                              "#!/usr/bin/env bash\nprintf 'JVM:%s\\n' \"$*\"\n")
           (git! origin "init" "-q" "-b" "main" ".")
           (spit (io/file origin "deps.edn") "{}")
           (commit! "one" "v0.1.1")
           ;; v0.1.10 must beat v0.1.9: the newest tag is a version sort, not a
           ;; lexicographic one.
           (commit! "two" "v0.1.9")
           (commit! "three" "v0.1.10")
           ;; Untagged work on the branch is exactly what the default must NOT follow.
           (commit! "unreleased" nil)
           (.mkdirs (io/file vis-home "install"))
           (git! root "clone" "-q" (.getAbsolutePath origin) (.getAbsolutePath managed-src))
           (spit (io/file vis-home "runtime") "jvm\n")
           (let [{:keys [exit output]} (run! ["update"])]
             (expect (= 0 exit) output)
             (expect (str/includes? output "source pinned at v0.1.10")))
           (expect (= "v0.1.10" (str/trim (slurp (io/file vis-home "install/ref")))))
           (expect (= "v0.1.10" (str/trim (:output (git! managed-src "describe" "--tags")))))
           (let [{:keys [output]} (run! ["runtime" "show"])]
             (expect (str/includes? output "Pinned at:    v0.1.10"))
             (expect (str/includes? output "Runtime:      jvm")))
           ;; A git ref that is not a release tag pins source, whatever is effective.
           (let
             [sha
              (str/trim (:output (git! origin "rev-parse" "HEAD")))

              {:keys [exit output]}
              (run! ["update" sha])]

             (expect (= 0 exit) output)
             (expect (str/includes? output (str "source pinned at " sha)))
             (expect (= sha (str/trim (slurp (io/file vis-home "install/ref"))))))
           ;; One runtime per update: naming two is a refusal, not a guess.
           (let [{:keys [exit output]} (run! ["update" "--native" "--dev"])]
             (expect (= 1 exit) output)
             (expect (str/includes? output "name one runtime, not --native and --dev")))
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
