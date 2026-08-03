(ns com.blockether.vis.release-bundle-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))

(defn- write-executable! [file body] (spit file body) (.setExecutable ^java.io.File file true) file)

(defn- run-bash
  "Runs `args` from the repository root with `env-extra` applied; merges stderr."
  [args env-extra]
  (let
    [pb
     (ProcessBuilder. ^java.util.List (vec args))

     env
     (.environment pb)]

    (.redirectErrorStream pb true)
    (doseq [[k v] env-extra]
      (.put env (str k) (str v)))
    (let
      [process
       (.start pb)

       output
       (slurp (.getInputStream process))]

      {:exit (.waitFor process) :output output})))

(defdescribe
  native-image-language-resources-test
  (it "keeps the language resources beside the image instead of inside it"
      (let
        [props (slurp "resources/META-INF/native-image/com.blockether/vis/native-image.properties")]
        ;; Embedding GraalPy's stdlib pushed the builder's live set past what a
        ;; 16 GB runner survives; the sidecar is what makes the release build finish.
        (expect (str/includes? props "-H:-IncludeLanguageResources") props)
        (expect (str/includes? props "-H:+CopyLanguageResources") props))))

(defdescribe
  stage-release-bundle-test
  (it
    "packs the sidecar and refuses a bundle that lost it"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-release-bundle-test-" (make-array FileAttribute 0)))

       from-dir
       (doto (io/file root "from") .mkdirs)

       bundle-dir
       (io/file root "bundle")

       asset
       (io/file root "vis-agent-linux-arm64-community.tar.gz")

       stage!
       (fn []
         (run-bash ["bash" "bin/stage-release-bundle" "--from-dir" (.getAbsolutePath from-dir)
                    (.getAbsolutePath asset)]
                   {"VIS_BUNDLE_DIR" (.getAbsolutePath bundle-dir)}))]

      (try (doseq [entry ["vis-agent" "vis-agent-native" "install-vis-agent"]]
             (write-executable! (io/file from-dir entry) "#!/usr/bin/env bash\nexit 0\n"))
           ;; A binary without its resources is a runtime whose first Python call
           ;; dies with "No module named 'ast'": hard error, never a warning.
           (let [{:keys [exit output]} (stage!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "vis-agent-resources") output)
             (expect (not (.isFile asset)) "no asset may survive a rejected bundle"))
           (.mkdirs (io/file from-dir "vis-agent-resources/python"))
           (spit (io/file from-dir "vis-agent-resources/python/marker") "stdlib\n")
           (let [{:keys [exit output]} (stage!)]
             (expect (= 0 exit) output)
             (expect (.isFile asset) output)
             (expect (= "stdlib\n"
                        (slurp (io/file bundle-dir "vis-agent-resources/python/marker"))))
             (doseq [entry ["vis-agent" "vis-agent-native" "install-vis-agent"]]
               (expect (.canExecute (io/file bundle-dir entry)) entry)))
           (finally (delete-tree! root))))))

(defn- fake-tools!
  "A PATH directory whose `uname` claims `os`/`arch`, plus a container engine that
  answers `--version`/`info`/`run` without a VM. Lets the host-target and
  emulation rules be exercised for hosts this machine is not."
  [os arch]
  (let
    [dir (.toFile (Files/createTempDirectory "vis-release-native-test-"
                                             (make-array FileAttribute 0)))]
    (write-executable! (io/file dir "uname")
                       (str "#!/usr/bin/env bash\n"
                            "case \"${1:-}\" in\n"
                            "  -s) printf '"
                            os
                            "\\n' ;;\n"
                            "  -m) printf '"
                            arch
                            "\\n' ;;\n"
                            "  *)  printf '" os
                            "\\n' ;;\n" "esac\n"))
    (write-executable! (io/file dir "engine")
                       (str "#!/usr/bin/env bash\n" "case \"${1:-}\" in\n"
                            ;; `info` doubles as the RAM probe: 32 GiB, so the
                            ;; memory guard passes and the emulation one decides.
                            "  --version) printf 'podman version 6.0.0\\n' ;;\n"
                            "  info)      printf '34359738368\\n' ;;\n"
                            "  *)         exit 0 ;;\n" "esac\n"))
    dir))

(defn- run-release-native
  [^java.io.File dir args env-extra]
  (run-bash (into ["bash" "bin/release-native"] args)
            (merge {"PATH" (str (.getAbsolutePath dir) ":" (System/getenv "PATH"))} env-extra)))

(defdescribe
  release-native-targets-test
  (it "builds every asset an Apple-silicon host can reach, and refuses the rest"
      (let
        [mac
         (fake-tools! "Darwin" "arm64")

         linux-arm
         (fake-tools! "Linux" "aarch64")]

        (try
          ;; linux-x64 belongs here now: native-image under Rosetta measured 4.8x
          ;; native (15.8 s vs 1 m 16 s for a hello-world image), still inside the
          ;; 86-130 min the free x64 runner takes when it does not OOM.
          (let [{:keys [exit output]} (run-release-native mac ["--list"] {})]
            (expect (= 0 exit) output)
            (doseq [target ["macos-arm64" "linux-arm64" "linux-x64"]]
              (expect (str/includes? output target) output)))
          ;; A Linux host cannot produce the macOS asset, whatever is installed.
          (let [{:keys [exit output]} (run-release-native linux-arm ["--targets" "macos-arm64"] {})]
            (expect (not= 0 exit) output)
            (expect (str/includes? output "cannot build") output))
          (finally (delete-tree! mac) (delete-tree! linux-arm))))))

(defdescribe release-native-emulation-guard-test
             (it "refuses a foreign platform that is not Rosetta-fast, before building anything"
                 (let [mac (fake-tools! "Darwin" "arm64")]
                   (try (let
                          [{:keys [exit output]}
                           ;; A budget no measurement can meet stands in for qemu-user: the guard
                           ;; must fire on the probe, never after an hour of analysis.
                           (run-release-native mac
                                               ["--targets" "linux-x64"]
                                               {"VIS_CONTAINER_CLI" (.getAbsolutePath
                                                                      (io/file mac "engine"))
                                                "VIS_EMULATION_MAX_SECONDS" "-1"})]
                          (expect (not= 0 exit) output)
                          (expect (str/includes? output "qemu-user") output)
                          (expect (str/includes? output "native-release.yml") output)
                          (expect (not (str/includes? output "building linux-x64")) output))
                        (finally (delete-tree! mac))))))

(defdescribe
  release-native-builder-machine-test
  (it
    "prefers a podman machine big enough for the builder over a small default"
    (let
      [mac
       (fake-tools! "Darwin" "arm64")

       podman
       (io/file mac "podman")]

      ;; Two machines, as on a real workstation: the everyday default with 2
      ;; GiB (native-image OOMs there) and a dedicated 24 GiB builder.
      (write-executable!
        podman
        (str "#!/usr/bin/env bash\n" "conn=\"\"\n"
             "if [ \"${1:-}\" = \"--connection\" ]; then conn=\"$2\"; shift 2; fi\n"
             "case \"${1:-}\" in\n"
             "  --version) printf 'podman version 6.0.0\\n' ;;\n"
             "  info)      if [ \"$conn\" = vis-builder ]; then printf '25769803776\\n';"
             " else printf '2147483648\\n'; fi ;;\n"
             "  system)    printf 'podman-machine-default\\nvis-builder\\n' ;;\n"
             "  *)         exit 0 ;;\n" "esac\n"))
      (try (let
             [{:keys [exit output]} (run-release-native mac
                                                        ["--targets" "linux-x64"]
                                                        {"VIS_CONTAINER_CLI" (.getAbsolutePath
                                                                               podman)
                                                         "VIS_EMULATION_MAX_SECONDS" "-1"})]
             ;; No VIS_CONTAINER_CONNECTION: the builder machine is found and used
             ;; on its own, and the 2 GiB default never decides the run.
             (expect (str/includes? output "vis-builder") output)
             (expect (not (str/includes? output "only has 2 GB")) output)
             ;; Emulation speed still gates the build itself.
             (expect (not= 0 exit) output))
           (finally (delete-tree! mac))))))

(defdescribe
  release-native-podman-export-test
  (it
    "copies the bundle out of a container, because podman-remote rejects --output"
    (let
      [mac
       (fake-tools! "Darwin" "arm64")

       log
       (io/file mac "engine.log")

       podman
       (io/file mac "podman")]

      ;; `podman build -o type=local,dest=…` is a LOCAL-only flag: a Mac client
      ;; drives its Linux VM in REMOTE mode and refuses it AFTER the whole build
      ;; has run. The export therefore goes through create + cp, and the build
      ;; args this host overrides (heap, version sha) must reach the builder.
      (write-executable!
        podman
        (str "#!/usr/bin/env bash\n"
             "printf '%s\\n' \"$*\" >> \"" (.getAbsolutePath log)
             "\"\n" "if [ \"${1:-}\" = \"--connection\" ]; then shift 2; fi\n"
             "case \"${1:-}\" in\n" "  --version) printf 'podman version 6.0.0\\n' ;;\n"
             "  info)      printf '34359738368\\n' ;;\n" "  create)    printf 'ctr123\\n' ;;\n"
             "  *)         exit 0 ;;\n" "esac\n"))
      (try (let
             [{:keys [exit output]}
              (run-release-native mac
                                  ["--targets" "linux-arm64"]
                                  {"VIS_CONTAINER_CLI" (.getAbsolutePath podman)
                                   "VIS_NATIVE_EXTRA_ARGS" "-J-Xmx7g"})

              logged
              (slurp log)]

             ;; The fake engine copies nothing, so staging rejects the bundle —
             ;; the run must still have driven the export the documented way.
             (expect (not= 0 exit) output)
             (expect (str/includes? logged "--target native-export") logged)
             (expect (not (str/includes? logged "type=local")) logged)
             (expect (str/includes? logged "--build-arg VIS_NATIVE_EXTRA_ARGS=-J-Xmx7g") logged)
             (expect (re-find #"--build-arg VIS_BUILD_SHA=[0-9a-f]{7,}" logged) logged)
             (expect (str/includes? logged "create --platform linux/arm64") logged)
             (doseq
               [entry ["vis-agent" "vis-agent-native" "install-vis-agent" "vis-agent-resources"]]
               (expect (str/includes? logged (str "cp ctr123:/" entry " ")) logged)))
           (finally (delete-tree! mac))))))

(defdescribe
  native-build-sha-stamp-test
  (it "lets a container build stamp a real sha instead of \"dev\""
      (let
        [dockerfile
         (slurp "Dockerfile")

         build-clj
         (slurp "build.clj")]

        ;; `.dockerignore` drops `.git`, so build.clj's own `git rev-parse` finds
        ;; no repo inside the image and every container-built asset reported
        ;; `vis-agent dev`. The host's sha arrives as a build arg instead.
        (expect (str/includes? (slurp ".dockerignore") ".git"))
        (expect (str/includes? dockerfile "ARG VIS_BUILD_SHA") dockerfile)
        (expect (str/includes? dockerfile "RUN VIS_BUILD_SHA=\"${VIS_BUILD_SHA}\"") dockerfile)
        (expect (str/includes? build-clj "(System/getenv \"VIS_BUILD_SHA\")") build-clj))))

(defdescribe
  release-native-engine-fallback-test
  (it "steps over an installed engine whose VM is down instead of ending the run"
      (let [mac (fake-tools! "Darwin" "arm64")]
        ;; Exactly this workstation: Docker Desktop installed but NOT running,
        ;; while the podman builder machine is up. `docker` resolves first on
        ;; PATH, so a run that trusts PATH alone dies with nothing built.
        (write-executable!
          (io/file mac "docker")
          (str "#!/usr/bin/env bash\n"
               "case \"${1:-}\" in\n" "  --version) printf 'Docker version 28.0.0\\n' ;;\n"
               "  *)         printf 'Cannot connect to the Docker daemon\\n' >&2; exit 1 ;;\n"
               "esac\n"))
        (write-executable! (io/file mac "podman")
                           (str "#!/usr/bin/env bash\n" "case \"${1:-}\" in\n"
                                "  --version) printf 'podman version 6.0.0\\n' ;;\n"
                                "  info)      printf '25769803776\\n' ;;\n"
                                "  *)         exit 0 ;;\n" "esac\n"))
        (try (let
               [{:keys [exit output]} (run-release-native mac
                                                          ["--targets" "linux-x64"]
                                                          {"VIS_EMULATION_MAX_SECONDS" "-1"})]
               (expect (str/includes? output "using podman instead") output)
               ;; It got PAST engine resolution: the run now fails on the
               ;; emulation guard, not on a dead docker daemon.
               (expect (str/includes? output "qemu-user") output)
               (expect (not= 0 exit) output))
             (finally (delete-tree! mac))))))
