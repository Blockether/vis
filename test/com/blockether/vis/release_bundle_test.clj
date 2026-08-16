(ns com.blockether.vis.release-bundle-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.protocol :as protocol]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.net URL URLClassLoader]
           [java.nio.file Files]
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

       stamp
       "0.1.28 4c1f2a9dabcdef0123456789abcdef01234567 beta 2026-08-17T10:22:31.123Z\n"

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
           ;; Regression, issue #148: a bundle carried no record of the commit its
           ;; runtime was built from, so a months-old binary beside fresh source
           ;; passed for current and its long-fixed crash was reported again.
           (let [{:keys [exit output]} (stage!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "vis-agent-native.build") output)
             (expect (not (.isFile asset)) "no asset may survive a rejected bundle"))
           (spit (io/file from-dir "vis-agent-native.build") stamp)
           (let [{:keys [exit output]} (stage!)]
             (expect (= 0 exit) output)
             (expect (.isFile asset) output)
             (expect (= "stdlib\n"
                        (slurp (io/file bundle-dir "vis-agent-resources/python/marker"))))
             (expect (= stamp (slurp (io/file bundle-dir "vis-agent-native.build"))))
             (doseq [entry ["vis-agent" "vis-agent-native" "install-vis-agent"]]
               (expect (.canExecute (io/file bundle-dir entry)) entry)))
           (finally (delete-tree! root))))))

;; Regression, issue #148: `vis-agent runtime` printed the SOURCE pin beside a
;; native runtime built from an entirely different commit, so a binary from
;; before a fix looked like the pinned one and its crash was filed all over
;; again. Nothing recorded which commit an installed runtime came from.
(defdescribe
  native-build-stamp-test
  (it
    "dates the installed runtime from its stamp and calls out one older than the pin"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-build-stamp-test-" (make-array FileAttribute 0)))

       home
       (doto (io/file root "home") .mkdirs)

       bin-dir
       (doto (io/file root "bin") .mkdirs)

       launcher
       (io/file bin-dir "vis-agent")

       runtime!
       (fn []
         (run-bash ["bash" (.getAbsolutePath launcher) "runtime"]
                   {"HOME" (.getAbsolutePath home)}))]

      (try (io/copy (io/file "bin/vis-agent") launcher)
           (.setExecutable ^java.io.File launcher true)
           (write-executable! (io/file bin-dir "vis-agent-native") "#!/usr/bin/env bash\nexit 0\n")
           ;; No stamp means a runtime built before stamps existed — say so instead
           ;; of letting it pass for current.
           (let [{:keys [exit output]} (runtime!)]
             (expect (= 0 exit) output)
             (expect (str/includes? output "predates build stamps") output))
           (spit (io/file bin-dir "vis-agent-native.build")
                 "0.1.28 4c1f2a9dabc beta 2026-08-17T10:22:31.123Z\n")
           (let [{:keys [exit output]} (runtime!)]
             (expect (= 0 exit) output)
             (expect (str/includes? output "Built:        0.1.28 4c1f2a9dabc beta") output)
             (expect (not (str/includes? output "STALE")) output))
           ;; The reported situation: source pinned at one commit, native built from
           ;; another. The stamp is read as a file — the binary is never run, because
           ;; the one whose provenance matters is the one that aborts on every call.
           (let
             [src
              (doto (io/file home ".vis" "install" "src") .mkdirs)

              _
              (run-bash ["bash" "-c"
                         (str
                           "cd "
                           (.getAbsolutePath src)
                           " && git init -q"
                           " && printf '{}' > deps.edn && git add -A"
                           " && git -c user.email=ci@example.com -c user.name=ci commit -qm init")]
                        {})

              head-sha
              (str/trim (:output (run-bash ["git" "-C" (.getAbsolutePath src) "rev-parse" "HEAD"]
                                           {})))]

             (spit (io/file home ".vis" "install" "ref") (str head-sha "\n"))
             (let [{:keys [exit output]} (runtime!)]
               (expect (= 0 exit) output)
               (expect (str/includes? output "STALE") output)
               (expect (str/includes? output head-sha) output))
             (spit (io/file bin-dir "vis-agent-native.build")
                   (str "0.1.28 " head-sha " stable 2026-08-17T10:22:31.123Z\n"))
             (let [{:keys [exit output]} (runtime!)]
               (expect (= 0 exit) output)
               (expect (not (str/includes? output "STALE")) output)))
           (finally (delete-tree! root)))))
  (it "writes that stamp from the build — into the image and beside the binary"
      (let
        [build-clj
         (slurp "build.clj")

         stage
         (slurp "bin/stage-release-bundle")]

        (expect (str/includes? build-clj "(spit (str native-bin \".build\")") build-clj)
        (expect (str/includes? build-clj "\"-H:IncludeResources=vis/BUILD\"") build-clj)
        ;; Provenance, never a version: `--version` still reports VIS_VERSION alone.
        (expect (str/includes? build-clj "(spit vfile version)") build-clj)
        (expect (str/includes? stage "vis-agent-native.build") stage))))
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
      ;; args this host overrides (heap) must reach the builder.
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
             (expect (str/includes? logged "create --platform linux/arm64") logged)
             (doseq
               [entry ["vis-agent" "vis-agent-native" "install-vis-agent" "vis-agent-resources"]]
               (expect (str/includes? logged (str "cp ctr123:/" entry " ")) logged)))
           (finally (delete-tree! mac))))))

;; Regression: container-built assets reported `vis-agent <git-sha>`, then
;; `vis-agent <VIS_VERSION>+<git-sha>`, so a deployed gateway never simply said
;; which VIS_VERSION it was running. VIS_VERSION is the only version there is.
(defdescribe
  version-stamp-test
  (it
    "stamps the repo-root VIS_VERSION into `vis/VERSION`, verbatim"
    (let
      [dockerfile
       (slurp "Dockerfile")

       build-clj
       (slurp "build.clj")

       compose
       (slurp "docker-compose.yml")

       release-native
       (slurp "bin/release-native")

       declared
       (str/trim (slurp "VIS_VERSION"))]

      (expect (re-matches #"\d+\.\d+\.\d+" declared) declared)
      ;; VIS_VERSION is the ONLY version source: build.clj's `version` IS that
      ;; file and the native build spits it unchanged.
      (expect (str/includes? build-clj "(str/trim (slurp \"VIS_VERSION\"))") build-clj)
      (expect (str/includes? build-clj "(spit vfile version)") build-clj)
      ;; no second version source may creep back in through a build arg, an env
      ;; override or a snapshot suffix
      (doseq
        [[what source] {"build.clj" build-clj
                        "Dockerfile" dockerfile
                        "docker-compose.yml" compose
                        "bin/release-native" release-native}]
        (expect (not (str/includes? source "VIS_BUILD_SHA")) what)
        (expect (not (str/includes? source "-SNAPSHOT")) what)
        (expect (not (str/includes? source "rev-parse --short HEAD")) what))
      ;; the native stage refuses to ship an image whose --version is anything
      ;; other than the declared VIS_VERSION
      (expect (str/includes? dockerfile "= \"vis-agent$(tr -d '[:space:]' < VIS_VERSION)\"")
              dockerfile)
      ;; the runtime image installs that very binary, so it needs no second
      ;; stamping step of its own
      (expect (str/includes? dockerfile "COPY --from=native-export") dockerfile)
      (expect (not (str/includes? dockerfile "/opt/vis/src/resources/vis/VERSION")) dockerfile)))
  (it "reports the stamped string verbatim from the classpath resource"
      (let
        [dir
         (.toFile (Files/createTempDirectory "vis-version" (make-array FileAttribute 0)))

         stamped
         (str/trim (slurp "VIS_VERSION"))

         thread
         (Thread/currentThread)

         prior
         (.getContextClassLoader thread)]

        (spit (doto (io/file dir "vis" "VERSION") io/make-parents) (str stamped "\n"))
        (try (.setContextClassLoader
               thread
               (URLClassLoader. (into-array URL [(.toURL (.toURI ^java.io.File dir))]) prior))
             ;; what `/healthz` and `/v1/capabilities` advertise
             (expect (= stamped (protocol/release-version)))
             (finally (.setContextClassLoader thread prior) (delete-tree! dir))))))

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

;; The distribution TRACK: `stable` (release tags) or `beta` (the rolling
;; per-commit prerelease). It is deliberately not called a channel — a channel
;; in Vis is a user interface an extension registers (TUI, web, Telegram) — and
;; the two must never share a word in build, wrapper or workflow.
(defdescribe
  distribution-track-test
  (it
    "remembers the track, and never moves it without being told to"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-track-test-" (make-array FileAttribute 0)))

       home
       (doto (io/file root "home") .mkdirs)

       bin-dir
       (doto (io/file root "bin") .mkdirs)

       path-dir
       (doto (io/file root "path") .mkdirs)

       launcher
       (io/file bin-dir "vis-agent")

       track-file
       (io/file home ".vis" "install" "track")

       update!
       (fn [& args]
         (run-bash (into ["bash" (.getAbsolutePath launcher) "update"] args)
                   {"HOME" (.getAbsolutePath home)
                    "PATH" (str (.getAbsolutePath path-dir) ":" (System/getenv "PATH"))}))]

      (try (io/copy (io/file "bin/vis-agent") launcher)
           (.setExecutable ^java.io.File launcher true)
           (write-executable! (io/file bin-dir "vis-agent-native") "#!/usr/bin/env bash\nexit 0\n")
           ;; A curl that resolves nothing stops the update at the endpoint it
           ;; CHOSE — which is the whole question here. No network, no download.
           (write-executable! (io/file path-dir "curl") "#!/usr/bin/env bash\nexit 22\n")
           (let [{:keys [exit output]} (update! "--track" "beta")]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "releases/tags/beta") output))
           (expect (= "beta\n" (slurp track-file)))
           ;; The next plain `update` must stay on beta: a tester silently
           ;; dropped back to stable files bugs against a build never running.
           (let [{:keys [exit output]} (update!)]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "releases/tags/beta") output))
           ;; Naming a version is a one-off, not a track switch.
           (let [{:keys [exit output]} (update! "v9.9.9")]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "releases/tags/v9.9.9") output))
           (expect (= "beta\n" (slurp track-file)))
           (let [{:keys [exit output]} (update! "--track" "stable")]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "releases/latest") output))
           (expect (= "stable\n" (slurp track-file)))
           ;; The beta track has one moving tag and no versions at all.
           (let [{:keys [exit output]} (update! "--track" "beta" "v1.2.3")]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "has no versions") output))
           (let [{:keys [exit output]} (update! "--track" "nightly")]
             (expect (not= 0 exit) output)
             (expect (str/includes? output "unknown track") output))
           ;; `runtime` reports the followed track, and says when the installed
           ;; binary was built for a different one.
           (spit (io/file bin-dir "vis-agent-native.build")
                 "0.1.28 4c1f2a9dabc beta 2026-08-17T10:22:31.123Z\n")
           (let
             [{:keys [exit output]} (run-bash ["bash" (.getAbsolutePath launcher) "runtime"]
                                              {"HOME" (.getAbsolutePath home)})]
             (expect (= 0 exit) output)
             (expect (str/includes? output "Track:        stable") output)
             (expect (str/includes? output "built on the beta track") output))
           (finally (delete-tree! root)))))
  ;; A hosted Apple-silicon fallback is only useful if it can FINISH: the free
  ;; macOS arm64 class is 3 cores / 7 GiB RAM / 14 GiB disk, so a heap above
  ;; physical RAM has no volume to swap into and native-image exits on the
  ;; first OutOfMemoryError. The fallback shrinks the build, not the promise.
  (it "sizes the hosted macOS fallback to fit the free runner instead of swapping"
      (let
        [stable
         (slurp ".github/workflows/native-release.yml")

         fallback
         (->> (str/split-lines stable)
              (drop-while #(not (str/includes? % "-lt 16")))
              (take 8)
              (str/join "\n"))]

        (expect (str/includes? fallback "--parallelism=2") fallback)
        (expect (str/includes? fallback "-J-Xmx5g") fallback)
        (expect (nil? (re-find #"-J-Xmx(?:[89]|1[0-9])g" fallback)) fallback)
        ;; Quick build produces a slower binary, so it is for dry runs only.
        (expect (str/includes? fallback "-Ob") fallback)
        (expect (str/includes? fallback "steps.target.outputs.publish") fallback)
        ;; One dispatch input moves the job to a bigger CLOUD label for a
        ;; single run, without touching the repository variable.
        (expect (str/includes? stable "inputs.runner || vars.VIS_MACOS_ARM64_RUNNER") stable)))
  ;; NO RELEASE MAY DEPEND ON A LAPTOP BEING AWAKE. A job whose label matches no
  ;; online runner queues for 24 hours and then fails, so a self-hosted default
  ;; shipped tags with no macOS asset and no error until the next day.
  (it "runs every workflow on hosted runners, never a self-hosted label"
      (doseq
        [workflow (->> (file-seq (io/file ".github/workflows"))
                       (filter #(str/ends-with? (.getName ^java.io.File %) ".yml")))]
        (let
          [directives (->> (str/split-lines (slurp workflow))
                           (remove #(str/starts-with? (str/triml %) "#"))
                           (str/join "\n")
                           str/lower-case)]
          (expect (not (str/includes? directives "self-hosted")) (.getPath ^java.io.File workflow))
          (expect (not (str/includes? directives "vis-macos-arm64"))
                  (.getPath ^java.io.File workflow))))
      ;; And the macOS default is the free hosted Apple-silicon class.
      (expect (str/includes? (slurp ".github/workflows/native-release.yml")
                             "vars.VIS_MACOS_ARM64_RUNNER || 'macos-26'")))
  ;; The tuning history in native-release.yml records runs labelled "no extra
  ;; args" that still carried build.clj's computed `-J-Xmx`/`-J-Xms` pair, so
  ;; native-image's OWN sizing has never actually been measured for this image.
  ;; One dispatch must be able to try it, on one platform, without spending two
  ;; hours of Linux builds to watch a macOS experiment.
  (it "can measure native-image's own configuration from one dispatch"
      (let
        [stable
         (slurp ".github/workflows/native-release.yml")

         build
         (slurp "build.clj")]

        (doseq [input ["only:" "native_args:" "builder_heap:"]]
          (expect (str/includes? stable input) input))
        ;; Either job runs alone.
        (expect (str/includes? stable "if: inputs.only != 'macos'") stable)
        (expect (str/includes? stable "if: inputs.only != 'linux'") stable)
        ;; A dispatched argument set wins over the automatic hosted fallback,
        ;; which would otherwise silently put the measured heap back.
        (expect (str/includes? stable "[ -z \"$args\" ] && [ -z \"$heap\" ] && [ \"$gib\" -lt 16 ]")
                stable)
        ;; And two experiments on one branch do not serialize behind each other.
        (expect (str/includes? stable "group: native-release-${{ github.ref }}-${{ inputs.only }}")
                stable)
        ;; `natural` means NEITHER -J flag: an explicit ceiling is exactly what
        ;; the measurement is trying to remove.
        (expect (str/includes? build "(System/getenv \"VIS_NATIVE_BUILDER_HEAP\")") build)
        (expect (re-find #"\(not natural-heap\?\)\s+\(conj \(str \"-J-Xmx\"" build) build)))
  (it
    "builds the beta track on free runners only, off a commit CI already passed"
    (let
      [beta
       (slurp ".github/workflows/beta-native.yml")

       stable
       (slurp ".github/workflows/native-release.yml")

       directives
       (->> (str/split-lines beta)
            (remove #(str/starts-with? (str/triml %) "#"))
            (str/join "\n")
            str/lower-case)]

      ;; A beta must never take the workstation-class Apple-silicon builder the
      ;; stable macOS asset needs: a build pins every core and ~15 GiB, and one
      ;; every few hours takes the machine away from the person using it.
      (expect (not (str/includes? directives "macos-")) directives)
      (expect (not (str/includes? directives "self-hosted")) directives)
      (expect (not (str/includes? directives "vis_macos_arm64_runner")) directives)
      (doseq [runner ["ubuntu-latest" "ubuntu-24.04-arm"]]
        (expect (str/includes? beta runner) runner))
      ;; Coalesced, gated on a green CI run for that exact commit, and stamped.
      (expect (str/includes? beta "cron:") beta)
      (expect (str/includes? beta "actions/workflows/ci.yml/runs?head_sha=") beta)
      (expect (str/includes? beta "VIS_RELEASE_TRACK: beta") beta)
      (expect (str/includes? (slurp "build.clj") "(System/getenv \"VIS_RELEASE_TRACK\")")
              "build.clj")
      (expect (str/includes? beta "prerelease: true") beta)
      ;; The rolling tag is not a v* tag, or every beta would start a stable
      ;; native release too.
      (expect (str/includes? beta "VIS_BETA_TAG: beta") beta)
      (expect (str/includes? stable "tags: ['v[0-9]*']") stable)
      (expect (str/includes? stable "VIS_RELEASE_TRACK:") stable)
      ;; One word, one axis: `channel` belongs to the TUI/web/Telegram adapters.
      (doseq
        [[what source] {"build.clj" (slurp "build.clj")
                        "bin/vis-agent" (slurp "bin/vis-agent")
                        "beta-native.yml" beta
                        "native-release.yml" stable}]
        (expect (not (str/includes? source "VIS_CHANNEL")) what)))))
