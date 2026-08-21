(ns com.blockether.vis.container-image-test
  "The gateway container runs the NATIVE runtime, and nothing else.

   A deployment is only worth trusting when it serves the artifact users
   install. The image therefore installs the release bundle the `builder` and
   `native-export` stages produce — wrapper, `vis-agent-native` and the
   language-resources sidecar in one directory — so a gap in
   `reachability-metadata.json` fails in the build rather than in production.
   The image's home is the `vis` user's own — HOME=/home/vis, so `~/.vis` is
   /home/vis/.vis and never root's — and the build proves that by running the
   binary and looking at what it wrote. These tests pin that arrangement, and
   the wrapper contract it stands on."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(defn- dockerfile [] (slurp "Dockerfile"))

(defn- runtime-stage
  "The Dockerfile from the runtime stage header down: the only stage the gateway
   image is built from."
  []
  (let [text
        (dockerfile)

        at
        (str/index-of text "AS runtime")]

    (expect (some? at) "Dockerfile still declares a runtime stage")
    (subs text at)))

(defn- run-wrapper
  "Runs `bin/vis-agent` from `dir` (a symlink to it) with `env-extra`; merged output."
  [^java.io.File launcher env-extra args]
  (let [pb (ProcessBuilder. ^java.util.List (into [(.getAbsolutePath launcher)] args))]
    (.redirectErrorStream pb true)
    (doseq [[k v] env-extra]
      (.put (.environment pb) (str k) (str v)))
    (let [process (.start pb)
          output (slurp (.getInputStream process))]

      {:exit (.waitFor process) :output output})))

(defn- run-command!
  "Run a setup command in `dir`, throwing with merged output when it fails."
  [^java.io.File dir args]
  (let [pb
        (doto (ProcessBuilder. ^java.util.List (mapv str args))
          (.directory dir)
          (.redirectErrorStream true))

        process
        (.start pb)

        output
        (slurp (.getInputStream process))

        exit
        (.waitFor process)]

    (when-not (zero? exit)
      (throw (ex-info "Test setup command failed" {:args args :exit exit :output output})))
    output))

(defdescribe
  container-runs-the-native-runtime-test
  (it "installs the release bundle and links the wrapper at it"
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "COPY --from=native-export --chown=vis:vis / /opt/vis/agent/")
                stage)
        (expect (str/includes? stage "ln -sf /opt/vis/agent/vis-agent /usr/local/bin/vis-agent")
                stage)
        ;; No runtime selector: the wrapper finds `vis-agent-native` beside
        ;; itself, so the image never has to name a runtime.
        (expect (not (str/includes? stage "VIS_RUNTIME")) stage)))
  (it "ships no Vis source for the wrapper to run instead"
      ;; Two runtimes in one image is two answers to the question of what is
      (let [stage (runtime-stage)]
        (expect (not (str/includes? stage "/opt/vis/src")) stage)
        (expect (not (str/includes? stage "clojure -P -M:vis")) stage)))
  (it "serves the binary this source builds, not a downloaded one"
      (let [text (dockerfile)]
        (expect (str/includes? text "FROM jdk AS builder") text)
        (expect (str/includes? text "FROM scratch AS native-export") text)
        (expect (str/includes? text "COPY --from=builder /build/target/vis /vis-agent-native")
                text)))
  (it "proves the runtime while building, not in production"
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "vis-agent runtime | grep -Eq '^Runtime: +native'") stage)
        (expect (str/includes? stage "test -x /opt/vis/agent/vis-agent-native") stage)
        ;; The GraalPy/Truffle resources ship BESIDE the binary; without them
        ;; every Python tool dies with "No module named 'ast'".
        (expect (str/includes? stage "test -d /opt/vis/agent/vis-agent-resources") stage)
        (expect (str/includes? stage "vis-agent python -c") stage)))
  (it "gives the agent the vis user's home, and proves the runtime uses it"
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "ENV HOME=/home/vis") stage)
        (expect (str/includes? stage "VIS_HOME=/home/vis/.vis") stage)
        (expect (str/includes? stage "useradd --create-home --shell /bin/bash --uid 10001 vis")
                stage)
        (expect (str/includes? stage "chown -R vis:vis /home/vis /work") stage)
        ;; Existence of ~/.vis proves only that the image mkdir'd it. `logs` is
        ;; written by `config/init-cli!` on every command, so the assertion below
        ;; is the RUNTIME saying where its home is — and /root/.vis staying
        ;; absent is the same statement from the other side.
        (expect (str/includes? stage "test ! -e /root/.vis") stage)
        (expect (str/includes? stage "test -d /home/vis/.vis/logs") stage))))

(defdescribe
  image-is-a-base-a-deployment-extends-test
  ;; The image carries vis and the toolchain VIS drives. An operator's own CLI
  ;; is a layer in the deployment's own repository: baked in here, every user of
  ;; vis pays download time and attack surface for one operator's habits.
  (it "installs no operator-specific CLI"
      (let [stage (runtime-stage)]
        (expect (not (str/includes? stage "cli.github.com")) stage)
        (expect (nil? (re-find #"install[^\n]*\bgh\b" stage)) stage)
        (expect (not (str/includes? stage "/home/vis/.config/gh")) stage)))
  (it "documents the recipe and the contract a derived image builds on"
      (let [text (dockerfile)]
        (expect (str/includes? text "EXTENDING THIS IMAGE") text)
        (expect (str/includes? text "FROM vis-gateway:local") text)
        (expect (str/includes? text "USER root") text)))
  (it "leaves a derived image a seeded, vis-owned .config to add to"
      ;; docker seeds a named volume from the image's directory and inherits its
      ;; owner: root-owned .config is a credential directory nothing can write.
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "/home/vis/.config/git") stage)
        (expect (str/includes? stage "test \"$(stat -c '%U' /home/vis/.config)\" = 'vis'") stage))))


(defdescribe compose-passes-no-second-version-source-test
             (it "leaves the version to the image it builds"
                 (let [compose (slurp "docker-compose.yml")]
                   ;; A build-arg version would be a SECOND version source; the only one
                   ;; is the repo-root VIS_VERSION, baked into the binary.
                   (expect (not (str/includes? compose "VIS_BUILD_SHA")) compose))))

(defdescribe
  wrapper-runs-the-bundled-native-test
  (it "runs the runtime beside it when linked from elsewhere"
      ;; Exactly the image's arrangement: /opt/vis/agent holds the wrapper and
      ;; its `vis-agent-native`, /usr/local/bin/vis-agent is a symlink into that
      ;; directory, and nothing at all says which runtime: the one beside the
      ;; wrapper is the one that runs.
      (let [tmp
            (.toFile (Files/createTempDirectory "vis-native-launcher" (make-array FileAttribute 0)))

            bundle
            (io/file tmp "agent")

            wrapper
            (io/file bundle "vis-agent")

            native
            (io/file bundle "vis-agent-native")

            link
            (io/file tmp "vis-agent")]

        (try (.mkdirs bundle)
             (io/copy (io/file "bin" "vis-agent") wrapper)
             (.setExecutable wrapper true false)
             (spit native "#!/bin/sh\nexit 0\n")
             (.setExecutable native true false)
             (Files/createSymbolicLink (.toPath link)
                                       ^Path (.toPath wrapper)
                                       (make-array FileAttribute 0))
             (let [{:keys [exit output]}
                   (run-wrapper link {"HOME" (.getAbsolutePath tmp)} ["runtime"])]
               (expect (zero? exit) output)
               (expect (re-find #"(?m)^Runtime: +native$" output) output)
               (expect (re-find (re-pattern (str "(?m)^Native: +"
                                                 (java.util.regex.Pattern/quote (.getCanonicalPath
                                                                                  native))
                                                 "$"))
                                output)
                       output))
             (finally (doseq [file (reverse (file-seq tmp))]
                        (io/delete-file file true)))))))

(defdescribe
  wrapper-runs-the-checkout-it-sits-in-test
  (it "runs the checkout it sits in, and never a binary built there"
      ;; A wrapper with a `deps.edn` one level up IS a checkout invocation: with
      ;; nothing installed under VIS_HOME, running that tree's own live source is
      ;; the only thing it can mean. What the tree BUILT is still not a runtime —
      ;; a `target/vis` of its own never stands in for an installed release.
      (let [tmp
            (.toFile (Files/createTempDirectory "vis-checkout-launcher"
                                                (make-array FileAttribute 0)))

            checkout
            (io/file tmp "checkout")

            wrapper
            (io/file checkout "bin" "vis-agent")

            native
            (io/file checkout "target" "vis")]

        (try (.mkdirs (io/file checkout "bin"))
             (.mkdirs (io/file checkout "target"))
             (spit (io/file checkout "deps.edn") "{}\n")
             (io/copy (io/file "bin" "vis-agent") wrapper)
             (.setExecutable wrapper true false)
             (spit native "#!/bin/sh\nexit 0\n")
             (.setExecutable native true false)
             (let [{:keys [exit output]} (run-wrapper wrapper
                                                      {"HOME" (.getAbsolutePath tmp)
                                                       "VIS_HOME" (.getAbsolutePath
                                                                    (io/file tmp "state"))}
                                                      ["runtime"])]
               (expect (zero? exit) output)
               (expect (re-find #"(?m)^Runtime: +jvm$" output) output)
               (expect (re-find #"(?m)^Native: +not installed$" output) output)
               (expect (re-find (re-pattern (str "(?m)^Source: +"
                                                 (java.util.regex.Pattern/quote (.getCanonicalPath
                                                                                  checkout))
                                                 "$"))
                                output)
                       output))
             (finally (doseq [file (reverse (file-seq tmp))]
                        (io/delete-file file true)))))))

(defdescribe
  wrapper-system-trust-test
  (it
    "passes a discovered system trust store and CA bundle to the native runtime"
    (let [tmp
          (.toFile (Files/createTempDirectory "vis-native-system-trust"
                                              (make-array FileAttribute 0)))

          bundle
          (doto (io/file tmp "agent") .mkdirs)

          wrapper
          (io/file bundle "vis-agent")

          native
          (io/file bundle "vis-agent-native")

          trust-store
          (doto (io/file tmp "system-cacerts") (spit "store"))

          ca-bundle
          (doto (io/file tmp "system-ca.pem") (spit "certificate"))]

      (try
        (io/copy (io/file "bin" "vis-agent") wrapper)
        (.setExecutable wrapper true false)
        (spit native
              (str "#!/bin/sh\n" "printf 'ARGS=%s\n' \"$*\"\n"
                   "printf 'JAVA_TOOL_OPTIONS=%s\n' \"${JAVA_TOOL_OPTIONS:-}\"\n"
                   "printf 'SSL_CERT_FILE=%s\n' \"${SSL_CERT_FILE:-}\"\n"
                   "printf 'REQUESTS_CA_BUNDLE=%s\n' \"${REQUESTS_CA_BUNDLE:-}\"\n"
                   "printf 'NODE_EXTRA_CA_CERTS=%s\n' \"${NODE_EXTRA_CA_CERTS:-}\"\n"))
        (.setExecutable native true false)
        (let [{:keys [exit output]} (run-wrapper wrapper
                                                 {"HOME" (.getAbsolutePath tmp)
                                                  "JAVA_TOOL_OPTIONS" ""
                                                  "VIS_SYSTEM_TRUSTSTORE" (.getAbsolutePath
                                                                            trust-store)
                                                  "VIS_SYSTEM_TRUSTSTORE_TYPE" "JKS"
                                                  "VIS_SYSTEM_CA_CERT" (.getAbsolutePath ca-bundle)}
                                                 ["help"])]
          (expect (zero? exit) output)
          (expect (str/includes? output
                                 (str "-Djavax.net.ssl.trustStore=" (.getAbsolutePath trust-store)))
                  output)
          (expect (str/includes? output "-Djavax.net.ssl.trustStoreType=JKS") output)
          (expect (str/includes? output (str "SSL_CERT_FILE=" (.getAbsolutePath ca-bundle))) output)
          (expect (str/includes? output (str "REQUESTS_CA_BUNDLE=" (.getAbsolutePath ca-bundle)))
                  output)
          (expect (str/includes? output (str "NODE_EXTRA_CA_CERTS=" (.getAbsolutePath ca-bundle)))
                  output))
        (finally (doseq [file (reverse (file-seq tmp))]
                   (io/delete-file file true))))))
  (it
    "exports the same trust to the JVM source runtime"
    (let [tmp
          (.toFile (Files/createTempDirectory "vis-jvm-system-trust" (make-array FileAttribute 0)))

          checkout
          (doto (io/file tmp "checkout") .mkdirs)

          bin-dir
          (doto (io/file checkout "bin") .mkdirs)

          fake-path
          (doto (io/file tmp "path") .mkdirs)

          wrapper
          (io/file bin-dir "vis-agent")

          trust-store
          (doto (io/file tmp "system-cacerts") (spit "store"))

          ca-bundle
          (doto (io/file tmp "system-ca.pem") (spit "certificate"))]

      (try (spit (io/file checkout "deps.edn") "{}\n")
           (io/copy (io/file "bin" "vis-agent") wrapper)
           (.setExecutable wrapper true false)
           (doseq [[name body] [["java" "#!/bin/sh\necho '    java.vendor.version = OpenJDK' >&2\n"]
                                ["clojure"
                                 (str "#!/bin/sh\n"
                                      "printf 'JAVA_TOOL_OPTIONS=%s\n' \"${JAVA_TOOL_OPTIONS:-}\"\n"
                                      "printf 'SSL_CERT_FILE=%s\n' \"${SSL_CERT_FILE:-}\"\n")]]]
             (let [file (io/file fake-path name)]
               (spit file body)
               (.setExecutable file true false)))
           (let [{:keys [exit output]}
                 (run-wrapper wrapper
                              {"HOME" (.getAbsolutePath tmp)
                               "VIS_HOME" (.getAbsolutePath (io/file tmp "state"))
                               "VIS_JVM" "1"
                               "VIS_NO_AUTO_INSTALL" "1"
                               "PATH" (str (.getAbsolutePath fake-path) ":" (System/getenv "PATH"))
                               "JAVA_TOOL_OPTIONS" ""
                               "VIS_SYSTEM_TRUSTSTORE" (.getAbsolutePath trust-store)
                               "VIS_SYSTEM_TRUSTSTORE_TYPE" "JKS"
                               "VIS_SYSTEM_CA_CERT" (.getAbsolutePath ca-bundle)}
                              ["help"])]
             (expect (zero? exit) output)
             (expect (str/includes? output
                                    (str "-Djavax.net.ssl.trustStore="
                                         (.getAbsolutePath trust-store)))
                     output)
             (expect (str/includes? output "-Djavax.net.ssl.trustStoreType=JKS") output)
             (expect (str/includes? output (str "SSL_CERT_FILE=" (.getAbsolutePath ca-bundle)))
                     output))
           (finally (doseq [file (reverse (file-seq tmp))]
                      (io/delete-file file true)))))))


(defdescribe
  wrapper-wsl-system-trust-test
  (it
    "exports Windows roots into WSL for both Vis and child tools"
    (let [tmp
          (.toFile (Files/createTempDirectory "vis-wsl-system-trust" (make-array FileAttribute 0)))

          bundle
          (doto (io/file tmp "agent") .mkdirs)

          fake-path
          (doto (io/file tmp "path") .mkdirs)

          state
          (io/file tmp "state")

          wrapper
          (io/file bundle "vis-agent")

          native
          (io/file bundle "vis-agent-native")

          powershell
          (io/file fake-path "powershell.exe")

          ubuntu-pem
          "-----BEGIN CERTIFICATE-----\nVUJVTlRVLVJPT1Q=\n-----END CERTIFICATE-----\n"

          windows-pem
          "-----BEGIN CERTIFICATE-----\nV0lORE9XUy1ST09U\n-----END CERTIFICATE-----\n"

          ubuntu-bundle
          (doto (io/file tmp "ubuntu-ca.pem") (spit ubuntu-pem))]

      (try (io/copy (io/file "bin" "vis-agent") wrapper)
           (.setExecutable wrapper true false)
           (spit native
                 (str "#!/bin/sh\n"
                      "printf 'SSL_CERT_FILE=%s\\n' \"${SSL_CERT_FILE:-}\"\n"
                      "printf 'VIS_SYSTEM_CA_CERT=%s\\n' \"${VIS_SYSTEM_CA_CERT:-}\"\n"))
           (.setExecutable native true false)
           (spit powershell
                 (str "#!/bin/sh\n"
                      "case \"$*\" in *'::new('*|*'OutputEncoding'*) exit 1 ;; esac\n"
                      "printf '%s' '"
                      windows-pem
                      "'\n"))
           (.setExecutable powershell true false)
           (let [{:keys [exit output]}
                 (run-wrapper wrapper
                              {"HOME" (.getAbsolutePath tmp)
                               "VIS_HOME" (.getAbsolutePath state)
                               "WSL_DISTRO_NAME" "Ubuntu"
                               "VIS_SYSTEM_CA_CERT" ""
                               "SSL_CERT_FILE" (.getAbsolutePath ubuntu-bundle)
                               "PATH" (str (.getAbsolutePath fake-path) ":" (System/getenv "PATH"))}
                              ["help"])

                 exported
                 (io/file state "trust" "wsl-system-ca.pem")]

             (expect (zero? exit) output)
             (expect (= (str ubuntu-pem "\n" windows-pem) (slurp exported)))
             (expect (str/includes? output (str "SSL_CERT_FILE=" (.getAbsolutePath exported)))
                     output)
             (expect (str/includes? output (str "VIS_SYSTEM_CA_CERT=" (.getAbsolutePath exported)))
                     output))
           (finally (doseq [file (reverse (file-seq tmp))]
                      (io/delete-file file true)))))))

(defdescribe
  wrapper-self-update-test
  (it
    "replaces an installed Bash wrapper from the source commit it pins"
    (let [tmp
          (.toFile (Files/createTempDirectory "vis-wrapper-self-update"
                                              (make-array FileAttribute 0)))

          remote
          (doto (io/file tmp "remote") .mkdirs)

          remote-bin
          (doto (io/file remote "bin") .mkdirs)

          installed-bin
          (doto (io/file tmp "installed-bin") .mkdirs)

          installed
          (io/file installed-bin "vis-agent")

          replacement
          "#!/usr/bin/env bash
printf 'updated wrapper\n'
"

          state
          (io/file tmp "state")]

      (try (spit (io/file remote "deps.edn") "{}
")
           (spit (io/file remote-bin "vis-agent") replacement)
           (.setExecutable (io/file remote-bin "vis-agent") true false)
           (run-command! remote ["git" "init" "--quiet" "--initial-branch=main"])
           (run-command! remote ["git" "add" "deps.edn" "bin/vis-agent"])
           (run-command! remote
                         ["git" "-c" "user.name=Vis Test" "-c" "user.email=vis@example.com" "commit"
                          "--quiet" "-m" "test source"])
           (io/copy (io/file "bin" "vis-agent") installed)
           (.setExecutable installed true false)
           (let [{:keys [exit output]} (run-wrapper installed
                                                    {"HOME" (.getAbsolutePath tmp)
                                                     "VIS_HOME" (.getAbsolutePath state)
                                                     "VIS_REPO_SLUG" "local/vis"
                                                     "VIS_REPO_URL" (.getAbsolutePath remote)}
                                                    ["update" "--keep-gateway"])]
             (expect (zero? exit) output)
             (expect (= replacement (slurp installed)))
             (expect (str/includes? output "vis-agent command updated from") output))
           (finally (doseq [file (reverse (file-seq tmp))]
                      (io/delete-file file true)))))))
