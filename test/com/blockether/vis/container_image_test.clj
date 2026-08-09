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
  (let
    [text
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
    (let
      [process (.start pb)
       output (slurp (.getInputStream process))]

      {:exit (.waitFor process) :output output})))

(defdescribe
  container-runs-the-native-runtime-test
  (it "installs the release bundle and links the wrapper at it"
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "COPY --from=native-export --chown=vis:vis / /opt/vis/agent/")
                stage)
        (expect (str/includes? stage "ln -sf /opt/vis/agent/vis-agent /usr/local/bin/vis-agent")
                stage)
        (expect (str/includes? stage "VIS_RUNTIME=native") stage)))
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

(defdescribe
  the-build-runs-the-binary-it-shipped-test
  ;; Everything above proves the image ASSEMBLED. These pin the checks that
  ;; prove it RUNS — the three paths native-image is most likely to have broken,
  ;; and the three no unit test can reach, because they exist only once the
  ;; binary is linked.
  (it "starts the TUI on a pty and fails unless it is still painting"
      ;; Lanterna reaches the terminal through JNI and reflection: a missing
      ;; reachability entry compiles fine and dies on the first frame. A build
      ;; has no pty, so `script` lends one; exit 124 (timeout killed a live
      ;; process) is the proof, and any other status is a TUI that died.
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "script -qec 'timeout 25 vis-agent channels tui' /dev/null")
                stage)
        (expect (str/includes? stage "test \"$tui_rc\" -eq 124") stage)
        (expect (str/includes? stage "TERM=xterm-256color") stage)))
  (it "runs the one-shot agent and lets it stop only for a missing credential"
      ;; The entrypoint boots the session store, the tool registry and provider
      ;; selection. An image with no credential must reach exactly one refusal
      ;; and name it; a native binary that broke earlier never gets that far.
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "vis-agent --db :memory --raw 'hello world'") stage)
        (expect (str/includes? stage "grep -q 'needs an AI provider' /tmp/agent.log") stage)
        (expect (str/includes? stage "test \"$agent_rc\" -ne 0") stage)))
  (it "requires the zai-coding-plan provider to be compiled into the binary"
      ;; A deployment configures that provider from OUTSIDE the image — a key in
      ;; the environment, a `providers:` entry in ~/.vis/config.yml — which can
      ;; only work if the extension is inside it. Here the absence is cheap.
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "vis-agent providers list | grep -q 'zai-coding-plan'")
                stage)))
  (it "runs those proofs on a throwaway home and clears it"
      ;; A TUI and an agent run write logs, drafts and a gateway registry entry.
      ;; Baking them into /home/vis would ship one build's leftovers to every
      ;; container started from the image.
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "proof_home=/tmp/vis-proof") stage)
        (expect (str/includes? stage "HOME=\"$proof_home\"") stage)
        (expect (str/includes? stage "rm -rf \"$proof_home\"") stage))))

(defdescribe compose-passes-no-second-version-source-test
             (it "leaves the version to the image it builds"
                 (let [compose (slurp "docker-compose.yml")]
                   ;; A build-arg version would be a SECOND version source; the only one
                   ;; is the repo-root VIS_VERSION, baked into the binary.
                   (expect (not (str/includes? compose "VIS_BUILD_SHA")) compose))))

(defdescribe
  wrapper-selects-the-bundled-native-test
  (it "runs the runtime beside it when linked from elsewhere"
      ;; Exactly the image's arrangement: /opt/vis/agent holds the wrapper and
      ;; its `vis-agent-native`, /usr/local/bin/vis-agent is a symlink into that
      ;; directory, and VIS_RUNTIME says which runtime.
      (let
        [tmp
         (.toFile (Files/createTempDirectory "vis-native-launcher" (make-array FileAttribute 0)))

         bundle
         (io/file tmp "agent")

         wrapper
         (io/file bundle "vis-agent")

         native
         (io/file bundle "vis-agent-native")

         link
         (io/file tmp "vis-agent")]

        (try
          (.mkdirs bundle)
          (io/copy (io/file "bin" "vis-agent") wrapper)
          (.setExecutable wrapper true false)
          (spit native "#!/bin/sh\nexit 0\n")
          (.setExecutable native true false)
          (Files/createSymbolicLink (.toPath link)
                                    ^Path (.toPath wrapper)
                                    (make-array FileAttribute 0))
          (let
            [{:keys [exit output]}
             (run-wrapper link {"HOME" (.getAbsolutePath tmp) "VIS_RUNTIME" "native"} ["runtime"])]
            (expect (zero? exit) output)
            (expect (re-find #"(?m)^Runtime: +native \(VIS_RUNTIME\)$" output) output)
            (expect (re-find (re-pattern (str "(?m)^Native: +"
                                              (java.util.regex.Pattern/quote (.getCanonicalPath
                                                                               native))
                                              "$"))
                             output)
                    output))
          (finally (doseq [file (reverse (file-seq tmp))]
                     (io/delete-file file true)))))))
