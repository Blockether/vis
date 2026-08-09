(ns com.blockether.vis.container-image-test
  "The gateway container runs the NATIVE runtime, and nothing else.

   A deployment is only worth trusting when it serves the artifact users
   install. The image therefore installs the release bundle the `builder` and
   `native-export` stages produce — wrapper, `vis-agent-native` and the
   language-resources sidecar in one directory — so a gap in
   `reachability-metadata.json`, or a constant native-image folded in at BUILD
   time (`config.clj` folds `config-dir` off `user.home`, which is why the
   builder is handed `-Duser.home=/home/vis`), fails in the build rather than in
   production. These tests pin that arrangement, and the wrapper contract it
   stands on."
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
        (expect (str/includes? stage "vis-agent python -c") stage))))

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
