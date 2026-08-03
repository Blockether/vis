(ns com.blockether.vis.container-image-test
  "The gateway container runs the JVM runtime, and nothing else.

   Native-image is what made that image expensive and fragile: an hours-long,
   RAM-hungry compile that also FOLDS build-time state into the binary (a
   `user.home`-derived `config-dir`) and aborts at runtime on any reflective
   call missing from `reachability-metadata.json` — one unregistered
   `java.lang.Character.codePointAt` killed the deployed gateway on every
   non-empty `~/.vis/config.yml`. The image ships the source and runs
   `clojure -M:vis` instead. These tests pin that arrangement, and the wrapper
   contract it stands on."
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
  container-runs-the-jvm-runtime-test
  (it "ships the source and links the wrapper at it, instead of a binary"
      (let [stage (runtime-stage)]
        (expect (str/includes? stage "COPY --chown=vis:vis . /opt/vis/src") stage)
        (expect (str/includes? stage "ln -sf /opt/vis/src/bin/vis-agent /usr/local/bin/vis-agent")
                stage)
        (expect (str/includes? stage "VIS_RUNTIME=jvm") stage)))
  (it "never pulls a native image into the gateway build"
      (let [stage (runtime-stage)]
        ;; The `builder` stage still exists — for RELEASE assets, through
        ;; `--target native-export`. A plain `docker build` must not reach it,
        ;; and it only reaches it through a COPY.
        (expect (not (str/includes? stage "--from=builder")) stage)
        (expect (not (re-find #"(?m)^COPY .*vis-agent-native" stage)) stage)
        ;; …and the image proves that to itself while building.
        (expect (str/includes? stage "test ! -e /usr/local/bin/vis-agent-native") stage)
        (expect (str/includes? (dockerfile) "FROM jdk AS builder") (dockerfile))))
  (it "warms the dependency cache as the user that runs it"
      (let
        [stage
         (runtime-stage)

         user-at
         (str/index-of stage "USER vis")

         warm-at
         (str/index-of stage "clojure -P -M:vis")]

        ;; Resolved as root, the deps would land in /root/.m2 and the first boot
        ;; would refetch the world as `vis`.
        (expect (some? warm-at) stage)
        (expect (and (some? user-at) (< user-at warm-at)) stage)))
  (it "proves the runtime while building, not in production"
      (expect (str/includes? (runtime-stage) "vis-agent runtime | grep -Eq '^Runtime: +jvm'")
              (runtime-stage))))

(defdescribe compose-build-is-native-image-free-test
             (it "drops the native-image knob and stamps the version instead"
                 (let [compose (slurp "docker-compose.yml")]
                   ;; VIS_ORACLE_NATIVE_IMAGE only ever tuned the native build; passing it
                   ;; to an image that has none is a lie about what the build does.
                   (expect (not (str/includes? compose "VIS_ORACLE_NATIVE_IMAGE")) compose)
                   (expect (str/includes? compose "VIS_BUILD_SHA: ${VIS_BUILD_SHA:-}") compose))))

(defdescribe
  wrapper-selects-jvm-through-a-symlink-test
  (it "runs THIS checkout when linked from elsewhere, with VIS_RUNTIME=jvm"
      ;; Exactly the image's arrangement: /usr/local/bin/vis-agent is a symlink
      ;; into the checkout at /opt/vis/src, and VIS_RUNTIME says which runtime.
      (let
        [source-root
         (.getCanonicalPath (io/file "."))

         tmp
         (.toFile (Files/createTempDirectory "vis-jvm-launcher" (make-array FileAttribute 0)))

         link
         (io/file tmp "vis-agent")]

        (try (Files/createSymbolicLink (.toPath link)
                                       ^Path (.toPath (io/file source-root "bin" "vis-agent"))
                                       (make-array FileAttribute 0))
             (let
               [{:keys [exit output]}
                (run-wrapper link {"HOME" (.getAbsolutePath tmp) "VIS_RUNTIME" "jvm"} ["runtime"])]
               (expect (zero? exit) output)
               (expect (re-find #"(?m)^Runtime: +jvm \(VIS_RUNTIME\)$" output) output)
               (expect (re-find (re-pattern (str "(?m)^Source: +"
                                                 (java.util.regex.Pattern/quote source-root)
                                                 "$"))
                                output)
                       output))
             (finally (doseq [file (reverse (file-seq tmp))]
                        (io/delete-file file true)))))))
