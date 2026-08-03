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
