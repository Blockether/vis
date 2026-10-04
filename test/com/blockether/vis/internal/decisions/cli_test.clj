(ns com.blockether.vis.internal.decisions.cli-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.assets-test :as assets-test]
            [com.blockether.vis.internal.decisions.cli :as cli]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayOutputStream File PrintStream]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- artifact
  [^File archive requires]
  {:url (str (.toURI archive))
   :bytes (.length archive)
   :sha256 (util/sha256-hex (Files/readAllBytes (.toPath archive)))
   :requires requires})

(defn- download!
  "Run `vis-agent decisions models download` with `args` and return its output lines."
  [& args]
  (let [out (ByteArrayOutputStream.)]
    (with-redefs [config/init-cli! (fn [& _])
                  config/original-stdout (PrintStream. out true "UTF-8")]

      (expect (= :ok
                 (:status (commandline/dispatch!
                            {:cmd/name "vis-agent" :cmd/subcommands [cli/command]}
                            (into ["vis-agent" "decisions" "models" "download"] args))))))
    (str/split-lines (.toString out "UTF-8"))))

(defdescribe
  download-command-installs-the-training-checkpoint-on-request
  (it
    "installs only FP32 by default, and FP32 with the complete training checkpoint with --training"
    (let [inference
          (#'assets-test/fixture! ["model.onnx"])

          training
          (#'assets-test/fixture! ["model.safetensors"])

          root
          (.toFile (Files/createTempDirectory "vis-decisions-cli-" (make-array FileAttribute 0)))

          model
          {:id "test-model"
           :revision "test-revision"
           :artifacts {:inference (artifact inference ["model.onnx" "PROVENANCE.json"])
                       :training (artifact training ["model.safetensors" "PROVENANCE.json"])}}

          installed?
          (fn [kind]
            (assets/installed? (assets/artifact model kind) (assets/install-dir model kind)))]

      (try (with-redefs [assets/models-root
                         (constantly (.getPath root))

                         assets/manifest
                         (constantly [model])]

             (expect (= [(str "inference: " (assets/install-dir model :inference))]
                        (download! "--model" "test-model")))
             (expect (installed? :inference))
             (expect (not (installed? :training)))
             (expect (= [(str "inference: " (assets/install-dir model :inference))
                         (str "training: " (assets/install-dir model :training))
                         "Training runtime: https://github.com/Blockether/vis-decisions"]
                        (download! "--model" "test-model" "--training")))
             (expect (installed? :inference))
             (expect (installed? :training)))
           (finally (.delete ^File inference) (.delete ^File training) (files/delete-dir! root))))))
