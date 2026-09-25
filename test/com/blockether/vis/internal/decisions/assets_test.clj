(ns com.blockether.vis.internal.decisions.assets-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.io File FileOutputStream]
           [java.nio.file Files]
           [java.util.zip ZipEntry ZipOutputStream]))

(defn- fixture!
  [names]
  (let [^File archive
        (File/createTempFile "vis-decisions-test-" ".zip")

        payloads
        (into {}
              (map (fn [name]
                     [name (.getBytes (str "test " name) "UTF-8")]))
              names)

        provenance
        {"model" "test-model"
         "revision" "test-revision"
         "kind" "inference"
         "files" (into {}
                       (map (fn [[name data]]
                              [name
                               {"bytes" (alength ^bytes data) "sha256" (util/sha256-hex data)}]))
                       payloads)}]

    (with-open [zip (ZipOutputStream. (FileOutputStream. archive))]
      (doseq [[name data] (conj (vec payloads)
                                ["PROVENANCE.json" (.getBytes (wire/json-str provenance) "UTF-8")])]
        (.putNextEntry zip (ZipEntry. name))
        (.write zip ^bytes data)
        (.closeEntry zip)))
    archive))

(defn- model-for
  [^File archive]
  {:id "test-model"
   :revision "test-revision"
   :artifacts {:inference {:file "test.zip"
                           :bytes (.length archive)
                           :sha256 (util/sha256-hex (Files/readAllBytes (.toPath archive)))
                           :requires ["model.onnx" "tokenizer/tokenizer.json" "PROVENANCE.json"]}}})

(defn- error-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe
  catalog-test
  (it "pins the baseline, FP32, complete training checkpoint and both CPU wheelhouses"
      (let [model
            (assets/entry "laya-typed-decisions")

            artifacts
            (concat (map #(assets/artifact model %) [:inference :training])
                    (map #(assets/artifact model :wheels %) ["macos-arm64" "linux-x86_64"]))]

        (expect (= "Apache-2.0" (:license model)))
        (expect (= 40 (count (:revision model))))
        (expect (= 4 (count artifacts)))
        (doseq [artifact artifacts]
          (expect (re-matches #"[0-9a-f]{64}" (:sha256 artifact)))
          (expect (pos? (:bytes artifact)))
          (expect (str/starts-with?
                    (:url artifact)
                    "https://github.com/Blockether/vis/releases/download/assets-pack/"))
          (expect (seq (:requires artifact))))
        (expect (= :decisions/unknown-model (:type (error-data #(assets/entry "unknown")))))))
  (it "publishes both complete GLiNER2.5 families with distinct pinned artifacts"
      (expect (= #{"laya-typed-decisions" "gliner2.5-base" "gliner2.5-decide"}
                 (set (map :id (assets/manifest)))))
      (doseq [[id revision] {"gliner2.5-base" "7f1ae80f150e9d3e262ec1684d0d78208e2595d0"
                             "gliner2.5-decide" "bbe10ff77ebb238777c17d3a8ac9260e30929057"}]
        (let [model (assets/entry id)
              artifacts (concat (map #(assets/artifact model %) [:inference :training])
                                (map #(assets/artifact model :wheels %)
                                     ["macos-arm64" "linux-x86_64"]))]

          (expect (= revision (:revision model)))
          (expect (= "Apache-2.0" (:license model)))
          (expect (str/ends-with? (:source-url model) revision))
          (expect (= (assets/inference-required id) (:requires (assets/artifact model :inference))))
          (expect (some #{"model.safetensors"} (:requires (assets/artifact model :training))))
          (expect (= 4 (count artifacts)))
          (doseq [artifact artifacts]
            (expect (re-matches #"[0-9a-f]{64}" (:sha256 artifact)))
            (expect (pos? (:bytes artifact)))
            (expect (str/starts-with?
                      (:url artifact)
                      "https://github.com/Blockether/vis/releases/download/assets-pack/"))
            (expect (seq (:requires artifact)))))))
  (it "selects only a declared local training platform"
      (expect (contains? #{"macos-arm64" "linux-x86_64"} (assets/platform)))))

(defdescribe
  installation-test
  (it "installs the complete model from a pinned local archive, with no network"
      (let [^File archive
            (fixture! ["model.onnx" "tokenizer/tokenizer.json"])

            model
            (model-for archive)

            dir
            (str (io/file (System/getProperty "java.io.tmpdir")
                          (str "vis-decision-install-" (System/nanoTime))))]

        (try (expect (= dir (assets/install! model :inference dir (str (.toURI archive)))))
             (expect (assets/installed? (assets/artifact model :inference) dir))
             (expect (= "test model.onnx" (slurp (io/file dir "model.onnx"))))
             (.delete archive)
             (expect (= dir (assets/install! model :inference dir "file:///unavailable.zip")))
             (finally (files/delete-dir! (io/file dir)) (.delete archive)))))
  (it "refuses a mismatched archive without changing the installed model"
      (let [^File archive
            (fixture! ["model.onnx" "tokenizer/tokenizer.json"])

            model
            (model-for archive)

            dir
            (str (io/file (System/getProperty "java.io.tmpdir")
                          (str "vis-decision-mismatch-" (System/nanoTime))))]

        (try (assets/install! model :inference dir (str (.toURI archive)))
             (let [bad-model
                   (assoc-in model [:artifacts :inference :sha256] (apply str (repeat 64 "0")))]
               (expect (= :speech/checksum-mismatch
                          (:type
                            (error-data
                              #(assets/install! bad-model :inference dir (str (.toURI archive)))))))
               (expect (= "test model.onnx" (slurp (io/file dir "model.onnx")))))
             (finally (files/delete-dir! (io/file dir)) (.delete archive)))))
  (it "rejects zip traversal and leaves no partial install"
      (let [^File archive
            (fixture! ["../outside.onnx" "model.onnx" "tokenizer/tokenizer.json"])

            model
            (model-for archive)

            dir
            (str (io/file (System/getProperty "java.io.tmpdir")
                          (str "vis-decision-traversal-" (System/nanoTime))))]

        (try (expect (= :decisions/unsafe-path
                        (:type (error-data
                                 #(assets/install! model :inference dir (str (.toURI archive)))))))
             (expect (not (.exists (io/file dir))))
             (finally (files/delete-dir! (io/file dir)) (.delete archive))))))
