(ns com.blockether.vis.internal.decisions.assets-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
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

;; #294: SDK packaging, transport and gateway extraction share canonical bounds.
(defdescribe publication-limits-test
             (it "derives compressed and expanded byte limits from the gateway schema"
                 (let [schema (document/schema-document "gateway")]
                   (expect (= 2400000000
                              assets/max-inference-upload-bytes
                              gateway-contract/max-decision-archive-bytes
                              (get-in schema ["$defs" "decision_archive_bytes" "maximum"])))
                   (expect (= 3000000000
                              @#'assets/max-expanded-bytes
                              gateway-contract/max-decision-expanded-bytes
                              (get-in schema ["$defs" "decision_expanded_bytes" "maximum"])))))
             (it "rejects excessive expanded bytes and cleans the partial installation"
                 (let [^File archive
                       (fixture! ["model.onnx" "tokenizer/tokenizer.json"])

                       model
                       (model-for archive)

                       dir
                       (io/file (System/getProperty "java.io.tmpdir")
                                (str "vis-decision-expanded-limit-" (System/nanoTime)))]

                   (try (with-redefs-fn {#'assets/max-expanded-bytes 8}
                          #(expect (= :decisions/invalid-archive
                                      (:type (error-data (fn []
                                                           (assets/install! model
                                                                            :inference
                                                                            (str dir)
                                                                            (str (.toURI
                                                                                   archive)))))))))
                        (expect (not (.exists dir)))
                        (finally (files/delete-dir! dir) (.delete archive))))))

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
  (it "publishes every complete GLiNER2.5 model with distinct pinned artifacts"
      (expect (= (conj (set (keys assets/gliner-architectures)) "laya-typed-decisions")
                 (set (map :id (assets/manifest)))))
      (expect (= #{"gliner2.5-base" "gliner2.5-small" "gliner2.5-multi" "gliner2.5-decide"
                   "gliner2.5-multi-decide"}
                 (set (keys assets/gliner-architectures))))
      (expect (apply distinct?
                (for [id
                      (keys assets/gliner-architectures)

                      kind
                      [:inference :training]]

                  (:sha256 (assets/artifact (assets/entry id) kind)))))
      (doseq [[id revision] {"gliner2.5-base" "7f1ae80f150e9d3e262ec1684d0d78208e2595d0"
                             "gliner2.5-small" "7132dc4561c3f94563c6147e75ffa8ef34c4964a"
                             "gliner2.5-multi" "2ca71aafb3446d9014e1c55c7ff51c9bc7209c47"
                             "gliner2.5-decide" "bbe10ff77ebb238777c17d3a8ac9260e30929057"
                             "gliner2.5-multi-decide" "a35a0cd3b7a0f00f2effc576f454cd48fa98aa5f"}]
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
