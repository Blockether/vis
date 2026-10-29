(ns com.blockether.vis.internal.decisions.assets-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.io File FileOutputStream]
           [java.net URI]
           [java.nio.file Files]
           [java.util Arrays]
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

(defn- split!
  "Write an archive as two local part files, described like manifest parts."
  [^File archive]
  (let [data
        (Files/readAllBytes (.toPath archive))

        middle
        (quot (alength data) 2)]

    (mapv (fn [[from to]]
            (let [^File part
                  (File/createTempFile "vis-decisions-part-" ".zip")

                  piece
                  (Arrays/copyOfRange data (int from) (int to))]

              (with-open [out (FileOutputStream. part)]
                (.write out piece))
              {:file (.getName part)
               :bytes (alength piece)
               :sha256 (util/sha256-hex piece)
               :url (str (.toURI part))}))
          [[0 middle] [middle (alength data)]])))

;; #294: SDK packaging, transport and gateway extraction share canonical bounds.
(defdescribe publication-limits-test
             (it "derives compressed and expanded byte limits from the gateway schema"
                 (let [schema (document/schema-document "gateway")]
                   (expect (= 6000000000
                              assets/max-inference-upload-bytes
                              gateway-contract/max-decision-archive-bytes
                              (get-in schema ["$defs" "decision_archive_bytes" "maximum"])))
                   (expect (= 6000000000
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
  (it "splits every archive above the release asset limit into verified parts"
      (expect (seq (:parts (assets/artifact (assets/entry "gliner2.5-decide-1b") :training))))
      (expect (seq (:parts (assets/artifact (assets/entry "gliner2.5-decide-1b") :inference))))
      (doseq [model
              (assets/manifest)

              artifact
              (vals (:artifacts model))]

        (if-let [parts (seq (:parts artifact))]
          (do (expect (nil? (:url artifact)))
              (expect (= (:bytes artifact) (reduce + (map :bytes parts))))
              (doseq [part parts]
                (expect (re-matches #"[0-9a-f]{64}" (:sha256 part)))
                (expect (< 0 (:bytes part) 2147483648))))
          (expect (< (:bytes artifact) 2147483648))))))

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
  (it
    "joins an archive from verified parts and refuses a wrong size or checksum"
    (let [^File archive
          (fixture! ["model.onnx" "tokenizer/tokenizer.json"])

          parts
          (split! archive)

          model
          (assoc-in (model-for archive) [:artifacts :inference :parts] parts)

          dir
          (str (io/file (System/getProperty "java.io.tmpdir")
                        (str "vis-decision-parts-" (System/nanoTime))))

          refused
          (str dir "-refused")]

      (try (expect (= dir (assets/install! model :inference dir nil)))
           (expect (assets/installed? (assets/artifact model :inference) dir))
           (expect (= "test model.onnx" (slurp (io/file dir "model.onnx"))))
           (expect (= :decisions/archive-size
                      (:type (error-data #(assets/install! (assoc-in model
                                                             [:artifacts :inference :parts 0 :bytes]
                                                             1)
                                                           :inference
                                                           refused
                                                           nil)))))
           (expect (= :decisions/archive-checksum
                      (:type (error-data #(assets/install! (assoc-in model
                                                             [:artifacts :inference :sha256]
                                                             (apply str (repeat 64 "0")))
                                                           :inference
                                                           refused
                                                           nil)))))
           (expect (not (.exists (io/file refused))))
           (finally (files/delete-dir! (io/file dir))
                    (.delete archive)
                    (doseq [part parts]
                      (.delete (File. (URI. ^String (:url part)))))))))
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
