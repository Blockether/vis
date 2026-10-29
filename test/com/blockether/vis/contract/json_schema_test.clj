(ns com.blockether.vis.contract.json-schema-test
  "JSON Schemas are the sole portable structural contracts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private contract-root (io/file "packages/vis-contract/resources/vis-contract"))

(defn- files-under
  [root]
  (when (.exists (io/file root)) (filter #(.isFile ^java.io.File %) (file-seq (io/file root)))))

(defn- json-names
  [root]
  (let [root-file (.getCanonicalFile (io/file root))]
    (->> (files-under root-file)
         (filter (fn [file]
                   (and (= root-file (.getCanonicalFile (.getParentFile ^java.io.File file)))
                        (str/ends-with? (.getName ^java.io.File file) ".json"))))
         (map (fn [file]
                (str/replace (.getName ^java.io.File file) #"\.json$" "")))
         set)))

(defdescribe json-schema-only-contract-test
             (it "ships no separate JSON catalogs beside the schemas"
                 (expect (empty? (json-names contract-root))))
             (it "loads and compiles every schema without a paired catalog"
                 (let [names (json-names (io/file contract-root "schema"))]
                   (expect (seq names))
                   (doseq [schema-name names]
                     (let [schema (document/schema-document schema-name)]
                       (expect (= "https://json-schema.org/draft/2020-12/schema"
                                  (get schema "$schema")))
                       (expect (seq (get schema "$defs")))
                       (expect (boolean? (document/valid? schema-name nil))))))))
