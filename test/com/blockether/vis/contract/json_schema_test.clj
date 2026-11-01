(ns com.blockether.vis.contract.json-schema-test
  "JSON Schemas are the sole portable structural contracts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.skjema.core :as skjema]
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

(defdescribe
  json-schema-only-contract-test
  (it "ships no separate JSON catalogs beside the schemas"
      (expect (empty? (json-names contract-root))))
  (it "loads and compiles every schema without a paired catalog"
      (let [names (json-names (io/file contract-root "schema"))]
        (expect (seq names))
        (doseq [schema-name names]
          (let [schema (document/schema-document schema-name)]
            (expect (= "https://json-schema.org/draft/2020-12/schema" (get schema "$schema")))
            (expect (seq (get schema "$defs")))
            (expect (boolean? (document/valid? schema-name nil)))))))
  ;; Regression: each definition compiled its whole document again. The 54
  ;; view definitions cost 400 ms at gateway start.
  (it "compiles a document once for all of its definitions"
      (let [compiles
            (atom [])

            compile-schema
            skjema/compile-schema]

        (reset! @#'document/documents {})
        (reset! @#'document/validators {})
        (with-redefs [skjema/compile-schema (fn [schema opts]
                                              (swap! compiles conj (:base opts))
                                              (compile-schema schema opts))]
          (doseq [definition (take 3 (keys (get (document/schema-document "view") "$defs")))]
            (document/explain "view" definition {}))
          (expect (= 1 (count @compiles)))))))

(defn- cross-document-refs
  "The `$ref` strings of `schema` that name another schema document."
  [schema]
  (->> (tree-seq coll? seq schema)
       (keep #(when (and (map-entry? %) (= "$ref" (key %))) (val %)))
       (filter document/ref-document)
       distinct))

(defdescribe
  cross-document-ref-test
  ;; Regression: council.publish with `files` threw `cannot resolve $ref
  ;; "agents.json#/$defs/files"`. A hand-kept list chose the documents that a
  ;; schema could reference, and it did not have agents.json.
  (it "resolves every cross-document $ref of every schema"
      (doseq [schema-name
              (json-names (io/file contract-root "schema"))

              ref
              (cross-document-refs (document/schema-document schema-name))]

        (let [compiled
              (skjema/compile-ref (#'document/compiled-document schema-name) ref)

              outcome
              (try (skjema/explain compiled {}) :resolved (catch Throwable t (ex-message t)))]

          (expect (= :resolved outcome) (str schema-name " " ref))))))
