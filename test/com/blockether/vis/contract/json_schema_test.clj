(ns com.blockether.vis.contract.json-schema-test
  "The published contract has one language-neutral representation: JSON documents validated by JSON Schema."
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

(defdescribe
  json-schema-only-contract-test
  (it "ships no EDN contract documents"
      (expect (empty? (filter #(str/ends-with? (.getName ^java.io.File %) ".edn")
                              (files-under contract-root)))))
  (it "checks in no generated aggregate"
      (expect (every? #(not (.exists (io/file %)))
                      ["packages/vis-contract/contract.json"
                       "packages/vis-contract/python/src/vis_contract/contract.json"])))
  (it "has one schema for every contract document"
      (let [documents
            (json-names contract-root)

            schemas
            (disj (json-names (io/file contract-root "schema")) "common")]

        (expect (seq documents))
        (expect (= documents schemas))))
  (it "keeps API-style spellings in the JSON document and schema aligned"
      (expect (= (set (keys (get (document/load! "config") "api_style_aliases")))
                 (set (get-in (document/schema-document "config") ["$defs" "apiStyle" "enum"])))))
  (it "does not mirror the configuration schema in engine predicates"
      (expect (nil? (re-find #"\(def(?:n)?\s+[^\s]+-schema\b"
                             (slurp "src/com/blockether/vis/internal/config/validation.clj")))))
  (it "has no Clojure Spec dependency in repository code"
      (let [dependency-name
            (str "clojure." "spec.alpha")

            offenders
            (->> (cons (io/file "deps.edn")
                       (mapcat files-under ["src" "test" "apps" "extensions" "packages"]))
                 (remove (fn [file]
                           (some #{"target"} (str/split (.getPath ^java.io.File file) #"[\/]"))))
                 (filter #(let [name (.getName ^java.io.File %)] (or (= "deps.edn" name)
                                                                     (re-find #"\.clj[cs]?$"
                                                                              name))))
                 (filter #(str/includes? (slurp %) dependency-name))
                 (map #(.getPath ^java.io.File %))
                 sort
                 vec)]

        (expect (= [] offenders) (pr-str offenders)))))
