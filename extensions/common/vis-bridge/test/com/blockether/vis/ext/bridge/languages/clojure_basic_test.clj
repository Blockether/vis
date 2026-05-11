(ns com.blockether.vis.ext.bridge.languages.clojure-basic-test
  (:require
   [com.blockether.vis.ext.bridge.languages.clojure-basic :as basic]
   [com.blockether.vis.ext.bridge.schema :as schema]
   [lazytest.core :refer [defdescribe expect it]]))

(def sample-src
  "(ns demo.core\n  (:require [clojure.string :as str]))\n\n(def x 1)\n(defn run [s] (str/trim s))\n(defn- hidden [] :ok)\n")

(defdescribe clojure-basic-extractor-test
  (it "extracts namespace, vars, and require edges"
    (let [result (basic/extract-file "src/demo/core.clj" sample-src)
          nodes (:nodes result)
          edges (:edges result)]
      (expect (schema/valid-extract-result? result))
      (expect (= #{"demo.core" "demo.core/x" "demo.core/run" "demo.core/hidden"}
                (set (map :qualified-name nodes))))
      (expect (some #(and (= :requires (:edge-kind %))
                       (= "clojure.string" (:target %)))
                edges))
      (expect (= :private (:visibility (first (filter #(= "demo.core/hidden" (:qualified-name %)) nodes))))))))
