(ns com.blockether.vis.ext.bridge.languages.registry-test
  (:require
   [com.blockether.vis.ext.bridge.languages.registry :as registry]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe bridge-language-registry-test
  (it "selects extractors by path and language"
    (expect (= "markdown" (:language (registry/extractor-for-path "README.md"))))
    (expect (= "clojure" (:language (registry/extractor-for-path "src/demo/core.clj"))))
    (expect (= "clojure" (:language (registry/extractor-for-language :clojure)))))

  (it "dispatches one-file Markdown extraction"
    (let [result (registry/extract-file "README.md" "# A" {:language "markdown"})]
      (expect (= "markdown" (get-in result [:stats :language])))
      (expect (= 2 (count (:nodes result)))))))
