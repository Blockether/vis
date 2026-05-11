(ns com.blockether.vis.ext.bridge.languages.markdown-test
  (:require
   [com.blockether.vis.ext.bridge.languages.markdown :as md]
   [com.blockether.vis.ext.bridge.schema :as schema]
   [lazytest.core :refer [defdescribe expect it]]))

(def sample-md
  "# Bridge\n\nSee [docs](docs/src/SUMMARY.md) and `com.foo/bar`.\n\n## Extractors\n\n```clojure\n(defn x [])\n```\n")

(defdescribe markdown-extractor-test
  (it "supports markdown paths"
    (expect (true? (md/supports-path? "README.md")))
    (expect (false? (md/supports-path? "src/core.clj"))))

  (it "extracts file, doc-section, link, mention, and code-block payload facts"
    (let [result (md/extract-file "README.md" sample-md)
          nodes (:nodes result)
          edges (:edges result)
          extractor-section (first (filter #(= "doc:README.md#extractors" (:qualified-name %)) nodes))]
      (expect (schema/valid-extract-result? result))
      (expect (= 3 (count nodes)))
      (expect (= #{:file :doc-section} (set (map :kind nodes))))
      (expect (some #(= "doc:README.md#bridge" (:qualified-name %)) nodes))
      (expect (= 1 (get-in result [:stats :code-block-count])))
      (expect (= "clojure" (-> extractor-section :metadata :code-blocks first :language)))
      (expect (some #(and (= :links-to (:edge-kind %))
                       (= "docs/src/SUMMARY.md" (:target %)))
                edges))
      (expect (some #(and (= :mentions (:edge-kind %))
                       (= "com.foo/bar" (:target %)))
                edges)))))
