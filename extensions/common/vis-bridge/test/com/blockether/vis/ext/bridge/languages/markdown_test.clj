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

  (it "extracts file, section, link, mention, and code-block facts"
    (let [result (md/extract-file "README.md" sample-md)
          nodes (:nodes result)
          edges (:edges result)]
      (expect (schema/valid-extract-result? result))
      (expect (= 4 (count nodes)))
      (expect (some #(= "doc:README.md#bridge" (:qualified-name %)) nodes))
      (expect (some #(= :code-block (:kind %)) nodes))
      (expect (some #(and (= :links-to (:edge-kind %))
                       (= "docs/src/SUMMARY.md" (:target %)))
                edges))
      (expect (some #(and (= :mentions (:edge-kind %))
                       (= "com.foo/bar" (:target %)))
                edges)))))
