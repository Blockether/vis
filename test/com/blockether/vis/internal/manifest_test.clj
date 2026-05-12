(ns com.blockether.vis.internal.manifest-test
  (:require
   [com.blockether.vis.internal.manifest :as manifest]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe manifest-doc-normalization-test
  (it "normalizes docs without adding computed backlinks"
    (let [normalize    (deref (resolve 'com.blockether.vis.internal.manifest/normalize-doc-descriptor))
          backlink-key (keyword (str "ref" "links"))
          out          (normalize "README.md" {:description "demo"
                                               :content "body"
                                               :links [{:to-doc "OTHER.md"}
                                                       {:bad true}]})]
      (expect (= {:created-at nil
                  :description "demo"
                  :content "body"
                  :links [{:to-doc "OTHER.md"}]}
                out))
      (expect (not (contains? out backlink-key)))))

  (it "exposes public load failure state as a vector"
    (expect (vector? (manifest/load-failures)))))
