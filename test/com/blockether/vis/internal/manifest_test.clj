(ns com.blockether.vis.internal.manifest-test
  (:require
   [com.blockether.vis.internal.manifest :as manifest]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe manifest-normalization-test
  (it "normalizes manifests to namespaces only"
    (let [normalize (deref (resolve 'com.blockether.vis.internal.manifest/normalize-vis-edn))]
      (expect (= {'demo {:nses ['demo.core]}}
                (normalize {'demo {:nses ['demo.core]
                                   :ignored "not part of the manifest contract"}})))))

  (it "exposes public load failure state as a vector"
    (expect (vector? (manifest/load-failures)))))
