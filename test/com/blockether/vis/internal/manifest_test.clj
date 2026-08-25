(ns com.blockether.vis.internal.manifest-test
  "The distribution has one closed manifest and no ambient discovery format."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [com.blockether.vis.internal.manifest :as manifest]
            [lazytest.core :refer [defdescribe expect it]]))


(defdescribe manifest-shape-test
             (it "reads exactly one versioned initialization and apropos list"
                 (let [m (manifest/read-manifest)]
                   (expect (s/valid? ::manifest/manifest m)
                           (pr-str (s/explain-data ::manifest/manifest m)))
                   (expect (= #{:version :initialization :apropos} (set (keys m))))
                   (expect (= 1 (:version m)))
                   (expect (seq (:initialization m)))
                   (expect (seq (:apropos m)))))
             (it "names every initializer and apropos resource explicitly"
                 (let [{:keys [initialization apropos]} (manifest/read-manifest)]
                   (doseq [initializer initialization]
                     (expect (qualified-symbol? initializer)))
                   (doseq [resource apropos]
                     (expect (some? (io/resource resource)) resource)))))

(defdescribe initialization-order-test
             (it "invokes each listed function once in manifest order"
                 (let [calls
                       (atom [])

                       fns
                       {'example.alpha/start! #(swap! calls conj :alpha)
                        'example.beta/start! #(swap! calls conj :beta)}]

                   (with-redefs [clojure.core/requiring-resolve #(get fns %)]
                     (manifest/initialize-manifest! {:version 1
                                                     :initialization ['example.alpha/start!
                                                                      'example.beta/start!]
                                                     :apropos ["META-INF/vis/apropos/docs.edn"]}))
                   (expect (= [:alpha :beta] @calls)))))
