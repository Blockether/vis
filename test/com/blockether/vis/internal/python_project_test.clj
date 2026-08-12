(ns com.blockether.vis.internal.python-project-test
  "Layout reads degrade LOUDLY. A GraalPy read that FAILS is retried once and then
   reported as a `:warning`, instead of pretending the project declares no import
   roots — silent degradation surfaces as bogus `No module named <pkg>` errors in
   the user's own tests (Blockether/vis#98)."
  (:require [com.blockether.vis.internal.python-extensions :as pyx]
            [com.blockether.vis.internal.python-project :as pyproj]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tmp (System/getProperty "java.io.tmpdir"))

(defdescribe project-layout-warning-test
             (it "retries once and warns when the GraalPy context cannot be built"
                 (let [calls (atom 0)]
                   (with-redefs
                     [pyx/build-context (fn [_label]
                                          (swap! calls inc)
                                          (throw (ex-info "cold context" {})))]
                     (let [layout (pyproj/project-layout tmp)]
                       (expect (= [] (:import-roots layout)))
                       (expect (= [] (:testpaths layout)))
                       (expect (= 2 @calls))
                       (expect (re-find #"cold context" (str (:warning layout))))
                       (expect (re-find #"retried once" (str (:warning layout))))))))
             (it "stops retrying as soon as a read succeeds"
                 (let
                   [calls
                    (atom 0)

                    real
                    pyx/build-context]

                   (with-redefs
                     [pyx/build-context (fn [label]
                                          (if (= 1 (swap! calls inc))
                                            (throw (ex-info "cold context" {}))
                                            (real label)))]
                     (let [layout (pyproj/project-layout tmp)]
                       (expect (nil? (:warning layout)))
                       (expect (= 2 @calls))))))
             (it "carries no warning for a readable directory"
                 (expect (nil? (:warning (pyproj/project-layout tmp))))))
