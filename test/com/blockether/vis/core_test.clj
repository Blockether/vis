(ns com.blockether.vis.core-test
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe extension-api-export-test
  (it "exports symbol as the callable symbol builder"
    (expect (identical? extension/symbol vis/symbol))))
