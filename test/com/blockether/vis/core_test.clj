(ns com.blockether.vis.core-test
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe extension-api-export-test
  (it "exports raw callable symbol builders"
    (expect (identical? extension/helper vis/helper))
    (expect (identical? extension/raw-var vis/raw-var))))
