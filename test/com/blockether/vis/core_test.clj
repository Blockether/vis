(ns com.blockether.vis.core-test
  (:require
   [com.blockether.vis.core]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe core-retired-aliases-test
  (it "does not re-export removed loop helpers"
    (expect (nil? (ns-resolve 'com.blockether.vis.core 'reset-router!)))
    (expect (nil? (ns-resolve 'com.blockether.vis.core 'provider-has-reasoning?)))
    (expect (nil? (ns-resolve 'com.blockether.vis.core 'remove-provider!)))))
