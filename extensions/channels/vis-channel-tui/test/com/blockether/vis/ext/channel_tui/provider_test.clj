(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [com.blockether.vis.ext.channel-tui.provider]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-dialog-namespace-test
  (it "loads the provider dialog namespace"
    (expect (some? (find-ns 'com.blockether.vis.ext.channel-tui.provider)))))
