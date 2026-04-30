(ns com.blockether.vis.ext.channel-tui.provider-test
  (:require [com.blockether.vis.ext.channel-tui.provider :as provider]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-dialog-namespace-test
  (it "loads the provider dialog namespace"
    (expect (some? (find-ns 'com.blockether.vis.ext.channel-tui.provider)))))

(defdescribe swap-items-test
  (it "swaps two positions without touching the others"
    (let [swap-items @#'provider/swap-items]
      (expect (= [:a :c :b :d]
                (swap-items [:a :b :c :d] 1 2))))))

(defdescribe move-model-to-front-test
  (it "moves the selected model to the first slot"
    (let [move-model-to-front @#'provider/move-model-to-front]
      (expect (= [{:name "beta"} {:name "alpha"} {:name "gamma"}]
                (move-model-to-front [{:name "alpha"} {:name "beta"} {:name "gamma"}] 1))))))
