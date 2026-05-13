(ns com.blockether.vis.ext.channel-tui.code-block-test
  (:require
   [com.blockether.vis.ext.channel-tui.render :as render]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe tui-code-block-test
  (it "keeps code text visible in markdown formatting"
    (let [out (render/format-answer-markdown [:ir {} [:code {:lang "clojure"} "(+ 1 2)"]] 80)]
      (expect (string? out))
      (expect (re-find #"\(\+ 1 2\)" out)))))
