(ns com.blockether.vis.ext.channel-tui.builtin-hooks-test
  (:require
   [com.blockether.vis.ext.channel-tui.builtin-hooks :as builtin-hooks]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe builtin-hooks-test
  (it "captures its source namespace for hot reload diffing"
    (expect (= "channel-tui.builtin-hooks"
              (:ext/name builtin-hooks/vis-extension)))
    (expect (= ['com.blockether.vis.ext.channel-tui.builtin-hooks]
              (:ext/source-nses builtin-hooks/vis-extension)))))
