(ns com.blockether.vis.ext.channel-tui.builtin-hooks-test
  (:require
   [com.blockether.vis.ext.channel-tui.builtin-hooks :as builtin-hooks]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe builtin-hooks-test
  (it "declares the TUI core loader ns for hot reload diffing"
    (expect (= 'com.blockether.vis.ext.channel-tui.builtin-hooks
              (:ext/namespace builtin-hooks/vis-extension)))
    (expect (= ['com.blockether.vis.ext.channel-tui.core]
              (:ext/nses builtin-hooks/vis-extension)))))
