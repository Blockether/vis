(ns com.blockether.vis.ext.channel-tui.builtin-hooks-test
  (:require [com.blockether.vis.ext.channel-tui.builtin-hooks :as builtin-hooks]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe builtin-hooks-test
             (it "exposes footer contributions without registering a standalone extension"
                 (expect (= [:tui.slot/footer-segment] (keys builtin-hooks/channel-contributions)))
                 (expect
                   (= [:tui.builtin.model/footer]
                      (mapv :id (:tui.slot/footer-segment builtin-hooks/channel-contributions))))))
