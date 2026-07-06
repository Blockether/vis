(ns com.blockether.vis.core-test
  (:require [com.blockether.vis.core :as vis]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe channel-contributions-api-test
             (it "exposes the channel contribution lookup surface"
                 (expect (ifn? vis/channel-contributions-for))))
