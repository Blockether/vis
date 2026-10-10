(ns com.blockether.vis.internal.foundation.session-slashes-test
  (:require [com.blockether.vis.internal.foundation.session-slashes :as session-slashes]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe saveable-session-slashes-test
             ;; Regression for #360: `/goal` is saveable as a prompt; other session commands are not.
             (it "declares only /goal saveable"
                 (expect (= #{"goal"}
                            (set (keep #(when (:slash/saveable? %) (:slash/name %))
                                       session-slashes/specs))))))
