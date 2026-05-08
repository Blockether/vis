(ns com.blockether.vis.ext.foundation.introspection-test
  (:require
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe introspection-public-surface-test
  (it "exposes conversation-state and conversation-report symbols"
    (let [symbols (set (map :ext.symbol/sym introspection/all-symbols))]
      (expect (contains? symbols 'conversation-state))
      (expect (contains? symbols 'conversation-report))
      (expect (= 2 (count symbols))))))
