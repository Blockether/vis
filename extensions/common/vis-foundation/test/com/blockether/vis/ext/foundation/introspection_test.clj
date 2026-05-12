(ns com.blockether.vis.ext.foundation.introspection-test
  (:require
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe introspection-public-surface-test
  (it "exposes conversation and Clojure symbol introspection symbols"
    (let [symbols (set (map :ext.symbol/symbol introspection/all-symbols))]
      (expect (contains? symbols 'conversation-state))
      (expect (contains? symbols 'conversation-report))
      (expect (contains? symbols 'clojure-symbol-documentation))
      (expect (contains? symbols 'clojure-symbol-source-code))
      (expect (contains? symbols 'clojure-symbol-metadata))
      (expect (contains? symbols 'clojure-symbol-apropos))
      (expect (= 6 (count symbols))))))
