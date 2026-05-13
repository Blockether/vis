(ns com.blockether.vis.ext.foundation.introspection-test
  (:require
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe introspection-public-surface-test
  (it "exposes conversation and Clojure symbol introspection symbols"
    (let [symbols (set (map :ext.symbol/symbol introspection/all-symbols))]
      (expect (contains? symbols 'conversation-state))
      (expect (contains? symbols 'conversation-report))
      (expect (contains? symbols 'engine-symbol-documentation))
      (expect (contains? symbols 'engine-symbol-source-code))
      (expect (contains? symbols 'engine-symbol-metadata))
      (expect (contains? symbols 'engine-symbol-apropos))
      (expect (= 6 (count symbols))))))

(defdescribe conversation-state-envelope-test
  (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
    (let [inspect @#'introspection/foundation-inspect
          result  (inspect {:conversation-id nil :db-info nil})]
      (expect (extension/tool-result? result))
      (expect (= :v/conversation-state (:symbol result)))
      (expect (map? (:result result))))))
