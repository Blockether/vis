(ns com.blockether.vis.ext.foundation.introspection-test
  (:require
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe introspection-public-surface-test
  (it "exposes only inspect/report/docs symbols after workflow cleanup"
    (let [symbols (set (map :sym introspection/all-symbols))]
      (expect (contains? symbols 'inspect))
      (expect (contains? symbols 'report))
      (expect (contains? symbols 'extensions)))))
