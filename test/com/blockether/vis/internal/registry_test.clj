(ns com.blockether.vis.internal.registry-test
  (:require [com.blockether.vis.internal.registry :as registry]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe provider-builder-test
  (it "accepts provider descriptors with an optional :provider/limits-fn"
    (let [provider (registry/provider
                     {:provider/id :demo
                      :provider/label "Demo"
                      :provider/limits-fn (fn [] {:provider-id :demo})})]
      (expect (= :demo (:provider/id provider)))
      (expect (ifn? (:provider/limits-fn provider)))))

  (it "still rejects invalid provider descriptors"
    (expect (throws? clojure.lang.ExceptionInfo
              #(registry/provider {:provider/id :demo})))))
