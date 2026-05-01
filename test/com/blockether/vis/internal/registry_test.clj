(ns com.blockether.vis.internal.registry-test
  (:require [com.blockether.vis.internal.registry :as registry]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe provider-builder-test
  (it "accepts provider descriptors with optional runtime hooks"
    (let [provider (registry/provider
                     {:provider/id :demo
                      :provider/label "Demo"
                      :provider/limits-fn (fn [] {:provider-id :demo})
                      :provider/on-selected-fn (fn [_ctx] nil)
                      :provider/prompt-fn (fn [_ctx] "provider prompt")})]
      (expect (= :demo (:provider/id provider)))
      (expect (ifn? (:provider/limits-fn provider)))
      (expect (ifn? (:provider/on-selected-fn provider)))
      (expect (ifn? (:provider/prompt-fn provider)))))

  (it "still rejects invalid provider descriptors"
    (expect (throws? clojure.lang.ExceptionInfo
              #(registry/provider {:provider/id :demo})))))
