(ns com.blockether.vis.internal.registry-test
  (:require
   [com.blockether.vis.internal.registry :as registry]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-descriptor-test
  (it "builds minimal provider descriptors"
    (expect (= {:provider/id :demo
                :provider/label "Demo"}
              (registry/provider {:provider/id :demo
                                  :provider/label "Demo"}))))

  (it "rejects removed provider-specific prompt slots"
    (let [thrown (try
                   (registry/provider {:provider/id :demo
                                       :provider/label "Demo"
                                       :provider/prompt-fn (constantly "nope")})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :provider/invalid-spec (:type (ex-data thrown)))))))
