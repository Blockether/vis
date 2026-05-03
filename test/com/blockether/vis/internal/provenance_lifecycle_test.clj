(ns com.blockether.vis.internal.provenance-lifecycle-test
  (:require
   [com.blockether.vis.internal.provenance-lifecycle :as lifecycle]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provenance-lifecycle-test
  (it "allocates canonical block and child refs"
    (let [parent (lifecycle/block-ref {:turn-prefix "3f2a91c0" :iteration 4 :block 2})]
      (expect (= "turn/3f2a91c0/iteration/4/block/2" parent))
      (expect (= "turn/3f2a91c0/iteration/4/block/2/tool/v.bash"
                (lifecycle/child-ref parent {:op :v/bash})))
      (expect (= "turn/3f2a91c0/iteration/4/block/2/error"
                (lifecycle/child-ref parent {:kind :error})))))

  (it "models running then terminal lifecycle events"
    (let [started (lifecycle/start-event
                    {:ref "turn/3f2a91c0/iteration/4/block/2/tool/job-7"
                     :parent-ref "turn/3f2a91c0/iteration/4/block/2"
                     :op :v/async-tool
                     :rendering-kind :vis/tool})
          finished (lifecycle/finish-event started {:status :done})]
      (expect (= :running (get-in started [:provenance :status])))
      (expect (= :done (get-in finished [:provenance :status])))
      (expect (false? (lifecycle/proof-compatible? started)))
      (expect (true? (lifecycle/proof-compatible? finished)))))

  (it "does not allow running events as blocker evidence either"
    (let [started (lifecycle/start-event
                    {:ref "turn/3f2a91c0/iteration/4/block/2/tool/job-7"
                     :parent-ref "turn/3f2a91c0/iteration/4/block/2"
                     :op :v/async-tool
                     :rendering-kind :vis/tool})]
      (expect (false? (lifecycle/blocker-compatible? started)))
      (expect (true? (lifecycle/blocker-compatible?
                       (lifecycle/finish-event started {:status :timeout})))))))
