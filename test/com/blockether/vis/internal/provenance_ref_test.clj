(ns com.blockether.vis.internal.provenance-ref-test
  (:require
   [com.blockether.vis.internal.provenance-ref :as prov-ref]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provenance-reference-test
  (it "parses canonical turn block references"
    (expect (= {:scope :turn
                :turn-prefix "3f2a91c0"
                :iteration 4
                :block 2}
              (prov-ref/parse-ref "turn/3f2a91c0/iteration/4/block/2"))))

  (it "parses canonical conversation-scoped tool references"
    (expect (= {:scope :conversation
                :conversation-prefix "9ab331ef"
                :turn-prefix "3f2a91c0"
                :iteration 4
                :block 2
                :child {:kind :tool :op "bash"}}
              (prov-ref/parse-ref "conversation/9ab331ef/turn/3f2a91c0/iteration/4/block/2/tool/bash"))))

  (it "rejects compact and invented aliases"
    (doseq [ref ["i4.2" "i4.2/tool" "E1" "G1" "turn/nope/iteration/1/block/1"]]
      (expect (nil? (prov-ref/parse-ref ref)))
      (expect (false? (prov-ref/canonical-ref? ref)))))

  (it "formats canonical refs from data"
    (expect (= "turn/3f2a91c0/iteration/4/block/2"
              (prov-ref/format-ref {:turn-prefix "3F2A91C0" :iteration 4 :block 2})))
    (expect (= "turn/3f2a91c0/iteration/4/block/2/tool/v.bash"
              (prov-ref/format-ref {:turn-prefix "3f2a91c0"
                                    :iteration 4
                                    :block 2
                                    :child {:kind :tool :op "v.bash"}}))))

  (it "returns display labels without changing the canonical copy form"
    (expect (= {:canonical "turn/3f2a91c0/iteration/4/block/2/tool/bash"
                :label "T3f2a91c0 · i4.2 · bash"
                :short "i4.2/bash"
                :markdown "`T3f2a91c0 · i4.2 · bash`"}
              (prov-ref/display-ref "turn/3f2a91c0/iteration/4/block/2/tool/bash")))))
