(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe query-options-test
  (it "forwards reasoning-default and extra-body to vis/send!"
    (let [seen (atom nil)]
      (with-redefs [vis/send! (fn [_id _text opts]
                                (reset! seen opts)
                                {:answer "ok"})]
        (let [result (chat/query! {:id "c1"} "hello"
                       {:reasoning-default :deep
                        :extra-body {:text {:verbosity "high"}}})]
          (expect (= "ok" (:answer result)))
          (expect (= 1 (:iteration-count result)))
          (expect (= :deep (:reasoning-default @seen)))
          (expect (= {:text {:verbosity "high"}} (:extra-body @seen))))))))
