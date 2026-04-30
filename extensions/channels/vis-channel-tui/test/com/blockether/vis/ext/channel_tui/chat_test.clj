(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe query-options-test
  (it "forwards reasoning-default to vis/send!"
    (let [seen (atom nil)]
      (with-redefs [vis/send! (fn [_id _text opts]
                                (reset! seen opts)
                                {:answer "ok"})]
        (let [result (chat/query! {:id "c1"} "hello" {:reasoning-default :deep})]
          (expect (= "ok" (:answer result)))
          (expect (= 1 (:iteration-count result)))
          (expect (= :deep (:reasoning-default @seen))))))))
