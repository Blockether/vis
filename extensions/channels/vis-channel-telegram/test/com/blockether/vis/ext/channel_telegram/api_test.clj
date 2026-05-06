(ns com.blockether.vis.ext.channel-telegram.api-test
  (:require [com.blockether.vis.ext.channel-telegram.api :as tg]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe markdown-escape-test
  (it "escapes Telegram MarkdownV2 specials in plain text"
    (expect (= "a\\_b\\! \\(x\\)"
              (tg/escape-markdown-v2 "a_b! (x)")))))
