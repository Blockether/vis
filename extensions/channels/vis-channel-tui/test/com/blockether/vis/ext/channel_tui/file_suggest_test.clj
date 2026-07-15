(ns com.blockether.vis.ext.channel-tui.file-suggest-test
  (:require [com.blockether.vis.ext.channel-tui.file-suggest :as suggest]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe file-mention-trigger-test
             (it "detects an active @ file mention at the caret"
                 (expect (= {:query "" :at 5} (suggest/mention-at "open @")))
                 (expect (= {:query "src/com" :at 5} (suggest/mention-at "open @src/com"))))
             (it "stops suggesting once the @ token is followed by whitespace"
                 (expect (nil? (suggest/mention-at "open @ ")))
                 (expect (nil? (suggest/mention-at "open @src "))))
             (it "does not treat @@ as a file sigil"
                 (expect (nil? (suggest/mention-at "literal @@")))))
