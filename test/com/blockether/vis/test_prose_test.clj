(ns com.blockether.vis.test-prose-test
  "The shared prose limits for reader-facing text."
  (:require [clojure.string :as str]
            [com.blockether.vis.test-prose :as prose]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe text-units-test
             (it "ignores markup-only HTML without hiding prose inside HTML"
                 (let [md (str "<div class=\"store-links\">\n"
                               "<a href=\"/download\"><img src=\"badge.png\"></a>\n" "</div>\n\n"
                               "<p>Readable copy.</p>\n" "Continues here.\n")]
                   (expect (= [[5 "<p>Readable copy.</p> Continues here."]]
                              (prose/text-units md))))))

(defdescribe
  breaks-test
  (it "accepts short plain sentences and counts a code span as one word"
      (expect (empty? (prose/breaks
                        "Call `grep({\"query\": q, \"paths\": [\"src\"]})` first. It is fast."))))
  (it "names a long sentence, a crowded paragraph and a semicolon"
      (let [long-sentence
            (str (str/join " " (repeat 26 "word")) ".")

            crowded
            (str/join " " (repeat 7 "One sentence."))]

        (expect (= 1 (count (prose/breaks long-sentence))))
        (expect (re-find #"runs 26 words" (first (prose/breaks long-sentence))))
        (expect (re-find #"holds 7 sentences" (first (prose/breaks crowded))))
        (expect (re-find #"semicolon" (first (prose/breaks "One clause; another clause."))))))
  (it "names a hard word with its simpler word, outside code spans and link targets"
      (expect (re-find #"uses \"e\.g\.\" — write \"for example\""
                       (first (prose/breaks "Name a path, e.g. a file."))))
      (expect (re-find #"uses \"utilized\" — write \"use\""
                       (first (prose/breaks "The gateway utilized the cache."))))
      (expect (empty? (prose/breaks "Call `ensure_dir()` and read [the guide](via.md).")))))
