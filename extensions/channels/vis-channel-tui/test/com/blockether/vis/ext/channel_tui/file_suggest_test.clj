(ns com.blockether.vis.ext.channel-tui.file-suggest-test
  (:require [com.blockether.vis.ext.channel-tui.file-suggest :as suggest]
            [com.blockether.vis.internal.file-picker :as picker]
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

(defdescribe
  file-mention-suggestions-test
  (let [st
        {:lines ["open @fpick"] :crow 0 :ccol 11}

        rows
        [{:path "src/a/file_picker.clj"
          :size-label "5.7K"
          :age-label "now"
          :status-label "modified"}
         {:path "src/b/other.clj" :size-label "1B" :age-label "5d" :status-label "clean"}]]

    (it "shows NOTHING while the pooled fff index is still cold (never blocks a keystroke)"
        (with-redefs [picker/index-warm?
                      (constantly false)

                      picker/prewarm-index!
                      (fn []
                        nil)

                      picker/fuzzy-file-rows
                      (fn [& _]
                        (throw (AssertionError. "must not search a cold index")))]

          (expect (nil? (suggest/suggestions st 0)))))
    (it "searches the POOLED index once warm and shapes rows for the slash overlay"
        (with-redefs [picker/index-warm?
                      (constantly true)

                      picker/fuzzy-file-rows
                      (fn [q _]
                        (expect (= "fpick" q))
                        rows)]

          (let [s (vec (suggest/suggestions st 1))]
            (expect (= 2 (count s)))
            (expect (= "src/a/file_picker.clj" (:file/path (first s))))
            (expect (= "src/a/file_picker.clj" (:slash/usage (first s))))
            ;; git status collapses to git's own single letter; "clean" is hidden
            (expect (= "5.7K · now · M" (:label (first s))))
            (expect (= "1B · 5d" (:label (second s))))
            (expect (false? (:slash/selected? (first s))))
            (expect (true? (:slash/selected? (second s)))))))
    (it "an out-of-range selection clamps instead of throwing"
        (with-redefs [picker/index-warm?
                      (constantly true)

                      picker/fuzzy-file-rows
                      (fn [_ _]
                        rows)]

          (expect (= [false true] (mapv :slash/selected? (suggest/suggestions st 99))))))
    (it "picking splices a visible mention plus a trailing space at the caret"
        (expect (= {:lines ["open @src/a/file_picker.clj "] :crow 0 :ccol 28}
                   (suggest/apply-mention st "src/a/file_picker.clj"))))
    (it "picking with no active mention leaves the input untouched"
        (let [plain {:lines ["hello"] :crow 0 :ccol 5}]
          (expect (= plain (suggest/apply-mention plain "a.clj")))))))
