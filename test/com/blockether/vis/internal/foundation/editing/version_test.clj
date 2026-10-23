(ns com.blockether.vis.internal.foundation.editing.version-test
  (:require [com.blockether.vis.internal.foundation.editing.version :as version]
            [lazytest.core :refer [defdescribe describe it expect]]))

(defdescribe
  content-version-test
  (it "is the first 8 hex chars of the SHA-256 of the UTF-8 text"
      ;; sha256("") = e3b0c442…; sha256("abc") = ba7816bf…
      (expect (= "e3b0c442" (version/content-version "")))
      (expect (= "ba7816bf" (version/content-version "abc"))))
  (it "changes with any byte, line endings included"
      (expect (not= (version/content-version "a\nb\n") (version/content-version "a\r\nb\r\n")))
      (expect (not= (version/content-version "a\nb\n") (version/content-version "a\nb"))))
  (it "parses a version with or without the leading @, in any case, and nothing else"
      (expect (= "3f9a1c2b" (version/parse-version "3f9a1c2b")))
      (expect (= "3f9a1c2b" (version/parse-version "@3f9a1c2b")))
      (expect (= "3f9a1c2b" (version/parse-version " @3f9a1c2b ")))
      (expect (= "3f9a1c2b" (version/parse-version "3F9A1C2B")))
      (expect (nil? (version/parse-version "3f9a1c2")))
      (expect (nil? (version/parse-version "12:abc")))
      (expect (nil? (version/parse-version nil)))
      (expect (nil? (version/parse-version 12)))))

(defdescribe render-numbered-block-test
             (it "prints a header and one `N│ text` row per line"
                 (expect (= "src/a.clj @3f9a1c2b" (version/header "src/a.clj" "3f9a1c2b")))
                 (expect (= "1│ alpha\n2│ beta"
                            (version/render-numbered-block [[1 "alpha"] [2 "beta"]]))))
             (it "prints each number as it is, without padding, and strips a CR"
                 (expect (= "9│ a\n10│ b" (version/render-numbered-block [[9 "a\r"] [10 "b"]]))))
             (it "indents every row when asked"
                 (expect (= "  7│ x" (version/render-numbered-block [[7 "x"]] "  ")))))

(defdescribe version-store-test
             (it "recalls the text of a remembered version and forgets it on demand"
                 (version/forget-all!)
                 (let [v (version/remember! "/tmp/a.txt" "one\ntwo\n")]
                   (expect (= (version/content-version "one\ntwo\n") v))
                   (expect (= "one\ntwo\n" (version/recall "/tmp/a.txt" v)))
                   (expect (nil? (version/recall "/tmp/b.txt" v)))
                   (version/forget-all!)
                   (expect (nil? (version/recall "/tmp/a.txt" v)))))
             (it "keys each version by path, so one text under two paths stays apart"
                 (version/forget-all!)
                 (let [v (version/remember! "/tmp/a.txt" "same\n")]
                   (expect (= v (version/remember! "/tmp/b.txt" "same\n")))
                   (expect (= "same\n" (version/recall "/tmp/a.txt" v)))
                   (expect (= "same\n" (version/recall "/tmp/b.txt" v)))
                   (version/forget-all!))))

(defdescribe
  edit-span-test
  (describe
    "line replacement keeps the line terminator"
    (it "on LF"
        (let [content
              "a\nb\nc\n"

              {:keys [start end replacement]}
              (version/edit-span content 2 2 "B")]

          (expect (= "a\nB\nc\n" (str (subs content 0 start) replacement (subs content end))))))
    (it "on CRLF"
        (let [content
              "a\r\nb\r\nc\r\n"

              {:keys [start end replacement]}
              (version/edit-span content 2 2 "B")]

          (expect (= "a\r\nB\r\nc\r\n"
                     (str (subs content 0 start) replacement (subs content end))))))
    (it "without doubling a trailing newline in the replacement"
        (let [content
              "a\nb\n"

              {:keys [start end replacement]}
              (version/edit-span content 1 1 "A\n")]

          (expect (= "A\nb\n" (str (subs content 0 start) replacement (subs content end)))))))
  (it "an empty replacement deletes whole lines"
      (let [content
            "a\nb\nc\n"

            {:keys [start end replacement]}
            (version/edit-span content 2 3 "")]

        (expect (= "a\n" (str (subs content 0 start) replacement (subs content end)))))))

(defdescribe shown-lines-test
             (it "merges overlapping and adjacent ranges of one version"
                 (version/forget-all!)
                 (version/note-shown! "/tmp/shown.txt" "0000000a" [[5 7] [1 2] [3 4] [9 9]])
                 (expect (= [[1 7] [9 9]] (version/shown-ranges "/tmp/shown.txt" "0000000a")))
                 (expect (nil? (version/shown-ranges "/tmp/shown.txt" "0000000b"))))
             (it "merges a later range into an open range that ends at Long/MAX_VALUE"
                 ;; A text patch without a read records [1 Long/MAX_VALUE]; the next cat or
                 ;; grep of that version failed with `long overflow`.
                 (version/note-shown! "/tmp/shown.txt" "0000000d" [[1 Long/MAX_VALUE]])
                 (version/note-shown! "/tmp/shown.txt" "0000000d" [[20 25]])
                 (expect (= [[1 Long/MAX_VALUE]]
                            (version/shown-ranges "/tmp/shown.txt" "0000000d"))))
             (it "covers a span only when one range holds all of its lines"
                 (expect (version/covers? [[1 7] [9 9]] 2 7))
                 (expect (not (version/covers? [[1 7] [9 9]] 7 9)))
                 (expect (not (version/covers? [] 1 1))))
             (it "ignores empty and inverted ranges, and forget-all! drops the record"
                 (version/note-shown! "/tmp/shown.txt" "0000000c" [[0 0] [3 2]])
                 (expect (nil? (version/shown-ranges "/tmp/shown.txt" "0000000c")))
                 (version/forget-all!)
                 (expect (nil? (version/shown-ranges "/tmp/shown.txt" "0000000a")))))
