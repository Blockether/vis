(ns com.blockether.vis.internal.foundation.editing.diff-test
  (:require [com.blockether.vis.internal.foundation.editing.diff :as diff]
            [lazytest.core :refer [defdescribe describe it expect]]))

(def ^:private old-lines ["a" "b" "c" "d" "e"])

(defdescribe
  remap-line-span-test
  (it "keeps the span when nothing changed"
      (expect (= {:from 2 :to 3 :shift 0} (diff/remap-line-span old-lines old-lines 2 3))))
  (describe "a change outside the span only moves it"
            (it "lines added before shift it down"
                (expect (= {:from 4 :to 5 :shift 2}
                           (diff/remap-line-span old-lines ["x" "y" "a" "b" "c" "d" "e"] 2 3))))
            (it "lines removed before shift it up"
                (expect (= {:from 1 :to 2 :shift -1}
                           (diff/remap-line-span old-lines ["b" "c" "d" "e"] 2 3))))
            (it "a change after it leaves it in place"
                (expect (= {:from 2 :to 3 :shift 0}
                           (diff/remap-line-span old-lines ["a" "b" "c" "D" "E" "F"] 2 3))))
            (it "an insert at its first line lands before it"
                (expect (= {:from 3 :to 4 :shift 1}
                           (diff/remap-line-span old-lines ["a" "x" "b" "c" "d" "e"] 2 3))))
            (it "an insert right after its last line lands after it"
                (expect (= {:from 2 :to 3 :shift 0}
                           (diff/remap-line-span old-lines ["a" "b" "c" "x" "d" "e"] 2 3)))))
  (describe "a change of the span refuses"
            (it "an edited line inside"
                (expect (= {:error :span-changed}
                           (diff/remap-line-span old-lines ["a" "B" "c" "d" "e"] 2 3))))
            (it "a removed line inside"
                (expect (= {:error :span-changed}
                           (diff/remap-line-span old-lines ["a" "b" "d" "e"] 2 3))))
            (it "an insert strictly inside"
                (expect (= {:error :span-changed}
                           (diff/remap-line-span old-lines ["a" "b" "x" "c" "d" "e"] 2 3))))
            (it "a change that overlaps its first line"
                (expect (= {:error :span-changed}
                           (diff/remap-line-span old-lines ["A" "B" "c" "d" "e"] 2 3))))))
