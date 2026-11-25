(ns com.blockether.vis.tui.columns-test
  "Shared row-fit boundaries for Live and Ask layouts."
  (:require [com.blockether.vis.tui.columns :as columns]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe readable-row-test
             (it "children share a row only when each has 24 text columns"
                 (doseq [[children width] [[1 26] [2 54] [3 82]]]
                   (expect (false? (columns/row-fits? (dec width) children)))
                   (expect (true? (columns/row-fits? width children)))
                   (expect (= 24 (columns/cell-width width children)))))
             (it "unbounded measurement does not force columns to stack"
                 (expect (true? (columns/row-fits? nil 2))))
             (it "empty groups and narrow widths do not split"
                 (doseq [width [nil 0 10 80]]
                   (expect (false? (columns/row-fits? width 0))))
                 (doseq [width [0 1 4 20]]
                   (expect (false? (columns/row-fits? width 2))))))

(defdescribe width-slots-test
             (it "gives each cell its text and chrome, the last cell the remainder"
                 (expect (= [[0 12] [12 12] [24 16]] (columns/width-slots 40 [8 8 8]))))
             (it "gives a cell past the right edge no width"
                 (expect (= [[0 10] [10 0]] (columns/width-slots 10 [20 20])))))

(defdescribe natural-width-test
             (it "is the widest row, capped at the text width"
                 (expect (= 12 (columns/natural-width 80 [3 12 7])))
                 (expect (= 40 (columns/natural-width 40 [90])))
                 (expect (= 1 (columns/natural-width 40 [])))))
