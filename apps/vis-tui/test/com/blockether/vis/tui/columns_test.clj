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

(defdescribe grid-span-test
             (it "takes as many equal columns as fit, never more than the children"
                 (expect (= 3 (columns/grid-span 82 5 nil)))
                 (expect (= 2 (columns/grid-span 2000 2 nil))))
             (it "never exceeds max-columns" (expect (= 2 (columns/grid-span 200 9 2))))
             (it "falls to one column, a stack, when no two cells fit"
                 (expect (= 1 (columns/grid-span 40 6 nil)))
                 (expect (= 0 (columns/grid-span 80 0 nil)))))

(defdescribe grid-chunks-test
             (it "pads the last row so its columns line up with the rows above"
                 (expect (= [[[:a] [:b]] [[:c] []]] (columns/grid-chunks [[:a] [:b] [:c]] 2)))))

(defdescribe wrap-lines-test
             (it "packs children left to right and wraps where the next does not fit"
                 ;; Three cells of 8 cost 8+4+8+4+8 = 32 text columns, inside 34.
                 (expect (= [[0 1 2]] (columns/wrap-lines 34 [8 8 8])))
                 (expect (= [[0 1] [2]] (columns/wrap-lines 33 [8 8 8]))))
             (it "gives a child wider than the line a line of its own"
                 (expect (= [[0] [1] [2]] (columns/wrap-lines 40 [10 60 10]))))
             (it "keeps an unbounded plan on one line"
                 (expect (= [[0 1 2]] (columns/wrap-lines nil [80 80 80])))))

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
