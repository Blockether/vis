(ns com.blockether.vis.ext.channel-tui.selection-test
  (:require [com.blockether.vis.ext.channel-tui.selection :as selection]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe selection-ranges-test
  (it "normalizes reverse drags into top-left to bottom-right ranges"
    (expect (= [{:row 1 :col 2 :width 3}]
              (selection/selected-ranges
                {:anchor (selection/point 4 1)
                 :focus  (selection/point 2 1)}
                10 5))))

  (it "spans multiple rows using full middle rows"
    (expect (= [{:row 1 :col 3 :width 2}
                {:row 2 :col 0 :width 5}
                {:row 3 :col 0 :width 2}]
              (selection/selected-ranges
                {:anchor (selection/point 3 1)
                 :focus  (selection/point 1 3)}
                5 5))))

  (it "clips stale coordinates to the current screen size"
    (expect (= [{:row 0 :col 0 :width 4}
                {:row 1 :col 0 :width 4}]
              (selection/selected-ranges
                {:anchor (selection/point -5 -2)
                 :focus  (selection/point 99 99)}
                4 2)))))

(defdescribe selected-text-test
  (it "extracts selected visible text and trims screen padding"
    (expect (= "llo\nworld"
              (selection/selected-text
                ["hello     "
                 "world     "]
                {:anchor (selection/point 2 0)
                 :focus  (selection/point 4 1)}))))

  (it "preserves leading indentation inside selected code"
    (expect (= "  (foo)"
              (selection/selected-text
                [[" " " " "(" "f" "o" "o" ")" " " " "]]
                {:anchor (selection/point 0 0)
                 :focus  (selection/point 8 0)})))))
