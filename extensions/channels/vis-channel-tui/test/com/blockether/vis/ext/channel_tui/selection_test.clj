(ns com.blockether.vis.ext.channel-tui.selection-test
  (:require [com.blockether.vis.ext.channel-tui.selection :as selection]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe selection-ranges-test
             (it "normalizes reverse drags into top-left to bottom-right ranges"
                 (expect (= [{:row 1 :col 2 :width 3}]
                            (selection/selected-ranges {:anchor (selection/point 4 1)
                                                        :focus (selection/point 2 1)}
                                                       10
                                                       5))))
             (it "spans multiple rows using full middle rows"
                 (expect (= [{:row 1 :col 3 :width 2} {:row 2 :col 0 :width 5}
                             {:row 3 :col 0 :width 2}]
                            (selection/selected-ranges {:anchor (selection/point 3 1)
                                                        :focus (selection/point 1 3)}
                                                       5
                                                       5))))
             (it "clips stale coordinates to the current screen size"
                 (expect (= [{:row 0 :col 0 :width 4} {:row 1 :col 0 :width 4}]
                            (selection/selected-ranges {:anchor (selection/point -5 -2)
                                                        :focus (selection/point 99 99)}
                                                       4
                                                       2))))
             (it "fills visible rows when the selection starts above the viewport"
                 (expect (= [{:row 0 :col 0 :width 10} {:row 1 :col 0 :width 3}]
                            (selection/selected-ranges {:anchor (selection/point 5 -10)
                                                        :focus (selection/point 2 1)}
                                                       10
                                                       4)))))

(defdescribe selected-text-test
             (it "extracts selected visible text and trims screen padding"
                 (expect (= "llo\nworld"
                            (selection/selected-text ["hello     " "world     "]
                                                     {:anchor (selection/point 2 0)
                                                      :focus (selection/point 4 1)}))))
             (it "preserves leading indentation inside selected code"
                 (expect (= "  (foo)"
                            (selection/selected-text [[" " " " "(" "f" "o" "o" ")" " " " "]]
                                                     {:anchor (selection/point 0 0)
                                                      :focus (selection/point 8 0)}))))
             (it "copies only selectable bubble ranges when a drag crosses TUI chrome"
                 (expect (= "bubble"
                            (selection/selected-text ["HEADER    " "  bubble  " "INPUT     "]
                                                     {:anchor (selection/point 0 0)
                                                      :focus (selection/point 9 2)}
                                                     [{:row 1 :col 2 :width 6}]))))
             (it "strips Vis render markers and ANSI escapes from copied text"
                 (expect (= "(def x 1)\nplain ok"
                            (selection/selected-text
                              ["\u2061\u001B[32m(def\u001B[0m x 1)\uE000   "
                               "\u206Eplain \u241B[31mok\u241B[0m\uE110\uE111  "]
                              {:anchor (selection/point 0 0) :focus (selection/point 40 1)}))))
             (it "cleans whole-bubble copy payloads before they can be pasted back"
                 (expect (= "(def x 1)\nplain ok\nred bare"
                            (selection/clean-copied-text
                              (str "\u001B[32m(def\u001B[0m x 1)\uE000   \r\n"
                                   "\u241B]52;c;AAAA\u0007plain \u241B[31mok\u241B[0m\uE111  \n"
                                   "[31mred[0m bare  "))))))

(defdescribe
  selectable-ranges-test
  (it "reports whether a point is inside a selectable bubble range"
      (expect (selection/point-in-ranges? (selection/point 3 1) [{:row 1 :col 2 :width 6}]))
      (expect (not (selection/point-in-ranges? (selection/point 1 1) [{:row 1 :col 2 :width 6}]))))
  (it "accepts selection starts in a small vertical comfort zone around bubbles"
      (expect (selection/point-in-ranges? (selection/point 3 3)
                                          [{:row 5 :col 2 :width 6}]
                                          {:row-padding 2}))
      (expect (selection/point-in-ranges? (selection/point 3 7)
                                          [{:row 5 :col 2 :width 6}]
                                          {:row-padding 2}))
      (expect (not (selection/point-in-ranges? (selection/point 3 2)
                                               [{:row 5 :col 2 :width 6}]
                                               {:row-padding 2}))))
  (it "keeps the comfort zone within the bubble columns"
      (expect (not (selection/point-in-ranges? (selection/point 1 5)
                                               [{:row 5 :col 2 :width 6}]
                                               {:row-padding 2})))))

(defdescribe
  click-selection-test
  (it "finds the same selection source for input and transcript rows"
      (expect (= :input
                 (selection/source-at-point (selection/point 4 8)
                                            [{:row 3 :col 2 :width 10}]
                                            [{:row 8 :col 2 :width 10}]
                                            {:row-padding 1})))
      (expect (= :transcript
                 (selection/source-at-point (selection/point 4 3)
                                            [{:row 3 :col 2 :width 10}]
                                            [{:row 8 :col 2 :width 10}]
                                            {:row-padding 1}))))
  (it "detects a rapid second click on the same source row"
      (let [last-click {:source :input :point (selection/point 3 8) :time-ms 1000}]
        (expect (selection/double-click? last-click 1200 :input (selection/point 9 8) 500))
        (expect (not
                  (selection/double-click? last-click 1200 :transcript (selection/point 9 8) 500)))
        (expect (not (selection/double-click? last-click 1200 :input (selection/point 9 9) 500)))
        (expect (not (selection/double-click? last-click 1700 :input (selection/point 9 8) 500)))))
  (it "expands a double-click to the whole selectable line"
      (expect (= {:anchor (selection/point 2 11) :focus (selection/point 7 11)}
                 (selection/line-selection-at-point (selection/point 5 4)
                                                    [{:row 4 :col 2 :width 6}]
                                                    {:viewport-top 2 :eff-scroll 9})))))

(defdescribe document-selection-test
             (it "projects document-space selection into the current viewport"
                 (expect (= {:anchor (selection/point 4 2) :focus (selection/point 8 6)}
                            (selection/document->screen-selection
                              {:anchor (selection/point 4 10) :focus (selection/point 8 14)}
                              {:viewport-top 3 :eff-scroll 11}))))
             (it "converts screen points to document rows using the effective scroll"
                 (expect (= (selection/point 7 20)
                            (selection/screen->document-point (selection/point 7 8)
                                                              {:viewport-top 3 :eff-scroll 15})))))

(defdescribe
  auto-scroll-test
  (it "requests upward or downward scroll while selection is dragged at viewport edges"
      (expect (= :up
                 (selection/auto-scroll-direction (selection/point 5 3)
                                                  {:top 3 :bottom 10 :edge-size 1})))
      (expect (= :down
                 (selection/auto-scroll-direction (selection/point 5 9)
                                                  {:top 3 :bottom 10 :edge-size 1})))
      (expect (nil? (selection/auto-scroll-direction (selection/point 5 6)
                                                     {:top 3 :bottom 10 :edge-size 1}))))
  (it "scrolls faster the deeper the drag is in the edge zone"
      (expect (= {:direction :up :amount 4}
                 (selection/auto-scroll-step (selection/point 5 3)
                                             {:top 3 :bottom 20 :edge-size 4 :max-step 6})))
      (expect (= {:direction :up :amount 1}
                 (selection/auto-scroll-step (selection/point 5 6)
                                             {:top 3 :bottom 20 :edge-size 4 :max-step 6})))
      (expect (= {:direction :down :amount 4}
                 (selection/auto-scroll-step (selection/point 5 19)
                                             {:top 3 :bottom 20 :edge-size 4 :max-step 6})))))
