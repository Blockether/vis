(ns com.blockether.vis.tui.columns-test
  "Shared row-fit boundaries for Live and Ask layouts."
  (:require [com.blockether.vis.tui.columns :as columns]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest readable-row-test
  (testing "children share a row only when each has 24 text columns"
    (doseq [[children width] [[1 26] [2 54] [3 82]]]
      (is (false? (columns/row-fits? (dec width) children)))
      (is (true? (columns/row-fits? width children)))
      (is (= 24 (columns/cell-width width children)))))
  (testing "unbounded measurement does not force columns to stack"
    (is (true? (columns/row-fits? nil 2))))
  (testing "empty groups and narrow widths do not split"
    (doseq [width [nil 0 10 80]]
      (is (false? (columns/row-fits? width 0))))
    (doseq [width [0 1 4 20]]
      (is (false? (columns/row-fits? width 2))))))
