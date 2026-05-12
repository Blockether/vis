(ns com.blockether.vis.internal.render-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.render :as render]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe answer-ir-lazy-seq-test
  (it "realizes lazy Hiccup children before canonicalization and caps infinite seqs"
    (let [ast (render/->ast
                [:ir [:ul (map (fn [n] [:li (str "item " n)]) (range))]])
          items (->> (nth ast 2) (drop 2) vec)
          rendered (render/render ast :plain)
          printed (pr-str ast)]
      (expect (= :ir (first ast)))
      (expect (= 101 (count items)))
      (expect (str/includes? rendered "item 0"))
      (expect (str/includes? rendered "item 99"))
      (expect (not (str/includes? rendered "item 100")))
      (expect (str/includes? rendered "… many more"))
      (expect (not (str/includes? printed "LazySeq"))))))
