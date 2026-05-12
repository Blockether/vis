(ns com.blockether.vis.internal.format-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.format :as fmt]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe format-test
  (it "formats iteration metadata with silent count beside iteration count"
    (expect (= "3 iters (2 silent)"
              (fmt/format-meta-line {:iteration-count 3
                                     :silent-count 2})))
    (expect (= "1 iter"
              (fmt/format-meta-line {:iteration-count 1
                                     :silent-count 0}))))

  (it "smoke formats tokens, cost, and duration in one meta line"
    (let [line (fmt/format-meta-line {:iteration-count 1
                                      :tokens {:input 10 :cached 4 :output 2}
                                      :cost {:total-cost 0.1}
                                      :duration-ms 1200})]
      (expect (str/includes? line "1 iter"))
      (expect (str/includes? line "↑10 (cached 4) ↓2"))
      (expect (str/includes? line "~$0.100000"))
      (expect (str/includes? line "1.2s")))))
