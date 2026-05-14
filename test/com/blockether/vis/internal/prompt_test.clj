(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe prompt-budget-trimming-test
  (it "drops an oversized first journal line instead of exceeding budget"
    (with-redefs [prompt/count-prompt-tokens (fn [_ _] 10)]
      (let [[lines dropped budget used]
            (#'prompt/trim-journal-lines nil ["i1.0 giant"] 1)]
        (expect (= [] lines))
        (expect (= 1 dropped))
        (expect (= 1 budget))
        (expect (= 0 used)))))

  (it "replaces an oversized first bindings entry with an omission marker"
    (with-redefs [prompt/count-prompt-tokens (fn [_ _] 10)]
      (let [out (#'prompt/trim-bindings-str nil ";; v=big\n(def BIG 1)" 1)]
        (expect (str/includes? out "<bindings> entries omitted"))
        (expect (false? (str/includes? out "def BIG")))))))
