(ns com.blockether.vis.internal.presentation-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.presentation :as ps]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe presentation-test
  (it "renders basic presentation kinds"
    (expect (= "hello" (ps/present (ps/markdown "hello"))))
    (expect (str/includes? (ps/present (ps/details "More" "Body")) "<details>"))
    (expect (str/includes? (ps/present (ps/provider-error {:message "bad"})) "bad"))
    (expect (str/includes? (ps/present (ps/mermaid {:source "graph TD; A-->B"})) "```mermaid")))

  (it "keeps the recognized kind set small"
    (expect (contains? ps/presentation-kinds :vis.presentation/markdown))))
