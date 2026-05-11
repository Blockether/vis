(ns com.blockether.vis.internal.presentation-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.presentation :as ps]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe presentation-test
  (it "renders basic presentation kinds"
    ;; The `:details` kind is now rendered as a markdown
    ;; **summary** + body block (not a HTML <details> tag) —
    ;; matches the IR pipeline (see internal/render.clj :details
    ;; tag handling). Tests pin the markdown shape.
    (expect (= "hello" (ps/present (ps/markdown "hello"))))
    (let [d (ps/present (ps/details "More" "Body"))]
      (expect (str/includes? d "More"))
      (expect (str/includes? d "Body")))
    (expect (str/includes? (ps/present (ps/provider-error {:message "bad"})) "bad"))
    (expect (str/includes? (ps/present (ps/mermaid {:source "graph TD; A-->B"})) "```mermaid")))

  (it "keeps the recognized kind set small"
    (expect (contains? ps/presentation-kinds :vis.presentation/markdown))))
