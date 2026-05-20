(ns com.blockether.vis.internal.paren-repair-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.paren-repair :as paren-repair]
   [lazytest.core :refer [defdescribe it expect]]))

(defdescribe delimiter-error-test
  (it "detects delimiter-shaped parse errors"
    (expect (true? (paren-repair/delimiter-error? "(def x 1))"))))

  (it "does not flag valid code"
    (expect (false? (paren-repair/delimiter-error? "(def x 1)")))))

(defdescribe parinfer-repair-test
  (it "removes stray close delimiters when repaired text parses"
    (expect (= "(def x 1)"
              (paren-repair/parinfer-repair "(def x 1))"))))

  (it "adds missing close delimiters when indentation makes repair clear"
    (expect (= "(def x 1)"
              (paren-repair/parinfer-repair "(def x 1"))))

  (it "repairs delimiter slips inside direct-answer IR blocks"
    (let [src "(done [:ir [:p \"Hotword biasing - add \" [:c \"setHotwordsFile\") \"/\" [:c \"setHotwordsScore\"]]])"
          repaired (paren-repair/parinfer-repair src)]
      (expect (string? repaired))
      (expect (str/includes? repaired "\"setHotwordsFile\" \"/\""))
      (expect (false? (paren-repair/delimiter-error? repaired)))))

  (it "returns nil when quote damage makes Parinfer unsafe"
    (expect (nil? (paren-repair/parinfer-repair "(println \"a\" broken \"b)")))))

(defdescribe maybe-repair-delimiters-test
  (it "only attempts delimiter-shaped failures"
    (expect (= "(def x 1)" (paren-repair/maybe-repair-delimiters "(def x 1))")))
    (expect (nil? (paren-repair/maybe-repair-delimiters "(println \"a\" broken \"b)")))))
