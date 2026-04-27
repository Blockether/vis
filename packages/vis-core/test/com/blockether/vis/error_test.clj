(ns com.blockether.vis.error-test
  (:require [com.blockether.vis.error :as vis-error]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe error-formatting-test
  (it "adds the standard ERROR prefix"
    (expect (= "ERROR: Boom" (vis-error/format-error "Boom"))))

  (it "keeps already-prefixed messages stable"
    (expect (= "ERROR: Boom" (vis-error/format-error "ERROR: Boom"))))

  (it "normalizes throwable values"
    (expect (= "ERROR: Broken" (vis-error/format-error (ex-info "Broken" {})))))

  (it "formats map errors using :message first"
    (expect (= "ERROR: Missing field"
              (vis-error/format-error {:message "Missing field" :type :vis/missing-field}))))

  (it "exposes the standardized final-answer validation message"
    (expect (= "Final answer is missing :answer-type. Set :answer-type to \"mustache-text\" or \"mustache-markdown\"."
              (vis-error/missing-answer-type-message)))))
