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
    (let [message (vis-error/missing-answer-type-message)]
      ;; The message MUST advertise all three valid answer-types --
      ;; if a future PR adds a fourth, this test forces the
      ;; message update.
      (expect (re-find #"mustache-text"     message))
      (expect (re-find #"mustache-markdown" message))
      (expect (re-find #"sci-expression"    message))))

  (it "formats sci-expression eval errors with a recovery hint"
    (let [message (vis-error/sci-expression-eval-error-message
                    (ex-info "Unable to resolve symbol: foo" {}))]
      (expect (re-find #"sci-expression" message))
      (expect (re-find #"Unable to resolve symbol: foo" message))
      (expect (re-find #":code first" message)))))
