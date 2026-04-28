(ns com.blockether.vis-extension.error-test
  (:require [com.blockether.vis-extension.error :as vis-error]
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

  (it "formats final-answer code-error messages"
    ;; The only :answer-related validation message left after the
    ;; finalize collapse: when :code blocks fail mid-finalize.
    (let [message (vis-error/final-answer-code-error-message
                    (ex-info "div by zero" {}))]
      (expect (re-find #"code execution failed" message))
      (expect (re-find #"div by zero" message)))))
