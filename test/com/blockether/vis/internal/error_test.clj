(ns com.blockether.vis.internal.error-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.error :as err]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe error-message-test
             (it "reads a Throwable's message"
                 (expect (= "boom" (err/error-message (ex-info "boom" {}))))
                 (expect (= "boom" (err/error-message (RuntimeException. "boom")))))
             (it "falls back to the Throwable itself when it carries no message"
                 (let [msg (err/error-message (RuntimeException.))]
                   (expect (string? msg))
                   (expect (str/includes? msg "RuntimeException"))))
             (it "prefers :message then :msg on an anomaly map, else prints it"
                 (expect (= "nope" (err/error-message {:message "nope"})))
                 (expect (= "nope" (err/error-message {:msg "nope"})))
                 (expect (= "nope" (err/error-message {:message "nope" :msg "other"})))
                 (expect (= "{:a 1}" (err/error-message {:a 1}))))
             (it "passes a string through and prints anything else"
                 (expect (= "plain" (err/error-message "plain")))
                 (expect (= "nil" (err/error-message nil)))
                 (expect (= ":kw" (err/error-message :kw)))
                 (expect (= "42" (err/error-message 42)))))

(defdescribe format-error-test
             (it "adds the standard prefix"
                 (expect (= "ERROR: boom" (err/format-error (ex-info "boom" {}))))
                 (expect (= "ERROR: plain" (err/format-error "plain"))))
             (it "is idempotent, so a re-formatted error never stutters"
                 (expect (= "ERROR: nope" (err/format-error "ERROR: nope")))
                 (expect (= (err/format-error "boom")
                            (err/format-error (err/format-error "boom"))))))

(defdescribe final-answer-code-error-message-test
             (it "prefixes the loop's own final-answer failure"
                 (expect (= "Final-answer code error: boom"
                            (err/final-answer-code-error-message (ex-info "boom" {}))))))
