(ns com.blockether.vis.internal.error-test
  (:require [com.blockether.vis.internal.error :as err]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe error-message-test
             (it "reads a Throwable's message"
                 (expect (= "boom" (err/error-message (ex-info "boom" {}))))
                 (expect (= "boom" (err/error-message (RuntimeException. "boom")))))
             (it "prefers :message then :msg on an anomaly map, else prints it"
                 (expect (= "nope" (err/error-message {:message "nope"})))
                 (expect (= "nope" (err/error-message {:msg "nope"})))
                 (expect (= "nope" (err/error-message {:message "nope" :msg "other"})))
                 (expect (= "{:a 1}" (err/error-message {:a 1})))))

(defdescribe format-error-test
             (it "is idempotent, so a re-formatted error never stutters"
                 (expect (= "ERROR: nope" (err/format-error "ERROR: nope")))
                 (expect (= (err/format-error "boom")
                            (err/format-error (err/format-error "boom"))))))
