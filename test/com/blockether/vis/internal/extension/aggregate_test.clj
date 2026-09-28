(ns com.blockether.vis.internal.extension.aggregate-test
  "Extension callers never choose the owner of an aggregate row."
  (:require [com.blockether.vis.internal.extension.aggregate :as aggregate]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe reject-extension-id-test
             (it "refuses a caller-supplied extension id"
                 ;; Issue #291: extension callers pass engine (kebab keyword) keys only.
                 (expect (throws? clojure.lang.ExceptionInfo
                                  #(#'aggregate/reject-extension-id!
                                     {:key "k" :extension-id "other"})))
                 (expect (nil? (#'aggregate/reject-extension-id! {:key "k"})))))
