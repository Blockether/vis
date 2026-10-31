;; User request 2026-10-31: the perf lines must separate framework time from model
;; and tool time, or a slow model reads as a slow framework.
(ns com.blockether.vis.internal.loop.perf-test
  (:require [com.blockether.vis.internal.loop.perf :as perf]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe summary-test
             (it "takes the model and tool time out of the overhead, and never goes below zero"
                 (let [stages (-> {}
                                  (perf/add-stage :provider-call/stop 700.0)
                                  (perf/add-stage :code-result 100.0)
                                  (perf/add-stage :code-result 50.0)
                                  (perf/add-stage :code-exec nil))]
                   (expect (= 150.0 (:overhead-ms (perf/summary 1000.0 {} stages))))
                   (expect (= {:ms 150.0 :n 2}
                              (get-in (perf/summary 1000.0 {} stages) [:stages :code-result])))
                   (expect (nil? (get-in (perf/summary 1000.0 {} stages) [:stages :code-exec])))
                   (expect (= 0.0 (:overhead-ms (perf/summary 500.0 {} stages)))))))
