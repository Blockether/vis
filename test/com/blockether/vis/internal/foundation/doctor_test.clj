(ns com.blockether.vis.internal.foundation.doctor-test
  "Contract tests for foundation's `:ext/doctor-fn` contribution."
  (:require [com.blockether.vis.internal.foundation.doctor :as doctor]
            [lazytest.core :refer [defdescribe expect it]]))

;; Composite doctor-fn shape

(defdescribe
  doctor-fn-shape-test
  (it "doctor-fn is a function suitable for `:ext/doctor-fn`" (expect (fn? doctor/doctor-fn)))
  (it "every emitted message carries one of the documented :check-ids in section order"
      (let [msgs
            (doctor/doctor-fn {})

            ids
            (distinct (mapv :check-id msgs))]

        (expect (every? #{::doctor/agents-md ::doctor/housekeeping ::doctor/image-render} ids))
        ;; Sections appear in documented order.
        (let [section-order
              [::doctor/agents-md ::doctor/housekeeping ::doctor/image-render]

              present
              (filter (set ids) section-order)]

          (expect (= present ids))))))
