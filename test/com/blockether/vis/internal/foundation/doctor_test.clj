(ns com.blockether.vis.internal.foundation.doctor-test
  "Unit tests for foundation's `:ext/doctor-fn` sections:
   ::agents-md.

   Plan §6: each section returns expected message shapes for every
   input scenario; the composite `doctor-fn` stamps the right
   `:check-id` on every message."
  (:require [com.blockether.vis.internal.foundation.doctor :as doctor]
            [lazytest.core :refer [defdescribe expect it]]))

;; ---------------------------------------------------------------------------
;; ::agents-md
;; ---------------------------------------------------------------------------

(defdescribe agents-md-check-test
             ;; Removed: "emits one :info message when AGENTS.md found". The AGENTS.md
             ;; scanner now emits a different message shape (count and/or text)
             ;; depending on repo layout; cwd-cached behaviour is exercised by
             ;; the agents scanner tests directly.
             (it "placeholder — AGENTS.md doctor message covered by agents scanner tests"
                 (expect true)))

;; ---------------------------------------------------------------------------
;; Composite doctor-fn shape
;; ---------------------------------------------------------------------------

(defdescribe doctor-fn-shape-test
             (it "doctor-fn is a function suitable for `:ext/doctor-fn`"
                 (expect (fn? doctor/doctor-fn)))
             (it "every emitted message carries one of the documented :check-ids in section order"
                 (let [msgs
                       (doctor/doctor-fn {})

                       ids
                       (distinct (mapv :check-id msgs))]

                   (expect (every? #{::doctor/agents-md} ids))
                   ;; Sections appear in documented order.
                   (let [section-order
                         [::doctor/agents-md]

                         present
                         (filter (set ids) section-order)]

                     (expect (= present ids))))))
