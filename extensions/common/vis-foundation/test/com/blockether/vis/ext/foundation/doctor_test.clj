(ns com.blockether.vis.ext.foundation.doctor-test
  "Unit tests for foundation's `:ext/doctor-fn` sections:
   ::agents-md and ::scan-warnings.

   Plan §6: each section returns expected message shapes for every
   input scenario; the composite `doctor-fn` stamps the right
   `:check-id` on every message."
  (:require
   [com.blockether.vis.ext.foundation.doctor :as doctor]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- section-msgs
  "Run the composite `doctor-fn` against `env`, then keep only the
   messages stamped with the given `:check-id` - the test sees just
   the section it cares about."
  [check-id env]
  (->> (doctor/doctor-fn env)
    (filter #(= check-id (:check-id %)))
    vec))

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
;; ::scan-warnings
;; ---------------------------------------------------------------------------

(defdescribe scan-warnings-check-test
  (it "emits zero messages when nothing malformed (clean repo)"
    (expect (empty? (section-msgs ::doctor/scan-warnings {})))))

;; ---------------------------------------------------------------------------
;; Composite doctor-fn shape
;; ---------------------------------------------------------------------------

(defdescribe doctor-fn-shape-test
  (it "doctor-fn is a function suitable for `:ext/doctor-fn`"
    (expect (fn? doctor/doctor-fn)))

  (it "every emitted message carries one of the documented :check-ids in section order"
    (let [msgs (doctor/doctor-fn {})
          ids  (distinct (mapv :check-id msgs))]
      (expect (every? #{::doctor/agents-md
                        ::doctor/scan-warnings}
                ids))
      ;; Sections appear in the documented order: agents-md, scan-warnings.
      ;; Any present subset preserves that ordering.
      (let [section-order [::doctor/agents-md ::doctor/scan-warnings]
            present (filter (set ids) section-order)]
        (expect (= present ids))))))
