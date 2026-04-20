(ns com.blockether.vis.loop.nudges-test
  "Pure tests for SYSTEM_NUDGE composers. These fire on every iteration
   that matches their trigger; keep them deterministic and free of
   I/O so the iteration-loop can rely on them as pure string builders."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.nudges :as sut]))

(defdescribe budget-warning-test
  (describe "budget-warning"
    (it "returns nil when plenty of budget remains"
      (expect (nil? (sut/budget-warning {:iteration 0 :current-max-iterations 10}))))

    (it "fires close-to-cap when remaining = 3"
      (let [s (sut/budget-warning {:iteration 7 :current-max-iterations 10})]
        (expect (string? s))
        (expect (str/includes? s "CLOSE TO CAP"))
        (expect (str/includes? s "3 turns left"))))

    (it "fires LAST ITERATION when remaining = 1"
      (let [s (sut/budget-warning {:iteration 9 :current-max-iterations 10})]
        (expect (string? s))
        (expect (str/includes? s "LAST ITERATION"))
        (expect (str/includes? s "request-more-iterations"))))

    (it "still fires LAST ITERATION past the boundary (defensive)"
      (let [s (sut/budget-warning {:iteration 12 :current-max-iterations 10})]
        (expect (string? s))
        (expect (str/includes? s "LAST ITERATION"))))))

(defdescribe var-index-overflow-test
  (describe "var-index-overflow"
    (it "returns nil for counts at or below the threshold"
      (expect (nil? (sut/var-index-overflow 10)))
      (expect (nil? (sut/var-index-overflow sut/VAR_INDEX_NUDGE_THRESHOLD))))

    (it "fires once var count crosses the threshold"
      (let [s (sut/var-index-overflow (inc sut/VAR_INDEX_NUDGE_THRESHOLD))]
        (expect (string? s))
        (expect (str/includes? s "SYSTEM_NUDGE"))
        (expect (str/includes? s ":forget"))
        (expect (str/includes? s "restore-var"))))

    (it "reports the exact count so the agent can calibrate"
      (let [s (sut/var-index-overflow 42)]
        (expect (str/includes? s "42 user vars"))))

    (it "is safe on nil / non-int input"
      (expect (nil? (sut/var-index-overflow nil)))
      (expect (nil? (sut/var-index-overflow "lots"))))))
