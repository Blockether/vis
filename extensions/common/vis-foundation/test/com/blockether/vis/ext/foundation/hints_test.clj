(ns com.blockether.vis.ext.foundation.hints-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.hints :as hints]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe title-hint-test
  (it "hints :high when the session title is blank (regardless of turn position)"
    ;; :high — a blank title is a real gap, not soft advice. Models
    ;; routinely skip :low hints; :high makes the title call happen.
    (doseq [tp [1 2 5 10 17 100]]
      (let [n (hints/title-hint {:session-title nil
                                 :title-refresh? false
                                 :turn-position tp
                                 :iteration 1})]
        (expect (= :high (:importance n)))
        (expect (str/includes? (:text n) "session title is currently empty"))
        (expect (str/includes? (:text n) "top-level form"))
        (expect (str/includes? (:text n) "bare `(set-session-title!"))
        (expect (str/includes? (:text n) "not a foundation `v/` tool"))
        (expect (not (str/includes? (:text n) "(do"))))))

  (it "fires on turn 1 (first turn) when host flags :title-refresh?"
    (let [n (hints/title-hint {:session-title "Refactor auth flow"
                               :title-refresh? true
                               :turn-position 1
                               :iteration 1})]
      (expect (= :low (:importance n)))
      (expect (str/includes? (:text n) "Refactor auth flow"))
      ;; Text reworded from "refresh the title via ..." to "refresh it
      ;; via ..."; assert the call-to-action via the primitive instead.
      (expect (str/includes? (:text n) "set-session-title!"))
      (expect (str/includes? (:text n) "do not namespace-qualify it"))
      (expect (str/includes? (:text n) "1 turn(s)"))))

  (it "fires on every TITLE_REFRESH_TURN_PERIOD-th turn when host flags :title-refresh?"
    (doseq [tp [hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)
                (* 5 hints/TITLE_REFRESH_TURN_PERIOD)]]
      (let [n (hints/title-hint {:session-title "Triage 148 path failures"
                                 :title-refresh? true
                                 :turn-position tp
                                 :iteration 1})]
        (expect (some? n))
        (expect (str/includes? (:text n) (str tp " turn(s)")))
        (expect (str/includes? (:text n) "Triage 148 path failures")))))

  (it "stays silent on non-cadence turns even when refresh is flagged"
    (doseq [tp [2 3 4 5 6 7 8 9 11 19 21 99]]
      (expect (nil? (hints/title-hint {:session-title "Stable"
                                       :title-refresh? true
                                       :turn-position tp
                                       :iteration 1})))))

  (it "stays silent on cadence turn when host did not flag :title-refresh?"
    ;; :title-refresh? is a single boolean from the host signalling that
    ;; THIS iteration is the start of a turn. Without it, even the
    ;; cadence turn is silent — mid-turn iterations never re-fire.
    (doseq [tp [1 hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)]]
      (expect (nil? (hints/title-hint {:session-title "Stable"
                                       :title-refresh? false
                                       :turn-position tp
                                       :iteration 1})))))

  (it "never fires on iteration cadence (the old mod-N-iterations rule is gone)"
    ;; Old behavior: iteration 12 (TITLE_REFRESH_NUDGE_PERIOD) re-fired.
    ;; New behavior: iteration position is ignored for cadence; only
    ;; turn-position matters.
    (doseq [it [3 6 9 12 24 36 100]]
      (expect (nil? (hints/title-hint {:session-title "Stable"
                                       :title-refresh? false
                                       :turn-position 5
                                       :iteration it}))))))

(defdescribe context-pressure-hint-test
  (it "stays silent below the threshold"
    (expect (nil? (hints/context-pressure-hint
                    {:input-tokens 1000
                     :context-limit 200000}))))

  (it "fires at or above CONTEXT_PRESSURE_THRESHOLD"
    (let [limit 200000
          used  (long (* limit hints/CONTEXT_PRESSURE_THRESHOLD))
          n     (hints/context-pressure-hint {:input-tokens used
                                              :context-limit limit})]
      (expect (= :high (:importance n)))
      (expect (str/includes? (:text n) "Context pressure"))
      (expect (str/includes? (:text n) "Converge now"))))

  (it "fires for 100k/200k (the z.ai GLM sweet-spot boundary)"
    (let [n (hints/context-pressure-hint {:input-tokens 100000
                                          :context-limit 200000})]
      (expect (some? n))
      (expect (str/includes? (:text n) "100000"))
      (expect (str/includes? (:text n) "200000"))))

  (it "is nil-safe when token/limit info is missing or zero"
    (expect (nil? (hints/context-pressure-hint {})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 0 :context-limit 200000})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 50000 :context-limit 0})))))

(defdescribe hooks-registration-test
  (it "foundation ships only the two iteration-start soft hints"
    ;; Evidence/blind-answer guards were removed; their job is now the
    ;; harness's structural gate inside `(done ...)`.
    (let [ids (set (map :id hints/hooks))]
      (expect (= #{:vis.foundation/session-title
                   :vis.foundation/context-pressure}
                ids))))

  (it "every hook declares the four required keys (:id :doc :phase :fn)"
    (doseq [h hints/hooks]
      (expect (keyword? (:id h)))
      (expect (string? (:doc h)))
      (expect (= :turn.iteration/start (:phase h)))
      (expect (fn? (:fn h)))))

  (it "title hook adapts title-hint into the {:text :importance} shape"
    (let [h (some #(when (= :vis.foundation/session-title (:id %)) %) hints/hooks)
          hit ((:fn h) {:session-title nil :title-refresh? false
                        :turn-position 1 :iteration 1})]
      (expect (string? (:text hit)))
      ;; Blank-title branch is :high (see title-hint-test).
      (expect (= :high (:importance hit)))))

  (it "hooks return nil when their underlying condition is absent"
    (let [title-h    (some #(when (= :vis.foundation/session-title (:id %)) %) hints/hooks)
          pressure-h (some #(when (= :vis.foundation/context-pressure (:id %)) %) hints/hooks)]
      (expect (nil? ((:fn title-h)    {:session-title "Set" :title-refresh? false
                                       :turn-position 5 :iteration 1})))
      (expect (nil? ((:fn pressure-h) {:input-tokens 100 :context-limit 200000}))))))

;; Removed: blind-answer-guard-test and action-request-needs-evidence-test.
;; Their behaviours are now enforced by the harness structural gate inside
;; `(done ...)` (see `final-answer-structural-criteria-errors` in
;; `com.blockether.vis.internal.loop`); coverage lives in the loop tests.
