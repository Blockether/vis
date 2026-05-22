(ns com.blockether.vis.ext.foundation-core.hints-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-core.hints :as hints]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe title-hint-test
  (it "hints :critical when the session title is blank (regardless of turn position)"
    ;; :critical — a blank title is a real gap, not soft advice. Models
    ;; routinely skip :info hints; :critical makes the title call happen.
    (doseq [tp [1 2 5 10 17 100]]
      (let [n (hints/title-hint {:session-title nil
                                 :title-refresh? false
                                 :turn-position tp
                                 :iteration 1})]
        (expect (= :critical (:importance n)))
        (expect (string? (:validator-fn n)))
        (expect (str/includes? (:body n) "session title is currently empty"))
        (expect (str/includes? (:body n) "top-level form"))
        (expect (str/includes? (:body n) "bare `(set-session-title!"))
        (expect (str/includes? (:body n) "not a foundation `v/` tool"))
        (expect (not (str/includes? (:body n) "(do"))))))

  (it "fires on turn 1 (first turn) when host flags :title-refresh?"
    (let [n (hints/title-hint {:session-title "Refactor auth flow"
                               :title-refresh? true
                               :turn-position 1
                               :iteration 1})]
      (expect (= :info (:importance n)))
      (expect (string? (:validator-fn n)))
      (expect (str/includes? (:body n) "Refactor auth flow"))
      (expect (str/includes? (:body n) "set-session-title!"))
      (expect (str/includes? (:body n) "do not namespace-qualify it"))
      (expect (str/includes? (:body n) "1 turn(s)"))))

  (it "fires on every TITLE_REFRESH_TURN_PERIOD-th turn when host flags :title-refresh?"
    (doseq [tp [hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)
                (* 5 hints/TITLE_REFRESH_TURN_PERIOD)]]
      (let [n (hints/title-hint {:session-title "Triage 148 path failures"
                                 :title-refresh? true
                                 :turn-position tp
                                 :iteration 1})]
        (expect (some? n))
        (expect (str/includes? (:body n) (str tp " turn(s)")))
        (expect (str/includes? (:body n) "Triage 148 path failures")))))

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

(defdescribe title-validator-fn-test
  ;; The validator-fn is a SCI source string the engine compiles and
  ;; evaluates against a form envelope at end-of-iter. We round-trip
  ;; through Clojure's reader+eval here so the test exercises the same
  ;; predicate the engine will.
  (let [pred (eval (read-string hints/TITLE_VALIDATOR_FN_SRC))]
    (it "passes when src calls set-session-title!"
      (expect (true? (boolean (pred {:src "(set-session-title! \"Auth refactor\")"
                                     :result :vis/silent})))))

    (it "fails when src does not call set-session-title!"
      (expect (false? (boolean (pred {:src "(v/cat \"README.md\")" :result :ok})))))

    (it "fails when the proof form errored"
      (expect (false? (boolean (pred {:src "(set-session-title! \"x\")"
                                      :error {:message "boom"}})))))

    (it "fails on missing :src"
      (expect (false? (boolean (pred {:result :ok})))))))

(defdescribe context-pressure-hint-test
  (it "stays silent below the threshold"
    (expect (nil? (hints/context-pressure-hint
                    {:input-tokens 1000
                     :context-limit 200000}))))

  (it "fires at or above CONTEXT_PRESSURE_THRESHOLD with :warn importance + validator-fn"
    (let [limit 200000
          used  (long (* limit hints/CONTEXT_PRESSURE_THRESHOLD))
          n     (hints/context-pressure-hint {:input-tokens used
                                              :context-limit limit})]
      (expect (= :warn (:importance n)))
      (expect (string? (:validator-fn n)))
      (expect (str/includes? (:body n) "Context pressure"))
      (expect (str/includes? (:body n) "Converge now"))))

  (it "fires for 100k/200k (the z.ai GLM sweet-spot boundary)"
    (let [n (hints/context-pressure-hint {:input-tokens 100000
                                          :context-limit 200000})]
      (expect (some? n))
      (expect (str/includes? (:body n) "100000"))
      (expect (str/includes? (:body n) "200000"))))

  (it "is nil-safe when token/limit info is missing or zero"
    (expect (nil? (hints/context-pressure-hint {})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 0 :context-limit 200000})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 50000 :context-limit 0})))))

(defdescribe context-pressure-validator-fn-test
  (let [pred (eval (read-string hints/CONTEXT_PRESSURE_VALIDATOR_FN_SRC))]
    (it "passes on a (done …) form"
      (expect (true? (boolean (pred {:src "(done {:answer \"x\"})" :result :vis/answer})))))

    (it "passes on a (done …) call that drops trailer scopes"
      (expect (true? (boolean (pred {:src "(done {:answer \"x\" :trailer-drop [\"t1/i1\"]})"
                                     :result :vis/answer})))))

    (it "passes when src mentions :trailer-summarize"
      (expect (true? (boolean (pred {:src "(done {:answer \"x\" :trailer-summarize [{:scope-start \"t1/i1\" :scope-end \"t1/i2\" :summary \"explored\"}]})"
                                     :result :vis/answer})))))

    (it "fails on unrelated form"
      (expect (false? (boolean (pred {:src "(v/ls \".\")" :result :ok})))))

    (it "fails when proof form errored"
      (expect (false? (boolean (pred {:src "(done {:answer \"x\"})"
                                      :error {:message "boom"}})))))))

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

  (it "title hook returns ctx-shape hint with :body + :validator-fn + :importance"
    (let [h (some #(when (= :vis.foundation/session-title (:id %)) %) hints/hooks)
          hit ((:fn h) {:session-title nil :title-refresh? false
                        :turn-position 1 :iteration 1})]
      (expect (string? (:body hit)))
      (expect (string? (:validator-fn hit)))
      ;; Blank-title branch is :critical.
      (expect (= :critical (:importance hit)))))

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
