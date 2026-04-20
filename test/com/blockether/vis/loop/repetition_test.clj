(ns com.blockether.vis.loop.repetition-test
  "Unit tests for repetition detection inside the RLM iteration loop.

   The iteration loop's `detect-repetition` fires a strong warning to the
   LLM when it re-executes the same call with the same result, on the
   theory that a second identical call will never produce a different
   answer. Prior implementation keyed on `[code, result-str]` ONLY, which
   missed a real failure mode seen in conversation
   dbc0a62f-313b-4d8b-bf87-dac2ee24ffe9:

     iter 2: (grep \"foo\" \"src\" ...)  → ERROR \"'other' is different type of Path\"
     iter 3: (grep \"bar\" \"src\" ...)  → ERROR \"'other' is different type of Path\"
     iter 4: (grep \"baz\" \"src\" ...)  → ERROR \"'other' is different type of Path\"

   Code strings are ALL different, so `[code, result]` pairs are never
   repeated. But the AGENT is stuck in an identical failure mode. The
   fix also keys on `:error` message alone, so the repetition warning
   fires at iter 3 with the teaching signal 'stop varying inputs — the
   tool is rejecting the SHAPE of your call'."
  (:require
    [lazytest.core :refer [defdescribe describe it expect]]
    [com.blockether.vis.loop.nudges :as sut]))

(defdescribe build-repetition-warning-test
  (describe "build-repetition-warning"
    (it "returns nil when no call or error has repeated"
      (let [executions [{:code "(foo 1)" :result {:ok true}}]
            [_ warning] (sut/bump-and-detect-repetition {} executions)]
        (expect (nil? warning))))

    (it "flags identical code+result pairs (existing behavior)"
      (let [e {:code "(foo 1)" :result {:ok true}}
            [counts1 w1] (sut/bump-and-detect-repetition {} [e])
            [_ w2]       (sut/bump-and-detect-repetition counts1 [e])]
        (expect (nil? w1))
        (expect (some? w2))
        (expect (re-find #"REPETITION DETECTED" w2))))

    (it "flags same error message across DIFFERENT code calls (regression)"
      ;; This is the real-world scenario: agent tries (grep \"foo\" \"src\"),
      ;; (grep \"bar\" \"src\"), (grep \"baz\" \"src\"). Code strings all
      ;; differ. Result maps all carry the same `:error` string. Prior
      ;; implementation never flagged this because the code-result pair
      ;; is new each time. New implementation also keys on `:error`, so
      ;; by the 2nd occurrence the warning fires.
      (let [err "'other' is different type of Path"
            e1 {:code "(grep \"foo\" \"src\")" :result nil :error err}
            e2 {:code "(grep \"bar\" \"src\")" :result nil :error err}
            [counts1 w1] (sut/bump-and-detect-repetition {} [e1])
            [_counts2 w2] (sut/bump-and-detect-repetition counts1 [e2])]
        (expect (nil? w1))
        (expect (some? w2))
        (expect (re-find #"REPETITION DETECTED" w2))
        (expect (re-find #"different type of Path" w2))))

    (it "does not flag different errors from different calls"
      (let [e1 {:code "(a)" :result nil :error "boom-1"}
            e2 {:code "(b)" :result nil :error "boom-2"}
            [counts1 _] (sut/bump-and-detect-repetition {} [e1])
            [_ w2]      (sut/bump-and-detect-repetition counts1 [e2])]
        (expect (nil? w2))))))
