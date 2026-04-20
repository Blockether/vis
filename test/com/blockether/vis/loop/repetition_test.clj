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
   tool is rejecting the SHAPE of your call'.

   A second failure mode was seen in conversation
   6f832df0-6762-402b-8ca0-275f9aeb54a4 where a weak model wrapped
   identical tool calls in uniquely-named `def`s to bypass the pair
   check:

     iter N:   (def conv-v67 (:content (read-file \"x.clj\")))  → same content
     iter N+1: (def conv-v68 (:content (read-file \"x.clj\")))  → same content
     iter N+2: (def conv-v69 (:content (read-file \"x.clj\")))  → same content

   Each code string is unique so the pair key never repeats. No error
   either — the calls succeed. But the AGENT learned nothing between
   them. The fix keys on the RESULT preview alone (with a higher
   threshold than the pair key) to catch this cloaked-loop pattern."
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
        (expect (nil? w2))))

    (it "flags SAME successful result under DIFFERENT code after 3 occurrences (cloaked-loop regression)"
      ;; Real-world: model wraps identical `(read-file X)` calls in
      ;; differently-named `def`s. Pair key never fires because code
      ;; strings all differ; :error key never fires because calls
      ;; succeed. Detector must catch this via the result preview.
      ;;
      ;; Threshold 3 (not 2) for the result-only key — two successful
      ;; calls with equal results can be legitimate (retry after an
      ;; edit to confirm it took effect). The THIRD repeat is the
      ;; unambiguous loop signal.
      (let [content     {:content "identical file content here"}
            e1          {:code "(def conv-v67 (:content (read-file \"x.clj\")))" :result content}
            e2          {:code "(def conv-v68 (:content (read-file \"x.clj\")))" :result content}
            e3          {:code "(def conv-v69 (:content (read-file \"x.clj\")))" :result content}
            [c1 w1]     (sut/bump-and-detect-repetition {} [e1])
            [c2 w2]     (sut/bump-and-detect-repetition c1 [e2])
            [_ w3]      (sut/bump-and-detect-repetition c2 [e3])]
        (expect (nil? w1))
        (expect (nil? w2))
        (expect (some? w3))
        (expect (re-find #"REPETITION DETECTED" w3))
        (expect (re-find #"result repeated" w3))))

    (it "does not flag a single retry of the same successful result"
      ;; Counter-test: two reads of the same file (e.g., verify after
      ;; an edit) must NOT fire. The detector is advisory — false
      ;; positives here train the agent to ignore genuine signals.
      (let [content {:content "file contents"}
            e1      {:code "(read-file \"x.clj\")" :result content}
            e2      {:code "(read-file \"x.clj\")" :result content}
            [c1 _]  (sut/bump-and-detect-repetition {} [e1])
            [_ w2]  (sut/bump-and-detect-repetition c1 [e2])]
        ;; The PAIR key fires at 2 (that behavior is tested above).
        ;; This test asserts the result-only key does not ALSO fire
        ;; at 2 — the warning count and teaching signal should come
        ;; from the more specific pair key, not a duplicated one.
        (expect (some? w2))
        ;; Warning must not contain the result-only teaching line
        ;; yet — threshold is 3.
        (expect (not (re-find #"result repeated" w2)))))

    (it "does not fire result-only key on error executions (avoids double-counting)"
      ;; When :error is set, the :error key handles the signal. The
      ;; result-only key should skip error rows so a single failing
      ;; call does not fire twice under two different teaching frames.
      (let [err "boom"
            e1  {:code "(a)" :result nil :error err}
            e2  {:code "(b)" :result nil :error err}
            e3  {:code "(c)" :result nil :error err}
            [c1 _] (sut/bump-and-detect-repetition {} [e1])
            [c2 _] (sut/bump-and-detect-repetition c1 [e2])
            [_ w3] (sut/bump-and-detect-repetition c2 [e3])]
        ;; Error key fires; result-only line must NOT also appear.
        (expect (some? w3))
        (expect (re-find #"error repeated" w3))
        (expect (not (re-find #"result repeated" w3)))))))
