(ns com.blockether.vis.internal.env-python-form-eval-test
  "The Python form-eval contract, as executable tests. (Supersedes the old
   FORM_EVAL_CONTRACT.md spec — that was a pre-implementation agreement; the
   behavior is shipped, so these tests are the living record.)

   Two halves:
   1. `run-python-block` per-form AST eval semantics (E1–E7 / R1–R7): the reply
      splits into top-level statements via CPython `ast`; a bare expression
      echoes its value, `x = …` echoes x, a comment is not a form, the last
      form's value is the turn result, and evaluation stops at the first error.
   2. Prose-leading SyntaxError detection: when the model opens with PROSE, the
      whole reply parses as Python and fails with a CPython error whose text
      varies by which mangled token trips first (apostrophe → unterminated
      string, `×` → invalid character, apostrophe-pairs → unmatched ')').
      `map-polyglot-error` tags these `:prose-leading? true` with an actionable
      message, while NEVER mislabeling a genuine code typo as prose."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.env-python :as ep]
   [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot PolyglotException]))

(defn- classify
  "Parse `code` (parse-only — never evaluates the forms). On SyntaxError, run it
   through `map-polyglot-error` and return the op-error map; otherwise `:parsed`."
  [code]
  (try (ep/count-top-level-forms code) :parsed
    (catch PolyglotException e (ep/map-polyglot-error e code))))

(defn- prose-leading?
  [code]
  (boolean (get-in (classify code) [:data :prose-leading?])))

(defn- glued-forms?
  [code]
  (boolean (get-in (classify code) [:data :glued-forms?])))

(defdescribe prose-leading-guard-test
  ;; --- positives: real failing replies seen live (sessions 2e98be97 / 4c0eff03)
  (it "flags markdown heading + prose (apostrophe -> unterminated string)"
    (expect (prose-leading? "## Root cause found\n\n`/draft` clones via rift's CoW.\ndone(\"x\")")))

  (it "flags a prose sentence with a unicode char (invalid character ×)"
    (expect (prose-leading? "Both dialogs now resolve to an identical box at 120×40.\ndone(\"\"\"ok\"\"\")")))

  (it "flags a prose sentence whose apostrophes orphan a paren (unmatched ')')"
    (expect (prose-leading? "I've spent enough (removing them didn't help).\ngit_status()")))

  ;; --- negatives: valid code or genuine code typos must NOT be flagged
  (it "does NOT flag valid code"
    (expect (= :parsed (classify "git_status()"))))

  (it "does NOT flag a real code typo whose first line is valid code"
    (expect (not (prose-leading? "git_status()\nx = (1 + 2"))))

  (it "does NOT flag a multiline call cut mid-construct (no prose signature)"
    (expect (not (prose-leading? "cat(\"a.clj\"\nfoo"))))

  (it "does NOT flag a comment followed by a code typo"
    (expect (not (prose-leading? "# read the file\nx = (1 + 2"))))

  ;; --- the message must name PROSE (not unicode/typo) to break the misdiagnosis loop
  (it "actionable message names prose, not the character that tripped"
    (let [msg (:message (classify "Both dialogs resolve at 120×40 now.\ndone(\"\"\"ok\"\"\")"))]
      (expect (str/includes? msg "PROSE"))))

  ;; --- a stray non-ASCII char in code position ANYWHERE (not just line 1) — the
  ;;     em-dash-at-line-71 gap the prose-leading detector (first line only) missed
  (it "flags a non-ASCII char in code position even mid-reply, with its line"
    (let [r (classify "x = 5\n# a note\ny = 3 — 1")]
      (expect (true? (get-in r [:data :non-ascii-in-code?])))
      (expect (= 3 (get-in r [:data :line])))
      (expect (str/includes? (:message r) "non-ASCII"))))

  (it "a leading-prose failure stays tagged prose-leading, not non-ascii"
    (let [r (classify "I've spent enough (removing them didn't help).\ngit_status()")]
      (expect (nil? (get-in r [:data :non-ascii-in-code?])))
      (expect (true? (get-in r [:data :prose-leading?]))))))

(defdescribe glued-top-level-forms-guard-test
  ;; --- positives: the OpenAI/Codex missing-newline pattern seen live (982f3716)
  (it "flags a call glued directly onto done(...)"
    (expect (glued-forms? "cat(\"a.clj\")done(\"\"\"ok\"\"\")")))

  (it "flags a done(...) answer glued onto the next call (\"\"\")rg(...))"
    (expect (glued-forms? "done(\"\"\"summary\"\"\")rg({\"any\": [\"x\"]})")))

  (it "flags a dict-arg call glued onto another (})patch([)"
    (expect (glued-forms? "cat(\"a\", {\"range\": [1, 9]})patch([{\"file\": \"a\"}])")))

  (it "actionable message names the one-statement-per-line fix"
    (let [msg (:message (classify "cat(\"a\")done(\"\"\"ok\"\"\")"))]
      (expect (str/includes? msg "OWN line"))))

  ;; --- negatives: properly separated or unrelated syntax errors must NOT flag
  (it "does NOT flag properly newline-separated forms (they parse)"
    (expect (= :parsed (classify "cat(\"a\")\ndone(\"\"\"ok\"\"\")"))))

  (it "does NOT flag an unterminated string (no glue signature)"
    (expect (not (glued-forms? "done(\"\"\"abc"))))

  (it "does NOT flag a mid-construct cut (no glue signature)"
    (expect (not (glued-forms? "x = (1 + 2"))))

  ;; --- disjoint from the other two classes
  (it "a prose-leading failure is not mislabeled glued"
    (let [r (classify "I've spent enough (removing them didn't help).\ngit_status()")]
      (expect (nil? (get-in r [:data :glued-forms?])))
      (expect (true? (get-in r [:data :prose-leading?]))))))

(def ^:private py-ctx
  ;; One shared GraalPy sandbox for the eval cases; unique var names per case
  ;; avoid cross-test global contamination. No tools needed — plain Python
  ;; exercises the per-form eval semantics (R1–R7).
  (delay (:python-context (ep/create-python-context {}))))

(defdescribe run-python-block-form-eval-test
  (it "E1 — comment is not a form; assign + bare expr; last value is the result"
    (let [r (ep/run-python-block @py-ctx "# read it\ne1x = 41\ne1x")]
      (expect (= 41 (:result r)))
      (expect (= 2 (count (:forms r))))
      (expect (nil? (:error r)))))

  (it "E2 — a single value-returning expression echoes its value"
    (let [r (ep/run-python-block @py-ctx "40 + 2")]
      (expect (= 42 (:result r)))
      (expect (= 1 (count (:forms r))))))

  (it "E3 — multiple statements; the trailing tuple echoes both"
    (let [r (ep/run-python-block @py-ctx "e3a = 1\ne3b = 2\n(e3a, e3b)")]
      (expect (= [1 2] (:result r)))
      (expect (= 3 (count (:forms r))))))

  (it "E6 — a call expression echoes its return value"
    (let [r (ep/run-python-block @py-ctx "str(99)")]
      (expect (= "99" (:result r)))))

  (it "a def is one form; a following call evaluates"
    (let [r (ep/run-python-block @py-ctx "def e_f():\n    return 7\ne_f()")]
      (expect (= 7 (:result r)))
      (expect (= 2 (count (:forms r))))))

  (it "E7 — evaluation stops at the first erroring form; later forms do not run"
    (let [r (ep/run-python-block @py-ctx "e7x = 1\ne7_boom\ne7y = 2")]
      (expect (nil? (:result r)))
      (expect (= :python/runtime (get-in (:error r) [:data :phase])))
      (expect (= 2 (count (:forms r))))
      (expect (some? (:error (last (:forms r))))))))
