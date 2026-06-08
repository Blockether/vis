(ns com.blockether.vis.internal.env-python-form-eval-test
  "Golden cases for prose-leading SyntaxError detection (FORM_EVAL_CONTRACT E4).

   When the model opens a reply with PROSE (a markdown heading or a sentence),
   the whole reply is parsed as Python and fails with a CPython error whose text
   varies by which mangled token trips first (an apostrophe → unterminated
   string, a `×` → invalid character, apostrophe-paired quotes → unmatched ')').
   `map-polyglot-error` must tag these `:prose-leading? true` with an actionable
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
      (expect (str/includes? msg "PROSE")))))
