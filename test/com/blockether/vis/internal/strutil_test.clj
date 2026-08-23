(ns com.blockether.vis.internal.strutil-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.strutil :as su]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe truncate-test
             (it "head-clips to at most n chars, with no ellipsis"
                 (expect (= "abc" (su/truncate "abcdef" 3)))
                 (expect (= "abc" (su/truncate "abc" 3)))
                 (expect (= "ab" (su/truncate "ab" 3)))
                 (expect (= "" (su/truncate "abc" 0))))
             (it "stringifies non-strings and is nil-safe"
                 (expect (= "" (su/truncate nil 5)))
                 (expect (= "123" (su/truncate 12345 3)))
                 (expect (= "[1 2" (su/truncate [1 2 3] 4)))))

(defdescribe fence-delimiter-test
             (it "never goes below the CommonMark minimum of three backticks"
                 (expect (= "```" (su/fence-delimiter "plain text")))
                 (expect (= "```" (su/fence-delimiter "")))
                 (expect (= "```" (su/fence-delimiter nil)))
                 (expect (= "```" (su/fence-delimiter "a `code` span"))))
             (it "outruns the longest backtick run in the body"
                 ;; A fixed triple-backtick wrapper is ambiguous: the INNER fence closes the
                 ;; outer block early and everything after it renders as prose.
                 (expect (= "````" (su/fence-delimiter "a ``` b")))
                 (expect (= "`````" (su/fence-delimiter "````")))
                 (expect (= "````" (su/fence-delimiter "``` one\n`` two")))))

(defdescribe fenced-test
             (it "wraps a body the body itself cannot close early"
                 (expect (= "```\nx\n```" (su/fenced "x")))
                 (expect (= "```clj\nx\n```" (su/fenced "x" "clj")))
                 (let [body
                       "```\nnested\n```"

                       out
                       (su/fenced body "md")

                       delimiter
                       (first (str/split-lines out))]

                   (expect (str/starts-with? delimiter "````"))
                   (expect (str/ends-with? out (str/replace delimiter "md" "")))
                   (expect (str/includes? out body))))
             (it "stringifies the body and keeps the info string optional"
                 (expect (= "```\n\n```" (su/fenced nil)))
                 (expect (= "```\n42\n```" (su/fenced 42)))
                 (expect (= (su/fenced "x") (su/fenced "x" nil)))))


;; Regression: Anthropic's summarized extended thinking ends every block with a
;; `thinking_delta` whose whole payload is `…`, so a two-word summary was
;; persisted and painted as `I need…` — reading exactly like Vis had truncated
;; the thought mid-word.
(defdescribe strip-elision-marker-test
             (it "drops the provider's trailing elision marker"
                 (expect (= "I need" (su/strip-elision-marker "I need…")))
                 (expect (= "I need" (su/strip-elision-marker "I need …")))
                 (expect (= "I need" (su/strip-elision-marker "I need…  ")))
                 (expect (= "" (su/strip-elision-marker "…"))))
             (it "leaves text the model actually wrote alone"
                 (expect (= "wait… then go" (su/strip-elision-marker "wait… then go")))
                 (expect (= "I need..." (su/strip-elision-marker "I need...")))
                 (expect (= "I need" (su/strip-elision-marker "I need")))
                 (expect (nil? (su/strip-elision-marker nil)))))

(defdescribe normalize-thinking-text-test
             (it "collapses blank-line runs and padded blank rows"
                 (expect (= "alpha\n beta\n gamma"
                            (su/normalize-thinking-text " alpha\n\n\n beta  \n\t\n gamma "))))
             (it "strips the elision marker the stream ends with"
                 (expect (= "I need" (su/normalize-thinking-text "I need…")))
                 (expect (= "first\nsecond" (su/normalize-thinking-text "first\n\nsecond …"))))
             (it "answers nil for nothing, so a blank tick never wipes the screen"
                 (expect (nil? (su/normalize-thinking-text nil)))
                 (expect (nil? (su/normalize-thinking-text "")))
                 (expect (nil? (su/normalize-thinking-text "   \n\n ")))
                 (expect (nil? (su/normalize-thinking-text "…")))))

;; Regression: the provider's CUT summary was rendered as the iteration's whole
;; thought — `I found…`, `So the issue…`, `The diff…` — reading as if Vis had
;; truncated the model, on iterations that had really thought for 10-25s. Then
;; the same cut, one sentence later, still reached the screen as a dangling
;; `… needed for Pocket models. The real bl` tail.
(defdescribe
  settled-thinking-text-test
  (it "clips a summary at the last sentence the model closed"
      (expect (= "The tests pass." (su/settled-thinking-text "The tests pass. I need…")))
      (expect (= "Checked the parser." (su/settled-thinking-text "Checked the parser.…")))
      (expect (= "No sentencepiece dependency is needed for Pocket models."
                 (su/settled-thinking-text
                   "No sentencepiece dependency is needed for Pocket models. The real bl…")))
      (expect (= "first" (su/settled-thinking-text "first\n\nsecond…"))))
  (it "keeps a summary that ends ON a closed sentence or heading untouched"
      (expect (= "One. Two. Three!" (su/settled-thinking-text "One. Two. Three!…")))
      (expect (= "Done?" (su/settled-thinking-text "Done?")))
      (expect (= "**Planning**" (su/settled-thinking-text "**Planning**")))
      (expect (= "## Check the parser" (su/settled-thinking-text "## Check the parser"))))
  (it "keeps a closed heading before partially streamed prose"
      (expect (= "**Planning**" (su/settled-thinking-text "**Planning**\nI should…"))))
  (it "drops a summary the provider cut before its first sentence"
      (expect (nil? (su/settled-thinking-text "So the issue…")))
      (expect (nil? (su/settled-thinking-text "I found…")))
      (expect (nil? (su/settled-thinking-text "I should…")))
      (expect (nil? (su/settled-thinking-text
                      "The condition looks right for `.txt` files, so I need…")))
      (expect (nil? (su/settled-thinking-text "The diff"))))
  (it "answers nil for nothing, like the live normalizer"
      (expect (nil? (su/settled-thinking-text nil)))
      (expect (nil? (su/settled-thinking-text "   \n ")))
      (expect (nil? (su/settled-thinking-text "…")))))
