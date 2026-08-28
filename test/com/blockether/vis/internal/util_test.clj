(ns com.blockether.vis.internal.util-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe truncate-test
             (it "head-clips to at most n chars, with no ellipsis"
                 (expect (= "abc" (util/truncate "abcdef" 3)))
                 (expect (= "abc" (util/truncate "abc" 3)))
                 (expect (= "ab" (util/truncate "ab" 3)))
                 (expect (= "" (util/truncate "abc" 0))))
             (it "stringifies non-strings and is nil-safe"
                 (expect (= "" (util/truncate nil 5)))
                 (expect (= "123" (util/truncate 12345 3)))
                 (expect (= "[1 2" (util/truncate [1 2 3] 4)))))

(defdescribe fence-delimiter-test
             (it "never goes below the CommonMark minimum of three backticks"
                 (expect (= "```" (util/fence-delimiter "plain text")))
                 (expect (= "```" (util/fence-delimiter "")))
                 (expect (= "```" (util/fence-delimiter nil)))
                 (expect (= "```" (util/fence-delimiter "a `code` span"))))
             (it "outruns the longest backtick run in the body"
                 ;; A fixed triple-backtick wrapper is ambiguous: the INNER fence closes the
                 ;; outer block early and everything after it renders as prose.
                 (expect (= "````" (util/fence-delimiter "a ``` b")))
                 (expect (= "`````" (util/fence-delimiter "````")))
                 (expect (= "````" (util/fence-delimiter "``` one\n`` two")))))

(defdescribe fenced-test
             (it "wraps a body the body itself cannot close early"
                 (expect (= "```\nx\n```" (util/fenced "x")))
                 (expect (= "```clj\nx\n```" (util/fenced "x" "clj")))
                 (let [body
                       "```\nnested\n```"

                       out
                       (util/fenced body "md")

                       delimiter
                       (first (str/split-lines out))]

                   (expect (str/starts-with? delimiter "````"))
                   (expect (str/ends-with? out (str/replace delimiter "md" "")))
                   (expect (str/includes? out body))))
             (it "stringifies the body and keeps the info string optional"
                 (expect (= "```\n\n```" (util/fenced nil)))
                 (expect (= "```\n42\n```" (util/fenced 42)))
                 (expect (= (util/fenced "x") (util/fenced "x" nil)))))


;; Regression: Anthropic's summarized extended thinking ends every block with a
;; `thinking_delta` whose whole payload is `…`, so a two-word summary was
;; persisted and painted as `I need…` — reading exactly like Vis had truncated
;; the thought mid-word.
(defdescribe strip-elision-marker-test
             (it "drops the provider's trailing elision marker"
                 (expect (= "I need" (util/strip-elision-marker "I need…")))
                 (expect (= "I need" (util/strip-elision-marker "I need …")))
                 (expect (= "I need" (util/strip-elision-marker "I need…  ")))
                 (expect (= "" (util/strip-elision-marker "…"))))
             (it "leaves text the model actually wrote alone"
                 (expect (= "wait… then go" (util/strip-elision-marker "wait… then go")))
                 (expect (= "I need..." (util/strip-elision-marker "I need...")))
                 (expect (= "I need" (util/strip-elision-marker "I need")))
                 (expect (nil? (util/strip-elision-marker nil)))))

(defdescribe normalize-thinking-text-test
             (it "collapses blank-line runs and padded blank rows"
                 (expect (= "alpha\n beta\n gamma"
                            (util/normalize-thinking-text " alpha\n\n\n beta  \n\t\n gamma "))))
             (it "strips the elision marker the stream ends with"
                 (expect (= "I need" (util/normalize-thinking-text "I need…")))
                 (expect (= "first\nsecond" (util/normalize-thinking-text "first\n\nsecond …"))))
             (it "answers nil for nothing, so a blank tick never wipes the screen"
                 (expect (nil? (util/normalize-thinking-text nil)))
                 (expect (nil? (util/normalize-thinking-text "")))
                 (expect (nil? (util/normalize-thinking-text "   \n\n ")))
                 (expect (nil? (util/normalize-thinking-text "…")))))

;; Regression: the provider's CUT summary was rendered as the iteration's whole
;; thought — `I found…`, `So the issue…`, `The diff…` — reading as if Vis had
;; truncated the model, on iterations that had really thought for 10-25s. Then
;; the same cut, one sentence later, still reached the screen as a dangling
;; `… needed for Pocket models. The real bl` tail.
(defdescribe
  settled-thinking-text-test
  (it "clips a summary at the last sentence the model closed"
      (expect (= "The tests pass." (util/settled-thinking-text "The tests pass. I need…")))
      (expect (= "Checked the parser." (util/settled-thinking-text "Checked the parser.…")))
      (expect (= "No sentencepiece dependency is needed for Pocket models."
                 (util/settled-thinking-text
                   "No sentencepiece dependency is needed for Pocket models. The real bl…")))
      (expect (= "first" (util/settled-thinking-text "first\n\nsecond…"))))
  (it "keeps a summary that ends ON a closed sentence or heading untouched"
      (expect (= "One. Two. Three!" (util/settled-thinking-text "One. Two. Three!…")))
      (expect (= "Done?" (util/settled-thinking-text "Done?")))
      (expect (= "**Planning**" (util/settled-thinking-text "**Planning**")))
      (expect (= "## Check the parser" (util/settled-thinking-text "## Check the parser"))))
  (it "keeps a closed heading before partially streamed prose"
      (expect (= "**Planning**" (util/settled-thinking-text "**Planning**\nI should…"))))
  (it "drops a summary the provider cut before its first sentence"
      (expect (nil? (util/settled-thinking-text "So the issue…")))
      (expect (nil? (util/settled-thinking-text "I found…")))
      (expect (nil? (util/settled-thinking-text "I should…")))
      (expect (nil? (util/settled-thinking-text
                      "The condition looks right for `.txt` files, so I need…")))
      (expect (nil? (util/settled-thinking-text "The diff"))))
  (it "answers nil for nothing, like the live normalizer"
      (expect (nil? (util/settled-thinking-text nil)))
      (expect (nil? (util/settled-thinking-text "   \n ")))
      (expect (nil? (util/settled-thinking-text "…")))))

(defdescribe now-ms-test
             (it "answers a millisecond clock that moves forward"
                 (let [a (util/now-ms)]
                   (expect (pos? a))
                   (expect (<= a (util/now-ms))))))

(defdescribe blank-string-test
             (it "non-blank-string? accepts only text with something in it"
                 (expect (util/non-blank-string? "x"))
                 (expect (not (util/non-blank-string? "   ")))
                 (expect (not (util/non-blank-string? "")))
                 (expect (not (util/non-blank-string? nil)))
                 (expect (not (util/non-blank-string? 7))))
             (it "non-blank trims to a value or nil"
                 (expect (= "x" (util/non-blank "  x  ")))
                 (expect (nil? (util/non-blank "   ")))
                 (expect (nil? (util/non-blank nil)))
                 (expect (= "7" (util/non-blank 7)))))

(defdescribe digest-test
             (it "encodes UTF-8 by the charset, not the platform default"
                 (expect (= [-60 -123] (vec (util/utf8 "\u0105")))))
             (it "hex-folds every byte to two lowercase characters"
                 (expect (= "007fff" (util/bytes->hex (byte-array [0 127 -1])))))
             (it "reads the same digits for a string and for its bytes"
                 (expect (= (util/sha256-hex "vis") (util/sha256-hex (util/utf8 "vis"))))
                 (expect (= 64 (count (util/sha256-hex "vis")))))
             (it "matches the known SHA-256 of the empty input"
                 (expect (= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                            (util/sha256-hex ""))))
             (it "hands out a fresh streaming digest each call"
                 (let [d (util/sha256-digest)]
                   (.update d (util/utf8 "vis"))
                   (expect (= (util/sha256-hex "vis") (util/bytes->hex (.digest d)))))))

(defdescribe env-val-test
             (it "answers nil for a variable no environment sets"
                 (expect (nil? (util/env-val "VIS_A_VARIABLE_NOTHING_SETS")))))

(def ^:private re-rolled
  "What `com.blockether.vis.internal.util` owns. A second copy is not a style
   question: the engine reached twelve `now-ms` wrappers and five different hex
   folds, and the copies DRIFT — one clipped the digest to eight bytes, one
   spelled the charset as a string, one narrowed nil differently."
  {"the millisecond clock (util/now-ms)" #"System/currentTimeMillis"
   "a SHA-256 digest (util/sha256, util/sha256-digest)" #"MessageDigest/getInstance\s+\"SHA-256\""
   "a blank-string predicate (util/non-blank-string?)"
   #"\(and\s+\(string\?\s+[^)]+\)\s+\(not\s+\(str/blank\?"
   "UTF-8 bytes (util/utf8)" #"\.getBytes\s+[^\s)]+\s+(?:StandardCharsets/UTF_8|\"UTF-8\")"})

(defdescribe shared-primitives-test
             (it "leaves every re-rolled primitive of the engine to internal.util"
                 (let [offenders (vec (for [^java.io.File f (file-seq (io/file "src"))
                                            :when (and (.isFile f)
                                                       (str/ends-with? (.getName f) ".clj"))
                                            :when (not= "util.clj" (.getName f))
                                            :let [source (slurp f)]
                                            [what pattern] re-rolled
                                            :when (re-find pattern source)]

                                        (str (.getPath f) " re-rolls " what)))]
                   (expect (= [] offenders)))))
