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
                 (let
                   [body
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
