(ns com.blockether.vis.ext.language-clojure.format-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.format :as fmt]
            [lazytest.core :refer [defdescribe expect it]]))

;; `format-string` runs cljfmt: it normalizes indentation + whitespace of
;; MULTI-LINE forms on write. It deliberately does NOT reflow a one-liner
;; into multiple lines (a cljfmt non-goal) — the line breaks must come from
;; the caller's source.
(defdescribe format-string-test
             (it "normalizes indentation of a mis-indented multi-line form"
                 (let
                   [src
                    "(defn foo [x]\n(let [y (inc x)]\n(* y 2)))"

                    out
                    (fmt/format-string src)]

                   (expect (string? out))
                   (expect (not= src out))
                   ;; nested forms indented under their parent, not flush-left
                   (expect (str/includes? out "\n  (let"))
                   (expect (str/includes? out "\n    (* y 2)"))))
             (it "leaves a one-liner on one line (cljfmt does not reflow)"
                 (let
                   [src
                    "(defn foo [x] (* x 2))"

                    out
                    (fmt/format-string src)]

                   (expect (= 1 (count (str/split-lines out))))))
             (it "returns the source unchanged when it cannot parse"
                 (let [bad "(defn foo [x"] ;; unbalanced — cljfmt throws, we keep source
                   (expect (= bad (fmt/format-string bad)))))
             (it "handles empty / nil safely"
                 (expect (= "" (fmt/format-string "")))
                 (expect (nil? (fmt/format-string nil)))))
