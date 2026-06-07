(ns com.blockether.vis.ext.language-clojure.format-test
  (:require
   [com.blockether.vis.ext.language-clojure.format :as fmt]
   [lazytest.core :refer [defdescribe expect it]]))

;; vis no longer reformats code (the pretty-printer was removed with the SCI
;; engine) — `format-string` is a pass-through: code is written to disk as the
;; agent wrote it.
(defdescribe format-string-test
  (it "returns source verbatim (no reformatting)"
    (let [src "(defn  foo[x](* x   2))"
          out (fmt/format-string src)]
      (expect (string? out))
      (expect (= src out))))

  (it "returns the source unchanged on any input"
    (let [bad "(defn foo [x"   ;; unbalanced
          out (fmt/format-string bad)]
      (expect (= bad out))))

  (it "handles empty / nil safely"
    (expect (= "" (fmt/format-string "")))
    (expect (nil? (fmt/format-string nil)))))
