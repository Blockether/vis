(ns com.blockether.vis.ext.language-clojure.format-test
  (:require
   [com.blockether.vis.ext.language-clojure.format :as fmt]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe format-string-test
  (it "pretty-prints a parsed form"
    (let [out (fmt/format-string "(defn  foo[x](* x   2))")]
      (expect (string? out))
      (expect (re-find #"defn foo" out))))

  (it "returns the source unchanged on parse failure"
    (let [bad "(defn foo [x"   ;; unbalanced
          out (fmt/format-string bad)]
      (expect (= bad out))))

  (it "handles empty / nil safely"
    (expect (= "" (fmt/format-string "")))
    (expect (nil? (fmt/format-string nil)))))
