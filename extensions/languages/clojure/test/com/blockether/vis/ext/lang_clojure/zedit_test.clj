(ns com.blockether.vis.ext.lang-clojure.zedit-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.lang-clojure.zedit :as zedit]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe zedit-surface-test
  (it "keeps prompt concise and exposes zedit symbol docs"
    (expect (str/includes? zedit/z-prompt "`z/` Clojure structured edit"))
    (expect (str/includes? zedit/z-prompt "Use z/zedit for Clojure edits"))
    (expect (str/includes? zedit/z-prompt "Do not use v/bash for grep/sed/nl/cat/find"))
    (expect (str/includes? zedit/z-prompt "use v/rg/v/cat/v/glob"))
    (expect (str/includes? zedit/z-prompt "z/find-value"))
    (expect (str/includes? zedit/z-prompt "full rewrite-clj.zip API"))
    (expect (< (count zedit/z-prompt) 850))
    (expect (= 'zedit (:ext.symbol/sym zedit/zedit-symbol)))
    (expect (str/includes? (:ext.symbol/doc zedit/zedit-symbol)
              "Structured Clojure edit"))))
