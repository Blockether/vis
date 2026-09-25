(ns com.blockether.vis.internal.python.format-test
  (:require [com.blockether.vis.internal.python.format :as pyfmt]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe beautify-python-test
             (it "formats model Python the way ruff does"
                 (expect (= "x = [1, 2]\nprint(x)\n" (pyfmt/beautify-python "x=[1,2];print(x)"))))
             (it "returns an empty string for nil and blank code"
                 (expect (= "" (pyfmt/beautify-python nil)))
                 (expect (= "" (pyfmt/beautify-python " \n\t"))))
             (it "keeps code that does not parse verbatim"
                 (expect (= "def f(:" (pyfmt/beautify-python "def f(:"))))
             (it "formats each distinct block once"
                 (let [code "y=2  # cached"]
                   (expect (identical? (pyfmt/beautify-python code)
                                       (pyfmt/beautify-python code))))))
