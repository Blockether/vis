(ns com.blockether.vis.python.highlighter-test
  "PythonHighlighter colors Python for a terminal: it inserts ANSI SGR sequences and changes
   nothing else, and every line can be painted on its own."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.blockether.vis.python PythonHighlighter)
           (java.io File)))

(def ^:private sgr
  {:comment PythonHighlighter/COMMENT
   :constant PythonHighlighter/CONSTANT
   :escape PythonHighlighter/ESCAPE
   :function PythonHighlighter/FUNCTION
   :keyword PythonHighlighter/KEYWORD
   :number PythonHighlighter/NUMBER
   :property PythonHighlighter/PROPERTY
   :string PythonHighlighter/STRING
   :type PythonHighlighter/TYPE})

(defn- runs
  "Expected colored runs, written as alternating token classes and texts."
  [& classes-and-texts]
  (mapv (fn [[class text]]
          [(sgr class) text])
        (partition 2 classes-and-texts)))

(defn- colored-runs
  "Every colored stretch of highlighted `source`, in order, as [sgr-code text]."
  [source]
  (into []
        (keep (fn [[_ code text]]
                (when (and (not= "0" code) (seq text)) [(parse-long code) text])))
        (re-seq #"\u001b\[(\d+)m([^\u001b]*)" (PythonHighlighter/highlight source))))

(defn- strip-sgr [s] (str/replace s #"\u001b\[[0-9;]*m" ""))

(def ^:private fixture-sources
  (delay (->> (.listFiles (io/file "test/com/blockether/vis/python/fixtures/format"))
              (filter #(str/ends-with? (.getName ^File %) ".py"))
              (sort-by #(.getName ^File %))
              (mapv slurp))))

(defdescribe
  highlight-test
  (it "colors keywords, definitions, types, literals, attributes, constants and comments"
      (expect
        (= (runs :function "@cache"
                 :keyword "def"
                 :function "area"
                 :type "Shape"
                 :number "2.5"
                 :type "float"
                 :string "\"\"\"Doc.\"\"\""
                 :keyword "return"
                 :property "width"
                 :keyword "if"
                 :keyword "else"
                 :constant "None"
                 :comment "# fallback")
           (colored-runs (str "@cache\n" "def area(shape: Shape, scale=2.5) -> float:\n"
                              "    \"\"\"Doc.\"\"\"\n"
                              "    return shape.width * scale if shape else None  # fallback\n")))))
  (it "colors string escapes and leaves f-string replacement fields as code"
      (expect
        (=
          (runs :function "print"
                :string "f\""
                :string ":>"
                :string " items"
                :escape "\\t"
                :string "\""
                :string "r\"\\d\""
                :constant "MAX_SIZE"
                :property "pi"
                :function "method")
          (colored-runs
            "print(f\"{total:>{width}} items\\t\", r\"\\d\", MAX_SIZE, math.pi, obj.method())\n"))))
  (it "colors the soft keywords match, case and type only where they are keywords"
      (expect (= (runs :number "1" :keyword "match" :keyword "case" :keyword "type" :type "Pair")
                 (colored-runs
                   "match = 1\nmatch command:\n    case [name]:\n        type Pair = tuple\n"))))
  (it "inserts color sequences and changes nothing else, also in broken code"
      (doseq [source (conj @fixture-sources "x = 'unterminated\ny = 1\n" "def broken(:\n" "f\"{")]
        (expect (= source (strip-sgr (PythonHighlighter/highlight source))))))
  (it "closes every colored run before a line break and reopens it after"
      (expect (= ["text = \u001b[31m\"\"\"first\u001b[0m" "\u001b[31msecond\u001b[0m" ""
                  "\u001b[31mthird\"\"\"\u001b[0m"]
                 (str/split-lines (PythonHighlighter/highlight
                                    "text = \"\"\"first\nsecond\n\nthird\"\"\"\n"))))
      (doseq [source
              @fixture-sources

              line
              (str/split-lines (PythonHighlighter/highlight source))

              :when (str/includes? line "\u001b")]

        (expect (= "\u001b[0m" (last (re-seq #"\u001b\[[0-9;]*m" line))) line)))
  (it "returns nil, empty and already colored source as it is"
      (expect (nil? (PythonHighlighter/highlight nil)))
      (expect (= "" (PythonHighlighter/highlight "")))
      (let [colored "\u001b[31mx\u001b[0m = 1\n"]
        (expect (identical? colored (PythonHighlighter/highlight colored))))))
