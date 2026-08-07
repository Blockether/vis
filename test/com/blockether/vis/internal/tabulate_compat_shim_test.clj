(ns com.blockether.vis.internal.tabulate-compat-shim-test
  "The tabulate-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism (`extension/sandbox-shims`): a `tabulate` module published
   into `sys.modules` (so `from tabulate import tabulate` works) and implemented in
   PURE Python on the stdlib. Renders list-of-lists / list-of-dicts / dict-of-lists
   / DataFrame across plain/simple/github/grid/rst/html tablefmts. No host bridge."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (tpc/shared)]
     ~@body))

(defdescribe
  tabulate-module-test
  (it "publishes tabulate under sys.modules"
      (with-python-context
        (expect (true?
                  (ev python-context
                      "import tabulate\n__import__('sys').modules.get('tabulate') is not None")))))
  (it "supports `from tabulate import tabulate`"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from tabulate import tabulate\n"
                                "isinstance(tabulate([[1,2]], tablefmt='plain'), str)")))))))

(defdescribe
  tabulate-format-test
  (it "simple format aligns numbers right, strings left, under a rule"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from tabulate import tabulate\n"
                                "t = tabulate([['Alice',30]], headers=['name','age'])\n"
                                "lines = t.split(chr(10))\n"
                                "lines[0].startswith('name') and set(lines[1]) <= set('- ') "
                                "and lines[2].rstrip().endswith('30')"))))))
  (it "github format emits a pipe header + alignment separator row"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from tabulate import tabulate\n"
                                "t = tabulate([['a',1]], headers=['s','n'], tablefmt='github')\n"
                                "lines = t.split(chr(10))\n"
                                "lines[0].startswith('|') and '--:' in lines[1]"))))))
  (it "grid format draws box borders with + corners"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from tabulate import tabulate\n"
                                "t = tabulate([['a',1]], headers=['s','n'], tablefmt='grid')\n"
                                "t.startswith('+') and t.count('+') >= 8 and '|' in t"))))))
  (it "headers='keys' reads column names from list-of-dicts"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "from tabulate import tabulate\n"
                  "t = tabulate([{'a':1,'b':2},{'a':3,'b':4}], headers='keys', tablefmt='plain')\n"
                  "t.split(chr(10))[0].split() == ['a','b']"))))))
  (it "renders a pandas-shim DataFrame directly"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from tabulate import tabulate\n"
                                "import pandas as pd\n"
                                "df = pd.DataFrame({'x':[1,2],'y':['p','q']})\n"
                                "t = tabulate(df, headers='keys', tablefmt='github')\n"
                                "'x' in t and 'y' in t and 'p' in t and 'q' in t")))))))

(defn- render
  "Renders one tabulate call inside the sandbox context and returns the string."
  [^Context c expr]
  (ev c (str "from tabulate import tabulate\n" expr)))


;; Byte-for-byte fidelity against upstream python-tabulate 0.9.0 output.
(defdescribe
  tabulate-fidelity-test
  (it "plain padding, decimal alignment and float trimming"
      (with-python-context
        (expect (= "item        cost\n------  --------\nspam     41.9999\neggs    451"
                   (render
                     python-context
                     "tabulate([['spam',41.9999],['eggs',451.0]], headers=['item','cost'])")))))
  (it "simple format pads headers to MIN_PADDING and rules the columns"
      (with-python-context
        (expect (= "name      age\n------  -----\nAlice      30\nBob         9"
                   (render python-context
                           "tabulate([['Alice',30],['Bob',9]], headers=['name','age'])")))))
  (it "headerless simple keeps a top AND bottom rule"
      (with-python-context (expect (= "-----  --\nAlice  30\nBob     9\n-----  --"
                                      (render python-context
                                              "tabulate([['Alice',30],['Bob',9]])")))))
  (it "numeric strings are parsed and aligned on the decimal point"
      (with-python-context (expect (= "   v\n----\n 1.5\n10"
                                      (render python-context
                                              "tabulate([['1.5'],['10']], headers=['v'])")))))
  (it "github separator carries per-column alignment colons"
      (with-python-context
        (expect (= "| s   |   n |\n|:----|----:|\n| a   |   1 |"
                   (render python-context
                           "tabulate([['a',1]], headers=['s','n'], tablefmt='github')")))))
  (it "pipe format matches github's colon separator"
      (with-python-context
        (expect (= "| s   |   n |\n|:----|----:|\n| a   |   1 |"
                   (render python-context
                           "tabulate([['a',1]], headers=['s','n'], tablefmt='pipe')")))))
  (it "orgtbl uses + at the header separator crossings"
      (with-python-context
        (expect (= "| s   |   n |\n|-----+-----|\n| a   |   1 |"
                   (render python-context
                           "tabulate([['a',1]], headers=['s','n'], tablefmt='orgtbl')")))))
  (it "rst rules with = and no pipes"
      (with-python-context
        (expect (= "===  ===\ns      n\n===  ===\na      1\n===  ==="
                   (render python-context
                           "tabulate([['a',1]], headers=['s','n'], tablefmt='rst')")))))
  (it "tsv keeps the padded cells"
      (with-python-context
        (expect (= "s  \t  n\na  \t  1"
                   (render python-context
                           "tabulate([['a',1]], headers=['s','n'], tablefmt='tsv')")))))
  (it
    "html carries per-column text-align styles"
    (with-python-context
      (expect
        (=
          "<table>\n<thead>\n<tr><th style=\"text-align: left;\">s  </th><th style=\"text-align: right;\">  n</th></tr>\n</thead>\n<tbody>\n<tr><td style=\"text-align: left;\">a  </td><td style=\"text-align: right;\">  1</td></tr>\n</tbody>\n</table>"
          (render python-context "tabulate([['a',1]], headers=['s','n'], tablefmt='html')")))))
  (it
    "multiline cells are split across grid rows"
    (with-python-context
      (expect
        (=
          "+-------+-----+\n| a     |   b |\n+=======+=====+\n| two   |   1 |\n| lines |     |\n+-------+-----+"
          (render python-context
                  "tabulate([['two\\nlines',1]], headers=['a','b'], tablefmt='grid')")))))
  (it
    "maxcolwidths wraps a long cell"
    (with-python-context
      (expect
        (=
          "+----------+-----+\n| s        |   n |\n+==========+=====+\n| a long   |   1 |\n| sentence |     |\n| here     |     |\n+----------+-----+"
          (render
            python-context
            "tabulate([['a long sentence here',1]], headers=['s','n'], maxcolwidths=[8,None], tablefmt='grid')")))))
  (it "colalign overrides the inferred alignment"
      (with-python-context
        (expect (= "  s  n\n---  ---\n  a  1"
                   (render python-context
                           "tabulate([['a',1]], headers=['s','n'], colalign=('right','left'))")))))
  (it "floatfmt applies to every float"
      (with-python-context
        (expect (= "   a     b\n----  ----\n1.23  2.00"
                   (render python-context
                           "tabulate([[1.23456,2.0]], headers=['a','b'], floatfmt='.2f')")))))
  (it "showindex prepends a right-aligned index column"
      (with-python-context
        (expect (= "    s      n\n--  ---  ---\n 0  a      1\n 1  b      2"
                   (render python-context
                           "tabulate([['a',1],['b',2]], headers=['s','n'], showindex=True)")))))
  (it "a generator of rows renders like a list of rows"
      (with-python-context
        (expect (= "s      n\n---  ---\na      1\nb      2"
                   (render python-context
                           "tabulate((r for r in [['a',1],['b',2]]), headers=['s','n'])")))))
  (it "missing values render as the empty string"
      (with-python-context (expect (= "s    n\n---  ---\na"
                                      (render python-context
                                              "tabulate([['a',None]], headers=['s','n'])"))))))

(defdescribe
  tabulate-surface-test
  (it "exposes tabulate_formats, simple_separated_format and TableFormat"
      (with-python-context (expect (= [true true true]
                                      (ev python-context
                                          (str "import tabulate as tb\n"
                                               "['github' in tb.tabulate_formats,\n"
                                               " tb.simple_separated_format(',').padding == 0,\n"
                                               " isinstance(tb.TableFormat, type)]"))))))
  (it
    "renders SEPARATING_LINE as a rule between body rows"
    (with-python-context
      (expect
        (=
          "s      n\n---  ---\na      1\n---  ---\nb      2"
          (render
            python-context
            "tabulate([['a',1], __import__('tabulate').SEPARATING_LINE, ['b',2]], headers=['s','n'])"))))))
