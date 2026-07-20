(ns com.blockether.vis.internal.tabulate-compat-shim-test
  "The tabulate-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism (`extension/sandbox-shims`): a `tabulate` module published
   into `sys.modules` (so `from tabulate import tabulate` works) and implemented in
   PURE Python on the stdlib. Renders list-of-lists / list-of-dicts / dict-of-lists
   / DataFrame across plain/simple/github/grid/rst/html tablefmts. No host bridge."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe tabulate-module-test
             (it "publishes tabulate under sys.modules"
                 (with-python-context
                   (expect (true? (ev python-context
                                      "__import__('sys').modules.get('tabulate') is not None")))))
             (it "supports `from tabulate import tabulate`"
                 (with-python-context
                   (expect (true?
                             (ev python-context
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
