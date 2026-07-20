(ns com.blockether.vis.internal.shim-regression-test
  "Regression tests for the sandbox-shim bugs found by fuzzing the shims:
   the two P0 eval-killers (sqlite3 big-int bind, pandas None->NaN upcast)
   plus the numpy / pandas / PIL / bs4 / tabulate / dateutil semantic drifts.
   Each case reproduces a formerly-broken idiom against a real GraalPy context."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context
                                                                         {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  sqlite3-bind-guards-test
  (it
    "raises a CATCHABLE OverflowError on an int above SQLite's 64-bit range (was an uncatchable host crash)"
    (with-python-context
      (expect (true? (ev python-context
                         (str "import sqlite3\n"
                              "con = sqlite3.connect(':memory:')\n"
                              "con.execute('CREATE TABLE t(x)')\n"
                              "try:\n" "    con.execute('INSERT INTO t VALUES(?)', (2**70,))\n"
                              "    ok = False\n" "except OverflowError:\n"
                              "    ok = True\n" "ok"))))))
  (it "rejects an unsupported bind type with InterfaceError instead of storing junk"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import sqlite3\n"
                                "con = sqlite3.connect(':memory:')\n"
                                "con.execute('CREATE TABLE t(x)')\n"
                                "try:\n" "    con.execute('INSERT INTO t VALUES(?)', ([1,2],))\n"
                                "    ok = False\n" "except sqlite3.InterfaceError:\n"
                                "    ok = True\n" "ok")))))))

(defdescribe
  pandas-missing-data-test
  (it "upcasts a numeric column with a None to float64 NaN so comparisons/filters work"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import pandas as pd, math\n"
                                "s = pd.Series([1,2,3,None])\n" "vals = s.tolist()\n"
                                "ok = (str(s.dtype) == 'float64'\n"
                                "      and vals[:3] == [1.0,2.0,3.0] and math.isnan(vals[3])\n"
                                "      and (s > 2).tolist() == [False,False,True,False])\n"
                                "ok"))))))
  (it "supports groupby column subscript, dict-agg, assign, nlargest and drop_duplicates"
      (with-python-context
        (expect
          (true?
            (ev
              python-context
              (str
                "import pandas as pd\n" "df = pd.DataFrame({'a':[1,2,3,4],'b':['x','y','x','y']})\n"
                "ok = (df.groupby('b')['a'].sum().tolist() == [4,6]\n"
                "      and df.groupby('b').agg({'a':'sum'}).to_dict('list') == {'a':[4,6]}\n"
                "      and df.assign(c=lambda d: d['a']*2)['c'].tolist() == [2,4,6,8]\n"
                "      and df.nlargest(2,'a')['a'].tolist() == [4,3]\n"
                "      and pd.DataFrame({'a':[1,1,2]}).drop_duplicates()['a'].tolist() == [1,2])\n"
                "ok")))))))

(defdescribe
  numpy-semantics-test
  (it "exposes sized dtypes as callable scalar constructors (uint8 wraps, float64 casts)"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "ok = (np.uint8(300) == 44 and np.float64(3.5) == 3.5\n"
                                "      and np.array([1,2],dtype=np.uint8).tolist() == [1,2])\n"
                                "ok"))))))
  (it "supports list fancy indexing arr[[i,j,k]]"
      (with-python-context
        (expect (= [40 20 10]
                   (ev python-context
                       "import numpy as np\nnp.array([10,20,30,40])[[3,1,0]].tolist()")))))
  (it "yields inf/nan for divide-by-zero, sqrt of a negative, and mean of empty (no exception)"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str
                       "import numpy as np, math\n"
                       "d = (np.array([1.0,-1.0,0.0])/0).tolist()\n"
                       "sq = np.sqrt(np.array([-1.0,4.0])).tolist()\n"
                       "ok = (d[0] == float('inf') and d[1] == float('-inf') and math.isnan(d[2])\n"
                       "      and math.isnan(sq[0]) and sq[1] == 2.0\n"
                       "      and math.isnan(np.mean(np.array([]))))\n" "ok"))))))
  (it "computes percentile"
      (with-python-context
        (expect (= 2.5
                   (ev python-context
                       "import numpy as np\nnp.percentile(np.array([1,2,3,4]), 50)"))))))

(defdescribe
  pil-convert-validation-test
  (it "raises ValueError on an unknown convert() mode instead of silently making a bogus image"
      (with-python-context (expect (true? (ev python-context
                                              (str "from PIL import Image\n"
                                                   "im = Image.new('RGB',(2,2),(1,2,3))\n"
                                                   "try:\n" "    im.convert('ZZZ')\n"
                                                   "    ok = False\n" "except ValueError:\n"
                                                   "    ok = True\n" "ok")))))))

(defdescribe
  bs4-mutation-and-regex-test
  (it "builds a tree with new_tag/append and supports insert_before / replace_with"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "from bs4 import BeautifulSoup\n"
                           "soup = BeautifulSoup('<ul><li>a</li><li>b</li></ul>', 'html.parser')\n"
                           "tag = soup.new_tag('li'); tag.append('c'); soup.ul.append(tag)\n"
                           "after = [li.get_text() for li in soup.find_all('li')]\n"
                           "first = soup.find('li'); first.insert_before(soup.new_tag('li'))\n"
                           "n = len(soup.find_all('li'))\n" "first.replace_with('X')\n"
                           "ok = (after == ['a','b','c'] and n == 4 and 'X' in soup.get_text())\n"
                           "ok"))))))
  (it
    "matches find_all(string=regex) / find_all(name=regex) and prettify() indents"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import re\n"
              "from bs4 import BeautifulSoup\n"
              "a = [str(s) for s in BeautifulSoup('<p>hi</p><p>world</p>','html.parser').find_all(string=re.compile('wor'))]\n"
              "b = len(BeautifulSoup('<h1>a</h1><h2>b</h2>','html.parser').find_all(re.compile('^h[12]$')))\n"
              "c = chr(10) in BeautifulSoup('<div><p>x</p></div>','html.parser').prettify()\n"
              "ok = (a == ['world'] and b == 2 and c)\n" "ok")))))))

(defdescribe
  tabulate-colalign-test
  (it "honours the colalign kwarg without raising"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str "from tabulate import tabulate\n"
                     "out = tabulate([[1,2],[3,4]], headers=['a','b'], colalign=('right','left'))\n"
                     "isinstance(out, str) and 'a' in out")))))))

(defdescribe dateutil-tz-abbreviation-test
             (it "parses a trailing timezone abbreviation into a fixed-offset aware datetime"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str "from dateutil import parser\n"
                                           "d1 = parser.parse('2021-01-01 12:00 EST')\n"
                                           "d2 = parser.parse('2021-06-15 08:30 PDT')\n"
                                           "ok = (d1.utcoffset().total_seconds() == -18000\n"
                                           "      and d2.utcoffset().total_seconds() == -25200)\n"
                                           "ok")))))))

(defdescribe
  sqlite3-null-json-test
  (it
    "reads SQL NULL back as real Python None so json.dumps works (was ForeignNone)"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import sqlite3, json, math\n"
              "con = sqlite3.connect(':memory:')\n" "con.execute('CREATE TABLE t(a,b)')\n"
              "con.execute('INSERT INTO t VALUES(1, NULL)')\n"
              "con.execute('INSERT INTO t VALUES(?, ?)', (2, None))\n"
              "rows = con.execute('SELECT a,b FROM t').fetchall()\n"
              "js = json.dumps([dict(zip(['a','b'], r)) for r in rows])\n"
              "ok = (rows[0][1] is None and js == '[{\\\"a\\\": 1, \\\"b\\\": null}, {\\\"a\\\": 2, \\\"b\\\": null}]')\n"
              "ok"))))))
  (it "dumps a whole schema+data via Connection.iterdump"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str "import sqlite3\n"
                     "con = sqlite3.connect(':memory:')\n" "con.execute('CREATE TABLE t(a,b)')\n"
                     "con.execute('INSERT INTO t VALUES(1,2)')\n" "lines = list(con.iterdump())\n"
                     "ok = (lines[0] == 'BEGIN TRANSACTION;' and lines[-1] == 'COMMIT;'\n"
                     "      and any('CREATE TABLE' in l for l in lines)\n"
                     "      and any('INSERT INTO' in l for l in lines))\n" "ok")))))))

(defdescribe
  numpy-array-dtype-test
  (it "array .astype(sized) wraps AND tracks the dtype (the numpy<->PIL image idiom)"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "a = np.array([256,257,-1]).astype('uint8')\n"
                                "b = np.array([256,257], dtype='uint8')\n"
                                "img = (np.array([0.0,0.5,1.0])*255).astype('uint8')\n"
                                "ok = (a.tolist() == [0,1,255] and str(a.dtype) == 'uint8'\n"
                                "      and b.tolist() == [0,1] and img.tolist() == [0,127,255])\n"
                                "ok"))))))
  (it "adds diff / roll / nan-reductions / pad and axis-wise median/sort/flip"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "import numpy as np\n" "ok = (np.diff([1,4,9,16]).tolist() == [3,5,7]\n"
                  "      and np.roll([1,2,3,4,5],2).tolist() == [4,5,1,2,3]\n"
                  "      and np.nansum([1.0,float('nan'),3.0]) == 4.0\n"
                  "      and np.nanmean([2.0,float('nan'),4.0]) == 3.0\n"
                  "      and np.pad([1,2,3],(1,2)).tolist() == [0,1,2,3,0,0]\n"
                  "      and np.median(np.array([[1,2,3],[4,5,6]]), axis=1).tolist() == [2.0,5.0]\n"
                  "      and np.sort(np.array([[3,1],[2,4]]), axis=0).tolist() == [[2,1],[3,4]]\n"
                  "      and np.flip(np.array([[1,2],[3,4]]), axis=1).tolist() == [[2,1],[4,3]])\n"
                  "ok"))))))
  (it "np.array(pil_image) honors the __array__ protocol (was: int() argument not Image)"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "import numpy as np\n" "from PIL import Image\n"
                  "g = Image.new('L', (4, 3), 0); g.putdata(list(range(12)))\n"
                  "a = np.array(g)\n"
                  "rgb = np.array(Image.new('RGB', (2, 2), (0, 0, 0)))\n"
                  "ok = (a.shape == (3, 4) and a.mean(axis=1).tolist() == [1.5, 5.5, 9.5]\n"
                  "      and rgb.shape == (2, 2, 3))\n" "ok")))))))

(defdescribe
  pandas-extra-surface-test
  (it "Series isin / where / replace / idxmax and DataFrame at / iat"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import pandas as pd\n"
                                "df = pd.DataFrame({'a':[1,2,3,4],'b':['x','y','x','y']})\n"
                                "ok = (df['a'].isin([2,4]).tolist() == [False,True,False,True]\n"
                                "      and df['a'].where(df['a']>2, 0).tolist() == [0,0,3,4]\n"
                                "      and df['a'].replace(1,99).tolist() == [99,2,3,4]\n"
                                "      and df['a'].idxmax() == 3\n"
                                "      and df.at[0,'b'] == 'x' and df.iat[1,0] == 2)\n" "ok"))))))
  (it "DataFrame elementwise arithmetic, astype-dict, melt and NaN->null to_json"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str "import pandas as pd, json\n"
                     "s = (pd.DataFrame({'a':[1,2]}) + pd.DataFrame({'a':[10,20]}))['a'].tolist()\n"
                     "typed = pd.DataFrame({'a':[1,2]}).astype({'a':'float64'})['a'].tolist()\n"
                     "m = pd.DataFrame({'id':[1],'x':[10],'y':[20]}).melt(id_vars=['id']).shape\n"
                     "js = pd.DataFrame({'a':[1.0,float('nan')]}).to_json()\n"
                     "ok = (s == [11,22] and typed == [1.0,2.0] and m == (2,3)\n"
                     "      and js == json.dumps([{'a':1.0},{'a':None}]))\n" "ok")))))))

(defdescribe
  http-shim-exception-mapping-test
  (it "httpx maps a bad-URL failure to an httpx.RequestError subclass (not requests.MissingSchema)"
      (with-python-context (expect (true? (ev python-context
                                              (str "import httpx\n"
                                                   "try:\n" "    httpx.get('notaurl')\n"
                                                   "    ok = False\n" "except httpx.RequestError:\n"
                                                   "    ok = True\n" "ok"))))))
  (it "urllib3 maps a bad-URL failure to urllib3.exceptions.HTTPError and exposes util.Retry"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import urllib3\n" "r = urllib3.util.Retry(total=3)\n"
                                "try:\n" "    urllib3.request('GET','notaurl')\n"
                                "    ok = False\n" "except urllib3.exceptions.HTTPError:\n"
                                "    ok = (r.total == 3)\n" "ok")))))))
