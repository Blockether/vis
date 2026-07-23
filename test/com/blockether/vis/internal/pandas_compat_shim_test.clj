(ns com.blockether.vis.internal.pandas-compat-shim-test
  "The pandas-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism (`extension/sandbox-shims`): a `pandas` module published
   into `sys.modules` (so `import pandas` works) and implemented in PURE Python on
   the stdlib (csv/json/math) — Series + DataFrame with selection, loc/iloc,
   boolean masks, groupby, merge, concat, describe, read_csv/to_csv. No host bridge."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  pandas-module-test
  (it "publishes pandas under sys.modules"
      (with-python-context
        (expect (true? (ev python-context
                           "import pandas\n__import__('sys').modules.get('pandas') is not None")))))
  (it "autoloads pandas onto builtins (no import needed)"
      (with-python-context (expect (true? (ev python-context
                                              "pandas.Series([1,2,3]).sum() == 6")))))
  (it "supports `import pandas as pd` with a version string"
      (with-python-context (expect
                             (true? (ev python-context
                                        "import pandas as pd\nisinstance(pd.__version__, str)"))))))

(defdescribe
  pandas-dataframe-test
  (it "constructs from a dict of columns: shape / columns / dtypes"
      (with-python-context (expect (true?
                                     (ev python-context
                                         (str "import pandas as pd\n"
                                              "df = pd.DataFrame({'a':[1,2,3],'b':[4.0,5.0,6.0]})\n"
                                              "df.shape == (3,2) and df.columns == ['a','b'] "
                                              "and df.dtypes.tolist() == ['int64','float64']"))))))
  (it "constructs from a list of records"
      (with-python-context (expect
                             (true? (ev python-context
                                        (str "import pandas as pd\n"
                                             "df = pd.DataFrame([{'x':1,'y':2},{'x':3,'y':4}])\n"
                                             "df['x'].tolist() == [1,3] and df.shape == (2,2)"))))))
  (it "column arithmetic + boolean-mask filtering"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import pandas as pd\n"
                                "df = pd.DataFrame({'a':[1,2,3,4],'b':[10,20,30,40]})\n"
                                "df['c'] = df['a'] + df['b']\n"
                                "sub = df[df['a'] > 2]\n"
                                "df['c'].tolist() == [11,22,33,44] and sub.shape[0] == 2"))))))
  (it "iloc / loc selection (row, scalar, column)"
      (with-python-context (expect
                             (true? (ev python-context
                                        (str "import pandas as pd\n"
                                             "df = pd.DataFrame({'a':[1,2,3],'b':['x','y','z']})\n"
                                             "df.iloc[1].tolist() == [2,'y'] and df.iloc[0,0] == 1 "
                                             "and df.loc[2,'b'] == 'z'"))))))
  (it "sort_values orders rows ascending/descending"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "import pandas as pd\n" "df = pd.DataFrame({'n':['a','b','c'],'v':[3,1,2]})\n"
                  "df.sort_values('v')['n'].tolist() == ['b','c','a'] "
                  "and df.sort_values('v', ascending=False)['n'].tolist() == ['a','c','b']")))))))

(defdescribe
  pandas-analytics-test
  (it "groupby sum / mean / size"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import pandas as pd\n"
                                "df = pd.DataFrame({'g':['a','a','b'],'v':[1,2,3]})\n"
                                "g = df.groupby('g').sum()\n"
                                "g.index == ['a','b'] and g['v'].tolist() == [3,3] "
                                "and df.groupby('g').size().to_dict() == {'a':2,'b':1}"))))))
  (it "merge inner / left join"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "import pandas as pd\n"
                           "l = pd.DataFrame({'k':[1,2,3],'a':['x','y','z']})\n"
                           "r = pd.DataFrame({'k':[2,3,4],'b':[20,30,40]})\n"
                           "m = l.merge(r, on='k', how='inner')\n"
                           "m.shape[0] == 2 and l.merge(r, on='k', how='left').shape[0] == 3"))))))
  (it "describe returns count/mean/std/min/max per numeric column"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import pandas as pd\n"
                                "df = pd.DataFrame({'a':[1,2,3,4],'s':['p','q','r','t']})\n"
                                "d = df.describe()\n"
                                "d.columns == ['a'] and d['a'].loc['mean'] == 2.5 "
                                "and d['a'].loc['count'] == 4"))))))
  (it "fillna / dropna skip NaN correctly"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import pandas as pd\n"
                             "df = pd.DataFrame({'a':[1,None,3],'b':[10.0,20.0,None]})\n"
                             "df.dropna().shape[0] == 1 and df.fillna(0)['a'].tolist() == [1,0,3] "
                             "and df['a'].mean() == 2.0"))))))
  (it "value_counts + str accessor on a Series"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import pandas as pd\n"
                                "s = pd.Series(['Apple','banana','Apple'])\n"
                                "s.value_counts().to_dict() == {'Apple':2,'banana':1} "
                                "and s.str.lower().tolist() == ['apple','banana','apple']")))))))

(defdescribe
  pandas-io-test
  (it "read_csv infers numeric dtypes and round-trips via to_csv"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str
                        "import pandas as pd\n"
                        "csv = 'x,y,z' + chr(10) + '1,2.5,a' + chr(10) + '3,4.5,b'\n"
                        "df = pd.read_csv(csv)\n"
                        "df['x'].sum() == 4 and df.dtypes.tolist() == ['int64','float64','object'] "
                        "and pd.read_csv(df.to_csv(index=False))['y'].sum() == 7.0"))))))
  (it "to_dict records / to_json"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import pandas as pd, json\n"
                                "df = pd.DataFrame({'a':[1,2],'b':['p','q']})\n"
                                "df.to_dict('records') == [{'a':1,'b':'p'},{'a':2,'b':'q'}] "
                                "and json.loads(df.to_json())[0]['a'] == 1"))))))
  (it "interoperates with the numpy shim via .values"
      (with-python-context (expect (true? (ev python-context
                                              (str "import pandas as pd, numpy as np\n"
                                                   "df = pd.DataFrame({'a':[1,2,3]})\n"
                                                   "float(np.mean(df['a'].values)) == 2.0")))))))
