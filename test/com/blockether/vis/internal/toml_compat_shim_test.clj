(ns com.blockether.vis.internal.toml-compat-shim-test
  "The toml-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism (`extension/sandbox-shims`): a `toml` module published
   into `sys.modules` (so `import toml` works). Reading delegates to the stdlib
   `tomllib` for a spec-correct parse; writing is a pure-Python serializer covering
   scalars, arrays, inline/nested tables and array-of-tables. No host bridge."
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
  toml-module-test
  (it "publishes toml under sys.modules"
      (with-python-context
        (expect (true? (ev python-context "import toml\n__import__('sys').modules.get('toml') is not None")))))
  (it "autoloads toml onto builtins (no import needed)"
      (with-python-context (expect (true? (ev python-context "'a = 1' in toml.dumps({'a':1})")))))
  (it "supports `import toml` with a version string"
      (with-python-context (expect (true? (ev python-context
                                              "import toml\nisinstance(toml.__version__, str)"))))))

(defdescribe
  toml-roundtrip-test
  (it "loads scalars, arrays and tables via the stdlib tomllib parser"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import toml\n"
                                "doc = 'title = ' + chr(39) + 'vis' + chr(39) + chr(10)\n"
                                "doc += '[owner]' + chr(10) + 'ports = [80, 443]' + chr(10)\n"
                                "d = toml.loads(doc)\n"
                                "d['title'] == 'vis' and d['owner']['ports'] == [80, 443]"))))))
  (it "dumps nested tables and round-trips back to the same dict"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import toml\n"
                                "obj = {'title':'vis','owner':{'name':'blk','ports':[1,2]}}\n"
                                "toml.loads(toml.dumps(obj)) == obj"))))))
  (it "serializes an array-of-tables as [[section]]"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import toml\n" "obj = {'items':[{'id':1},{'id':2}]}\n"
                                "s = toml.dumps(obj)\n"
                                "s.count('[[items]]') == 2 and toml.loads(s) == obj"))))))
  (it "serializes booleans/floats/strings with correct toml syntax"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import toml\n" "s = toml.dumps({'b':True,'f':1.5,'name':'ab'})\n"
                                "'b = true' in s and 'f = 1.5' in s "
                                "and ('name = ' + chr(34) + 'ab' + chr(34)) in s")))))))
