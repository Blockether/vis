(ns com.blockether.vis.internal.foundation.shim-pytest-test
  "The pytest-compat sandbox shim's DISK-DISCOVERY surface (issue #19):
   `pytest.main([paths])` walks dirs for `test_*.py` / `*_test.py`, imports each
   file into a fresh module namespace, and collects `test_*` across them — with
   assert introspection reading the file's source from `linecache`. The no-arg
   inline path (collect from the caller's block globals) must keep working too.

   Files are written on the Clojure side into a system temp dir (always readable
   by the confined sandbox FS); the Context is built with a `roots-fn` so Python
   `open()`/`os.walk` are enabled."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defn- tmp-dir
  ^String []
  (str (Files/createTempDirectory "vis-pytest-" (make-array FileAttribute 0))))

(defmacro with-fs-context
  "A sandbox Context whose Python filesystem is confined to `dir`."
  [dir & body]
  `(let [~(with-meta 'python-context {:tag `Context})
         (:python-context (ep/create-python-context {} (constantly [~dir])))]
     (try ~@body (finally (.close ~'python-context)))))

(defmacro with-context
  "A plain IO-NONE sandbox Context (inline mode only)."
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context
                                                                         {}))]
     (try ~@body (finally (.close ~'python-context)))))

(def ^:private report-code
  "Reduce `_vis_last_report` to a stable `RC=<rc>;nodeid|outcome;...` string."
  (str
    "rep = pytest.__dict__['_vis_last_report']\n"
    "'RC=' + str(rc) + ';' + ';'.join(sorted(n.rsplit('/',1)[-1] + '|' + o for (n,o,l) in rep))"))

(defdescribe
  disk-discovery-test
  (it "discovers test_*.py AND *_test.py in a directory and runs each test"
      (let [d (tmp-dir)]
        (spit (str d "/test_alpha.py")
              "def test_pass():\n    assert 1 + 1 == 2\ndef test_fail():\n    assert 2 + 2 == 5\n")
        (spit (str d "/beta_test.py") "def test_beta():\n    assert True\n")
        (spit (str d "/helper.py") "def test_ignored():\n    assert False\n")
        (with-fs-context d
                         (expect (= (str "RC=1;beta_test.py::test_beta|passed"
                                         ";test_alpha.py::test_fail|failed"
                                         ";test_alpha.py::test_pass|passed")
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" d
                                             "'])\n" report-code)))))))
  (it "recurses into subdirectories"
      (let [d (tmp-dir)]
        (.mkdirs (java.io.File. (str d "/nested")))
        (spit (str d "/nested/test_deep.py") "def test_deep():\n    assert True\n")
        (with-fs-context d
                         (expect (= "RC=0;test_deep.py::test_deep|passed"
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" d
                                             "'])\n" report-code)))))))
  (it "accepts a single file path directly"
      (let [d
            (tmp-dir)

            f
            (str d "/test_one.py")]

        (spit f "def test_one():\n    assert True\n")
        (with-fs-context d
                         (expect (= "RC=0;test_one.py::test_one|passed"
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "'])\n" report-code)))))))
  (it "collects Test* classes and parametrized cases from disk files"
      (let [d (tmp-dir)]
        (spit (str d "/test_shapes.py")
              (str "import pytest\n"
                   "class TestBox:\n    def test_area(self):\n        assert 2 * 3 == 6\n"
                   "@pytest.mark.parametrize('n', [1, 2])\n"
                   "def test_pos(n):\n    assert n > 0\n"))
        (with-fs-context d
                         (expect (= (str "RC=0;test_shapes.py::TestBox::test_area|passed"
                                         ";test_shapes.py::test_pos[1]|passed"
                                         ";test_shapes.py::test_pos[2]|passed")
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" d
                                             "'])\n" report-code)))))))
  (it "keeps assert introspection for disk-file failures (reads source from linecache)"
      (let [d (tmp-dir)]
        (spit (str d "/test_boom.py") "def test_boom():\n    x = 41\n    assert x == 42\n")
        (with-fs-context d
                         (let [lr (ev python-context
                                      (str "import pytest\nrc = pytest.main(['"
                                           d
                                           "'])\n"
                                           "rep = pytest.__dict__['_vis_last_report']\n"
                                           "[l for (n,o,l) in rep if o == 'failed'][0]"))]
                           (expect (str/includes? lr "test_boom"))
                           (expect (str/includes? lr "41")))))))

(defdescribe inline-mode-test
             (it "still collects from the caller's block globals when given no path args"
                 (with-context (expect (= "RC=1;test_x|passed;test_y|failed"
                                          (ev python-context
                                              (str "import pytest\n"
                                                   "def test_x():\n    assert True\n"
                                                   "def test_y():\n    assert False\n"
                                                   "rc = pytest.main()\n" report-code))))))
             (it "reports a clean summary rc=0 when every inline test passes"
                 (with-context (expect (= "RC=0;test_ok|passed"
                                          (ev python-context
                                              (str "import pytest\n"
                                                   "def test_ok():\n    assert 'vis' == 'vis'\n"
                                                   "rc = pytest.main()\n" report-code)))))))
