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
           [java.nio.file Files Paths]
           [java.nio.file.attribute FileAttribute]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defn- tmp-dir
  ^String []
  (str (Files/createTempDirectory "vis-pytest-" (make-array FileAttribute 0))))

(defmacro with-fs-context
  "A sandbox Context whose Python filesystem is confined to `dir`."
  [dir & body]
  `(let
     [~(with-meta 'python-context {:tag `Context})
      (:python-context (ep/create-python-context {} (constantly [~dir])))]
     (try ~@body (finally (.close ~'python-context)))))

(defmacro with-context
  "A plain IO-NONE sandbox Context (inline mode only)."
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
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
      (let
        [d
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
                         (let
                           [lr (ev python-context
                                   (str "import pytest\nrc = pytest.main(['"
                                        d
                                        "'])\n"
                                        "rep = pytest.__dict__['_vis_last_report']\n"
                                        "[l for (n,o,l) in rep if o == 'failed'][0]"))]
                           (expect (str/includes? lr "test_boom"))
                           (expect (str/includes? lr "41")))))))

(defdescribe conftest-discovery-test
             (it "discovers conftest.py fixtures from the test file's dir (up to fs root)"
                 (let [d (tmp-dir)]
                   (spit (str d "/conftest.py")
                         "import pytest\n@pytest.fixture\ndef greeting():\n    return 'hi'\n")
                   (spit (str d "/test_uses_conftest.py")
                         "def test_g(greeting):\n    assert greeting == 'hi'\n")
                   (with-fs-context d
                                    (expect (= "RC=0;test_uses_conftest.py::test_g|passed"
                                               (ev python-context
                                                   (str "import pytest\nrc = pytest.main(['" d
                                                        "'])\n" report-code)))))))
             (it "lets a file-local fixture override a conftest.py fixture of the same name"
                 (let [d (tmp-dir)]
                   (spit (str d "/conftest.py")
                         "import pytest\n@pytest.fixture\ndef val():\n    return 'outer'\n")
                   (spit (str d "/test_override.py")
                         (str "import pytest\n@pytest.fixture\ndef val():\n    return 'local'\n"
                              "def test_v(val):\n    assert val == 'local'\n"))
                   (with-fs-context d
                                    (expect (= "RC=0;test_override.py::test_v|passed"
                                               (ev python-context
                                                   (str "import pytest\nrc = pytest.main(['" d
                                                        "'])\n" report-code)))))))
             (it "applies an outer-dir conftest.py to tests in a subdirectory"
                 (let [d (tmp-dir)]
                   (spit (str d "/conftest.py")
                         "import pytest\n@pytest.fixture\ndef base():\n    return 7\n")
                   (let [sub (str d "/sub")]
                     (Files/createDirectories (Paths/get sub (make-array String 0))
                                              (make-array FileAttribute 0))
                     (spit (str sub "/test_nested.py") "def test_b(base):\n    assert base == 7\n"))
                   (with-fs-context d
                                    (expect (= "RC=0;test_nested.py::test_b|passed"
                                               (ev python-context
                                                   (str "import pytest\nrc = pytest.main(['" d
                                                        "'])\n" report-code))))))))

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

(defdescribe
  builtin-fixtures-test
  (it "injects tmp_path_factory (mktemp + getbasetemp)"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect (= "RC=0;test_f|passed"
                                    (ev python-context
                                        (str "import pytest, os\n"
                                             "os.environ['TMPDIR'] = "
                                             (pr-str (str d))
                                             "\n"
                                             "def test_f(tmp_path_factory):\n"
                                             "    p = tmp_path_factory.mktemp('data')\n"
                                             "    assert p.exists()\n"
                                             "    assert tmp_path_factory.getbasetemp().exists()\n"
                                             "rc = pytest.main()\n" report-code)))))))
  (it "injects tmpdir / tmpdir_factory (legacy py.path-ish)"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect (= "RC=0;test_td|passed"
                                    (ev python-context
                                        (str "import pytest, os\n"
                                             "os.environ['TMPDIR'] = " (pr-str (str d))
                                             "\n" "def test_td(tmpdir, tmpdir_factory):\n"
                                             "    assert tmpdir.exists()\n"
                                             "    assert tmpdir_factory.mktemp('x').exists()\n"
                                             "rc = pytest.main()\n" report-code)))))))
  (it "injects capfd and reads back captured output"
      (with-context (expect (= "RC=0;test_cf|passed"
                               (ev python-context
                                   (str "import pytest\n"
                                        "def test_cf(capfd):\n" "    print('hello-capfd')\n"
                                        "    out, err = capfd.readouterr()\n"
                                        "    assert 'hello-capfd' in out\n"
                                        "rc = pytest.main()\n" report-code))))))
  (it "injects caplog and captures log records"
      (with-context (expect (= "RC=0;test_cl|passed"
                               (ev python-context
                                   (str
                                     "import pytest, logging\n" "def test_cl(caplog):\n"
                                     "    caplog.set_level(logging.INFO)\n"
                                     "    logging.getLogger('x').info('logline-abc')\n"
                                     "    assert 'logline-abc' in caplog.text\n"
                                     "    assert any('logline-abc' in m for m in caplog.messages)\n"
                                     "rc = pytest.main()\n" report-code))))))
  (it "injects recwarn and records warnings"
      (with-context (expect (= "RC=0;test_rw|passed"
                               (ev python-context
                                   (str "import pytest, warnings\n" "def test_rw(recwarn):\n"
                                        "    warnings.warn('deprecated-xyz', UserWarning)\n"
                                        "    assert len(recwarn) >= 1\n"
                                        "    w = recwarn.pop(UserWarning)\n"
                                        "    assert 'deprecated-xyz' in str(w.message)\n"
                                        "rc = pytest.main()\n" report-code)))))))

(defdescribe
  pytest-compat-test
  "Behaviours drawn from real pytest semantics (parametrized fixtures, fixture
   ids, the request API, usefixtures, and caplog record capture)."
  (it "parametrizes a test once per value of a params= fixture"
      (with-context (expect (= "RC=0;test_np[1]|passed;test_np[2]|passed;test_np[3]|passed"
                               (ev python-context
                                   (str "import pytest\n" "@pytest.fixture(params=[1, 2, 3])\n"
                                        "def num(request):\n" "    return request.param\n"
                                        "def test_np(num):\n" "    assert num > 0\n"
                                        "rc = pytest.main()\n" report-code))))))
  (it "labels params= fixture cases with the fixture ids="
      (with-context (expect (= "RC=0;test_i[one]|passed;test_i[two]|passed"
                               (ev python-context
                                   (str "import pytest\n"
                                        "@pytest.fixture(params=[1, 2], ids=['one', 'two'])\n"
                                        "def n(request):\n" "    return request.param\n"
                                        "def test_i(n):\n" "    assert n\n"
                                        "rc = pytest.main()\n" report-code))))))
  (it "exposes request.fixturename / scope / function on the injected request"
      (with-context
        (expect
          (= "RC=0;test_ra|passed"
             (ev python-context
                 (str "import pytest\n" "@pytest.fixture\n"
                      "def info(request):\n"
                      "    return (request.fixturename, request.scope, request.function.__name__)\n"
                      "def test_ra(info):\n" "    assert info[0] == 'info'\n"
                      "    assert info[1] == 'function'\n" "    assert info[2] == 'test_ra'\n"
                      "rc = pytest.main()\n" report-code))))))
  (it "applies @pytest.mark.usefixtures without an argument"
      (with-context (expect (= "RC=0;test_uf|passed"
                               (ev python-context
                                   (str "import pytest\n" "state = []\n"
                                        "@pytest.fixture\n" "def prep():\n"
                                        "    state.append(1)\n" "@pytest.mark.usefixtures('prep')\n"
                                        "def test_uf():\n" "    assert state == [1]\n"
                                        "rc = pytest.main()\n" report-code))))))
  (it "captures log records with caplog.at_level / records / clear"
      (with-context (expect (= "RC=0;test_cr|passed"
                               (ev python-context
                                   (str "import pytest, logging\n" "def test_cr(caplog):\n"
                                        "    with caplog.at_level(logging.INFO):\n"
                                        "        logging.getLogger('a').info('hello')\n"
                                        "    assert caplog.records[0].levelname == 'INFO'\n"
                                        "    assert caplog.records[0].message == 'hello'\n"
                                        "    caplog.clear()\n" "    assert caplog.records == []\n"
                                        "rc = pytest.main()\n" report-code)))))))

(defdescribe
  pytester-fixture-test
  "The `pytester` / `testdir` fixture (pytest's own acceptance-test surface):
   `makepyfile`/`makeconftest` write test files into a fresh temp dir, `runpytest`
   drives a nested `pytest.main`, and the returned `RunResult` supports
   `assert_outcomes(...)` + `stdout.fnmatch_lines(...)`. These cases are adapted
   from pytest's own `testing/` suite (which uses this same fixture)."
  (it "makepyfile + runpytest + assert_outcomes + stdout.fnmatch_lines"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect (= "RC=0;test_pt|passed"
                                    (ev python-context
                                        (str "import pytest, os\n"
                                             "os.environ['TMPDIR'] = "
                                             (pr-str (str d))
                                             "\n"
                                             "def test_pt(pytester):\n"
                                             "    pytester.makepyfile('''\n"
                                             "def test_a():\n    assert 1 + 1 == 2\n"
                                             "def test_b():\n    assert 2 + 2 == 5\n"
                                             "''')\n" "    result = pytester.runpytest()\n"
                                             "    result.assert_outcomes(passed=1, failed=1)\n"
                                             "    result.stdout.fnmatch_lines(['*1 passed*'])\n"
                                             "rc = pytest.main()\n" report-code)))))))
  (it "makeconftest publishes a fixture the sub-run test can request"
      (let [d (tmp-dir)]
        (with-fs-context
          d
          (expect (= "RC=0;test_pc|passed"
                     (ev python-context
                         (str "import pytest, os\n"
                              "os.environ['TMPDIR'] = "
                              (pr-str (str d))
                              "\n"
                              "def test_pc(pytester):\n" "    pytester.makeconftest('''\n"
                              "import pytest\n@pytest.fixture\ndef spam():\n    return 'eggs'\n"
                              "''')\n"
                              "    pytester.makepyfile('''\n"
                              "def test_spam(spam):\n    assert spam == 'eggs'\n"
                              "''')\n" "    pytester.runpytest().assert_outcomes(passed=1)\n"
                              "rc = pytest.main()\n" report-code)))))))
  (it "a parametrized sub-run reports one pass per case"
      (let [d (tmp-dir)]
        (with-fs-context
          d
          (expect (= "RC=0;test_pp|passed"
                     (ev python-context
                         (str "import pytest, os\n"
                              "os.environ['TMPDIR'] = "
                              (pr-str (str d))
                              "\n"
                              "def test_pp(pytester):\n" "    pytester.makepyfile('''\n"
                              "import pytest\n@pytest.mark.parametrize('n', [1, 2, 3])\n"
                              "def test_n(n):\n    assert n > 0\n"
                              "''')\n" "    pytester.runpytest().assert_outcomes(passed=3)\n"
                              "rc = pytest.main()\n" report-code)))))))
  (it "skip / xfail marks are reflected in RunResult outcomes"
      (let [d (tmp-dir)]
        (with-fs-context
          d
          (expect (= "RC=0;test_px|passed"
                     (ev python-context
                         (str "import pytest, os\n"
                              "os.environ['TMPDIR'] = " (pr-str (str d))
                              "\n" "def test_px(pytester):\n"
                              "    pytester.makepyfile('''\n"
                              "import pytest\n@pytest.mark.skip(reason='no')\n"
                              "def test_s():\n    assert False\n@pytest.mark.xfail\n"
                              "def test_x():\n    assert False\n"
                              "''')\n"
                              "    pytester.runpytest().assert_outcomes(skipped=1, xfailed=1)\n"
                              "rc = pytest.main()\n" report-code)))))))
  (it "the legacy `testdir` alias works the same, incl. the -v flag"
      (let [d (tmp-dir)]
        (with-fs-context
          d
          (expect (= "RC=0;test_td|passed"
                     (ev python-context
                         (str "import pytest, os\n"
                              "os.environ['TMPDIR'] = " (pr-str (str d))
                              "\n" "def test_td(testdir):\n"
                              "    testdir.makepyfile('''\n" "def test_ok():\n    assert True\n"
                              "''')\n" "    testdir.runpytest('-v').assert_outcomes(passed=1)\n"
                              "rc = pytest.main()\n" report-code))))))))

(defdescribe
  indirect-and-selection-test
  "The next pytest tier: indirect parametrize (values routed through a fixture
   via request.param), getfixturevalue chains, and the -k / -x / --maxfail
   selection surface — with deselected counts visible to RunResult."
  (it "indirect=True routes each parametrized value through the named fixture"
      (with-context (expect (= "RC=0;test_ind[1]|passed;test_ind[2]|passed;test_ind[3]|passed"
                               (ev python-context
                                   (str "import pytest\n"
                                        "@pytest.fixture\n"
                                        "def x(request):\n    return request.param * 10\n"
                                        "@pytest.mark.parametrize('x', [1, 2, 3], indirect=True)\n"
                                        "def test_ind(x):\n    assert x % 10 == 0\n"
                                        "rc = pytest.main()\n" report-code))))))
  (it "request.getfixturevalue resolves a fixture chain on demand"
      (with-context
        (expect (= "RC=0;test_chain|passed"
                   (ev python-context
                       (str
                         "import pytest\n" "@pytest.fixture\n"
                         "def a():\n    return 2\n" "@pytest.fixture\n"
                         "def b(request):\n    return request.getfixturevalue('a') + 3\n"
                         "def test_chain(request):\n    assert request.getfixturevalue('b') == 5\n"
                         "rc = pytest.main()\n" report-code))))))
  (it "-k selects by a boolean keyword expression and deselects the rest"
      (with-context (expect (= "RC=0;test_alpha|passed;test_gamma|passed"
                               (ev python-context
                                   (str "import pytest\n" "def test_alpha():\n    assert True\n"
                                        "def test_beta():\n    assert True\n"
                                        "def test_gamma():\n    assert True\n"
                                        "rc = pytest.main(['-k', 'alpha or gamma'])\n"
                                        report-code))))))
  (it "-x stops the session at the first failure"
      (with-context (expect (= "RC=1;test_a|failed"
                               (ev python-context
                                   (str "import pytest\n" "def test_a():\n    assert False\n"
                                        "def test_b():\n    assert False\n"
                                        "def test_c():\n    assert False\n"
                                        "rc = pytest.main(['-x'])\n" report-code))))))
  (it "RunResult.assert_outcomes sees -k deselected counts under pytester"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect (= "RC=0;test_kd|passed"
                                    (ev python-context
                                        (str "import pytest, os\n"
                                             "os.environ['TMPDIR'] = " (pr-str (str d))
                                             "\n" "def test_kd(pytester):\n"
                                             "    pytester.makepyfile('''\n"
                                             "def test_keep():\n    assert True\n"
                                             "def test_drop():\n    assert True\n" "''')\n"
                                             "    result = pytester.runpytest('-k', 'keep')\n"
                                             "    result.assert_outcomes(passed=1, deselected=1)\n"
                                             "rc = pytest.main()\n" report-code))))))))
