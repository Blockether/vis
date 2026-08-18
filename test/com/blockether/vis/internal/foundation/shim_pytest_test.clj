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

(defdescribe
  node-id-selection-test
  "Issue #78: `path.py::name` must SELECT that node, and a run that executed
   nothing must not exit 0."
  (it "runs only the named function of a node id"
      (let [d
            (tmp-dir)

            f
            (str d "/test_pick.py")]

        (spit f "def test_one():\n    assert True\ndef test_two():\n    assert False\n")
        (with-fs-context d
                         (expect (= "RC=0;test_pick.py::test_one|passed"
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "::test_one'])\n" report-code)))))))
  (it "selects a class method, and a bare class name selects its methods"
      (let [d
            (tmp-dir)

            f
            (str d "/test_cls.py")]

        (spit f
              (str "class TestBox:\n" "    def test_a(self):\n        assert True\n"
                   "    def test_b(self):\n        assert True\n"
                   "def test_loose():\n    assert False\n"))
        (with-fs-context d
                         (expect (= "RC=0;test_cls.py::TestBox::test_a|passed"
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "::TestBox::test_a'])\n" report-code))))
                         (expect (= (str "RC=0;test_cls.py::TestBox::test_a|passed"
                                         ";test_cls.py::TestBox::test_b|passed")
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "::TestBox'])\n" report-code)))))))
  (it "selects one parametrized case by its bracketed id"
      (let [d
            (tmp-dir)

            f
            (str d "/test_param.py")]

        (spit f
              (str "import pytest\n"
                   "@pytest.mark.parametrize('n', [1, 2])\n"
                   "def test_pos(n):\n    assert n > 0\n"))
        (with-fs-context d
                         (expect (= "RC=0;test_param.py::test_pos[2]|passed"
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "::test_pos[2]'])\n" report-code)))))))
  (it "merges several node ids naming the same file into ONE load"
      (let [d
            (tmp-dir)

            f
            (str d "/test_merge.py")]

        (spit f
              (str "def test_a():\n    assert True\n"
                   "def test_b():\n    assert True\n"
                   "def test_c():\n    assert False\n"))
        (with-fs-context d
                         (expect (= (str "RC=0;test_merge.py::test_a|passed"
                                         ";test_merge.py::test_b|passed")
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "::test_a', '" f
                                             "::test_b'])\n" report-code)))))))
  (it "exits 5 (no tests collected), never 0, when a node id matches nothing"
      (let [d
            (tmp-dir)

            f
            (str d "/test_none.py")]

        (spit f "def test_one():\n    assert True\n")
        (with-fs-context d
                         (expect (= "RC=5;"
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "::test_typo'])\n" report-code)))))))
  (it "exits 4 with a diagnostic when a named path does not exist"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect
                           (= 4
                              (ev python-context
                                  (str "import pytest\npytest.main(['" d "/test_absent.py'])")))))))
  (it "--collect-only LISTS node ids and runs nothing"
      (let [d
            (tmp-dir)

            f
            (str d "/test_co.py")]

        (spit f "def test_one():\n    assert True\ndef test_two():\n    assert False\n")
        (with-fs-context d
                         (expect (= "RC=0;"
                                    (ev python-context
                                        (str "import pytest\nrc = pytest.main(['" f
                                             "', '--collect-only'])\n" report-code))))))))

(defn- hint?
  "Run `pytest.main([target])` after `prelude`, and reduce the shim's own stdout
   to whether it printed the undeclared-import-root hint."
  [^Context c prelude target]
  (ev c
      (str "import pytest, io, contextlib, sys\n" prelude
           "_b = io.StringIO()\n" "with contextlib.redirect_stdout(_b):\n"
           "    pytest.main(['" target
           "'])\n" "'HINT' if 'is not an import root' in _b.getvalue() else 'NONE'\n")))

(defdescribe
  import-root-hint-test
  "Issue #62: `vis-agent python` infers import roots from DECLARATIVE packaging
   metadata only, so a src-layout project that declares nothing anywhere still
   fails collection with a bare `ModuleNotFoundError`. When that happens the run
   must name the declaration that is missing — and must stay quiet whenever the
   failure has nothing to do with an unreachable source root."
  (it "hints at an undeclared `src` root when collection dies on an import"
      (let [d (tmp-dir)]
        (.mkdirs (java.io.File. (str d "/src/einmal")))
        (.mkdirs (java.io.File. (str d "/tests")))
        (spit (str d "/src/einmal/__init__.py") "VALUE = 42\n")
        (spit (str d "/tests/test_g.py")
              "from einmal import VALUE\n\n\ndef test_v():\n    assert VALUE == 42\n")
        (with-fs-context d
                         (expect (= "HINT" (hint? python-context "" (str d "/tests/test_g.py")))))))
  (it "stays quiet when that same `src` root is already importable"
      (let [d (tmp-dir)]
        (.mkdirs (java.io.File. (str d "/src/einmal")))
        (.mkdirs (java.io.File. (str d "/tests")))
        (spit (str d "/src/einmal/__init__.py") "VALUE = 42\n")
        (spit (str d "/tests/test_bad.py") "import nope_missing\n")
        (with-fs-context d
                         (expect (= "NONE"
                                    (hint? python-context
                                           (str "sys.path.insert(0, '" d "/src')\n")
                                           (str d "/tests/test_bad.py")))))))
  (it "stays quiet when the unimportable module has no source root to blame"
      (let [d (tmp-dir)]
        (.mkdirs (java.io.File. (str d "/tests")))
        (spit (str d "/tests/test_bad.py") "import nope_missing\n")
        (with-fs-context d
                         (expect (= "NONE"
                                    (hint? python-context "" (str d "/tests/test_bad.py"))))))))

(defn- captured
  "Run `pytest.main(<args-src>)` with the caller's stdout redirected into a
   buffer, and return `\"<rc>\\u0000<stdout>\"` — the terminal report exactly as
   the stream the run STARTED on saw it."
  [^Context c args-src]
  (ev c
      (str "import pytest, io, contextlib\n"
           "_b = io.StringIO()\n" "with contextlib.redirect_stdout(_b):\n"
           "    _rc = pytest.main(" args-src
           ")\n" "str(_rc) + chr(0) + _b.getvalue()\n")))

(defdescribe
  terminal-report-test
  "Issue #95: a run's terminal report is its product, so it must reach the
   stream the run STARTED on. A test that leaves `sys.stdout` swapped (an
   unrestored capture, an escaped `redirect_stdout`) used to swallow the whole
   report — tests' own prints showed, `N failed` / FAILURES did not — and an
   internal error used to propagate out of `main`, losing the report with it.
   `--junitxml` is the machine-readable half of the same contract."
  (it
    "reports even when a test leaves sys.stdout pointing elsewhere"
    (let [d
          (tmp-dir)

          f
          (str d "/test_swap.py")]

      (spit f
            (str "import sys, io\n"
                 "def test_ok():\n    print('own output')\n    assert True\n"
                 "def test_swap():\n    sys.stdout = io.StringIO()\n    assert 1 == 2\n"))
      (with-fs-context
        d
        (let [[rc restored out]
              (str/split (ev python-context
                             (str "import pytest, io, contextlib, sys\n"
                                  "_b = io.StringIO()\n" "with contextlib.redirect_stdout(_b):\n"
                                  "    _rc = pytest.main(['" f
                                  "'])\n" "    _ok = sys.stdout is _b\n"
                                  "str(_rc) + chr(0) + ('RESTORED' if _ok else 'LEAKED')"
                                  " + chr(0) + _b.getvalue()\n"))
                         #"\x00"
                         3)]
          (expect (= "1" rc))
          (expect (= "RESTORED" restored))
          ;; A PASSING test's stdout is CAPTURED (issue #110) — like real
          ;; pytest, it is not echoed into the report. What this case pins is
          ;; that the report itself still reaches the stream the run started
          ;; on, even after a test swapped `sys.stdout` from under it.
          (expect (str/includes? out "test session starts"))
          (expect (not (str/includes? out "own output")))
          (expect (str/includes? out "1 failed, 1 passed"))
          (expect (str/includes? out "FAILED"))))))
  (it "consumes the value of a value-taking option instead of collecting it"
      (let [d
            (tmp-dir)

            f
            (str d "/test_p.py")]

        (spit f "def test_one():\n    assert True\n")
        (with-fs-context d
                         (let [[rc out] (str/split (captured
                                                     python-context
                                                     (str "['-p', 'no:cacheprovider', '" f "']"))
                                                   #"\x00"
                                                   2)]
                           (expect (= "0" rc))
                           (expect (str/includes? out "1 passed"))))))
  (it
    "--junitxml writes a JUnit report file and says so"
    (let [d
          (tmp-dir)

          f
          (str d "/test_j.py")

          x
          (str d "/j.xml")]

      (spit f "def test_pass():\n    assert True\ndef test_fail():\n    assert 1 == 2\n")
      (with-fs-context
        d
        (let [[rc out]
              (str/split (captured python-context (str "['" f "', '--junitxml=" x "']")) #"\x00" 2)

              xml
              (slurp x)]

          (expect (= "1" rc))
          (expect (str/includes? out (str "generated xml file: " x)))
          (expect (str/includes? xml "<testsuite name=\"pytest\""))
          (expect (str/includes? xml "tests=\"2\""))
          (expect (str/includes? xml "failures=\"1\""))
          (expect (str/includes? xml "name=\"test_fail\""))
          (expect (str/includes? xml "<failure message="))))))
  (it "reports an unwritable --junitxml target without failing the run"
      (let [d
            (tmp-dir)

            f
            (str d "/test_ju.py")]

        (spit f "def test_one():\n    assert True\n")
        (with-fs-context d
                         (let [[rc out] (str/split (captured python-context
                                                             (str "['" f "', '--junitxml=" d "']"))
                                                   #"\x00"
                                                   2)]
                           (expect (= "0" rc))
                           (expect (str/includes? out "1 passed"))
                           (expect (str/includes? out "could not write xml file"))))))
  (it "turns an internal error into EXIT_INTERNALERROR with a visible report"
      (with-context (let [[rc out] (str/split (captured python-context "[], ns=5") #"\x00" 2)]
                      (expect (= "3" rc))
                      (expect (str/includes? out "INTERNALERROR"))))))

(defn- capture-probe
  "Source that runs `pytest.main(args)` with `sys.stdout` diverted into a
   StringIO (the shim writes its report to whatever stream the run STARTED on),
   then evaluates `tail` over the captured report text `out` and exit code `rc`."
  [args tail]
  (str "import io, sys, pytest\n"
       "_buf = io.StringIO()\n"
       "_old = sys.stdout\n"
       "sys.stdout = _buf\n"
       "try:\n"
       "    rc = pytest.main(["
       args
       "])\n"
       "finally:\n" "    sys.stdout = _old\n"
       "out = _buf.getvalue()\n" tail))

(defn- capture-dir
  "A temp dir holding one noisy file: a PASSING test that prints, and a FAILING
   one that writes to both stdout and stderr."
  ^String []
  (let [d (tmp-dir)]
    (spit (str d "/test_noise.py")
          (str "import sys\n"
               "def test_quiet():\n" "    print('QUIET-OUT')\n"
               "def test_loud():\n" "    print('LOUD-OUT')\n"
               "    sys.stderr.write('LOUD-ERR\\n')\n" "    assert 1 == 2\n"))
    d))

;; Regression, issue #110: the shim had NO global output capture. `--capture=fd`
;; / `--capture=sys` / `-s` were silently accepted no-ops, a test's stdout
;; leaked out ahead of the (buffered) report, its stderr was dropped entirely,
;; and a failing test's report carried no `Captured stdout/stderr call` section.
(defdescribe
  global-capture-test
  (it "captures test output by default and replays it under the failure"
      (let [d (capture-dir)]
        (with-fs-context d
                         (expect (= "RC=1;cap=True;err=True;quiet=False;after=True"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "'")
                                          (str "'RC=' + str(rc)"
                                               " + ';cap=' + str('Captured stdout call' in out)"
                                               " + ';err=' + str('LOUD-ERR' in out)"
                                               " + ';quiet=' + str('QUIET-OUT' in out)"
                                               " + ';after=' + str(out.find('LOUD-OUT')"
                                               " > out.find('test session starts') > -1)"))))))))
  (it "-s writes straight through, ahead of the report, with no captured section"
      (let [d (capture-dir)]
        (with-fs-context d
                         (expect (= "RC=1;cap=False;before=True"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "', '-s'")
                                          (str "'RC=' + str(rc)"
                                               " + ';cap=' + str('Captured stdout call' in out)"
                                               " + ';before=' + str(-1 < out.find('LOUD-OUT')"
                                               " < out.find('test session starts'))"))))))))
  (it "--capture=no is -s"
      (let [d (capture-dir)]
        (with-fs-context d
                         (expect (= "cap=False;before=True"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "', '--capture=no'")
                                          (str "'cap=' + str('Captured stdout call' in out)"
                                               " + ';before=' + str(-1 < out.find('LOUD-OUT')"
                                               " < out.find('test session starts'))"))))))))
  (it "--capture fd consumes its value instead of reading `fd` as a path"
      (let [d (capture-dir)]
        (with-fs-context d
                         (expect
                           (= "RC=1;notfound=False;cap=True"
                              (ev python-context
                                  (capture-probe
                                    (str "'" d "', '--capture', 'fd'")
                                    (str "'RC=' + str(rc)"
                                         " + ';notfound=' + str('not found' in out)"
                                         " + ';cap=' + str('Captured stdout call' in out)"))))))))
  (it "one test never inherits another test's captured output"
      (let [d
            (tmp-dir)

            _
            (spit (str d "/test_two.py")
                  (str "def test_first():\n" "    print('FIRST-ONLY')\n"
                       "    assert 1 == 2\n" "def test_second():\n"
                       "    print('SECOND-ONLY')\n" "    assert 1 == 3\n"))]

        (with-fs-context
          d
          (expect (= "RC=1;leak=False"
                     (ev python-context
                         (capture-probe (str "'" d "'")
                                        (str "'RC=' + str(rc) + ';leak='"
                                             " + str('FIRST-ONLY' in"
                                             " out.split('test_two.py::test_second')[-1])")))))))))

(defn- capsys-flow-dir
  "A temp dir with one FAILING test that drains `capsys` mid-test and then keeps
   writing to both streams."
  ^String []
  (let [d (tmp-dir)]
    (spit (str d "/test_capsys_flow.py")
          (str "import sys\n" "def test_read_then_fail(capsys):\n"
               "    print('BEFORE-READ')\n" "    o, e = capsys.readouterr()\n"
               "    assert o == 'BEFORE-READ\\n', 'drained=' + repr(o)\n"
               "    print('AFTER-READ-OUT')\n"
               "    sys.stderr.write('AFTER-READ-ERR\\n')\n" "    assert 1 == 2\n"))
    d))

(defn- phase-dir
  "A temp dir with one FAILING test whose fixture writes during SETUP and during
   TEARDOWN, so each phase's output must be reported under its own banner."
  ^String []
  (let [d (tmp-dir)]
    (spit (str d "/test_phases.py")
          (str "import pytest, sys\n" "@pytest.fixture\n"
               "def noisy():\n" "    print('SETUP-OUT')\n"
               "    sys.stderr.write('SETUP-ERR\\n')\n" "    yield 1\n"
               "    print('TEARDOWN-OUT')\n" "def test_phased(noisy):\n"
               "    print('CALL-OUT')\n" "    assert 0\n"))
    d))

;; Regression, issue #110 (cross-validated against real pytest 9.0.2): the global
;; capture and the `capsys` fixture were two SEPARATE captures, so everything a
;; test printed after `capsys.readouterr()` vanished from the report instead of
;; being replayed under the failure; and the one snapshot taken after teardown
;; labelled setup and teardown output `... call`, a phase real pytest never
;; attributes it to.
(defdescribe
  capture-parity-test
  (it "replays what a test writes after capsys.readouterr() under the failure"
      (let [d (capsys-flow-dir)]
        (with-fs-context d
                         (expect (= "RC=1;fail=True;out=True;err=True;drained=False"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "'")
                                          (str "'RC=' + str(rc)"
                                               " + ';fail=' + str('assert 1 == 2' in out)"
                                               " + ';out=' + str('AFTER-READ-OUT' in out)"
                                               " + ';err=' + str('AFTER-READ-ERR' in out)"
                                               " + ';drained=' + str('BEFORE-READ' in out)"))))))))
  (it "labels captured sections by phase: setup, call, teardown"
      (let [d (phase-dir)]
        (with-fs-context d
                         (expect (= "so=True;se=True;call=True;td=True;order=True"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "'")
                                          (str "'so=' + str('Captured stdout setup' in out)"
                                               " + ';se=' + str('Captured stderr setup' in out)"
                                               " + ';call=' + str('Captured stdout call' in out)"
                                               " + ';td=' + str('Captured stdout teardown' in out)"
                                               " + ';order=' + str(-1 < out.find('SETUP-OUT')"
                                               " < out.find('CALL-OUT')"
                                               " < out.find('TEARDOWN-OUT'))"))))))))
  (it "pops a capsys tail nobody read back to the original stream under -s"
      (let [d
            (tmp-dir)

            _
            (spit (str d "/test_pop.py")
                  (str "def test_pop(capsys):\n"
                       "    print('CAPTURED-HELLO')\n" "    o, e = capsys.readouterr()\n"
                       "    print('POPPED-' + o.strip())\n" "    print('TAIL-NEVER-READ')\n"))]

        (with-fs-context d
                         (expect (= "RC=0;pop=True;tail=True;live=True"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "', '-s'")
                                          (str "'RC=' + str(rc)"
                                               " + ';pop=' + str('POPPED-CAPTURED-HELLO' in out)"
                                               " + ';tail=' + str('TAIL-NEVER-READ' in out)"
                                               " + ';live=' + str(-1 < out.find('POPPED-')"
                                               " < out.find('test session starts'))"))))))))
  (it "counts a single setup error as pytest's `1 error`, not `1 errors`"
      (let [d
            (tmp-dir)

            _
            (spit (str d "/test_boom.py")
                  (str "import pytest\n" "@pytest.fixture\n"
                       "def boom():\n" "    raise RuntimeError('setup exploded')\n"
                       "def test_uses_boom(boom):\n" "    pass\n"))]

        (with-fs-context d
                         (expect (= "RC=1;singular=True;plural=False"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "'")
                                          (str "'RC=' + str(rc)"
                                               " + ';singular=' + str('1 error in' in out)"
                                               " + ';plural=' + str('1 errors' in out)")))))))))

;; Regression, issue #138: `capfd` was a second name for `capsys` -- a
;; `sys.stdout`/`sys.stderr` swap and nothing else -- so output written straight
;; to the descriptor (`os.write(1, ...)`, a C-level write, a child process) never
;; came back from `readouterr()`, and a test reading it back saw an empty string.
(defdescribe
  capfd-descriptor-test
  (it "reads back a descriptor-level write, which capsys still cannot see"
      (let [d
            (tmp-dir)

            _
            (spit
              (str d "/test_fd.py")
              (str
                "import os\n" "def test_fd(capfd):\n"
                "    print('VIA-STREAM')\n" "    os.write(1, b'VIA-DESCRIPTOR\\n')\n"
                "    out, err = capfd.readouterr()\n" "    assert 'VIA-STREAM' in out, repr(out)\n"
                "    assert 'VIA-DESCRIPTOR' in out, repr(out)\n"
                "    os.write(2, b'ERR-DESCRIPTOR\\n')\n"
                "    o2, e2 = capfd.readouterr()\n" "    assert 'ERR-DESCRIPTOR' in e2, repr(e2)\n"
                "def test_capsys_is_stream_only(capsys):\n" "    os.write(1, b'NEVER-SEEN\\n')\n"
                "    out, err = capsys.readouterr()\n"
                "    assert 'NEVER-SEEN' not in out, repr(out)\n"))]

        (with-fs-context
          d
          (expect (= (str "RC=0;test_fd.py::test_capsys_is_stream_only|passed"
                          ";test_fd.py::test_fd|passed")
                     (ev python-context
                         (str "import pytest\n" "rc = pytest.main(['" d "'])\n" report-code)))))))
  (it "replays a descriptor tail nobody read under the failure"
      (let [d
            (tmp-dir)

            _
            (spit (str d "/test_fd_tail.py")
                  (str "import os\n" "def test_tail(capfd):\n"
                       "    os.write(1, b'FD-TAIL-NEVER-READ\\n')\n" "    assert 1 == 2\n"))]

        (with-fs-context d
                         (expect (= "RC=1;tail=True"
                                    (ev python-context
                                        (capture-probe
                                          (str "'" d "'")
                                          (str "'RC=' + str(rc) + ';tail='"
                                               " + str('FD-TAIL-NEVER-READ' in out)"))))))))
  (it "captures the descriptor with no filesystem granted at all"
      (with-context (expect (= "RC=0;test_nofd|passed"
                               (ev python-context
                                   (str "import pytest, os\n"
                                        "def test_nofd(capfd):\n" "    print('STREAM-ONLY')\n"
                                        "    os.write(1, b'NO-FS-DESCRIPTOR\\n')\n"
                                        "    out, err = capfd.readouterr()\n"
                                        "    assert 'STREAM-ONLY' in out, repr(out)\n"
                                        "    assert 'NO-FS-DESCRIPTOR' in out, repr(out)\n"
                                        "rc = pytest.main()\n" report-code)))))))
