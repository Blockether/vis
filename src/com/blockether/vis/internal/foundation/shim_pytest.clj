(ns com.blockether.vis.internal.foundation.shim-pytest
  "Built-in sandbox SHIM: a `pytest`-compatible module for the model's Python
   sandbox, implemented PURELY in Python on the stdlib (`ast`, `inspect`,
   `linecache`, `traceback`, `warnings`, `tempfile`) — NO host/JVM bridge, NOT a
   line of Clojure or babashka. `pytest` is a third-party wheel that does not
   ship in GraalPy, so agents writing full Python extensions and wanting to
   TEST them inline would otherwise hit ModuleNotFoundError; this extension
   contributes a `:ext/sandbox-shims` entry that
   `env-python/build-agent-context` installs into every sandbox Context (main +
   every `sub_loop` fork).

   It is NOT real pytest: there is no pluggy/plugin system, no `conftest.py`
   file discovery, no CLI, and no import-time assertion rewrite. Instead it
   implements the subset that matters in an inline sandbox where the model
   writes tests + `pytest.main()` in ONE block:

     - collection of `test_*` functions and `Test*` classes (scoped to the
       CURRENT block via `__vis_src__`, so leftovers from earlier blocks in the
       shared globals are NOT swept in),
     - RUNTIME assert introspection (`assert 2 == 3` reconstructed with operand
       values) done by registering `__vis_src__` into `linecache` and walking
       the failing frame's AST — the same UX as pytest's rewrite, via a
       different mechanism,
     - `pytest.raises` / `warns` / `approx` / `fail` / `skip` / `xfail` /
       `importorskip`, `@pytest.fixture` (function/module/session scope,
       yield-teardown, autouse, recursive injection), `@pytest.mark.parametrize`
       / `skip` / `skipif` / `xfail` (+ arbitrary marks), `pytest.param`,
       builtin fixtures `request` / `monkeypatch` / `capsys` / `capfd` /
       `tmp_path`, and a `pytest.main()` runner that prints progress + failure
       reports + a summary line and returns an exit code.

   Unlike `shim-yaml`/`shim-matplotlib` there are NO `:shim/bindings`: the shim
   is a self-contained Python preamble with zero host callables. It publishes a
   `pytest` module into `sys.modules` (so `import pytest` works) and staples it
   onto builtins (so `pytest.raises(...)` works with NO import, like
   json/os/requests)."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private pytest-compat-shim-src
  "Pure-Python preamble that publishes a `pytest`-compatible module built ONLY
   on the stdlib. Zero host callables. Published into `sys.modules` under
   `pytest` (so `import pytest` finds it) AND stapled onto builtins (so
   `pytest.raises(...)` works with NO import). INLINED here so it ships in-jar
   with no separate `.py` resource. Installed once per sandbox context (main +
   every `sub_loop` fork), BEFORE the baseline snapshot so its `__vis_*` names
   are filtered out of the model-visible live-vars view. The Python body uses
   ONLY single-quoted string literals and `chr(...)` for double-quote/CR/LF, so
   this Clojure string needs zero backslash escaping.

   Assert introspection leans on a proven fact of this sandbox: `run-python-block`
   binds the block source to the `__vis_src__` global, and registering it into
   `linecache.cache` under the `<prog>` co_filename makes `inspect`/`ast` see the
   real source even though nothing lives on disk. Collection is scoped to the
   top-level defs of that same `__vis_src__`, so the shared-across-blocks globals
   dict never leaks stale `test_*` from an earlier cell into a run."
  "# vis sandbox pytest-compat shim.
#
# The agent sandbox ships no third-party `pytest` wheel. This shim publishes a
# pytest-compatible module implemented entirely on the stdlib (ast/inspect/
# linecache/traceback), so a model writing a full Python extension can write
# `def test_*` + `pytest.main()` inline and get real pass/fail reporting with
# assert introspection. No pluggy, no conftest discovery, no CLI, no import
# rewrite. Published into sys.modules so `import pytest` works, and stapled onto
# builtins so pytest.raises(...) needs no import (mirrors json/os/requests).

def __vis_install_pytest_compat__():
    import sys
    import types
    import inspect
    import linecache
    import ast
    import io
    import time
    import warnings as _warnings
    import traceback as _tb

    _NL = chr(10)
    _NOTSET = object()
    _PROG = '<prog>'
    _FIXTURE_ATTR = '__vis_pytest_fixture__'
    _MARKS_ATTR = '__vis_pytest_marks__'

    # ---- outcome exceptions -------------------------------------------------
    class OutcomeException(Exception):
        def __init__(self, msg=''):
            super().__init__(msg)
            self.msg = msg

    class Skipped(OutcomeException):
        pass

    class Failed(OutcomeException):
        pass

    class XFailed(OutcomeException):
        pass

    class Exit(Exception):
        def __init__(self, msg='', returncode=None):
            super().__init__(msg)
            self.msg = msg
            self.returncode = returncode

    class UsageError(Exception):
        pass

    def fail(reason='', pytrace=True):
        raise Failed(reason)

    def skip(reason='', allow_module_level=False):
        raise Skipped(reason)

    def xfail(reason=''):
        raise XFailed(reason)

    def exit(reason='', returncode=None):
        raise Exit(reason, returncode)

    def importorskip(modname, minversion=None, reason=None):
        try:
            return __import__(modname)
        except ImportError:
            raise Skipped(reason or ('could not import ' + str(modname)))

    # ---- approx -------------------------------------------------------------
    class approx:
        def __init__(self, expected, rel=None, abs=None, nan_ok=False):
            self.expected = expected
            self.rel = rel
            self.abs = abs
            self.nan_ok = nan_ok

        def _child(self, v):
            return approx(v, rel=self.rel, abs=self.abs, nan_ok=self.nan_ok)

        def _scalar(self, actual, expected):
            if expected != expected:
                return self.nan_ok and actual != actual
            if actual == expected:
                return True
            rel = self.rel
            ab = self.abs
            if rel is None and ab is None:
                rel = 1e-06
                ab = 1e-12
            tol = 0.0
            if ab is not None:
                tol = max(tol, ab)
            if rel is not None:
                try:
                    tol = max(tol, rel * builtins_abs(expected))
                except TypeError:
                    pass
            try:
                return builtins_abs(actual - expected) <= tol
            except TypeError:
                return actual == expected

        def __eq__(self, actual):
            exp = self.expected
            if isinstance(exp, dict) and isinstance(actual, dict):
                if set(exp) != set(actual):
                    return False
                return all(self._child(exp[k]) == actual[k] for k in exp)
            if isinstance(exp, (list, tuple)) and isinstance(actual, (list, tuple)):
                if len(exp) != len(actual):
                    return False
                return all(self._child(e) == a for e, a in zip(exp, actual))
            return self._scalar(actual, exp)

        def __ne__(self, actual):
            return not self.__eq__(actual)

        def __repr__(self):
            return 'approx(' + repr(self.expected) + ')'

    builtins_abs = abs

    # ---- raises / warns -----------------------------------------------------
    class ExceptionInfo:
        def __init__(self, tup):
            self._tup = tup

        @property
        def type(self):
            return self._tup[0]

        @property
        def value(self):
            return self._tup[1]

        @property
        def tb(self):
            return self._tup[2]

        @property
        def typename(self):
            t = self._tup[0]
            return getattr(t, '__name__', str(t))

        def match(self, regexp):
            import re as _re
            s = str(self.value)
            if _re.search(regexp, s) is None:
                raise AssertionError('regex ' + repr(regexp) + ' does not match ' + repr(s))
            return True

        def __repr__(self):
            return '<ExceptionInfo ' + repr(self.value) + '>'

    class RaisesContext:
        def __init__(self, expected, match=None):
            self.expected = expected
            self.match_expr = match
            self.excinfo = None

        def __enter__(self):
            self.excinfo = ExceptionInfo((None, None, None))
            return self.excinfo

        def __exit__(self, exc_type, exc_val, exc_tb):
            if exc_type is None:
                raise Failed('DID NOT RAISE ' + _name_of(self.expected))
            if not issubclass(exc_type, self.expected):
                return False
            self.excinfo._tup = (exc_type, exc_val, exc_tb)
            if self.match_expr is not None:
                self.excinfo.match(self.match_expr)
            return True

    def raises(expected_exception, *args, **kwargs):
        match = kwargs.pop('match', None)
        if not args:
            return RaisesContext(expected_exception, match=match)
        func = args[0]
        try:
            func(*args[1:], **kwargs)
        except expected_exception as e:
            info = ExceptionInfo((type(e), e, e.__traceback__))
            if match is not None:
                info.match(match)
            return info
        raise Failed('DID NOT RAISE ' + _name_of(expected_exception))

    class WarningsChecker:
        def __init__(self, expected):
            self.expected = expected
            self._cm = None
            self.caught = []

        def __enter__(self):
            self._cm = _warnings.catch_warnings(record=True)
            self.caught = self._cm.__enter__()
            _warnings.simplefilter('always')
            return self.caught

        def __exit__(self, exc_type, exc_val, exc_tb):
            self._cm.__exit__(exc_type, exc_val, exc_tb)
            if exc_type is not None:
                return False
            if self.expected is not None:
                ok = any(issubclass(w.category, self.expected) for w in self.caught)
                if not ok:
                    raise Failed('DID NOT WARN ' + _name_of(self.expected))
            return False

    def warns(expected_warning=Warning, *args, **kwargs):
        if not args:
            return WarningsChecker(expected_warning)
        func = args[0]
        with WarningsChecker(expected_warning):
            return func(*args[1:], **kwargs)

    def _name_of(x):
        return getattr(x, '__name__', str(x))

    # ---- fixtures -----------------------------------------------------------
    class FixtureInfo:
        def __init__(self, func, scope='function', params=None, autouse=False, name=None, ids=None):
            self.func = func
            self.scope = scope
            self.params = params
            self.autouse = autouse
            self.name = name or func.__name__
            self.ids = ids

    def fixture(func=None, scope='function', params=None, autouse=False, name=None, ids=None):
        def wrap(f):
            f.__dict__[_FIXTURE_ATTR] = FixtureInfo(f, scope=scope, params=params, autouse=autouse, name=name, ids=ids)
            return f
        if func is not None and callable(func):
            return wrap(func)
        return wrap

    class FixtureRequest:
        def __init__(self, manager, nodeid):
            self._manager = manager
            self.nodeid = nodeid
            self.param = None
            self._finalizers = []

        def getfixturevalue(self, name):
            return self._manager.resolve(name, self)

        def addfinalizer(self, fin):
            self._finalizers.append(fin)

        @property
        def node(self):
            return self

    # builtin fixtures (function scoped, fresh + torn down per test) ----------
    class CaptureResult:
        def __init__(self, out, err):
            self.out = out
            self.err = err

        def __iter__(self):
            return iter((self.out, self.err))

        def __getitem__(self, i):
            return (self.out, self.err)[i]

    class CaptureFixture:
        def __init__(self, fd=False):
            self._out = io.StringIO()
            self._err = io.StringIO()
            self._old = None

        def _start(self):
            self._old = (sys.stdout, sys.stderr)
            sys.stdout = self._out
            sys.stderr = self._err

        def _stop(self):
            if self._old is not None:
                sys.stdout, sys.stderr = self._old
                self._old = None

        def readouterr(self):
            o = self._out.getvalue()
            e = self._err.getvalue()
            self._out.seek(0)
            self._out.truncate(0)
            self._err.seek(0)
            self._err.truncate(0)
            return CaptureResult(o, e)

    class MonkeyPatch:
        def __init__(self):
            self._undo = []

        def _resolve_target(self, dotted):
            import importlib
            parts = dotted.split('.')
            for i in range(len(parts) - 1, 0, -1):
                modname = '.'.join(parts[:i])
                try:
                    obj = importlib.import_module(modname)
                except Exception:
                    continue
                for p in parts[i:-1]:
                    obj = getattr(obj, p)
                return obj, parts[-1]
            raise Failed('could not resolve monkeypatch target ' + repr(dotted))

        def setattr(self, target, name, value=_NOTSET, raising=True):
            if isinstance(target, str):
                value = name
                target, name = self._resolve_target(target)
            old = getattr(target, name, _NOTSET)
            if raising and old is _NOTSET and not hasattr(target, name):
                raise AttributeError(name)
            self._undo.append(('attr', target, name, old))
            setattr(target, name, value)

        def delattr(self, target, name=_NOTSET, raising=True):
            if isinstance(target, str):
                target, name = self._resolve_target(target)
            old = getattr(target, name, _NOTSET)
            if old is _NOTSET:
                if raising:
                    raise AttributeError(name)
                return
            self._undo.append(('attr', target, name, old))
            delattr(target, name)

        def setitem(self, dic, name, value):
            old = dic[name] if name in dic else _NOTSET
            self._undo.append(('item', dic, name, old))
            dic[name] = value

        def delitem(self, dic, name, raising=True):
            if name not in dic:
                if raising:
                    raise KeyError(name)
                return
            self._undo.append(('item', dic, name, dic[name]))
            del dic[name]

        def setenv(self, name, value, prepend=None):
            import os
            old = os.environ[name] if name in os.environ else _NOTSET
            self._undo.append(('env', name, old))
            v = str(value)
            if prepend and name in os.environ:
                v = v + prepend + os.environ[name]
            os.environ[name] = v

        def delenv(self, name, raising=True):
            import os
            if name not in os.environ:
                if raising:
                    raise KeyError(name)
                return
            self._undo.append(('env', name, os.environ[name]))
            del os.environ[name]

        def syspath_prepend(self, path):
            self._undo.append(('syspath', None, None, None))
            sys.path.insert(0, str(path))

        def chdir(self, path):
            import os
            self._undo.append(('cwd', os.getcwd(), None, None))
            os.chdir(str(path))

        def undo(self):
            for entry in reversed(self._undo):
                kind = entry[0]
                try:
                    if kind == 'attr':
                        _, tgt, nm, old = entry
                        if old is _NOTSET:
                            delattr(tgt, nm)
                        else:
                            setattr(tgt, nm, old)
                    elif kind == 'item':
                        _, dic, nm, old = entry
                        if old is _NOTSET:
                            if nm in dic:
                                del dic[nm]
                        else:
                            dic[nm] = old
                    elif kind == 'env':
                        import os
                        _, nm, old = entry
                        if old is _NOTSET:
                            os.environ.pop(nm, None)
                        else:
                            os.environ[nm] = old
                    elif kind == 'syspath':
                        try:
                            sys.path.pop(0)
                        except Exception:
                            pass
                    elif kind == 'cwd':
                        import os
                        os.chdir(entry[1])
                except Exception:
                    pass
            self._undo = []

    def _bi_monkeypatch(request):
        mp = MonkeyPatch()
        return mp, mp.undo

    def _bi_capsys(request):
        cap = CaptureFixture()
        cap._start()
        return cap, cap._stop

    # NB: no tmp_path / capfd — this sandbox has NO real filesystem, so
    # tempfile/os writes raise an uncatchable host SecurityException. Only
    # sys-level, FS-free builtin fixtures are offered.
    _BUILTIN_FIXTURES = {
        'monkeypatch': _bi_monkeypatch,
        'capsys': _bi_capsys,
    }

    class FixtureManager:
        def __init__(self, fixtures):
            self.fixtures = fixtures
            self.cache = {}
            self.active = {'function': [], 'module': [], 'session': []}
            self._per_test = {}

        def begin_test(self):
            self._per_test = {}

        def has(self, name):
            return name == 'request' or name in self.fixtures or name in _BUILTIN_FIXTURES

        def resolve(self, name, request):
            if name == 'request':
                return request
            if name in self._per_test:
                return self._per_test[name]
            if name in _BUILTIN_FIXTURES:
                val, td = _BUILTIN_FIXTURES[name](request)
                self._per_test[name] = val
                self.active['function'].append(('fn', td))
                return val
            info = self.fixtures.get(name)
            if info is None:
                raise Failed('fixture ' + repr(name) + ' not found')
            scope = info.scope if info.scope in ('function', 'module', 'session') else 'function'
            key = (scope, name)
            if scope in ('module', 'session') and key in self.cache:
                return self.cache[key]
            kwargs = {}
            for pname in inspect.signature(info.func).parameters:
                if self.has(pname):
                    kwargs[pname] = self.resolve(pname, request)
            result = info.func(**kwargs)
            if inspect.isgenerator(result):
                gen = result
                val = next(gen)
                self.active[scope].append(('gen', gen))
                if scope in ('module', 'session'):
                    self.cache[key] = val
                if scope == 'function':
                    self._per_test[name] = val
                return val
            if scope in ('module', 'session'):
                self.cache[key] = result
            if scope == 'function':
                self._per_test[name] = result
            return result

        def teardown(self, scope):
            items = self.active.get(scope, [])
            while items:
                kind, obj = items.pop()
                try:
                    if kind == 'gen':
                        next(obj)
                    else:
                        obj()
                except StopIteration:
                    pass
                except Exception:
                    pass
            for k in list(self.cache):
                if k[0] == scope:
                    del self.cache[k]
            if scope == 'function':
                self._per_test = {}

    # ---- marks --------------------------------------------------------------
    class Mark:
        def __init__(self, name, args=(), kwargs=None):
            self.name = name
            self.args = args
            self.kwargs = kwargs or {}

    class MarkDecorator:
        def __init__(self, name):
            self.name = name

        def _attach(self, f, mark):
            marks = list(getattr(f, _MARKS_ATTR, []))
            marks.append(mark)
            f.__dict__[_MARKS_ATTR] = marks
            return f

        def __call__(self, *args, **kwargs):
            if len(args) == 1 and callable(args[0]) and not kwargs and not isinstance(args[0], MarkDecorator):
                return self._attach(args[0], Mark(self.name, (), {}))

            def deco(f):
                return self._attach(f, Mark(self.name, args, kwargs))
            return deco

    class MarkGenerator:
        def __getattr__(self, name):
            return MarkDecorator(name)

    mark = MarkGenerator()

    class ParamSet:
        def __init__(self, values, marks=(), id=None):
            self.values = values
            if not isinstance(marks, (list, tuple)):
                marks = (marks,)
            self.marks = marks
            self.id = id

    def param(*values, **kwargs):
        return ParamSet(values, marks=kwargs.get('marks', ()), id=kwargs.get('id'))

    def _param_id(v):
        if isinstance(v, bool):
            return 'True' if v else 'False'
        if isinstance(v, (int, float, str)):
            return str(v)
        if v is None:
            return 'None'
        return type(v).__name__

    def _mark_to_case(m):
        # turn a pytest.param()-level mark object into a Mark
        if isinstance(m, MarkDecorator):
            return Mark(m.name, (), {})
        if isinstance(m, Mark):
            return m
        return None

    def _expand_params(marks):
        psets = None
        for m in marks:
            if m.name != 'parametrize':
                continue
            argnames = m.args[0]
            argvalues = list(m.args[1])
            if isinstance(argnames, str):
                names = [a.strip() for a in argnames.split(',') if a.strip()]
            else:
                names = list(argnames)
            ids = m.kwargs.get('ids')
            cur = []
            for i, val in enumerate(argvalues):
                casemarks = []
                caseid = None
                if isinstance(val, ParamSet):
                    caseid = val.id
                    for pm in val.marks:
                        mk = _mark_to_case(pm)
                        if mk is not None:
                            casemarks.append(mk)
                    val = val.values if len(names) > 1 else val.values[0]
                if len(names) == 1:
                    kw = {names[0]: val}
                    idpart = caseid or (ids[i] if ids else _param_id(val))
                else:
                    kw = dict(zip(names, val))
                    idpart = caseid or (ids[i] if ids else '-'.join(_param_id(x) for x in val))
                cur.append((idpart, kw, casemarks))
            if psets is None:
                psets = cur
            else:
                combined = []
                for id1, kw1, mk1 in psets:
                    for id2, kw2, mk2 in cur:
                        merged = dict(kw1)
                        merged.update(kw2)
                        combined.append((id2 + '-' + id1, merged, mk1 + mk2))
                psets = combined
        if psets is None:
            return [('', {}, [])]
        return psets

    # ---- assert introspection ----------------------------------------------
    def _register_src(src):
        if src is None:
            return
        lines = [ln + _NL for ln in src.split(_NL)]
        linecache.cache[_PROG] = (len(src), None, lines, _PROG)

    def _safe_eval(node, local, glob):
        try:
            code = compile(ast.Expression(node), '<assert>', 'eval')
            return True, eval(code, glob, local)
        except Exception as e:
            return False, e

    def _seg(node, src):
        try:
            s = ast.get_source_segment(src, node)
            return s if s is not None else '<expr>'
        except Exception:
            return '<expr>'

    def _render_assert(node, local, glob, src):
        test = node.test
        lines = ['assert ' + _seg(test, src)]
        if isinstance(test, ast.Compare) and len(test.ops) == 1:
            left = test.left
            right = test.comparators[0]
            okl, lv = _safe_eval(left, local, glob)
            okr, rv = _safe_eval(right, local, glob)
            if okl and not isinstance(left, ast.Constant):
                lines.append('  where ' + repr(lv) + ' = ' + _seg(left, src))
            if okr and not isinstance(right, ast.Constant):
                lines.append('  and   ' + repr(rv) + ' = ' + _seg(right, src))
        elif isinstance(test, ast.BoolOp):
            for v in test.values:
                ok, val = _safe_eval(v, local, glob)
                if ok and not isinstance(v, ast.Constant):
                    lines.append('  where ' + repr(val) + ' = ' + _seg(v, src))
        elif isinstance(test, ast.UnaryOp) and isinstance(test.op, ast.Not):
            ok, val = _safe_eval(test.operand, local, glob)
            if ok and not isinstance(test.operand, ast.Constant):
                lines.append('  where ' + repr(val) + ' = ' + _seg(test.operand, src))
        elif isinstance(test, ast.Call):
            for a in test.args:
                ok, val = _safe_eval(a, local, glob)
                if ok and not isinstance(a, ast.Constant):
                    lines.append('  where ' + repr(val) + ' = ' + _seg(a, src))
        else:
            ok, val = _safe_eval(test, local, glob)
            if ok and not isinstance(test, ast.Constant):
                lines.append('  where ' + repr(val) + ' = ' + _seg(test, src))
        return _NL.join(lines)

    def _explain_from_tb(tb, src):
        if src is None or tb is None:
            return None
        target = None
        t = tb
        while t is not None:
            if t.tb_frame.f_code.co_filename == _PROG:
                target = t
            t = t.tb_next
        if target is None:
            return None
        lineno = target.tb_lineno
        local = dict(target.tb_frame.f_locals)
        glob = target.tb_frame.f_globals
        try:
            tree = ast.parse(src)
        except Exception:
            return None
        node = None
        for n in ast.walk(tree):
            if isinstance(n, ast.Assert):
                s = n.lineno
                e = getattr(n, 'end_lineno', n.lineno)
                if s <= lineno <= e:
                    node = n
                    break
        if node is None:
            return None
        try:
            return _render_assert(node, local, glob, src)
        except Exception:
            return None

    def _render_failure(exc, src):
        _register_src(src)
        tb = exc.__traceback__
        if tb is not None:
            tb = tb.tb_next
        out = []
        entries = _tb.extract_tb(tb) if tb is not None else []
        for fr in entries:
            out.append('  ' + str(fr.filename) + ':' + str(fr.lineno) + ' in ' + str(fr.name))
            if fr.line:
                out.append('      ' + fr.line)
        if isinstance(exc, AssertionError):
            expl = _explain_from_tb(tb, src)
            if expl:
                for ln in expl.split(_NL):
                    out.append('E   ' + ln)
            else:
                msg = str(exc)
                out.append('E   AssertionError' + ((': ' + msg) if msg else ''))
        elif isinstance(exc, Failed):
            out.append('E   Failed: ' + str(exc.msg))
        else:
            out.append('E   ' + type(exc).__name__ + ': ' + str(exc))
        return _NL.join(out)

    # ---- collection ---------------------------------------------------------
    def _current_block_names(src):
        # top-level def/class names in THIS block, so shared globals do not leak
        # stale test_* from an earlier cell into the run.
        if src is None:
            return None
        try:
            tree = ast.parse(src)
        except Exception:
            return None
        names = []
        for n in tree.body:
            if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                names.append(n.name)
        return names

    def _collect(ns, src):
        allow = _current_block_names(src)
        items = []
        for name in list(ns.keys()):
            if allow is not None and name not in allow:
                continue
            obj = ns.get(name)
            if name.startswith('test') and inspect.isfunction(obj):
                items.append(('func', name, obj, None, obj.__code__.co_firstlineno))
            elif name.startswith('Test') and inspect.isclass(obj) and getattr(obj, '__test__', True):
                methods = []
                for mname, meth in vars(obj).items():
                    if mname.startswith('test') and inspect.isfunction(meth):
                        methods.append((mname, meth, meth.__code__.co_firstlineno))
                methods.sort(key=lambda t: t[2])
                for mname, meth, lno in methods:
                    items.append(('method', name + '::' + mname, meth, obj, lno))
        items.sort(key=lambda t: t[4])
        return items

    # ---- results ------------------------------------------------------------
    class _Result:
        def __init__(self, nodeid):
            self.nodeid = nodeid
            self.outcome = 'passed'
            self.longrepr = ''
            self.duration = 0.0

    _CHAR = {'passed': '.', 'failed': 'F', 'error': 'E', 'skipped': 's', 'xfailed': 'x', 'xpassed': 'X'}

    def _run_one(nodeid, func, cls, pkwargs, marks, fm, src):
        r = _Result(nodeid)
        skip_reason = _NOTSET
        xfail_mark = None
        for m in marks:
            if m.name == 'skip':
                skip_reason = m.kwargs.get('reason', '') or (m.args[0] if m.args else '')
            elif m.name == 'skipif':
                cond = m.args[0] if m.args else m.kwargs.get('condition', False)
                if cond:
                    skip_reason = m.kwargs.get('reason', 'condition true')
            elif m.name == 'xfail':
                xfail_mark = m
        if skip_reason is not _NOTSET:
            r.outcome = 'skipped'
            r.longrepr = 'SKIPPED ' + nodeid + ((': ' + str(skip_reason)) if skip_reason else '')
            return r
        fm.begin_test()
        request = FixtureRequest(fm, nodeid)
        callargs = dict(pkwargs)
        try:
            for info in fm.fixtures.values():
                if info.autouse:
                    fm.resolve(info.name, request)
            for pname in inspect.signature(func).parameters:
                if pname in callargs or pname == 'self':
                    continue
                if fm.has(pname):
                    callargs[pname] = fm.resolve(pname, request)
            t0 = time.time()
            if cls is not None:
                inst = cls()
                if hasattr(inst, 'setup_method'):
                    try:
                        inst.setup_method(func)
                    except TypeError:
                        inst.setup_method()
                try:
                    func(inst, **callargs)
                finally:
                    if hasattr(inst, 'teardown_method'):
                        try:
                            try:
                                inst.teardown_method(func)
                            except TypeError:
                                inst.teardown_method()
                        except Exception:
                            pass
            else:
                func(**callargs)
            r.duration = time.time() - t0
            for fin in reversed(request._finalizers):
                try:
                    fin()
                except Exception:
                    pass
            if xfail_mark is not None:
                if xfail_mark.kwargs.get('strict'):
                    r.outcome = 'failed'
                    r.longrepr = '[XPASS(strict)] ' + nodeid + ' ' + str(xfail_mark.kwargs.get('reason', ''))
                else:
                    r.outcome = 'xpassed'
            else:
                r.outcome = 'passed'
        except Skipped as e:
            r.outcome = 'skipped'
            r.longrepr = 'SKIPPED ' + nodeid + ((': ' + str(e.msg)) if e.msg else '')
        except XFailed as e:
            r.outcome = 'xfailed'
            r.longrepr = 'XFAIL ' + nodeid + ((': ' + str(e.msg)) if e.msg else '')
        except (AssertionError, Failed) as e:
            if xfail_mark is not None:
                r.outcome = 'xfailed'
                r.longrepr = 'XFAIL ' + nodeid
            else:
                r.outcome = 'failed'
                r.longrepr = _render_failure(e, src)
        except Exception as e:
            if xfail_mark is not None:
                r.outcome = 'xfailed'
                r.longrepr = 'XFAIL ' + nodeid
            else:
                r.outcome = 'error'
                r.longrepr = _render_failure(e, src)
        finally:
            fm.teardown('function')
        return r

    def _summary(results, write, elapsed):
        counts = {}
        for r in results:
            counts[r.outcome] = counts.get(r.outcome, 0) + 1
        fails = [r for r in results if r.outcome in ('failed', 'error')]
        if fails:
            write(_NL + _NL + '=' * 26 + ' FAILURES ' + '=' * 26 + _NL)
            for r in fails:
                title = ' ' + r.nodeid + ' '
                pad = max(0, (62 - len(title)) // 2)
                write(_NL + '_' * pad + title + '_' * pad + _NL)
                write(r.longrepr + _NL)
        skips = [r for r in results if r.outcome == 'skipped' and r.longrepr]
        if skips:
            write(_NL + '------- skipped -------' + _NL)
            for r in skips:
                write(r.longrepr + _NL)
        order = ['failed', 'error', 'passed', 'skipped', 'xfailed', 'xpassed']
        label = {'failed': 'failed', 'error': 'errors', 'passed': 'passed', 'skipped': 'skipped', 'xfailed': 'xfailed', 'xpassed': 'xpassed'}
        parts = []
        for k in order:
            if counts.get(k):
                parts.append(str(counts[k]) + ' ' + label[k])
        tail = (', '.join(parts) if parts else 'no tests ran') + ' in ' + ('%.2f' % elapsed) + 's'
        line = ' ' + tail + ' '
        pad = max(0, (62 - len(line)) // 2)
        write(_NL + '=' * pad + line + '=' * pad + _NL)
        return 1 if (counts.get('failed', 0) + counts.get('error', 0)) else 0

    def main(args=None, ns=None):
        verbose = False
        if args:
            if isinstance(args, str):
                args = [args]
            verbose = any(a in ('-v', '--verbose', '-vv', '-vvv') for a in args)
        if ns is None:
            ns = sys._getframe(1).f_globals
        src = ns.get('__vis_src__')
        fixtures = {}
        allow = _current_block_names(src)
        for nm, obj in list(ns.items()):
            info = getattr(obj, _FIXTURE_ATTR, None)
            if info is not None and (allow is None or nm in allow or info.name in (allow or [])):
                fixtures[info.name] = info
        fm = FixtureManager(fixtures)
        tests = _collect(ns, src)
        _buf = []
        write = _buf.append
        write(_NL + 'vis-pytest: collected ' + str(len(tests)) + ' item' + ('' if len(tests) == 1 else 's') + _NL + _NL)
        results = []
        t_start = time.time()
        for kind, nodeid, func, cls, _ln in tests:
            base_marks = list(getattr(func, _MARKS_ATTR, []))
            for pid, pkwargs, casemarks in _expand_params(base_marks):
                full_id = nodeid + (('[' + pid + ']') if pid else '')
                r = _run_one(full_id, func, cls, pkwargs, base_marks + casemarks, fm, src)
                results.append(r)
                if verbose:
                    write(full_id + ' ' + r.outcome.upper() + _NL)
                else:
                    write(_CHAR.get(r.outcome, '?'))
        fm.teardown('module')
        fm.teardown('session')
        elapsed = time.time() - t_start
        rc = _summary(results, write, elapsed)
        sys.stdout.write(''.join(_buf))
        sys.stdout.flush()
        # Publish the PER-TEST records (nodeid, outcome, longrepr) as the ONE
        # source of truth. The host derives counts from THIS list, so a bad
        # internal tally can never disagree with what actually ran.
        mod._vis_last_report = [(_r.nodeid, _r.outcome, (_r.longrepr or '')) for _r in results]
        return rc

    # ---- publish module -----------------------------------------------------
    mod = types.ModuleType('pytest')
    mod.__doc__ = 'vis pytest-compatible shim (pure Python stdlib, no plugins/CLI/conftest).'
    mod.__version__ = '8.0-vis'
    mod.raises = raises
    mod.warns = warns
    mod.approx = approx
    mod.fixture = fixture
    mod.mark = mark
    mod.param = param
    mod.fail = fail
    mod.skip = skip
    mod.xfail = xfail
    mod.exit = exit
    mod.importorskip = importorskip
    mod.main = main
    mod.FixtureRequest = FixtureRequest
    mod.ExceptionInfo = ExceptionInfo
    mod.OutcomeException = OutcomeException
    mod.Skipped = Skipped
    mod.Failed = Failed
    mod.XFailed = XFailed
    mod.UsageError = UsageError
    mod.MonkeyPatch = MonkeyPatch
    mod.__all__ = ['raises', 'warns', 'approx', 'fixture', 'mark', 'param', 'fail', 'skip', 'xfail', 'exit', 'importorskip', 'main']

    sys.modules['pytest'] = mod

    # Autoload: staple onto builtins so pytest.raises(...) works in every
    # run_python block WITHOUT an explicit `import pytest` (mirrors json/os).
    try:
        import builtins as _b
        _b.pytest = mod
    except Exception:
        pass

__vis_install_pytest_compat__()
del __vis_install_pytest_compat__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pytest"
     :ext/description
     "Sandbox shim: a `pytest`-compatible module (import pytest / pytest.main / @pytest.fixture / pytest.raises) implemented PURELY in stdlib Python. Real assert introspection via linecache+ast, fixtures, parametrize, marks, monkeypatch/capsys/tmp_path — no pip, no wheel, no host bridge, no plugins/CLI."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pytest"
       :shim/description
       "pytest-compatible `pytest` on the stdlib — collection, assert introspection, fixtures, parametrize, marks, raises/approx, `pytest.main()`. Not supported: test-file discovery, CLI, and import-time assertion rewrite (write tests + `pytest.main()` in one block)."
       :shim/preamble pytest-compat-shim-src}]}))

(vis/register-extension! vis-extension)
