(ns com.blockether.vis.internal.foundation.shim-toml
  "Built-in sandbox SHIM: a `toml`-compatible module for the model's Python
   sandbox — NO host/JVM bridge. The `toml` PyPI package is not in GraalPy, so
   agents that reach for `import toml` would otherwise hit ModuleNotFoundError;
   this extension contributes a `:ext/sandbox-shims` entry that env-python
   installs into every sandbox Context (main + every `sub_loop` fork).

   Reading (`toml.loads`/`toml.load`) delegates to the stdlib `tomllib` (present
   in GraalPy's 3.11 stdlib) for a spec-correct parse; writing
   (`toml.dumps`/`toml.dump`) is a pure-Python serializer covering scalars,
   arrays, inline tables, nested `[table]` sections and `[[array.of.tables]]`.
   A correctness-focused SUBSET of the `toml` package API.

   Like `shim-numpy` there are NO `:shim/bindings`: a self-contained Python
   preamble with zero host callables. Publishes a `toml` module into
   `sys.modules` (so `import toml` works) and staples it onto builtins."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private toml-compat-shim-src
  "Pure-Python preamble that publishes a `toml`-compatible module. Reads via the
   stdlib `tomllib`; writes with an inlined serializer. Zero host callables.
   Published into `sys.modules` under `toml` and stapled onto builtins. INLINED
   so it ships in-jar with no separate `.py` resource. Installed once per sandbox
   context, BEFORE the baseline snapshot so its `__vis_*` names are filtered from
   the live-vars view. The Python body uses ONLY single-quoted string literals
   and `chr(...)` for special chars, so this Clojure string needs zero backslash
   escaping."
  "# vis sandbox toml-compat shim.
#
# The agent sandbox ships no `toml` wheel. Reading delegates to the stdlib
# `tomllib` (present in GraalPy 3.11+); writing is a pure-Python serializer.
# A correctness-focused SUBSET of the `toml` package API.

def __vis_install_toml__():
    import sys, types, math, builtins as _bi
    import datetime as _dt
    try:
        import tomllib as _tl
    except Exception:
        _tl = None

    _NL = chr(10)
    _DQ = chr(34)
    _BS = chr(92)

    class TomlDecodeError(Exception):
        pass

    def loads(s, **kw):
        if _tl is None:
            raise RuntimeError('tomllib not available in this sandbox')
        if isinstance(s, bytes):
            s = s.decode('utf-8')
        try:
            return _tl.loads(s)
        except Exception as e:
            raise TomlDecodeError(str(e))

    def load(f, **kw):
        if hasattr(f, 'read'):
            data = f.read()
            if isinstance(data, bytes):
                return loads(data.decode('utf-8'))
            return loads(data)
        # treat as path
        fh = open(f, 'r')
        try:
            return loads(fh.read())
        finally:
            fh.close()

    def _esc(s):
        out = []
        for ch in s:
            o = ord(ch)
            if ch == _DQ:
                out.append(_BS + _DQ)
            elif ch == _BS:
                out.append(_BS + _BS)
            elif ch == _NL:
                out.append(_BS + 'n')
            elif ch == chr(9):
                out.append(_BS + 't')
            elif ch == chr(13):
                out.append(_BS + 'r')
            elif o < 32:
                out.append(_BS + 'u' + ('%04x' % o))
            else:
                out.append(ch)
        return ''.join(out)

    def _fmt_scalar(v):
        if isinstance(v, bool):
            return 'true' if v else 'false'
        if isinstance(v, int):
            return str(v)
        if isinstance(v, float):
            if math.isnan(v):
                return 'nan'
            if math.isinf(v):
                return 'inf' if v > 0 else '-inf'
            return repr(v)
        if isinstance(v, str):
            return _DQ + _esc(v) + _DQ
        if isinstance(v, (_dt.datetime, _dt.date, _dt.time)):
            return v.isoformat()
        if isinstance(v, (list, tuple)):
            return '[' + ', '.join(_fmt_scalar(x) for x in v) + ']'
        if isinstance(v, dict):
            return '{ ' + ', '.join(_fmt_key(k) + ' = ' + _fmt_scalar(val) for k, val in v.items()) + ' }'
        raise TypeError('cannot serialize ' + type(v).__name__)

    _BARE = set('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-')

    def _fmt_key(k):
        k = str(k)
        if k and all(c in _BARE for c in k):
            return k
        return _DQ + _esc(k) + _DQ

    def _is_table_array(v):
        return isinstance(v, (list, tuple)) and len(v) > 0 and all(isinstance(x, dict) for x in v)

    def _dump_table(d, prefix, out):
        # scalars first
        for k, v in d.items():
            if isinstance(v, dict):
                continue
            if _is_table_array(v):
                continue
            out.append(_fmt_key(k) + ' = ' + _fmt_scalar(v))
        # sub-tables
        for k, v in d.items():
            path = prefix + [k]
            if isinstance(v, dict):
                header = '.'.join(_fmt_key(p) for p in path)
                out.append('')
                out.append('[' + header + ']')
                _dump_table(v, path, out)
            elif _is_table_array(v):
                header = '.'.join(_fmt_key(p) for p in path)
                for item in v:
                    out.append('')
                    out.append('[[' + header + ']]')
                    _dump_table(item, path, out)

    def dumps(obj, **kw):
        if not isinstance(obj, dict):
            raise TypeError('toml.dumps requires a dict at the top level')
        out = []
        _dump_table(obj, [], out)
        text = _NL.join(out)
        while text.startswith(_NL):
            text = text[1:]
        return text + _NL if text else ''

    def dump(obj, f, **kw):
        s = dumps(obj)
        f.write(s)
        return s

    mod = types.ModuleType('toml')
    mod.loads = loads
    mod.load = load
    mod.dumps = dumps
    mod.dump = dump
    mod.TomlDecodeError = TomlDecodeError
    mod.__version__ = '0.10.2-vis-shim'
    sys.modules['toml'] = mod
    try:
        _bi.toml = mod
    except Exception:
        pass

__vis_install_toml__()
del __vis_install_toml__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-toml"
     :ext/description
     "Sandbox shim: a `toml`-compatible module (import toml). Reading delegates to the stdlib tomllib for a spec-correct parse; writing is a pure-Python serializer (scalars/arrays/inline tables/nested sections/array-of-tables). No pip, no native wheel, no host bridge. A correctness-focused subset."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "toml"
       :shim/description
       "toml-compatible `toml` module: loads/load via stdlib tomllib, dumps/dump via a pure-Python serializer. No host bridge."
       :shim/preamble toml-compat-shim-src}]}))

(vis/register-extension! vis-extension)
