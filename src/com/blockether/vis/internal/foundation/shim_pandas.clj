(ns com.blockether.vis.internal.foundation.shim-pandas
  "Built-in sandbox SHIM: a `pandas`-compatible module for the model's Python
   sandbox, implemented in PURE Python (stdlib csv/json/math) — NO host/JVM
   bridge, NOT a line of Clojure or babashka. pandas is a native/heavy wheel that
   does not ship in GraalPy, so agents that reach for `import pandas` would
   otherwise hit ModuleNotFoundError; this extension contributes a
   `:ext/sandbox-shims` entry that env-python installs into every sandbox Context
   (main + every `sub_loop` fork).

   The shim is a correctness-focused SUBSET, not C-speed pandas: a `Series` is a
   labelled 1-D column, a `DataFrame` is an ordered dict of columns. It covers
   construction (dict / records / list-of-lists / read_csv / read_json),
   `[]`/`loc`/`iloc` selection, boolean masking, column arithmetic, `groupby`
   (sum/mean/min/max/count/size/agg), `merge` (inner/left/right/outer),
   `concat`, `sort_values`, `describe`, `fillna`/`dropna`, `apply`, a `.str`
   accessor, `to_dict`/`to_csv`/`to_json` and a pandas-style `__repr__`. It
   interoperates with the numpy shim (`.values`) when present. Big frames are
   slow; the goal is that agent glue code just works.

   Like `shim-numpy` there are NO `:shim/bindings`: the shim is a self-contained
   Python preamble with zero host callables. It publishes a `pandas` module into
   `sys.modules` (so `import pandas` works) and staples it onto builtins (so
   `pandas.DataFrame(...)` works with NO import, like json/os)."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private pandas-compat-shim-src
  "Pure-Python preamble that publishes a `pandas`-compatible module implemented on
   the stdlib (csv/json/math). Zero host callables. Published into `sys.modules`
   under `pandas` (so `import pandas` finds it) AND stapled onto builtins (so
   `pandas.DataFrame(...)` works with NO import). INLINED here so it ships in-jar
   with no separate `.py` resource. Installed once per sandbox context (main +
   every `sub_loop` fork), BEFORE the baseline snapshot so its `__vis_*` names are
   filtered out of the model-visible live-vars view. The Python body uses ONLY
   single-quoted string literals and `chr(...)` for any special char, so this
   Clojure string needs zero backslash escaping."
  "# vis sandbox pandas-compat shim.
#
# The agent sandbox ships no pandas wheel. This shim publishes a pandas-compatible
# module implemented in PURE Python (stdlib only, interoperates with the numpy
# shim when present). Series = labelled 1-D column; DataFrame = ordered dict of
# columns. A deliberate correctness-focused SUBSET, not C-speed pandas.

def __vis_install_pandas__():
    import sys, types, math, builtins as _bi
    import csv as _csv, io as _io, json as _json

    _NL = chr(10)
    _COMMA = chr(44)
    _SQ = chr(39)

    def _is_seq(x):
        return isinstance(x, (list, tuple)) or (hasattr(x, '__iter__') and not isinstance(x, (str, bytes, dict)))

    def _to_list(x):
        if isinstance(x, list):
            return list(x)
        if isinstance(x, tuple):
            return list(x)
        if hasattr(x, 'tolist'):
            try:
                return list(x.tolist())
            except Exception:
                pass
        if _is_seq(x):
            return [v for v in x]
        return [x]

    _NA = float('nan')

    def _isna(v):
        if v is None:
            return True
        try:
            return isinstance(v, float) and math.isnan(v)
        except Exception:
            return False

    def _jsonable(v):
        if _isna(v):
            return None
        if hasattr(v, 'item'):
            try:
                return v.item()
            except Exception:
                return v
        return v

    def _fmt(v):
        if _isna(v):
            return 'NaN'
        if isinstance(v, float):
            if v == int(v) and abs(v) < 1e16:
                return str(v)
            return repr(round(v, 6)) if abs(v) < 1e12 else repr(v)
        return str(v)

    def _infer_dtype(vals):
        seen = set()
        for v in vals:
            if _isna(v):
                continue
            if isinstance(v, bool):
                seen.add('bool')
            elif isinstance(v, int):
                seen.add('int')
            elif isinstance(v, float):
                seen.add('float')
            else:
                seen.add('obj')
        if not seen:
            return 'float64'
        if seen == {'bool'}:
            return 'bool'
        if seen == {'int'}:
            return 'int64'
        if seen <= {'int', 'float'}:
            return 'float64'
        return 'object'

    def _norm_num(vals):
        vals = list(vals)
        has_na = False
        for v in vals:
            if v is None:
                has_na = True
                break
        if not has_na:
            return vals
        non_na = [v for v in vals if not _isna(v)]
        if non_na and all(isinstance(v, (int, float)) and not isinstance(v, bool) for v in non_na):
            return [_NA if v is None else (float(v) if isinstance(v, int) else v) for v in vals]
        return vals

    # ----- Series -----
    class Series:
        def __init__(self, data=None, index=None, name=None, dtype=None):
            if isinstance(data, Series):
                vals = list(data._v)
                if index is None:
                    index = list(data._i)
                if name is None:
                    name = data.name
            elif isinstance(data, dict):
                keys = list(data.keys())
                vals = [data[k] for k in keys]
                if index is None:
                    index = keys
            elif data is None:
                vals = []
            else:
                vals = _to_list(data)
            self._v = _norm_num(vals)
            if index is None:
                self._i = list(range(len(self._v)))
            else:
                self._i = _to_list(index)
            self.name = name

        @property
        def values(self):
            try:
                import numpy as _np
                return _np.array(self._v)
            except Exception:
                return list(self._v)

        @property
        def index(self):
            return list(self._i)

        @property
        def dtype(self):
            return _infer_dtype(self._v)

        @property
        def size(self):
            return len(self._v)

        @property
        def shape(self):
            return (len(self._v),)

        @property
        def empty(self):
            return len(self._v) == 0

        def __len__(self):
            return len(self._v)

        def tolist(self):
            return list(self._v)

        def to_list(self):
            return list(self._v)

        def _pos(self, label):
            for k, lab in enumerate(self._i):
                if lab == label:
                    return k
            raise KeyError(label)

        def __iter__(self):
            return iter(self._v)

        def __getitem__(self, key):
            if isinstance(key, Series):
                key = key._v
            if isinstance(key, slice):
                return Series(self._v[key], self._i[key], self.name)
            if isinstance(key, (list, tuple)):
                if len(key) and isinstance(key[0], bool):
                    v = [x for x, m in zip(self._v, key) if m]
                    i = [x for x, m in zip(self._i, key) if m]
                    return Series(v, i, self.name)
                out_v = []
                out_i = []
                for k in key:
                    p = self._pos(k)
                    out_v.append(self._v[p])
                    out_i.append(self._i[p])
                return Series(out_v, out_i, self.name)
            p = self._pos(key)
            return self._v[p]

        def __setitem__(self, key, value):
            p = self._pos(key)
            self._v[p] = value

        @property
        def iloc(self):
            return _ILoc(self)

        @property
        def loc(self):
            return _SLoc(self)

        @property
        def str(self):
            return _StrAccessor(self)

        def _binop(self, other, fn):
            if isinstance(other, Series):
                ov = other._v
                return Series([fn(a, b) for a, b in zip(self._v, ov)], self._i, self.name)
            return Series([fn(a, other) for a in self._v], self._i, self.name)

        def __add__(self, o): return self._binop(o, lambda a, b: a + b)
        def __radd__(self, o): return self._binop(o, lambda a, b: b + a)
        def __sub__(self, o): return self._binop(o, lambda a, b: a - b)
        def __rsub__(self, o): return self._binop(o, lambda a, b: b - a)
        def __mul__(self, o): return self._binop(o, lambda a, b: a * b)
        def __rmul__(self, o): return self._binop(o, lambda a, b: b * a)
        def __truediv__(self, o): return self._binop(o, lambda a, b: a / b)
        def __rtruediv__(self, o): return self._binop(o, lambda a, b: b / a)
        def __floordiv__(self, o): return self._binop(o, lambda a, b: a // b)
        def __mod__(self, o): return self._binop(o, lambda a, b: a % b)
        def __pow__(self, o): return self._binop(o, lambda a, b: a ** b)
        def __neg__(self): return Series([-a for a in self._v], self._i, self.name)
        def __gt__(self, o): return self._binop(o, lambda a, b: a > b)
        def __ge__(self, o): return self._binop(o, lambda a, b: a >= b)
        def __lt__(self, o): return self._binop(o, lambda a, b: a < b)
        def __le__(self, o): return self._binop(o, lambda a, b: a <= b)
        def __eq__(self, o): return self._binop(o, lambda a, b: a == b)
        def __ne__(self, o): return self._binop(o, lambda a, b: a != b)
        def __and__(self, o): return self._binop(o, lambda a, b: bool(a) and bool(b))
        def __or__(self, o): return self._binop(o, lambda a, b: bool(a) or bool(b))
        def __invert__(self): return Series([not bool(a) for a in self._v], self._i, self.name)

        def _num(self):
            return [x for x in self._v if not _isna(x)]

        def sum(self):
            return _bi.sum(self._num())

        def mean(self):
            n = self._num()
            return _bi.sum(n) / len(n) if n else _NA

        def min(self):
            n = self._num()
            return _bi.min(n) if n else _NA

        def max(self):
            n = self._num()
            return _bi.max(n) if n else _NA

        def count(self):
            return len(self._num())

        def median(self):
            n = sorted(self._num())
            if not n:
                return _NA
            k = len(n)
            return n[k // 2] if k % 2 else (n[k // 2 - 1] + n[k // 2]) / 2

        def std(self, ddof=1):
            n = self._num()
            if len(n) <= ddof:
                return _NA
            m = _bi.sum(n) / len(n)
            return math.sqrt(_bi.sum((x - m) ** 2 for x in n) / (len(n) - ddof))

        def var(self, ddof=1):
            s = self.std(ddof)
            return s * s if not _isna(s) else _NA

        def abs(self):
            return Series([abs(x) for x in self._v], self._i, self.name)

        def round(self, n=0):
            return Series([round(x, n) if not _isna(x) else x for x in self._v], self._i, self.name)

        def cumsum(self):
            out = []
            t = 0
            for x in self._v:
                t = t + (0 if _isna(x) else x)
                out.append(t)
            return Series(out, self._i, self.name)

        def nunique(self):
            return len(set(self._num()))

        def unique(self):
            seen = []
            for x in self._v:
                if x not in seen:
                    seen.append(x)
            try:
                import numpy as _np
                return _np.array(seen)
            except Exception:
                return seen

        def value_counts(self):
            counts = {}
            order = []
            for x in self._v:
                if _isna(x):
                    continue
                if x not in counts:
                    counts[x] = 0
                    order.append(x)
                counts[x] += 1
            order.sort(key=lambda k: -counts[k])
            return Series([counts[k] for k in order], order, self.name)

        def apply(self, fn):
            return Series([fn(x) for x in self._v], self._i, self.name)

        def map(self, arg):
            if isinstance(arg, dict):
                return Series([arg.get(x) for x in self._v], self._i, self.name)
            return Series([arg(x) for x in self._v], self._i, self.name)

        def isin(self, values):
            vs = list(values._v) if isinstance(values, Series) else list(values)
            return Series([x in vs for x in self._v], self._i, self.name)

        def where(self, cond, other=_NA):
            cv = cond._v if isinstance(cond, Series) else cond
            ov = other._v if isinstance(other, Series) else None
            out = []
            for k, x in enumerate(self._v):
                keep = cv[k] if isinstance(cv, (list, tuple)) else cv
                if keep:
                    out.append(x)
                elif ov is not None:
                    out.append(ov[k])
                else:
                    out.append(other)
            return Series(out, self._i, self.name)

        def mask(self, cond, other=_NA):
            neg = cond._v if isinstance(cond, Series) else cond
            if isinstance(neg, (list, tuple)):
                inv = Series([not b for b in neg], self._i, self.name)
            else:
                inv = not neg
            return self.where(inv, other)

        def replace(self, to_replace, value=None):
            if isinstance(to_replace, dict):
                m = to_replace
                return Series([m.get(x, x) for x in self._v], self._i, self.name)
            if isinstance(to_replace, (list, tuple)):
                rs = list(to_replace)
                return Series([value if x in rs else x for x in self._v], self._i, self.name)
            return Series([value if x == to_replace else x for x in self._v], self._i, self.name)

        def idxmax(self):
            best = None
            bl = None
            for lab, x in zip(self._i, self._v):
                if _isna(x):
                    continue
                if best is None or x > best:
                    best = x
                    bl = lab
            if bl is None:
                raise ValueError('attempt to get argmax of an empty sequence')
            return bl

        def idxmin(self):
            best = None
            bl = None
            for lab, x in zip(self._i, self._v):
                if _isna(x):
                    continue
                if best is None or x < best:
                    best = x
                    bl = lab
            if bl is None:
                raise ValueError('attempt to get argmin of an empty sequence')
            return bl

        def unique(self):
            seen = []
            for x in self._v:
                if x not in seen:
                    seen.append(x)
            return seen

        def drop_duplicates(self, keep='first'):
            seen = set()
            v = []
            i = []
            for x, lab in zip(self._v, self._i):
                if x not in seen:
                    seen.add(x)
                    v.append(x)
                    i.append(lab)
            return Series(v, i, self.name)

        def duplicated(self, keep='first'):
            seen = set()
            out = []
            for x in self._v:
                out.append(x in seen)
                seen.add(x)
            return Series(out, self._i, self.name)

        def to_json(self, orient='index'):
            return _json.dumps({str(k): _jsonable(v) for k, v in zip(self._i, self._v)})

        def astype(self, t):
            if t in (int, 'int', 'int64'):
                f = lambda x: int(x)
            elif t in (float, 'float', 'float64'):
                f = lambda x: float(x)
            elif t in (str, 'str', 'object'):
                f = lambda x: str(x)
            elif t in (bool, 'bool'):
                f = lambda x: bool(x)
            else:
                f = lambda x: x
            return Series([f(x) for x in self._v], self._i, self.name)

        def fillna(self, value):
            return Series([value if _isna(x) else x for x in self._v], self._i, self.name)

        def dropna(self):
            v = []
            i = []
            for x, lab in zip(self._v, self._i):
                if not _isna(x):
                    v.append(x)
                    i.append(lab)
            return Series(v, i, self.name)

        def isna(self):
            return Series([_isna(x) for x in self._v], self._i, self.name)

        def isnull(self):
            return self.isna()

        def notna(self):
            return Series([not _isna(x) for x in self._v], self._i, self.name)

        def sort_values(self, ascending=True):
            pairs = sorted(zip(self._v, self._i), key=lambda p: p[0], reverse=not ascending)
            return Series([p[0] for p in pairs], [p[1] for p in pairs], self.name)

        def head(self, n=5):
            return Series(self._v[:n], self._i[:n], self.name)

        def tail(self, n=5):
            return Series(self._v[-n:], self._i[-n:], self.name)

        def to_dict(self):
            return {k: v for k, v in zip(self._i, self._v)}

        def describe(self):
            n = self._num()
            data = {'count': len(n), 'mean': self.mean(), 'std': self.std(),
                    'min': self.min(), 'max': self.max()}
            return Series(list(data.values()), list(data.keys()), self.name)

        def __repr__(self):
            lines = []
            for lab, v in zip(self._i, self._v):
                lines.append(str(lab).rjust(6) + '    ' + _fmt(v))
            tail = 'Name: ' + str(self.name) + _COMMA + ' ' if self.name is not None else ''
            lines.append(tail + 'dtype: ' + self.dtype)
            return _NL.join(lines)

    class _StrAccessor:
        def __init__(self, s):
            self._s = s
        def _ap(self, fn):
            return Series([fn(x) if isinstance(x, str) else _NA for x in self._s._v], self._s._i, self._s.name)
        def lower(self): return self._ap(lambda x: x.lower())
        def upper(self): return self._ap(lambda x: x.upper())
        def strip(self): return self._ap(lambda x: x.strip())
        def len(self): return self._ap(lambda x: len(x))
        def contains(self, pat): return self._ap(lambda x: pat in x)
        def startswith(self, p): return self._ap(lambda x: x.startswith(p))
        def endswith(self, p): return self._ap(lambda x: x.endswith(p))
        def replace(self, a, b): return self._ap(lambda x: x.replace(a, b))
        def split(self, sep=None): return self._ap(lambda x: x.split(sep))

    class _ILoc:
        def __init__(self, s):
            self._s = s
        def __getitem__(self, k):
            s = self._s
            if isinstance(k, slice):
                return Series(s._v[k], s._i[k], s.name)
            if isinstance(k, (list, tuple)):
                return Series([s._v[j] for j in k], [s._i[j] for j in k], s.name)
            return s._v[k]

    class _SLoc:
        def __init__(self, s):
            self._s = s
        def __getitem__(self, k):
            return self._s[k]


    # ----- DataFrame -----
    class DataFrame:
        def __init__(self, data=None, columns=None, index=None):
            cols = {}
            idx = None
            if data is None:
                pass
            elif isinstance(data, DataFrame):
                cols = {c: list(data._d[c]) for c in data._c}
                idx = list(data._i)
                columns = columns or list(data._c)
            elif isinstance(data, dict):
                for k, v in data.items():
                    if isinstance(v, Series):
                        cols[k] = list(v._v)
                    else:
                        cols[k] = _to_list(v)
                columns = columns or list(data.keys())
            elif isinstance(data, list):
                if len(data) and isinstance(data[0], dict):
                    keys = []
                    for row in data:
                        for k in row.keys():
                            if k not in keys:
                                keys.append(k)
                    for k in keys:
                        cols[k] = [row.get(k) for row in data]
                    columns = columns or keys
                elif len(data) and _is_seq(data[0]):
                    ncol = len(data[0])
                    if columns is None:
                        columns = list(range(ncol))
                    for j, c in enumerate(columns):
                        cols[c] = [row[j] for row in data]
                else:
                    c = columns[0] if columns else 0
                    cols[c] = list(data)
                    columns = [c]
            self._d = {_c9: _norm_num(_v9) for _c9, _v9 in cols.items()}
            self._c = list(columns) if columns is not None else list(cols.keys())
            n = len(cols[self._c[0]]) if self._c else 0
            if index is not None:
                self._i = _to_list(index)
            elif idx is not None:
                self._i = idx
            else:
                self._i = list(range(n))

        @property
        def columns(self):
            return list(self._c)

        @columns.setter
        def columns(self, value):
            newc = _to_list(value)
            self._d = {nc: self._d[oc] for nc, oc in zip(newc, self._c)}
            self._c = newc

        @property
        def index(self):
            return list(self._i)

        @property
        def shape(self):
            n = len(self._d[self._c[0]]) if self._c else 0
            return (n, len(self._c))

        @property
        def empty(self):
            return self.shape[0] == 0 or len(self._c) == 0

        @property
        def dtypes(self):
            return Series([_infer_dtype(self._d[c]) for c in self._c], list(self._c))

        @property
        def values(self):
            rows = [[self._d[c][r] for c in self._c] for r in range(self.shape[0])]
            try:
                import numpy as _np
                return _np.array(rows)
            except Exception:
                return rows

        @property
        def T(self):
            n = self.shape[0]
            data = {}
            newcols = list(self._i)
            for r in range(n):
                data[self._i[r]] = [self._d[c][r] for c in self._c]
            return DataFrame(data, columns=newcols, index=list(self._c))

        def __len__(self):
            return self.shape[0]

        def _col(self, c):
            return Series(self._d[c], self._i, c)

        def __getitem__(self, key):
            if isinstance(key, Series):
                mask = key._v
                idxs = [r for r in range(self.shape[0]) if mask[r]]
                return self._take(idxs)
            if isinstance(key, list):
                return DataFrame({c: self._d[c] for c in key}, columns=key, index=self._i)
            if isinstance(key, slice):
                idxs = list(range(self.shape[0]))[key]
                return self._take(idxs)
            return self._col(key)

        def __setitem__(self, key, value):
            if isinstance(value, Series):
                vals = list(value._v)
            elif _is_seq(value):
                vals = _to_list(value)
            else:
                vals = [value] * self.shape[0]
            self._d[key] = vals
            if key not in self._c:
                self._c.append(key)

        def _take(self, idxs):
            data = {c: [self._d[c][r] for r in idxs] for c in self._c}
            return DataFrame(data, columns=list(self._c), index=[self._i[r] for r in idxs])

        def __getattr__(self, name):
            d = object.__getattribute__(self, '__dict__')
            if '_d' in d and name in d['_d']:
                return self._col(name)
            raise AttributeError(name)

        @property
        def iloc(self):
            return _DFILoc(self)

        @property
        def loc(self):
            return _DFLoc(self)

        def head(self, n=5):
            return self._take(list(range(min(n, self.shape[0]))))

        def tail(self, n=5):
            m = self.shape[0]
            return self._take(list(range(max(0, m - n), m)))

        def copy(self):
            return DataFrame({c: list(self._d[c]) for c in self._c}, columns=list(self._c), index=list(self._i))

        def rename(self, columns=None, **kw):
            columns = columns or kw.get('columns') or {}
            newc = [columns.get(c, c) for c in self._c]
            data = {columns.get(c, c): self._d[c] for c in self._c}
            return DataFrame(data, columns=newc, index=self._i)

        def drop(self, labels=None, axis=0, columns=None):
            if columns is not None:
                labels = columns
                axis = 1
            if not isinstance(labels, list):
                labels = [labels]
            if axis == 1:
                keep = [c for c in self._c if c not in labels]
                return DataFrame({c: self._d[c] for c in keep}, columns=keep, index=self._i)
            idxs = [r for r in range(self.shape[0]) if self._i[r] not in labels]
            return self._take(idxs)

        def sort_values(self, by, ascending=True):
            if isinstance(by, list):
                key = lambda r: tuple(self._d[c][r] for c in by)
            else:
                key = lambda r: self._d[by][r]
            idxs = sorted(range(self.shape[0]), key=key, reverse=not ascending)
            return self._take(idxs)

        def reset_index(self, drop=False):
            out = self.copy()
            if not drop:
                out._d = {'index': list(self._i)}
                out._d.update({c: list(self._d[c]) for c in self._c})
                out._c = ['index'] + list(self._c)
            out._i = list(range(self.shape[0]))
            return out

        def set_index(self, col):
            out = DataFrame({c: list(self._d[c]) for c in self._c if c != col},
                            columns=[c for c in self._c if c != col],
                            index=list(self._d[col]))
            return out

        def fillna(self, value):
            data = {c: [value if _isna(x) else x for x in self._d[c]] for c in self._c}
            return DataFrame(data, columns=list(self._c), index=self._i)

        def dropna(self):
            idxs = [r for r in range(self.shape[0]) if not any(_isna(self._d[c][r]) for c in self._c)]
            return self._take(idxs)

        def isna(self):
            data = {c: [_isna(x) for x in self._d[c]] for c in self._c}
            return DataFrame(data, columns=list(self._c), index=self._i)

        def apply(self, fn, axis=0):
            if axis == 0:
                return Series([fn(self._col(c)) for c in self._c], list(self._c))
            out = []
            for r in range(self.shape[0]):
                row = Series([self._d[c][r] for c in self._c], list(self._c))
                out.append(fn(row))
            return Series(out, self._i)

        def _binop(self, other, fn):
            if isinstance(other, DataFrame):
                data = {}
                for c in self._c:
                    if c in other._d:
                        data[c] = [fn(a, b) for a, b in zip(self._d[c], other._d[c])]
                    else:
                        data[c] = [_NA for _ in self._d[c]]
                return DataFrame(data, columns=list(self._c), index=self._i)
            if isinstance(other, Series):
                m = {lab: val for lab, val in zip(other._i, other._v)}
                data = {c: [fn(x, m.get(c, _NA)) for x in self._d[c]] for c in self._c}
                return DataFrame(data, columns=list(self._c), index=self._i)
            data = {c: [fn(x, other) for x in self._d[c]] for c in self._c}
            return DataFrame(data, columns=list(self._c), index=self._i)

        def __add__(self, o): return self._binop(o, lambda a, b: a + b)
        def __radd__(self, o): return self._binop(o, lambda a, b: b + a)
        def __sub__(self, o): return self._binop(o, lambda a, b: a - b)
        def __mul__(self, o): return self._binop(o, lambda a, b: a * b)
        def __truediv__(self, o): return self._binop(o, lambda a, b: a / b)

        @property
        def at(self):
            return _DFAt(self)

        @property
        def iat(self):
            return _DFIat(self)

        def astype(self, t):
            if isinstance(t, dict):
                data = {}
                for c in self._c:
                    if c in t:
                        data[c] = list(self._col(c).astype(t[c])._v)
                    else:
                        data[c] = list(self._d[c])
                return DataFrame(data, columns=list(self._c), index=self._i)
            data = {c: list(self._col(c).astype(t)._v) for c in self._c}
            return DataFrame(data, columns=list(self._c), index=self._i)

        def replace(self, to_replace, value=None):
            data = {c: list(self._col(c).replace(to_replace, value)._v) for c in self._c}
            return DataFrame(data, columns=list(self._c), index=self._i)

        def duplicated(self, subset=None, keep='first'):
            cols = subset if subset is not None else list(self._c)
            if not isinstance(cols, list):
                cols = [cols]
            seen = set()
            out = []
            for r in range(self.shape[0]):
                k = tuple(self._d[c][r] for c in cols)
                out.append(k in seen)
                seen.add(k)
            return Series(out, self._i)

        def idxmax(self):
            cols = self._numeric_cols()
            return Series([self._col(c).idxmax() for c in cols], cols)

        def idxmin(self):
            cols = self._numeric_cols()
            return Series([self._col(c).idxmin() for c in cols], cols)

        def melt(self, id_vars=None, value_vars=None, var_name=None, value_name='value'):
            id_vars = id_vars or []
            if not isinstance(id_vars, list):
                id_vars = [id_vars]
            value_vars = value_vars if value_vars is not None else [c for c in self._c if c not in id_vars]
            if not isinstance(value_vars, list):
                value_vars = [value_vars]
            var_name = var_name or 'variable'
            data = {c: [] for c in id_vars}
            data[var_name] = []
            data[value_name] = []
            for r in range(self.shape[0]):
                for vc in value_vars:
                    for ic in id_vars:
                        data[ic].append(self._d[ic][r])
                    data[var_name].append(vc)
                    data[value_name].append(self._d[vc][r])
            return DataFrame(data, columns=id_vars + [var_name, value_name])

        def _numeric_cols(self):
            return [c for c in self._c if _infer_dtype(self._d[c]) in ('int64', 'float64', 'bool')]

        def sum(self, axis=0):
            cols = self._numeric_cols()
            return Series([self._col(c).sum() for c in cols], cols)

        def mean(self, axis=0):
            cols = self._numeric_cols()
            return Series([self._col(c).mean() for c in cols], cols)

        def min(self):
            cols = self._numeric_cols()
            return Series([self._col(c).min() for c in cols], cols)

        def max(self):
            cols = self._numeric_cols()
            return Series([self._col(c).max() for c in cols], cols)

        def describe(self):
            cols = self._numeric_cols()
            stats = ['count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max']
            data = {}
            for c in cols:
                s = self._col(c)
                sn = sorted(s._num())
                def q(p):
                    if not sn:
                        return _NA
                    k = p * (len(sn) - 1)
                    lo = int(math.floor(k))
                    hi = int(math.ceil(k))
                    if lo == hi:
                        return sn[lo]
                    return sn[lo] + (sn[hi] - sn[lo]) * (k - lo)
                data[c] = [s.count(), s.mean(), s.std(), s.min(), q(0.25), q(0.5), q(0.75), s.max()]
            return DataFrame(data, columns=cols, index=stats)

        def groupby(self, by):
            return _GroupBy(self, by)

        def assign(self, **kwargs):
            out = self.copy()
            for k, v in kwargs.items():
                out[k] = v(out) if callable(v) else v
            return out

        def drop_duplicates(self, subset=None, keep='first'):
            cols = subset if subset is not None else list(self._c)
            if not isinstance(cols, list):
                cols = [cols]
            seen = set()
            keeprows = []
            rng = list(range(self.shape[0]))
            if keep == 'last':
                rng = list(reversed(rng))
            for r in rng:
                key = tuple(self._d[c][r] for c in cols)
                if key not in seen:
                    seen.add(key)
                    keeprows.append(r)
            if keep == 'last':
                keeprows = list(reversed(keeprows))
            return self._take(keeprows)

        def nlargest(self, n, columns):
            by = columns if isinstance(columns, list) else [columns]
            idxs = sorted(range(self.shape[0]), key=lambda r: tuple(self._d[c][r] for c in by), reverse=True)
            return self._take(idxs[:n])

        def nsmallest(self, n, columns):
            by = columns if isinstance(columns, list) else [columns]
            idxs = sorted(range(self.shape[0]), key=lambda r: tuple(self._d[c][r] for c in by))
            return self._take(idxs[:n])

        def merge(self, right, on=None, how='inner', left_on=None, right_on=None, suffixes=('_x', '_y')):
            lon = left_on or on
            ron = right_on or on
            if lon is None:
                common = [c for c in self._c if c in right._c]
                lon = ron = common[0]
            lon = lon if isinstance(lon, list) else [lon]
            ron = ron if isinstance(ron, list) else [ron]
            rindex = {}
            for r in range(right.shape[0]):
                k = tuple(right._d[c][r] for c in ron)
                rindex.setdefault(k, []).append(r)
            lkeys = [c for c in self._c]
            rkeys = [c for c in right._c if c not in ron]
            out_cols = list(lkeys)
            for c in rkeys:
                out_cols.append(c + suffixes[1] if c in lkeys else c)
            data = {c: [] for c in out_cols}
            matched_r = set()
            for lr in range(self.shape[0]):
                k = tuple(self._d[c][lr] for c in lon)
                rs = rindex.get(k, [])
                if not rs and how in ('left', 'outer'):
                    for c in lkeys:
                        data[c].append(self._d[c][lr])
                    for c in rkeys:
                        data[c + suffixes[1] if c in lkeys else c].append(_NA)
                for rr in rs:
                    matched_r.add(rr)
                    for c in lkeys:
                        data[c].append(self._d[c][lr])
                    for c in rkeys:
                        data[c + suffixes[1] if c in lkeys else c].append(right._d[c][rr])
            if how in ('right', 'outer'):
                for rr in range(right.shape[0]):
                    if rr in matched_r:
                        continue
                    for c in lkeys:
                        if c in ron:
                            data[c].append(right._d[ron[lon.index(c)]][rr] if c in lon else _NA)
                        else:
                            data[c].append(_NA)
                    for c in rkeys:
                        data[c + suffixes[1] if c in lkeys else c].append(right._d[c][rr])
            return DataFrame(data, columns=out_cols)

        def iterrows(self):
            for r in range(self.shape[0]):
                yield self._i[r], Series([self._d[c][r] for c in self._c], list(self._c))

        def itertuples(self, index=True):
            for r in range(self.shape[0]):
                vals = [self._d[c][r] for c in self._c]
                if index:
                    vals = [self._i[r]] + vals
                yield tuple(vals)

        def to_dict(self, orient='dict'):
            if orient == 'records':
                return [{c: self._d[c][r] for c in self._c} for r in range(self.shape[0])]
            if orient == 'list':
                return {c: list(self._d[c]) for c in self._c}
            return {c: {self._i[r]: self._d[c][r] for r in range(self.shape[0])} for c in self._c}

        def to_csv(self, path=None, index=True, sep=None):
            sep = sep if sep is not None else _COMMA
            buf = _io.StringIO()
            w = _csv.writer(buf, delimiter=sep, lineterminator=_NL)
            header = (['' ] if index else []) + [str(c) for c in self._c]
            w.writerow(header)
            for r in range(self.shape[0]):
                row = ([self._i[r]] if index else []) + [self._d[c][r] for c in self._c]
                w.writerow(row)
            s = buf.getvalue()
            if path is not None:
                raise NotImplementedError('to_csv to a path is disabled in the sandbox; call to_csv() to get a string')
            return s

        def to_json(self, orient='records'):
            if orient in ('list', 'columns'):
                return _json.dumps({str(c): [_jsonable(x) for x in self._d[c]] for c in self._c})
            if orient == 'index':
                return _json.dumps({str(self._i[r]): {str(c): _jsonable(self._d[c][r]) for c in self._c} for r in range(self.shape[0])})
            recs = [{str(c): _jsonable(self._d[c][r]) for c in self._c} for r in range(self.shape[0])]
            return _json.dumps(recs)

        def to_string(self):
            return self.__repr__()

        def __repr__(self):
            n = self.shape[0]
            cols = [str(c) for c in self._c]
            idxw = max([len(str(i)) for i in self._i] + [0]) if n else 0
            colcells = []
            widths = []
            for j, c in enumerate(self._c):
                cell = [_fmt(self._d[c][r]) for r in range(n)]
                w = max([len(c)] + [len(x) for x in cell] + [0])
                widths.append(w)
                colcells.append(cell)
            head = ' ' * idxw + '  ' + '  '.join(cols[j].rjust(widths[j]) for j in range(len(cols)))
            lines = [head]
            for r in range(n):
                cells = '  '.join(colcells[j][r].rjust(widths[j]) for j in range(len(cols)))
                lines.append(str(self._i[r]).rjust(idxw) + '  ' + cells)
            if n == 0:
                lines.append('Empty DataFrame')
            return _NL.join(lines)

    class _DFILoc:
        def __init__(self, df):
            self._df = df
        def __getitem__(self, key):
            df = self._df
            if isinstance(key, tuple):
                rk, ck = key
            else:
                rk, ck = key, None
            if isinstance(rk, slice):
                rows = list(range(df.shape[0]))[rk]
                sub = df._take(rows)
                return sub if ck is None else sub[df._c[ck] if isinstance(ck, int) else ck]
            if isinstance(rk, (list, tuple)):
                return df._take(list(rk))
            row = Series([df._d[c][rk] for c in df._c], list(df._c), name=df._i[rk])
            if ck is None:
                return row
            if isinstance(ck, int):
                return df._d[df._c[ck]][rk]
            return df._d[ck][rk]

    class _DFLoc:
        def __init__(self, df):
            self._df = df
        def _rowpos(self, lab):
            return self._df._i.index(lab)
        def __getitem__(self, key):
            df = self._df
            if isinstance(key, tuple):
                rk, ck = key
            else:
                rk, ck = key, None
            if isinstance(rk, Series):
                mask = rk._v
                rows = [r for r in range(df.shape[0]) if mask[r]]
                sub = df._take(rows)
                return sub if ck is None else sub[ck]
            if isinstance(rk, slice):
                sub = df
                return sub if ck is None else sub[ck]
            p = self._rowpos(rk)
            row = Series([df._d[c][p] for c in df._c], list(df._c), name=rk)
            if ck is None:
                return row
            return df._d[ck][p]

    class _DFAt:
        def __init__(self, df):
            self._df = df
        def __getitem__(self, key):
            rk, ck = key
            df = self._df
            return df._d[ck][df._i.index(rk)]
        def __setitem__(self, key, value):
            rk, ck = key
            df = self._df
            df._d[ck][df._i.index(rk)] = value

    class _DFIat:
        def __init__(self, df):
            self._df = df
        def __getitem__(self, key):
            r, c = key
            df = self._df
            return df._d[df._c[c]][r]
        def __setitem__(self, key, value):
            r, c = key
            df = self._df
            df._d[df._c[c]][r] = value

    class _GroupBy:
        def __init__(self, df, by):
            self._df = df
            self._by = by if isinstance(by, list) else [by]
            self._sel = None
            groups = {}
            order = []
            for r in range(df.shape[0]):
                k = tuple(df._d[c][r] for c in self._by)
                if k not in groups:
                    groups[k] = []
                    order.append(k)
                groups[k].append(r)
            self._groups = groups
            self._order = order
        def __getitem__(self, key):
            g = _GroupBy.__new__(_GroupBy)
            g._df = self._df
            g._by = self._by
            g._groups = self._groups
            g._order = self._order
            g._sel = key
            return g
        def _labels(self):
            return [k[0] if len(self._by) == 1 else k for k in self._order]
        def _valcols(self, numeric_only=True):
            df = self._df
            if self._sel is not None:
                return self._sel if isinstance(self._sel, list) else [self._sel]
            vcols = [c for c in df._c if c not in self._by]
            if numeric_only:
                vcols = [c for c in vcols if _infer_dtype(df._d[c]) in ('int64', 'float64', 'bool')]
            return vcols
        def _agg(self, fn, numeric_only=True):
            df = self._df
            vcols = self._valcols(numeric_only)
            single = self._sel is not None and not isinstance(self._sel, list)
            if single:
                c = vcols[0]
                out = []
                for k in self._order:
                    s = Series([df._d[c][r] for r in self._groups[k]], name=c)
                    out.append(fn(s))
                return Series(out, self._labels(), name=c)
            data = {c: [] for c in vcols}
            for k in self._order:
                rows = self._groups[k]
                for c in vcols:
                    s = Series([df._d[c][r] for r in rows], name=c)
                    data[c].append(fn(s))
            idx = [k[0] if len(self._by) == 1 else k for k in self._order]
            return DataFrame(data, columns=vcols, index=idx)
        def sum(self): return self._agg(lambda s: s.sum())
        def mean(self): return self._agg(lambda s: s.mean())
        def min(self): return self._agg(lambda s: s.min())
        def max(self): return self._agg(lambda s: s.max())
        def std(self): return self._agg(lambda s: s.std())
        def var(self): return self._agg(lambda s: s.var())
        def median(self): return self._agg(lambda s: s.median())
        def nunique(self): return self._agg(lambda s: s.nunique(), numeric_only=False)
        def first(self): return self._agg(lambda s: (s._v[0] if len(s._v) else _NA), numeric_only=False)
        def last(self): return self._agg(lambda s: (s._v[-1] if len(s._v) else _NA), numeric_only=False)
        def count(self):
            return self._agg(lambda s: len([x for x in s._v if not _isna(x)]), numeric_only=False)
        def size(self):
            return Series([len(self._groups[k]) for k in self._order], self._labels())
        def _apply_one(self, s, f):
            return getattr(s, f)() if isinstance(f, str) else f(s)
        def agg(self, fn=None, **kwargs):
            if isinstance(fn, dict):
                specs = list(fn.items())
                data = {c: [] for c, _f in specs}
                for k in self._order:
                    rows = self._groups[k]
                    for c, f in specs:
                        s = Series([self._df._d[c][r] for r in rows], name=c)
                        data[c].append(self._apply_one(s, f))
                cols = [c for c, _f in specs]
                idx = [k[0] if len(self._by) == 1 else k for k in self._order]
                return DataFrame(data, columns=cols, index=idx)
            if isinstance(fn, str):
                return getattr(self, fn)()
            if fn is not None:
                return self._agg(lambda s: fn(s))
            return self
        def apply(self, fn):
            return self._agg(lambda s: fn(s), numeric_only=False)
        def __iter__(self):
            df = self._df
            for k in self._order:
                key = k[0] if len(self._by) == 1 else k
                yield key, df._take(self._groups[k])

    # ----- module funcs -----
    def read_csv(path_or_buf, sep=None, header='infer'):
        sep = sep if sep is not None else _COMMA
        if hasattr(path_or_buf, 'read'):
            text = path_or_buf.read()
        elif isinstance(path_or_buf, str) and (_NL in path_or_buf or _COMMA in path_or_buf) and not path_or_buf.strip().endswith('.csv'):
            text = path_or_buf
        else:
            f = open(path_or_buf, 'r')
            text = f.read()
            f.close()
        rows = list(_csv.reader(_io.StringIO(text), delimiter=sep))
        rows = [r for r in rows if r]
        if not rows:
            return DataFrame()
        cols = rows[0]
        body = rows[1:]
        data = {}
        for j, c in enumerate(cols):
            raw = [r[j] if j < len(r) else None for r in body]
            data[c] = [_coerce(x) for x in raw]
        return DataFrame(data, columns=cols)

    def _coerce(x):
        if x is None or x == '':
            return _NA
        try:
            i = int(x)
            return i
        except Exception:
            pass
        try:
            return float(x)
        except Exception:
            return x

    def read_json(s, orient='records'):
        obj = _json.loads(s) if isinstance(s, str) else s
        return DataFrame(obj)

    def concat(objs, axis=0, ignore_index=False):
        objs = [o for o in objs if o is not None]
        if not objs:
            return DataFrame()
        if isinstance(objs[0], Series):
            v = []
            i = []
            for s in objs:
                v.extend(s._v)
                i.extend(s._i)
            return Series(v, list(range(len(v))) if ignore_index else i)
        if axis == 1:
            data = {}
            cols = []
            for df in objs:
                for c in df._c:
                    data[c] = list(df._d[c])
                    cols.append(c)
            return DataFrame(data, columns=cols, index=objs[0]._i)
        allcols = []
        for df in objs:
            for c in df._c:
                if c not in allcols:
                    allcols.append(c)
        data = {c: [] for c in allcols}
        idx = []
        for df in objs:
            for r in range(df.shape[0]):
                for c in allcols:
                    data[c].append(df._d[c][r] if c in df._d else _NA)
                idx.append(df._i[r])
        if ignore_index:
            idx = list(range(len(idx)))
        return DataFrame(data, columns=allcols, index=idx)

    def merge(left, right, **kw):
        return left.merge(right, **kw)

    def isna(x):
        if isinstance(x, (Series, DataFrame)):
            return x.isna()
        return _isna(x)

    def notna(x):
        if isinstance(x, Series):
            return x.notna()
        return not _isna(x)

    def unique(vals):
        return Series(_to_list(vals)).unique()

    def to_numeric(s, errors='raise'):
        if isinstance(s, Series):
            def cv(x):
                try:
                    return float(x)
                except Exception:
                    if errors == 'coerce':
                        return _NA
                    raise
            return s.apply(cv)
        return float(s)

    NA = _NA
    NaN = _NA

    mod = types.ModuleType('pandas')
    mod.DataFrame = DataFrame
    mod.Series = Series
    mod.read_csv = read_csv
    mod.read_json = read_json
    mod.concat = concat
    mod.merge = merge
    mod.isna = isna
    mod.isnull = isna
    mod.notna = notna
    mod.notnull = notna
    mod.unique = unique
    mod.to_numeric = to_numeric
    mod.NA = _NA
    mod.NaN = _NA
    mod.__version__ = '2.0.0-vis-shim'
    sys.modules['pandas'] = mod
    try:
        _bi.pandas = mod
    except Exception:
        pass

__vis_install_pandas__()
del __vis_install_pandas__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pandas"
     :ext/description
     "Sandbox shim: a `pandas`-compatible module (import pandas / pandas.DataFrame) implemented in PURE Python (Series + DataFrame with selection, loc/iloc, boolean masks, groupby, merge, concat, describe, read_csv/to_csv) on the stdlib. Interoperates with the numpy shim. No pip, no native wheel, no host bridge. A correctness-focused subset, not C-speed pandas."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pandas"
       :shim/description
       "pandas-compatible `pandas` module implemented in pure Python (Series, DataFrame, groupby, merge, read_csv). No host bridge."
       :shim/preamble pandas-compat-shim-src}]}))

(vis/register-extension! vis-extension)
