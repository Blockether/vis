# vis sandbox pandas-compat shim.
#
# The agent sandbox ships no pandas wheel. This shim publishes a pandas-compatible
# module implemented in PURE Python (stdlib only, interoperates with the numpy
# shim when present). Series = labelled 1-D column; DataFrame = ordered dict of
# columns. A deliberate correctness-focused SUBSET, not C-speed pandas.


def __vis_install_pandas__():
    import sys, types, math, builtins as _bi
    import datetime as _dt
    import csv as _csv, io as _io, json as _json

    _NL = chr(10)
    _COMMA = chr(44)
    _SQ = chr(39)

    def _is_seq(x):
        return isinstance(x, (list, tuple)) or (
            hasattr(x, "__iter__") and not isinstance(x, (str, bytes, dict))
        )

    def _to_list(x):
        if isinstance(x, list):
            return list(x)
        if isinstance(x, tuple):
            return list(x)
        if hasattr(x, "tolist"):
            try:
                return list(x.tolist())
            except Exception:
                pass
        if _is_seq(x):
            return [v for v in x]
        return [x]

    _NA = float("nan")

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
        if hasattr(v, "item"):
            try:
                return v.item()
            except Exception:
                return v
        return v

    def _fmt(v):
        if _isna(v):
            return "NaN"
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
                seen.add("bool")
            elif isinstance(v, int):
                seen.add("int")
            elif isinstance(v, float):
                seen.add("float")
            else:
                seen.add("obj")
        if not seen:
            return "float64"
        if seen == {"bool"}:
            return "bool"
        if seen == {"int"}:
            return "int64"
        if seen <= {"int", "float"}:
            return "float64"
        return "object"

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
        if non_na and all(
            isinstance(v, (int, float)) and not isinstance(v, bool) for v in non_na
        ):
            return [
                _NA if v is None else (float(v) if isinstance(v, int) else v)
                for v in vals
            ]
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
                other_by_index = dict(zip(other._i, other._v))
                index = list(self._i)
                index.extend(label for label in other._i if label not in self._i)
                return Series(
                    [
                        fn(self._v[self._pos(label)], other_by_index[label])
                        if label in self._i and label in other_by_index
                        else math.nan
                        for label in index
                    ],
                    index,
                    self.name,
                )
            return Series([fn(a, other) for a in self._v], self._i, self.name)

        def __add__(self, o):
            return self._binop(o, lambda a, b: a + b)

        def __radd__(self, o):
            return self._binop(o, lambda a, b: b + a)

        def __sub__(self, o):
            return self._binop(o, lambda a, b: a - b)

        def __rsub__(self, o):
            return self._binop(o, lambda a, b: b - a)

        def __mul__(self, o):
            return self._binop(o, lambda a, b: a * b)

        def __rmul__(self, o):
            return self._binop(o, lambda a, b: b * a)

        def __truediv__(self, o):
            return self._binop(o, lambda a, b: a / b)

        def __rtruediv__(self, o):
            return self._binop(o, lambda a, b: b / a)

        def __floordiv__(self, o):
            return self._binop(o, lambda a, b: a // b)

        def __mod__(self, o):
            return self._binop(o, lambda a, b: a % b)

        def __pow__(self, o):
            return self._binop(o, lambda a, b: a**b)

        def __neg__(self):
            return Series([-a for a in self._v], self._i, self.name)

        def __gt__(self, o):
            return self._binop(o, lambda a, b: a > b)

        def __ge__(self, o):
            return self._binop(o, lambda a, b: a >= b)

        def __lt__(self, o):
            return self._binop(o, lambda a, b: a < b)

        def __le__(self, o):
            return self._binop(o, lambda a, b: a <= b)

        def __eq__(self, o):
            return self._binop(o, lambda a, b: a == b)

        def __ne__(self, o):
            return self._binop(o, lambda a, b: a != b)

        def __and__(self, o):
            return self._binop(o, lambda a, b: bool(a) and bool(b))

        def __or__(self, o):
            return self._binop(o, lambda a, b: bool(a) or bool(b))

        def __invert__(self):
            return Series([not bool(a) for a in self._v], self._i, self.name)

        @property
        def dt(self):
            return _DtAccessor(self)

        def rolling(self, window, min_periods=None):
            return _Rolling(self, window, min_periods)

        def expanding(self, min_periods=1):
            return _Rolling(self, len(self._v) if self._v else 1, min_periods)

        def quantile(self, q=0.5, interpolation="linear"):
            sn = sorted(self._num())
            if _is_seq(q):
                qs = _to_list(q)
                return Series(
                    [_quant(sn, x) for x in qs], index=list(qs), name=self.name
                )
            return _quant(sn, q)

        def prod(self):
            out = 1.0
            for x in self._num():
                out = out * x
            return out

        def mode(self):
            counts = {}
            for v in self._v:
                if _isna(v):
                    continue
                counts[v] = counts.get(v, 0) + 1
            if not counts:
                return Series([], name=self.name)
            top = _bi.max(counts.values())
            vals = [v for v in counts if counts[v] == top]
            try:
                vals = sorted(vals)
            except Exception:
                pass
            return Series(vals, name=self.name)

        def to_frame(self, name=None):
            nm = (
                name
                if name is not None
                else (self.name if self.name is not None else 0)
            )
            return DataFrame({nm: list(self._v)}, columns=[nm], index=list(self._i))

        def reset_index(self, drop=False, name=None):
            if drop:
                return Series(list(self._v), name=self.name)
            nm = (
                name
                if name is not None
                else (self.name if self.name is not None else 0)
            )
            return DataFrame(
                {"index": list(self._i), nm: list(self._v)}, columns=["index", nm]
            )

        def sort_index(self, ascending=True):
            order = sorted(
                range(len(self._v)), key=lambda k: self._i[k], reverse=not ascending
            )
            return Series(
                [self._v[k] for k in order],
                index=[self._i[k] for k in order],
                name=self.name,
            )

        def shift(self, periods=1, fill_value=None):
            fill = _NA if fill_value is None else fill_value
            n = len(self._v)
            out = []
            for i in range(n):
                j = i - periods
                out.append(self._v[j] if 0 <= j < n else fill)
            return Series(out, index=list(self._i), name=self.name)

        def diff(self, periods=1):
            prev = self.shift(periods)
            out = [
                (_NA if (_isna(a) or _isna(b)) else a - b)
                for a, b in zip(self._v, prev._v)
            ]
            return Series(out, index=list(self._i), name=self.name)

        def pct_change(self, periods=1):
            prev = self.shift(periods)
            out = [
                (_NA if (_isna(a) or _isna(b) or b == 0) else (a - b) / b)
                for a, b in zip(self._v, prev._v)
            ]
            return Series(out, index=list(self._i), name=self.name)

        def clip(self, lower=None, upper=None):
            def _c(v):
                if _isna(v):
                    return v
                if lower is not None and v < lower:
                    return lower
                if upper is not None and v > upper:
                    return upper
                return v

            return Series([_c(v) for v in self._v], index=list(self._i), name=self.name)

        def between(self, left, right, inclusive="both"):
            def _b(v):
                if _isna(v):
                    return False
                lo = v >= left if inclusive in ("both", "left") else v > left
                hi = v <= right if inclusive in ("both", "right") else v < right
                return bool(lo and hi)

            return Series([_b(v) for v in self._v], index=list(self._i), name=self.name)

        def rank(self, method="average", ascending=True):
            pairs = [(i, v) for i, v in enumerate(self._v) if not _isna(v)]
            order = sorted(pairs, key=lambda p: p[1], reverse=not ascending)
            out = [_NA] * len(self._v)
            k = 0
            while k < len(order):
                j = k
                while j + 1 < len(order) and order[j + 1][1] == order[k][1]:
                    j = j + 1
                if method == "min":
                    r = float(k + 1)
                elif method == "max":
                    r = float(j + 1)
                elif method == "dense":
                    r = None
                else:
                    r = (k + 1 + j + 1) / 2.0
                for m in range(k, j + 1):
                    out[order[m][0]] = float(m + 1) if method == "first" else r
                k = j + 1
            if method == "dense":
                seen = []
                for _i9, v in order:
                    if not seen or seen[-1] != v:
                        seen.append(v)
                for i, v in pairs:
                    out[i] = float(seen.index(v) + 1)
            return Series(out, index=list(self._i), name=self.name)

        def cumprod(self):
            out = []
            acc = 1.0
            for v in self._v:
                if not _isna(v):
                    acc = acc * v
                out.append(acc)
            return Series(out, index=list(self._i), name=self.name)

        def cummax(self):
            out = []
            acc = None
            for v in self._v:
                if not _isna(v):
                    acc = v if acc is None or v > acc else acc
                out.append(_NA if acc is None else acc)
            return Series(out, index=list(self._i), name=self.name)

        def cummin(self):
            out = []
            acc = None
            for v in self._v:
                if not _isna(v):
                    acc = v if acc is None or v < acc else acc
                out.append(_NA if acc is None else acc)
            return Series(out, index=list(self._i), name=self.name)

        def corr(self, other, method="pearson"):
            b = other._v if isinstance(other, Series) else _to_list(other)
            pairs = [
                (float(x), float(y))
                for x, y in zip(self._v, b)
                if not _isna(x) and not _isna(y)
            ]
            if len(pairs) < 2:
                return _NA
            mx = _bi.sum(p[0] for p in pairs) / len(pairs)
            my = _bi.sum(p[1] for p in pairs) / len(pairs)
            sxy = _bi.sum((p[0] - mx) * (p[1] - my) for p in pairs)
            sxx = math.sqrt(_bi.sum((p[0] - mx) ** 2 for p in pairs))
            syy = math.sqrt(_bi.sum((p[1] - my) ** 2 for p in pairs))
            if sxx == 0.0 or syy == 0.0:
                return _NA
            return sxy / (sxx * syy)

        def cov(self, other, ddof=1):
            b = other._v if isinstance(other, Series) else _to_list(other)
            pairs = [
                (float(x), float(y))
                for x, y in zip(self._v, b)
                if not _isna(x) and not _isna(y)
            ]
            if len(pairs) - ddof <= 0:
                return _NA
            mx = _bi.sum(p[0] for p in pairs) / len(pairs)
            my = _bi.sum(p[1] for p in pairs) / len(pairs)
            return _bi.sum((p[0] - mx) * (p[1] - my) for p in pairs) / (
                len(pairs) - ddof
            )

        def nlargest(self, n=5):
            return self.sort_values(ascending=False).head(n)

        def nsmallest(self, n=5):
            return self.sort_values(ascending=True).head(n)

        def any(self):
            return _bi.any(bool(v) for v in self._v if not _isna(v))

        def all(self):
            return _bi.all(bool(v) for v in self._v if not _isna(v))

        def item(self):
            if len(self._v) != 1:
                raise ValueError("can only convert an array of size 1 to a scalar")
            return self._v[0]

        def agg(self, func):
            return self.aggregate(func)

        def aggregate(self, func):
            if isinstance(func, list):
                return Series(
                    [self.aggregate(f) for f in func],
                    index=[
                        f if isinstance(f, str) else getattr(f, "__name__", "fn")
                        for f in func
                    ],
                    name=self.name,
                )
            if isinstance(func, str):
                return getattr(self, func)()
            return func(self)

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
            return Series(
                [round(x, n) if not _isna(x) else x for x in self._v],
                self._i,
                self.name,
            )

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
                return Series(
                    [value if x in rs else x for x in self._v], self._i, self.name
                )
            return Series(
                [value if x == to_replace else x for x in self._v], self._i, self.name
            )

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
                raise ValueError("attempt to get argmax of an empty sequence")
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
                raise ValueError("attempt to get argmin of an empty sequence")
            return bl

        def unique(self):
            seen = []
            for x in self._v:
                if x not in seen:
                    seen.append(x)
            return seen

        def drop_duplicates(self, keep="first"):
            seen = set()
            v = []
            i = []
            for x, lab in zip(self._v, self._i):
                if x not in seen:
                    seen.add(x)
                    v.append(x)
                    i.append(lab)
            return Series(v, i, self.name)

        def duplicated(self, keep="first"):
            seen = set()
            out = []
            for x in self._v:
                out.append(x in seen)
                seen.add(x)
            return Series(out, self._i, self.name)

        def to_json(self, orient="index"):
            return _json.dumps({str(k): _jsonable(v) for k, v in zip(self._i, self._v)})

        def astype(self, t):
            if t in (int, "int", "int64"):
                f = lambda x: int(x)
            elif t in (float, "float", "float64"):
                f = lambda x: float(x)
            elif t in (str, "str", "object"):
                f = lambda x: str(x)
            elif t in (bool, "bool"):
                f = lambda x: bool(x)
            else:
                f = lambda x: x
            return Series([f(x) for x in self._v], self._i, self.name)

        def fillna(self, value):
            return Series(
                [value if _isna(x) else x for x in self._v], self._i, self.name
            )

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
            pairs = sorted(
                zip(self._v, self._i), key=lambda p: p[0], reverse=not ascending
            )
            return Series([p[0] for p in pairs], [p[1] for p in pairs], self.name)

        def head(self, n=5):
            return Series(self._v[:n], self._i[:n], self.name)

        def tail(self, n=5):
            return Series(self._v[-n:], self._i[-n:], self.name)

        def to_dict(self):
            return {k: v for k, v in zip(self._i, self._v)}

        def describe(self):
            n = self._num()
            data = {
                "count": len(n),
                "mean": self.mean(),
                "std": self.std(),
                "min": self.min(),
                "max": self.max(),
            }
            return Series(list(data.values()), list(data.keys()), self.name)

        def __repr__(self):
            labs = [str(lab) for lab in self._i]
            vals = [_fmt(v) for v in self._v]
            lw = max([len(x) for x in labs] + [0])
            vw = max([len(x) for x in vals] + [0])
            lines = []
            for lab, v in zip(labs, vals):
                lines.append(lab.ljust(lw) + "    " + v.rjust(vw))
            tail = (
                "Name: " + str(self.name) + _COMMA + " "
                if self.name is not None
                else ""
            )
            lines.append(tail + "dtype: " + self.dtype)
            return _NL.join(lines)

    class _StrAccessor:
        def __init__(self, s):
            self._s = s

        def _ap(self, fn):
            return Series(
                [fn(x) if isinstance(x, str) else _NA for x in self._s._v],
                self._s._i,
                self._s.name,
            )

        def lower(self):
            return self._ap(lambda x: x.lower())

        def upper(self):
            return self._ap(lambda x: x.upper())

        def strip(self):
            return self._ap(lambda x: x.strip())

        def len(self):
            return self._ap(lambda x: len(x))

        def contains(self, pat):
            return self._ap(lambda x: pat in x)

        def startswith(self, p):
            return self._ap(lambda x: x.startswith(p))

        def endswith(self, p):
            return self._ap(lambda x: x.endswith(p))

        def replace(self, a, b):
            return self._ap(lambda x: x.replace(a, b))

        def split(self, sep=None):
            return self._ap(lambda x: x.split(sep))

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
    def _med(w):
        n = sorted(w)
        k = len(n)
        if not k:
            return _NA
        return (
            float(n[k // 2])
            if k % 2
            else (float(n[k // 2 - 1]) + float(n[k // 2])) / 2.0
        )

    def _var(w, ddof=1):
        n = [float(x) for x in w]
        k = len(n)
        if k - ddof <= 0:
            return _NA
        m = _bi.sum(n) / k
        return _bi.sum((x - m) ** 2 for x in n) / (k - ddof)

    def _stdev(w, ddof=1):
        v = _var(w, ddof)
        return _NA if _isna(v) else math.sqrt(v)

    def _quant(sorted_vals, q):
        if not sorted_vals:
            return _NA
        pos = (len(sorted_vals) - 1) * float(q)
        lo = int(math.floor(pos))
        hi = int(math.ceil(pos))
        if lo == hi:
            return float(sorted_vals[lo])
        return float(sorted_vals[lo]) + (
            float(sorted_vals[hi]) - float(sorted_vals[lo])
        ) * (pos - lo)

    _AGGF = {
        "mean": lambda s: s.mean(),
        "sum": lambda s: s.sum(),
        "min": lambda s: s.min(),
        "max": lambda s: s.max(),
        "count": lambda s: s.count(),
        "size": lambda s: len(s),
        "median": lambda s: s.median(),
        "std": lambda s: s.std(),
        "var": lambda s: s.var(),
        "prod": lambda s: s.prod(),
        "nunique": lambda s: s.nunique(),
        "first": lambda s: s._v[0] if len(s._v) else _NA,
        "last": lambda s: s._v[-1] if len(s._v) else _NA,
    }

    class _Rolling:
        def __init__(self, s, window, min_periods=None):
            self._s = s
            self._w = int(window)
            self._mp = self._w if min_periods is None else int(min_periods)

        def _roll(self, fn):
            v = self._s._v
            out = []
            for i in range(len(v)):
                lo = i - self._w + 1
                if lo < 0:
                    lo = 0
                win = [x for x in v[lo : i + 1] if not _isna(x)]
                out.append(_NA if len(win) < self._mp else fn(win))
            return Series(out, index=list(self._s._i), name=self._s.name)

        def sum(self):
            return self._roll(lambda w: _bi.sum(w))

        def mean(self):
            return self._roll(lambda w: _bi.sum(w) / len(w))

        def min(self):
            return self._roll(lambda w: _bi.min(w))

        def max(self):
            return self._roll(lambda w: _bi.max(w))

        def count(self):
            return self._roll(lambda w: float(len(w)))

        def median(self):
            return self._roll(_med)

        def std(self, ddof=1):
            return self._roll(lambda w: _stdev(w, ddof))

        def var(self, ddof=1):
            return self._roll(lambda w: _var(w, ddof))

        def apply(self, fn, raw=True):
            return self._roll(lambda w: fn(list(w)))

    class _DtAccessor:
        def __init__(self, s):
            self._s = s

        def _map(self, fn, name=None):
            return Series(
                [_NA if _isna(v) else fn(v) for v in self._s._v],
                index=list(self._s._i),
                name=name or self._s.name,
            )

        @property
        def year(self):
            return self._map(lambda d: d.year)

        @property
        def month(self):
            return self._map(lambda d: d.month)

        @property
        def day(self):
            return self._map(lambda d: d.day)

        @property
        def hour(self):
            return self._map(lambda d: d.hour)

        @property
        def minute(self):
            return self._map(lambda d: d.minute)

        @property
        def second(self):
            return self._map(lambda d: d.second)

        @property
        def dayofweek(self):
            return self._map(lambda d: d.weekday())

        @property
        def weekday(self):
            return self._map(lambda d: d.weekday())

        @property
        def date(self):
            return self._map(lambda d: d.date())

        def strftime(self, fmt):
            return self._map(lambda d: d.strftime(fmt))

        def normalize(self):
            return self._map(lambda d: _dt.datetime(d.year, d.month, d.day))

    class DataFrame:
        def __init__(self, data=None, columns=None, index=None):
            cols = {}
            idx = None
            if isinstance(data, Series):
                if index is None:
                    index = list(data._i)
                data = {data.name if data.name is not None else 0: list(data._v)}
            elif (
                data is not None
                and not isinstance(data, (DataFrame, dict, list, tuple))
                and hasattr(data, "tolist")
                and hasattr(data, "shape")
            ):
                # numpy-style ndarray (incl. the vis numpy shim): materialise rows
                data = data.tolist()
                if len(data) and not _is_seq(data[0]):
                    data = [[v] for v in data]
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
                return DataFrame(
                    {c: self._d[c] for c in key}, columns=key, index=self._i
                )
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
            return DataFrame(
                data, columns=list(self._c), index=[self._i[r] for r in idxs]
            )

        def __getattr__(self, name):
            d = object.__getattribute__(self, "__dict__")
            if "_d" in d and name in d["_d"]:
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
            return DataFrame(
                {c: list(self._d[c]) for c in self._c},
                columns=list(self._c),
                index=list(self._i),
            )

        def rename(self, columns=None, **kw):
            columns = columns or kw.get("columns") or {}
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
                return DataFrame(
                    {c: self._d[c] for c in keep}, columns=keep, index=self._i
                )
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
                out._d = {"index": list(self._i)}
                out._d.update({c: list(self._d[c]) for c in self._c})
                out._c = ["index"] + list(self._c)
            out._i = list(range(self.shape[0]))
            return out

        def set_index(self, col):
            out = DataFrame(
                {c: list(self._d[c]) for c in self._c if c != col},
                columns=[c for c in self._c if c != col],
                index=list(self._d[col]),
            )
            return out

        def fillna(self, value):
            data = {c: [value if _isna(x) else x for x in self._d[c]] for c in self._c}
            return DataFrame(data, columns=list(self._c), index=self._i)

        def dropna(self):
            idxs = [
                r
                for r in range(self.shape[0])
                if not any(_isna(self._d[c][r]) for c in self._c)
            ]
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

        def __add__(self, o):
            return self._binop(o, lambda a, b: a + b)

        def __radd__(self, o):
            return self._binop(o, lambda a, b: b + a)

        def __sub__(self, o):
            return self._binop(o, lambda a, b: a - b)

        def __mul__(self, o):
            return self._binop(o, lambda a, b: a * b)

        def __truediv__(self, o):
            return self._binop(o, lambda a, b: a / b)

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
            data = {
                c: list(self._col(c).replace(to_replace, value)._v) for c in self._c
            }
            return DataFrame(data, columns=list(self._c), index=self._i)

        def duplicated(self, subset=None, keep="first"):
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

        def melt(
            self, id_vars=None, value_vars=None, var_name=None, value_name="value"
        ):
            id_vars = id_vars or []
            if not isinstance(id_vars, list):
                id_vars = [id_vars]
            value_vars = (
                value_vars
                if value_vars is not None
                else [c for c in self._c if c not in id_vars]
            )
            if not isinstance(value_vars, list):
                value_vars = [value_vars]
            var_name = var_name or "variable"
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
            return [
                c
                for c in self._c
                if _infer_dtype(self._d[c]) in ("int64", "float64", "bool")
            ]

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
            stats = ["count", "mean", "std", "min", "25%", "50%", "75%", "max"]
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

                data[c] = [
                    s.count(),
                    s.mean(),
                    s.std(),
                    s.min(),
                    q(0.25),
                    q(0.5),
                    q(0.75),
                    s.max(),
                ]
            return DataFrame(data, columns=cols, index=stats)

        def count(self):
            return Series([self._col(c).count() for c in self._c], list(self._c))

        def median(self):
            cols = self._numeric_cols()
            return Series([self._col(c).median() for c in cols], cols)

        def std(self, ddof=1):
            cols = self._numeric_cols()
            return Series([self._col(c).std() for c in cols], cols)

        def var(self, ddof=1):
            cols = self._numeric_cols()
            return Series([self._col(c).var() for c in cols], cols)

        def prod(self):
            cols = self._numeric_cols()
            return Series([self._col(c).prod() for c in cols], cols)

        def nunique(self):
            return Series([self._col(c).nunique() for c in self._c], list(self._c))

        def quantile(self, q=0.5):
            cols = self._numeric_cols()
            if _is_seq(q):
                qs = _to_list(q)
                data = {c: [self._col(c).quantile(x) for x in qs] for c in cols}
                return DataFrame(data, columns=cols, index=list(qs))
            return Series([self._col(c).quantile(q) for c in cols], cols)

        def round(self, decimals=0):
            out = self.copy()
            for c in out._c:
                out._d[c] = [
                    (
                        v
                        if _isna(v)
                        or not isinstance(v, (int, float))
                        or isinstance(v, bool)
                        else _bi.round(v, decimals)
                    )
                    for v in out._d[c]
                ]
            return out

        def corr(self, method="pearson", numeric_only=True):
            cols = self._numeric_cols()
            data = {
                c: [(1.0 if c == o else self._col(c).corr(self._col(o))) for o in cols]
                for c in cols
            }
            return DataFrame(data, columns=cols, index=list(cols))

        def cov(self, ddof=1, numeric_only=True):
            cols = self._numeric_cols()
            data = {
                c: [self._col(c).cov(self._col(o), ddof) for o in cols] for c in cols
            }
            return DataFrame(data, columns=cols, index=list(cols))

        def sort_index(self, ascending=True):
            order = sorted(
                range(self.shape[0]), key=lambda r: self._i[r], reverse=not ascending
            )
            return self._take(order)

        def query(self, expr, **kwargs):
            expr = expr.replace(chr(64), "")
            env0 = dict(kwargs)
            keep = []
            for r in range(self.shape[0]):
                env = dict(env0)
                for c in self._c:
                    if isinstance(c, str):
                        env[c] = self._d[c][r]
                env["index"] = self._i[r]
                if eval(expr, {"__builtins__": _bi}, env):
                    keep.append(r)
            return self._take(keep)

        def eval(self, expr, **kwargs):
            out = []
            for r in range(self.shape[0]):
                env = dict(kwargs)
                for c in self._c:
                    if isinstance(c, str):
                        env[c] = self._d[c][r]
                out.append(eval(expr.replace(chr(64), ""), {"__builtins__": _bi}, env))
            return Series(out, index=list(self._i))

        def pivot(self, index=None, columns=None, values=None):
            return self.pivot_table(
                values=values, index=index, columns=columns, aggfunc="first"
            )

        def pivot_table(
            self,
            values=None,
            index=None,
            columns=None,
            aggfunc="mean",
            fill_value=None,
            dropna=True,
        ):
            idxc = (
                index if isinstance(index, list) else ([] if index is None else [index])
            )
            colc = (
                columns
                if isinstance(columns, list)
                else ([] if columns is None else [columns])
            )
            if values is None:
                vals = [
                    c
                    for c in self._c
                    if c not in idxc
                    and c not in colc
                    and _infer_dtype(self._d[c]) in ("int64", "float64", "bool")
                ]
            else:
                vals = values if isinstance(values, list) else [values]
            fn = _AGGF[aggfunc] if isinstance(aggfunc, str) else aggfunc
            rowkeys = []
            colkeys = []
            cells = {}
            for r in range(self.shape[0]):
                rk = tuple(self._d[c][r] for c in idxc) if idxc else ("",)
                ck = tuple(self._d[c][r] for c in colc) if colc else None
                if rk not in cells:
                    cells[rk] = {}
                    rowkeys.append(rk)
                for v in vals:
                    key = (ck, v)
                    if key not in cells[rk]:
                        cells[rk][key] = []
                    cells[rk][key].append(self._d[v][r])
                    if key not in colkeys:
                        colkeys.append(key)
            try:
                rowkeys = sorted(rowkeys)
                colkeys = sorted(colkeys, key=lambda k: (str(k[0]), str(k[1])))
            except Exception:
                pass

            def _label(key):
                ck, v = key
                if ck is None:
                    return v
                lab = ck[0] if len(ck) == 1 else ck
                return lab if len(vals) == 1 else (v, lab)

            outcols = []
            data = {}
            for key in colkeys:
                lab = _label(key)
                outcols.append(lab)
                col = []
                for rk in rowkeys:
                    got = cells[rk].get(key)
                    if not got or not [x for x in got if not _isna(x)]:
                        col.append(_NA if fill_value is None else fill_value)
                    else:
                        col.append(fn(Series(got)))
                data[lab] = col
            idx = [rk[0] if len(rk) == 1 else rk for rk in rowkeys]
            return DataFrame(data, columns=outcols, index=idx)

        def to_markdown(self, index=True, tablefmt="pipe"):
            bar = chr(124)

            def _cell(v):
                if _isna(v):
                    return ""
                if isinstance(v, float):
                    return _fmt(v)
                return str(v)

            head = ([""] if index else []) + [str(c) for c in self._c]
            rows = []
            for r in range(self.shape[0]):
                cells = [_cell(self._d[c][r]) for c in self._c]
                rows.append(([str(self._i[r])] if index else []) + cells)
            widths = [len(h) for h in head]
            for row in rows:
                for j, cell in enumerate(row):
                    if len(cell) > widths[j]:
                        widths[j] = len(cell)

            def _line(cells):
                return (
                    bar
                    + bar.join(
                        " " + cells[j].ljust(widths[j]) + " " for j in range(len(cells))
                    )
                    + bar
                )

            sep = (
                bar
                + bar.join(":" + ("-" * (widths[j] + 1)) for j in range(len(head)))
                + bar
            )
            return _NL.join([_line(head), sep] + [_line(row) for row in rows])

        def groupby(self, by):
            return _GroupBy(self, by)

        def assign(self, **kwargs):
            out = self.copy()
            for k, v in kwargs.items():
                out[k] = v(out) if callable(v) else v
            return out

        def drop_duplicates(self, subset=None, keep="first"):
            cols = subset if subset is not None else list(self._c)
            if not isinstance(cols, list):
                cols = [cols]
            seen = set()
            keeprows = []
            rng = list(range(self.shape[0]))
            if keep == "last":
                rng = list(reversed(rng))
            for r in rng:
                key = tuple(self._d[c][r] for c in cols)
                if key not in seen:
                    seen.add(key)
                    keeprows.append(r)
            if keep == "last":
                keeprows = list(reversed(keeprows))
            return self._take(keeprows)

        def nlargest(self, n, columns):
            by = columns if isinstance(columns, list) else [columns]
            idxs = sorted(
                range(self.shape[0]),
                key=lambda r: tuple(self._d[c][r] for c in by),
                reverse=True,
            )
            return self._take(idxs[:n])

        def nsmallest(self, n, columns):
            by = columns if isinstance(columns, list) else [columns]
            idxs = sorted(
                range(self.shape[0]), key=lambda r: tuple(self._d[c][r] for c in by)
            )
            return self._take(idxs[:n])

        def merge(
            self,
            right,
            on=None,
            how="inner",
            left_on=None,
            right_on=None,
            suffixes=("_x", "_y"),
        ):
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
                if not rs and how in ("left", "outer"):
                    for c in lkeys:
                        data[c].append(self._d[c][lr])
                    for c in rkeys:
                        data[c + suffixes[1] if c in lkeys else c].append(_NA)
                for rr in rs:
                    matched_r.add(rr)
                    for c in lkeys:
                        data[c].append(self._d[c][lr])
                    for c in rkeys:
                        data[c + suffixes[1] if c in lkeys else c].append(
                            right._d[c][rr]
                        )
            if how in ("right", "outer"):
                for rr in range(right.shape[0]):
                    if rr in matched_r:
                        continue
                    for c in lkeys:
                        if c in ron:
                            data[c].append(
                                right._d[ron[lon.index(c)]][rr] if c in lon else _NA
                            )
                        else:
                            data[c].append(_NA)
                    for c in rkeys:
                        data[c + suffixes[1] if c in lkeys else c].append(
                            right._d[c][rr]
                        )
            return DataFrame(data, columns=out_cols)

        def iterrows(self):
            for r in range(self.shape[0]):
                yield (
                    self._i[r],
                    Series([self._d[c][r] for c in self._c], list(self._c)),
                )

        def itertuples(self, index=True):
            for r in range(self.shape[0]):
                vals = [self._d[c][r] for c in self._c]
                if index:
                    vals = [self._i[r]] + vals
                yield tuple(vals)

        def to_dict(self, orient="dict"):
            if orient == "records":
                return [
                    {c: self._d[c][r] for c in self._c} for r in range(self.shape[0])
                ]
            if orient == "list":
                return {c: list(self._d[c]) for c in self._c}
            return {
                c: {self._i[r]: self._d[c][r] for r in range(self.shape[0])}
                for c in self._c
            }

        def to_csv(self, path=None, index=True, sep=None):
            sep = sep if sep is not None else _COMMA
            buf = _io.StringIO()
            w = _csv.writer(buf, delimiter=sep, lineterminator=_NL)
            header = ([""] if index else []) + [str(c) for c in self._c]
            w.writerow(header)
            for r in range(self.shape[0]):
                row = ([self._i[r]] if index else []) + [self._d[c][r] for c in self._c]
                w.writerow(row)
            s = buf.getvalue()
            if path is not None:
                raise NotImplementedError(
                    "to_csv to a path is disabled in the sandbox; call to_csv() to get a string"
                )
            return s

        def to_json(self, orient="records"):
            if orient in ("list", "columns"):
                return _json.dumps(
                    {str(c): [_jsonable(x) for x in self._d[c]] for c in self._c}
                )
            if orient == "index":
                return _json.dumps(
                    {
                        str(self._i[r]): {
                            str(c): _jsonable(self._d[c][r]) for c in self._c
                        }
                        for r in range(self.shape[0])
                    }
                )
            recs = [
                {str(c): _jsonable(self._d[c][r]) for c in self._c}
                for r in range(self.shape[0])
            ]
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
            head = (
                " " * idxw
                + "  "
                + "  ".join(cols[j].rjust(widths[j]) for j in range(len(cols)))
            )
            lines = [head]
            for r in range(n):
                cells = "  ".join(
                    colcells[j][r].rjust(widths[j]) for j in range(len(cols))
                )
                lines.append(str(self._i[r]).rjust(idxw) + "  " + cells)
            if n == 0:
                lines.append("Empty DataFrame")
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
                return (
                    sub if ck is None else sub[df._c[ck] if isinstance(ck, int) else ck]
                )
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
                vcols = [
                    c
                    for c in vcols
                    if _infer_dtype(df._d[c]) in ("int64", "float64", "bool")
                ]
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

        def sum(self):
            return self._agg(lambda s: s.sum())

        def mean(self):
            return self._agg(lambda s: s.mean())

        def min(self):
            return self._agg(lambda s: s.min())

        def max(self):
            return self._agg(lambda s: s.max())

        def std(self):
            return self._agg(lambda s: s.std())

        def var(self):
            return self._agg(lambda s: s.var())

        def median(self):
            return self._agg(lambda s: s.median())

        def nunique(self):
            return self._agg(lambda s: s.nunique(), numeric_only=False)

        def first(self):
            return self._agg(
                lambda s: s._v[0] if len(s._v) else _NA, numeric_only=False
            )

        def last(self):
            return self._agg(
                lambda s: s._v[-1] if len(s._v) else _NA, numeric_only=False
            )

        def count(self):
            return self._agg(
                lambda s: len([x for x in s._v if not _isna(x)]), numeric_only=False
            )

        def size(self):
            return Series([len(self._groups[k]) for k in self._order], self._labels())

        def transform(self, func):
            df = self._df
            n = df.shape[0]
            vcols = self._valcols(True)
            single = self._sel is not None and not isinstance(self._sel, list)
            data = {}
            for c in vcols:
                out = [_NA] * n
                for k in self._order:
                    rows = self._groups[k]
                    s = Series([df._d[c][r] for r in rows], name=c)
                    res = func(s) if callable(func) else getattr(s, func)()
                    if isinstance(res, Series):
                        for pos, r in enumerate(rows):
                            out[r] = res._v[pos]
                    elif _is_seq(res):
                        vals = _to_list(res)
                        for pos, r in enumerate(rows):
                            out[r] = vals[pos]
                    else:
                        for r in rows:
                            out[r] = res
                data[c] = out
            if single:
                return Series(data[vcols[0]], index=list(df._i), name=vcols[0])
            return DataFrame(data, columns=vcols, index=list(df._i))

        def filter(self, func):
            df = self._df
            keep = []
            for k in self._order:
                rows = self._groups[k]
                if func(df._take(rows)):
                    keep.extend(rows)
            keep.sort()
            return df._take(keep)

        def quantile(self, q=0.5):
            return self._agg(lambda s: s.quantile(q))

        def prod(self):
            return self._agg(lambda s: s.prod())

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
    def read_csv(path_or_buf, sep=None, header="infer"):
        sep = sep if sep is not None else _COMMA
        if hasattr(path_or_buf, "read"):
            text = path_or_buf.read()
        elif (
            isinstance(path_or_buf, str)
            and (_NL in path_or_buf or _COMMA in path_or_buf)
            and not path_or_buf.strip().endswith(".csv")
        ):
            text = path_or_buf
        else:
            f = open(path_or_buf, "r")
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
        if x is None or x == "":
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

    def read_json(s, orient="records"):
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

    def to_numeric(s, errors="raise"):
        if isinstance(s, Series):

            def cv(x):
                try:
                    return float(x)
                except Exception:
                    if errors == "coerce":
                        return _NA
                    raise

            return s.apply(cv)
        return float(s)

    Timestamp = _dt.datetime
    Timedelta = _dt.timedelta

    _DT_FORMATS = (
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d",
        "%Y/%m/%d",
        "%d/%m/%Y",
        "%m/%d/%Y",
        "%Y%m%d",
    )

    def _parse_dt(v, fmt=None):
        if v is None or _isna(v):
            return _NA
        if isinstance(v, _dt.datetime):
            return v
        if isinstance(v, _dt.date):
            return _dt.datetime(v.year, v.month, v.day)
        if isinstance(v, (int, float)) and not isinstance(v, bool):
            return _dt.datetime.utcfromtimestamp(float(v))
        s = str(v).strip()
        if s.endswith("Z"):
            s = s[:-1]
        for f in (fmt,) if fmt else _DT_FORMATS:
            try:
                return _dt.datetime.strptime(s, f)
            except Exception:
                pass
        try:
            return _dt.datetime.fromisoformat(s)
        except Exception:
            raise ValueError("could not parse date: " + str(v))

    def to_datetime(arg, format=None, errors="raise"):
        def _one(v):
            try:
                return _parse_dt(v, format)
            except Exception:
                if errors == "coerce":
                    return _NA
                raise

        if isinstance(arg, Series):
            return Series([_one(v) for v in arg._v], index=list(arg._i), name=arg.name)
        if isinstance(arg, DataFrame):
            data = {c: [_one(v) for v in arg._d[c]] for c in arg._c}
            return DataFrame(data, columns=list(arg._c), index=list(arg._i))
        if _is_seq(arg):
            return Series([_one(v) for v in _to_list(arg)])
        return _one(arg)

    def _freq_step(freq):
        f = str(freq).upper()
        num = ""
        while f and f[0].isdigit():
            num = num + f[0]
            f = f[1:]
        mult = int(num) if num else 1
        if f in ("D", "DAY", "DAYS"):
            return ("delta", _dt.timedelta(days=mult))
        if f in ("W", "W-SUN", "WEEK"):
            return ("delta", _dt.timedelta(weeks=mult))
        if f in ("H", "HOUR", "HOURS"):
            return ("delta", _dt.timedelta(hours=mult))
        if f in ("T", "MIN", "MINUTE", "MINUTES"):
            return ("delta", _dt.timedelta(minutes=mult))
        if f in ("S", "SEC", "SECOND", "SECONDS"):
            return ("delta", _dt.timedelta(seconds=mult))
        if f in ("MS", "M", "MONTH", "ME"):
            return ("month", mult if f != "ME" and f != "M" else -mult)
        if f in ("Y", "YS", "A", "YEAR", "YE"):
            return ("year", mult if f in ("Y", "YS", "A", "YEAR") else -mult)
        raise ValueError("unsupported freq: " + str(freq))

    def _add_months(d, k):
        m = d.month - 1 + k
        y = d.year + m // 12
        m = m % 12 + 1
        day = d.day
        while True:
            try:
                return d.replace(year=y, month=m, day=day)
            except ValueError:
                day = day - 1

    def _month_end(d):
        nxt = _add_months(_dt.datetime(d.year, d.month, 1), 1)
        return nxt - _dt.timedelta(days=1)

    def date_range(start=None, end=None, periods=None, freq="D"):
        kind, step = _freq_step(freq)
        st = _parse_dt(start) if start is not None else None
        en = _parse_dt(end) if end is not None else None

        def _next(d):
            if kind == "delta":
                return d + step
            if kind == "month":
                k = abs(step)
                return _month_end(_add_months(d, k)) if step < 0 else _add_months(d, k)
            k = abs(step)
            nd = _add_months(d, 12 * k)
            return _month_end(nd) if step < 0 else nd

        if st is None:
            if en is None or periods is None:
                raise ValueError("date_range needs start, or end plus periods")
            out = [en]
            for _k in range(periods - 1):
                d = out[0]
                if kind == "delta":
                    out.insert(0, d - step)
                else:
                    out.insert(
                        0, _add_months(d, -abs(step) * (12 if kind == "year" else 1))
                    )
            return out
        if kind == "month" and step < 0:
            st = _month_end(st)
        out = [st]
        if periods is not None:
            while len(out) < periods:
                out.append(_next(out[-1]))
            return out
        if en is None:
            raise ValueError("date_range needs end or periods")
        while True:
            nxt = _next(out[-1])
            if nxt > en:
                break
            out.append(nxt)
        return out

    def pivot_table(
        data, values=None, index=None, columns=None, aggfunc="mean", fill_value=None
    ):
        return data.pivot_table(
            values=values,
            index=index,
            columns=columns,
            aggfunc=aggfunc,
            fill_value=fill_value,
        )

    def crosstab(rows, cols):
        a = rows._v if isinstance(rows, Series) else _to_list(rows)
        b = cols._v if isinstance(cols, Series) else _to_list(cols)
        df = DataFrame({"r": list(a), "c": list(b), "n": [1] * len(a)})
        return df.pivot_table(
            values="n", index="r", columns="c", aggfunc="sum", fill_value=0
        )

    NA = _NA
    NaN = _NA

    mod = types.ModuleType("pandas")
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
    mod.to_datetime = to_datetime
    mod.date_range = date_range
    mod.pivot_table = pivot_table
    mod.crosstab = crosstab
    mod.Timestamp = Timestamp
    mod.Timedelta = Timedelta
    mod.NA = _NA
    mod.NaN = _NA
    mod.__version__ = "2.0.0-vis-shim"
    # pandas is a package in the real distribution; retain that import contract.
    mod.__path__ = []

    api_mod = types.ModuleType("pandas.api")
    api_types = types.ModuleType("pandas.api.types")

    def _dtype_value(value):
        if isinstance(value, str):
            return value.lower()
        if hasattr(value, "dtype"):
            return str(value.dtype).lower()
        return _infer_dtype(_to_list(value)).lower()

    api_types.is_numeric_dtype = lambda value: any(
        token in _dtype_value(value) for token in ("int", "float", "complex")
    )
    api_types.is_integer_dtype = lambda value: "int" in _dtype_value(value)
    api_types.is_float_dtype = lambda value: "float" in _dtype_value(value)
    api_types.is_bool_dtype = lambda value: _dtype_value(value) == "bool"
    api_types.is_string_dtype = lambda value: (
        _dtype_value(value) in ("object", "string")
    )
    api_mod.types = api_types

    testing_mod = types.ModuleType("pandas.testing")

    def _assert_frame_equal(left, right, **kwargs):
        if not isinstance(left, DataFrame) or not isinstance(right, DataFrame):
            raise AssertionError("assert_frame_equal requires two DataFrames")
        if left.to_dict("records") != right.to_dict("records") or list(
            left.columns
        ) != list(right.columns):
            raise AssertionError("DataFrames are different")

    def _assert_series_equal(left, right, **kwargs):
        if (
            not isinstance(left, Series)
            or not isinstance(right, Series)
            or left.tolist() != right.tolist()
        ):
            raise AssertionError("Series are different")

    testing_mod.assert_frame_equal = _assert_frame_equal
    testing_mod.assert_series_equal = _assert_series_equal

    plotting_mod = types.ModuleType("pandas.plotting")
    plotting_mod.scatter_matrix = lambda frame, *args, **kwargs: []
    plotting_mod.register_matplotlib_converters = lambda: None

    tseries_mod = types.ModuleType("pandas.tseries")
    offsets_mod = types.ModuleType("pandas.tseries.offsets")

    class Day(_dt.timedelta):
        def __new__(cls, n=1, **kwargs):
            return _dt.timedelta.__new__(cls, days=int(n))

        @property
        def n(self):
            """Number of day units, matching pandas.tseries.offsets.Day."""
            return self.days

    offsets_mod.Day = Day
    tseries_mod.offsets = offsets_mod
    mod.api = api_mod
    mod.testing = testing_mod
    mod.plotting = plotting_mod
    mod.tseries = tseries_mod
    sys.modules["pandas"] = mod
    sys.modules["pandas.api"] = api_mod
    sys.modules["pandas.api.types"] = api_types
    sys.modules["pandas.testing"] = testing_mod
    sys.modules["pandas.plotting"] = plotting_mod
    sys.modules["pandas.tseries"] = tseries_mod
    sys.modules["pandas.tseries.offsets"] = offsets_mod
    try:
        _bi.pandas = mod
    except Exception:
        pass


__vis_install_pandas__()
del __vis_install_pandas__
