(ns com.blockether.vis.internal.foundation.shim-numpy
  "Built-in sandbox SHIM: a `numpy`-compatible module for the model's Python
   sandbox, implemented in PURE Python (stdlib math + random) — NO host/JVM
   bridge, NOT a line of Clojure or babashka. numpy is a native C wheel that does
   not ship in GraalPy, so agents that reach for `import numpy` would otherwise
   hit ModuleNotFoundError; this extension contributes a `:ext/sandbox-shims`
   entry that `env-python/build-agent-context` installs into every sandbox
   Context (main + every `sub_loop` fork).

   The shim is a correctness-focused SUBSET, not a C-speed numpy: an `ndarray`
   backed by a flat Python list + shape tuple, with broadcasting, reductions,
   ufuncs, fancy/boolean/slice indexing, `dot`/`matmul`, a `linalg` submodule
   (norm/det/inv/solve/matrix_power/matrix_rank via pure-Python Gaussian
   elimination) and a `random` submodule (stdlib random). Big arrays are slow;
   the goal is that agent glue code (`np.array`, arithmetic, `mean`/`sum`, small
   linear algebra) just works.

   Like `shim-requests` there are NO `:shim/bindings`: the shim is a
   self-contained Python preamble with zero host callables. It publishes a
   `numpy` module into `sys.modules` (so `import numpy` works) and staples it
   onto builtins (so `numpy.array(...)` works with NO import, like json/os)."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private numpy-compat-shim-src
  "Pure-Python preamble that publishes a `numpy`-compatible module implemented on
   the stdlib (math + random). Zero host callables. Published into `sys.modules`
   under `numpy` (so `import numpy` finds it) AND stapled onto builtins (so
   `numpy.array(...)` works with NO import). INLINED here so it ships in-jar with
   no separate `.py` resource. Installed once per sandbox context (main + every
   `sub_loop` fork), BEFORE the baseline snapshot so its `__vis_*` names are
   filtered out of the model-visible live-vars view. The Python body uses ONLY
   single-quoted string literals and `chr(...)` for any special char, so this
   Clojure string needs zero backslash escaping."
  "# vis sandbox numpy-compat shim.
#
# The agent sandbox ships no numpy wheel. This shim publishes a numpy-compatible
# module implemented in PURE Python (no host/JVM bridge): an ndarray backed by a
# flat Python list + shape tuple, with broadcasting, reductions, ufuncs, indexing,
# linear-algebra basics and a random submodule (stdlib random). A deliberate
# correctness-focused SUBSET, not a C-speed numpy. Published into sys.modules so
# `import numpy` works, and stapled onto builtins so `np`-less `numpy.array(...)`
# needs no import (mirrors json/os).

def __vis_install_numpy__():
    import sys, types, math
    import random as _random
    import builtins as _bi

    _INT = 'int64'
    _FLOAT = 'float64'
    _BOOL = 'bool'
    _NL = chr(10)

    def _prod(seq):
        r = 1
        for x in seq:
            r = r * x
        return r

    def _is_seq(x):
        return isinstance(x, (list, tuple))

    def _infer_shape(obj):
        shp = []
        o = obj
        while _is_seq(o):
            shp.append(len(o))
            o = o[0] if len(o) else 0
        return tuple(shp)

    def _flatten_into(obj, out):
        if _is_seq(obj):
            for e in obj:
                _flatten_into(e, out)
        else:
            out.append(obj)

    def _cast(v, dt):
        if dt == _BOOL:
            return bool(v)
        w = _WRAP.get(dt) if isinstance(dt, str) else None
        if w is not None:
            return w(v)
        if dt == _FLOAT or (isinstance(dt, str) and 'float' in dt):
            return float(v)
        return int(v)

    def _sdiv(x, y):
        try:
            return x / y
        except ZeroDivisionError:
            if x != x or x == 0:
                return float('nan')
            return float('inf') if x > 0 else float('-inf')
    def _sfloordiv(x, y):
        try:
            return x // y
        except ZeroDivisionError:
            return 0.0
    def _smod(x, y):
        try:
            return x % y
        except ZeroDivisionError:
            return float('nan')
    def _srecip(x):
        try:
            return 1.0 / x
        except ZeroDivisionError:
            return float('inf')
    def _ssqrt(x):
        if x < 0:
            return float('nan')
        return math.sqrt(x)
    def _slog(x):
        if x < 0:
            return float('nan')
        if x == 0:
            return float('-inf')
        return math.log(x)
    def _slog2(x):
        if x < 0:
            return float('nan')
        if x == 0:
            return float('-inf')
        return math.log2(x)
    def _slog10(x):
        if x < 0:
            return float('nan')
        if x == 0:
            return float('-inf')
        return math.log10(x)

    def _values_dtype(values):
        has_float = False
        all_bool = True
        for v in values:
            if isinstance(v, bool):
                continue
            all_bool = False
            if isinstance(v, float):
                has_float = True
        if values and all_bool:
            return _BOOL
        return _FLOAT if has_float else _INT

    def _strides(shape):
        st = [1] * len(shape)
        acc = 1
        for i in range(len(shape) - 1, -1, -1):
            st[i] = acc
            acc = acc * shape[i]
        return st

    def _unravel(off, shape):
        idx = []
        for s in _strides(shape):
            idx.append(off // s % (shape[len(idx)] if shape else 1))
        return tuple(idx)

    def _ravel(idx, strides):
        o = 0
        for i, s in zip(idx, strides):
            o = o + i * s
        return o

    class _DType:
        def __init__(self, name):
            self.name = name
        @property
        def kind(self):
            n = self.name
            if n[:4] == 'uint':
                return 'u'
            if n[:3] == 'int':
                return 'i'
            if n[:5] == 'float':
                return 'f'
            if n == 'bool':
                return 'b'
            return 'O'
        @property
        def itemsize(self):
            n = self.name
            if n == 'bool':
                return 1
            digits = ''.join(c for c in n if c.isdigit())
            return int(digits) // 8 if digits else 8
        def __eq__(self, other):
            if isinstance(other, _DType):
                return self.name == other.name
            return self.name == _dtype_name(other)
        def __hash__(self):
            return hash(self.name)
        def __repr__(self):
            return 'dtype(' + chr(39) + self.name + chr(39) + ')'
        def __str__(self):
            return self.name

    class _ScalarType:
        def __init__(self, name, base, cast):
            self.__name__ = name
            self.base = base
            self._cast = cast
        def __call__(self, x=0):
            if isinstance(x, ndarray):
                return x.astype(self)
            return self._cast(x)
        def __eq__(self, other):
            if isinstance(other, _ScalarType):
                return self.__name__ == other.__name__
            return _dtype_name(self) == _dtype_name(other)
        def __hash__(self):
            return hash(self.__name__)
        def __repr__(self):
            return '<class ' + chr(39) + 'numpy.' + self.__name__ + chr(39) + '>'
    def _wu(bits):
        m = 1 << bits
        def f(x):
            return int(x) & (m - 1)
        return f
    def _wi(bits):
        m = 1 << bits
        def f(x):
            y = int(x) & (m - 1)
            if y >= (m >> 1):
                y = y - m
            return y
        return f
    _t_int8 = _ScalarType('int8', _INT, _wi(8))
    _t_int16 = _ScalarType('int16', _INT, _wi(16))
    _t_int32 = _ScalarType('int32', _INT, _wi(32))
    _t_int64 = _ScalarType('int64', _INT, int)
    _t_uint8 = _ScalarType('uint8', _INT, _wu(8))
    _t_uint16 = _ScalarType('uint16', _INT, _wu(16))
    _t_uint32 = _ScalarType('uint32', _INT, _wu(32))
    _t_uint64 = _ScalarType('uint64', _INT, _wu(64))
    _t_float16 = _ScalarType('float16', _FLOAT, float)
    _t_float32 = _ScalarType('float32', _FLOAT, float)
    _t_float64 = _ScalarType('float64', _FLOAT, float)
    _t_bool = _ScalarType('bool_', _BOOL, bool)

    _WRAP = {'int8': _wi(8), 'int16': _wi(16), 'int32': _wi(32), 'int64': int,
             'uint8': _wu(8), 'uint16': _wu(16), 'uint32': _wu(32), 'uint64': _wu(64)}

    def _dtype_name(dt):
        if dt is None:
            return None
        if isinstance(dt, _DType):
            return dt.name
        if isinstance(dt, _ScalarType):
            return _BOOL if dt.base == _BOOL else dt.__name__
        if isinstance(dt, str):
            if dt in _WRAP or dt in ('float16', 'float32', 'float64', 'bool'):
                return dt
            alias = {'int': _INT, 'i8': _INT, 'i4': 'int32', 'i2': 'int16', 'i1': 'int8',
                     'u1': 'uint8', 'u2': 'uint16', 'u4': 'uint32', 'u8': 'uint64',
                     'float': _FLOAT, 'f8': _FLOAT, 'f4': 'float32', 'f2': 'float16',
                     'double': _FLOAT, 'b': _BOOL, 'bool_': _BOOL}
            if dt in alias:
                return alias[dt]
            return _FLOAT if 'float' in dt else _INT
        if dt in (int,):
            return _INT
        if dt in (float,):
            return _FLOAT
        if dt in (bool,):
            return _BOOL
        return _FLOAT

    def _mk(data, shape, dtype):
        return ndarray(data, shape, dtype)

    def _asarray(obj, dtype=None):
        if isinstance(obj, ndarray):
            if dtype is None:
                return obj
            dn = _dtype_name(dtype)
            return _mk([_cast(v, dn) for v in obj._d], obj._shape, dn)
        if _is_seq(obj):
            shape = _infer_shape(obj)
            flat = []
            _flatten_into(obj, flat)
            dn = _dtype_name(dtype) if dtype is not None else _values_dtype(flat)
            return _mk([_cast(v, dn) for v in flat], shape, dn)
        dn = _dtype_name(dtype) if dtype is not None else _values_dtype([obj])
        return _mk([_cast(obj, dn)], (), dn)

    def _broadcast_shapes(a, b):
        ra, rb = list(a), list(b)
        n = max(len(ra), len(rb))
        ra = [1] * (n - len(ra)) + ra
        rb = [1] * (n - len(rb)) + rb
        out = []
        for x, y in zip(ra, rb):
            if x == y or x == 1 or y == 1:
                out.append(max(x, y))
            else:
                raise ValueError('operands could not be broadcast together with shapes '
                                 + str(tuple(a)) + ' ' + str(tuple(b)))
        return tuple(out)

    def _bc_index(out_idx, shape):
        n = len(out_idx)
        pad = n - len(shape)
        idx = []
        for i in range(len(shape)):
            dim = shape[i]
            coord = out_idx[pad + i]
            idx.append(0 if dim == 1 else coord)
        return tuple(idx)

    def _elementwise(a, b, fn, bool_out=False):
        A = _asarray(a)
        B = _asarray(b)
        oshape = _broadcast_shapes(A._shape, B._shape)
        ast = _strides(A._shape)
        bst = _strides(B._shape)
        out = []
        total = _prod(oshape) if oshape else 1
        if not oshape:
            val = fn(A._d[0], B._d[0])
            dn = _BOOL if bool_out else _values_dtype([val])
            return _mk([_cast(val, dn)], (), dn)
        for off in range(total):
            oidx = _unravel(off, oshape)
            ai = _ravel(_bc_index(oidx, A._shape), ast)
            bi = _ravel(_bc_index(oidx, B._shape), bst)
            out.append(fn(A._d[ai], B._d[bi]))
        dn = _BOOL if bool_out else _values_dtype(out)
        return _mk([_cast(v, dn) for v in out], oshape, dn)

    def _unary(a, fn, dtype=None):
        A = _asarray(a)
        out = [fn(v) for v in A._d]
        dn = dtype if dtype else _values_dtype(out)
        return _mk([_cast(v, dn) for v in out], A._shape, dn)

    def _normalize_axis(axis, ndim):
        if axis < 0:
            axis = axis + ndim
        return axis

    def _reduce(a, axis, fn, initial):
        A = _asarray(a)
        if axis is None:
            acc = initial
            for v in A._d:
                acc = fn(acc, v)
            return acc
        axis = _normalize_axis(axis, A.ndim)
        oshape = tuple(s for i, s in enumerate(A._shape) if i != axis)
        ost = _strides(oshape)
        ast = _strides(A._shape)
        out = [None] * (_prod(oshape) if oshape else 1)
        for off in range(len(A._d)):
            full = list(_unravel(off, A._shape))
            red = full[:axis] + full[axis + 1:]
            oi = _ravel(red, ost) if oshape else 0
            cur = out[oi]
            out[oi] = fn(cur, A._d[off]) if cur is not None else fn(initial, A._d[off])
        return out, oshape

    class ndarray:
        def __init__(self, data, shape, dtype):
            self._d = list(data)
            self._shape = tuple(shape)
            self._dtype = dtype

        @property
        def shape(self):
            return self._shape
        @property
        def ndim(self):
            return len(self._shape)
        @property
        def size(self):
            return len(self._d)
        @property
        def dtype(self):
            return _DType(self._dtype)
        @property
        def itemsize(self):
            return _DType(self._dtype).itemsize
        @property
        def nbytes(self):
            return self.itemsize * self.size
        @property
        def T(self):
            return self.transpose()
        @property
        def flat(self):
            return iter(self._d)
        @property
        def real(self):
            return self
        @property
        def imag(self):
            return zeros(self._shape)

        def astype(self, dtype):
            dn = _dtype_name(dtype)
            return _mk([_cast(v, dn) for v in self._d], self._shape, dn)

        def copy(self):
            return _mk(list(self._d), self._shape, self._dtype)

        def tolist(self):
            return _to_nested(self._d, self._shape, 0, [0])

        def item(self, *args):
            if not args:
                return self._d[0]
            if len(args) == 1:
                return self._d[args[0]]
            return self._d[_ravel(args, _strides(self._shape))]

        def reshape(self, *shape):
            if len(shape) == 1 and _is_seq(shape[0]):
                shape = tuple(shape[0])
            shape = list(shape)
            neg = [i for i, s in enumerate(shape) if s == -1]
            if neg:
                known = _prod([s for s in shape if s != -1])
                shape[neg[0]] = len(self._d) // known if known else 0
            shape = tuple(shape)
            if _prod(shape) != len(self._d):
                raise ValueError('cannot reshape array of size ' + str(len(self._d))
                                 + ' into shape ' + str(shape))
            return _mk(list(self._d), shape, self._dtype)

        def ravel(self):
            return _mk(list(self._d), (len(self._d),), self._dtype)

        def flatten(self):
            return self.ravel()

        def transpose(self, *axes):
            if len(axes) == 1 and _is_seq(axes[0]):
                axes = tuple(axes[0])
            if not axes:
                axes = tuple(range(self.ndim - 1, -1, -1))
            nshape = tuple(self._shape[a] for a in axes)
            ast = _strides(self._shape)
            nst = _strides(nshape)
            out = [0] * len(self._d)
            for off in range(len(self._d)):
                nidx = _unravel(off, nshape)
                oidx = [0] * self.ndim
                for i, a in enumerate(axes):
                    oidx[a] = nidx[i]
                out[off] = self._d[_ravel(oidx, ast)]
            return _mk(out, nshape, self._dtype)

        def squeeze(self, axis=None):
            if axis is None:
                nshape = tuple(s for s in self._shape if s != 1)
            else:
                axis = _normalize_axis(axis, self.ndim)
                nshape = tuple(s for i, s in enumerate(self._shape) if i != axis)
            return _mk(list(self._d), nshape, self._dtype)

        def fill(self, value):
            self._d = [_cast(value, self._dtype) for _ in self._d]

        def sum(self, axis=None):
            return sum(self, axis)
        def prod(self, axis=None):
            return prod(self, axis)
        def mean(self, axis=None):
            return mean(self, axis)
        def min(self, axis=None):
            return amin(self, axis)
        def max(self, axis=None):
            return amax(self, axis)
        def argmin(self, axis=None):
            return argmin(self, axis)
        def argmax(self, axis=None):
            return argmax(self, axis)
        def std(self, axis=None, ddof=0):
            return std(self, axis, ddof)
        def var(self, axis=None, ddof=0):
            return var(self, axis, ddof)
        def cumsum(self, axis=None):
            return cumsum(self, axis)
        def clip(self, a_min, a_max):
            return clip(self, a_min, a_max)
        def round(self, decimals=0):
            return around(self, decimals)
        def dot(self, other):
            return dot(self, other)
        def any(self):
            return _bi.any(bool(v) for v in self._d)
        def all(self):
            return _bi.all(bool(v) for v in self._d)
        def nonzero(self):
            return nonzero(self)
        def sort(self):
            self._d = sorted(self._d)
        def argsort(self):
            return argsort(self)

        def _getflat(self, idx):
            return self._d[idx]

        def __len__(self):
            if not self._shape:
                raise TypeError('len() of unsized object')
            return self._shape[0]

        def __iter__(self):
            if self.ndim <= 1:
                return iter(self._d)
            return (self[i] for i in range(self._shape[0]))

        def __getitem__(self, key):
            return _getitem(self, key)

        def __setitem__(self, key, value):
            _setitem(self, key, value)

        def __bool__(self):
            if len(self._d) == 1:
                return bool(self._d[0])
            raise ValueError('The truth value of an array with more than one element '
                             + 'is ambiguous. Use a.any() or a.all()')

        def __float__(self):
            return float(self._d[0])
        def __int__(self):
            return int(self._d[0])

        def __add__(self, o):
            return _elementwise(self, o, lambda x, y: x + y)
        def __radd__(self, o):
            return _elementwise(o, self, lambda x, y: x + y)
        def __sub__(self, o):
            return _elementwise(self, o, lambda x, y: x - y)
        def __rsub__(self, o):
            return _elementwise(o, self, lambda x, y: x - y)
        def __mul__(self, o):
            return _elementwise(self, o, lambda x, y: x * y)
        def __rmul__(self, o):
            return _elementwise(o, self, lambda x, y: x * y)
        def __truediv__(self, o):
            return _elementwise(self, o, lambda x, y: _sdiv(x, y))
        def __rtruediv__(self, o):
            return _elementwise(o, self, lambda x, y: _sdiv(x, y))
        def __floordiv__(self, o):
            return _elementwise(self, o, lambda x, y: _sfloordiv(x, y))
        def __rfloordiv__(self, o):
            return _elementwise(o, self, lambda x, y: _sfloordiv(x, y))
        def __mod__(self, o):
            return _elementwise(self, o, lambda x, y: _smod(x, y))
        def __rmod__(self, o):
            return _elementwise(o, self, lambda x, y: _smod(x, y))
        def __pow__(self, o):
            return _elementwise(self, o, lambda x, y: x ** y)
        def __rpow__(self, o):
            return _elementwise(o, self, lambda x, y: x ** y)
        def __matmul__(self, o):
            return matmul(self, o)
        def __neg__(self):
            return _unary(self, lambda x: -x)
        def __pos__(self):
            return self
        def __abs__(self):
            return _unary(self, lambda x: abs(x))

        def __eq__(self, o):
            return _elementwise(self, o, lambda x, y: x == y, bool_out=True)
        def __ne__(self, o):
            return _elementwise(self, o, lambda x, y: x != y, bool_out=True)
        def __lt__(self, o):
            return _elementwise(self, o, lambda x, y: x < y, bool_out=True)
        def __le__(self, o):
            return _elementwise(self, o, lambda x, y: x <= y, bool_out=True)
        def __gt__(self, o):
            return _elementwise(self, o, lambda x, y: x > y, bool_out=True)
        def __ge__(self, o):
            return _elementwise(self, o, lambda x, y: x >= y, bool_out=True)

        def __and__(self, o):
            return _elementwise(self, o, lambda x, y: bool(x) and bool(y), bool_out=True)
        def __or__(self, o):
            return _elementwise(self, o, lambda x, y: bool(x) or bool(y), bool_out=True)
        def __invert__(self):
            return _unary(self, lambda x: not bool(x), dtype=_BOOL)

        def __hash__(self):
            return None

        def __repr__(self):
            return 'array(' + repr(self.tolist()) + ')'
        def __str__(self):
            return str(self.tolist())

    def _to_nested(flat, shape, off, ctr):
        if not shape:
            return flat[ctr[0]] if False else flat[0]
        if len(shape) == 1:
            start = ctr[0]
            ctr[0] = ctr[0] + shape[0]
            return list(flat[start:ctr[0]])
        return [_to_nested(flat, shape[1:], off, ctr) for _ in range(shape[0])]

    # ---- indexing --------------------------------------------------------------
    def _getitem(arr, key):
        if isinstance(key, ndarray) and key._dtype == _BOOL:
            out = [arr._d[i] for i, m in enumerate(key._d) if m]
            return _mk(out, (len(out),), arr._dtype)
        if isinstance(key, ndarray):
            out = [arr[int(i)] for i in key._d]
            if out and isinstance(out[0], ndarray):
                return stack(out)
            return _mk([o if not isinstance(o, ndarray) else o for o in out],
                       (len(out),), arr._dtype)
        if isinstance(key, list):
            if key and isinstance(key[0], bool):
                out = [arr._d[i] for i, m in enumerate(key) if m]
                return _mk(out, (len(out),), arr._dtype)
            out = [arr[int(i)] for i in key]
            if out and isinstance(out[0], ndarray):
                return stack(out)
            return _mk(out, (len(out),), arr._dtype)
        if not isinstance(key, tuple):
            key = (key,)
        # expand ellipsis
        if _bi.any(k is Ellipsis for k in key):
            nexp = _bi.sum(1 for k in key if isinstance(k, (int, slice)))
            fill = arr.ndim - nexp
            newk = []
            for k in key:
                if k is Ellipsis:
                    newk.extend([slice(None)] * fill)
                else:
                    newk.append(k)
            key = tuple(newk)
        # pad with full slices
        nidx = _bi.sum(1 for k in key if k is not None)
        key = list(key) + [slice(None)] * (arr.ndim - nidx)
        # build per-axis index lists
        ranges = []
        axis = 0
        keep = []
        for k in key:
            if k is None:
                ranges.append([0])
                keep.append(1)
                continue
            dim = arr._shape[axis]
            if isinstance(k, int):
                kk = k + dim if k < 0 else k
                ranges.append([kk])
                keep.append(None)
            elif isinstance(k, slice):
                ranges.append(list(range(*k.indices(dim))))
                keep.append(len(ranges[-1]))
            else:
                raise TypeError('unsupported index ' + str(type(k)))
            axis = axis + 1
        oshape = tuple(k for k in keep if k is not None)
        ast = _strides(arr._shape)
        out = []
        import itertools as _it
        # iterate over kept-dim coordinates in row-major order
        # Build the axis order aligning ranges (including newaxis placeholders)
        real_axes = [i for i, k in enumerate(key) if k is not None]
        for combo in _it.product(*ranges):
            coord = []
            ai = 0
            for i, k in enumerate(key):
                if k is None:
                    continue
                coord.append(combo[i])
                ai = ai + 1
            out.append(arr._d[_ravel(coord, ast)])
        if not oshape:
            return out[0]
        return _mk(out, oshape, arr._dtype)

    def _setitem(arr, key, value):
        if isinstance(key, ndarray) and key._dtype == _BOOL:
            vals = value._d if isinstance(value, ndarray) else None
            j = 0
            for i, m in enumerate(key._d):
                if m:
                    if vals is not None:
                        arr._d[i] = _cast(vals[j % len(vals)], arr._dtype)
                        j = j + 1
                    else:
                        arr._d[i] = _cast(value, arr._dtype)
            return
        if not isinstance(key, tuple):
            key = (key,)
        key = list(key) + [slice(None)] * (arr.ndim - len(key))
        ranges = []
        axis = 0
        for k in key:
            dim = arr._shape[axis]
            if isinstance(k, int):
                kk = k + dim if k < 0 else k
                ranges.append([kk])
            elif isinstance(k, slice):
                ranges.append(list(range(*k.indices(dim))))
            axis = axis + 1
        ast = _strides(arr._shape)
        import itertools as _it
        targets = [ _ravel(list(combo), ast) for combo in _it.product(*ranges) ]
        if isinstance(value, ndarray):
            src = value._d
            for n, t in enumerate(targets):
                arr._d[t] = _cast(src[n % len(src)], arr._dtype)
        else:
            for t in targets:
                arr._d[t] = _cast(value, arr._dtype)

    # ---- creation --------------------------------------------------------------
    def array(obj, dtype=None, copy=True, ndmin=0):
        a = _asarray(obj, dtype)
        if a is obj and copy:
            a = a.copy()
        while a.ndim < ndmin:
            a = _mk(list(a._d), (1,) + a._shape, a._dtype)
        return a

    def asarray(obj, dtype=None):
        return _asarray(obj, dtype)

    def _shape_of(shape):
        if isinstance(shape, int):
            return (shape,)
        return tuple(shape)

    def zeros(shape, dtype=None):
        shp = _shape_of(shape)
        dn = _dtype_name(dtype) if dtype is not None else _FLOAT
        n = _prod(shp) if shp else 1
        return _mk([_cast(0, dn)] * n, shp, dn)

    def ones(shape, dtype=None):
        shp = _shape_of(shape)
        dn = _dtype_name(dtype) if dtype is not None else _FLOAT
        n = _prod(shp) if shp else 1
        return _mk([_cast(1, dn)] * n, shp, dn)

    def full(shape, fill_value, dtype=None):
        shp = _shape_of(shape)
        dn = _dtype_name(dtype) if dtype is not None else _values_dtype([fill_value])
        n = _prod(shp) if shp else 1
        return _mk([_cast(fill_value, dn)] * n, shp, dn)

    def empty(shape, dtype=None):
        return zeros(shape, dtype)

    def zeros_like(a, dtype=None):
        A = _asarray(a)
        return zeros(A._shape, dtype if dtype is not None else A._dtype)

    def ones_like(a, dtype=None):
        A = _asarray(a)
        return ones(A._shape, dtype if dtype is not None else A._dtype)

    def full_like(a, fill_value, dtype=None):
        A = _asarray(a)
        return full(A._shape, fill_value, dtype if dtype is not None else A._dtype)

    def empty_like(a, dtype=None):
        return zeros_like(a, dtype)

    def arange(*args, dtype=None):
        if len(args) == 1:
            start, stop, step = 0, args[0], 1
        elif len(args) == 2:
            start, stop, step = args[0], args[1], 1
        else:
            start, stop, step = args[0], args[1], args[2]
        out = []
        if isinstance(start, float) or isinstance(stop, float) or isinstance(step, float):
            n = int(math.ceil((stop - start) / step))
            for i in range(max(0, n)):
                out.append(start + i * step)
        else:
            v = start
            if step > 0:
                while v < stop:
                    out.append(v)
                    v = v + step
            else:
                while v > stop:
                    out.append(v)
                    v = v + step
        dn = _dtype_name(dtype) if dtype is not None else _values_dtype(out)
        return _mk([_cast(v, dn) for v in out], (len(out),), dn)

    def linspace(start, stop, num=50, endpoint=True, dtype=None):
        if num == 1:
            return _mk([float(start)], (1,), _FLOAT)
        div = (num - 1) if endpoint else num
        step = (stop - start) / div
        out = [start + step * i for i in range(num)]
        if endpoint:
            out[-1] = stop
        dn = _dtype_name(dtype) if dtype is not None else _FLOAT
        return _mk([_cast(v, dn) for v in out], (num,), dn)

    def eye(n, m=None, k=0, dtype=None):
        m = n if m is None else m
        dn = _dtype_name(dtype) if dtype is not None else _FLOAT
        data = []
        for i in range(n):
            for j in range(m):
                data.append(_cast(1 if j - i == k else 0, dn))
        return _mk(data, (n, m), dn)

    def identity(n, dtype=None):
        return eye(n, n, 0, dtype)

    def diag(v, k=0):
        A = _asarray(v)
        if A.ndim == 1:
            n = A._shape[0] + abs(k)
            out = zeros((n, n), A._dtype)
            for i in range(A._shape[0]):
                r = i if k >= 0 else i - k
                c = i + k if k >= 0 else i
                out._d[r * n + c] = A._d[i]
            return out
        n, m = A._shape
        out = []
        i = 0
        while 0 <= i < n and 0 <= i + k < m:
            out.append(A._d[i * m + (i + k)])
            i = i + 1
        return _mk(out, (len(out),), A._dtype)

    # ---- reductions ------------------------------------------------------------
    def sum(a, axis=None):
        r = _reduce(a, axis, lambda acc, v: acc + v, 0)
        if axis is None:
            return r
        out, oshape = r
        A = _asarray(a)
        return _mk(out, oshape, A._dtype)

    def prod(a, axis=None):
        r = _reduce(a, axis, lambda acc, v: acc * v, 1)
        if axis is None:
            return r
        out, oshape = r
        return _mk(out, oshape, _asarray(a)._dtype)

    def amin(a, axis=None):
        A = _asarray(a)
        if axis is None:
            return min(A._d)
        r = _reduce(a, axis, lambda acc, v: v if acc is None else min(acc, v), None)
        out, oshape = r
        return _mk(out, oshape, A._dtype)

    def amax(a, axis=None):
        A = _asarray(a)
        if axis is None:
            return max(A._d)
        r = _reduce(a, axis, lambda acc, v: v if acc is None else max(acc, v), None)
        out, oshape = r
        return _mk(out, oshape, A._dtype)


    def mean(a, axis=None):
        A = _asarray(a)
        if axis is None:
            return _bi.sum(A._d) / len(A._d) if A._d else float('nan')
        s = sum(a, axis)
        n = A._shape[_normalize_axis(axis, A.ndim)]
        return _elementwise(s, n, lambda x, y: x / y)

    def _count_along(A, axis):
        return A._shape[_normalize_axis(axis, A.ndim)]

    def var(a, axis=None, ddof=0):
        A = _asarray(a)
        if axis is None:
            m = mean(a)
            return _bi.sum((v - m) ** 2 for v in A._d) / (len(A._d) - ddof)
        m = mean(a, axis)
        diff2 = _elementwise(a, _expand_dims_like(m, A, axis), lambda x, y: (x - y) ** 2)
        s = sum(diff2, axis)
        n = _count_along(A, axis) - ddof
        return _elementwise(s, n, lambda x, y: x / y)

    def std(a, axis=None, ddof=0):
        v = var(a, axis, ddof)
        if isinstance(v, ndarray):
            return _unary(v, lambda x: math.sqrt(x))
        return math.sqrt(v)

    def _expand_dims_like(reduced, A, axis):
        axis = _normalize_axis(axis, A.ndim)
        nshape = list(A._shape)
        nshape[axis] = 1
        if isinstance(reduced, ndarray):
            return _mk(list(reduced._d), tuple(nshape), reduced._dtype)
        return _mk([reduced], tuple(nshape), _values_dtype([reduced]))

    def median(a, axis=None):
        A = _asarray(a)
        def _med(vals):
            s = sorted(vals)
            n = len(s)
            if n == 0:
                return float('nan')
            if n % 2:
                return float(s[n // 2])
            return (s[n // 2 - 1] + s[n // 2]) / 2
        if axis is None:
            return _med(A._d)
        if A.ndim == 2:
            r, c = A._shape
            d = list(A._d)
            if axis in (-1, 1):
                return _mk([_med(d[i * c:(i + 1) * c]) for i in range(r)], (r,), _FLOAT)
            return _mk([_med(d[j::c]) for j in range(c)], (c,), _FLOAT)
        raise NotImplementedError('median along an axis of a >2d array is not supported in the vis shim')

    def percentile(a, q, axis=None):
        A = _asarray(a)
        s = sorted(A._d)
        def _p(pp):
            if not s:
                return float('nan')
            k = (pp / 100.0) * (len(s) - 1)
            lo = int(math.floor(k))
            hi = int(math.ceil(k))
            if lo == hi:
                return float(s[lo])
            return s[lo] + (s[hi] - s[lo]) * (k - lo)
        if _is_seq(q):
            return _mk([_p(x) for x in q], (len(q),), _FLOAT)
        return _p(q)
    def quantile(a, q, axis=None):
        if _is_seq(q):
            return percentile(a, [x * 100 for x in q])
        return percentile(a, q * 100)

    def _argreduce(a, axis, better):
        A = _asarray(a)
        if axis is None:
            best_i = 0
            for i in range(1, len(A._d)):
                if better(A._d[i], A._d[best_i]):
                    best_i = i
            return best_i
        axis = _normalize_axis(axis, A.ndim)
        oshape = tuple(s for i, s in enumerate(A._shape) if i != axis)
        ost = _strides(oshape)
        best = [None] * (_prod(oshape) if oshape else 1)
        besti = [0] * len(best)
        for off in range(len(A._d)):
            full = list(_unravel(off, A._shape))
            ai = full[axis]
            red = full[:axis] + full[axis + 1:]
            oi = _ravel(red, ost) if oshape else 0
            if best[oi] is None or better(A._d[off], best[oi]):
                best[oi] = A._d[off]
                besti[oi] = ai
        return _mk(besti, oshape, _INT)

    def argmin(a, axis=None):
        return _argreduce(a, axis, lambda x, b: x < b)
    def argmax(a, axis=None):
        return _argreduce(a, axis, lambda x, b: x > b)

    def cumsum(a, axis=None):
        A = _asarray(a)
        if axis is None:
            out = []
            acc = 0
            for v in A._d:
                acc = acc + v
                out.append(acc)
            return _mk(out, (len(out),), A._dtype)
        raise NotImplementedError('cumsum along an axis is not supported in the vis shim')

    def cumprod(a, axis=None):
        A = _asarray(a)
        out = []
        acc = 1
        for v in A._d:
            acc = acc * v
            out.append(acc)
        return _mk(out, (len(out),), A._dtype)

    def _bool_reduce(a):
        return _asarray(a)._d

    def any(a, axis=None):
        return _bi.any(bool(v) for v in _bool_reduce(a))
    def all(a, axis=None):
        return _bi.all(bool(v) for v in _bool_reduce(a))

    def count_nonzero(a):
        return _bi.sum(1 for v in _asarray(a)._d if v)

    def nonzero(a):
        A = _asarray(a)
        idxs = [i for i, v in enumerate(A._d) if v]
        return (_mk(idxs, (len(idxs),), _INT),)

    def clip(a, a_min, a_max):
        def f(x):
            if a_min is not None and x < a_min:
                return a_min
            if a_max is not None and x > a_max:
                return a_max
            return x
        return _unary(a, f)

    def around(a, decimals=0):
        return _unary(a, lambda x: round(x, decimals))
    round_ = around

    # ---- ufuncs ----------------------------------------------------------------
    def sqrt(a):
        return _unary(a, lambda x: _ssqrt(x), dtype=_FLOAT)
    def exp(a):
        return _unary(a, lambda x: math.exp(x), dtype=_FLOAT)
    def log(a):
        return _unary(a, lambda x: _slog(x), dtype=_FLOAT)
    def log2(a):
        return _unary(a, lambda x: _slog2(x), dtype=_FLOAT)
    def log10(a):
        return _unary(a, lambda x: _slog10(x), dtype=_FLOAT)
    def sin(a):
        return _unary(a, lambda x: math.sin(x), dtype=_FLOAT)
    def cos(a):
        return _unary(a, lambda x: math.cos(x), dtype=_FLOAT)
    def tan(a):
        return _unary(a, lambda x: math.tan(x), dtype=_FLOAT)
    def arcsin(a):
        return _unary(a, lambda x: math.asin(x), dtype=_FLOAT)
    def arccos(a):
        return _unary(a, lambda x: math.acos(x), dtype=_FLOAT)
    def arctan(a):
        return _unary(a, lambda x: math.atan(x), dtype=_FLOAT)
    def arctan2(y, x):
        return _elementwise(y, x, lambda a, b: math.atan2(a, b))
    def sinh(a):
        return _unary(a, lambda x: math.sinh(x), dtype=_FLOAT)
    def cosh(a):
        return _unary(a, lambda x: math.cosh(x), dtype=_FLOAT)
    def tanh(a):
        return _unary(a, lambda x: math.tanh(x), dtype=_FLOAT)
    def absolute(a):
        return _unary(a, lambda x: abs(x))
    abs_ = absolute
    def floor(a):
        return _unary(a, lambda x: math.floor(x), dtype=_FLOAT)
    def ceil(a):
        return _unary(a, lambda x: math.ceil(x), dtype=_FLOAT)
    def trunc(a):
        return _unary(a, lambda x: math.trunc(x), dtype=_FLOAT)
    def sign(a):
        return _unary(a, lambda x: (x > 0) - (x < 0))
    def rint(a):
        return _unary(a, lambda x: float(round(x)), dtype=_FLOAT)
    def square(a):
        return _unary(a, lambda x: x * x)
    def reciprocal(a):
        return _unary(a, lambda x: _srecip(x), dtype=_FLOAT)
    def degrees(a):
        return _unary(a, lambda x: math.degrees(x), dtype=_FLOAT)
    def radians(a):
        return _unary(a, lambda x: math.radians(x), dtype=_FLOAT)
    def isnan(a):
        return _unary(a, lambda x: x != x, dtype=_BOOL)
    def isinf(a):
        return _unary(a, lambda x: x in (float('inf'), float('-inf')), dtype=_BOOL)
    def isfinite(a):
        return _unary(a, lambda x: not (x != x or x in (float('inf'), float('-inf'))), dtype=_BOOL)

    def power(a, b):
        return _elementwise(a, b, lambda x, y: x ** y)
    def mod(a, b):
        return _elementwise(a, b, lambda x, y: x % y)
    def remainder(a, b):
        return _elementwise(a, b, lambda x, y: x % y)
    def add(a, b):
        return _elementwise(a, b, lambda x, y: x + y)
    def subtract(a, b):
        return _elementwise(a, b, lambda x, y: x - y)
    def multiply(a, b):
        return _elementwise(a, b, lambda x, y: x * y)
    def divide(a, b):
        return _elementwise(a, b, lambda x, y: _sdiv(x, y))
    true_divide = divide
    def floor_divide(a, b):
        return _elementwise(a, b, lambda x, y: _sfloordiv(x, y))
    def maximum(a, b):
        return _elementwise(a, b, lambda x, y: x if x >= y else y)
    def minimum(a, b):
        return _elementwise(a, b, lambda x, y: x if x <= y else y)
    def hypot(a, b):
        return _elementwise(a, b, lambda x, y: math.hypot(x, y))
    def logaddexp(a, b):
        return _elementwise(a, b, lambda x, y: math.log(math.exp(x) + math.exp(y)))
    def fmax(a, b):
        return maximum(a, b)
    def fmin(a, b):
        return minimum(a, b)
    def logical_and(a, b):
        return _elementwise(a, b, lambda x, y: bool(x) and bool(y), bool_out=True)
    def logical_or(a, b):
        return _elementwise(a, b, lambda x, y: bool(x) or bool(y), bool_out=True)
    def logical_xor(a, b):
        return _elementwise(a, b, lambda x, y: bool(x) != bool(y), bool_out=True)
    def logical_not(a):
        return _unary(a, lambda x: not bool(x), dtype=_BOOL)

    def where(cond, x=None, y=None):
        C = _asarray(cond)
        if x is None and y is None:
            return nonzero(C)
        X = _asarray(x)
        Y = _asarray(y)
        oshape = _broadcast_shapes(_broadcast_shapes(C._shape, X._shape), Y._shape)
        cst, xst, yst = _strides(C._shape), _strides(X._shape), _strides(Y._shape)
        out = []
        total = _prod(oshape) if oshape else 1
        for off in range(total):
            oidx = _unravel(off, oshape) if oshape else ()
            c = C._d[_ravel(_bc_index(oidx, C._shape), cst)]
            xv = X._d[_ravel(_bc_index(oidx, X._shape), xst)]
            yv = Y._d[_ravel(_bc_index(oidx, Y._shape), yst)]
            out.append(xv if c else yv)
        dn = _values_dtype(out)
        return _mk([_cast(v, dn) for v in out], oshape, dn)

    # ---- linear algebra --------------------------------------------------------
    def dot(a, b):
        A = _asarray(a)
        B = _asarray(b)
        if A.ndim == 1 and B.ndim == 1:
            return _bi.sum(x * y for x, y in zip(A._d, B._d))
        return matmul(A, B)

    def matmul(a, b):
        A = _asarray(a)
        B = _asarray(b)
        if A.ndim == 1 and B.ndim == 1:
            return _bi.sum(x * y for x, y in zip(A._d, B._d))
        if A.ndim == 2 and B.ndim == 1:
            n, k = A._shape
            out = []
            for i in range(n):
                out.append(_bi.sum(A._d[i * k + j] * B._d[j] for j in range(k)))
            return _mk(out, (n,), _values_dtype(out))
        if A.ndim == 1 and B.ndim == 2:
            k, m = B._shape
            out = []
            for j in range(m):
                out.append(_bi.sum(A._d[t] * B._d[t * m + j] for t in range(k)))
            return _mk(out, (m,), _values_dtype(out))
        n, k = A._shape
        k2, m = B._shape
        if k != k2:
            raise ValueError('shapes ' + str(A._shape) + ' and ' + str(B._shape)
                             + ' not aligned')
        out = [0] * (n * m)
        for i in range(n):
            for j in range(m):
                s = 0
                for t in range(k):
                    s = s + A._d[i * k + t] * B._d[t * m + j]
                out[i * m + j] = s
        return _mk(out, (n, m), _values_dtype(out))

    def transpose(a, axes=None):
        A = _asarray(a)
        if axes is None:
            return A.transpose()
        return A.transpose(axes)

    def outer(a, b):
        A = _asarray(a)
        B = _asarray(b)
        out = []
        for x in A._d:
            for y in B._d:
                out.append(x * y)
        return _mk(out, (len(A._d), len(B._d)), _values_dtype(out))

    def cross(a, b):
        A = _asarray(a)._d
        B = _asarray(b)._d
        return _mk([A[1] * B[2] - A[2] * B[1],
                    A[2] * B[0] - A[0] * B[2],
                    A[0] * B[1] - A[1] * B[0]], (3,), _values_dtype(A + B))

    def trace(a):
        A = _asarray(a)
        n, m = A._shape
        return _bi.sum(A._d[i * m + i] for i in range(min(n, m)))

    # ---- manipulation ----------------------------------------------------------
    def concatenate(arrays, axis=0):
        arrs = [_asarray(x) for x in arrays]
        if axis is None or arrs[0].ndim <= 1:
            out = []
            for a in arrs:
                out.extend(a._d)
            return _mk(out, (len(out),), _values_dtype(out))
        axis = _normalize_axis(axis, arrs[0].ndim)
        base = list(arrs[0]._shape)
        base[axis] = _bi.sum(a._shape[axis] for a in arrs)
        oshape = tuple(base)
        out = zeros(oshape, _values_dtype([v for a in arrs for v in a._d]))
        ost = _strides(oshape)
        offset = 0
        for a in arrs:
            ast = _strides(a._shape)
            for off in range(len(a._d)):
                idx = list(_unravel(off, a._shape))
                idx[axis] = idx[axis] + offset
                out._d[_ravel(idx, ost)] = a._d[off]
            offset = offset + a._shape[axis]
        return out

    def stack(arrays, axis=0):
        arrs = [_asarray(x) for x in arrays]
        expanded = [expand_dims(a, axis) for a in arrs]
        return concatenate(expanded, axis)

    def vstack(arrays):
        arrs = [_asarray(x) for x in arrays]
        arrs = [a if a.ndim >= 2 else a.reshape(1, a._shape[0] if a.ndim else 1) for a in arrs]
        return concatenate(arrs, 0)

    def hstack(arrays):
        arrs = [_asarray(x) for x in arrays]
        if arrs[0].ndim <= 1:
            return concatenate(arrs, 0)
        return concatenate(arrs, 1)

    def column_stack(arrays):
        arrs = [_asarray(x) for x in arrays]
        cols = [a.reshape(a._shape[0], 1) if a.ndim == 1 else a for a in arrs]
        return concatenate(cols, 1)

    def expand_dims(a, axis):
        A = _asarray(a)
        axis = axis if axis >= 0 else axis + A.ndim + 1
        nshape = list(A._shape)
        nshape.insert(axis, 1)
        return _mk(list(A._d), tuple(nshape), A._dtype)

    def repeat(a, repeats, axis=None):
        A = _asarray(a)
        if axis is None:
            out = []
            for v in A._d:
                out.extend([v] * repeats)
            return _mk(out, (len(out),), A._dtype)
        raise NotImplementedError('repeat along an axis is not supported in the vis shim')

    def tile(a, reps):
        A = _asarray(a)
        if isinstance(reps, int):
            out = A._d * reps
            return _mk(out, (len(out),), A._dtype)
        raise NotImplementedError('tile with a tuple reps is not supported in the vis shim')

    def flip(a, axis=None):
        A = _asarray(a)
        if axis is None or A.ndim == 1:
            return _mk(list(reversed(A._d)), A._shape, A._dtype)
        if A.ndim == 2:
            r, c = A._shape
            d = list(A._d)
            if axis in (-1, 1):
                out = [d[i * c + (c - 1 - j)] for i in range(r) for j in range(c)]
            else:
                out = [d[(r - 1 - i) * c + j] for i in range(r) for j in range(c)]
            return _mk(out, A._shape, A._dtype)
        raise NotImplementedError('flip along an axis of a >2d array is not supported in the vis shim')

    def sort(a, axis=-1):
        A = _asarray(a)
        if A.ndim <= 1:
            return _mk(sorted(A._d), A._shape, A._dtype)
        if A.ndim == 2:
            r, c = A._shape
            d = list(A._d)
            if axis in (-1, 1):
                out = []
                for i in range(r):
                    out.extend(sorted(d[i * c:(i + 1) * c]))
                return _mk(out, A._shape, A._dtype)
            if axis == 0:
                cols = [sorted(d[j::c]) for j in range(c)]
                out = [cols[j][i] for i in range(r) for j in range(c)]
                return _mk(out, A._shape, A._dtype)
        raise NotImplementedError('sort of a >2d array is not supported in the vis shim')

    def argsort(a):
        A = _asarray(a)
        order = sorted(range(len(A._d)), key=lambda i: A._d[i])
        return _mk(order, (len(order),), _INT)

    def unique(a, return_counts=False):
        A = _asarray(a)
        seen = sorted(set(A._d))
        u = _mk(list(seen), (len(seen),), A._dtype)
        if return_counts:
            counts = [A._d.count(v) for v in seen]
            return u, _mk(counts, (len(counts),), _INT)
        return u

    def diff(a, n=1):
        A = _asarray(a)
        d = list(A._d)
        for _ in range(n):
            d = [d[i + 1] - d[i] for i in range(len(d) - 1)]
        return _mk(d, (len(d),), A._dtype)

    def roll(a, shift):
        A = _asarray(a)
        d = list(A._d)
        n = len(d)
        if n == 0:
            return _mk(d, A._shape, A._dtype)
        s = shift % n
        out = (d[-s:] + d[:-s]) if s else d
        return _mk(out, A._shape, A._dtype)

    def _nonan(a):
        return [x for x in _asarray(a)._d if not (isinstance(x, float) and x != x)]

    def nansum(a, axis=None):
        return _bi.sum(_nonan(a))

    def nanmean(a, axis=None):
        vals = _nonan(a)
        return _bi.sum(vals) / len(vals) if vals else float('nan')

    def nanmax(a, axis=None):
        vals = _nonan(a)
        return _bi.max(vals) if vals else float('nan')

    def nanmin(a, axis=None):
        vals = _nonan(a)
        return _bi.min(vals) if vals else float('nan')

    def nanstd(a, axis=None):
        vals = _nonan(a)
        if not vals:
            return float('nan')
        m = _bi.sum(vals) / len(vals)
        return math.sqrt(_bi.sum([(x - m) ** 2 for x in vals]) / len(vals))

    def pad(a, pad_width, mode='constant', constant_values=0):
        A = _asarray(a)
        if A.ndim != 1:
            raise NotImplementedError('pad supports 1-D arrays only in the vis shim')
        if isinstance(pad_width, int):
            before = after = pad_width
        else:
            before, after = pad_width[0], pad_width[1]
        d = list(A._d)
        if mode == 'edge':
            lv = d[0] if d else 0
            rv = d[-1] if d else 0
            out = [lv] * before + d + [rv] * after
        else:
            cv = constant_values
            out = [cv] * before + d + [cv] * after
        return _mk(out, (len(out),), A._dtype)

    def ravel(a):
        return _asarray(a).ravel()
    def reshape(a, newshape):
        return _asarray(a).reshape(newshape)
    def squeeze(a, axis=None):
        return _asarray(a).squeeze(axis)
    def flatten(a):
        return _asarray(a).ravel()

    def allclose(a, b, rtol=1e-05, atol=1e-08):
        A = _asarray(a)
        B = _asarray(b)
        for x, y in zip(A._d, B._d):
            if abs(x - y) > atol + rtol * abs(y):
                return False
        return True

    def array_equal(a, b):
        A = _asarray(a)
        B = _asarray(b)
        return A._shape == B._shape and A._d == B._d

    def isclose(a, b, rtol=1e-05, atol=1e-08):
        return _elementwise(a, b, lambda x, y: abs(x - y) <= atol + rtol * abs(y), bool_out=True)

    def dstack(arrays):
        raise NotImplementedError('dstack is not supported in the vis shim')

    def meshgrid(x, y):
        X = _asarray(x)._d
        Y = _asarray(y)._d
        gx = _mk([xv for _ in Y for xv in X], (len(Y), len(X)), _values_dtype(X))
        gy = _mk([yv for yv in Y for _ in X], (len(Y), len(X)), _values_dtype(Y))
        return gx, gy

    # ---- random ----------------------------------------------------------------
    class _Random:
        def __init__(self, seed=None):
            self._r = _random.Random(seed)
        def seed(self, s=None):
            self._r.seed(s)
        def random(self, size=None):
            if size is None:
                return self._r.random()
            return self._filled(size, lambda: self._r.random(), _FLOAT)
        def rand(self, *shape):
            if not shape:
                return self._r.random()
            return self._filled(shape, lambda: self._r.random(), _FLOAT)
        def randn(self, *shape):
            if not shape:
                return self._r.gauss(0, 1)
            return self._filled(shape, lambda: self._r.gauss(0, 1), _FLOAT)
        def standard_normal(self, size=None):
            if size is None:
                return self._r.gauss(0, 1)
            return self._filled(size, lambda: self._r.gauss(0, 1), _FLOAT)
        def normal(self, loc=0.0, scale=1.0, size=None):
            if size is None:
                return self._r.gauss(loc, scale)
            return self._filled(size, lambda: self._r.gauss(loc, scale), _FLOAT)
        def uniform(self, low=0.0, high=1.0, size=None):
            if size is None:
                return self._r.uniform(low, high)
            return self._filled(size, lambda: self._r.uniform(low, high), _FLOAT)
        def randint(self, low, high=None, size=None):
            if high is None:
                low, high = 0, low
            if size is None:
                return self._r.randrange(low, high)
            return self._filled(size, lambda: self._r.randrange(low, high), _INT)
        def integers(self, low, high=None, size=None, endpoint=False):
            if high is None:
                low, high = 0, low
            hi = high + 1 if endpoint else high
            if size is None:
                return self._r.randrange(low, hi)
            return self._filled(size, lambda: self._r.randrange(low, hi), _INT)
        def choice(self, a, size=None, replace=True):
            pool = _asarray(a)._d if isinstance(a, (list, tuple, ndarray)) else list(range(a))
            if size is None:
                return self._r.choice(pool)
            n = _prod(_shape_of(size))
            if replace:
                vals = [self._r.choice(pool) for _ in range(n)]
            else:
                vals = self._r.sample(pool, n)
            return _mk(vals, _shape_of(size), _values_dtype(vals))
        def shuffle(self, a):
            if isinstance(a, ndarray):
                self._r.shuffle(a._d)
            else:
                self._r.shuffle(a)
        def permutation(self, a):
            if isinstance(a, int):
                pool = list(range(a))
            else:
                pool = list(_asarray(a)._d)
            self._r.shuffle(pool)
            return _mk(pool, (len(pool),), _values_dtype(pool))
        def _filled(self, shape, gen, dt):
            shp = _shape_of(shape)
            n = _prod(shp) if shp else 1
            return _mk([gen() for _ in range(n)], shp, dt)

    # ---- module assembly -------------------------------------------------------
    mod = types.ModuleType('numpy')
    mod.__version__ = '1.26-vis-pure'
    mod.ndarray = ndarray
    mod.dtype = _DType
    mod.newaxis = None
    mod.pi = math.pi
    mod.e = math.e
    mod.inf = float('inf')
    mod.Inf = float('inf')
    mod.nan = float('nan')
    mod.NaN = float('nan')
    mod.euler_gamma = 0.5772156649015329
    mod.int64 = _t_int64
    mod.int32 = _t_int32
    mod.int16 = _t_int16
    mod.int8 = _t_int8
    mod.uint8 = _t_uint8
    mod.uint16 = _t_uint16
    mod.uint32 = _t_uint32
    mod.uint64 = _t_uint64
    mod.float64 = _t_float64
    mod.float32 = _t_float32
    mod.float16 = _t_float16
    mod.bool_ = _t_bool
    mod.int_ = _t_int64
    mod.intp = _t_int64
    mod.intc = _t_int32
    mod.float_ = _t_float64
    mod.double = _t_float64
    mod.byte = _t_int8
    mod.ubyte = _t_uint8
    mod.short = _t_int16

    _exports = {
        'array': array, 'asarray': asarray, 'zeros': zeros, 'ones': ones,
        'full': full, 'empty': empty, 'zeros_like': zeros_like, 'ones_like': ones_like,
        'full_like': full_like, 'empty_like': empty_like, 'arange': arange,
        'linspace': linspace, 'eye': eye, 'identity': identity, 'diag': diag,
        'sum': sum, 'prod': prod, 'amin': amin, 'amax': amax, 'min': amin, 'max': amax,
        'mean': mean, 'median': median, 'var': var, 'std': std, 'argmin': argmin,
        'argmax': argmax, 'cumsum': cumsum, 'cumprod': cumprod, 'any': any, 'all': all,
        'count_nonzero': count_nonzero, 'nonzero': nonzero, 'clip': clip,
        'percentile': percentile, 'quantile': quantile,
        'around': around, 'round': around, 'round_': around, 'rint': rint,
        'sqrt': sqrt, 'exp': exp, 'log': log, 'log2': log2, 'log10': log10,
        'sin': sin, 'cos': cos, 'tan': tan, 'arcsin': arcsin, 'arccos': arccos,
        'arctan': arctan, 'arctan2': arctan2, 'sinh': sinh, 'cosh': cosh, 'tanh': tanh,
        'absolute': absolute, 'abs': absolute, 'fabs': absolute, 'floor': floor,
        'ceil': ceil, 'trunc': trunc, 'sign': sign, 'square': square,
        'reciprocal': reciprocal, 'degrees': degrees, 'radians': radians,
        'deg2rad': radians, 'rad2deg': degrees, 'isnan': isnan, 'isinf': isinf,
        'isfinite': isfinite, 'power': power, 'mod': remainder,
        'remainder': remainder, 'add': add, 'subtract': subtract, 'multiply': multiply,
        'divide': divide, 'true_divide': divide, 'floor_divide': floor_divide,
        'maximum': maximum, 'minimum': minimum, 'hypot': hypot, 'logaddexp': logaddexp,
        'fmax': fmax, 'fmin': fmin, 'logical_and': logical_and, 'logical_or': logical_or,
        'logical_xor': logical_xor, 'logical_not': logical_not, 'where': where,
        'dot': dot, 'matmul': matmul, 'inner': dot, 'outer': outer, 'cross': cross,
        'trace': trace, 'transpose': transpose, 'concatenate': concatenate,
        'stack': stack, 'vstack': vstack, 'hstack': hstack, 'column_stack': column_stack,
        'expand_dims': expand_dims, 'repeat': repeat, 'tile': tile, 'flip': flip,
        'sort': sort, 'argsort': argsort, 'unique': unique, 'ravel': ravel,
        'reshape': reshape, 'squeeze': squeeze, 'allclose': allclose,
        'array_equal': array_equal, 'isclose': isclose, 'meshgrid': meshgrid,
        'diff': diff, 'roll': roll, 'nansum': nansum, 'nanmean': nanmean,
        'nanmax': nanmax, 'nanmin': nanmin, 'nanstd': nanstd, 'pad': pad,
    }
    for _k, _v in _exports.items():
        setattr(mod, _k, _v)

    # linalg submodule
    def _lu_decompose(A_flat, n):
        M = [A_flat[i * n:(i + 1) * n][:] for i in range(n)]
        M = [[float(x) for x in row] for row in M]
        perm = list(range(n))
        det_sign = 1.0
        for col in range(n):
            piv = max(range(col, n), key=lambda r: abs(M[r][col]))
            if abs(M[piv][col]) < 1e-15:
                return None, None, 0.0
            if piv != col:
                M[col], M[piv] = M[piv], M[col]
                perm[col], perm[piv] = perm[piv], perm[col]
                det_sign = -det_sign
            for r in range(col + 1, n):
                f = M[r][col] / M[col][col]
                M[r][col] = f
                for c in range(col + 1, n):
                    M[r][c] = M[r][c] - f * M[col][c]
        return M, perm, det_sign

    class _LinAlg:
        def norm(self, a, ord=None):
            A = _asarray(a)
            if ord is None or ord == 2:
                return math.sqrt(_bi.sum(x * x for x in A._d))
            if ord == 1:
                return _bi.sum(abs(x) for x in A._d)
            if ord == float('inf'):
                return _bi.max(abs(x) for x in A._d)
            return _bi.sum(abs(x) ** ord for x in A._d) ** (1.0 / ord)
        def det(self, a):
            A = _asarray(a)
            n = A._shape[0]
            M, perm, sign = _lu_decompose(A._d, n)
            if M is None:
                return 0.0
            d = sign
            for i in range(n):
                d = d * M[i][i]
            return d
        def inv(self, a):
            A = _asarray(a)
            n = A._shape[0]
            M = [[float(A._d[i * n + j]) for j in range(n)] for i in range(n)]
            I = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
            for col in range(n):
                piv = _bi.max(range(col, n), key=lambda r: abs(M[r][col]))
                if abs(M[piv][col]) < 1e-15:
                    raise ValueError('Singular matrix')
                M[col], M[piv] = M[piv], M[col]
                I[col], I[piv] = I[piv], I[col]
                d = M[col][col]
                M[col] = [x / d for x in M[col]]
                I[col] = [x / d for x in I[col]]
                for r in range(n):
                    if r != col:
                        f = M[r][col]
                        M[r] = [a - f * b for a, b in zip(M[r], M[col])]
                        I[r] = [a - f * b for a, b in zip(I[r], I[col])]
            out = [I[i][j] for i in range(n) for j in range(n)]
            return _mk(out, (n, n), _FLOAT)
        def solve(self, a, b):
            A = _asarray(a)
            B = _asarray(b)
            inv = self.inv(A)
            return matmul(inv, B)
        def matrix_power(self, a, n):
            A = _asarray(a)
            if n == 0:
                return identity(A._shape[0])
            r = A
            for _ in range(n - 1):
                r = matmul(r, A)
            return r
        def matrix_rank(self, a):
            A = _asarray(a)
            n, m = A._shape
            M = [[float(A._d[i * m + j]) for j in range(m)] for i in range(n)]
            rank = 0
            for col in range(m):
                piv = None
                for r in range(rank, n):
                    if abs(M[r][col]) > 1e-12:
                        piv = r
                        break
                if piv is None:
                    continue
                M[rank], M[piv] = M[piv], M[rank]
                d = M[rank][col]
                M[rank] = [x / d for x in M[rank]]
                for r in range(n):
                    if r != rank:
                        f = M[r][col]
                        M[r] = [a - f * b for a, b in zip(M[r], M[rank])]
                rank = rank + 1
            return rank

    linalg = types.ModuleType('numpy.linalg')
    _la = _LinAlg()
    for _n in ('norm', 'det', 'inv', 'solve', 'matrix_power', 'matrix_rank'):
        setattr(linalg, _n, getattr(_la, _n))
    mod.linalg = linalg

    random_mod = types.ModuleType('numpy.random')
    _rnd = _Random()
    for _n in ('seed', 'random', 'rand', 'randn', 'standard_normal', 'normal',
               'uniform', 'randint', 'integers', 'choice', 'shuffle', 'permutation'):
        setattr(random_mod, _n, getattr(_rnd, _n))
    random_mod.random_sample = _rnd.random
    random_mod.ranf = _rnd.random
    random_mod.RandomState = _Random
    random_mod.Generator = _Random
    random_mod.default_rng = lambda seed=None: _Random(seed)
    mod.random = random_mod

    def _mk_isscalar(x):
        return isinstance(x, (int, float, bool, complex))
    mod.isscalar = _mk_isscalar
    mod.ndim = lambda a: _asarray(a).ndim
    mod.shape = lambda a: _asarray(a).shape
    mod.size = lambda a: _asarray(a).size

    sys.modules['numpy'] = mod
    sys.modules['numpy.linalg'] = linalg
    sys.modules['numpy.random'] = random_mod

    try:
        import builtins as _b
        _b.numpy = mod
    except Exception:
        pass

__vis_install_numpy__()
del __vis_install_numpy__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-numpy"
     :ext/description
     "Sandbox shim: a `numpy`-compatible module (import numpy / numpy.array) implemented in PURE Python (ndarray with broadcasting, reductions, ufuncs, indexing, dot/matmul, linalg norm/det/inv/solve, random) on the stdlib. No pip, no native wheel, no host bridge. A correctness-focused subset, not C-speed numpy."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "numpy"
       :shim/description
       "numpy-compatible `numpy` in pure Python (ndarray, broadcasting, linalg, random). Not supported: C-speed; a few ops limited to <=2-D — median/cumsum/sort/flip/repeat along an axis, `tile` with tuple reps, `pad` beyond 1-D, `dstack` raise `NotImplementedError`."
       :shim/preamble numpy-compat-shim-src}]}))

(vis/register-extension! vis-extension)
