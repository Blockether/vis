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
       :shim/imports ["numpy"]
       :shim/description
       "numpy-compatible `numpy` in pure Python (ndarray, broadcasting, reductions with keepdims + tuple axis, linalg norm/det/inv/solve, random, split/take/repeat/histogram). Not supported: C-speed; slice views don't share memory; no linalg eig/svd/qr; a few ops limited to <=2-D — median/cumsum/sort/flip along an axis, `tile` with tuple reps, `pad` beyond 1-D, `dstack` raise `NotImplementedError`."
       :shim/source "vis-shims/numpy.py"}]}))

(vis/register-extension! vis-extension)
