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


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pandas"
     :ext/description
     "Sandbox pure-Python `pandas` subset: Series/DataFrame, selection, loc/iloc, masks, groupby, merge, concat, describe, and CSV I/O; interoperates with the NumPy shim. Stdlib only, no pip/wheel/host bridge; correctness-focused, not C-speed."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pandas"
       :shim/imports ["pandas"]
       :shim/description
       "Pure-Python `pandas` subset: Series, DataFrame, groupby, merge, read_csv. No C speed or broader vectorized/IO-heavy APIs; `to_csv(path)` is disabled—use `to_csv()` for text."
       :shim/source "vis-shims/pandas.py"}]}))

(vis/register-extension! vis-extension)
