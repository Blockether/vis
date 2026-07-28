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
     "Sandbox shim: a `pandas`-compatible module (import pandas / pandas.DataFrame) implemented in PURE Python (Series + DataFrame with selection, loc/iloc, boolean masks, groupby, merge, concat, describe, read_csv/to_csv) on the stdlib. Interoperates with the numpy shim. No pip, no native wheel, no host bridge. A correctness-focused subset, not C-speed pandas."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pandas"
       :shim/imports ["pandas"]
       :shim/description
       "pandas-compatible `pandas` in pure Python (Series, DataFrame, groupby, merge, read_csv). Not supported: C-speed; `to_csv(path)` disabled (use `to_csv()` for a string); vectorized/IO-heavy APIs beyond the documented set."
       :shim/source "vis-shims/pandas.py"}]}))

(vis/register-extension! vis-extension)
