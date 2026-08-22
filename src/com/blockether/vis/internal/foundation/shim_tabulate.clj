(ns com.blockether.vis.internal.foundation.shim-tabulate
  "Built-in sandbox SHIM: a `tabulate`-compatible module for the model's Python
   sandbox, implemented in PURE Python (stdlib only) — NO host/JVM bridge. The
   `tabulate` PyPI package is not in GraalPy, so agents that reach for
   `from tabulate import tabulate` would otherwise hit ModuleNotFoundError; this
   extension contributes a `:ext/sandbox-shims` entry that env-python installs
   into every sandbox Context.

   The shim covers the tablefmts agents reach for most: plain, simple, github,
   pipe, orgtbl, presto, grid, fancy_grid, rst, tsv and html, with numeric /
   string alignment, `floatfmt`, `showindex`, and `headers='keys'/'firstrow'`.
   It accepts list-of-lists, list-of-dicts, dict-of-lists, and duck-types the
   pandas shim's DataFrame. A correctness-focused SUBSET, not full tabulate.

   Like `shim-numpy` there are NO `:shim/bindings`: a self-contained Python
   preamble with zero host callables. Publishes a `tabulate` module into
   `sys.modules` (so `import tabulate` works) and staples the `tabulate` fn onto
   builtins."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-tabulate"
     :ext/description
     (str "Sandbox pure-Python `tabulate` subset for lists, dicts, and DataFrames: "
          "plain/simple/GitHub/pipe/grid/fancy_grid/rst/tsv/html tables, alignment, and "
          "floatfmt. No pip/wheel/host bridge.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "tabulate" :shim/imports ["tabulate"] :shim/source "vis-shims/tabulate.py"}]}))

(vis/register-extension! vis-extension)
