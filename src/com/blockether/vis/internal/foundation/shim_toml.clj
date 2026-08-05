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


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-toml"
     :ext/description
     (str "Sandbox pure-Python `toml` subset: stdlib `tomllib` reading plus writing scalars, "
          "arrays, inline tables, nested sections, and arrays of tables. "
          "No pip/wheel/host bridge.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "toml"
       :shim/imports ["toml"]
       :shim/description
       (str "`toml`: `loads`/`load` via stdlib tomllib; `dumps`/`dump` via pure Python. "
            "Reads are spec-correct; no comment preservation or exotic writer formatting.")
       :shim/source "vis-shims/toml.py"}]}))

(vis/register-extension! vis-extension)
