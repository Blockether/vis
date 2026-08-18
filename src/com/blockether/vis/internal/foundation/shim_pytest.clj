(ns com.blockether.vis.internal.foundation.shim-pytest
  "Built-in sandbox SHIM: a `pytest`-compatible module for the model's Python
   sandbox, implemented PURELY in Python on the stdlib (`ast`, `inspect`,
   `linecache`, `traceback`, `warnings`, `tempfile`) — NO host/JVM bridge, NOT a
   line of Clojure or babashka. `pytest` is a third-party wheel that does not
   ship in GraalPy, so agents writing full Python extensions and wanting to
   TEST them inline would otherwise hit ModuleNotFoundError; this extension
   contributes a `:ext/sandbox-shims` entry that
   `env-python/build-agent-context` installs into every sandbox Context.

   It is NOT real pytest: there is no pluggy/plugin system, no ini/plugin CLI
   (only `-k` / `-x` / `--maxfail` / `-v`), and no import-time assertion
   rewrite. It DOES do `conftest.py` fixture discovery
   (walked from the test file's dir up to the fs root, outer→inner) in disk
   mode. Instead it
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
       yield-teardown, autouse, recursive injection, parametrized fixtures via
       `params`/`ids` with `request.param`, `request.getfixturevalue` chains),
       `@pytest.mark.parametrize` (incl. `indirect=`) / `skip` / `skipif` / `xfail`
       / `usefixtures` (+ arbitrary marks), `pytest.param`,
       builtin fixtures `request` / `monkeypatch` / `capsys` / `capfd` /
       `tmp_path` / `tmp_path_factory` / `tmpdir` / `tmpdir_factory` /
       `caplog` / `recwarn` / `pytester` / `testdir`, `conftest.py` fixture discovery,
       and a `pytest.main()` runner (with `-k` keyword selection, `-x` /
       `--maxfail` fail-fast, and `-v`) that prints progress + failure reports
       + a summary line (incl. deselected counts) and returns an exit code.

   Unlike `shim-yaml`/`shim-matplotlib` there are NO `:shim/bindings`: the shim
   is a self-contained Python preamble with zero host callables. It publishes a
   `pytest` module into `sys.modules` (so `import pytest` works) and staples it
   onto builtins (so `pytest.raises(...)` works with NO import, like
   json/os/requests)."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pytest"
     :ext/description
     (str "Sandbox pure-stdlib `pytest` subset: collection, assert introspection, fixtures, "
          "parametrize/marks, raises, monkeypatch, capsys, tmp_path, and `pytest.main`. "
          "No pip/wheel/host bridge; no plugins and most CLI options unsupported.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pytest"
       :shim/imports ["pytest"]
       :shim/description
       (str
         "Stdlib `pytest` subset: collection, assert introspection, conftest, parametrize, marks, "
         "`raises`/`warns`/`approx`, the common fixtures, and `pytest.main(args)` over node ids. "
         "Not supported: plugins, most CLI options, import-time assertion rewriting.")
       :shim/docs
       (str "Stdlib pytest subset: collection/assert introspection; conftest, parametrize, "
            "marks, raises/warns/approx; fixtures monkeypatch, capsys/capfd/caplog, recwarn, "
            "tmp_path/tmpdir factories, request, and pytester/testdir. "
            "`pytest.main(args)` discovers test_*.py/*_test.py, selects node ids (`file::name`, "
            "`file::Class::method`, `file::name[id]`), honours `--collect-only`, exits 5 when "
            "nothing ran and 4 on a missing path; no paths runs current-block tests. "
            "pytester supports makepyfile/makeconftest/runpytest and RunResult assertions. "
            "Not supported: plugins, most CLI options, import-time assertion rewriting.")
       :shim/source "vis-shims/pytest.py"}]}))

(vis/register-extension! vis-extension)
