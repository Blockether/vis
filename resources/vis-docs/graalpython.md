# GraalPython sandbox

The agent's actions are **code**, and that code runs in an embedded Python interpreter — GraalPy, running in-process on the same GraalVM that hosts the Vis core. This is the substrate that makes "context as an environment" real: the model emits a program, the sandbox executes it, and only the journal comes back.

## In-process, not a subprocess

GraalPy is a Truffle language on the GraalVM runtime, so the interpreter shares the process with the Clojure core. Tools are exposed to the sandbox as ordinary async functions — `grep`, `cat`, `patch`, `shell`, `run_tests` — while `apropos` and `doc` synchronously inspect the live surface. `grep` and `cat` answer anchored TEXT that `patch` spends directly; the rest answer structured values the model can compose, filter, and summarize in vars, then print only the useful slice.

## Sandboxed by design

The GraalPy Context is **deny-by-default**. Every capability below is off, or narrowed, unless Vis turns it on:

| capability | default |
|---|---|
| Host classes, native access, polyglot access | off |
| Filesystem IO | confined to the workspace roots, or disabled |
| HTTP clients | routed through the gateway egress policy and the programmable network filters |
| Raw sockets | a socket-level host guard is the floor |
| `exec`, `eval`, `compile`, `__import__` | refused before a block runs |
| Wall-clock time | every eval is bounded by a timeout |

GraalPy is in-process, so Seatbelt cannot be applied to its JVM thread alone — the Context *is* the boundary there. See [Process jail and gateway egress](jail.md) for the complete boundary.

## Two Python surfaces, on purpose

Vis runs Python in **two different places**, and they deliberately do not see the same modules. Reaching for the wrong one is the usual source of a confusing `ModuleNotFoundError`, so the split is worth naming:

- **The sandbox surface — `python_execution`.** The in-process GraalPy Context described above. It is **hermetic**: the Python standard library plus Vis's advertised compatibility shims, and **none of the host project's installed packages**. This is the action layer for composing tools, filtering output, and pure-logic compute. There is no `pip install`; inspect the live capabilities with `apropos` / `doc` instead of assuming a module is available.

- **The project surface — `repl_start({"language": "python"})` + `repl_eval({"language": "python", "code": …})`.** Starting the REPL spawns a real project interpreter subprocess selected from `uv` / Poetry / `.venv` / `python3`. On macOS that managed process inherits the same filesystem jail and gateway-proxied network policy as shell children; dependency caches (`~/.cache/uv`, a project `.venv`, …) enter through the shared `workspace.filesystem` catalog, so cache access is explicit. It sees the project's installed dependencies and site-packages but none of the Vis Python shims. `repl_connect` is different: it attaches to a user-owned process that already exists, so Vis cannot retroactively jail it.

The practical rule is one question — does this code import your project's dependencies?

| what you are doing | where it runs |
|---|---|
| stdlib-only compute, tool glue, filtering a result | `python_execution` |
| anything that needs your installed packages | `repl_start({"language": "python"})`, then `repl_eval({"language": "python", "code": …})` |

`run_tests({"language": "python"})` follows the same divide: it defaults to the hermetic GraalPy runner (stdlib plus the bundled pytest shim) and switches to the project interpreter's pytest with `{"runner": "project"}`. A hermetic run that trips over a missing third-party module says so, and points at the project runner.

## Why Python for the action layer

Python is the lingua franca models write most fluently, so the action layer meets the model where it is strongest. The *core* is Clojure (and the languages it edits are whatever tree-sitter supports) — but the glue the model writes each turn is Python, executed by GraalPy.

## Build-time cost, runtime payoff

GraalPy ships substantial native-image configuration (build-time initialization, a heavy heap during the build). Vis does **not** duplicate that config — it inherits it from the GraalPy language jar on the classpath, and adds only its own app-level reflection and flags. See [JVM & native-image](jvm-native-image.md).

## Internal resources cache

GraalPy's Python stdlib ships as **internal resources**, not as loose files on your machine. Where they live depends on how Vis is running:

| runtime | where the stdlib lives |
|---|---|
| JVM | inside the GraalPy language jar, extracted at **runtime** into `$XDG_CACHE_HOME/org.graalvm.polyglot` (default `~/.cache/org.graalvm.polyglot`) |
| Native binary | a `resources/` directory **beside the executable** (`-H:-IncludeLanguageResources -H:+CopyLanguageResources`; `PythonHome` is never used) |
| Release bundle | `vis-agent-resources/`, handed to the launcher as `-Dpolyglot.engine.resourcePath`, so nothing is extracted at runtime |

`VIS_NATIVE_RESOURCES` points a native run at a different copy of that directory.

Only the JVM path writes anything at runtime, so it is the one that can fail:

- **Symptom** — the first stdlib import fails with `ModuleNotFoundError: No module named 'ast'`, the same error a native bundle gives when its resources directory is missing.
- **Cause** — the cache root is unwritable: a confined process, a read-only home.
- **Fix** — Vis redirects to a writable fallback automatically. To choose the root yourself, set `python.resource_cache` in `vis.yml`, or pass the `-Dpolyglot.engine.userResourceCache` system property, which always wins.
- **Scope** — the root is read once per process, so a change needs a restart; `/reload` will not move it.

See [Configuration](configuration.md#graalpy-internal-resource-cache).

## See also

- [Token optimization](token-optimization.md) — what the sandbox buys, measured in context.
- [Process jail & egress](jail.md) — the filesystem and network policy the sandbox runs under.
- [Configuration → Python import roots](configuration.md#python-import-roots) — making your own modules importable in the sandbox.
- [Extending Vis → Batteries in the model's sandbox](extending.md#batteries-in-the-model-s-sandbox) — the shim modules that are already there.
