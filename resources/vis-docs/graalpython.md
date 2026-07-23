# GraalPython sandbox

The agent's actions are **code**, and that code runs in an embedded Python interpreter — GraalPy, running in-process on the same GraalVM that hosts the Vis core. This is the substrate that makes "context as an environment" real: the model emits a program, the sandbox executes it, and only the journal comes back.

## In-process, not a subprocess

GraalPy is a Truffle language on the GraalVM runtime, so the interpreter shares the process with the Clojure core. Tools are exposed to the sandbox as ordinary async functions — `cat`, `struct_index`, `struct_patch`, `find_files` — while `apropos` and `doc` synchronously inspect the live surface. The model can compose, filter, and summarize many results in vars, then print only the useful slice into context.

## Sandboxed by design

The Context is **deny-by-default**: no host-class access, native access off, polyglot access off, and filesystem IO either confined to workspace roots or disabled. HTTP clients are routed through the gateway egress policy and programmable network filters; a socket-level host guard remains the floor for raw sockets. GraalPy is in-process, so Seatbelt cannot be applied to only its JVM thread. Dangerous builtins (`exec`, `eval`, `compile`, `__import__`) are refused before a block runs, and each eval is bounded by a wall-clock timeout. See [Process sandbox and gateway egress](sandbox.md) for the complete boundary.

## Two Python surfaces, on purpose

Vis runs Python in **two different places**, and they deliberately do not see the same modules. Reaching for the wrong one is the usual source of a confusing `ModuleNotFoundError`, so the split is worth naming:

- **The sandbox surface — `python_execution`.** The in-process GraalPy Context described above. It is **hermetic**: the Python standard library plus Vis's advertised compatibility shims, and **none of the host project's installed packages**. This is the action layer for composing tools, filtering output, and pure-logic compute. There is no `pip install`; inspect the live capabilities with `apropos` / `doc` instead of assuming a module is available.

- **The project surface — `repl_start("python")` + `repl_eval("python")`.** `repl_start` spawns a real project interpreter subprocess selected from `uv` / Poetry / `.venv` / `python3`. On macOS that managed process inherits the same filesystem jail and gateway-proxied network policy as shell children; dependency-cache access is explicit under `jail.filesystem.language-caches`. It sees the project's installed dependencies and site-packages but none of the Vis Python shims. `repl_connect` is different: it attaches to a user-owned process that already exists, so Vis cannot retroactively jail it.

The practical rule: **stdlib-only compute and tool glue → `python_execution`; anything that needs your installed packages → check the session resource, start with `repl_start("python")` when needed, then use `repl_eval("python")`.** The same divide governs `run_tests("python")` — it defaults to the hermetic GraalPy runner (stdlib + the bundled pytest shim) and switches to the real interpreter's pytest with `{"runner": "project"}` when your tests import third-party packages. When a hermetic run trips over a missing third-party module, the result says so and points you at the project runner.

## Why Python for the action layer

Python is the lingua franca models write most fluently, so the action layer meets the model where it is strongest. The *core* is Clojure (and the languages it edits are whatever tree-sitter supports) — but the glue the model writes each turn is Python, executed by GraalPy.

## Build-time cost, runtime payoff

GraalPy ships substantial native-image configuration (build-time initialization, a heavy heap during the build). Vis does **not** duplicate that config — it inherits it from the GraalPy language jar on the classpath, and adds only its own app-level reflection and flags. See [JVM & native-image](jvm-native-image.md).
