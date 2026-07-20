# GraalPython sandbox

The agent's actions are **code**, and that code runs in an embedded Python interpreter — GraalPy, running in-process on the same GraalVM that hosts the Vis core. This is the substrate that makes "context as an environment" real: the model emits a program, the sandbox executes it, and only the journal comes back.

## In-process, not a subprocess

GraalPy is a Truffle language on the GraalVM runtime, so the interpreter shares the process with the Clojure core. Tools are exposed to the sandbox as ordinary async functions — `cat`, `index`, `struct_patch`, `rg`, `find_files` — so the model composes them in code: filter, map, summarize, pipe one tool's output into another, all without any of it touching the prompt.

## Sandboxed by design

The Context is **deny-by-default**: no host-class access, native access off, polyglot access off, and filesystem IO either confined to the workspace roots or disabled entirely — with network off by default (opt-in, and even then steered by an allow/deny domain policy). Dangerous builtins (`exec`, `eval`, `compile`, `__import__`) are refused before a block runs, and each eval is bounded by a wall-clock timeout. Guest threads are allowed but GIL-like and still can't reach IO, native, or host, so the model can compute freely while the blast radius stays bounded — and because results are bound to vars, a large computation can be reduced to a small summary before anything is shown to the model.

## Two Python surfaces, on purpose

Vis runs Python in **two different places**, and they deliberately do not see the same modules. Reaching for the wrong one is the usual source of a confusing `ModuleNotFoundError`, so the split is worth naming:

- **The sandbox surface — `python_execution`.** The in-process GraalPy Context described above. It is **hermetic**: the Python standard library plus the shims Vis bundles (a stdlib-only `pytest`, `paramiko`, `tomllib`, and friends), and **none of your project's installed packages**. This is the action layer — where the model composes tools, filters output, and runs pure-logic compute. There is no `pip install` here; if a block imports `requests` or `numpy` it fails, by design, because the sandbox has no site-packages and no network.

- **The project surface — `repl_start("python")` + `repl_eval("python")`.** `repl_start` spawns a **real interpreter subprocess** — your project's `uv` / `poetry` / `.venv` / `python3`, chosen the way you'd run it yourself — and `repl_eval` evaluates in that already-running process. It sees the project's actual installed dependencies and site-packages. It is *not* the sandbox: none of the Vis shims are present, IO and network are whatever that interpreter normally has, and it is meant for exercising real project code against real deps.

The practical rule: **stdlib-only compute and tool glue → `python_execution`; anything that needs your installed packages → check the session resource, start with `repl_start("python")` when needed, then use `repl_eval("python")`.** The same divide governs `run_tests("python")` — it defaults to the hermetic GraalPy runner (stdlib + the bundled pytest shim) and switches to the real interpreter's pytest with `{"runner": "project"}` when your tests import third-party packages. When a hermetic run trips over a missing third-party module, the result says so and points you at the project runner.

## Why Python for the action layer

Python is the lingua franca models write most fluently, so the action layer meets the model where it is strongest. The *core* is Clojure (and the languages it edits are whatever tree-sitter supports) — but the glue the model writes each turn is Python, executed by GraalPy.

## Build-time cost, runtime payoff

GraalPy ships substantial native-image configuration (build-time initialization, a heavy heap during the build). Vis does **not** duplicate that config — it inherits it from the GraalPy language jar on the classpath, and adds only its own app-level reflection and flags. See [JVM & native-image](jvm-native-image.md).
