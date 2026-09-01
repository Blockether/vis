# Python sandbox

The agent's actions are **code**, and that code runs in an embedded Python interpreter — a real CPython, linked into the Vis process through the JDK Foreign Function & Memory API. This is the substrate that makes "context as an environment" real: the model emits a program, the sandbox executes it, and only the journal comes back.

## In-process, not a subprocess

The interpreter is a cdylib (`libvispython`) plus a vendored CPython tree, started once per process and driven by downcalls; a session is a module namespace inside it, not a second interpreter. Tools are exposed to the sandbox as ordinary async functions — `grep`, `cat`, `patch`, `shell`, `run_tests` — while `apropos` and `doc` synchronously inspect the live surface. `grep` and `cat` answer anchored TEXT that `patch` spends directly; the rest answer structured values the model can compose, filter, and summarize in vars, then print only the useful slice.

## Sandboxed by design

The interpreter is **confined by the host, deny-by-default**: an audit hook inside CPython refuses the operation itself, so the policy is not a wrapper anyone can import around.

| capability | default |
|---|---|
| Filesystem IO | confined to the workspace roots — every other path is refused |
| Spawning a process (`subprocess`, `os.system`, `os.popen`) | refused; the host's `shell` is the only door, and it owns the jail |
| `ctypes`, loading a foreign library | refused |
| HTTP clients | routed through the gateway egress policy and the programmable network filters |
| Raw sockets | a socket-level host guard is the floor |
| Threads | capped per process; exhaustion is a loud `RuntimeError`, never a hang |
| Wall-clock time | every eval is bounded by a timeout |

The audit hook is process-wide, so a capability the guest must not have is never present in the sandbox process — including for Python that an extension installed. What the host exposes from Clojure is a **named door with its own check**, not a hole in the policy. See [Process jail and gateway egress](jail.md) for the complete boundary.

## Packages: the real wheel, fetched by the host

The sandbox is not hermetic any more. A first `import numpy` that nothing on `sys.path` can answer reaches a finder that goes LAST on `sys.meta_path` and asks the HOST to install the distribution — because the guest may neither spawn a process nor route its own egress. That door has its own policy: a plain distribution name only, only when the session has network at all, and only ever a WHEEL (`--only-binary=:all:`, since an sdist would run its own `setup.py` outside every boundary). A refusal is the ordinary `ModuleNotFoundError`.

Installed distributions land in `~/.vis/python/packages`, shared by every session, and the confinement makes that directory readable, never writable, for the guest.

## Two Python surfaces, on purpose

Vis runs Python in **two different places**, and they deliberately do not see the same modules:

- **The sandbox surface — `python_execution`.** The in-process interpreter described above: the standard library, whatever the host has installed into `~/.vis/python/packages`, and **none of the host project's own environment**. This is the action layer for composing tools, filtering output, and pure-logic compute.

- **The project surface — `repl_start({"language": "python"})` + `repl_eval({"language": "python", "code": …})`.** Starting the REPL spawns a real project interpreter subprocess selected from `uv` / Poetry / `.venv` / `python3`. On macOS that managed process inherits the same filesystem jail and gateway-proxied network policy as shell children; dependency caches (`~/.cache/uv`, a project `.venv`, …) enter through the shared `workspace.filesystem` catalog, so cache access is explicit. It sees the project's installed dependencies and site-packages. `repl_connect` is different: it attaches to a user-owned process that already exists, so Vis cannot retroactively jail it.

The practical rule is one question — does this code import your project's dependencies?

| what you are doing | where it runs |
|---|---|
| stdlib-only compute, tool glue, filtering a result | `python_execution` |
| anything that needs your installed packages | `repl_start({"language": "python"})`, then `repl_eval({"language": "python", "code": …})` |

`run_tests({"language": "python"})` follows the same divide: it defaults to the sandbox runner (the embedded interpreter's own pytest) and switches to the project interpreter's pytest with `{"runner": "project"}`. A sandbox run that trips over a missing project module says so, and points at the project runner.

## Why Python for the action layer

Python is the lingua franca models write most fluently, so the action layer meets the model where it is strongest. The *core* is Clojure (and the languages it edits are whatever tree-sitter supports) — but the glue the model writes each turn is Python.

## Where the interpreter lives

| runtime | where the interpreter lives |
|---|---|
| JVM | inside `com.blockether/vis-python-runtime-native-<platform>` on the classpath |
| Native binary | a `vis-agent-python/` directory **beside the executable**, staged by the build |
| Release bundle | the same `vis-agent-python/`, unpacked together with the wrapper and the runtime |

`VIS_PYTHON_NATIVE_PATH` points a run at a different copy of the cdylib, and `VIS_PYTHON_HOME` at a different interpreter tree; neither is needed in a normal install. See [JVM & native-image](jvm-native-image.md).

## See also

- [Token optimization](token-optimization.md) — what the sandbox buys, measured in context.
- [Process jail & egress](jail.md) — the filesystem and network policy the sandbox runs under.
- [Configuration → Python import roots](configuration.md#python-import-roots) — making your own modules importable in the sandbox.
- [Extending Vis](extending.md) — adding your own doors from Clojure.
