# Python sandbox

The model's actions are Python code, and that code runs in a CPython interpreter
embedded in the Vis process. This page describes what that interpreter can
reach, how packages get in, and how it differs from your project's own Python.

## In-process interpreter

The interpreter is a shared library (`libvispython`) with a vendored CPython
tree, started once per process and reached through the JDK Foreign Function
and Memory API. A session is a module namespace inside it, not a second
interpreter. Tools such as `grep`, `cat`, `patch`, `shell` and `run_tests`
are ordinary async functions in that namespace; `apropos` and `doc` inspect the
live surface synchronously.

## What the sandbox may do

An audit hook inside CPython enforces the policy, so it cannot be bypassed by
importing around a wrapper:

| Capability | Default |
| --- | --- |
| Filesystem IO | confined to the workspace roots |
| Spawning a process (`subprocess`, `os.system`, `os.popen`) | refused; `shell(...)` is the only door, and it owns the jail |
| `ctypes` and foreign libraries | refused |
| HTTP clients | routed through the gateway policy and network filters |
| Raw sockets | guarded at the socket level |
| Threads | capped per process; exhaustion raises `RuntimeError` |
| Wall-clock time | every block has a timeout, lifted while a live view is open |

Host capabilities exposed from Clojure are named doors with their own checks.
See [Process jail and network policy](jail.md) for the complete boundary.

## Packages

The sandbox has `pip`. A top-level import that nothing on `sys.path` answers
asks the host to install the distribution: by plain name, wheels only, and only
while the session has network access. A refusal is an ordinary
`ModuleNotFoundError`. Installed distributions land in
`~/.vis/python/packages`, shared by every session and read-only to the
sandbox.

Outside a session, `vis-agent python -m pip install <package>` installs into
the same location, and `vis-agent python -m <module>` runs it.

Attachments are reachable without an import: `attach(...)` stores a file and
returns its descriptor; `list_attachments()`, `get_attachment(...)`,
`read_attachment(...)` and `show_attachment(...)` read it back by filename or
id. A file stored again under the same name becomes the next version of that
attachment.

## Sandbox versus project Python

Vis runs Python in two places that deliberately do not share modules:

| Code | Where it runs |
| --- | --- |
| stdlib-only compute, tool glue, filtering results | `python_execution` (the sandbox) |
| anything that imports your project's dependencies | a project interpreter: `repl_start({"language": "python"})`, then `repl_eval({"language": "python", "code": ...})` |

The project interpreter is a real subprocess selected from `uv`, Poetry, a
`.venv` or `python3`. It inherits the same jail and network policy as shell
children; dependency caches enter through the `workspace.filesystem` catalog.
`repl_connect` attaches to a process you started yourself, which Vis cannot
jail.

`run_tests({"language": "python"})` defaults to the sandbox runner and switches
to the project interpreter's pytest with `{"runner": "project"}`. A sandbox run
that fails on a missing project module says so and points at the project
runner.

## Where the interpreter lives

| Runtime | Location |
| --- | --- |
| JVM | `com.blockether/vis-python-runtime-native-<platform>` on the classpath |
| Native binary and release bundle | `vis-agent-python/` beside the executable |

`VIS_PYTHON_NATIVE_PATH` points a run at another copy of the library and
`VIS_PYTHON_HOME` at another interpreter tree. Neither is needed in a normal
install.

## See also

- [How Vis manages context](token-optimization.md) — why the action layer is a program.
- [Process jail and network policy](jail.md) — the policy the sandbox runs under.
- [Configuration](configuration.md#python-import-roots) — making your own modules importable.
- [Clojure extensions](clojure-extensions.md#sandbox-shims) — publishing a host-backed module into the sandbox.
