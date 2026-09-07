# Python sandbox

The model runs Python code in a CPython sandbox. This page describes its
permissions, package installation and differences from your project's Python.

## Interpreter processes

Each gateway session has a worker process with its own CPython interpreter.
The model's sandbox and trusted Python extensions use separate namespaces
inside that worker. A one-shot CLI session can use an interpreter in its
existing process.

The interpreter is provided by `libvispython`, a shared library with a bundled
CPython installation, accessed through the JDK Foreign Function and Memory API.
Tools such as `grep`, `cat`, `patch`, `shell` and `run_tests` are available as
Python functions. `apropos` and `doc` inspect the available API synchronously.

## What the sandbox may do

A CPython audit hook checks filesystem, process and network operations,
including operations made through imported libraries.

| Capability | Policy |
| --- | --- |
| Filesystem IO | confined to allowed workspace roots when the jail is enabled |
| Spawning a process (`subprocess`, `os.system`, `os.popen`) | refused; use `shell(...)`, which applies the process policy |
| `ctypes` and foreign libraries | refused |
| HTTP clients | routed through the gateway policy and network filters |
| Raw sockets | guarded at the socket level |
| Threads | capped per process; exhaustion raises `RuntimeError` |
| Wall-clock time | every block has a timeout, lifted while a live view is open |

Host functions exposed from Clojure apply their own permission checks. See
[Process jail and network policy](jail.md) for the complete policy.

## Packages

If a top-level import is not found on `sys.path`, the host tries to install
a distribution with that name. Automatic installation accepts wheels only and
requires session network access. If installation is refused, the import raises
`ModuleNotFoundError`. Packages are installed in `~/.vis/python/packages`,
which is shared across sessions and read-only to the sandbox.

Outside a session, `vis-agent python -m pip install <package>` installs into
the same location, and `vis-agent python -m <module>` runs it.

Attachment functions are available without an import. `attach(...)` stores a
file and returns its descriptor. Use `list_attachments()`, `get_attachment(...)`,
`read_attachment(...)` and `show_attachment(...)` to retrieve attachments by
filename or id. Saving another file with the same name creates a new version.

## Sandbox versus project Python

The sandbox and project interpreter have separate installed packages:

| Code | Where it runs |
| --- | --- |
| computations, tool calls and result filtering | `python_execution` (the sandbox) |
| anything that imports your project's dependencies | a project interpreter: `repl_start({"language": "python"})`, then `repl_eval({"language": "python", "code": ...})` |

The project interpreter runs as a subprocess selected from `uv`, Poetry, a
`.venv` or `python3`. It uses the same jail and network policy as shell
processes. Allow dependency cache directories through `workspace.filesystem`.
`repl_connect` attaches to an existing process, which Vis cannot jail.

`run_tests({"language": "python", "runner": "project"})` uses the project's pytest.
Use `"runner": "vispython"` for the sandbox runner. The default is configurable
through `python.runner`; see [Configuration](configuration.md#python-import-roots).

## Runtime locations

| Runtime | Location |
| --- | --- |
| JVM | `com.blockether/vis-python-runtime-native-<platform>` on the classpath |
| Native binary and release bundle | `vis-agent-python/` beside the executable |

`VIS_PYTHON_NATIVE_PATH` points a run at another copy of the library and
`VIS_PYTHON_HOME` at another interpreter tree. Neither is needed in a normal
install.

## See also

- [How Vis manages context](token-optimization.md) — batching tool calls and storing results in Python.
- [Process jail and network policy](jail.md) — the policy the sandbox runs under.
- [Configuration](configuration.md#python-import-roots) — making your own modules importable.
