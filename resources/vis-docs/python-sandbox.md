# Python sandbox

The model runs Python code in a CPython sandbox. This page describes its
permissions, package installation and differences from your project's Python.

## Running Python

Each session has its own Python state. The sandbox and trusted Python extensions
use separate namespaces.
Tools such as `grep`, `cat`, `patch`, `shell` and `run_tests` are available as
Python functions. `apropos` and `doc` inspect the available API synchronously.

## Reading tool results

Python receives complete tool data. Only printed text enters the model's next
request. Keep results in variables and print the fields needed for a decision:

```python
r = await run_tests({"language": "python"})
print(r)                 # verdict, counts and bounded diagnostics
print(r["failures"])     # every recorded fault
print(r["output"])       # full returned runner output
```

Shell results remain dictionary-like handles with `wait`, `logs`, `type` and
`stop`. Their short view preserves status, exit and timeout information. If text
is omitted, the view names the field or log cursor to read next. `dict(r)` exposes
the mapping; `json.dumps(r)` serializes every field without the compact view.
Do not routinely print either for large results.

`apropos(pattern)` remains a list of `(type, name, body)` records. Printing it
shows one compact row per symbol; attributes, indexing and `doc(row)` still work.

`read_session()` returns structured history; its printed view is a summary.
Read `transcript["turns"]`, then `iterations`, then `blocks` for `code`, `stdout`
and optional `error`, including folded history. Use `list_sessions()` to find
another session.

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

Both `python_execution` and trusted Python extension workers import packages from
**`~/.vis/python/packages`**. They use the same installed files and versions, but
separate interpreters, module caches and permissions. This directory is shared
across sessions and read-only to sandbox code.

Imports do not install packages. Prepare a locked project explicitly with
`vis-agent python uv sync --project PATH --locked`, or install a wheel with
`vis-agent python -m pip install <package>`. These commands use the same directory;
`vis-agent python -m <module>` runs against it. Missing imports raise
`ModuleNotFoundError`. Sharing installed files does not relax sandbox restrictions:
a package that requires refused native operations may work only in an extension.
After updating installed packages, use `/reload` to rebuild session workers.

A locked uv project can install its own package and local dependencies editably.
Their `.pth` files or backend import hooks resolve imports to the source checkout.
After editing Python source, `/reload` refreshes editable imports and extension
tools without another sync or gateway restart. Dependency or packaging-metadata
changes require another sync. See the
[editable-project guide](extension-development.md).

Editable source is not a frozen snapshot. Already imported modules can retain old
code until reload, and an in-flight call can still use old bindings. Native extension
libraries are not hot-reloaded. Source paths referenced by an editable install must
remain within the sandbox's allowed filesystem roots; installing a package grants
no extra access. Use `print(package.__file__)` after importing your package to check
whether it resolves to the checkout or an installed copy.

Attachment functions are available without an import. `attach(...)` stores a
file and returns its descriptor. Use `list_attachments()`, `get_attachment(...)`,
`read_attachment(...)` and `show_attachment(...)` to retrieve attachments by
filename or id. Saving another file with the same name creates a new version.

## Sandbox versus project Python

An independently selected project interpreter has its own environment, unlike the
two embedded Vis workers that share the package directory above:

| Code | Where it runs |
| --- | --- |
| computations, tool calls and result filtering | `python_execution` (the sandbox) |
| work against the project's own environment | a project interpreter: `repl_start({"language": "python"})`, then `repl_eval({"language": "python", "code": ...})` |

The project interpreter runs as a subprocess selected from `uv`, Poetry, a
`.venv` or `python3`. It uses the same jail and network policy as shell
processes. Allow dependency cache directories through `workspace.filesystem`.
`repl_connect` attaches to an existing process, which Vis cannot jail.

`run_tests("python", {"runner": "project"})` uses the project's pytest.
Use `"runner": "vispython"` for the sandbox runner. The default is configurable
through `python.runner`; see [Configuration](configuration.md#python-import-roots).

## Runtime locations

| Runtime | Location |
| --- | --- |
| JVM | Git-pinned `vis-python-runtime` bridge; platform archive cached under `~/.vis/python/runtime/<version>/<platform>/` |
| Native binary and release bundle | `vis-agent-python/` beside the executable |

`VIS_PYTHON_NATIVE_PATH` points a run at another copy of the library and
`VIS_PYTHON_HOME` at another interpreter tree. Neither is needed in a normal
install.

## See also

- [How Vis manages context](token-optimization.md) — batching tool calls and storing results in Python.
- [Process jail and network policy](jail.md) — the policy the sandbox runs under.
- [Configuration](configuration.md#python-import-roots) — making your own modules importable.
