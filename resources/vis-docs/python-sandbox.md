# Python sandbox

Vis uses a CPython sandbox to run the agent's tool calls and calculations. It is
separate from your project's Python environment: installing a package in one
does not necessarily make it available in the other. This page explains which
environment to use, how to install packages and what the sandbox can access.

## Running Python

Each session has its own Python state. The sandbox and trusted Python extensions
use separate namespaces.
Tools such as `grep`, `cat`, `patch`, `shell` and `run_tests` are available as
Python functions. `apropos` and `doc` inspect the available API synchronously.

## Reading tool results

A tool returns its complete result to Python, but only printed text reaches the
model's next request. The agent can keep a large result in a variable and inspect
just the fields it needs. For example, these calls show progressively more of a
test result:

```python
r = await run_tests({"language": "python"})
print(r)                 # verdict, counts and bounded diagnostics
print(r["failures"])     # every recorded fault
print(r["output"])       # full returned runner output
```

Shell results are dictionary-like handles with `wait`, `logs`, `type` and `stop`
methods. Their short view includes status, exit and timeout information. When it
omits text, it names the field or log cursor that holds the rest. `dict(r)` exposes
the full mapping and `json.dumps(r)` serializes every field; both can produce much
more output than the model needs.

`apropos(pattern)` remains a list of `(type, name, body)` records. Printing it
shows one compact row per symbol; attributes, indexing and `doc(row)` still work.

`read_session()` also returns more data than its printed summary. The full history,
including folded steps, is under `transcript["turns"]`, then `iterations`, then
`blocks`. Each block holds `code`, `stdout` and any `error`. `list_sessions()`
finds other saved sessions.

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

`python_execution` imports explicitly installed shared packages from
**`~/.vis/python/packages`**, which is read-only to sandbox code. Extensions without
a uv project also use that directory. Project extensions instead use dependencies
from their uv environment in separate trusted workers.

Imports do not install packages. Use `vis-agent python -m pip install <package>` for
shared sandbox packages; `vis-agent python -m <module>` runs against that directory.
`vis-agent python uv sync --project PATH` is upstream uv: it prepares the project's
environment, normally `.venv`, not shared sandbox packages. Use `vis-agent python uv
run --project PATH python -m <module>` to run against that project environment.

Missing imports raise `ModuleNotFoundError`. Sharing installed files does not relax
sandbox restrictions: a package requiring refused native operations may work only
in an extension. After updating shared packages, use `/reload` to rebuild session workers.

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

Use a project interpreter when code needs your project's dependencies. It has
its own environment, separate from the agent's sandbox:

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

### Select the package directory in a monorepo

`cwd` selects the Python project for both REPLs and the project test runner.
It defaults to the workspace root, **not the parent of a selected test file**.
Pytest can discover a nested `pyproject.toml` after launch; that does not change
which interpreter Vis already launched. A `.venv` in a nested package is not
selected when `cwd` still names the monorepo root.

For example, in the Vis checkout:

```python
print(await run_tests({
    "language": "python", "runner": "project", "cwd": "packages/vis-agent",
    "path": "tests/test_contracts.py",
}))
print(await repl_start({"language": "python", "cwd": "packages/vis-agent"}))
print(await repl_eval({
    "language": "python", "cwd": "packages/vis-agent",
    "code": "import sys, jsonschema; print(sys.executable, sys.prefix, jsonschema.__file__)",
}))
```

Check the returned `command`/`cmd` and `cwd` before changing dependencies. A
missing `jsonschema` under system Python does not mean it is missing from the
package's `.venv`. If absent there too, prepare that project's declared dependencies;
do not merge unrelated environments by appending shared packages to `PYTHONPATH`.

`repl_start(..., env={...})` supplies variables when creating that process;
`repl_eval` cannot change its startup environment. An existing REPL is reused,
not rebuilt after installing dependencies or changing configuration. Stop it by
its returned id and start it again when its startup environment must change.
The project test runner launches a separate process; it does not inherit a REPL's
per-start environment overrides. Temporary verification REPLs should be stopped
when the checks are finished.

## Runtime locations

| Runtime | Location |
| --- | --- |
| JVM | Git-pinned `vis-python-runtime` bridge; platform archive cached under `~/.vis/python/runtime/<version>/<platform>/` |
| Native binary and release bundle | `vis-agent-python/` beside the executable |

`VIS_PYTHON_NATIVE_PATH` points a run at another copy of the library and
`VIS_PYTHON_HOME` at another interpreter tree. Neither is needed in a normal
install.

## TLS compatibility

`python.tls_strict` defaults to `true`. An explicit `false` clears only strict
X.509 validation, for both sandbox execution and trusted Python extensions;
certificate trust and hostname checks remain enabled. Configure it in user or
project YAML. See [TLS validation](configuration.md#python-tls-validation) for
security trade-offs, scope and worker reload requirements.

## See also

- [How Vis manages context](token-optimization.md) — batching tool calls and storing results in Python.
- [Process jail and network policy](jail.md) — the policy the sandbox runs under.
- [Configuration](configuration.md#python-import-roots) — making your own modules importable.
