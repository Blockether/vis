# Python sandbox

Vis uses a CPython sandbox to run the agent's tool calls and calculations. It is
separate from your project's Python environment: installing a package in one
does not necessarily make it available in the other. This page explains which
environment to use, how to install packages and what the sandbox can access.

## Running Python

Each session has its own Python state. The sandbox and trusted Python extensions
use separate namespaces.
Tools such as `grep`, `cat`, `patch` and `shell` are available as
Python functions. `apropos` and `doc` inspect the available API synchronously.

## Experiment with extension declarations

Ask Vis to inspect an SDK type or prototype a tool declaration in `python_execution`.
The sandbox includes the bundled `blockether.vis.extension` module; you do not need
to install `vis-agent` to import it. For example:

```python
from __future__ import annotations

import inspect

import blockether.vis.extension as sdk

print(inspect.signature(sdk.ActivityProgress))
progress = sdk.ActivityProgress("Inspect SDK", value=1, total=2)
print(progress.to_wire())


def greet(name: str) -> str:
    """Greet one person."""
    return "Hello, " + name


symbol = sdk.Symbol(greet)
print(symbol.contract)
```

You can inspect public types, validate declarations and call your own Python
functions. This does not install a tool. `sdk.register_extension(...)` and
extension host operations such as `sdk.state`, `sdk.shell(...)` and
`sdk.notify(...)` raise `RuntimeError` explaining that they are unavailable in
`python_execution`. Native filesystem helpers such as `sdk.fs.read(...)` remain
restricted to trusted extensions and raise `PermissionError` in the sandbox.
Importing the SDK does not relax filesystem, process or network restrictions or
initialize the standalone SDK host.

### What the sandbox does not provide

The sandbox carries one SDK module, `blockether.vis.extension`, and nothing else
under `blockether.vis`, so imports that work against an installed `vis-agent`
package fail here:

```python
import blockether.vis.extension as sdk   # the bundled module
from blockether.vis.engine import Agent  # ModuleNotFoundError
```

`blockether.vis.engine` and `blockether.vis.views` belong to the
[Python SDK](python-sdk.md), which drives Vis from your own program. Install
`vis-agent` and run that code in your own interpreter.

Underscore-prefixed names inside the bundled module are internal, and reading
them here describes nothing outside the sandbox: no extension registers in
`python_execution`, so the module's own registration record stays empty.

Executing an extension entry file yourself does not get around that. `importlib`
runs the file, and the file stops at the first host operation it performs while
declaring itself:

```text
RuntimeError: Extension host operation 'declare_env' is unavailable in
python_execution. You can inspect SDK types and construct declarations here;
load an extension to use its host APIs.
```

To register tools and test their host operations, load an extension using the
[extension tutorial](extending.md). Trusted extension code runs in a separate
process with its own host bindings. The bundled SDK takes precedence over pip and
editable copies in both contexts.

## Reading tool results

A tool returns its complete result to Python, but only printed text reaches the
model's next request. The agent can keep a large result in a variable and inspect
just the fields it needs. For example, these calls show progressively more of a
shell result:

```python
r = await shell("ls -la")
print(r["status"])       # running, exited or timed out
print(r.logs(-20))       # the last twenty output lines
print(r["out"])          # everything the process has written
```

Shell results are dictionary-like handles with `wait`, `logs`, `type` and `stop`
methods. Their short view includes status, exit and timeout information. When it
omits text, it names the field or log cursor that holds the rest. `dict(r)` exposes
the full mapping and `json.dumps(r)` serializes every field; both can produce much
more output than the model needs.

`apropos(pattern)` remains a list of `(type, name, body)` records. Printing it
shows one compact row per symbol; attributes, indexing and `doc(row)` still work.

Extension tools answer with records built from their public fields, not with the
extension's own Python objects: a `PageList` result has `r.results` or `r["results"]`
and `r.total`, but none of the original methods. A name that is not a field raises
`KeyError` (`r["methods"]`), whose message lists the fields the record does have, or
`AttributeError` (`r.methods`), which reports only the missing name. Use a listed field
instead of guessing another one. `doc(tool)` shows
the same fields under **Model schemas**. Only records whose extension declares a
[backing field](extension-api.md#field-backed-sequences) iterate; read other records
through their list field.

`read_session()` also returns more data than its printed summary. The full history,
including folded steps, is under `transcript["turns"]`, then `iterations`, then
`blocks`. Each block holds `code`, `stdout` and any `error`. `list_sessions()`
finds other saved sessions.

## What the sandbox may do

These limits come from the process jail, which is off by default. Until you turn it on,
sandbox code reaches the same files and hosts your own account can, and
`session["access"]["is_jailed"]` is `false`. With the jail enabled, a CPython audit hook
checks filesystem, process and network operations, including operations made through
imported libraries.

| Capability | Policy with the jail enabled |
| --- | --- |
| Filesystem IO | confined to allowed workspace roots |
| Spawning a process (`subprocess`, `os.system`, `os.popen`) | refused; use `shell(...)`, which applies the process policy |
| `ctypes` and foreign libraries | refused |
| HTTP clients | routed through the gateway policy and network filters |
| Raw sockets | guarded at the socket level |
| Threads | capped per process; exhaustion raises `RuntimeError` (also without the jail) |
| Wall-clock time | every block has a timeout, lifted while a live view is open (also without the jail) |

Host functions exposed from Clojure apply their own permission checks either way:
`sdk.fs.read(...)` raises `PermissionError` in `python_execution` whether or not the
jail is enabled. See [Process jail and network policy](jail.md) to turn the jail on and
for the complete policy.

## Packages

`python_execution` imports explicitly installed shared packages from
**`~/.vis/python/packages`**. Install them with the CLI below rather than from a block.
Extensions without
a project environment also use that directory, even if they declare a uv project or
have a `pyproject.toml`. Startup and plain `/reload` do not create a missing `.venv`.
Extensions with an existing uv environment use its dependencies in separate trusted
workers, without shared-package fallback. Preparing that environment does not add its
dependencies to `python_execution`. Use `/reload --sync` to create it deliberately.

Imports do not install packages. Use `vis-agent python --shared -m pip install <package>`
for shared sandbox packages and `vis-agent python --shared -m <module>` to run a
shared tool, including from a project directory.

`vis-agent python uv sync --project PATH` is upstream uv: it prepares the project's
environment, normally `.venv`, not shared sandbox packages. From that project,
`vis-agent python -m <module>` uses its environment with Vis's embedded Python,
without shared packages or their startup hooks. A missing or incompatible environment
reports an error; it does not fall back to shared packages. Without a project, the
CLI defaults to shared packages. See [Python import roots](configuration.md#python-import-roots)
for selection rules and explicit source paths. To use the project's own interpreter,
run `vis-agent python uv run --project PATH --no-sync python -m <module>`.

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

Attachments are read-only by default. Pass `commentable=True` when you want people
to review an attachment in Companion or the TUI. This is a property of each version,
not something inferred from its filename. For example, a specification allows
comments, while its implementation report does not:

```python
attach(specification.encode("utf-8"), filename="PLAN-search.md", kind="doc",
       media_type="text/markdown", commentable=True)
attach(report.encode("utf-8"), filename="IMPLEMENTATION-search.md", kind="doc",
       media_type="text/markdown", commentable=False)
```

Here `specification` and `report` are the Markdown strings you produced. Read-only
prevents human comment saves, not a producer's later update under the same filename.
See [draft diffs](drafts.md) for reviewable patches from an isolated working copy.

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

## Diagnosing an unresponsive worker

If Vis has to retire an unresponsive Python worker, it first tries to save local
hang evidence under `~/.vis/logs/YYYY-MM-DD/pyext-*/`, using the worker
start date in UTC. See [Logs and diagnostics](logging.md) for locations and
retention. The warning in the gateway log gives the
path to `hang.edn`, which records the worker PID, active and last completed RPCs,
and capped JVM stacks. When the runtime supports it, `python-stacks.log` contains
Python frames even if native code holds the GIL. The report records whether that
capture succeeded, failed or timed out.

Collection adds at most 500 ms to retirement. A failed diagnostic does not prevent
Vis from stopping the worker. Healthy readiness checks and ordinary session
cleanup do not dump stacks.

Both files are private to your OS account. They omit Python source, arguments,
return values and local variables, but stack frames can contain file paths and
function names. Review and redact them before sharing a bug report; do not upload
them automatically. Python frame locations show the function's definition line
and bytecode offset, not a decoded current source line.

## See also

- [How Vis manages context](token-optimization.md) — batching tool calls and storing results in Python.
- [Process jail and network policy](jail.md) — the policy the sandbox runs under.
- [Configuration](configuration.md#python-import-roots) — making your own modules importable.
