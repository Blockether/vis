# Python sandbox

Vis uses a CPython sandbox to run the agent's tool calls and calculations, and to
start its shell commands. It is separate from your project's Python environment:
installing a package in one does not necessarily make it available in the other.
This page explains which environment to use, how to install packages with `pip`
and what the sandbox can access.

## When to use

- **The agent cannot import a package that your project already has.** The sandbox
  has its own packages. [Install the package there](#packages).
- **You want to try an extension declaration before you save a file.** [Experiment
  in the sandbox](#experiment-with-extension-declarations).
- **You need to know what the agent's code can read, write or connect to.** See
  [What the sandbox may do](#what-the-sandbox-may-do).
- **Python HTTPS requests fail a strict certificate check.** See [TLS
  compatibility](#tls-compatibility).
- **The Python worker stops responding.** Look for the [hang
  evidence](#diagnosing-an-unresponsive-worker) that Vis tries to save.
- **A helper or variable from an earlier turn is gone.** See [Sandbox state after a
  restart](#sandbox-state-after-a-restart).

To limit what the agent's commands can reach, turn on the [process jail](jail.md).

## Running Python

Each session has its own Python state. The sandbox and trusted Python extensions
use separate namespaces.
Tools such as `grep`, `cat`, `patch` and `shell` are available as
Python functions. `apropos` and `doc` inspect the available API synchronously.

### Unbalanced quotes and brackets

Sometimes the agent writes a block that Python cannot parse because a quote or bracket does not
balance. For example:

- A string never closes.
- A quote inside a string ends it too early.
- A bracket is closed by the wrong character or not closed at all.

The Python language extension can repair these errors before a block runs.
It can also repair invalid escapes and literal braces in f-strings. Its repair engine and syntax
checks run locally in Python. The repaired source must pass parsing and compilation before it runs.
The output identifies each correction. Vis also records the source that actually ran.

Without that extension, Vis runs the supplied source unchanged.

If a block still does not parse, it does not run. The error shows Python's own message and the
first wrong quote, bracket or escape. It also shows the line with that problem. This check belongs
to `python_execution`, so it works with or without the extension.

### File edits and formatting

Language extensions check proposed `patch` edits before Vis writes the file. They can propose a
repair, validate it, then let Vis write the final source once. The patch result shows corrections,
the actual diff and fresh anchors. If validation fails, the original file stays unchanged.

Plain writes inside Python blocks have a different boundary. The extensions inspect changed files
after the block, including a block that raises an exception. They can then repair invalid files and
report corrections in the next context. These checks do not intercept each write or roll back a block.

The Clojure repair engine runs in Python, but full validation still needs the Clojure reader on a
JVM. A repair is not accepted without that validation. Clojure and Python formatters change layout,
not program structure.

## Sandbox state after a restart

The sandbox keeps your helpers and variables while you work. You can use a result
from an earlier turn without computing it again.

The sandbox process stops only in these cases:

- The session was idle for 5 minutes. Set `VIS_ENV_IDLE_TTL_MS` on the gateway to
  change this time.
- You changed settings that need a new sandbox, for example with `/reload`.
- The gateway freed memory for other sessions. See [Resource
  limits](gateway-service.md#resource-limits).
- The gateway restarted.

The number of turns does not stop the sandbox. A session that stays active keeps
the same process.

After each block, Vis saves a snapshot of the session:

- The source of each helper function and class, and each `import`.
- Each variable that Python can pickle. One value can use up to 1 MiB, and all
  values together up to 4 MiB.

The next sandbox restores this snapshot before it runs your next block. The
output of that block starts with a `[Sandbox restarted]` notice. The notice names
the restored helpers and variables. It also names each value that did not come
back, with the reason.

These values never survive a restart. Create them again when you need them:

- Open files, sockets and other handles.
- Generators and running processes.
- Values that are larger than the limits.

`defs()` lists your helpers and variables. A variable row shows the type and size
of the value, and tells you if Vis saved it. `defs()` never shows values. To remove
a helper or variable from the snapshot, delete it with `del name`. This also frees
its memory.

## Experiment with extension declarations

Ask Vis to inspect an SDK type or prototype a tool declaration in `python_execution`.
The sandbox includes the bundled `blockether.vis.extension` module. You do not need
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

Names that start with an underscore in the bundled module are internal. Reading them here tells you
nothing about the world outside the sandbox. No extension registers in `python_execution`, so the
registration record of the module stays empty.

Running an extension entry file yourself does not change that. `importlib` runs the file, and the
file stops at its first host operation while it declares itself:

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

A tool returns its complete result to Python, but only printed text reaches the next model request.
The agent can keep a large result in a variable and inspect only the fields it needs. For example,
each of these calls shows more of a shell result:

```python
r = await shell("ls -la")
print(r["status"])       # running, exited or timed out
print(r.logs(-20))       # the last twenty output lines
print(r["out"])          # everything the process has written
```

Shell results are dictionary-like handles with `wait`, `logs`, `type` and `stop`
methods. Their short view includes status, exit and timeout information. When it
omits text, it names the field or log cursor that holds the rest. `dict(r)` exposes
the full mapping and `json.dumps(r)` serializes every field. Both can produce much
more output than the model needs.

You can read a result map either way: `r["status"]` and `r.status` return the same
field, and that holds at any depth, so `r["transcript"]["turns"]` is
`r.transcript.turns`. Dictionary names keep their dictionary meaning, so `r.items` is
the mapping method and the field of that name stays `r["items"]`. A name that is not
a field raises `KeyError` or `AttributeError` listing the fields the result does
carry. `session` reads the same way.

`apropos(pattern)` remains a list of `(type, name, body)` records. Printing it
shows one compact row per symbol. Attributes, indexing and `doc(row)` still work.

Extension tools answer with records built from their public fields, not with the extension's own
Python objects. For example, a `PageList` result has `r.results` or `r["results"]` and `r.total`,
but none of the original methods.

A name that is not a field raises an error:

- `r["methods"]` raises `KeyError`. Its message lists the fields that the record has.
- `r.methods` raises `AttributeError`. It reports only the missing name.

Use a listed field. Do not guess another one. `doc(tool)` shows the same fields under **Model
schemas**. Only records whose extension declares a
[backing field](extension-api.md#field-backed-sequences) iterate. Read other records through their
list field.

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
| Spawning a process (`subprocess`, `os.system`, `os.popen`) | refused (use `shell(...)`, which applies the process policy) |
| `ctypes` and foreign libraries | refused |
| HTTP clients | routed through the gateway policy and network filters |
| Raw sockets | guarded at the socket level |
| Threads | capped per process, and exhaustion raises `RuntimeError` (also without the jail) |
| Wall-clock time | every block has a timeout, lifted while a live view is open (also without the jail) |

Host functions exposed from Clojure always apply their own permission checks. For example,
`sdk.fs.read(...)` raises `PermissionError` in `python_execution`, with or without the jail. To turn
the jail on and to read the complete policy, see [Process jail and network policy](jail.md).

## Packages

`python_execution` imports explicitly installed shared packages from
**`~/.vis/python/packages`**. Install them with the CLI below rather than from a block.
Extensions without a declared uv project also use that directory, and every project
on the gateway shares it. Vis prepares the uv environment of an extension package or
declared uv project when it loads that extension. Such extensions run in separate
trusted workers with their own dependencies and no shared-package fallback. Preparing
that environment does not add its dependencies to `python_execution`.

Imports do not install packages. Use `vis-agent python --shared -m pip install <package>`
for shared sandbox packages and `vis-agent python --shared -m <module>` to run a
shared tool, including from a project directory.

From a terminal, `vis-agent python` runs programs the way the `python` command does.
`-c CODE`, `-m MODULE`, a script path or `-` for standard input runs as `__main__`, with
the remaining arguments in `sys.argv`. An uncaught error prints its traceback to stderr,
and the command exits with your program's status. Unlike `python_execution`, it has no
tool functions or automatic imports. `vis-agent python --version` prints the Python
version. Other interpreter options, such as `-u` or `-X`, are not supported.

`vis-agent python uv sync --project PATH` is upstream uv. It prepares the environment of the
project, normally `.venv`, not the shared sandbox packages. From that project,
`vis-agent python -m <module>` uses that environment with the embedded Python of Vis. It does not
use shared packages or their startup hooks. A missing or incompatible environment reports an error
and does not fall back to shared packages.

Without a project, the CLI uses shared packages. For selection rules and explicit source paths, see
[Python import roots](configuration.md#python-import-roots). To use the project's own interpreter,
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

Editable source is not a frozen snapshot. Modules that are already imported can keep old code until
a reload, and a running call can still use old bindings. Native extension libraries do not
hot-reload. Source paths of an editable install must stay inside the allowed filesystem roots of the
sandbox. Installing a package gives no extra access. To check whether your package resolves to the
checkout or an installed copy, import it and run `print(package.__file__)`.

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
| JVM | Git-pinned `vis-python-runtime` bridge, with the platform archive cached under `~/.vis/python/runtime/<version>/<platform>/` |
| Native binary and release bundle | `vis-agent-python/` beside the executable |

`VIS_PYTHON_NATIVE_PATH` points a run at another copy of the library and
`VIS_PYTHON_HOME` at another interpreter tree. Neither is needed in a normal
install.

## TLS compatibility

`python.tls_strict` defaults to `true`. An explicit `false` clears only strict
X.509 validation, for both sandbox execution and trusted Python extensions.
Certificate trust and hostname checks remain enabled. Configure it in user or
project YAML. See [TLS validation](configuration.md#python-tls-validation) for
security trade-offs, scope and worker reload requirements.

## Diagnosing an unresponsive worker

If Vis must stop a Python worker that does not respond, it first tries to save local hang evidence.
It saves the evidence under `~/.vis/logs/YYYY-MM-DD/pyext-*/`, with the start date of the worker in
UTC. For locations and retention, see [Logs and diagnostics](logging.md).

The warning in the gateway log gives the path to `hang.edn`. This file records the worker PID, the
active and last completed RPCs, and capped JVM stacks. When the runtime supports it,
`python-stacks.log` contains Python frames, even if native code holds the GIL. The report records
whether that capture succeeded, failed or timed out.

Collection adds at most 500 ms to retirement. A failed diagnostic does not prevent
Vis from stopping the worker. Healthy readiness checks and ordinary session
cleanup do not dump stacks.

Both files are private to your OS account. They omit Python source, arguments,
return values and local variables, but stack frames can contain file paths and
function names. Review and redact them before sharing a bug report. Do not upload
them automatically. Python frame locations show the function's definition line
and bytecode offset, not a decoded current source line.

## See also

- [How Vis manages context](token-optimization.md) — batching tool calls and storing results in Python.
- [Process jail and network policy](jail.md) — the policy the sandbox runs under.
- [Configuration](configuration.md#python-import-roots) — making your own modules importable.
