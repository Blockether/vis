# Python sandbox

Vis runs the tool calls and calculations of the agent in a CPython sandbox, and starts its shell
commands from there. The sandbox is separate from the Python environment of your project, so a
package in one is not always available in the other. This page explains which environment to use,
how to install packages with `pip`, and what the sandbox can access.

## When to use

- **The agent cannot import a package that your project has.**
  [Install the package in the sandbox](#packages).
- **You want to try an extension declaration before you save a file.**
  [Experiment in the sandbox](#experiment-with-extension-declarations).
- **You need to know what the code of the agent can read, write, or connect to.** Read
  [What the sandbox may do](#what-the-sandbox-may-do).
- **Python HTTPS requests fail a strict certificate check.** Read
  [TLS compatibility](#tls-compatibility).
- **The Python worker does not respond.** Find the
  [hang evidence](#diagnosing-an-unresponsive-worker).
- **A helper or a variable from an earlier turn is gone.** Read
  [Sandbox state after a restart](#sandbox-state-after-a-restart).
- **The agent must give you a file or a report.** Read [Attachments](#attachments).

To limit what the commands of the agent can reach, turn on the [process jail](jail.md).

## Running Python

Each session has its own Python state. The sandbox and the trusted Python extensions use separate
namespaces. Tools such as `grep`, `ls`, and `shell` are Python functions. `apropos` and `doc` show
the available functions.

### Unbalanced quotes and brackets

The agent can write a block that Python cannot parse, because a quote or a bracket does not balance.
Literal braces in an f-string, and a backslash escape that Python rejects, stop a block in the same
way. Vis repairs such a block and runs the repaired code only when Python can parse it. The output
then starts with a note of the Python error and each fix, and Vis marks the block as repaired. The
agent reads the same note.

If the repair cannot make the block parse, the block does not run. The error shows the message of
Python, the first wrong quote, bracket, or escape, and the line that holds it.

### File edits and formatting

After a block, also one that raises an exception, the language extensions inspect the changed
files. They can repair invalid files and
report the corrections in the next context. These checks do not intercept a write and do not roll
back a block.

The Clojure extension checks and repairs files in Python, without a JVM. The Clojure and Python
formatters change layout, not program structure.

## Sandbox state after a restart

The sandbox keeps your helpers and variables while you work, for any number of turns. The sandbox
process stops only in these cases:

- The session was idle for 15 minutes. To change this time, set `VIS_ENV_IDLE_TTL_MS` on the
  gateway.
- You changed settings that need a new sandbox, for example with `/reload`.
- The gateway released memory for other sessions. Read
  [Resource limits](gateway-service.md#resource-limits).
- The gateway restarted.

After each block, Vis saves a snapshot of the session:

- The source of each helper function and class, and each `import`.
- Each variable that Python can pickle. One value can use up to 1 MiB, and all values together up
  to 4 MiB.
- Each `bytes` or `str` value above these limits, up to 32 MiB. All these values together can use
  up to 128 MiB. Vis keeps them in separate files next to the snapshot, and deletes the files when
  you delete the session.

The next sandbox restores the snapshot before it runs your next block. The output of that block
shows no sign of the restart. If a block uses a name that did not come back, the error gives the
reason.

These values are never in the snapshot, so create them again:

- Open files, sockets, and other handles.
- Generators and running processes.
- Other values above the limits.

`defs()` lists your helpers and variables. A variable row shows the type and the size of the
value, and tells you if Vis saved it. `defs()` never shows values. To remove a helper or a
variable from the snapshot and release its memory, run `del name`.

## Experiment with extension declarations

Ask Vis to inspect an SDK type or to prototype a tool declaration in `py3`. The
sandbox includes the bundled `blockether.vis.extension` module, so the import needs no installed
`vis-agent`:

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

In the sandbox, you can inspect public types, validate declarations, and call your own Python
functions. You cannot install a tool:

- `sdk.register_extension(...)` and host operations such as `sdk.state`, `sdk.shell(...)`, and
  `sdk.notify(...)` raise `RuntimeError`.
- Native filesystem helpers such as `sdk.fs.read(...)` are for trusted extensions only. They raise
  `PermissionError` in the sandbox.
- An import of the SDK does not relax the filesystem, process, or network restrictions.

### What the sandbox does not provide

The sandbox has one module under `blockether.vis`, which is `blockether.vis.extension`. An import
that works against an installed `vis-agent` package fails here:

```python
import blockether.vis.extension as sdk   # the bundled module
from blockether.vis.engine import Agent  # ModuleNotFoundError
```

`blockether.vis.engine` and `blockether.vis.views` belong to the [Python SDK](python-sdk.md),
which drives Vis from your own program. Install `vis-agent` and run that code in your own
interpreter.

A name in the bundled module that starts with `_` is internal. No extension registers in
`py3`, also when you run an extension entry file yourself with `importlib`. The file
stops at its first host operation while it declares itself:

```text
RuntimeError: Extension host operation 'declare_env' is unavailable in
py3. You can inspect SDK types and construct declarations here;
load an extension to use its host APIs.
```

To register tools and test their host operations, load an extension as the
[extension tutorial](extending.md) shows. Trusted extension code runs in a separate process with
its own host bindings. In both places, the bundled SDK wins over pip copies and editable copies.

## Reading tool results

A tool returns its complete result to Python, but only printed text reaches the next model
request. The agent keeps a large result in a variable and prints only the fields that it needs.
Each of these calls shows more of a shell result:

```python
r = await shell("ls -la")
print(r["status"])       # running, exited or timed out
print(r.logs(-20))       # the last twenty output lines
print(r["out"])          # everything the process has written
```

A shell result is a handle that reads as a dictionary and has the `wait`, `logs`, `type`, and
`stop` methods. Its short view has the status, exit, and timeout information. When the view omits
text, it names the field or the log cursor that holds the rest. `dict(r)` gives the full mapping,
and `json.dumps(r)` serializes every field. Both can print much more than the model needs.

Read a result map by key or by attribute. `r["status"]` and `r.status` return the same field, at
each depth, so `r["transcript"]["turns"]` is `r.transcript.turns`. `session` reads the same way.

`apropos(pattern)` is still a list of `(type, name, body)` records. Its print shows one compact
row for each symbol. Attributes, indexes, and `doc(row)` still work.

An extension tool answers with a record of its public fields, not with the Python object of the
extension. For example, a `PageList` result has `r.results` or `r["results"]`, and `r.total`, but
none of the original methods.

A record is a read-only dictionary of its fields:

- `r.get("total")`, `r.keys()`, `r.items()`, and `r.values()` read the fields.
- `json.dumps(r)` writes the fields, also for a record in a record and with your own `default=`.
- `dict(r)`, `r | other`, and `r.copy()` give a plain dictionary that you can change.
- Two records with equal fields are equal, also when their class names differ. A record is also
  equal to a plain dictionary with the same fields.

On a result map and on a record, a dictionary name keeps its dictionary meaning. With an `items`
field, `r.items` is the mapping method and `r["items"]` is the field. A name that is not a field
raises an error that lists the real fields:

- `r["methods"]` raises `KeyError`.
- `r.methods` raises `AttributeError`.

Use a listed field. `doc(tool)` shows the same fields under **Model schemas**.

When the extension declares a [backing field](extension-api.md#field-backed-sequences),
iteration, `len(r)`, and `r[0]` read that field. `x in r` tests the rows, and a record with no
rows is false. For such an empty record, `json.dump(r, file)` writes `{}`, so use
`file.write(json.dumps(r))`. A record without a backing field iterates over its field names, as a
dictionary does.

`read_session()` also returns more data than its printed summary. The full history, folded steps
included, is under `transcript["turns"]`, then `iterations`, then `blocks`. Each block holds
`code`, `stdout`, and an `error` if one occurred. `list_sessions()` finds other saved sessions.

## What the sandbox may do

The limits in the table come from the process jail, which is off by default. With the jail off,
sandbox code reaches the same files and hosts as your account, and `session["access"]["is_jailed"]`
is `false`. With the jail on, Vis checks filesystem, process, and network operations, also those of
imported libraries.

| Capability | Policy with the jail enabled |
| --- | --- |
| Filesystem IO | Confined to the allowed workspace roots |
| Start of a process (`subprocess`, `os.system`, `os.popen`) | Refused. Use `shell(...)`, which applies the process policy |
| `ctypes` and foreign libraries | Refused |
| HTTP clients | Sent through the gateway policy and the network filters |
| Raw sockets | Guarded at the socket level |
| Threads | Limited for each process. At the limit, a new thread raises `RuntimeError`. Also without the jail |
| Wall-clock time | Each block has a timeout, which does not apply while a live view is open. Also without the jail |
| Open files and sockets | Limited by `VIS_PY_MAX_OPEN_FILES`. A full table stops `shell` from starting processes. Also without the jail |

Host functions always apply their own permission checks, with or without the jail. To turn the jail
on and to read the complete policy, go to [Process jail and network policy](jail.md).

## Packages

Imports do not install packages. A missing import raises `ModuleNotFoundError`.

### Where each kind of code gets its packages

| Code | Packages |
| --- | --- |
| `py3` | The shared packages in `~/.vis/python/packages`, which you install explicitly |
| An extension without a declared uv project | The same shared directory. Every project on the gateway shares it |
| An extension package or a declared uv project | Its own uv environment, which Vis prepares when it loads the extension. No fallback to shared packages |

The dependencies of an extension with its own environment do not become available in
`py3`.

### Install shared packages

Install from a terminal, not from a block:

- `vis-agent python --shared -m pip install <package>` installs a shared sandbox package.
- `vis-agent python --shared -m <module>` runs a shared tool, also from a project directory.

After you update shared packages, run `/reload` to build the session workers again. Shared packages
do not relax the sandbox restrictions, so a package that needs a refused native operation may work
only in an extension.

### Run programs with `vis-agent python`

From a terminal, `vis-agent python` runs programs as the `python` command does. `-c CODE`,
`-m MODULE`, a script path, or `-` for standard input runs as `__main__`, with the remaining
arguments in `sys.argv`. An uncaught error prints its traceback to stderr, and the command exits
with the status of your program. Unlike `py3`, the command has no tool functions and
no automatic imports. `vis-agent python --version` prints the Python version. Other interpreter
options, such as `-u` or `-X`, are not supported.

To use the tools from a program, send a block to a session through
`client.routes.post_session_run`, as [Operations](sessions-api.md#operations) shows. The block runs
with the tools of that session and no model.

Each word after the program goes to the program, also `--help` and `-v`. Put the options of Vis
before the program: `vis-agent python --no-network script.py --help`.

- `vis-agent python uv sync --project PATH` is upstream uv. It prepares the environment of the
  project, normally `.venv`, not the shared sandbox packages.
- From that project, `vis-agent python -m <module>` uses that environment with the embedded Python
  of Vis. A missing or incompatible environment reports an error, with no fallback to the shared
  packages.
- To use the interpreter of the project, run
  `vis-agent python uv run --project PATH --no-sync python -m <module>`.

[Python import roots](configuration.md#python-import-roots) has the selection rules and the
explicit source paths.

`vis-agent python -m pip install` installs into the environment that the CLI uses. A child process
that your program starts with `sys.executable` imports the same packages. In a project, that child
process is the interpreter of the project environment. A console script that pip installed there
also runs by itself. A uv environment has no pip, so run pip as `vis-agent python -m pip`.

### Editable projects

A locked uv project can install its own package and its local dependencies as editable. Their
`.pth` files or backend import hooks resolve imports to the source checkout. After you edit Python
source, `/reload` refreshes the editable imports and the extension tools, with no sync and no
gateway restart. A change to dependencies or packaging metadata needs a new sync. The
[editable-project guide](extension-development.md) has the steps.

An imported module can keep old code until a reload, and a running call can still use old bindings.
Native extension libraries do not reload. The source paths of an editable install must stay in the
allowed filesystem roots of the sandbox. An installed package gets no extra access. To check that
your package resolves to the checkout, import it and run `print(package.__file__)`.

## Attachments

Attachment functions need no import. `attach(...)` stores a file and returns its descriptor.
`list_attachments()`, `get_attachment(...)`, `read_attachment(...)`, and `show_attachment(...)`
get attachments by filename or id. A second file with the same name becomes a new version.

To store a Markdown report that you produced as `report`:

```python
attach(report.encode("utf-8"), filename="IMPLEMENTATION-search.md", kind="doc",
       media_type="text/markdown")
```

`attach(...)` also opens an app, a live view that people use after your block ends. Import the
builders with `from blockether.vis.extension import ui`, then call `attach(ui.app(...))`.
[Apps](live-views.md#apps) shows the components and how you answer a Send.

## Runtime locations

| Runtime | Location |
| --- | --- |
| JVM | Git-pinned `vis-python-runtime` bridge, with the platform archive cached under `~/.vis/python/runtime/<version>/<platform>/` |
| Native binary and release bundle | `vis-agent-python/` next to the executable |

`VIS_PYTHON_NATIVE_PATH` selects a different copy of the library, and `VIS_PYTHON_HOME` a
different interpreter tree. A normal installation needs neither.

## TLS compatibility

`python.tls_strict` defaults to `true`. An explicit `false` turns off only strict X.509 validation
for sandbox execution, trusted Python extensions, and `vis-agent python`. Certificate trust and
hostname checks stay on. Set the key in user or project YAML.
[TLS validation](configuration.md#python-tls-validation) explains the security cost, the scope,
and the worker reload.

## Diagnosing an unresponsive worker

Before Vis stops a Python worker that does not respond, it tries to save local hang evidence. The
evidence goes under `~/.vis/logs/YYYY-MM-DD/pyext-*/`, with the UTC start date of the worker.
[Logs and diagnostics](logging.md) has the locations and the retention.

The warning in the gateway log gives the path to `hang.edn`. This file records the worker PID, the
active RPC, the last completed RPC, and limited JVM stacks. When the runtime supports it,
`python-stacks.log` holds the Python frames, also when native code holds the GIL. The report
records if that capture succeeded, failed, or timed out.

Collection adds at most 500 ms before the worker stops, and a failed diagnostic does not prevent the
stop.

Both files are private to your OS account. They omit Python source, arguments, return values, and
local variables. Stack frames can still hold file paths and function names. Review and redact the
files before you add them to a bug report, and do not upload them automatically. A Python frame
location shows the definition line of the function and the bytecode offset, not the current source
line.

## See also

- To batch tool calls and keep results in Python, read
  [Context management](context-management.md).
- For the policy that limits the sandbox, read [Process jail and network policy](jail.md).
- To make your own modules importable, read
  [Python import roots](configuration.md#python-import-roots).
