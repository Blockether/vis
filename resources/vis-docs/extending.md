# Extending Vis

Python extensions add tools, slash commands, file-operation checks and LLM
providers to Vis. Load them from a file and update them with `/reload`.

## Your first extension

1. Create `~/.vis/extensions/greeter.py`:

   ```python
   """A greeting tool."""
   import blockether.vis.extension as vis


   def greeter_hello(name):
       """Return a greeting in a dict with a `greeting` key."""
       return {"greeting": f"hello {name}"}


   vis.register(vis.Extension(
       name="greeter",
       description="Greets people.",
       alias="greeter",
       symbols=[vis.Symbol(greeter_hello, tag="observation")],
   ))
   ```

2. Start Vis, or run `/reload` in a running session.
3. Ask the model to greet you. It calls `await greeter_hello("vis")` and reads
   the docstring through `doc("greeter_hello")`.

Extensions load from two directories:

| Directory | Scope |
| --- | --- |
| `~/.vis/extensions/` | every project |
| `<project>/.vis/extensions/` | that project only |

A project file with the same `name` as a global one replaces it. A file that
fails to load is reported by `vis-agent doctor` and never stops Vis from
starting.

Treat a project's `.vis/extensions/` like its build scripts: the files run with
your permissions when Vis starts in that checkout. Review them before opening
an untrusted repository.

## Separate implementation and dependencies

Keep a thin entry file under `.vis/extensions/` and the implementation in its
own source tree. Declare both import roots and dependencies in a
[PEP 723 script metadata block](https://peps.python.org/pep-0723/), before imports:

```text
project/
  vis.yml
  einmal/src/einmal/__init__.py
  .vis/extensions/einmal.py
```

```python
# .vis/extensions/einmal.py
# /// script
# requires-python = ">=3.11"
# dependencies = ["httpx==0.28.1"]
# [tool.vis]
# source_paths = ["../../einmal/src"]
# ///
import blockether.vis.extension as vis
from einmal import status

vis.register(vis.Extension(
    name="einmal",
    description="Company tools.",
    alias="einmal",
    symbols=[vis.Symbol(status)],
))
```

`status` must have a docstring, like any exported tool. The metadata is parsed
without executing the entry. Vis validates it, snapshots the source files,
installs dependencies, then evaluates the entry and registers its tools.

- `source_paths` names **import roots**: directories containing the packages or
  modules you import, not the package directories themselves. Relative paths are
  resolved against the extension entry's directory, not the current working
  directory. Absolute paths are also accepted.
- Each declared root's contents are merged into the frozen extension directory.
  Missing directories, duplicate relative file names and roots containing the
  extension directory are rejected. Use narrow source roots such as `src`, not
  the whole checkout or a virtual environment. `.vis-packages` is reserved.
- `dependencies` accepts standard package requirements, including version pins,
  extras and environment markers. Vis uses its bundled **pip**, installing only
  wheels into the snapshot's private `.vis-packages` directory. It does not
  modify the project's `.venv` or the shared sandbox package cache.
- Set the index with [`python.index_url` in `vis.yml`](configuration.md#python-package-index).
  Normal pip authentication and certificate settings still apply. A missing
  wheel or failed install is a load failure, not a fallback to source builds.
- `requires-python` checks the embedded interpreter. Vis does not download
  another Python version to satisfy it.
- `/reload` takes new source snapshots and resolves declared dependencies again;
  an unchanged loader scan does neither. Source edits are not used by existing
  tools until reload. A failed reload retains the last working extension.

The installed dependencies travel with that snapshot into session workers;
calling a tool does not install them again. A snapshot is not a separate virtual
environment: extensions in the same worker still share its interpreter and
module cache, so incompatible dependency versions are not isolated.

### uv projects

For an implementation managed by uv, select its project directory instead of
repeating its dependencies in the script:

```python
# /// script
# dependencies = []
# [tool.vis]
# project = "../../einmal"
# source_paths = ["../../einmal/src"]
# ///
```

`project` is relative to the entry file (absolute paths also work). It must
contain both `pyproject.toml` and an existing `uv.lock`. Put uv settings, including
`[tool.uv.sources]`, in that project's `pyproject.toml`, not the script block.
Project mode rejects nonempty script dependencies to avoid ignoring them.

Install uv on the **sync command's PATH**, then explicitly prepare dependencies:

```bash
vis-agent python uv sync --project ./einmal --locked
```

Run this as the same OS user and with the same Vis runtime as the gateway.
The command uses the embedded Python and runs real
`uv sync --locked --no-editable --no-default-groups --no-python-downloads`.
It respects uv sources and named indexes. It does not update `uv.lock` or touch
the project's `.venv`. Default groups and optional extras are not installed.
`--offline` and `--no-cache` are supported; other uv options are rejected so they
cannot redirect the environment or interpreter. Generate the lockfile separately.

Vis publishes dependencies under `~/.vis/python/projects/<project-hash>/<generation>/`.
The command prints the prepared `.vis-packages` directory. Start and `/reload`
**do not run an installer for uv projects**: they copy those prepared packages
into the extension's private snapshot, then import and register its tools.
A missing environment or changed `pyproject.toml`, `uv.lock`, runtime or default
index is a load error with the sync command to run. A failed reload retains the
last working extension. Sync failures do not replace the last prepared generation.

Package builds run only during the explicit command, with the invoking user's
permissions; select trusted projects and dependencies. Local packages are installed
noneditably. After changing an installed local package, run sync again. Code in
`source_paths` only needs `/reload`. Once a manual project is loaded, missing-import
autoinstall is disabled in that interpreter, including its session worker.
Extensions still share the worker's module cache; conflicting versions are not isolated.

`python.index_url` supplies uv's default index; named source indexes are not replaced.
Keep credentials in uv's supported credential configuration or the sync process's
environment, not committed URLs. Installer diagnostics are suppressed because they
may contain credentials. The gateway no longer needs uv on its PATH just to load
a prepared project.

Outside Vis, provide the implementation's import roots in your test or
packaging configuration; `tool.vis.source_paths` is a Vis loader setting.

### Explicit installation workflow

1. Update `uv.lock` with uv when dependencies change.
2. Run `vis-agent python uv sync --project ./einmal --locked`.
3. Run `/reload` in Vis, then call the registered tool.

This manual workflow is selected by `tool.vis.project`. Script-only PEP 723
`dependencies` above still use the automatic pip loader; use project mode when
installation must be explicit. A plain external `uv sync` prepares a different
project environment and does not publish dependencies for Vis.

### Keep business logic outside the entry file

The entry file above only imports and registers tools. Put their implementation
in the ordinary Python package, for example `einmal/src/einmal/__init__.py`:

```python
def status() -> str:
    """Return the company integration status."""
    return "ready"
```

Test that package independently in `einmal/tests/test_status.py`:

```python
from einmal import status


def test_status():
    assert status() == "ready"
```

From the repository root, run the tests with an explicit import root:

```bash
PYTHONPATH=einmal/src vis-agent python -m pytest einmal/tests/ -q
```

The spelling is `vis-agent python -m pytest`, not `vis-agent python pytest`.
The explicit `PYTHONPATH` makes this example independent of package-layout
inference. This command does not select the extension's private uv environment
or prove that locked dependencies are prepared. The current embedded runtime can
automatically install missing imports; this is not an install-free test workflow.

After unit tests pass, verify extension registration and a tool call in a Vis
session: explicit sync, `/reload`, then call the tool. `vis-agent extension list`
checks registration only. A passing package test alone does not prove that the
prepared dependencies can be imported and called in the session worker.

**Current limitation:** the confined worker refuses native calls through `ctypes`.
SciPy's low-level callback initialization requires those calls, so SciPy is not
currently supported there even when sync and standalone Python tests succeed.
Do not treat successful installation or registration as a compatibility check.

## Developing outside Vis

The engine's `blockether.vis.extension` module is also published on PyPI as
[`vis-agent`](https://pypi.org/project/vis-agent/). Install it to import, test
and lint extensions outside Vis:

```bash
pip install vis-agent
```

Outside the engine, `vis.state`, `vis.log` and `vis.shell` use a local host
implementation, and `vis.ask` prompts in the terminal. Sandbox-only operations
return errors. Supply test answers with `vis.outside.answer_with({...})` or the
`VIS_OUTSIDE_ANSWERS` JSON variable. Set `VIS_OUTSIDE_NONINTERACTIVE=1` to make
input requests return an undeliverable status.

## The declaration

Call `vis.register(vis.Extension(...))` once per extension file. The
`Extension`, `Symbol`, `SlashCommand`, `OpHook`, `NetworkFilter` and `Provider`
constructors create immutable declarations. Construction validates values but
performs no IO; registration applies them.

| Argument | Type | Purpose |
| --- | --- | --- |
| `name` | str, required | Unique extension name. |
| `description` | str, required | One line for `vis-agent extension list` and the model's extension snapshot. |
| `alias` | str | Registry identity; required with `symbols`. It does not prefix tool names. |
| `symbols` | list of `vis.Symbol` | Tools the model can call. See [Tools](#tools). |
| `prompt` | str or callable | Text added to the model's prompt. A callable receives the env dict every turn and returns a string or `None`. |
| `activation` | callable | `(env) -> bool`, evaluated per turn. `False` hides the whole extension for that turn. |
| `slash_commands` | list of `vis.SlashCommand` | Commands for the user. See [Slash commands](#slash-commands). |
| `op_hooks` | list of `vis.OpHook` | Guards and observers over tool calls. See [Op hooks](#op-hooks). |
| `network_filters` | list of `vis.NetworkFilter` | Request and response policy at the gateway proxy. See [Process jail and network policy](jail.md#project-network-filters). |
| `providers` | list of `vis.Provider` | LLM providers. See [Provider extensions](provider-extensions.md). |
| `ctx` | callable | `(env) -> dict`, merged into the model's `session` dict every turn. See [Session context](#session-context). |
| `env` | list of str | Host environment variables this file may read. See [Environment](#environment). |
| `kind`, `version` | str | Display metadata. |

The env dict passed to `prompt`, `activation` and `ctx` contains only `cwd`,
`session_id` and `channel`.

Keep `prompt` short. The model finds tools with `apropos(pattern)` and reads
their docstrings with `doc(name)`. Do not repeat signatures in the prompt. Use
it for additional context, such as a project-specific catalog.

## Tools

```python
vis.Symbol(fn_or_object, name=None, tag="observation", is_hidden=False, activity=None)
```

- `tag` is `"observation"` for a tool that reads state or `"mutation"` for one
  that changes it.
- The sandbox name is `name`, defaulting to the function name.
- The docstring is the contract. `doc(name)` returns it verbatim under a
  signature rendered from the real Python signature, so do not repeat the
  signature in the docstring. State preconditions, side effects and what the
  result contains. The first line is what `apropos` previews.
- Parameter names are shown to the model. Name them as the model should type them.
- The return value is the result. Raise an exception to fail; its message
  reaches the model as an ordinary tool failure it can react to.
- `is_hidden=True` keeps a tool callable but out of the model-facing listing.

```python
def todo_toggle(id):
    """Toggle a todo; return `id` and `done` keys. Raise if the id is unknown."""
    todos = vis.state.get("todos", [])
    for t in todos:
        if t["id"] == id:
            t["done"] = not t["done"]
            vis.state["todos"] = todos
            return {"id": id, "done": t["done"]}
    raise ValueError(f"no todo with id {id}; call todo_list() to see ids")
```

### Return typed objects

Results are Python values in the sandbox. A dict of scalars is sufficient for
simple results. Use a frozen dataclass when fields need explanation, `None`
has a specific meaning, or multiple callers use the result:

- Annotate every parameter and return value; the model reads them next to the
  docstring.
- Never return a formatted string or JSON text that the caller must parse.
- Put field meaning in the class docstring: units, when a field is `None`,
  what a sentinel means.
- Use snake_case dict keys; they pass through as written.

```python
from dataclasses import dataclass


@dataclass(frozen=True)
class CommandResult:
    """Outcome of one command run on the remote server.

    `exit_code` is None exactly when the command was killed locally after
    `timeout_s`. `stdout` and `stderr` are decoded UTF-8 capped at 256 KiB;
    `is_truncated` is true if either output exceeded that limit.
    """

    command: str
    exit_code: int | None
    stdout: str
    stderr: str
    is_timed_out: bool
    is_truncated: bool
```

The Vis repository's own `.vis/extensions/uplink.py` is a complete example of
this pattern.

### Object namespaces

Register an object to expose nested tools under one name:

```python
class Issues:
    def find(self, query: str) -> Issue:
        """Find one issue by query."""
        ...

    @vis.method(tag="mutation")
    def create(self, title: str) -> Issue:
        """Create one issue."""
        ...


class Tracker:
    def __init__(self):
        self.issues = Issues()


vis.register(vis.Extension(
    name="tracker",
    description="Issue tracker.",
    alias="tracker",
    symbols=[vis.Symbol(Tracker(), name="tracker")],
))
```

The model calls `tracker.issues.find(...)` and `tracker.issues.create(...)`.
Public methods become tools, and object attributes become nested namespaces.
Names starting with `_` are excluded. Public scalars, modules and classes are
rejected with an error identifying their path. `vis.method(...)` overrides
`tag`, `is_hidden` or `activity` for one method.

### Activity presentation

`activity=vis.Activity(presenter="tests", label="Run checks")` describes how a
running tool is shown in the TUI and the Companion app. Add `render=callback`
to customize the display. The callback receives `phase` (`start`, `success`
or `failure`), `args`, `kwargs`, `result` and `error` and returns a
`vis.ActivityPresentation(headline, summary, blocks)` or `None`. Blocks are
`heading`, `text`, `markdown`, `code`, `diff`, `table`, `progress`, `image`,
`video`, `audio` and `file`. `vis.publish_activity(presentation)` replaces the
presentation while the tool runs. Presentation errors never change a tool's
result. Supported block types are defined in the
[Activity contract](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/activity.json).

## Slash commands

```python
vis.SlashCommand(name, run, doc=None, usage=None)
```

`run(ctx)` receives `{"channel", "args", "raw", "session_id"}` and returns
`vis.ok(title, body=None, data=None)`, `vis.err(title, body=None, data=None)`
or a plain string, which counts as an ok title. `body` is Markdown.

## Op hooks

```python
vis.OpHook(ops, fn, phase="before")
```

`ops` names sandbox tools such as `"patch"`, `"shell"` or `"python_execution"`.
With `phase="before"`, `fn(call)` receives `{"op", "args"}` and returns
`vis.block(reason)` to refuse the call or `None` to allow it; the model sees the
reason as a tool failure. With `phase="after"`, `fn` receives `{"op", "args",
"result"}` and its return value is ignored. An error inside a tool hook allows
the call.

`"fs_access"` checks paths used by the host file tools (`cat`, `grep`, `patch`,
`ls`). It is not a tool itself and takes no `phase`. Its callback receives
`{"operation": "file-read" | "file-write", "path": <absolute path>}`. An error
in the callback refuses the operation. This check does not apply to `open()`
in the sandbox, which uses the sandbox's filesystem policy.

`vis.strings_of(value)` collects strings from a nested structure, for example
to check paths in tool arguments.

## Durable state

`vis.state` is a dict-like store persisted in the Vis database. It survives
`/reload` and restarts and is keyed by extension `name`, so a project override
shares state with the global extension it replaces and two different extensions
never share.

```python
vis.state["repo"] = "acme/widgets"
vis.state.get("count", 0)
"repo" in vis.state
del vis.state["repo"]
vis.state.update({"repo": "acme/widgets", "count": 0})
```

It is a `collections.abc.MutableMapping`, so `pop`, `setdefault`, `clear`,
`keys`, `items`, `len` and iteration behave as on a dict. Values must be plain
data: dicts, lists, strings, numbers and booleans. Writing `None` removes the
key.

## Logging and notifications

```python
vis.log("info", "loaded 3 rules")        # trace, debug, info, warn, error
vis.notify("Rules reloaded", "success")  # info, success, warn, error
```

`vis.log` writes to the gateway log under `~/.vis/logs/`. `vis.notify` shows a
toast in the active channel.

## Asking the human and showing live work

`vis.ask(title, fields)` pauses the extension and shows a typed form in the TUI
or the Companion app. `vis.live(title, nodes)` opens a view that the extension
updates while a job runs. Both are documented on their own pages:
[Asking the human](human-input.md) and [Live views](live-views.md).

## Environment

An extension does not automatically receive the full host environment.
Declare the variables it needs in `env`. `vis.register()` resolves them and
adds them to the extension's `os.environ`; read them after registration.

```python
import os

vis.register(vis.Extension(
    name="acme",
    description="Acme integration.",
    env=["ACME_API_KEY"],
))

key = os.environ.get("ACME_API_KEY")   # absent when nothing resolves it
```

Each name resolves through the project's `environment:` block, then `.env` and
`.env.local`, then the environment that started Vis (see
[Configuration](configuration.md#environment)). Names defined by the project
itself need no declaration. `env=` affects only the extension's `os.environ`,
not the environment of jailed child processes. To pass a variable to a jailed
child, declare it under `environment:`.

## Session context

A `ctx` callable adds data to the model's `session` dict each turn:

```python
def _ctx(env):
    return {"session_env": {"todo": {"open": len(vis.state.get("todos", []))}}}

vis.register(vis.Extension(name="todo", description="Todo list.", ctx=_ctx))
```

Return a string-keyed dict under a key unique to your extension. Results from
all extensions are deep-merged. A non-dict return or exception adds no context
and does not block the turn.

## Filesystem and processes

Extension code runs in a trusted namespace, separate from the model's sandbox.
A gateway-wide worker loads extension registrations. Session calls use another
instance beside that session's sandbox, in the session's worker process.

| | Model sandbox | Extension context |
| --- | --- | --- |
| Author | the model | you |
| Filesystem | workspace roots when the jail is enabled | your user's permissions |
| Network and processes | gateway policy; no direct spawn | unrestricted |
| Environment | project values | declared `env` plus project values |
| Lifetime | session worker | registration or session instance; reloaded by `/reload` |

`subprocess`, `os.system` and `vis.shell({...})` run without the jail. Output
not read by the extension is captured in its log. Child processes receive
pipes rather than a terminal; output is drained into an 8 MiB buffer per
stream, including while the extension waits for the child to exit.

To confine a child, use a jailed shell:

| Call | Policy | Needs a session |
| --- | --- | --- |
| `vis.shell({...})` | none | no |
| `vis.jailed_shell({...})` | merged configuration on disk, read at each spawn | no |
| `vis.jailed_shell_session({...})` | the invoking session's policy snapshot | yes |

`vis.fs` provides filesystem operations with extension permissions: `mkdir`,
`write`, `read` (bytes), `read_text`, `copy`, `move`, `list`, `stat` and `remove`.
Use ordinary `open()` for paths inside the session's roots.

Calls into one extension instance are serialized. Keep `prompt`, `activation`
and `ctx` short-running; tools can perform longer operations.

## Reloading

`/reload` closes Python extension contexts and loads the current files.
`vis.state` persists, and live sessions can use the new tools and commands.
File edits do not affect a running extension until it reloads.

At load, Vis copies the extension's directory to a private location and runs
that copy. Changes to helper modules also require `/reload`. Files written
next to the extension are created in the private copy; use `vis.state` for
persistent data.

## Packages and tests

For anything larger than one file, use a directory with an `extension.py`
entry point:

```text
~/.vis/extensions/
  my_ext/
    extension.py      # calls vis.register(...)
    mypkg/
      __init__.py
      core.py
    test_core.py
```

The directory is on `sys.path` before `extension.py` runs, so `from mypkg.core
import add` works without path manipulation. Only `extension.py` is an entry
point.

Test files (`test_*.py` or `*_test.py`) are never loaded as extensions. Run
`/test` in a session to execute them with pytest, installed on first use.
Outside Vis or in CI, install `vis-agent` and `pytest`, then run
`python -m pytest /path/to/my_ext`.

```python
# ~/.vis/extensions/my_ext/test_core.py
from mypkg.core import add

def test_add():
    assert add(2, 3) == 5
```

`vis.testing.LiveRecorder` records live-view updates and simulates user actions
without a session; see [Live views](live-views.md#testing-a-view).

## See also

- [Asking the human](human-input.md) — forms, field types, layout and validation.
- [Live views](live-views.md) — progress a person can watch while a tool runs.
- [Provider extensions](provider-extensions.md) — registering an LLM provider.
- [Process jail and network policy](jail.md) — permissions for jailed processes.
