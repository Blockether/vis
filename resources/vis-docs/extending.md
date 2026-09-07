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
