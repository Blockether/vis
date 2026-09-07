# Extending Vis

An extension adds capabilities to Vis: tools the model can call, slash commands
for you, guards over file operations, LLM providers and more. This page shows
how to write a Python extension. Python extensions are single files you drop
into a directory and reload in place, which makes them the right choice for
project-specific tools. For engine-level integrations that ship inside the
binary, see [Clojure extensions](clojure-extensions.md).

## Your first extension

1. Create `~/.vis/extensions/greeter.py`:

   ```python
   """Greeter — smallest possible tool extension."""
   import blockether.vis.extension as vis


   def greeter_hello(name):
       """await greeter_hello(name) -> {"greeting"} — greet someone."""
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

`blockether.vis.extension` is the same module the engine injects, published on
PyPI as [`vis-agent`](https://pypi.org/project/vis-agent/). Install it to
import, test and lint an extension in an ordinary Python process:

```bash
pip install vis-agent
```

Without an engine, the module binds a local host: `vis.state`, `vis.log` and
`vis.shell` work against your machine, sandbox-only operations refuse by name,
and `vis.ask` prompts in the terminal. Prime answers for tests with
`vis.outside.answer_with({...})` or the `VIS_OUTSIDE_ANSWERS` JSON variable, or
set `VIS_OUTSIDE_NONINTERACTIVE=1` to make every ask come back undeliverable.

## The declaration

`vis.register(vis.Extension(...))` is the only call that registers anything, and
a file makes it once. `Extension`, `Symbol`, `SlashCommand`, `OpHook`,
`NetworkFilter` and `Provider` are frozen declarations: constructing one
validates it but performs no IO.

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
their docstrings with `doc(name)`, so a prompt fragment that repeats a
signature costs tokens without adding information. Use it for facts the
docstrings cannot carry, such as a per-project catalog.

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
    """Flip one todo. Answers {"id", "done"}; raises when the id is unknown."""
    todos = vis.state.get("todos", [])
    for t in todos:
        if t["id"] == id:
            t["done"] = not t["done"]
            vis.state["todos"] = todos
            return {"id": id, "done": t["done"]}
    raise ValueError(f"no todo with id {id}; call todo_list() to see ids")
```

### Return typed objects

A result crosses into the sandbox as a real Python value, so return a real
type. A flat dict of scalars is fine for a trivial result. As soon as a result
has a field whose meaning is not obvious, a `None` that means something, or a
second consumer, make it a frozen dataclass:

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
    `is_truncated` says the cap cut something.
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

Register an object instead of a function to publish a tree of tools under one
name:

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
Public methods become tools, public attributes holding objects become nested
namespaces, and names starting with `_` never cross. Public scalars, modules
and classes are rejected with their path rather than serialized silently.
`vis.method(...)` overrides `tag`, `is_hidden` or `activity` for one method.

### Activity presentation

`activity=vis.Activity(presenter="tests", label="Run checks")` describes how a
running tool is shown in the TUI and the Companion app. Add `render=callback`
to compose the presentation: the callback receives `phase` (`start`, `success`
or `failure`), `args`, `kwargs`, `result` and `error` and returns a
`vis.ActivityPresentation(headline, summary, blocks)` or `None`. Blocks are
`heading`, `text`, `markdown`, `code`, `diff`, `table`, `progress`, `image`,
`video`, `audio` and `file`. `vis.publish_activity(presentation)` replaces the
presentation while the tool runs. Presentation errors never change a tool's
result. The normative vocabulary is the
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

`"fs_access"` is a gate, not a tool. It is asked for every path the host file
tools touch (`cat`, `grep`, `patch`, `ls`), so a guard cannot be bypassed by
choosing another tool. `fn(access)` receives `{"operation": "file-read" |
"file-write", "path": <absolute path>}`. A gate takes no `phase`, and an error
inside it refuses the operation. It does not apply to `open()` inside the
sandbox, which is bounded by the sandbox roots instead.

`vis.strings_of(value)` collects every string leaf of a nested structure, which
is handy for scanning arguments for paths.

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

An extension does not receive a copy of the host environment. Name the
variables you need and the host injects them into `os.environ` before the file
runs:

```python
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
itself need no declaration. `env=` affects only the extension's own
`os.environ`; it never widens what a jailed child process receives. To pass a
variable to a confined child, declare it under `environment:`.

## Session context

A `ctx` callable writes a slice into the `session` dict the model sees every
turn:

```python
def _ctx(env):
    return {"session_env": {"todo": {"open": len(vis.state.get("todos", []))}}}

vis.register(vis.Extension(name="todo", description="Todo list.", ctx=_ctx))
```

Return a string-keyed dict nested under a key unique to your extension. Slices
from every extension are deep-merged. A non-dict return or an exception
contributes nothing and never blocks a turn.

## Filesystem and processes

Extension code runs in a trusted context, separate from the model's sandbox:

| | Model sandbox | Extension context |
| --- | --- | --- |
| Author | the model | you |
| Filesystem | workspace roots only | your user's permissions |
| Network and processes | gateway policy; no direct spawn | unrestricted |
| Environment | project values | declared `env` plus project values |
| Lifetime | one session | the process, rebuilt on `/reload` |

`subprocess`, `os.system` and `vis.shell({...})` run unconfined. Output the
extension does not read itself is captured into the extension's log rather
than written to the terminal, and a child that reads `isatty()` sees a pipe.
Pipes are drained into an 8 MiB backlog per stream, so `Popen(stdout=PIPE)`
followed by `wait()` never deadlocks.

To confine a child, use a jailed shell:

| Call | Policy | Needs a session |
| --- | --- | --- |
| `vis.shell({...})` | none | no |
| `vis.jailed_shell({...})` | merged configuration on disk, read at each spawn | no |
| `vis.jailed_shell_session({...})` | the invoking session's policy snapshot | yes |

`vis.fs` reads and writes files outside the session roots on behalf of the
extension: `mkdir`, `write`, `read` (bytes), `read_text`, `copy`, `move`,
`list`, `stat` and `remove`. Ordinary `open()` remains the right call for
paths inside the session's roots.

Calls into one extension file are serialized. Keep `prompt`, `activation` and
`ctx` fast; tools may take their time.

## Reloading

`/reload` closes every Python extension context and loads the current files.
State in `vis.state` survives; new tools and commands are available to live
sessions immediately. Nothing else picks up an edit: a running Vis serves the
files it loaded until you reload.

At load, the extension's directory is copied to a private location and that
copy is what runs, so editing a helper module after the load also waits for
`/reload`. Files an extension writes next to itself land in that copy; keep
durable data in `vis.state`.

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
them with `/test` in a session or `vis-agent extension test` in CI; both use
real pytest, installed on first use, and exit non-zero on failure.

```python
# ~/.vis/extensions/my_ext/test_core.py
from mypkg.core import add

def test_add():
    assert add(2, 3) == 5
```

`vis.testing.LiveRecorder` records what a live view emitted and simulates
surface actions without a session; see [Live views](live-views.md#testing-a-view).

## See also

- [Asking the human](human-input.md) — forms, field types, layout and validation.
- [Live views](live-views.md) — progress a person can watch while a tool runs.
- [Provider extensions](provider-extensions.md) — registering an LLM provider.
- [Clojure extensions](clojure-extensions.md) — engine integrations that ship in the binary.
- [Process jail and network policy](jail.md) — what a confined child may reach.
