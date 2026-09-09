# Extension API

Reference for extension declarations, tool contracts, commands, hooks and host operations.

For a runnable package, start with the [quickstart](extending.md). Dependency and
skill metadata belong to the [package manifest](extension-packages.md), not `Extension`.

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

`name` is the public sandbox name, defaulting to the function name. `tag` is
`observation` or `mutation`; `is_hidden=True` leaves the tool callable but removes
it from model-facing discovery. A callable must have a nonblank docstring.

`Symbol.contract` returns fresh portable data without registration. A callable
contract has `version`, `name`, `tag`, `description`, `signature`, `parameters` and
`returns`. A namespace contract has `version`, `name` and `members`, each with its
full public name. The engine exposes each callable's description as its `.contract`
attribute; changing that local dict does not change the declaration or permissions.
Read [type descriptions and limitations](extension-design.md#one-description-two-readers).

`doc(name)` uses the same contract for its signature, prose, argument types and
result fields. `apropos(pattern)` remains a compact name-filtered catalog, not a
schema dump. Operations continue to accept Python arguments and return Python
values; exceptions remain ordinary tool failures.

### Return typed objects

A dict is sufficient for simple results. Use frozen dataclasses with annotated
public fields when shape and meaning matter. The [tested example](extension-design.md#describe-structure-once)
shows field descriptions with `Annotated`. Private fields and original methods do
not cross into the sandbox.

### Object namespaces

`vis.Symbol(Greeter(), name="greet")` exports `greet.hello(...)` in the
[quickstart](extending.md). Public methods become tools; object attributes become
nested namespaces. Names beginning with `_` are excluded. Public scalars, modules,
classes, cycles and repeated object references are rejected with their path.
`vis.method(tag="observation", is_hidden=False, activity=None)` overrides metadata
for one method. Metadata never changes the method's Python signature.

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

`ops` also names the draft lifecycle: `"draft/create"`, `"draft/approve"` and
`"draft/discard"` run for the sandbox's `draft_create()`, `draft_approve()` and
`draft_discard()`. Their `args` carry the draft's `workspace_id`, `label`,
`root`, `repo_root`, `backend` and, for approval, `branch`, `target_branch`,
`files` and `message`. Approval commits and merges into the default branch;
each new commit also crosses `git/commit`. A `before` hook that returns
`vis.block(reason)` refuses the operation and the user sees the reason. See
[Drafts](drafts.md).

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

Extension code runs in a trusted process, separate from the model's sandbox.
A gateway-wide worker loads registrations. Each session gets a separate trusted
extension worker on its first extension call; session disposal stops both workers.
The workers do not share interpreter memory or host-call identities.

| | Model sandbox | Extension context |
| --- | --- | --- |
| Author | the model | you |
| Filesystem | workspace roots when the jail is enabled | your user's permissions |
| Network and processes | gateway policy; no direct spawn | unrestricted |
| Environment | project values | declared `env` plus project values |
| Native calls through `ctypes` | refused under confinement | supported |
| Lifetime | session worker | separate registration or session worker; reloaded by `/reload` |

Tool results cross as data. Objects and dataclasses expose their public fields as
frozen data records in the sandbox, including nested records. The original class,
methods, native pointers and object identity stay in the trusted process. Expose
operations as declared tools rather than methods on returned objects.

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

## See also

- [Extension packages](extension-packages.md) — installation and reload.
- [Extension troubleshooting](extension-troubleshooting.md) — loading and call errors.
