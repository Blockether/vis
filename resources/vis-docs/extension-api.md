# Extension API

Reference for Python extension declarations, tool contracts, callbacks and host
operations. Start with the [tutorial](extending.md) for a complete entry file or
[Extension design](extension-design.md) for authoring and test guidance.

## Find an API

| Task | Section |
| --- | --- |
| Declare and register an extension | [Registration](#registration) |
| Export a callable or object namespace | [Tools](#tools) |
| Explain when the agent should use it | [Prompts and discovery](#prompts-and-discovery) |
| Read types, defaults and introspection limits | [Tool contracts](#tool-contracts) |
| Add a user command or guard | [Slash commands](#slash-commands) · [Op hooks](#op-hooks) |
| Persist data or report status | [Durable state](#durable-state) · [Logging and notifications](#logging-and-notifications) |
| Read environment or add session context | [Environment](#environment) · [Session context](#session-context) |
| Access files or start a process | [Filesystem and processes](#filesystem-and-processes) |
| Show forms, live work or register a provider | [Forms](human-input.md) · [Live views](live-views.md) · [Providers](provider-extensions.md) |

## Registration

Examples on this page use `import blockether.vis.extension as vis` inside a trusted
extension, not in `python_execution`. Call `vis.register(vis.Extension(...))` once
per entry file. The declaration constructors validate and copy values without IO;
registration applies them and resolves the declared environment.

`Extension` arguments are keyword-only. Its collection arguments accept sequences
of the corresponding SDK declarations and are stored as tuples, not mutable registries.

| Argument | Default | Meaning |
| --- | --- | --- |
| `name: str` | Required | Unique extension identity; also keys durable state |
| `description: str` | Required | One-line summary for the extension list and model snapshot |
| `alias: str` | `None`; required with symbols | Registry identity, not a tool-name prefix |
| `symbols` | `()` | `vis.Symbol` declarations for callable tools |
| `prompt: str` or callable | `None` | Model instructions; a callable receives the env dict each turn and returns text or `None` |
| `activation` | `None` | Optional `(env) -> bool`; `False` hides the extension for that turn |
| `slash_commands` | `()` | `vis.SlashCommand` declarations for user commands |
| `op_hooks` | `()` | `vis.OpHook` guards or observers |
| `network_filters` | `()` | `vis.NetworkFilter` request/response policy; see [network filters](jail.md#project-network-filters) |
| `providers` | `()` | `vis.Provider` declarations |
| `ctx` | `None` | Optional `(env) -> dict` merged into the model's session context |
| `env` | `()` | Names of host environment variables this entry may read |
| `kind`, `version` | `None` | Display metadata; an installed package supplies its manifest metadata |

The callback env dict contains `cwd`, `session_id` and `channel`. Keep `prompt`,
`activation` and `ctx` short-running. Calls into one extension instance are serialized.
Dependency and skill metadata belong to the [package manifest](extension-packages.md#package-manifest),
not `Extension`.

## Tools

```python
vis.Symbol(fn_or_object, name=None, tag="observation", is_hidden=False, activity=None)
```

`name=None` uses a function's name; set `name` explicitly for an object namespace.
`tag` is `observation` or `mutation`. `is_hidden=True` removes a callable from
model-facing discovery but does not make it inaccessible or authorize its use.
Every exported callable needs a nonblank docstring and an explicit activity
presentation. `activity=None` leaves only engine execution evidence; it does not
create a result view. Do not rely on this omission for a finished extension.

Source functions can use `def` or `async def`. The model calls their proxies with
`await` in `python_execution`. The trusted worker awaits an asynchronous result
before returning it; execution-watchdog parking covers that wait too. Each async
invocation uses its own event loop, so create and close loop-bound resources within
that invocation. Arguments and completed results remain Python values. Exceptions
are ordinary tool failures. The [execution boundary](#filesystem-and-processes)
determines which values can cross to the sandbox.

### Return typed objects

A dict is sufficient for simple results. Use frozen dataclasses with annotated
public fields when shape and meaning matter. The [tested example](extension-design.md#describe-structure-once)
shows field descriptions with `Annotated`. Private fields and original methods do
not cross into the sandbox.

### Object namespaces

`vis.Symbol(Greeter(), name="greet")` exports `greet.hello(...)` in the
[packaged example](extension-design.md#keep-the-entrypoint-small). Public methods become tools; object attributes become
nested namespaces. Names beginning with `_` are excluded. Public scalars, modules,
classes, cycles and repeated object references are rejected with their path.
`vis.method(tag="observation", is_hidden=False, activity=None)` overrides metadata
for one method. Metadata never changes the method's Python signature.

### Activity presentation

Declare presentation beside each tool binding:
`vis.Symbol(fn, activity=vis.Activity(label="Run checks", render=render_checks))`.
For an object namespace, put `@vis.method(activity=...)` on **each exported
method**, including nested methods. Namespace-level Activity is rejected.

Activities are meant for human consumption, not as a second machine protocol.
Write understandable English: an action in the headline, its target or outcome
in the summary, and selected evidence in the content. Use sentence case: "Read
file", "Run tests", "Check service health". Do not expose `run_tests`, qualified
method names, object representations or all-caps sentences as labels. Preserve
proper names such as GitHub and acronyms such as SDK. Keep terminology consistent
and never use profanity or vulgarity. Do not change the case of filenames, code,
commands or returned content to match a label.

`vis.Activity(presenter="tests", label="Run checks", render=render_checks)` selects
a semantic presenter and a synchronous callback. `presenter` classifies the
operation; it does **not** generate tool-specific content. The default
`"generic"` is only a classification, not a result renderer. The engine never
turns an arbitrary result object into an activity view or a generic result-summary
block. The tool's return value remains independently available to Python.

Not every operation needs a visible start. Set `show_start=False` for fast local
actions such as reading a file, applying a patch or looking up a cached record:

```python
activity = vis.Activity(
    label="Read record", show_start=False, render=render_record
)
```

This is **end-only presentation**, not missing lifecycle tracking. The engine
still records paired start/end evidence for order, duration, errors and
cancellation. It does not publish a running row or start progress for that call.
The callback skips `start` and runs on `success` or `failure`. Published content
is retained internally and becomes visible only when the call settles, including
failure or cancellation. Returning `None` from a start callback alone does not
hide the engine's running row; use `show_start=False`.

The final presentation must make sense without any earlier progress: name the
operation and target, state the outcome, and retain useful counts, errors or
changes. An empty result should say what was absent, such as "No jobs reported".
A successful tool return does not imply a successful external operation: a
workflow watcher can return normally with a failed workflow. Show that failure
in the final summary rather than saying only "Completed". Label partial lists
and excerpts so they cannot be mistaken for complete results.

Keep the default `show_start=True` for work a person waits for: tests, builds,
network requests, file transfers or user input. Publish intermediate updates only
when they communicate a meaningful change. Choose the policy at each binding,
not from an arbitrary duration threshold; a remote read may need progress even
though a local read does not. Internal counts still include all observed calls.

The callback receives keyword arguments `phase` (`start`, `success`, `failure`),
`args`, `kwargs`, `result` and `error`. It returns
`vis.ActivityPresentation(headline, summary, content=(), sections=())` or `None`
to keep the current presentation. The engine supplies identity, state, timing,
errors and observed file changes; callbacks must not invent them. Callback errors
do not change the tool's return value or exception.

Headline and one-line summary stay visible when collapsed; only content waits
behind disclosure. Show meaningful counts, targets and failure context, not a
serialized result or a duplicate output preview. Choose typed `ActivityText`,
`ActivityHeading`, `ActivityMarkdown`, `ActivityCode`, `ActivityDiff`,
`ActivityTable`, `ActivityProgress`, `ActivityImage`, `ActivityVideo`,
`ActivityAudio` and `ActivityFile` blocks. Media references existing attachments.
`ActivitySection(headline, summary, content=())` groups related results without
nesting sections. `vis.publish_activity(presentation)` replaces the current
snapshot while a tool runs; empty content clears the body.

Each headline and summary is one line of at most 512 UTF-8 bytes. A presentation
allows 8 sections, 32 total blocks and 32 KiB. Bound large content deliberately
and label excerpts. Test declared start visibility, success, failure, cancellation,
empty results, replacement, redaction and disclosure in both clients. The
[tested greeter entrypoint](extension-design.md#keep-the-entrypoint-small)
demonstrates the callback beside registration. Exact portable limits and block
shapes live in the
[Activity contract](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/activity.json).

## Prompts and discovery

Use `prompt` for a short explanation of **when** to choose this extension and the
public names to search. For the packaged greeter, a suitable value is:

```text
Use greet.hello to generate greeting text; it never sends messages.
Discover the tools with apropos(r"^greet\.") and read doc("greet.hello") before calling.
```

Assign this text to `Extension.prompt`; it is not Python code and does not execute
at registration. A prompt callable computes text each turn; it should not log in,
start background work or repeat the entire API. `description` supplies the extension
summary even when `prompt` is omitted.

`apropos(pattern)` filters public symbol names by regular expression. `doc(name)`
or `doc(hit)` reads the complete matching document. Put parameter details in the
tool's docstring and annotations, not the prompt. Put an optional multi-step
procedure in a [skill](skills.md). Neither prompt text nor reading a skill enforces
permissions; use explicit policy mechanisms for guards.

## Tool contracts

`Symbol.contract` returns fresh portable data without registration or a tool call.
In the sandbox, each callable exposes its contract as an attribute, for example
`greet.hello.contract`. Changing that local dictionary does not change the declared
tool or its permissions.

| Contract | Fields |
| --- | --- |
| Callable | `version`, `name`, `tag`, `description`, `signature`, `parameters`, `returns` |
| Namespace | `version`, `name`, `members` with full public member names |
| Parameter | Name, parameter kind, `required`, `has_default`, `default_is_none`, and type description |

`doc()` renders this same contract as the signature, prose, argument types and
result fields. This is a documentation contract, not JSON invocation or runtime
validation. Python binds arguments; the implementation validates domain constraints.
There is no manual schema/signature override. The exact portable shape is the
[symbol schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/symbol.json).

### Defaults and introspection

Non-`None` runtime default values are withheld, and their `repr()` is never called.
This avoids exposing private host objects or credentials, including values whose
Python type looks ordinary. **Public default behavior still belongs in the tool's
documentation**; see [documenting defaults](extension-design.md#document-default-behavior).

| Python declaration | `has_default` | `default_is_none` | Rendered default |
| --- | --- | --- | --- |
| Required argument | `False` | `False` | No default |
| Argument with `= None` | `True` | `True` | `None` |
| Argument with any other default, including `False` or `30` | `True` | `False` | `...` |

`*args` and `**kwargs` are not required even though they have no default.
The original defaults apply when arguments are omitted. `...` is a display marker,
not an instruction to pass `Ellipsis`. Dataclass-field defaults and factories are
also described without exporting values or running factories.

| Sandbox inspection | Supported result |
| --- | --- |
| `tool.contract` | Portable parameter/result types, fields and documented meaning |
| `doc("tool")` | Human-readable rendering of that contract |
| `inspect.signature(tool)` | Names and parameter kinds; `None` or `Ellipsis` defaults; no type annotations |
| `tool.__annotations__`, `typing.get_type_hints(tool)` | Empty dictionaries, not a supported type-discovery API |
| `tool.__signature__` | Not supplied |

The original host classes and their identity do not cross the sandbox boundary.
Use `.contract` for type discovery instead of trying to reconstruct host annotations.

### Supported types and unresolved annotations

Descriptions cover every parameter kind, return types, dataclass fields, unions,
common containers, `Literal` and string metadata in `Annotated`. Recursive records
use references rather than expanding forever.

- `tuple[T, ...]` has `variadic: true` and one type in `arguments`; a fixed-length
  tuple retains each item type and omits `variadic`.
- `Name (unresolved)` means the annotation could not be resolved safely. Vis does
  not guess, import or evaluate an expression to discover a type.
- `Name (opaque)` means the class is known but its structure is not described.
  Both labels appear in nested types and record fields, not just top-level returns.

Use `from __future__ import annotations` and module-level result classes. Python
3.14's deferred annotation functions can execute code even when asked for strings;
without that import they are reported as unresolved instead of evaluated. Local
forward references absent from the defining module remain unresolved.

### Cross-module decorators

`functools.wraps` chains resolve annotations in the wrapped function's defining
module, including bound namespace methods and qualified names such as `models.Result`.
Cyclic `__wrapped__` chains raise `ValueError`. These two complete modules illustrate
cross-module wrapping and a variadic tuple result:

```python
# decorators.py
from functools import wraps


def traced(fn):
    @wraps(fn)
    def call(*args, **kwargs):
        return fn(*args, **kwargs)
    return call
```

```python
# tools.py
from __future__ import annotations
from dataclasses import dataclass
from decorators import traced


@dataclass(frozen=True)
class Result:
    text: str


class Tools:
    @traced
    def read(self, text: str = "ready") -> tuple[Result, ...]:
        """Read one result without changing state."""
        return (Result(text),)
```

`vis.Symbol(Tools(), name="tools").contract` expands `Result.text` beneath the
variadic tuple. Omitting `text` uses the public string `"ready"`. The SDK tests
execute these snippets, and host-to-sandbox tests cover invocation, nested records,
redacted defaults, introspection and refreshed metadata after reload. This is
contract inspection, not registration. When you bind `Tools.read`, declare its
Activity in the entrypoint as in the [greeter example](extension-design.md#keep-the-entrypoint-small);
this immediate local operation can use `show_start=False`.

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
A gateway-wide worker loads registrations; a session gets its own trusted extension
worker on its first extension call. Session disposal stops that session's worker,
not the gateway-wide registration worker. Interpreter memory and host-call identities
are not shared across workers.

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
Ordinary `open()` uses the extension's permissions too, not the model's jail.

## See also

- [Extension design](extension-design.md) — choosing and documenting tool behavior.
- [Installing and sharing extensions](extension-packages.md) — installation and reload.
- [Extension troubleshooting](extension-troubleshooting.md) — loading and call errors.
