# Extension API

This reference describes how a Python extension registers with Vis: its declarations, tool
contracts, callbacks, and host operations.

## When to use

- **You need the exact declaration, argument, or return value of an extension feature.** Start
  at [Find an API](#find-an-api).
- **`doc()` shows a tool's types or defaults differently from your declaration.** Check the
  [tool contract rules](#tool-contracts).
- **People must configure your extension for a project or a session.** Declare
  [settings](#settings) with their allowed scopes.
- **Your extension needs a service from Vis**, such as [durable state](#durable-state),
  [environment variables](#environment), [session context](#session-context),
  [attachments](#attachments), [another session tool](#call-other-session-tools), or [other
  sessions](#find-and-read-sessions).
- **Your extension must start work in new sessions.** Read [Create and control
  sessions](#create-and-control-sessions).
- **People must start your command with one key in the terminal.** Declare [key
  bindings](#key-bindings).
- **Your own program must read or export Activity history.** Read [Sessions
  API](sessions-api.md#read-activity-history).
- **Your editor or type checker reports errors for code that runs in Vis.** Install the [SDK
  version that Vis bundles](#editor-types).
- **A jailed tool cannot find its JDK, package cache, or launcher.** Declare [toolchain
  grants](#toolchain-grants).

For a complete entry file, start with the [tutorial](extending.md). For design and test
guidance, read [Extension design](extension-design.md).

## Find an API

| Task | Section |
| --- | --- |
| Declare and register an extension | [Registration](#registration) |
| Export a callable or object namespace | [Tools](#tools) |
| Explain when the agent should use it | [Prompts and discovery](#prompts-and-discovery) |
| Read types, defaults and introspection limits | [Tool contracts](#tool-contracts) |
| Add a user command or guard | [Slash commands](#slash-commands) · [Op hooks](#op-hooks) |
| Add configurable behavior | [Settings](#settings) |
| Persist data or report status | [Durable state](#durable-state) · [Logging and notifications](#logging-and-notifications) |
| Store or read files that a tool makes | [Attachments](#attachments) |
| Read environment or add session context | [Environment](#environment) · [Session context](#session-context) |
| Access files or start a process | [Filesystem and processes](#filesystem-and-processes) |
| Find the directory of the session | [Workspace root](#workspace-root) · [Python environment](#python-environment) |
| Show forms, live work or register a provider | [Forms](human-input.md) · [Live views](live-views.md) · [Providers](provider-extensions.md) |

## Build metadata

Extension entry files and `python_execution` get these globals without an import:

| Global | Meaning |
| --- | --- |
| `VIS_VERSION` | Version of the loaded Vis build |
| `VIS_SHA_RELEASE` | Full commit of the build, with `-dirty` for an uncommitted native build, or `None` if unavailable |
| `VIS_PYTHON_RUNTIME_VERSION` | Version of the embedded Python runtime library, not of the CPython interpreter |
| `VIS_PYTHON_SDK_VERSION` | PyPI version of the bundled SDK. A release has `VIS_VERSION`. A beta has a development release such as `0.2.47.dev7678` |

The model's system context reports the same values. They describe the loaded build, which is not
always a published release, and a source build can report `dev`. Use them for diagnostics.
Extensions and `python_execution` always use the bundled SDK. A copy from pip or an editable
install changes neither that nor these globals.

### Editor types

Your editor and type checker, for example Pyright or Pylance, read the `vis-agent` package in
your Python environment. Vis runs its bundled SDK. To get the same types, install the version in
`VIS_PYTHON_SDK_VERSION`:

```bash
pip install "vis-agent==0.2.46"
```

PyPI has the SDK of every release and every beta, some minutes after a beta release. Give the
exact version of a beta, because pip skips development versions otherwise. Pin the version only
in your development environment, not in an extension that you publish.

A lock file keeps the old SDK when you update Vis, so your editor shows old types.
`vis-agent doctor` reports each `vis-agent` in the project `.venv` or the shared packages that
differs from the bundled SDK. To update a uv project, set the new version in `pyproject.toml`.
Then run `uv lock --upgrade-package vis-agent` and `uv sync`.

A `dev` value, or a value that ends in `+dirty`, comes from a source run or a local build. PyPI
does not have these versions. Install `packages/vis-agent` from the same checkout.

In `python_execution`, `blockether.vis.extension` is the only import under `blockether.vis`. Use
it to inspect public types and to build declarations. No host exists there:

- Registration and host callbacks such as `state` and `shell` raise `RuntimeError`.
- Native filesystem helpers such as `fs.read` raise `PermissionError`.

[Sandbox experimentation](python-sandbox.md#experiment-with-extension-declarations) has an
example.

## Registration

The examples on this page use `import blockether.vis.extension as vis` in a trusted extension,
not in `python_execution`. Call `vis.register_extension(vis.Extension(...))` one time in each
entry file. The declaration constructors validate and copy values without IO. Registration
applies them and resolves the declared environment.

`Extension` arguments are keyword-only. A collection argument takes a sequence of the matching
SDK declarations and is stored as a tuple, not as a mutable registry.

| Argument | Default | Meaning |
| --- | --- | --- |
| `name: str` | Required | Unique extension identity, also the key of its durable state |
| `description: str` | Required | One-line summary for the extension list and the model snapshot |
| `alias: str` | `None` (required with symbols) | Registry identity, not a tool-name prefix |
| `symbols` | `()` | `vis.Symbol` declarations for callable tools |
| `prompt: str` or callable | `None` | Model instructions. A callable receives the env dict each turn and returns text or `None` |
| `activation` | `None` | Optional `(env) -> bool`. `False` hides the extension for that turn |
| `slash_commands` | `()` | `vis.SlashCommand` declarations for user commands |
| `automations` | `()` | `vis.Automation` declarations for scheduled runs without a model (see [automations](#automations)) |
| `keybindings` | `()` | `vis.Keybinding` declarations for terminal keys (see [key bindings](#key-bindings)) |
| `session_app` | `None` | One `vis.SessionApp` that each session lists from its start (see [session apps](#session-apps)) |
| `op_hooks` | `()` | `vis.OpHook` guards or observers |
| `network_filters` | `()` | `vis.NetworkFilter` policy for requests and responses (see [network filters](jail.md#project-network-filters)) |
| `providers` | `()` | `vis.Provider` declarations |
| `ctx` | `None` | Optional `(env) -> dict`, merged into the model's session context |
| `env` | `()` | Names of host environment variables that this entry may read |
| `grants` | `()` | `vis.Grant` toolchain paths for this extension's jailed children (see [toolchain grants](#toolchain-grants)) |
| `kind`, `version` | `None` | Display metadata. An installed package supplies its manifest metadata |

The callback env dict has `cwd`, `workspace_root`, `project_root`, `session_id`, and `channel`.
`cwd` and `workspace_root` are the session root. `project_root` is the [extension's
project](#project-root). Keep `prompt`, `activation`, and `ctx` short. Vis runs the calls into
one extension instance one at a time. Dependency and skill metadata belong to the [package
manifest](extension-packages.md#package-manifest), not to `Extension`.

## Settings

Declare `vis.Setting` objects in `Extension(settings=[...])` to get rows in the Settings of the
app and the TUI. In a callback, `setting.value()` returns the effective value for that response
and does not change the global setting. Outside Vis, it returns the declared default. A type
checker gives `value()` the type of the default, `bool` or `str`. Registration validates the
declarations and creates no override.

```python
import blockether.vis.extension as vis

include_details = vis.Setting(
    id="report_details",
    label="Report details",
    default=False,
    scopes=["global", "project", "group", "session"],
    description="Include the supporting detail in reports.",
)

def report() -> str:
    """Prepare a report with the selected level of detail."""
    return "Detailed report" if include_details.value() else "Summary"

vis.register_extension(vis.Extension(
    name="reports",
    alias="reports",
    description="Prepare reports.",
    settings=[include_details],
    symbols=[vis.Symbol(report, activity=vis.Activity(
        label="Prepare report", show_start=False,
    ))],
))
```

To nest a setting, set `parent` to the `id` of another setting. Settings shows the nested
setting one step in, below its parent, and nesting has no depth limit. If the parent is not in
the same settings section, the setting stays at the top level.

```python
detail_level = vis.Setting(
    id="report_detail_level",
    label="Detail level",
    type="enum",
    default="short",
    choices=["short", "full"],
    parent="report_details",
)
```

`id` is a lower-case snake_case identifier, unique among the settings of Vis and of the other
loaded extensions. If another owner has your `id`, your extension does not load, and the load
error names that owner.

`type` selects the kind of row:

- `"boolean"`, the default, takes a boolean `default`.
- `type="enum"` is a choice row. Give string `choices` and a `default` from those choices.
- `type="string"` is one line of text, such as a server address, with a string `default`. Vis
  removes spaces at both ends. A value has at most 4096 characters and no control characters,
  such as a line break.

For a password or an access token, also set `is_secret=True`. A secret setting needs
`default=""` and only the `global` scope. Settings then shows only **Saved** or **Not set**,
never the value. Vis keeps the value on this computer and writes `[REDACTED]` in its place in
trajectories.

```python
server_url = vis.Setting(
    id="reports_server_url",
    label="Report server",
    type="string",
    default="https://reports.example.com",
)

server_token = vis.Setting(
    id="reports_server_token",
    label="Report server token",
    type="string",
    default="",
    is_secret=True,
)
```

A secret setting has these limits:

- A project extension cannot save one. Settings does not show it, and `value()` returns the
  default.
- The `--toggles` command line option refuses a secret, because other programs can read a
  command line.
- Read it while Vis calls your extension, for example in a tool. At import time, and in a thread
  that your extension starts, `value()` can return the default.
- Another extension that reads it gets the default.

To act when a person changes a setting, give `on_change`. Vis calls `on_change(old, new, scope)`
one time after the app or the TUI saves the setting, if the effective value changed. `scope` is
the level of the change: `global`, `project`, `group`, or `session`. Use the hook for a side
effect, such as a file update. The Settings request waits for the hook, so keep it short.

```python
from pathlib import Path

def record_mode(old: str, new: str, scope: str) -> None:
    Path("steering-mode.txt").write_text(new)

steering_mode = vis.Setting(
    id="steering_mode",
    label="Steering mode",
    type="enum",
    default="vibe",
    choices=["vibe", "control"],
    scopes=["global", "project"],
    on_change=record_mode,
)
```

If the hook raises an error, the value stays saved. The app and the TUI show the error, and the
gateway log records it. Vis does not call the hook when the extension loads.

To show a preference that another tool saves, give `read` and `write`. Vis then keeps no copy of
the value:

- `read(root)` returns the saved value for the project directory `root`, or `None` if the tool
  has no saved choice. `root` is the project that holds your extension, as `vis.project_root()`
  gives it.
- `write(root, value)` saves a change from Settings. `None` removes the saved choice.

Such a setting needs `scopes=["project"]` and a `read` that is not `async`. It cannot have
`on_change` or `is_secret`.

```python
import json
from pathlib import Path

def saved(root: str) -> dict:
    file = Path(root) / ".linter.json"
    return json.loads(file.read_text()) if file.exists() else {}

def read_strict(root: str) -> bool | None:
    return saved(root).get("strict")

def write_strict(root: str, value: bool | None) -> None:
    choices = saved(root)
    if value is None:
        choices.pop("strict", None)
    else:
        choices["strict"] = value
    (Path(root) / ".linter.json").write_text(json.dumps(choices))

strict_lint = vis.Setting(
    id="linter_strict",
    label="Strict lint",
    default=False,
    scopes=["project"],
    read=read_strict,
    write=write_strict,
)
```

Vis calls `read` for each Settings request, each turn, and each `value()`, so keep `read` short.
A change that the tool makes shows at the next read.

- If `read` raises an error or returns a value of another type, Vis uses the default and the
  gateway log records the error.
- In your code, `value()` calls `read` directly, so an error of `read` reaches your code.
- If `write` raises an error, Settings shows it, the gateway log records it, and nothing
  changes.
- A `toggles:` value in a Vis configuration file has no effect on this setting.

`value()` reads the current value in every callback, also outside a turn, such as in a
background thread. In a session, it uses the session's settings. Without a session, a project
extension's setting resolves at project scope, as Settings shows it there. Project values, such
as `toggles:` in the project's `vis.yml`, win over global values if `scopes` includes `project`.
Vis reads a `vis.yml` edit after `/reload`. A file edit does not call `on_change`.

The app and the TUI show your settings under **Extensions**, in a group with your extension's
name. That group also holds the extension's engine choice and its packaged skills. [Extension
settings](configuration.md#extension-settings) describes the `project` and `global` tags, the
**Reload** control, and the group of an extension that fails to load. Settings does not run your
code when it opens.

`scopes` accepts a non-empty subset of `global`, `project`, `group`, and `session`, without
duplicates. The default permits only `global`. Vis checks the scope on each save and each read
of a value, in Settings and elsewhere. A missing override inherits, but `False` does not. A
reload or a temporary removal of an extension keeps its stored choices.

For a project extension, `global` means the whole project, and its settings show only in the
settings of that project.

For inheritance, submission timing, engine activation, and access limits, read [scoped
settings](configuration.md#project-group-and-session-settings).


## Tools

```python
vis.Symbol(fn_or_object, name=None, tag="observation", is_hidden=False, activity=None)
```

`name=None` uses the name of the function. An object namespace needs an explicit `name`. `tag`
is `observation`, `mutation`, `verification`, or `external`, as [Report checks and outside
effects](#report-checks-and-outside-effects) describes. `is_hidden=True` removes a callable from
the discovery that the model sees. The callable stays callable, and the flag authorizes nothing.

Every exported callable needs a nonblank docstring and an explicit [Activity
presentation](#activity-presentation). `activity=None` leaves only the execution evidence of the
engine and creates no result view. Do not ship a tool that way.

A source function can use `def` or `async def`. The model calls its proxy with `await` in
`python_execution`. The trusted worker awaits an asynchronous result before it returns, and the
execution watchdog parks during that wait. Each async call uses its own event loop, so create
and close loop-bound resources within that call.

Arguments and completed results stay Python values. An exception is an ordinary tool failure.
The [execution boundary](#filesystem-and-processes) decides which values can cross to the
sandbox.

### Typed catalog

`vis.Catalog(symbols)` accepts the `Symbol` declarations that `Extension` accepts. It takes a
snapshot of the public callable contracts, without registration or IO. It rejects duplicate
declaration names and does not list hidden tools. Build the snapshot again after a declaration
changes.

| API | Result |
| --- | --- |
| `catalog.spec()` | Tuple of top-level `ToolSpec` or `NamespaceSpec` values |
| `catalog.spec("store.items.read")` | The `ToolSpec` of one callable |
| `catalog.spec("store.items")` | `NamespaceSpec(name, members)`: the fully named `ToolSpec` of every public callable descendant |
| `catalog.help("store.items.read")` | `HelpDocument(tool, text)`, generated from the registered documentation |
| `catalog.help("store")` | Generated reference for all public descendants |

The records have these fields:

- `ToolSpec` has `version`, `name`, `tag`, `description`, `signature`, `parameters`, and
  `returns`.
- `ParameterSpec` has `name`, `kind`, `type`, `required`, `has_default`, `default_is_none`, and
  `default_source`, which is Python source text or `None`.
- `TypeSpec` has `kind`, `name`, `description`, `arguments`, `fields`, `values`, `variadic`, and
  `sequence_field`.
- Result fields use `FieldSpec`.

The records are frozen and slotted, and each nested collection is a tuple. The adapter keeps no
host default object and no callable, and copies a literal default as source text.
`sequence_field` is `None` unless the record declares [field-backed sequence
behavior](#field-backed-sequences). The portable `.contract` has this key only for such a
record.

`spec(None)` lists the roots. Another non-string name raises `TypeError`. An unknown or hidden
name raises `ValueError`. `help` needs a string. A lookup never configures, authenticates,
dispatches, or runs the described operation. `Catalog.spec` and `Catalog.help` have their own
end-only Activities, so a catalog can itself be an object namespace.

`vis.testing.assert_catalog(catalog, names=..., mutations=...)` checks the catalog against the
public registered callable names and the expected mutation names. It also checks that help
matches each signature and that no nested type is unresolved. It does not call tools, validate
argument values, or prove cancellation. Read the [tested recipe and boundary
checks](extension-design.md#typed-catalog-and-generated-help).

### Return typed objects

A dict is enough for a simple result. When shape and meaning matter, use a frozen dataclass with
annotated public fields. The [tested example](extension-design.md#describe-structure-once) shows
field descriptions with `Annotated`. Private fields do not cross into the sandbox.

Derived data and a text report can cross with the fields:

- Each public `@property` or `functools.cached_property` of the dataclass becomes a field of the
  record. Vis reads it one time when the tool answers, and leaves out a property that raises.
- If the class defines its own `__str__`, that text becomes the `str` of the record. Then
  `print(result)` shows your report, and `repr(result)` still shows the fields.

```python
@dataclass(frozen=True)
class Status:
    env: str
    phases: tuple[int, ...] = ()

    @property
    def next_phase(self) -> int:
        return len(self.phases) + 1

    def __str__(self) -> str:
        return f"{self.env}: next phase {self.next_phase}"
```

In the sandbox, `status.next_phase` is a field and `print(status)` shows `DEV04: next phase 1`.
Another method, such as `status.text_report()`, stays in your process. Expose its value as a
property or as `__str__`, or expose it as a tool. When the record goes back to a tool as an
argument, Vis ignores the property fields and calls your constructor with the declared fields.

A generated record is a read-only dictionary of its fields, nested records included:

- `result.url` and `result["url"]` both read a field.
- An unknown name raises `KeyError` for `result["missing"]` or `AttributeError` for
  `result.missing`. The error lists the available fields. So `hasattr` and
  `getattr(result, name, default)` keep their usual meaning.
- `result.keys()`, `result.get("url")`, `dict(result)`, and `json.dumps(result)` read the
  fields. An assignment raises an error.
- By default, a record iterates over its field names and `len(result)` counts its fields. A
  non-string index raises `TypeError`.
- A dictionary name keeps its dictionary meaning. Read a field named `items`, `keys`, or `get`
  as `result["items"]`.

### Accept records as arguments

A tool can take a record that another tool returned. Annotate the parameter with the dataclass,
and your function receives an instance of that class:

```python
from dataclasses import dataclass


@dataclass(frozen=True)
class TriggerResult:
    http_code: int
    job_path: str


def trigger(job_path: str) -> TriggerResult:
    """Start a build."""
    return TriggerResult(201, job_path)


def status(result: TriggerResult) -> str:
    """Report the job of a started build."""
    return f"{result.job_path}: {result.http_code}"
```

Sandbox code can pass the result straight back, as in
`await status(await trigger("docs/build"))`. Arguments cross the sandbox boundary as JSON, so
the record travels as its class name and its public fields. Before your function runs, Vis
rebuilds the record with the constructor of your dataclass. Vis does the same for a dataclass
from sandbox code, a `types.SimpleNamespace`, or a dict with the field names of the record.
These rules apply:

- Vis also rebuilds records inside `Optional`, unions, `list`, `tuple`, `set`, `frozenset`, and
  `dict` annotations, and in nested record fields.
- In a union, a record whose class name matches a member becomes that member. Another value uses
  the first member that fits, in the order that you declared.
- If a dict fits two record members of a union, the call fails. For example, `{}` fits both
  `Work` and `Personal`. Pass `Personal()` or a record that a tool returned.
- The dict can leave out fields that have defaults. Vis skips fields declared with
  `field(init=False)`, because the constructor sets them.
- A value that does not match the record fails the call before your function runs. Examples are
  a dict with unknown keys or without a required field. The error names the argument path, such
  as `chats[1].category`, and the expected and received types. It never shows the values.
- The error also names the sha256 of the extension source that handled the call. Compare it with
  `shasum -a 256` of your file to learn if Vis runs old code.
- An error from the constructor, for example in `__post_init__`, fails the call.
- Private fields, whose names start with `_`, do not cross the boundary. Give them defaults if
  Vis must rebuild the record.
- After a reload of the extension, Vis rebuilds an earlier record as the reloaded class, so
  `isinstance` checks keep working.
- Vis resolves the annotation as it does for [tool
  contracts](#supported-types-and-unresolved-annotations). If Vis cannot resolve the class, the
  argument stays a dict.

### Field-backed sequences

If your result wraps a collection, `@vis.sequence(field="results")` lets sandbox callers iterate
it directly. Apply the decorator **outside** `@dataclass`:

```python
from dataclasses import dataclass

import blockether.vis.extension as vis


@dataclass(frozen=True, slots=True)
class Page:
    title: str


@vis.sequence(field="results")
@dataclass(frozen=True, slots=True)
class PageList:
    results: tuple[Page, ...]
    total: int
```

When a tool returns `PageList`, sandbox callers can use `for page in result`, `len(result)`,
`if result`, `result[0]`, `result[-1]`, and `result[1:]`. `result.results` and
`result["results"]` still give the received list, and nested items keep their generated record
types. A slice returns a list. The wrapper supports no field or item assignment. Its backing
list is ordinary local data.

The field must be a public dataclass field that holds a **built-in list or tuple** at return
time. Vis rejects strings, mappings, sets, generators, and subclasses of list or tuple, and does
not consume lazy iterators. Length and truth describe the received items, not a `total` field.
Iteration never fetches another page. A record with no received items is false. So
`json.dump(result, file)` writes `{}` for it, while `json.dumps(result)` writes its fields.

The declaration adds metadata to your class, not methods. Keep the methods that you need when
you call the class outside the sandbox.

Each type must opt in explicitly, subclasses included. An existing `__iter__` method or a field
named `results` does not opt in. To adapt a third-party value without a change to its type, copy
it into a decorated dataclass.

The backing field is the `sequence_field` of the `.contract` and of the [typed
catalog](#typed-catalog). The generated `doc()` text describes the sequence operations.

### Object namespaces

`vis.Symbol(Greeter(), name="greet")` exports `greet.hello(...)` in the [packaged
example](extension-design.md#keep-the-entrypoint-small). Public methods become tools, and object
attributes become nested namespaces. Vis leaves out names that begin with `_`. It rejects public
scalars, modules, classes, cycles, and repeated object references, and names their path.

`vis.method(tag="observation", is_hidden=False, activity=None)` overrides the metadata of one
method. Metadata never changes the Python signature of the method. If you keep a tag in a
variable or a helper parameter, annotate it with `vis.SymbolTag`.


### Activity presentation

An Activity explains a tool call to the person who follows the session. Declare it at each tool
binding: `vis.Symbol(fn, activity=vis.Activity(label="Run checks", render=render_checks))`. In
an object namespace, put `@vis.method(activity=...)` on **each exported method**, nested methods
included. Vis rejects an Activity on the namespace.

`vis.Activity(presenter="tests", label="Run checks", render=render_checks)` selects a presenter
and a synchronous callback. `presenter` only classifies the operation, and its default is
`"generic"`. Only your `render` callback draws content. The return value of the tool stays
available to Python.

Put the action in the headline, the target or the outcome in the summary, and the evidence in
the content. Use sentence-case labels such as "Read file", "Search files", or "Check service
health". Do not use `search_files`, a qualified method name, an object representation, or an
all-caps sentence. Keep proper names such as GitHub and acronyms such as SDK, and the case of
filenames, code, commands, and returned content. Use consistent terms, without profanity or
vulgarity. [Extension design](extension-design.md) gives the reasons.

The host shortens filesystem paths in Activity headlines, summaries, plain text, headings, and
table cells. A path in the session workspace becomes relative. Another path under the home of
the host user uses `~/…`. Other absolute paths stay as they are. The rule covers built-ins,
Python methods, callbacks, and published sections, before the TUI or the Companion renders them.
Tool arguments, return values, resource identities, code, Markdown, and diff bodies keep their
original paths.

Set `show_start=False` for a fast local action, such as a file read, a patch, or a cache lookup:

```python
activity = vis.Activity(
    label="Read record", show_start=False, render=render_record
)
```

With **end-only presentation**, the person sees the result without an earlier running row:

- Vis still records the start and end order, the duration, errors, and cancellation.
- The callback skips `start` and runs on `success` or `failure`.
- Vis saves content that the tool publishes during the call and shows it when the call ends,
  also after a failure or a cancellation.
- `None` from a start callback does not hide the running row. Only `show_start=False` hides it.

The final presentation must make sense without earlier progress. Name the operation and the
target, state the outcome, and keep useful counts, errors, or changes. An empty result says what
was absent, such as "No jobs reported". Label partial lists and excerpts, so that nobody reads
them as complete results.

A tool can return normally when the external operation failed, as a workflow watcher does for a
failed workflow. Show that failure in the final summary. "Completed" alone hides it. The [CI
report example](extension-design.md#show-a-ci-report-without-hiding-failures) does so.

Keep the default `show_start=True` for work that a person waits for: tests, builds, network
requests, file transfers, or user input. Publish an intermediate update only for a change that
matters. Choose the policy for each binding, not from a fixed duration threshold. A remote read
can need progress when a local read does not. Internal counts include every observed call.

The callback receives the keyword arguments `phase` (`start`, `success`, or `failure`), `args`,
`kwargs`, `result`, and `error`. It returns
`vis.ActivityPresentation(headline, summary, content=(), sections=())`, or `None` to keep the
current presentation. The engine supplies identity, state, timing, errors, and observed file
changes. A callback must not invent them. A callback error does not change the return value or
the exception of the tool.

The headline and the one-line summary stay visible when the step is collapsed. Only the content
waits behind the disclosure. Do not show a serialized result or a second output preview.

Build content from typed blocks: `ActivityText`, `ActivityHeading`, `ActivityMarkdown`,
`ActivityCode`, `ActivityDiff`, `ActivityTable`, `ActivityProgress`, `ActivityTerminal`,
`ActivityImage`, `ActivityVideo`, `ActivityAudio`, and `ActivityFile`. A media block refers to
an existing attachment. `ActivitySection(headline, summary, content=())` groups related results
and cannot hold another section. Each section has its own disclosure, below its headline and
summary.

`vis.publish_activity(presentation)` replaces the current snapshot while a tool runs. Empty
content clears the body.

### Link receipts for one operation

When a tool returns a persistent handle and later calls check its progress, pass the same
`handle_id` in each `ActivityPresentation`. Successive findings then share one current card:

```python
import blockether.vis.extension as vis


def check_visual_differences(handle: str, finding: str) -> str:
    """Report one finding for the named visual comparison."""
    vis.publish_activity(
        vis.ActivityPresentation(
            "Checking visual differences",
            finding,
            (vis.ActivityText(finding),),
            handle_id=handle,
        )
    )
    return finding


vis.register_extension(
    vis.Extension(
        name="visual-comparisons",
        description="Report visual comparison findings.",
        alias="visual",
        symbols=[
            vis.Symbol(
                check_visual_differences,
                activity=vis.Activity(label="Check visual differences", show_start=False),
            )
        ],
    )
)
```

Call `visual.check_visual_differences("comparison-7", "One difference")` and then
`visual.check_visual_differences("comparison-7", "No differences")` in the same Python form.
Activity shows the latest finding, with the earlier finding in its details. The expanded receipt
still shows both calls. A different handle stays separate. The handle links calls only within
one extension and one form.

`handle_id` is an optional, nonblank, single-line string of at most 256 characters and 512 UTF-8
bytes. Use a stable, non-secret value that is unique to the operation. Do not use the label or
the arguments, and do not reuse the value for unrelated work. Omit it for a standalone call. It
identifies a receipt and does not replace tool arguments or results.

The engine keeps the newest outcome current, keeps the earlier distinct details and errors,
and counts and saves every call in history. Shell handles use the same grouping, with command-specific output and
exit reconciliation.

To make summary links clickable in the app and the terminal, pass `summary_format="markdown"` to
`ActivityPresentation` or `ActivitySection`:

```python
vis.ActivityPresentation(
    "Find issues",
    "Found [#252](https://github.com/Blockether/vis/issues/252)",
    summary_format="markdown",
)
```

A Markdown summary supports inline formatting and HTTP or HTTPS links. It does not support
images, raw HTML, or block layouts. Links work with the details collapsed or expanded. Omit
`summary_format`, or use `"inline"`, to show the summary literally.

In both clients, a person can press the files that a step reports: the resources that it read
and the files that it patched. The press opens the file in the editor on the machine that runs
the session. Report real paths in your file resources. A label that only looks like a path opens
nothing.

A table row can name its own file. Pass `ActivityTable(headers, rows, paths=[...])` with one
entry for each row. The first cell of the row becomes the press target and keeps your words. The
press and its hover title carry the whole path. Use an empty string for a row that names no
file, such as a directory or a total. Vis rejects the block unless the list is exactly as long
as `rows`.

A file that Vis finds by itself is pressable in the same way, such as the file of a failed test
in a `run_tests` result.

Each headline and summary is one line of at most 512 UTF-8 bytes, Markdown source included. Put
the complete result in content blocks and non-nested sections. Vis does not cut their text,
table rows, or block counts to fit a presentation budget. The [Activity
schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/activity.json)
has the exact portable limits and block shapes.

In both clients, test the declared start visibility, success, failure, cancellation, empty
results, replacement, redaction, and disclosure. The [tested greeter
entrypoint](extension-design.md#keep-the-entrypoint-small) shows the callback next to the
registration.

Vis saves every call and its complete presentation in the session, without a size or count
limit. The app and the terminal load small pages as you scroll. Search, Copy all, and Export
cover the full history, and a restart does not remove it. To read or export the history from
your own program, use the [Sessions API](sessions-api.md#read-activity-history).

### Report checks and outside effects

The `tag` of a tool tells Vis what kind of work each call did. Each Activity counts its steps in
one line, such as `1 mutation · 6 observations · 2 verifications`. Short summaries use `ver` for
verification.
| Tag | Use it for |
|---|---|
| `observation` | Reads that change nothing, such as searches and status queries. |
| `mutation` | Changes to local state, such as writing files or starting a process. |
| `verification` | Checks of the work, such as tests, lint and CI status. |
| `external` | Work that reaches people or systems outside the session, such as messages, publications and remote hosts. |

A tag describes the operation. It grants no permission and enforces no policy.

A check can finish without an error and still find problems. Set `verdict` on the top-level `ActivityPresentation` to say what the
check found:

```python
vis.ActivityPresentation(
    "Run tests",
    "38 passed, 2 failed",
    verdict="failed",
)
```

`verdict` is `"passed"` or `"failed"`. A section does not accept it. Leave it out for a call
that checks nothing.

The verdict describes the result of the check, not the execution state of the call. A failed or
cancelled call shows that state on its own step. Verification counters count calls, not
individual tests, and have no failure count. Show the result in the summary and the content of
the step.

To show a command and its output, use an `ActivityTerminal` block:

- Give the command as a list of stages. Start each later stage with its operator, such as `&&`
  or `|`.
- Give each output line as an `ActivityTerminalLine` with its stream, `stdout` or `stderr`. A
  plain string is a `stdout` line.
- Set `omitted_lines` when you keep only the last lines of a long output.

Two optional fields of the top-level `ActivityPresentation` complete the head line of the step:

- `outcome` is one line that tells what the work produced, such as `41 passed`. It shows after
  the headline when `summary` is empty.
- `aside` is quiet context, such as the working directory and the exit code. It shows at the end
  of the head line.

```python
vis.ActivityPresentation(
    "npm test && npm run lint",
    "",
    content=(
        vis.ActivityTerminal(
            ("npm test", "&& npm run lint"),
            lines=(
                "41 passed",
                vis.ActivityTerminalLine("warning: slow test", stream="stderr"),
            ),
            exit=0,
        ),
    ),
    outcome="41 passed",
    aside="~/vis · exit 0",
)
```

The Companion app and the TUI show each stage on its own line and show `stderr` lines in the
error color. A closed step that still runs shows its last three output lines. Copy text starts
with `$ ` and the command.


## Prompts and discovery

Use `prompt` to say in a few lines **when** to choose this extension and which public names to
search. For the packaged greeter, a suitable value is:

```text
Use greet.hello to generate greeting text; it never sends messages.
Discover the tools with apropos(r"^greet\.") and read doc("greet.hello") before calling.
```

Assign the text to `Extension.prompt`. It is not Python code, and registration does not run it.
A prompt callable computes text each turn. It must not log in, start background work, or repeat
the whole API. `description` supplies the extension summary also without a `prompt`.

`apropos(pattern)` filters public symbols by a regular expression and ignores case. A tool
matches by its name or by the first sentence of its docstring, which its row shows. Name matches
come first. A bundled skill also matches by its description and its headings. `doc(name)` or
`doc(hit)` reads the complete document.

The signature and the resolved annotations give the call structure. The docstring and the
`Annotated` descriptions give the meaning. Keep a useful first-line summary for discovery, and
do not repeat the signature, the defaults, or the return schema. Put an optional multi-step
procedure in a [skill](skills.md). Prompt text and skills enforce no permission. For a guard,
use an explicit policy mechanism, such as an [op hook](#op-hooks).

## Tool contracts

`Symbol.contract` returns new portable data without registration or a tool call. In the sandbox,
each callable has its contract as an attribute, for example `greet.hello.contract`. A change to
that local dictionary changes neither the declared tool nor its permissions.

| Contract | Fields |
| --- | --- |
| Callable | `version`, `name`, `tag`, `description`, `signature`, `parameters`, `returns` |
| Namespace | `version`, `name`, `members` with full public member names |
| Parameter | Name, parameter kind, `required`, `has_default`, `default_is_none`, `default_source`, and type description |

`doc()` renders the contract: signature, parameter kinds, required or default status, return type
and fields, mutation tag, and description. The `signature` text is the full Python signature,
for example `(name: str, /, *, loud: bool = False) -> 'Results'`. A method with no arguments has
the signature `()`, and no metadata is missing.

The agent reads the arguments with `inspect.signature(tool)` and reads `doc("tool")` only for
the meaning. So put preconditions, side effects, safety limits, units, and retry behavior in the
docstring. For nested types, the agent selects fields from `tool.contract`. `Catalog.spec(name)`
gives the same metadata as an SDK `ToolSpec`.

The contract is documentation, not JSON invocation or runtime validation. Python binds the
arguments, and your code validates the domain constraints. You cannot override the schema or the
signature by hand. The exact portable shape is the [symbol
schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/symbol.json).

### Compact model schemas

When parameter or return types contain records, `doc()` defines each shared model one time and
refers to it by name. So a type such as `ToolSpec | tuple[ToolSpec, ...]` has one `ToolSpec`
field definition, not one for each union branch. Different model shapes with one name get
numbered display labels.

The generated **Model schemas** section has a budget of 2,048 characters, with its heading and
its abbreviation notice. Vis visits the definitions breadth-first and never cuts a field line in
the middle. The budget does not shorten the description, the signature, the parameter kinds and
default markers, the return type, or the effect tag of the callable.

The notice of an abbreviated section points to the `.contract` of the callable. In Python, walk
its `parameters` or `returns` branch and print only the leaf fields that you need. The complete
type metadata is there and in `Catalog.spec(name)`. The budget changes neither the contract nor
the returned values.

### Defaults and introspection

An ordinary declared default needs no opt-in. `False`, `200`, `"1h"`, and `()` appear in
`tool.contract`, `Catalog`, the generated help, `doc()`, and `inspect.signature()`.

Vis renders exact built-in literals: `None`, `Ellipsis`, booleans, integers, finite floats and
complex numbers, strings, bytes, and tuples, lists, dictionaries, and sets of such values. Vis
escapes a string as Python source and does not filter it by its content or its parameter name. A
declared default is public metadata. Resolve a credential inside the tool, not in a parameter
default.

`default_source` keeps the Python spelling and the type of the literal without a host object.
Its value is source text, not the default.

| Python declaration | `has_default` | `default_is_none` | `default_source` | Rendered default |
| --- | --- | --- | --- | --- |
| Required argument | `False` | `False` | `None` | No default |
| Argument with `= None` | `True` | `True` | `"None"` | `None` |
| Argument with `= False` | `True` | `False` | `"False"` | `False` |
| Argument with `= "1h"` | `True` | `False` | `"'1h'"` | `'1h'` |
| Argument with `= ()` | `True` | `False` | `"()"` | `()` |
| Opaque or unrepresentable default | `True` | `False` | `None` | `...` |

Opaque objects, subclasses, nonfinite numbers, and cyclic containers stay `...`. So does a value
outside the inspection budget of 4,096 source characters and eight container levels. Vis never
calls a custom `repr()` method or a factory to render a default.

A placeholder `...` means that you can omit the argument. Do not pass `Ellipsis`. An omitted
argument gets its original default, and inspection never puts a copied literal into a call.
`*args` and `**kwargs` are not required, although they have no default.

Vis also describes dataclass-field defaults and factories, but exports no value and runs no
factory. Explain in prose what an omitted argument does in context. Read [documenting
defaults](extension-design.md#document-default-behavior).

| Sandbox inspection | Supported result |
| --- | --- |
| `tool.contract` | Portable parameter and result types, fields, and documented meaning |
| `doc("tool")` | Readable rendering of that contract |
| `inspect.signature(tool)` | Names, parameter kinds and annotations, representable literal defaults or `Ellipsis` placeholders, and the return annotation when declared |
| `tool.__annotations__` | Annotation objects resolved statically from the signature text. Builtins, `typing` and `collections.abc` names, unions and subscripts become the real objects. Record, opaque and unresolved names stay quoted forward-reference strings such as `'Results'` |
| `typing.get_type_hints(tool)` | Resolves the same annotations. Pass `localns` for forward-reference strings, or it raises `NameError` for them |
| `tool.__signature__` | Not supplied (`inspect` follows `tool.__wrapped__`) |

Engine tools also show their registered option keys as keyword-only parameters. For example,
`inspect.signature(council.publish)` names the required `kind` argument, and
`inspect.signature(grep)` names `query`, `paths`, and the other search options. An optional key
with an undisclosed default shows `Ellipsis`.

A host signature describes the canonical named call, not every accepted overload. Positional
calls and options-dictionary calls still work, and `Signature.bind()` cannot validate all of
them. Extension functions and methods keep their Python parameter kinds, positional-only and
variadic arguments included. The remote tool schemas of MCP stay separate from the `server`,
`tool`, and `args` parameters of the dispatcher.

The host classes and their identity do not cross the sandbox boundary. A type name describes a
host annotation. For records passed back to tools, read [Accept records as
arguments](#accept-records-as-arguments).

Sandbox sequences are list-like. Use `len()`, an index, or iteration, not
`isinstance(value, tuple)`. The `.contract` of a callable is a dictionary. To find types, select
`['parameters']` or `['returns']`. Do not try to rebuild host annotations.

### Supported types and unresolved annotations

Descriptions cover every parameter kind, return types, dataclass fields, unions, common
containers, `Literal`, and string metadata in `Annotated`. A recursive record uses a reference
and does not expand without end.

- `tuple[T, ...]` has `variadic: true` and one type in `arguments`. A fixed-length tuple keeps
  each item type and omits `variadic`.
- `Name (unresolved)` means that Vis could not resolve the annotation safely. Vis does not
  guess, import, or evaluate an expression to find a type.
- `Name (opaque)` means that Vis knows the class but has no description of its structure. Both
  labels also appear in nested types and record fields.

Vis loads each extension file as if it began with `from __future__ import annotations`.
Annotations stay strings, and Vis resolves them against the names of the module. So define
result classes at module level. A local forward reference that the defining module lacks stays
unresolved.

A module that your extension file imports follows normal Python rules. Add
`from __future__ import annotations` to each helper or package module that defines tools. When
tests import the extension file directly, keep the import in that file too. Without it, Python
3.14 defers annotations into functions that can run code, also when Vis asks for strings. Vis
then reports them as unresolved and does not evaluate them.

### Cross-module decorators

A `functools.wraps` chain resolves annotations in the module that defines the wrapped function,
also for bound namespace methods and qualified names such as `models.Result`. A cyclic
`__wrapped__` chain raises `ValueError`. These two complete modules show cross-module wrapping
and a variadic tuple result:

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

`vis.Symbol(Tools(), name="tools").contract` expands `Result.text` below the variadic tuple. A
call without `text` uses the public string `"ready"`. When you bind `Tools.read`, declare its Activity in the entrypoint, as in the
[greeter example](extension-design.md#keep-the-entrypoint-small). This local operation can use
`show_start=False`.


## Slash commands

```python
vis.SlashCommand(name, run, doc=None, usage=None, saveable=False)
```

`run(ctx)` receives
`{"channel", "args", "raw", "session_id", "workspace_root", "project_root"}`. It returns
`vis.ok(title, body=None, data=None)`, `vis.err(title, body=None, data=None)`, or a plain
string, which counts as an ok title. `body` is Markdown.

`args` is the list of words after the command. Vis splits the text at spaces, and quotes do not
group words. `raw` is the full text of the command, with its `/`. For example,
`/deploy staging now` gives the `args` `["staging", "now"]`.

Vis runs `run` in the session that sent the command, as it does for a tool call. In `run`,
[`vis.workspace_root()`](#workspace-root) returns the directory of that session.

Set `saveable=True` when the command text is real prompt content, such as a goal. The terminal then keeps the full command in its prompt
history, so **↑** recalls it. The default is `False`. Vis does not save a control command, such
as `/reload`, unless its definition sets the flag. The built-in `/goal` command is saveable.

## Automations

```python
vis.Automation(key, run, triggers, name=None, doc=None, session="temporary", push=True, enabled=True)
```

An automation calls `run` on a schedule. A run calls no model, sends no provider request, and
uses no tokens. Declare automations in `vis.Extension(automations=[...])`. Vis adds each one to
the automations of the gateway when the extension loads.

```python
def sync(ctx):
    """Send the new session exports to the Iceberg table."""
    exported = upload(vis.sessions.export())
    return f"Sent {exported} sessions." if exported else None


vis.Extension(
    name="iceberg",
    description="Iceberg export.",
    automations=[
        vis.Automation(
            key="iceberg-sync",
            run=sync,
            triggers=[{"kind": "every", "seconds": 3600}],
            session="persistent",
        )
    ],
)
```

The arguments of `vis.Automation` have these rules:

- `key` identifies the automation in the extension. It has 1 to 64 characters: lowercase
  letters, digits, `.`, `_`, and `-`. It starts with a letter or a digit. Each key is unique in
  one extension.
- `triggers` holds 1 to 8 triggers of the kinds `cron`, `every`, and `once`.
  [Triggers](automations.md#triggers) describes them. An extension automation cannot have a
  `webhook` trigger.
- `name` shows in lists and notifications. The default is `key`.
- `doc` is the task that the app shows. The default is the docstring of `run`. It must not be
  empty.
- `session` is `"temporary"` or `"persistent"`. A `temporary` run gets a new session that Vis
  deletes after the run. A `persistent` run uses one session that Vis keeps for all runs. Each
  session works in the project folder of the extension.
- `push` sends a phone notification for each run that is not silent.
- `enabled` applies only when Vis adds the automation for the first time. Later loads keep the
  pause state that a person set.

`run(ctx)` receives `{"automation", "run", "session_id", "workspace_root", "project_root"}`.
`automation` has `id`, `name`, and `key`. `run` has `id`, `trigger_kind`, and `request`. Vis
calls `run` in the session of the run, as it does for a slash command. So `vis.session`,
`vis.tools`, `vis.sessions`, and `vis.run(...)` work. To ask a model, call `vis.run(...)`.

The return value sets the result of the run:

- `None` completes the run without an answer and without a notification, as `[SILENT]` does.
- A text, or `vis.ok(title, body=None)`, completes the run. The text, or the body or the title,
  is the answer.
- `vis.err(...)` or an exception fails the run with that text as its error.

A run has these limits:

- A run cannot ask a person. A question or a form fails the run at once.
- A run that works for more than 180 minutes fails. This limit is the only time limit of `run`.
  A run does not need to give output while it works.
- When a person cancels the turn of a run, Vis stops `run` and the run ends as cancelled.
- Each run adds one turn without a model to its session. So a `persistent` session shows the
  history of the runs.

The extension owns the definition. A person can only run, pause, resume, or delete the
automation. When the extension no longer declares a key, Vis deletes that automation at the next
load. A deleted automation comes back at the next load while the extension declares it.
[Automations from extensions](automations.md#automations-from-extensions) has the other rules.

## Key bindings

```python
vis.Keybinding(key, command=None, action=None, label=None, group=None, run=None)
```

The person presses Ctrl+X, a group
key, then your key. By default, the key is in the extensions group, so the person presses Ctrl+X
e, then the key. Declare the bindings in `vis.Extension(keybindings=[...])`.

```python
vis.Extension(
    name="release",
    description="Release tools.",
    slash_commands=[vis.SlashCommand("release", run_release)],
    keybindings=[
        vis.Keybinding("R", command="/release", label="Start a release"),
        vis.Keybinding("N", action="new-session"),
    ],
)
```

The arguments of `vis.Keybinding` have these rules:

- `key` is one printable ASCII character from `!` to `~`. A letter is case-sensitive.
- Give exactly one of `command`, `action`, and `run`.
- `command` is a slash command line that starts with `/`, at most 200 characters. The terminal
  runs it as if the person typed it and pressed Enter. Give it with `label`.
- `action` moves a built-in terminal verb to `key` in the group of the verb, for example
  `new-session`. [Keys from extensions](keyboard-shortcuts.md#keys-from-extensions) lists the
  verb ids.
- `run` is a Python function that the gateway calls when the person presses the key. Give it
  with `label`. Read [Run a function on a key](#run-a-function-on-a-key).
- `label` holds at most 40 characters and shows in the key menu and the command palette.
- `group` holds at most 24 characters and selects the group of the key. A built-in group name,
  such as `Session` or `Model`, puts the key in that group. Case does not matter. Without
  `group`, a `command` or `run` binding uses `Extensions`.
- Any other `group` name makes a group of your own under Ctrl+X e. Its key is the first free
  letter of its name. Names that differ only in case share one group.

```python
keybindings=[
    vis.Keybinding("s", command="/release start", label="Start", group="Release"),
    vis.Keybinding("p", command="/release publish", label="Publish", group="Release"),
]
```

Here Ctrl+X e r opens the `Release` group, and then s starts the release. A key that you bind
directly under Ctrl+X e keeps its letter. A group of your own then takes the next free letter of
its name.

The terminal refuses a binding whose key is reserved or taken in its group, and shows one
notice. An [`Agent` extension](python-sdk.md) of your own program cannot declare key bindings.
`session.keybindings()` returns the bindings of a session, as [Sessions
API](sessions-api.md#read-the-key-bindings) describes.

### Run a function on a key

Use `run` when a key must do work without a new turn in the transcript.

```python
def deploy(ctx):
    vis.notify("Deploy started", "info")
    # ... do the work ...
    return vis.ok("Deployed to staging")


vis.Extension(
    name="deploy",
    description="Deploy tools.",
    keybindings=[vis.Keybinding("D", run=deploy, label="Deploy to staging")],
)
```

The terminal stays free while `deploy` runs. The function
follows these rules:

- The function gets one dict with `key`, `group`, `channel`, `session_id`, `workspace_root`, and
  `project_root`.
- Return `None` for no message, or a text for a short success message.
- Return `vis.ok(title, body)` or `vis.err(title, body)` for a message with a level.
- An exception shows as an error message with the extension name.
- Call `vis.notify` for progress while the function runs. Each client of the session shows it.

## Session apps

```python
vis.SessionApp(key, title, open, icon=None, surfaces=None)
```

An app of an extension exists in a session only after the code of the extension opened it there.
A session app shows in each session from the start of that session, before the extension runs.
When the person opens the app, Vis starts the Python of the session and calls `open`. Declare
one session app in `vis.Extension(session_app=...)`.

```python
import blockether.vis.extension as vis


def open_board(app):
    vis.attach(
        vis.ui.app("Board", vis.ui.thread("general"), key="board", settings_name="board"),
        on_event=lambda event: None,
        on_reload=open_board,
    )


vis.register_extension(
    vis.Extension(
        name="board",
        description="A board for each session.",
        session_app=vis.SessionApp("board", "Board", open_board, icon="chat"),
    )
)
```

A session app follows these rules:

- `open` is a function at module level. Vis finds the function by its name in a new process.
- `open` gets one dict with `key`, `view_id`, `session_id`, `workspace_root`, and `project_root`.
- The first app that the extension opens in the session replaces the listed app and keeps its
  `view_id`. An open pane then shows the real app.
- The session lists the app again after the person closed the real app.
- `icon` and `surfaces` take the values of `vis.ui.app`. Give the same values to the real app.
- `key` names the settings of the person for the listed app. Give the real app
  `settings_name=key`, so both use the same settings.
- While a turn runs in a session that has no Python yet, the app does not open. The app opens
  when the extension opens it in that turn, or when the person opens it after the turn.

## Op hooks

```python
vis.OpHook(ops, fn, phase="before")
```

`ops` names operations such as `"patch"`, `"shell"`, or `"python_execution"`. A hook also works
on a tool of another extension or of an MCP server. Write the name that `python_execution`
calls. The hook receives the name of the call as `call["op"]`, never a pattern.

| Target | Name in `ops` | `args` that the hook receives |
| --- | --- | --- |
| Extension tool | `"clj.repl_start"` | The positional arguments, then one dict with the keyword arguments. `clj.repl_start("app", aliases=["dev"])` gives `["app", {"aliases": ["dev"]}]`. |
| MCP tool | `"linear.get_issue"` | One dict with the arguments. `linear.get_issue(id="ENG-1")` gives `[{"id": "ENG-1"}]`. |
| MCP prompt | `"linear.prompts.<name>"` | One dict with the arguments. |
| Python block | `"python_execution"` | `[{"code": <Python source>}]` |

A name with `*` is a pattern, and `*` matches any characters. `"gh.*"` matches each tool of
`gh`, and `"gh.watch"` matches one. `"linear.*"` matches each tool and prompt of the server
`linear`. A pattern matches only tools, not `"python_execution"`.

A hook on a name that no loaded tool or operation has never runs. `vis-agent doctor` and the log
then show a warning with the correct name, when Vis can find one. A name such as `"linear.*"`
can be an MCP server that only a project or a session adds. For such a name, they show a note
and not a warning.

With `phase="before"`, `fn(call)` receives `{"op", "args"}`. Return `None` to allow the call or
`vis.block(reason)` to refuse it. The model sees the reason as a tool failure. Give
`vis.block(reason, hint=...)` a `hint` to tell the model what to do next. Without a hint, the
failure tells the model to ask the user before a retry.

With `phase="after"`, `fn` receives `{"op", "args", "result"}`, and Vis ignores its return
value. An after hook cannot undo the effects of the operation. Vis logs an ordinary error of a
tool hook and does not block the operation.

### Repair a patch

For `"patch"`, a before hook also receives `preview`: `{"path", "before", "after", "spans"}`.
The preview holds the current file and the proposed result of all edits. Each span is
`[start, end]`, with inclusive, one-based lines in the proposed result. Use the preview to
validate a repair before Vis writes. `preview` is absent when Vis would refuse the patch, for
example when the edited lines changed.

To propose corrected source, return `vis.repair(source, notes=[...])`. A repair replaces the
complete proposed source, not the edit list. Vis checks that the original file still matches,
then writes the final source one time. Later guards see the repaired preview. The result has the
correction notes, the actual diff, and the new file version. The hook must validate its
candidate before it returns the repair.

`vis.repair` needs string source and a nonempty list or tuple of nonblank notes. An invalid
repair marker fails, and the operation does not run. Only `"patch"` accepts a repair. Vis
repairs `"python_execution"` blocks by itself, so a block hook that returns `vis.repair` fails
the block.

### Observe a Python block

For `"python_execution"`, the before hook runs before the block. The after hook runs when the
evaluation returns. `result` is the execution result map, with the captured `stdout`, the
evaluated `forms`, and any `error`. The after hook also runs when a normal Python exception ends
the block, because earlier statements may have written files. If a before hook refuses the
block, the after hook receives `result=None`.

The hook marks the boundary of a block, not each `open()` or `Path.write_text()`. It does not
observe background work, and it does not guarantee a callback after a process is killed.

For checks after each edit, register for both `"patch"` and `"python_execution"`, to cover host
patches and plain Python writes. The return value of the callback does not reach the model. To
give findings to the next model request, use `vis.state` and a `ctx` contribution. Read the
[complexity-check example](extension-design.md#check-code-complexity-after-edits).

### Check file access

`"fs_access"` checks the paths that the host file tools `grep` and `ls` use. It is not a tool
and takes no `phase`. Its callback receives
`{"operation": "file-read" | "file-write", "path": <absolute path>}`. An error in the callback
refuses the operation. The check does not apply to `open()` in the sandbox, which follows the
sandbox's filesystem policy.

`vis.strings_of(value)` collects the strings of a nested structure, for example to check paths
in tool arguments.

### Add environment variables to a tool call

A before hook can also return `vis.with_env(variables)`, which allows the call and adds the
variables to it. Map a name to `None` to unset that variable. The tool reads the variables with
`vis.call_env()` and gives them to the processes that it starts. Other calls do not get them.
Outside Vis, `vis.call_env()` returns an empty dict.

The Clojure and Python language tools pass these variables on. `clj.run_tests` and
`py.run_tests` give them to the test process. `clj.repl_start` and `py.repl_start` give them to
the REPL that they start:

```python
import blockether.vis.extension as vis


def dev_env(call):
    return vis.with_env({"AWS_PROFILE": "dev", "HEADLESS": "true"})


vis.register_extension(
    vis.Extension(
        name="dev-env",
        description="Run tests and REPLs with the dev AWS profile.",
        op_hooks=[
            vis.OpHook(
                ["clj.run_tests", "clj.repl_start", "py.run_tests", "py.repl_start"],
                dev_env,
            )
        ],
    )
)
```

A REPL that already runs keeps its environment. If the variables differ, the REPL start fails
and names the variables, never their values. Stop the REPL, then start it again. `clj.run_tests`
uses a live REPL only when that REPL has the same variables. If not, the tests run in a new JVM.
Do not set one variable in two hooks, because the order of the hooks is not fixed.

### Ask the person in a hook

A hook can ask the person before it decides. It can call `vis.ask`, open a `vis.live` view, or
call a tool through `vis.tools`, also a tool that asks. The operation waits until the hook
returns. The dialog shows in the session of the operation.

```python
import blockether.vis.extension as vis


def before(call):
    answer = vis.ask("Start the REPL?", [{"name": "go", "type": "checkbox"}])
    if not answer or not answer.values.get("go"):
        return vis.block("The person did not allow the REPL start.")
    return None


vis.register_extension(vis.Extension(
    name="repl-gate", description="Ask before a REPL start.",
    op_hooks=[vis.OpHook(["clj.repl_start"], before)]))
```

When the person cancels or the dialog times out, `answer` is false, and this hook refuses the
call. A session that cannot ask the person, for example the session of an extension automation
run, gets `answer.reason == "undeliverable"` at once.

If a hook raises an exception, the operation runs. When the operation must not run without an
answer, catch the exception and return `vis.block`. A hook that calls a tool of its own
extension through `vis.tools` turns off its own hooks for that inner call. Vis logs a warning,
and the inner call runs without them.


## Durable state

`vis.state` is a dict-like store in the Vis database. It survives `/reload` and restarts. The
extension `name` is its key. So a project override shares state with the global extension that
it replaces, and two different extensions never share state.

```python
vis.state["repo"] = "acme/widgets"
vis.state.get("count", 0)
"repo" in vis.state
del vis.state["repo"]
vis.state.update({"repo": "acme/widgets", "count": 0})
```

`vis.state` is a `collections.abc.MutableMapping`, so `pop`, `setdefault`, `clear`, `keys`,
`items`, `len`, and iteration work as on a dict. Values must be plain data: dicts, lists,
strings, numbers, and booleans. A write of `None` removes the key.

## Logging and notifications

```python
vis.log("info", "loaded 3 rules")        # trace, debug, info, warn, error
vis.notify("Rules reloaded", "success")  # info, success, warn, error
vis.notify("Uploading 3 of 10", title="Deploy", notice_id="deploy", duration_ms=10000)
vis.notify("Deploy finished", "success", title="Deploy", notice_id="deploy")
vis.notify("Index rebuilt", scope="global")  # every open session
```

`vis.log` writes to the gateway log in `~/.vis/logs/YYYY-MM-DD/`. For filenames, retention, and
sharing, read [Logs and diagnostics](logging.md).

`vis.notify` shows a toast card in the top-left corner of the session. The terminal and the
Companion app show the same card. One card shows at a time:

- The top border shows the title and the close mark `✕`.
- The bottom-right corner shows the page counter, for example `1/3`, between two arrows.
- Click an arrow to see the other cards that wait.
- The bar on the bottom edge shrinks for the time that is left.
- Only the card on screen counts down. A card that waits keeps all its time.
- Click `✕` to close the card before its time ends. A click on the rest of the card goes to the
  text under it.

Use these arguments to change the card:

- Give `title` for a bold first line of at most 120 characters.
- Give `duration_ms` to keep the card on screen from 1000 to 60000 milliseconds.
- Without `duration_ms`, an info or success card stays 4 seconds, and a warning or error card 8
  seconds.
- Give the same `notice_id` again to replace a card, for example to update progress.

`scope` selects who sees the card:

- `scope="session"` shows the card only in the session of the call. When no client shows that
  session, the card waits until a client opens it. Then that client shows it one time.
- `scope="global"` shows the card now in every open session. Nothing keeps it for later.
- Without `scope`, a call for a session is a session card, and other calls are global.
- `scope="session"` outside a session raises an error.

The transcript never keeps a card, and a reconnect never shows it again.

To save a secret file, such as OAuth tokens, use `vis.write_private_file(path, text)`. [Provider
extensions](provider-extensions.md#store-and-renew-tokens-safely) shows it with token renewal.

## Asking the human and showing live work

`vis.ask(title, nodes)` pauses the extension and shows a typed form in the TUI or the Companion
app. `vis.live(title, nodes)` opens a view that the extension updates while a job runs. Both
take the same [components](components.md). Each has its own page: [Forms and user
input](human-input.md) and [Live views](live-views.md). For concurrent read-only polling with
fail-fast, Stop, and owned cleanup, use the [fixed-build monitoring
recipe](live-views.md#monitor-a-fixed-build-set).

Live views has a section for each of these calls:

- `vis.live(..., placement="panel", key=...)` opens a view in the side panel of a session.
  [Choose a placement](live-views.md#choose-a-placement) explains its lifetime.
- `on_event=callback` gets each `vis.LiveEvent` of a panel view as an extension callback.
- `vis.attach(vis.ui.app(title, *nodes))` opens an app: a live view that stays in the session.
  People save drafts and send them. [Apps](live-views.md#apps) shows how you answer them.
- `vis.ui.app(..., shortcuts=[vis.ui.shortcut("n", "next")])` gives an app single-key shortcuts.
  `vis.attach(app, on_event=callback)` and `app.run(callback)` handle its events. [Add shortcuts
  to an app](live-views.md#add-shortcuts-to-an-app) shows them.
- `view.values()`, `view.events(timeout=None)`, and `view.on_event(callback)` read the inputs
  and the actions of a person. [Collect input in a view](live-views.md#collect-input-in-a-view)
  shows them.

## Environment

An extension does not get the full host environment. Declare the variables that it needs in
`env`. `vis.register_extension()` resolves them and adds them to the extension's `os.environ`.
Read them after registration.

```python
import os

vis.register_extension(vis.Extension(
    name="acme",
    description="Acme integration.",
    env=["ACME_API_KEY"],
))

key = os.environ.get("ACME_API_KEY")   # absent when nothing resolves it
```

Each name resolves through the project's `environment:` block, then `.env` and `.env.local`,
then the environment that started Vis. [Configuration](configuration.md#environment) has the
detail. A name that the project defines needs no declaration. `env=` changes only the
extension's `os.environ`, not the environment of jailed child processes. To pass a variable to a
jailed child, declare it under `environment:`.

## Session context

A `ctx` callable adds data to the model's `session` dict each turn:

```python
def _ctx(env):
    return {"session_env": {"todo": {"open": len(vis.state.get("todos", []))}}}

vis.register_extension(vis.Extension(name="todo", description="Todo list.", ctx=_ctx))
```

Return a dict with string keys, under a key that is unique to your extension. Vis puts custom
keys directly in `session`. Core fields and the `engine_` and `session_` prefixes are reserved.
Use `session_env` to add nested data under `session["env"]`.

Vis deep-merges the results of all extensions. A return that is not a dict, or an exception,
adds no context and does not block the turn.

## Workspace root

In a session-bound extension, `vis.workspace_root()` returns a `pathlib.Path` for the directory
where the session works. Start relative file and process paths there. The value follows the
session when its directory changes, for example after `/cd`. Do not cache it across tool calls.

```python
import blockether.vis.extension as vis

source = vis.workspace_root() / "src" / "main.py"
```

The workspace root is not the project root of a language. When you need a narrower root, find
package markers below it. Absolute paths stay absolute. Outside a Vis process, the function
returns the process working directory. A hosted call without a bound session fails and does not
use the gateway directory.

The function grants no filesystem permission. The model sandbox has `project_root_path` for the
same root.

## Project root

`vis.project_root()` returns a `pathlib.Path` for the project that holds your extension. The
project is the nearest directory above the extension file that holds a `.vis` directory.

```python
import blockether.vis.extension as vis

config = vis.project_root() / "tools.toml"
```

For an extension in the workspace's own `.vis/extensions`, `vis.project_root()` and
`vis.workspace_root()` return the same path. A [glob
declaration](extension-packages.md#load-extensions-from-nested-projects) can load an extension
from `repositories/api/.vis/extensions`. `vis.project_root()` then returns the
`repositories/api` directory of the session. Use it, not a path calculation from `__file__`.
Outside a Vis process, it returns the process working directory.

The project root also changes these values:

- `vis.shell()` and `vis.jailed_shell()` start a relative or empty `cwd` at the project root.
- The env dict of activation, prompt, and context callbacks has `workspace_root` and
  `project_root`.
- The `run(ctx)` of a slash command also receives `workspace_root` and `project_root`.

A setting of a nested extension uses the configuration of its own project, as a session at that
project does. Later entries in this list win:

1. The workspace configuration.
2. A `toggles:` value in `repositories/api/vis.yml`.
3. A value that you set for the session or its group.
4. A `vis run --toggles` value.

## List workspace files

`vis.workspace_files()` returns the files of the session workspace. The list holds the paths
that Git sees under `vis.workspace_root()`: tracked files, and untracked files that Git does not
ignore.

```python
import blockether.vis.extension as vis

files = vis.workspace_files() or []
sources = [path for path in files if path.suffix == ".py"]
```

The host lists the workspace one time and gives the same list to each extension that asks within
two seconds. A `git` child process in each extension is slower, also when the jail is on. The
function returns None when the workspace is not in a Git working tree. Outside a Vis process, it
lists the process working directory.

## Python environment

Call `vis.python_environment()` before your extension starts project Python, such as a test
runner or a REPL. It returns `project` or `shared`, from `python.environment` in the
[configuration](configuration.md#python-extension-environment) of the session project.

```python
import blockether.vis.extension as vis

runtime = vis.python_runtime()
if vis.python_environment() == "shared":
    command = runtime.python("-m", "pytest", shared=True)
else:
    command = runtime.uv("run", "python", "-m", "pytest")
```

With `shared`, start Python with `runtime.python(..., shared=True)`. That interpreter imports
the Vis shared packages and does not use the project's `.venv`. With `project`, the default, use
the project's own environment. Outside a Vis process, the function returns `project`.

Call `vis.python_runtime()` to get the commands. Do not search `PATH` for `vis-agent`, and do
not compute the Vis home directory. Vis can run as a native binary in a container or from a
second installation, and then these locations differ. The result has these fields and methods:

- `python(*args, shared=False)` returns the command that runs Vis Python, like
  `vis-agent python`.
- `uv(*args, shared=False)` returns the command that runs the bundled uv, like
  `vis-agent python uv`.
- `launcher` is the command that starts this Vis again from any directory.
- `shared_packages` is the directory of the Vis shared packages.
- `python_executable` and `uv_executable` are the bundled programs, or None when Vis does not
  know them.

The commands from `python()` and `uv()` apply the package index and the bytecode cache of Vis. A
direct call of `uv_executable` does not apply them. Outside a Vis process, the launcher is
`vis-agent` from `PATH`. When no Vis is found, `launcher` is empty, and `python()` and `uv()`
raise `RuntimeError`.

## Find and read sessions

`vis.list_sessions(search=…)` finds a conversation, and `vis.read_session(id)` reads it. Both
return the string-keyed data and the result types of the sandbox tools with the same names. A
session read has `op="read_session"` and prints a summary. Its full data stays available by key.
A missing field names the available keys.

```python
import blockether.vis.extension as vis

rows = vis.list_sessions(search="release")
data = vis.read_session(rows[0]["id"]) if rows else vis.read_session()
last = data["transcript"]["turns"][-1]
request, status = last["user_request"], last["status"]
```

`vis.list_sessions()` lists every session, newest first. With `search`, it runs the session
search of the TUI and the Companion app. Matches in the title come first, then the request, the
reply, and the thinking. Keep that order. Each row has `id`, `title`, `turn_count`,
`created_at`, and `modified_at`. A match adds `rank`, the `is_in_*` flags, and optional
snippets.

`vis.read_session()` without an argument reads the session that runs your extension. An argument
is a bare id, an unambiguous prefix, or the `vis_session_id#<uuid>` marker that a person copies
from the TUI or the Companion app. The result has these parts:

- The dict has `session`, `current_turn`, `failures`, `diagnosis`, `session_forks`,
  `turn_retries`, `usage`, and `transcript`.
- Each turn has `position`, `user_request`, `status`, `content`, and `iterations`.
- Each iteration has `blocks` with `code`, `stdout`, and `error`.
- In a fork, `session.forked_from` has the source `session_id`, the last copied `turn_id`, and
  the current `outcome` of that turn.

The `introspection` setting controls only the model's session tools. It does not apply here,
because an extension is trusted code. Outside a Vis process, both functions raise an error.
There, use the SDK client.

## Create and control sessions

Use `vis.sessions`, `vis.run`, and `vis.session` when your extension must start work in other
sessions. These calls return the `Session` handles of the [Sessions API](sessions-api.md).

```python
import blockether.vis.extension as vis


def second_opinion(question: str) -> str:
    """Ask a new independent session and return its answer."""
    turn = vis.run(question, timeout=300)
    return turn["content"] if turn["status"] == "completed" else turn["status"]


def start_review(path: str) -> str:
    """Start a review in a new session and return the session ID."""
    session = vis.sessions.create()
    session.send(f"Review {path}. Do not change files.")
    return session.id
```

- `vis.session` is the `Session` of the bound session, the session that called your extension.
  `vis.council` is its [Council](council-api.md) handle. Without a bound session, both raise
  `RuntimeError`.
- `vis.sessions.create(provider=..., model=..., **options)` creates an independent session.
  Without `root`, it starts in the root of the bound session. The options are those of
  `create_session`.
- `vis.sessions.get(session_id)` returns a handle without a request.
  `vis.sessions.list(search=None)` returns the sessions of the first page, or the matches of
  `search`.
- Each record of `vis.sessions.list()` has the `status`, the `current_turn_id`, and the
  `workspace` of its session, with the folder in `workspace["root"]`. Vis reads these records
  from stored data. To check the state or the folder of other sessions, use these records. Do
  not read the [context](context-management-api.md) of each session for this check. A context
  read builds the context of the session and runs the context hooks of its extensions.
- `vis.sessions.export(session_id=None)` returns a
  [snapshot](sessions-api.md#save-and-restore-a-session-snapshot) of the bound session or of
  `session_id`. `vis.sessions.restore(data, root=..., group_id=..., key=...)` restores one.
- `vis.run(request, ...)` creates an independent session and runs one turn in it. It takes the
  arguments of `Agent.run`, also `response_model`. Without `response_model`, check the `status`
  of the turn.
- `vis.speech` speaks text, transcribes audio, and adds, changes, or removes voices. It has the
  methods of `client.speech` in the [Speech API](speech-api.md#use-speech-in-an-extension).

Each turn of a new session costs model tokens. [Choose the model of a
session](sessions-api.md#choose-the-model-of-a-session) shows the model list with prices and
limits. The host records your extension as the origin of each session that it creates. Such a
session has no parent.

### Where each call works

Vis checks each call against the thread that makes it:

| Call | Where it works |
|---|---|
| Reads: `get`, `list`, `export`, `models()`, transcripts, turns and Council reads | In a callback and in a thread that your extension created. |
| `Session.send()` and `Turn.cancel()` | In a callback. A thread of your extension can use them only in sessions that your extension created. |
| Other changes: `create`, `restore`, `run`, `fork`, `set_model` and `publish` | Only in an active callback, such as a tool call or a command. |
| `vis.state` and `vis.log` | In a callback and in a thread that your extension created. |
| Patch, read and close a panel view that your extension opened | In a callback and in a thread that your extension created. Open the view in a callback. |

A change also needs a bound session. It can change only that session and the sessions that your
extension created. A refused call raises `GatewayError` with the status 403:

| Code | Meaning |
|---|---|
| `forbidden-route` | Extensions cannot use this call, or this thread cannot use it. Use a callback. |
| `forbidden-session` | The session belongs to another extension or to a person. |
| `session-not-bound` | The call changes something, but no session called your extension. |

These calls are for trusted extension code. The model cannot use them from `python_execution`. A
read shows no Activity. Each change shows its own Activity under your tool's Activity.

Vis runs each call to its end. The `timeout` argument of one request does not stop the request.
A wait, such as `Turn.wait(timeout=...)`, checks its deadline between two requests.


## Call other session tools

`vis.tools` calls a tool that the session already has. Name
the tool as the model calls it in `python_execution`:

```python
import blockether.vis.extension as vis

runs = vis.tools.gh_runs(limit=5)                   # the model calls gh_runs(limit=5)
reservation = vis.tools.spel.reserve("x-login")     # an object method, spel.reserve(...)
reservation = vis.tools["spel.reserve"]("x-login")  # the same call, named by a string
issue = vis.tools.linear.get_issue(id="ENG-1")      # a tool of the MCP server linear
```

An MCP server has the name that `python_execution` gives it: the `python_name` of its row in
`session["env"]["mcp"]["servers"]`.

The call runs in the session that called your extension, as the model's call would. It uses only
the tools that are active in that session. It shows its own Activity under your tool's Activity.

A result has the shape that the model sees:

- A typed object from a Python extension becomes a read-only record with the same class name and
  public fields. [Return typed objects](#return-typed-objects) gives its rules: field access,
  errors for an unknown field, property fields, `str` text, and dictionary methods.
- An object with a [field-backed sequence](#field-backed-sequences) iterates over that field and
  supports `len()`.
- Other results are JSON data. An object becomes a dict that also allows attribute access, such
  as `page.meta.total`. A list stays a list.
- Text and list answers support `.get()` and `.keys()`, as sandbox results do.
- Search pages support `next(page)`, `page.pages()`, and `page.all()`. Each next page calls the
  host with the original search options.
- The answer of `vis.tools.shell(...)` is the live handle that `python_execution` gets.
  `sh.wait()`, `sh.logs()`, and `sh.stop()` control the process.

The record is a copy in your extension's process, not an instance of the other extension's
class. So check its fields, not `isinstance` with that class.

Pass JSON-compatible arguments: strings, numbers, booleans, `None`, lists, and dicts. Vis sends
a dataclass argument as a dict of its public fields, and also a record that `vis.tools`
answered. An extension tool that annotates the parameter with a dataclass receives an instance
of its own class, as in [Accept records as arguments](#accept-records-as-arguments).

A failed call raises `VisToolError`, a `RuntimeError`. Its message carries the tool's error,
such as `ValueError: no browser available`. An unknown name raises the same error and lists
active tools with similar names.

Vis refuses a call back into your own extension, or into another extension that waits on the
current call. Call your own functions directly. Outside a Vis
session, for example in plain unit tests, `vis.tools` raises an error. There, import and call
the other package directly.

## Attachments

`vis.attach` stores a file that your tool makes, such as a chart, a CSV table, or a PDF report.
The session keeps the file as a durable attachment of the `python_execution` block that called
your tool. People see it in the transcript, and an image can also go to the model. The functions
match the `attach` functions of `python_execution`:

```python
import blockether.vis.extension as vis

row = vis.attach("out/totals.png", label="Totals by month")  # a path, bytes, a figure or a Pillow image
vis.attach(csv_bytes, "totals.csv")                           # bytes need a filename
versions = vis.list_attachments("totals.csv")                 # every version, oldest first
latest = vis.get_attachment("totals.csv")                     # the descriptor, without bytes
data = vis.read_attachment(latest)                            # the bytes
vis.show_attachment(row)                                      # show the image to the model again
```

`vis.attach` returns a descriptor dict with `id`, `filename`, `version`, `media_type`, `kind`,
`size`, and `audience`. A second attach under the same filename stores the next version of that
file. Use a new filename only for a different file. An app declaration from `vis.ui.app(...)`
opens an app and returns a `vis.LiveAttachment` handle, as [Apps](live-views.md#apps) explains.

`audience` sets who sees the file: `both` (the default), `user`, or `model`. A PDF or HTML
document is for the user only. A script in an HTML page can still give the model a note, as
[Tell the model what people see](live-views.md#tell-the-model-what-people-see) shows. A CSV or
TSV table shows as a table, and its rows do not go to the model.

Without `media_type`, Vis detects the type in this order:

1. Vis reads the bytes, then the filename. For ZIP bytes, only the filename of a ZIP-based
   format, such as `.xlsx` or `.epub`, replaces `application/zip`.
2. Without a filename type, two or more lines of JSON objects are `application/x-ndjson`.
3. After that, UTF-8 text is `text/plain` and other bytes are `application/octet-stream`.

The sandbox `attach` uses the same rule.

These limits and differences apply:

- `vis.attach` accepts the near-miss keywords of the sandbox, such as `name=` for `filename` and
  `title=` for `label`.
- Vis does not copy a file larger than the attachment limit. It keeps a reference to the path. A
  reference is for people only, and `read_attachment` and `show_attachment` refuse it.
- In `python_execution`, `attach` also prints a preview of the file in the block output.
- Only `vis.attach` in an extension accepts `on_event` for an app.

The read functions take the same target: a filename, an `id`, or a descriptor. A filename means
the latest version, unless you give `version`. A negative version counts back from the latest
version. `read_attachment` is the only function that returns the bytes. `show_attachment` shows
a stored image to the model for the next request only.

If no `python_execution` block runs your code, for example in a slash command, these functions
raise an error.

Outside Vis, for example in plain unit tests, the functions keep the files in process memory. Some answers differ from the answers of Vis:

- No size limit and no reference exist. The functions read each file whole.
- The bytes do not name the media type. The filename or `media_type` names it by the rule of
  Python `mimetypes`. Otherwise the type is `application/octet-stream`.
- No rule knows a document, so the kind is `doc` only when the call gives it.
- The audience is the one that the call gives, or `both`. This also holds for a document that
  Vis keeps for the user.
- A descriptor from a read function has no `position` key.
- Where Vis raises `VisToolError`, the refusal is a `RuntimeError`. It has the same words for
  the calls of the contract fixture.

## Filesystem and processes

Extension code runs in a trusted process, separate from the model's sandbox. A gateway-wide
worker loads the registrations. A session gets its own trusted extension worker on its first
extension call. The disposal of a session stops that worker, not the registration worker.
Workers do not share interpreter memory or host-call identities.

| | Model sandbox | Extension context |
| --- | --- | --- |
| Author | the model | you |
| Filesystem | workspace roots when the jail is enabled | your user's permissions |
| Network and processes | gateway policy, no direct spawn | unrestricted |
| Environment | project values | declared `env` plus project values |
| Native calls through `ctypes` | refused under confinement | supported |
| Lifetime | session worker | separate registration or session worker, reloaded by `/reload` |

Tool results cross as data. In the sandbox, an object or a dataclass is a frozen record of its
public fields, as [Return typed objects](#return-typed-objects) describes. Methods, native
pointers, and object identity stay in the trusted process. A [field-backed
sequence](#field-backed-sequences) adds only local collection operations. Expose another
operation as a declared tool, not as a method on a returned object.

`vis.fs` has filesystem operations with the extension's permissions: `mkdir`, `write`, `read`
(bytes), `read_text`, `copy`, `move`, `list`, `stat`, and `remove`. Ordinary `open()` also uses
the extension's permissions, not the model's jail.

### Start a process

`subprocess`, `os.system`, and `vis.shell(...)` run without the jail. A child of `subprocess` or
`os.system` gets pipes, not a terminal. Vis drains its output into a buffer of 8 MiB for each
stream, also while the extension waits for the child to exit. Output that the extension does not
read goes to the extension log.

To confine a child, use `vis.jailed_shell(...)`. It applies the jail policy from the merged
configuration on disk, read again at every spawn, with or without a session.

`vis.shell` and `vis.jailed_shell` take a command with keyword options, or one options map. So
`vis.shell("npm test", cwd="app")` and `vis.shell({"command": "npm test", "cwd": "app"})` start
the same process. The child runs under a terminal, and Vis keeps its output in a log.

Both calls return the same live handle. `sh.logs()`, `sh.wait(30)`, `sh.type("y")`, and
`sh.stop()` control the process that you started. The stdin of the child is empty, so a program
that reads it gets end of input at once. To answer a prompt with `sh.type("y")`, add
`stdin="tty"` to the call. [Read the shell log](#read-the-shell-log) gives the calls of the
handle.

A jailed child sees the session's roots and the egress proxy. A language runtime may need more.
Two options of `vis.jailed_shell` add exactly what you created for the child:

| Option | Value | Use it for |
| --- | --- | --- |
| `allow_read_write` | one path or a list of paths | a FIFO or working file your extension made outside the session roots |
| `unix_connect` | one socket path or a list of them | a unix socket your extension is listening on |

```python
sh = vis.jailed_shell({
    "command": "java -cp … my.runtime",
    "cwd": project,
    "env": {"MY_RUNTIME_SOCKET": socket_path},
    "unix_connect": [socket_path],
})
```

Use the options when the child must talk back to the extension. A shell child runs under a real
terminal, so its merged output cannot carry a request and response protocol. Give the child a
FIFO or a socket for that. When the jail is disabled, Vis ignores both options.

### Read the shell log

`sh.logs()` answers one page of the log. A page is a dictionary with `out`, `offset`,
`next_offset`, and `is_eof`. The offsets count bytes.

| Call | Page |
| --- | --- |
| `sh.logs()` | the end of the log |
| `sh.logs(-20)` or `sh.logs(lines=20)` | the last 20 lines |
| `sh.logs(0, 100)` or `sh.logs(0, lines=100)` | the first 100 lines from byte 0 |
| `sh.logs(offset, -20)` | the 20 lines that end at the byte `offset` |
| `sh.logs(0, limit=4096)` | at most 4096 bytes from byte 0 |

`limit` is the byte size of a page. The default is 16384 bytes and the maximum is 262144 bytes.
A page never holds a part of a character. `n` and `tail` mean `lines`, `start` means `offset`,
and `bytes` means `limit`.

`next(page)` answers the page that follows, and `next(page, None)` answers `None` after the last
page. `page.pages()` gives the pages from this page to the end of the log. `sh.wait(30)` waits
for the exit for at most 30 seconds, and `secs` and `timeout` mean `seconds`.

Read a field of the handle by key, such as `sh["exit"]`. A field is also an attribute, such as
`sh.exit`, when no dictionary method has its name. So read the `keys` field with `sh["keys"]`.

### The shell outside Vis

Outside Vis, for example in a plain unit test, the SDK starts the command itself. The handle
takes the same calls. For the calls of the contract fixture `shell-handle.json`, it gives the
answers of Vis, with these differences:

- The child has pipes and no terminal. In Vis, a terminal writes two bytes at the end of a line.
  Outside Vis, the log has one, so the byte offsets of the same output differ.
- The log does not show the text that you type, and `sh.type()` needs `stdin="tty"`.
- The command runs with `/bin/sh -c`, not with `bash -lc`.
- `vis.jailed_shell` raises an error, because no jail exists there.
- No session holds the shell, so `attach` and `keys` are empty and `note` names a local process.
- The start answer always has the status `running`. In Vis, a command that ends at once can show
  `exited` there.
- A value of `env` is a string, a number, a boolean, or `None`. A source map, such as
  `{"keychain": "name"}`, raises an error.
- Ctrl-D that you type does not end the input of the command. The byte goes into the pipe.
- An error is a `RuntimeError`, but its class is not `VisToolError`.
- The keys of the handle have a different order.

### Toolchain grants

A jailed child can read system files and the session roots. A toolchain can live in another
place: a JDK under `~/.sdkman`, a package cache, or a launcher script. Declare these paths one
time with `vis.Grant` in `Extension.grants`.

```python
vis.register_extension(vis.Extension(
    name="java-tools",
    description="Java build tools.",
    grants=[
        vis.Grant(java_home, reason="Run the JDK."),
        vis.Grant("~/.m2/repository", "read_write", reason="Read and fill the Maven cache."),
    ],
))
```

| Argument | Default | Meaning |
| --- | --- | --- |
| `path` | Required | Absolute path, or a path that starts with `~` |
| `mode` | `"read"` | `"read"` to read and run files. `"read_write"` for a cache that the tool also writes |
| `reason` | Required | Short text that tells people why the extension needs the path |

Vis checks each grant when it loads the extension:

- The path must be absolute after `~` expansion.
- The grant must not cover the filesystem root, your home directory, or the Vis home `~/.vis`. A
  path inside them is allowed.
- A refused grant makes the extension a load failure. `vis-agent doctor` shows the reason.

Vis adds the grants only to the `vis.jailed_shell` children of the extension that declared them.
The model's shell and other extensions never get them. The deny rules of the jail configuration
still win. A grant can name a symbolic link, such as `~/.sdkman/candidates/java/current`. The
jail then also follows the link to its target.

Use `allow_read_write` only for a path that you create for one run, such as a FIFO. Declare a
fixed toolchain path as a grant.

### Background work and reload

Your code can start threads and timers and hold file locks. They belong to the worker process
that ran the code. Python cannot stop a thread from outside, so this work stops only when that
process ends.

When `/reload` replaces an extension, Vis restarts each worker that ran the old code. These are
the registration worker and the extension worker of each session that used the extension. The
threads, timers, and file locks of the old code end with the process. A worker with a running
call restarts when that call ends, so the call finishes with the old code. A reload that
replaces no extension restarts no worker. To restart the workers of unchanged extensions, run
`/reload --all`.

The restart ends the complete process:

- Each extension in that worker loads again at its next call, without the values of its module
  globals.
- Live views that those extensions drive close. An app stays open and shows as disconnected.
  Give the app the [`on_reload` hook](live-views.md#after-a-reload-of-your-extension) to connect
  it again.
- Processes that the old code started keep running.

Keep data that a reload must not lose outside the process, for example in a file.

## See also

- To choose and document the behavior of a tool, read [Extension design](extension-design.md).
- To install an extension and reload it, read [Installing and sharing
  extensions](extension-packages.md).
- To fix a load error or a call error, read [Extension
  troubleshooting](extension-troubleshooting.md).
- To read the saved Activity of a session from a program, read [Sessions
  API](sessions-api.md#read-activity-history).
