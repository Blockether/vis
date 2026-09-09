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
  .vis/extensions/einmal_tools.py
```

```python
# .vis/extensions/einmal_tools.py
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
  the whole checkout or a virtual environment.
- `dependencies` accepts standard package requirements, including version pins,
  extras and environment markers. Vis uses its bundled **pip**, installing only
  wheels into `~/.vis/python/packages`, shared with `python_execution` and other
  extensions. It does not modify the project's `.venv`.
- Set the index with [`python.index_url` in `vis.yml`](configuration.md#python-package-index).
  Normal pip authentication and certificate settings still apply. A missing
  wheel or failed install is a load failure, not a fallback to source builds.
- `requires-python` checks the embedded interpreter. Vis does not download
  another Python version to satisfy it.
- `/reload` takes new source snapshots and resolves declared dependencies again;
  an unchanged loader scan does neither. Source edits are not used by existing
  tools until reload. A failed reload retains the last working extension.

Source snapshots contain extension code, not installed dependencies. Both embedded
workers import the same shared package directory; calling a tool does not install
packages again. Each process has its own module cache and permissions. Dependency
versions are shared across all sessions and extensions, not isolated per project.
A failed reload retains the extension definition, but does not roll back shared
package updates. After changing installed packages, use `/reload` to rebuild the
session workers; an already running call can retain its imported modules until it ends.

### Extension Center projects

Use this layout for automatic dependency preparation and GitHub discovery. Keep these
files at repository root or together in a selected subdirectory:

```text
vis-greeter/
  pyproject.toml
  extension.py
  README.md                 # optional; linked on GitHub
  uv.lock                   # optional; recommended for repeatable resolution
```

```toml
# pyproject.toml
[project]
name = "vis-greeter"
version = "1.0.0"
description = "Small greeting tools for Vis."
requires-python = ">=3.11"
dependencies = ["vis-agent>=0.1.45"]

[tool.vis]
category = "tools"
# source_paths = ["src"]    # optional import roots within this package
```

```python
# extension.py
import blockether.vis.extension as vis


def greet(name: str) -> str:
    """Return a greeting for a name."""
    return f"Hello, {name}!"


vis.register(vis.Extension(
    name="vis-greeter",
    description="Small greeting tools for Vis.",
    alias="greeter",
    symbols=[vis.Symbol(greet)],
))
```

The manifest must declare an unconditional `vis-agent` dependency. Its version
constraint checks compatibility with the running Vis release; `requires-python`
checks the embedded interpreter. Vis does not download another Python interpreter.
The registered extension name must match the normalized project name. The manifest
supplies the displayed version, description and category: `providers`, `tools` or
`workflows`. Keep any additional Python implementation under the package directory.
Do not combine this layout with a PEP 723 block in `extension.py`.

The Extension Center aggregates public GitHub repositories. Choose **Add a repository**,
enter an HTTPS repository URL and leave **Project folder** empty for repository root.
For a monorepo, specify the folder containing both `pyproject.toml` and `extension.py`,
for example `extensions/greeting`. The Worker reads metadata without executing code.
Review the resolved commit and submit it for moderation. New entries and updates remain
private until approved; resubmission never replaces a published listing automatically.
Separate folders can have separate entries.
See the [docs application instructions](https://github.com/Blockether/vis/tree/main/apps/vis-docs).

After reviewing the source and dependencies, copy the catalog's commit-pinned install
command. You can also install a GitHub project's default branch or link local source.
These examples use a placeholder public repository; replace it with your own:

```bash
vis-agent extension install https://github.com/example/vis-greeter --trust
vis-agent extension install https://github.com/example/extensions --subdirectory tools/greeting --trust
vis-agent extension install ./vis-greeter/pyproject.toml --project --trust
```

For a reviewed immutable source version, add `--revision` followed by its full lowercase
40-character Git commit SHA. GitHub installs require Git on `PATH`; only HTTPS
`github.com/owner/repository` URLs are accepted. Specify a folder separately instead of
pasting a GitHub file or tree URL. Submodules and Git LFS are not fetched. Keep required
source and portable dependency paths within the selected project. Symlinks are not
accepted in downloaded projects; selected contents are limited to 4096 entries and 64 MiB.

Installation defaults to `~/.vis/extensions/`; `--project` selects the current
workspace's `.vis/extensions/`. A Git checkout is staged and only the selected project
is installed atomically. A local checkout is linked rather than copied, so edits become
available on `/reload`. Existing destinations are never overwritten. To replace one,
explicitly remove the installed link or directory first, preserving source work.
The catalog stores no source bundles and is not consulted during installation.

At gateway startup and on `/reload`, Vis automatically prepares these projects using
`uv` from `PATH`. It creates `uv.lock` if absent, respects an existing lock, and skips
installation when its readiness record still matches the project, runtime, index and
installed distributions. A stale supplied lock is an error: update it deliberately
with `uv lock` rather than expecting reload to rewrite it. Source-only edits need
`/reload`, not another install command. Use `python.index_url` to select the index.
The connecting terminal reports startup preparation and dependency stages; status
is also included in authenticated gateway administration responses.

`--trust` permits extension code and dependency build backends to run with your
user permissions. Validation is not a security review. Dependencies use the shared
`~/.vis/python/packages` directory, not isolated per-extension environments. A failed
reload retains the last working extension definition but cannot roll back shared
package changes. Fix the dependency error before retrying `/reload`.

### uv projects

For a manually prepared uv package, keep normal Python packaging metadata in the package
and a thin Vis entry file beside the workspace. The implementation does not need
to depend on Vis; only the entry imports the host-provided `blockether.vis.extension`.

```text
project/
  einmal/
    pyproject.toml
    uv.lock                       # generated by uv lock
    src/einmal/__init__.py
    tests/test_status.py
  .vis/extensions/einmal_tools.py
```

This complete example uses setuptools with an editable `src/` layout:

```toml
# einmal/pyproject.toml
[project]
name = "einmal"
version = "0.1.0"
requires-python = ">=3.12"
dependencies = []

[build-system]
requires = ["setuptools>=64"]
build-backend = "setuptools.build_meta"

[tool.setuptools.packages.find]
where = ["src"]
```

```python
# einmal/src/einmal/__init__.py
def status() -> str:
    """Return the integration status."""
    return "ready"
```

```python
# .vis/extensions/einmal_tools.py
# /// script
# requires-python = ">=3.12"
# dependencies = []
# [tool.vis]
# project = "../../einmal"
# ///
import blockether.vis.extension as vis
from einmal import status

vis.register(vis.Extension(
    name="einmal",
    description="Package example.",
    alias="einmal",
    symbols=[vis.Symbol(status)],
))
```

`project` is relative to the entry file, not the working directory. Absolute paths
also work. It must contain both `pyproject.toml` and `uv.lock`. Project mode rejects
nonempty script dependencies; declare dependencies in `pyproject.toml` instead.
Do not add this package's `src` to `tool.vis.source_paths`: its editable install
already provides the import root. `source_paths` is the alternative for source
that Vis snapshots without installing it as a package.

Install uv on the **sync command's PATH**. From `project/`, generate and commit the
lockfile, then prepare the package for Vis:

```bash
uv lock --project ./einmal
vis-agent python uv sync --project ./einmal --locked
vis-agent python -c "import einmal; print(einmal.status(), einmal.__file__)"
vis-agent extension list
```

The import prints `ready` and the path to `einmal/src/einmal/__init__.py` in this
checkout, not a copied module under `~/.vis/python/packages`. The extension list
includes `einmal`. Start Vis in `project/`, or run `/reload` there, to load its tools.

Run sync as the same OS user and with the same Vis runtime and package-directory
settings as the gateway. It uses the embedded Python and runs
`uv export --locked --no-default-groups --format pylock.toml`, then
`uv pip install --target` on the exported lock. It preserves editable local
sources, resolved dependencies, artifact hashes and named indexes while retaining
unrelated packages. Python downloads are disabled. The temporary export is removed;
`uv.lock` and the project's `.venv` are unchanged. Default dependency groups and
optional extras are not installed. `--offline` and `--no-cache` are supported;
other uv sync options are rejected.

A project needs a build backend to install its own package. Without `[build-system]`
(or when uv is configured not to package the project), preparing dependencies does
not install the project's source. For a local dependency, declare it in
`project.dependencies` and explicitly select editable mode in the project's TOML:

```toml
[tool.uv.sources]
shared-tools = { path = "../shared-tools", editable = true }
```

The sibling package must have its own packaging metadata. A plain path dependency
without `editable = true` is not a promise of live source imports. Published wheels
are normal installed dependencies, not editable source trees.

Vis installs into **`~/.vis/python/packages`**. Editable installs place `.pth` files
or backend import hooks there, rather than copying the implementation. Both the
sandbox and trusted extension worker activate them. They do **not** grant access
to the referenced source: sandbox imports still require that checkout to be in an
allowed [workspace filesystem root](jail.md#filesystem-access).
Keep the checkout at its installed path; moving it requires another sync.

`~/.vis/python/packages` is shared across projects, so dependency versions are
not isolated. Start and `/reload` do not install these manually selected uv projects. Changes to
`pyproject.toml`, `uv.lock`, runtime, default index or installed distribution metadata
require another explicit sync. A failed load does not roll back shared package changes.

Build backends and executable `.pth` lines are trusted package code, not inert
configuration. Review projects and dependencies before installing them. Builds run
during explicit sync; `.pth` setup runs when a worker activates the package site.
Imports in `python_execution` never install packages, and the shared directory is
read-only to sandbox code.

`python.index_url` supplies uv's default index; named source indexes are not replaced.
Keep credentials in uv's supported credential configuration or the sync process's
environment, not committed URLs. Installer diagnostics are suppressed because they
may contain credentials. Loading a prepared project does not require uv on the
gateway's PATH.

### Explicit installation workflow

| Change | Required action |
| --- | --- |
| First use of a checkout | Generate `uv.lock`, run `vis-agent python uv sync --project ./einmal --locked`, then start Vis or `/reload` |
| Edit existing editable Python source or an extension entry | `/reload`; no reinstall or gateway restart |
| Change dependencies, packaging metadata or the checkout location | Update the lock if needed, sync again, then `/reload` |
| Change Vis runtime, package directory or index | Sync using the intended runtime and settings, then load the extension |
| Replace compiled extension code | Rebuild and install it, then use a fresh Vis process; Python source reload is not a native-library reload |

`/reload` updates tools in live sessions at the next turn boundary.
Already running calls may finish with old code. `/reload` does not replace a running
gateway's binary or startup environment; adopting a new Vis build or changing
startup location overrides requires starting the gateway with those settings once.

Editable packages use the live checkout, not Vis's frozen source snapshots. Cached
imports can retain old code until reload, while a first import can read edits sooner.
Use `/reload` as the update step; do not rely on editing a file alone to refresh an
already imported function. Ordinary installed dependencies are not cleared from
the registration worker's module cache by editable reload.

This manual installation workflow is selected by `tool.vis.project`. Script-only
PEP 723 `dependencies` still use the automatic pip loader. A plain external `uv sync`
prepares a separate project environment; it does not install packages for Vis.

### Keep business logic outside the entry file

Keep the implementation in the ordinary Python package, as in the example above,
and test it independently:

```python
# einmal/tests/test_status.py
from einmal import status


def test_status():
    assert status() == "ready"
```

After the initial sync, run from `project/`:

```bash
vis-agent python -m pip install pytest
vis-agent python -m pytest einmal/tests/ -q
```

No `PYTHONPATH` or extra `source_paths` is needed for this editable package. The
spelling is `vis-agent python -m pytest`, not `vis-agent python pytest`. The command
uses the shared Vis packages, not the project's `.venv`; dev groups in `uv.lock`
are not installed by the sync command. Update the test expectation when changing
`status()`'s result. For source used only through `tool.vis.source_paths`, configure
import roots separately in the package's own test environment.

After unit tests pass, verify extension registration and a tool call in a Vis
session: explicit sync, `/reload`, then call the tool. `vis-agent extension list`
checks registration only. A passing package test alone does not prove that the
prepared dependencies can be imported and called in the trusted extension worker.

Trusted extension workers support native calls through `ctypes`, including SciPy's
callback initialization. The model sandbox remains a separate, confined process.
Verify a representative calculation through the extension tool; installation or
registration alone is not a compatibility check.

### Package troubleshooting

- **Circular import or partially initialized module:** name the entry differently
  from the package it imports, for example `einmal_tools.py`, not `einmal.py`.
  The extension directory is an import root and a same-named file can shadow the package.
- **Missing import:** check the build backend, editable source configuration and
  `package.__file__`. Sync must use the gateway's OS user, runtime and package directory.
- **Missing or stale Vis environment:** update the lock when needed, run the printed
  sync command, then `/reload`. Imports do not repair readiness or install packages.
- **Import works in the CLI or extension but not the sandbox:** inspect
  [filesystem access](jail.md#filesystem-access) and the package's native operations.
  An editable install does not widen the sandbox policy.
- **Old tool result after an edit:** use `/reload` and invoke the tool on the next
  turn. Check whether the import came from the checkout, a frozen source snapshot
  or an ordinary installed wheel. Copied wheels need another install; editable
  Python source and declared `source_paths` need `/reload`.

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
