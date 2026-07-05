# Python extensions

[Clojure extensions](extending.md) are compiled into the binary at build
time. **Python extensions** are the dynamic
counterpart: single `.py` files you drop into a directory, loaded at startup
and reloadable in place with `/reload` — no rebuild, identical behavior on
the JVM and in the native binary.

```
~/.vis/extensions/           global — loads in every project
<project>/.vis/extensions/   project-local — loads for that project only
```

A project file registering the same extension **name** as a global one wins
(same layering as configuration). A file that fails to load becomes a
warning in `vis doctor` — it never crashes Vis.

A Python extension can contribute:

- **tools** — functions the model calls in its sandbox (`todo_add("x")`)
- **prompt fragments** — constant or recomputed every turn
- **slash commands** — `/todos`, `/gh-repo …` for the user
- **op hooks** — guards that can block file operations
- **durable state** — a key/value store that survives restarts

Channels, providers, and TUI rendering stay Clojure-side.

## Hello, extension

```python
# ~/.vis/extensions/greeter.py
"""Greeter — smallest possible tool extension."""
import vis


def greeter_hello(name):
    """await greeter_hello(name) -> {"greeting"} — greet someone."""
    return {"greeting": f"hello {name}"}


vis.extension(
    name="greeter",
    description="Greets people.",
    kind="integration",
    alias="greeter",
    symbols=[vis.symbol(greeter_hello, tag="observation")],
    prompt="greeter_ surface active: greeter_hello(name).",
)
```

Start Vis (or `/reload`) and the model can call `await greeter_hello("vis")`.
Ready-to-copy examples — a todo list, a protected-paths guard, a dynamic
prompt toggle, a GitHub issues integration — live in the repo under
`resources/examples/python-extensions/`.

## `vis.extension(...)`

Exactly one call per file. Keyword arguments:

| Argument | Type | What it does |
| --- | --- | --- |
| `name` | str, required | Unique extension name. Project file with the same name overrides a global one. |
| `description` | str, required | One-liner for `vis extensions list` and the model's extensions snapshot. |
| `kind` | str | Section label (`"integration"`, `"guard"`, …). Defaults to `"python"`. |
| `version` | str | Plain metadata. |
| `alias` | str | Python-name prefix for tools. Required when `symbols=` is declared. |
| `symbols` | list of `vis.symbol(...)` | Model-facing tools. |
| `prompt` | str or callable | Model-facing fragment. A callable receives the env dict every turn and returns a string or `None` (no fragment that turn). |
| `activation` | callable | `(env) -> bool`, evaluated per turn; gates the whole extension. Default: always on. |
| `slash_commands` | list of `vis.slash(...)` | User-facing commands. |
| `op_hooks` | list of `vis.op_hook(...)` | Guards/observers over file ops. |

The **env dict** passed to `prompt`/`activation` callables is deliberately
small: `{"cwd", "session_id", "channel"}`.

## Tools

```python
vis.symbol(fn, name=None, tag="observation", hidden=False)
```

- `tag` declares what the tool does: `"observation"` (reads state) or `"mutation"`
  (changes state) — same contract as Clojure tools.
- The sandbox name is `f"{alias}_{name}"`; `name` defaults to `fn.__name__`
  with a leading `"{alias}_"` stripped, so a module can use readable full
  names (`todo_add` under alias `todo`) without double-prefixing.
- **The docstring is mandatory** — it becomes the model's `doc()` text.
  House style: `await name(args) -> shape` on the first line.
- Parameter names are read from the real signature and shown to the model.

**Envelope semantics — Python authors never construct envelopes:**

- the **return value** (dict/list/str/number) *is* the success payload;
- **raising is the failure path** — the exception message surfaces to the
  model as a normal tool failure it can route around:

```python
def todo_toggle(id):
    """await todo_toggle(id) -> {"id", "done"} — flip one todo."""
    for t in vis.state.get("todos", []):
        if t["id"] == id:
            ...
    raise ValueError(f"no todo with id {id}; call todo_list() to see ids")
```

Dict keys pass through as written — use snake_case.

## Slash commands

```python
vis.slash(name, run, doc=None, usage=None)
```

`run(ctx)` receives `{"channel", "args", "raw", "session_id"}` and returns:

```python
vis.ok(title, body=None, data=None)    # body: Markdown string
vis.err(title, body=None, data=None)
```

(or a plain string, treated as an ok title).

## Op hooks

```python
vis.op_hook(ops, fn, phase="before")
```

- `ops` — sandbox tool names to hook: `"write"`, `"patch"`,
  `"struct_patch"`, `"move"`, `"copy"`, `"delete"`, …
- `phase="before"` — `fn(call)` receives `{"op", "args"}` **before** the op
  runs. Return `vis.block(reason)` to refuse it (the model sees the reason
  as a tool failure) or `None` to allow. A hook error fails open.
- `phase="after"` — `fn(call)` receives `{"op", "args", "result"}` after the
  op; observe-only (the return value is ignored).

`vis.strings_of(value)` collects every string leaf of a nested structure —
handy for scanning op args for paths.

## Durable state

`vis.state` is a dict-like store persisted to the database (the same
`vis.db` sessions live in, under the `extension_aggregate` table) — no files
on disk. It survives `/reload` and process restarts, and is owned by the
extension **name** (a project-local override of a global extension shares its
state; two different extensions never do).

```python
vis.state["repo"] = "acme/widgets"      # write-through
vis.state.get("repo")                   # read, None when missing
vis.state.get("count", 0)               # read with default
"repo" in vis.state                     # membership
del vis.state["repo"]                   # delete
```

Values must be plain data (dicts, lists, strings, numbers, booleans).

## Logging and notifications

```python
vis.log("info", "loaded 3 rules")       # levels: trace debug info warn error
vis.notify("Rules reloaded", "success") # user-facing toast: info success warn error
```

`vis.log` writes to `~/.vis/vis.log`; `vis.notify` shows in whatever channel
is active (TUI banner, web toast, …).

## Execution model and trust

Extension files run in **trusted GraalPy contexts** — one per file, separate
from the model's sandbox:

|  | Model sandbox | Extension context |
| --- | --- | --- |
| Who writes the code | the model | **you** |
| Filesystem | confined to workspace roots | real |
| Network / env vars / subprocess | off by default | real |
| Lifetime | per session | process (rebuilt on `/reload`) |

The two share nothing. The model can *call* your tools (through the host
wrapper, envelope-checked like any tool) but can never evaluate code in your
context. Your code reaches the host only through the `vis` API — there is no
Java interop surface.

Treat `.py` files in a project's `.vis/extensions/` like you treat its
`deps.edn`: they execute with your user's permissions when Vis starts in
that checkout — review before running Vis in untrusted repositories.

Calls into an extension are **serialized** (one at a time per file). Keep
per-turn callables (`prompt`, `activation`) fast; tools may take their time.

## Reloading

- `/reload` — tears down every Python extension (contexts closed) and loads
  the current files fresh. State survives (it lives in the database).
- Changes propagate to LIVE sessions immediately: new/changed slash
  commands dispatch right away and reloaded tools rebind into the sandbox
  — no restart, no new session.
- Startup is fingerprint-checked: unchanged files are a no-op; changed files
  reload automatically on the next Vis start.
- `vis doctor` lists every loaded file and every load failure with its
  Python error.

## Python vs Clojure extensions

| | Python (`.vis/extensions/*.py`) | Clojure (classpath) |
| --- | --- | --- |
| Ship | drop a file | build into the binary |
| Reload | `/reload`, in place | rebuild + restart (native) |
| Scope | per project or per user | every install of the distribution |
| Can contribute | tools, prompts, slash, op hooks, state | everything (channels, providers, persistence, TUI, CLI, themes, …) |

Reach for Python for project-specific tools and guards; graduate to a
[Clojure extension](extending.md) when you need deeper
surfaces or want to ship to others as part of a distribution.
