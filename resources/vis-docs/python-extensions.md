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
- **session context** — data folded into the model's `session` bag every turn
- **LLM providers** — register an API-key provider the router can call (`vis.provider(...)`)

Channels, persistence backends, and TUI rendering stay Clojure-side.

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
| `network_filters` | list of `vis.network_filter(...)` | Request/response policy at the gateway's decrypted HTTP boundary. |
| `ctx` | callable | `(env) -> dict`, evaluated per turn; the returned dict is deep-merged into the model's `session` bag. See [Session context](#session-context). |

The **env dict** passed to `prompt`/`activation` callables is deliberately
small: `{"cwd", "session_id", "channel"}`.

## Session context

The model sees a live `session` bag every turn — turn/iteration counters,
workspace facts, per-language REPL state, and so on. A `ctx=` callable lets an
extension **write its own slice** into that bag:

```python
def _ctx(env):
    # STRING keys all the way down — the bag crosses into Python as the
    # model's `session` dict, which rejects non-string keys.
    return {"session_env": {"todo": {"open": len(vis.state.get("todos", []))}}}

vis.extension(
    name="todo",
    description="Todo list.",
    ctx=_ctx,
)
```

- Runs **once per turn** during context render, so the slice is always current.
- The return **must be a string-keyed dict** (Python dict keys already are).
  Slices from every extension are deep-merged, so nest under a unique key to
  avoid clobbering another extension — `"session_env"` is the common home for
  live environment facts.
- A non-dict return or a raised exception degrades to an empty contribution —
  bad optional context never blocks a turn.
- The `env` dict is the same small `{"cwd", "session_id", "channel"}` handed to
  `prompt`/`activation`.

## Tools

```python
vis.symbol(fn, name=None, tag="observation", is_hidden=False)
```

- `tag` declares what the tool does: `"observation"` (reads state) or `"mutation"`
  (changes state) — same contract as Clojure tools.
- The sandbox name is `f"{alias}_{name}"`; `name` defaults to `fn.__name__`
  with a leading `"{alias}_"` stripped, so a module can use readable full
  names (`todo_add` under alias `todo`) without double-prefixing.
- **The docstring is mandatory** — it becomes the model's `doc()` text.
  House style: `await name(args) -> shape` on the first line.
- Parameter names are read from the real signature and shown to the model.
- `is_hidden=True` hides the tool from the model-facing listing (still callable).
  This follows the bridge-wide convention: a Python boolean spelled `is_<name>`
  maps to the Clojure `:<name>?` predicate key (Python identifiers can't carry a
  trailing `?`), so `is_hidden` → `:hidden?`, `is_authenticated` → `:is-authenticated`.

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
| Filesystem | confined to workspace roots | **real, unrestricted** |
| Network / env vars / subprocess | gateway policy / restricted | **real, inherited, unrestricted** |
| Lifetime | per session | process (rebuilt on `/reload`) |

This is an intentional trust decision, not a missing sandbox feature. Extension
contexts allow full IO, process creation, threads, sockets, and inherited
environment variables because they are user-installed plugins. They still deny
arbitrary host-class, native, and polyglot interop; host access is limited to the
bound `vis` API. The model can call an exported tool but cannot evaluate code in
the extension context. See [Process sandbox and gateway egress](sandbox.md).

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

## Multiple files and packages

A single `.py` file is the simplest extension. For anything larger, drop a
**package directory** whose `extension.py` is the entry point:

```
~/.vis/extensions/
  my_ext/
    extension.py      # the entry — calls vis.extension(...)
    mypkg/
      __init__.py
      core.py
    test_core.py      # tests (see below)
```

- The directory is prepended to `sys.path` before `extension.py` runs, so
  `import mypkg` / `from mypkg.core import add` just work — no manual
  `sys.path.insert(...)`.
- Only `extension.py` is an entry point; the package's other modules are
  imported by it, never scanned as separate extensions.
- A plain top-level `.py` file gets the same sugar for a sibling module or
  package placed next to it.

So an ordinary Python project becomes a Vis extension by adding one
`extension.py` on top that imports it.

## Testing your extension

Ship real Python tests next to the code and run them with Vis's built-in
`pytest`-compatible runner — no pip, no wheels, pure stdlib.

- Test files are `test_*.py` or `*_test.py`, at any depth under an extension
  directory. They are **never loaded as extensions** (excluded from the scan).
- Each test file runs in its own trusted GraalPy context and imports the
  extension's package through the same `sys.path` sugar the entry file gets.

```python
# ~/.vis/extensions/my_ext/test_core.py
from mypkg.core import add

def test_add():
    assert add(2, 3) == 5
```

Run them:

```
/test            # in a session — inline pass/fail report
vis ext test     # from the shell — prints a report, exits non-zero on failure
```

The report is **per test**: each `test_*` shows ✓/✗ with the failing
assertion's detail, grouped by file, under a one-line summary
(`✓ N file(s): P passed, F failed, …`). Counts are derived from the actual
per-test outcomes — never a separate tally, never scraped from output. `vis ext
test` exits non-zero when anything fails (it signals failure to the CLI, it
does not kill the process), so it drops straight into CI.

The runner supports the pytest surface the shim implements: plain `assert` with
real introspection, `pytest.raises`, `@pytest.fixture`, `@pytest.mark`
parametrize, `monkeypatch` / `capsys` / `tmp_path`. It is a stdlib
reimplementation of a subset — not upstream pytest (no `conftest.py`, plugins,
or CLI).

## Batteries in the model's sandbox

The model's sandbox ships a few pure-Python, stdlib-only module shims so common
imports work without pip:

- `requests` — an HTTP client over `urllib` (`requests.get/post/...`,
  `Session`, `Response.json()`, …).
- `pytest` — the assertion/fixture/mark surface above (the same shim the test
  runner installs).
- `yaml`, `matplotlib` — YAML round-trip and plotting. `matplotlib` renders
  through a Java2D PNG backend: `plt.show()` is the one display call — it
  paints the figure inline in a graphics-capable terminal (Kitty/iTerm2, e.g.
  Ghostty) and automatically falls back to an ASCII plot on text-only
  terminals — and `savefig` writes a PNG (or `*.txt`/`*.asc`/`format='txt'`
  ASCII, honoring `width`/`height`/`color` kwargs).
- `socket` is imported and ready.

These are compatibility subsets, not the full PyPI packages — enough for
scripting and tests, not a substitute for the real library's every corner.

## LLM providers

`vis.provider(...)` registers a first-class provider the model can actually
route to — the same descriptor a Clojure provider extension builds, minus the
Clojure. Hand it an `id`, a `label`, a `preset` (base URL / API style / default
models), and any of the credential callables:

```python
import os, vis

def _token():
    key = os.environ.get("ACME_API_KEY")
    if not key:
        raise ValueError("set ACME_API_KEY")
    return {"token": key, "api_url": "https://api.acme.ai/v1"}

def _status():
    ok = bool(os.environ.get("ACME_API_KEY"))
    return {"is_authenticated": ok, "source": "env-var", "provider_id": "acme"}

vis.extension(
    name="provider-acme",
    description="Acme AI (OpenAI-compatible) provider.",
    providers=[
        vis.provider(
            id="acme",
            label="Acme AI",
            preset={"base_url": "https://api.acme.ai/v1",
                    "api_style": "openai",
                    "default_models": ["acme-large", "acme-small"]},
            get_token_fn=_token,
            status_fn=_status,
        ),
    ],
)
```

The `preset` flows into the router the same way a built-in provider's does, so
adding `acme` to `~/.vis/config.edn`'s `:providers` (or the TUI *Add Provider*
picker, which lists any labelled provider) makes the model call it. Callable
slots — `get_token_fn`, `detect_fn`, `status_fn`, `logout_fn`, `limits_fn`,
`refresh_token_fn`, `auth_fn`, `auth_prompt_fn`, `enrich_models_fn`,
`on_selected_fn` — are all optional (every function-valued slot carries the
`_fn` suffix); a static-key provider usually just
needs `get_token_fn` + a `preset`. Dict keys may be snake_case or kebab (`api_url` ≡
`:api-url`), `api_style` becomes a keyword, and a Python boolean-predicate key
written `is_<name>` maps to the `:<name>?` the host reads — so a `status_fn` result
returns `is_authenticated` (Python can't spell the trailing `?`), which the
runtime consumes as `:is-authenticated`. `vis providers auth/status/limits
<id>` work against it like any other provider.

For an interactive login, give `auth_fn=` a `def login(printer): ...` — the runtime
hands it a `printer(line)` callback to emit instructions, and its return signals
the outcome (`"ok"` / `"already-authenticated"` = silent success; anything else
surfaces the printed lines so the user knows what to do next). `auth_prompt_fn=`
is a `() -> [line, ...]` for the static guidance shown in the API-key dialog.

Two more optional hooks mirror their Clojure counterparts. `enrich_models_fn=` is a
`def enrich(provider, router_opts): ...` called once at router-build to resolve
each model's real context window — return `[{"name": ..., "context": N,
"is_tool_call": True}, ...]` (the host reads `context` and the `is_tool_call`
predicate as `:tool-call?`), as LM Studio's
built-in provider does. `on_selected_fn=` is a `def on_selected(event): ...`
side-effect hook fired after this provider becomes the active one and config is
persisted; the `event` carries `previous_provider` / `provider` / `config` /
`source`. Both fail soft — a throw is logged and never blocks router build or
selection.

## Python vs Clojure extensions

| | Python (`.vis/extensions/*.py`) | Clojure (classpath) |
| --- | --- | --- |
| Ship | drop a file | build into the binary |
| Reload | `/reload`, in place | rebuild + restart (native) |
| Scope | per project or per user | every install of the distribution |
| Can contribute | tools, prompts, slash, op hooks, state, providers | everything (channels, persistence, TUI, CLI, themes, …) |

Reach for Python for project-specific tools and guards; graduate to a
[Clojure extension](extending.md) when you need deeper
surfaces or want to ship to others as part of a distribution.
