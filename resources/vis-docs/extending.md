# Extending Vis

Vis is a small core plus **extensions**. Everything beyond the engine loop ships
that way: the git surface, the TUI, the Clojure and Python
language packs are all extensions. Your own follow the same recipes.

There are two flavors, and this page is the whole story for both:

| | **[Python extensions](#python-extensions)** (`.vis/extensions/*.py`) | **[Clojure extensions](#clojure-extensions)** (classpath) |
| --- | --- | --- |
| Ship | drop a file | build into the binary |
| Reload | `/reload`, in place | rebuild + restart (native) |
| Scope | per project or per user | every install of the distribution |
| Can contribute | tools, prompts, slash commands, op hooks, network filters, session context, durable state, LLM providers | everything (channels, persistence backends, TUI, CLI, themes, sandbox shims, …) |

Reach for Python for project-specific tools and guards — Vis can write those for
itself mid-session. Graduate to Clojure when you need deeper surfaces or want to
ship to others as part of a
[distribution](distributions.md).

Both flavors converge on the same contracts:

1. **Declare the extension** — one spec per file/namespace.
2. **Expose tools** — plain functions that surface as sandbox Python functions.
3. **Publish one contract per tool** — its docstring (or `:description`), which is
   what `apropos` searches and `doc(name)` returns; prompt fragments only for
   dynamic routing or catalogs.

---

## Python extensions

Single `.py` files you drop into a directory, loaded at startup and reloadable in
place with `/reload` — no rebuild, identical behavior on the JVM and in the
native binary.

```
~/.vis/extensions/           global — loads in every project
<project>/.vis/extensions/   project-local — loads for that project only
```

A project file registering the same extension **name** as a global one wins
(same layering as configuration). A file that fails to load becomes a
warning in `vis-agent doctor` — it never crashes Vis.

A Python extension can contribute:

- **tools** — functions the model calls in its sandbox (`todo_add("x")`)
- **prompt fragments** — constant or recomputed every turn
- **slash commands** — `/todos`, `/gh-repo …` for the user
- **op hooks** — guards that can block file operations
- **durable state** — a key/value store that survives restarts
- **session context** — data folded into the model's `session` bag every turn
- **LLM providers** — register an API-key provider the router can call

Channels, persistence backends, sandbox shims and TUI rendering stay
Clojure-side.

### Hello, extension

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

### `vis.extension(...)`

Exactly one call per file. Keyword arguments:

| Argument | Type | What it does |
| --- | --- | --- |
| `name` | str, required | Unique extension name. Project file with the same name overrides a global one. |
| `description` | str, required | One-liner for `vis-agent extension list` and the model's extensions snapshot. |
| `kind` | str | Section label (`"integration"`, `"guard"`, …). Defaults to `"python"`. |
| `version` | str | Plain metadata. |
| `alias` | str | Python-name prefix for tools. Required when `symbols=` is declared. |
| `symbols` | list of `vis.symbol(...)` | Model-facing tools. |
| `prompt` | str or callable | Model-facing fragment. A callable receives the env dict every turn and returns a string or `None` (no fragment that turn). |
| `activation` | callable | `(env) -> bool`, evaluated per turn; gates the whole extension. Default: always on. |
| `slash_commands` | list of `vis.slash(...)` | User-facing commands. |
| `op_hooks` | list of `vis.op_hook(...)` | Guards/observers over ops, and gates such as `fs_access`. |
| `network_filters` | list of `vis.network_filter(...)` | Request/response policy at the gateway's decrypted HTTP boundary. |
| `providers` | list of `vis.provider(...)` | LLM providers the router can select. |
| `ctx` | callable | `(env) -> dict`, evaluated per turn; the returned dict is deep-merged into the model's `session` bag. See [Session context](#session-context). |
| `env` | list of str | Host environment variables this file may read. See [Environment](#environment). |

The **env dict** passed to `prompt`/`activation` callables is deliberately
small: `{"cwd", "session_id", "channel"}` — unrelated to `env=` below.

### Environment

An extension context gets **no blanket copy of the host environment**; that
would hand every third-party file your AWS, GitHub and Gerrit credentials.
Name what you need and the host injects those values — and only those — into
this context's `os.environ` before the file's first line runs:

```python
import os, vis

vis.extension(
    name="acme",
    description="Acme integration.",
    env=["ACME_API_KEY"],
)

key = os.environ.get("ACME_API_KEY")   # resolved by the host, or absent
```

Each name resolves through the one funnel every Vis surface uses:
**an `environment:` declaration → the workspace's `.env`, then `.env.local` →
the environment that started Vis** (see
[Configuration](configuration.md#environment)). A name nothing resolves is
simply *absent* from `os.environ` — never an empty string, so
`os.environ.get(name) or default` still works.

The **project's own variables need no declaration**: every name written under
`environment:` and every name assigned in the workspace's `.env`/`.env.local`
is offered to the extension alongside what it declared, because those files
belong to the project the extension is running in. `env=` is what a *third*
party's variable needs.

**`env=` does not widen the jail.** It is scoped to *this context's*
`os.environ` and nothing else. A confined child — the model's `shell(...)`,
`vis.jailed_shell(...)` (including one spawned by this very extension), a
managed REPL or test process — is handed only the workspace's `.env`/`.env.local`
plus the `environment:` declarations, so a name that ONLY an extension declared
is absent there. That asymmetry is deliberate and runs the safe way: extension
contexts are trusted and unconfined, so `env=["ACME_API_KEY"]` reads an ambient
host variable even while `jail.environment: declared` withholds it from every
confined child. To give a confined child a variable, declare it under
`environment:` (or put it in `.env`) — see
[the jail's environment scrubbing](jail.md#environment-scrubbing).

### Session context

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

### Tools

```python
vis.symbol(fn, name=None, tag="observation", is_hidden=False)
```

- `tag` declares what the tool does: `"observation"` (reads state) or `"mutation"`
  (changes state) — same contract as Clojure tools.
- The sandbox name is `f"{alias}_{name}"`; `name` defaults to `fn.__name__`
  with a leading `"{alias}_"` stripped, so a module can use readable full
  names (`todo_add` under alias `todo`) without double-prefixing.
- **The docstring is mandatory** — it is the whole contract. `apropos(text)`
  searches it and `doc(name)` returns it verbatim, so it is the only place the
  tool is described. House style: `await name(args) -> shape` on the first line,
  because that first line is what the curated `doc()` index prints.
- Parameter names are read from the real signature and shown to the model.
- `is_hidden=True` hides the tool from the model-facing listing (still callable).
- Boolean keys keep **one spelling** across the boundary: a Python `is_<name>` key
  is the Clojure `:is-<name>` keyword — the same mechanical `_` ↔ `-` mirror the
  gateway wire uses (`wire-key` / `engine-key` in `gateway/wire.clj`). A provider's
  `is_authenticated` → `:is-authenticated`, `is_unlimited` → `:is-unlimited`,
  `is_hidden` → `:is-hidden`. The only exceptions are the few keys svar's router
  itself spells with a trailing `?` (`is_tool_call` → `:tool-call?`); those map
  through one named table instead of by convention.

**A tool is a Python function, never a provider tool.** `python_execution` is the
only thing a model is handed a schema for; every symbol an extension registers is
a bare name inside that sandbox, called like any other Python function. There is
no JSON Schema to write, no `description=`/`result=` pair to keep in sync with the
docstring, and no per-symbol renderer: a printed result is carded from the value
itself.

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

### Slash commands

```python
vis.slash(name, run, doc=None, usage=None)
```

`run(ctx)` receives `{"channel", "args", "raw", "session_id"}` and returns:

```python
vis.ok(title, body=None, data=None)    # body: Markdown string
vis.err(title, body=None, data=None)
```

(or a plain string, treated as an ok title).

### Op hooks

```python
vis.op_hook(ops, fn, phase="before")
```

- `ops` — sandbox tool names to hook: `"struct_index"`,
  `"struct_patch"`, `"shell"`, `"python_execution"`, … or a GATE op
  (below), which is a different shape and may not share a hook with them.
- `phase="before"` — `fn(call)` receives `{"op", "args"}` **before** the op
  runs. Return `vis.block(reason)` to refuse it (the model sees the reason
  as a tool failure) or `None` to allow. A hook error fails open.
- `phase="after"` — `fn(call)` receives `{"op", "args", "result"}` after the
  op; observe-only (the return value is ignored).

**Gate ops.** `"fs_access"` is not a tool: it is asked for every path the
engine's editors AND the Python interpreter touch, so `open(p, "w")`,
`Path.unlink` and `shutil.move` are refused by the same rule as `grep` and
`struct_patch` — a guard cannot be routed around by picking another tool or another
language. `fn(access)` receives `{"operation": "file-read" | "file-write",
"path": <absolute path>}`, returns `vis.block(reason)` to refuse or `None` to
allow, and `phase` says nothing (the operation has not run yet, so declaring
one is refused at the call site). A gate fails **closed**: an error inside the
hook refuses the operation, because a boundary that opens when its guard breaks
is not a boundary. See `resources/examples/python-extensions/protected_paths.py`.

`vis.strings_of(value)` collects every string leaf of a nested structure —
handy for scanning op args for paths.

### Durable state

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

### Logging and notifications

```python
vis.log("info", "loaded 3 rules")       # levels: trace debug info warn error
vis.notify("Rules reloaded", "success") # user-facing toast: info success warn error
```

`vis.log` writes to `~/.vis/vis.log`; `vis.notify` shows in whatever channel
is active (TUI banner, web toast, …).

### Asking the human

`vis.ask` pauses the extension and asks the person a typed question, then
blocks until they answer. The request is published to whatever channel is
active, so the same call works in the TUI, the web UI, or the companion app.

A question belongs to a **session**: it is asked inside a turn somebody is
watching, and the session is what the answer travels back on. `vis.ask` takes
the session of the call that is running — a tool, a slash command, a hook — and
refuses on the spot when there is none, rather than parking on a dialog nobody
can see. Provider lifecycle callbacks (`detect_fn`, `status_fn`) run outside any
session, so they must never ask; keep them non-interactive and quick, and read
credentials from the environment instead. `vis.check` needs no session at all:
it only judges a request.

```python
answer = vis.ask("Deploy", [
    {"name": "env", "label": "Target", "description": "Where this deploy lands.",
     "type": "select", "options": ["staging", "prod"], "is_required": True},
    {"name": "notes", "type": "multiline", "label": "Release notes"},
    {"name": "token", "type": "password", "label": "Deploy token"},
], description="Pick a target", timeout_ms=120000)

if answer:                       # falsey when cancelled or timed out
    deploy(answer["env"], answer.reveal("token"))
else:
    vis.log("info", "deploy skipped: " + answer.reason)
```

Every key is a **snake_case string** — `is_required`, `max_length`, `timeout_ms`.
A camelCase or kebab-case key (`isRequired`, `is-required`) is **refused** with an
error naming the right spelling; it is never accepted and quietly ignored, so a
mandatory field can never turn optional behind your back. (Clojure callers write
the same names as kebab keywords: `:is-required`.)

Three names, three jobs: `name` keys the answer in `answer.values`, `label` is
what the dialog shows above the input, and `description` is the italic line under
that label. `id` is the same identity as `name` and may be written instead of it;
every other key has exactly one spelling, and an unknown one is refused by name.

Field `type` is one of `plaintext`, `password`, `multiline`, `select`,
`multiselect`, `checkbox`, `range`, `otp`. A field may also carry
`placeholder`, `default`, `is_required`, `min_length`, `max_length`, `validate`,
and — for the two `select` types — `options` (strings, or `{"value": ..., "label": ...}` maps).

Two node types are not fields at all, because nothing answers them: a `group`
LAYS OUT the nodes under it (`fields`, `direction`) and a `heading` or
`paragraph` is pure DECORATION carrying `text` alone. Both are described below.

#### Toggles: exclusive `select`, inclusive `multiselect` and `checkbox`

There are two kinds of toggle, and every surface says which one it is drawing
before the person touches it:

| Field | Rule | Answer | TUI | Companion app |
| --- | --- | --- | --- | --- |
| `select` | **exclusive** — exactly one option | the chosen `value` | `●` / `○` dots, `Space` **picks** | radio group |
| `multiselect` | **inclusive** — any number of options | list of values, in the order the request declared them | `[✓]` / `[ ]` boxes, `Space` **toggles** | pressed toggles |
| `checkbox` | **inclusive**, one lone switch | `true` / `false` | `[✓]` / `[ ]` box on the label row | pressed toggle |

An exclusive option has no off state: `Space` moves the single choice onto the row
under the cursor and never clears it. The TUI starts a `select` on its `default`, or
on the first option when the spec names none, so an exclusive field is never left
unanswered by accident. An inclusive option is independent: `Space` adds or drops
exactly that value, and an empty list is a legal answer unless the field says
`is_required`.

```python
answer = vis.ask("Release", [
    {"name": "channel", "type": "select", "label": "Channel",
     "options": ["testflight", "appstore"], "default": "testflight"},
    {"name": "stores", "type": "multiselect", "label": "Also ship to",
     "options": ["ios", "android", "web"], "default": ["ios"], "is_required": True},
    {"name": "notes", "type": "checkbox", "label": "Attach release notes"},
])
# answer.values == {"channel": "appstore", "stores": ["ios", "web"], "notes": True}
```

An `options` entry is either a plain string, where the value IS the label, or a
`{"value": ..., "label": ...}` map when the answer key and the shown text differ.
An option nobody declared is refused, not dropped: a `multiselect` answers
`unknown option zz` and a `select` answers `must be one of a, b`, and the dialog
turns red on that one field. Shape follows the kind of toggle, and only the inclusive
side is forgiving about it: one bare value counts as a list of one, while a list
handed to an exclusive `select` is not one of its options and answers nothing.

A `range` is a bounded number: `min` (default 0), `max` (default 100) and `step`
(default 1). It answers with a number, not a string — a `long` when all three
bounds are whole, otherwise a `double` — and the engine clamps nothing: a value
outside `[min, max]` is refused like any other bad answer. The TUI draws it as a
slider you nudge with `←`/`→` (`Home`/`End` for the bounds); the app draws a real
slider.

An `otp` is a one-time code, entered as one box per digit. `min_length` and
`max_length` say how many (default 6, at most 12; give only `max_length` for a
fixed length). Only digits get in — typing a letter does nothing, and pasting
`123 456` fills the boxes instead of dropping six characters into one. A code is
a credential, so — like a `password` — it answers with a `vis-secret:` handle,
and the terminal fills its boxes with `•` rather than the digits.

### Laying fields out: `group`

A `group` answers nothing — it is a layout node, one flexbox line. It carries
`fields` (its children) and `direction`: `column` stacks them (the default, and
what a form has always done) and `row` lays them side by side. A child may be a
group again, so the two directions compose into any arrangement:

```python
vis.ask("Where should the pool connect?", [
    {"type": "group", "label": "Server", "direction": "row", "fields": [
        {"name": "host", "label": "Host", "is_required": True},
        {"name": "port", "label": "Port",
         "validate": lambda port: None if port.isdigit() else "digits only"},
    ]},
    {"type": "group", "direction": "column", "fields": [
        {"type": "group", "direction": "row", "fields": [
            {"name": "size", "label": "Pool size"},
            {"name": "idle", "label": "Idle (s)"},
        ]},
        {"name": "tls", "label": "Require TLS", "type": "checkbox"},
    ]},
])
```

A group needs no `name` (it derives one from its children) and shows a heading
only when you give it a `label`; `description` works as everywhere else. Layout
keys and value keys never mix, in either direction: `default`, `placeholder`,
`options`, `is_required` and `validate` are **refused** on a group that could
never use them, and `fields` or `direction` on an answerable field is refused
too — a mistyped layout is an error, never a form that quietly loses half its
fields.

Grouping is layout and nothing else. `answer.values` stays **flat** — the leaves
key the answer, whatever the arrangement — errors come back keyed by the leaf,
`is_required` and every validator run exactly as before, and a two-argument
validator is handed that same flat map, so a field can compare itself with a
field in any other group. Names stay unique across the whole tree, so two groups
cannot both hold a `host`.

The group crosses the wire as a tree — `{"type": "group", "direction": "row",
"fields": [...]}` — so both surfaces read one layout instead of inventing their
own. The TUI splits the band's width across a `row`: each column keeps its own
label, required `*`, error line and cursor, `←`/`→` still move inside a
field and `↑`/`↓` between them in reading order. The app renders a
`fieldset`/`legend` with the same row or column flex, wrapping on a phone instead
of overflowing.

### Pure decoration: `heading` and `paragraph`

A form is not only questions. A `heading` opens a section of a long form and a
`paragraph` explains one. Both carry `text` and nothing else — no `name`, no
`label`, no `default`, no `validate` — because nothing answers ink:

```python
vis.ask("Deploy", [
    {"type": "heading", "text": "Target"},
    {"type": "paragraph", "text": "Staging pages nobody. Production pages the on-call."},
    {"name": "env", "type": "select", "label": "Env", "options": ["stg", "prod"]},
    {"type": "heading", "text": "Credentials"},
    {"name": "key", "type": "password", "label": "Deploy key", "is_required": True},
])
```

A decoration has no identity, so it is never keyed, never focused and never in
`answer.values`: `Tab` and `↑`/`↓` step straight over it, and two paragraphs
saying the same words are two decorations rather than a name collision. Give one
a `name`, a `default` or a `validate` and the request is **refused** — that spec
meant to ask something. A decoration sits anywhere a field does, including inside
a `group`, so a `row` of two paragraphs is a two-column note.

The TUI paints a `heading` bold in the dialog's own ink and a `paragraph` in
dimmed italics, both straight on the dialog paper: the pale input surface belongs
to things you can type into, and decoration is not one of them. The app renders
the same two as an `h3` and a `p`.

### Builders instead of dicts

A field is a dict, and a dict is easy to misspell. The `vis` module ships one
builder per node type: the type is the function you call, and the node is checked
the moment it is built, so a bad `default` or an unknown key raises at the line
that wrote it instead of in front of the human.

```python
form = vis.column(
    vis.heading("Target"),
    vis.paragraph("Staging pages nobody."),
    vis.row(
        vis.select(
            "env",
            [vis.option("staging", "Staging"), vis.option("prod")],
            is_required=True,
        ),
        vis.slider("canary", label="Canary %", min=0, max=100, step=5, default=10),
    ),
    vis.checkbox("ack", label="I read the runbook", is_required=True),
    vis.password("token", label="Deploy token", is_required=True),
)

answer = vis.ask("Deploy", [form], submit_label="Ship it")
```

The full set: `plaintext`, `password`, `multiline`, `select`, `multiselect`,
`checkbox`, `slider`, `otp`, `option`, `row`, `column`, `heading`, `paragraph`.
The range field is spelled `slider` so it never shadows the `range` builtin;
everything else is the wire type. Keyword arguments are the field keys you
already know (`label=`, `description=`, `default=`, `is_required=`, `min=`,
`validate=` ...) and are passed through untouched, so there is no second
vocabulary beside the one above.

Buttons are not nodes: a dialog has exactly the two a dialog has, and you name
them with `submit_label=` and `cancel_label=` (or drop the second one with
`is_cancellable=False`).

### Checking a form without asking

`vis.check(title, fields, **options)` runs the very same validator `vis.ask`
runs and then throws nothing away: it returns `None` when the request is valid,
and the one-line reason when it is not. No dialog opens and nobody is
interrupted, so it is what a test asserts on and what a tool can call before it
decides to ask.

```python
vis.check("Deploy", [vis.select("env", [])])
# -> 'Invalid human-input field env: select needs at least one option'

vis.check("Deploy", [vis.plaintext("who"), vis.password("who")])
# -> 'Invalid human-input request: field names must be distinct'

vis.check("Deploy", [vis.plaintext("who")])
# -> None
```

`vis-agent extension check <path>` is this same seam applied to a file you have
not run: see **Checking an extension before it runs** below.

### Validating a field

`validate` is a **function** — or a list of functions — that you write. There is
no rule language: a validator takes the coerced answer and returns `None` (or
`True`) to accept it, or the error message as a **string** to refuse it.

```python
def a_slug(text):
    if not re.fullmatch(r"[a-z][a-z0-9-]*", text):
        return "lowercase, digits and dashes"
    if len(text) > 32:
        return "at most 32 characters"


def is_free(name):
    return None if name not in TAKEN else "already taken"


answer = vis.ask("Sign up", [
    vis.plaintext("email", label="Email",
                  validate=lambda text: None if "@" in text else "must be an email"),
    vis.plaintext("slug", label="Project", validate=[a_slug, is_free]),
    vis.password("pass", label="Password",
                 validate=lambda text: "at least 12 characters" if len(text) < 12 else None),
    vis.password("again", label="Repeat it",
                 validate=lambda text, values:
                     None if text == values["pass"] else "the two do not match"),
])

if answer:
    print(answer["slug"])          # already validated, already coerced
```

A list runs **in order** and the first message wins, which is how a cheap shape
check comes before the expensive lookup behind it.

A validator takes **one** argument (the value) or **two** (the value and every
answer in the form — flat, whatever the layout); that second argument is how one
field compares itself with another, across groups included. Any other shape is
refused where you wrote it - by `vis.ask` and `vis.check` when the request is
built, and by `vis-agent extension check` without running the file at all - never
in front of the human who is finally typing:

```python
vis.check("Sign up", [vis.plaintext("email", validate=lambda: None)])
# -> 'a validate function takes the value, or the value and every value - this
#     one takes neither'

vis.check("Sign up", [vis.plaintext("email", validate=r"[a-z]+")])
# -> 'validate is a function, or a list of functions, taking the value (and
#     optionally every value) and answering None or a message string'

vis.check("Sign up", [vis.plaintext("email", validate=a_slug)])
# -> None
```

`False`
refuses with `is not valid`, anything else is refused with its own text, and a
validator that raises refuses with `could not be validated: <the exception>` — a
broken check never swallows an answer silently. A validator never fires on a
blank answer: emptiness is `is_required`'s job.

The functions stay where you wrote them. They run **in the engine**, calling back
into your own extension, and they never cross the wire: `validate` is stripped
from the field before any surface sees it, so a validator cannot be read,
replayed or re-run by a client. (Clojure callers pass ordinary fns and get the
same contract.)

**Validation happens once, on confirmation.** Nothing is checked while you type:
a pristine form says nothing, and pressing Enter — or *Submit* — always sends.
The engine then runs `is_required` and every validator over the whole answer and
either accepts it or answers with one message per broken field; only then does
the dialog redden those fields and put the cursor on the first one. The next
**touch** of a field — a keystroke, a backspace, a tick, a nudge — clears *that*
field's error and only that one, and nothing is re-checked until you confirm
again. The TUI and the app behave identically, because both are rendering the
same engine decision.

The DIALOG itself takes the same optional `description`: prose under the title
that says what the whole ask is about, before the operator reads a single field.
It wraps onto as many lines as it needs, so a sentence is safe there. The other
request options are `submit_label`, `cancel_label`, `is_cancellable`, and
`timeout_ms`.

**A dialog either has a deadline or it has none.** `timeout_ms` is the wait an
extension is willing to bill: 5 minutes when the spec says nothing, any number of
milliseconds it names, or `0` to wait **indefinitely**, until the human answers
or cancels, however long that takes. Nothing is capped and nothing is guessed: an
extension that must not proceed without a human says `0` and parks, and one that
can carry on alone names its wait and gets a falsey `Answer` whose `reason` is
`timeout` the moment it runs out. Either way the dialog leaves every surface at
the same instant the extension resumes, so a form nobody can answer is never left
on screen.

`is_required` is enforced, not decorated: every dialog marks the field with a red
`*` next to its label, and a blank one is refused on confirmation — by
the engine, so an answer that arrives straight over HTTP is judged by exactly the
same rule as one typed into a dialog. A required `checkbox` has to be ticked —
`false` is not an answer to it.

`vis.ask` never raises for a refusal: cancelling or timing out returns a falsey
`Answer` whose `reason` says which (`cancelled`, `timeout`, or whatever reason
the host cancelled with). `answer.values` always carries every field, defaults
included. A run with no surface mounted to show the dialog answers
`undeliverable` immediately — and logs an error naming the request — instead of
parking your extension until the timeout.

**Secrets never round-trip as plaintext.** A `password` and an `otp` field both
answer with an
opaque `vis-secret:` handle; `answer.reveal("token")` (or `vis.reveal(handle)`)
resolves it inside the process, and `vis.forget(handle)` drops it. Handles are
what the transcript, logs, and the model see — so pass the handle around and
reveal it only at the moment of use.

### LLM providers

`vis.provider(...)` registers a first-class provider the model can actually
route to — the same descriptor a Clojure provider extension builds, minus the
Clojure. Hand it an `id`, a `label`, a `preset` (base URL / API style / default
models), and any of the credential callables:

```python
import os, vis

def _token():
    # `env=["ACME_API_KEY"]` below is what puts this in os.environ.
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
    env=["ACME_API_KEY"],
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
adding `acme` to `~/.vis/config.yml`'s `providers` (or the TUI *Add Provider*
picker, which lists any labelled provider) makes the model call it. Callable
slots — `get_token_fn`, `detect_fn`, `status_fn`, `logout_fn`, `limits_fn`,
`refresh_token_fn`, `auth_fn`, `auth_prompt_fn`, `enrich_models_fn`,
`on_selected_fn` — are all optional (every function-valued slot carries the
`_fn` suffix); a static-key provider usually just
needs `get_token_fn` + a `preset`. Dict keys may be snake_case or kebab (`api_url` ≡
`:api-url`), `api_style` becomes a keyword, and a Python boolean-predicate key
written `is_<name>` maps to the `:<name>?` the host reads — so a `status_fn` result
returns `is_authenticated` (Python can't spell the trailing `?`), which the
runtime consumes as `:is-authenticated`. `vis-agent providers auth/status/limits
<id>` work against it like any other provider.

Where a slot's output lands differs. `status_fn` answers *am I connected*: the host
reads `is_authenticated` (plus an optional `error`) for the provider dot and for
routing, and any extra key you return is shown in the status dialog only. **Live
quota is a different slot** — only `limits_fn` feeds the TUI footer's usage line.
It returns `{"provider_id": ..., "status": "ok", "dynamic": {"limits": [row, ...]}}`,
and every row must carry `id`, `label`, `scope` (`account` / `plan` / `workspace` /
`model`), `kind` (`requests` / `tokens` / `usd` / `credits` / `sessions` / `rate`),
`precision` (`exact` / `estimate` / `derived` / `unknown`), `source` (`provider-api` /
`derived` / `static` / `local`) and `is_unlimited`; `used`, `limit`, `remaining`,
`subject`, `note` and `window` (`{"kind": "calendar"|"rolling"|"lifetime", "unit":
..., "size": N, "resets_at_ms": ...}`) are optional. `is_authenticated` and
`is_unlimited` are the two keys kept **verbatim** — every other `is_<name>` still
becomes `:<name>?`. One missing required key invalidates the whole report and the
footer renders `limits: error`, so check yours with `vis-agent providers limits <id>`.

**Provider callbacks run at process level, not inside a session.** `detect_fn`,
`status_fn` and `limits_fn` are called while Vis starts, while the provider picker
and *Settings → Providers* paint, and from the footer's own polling thread —
moments when no turn is being handled at all. `subprocess`, `os.system`,
`vis.shell` and `vis.jailed_shell` work with or without a session and are what a
credential helper must use here; `vis.jailed_shell_session` and `vis.ask` require
a live turn and refuse with *available only while handling a session*. Write these
slots so they can answer with no session — cache a minted credential on disk
rather than shelling out to mint one at render time. When a callback *is* invoked
from a caller that has a session, the host keeps that session around it.

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

### Execution model and trust

Extension files run in **trusted GraalPy contexts** — one per file, separate
from the model's sandbox:

|  | Model sandbox | Extension context |
| --- | --- | --- |
| Who writes the code | the model | **you** |
| Filesystem | confined to workspace roots | **real, unrestricted** |
| Network / subprocess | gateway policy / restricted | **real, unrestricted** — output captured, not on your terminal |
| Environment variables | resolved project + declared values | declared (`env=`) + the project's own `environment:` / `.env`; never a blanket copy of the host's |
| Lifetime | per session | process (rebuilt on `/reload`) |

This is an intentional trust decision, not a missing sandbox feature. Extension
contexts allow full IO, process creation, threads and sockets because they are
user-installed plugins; their environment stays the declared one
([Environment](#environment)). They still deny
arbitrary host-class, native, and polyglot interop; host access is limited to the
bound `vis` API. The model can call an exported tool but cannot evaluate code in
the extension context. See [Process jail and gateway egress](jail.md).

**Output is captured, never inherited.** Unrestricted means the process runs with
your permissions, not that it owns Vis' terminal. Any stream the extension does
not read itself — a child's stdout/stderr, the context's own `print()` and
tracebacks — is piped, drained and logged under the extension's file name, so it
lands in the diagnostic log instead of on whatever terminal happens to own the
running Vis. Five consequences worth knowing before you write one:

- A child that asks `isatty()` about an uncaptured stream now sees a pipe:
  progress bars render plain and a genuinely interactive child cannot work.
- Read the bytes yourself when you need them (`capture_output=True`,
  `stdout=subprocess.PIPE`, `check_output`) — you get every byte, in order, as
  it arrives.
- A file or descriptor redirect (`stdout=open(...)`, `stderr=open(...)`,
  `stdout=os.open(...)`, `stdin=open(...)`) reaches the file you named, and the
  file is complete once the call returns. GraalPy itself discards such a
  redirect, so Vis translates it into a pumped pipe on your behalf; a sink with
  no descriptor (a `BytesIO`) raises `io.UnsupportedOperation`, exactly as on
  CPython. `stdout=sys.stdout` goes to the extension's log rather than to
  descriptor 1, which belongs to the JVM and not to your extension.
- A stream you asked for but never read does **not** deadlock the child, unlike
  CPython: Vis keeps reading the pipe into a backlog of up to 8 MiB per stream,
  so `Popen(stdout=PIPE)` followed by `wait()` still completes. Beyond that the
  child is throttled until you read — never dropped — so a child that streams
  without end still needs you to read it or close the stream.
- `Popen.pid` is the child's real OS pid, so `ps`, `lsof`, a pidfile or your own
  supervisor all find it, and `poll()`, `wait()`, `terminate()` and
  `os.kill(p.pid, sig)` work on it. GraalPy's emulated posix would otherwise
  hand you a per-context child-slot index (`1`, `2`, ..., reused after a reap);
  Vis replaces it and keeps the index on `__vis_virtual_pid__` for its own use.
  A pid held past `wait()` names a dead process rather than another child.

Ordinary process paths stay trusted and unrestricted: `subprocess`, `os.system`,
`os.popen`, and `vis.shell({...})` ignore the process jail even when a session has
one enabled. Use a jailed shell only when you want confinement:

| API | Policy source | Session required? |
| --- | --- | --- |
| `vis.shell({...})` | None; trusted and unrestricted | No |
| `vis.jailed_shell({...})` | Latest merged config on disk, read and validated at **each process spawn** | No |
| `vis.jailed_shell_session({...})` | Invoking session's immutable security snapshot | Yes |

`vis.jailed_shell` reads the normal merged global, state, project `vis.yml`, and
project `.vis/config.yml` sources without requiring `/reload`. Invalid current
config fails closed: that spawn is refused rather than falling back to an older
policy. A background process keeps the policy captured when it started; config
changes apply when a new process is spawned, not retroactively.

Treat `.py` files in a project's `.vis/extensions/` like you treat its
`deps.edn`: they execute with your user's permissions when Vis starts in
that checkout — review before running Vis in untrusted repositories.

Calls into an extension are **serialized** (one at a time per file). Keep
per-turn callables (`prompt`, `activation`) fast; tools may take their time.

### Reloading

- `/reload` — tears down every Python extension (contexts closed) and loads
  the current files fresh. State survives (it lives in the database).
- Changes propagate to LIVE sessions immediately: new/changed slash
  commands dispatch right away and reloaded tools rebind into the sandbox
  — no restart, no new session.
- A process start and `/reload` are the ONLY things that pick an edit up: a
  running Vis keeps serving the files its own start (or the last `/reload`)
  loaded, so editing a `.py` — or anything else editing one — changes nothing
  until you reload. Unchanged files are fingerprint-checked, so reloading
  untouched extensions is a no-op.
- The whole extension is frozen, not just its entry file. At load, the import
  root (a package directory, or the extensions directory for a single-file
  extension) is copied into a private temp tree and THAT copy is what
  `sys.path` sees, and the reload check hashes every `.py` under it. A lazy
  `import helper` inside a tool therefore runs the bytes the load admitted:
  editing a sidecar module after the load changes nothing until `/reload`
  either. A symlinked module or package inside the root is followed and frozen
  with it, so a package you develop elsewhere and link into
  `~/.vis/extensions` behaves like any other file — including waiting for
  `/reload`. Files an extension writes next to itself land in the frozen copy —
  durable state belongs in `vis.state`.
- `vis-agent doctor` lists every loaded file and every load failure with its
  Python error.

### Multiple files and packages

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

- The frozen copy of the directory is prepended to `sys.path` before
  `extension.py` runs, so `import mypkg` / `from mypkg.core import add` just
  work — no manual `sys.path.insert(...)`.
- Only `extension.py` is an entry point; the package's other modules are
  imported by it, never scanned as separate extensions.
- A plain top-level `.py` file gets the same sugar for a sibling module or
  package placed next to it.

So an ordinary Python project becomes a Vis extension by adding one
`extension.py` on top that imports it.

### Checking an extension before it runs

```
vis-agent extension check                       # every file that would load
vis-agent extension check .vis/extensions/deploy.py
```

The check never runs your extension. The file is parsed, and three questions get
answered from the parse tree alone:

* does it parse at all;
* does it only reach for `vis.<name>` that the `vis` module actually has, so
  `vis.plaintxt(...)` is caught here instead of in front of a human;
* would every `vis.ask(...)` / `vis.check(...)` request be accepted - judged by
  the same engine seam `vis.check` uses, never a second opinion about it.

That is possible because the builders are pure: reconstructing
`vis.select("env", [])` builds a dict and touches nothing else. An argument that
cannot be known without running the file (a field list handed in as a parameter,
an f-string title, a comprehension) is reported as **skipped** rather than
guessed at.

A `validate=` function is judged too, by SHAPE and without ever being called: a
lambda or a `def` in the file stands in for one taking exactly the arguments it
declares, so `validate=lambda: None` and `validate=takes_nothing` are refused
here for the same reason `vis.ask` refuses them. A validator that arrives from
somewhere the parse tree cannot see is assumed to be the ordinary one value
shape rather than reported.

```
FAIL .vis/extensions/deploy.py  (2 forms checked, 1 skipped)
  .vis/extensions/deploy.py:31:12: invalid-request: Invalid human-input field env: select needs at least one option
  .vis/extensions/deploy.py:44:12: unknown-attribute: the vis module has no plaintxt
1 file, 2 forms checked, 2 problems
```

The exit code is `1` when anything was refused, so it drops straight into a
pre-commit hook or CI. A file that registers nothing (`no-extension`) and a file
that cannot be read (`unreadable`) are problems too - a run over a directory
always reaches the last file and reports every one of them.

### Testing your Python extension

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
vis-agent extension test     # from the shell — prints a report, exits non-zero on failure
```

The report is **per test**: each `test_*` shows ✓/✗ with the failing
assertion's detail, grouped by file, under a one-line summary
(`✓ N file(s): P passed, F failed, …`). Counts are derived from the actual
per-test outcomes — never a separate tally, never scraped from output. `vis-agent extension
test` exits non-zero when anything fails (it signals failure to the CLI, it
does not kill the process), so it drops straight into CI.

The runner supports the pytest surface the shim implements: plain `assert` with
real introspection, `pytest.raises` / `warns` / `approx`, `@pytest.fixture`
(including `params=`, `ids=`, `request`, `getfixturevalue` and indirect
parametrize), `@pytest.mark` parametrize / skip / xfail / usefixtures, the
built-in `monkeypatch` / `capsys` / `tmp_path` / `tmp_path_factory` / `caplog` /
`recwarn` / `pytester` fixtures, `conftest.py` in disk mode, and the `-k` / `-x`
/ `--maxfail` selection flags. It is a stdlib reimplementation of a subset — not
upstream pytest (no plugins, no assertion-rewriting import hook).

### Batteries in the model's sandbox

The model's sandbox ships pure-Python, stdlib-only module shims so common
imports work without pip. Each one is a real `.py` file under
`resources/vis-shims/`, published into every sandbox context (main session and
every `sub_loop` fork) and loaded lazily on first import:

- Data / formats — `numpy`, `pandas`, `yaml`, `toml`, `tabulate`, `sqlite3`,
  `brotli`.
- HTTP / web — `requests`, `httpx`, `urllib3`, `bs4`.
- Documents / media — `anydoc` (any document as Markdown, and any question about
  it as citations — see below), `PIL`, `matplotlib`, `pptx`, `xlsxwriter`,
  `fontTools`.
- Time — `zoneinfo` (604+ zones from `java.time`), `dateutil`.
- Ops / testing — `paramiko`, `pytest` (the same shim the test runner installs).
- Globals, no import needed — `attach`, `list_attachments`, `get_attachment`,
  `read_attachment`, `show_attachment`, plus `nippy_encode` / `nippy_decode`.
  `list_attachments()` and `get_attachment(...)` return descriptor dicts (id,
  filename, version, media type, kind, size, audience, and the `turn_id` it
  belongs to — a tool artifact adds `iteration_id` / `tool_call_id`);
  `read_attachment(...)` returns the raw bytes and nothing else.
  **Same document, same name**: a revision goes back under the filename it
  already had and is stored as that artifact's next **version**, never
  `report_v2.png` beside `report.png`; a fresh name is a different document, and
  `list_attachments(name)` walks the thread.

  **Name vs id — one addressing rule.** The filename is the artifact, an id is
  one exact stored version of it. Every read call takes either as its first
  argument: a filename resolves to the latest cut unless you pass a `version`
  (negative counts back from the latest), an id resolves to that one cut.

`matplotlib` renders through a native `imaging` PNG backend: `plt.show()` paints the
figure inline in a graphics-capable terminal (Kitty/iTerm2, e.g. Ghostty) and
falls back to an ASCII plot on text-only terminals; `savefig` writes a PNG (or
`*.txt`/`*.asc`/`format='txt'` ASCII, honoring `width`/`height`/`color`).
`mpl_toolkits.mplot3d` / `projection='3d'` is real: `plot_surface`,
`plot_wireframe`, `contour(offset=…)`, 3-D `scatter`/`plot`/`text`, `bar3d` and
`view_init` go through a painter's-algorithm camera with shading and colormaps,
and the ASCII backend projects the same scene into braille.
`subprocess`, `os.system` and `os.popen` never spawn in the agent sandbox: they
raise and name the sandbox's `shell(...)` call, which is the one door to a process. (Trusted
extension code, outside the sandbox, keeps the real `subprocess`.)

These are compatibility subsets, not the full PyPI packages — enough for
scripting and tests, not a substitute for the real library's every corner. Each
shim's `:shim/description` names what it does NOT support, and the authoring
contract lives in [Sandbox shims and autoloads](#sandbox-shims-and-autoloads)
below — a Clojure-extension capability, since a shim needs host callables.

#### Asking a document a question — `anydoc`

`anydoc` converts anything the native `imaging` cdylib reads (PDF, DOCX, XLSX,
PPTX, HTML, CSV, EPUB, Markdown, …) and answers questions **about** it with
citations, so nothing has to paste a 200-page PDF into a context window:

```python
import anydoc

hits = anydoc.search('"quarterly revenue" +march -draft', "/data/reports")
for c in hits[:5]:
    print(c)                       # q1.pdf p.7 line 12 > Revenue > …
    print(c.snippet, c.page, c.section, c.score)
print(hits.total_matches, hits.is_truncated, hits.skipped)
print(hits.explain())              # the parse, the filters, the ranking
```

Reading is separate from asking: `anydoc.read(path)` gives a `Document`
(`.markdown .text .blocks .pages .assets .outline()`), `anydoc.to_markdown(…)`
the Markdown alone, and `doc.search(…)` asks one document you already hold.
`sources` is a path, a directory (walked), bytes, a `Document`, a list of any of
those, or a `{id: source}` mapping when the ids — or the names those bytes need
to be read at all — are yours to choose.

`blocks` are the document's OWN structure, straight from the cdylib: heading,
paragraph, list item, table row (with its cells), code, note — each carrying its
page, line, breadcrumb path and character offsets. That is what makes a citation
possible in a format that has no lines, and what a phrase crosses when it wraps.

**Query language** — `anydoc.explain_query(q)` parses one without reading a file:

| Query | Means |
| --- | --- |
| `march revenue` | bare terms, ANY may match, ranked by BM25 |
| `"quarterly revenue"` | a phrase — crosses line wraps AND table cells |
| `revenue +march`, `revenue AND march` | the document MUST contain `march` |
| `revenue -draft`, `revenue NOT draft` | the document must NOT contain `draft` |
| `rev*` | prefix |
| `NEAR(revenue march, 8)` | within 8 words of each other |
| `/reven[us]e?/` | a regular expression over folded text |
| `heading:march` | headings only; also `table: list: code: note: paragraph:` |
| `revenue section:Revenue`, `revenue page:3` | filters — under that heading, on that page |

Exclusions and filters only NARROW a search, so a query needs at least one thing
to look FOR: `-draft` or `page:3` on its own is refused, pointing at the
character and saying what to add.

Both the corpus and the query are FOLDED before matching, so `efficient` finds a
PDF's `ﬁ` ligature, `Zurich` finds `Zürich`, `HAUPTSTRASSE` finds `Hauptstraße`,
`don't` finds Word's curly apostrophe, `quarterly` finds a `quar-` / `terly`
hyphen break across two lines, and
`payments` finds `payment`. `fold=False`, `stem=False`, `ignore_case=False` and
`whole_word=False` each turn one of those off. `limit` / `per_document` cap what
is RETURNED and never what is counted (`total_matches`, `is_truncated`),
`snippet` and `mark=("**", "**")` shape the quote, `context` adds neighbouring
lines as `.before` / `.after`, and `kinds` / `pages` / `format` / `order` narrow
without touching the query.

Every hit is a `Citation`: `.document_id .format .page .section .path .line
.column .offset .end .match .text .snippet .highlight .score .cell .block_kind`.

Refusals are typed and say what to do about it: `QueryError` points at the
character it choked on, `DocumentError` carries `.document_id` and `.format`,
`SourceError` rejects something that cannot be a document — all `AnydocError`,
and each also the builtin (`ValueError` / `TypeError`) a caller would have
caught anyway. A file merely FOUND under a directory never ends a search: it
lands in `results.skipped` with its reason. A term nothing matched comes back in
`results.suggestions` (`{"marhc": ["march"]}`), which is how a typo answers.

Conversions are cached in the host on the content HASH (LRU, byte-budgeted), so
`doc.search(…)` and a second question about the same corpus convert nothing —
`anydoc.cache_info()` is the proof (`hits` climbs, `misses` does not) and
`anydoc.clear_cache()` empties it.

---

## Clojure extensions

Libraries on the classpath that register tools, providers, channels, language
packs, sandbox shims and slash commands. They compile into the binary and reach
every surface Vis has.

### How extensions load

Discovery is classpath-wide and manifest-driven. Each extension jar ships **one resource**:

```
resources/META-INF/vis-extension/vis.edn
```

```clojure
{weather {:nses [com.acme.ext.weather.core]}}
```

At startup Vis scans every `META-INF/vis-extension/vis.edn` on the classpath and `require`s each namespace listed under `:nses` exactly once. Your namespace's top-level `(vis/register-extension! …)` fires during that require — that's the whole registration protocol. A namespace that throws during load doesn't crash Vis; the failure is surfaced as a warning to both the user and the model.

Getting on the classpath:

- **JVM / source runs** — add the extension to `deps.edn` like any Clojure dep (the first-party extensions use `:local/root` entries in Vis's own `deps.edn`).
- **Native binary** — extensions compile into the image. Add the dep, rebuild with `vis-agent update --rebuild` (see [Runtime distributions](distributions.md)), and mind the [native-image rules](#native-image-rules) below.

### Anatomy

```
my-extension/
├── deps.edn
├── src/com/acme/ext/weather/core.clj
└── resources/
    ├── META-INF/vis-extension/vis.edn                      ; discovery manifest
    ├── META-INF/native-image/com.acme/weather/             ; only if you pull in
    │   └── reachability-metadata.json                      ;   reflective libs
    └── vis-docs/                                           ; optional doc pages
        ├── vis-docs.edn
        └── weather.md
```

```clojure
;; deps.edn
{:paths ["src" "resources"]
 :deps  {com.blockether/vis {:local/root "../vis"}}}   ; or a released coordinate
```

### The extension spec

`vis/extension` validates the map and fills defaults; `vis/register-extension!` puts it in the registry.

| Key | What it is |
| --- | --- |
| `:ext/name` | Unique name string, e.g. `"weather"`. |
| `:ext/description` | One-liner shown in `vis-agent extension list` and to the model in its extensions snapshot. |
| `:ext/version` `:ext/author` `:ext/owner` `:ext/license` | Plain metadata strings. |
| `:ext/kind` | Categorical bucket used as a section label: `"foundation"`, `"language"`, `"channel"`, `"provider"`, … |
| `:ext/activation-fn` | `(fn [env] -> boolean)`, called **once per turn**. Falsy hides every symbol and the prompt fragment for that turn. Defaults to always-on. |
| `:ext/engine` | `{:ext.engine/alias 'weather :ext.engine/symbols [...]}` — the sandbox surface (below). |
| `:ext/prompt-fn` | `(fn [env] -> string)` — optional dynamic routing/capability text; never a copy of what `doc(name)` answers. |
| `:ext/ctx-fn` | `(fn [env] -> map)` — structured per-turn context contributed into the model's `session` dict. |
| `:ext/sandbox-shims` | Vec of Python **shim** specs — host-backed modules published into the model's Python sandbox (below). |
| `:ext/slash-commands` | Vec of slash-command specs (below). |
| `:ext/doctor-fn` | `(fn [env] -> [checks])` — health checks for `vis-agent doctor`. |
| `:ext/settings` `:ext/env` | Declared settings / environment variables, resolved exactly as a Python extension's `env=` is ([Environment](#environment)). |

Channels, providers, persistence backends, and workspace backends register through their own keys (`:ext/channels`, `:ext/providers`, `:ext/persistance`, `:ext/workspace-backends`) — read a first-party extension of the matching kind as the reference implementation.

The remaining accepted keys are declarative registrations: the host applies them when the extension registers and undoes them when it unregisters, so nothing needs a global atom or an imperative `register-*!` call. `:ext/cli` adds CLI commands (auto-placed under the `vis-agent extension` parent unless the entry names its own `:cmd/parent`); `:ext/language-tools` contributes a language's format/lint/test/REPL handlers; `:ext/hooks` and `:ext/op-hooks` run at named lifecycle phases (an op-hook on a GATE op such as `:fs/access` guards paths instead, and is asked rather than wrapped); `:ext/network-filters` adds egress predicates; `:ext/attachment-storage` supplies an attachment backend; `:ext/channel-contributions` fills channel UI slots; `:ext/theme` ships theme overrides; `:ext/requires` names extensions that must register first (load order is topologically sorted); `:ext/source-nses` marks the namespaces the extension is built from. The authoritative, complete list is the `::extension` spec in `com.blockether.vis.internal.extension`.

### Tools: symbols

A tool is a Clojure `defn` wrapped with `vis/symbol` and listed under `:ext.engine/symbols`:

```clojure
(defn- lookup-fn
  "await weather_lookup(city)
Returns {\"city\", \"summary\"} — current conditions for a city."
  [city]
  (extension/success {:result {:city city :summary "sunny, 21°C"}}))

(def lookup-symbol
  (vis/symbol #'lookup-fn {:symbol 'lookup :tag :observation}))
```

The rules:

- **Pass the var** (`#'lookup-fn`), never a bare fn: its docstring and arglists become `doc("weather_lookup")`.
- **Naming.** The Python name is `<alias>_<symbol>` in snake_case: alias `'weather` + symbol `'lookup` → `weather_lookup`. Kebab-case folds to snake_case, and a trailing `?`/`!` is stripped (`refresh!` → `refresh`).
- **`:tag` is required**: `:observation` for pure reads, `:mutation` for anything that writes.
- **Arguments** arrive as plain values; a Python dict of options becomes a Clojure map with keyword keys (`weather_lookup("Oslo", {"units": "metric"})` → `[city {:units "metric"}]`). Use multiple arities for optional args.
- **Return an envelope.** `extension/success {:result value}` on success; on failure either throw (`ex-info` is converted for you) or return `extension/failure {:result nil :error {:message "…" :hint "…"}}`. The model sees only the `:result` payload — map keys convert kebab→snake automatically — and failures surface as normal Python exceptions.
- Envelope constructors live in `com.blockether.vis.internal.extension` (`success` / `failure`); the spec/registration API is `com.blockether.vis.core` (aliased `vis`).

Useful `vis/symbol` opts beyond `:symbol` and `:tag`: `:before-fn` (e.g. inject the turn's `env` as the first argument), `:hidden?` (bind but don't advertise), and `:description` — one compact paragraph that REPLACES the docstring in `doc(name)` when the docstring is a developer note rather than the model's contract.

### One tool, and it is `python_execution`

`python_execution` is the ONLY tool a provider is ever handed a schema for. Every
symbol an extension registers is a bare Python name inside that sandbox, so there
is nothing to advertise, no JSON Schema to keep portable across providers, and no
per-symbol renderer.

What follows from that:

| Owner | Contains | Must not contain |
| --- | --- | --- |
| Function docstring (or `:description`) | Compact routing, preconditions, side effects, result semantics, and the exact arguments | Anything the signature already says twice |
| `:result` | The raw-result contract, appended by `doc(name)` as `Raw result: …` | Workflow prose already in the description |
| `:ext/prompt-fn` | Dynamic availability, routing, or catalogs only | Signatures, example calls, or anything `doc(name)` already answers |

A model finds a symbol with `apropos(text)` — full-text over every docstring,
documentation page, skill body and MCP tool description — and reads its contract
with `doc(name)`. The prompt therefore does not need to carry either one, which is
the whole reason a fragment must not restate them.

### Sandbox shims and autoloads

The agent writes **Python**, but its sandbox ships only the pure-stdlib — no
pip, no native wheels. A **shim** lets your extension publish a *host-backed*
Python module into every sandbox (the main session and every `sub_loop` fork):
the familiar Python API is a thin façade whose real work is DELEGATED across the
boundary to Clojure/JVM callables you supply. This is exactly how `import yaml`
(backed by the pure-Clojure YAMLStar loader) and `import matplotlib.pyplot`
(backed by a native imaging PNG renderer) work — both ship as built-in shim extensions
(`foundation.shim-yaml`, `foundation.shim-matplotlib`), and the engine installs
them through the SAME generic path any extension uses.

List one or more shim specs under `:ext/sandbox-shims`:

```clojure
{:shim/name        "yaml"
 ;; PUSHED into every request's system prompt: one line, the surface and what it
 ;; does NOT support. Detail a caller needs only while calling belongs in
 ;; `:shim/docs`, which nothing pushes and `doc("yaml")` answers.
 :shim/description "PyYAML-compatible module backed by YAMLStar. No custom tags."
 :shim/docs        "PyYAML-compatible `yaml` ... every option, in full."
 ;; Host callables the shim's Python delegates to — a `{py-name -> fn}` map (or a
 ;; 0-arg fn returning one). Each is wired onto the sandbox globals as a Python
 ;; callable (args marshalled Python->Clojure, result back) BEFORE the `.py` source
 ;; evals. Return a 2-vec envelope `[true payload]` / `[false message]` so a
 ;; failure crosses the boundary as a catchable Python exception.
 :shim/bindings    (fn [] {"__vis_yaml_load__" (fn [s] (try [true (yamlstar/load s)]
                                                        (catch Throwable t [false (str t)])))})
 ;; CLASSPATH RESOURCE path of the shim's Python source — a real `.py` file, never
 ;; a Clojure string. It is eval'd into the sandbox: publish your module into
 ;; `sys.modules` (so `import yaml` finds it) and optionally staple it onto
 ;; `builtins` (autoload — `yaml.safe_load(...)` with no import). Built-in shims
 ;; live in `resources/vis-shims/`; ship yours on your own classpath and, for a
 ;; native image, embed it with `-H:IncludeResources=<your-prefix>/.*`.
 :shim/source      "vis-shims/yaml.py"}
```

Installed BEFORE the sandbox's baseline snapshot, so your `__vis_*` bridge names
and published module are hidden from the model's live-vars view. Install is
best-effort: a shim that throws is logged and skipped — it never breaks the
sandbox. Shims are a Clojure-extension capability (they need host callables);
drop-in Python extensions contribute tools/prompts/slash/hooks instead.

`:shim/source` is the ONLY way to supply the Python: there is no inline-string
form. The file is read once through `extension/shim-src`, which THROWS when the
resource is missing — a shim whose `.py` never reached the classpath fails
loudly instead of publishing an empty module. Each shim's source is eval'd
LAZILY, on the first `import <name>` (or first touch of an autoloaded global),
so a session that never imports it pays nothing. Because the source is a real
file, it is lintable, diffable, testable with the built-in `pytest` shim, and
free of Clojure escaping hazards.

### The prompt fragment

`:ext/prompt-fn` rides in a labeled `;; -- EXTENSION <alias> --` block only while the extension is active. Use it only for facts unavailable from a symbol's own documentation—for example, a dynamic capability matrix or a catalog that changes per turn.

```
Weather service configured for this workspace; live lookups are available.
```

A fixed extension usually needs no prompt fragment. Do not repeat signatures, fields, defaults, or return contracts here — that is exactly the text `doc(name)` already carries, and a copy here costs MORE than the pull it duplicates.

### Activation

`:ext/activation-fn` gates the whole extension per turn. Use it to hide tools that can't work in the current workspace — a Node-only extension activates only when the workspace root holds a `package.json`:

```clojure
(defn- activation-fn [env]
  (boolean (some-> (:workspace/root env) (io/file "package.json") .isFile)))
```

An inactive extension costs zero prompt tokens.

### Slash commands (Clojure)

User-facing `/commands` (TUI and companion) are data too:

```clojure
:ext/slash-commands
[{:slash/name   "weather"
  :slash/doc    "Show current weather."
  :slash/usage  "/weather <city>"
  :slash/run-fn (fn [ctx]
                  {:slash/status :ok
                   :slash/title  "Sunny in Oslo"
                   :slash/data   {:city "Oslo"}})}]
```

Return `{:slash/status :ok | :error, :slash/title "…"}` plus optional `:slash/data`.

### Asking the human (Clojure)

`com.blockether.vis.core/request-human-input!` takes the same request map the
Python side sends, and `com.blockether.vis.human-input` builds it with the same
names - the only namespace besides `core` an extension imports:

```clojure
(require '[com.blockether.vis.core :as vis]
         '[com.blockether.vis.human-input :as hi])

(vis/request-human-input!
  (hi/form {:title "Deploy" :submit-label "Ship it"}
           (hi/heading "Target")
           (hi/paragraph "Staging pages nobody.")
           (hi/row (hi/select "env" ["staging" "prod"] {:label "Environment"
                                                        :is-required true})
                   (hi/slider "canary" {:label "Canary %" :min 0 :max 100 :step 5}))
           (hi/checkbox "ack" {:label "I read the runbook" :is-required true})
           (hi/password "token" {:label "Deploy token" :is-required true})))
```

Every builder returns the plain map you could have typed by hand, and validates
it on the way out: `(hi/select "env" [])` throws at that line. `hi/form` does the
same for the assembled request, and `hi/check` answers instead of throwing -
`nil` when the request is fine, one line of prose when it is not.

### Validating a field (Clojure)

`:validate` is a **function**, or a vector of them, the same contract Python
has. One argument is the coerced value; two are the value and the whole
`field name -> value` map of the answer (string keys, flat, whatever the
layout). Answer `nil`/`true` to accept, a string to refuse with that message.
The vector runs in order and the first message wins.

```clojure
(defn- a-slug [text]
  (when-not (re-matches #"[a-z][a-z0-9-]*" text)
    "lowercase, digits and dashes"))

(defn- is-free [text]
  (when (contains? @taken text) "already taken"))

(vis/request-human-input!
  (hi/form {:title "Sign up"}
           (hi/plaintext "slug" {:label "Project" :validate [a-slug is-free]})
           (hi/password "pass" {:label "Password"
                                :validate #(when (< (count %) 12) "at least 12 characters")})
           (hi/password "again" {:label "Repeat it"
                                 :validate (fn [text values]
                                             (when-not (= text (get values "pass"))
                                               "the two do not match"))})))
```

The function is judged where you wrote it, long before a human sees the form:

```clojure
(hi/plaintext "slug" {:validate "[a-z]+"})
;; throws Invalid human-input field slug: :validate takes a FUNCTION, not "[a-z]+" ...

(hi/checkbox "ack" {:validate (fn [] nil)})
;; throws Invalid human-input field ack: :validate function takes the value, or
;;        the value and every value ... this one takes neither

(hi/check {:title "Sign up"
           :fields [{:type "plaintext" :name "slug" :validate "[a-z]+"}]})
;; => "Invalid human-input field slug: :validate takes a FUNCTION ...", never a throw
```

The functions stay in your process. The engine runs them when the form is
CONFIRMED, hands the surfaces one message per broken field, and `:validate` is
stripped from the field before any surface sees it.

### Shipping doc pages

Any extension can add pages to Vis's embedded docs — the same corpus the `/docs` site renders and the model searches with `apropos(text)` and reads with `doc(slug)`. Drop markdown under `resources/vis-docs/` with a manifest:

```clojure
;; resources/vis-docs/vis-docs.edn
{:pages [{:file "weather.md" :title "Weather" :section "Extensions" :order 50}]}
```

Every `vis-docs/vis-docs.edn` on the classpath is discovered — no central registry to edit. Ask a running Vis about your extension and it reads the page you shipped.

### Complete minimal example

`src/com/acme/ext/weather/core.clj`:

```clojure
(ns com.acme.ext.weather.core
  "Weather lookups under the `weather_` alias."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]))

(defn- lookup-fn
  "Implementation for a current-conditions lookup."
  [city]
  (extension/success {:result {:city (str city) :summary "sunny, 21°C"}}))

(def ^:private symbols
  [(vis/symbol
     #'lookup-fn
     {:symbol 'lookup
      :name "weather_lookup"
      :tag :observation
      :description "Read live weather when current conditions are required. ONE city."
      :result "Object with string `city` and string `summary`."
      :call {:pos ["city"]}})])

(def vis-extension
  (vis/extension
   {:ext/name        "weather"
    :ext/description "Current-conditions weather lookups for the model."
    :ext/version     "0.1.0"
    :ext/kind        "integration"
    :ext/engine      {:ext.engine/alias 'weather
                      :ext.engine/symbols symbols}}))

(vis/register-extension! vis-extension)
```

`resources/META-INF/vis-extension/vis.edn`:

```clojure
{weather {:nses [com.acme.ext.weather.core]}}
```

Add the dep, restart Vis, and the model can call `weather_lookup("Oslo")`.

### Native image rules

The native binary compiles extensions ahead of time, which brings a few hard constraints (see [JVM & native-image](jvm-native-image.md) for the background):

- **No `defrecord` / `deftype` / `gen-class`** in sandbox-facing code — the build refuses them (`validate-no-banned-defs!`). Plain maps and functions only.
- **Reachability metadata travels inside your jar**: `resources/META-INF/native-image/<group>/<artifact>/reachability-metadata.json` — the unified format only, never the legacy `reflect-config.json` family. Only add entries for reflection/resources **your extension uniquely pulls in**; never duplicate a library's own config.
- **Generate it with the tracing agent**, not by hand: run your code paths under `java -agentlib:native-image-agent=config-merge-dir=<your-artifact-dir> …`, then strip Clojure-internal noise.
- Resources your extension reads at runtime via `io/resource` (templates, assets) need a resource glob in that metadata — the agent only captures what the trace actually touched.

### Testing and verification

Vis uses [lazytest](https://github.com/NoahTheDuke/lazytest); test tool functions directly against the envelope contract:

```clojure
(ns com.acme.ext.weather.core-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe lookup-test
  (it "returns a canonical success envelope"
    (let [result (@#'com.acme.ext.weather.core/lookup-fn "Oslo")]
      (expect (extension/envelope-success? result))
      (expect (= "Oslo" (:city (:result result)))))))
```

Before shipping, run `clojure -M:format check`, `clojure -M:lint src extensions test build.clj`, and the relevant tests.
