# Asking the human

`vis.ask` pauses an extension, shows a typed form in the TUI, the web UI or the
Companion app, and returns the answer. This page is the reference for the
request, every field type, layout, validation and the answer object. It
applies to Python extensions; the Clojure builders are listed at the end.

## A request

```python
answer = vis.ask("Deploy", [
    {"name": "env", "label": "Target", "description": "Where this deploy lands.",
     "type": "select", "options": ["staging", "prod"], "is_required": True},
    {"name": "notes", "type": "multiline", "label": "Release notes"},
    {"name": "token", "type": "password", "label": "Deploy token"},
], description="Pick a target", timeout_ms=120000)

if answer:                       # false when cancelled, timed out or undeliverable
    deploy(answer["env"], answer.reveal("token"))
else:
    vis.log("info", "deploy skipped: " + answer.reason)
```

Request options:

| Option | Meaning |
| --- | --- |
| `description` | Prose under the title. |
| `submit_label`, `cancel_label` | Button labels. |
| `is_cancellable` | `False` removes the cancel button. |
| `timeout_ms` | 5 minutes by default; `0` waits until the person answers or cancels. |

A question needs a session: `vis.ask` uses the session of the running tool,
slash command or hook, and refuses immediately when there is none. Provider
callbacks (`detect_fn`, `status_fn`) run without a session and must not ask.

Every key is a snake_case string (`is_required`, `max_length`, `timeout_ms`).
A camelCase or kebab-case key is refused with an error naming the right
spelling. Unknown keys are refused by name.

## The answer

`vis.ask` never raises for a refusal. It returns an `Answer` that is truthy
when the person confirmed:

| Attribute | Meaning |
| --- | --- |
| `answer.values` | every field's value, defaults included; `answer["env"]` reads one |
| `answer.reason` | `cancelled`, `timeout`, `undeliverable` or the host's reason when the answer is falsy |
| `answer.reveal(name)` | resolve a secret handle in process |

A `password` or `otp` field answers with an opaque `vis-secret:` handle. The
transcript, logs and the model see only the handle; `answer.reveal(name)` or
`vis.reveal(handle)` resolves it at the moment of use, and
`vis.forget(handle)` drops it.

When no surface can show the dialog, the answer is `undeliverable` at once and
an error is logged; the extension is never parked until the timeout.

## Fields

A field has a `name` (the key in `answer.values`), a `label` shown above the
input and an optional `description` shown under the label. `id` is accepted as
an alias for `name`.

| `type` | Answers with | Notes |
| --- | --- | --- |
| `plaintext` | string | the default type |
| `password` | secret handle | shown as dots |
| `multiline` | string | |
| `select` | one option value | exclusive: exactly one option, never none |
| `multiselect` | list of option values, in declared order | inclusive: any number; empty is legal unless required |
| `checkbox` | `true` or `false` | a required checkbox must be ticked |
| `range` | number | `min` (0), `max` (100), `step` (1); an out-of-range value is refused |
| `otp` | secret handle | digits only, one box per digit; `min_length`/`max_length` set the length (default 6, at most 12) |

Common keys: `placeholder`, `default`, `is_required`, `min_length`,
`max_length`, `validate`, and `options` for the two select types. An option is
a string, or `{"value": ..., "label": ...}` when the stored value and the shown
text differ. An answer naming an undeclared option is refused.

`is_required` is enforced by the engine, not only drawn: a blank required field
is refused on confirmation, whether the answer came from a dialog or straight
over HTTP.

## Layout

Two node types answer nothing. A `group` arranges its `fields` in a `column`
(default) or a `row`, and may nest. A `heading` or `paragraph` carries `text`
only.

```python
vis.ask("Where should the pool connect?", [
    {"type": "heading", "text": "Server"},
    {"type": "group", "direction": "row", "fields": [
        {"name": "host", "label": "Host", "is_required": True},
        {"name": "port", "label": "Port"},
    ]},
    {"type": "paragraph", "text": "TLS is required outside the office network."},
    {"name": "tls", "label": "Require TLS", "type": "checkbox"},
])
```

Layout never changes the answer: `answer.values` stays flat, keyed by the leaf
fields, and names must be unique across the whole tree. A value key on a group
(`default`, `options`, `validate`) or a layout key on a field (`fields`,
`direction`) is refused. Decorations are never focused and never appear in the
answer.

## Builders

Each node type has a builder that validates the node where it is written:

```python
form = vis.column(
    vis.heading("Target"),
    vis.paragraph("Staging pages nobody."),
    vis.row(
        vis.select("env", [vis.option("staging", "Staging"), vis.option("prod")],
                   is_required=True),
        vis.slider("canary", label="Canary %", min=0, max=100, step=5, default=10),
    ),
    vis.checkbox("ack", label="I read the runbook", is_required=True),
    vis.password("token", label="Deploy token", is_required=True),
)

answer = vis.ask("Deploy", [form], submit_label="Ship it")
```

Builders: `plaintext`, `password`, `multiline`, `select`, `multiselect`,
`checkbox`, `slider` (the `range` field, named to avoid the builtin), `otp`,
`option`, `row`, `column`, `heading` and `paragraph`. Keyword arguments are the
field keys above.

## Validation

`validate` is a function, or a list of functions run in order until one
refuses. A validator receives the coerced value, or the value and the flat map
of every answer, and returns `None` or `True` to accept or a message string to
refuse:

```python
def a_slug(text):
    if not re.fullmatch(r"[a-z][a-z0-9-]*", text):
        return "lowercase, digits and dashes"


answer = vis.ask("Sign up", [
    vis.plaintext("slug", label="Project", validate=[a_slug, is_free]),
    vis.password("pass", label="Password",
                 validate=lambda text: "at least 12 characters" if len(text) < 12 else None),
    vis.password("again", label="Repeat it",
                 validate=lambda text, values:
                     None if text == values["pass"] else "the two do not match"),
])
```

- A validator with any other signature, or a non-function such as a regex
  string, is refused when the request is built.
- `False` refuses with `is not valid`; a validator that raises refuses with
  `could not be validated: <exception>`.
- Validators never run on a blank answer; emptiness is `is_required`'s job.
- Validation runs once, on confirmation, in the engine. The dialog then marks
  each refused field with its message. Validators never cross the wire.

## Clojure builders

`com.blockether.vis.core/request-human-input!` takes the same request, built
with `com.blockether.vis.view` using kebab-case keys:

```clojure
(require '[com.blockether.vis.core :as vis]
         '[com.blockether.vis.view :as hi])

(vis/request-human-input!
  (hi/form {:title "Deploy" :submit-label "Ship it"}
           (hi/heading "Target")
           (hi/row (hi/select "env" ["staging" "prod"] {:label "Environment"
                                                        :is-required true})
                   (hi/slider "canary" {:label "Canary %" :min 0 :max 100 :step 5}))
           (hi/password "token" {:label "Deploy token" :is-required true
                                 :validate #(when (< (count %) 12) "at least 12 characters")})))
```

Each builder returns the plain map and validates it on the way out;
`(hi/select "env" [])` throws at that line. `:validate` takes a function or a
vector of functions with the same contract as Python.

## See also

- [Extending Vis](extending.md) — the extension that calls `vis.ask`.
- [Live views](live-views.md) — the other direction: showing work instead of asking.
- [Clojure extensions](clojure-extensions.md) — the Clojure side of the same API.
