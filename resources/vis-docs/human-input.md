# Forms and user input

A form asks the user for a choice, a password or token, or a confirmation. `vis.ask` shows the form
in the terminal or the Companion app, waits, and returns the answer to your Python code.

## When to use

- **A tool is about to do something that needs the approval of a person**, such as a deployment.
  [Ask for confirmation](#ask-and-handle-cancellation) and stop if the person cancels.
- **The tool needs a token or a one-time code that must not appear in the conversation.** A
  `password` or `otp` field returns [a handle, not the value](#the-answer).
- **Only a person can make the choice**, such as the environment for a deployment. Offer the
  options as [fields](#fields).
- **The input must be valid before the tool continues.** Add [validation](#validation) that
  refuses a bad value with a message.

To show progress and ask no question, use a [live view](live-views.md). A live view can also
[collect input](live-views.md#collect-input-in-a-view) while work runs.

## Before you start

Call `vis.ask` from a registered tool, a user command, or a session-bound hook. The call needs a
calling session and an available TUI or Companion client. It cannot run during registration or
from a passive provider callback. Outside Vis, the SDK uses terminal input, as
[testing outside Vis](extension-design.md#test-the-python-implementation) shows.

## Ask and handle cancellation

Call your operation only after a submitted answer, and handle cancellation in your code. This
fragment belongs in your tool. The module imports `blockether.vis.extension as vis` and supplies its
own `deploy` function.

```python
answer = vis.ask("Deploy", [
    {"name": "env", "label": "Target", "description": "Deployment environment.",
     "type": "select", "options": ["staging", "prod"], "is_required": True},
    {"name": "notes", "type": "multiline", "label": "Release notes"},
    {"name": "token", "type": "password", "label": "Deploy token"},
], description="Pick a target", timeout_ms=120000)

if answer:                       # false when cancelled, timed out or undeliverable
    deploy(answer["env"], answer.reveal("token"))
else:
    vis.log("info", "deploy skipped: " + answer.reason)
```

The request opens this form in the Vis terminal, with the password masked.

[![Vis terminal form with Target, Release notes and a masked Deploy token field](assets/screenshots/ask.png)](assets/screenshots/ask.png)

| Option | Meaning |
| --- | --- |
| `description` | Markdown under the title. |
| `submit_label`, `cancel_label` | Button labels. |
| `is_cancellable` | `False` removes the cancel button. |
| `timeout_ms` | 5 minutes by default. `0` waits until the person answers or cancels. |

Every key is a snake_case string, such as `timeout_ms`. Vis refuses a camelCase or kebab-case key
with an error that names the right spelling. It refuses an unknown key by name.

A form from a tool call shows the name of that tool, so that the person knows which tool asks.

## The answer

`vis.ask` never raises for a refusal. It returns an `Answer` that is truthy when the person
confirmed:

| Attribute | Meaning |
| --- | --- |
| `answer.values` | The value of every field, defaults included. `answer["env"]` reads one value. |
| `answer.reason` | For a falsy answer: `cancelled`, `timeout`, `undeliverable`, or the reason of the host. |
| `answer.reveal(name)` | Resolves a secret handle in process. |

A `password` or `otp` field returns an opaque `vis-secret:` handle. The transcript, the logs, and
the model get only the handle. To get the value, call `answer.reveal(name)` or `vis.reveal(handle)`.
`vis.forget(handle)` removes the value. Do not print or log the revealed value.

If no client can show the form, the request logs an error and returns `undeliverable` at once.
The session of an extension automation run cannot ask the person, so its request also returns
`undeliverable` at once.

A form from a subagent shows in the session of its leader, because the person watches that
session. The form names the subagent that asks. The answer goes to the call of that subagent.

## Fields

`name` is the key of the field in `answer.values`, and `id` is an alias for `name`. `label` shows
above the input. The optional `description` shows under the label.

| `type` | Answers with | Notes |
| --- | --- | --- |
| `plaintext` | string | The default type. |
| `password` | secret handle | Shown as dots. |
| `multiline` | string | |
| `select` | one option value | Exclusive: exactly one option, never none. |
| `multiselect` | list of option values, in declared order | Inclusive: any number. Empty is legal unless required. |
| `checkbox` | `true` or `false` | A required checkbox must be ticked. |
| `range` | number | `min` (0), `max` (100), `step` (1). Out-of-range values are refused. |
| `datetime` | string | `mode` is `date` (`YYYY-MM-DD`), `time` (`HH:MM`) or `datetime` (`YYYY-MM-DDTHH:MM`, the default). `min` and `max` take the same form. |
| `otp` | secret handle | Digits only, one box per digit. `min_length` and `max_length` set the length (default 6, at most 12). |

The common keys are `placeholder`, `default`, `is_required`, `min_length`, `max_length`, and
`validate`. The two select types also take `options`. An option is a string, or
`{"value": ..., "label": ...}` when the stored value and the shown text differ. Vis refuses an
answer that names an undeclared option.

On confirmation, the engine rejects a blank `is_required` field in an answer from a form or from an
HTTP request.

## Layout

Forms and [live views](live-views.md#layout-and-text) share the layouts `row`, `column`, `grid`, and
`wrap`. A `group` arranges its `children` in one of them, `column` by default. Groups can nest.

| Direction | Arrangement | On a narrow screen |
| --- | --- | --- |
| `row` | Children side by side in equal columns. | The children stack. Companion stacks them one by one, and the TUI stacks the whole row. |
| `column` | Children one under the other. | No change. |
| `grid` | Children in equal columns that continue on new rows, in their order. `max_columns` (1 to 12) limits the columns. | Fewer columns, then a stack on the narrowest screens. |
| `wrap` | Children side by side at their own width. | A child that does not fit starts a new line. |

A group in a form can have a `label` and a plain-text `description`. A `heading` shows plain text. A
`paragraph` shows Markdown, including emphasis, inline code, and lists, as does the `description` of
the request. A form does not get the collapse and interaction behavior of a live view.

```python
vis.ask("Where should the pool connect?", [
    {"type": "heading", "text": "Server"},
    {"type": "group", "direction": "row", "children": [
        {"name": "host", "label": "Host", "is_required": True},
        {"name": "port", "label": "Port"},
    ]},
    {"type": "paragraph", "text": "TLS is required outside the office network."},
    {"name": "tls", "label": "Require TLS", "type": "checkbox"},
])
```

`answer.values` is a flat map keyed by field name, with any layout. These rules apply:

- Names must be unique.
- A group rejects the value keys `default`, `options`, and `validate`.
- A field rejects the layout keys `children` and `direction`.
- A heading or a paragraph has no input, cannot receive focus, and does not appear in the answer.

## Builders

Each node type has a builder that validates its arguments:

```python
form = vis.ui.column(
    vis.ui.heading("Target"),
    vis.ui.paragraph("Staging does not send on-call alerts."),
    vis.ui.row(
        vis.ui.select("env", [vis.ui.option("staging", "Staging"), vis.ui.option("prod")],
                   is_required=True),
        vis.ui.slider("canary", label="Canary %", min=0, max=100, step=5, default=10),
    ),
    vis.ui.checkbox("ack", label="I read the runbook", is_required=True),
    vis.ui.password("token", label="Deploy token", is_required=True),
)

answer = vis.ask("Deploy", [form], submit_label="Deploy")
```

The builders are `plaintext`, `password`, `multiline`, `select`, `multiselect`, `checkbox`,
`slider`, `datetime`, `otp`, `option`, `row`, `column`, `group`, `heading`, and `paragraph`.
`slider` builds the `range` field. The keyword arguments are the keys of [Fields](#fields).

A form takes the same [components](components.md) as a live view, such as a `status`, a `table`, or
a `card` above the inputs. A form needs no `button`.

## Validation

`validate` is a function, or a list of functions that run in order until one refuses. A validator
receives the coerced value, or the value and the flat map of every answer. To accept, return `None`
or `True`. To refuse, return a message string.

In this fragment, your module imports `re` and defines the validator `is_free`. `is_free` returns
`None` for an available slug and an error message for any other slug.

```python
def a_slug(text):
    if not re.fullmatch(r"[a-z][a-z0-9-]*", text):
        return "lowercase, digits and dashes"


answer = vis.ask("Sign up", [
    vis.ui.plaintext("slug", label="Project", validate=[a_slug, is_free]),
    vis.ui.password("pass", label="Password",
                 validate=lambda text: "at least 12 characters" if len(text) < 12 else None),
    vis.ui.password("again", label="Repeat it",
                 validate=lambda text, values:
                     None if text == values["pass"] else "the two do not match"),
])
```

- Vis refuses a validator with any other signature when it builds the request. It also refuses a
  value that is not a function, such as a regex string.
- `False` refuses with `is not valid`. A validator that raises refuses with
  `could not be validated: <exception>`.
- Validators do not run on blank answers. To reject a blank answer, use `is_required`.
- Validation runs in the engine on confirmation. The form shows the error message of each field.

## See also

- To write the extension that calls `vis.ask`, read [Extending Vis](extending.md).
- To show progress while an extension runs, read [Live views](live-views.md).
- To look up every component and its builder, read [Components](components.md).
