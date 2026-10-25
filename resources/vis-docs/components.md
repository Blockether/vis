# Components

Vis builds every form and live view from one list of components. This page describes each
component, its Python builder and where it works.

## When to use

- **You build a form and need the right input for a value.** Find it in [Inputs](#inputs).
- **You show work in a live view and want progress, counters or a log.** Read
  [State](#state).
- **You want rows, links or cards that a person can select.** Read [Data](#data).
- **You want controls side by side, a folding section or a small form inside a view.** Read
  [Layout](#layout) and [Button](#button).

To ask a question and wait for the answer, read [Forms and user input](human-input.md). To keep a
view open while work runs, read [Live views](live-views.md).

## How components work

A component is a JSON object with an `id` and a `type`. Each builder in
`blockether.vis.extension` returns one. A form and a live view take the same components:

```python
import blockether.vis.extension as vis

answer = vis.ask("Deploy", [vis.ui.select("env", ["staging", "prod"], label="Target")])

with vis.live("Deploy", [vis.ui.status("state", "Waiting", tone="idle"),
                         vis.ui.select("env", ["staging", "prod"], label="Target")]) as view:
    view["state"].set("Deploying", tone="running")
```

- A form returns the input values when the person submits it.
- A live view keeps each input `value` while it is open. Read the values with `view.values()`.
  Read each change with `view.events()`.
- The engine validates each component against the `component` schema in
  `packages/vis-contract/resources/vis-contract/schema/view.json`. An unknown key is refused.
- Every surface draws the same component in its own style. The terminal and the Companion app
  show the same text, values and state.

Text and status fields accept inline Markdown: `` `code` ``, `**bold**`, `_italic_` and links. A
`tone` is one of `idle`, `running`, `ok`, `warn` and `error`.

## Text

Text components show content and take no input.

### Heading

`vis.ui.heading(id, text, level=2)` shows a title. `level` is 1 to 6. In a form,
`vis.ui.heading(text)` without an id is also accepted.

### Paragraph

`vis.ui.paragraph(id, text)` shows a paragraph of inline Markdown. In a form, `vis.ui.paragraph(text)`
without an id is also accepted.

### Code

`vis.ui.code(id, text, language="python")` shows literal code. Vis keeps the whitespace and never runs
the code.

### Divider

`vis.ui.divider(id)` draws a rule across its container, like an HTML `<hr>`. It has no text or options.

## State

State components show how work moves. A live view changes them while the work runs.

### Status

`vis.ui.status(id, text, tone="running", detail=None)` shows one line about the current state. Change
it with `view[id].set(text, tone=...)`.

### Spinner

`vis.ui.spinner(id, text, variant="braille", is_active=True)` shows activity without a known end. The
variants are `braille`, `dots`, `line` and `pulse`. A completed view never animates.

### Progress

`vis.ui.progress(id, value=0.4)` shows a bar from 0 to 1. Give `done` and `total` instead to show a
count. A bar without a value shows work of unknown length.

### Stat

`vis.ui.stat(id, stats=[...])` shows a strip of counters. Each item has `id`, `label`, `value_text`
and an optional `tone`.

```python
vis.ui.stat("summary", stats=[{"id": "open", "label": "Open", "value_text": "3"},
                           {"id": "failed", "label": "Failed", "value_text": "1",
                            "tone": "error"}])
```

### Steps

`vis.ui.steps(id, steps=[...])` shows a checklist. Each step has `id`, `label`, `tone` and an optional
`detail`. Add or change one step with `view[id].set(step_id, label, tone="ok")`.

### Log

`vis.ui.output(id, label="Build log")` shows output lines. Its type on the wire is `log`. Add lines
with `view[id].write(...)` and an optional line `tone`. [Live views](live-views.md#add-severity-to-streaming-output)
explains windows, search and severity.

## Data

Data components show records that a person can read, open or select.

### Table

`vis.ui.table(id, columns=[...], rows=[...])` shows rows with an id each. Use `vis.ui.table_column`,
`vis.ui.table_row` and `vis.ui.table_group` to build them. With `is_selectable=True`, a person can select
rows, and `selected_ids` holds the selection.

### Link

`vis.ui.link(id, links=[...])` shows targets that a person can open. Each link has `id`, `label`,
`target` and a `target_kind` of `url`, `path` or `attachment`.

### Card

`vis.ui.card(id, title, text=None, tone=None, meta=None)` shows a titled box with short text and meta
lines. With `is_selectable=True`, a person can activate the card like a button. Read
`view[id].clicks` or handle the `activate` event.

```python
vis.ui.card("task-7", "T7 Fix the login timeout", text="In review", tone="warn",
         is_selectable=True)
```

## Inputs

Inputs take a value from a person. Each input has a `name`, a `label` and optional
`description`, `placeholder`, `default`, `is_required` and `validate` keys. In a form, the answer
holds each value under its `name`. In a live view, the input keeps its `value`.

| Builder | Type | Value |
| --- | --- | --- |
| `vis.ui.plaintext(name)` | `plaintext` | One line of text |
| `vis.ui.multiline(name)` | `multiline` | Text with line breaks |
| `vis.ui.password(name)` | `password` | A `vis-secret:` handle |
| `vis.ui.otp(name)` | `otp` | A `vis-secret:` handle for a code of digits |
| `vis.ui.select(name, options)` | `select` | One option value |
| `vis.ui.multiselect(name, options)` | `multiselect` | A list of option values |
| `vis.ui.checkbox(name)` | `checkbox` | `True` or `False` |
| `vis.ui.slider(name, min=0, max=100, step=1)` | `range` | A number |

[Forms and user input](human-input.md#fields) gives the rules for each input.

### Plaintext

`vis.ui.plaintext(name, label="Title")` takes one line. `min_length` and `max_length` limit it.

### Multiline

`vis.ui.multiline(name, label="Details")` takes text with line breaks.

### Password

`vis.ui.password(name, label="Token")` masks the text. The value is an opaque `vis-secret:` handle.
Read the text on the trusted side with `vis.reveal(handle)`. A live view never shows or stores the
text. A `change` event gives only `is_set`.

### Otp

`vis.ui.otp(name, label="Code")` shows one box for each digit. `min_length` and `max_length` set the
length, 6 by default and 12 at most. The value is a `vis-secret:` handle.

### Select

`vis.ui.select(name, options, label="Target")` takes exactly one option. An option is a string, or
`vis.ui.option(value, label)` when the value and the shown text differ.

### Multiselect

`vis.ui.multiselect(name, options)` takes any number of options. The value is a list in the declared
order.

### Checkbox

`vis.ui.checkbox(name, label="I read the runbook")` takes `True` or `False`. A required checkbox must
be ticked.

### Range

`vis.ui.slider(name, min=0, max=100, step=5)` takes a number on a track. Its type is `range`. The
builder name keeps the Python built-in `range` free.

## Actions

### Button

`vis.ui.button(id, label, action="activate")` is an action that a person presses. No code goes to the
client. The extension decides what a press does.

- `action="activate"`, the default, counts the press in `clicks` and sends an `activate` event.
- `action="submit"` collects the inputs of the nearest group of the button. Vis checks each
  required input and each validator first. Then it sends one `submit` event with all `values`.
- `is_disabled=True` refuses presses. Change it with `view[id].set(is_disabled=False)`.

A form has its own submit and cancel buttons. Use buttons in live views.

## Layout

### Group

`vis.ui.group(id, *children, direction="column")` arranges its children. A group has no value.

- `direction="row"` puts the children side by side. They stack when the space is too narrow.
- `direction="grid"` puts the children in equal columns that continue on new rows. Use it for many
  cards. `max_columns` (1 to 12) sets the most columns. Only a grid accepts `max_columns`.
- `direction="wrap"` puts the children side by side at their own width. A child that does not fit
  starts a new line. Use it for buttons, stats and statuses.
- `label` and `description` show a title and a short text above the children.
- `is_collapsible=True` lets a person fold the group. `default_expanded` sets the first state.
- A submit button collects only the inputs of its own group. So one view can hold several small
  forms.

`vis.ui.row(id, ...)`, `vis.ui.column(id, ...)`, `vis.ui.grid(id, ..., max_columns=3)` and
`vis.ui.wrap(id, ...)` are short forms of a group. `vis.ui.disclosure(id, label,
...)` is a collapsible column that starts folded. In a form, a group without an id is accepted.

```python
vis.ui.group("new-task",
          vis.ui.plaintext("title", label="Title", is_required=True),
          vis.ui.multiline("details", label="Details"),
          vis.ui.button("add", "Add task", action="submit"),
          label="New task")
```

## See also

- [Live views](live-views.md) — open a view, change its components and handle events.
- [Forms and user input](human-input.md) — ask with the same components and wait for the answer.
- [Extension API](extension-api.md#asking-the-human-and-showing-live-work) — where each builder
  belongs in an extension.
