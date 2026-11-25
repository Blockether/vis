# Components

Vis builds every form and every live view from one list of components. This page describes each
component, its Python builder, and where it works.

## When to use

- **You build a form and need the right input for a value.** Find it in [Inputs](#inputs).
- **You show work in a live view and want progress, counters, or a log.** Read [State](#state).
- **You want rows, links, or cards that a person can select.** Read [Data](#data).
- **You want short labels, a list of points, or numbers as a chart.** Read [Badge](#badge),
  [List](#list), and [Chart](#chart).
- **You need a date or a time from a person.** Read [Datetime](#datetime).
- **You want controls side by side, a folding section, or a small form inside a view.** Read
  [Layout](#layout) and [Button](#button).

To ask a question and wait for the answer, read [Forms and user input](human-input.md). To keep a
view open while work runs, read [Live views](live-views.md).

## How components work

A component is a JSON object with an `id` and a `type`. Each builder in `blockether.vis.extension`
returns one component. A form and a live view take the same components:

```python
import blockether.vis.extension as vis

answer = vis.ask("Deploy", [vis.ui.select("env", ["staging", "prod"], label="Target")])

with vis.live("Deploy", [vis.ui.status("state", "Waiting", tone="idle"),
                         vis.ui.select("env", ["staging", "prod"], label="Target")]) as view:
    view["state"].set("Deploying", tone="running")
```

- A form returns the input values when the person submits it.
- In a live view, read the values with `view.values()`, and read each change with `view.events()`.
- The engine validates each component against the `component` schema in
  `packages/vis-contract/resources/vis-contract/schema/view.json`. It refuses an unknown key.
- Each client draws a component in its own style, with the same text, values, and state.

Text and status fields accept [inline Markdown](live-views.md#layout-and-text). A `tone` is one of
`idle`, `running`, `ok`, `warn`, and `error`.

## Text

### Heading

`vis.ui.heading(id, text, level=2)` shows a title at `level` 1 to 6. A form also accepts
`vis.ui.heading(text)` without an id.

### Paragraph

`vis.ui.paragraph(id, text)` shows a paragraph of inline Markdown. A form also accepts
`vis.ui.paragraph(text)` without an id.

### Code

`vis.ui.code(id, text, language="python")` shows literal code. Vis keeps the whitespace and never
runs the code.

### Divider

`vis.ui.divider(id)` draws a rule across its container, like an HTML `<hr>`. It has no text and no
options. [Layout and text](live-views.md#layout-and-text) describes a divider inside a
`vis.ui.wrap` group.

## State

### Status

`vis.ui.status(id, text, tone="running", detail=None)` shows one line about the current state.
Change it with `view[id].set(text, tone=...)`.

### Spinner

`vis.ui.spinner(id, text, variant="braille", is_active=True)` shows activity without a known end.
[Add spinners and buttons](live-views.md#add-spinners-and-buttons) lists the variants. A completed
view never animates.

### Progress

`vis.ui.progress(id, value=0.4)` shows a bar from 0 to 1. To show a count, give `done` and `total`.
A bar without a value shows work of unknown length.

### Stat

`vis.ui.stat(id, stats=[...])` shows a strip of counters. Each item has `id`, `label`, `value_text`,
and an optional `tone`. The Companion app shows each counter as a tile. A counter with the tone
`running`, `ok`, `warn`, or `error` shows the mark of that tone before its value.

```python
vis.ui.stat("summary", stats=[{"id": "open", "label": "Open", "value_text": "3"},
                           {"id": "failed", "label": "Failed", "value_text": "1",
                            "tone": "error"}])
```

### Steps

`vis.ui.steps(id, steps=[...])` shows a checklist. Each step has `id`, `label`, `tone`, and an
optional `detail`. Add or change one step with `view[id].set(step_id, label, tone="ok")`.

### Timeline

`vis.ui.timeline(id, lanes=[...])` draws one bar for each lane on a shared time axis. Each lane has
`id`, `label`, `tone`, `start`, and optional `end` and `detail`. The times are epoch milliseconds. A
lane without `end` still runs and grows to the current time. Add or change one lane with
`view[id].set(lane_id, label, start, end=None, tone="running")`.

### Log

`vis.ui.output(id, label="Build log")` shows output lines. Its type on the wire is `log`. Add lines
with `view[id].write(...)` and an optional line `tone`. [Add severity to streaming
output](live-views.md#add-severity-to-streaming-output) explains windows, search, and severity.

## Data

### Table

`vis.ui.table(id, columns=[...], rows=[...])` shows rows, each with an id. Build them with
`vis.ui.table.column`, `vis.ui.table.row`, and `vis.ui.table.group`. With `is_selectable=True`, a
person can select rows, and `selected_ids` holds the selection. Each client marks a selected row
with `●` and an unselected row with `○`.

### Choose one value for each item

A table cell holds text only. To let a person choose one value for each item, give each item its
own `vis.ui.select`. Put the selects in one group with a submit button. The submit event then holds
every choice under the name of its select.

```python
ITEMS = [("item-1", "Item one"), ("item-2", "Item two"), ("item-3", "Item three"),
         ("item-4", "Item four"), ("item-5", "Item five"), ("item-6", "Item six")]

vis.ui.group(
    "sort",
    *[vis.ui.select(item_id, ["Work", "Personal"], label=label) for item_id, label in ITEMS],
    vis.ui.button("save", "Save", action="submit", variant="primary"),
    vis.ui.button("cancel", "Cancel"),
)
```

Keep the submit button directly in the group of the selects. A button inside a nested `vis.ui.wrap`
collects only the inputs of that wrap.

Use a selectable table only when the person picks items, not values for items.

### Link

`vis.ui.link(id, links=[...])` shows targets that a person can open. Each link has `id`, `label`,
`target`, and a `target_kind` of `url`, `path`, or `attachment`.

### Card

`vis.ui.card(id, title, text=None, tone=None, meta=None)` shows a titled box with short text and
meta lines. With `is_selectable=True`, a person can activate the card like a button. Read
`view[id].clicks` or handle the `activate` event.

To show another app of the same owner, give `reveals="key"` to a selectable card or a button. When
a person activates the node, the client shows the app with that key. If the app is not open yet,
the client waits for it. Open the app in your `activate` handler.

```python
vis.ui.card("task-7", "T7 Fix the login timeout", text="In review", tone="warn",
         is_selectable=True)
```

### Badge

`vis.ui.badge(id, badges=[...])` shows a row of short labels, such as `Stable`, `Beta`, or `3 open`.
Each badge has `id`, `label`, and an optional `tone`. A badge with a tone shows the mark of that
tone before its label. A node holds 32 badges at most. Add or change one badge with
`view[id].set(badge_id, label, tone="ok")`.

```python
vis.ui.badge("labels", badges=[{"id": "stable", "label": "Stable", "tone": "ok"},
                               {"id": "beta", "label": "Beta", "tone": "warn"}])
```

### List

`vis.ui.listing(id, items=[...])` shows a list of points. Each item has `id`, `text`, and an
optional `detail` and `tone`. `is_ordered=True` numbers the items. A node holds 200 items at most.
Its type on the wire is `list`.

Add or change one item with `view[id].set(item_id, text, detail=None, tone=None)`. Change the
numbers with `view[id].order(True)`.

```python
vis.ui.listing("todo", is_ordered=True,
               items=[{"id": "pin", "text": "Pin the release", "tone": "ok"},
                      {"id": "deploy", "text": "Deploy the playground"}])
```

### Chart

`vis.ui.chart(id, series=[...], kind="bar", categories=[...])` shows numbers as bars or as lines.
Each series has `id`, `label`, `values`, and an optional `tone`. `categories` names the values of
every series in order. `kind` is `bar`, the default, or `line`.

- A chart holds 8 series at most, and each series holds 64 values at most.
- A bar chart always shows zero. A negative value runs to the left of zero.
- The terminal draws the same numbers with block characters. Each bar ends with its number.
- Add or change one series with `view[id].set(series_id, label, values, tone=None)`. Change the
  kind or the categories with `view[id].configure(kind="line")`.

```python
vis.ui.chart("load", categories=["Mon", "Tue", "Wed"],
             series=[{"id": "api", "label": "API", "values": [12, 18, 9]},
                     {"id": "web", "label": "Web", "values": [4, 7, 11]}])
```

## Inputs

Each input has a `name`, a `label`, and optional `description`, `placeholder`, `default`,
`is_required`, and `validate` keys. In a form, the answer holds each value under its `name`. In a
live view, the input keeps its `value`.

| Builder | Type | Value |
| --- | --- | --- |
| `vis.ui.plaintext(name)` | `plaintext` | One line of text |
| `vis.ui.multiline(name)` | `multiline` | Text with line breaks |
| `vis.ui.password(name)` | `password` | A `vis-secret:` handle |
| `vis.ui.otp(name)` | `otp` | A `vis-secret:` handle for a code of digits |
| `vis.ui.select(name, options)` | `select` | One option value |
| `vis.ui.select.many(name, options)` | `multiselect` | A list of option values |
| `vis.ui.checkbox(name)` | `checkbox` | `True` or `False` |
| `vis.ui.slider(name, min=0, max=100, step=1)` | `range` | A number |
| `vis.ui.datetime(name, mode="datetime")` | `datetime` | Text for a date, a time, or both |

[Forms and user input](human-input.md#fields) gives the rules for each input.

### Plaintext

`vis.ui.plaintext(name, label="Title")` takes one line. `min_length` and `max_length` limit it.

### Multiline

`vis.ui.multiline(name, label="Details")` takes text with line breaks.

### Password

`vis.ui.password(name, label="Token")` masks the text. The value is an opaque `vis-secret:` handle.
Read the text on the trusted side with `vis.reveal(handle)`. A live view never shows or stores the text, and a `change` event gives only
`is_set`.

### Otp

`vis.ui.otp(name, label="Code")` shows one box for each digit. The value is a `vis-secret:` handle.
[Fields](human-input.md#fields) lists its length limits.

### Select

`vis.ui.select(name, options, label="Target")` takes exactly one option. An option is a string, or
`vis.ui.select.option(value, label)` when the value and the shown text differ.

The Companion app shows two to four short options as one strip. Both clients show six or more
options as one line with the current option. A press on that line opens a list, and in the terminal,
Space opens it. Both clients show other sets of options as rows.

The list starts on the current option. It scrolls when the screen cannot show all options, and the
terminal shows at most eight rows. Arrow keys, Page Up, Page Down, Home, End, and a typed letter
move in the list. Enter or Space chooses an option and closes the list, but does not submit the
form. Escape closes the list and keeps the value.

### Multiselect

`vis.ui.select.many(name, options)` takes any number of options. The value is a list in the
declared order.

### Checkbox

`vis.ui.checkbox(name, label="I read the runbook")` takes `True` or `False`. A person must tick a
required checkbox.

### Range

`vis.ui.slider(name, min=0, max=100, step=5)` takes a number on a track. Its type is `range`.

### Datetime

`vis.ui.datetime(name, mode="date", min="2026-01-01", max="2026-12-31")` takes a date, a time, or
both. The value is text without a time zone. `mode` sets its form:

| `mode` | Form | Example |
| --- | --- | --- |
| `date` | `YYYY-MM-DD` | `2026-03-14` |
| `time` | `HH:MM` | `09:30` |
| `datetime`, the default | `YYYY-MM-DDTHH:MM` | `2026-03-14T09:30` |

`min`, `max`, and `default` have the form of the mode. Vis refuses text in another form and a value
outside the bounds. The Companion app shows the date or time control of the device. The terminal
takes typed text and shows the form as a hint.

## Actions

### Button

`vis.ui.button(id, label, action="activate")` is an action that a person presses. No code goes to
the client. The extension decides what a press does.

- `action="activate"`, the default, counts the press in `clicks` and sends an `activate` event.
- `action="submit"` collects the inputs of the nearest group of the button. Vis checks each
  required input and each validator first. Then it sends one `submit` event with all `values`.
- `is_disabled=True` refuses presses. Change it with `view[id].set(is_disabled=False)`.
- `is_selected=True` or `False` makes the button one choice of a set, such as a tab. Put only such
  buttons in one `vis.ui.wrap` or `vis.ui.row`. The Companion app shows that group as a strip of
  tabs. Each client marks the selected choice, and the terminal marks it with `•`.
- `variant` sets the rank of the button. It is `primary`, `secondary`, or `destructive`, and
  `secondary` is the default. Give the main action of a group `variant="primary"`. Give an action
  that deletes or discards data `variant="destructive"`. The terminal fills a primary button. Vis
  refuses an unknown variant and names the allowed variants in the error.

A form has its own submit and cancel buttons. Use buttons in live views.

## Layout

### Group

`vis.ui.group(id, *children, direction="column")` arranges its children. A group has no value.

- `direction="row"` puts the children side by side. They stack when the space is too narrow.
- `direction="grid"` and `direction="wrap"` arrange the children as [Layout](human-input.md#layout)
  describes. Only a grid accepts `max_columns`. Use a grid for many cards, and a wrap for buttons,
  stats, and statuses.
- `label` and `description` show a title and a short text above the children.
- `is_collapsible=True` lets a person fold the group. `default_expanded` sets the first state.
- A submit button collects only the inputs of its own group, so one view can hold several small
  forms.

`vis.ui.row(id, ...)`, `vis.ui.column(id, ...)`, `vis.ui.grid(id, ..., max_columns=3)`, and
`vis.ui.wrap(id, ...)` are short forms of a group. `vis.ui.disclosure(id, label,
...)` is a collapsible column that starts folded. A form accepts a group without an id.

```python
vis.ui.group("new-task",
          vis.ui.plaintext("title", label="Title", is_required=True),
          vis.ui.multiline("details", label="Details"),
          vis.ui.button("add", "Add task", action="submit"),
          label="New task")
```

## See also

- To open a view, change its components, and handle events, read [Live views](live-views.md).
- To ask with the same components and wait for the answer, read [Forms and user
  input](human-input.md).
- To see where each builder belongs in an extension, read [Extension
  API](extension-api.md#asking-the-human-and-showing-live-work).
