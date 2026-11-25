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

### Command icons

The `icon` field accepts these names on buttons, menus, `plaintext` inputs, sections,
collapsible sections, and toggle options. When an icon replaces the visible label, the label
stays the accessible name. [App icons](live-views.md#apps) use a separate list.

- `refresh`, `more`, `plus`, `search`, `filter`.
- `list`, `grid`, `columns`, `rows`, `wrap`, `expand`, `collapse`.
- `copy`, `edit`, `trash`, `sort`, `settings`.

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

`vis.ui.divider(id)` draws a rule between nodes. In a [menu](#menu), it separates commands.

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
own `vis.ui.select`. Put the selects and the submit button in one [section](#section). The
submit event holds each choice under the name of its select.

```python
ITEMS = [("item-1", "Item one"), ("item-2", "Item two"), ("item-3", "Item three"),
         ("item-4", "Item four"), ("item-5", "Item five"), ("item-6", "Item six")]

vis.ui.section(
    "sort",
    *[vis.ui.select(item_id, ["Work", "Personal"], label=label) for item_id, label in ITEMS],
    vis.ui.button("save", "Save", action="submit", variant="primary"),
    vis.ui.button("cancel", "Cancel"),
)
```

Keep the submit button directly in the section that holds the selects. A button in a nested
[flex](#flex) has a different [submit scope](live-views.md#collect-input-in-a-view).

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
| `vis.ui.select(name, options)` | `select` | One option value, or a list with `is_multiple=True` |
| `vis.ui.toggle(name, options)` | `toggle` | One option value, or a list with `is_multiple=True` |
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

By default, `vis.ui.select(name, options, label="Target")` takes exactly one option. An option is
a string, or `vis.ui.select.option(value, label)` when the value and the shown text differ.

The Companion app shows two to four short options as one strip. Both clients show six or more
options as one line with the current option. A press on that line opens a list, and in the terminal,
Space opens it. Both clients show other sets of options as rows.

The list starts on the current option. It scrolls when the screen cannot show all options, and the
terminal shows at most eight rows. Arrow keys, Page Up, Page Down, Home, End, and a typed letter
move in the list. Enter or Space chooses an option and closes the list, but does not submit the
form. Escape closes the list and keeps the value.

Use `vis.ui.select(name, options, is_multiple=True)` to let the person choose any number of
options. The value and `default` are lists in the order of `options`. Without `is_multiple=True`,
`value` and `default` are strings or `None`.

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

### Toggle

Use `vis.ui.toggle(name, options, is_multiple=False)` when all two to eight choices should stay
visible. The options appear as joined buttons. Use `is_multiple=True` to let the person choose
more than one. The value is then a list, as for [Select](#select).

Give `label` to name the input. An option from `vis.ui.toggle.option(value, label, icon=...)`
shows only the [command icon](#command-icons) when you give one. The input supports `default`,
`is_required`, and `validate`, as [Select](#select) does.

For a single-choice toggle, use the arrow keys to move focus without changing the value. Press
**Space** or **Enter** to select the focused option.

```python
vis.ui.toggle("file-layout", [
    vis.ui.toggle.option("list", "List", icon="list"),
    vis.ui.toggle.option("grid", "Grid", icon="grid"),
], label="File layout", default="list")
```

## Actions

### Button

`vis.ui.button(id, label, action="activate")` is an action that a person presses. No code goes to
the client. The extension decides what a press does.

- `action="activate"`, the default, counts the press in `clicks` and sends an `activate` event.
- `action="submit"` sends one `submit` event with the validated inputs in the button's
  [submit scope](live-views.md#collect-input-in-a-view).
- `is_disabled=True` refuses presses. Change it with `view[id].set(is_disabled=False)`.
- `is_selected=True` marks a button as pressed. In a [menu](#menu), it marks the current choice.
  Use [Toggle](#toggle) for a set of joined choices.
- `variant` sets the rank of the button. It is `primary`, `secondary`, or `destructive`, and
  `secondary` is the default. Give the main action `variant="primary"`. Give an action
  that deletes or discards data `variant="destructive"`. The terminal fills a primary button. Vis
  refuses an unknown variant and names the allowed variants in the error.

A form has its own submit and cancel buttons. Use buttons in live views.

### Menu

`vis.ui.menu(id, label, *items, icon=None)` opens a list of 1 to 64 items. A button sends an
`activate` event with its own ID. A disabled button cannot be selected. A nested menu opens a
submenu, and a divider separates commands. With an icon, the trigger shows the icon and keeps
the label as its accessible name.

Use a menu in the body of a live view, the app header, or the diff header.

```python
vis.ui.menu("file-actions", "File actions",
    vis.ui.button("rename", "Rename"),
    vis.ui.menu("export", "Export", vis.ui.button("csv", "CSV")),
    vis.ui.divider("file-action-break"),
    vis.ui.button("archive", "Archive", is_disabled=True),
)
```

## Layout

A layout needs at least one child and produces no input value. Give each layout an ID in a live
view. In a form, you can omit the ID of a flex, grid, or section.

### Flex

`vis.ui.flex(id, *children, direction="column")` puts its children in a column. Set
`direction="row"` to put them side by side. In a row, `is_wrapping=True` starts a new line
when the next child does not fit. The default is `False`.

- `gap` sets the space between children. It accepts `none`, `xs`, `sm`, `md`, and `lg`. The
  default is `md`. The terminal uses whole text cells.
- `align` sets alignment across the layout direction. It accepts `start`, `center`, `end`,
  `stretch`, and `baseline`. The default is `stretch`.
- `justify` places children along a row. It accepts `start`, `center`, `end`, and `between`.
  The default is `start`. Use `between` to put the free space between children.
- A child with `is_growing=True` takes free width in a flex row. Other parents ignore the flag.

In a terminal row, `baseline` uses the same alignment as `start`. When a row is too wide and
wrapping is off, the terminal shows marked continuation rows. The Companion app lets you scroll
sideways.

```python
vis.ui.flex("filters",
    vis.ui.plaintext("query", label="Search", is_growing=True),
    vis.ui.button("find", "Find", variant="primary"),
    direction="row", is_wrapping=True, gap="sm", align="center",
)
```

### Grid

`vis.ui.grid(id, *children, columns=None, min_width="md")` arranges children in equal columns.
As the available width decreases, fewer columns fit. At least one column stays visible.

- `columns` sets a limit of 1 to 12 columns.
- `min_width` sets the minimum column width. Choose a width from the table. The default is `md`.
- `gap` takes the same values as [Flex](#flex).

| `min_width` | Companion app | Terminal |
| --- | --- | --- |
| `xs` | `10rem` | 16 text cells |
| `sm` | `14rem` | 24 text cells |
| `md` | `18rem` | 32 text cells |
| `lg` | `24rem` | 40 text cells |

```python
vis.ui.grid("services",
    vis.ui.status("api", "API ready", tone="ok"),
    vis.ui.status("worker", "Worker ready", tone="ok"),
    columns=2, min_width="sm", gap="sm",
)
```

### Section

`vis.ui.section(id, *children, label=None, description=None, icon=None)` frames related nodes.
The header shows the label, description, and icon when you give them.

```python
vis.ui.section("build",
    vis.ui.status("checks", "Checks passed", tone="ok"),
    label="Build", description="Results from the last run", icon="refresh",
)
```

### Collapsible

`vis.ui.collapsible(id, label, *children, default_expanded=False)` lets a person show or hide
part of a form or view. Set `default_expanded=True` to show the children at first. Updates keep
the person's choice. A completed receipt starts with the children hidden.

The optional `description` stays visible under the trigger label when the section is closed.
The `icon` is optional.

```python
vis.ui.collapsible("details", "Build details",
    vis.ui.paragraph("workers", "Four workers completed the build."),
)
```

### Dialog

`vis.ui.dialog(id, title, *children, is_open=False)` shows its children in a window over a
live view when `is_open` is `True`. Give `description` to add text under the title. Call
`view[id].set(is_open=True)` to open it. When a person closes it, the host sets `is_open` to
`False` and sends an `activate` event with the dialog's ID.

```python
vis.ui.dialog("create", "New issue",
    vis.ui.plaintext("title", label="Title", is_required=True),
    vis.ui.button("save", "Create", action="submit", variant="primary"),
    description="The issue starts in Backlog.",
)
```

## See also

- To open a view, change its components, and handle events, read [Live views](live-views.md).
- To ask with the same components and wait for the answer, read [Forms and user
  input](human-input.md).
- To see where each builder belongs in an extension, read [Extension
  API](extension-api.md#asking-the-human-and-showing-live-work).
