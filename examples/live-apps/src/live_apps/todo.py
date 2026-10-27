"""Todo list: an input, a selectable table, filters and bulk actions."""

from live_apps import ui

KEY = "todo"
TITLE = "Todo"
DESCRIPTION = "Add tasks, mark them done and filter the list."
READS = ("items",)
# Only the selection: a select sends no event yet. Inputs send ordered `change`
# events; reading their live value would give a later value to an earlier event.
FILTERS = ("all", "active", "done")
MAX_TEXT = 120


def initial():
    return {
        "items": [],
        "next": 1,
        "filter": "all",
        "draft": "",
        "selected": [],
        "notice": "Write a task and press Add.",
    }


def _learn(state, event):
    """Keep what the person typed and chose: the render shows it as it is."""
    read = event.get("read") or {}
    if "new" in read:
        state["draft"] = str(read["new"] or "")
    if "items" in read:
        state["selected"] = list(read["items"] or [])
    if event.get("type") == "change" and event.get("node_id") == "new":
        state["draft"] = str(event.get("value") or "")
    if event.get("type") == "select" and event.get("node_id") == "items":
        state["selected"] = list(event.get("item_ids") or [])
    known = {t["id"] for t in state["items"]}
    state["selected"] = [i for i in state["selected"] if i in known]


def reduce(state, event):
    _learn(state, event)
    what = ui.intent(event)
    if what == "add":
        text = state["draft"].strip()
        if not text:
            state["notice"] = "Write a task first."
        elif len(text) > MAX_TEXT:
            state["notice"] = f"A task has at most {MAX_TEXT} characters."
        else:
            state["items"].append(
                {"id": f"t{state['next']}", "text": text, "is_done": False}
            )
            state["next"] += 1
            state["notice"] = f"Added: {text}"
            state["draft"] = ""
    elif what in ("toggle", "delete"):
        chosen = state["selected"]
        if not chosen:
            state["notice"] = "Select one or more tasks in the list first."
        elif what == "toggle":
            for item in state["items"]:
                if item["id"] in chosen:
                    item["is_done"] = not item["is_done"]
            state["notice"] = f"Changed {len(chosen)} task(s)."
        else:
            state["items"] = [t for t in state["items"] if t["id"] not in chosen]
            state["notice"] = f"Deleted {len(chosen)} task(s)."
            state["selected"] = []
    elif what == "clear-done":
        before = len(state["items"])
        state["items"] = [t for t in state["items"] if not t["is_done"]]
        state["notice"] = f"Removed {before - len(state['items'])} done task(s)."
        known = {t["id"] for t in state["items"]}
        state["selected"] = [i for i in state["selected"] if i in known]
    elif what == "filter":
        state["filter"] = FILTERS[(FILTERS.index(state["filter"]) + 1) % len(FILTERS)]
    elif what in ("show-all", "show-active", "show-done"):
        state["filter"] = what.removeprefix("show-")
    return state


def visible(state):
    keep = {
        "all": lambda t: True,
        "active": lambda t: not t["is_done"],
        "done": lambda t: t["is_done"],
    }
    return [t for t in state["items"] if keep[state["filter"]](t)]


def render(state):
    done = sum(1 for t in state["items"] if t["is_done"])
    rows = [
        ui.table_row(
            t["id"],
            ["\u2713" if t["is_done"] else "\u00b7", t["text"]],
            tone="ok" if t["is_done"] else None,
        )
        for t in visible(state)
    ]
    shown = {t["id"] for t in visible(state)}
    items = ui.table(
        "items",
        [("state", "Done"), ("task", "Task")],
        rows,
        is_selectable=True,
        label="Tasks",
        selected_ids=[i for i in state["selected"] if i in shown],
    )
    entry = ui.field(
        "plaintext",
        "new",
        "New task",
        placeholder="What needs doing?",
        value=state["draft"] or None,
    )
    return [
        ui.heading("title", "Todo"),
        ui.stat(
            "counts",
            [
                ui.one_stat("open", "Open", len(state["items"]) - done, "running"),
                ui.one_stat("done", "Done", done, "ok"),
                ui.one_stat("filter", "Showing", state["filter"]),
            ],
        ),
        ui.row("entry", entry, ui.button("add", "Add")),
        items,
        ui.wrap(
            "actions",
            ui.button("toggle", "Done / undo"),
            ui.button("delete", "Delete"),
            ui.button("clear-done", "Clear done", is_disabled=done == 0),
            ui.button("show-all", "All", is_disabled=state["filter"] == "all"),
            ui.button("show-active", "Active", is_disabled=state["filter"] == "active"),
            ui.button("show-done", "Done", is_disabled=state["filter"] == "done"),
        ),
        ui.status("notice", state["notice"]),
    ]


def shortcuts(state):
    return [
        ui.shortcut("a", "Add", action="add"),
        ui.shortcut("x", "Done", action="toggle"),
        ui.shortcut("Delete", "Delete", action="delete"),
        ui.shortcut("f", "Filter", action="filter"),
    ]
