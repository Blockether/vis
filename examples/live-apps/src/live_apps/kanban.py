"""Kanban board: three selectable columns, moves between them and a card editor."""

from live_apps import ui

KEY = "kanban"
TITLE = "Kanban"
DESCRIPTION = "Plan work on a board: add cards and move them between columns."
COLUMNS = (("todo", "To do"), ("doing", "Doing"), ("done", "Done"))
COLUMN_IDS = tuple(c for c, _ in COLUMNS)
READS = COLUMN_IDS
# Only the selections: a select sends no event yet. The title input sends
# ordered `change` events.
WIP_LIMIT = 3
PRIORITIES = ("low", "normal", "high")


def initial():
    return {
        "cards": [
            {
                "id": "c1",
                "title": "Write the brief",
                "column": "done",
                "priority": "normal",
            },
            {
                "id": "c2",
                "title": "Build the board",
                "column": "doing",
                "priority": "high",
            },
            {"id": "c3", "title": "Ship it", "column": "todo", "priority": "normal"},
        ],
        "next": 4,
        "draft": "",
        "selected": {c: [] for c in COLUMN_IDS},
        "notice": "Select a card, then move it with the buttons or the h and l keys.",
    }


def _learn(state, event):
    read = event.get("read") or {}
    if "title" in read:
        state["draft"] = str(read["title"] or "")
    # A person chooses in one column at a time: the column whose selection
    # changed is the new choice, and the other columns let go of theirs.
    changed = [
        c
        for c in COLUMN_IDS
        if c in read and list(read[c] or []) != state["selected"][c]
    ]
    for column in COLUMN_IDS:
        if changed:
            state["selected"][column] = (
                list(read[column] or []) if column in changed else []
            )
    if event.get("type") == "change" and event.get("node_id") == "title":
        state["draft"] = str(event.get("value") or "")
    if event.get("type") == "select" and event.get("node_id") in COLUMN_IDS:
        state["selected"] = {c: [] for c in COLUMN_IDS}
        state["selected"][event["node_id"]] = list(event.get("item_ids") or [])
    for column in COLUMN_IDS:
        owned = {c["id"] for c in state["cards"] if c["column"] == column}
        state["selected"][column] = [i for i in state["selected"][column] if i in owned]


def chosen(state):
    """The selected cards, in board order."""
    ids = {i for column in COLUMN_IDS for i in state["selected"][column]}
    return [c for c in state["cards"] if c["id"] in ids]


def _move(state, step):
    cards = chosen(state)
    if not cards:
        state["notice"] = "Select a card first."
        return
    moved = []
    for card in cards:
        index = COLUMN_IDS.index(card["column"]) + step
        if not 0 <= index < len(COLUMN_IDS):
            continue
        target = COLUMN_IDS[index]
        load = sum(1 for c in state["cards"] if c["column"] == target)
        if target == "doing" and load >= WIP_LIMIT:
            state["notice"] = (
                f"Doing holds at most {WIP_LIMIT} cards. Finish one first."
            )
            return
        state["selected"][card["column"]].remove(card["id"])
        card["column"] = target
        state["selected"][target].append(card["id"])
        moved.append(card["title"])
    state["notice"] = (
        f"Moved: {', '.join(moved)}" if moved else "The card is already at the edge."
    )


def reduce(state, event):
    _learn(state, event)
    what = ui.intent(event)
    if what == "add":
        title = state["draft"].strip()
        if not title:
            state["notice"] = "Write a card title first."
        else:
            card = {
                "id": f"c{state['next']}",
                "title": title[:80],
                "column": "todo",
                "priority": "normal",
            }
            state["cards"].append(card)
            state["next"] += 1
            state["draft"] = ""
            state["notice"] = f"Added: {card['title']}"
    elif what in ("left", "right"):
        _move(state, -1 if what == "left" else 1)
    elif what == "priority":
        cards = chosen(state)
        for card in cards:
            card["priority"] = PRIORITIES[
                (PRIORITIES.index(card["priority"]) + 1) % len(PRIORITIES)
            ]
        state["notice"] = "Select a card first." if not cards else "Priority changed."
    elif what == "delete":
        ids = {c["id"] for c in chosen(state)}
        state["cards"] = [c for c in state["cards"] if c["id"] not in ids]
        state["selected"] = {c: [] for c in COLUMN_IDS}
        state["notice"] = (
            f"Deleted {len(ids)} card(s)." if ids else "Select a card first."
        )
    return state


TONE = {"high": "warn", "normal": None, "low": "idle"}


def render(state):
    columns = []
    for column, label in COLUMNS:
        cards = [c for c in state["cards"] if c["column"] == column]
        rows = [
            ui.table_row(c["id"], [c["title"], c["priority"]], tone=TONE[c["priority"]])
            for c in cards
        ]
        columns.append(
            ui.table(
                column,
                [("title", label), ("priority", "Priority")],
                rows,
                is_selectable=True,
                label=f"{label} ({len(cards)})",
                selected_ids=list(state["selected"][column]),
            )
        )
    picked = chosen(state)
    return [
        ui.heading("title-heading", "Kanban"),
        ui.grid("board", *columns, max_columns=3),
        ui.row(
            "editor",
            ui.field(
                "plaintext",
                "title",
                "New card",
                placeholder="Card title",
                value=state["draft"] or None,
            ),
            ui.button("add", "Add card"),
        ),
        ui.wrap(
            "moves",
            ui.button("left", "\u2190 Move left", is_disabled=not picked),
            ui.button("right", "Move right \u2192", is_disabled=not picked),
            ui.button("priority", "Priority", is_disabled=not picked),
            ui.button("delete", "Delete", is_disabled=not picked),
        ),
        ui.status(
            "notice",
            state["notice"],
            "warn" if "at most" in state["notice"] else "idle",
        ),
    ]


def shortcuts(state):
    return [
        ui.shortcut("ArrowLeft", "Left", action="left"),
        ui.shortcut("ArrowRight", "Right", action="right"),
        ui.shortcut("h", "Left", action="left"),
        ui.shortcut("l", "Right", action="right"),
        ui.shortcut("p", "Priority", action="priority"),
        ui.shortcut("n", "Add", action="add"),
        ui.shortcut("Delete", "Delete", action="delete"),
    ]
