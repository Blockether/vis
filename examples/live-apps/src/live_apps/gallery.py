"""Gallery: one card for each proof app. Press a card (or its number) to open it."""

from live_apps import counter, game2048, incident, kanban, todo, ui, wizard

KEY = "gallery"
TITLE = "Live apps"
DESCRIPTION = "Open one of the proof apps."
APPS = (counter, todo, kanban, game2048, incident, wizard)
LEVEL = {
    "counter": "Simple",
    "todo": "Simple",
    "kanban": "Medium",
    "2048": "Medium",
    "incident": "Complex",
    "wizard": "Complex",
}


def initial():
    return {"opened": [], "effect_id": 0, "outbox": []}


def reduce(state, event):
    what = ui.intent(event)
    keys = {f"open-{app.KEY}": app.KEY for app in APPS}
    if what in keys:
        key = keys[what]
        state["opened"] = (state["opened"] + [key])[-20:]
        state["effect_id"] += 1
        state["outbox"] = (
            state["outbox"] + [{"id": state["effect_id"], "do": "open", "app": key}]
        )[-20:]
    return state


def effects(old, new):
    return [e for e in new["outbox"] if e["id"] > old["effect_id"]]


def render(state):
    cards = [
        ui.card(
            f"open-{app.KEY}",
            f"{index}. {app.TITLE}",
            text=app.DESCRIPTION,
            meta=LEVEL[app.KEY],
            is_selectable=True,
            tone="ok" if app.KEY in state["opened"] else None,
        )
        for index, app in enumerate(APPS, start=1)
    ]
    last = state["opened"][-1] if state["opened"] else None
    return [
        ui.heading("title", "Live apps"),
        ui.paragraph(
            "intro",
            "Each card opens one app in this session. The apps keep their state.",
        ),
        ui.grid("apps", *cards, max_columns=3),
        ui.status(
            "last",
            f"Opened: {last}" if last else "Nothing opened yet.",
            "ok" if last else "idle",
        ),
    ]


def shortcuts(state):
    return [
        ui.shortcut(str(index), app.TITLE[:40], node_id=f"open-{app.KEY}")
        for index, app in enumerate(APPS, start=1)
    ]
