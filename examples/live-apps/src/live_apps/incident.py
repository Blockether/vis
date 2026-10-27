"""Incident dashboard: a producer adds alerts, people ack them and Send notes.

The runner gives a `tick` event every `TICK_SECONDS`; a tick may open a new
incident and moves the open ones forward. A Send from the notes thread gets a
receipt. The owner moves it to `processing`, replies in the thread and marks it
`answered` through the effects that the reducer asks for.
"""

from live_apps import ui

KEY = "incident"
TITLE = "Incident dashboard"
DESCRIPTION = "Watch live alerts, acknowledge them and send notes to the on-call owner."
TICK_SECONDS = 2.0
READS = ("incidents",)
SERVICES = ("api", "billing", "search", "auth", "queue")
SEVERITY = ("low", "high", "critical")
MAX_OPEN = 12
MAX_LOG = 40
MAX_OUTBOX = 20


def _random(state):
    state["seed"] = (state["seed"] * 1103515245 + 12345) % (2**31)
    return state["seed"]


def initial():
    return {
        "seed": 7,
        "tick": 0,
        "next": 1,
        "incidents": [],
        "selected": [],
        "log": ["Dashboard started."],
        "is_paused": False,
        "outbox": [],
        "effect_id": 0,
        "sends": 0,
    }


def _note(state, line):
    state["log"] = (state["log"] + [f"[{state['tick']:04d}] {line}"])[-MAX_LOG:]


def _effect(state, **effect):
    state["effect_id"] += 1
    state["outbox"] = (state["outbox"] + [dict(effect, id=state["effect_id"])])[
        -MAX_OUTBOX:
    ]


def _open_incident(state):
    service = SERVICES[_random(state) % len(SERVICES)]
    severity = SEVERITY[_random(state) % len(SEVERITY)]
    incident = {
        "id": f"i{state['next']}",
        "service": service,
        "severity": severity,
        "status": "open",
        "age": 0,
    }
    state["next"] += 1
    state["incidents"].append(incident)
    _note(state, f"{incident['id']} opened: {service} {severity}")


def _tick(state):
    if state["is_paused"]:
        return
    state["tick"] += 1
    for incident in state["incidents"]:
        if incident["status"] != "resolved":
            incident["age"] += 1
        if incident["status"] == "acked" and incident["age"] >= 6:
            incident["status"] = "resolved"
            _note(state, f"{incident['id']} resolved")
    if (
        _random(state) % 3 == 0
        and sum(1 for i in state["incidents"] if i["status"] != "resolved") < MAX_OPEN
    ):
        _open_incident(state)
    # Keep the table short: drop resolved incidents beyond the newest five.
    resolved = [i for i in state["incidents"] if i["status"] == "resolved"]
    drop = {i["id"] for i in resolved[:-5]}
    state["incidents"] = [i for i in state["incidents"] if i["id"] not in drop]


def _learn(state, event):
    read = event.get("read") or {}
    if "incidents" in read:
        state["selected"] = list(read["incidents"] or [])
    if event.get("type") == "select" and event.get("node_id") == "incidents":
        state["selected"] = list(event.get("item_ids") or [])
    known = {i["id"] for i in state["incidents"]}
    state["selected"] = [i for i in state["selected"] if i in known]


def reduce(state, event):
    kind = event.get("type")
    if kind == "tick":
        _tick(state)
        return state
    _learn(state, event)
    what = ui.intent(event)
    if what in ("ack", "resolve"):
        chosen = [i for i in state["incidents"] if i["id"] in state["selected"]]
        target = "acked" if what == "ack" else "resolved"
        changed = [i for i in chosen if i["status"] not in (target, "resolved")]
        for incident in changed:
            incident["status"] = target
            _note(state, f"{incident['id']} {target} by a person")
        if not chosen:
            _note(state, "Select an incident first.")
    elif what == "pause":
        state["is_paused"] = not state["is_paused"]
        _note(state, "Paused." if state["is_paused"] else "Resumed.")
    elif what == "spawn":
        _open_incident(state)
    elif kind == "submit" and event.get("submission_id"):
        state["sends"] += 1
        bodies = [c.get("body", "") for c in event.get("comments") or []]
        text = " / ".join(b for b in bodies if b) or "(no text)"
        _note(state, f"note received: {text[:80]}")
        sub = event["submission_id"]
        open_count = sum(1 for i in state["incidents"] if i["status"] == "open")
        _effect(
            state,
            do="set_status",
            submission_id=sub,
            status="processing",
            note="The on-call owner reads it.",
        )
        _effect(
            state,
            do="reply",
            node_id="notes",
            submission_id=sub,
            body=f"Noted. {open_count} incident(s) are still open; the on-call owner has your note.",
        )
    return state


def effects(old, new):
    """The new effects of this step, oldest first."""
    seen = old["effect_id"]
    return [e for e in new["outbox"] if e["id"] > seen]


TONE = {"critical": "error", "high": "warn", "low": "idle"}


def render(state):
    active = [i for i in state["incidents"] if i["status"] != "resolved"]
    rows = [
        ui.table_row(
            i["id"],
            [
                i["id"],
                i["service"],
                i["severity"],
                i["status"],
                f"{i['age'] * TICK_SECONDS:.0f}s",
            ],
            tone="ok"
            if i["status"] == "resolved"
            else "running"
            if i["status"] == "acked"
            else TONE[i["severity"]],
        )
        for i in state["incidents"]
    ]
    worst = (
        "error"
        if any(i["severity"] == "critical" and i["status"] == "open" for i in active)
        else ("warn" if any(i["status"] == "open" for i in active) else "ok")
    )
    open_count = sum(1 for i in active if i["status"] == "open")
    return [
        ui.heading("title", "Incident dashboard"),
        ui.stat(
            "summary",
            [
                ui.one_stat(
                    "open", "Open", open_count, "error" if open_count else "ok"
                ),
                ui.one_stat(
                    "acked",
                    "Acked",
                    sum(1 for i in active if i["status"] == "acked"),
                    "running",
                ),
                ui.one_stat(
                    "resolved",
                    "Resolved",
                    sum(1 for i in state["incidents"] if i["status"] == "resolved"),
                    "ok",
                ),
                ui.one_stat("sends", "Notes sent", state["sends"]),
            ],
        ),
        ui.status(
            "health",
            "Paused"
            if state["is_paused"]
            else ("All clear" if worst == "ok" else f"{open_count} open incident(s)"),
            "idle" if state["is_paused"] else worst,
            detail=f"tick {state['tick']}",
        ),
        ui.table(
            "incidents",
            [
                ("id", "Id"),
                ("service", "Service"),
                ("severity", "Severity"),
                ("status", "Status"),
                ("age", "Age"),
            ],
            rows,
            is_selectable=True,
            label="Incidents",
            selected_ids=list(state["selected"]),
        ),
        ui.wrap(
            "actions",
            ui.button("ack", "Ack", is_disabled=not state["selected"]),
            ui.button("resolve", "Resolve", is_disabled=not state["selected"]),
            ui.button("pause", "Resume" if state["is_paused"] else "Pause"),
            ui.button("spawn", "Simulate alert"),
        ),
        {
            "type": "thread",
            "id": "notes",
            "label": "Notes for the on-call owner",
            "messages": [],
        },
        ui.log("events", state["log"], label="Event log", window_lines=MAX_LOG),
    ]


def shortcuts(state):
    return [
        ui.shortcut("a", "Ack", action="ack"),
        ui.shortcut("r", "Resolve", action="resolve"),
        ui.shortcut("p", "Resume" if state["is_paused"] else "Pause", node_id="pause"),
        ui.shortcut("!", "Simulate", action="spawn"),
    ]
