"""Counter: the smallest app. Buttons, a stat and keyboard shortcuts."""

from live_apps import ui

KEY = "counter"
TITLE = "Counter"
DESCRIPTION = "Count up and down with buttons or keys."
STEP_CHOICES = (1, 5, 10)


def initial():
    return {"count": 0, "step": 1, "presses": 0}


def reduce(state, event):
    what = ui.intent(event)
    if what in ("inc", "dec"):
        state["count"] += state["step"] if what == "inc" else -state["step"]
        state["presses"] += 1
    elif what == "reset":
        state.update(count=0, presses=state["presses"] + 1)
    elif what == "step":
        index = STEP_CHOICES.index(state["step"])
        state["step"] = STEP_CHOICES[(index + 1) % len(STEP_CHOICES)]
    return state


def render(state):
    count = state["count"]
    tone = "ok" if count > 0 else "error" if count < 0 else "idle"
    return [
        ui.heading("title", "Counter"),
        ui.stat(
            "numbers",
            [
                ui.one_stat("count", "Count", count, tone),
                ui.one_stat("step", "Step", state["step"]),
                ui.one_stat("presses", "Presses", state["presses"]),
            ],
        ),
        ui.wrap(
            "controls",
            ui.button("dec", f"\u2212{state['step']}"),
            ui.button("inc", f"+{state['step']}"),
            ui.button("step", "Change step"),
            ui.button("reset", "Reset", is_disabled=count == 0),
        ),
        ui.paragraph("help", ui.key_help(shortcuts(state))),
    ]


def shortcuts(state):
    keys = [
        ui.shortcut("+", "Up", node_id="inc"),
        ui.shortcut("-", "Down", node_id="dec"),
        ui.shortcut("ArrowUp", "Up", action="inc"),
        ui.shortcut("ArrowDown", "Down", action="dec"),
        ui.shortcut("s", "Step", action="step"),
        # An action, not the Reset button: the key works also at 0, where the
        # button is disabled.
        ui.shortcut("0", "Reset", action="reset"),
    ]
    return keys
