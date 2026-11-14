"""The Workflows app: a global app of Vis that the worker owns.

Vis shows a global app behind the global apps button of its header, in each session
and without one. The app has two tabs. `Run` starts a workflow and follows the run.
`History` lists the last runs.

One thread owns the app. It reads the actions of a person and it paints the app, so
the view has one writer.
"""

from __future__ import annotations

import threading
import time
from typing import Any

import blockether.vis.extension as vis
from blockether.vis.engine import GatewayClient, GatewayError, TransportError

from playground.api import OPEN, Api, Refused
from playground.env import log

TABS = (("run", "Run"), ("history", "History"))
# The tone of a status. The names are the live tones of the Vis view contract.
TONES = {"queued": "idle", "running": "running", "waiting": "warn", "completed": "ok", "failed": "error", "cancelled": "warn"}
# The counters of the History tab: an id, a label, the statuses that it counts and its tone.
COUNTS = (("open", "In progress", OPEN, "running"), ("completed", "Completed", ("completed",), "ok"), ("failed", "Failed", ("failed",), "error"))
# The runs that the History tab lists, and the seconds between two looks at the runs.
LISTED, LOOK_S = 30, 2


def tabs(tab: str) -> dict[str, Any]:
    """Return the row of tabs. Vis marks the button with `is_selected`."""
    return vis.ui.wrap("tabs", *(vis.ui.button(f"tab-{name}", label, is_selected=name == tab) for name, label in TABS))


def done(activities: list[list[str]]) -> str:
    return f"{sum(1 for _, status in activities if status == 'completed')}/{len(activities)}"


class Workflows(threading.Thread):
    """Open the Workflows app and serve it until the gateway stops."""

    def __init__(self, api: Api, client: GatewayClient) -> None:
        super().__init__(name="workflows-app", daemon=True)
        self.api, self.client = api, client
        self.tab = "run"
        # The run that the Run tab follows, or the reason why the last run did not start.
        self.followed: str | None = None
        self.refusal = ""
        # The picture that the app shows now, and whether the open tab has the status node.
        self.shown: dict[str, Any] = {}
        self.has_status = False

    # -- the content of each tab ------------------------------------------------

    def run_tab(self) -> dict[str, Any]:
        """Return the form, or the progress of the followed run in its place."""
        if self.followed:
            headline, tone, detail = self.picture()["state"]
            return vis.ui.column(
                "body",
                vis.ui.status("state", headline, tone=tone, detail=detail),
                vis.ui.steps("activities", []),
                vis.ui.button("new-run", "New run"),
            )
        flows = self.api.workflows()["workflows"]
        return vis.ui.column(
            "body",
            vis.ui.paragraph("about", "Select a workflow and give its input. Each run works in its own Vis session."),
            vis.ui.group(
                "start",
                vis.ui.select("workflow", [vis.ui.option(flow["name"], flow["title"]) for flow in flows], label="Workflow", is_required=True),
                vis.ui.multiline("input", label="Input", description="A text for the first input of the workflow, or a JSON object."),
                vis.ui.button("start-run", "Run workflow", action="submit", variant="primary"),
            ),
        )

    def history_tab(self) -> dict[str, Any]:
        # Three columns: a phone shows them without a break in a word.
        columns = [("workflow", "Workflow"), ("started", "Started (UTC)"), ("status", "Status")]
        return vis.ui.column(
            "body",
            vis.ui.stat("counts", [{"id": count, "label": label, "value_text": "0"} for count, label, _, _ in COUNTS]),
            vis.ui.table("runs", [vis.ui.table_column(column, label) for column, label in columns]),
        )

    def picture(self) -> dict[str, Any]:
        """Return the data that the open tab shows."""
        if self.tab == "history":
            lines = self.api.runs.index
            counts = {count: sum(1 for line in lines if line["status"] in statuses) for count, _, statuses, _ in COUNTS}
            rows = [
                (line["id"], [line["title"], time.strftime("%d %b %H:%M", time.gmtime(line["created"])), f"{line['status'].capitalize()} {done(line['activities'])}"], TONES[line["status"]])
                for line in lines[:LISTED]
            ]
            return {"counts": counts, "rows": rows}
        if self.refusal:
            return {"state": ("The run did not start", "error", self.refusal), "steps": []}
        if not self.followed:
            return {}
        run = self.api.runs.copy(self.followed)
        detail = f"Run {run['id'][:8]}. Activities completed: {done([[step['title'], step['status']] for step in run['activities']])}."
        if run["status"] == "waiting":
            # The Workflows page of the playground takes the file.
            detail += f" It waits for a file: {run['waiting']['title']}. Give it on the Workflows page."
        if run["status"] == "failed" and run.get("error"):
            detail += f" {str(run['error'])[:200]}"
        steps = [(f"step-{at}", step["title"], TONES.get(step["status"], "idle")) for at, step in enumerate(run["activities"])]
        # The status node names the tone, so the text names only the workflow.
        return {"state": (run["title"], TONES[run["status"]], detail), "steps": steps}

    # -- paint --------------------------------------------------------------------

    def show(self, app: Any, tab: str) -> None:
        """Change the open tab: put the row of tabs and the content of `tab` in the app."""
        self.tab, self.shown, self.has_status = tab, {}, tab == "run" and bool(self.followed)
        body = self.history_tab() if tab == "history" else self.run_tab()
        with app.batch():
            for node in ("tabs", "body"):
                app.drop(node)
            app.add(tabs(tab))
            app.add(body)
        self.paint(app)

    def paint(self, app: Any) -> None:
        """Put the data of the open tab in the app, when it changed."""
        picture = self.picture()
        if picture == self.shown:
            return
        with app.batch():
            if "rows" in picture:
                for count, label, _, tone in COUNTS:
                    app["counts"].set(count, str(picture["counts"][count]), label, tone=tone if picture["counts"][count] else None)
                app["runs"].replace([vis.ui.table_row(run, cells, tone=tone) for run, cells, tone in picture["rows"]])
            if "state" in picture:
                text, tone, detail = picture["state"]
                if not self.has_status:
                    # The reason of a refusal stands above the form: a person sees it first.
                    app.add(vis.ui.status("state", text, tone=tone, detail=detail), after="about")
                    self.has_status = True
                app["state"].set(text, tone=tone, detail=detail)
                if picture["steps"] and picture["steps"] != self.shown.get("steps"):
                    app["activities"].clear()
                    for step, label, step_tone in picture["steps"]:
                        app["activities"].set(step, label, tone=step_tone)
        self.shown = picture

    # -- the actions of a person ---------------------------------------------------

    def handle(self, app: Any, event: Any) -> None:
        if event.type == "activate" and event.node_id in {f"tab-{name}" for name, _ in TABS}:
            self.show(app, event.node_id.removeprefix("tab-"))
        elif event.type == "submit" and event.node_id == "start-run":
            values = event.values or {}
            try:
                self.followed, self.refusal = self.api.run({"workflow": values.get("workflow"), "input": values.get("input") or ""})["id"], ""
            except Refused as refusal:
                self.followed, self.refusal = None, str(refusal)
            else:
                self.show(app, "run")
        elif event.type == "activate" and event.node_id == "new-run":
            self.followed = None
            self.show(app, "run")

    def run(self) -> None:
        app = vis.ui.app(
            "Workflows",
            tabs(self.tab),
            self.run_tab(),
            description="Start a workflow of the playground and see the runs.",
            key="workflows",
            scope="global",
            # The worker opens the app at each start, so a gateway restart can drop it.
            is_persistent=False,
        )
        try:
            app = self.client.attach_app(app, owner="playground")
            while True:
                # The wait is also the pause between two looks at the runs.
                for event in app.events(timeout=LOOK_S):
                    self.handle(app, event)
                self.paint(app)
        except (GatewayError, TransportError, vis.Interrupted) as failure:  # the gateway stops with the worker
            log("the workflows app stops", repr(failure))
