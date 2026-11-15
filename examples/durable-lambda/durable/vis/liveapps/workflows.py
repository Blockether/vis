"""The Workflows app: a global app of Vis that the worker owns.

Vis shows a global app behind the global apps button of its header, in each session
and without one. The app has three tabs. `Run` starts a workflow and follows the run.
`Gates` shows the run that waits for the decision of a person and takes the decision.
`History` lists the last runs.

One thread owns the app. It reads the actions of a person and it paints the app, so
the view has one writer.
"""

from __future__ import annotations

import threading
import time
from collections.abc import Callable
from typing import Any

import blockether.vis.extension as vis
from blockether.vis.engine import GatewayClient, GatewayError, TransportError

from durable.app import Api
from durable.domain import OPEN, Refused
from durable.helpers import log
from durable.settings import APP_RUNS, LOOK_S

TABS = (("run", "Run"), ("gates", "Gates"), ("history", "History"))
# The tone of a status. The names are the live tones of the Vis view contract.
TONES = {"queued": "idle", "running": "running", "waiting": "warn", "completed": "ok", "failed": "error", "cancelled": "warn"}
# The counters of the History tab: an id, a label, the statuses that it counts and its tone.
COUNTS = (("open", "In progress", OPEN, "running"), ("completed", "Completed", ("completed",), "ok"), ("failed", "Failed", ("failed",), "error"))
# The value of the session field for a run that makes its own session.
NEW = "new"
# What the Gates tab says when no run waits.
NO_GATE = ("No run waits", "idle", "A run that waits for the decision of a person shows here.")


def tabs(tab: str) -> dict[str, Any]:
    """Return the row of tabs. Vis marks the button with `is_selected`."""
    return vis.ui.wrap("tabs", *(vis.ui.button(f"tab-{name}", label, is_selected=name == tab) for name, label in TABS))


def done(activities: list[list[str]]) -> str:
    """Return the completed activities of a run as `completed/all`."""
    return f"{sum(1 for _, status in activities if status == 'completed')}/{len(activities)}"


class Workflows(threading.Thread):
    """Open the Workflows app and serve it until the gateway stops."""

    def __init__(self, api: Api, client: Callable[[], GatewayClient]) -> None:
        """Keep the API and `client`, which returns the SDK client of the thread that calls it."""
        super().__init__(name="workflows-app", daemon=True)
        self.api, self.client = api, client
        self.tab = "run"
        # The run that the Run tab follows, or the reason why the last run did not start.
        self.followed: str | None = None
        self.refusal = ""
        # The picture that the app shows now, and whether the open tab has the status node.
        self.shown: dict[str, Any] = {}
        self.has_status = False
        # The reason why the API did not take the last decision at a gate.
        self.problem = ""

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
        sessions = [vis.ui.option(session, title) for session, title in self.api.sessions.places()]
        return vis.ui.column(
            "body",
            vis.ui.paragraph("about", "Select a workflow and give its input. A run works in its own new Vis session, or in a session that you select."),
            vis.ui.group(
                "start",
                vis.ui.select("workflow", [vis.ui.option(flow["name"], flow["title"]) for flow in flows], label="Workflow", is_required=True),
                vis.ui.multiline("input", label="Input", description="A text for the first input of the workflow, or a JSON object."),
                vis.ui.select(
                    "session",
                    [vis.ui.option(NEW, "A new session for this run"), *sessions],
                    label="Session",
                    description="In a session that you select, the run works in the directory of that session.",
                ),
                vis.ui.button("start-run", "Run workflow", action="submit", variant="primary"),
            ),
        )

    def gates_tab(self) -> dict[str, Any]:
        """Return the place of the gate. `paint` puts the gate of the oldest waiting run below the status."""
        text, tone, detail = NO_GATE
        return vis.ui.column("body", vis.ui.status("state", text, tone=tone, detail=detail))

    def gate_form(self, gate: tuple[str, str, str, str | None, str | None]) -> dict[str, Any]:
        """Return the form of a gate: what to check, its document, a comment and the two decisions."""
        _, _, about, document, session = gate
        nodes = [vis.ui.paragraph("gate-about", about)] if about else []
        if document and session:
            try:
                body = self.api.files.attached_file(session, document)[2]
                nodes.append(vis.ui.disclosure("gate-document", document, vis.ui.code("gate-text", body.decode(errors="replace")[:20000], language="markdown"), default_expanded=True))
            except Refused:  # S3 has no copy of the file: the session of the run has it
                nodes.append(vis.ui.paragraph("gate-document", f"Read {document} in the session of the run."))
        return vis.ui.group(
            "gate",
            *nodes,
            vis.ui.multiline("comment", label="Comment", description="For a rejection: the change that you want."),
            vis.ui.button("gate-approve", "Approve", action="submit", variant="primary"),
            vis.ui.button("gate-reject", "Reject", action="submit"),
        )

    def history_tab(self) -> dict[str, Any]:
        # Three columns: a phone shows them without a break in a word.
        """Return the counters and the table of the last runs, without their data."""
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
                for line in lines[:APP_RUNS]
            ]
            return {"counts": counts, "rows": rows}
        if self.tab == "gates":
            waiting = [line["id"] for line in self.api.runs.index if line["status"] == "waiting"]
            # The oldest waiting run comes first. The index has the newest run first.
            run = self.api.runs.copy(waiting[-1]) if waiting else {}
            gate = run.get("waiting")
            if not gate:
                return {"state": NO_GATE, "steps": [], "gate": None}
            more = f" {len(waiting) - 1} more wait." if len(waiting) > 1 else ""
            detail = f"Run {run['id'][:8]} of {run['title']}.{more} {self.problem}".rstrip()
            return {"state": (gate["title"], "warn", detail), "steps": [], "gate": (run["id"], gate["key"], gate["about"], gate["document"], run["session_id"])}
        if self.refusal:
            return {"state": ("The run did not start", "error", self.refusal), "steps": []}
        if not self.followed:
            return {}
        run = self.api.runs.copy(self.followed)
        detail = f"Run {run['id'][:8]}. Activities completed: {done([[step['title'], step['status']] for step in run['activities']])}."
        if run["status"] == "waiting":
            detail += f" It waits for a decision: {run['waiting']['title']}. Decide on the Gates tab."
        if run["status"] == "failed" and run.get("error"):
            detail += f" {str(run['error'])[:200]}"
        steps = [(f"step-{at}", step["title"], TONES.get(step["status"], "idle")) for at, step in enumerate(run["activities"])]
        # The status node names the tone, so the text names only the workflow.
        return {"state": (run["title"], TONES[run["status"]], detail), "steps": steps}

    # -- paint --------------------------------------------------------------------

    def show(self, app: Any, tab: str) -> None:
        """Change the open tab: put the row of tabs and the content of `tab` in the app."""
        self.tab, self.shown, self.problem = tab, {}, ""
        self.has_status = tab == "gates" or (tab == "run" and bool(self.followed))
        body = {"history": self.history_tab, "gates": self.gates_tab, "run": self.run_tab}[tab]()
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
            if "gate" in picture and picture["gate"] != self.shown.get("gate"):
                if self.shown.get("gate"):
                    app.drop("gate")
                if picture["gate"]:
                    app.add(self.gate_form(picture["gate"]), after="state")
        self.shown = picture

    # -- the actions of a person ---------------------------------------------------

    def handle(self, app: Any, event: Any) -> None:
        """Do one action of a person: change the tab, start a run, decide at a gate or show the form again."""
        if event.type == "activate" and event.node_id in {f"tab-{name}" for name, _ in TABS}:
            self.show(app, event.node_id.removeprefix("tab-"))
        elif event.type == "submit" and event.node_id == "start-run":
            values = event.values or {}
            try:
                session = values.get("session")
                asked = {"workflow": values.get("workflow"), "input": values.get("input") or "", "session": None if session in (None, "", NEW) else session}
                self.followed, self.refusal = self.api.run(asked)["id"], ""
            except Refused as refusal:
                self.followed, self.refusal = None, str(refusal)
            else:
                self.show(app, "run")
        elif event.type == "submit" and event.node_id in ("gate-approve", "gate-reject") and self.shown.get("gate"):
            run, key = self.shown["gate"][:2]
            try:
                self.api.decide(run, key, payload={"approved": event.node_id == "gate-approve", "comment": (event.values or {}).get("comment") or ""})
                self.problem = ""
            except Refused as refusal:
                self.problem = f"The decision did not go in: {refusal}."
        elif event.type == "activate" and event.node_id == "new-run":
            self.followed = None
            self.show(app, "run")

    def run(self) -> None:
        """Open the app with the SDK client of this thread, then serve its events and paint it until the gateway stops."""
        app = vis.ui.app(
            "Workflows",
            tabs(self.tab),
            self.run_tab(),
            description="Start a workflow of Durable Lambda, decide at its gates and see the runs.",
            key="workflows",
            scope="global",
            # The worker opens the app at each start, so a gateway restart can drop it.
            is_persistent=False,
        )
        try:
            app = self.client().attach_app(app, owner="durable")
            while True:
                # The wait is also the pause between two looks at the runs.
                for event in app.events(timeout=LOOK_S):
                    self.handle(app, event)
                self.paint(app)
        except (GatewayError, TransportError, vis.Interrupted) as failure:  # the gateway stops with the worker
            log("the workflows app stops", repr(failure))
