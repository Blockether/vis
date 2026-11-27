"""The records and processes of workflow runs."""

from __future__ import annotations

import os
import subprocess
import sys
import threading
import time
from collections.abc import Callable
from pathlib import Path
from typing import Any, Protocol

from vis_optmem.shared import Absent, Condition, Equals, Table

from factory import settings
from factory.records import FILE, ID, NAME, SESSION, Document, Records, Refused, need, new_id, pack, unpack

LIVE = ("queued", "running")
OPEN = (*LIVE, "waiting")


class Process(Protocol):
    """The calls that the run manager needs from a child process."""

    def poll(self) -> int | None:
        """Return the exit code, or None while the process runs."""

    def terminate(self) -> None:
        """Ask the process to end when the factory stops."""

    def kill(self) -> None:
        """Stop a cancelled run process."""


class Starts(Protocol):
    """Start a process for one run."""

    def start(self, run_id: str) -> Process:
        """Start the run and return its process."""


class Starter:
    """Start each run in the project directory."""

    def start(self, run_id: str) -> subprocess.Popen[bytes]:
        """Start a child with the factory API address and token."""
        example = str(Path(__file__).resolve().parents[1])
        pythonpath = os.pathsep.join(part for part in (example, settings.ENV.get("PYTHONPATH", "")) if part)
        env = {**settings.ENV, "PYTHONPATH": pythonpath, "PYTHONUNBUFFERED": "1", "FACTORY_URL": settings.LOCAL_URL, "FACTORY_TOKEN": settings.TOKEN}
        return subprocess.Popen([sys.executable, "-m", "factory.workflows.process", run_id], cwd=settings.PROJECT, env=env)


class Runs:
    """Keep run documents on a table and manage the child processes of one factory."""

    def __init__(self, table: Table, starter: Starts, busy: Callable[[str], bool] | None = None) -> None:
        """Use the given table and starter. A later fill starts any live runs."""
        self.records, self.starter, self.busy = Records(table), starter, busy
        self.lock = threading.RLock()
        self.processes: dict[str, Process] = {}
        # A factory recovers the running records that it found on startup. It does
        # not take a run that another scheduler starts after this first read.
        self.recover = {doc["id"]: doc["starts"] for doc in self.listing(status="running")}
        self.stopped = False

    def listing(self, workflow: str = "", status: str = "") -> list[Document]:
        """List the runs newest first, with optional filters."""
        return [doc for doc in self.records.listing("run/") if (not workflow or doc["workflow"] == workflow) and (not status or doc["status"] == status)]

    def copy(self, run_id: str) -> Document:
        """Read one run."""
        return self.records.get(f"run/{need(ID, run_id, 'the run id')}", "run")

    def source(self, run_id: str) -> str:
        """Return the source saved when one run started."""
        return self.records.get(f"source/{need(ID, run_id, 'the run id')}", "workflow source")["text"]

    def create(
        self,
        workflow: str,
        source: str,
        about: Document,
        given: Document,
        session: str | None = None,
        *,
        trigger: Document | None = None,
        mark: str | None = None,
    ) -> Document | None:
        """Record a run and its source. If given, write its trigger mark in the same step."""
        if session is not None:
            need(SESSION, session, "the session id")
        with self.lock:
            if session and any(doc["session_id"] == session and doc["status"] in OPEN for doc in self.listing()):
                raise Refused(409, "another run uses the session")
            while True:
                run_id = new_id()
                doc = {
                    "id": run_id,
                    "workflow": workflow,
                    "title": about["title"],
                    "input": given,
                    "status": "queued",
                    "created": time.time(),
                    "started": None,
                    "finished": None,
                    "session_id": session,
                    "own": session is None,
                    "result": None,
                    "error": None,
                    "starts": 0,
                    "activities": [],
                    "notes": [],
                    "waiting": None,
                    "base": None,
                    "decisions": {},
                    "retry": False,
                    "sessions": [],
                    "trigger": trigger,
                }
                key = f"run/{run_id}"
                puts = {key: pack(doc), f"source/{run_id}": pack({"text": source})}
                conditions: list[Condition] = [Absent(key)]
                if mark is not None:
                    puts[mark] = pack({"run": run_id, "at": doc["created"]})
                    conditions.append(Absent(mark))
                if self.records.write(puts, conditions):
                    break
                if mark is not None and self.records.table.get(mark) is not None:
                    return None
            if mark is None:
                self.fill()
            return doc

    def fill(self) -> None:
        """Close runs whose process stopped and start live runs while a place is free."""
        with self.lock:
            if self.stopped:
                return
            for run_id, process in list(self.processes.items()):
                code = process.poll()
                if code is not None:
                    del self.processes[run_id]
                    self.fail_live(run_id, f"the run process stopped with exit code {code}", expected="running")
            for doc in reversed(self.listing()):
                if len(self.processes) >= settings.PARALLEL:
                    break
                if doc["status"] not in LIVE or doc["id"] in self.processes:
                    continue
                run_id = doc["id"]
                if not doc["starts"] and doc["session_id"] and self.busy and self.busy(doc["session_id"]):
                    continue
                self.begin(run_id)

    def begin(self, run_id: str) -> None:
        """Start one queued or recovered run, if no other tick started it first."""
        key = f"run/{run_id}"
        item = self.records.table.get(key)
        if item is None:
            return
        doc, version = unpack(item), int(item["version"])
        if doc["status"] != "queued" and (doc["status"] != "running" or self.recover.get(run_id) != doc["starts"]):
            return
        event = None
        if doc["starts"] >= settings.STARTS:
            event = finish(doc, "failed", error=f"the run reached its limit of {settings.STARTS} starts")
        else:
            doc["starts"] += 1
            doc["status"] = "running"
        if not self.records.write({key: pack(doc, version + 1)}, [Equals(key, "version", version)], event):
            return
        self.recover.pop(run_id, None)
        if doc["status"] == "running":
            try:
                self.processes[run_id] = self.starter.start(run_id)
            except OSError:
                self.fail_live(run_id, "the factory could not start the run process", expected="running")

    def fail_live(self, run_id: str, error: str, *, expected: str) -> None:
        """Fail a run only if it still has the expected status."""

        def change(doc: Document) -> tuple[str, Document] | None:
            """End a process that did not send its last event."""
            return finish(doc, "failed", error=error) if doc["status"] == expected else None

        self.records.update(f"run/{run_id}", "run", change)

    def event(self, run_id: str, event: Document) -> None:
        """Record what a run process reports."""
        seq = event.get("seq")
        if seq is not None and (type(seq) is not int or not 0 <= seq < settings.STEPS):
            raise Refused(400, "seq is not the number of an activity")

        def change(doc: Document) -> tuple[str, Document] | None:
            """Apply a process event to a live run."""
            if doc["status"] not in LIVE:
                raise Refused(409, f"the run is {doc['status']}")
            return record_event(doc, event)

        self.records.update(f"run/{need(ID, run_id, 'the run id')}", "run", change)

    def decide(self, run_id: str, key: str, approved: bool, comment: str) -> Document:
        """Keep a gate decision and put the run back in the queue."""
        need(NAME, key, "the gate key")

        def change(doc: Document) -> None:
            """Take the decision only at the gate where the run waits."""
            if doc["status"] != "waiting" or doc["waiting"]["key"] != key:
                raise Refused(409, "the run does not wait at this gate")
            shown = {name: doc["waiting"].get(name) for name in ("document", "version", "size")}
            doc["decisions"][key] = {"approved": approved, "comment": comment, "at": time.time(), **shown}
            doc.update(status="queued", waiting=None)
            for step in doc["activities"]:
                if step["status"] == "waiting":
                    step["status"] = "running"

        return self.requeue(run_id, change)

    def retry(self, run_id: str) -> Document:
        """Start a failed run again. Its new process forks before the failure."""

        def change(doc: Document) -> None:
            """Mark a failed run for retry."""
            if doc["status"] != "failed":
                raise Refused(409, f"the run is {doc['status']}")
            doc.update(status="queued", finished=None, error=None, trace=None, retry=True, starts=0)

        return self.requeue(run_id, change)

    def requeue(self, run_id: str, change: Callable[[Document], None]) -> Document:
        """Apply a change and then fill the run queue."""
        with self.lock:
            doc = self.records.update(f"run/{need(ID, run_id, 'the run id')}", "run", change)
            self.fill()
            return doc

    def cancel(self, run_id: str) -> Document:
        """Cancel an open run and stop its process."""

        def change(doc: Document) -> None:
            """End a run only if it is still open."""
            if doc["status"] in OPEN:
                finish(doc, "cancelled")

        with self.lock:
            doc = self.records.update(f"run/{need(ID, run_id, 'the run id')}", "run", change)
            process = self.processes.get(run_id)
            if doc["status"] == "cancelled" and process and process.poll() is None:
                process.kill()
            return doc

    def live(self) -> int:
        """Count the runs that can have a process."""
        return sum(doc["status"] in LIVE for doc in self.listing())

    def stop(self) -> None:
        """Stop the child processes. A new factory can start the runs again."""
        with self.lock:
            self.stopped = True
            for process in self.processes.values():
                if process.poll() is None:
                    process.terminate()
            self.processes.clear()


def finish(doc: Document, status: str, **fields: Any) -> tuple[str, Document] | None:
    """End a run and return its event, if it has one."""
    now = time.time()
    doc.update(status=status, finished=now, waiting=None, retry=False, **fields)
    for step in doc["activities"]:
        if step["status"] in ("running", "waiting"):
            step.update(status="cancelled" if status == "cancelled" else "failed", finished=now)
    if status in ("completed", "failed"):
        return f"run.{status}", {"run": doc["id"], "workflow": doc["workflow"]}
    return None


def record_event(doc: Document, event: Document) -> tuple[str, Document] | None:
    """Apply one valid process event and return any trigger event."""
    kind, now, seq = event.get("type"), time.time(), event.get("seq")
    step = next((item for item in doc["activities"] if item["seq"] == seq), None)
    if kind == "started":
        session = need(SESSION, event.get("session_id"), "the session id")
        previous = doc["session_id"]
        doc.update(status="running", session_id=session, started=doc["started"] or now)
        if previous and previous != session:
            doc["sessions"].append(previous)
        if (doc["base"] is None or previous != session) and type(event.get("base")) is int:
            doc["base"] = event["base"]
    elif kind == "forked":
        session = need(SESSION, event.get("session_id"), "the session id")
        if doc["session_id"] != session and doc["session_id"]:
            doc["sessions"].append(doc["session_id"])
        doc.update(session_id=session, retry=False)
    elif kind == "activity_started" and seq is not None and isinstance(event.get("name"), str):
        name = event["name"][:80]
        fresh = {"seq": seq, "name": name, "title": str(event.get("title") or name)[:200], "status": "running"}
        fresh.update(started=now, finished=None, result=None, error=None, files=[], notes=[])
        if step and step["name"] == name:
            fresh.update(started=step["started"], files=step["files"], notes=step["notes"])
        doc["activities"] = [item for item in doc["activities"] if item["seq"] != seq] + [fresh]
        doc["activities"].sort(key=lambda item: item["seq"])
    elif kind == "activity_completed" and step:
        step.update(status="completed", finished=now, result=event.get("result"))
    elif kind == "activity_failed" and step:
        step.update(status="failed", finished=now, error=event.get("error"))
    elif kind == "attached":
        name = need(FILE, event.get("name"), "the document name")
        if step and name not in step["files"]:
            step["files"].append(name)
    elif kind == "note":
        notes, line = step["notes"] if step else doc["notes"], str(event.get("text", ""))[:2000]
        if all(note["text"] != line for note in notes):
            notes.append({"at": now, "text": line})
    elif kind == "waiting":
        gate = {"seq": seq, "key": need(NAME, event.get("key"), "the gate key"), "title": str(event.get("title", ""))[:200], "about": str(event.get("about", ""))[:2000]}
        document = event.get("document")
        shown = {key: event.get(key) if type(event.get(key)) is int else None for key in ("version", "size")}
        doc.update(status="waiting", waiting={**gate, "document": need(FILE, document, "the document name") if document else None, **shown})
        if step:
            step["status"] = "waiting"
    elif kind == "completed":
        return finish(doc, "completed", result=event.get("result"))
    elif kind == "failed":
        return finish(doc, "failed", error=event.get("error"), trace=event.get("trace"))
    else:
        raise Refused(400, "the event is not valid")
    return None
