"""The workflow runs: their records in S3 and their processes.

runs/index.json              one line for each run of each shard
runs/<id>/run.json           the run, its activities and the decisions of a person at its gates
runs/<id>/workflow.py        the source that the run uses, also after an edit

The transcript of the session of a run is the record of its work: a new process of a
run reads the answers there (see `sdk.py`). The run in S3 is what the pages show.

Each worker writes only the runs of its own shard (see `shards.py`). The index is the
one object that each worker writes, so each write of it is conditional on its ETag.
"""

from __future__ import annotations

import copy
import json
import os
import subprocess
import sys
import threading
import time
import uuid
from collections.abc import Callable
from typing import Any

from durable import aws, shards, worktree
from durable.domain import FILE, LIVE, NAME, OPEN, SESSION, Elsewhere, Refused, need
from durable.helpers import log
from durable.settings import ENV, LOOK_S, MAIN, PARALLEL, RUN_LIMIT_S, RUNS_KEPT, STARTS, STEPS, WORKER_FUNCTION
from durable.worktree import FAILURES, Workspaces

# What S3 answers when another worker wrote the index first.
RACED = ("PreconditionFailed", "ConditionalRequestConflict")
# The variables of the worker that a run process reads in `settings.py`. Only a local run sets them.
PLACE = ("VIS_GATEWAY_PORT", "VIS_WORKER_DIR")


class Runs:
    """The workflow runs of one shard. Only the worker that holds the lease of the shard writes them."""

    def __init__(self, release: str, environment: Callable[[], dict[str, str]], workspaces: Workspaces, shard: str = MAIN) -> None:
        """Read the index of the runs. `environment` returns the variables of a run process."""
        self.release, self.environment, self.workspaces, self.shard = release, environment, workspaces, shard
        self.lock = threading.RLock()
        self.docs: dict[str, dict[str, Any]] = {}
        self.processes: dict[str, subprocess.Popen[bytes]] = {}
        # The worker sets these after the gateway starts: the session group of the runs, and True while a turn runs in a session.
        self.group = ""
        self.busy: Callable[[str], bool] = lambda session: False
        # The index as this worker read or wrote it last, and the time of that look.
        self.index: list[dict[str, Any]] = []
        self.looked = 0.0
        self.listing(is_fresh=True)

    def listing(self, is_fresh: bool = False) -> list[dict[str, Any]]:
        """Return the line of each run of each shard. Read the index again after LOOK_S."""
        with self.lock:  # a write of this worker must not come between the read and the assignment
            if is_fresh or time.time() - self.looked > LOOK_S:
                raw, _ = aws.get("runs/index.json")
                self.index, self.looked = (json.loads(raw) if raw else []), time.time()
            return self.index

    def mine(self) -> list[dict[str, Any]]:
        """Return the line of each run of this shard. Only this worker changes them."""
        return [line for line in self.index if (line.get("shard") or MAIN) == self.shard]

    def doc(self, run_id: str) -> dict[str, Any]:
        """Return the one object of a run of this shard. Read it from S3 at its first use."""
        with self.lock:  # two callers must get the same object, or one of them loses its change
            if run_id not in self.docs:
                raw, _ = aws.get(f"runs/{run_id}/run.json")
                if raw is None:
                    raise Refused(404, "no such run")
                doc = json.loads(raw)
                if (doc.get("shard") or MAIN) != self.shard:
                    raise Elsewhere(doc.get("shard") or MAIN)
                self.docs[run_id] = doc
            return self.docs[run_id]

    def copy(self, run_id: str) -> dict[str, Any]:
        """Return a run as a copy that no other thread changes. S3 has the run of another shard."""
        with self.lock:
            if run_id in self.docs:
                return copy.deepcopy(self.docs[run_id])
        raw, _ = aws.get(f"runs/{run_id}/run.json")
        if raw is None:
            raise Refused(404, "no such run")
        return json.loads(raw)

    def write(self, doc: dict[str, Any]) -> None:
        """Save a run and its line in the index."""
        aws.put(f"runs/{doc['id']}/run.json", json.dumps(doc).encode())
        line = {key: doc.get(key) for key in ("id", "workflow", "title", "status", "created", "finished", "session_id", "shard")}
        line["activities"] = [[a["title"], a["status"]] for a in doc["activities"]]
        while True:
            raw, etag = aws.get("runs/index.json")
            index = ([line] + [old for old in (json.loads(raw) if raw else []) if old["id"] != doc["id"]])[:RUNS_KEPT]
            try:
                aws.put("runs/index.json", json.dumps(index).encode(), **({"IfMatch": etag} if etag else {"IfNoneMatch": "*"}))
            except aws.ClientError as failure:
                if failure.response["Error"]["Code"] not in RACED:
                    raise
                continue  # the worker of another shard wrote the index first
            self.index, self.looked = index, time.time()
            return

    def create(self, workflow: str, source: str, about: dict[str, Any], given: dict[str, Any], session: str | None = None, root: str | None = None) -> dict[str, Any]:
        """Record a new run with a copy of its source, and start it when a place is free.

        Without a session, the run makes its own session in its own directory, and the
        worker of a new shard works on it. With a session, the run works in that
        session and in its directory: this worker owns the session.
        """
        known = {item["name"] for item in about["inputs"]}
        unknown = set(given) - known
        if unknown:
            raise Refused(400, f"the workflow has no input {sorted(unknown)[0]!r}")
        doc = {
            "id": uuid.uuid4().hex,
            "workflow": workflow,
            "title": about["title"],
            "input": given,
            "status": "queued",
            # The shard whose worker works on the run.
            "shard": self.shard if session else shards.fresh(),
            "created": time.time(),
            "started": None,
            "finished": None,
            "session_id": session,
            # True when the run makes its own session. A new process makes that session again when the gateway lost it.
            "own": session is None,
            "result": None,
            "error": None,
            "starts": 0,
            # A run without a session works in its own clone of a release, so two runs do not collide.
            "root": root,
            "activities": [],
            "notes": [],
            # The number of turns that the session had before the run. The transcript of the run starts there.
            "base": None,
            # The decision of a person at each gate, by the key of the gate. The run process sends it to the session.
            "decisions": {},
            # True while a retry waits for its fork, and the sessions that the run left with a fork.
            "retry": False,
            "sessions": [],
        }
        with self.lock:
            if session and any(line["session_id"] == session and line["status"] in OPEN for line in self.mine()):
                raise Refused(409, "another run uses the session")
            aws.put(f"runs/{doc['id']}/workflow.py", source.encode())
            # The front sends the calls for the run to its shard.
            shards.assign(doc["id"], doc["shard"])
            if doc["shard"] == self.shard:
                self.docs[doc["id"]] = doc
                self.write(doc)
                self.fill()
                return doc
            self.write(doc)
        # The mark comes first: the schedule starts the worker when this start does not.
        shards.mark(doc["shard"])
        aws.start(WORKER_FUNCTION, kind="run", shard=doc["shard"])
        return doc

    def fill(self) -> None:
        """Start queued runs while a place is free, and close the runs whose process stopped."""
        with self.lock:
            for run_id, process in list(self.processes.items()):
                if process.poll() is not None:
                    del self.processes[run_id]
                    doc = self.doc(run_id)
                    self.workspaces.mark(doc["root"])
                    if doc["status"] in LIVE:
                        self.finish(doc, "failed", error=f"the run process stopped with exit code {process.returncode}")
            ready = [line["id"] for line in reversed(self.mine()) if line["status"] in LIVE and line["id"] not in self.processes]
            for run_id in ready:
                if len(self.processes) >= PARALLEL:
                    break
                doc = self.doc(run_id)
                if doc["status"] not in LIVE:
                    continue
                if doc["starts"] >= STARTS:
                    self.finish(doc, "failed", error=f"the run did not end in {RUN_LIMIT_S // 3600} hours of work ({STARTS} processes)")
                    continue
                # A run in the session of a person waits until the turn of that person ends.
                if not doc["starts"] and doc["session_id"] and self.busy(doc["session_id"]):
                    continue
                # `settings.py` of the run process reads the group as RUN_GROUP.
                env = {**self.environment(), "PYTHONPATH": ENV.get("PYTHONPATH", ""), "PYTHONUNBUFFERED": "1", "DURABLE_GROUP": self.group}
                env.update((name, ENV[name]) for name in PLACE if name in ENV)
                # A new run starts from the newest release, also when the worker is older.
                release = worktree.current() or self.release
                try:
                    doc["root"] = doc.get("root") or worktree.clone(release, f"{release[:12]}-{run_id[:8]}", env)
                    is_ready = self.workspaces.ensure(doc["root"])
                except FAILURES as failure:
                    log("no directory for the run", run_id, repr(failure))
                    is_ready = False
                if not is_ready:
                    self.finish(doc, "failed", error="the work directory of the run is not there, and S3 has no copy of it")
                    continue
                doc["starts"] += 1
                self.workspaces.mark(doc["root"])
                self.write(doc)
                self.processes[run_id] = subprocess.Popen([sys.executable, "-m", "durable.workflows.process", run_id], cwd=doc["root"], env=env)
                log("run started", run_id, doc["workflow"], "start", doc["starts"])

    def finish(self, doc: dict[str, Any], status: str, **fields: Any) -> None:
        """End a run, and the activity that it left running or waiting."""
        doc.update(status=status, finished=time.time(), waiting=None, retry=False, **fields)
        if doc["root"]:
            self.workspaces.mark(doc["root"])
        for activity in doc["activities"]:
            if activity["status"] in ("running", "waiting"):
                activity.update(status="cancelled" if status == "cancelled" else "failed", finished=time.time())
        self.write(doc)

    def event(self, run_id: str, event: dict[str, Any]) -> None:
        """Record what a run process reports."""
        kind, now, seq = event.get("type"), time.time(), event.get("seq")
        # The number of an activity becomes a part of an S3 key.
        if seq is not None and (type(seq) is not int or not 0 <= seq < STEPS):
            raise Refused(400, "seq is not the number of an activity")
        with self.lock:
            doc = self.doc(run_id)
            if doc["status"] not in LIVE:
                raise Refused(409, f"the run is {doc['status']}")
            step = next((step for step in doc["activities"] if step["seq"] == seq), None)
            if kind == "started":
                session = need(SESSION, event.get("session_id"), "the session id")
                doc.update(status="running", session_id=session, started=doc["started"] or now)
                if doc.get("base") is None and type(event.get("base")) is int:
                    doc["base"] = event["base"]
                # The record comes before the session owns a workspace: no other worker then removes the workspace.
                shards.assign(session, self.shard)
                self.workspaces.own(doc["root"], session)
            elif kind == "forked":  # a retry continues in a fork of the session
                session = need(SESSION, event.get("session_id"), "the session id")
                shards.assign(session, self.shard)
                doc.setdefault("sessions", []).append(doc["session_id"])
                doc.update(session_id=session, retry=False)
            elif kind == "activity_started" and seq is not None and isinstance(event.get("name"), str):
                name = event["name"][:80]
                fresh = {"seq": seq, "name": name, "title": str(event.get("title") or name)[:200], "status": "running"}
                fresh.update(started=now, finished=None, result=None, error=None, files=[], notes=[])
                if step and step["name"] == name:  # the same activity continues in a new process
                    fresh.update(started=step["started"], files=step["files"], notes=step["notes"])
                doc["activities"] = [s for s in doc["activities"] if s["seq"] != seq] + [fresh]
            elif kind == "activity_completed" and step:
                step.update(status="completed", finished=now, result=event.get("result"))
            elif kind == "activity_failed" and step:
                step.update(status="failed", finished=now, error=event.get("error"))
            elif kind == "note":
                notes, line = step["notes"] if step else doc["notes"], str(event.get("text", ""))[:2000]
                # A new process of the run makes each note of its completed work again.
                if all(note["text"] != line for note in notes):
                    notes.append({"at": now, "text": line})
            elif kind == "waiting":
                gate = {"seq": seq, "key": need(NAME, event.get("key"), "the gate key"), "title": str(event.get("title", ""))[:200], "about": str(event.get("about", ""))[:2000]}
                # The pages show this file of the session next to the gate.
                document = event.get("document")
                # The version and the size tell which document the person reads. The decision keeps them.
                shown = {key: event.get(key) if type(event.get(key)) is int else None for key in ("version", "size")}
                doc.update(status="waiting", waiting={**gate, "document": need(FILE, document, "the document name") if document else None, **shown})
                if step:
                    step["status"] = "waiting"
                # The process ends by itself now. Without it, the run uses no place.
                self.processes.pop(run_id, None)
            elif kind == "completed":
                return self.finish(doc, "completed", result=event.get("result"))
            elif kind == "failed":
                return self.finish(doc, "failed", error=event.get("error"), trace=event.get("trace"))
            else:
                raise Refused(400, "the event is not valid")
            self.write(doc)

    def attach(self, session: str, name: str) -> None:
        """Show an attached file in the running activity of the run of a session."""
        with self.lock:
            line = next((row for row in self.mine() if row["session_id"] == session and row["status"] == "running"), None)
            if not line:
                return
            doc = self.doc(line["id"])
            step = next((s for s in doc["activities"] if s["status"] == "running"), None)
            if step and name not in step["files"]:
                step["files"].append(name)
                self.write(doc)

    def decide(self, run_id: str, key: str, approved: bool, comment: str) -> dict[str, Any]:
        """Keep the decision of a person at the gate where a run waits, and put the run in the queue again.

        The next process of the run sends the decision to the session, so the transcript has it.
        The decision keeps the document that the gate showed: the run applies it only to that document.
        """
        with self.lock:
            doc = self.doc(run_id)
            if doc["status"] != "waiting" or doc["waiting"]["key"] != key:
                raise Refused(409, "the run does not wait at this gate")
            shown = {name: doc["waiting"].get(name) for name in ("document", "version", "size")}
            doc.setdefault("decisions", {})[key] = {"approved": approved, "comment": comment, "at": time.time(), **shown}
            doc.update(status="queued", waiting=None)
            for step in doc["activities"]:
                if step["status"] == "waiting":  # the activity continues when a process takes the run
                    step["status"] = "running"
            self.write(doc)
            self.fill()
            return doc

    def retry(self, run_id: str) -> dict[str, Any]:
        """Put a failed run in the queue again.

        Its next process forks the session: the fork has the transcript before the
        failed activity, and the run continues there. The session of the failed run
        stays as it is.
        """
        with self.lock:
            doc = self.doc(run_id)
            if doc["status"] != "failed":
                raise Refused(409, f"the run is {doc['status']}")
            doc.update(status="queued", finished=None, error=None, trace=None, retry=True, starts=0)
            self.write(doc)
            self.fill()
            return doc

    def cancel(self, run_id: str) -> dict[str, Any]:
        """Stop the process of a run and end the run as cancelled."""
        with self.lock:
            doc = self.doc(run_id)
            process = self.processes.pop(run_id, None)
            if process and process.poll() is None:
                process.kill()
            if doc["status"] in OPEN:
                self.finish(doc, "cancelled")
            return doc

    def live(self) -> int:
        """Return the number of runs of this shard that have, or wait for, a process."""
        return sum(1 for line in self.mine() if line["status"] in LIVE)

    def roots(self) -> set[str]:
        """Return the directory of each run of this shard that did not end."""
        with self.lock:
            roots = [self.doc(line["id"])["root"] for line in self.mine() if line["status"] in OPEN]
        return {os.path.realpath(root) for root in roots if root}

    def stop(self) -> None:
        """Stop the run processes. The runs stay live, so the next worker starts them again."""
        with self.lock:
            for process in self.processes.values():
                process.terminate()
            self.processes.clear()
