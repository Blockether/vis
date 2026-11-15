"""The Durable Lambda API: workflows, runs, attached files, temporary files and publishing.

The browser reaches it through the front function and the relay. The workflow runs
and the Vis extension reach it through the broker. S3 holds all its data:

    workflows/<name>.py          the source of a workflow
    runs/index.json              one line for each run
    runs/<id>/run.json           the run and its activities
    runs/<id>/workflow.py        the source that the run uses, also after an edit
    runs/<id>/results/<n>.json   the result of activity n, for a new start of the run
    runs/<id>/inputs/<key>.json  a file that a person gave to a waiting run
    attached/<session>/<name>    a copy of each file that a session attached
    tmp/<session>/<path>         the files that a session wrote, kept for 7 days
    work/<name>/                 the copy of the work directory of a session (see `workspaces.py`)
    outbox/<id>.bundle           commits that wait for the repo function
"""

from __future__ import annotations

import ast
import base64
import copy
import glob
import json
import os
import re
import subprocess
import sys
import threading
import time
import urllib.parse
import uuid
from collections.abc import Callable
from typing import Any

from blockether.vis.engine import GatewayError, Session, TransportError

from durable import aws, sync, wire
from durable.env import ENV, LIFETIME, MODELS, REPO_FUNCTION, log, private
from durable.gateway import Gateway
from durable.workspaces import FAILURES, Conflict, Workspaces, name_of

NAME = re.compile(r"[a-z][a-z0-9_]{1,39}")
SESSION = re.compile(r"[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}")
RUN = re.compile(r"[0-9a-f]{32}")
FILE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._ -]{0,79}")
JSON = [*wire.JSON, ["cache-control", "no-store"]]
SAMPLES = "/opt/durable/workflows"
# The runs that work at the same time. The other runs wait.
PARALLEL = 3
# The time that one run can work. A worker handoff starts a new process, and one
# process works for at most the lifetime of a worker.
RUN_LIMIT_S = int(ENV.get("VIS_RUN_LIMIT_S", "28800"))
STARTS = -(-RUN_LIMIT_S // LIFETIME)
# The activities that one run can have.
STEPS = 1000
LIVE = ("queued", "running")
# A waiting run has no process: it needs a file from a person.
OPEN = (*LIVE, "waiting")
# The largest file that a person gives to a run, and the largest attached file that S3 gets.
INPUT_LIMIT, ATTACHED_LIMIT = 300_000, 2_000_000
Answer = tuple[int, list[list[str]], bytes]


class Refused(Exception):
    """A call that the API does not accept."""

    def __init__(self, status: int, message: str, **detail: Any) -> None:
        super().__init__(message)
        self.status, self.detail = status, detail


def need(pattern: re.Pattern[str], value: Any, what: str) -> str:
    """Return a value that has the form of a pattern, or refuse it. The value becomes a part of an S3 key."""
    if not isinstance(value, str) or not pattern.fullmatch(value):
        raise Refused(400, f"{what} is not valid")
    return value


def relative(path: Any) -> str:
    """Return a file path below a session prefix, or refuse it."""
    parts = str(path).split("/")
    if not path or len(str(path)) > 300 or str(path).startswith("/") or ".." in parts or "" in parts:
        raise Refused(400, "a path is not valid")
    return str(path)


def literal(node: ast.expr | None) -> Any:
    """Return the value of a constant in the source, or None for other code."""
    try:
        return None if node is None else ast.literal_eval(node)
    except ValueError:
        return None


def marks(function: ast.FunctionDef, name: str) -> dict[str, Any] | None:
    """Return the keyword arguments of the decorator `name` of a function."""
    for decorator in function.decorator_list:
        call = decorator if isinstance(decorator, ast.Call) else None
        target = call.func if call else decorator
        if isinstance(target, ast.Name) and target.id == name:
            return {word.arg: literal(word.value) for word in (call.keywords if call else []) if word.arg}
    return None


def describe(name: str, source: str) -> dict[str, Any]:
    """Read the title, the inputs and the activities of a workflow. The source does not run."""
    try:
        tree = ast.parse(source)
    except SyntaxError as failure:
        raise Refused(400, f"line {failure.lineno}: {failure.msg}") from None
    functions = [node for node in tree.body if isinstance(node, ast.FunctionDef)]
    entries = [(node, marks(node, "workflow")) for node in functions]
    entries = [(node, found) for node, found in entries if found is not None]
    if len(entries) != 1:
        raise Refused(400, "a workflow file needs exactly one function with @workflow")
    entry, found = entries[0]
    arguments = entry.args.args[1:]
    defaults = [None] * (len(arguments) - len(entry.args.defaults)) + list(entry.args.defaults)
    activities = [(node, marks(node, "activity")) for node in functions]
    return {
        "name": name,
        "title": found.get("title") or entry.name,
        "description": found.get("description") or ast.get_docstring(entry) or "",
        "model": found.get("model"),
        "inputs": [{"name": a.arg, "default": literal(d)} for a, d in zip(arguments, defaults, strict=True)],
        "activities": [{"name": node.name, "title": kept.get("title") or node.name} for node, kept in activities if kept is not None],
        "lines": source.count("\n") + 1,
    }


def inputs(about: dict[str, Any], text: str) -> dict[str, Any]:
    """Return the input of a run from a text: a JSON object, or the value of the first input."""
    text = text.strip()
    if not text.startswith("{"):
        return {about["inputs"][0]["name"]: text} if text and about["inputs"] else {}
    try:
        return json.loads(text)
    except ValueError:
        raise Refused(400, "the input is not a JSON object") from None


class Runs:
    """The workflow runs. Only the worker that holds the lease writes them."""

    def __init__(self, release: str, environment: Callable[[], dict[str, str]], workspaces: Workspaces) -> None:
        self.release, self.environment, self.workspaces = release, environment, workspaces
        self.lock = threading.RLock()
        self.docs: dict[str, dict[str, Any]] = {}
        self.processes: dict[str, subprocess.Popen[bytes]] = {}
        # The worker sets these after the gateway starts: the session group of the runs, and True while a turn runs in a session.
        self.group = ""
        self.busy: Callable[[str], bool] = lambda session: False
        raw, _ = aws.get("runs/index.json")
        self.index: list[dict[str, Any]] = json.loads(raw) if raw else []

    def doc(self, run_id: str) -> dict[str, Any]:
        """Return the one object of a run. Read it from S3 at its first use."""
        with self.lock:  # two callers must get the same object, or one of them loses its change
            if run_id not in self.docs:
                raw, _ = aws.get(f"runs/{run_id}/run.json")
                if raw is None:
                    raise Refused(404, "no such run")
                self.docs[run_id] = json.loads(raw)
            return self.docs[run_id]

    def copy(self, run_id: str) -> dict[str, Any]:
        """Return a run as a copy that no other thread changes."""
        with self.lock:
            return copy.deepcopy(self.doc(run_id))

    def write(self, doc: dict[str, Any]) -> None:
        """Save a run and its line in the index."""
        aws.put(f"runs/{doc['id']}/run.json", json.dumps(doc).encode())
        line = {key: doc.get(key) for key in ("id", "workflow", "title", "status", "created", "finished", "session_id")}
        line["activities"] = [[a["title"], a["status"]] for a in doc["activities"]]
        self.index = [line] + [old for old in self.index if old["id"] != doc["id"]]
        aws.put("runs/index.json", json.dumps(self.index[:300]).encode())

    def create(self, workflow: str, source: str, about: dict[str, Any], given: dict[str, Any], session: str | None = None, root: str | None = None) -> dict[str, Any]:
        """Record a new run with a copy of its source, and start it when a place is free.

        Without a session, the run makes its own session in its own directory. With a
        session, the run works in that session and in its directory.
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
            "created": time.time(),
            "started": None,
            "finished": None,
            "session_id": session,
            "result": None,
            "error": None,
            "starts": 0,
            # A run without a session works in its own clone of a release, so two runs do not collide.
            "root": root,
            "activities": [],
            "notes": [],
            # The session turn count before each request of an activity: `<seq>:<n>`.
            "asked": {},
        }
        with self.lock:
            if session and any(line["session_id"] == session and line["status"] in OPEN for line in self.index):
                raise Refused(409, "another run uses the session")
            aws.put(f"runs/{doc['id']}/workflow.py", source.encode())
            self.docs[doc["id"]] = doc
            self.write(doc)
            self.fill()
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
            ready = [line["id"] for line in reversed(self.index) if line["status"] in LIVE and line["id"] not in self.processes]
            for run_id in ready:
                if len(self.processes) >= PARALLEL:
                    break
                doc = self.doc(run_id)
                if doc["starts"] >= STARTS:
                    self.finish(doc, "failed", error=f"the run did not end in {RUN_LIMIT_S // 3600} hours of work ({STARTS} processes)")
                    continue
                # A run in the session of a person waits until the turn of that person ends.
                if not doc["starts"] and doc["session_id"] and self.busy(doc["session_id"]):
                    continue
                env = {**self.environment(), "PYTHONPATH": ENV.get("PYTHONPATH", ""), "PYTHONUNBUFFERED": "1", "DURABLE_GROUP": self.group}
                # A new run starts from the newest release, also when the worker is older.
                release = sync.current() or self.release
                try:
                    doc["root"] = doc.get("root") or sync.clone(release, f"{release[:12]}-{run_id[:8]}", env)
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
                self.processes[run_id] = subprocess.Popen([sys.executable, "-m", "durable.run", run_id], cwd=doc["root"], env=env)
                log("run started", run_id, doc["workflow"], "start", doc["starts"])

    def finish(self, doc: dict[str, Any], status: str, **fields: Any) -> None:
        """End a run, and the activity that it left running or waiting."""
        doc.update(status=status, finished=time.time(), waiting=None, **fields)
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
                self.workspaces.own(doc["root"], session)
            elif kind == "activity_started" and seq is not None and isinstance(event.get("name"), str):
                name = event["name"][:80]
                fresh = {"seq": seq, "name": name, "title": str(event.get("title") or name)[:200], "status": "running"}
                fresh.update(started=now, finished=None, result=None, error=None, files=[], notes=[])
                if step and step["name"] == name:  # the same activity continues in a new process
                    fresh.update(started=step["started"], files=step["files"], notes=step["notes"])
                doc["activities"] = [s for s in doc["activities"] if s["seq"] != seq] + [fresh]
            elif kind == "activity_completed" and step:
                record = {"name": event.get("name"), "result": event.get("result")}
                aws.put(f"runs/{run_id}/results/{seq}.json", json.dumps(record).encode())
                step.update(status="completed", finished=now, result=record["result"])
            elif kind == "activity_failed" and step:
                step.update(status="failed", finished=now, error=event.get("error"))
            elif kind == "asked" and type(event.get("turns")) is int:
                doc.setdefault("asked", {})[str(event.get("key"))[:40]] = event["turns"]
            elif kind == "note":
                (step["notes"] if step else doc["notes"]).append({"at": now, "text": str(event.get("text", ""))[:2000]})
            elif kind == "waiting":
                asked = {"seq": seq, "key": need(NAME, event.get("key"), "the input key")}
                doc.update(status="waiting", waiting={**asked, "title": str(event.get("title", ""))[:200], "accept": str(event.get("accept", ""))[:100]})
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
            line = next((row for row in self.index if row["session_id"] == session and row["status"] == "running"), None)
            if not line:
                return
            doc = self.doc(line["id"])
            step = next((s for s in doc["activities"] if s["status"] == "running"), None)
            if step and name not in step["files"]:
                step["files"].append(name)
                self.write(doc)

    def give(self, run_id: str, key: str, name: str, content: str) -> dict[str, Any]:
        """Store the file that a waiting run asked for, and put the run in the queue again."""
        with self.lock:
            doc = self.doc(run_id)
            if doc["status"] != "waiting" or doc["waiting"]["key"] != key:
                raise Refused(409, "the run does not wait for this input")
            aws.put(f"runs/{run_id}/inputs/{key}.json", json.dumps({"name": name, "content": content}).encode())
            doc.update(status="queued", waiting=None)
            for step in doc["activities"]:
                if step["status"] == "waiting":  # the activity continues when a process takes the run
                    step["status"] = "running"
            self.write(doc)
            self.fill()
            return doc

    def cancel(self, run_id: str) -> dict[str, Any]:
        with self.lock:
            doc = self.doc(run_id)
            process = self.processes.pop(run_id, None)
            if process and process.poll() is None:
                process.kill()
            if doc["status"] in OPEN:
                self.finish(doc, "cancelled")
            return doc

    def live(self) -> int:
        return sum(1 for line in self.index if line["status"] in LIVE)

    def roots(self) -> set[str]:
        """Return the directory of each run that did not end."""
        with self.lock:
            roots = [self.doc(line["id"])["root"] for line in self.index if line["status"] in OPEN]
        return {os.path.realpath(root) for root in roots if root}

    def stop(self) -> None:
        """Stop the run processes. The runs stay live, so the next worker starts them again."""
        with self.lock:
            for process in self.processes.values():
                process.terminate()
            self.processes.clear()


class Api:
    """The routes of the API. `environment` returns the variables of a process that works for a session."""

    def __init__(self, project: str, release: str, environment: Callable[[], dict[str, str]]) -> None:
        self.project, self.release, self.environment = project, release, environment
        self.started = time.time()
        self.workspaces = Workspaces(environment)
        self.runs = Runs(release, environment, self.workspaces)
        # The worker sets these after the gateway starts: the gateway, the group of the sessions of a person, and open a session with the SDK.
        self.engine: Gateway | None = None
        self.people = ""
        self.session: Callable[[str], Session] | None = None
        # The directory of each session that reported a change, and the time of the answer of the gateway.
        self.homes: dict[str, tuple[float, str]] = {}
        # The copy that S3 has of each attached file: (session, name) -> its turn and version.
        self.copied: dict[tuple[str, str], dict[str, Any]] = {}
        routes: list[tuple[str, str, Callable[..., Any]]] = [
            ("GET", r"status", self.status),
            ("GET", r"workflows", self.workflows),
            ("POST", r"workflows/check", self.check),
            ("GET", r"workflows/(\w+)", self.workflow),
            ("PUT", r"workflows/(\w+)", self.save),
            ("DELETE", r"workflows/(\w+)", self.remove),
            ("GET", r"runs", self.list_runs),
            ("POST", r"runs", self.run),
            ("GET", r"runs/(\w+)", self.show),
            ("POST", r"runs/(\w+)/cancel", self.cancel),
            ("POST", r"runs/(\w+)/events", self.event),
            ("GET", r"runs/(\w+)/results/(\d+)", self.result),
            ("GET", r"runs/(\w+)/inputs/(\w+)", self.given),
            ("POST", r"runs/(\w+)/inputs/(\w+)", self.give),
            ("GET", r"attached", self.attached),
            ("GET", r"attached/([\w-]+)/(.+)", self.attached_file),
            ("GET", r"tmp", self.temporary),
            ("GET", r"tmp/([\w-]+)/(.+)", self.temporary_file),
            ("POST", r"tmp", self.sync),
            ("GET", r"sessions/([\w-]+)/workspace", self.workspace),
            ("POST", r"sessions/([\w-]+)/resync", self.resync),
            ("POST", r"publish", self.publish),
        ]
        self.routes = [(method, re.compile(pattern), function) for method, pattern, function in routes]
        if not aws.keys("workflows/"):  # a new stack starts with the workflows of the image
            for path in sorted(glob.glob(f"{SAMPLES}/*.py")):
                with open(path, "rb") as file:
                    aws.put(f"workflows/{os.path.basename(path)}", file.read())

    def handle(self, method: str, target: str, body: bytes) -> Answer:
        """Answer one call. The answer is a status, the headers and the body."""
        url = urllib.parse.urlsplit(target)
        path = urllib.parse.unquote(url.path)[len("/play/api/") :].rstrip("/")
        query = dict(urllib.parse.parse_qsl(url.query))
        try:
            payload = json.loads(body) if body else {}
            if not isinstance(payload, dict):
                raise Refused(400, "the body must be a JSON object")
            for verb, pattern, function in self.routes:
                found = pattern.fullmatch(path)
                if verb == method and found:
                    answer = function(*found.groups(), query=query, payload=payload)
                    return answer if isinstance(answer, tuple) else (200, JSON, json.dumps(answer).encode())
            raise Refused(404, "no such path")
        except Refused as refusal:
            return refusal.status, JSON, json.dumps({"error": str(refusal), **refusal.detail}).encode()
        except ValueError:
            return 400, JSON, b'{"error":"the body is not JSON"}'

    # -- status ----------------------------------------------------------------

    def status(self, **_: Any) -> dict[str, Any]:
        return {
            "release": self.release,
            "project": self.project,
            "repository": ENV.get("VIS_REPO", ""),
            "models": [model["name"] for model in MODELS],
            "started": self.started,
            "live_runs": self.runs.live(),
        }

    # -- workflows -------------------------------------------------------------

    def source(self, name: str) -> str:
        raw, _ = aws.get(f"workflows/{need(NAME, name, 'the workflow name')}.py")
        if raw is None:
            raise Refused(404, "no such workflow")
        return raw.decode()

    def workflows(self, **_: Any) -> dict[str, Any]:
        found = []
        for item in aws.keys("workflows/"):
            name = item["Key"][len("workflows/") : -len(".py")]
            try:
                about = describe(name, self.source(name))
            except Refused as refusal:
                about = {"name": name, "title": name, "error": str(refusal), "inputs": [], "activities": []}
            found.append({**about, "updated": item["LastModified"].timestamp()})
        return {"workflows": sorted(found, key=lambda about: about["title"].lower())}

    def workflow(self, name: str, **_: Any) -> dict[str, Any]:
        source = self.source(name)
        return {**describe(name, source), "source": source}

    def check(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        return describe("draft", str(payload.get("source", "")))

    def save(self, name: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        source = str(payload.get("source", ""))
        if len(source) > 200_000:
            raise Refused(413, "the workflow is too large")
        about = describe(need(NAME, name, "the workflow name"), source)
        aws.put(f"workflows/{name}.py", source.encode())
        return about

    def remove(self, name: str, **_: Any) -> dict[str, Any]:
        aws.delete(f"workflows/{need(NAME, name, 'the workflow name')}.py")
        return {"removed": name}

    # -- runs ------------------------------------------------------------------

    def list_runs(self, **_: Any) -> dict[str, Any]:
        return {"runs": self.runs.index[:100]}

    def run(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        name, given = payload.get("workflow"), payload.get("input") or {}
        source = self.source(name)
        about = describe(name, source)
        if isinstance(given, str):
            given = inputs(about, given)
        if not isinstance(given, dict):
            raise Refused(400, "input must be an object or a text")
        session = payload.get("session")
        if not session:
            return self.runs.create(name, source, about, given)
        root = self.home(session)
        if os.path.dirname(os.path.realpath(root)) != os.path.realpath(sync.WORK):
            raise Refused(400, "the session does not work in a directory of Durable Lambda")
        return self.runs.create(name, source, about, given, session, root)

    def places(self) -> list[tuple[str, str]]:
        """Return the id and the title of each session that a run can work in: a session with a turn in a clone of the repository."""
        rows = self.engine.sessions("") if self.engine else []
        found = [row for row in rows if os.path.dirname(os.path.realpath((row.get("workspace") or {}).get("root") or "/")) == os.path.realpath(sync.WORK)]
        return [(row["id"], f"{row.get('title') or 'Untitled session'} · {row['id'][:8]}") for row in found[:50]]

    def show(self, run_id: str, **_: Any) -> dict[str, Any]:
        doc = self.runs.copy(need(RUN, run_id, "the run id"))
        raw, _ = aws.get(f"runs/{run_id}/workflow.py")
        files = self.documents(doc["session_id"]) if doc["session_id"] else []
        return {**doc, "source": (raw or b"").decode(), "files": files}

    def cancel(self, run_id: str, **_: Any) -> dict[str, Any]:
        doc = self.runs.cancel(need(RUN, run_id, "the run id"))
        if doc["session_id"] and self.engine:
            self.engine.cancel(doc["session_id"])
        return doc

    def event(self, run_id: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        self.runs.event(need(RUN, run_id, "the run id"), payload)
        return {"recorded": True}

    def result(self, run_id: str, seq: str, **_: Any) -> Any:
        raw, _ = aws.get(f"runs/{need(RUN, run_id, 'the run id')}/results/{int(seq)}.json")
        return json.loads(raw) if raw else None

    def given(self, run_id: str, key: str, **_: Any) -> Any:
        """Return the file that a person gave to a run, or null."""
        raw, _ = aws.get(f"runs/{need(RUN, run_id, 'the run id')}/inputs/{need(NAME, key, 'the input key')}.json")
        if raw is None:
            return None
        record = json.loads(raw)
        return {**record, "size": len(record["content"].encode())}

    def give(self, run_id: str, key: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        name, content = need(FILE, payload.get("name"), "the file name"), payload.get("content")
        if not isinstance(content, str) or not content.strip():
            raise Refused(400, "the file is empty or is not text")
        if len(content.encode()) > INPUT_LIMIT:
            raise Refused(413, "the file is too large")
        return self.runs.give(need(RUN, run_id, "the run id"), need(NAME, key, "the input key"), name, content)

    # -- attached files and temporary files ------------------------------------

    def files(self, kind: str, session: str) -> list[dict[str, Any]]:
        """List the files of a session below `attached/` or `tmp/`."""
        prefix = f"{kind}/{need(SESSION, session, 'the session id')}/"
        return [{"name": item["Key"][len(prefix) :], "size": item["Size"], "at": item["LastModified"].timestamp()} for item in aws.keys(prefix)]

    def stored(self, session: str, name: str) -> dict[str, Any]:
        """Return the turn and the version of the copy that S3 has of an attached file."""
        known = (session, name)
        if known not in self.copied:  # a new worker: the metadata of the object names its copy
            found = aws.metadata(f"attached/{session}/{name}") or {}
            self.copied[known] = {"turn": found.get("turn"), "version": int(found.get("version", 0))}
        return self.copied[known]

    def mirror(self, session: str) -> None:
        """Copy each new file that a session attached to `attached/<session>/`.

        Vis keeps the attached files of a session. S3 has a copy, so the pages read all
        documents from one place.
        """
        if self.session is None:
            return
        try:
            files = [file for file in self.session(session).attachments() if file.filename and FILE.fullmatch(file.filename)]
            newest = {file.filename: file for file in sorted(files, key=lambda file: file.version)}
            for name, file in newest.items():
                if self.stored(session, name)["version"] < file.version and file.size <= ATTACHED_LIMIT:
                    about = {"turn": file.turn_id, "version": file.version}
                    aws.put(f"attached/{session}/{name}", file.read(), ContentType="text/plain; charset=utf-8", Metadata={key: str(value) for key, value in about.items() if value is not None})
                    self.copied[session, name] = about
                    self.runs.attach(session, name)
        except (GatewayError, TransportError) as failure:  # the gateway does not know the session, or it stops
            log("no copy of the attached files", session, repr(failure))

    def documents(self, session: str) -> list[dict[str, Any]]:
        """List the attached files of a session, each with the turn that attached it and its version."""
        self.mirror(need(SESSION, session, "the session id"))
        return [{**file, **self.stored(session, file["name"])} for file in self.files("attached", session)]

    def attached(self, query: dict[str, str], **_: Any) -> dict[str, Any]:
        return {"files": self.documents(query.get("session", ""))}

    def attached_file(self, session: str, name: str, **_: Any) -> Answer:
        key = f"attached/{need(SESSION, session, 'the session id')}/{need(FILE, name, 'the file name')}"
        return self.download(key, name)

    def download(self, key: str, name: str) -> Answer:
        raw, _ = aws.get(key)
        if raw is None:
            raise Refused(404, "no such file")
        # The browser never runs a stored file: it is plain text and a download.
        headers = [["content-type", "text/plain; charset=utf-8"], ["cache-control", "no-store"]]
        filename = re.sub(r"[^\w.-]", "_", os.path.basename(name))
        return 200, [*headers, ["content-disposition", f'attachment; filename="{filename}"']], raw

    def temporary(self, query: dict[str, str], **_: Any) -> dict[str, Any]:
        return {"files": self.files("tmp", query.get("session", ""))}

    def temporary_file(self, session: str, path: str, **_: Any) -> Answer:
        return self.download(f"tmp/{need(SESSION, session, 'the session id')}/{relative(path)}", path)

    def sync(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Copy the files that a session wrote to `tmp/<session>/`."""
        session = need(SESSION, payload.get("session"), "the session id")
        files, removed = payload.get("files") or [], payload.get("removed") or []
        for path in [item.get("path") for item in files] + removed:
            relative(path)
        for item in files:
            aws.put(f"tmp/{session}/{item['path']}", base64.b64decode(item.get("data") or ""))
        for path in removed:
            aws.delete(f"tmp/{session}/{path}")
        # The session reports after each step that can change a file, so its work directory gets a new copy.
        self.touch(session, is_fresh=False)
        return {"synced": len(files), "removed": len(removed)}

    # -- the work directory of a session ---------------------------------------

    def home(self, session: Any, is_fresh: bool = True) -> str:
        """Return the work directory of a session, or refuse a session that the gateway does not have."""
        need(SESSION, session, "the session id")
        at, root = self.homes.get(session, (0.0, ""))
        if is_fresh or time.time() - at > 60:
            row = self.engine.soul(session) if self.engine else None
            root = ((row or {}).get("workspace") or {}).get("root") or ""
            self.homes[session] = (time.time(), root)
        if not root:
            raise Refused(404, "no such session")
        return root

    def touch(self, session: str, is_fresh: bool = True) -> None:
        """Make the work directory of a session ready, and record that the session changes it."""
        try:
            root = self.home(session, is_fresh)
            if not self.workspaces.ensure(root):
                log("the session has no work directory", session, root)
            self.workspaces.mark(root)
        except (Refused, *FAILURES) as failure:
            log("no look at the work directory", session, repr(failure))

    def place(self, body: bytes) -> tuple[bytes, str | None]:
        """Give a new session its own work directory. Return the body for the gateway, and the directory.

        Each session of a person works in its own clone of the current release, so two
        people do not change the same files. The session joins the group of these
        sessions when the call names no group: the group keeps it in the one project.
        """
        try:
            asked = json.loads(body) if body else {}
        except ValueError:
            return body, None
        if not isinstance(asked, dict) or asked.get("workspace_id"):
            return body, None
        root = self.workspaces.create(sync.current() or self.release)
        asked.update(root=root, group_id=asked.get("group_id") or self.people or None)
        return json.dumps(asked).encode(), root

    def placed(self, root: str, status: int, answer: bytes) -> None:
        """Keep the directory that a new session got, or remove it when no session works in it.

        A call with the key of an earlier call returns the earlier session, with its own directory.
        """
        try:
            made = json.loads(answer) if status in (200, 201) else {}
            session = made.get("id") if isinstance(made, dict) else None
            if session and os.path.realpath(self.home(session)) == os.path.realpath(root):
                return self.workspaces.own(root, session)
        except (ValueError, Refused):
            pass
        self.workspaces.remove(name_of(root) or "")

    def leaving(self, session: str) -> list[str]:
        """Return the work directory of a session that a call deletes now."""
        try:
            return [self.home(session)]
        except Refused:
            return []

    def reap(self, roots: list[str] | None) -> None:
        """Remove the work directory and the S3 copy of each session that the gateway deleted.

        `roots` has the directory of the one session of the call. Without it, the call
        could delete many sessions: then each workspace whose session is gone goes. A
        directory stays while another session or a run that did not end works in it.
        """
        if not self.engine:
            return
        used = self.engine.roots() | self.runs.roots()
        if roots is None:
            names = [name for name in self.workspaces.names() if (owner := self.workspaces.owner(name)) and self.engine.is_gone(owner)]
        else:
            names = [name for name in map(name_of, roots) if name]
        for name in names:
            if os.path.realpath(f"{sync.WORK}/{name}") not in used:
                self.workspaces.remove(name)

    def directory(self, session: str) -> str:
        """Return the work directory of a session as a directory that exists."""
        root = self.home(session)
        try:
            is_ready = self.workspaces.ensure(root) and os.path.isfile(f"{root}/.git/durable-base")
        except FAILURES as failure:
            log("no work directory", session, repr(failure))
            is_ready = False
        if not is_ready:
            raise Refused(409, "the session has no work directory of Durable Lambda")
        return root

    def workspace(self, session: str, **_: Any) -> dict[str, Any]:
        """Return the release of the work directory of a session, its commits and its changed files."""
        return self.workspaces.status(self.directory(session))

    def resync(self, session: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Put the work of a session on the current release of the repository.

        A person cannot do this while a turn or a run changes the files. The tool of the
        extension can: it is a step of that turn.
        """
        root = self.directory(session)
        if not name_of(root):
            raise Refused(400, "the project directory takes each release when a worker starts")
        if not payload.get("is_own_turn") and ((self.engine and self.engine.busy(session)) or os.path.realpath(root) in self.runs.roots()):
            raise Refused(409, "a turn or a run works in the session: try again when it ends")
        try:
            return self.workspaces.resync(root)
        except Conflict as conflict:
            raise Refused(409, "the work does not apply to the new release: the directory stays as it was", files=conflict.files) from None
        except FAILURES as failure:
            log("resync failed", session, repr(failure))
            raise Refused(500, "Git could not put the work on the new release") from None

    # -- publishing ------------------------------------------------------------

    def git(self, root: str, *arguments: str, check: bool = True) -> str:
        """Run Git in a project directory, with the environment of a session. Return its output."""
        done = subprocess.run(["git", *arguments], cwd=root, env=self.environment(), capture_output=True, text=True, timeout=120)
        if check and done.returncode:
            raise Refused(500, f"git {arguments[0]} failed: {done.stderr.strip()[-300:]}")
        return done.stdout.strip()

    def publish(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Commit the work of a project directory and give it to the repo function."""
        root = os.path.realpath(str(payload.get("root") or self.project))
        if os.path.dirname(root) != os.path.realpath(sync.WORK) or not os.path.isfile(f"{root}/.git/durable-base"):
            raise Refused(400, "the directory is not a project of Durable Lambda")
        self.workspaces.mark(root)
        with open(f"{root}/.git/durable-base", encoding="utf-8") as file:
            base = file.read().strip()
        message = " ".join(str(payload.get("message") or "Changes from Durable Lambda").split())[:200]
        self.git(root, "add", "--all")
        if self.git(root, "status", "--porcelain"):
            self.git(root, "commit", "--quiet", "--message", message)
        head = self.git(root, "rev-parse", "HEAD")
        if head == base:
            raise Refused(409, "there are no new commits to publish")
        publication = uuid.uuid4().hex
        bundle = private(f"{publication}.bundle")
        self.git(root, "branch", "--force", "publish", "HEAD")
        try:
            self.git(root, "bundle", "create", bundle, "publish", f"^{base}")
            with open(bundle, "rb") as file:
                aws.put(f"outbox/{publication}.bundle", file.read())
        finally:
            if os.path.exists(bundle):
                os.remove(bundle)
        raw, failed = aws.invoke(REPO_FUNCTION, action="publish", id=publication)
        result = json.loads(raw)
        if failed:
            log("publish failed", result)
            raise Refused(502, "the repo function could not push the commits")
        if result.get("refused"):
            raise Refused(422, result["refused"])
        return {**result, "commits": int(self.git(root, "rev-list", "--count", f"{base}..{head}"))}
