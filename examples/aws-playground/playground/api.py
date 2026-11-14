"""The playground API: workflows, runs, artifacts, temporary files and publishing.

The browser reaches it through the front function and the relay. The workflow runs
and the Vis extension reach it through the broker. S3 holds all its data:

    workflows/<name>.py          the source of a workflow
    runs/index.json              one line for each run
    runs/<id>/run.json           the run and its activities
    runs/<id>/workflow.py        the source that the run uses, also after an edit
    runs/<id>/results/<n>.json   the result of activity n, for a new start of the run
    artifacts/<session>/<name>   the documents of a session
    tmp/<session>/<path>         the files that a session wrote, kept for some days
    outbox/<id>.bundle           commits that wait for the repo function
"""

from __future__ import annotations

import ast
import base64
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

from playground import aws, sync
from playground.env import EFS, ENV, MODELS, REPO_FUNCTION, log, private

NAME = re.compile(r"[a-z][a-z0-9_]{1,39}")
SESSION = re.compile(r"[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}")
RUN = re.compile(r"[0-9a-f]{32}")
FILE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._ -]{0,79}")
JSON = [["content-type", "application/json"], ["cache-control", "no-store"]]
SAMPLES = "/opt/playground/workflows"
# The runs that work at the same time. The other runs wait.
PARALLEL = 3
LIVE = ("queued", "running")
Answer = tuple[int, list[list[str]], bytes]


class Refused(Exception):
    """A call that the API does not accept."""

    def __init__(self, status: int, message: str) -> None:
        super().__init__(message)
        self.status = status


def need(pattern: re.Pattern[str], value: Any, what: str) -> str:
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


class Runs:
    """The workflow runs. Only the worker that holds the lease writes them."""

    def __init__(self, release: str, environment: Callable[[], dict[str, str]]) -> None:
        self.release, self.environment = release, environment
        self.lock = threading.RLock()
        self.docs: dict[str, dict[str, Any]] = {}
        self.processes: dict[str, subprocess.Popen[bytes]] = {}
        raw, _ = aws.get("runs/index.json")
        self.index: list[dict[str, Any]] = json.loads(raw) if raw else []

    def doc(self, run_id: str) -> dict[str, Any]:
        if run_id not in self.docs:
            raw, _ = aws.get(f"runs/{run_id}/run.json")
            if raw is None:
                raise Refused(404, "no such run")
            self.docs[run_id] = json.loads(raw)
        return self.docs[run_id]

    def write(self, doc: dict[str, Any]) -> None:
        """Save a run and its line in the index."""
        aws.put(f"runs/{doc['id']}/run.json", json.dumps(doc).encode())
        line = {key: doc.get(key) for key in ("id", "workflow", "title", "status", "created", "finished", "session_id")}
        line["activities"] = [[a["title"], a["status"]] for a in doc["activities"]]
        self.index = [line] + [old for old in self.index if old["id"] != doc["id"]]
        aws.put("runs/index.json", json.dumps(self.index[:300]).encode())

    def create(self, workflow: str, source: str, about: dict[str, Any], given: dict[str, Any]) -> dict[str, Any]:
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
            "session_id": None,
            "result": None,
            "error": None,
            "starts": 0,
            # Each run works in its own clone of a release, so two runs do not collide.
            "root": None,
            "activities": [],
            "notes": [],
        }
        with self.lock:
            aws.put(f"runs/{doc['id']}/workflow.py", source.encode())
            self.docs[doc["id"]] = doc
            self.write(doc)
            self.fill()
        return doc

    def fill(self) -> None:
        """Start waiting runs while a place is free, and close the runs whose process stopped."""
        with self.lock:
            for run_id, process in list(self.processes.items()):
                if process.poll() is not None:
                    del self.processes[run_id]
                    doc = self.doc(run_id)
                    if doc["status"] in LIVE:
                        self.finish(doc, "failed", error=f"the run process stopped with exit code {process.returncode}")
            waiting = [line["id"] for line in reversed(self.index) if line["status"] in LIVE and line["id"] not in self.processes]
            for run_id in waiting[: max(0, PARALLEL - len(self.processes))]:
                doc = self.doc(run_id)
                doc["starts"] += 1
                env = {**self.environment(), "PYTHONPATH": ENV.get("PYTHONPATH", ""), "PYTHONUNBUFFERED": "1"}
                # A new run starts from the newest release, also when the worker is older.
                release = sync.current() or self.release
                doc["root"] = doc.get("root") or sync.clone(release, f"{release[:12]}-{run_id[:8]}", env)
                self.write(doc)
                self.processes[run_id] = subprocess.Popen([sys.executable, "-m", "playground.run", run_id], cwd=doc["root"], env=env)
                log("run started", run_id, doc["workflow"], "start", doc["starts"])

    def finish(self, doc: dict[str, Any], status: str, **fields: Any) -> None:
        doc.update(status=status, finished=time.time(), **fields)
        for activity in doc["activities"]:
            if activity["status"] == "running":
                activity.update(status="cancelled" if status == "cancelled" else "failed", finished=time.time())
        self.write(doc)

    def event(self, run_id: str, event: dict[str, Any]) -> None:
        """Record what a run process reports."""
        kind, now = event.get("type"), time.time()
        with self.lock:
            doc = self.doc(run_id)
            if doc["status"] not in LIVE:
                raise Refused(409, f"the run is {doc['status']}")
            steps = {step["seq"]: step for step in doc["activities"]}
            step = steps.get(event.get("seq"))
            if kind == "started":
                doc.update(status="running", session_id=event["session_id"], started=doc["started"] or now)
            elif kind == "activity_started":
                fresh = {"seq": event["seq"], "name": event["name"], "title": event["title"], "status": "running"}
                fresh.update(started=now, finished=None, result=None, error=None, artifacts=[], notes=[])
                doc["activities"] = [s for s in doc["activities"] if s["seq"] != event["seq"]] + [fresh]
            elif kind == "activity_completed" and step:
                record = {"name": event["name"], "result": event["result"]}
                aws.put(f"runs/{run_id}/results/{event['seq']}.json", json.dumps(record).encode())
                step.update(status="completed", finished=now, result=event["result"])
            elif kind == "activity_failed" and step:
                step.update(status="failed", finished=now, error=event.get("error"))
            elif kind == "note":
                (step["notes"] if step else doc["notes"]).append({"at": now, "text": event.get("text", "")})
            elif kind == "completed":
                return self.finish(doc, "completed", result=event.get("result"))
            elif kind == "failed":
                return self.finish(doc, "failed", error=event.get("error"), trace=event.get("trace"))
            else:
                raise Refused(400, "unknown event")
            self.write(doc)

    def attach(self, session: str, name: str) -> None:
        """Show an artifact in the running activity of the run of a session."""
        with self.lock:
            line = next((row for row in self.index if row["session_id"] == session and row["status"] == "running"), None)
            if not line:
                return
            doc = self.doc(line["id"])
            step = next((s for s in doc["activities"] if s["status"] == "running"), None)
            if step and name not in step["artifacts"]:
                step["artifacts"].append(name)
                self.write(doc)

    def cancel(self, run_id: str) -> dict[str, Any]:
        with self.lock:
            doc = self.doc(run_id)
            process = self.processes.pop(run_id, None)
            if process and process.poll() is None:
                process.kill()
            if doc["status"] in LIVE:
                self.finish(doc, "cancelled")
            return doc

    def live(self) -> int:
        return sum(1 for line in self.index if line["status"] in LIVE)

    def stop(self) -> None:
        """Stop the run processes. The runs stay live, so the next worker starts them again."""
        with self.lock:
            for process in self.processes.values():
                process.terminate()
            self.processes.clear()


class Api:
    def __init__(self, project: str, release: str, environment: Callable[[], dict[str, str]]) -> None:
        self.project, self.release, self.environment = project, release, environment
        self.started = time.time()
        self.runs = Runs(release, environment)
        self.cancel_turn: Callable[[str], None] = lambda session: None
        self.routes: list[tuple[str, re.Pattern[str], Callable[..., Any]]] = [
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
            ("GET", r"artifacts", self.artifacts),
            ("POST", r"artifacts", self.keep),
            ("GET", r"artifacts/([\w-]+)/(.+)", self.artifact),
            ("GET", r"tmp", self.temporary),
            ("GET", r"tmp/([\w-]+)/(.+)", self.temporary_file),
            ("POST", r"tmp", self.sync),
            ("POST", r"publish", self.publish),
        ]
        self.routes = [(method, re.compile(pattern), function) for method, pattern, function in self.routes]
        if not aws.keys("workflows/"):
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
            return refusal.status, JSON, json.dumps({"error": str(refusal)}).encode()
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
        if not isinstance(given, dict):
            raise Refused(400, "input must be an object")
        source = self.source(name)
        return self.runs.create(name, source, describe(name, source), given)

    def show(self, run_id: str, **_: Any) -> dict[str, Any]:
        doc = self.runs.doc(need(RUN, run_id, "the run id"))
        raw, _ = aws.get(f"runs/{run_id}/workflow.py")
        files = self.files("artifacts", doc["session_id"]) if doc["session_id"] else []
        return {**doc, "source": (raw or b"").decode(), "artifacts": files}

    def cancel(self, run_id: str, **_: Any) -> dict[str, Any]:
        doc = self.runs.cancel(need(RUN, run_id, "the run id"))
        if doc["session_id"]:
            self.cancel_turn(doc["session_id"])
        return doc

    def event(self, run_id: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        self.runs.event(need(RUN, run_id, "the run id"), payload)
        return {"recorded": True}

    def result(self, run_id: str, seq: str, **_: Any) -> Any:
        raw, _ = aws.get(f"runs/{need(RUN, run_id, 'the run id')}/results/{int(seq)}.json")
        return json.loads(raw) if raw else None

    # -- artifacts and temporary files -----------------------------------------

    def files(self, kind: str, session: str) -> list[dict[str, Any]]:
        prefix = f"{kind}/{need(SESSION, session, 'the session id')}/"
        return [
            {"name": item["Key"][len(prefix) :], "size": item["Size"], "at": item["LastModified"].timestamp()}
            for item in aws.keys(prefix)
        ]

    def artifacts(self, query: dict[str, str], **_: Any) -> dict[str, Any]:
        return {"artifacts": self.files("artifacts", query.get("session", ""))}

    def keep(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        session = need(SESSION, payload.get("session"), "the session id")
        name = need(FILE, payload.get("name"), "the artifact name")
        content = str(payload.get("content", "")).encode()
        if len(content) > 2_000_000:
            raise Refused(413, "the artifact is too large")
        aws.put(f"artifacts/{session}/{name}", content, ContentType="text/plain; charset=utf-8")
        self.runs.attach(session, name)
        return {"name": name, "session": session, "size": len(content)}

    def artifact(self, session: str, name: str, **_: Any) -> Answer:
        key = f"artifacts/{need(SESSION, session, 'the session id')}/{need(FILE, name, 'the artifact name')}"
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
        return {"synced": len(files), "removed": len(removed)}

    # -- publishing ------------------------------------------------------------

    def git(self, root: str, *arguments: str, check: bool = True) -> str:
        done = subprocess.run(["git", *arguments], cwd=root, env=self.environment(), capture_output=True, text=True, timeout=120)
        if check and done.returncode:
            raise Refused(500, f"git {arguments[0]} failed: {done.stderr.strip()[-300:]}")
        return done.stdout.strip()

    def publish(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Commit the work of a project directory and give it to the repo function."""
        root = os.path.realpath(str(payload.get("root") or self.project))
        if os.path.dirname(root) != os.path.realpath(f"{EFS}/work") or not os.path.isfile(f"{root}/.git/playground-base"):
            raise Refused(400, "the directory is not a project of the playground")
        with open(f"{root}/.git/playground-base", encoding="utf-8") as file:
            base = file.read().strip()
        message = " ".join(str(payload.get("message") or "Changes from the Vis playground").split())[:200]
        self.git(root, "add", "--all")
        if self.git(root, "status", "--porcelain"):
            self.git(root, "commit", "--quiet", "--message", message)
        head = self.git(root, "rev-parse", "HEAD")
        if head == base:
            raise Refused(409, "there are no new commits to publish")
        publication, bundle = uuid.uuid4().hex, private(f"{uuid.uuid4().hex}.bundle")
        self.git(root, "branch", "--force", "publish", "HEAD")
        try:
            self.git(root, "bundle", "create", bundle, "publish", f"^{base}")
            with open(bundle, "rb") as file:
                aws.put(f"outbox/{publication}.bundle", file.read())
        finally:
            if os.path.exists(bundle):
                os.remove(bundle)
        request = json.dumps({"action": "publish", "id": publication}).encode()
        answer = aws.client("lambda").invoke(FunctionName=REPO_FUNCTION, Payload=request)
        result = json.loads(answer["Payload"].read())
        if answer.get("FunctionError"):
            log("publish failed", result)
            raise Refused(502, "the repo function could not push the commits")
        if result.get("refused"):
            raise Refused(422, result["refused"])
        return {**result, "commits": int(self.git(root, "rev-list", "--count", f"{base}..{head}"))}
