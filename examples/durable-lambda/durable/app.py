"""The Durable Lambda API: workflows, runs, attached files, temporary files and publishing.

The browser reaches it through the front function and the relay. The workflow runs
and the Vis extension reach it through the broker. This module has the routes. Each
route calls the module that owns its data, and S3 holds all the data:

    workflows/   `workflows/source.py`
    runs/        `workflows/runs.py`
    attached/    `vis/files.py`
    tmp/         `vis/files.py`
    work/        `worktree.py`
    outbox/      `worktree.py`
"""

from __future__ import annotations

import json
import os
import re
import time
import urllib.parse
from collections.abc import Callable
from typing import Any

from durable import aws, wire, worktree
from durable.domain import FILE, INPUT_LIMIT, NAME, RUN, Answer, Refused, need
from durable.settings import ENV, MODELS
from durable.vis.files import Files
from durable.vis.sessions import Sessions
from durable.workflows import source
from durable.workflows.runs import Runs
from durable.worktree import Workspaces

JSON = [*wire.JSON, ["cache-control", "no-store"]]


class Api:
    """The routes of the API. `environment` returns the variables of a process that works for a session."""

    def __init__(self, project: str, release: str, environment: Callable[[], dict[str, str]]) -> None:
        self.project, self.release = project, release
        self.started = time.time()
        self.workspaces = Workspaces(environment)
        self.runs = Runs(release, environment, self.workspaces)
        self.sessions = Sessions(release, self.workspaces, self.runs)
        self.files = Files(self.runs.attach, self.sessions.touch)
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
            ("GET", r"attached", self.files.attached),
            ("GET", r"attached/([\w-]+)/(.+)", self.files.attached_file),
            ("GET", r"tmp", self.files.temporary),
            ("GET", r"tmp/([\w-]+)/(.+)", self.files.temporary_file),
            ("POST", r"tmp", self.files.sync),
            ("GET", r"sessions/([\w-]+)/workspace", self.sessions.workspace),
            ("POST", r"sessions/([\w-]+)/resync", self.sessions.resync),
            ("POST", r"publish", self.publish),
        ]
        self.routes = [(method, re.compile(pattern), function) for method, pattern, function in routes]
        source.seed()

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

    def workflows(self, **_: Any) -> dict[str, Any]:
        return {"workflows": source.listing()}

    def workflow(self, name: str, **_: Any) -> dict[str, Any]:
        text = source.read(name)
        return {**source.describe(name, text), "source": text}

    def check(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        return source.describe("draft", str(payload.get("source", "")))

    def save(self, name: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        return source.save(name, str(payload.get("source", "")))

    def remove(self, name: str, **_: Any) -> dict[str, Any]:
        source.remove(name)
        return {"removed": name}

    # -- runs ------------------------------------------------------------------

    def list_runs(self, **_: Any) -> dict[str, Any]:
        return {"runs": self.runs.index[:100]}

    def run(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        name, given = payload.get("workflow"), payload.get("input") or {}
        text = source.read(name)
        about = source.describe(name, text)
        if isinstance(given, str):
            given = source.inputs(about, given)
        if not isinstance(given, dict):
            raise Refused(400, "input must be an object or a text")
        session = payload.get("session")
        if not session:
            return self.runs.create(name, text, about, given)
        root = self.sessions.home(session)
        if os.path.dirname(os.path.realpath(root)) != os.path.realpath(worktree.WORK):
            raise Refused(400, "the session does not work in a directory of Durable Lambda")
        return self.runs.create(name, text, about, given, session, root)

    def show(self, run_id: str, **_: Any) -> dict[str, Any]:
        doc = self.runs.copy(need(RUN, run_id, "the run id"))
        raw, _ = aws.get(f"runs/{run_id}/workflow.py")
        files = self.files.documents(doc["session_id"]) if doc["session_id"] else []
        return {**doc, "source": (raw or b"").decode(), "files": files}

    def cancel(self, run_id: str, **_: Any) -> dict[str, Any]:
        doc = self.runs.cancel(need(RUN, run_id, "the run id"))
        if doc["session_id"]:
            self.sessions.cancel(doc["session_id"])
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

    # -- publishing ------------------------------------------------------------

    def publish(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Commit the work of a project directory and give it to the repo function."""
        try:
            return self.workspaces.publish(str(payload.get("root") or self.project), str(payload.get("message") or ""))
        except RuntimeError as failure:  # a Git command failed
            raise Refused(500, str(failure)) from None
