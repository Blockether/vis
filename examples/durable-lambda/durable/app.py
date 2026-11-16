"""The Software factory API: workflows, runs, attached files, temporary files and publishing.

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

from durable import aws, shards, wire
from durable.domain import LIVE, NAME, RUN, Answer, Elsewhere, Refused, need
from durable.settings import COMMENT_LIMIT, MAIN, MODELS, REPO, RUNS_LISTED, WORK
from durable.vis.files import Files
from durable.vis.sessions import Sessions
from durable.workflows import source
from durable.workflows.runs import Runs
from durable.worktree import Workspaces


class Api:
    """The routes of the API. `environment` returns the variables of a process that works for a session."""

    def __init__(self, project: str, release: str, environment: Callable[[], dict[str, str]], shard: str = MAIN) -> None:
        """Make the owner of each kind of data and the table of the routes. `project` is the project directory, `release` is its commit and `shard` is the shard of this worker."""
        self.project, self.release, self.shard = project, release, shard
        self.started = time.time()
        self.workspaces = Workspaces(environment)
        self.runs = Runs(release, environment, self.workspaces, shard)
        self.sessions = Sessions(release, self.workspaces, self.runs)
        self.files = Files(self.runs.attach, self.sessions.touch)
        # The method, the path below `/dashboard/api/` and the function of each route. A group of the path is an argument.
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
            ("POST", r"runs/(\w+)/retry", self.retry),
            ("POST", r"runs/(\w+)/events", self.event),
            ("POST", r"runs/(\w+)/gates/(\w+)", self.decide),
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

    def handle(self, method: str, target: str, body: bytes) -> Answer:
        """Answer one call. The answer is a status, the headers and the body."""
        url = urllib.parse.urlsplit(target)
        path = urllib.parse.unquote(url.path)[len("/dashboard/api/") :].rstrip("/")
        query = dict(urllib.parse.parse_qsl(url.query))
        try:
            payload = json.loads(body) if body else {}
            if not isinstance(payload, dict):
                raise Refused(400, "the body must be a JSON object")
            for verb, pattern, function in self.routes:
                found = pattern.fullmatch(path)
                if verb == method and found:
                    answer = function(*found.groups(), query=query, payload=payload)
                    return answer if isinstance(answer, tuple) else (200, wire.JSON, json.dumps(answer).encode())
            raise Refused(404, "no such path")
        except Elsewhere as other:  # the worker of the shard that owns the run or the session answers
            return shards.ask(other.shard, method, target, body)
        except Refused as refusal:
            return refusal.status, wire.JSON, json.dumps({"error": str(refusal), **refusal.detail}).encode()
        except ValueError:
            return 400, wire.JSON, b'{"error":"the body is not JSON"}'

    def ask(self, method: str, path: str, payload: dict[str, Any]) -> dict[str, Any]:
        """Call a route for a caller in this process, and return its answer or raise its refusal. The run can be in another shard."""
        status, _, data = self.handle(method, f"/dashboard/api/{path}", json.dumps(payload).encode())
        answer = json.loads(data)
        if status >= 400:
            raise Refused(status, str(answer.get("error") or "the call failed"))
        return answer

    # -- status ----------------------------------------------------------------

    def status(self, **_: Any) -> dict[str, Any]:
        """Return the release, the repository and the models of the stack, and the number of live runs."""
        return {
            "release": self.release,
            "project": self.project,
            "repository": REPO,
            "models": [model["name"] for model in MODELS],
            "started": self.started,
            "live_runs": sum(1 for line in self.runs.listing() if line["status"] in LIVE),
        }

    # -- workflows -------------------------------------------------------------

    def workflows(self, **_: Any) -> dict[str, Any]:
        """List the workflows."""
        return {"workflows": source.listing()}

    def workflow(self, name: str, **_: Any) -> dict[str, Any]:
        """Return one workflow: what it says about itself and its source."""
        text = source.read(name)
        return {**source.describe(name, text), "source": text}

    def check(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Describe a source that is not saved, or refuse a source that is not a workflow."""
        return source.describe("draft", str(payload.get("source", "")))

    def save(self, name: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Store the source of a workflow."""
        base = payload.get("base")
        return source.save(name, str(payload.get("source", "")), base if isinstance(base, str) else None)

    def remove(self, name: str, **_: Any) -> dict[str, Any]:
        """Remove a workflow. Its runs keep their own copy of the source."""
        source.remove(name)
        return {"removed": name}

    # -- runs ------------------------------------------------------------------

    def list_runs(self, **_: Any) -> dict[str, Any]:
        """List the last runs, the newest first."""
        return {"runs": self.runs.listing()[:RUNS_LISTED]}

    def run(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Start a run of a workflow, in its own new session or in the session that the call names."""
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
        if shards.owner(str(session)) != self.shard:
            raise Elsewhere(shards.owner(str(session)))
        root = self.sessions.home(session)
        if os.path.dirname(os.path.realpath(root)) != os.path.realpath(WORK):
            raise Refused(400, "the session does not work in a directory of Software factory")
        return self.runs.create(name, text, about, given, session, root)

    def show(self, run_id: str, **_: Any) -> dict[str, Any]:
        """Return a run with its source and the files that its session attached."""
        doc = self.runs.copy(need(RUN, run_id, "the run id"))
        raw, _ = aws.get(f"runs/{run_id}/workflow.py")
        files = self.files.documents(doc["session_id"]) if doc["session_id"] else []
        return {**doc, "source": (raw or b"").decode(), "files": files}

    def cancel(self, run_id: str, **_: Any) -> dict[str, Any]:
        """Cancel a run and the turn that runs in its session."""
        doc = self.runs.cancel(need(RUN, run_id, "the run id"))
        if doc["session_id"]:
            self.sessions.cancel(doc["session_id"])
        return doc

    def retry(self, run_id: str, **_: Any) -> dict[str, Any]:
        """Start a failed run again. It continues in a fork of its session."""
        return self.runs.retry(need(RUN, run_id, "the run id"))

    def event(self, run_id: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Record what a run process reports."""
        self.runs.event(need(RUN, run_id, "the run id"), payload)
        return {"recorded": True}

    def decide(self, run_id: str, key: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Take the decision of a person at the gate where a run waits."""
        approved, comment = payload.get("approved"), payload.get("comment", "")
        if type(approved) is not bool or not isinstance(comment, str):
            raise Refused(400, "approved must be true or false, and comment must be a text")
        if len(comment) > COMMENT_LIMIT:
            raise Refused(413, "the comment is too long")
        return self.runs.decide(need(RUN, run_id, "the run id"), need(NAME, key, "the gate key"), approved, comment.strip())

    # -- publishing ------------------------------------------------------------

    def publish(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Commit the work of a project directory and give it to the repo function."""
        try:
            return self.workspaces.publish(str(payload.get("root") or self.project), str(payload.get("message") or ""))
        except RuntimeError as failure:  # a Git command failed
            raise Refused(500, str(failure)) from None
