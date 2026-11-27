"""The HTTP API of the factory."""

from __future__ import annotations

import hmac
import json
import re
import time
from collections.abc import Callable, Mapping
from typing import Any
from urllib.parse import parse_qsl, unquote, urlsplit

from blockether.vis.engine import GatewayError, TransportError
from vis_optmem.shared import Table

from factory import settings
from factory.gateway import Gateway
from factory.memory import Memories
from factory.records import FILE, NAME, SESSION, Document, Records, Refused, need
from factory.runs import Runs, Starts
from factory.suggestions import Suggestions, text
from factory.triggers import Triggers
from factory.workflows import source
from factory.workflows.store import Workflows

JSON = "application/json; charset=utf-8"
Answer = tuple[int, str, bytes]


def answer(status: int, data: Any) -> Answer:
    """Encode a JSON response."""
    return status, JSON, json.dumps(data, ensure_ascii=False).encode()


class App:
    """Route factory requests and check for due work."""

    def __init__(self, table: Table, memories: Memories, gateway: Gateway, starter: Starts) -> None:
        """Keep the four parts of the factory and set its routes."""
        self.memories, self.gateway = memories, gateway
        self.records = Records(table)
        self.runs = Runs(table, starter, gateway.busy)
        self.store = Workflows(table, starter.check)
        self.triggers = Triggers(table, self.runs, self.store)
        self.suggestions = Suggestions(table, gateway.discuss)
        self.started = time.time()
        # A session of the workflow project has a turn in progress: the factory reads the files after it.
        self.editing = False
        routes: list[tuple[str, str, Callable[..., Any]]] = [
            ("GET", r"status", self.status),
            ("GET", r"models", self.models),
            ("GET", r"workflows", self.workflows),
            ("POST", r"workflows/check", self.check),
            ("GET", r"workflows/(\w+)", self.workflow),
            ("POST", r"workflows/(\w+)/restore", self.restore),
            ("POST", r"workflows/(\w+)/model", self.select),
            ("DELETE", r"workflows/(\w+)", self.remove),
            ("GET", r"runs", self.list_runs),
            ("POST", r"runs", self.run),
            ("GET", r"runs/(\w+)", self.show),
            ("POST", r"runs/(\w+)/cancel", self.cancel),
            ("POST", r"runs/(\w+)/retry", self.retry),
            ("POST", r"runs/(\w+)/events", self.event),
            ("POST", r"runs/(\w+)/gates/(\w+)", self.decide),
            ("GET", r"attached", self.attached),
            ("GET", r"attached/([\w-]+)/(.+)", self.attachment),
            ("GET", r"triggers", self.list_triggers),
            ("GET", r"events", self.events),
            ("POST", r"hooks/(\w+)", self.hook),
            ("GET", r"suggestions", self.list_suggestions),
            ("POST", r"suggestions", self.suggest),
            ("GET", r"suggestions/(\w+)", self.suggestion),
            ("POST", r"suggestions/(\w+)/(accept|decline)", self.suggestion_decide),
            ("POST", r"suggestions/(\w+)/discuss", self.discuss),
            ("POST", r"suggestions/(\w+)/comments", self.comment),
            ("GET", r"sessions", self.list_sessions),
            ("GET", r"memory", self.memory),
            ("GET", r"memory/(\w+)", self.entries),
        ]
        self.routes = [(verb, re.compile(pattern), route) for verb, pattern, route in routes]

    def handle(self, method: str, target: str, body: bytes, headers: Mapping[str, str]) -> Answer:
        """Return the status, content type and body of one factory request."""
        try:
            url = urlsplit(target)
        except ValueError:
            return answer(400, {"error": "the request path is not valid"})
        prefix = f"{settings.BASE}/api/"
        if not url.path.startswith(prefix):
            return answer(404, {"error": "no such path"})
        path = unquote(url.path[len(prefix) :]).rstrip("/")
        query = dict(parse_qsl(url.query))
        try:
            if method == "POST" and path == "memory":
                return answer(*self.memories.answer(body))
            try:
                payload = json.loads(body, parse_constant=invalid_constant) if body else None
            except (ValueError, RecursionError):
                raise Refused(400, "the body is not JSON") from None
            if not (path.startswith("hooks/") and method == "POST"):
                if body and not isinstance(payload, dict):
                    raise Refused(400, "the body must be a JSON object")
                payload = payload or {}
            for verb, pattern, route in self.routes:
                if verb == method and (found := pattern.fullmatch(path)):
                    result = route(*found.groups(), query=query, payload=payload, headers=headers)
                    return result if isinstance(result, tuple) else answer(200, result)
            raise Refused(404, "no such path")
        except Refused as failure:
            return answer(failure.status, {"error": str(failure)})
        except GatewayError as failure:
            return answer(404 if failure.status == 404 else 502, {"error": "the gateway refused the request"})
        except TransportError:
            return answer(502, {"error": "the gateway did not answer"})

    def tick(self) -> None:
        """Check for due work and give it to the run processes."""
        try:
            sessions = self.sessions()
            self.editing = self.gateway.editing()
        except (GatewayError, TransportError, Refused):
            # gateway unavailability does not stop time and webhook triggers, and no session changes a file then
            sessions, self.editing = [], False
        if not self.editing:
            self.store.sync()
        self.triggers.tick(sessions)
        self.runs.fill()

    def stop(self) -> None:
        """Stop this factory's run processes without ending their runs."""
        self.runs.stop()

    def status(self, **_: Any) -> Document:
        """Return the two projects, the gateway state and the count of live runs."""
        gateway, models = self.gateway.status()
        kind = urlsplit(settings.DATABASE).scheme
        return {
            "name": settings.NAME,
            "project": settings.PROJECT,
            "workflows": settings.WORKFLOWS,
            "models": models,
            "started": self.started,
            "live_runs": self.runs.live(),
            "database": "postgres" if kind == "postgresql" else kind,
            "gateway": gateway,
        }

    def models(self, **_: Any) -> Document:
        """List the models that a workflow can use, in the order of the gateway."""
        return {"models": self.gateway.models()}

    def workflows(self, **_: Any) -> Document:
        """List the workflows and what their current versions say about themselves."""
        return {"workflows": self.store.listing(), "project": settings.WORKFLOWS_NAME, "editing": self.editing}

    def workflow(self, name: str, query: dict[str, str], **_: Any) -> Document:
        """Return the current version of a workflow, or the version that the query names."""
        given = query.get("version")
        if given is not None and not (given.isascii() and given.isdigit() and len(given) <= 8):
            raise Refused(400, "the version is not valid")
        return self.store.detail(name, None if given is None else int(given))

    def check(self, payload: Document, **_: Any) -> Document:
        """Describe a source without keeping it. The source has the name of its function."""
        name = payload.get("name", "draft")
        return self.store.check(need(NAME, name, "the workflow name"), workflow_text(payload))

    def restore(self, name: str, payload: Document, **_: Any) -> Document:
        """Make an old version the current one."""
        return self.store.restore(name, payload.get("version"))

    def select(self, name: str, payload: Document, **_: Any) -> Document:
        """Keep the model that a person selected for a workflow. A body without a model selects the model of the code again."""
        self.store.head(name)
        provider, model = payload.get("provider"), payload.get("model")
        if (provider, model) != (None, None) and not any((row["provider"], row["model"]) == (provider, model) for row in self.gateway.models()):
            raise Refused(400, "the gateway does not offer that model")
        self.store.choose(name, provider, model)
        return self.store.detail(name)

    def remove(self, name: str, **_: Any) -> Document:
        """Remove one workflow. Its versions stay."""
        return self.store.remove(name)

    def list_runs(self, query: dict[str, str], **_: Any) -> Document:
        """List the newest runs, with optional workflow and status filters."""
        listed = self.runs.listing(query.get("workflow", ""), query.get("status", ""))[: settings.RUNS_LISTED]
        rows = [
            {
                **{key: doc[key] for key in ("id", "workflow", "title", "status", "created", "finished", "session_id", "trigger")},
                "activities": [[step["title"], step["status"]] for step in doc["activities"]],
            }
            for doc in listed
        ]
        return {"runs": rows}

    def run(self, payload: Document, **_: Any) -> Document:
        """Record a run for the given workflow."""
        name = need(NAME, payload.get("workflow"), "the workflow name")
        about, value = self.store.current(name)
        given = payload.get("input", {})
        if isinstance(given, str):
            given = source.inputs(about, given)
        if not isinstance(given, dict):
            raise Refused(400, "input must be an object or text")
        source.check(about, given)
        return self.runs.create(name, value, about, given, payload.get("session"))

    def show(self, run_id: str, query: dict[str, str], **_: Any) -> Document:
        """Return the recorded run. Include the files and transcript for the page."""
        doc = self.runs.copy(run_id)
        files, turns = [], []
        if (session := doc["session_id"]) and query.get("worker") != "1":
            # A gateway outage must not hide the run or the data that is still available.
            try:
                files = self.gateway.files(session)
            except (GatewayError, TransportError, Refused):
                pass
            try:
                turns = self.gateway.transcript(session)
            except (GatewayError, TransportError, Refused):
                pass
        return {**doc, "source": self.runs.source(run_id), "files": files, "turns": turns}

    def cancel(self, run_id: str, **_: Any) -> Document:
        """Cancel a run and its current turn."""
        doc = self.runs.cancel(run_id)
        if doc["status"] == "cancelled" and doc["session_id"]:
            self.gateway.cancel(doc["session_id"])
        return doc

    def retry(self, run_id: str, **_: Any) -> Document:
        """Start a failed run again."""
        return self.runs.retry(run_id)

    def event(self, run_id: str, payload: Document, **_: Any) -> Document:
        """Record an event from a run process."""
        self.runs.event(run_id, payload)
        return {"recorded": True}

    def decide(self, run_id: str, key: str, payload: Document, **_: Any) -> Document:
        """Record a decision at a gate."""
        approved = payload.get("approved")
        if type(approved) is not bool:
            raise Refused(400, "approved must be true or false")
        return self.runs.decide(run_id, key, approved, text(payload.get("comment", ""), "the comment", settings.COMMENT_LIMIT, empty=True))

    def attached(self, query: dict[str, str], **_: Any) -> Document:
        """List the files that a session attached."""
        session = need(SESSION, query.get("session"), "the session id")
        return {"files": self.gateway.files(session)}

    def attachment(self, session: str, name: str, **_: Any) -> Answer:
        """Return an attachment as plain bytes, not as executable page content."""
        data = self.gateway.attachment(need(SESSION, session, "the session id"), need(FILE, name, "the file name"))
        return 200, "text/plain; charset=utf-8", data

    def list_triggers(self, **_: Any) -> Document:
        """List the triggers with their most recent run and next scheduled time."""
        return {"triggers": self.triggers.listing()}

    def events(self, query: dict[str, str], **_: Any) -> Document:
        """List the newest events."""
        return {"events": self.records.listing("event/")[: limit(query, settings.EVENTS_LISTED)]}

    def hook(self, name: str, query: dict[str, str], payload: Any, headers: Mapping[str, str], **_: Any) -> Document:
        """Accept a webhook with its own token and keep its JSON as event data."""
        token = next((value for name, value in headers.items() if name.lower() == "x-factory-token"), query.get("token", ""))
        if not settings.TOKEN or not hmac.compare_digest(settings.TOKEN.encode(), token.encode()):
            raise Refused(401, "the hook token is not valid")
        kind = f"webhook.{need(NAME, name, 'the hook name')}"
        return {"event": self.triggers.emit(kind, {"body": payload})}

    def list_suggestions(self, query: dict[str, str], **_: Any) -> Document:
        """List suggestions, with an optional status filter."""
        return {"suggestions": self.suggestions.listing(query.get("status", ""))}

    def suggest(self, payload: Document, **_: Any) -> Document:
        """Store one suggestion."""
        return self.suggestions.create(payload)

    def suggestion(self, suggestion_id: str, **_: Any) -> Document:
        """Return one suggestion."""
        return self.suggestions.get(suggestion_id)

    def suggestion_decide(self, suggestion_id: str, action: str, payload: Document, **_: Any) -> Document:
        """Accept or decline one suggestion."""
        return self.suggestions.decide(suggestion_id, "accepted" if action == "accept" else "declined", payload.get("comment", ""))

    def discuss(self, suggestion_id: str, **_: Any) -> Document:
        """Return the Vis session where a person can discuss this suggestion."""
        return {"session_id": self.suggestions.discuss(suggestion_id)}

    def comment(self, suggestion_id: str, payload: Document, **_: Any) -> Document:
        """Add a comment to one suggestion."""
        return self.suggestions.comment(suggestion_id, payload)

    def sessions(self) -> list[Document]:
        """Map the gateway sessions to their run, if they belong to one."""
        owners = {}
        for run in self.runs.listing():
            for session in [run["session_id"], *run["sessions"]]:
                if session:
                    owners.setdefault(session, run["id"])
        return [{**session, "run": owners.get(session["id"])} for session in self.gateway.sessions()]

    def list_sessions(self, **_: Any) -> Document:
        """List the visible session fields for the factory page."""
        fields = ("id", "title", "group", "updated", "turns", "run")
        return {"sessions": [{key: session[key] for key in fields} for session in self.sessions()]}

    def memory(self, **_: Any) -> Document:
        """List the team and personal memories."""
        return self.memories.listing()

    def entries(self, name: str, query: dict[str, str], **_: Any) -> Document:
        """Read matching memory entries, newest first."""
        try:
            return self.memories.entries(name, query=query.get("query", ""), pattern=query.get("pattern", ""), limit=limit(query, 200, 50), before=query.get("before"))
        except LookupError as failure:
            raise Refused(404, str(failure)) from None
        except ValueError as failure:
            raise Refused(400, str(failure)) from None


def invalid_constant(_: str) -> None:
    """Refuse non-JSON numbers in a JSON body."""
    raise ValueError("not a JSON value")


def workflow_text(payload: Document) -> str:
    """Return the workflow text, after checking its size."""
    value = payload.get("source")
    if not isinstance(value, str):
        raise Refused(400, "the source must be text")
    if len(value.encode()) > settings.WORKFLOW_LIMIT:
        raise Refused(413, "the workflow is too large")
    return value


def limit(query: dict[str, str], maximum: int, default: int | None = None) -> int:
    """Read a nonnegative limit no larger than the given maximum."""
    try:
        value = int(query.get("limit", str(maximum if default is None else default)))
    except ValueError:
        raise Refused(400, "the limit must be a whole number") from None
    if value < 0:
        raise Refused(400, "the limit must not be negative")
    return min(value, maximum)
