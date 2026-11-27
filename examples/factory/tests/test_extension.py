"""Exercise the Factory tools with an HTTP server and a session index."""

from __future__ import annotations

import json
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from runpy import run_path
from threading import Thread

import blockether.vis.extension as vis
import pytest

from factory.models import RunRef, Suggestion

SUGGESTION = {
    "id": "0123456789abcdef0000",
    "title": "Add a check",
    "body": "Check the saved record.",
    "kind": "test",
    "effort": "small",
    "session": "session-id",
    "run": "run-id",
    "status": "open",
    "talk": None,
    "comments": [],
    "created": 1.0,
    "decided": None,
}
RUN = {"id": "run-id", "workflow": "review", "title": "Review", "status": "queued", "session_id": None}


@pytest.fixture
def tools(monkeypatch):
    """Load the extension through the Vis registration function."""

    def declare_env(names):
        """Check the requested environment names, without supplying credentials yet."""
        assert set(json.loads(names)) == {"FACTORY_URL", "FACTORY_TOKEN"}
        return "{}"

    monkeypatch.setitem(vis._registration, "spec", None)
    monkeypatch.setattr(vis._host, "declare_env", declare_env)
    loaded = run_path(str(Path(__file__).resolve().parents[1] / "extension" / "factory.py"))
    return loaded["Factory"]()


@pytest.fixture
def fake_api(monkeypatch):
    """Serve Factory responses over HTTP and record the requests."""
    calls = []

    class Handler(BaseHTTPRequestHandler):
        """Return the modelled response to each request."""

        def do_GET(self):
            """List the records or return a Factory error."""
            self.answer()

        def do_POST(self):
            """Create a record."""
            self.answer()

        def log_message(self, *_args):
            """Do not write server logs during the test."""

        def answer(self):
            """Record method, path, body and authentication, then send JSON."""
            length = int(self.headers.get("Content-Length", "0"))
            body = json.loads(self.rfile.read(length)) if length else None
            calls.append((self.command, self.path, body, self.headers.get("Authorization")))
            responses = {
                ("POST", "/factory/api/suggestions"): SUGGESTION,
                ("GET", "/factory/api/suggestions?status=open"): {"suggestions": [SUGGESTION]},
                ("GET", "/factory/api/runs"): {"runs": [RUN | {"log": ["not part of a run reference"]}]},
                ("POST", "/factory/api/runs"): RUN | {"input": {"why": "new"}},
            }
            value = responses.get((self.command, self.path))
            data = json.dumps(value or {"error": "No such resource"}).encode()
            self.send_response(200 if value is not None else 404)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

    with ThreadingHTTPServer(("127.0.0.1", 0), Handler) as server:
        monkeypatch.setenv("FACTORY_URL", f"http://127.0.0.1:{server.server_port}")
        monkeypatch.setenv("FACTORY_TOKEN", "test-token")
        thread = Thread(target=server.serve_forever)
        thread.start()
        try:
            yield calls
        finally:
            server.shutdown()
            thread.join()


def test_calls_factory_api(tools, fake_api):
    """Send valid records and return only the documented fields."""
    assert Suggestion.model_validate(tools.suggest("Add a check", "Check the saved record.", "test", "small", "session-id", "run-id")["suggestion"]).id == SUGGESTION["id"]
    assert tools.suggestions(status="open")["suggestions"] == [SUGGESTION]
    assert [RunRef.model_validate(value).id for value in tools.runs()["runs"]] == [RUN["id"]]
    assert RunRef.model_validate(tools.start("review", {"why": "new"})["run"]).id == RUN["id"]
    assert fake_api == [
        ("POST", "/factory/api/suggestions", {key: SUGGESTION[key] for key in ("title", "body", "kind", "effort", "session", "run")}, "Bearer test-token"),
        ("GET", "/factory/api/suggestions?status=open", None, "Bearer test-token"),
        ("GET", "/factory/api/runs", None, "Bearer test-token"),
        ("POST", "/factory/api/runs", {"workflow": "review", "input": {"why": "new"}}, "Bearer test-token"),
    ]
    with pytest.raises(RuntimeError, match="No such resource"):
        tools.suggestions(status="declined")


def test_digest_excludes_tool_output_and_incomplete_turns(tools, monkeypatch):
    """Return only bounded completed requests and final answers, oldest first."""

    def read_session(session, *, view, limit):
        """Return a newest-first Vis session index page."""
        assert (session, view, limit) == ("session-id", "index", 4)
        return {
            "session": {"id": session},
            "session_id": session,
            "turns": [
                {"outcome": "running", "request_preview": "Pending", "answer_preview": "Partial"},
                {"outcome": "complete", "answer_preview": "Council answer"},
                {"outcome": "complete", "request_preview": "B" * 170, "answer_preview": "Answer B", "stdout": "private output"},
                {"outcome": "complete", "request_preview": "A", "answer_preview": "Answer A"},
                {"outcome": "complete", "request_preview": "Before A", "answer_preview": "Old answer"},
            ],
        }

    monkeypatch.setattr(vis, "read_session", read_session)
    assert tools.transcript("session-id", limit=4)["turns"] == [{"request": "A", "answer": "Answer A"}, {"request": "B" * 160, "answer": "Answer B"}]
    with pytest.raises(ValueError, match="between 1 and 20"):
        tools.transcript("session-id", limit=21)
