"""Give engine tests a factory with a fake gateway and child processes."""

from __future__ import annotations

import json
from types import SimpleNamespace

import pytest
from vis_optmem.shared import MemoryTable

from factory import settings
from factory.api import App
from factory.records import Refused

SESSION = "12345678-1234-1234-1234-123456789abc"


class Process:
    """Keep the exit code that the factory reads from a child process."""

    code = None

    def poll(self):
        """Return the fake exit code."""
        return self.code

    def kill(self):
        """End the process."""
        self.code = -9

    def terminate(self):
        """End the process without finishing its run."""
        self.code = -15


class Starter:
    """Give each run a fake child process."""

    def __init__(self):
        """Record all starts and all checks."""
        self.started, self.checked, self.fault = [], [], ""

    def start(self, run_id):
        """Record a start and return its process."""
        process = Process()
        self.started.append((run_id, process))
        return process

    def check(self, name, source):
        """Record a check. Refuse the source when the test set a fault."""
        self.checked.append(name)
        if self.fault:
            raise Refused(400, self.fault)


class Gateway:
    """Give engine tests a list of gateway sessions and a fake discussion."""

    def __init__(self):
        """Start without any sessions."""
        self.rows, self.talks, self.cancelled = [], [], []
        self.is_editing = False

    def sessions(self):
        """List the fake sessions."""
        return self.rows

    def editing(self):
        """Return whether a session of the workflow project has a running turn."""
        return self.is_editing

    def busy(self, session):
        """Return whether the session has a running turn."""
        return any(row["id"] == session and row.get("busy") for row in self.rows)

    def discuss(self, suggestion):
        """Record the suggestion and return a session id."""
        self.talks.append(suggestion["id"])
        return SESSION

    def cancel(self, session):
        """Record the cancellation."""
        self.cancelled.append(session)

    def files(self, session):
        """List the files of this session."""
        return []

    def transcript(self, session):
        """Return a turn that the run page can show."""
        return [{"id": "turn", "request": "Review this"}]


@pytest.fixture
def engine(monkeypatch, tmp_path):
    """Set up the engine with a real table contract and fake services."""
    workflows = tmp_path / "workflows"
    workflows.mkdir()
    (tmp_path / "project").mkdir()
    monkeypatch.setattr(settings, "PROJECT", str(tmp_path / "project"))
    monkeypatch.setattr(settings, "WORKFLOWS", str(workflows))
    monkeypatch.setattr(settings, "PARALLEL", 3)
    table, starter, gateway = MemoryTable(), Starter(), Gateway()
    app = App(table, None, gateway, starter)

    def call(method, path, payload=None, headers=None):
        """Return the status and JSON of one factory request."""
        body = json.dumps(payload).encode() if payload is not None else b""
        status, _, raw = app.handle(method, f"/factory/api/{path}", body, headers or {})
        return status, json.loads(raw)

    def write(name, source):
        """Change a workflow file as a session does, and let the factory look at it."""
        (workflows / f"{name}.py").write_text(source, encoding="utf-8")
        app.store.sync()

    return SimpleNamespace(table=table, app=app, starter=starter, gateway=gateway, workflows=workflows, call=call, write=write)
