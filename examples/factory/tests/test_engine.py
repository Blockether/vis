"""Check workflow runs, schedules and suggestions through the factory engine."""

from __future__ import annotations

import shutil
import time
from concurrent.futures import ThreadPoolExecutor
from datetime import UTC, datetime
from threading import Barrier

import pytest

from factory import settings
from factory.api import App
from factory.records import Refused
from factory.triggers import Cron
from factory.workflows import source

SESSION = "12345678-1234-1234-1234-123456789abc"
SIMPLE = "from factory.workflows import workflow\n\n@workflow()\ndef review(ctx):\n    pass\n"


def test_cron_match():
    """A schedule supports steps, ranges, lists and the standard day rule."""
    cron = Cron.parse("*/15 3-4 1,10 2,3 1")
    monday = datetime(2026, 2, 2, 3, 45, tzinfo=UTC)
    assert cron.matches(monday)
    assert cron.matches(datetime(2026, 2, 10, 4, 15, tzinfo=UTC))
    assert not cron.matches(datetime(2026, 2, 3, 3, 45, tzinfo=UTC))
    assert not cron.matches(datetime(2026, 2, 2, 3, 44, tzinfo=UTC))
    assert cron.next(int(monday.timestamp() // 60)) == int(datetime(2026, 2, 2, 4, tzinfo=UTC).timestamp() // 60)
    with pytest.raises(Refused):
        Cron.parse("*/0 3 * * *")


def test_two_ticks_fire_one_run_for_one_cron_slot(engine, monkeypatch):
    """Two schedulers that both read a due slot must record just one run."""
    text = "from factory.workflows import cron, workflow\n\n@workflow(triggers=[cron('* * * * *', input={'reason': 'schedule'})])\ndef review(ctx, reason):\n    pass\n"
    assert engine.call("PUT", "workflows/review", {"source": text})[0] == 200
    peer = App(engine.table, None, engine.gateway, type(engine.starter)())
    barrier = Barrier(2)
    write = engine.table.transact
    first = set()

    def concurrent(puts=None, deletes=(), conditions=()):
        """Put both initial fired transactions at the same barrier."""
        if puts and any(key.startswith("fired/") for key in puts) and len(first) < 2:
            first.add(next(key for key in puts if key.startswith("run/")))
            barrier.wait(timeout=5)
        return write(puts, deletes, conditions)

    monkeypatch.setattr(engine.table, "transact", concurrent)
    now = datetime(2026, 2, 2, 3, tzinfo=UTC).timestamp()
    with ThreadPoolExecutor(2) as pool:
        futures = [pool.submit(app.triggers.tick, [], now) for app in (engine.app, peer)]
        for future in futures:
            future.result()
    engine.app.triggers.tick([], now)
    (first_run,) = engine.app.runs.listing()
    with ThreadPoolExecutor(2) as pool:
        list(pool.map(lambda app: app.runs.fill(), (engine.app, peer)))
    assert len(engine.starter.started) + len(peer.runs.starter.started) == 1
    assert first_run["input"] == {"reason": "schedule"}
    assert first_run["trigger"] == {"id": "review/0", "kind": "cron", "slot": str(int(now // 60))}
    assert engine.app.runs.source(first_run["id"]) == text
    engine.app.triggers.tick([], now + 120)
    assert len(engine.app.runs.listing()) == 3
    engine.call("PUT", "workflows/new", {"source": text})
    engine.app.triggers.tick([], now + 120)
    assert len(engine.app.runs.listing(workflow="new")) == 1


def test_hook_and_trigger_input(engine, monkeypatch):
    """A valid hook records any JSON value. Only workflow inputs reach the run."""
    monkeypatch.setattr(settings, "TOKEN", "hook-test")
    script = "from factory.workflows import on, workflow\n\n@workflow(triggers=[on('webhook.github', input={'reason':'push'})])\ndef review(ctx, body, reason):\n    pass\n"
    engine.call("PUT", "workflows/review", {"source": script})
    assert engine.call("POST", "hooks/github", [3])[0] == 401
    status, result = engine.call("POST", "hooks/github", [3], {"X-Factory-Token": "hook-test"})
    assert status == 200
    engine.app.tick()
    engine.app.tick()
    (run,) = engine.app.runs.listing()
    assert run["input"] == {"body": [3], "reason": "push"}
    assert run["trigger"]["slot"] == result["event"]
    assert engine.starter.started[0][0] == run["id"]


def test_run_input_must_fit_the_workflow(engine):
    """A run does not start without a necessary input or with a value of the wrong simple type."""
    script = "from factory.workflows import workflow\n\n@workflow()\ndef review(ctx, body: str, count: int = 1):\n    pass\n"
    engine.call("PUT", "workflows/review", {"source": script})
    for given in ({}, {"body": 123}, {"body": "x", "count": True}, {"body": "x", "other": 1}):
        assert engine.call("POST", "runs", {"workflow": "review", "input": given})[0] == 400, given
    assert engine.call("POST", "runs", {"workflow": "review", "input": {"body": "x", "count": 2}})[0] == 200


def test_session_idle_events(engine):
    """Only a person's idle session makes an event, once for each turn count."""
    now = time.time()
    text = "from factory.workflows import on, workflow\n\n@workflow(triggers=[on('session.idle', input={'title':'Follow-up'})])\ndef review(ctx, session, title):\n    pass\n"
    engine.call("PUT", "workflows/review", {"source": text})
    idle = {"id": SESSION, "title": "Chat", "group": None, "updated": now - settings.IDLE_S - 1, "turns": 2, "busy": False}
    engine.gateway.rows = [
        idle,
        {**idle, "id": "run-session", "group": settings.RUNS_GROUP},
        {**idle, "id": "talk-session", "group": settings.TALKS_GROUP},
        {**idle, "id": "busy-session", "busy": True},
    ]
    engine.app.tick()
    engine.app.tick()
    (first,) = engine.app.runs.listing()
    assert first["input"] == {"session": SESSION, "title": "Follow-up"}
    idle["turns"] += 1
    engine.app.tick()
    assert len(engine.app.runs.listing()) == 2
    assert [event["data"]["turns"] for event in engine.call("GET", "events")[1]["events"]] == [3, 2]


def test_gate_and_restart(engine):
    """A gate decision returns a waiting run to work; a new factory can resume it."""
    engine.call("PUT", "workflows/review", {"source": SIMPLE})
    status, run = engine.call("POST", "runs", {"workflow": "review"})
    assert status == 200
    path = f"runs/{run['id']}"
    for data in (
        {"type": "started", "session_id": SESSION, "base": 0},
        {"type": "activity_started", "seq": 0, "name": "plan"},
        {"type": "attached", "seq": 0, "name": "PLAN.md"},
        {"type": "waiting", "seq": 0, "key": "plan", "title": "Approve", "document": "PLAN.md", "version": 1, "size": 24},
    ):
        assert engine.call("POST", f"{path}/events", data)[0] == 200
    waiting = engine.call("GET", path)[1]
    assert waiting["status"] == "waiting"
    assert waiting["waiting"]["document"] == "PLAN.md"
    assert waiting["activities"][0]["files"] == ["PLAN.md"]
    assert waiting["turns"][0]["request"] == "Review this"
    engine.starter.started[0][1].code = 0
    assert engine.call("POST", f"{path}/gates/plan", {"approved": True, "comment": "Proceed"})[0] == 200
    assert engine.call("POST", f"{path}/gates/plan", {"approved": False})[0] == 409
    assert len(engine.starter.started) == 2
    engine.app.stop()
    new_starter = type(engine.starter)()
    next_app = App(engine.table, None, engine.gateway, new_starter)
    next_app.tick()
    assert [run_id for run_id, _ in new_starter.started] == [run["id"]]
    current = next_app.runs.copy(run["id"])
    assert current["status"] == "running"
    assert current["decisions"]["plan"] == {"approved": True, "comment": "Proceed", "at": current["decisions"]["plan"]["at"], "document": "PLAN.md", "version": 1, "size": 24}
    next_app.runs.event(run["id"], {"type": "activity_completed", "seq": 0, "result": "done"})
    next_app.runs.event(run["id"], {"type": "completed", "result": "done"})
    assert next_app.runs.copy(run["id"])["status"] == "completed"
    (event,) = next_app.records.listing("event/")
    assert (event["kind"], event["data"]["run"]) == ("run.completed", run["id"])


def test_suggestion_decision_and_discussion(engine):
    """Replay does not make a second suggestion, talk or decision."""
    data = {"title": "Test the scheduler", "body": "Add a test", "kind": "test", "effort": "small", "run": "1" * 20}
    status, created = engine.call("POST", "suggestions", data)
    assert status == 200
    assert engine.call("POST", "suggestions", data)[1] == created
    path = f"suggestions/{created['id']}"
    assert engine.call("POST", f"{path}/discuss")[1] == {"session_id": SESSION}
    assert engine.call("POST", f"{path}/discuss")[1] == {"session_id": SESSION}
    assert engine.gateway.talks == [created["id"]]
    status, accepted = engine.call("POST", f"{path}/accept", {"comment": "Yes"})
    assert status == 200
    assert accepted["status"] == "accepted"
    assert accepted["comments"][0]["text"] == "Yes"
    assert engine.call("POST", f"{path}/decline")[0] == 409
    assert [event["kind"] for event in engine.call("GET", "events")[1]["events"]] == ["suggestion.accepted", "suggestion.created"]


def test_workflow_save_checks_its_base(engine):
    """An editor cannot overwrite a workflow that changed after its read."""
    assert engine.call("PUT", "workflows/review", {"source": SIMPLE})[0] == 200
    changed = SIMPLE.replace("@workflow()", "@workflow(title='New')")
    assert engine.call("PUT", "workflows/review", {"source": changed, "base": SIMPLE})[0] == 200
    assert engine.call("PUT", "workflows/review", {"source": SIMPLE + "\n", "base": SIMPLE})[0] == 409
    assert source.read("review") == changed


@pytest.mark.skipif(shutil.which("git") is None, reason="This test needs Git.")
def test_git_save_commits_only_the_workflow(engine):
    """Saving a workflow does not commit another file that a person staged."""
    project = engine.workflows.parent
    assert source.git(project, "init").returncode == 0
    (project / "OTHER").write_text("Another file", encoding="utf-8")
    assert source.git(project, "add", "OTHER").returncode == 0
    status, saved = engine.call("PUT", "workflows/review", {"source": SIMPLE})
    assert status == 200
    assert saved["committed"]
    assert source.git(project, "show", "HEAD:workflows/review.py").stdout == SIMPLE
    assert source.git(project, "show", "HEAD:OTHER").returncode != 0
    assert source.git(project, "diff", "--cached", "--name-only").stdout.splitlines() == ["OTHER"]
    assert engine.call("DELETE", "workflows/review")[0] == 200
    assert source.git(project, "show", "HEAD:workflows/review.py").returncode != 0


def test_factory_comment_is_replay_safe(engine):
    """Repeating one factory comment does not add another comment."""
    data = {"title": "Review", "body": "Add a test", "kind": "test", "effort": "small"}
    created = engine.call("POST", "suggestions", data)[1]
    path = f"suggestions/{created['id']}/comments"
    comment = {"author": "factory", "text": "Run A: done"}
    status, first = engine.call("POST", path, comment)
    assert status == 200
    assert engine.call("POST", path, comment) == (200, first)
    _, second = engine.call("POST", path, {**comment, "text": "Run B: done"})
    assert [entry["text"] for entry in second["comments"]] == ["Run A: done", "Run B: done"]


def test_talk_can_finish_a_claim_after_gateway_error(engine, monkeypatch):
    """Keep the talk claim durable until a gateway call succeeds."""
    data = {"title": "Review", "body": "Add a test", "kind": "test", "effort": "small"}
    created = engine.call("POST", "suggestions", data)[1]
    key = f"suggestion/{created['id']}"
    path = f"suggestions/{created['id']}/discuss"
    calls = []

    def discuss(suggestion):
        """Simulate a failure after the claim, then return a talk session."""
        assert engine.table.get(key)["talk_claim"] == 1
        calls.append(suggestion["id"])
        if len(calls) == 1:
            raise Refused(502, "the gateway did not answer")
        return SESSION

    monkeypatch.setattr(engine.app.suggestions, "open_talk", discuss)
    assert engine.call("POST", path)[0] == 502
    assert engine.call("POST", path) == (200, {"session_id": SESSION})
    assert engine.call("POST", path) == (200, {"session_id": SESSION})
    assert len(calls) == 2
