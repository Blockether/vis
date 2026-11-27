"""Check workflow runs, schedules and suggestions through the factory engine."""

from __future__ import annotations

import time
from concurrent.futures import ThreadPoolExecutor
from datetime import UTC, datetime
from pathlib import Path
from threading import Barrier

import pytest

from factory import settings
from factory.api import App
from factory.records import Refused
from factory.runs import Starter
from factory.triggers import Cron

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
    engine.write("review", text)
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
    engine.write("new", text)
    engine.app.triggers.tick([], now + 120)
    assert len(engine.app.runs.listing(workflow="new")) == 1


def test_hook_and_trigger_input(engine, monkeypatch):
    """A valid hook records any JSON value. Only workflow inputs reach the run."""
    monkeypatch.setattr(settings, "TOKEN", "hook-test")
    script = "from factory.workflows import on, workflow\n\n@workflow(triggers=[on('webhook.github', input={'reason':'push'})])\ndef review(ctx, body, reason):\n    pass\n"
    engine.write("review", script)
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
    engine.write("review", script)
    for given in ({}, {"body": 123}, {"body": "x", "count": True}, {"body": "x", "other": 1}):
        assert engine.call("POST", "runs", {"workflow": "review", "input": given})[0] == 400, given
    assert engine.call("POST", "runs", {"workflow": "review", "input": {"body": "x", "count": 2}})[0] == 200


def test_session_idle_events(engine):
    """Only a person's idle session makes an event, once for each turn count."""
    now = time.time()
    text = "from factory.workflows import on, workflow\n\n@workflow(triggers=[on('session.idle', input={'title':'Follow-up'})])\ndef review(ctx, session, title):\n    pass\n"
    engine.write("review", text)
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
    engine.write("review", SIMPLE)
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


def test_file_change_becomes_a_version(engine):
    """A changed file of the workflow project is the next version, and a run uses it."""
    engine.write("review", SIMPLE)
    changed = SIMPLE.replace("@workflow()", "@workflow(title='New')")
    engine.gateway.is_editing = True
    (engine.workflows / "review.py").write_text(changed, encoding="utf-8")
    engine.app.tick()
    assert engine.call("GET", "workflows/review")[1]["version"] == 1  # the session did not complete its change
    engine.gateway.is_editing = False
    engine.app.tick()
    engine.app.tick()
    shown = engine.call("GET", "workflows/review")[1]
    assert (shown["version"], shown["title"], shown["source"]) == (2, "New", changed)
    assert [(row["version"], row["origin"]) for row in shown["versions"]] == [(2, "project"), (1, "project")]
    assert engine.call("GET", "workflows/review?version=1")[1]["source"] == SIMPLE
    run = engine.call("POST", "runs", {"workflow": "review"})[1]
    assert run["version"] == 2
    assert engine.app.runs.source(run["id"]) == changed
    assert engine.call("PUT", "workflows/review", {"source": SIMPLE})[0] == 404  # the API does not take source


def test_file_that_does_not_load_keeps_the_version(engine):
    """A file with an error makes no version. The page gets the error, and a run uses the last version."""
    engine.write("review", SIMPLE)
    engine.write("review", "def review(:\n")
    (row,) = engine.call("GET", "workflows")[1]["workflows"]
    assert row["version"] == 1
    assert row["draft"]
    run = engine.call("POST", "runs", {"workflow": "review"})[1]
    assert engine.app.runs.source(run["id"]) == SIMPLE
    engine.write("review", "return 1\n" + SIMPLE)  # the parser accepts this, a run does not
    assert engine.call("GET", "workflows/review")[1]["draft"] == "line 1: 'return' outside function"
    engine.starter.fault = "ModuleNotFoundError: No module named 'absent'"
    engine.write("review", "import absent\n" + SIMPLE)
    engine.app.tick()
    shown = engine.call("GET", "workflows/review")[1]
    assert (shown["version"], shown["draft"]) == (1, engine.starter.fault)
    assert engine.starter.checked.count("review") == 2  # the first version, then one check of the refused file
    engine.starter.fault = ""
    engine.write("review", SIMPLE + "\n")
    (row,) = engine.call("GET", "workflows")[1]["workflows"]
    assert (row["version"], row["draft"]) == (2, None)


def test_child_load_finds_what_the_parser_accepts(engine):
    """The check loads the source in a child as a run does, so a missing module is a fault before a run."""
    Starter().check("review", SIMPLE)
    with pytest.raises(Refused, match="ModuleNotFoundError"):
        Starter().check("review", "import absent_module_of_the_factory_test\n" + SIMPLE)


def test_file_written_during_a_look_is_not_lost(engine, monkeypatch):
    """A file that appears after the factory read the directory stays, and becomes a version."""
    engine.write("review", SIMPLE)
    (engine.workflows / "review.py").unlink()
    changed = SIMPLE.replace("@workflow()", "@workflow(title='New')")
    raw = engine.app.store.raw

    def late(name):
        """Answer that no file is there, then let an editor write one."""
        monkeypatch.setattr(engine.app.store, "raw", raw)
        (engine.workflows / "review.py").write_text(changed, encoding="utf-8")

    monkeypatch.setattr(engine.app.store, "raw", late)
    engine.app.tick()
    engine.app.tick()
    assert (engine.workflows / "review.py").read_text() == changed
    assert engine.call("GET", "workflows/review")[1]["version"] == 2


@pytest.mark.parametrize("call", [("POST", "workflows/review/restore", {"version": 1}), ("DELETE", "workflows/review", None)])
def test_late_edit_survives_a_restore_and_a_removal(engine, monkeypatch, call):
    """An edit that lands after the factory read the file is not overwritten or removed. It is the next version."""
    engine.write("review", SIMPLE)
    engine.write("review", SIMPLE + "\n")
    edit = SIMPLE.replace("@workflow()", "@workflow(title='Late')")
    raw, reads = engine.app.store.raw, []

    def late(name):
        """Give the content of the file, then let an editor change the file after the last read of the call."""
        found = raw(name)
        reads.append(found)
        if len(reads) == 2:
            (engine.workflows / "review.py").write_text(edit, encoding="utf-8")
        return found

    monkeypatch.setattr(engine.app.store, "raw", late)
    assert engine.call(*call)[0] == 200
    monkeypatch.setattr(engine.app.store, "raw", raw)
    engine.app.tick()
    engine.app.tick()
    assert (engine.workflows / "review.py").read_text() == edit
    shown = engine.call("GET", "workflows/review")[1]
    assert (shown["version"], shown["source"], shown["deleted"]) == (4, edit, False)
    assert sorted(path.name for path in engine.workflows.iterdir()) == ["review.py"]


def test_restore_is_a_new_version_and_survives_a_stop(engine, monkeypatch):
    """A restore writes the table first. A stop before the file write does not make the old file a version."""
    engine.write("review", SIMPLE)
    changed = SIMPLE.replace("@workflow()", "@workflow(title='New')")
    engine.write("review", changed)
    write = engine.app.store.write

    def stop(name, source, before):
        """Fail as a full disk does."""
        raise OSError(28, "No space left on device")

    monkeypatch.setattr(engine.app.store, "write", stop)
    assert engine.call("POST", "workflows/review/restore", {"version": 1})[1]["version"] == 3
    assert (engine.workflows / "review.py").read_text() == changed
    monkeypatch.setattr(engine.app.store, "write", write)
    engine.app.tick()
    engine.app.tick()
    shown = engine.call("GET", "workflows/review")[1]
    assert (shown["version"], shown["origin"], shown["source"], shown["draft"]) == (3, "restore", SIMPLE, None)
    assert (engine.workflows / "review.py").read_text() == SIMPLE
    assert engine.call("POST", "workflows/review/restore", {"version": 3})[0] == 400
    assert engine.call("POST", "workflows/review/restore", {"version": 9})[0] == 404
    for value in (0, -1, 1.5, 1e999, True, "x", "1", None, [1]):
        assert engine.call("POST", "workflows/review/restore", {"version": value})[0] == 400, value
    assert engine.call("GET", "workflows/review?version=1e999")[0] == 400


def test_removed_workflow_keeps_its_versions(engine):
    """A removed workflow does not run and has no file. A person can restore it, or a session can write it again."""
    text = "from factory.workflows import cron, workflow\n\n@workflow(triggers=[cron('* * * * *')])\ndef review(ctx):\n    pass\n"
    engine.write("review", text)
    assert engine.call("DELETE", "workflows/review")[1] == {"removed": "review"}
    assert not (engine.workflows / "review.py").exists()
    engine.app.tick()
    assert engine.app.runs.listing() == []
    assert engine.call("POST", "runs", {"workflow": "review"})[0] == 404
    (row,) = engine.call("GET", "workflows")[1]["workflows"]
    assert (row["deleted"], row["version"], row["triggers"]) == (True, 2, [])
    engine.write("review", text)
    assert engine.call("GET", "workflows/review")[1]["version"] == 3
    engine.call("DELETE", "workflows/review")
    assert engine.call("POST", "workflows/review/restore", {"version": 1})[1]["version"] == 5
    assert (engine.workflows / "review.py").read_text() == text


def test_new_machine_gets_the_files_from_the_table(engine, tmp_path, monkeypatch):
    """The table is the store: an empty workflow project gets each current version as a file."""
    engine.write("review", SIMPLE)
    monkeypatch.setattr(settings, "WORKFLOWS", str(tmp_path / "other"))
    App(engine.table, None, engine.gateway, type(engine.starter)()).tick()
    assert (tmp_path / "other" / "review.py").read_text() == SIMPLE
    assert engine.call("GET", "workflows/review")[1]["version"] == 1


def test_new_database_gets_the_first_workflows_one_time(engine):
    """A new database gets the first workflows. A later start does not bring a removed one back."""
    first = Path(__file__).resolve().parents[1] / "workflows"
    engine.app.store.seed(first)
    engine.app.tick()
    rows = engine.call("GET", "workflows")[1]["workflows"]
    assert {(row["name"], row["version"], row["draft"]) for row in rows} == {("implement", 1, None), ("remember", 1, None), ("suggest", 1, None)}
    assert (engine.workflows / "suggest.py").read_text() == (first / "suggest.py").read_text()
    engine.call("DELETE", "workflows/suggest")
    engine.app.store.seed(first)
    engine.app.tick()
    assert engine.call("GET", "workflows/suggest")[1]["deleted"]
    assert not (engine.workflows / "suggest.py").exists()


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
