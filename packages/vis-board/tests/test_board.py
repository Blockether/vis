"""Check the saved board and the SDK events that change it."""

import json
import time
from concurrent.futures import ThreadPoolExecutor

import blockether.vis.extension as vis
import pytest

from vis_board import start, store, tools
from vis_board.app import CHANGED, BoardApp
from vis_board.store import Store, edit


@pytest.fixture
def saved(tmp_path, monkeypatch):
    workspace = tmp_path / "ab-12-Project"
    workspace.mkdir()
    monkeypatch.setenv("VIS_HOME", str(tmp_path / "home"))
    monkeypatch.setattr(vis, "workspace_root", lambda: workspace)
    return Store(workspace)


def _board(saved):
    saved.read()
    return BoardApp(projects=lambda: [saved.workspace])


@pytest.fixture
def app(saved, monkeypatch):
    recorder = vis.testing.LiveRecorder(object())
    monkeypatch.setattr(vis._host, "live", recorder.live)
    board = _board(saved).open(watch=False)
    yield board, recorder
    board.stop()


def test_round_trip_and_failed_replace(saved, monkeypatch):
    before = saved.read()
    assert before["key_prefix"] == "ABPR"
    first = saved.change(lambda data: edit(data, {"title": "Measure the path"}))
    baseline = saved.path.read_bytes()

    def refuse(name, path):
        assert path == saved.path
        assert (
            json.loads(open(name, encoding="utf-8").read())["issues"][0]["title"]
            == "Other"
        )
        raise OSError("Cannot replace")

    monkeypatch.setattr(store.os, "replace", refuse)
    with pytest.raises(OSError, match="Cannot replace"):
        Store(saved.workspace).change(
            lambda data: edit(data, {"title": "Other"}, first["id"])
        )
    assert saved.path.read_bytes() == baseline
    assert Store(saved.workspace).read()["issues"] == [first]
    assert not list(saved.path.parent.glob(".board-*"))


def test_concurrent_changes_make_different_keys(saved):
    def create(i):
        return Store(saved.workspace).change(
            lambda data: edit(data, {"title": f"Task {i}"})
        )

    with ThreadPoolExecutor(max_workers=4) as pool:
        made = list(pool.map(create, range(10)))
    assert {issue["key"] for issue in made} == {f"ABPR-{i}" for i in range(1, 11)}
    assert len(saved.read()["issues"]) == 10


def test_edit_create_change_delete_and_next_key(saved, app):
    board, rec = app
    assert rec.edit("issues", {"title": "Write tests", "column": "backlog"})[
        "is_accepted"
    ]
    first = rec.node("issues")["issues"][0]
    assert first["key"] == "ABPR-1"
    rec.edit("issues", {"title": "Run tests", "assignee": "owner"}, first["id"])
    assert saved.read()["issues"][0]["title"] == "Run tests"
    assert rec.node("issues")["issues"][0]["assignee"] == "owner"
    rec.edit("issues", {"assignee": None}, first["id"])
    assert "assignee" not in rec.node("issues")["issues"][0]
    rec.edit("issues", {}, first["id"], is_removed=True)
    assert not saved.read()["issues"]
    rec.edit("issues", {"title": "Run checks", "column": "review"})
    assert rec.node("issues")["issues"][0]["key"] == "ABPR-2"
    assert board.view.state()


def _order(issues, column):
    return [
        one["title"]
        for one in sorted(issues, key=lambda one: one["rank"])
        if one["column"] == column
    ]


def test_move_uses_the_full_column_order(saved, app):
    _, rec = app
    for name in ("First", "Middle", "Last"):
        rec.edit("issues", {"title": name, "column": "backlog"})
    rec.edit("issues", {"title": "Moved", "column": "review"})
    issue = next(one for one in saved.read()["issues"] if one["title"] == "Moved")
    assert rec.move("issues", issue["id"], "backlog", 2)["is_accepted"]
    assert _order(saved.read()["issues"], "backlog") == [
        "First",
        "Middle",
        "Moved",
        "Last",
    ]
    assert _order(rec.node("issues")["issues"], "backlog") == [
        "First",
        "Middle",
        "Moved",
        "Last",
    ]
    assert rec.move("issues", issue["id"], "backlog", 0)["is_accepted"]
    assert _order(saved.read()["issues"], "backlog") == [
        "Moved",
        "First",
        "Middle",
        "Last",
    ]


def test_list_issues_combines_filters(saved):
    def setup(data):
        data["people"] = [
            vis.ui.board.person("one", "Person One"),
            vis.ui.board.person("two", "Person Two"),
        ]
        data["epics"] = [vis.ui.board.epic("upgrade", "Upgrade")]
        edit(
            data,
            {
                "title": "Search One",
                "assignee": "one",
                "epic": "upgrade",
                "is_flagged": True,
            },
        )
        edit(data, {"title": "Search Two", "assignee": "two", "epic": "upgrade"})
        edit(data, {"title": "Search Three", "assignee": "one", "is_flagged": True})

    saved.change(setup)
    found = tools.list_issues(
        "SEARCH abpr", assignees=["one", "two"], epic="upgrade", flagged=True
    )
    assert [one["title"] for one in found] == ["Search One"]
    assert len(tools.list_issues()) == 3


def test_bad_event_does_not_end_the_app(saved, app):
    board, rec = app
    rec.edit("issues", {"title": "", "column": "backlog"})
    assert not saved.read()["issues"]
    assert board.view.state()["log"][-1]["level"] == "error"
    rec.edit("issues", {"title": "Try again", "column": "backlog"})
    assert rec.node("issues")["issues"][0]["key"] == "ABPR-1"


def test_tools_change_the_board_and_refresh_open_views(saved, monkeypatch):
    rec = vis.testing.LiveRecorder(object())
    monkeypatch.setattr(vis._host, "live", rec.live)
    board = _board(saved).open()
    try:
        made = tools.create_issue(
            "Make a test", values={"labels": ["sdk"], "is_flagged": True}
        )
        assert rec.node("issues")["issues"][0]["key"] == made["key"]
        assert tools.list_issues("sdk", flagged=True) == [made]
        changed = tools.change_issue(made["key"], {"title": "Run the test"})
        assert rec.node("issues")["issues"][0]["title"] == changed["title"]
        tools.move_issue(made["key"], "done", 0)
        assert rec.node("issues")["issues"][0]["column"] == "done"
        tools.delete_issue(made["key"])
        assert not rec.node("issues")["issues"]
        assert not saved.read()["issues"]
    finally:
        board.stop()


def test_stop_ends_the_subscription(saved, monkeypatch):
    # A reload opens the Board again; the old app must stop repainting.
    monkeypatch.setattr(vis._host, "live", vis.testing.LiveRecorder(object()).live)
    board = _board(saved).open()
    assert vis.bus.publish(CHANGED, {}) == 1
    board.stop()
    assert vis.bus.publish(CHANGED, {}) == 0


def test_store_change_refreshes_another_runtime(saved, monkeypatch):
    rec = vis.testing.LiveRecorder(object())
    monkeypatch.setattr(vis._host, "live", rec.live)
    board = _board(saved).open()
    try:
        made = tools.create_issue("Made in a different runtime")
        end = time.monotonic() + 4
        while not rec.node("issues")["issues"] and time.monotonic() < end:
            time.sleep(0.05)
        assert rec.node("issues")["issues"] == [made]
    finally:
        board.stop()


def test_app_filters_the_board_with_its_own_inputs(saved, app):
    board, rec = app
    rec.edit("issues", {"title": "Write tests", "column": "backlog", "labels": ["qa"]})
    rec.edit(
        "issues",
        {
            "title": "Ship it",
            "column": "backlog",
            "priority": "high",
            "is_flagged": True,
        },
    )
    titles = lambda: [one["title"] for one in rec.node("issues")["issues"]]  # noqa: E731
    rec.change("query", "ship")
    assert titles() == ["Ship it"]
    rec.change("query", None)
    rec.change("priority", "high")
    assert titles() == ["Ship it"]
    rec.change("priority", "any")
    rec.change("label", "qa")
    assert titles() == ["Write tests"]
    rec.change("label", "any")
    rec.change("flagged", True)
    assert titles() == ["Ship it"]
    rec.change("group", "priority")
    assert rec.node("issues")["group_by"] == "priority"
    assert {one["title"] for one in saved.read()["issues"]} == {
        "Write tests",
        "Ship it",
    }


def test_app_creates_an_issue_in_its_dialog(saved, app):
    board, rec = app
    assert rec.node("new-issue")["is_open"] is False
    rec.activate("create")
    assert rec.node("new-issue")["is_open"] is True
    assert rec.submit(
        "new-submit", {"new_title": "Plan release", "new_project": str(saved.workspace)}
    )["is_accepted"]
    assert rec.node("new-issue")["is_open"] is False
    [issue] = saved.read()["issues"]
    assert (issue["title"], issue["column"], issue.get("assignee")) == (
        "Plan release",
        "backlog",
        None,
    )
    rec.activate("create")
    rec.activate("new-issue")
    assert rec.node("new-issue")["is_open"] is False


def test_old_board_moves_its_issues_to_the_sdlc_columns(saved):
    saved.path.parent.mkdir(parents=True, exist_ok=True)
    old = [
        ("todo", "To do"),
        ("progress", "In progress"),
        ("review", "In review"),
        ("done", "Done"),
    ]
    issues = [
        {"id": str(i), "key": f"ABPR-{i}", "title": column, "column": column, "rank": 0}
        for i, (column, _) in enumerate(old)
    ]
    saved.path.write_text(
        json.dumps(
            {
                "key_prefix": "ABPR",
                "next_number": 5,
                "columns": [vis.ui.board.column(i, title) for i, title in old],
                "people": [],
                "epics": [],
                "issues": issues,
            }
        )
    )
    data = Store(saved.workspace).read()
    assert [one["id"] for one in data["columns"]] == list(store.COLUMN_IDS)
    assert {one["title"]: one["column"] for one in data["issues"]} == {
        "todo": "backlog",
        "progress": "implementing",
        "review": "verification",
        "done": "done",
    }
    assert data["workspace"] == str(saved.workspace)
    assert [one.path for one in Store.all()] == [saved.path]


def test_app_shows_the_boards_of_all_projects(saved, tmp_path, monkeypatch):
    other = tmp_path / "Other"
    other.mkdir()
    saved.change(lambda data: edit(data, {"title": "First project"}))
    Store(other).change(lambda data: edit(data, {"title": "Second project"}))
    rec = vis.testing.LiveRecorder(object())
    monkeypatch.setattr(vis._host, "live", rec.live)
    board = BoardApp(projects=lambda: []).open(watch=False)
    try:
        titles = lambda: sorted(one["title"] for one in rec.node("issues")["issues"])  # noqa: E731
        assert titles() == ["First project", "Second project"]
        rec.change("project", str(other.resolve()))
        assert titles() == ["Second project"]
        # An issue that the board makes goes to the project that it shows.
        rec.edit("issues", {"title": "Made here", "column": "ready"})
        assert sorted(one["title"] for one in Store(other).read()["issues"]) == [
            "Made here",
            "Second project",
        ]
        rec.change("project", "all")
        rec.activate("create")
        assert rec.submit(
            "new-submit", {"new_title": "Chosen", "new_project": str(saved.workspace)}
        )["is_accepted"]
        assert "Chosen" in [one["title"] for one in saved.read()["issues"]]
    finally:
        board.stop()


def _repo(folder):
    folder.mkdir(exist_ok=True)
    for args in (
        ["init", "-q", "-b", "main"],
        [
            "-c",
            "user.name=T",
            "-c",
            "user.email=t@e",
            "commit",
            "-q",
            "--allow-empty",
            "-m",
            "base",
        ],
    ):
        start.git(folder, *args)
    return folder


class _Sessions:
    """Record the agent sessions that Start makes and the requests that it sends."""

    def __init__(self):
        self.made, self.sent = [], []

    def create(self, **options):
        self.made.append(options)
        return self.get(f"s{len(self.made)}")

    def get(self, session_id):
        sent = self.sent

        class Session:
            id = session_id

            def send(self, request):
                sent.append((session_id, request))

        return Session()


def test_start_makes_a_worktree_and_an_agent_session_once(saved):
    _repo(saved.workspace)
    issue = saved.change(
        lambda data: edit(data, {"title": "Fix the login", "text": "Use OAuth."})
    )
    sessions = _Sessions()
    record = start.start(saved, issue["id"], start.Plan(), sessions=sessions)
    folder = saved.workspace.parent / "ab-12-Project-abpr-1"
    assert (
        record["workspace"] == str(folder)
        and record["branch"] == "board/abpr-1-fix-the-login"
    )
    assert start.git(folder, "rev-parse", "--abbrev-ref", "HEAD") == record["branch"]
    assert sessions.made == [
        {
            "root": str(folder),
            "title": "ABPR-1 Fix the login",
            "idempotency_key": f"vis-board-{issue['id']}-worktree",
        }
    ]
    assert "Use OAuth." in sessions.sent[0][1]
    data = saved.read()
    assert data["issues"][0]["assignee"] == "vis-agent"
    assert [one["id"] for one in data["people"]].count("vis-agent") == 1
    # A second Start reuses the worktree and the session.
    again = start.start(saved, issue["id"], start.Plan(), sessions=sessions)
    assert again == record and len(sessions.made) == 1
    assert sessions.sent[1][1].startswith("The issue is back in Implementing.")
    # A session in the worktree uses the board of the project.
    assert start.project_folder(folder) == saved.workspace


def test_start_for_a_person_on_a_branch_makes_no_session(saved):
    _repo(saved.workspace)
    issue = saved.change(
        lambda data: edit(data, {"title": "Tidy", "assignee": "owner"})
    )
    sessions = _Sessions()
    plan = start.Plan(workspace="branch", assignee="person")
    record = start.start(saved, issue["id"], plan, sessions=sessions)
    assert record["workspace"] == str(saved.workspace)
    assert (
        start.git(saved.workspace, "rev-parse", "--abbrev-ref", "HEAD")
        == "board/abpr-1-tidy"
    )
    assert not sessions.made and saved.read()["issues"][0]["assignee"] == "owner"
    folder = start.start(
        saved, issue["id"], start.Plan(workspace="folder", assignee="person")
    )
    assert folder["branch"] is None and folder["mode"] == "folder"


def test_sync_makes_an_external_issue_and_done_closes_it_once(saved, monkeypatch):
    issue = saved.change(lambda data: edit(data, {"title": "Ship"}))
    made, closed, logged = [], [], []
    monkeypatch.setitem(
        start.MAKERS,
        "github",
        lambda plan, project, one: (
            made.append(one["id"]) or {"kind": "github", "id": "u", "url": "u"}
        ),
    )
    monkeypatch.setitem(
        start.CLOSERS,
        "github",
        lambda plan, project, external: closed.append(external["id"]),
    )
    plan = start.Plan(workspace="folder", assignee="person", sync="github")
    start.start(saved, issue["id"], plan)
    start.start(saved, issue["id"], plan)
    assert made == [issue["id"]]
    start.finish(saved, issue["id"], plan)
    start.finish(saved, issue["id"], plan)
    assert closed == ["u"]
    # A failed sync is logged, and the Start goes on.
    other = saved.change(lambda data: edit(data, {"title": "Other"}))
    record = start.start(
        saved,
        other["id"],
        start.Plan(workspace="folder", assignee="person", sync="linear"),
        log=logged.append,
    )
    assert "external" not in record and "Linear sync needs" in logged[0]


def test_moving_an_issue_to_implementing_starts_it(saved, app, monkeypatch):
    board, rec = app
    steps = []
    monkeypatch.setattr(start, "plan", lambda: start.Plan(assignee="person"))
    monkeypatch.setattr(
        start,
        "start",
        lambda store, issue_id, plan, log: (
            steps.append(("start", issue_id)) or {"workspace": "/w"}
        ),
    )
    monkeypatch.setattr(
        start,
        "finish",
        lambda store, issue_id, plan, log: steps.append(("finish", issue_id)),
    )
    rec.edit("issues", {"title": "Build", "column": "ready"})
    issue = rec.node("issues")["issues"][0]
    rec.move("issues", issue["id"], "implementing", 0)
    rec.move("issues", issue["id"], "implementing", 0)
    rec.edit("issues", {"column": "done"}, issue["id"])
    assert steps == [("start", issue["id"]), ("finish", issue["id"])]
