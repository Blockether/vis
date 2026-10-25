"""Swarm Forge runs its pipeline, board and restore against fake role sessions."""

import importlib.util
import json
import sys
from pathlib import Path
from types import SimpleNamespace

import blockether.vis.extension as vis
import pytest

DONE = '{{"status": "done", "summary": "{summary}", "commit": "{commit}", "branch": "{branch}"}}'


class FakeSession:
    def __init__(self, session_id, options):
        self.id = session_id
        self.options = options
        self.sent = []
        self.records = {}
        self.views = []
        self.answers = []
        self.models = []
        self.cancelled = []

    def send(self, text):
        turn_id = f"{self.id}-turn-{len(self.sent) + 1}"
        self.sent.append(text)
        self.records[turn_id] = {"status": "running"}
        return SimpleNamespace(id=turn_id)

    def finish(self, content, status="completed"):
        last = f"{self.id}-turn-{len(self.sent)}"
        self.records[last] = {"status": status, "content": content}

    def done(self, summary, commit, branch):
        self.finish(DONE.format(summary=summary, commit=commit, branch=branch))

    def input_views(self):
        return list(self.views)

    def answer(self, view_id, values):
        self.answers.append((view_id, values))
        self.views = []

    def set_model(self, provider, model):
        self.models.append((provider, model))


class FakeSessions:
    def __init__(self):
        self.made = {}

    def create(self, provider=None, model=None, **options):
        session = FakeSession(
            f"S{len(self.made) + 1}", dict(options, provider=provider, model=model)
        )
        self.made[session.id] = session
        return session

    def get(self, session_id):
        return self.made[session_id]

    def models(self, is_available=None):
        return (
            SimpleNamespace(provider="cheap", name="mini", label="Mini"),
            SimpleNamespace(provider="cheap", name="midi", label="Midi"),
        )


class Host:
    """One state store and one live recorder for each view key."""

    def __init__(self, inner):
        self.inner = inner
        self.store = {}
        self.by_key = {}
        self.by_view = {}

    def __getattr__(self, name):
        return getattr(self.inner, name)

    def state_get(self, key):
        value = self.store.get(key)
        return None if value is None else json.loads(json.dumps(value))

    def state_put(self, key, value):
        if value is None:
            self.store.pop(key, None)
        else:
            self.store[key] = json.loads(json.dumps(value))
        return value

    def state_del(self, key):
        self.store.pop(key, None)

    def state_keys(self):
        return list(self.store)

    def live(self, envelope_json, listener=None):
        envelope = json.loads(envelope_json)
        if envelope.get("op") == "open":
            key = envelope["view"]["key"]
            recorder = self.by_key.get(key) or vis.testing.LiveRecorder(
                self.inner, view_id=f"view-{key}"
            )
            self.by_key[key] = recorder
            self.by_view[recorder.view_id] = recorder
        else:
            recorder = self.by_view[envelope["view_id"]]
        return recorder.live(envelope_json, listener)

    @property
    def board(self):
        return self.by_key["swarm-board"]


def _load(monkeypatch, name="swarm_forge_fixture"):
    spec = importlib.util.spec_from_file_location(
        name, Path(__file__).with_name("swarm_forge.py")
    )
    module = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, name, module)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    spec.loader.exec_module(module)
    monkeypatch.setattr(module, "start_worker", lambda swarm_id: None)
    monkeypatch.setattr(
        module, "_read_turn", lambda session, turn_id: session.records[turn_id]
    )
    monkeypatch.setattr(
        module,
        "_cancel_turn",
        lambda session, turn_id: session.cancelled.append(turn_id),
    )
    return module


@pytest.fixture
def world(monkeypatch):
    host = Host(vis._host)
    sessions = FakeSessions()
    monkeypatch.setattr(vis, "_host", host)
    monkeypatch.setattr(vis, "sessions", sessions)
    module = _load(monkeypatch)
    module.start("LEADER", "two-pack")
    return SimpleNamespace(host=host, sessions=sessions, sf=module)


def _roles(world):
    return world.sessions.get("S1"), world.sessions.get("S2")


def _add(world, title="Add greet()"):
    world.host.board.change("task-title", title)
    assert world.host.board.submit("add-task")["is_accepted"]


def _state(world):
    return world.host.store["swarm:LEADER"]


def _task(world):
    return _state(world)["tasks"][0]


def test_the_pipeline_moves_a_task_through_each_role_and_lands_it(world):
    coder, cleaner = _roles(world)
    assert coder.options["draft_backend"] == "worktree"
    assert cleaner.options["draft_backend"] == "worktree"
    board = world.host.board
    assert board.said[0]["view"]["placement"] == "panel"
    assert (
        world.host.by_key["swarm-pin-LEADER"].said[0]["view"]["placement"] == "header"
    )
    _add(world)
    assert "## Task T1: Add greet()" in coder.sent[0]
    assert board.node("task-title").get("value") is None, "the form clears after a task"
    assert board.node("card:coder:T1")["text"] == "Working"
    coder.done("Added greet", "abc1234", "vis/swarm-coder")
    world.sf.tick("LEADER")
    assert "git merge --no-edit abc1234" in cleaner.sent[0]
    assert "Coder: Added greet" in cleaner.sent[0]
    cleaner.done("Tidied", "def5678", "vis/swarm-cleaner")
    world.sf.tick("LEADER")
    assert _task(world)["state"] == "review"
    job = _state(world)["jobs"][0]["id"]
    assert board.node(f"att-card:{job}")["text"] == "Tidied"
    board.activate(f"approve:{job}")
    assert 'draft_approve("feat: Add greet()")' in cleaner.sent[1]
    cleaner.done("Landed", "0aa1234", "vis/swarm-cleaner")
    world.sf.tick("LEADER")
    assert _task(world)["state"] == "landed"
    assert _task(world)["commit"] == "0aa1234"
    pin = world.host.by_key["swarm-pin-LEADER"].node("pin")
    assert pin["text"] == "1 landed"
    stats = {item["id"]: item for item in board.node("summary")["stats"]}
    assert (stats["landed"]["value_text"], stats["landed"]["tone"]) == ("1", "ok")


def test_a_bad_answer_gets_one_correction_then_blocks_and_retries(world):
    coder, _ = _roles(world)
    _add(world)
    coder.finish("I am done.")
    world.sf.tick("LEADER")
    assert "no valid final answer" in coder.sent[1]
    coder.finish('{"status": "done", "summary": "x"}')
    world.sf.tick("LEADER")
    assert _task(world)["state"] == "blocked"
    job = _state(world)["jobs"][0]["id"]
    assert "commit" in world.host.board.node(f"att-card:{job}")["text"]
    world.host.board.activate(f"retry:{job}")
    assert len(coder.sent) == 3 and "## Task T1" in coder.sent[2]
    assert _task(world)["state"] == "working"


def test_a_rejection_sends_the_task_back_with_the_comment(world):
    coder, cleaner = _roles(world)
    _add(world)
    coder.done("Added", "abc1234", "vis/swarm-coder")
    world.sf.tick("LEADER")
    cleaner.done("Tidied", "def5678", "vis/swarm-cleaner")
    world.sf.tick("LEADER")
    job = _state(world)["jobs"][0]["id"]
    world.host.board.change(f"comment:{job}", "Use a docstring.")
    assert world.host.board.submit(f"reject:{job}")["is_accepted"]
    assert _task(world)["stage"] == 0, "the cleaner sends back one role"
    world.sf.tick("LEADER")
    assert "rejected the result: Use a docstring." in coder.sent[1]
    coder.done("Added docstring", "abc9999", "vis/swarm-coder")
    world.sf.tick("LEADER")
    assert "git merge --no-edit abc9999" in cleaner.sent[1]


def test_clarifications_reach_the_board_and_go_back_to_the_role(world):
    coder, _ = _roles(world)
    _add(world)
    coder.views = [
        SimpleNamespace(
            id="input-1",
            title="Pick a database",
            description=None,
            nodes=({"type": "plaintext", "name": "db", "label": "Database"},),
            submit_label="Send",
        )
    ]
    world.sf.tick("LEADER")
    job = _state(world)["jobs"][0]["id"]
    board = world.host.board
    board.change(f"q-{job}-0-db", "postgres")
    assert board.submit(f"form:{job}:0")["is_accepted"]
    assert coder.answers == [("input-1", {"db": "postgres"})]
    coder.finish(
        '{"status": "question", "summary": "Unsure", "question": "Which name?"}'
    )
    world.sf.tick("LEADER")
    assert board.node(f"att-card:{job}")["text"] == "Which name?"
    board.change(f"answer:{job}", "greet")
    assert board.submit(f"reply:{job}")["is_accepted"]
    assert "The person answered your question:\n\ngreet" in coder.sent[1]


def test_teardown_asks_first_then_cancels_and_closes(world):
    coder, _ = _roles(world)
    _add(world)
    board = world.host.board
    board.activate("teardown")
    assert board.node("teardown-confirm")["label"] == "Confirm teardown"
    board.activate("teardown-cancel")
    board.activate("teardown")
    board.activate("teardown-confirm")
    assert coder.cancelled == ["S1-turn-1"]
    assert "swarm:LEADER" not in world.host.store
    assert board._result is not None, "the board closed"
    assert world.sf.tick("LEADER") is False


def test_a_restart_opens_the_same_board_again(world, monkeypatch):
    _add(world)
    world.host.board.said.clear()
    again = _load(monkeypatch, "swarm_forge_again")
    opened = world.host.board.said[0]["view"]
    assert (opened["key"], opened["session_id"]) == ("swarm-board", "LEADER")
    assert world.host.board.node("card:coder:T1")["text"] == "Working"
    assert again.load("LEADER")["tasks"][0]["id"] == "T1"


def test_a_role_model_change_pins_the_role_session(world):
    _, cleaner = _roles(world)
    board = world.host.board
    board.change("provider:cleaner", "cheap")
    assert cleaner.models == [("cheap", "mini")]
    board.change("model:cleaner", "midi")
    assert cleaner.models[-1] == ("cheap", "midi")
    options = [one["value"] for one in board.node("model:cleaner")["options"]]
    assert options == ["default", "mini", "midi"]
    board.change("provider:cleaner", "default")
    assert cleaner.models[-1] == (None, None)


@pytest.mark.parametrize(
    "text, error",
    [
        ("no json", "does not end with a JSON object"),
        ('{"status": "done", "summary": "s", "commit": "abc1234"}', "branch"),
        ('{"status": "question", "summary": "s"}', "question"),
    ],
)
def test_parse_answer_names_the_missing_field(world, text, error):
    with pytest.raises(ValueError, match=error):
        world.sf.parse_answer(text)


def test_parse_answer_reads_the_reply_blocks_of_a_turn(world):
    # A turn record gives its reply as a list of blocks, not as one string.
    content = [
        {"id": "b1", "type": "prose", "markdown": "Added farewell()."},
        {
            "id": "b2",
            "type": "prose",
            "markdown": '```json\n{"status": "done", "summary": "s", '
            '"commit": "0eb2853", "branch": "vis/swarm-coder"}\n```',
        },
    ]
    answer = world.sf.parse_answer(content)
    assert (answer.status, answer.commit) == ("done", "0eb2853")


def test_the_worker_survives_a_failed_tick(world, monkeypatch):
    # A thread that dies on one exception leaves the board on "Working" for good.
    results = iter([RuntimeError("read failed"), True, False])
    calls = []

    def tick(swarm_id):
        calls.append(swarm_id)
        result = next(results)
        if isinstance(result, Exception):
            raise result
        return result

    monkeypatch.setattr(world.sf, "TICK_S", 0)
    monkeypatch.setattr(world.sf, "tick", tick)
    monkeypatch.setattr(world.sf.vis, "log", lambda level, msg: calls.append(level))
    panel = SimpleNamespace(is_interrupted=False)
    world.sf._worker("LEADER", panel)
    assert calls == ["LEADER", "warn", "LEADER", "LEADER"]


def test_every_pack_loads(world):
    assert world.sf.pack_names() == ["four-pack", "six-pack", "two-pack"]
    for name in world.sf.pack_names():
        roles = world.sf.load_pack(name)["roles"]
        assert len(roles) == int({"two": 2, "four": 4, "six": 6}[name.split("-")[0]])


def test_swarm_tools_declare_quick_natural_language_activities(world):
    from blockether.vis import _contracts

    exported = vis._registration["spec"]["symbols"][0]["methods"]
    samples = {
        "add_task": {"id": "T1", "title": "🦉" * 300},
        "status": {"summary": "1 open", "roles": [], "tasks": []},
    }
    assert {entry["name"] for entry in exported} == set(samples)
    for entry in exported:
        declaration = getattr(world.sf.swarm, entry["name"]).__vis_symbol_activity__
        # Both tools read or change local state only, so they show only the end.
        assert declaration.show_start is False
        assert entry["activity"]["show_start"] is False
        assert declaration.render(phase="start", result=None) is None
        view = declaration.render(phase="success", result=samples[entry["name"]])
        assert view.headline == declaration.label
        assert _contracts.validate("activity", "presentation", view.to_wire())
