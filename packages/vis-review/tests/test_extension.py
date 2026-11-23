"""The extension door: which sessions record a journal and open the app."""

import importlib.util
import os
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from vis_lang_interface.tree import walked

from vis_review.recorder import Recorder

_SPEC = importlib.util.spec_from_file_location(
    "vis_review_door", Path(__file__).resolve().parents[1] / "extension.py"
)
# The extension registers itself once per process.
DOOR = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(DOOR)


class Session:
    def __init__(self, session_id, record):
        self.id = session_id
        self._record = record

    def read(self):
        return self._record


@pytest.fixture
def door(tmp_path, monkeypatch):
    """The extension module with its journals under `tmp_path` and a record of opened apps."""
    module = DOOR
    root = tmp_path / "root"
    root.mkdir()
    (root / "base.py").write_text("b\n")
    monkeypatch.setattr(module, "RECORDER", Recorder(tmp_path / "base", files=walked))
    monkeypatch.setattr(module, "_KEYS", {})
    monkeypatch.setitem(vis.__dict__, "workspace_root", lambda: root)
    opened = []

    class App:
        def __init__(self, session_id, journal):
            self.session_id = session_id
            self.journal = journal

        def open(self):
            opened.append(self.session_id)
            module.RECORDER.keep(self.session_id, self)

    monkeypatch.setattr(module, "_app", App)
    monkeypatch.setattr(module, "opened", opened, raising=False)
    monkeypatch.setattr(module, "root", root, raising=False)
    return module


def step(door, monkeypatch, session_id, record, rel=None):
    """One block of `session_id`: the hook before it, a write of `rel`, the hook after it."""
    monkeypatch.setitem(vis.__dict__, "session", Session(session_id, record))
    door.before_block({"op": "py3"})
    if rel:
        path = door.root / rel
        path.write_text(path.read_text() + session_id + "\n")
        stat = os.stat(path)
        os.utime(path, ns=(stat.st_atime_ns, stat.st_mtime_ns + 1_000_000_000))
    door.after_step({"op": "py3", "args": [{"code": "work()"}]})


LEADER = {"agent": {"role": "leader", "session_id": "L", "leader_id": "L"}}
SUBAGENT = {"agent": {"role": "subagent", "session_id": "S", "leader_id": "L"}}


def test_a_subagent_records_for_its_leader_and_opens_no_app(door, monkeypatch):
    # User request: review is not available in subagents, and the review of the
    # leader shows the changes of its subagents.
    step(door, monkeypatch, "L", LEADER)
    step(door, monkeypatch, "S", SUBAGENT, "base.py")
    assert door.opened == ["L"]
    step(door, monkeypatch, "L", LEADER)
    leader = door.RECORDER.journal("L", door.root)
    assert leader.own_paths() == ["base.py"]
    assert [e.kind for e in leader.entries] == ["baseline", "team"]


def test_a_session_of_the_ai_review_records_nothing(door, monkeypatch):
    record = {
        "agent": {"role": "leader", "session_id": "R", "leader_id": "R"},
        "origin": {"kind": "extension", "extension": "vis-review", "session_id": "L"},
    }
    step(door, monkeypatch, "R", record, "base.py")
    assert door.opened == []
    assert not list(
        (door.RECORDER.journal("L", door.root).path.parent).glob("R*.jsonl")
    )


def test_a_subagent_does_not_open_a_review_app_that_it_saved_before(door, monkeypatch):
    monkeypatch.setitem(vis.__dict__, "session", Session("S", SUBAGENT))
    door.open_review({"session_id": "S", "key": "review"})
    door.reopen({"session_id": "S", "key": "review"})
    assert door.opened == []
