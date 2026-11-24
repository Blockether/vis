"""The extension door: which sessions record a journal and open the app."""

import importlib.util
import os
import time
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
            self.view = None

        def open(self, *, checkpoint=True):
            if checkpoint:
                self.journal.checkpoint("manual", "Review opened")
            opened.append(self.session_id)
            self.view = object()
            module.RECORDER.keep(self.session_id, self)

        def is_behind(self):
            return False

        def refresh(self):
            pass

    monkeypatch.setattr(module, "_app", App)
    monkeypatch.setattr(module, "opened", opened, raising=False)
    monkeypatch.setattr(module, "root", root, raising=False)
    return module


def step(door, monkeypatch, session_id, record, rel=None):
    """One block of `session_id` and its after hook."""
    monkeypatch.setitem(vis.__dict__, "session", Session(session_id, record))
    start = time.time()
    if rel:
        path = door.root / rel
        path.write_text(path.read_text() + session_id + "\n")
    door.after_step(
        {
            "op": "py3",
            "args": [{"code": f"work({rel!r})"}],
            "session_id": session_id,
            "started_at": start,
            "ended_at": time.time(),
            "turn": 1,
            "iteration": 2,
            "call_id": f"call-{session_id}",
        }
    )


LEADER = {"agent": {"role": "leader", "session_id": "L", "leader_id": "L"}}
SUBAGENT = {"agent": {"role": "subagent", "session_id": "S", "leader_id": "L"}}


def test_an_open_cut_short_opens_again_at_the_next_step(door, monkeypatch):
    # Session f183fc01: the 300 s limit cut the first open short, and the app
    # stayed without a view: later blocks never opened Review again.
    made = door._app

    class CutOnce(made):
        cut = []

        def open(self, *, checkpoint=True):
            if not self.cut:
                self.cut.append(self.session_id)
                door.RECORDER.keep(self.session_id, self)
                raise KeyboardInterrupt
            super().open(checkpoint=checkpoint)

    monkeypatch.setattr(door, "_app", CutOnce)
    with pytest.raises(KeyboardInterrupt):
        step(door, monkeypatch, "L", LEADER)
    step(door, monkeypatch, "L", LEADER)
    assert door.opened == ["L"]


def test_a_subagent_records_for_its_leader_and_opens_no_app(door, monkeypatch):
    # User request: review is not available in subagents, and the review of the
    # leader shows the changes of its subagents.
    step(door, monkeypatch, "L", LEADER)
    step(door, monkeypatch, "S", SUBAGENT)
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


def test_auto_open_leaves_a_newer_edit_for_the_next_hook(door, monkeypatch):
    monkeypatch.setitem(vis.__dict__, "session", Session("L", LEADER))
    path = door.root / "base.py"
    os.utime(path, (11, 11))
    door.after_step({"session_id": "L", "started_at": 1, "ended_at": 2})
    assert door.opened == ["L"]
    assert door.RECORDER.journal("L", door.root).state == {}
    door.after_step(
        {"session_id": "L", "started_at": 10, "ended_at": 12, "call_id": "next"}
    )
    [entry] = door.RECORDER.journal("L", door.root).snapshots()
    assert entry.kind == "block" and entry.call == "next"
