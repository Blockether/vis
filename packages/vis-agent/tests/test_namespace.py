"""The two distributions share only a PEP 420 parent, not global import names."""

import importlib
import sys
import types
from dataclasses import fields, is_dataclass
from importlib.resources import files

import pytest
from blockether.vis.client import Event, ProtocolError, Response, Session, Turn


def test_namespace_coexists_with_unrelated_modules(monkeypatch):
    unrelated = types.ModuleType("vis")
    sibling = types.ModuleType("blockether.unrelated")
    monkeypatch.setitem(sys.modules, "vis", unrelated)
    monkeypatch.setitem(sys.modules, "blockether.unrelated", sibling)
    parent = importlib.import_module("blockether")
    sdk = importlib.import_module("blockether.vis")
    contract = importlib.import_module("blockether.vis_contract")
    assert parent.__spec__.origin is None
    assert sdk.__name__ == "blockether.vis"
    assert contract.GATEWAY["routes"]
    assert files(sdk).joinpath("py.typed").is_file()
    assert files(contract).joinpath("py.typed").is_file()
    assert sys.modules["vis"] is unrelated
    assert sys.modules["blockether.unrelated"] is sibling


def test_sdk_records_are_frozen_slotted_dataclasses():
    for cls in (Event, Response, Session, Turn):
        assert is_dataclass(cls)
        assert cls.__dataclass_params__.frozen
        assert "__dict__" not in cls.__dict__
        assert all(f.type for f in fields(cls))
    event = Event.from_wire(
        {
            "type": "turn.delta",
            "session_id": "s",
            "seq": 1,
            "turn_id": "t",
            "delta": "hello",
        }
    )
    assert (event.type, event.session_id, event.seq, event.turn_id) == (
        "turn.delta",
        "s",
        1,
        "t",
    )
    assert event.data == {"delta": "hello"}


@pytest.mark.parametrize(
    "changes",
    [
        {"seq": True},
        {"seq": -1},
        {"seq": "1"},
        {"type": ""},
        {"session_id": None},
        {"turn_id": 1},
    ],
)
def test_event_boundary_rejects_invalid_envelopes(changes):
    with pytest.raises(ProtocolError):
        Event.from_wire({"type": "turn.delta", "session_id": "s", "seq": 1, **changes})
