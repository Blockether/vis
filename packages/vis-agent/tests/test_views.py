"""View lifecycle admission uses the same contract as the engine and UI."""

import copy
import json
from dataclasses import FrozenInstanceError, fields

import pytest
from blockether import vis, vis_contract
from blockether.vis.client import Event, ProtocolError


def fixtures():
    return json.loads((vis_contract._DATA / "fixtures/view.json").read_text())


def test_view_records_are_canonical_immutable_and_roundtrip():
    from blockether.vis.views import (
        InputResult,
        InputView,
        LivePatch,
        LiveResult,
        LiveView,
    )

    samples = fixtures()
    for name, model in (
        ("input", InputView),
        ("live", LiveView),
        ("patch", LivePatch),
        ("input_result", InputResult),
        ("result", LiveResult),
    ):
        sample = samples[name]
        value = model.from_wire(sample)
        assert value.to_wire() == sample
        with pytest.raises(FrozenInstanceError):
            setattr(value, fields(value)[0].name, "invalid")
        with pytest.raises(ValueError):
            model.from_wire({**sample, "legacy": True})
    live = LiveView.from_wire(samples["live"])
    with pytest.raises(TypeError):
        live.nodes[0]["text"] = "different"
    assert LiveResult.from_wire(samples["result"]).view.title == "Build"


def test_view_events_validate_kind_identity_and_patch_sequence():
    samples = fixtures()
    base = {"session_id": "s", "turn_id": "t", "seq": 5}
    payloads = [
        {
            "type": "view.open",
            "kind": "input",
            "view_id": "input-one",
            "view": samples["input"],
        },
        {
            "type": "view.open",
            "kind": "live",
            "view_id": "live-one",
            "view": samples["live"],
        },
        {
            "type": "view.patch",
            "kind": "live",
            "view_id": "live-one",
            "first_seq": 1,
            "patch": samples["patch"],
        },
        {
            "type": "view.close",
            "kind": "input",
            "view_id": "input-one",
            "result": samples["input_result"],
        },
        {
            "type": "view.close",
            "kind": "live",
            "view_id": "live-one",
            "result": samples["result"],
        },
    ]
    for payload in payloads:
        event = Event.from_wire({**base, **payload})
        assert event.view.to_wire() == {k: v for k, v in payload.items() if k != "type"}
        assert not set(event.data) & {
            "kind",
            "view_id",
            "view",
            "patch",
            "result",
            "first_seq",
        }
        invalid = [{"kind": "legacy"}, {"view_id": ""}]
        if not (payload["type"] == "view.close" and payload["kind"] == "input"):
            invalid.append({"view_id": "wrong"})
        for changes in invalid:
            with pytest.raises(ProtocolError):
                Event.from_wire({**base, **payload, **changes})
    with pytest.raises(ProtocolError):
        Event.from_wire({**base, **payloads[2], "first_seq": 2})
    malformed = copy.deepcopy(payloads[-1])
    malformed["result"]["elided"] = {}
    with pytest.raises(ProtocolError):
        Event.from_wire({**base, **malformed})


def test_view_actions_are_closed_and_recorder_emits_a_real_result():
    from blockether.vis.views import LiveResult

    valid = [
        {"action": "submit", "values": {"name": "Ada"}},
        {"action": "cancel"},
        {"action": "select", "node_id": "table", "item_ids": ["one"]},
        {"action": "interrupt", "note": "Stop"},
    ]
    for action in valid:
        assert vis_contract.validate("view", "operator_action", action) is action
        with pytest.raises(ValueError):
            vis_contract.validate("view", "operator_action", {**action, "legacy": True})
    for action in (
        {"action": "submit"},
        {"action": "select", "node_id": "table", "item_ids": None},
        {"action": "interrupt", "note": 42},
        {"action": "dismiss"},
    ):
        with pytest.raises(ValueError):
            vis_contract.validate("view", "operator_action", action)
    recorder = vis.testing.LiveRecorder(vis._host)
    recorder.host_live(
        json.dumps(
            {
                "op": "open",
                "view": {
                    "title": "Build",
                    "nodes": [
                        {
                            "type": "status",
                            "id": "s",
                            "text": "Working",
                            "tone": "running",
                        }
                    ],
                },
            }
        )
    )
    response = json.loads(recorder.host_live(json.dumps({"op": "close", "ending": {}})))
    assert LiveResult.from_wire(response["result"]).elided == ()


def test_public_close_receipts_do_not_require_or_expose_host_answers():
    from blockether.vis.views import InputResult, LiveResult

    samples = fixtures()
    assert InputResult.from_wire(samples["input_result"]).reason == "submitted"
    with pytest.raises(ValueError):
        InputResult.from_wire(samples["answer"])
    without_picture = {k: v for k, v in samples["result"].items() if k != "view"}
    receipt = LiveResult.from_wire(without_picture)
    assert receipt.view is None
    assert receipt.to_wire() == without_picture
    with pytest.raises(ValueError):
        LiveResult.from_wire({**without_picture, "view": None})
