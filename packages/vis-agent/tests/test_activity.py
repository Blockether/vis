"""Activity is host-owned execution evidence, not an extension-authored View."""

import json
from dataclasses import FrozenInstanceError

import pytest
from blockether import vis, vis_contract
from blockether.vis.client import Event, ProtocolError


def test_symbol_declares_activity_without_changing_execution():
    def check():
        """Check one component."""
        return 7

    activity = vis.Activity(presenter="tests", label="checking components")
    spec = vis.symbol(check, activity=activity)
    assert spec["activity"] == {"presenter": "tests", "label": "checking components"}
    assert spec["fn"]() == 7
    with pytest.raises(FrozenInstanceError):
        activity.presenter = "shell"
    with pytest.raises(ValueError):
        vis.Activity(presenter="custom-html")
    with pytest.raises(TypeError):
        vis.symbol(check, activity={"presenter": "tests"})


def test_method_activity_is_explicit_and_does_not_inherit_a_fake_state():
    class Checks:
        @vis.method(activity=vis.Activity(presenter="tests"))
        def check(self):
            """Check one component."""
            return True

    assert vis.symbol(Checks(), name="checks")["methods"][0]["activity"] == {
        "presenter": "tests"
    }
    with pytest.raises(TypeError):
        vis.Activity(state="succeeded")
    assert set(vis._ACTIVITY_PRESENTERS) == set(vis_contract.ACTIVITY["presenters"])


def test_activity_event_uses_the_shared_fixture_and_named_records():
    from blockether.vis.activity import ActivityProjection

    fixture = json.loads((vis_contract._DATA / "fixtures/activity.json").read_text())
    projection = ActivityProjection.from_wire(fixture)
    assert projection.to_wire() == fixture
    frame = Event.from_wire(
        {
            "type": "block.activity",
            "session_id": "s",
            "turn_id": "t",
            "seq": 3,
            "iteration": 1,
            "form_index": 0,
            "activity": fixture,
        }
    )
    assert frame.activity == projection
    assert frame.activity.rows[0].operation
    with pytest.raises(FrozenInstanceError):
        projection.state = "failed"
    for malformed in [
        dict(fixture, schema_version=1),
        dict(fixture, anchor={}),
        dict(fixture, rows=[fixture["rows"][0], fixture["rows"][0]]),
    ]:
        with pytest.raises(ValueError):
            ActivityProjection.from_wire(malformed)
        with pytest.raises(ProtocolError):
            Event.from_wire(
                {
                    "type": "block.activity",
                    "session_id": "s",
                    "seq": 4,
                    "iteration": 1,
                    "form_index": 0,
                    "activity": malformed,
                }
            )


@pytest.mark.parametrize(
    "sample",
    json.loads((vis_contract._DATA / "fixtures/activity-cases.json").read_text()),
    ids=lambda sample: sample["name"],
)
def test_shared_activity_admission(sample):
    from blockether.vis.activity import ActivityProjection

    if sample["valid"]:
        assert (
            ActivityProjection.from_wire(sample["projection"]).to_wire()
            == sample["projection"]
        )
    else:
        with pytest.raises(ValueError):
            ActivityProjection.from_wire(sample["projection"])


def test_custom_activity_callbacks_preserve_results_and_errors(monkeypatch):
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda blocks: updates.append(blocks) or True
    )

    def render(phase, result, **_):
        return [
            {"type": "heading", "text": phase},
            {"type": "text", "text": str(result)},
        ]

    def check(value):
        """Check one value."""
        vis.publish_activity({"type": "progress", "label": "Working"})
        if value < 0:
            raise ValueError("negative")
        return value + 1

    tool = vis.symbol(check, activity=vis.Activity(render=render))
    assert tool["fn"](2) == 3
    assert [b[0].get("text", "Working") for b in updates] == [
        "start",
        "Working",
        "success",
    ]
    with pytest.raises(ValueError, match="negative"):
        tool["fn"](-1)
    assert updates[-1][0]["text"] == "failure"
    assert "render" not in tool["activity"]

    def broken(**_):
        raise RuntimeError("presentation error")

    assert vis.symbol(check, activity=vis.Activity(render=broken))["fn"](2) == 3


def test_custom_activity_async_and_wire_roundtrip(monkeypatch):
    import asyncio

    from blockether.vis.activity import ActivityProjection

    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda blocks: updates.append(blocks) or True
    )

    async def check():
        """Check asynchronously."""
        return 9

    tool = vis.symbol(
        check,
        activity=vis.Activity(
            render=lambda phase, **_: [{"type": "text", "text": phase}]
        ),
    )
    assert asyncio.run(tool["fn"]()) == 9
    assert [b[0]["text"] for b in updates] == ["start", "success"]
    fixture = json.loads((vis_contract._DATA / "fixtures/activity.json").read_text())
    fixture["rows"][0]["content"] = updates[-1]
    projection = ActivityProjection.from_wire(fixture)
    assert projection.to_wire() == fixture
    with pytest.raises(TypeError):
        projection.rows[0].content[0]["text"] = "changed"
    assert fixture["rows"][0]["content"][0]["text"] == "success"
