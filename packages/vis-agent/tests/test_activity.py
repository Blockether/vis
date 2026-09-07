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
