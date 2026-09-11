"""Activity is host-owned execution evidence, not an extension-authored View."""

import json
from dataclasses import FrozenInstanceError

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts
from blockether.vis.engine import Event, ProtocolError


def test_symbol_declares_activity_without_changing_execution():
    def check():
        """Check one component."""
        return 7

    activity = vis.Activity(presenter="tests", label="Check components")
    spec = vis.Symbol(check, activity=activity)._spec()
    assert spec["activity"] == {
        "presenter": "tests",
        "label": "Check components",
        "show_start": True,
    }
    assert spec["fn"]() == 7
    with pytest.raises(FrozenInstanceError):
        activity.presenter = "shell"
    with pytest.raises(ValueError):
        vis.Activity(presenter="custom-html")
    with pytest.raises(TypeError):
        vis.Symbol(check, activity={"presenter": "tests"})


def test_method_activity_is_explicit_and_does_not_inherit_a_fake_state():
    class Checks:
        @vis.method(activity=vis.Activity(presenter="tests"))
        def check(self):
            """Check one component."""
            return True

    assert vis.Symbol(Checks(), name="checks")._spec()["methods"][0]["activity"] == {
        "presenter": "tests",
        "show_start": True,
    }
    with pytest.raises(TypeError):
        vis.Activity(state="succeeded")
    assert set(vis._ACTIVITY_PRESENTERS) == set(_contracts.ACTIVITY["presenters"])


@pytest.mark.parametrize(
    "sample",
    json.loads((_contracts._DATA / "fixtures/activity-groups.json").read_text()),
    ids=lambda sample: sample["name"],
)
def test_shared_operation_groups(sample):
    # Regression #201: SDK extension group labels must agree with Companion and TUI.
    from blockether.vis.activity import ActivityProjection

    projection = ActivityProjection.from_wire(sample["projection"])
    assert [
        {"id": group.id, "label": group.label, "rows": [row.id for row in group.rows]}
        for group in projection.groups
    ] == sample["groups"]
    assert projection.to_wire() == sample["projection"]
    if projection.groups:
        with pytest.raises(FrozenInstanceError):
            projection.groups[0].label = "changed"


@pytest.mark.parametrize(
    "sample",
    json.loads((_contracts._DATA / "fixtures/activity-arguments.json").read_text()),
    ids=lambda sample: sample["name"],
)
def test_shared_argument_groups(sample):
    from blockether.vis.activity import ActivityProjection

    projection = ActivityProjection.from_wire(sample["projection"])
    assert projection.to_wire() == sample["projection"]
    assert [
        {"id": group.id, "rows": [row.id for row in group.rows]}
        for group in projection.argument_groups
    ] == sample["groups"]
    for group in projection.groups:
        assert sum(len(item.rows) for item in group.argument_groups) == len(group.rows)
    with pytest.raises(FrozenInstanceError):
        projection.argument_groups[0].id = "changed"


def test_activity_event_uses_the_shared_fixture_and_named_records():
    from blockether.vis.activity import ActivityProjection

    fixture = json.loads((_contracts._DATA / "fixtures/activity.json").read_text())
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
    json.loads((_contracts._DATA / "fixtures/activity-cases.json").read_text()),
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


@pytest.mark.parametrize("is_async", [False, True])
@pytest.mark.parametrize("fails", [False, True])
def test_end_only_activity_skips_start_and_preserves_outcome(
    monkeypatch, is_async, fails
):
    import asyncio

    phases = []
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda value: updates.append(value) or True
    )
    failure = ValueError("Record unavailable")

    def read_record():
        """Read one record."""
        assert not phases
        if fails:
            raise failure
        return {"records": 1}

    async def read_async():
        """Read one record asynchronously."""
        return read_record()

    def render(phase, **_):
        phases.append(phase)
        return vis.ActivityPresentation(
            "Read record", "Record unavailable" if fails else "One record"
        )

    spec = vis.Symbol(
        read_async if is_async else read_record,
        activity=vis.Activity(label="Read record", show_start=False, render=render),
    )._spec()
    assert spec["activity"]["show_start"] is False

    def invoke():
        return asyncio.run(spec["fn"]()) if is_async else spec["fn"]()

    if fails:
        with pytest.raises(ValueError) as caught:
            invoke()
        assert caught.value is failure
    else:
        assert invoke() == {"records": 1}
    assert phases == ["failure" if fails else "success"]
    assert len(updates) == 1


def test_end_only_activity_preserves_cancellation_when_rendering_fails():
    import asyncio

    cancellation = asyncio.CancelledError()
    phases = []

    async def read_record():
        """Read one record."""
        raise cancellation

    def render(phase, error, **_):
        phases.append(phase)
        assert error is cancellation
        raise RuntimeError("Presentation unavailable")

    tool = vis.Symbol(
        read_record,
        activity=vis.Activity(label="Read record", show_start=False, render=render),
    )._spec()
    with pytest.raises(asyncio.CancelledError) as caught:
        asyncio.run(tool["fn"]())
    assert caught.value is cancellation
    assert phases == ["failure"]


@pytest.mark.parametrize("invalid", [None, 0, 1, "false", []])
def test_activity_start_policy_requires_a_boolean(invalid):
    with pytest.raises(TypeError, match="show_start"):
        vis.Activity(label="Read record", show_start=invalid)


def test_custom_activity_callbacks_preserve_results_and_errors(monkeypatch):
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda blocks: updates.append(blocks) or True
    )

    def render(phase, result, **_):
        return vis.ActivityPresentation(
            phase, str(result), (vis.ActivityText("Detail"),)
        )

    def check(value):
        """Check one value."""
        vis.publish_activity(
            vis.ActivityPresentation(
                "Working", "In progress", (vis.ActivityProgress("Working"),)
            )
        )
        if value < 0:
            raise ValueError("negative")
        return value + 1

    tool = vis.Symbol(check, activity=vis.Activity(render=render))._spec()
    assert tool["fn"](2) == 3
    assert [b["headline"] for b in updates] == [
        "start",
        "Working",
        "success",
    ]
    with pytest.raises(ValueError, match="negative"):
        tool["fn"](-1)
    assert updates[-1]["headline"] == "failure"
    assert "render" not in tool["activity"]

    def broken(**_):
        raise RuntimeError("presentation error")

    assert vis.Symbol(check, activity=vis.Activity(render=broken))._spec()["fn"](2) == 3


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

    tool = vis.Symbol(
        check,
        activity=vis.Activity(
            render=lambda phase, **_: vis.ActivityPresentation(phase, "One result")
        ),
    )._spec()
    assert asyncio.run(tool["fn"]()) == 9
    assert [b["headline"] for b in updates] == ["start", "success"]
    fixture = json.loads((_contracts._DATA / "fixtures/activity.json").read_text())
    fixture["rows"][0]["presentation"] = updates[-1]
    projection = ActivityProjection.from_wire(fixture)
    assert projection.to_wire() == fixture
    with pytest.raises(TypeError):
        projection.rows[0].presentation["headline"] = "changed"
    assert fixture["rows"][0]["presentation"]["headline"] == "success"
