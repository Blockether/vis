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


def test_activity_presentation_links_a_bounded_handle_without_changing_content():
    receipt = vis.ActivityPresentation(
        "Checking visual differences",
        "Comparison running",
        (vis.ActivityText("First finding"),),
        handle_id="comparison-1",
    )
    assert receipt.to_wire()["handle_id"] == "comparison-1"
    assert receipt.to_wire()["content"] == [{"type": "text", "text": "First finding"}]
    for invalid in ("", "  ", "line\nother", "x" * 513, 5):
        with pytest.raises((TypeError, ValueError)):
            vis.ActivityPresentation("Compare", "Running", handle_id=invalid)


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
    assert set(vis._ACTIVITY_PRESENTERS) == set(
        _contracts.definition("activity", "presenter")["enum"]
    )


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


def test_generic_handle_receipt_round_trips_and_groups_by_first_call():
    from blockether.vis.activity import ActivityProjection

    fixture = _history_projection(2, paged=False)
    children = [{**row, "handle_id": "compare-7"} for row in fixture["rows"]]
    fixture["rows"] = [{**children[0], "id": "group-call-0", "children": children}]
    projection = ActivityProjection.from_wire(fixture)
    assert projection.to_wire() == fixture
    assert projection.rows[0].handle_id == "compare-7"
    assert [row.handle_id for row in projection.rows[0].children] == [
        "compare-7",
        "compare-7",
    ]
    assert projection.groups[0].id == "call-0"
    assert projection.argument_groups[0].id == "call-0"


def test_activity_receipt_rejects_invalid_handle_bounds_in_rows_and_presentations():
    from blockether.vis.activity import ActivityProjection

    fixture = _history_projection(1, paged=False)
    for invalid in ("界" * 171, "a\x00b"):
        for place in ("row", "presentation"):
            row = fixture["rows"][0]
            target = row if place == "row" else row["presentation"]
            target["handle_id"] = invalid
            with pytest.raises(ValueError):
                ActivityProjection.from_wire(fixture)
            del target["handle_id"]


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


def _history_projection(count, *, paged):
    fixture = json.loads((_contracts._DATA / "fixtures/activity.json").read_text())
    first = fixture["rows"][0]
    fixture["rows"] = [
        {
            **first,
            "id": f"call-{index}",
            "sequence": index + 1,
            "state": "succeeded",
            "presentation": {
                "headline": "Read record",
                "summary": f"Record {index}",
                "content": [{"type": "text", "text": "界" * 2000}],
            },
        }
        for index in range(count)
    ]
    fixture["state"] = "succeeded"
    fixture["counts"] = {"running": 0, "succeeded": count, "failed": 0, "cancelled": 0}
    fixture["omitted"] = {"rows": 0, "by_classification": {}}
    if paged:
        fixture["history"] = {
            "id": "00000000-0000-4000-8000-000000000212",
            "revision": 480,
            "total": 160,
            "after": 0,
            "next_after": count,
        }
    return fixture


def test_activity_history_has_no_total_retention_cap():
    # Regression #212: delivery windows do not discard large inline histories.
    from blockether.vis.activity import ActivityProjection

    fixture = _history_projection(160, paged=False)
    assert len(json.dumps(fixture, ensure_ascii=False).encode()) > 65536
    projection = ActivityProjection.from_wire(fixture)
    assert len(projection.rows) == 160
    assert projection.to_wire() == fixture


@pytest.mark.parametrize("next_after", [32, None])
def test_activity_page_roundtrips_history_and_nullable_end(next_after):
    from blockether.vis.activity import ActivityProjection

    fixture = _history_projection(32, paged=True)
    fixture["history"]["next_after"] = next_after
    projection = ActivityProjection.from_wire(fixture)
    assert projection.to_wire() == fixture
    assert projection.history["total"] == 160
    with pytest.raises(TypeError):
        projection.history["after"] = 10
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


def test_activity_page_counts_real_invocations_not_synthetic_groups():
    from blockether.vis.activity import ActivityProjection

    fixture = _history_projection(32, paged=True)
    fixture["rows"] = [
        {**fixture["rows"][0], "id": "group-1", "children": fixture["rows"]}
    ]
    assert ActivityProjection.from_wire(fixture).to_wire() == fixture
    fixture["rows"][0]["children"].append(
        {**fixture["rows"][0]["children"][0], "id": "call-33"}
    )
    with pytest.raises(ValueError):
        ActivityProjection.from_wire(fixture)


@pytest.mark.parametrize(
    "content",
    [
        [{"type": "code", "text": "界" * 20000}],
        [{"type": "text", "text": str(index)} for index in range(40)],
        [{"type": "table", "columns": ["Field"], "rows": [["x" * 300]] * 201}],
        [{"type": "table", "columns": [str(index) for index in range(17)], "rows": []}],
    ],
)
def test_complete_large_activity_content_roundtrips(content):
    # Regression #218: a complete result must not disappear at presentation admission.
    from blockether.vis.activity import ActivityProjection

    fixture = _history_projection(1, paged=False)
    fixture["rows"][0]["presentation"]["content"] = content
    assert ActivityProjection.from_wire(fixture).to_wire() == fixture


def test_large_typed_presentation_publishes_all_content(monkeypatch):
    # Regression #218: typed SDK values retain complete text, tables and sections.
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda value: updates.append(value) or True
    )
    text = "界" * 20000
    table = vis.ActivityTable(["Column" * 50] * 17, [["Cell" * 100] * 17] * 201)
    blocks = [vis.ActivityCode(text)] * 40 + [table]
    sections = [
        vis.ActivitySection(f"Section {index}", "", blocks) for index in range(9)
    ]
    view = vis.ActivityPresentation(
        "Complete results", "Nine sections", blocks, sections
    )
    assert vis.publish_activity(view)
    assert updates == [view.to_wire()]
    assert updates[0]["content"][0]["text"] == text
    assert updates[0]["sections"][-1]["content"][-1]["rows"][-1][-1] == "Cell" * 100
    assert _contracts.validate("activity", "presentation", updates[0]) == updates[0]


def test_activity_table_rows_carry_the_paths_that_open_them():
    # BLO-172: a listing printed file names nobody could open. One path per row
    # lets a client open that row's file; a row without one keeps plain words.
    table = vis.ActivityTable(
        ["Name", "Kind"],
        [["core.clj", "File"], ["util", "Directory"]],
        ["/w/src/core.clj", ""],
    )
    view = vis.ActivityPresentation("Listed directory", "1 file", [table])
    wire = view.to_wire()
    assert wire["content"][0]["paths"] == ["/w/src/core.clj", ""]
    assert _contracts.validate("activity", "presentation", wire) == wire
    assert "paths" not in vis.ActivityTable(["Name"], [["core.clj"]]).to_wire()

    with pytest.raises(ValueError):
        vis.ActivityTable(["Name"], [["a"], ["b"]], ["/w/a"])
    with pytest.raises(ValueError):
        vis.ActivityTable(["Name"], [["a"]], [7])


def test_single_large_activity_invocation_roundtrips_as_one_page():
    # Regression #218: a page budget cannot discard an indivisible invocation.
    from blockether.vis.activity import ActivityProjection

    fixture = _history_projection(1, paged=True)
    fixture["rows"][0]["presentation"]["content"] = [
        {"type": "code", "text": "界" * 400000}
    ]
    assert ActivityProjection.from_wire(fixture).to_wire() == fixture
    fixture["rows"].append({**fixture["rows"][0], "id": "second", "sequence": 2})
    with pytest.raises(ValueError):
        ActivityProjection.from_wire(fixture)
