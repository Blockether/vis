"""The engine and the SDK checked a live view in two ways, so one view opened in one
host and failed in the other (#453). Both hosts run every case of
`vis-contract/fixtures/live-view-checks.json` and must give its answer."""

import json
import math
import re

import pytest
from blockether.vis import _contracts

_FIXTURE = json.loads(
    (_contracts._DATA / "fixtures" / "live-view-checks.json").read_text(
        encoding="utf-8"
    )
)
_CASES = _FIXTURE["cases"]
_PATCHES = _FIXTURE["patches"]


def _with_nan(case):
    # JSON has no NaN, so the case names the path that holds it.
    view = json.loads(json.dumps(case["view"]))
    if at := case.get("nan_at"):
        target = view
        for part in at[:-1]:
            target = target[part]
        target[at[-1]] = math.nan
    return view


def _answer(case):
    faults = _contracts.live_view_faults(_with_nan(case))
    if faults.view:
        answer = {"class": "view", "cards": [], "text": faults.view}
    elif faults.nodes:
        answer = {
            "class": "node",
            "cards": list(faults.nodes),
            "text": list(faults.nodes.values()),
        }
    else:
        answer = {"class": "ok", "cards": [], "text": []}
    if faults.dropped:
        answer["dropped"] = [one.note for one in faults.dropped]
    return answer


def test_a_node_without_a_type_is_named_null_as_in_every_host():
    lines = _contracts.live_op_target_lines(None, "x", {"op": "set", "text": "y"})
    assert lines == [
        "/op - the set op does not apply to a node of type null (op_target)"
    ]


def test_the_fixture_holds_the_cases():
    assert len(_CASES) > 30


@pytest.mark.parametrize("case", _CASES, ids=[case["name"] for case in _CASES])
def test_each_shared_case_gives_its_answer(case):
    assert _answer(case) == case["expect"]


@pytest.mark.parametrize("case", _PATCHES, ids=[case["name"] for case in _PATCHES])
def test_each_shared_patch_case_gives_its_lines(case):
    assert _contracts.live_patch_lines(case["patch"]) == case["expect"]


_MARKERS = ["MARKERVALUE", -12345, 987654321.5, ["MARKERVALUE"], {"k": "MARKERVALUE"}]
_HIDDEN = re.compile(r"MARKERVALUE|12345|987654321")


def test_a_fault_never_shows_a_value_of_a_node_or_an_op():
    for definition, tag_key in (("component", "type"), ("live_op", "op")):
        for variant in _contracts.definition("view", definition)["oneOf"]:
            tag = variant["properties"][tag_key]["const"]
            for key in variant["properties"]:
                if key in ("id", tag_key):
                    continue
                for marker in _MARKERS:
                    if definition == "component":
                        node = {"id": "n", "type": tag, key: marker}
                        faults = _contracts.live_view_faults(
                            {"title": "T", "nodes": [node]}
                        )
                        text = repr((faults.view, faults.nodes))
                    else:
                        op = {"op": tag, "node_id": "n", key: marker}
                        patch = {"view_id": "v", "seq": 1, "ops": [op]}
                        text = repr(_contracts.live_patch_lines(patch))
                    assert not _HIDDEN.search(text), (tag, key)
    unknown = {"id": "n", "type": "MARKERVALUE"}
    assert "MARKERVALUE" not in repr(
        _contracts.live_view_faults({"title": "T", "nodes": [unknown]}).nodes
    )


def _recorder_answer(case):
    import blockether.vis.extension as vis

    recorder = vis.testing.LiveRecorder(vis._host, extension=case.get("owner"))
    recorder.host_live(json.dumps({"op": "open", "view": case["view"]}))
    try:
        recorder.host_live(json.dumps({"op": "patch", "patch": {"ops": case["ops"]}}))
    except RuntimeError as error:
        return {"refusal": str(error)}
    value = json.loads(recorder.view_json())
    at = case["expect"].get("at")
    if at is None:
        return {"accepted": True}
    for part in at:
        value = value[int(part) if isinstance(value, list) else part]
    return {"at": at, "value": value}


@pytest.mark.parametrize("case", _FIXTURE["applies"], ids=lambda c: c["name"])
def test_the_recorder_applies_each_shared_case_as_the_engine(case):
    assert _recorder_answer(case) == case["expect"]


def _outside_answer(case, monkeypatch):
    from blockether.vis import _outside

    if case.get("owner"):
        monkeypatch.setenv("VIS_OUTSIDE_EXTENSION", case["owner"])
    else:
        monkeypatch.delenv("VIS_OUTSIDE_EXTENSION", raising=False)

    opened = json.loads(_outside.live(json.dumps({"op": "open", "view": case["view"]})))
    view_id = opened["view_id"]
    patch = {"op": "patch", "view_id": view_id, "patch": {"ops": case["ops"]}}
    try:
        _outside.live(json.dumps(patch))
    except RuntimeError as error:
        return {"refusal": str(error)}
    state = json.loads(_outside.live(json.dumps({"op": "state", "view_id": view_id})))
    at = case["expect"].get("at")
    if at is None:
        return {"accepted": True}
    value = state["view"]
    for part in at:
        value = value[int(part) if isinstance(value, list) else part]
    return {"at": at, "value": value}


@pytest.mark.parametrize("case", _FIXTURE["applies"], ids=lambda c: c["name"])
def test_the_outside_host_applies_each_shared_case_as_the_engine(case, monkeypatch):
    assert _outside_answer(case, monkeypatch) == case["expect"]
