"""The full live vocabulary crosses both SDK hosts without losing content."""

import json
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside


def presentation():
    return [
        *[
            vis.heading(f"h{level}", f"Heading {level}", level=level)
            for level in range(1, 7)
        ],
        vis.paragraph("prose", "A **paragraph**"),
        vis.code("code", "  <button>\n", language="html"),
        *[
            vis.spinner(variant, variant, variant=variant)
            for variant in ("braille", "dots", "line", "pulse")
        ],
        vis.disclosure(
            "details",
            "Details",
            vis.button("go", "Go"),
            vis.output("a", lines=["retained"]),
            vis.output("b", lines=["independent"], default_expanded=True),
        ),
    ]


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_builders_and_mutations(monkeypatch, host_kind):
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    assert vis.heading("Form title") == {"type": "heading", "text": "Form title"}
    assert vis.paragraph("Form prose") == {"type": "paragraph", "text": "Form prose"}
    with vis.live("Presentation", presentation(), flush_ms=0) as view:
        view["h1"].set("Updated", level=2)
        view["prose"].set("Changed paragraph")
        view["code"].set("")
        view["braille"].set("Done", variant="pulse", is_active=False)
        view["go"].set(is_disabled=True)
        view["a"].write("new line")
        nodes = {n["id"]: n for n in view.state()["nodes"]}
        assert nodes["h1"]["level"] == 2
        assert nodes["code"]["text"] == ""
        assert nodes["braille"]["is_active"] is False
        children = {n["id"]: n for n in nodes["details"]["fields"]}
        assert children["go"]["clicks"] == 0
        assert children["go"]["is_disabled"] is True
        assert children["a"]["lines"] == ["retained", "new line"]
        assert children["b"]["lines"] == ["independent"]


def test_operator_presses_wake_the_producer_and_are_counted(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Actions", [vis.button("go", "Go")], flush_ms=0) as view:
        with ThreadPoolExecutor(max_workers=4) as pool:
            list(pool.map(lambda _: recorder.activate("go"), range(20)))
        assert view.sleep(0.1) is True
        assert view.state()["nodes"][0]["clicks"] == 20
        view["go"].set(is_disabled=True)
        with pytest.raises(AssertionError):
            recorder.activate("go")
    with pytest.raises(AssertionError):
        recorder.activate("go")


def test_shared_fixture_covers_every_primitive_and_spinner():
    fixture = (
        Path(__file__).parents[2]
        / "vis-contract/resources/vis-contract/fixtures/live-primitives.json"
    )
    contract = json.loads(fixture.parent.parent.joinpath("view.json").read_text())
    nodes = []

    def collect(items):
        for node in items:
            nodes.append(node)
            collect(node.get("fields", []))

    collect(json.loads(fixture.read_text())["nodes"])
    assert {n["type"] for n in nodes} - {"group"} == set(contract["live"]["node_types"])
    assert {n["variant"] for n in nodes if n["type"] == "spinner"} == set(
        contract["live"]["spinner_frames"]
    )


def test_documented_review_example_accepts_a_real_operator_press(monkeypatch):
    import re

    document = Path(__file__).parents[3] / "resources/vis-docs/live-views.md"
    example = next(
        block
        for block in re.findall(r"```python\n(.*?)\n```", document.read_text(), re.S)
        if 'vis.live("Review"' in block
    )
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)

    def press_while_waiting(view, seconds):
        recorder.activate("continue")
        return True

    monkeypatch.setattr(vis.LiveView, "sleep", press_while_waiting)
    exec(compile(example, str(document), "exec"), {"vis": vis})
    assert recorder.node("waiting")["is_active"] is False
    assert recorder.node("continue")["is_disabled"] is True
    assert recorder.node("tests")["lines"] == ["Tests passed"]


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
@pytest.mark.parametrize(
    ("node", "expected"),
    [
        ({"id": "x", "type": "heading", "text": "Title"}, {"level": 2}),
        (
            {"id": "x", "type": "spinner"},
            {"text": "Working", "variant": "braille", "is_active": True},
        ),
        (
            {"id": "x", "type": "button", "label": "Go"},
            {"clicks": 0, "is_disabled": False},
        ),
    ],
)
def test_raw_declarations_receive_engine_defaults(
    monkeypatch, host_kind, node, expected
):
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    with vis.live("Defaults", [node], flush_ms=0) as view:
        actual = view.state()["nodes"][0]
        assert expected.items() <= actual.items()


def test_outside_refused_patch_leaves_state_unchanged():
    opened = json.loads(
        _outside.live(
            json.dumps(
                {
                    "op": "open",
                    "view": {"title": "Atomic update", "nodes": presentation()},
                }
            )
        )
    )
    view_id = opened["view_id"]
    try:
        with pytest.raises(_outside.Refused):
            _outside.live(
                json.dumps(
                    {
                        "op": "patch",
                        "view_id": view_id,
                        "patch": {
                            "ops": [
                                {
                                    "op": "set",
                                    "node_id": "code",
                                    "text": "must not be committed",
                                },
                                {"op": "set", "node_id": "h1", "level": 0},
                            ]
                        },
                    }
                )
            )
        current = json.loads(
            _outside.live(json.dumps({"op": "state", "view_id": view_id}))
        )
        assert current["view"] == opened["view"]
    finally:
        _outside.live(json.dumps({"op": "close", "view_id": view_id}))


@pytest.mark.parametrize(
    "invalid",
    [
        {"id": "x", "type": "button", "label": "Go", "clicks": 10},
        {"id": "x", "type": "code", "text": "literal", "language": 4},
    ],
)
def test_outside_refuses_invalid_primitive_declarations(monkeypatch, invalid):
    monkeypatch.setattr(vis, "_host", _outside.host)
    with pytest.raises(_outside.Refused):
        with vis.live(
            "Invalid", [vis.disclosure("section", "Section", invalid)], flush_ms=0
        ):
            pass
