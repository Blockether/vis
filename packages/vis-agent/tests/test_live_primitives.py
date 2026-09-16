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
    from blockether.vis._contracts import definition

    nodes = []

    def collect(items):
        for node in items:
            nodes.append(node)
            collect(node.get("fields", []))

    collect(json.loads(fixture.read_text())["nodes"])
    assert {n["type"] for n in nodes} - {"group"} == {
        branch["properties"]["type"]["const"]
        for branch in definition("view", "live_node")["oneOf"]
    } - {"group"}
    assert {n["variant"] for n in nodes if n["type"] == "spinner"} == {
        branch["const"] for branch in definition("view", "spinner_variant")["oneOf"]
    }


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


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_styled_output_appends_preserve_plain_text_and_chunk_tones(
    monkeypatch, host_kind
):
    # #209: style is metadata, never an ANSI/HTML interpreter or rewritten history.
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    with vis.live(
        "Styled output", [vis.output("log", window_lines=3)], flush_ms=10000
    ) as view:
        view.write("first")
        with view.batch():
            view.write("WARN low space", tone="warn")
            view.write("ERROR <script>literal</script>", tone="error")
            view.write("last")
        node = view.state()["nodes"][0]
        assert node["lines"] == [
            "WARN low space",
            "ERROR <script>literal</script>",
            "last",
        ]
        assert node["line_tones"] == ["warn", "error", None]
        assert node["total_lines"] == 4
        result = view.close()
        assert result["view"]["nodes"][0] == node


def test_log_controls_are_visible_and_styles_are_closed(monkeypatch, capsys):
    monkeypatch.setattr(vis, "_host", _outside.host)
    with vis.live("Safe output", [vis.output("log")], flush_ms=0) as view:
        view.write("error\x1b[2J\x1b]8;;javascript:alert(1)\x07", tone="error")
        line = view.state()["nodes"][0]["lines"][0]
        assert line == r"error\u001b[2J\u001b]8;;javascript:alert(1)\u0007"
        assert "\x1b" not in capsys.readouterr().err
        with pytest.raises((ValueError, _outside.Refused)):
            view.write("invalid", tone="red;url(javascript:alert(1))")
        view["log"].clear()
        view.write("plain")
        node = view.state()["nodes"][0]
        assert node["lines"] == ["plain"]
        assert not any(node.get("line_tones", []))


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_divider_is_static_and_retained(monkeypatch, host_kind):
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    divider = vis.divider("section-break")
    assert divider == {"id": "section-break", "type": "divider"}
    with vis.live(
        "Build sections", [vis.paragraph("before", "Build finished")], flush_ms=0
    ) as view:
        view.add(divider, after="before")
        assert view["section-break"].type == "divider"
        for verb in ("set", "write", "clear", "remove", "add", "select"):
            assert not hasattr(view["section-break"], verb)
        assert view.state()["nodes"][1] == divider
        view.drop("section-break")
        assert len(view.state()["nodes"]) == 1
        view.add(
            vis.column("results", divider, vis.paragraph("after", "Review results"))
        )
        assert view.state()["nodes"][1]["fields"][0] == divider
        receipt = view.close()
        assert receipt["view"]["nodes"][1] == divider


@pytest.mark.parametrize(
    "extra",
    [{"label": "Section"}, {"text": "---"}, {"tone": "idle"}, {"style": "dashed"}],
)
def test_outside_divider_rejects_content_and_style(extra):
    with pytest.raises(_outside.Refused):
        _outside.live(
            json.dumps(
                {
                    "op": "open",
                    "view": {
                        "title": "Invalid divider",
                        "nodes": [{"id": "break", "type": "divider", **extra}],
                    },
                }
            )
        )


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_divider_refuses_mutation(host_kind):
    host = (
        _outside.live
        if host_kind == "outside"
        else vis.testing.LiveRecorder(vis._host).host_live
    )
    refused = _outside.Refused if host_kind == "outside" else AssertionError
    divider = {"id": "break", "type": "divider"}
    opened = json.loads(
        host(
            json.dumps(
                {"op": "open", "view": {"title": "Static divider", "nodes": [divider]}}
            )
        )
    )
    view_id = opened["view_id"]
    try:
        for operation in (
            {"op": "set"},
            {"op": "set", "label": "Title"},
            {"op": "append", "lines": ["line"]},
            {"op": "clear"},
            {"op": "remove", "item_ids": ["x"]},
        ):
            with pytest.raises(refused):
                host(
                    json.dumps(
                        {
                            "op": "patch",
                            "view_id": view_id,
                            "patch": {"ops": [{"node_id": "break", **operation}]},
                        }
                    )
                )
            state = json.loads(host(json.dumps({"op": "state", "view_id": view_id})))
            assert state["view"]["nodes"][0] == divider
    finally:
        host(json.dumps({"op": "close", "view_id": view_id}))
