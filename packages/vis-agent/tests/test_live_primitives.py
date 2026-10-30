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
            vis.ui.heading(f"h{level}", f"Heading {level}", level=level)
            for level in range(1, 7)
        ],
        vis.ui.paragraph("prose", "A **paragraph**"),
        vis.ui.code("code", "  <button>\n", language="html"),
        *[
            vis.ui.spinner(variant, variant, variant=variant)
            for variant in ("braille", "dots", "line", "pulse")
        ],
        vis.ui.disclosure(
            "details",
            "Details",
            vis.ui.button("go", "Go"),
            vis.ui.output("a", lines=["retained"]),
            vis.ui.output("b", lines=["independent"], default_expanded=True),
        ),
    ]


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_builders_and_mutations(monkeypatch, host_kind):
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    assert vis.ui.heading("Form title") == {"type": "heading", "text": "Form title"}
    assert vis.ui.paragraph("Form prose") == {"type": "paragraph", "text": "Form prose"}
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
        children = {n["id"]: n for n in nodes["details"]["children"]}
        assert children["go"]["clicks"] == 0
        assert children["go"]["is_disabled"] is True
        assert children["a"]["lines"] == ["retained", "new line"]
        assert children["b"]["lines"] == ["independent"]


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_an_app_keeps_its_note_for_the_model(monkeypatch, host_kind):
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    app = vis.attach(vis.ui.app("Board", vis.ui.button("go", "Go")))
    app.set_agent_note(" Card 3 is done ")
    assert app.state()["agent_note"] == "Card 3 is done"
    app.set_agent_note(" ")
    assert "agent_note" not in app.state()
    app.close()


def test_operator_presses_wake_the_producer_and_are_counted(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Actions", [vis.ui.button("go", "Go")], flush_ms=0) as view:
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
            collect(node.get("children", []))

    collect(json.loads(fixture.read_text())["nodes"])
    branches = definition("view", "component")["oneOf"]
    inputs = {
        branch["properties"]["type"]["const"]
        for branch in branches
        if "name" in branch["required"]
    }
    assert {n["type"] for n in nodes} - {"group"} - inputs == {
        branch["properties"]["type"]["const"] for branch in branches
    } - {"group"} - inputs
    assert {n["type"] for n in nodes} & inputs, "the fixture shows one input"
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


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_table_replace_keeps_selection_of_rows_that_remain(monkeypatch, host_kind):
    # #428: a clear-then-upsert refresh lost the selection on the engine, while
    # these test hosts kept it. Replace swaps the rows and prunes in one step.
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    table = vis.ui.table(
        "chats",
        columns=[vis.ui.table_column("chat", "Chat")],
        rows=[vis.ui.table_row("a", ["A"]), vis.ui.table_row("b", ["B"])],
        is_selectable=True,
    )
    with vis.live("Chats", [table], flush_ms=0) as view:
        chats = view["chats"]
        chats.select("a", "b")
        chats.replace([vis.ui.table_row("c", ["C"]), vis.ui.table_row("b", ["B*"])])
        node = view.state()["nodes"][0]
        assert [(row["id"], row["cells"]) for row in node["rows"]] == [
            ("c", ["C"]),
            ("b", ["B*"]),
        ]
        assert node["selected_ids"] == ["b"]
        chats.clear()
        assert view.state()["nodes"][0]["selected_ids"] == []
        chats.replace([])
        assert view.state()["nodes"][0]["rows"] == []
        with pytest.raises(TypeError, match="table_row"):
            chats.replace([("d", ["D"])])


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_scene_changes_merge_onto_their_objects(monkeypatch, host_kind):
    # A game tick sends only what moved. Two changes to one object in one batch
    # must not drop the keys of the first, and a new object needs its mesh.
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    scene = vis.ui.scene(
        "world",
        16,
        9,
        objects=[vis.ui.scene_object("ship", "cone", x=8, y=4, color="#88c0d0")],
    )
    with vis.live("Game", [scene], flush_ms=10000) as view:
        world = view["world"]
        with view.batch():
            world.put("ship", x=9)
            world.put("ship", rz=30)
            world.mesh("shard", [(0, 1, 0), (-1, -1, 0), (1, -1, 0)], [(0, 1, 2)])
            world.put("rock", mesh="shard", x=2)
        ship = world.objects["ship"]
        assert (ship["x"], ship["y"], ship["rz"], ship["color"]) == (
            9,
            4,
            30,
            "#88c0d0",
        )
        assert world.objects["rock"]["scale"] == 1
        with pytest.raises(Exception, match="mesh"):
            world.put("ghost", x=1)
            view.flush()


def test_table_builder_refuses_text_columns():
    # #428: text columns passed every SDK check and failed only when a real app opened.
    with pytest.raises(
        TypeError,
        match=r"columns\[1\] is 'Category'.*table_column\('category', 'Category'\)",
    ):
        vis.ui.table("chats", columns=[vis.ui.table_column("chat", "Chat"), "Category"])


def test_button_variant_is_kept_and_a_typo_names_the_variants():
    # #430: a button had no hierarchy, and a misspelled variant reached the host.
    assert vis.ui.button("save", "Save", variant="primary")["variant"] == "primary"
    assert "variant" not in vis.ui.button("save", "Save")
    with pytest.raises(ValueError, match=r"primary, secondary, destructive.*'danger'"):
        vis.ui.button("drop", "Drop", variant="danger")


def test_outside_refuses_an_unknown_button_variant():
    # #430: outside Vis a raw button with a typo variant was accepted.
    button = {"id": "drop", "type": "button", "label": "Drop", "variant": "danger"}
    view = {"title": "Rows", "nodes": [button]}
    with pytest.raises(RuntimeError, match=r"primary, secondary, destructive"):
        _outside.live(json.dumps({"op": "open", "view": view}))


def test_recorder_refuses_a_live_group_without_an_id(monkeypatch):
    # #428: the recorder hid the buttons of an unnamed group; the engine refuses it.
    # Vis raises VisToolError, a RuntimeError, so the recorder must raise one too.
    monkeypatch.setattr(vis, "_host", vis.testing.LiveRecorder(vis._host))
    with pytest.raises(
        RuntimeError, match=r"group needs an id.*vis\.ui\.group\('actions'"
    ):
        with vis.live(
            "Chats", [vis.ui.wrap(vis.ui.button("save", "Save"))], flush_ms=0
        ):
            pass


def test_outside_refuses_a_live_group_without_an_id_with_the_fix():
    # #428: outside Vis the refusal named no fix for an unnamed group.
    view = {"title": "Chats", "nodes": [vis.ui.wrap(vis.ui.button("save", "Save"))]}
    with pytest.raises(
        RuntimeError, match=r"group needs an id.*vis\.ui\.group\('actions'"
    ):
        _outside.live(json.dumps({"op": "open", "view": view}))


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
            "Invalid", [vis.ui.disclosure("section", "Section", invalid)], flush_ms=0
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
        "Styled output", [vis.ui.output("log", window_lines=3)], flush_ms=10000
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
    with vis.live("Safe output", [vis.ui.output("log")], flush_ms=0) as view:
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
    divider = vis.ui.divider("section-break")
    assert divider == {"id": "section-break", "type": "divider"}
    with vis.live(
        "Build sections", [vis.ui.paragraph("before", "Build finished")], flush_ms=0
    ) as view:
        view.add(divider, after="before")
        assert view["section-break"].type == "divider"
        for verb in ("set", "write", "clear", "remove", "add", "select"):
            assert not hasattr(view["section-break"], verb)
        assert view.state()["nodes"][1] == divider
        view.drop("section-break")
        assert len(view.state()["nodes"]) == 1
        view.add(
            vis.ui.column(
                "results", divider, vis.ui.paragraph("after", "Review results")
            )
        )
        assert view.state()["nodes"][1]["children"][0] == divider
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


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_a_blank_detail_clears_the_status_line(monkeypatch, host_kind):
    # Issue #255: a poller with nothing to add this round sent detail="" and the
    # host refused the whole patch, so a finished operation looked failed.
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    with vis.live(
        "Poll", [vis.ui.status("state", "Polling", detail="attempt 1")], flush_ms=0
    ) as view:
        assert view.state()["nodes"][0]["detail"] == "attempt 1"
        view["state"].set("Polling", detail="")
        assert "detail" not in view.state()["nodes"][0]


def test_grid_and_wrap_build_the_layout_directions_of_the_contract():
    grid = vis.ui.grid(
        "board", vis.ui.paragraph("a", "A"), vis.ui.paragraph("b", "B"), max_columns=3
    )
    assert grid["direction"] == "grid"
    assert grid["max_columns"] == 3
    assert [child["id"] for child in grid["children"]] == ["a", "b"]
    actions = vis.ui.wrap(
        "actions", vis.ui.button("go", "Go"), vis.ui.button("stop", "Stop")
    )
    assert actions["direction"] == "wrap"
    assert "max_columns" not in actions
    assert "max_columns" not in vis.ui.grid(vis.ui.plaintext("host"))
    with vis.live("Layout", [grid, actions], flush_ms=0) as view:
        nodes = view.state()["nodes"]
        assert [node["direction"] for node in nodes] == ["grid", "wrap"]


def test_a_group_arranging_nothing_is_refused_where_it_was_written():
    # Issue #255: an empty layout group used to reach the host as an invalid view.
    for build in (
        lambda: vis.ui.column("targets"),
        lambda: vis.ui.row(),
        lambda: vis.ui.grid(),
        lambda: vis.ui.wrap("actions"),
        lambda: vis.ui.disclosure("targets", "Targets"),
    ):
        with pytest.raises(ValueError, match="at least one node"):
            build()


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_stat_and_step_updates_carry_the_label_the_schema_requires(
    monkeypatch, host_kind
):
    # The evals watch view sent a counter update without its label, and the host
    # refused it. The recorder now refuses the same patch, as the View schema says.
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    nodes = [
        vis.ui.stat(
            "counts", stats=[{"id": "ok", "label": "Passed", "value_text": "0"}]
        ),
        vis.ui.steps("plan", steps=[{"id": "build", "label": "Build"}]),
    ]
    with vis.live("Counts", nodes, flush_ms=0) as view:
        with pytest.raises((ValueError, _outside.Refused), match="label"):
            view["counts"]._op("append", stats=[{"id": "ok", "value_text": "1"}])
        view["counts"].set("ok", "2", "Passed")
        view["plan"].set("build", "Build", tone="ok")
        counts, plan = view.state()["nodes"]
        stats, steps = counts["stats"], plan["steps"]
        assert stats == [{"id": "ok", "label": "Passed", "value_text": "2"}]
        assert steps[0]["label"] == "Build" and steps[0]["tone"] == "ok"


def _form_nodes():
    return [
        vis.ui.heading("title", "Deploy", level=2),
        vis.ui.group(
            "deploy-form",
            vis.ui.plaintext("target", label="Target", is_required=True),
            vis.ui.password("token", label="Token"),
            vis.ui.slider("pct", min=0, max=10),
            vis.ui.button("apply", "Apply", action="submit"),
            label="Target",
        ),
        vis.ui.card(
            "plan-small", "Small", text="2 CPU", meta=["$5"], is_selectable=True
        ),
    ]


def test_a_full_form_runs_inside_a_live_view(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    seen = []
    with vis.live("Deploy", _form_nodes(), placement="panel", key="deploy") as view:
        view.on_event(seen.append)
        opened = recorder.said[0]["view"]
        assert (opened["placement"], opened["key"]) == ("panel", "deploy")
        assert opened["nodes"][1]["children"][0]["name"] == "target"
        assert set(view.values()) == {"target", "pct"}, "a secret input stays out"
        assert recorder.submit("apply")["errors"] == {"target": "is required"}
        assert view.events() == []
        recorder.change("target", "db-2")
        recorder.change("token", "fixture-live-secret")
        assert recorder.submit("apply", {"token": "fixture-live-secret"})["is_accepted"]
        recorder.activate("plan-small")
        events = view.events()
        assert [(e.type, e.node_id) for e in events] == [
            ("change", "target"),
            ("change", "token"),
            ("submit", "apply"),
            ("activate", "plan-small"),
        ]
        assert events[1].is_set is True and events[1].value is None
        assert events[2].values["target"] == "db-2"
        assert events[2].values["token"].startswith("vis-secret:")
        assert seen == events
        assert view["target"].value == "db-2"
        assert view["token"].value is None
        assert view["apply"].clicks == 1
        assert view["plan-small"].clicks == 1
        view["target"].set("db-3")
        assert view.values()["target"] == "db-3"
        assert view.events(timeout=0.01) == []
        assert "fixture-live-secret" not in json.dumps(recorder.picture())
    assert view.result["is_completed"] is True


def test_a_live_form_outside_vis_has_values_and_no_events(monkeypatch):
    monkeypatch.setattr(vis, "_host", _outside.host)
    nodes = [
        vis.ui.plaintext("target", label="Target", value="db-2"),
        vis.ui.button("apply", "Apply", action="submit"),
    ]
    with vis.live("Deploy", nodes, placement="inline") as view:
        assert view.values() == {"target": "db-2"}
        view["target"].set("db-3")
        assert view["target"].value == "db-3"
        assert view.events() == []


def test_ask_sends_one_component_tree():
    validators = {}
    request = vis._request_spec(
        "Deploy",
        [
            vis.ui.group("g", vis.ui.plaintext("a"), label="Group"),
            vis.ui.card("c", "Card"),
        ],
        {},
        validators,
    )
    assert request["nodes"][0]["children"] == [{"type": "plaintext", "name": "a"}]
    assert request["nodes"][1] == {"type": "card", "id": "c", "title": "Card"}


def test_a_panel_view_pushes_events_to_its_open_callback(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    seen = []
    nodes = [vis.ui.button("go", "Rerun")]
    with vis.live(
        "Build", nodes, placement="panel", key="build", on_event=seen.append
    ) as view:
        recorder.activate("go")
        assert [(e.type, e.node_id) for e in seen] == [("activate", "go")]
        assert [e.node_id for e in view.events()] == ["go"]
        assert len(seen) == 1, "a pushed event reaches each callback once"
