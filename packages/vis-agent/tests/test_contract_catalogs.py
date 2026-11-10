"""The SDK reads its vocabulary from the vis-contract schemas, not from a copy."""

import copy
import json
import sys
import types
import typing
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts, _outside
from blockether.vis.activity import ActivityProjection
from blockether.vis.engine import GatewayClient


def _add_input(monkeypatch, kind, *, like):
    """Add input component `kind` to the view schema, shaped as `like`."""
    union = _contracts.definition("view", "component")
    branch = copy.deepcopy(_contracts.component_variant(like)[1])
    branch["properties"]["type"]["const"] = kind
    monkeypatch.setitem(union, "oneOf", [*union["oneOf"], branch])


def test_activity_takes_each_presenter_of_the_schema(monkeypatch):
    presenter = _contracts.definition("activity", "presenter")
    for name in presenter["enum"]:
        assert vis.Activity(presenter=name).presenter == name
    monkeypatch.setitem(presenter, "enum", [*presenter["enum"], "chart"])
    assert vis.Activity(presenter="chart").presenter == "chart"
    with pytest.raises(ValueError, match="unknown Activity presenter"):
        vis.Activity(presenter="nope")


def test_the_default_presenter_is_in_the_schema_and_needs_no_schema_read(monkeypatch):
    # The SDK import builds activities. It must not load the contract reader there.
    default = vis.Activity().presenter
    assert default in _contracts.definition("activity", "presenter")["enum"]

    def refuse(*_):
        raise AssertionError("the default presenter read the schema")

    monkeypatch.setattr(_contracts, "definition", refuse)
    monkeypatch.setattr(_contracts, "schema", refuse)
    assert vis.Activity().presenter == default


def test_the_sdk_import_reads_no_schema(monkeypatch):
    # A worker runs the SDK source for each extension. The source declares tools with
    # an Activity and a tag, and these must not read a schema there.
    def refuse(*_):
        raise AssertionError("the SDK import read a schema")

    monkeypatch.setattr(_contracts, "definition", refuse)
    monkeypatch.setattr(_contracts, "schema", refuse)
    source = Path(vis.__file__)
    module = types.ModuleType("blockether.vis._import_probe")
    module.__package__ = "blockether.vis"
    module.__file__ = str(source)
    module.__dict__["_host"] = object()
    monkeypatch.setitem(sys.modules, module.__name__, module)
    exec(compile(source.read_text(encoding="utf-8"), str(source), "exec"), vars(module))
    assert module.Catalog.spec.__vis_symbol_activity__.label == "Inspect tool catalog"


def test_each_activity_that_the_sdk_declares_passes_the_schema_checks():
    declared = [
        member.__vis_symbol_activity__
        for owner in vars(vis).values()
        if isinstance(owner, type)
        for member in vars(owner).values()
        if getattr(member, "__vis_symbol_activity__", None) is not None
    ]
    assert declared
    for activity in declared:
        assert activity == vis.Activity(
            presenter=activity.presenter,
            label=activity.label,
            render=activity.render,
            show_start=activity.show_start,
        )


def test_an_app_row_is_known_by_the_media_type_of_the_schema(monkeypatch):
    row = _contracts.definition("view", "app_attachment")
    assert vis._APP_MEDIA_TYPE == row["contentMediaType"]
    monkeypatch.setitem(row, "contentMediaType", "application/x-next-app+json")
    with pytest.raises(ValueError, match="no view_id"):
        vis.LiveAttachment._from({"media_type": "application/x-next-app+json"})


def test_a_view_reads_each_input_type_of_the_schema(monkeypatch):
    _add_input(monkeypatch, "color", like="plaintext")
    _add_input(monkeypatch, "pin", like="password")
    view = vis.LiveView.__new__(vis.LiveView)
    view._nodes = {"tint": "color"}
    nodes = [
        {"type": "color", "id": "tint", "value": "red", "is_secret": False},
        {"type": "pin", "id": "code", "value": "1234", "is_secret": True},
    ]
    view.state = lambda: {"nodes": nodes}
    assert view.values() == {"tint": "red"}
    assert isinstance(view.node("tint"), vis.Input)
    assert _contracts.secret_input_types() == {"otp", "password", "pin"}


def test_the_outside_host_and_the_sdk_share_one_input_derivation():
    assert _outside._FIELD_SCHEMAS == _contracts.input_branches()
    assert _contracts.input_types() == {
        "checkbox",
        "multiline",
        "multiselect",
        "otp",
        "password",
        "plaintext",
        "range",
        "select",
    }


def test_each_schema_component_has_a_live_handle():
    # A handle class is Python code, so the schema cannot own this table.
    layout = {
        branch["properties"]["type"]["const"]
        for branch in _contracts.definition("view", "component")["oneOf"]
        if "children" in branch["properties"]
    }
    known = {
        branch["properties"]["type"]["const"]
        for branch in _contracts.definition("view", "component")["oneOf"]
    }
    assert set(vis._LIVE_NODES) | _contracts.input_types() | layout == known
    assert not set(vis._LIVE_NODES) & _contracts.input_types()


def _content_line_stream():
    lines = next(
        branch["properties"]["lines"]
        for branch in _contracts.definition("activity", "content")["items"]["oneOf"]
        if "lines" in branch["properties"]
        and "stream" in branch["properties"]["lines"]["items"].get("properties", {})
    )
    return lines["items"]["properties"]["stream"]


# A type checker reads a Literal from the source, so each one stays a literal.
@pytest.mark.parametrize(
    ("literal", "rule"),
    [
        (lambda: vis._ActivityStream, _content_line_stream),
        (
            lambda: vis._ActivitySummaryFormat,
            lambda: _contracts.definition("activity", "text_format"),
        ),
        (
            lambda: vis._ActivityVerdict,
            lambda: _contracts.definition("activity", "verdict"),
        ),
        (lambda: vis.LogTone, lambda: _contracts.definition("view", "tone")),
        (
            lambda: typing.get_type_hints(vis.TypeSpec)["kind"],
            lambda: _contracts.definition("symbol", "type")["properties"]["kind"],
        ),
        (
            lambda: typing.get_type_hints(vis.ParameterSpec)["kind"],
            lambda: _contracts.definition("symbol", "parameter")["properties"]["kind"],
        ),
        (
            lambda: typing.get_type_hints(vis.python_environment)["return"],
            lambda: _contracts.definition("config", "python")["properties"][
                "environment"
            ],
        ),
    ],
)
def test_a_literal_type_names_the_values_of_its_schema(literal, rule):
    assert set(typing.get_args(literal())) == set(rule()["enum"])


def test_a_symbol_tag_literal_names_the_values_of_its_schema():
    tags = _contracts.definition("symbol", "callable")["properties"]["tag"]["enum"]
    assert set(typing.get_args(vis.SymbolTag)) == set(tags)
    assert vis._DEFAULT_SYMBOL_TAG in tags
    assert vis._SYMBOL_TAGS == tuple(tags)


def _more(monkeypatch, rule, *names):
    """Add `names` to the enum of `rule`."""
    monkeypatch.setitem(rule, "enum", [*rule["enum"], *names])


def test_an_activity_takes_each_verdict_format_and_stream_of_the_schema(monkeypatch):
    _more(monkeypatch, _contracts.definition("activity", "verdict"), "skipped")
    _more(monkeypatch, _contracts.definition("activity", "text_format"), "rich")
    _more(monkeypatch, _content_line_stream(), "log")
    assert (
        vis.ActivityPresentation("Run", "Done", verdict="skipped").verdict == "skipped"
    )
    assert (
        vis.ActivitySection("Run", "Done", summary_format="rich").to_wire()[
            "summary_format"
        ]
        == "rich"
    )
    assert vis.ActivityTerminalLine("line", "log").stream == "log"
    with pytest.raises(ValueError, match="must be passed, failed or skipped"):
        vis.ActivityPresentation("Run", "Done", verdict="nope")
    with pytest.raises(ValueError, match="must be stdout, stderr or log"):
        vis.ActivityTerminalLine("line", "nope")
    with pytest.raises(ValueError, match="summary_format"):
        vis.ActivitySection("Run", "Done", summary_format="nope")


def test_a_tool_takes_each_tag_of_the_schema(monkeypatch):
    def check():
        """Check one component."""

    _more(
        monkeypatch,
        _contracts.definition("symbol", "callable")["properties"]["tag"],
        "audit",
    )
    assert "audit" in vis._SYMBOL_TAGS
    assert vis.Symbol(check, tag="audit")._spec()["tag"] == "audit"
    assert vis.method(tag="audit")(check).__vis_symbol_tag__ == "audit"
    with pytest.raises(ValueError, match="verification, external, audit, got 'nope'"):
        vis.method(tag="nope")


def test_a_log_takes_each_tone_of_the_schema(monkeypatch):
    _more(monkeypatch, _contracts.definition("view", "tone"), "muted")
    monkeypatch.setattr(vis.Log, "_op", lambda self, op, **fields: fields)
    log = vis.Log.__new__(vis.Log)
    assert log.write("line", tone="muted") == {"lines": ["line"], "tone": "muted"}
    with pytest.raises(ValueError, match="running, warn or muted"):
        log.write("line", tone="nope")


def test_a_provider_value_takes_each_name_of_the_schemas(monkeypatch):
    window = _contracts.definition("provider", "window")["properties"]
    _more(monkeypatch, window["unit"], "decade")
    _more(
        monkeypatch, _contracts.definition("config", "apiStyle")["oneOf"][0], "wire-9"
    )
    assert vis.ProviderLimitWindow("rolling", unit="decade", size=1).unit == "decade"
    assert vis.ProviderPreset(api_style="wire-9").api_style == "wire-9"
    with pytest.raises(ValueError):
        vis.ProviderLimitWindow("rolling", unit="nope", size=1)
    with pytest.raises(ValueError):
        vis.ProviderPreset(api_style="nope")


def _block(kind, *path):
    rule = next(
        branch["properties"]
        for branch in _contracts.definition("activity", "content")["items"]["oneOf"]
        if branch["properties"]["type"]["const"] == kind
    )
    for key in path:
        rule = rule[key]
    return rule


def _field(definition, field):
    return _contracts.definition("activity", definition)["properties"][field]


def _section(field):
    return _field("section", field)


_MEDIA = {
    "file": vis.ActivityFile,
    "image": vis.ActivityImage,
    "video": vis.ActivityVideo,
    "audio": vis.ActivityAudio,
}

# Each row: the rule, its key, a builder that takes a size, the bound in the error text.
_BOUNDS = {
    "code language": (
        lambda: _block("code", "language"),
        "maxLength",
        lambda size: vis.ActivityCode("x", "l" * size),
        "at most {} characters",
    ),
    "diff language": (
        lambda: _block("diff", "language"),
        "maxLength",
        lambda size: vis.ActivityDiff("x", "l" * size),
        "at most {} characters",
    ),
    **{
        f"{kind} attachment id": (
            lambda kind=kind: _block(kind, "attachment_id"),
            "maxLength",
            lambda size, block=block: block("a" * size, "label"),
            "at most {} characters",
        )
        for kind, block in _MEDIA.items()
    },
    **{
        f"{kind} label": (
            lambda kind=kind: _block(kind, "label"),
            "maxLength",
            lambda size, block=block: block("id", "l" * size),
            "at most {} characters",
        )
        for kind, block in _MEDIA.items()
    },
    "progress label": (
        lambda: _block("progress", "label"),
        "maxLength",
        lambda size: vis.ActivityProgress("l" * size),
        "at most {} characters",
    ),
    "terminal line": (
        lambda: _block("terminal", "lines", "items", "properties", "text"),
        "maxLength",
        lambda size: vis.ActivityTerminalLine("x" * size),
        "at most {} characters",
    ),
    "terminal stages": (
        lambda: _block("terminal", "command"),
        "maxItems",
        lambda size: vis.ActivityTerminal(("ls",) * size),
        "1 to {} nonempty stages",
    ),
    "terminal stage": (
        lambda: _block("terminal", "command", "items"),
        "maxLength",
        lambda size: vis.ActivityTerminal(("x" * size,)),
        "nonempty stages",
    ),
    "terminal lines": (
        lambda: _block("terminal", "lines"),
        "maxItems",
        lambda size: vis.ActivityTerminal(("ls",), ("x",) * size),
        "at most {} lines",
    ),
    "headline": (
        lambda: _section("headline"),
        "maxLength",
        lambda size: vis.ActivitySection("h" * size, "Done"),
        "at most {} characters",
    ),
    "summary": (
        lambda: _section("summary"),
        "maxLength",
        lambda size: vis.ActivitySection("Run", "s" * size),
        "at most {} characters",
    ),
    "summary bytes": (
        lambda: _section("summary"),
        "x-vis-max-bytes",
        lambda size: vis.ActivitySection("Run", "s" * size),
        "at most {} UTF-8 bytes",
    ),
    "outcome": (
        lambda: _section("summary"),
        "maxLength",
        lambda size: vis.ActivityPresentation("Run", "", outcome="o" * size),
        "Activity outcome must be text of at most {} characters",
    ),
    "aside": (
        lambda: _field("presentation", "aside"),
        "maxLength",
        lambda size: vis.ActivityPresentation("Run", "Done", aside="a" * size),
        "at most {} characters",
    ),
    "handle id": (
        lambda: _contracts.definition("activity", "handle_id"),
        "maxLength",
        lambda size: vis.ActivityPresentation("Run", "Done", handle_id="i" * size),
        "at most {} characters",
    ),
    "handle id bytes": (
        lambda: _contracts.definition("activity", "handle_id"),
        "x-vis-max-bytes",
        lambda size: vis.ActivityPresentation("Run", "Done", handle_id="i" * size),
        "and {} UTF-8 bytes",
    ),
    "label": (
        lambda: _field("declaration", "label"),
        "maxLength",
        lambda size: vis.Activity(label="l" * size),
        "at most {} characters",
    ),
}


@pytest.mark.parametrize("name", _BOUNDS)
def test_an_activity_bound_is_the_bound_of_the_schema(monkeypatch, name):
    rule, key, build, text = _BOUNDS[name]
    monkeypatch.setitem(rule(), key, 3)
    build(3)
    with pytest.raises(ValueError, match=text.format(3)):
        build(4)


def test_an_activity_label_grows_with_the_schema(monkeypatch):
    label = _field("declaration", "label")
    with pytest.raises(ValueError, match=f"at most {label['maxLength']} characters"):
        vis.Activity(label="l" * 150)
    monkeypatch.setitem(label, "maxLength", 200)
    assert len(vis.Activity(label="l" * 150).label) == 150
    with pytest.raises(ValueError, match="at most 200 characters"):
        vis.Activity(label="l" * 201)


def test_a_projection_reads_the_byte_bound_of_the_schema(monkeypatch):
    fixture = json.loads((_contracts._DATA / "fixtures/activity.json").read_text())
    fixture["rows"][0]["presentation"] = vis.ActivityPresentation(
        "Run", "ten bytes!"
    ).to_wire()
    assert ActivityProjection.from_wire(fixture).to_wire() == fixture
    monkeypatch.setitem(_section("summary"), "x-vis-max-bytes", 9)
    with pytest.raises(ValueError, match="exceeds bound"):
        ActivityProjection.from_wire(fixture)


def test_a_goal_is_checked_by_the_bounds_of_the_schema_before_the_network(monkeypatch):
    rules = _contracts.definition("gateway", "session_goal")["properties"]
    monkeypatch.setitem(rules["objective"], "maxLength", 10)
    monkeypatch.setitem(rules["iteration_budget"], "maximum", 7)
    client = GatewayClient("http://127.0.0.1:1")
    try:
        session = client.session("s1")
        with pytest.raises(ValueError, match="objective must contain 1–10 characters"):
            session.goal("x" * 11)
        with pytest.raises(ValueError, match="an integer from 1 to 7"):
            session.goal("work", iteration_budget=8)
    finally:
        client.close()


def test_each_view_event_of_the_gateway_has_a_view_definition():
    from blockether.vis.views import EVENT_DEFINITIONS

    events = {
        branch["const"]
        for branch in _contracts.definition("gateway", "session_event_type")["oneOf"]
        if branch["const"].startswith("view.")
    }
    assert set(EVENT_DEFINITIONS) == events
    assert set(EVENT_DEFINITIONS.values()) <= set(_contracts.schema("view")["$defs"])
