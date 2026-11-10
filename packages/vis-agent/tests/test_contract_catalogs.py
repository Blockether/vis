"""The SDK reads its vocabulary from the vis-contract schemas, not from a copy."""

import copy
import typing

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts, _outside


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
    assert vis.Activity(label="Greet person").presenter == default


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
