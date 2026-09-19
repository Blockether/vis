"""Typed declarations are pure values until the explicit registration boundary."""

from dataclasses import FrozenInstanceError, fields, is_dataclass

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


def greet(name: str, *, loud: bool = False):
    """Greet one person."""
    return name.upper() if loud else name


def format_toml(options):
    """Format the TOML files one `format_code` call names."""
    return {"op": "format_code", "language": "toml", "files": [], "changed": 0}


def test_declarations_are_typed_pure_and_register_once(monkeypatch):
    calls = []
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    monkeypatch.setattr(
        vis._host, "declare_env", lambda names: calls.append(names) or "{}"
    )
    symbol = vis.Symbol(greet)
    provider = vis.Provider("example", "Example", get_token_fn=lambda: None)
    command = vis.SlashCommand("greet", greet)
    hook = vis.OpHook(["shell"], lambda event: None)
    network_filter = vis.NetworkFilter(lambda request: request)
    surface = vis.LanguageSurface(
        language="toml", extensions=["toml"], format=format_toml
    )
    extension = vis.Extension(
        name="greeter",
        description="Greeting tools",
        alias="greet",
        symbols=[symbol],
        providers=[provider],
        slash_commands=[command],
        op_hooks=[hook],
        language_tools=[surface],
        network_filters=[network_filter],
        env=["SDK_EXAMPLE_ENV"],
    )
    assert calls == []
    assert vis._registration["spec"] is None
    assert extension.symbols == (symbol,)
    assert hook.ops == ("shell",)
    for value in (symbol, provider, command, hook, surface, network_filter, extension):
        assert is_dataclass(value)
        assert not hasattr(value, "__dict__")
        assert all(field.type for field in fields(value))
        for field in fields(value):
            with pytest.raises(FrozenInstanceError):
                setattr(value, field.name, getattr(value, field.name))
    vis.register_extension(extension)
    assert len(calls) == 1
    wire = vis._registration["spec"]
    assert wire["symbols"][0]["marker"] == "symbol"
    # #197: the bridge preserves real keywords, not a trailing positional map.
    assert wire["symbols"][0]["fn"]("Ada", loud=True) == "ADA"
    assert wire["symbols"][0]["fn"]("Ada") == "Ada"
    with pytest.raises(TypeError):
        wire["symbols"][0]["fn"]("Ada", {"loud": True})
    assert wire["providers"][0]["marker"] == "provider"
    assert wire["slash_commands"][0]["marker"] == "slash"
    assert wire["op_hooks"][0]["ops"] == ["shell"]
    assert wire["language_tools"][0]["marker"] == "language_surface"
    assert wire["language_tools"][0]["language"] == "toml"
    assert wire["language_tools"][0]["extensions"] == ["toml"]
    assert wire["language_tools"][0]["format"]({})["op"] == "format_code"
    # A surface carries exactly the capabilities it declares.
    assert "lint" not in wire["language_tools"][0]
    assert wire["network_filters"][0]["marker"] == "network_filter"
    with pytest.raises(ValueError, match="once per file"):
        vis.register_extension(extension)
    assert len(calls) == 1


@pytest.mark.parametrize(
    "name", ["extension", "symbol", "slash", "provider", "op_hook", "network_filter"]
)
def test_dictionary_builders_are_retired(name):
    assert not hasattr(vis, name)


@pytest.mark.parametrize(
    "field",
    [
        "symbols",
        "providers",
        "slash_commands",
        "op_hooks",
        "language_tools",
        "network_filters",
    ],
)
def test_declarations_refuse_untyped_children(field, monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    with pytest.raises(TypeError):
        vis.Extension(name="typed", description="Typed", alias="typed", **{field: [{}]})
    assert vis._registration["spec"] is None


def test_register_refuses_raw_maps_and_failure_does_not_register(monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    with pytest.raises(TypeError):
        vis.register_extension({"name": "not-a-declaration"})
    extension = vis.Extension(name="typed", description="Typed")

    def fail(_):
        raise RuntimeError("host unavailable")

    monkeypatch.setattr(vis._host, "declare_env", fail)
    with pytest.raises(RuntimeError, match="host unavailable"):
        vis.register_extension(extension)
    assert vis._registration["spec"] is None


@pytest.mark.parametrize(
    "class_name, kwargs",
    [
        ("ActivityHeading", {"text": "Build"}),
        ("ActivityText", {"text": "Working"}),
        ("ActivityMarkdown", {"text": "**Result**"}),
        ("ActivityCode", {"text": "print(1)", "language": "python"}),
        ("ActivityDiff", {"text": "+new", "language": "diff"}),
        ("ActivityTable", {"columns": ["name"], "rows": [["one"]]}),
        ("ActivityImage", {"attachment_id": "attachment-1", "label": "Image"}),
        ("ActivityVideo", {"attachment_id": "attachment-1", "label": "Video"}),
        ("ActivityAudio", {"attachment_id": "attachment-1", "label": "Audio"}),
        ("ActivityFile", {"attachment_id": "attachment-1", "label": "File"}),
        ("ActivityProgress", {"label": "Building"}),
        ("ActivityProgress", {"label": "Building", "value": 1, "total": 2}),
    ],
)
def test_activity_blocks_are_immutable_and_match_canonical_schema(class_name, kwargs):
    block = getattr(vis, class_name)(**kwargs)
    assert is_dataclass(block)
    assert not hasattr(block, "__dict__")
    wire = block.to_wire()
    assert _contracts.validate("activity", "content", [wire]) == [wire]
    for field in fields(block):
        with pytest.raises(FrozenInstanceError):
            setattr(block, field.name, getattr(block, field.name))
    if class_name == "ActivityTable":
        kwargs["rows"][0][0] = "changed"
        assert block.rows == (("one",),)
        assert block.to_wire()["rows"] == [["one"]]


@pytest.mark.parametrize(
    "class_name, kwargs",
    [
        ("ActivityText", {"text": 7}),
        ("ActivityImage", {"attachment_id": " ", "label": "Image"}),
        ("ActivityProgress", {"label": "Work", "value": 1}),
        ("ActivityProgress", {"label": "Work", "value": True, "total": 2}),
        ("ActivityProgress", {"label": "Work", "value": 3, "total": 2}),
        ("ActivityProgress", {"label": "Work", "value": float("nan"), "total": 2}),
        ("ActivityTable", {"columns": ["a"], "rows": [["a", "b"]]}),
    ],
)
def test_invalid_activity_blocks_are_refused(class_name, kwargs):
    with pytest.raises((TypeError, ValueError)):
        getattr(vis, class_name)(**kwargs)


def test_activity_publish_is_typed_and_preserves_host_result(monkeypatch):
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda blocks: updates.append(blocks) or True
    )
    view = vis.ActivityPresentation(
        "Build", "1 of 2", (vis.ActivityProgress("Build", value=1, total=2),)
    )
    assert vis.publish_activity(view)
    assert updates == [view.to_wire()]
    assert _contracts.validate("activity", "presentation", updates[0]) == updates[0]
    assert not vis.publish_activity({"type": "text", "text": "retired raw block"})
    assert len(updates) == 1
    assert vis.publish_activity(vis.ActivityPresentation("Build", "Complete"))
    assert updates[-1]["content"] == []
    monkeypatch.setattr(vis._host, "activity", lambda _: False)
    assert not vis.publish_activity(view)


@pytest.mark.parametrize(
    "summary_format", [None, *_contracts.definition("activity", "text_format")["enum"]]
)
def test_presentation_summary_format_is_explicit_and_portable(summary_format):
    # Regression #254: Markdown links need explicit format metadata, not guessing.
    summary = "Issues: [#252](https://github.com/Blockether/vis/issues/252)"
    section = vis.ActivitySection("Matches", summary, summary_format=summary_format)
    view = vis.ActivityPresentation(
        "Find issues", summary, (), (section,), summary_format=summary_format
    )
    wire = view.to_wire()
    assert wire["summary"] == summary
    assert wire["sections"][0]["summary"] == summary
    assert _contracts.validate("activity", "presentation", wire) == wire
    if summary_format is None:
        assert "summary_format" not in wire
        assert "summary_format" not in wire["sections"][0]
    else:
        assert wire["summary_format"] == summary_format
        assert wire["sections"][0]["summary_format"] == summary_format


@pytest.mark.parametrize("record", [vis.ActivitySection, vis.ActivityPresentation])
@pytest.mark.parametrize("summary", ["two\nlines", "é" * 257])
def test_markdown_summary_keeps_one_line_byte_limit(record, summary):
    with pytest.raises(ValueError):
        record("Issues", summary, summary_format="markdown")


@pytest.mark.parametrize("record", [vis.ActivitySection, vis.ActivityPresentation])
@pytest.mark.parametrize("summary_format", ["html", "", True, 7, []])
def test_invalid_summary_format_is_refused(record, summary_format):
    with pytest.raises(ValueError):
        record("Issues", "", summary_format=summary_format)


@pytest.mark.parametrize(
    "kwargs",
    [
        {"headline": "two\nlines", "summary": ""},
        {"headline": "Build", "summary": "two\rlines"},
        {"headline": "Build", "summary": "é" * 257},
        {"headline": "", "summary": "Ready"},
        {
            "headline": "Build",
            "summary": "",
            "sections": [vis.ActivityPresentation("nested", "")],
        },
    ],
)
def test_presentation_refuses_multiline_or_invalid_structure(kwargs):
    with pytest.raises((ValueError, TypeError)):
        vis.ActivityPresentation(**kwargs)


def test_presentation_sections_are_typed_immutable_and_portable():
    blocks = [vis.ActivityText("Details")]
    sections = [vis.ActivitySection("src", "2 files", blocks)]
    view = vis.ActivityPresentation("Listed 1 directory", "2 files", sections=sections)
    blocks.clear()
    sections.clear()
    wire = view.to_wire()
    assert len(wire["sections"][0]["content"]) == 1
    assert _contracts.validate("activity", "presentation", wire) == wire
    wire["sections"][0]["summary"] = "changed"
    assert view.sections[0].summary == "2 files"
    with pytest.raises(FrozenInstanceError):
        view.summary = "changed"


@pytest.mark.parametrize(
    "ops, phase",
    [
        (["fs_access", "shell"], "before"),
        (["fs_access"], "after"),
        (["shell"], "during"),
    ],
)
def test_gate_declarations_reject_invalid_execution_semantics(ops, phase):
    with pytest.raises(ValueError):
        vis.OpHook(ops, lambda event: None, phase=phase)


def test_language_surface_capabilities_follow_the_canonical_contract():
    contract = _contracts.definition("surface", "language_surface")
    assert set(vis._LANGUAGE_CAPABILITIES) == set(
        contract["properties"]["capabilities"]["items"]["enum"]
    )


@pytest.mark.parametrize(
    "kwargs, error",
    [
        ({"language": "TOML", "format": format_toml}, ValueError),
        ({"language": "", "format": format_toml}, ValueError),
        ({"language": "toml"}, ValueError),
        (
            {"language": "toml", "extensions": ["toml", ""], "format": format_toml},
            ValueError,
        ),
        ({"language": "toml", "extensions": "toml", "format": format_toml}, ValueError),
        (
            {"language": "toml", "is_exact_syntax": "yes", "format": format_toml},
            TypeError,
        ),
        ({"language": "toml", "syntax": "parser"}, TypeError),
    ],
)
def test_language_surface_refuses_an_unserviceable_language(kwargs, error):
    with pytest.raises(error):
        vis.LanguageSurface(**kwargs)


def test_registration_snapshots_collections_and_provider_config(monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    monkeypatch.setattr(vis._host, "declare_env", lambda _: "{}")
    body = {"models": [{"name": "example", "flags": ["tool"]}]}
    preset = vis.ProviderPreset(extra_body=body)
    provider = vis.Provider("example", "Example", preset=preset, is_managed=True)
    symbols = [vis.Symbol(greet)]
    extension = vis.Extension(
        name="snapshot",
        description="Snapshot",
        alias="snap",
        symbols=symbols,
        providers=[provider],
    )
    symbols.clear()
    body["models"][0]["flags"].append("changed")
    with pytest.raises(TypeError):
        provider.preset.extra_body["models"][0]["name"] = "changed"
    vis.register_extension(extension)
    wire = vis._registration["spec"]
    assert len(wire["symbols"]) == 1
    assert wire["providers"][0]["is_managed"] is True
    assert wire["providers"][0]["preset"]["extra_body"] == {
        "models": [{"name": "example", "flags": ["tool"]}]
    }
    wire["providers"][0]["preset"]["extra_body"]["models"].clear()
    assert provider.preset.extra_body["models"][0]["name"] == "example"
