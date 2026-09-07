"""Typed declarations are pure values until the explicit registration boundary."""

from dataclasses import FrozenInstanceError, fields, is_dataclass

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


def greet(name: str, *, loud: bool = False):
    """Greet one person."""
    return name.upper() if loud else name


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
    extension = vis.Extension(
        name="greeter",
        description="Greeting tools",
        alias="greet",
        symbols=[symbol],
        providers=[provider],
        slash_commands=[command],
        op_hooks=[hook],
        network_filters=[network_filter],
        env=["SDK_EXAMPLE_ENV"],
    )
    assert calls == []
    assert vis._registration["spec"] is None
    assert extension.symbols == (symbol,)
    assert hook.ops == ("shell",)
    for value in (symbol, provider, command, hook, network_filter, extension):
        assert is_dataclass(value)
        assert not hasattr(value, "__dict__")
        assert all(field.type for field in fields(value))
        for field in fields(value):
            with pytest.raises(FrozenInstanceError):
                setattr(value, field.name, getattr(value, field.name))
    vis.register(extension)
    assert len(calls) == 1
    wire = vis._registration["spec"]
    assert wire["symbols"][0]["marker"] == "symbol"
    assert wire["symbols"][0]["fn"]("Ada", {"loud": True}) == "ADA"
    assert wire["providers"][0]["marker"] == "provider"
    assert wire["slash_commands"][0]["marker"] == "slash"
    assert wire["op_hooks"][0]["ops"] == ["shell"]
    assert wire["network_filters"][0]["marker"] == "network_filter"
    with pytest.raises(ValueError, match="once per file"):
        vis.register(extension)
    assert len(calls) == 1


@pytest.mark.parametrize(
    "name", ["extension", "symbol", "slash", "provider", "op_hook", "network_filter"]
)
def test_dictionary_builders_are_retired(name):
    assert not hasattr(vis, name)


@pytest.mark.parametrize(
    "field", ["symbols", "providers", "slash_commands", "op_hooks", "network_filters"]
)
def test_declarations_refuse_untyped_children(field, monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    with pytest.raises(TypeError):
        vis.Extension(name="typed", description="Typed", alias="typed", **{field: [{}]})
    assert vis._registration["spec"] is None


def test_register_refuses_raw_maps_and_failure_does_not_register(monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    with pytest.raises(TypeError):
        vis.register({"name": "not-a-declaration"})
    extension = vis.Extension(name="typed", description="Typed")

    def fail(_):
        raise RuntimeError("host unavailable")

    monkeypatch.setattr(vis._host, "declare_env", fail)
    with pytest.raises(RuntimeError, match="host unavailable"):
        vis.register(extension)
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
        ("ActivityText", {"text": "x" * 16385}),
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


def test_activity_publish_is_typed_bounded_and_preserves_host_result(monkeypatch):
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
    with pytest.raises((ValueError, TypeError)):
        vis.ActivityPresentation("Build", "", [vis.ActivityText("x")] * 33)
    with pytest.raises(ValueError):
        vis.ActivityPresentation("Build", "", [vis.ActivityText("é" * 16384)] * 2)
    assert len(updates) == 1
    assert vis.publish_activity(vis.ActivityPresentation("Build", "Complete"))
    assert updates[-1]["content"] == []
    monkeypatch.setattr(vis._host, "activity", lambda _: False)
    assert not vis.publish_activity(view)


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
            "sections": [vis.ActivitySection("src", "Ready")] * 9,
        },
        {
            "headline": "Build",
            "summary": "",
            "sections": [vis.ActivityPresentation("nested", "")],
        },
        {
            "headline": "Build",
            "summary": "",
            "content": [vis.ActivityText("x")] * 17,
            "sections": [vis.ActivitySection("src", "", [vis.ActivityText("y")] * 16)],
        },
    ],
)
def test_presentation_refuses_multiline_or_unbounded_structure(kwargs):
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
    vis.register(extension)
    wire = vis._registration["spec"]
    assert len(wire["symbols"]) == 1
    assert wire["providers"][0]["is_managed"] is True
    assert wire["providers"][0]["preset"]["extra_body"] == {
        "models": [{"name": "example", "flags": ["tool"]}]
    }
    wire["providers"][0]["preset"]["extra_body"]["models"].clear()
    assert provider.preset.extra_body["models"][0]["name"] == "example"
