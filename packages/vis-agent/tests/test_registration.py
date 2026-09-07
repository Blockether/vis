"""Extension declarations work with the same API in a checkout and an installed wheel."""

import runpy
from dataclasses import FrozenInstanceError

import blockether.vis.extension as vis
import pytest


@pytest.fixture(autouse=True)
def fresh_registration(monkeypatch):
    """Each test models one extension file, without leaking its declaration."""
    monkeypatch.setattr(vis, "_registration", {"spec": None})


def test_importable_extension_exposes_typed_tools(tmp_path):
    source = tmp_path / "greeter.py"
    source.write_text(
        "\n".join(
            [
                "from dataclasses import dataclass",
                "import blockether.vis.extension as vis",
                "@dataclass(frozen=True)",
                "class Greeting:",
                '    """A greeting for one person."""',
                "    text: str",
                "def greet(name: str, *, loud: bool = False) -> Greeting:",
                '    """Greet one person, optionally in uppercase."""',
                '    text = "Hello " + name',
                "    return Greeting(text.upper() if loud else text)",
                'vis.register(vis.Extension(name="greeter", description="Greeting tools.",',
                '              alias="greet", symbols=[vis.Symbol(greet)]))',
            ]
        ),
        encoding="utf-8",
    )
    module = runpy.run_path(str(source))
    declaration = vis._registration["spec"]
    tool = declaration["symbols"][0]
    assert declaration["name"] == "greeter"
    assert tool["name"] == "greet"
    assert tool["doc"] == "Greet one person, optionally in uppercase."
    assert tool["params"] == ["name"]
    # Folded keyword arguments are how the engine invokes the same tool.
    result = tool["fn"]("Ada", {"loud": True})
    assert isinstance(result, module["Greeting"])
    assert result.text == "HELLO ADA"
    with pytest.raises(FrozenInstanceError):
        result.text = "changed"
    with pytest.raises(ValueError, match="once per file"):
        vis.register(
            vis.Extension(name="second", description="A duplicate declaration.")
        )


def test_object_tools_keep_method_metadata_and_raise_normally():
    class Tools:
        @vis.method(tag="mutation", is_hidden=True)
        def save(self, value: str) -> str:
            """Save a nonempty value."""
            if not value:
                raise ValueError("value is empty")
            return value

        def _private(self):
            raise AssertionError("private method must not be exposed")

    namespace = vis.Symbol(Tools(), name="tools")._spec()
    assert namespace["marker"] == "namespace"
    assert len(namespace["methods"]) == 1
    method = namespace["methods"][0]
    assert namespace["name"] + "." + method["name"] == "tools.save"
    assert method["tag"] == "mutation"
    assert method["hidden"] is True
    assert method["fn"]("value") == "value"
    with pytest.raises(ValueError, match="value is empty"):
        method["fn"]("")


@pytest.mark.parametrize(
    "kwargs, message",
    [
        ({"name": "", "description": "Tools."}, "requires name"),
        ({"name": "tools", "description": ""}, "requires description"),
        (
            {"name": "tools", "description": "Tools.", "symbols": [object()]},
            "requires alias",
        ),
        ({"name": "tools", "description": "Tools.", "ctx": {}}, "must be a callable"),
        ({"name": "tools", "description": "Tools.", "env": "TOKEN"}, "must be a list"),
    ],
)
def test_invalid_declarations_fail_before_registration(kwargs, message):
    with pytest.raises(ValueError, match=message):
        vis.register(vis.Extension(**kwargs))
    assert vis._registration["spec"] is None


@pytest.mark.parametrize("kwargs", [{"name": "tools"}, {"description": "Tools"}])
def test_required_declaration_fields_are_constructor_arguments(kwargs):
    with pytest.raises(TypeError, match="required keyword-only argument"):
        vis.Extension(**kwargs)
    assert vis._registration["spec"] is None


def test_extension_activity_example_runs_with_the_installed_sdk(monkeypatch):
    import sys
    import types

    example = """
from dataclasses import dataclass

import blockether.vis.extension as vis


@dataclass(frozen=True, slots=True)
class Greeting:
    text: str


def greet(name: str) -> Greeting:
    \"\"\"Greet one person and return a typed result.\"\"\"
    vis.publish_activity(
        vis.ActivityPresentation(
            "Greeting", "Preparing reply", (vis.ActivityProgress("Working"),)
        )
    )
    return Greeting(f"Hello, {name}!")


def greeting_activity(phase, result, **_) -> vis.ActivityPresentation:
    return vis.ActivityPresentation(
        "Greeting",
        phase,
        (vis.ActivityText(result.text if phase == "success" else "Preparing reply"),),
    )


vis.register(
    vis.Extension(
        name="greeter",
        description="Greeting tools.",
        alias="greeter",
        symbols=[vis.Symbol(greet, activity=vis.Activity(render=greeting_activity))],
    )
)
"""
    module = types.ModuleType("sdk_readme_example")
    monkeypatch.setitem(sys.modules, module.__name__, module)
    updates = []
    monkeypatch.setattr(vis._host, "declare_env", lambda _: "{}")
    monkeypatch.setattr(
        vis._host, "activity", lambda value: updates.append(value) or True
    )
    exec(compile(example, "extension_activity_example", "exec"), module.__dict__)
    assert updates == []
    tool = vis._registration["spec"]["symbols"][0]
    result = tool["fn"]("Ada")
    assert isinstance(result, module.Greeting)
    assert result.text == "Hello, Ada!"
    assert [update["summary"] for update in updates] == [
        "start",
        "Preparing reply",
        "success",
    ]
    assert updates[-1]["content"] == [{"type": "text", "text": result.text}]


def test_documented_extension_example_runs_with_the_installed_sdk(monkeypatch):
    import re
    from pathlib import Path

    readme = (Path(__file__).parents[1] / "README.md").read_text()
    example = re.search(r"```python\n(.*?)\n```", readme, re.DOTALL).group(1)
    monkeypatch.setattr(vis._host, "declare_env", lambda _: "{}")
    exec(compile(example, "README.md", "exec"), {})
    tool = vis._registration["spec"]["symbols"][0]
    assert tool["fn"]("Ada") == "Hello, Ada!"
