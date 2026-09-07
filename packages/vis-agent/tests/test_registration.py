"""Extension declarations work with the same API in a checkout and an installed wheel."""

import runpy
from dataclasses import FrozenInstanceError

import pytest
from blockether import vis


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
                "from blockether import vis",
                "@dataclass(frozen=True)",
                "class Greeting:",
                '    """A greeting for one person."""',
                "    text: str",
                "def greet(name: str, *, loud: bool = False) -> Greeting:",
                '    """Greet one person, optionally in uppercase."""',
                '    text = "Hello " + name',
                "    return Greeting(text.upper() if loud else text)",
                'vis.extension(name="greeter", description="Greeting tools.",',
                '              alias="greet", symbols=[vis.symbol(greet)])',
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
        vis.extension(name="second", description="A duplicate declaration.")


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

    namespace = vis.symbol(Tools(), name="tools")
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
        ({"description": "Tools."}, "requires name"),
        ({"name": "tools"}, "requires description"),
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
        vis.extension(**kwargs)
    assert vis._registration["spec"] is None
