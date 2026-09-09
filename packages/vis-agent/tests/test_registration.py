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
    with pytest.raises(ValueError, match="once per file") as error:
        vis.register(
            vis.Extension(name="second", description="A duplicate declaration.")
        )
    assert "extension 'greeter' is already registered" in str(error.value)
    assert "Keep a single registration in the entrypoint" in str(error.value)
    assert vis._registration["spec"] is declaration


def test_thin_entry_registers_tool_from_separate_package(tmp_path, monkeypatch):
    """The guide's thin entry registers and calls an ordinary package function."""
    import sys

    package = tmp_path / "einmal" / "src" / "einmal"
    package.mkdir(parents=True)
    (package / "__init__.py").write_text(
        'def status() -> str:\n    """Return the company integration status."""\n    return "ready"\n',
        encoding="utf-8",
    )
    entry = tmp_path / ".vis" / "extensions" / "einmal.py"
    entry.parent.mkdir(parents=True)
    entry.write_text(
        'import blockether.vis.extension as vis\nfrom einmal import status\nvis.register(vis.Extension(name="einmal", description="Company tools.", alias="einmal", symbols=[vis.Symbol(status)]))\n',
        encoding="utf-8",
    )
    monkeypatch.syspath_prepend(str(package.parent))
    monkeypatch.delitem(sys.modules, "einmal", raising=False)
    try:
        module = runpy.run_path(str(entry))
        assert module["status"]() == "ready"
        declaration = vis._registration["spec"]
        assert declaration["name"] == "einmal"
        tool = declaration["symbols"][0]
        assert tool["name"] == "status"
        assert tool["doc"] == "Return the company integration status."
        assert tool["fn"]() == "ready"
    finally:
        sys.modules.pop("einmal", None)


@pytest.mark.parametrize("filename", ["issue176_demo.py", "issue176_bridge.py"])
def test_entrypoint_import_collision_has_actionable_error(
    tmp_path, monkeypatch, filename
):
    # Issue #176: an entrypoint can shadow the package imported by its tool.
    import sys

    package = tmp_path / "src" / "issue176_demo"
    package.mkdir(parents=True)
    (package / "__init__.py").write_text("", encoding="utf-8")
    (package / "core.py").write_text("VALUE = 'OK'\n", encoding="utf-8")
    entry = tmp_path / ".vis" / "extensions" / filename
    entry.parent.mkdir(parents=True)
    entry.write_text(
        "import importlib\nimport blockether.vis.extension as vis\n"
        "def ping():\n"
        "    'Return the implementation value.'\n"
        "    return importlib.import_module('issue176_demo.core').VALUE\n"
        "vis.register(vis.Extension(name='issue176-collision', "
        "description='Import collision fixture', alias='issue176_demo', "
        "symbols=[vis.Symbol(ping)]))\n",
        encoding="utf-8",
    )
    monkeypatch.syspath_prepend(str(package.parent))
    monkeypatch.syspath_prepend(str(entry.parent))
    module_names = ("issue176_demo", "issue176_demo.core")
    for name in module_names:
        monkeypatch.delitem(sys.modules, name, raising=False)
    try:
        runpy.run_path(str(entry))
        declaration = vis._registration["spec"]
        ping = declaration["symbols"][0]["fn"]
        import_path = list(sys.path)
        for _ in range(2):
            if filename == "issue176_demo.py":
                with pytest.raises(ValueError, match="once per file") as error:
                    ping()
                message = str(error.value)
                assert "extension 'issue176-collision' is already registered" in message
                assert "If this happened during an import" in message
                assert (
                    "may be shadowing a package or module with the same name" in message
                )
                assert "Rename the entrypoint" in message
                assert "demo.py -> demo_bridge.py" in message
                assert "public alias can stay unchanged" in message
            else:
                assert ping() == "OK"
            assert vis._registration["spec"] is declaration
            assert declaration["alias"] == "issue176_demo"
            assert sys.path == import_path
    finally:
        for name in module_names:
            sys.modules.pop(name, None)


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
