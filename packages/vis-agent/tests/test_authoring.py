"""Issue #176: the authoring pages use one executable package, not drifting snippets."""

import re
import runpy
import sys
from dataclasses import FrozenInstanceError
from pathlib import Path
from types import ModuleType

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts, extension_package

REPOSITORY = Path(__file__).resolve().parents[3]
EXAMPLE = REPOSITORY / "packages/vis-agent/examples/greeter"
DOCS = REPOSITORY / "resources/vis-docs"


@pytest.mark.parametrize(
    "page, source, language",
    [
        ("extension-design.md", "extension.py", "python"),
        ("extension-design.md", "src/vis_greeter/__init__.py", "python"),
        ("extension-packages.md", "pyproject.toml", "toml"),
    ],
)
def test_example_snippets_match_the_tested_source(page, source, language):
    snippet = (EXAMPLE / source).read_text().rstrip()
    assert f"```{language}\n{snippet}\n```" in (DOCS / page).read_text()


def test_example_manifest_registration_and_domain_behavior(monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    monkeypatch.syspath_prepend(str(EXAMPLE / "src"))
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda value: updates.append(value) or True
    )
    metadata = extension_package.inspect_source(EXAMPLE)
    assert metadata["name"] == "vis-greeter"
    assert metadata["skills"] == ["skills/greeting"]
    assert vis._registration["spec"] is None
    runpy.run_path(str(EXAMPLE / "extension.py"))
    declaration = vis._registration["spec"]
    tool = declaration["symbols"][0]["methods"][0]
    assert tool["contract"]["name"] == "greet.hello"
    assert _contracts.validate("symbol", "callable", tool["contract"])
    assert "Unicode code points" in tool["doc"]
    assert tool["activity"]["label"] == "Greet person"
    assert tool["activity"]["show_start"] is False
    assert _contracts.validate("activity", "declaration", tool["activity"])
    result = tool["fn"]("Ada")
    assert result.text == "Hello, Ada!"
    assert len(updates) == 1
    assert updates[-1]["headline"] == "Greet person"
    assert updates[-1]["content"] == [{"type": "text", "text": "Hello, Ada!"}]
    domain_tests = runpy.run_path(str(EXAMPLE / "tests/test_greeter.py"))
    for name, test in domain_tests.items():
        if name.startswith("test_"):
            test()


def test_documented_cross_module_wrapper_example(monkeypatch):
    # #179: execute the actual guide, not a second implementation of its example.
    snippets = re.findall(
        r"```python\n# (decorators|tools)\.py\n(.*?)```",
        (DOCS / "extension-api.md").read_text(),
        re.S,
    )
    assert [name for name, _ in snippets] == ["decorators", "tools"]
    modules = {}
    for name, source in snippets:
        module = ModuleType(name)
        monkeypatch.setitem(sys.modules, name, module)
        exec(compile(source, f"{name}.py", "exec", dont_inherit=True), module.__dict__)
        modules[name] = module

    tools = modules["tools"].Tools()
    contract = vis.Symbol(tools, name="tools").contract["members"][0]
    assert _contracts.validate("symbol", "callable", contract) == contract
    assert contract["signature"] == "(text: str = ...) -> tuple['Result', ...]"
    result_type = contract["returns"]
    assert result_type["name"] == "tuple"
    assert result_type["variadic"] is True
    (record,) = result_type["arguments"]
    assert record["kind"] == "record"
    assert record["name"] == "Result"
    (text,) = record["fields"]
    assert text["name"] == "text"
    assert text["type"] == {"kind": "scalar", "name": "str"}
    result = tools.read()
    assert result[0].text == "ready"
    with pytest.raises(FrozenInstanceError):
        result[0].text = "changed"


@pytest.mark.parametrize("page", ["extension-development.md", "extension-packages.md"])
def test_documented_package_entry_owns_its_end_only_activity(monkeypatch, page):
    domain_source = re.search(
        r"```python\n# einmal/src/einmal/__init__\.py\n(.*?)\n```",
        (DOCS / "extension-development.md").read_text(),
        re.S,
    )[1]
    domain = ModuleType("einmal")
    monkeypatch.setitem(sys.modules, "einmal", domain)
    exec(compile(domain_source, "einmal/__init__.py", "exec"), domain.__dict__)
    entry_source = re.search(
        r"```python\n# \.vis/extensions/einmal_tools\.py\n(.*?)\n```",
        (DOCS / page).read_text(),
        re.S,
    )[1]
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda value: updates.append(value) or True
    )
    exec(compile(entry_source, page, "exec"), {})
    (tool,) = vis._registration["spec"]["symbols"]
    assert tool["activity"]["show_start"] is False
    assert tool["activity"]["label"] == "Check integration status"
    assert tool["fn"]() == "ready"
    assert updates == [
        {
            "headline": "Check integration status",
            "summary": "Status: ready",
            "content": [],
        }
    ]


def test_quickstart_runs_as_one_project_extension(monkeypatch):
    document = (DOCS / "extending.md").read_text()
    match = re.search(
        r"```python\n# \.vis/extensions/greeting_tools\.py\n(.*?)\n```",
        document,
        re.S,
    )
    assert match, "The first-extension tutorial must include the complete entry file"
    module = ModuleType("greeting_tools")
    monkeypatch.setitem(sys.modules, module.__name__, module)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    exec(
        compile(match[1], "greeting_tools.py", "exec", dont_inherit=True),
        module.__dict__,
    )
    declaration = vis._registration["spec"]
    assert declaration["name"] == "greeting"
    (tool,) = declaration["symbols"]
    assert tool["contract"]["name"] == "hello"
    assert _contracts.validate("symbol", "callable", tool["contract"])
    # #232: prose explains behavior; the generated contract supplies call structure.
    assert "Preserves capitalization unless uppercase is requested." in tool["doc"]
    assert "uppercase: bool (keyword_only; default omitted)" in tool["doc"]
    assert module.hello(" Ada ") == "Hello, Ada!"
    assert module.hello("Ada", uppercase=True) == "HELLO, ADA!"
    with pytest.raises(ValueError, match="blank"):
        module.hello(" ")


def test_greeter_derives_call_shape_with_a_semantic_only_docstring(monkeypatch):
    # #232: a short method docstring must preserve discovery, types and omission.
    monkeypatch.syspath_prepend(str(EXAMPLE / "src"))
    from vis_greeter import Greeter

    greeter = Greeter()
    symbol = vis.Symbol(greeter, name="greet")
    catalog = vis.Catalog([symbol])
    contract = symbol.contract["members"][0]
    spec = catalog.spec("greet.hello")
    document = catalog.help("greet.hello").text
    assert "Preserves capitalization unless uppercase is requested." in spec.description
    assert "False" not in spec.description
    assert (
        spec.signature
        == contract["signature"]
        == "(name: str, *, uppercase: bool = ...) -> 'Greeting'"
    )
    name, uppercase = spec.parameters
    assert name.required and name.type.name == "str"
    assert uppercase.kind == "keyword_only" and uppercase.type.name == "bool"
    assert uppercase.has_default and not uppercase.required
    assert not uppercase.default_is_none
    assert spec.returns.name == "Greeting"
    assert [field.name for field in spec.returns.fields] == ["text", "characters"]
    assert "greet.hello(name: str, *, uppercase: bool = ...) -> 'Greeting'" in document
    assert "Effect: observation" in document
    assert "uppercase: bool (keyword_only; default omitted)" in document
    assert "Returns: Greeting" in document
    assert "Unicode code points" in document
    vis.testing.assert_catalog(catalog, names=["greet.hello"])
    assert greeter.hello("Ada").text == "Hello, Ada!"
    assert greeter.hello("Ada", uppercase=True).text == "HELLO, ADA!"
    with pytest.raises(TypeError):
        greeter.hello("Ada", True)


def test_documented_provider_loads_without_network_or_login(monkeypatch):
    document = (DOCS / "provider-extensions.md").read_text()
    source = re.search(r"```python\n(.*?)\n```", document, re.S)[1]
    from types import SimpleNamespace

    monkeypatch.setattr(vis, "_host", SimpleNamespace(declare_env=lambda _: "{}"))
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    monkeypatch.delenv("EXAMPLE_API_KEY", raising=False)
    namespace = {}
    exec(compile(source, "example_provider.py", "exec"), namespace)
    assert vis._registration["spec"]["name"] == "provider-example"
    assert namespace["credential"]() is None
    assert not namespace["status"]().is_authenticated
    monkeypatch.setenv("EXAMPLE_API_KEY", "public-test-fixture")
    assert namespace["credential"]().token == "public-test-fixture"
    assert namespace["status"]().is_authenticated
