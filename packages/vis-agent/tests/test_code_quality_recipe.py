"""Execute the documented SDK nesting hook rather than a separate example."""

import ast
import re
import sys
from pathlib import Path
from types import ModuleType

import blockether.vis.extension as vis
import pytest


@pytest.fixture
def recipe(tmp_path, monkeypatch):
    document = Path(__file__).parents[3] / "resources/vis-docs/extension-design.md"
    source = re.search(
        r"```python\n# code_quality.py\n(.*?)\n```", document.read_text(), re.S
    )[1]
    entry = tmp_path / ".vis/extensions/code_quality.py"
    entry.parent.mkdir(parents=True)
    entry.write_text(source)
    (tmp_path / "src").mkdir()
    module = ModuleType("nesting_recipe")
    module.__file__ = str(entry)
    monkeypatch.setitem(sys.modules, module.__name__, module)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    exec(compile(source, str(entry), "exec"), module.__dict__)
    return module, vis._registration["spec"]


def test_recipe_registers_both_edit_boundaries_and_supplies_context(recipe):
    module, registration = recipe
    (hook,) = registration["op_hooks"]
    assert hook["ops"] == ["patch", "python_execution"]
    assert hook["phase"] == "after"
    assert "not been checked" in module.context({})["code_quality"]["findings"][0]
    hook["fn"]({"op": "python_execution", "args": [], "result": {"stdout": ""}})
    assert module.context({})["code_quality"] == {
        "checked": 0,
        "limit": 3,
        "findings": [],
    }
    readme = Path(__file__).parents[3] / "README.md"
    assert (
        "resources/vis-docs/extension-design.md#check-code-complexity-after-edits"
        in readme.read_text()
    )


@pytest.mark.parametrize(
    ("source", "depth", "line"),
    [
        ("value = 1\n", 0, 1),
        ("if a and b:\n    pass\n", 1, 1),
        (
            "if a:\n    if b:\n        if c:\n            if d:\n                pass\n",
            4,
            4,
        ),
        ("if a:\n    pass\nelif b:\n    pass\n", 2, 3),
        ("for a in items:\n    while a:\n        break\n", 2, 2),
        (
            "try:\n    with manager():\n        pass\nexcept Exception:\n    pass\n",
            2,
            2,
        ),
        ("try:\n    pass\nexcept* Exception:\n    pass\n", 1, 1),
        ("match value:\n    case 1:\n        if a:\n            pass\n", 2, 3),
        (
            "async def f():\n    async for a in items:\n        async with a:\n            pass\n",
            2,
            3,
        ),
        ("if a:\n    def f():\n        if b:\n            pass\n", 1, 1),
        ("if a:\n    class C:\n        if b:\n            pass\n", 1, 1),
        (
            "if a:\n    async def f():\n        if b:\n            if c:\n                pass\n",
            2,
            4,
        ),
        ("items = [x for x in values if x]\n", 0, 1),
        ("f = lambda x: x if x else 0\n", 0, 1),
    ],
)
def test_metric_matches_its_documented_scope(recipe, source, depth, line):
    module, _ = recipe
    assert module.deepest(ast.parse(source)) == (depth, line)


def test_scan_reports_locations_and_clears_fixed_or_deleted_files(recipe):
    module, _ = recipe
    path = module.SOURCE / "orders.py"
    path.write_text(
        "if a:\n    if b:\n        if c:\n            if d:\n                pass\n"
    )
    assert module.scan() == {
        "checked": 1,
        "limit": 3,
        "findings": ["src/orders.py:4: nesting 4 exceeds 3"],
    }
    path.write_text("value = 1\n")
    assert module.scan()["findings"] == []
    path.unlink()
    assert module.scan() == {"checked": 0, "limit": 3, "findings": []}


def test_scan_does_not_present_parse_or_read_errors_as_success(recipe, monkeypatch):
    module, _ = recipe
    path = module.SOURCE / "broken.py"
    path.write_text("if :\n")
    report = module.scan()
    assert report["checked"] == 0
    assert "src/broken.py: could not check:" in report["findings"][0]

    def unreadable(self):
        raise PermissionError("fixture denied")

    monkeypatch.setattr(Path, "read_bytes", unreadable)
    assert "fixture denied" in module.scan()["findings"][0]


def test_scan_reports_missing_source_directory_and_isolates_project_state(recipe):
    module, _ = recipe
    module.SOURCE.rmdir()
    assert "Expected a readable src/ directory" in module.scan()["findings"][0]
    vis.state["nesting:/another-project"] = {"findings": []}
    assert "not been checked" in module.context({})["code_quality"]["findings"][0]


def test_registered_check_presents_the_edit_check_fix_cycle(recipe, monkeypatch):
    module, registration = recipe
    (symbol,) = registration["symbols"]
    assert symbol["name"] == "check_nesting"
    assert symbol["activity"]["show_start"] is False
    shown = []
    monkeypatch.setattr(
        vis, "publish_activity", lambda presentation: shown.append(presentation)
    )
    path = module.SOURCE / "orders.py"
    path.write_text(
        "if a:\n    if b:\n        if c:\n            if d:\n                pass\n"
    )
    report = symbol["fn"]()
    assert report == module.context({})["code_quality"]
    assert report["findings"] == ["src/orders.py:4: nesting 4 exceeds 3"]
    assert len(shown) == 1  # This quick local check has no running presentation.
    assert shown[-1].headline == "Check code nesting"
    assert shown[-1].summary == "1 file checked · 1 finding"
    assert "src/orders.py:4: nesting 4 exceeds 3" in shown[-1].content[-1].text
    path.write_text("value = 1\n")
    assert symbol["fn"]()["findings"] == []
    assert shown[-1].summary == "1 file checked · 0 findings"
    assert shown[-1].content[-1].text == "No nesting findings."
    path.unlink()
    assert symbol["fn"]()["checked"] == 0
    assert shown[-1].summary == "No Python files found"


def test_nesting_activity_preserves_scan_errors_and_bounds_only_presentation(recipe):
    module, _ = recipe
    (module.SOURCE / "broken.py").write_text("if :\n")
    report = module.check_nesting()
    shown = module.present_nesting(phase="success", result=report)
    assert shown.summary == "0 files checked · 1 finding"
    assert "could not check" in shown.content[-1].text
    assert "No nesting findings" not in shown.content[-1].text
    report = {"checked": 200, "limit": 3, "findings": ["x" * 7000]}
    shown = module.present_nesting(phase="success", result=report)
    assert len(shown.content[-1].text) < 6200
    assert (
        "[Excerpt; full findings are in the returned report.]" in shown.content[-1].text
    )
    assert report["findings"] == ["x" * 7000]
    assert module.present_nesting(phase="start").summary == "Scanning src/**/*.py"
    error = RuntimeError("scan unavailable")
    shown = module.present_nesting(phase="failure", error=error)
    assert shown.summary == "Could not check code nesting"
    assert shown.content[-1].text == "RuntimeError: scan unavailable"
