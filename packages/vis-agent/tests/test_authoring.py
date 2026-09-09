"""Issue #176: the authoring pages use one executable package, not drifting snippets."""

import runpy
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts, extension_package

REPOSITORY = Path(__file__).resolve().parents[3]
EXAMPLE = REPOSITORY / "packages/vis-agent/examples/greeter"
DOCS = REPOSITORY / "resources/vis-docs"


@pytest.mark.parametrize(
    "page, source, language",
    [
        ("extending.md", "extension.py", "python"),
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
    domain_tests = runpy.run_path(str(EXAMPLE / "tests/test_greeter.py"))
    for name, test in domain_tests.items():
        if name.startswith("test_"):
            test()
