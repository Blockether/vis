"""Execute the documented public-SDK Activity recipe, including empty and error states."""

import json
import re
import sys
from dataclasses import FrozenInstanceError
from pathlib import Path
from types import ModuleType

import blockether.vis.extension as vis
import pytest


@pytest.fixture
def recipe(monkeypatch):
    document = Path(__file__).parents[3] / "resources/vis-docs/extension-design.md"
    source = re.search(
        r"```python\n# ci_report.py\n(.*?)\n```", document.read_text(), re.S
    )[1]
    module = ModuleType("ci_report_recipe")
    monkeypatch.setitem(sys.modules, module.__name__, module)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    exec(compile(source, str(document), "exec"), module.__dict__)
    return module, vis._registration["spec"]


def test_recipe_registers_a_typed_read_without_changing_the_report(recipe, tmp_path):
    module, registration = recipe
    assert registration["name"] == "ci-report-example"
    path = tmp_path / "report.json"
    source = '{"passed": 42, "failed": 1, "build": "example"}'
    path.write_text(source)
    report = module.read_ci_report(str(path))
    assert isinstance(report, module.CIReport)
    assert (report.path, report.passed, report.failed) == (str(path), 42, 1)
    assert path.read_text() == source
    with pytest.raises(FrozenInstanceError):
        report.failed = 0


@pytest.mark.parametrize(
    ("data", "message"),
    [
        ([], "report must be a JSON object"),
        ({}, "passed must be a nonnegative integer"),
        ({"passed": 1}, "failed must be a nonnegative integer"),
        ({"passed": -1, "failed": 0}, "passed must be a nonnegative integer"),
        ({"passed": True, "failed": 0}, "passed must be a nonnegative integer"),
        ({"passed": 1, "failed": False}, "failed must be a nonnegative integer"),
        ({"passed": 1, "failed": "0"}, "failed must be a nonnegative integer"),
        ({"passed": 1, "failed": 0.5}, "failed must be a nonnegative integer"),
    ],
)
def test_recipe_rejects_invalid_summaries(recipe, tmp_path, data, message):
    module, _ = recipe
    path = tmp_path / "report.json"
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match=message):
        module.read_ci_report(str(path))


def test_recipe_keeps_path_file_encoding_and_json_errors_explicit(recipe, tmp_path):
    module, _ = recipe
    with pytest.raises(ValueError, match="path must be absolute"):
        module.read_ci_report("report.json")
    path = tmp_path / "report.json"
    with pytest.raises(FileNotFoundError):
        module.read_ci_report(str(path))
    path.write_text("not JSON")
    with pytest.raises(json.JSONDecodeError):
        module.read_ci_report(str(path))
    path.write_bytes(b"\xff")
    with pytest.raises(UnicodeDecodeError):
        module.read_ci_report(str(path))


def test_recipe_activity_covers_running_failures_counts_and_empty_results(
    recipe, tmp_path
):
    module, _ = recipe
    running = module.present_report(phase="start", args=(), kwargs={})
    assert isinstance(running, vis.ActivityPresentation)
    assert running.headline == "Read CI report"
    assert running.summary == "Reading local test results"
    path = tmp_path / "report.json"
    for passed, failed, expected in (
        (42, 1, "42 passed · 1 failed"),
        (42, 0, "42 passed · 0 failed"),
        (0, 1, "0 passed · 1 failed"),
        (0, 0, "No tests reported"),
    ):
        path.write_text(json.dumps({"passed": passed, "failed": failed}))
        report = module.read_ci_report(str(path))
        shown = module.present_report(phase="success", result=report)
        assert shown.headline == "Read CI report"
        assert shown.summary == expected
        assert shown.content == (vis.ActivityText(f"Source: {path}"),)
        assert (report.passed, report.failed) == (passed, failed)
    error = ValueError("failed must be a nonnegative integer")
    shown = module.present_report(phase="failure", error=error)
    assert shown.summary == "Could not read test results"
    assert shown.content == (vis.ActivityText(f"ValueError: {error}"),)


def test_recipe_labels_long_error_excerpts_without_changing_the_exception(recipe):
    module, _ = recipe
    message = "x" * 2000
    error = RuntimeError(message)
    shown = module.present_report(phase="failure", error=error)
    assert shown.content == (
        vis.ActivityText(("RuntimeError: " + message)[:1000] + "\n[Error excerpt]"),
    )
    assert str(error) == message
