"""Issue #203: execute the generated-help recipe, not a separately maintained demo."""

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
        r"```python\n# counter.py\n(.*?)\n```", document.read_text(), re.S
    )[1]
    module = ModuleType("counter_recipe")
    monkeypatch.setitem(sys.modules, module.__name__, module)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    exec(compile(source, str(document), "exec"), module.__dict__)
    return module, vis._registration["spec"]


def test_registered_recipe_uses_one_catalog_and_typed_methods(recipe, tmp_path):
    module, registration = recipe
    tools = registration["symbols"][0]["methods"]
    names = [item["contract"]["name"] for item in tools if not item["hidden"]]
    vis.testing.assert_catalog(module.catalog, names=names, mutations=["counter.write"])
    by_name = {item["name"]: item["fn"] for item in tools}
    path = str(tmp_path / "count.txt")
    assert by_name["read"](path).value == 0
    assert not Path(path).exists()
    result = by_name["write"](path, value=3)
    assert by_name["read"](path).value == 3
    assert isinstance(result, module.Count)
    with pytest.raises(FrozenInstanceError):
        result.value = 9
    with pytest.raises(TypeError):
        by_name["write"](path, 9)
    for tool in tools:
        assert tool["doc"] in module.catalog.help(tool["contract"]["name"]).text


def test_recipe_validates_before_io_and_keeps_operational_errors_explicit(
    recipe, monkeypatch
):
    module, _ = recipe
    calls = []

    def fail(*args, **kwargs):
        calls.append(args)
        raise OSError("fixture write failure")

    monkeypatch.setattr(Path, "write_text", fail)
    counter = module.Counter()
    for value, exception in ((-1, ValueError), (True, TypeError), ("1", TypeError)):
        with pytest.raises(exception):
            counter.write("count.txt", value=value)
    assert calls == []
    with pytest.raises(module.CounterError) as error:
        counter.write("count.txt", value=1)
    assert isinstance(error.value.__cause__, OSError)
    assert len(calls) == 1


def test_recipe_activity_covers_running_success_failure_and_zero(recipe):
    module, _ = recipe
    for phase in ("start", "success", "failure"):
        result = module.present_count(
            phase=phase, result=module.Count(0), error=RuntimeError("read failed")
        )
        assert isinstance(result, vis.ActivityPresentation)
        if phase == "success":
            assert "0 completed" in result.summary
        elif phase == "failure":
            assert "read failed" in result.summary
