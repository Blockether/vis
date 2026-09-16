"""Issue #253: bundled declarations share a loader, never an implicit host."""

import importlib
import importlib.util
import sys
from pathlib import Path
from types import ModuleType, SimpleNamespace

import pytest

REPOSITORY = Path(__file__).resolve().parents[3]
SOURCE = (REPOSITORY / "packages/vis-agent/src/blockether/vis/extension.py").read_text()


@pytest.fixture
def bootstrap(monkeypatch):
    parent = importlib.import_module("blockether.vis")
    monkeypatch.setattr(parent, "extension", parent.extension)
    monkeypatch.setitem(
        sys.modules, "blockether.vis.extension", sys.modules["blockether.vis.extension"]
    )
    monkeypatch.delitem(sys.modules, "blockether.vis._outside")
    spec = importlib.util.spec_from_file_location(
        "vis_sdk", REPOSITORY / "resources/vis-guest/vis_sdk.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.install


def test_bundled_module_replaces_installed_sdk_but_preserves_packages(
    bootstrap, monkeypatch
):
    parent = importlib.import_module("blockether.vis")
    namespace = importlib.import_module("blockether")
    search_path = parent.__path__
    sibling = ModuleType("blockether.sibling")
    monkeypatch.setitem(sys.modules, sibling.__name__, sibling)
    installed = sys.modules["blockether.vis.extension"]

    sdk = bootstrap(SOURCE)

    assert sdk is not installed
    assert importlib.import_module("blockether.vis.extension") is sdk
    assert parent.extension is sdk
    assert sys.modules["blockether"] is namespace
    assert parent.__path__ is search_path
    assert sys.modules[sibling.__name__] is sibling
    assert "blockether.vis._outside" not in sys.modules
    assert sdk.ActivityProgress("Inspect SDK", value=1, total=2).to_wire() == {
        "type": "progress",
        "label": "Inspect SDK",
        "value": 1,
        "total": 2,
    }


def test_explicit_extension_host_is_not_reused_by_sandbox(bootstrap):
    host = SimpleNamespace(
        declare_env=lambda _: "{}", state_get=lambda key: "trusted " + key
    )
    extension = bootstrap(SOURCE, host)
    extension.register_extension(
        extension.Extension(name="trusted", description="Explicit host")
    )
    assert extension._registration["spec"]["name"] == "trusted"
    assert extension.state["key"] == "trusted key"

    sandbox = bootstrap(SOURCE)

    for name in dir(sandbox.Host):
        if not name.startswith("_"):
            with pytest.raises(RuntimeError, match="unavailable in python_execution"):
                getattr(sandbox._host, name)
    assert sandbox._registration["spec"] is None
    assert "blockether.vis._outside" not in sys.modules
    assert extension.state["key"] == "trusted key"


@pytest.mark.parametrize("previous", [True, False])
def test_failed_load_restores_previous_module(bootstrap, monkeypatch, previous):
    parent = importlib.import_module("blockether.vis")
    installed = parent.extension
    if not previous:
        monkeypatch.delitem(sys.modules, "blockether.vis.extension")

    with pytest.raises(RuntimeError, match="broken source"):
        bootstrap("raise RuntimeError('broken source')")

    assert parent.extension is installed
    assert sys.modules.get("blockether.vis.extension") is (
        installed if previous else None
    )
