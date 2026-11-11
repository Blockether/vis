"""Issue #253: bundled declarations share a loader, never an implicit host.

Run by sdk-bootstrap-test in a real trusted runtime worker, not the SDK wheel.
"""

import importlib
import sys
from contextlib import contextmanager
from pathlib import Path
from types import ModuleType, SimpleNamespace

import vis_sdk

bootstrap = vis_sdk.install


@contextmanager
def raises(message):
    try:
        yield
    except RuntimeError as error:
        assert message in str(error), str(error)
    else:
        raise AssertionError("Expected RuntimeError: " + message)


def test_bundled_module_replaces_installed_sdk_but_preserves_packages(source):
    parent = importlib.import_module("blockether.vis")
    namespace = importlib.import_module("blockether")
    search_path = list(parent.__path__)
    sibling = ModuleType("blockether.sibling")
    sys.modules[sibling.__name__] = sibling
    installed = sys.modules["blockether.vis.extension"]

    sdk = bootstrap(source)

    assert sdk is not installed
    assert importlib.import_module("blockether.vis.extension") is sdk
    assert parent.extension is sdk
    assert sys.modules["blockether"] is namespace
    staged = str(Path(vis_sdk.__file__).parent / "vis_engine/blockether/vis")
    assert list(parent.__path__) == [staged, *search_path]
    assert sys.modules[sibling.__name__] is sibling
    assert "blockether.vis._outside" not in sys.modules
    assert sdk.ActivityProgress("Inspect SDK", value=1, total=2).to_wire() == {
        "type": "progress",
        "label": "Inspect SDK",
        "value": 1,
        "total": 2,
    }


def test_explicit_extension_host_is_not_reused_by_sandbox(source):
    host = SimpleNamespace(
        declare_env=lambda _: "{}", state_get=lambda key: "trusted " + key
    )
    extension = bootstrap(source, host)
    extension.register_extension(
        extension.Extension(name="trusted", description="Explicit host")
    )
    assert extension._registration["spec"]["name"] == "trusted"
    assert extension.state["key"] == "trusted key"

    sandbox = bootstrap(source)

    for name in dir(sandbox.Host):
        if not name.startswith("_"):
            with raises("unavailable in python_execution"):
                getattr(sandbox._host, name)
    assert sandbox._registration["spec"] is None
    assert "blockether.vis._outside" not in sys.modules
    assert extension.state["key"] == "trusted key"


def test_sandbox_installs_share_one_module_for_one_source(source):
    # Each sandbox session installs the SDK. Executing the body each time cost
    # about 15 ms for each new session.
    parent = importlib.import_module("blockether.vis")
    sandbox = bootstrap(source)
    hosted = bootstrap(source, SimpleNamespace())

    again = bootstrap(source)

    assert hosted is not sandbox
    assert again is sandbox
    assert sys.modules["blockether.vis.extension"] is sandbox
    assert parent.extension is sandbox
    assert bootstrap(source + "\n") is not sandbox


def test_failed_load_restores_previous_module(source, previous):
    parent = importlib.import_module("blockether.vis")
    installed = parent.extension
    if not previous:
        del sys.modules["blockether.vis.extension"]

    with raises("broken source"):
        bootstrap("raise RuntimeError('broken source')")

    assert parent.extension is installed
    assert sys.modules.get("blockether.vis.extension") is (
        installed if previous else None
    )
