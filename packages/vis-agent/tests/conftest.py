"""Make the package importable from the checkout, and give every test a clean host."""

import os
import sys
from pathlib import Path

if not os.environ.get("VIS_TEST_INSTALLED"):
    sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))
    sys.path.insert(
        0,
        str(Path(__file__).resolve().parents[2] / "vis-contract" / "python" / "src"),
    )

# Remove only our SDK modules, never an unrelated package named `vis`.
sys.modules.pop("blockether.vis", None)
sys.modules.pop("blockether.vis._outside", None)

import pytest  # noqa: E402  (the path above is what makes `vis` importable)
from blockether import vis  # noqa: E402


@pytest.fixture(autouse=True)
def outside_home(tmp_path, monkeypatch):
    """State, shell logs and primed answers never leak between tests."""
    monkeypatch.setenv("VIS_OUTSIDE_HOME", str(tmp_path))
    monkeypatch.delenv("VIS_OUTSIDE_ANSWERS", raising=False)
    monkeypatch.delenv("VIS_OUTSIDE_NONINTERACTIVE", raising=False)
    vis.outside.answer_with({})
    yield tmp_path
    vis.outside.answer_with({})
