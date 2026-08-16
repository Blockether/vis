"""Make the package importable from the checkout, and give every test a clean host."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

# Whatever else claimed the name first — a Vis engine seeds its own `vis` into
# `sys.modules`, and a bare `vis` directory beside the checkout makes an empty
# namespace package — the module under test is the one in THIS checkout.
sys.modules.pop("vis", None)
sys.modules.pop("vis._outside", None)

import pytest  # noqa: E402  (the path above is what makes `vis` importable)
import vis  # noqa: E402


@pytest.fixture(autouse=True)
def outside_home(tmp_path, monkeypatch):
    """State, shell logs and primed answers never leak between tests."""
    monkeypatch.setenv("VIS_OUTSIDE_HOME", str(tmp_path))
    monkeypatch.delenv("VIS_OUTSIDE_ANSWERS", raising=False)
    monkeypatch.delenv("VIS_OUTSIDE_NONINTERACTIVE", raising=False)
    vis.outside.answer_with({})
    yield tmp_path
    vis.outside.answer_with({})
