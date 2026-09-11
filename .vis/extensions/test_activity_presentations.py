"""Bundled tools own their Activity presentation without making external calls."""

import importlib.util
import re
from pathlib import Path

import blockether.vis.extension as vis
import pytest


@pytest.mark.parametrize(
    "filename, instance", [("gh", "gh"), ("uplink", "uplink"), ("sdk_checks", "sdk")]
)
def test_every_bundled_tool_declares_a_bounded_natural_language_activity(
    monkeypatch, filename, instance
):
    spec = importlib.util.spec_from_file_location(
        f"activity_fixture_{filename}", Path(__file__).with_name(f"{filename}.py")
    )
    module = importlib.util.module_from_spec(spec)
    import sys

    monkeypatch.setitem(sys.modules, spec.name, module)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    spec.loader.exec_module(module)
    tools = getattr(module, instance)
    exported = vis._registration["spec"]["symbols"][0]["methods"]
    assert exported
    if filename == "gh":
        samples = {
            "login": module.Account("github.com", True, True),
            "runs": (),
            "watch": module.WatchOutcome(
                None, "Checks", "main", "completed", "failure", "", "completed", (), ()
            ),
        }
    elif filename == "uplink":
        samples = {
            "run": module.CommandResult(
                "printf hello", None, "x" * 8000, "", 1, True, True
            ),
            "service": module.ServiceStatus(
                "visgw", "loaded", "inactive", "dead", False, None
            ),
            "health": module.HealthCheck(
                "http://127.0.0.1/healthz", None, False, "", "Connection refused"
            ),
            "info": module.HostInfo("10.0.0.5", "visgw", None, None, None, None),
            "put": module.TransferResult("local.txt", "remote.txt", 42, 1),
            "get": module.TransferResult("local.txt", "remote.txt", 42, 1),
        }
    else:
        samples = {
            "check": module.CheckReport(
                (module.CheckResult("Tests", 1, 1, "Failed"),), False
            )
        }
    assert {entry["name"] for entry in exported} == set(samples)
    for entry in exported:
        declaration = getattr(tools, entry["name"]).__vis_symbol_activity__
        assert re.fullmatch(r"[A-Z][A-Za-z ]+", declaration.label)
        # These bundled tools perform network IO or verification, so show live progress.
        assert declaration.show_start is True
        assert entry["activity"]["show_start"] is True
        assert callable(declaration.render)
        for phase in ("start", "failure"):
            assert (
                declaration.render(
                    phase=phase, result=None, args=(), kwargs={}, error=None
                )
                is None
            )
        presentation = declaration.render(
            phase="success",
            result=samples[entry["name"]],
            args=(),
            kwargs={},
            error=None,
        )
        assert isinstance(presentation, vis.ActivityPresentation)
        assert presentation.headline == declaration.label
        assert presentation.to_wire()["headline"]
        if entry["name"] == "run":
            assert "Timed out" in presentation.summary
            assert any(
                "excerpt" in getattr(block, "text", "")
                for block in presentation.content
            )
        if entry["name"] == "check":
            assert "1 failed" in presentation.summary
