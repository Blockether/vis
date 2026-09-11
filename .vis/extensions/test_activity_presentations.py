"""Bundled tools own their Activity presentation without making external calls."""

import importlib.util
import re
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


def _load_extension(monkeypatch, filename):
    spec = importlib.util.spec_from_file_location(
        f"activity_fixture_{filename}", Path(__file__).with_name(f"{filename}.py")
    )
    module = importlib.util.module_from_spec(spec)
    import sys

    monkeypatch.setitem(sys.modules, spec.name, module)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize(
    "filename, instance", [("gh", "gh"), ("uplink", "uplink"), ("sdk_checks", "sdk")]
)
def test_every_bundled_tool_declares_a_bounded_natural_language_activity(
    monkeypatch, filename, instance
):
    module = _load_extension(monkeypatch, filename)
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
        assert _contracts.validate("activity", "presentation", presentation.to_wire())
        if entry["name"] == "run":
            assert "Timed out" in presentation.summary
            assert any(
                "excerpt" in getattr(block, "text", "")
                for block in presentation.content
            )
        if entry["name"] == "check":
            assert "1 failed" in presentation.summary


@pytest.mark.parametrize(
    "ending, conclusion, verdict",
    [
        ("completed", "success", "Succeeded"),
        ("completed", "failure", "Failed"),
        ("completed", "cancelled", "Cancelled"),
        ("completed", "timed_out", "Timed out"),
        ("completed", "action_required", "Action required"),
        ("completed", "", "Conclusion unavailable"),
        ("poll_failure", "success", "Could not refresh workflow"),
        ("interrupted", "", "Stopped watching"),
        ("superseded", "", "Newer run took over"),
    ],
)
def test_watch_activity_reports_the_actual_outcome_without_prior_progress(
    monkeypatch, ending, conclusion, verdict
):
    module = _load_extension(monkeypatch, "gh")
    outcome = module.WatchOutcome(
        None,
        "Checks",
        "main",
        "completed",
        conclusion,
        "",
        ending,
        (),
        (),
        error="Connection unavailable" if ending == "poll_failure" else None,
    )
    # A successful tool return must not hide a failed or incomplete workflow.
    view = module.gh.watch.__vis_symbol_activity__.render(
        phase="success", result=outcome
    )
    assert verdict in view.summary
    assert "Checks" in view.summary
    assert "0 jobs" in view.summary
    assert any(
        "No jobs reported" in getattr(block, "text", "") for block in view.content
    )
    if outcome.error:
        assert any(
            outcome.error in getattr(block, "text", "") for block in view.content
        )
    if ending != "completed":
        assert "Succeeded" not in view.summary
    assert view.to_wire()["headline"] == "Watch workflow"


def test_watch_activity_labels_partial_job_lists(monkeypatch):
    module = _load_extension(monkeypatch, "gh")
    jobs = tuple(
        module.JobOutcome(i, f"Check {i}", "success", "", "", ()) for i in range(21)
    )
    outcome = module.WatchOutcome(
        1, "Checks", "main", "completed", "success", "", "completed", jobs, ()
    )
    view = module.gh.watch.__vis_symbol_activity__.render(
        phase="success", result=outcome
    )
    table = next(
        block for block in view.content if isinstance(block, vis.ActivityTable)
    )
    assert len(table.rows) == 20
    assert "21 jobs" in view.summary
    assert any("20 of 21 jobs" in getattr(block, "text", "") for block in view.content)


@pytest.mark.parametrize("filename", ["gh", "uplink"])
def test_bundled_activities_keep_unicode_within_portable_limits(monkeypatch, filename):
    module = _load_extension(monkeypatch, filename)
    long_text = "🦉" * 1000
    if filename == "gh":
        samples = [
            (
                module._runs_activity,
                (
                    module.RunSummary(
                        1,
                        long_text,
                        long_text,
                        "Checks",
                        "push",
                        "completed",
                        "success",
                        "",
                        "",
                    ),
                ),
            ),
            (
                module._watch_activity,
                module.WatchOutcome(
                    1,
                    long_text,
                    "main",
                    "completed",
                    "failure",
                    "",
                    "completed",
                    (module.JobOutcome(1, long_text, "failure", "", "", ()),),
                    (),
                    error=long_text,
                ),
            ),
        ]
    else:
        samples = [
            (
                module._info_activity,
                module.HostInfo("10.0.0.5", long_text, long_text, 0, 0, 0),
            ),
            (
                module._upload_activity,
                module.TransferResult(long_text, long_text, 0, 0),
            ),
            (
                module._download_activity,
                module.TransferResult(long_text, long_text, 0, 0),
            ),
        ]
    for render, result in samples:
        view = render(phase="success", result=result)
        assert _contracts.validate("activity", "presentation", view.to_wire())
        if filename == "gh" and render == module._watch_activity:
            assert any(
                "Watch error excerpt" == getattr(block, "text", "")
                for block in view.content
            )
