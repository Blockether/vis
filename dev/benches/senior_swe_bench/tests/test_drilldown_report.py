from pathlib import Path
import importlib.util
import json
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("drilldown_report", ROOT / "drilldown_report.py")
drilldown_report = importlib.util.module_from_spec(spec)
sys.modules["drilldown_report"] = drilldown_report
spec.loader.exec_module(drilldown_report)  # type: ignore[union-attr]


def _run_dir(root: Path, name: str, task_id: str = "task-a") -> Path:
    run = root / name
    verifier = run / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (run / "command.json").write_text(
        json.dumps(
            {
                "run_id": name,
                "task_ids": [task_id],
                "SECRET_TOKEN": "do-not-report",
            }
        )
    )
    (run / "summary.json").write_text(
        json.dumps(
            {
                "task_id": task_id,
                "resolved": None,
                "patch_bloat": {"files": 1, "added_lines": 2, "deleted_lines": 0},
                "vis": {"iterations": 3, "elapsed_s": 12.5, "tokens": {"total": 42}, "cost_usd": 0.01},
            }
        )
    )
    return run


def test_drilldown_classifies_auth_failure_and_redacts_secret_keys(tmp_path):
    run = _run_dir(tmp_path, "subset-task-a")
    verifier = run / "harbor-output" / "trial" / "verifier"
    (verifier / "validation_agent_status.json").write_text(
        json.dumps({"exit_status": "AuthenticationError", "model": "openai/glm-5.2", "harness": "miniswe", "returncode": 1})
    )
    (verifier / "validation_results.json").write_text(
        json.dumps({"validation_score": 0.0, "validation_agent_infrastructure_failure": "miniswe exited with AuthenticationError"})
    )
    (verifier / "judge_output.json").write_text(json.dumps({"rubric_status": "failed:api_error"}))
    (verifier / "test-stdout.txt").write_text("litellm.AuthenticationError: Authentication Failed\n")
    subset = tmp_path / "subset.json"
    subset.write_text(json.dumps({"subset_name": "subset", "tasks": [{"run_dir": str(run), "exit_status": 0}]}))

    report = drilldown_report.build_report(subset)

    assert report["failure_counts"] == {"auth_failure": 1}
    task = report["tasks"][0]
    assert task["validation_agent_status"]["model"] == "openai/glm-5.2"
    assert task["command"]["SECRET_TOKEN"] == "[REDACTED]"
    assert task["vis"]["tokens"] == {"total": 42}
    assert "Authentication Failed" in task["diagnostics"]["test_stdout"]


def test_drilldown_reports_scoreable_pass_and_metadata(tmp_path):
    run = _run_dir(tmp_path, "subset-task-a")
    task_toml = run / "dataset" / "tasks" / "task-a" / "task.toml"
    task_toml.parent.mkdir(parents=True)
    task_toml.write_text(
        """
[metadata]
repo = "paperless"
segment = "debug"
[metadata.taxonomy]
task_type = "bugfix"
[metadata.origin]
repo = "paperless-ngx/paperless-ngx"
""".strip()
    )
    verifier = run / "harbor-output" / "trial" / "verifier"
    (verifier / "reward_details.json").write_text(json.dumps({"reward": 1.0, "correctness": 1.0, "resolved": True}))
    (verifier / "verifier_results.json").write_text(json.dumps({"passed": 2, "total": 2, "all_pass": True}))
    (verifier / "agent.patch").write_text("diff --git a/a.py b/a.py\nnew file mode 100644\n")
    subset = tmp_path / "subset.json"
    subset.write_text(json.dumps({"subset_name": "subset", "tasks": [{"run_dir": str(run), "exit_status": 0}]}))

    report = drilldown_report.build_report(subset)
    task = report["tasks"][0]

    assert report["failure_counts"] == {"scoreable_pass": 1}
    assert task["metadata"] == {"repo": "paperless-ngx/paperless-ngx", "segment": "debug", "task_type": "bugfix"}
    assert task["patch"]["files"] == ["a.py"]
    assert task["patch"]["added"] == 1


def test_drilldown_distinguishes_verifier_zero_total_from_scoreable_fail(tmp_path):
    run = _run_dir(tmp_path, "subset-task-a")
    verifier = run / "harbor-output" / "trial" / "verifier"
    (verifier / "verifier_results.json").write_text(json.dumps({"passed": 0, "total": 0, "all_pass": True}))
    subset = tmp_path / "subset.json"
    subset.write_text(json.dumps({"subset_name": "subset", "tasks": [{"run_dir": str(run), "exit_status": 0}]}))

    report = drilldown_report.build_report(subset)

    assert report["failure_counts"] == {"verifier_zero_total": 1}
