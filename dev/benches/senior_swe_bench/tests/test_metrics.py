from pathlib import Path
import importlib.util
import json
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("metrics", ROOT / "metrics.py")
metrics = importlib.util.module_from_spec(spec)
spec.loader.exec_module(metrics)  # type: ignore[union-attr]


def test_patch_bloat_counts_diff_lines():
    diff = """diff --git a/a b/a
--- a/a
+++ b/a
@@
-old
+new
+more
"""
    assert metrics.patch_bloat(diff) == {"files": 1, "added_lines": 2, "deleted_lines": 1, "total_changed_lines": 3}


def test_summarize_trace_counts_trace_chunk_iterations(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        "\n".join(
            [
                '{"event":"trace-chunk","payload":{"phase":"provider-call","iteration":1}}',
                '{"event":"trace-chunk","payload":{"phase":"reasoning","iteration":3}}',
                '{"event":"trace-chunk","payload":{"phase":"iteration-final","iteration":3}}',
            ]
        )
    )

    assert metrics.summarize_trace(trace)["iterations"] == 3


def test_summarize_trace_reads_wrapped_final_usage(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        json.dumps(
            {
                "event": "trace-chunk",
                "payload": {
                    "phase": "iteration-final",
                    "iteration": 81,
                    "final": {
                        "iteration-count": 81,
                        "duration-ms": 1889654.425532,
                        "tokens": {"input": 5384504, "output": 62380, "total": 5446884},
                        "cost": {"total-cost": 7.812777599999998},
                    },
                },
            }
        )
        + "\n"
    )

    summary = metrics.summarize_trace(trace)

    assert summary["iterations"] == 81
    assert summary["tokens"]["total"] == 5446884
    assert summary["cost_usd"] == 7.812777599999998
    assert round(summary["elapsed_s"], 3) == 1889.654


def _run_metrics(tmp_path, run_dir):
    out = tmp_path / "summary.json"
    old_argv = sys.argv
    try:
        sys.argv = [
            "metrics.py",
            "--task-id",
            "task-a",
            "--run-dir",
            str(run_dir),
            "--out",
            str(out),
        ]
        assert metrics.main() == 0
    finally:
        sys.argv = old_argv
    return json.loads(out.read_text())


def test_summary_reads_copied_verifier_results(tmp_path):
    run_dir = tmp_path / "run"
    verifier_dir = run_dir / "harbor-output" / "trial" / "verifier"
    verifier_dir.mkdir(parents=True)
    (verifier_dir / "verifier_results.json").write_text(
        json.dumps({"passed": 14, "total": 15, "all_pass": False, "tests": {}})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["verifier_pass"] is False
    assert summary["verifier"] == {"passed": 14, "total": 15, "all_pass": False, "runner_errors": None}
    assert summary["failure_class"] == "verifier_failed"


def test_summary_includes_judge_status(tmp_path):
    run_dir = tmp_path / "run"
    verifier_dir = run_dir / "harbor-output" / "trial" / "verifier"
    verifier_dir.mkdir(parents=True)
    (verifier_dir / "judge_output.json").write_text(
        json.dumps({"rubric_status": "failed:api_error", "rubric_error": "bad tool_choice"})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["judge"] == {"rubric_status": "failed:api_error", "rubric_error": "bad tool_choice"}


def test_summary_reads_harbor_trial_exception(tmp_path):
    run_dir = tmp_path / "run"
    trial = run_dir / "harbor-output" / "trial"
    trial.mkdir(parents=True)
    (trial / "result.json").write_text(
        json.dumps({"exception_info": {"exception_type": "RewardFileNotFoundError", "exception_message": "missing"}})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["harbor_exception"]["exception_type"] == "RewardFileNotFoundError"
    assert summary["failure_class"] == "RewardFileNotFoundError"


def test_summary_preserves_false_reward_fields(tmp_path):
    run_dir = tmp_path / "run"
    verifier = run_dir / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (verifier / "reward.json").write_text(
        json.dumps({"resolved": False, "verifier_pass": False, "validation_agent_pass": False, "score": 0})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["resolved"] is False
    assert summary["verifier_pass"] is False
    assert summary["validation_agent_pass"] is False
    assert summary["rubric_score"] == 0


def test_summary_prefers_reward_details_over_reward(tmp_path):
    run_dir = tmp_path / "run"
    verifier = run_dir / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (verifier / "reward.json").write_text(json.dumps({"reward": 0.0}))
    (verifier / "reward_details.json").write_text(
        json.dumps({"reward": 1.0, "rubric": {"status": "ok"}, "taste": {"status": "ok"}})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["harbor_reward"] == {"reward": 1.0, "rubric": {"status": "ok"}, "taste": {"status": "ok"}}


def test_summary_classifies_docker_daemon_failure_from_harbor_log(tmp_path):
    run_dir = tmp_path / "run"
    run_dir.mkdir()
    (run_dir / "harbor.log").write_text("Docker daemon is not running. Please start Docker and try again.\n")

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["failure_class"] == "docker_daemon_unavailable"


def test_summary_classifies_preflight_failure_artifact(tmp_path):
    run_dir = tmp_path / "run"
    run_dir.mkdir()
    (run_dir / "preflight-failure.json").write_text(
        json.dumps(
            {
                "failure_class": "remote_home_mount_invalid",
                "message": "ro home is not supported",
                "stage": "pre-harbor",
            }
        )
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["failure_class"] == "remote_home_mount_invalid"
    assert summary["preflight_failure"]["message"] == "ro home is not supported"


def test_summary_treats_successful_install_only_trial_as_no_failure(tmp_path):
    run_dir = tmp_path / "run"
    trial = run_dir / "harbor-output" / "trial"
    trial.mkdir(parents=True)
    (trial / "result.json").write_text(
        json.dumps({"config": {"install_only": True}, "exception_info": None})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["install_only_success"] is True
    assert summary["failure_class"] is None
