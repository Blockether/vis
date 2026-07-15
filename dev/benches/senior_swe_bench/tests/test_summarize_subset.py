from pathlib import Path
import importlib.util
import json
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("summarize_subset", ROOT / "summarize_subset.py")
summarize_subset = importlib.util.module_from_spec(spec)
sys.modules["summarize_subset"] = summarize_subset
spec.loader.exec_module(summarize_subset)  # type: ignore[union-attr]


def _run_dir(root: Path, name: str, *, task_id: str, failure_class=None, install_only_success=True):
    run = root / name
    (run / "harbor-output").mkdir(parents=True)
    (run / "command.json").write_text(
        json.dumps(
            {
                "run_id": name,
                "task_ids": [task_id],
                "task_base_image": f"{task_id}:latest",
                "selected_task_image": "",
                "vis_bench_verifier_provider": "zai",
                "vis_bench_verifier_judge_model": "openai/glm-5.2",
                "vis_bench_verifier_usage_capture": "litellm_jsonl+validation_trajectories",
            }
        )
    )
    (run / "dataset.lock.json").write_text(json.dumps({"dataset_commit": "abc123"}))
    (run / "summary.json").write_text(
        json.dumps(
            {
                "task_id": task_id,
                "failure_class": failure_class,
                "install_only_success": install_only_success,
                "patch_bloat": {"files": 0, "added_lines": 0, "deleted_lines": 0, "total_changed_lines": 0},
                "vis": {"iterations": 0},
                "agent_trace": {
                    "source": "vis_trace",
                    "available": True,
                    "telemetry_complete": True,
                    "iterations": 2,
                    "tool_calls": 3,
                    "shell_calls": 2,
                    "file_reads": 1,
                },
                "agent": {
                    "name": "vis-installed",
                    "harness": "Vis",
                    "provider": "zai-coding-plan",
                    "model": "glm-5.2",
                    "route": "zai-coding-plan",
                    "reasoning_effort": "high",
                    "reasoning_effort_explicit": True,
                    "pi_thinking_level": None,
                    "output_cap": 32768,
                    "vis_eval_valid": True,
                    "pi_models_sha256": None,
                    "usage": {
                        "available": True,
                        "complete": True,
                        "input_tokens": 100,
                        "cached_input_tokens": 80,
                        "uncached_input_tokens": 20,
                        "output_tokens": 10,
                        "total_tokens": 110,
                        "reported_cost_usd": 0.5,
                    },
                },
                "completion": {
                    "status": "complete_pass",
                    "complete": True,
                    "scoreable": True,
                    "passed": True,
                    "score": 1.0,
                },
                "verifier_usage": {
                    "available": True,
                    "complete": True,
                    "calls": 2,
                    "input_tokens": 50,
                    "output_tokens": 10,
                    "total_tokens": 60,
                    "reported_cost_usd": 0.0,
                },
            }
        )
    )
    (run / "harbor-output" / "collection.json").write_text(
        json.dumps({"job_dir": f"jobs/{name}", "trials": [{"trial_dir": "trial"}]})
    )
    (run / "secret-redaction.json").write_text(
        json.dumps({"clean": True, "remaining_occurrences": 0, "secret_values_loaded": 1})
    )
    return run


def test_summarize_subset_aggregates_successful_install_only_runs(tmp_path):
    run_a = _run_dir(tmp_path, "subset-1-task-a", task_id="task-a")
    run_b = _run_dir(tmp_path, "subset-1-task-b", task_id="task-b")

    summary = summarize_subset.summarize(
        subset_name="subset",
        subset_path="subsets/subset.json",
        run_dirs=[(run_a, 0), (run_b, 0)],
    )

    assert summary["total_tasks"] == 2
    assert summary["exit_status"] == 0
    assert summary["failure_counts"] == {}
    assert summary["all_install_only_success"] is True
    assert summary["tasks"][0]["harbor_job_dir"] == "jobs/subset-1-task-a"
    assert summary["tasks"][1]["collected_trials"] == 1
    assert summary["completion"]["authoritative"] is True
    assert summary["completion"]["pass_rate"] == 1.0
    assert summary["usage"]["complete"] is True
    assert summary["usage"]["tokens"]["total_tokens"] == 220
    assert summary["usage"]["reported_cost_usd"] == 1.0
    assert summary["agent_usage"]["tokens"]["total_tokens"] == 220
    assert summary["verifier_usage"]["tokens"]["total_tokens"] == 120
    assert summary["verifier_usage"]["calls"] == 4
    assert summary["spend_reporting_complete"] is True
    assert summary["tool_telemetry_complete"] is True
    assert summary["agent_telemetry"]["tool_calls"] == 6
    assert summary["secret_redaction_complete"] is True
    assert summary["comparison_ready"] is True
    assert summary["provenance"]["consistent"] is True
    assert summary["provenance"]["dataset_commits"] == ["abc123"]
    assert summary["provenance"]["agents"][0]["harness"] == "Vis"
    assert summary["provenance"]["agents"][0]["reasoning_effort"] == "high"
    assert summary["tasks"][0]["agent"]["vis_eval_valid"] is True


def test_summarize_subset_marks_missing_summary_and_nonzero_status(tmp_path):
    run = tmp_path / "subset-1-task-a"
    run.mkdir()
    (run / "command.json").write_text(json.dumps({"run_id": "subset-1-task-a", "task_ids": ["task-a"]}))

    summary = summarize_subset.summarize(
        subset_name="subset",
        run_dirs=[(run, 2)],
    )

    assert summary["exit_status"] == 2
    assert summary["failure_counts"] == {"missing_summary": 1}
    assert summary["tasks"][0]["failure_class"] == "missing_summary"


def test_summarize_subset_infers_zero_exit_for_historical_successes(tmp_path):
    run = _run_dir(tmp_path, "subset-1-task-a", task_id="task-a")

    summary = summarize_subset.summarize(
        subset_name="subset",
        run_dirs=[(run, None)],
    )

    assert summary["exit_status"] == 0


def test_summarize_subset_requires_exact_expected_task_ids(tmp_path):
    subset = tmp_path / "subset.json"
    subset.write_text(json.dumps({"task_ids": ["task-a", "task-b"]}))
    run_a = _run_dir(tmp_path, "run-a", task_id="task-a")
    duplicate_a = _run_dir(tmp_path, "run-a-again", task_id="task-a")

    summary = summarize_subset.summarize(
        subset_name="subset",
        subset_path=str(subset),
        run_dirs=[(run_a, 0), (duplicate_a, 0)],
    )

    assert summary["completion"]["all_tasks_present"] is False
    assert summary["completion"]["duplicate_task_ids"] == ["task-a"]
    assert summary["completion"]["missing_task_ids"] == ["task-b"]
    assert summary["completion"]["authoritative"] is False
    assert summary["comparison_ready"] is False
