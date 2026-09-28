"""Result accounting for scored, failed, and archived Harbor trials."""

import gzip
import json
import shutil
import subprocess

import pytest
from summarize import elapsed_seconds, make_report, trace_summary


def test_elapsed_seconds_requires_both_timestamps():
    assert elapsed_seconds("2026-01-01T00:00:00Z", "2026-01-01T00:00:02.500Z") == 2.5
    assert elapsed_seconds(None, "2026-01-01T00:00:02Z") is None


def test_report_keeps_exceptions_out_of_reward_mean(tmp_path):
    dataset = tmp_path / "dataset"
    for task, gpu in (("cpu-task", 0), ("gpu-task", 1), ("unrun-task", 0)):
        directory = dataset / task
        directory.mkdir(parents=True)
        (directory / "task.toml").write_text(f"[environment]\ngpus = {gpu}\n")

    jobs = tmp_path / "jobs"
    verified = jobs / "canary" / "cpu-task__ok"
    verified.mkdir(parents=True)
    (verified / "result.json").write_text(
        json.dumps(
            {
                "task_name": "terminal-bench/cpu-task",
                "trial_name": "cpu-task__ok",
                "started_at": "2026-01-01T00:00:00Z",
                "finished_at": "2026-01-01T00:00:12Z",
                "agent_execution": {
                    "started_at": "2026-01-01T00:00:01Z",
                    "finished_at": "2026-01-01T00:00:10Z",
                },
                "verifier": {
                    "started_at": "2026-01-01T00:00:10Z",
                    "finished_at": "2026-01-01T00:00:12Z",
                },
                "agent_result": {
                    "n_input_tokens": 100,
                    "n_cache_tokens": 20,
                    "n_output_tokens": 30,
                    "cost_usd": None,
                    "metadata": {
                        "vis": {
                            "model": "zai-coding-plan/glm-5.3-flash",
                            "estimated_metered_api_cost_usd": 0.04,
                            "duration_ms": 9000,
                            "iteration_count": 2,
                        }
                    },
                },
                "verifier_result": {"rewards": {"reward": 1.0}},
                "exception_info": None,
            }
        )
    )
    (verified / "agent").mkdir()
    (verified / "verifier").mkdir()
    (verified / "verifier/ctrf.json").write_text(
        json.dumps({"results": {"summary": {"tests": 3, "passed": 2, "failed": 1}}})
    )
    (verified / "agent/vis-trace.jsonl").write_text(
        json.dumps(
            {
                "event": "trace-chunk",
                "payload": {
                    "phase": "provider-call",
                    "provider": "zai-coding-plan",
                    "model": "glm-5.3-flash",
                    "iteration": 1,
                },
            }
        )
        + "\n"
        + json.dumps(
            {
                "event": "trace-chunk",
                "payload": {
                    "phase": "form-start",
                    "tool-name": "python_execution",
                    "iteration": 2,
                },
            }
        )
        + "\n"
    )

    failed = jobs / "earlier" / "gpu-task__failed"
    failed.mkdir(parents=True)
    (failed / "result.json").write_text(
        json.dumps(
            {
                "task_name": "terminal-bench/gpu-task",
                "trial_name": "gpu-task__failed",
                "finished_at": "2026-01-01T00:00:05Z",
                "exception_info": {
                    "exception_type": "SetupError",
                    "exception_message": "private",
                },
                "agent_result": None,
                "verifier_result": {"rewards": {"reward": 0.0}},
            }
        )
    )

    report = make_report(jobs, dataset)
    assert (
        report["attempts"],
        report["verified_attempts"],
        report["exception_attempts"],
    ) == (
        2,
        1,
        1,
    )
    assert report["mean_verified_reward"] == 1.0
    assert report["gpu_required_tasks"] == ["gpu-task"]
    assert report["unattempted_tasks"] == ["unrun-task"]
    good = next(trial for trial in report["trials"] if trial["status"] == "verified")
    assert (
        good["elapsed_seconds"],
        good["agent_seconds"],
        good["verifier_seconds"],
    ) == (
        12,
        9,
        2,
    )
    assert good["tokens"] == {"input": 100, "cached": 20, "output": 30}
    assert good["estimated_metered_api_cost_usd"] == 0.04
    assert good["billed_cost_usd"] is None
    assert good["trace"]["provider_calls"] == {"zai-coding-plan/glm-5.3-flash": 1}
    assert good["trace"]["tool_calls"] == {"python_execution": 1}
    assert good["verifier_tests"] == {"tests": 3, "passed": 2, "failed": 1}
    assert "private" not in json.dumps(report)


def test_report_scores_each_task_by_latest_verified_model_attempt(tmp_path):
    dataset = tmp_path / "dataset"
    dataset.mkdir()
    jobs = tmp_path / "jobs"
    provider_call = json.dumps(
        {
            "event": "trace-chunk",
            "payload": {
                "phase": "provider-call",
                "provider": "zai-coding-plan",
                "model": "glm-5.3-flash",
            },
        }
    )
    vis = {"metadata": {"vis": {"model": "zai-coding-plan/glm-5.3-flash"}}}
    timeout = {"metadata": None}
    for job, task, finished, exception, agent, reward in (
        ("first", "retried", "2026-01-01T01:00:00Z", None, vis, 0.0),
        ("second", "retried", "2026-01-01T02:00:00Z", None, vis, 1.0),
        (
            "first",
            "timed-out",
            "2026-01-01T08:00:00Z",
            "AgentTimeoutError",
            timeout,
            0.0,
        ),
        (
            "first",
            "killed",
            "2026-01-01T03:00:00Z",
            "NonZeroAgentExitCodeError",
            timeout,
            None,
        ),
        ("first", "setup", "2026-01-01T00:10:00Z", "SetupError", None, None),
    ):
        trial = jobs / job / f"{task}__abcd"
        (trial / "agent").mkdir(parents=True)
        (trial / "result.json").write_text(
            json.dumps(
                {
                    "task_name": f"terminal-bench/{task}",
                    "finished_at": finished,
                    "exception_info": {"exception_type": exception}
                    if exception
                    else None,
                    "agent_result": agent,
                    "verifier_result": None
                    if reward is None
                    else {"rewards": {"reward": reward}},
                }
            )
        )
        if task != "setup":
            (trial / "agent/vis-trace.jsonl").write_text(provider_call + "\n")
    report = make_report(jobs, dataset)
    assert report["solved_tasks"] == ["retried"]
    assert report["scored_tasks"] == 2
    assert report["task_pass_rate"] == 0.5
    assert report["unscored_model_tasks"] == ["killed"]


def test_report_groups_scored_tasks_by_how_their_latest_attempt_ended(tmp_path):
    dataset = tmp_path / "dataset"
    dataset.mkdir()
    jobs = tmp_path / "jobs"
    provider_call = {
        "event": "trace-chunk",
        "payload": {
            "phase": "provider-call",
            "provider": "zai-coding-plan",
            "model": "glm-5.3-flash",
        },
    }
    capped = {
        "event": "result",
        "payload": {
            "status": "error",
            "trace": [{"iteration": 0, "error": {"type": "max-tokens-exceeded"}}],
        },
    }
    vis = {"metadata": {"vis": {"model": "zai-coding-plan/glm-5.3-flash"}}}
    for job, task, finished, exception, agent, frames, reward in (
        ("first", "solved", "2026-01-01T01:00:00Z", None, vis, [], 1.0),
        ("first", "tests", "2026-01-01T01:00:00Z", None, vis, [], 0.0),
        (
            "first",
            "timed-out",
            "2026-01-01T08:00:00Z",
            "AgentTimeoutError",
            {"metadata": None},
            [],
            0.0,
        ),
        (
            "first",
            "capped",
            "2026-01-01T01:00:00Z",
            "NonZeroAgentExitCodeError",
            vis,
            [capped],
            0.0,
        ),
        (
            "first",
            "retried",
            "2026-01-01T01:00:00Z",
            "NonZeroAgentExitCodeError",
            vis,
            [capped],
            0.0,
        ),
        ("second", "retried", "2026-01-01T02:00:00Z", None, vis, [], 1.0),
    ):
        trial = jobs / job / f"{task}__abcd"
        (trial / "agent").mkdir(parents=True)
        (trial / "result.json").write_text(
            json.dumps(
                {
                    "task_name": f"terminal-bench/{task}",
                    "finished_at": finished,
                    "exception_info": {"exception_type": exception}
                    if exception
                    else None,
                    "agent_result": agent,
                    "verifier_result": {"rewards": {"reward": reward}},
                }
            )
        )
        (trial / "agent/vis-trace.jsonl").write_text(
            "".join(json.dumps(frame) + "\n" for frame in [provider_call, *frames])
        )
    report = make_report(jobs, dataset)
    assert report["task_outcomes"] == {
        "agent_timeout": ["timed-out"],
        "failed_tests": ["tests"],
        "solved": ["retried", "solved"],
        "vis_error:max-tokens-exceeded": ["capped"],
    }


def test_compressed_trace_and_invalid_line(tmp_path):
    path = tmp_path / "vis-trace.jsonl.gz"
    with gzip.open(path, "wt") as stream:
        stream.write('not json\n{"event":"result","payload":{"status":"completed"}}\n')
    summary = trace_summary(tmp_path / "vis-trace.jsonl", tmp_path)
    assert summary["path"] == "vis-trace.jsonl.gz"
    assert summary["invalid_jsonl_lines"] == 1
    assert summary["event_counts"] == {"result": 1}
    assert summary["vis_result_status"] == "completed"


def test_interrupted_gzip_retains_partial_trace_counts(tmp_path):
    path = tmp_path / "vis-trace.jsonl.gz"
    with gzip.open(path, "wt", encoding="utf-8") as stream:
        stream.write(
            json.dumps(
                {
                    "event": "trace-chunk",
                    "payload": {
                        "phase": "provider-call",
                        "provider": "zai-coding-plan",
                        "model": "glm-5.3-flash",
                    },
                }
            )
            + "\n"
        )
    path.write_bytes(path.read_bytes()[:-8])
    summary = trace_summary(tmp_path / "vis-trace.jsonl", tmp_path)
    assert summary["provider_calls"] == {"zai-coding-plan/glm-5.3-flash": 1}
    assert summary["truncated"] is True


def test_result_error_records_only_safe_provider_usage(tmp_path):
    path = tmp_path / "vis-trace.jsonl.gz"
    frame = {
        "event": "result",
        "payload": {
            "status": "error",
            "trace": [
                {
                    "error": {
                        "type": "max-tokens-exceeded",
                        "data": {
                            "api-usage": {
                                "input-tokens": 4313,
                                "output-tokens": 16384,
                                "total-tokens": 20697,
                            },
                            "provider-state": {"thinking": "private"},
                        },
                    }
                }
            ],
        },
    }
    with gzip.open(path, "wt") as stream:
        stream.write(json.dumps(frame) + "\n")
    summary = trace_summary(tmp_path / "vis-trace.jsonl", tmp_path)
    assert summary["vis_result_status"] == "error"
    assert summary["vis_error_type"] == "max-tokens-exceeded"
    assert summary["last_provider_error_usage"] == {
        "input": 4313,
        "output": 16384,
        "total": 20697,
    }
    assert "private" not in json.dumps(summary)


def test_long_window_archived_trace_reads_without_losing_events(tmp_path):
    zstd = shutil.which("zstd")
    if not zstd:
        pytest.skip("zstd executable is required for the trace archive")
    path = tmp_path / "vis-trace.jsonl.zst"
    contents = (
        json.dumps(
            {
                "event": "trace-chunk",
                "payload": {
                    "phase": "provider-call",
                    "provider": "zai-coding-plan",
                    "model": "glm-5.3-flash",
                },
            }
        )
        + "\n"
    ).encode()
    compressed = subprocess.run(
        [zstd, "-q", "-c"], input=contents, capture_output=True, check=True
    ).stdout
    path.write_bytes(compressed)
    summary = trace_summary(tmp_path / "vis-trace.jsonl", tmp_path)
    assert summary["path"] == "vis-trace.jsonl.zst"
    assert summary["provider_calls"] == {"zai-coding-plan/glm-5.3-flash": 1}
    assert summary["truncated"] is False


def test_report_redacts_credentials_in_free_form_fields(tmp_path, monkeypatch):
    key = "fixture-zai-credential-12345"
    monkeypatch.setenv("ZAI_CODING_API_KEY", key)
    dataset = tmp_path / "dataset"
    dataset.mkdir()
    trial = tmp_path / "jobs/batch/task"
    trial.mkdir(parents=True)
    (trial / "result.json").write_text(
        json.dumps(
            {
                "task_name": key,
                "trial_name": key,
                "finished_at": "2026-01-01T00:00:00Z",
                "exception_info": {"exception_type": key},
                "agent_result": {"metadata": {"vis": {"model": key}}},
            }
        )
    )
    (trial / "agent").mkdir()
    (trial / "agent/vis-trace.jsonl").write_text(
        json.dumps(
            {
                "event": key,
                "payload": {"phase": "provider-call", "provider": key, "model": key},
            }
        )
        + "\n"
    )
    report = make_report(tmp_path / "jobs", dataset)
    assert key not in json.dumps(report)
    assert "[REDACTED]" in json.dumps(report)
