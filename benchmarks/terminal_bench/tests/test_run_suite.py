"""Resumable CPU batch selection and result validation."""

import gzip
import json
import shutil

import pytest
import run_suite
from run_suite import (
    accounted_tasks,
    archive_batch_traces,
    batch_results,
    catalog,
    job_name,
    next_batch,
)


def write_model_attempt(trial, result):
    """Write a Harbor trial whose trace shows a pinned provider call."""
    (trial / "agent").mkdir(parents=True, exist_ok=True)
    (trial / "result.json").write_text(json.dumps(result))
    with gzip.open(
        trial / "agent/vis-trace.jsonl.gz", "wt", encoding="utf-8"
    ) as stream:
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


def test_catalog_skips_gpu_and_reserves_high_memory(tmp_path):
    for name, hours, memory, gpu in (
        ("small", 1, 4096, 0),
        ("heavy", 2, 16384, 0),
        ("medium", 3, 8192, 0),
        ("gpu-only", 0.5, 8192, 1),
    ):
        folder = tmp_path / name
        folder.mkdir()
        (folder / "task.toml").write_text(
            f"[metadata]\nexpert_time_estimate_hours = {hours}\n"
            f"[environment]\nmemory_mb = {memory}\ngpus = {gpu}\n"
        )
    tasks, gpu = catalog(tmp_path)
    assert [task["name"] for task in tasks] == ["small", "heavy", "medium"]
    assert gpu == ["gpu-only"]
    assert [task["name"] for task in next_batch(tasks)] == ["small", "medium"]
    assert [task["name"] for task in next_batch(tasks)] == ["heavy"]


def test_accounting_retries_setup_only_failures_but_not_live_trials(tmp_path):
    jobs = tmp_path / "jobs"
    for name, agent in (
        (
            "completed",
            {"metadata": {"vis": {"model": "zai-coding-plan/glm-5.3-flash"}}},
        ),
        ("onboarding", {}),
        ("setup-failed", None),
    ):
        folder = jobs / "old" / f"{name}__abcd"
        folder.mkdir(parents=True)
        (folder / "result.json").write_text(
            json.dumps(
                {
                    "task_name": f"terminal-bench/{name}",
                    "finished_at": "2026-01-01T00:00:01Z",
                    "agent_result": agent,
                    "verifier_result": {"rewards": {"reward": 0}},
                }
            )
        )
    live = jobs / "running" / "still-live__abcd"
    live.mkdir(parents=True)
    (live / "config.json").write_text("{}")
    (jobs / "running" / "result.json").write_text('{"finished_at":null}')
    stale = jobs / "finished" / "retry-me__abcd"
    stale.mkdir(parents=True)
    (stale / "config.json").write_text("{}")
    (jobs / "finished" / "result.json").write_text(
        '{"finished_at":"2026-01-01T00:00:01Z"}'
    )
    completed, in_flight, failed = accounted_tasks(jobs)
    assert completed == {"completed"}
    assert in_flight == {"still-live"}
    assert failed == set()


def test_accounting_records_model_failure_but_retries_canceled_peer(tmp_path):
    jobs = tmp_path / "jobs"
    for name, exception in (
        ("model-failed", "NonZeroAgentExitCodeError"),
        ("canceled-peer", "EOFError"),
    ):
        write_model_attempt(
            jobs / "interrupted" / f"{name}__abcd",
            {
                "task_name": f"terminal-bench/{name}",
                "finished_at": "2026-01-01T00:00:01Z",
                "exception_info": {"exception_type": exception},
            },
        )
    completed, in_flight, failed = accounted_tasks(jobs)
    assert completed == in_flight == set()
    assert failed == {"model-failed"}


def test_accounting_scores_verified_timeouts_after_model_work(tmp_path):
    jobs = tmp_path / "jobs"
    for name, verifier in (
        ("verified-timeout", {"rewards": {"reward": 0.0}}),
        ("unverified-timeout", None),
    ):
        write_model_attempt(
            jobs / "long" / f"{name}__abcd",
            {
                "task_name": f"terminal-bench/{name}",
                "finished_at": "2026-01-01T08:00:01Z",
                "exception_info": {"exception_type": "AgentTimeoutError"},
                "verifier_result": verifier,
            },
        )
    setup = jobs / "long" / "setup-timeout__abcd"
    setup.mkdir(parents=True)
    (setup / "result.json").write_text(
        json.dumps(
            {
                "task_name": "terminal-bench/setup-timeout",
                "finished_at": "2026-01-01T08:00:01Z",
                "exception_info": {"exception_type": "AgentTimeoutError"},
                "verifier_result": {"rewards": {"reward": 0.0}},
            }
        )
    )
    completed, in_flight, failed = accounted_tasks(jobs)
    assert completed == {"verified-timeout"}
    assert in_flight == set()
    assert failed == {"unverified-timeout"}


@pytest.mark.parametrize("name", ["completed", "live", "pending", "unknown"])
def test_retry_task_refuses_unaccounted_completed_or_active_trials(
    tmp_path, monkeypatch, capsys, name
):
    monkeypatch.setattr(
        run_suite.sys, "argv", ["run_suite", "--dry-run", "--retry-task", name]
    )
    monkeypatch.setattr(run_suite, "JOBS", tmp_path / "jobs")
    monkeypatch.setattr(
        run_suite,
        "catalog",
        lambda _: (
            [
                {"name": task}
                for task in ("completed", "live", "interrupted", "pending")
            ],
            [],
        ),
    )
    monkeypatch.setattr(
        run_suite,
        "accounted_tasks",
        lambda _: ({"completed"}, {"live"}, {"interrupted", "completed", "live"}),
    )
    with pytest.raises(SystemExit, match="2"):
        run_suite.main()
    assert (
        "--retry-task requires an uncompleted, inactive failed model attempt"
        in capsys.readouterr().err
    )


def test_retry_task_selects_interrupted_model_attempt_without_losing_failed_artifacts(
    tmp_path, monkeypatch, capsys
):
    monkeypatch.setattr(
        run_suite.sys,
        "argv",
        [
            "run_suite",
            "--dry-run",
            "--retry-task",
            "interrupted",
            "--retry-task",
            "interrupted2",
        ],
    )
    monkeypatch.setattr(run_suite, "JOBS", tmp_path / "jobs")
    monkeypatch.setattr(
        run_suite,
        "catalog",
        lambda _: (
            [
                {"name": task}
                for task in (
                    "completed",
                    "live",
                    "interrupted",
                    "pending",
                    "interrupted2",
                )
            ],
            [],
        ),
    )
    monkeypatch.setattr(
        run_suite,
        "accounted_tasks",
        lambda _: (
            {"completed"},
            {"live"},
            {"interrupted", "interrupted2", "completed", "live"},
        ),
    )
    run_suite.main()
    output = capsys.readouterr().out
    assert "failed after model call: 4" in output
    assert "pending: 3" in output
    assert "Next tasks: interrupted, interrupted2, pending" in output


def test_job_names_and_missing_metrics_are_not_silently_accepted(tmp_path):
    jobs = tmp_path / "jobs"
    job = jobs / "suite-001"
    trial = job / "small__abcd"
    trial.mkdir(parents=True)
    assert job_name("suite", jobs) == "suite-002"
    result = {
        "task_name": "terminal-bench/small",
        "agent_result": {
            "n_input_tokens": 20,
            "metadata": {"vis": {"model": "zai-coding-plan/glm-5.3-flash"}},
        },
        "verifier_result": {"rewards": {"reward": 1.0}},
    }
    path = trial / "result.json"
    path.write_text(json.dumps(result))
    assert batch_results(job, [{"name": "small"}]) == [result]
    result["agent_result"] = {}
    path.write_text(json.dumps(result))
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        batch_results(job, [{"name": "small"}])
    result["agent_result"] = None
    path.write_text(json.dumps(result))
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        batch_results(job, [{"name": "small"}])
    with pytest.raises(RuntimeError, match="missing results"):
        batch_results(job, [{"name": "another"}])


def test_batch_accepts_verified_timeout_only_after_model_work(tmp_path):
    job = tmp_path / "suite-001"
    trial = job / "long__abcd"
    trial.mkdir(parents=True)
    result = {
        "task_name": "terminal-bench/long",
        "agent_result": {"metadata": None},
        "verifier_result": {"rewards": {"reward": 0.0}},
        "exception_info": {"exception_type": "AgentTimeoutError"},
    }
    (trial / "result.json").write_text(json.dumps(result))
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        batch_results(job, [{"name": "long"}])
    write_model_attempt(trial, result)
    assert batch_results(job, [{"name": "long"}]) == [result]
    result["verifier_result"] = None
    write_model_attempt(trial, result)
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        batch_results(job, [{"name": "long"}])


@pytest.mark.skipif(shutil.which("zstd") is None, reason="zstd executable required")
def test_batch_archive_preserves_incomplete_traces(tmp_path):
    job = tmp_path / "suite-003"
    for name in ("complete", "interrupted"):
        agent = job / f"{name}__abcd" / "agent"
        agent.mkdir(parents=True)
        with gzip.open(agent / "vis-trace.jsonl.gz", "wb") as stream:
            stream.write(b'{"event":"trace-chunk"}\n')
        if name == "interrupted":
            trace = agent / "vis-trace.jsonl.gz"
            trace.write_bytes(trace.read_bytes()[:-8])
    archive_batch_traces(job)
    assert (job / "complete__abcd/agent/vis-trace.jsonl.zst").is_file()
    assert not (job / "complete__abcd/agent/vis-trace.jsonl.gz").exists()
    assert (job / "interrupted__abcd/agent/vis-trace.jsonl.gz").is_file()
    assert not (job / "interrupted__abcd/agent/vis-trace.jsonl.zst").exists()


@pytest.fixture
def one_task_queue(tmp_path, monkeypatch):
    """Run one queue batch without Harbor, Podman or existing jobs."""
    monkeypatch.setenv("ZAI_CODING_API_KEY", "fixture-zai-credential-12345")
    monkeypatch.setattr(run_suite.sys, "argv", ["run_suite", "--max-batches", "1"])
    monkeypatch.setattr(run_suite, "ROOT", tmp_path)
    monkeypatch.setattr(run_suite, "JOBS", tmp_path / "jobs")
    task = {"name": "sample"}
    monkeypatch.setattr(run_suite, "catalog", lambda _: ([task], []))
    monkeypatch.setattr(run_suite, "accounted_tasks", lambda _: (set(), set(), set()))
    monkeypatch.setattr(run_suite, "free_gb", lambda _: (32, 32))
    monkeypatch.setattr(run_suite, "next_batch", lambda _: [task])
    return task


def test_queue_captures_harbor_logs_through_redaction(
    tmp_path, monkeypatch, one_task_queue
):
    calls = []

    def capture(command, path, *, cwd):
        calls.append((command, path, cwd))
        return 7

    monkeypatch.setattr(run_suite, "capture", capture)
    with pytest.raises(RuntimeError, match="Harbor exited 7"):
        run_suite.main()
    assert len(calls) == 1
    command, path, cwd = calls[0]
    assert command[1] == "run"
    assert path == tmp_path / "runs/suite-001.log"
    assert cwd == tmp_path


def test_queue_continues_after_verified_agent_timeout(
    tmp_path, monkeypatch, capsys, one_task_queue
):
    monkeypatch.setattr(run_suite, "archive_batch_traces", lambda _: None)

    def capture(command, path, *, cwd):
        write_model_attempt(
            tmp_path / "jobs/suite-001/sample__abcd",
            {
                "task_name": "terminal-bench/sample",
                "agent_result": {"metadata": None},
                "verifier_result": {"rewards": {"reward": 0.0}},
                "exception_info": {"exception_type": "AgentTimeoutError"},
            },
        )
        return 0

    monkeypatch.setattr(run_suite, "capture", capture)
    run_suite.main()
    output = capsys.readouterr().out
    assert "Finished suite-001/sample: reward=0.0, agent_error=False" in output
    assert "agent_timeout=True" in output
