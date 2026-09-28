"""Resumable CPU batch selection and result validation."""

import gzip
import json
import shutil

import pytest
from run_suite import (
    accounted_tasks,
    archive_batch_traces,
    batch_results,
    catalog,
    job_name,
    next_batch,
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
        trial = jobs / "interrupted" / f"{name}__abcd"
        (trial / "agent").mkdir(parents=True)
        (trial / "result.json").write_text(
            json.dumps(
                {
                    "task_name": f"terminal-bench/{name}",
                    "finished_at": "2026-01-01T00:00:01Z",
                    "exception_info": {"exception_type": exception},
                }
            )
        )
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
    completed, in_flight, failed = accounted_tasks(jobs)
    assert completed == in_flight == set()
    assert failed == {"model-failed"}


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
