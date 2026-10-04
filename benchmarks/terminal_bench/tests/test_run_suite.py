"""Resumable CPU task pool selection and result validation."""

import gzip
import json
import shutil
import subprocess
import threading
from collections import Counter

import pytest
import run_suite
from run_suite import (
    accounted_tasks,
    archive_job_traces,
    catalog,
    job_name,
    job_results,
    next_task,
)


def write_model_trace(trial, *, streamed=True):
    """Write a trace with a pinned provider call and, optionally, streamed output."""
    frames = [
        {
            "event": "trace-chunk",
            "payload": {
                "phase": "provider-call",
                "provider": "zai-coding-plan",
                "model": "glm-5.3-flash",
            },
        }
    ]
    if streamed:
        frames.append(
            {"event": "trace-chunk", "payload": {"phase": "reasoning", "delta": "Let"}}
        )
    (trial / "agent").mkdir(parents=True, exist_ok=True)
    with gzip.open(
        trial / "agent/vis-trace.jsonl.gz", "wt", encoding="utf-8"
    ) as stream:
        stream.writelines(json.dumps(frame) + "\n" for frame in frames)


def write_model_attempt(trial, result, *, streamed=True):
    """Write a Harbor trial whose trace shows a pinned provider call."""
    write_model_trace(trial, streamed=streamed)
    (trial / "result.json").write_text(json.dumps(result))


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
            f'docker_image = "{name}-environment"\n'
            f'[verifier.environment]\ndocker_image = "{name}-verifier"\n'
        )
    tasks, gpu = catalog(tmp_path)
    assert [task["name"] for task in tasks] == ["small", "heavy", "medium"]
    assert gpu == ["gpu-only"]
    assert tasks[0]["images"] == ["small-environment", "small-verifier"]
    running = [next_task(tasks, [])]
    running.append(next_task(tasks, running))
    assert [task["name"] for task in running] == ["small", "medium"]
    assert next_task([{"name": "extra", "memory_mb": 0}], running) is None
    assert next_task(tasks, running[1:]) is None
    assert next_task(tasks, [])["name"] == "heavy"


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
    write_model_trace(jobs / "old" / "completed__abcd")
    scored, in_flight, failed = accounted_tasks(jobs)
    assert scored == {"completed": 1}
    assert in_flight == {"still-live"}
    assert failed == set()


def test_refused_model_calls_are_retryable_failures_not_scores(tmp_path):
    jobs = tmp_path / "jobs"
    trial = jobs / "suite-001" / "refused__abcd"
    result = {
        "task_name": "terminal-bench/refused",
        "finished_at": "2026-01-01T00:00:01Z",
        "exception_info": {"exception_type": "NonZeroAgentExitCodeError"},
        "agent_result": {
            "metadata": {"vis": {"model": run_suite.MODEL, "status": "error"}}
        },
        "verifier_result": {"rewards": {"reward": 0.0}},
    }
    write_model_attempt(trial, result, streamed=False)
    assert accounted_tasks(jobs) == ({}, set(), {"refused"})
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        job_results(jobs / "suite-001", {"name": "refused"})
    write_model_attempt(trial, result)
    assert accounted_tasks(jobs) == ({"refused": 1}, set(), set())


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
    scored, in_flight, failed = accounted_tasks(jobs)
    assert not scored and not in_flight
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
    scored, in_flight, failed = accounted_tasks(jobs)
    assert scored == {"verified-timeout": 1}
    assert in_flight == set()
    assert failed == {"unverified-timeout"}


@pytest.mark.skipif(shutil.which("zstd") is None, reason="zstd executable required")
def test_accounting_reads_model_work_from_archived_traces(tmp_path):
    jobs = tmp_path / "jobs"
    trial = jobs / "long" / "archived-timeout__abcd"
    write_model_attempt(
        trial,
        {
            "task_name": "terminal-bench/archived-timeout",
            "finished_at": "2026-01-01T08:00:01Z",
            "exception_info": {"exception_type": "AgentTimeoutError"},
            "verifier_result": {"rewards": {"reward": 0.0}},
        },
    )
    archive_job_traces(jobs / "long")
    assert not (trial / "agent/vis-trace.jsonl.gz").exists()
    assert accounted_tasks(jobs)[0] == {"archived-timeout": 1}


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
        lambda _: (
            Counter(["completed"]),
            {"live"},
            {"interrupted", "completed", "live"},
        ),
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
            Counter(["completed"]),
            {"live"},
            {"interrupted", "interrupted2", "completed", "live"},
        ),
    )
    run_suite.main()
    output = capsys.readouterr().out
    assert "failed after model call: 4" in output
    assert "pending: 3" in output
    assert "Next tasks: interrupted, interrupted2, pending" in output


def test_dry_run_json_reports_the_queue_plan_for_tools(monkeypatch, capsys):
    monkeypatch.setattr(run_suite.sys, "argv", ["run_suite", "--dry-run", "--json"])
    monkeypatch.setattr(
        run_suite,
        "catalog",
        lambda _: (
            [{"name": task} for task in ("done", "live", "broken", "next")],
            ["gpu-task"],
        ),
    )
    monkeypatch.setattr(
        run_suite,
        "accounted_tasks",
        lambda _: (Counter(["done"]), {"live"}, {"broken", "done", "elsewhere"}),
    )
    run_suite.main()
    plan = json.loads(capsys.readouterr().out)
    assert plan == {
        "model": run_suite.MODEL,
        "attempts": 1,
        "cpu_tasks": 4,
        "completed": ["done"],
        "scored": ["done"],
        "retryable": ["broken"],
        "live": ["live"],
        "pending": ["next"],
        "gpu_only": ["gpu-task"],
    }


def test_json_plan_requires_dry_run(monkeypatch):
    monkeypatch.setattr(run_suite.sys, "argv", ["run_suite", "--json"])
    with pytest.raises(SystemExit):
        run_suite.main()


def test_dry_run_json_counts_the_scored_attempts_of_each_task(monkeypatch, capsys):
    monkeypatch.setattr(
        run_suite.sys, "argv", ["run_suite", "--dry-run", "--json", "--attempts", "2"]
    )
    monkeypatch.setattr(
        run_suite,
        "catalog",
        lambda _: ([{"name": task} for task in ("done", "half", "new")], []),
    )
    monkeypatch.setattr(
        run_suite,
        "accounted_tasks",
        lambda _: (Counter({"done": 2, "half": 1}), set(), set()),
    )
    run_suite.main()
    plan = json.loads(capsys.readouterr().out)
    assert plan["attempts"] == 2
    assert plan["completed"] == ["done"]
    assert plan["scored"] == ["done", "half"]
    assert plan["pending"] == ["half", "new"]


def test_attempts_must_be_positive(monkeypatch):
    monkeypatch.setattr(
        run_suite.sys, "argv", ["run_suite", "--dry-run", "--attempts", "0"]
    )
    with pytest.raises(SystemExit):
        run_suite.main()


def test_job_names_and_missing_metrics_are_not_silently_accepted(tmp_path):
    jobs = tmp_path / "jobs"
    job = jobs / "suite-001"
    trial = job / "small__abcd"
    trial.mkdir(parents=True)
    write_model_trace(trial)
    assert job_name("suite", jobs) == "suite-002"
    assert job_name("suite", jobs, {"suite-002"}) == "suite-003"
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
    assert job_results(job, {"name": "small"}) == [result]
    result["agent_result"] = {}
    path.write_text(json.dumps(result))
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        job_results(job, {"name": "small"})
    result["agent_result"] = None
    path.write_text(json.dumps(result))
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        job_results(job, {"name": "small"})
    with pytest.raises(RuntimeError, match="0 of 1 results"):
        job_results(job, {"name": "another"})


def test_job_accepts_verified_timeout_only_after_model_work(tmp_path):
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
        job_results(job, {"name": "long"})
    write_model_attempt(trial, result)
    assert job_results(job, {"name": "long"}) == [result]
    result["verifier_result"] = None
    write_model_attempt(trial, result)
    with pytest.raises(RuntimeError, match="Incomplete metrics"):
        job_results(job, {"name": "long"})


def test_job_needs_every_requested_attempt_in_finish_order(tmp_path):
    job = tmp_path / "suite-001"
    first, second = scored_result("pair"), scored_result("pair")
    first["finished_at"] = "2026-01-01T00:00:02Z"
    second["finished_at"] = "2026-01-01T00:00:01Z"
    write_model_attempt(job / "pair__one", first)
    with pytest.raises(RuntimeError, match="1 of 2 results"):
        job_results(job, {"name": "pair", "attempts": 2})
    write_model_attempt(job / "pair__two", second)
    assert job_results(job, {"name": "pair", "attempts": 2}) == [second, first]


@pytest.mark.skipif(shutil.which("zstd") is None, reason="zstd executable required")
def test_job_archive_preserves_incomplete_traces(tmp_path):
    job = tmp_path / "suite-003"
    for name in ("complete", "interrupted"):
        agent = job / f"{name}__abcd" / "agent"
        agent.mkdir(parents=True)
        with gzip.open(agent / "vis-trace.jsonl.gz", "wb") as stream:
            stream.write(b'{"event":"trace-chunk"}\n')
        if name == "interrupted":
            trace = agent / "vis-trace.jsonl.gz"
            trace.write_bytes(trace.read_bytes()[:-8])
    archive_job_traces(job)
    assert (job / "complete__abcd/agent/vis-trace.jsonl.zst").is_file()
    assert not (job / "complete__abcd/agent/vis-trace.jsonl.gz").exists()
    assert (job / "interrupted__abcd/agent/vis-trace.jsonl.gz").is_file()
    assert not (job / "interrupted__abcd/agent/vis-trace.jsonl.zst").exists()


@pytest.fixture
def one_task_queue(tmp_path, monkeypatch):
    """Run one queued task without Harbor, Podman or existing jobs."""
    monkeypatch.setenv("ZAI_CODING_API_KEY", "fixture-zai-credential-12345")
    monkeypatch.setattr(run_suite.sys, "argv", ["run_suite", "--max-tasks", "1"])
    monkeypatch.setattr(run_suite, "ROOT", tmp_path)
    monkeypatch.setattr(run_suite, "JOBS", tmp_path / "jobs")
    task = {"name": "sample", "memory_mb": 4096}
    monkeypatch.setattr(run_suite, "catalog", lambda _: ([task], []))
    monkeypatch.setattr(
        run_suite, "accounted_tasks", lambda _: (Counter(), set(), set())
    )
    monkeypatch.setattr(run_suite, "free_gb", lambda _: (32, 32))
    monkeypatch.setattr(run_suite, "reclaim_images", lambda *_: None)
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
    assert command[-4:] == ["--job-name", "suite-001", "-i", "sample"]


def test_queue_continues_after_verified_agent_timeout(
    tmp_path, monkeypatch, capsys, one_task_queue
):
    monkeypatch.setattr(run_suite, "archive_job_traces", lambda _: None)

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


def scored_result(name, **vis):
    """Build a verified Harbor result carrying a Vis result from the pinned model."""
    return {
        "task_name": f"terminal-bench/{name}",
        "agent_result": {"metadata": {"vis": {"model": run_suite.MODEL, **vis}}},
        "verifier_result": {"rewards": {"reward": 0.0}},
    }


def write_job_attempt(jobs, command, **vis):
    """Write the trial a fake Harbor command would leave behind."""
    name = command[-1]
    job = command[command.index("--job-name") + 1]
    write_model_attempt(jobs / job / f"{name}__abcd", scored_result(name, **vis))


def write_attempts(jobs, job, name, count, **vis):
    """Write the trials of one Harbor job that ran several attempts of a task."""
    for number in range(count):
        write_model_attempt(
            jobs / job / f"{name}__{number:04}", scored_result(name, **vis)
        )


def test_queue_runs_missing_attempts_in_the_round_jobs_directory(
    tmp_path, monkeypatch, capsys, one_task_queue
):
    jobs = tmp_path / "rounds/first/jobs"
    monkeypatch.setattr(
        run_suite.sys, "argv", ["run_suite", "--attempts", "3", "--jobs", str(jobs)]
    )
    monkeypatch.setattr(
        run_suite, "accounted_tasks", lambda _: (Counter(["sample"]), set(), set())
    )
    monkeypatch.setattr(run_suite, "archive_job_traces", lambda _: None)
    calls = []

    def capture(command, path, *, cwd):
        calls.append((command, path))
        write_attempts(jobs, "suite-001", "sample", 2, status="success")
        return 0

    monkeypatch.setattr(run_suite, "capture", capture)
    run_suite.main()
    [(command, path)] = calls
    assert command[command.index("-k") + 1] == "2"
    assert command[command.index("-o") + 1] == str(jobs)
    assert path == tmp_path / "rounds/first/runs/suite-001.log"
    output = capsys.readouterr().out
    assert output.count("Finished suite-001/sample: reward=0.0") == 2


def test_queue_counts_fast_agent_errors_per_attempt(
    tmp_path, monkeypatch, one_task_queue
):
    monkeypatch.setattr(run_suite.sys, "argv", ["run_suite", "--attempts", "2"])
    monkeypatch.setattr(run_suite, "archive_job_traces", lambda _: None)

    def capture(command, path, *, cwd):
        write_attempts(
            tmp_path / "jobs", "suite-001", "sample", 2, status="error", duration_ms=1
        )
        return 0

    monkeypatch.setattr(run_suite, "capture", capture)
    with pytest.raises(RuntimeError, match="Two consecutive fast agent errors"):
        run_suite.main()


def test_queue_starts_next_task_when_one_slot_frees(
    tmp_path, monkeypatch, capsys, one_task_queue
):
    tasks = [{"name": name, "memory_mb": 4096} for name in ("long", "short", "next")]
    monkeypatch.setattr(run_suite, "catalog", lambda _: (tasks, []))
    monkeypatch.setattr(run_suite.sys, "argv", ["run_suite"])
    monkeypatch.setattr(run_suite, "archive_job_traces", lambda _: None)
    next_started = threading.Event()

    def capture(command, path, *, cwd):
        if command[-1] == "next":
            next_started.set()
        elif command[-1] == "long":
            # A two-task barrier would hold "next" until "long" finished.
            assert next_started.wait(5)
        write_job_attempt(tmp_path / "jobs", command, status="success")
        return 0

    monkeypatch.setattr(run_suite, "capture", capture)
    run_suite.main()
    output = capsys.readouterr().out
    assert output.index("Starting suite-003: next") < output.index(
        "Finished suite-001/long"
    )
    assert "tasks started: 3" in output


def test_queue_stops_after_two_fast_agent_errors_but_not_slow_ones(
    tmp_path, monkeypatch, capsys, one_task_queue
):
    durations = {"slow": 3_600_000, "fast": 1_000, "faster": 2_000, "never": 1}
    # High-memory tasks run alone, so their jobs finish in queue order.
    tasks = [{"name": name, "memory_mb": 16384} for name in durations]
    monkeypatch.setattr(run_suite, "catalog", lambda _: (tasks, []))
    monkeypatch.setattr(run_suite.sys, "argv", ["run_suite"])
    monkeypatch.setattr(run_suite, "archive_job_traces", lambda _: None)

    def capture(command, path, *, cwd):
        write_job_attempt(
            tmp_path / "jobs",
            command,
            status="error",
            duration_ms=durations[command[-1]],
        )
        return 0

    monkeypatch.setattr(run_suite, "capture", capture)
    with pytest.raises(RuntimeError, match="Two consecutive fast agent errors"):
        run_suite.main()
    output = capsys.readouterr().out
    assert "Finished suite-001/slow: reward=0.0, agent_error=True" in output
    assert "Finished suite-003/faster: reward=0.0, agent_error=True" in output
    assert "never" not in output


def test_reclaim_images_removes_task_images_and_reports_failures(monkeypatch, capsys):
    commands = []

    def run(command, **_):
        commands.append(command)
        failed = "fstrim" in command
        return subprocess.CompletedProcess(command, int(failed), "", "trim refused")

    monkeypatch.setattr(run_suite.subprocess, "run", run)
    task = {"name": "sample", "images": ["sample-environment", "sample-verifier"]}
    run_suite.reclaim_images("vis-amd64", task)
    assert commands[0] == [
        "podman",
        "--connection",
        "vis-amd64",
        "rmi",
        "--ignore",
        "sample-environment",
        "sample-verifier",
    ]
    assert commands[1][-3:] == ["sudo", "fstrim", "/var/lib/containers"]
    assert capsys.readouterr().out == (
        "Could not trim the VM disk after sample: trim refused\n"
    )


def test_queue_reclaims_images_after_each_validated_job(
    tmp_path, monkeypatch, one_task_queue
):
    reclaimed = []
    monkeypatch.setattr(run_suite, "archive_job_traces", lambda _: None)
    monkeypatch.setattr(
        run_suite,
        "reclaim_images",
        lambda machine, task: reclaimed.append((machine, task["name"])),
    )

    def capture(command, path, *, cwd):
        write_job_attempt(tmp_path / "jobs", command, status="success")
        return 0

    monkeypatch.setattr(run_suite, "capture", capture)
    run_suite.main()
    assert reclaimed == [("vis-amd64", "sample")]


def test_queue_says_why_it_stops_starting_tasks(monkeypatch, capsys, one_task_queue):
    monkeypatch.setattr(run_suite, "free_gb", lambda _: (5, 50))
    with pytest.raises(RuntimeError, match="Low disk space"):
        run_suite.main()
    assert (
        "Not starting more tasks: Low disk space (host 5.0 GB, VM 50.0 GB)"
        in capsys.readouterr().out
    )
