"""Tests for the Harbor job reader of `blockether.vis.evals`. All data is synthetic."""

import hashlib
import json

import pytest
from blockether.vis.evals import (
    case_results,
    read_trial,
    read_trials,
    seal_digest,
    seal_round,
    summarize,
)


def _write(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data))
    return path


def _job(directory):
    _write(directory / "config.json", {"job_name": directory.name})
    _write(directory / "result.json", {"id": "job", "n_total_trials": 2, "stats": {}})
    return directory


def _trial(
    job, name, task, *, agent="vis-solo", model="openai/gpt", reward=1.0, **more
):
    data = {
        "id": name,
        "task_name": task,
        "trial_name": name,
        "task_checksum": f"sum-{task}",
        "config": {"agent": {"name": "ctb.agents.vis:VisSolo", "model_name": model}},
        "agent_info": {"name": agent, "version": "vis-agent 1.0"},
        "agent_result": {
            "n_input_tokens": 100,
            "n_cache_tokens": 40,
            "n_output_tokens": 10,
            "cost_usd": 0.25,
        },
        "verifier_result": None if reward is None else {"rewards": {"reward": reward}},
        "exception_info": None,
        "started_at": more.pop("started_at", f"2026-01-01T00:00:0{len(name) % 10}Z"),
        "finished_at": "2026-01-01T00:10:00Z",
    }
    data.update(more)
    return _write(job / name / "result.json", data)


def test_read_trial_reads_the_fields_of_one_result(tmp_path):
    path = _trial(
        _job(tmp_path / "job"),
        "t1",
        "terminal-bench/fix-git",
        reward=0.0,
        exception_info={
            "exception_type": "AgentTimeoutError",
            "exception_message": "x",
        },
    )
    trial = read_trial(path)
    assert (trial.job, trial.trial, trial.task) == (
        "job",
        "t1",
        "terminal-bench/fix-git",
    )
    assert (trial.agent, trial.agent_version, trial.model) == (
        "vis-solo",
        "vis-agent 1.0",
        "openai/gpt",
    )
    assert (trial.reward, trial.error, trial.is_scored, trial.is_passed) == (
        0.0,
        "AgentTimeoutError",
        True,
        False,
    )
    assert (trial.input_tokens, trial.cache_tokens, trial.output_tokens) == (
        100,
        40,
        10,
    )
    assert (trial.cost_usd, trial.task_checksum, trial.path) == (
        0.25,
        "sum-terminal-bench/fix-git",
        path,
    )
    with pytest.raises(TypeError):
        trial.rewards["reward"] = 1.0


def test_read_trial_gives_none_for_other_files(tmp_path):
    job = _job(tmp_path / "job")
    assert read_trial(job / "result.json") is None
    assert read_trial(_trial(job, "t1", "a", finished_at=None)) is None
    (tmp_path / "broken.json").write_text("{")
    assert read_trial(tmp_path / "broken.json") is None
    assert read_trial(tmp_path / "missing.json") is None


def test_a_trial_without_a_verifier_reward_is_not_scored(tmp_path):
    trial = read_trial(_trial(_job(tmp_path / "job"), "t1", "a", reward=None))
    assert (trial.reward, dict(trial.rewards), trial.is_scored, trial.is_passed) == (
        None,
        {},
        False,
        False,
    )


def test_read_trials_reads_a_job_or_a_round_in_start_order(tmp_path):
    first = _job(tmp_path / "round" / "vis-solo__openai__a")
    second = _job(tmp_path / "round" / "vis-team__openai__a")
    _trial(first, "late", "a", started_at="2026-01-01T00:00:09Z")
    _trial(second, "early", "a", agent="vis-team", started_at="2026-01-01T00:00:01Z")
    _write(first / "late" / "verifier" / "result.json", {"note": "not a trial"})
    assert [trial.trial for trial in read_trials(tmp_path / "round")] == [
        "early",
        "late",
    ]
    assert [trial.trial for trial in read_trials(first)] == ["late"]
    with pytest.raises(FileNotFoundError):
        read_trials(tmp_path / "missing")


def test_case_results_count_scored_trials_by_task_agent_and_model(tmp_path):
    job = _job(tmp_path / "job")
    _trial(job, "t1", "a", reward=1.0)
    _trial(job, "t2", "a", reward=0.0)
    _trial(job, "t3", "a", reward=None)
    _trial(job, "t4", "b", model="anthropic/claude", reward=1.0)
    _trial(job, "t5", "c", reward=None)
    assert case_results(read_trials(job)) == {
        ("a", "vis-solo", "openai/gpt"): (1, 2),
        ("b", "vis-solo", "anthropic/claude"): (1, 1),
    }


def test_summarize_gives_counts_rates_and_cost(tmp_path):
    job = _job(tmp_path / "job")
    _trial(job, "t1", "a", reward=1.0, started_at="2026-01-01T00:00:01Z")
    _trial(job, "t2", "b", reward=0.0, started_at="2026-01-01T00:00:02Z")
    _trial(
        job,
        "t3",
        "c",
        reward=None,
        agent_result={"n_input_tokens": 5},
        exception_info={"exception_type": "RuntimeError"},
        started_at="2026-01-01T00:00:03Z",
    )
    summary = summarize(read_trials(job))
    assert (summary.trials, summary.scored, summary.passed, summary.errors) == (
        3,
        2,
        1,
        1,
    )
    assert (summary.cases, summary.pass_rate) == (2, 0.5)
    low, high = summary.pass_rate_ci95
    assert 0 < low < 0.5 < high < 1
    assert (summary.input_tokens, summary.cache_tokens, summary.output_tokens) == (
        205,
        80,
        20,
    )
    assert (summary.cost_usd, summary.cost_per_pass_usd) == (0.5, 0.5)
    assert (summary.tasks, summary.agents, summary.models) == (
        ("a", "b", "c"),
        ("vis-solo",),
        ("openai/gpt",),
    )
    assert (summary.started_at, summary.finished_at) == (
        "2026-01-01T00:00:01Z",
        "2026-01-01T00:10:00Z",
    )
    empty = summarize(())
    assert (empty.pass_rate, empty.pass_rate_ci95, empty.cost_usd) == (None, None, None)


def test_seal_round_has_one_row_for_each_trial_and_a_digest(tmp_path):
    job = _job(tmp_path / "jobs" / "r1" / "vis-solo__openai__a")
    path = _trial(job, "t1", "a")
    _trial(job, "t2", "b", reward=None)
    runs = [{"commit": "abc", "dirty": False, "pins": {"harbor": "0.23.0"}}]
    seal = seal_round(
        "r1", runs, read_trials(job), tmp_path, sealed_at="2026-01-02T00:00:00Z"
    )
    assert (seal["round"], seal["sealed_at"], seal["runs"]) == (
        "r1",
        "2026-01-02T00:00:00Z",
        runs,
    )
    first = seal["rows"][0]
    assert sorted(first) == sorted(
        [
            "agent",
            "model",
            "task",
            "reward",
            "error",
            "input",
            "cache",
            "output",
            "agent_version",
            "task_checksum",
            "started_at",
            "finished_at",
            "result",
            "result_sha256",
        ]
    )
    assert (first["task"], first["result"], first["task_checksum"]) == (
        "a",
        "jobs/r1/vis-solo__openai__a/t1/result.json",
        "sum-a",
    )
    assert first["result_sha256"] == hashlib.sha256(path.read_bytes()).hexdigest()
    assert seal["rows"][1]["reward"] is None
    body = {key: value for key, value in seal.items() if key != "digest"}
    expected = hashlib.sha256(json.dumps(body, sort_keys=True).encode()).hexdigest()
    assert seal["digest"] == seal_digest(seal) == expected
    seal["rows"][0]["reward"] = 0.0
    assert seal_digest(seal) != seal["digest"]
