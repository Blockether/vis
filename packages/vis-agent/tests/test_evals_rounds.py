"""Tests for the round summary of a Harbor job folder in blockether.vis.evals."""

import json

from blockether.vis.evals import bench_report, diagnose, round_summary

MODEL = "glm-5.3"
PROVIDER = "zai"


def step(step_id, iteration, *, error=None, tokens=10):
    calls = []
    if error:
        calls.append(
            {
                "tool_call_id": f"call-{step_id}",
                "function_name": "python_execution",
                "arguments": {},
                "extra": {"error": {"type": error, "message": "a log line"}},
            }
        )
    return {
        "step_id": step_id,
        "source": "agent",
        "message": "",
        "metrics": {"completion_tokens": tokens},
        "tool_calls": calls,
        "extra": {"iteration": iteration, **({"error": "a log line"} if error else {})},
    }


def trajectory(*, model=MODEL, failing=0, tokens=10):
    steps = [step(1, 1, tokens=tokens)]
    steps += [step(2 + n, 2 + n, error="python-worker-retired") for n in range(failing)]
    return {
        "schema_version": "ATIF-v1.6",
        "agent": {"name": "vis", "model_name": model, "extra": {"provider": PROVIDER}},
        "steps": steps,
        "final_metrics": {
            "total_prompt_tokens": 100,
            "total_completion_tokens": tokens,
            "total_cost_usd": 0.25,
            "iteration_count": len(steps),
        },
        "extra": {"error": "a log line"} if failing else {},
    }


def add_trial(jobs, task, *, reward, tests, atif=None, exception=None, raw=None):
    folder = jobs / "job-1" / f"{task}__abc"
    (folder / "agent").mkdir(parents=True)
    (folder / "verifier").mkdir()
    result = {
        "task_name": f"terminal-bench/{task}",
        "trial_name": f"{task}__abc",
        "started_at": "2026-01-02T00:00:00Z",
        "finished_at": "2026-01-02T00:10:00Z",
        "agent_execution": {
            "started_at": "2026-01-02T00:00:00Z",
            "finished_at": "2026-01-02T00:05:00Z",
        },
        "agent_result": {"n_input_tokens": 100, "n_cache_tokens": 40},
        "verifier_result": {"rewards": {"reward": reward}},
        "exception_info": {"exception_type": exception} if exception else None,
    }
    (folder / "result.json").write_text(json.dumps(result))
    if raw is not None:
        (folder / "agent/trajectory.json").write_text(raw)
    elif atif is not None:
        (folder / "agent/trajectory.json").write_text(json.dumps(atif))
    report = {"results": {"summary": {"tests": tests[1], "passed": tests[0]}}}
    (folder / "verifier/ctrf.json").write_text(json.dumps(report))


def dataset(tmp_path, *names, gpu=()):
    root = tmp_path / "dataset"
    for name in names:
        (root / name).mkdir(parents=True)
        gpus = 1 if name in gpu else 0
        (root / name / "task.toml").write_text(f"[environment]\ngpus = {gpus}\n")
    return root


def by_task(summary, name):
    return next(t for t in summary["trials"] if t["task"].endswith(f"/{name}"))


def test_round_summary_scores_each_task_and_names_its_outcome(tmp_path):
    jobs = tmp_path / "jobs"
    add_trial(jobs, "alpha", reward=1.0, tests=(5, 5), atif=trajectory())
    add_trial(jobs, "beta", reward=0.0, tests=(4, 5), atif=trajectory())
    add_trial(jobs, "gamma", reward=0.0, tests=(0, 5), exception="AgentTimeoutError")
    add_trial(jobs, "delta", reward=1.0, tests=(5, 5), atif=trajectory(model="other"))
    add_trial(jobs, "zeta", reward=0.0, tests=(0, 5), atif=trajectory(failing=3))
    names = ("alpha", "beta", "gamma", "delta", "zeta", "eta")
    tasks = dataset(tmp_path, *names, gpu=("eta",))

    summary = round_summary(jobs, tasks, model=f"{PROVIDER}/{MODEL}")

    assert summary["total_dataset_tasks"] == 6
    assert summary["gpu_required_tasks"] == ["eta"]
    assert summary["unattempted_tasks"] == ["eta"]
    assert summary["scored_tasks"] == 4
    assert summary["solved_tasks"] == ["alpha"]
    assert summary["task_outcomes"] == {
        "agent_error:python-worker-retired": ["zeta"],
        "agent_timeout": ["gamma"],
        "failed_tests": ["beta"],
        "solved": ["alpha"],
    }
    assert summary["unscored_model_tasks"] == []
    assert summary["usage_totals"]["model_attempts"] == 4
    assert summary["usage_totals"]["estimated_metered_api_cost_usd"] == 0.75
    assert summary["repeated_final_errors"] == [
        {"trial": "job-1/zeta__abc", "type": "python-worker-retired", "iterations": 3}
    ]
    beta = by_task(summary, "beta")
    assert beta["verifier_tests"] == {"tests": 5, "passed": 4}
    assert beta["agent_seconds"] == 300
    assert beta["iterations"] == 1
    assert beta["tokens"] == {"input": 100, "cached": 40, "output": None}
    assert json.loads(json.dumps(summary)) == summary


def test_round_summary_copies_no_message_or_log_text(tmp_path):
    jobs = tmp_path / "jobs"
    add_trial(jobs, "zeta", reward=0.0, tests=(0, 5), atif=trajectory(failing=2))

    assert "a log line" not in json.dumps(round_summary(jobs))


def test_round_summary_names_a_harbor_exception(tmp_path):
    jobs = tmp_path / "jobs"
    add_trial(
        jobs,
        "alpha",
        reward=0.0,
        tests=(0, 5),
        atif=trajectory(),
        exception="RuntimeError",
    )

    assert round_summary(jobs)["task_outcomes"] == {
        "agent_error:RuntimeError": ["alpha"]
    }


def test_round_summary_skips_runs_without_model_output(tmp_path):
    jobs = tmp_path / "jobs"
    add_trial(jobs, "alpha", reward=0.0, tests=(0, 5), atif=trajectory(tokens=0))
    add_trial(jobs, "beta", reward=0.0, tests=(0, 5), raw="not json")

    summary = round_summary(jobs)

    assert summary["scored_tasks"] == 0
    assert summary["unscored_model_tasks"] == ["alpha"]
    assert by_task(summary, "beta")["trajectory"]["valid"] is False


def test_round_summary_feeds_the_report_and_the_diagnosis(tmp_path):
    jobs = tmp_path / "jobs"
    add_trial(jobs, "alpha", reward=1.0, tests=(5, 5), atif=trajectory())
    add_trial(jobs, "beta", reward=0.0, tests=(4, 5), atif=trajectory())

    summary = round_summary(jobs)

    report = bench_report(summary)
    assert (report.total_tasks, report.scored_tasks, report.solved_tasks) == (2, 2, 1)
    assert diagnose(summary).scored_tasks == 2


def test_round_summary_without_trials_is_empty(tmp_path):
    summary = round_summary(tmp_path / "jobs")
    assert summary["attempts"] == 0
    assert summary["task_pass_rate"] is None
