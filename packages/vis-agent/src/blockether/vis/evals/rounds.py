"""Summarize the Harbor trials of one benchmark round from its job folder.

`round_summary` reads only standard Harbor files: each `<job>/<trial>/result.json`,
the CTRF report `verifier/ctrf.json` and the ATIF trajectory `agent/trajectory.json`.
Any agent that Harbor runs can write these files. It gives the summary that
`bench_report` and `diagnose` read. The summary holds names, counts, error types and
times. It copies no prompt, message, log line or credential.
"""

from __future__ import annotations

import json
import re
from collections import Counter
from datetime import datetime
from pathlib import Path

from .bench import agent_limits, attempt_outcome
from .harbor import read_tasks
from .trajectory import Step, Trajectory, load_trajectory

__all__ = ["round_summary"]

_TRAJECTORY = "agent/trajectory.json"
_KIND = re.compile(r"[A-Za-z0-9_.:-]{1,100}")


def _elapsed(start: str | None, end: str | None) -> float | None:
    if not start or not end:
        return None
    begin = datetime.fromisoformat(start.replace("Z", "+00:00"))
    finish = datetime.fromisoformat(end.replace("Z", "+00:00"))
    return round((finish - begin).total_seconds(), 3)


def _relative(path: Path, root: Path) -> str:
    try:
        return str(path.relative_to(root))
    except ValueError:
        return str(path)


def _count(value) -> bool:
    return isinstance(value, int) and not isinstance(value, bool) and value >= 0


def _kind(value) -> str | None:
    """Keep a short error type name, never free text."""
    return value if isinstance(value, str) and _KIND.fullmatch(value) else None


def _step_error(step: Step) -> str | None:
    """Name the error of one agent step, or None when the step has no error.

    A tool-call error gives its `type`. A timed-out call gives `timeout`. A step error
    without a type gives `step_error`, because its text can hold log lines.
    """
    for call in reversed(step.tool_calls):
        if call.error is not None:
            return _kind(call.error.get("type")) or "tool_error"
        if call.is_timeout:
            return "timeout"
    return "step_error" if step.error is not None else None


def _route(name: str | None, provider: str | None) -> set[str]:
    if name is None:
        return set()
    return {name, f"{provider}/{name}"} if provider else {name}


def _harbor_model(result: dict) -> tuple[str | None, str | None]:
    """Read the model name and provider of `agent_info.model_info` in `result.json`."""
    info = (result.get("agent_info") or {}).get("model_info") or {}
    name, provider = info.get("name"), info.get("provider")
    return (
        name if isinstance(name, str) else None,
        provider if isinstance(provider, str) else None,
    )


def _output(trajectory: Trajectory) -> bool:
    """Tell if the model gave any output tokens."""
    metrics = trajectory.final_metrics
    if metrics and metrics.total_completion_tokens:
        return True
    return any(
        step.metrics is not None and bool(step.metrics.completion_tokens)
        for step in trajectory.agent_steps
    )


def _trajectory(path: Path, root: Path) -> tuple[Trajectory | None, dict | None]:
    """Read one ATIF trajectory and summarize its steps, tools and errors."""
    if not path.is_file():
        return None, None
    try:
        trajectory = load_trajectory(path, strict=False)
    except (OSError, UnicodeDecodeError, ValueError):
        return None, {"path": _relative(path, root), "valid": False}
    steps = trajectory.agent_steps
    errors = Counter(kind for step in steps if (kind := _step_error(step)))
    tools = Counter(call.function_name for call in trajectory.tool_calls)
    # Count the same error at the end of the run, such as a lost sandbox.
    trailing_type, trailing = None, 0
    for step in reversed(steps):
        kind = _step_error(step)
        if kind is None or (trailing_type is not None and kind != trailing_type):
            break
        trailing_type, trailing = kind, trailing + 1
    metrics = trajectory.final_metrics
    iterations = metrics.iteration_count if metrics else None
    if iterations is None:
        seen = [step.iteration for step in steps if step.iteration is not None]
        iterations = max(seen) if seen else None
    summary = {
        "path": _relative(path, root),
        "valid": True,
        "agent": trajectory.agent_name,
        "model": trajectory.model_name,
        "provider": trajectory.provider,
        "agent_steps": len(steps),
        "tool_calls": dict(tools),
        "step_error_types": dict(errors),
        "trailing_errors": (
            {"type": trailing_type, "iterations": trailing} if trailing else None
        ),
        "is_failed": trajectory.error is not None,
        "iterations": iterations,
        "duration_ms": metrics.duration_ms if metrics else None,
        "cost_usd": metrics.total_cost_usd if metrics else None,
    }
    return trajectory, summary


def _tests(path: Path) -> dict | None:
    """Read only the numeric test counts of a CTRF verifier report."""
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    results = payload.get("results") if isinstance(payload, dict) else None
    summary = results.get("summary") if isinstance(results, dict) else None
    if not isinstance(summary, dict):
        return None
    keys = ("tests", "passed", "failed", "skipped", "pending", "other")
    return {key: summary[key] for key in keys if _count(summary.get(key))} or None


def _results(jobs: Path) -> list[tuple[Path, dict]]:
    """Read each trial `result.json` one or two levels below `jobs`, in path order.

    A job writes its own `result.json` without `trial_name`. It is not a trial.
    """
    found = []
    for path in sorted({*jobs.glob("*/result.json"), *jobs.glob("*/*/result.json")}):
        try:
            result = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        if isinstance(result, dict) and result.get("trial_name"):
            found.append((path, result))
    return found


def _trial(result_path: Path, result: dict, root: Path, model: str | None) -> dict:
    folder = result_path.parent
    agent = result.get("agent_result") or {}
    verifier = result.get("verifier_result") or {}
    exception = result.get("exception_info")
    finished = result.get("finished_at")
    if not finished:
        status = "incomplete"
    elif exception:
        status = "exception"
    elif (
        result.get("agent_result") is not None
        and result.get("verifier_result") is not None
    ):
        status = "verified"
    else:
        status = "unverified"
    exception_type = _kind(exception.get("exception_type")) if exception else None
    trajectory, trace = _trajectory(folder / _TRAJECTORY, root)
    # Without a trajectory, the standard Harbor result names the model and its tokens.
    if trajectory is not None:
        name, provider = trajectory.model_name, trajectory.provider
    else:
        name, provider = _harbor_model(result)
    called = (trajectory is not None or name is not None) and (
        model is None or model in _route(name, provider)
    )
    timeout = exception_type == "AgentTimeoutError"
    if trajectory is not None:
        produced = called and _output(trajectory)
    else:
        produced = called and bool(agent.get("n_output_tokens"))
    # Harbor verifies a timed-out agent, which can stop before it writes its
    # trajectory. A model that gave no output measured the runner, not the agent.
    scored = (
        bool(finished)
        and result.get("verifier_result") is not None
        and (timeout and trajectory is None or produced)
    )
    tokens = {
        "input": agent.get("n_input_tokens"),
        "cached": agent.get("n_cache_tokens"),
        "output": agent.get("n_output_tokens"),
    }
    metrics = trajectory.final_metrics if trajectory else None
    if metrics and all(value is None for value in tokens.values()):
        tokens = {
            "input": metrics.total_prompt_tokens,
            "cached": metrics.total_cached_tokens,
            "output": metrics.total_completion_tokens,
        }
    cost = agent.get("cost_usd")
    if cost is None and metrics:
        cost = metrics.total_cost_usd
    execution = result.get("agent_execution") or {}
    check = result.get("verifier") or {}
    return {
        "job": folder.parent.name,
        "dataset": result.get("source"),
        "task": result.get("task_name"),
        "trial": result.get("trial_name"),
        "status": status,
        "exception_type": exception_type,
        "finished_at": finished,
        "model_attempt": called or (timeout and trajectory is None),
        "scored": scored,
        "result_path": _relative(result_path, root),
        "trajectory": trace,
        "model": name,
        "reward": (verifier.get("rewards") or {}).get("reward"),
        "verifier_tests": _tests(folder / "verifier/ctrf.json"),
        "tokens": tokens,
        "cost_usd": cost,
        "iterations": (trace or {}).get("iterations"),
        "elapsed_seconds": _elapsed(result.get("started_at"), finished),
        "agent_seconds": _elapsed(
            execution.get("started_at"), execution.get("finished_at")
        ),
        "verifier_seconds": _elapsed(check.get("started_at"), check.get("finished_at")),
    }


def _total(values) -> float:
    return sum(
        value
        for value in values
        if isinstance(value, (int, float)) and not isinstance(value, bool)
    )


def _usage_totals(trials: list[dict]) -> dict:
    attempts = [trial for trial in trials if trial["model_attempt"]]
    return {
        "model_attempts": len(attempts),
        "input_tokens": _total(trial["tokens"]["input"] for trial in attempts),
        "cached_input_tokens": _total(trial["tokens"]["cached"] for trial in attempts),
        "output_tokens": _total(trial["tokens"]["output"] for trial in attempts),
        "estimated_metered_api_cost_usd": _total(
            trial["cost_usd"] for trial in attempts
        ),
        "agent_hours": _total(trial["agent_seconds"] for trial in attempts) / 3600,
    }


def _name(task) -> str | None:
    """Name a task without its dataset, as `bench_report` and `diagnose` do."""
    return task.rsplit("/", 1)[-1] if isinstance(task, str) and task else None


def _dataset(trials: list[dict], dataset_dir: Path | str | None) -> str:
    """Name the dataset: the most common Harbor `source` of the trials, or the folder."""
    sources = Counter(trial["dataset"] for trial in trials if trial["dataset"])
    if sources:
        return sources.most_common(1)[0][0]
    return Path(dataset_dir).name if dataset_dir is not None else "unknown"


def round_summary(
    jobs_dir: Path | str,
    dataset_dir: Path | str | None = None,
    *,
    model: str | None = None,
    dataset: str | None = None,
) -> dict:
    """Summarize every trial under `jobs_dir`, as `bench_report` and `diagnose` read it.

    `jobs_dir` is one Harbor job folder or a folder of jobs. `dataset_dir` holds one
    folder with a `task.toml` for each task. It gives the total, the GPU tasks, the
    tasks without an attempt and the `agent_limits` that `diagnose` uses. Without it,
    the total is the attempted tasks. `dataset` names the dataset. Without it, the
    name is the `source` that Harbor wrote in the trials. `model` is the model name or
    the `provider/model` route of the agent. An attempt of another model does not
    count. With no `model`, any model counts. Each task counts once, by its latest
    scored attempt.
    """
    jobs = Path(jobs_dir)
    root = jobs.parent
    trials = [_trial(path, result, root, model) for path, result in _results(jobs)]
    attempted = {name for trial in trials if (name := _name(trial["task"]))}
    limits = {} if dataset_dir is None else agent_limits(dataset_dir)
    if dataset_dir is None:
        tasks, gpu_tasks = sorted(attempted), []
    else:
        dataset_tasks = read_tasks(dataset_dir)
        tasks = [task.name for task in dataset_tasks]
        gpu_tasks = [task.name for task in dataset_tasks if task.gpus > 0]
    verified = [trial for trial in trials if trial["status"] == "verified"]
    rewards = [
        trial["reward"]
        for trial in verified
        if isinstance(trial["reward"], (int, float))
        and not isinstance(trial["reward"], bool)
    ]
    scored: dict[str, dict] = {}
    for trial in sorted(
        (trial for trial in trials if trial["scored"] and _name(trial["task"])),
        key=lambda trial: trial["finished_at"],
    ):
        scored[str(_name(trial["task"]))] = trial
    model_tasks = {
        name
        for trial in trials
        if trial["model_attempt"] and (name := _name(trial["task"]))
    }
    solved = sorted(name for name, trial in scored.items() if trial["reward"] == 1)
    outcomes: dict[str, list[str]] = {}
    for name, trial in sorted(scored.items()):
        outcomes.setdefault(attempt_outcome(trial), []).append(name)
    repeated = []
    for trial in trials:
        streak = (trial["trajectory"] or {}).get("trailing_errors")
        if streak and streak["iterations"] > 1:
            repeated.append({"trial": f"{trial['job']}/{trial['trial']}", **streak})
    return {
        "dataset": dataset or _dataset(trials, dataset_dir),
        "total_dataset_tasks": len(tasks),
        "gpu_required_tasks": gpu_tasks,
        "unattempted_tasks": [task for task in tasks if task not in attempted],
        "agent_limits": limits,
        "attempts": len(trials),
        "verified_attempts": len(verified),
        "exception_attempts": sum(trial["status"] == "exception" for trial in trials),
        "mean_verified_reward": sum(rewards) / len(rewards) if rewards else None,
        "scored_tasks": len(scored),
        "solved_tasks": solved,
        "task_pass_rate": len(solved) / len(scored) if scored else None,
        "task_outcomes": dict(sorted(outcomes.items())),
        "unscored_model_tasks": sorted(model_tasks - set(scored)),
        "usage_totals": _usage_totals(trials),
        "repeated_final_errors": repeated,
        "trials": trials,
    }
