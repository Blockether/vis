"""Summarize local Harbor trials without copying prompts or credentials."""

import argparse
import gzip
import io
import json
import subprocess
import tomllib
from collections import Counter
from datetime import datetime
from pathlib import Path


def elapsed_seconds(start: str | None, end: str | None) -> float | None:
    if not start or not end:
        return None
    start_time = datetime.fromisoformat(start.replace("Z", "+00:00"))
    end_time = datetime.fromisoformat(end.replace("Z", "+00:00"))
    return round((end_time - start_time).total_seconds(), 3)


def relative(path: Path, root: Path) -> str:
    try:
        return str(path.relative_to(root))
    except ValueError:
        return str(path)


def trace_summary(path: Path, root: Path) -> dict | None:
    if not path.is_file():
        for extension in (".gz", ".zst"):
            candidate = path.with_name(path.name + extension)
            if candidate.is_file():
                path = candidate
                break
    if not path.is_file():
        return None

    events = Counter()
    phases = Counter()
    tools = Counter()
    providers = Counter()
    invalid_lines = 0
    iterations = 0
    result_status = None
    result_error_type = None
    last_provider_error_usage = None
    truncated = False
    process = None
    if path.suffix == ".zst":
        process = subprocess.Popen(
            ["zstd", "-dc", "--", str(path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
        stream = io.TextIOWrapper(process.stdout, encoding="utf-8")
    else:
        opener = gzip.open if path.suffix == ".gz" else open
        stream = opener(path, "rt", encoding="utf-8")
    with stream:
        while True:
            try:
                line = next(stream)
            except StopIteration:
                break
            except (EOFError, OSError):
                truncated = True
                break
            try:
                frame = json.loads(line)
            except json.JSONDecodeError:
                invalid_lines += 1
                continue
            events[frame.get("event")] += 1
            payload = frame.get("payload")
            if not isinstance(payload, dict):
                continue
            phase = payload.get("phase")
            if phase:
                phases[phase] += 1
            if phase == "form-start" and payload.get("tool-name"):
                tools[payload["tool-name"]] += 1
            if phase == "provider-call":
                providers[f"{payload.get('provider')}/{payload.get('model')}"] += 1
            iteration = payload.get("iteration")
            if isinstance(iteration, int) and not isinstance(iteration, bool):
                iterations = max(iterations, iteration)
            if frame.get("event") == "result":
                result_status = payload.get("status")
                trace = payload.get("trace")
                if result_status == "error" and isinstance(trace, list):
                    for entry in reversed(trace):
                        if not isinstance(entry, dict) or not isinstance(
                            entry.get("error"), dict
                        ):
                            continue
                        error = entry["error"]
                        error_type = error.get("type")
                        if isinstance(error_type, str) and len(error_type) <= 100:
                            result_error_type = error_type
                        data = error.get("data")
                        usage = (
                            data.get("api-usage") if isinstance(data, dict) else None
                        )
                        if isinstance(usage, dict):
                            last_provider_error_usage = {
                                name: usage[key]
                                for name, key in (
                                    ("input", "input-tokens"),
                                    ("output", "output-tokens"),
                                    ("total", "total-tokens"),
                                )
                                if isinstance(usage.get(key), int)
                                and not isinstance(usage[key], bool)
                                and usage[key] >= 0
                            } or None
                        break

    if process is not None and process.wait() != 0:
        truncated = True
    return {
        "path": relative(path, root),
        "stored_bytes": path.stat().st_size,
        "truncated": truncated,
        "event_counts": dict(events),
        "phase_counts": dict(phases),
        "provider_calls": dict(providers),
        "tool_calls": dict(tools),
        "iterations_observed": iterations,
        "invalid_jsonl_lines": invalid_lines,
        "vis_result_status": result_status,
        "vis_error_type": result_error_type,
        "last_provider_error_usage": last_provider_error_usage,
    }


def verifier_test_counts(path: Path) -> dict | None:
    """Read only numeric test counts from an optional CTRF verifier report."""
    if not path.is_file():
        return None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    results = payload.get("results") if isinstance(payload, dict) else None
    summary = results.get("summary") if isinstance(results, dict) else None
    if not isinstance(summary, dict):
        return None
    return {
        key: summary[key]
        for key in ("tests", "passed", "failed", "skipped", "pending", "other")
        if isinstance(summary.get(key), int)
        and not isinstance(summary[key], bool)
        and summary[key] >= 0
    } or None


def trial_summary(result_path: Path, root: Path) -> dict:
    result = json.loads(result_path.read_text(encoding="utf-8"))
    agent = result.get("agent_result") or {}
    verifier = result.get("verifier_result") or {}
    metadata = (agent.get("metadata") or {}).get("vis") or {}
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

    return {
        "job": result_path.parent.parent.name,
        "task": result.get("task_name"),
        "trial": result.get("trial_name"),
        "status": status,
        "exception_type": exception.get("exception_type") if exception else None,
        "result_path": relative(result_path, root),
        "trace": trace_summary(result_path.parent / "agent/vis-trace.jsonl", root),
        "model": metadata.get("model"),
        "reward": (verifier.get("rewards") or {}).get("reward"),
        "verifier_tests": verifier_test_counts(
            result_path.parent / "verifier/ctrf.json"
        ),
        "tokens": {
            "input": agent.get("n_input_tokens"),
            "cached": agent.get("n_cache_tokens"),
            "output": agent.get("n_output_tokens"),
        },
        "billed_cost_usd": agent.get("cost_usd"),
        "estimated_metered_api_cost_usd": metadata.get(
            "estimated_metered_api_cost_usd"
        ),
        "vis_duration_ms": metadata.get("duration_ms"),
        "vis_iterations": metadata.get("iteration_count"),
        "elapsed_seconds": elapsed_seconds(result.get("started_at"), finished),
        "agent_seconds": elapsed_seconds(
            (result.get("agent_execution") or {}).get("started_at"),
            (result.get("agent_execution") or {}).get("finished_at"),
        ),
        "verifier_seconds": elapsed_seconds(
            (result.get("verifier") or {}).get("started_at"),
            (result.get("verifier") or {}).get("finished_at"),
        ),
    }


def make_report(jobs_dir: Path, dataset_dir: Path) -> dict:
    root = jobs_dir.parent
    trials = [
        trial_summary(path, root) for path in sorted(jobs_dir.glob("*/*/result.json"))
    ]
    task_dirs = sorted(path for path in dataset_dir.iterdir() if path.is_dir())
    gpu_tasks = []
    for task in task_dirs:
        info = tomllib.loads((task / "task.toml").read_text(encoding="utf-8"))
        if (info.get("environment", {}).get("gpus") or 0) > 0:
            gpu_tasks.append(task.name)
    attempted = {
        trial["task"].removeprefix("terminal-bench/")
        for trial in trials
        if isinstance(trial["task"], str)
    }
    verified = [trial for trial in trials if trial["status"] == "verified"]
    rewards = [
        trial["reward"]
        for trial in verified
        if isinstance(trial["reward"], (int, float))
    ]
    return {
        "dataset": "terminal-bench/terminal-bench@4.0.0",
        "total_dataset_tasks": len(task_dirs),
        "gpu_required_tasks": gpu_tasks,
        "unattempted_tasks": [
            task.name for task in task_dirs if task.name not in attempted
        ],
        "attempts": len(trials),
        "verified_attempts": len(verified),
        "exception_attempts": sum(trial["status"] == "exception" for trial in trials),
        "mean_verified_reward": sum(rewards) / len(rewards) if rewards else None,
        "trials": trials,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--jobs", type=Path, default=Path("jobs"))
    parser.add_argument(
        "--dataset", type=Path, default=Path("artifacts/datasets/terminal-bench")
    )
    parser.add_argument("--output", type=Path, default=Path("runs/summary.json"))
    args = parser.parse_args()
    report = make_report(args.jobs, args.dataset)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"{report['verified_attempts']}/{report['attempts']} verified; {args.output}")


if __name__ == "__main__":
    main()
