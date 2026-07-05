#!/usr/bin/env python3
"""Emit Senior SWE-Bench run metrics without collapsing to pass/fail."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


def load_json(path: Path) -> Any:
    if not path.exists():
        return None
    return json.loads(path.read_text())


def first_json(run_dir: Path, *names: str) -> Any:
    for name in names:
        for path in sorted(run_dir.glob(f"**/{name}")):
            data = load_json(path)
            if data is not None:
                return data
    return None


def first_harbor_exception(run_dir: Path) -> dict[str, Any] | None:
    for path in sorted(run_dir.glob("harbor-output/**/result.json")):
        data = load_json(path)
        if isinstance(data, dict) and isinstance(data.get("exception_info"), dict):
            return data["exception_info"]
    return None


def harbor_log_failure_class(run_dir: Path) -> str | None:
    log = run_dir / "harbor.log"
    if not log.exists():
        return None
    text = log.read_text(errors="replace")
    docker_markers = [
        "Docker daemon is not running",
        "Cannot connect to the Docker daemon",
        "Is the docker daemon running",
    ]
    if any(marker in text for marker in docker_markers):
        return "docker_daemon_unavailable"
    return None


def preflight_failure(run_dir: Path) -> dict[str, Any] | None:
    data = load_json(run_dir / "preflight-failure.json")
    return data if isinstance(data, dict) else None


def install_only_success(run_dir: Path) -> bool:
    for path in sorted(run_dir.glob("harbor-output/**/result.json")):
        data = load_json(path)
        if not isinstance(data, dict):
            continue
        config = data.get("config")
        if isinstance(config, dict) and config.get("install_only") is True and data.get("exception_info") is None:
            return True
    return False


def patch_bloat(diff: str) -> dict[str, int]:
    added = deleted = files = 0
    for line in diff.splitlines():
        if line.startswith("diff --git "):
            files += 1
        elif line.startswith("+") and not line.startswith("+++"):
            added += 1
        elif line.startswith("-") and not line.startswith("---"):
            deleted += 1
    return {"files": files, "added_lines": added, "deleted_lines": deleted, "total_changed_lines": added + deleted}


def _cost_usd(cost: dict[str, Any]) -> Any:
    for key in ("total-cost", "total_cost", "cost_usd", "usd"):
        if key in cost:
            return cost[key]
    return None


def _merge_trace_metrics(stats: dict[str, Any], source: dict[str, Any], max_iteration: int) -> int:
    if isinstance(source.get("tokens"), dict):
        stats["tokens"] = source["tokens"]
    if isinstance(source.get("cost"), dict):
        cost = _cost_usd(source["cost"])
        if cost is not None:
            stats["cost_usd"] = cost
    if isinstance(source.get("elapsed_s"), (int, float)):
        stats["elapsed_s"] = source["elapsed_s"]
    if isinstance(source.get("duration-ms"), (int, float)):
        stats["elapsed_s"] = source["duration-ms"] / 1000.0
    if isinstance(source.get("duration_ms"), (int, float)):
        stats["elapsed_s"] = source["duration_ms"] / 1000.0
    iteration_count = source.get("iteration-count", source.get("iteration_count"))
    if isinstance(iteration_count, int):
        max_iteration = max(max_iteration, iteration_count)
    return max_iteration


def summarize_trace(trace: Path) -> dict[str, Any]:
    stats: dict[str, Any] = {
        "iterations": 0,
        "tool_calls": 0,
        "shell_calls": 0,
        "failed_shell_calls": 0,
        "file_reads": 0,
        "elapsed_s": None,
        "tokens": {},
        "cost_usd": None,
    }
    if not trace.exists():
        return stats
    max_iteration = 0
    for line in trace.read_text(errors="replace").splitlines():
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        payload = obj.get("payload") if isinstance(obj.get("payload"), dict) else {}
        phase = obj.get("phase") or obj.get("type") or payload.get("phase")
        if phase in {"turn-complete", "iteration"}:
            stats["iterations"] += 1
        iteration = obj.get("iteration", payload.get("iteration"))
        if isinstance(iteration, int):
            max_iteration = max(max_iteration, iteration)
        tool = str(obj.get("tool") or obj.get("name") or "")
        if tool:
            stats["tool_calls"] += 1
        if "shell" in tool:
            stats["shell_calls"] += 1
            if obj.get("exit") not in (None, 0) or obj.get("error"):
                stats["failed_shell_calls"] += 1
        if tool in {"cat", "rg", "find_files", "ls", "outline"}:
            stats["file_reads"] += 1
        max_iteration = _merge_trace_metrics(stats, obj, max_iteration)
        max_iteration = _merge_trace_metrics(stats, payload, max_iteration)
        final_payload = payload.get("final")
        if isinstance(final_payload, dict):
            max_iteration = _merge_trace_metrics(stats, final_payload, max_iteration)
    if max_iteration:
        stats["iterations"] = max(stats["iterations"], max_iteration)
    return stats


def task_metadata(dataset_copy: Path | None, task_id: str) -> dict[str, Any]:
    if not dataset_copy:
        return {}
    toml = dataset_copy / "tasks" / task_id / "task.toml"
    if not toml.exists():
        return {}
    try:
        import tomllib
        data = tomllib.loads(toml.read_text())
    except ModuleNotFoundError:
        from list_tasks import _mini_toml_metadata
        data = _mini_toml_metadata(toml.read_text())
    md = data.get("metadata", {})
    tx = md.get("taxonomy", {})
    return {"segment": md.get("segment"), "task_type": tx.get("task_type"), "tags": md.get("tags", [])}


def verifier_summary(verifier: Any) -> dict[str, Any] | None:
    if not isinstance(verifier, dict):
        return None
    return {
        "passed": verifier.get("passed"),
        "total": verifier.get("total"),
        "all_pass": verifier.get("all_pass"),
        "runner_errors": verifier.get("runner_errors"),
    }


def judge_summary(judge_output: Any) -> dict[str, Any] | None:
    if not isinstance(judge_output, dict):
        return None
    keys = [
        "rubric_status",
        "rubric_error",
        "taste_status",
        "taste_error",
        "patch_bloat",
        "patch_classifications",
    ]
    return {key: judge_output[key] for key in keys if key in judge_output}


def first_present(mapping: dict[str, Any], *keys: str) -> Any:
    for key in keys:
        if key in mapping:
            return mapping[key]
    return None


def failure_class(
    resolved: Any,
    reward: Any,
    verifier: Any,
    harbor_exception: dict[str, Any] | None,
    preflight_error: dict[str, Any] | None = None,
    log_failure: str | None = None,
    install_only_ok: bool = False,
) -> str | None:
    if resolved:
        return None
    if install_only_ok:
        return None
    if preflight_error and preflight_error.get("failure_class"):
        return str(preflight_error["failure_class"])
    if harbor_exception and harbor_exception.get("exception_type"):
        return str(harbor_exception["exception_type"])
    if isinstance(verifier, dict):
        if verifier.get("runner_errors"):
            return "verifier_runner_failure"
        if verifier.get("total") and verifier.get("all_pass") is False:
            return "verifier_failed"
    if isinstance(reward, dict) and reward.get("reward") == 0:
        return "reward_zero"
    if log_failure:
        return log_failure
    return "unknown"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--task-id", required=True)
    ap.add_argument("--run-dir", type=Path, required=True)
    ap.add_argument("--harbor-reward", type=Path)
    ap.add_argument("--patch", type=Path)
    ap.add_argument("--trace", type=Path)
    ap.add_argument("--dataset-copy", type=Path)
    ap.add_argument("--out", type=Path, required=True)
    args = ap.parse_args()

    reward = load_json(args.harbor_reward) if args.harbor_reward else first_json(args.run_dir, "reward.json", "score.json")
    verifier = first_json(args.run_dir, "verifier_results.json")
    judge_output = first_json(args.run_dir, "judge_output.json")
    harbor_exception = first_harbor_exception(args.run_dir)
    preflight_error = preflight_failure(args.run_dir)
    log_failure = harbor_log_failure_class(args.run_dir)
    install_only_ok = install_only_success(args.run_dir)
    patch_text = args.patch.read_text(errors="replace") if args.patch and args.patch.exists() else ""
    trace = args.trace or (args.run_dir / "vis-traces" / args.task_id / "vis.trace.jsonl")
    meta = task_metadata(args.dataset_copy, args.task_id)

    resolved = None
    verifier_pass = None
    validation_agent_pass = None
    rubric_score = None
    if isinstance(reward, dict):
        resolved = reward.get("resolved")
        verifier_pass = first_present(reward, "verifier_pass", "tests_passed")
        validation_agent_pass = first_present(reward, "validation_agent_pass")
        rubric_score = first_present(reward, "rubric_score", "score")
    if isinstance(verifier, dict) and verifier_pass is None and verifier.get("total") is not None:
        verifier_pass = verifier.get("all_pass")

    summary = {
        "task_id": args.task_id,
        "segment": meta.get("segment", "Investigate-and-Fix"),
        "harbor_reward": reward,
        "harbor_exception": harbor_exception,
        "preflight_failure": preflight_error,
        "resolved": bool(resolved) if resolved is not None else None,
        "tasteful_solve": False,
        "verifier_pass": bool(verifier_pass) if verifier_pass is not None else None,
        "verifier": verifier_summary(verifier),
        "judge": judge_summary(judge_output),
        "install_only_success": install_only_ok,
        "validation_agent_pass": bool(validation_agent_pass) if validation_agent_pass is not None else None,
        "rubric_score": rubric_score,
        "patch_bloat": patch_bloat(patch_text),
        "vis": summarize_trace(trace),
        "failure_class": failure_class(
            resolved,
            reward,
            verifier,
            harbor_exception,
            preflight_error,
            log_failure,
            install_only_ok,
        ),
    }
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(summary, indent=2, sort_keys=True))
    print(json.dumps(summary, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
