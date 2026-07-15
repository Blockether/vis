#!/usr/bin/env python3
"""Aggregate per-task Senior SWE-Bench run summaries into a subset ledger."""
from __future__ import annotations

import argparse
import json
from collections import Counter
from pathlib import Path
from typing import Any


def load_json(path: Path) -> Any:
    if not path.exists():
        return None
    return json.loads(path.read_text())


def _run_id_from_dir(run_dir: Path) -> str:
    return run_dir.name


def _command(run_dir: Path) -> dict[str, Any]:
    data = load_json(run_dir / "command.json")
    return data if isinstance(data, dict) else {}


def _summary(run_dir: Path) -> dict[str, Any]:
    data = load_json(run_dir / "summary.json")
    return data if isinstance(data, dict) else {}


def _collection(run_dir: Path) -> dict[str, Any]:
    data = load_json(run_dir / "harbor-output" / "collection.json")
    return data if isinstance(data, dict) else {}


def _dataset_lock(run_dir: Path) -> dict[str, Any]:
    data = load_json(run_dir / "dataset.lock.json")
    return data if isinstance(data, dict) else {}


def _secret_redaction(run_dir: Path) -> dict[str, Any]:
    data = load_json(run_dir / "secret-redaction.json")
    return data if isinstance(data, dict) else {}


def _unique_dicts(values: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [json.loads(value) for value in sorted({json.dumps(item, sort_keys=True) for item in values})]


def _task_entry(run_dir: Path, exit_status: int | None = None) -> dict[str, Any]:
    command = _command(run_dir)
    summary = _summary(run_dir)
    collection = _collection(run_dir)
    dataset_lock = _dataset_lock(run_dir)
    secret_redaction = _secret_redaction(run_dir)
    task_ids = command.get("task_ids") if isinstance(command.get("task_ids"), list) else []
    task_id = summary.get("task_id") or (task_ids[0] if task_ids else None)
    harbor_exception = summary.get("harbor_exception")
    harbor_exception_type = None
    if isinstance(harbor_exception, dict):
        harbor_exception_type = harbor_exception.get("exception_type")

    failure_class = summary.get("failure_class")
    if not summary:
        failure_class = "missing_summary"
    elif exit_status not in (None, 0) and not failure_class:
        failure_class = "run_failed"

    agent = summary.get("agent") if isinstance(summary.get("agent"), dict) else {}
    harbor = summary.get("harbor") if isinstance(summary.get("harbor"), dict) else {}
    completion = summary.get("completion") if isinstance(summary.get("completion"), dict) else {}
    agent_usage = agent.get("usage") if isinstance(agent.get("usage"), dict) else {}
    verifier_usage = summary.get("verifier_usage") if isinstance(summary.get("verifier_usage"), dict) else {}
    agent_trace = summary.get("agent_trace") if isinstance(summary.get("agent_trace"), dict) else {}
    verifier_config = {
        "provider": command.get("vis_bench_verifier_provider"),
        "base_url": command.get("vis_bench_verifier_openai_base_url"),
        "judge_model": command.get("vis_bench_verifier_judge_model"),
        "classifier_model": command.get("vis_bench_verifier_classifier_model"),
        "validation_agent_model": command.get("vis_bench_verifier_va_model"),
        "validation_agent_harness": command.get("vis_bench_verifier_va_harness"),
        "tool_choice_compat": command.get("vis_bench_verifier_tool_choice_compat"),
        "response_format_compat": command.get("vis_bench_verifier_response_format_compat"),
        "usage_capture": command.get("vis_bench_verifier_usage_capture"),
    }

    return {
        "task_id": task_id,
        "run_id": command.get("run_id") or _run_id_from_dir(run_dir),
        "run_dir": str(run_dir),
        "exit_status": exit_status,
        "failure_class": failure_class,
        "agent": {
            key: agent.get(key)
            for key in (
                "name",
                "harness",
                "version",
                "requested_version",
                "version_matches_requested",
                "provider",
                "model",
                "reasoning_effort",
                "reasoning_effort_explicit",
                "pi_thinking_level",
                "output_cap",
                "vis_eval_valid",
                "pi_models_sha256",
                "endpoint",
                "route",
                "artifact",
            )
        },
        "harbor": harbor,
        "completion": completion,
        "usage": agent_usage,
        "agent_usage": agent_usage,
        "agent_trace": agent_trace,
        "verifier_usage": verifier_usage,
        "secret_redaction": secret_redaction,
        "dataset_commit": dataset_lock.get("dataset_commit"),
        "task_checksum": summary.get("task_checksum"),
        "verifier_config": verifier_config,
        "install_only_success": summary.get("install_only_success"),
        "verifier_pass": summary.get("verifier_pass"),
        "resolved": summary.get("resolved"),
        "harbor_exception_type": harbor_exception_type,
        "patch_bloat": summary.get("patch_bloat"),
        "vis_iterations": (summary.get("vis") or {}).get("iterations") if isinstance(summary.get("vis"), dict) else None,
        "agent_iterations": agent_trace.get("iterations"),
        "agent_tool_calls": agent_trace.get("tool_calls"),
        "task_base_image": command.get("task_base_image"),
        "selected_task_image": command.get("selected_task_image"),
        "harbor_job_dir": collection.get("job_dir"),
        "collected_trials": len(collection.get("trials", [])) if isinstance(collection.get("trials"), list) else 0,
    }


def _manifest_runs(path: Path) -> tuple[dict[str, Any], list[tuple[Path, int | None]]]:
    data = load_json(path)
    if not isinstance(data, dict):
        raise SystemExit(f"manifest must be a JSON object: {path}")
    runs = []
    for item in data.get("runs", []):
        if not isinstance(item, dict) or not item.get("run_dir"):
            continue
        status = item.get("exit_status")
        runs.append((Path(item["run_dir"]), int(status) if status is not None else None))
    return data, runs


def _aggregate_task_usage(tasks: list[dict[str, Any]], key: str, scope: str) -> dict[str, Any]:
    usages = [task.get(key, {}) for task in tasks if task.get(key, {}).get("available") is True]
    complete_usages = [usage for usage in usages if usage.get("complete") is True]
    token_keys = [
        "input_tokens",
        "cached_input_tokens",
        "uncached_input_tokens",
        "cache_write_tokens",
        "output_tokens",
        "reasoning_tokens",
        "total_tokens",
    ]
    token_values = {
        token_key: [
            value
            for usage in usages
            if isinstance((value := usage.get(token_key)), (int, float))
        ]
        for token_key in token_keys
    }
    costs = [
        usage["reported_cost_usd"]
        for usage in usages
        if isinstance(usage.get("reported_cost_usd"), (int, float))
    ]
    calls = [usage["calls"] for usage in usages if isinstance(usage.get("calls"), int)]
    return {
        "scope": scope,
        "tasks_with_usage": len(usages),
        "tasks_missing_usage": len(tasks) - len(usages),
        "tasks_with_complete_usage": len(complete_usages),
        "complete": bool(tasks) and len(complete_usages) == len(tasks),
        "tokens": {token_key: sum(values) if values else None for token_key, values in token_values.items()},
        "token_coverage": {token_key: len(values) for token_key, values in token_values.items()},
        "reported_cost_usd": sum(costs) if costs else None,
        "tasks_with_reported_cost": len(costs),
        "calls": sum(calls) if calls else None,
    }


def _aggregate_task_telemetry(tasks: list[dict[str, Any]]) -> dict[str, Any]:
    traces = [task.get("agent_trace", {}) for task in tasks if task.get("agent_trace", {}).get("available") is True]
    complete_traces = [trace for trace in traces if trace.get("telemetry_complete") is True]
    def numeric(key: str) -> int | float | None:
        values = [
            value
            for trace in complete_traces
            if isinstance((value := trace.get(key)), (int, float))
        ]
        return sum(values) if values else None
    sources = sorted({str(trace.get("source")) for trace in traces if trace.get("source")})
    return {
        "tasks_with_trace": len(traces),
        "tasks_missing_trace": len(tasks) - len(traces),
        "tasks_with_complete_trace": len(complete_traces),
        "complete": bool(tasks) and len(complete_traces) == len(tasks),
        "iterations": numeric("iterations"),
        "tool_calls": numeric("tool_calls"),
        "shell_calls": numeric("shell_calls"),
        "file_reads": numeric("file_reads"),
        "sources": sources,
    }


def summarize(
    *,
    subset_name: str,
    run_dirs: list[tuple[Path, int | None]],
    subset_path: str | None = None,
    manifest_path: str | None = None,
) -> dict[str, Any]:
    tasks = [_task_entry(run_dir, exit_status) for run_dir, exit_status in run_dirs]
    failure_counts = Counter(str(task["failure_class"]) for task in tasks if task.get("failure_class"))
    completion_counts = Counter(
        str(task["completion"].get("status"))
        for task in tasks
        if isinstance(task.get("completion"), dict) and task["completion"].get("status")
    )
    scoreable_tasks = sum(task.get("completion", {}).get("scoreable") is True for task in tasks)
    completed_tasks = sum(task.get("completion", {}).get("complete") is True for task in tasks)
    passed_tasks = sum(task.get("completion", {}).get("passed") is True for task in tasks)
    agent_usage = _aggregate_task_usage(tasks, "agent_usage", "agent")
    agent_telemetry = _aggregate_task_telemetry(tasks)
    verifier_usage = _aggregate_task_usage(tasks, "verifier_usage", "verifier")
    redaction_reports = [task["secret_redaction"] for task in tasks if task.get("secret_redaction")]
    clean_redaction_reports = [report for report in redaction_reports if report.get("clean") is True]
    secret_redaction = {
        "tasks_with_report": len(redaction_reports),
        "tasks_clean": len(clean_redaction_reports),
        "complete": bool(tasks) and len(clean_redaction_reports) == len(tasks),
    }
    expected_task_ids: list[str] | None = None
    if subset_path:
        subset_data = load_json(Path(subset_path))
        if isinstance(subset_data, dict) and isinstance(subset_data.get("task_ids"), list):
            expected_task_ids = [str(task_id) for task_id in subset_data["task_ids"]]
    actual_task_ids = [str(task["task_id"]) for task in tasks if task.get("task_id")]
    actual_counts = Counter(actual_task_ids)
    duplicate_task_ids = sorted(task_id for task_id, count in actual_counts.items() if count > 1)
    missing_task_ids = sorted(set(expected_task_ids or []) - set(actual_task_ids))
    unexpected_task_ids = sorted(set(actual_task_ids) - set(expected_task_ids or [])) if expected_task_ids is not None else []
    all_tasks_present = expected_task_ids is None or (
        not duplicate_task_ids
        and not missing_task_ids
        and not unexpected_task_ids
        and len(actual_task_ids) == len(expected_task_ids)
    )
    agents = _unique_dicts([task["agent"] for task in tasks if any(task["agent"].values())])
    verifiers = _unique_dicts([task["verifier_config"] for task in tasks if any(task["verifier_config"].values())])
    dataset_commits = sorted({str(task["dataset_commit"]) for task in tasks if task.get("dataset_commit")})
    artifacts = _unique_dicts(
        [task["agent"]["artifact"] for task in tasks if isinstance(task["agent"].get("artifact"), dict)]
    )
    harbor_versions = sorted(
        {str(task["harbor"]["version"]) for task in tasks if task.get("harbor", {}).get("version")}
    )
    harbor_locked_versions = sorted(
        {
            str(task["harbor"]["locked_version"])
            for task in tasks
            if task.get("harbor", {}).get("locked_version")
        }
    )
    exit_statuses = [task.get("exit_status") for task in tasks if task.get("exit_status") is not None]
    exit_status = max(exit_statuses) if exit_statuses else (1 if failure_counts else 0 if tasks else None)
    if exit_status in (None, 0) and tasks and completed_tasks < len(tasks) and not all(task.get("install_only_success") is True for task in tasks):
        exit_status = 3
    install_only_values = [task.get("install_only_success") for task in tasks]
    return {
        "subset_name": subset_name,
        "subset_path": subset_path,
        "manifest_path": manifest_path,
        "total_tasks": len(tasks),
        "exit_status": exit_status,
        "failure_counts": dict(sorted(failure_counts.items())),
        "completion": {
            "counts": dict(sorted(completion_counts.items())),
            "expected_tasks": len(expected_task_ids) if expected_task_ids is not None else None,
            "expected_task_ids": expected_task_ids,
            "actual_task_ids": actual_task_ids,
            "duplicate_task_ids": duplicate_task_ids,
            "missing_task_ids": missing_task_ids,
            "unexpected_task_ids": unexpected_task_ids,
            "all_tasks_present": all_tasks_present,
            "scoreable_tasks": scoreable_tasks,
            "unscoreable_tasks": len(tasks) - scoreable_tasks,
            "completed_tasks": completed_tasks,
            "incomplete_tasks": len(tasks) - completed_tasks,
            "passed_tasks": passed_tasks,
            "pass_rate": (passed_tasks / scoreable_tasks) if scoreable_tasks else None,
            "authoritative": bool(tasks) and all_tasks_present and completed_tasks == len(tasks),
        },
        "usage": agent_usage,
        "agent_usage": agent_usage,
        "agent_telemetry": agent_telemetry,
        "verifier_usage": verifier_usage,
        "spend_reporting_complete": agent_usage["complete"] and verifier_usage["complete"],
        "tool_telemetry_complete": agent_telemetry["complete"],
        "secret_redaction": secret_redaction,
        "secret_redaction_complete": secret_redaction["complete"],
        "comparison_ready": bool(tasks)
        and all_tasks_present
        and completed_tasks == len(tasks)
        and agent_usage["complete"]
        and agent_telemetry["complete"],
        "provenance": {
            "dataset_commits": dataset_commits,
            "agents": agents,
            "verifiers": verifiers,
            "artifacts": artifacts,
            "harbor_versions": harbor_versions,
            "harbor_locked_versions": harbor_locked_versions,
            "consistent": (
                len(dataset_commits) <= 1
                and len(agents) <= 1
                and len(verifiers) <= 1
                and len(artifacts) <= 1
                and len(harbor_versions) <= 1
                and len(harbor_locked_versions) <= 1
            ),
        },
        "all_install_only_success": bool(tasks) and all(value is True for value in install_only_values),
        "tasks": tasks,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--subset-name", required=True)
    parser.add_argument("--subset-path")
    parser.add_argument("--manifest", type=Path)
    parser.add_argument("--run-dir", action="append", type=Path, default=[])
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()

    manifest_data: dict[str, Any] = {}
    manifest_runs: list[tuple[Path, int | None]] = []
    if args.manifest:
        manifest_data, manifest_runs = _manifest_runs(args.manifest)

    run_dirs = manifest_runs + [(path, None) for path in args.run_dir]
    subset_name = manifest_data.get("subset_name") or args.subset_name
    subset_path = manifest_data.get("subset_path") or args.subset_path
    result = summarize(
        subset_name=str(subset_name),
        subset_path=str(subset_path) if subset_path else None,
        manifest_path=str(args.manifest) if args.manifest else None,
        run_dirs=run_dirs,
    )
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
