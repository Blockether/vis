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


def _task_entry(run_dir: Path, exit_status: int | None = None) -> dict[str, Any]:
    command = _command(run_dir)
    summary = _summary(run_dir)
    collection = _collection(run_dir)
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

    return {
        "task_id": task_id,
        "run_id": command.get("run_id") or _run_id_from_dir(run_dir),
        "run_dir": str(run_dir),
        "exit_status": exit_status,
        "failure_class": failure_class,
        "install_only_success": summary.get("install_only_success"),
        "verifier_pass": summary.get("verifier_pass"),
        "resolved": summary.get("resolved"),
        "harbor_exception_type": harbor_exception_type,
        "patch_bloat": summary.get("patch_bloat"),
        "vis_iterations": (summary.get("vis") or {}).get("iterations") if isinstance(summary.get("vis"), dict) else None,
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


def summarize(
    *,
    subset_name: str,
    run_dirs: list[tuple[Path, int | None]],
    subset_path: str | None = None,
    manifest_path: str | None = None,
) -> dict[str, Any]:
    tasks = [_task_entry(run_dir, exit_status) for run_dir, exit_status in run_dirs]
    failure_counts = Counter(str(task["failure_class"]) for task in tasks if task.get("failure_class"))
    exit_statuses = [task.get("exit_status") for task in tasks if task.get("exit_status") is not None]
    exit_status = max(exit_statuses) if exit_statuses else (1 if failure_counts else 0 if tasks else None)
    install_only_values = [task.get("install_only_success") for task in tasks]
    return {
        "subset_name": subset_name,
        "subset_path": subset_path,
        "manifest_path": manifest_path,
        "total_tasks": len(tasks),
        "exit_status": exit_status,
        "failure_counts": dict(sorted(failure_counts.items())),
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
