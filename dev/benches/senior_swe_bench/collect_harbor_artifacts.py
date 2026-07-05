#!/usr/bin/env python3
"""Copy useful Harbor job artifacts into a Senior SWE-Bench run directory."""
from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path
from typing import Any


JOB_FILES = ("result.json", "config.json", "job.log", "lock.json")
TRIAL_FILES = ("result.json", "config.json", "trial.log", "exception.txt", "lock.json")


def _load_json(path: Path) -> Any:
    try:
        return json.loads(path.read_text())
    except Exception:
        return None


def _copy_file(src: Path, dest: Path) -> bool:
    if not src.exists() or not src.is_file():
        return False
    dest.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dest)
    return True


def _copy_tree(src: Path, dest: Path) -> bool:
    if not src.exists() or not src.is_dir():
        return False
    if dest.exists():
        shutil.rmtree(dest)
    shutil.copytree(src, dest)
    return True


def _task_matches(result: dict[str, Any], task_id: str) -> bool:
    if result.get("task_name") == task_id:
        return True
    task = result.get("task_id")
    if isinstance(task, dict):
        task_path = task.get("path")
        if isinstance(task_path, str) and Path(task_path).name == task_id:
            return True
    return False


def _trial_dirs(job_dir: Path, task_id: str) -> list[Path]:
    matched: list[Path] = []
    candidates: list[Path] = []
    for result_path in sorted(job_dir.glob("*/result.json")):
        trial_dir = result_path.parent
        data = _load_json(result_path)
        if isinstance(data, dict):
            candidates.append(trial_dir)
            if _task_matches(data, task_id):
                matched.append(trial_dir)
    return matched or candidates


def collect(job_dir: Path, run_dir: Path, task_id: str) -> dict[str, Any]:
    harbor_dir = run_dir / "harbor-output"
    harbor_dir.mkdir(parents=True, exist_ok=True)

    copied_job_files = []
    for name in JOB_FILES:
        if _copy_file(job_dir / name, harbor_dir / f"job-{name}"):
            copied_job_files.append(name)

    trials = _trial_dirs(job_dir, task_id)
    copied_trials = []
    copied_traces = []
    for idx, trial_dir in enumerate(trials, start=1):
        dest_name = "trial" if len(trials) == 1 else f"trial-{idx}-{trial_dir.name}"
        dest = harbor_dir / dest_name
        dest.mkdir(parents=True, exist_ok=True)

        copied_trial_files = []
        for name in TRIAL_FILES:
            if _copy_file(trial_dir / name, dest / name):
                copied_trial_files.append(name)

        verifier_copied = _copy_tree(trial_dir / "verifier", dest / "verifier")
        _copy_file(trial_dir / "artifacts" / "manifest.json", dest / "artifacts-manifest.json")

        for trace in trial_dir.glob(f"**/vis-traces/{task_id}/vis.trace.jsonl"):
            trace_dest = run_dir / "vis-traces" / task_id
            _copy_tree(trace.parent, trace_dest)
            copied_traces.append(str(trace))

        copied_trials.append(
            {
                "trial_dir": str(trial_dir),
                "dest": str(dest),
                "files": copied_trial_files,
                "verifier": verifier_copied,
            }
        )

    reward_files = sorted(
        str(path)
        for pattern in ("*reward*.json", "*score*.json")
        for path in run_dir.glob(f"**/{pattern}")
    )
    summary = {
        "job_dir": str(job_dir),
        "run_dir": str(run_dir),
        "task_id": task_id,
        "job_files": copied_job_files,
        "trials": copied_trials,
        "traces": copied_traces,
        "reward_files": reward_files,
    }
    (harbor_dir / "collection.json").write_text(json.dumps(summary, indent=2, sort_keys=True))
    return summary


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--job-dir", type=Path, required=True)
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--task-id", required=True)
    args = parser.parse_args()

    print(json.dumps(collect(args.job_dir, args.run_dir, args.task_id), indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
