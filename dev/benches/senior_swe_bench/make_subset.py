#!/usr/bin/env python3
"""Generate Senior SWE-Bench subset files for longer runs."""
from __future__ import annotations

import argparse
import hashlib
import json
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable

HERE = Path(__file__).resolve().parent

try:
    import list_tasks
except ModuleNotFoundError:  # pragma: no cover - supports importlib-based tests
    import importlib.util

    spec = importlib.util.spec_from_file_location("list_tasks", HERE / "list_tasks.py")
    list_tasks = importlib.util.module_from_spec(spec)  # type: ignore[assignment]
    sys.modules["list_tasks"] = list_tasks
    spec.loader.exec_module(list_tasks)  # type: ignore[union-attr]


DEFAULT_OVERNIGHT_TASKS = 10
PRESET_SUBSETS = {
    "smoke": HERE / "subsets" / "smoke.json",
    "public-5": HERE / "subsets" / "public-5.json",
}


def _task_id(task: dict[str, Any]) -> str:
    return str(task.get("task_id") or "")


def _stable_key(seed: str, *parts: Any) -> str:
    text = "\0".join([seed, *(str(part) for part in parts)])
    return hashlib.sha256(text.encode()).hexdigest()


def _as_set(values: Iterable[str]) -> set[str]:
    return {str(value) for value in values if str(value)}


def filter_tasks(
    tasks: list[dict[str, Any]],
    *,
    repos: Iterable[str] = (),
    segments: Iterable[str] = (),
    task_types: Iterable[str] = (),
    include_tasks: Iterable[str] = (),
    exclude_tasks: Iterable[str] = (),
) -> list[dict[str, Any]]:
    repo_set = _as_set(repos)
    segment_set = _as_set(segments)
    type_set = _as_set(task_types)
    include_set = _as_set(include_tasks)
    exclude_set = _as_set(exclude_tasks)
    selected = []
    for task in tasks:
        task_id = _task_id(task)
        if not task_id or task_id in exclude_set:
            continue
        if include_set and task_id not in include_set:
            continue
        if repo_set and str(task.get("repo")) not in repo_set:
            continue
        if segment_set and str(task.get("segment")) not in segment_set:
            continue
        if type_set and str(task.get("task_type")) not in type_set:
            continue
        selected.append(task)
    return selected


def _repo_round_robin(tasks: list[dict[str, Any]], limit: int, seed: str) -> list[dict[str, Any]]:
    groups: dict[str, list[dict[str, Any]]] = {}
    for task in tasks:
        groups.setdefault(str(task.get("repo") or "unknown"), []).append(task)
    for repo, items in groups.items():
        items.sort(key=lambda task: _stable_key(seed, repo, _task_id(task)))
    repos = sorted(groups, key=lambda repo: _stable_key(seed, "repo", repo))

    selected: list[dict[str, Any]] = []
    while len(selected) < limit and any(groups.values()):
        for repo in repos:
            if len(selected) >= limit:
                break
            items = groups[repo]
            if items:
                selected.append(items.pop(0))
    return selected


def balanced_pick(tasks: list[dict[str, Any]], limit: int, seed: str) -> list[dict[str, Any]]:
    if limit <= 0:
        return []
    if len(tasks) <= limit:
        return sorted(tasks, key=_task_id)

    buckets: dict[str, list[dict[str, Any]]] = {}
    for task in tasks:
        buckets.setdefault(str(task.get("segment") or "unknown"), []).append(task)
    segments = sorted(buckets, key=lambda segment: _stable_key(seed, "segment", segment))

    base = limit // len(segments)
    remainder = limit % len(segments)
    segment_picks: dict[str, list[dict[str, Any]]] = {}
    for index, segment in enumerate(segments):
        quota = base + (1 if index < remainder else 0)
        segment_picks[segment] = _repo_round_robin(buckets[segment], quota, f"{seed}:{segment}")

    selected: list[dict[str, Any]] = []
    while len(selected) < limit and any(segment_picks.values()):
        for segment in segments:
            if len(selected) >= limit:
                break
            picks = segment_picks[segment]
            if picks:
                selected.append(picks.pop(0))

    seen = {_task_id(task) for task in selected}
    if len(selected) < limit:
        remaining = [task for task in tasks if _task_id(task) not in seen]
        selected.extend(_repo_round_robin(remaining, limit - len(selected), f"{seed}:fill"))
    return selected[:limit]


def _preset_ids(mode: str) -> list[str]:
    path = PRESET_SUBSETS[mode]
    data = json.loads(path.read_text())
    ids = data.get("task_ids", data if isinstance(data, list) else None)
    if not isinstance(ids, list):
        raise SystemExit(f"preset subset must contain task_ids list: {path}")
    return [str(task_id) for task_id in ids]


def select_tasks(
    tasks: list[dict[str, Any]],
    *,
    mode: str,
    max_tasks: int | None = None,
    seed: str = "senior-swe-bench",
) -> list[dict[str, Any]]:
    by_id = {_task_id(task): task for task in tasks}
    if mode in PRESET_SUBSETS:
        selected = [by_id[task_id] for task_id in _preset_ids(mode) if task_id in by_id]
    elif mode == "full":
        selected = sorted(tasks, key=_task_id)
    elif mode == "overnight":
        selected = balanced_pick(tasks, max_tasks or DEFAULT_OVERNIGHT_TASKS, seed)
    else:
        raise SystemExit(f"unknown subset mode: {mode}")

    if max_tasks is not None and mode != "overnight":
        selected = selected[:max_tasks]
    return selected


def _load_all_tasks() -> tuple[dict[str, Any], list[dict[str, Any]]]:
    lock = list_tasks.load_lock()
    tasks = list_tasks.harbor_tasks(lock["dataset_repo"]) or list_tasks.clone_tasks(lock)
    return lock, tasks


def build_subset(
    *,
    mode: str,
    tasks: list[dict[str, Any]],
    dataset: dict[str, Any],
    max_tasks: int | None = None,
    seed: str = "senior-swe-bench",
    repos: Iterable[str] = (),
    segments: Iterable[str] = (),
    task_types: Iterable[str] = (),
    include_tasks: Iterable[str] = (),
    exclude_tasks: Iterable[str] = (),
    name: str | None = None,
) -> dict[str, Any]:
    filtered = filter_tasks(
        tasks,
        repos=repos,
        segments=segments,
        task_types=task_types,
        include_tasks=include_tasks,
        exclude_tasks=exclude_tasks,
    )
    selected = select_tasks(filtered, mode=mode, max_tasks=max_tasks, seed=seed)
    if not selected:
        raise SystemExit("selection produced no tasks")

    subset_name = name or f"{mode}-{len(selected)}"
    return {
        "name": subset_name,
        "description": f"Generated {mode} Senior SWE-Bench subset with {len(selected)} task(s).",
        "generated_by": "dev/benches/senior_swe_bench/make_subset.py",
        "generated_at": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "mode": mode,
        "selection": {
            "max_tasks": max_tasks,
            "seed": seed,
            "repos": sorted(_as_set(repos)),
            "segments": sorted(_as_set(segments)),
            "task_types": sorted(_as_set(task_types)),
            "include_tasks": sorted(_as_set(include_tasks)),
            "exclude_tasks": sorted(_as_set(exclude_tasks)),
        },
        "dataset": dataset,
        "task_ids": [_task_id(task) for task in selected],
        "tasks": [
            {
                "task_id": _task_id(task),
                "segment": task.get("segment"),
                "task_type": task.get("task_type"),
                "repo": task.get("repo"),
                "visibility": task.get("visibility"),
                "verifier_timeout_sec": task.get("verifier_timeout_sec"),
            }
            for task in selected
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=["smoke", "public-5", "overnight", "full"])
    parser.add_argument("--out", type=Path, help="Write subset JSON to this path. Defaults to stdout.")
    parser.add_argument("--name", help="Override generated subset name.")
    parser.add_argument("--max-tasks", type=int, help="Limit selected task count. Overnight defaults to 10.")
    parser.add_argument("--seed", default="senior-swe-bench", help="Deterministic balancing seed.")
    parser.add_argument("--repo", action="append", default=[], help="Only include this repo; repeatable.")
    parser.add_argument("--segment", action="append", default=[], help="Only include this segment; repeatable.")
    parser.add_argument("--task-type", action="append", default=[], help="Only include this task type; repeatable.")
    parser.add_argument("--include-task", action="append", default=[], help="Only include this task id; repeatable.")
    parser.add_argument("--exclude-task", action="append", default=[], help="Exclude this task id; repeatable.")
    args = parser.parse_args()

    if args.max_tasks is not None and args.max_tasks <= 0:
        raise SystemExit("--max-tasks must be positive")

    dataset, tasks = _load_all_tasks()
    subset = build_subset(
        mode=args.mode,
        tasks=tasks,
        dataset=dataset,
        max_tasks=args.max_tasks,
        seed=args.seed,
        repos=args.repo,
        segments=args.segment,
        task_types=args.task_type,
        include_tasks=args.include_task,
        exclude_tasks=args.exclude_task,
        name=args.name,
    )

    text = json.dumps(subset, indent=2, sort_keys=True) + "\n"
    if args.out:
        args.out.parent.mkdir(parents=True, exist_ok=True)
        args.out.write_text(text)
        print(f"wrote {args.out} ({len(subset['task_ids'])} tasks)")
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
