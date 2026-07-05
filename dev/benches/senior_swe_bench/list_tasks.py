#!/usr/bin/env python3
"""List Senior SWE-Bench tasks from Harbor or a pinned shallow clone."""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:  # pragma: no cover - local macOS may still ship 3.9
    tomllib = None  # type: ignore[assignment]
import re
from pathlib import Path
from typing import Any

HERE = Path(__file__).resolve().parent
LOCK = HERE / "dataset.lock.json"
CACHE_ROOT = Path(os.environ.get("VIS_SSB_CACHE", Path.home() / ".cache" / "vis" / "senior-swe-bench"))


def load_lock() -> dict[str, Any]:
    return json.loads(LOCK.read_text())


def run(cmd: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, text=True, capture_output=True, **kwargs)


def harbor_tasks(repo: str) -> list[dict[str, Any]] | None:
    """Best-effort Harbor listing hook; returns None when Harbor lacks a clean API."""
    harbor = shutil.which("harbor")
    if not harbor:
        return None
    # Harbor CLI has changed while Senior SWE-Bench is evolving. Try only cheap,
    # read-only JSON-ish commands; fall back to the pinned git clone on any miss.
    candidates = [
        [harbor, "tasks", "list", "--repo", repo, "--json"],
        [harbor, "dataset", "tasks", "--repo", repo, "--json"],
    ]
    for cmd in candidates:
        proc = run(cmd, check=False)
        if proc.returncode != 0 or not proc.stdout.strip():
            continue
        try:
            data = json.loads(proc.stdout)
        except json.JSONDecodeError:
            continue
        items = data.get("tasks") if isinstance(data, dict) else data
        if isinstance(items, list):
            return [normalize_task(x) for x in items if isinstance(x, dict)]
    return None


def ensure_dataset_clone(lock: dict[str, Any]) -> Path:
    repo = lock["dataset_repo"]
    commit = lock["dataset_commit"]
    dest = CACHE_ROOT / repo.replace("/", "__") / commit
    if (dest / ".git").exists():
        return dest
    dest.parent.mkdir(parents=True, exist_ok=True)
    tmp = dest.with_suffix(".tmp")
    if tmp.exists():
        shutil.rmtree(tmp)
    url = f"https://github.com/{repo}.git"
    subprocess.run(["git", "clone", "--depth", "1", url, str(tmp)], check=True)
    subprocess.run(["git", "-C", str(tmp), "fetch", "--depth", "1", "origin", commit], check=True)
    subprocess.run(["git", "-C", str(tmp), "checkout", "--detach", commit], check=True)
    if dest.exists():
        shutil.rmtree(dest)
    tmp.rename(dest)
    return dest


def normalize_task(raw: dict[str, Any]) -> dict[str, Any]:
    metadata = raw.get("metadata") or {}
    taxonomy = metadata.get("taxonomy") or {}
    origin = metadata.get("origin") or {}
    task_id = raw.get("id") or raw.get("task_id") or raw.get("name")
    return {
        "task_id": str(task_id),
        "segment": metadata.get("segment") or raw.get("segment"),
        "task_type": taxonomy.get("task_type") or raw.get("task_type"),
        "repo": metadata.get("repo") or origin.get("repo") or raw.get("repo"),
        "visibility": metadata.get("visibility") or raw.get("visibility"),
        "tags": metadata.get("tags") or raw.get("tags") or [],
        "verifier_timeout_sec": (((raw.get("verifier") or {}).get("timeout_sec"))),
        "source": raw.get("source"),
    }


def _mini_toml_metadata(text: str) -> dict[str, Any]:
    """Parse just the task.toml fields this lister needs when tomllib is absent."""
    section = ""
    out: dict[str, Any] = {"metadata": {"taxonomy": {}}, "verifier": {}}
    for raw in text.splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("[") and line.endswith("]"):
            section = line.strip("[]")
            continue
        m = re.match(r"([A-Za-z0-9_]+)\s*=\s*(.+)", line)
        if not m:
            continue
        key, value = m.groups()
        value = value.strip()
        if value.startswith('"') and value.endswith('"'):
            parsed: Any = value[1:-1]
        elif value.startswith("[") and value.endswith("]"):
            parsed = re.findall(r'"([^"]+)"', value)
        else:
            try:
                parsed = float(value)
            except ValueError:
                parsed = value
        if section == "metadata":
            out["metadata"][key] = parsed
        elif section == "metadata.taxonomy":
            out["metadata"]["taxonomy"][key] = parsed
        elif section == "verifier":
            out["verifier"][key] = parsed
    return out


def task_from_dir(path: Path) -> dict[str, Any]:
    text = (path / "task.toml").read_text()
    task_toml = tomllib.loads(text) if tomllib is not None else _mini_toml_metadata(text)
    metadata = task_toml.get("metadata", {})
    taxonomy = metadata.get("taxonomy", {})
    verifier = task_toml.get("verifier", {})
    return {
        "task_id": path.name,
        "segment": metadata.get("segment"),
        "task_type": taxonomy.get("task_type"),
        "repo": metadata.get("repo"),
        "visibility": metadata.get("visibility"),
        "tags": metadata.get("tags", []),
        "verifier_timeout_sec": verifier.get("timeout_sec"),
        "source": str(path),
    }


def clone_tasks(lock: dict[str, Any]) -> list[dict[str, Any]]:
    root = ensure_dataset_clone(lock) / "tasks"
    if not root.exists():
        raise SystemExit(f"dataset clone has no tasks/ directory: {root}")
    return [task_from_dir(d) for d in sorted(root.iterdir()) if (d / "task.toml").exists()]


def load_subset(path: str | None) -> set[str] | None:
    if not path:
        return None
    p = Path(path)
    if not p.is_absolute():
        cwd_relative = Path.cwd() / p
        package_relative = HERE / p
        if cwd_relative.exists():
            p = cwd_relative
        else:
            p = package_relative
    data = json.loads(p.read_text())
    ids = data.get("task_ids", data if isinstance(data, list) else None)
    if not isinstance(ids, list):
        raise SystemExit(f"subset file must contain task_ids list: {p}")
    return {str(x) for x in ids}


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--subset", help="Subset JSON path relative to this package, e.g. subsets/smoke.json")
    ap.add_argument("--json", action="store_true", help="Emit JSON instead of a table")
    ap.add_argument("--refresh", action="store_true", help="Delete cached clone before listing")
    args = ap.parse_args()

    lock = load_lock()
    if args.refresh:
        shutil.rmtree(CACHE_ROOT / lock["dataset_repo"].replace("/", "__") / lock["dataset_commit"], ignore_errors=True)

    tasks = harbor_tasks(lock["dataset_repo"]) or clone_tasks(lock)
    subset = load_subset(args.subset)
    if subset is not None:
        tasks = [t for t in tasks if t["task_id"] in subset]
        missing = subset - {t["task_id"] for t in tasks}
        if missing:
            raise SystemExit(f"subset references unknown tasks: {sorted(missing)}")

    if args.json:
        json.dump({"dataset": lock, "tasks": tasks}, sys.stdout, indent=2)
        sys.stdout.write("\n")
    else:
        for t in tasks:
            print(f"{t['task_id']}\t{t.get('segment') or '-'}\t{t.get('task_type') or '-'}\t{t.get('repo') or '-'}\t{t.get('visibility') or '-'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
