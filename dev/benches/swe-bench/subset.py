#!/usr/bin/env python3
"""Materialize a deterministic stratified subset of SWE-bench Lite.

Stratification is per-repo so the subset preserves the repo distribution
of the full split. Commit the resulting JSON; the autoresearch loop treats
it as a frozen workload.
"""
from __future__ import annotations

import argparse
import json
import random
import sys
from collections import defaultdict
from pathlib import Path


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--split", choices=["lite", "verified"], default="lite")
    p.add_argument("--n", type=int, required=True, help="Subset size")
    p.add_argument("--seed", type=int, default=42)
    p.add_argument("--out", type=Path, required=True)
    args = p.parse_args()

    from datasets import load_dataset  # type: ignore

    ds_name = {
        "lite": "princeton-nlp/SWE-bench_Lite",
        "verified": "princeton-nlp/SWE-bench_Verified",
    }[args.split]
    ds = load_dataset(ds_name, split="test")
    rng = random.Random(args.seed)

    by_repo: dict[str, list[dict]] = defaultdict(list)
    for row in ds:
        by_repo[row["repo"]].append(dict(row))
    for items in by_repo.values():
        items.sort(key=lambda r: r["instance_id"])
        rng.shuffle(items)

    total = sum(len(v) for v in by_repo.values())
    selected: list[dict] = []
    for repo, items in sorted(by_repo.items()):
        share = max(1, round(len(items) / total * args.n))
        selected.extend(items[:share])
    # Trim or pad to exactly n (deterministic).
    selected.sort(key=lambda r: r["instance_id"])
    if len(selected) > args.n:
        rng.shuffle(selected)
        selected = selected[: args.n]
        selected.sort(key=lambda r: r["instance_id"])

    payload = [
        {
            "instance_id": r["instance_id"],
            "repo": r["repo"],
            "base_commit": r["base_commit"],
            "problem_statement": r["problem_statement"],
            "version": r.get("version"),
        }
        for r in selected
    ]
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(payload, indent=2))
    print(f"wrote {len(payload)} instances → {args.out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
