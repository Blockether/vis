#!/usr/bin/env python3
"""Materialize a deterministic difficulty-stratified 4Clojure subset."""
from __future__ import annotations

import argparse
import json
import random
import sys
from collections import defaultdict
from pathlib import Path


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--problems", type=Path, default=Path(__file__).with_name("problems.json"))
    p.add_argument("--n", type=int, required=True)
    p.add_argument("--seed", type=int, default=42)
    p.add_argument("--out", type=Path, required=True)
    args = p.parse_args()

    problems = json.loads(args.problems.read_text())
    rng = random.Random(args.seed)
    by_diff: dict[str, list[dict]] = defaultdict(list)
    for problem in problems:
        by_diff[problem.get("difficulty") or "Unknown"].append(problem)

    total = len(problems)
    selected: list[dict] = []
    for _difficulty, items in sorted(by_diff.items()):
        items = sorted(items, key=lambda p: p["id"])
        rng.shuffle(items)
        share = max(1, round(len(items) / total * args.n))
        selected.extend(items[:share])

    if len(selected) > args.n:
        rng.shuffle(selected)
        selected = selected[: args.n]
    elif len(selected) < args.n:
        selected_ids = {p["id"] for p in selected}
        remaining = [p for p in sorted(problems, key=lambda p: p["id"]) if p["id"] not in selected_ids]
        rng.shuffle(remaining)
        selected.extend(remaining[: args.n - len(selected)])

    selected.sort(key=lambda p: p["id"])
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(selected, indent=2, ensure_ascii=False) + "\n")
    print(f"wrote {len(selected)} problems → {args.out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
