#!/usr/bin/env python3
"""Collapse 4Clojure predictions/evaluations → summary.json + METRIC lines."""
from __future__ import annotations

import argparse
import json
import statistics
from pathlib import Path


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--predictions", type=Path, required=True)
    p.add_argument("--total", type=int, required=True)
    p.add_argument("--out", type=Path, required=True)
    args = p.parse_args()

    preds = [json.loads(line) for line in args.predictions.read_text().splitlines() if line.strip()]
    passed_n = sum(1 for r in preds if r.get("passed") is True)
    empty_n = sum(1 for r in preds if r.get("empty"))
    err_n = sum(1 for r in preds if r.get("error") or r.get("eval_error"))
    times = [r["elapsed_s"] for r in preds if isinstance(r.get("elapsed_s"), (int, float))]
    tokens = [r["tokens"] for r in preds if isinstance(r.get("tokens"), (int, float))]
    costs = [r["cost_usd"] for r in preds if isinstance(r.get("cost_usd"), (int, float))]

    by_difficulty: dict[str, dict[str, int]] = {}
    for r in preds:
        diff = r.get("difficulty") or "Unknown"
        bucket = by_difficulty.setdefault(diff, {"run": 0, "passed": 0})
        bucket["run"] += 1
        if r.get("passed") is True:
            bucket["passed"] += 1

    error_categories: dict[str, int] = {}
    for r in preds:
        err = r.get("error") or r.get("eval_error")
        if not err:
            continue
        tag = str(err).split(":", 1)[0]
        error_categories[tag] = error_categories.get(tag, 0) + 1

    pass_pct = 100.0 * passed_n / args.total if args.total else 0.0
    summary = {
        "instances_total": args.total,
        "instances_run": len(preds),
        "passed": passed_n,
        "pass_pct": round(pass_pct, 2),
        "empty_solutions": empty_n,
        "errors": err_n,
        "error_categories": error_categories,
        "by_difficulty": by_difficulty,
        "median_seconds": round(statistics.median(times), 1) if times else None,
        "p95_seconds": round(statistics.quantiles(times, n=20)[-1], 1) if len(times) >= 20 else None,
        "tokens_total": sum(tokens) if tokens else None,
        "cost_usd": round(sum(costs), 4) if costs else None,
    }
    args.out.write_text(json.dumps(summary, indent=2) + "\n")

    for key in ("pass_pct", "passed", "instances_run", "empty_solutions", "errors"):
        print(f"METRIC {key}={summary[key]}")
    for key in ("median_seconds", "p95_seconds", "tokens_total", "cost_usd"):
        if summary[key] is not None:
            print(f"METRIC {key}={summary[key]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
