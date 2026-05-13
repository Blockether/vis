#!/usr/bin/env python3
"""Collapse SWE-bench predictions + harness report → summary.json."""
from __future__ import annotations

import argparse
import json
import statistics
from pathlib import Path


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--predictions", type=Path, required=True)
    p.add_argument("--report-dir", type=Path, required=True)
    p.add_argument("--run-id", required=True)
    p.add_argument("--total", type=int, required=True)
    p.add_argument("--out", type=Path, required=True)
    a = p.parse_args()

    preds = [json.loads(l) for l in a.predictions.read_text().splitlines() if l.strip()]
    by_id = {r["instance_id"]: r for r in preds}

    # SWE-bench writes a report-<run_id>.json with resolved instance ids.
    resolved: set[str] = set()
    report_files = list(a.report_dir.glob(f"*{a.run_id}*report*.json")) + \
                   list(a.report_dir.glob("report.json"))
    for f in report_files:
        try:
            data = json.loads(f.read_text())
            for k in ("resolved", "resolved_ids", "resolved_instances"):
                if k in data:
                    resolved.update(data[k])
        except Exception:
            continue

    solved_n = sum(1 for r in preds if not r.get("empty") and not r.get("error"))
    empty_n  = sum(1 for r in preds if r.get("empty"))
    err_n    = sum(1 for r in preds if r.get("error"))
    times    = [r["elapsed_s"] for r in preds if isinstance(r.get("elapsed_s"), (int, float))]
    tokens   = [r["tokens"]    for r in preds if isinstance(r.get("tokens"),    (int, float))]
    costs    = [r["cost_usd"]  for r in preds if isinstance(r.get("cost_usd"),  (int, float))]

    # Failure categorization
    err_categories: dict[str, int] = {}
    for r in preds:
        if not r.get("error"):
            continue
        tag = str(r["error"]).split(":", 1)[0]
        err_categories[tag] = err_categories.get(tag, 0) + 1

    n_attempted = len(preds)
    resolved_pct = 100.0 * len(resolved) / a.total if a.total else 0.0

    summary = {
        "run_id":          a.run_id,
        "instances_total": a.total,
        "instances_run":   n_attempted,
        "resolved":        len(resolved),
        "resolved_pct":    round(resolved_pct, 2),
        "patches_emitted": solved_n,
        "empty_patches":   empty_n,
        "errors":          err_n,
        "error_categories": err_categories,
        "median_seconds":  round(statistics.median(times), 1) if times else None,
        "p95_seconds":     round(statistics.quantiles(times, n=20)[-1], 1) if len(times) >= 20 else None,
        "tokens_total":    sum(tokens) if tokens else None,
        "cost_usd":        round(sum(costs), 4) if costs else None,
    }
    a.out.write_text(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
