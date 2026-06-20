#!/usr/bin/env python3
"""Vis sanity harness runner.

Runs each task in tasks.json against a model through `bin/vis`, grades the
output, and — on failure — dissects the session DB iteration-by-iteration so
you can see exactly what code the model wrote and what the sandbox returned.

Usage:
    dev/benches/sanity/run.py <provider/model> [--task ID] [--keep] [--repeat N]

Examples:
    dev/benches/sanity/run.py zai-anthropic/glm-5-turbo
    dev/benches/sanity/run.py openai-codex/gpt-5.5 --task python-compute --repeat 3

Grades: PASS (completed, expectations met, no forbidden phrase),
        SOFT (completed but an expectation was not matched),
        FAIL (timed out / crashed, or emitted a forbidden phrase such as the
              "sandbox is non-functional" misdiagnosis or a leaked unawaited
              tool-call placeholder).
Exit code is the number of non-PASS tasks (0 = all green).
"""
import argparse
import json
import os
import re
import shutil
import sqlite3
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
VIS = REPO / "bin" / "vis"
TIMEOUT = int(os.environ.get("SANITY_TIMEOUT", "300"))


def load_tasks():
    spec = json.loads((HERE / "tasks.json").read_text())
    return spec["tasks"], spec.get("forbid_global", [])


def run_task(model, task, db_dir):
    # The run path redirects the session DB via the `--db <dir>` flag (NOT
    # VIS_DB_PATH, which only the `sessions`/`doctor` subcommands honour). vis
    # creates `<dir>/vis.db` inside it, so the autopsy reads that file.
    t0 = time.time()
    try:
        p = subprocess.run(
            [str(VIS), "--db", str(db_dir), "--model", model, task["prompt"]],
            cwd=str(REPO), capture_output=True, text=True,
            timeout=TIMEOUT,
        )
        out, code, timed = p.stdout + p.stderr, p.returncode, False
    except subprocess.TimeoutExpired as e:
        out = (e.stdout or "") + (e.stderr or "") if isinstance(e.stdout, str) else ""
        code, timed = -1, True
    return {"out": out, "code": code, "timed_out": timed, "secs": round(time.time() - t0, 1)}


def grade(task, run, forbid_global):
    out = run["out"]
    low = out.lower()
    forbidden = [f for f in (forbid_global + task.get("forbid", [])) if f.lower() in low]
    if run["timed_out"] or run["code"] != 0:
        return "FAIL", forbidden, ["did not complete (timeout/crash)"]
    if forbidden:
        return "FAIL", forbidden, ["forbidden phrase emitted"]
    misses = []
    for s in task.get("expect_all", []):
        if s.lower() not in low:
            misses.append(f"missing required: {s!r}")
    any_set = task.get("expect_any", [])
    if any_set and not any(s.lower() in low for s in any_set):
        misses.append(f"none of: {any_set}")
    rx = task.get("expect_regex")
    if rx and not re.search(rx, out):
        misses.append(f"regex no match: {rx}")
    return ("PASS" if not misses else "SOFT"), forbidden, misses


def db_stats(db_path):
    """Iteration count, error count, total cost from the per-task session DB."""
    try:
        con = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
        rows = con.execute(
            "SELECT position, status, code, forms, llm_assistant_message, cost_usd "
            "FROM session_turn_iteration ORDER BY position"
        ).fetchall()
        con.close()
    except Exception as e:
        return {"iters": 0, "errors": 0, "cost": 0.0, "rows": [], "db_err": str(e)}
    cost = sum((r[5] or 0) for r in rows)
    errs = sum(1 for r in rows if r[1] == "error")
    return {"iters": len(rows), "errors": errs, "cost": round(cost, 4), "rows": rows}


def _s(v):
    """Coerce a sqlite cell (TEXT or BLOB/bytes) to str."""
    if v is None:
        return ""
    if isinstance(v, (bytes, bytearray)):
        return bytes(v).decode("utf-8", "replace")
    return str(v)


def dissect(stats):
    """Print every iteration's code + sandbox result — the failure autopsy."""
    for pos, status, code, forms, msg, _cost in stats["rows"]:
        code, forms, msg = _s(code), _s(forms), _s(msg)
        print(f"    ── iter {pos} [{status}] ──")
        if code.strip():
            for ln in code.strip().splitlines():
                print(f"      | {ln}")
        if forms.strip():
            print(f"      → forms: {forms if len(forms) < 600 else forms[:600] + ' …'}")
        if msg.strip() and not code.strip():
            m = msg.strip()
            print(f"      → answer: {m if len(m) < 300 else m[:300] + ' …'}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("model", help="provider/model spec, e.g. zai-anthropic/glm-5-turbo")
    ap.add_argument("--task", help="run only this task id")
    ap.add_argument("--repeat", type=int, default=1, help="run each task N times")
    ap.add_argument("--keep", action="store_true", help="keep result DBs (default: keep)")
    ap.add_argument("--results", default=None, help="results dir (default: /tmp/vis-sanity)")
    args = ap.parse_args()

    tasks, forbid_global = load_tasks()
    if args.task:
        tasks = [t for t in tasks if t["id"] == args.task]
        if not tasks:
            sys.exit(f"no such task: {args.task}")

    results_dir = Path(args.results or "/tmp/vis-sanity")
    results_dir.mkdir(parents=True, exist_ok=True)
    print(f"# vis sanity harness — model={args.model} timeout={TIMEOUT}s results={results_dir}\n")

    summary, bad = [], 0
    for task in tasks:
        for n in range(1, args.repeat + 1):
            tag = task["id"] + (f"#{n}" if args.repeat > 1 else "")
            db_dir = results_dir / tag.replace("#", "_")
            if db_dir.exists():
                shutil.rmtree(db_dir, ignore_errors=True)
            db_dir.mkdir(parents=True, exist_ok=True)
            print(f"## {tag}", flush=True)
            run = run_task(args.model, task, db_dir)
            verdict, forbidden, misses = grade(task, run, forbid_global)
            stats = db_stats(db_dir / "vis.db")
            mark = {"PASS": "✓", "SOFT": "~", "FAIL": "✗"}[verdict]
            print(f"   {mark} {verdict}  {run['secs']}s  iters={stats['iters']} "
                  f"errs={stats['errors']} cost=${stats['cost']}")
            for m in misses:
                print(f"     - {m}")
            if forbidden:
                print(f"     - FORBIDDEN: {forbidden}")
            if verdict != "PASS":
                bad += 1
                try:
                    dissect(stats)
                except Exception as e:
                    print(f"     (autopsy failed: {e})")
            summary.append({"task": tag, "verdict": verdict, "secs": run["secs"],
                            "iters": stats["iters"], "errors": stats["errors"],
                            "cost": stats["cost"], "misses": misses, "forbidden": forbidden})
            cleanup = task.get("cleanup")
            if cleanup:
                (REPO / cleanup).unlink(missing_ok=True)
            print()

    (results_dir / "results.json").write_text(json.dumps(summary, indent=2))
    p = sum(1 for s in summary if s["verdict"] == "PASS")
    soft = sum(1 for s in summary if s["verdict"] == "SOFT")
    f = sum(1 for s in summary if s["verdict"] == "FAIL")
    tot = sum(s["cost"] for s in summary)
    print(f"# SUMMARY: {p} PASS / {soft} SOFT / {f} FAIL  (total ${round(tot, 4)})  → {results_dir}/results.json")
    sys.exit(bad)


if __name__ == "__main__":
    main()
