#!/usr/bin/env python3
"""Autoresearch driver for 4Clojure iteration-minimization.

For one autoresearch iteration:
  * choose 3 problems by deterministic rotation (no repeats),
  * run `bin/vis run --json` per problem in a fresh tmp workdir,
  * parse {iteration-count, tokens, cost} from the Vis JSON envelope,
  * read `solution.edn`, judge with `eval_one.clj`,
  * emit `METRIC name=value` lines.

Failed tasks are penalized at FAIL_PENALTY iterations so the optimizer
cannot improve `iter_score` by skipping work.

Environment variables (used by tests and ad-hoc reruns):
  VIS_PROVIDER             - default "zai-coding-plan"
  VIS_MODEL                - default "glm-5.1"
  VIS_BIN                  - default <repo>/bin/vis
  AUTORESEARCH_N           - tasks per iteration (default 3)
  AUTORESEARCH_TIMEOUT     - per-task wall timeout, seconds (default 120)
  AUTORESEARCH_FAIL_PENALTY- iterations attributed to a failed task (default 30)
  AUTORESEARCH_OFFSET      - override rotation offset (default = wc -l autoresearch.jsonl)
  AUTORESEARCH_OUT         - artifact directory (default ./dev/benches/4clojure/autoresearch/<ts>)

Do NOT log final answers to autoresearch.jsonl: hashes only. The judge IS
the eval_one.clj evaluator; this script never grades.
"""
from __future__ import annotations

import json
import os
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any

HERE = Path(__file__).resolve().parent
REPO_ROOT = HERE.parents[2]
PROBLEMS_JSON = HERE / "problems.json"
EVALUATOR = HERE / "eval_one.clj"
PROMPT_TEMPLATE = HERE / "autoresearch_prompt.md"
JSONL = REPO_ROOT / "autoresearch.jsonl"

VIS_BIN = os.environ.get("VIS_BIN", str(REPO_ROOT / "bin" / "vis"))
VIS_PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
VIS_MODEL = os.environ.get("VIS_MODEL", "glm-5.1")
CLOJURE = os.environ.get("CLOJURE", "clojure")
N_TASKS = int(os.environ.get("AUTORESEARCH_N", "3"))
TIMEOUT = int(os.environ.get("AUTORESEARCH_TIMEOUT", "120"))
FAIL_PENALTY = int(os.environ.get("AUTORESEARCH_FAIL_PENALTY", "30"))
EVAL_TIMEOUT = int(os.environ.get("AUTORESEARCH_EVAL_TIMEOUT", "20"))
SENTINEL = "__VIS_4CLOJURE_SOLUTION__"


def log(msg: str) -> None:
    print(msg, file=sys.stderr, flush=True)


def autoresearch_offset() -> int:
    raw = os.environ.get("AUTORESEARCH_OFFSET")
    if raw:
        return int(raw)
    if JSONL.exists():
        with JSONL.open() as f:
            return sum(1 for line in f if line.strip())
    return 0


def pick_problems(problems: list[dict], offset: int, n: int) -> list[dict]:
    if not problems:
        raise RuntimeError("problems.json is empty")
    return [problems[(offset * n + i) % len(problems)] for i in range(n)]


def render_problem(problem: dict) -> str:
    lines = [
        f"# 4Clojure problem {problem['id']}: {problem['title']}",
        "",
        f"Difficulty: {problem.get('difficulty', 'Unknown')}",
        "",
        "## Description",
        problem.get("description", ""),
        "",
        "## Tests",
    ]
    lines.extend(f"```clojure\n{t}\n```" for t in problem.get("tests", []))
    if problem.get("restricted"):
        lines.extend([
            "",
            "## Restricted symbols",
            ", ".join(map(str, problem["restricted"])),
        ])
    if problem.get("extra_requires"):
        lines.extend([
            "",
            "## Extra requires",
            ", ".join(map(str, problem["extra_requires"])),
        ])
    return "\n".join(lines) + "\n"


def render_prompt(problem: dict, solution_path: Path) -> str:
    template = PROMPT_TEMPLATE.read_text()
    return template.format(
        problem=render_problem(problem),
        solution_path=str(solution_path),
        sentinel=SENTINEL,
    )


def safe_get(d: dict | None, *path, default=None):
    cur: Any = d
    for k in path:
        if not isinstance(cur, dict):
            return default
        cur = cur.get(k)
        if cur is None:
            return default
    return cur if cur is not None else default


def judge(problem: dict, solution: str, workdir: Path) -> dict:
    p_path = workdir / "judge_problem.json"
    s_path = workdir / "judge_solution.edn"
    p_path.write_text(json.dumps(problem, ensure_ascii=False))
    s_path.write_text(solution)
    try:
        proc = subprocess.run(
            [CLOJURE, "-M", str(EVALUATOR), str(p_path), str(s_path)],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            timeout=EVAL_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return {"passed": False, "judge_error": f"timeout after {EVAL_TIMEOUT}s"}
    if proc.returncode != 0:
        return {
            "passed": False,
            "judge_error": f"evaluator-exit-{proc.returncode}: {proc.stderr[-300:]}",
        }
    try:
        return json.loads(proc.stdout)
    except Exception as e:
        return {
            "passed": False,
            "judge_error": f"bad-evaluator-json: {e}: {proc.stdout[-300:]}",
        }


def run_one(problem: dict, out_dir: Path) -> dict:
    workdir = Path(tempfile.mkdtemp(prefix=f"4clojure-{problem['id']}-"))
    started = time.time()
    raw_envelope: dict | None = None
    rc = None
    stdout = ""
    stderr = ""
    timed_out = False
    try:
        (workdir / "problem.md").write_text(render_problem(problem))
        (workdir / "solution.edn").write_text(SENTINEL)
        prompt = render_prompt(problem, workdir / "solution.edn")
        prompt_chars = len(prompt)
        cmd = [
            VIS_BIN, "run", "--json",
            "--db", ":memory",
            "--provider", VIS_PROVIDER,
            "--model", VIS_MODEL,
            prompt,
        ]
        try:
            proc = subprocess.run(
                cmd, cwd=workdir, capture_output=True, text=True, timeout=TIMEOUT,
            )
            rc = proc.returncode
            stdout = proc.stdout
            stderr = proc.stderr
        except subprocess.TimeoutExpired as e:
            timed_out = True
            stdout = (e.stdout or b"").decode("utf-8", "replace") if isinstance(e.stdout, bytes) else (e.stdout or "")
            stderr = (e.stderr or b"").decode("utf-8", "replace") if isinstance(e.stderr, bytes) else (e.stderr or "")

        if stdout.strip():
            try:
                raw_envelope = json.loads(stdout)
            except Exception:
                # Trace stream concatenation, salvage last { ... }
                raw_envelope = None

        raw_solution = ""
        if (workdir / "solution.edn").exists():
            raw_solution = (workdir / "solution.edn").read_text().strip()
        solution = "" if raw_solution == SENTINEL else raw_solution

        verdict = judge(problem, solution, workdir) if solution else {
            "passed": False,
            "judge_error": "empty-solution",
        }

        iterations_from_envelope = safe_get(raw_envelope, "iteration-count", default=None)
        # Fail-safe: when Vis exits cleanly but never called turn-answer!, it
        # still returns iteration-count. When it timed out, we don't know.
        iterations = (
            int(iterations_from_envelope)
            if isinstance(iterations_from_envelope, (int, float))
            else None
        )

        tokens_total = safe_get(raw_envelope, "tokens", "total", default=0)
        prompt_tokens = safe_get(raw_envelope, "tokens", "input", default=0)
        output_tokens = safe_get(raw_envelope, "tokens", "output", default=0)
        reasoning_tokens = safe_get(raw_envelope, "tokens", "reasoning", default=0)
        cost_usd = safe_get(raw_envelope, "cost", "total-cost", default=0.0)

        elapsed = round(time.time() - started, 2)
        passed = bool(verdict.get("passed"))
        outcome = {
            "id": problem["id"],
            "title": problem.get("title"),
            "difficulty": problem.get("difficulty"),
            "passed": passed,
            "iterations": iterations,
            "iterations_score": (
                iterations if (passed and isinstance(iterations, int)) else FAIL_PENALTY
            ),
            "timed_out": timed_out,
            "returncode": rc,
            "tokens_total": tokens_total,
            "prompt_tokens": prompt_tokens,
            "output_tokens": output_tokens,
            "reasoning_tokens": reasoning_tokens,
            "cost_usd": float(cost_usd) if isinstance(cost_usd, (int, float)) else 0.0,
            "prompt_chars": prompt_chars,
            "solution": solution,
            "verdict": verdict,
            "elapsed_s": elapsed,
        }

        # Persist artifacts per task.
        task_dir = out_dir / f"task-{problem['id']:03d}"
        task_dir.mkdir(parents=True, exist_ok=True)
        (task_dir / "prompt.txt").write_text(prompt)
        (task_dir / "vis-stdout.json").write_text(stdout)
        (task_dir / "vis-stderr.log").write_text(stderr)
        (task_dir / "outcome.json").write_text(
            json.dumps(outcome, indent=2, ensure_ascii=False) + "\n"
        )
        return outcome
    finally:
        shutil.rmtree(workdir, ignore_errors=True)


def aggregate(outcomes: list[dict]) -> dict:
    pass_count = sum(1 for o in outcomes if o["passed"])
    iter_score = sum(o["iterations_score"] for o in outcomes)
    iters_pass = [o["iterations"] for o in outcomes if o["passed"] and isinstance(o["iterations"], int)]
    cost_total = round(sum(o["cost_usd"] for o in outcomes), 6)
    tokens_total = sum(int(o["tokens_total"] or 0) for o in outcomes)
    prompt_chars = [o["prompt_chars"] for o in outcomes]
    return {
        "task_count": len(outcomes),
        "pass_count": pass_count,
        "pass_rate": round(100.0 * pass_count / len(outcomes), 2) if outcomes else 0.0,
        "iter_score": iter_score,
        "mean_iterations_pass": (
            round(statistics.mean(iters_pass), 2) if iters_pass else None
        ),
        "total_cost_usd": cost_total,
        "total_tokens": tokens_total,
        "mean_prompt_chars": int(statistics.mean(prompt_chars)) if prompt_chars else 0,
        "wall_seconds": round(sum(o["elapsed_s"] for o in outcomes), 2),
    }


def main() -> int:
    problems = json.loads(PROBLEMS_JSON.read_text())
    offset = autoresearch_offset()
    chosen = pick_problems(problems, offset, N_TASKS)

    out_root = Path(
        os.environ.get(
            "AUTORESEARCH_OUT",
            str(HERE / "autoresearch" / time.strftime("%Y%m%d-%H%M%S")),
        )
    )
    out_root.mkdir(parents=True, exist_ok=True)
    (out_root / "chosen.json").write_text(
        json.dumps(
            [{"id": p["id"], "title": p["title"], "difficulty": p["difficulty"]} for p in chosen],
            indent=2,
            ensure_ascii=False,
        ) + "\n"
    )
    log(f"[autoresearch] offset={offset} tasks={[p['id'] for p in chosen]} out={out_root}")

    outcomes: list[dict] = []
    for problem in chosen:
        log(f"[autoresearch] running task {problem['id']} ({problem.get('difficulty')}): {problem.get('title')}")
        o = run_one(problem, out_root)
        verdict_str = "PASS" if o["passed"] else "FAIL"
        iters_str = o["iterations"] if o["iterations"] is not None else "?"
        log(
            f"  -> {verdict_str}  iters={iters_str}  "
            f"score={o['iterations_score']}  "
            f"cost=${o['cost_usd']:.4f}  "
            f"tokens={o['tokens_total']}  "
            f"prompt_chars={o['prompt_chars']}  "
            f"elapsed={o['elapsed_s']}s"
        )
        outcomes.append(o)

    summary = aggregate(outcomes)
    summary["chosen"] = [
        {
            "id": o["id"],
            "passed": o["passed"],
            "iterations": o["iterations"],
            "iterations_score": o["iterations_score"],
            "cost_usd": o["cost_usd"],
        }
        for o in outcomes
    ]
    (out_root / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")

    metric_keys = [
        "iter_score",
        "pass_count",
        "pass_rate",
        "mean_iterations_pass",
        "total_cost_usd",
        "total_tokens",
        "mean_prompt_chars",
        "wall_seconds",
    ]
    for k in metric_keys:
        v = summary.get(k)
        if v is None:
            continue
        print(f"METRIC {k}={v}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
