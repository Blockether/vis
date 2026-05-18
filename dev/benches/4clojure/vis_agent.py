#!/usr/bin/env python3
"""Solve one 4Clojure problem with Vis and evaluate it."""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any

HERE = Path(__file__).resolve().parent
REPO_ROOT = HERE.parents[2]
PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
MODEL = os.environ.get("VIS_MODEL", "glm-5.1")
TIMEOUT = int(os.environ.get("VIS_INSTANCE_TIMEOUT", "300"))
EVAL_TIMEOUT = int(os.environ.get("FOURCLOJURE_EVAL_TIMEOUT", "20"))
TRACE_DIR = os.environ.get("FOURCLOJURE_TRACE_DIR")
VIS_BIN = os.environ.get("VIS_BIN", str(REPO_ROOT / "bin" / "vis"))
CLOJURE = os.environ.get("CLOJURE", "clojure")
MODEL_NAME = f"vis+{PROVIDER}/{MODEL}"
SOLUTION_SENTINEL = "__VIS_4CLOJURE_SOLUTION__"


def log(msg: str) -> None:
    print(f"[4clojure_agent] {msg}", file=sys.stderr, flush=True)


def render_problem(problem: dict[str, Any]) -> str:
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
    lines.extend(f"```clojure\n{test}\n```" for test in problem.get("tests", []))
    if problem.get("restricted"):
        lines.extend(["", "## Restricted symbols", ", ".join(map(str, problem["restricted"]))])
    if problem.get("extra_requires"):
        lines.extend(["", "## Extra requires", ", ".join(map(str, problem["extra_requires"]))])
    return "\n".join(lines) + "\n"


def render_prompt(problem: dict[str, Any], workdir: Path) -> str:
    return f"""You are solving a 4Clojure exercise.

Write the answer to {workdir / 'solution.edn'} as the exact Clojure source fragment
that should replace every `__` placeholder in the problem tests.
The file currently contains the sentinel `{SOLUTION_SENTINEL}`.
Use this exact write form, changing only the replacement string:

```clojure
(v/patch [{{:path "solution.edn" :search "{SOLUTION_SENTINEL}" :replace "YOUR_ANSWER"}}])
```

Do not use `slurp`, `spit`, shell commands, Markdown fences in the file, prose,
comments, `(def ...)`, or test code. Some blanks require multiple forms, for
example `:a :b :c` in `(list __)`. Use only Clojure source that is valid after
direct textual substitution. Respect any restricted-symbol list in the problem.

Problem:

{render_problem(problem)}

When done, stop. The benchmark will evaluate solution.edn automatically.
"""


def result_base(problem: dict[str, Any]) -> dict[str, Any]:
    return {
        "id": problem["id"],
        "title": problem.get("title"),
        "difficulty": problem.get("difficulty"),
        "model_name_or_path": MODEL_NAME,
        "solution": "",
        "empty": True,
    }


def extract_usage(stdout: str) -> tuple[int | None, float | None]:
    try:
        payload = json.loads(stdout)
        usage = payload.get("usage", {}) or {}
        token_payload = payload.get("tokens") or {}
        cost_payload = payload.get("cost") or {}
        tokens = usage.get("total_tokens") or token_payload.get("total")
        cost = usage.get("cost_usd") or cost_payload.get("total-cost")
        return tokens, cost
    except Exception:
        return None, None


def write_trace(problem: dict[str, Any], trace: dict[str, Any]) -> None:
    if not TRACE_DIR:
        return
    path = Path(TRACE_DIR)
    path.mkdir(parents=True, exist_ok=True)
    trace_path = path / f"{int(problem['id']):03d}.json"
    trace_path.write_text(json.dumps(trace, indent=2, ensure_ascii=False) + "\n")


def evaluate(problem: dict[str, Any], solution: str, workdir: Path) -> dict[str, Any]:
    if os.environ.get("FOURCLOJURE_EVALUATE", "1") == "0":
        return {}
    problem_path = workdir / "problem.json"
    solution_path = workdir / "solution.edn"
    problem_path.write_text(json.dumps(problem, ensure_ascii=False))
    solution_path.write_text(solution)
    try:
        proc = subprocess.run(
            [CLOJURE, "-M", str(HERE / "eval_one.clj"), str(problem_path), str(solution_path)],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            timeout=EVAL_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return {"passed": False, "eval_error": f"timeout after {EVAL_TIMEOUT}s"}
    if proc.returncode != 0:
        return {"passed": False, "eval_error": f"evaluator-exit-{proc.returncode}: {proc.stderr[-400:]}"}
    try:
        data = json.loads(proc.stdout)
    except Exception as e:
        return {"passed": False, "eval_error": f"bad-evaluator-json: {e}: {proc.stdout[-400:]}"}
    if data.get("error"):
        data["eval_error"] = data.pop("error")
    return data


def solve(problem: dict[str, Any]) -> dict[str, Any]:
    started = time.time()
    workdir = Path(tempfile.mkdtemp(prefix="4clojure-vis-"))
    trace: dict[str, Any] = {"problem": problem, "model": MODEL_NAME}
    try:
        problem_md = render_problem(problem)
        prompt = render_prompt(problem, workdir)
        trace.update({"problem_md": problem_md, "prompt": prompt})
        (workdir / "problem.md").write_text(problem_md)
        (workdir / "solution.edn").write_text(SOLUTION_SENTINEL)
        if os.environ.get("FOURCLOJURE_SOLVE", "1") == "0":
            result = {**result_base(problem), **evaluate(problem, "", workdir), "elapsed_s": 0.0}
            trace["result"] = result
            write_trace(problem, trace)
            return result

        cmd = [
            VIS_BIN,
            "--json",
            "--db",
            ":memory",
            "--provider",
            PROVIDER,
            "--model",
            MODEL,
            prompt,
        ]
        trace["command"] = cmd
        log(f"{problem['id']}: launching vis ({MODEL})")
        try:
            proc = subprocess.run(cmd, cwd=workdir, capture_output=True, text=True, timeout=TIMEOUT)
        except subprocess.TimeoutExpired:
            result = {
                **result_base(problem),
                "error": f"timeout after {TIMEOUT}s",
                "elapsed_s": TIMEOUT,
            }
            trace["result"] = result
            write_trace(problem, trace)
            return result

        elapsed = time.time() - started
        trace.update(
            {
                "vis_returncode": proc.returncode,
                "vis_stdout": proc.stdout,
                "vis_stderr": proc.stderr,
            }
        )
        if proc.returncode != 0:
            result = {
                **result_base(problem),
                "error": f"vis-exit-{proc.returncode}: {proc.stderr[-400:]}",
                "elapsed_s": round(elapsed, 1),
            }
            trace["result"] = result
            write_trace(problem, trace)
            return result

        raw_solution = (workdir / "solution.edn").read_text().strip()
        solution = "" if raw_solution == SOLUTION_SENTINEL else raw_solution
        tokens, cost = extract_usage(proc.stdout)
        result = {
            **result_base(problem),
            "solution": solution,
            "empty": solution == "",
            "elapsed_s": round(elapsed, 1),
            "tokens": tokens,
            "cost_usd": cost,
            **evaluate(problem, solution, workdir),
        }
        trace.update({"solution": solution, "result": result})
        write_trace(problem, trace)
        return result
    finally:
        shutil.rmtree(workdir, ignore_errors=True)


def main() -> int:
    problem = json.load(sys.stdin)
    json.dump(solve(problem), sys.stdout, ensure_ascii=False)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
