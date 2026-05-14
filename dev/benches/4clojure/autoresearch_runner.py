#!/usr/bin/env python3
"""Autoresearch driver: minimize Vis iterations on a mixed Clojure workload.

Each autoresearch iteration picks N tasks from two pools by deterministic
rotation: 4Clojure (`dev/benches/4clojure/problems.json`) and filewrite
(`dev/benches/filewrite/problems.json`). With the default split 2:1 each
iter scores 2 4clojure tasks + 1 filewrite task, but the offsets advance
independently so we always see fresh problems.

For each task we run `bin/vis run --full-trace-json-stream` in a fresh
tmp workdir, parse the trace stream for `iteration-count`, `tokens`,
`cost`, and grade locally:

  * 4Clojure  → `dev/benches/4clojure/eval_one.clj` runs the public tests
  * filewrite → `clojure -Sdeps '{:paths ["<workdir>"]}' -M -e <verify>`
                expecting `:pass` on stdout

Output: `METRIC name=value` lines on stdout.

Environment overrides:
  VIS_PROVIDER             default "zai-coding-plan"
  VIS_MODEL                default "glm-5.1"
  VIS_BIN                  default <repo>/bin/vis
  AUTORESEARCH_N           total tasks per iteration (default 3)
  AUTORESEARCH_FILEWRITE_PER_ITER  filewrite slice size (default 1)
  AUTORESEARCH_TIMEOUT     per-task wall timeout, seconds (default 240)
  AUTORESEARCH_FAIL_PENALTY iter count attributed to a failed task (30)
  AUTORESEARCH_EVAL_TIMEOUT judge timeout, seconds (default 30)
  AUTORESEARCH_OFFSET      override rotation offset (default = len(jsonl))
  AUTORESEARCH_OUT         artifact dir (default ./dev/benches/4clojure/autoresearch/<ts>)
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
FOURCLOJURE_PROBLEMS = HERE / "problems.json"
FOURCLOJURE_EVALUATOR = HERE / "eval_one.clj"
FOURCLOJURE_PROMPT_TEMPLATE = HERE / "autoresearch_prompt.md"
FILEWRITE_DIR = REPO_ROOT / "dev" / "benches" / "filewrite"
FILEWRITE_PROBLEMS = FILEWRITE_DIR / "problems.json"
FILEWRITE_PROMPT_TEMPLATE = FILEWRITE_DIR / "autoresearch_prompt.md"
SYS_PROMPT_SRC = REPO_ROOT / "src" / "com" / "blockether" / "vis" / "internal" / "prompt.clj"
FOUNDATION_PROMPT_SRC = REPO_ROOT / "extensions" / "common" / "vis-foundation" / "src" / "com" / "blockether" / "vis" / "ext" / "foundation" / "core.clj"
Z_PROMPT_SRC = REPO_ROOT / "extensions" / "languages" / "clojure" / "src" / "com" / "blockether" / "vis" / "ext" / "lang_clojure" / "core.clj"
PROMPT_SIZE_CACHE = HERE / ".prompt_sizes.cache.json"
JSONL = REPO_ROOT / "autoresearch.jsonl"

VIS_BIN = os.environ.get("VIS_BIN", str(REPO_ROOT / "bin" / "vis"))
VIS_PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
VIS_MODEL = os.environ.get("VIS_MODEL", "glm-5.1")
CLOJURE = os.environ.get("CLOJURE", "clojure")
N_TASKS = int(os.environ.get("AUTORESEARCH_N", "3"))
FILEWRITE_PER_ITER = int(os.environ.get("AUTORESEARCH_FILEWRITE_PER_ITER", "1"))
TIMEOUT = int(os.environ.get("AUTORESEARCH_TIMEOUT", "240"))
FAIL_PENALTY = int(os.environ.get("AUTORESEARCH_FAIL_PENALTY", "30"))
EVAL_TIMEOUT = int(os.environ.get("AUTORESEARCH_EVAL_TIMEOUT", "30"))
ROTATE = os.environ.get("AUTORESEARCH_ROTATE", "0") == "1"
SENTINEL = "__VIS_4CLOJURE_SOLUTION__"

# Heuristic for inferring iteration count from wall-clock when neither
# the envelope nor the trace stream carries one (e.g., on timeout).
# Calibrated on this machine against glm-5.1 on zai-coding-plan:
#   `vis run --json` for a 1-iteration trivial answer took 12.5 s wall.
# Decomposed as: ~5 s JVM bootstrap + ~7 s per iteration.
WALL_BOOTSTRAP_SECONDS = float(os.environ.get("AUTORESEARCH_WALL_BOOTSTRAP", "5"))
WALL_SECONDS_PER_ITER  = float(os.environ.get("AUTORESEARCH_WALL_PER_ITER", "7"))


def estimate_iters_from_wall(elapsed_s: float) -> int:
    """Wall-time fallback iteration estimate. Clamped to >= 1."""
    return max(1, round((max(0.0, elapsed_s - WALL_BOOTSTRAP_SECONDS)) / WALL_SECONDS_PER_ITER))

# Fixed workload for the primary `./autoresearch.sh` run. Same problems
# every iteration => `iter_score` is comparable across runs and a code
# change is the only thing that can move it. Anti-overfit rotation is
# triggered by `./autoresearch.revalidate.sh` (sets AUTORESEARCH_ROTATE=1).
#
# Picked for mixed difficulty + non-trivial iteration count so the
# optimizer has visible headroom:
#   - 4Clojure #5  (Lists: conj)       - Elementary, ~12 iters at baseline
#   - 4Clojure #15 (Double Down)       - Elementary, single-form fragment
#   - filewrite fw-005 (sum a sequence)- Elementary, single-file v/patch
FIXED_WORKLOAD = [
    {"kind": "4clojure",  "id": 5},
    {"kind": "4clojure",  "id": 15},
    {"kind": "filewrite", "id": "fw-005"},
]


def log(msg: str) -> None:
    print(msg, file=sys.stderr, flush=True)


# ---------------------------------------------------------------------------
# Static prompt-size measurement (cached; one JVM call when sources change).
# ---------------------------------------------------------------------------

def _prompt_sizes_via_clojure() -> dict[str, int]:
    form = (
        "(require '[com.blockether.vis.internal.prompt :as p] "
        "         '[com.blockether.vis.ext.foundation.core :as f] "
        "         '[com.blockether.vis.ext.lang-clojure.core :as zc]) "
        "(println (count (p/build-system-prompt {}))) "
        "(println (count ((:ext/prompt f/vis-extension) {}))) "
        "(println (count ((:ext/prompt zc/clojure-extension) {})))"
    )
    proc = subprocess.run(
        [CLOJURE, "-M", "-e", form],
        cwd=REPO_ROOT, capture_output=True, text=True, timeout=180,
    )
    lines = [l.strip() for l in proc.stdout.splitlines() if l.strip()]
    nums = [int(l) for l in lines if l.lstrip("-").isdigit()][-3:]
    if len(nums) != 3:
        raise RuntimeError(f"prompt-size probe parse failed; stdout={proc.stdout[-200:]} stderr={proc.stderr[-200:]}")
    return {
        "system_prompt_chars": nums[0],
        "foundation_prompt_chars": nums[1],
        "z_prompt_chars": nums[2],
    }


def prompt_sizes() -> dict[str, int | None]:
    """Return {system_prompt_chars, foundation_prompt_chars, z_prompt_chars}.

    Cached on disk; invalidates when any of the relevant sources change.
    On probe failure: prefer last-known cached value over a sentinel so a
    transient JVM blip doesn't hide the metric. If no cache exists either,
    return None for each (the metric loop skips None entries).
    """
    fingerprint: dict[str, float] = {}
    for src in (SYS_PROMPT_SRC, FOUNDATION_PROMPT_SRC, Z_PROMPT_SRC):
        try:
            fingerprint[src.name] = src.stat().st_mtime
        except OSError:
            fingerprint[src.name] = 0.0
    cached_sizes: dict[str, int] | None = None
    if PROMPT_SIZE_CACHE.exists():
        try:
            cached = json.loads(PROMPT_SIZE_CACHE.read_text())
            if isinstance(cached.get("sizes"), dict):
                cached_sizes = {k: int(v) for k, v in cached["sizes"].items()}
                if cached.get("fingerprint") == fingerprint:
                    return dict(cached_sizes)
        except Exception:
            cached_sizes = None
    try:
        sizes = _prompt_sizes_via_clojure()
    except Exception as e:
        log(f"[autoresearch] prompt_sizes probe failed: {e}; falling back to cache={bool(cached_sizes)}")
        if cached_sizes:
            return cached_sizes
        return {"system_prompt_chars": None, "foundation_prompt_chars": None, "z_prompt_chars": None}
    try:
        PROMPT_SIZE_CACHE.write_text(
            json.dumps({"fingerprint": fingerprint, "sizes": sizes})
        )
    except OSError:
        pass
    return sizes


# ---------------------------------------------------------------------------
# Workload selection
# ---------------------------------------------------------------------------

def autoresearch_offset() -> int:
    raw = os.environ.get("AUTORESEARCH_OFFSET")
    if raw is not None:
        return int(raw)
    if JSONL.exists():
        with JSONL.open() as f:
            return sum(1 for line in f if line.strip())
    return 0


def _problem_by_id(pool: list[dict], pid) -> dict:
    """Look up a problem by `id` (4Clojure ids are ints, filewrite are strs)."""
    for p in pool:
        if p.get("id") == pid:
            return p
    raise RuntimeError(f"problem id {pid!r} not found in pool of {len(pool)} entries")


def workload_signature(workload: list[dict]) -> str:
    """Stable signature of the chosen tasks. Two runs with the same
    signature should yield comparable `iter_score` modulo LLM nondeterminism.
    """
    parts = [f"{p['kind']}:{p['id']}" for p in workload]
    return ",".join(parts)


def pick_workload(offset: int) -> list[dict]:
    """Default: return FIXED_WORKLOAD (comparable iter_score across iters).

    With AUTORESEARCH_ROTATE=1, return a deterministic rotation slice of
    `N_TASKS` problems: FILEWRITE_PER_ITER from filewrite + the rest from
    4Clojure. The two pools rotate independently. Used by
    `./autoresearch.revalidate.sh` for anti-overfit confirmation.

    Raises when the resulting workload would be empty so the optimizer
    never sees a degenerate `iter_score=0` "perfect" run.
    """
    if N_TASKS <= 0:
        raise RuntimeError(
            f"AUTORESEARCH_N must be >= 1, got {N_TASKS}; refusing degenerate iter_score=0."
        )
    fc = json.loads(FOURCLOJURE_PROBLEMS.read_text())
    fw = json.loads(FILEWRITE_PROBLEMS.read_text())
    tasks: list[dict] = []
    if not ROTATE:
        for entry in FIXED_WORKLOAD:
            pool = fc if entry["kind"] == "4clojure" else fw
            problem = dict(_problem_by_id(pool, entry["id"]))
            problem["kind"] = entry["kind"]
            tasks.append(problem)
    else:
        fw_n = max(0, min(FILEWRITE_PER_ITER, N_TASKS))
        fc_n = max(0, N_TASKS - fw_n)
        if fc_n > 0 and fc:
            for i in range(fc_n):
                problem = dict(fc[(offset * fc_n + i) % len(fc)])
                problem["kind"] = "4clojure"
                tasks.append(problem)
        if fw_n > 0 and fw:
            for i in range(fw_n):
                problem = dict(fw[(offset * fw_n + i) % len(fw)])
                problem["kind"] = "filewrite"
                tasks.append(problem)
    if not tasks:
        raise RuntimeError(
            "Workload selection produced zero tasks; check FILEWRITE_PER_ITER, AUTORESEARCH_N, problem pools."
        )
    return tasks


# ---------------------------------------------------------------------------
# Prompt rendering
# ---------------------------------------------------------------------------

def render_fc_problem_body(problem: dict) -> str:
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


def render_fc_prompt(problem: dict, solution_path: Path) -> str:
    template = FOURCLOJURE_PROMPT_TEMPLATE.read_text()
    return template.format(
        problem=render_fc_problem_body(problem),
        solution_path=str(solution_path),
        sentinel=SENTINEL,
    )


def render_fw_problem_body(problem: dict) -> str:
    return "\n".join([
        f"# {problem['id']}: {problem['title']}",
        "",
        f"Difficulty: {problem.get('difficulty', 'Unknown')}",
        "",
        problem["description"],
    ]) + "\n"


def render_fw_starter_listing(problem: dict) -> str:
    lines = []
    for f in problem.get("starter", []):
        lines.append(f"`{f['path']}`:")
        lines.append("```clojure")
        lines.append(f["content"].rstrip())
        lines.append("```")
    return "\n".join(lines) + ("\n" if lines else "")


def render_fw_prompt(problem: dict) -> str:
    template = FILEWRITE_PROMPT_TEMPLATE.read_text()
    return template.format(
        problem=render_fw_problem_body(problem),
        starter_listing=render_fw_starter_listing(problem),
    )


# ---------------------------------------------------------------------------
# Judges
# ---------------------------------------------------------------------------

def safe_get(d: dict | None, *path, default=None):
    cur: Any = d
    for k in path:
        if not isinstance(cur, dict):
            return default
        cur = cur.get(k)
        if cur is None:
            return default
    return cur if cur is not None else default


def judge_fourclojure(problem: dict, solution: str, workdir: Path) -> dict:
    p_path = workdir / "judge_problem.json"
    s_path = workdir / "judge_solution.edn"
    p_path.write_text(json.dumps(problem, ensure_ascii=False))
    s_path.write_text(solution)
    try:
        proc = subprocess.run(
            [CLOJURE, "-M", str(FOURCLOJURE_EVALUATOR), str(p_path), str(s_path)],
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


def judge_filewrite(problem: dict, workdir: Path) -> dict:
    """Run the task's verify form with workdir on classpath; check for :pass."""
    verify = problem.get("verify")
    if not verify:
        return {"passed": False, "judge_error": "missing verify"}
    # `-Sdeps` expects EDN, not JSON. Path is a plain string so a literal
    # `{:paths ["..."]}` is safe (the workdir comes from tempfile).
    deps_edn = '{:paths ["' + str(workdir).replace('\\', '\\\\').replace('"', '\\"') + '"]}'
    try:
        proc = subprocess.run(
            [CLOJURE, "-Sdeps", deps_edn, "-M", "-e", verify],
            cwd=workdir,
            capture_output=True,
            text=True,
            timeout=EVAL_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return {"passed": False, "judge_error": f"timeout after {EVAL_TIMEOUT}s"}
    out = proc.stdout
    if proc.returncode != 0:
        return {
            "passed": False,
            "judge_error": f"verify-exit-{proc.returncode}: {proc.stderr[-300:] or out[-300:]}",
            "stdout": out[-300:],
        }
    return {"passed": ":pass" in out, "stdout": out[-300:]}


# ---------------------------------------------------------------------------
# Trace stream parsing
# ---------------------------------------------------------------------------

def _classify_patch_extension(ext: str) -> str | None:
    """Return 'z' for z/patch, 'v' for v/patch, None otherwise.

    Both surfaces register the tool symbol `patch`; they differ by the
    `:extension` namespace (`...lang-clojure...` vs `...foundation...`).
    """
    e = (ext or "").lower()
    if "lang-clojure" in e or "lang_clojure" in e:
        return "z"
    if "foundation" in e:
        return "v"
    return None


def parse_trace_stream(raw: str) -> dict:
    result_payload: dict | None = None
    iteration_seen = -1
    last_phase: str | None = None
    events = 0
    z_patch_calls = 0
    v_patch_calls = 0
    block_errors = 0           # per-form eval errors during the turn
    error_iterations: set[int] = set()  # distinct iters that had >=1 error
    for raw_line in raw.splitlines():
        line = raw_line.strip()
        if not line or not (line.startswith("{") and line.endswith("}")):
            continue
        try:
            frame = json.loads(line)
        except Exception:
            continue
        events += 1
        ev = frame.get("event")
        payload = frame.get("payload")
        if ev == "result" and isinstance(payload, dict):
            result_payload = payload
            continue
        if not isinstance(payload, dict):
            continue
        last_phase = payload.get("phase") or last_phase
        for k in ("iteration", "iteration-count"):
            v = payload.get(k)
            if isinstance(v, int) and v > iteration_seen:
                iteration_seen = v
        te = payload.get("tool-event")
        if (
            isinstance(te, dict)
            and te.get("op") == "patch"
            and te.get("phase") == "tool-start"
        ):
            bucket = _classify_patch_extension(te.get("extension", ""))
            if bucket == "z":
                z_patch_calls += 1
            elif bucket == "v":
                v_patch_calls += 1
        # Per-form errors: form-result frames where info.status == "error".
        # Dedup by `ref` so a repaired form doesn't double-count.
        info = payload.get("info")
        if (
            isinstance(info, dict)
            and info.get("status") == "error"
            and payload.get("phase") == "form-result"
        ):
            block_errors += 1
            iter_n = info.get("iteration")
            if isinstance(iter_n, int):
                error_iterations.add(iter_n)
    return {
        "result": result_payload,
        "iterations_observed": max(iteration_seen, 0),
        "last_phase": last_phase,
        "events": events,
        "z_patch_calls": z_patch_calls,
        "v_patch_calls": v_patch_calls,
        "block_errors": block_errors,
        "error_iterations": sorted(error_iterations),
    }


# ---------------------------------------------------------------------------
# Task runner
# ---------------------------------------------------------------------------

def setup_workdir(problem: dict, workdir: Path) -> str:
    """Seed task-specific files, return the prompt string to send to Vis."""
    if problem["kind"] == "4clojure":
        (workdir / "problem.md").write_text(render_fc_problem_body(problem))
        (workdir / "solution.edn").write_text(SENTINEL)
        return render_fc_prompt(problem, workdir / "solution.edn")
    if problem["kind"] == "filewrite":
        for f in problem.get("starter", []):
            (workdir / f["path"]).write_text(f["content"])
        return render_fw_prompt(problem)
    raise ValueError(f"unknown task kind: {problem['kind']}")


def grade_task(problem: dict, workdir: Path) -> tuple[bool, dict, str]:
    """Return (passed, verdict, solution_payload_for_logging)."""
    if problem["kind"] == "4clojure":
        raw = (workdir / "solution.edn").read_text().strip() if (workdir / "solution.edn").exists() else ""
        solution = "" if raw == SENTINEL else raw
        if not solution:
            return False, {"passed": False, "judge_error": "empty-solution"}, ""
        verdict = judge_fourclojure(problem, solution, workdir)
        return bool(verdict.get("passed")), verdict, solution
    if problem["kind"] == "filewrite":
        # For logging, save the modified file paths' SHA + length.
        snapshot = {}
        for f in problem.get("starter", []):
            p = workdir / f["path"]
            if p.exists():
                snapshot[f["path"]] = {
                    "bytes": p.stat().st_size,
                    "head": p.read_text()[:200],
                }
        verdict = judge_filewrite(problem, workdir)
        return bool(verdict.get("passed")), verdict, json.dumps(snapshot, indent=2)
    raise ValueError(f"unknown task kind: {problem['kind']}")


def run_one(problem: dict, out_dir: Path) -> dict:
    workdir = Path(tempfile.mkdtemp(prefix=f"{problem['kind']}-{problem['id']}-"))
    started = time.time()
    rc = None
    stdout = ""
    stderr = ""
    timed_out = False
    try:
        prompt = setup_workdir(problem, workdir)
        prompt_chars = len(prompt)
        cmd = [
            VIS_BIN, "run", "--full-trace-json-stream",
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
            stdout = (
                e.stdout.decode("utf-8", "replace")
                if isinstance(e.stdout, (bytes, bytearray))
                else (e.stdout or "")
            )
            stderr = (
                e.stderr.decode("utf-8", "replace")
                if isinstance(e.stderr, (bytes, bytearray))
                else (e.stderr or "")
            )
        except FileNotFoundError:
            rc = -1
            stderr = f"VIS_BIN not found: {VIS_BIN}"

        trace = parse_trace_stream(stdout)
        envelope = trace["result"] or {}

        passed, verdict, solution_payload = grade_task(problem, workdir)

        envelope_iters = envelope.get("iteration-count")
        observed_iters = trace["iterations_observed"]
        elapsed_now   = time.time() - started
        if isinstance(envelope_iters, int) and envelope_iters > 0:
            iterations: int | None = envelope_iters
            iter_source = "envelope"
        elif observed_iters > 0:
            iterations = observed_iters
            iter_source = "trace"
        elif elapsed_now > 0:
            # Wall-time heuristic: timeouts / truncated streams reach
            # here. We attribute (elapsed - bootstrap) / per_iter to a
            # whole number of iterations (>= 1).
            iterations = estimate_iters_from_wall(elapsed_now)
            iter_source = "wall-estimate"
        else:
            iterations = None
            iter_source = "unknown"

        tokens_total = safe_get(envelope, "tokens", "total", default=0)
        prompt_tokens = safe_get(envelope, "tokens", "input", default=0)
        output_tokens = safe_get(envelope, "tokens", "output", default=0)
        reasoning_tokens = safe_get(envelope, "tokens", "reasoning", default=0)
        cost_usd = safe_get(envelope, "cost", "total-cost", default=0.0)
        elapsed = round(time.time() - started, 2)

        if not passed:
            iterations_score = FAIL_PENALTY
        elif isinstance(iterations, int):
            iterations_score = iterations
        else:
            iterations_score = FAIL_PENALTY

        outcome = {
            "kind": problem["kind"],
            "id": problem["id"],
            "title": problem.get("title"),
            "difficulty": problem.get("difficulty"),
            "passed": passed,
            "iterations": iterations,
            "iter_source": iter_source,
            "iterations_score": iterations_score,
            "timed_out": timed_out,
            "returncode": rc,
            "tokens_total": tokens_total,
            "prompt_tokens": prompt_tokens,
            "output_tokens": output_tokens,
            "reasoning_tokens": reasoning_tokens,
            "cost_usd": float(cost_usd) if isinstance(cost_usd, (int, float)) else 0.0,
            "prompt_chars": prompt_chars,
            "verdict": verdict,
            "trace_events": trace["events"],
            "trace_last_phase": trace["last_phase"],
            "z_patch_calls": trace["z_patch_calls"],
            "v_patch_calls": trace["v_patch_calls"],
            "block_errors": trace["block_errors"],
            "error_iterations": trace["error_iterations"],
            "elapsed_s": elapsed,
        }

        task_dir = out_dir / f"{problem['kind']}-{problem['id']}"
        task_dir.mkdir(parents=True, exist_ok=True)
        (task_dir / "prompt.txt").write_text(prompt)
        (task_dir / "vis-trace.jsonl").write_text(stdout)
        (task_dir / "vis-stderr.log").write_text(stderr)
        (task_dir / "outcome.json").write_text(
            json.dumps(outcome, indent=2, ensure_ascii=False) + "\n"
        )
        if solution_payload:
            (task_dir / "solution.txt").write_text(solution_payload)
        return outcome
    finally:
        shutil.rmtree(workdir, ignore_errors=True)


# ---------------------------------------------------------------------------
# Aggregation
# ---------------------------------------------------------------------------

def aggregate(outcomes: list[dict]) -> dict:
    pass_count = sum(1 for o in outcomes if o["passed"])
    iter_score = sum(o["iterations_score"] for o in outcomes)
    iters_pass = [
        o["iterations"] for o in outcomes
        if o["passed"] and isinstance(o["iterations"], int)
    ]
    cost_total = round(sum(o["cost_usd"] for o in outcomes), 6)
    tokens_total = sum(int(o["tokens_total"] or 0) for o in outcomes)
    prompt_chars = [o["prompt_chars"] for o in outcomes]
    z_patch_total = sum(int(o.get("z_patch_calls") or 0) for o in outcomes)
    v_patch_total = sum(int(o.get("v_patch_calls") or 0) for o in outcomes)
    patch_total   = z_patch_total + v_patch_total
    block_errors_total = sum(int(o.get("block_errors") or 0) for o in outcomes)
    by_kind: dict[str, dict[str, int]] = {}
    for o in outcomes:
        k = o["kind"]
        bucket = by_kind.setdefault(k, {"total": 0, "passed": 0, "iter_score": 0})
        bucket["total"] += 1
        bucket["iter_score"] += o["iterations_score"]
        if o["passed"]:
            bucket["passed"] += 1
    return {
        "task_count": len(outcomes),
        "pass_count": pass_count,
        "pass_rate": round(100.0 * pass_count / len(outcomes), 2) if outcomes else 0.0,
        "iter_score": iter_score,
        "mean_iterations_pass": (
            round(statistics.mean(iters_pass), 2) if iters_pass else None
        ),
        "max_iterations": max(iters_pass) if iters_pass else None,
        "total_cost_usd": cost_total,
        "total_tokens": tokens_total,
        "mean_prompt_chars": int(statistics.mean(prompt_chars)) if prompt_chars else 0,
        "wall_seconds": round(sum(o["elapsed_s"] for o in outcomes), 2),
        "by_kind":         by_kind,
        "z_patch_calls":   z_patch_total,
        "v_patch_calls":   v_patch_total,
        "z_patch_share":   (
            round(100.0 * z_patch_total / patch_total, 2) if patch_total else 0.0
        ),
        "block_errors":    block_errors_total,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> int:
    offset = autoresearch_offset()
    sizes = prompt_sizes()
    workload = pick_workload(offset)

    out_root = Path(
        os.environ.get(
            "AUTORESEARCH_OUT",
            str(HERE / "autoresearch" / time.strftime("%Y%m%d-%H%M%S")),
        )
    )
    out_root.mkdir(parents=True, exist_ok=True)
    (out_root / "chosen.json").write_text(
        json.dumps(
            [
                {
                    "kind": p["kind"], "id": p["id"], "title": p.get("title"),
                    "difficulty": p.get("difficulty"),
                }
                for p in workload
            ],
            indent=2,
            ensure_ascii=False,
        ) + "\n"
    )
    log(f"[autoresearch] offset={offset} workload={[(p['kind'], p['id']) for p in workload]} out={out_root}")
    log(f"[autoresearch] prompt sizes: system={sizes['system_prompt_chars']} foundation={sizes['foundation_prompt_chars']} z={sizes['z_prompt_chars']}")

    outcomes: list[dict] = []
    for problem in workload:
        log(
            f"[autoresearch] running {problem['kind']} {problem['id']} "
            f"({problem.get('difficulty')}): {problem.get('title')}"
        )
        o = run_one(problem, out_root)
        verdict_str = "PASS" if o["passed"] else "FAIL"
        iters_str = (
            f"{o['iterations']}({o['iter_source']})"
            if o["iterations"] is not None
            else f"?({o['iter_source']})"
        )
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
    summary["offset"]   = offset
    summary["rotated"]  = ROTATE
    summary["workload_signature"] = workload_signature(workload)
    # Prompt-size probe can fail. When it does every size is None;
    # propagate that to derived metrics so the metric loop skips them.
    valid_sizes = all(isinstance(v, int) and v >= 0 for v in sizes.values())
    summary["system_prompt_chars"]     = sizes["system_prompt_chars"]     if valid_sizes else None
    summary["foundation_prompt_chars"] = sizes["foundation_prompt_chars"] if valid_sizes else None
    summary["z_prompt_chars"]          = sizes["z_prompt_chars"]          if valid_sizes else None
    summary["ext_prompt_chars"]        = (
        sum(v for v in sizes.values() if isinstance(v, int)) if valid_sizes else None
    )
    summary["context_score"]           = (
        summary["ext_prompt_chars"] + summary["mean_prompt_chars"]
        if valid_sizes else None
    )
    summary["chosen"] = [
        {
            "kind": o["kind"],
            "id": o["id"],
            "passed": o["passed"],
            "iterations": o["iterations"],
            "iter_source": o["iter_source"],
            "iterations_score": o["iterations_score"],
            "cost_usd": o["cost_usd"],
            "timed_out": o["timed_out"],
            "z_patch_calls": o.get("z_patch_calls", 0),
            "v_patch_calls": o.get("v_patch_calls", 0),
            "block_errors":  o.get("block_errors", 0),
            "error_iterations": o.get("error_iterations", []),
        }
        for o in outcomes
    ]
    (out_root / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")

    metric_keys = [
        "iter_score",
        "pass_count",
        "pass_rate",
        "mean_iterations_pass",
        "max_iterations",
        "total_cost_usd",
        "total_tokens",
        "mean_prompt_chars",
        "system_prompt_chars",
        "foundation_prompt_chars",
        "z_prompt_chars",
        "ext_prompt_chars",
        "context_score",
        "z_patch_calls",
        "v_patch_calls",
        "z_patch_share",
        "block_errors",
        "wall_seconds",
    ]
    for k in metric_keys:
        v = summary.get(k)
        if v is None:
            continue
        print(f"METRIC {k}={v}")

    # Self-consistency assertions — fail loudly if invariants break.
    assert summary["task_count"] == len(workload), (
        f"task_count {summary['task_count']} != workload size {len(workload)}"
    )
    assert 0 <= summary["pass_count"] <= summary["task_count"], (
        f"pass_count out of range: {summary['pass_count']} / {summary['task_count']}"
    )
    if summary["pass_count"] > 0:
        assert summary["mean_iterations_pass"] is not None, "mean_iterations_pass missing despite pass_count > 0"
        assert summary["max_iterations"] is not None, "max_iterations missing despite pass_count > 0"
    if summary["z_patch_calls"] + summary["v_patch_calls"] == 0:
        assert summary["z_patch_share"] == 0.0, "z_patch_share must be 0 when there are no patch calls"
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
