#!/usr/bin/env python3
"""Solve one SWE-bench instance with Vis. Prints a unified-diff prediction.

Contract:
- stdin: one JSON object from instances.json (or the full Lite dataset).
- stdout (on success): JSON `{"instance_id": ..., "model_patch": "..."}`.
- stdout (on failure): JSON `{"instance_id": ..., "model_patch": "",
                              "error": "<reason>"}`.
- stderr: human-readable progress.

Vis is invoked with `--db :memory` so no state leaks between instances.
The agent runs inside a fresh git worktree of the target repo at
`base_commit` so the final `git diff` is the patch SWE-bench will score.
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path

REPO_CACHE = Path(
    os.environ.get("SWEBENCH_REPO_CACHE", str(Path.home() / ".cache" / "swebench-repos"))
)
PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
MODEL = os.environ.get("VIS_MODEL", "glm-5.1")
TIMEOUT = int(os.environ.get("VIS_INSTANCE_TIMEOUT", "900"))
VIS_BIN = os.environ.get("VIS_BIN", "vis")


def log(msg: str) -> None:
    print(f"[vis_agent] {msg}", file=sys.stderr, flush=True)


def ensure_repo(repo: str) -> Path:
    """Mirror clone, cached in REPO_CACHE."""
    REPO_CACHE.mkdir(parents=True, exist_ok=True)
    mirror = REPO_CACHE / repo.replace("/", "__")
    if not mirror.exists():
        log(f"cloning {repo} → {mirror}")
        subprocess.run(
            ["git", "clone", "--mirror", f"https://github.com/{repo}.git", str(mirror)],
            check=True,
        )
    else:
        subprocess.run(["git", "-C", str(mirror), "fetch", "--all", "--tags", "-q"], check=False)
    return mirror


def make_worktree(mirror: Path, base_commit: str) -> Path:
    """Clone-from-mirror so we have a real working tree at base_commit."""
    wt = Path(tempfile.mkdtemp(prefix="swebench-wt-"))
    subprocess.run(["git", "clone", "-q", str(mirror), str(wt)], check=True)
    subprocess.run(["git", "-C", str(wt), "checkout", "-q", base_commit], check=True)
    return wt


def render_prompt(instance: dict, workdir: Path) -> str:
    return f"""You are working inside a git checkout of {instance['repo']} at commit \
{instance['base_commit']} (cwd = {workdir}).

Your task:

<problem_statement>
{instance['problem_statement']}
</problem_statement>

Constraints:
- Modify only source files needed to fix the issue described above.
- Do NOT modify tests, CI files, or version bump files.
- When you believe you are done, STOP. Do not run tests or commit; the
  grader will diff the working tree against HEAD.
- If you cannot solve the task, exit with an empty patch rather than
  speculating.
"""


def solve(instance: dict) -> dict:
    iid = instance["instance_id"]
    started = time.time()
    try:
        mirror = ensure_repo(instance["repo"])
        wt = make_worktree(mirror, instance["base_commit"])
    except subprocess.CalledProcessError as e:
        return {"instance_id": iid, "model_patch": "", "error": f"checkout-failed: {e}"}

    prompt = render_prompt(instance, wt)
    cmd = [
        VIS_BIN, "run",
        "--json",
        "--db", ":memory",
        "--provider", PROVIDER,
        "--model", MODEL,
        prompt,
    ]
    log(f"{iid}: launching vis ({MODEL})")
    try:
        proc = subprocess.run(
            cmd, cwd=wt, capture_output=True, text=True, timeout=TIMEOUT
        )
    except subprocess.TimeoutExpired:
        return {
            "instance_id": iid,
            "model_patch": "",
            "error": f"timeout after {TIMEOUT}s",
            "elapsed_s": TIMEOUT,
        }

    elapsed = time.time() - started
    if proc.returncode != 0:
        return {
            "instance_id": iid,
            "model_patch": "",
            "error": f"vis-exit-{proc.returncode}: {proc.stderr[-400:]}",
            "elapsed_s": elapsed,
        }

    diff = subprocess.run(
        ["git", "-C", str(wt), "diff", "--no-color", "HEAD"],
        capture_output=True, text=True, check=True,
    ).stdout

    # Best-effort token/cost extraction from vis --json output
    tokens = cost = None
    try:
        payload = json.loads(proc.stdout)
        tokens = payload.get("usage", {}).get("total_tokens")
        cost = payload.get("usage", {}).get("cost_usd")
    except Exception:
        pass

    return {
        "instance_id": iid,
        "model_patch": diff,
        "model_name_or_path": f"vis+{PROVIDER}/{MODEL}",
        "elapsed_s": round(elapsed, 1),
        "tokens": tokens,
        "cost_usd": cost,
        "empty": (diff.strip() == ""),
    }


def main() -> int:
    instance = json.load(sys.stdin)
    result = solve(instance)
    json.dump(result, sys.stdout)
    sys.stdout.write("\n")
    return 0 if result.get("model_patch") else 0  # never crash the driver


if __name__ == "__main__":
    sys.exit(main())
