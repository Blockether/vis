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
import shutil
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
MODEL_NAME = f"vis+{PROVIDER}/{MODEL}"
ARTIFACT_ROOT = os.environ.get("SWEBENCH_ARTIFACT_DIR")


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


def safe_id(iid: str) -> str:
    return "".join(c if c.isalnum() or c in "._-" else "_" for c in iid)


def artifact_dir(iid: str) -> Path | None:
    if not ARTIFACT_ROOT:
        return None
    d = Path(ARTIFACT_ROOT) / safe_id(iid)
    d.mkdir(parents=True, exist_ok=True)
    return d


def write_artifact(iid: str, name: str, content: str | bytes | dict | list | None) -> None:
    d = artifact_dir(iid)
    if d is None or content is None:
        return
    path = d / name
    if isinstance(content, (dict, list)):
        path.write_text(json.dumps(content, indent=2, ensure_ascii=False))
    elif isinstance(content, bytes):
        path.write_bytes(content)
    else:
        path.write_text(str(content))


def result_base(iid: str) -> dict:
    result = {"instance_id": iid, "model_name_or_path": MODEL_NAME, "model_patch": ""}
    if ARTIFACT_ROOT:
        result["artifact_dir"] = str(Path(ARTIFACT_ROOT) / safe_id(iid))
    return result


def solve(instance: dict) -> dict:
    iid = instance["instance_id"]
    started = time.time()
    wt: Path | None = None
    write_artifact(iid, "instance.json", instance)
    try:
        try:
            mirror = ensure_repo(instance["repo"])
            wt = make_worktree(mirror, instance["base_commit"])
        except subprocess.CalledProcessError as e:
            result = {**result_base(iid), "error": f"checkout-failed: {e}"}
            write_artifact(iid, "prediction.json", result)
            return result

        prompt = render_prompt(instance, wt)
        write_artifact(iid, "prompt.txt", prompt)
        cmd = [
            VIS_BIN, "run",
            "--json",
            "--db", ":memory",
            "--provider", PROVIDER,
            "--model", MODEL,
            prompt,
        ]
        write_artifact(iid, "command.json", {"cmd": cmd, "cwd": str(wt), "timeout_s": TIMEOUT})
        log(f"{iid}: launching vis ({MODEL})")
        try:
            proc = subprocess.run(
                cmd, cwd=wt, capture_output=True, text=True, timeout=TIMEOUT
            )
        except subprocess.TimeoutExpired as e:
            write_artifact(iid, "vis.stdout.txt", e.stdout)
            write_artifact(iid, "vis.stderr.txt", e.stderr)
            result = {
                **result_base(iid),
                "error": f"timeout after {TIMEOUT}s",
                "elapsed_s": TIMEOUT,
            }
            write_artifact(iid, "prediction.json", result)
            return result

        elapsed = time.time() - started
        write_artifact(iid, "vis.stdout.json", proc.stdout)
        write_artifact(iid, "vis.stderr.txt", proc.stderr)
        if proc.returncode != 0:
            result = {
                **result_base(iid),
                "error": f"vis-exit-{proc.returncode}: {proc.stderr[-400:]}",
                "elapsed_s": round(elapsed, 1),
            }
            write_artifact(iid, "prediction.json", result)
            return result

        diff = subprocess.run(
            ["git", "-C", str(wt), "diff", "--no-color", "HEAD"],
            capture_output=True, text=True, check=True,
        ).stdout

        # Best-effort token/cost extraction from vis --json output. The Vis
        # envelope currently exposes top-level :tokens/:cost maps; keep the
        # older :usage shape too so historical runs remain readable.
        tokens = cost = None
        try:
            payload = json.loads(proc.stdout)
            usage = payload.get("usage", {}) or {}
            token_payload = payload.get("tokens") or {}
            cost_payload = payload.get("cost") or {}
            tokens = usage.get("total_tokens") or token_payload.get("total")
            cost = usage.get("cost_usd") or cost_payload.get("total-cost")
        except Exception:
            pass

        write_artifact(iid, "patch.diff", diff)
        result = {
            **result_base(iid),
            "model_patch": diff,
            "elapsed_s": round(elapsed, 1),
            "tokens": tokens,
            "cost_usd": cost,
            "empty": (diff.strip() == ""),
        }
        write_artifact(iid, "prediction.json", result)
        return result
    finally:
        if wt is not None:
            shutil.rmtree(wt, ignore_errors=True)


def main() -> int:
    instance = json.load(sys.stdin)
    result = solve(instance)
    json.dump(result, sys.stdout)
    sys.stdout.write("\n")
    return 0 if result.get("model_patch") else 0  # never crash the driver


if __name__ == "__main__":
    sys.exit(main())
