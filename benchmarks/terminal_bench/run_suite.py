"""Run remaining CPU Terminal-Bench tasks in resumable Harbor batches."""

import argparse
import gzip
import json
import os
import shutil
import subprocess
import sys
import tomllib
from pathlib import Path

from archive_traces import archive_trace
from capture_trace import capture

MODEL = "zai-coding-plan/glm-5.3-flash"
ROOT = Path(__file__).resolve().parent
DATASET = ROOT / "artifacts/datasets/terminal-bench"
JOBS = ROOT / "jobs"


def catalog(dataset: Path) -> tuple[list[dict], list[str]]:
    """Order CPU tasks by expert estimate; report GPU tasks separately."""
    tasks = []
    gpu_tasks = []
    for directory in dataset.iterdir():
        if not directory.is_dir():
            continue
        info = tomllib.loads((directory / "task.toml").read_text(encoding="utf-8"))
        environment = info.get("environment", {})
        verifier = info.get("verifier", {}).get("environment", {})
        if (environment.get("gpus") or 0) or (verifier.get("gpus") or 0):
            gpu_tasks.append(directory.name)
            continue
        tasks.append(
            {
                "name": directory.name,
                "hours": info.get("metadata", {}).get(
                    "expert_time_estimate_hours", 999
                ),
                "memory_mb": max(
                    environment.get("memory_mb") or 0,
                    verifier.get("memory_mb") or 0,
                ),
            }
        )
    tasks.sort(key=lambda task: (task["hours"], task["name"]))
    return tasks, sorted(gpu_tasks)


def has_vis_result(result: dict) -> bool:
    """Require metrics from the pinned model, not a synthetic setup result."""
    agent = result.get("agent_result")
    metadata = agent.get("metadata") if isinstance(agent, dict) else None
    vis = metadata.get("vis") if isinstance(metadata, dict) else None
    return isinstance(vis, dict) and vis.get("model") == MODEL


def has_pinned_provider_call(trial: Path) -> bool:
    """Detect real model work in a failed trial with no final Vis result."""
    for path in (trial / "agent/vis-trace.jsonl.gz", trial / "agent/vis-trace.jsonl"):
        if not path.is_file():
            continue
        opener = gzip.open if path.suffix == ".gz" else open
        try:
            with opener(path, "rt", encoding="utf-8") as stream:
                for line in stream:
                    if "provider-call" not in line:
                        continue
                    try:
                        frame = json.loads(line)
                    except json.JSONDecodeError:
                        continue
                    payload = frame.get("payload")
                    if (
                        isinstance(payload, dict)
                        and payload.get("phase") == "provider-call"
                        and f"{payload.get('provider')}/{payload.get('model')}" == MODEL
                    ):
                        return True
        except (EOFError, OSError, UnicodeDecodeError):
            continue
    return False


def accounted_tasks(jobs: Path) -> tuple[set[str], set[str], set[str]]:
    """Skip scored and failed model attempts, but retry setup and canceled trials."""
    completed = set()
    failed = set()
    for path in jobs.glob("*/*/result.json"):
        result = json.loads(path.read_text(encoding="utf-8"))
        task = result.get("task_name")
        if (
            isinstance(task, str)
            and result.get("finished_at")
            and has_vis_result(result)
            and result.get("verifier_result") is not None
        ):
            completed.add(task.removeprefix("terminal-bench/"))
        elif (
            isinstance(task, str)
            and result.get("finished_at")
            and (result.get("exception_info") or {}).get("exception_type")
            == "NonZeroAgentExitCodeError"
            and has_pinned_provider_call(path.parent)
        ):
            failed.add(task.removeprefix("terminal-bench/"))
    in_flight = set()
    for path in jobs.glob("*/*/config.json"):
        trial = path.parent
        job_result = trial.parent / "result.json"
        if (trial / "result.json").exists():
            continue
        if job_result.exists() and json.loads(job_result.read_text()).get(
            "finished_at"
        ):
            continue
        in_flight.add(trial.name.split("__", 1)[0])
    return completed, in_flight, failed


def next_batch(pending: list[dict]) -> list[dict]:
    """Reserve the high-memory tasks for single-trial Harbor jobs."""
    first = pending.pop(0)
    batch = [first]
    if first["memory_mb"] >= 16384:
        return batch
    for index, task in enumerate(pending):
        if (
            task["memory_mb"] < 16384
            and first["memory_mb"] + task["memory_mb"] <= 16384
        ):
            batch.append(pending.pop(index))
            break
    return batch


def free_gb(machine: str) -> tuple[float, float]:
    """Measure both the host artifact volume and the Podman VM image volume."""
    host = shutil.disk_usage(ROOT).free / 1e9
    result = subprocess.run(
        ["podman", "machine", "ssh", machine, "--", "df", "-Pk", "/var/lib/containers"],
        check=True,
        capture_output=True,
        text=True,
        timeout=30,
    )
    vm = int(result.stdout.strip().splitlines()[-1].split()[3]) * 1024 / 1e9
    return host, vm


def job_name(prefix: str, jobs: Path) -> str:
    """Never overwrite a previous job, including incomplete attempts."""
    number = 1
    while (jobs / f"{prefix}-{number:03}").exists():
        number += 1
    return f"{prefix}-{number:03}"


def batch_results(job: Path, tasks: list[dict]) -> list[dict]:
    """Treat missing metrics or a missing verifier as a runner failure."""
    expected = {task["name"] for task in tasks}
    found = {}
    for path in job.glob("*/result.json"):
        result = json.loads(path.read_text(encoding="utf-8"))
        name = str(result.get("task_name") or "").removeprefix("terminal-bench/")
        if name in expected:
            found[name] = result
    if set(found) != expected:
        raise RuntimeError(
            f"Incomplete job {job.name}: missing results for {sorted(expected - set(found))}"
        )
    for name, result in found.items():
        if not has_vis_result(result) or result.get("verifier_result") is None:
            raise RuntimeError(
                f"Incomplete metrics in {job.name}/{name}; inspect the trial"
            )
    return [found[task["name"]] for task in tasks]


def archive_batch_traces(job: Path) -> None:
    """Archive finished gzip traces, preserving any incomplete streams."""
    for source in sorted(job.glob("*/agent/vis-trace.jsonl.gz")):
        try:
            manifest = archive_trace(source)
        except (EOFError, gzip.BadGzipFile):
            print(
                f"Preserved incomplete gzip: {job.name}/{source.parent.parent.name}",
                flush=True,
            )
            continue
        print(
            f"Archived {job.name}/{source.parent.parent.name}: "
            f"{manifest['original_gzip_bytes']} -> {manifest['archived_zstd_bytes']} bytes",
            flush=True,
        )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--machine", default="vis-amd64")
    parser.add_argument("--job-prefix", default="suite")
    parser.add_argument("--max-batches", type=int)
    parser.add_argument("--min-free-gb", type=float, default=12.0)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    tasks, gpu_tasks = catalog(DATASET)
    completed, in_flight, failed = accounted_tasks(JOBS)
    pending = [
        task for task in tasks if task["name"] not in completed | in_flight | failed
    ]
    print(
        f"CPU tasks: {len(tasks)}; completed: {len(completed & {t['name'] for t in tasks})}; "
        f"failed after model call: {len(failed)}; live: {len(in_flight)}; "
        f"pending: {len(pending)}; GPU-only: {len(gpu_tasks)}",
        flush=True,
    )
    if args.dry_run:
        print("Next tasks:", ", ".join(task["name"] for task in pending[:10]))
        return
    if not os.environ.get("ZAI_CODING_API_KEY"):
        raise RuntimeError("ZAI_CODING_API_KEY is required in the Harbor host")
    if args.max_batches is not None and args.max_batches < 1:
        parser.error("--max-batches must be positive")
    batch_count = 0
    errors_in_a_row = 0
    (ROOT / "runs").mkdir(exist_ok=True)
    while pending and (args.max_batches is None or batch_count < args.max_batches):
        host, vm = free_gb(args.machine)
        if min(host, vm) < args.min_free_gb:
            raise RuntimeError(
                f"Low disk space (host {host:.1f} GB, VM {vm:.1f} GB); "
                "reclaim finished artifacts/images and resume"
            )
        batch = next_batch(pending)
        name = job_name(args.job_prefix, JOBS)
        command = [
            str(Path(sys.executable).parent / "harbor"),
            "run",
            "-p",
            str(DATASET),
            "-a",
            "vis_agent:VisAgent",
            "-m",
            MODEL,
            "-e",
            "podman",
            "-k",
            "1",
            "-n",
            str(len(batch)),
            "-o",
            str(JOBS),
            "--job-name",
            name,
        ]
        for task in batch:
            command.extend(("-i", task["name"]))
        print(
            f"Starting {name}: {', '.join(task['name'] for task in batch)}", flush=True
        )
        returncode = capture(command, ROOT / "runs" / f"{name}.log", cwd=ROOT)
        if returncode:
            raise RuntimeError(
                f"Harbor exited {returncode} in {name}; inspect runs/{name}.log"
            )
        results = batch_results(JOBS / name, batch)
        archive_batch_traces(JOBS / name)
        for task, trial in zip(batch, results, strict=True):
            metadata = (trial["agent_result"].get("metadata") or {}).get("vis") or {}
            is_error = metadata.get("status") == "error"
            errors_in_a_row = errors_in_a_row + 1 if is_error else 0
            reward = (trial["verifier_result"].get("rewards") or {}).get("reward")
            print(
                f"Finished {name}/{task['name']}: reward={reward}, agent_error={is_error}",
                flush=True,
            )
        if errors_in_a_row >= 2:
            raise RuntimeError(
                "Two consecutive agent errors; inspect the traces before continuing"
            )
        batch_count += 1
    print(
        f"Batch limit or remaining tasks reached; batches completed: {batch_count}",
        flush=True,
    )


if __name__ == "__main__":
    main()
