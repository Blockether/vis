"""Run remaining CPU Terminal-Bench tasks as a resumable pool of Harbor jobs."""

import argparse
import gzip
import json
import os
import shutil
import subprocess
import sys
import tomllib
from collections.abc import Collection, Iterator
from concurrent.futures import FIRST_COMPLETED, Future, ThreadPoolExecutor, wait
from contextlib import contextmanager
from pathlib import Path
from typing import IO

from archive_traces import archive_trace
from capture_trace import capture

MODEL = "zai-coding-plan/glm-5.3-flash"
ROOT = Path(__file__).resolve().parent
DATASET = ROOT / "artifacts/datasets/terminal-bench"
JOBS = ROOT / "jobs"
CONCURRENCY = 2
MEMORY_BUDGET_MB = 16384
# Provider or setup breakage fails fast; observed model failures took over 20 minutes.
FAST_ERROR_MS = 600_000


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
                "images": [
                    image
                    for image in (
                        environment.get("docker_image"),
                        verifier.get("docker_image"),
                    )
                    if image
                ],
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


@contextmanager
def trace_stream(path: Path) -> Iterator[IO[str]]:
    """Stream raw, gzip or archived zstd traces, stopping zstd on early exit."""
    if path.suffix != ".zst":
        opener = gzip.open if path.suffix == ".gz" else open
        with opener(path, "rt", encoding="utf-8") as stream:
            yield stream
        return
    with subprocess.Popen(
        ["zstd", "-dc", "--", str(path)],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
        encoding="utf-8",
    ) as process:
        try:
            yield process.stdout
        finally:
            process.kill()


def pinned_model_work(trial: Path) -> tuple[bool, bool]:
    """Report whether a trace called the pinned model and whether it streamed output."""
    for path in (
        trial / "agent/vis-trace.jsonl.gz",
        trial / "agent/vis-trace.jsonl.zst",
        trial / "agent/vis-trace.jsonl",
    ):
        if not path.is_file():
            continue
        called = False
        try:
            with trace_stream(path) as stream:
                for line in stream:
                    if "provider-call" not in line and '"delta"' not in line:
                        continue
                    try:
                        frame = json.loads(line)
                    except json.JSONDecodeError:
                        continue
                    payload = frame.get("payload")
                    if not isinstance(payload, dict):
                        continue
                    phase = payload.get("phase")
                    if phase == "provider-call":
                        model = f"{payload.get('provider')}/{payload.get('model')}"
                        called = called or model == MODEL
                    elif (
                        called
                        and phase in {"reasoning", "content"}
                        and payload.get("delta")
                    ):
                        return True, True
        except (EOFError, OSError, UnicodeDecodeError):
            pass
        if called:
            return True, False
    return False, False


def has_pinned_provider_call(trial: Path) -> bool:
    """Detect real model work in a failed trial with no final Vis result."""
    return pinned_model_work(trial)[0]


def exception_type(result: dict) -> str | None:
    """Name the exception Harbor recorded for a trial, if any."""
    return (result.get("exception_info") or {}).get("exception_type")


def is_scored_attempt(result: dict, trial: Path) -> bool:
    """Accept verified Vis results and timeouts once the pinned model streamed output."""
    if result.get("verifier_result") is None:
        return False
    # Harbor verifies a timed-out agent, but Vis never writes its final result.
    if not has_vis_result(result) and exception_type(result) != "AgentTimeoutError":
        return False
    # A provider that refused every call measured the runner, not the agent.
    return pinned_model_work(trial)[1]


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
            and is_scored_attempt(result, path.parent)
        ):
            completed.add(task.removeprefix("terminal-bench/"))
        elif (
            isinstance(task, str)
            and result.get("finished_at")
            and exception_type(result)
            in {"AgentTimeoutError", "NonZeroAgentExitCodeError"}
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


def next_task(pending: list[dict], running: list[dict]) -> dict | None:
    """Start the first pending task that fits beside the running trials."""
    for index, task in enumerate(pending):
        peers = [*running, task]
        if not running or (
            len(peers) <= CONCURRENCY
            and all(peer["memory_mb"] < MEMORY_BUDGET_MB for peer in peers)
            and sum(peer["memory_mb"] for peer in peers) <= MEMORY_BUDGET_MB
        ):
            return pending.pop(index)
    return None


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


def reclaim_images(machine: str, task: dict) -> None:
    """Remove a finished task's images, then return freed VM blocks to the host."""
    steps = (
        (
            "remove images",
            ["podman", "--connection", machine, "rmi", "--ignore", *task["images"]],
        ),
        (
            "trim the VM disk",
            ["podman", "machine", "ssh", machine, "--"]
            + ["sudo", "fstrim", "/var/lib/containers"],
        ),
    )
    for step, command in steps:
        result = subprocess.run(command, capture_output=True, text=True, timeout=600)
        if result.returncode:
            print(
                f"Could not {step} after {task['name']}: {result.stderr.strip()}",
                flush=True,
            )


def job_name(prefix: str, jobs: Path, taken: Collection[str] = ()) -> str:
    """Never overwrite a previous job, including incomplete or starting attempts."""
    number = 1
    while (name := f"{prefix}-{number:03}") in taken or (jobs / name).exists():
        number += 1
    return name


def job_result(job: Path, task: dict) -> dict:
    """Treat missing metrics or a missing verifier as a runner failure."""
    for path in job.glob("*/result.json"):
        result = json.loads(path.read_text(encoding="utf-8"))
        name = str(result.get("task_name") or "").removeprefix("terminal-bench/")
        if name != task["name"]:
            continue
        if not is_scored_attempt(result, path.parent):
            raise RuntimeError(
                f"Incomplete metrics in {job.name}/{name}; inspect the trial"
            )
        return result
    raise RuntimeError(f"Incomplete job {job.name}: missing result for {task['name']}")


def archive_job_traces(job: Path) -> None:
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


def harbor_command(name: str, task: dict) -> list[str]:
    """Give each task its own Harbor job so a free slot can start the next task."""
    return [
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
        "1",
        "-o",
        str(JOBS),
        "--job-name",
        name,
        "-i",
        task["name"],
    ]


def finish_job(name: str, task: dict, returncode: int) -> bool:
    """Validate, archive and report a job; return whether Vis failed fast."""
    if returncode:
        raise RuntimeError(
            f"Harbor exited {returncode} in {name}; inspect runs/{name}.log"
        )
    result = job_result(JOBS / name, task)
    archive_job_traces(JOBS / name)
    agent = result.get("agent_result") or {}
    metadata = (agent.get("metadata") or {}).get("vis") or {}
    is_error = metadata.get("status") == "error"
    is_timeout = exception_type(result) == "AgentTimeoutError"
    reward = (result["verifier_result"].get("rewards") or {}).get("reward")
    print(
        f"Finished {name}/{task['name']}: reward={reward}, "
        f"agent_error={is_error}, agent_timeout={is_timeout}",
        flush=True,
    )
    return is_error and (metadata.get("duration_ms") or 0) < FAST_ERROR_MS


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--machine", default="vis-amd64")
    parser.add_argument("--job-prefix", default="suite")
    parser.add_argument("--max-tasks", type=int)
    parser.add_argument("--min-free-gb", type=float, default=12.0)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--retry-task",
        action="append",
        default=[],
        metavar="NAME",
        help="retry a named failed model attempt (repeatable)",
    )
    args = parser.parse_args()
    tasks, gpu_tasks = catalog(DATASET)
    completed, in_flight, failed = accounted_tasks(JOBS)
    retry_tasks = set(args.retry_task)
    retryable = (failed & {task["name"] for task in tasks}) - completed - in_flight
    if invalid := retry_tasks - retryable:
        parser.error(
            "--retry-task requires an uncompleted, inactive failed model attempt: "
            + ", ".join(sorted(invalid))
        )
    pending = [task for task in tasks if task["name"] in retry_tasks] + [
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
    if args.max_tasks is not None and args.max_tasks < 1:
        parser.error("--max-tasks must be positive")
    (ROOT / "runs").mkdir(exist_ok=True)
    running: dict[Future, tuple[str, dict]] = {}
    failures = []
    started = 0
    fast_errors = 0

    def stop_starting(reason: str) -> None:
        """Start no more tasks, and say why while running trials finish."""
        failures.append(reason)
        print(f"Not starting more tasks: {reason}", flush=True)

    with ThreadPoolExecutor(max_workers=CONCURRENCY) as pool:
        while True:
            while not failures and (args.max_tasks is None or started < args.max_tasks):
                task = next_task(pending, [peer for _, peer in running.values()])
                if task is None:
                    break
                host, vm = free_gb(args.machine)
                if min(host, vm) < args.min_free_gb:
                    stop_starting(
                        f"Low disk space (host {host:.1f} GB, VM {vm:.1f} GB); "
                        "reclaim finished artifacts/images and resume"
                    )
                    break
                name = job_name(
                    args.job_prefix, JOBS, [job for job, _ in running.values()]
                )
                print(f"Starting {name}: {task['name']}", flush=True)
                log = ROOT / "runs" / f"{name}.log"
                future = pool.submit(capture, harbor_command(name, task), log, cwd=ROOT)
                running[future] = (name, task)
                started += 1
            if not running:
                break
            done, _ = wait(running, return_when=FIRST_COMPLETED)
            for future in done:
                name, task = running.pop(future)
                try:
                    fast_error = finish_job(name, task, future.result())
                except RuntimeError as error:
                    stop_starting(str(error))
                    continue
                reclaim_images(args.machine, task)
                fast_errors = fast_errors + 1 if fast_error else 0
                if fast_errors == 2:
                    stop_starting(
                        "Two consecutive fast agent errors; inspect the traces before continuing"
                    )
    if failures:
        raise RuntimeError("; ".join(failures))
    print(
        f"Task limit or remaining tasks reached; tasks started: {started}", flush=True
    )


if __name__ == "__main__":
    main()
