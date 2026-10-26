"""Check Harbor tasks with the ``harbor check`` rubric and the Vis agent.

Run it with ``python -m blockether.vis.evals.check TASK_DIR --provider P --model M``.
``TASK_DIR`` is one task or a directory of tasks, for example ``e2e`` of the Vis
repository. Each check makes paid model calls. Before them, the command runs the
file checks of `task_problems` and stops at the first problem. ``--static`` runs only
those file checks, without a model.

``harbor check`` runs Claude Code in Docker. This module uses the same rubric,
prompt and result validation of Harbor. The Vis agent runs each check in the host
environment, so the check needs no Docker and no Anthropic key. The reviewed task
is in ``task/`` of the trial workspace, and the agent writes ``check-result.json``
next to it. The job directory holds ``check_report.json``.
"""

from __future__ import annotations

import argparse
import asyncio
import shutil
import sys
import tempfile
from collections.abc import Sequence
from datetime import UTC, datetime
from pathlib import Path

from harbor.analyze import checker
from harbor.analyze.models import (
    QualityCheckResult,
    build_check_response_model,
    load_rubric,
)
from harbor.cli.quality_checker.models import CheckReport
from harbor.models.job.config import JobConfig
from harbor.models.trial.config import AgentConfig, EnvironmentConfig, TaskConfig

from .environment import WORKSPACE_DIRECTORY
from .harbor import TASK_FILE, read_tasks, task_problems

AGENT = "blockether.vis.evals.agent:VisAgent"
ENVIRONMENT = "blockether.vis.evals.environment:HostEnvironment"
DEFAULT_WORKERS = 5
TASK_DIRECTORY = "task"
DEFAULT_JOBS = "jobs/vis-check"
REPORT_FILE = "check_report.json"
TEST_SCRIPT = """#!/usr/bin/env bash
# Validate the check result. The host environment sets the VIS_HARBOR_* paths.
if "$VIS_HARBOR_PYTHON" "$VIS_HARBOR_TESTS/validate.py" check-result.json; then
  echo 1 > "$VIS_HARBOR_VERIFIER_LOGS/reward.txt"
else
  echo 0 > "$VIS_HARBOR_VERIFIER_LOGS/reward.txt"
fi
"""


def prompt_template() -> str:
    """Return Harbor's check prompt with the reviewed task in ``task/`` of the workspace."""
    return (
        (checker.PROMPTS_DIR / "check.txt")
        .read_text()
        .replace("{task_path}", TASK_DIRECTORY)
    )


def assemble(task_dir: Path, rubric, response_model, dest: Path) -> Path:
    """Build the check task of ``task_dir`` in ``dest`` for the host environment.

    Harbor's verifier script writes to the container paths ``/logs`` and ``/tests``.
    The host environment translates only commands, so the wrapper gets a script
    that reads the ``VIS_HARBOR_*`` paths.
    """
    wrapper = checker.assemble_check_task(
        task_dir=task_dir,
        rubric=rubric,
        template=prompt_template(),
        output_schema=response_model.model_json_schema(),
        dest=dest,
    )
    environment = wrapper / "environment"
    workspace = environment / WORKSPACE_DIRECTORY
    workspace.mkdir()
    (environment / TASK_DIRECTORY).rename(workspace / TASK_DIRECTORY)
    script = wrapper / "tests" / "test.sh"
    script.write_text(TEST_SCRIPT)
    script.chmod(0o755)
    return wrapper


def job_config(
    wrappers: Sequence[Path],
    provider: str,
    model: str,
    jobs_dir: Path,
    job_name: str,
    workers: int,
) -> JobConfig:
    """Return the Harbor job that checks each wrapper task once with the Vis agent."""
    return JobConfig(
        job_name=job_name,
        jobs_dir=jobs_dir,
        n_concurrent_trials=workers,
        n_attempts=1,
        quiet=True,
        agents=[AgentConfig(import_path=AGENT, model_name=f"{provider}/{model}")],
        environment=EnvironmentConfig(import_path=ENVIRONMENT),
        tasks=[TaskConfig(path=wrapper) for wrapper in wrappers],
        datasets=[],
    )


async def check(
    path: Path,
    provider: str,
    model: str,
    jobs_dir: Path = Path(DEFAULT_JOBS),
    job_name: str | None = None,
    workers: int = DEFAULT_WORKERS,
) -> tuple[CheckReport, Path]:
    """Check the task or the tasks in ``path``. Return the report and the job directory."""
    from harbor.job import Job

    task_dirs = checker._resolve_task_dirs(Path(path), None, None, None)
    if problems := [problem for task in task_dirs for problem in task_problems(task)]:
        raise ValueError("fix the tasks before a paid check:\n" + "\n".join(problems))
    rubric = load_rubric(None)
    response_model = build_check_response_model(rubric)
    name = job_name or datetime.now(UTC).strftime("check-%Y%m%d-%H%M%S")
    tmp = Path(tempfile.mkdtemp(prefix="vis-harbor-check-"))
    try:
        names = {}
        wrappers = []
        for task_dir in task_dirs:
            wrapper = assemble(
                task_dir,
                rubric,
                response_model,
                tmp / f"check-{task_dir.resolve().name}",
            )
            wrappers.append(wrapper)
            names[str(wrapper.resolve())] = task_dir.name
        config = job_config(wrappers, provider, model, Path(jobs_dir), name, workers)
        job = await Job.create(config)
        job_result = await job.run()
        results = []
        for trial_result in job_result.trial_results:
            wrapper_path = trial_result.config.task.path
            task_name = names.get(
                str(wrapper_path.resolve()) if wrapper_path else "",
                trial_result.trial_name,
            )
            trial_dir = job.job_dir / trial_result.trial_name
            try:
                result = checker._extract_check_result(
                    trial_result, trial_dir, response_model
                )
                result.task_name = task_name
            except (ValueError, RuntimeError) as error:
                result = QualityCheckResult(task_name=task_name, error=str(error))
            results.append(result)
        results.sort(key=lambda result: result.task_name or "")
        report = CheckReport(results=results)
        (job.job_dir / REPORT_FILE).write_text(report.model_dump_json(indent=2))
        return report, job.job_dir
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def print_report(report: CheckReport, job_dir: Path) -> None:
    """Print the report with the tables of ``harbor check``."""
    from harbor.cli.analyze import _render_check_summary, _render_checks_table, console

    if len(report.results) == 1:
        result = report.results[0]
        if result.error:
            console.print(f"{result.task_name}: {result.error}")
        else:
            _render_checks_table(
                f"Task Quality Checks: {result.task_name}", result.checks
            )
    else:
        _render_check_summary(report)
    console.print(f"Report: {job_dir / REPORT_FILE}")


def static_check(path: Path) -> int:
    """Print the file problems of the tasks in ``path``. Return 1 if a task has one."""
    root = Path(path)
    if (root / TASK_FILE).is_file():
        folders = [root]
    else:
        try:
            folders = [task.path for task in read_tasks(root)]
        except FileNotFoundError as error:
            print(error, file=sys.stderr)
            return 1
    problems = [problem for folder in folders for problem in task_problems(folder)]
    for problem in problems:
        print(problem)
    print(f"{len(folders)} tasks, {len(problems)} problems")
    return 1 if problems or not folders else 0


def main(argv: Sequence[str] | None = None) -> int:
    """Check the tasks of ``argv``. Return 1 if a check has no valid result."""
    parser = argparse.ArgumentParser(prog="python -m blockether.vis.evals.check")
    parser.add_argument(
        "path", type=Path, help="a task directory or a directory of tasks"
    )
    parser.add_argument("--provider")
    parser.add_argument("--model")
    parser.add_argument("--jobs-dir", type=Path, default=Path(DEFAULT_JOBS))
    parser.add_argument("--job-name")
    parser.add_argument("--workers", type=int, default=DEFAULT_WORKERS)
    parser.add_argument(
        "--static",
        action="store_true",
        help="run only the file checks of each task, without model calls",
    )
    args = parser.parse_args(argv)
    if args.static:
        return static_check(args.path)
    if not args.provider or not args.model:
        parser.error("--provider and --model are required without --static")
    try:
        report, job_dir = asyncio.run(
            check(
                args.path,
                provider=args.provider,
                model=args.model,
                jobs_dir=args.jobs_dir,
                job_name=args.job_name,
                workers=args.workers,
            )
        )
    except (FileNotFoundError, ValueError) as error:
        print(error, file=sys.stderr)
        return 1
    print_report(report, job_dir)
    return 1 if any(result.error for result in report.results) else 0


if __name__ == "__main__":
    raise SystemExit(main())
