"""Judge Harbor trials with a rubric and the Vis agent.

Run it with ``uv run --project e2e python -m vis_harbor.judge PATH``. ``PATH``
is one trial directory or a Harbor job directory, for example a run of
``python -m vis_harbor.run``. Each judgement makes paid model calls.

``harbor analyze`` runs Claude Code in Docker. This module uses the same prompt,
result validation and ``analysis.json`` file of Harbor. The Vis agent runs each
judgement in the host environment, so it needs no Docker and no Anthropic key.
The judged trial is in ``trial/`` and its task in ``task/`` of the workspace.
The rubric checks the stages of the run in order: understand, write and verify.
The first failed stage is the failure stage of the trial. The job directory
holds ``judge_report.json`` and the stage of each sample in ``judge_stages.json``.
"""

from __future__ import annotations

import argparse
import asyncio
import json
import shutil
import sys
import tempfile
from collections.abc import Mapping, Sequence
from datetime import UTC, datetime
from pathlib import Path

from harbor.analyze import analyzer
from harbor.analyze.models import (
    AnalyzeReport,
    AnalyzeReportResult,
    build_analyze_response_model,
    load_rubric,
)

from vis_harbor.agent import DEFAULT_MODEL, DEFAULT_PROVIDER
from vis_harbor.check import job_config
from vis_harbor.environment import WORKSPACE_DIRECTORY
from vis_harbor.run import DEFAULT_WORKERS

RUBRIC = Path(__file__).with_name("judge-rubric.toml")
STAGES = ("understand", "write", "verify")
OTHER_STAGE = "other"
DEFAULT_JOBS = "/tmp/vis_e2e/judge"
REPORT_FILE = "judge_report.json"
STAGES_FILE = "judge_stages.json"
CONTAINER_WORKDIR = "/app/"
TEST_SCRIPT = """#!/usr/bin/env bash
# Validate the analysis. The host environment sets the VIS_E2E_* paths.
if "$VIS_E2E_PYTHON" "$VIS_E2E_TESTS/validate.py" analysis.json; then
  echo 1 > "$VIS_E2E_VERIFIER_LOGS/reward.txt"
else
  echo 0 > "$VIS_E2E_VERIFIER_LOGS/reward.txt"
fi
"""


def failure_stage(checks: Mapping[str, object]) -> str:
    """Return the first stage of ``STAGES`` whose check failed, else ``OTHER_STAGE``.

    A check is a ``QualityCheckModel`` of a Harbor report or its JSON object.
    """
    for stage in STAGES:
        check = checks.get(stage)
        outcome = (
            check.get("outcome")
            if isinstance(check, Mapping)
            else getattr(check, "outcome", None)
        )
        if getattr(outcome, "value", outcome) == "fail":
            return stage
    return OTHER_STAGE


def assemble(trial_dir: Path, rubric, response_model, dest: Path) -> Path:
    """Build the judge task of ``trial_dir`` in ``dest`` for the host environment.

    Harbor's wrapper puts the trial and its task under the container workdir
    ``/app``. The host environment translates only commands, so the trial and
    the task move into the workspace, and the instruction names relative paths.
    """
    wrapper = analyzer.assemble_analyze_task(
        trial_dir=trial_dir,
        task_dir=analyzer._extract_task_dir(trial_dir),
        rubric=rubric,
        template=(analyzer.PROMPTS_DIR / "analyze.txt").read_text(),
        output_schema=response_model.model_json_schema(),
        dest=dest,
    )
    environment = wrapper / "environment"
    workspace = environment / WORKSPACE_DIRECTORY
    workspace.mkdir()
    for name in ("trial", "task"):
        if (environment / name).exists():
            (environment / name).rename(workspace / name)
    instruction = wrapper / "instruction.md"
    instruction.write_text(instruction.read_text().replace(CONTAINER_WORKDIR, ""))
    script = wrapper / "tests" / "test.sh"
    script.write_text(TEST_SCRIPT)
    script.chmod(0o755)
    return wrapper


def stages(report: AnalyzeReport) -> dict[str, list[str]]:
    """Return the failure stage of each valid sample, grouped by trial."""
    grouped: dict[str, list[str]] = {}
    for result in report.results:
        if result.trial_name and not result.error:
            grouped.setdefault(result.trial_name, []).append(
                failure_stage(result.checks)
            )
    return grouped


async def judge(
    path: Path,
    provider: str = DEFAULT_PROVIDER,
    model: str = DEFAULT_MODEL,
    jobs_dir: Path = Path(DEFAULT_JOBS),
    job_name: str | None = None,
    workers: int = DEFAULT_WORKERS,
    samples: int = 1,
    failing: bool = True,
) -> tuple[AnalyzeReport, Path]:
    """Judge the trials in ``path``. Return the report and the job directory.

    ``failing`` keeps only failed trials. ``samples`` judges each trial that
    many times; the report then holds one result for each sample.
    """
    from harbor.job import Job

    trial_dirs = analyzer._resolve_trial_dirs(
        Path(path), False if failing else None, None
    )
    rubric = load_rubric(RUBRIC)
    response_model = build_analyze_response_model(rubric)
    name = job_name or datetime.now(UTC).strftime("judge-%Y%m%d-%H%M%S")
    tmp = Path(tempfile.mkdtemp(prefix="vis-harbor-judge-"))
    try:
        sources = {}
        wrappers = []
        for trial_dir in trial_dirs:
            wrapper = assemble(
                trial_dir, rubric, response_model, tmp / f"judge-{trial_dir.name}"
            )
            wrappers.append(wrapper)
            sources[str(wrapper.resolve())] = trial_dir
        config = job_config(wrappers, provider, model, Path(jobs_dir), name, workers)
        config.n_attempts = samples
        job = await Job.create(config)
        job_result = await job.run()
        results = []
        for trial_result in job_result.trial_results:
            wrapper_path = trial_result.config.task.path
            source = sources.get(str(wrapper_path.resolve()) if wrapper_path else "")
            trial_name = source.name if source else trial_result.trial_name
            trial_dir = job.job_dir / trial_result.trial_name
            try:
                result = analyzer._extract_analyze_result(
                    trial_result, trial_dir, response_model
                )
                result.trial_name = trial_name
                if source is not None and samples == 1:
                    analyzer._write_analysis_json(source, result)
            except (ValueError, RuntimeError) as error:
                result = AnalyzeReportResult(trial_name=trial_name, error=str(error))
            results.append(result)
        results.sort(key=lambda result: result.trial_name or "")
        report = AnalyzeReport(results=results)
        (job.job_dir / REPORT_FILE).write_text(report.model_dump_json(indent=2))
        (job.job_dir / STAGES_FILE).write_text(json.dumps(stages(report), indent=2))
        return report, job.job_dir
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def main(argv: Sequence[str] | None = None) -> int:
    """Judge the trials of ``argv``. Return 1 if a judgement has no valid result."""
    parser = argparse.ArgumentParser(prog="python -m vis_harbor.judge")
    parser.add_argument("path", type=Path, help="a trial or a Harbor job directory")
    parser.add_argument("--provider", default=DEFAULT_PROVIDER)
    parser.add_argument("--model", default=DEFAULT_MODEL)
    parser.add_argument("--jobs-dir", type=Path, default=Path(DEFAULT_JOBS))
    parser.add_argument("--job-name")
    parser.add_argument("--workers", type=int, default=DEFAULT_WORKERS)
    parser.add_argument("--samples", type=int, default=1)
    parser.add_argument(
        "--all", action="store_true", help="also judge the trials that passed"
    )
    args = parser.parse_args(argv)
    try:
        report, job_dir = asyncio.run(
            judge(
                args.path,
                provider=args.provider,
                model=args.model,
                jobs_dir=args.jobs_dir,
                job_name=args.job_name,
                workers=args.workers,
                samples=args.samples,
                failing=not args.all,
            )
        )
    except (FileNotFoundError, ValueError) as error:
        print(error, file=sys.stderr)
        return 1
    for result in report.results:
        if result.error:
            print(f"{result.trial_name}: {result.error}")
        else:
            print(f"{result.trial_name}: {failure_stage(result.checks)}")
    print(f"Report: {job_dir / REPORT_FILE}")
    return 1 if any(result.error for result in report.results) else 0


if __name__ == "__main__":
    raise SystemExit(main())
