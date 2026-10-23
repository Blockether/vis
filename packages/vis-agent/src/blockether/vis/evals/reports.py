"""Build the full report of one Harbor round: scores, repeats, causes and agreement.

`round_report` reads one Harbor job folder or a folder of jobs. It gives the pass rates
with 95% intervals, pass@k and pass^k for each k, the outcomes, the failure causes with
the near misses, and the agreement of the causes and of a judge with human labels.
`render_report` gives the text and `write_report` the JSON file `REPORT_FILE`.
`run_tasks` writes this file in the job folder after each run, and the command
``python -m blockether.vis.evals.report JOBS`` prints it. Harbor ignores files at the
top of a job folder, so a resumed job keeps working.
"""

from __future__ import annotations

import json
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from ._gateway import plain
from .bench import (
    BenchReport,
    Diagnosis,
    bench_report,
    diagnose,
    load_labels,
    summary_attempts,
)
from .labels import KAPPA_REVIEW, MIN_LABELS
from .repeats import repeat_stats
from .rounds import round_summary
from .stages import (
    StageAgreement,
    judge_errors,
    judged_samples,
    load_stage_labels,
    stage_agreement,
)

__all__ = [
    "LABELS_FILE",
    "REPORT_FILE",
    "STAGE_LABELS_FILE",
    "RepeatRow",
    "RoundReport",
    "render_report",
    "round_report",
    "write_report",
]

REPORT_FILE = "vis-report.json"
"""The name of the report file in a job folder."""

LABELS_FILE = "labels.json"
"""The default name of the cause labels file in a job folder."""

STAGE_LABELS_FILE = "stage-labels.json"
"""The default name of the stage labels file in a judge job folder."""


@dataclass(frozen=True)
class RepeatRow:
    """pass@k and pass^k in percent for one `k`, over the tasks with `k` attempts."""

    k: int
    tasks: int
    pass_at_k_percent: float | None
    pass_all_k_percent: float | None


@dataclass(frozen=True)
class RoundReport:
    """The full report of one round.

    `score` and `diagnosis` are the `bench_report` and `diagnose` results. `repeats`
    has one row for each k from 1 to the attempts per task. `judge` compares the
    stages of a judge with human stage labels, or is None without a judge folder.
    """

    jobs: str
    score: BenchReport
    repeats: tuple[RepeatRow, ...]
    diagnosis: Diagnosis
    judge: StageAgreement | None


def _percent(value: float | None) -> float | None:
    return None if value is None else round(100 * value, 1)


def _repeats(attempts: dict[str, list[bool]], per_task: int) -> tuple[RepeatRow, ...]:
    rows = []
    for k in range(1, per_task + 1):
        stats = repeat_stats(attempts, k)
        rows.append(
            RepeatRow(
                k=k,
                tasks=sum(len(flags) >= k for flags in attempts.values()),
                pass_at_k_percent=_percent(stats.pass_at_k),
                pass_all_k_percent=_percent(stats.pass_all_k),
            )
        )
    return tuple(rows)


def round_report(
    jobs_dir: Path | str,
    dataset_dir: Path | str | None = None,
    *,
    model: str | None = None,
    round_name: str | None = None,
    labels: Path | str | None = None,
    judge_dir: Path | str | None = None,
    stage_labels: Path | str | None = None,
) -> RoundReport:
    """Build the full report of the round in `jobs_dir`.

    `dataset_dir` and `model` go to `round_summary`. `labels` is the cause labels file,
    by default `LABELS_FILE` in `jobs_dir`. `judge_dir` is the job folder of a judge.
    Without it, the report uses `jobs_dir` when it holds judge stages. `stage_labels`
    is the stage labels file, by default `STAGE_LABELS_FILE` in the judge folder.
    Raises `FileNotFoundError` when `jobs_dir` holds no trial.
    """
    jobs = Path(jobs_dir)
    summary = round_summary(jobs, dataset_dir, model=model)
    if not summary["attempts"]:
        raise FileNotFoundError(f"no Harbor trials in {jobs}")
    score = bench_report(summary, round_name or jobs.name)
    cause_labels = load_labels(jobs / LABELS_FILE if labels is None else labels)
    judge_folder = Path(judge_dir) if judge_dir is not None else jobs
    samples = judged_samples(judge_folder)
    judge = None
    if samples:
        judge = stage_agreement(
            samples,
            load_stage_labels(
                judge_folder / STAGE_LABELS_FILE
                if stage_labels is None
                else stage_labels
            ),
            judge_errors(judge_folder),
        )
    return RoundReport(
        jobs=str(jobs),
        score=score,
        repeats=_repeats(summary_attempts(summary), score.attempts_per_task),
        diagnosis=diagnose(summary, cause_labels),
        judge=judge,
    )


def write_report(report: RoundReport, path: Path | str) -> Path:
    """Write `report` as JSON to `path` and replace the old file at once."""
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    draft = target.with_name(target.name + ".tmp")
    draft.write_text(json.dumps(plain(report), indent=2) + "\n", "utf-8")
    draft.replace(target)
    return target


def _value(value: float | None, unit: str = "") -> str:
    return "n/a" if value is None else f"{value}{unit}"


def _interval(interval: Sequence[float] | None) -> str:
    return "" if interval is None else f"  95% CI {interval[0]}-{interval[1]}%"


def _agreement_lines(title: str, found, advice: str) -> list[str]:
    lines = [f"{title}: {found.labelled} labels"]
    if found.labelled:
        lines.append(
            f"  agreement {_value(found.agreement_percent, '%')}, Cohen kappa "
            f"{_value(found.kappa)}, Krippendorff alpha {_value(found.alpha)}"
        )
    lines.append(f"  {advice}")
    lines.extend(f"  differs: {item}" for item in found.disagreements[:10])
    return lines


def render_report(report: RoundReport) -> str:
    """Give the text that the command prints for `report`."""
    score = report.score
    lines = [
        f"Round {score.round_name} - dataset {score.dataset or 'unknown'}, "
        f"model {score.model or 'unknown'}",
        "",
        "Score",
        f"  Pass rate          {score.solved_tasks}/{score.scored_tasks} tasks  "
        f"{score.pass_rate_percent}%{_interval(score.pass_rate_ci95)}",
        f"  Strict pass rate   {score.solved_tasks}/{score.total_tasks} tasks  "
        f"{score.strict_pass_rate_percent}%{_interval(score.strict_pass_rate_ci95)}",
        f"  Attempt pass rate  {score.solved_attempts}/{score.scored_attempts} attempts  "
        f"{score.attempt_pass_rate_percent}%{_interval(score.attempt_pass_rate_ci95)}",
        f"  Detectable change  {_value(score.detectable_change_points, ' points')}",
        "",
        f"Repeats ({score.attempts_per_task} attempts per task)",
        "  k  tasks  pass@k  pass^k",
    ]
    lines.extend(
        f"  {row.k:<2} {row.tasks:<6} {_value(row.pass_at_k_percent, '%'):<7} "
        f"{_value(row.pass_all_k_percent, '%')}"
        for row in report.repeats
    )
    if score.unstable_tasks:
        lines.append(f"  unstable: {', '.join(score.unstable_tasks)}")
    lines += ["", "Outcomes"]
    lines.extend(
        f"  {outcome.outcome}: {outcome.count} ({', '.join(outcome.tasks)})"
        for outcome in score.outcomes
    )
    lines += ["", "Failure causes"]
    if not report.diagnosis.findings:
        lines.append("  no rule matched")
    for finding in report.diagnosis.findings:
        tasks = ", ".join(note.task for note in finding.tasks)
        lines.append(f"  {finding.kind}: {finding.title} ({tasks})")
    if report.diagnosis.estimate:
        lines.append(f"  {report.diagnosis.estimate}")
    check = report.diagnosis.label_check
    lines += [""] + _agreement_lines("Cause labels", check, check.advice)
    if report.judge is not None:
        judge = report.judge
        lines += [
            "",
            f"Judge: {judge.judged} judged trials, {judge.errors} errors",
            f"  sample agreement {_value(judge.sample_agreement_percent, '%')}, "
            f"Krippendorff alpha {_value(judge.sample_alpha)}",
            f"  {judge.sample_advice}",
        ]
        lines += _agreement_lines("Stage labels", judge, judge.advice)
    lines += [
        "",
        "Usage",
        f"  {score.attempts} attempts, {score.agent_hours} agent hours, "
        f"{score.input_tokens} input tokens ({score.cached_input_percent}% cached), "
        f"{score.output_tokens} output tokens",
        f"  estimated metered cost {score.estimated_metered_cost_usd} USD, per solved "
        f"attempt {_value(score.cost_per_solved_usd, ' USD')}",
    ]
    if check.labelled < MIN_LABELS or check.kappa is None or check.kappa < KAPPA_REVIEW:
        lines.append("")
        lines.append(
            "Read the trajectories of failed attempts before you act on the causes."
        )
    return "\n".join(lines) + "\n"
