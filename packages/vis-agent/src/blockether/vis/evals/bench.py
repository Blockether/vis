"""Score a Terminal-Bench round and find where it loses tasks.

`round_summary` reads a round summary from Harbor job folders: the trials, the task
outcomes and the usage of the round. Give that summary to `bench_report` for
the score with 95% intervals, or to `diagnose` for the failure causes, largest lever
first. Read the agent time limits of the tasks with `agent_limits`. Percent values go
from 0 to 100 and are rounded to 0.1.
"""

from __future__ import annotations

import json
import statistics
import tomllib
from collections import Counter, defaultdict
from collections.abc import Mapping
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path

from .labels import Agreement, agreement
from .repeats import repeat_stats
from .stats import wilson_interval

__all__ = [
    "CAUSES",
    "NOTE_LIMIT",
    "TrialLabel",
    "find_trial",
    "load_labels",
    "save_label",
    "EARLY_STOP_SHARE",
    "NEAR_MISS_RATIO",
    "BenchReport",
    "Diagnosis",
    "Finding",
    "OutcomeCount",
    "TaskNote",
    "agent_limits",
    "attempt_cause",
    "attempt_outcome",
    "bench_report",
    "cause_check",
    "diagnose",
    "latest_trials",
    "summary_attempts",
    "task_note",
]

EARLY_STOP_SHARE = 0.25
"""A failed attempt that used at most this share of its agent time limit stopped early."""

NEAR_MISS_RATIO = 0.8
"""A failed attempt that passed at least this share of its verifier tests is a near miss."""

CAUSES = (
    "dead_tools",
    "agent_errors",
    "agent_timeouts",
    "early_stops",
    "unscored",
    "other",
)
"""The failure causes of `attempt_cause` that a person can give with `save_label`."""

NOTE_LIMIT = 500
"""The maximum length in characters of the note of a label."""


@dataclass(frozen=True)
class OutcomeCount:
    """Tasks whose latest scored attempt ended the same way."""

    outcome: str
    count: int
    tasks: tuple[str, ...]


@dataclass(frozen=True)
class BenchReport:
    """The Terminal-Bench score of one round, with its usage.

    `pass_rate_percent` divides solved tasks by scored tasks, by the latest scored
    attempt of each task. `strict_pass_rate_percent` divides them by all dataset tasks,
    so unscored, unattempted and GPU tasks count as failures. The attempt pass rate
    counts every scored attempt, with the attempts of one task as one cluster. Each
    `_ci95` value is a 95% Wilson interval in percent. A second round of the same size
    cannot detect a change below `detectable_change_points`. `attempts_per_task` is the
    most common scored attempt count of a task. When it is more than one,
    `pass_at_k_percent` and `pass_all_k_percent` give the share of tasks that solved at
    least one or all of their first attempts, up to that count. Otherwise they are None.
    `unstable_tasks` lists tasks with mixed results. `estimated_metered_cost_usd` and
    `cost_per_solved_usd` use metered API prices. They are not subscription charges.
    """

    dataset: str
    round_name: str | None
    model: str
    total_tasks: int
    scored_tasks: int
    solved_tasks: int
    pass_rate_percent: float
    pass_rate_ci95: tuple[float, float] | None
    strict_pass_rate_percent: float
    strict_pass_rate_ci95: tuple[float, float] | None
    attempts_per_task: int
    scored_attempts: int
    solved_attempts: int
    attempt_pass_rate_percent: float
    attempt_pass_rate_ci95: tuple[float, float] | None
    detectable_change_points: float | None
    pass_at_k_percent: float | None
    pass_all_k_percent: float | None
    unstable_tasks: tuple[str, ...]
    outcomes: tuple[OutcomeCount, ...]
    unscored_tasks: tuple[str, ...]
    gpu_tasks: tuple[str, ...]
    unattempted_tasks: int
    attempts: int
    model_attempts: int
    agent_hours: float
    input_tokens: int
    cached_input_percent: float
    output_tokens: int
    estimated_metered_cost_usd: float
    cost_per_solved_usd: float | None
    repeated_final_errors: tuple[str, ...]


@dataclass(frozen=True)
class TaskNote:
    """One task in a finding.

    `checks` is passed/total verifier tests, or empty. `budget_used` is the share of
    the task's agent time limit that the agent used.
    """

    task: str
    outcome: str
    checks: str
    pass_ratio: float | None
    budget_used: float | None
    minutes: float | None
    iterations: int | None


@dataclass(frozen=True)
class Finding:
    """One problem class. `share` is the fraction of scored tasks it affects."""

    kind: str
    title: str
    task_count: int
    share: float
    detail: str
    tasks: tuple[TaskNote, ...]


@dataclass(frozen=True)
class Diagnosis:
    """Where the benchmark loses tasks, largest lever first.

    `estimate` projects the effect of full attempts on agent-error tasks. It is an
    estimate, not a measurement. `label_check` compares the causes with human labels.
    """

    scored_tasks: int
    solved_tasks: int
    findings: tuple[Finding, ...]
    estimate: str
    label_check: Agreement


@dataclass(frozen=True)
class TrialLabel:
    """A human cause for one failed attempt, kept in the labels file of its round.

    `diagnosed_cause` is the automatic `attempt_cause` of the same attempt.
    """

    task: str
    trial: str
    round_name: str | None
    cause: str
    diagnosed_cause: str
    note: str
    labelled_at: str


def _short(task: object) -> str:
    return str(task).rsplit("/", 1)[-1]


def _percent(part: float, whole: float) -> float:
    return round(100 * part / whole, 1) if whole else 0.0


def _count(number: int, noun: str) -> str:
    return f"{number} {noun}" if number == 1 else f"{number} {noun}s"


def _percent_interval(
    interval: tuple[float, float] | None,
) -> tuple[float, float] | None:
    if interval is None:
        return None
    low, high = interval
    return round(low * 100, 1), round(high * 100, 1)


def _points(change: float | None) -> float | None:
    return None if change is None else round(change * 100, 1)


def _model(summary: Mapping) -> str:
    models = Counter(
        str(trial.get("model"))
        for trial in summary.get("trials") or ()
        if trial.get("model")
    )
    return models.most_common(1)[0][0] if models else ""


def summary_attempts(summary: Mapping) -> dict[str, list[bool]]:
    """List the scored attempts of each task in finish order, True when solved."""
    attempts: dict[str, list[bool]] = defaultdict(list)
    trials = sorted(
        (trial for trial in summary.get("trials") or () if trial.get("scored")),
        key=lambda trial: str(trial.get("finished_at") or ""),
    )
    for trial in trials:
        attempts[_short(trial.get("task"))].append(trial.get("reward") == 1)
    return dict(attempts)


def latest_trials(summary: Mapping) -> dict[str, dict]:
    """Give the latest scored trial of each task, by its finish time."""
    latest: dict[str, dict] = {}
    for trial in summary.get("trials") or ():
        if not trial.get("scored"):
            continue
        task = _short(trial.get("task"))
        if str(trial.get("finished_at") or "") >= str(
            latest.get(task, {}).get("finished_at") or ""
        ):
            latest[task] = trial
    return latest


def bench_report(summary: Mapping, round_name: str | None = None) -> BenchReport:
    """Score one round from its `round_summary` data."""
    usage = summary.get("usage_totals") or {}
    outcomes = sorted(
        (summary.get("task_outcomes") or {}).items(),
        key=lambda item: (-len(item[1]), item[0]),
    )
    solved = len(summary.get("solved_tasks") or ())
    scored = int(summary.get("scored_tasks") or 0)
    total = int(summary.get("total_dataset_tasks") or 0)
    input_tokens = int(usage.get("input_tokens") or 0)
    repeats = repeat_stats(summary_attempts(summary))
    solved_attempts = repeats.solved_attempts
    cost = round(float(usage.get("estimated_metered_api_cost_usd") or 0), 2)
    return BenchReport(
        dataset=str(summary.get("dataset") or ""),
        round_name=round_name,
        model=_model(summary),
        total_tasks=total,
        scored_tasks=scored,
        solved_tasks=solved,
        pass_rate_percent=_percent(solved, scored),
        pass_rate_ci95=_percent_interval(wilson_interval(solved, scored)),
        strict_pass_rate_percent=_percent(solved, total),
        strict_pass_rate_ci95=_percent_interval(wilson_interval(solved, total)),
        attempts_per_task=repeats.attempts_per_task,
        scored_attempts=repeats.scored_attempts,
        solved_attempts=solved_attempts,
        attempt_pass_rate_percent=_percent(solved_attempts, repeats.scored_attempts),
        attempt_pass_rate_ci95=_percent_interval(repeats.attempt_pass_rate_ci95),
        detectable_change_points=_points(repeats.detectable_change),
        pass_at_k_percent=_points(repeats.pass_at_k),
        pass_all_k_percent=_points(repeats.pass_all_k),
        unstable_tasks=repeats.unstable_tasks,
        outcomes=tuple(
            OutcomeCount(outcome, len(tasks), tuple(sorted(map(_short, tasks))))
            for outcome, tasks in outcomes
        ),
        unscored_tasks=tuple(map(_short, summary.get("unscored_model_tasks") or ())),
        gpu_tasks=tuple(map(_short, summary.get("gpu_required_tasks") or ())),
        unattempted_tasks=len(summary.get("unattempted_tasks") or ()),
        attempts=int(summary.get("attempts") or 0),
        model_attempts=int(usage.get("model_attempts") or 0),
        agent_hours=round(float(usage.get("agent_hours") or 0), 1),
        input_tokens=input_tokens,
        cached_input_percent=_percent(
            int(usage.get("cached_input_tokens") or 0), input_tokens
        ),
        output_tokens=int(usage.get("output_tokens") or 0),
        estimated_metered_cost_usd=cost,
        cost_per_solved_usd=round(cost / solved_attempts, 2)
        if cost and solved_attempts
        else None,
        repeated_final_errors=tuple(
            f"{error.get('trial')}: {error.get('type')} for "
            f"{error.get('iterations')} iterations"
            for error in summary.get("repeated_final_errors") or ()
        ),
    )


def agent_limits(dataset: Path | str) -> dict[str, float]:
    """Read the agent time limit in seconds of each task of a dataset directory.

    Each task directory holds a `task.toml` with `agent.timeout_sec`. A directory that
    does not exist gives an empty map.
    """
    limits = {}
    root = Path(dataset)
    for path in root.glob("*/task.toml") if root.is_dir() else ():
        try:
            info = tomllib.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        limit = (info.get("agent") or {}).get("timeout_sec")
        if limit:
            limits[path.parent.name] = float(limit)
    return limits


def attempt_outcome(trial: Mapping) -> str:
    """Name how one attempt ended, with the task outcome rules of the round summary.

    A Harbor exception or a failed trajectory gives `agent_error:<type>`.
    """
    if not trial.get("scored"):
        return "unscored"
    if trial.get("reward") == 1:
        return "solved"
    exception = trial.get("exception_type")
    if exception == "AgentTimeoutError":
        return "agent_timeout"
    if exception:
        return f"agent_error:{exception}"
    trace = trial.get("trajectory") or {}
    if trace.get("is_failed"):
        streak = trace.get("trailing_errors") or {}
        return f"agent_error:{streak.get('type') or 'unknown'}"
    return "failed_tests"


def _dead_trials(summary: Mapping) -> set[str]:
    return {
        _short(error.get("trial"))
        for error in summary.get("repeated_final_errors") or ()
    }


def attempt_cause(trial: Mapping, summary: Mapping, limits: Mapping[str, float]) -> str:
    """Give the `diagnose` cause of one attempt, or `solved`.

    The causes are `dead_tools`, `agent_errors`, `agent_timeouts`, `unscored`,
    `early_stops` and `other`.
    """
    outcome = attempt_outcome(trial)
    if outcome == "solved":
        return outcome
    if str(trial.get("trial")) in _dead_trials(summary):
        return "dead_tools"
    if outcome.startswith("agent_error:"):
        return "agent_errors"
    if outcome == "agent_timeout":
        return "agent_timeouts"
    if outcome == "unscored":
        return "unscored"
    seconds = trial.get("agent_seconds")
    limit = limits.get(_short(trial.get("task")))
    if seconds and limit and seconds / limit <= EARLY_STOP_SHARE:
        return "early_stops"
    return "other"


def cause_check(
    summary: Mapping, labels: Mapping[str, Mapping], limits: Mapping[str, float]
) -> Agreement:
    """Compare the `diagnose` causes with the human labels of the same attempts.

    `labels` maps a trial name to a label with a `cause` value. Labels of trials that
    are not in the summary do not count.
    """
    trials = {str(trial.get("trial")): trial for trial in summary.get("trials") or ()}
    return agreement(
        (
            (
                name,
                attempt_cause(trials[name], summary, limits),
                str(label.get("cause")),
            )
            for name, label in sorted(labels.items())
            if name in trials
        ),
        source="diagnose()",
        noun="causes",
        labeller="label()",
        fix="Correct the diagnose() rules first.",
    )


def task_note(task: str, outcome: str, trial: Mapping, limit: float | None) -> TaskNote:
    """Describe one attempt of a task for a finding. `limit` is its agent time limit."""
    tests = trial.get("verifier_tests") or {}
    total = int(tests.get("tests") or 0) if isinstance(tests, dict) else 0
    passed = int(tests.get("passed") or 0) if total else 0
    seconds = trial.get("agent_seconds")
    return TaskNote(
        task=task,
        outcome=outcome,
        checks=f"{passed}/{total}" if total else "",
        pass_ratio=round(passed / total, 3) if total else None,
        budget_used=round(seconds / limit, 3) if seconds and limit else None,
        minutes=round(seconds / 60, 1) if seconds else None,
        iterations=trial.get("iterations"),
    )


def diagnose(
    summary: Mapping,
    labels: Mapping[str, Mapping] | None = None,
    limits: Mapping[str, float] | None = None,
) -> Diagnosis:
    """Find where a round loses tasks, from its `round_summary` data.

    `limits` gives the agent time limit of each task, from `agent_limits`. Without
    limits, no attempt counts as an early stop. `labels` maps a trial name to a human
    label with a `cause` value.
    """
    outcomes = {
        outcome: sorted(map(_short, tasks))
        for outcome, tasks in (summary.get("task_outcomes") or {}).items()
    }
    scored = int(summary.get("scored_tasks") or 0)
    solved = len(outcomes.get("solved", ()))
    latest = latest_trials(summary)
    limits = limits or {}

    def notes(outcome: str) -> list[TaskNote]:
        return [
            task_note(task, outcome, latest.get(task, {}), limits.get(task))
            for task in outcomes.get(outcome, ())
        ]

    def share(count: int) -> float:
        return round(count / scored, 3) if scored else 0.0

    findings = []
    errors = [
        note
        for outcome in sorted(outcomes)
        if outcome.startswith("agent_error:")
        for note in notes(outcome)
    ]
    if errors:
        kinds = Counter(note.outcome.removeprefix("agent_error:") for note in errors)
        findings.append(
            Finding(
                "agent_errors",
                f"The agent stopped with an error in {len(errors)} of {scored} scored tasks",
                len(errors),
                share(len(errors)),
                "By type: "
                + ", ".join(f"{kind} {count}" for kind, count in kinds.most_common())
                + ". These tasks did not get a full attempt. Provider failures that a "
                "retry can survive, such as a dropped stream or an output-token "
                "limit, are the cheapest score to recover.",
                tuple(errors),
            )
        )
    failed = notes("failed_tests")
    used = [note.budget_used for note in failed if note.budget_used is not None]
    early = sorted(
        (
            note
            for note in failed
            if note.budget_used is not None and note.budget_used <= EARLY_STOP_SHARE
        ),
        key=lambda note: note.budget_used or 0.0,
    )
    if early:
        findings.append(
            Finding(
                "early_stops",
                f"The agent finished early and failed the tests in {_count(len(early), 'task')}",
                len(early),
                share(len(early)),
                f"The agent ended these runs itself, without an error, after at most "
                f"{EARLY_STOP_SHARE:.0%} of the agent time limit. The median "
                f"failed-tests run used {statistics.median(used):.0%}. The agent did not "
                "check its work against the task before it finished.",
                tuple(early),
            )
        )
    timeouts = notes("agent_timeout")
    near = sorted(
        (
            note
            for note in [*failed, *errors, *timeouts]
            if note.pass_ratio is not None
            and note.pass_ratio >= NEAR_MISS_RATIO
            and note.pass_ratio < 1
        ),
        key=lambda note: -(note.pass_ratio or 0.0),
    )
    if near:
        findings.append(
            Finding(
                "near_misses",
                f"{_count(len(near), 'failed task')} passed at least "
                f"{NEAR_MISS_RATIO:.0%} of their verifier tests",
                len(near),
                share(len(near)),
                "Rewards are all or nothing, so these close runs score zero. Read "
                "their traces first. They overlap the other findings.",
                tuple(near),
            )
        )
    if timeouts:
        findings.append(
            Finding(
                "agent_timeouts",
                f"{_count(len(timeouts), 'task')} reached the agent time limit",
                len(timeouts),
                share(len(timeouts)),
                "Harbor verified them after the timeout. Look in the trace for a loop "
                "or a dead tool.",
                tuple(timeouts),
            )
        )
    repeated = summary.get("repeated_final_errors") or ()
    if repeated:
        findings.append(
            Finding(
                "dead_tools",
                f"{_count(len(repeated), 'attempt')} repeated one error until the end",
                len(repeated),
                share(len(repeated)),
                "All last iterations failed the same way, for example "
                "python-worker-retired after the sandbox Python worker exited. The "
                "model kept calling a tool that could not work, for hours.",
                tuple(
                    TaskNote(
                        _short(error.get("trial")).split("__", 1)[0],
                        str(error.get("type")),
                        "",
                        None,
                        None,
                        None,
                        error.get("iterations"),
                    )
                    for error in repeated
                ),
            )
        )
    unscored = tuple(map(_short, summary.get("unscored_model_tasks") or ()))
    if unscored:
        findings.append(
            Finding(
                "unscored",
                f"{_count(len(unscored), 'model attempt')} ended without a verifier result",
                len(unscored),
                share(len(unscored)),
                "Infrastructure stopped these attempts, for example an out-of-memory "
                "kill. Retry them in the same round. A "
                "strict score counts them as failures.",
                tuple(
                    TaskNote(task, "unscored", "", None, None, None, None)
                    for task in unscored
                ),
            )
        )
    full = solved + len(failed) + len(timeouts)
    estimate = ""
    if errors and full:
        rate = solved / full
        extra = rate * len(errors)
        estimate = (
            f"{solved} of {full} tasks with a full attempt passed ({rate:.0%}). At that "
            f"rate, full attempts on the {_count(len(errors), 'agent-error task')} would "
            f"add about {extra:.1f} solved tasks: {_percent(solved + extra, scored)}% "
            f"instead of {_percent(solved, scored)}%. This is an estimate, not a "
            "measurement."
        )
    return Diagnosis(
        scored,
        solved,
        tuple(findings),
        estimate,
        cause_check(summary, labels or {}, limits),
    )


def find_trial(summary: Mapping, task: str) -> dict:
    """Find an attempt by trial name, or the latest scored attempt of a task.

    A name with `__` is a trial name, such as `task__id`. Raises `LookupError` when the
    summary has no such attempt.
    """
    trials = summary.get("trials") or ()
    if "__" in task:
        matches = [trial for trial in trials if str(trial.get("trial")) == task]
    else:
        matches = [
            trial for trial in trials if _short(trial.get("task")) == _short(task)
        ]
    if not matches:
        raise LookupError(
            f"no attempt of {task!r} in the benchmark summary. Use a task name or a "
            "trial name such as task__id."
        )
    scored = [trial for trial in matches if trial.get("scored")] or matches
    return max(scored, key=lambda trial: str(trial.get("finished_at") or ""))


def load_labels(path: Path | str) -> dict[str, dict]:
    """Read the labels file of a round: a map from trial name to its label.

    A file that is missing or is not a JSON object gives an empty map.
    """
    try:
        data = json.loads(Path(path).read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return data if isinstance(data, dict) else {}


def save_label(
    path: Path | str,
    summary: Mapping,
    task: str,
    cause: str,
    note: str = "",
    *,
    limits: Mapping[str, float] | None = None,
    round_name: str | None = None,
) -> TrialLabel:
    """Save the human cause of one failed attempt in the labels file at `path`.

    `task` is a task name or a trial name, as in `find_trial`. `cause` is one of
    `CAUSES`. The write replaces the file at once. Raises `ValueError` for an unknown
    cause, a note longer than `NOTE_LIMIT` or a solved attempt.
    """
    if cause not in CAUSES:
        raise ValueError(f"cause must be one of {', '.join(CAUSES)}, not {cause!r}")
    if len(note) > NOTE_LIMIT:
        raise ValueError(
            f"the note has {len(note)} characters; the limit is {NOTE_LIMIT}"
        )
    trial = find_trial(summary, task)
    diagnosed = attempt_cause(trial, summary, limits or {})
    if diagnosed == "solved":
        raise ValueError(
            f"{trial.get('trial')} solved its task. Label only failed attempts."
        )
    label = TrialLabel(
        task=_short(trial.get("task")),
        trial=str(trial.get("trial")),
        round_name=round_name,
        cause=cause,
        diagnosed_cause=diagnosed,
        note=note,
        labelled_at=datetime.now(UTC).isoformat(timespec="seconds"),
    )
    labels = load_labels(path)
    labels[label.trial] = {
        "task": label.task,
        "cause": cause,
        "note": note,
        "labelled_at": label.labelled_at,
    }
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    draft = target.with_name(target.name + ".tmp")
    draft.write_text(json.dumps(labels, indent=2, sort_keys=True) + "\n", "utf-8")
    draft.replace(target)
    return label
