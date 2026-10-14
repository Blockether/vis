"""Statistics of repeated attempts: pass@k, pass^k and unstable tasks.

Run each task more than once to see how stable an agent is. Give `repeat_stats` the
attempts of each task in finish order, True when solved. With Harbor trials, get
these attempts from `task_attempts`. Each rate and interval is a fraction from 0 to 1.
"""

from __future__ import annotations

from collections import Counter, defaultdict
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass

from .harbor import HarborTrial
from .stats import cluster_interval, detectable_change

__all__ = ["RepeatStats", "repeat_stats", "task_attempts"]


@dataclass(frozen=True)
class RepeatStats:
    """The statistics of the repeated attempts of a set of tasks.

    `attempts_per_task` is the most common attempt count, the larger one on a tie.
    `attempt_pass_rate_ci95` is the 95% cluster interval of the attempt pass rate over
    the tasks. A true change of `detectable_change` or more shows with 80% power.
    `pass_at_k` is the share of tasks with a solved attempt among their first
    `attempts_per_task` attempts, and `pass_all_k` the share with all of them solved.
    Both are None with one attempt per task, and count only tasks with enough
    attempts. `unstable_tasks` names the tasks with both solved and failed attempts.
    """

    attempts_per_task: int
    scored_attempts: int
    solved_attempts: int
    attempt_pass_rate: float | None
    attempt_pass_rate_ci95: tuple[float, float] | None
    detectable_change: float | None
    pass_at_k: float | None
    pass_all_k: float | None
    unstable_tasks: tuple[str, ...]


def task_attempts(trials: Iterable[HarborTrial]) -> dict[str, list[bool]]:
    """List the scored attempts of each task in finish order, True when passed."""
    attempts: dict[str, list[bool]] = defaultdict(list)
    scored = sorted(
        (trial for trial in trials if trial.is_scored),
        key=lambda trial: trial.finished_at or "",
    )
    for trial in scored:
        attempts[trial.task].append(trial.is_passed)
    return dict(attempts)


def repeat_stats(attempts: Mapping[str, Sequence[bool]]) -> RepeatStats:
    """Give the repeat statistics of the attempts of each task, True when solved."""
    units = [(sum(flags), len(flags)) for flags in attempts.values()]
    scored = sum(runs for _, runs in units)
    solved = sum(passed for passed, _ in units)
    counts = Counter(len(flags) for flags in attempts.values())
    per_task = max(counts, key=lambda value: (counts[value], value)) if counts else 0
    complete = [
        flags[:per_task] for flags in attempts.values() if len(flags) >= per_task
    ]
    is_repeated = per_task > 1 and bool(complete)
    return RepeatStats(
        attempts_per_task=per_task,
        scored_attempts=scored,
        solved_attempts=solved,
        attempt_pass_rate=solved / scored if scored else None,
        attempt_pass_rate_ci95=cluster_interval(units),
        detectable_change=detectable_change(units),
        pass_at_k=sum(map(any, complete)) / len(complete) if is_repeated else None,
        pass_all_k=sum(map(all, complete)) / len(complete) if is_repeated else None,
        unstable_tasks=tuple(
            sorted(
                task for task, flags in attempts.items() if 0 < sum(flags) < len(flags)
            )
        ),
    )
