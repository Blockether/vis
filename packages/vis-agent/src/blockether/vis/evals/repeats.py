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
from .stats import cluster_interval, detectable_change, pass_all_k, pass_at_k

__all__ = ["RepeatStats", "repeat_stats", "task_attempts"]


@dataclass(frozen=True)
class RepeatStats:
    """The statistics of the repeated attempts of a set of tasks.

    `attempts_per_task` is the most common attempt count, the larger one on a tie.
    `attempt_pass_rate_ci95` is the 95% cluster interval of the attempt pass rate over
    the tasks. A true change of `detectable_change` or more shows with 80% power.
    `pass_at_k` is the mean chance over the tasks that at least one of `k` attempts
    solves the task, and `pass_all_k` that all `k` attempts solve it. Each task gives
    the unbiased estimate from all its attempts, so a task with more than `k` attempts
    uses all of them. Only tasks with at least `k` attempts count. Both are None when no
    task has `k` attempts, or when each task has one attempt and you give no `k`.
    `unstable_tasks` names the tasks with both solved and failed attempts.
    """

    attempts_per_task: int
    k: int
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


def _mean(values: Sequence[float]) -> float | None:
    return sum(values) / len(values) if values else None


def repeat_stats(
    attempts: Mapping[str, Sequence[bool]], k: int | None = None
) -> RepeatStats:
    """Give the repeat statistics of the attempts of each task, True when solved.

    `k` is the attempt count of pass@k and pass^k. Without it, `k` is
    `attempts_per_task`. Raises `ValueError` for a `k` below 1.
    """
    if k is not None and k < 1:
        raise ValueError("k must be at least 1")
    units = [(sum(flags), len(flags)) for flags in attempts.values()]
    scored = sum(runs for _, runs in units)
    solved = sum(passed for passed, _ in units)
    counts = Counter(len(flags) for flags in attempts.values())
    per_task = max(counts, key=lambda value: (counts[value], value)) if counts else 0
    is_repeated = per_task > 1 or k is not None
    k = per_task if k is None else k
    complete = [(runs, passed) for passed, runs in units if k and runs >= k]
    is_repeated = is_repeated and bool(complete)
    return RepeatStats(
        attempts_per_task=per_task,
        k=k,
        scored_attempts=scored,
        solved_attempts=solved,
        attempt_pass_rate=solved / scored if scored else None,
        attempt_pass_rate_ci95=cluster_interval(units),
        detectable_change=detectable_change(units),
        pass_at_k=_mean([pass_at_k(n, c, k) for n, c in complete])
        if is_repeated
        else None,
        pass_all_k=_mean([pass_all_k(n, c, k) for n, c in complete])
        if is_repeated
        else None,
        unstable_tasks=tuple(
            sorted(
                task for task, flags in attempts.items() if 0 < sum(flags) < len(flags)
            )
        ),
    )
