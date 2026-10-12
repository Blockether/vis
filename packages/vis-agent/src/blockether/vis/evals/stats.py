"""Statistics for evaluation results: intervals, power, paired tests and multiple tests.

Each rate, interval and change is a fraction from 0 to 1. A unit is the
`(passed, runs)` count of one case, for example one task on one route. The repeats of
one unit are not independent, so the cluster interval and the power use an effective
size between the unit count and the run count.
"""

from __future__ import annotations

import math
from collections import Counter
from collections.abc import Hashable, Sequence

__all__ = [
    "Z95",
    "Z_POWER",
    "benjamini_hochberg",
    "cluster_interval",
    "cohen_kappa",
    "detectable_change",
    "effective_size",
    "mcnemar_exact",
    "paired_change",
    "wilson_interval",
]

Z95 = 1.959964
"""The normal quantile of a two-sided test at the 5% level."""

Z_POWER = 0.841621
"""The normal quantile of 80% power."""


def wilson_interval(
    passed: int, runs: int, size: float | None = None
) -> tuple[float, float] | None:
    """Return the 95% Wilson interval of a pass rate, or None without runs.

    `size` replaces the run count when the runs are not independent.
    """
    size = runs if size is None else size
    if not runs or size <= 0:
        return None
    rate = passed / runs
    spread = Z95**2 / size
    center = (rate + spread / 2) / (1 + spread)
    half = Z95 * math.sqrt(rate * (1 - rate) / size + spread / size / 4) / (1 + spread)
    return max(0.0, center - half), min(1.0, center + half)


def effective_size(units: Sequence[tuple[int, int]]) -> float:
    """Return the run count of clustered results, made smaller by their design effect.

    Each unit is `(passed, runs)` of one case. The size stays between the unit count
    and the run count. Without any variation, it is the unit count.
    """
    units = [(passed, runs) for passed, runs in units if runs]
    count = len(units)
    total = sum(runs for _, runs in units)
    if count < 2 or total == count:
        return float(total)
    rate = sum(passed for passed, _ in units) / total
    if rate in (0, 1):
        return float(count)
    spread = sum((passed - rate * runs) ** 2 for passed, runs in units)
    variance = count / (count - 1) * spread / total**2
    if not variance:
        return float(total)
    return min(float(total), max(float(count), rate * (1 - rate) / variance))


def cluster_interval(units: Sequence[tuple[int, int]]) -> tuple[float, float] | None:
    """Return the 95% interval of the pooled pass rate of clustered units, or None."""
    passed = sum(passed for passed, _ in units)
    runs = sum(runs for _, runs in units)
    return wilson_interval(passed, runs, effective_size(units))


def detectable_change(units: Sequence[tuple[int, int]]) -> float | None:
    """Return the smallest change of the pass rate that a second run of this size detects.

    The test compares two independent runs of the same size, two-sided at the 5% level,
    with 80% power. The change has steps of 0.001. None means that there are no runs, or
    that no change from 0 to 1 is detectable.
    """
    total = sum(runs for _, runs in units)
    if not total:
        return None
    rate = sum(passed for passed, _ in units) / total
    size = effective_size(units)

    def detects(other: float) -> bool:
        pooled = (rate + other) / 2
        noise = Z95 * math.sqrt(2 * pooled * (1 - pooled) / size)
        power = Z_POWER * math.sqrt((rate * (1 - rate) + other * (1 - other)) / size)
        return abs(other - rate) >= noise + power

    smallest = None
    for sign in (1, -1):
        for step in range(1, 1001):
            other = rate + sign * step / 1000
            if not 0 <= other <= 1:
                break
            if detects(other):
                smallest = step if smallest is None else min(smallest, step)
                break
    return smallest / 1000 if smallest is not None else None


def paired_change(changes: Sequence[float]) -> tuple[tuple[float, float], float]:
    """Return the Agresti-Min 95% interval of a mean paired change, and its floor.

    Each change is the candidate pass rate of one case minus its baseline pass rate.
    The floor is the smallest true change that this many cases detect with 80% power.
    """
    count = len(changes)
    mean = sum(changes) / (count + 2)
    square = (sum(change * change for change in changes) + 1) / (count + 2)
    error = math.sqrt(max(0.0, square - mean * mean) / (count + 2))
    return (mean - Z95 * error, mean + Z95 * error), (Z95 + Z_POWER) * error


def cohen_kappa(pairs: Sequence[tuple[Hashable, Hashable]]) -> float | None:
    """Return Cohen's kappa of two raters over the same items, or None when undefined."""
    if not pairs:
        return None
    count = len(pairs)
    observed = sum(first == second for first, second in pairs) / count
    firsts = Counter(first for first, _ in pairs)
    seconds = Counter(second for _, second in pairs)
    expected = sum(firsts[label] * seconds[label] for label in firsts) / count**2
    if expected >= 1:
        return None
    return (observed - expected) / (1 - expected)


def mcnemar_exact(worse: int, better: int) -> float:
    """Return the two-sided exact McNemar p-value of paired pass and fail results.

    `worse` counts the cases that pass only in the baseline. `better` counts the cases
    that pass only in the candidate. Cases with the same result on both sides do not
    change the value. Without a changed case, the value is 1.
    """
    if worse < 0 or better < 0:
        raise ValueError("worse and better must not be negative")
    total = worse + better
    if not total:
        return 1.0
    tail = sum(math.comb(total, count) for count in range(min(worse, better) + 1))
    return min(1.0, 2 * tail / 2**total)


def benjamini_hochberg(p_values: Sequence[float]) -> list[float]:
    """Return the Benjamini-Hochberg q-value of each p-value, in the input order.

    A q-value is the smallest false discovery rate at which its test is a discovery.
    A larger p-value never gets a smaller q-value, and each q-value is at most 1.
    """
    if any(not 0 <= value <= 1 for value in p_values):
        raise ValueError("each p-value must be from 0 to 1")
    count = len(p_values)
    order = sorted(range(count), key=lambda index: p_values[index])
    q_values = [1.0] * count
    smallest = 1.0
    for rank in range(count, 0, -1):
        index = order[rank - 1]
        smallest = min(smallest, p_values[index] * count / rank)
        q_values[index] = smallest
    return q_values
