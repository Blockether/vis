"""Statistics for evaluation results: intervals, power, pass@k, paired tests and agreement.

Each rate, interval and change is a fraction from 0 to 1. A unit is the
`(passed, runs)` count of one case, for example one task on one route. The repeats of
one unit are not independent, so the cluster interval and the power use an effective
size between the unit count and the run count.

References:

- Miller, "Adding Error Bars to Evals", 2024, arXiv:2411.00640: cluster intervals,
  paired changes and power.
- Chen et al., "Evaluating Large Language Models Trained on Code", 2021,
  arXiv:2107.03374: the unbiased pass@k estimator.
- Yao et al., "tau-bench", 2024, arXiv:2406.12045: pass^k.
- Cohen, "A Coefficient of Agreement for Nominal Scales", 1960.
- Krippendorff, "Computing Krippendorff's Alpha-Reliability", 2011.
"""

from __future__ import annotations

import math
from collections import Counter, defaultdict
from collections.abc import Hashable, Iterable, Sequence

__all__ = [
    "Z95",
    "Z_POWER",
    "benjamini_hochberg",
    "cluster_interval",
    "cohen_kappa",
    "detectable_change",
    "effective_size",
    "krippendorff_alpha",
    "mcnemar_exact",
    "paired_change",
    "pass_all_k",
    "pass_at_k",
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
    with 80% power. The change holds for a rise and for a fall, so the larger of the two
    applies. A direction without a detectable change before 0 or 1 does not count. The
    change has steps of 0.001. None means that there are no runs, or that no change
    from 0 to 1 is detectable.
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

    steps = []
    for sign in (1, -1):
        for step in range(1, 1001):
            other = rate + sign * step / 1000
            if not 0 <= other <= 1:
                break
            if detects(other):
                steps.append(step)
                break
    return max(steps) / 1000 if steps else None


def paired_change(changes: Sequence[float]) -> tuple[tuple[float, float], float]:
    """Return the Agresti-Min 95% interval of a mean paired change, and its floor.

    Each change is the candidate pass rate of one case minus its baseline pass rate.
    The floor is the smallest true change that this many cases detect with 80% power.
    The interval stays inside the possible changes, from -1 to 1.
    """
    count = len(changes)
    mean = sum(changes) / (count + 2)
    square = (sum(change * change for change in changes) + 1) / (count + 2)
    error = math.sqrt(max(0.0, square - mean * mean) / (count + 2))
    low = max(-1.0, mean - Z95 * error)
    high = min(1.0, mean + Z95 * error)
    return (low, high), (Z95 + Z_POWER) * error


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


def krippendorff_alpha(units: Iterable[Sequence[Hashable | None]]) -> float | None:
    """Return Krippendorff's alpha of nominal values, or None when it is undefined.

    Each unit holds the values that the raters gave one item. None is a missing value.
    A unit with fewer than two values does not count. Alpha is 1 for full agreement, 0
    for agreement by chance, and below 0 for less agreement than chance. It is
    undefined without two values in one unit, or when all values are the same.
    """
    pairs: defaultdict[tuple[Hashable, Hashable], float] = defaultdict(float)
    for unit in units:
        values = Counter(value for value in unit if value is not None)
        size = sum(values.values())
        if size < 2:
            continue
        for first, count in values.items():
            for second, other in values.items():
                same = 1 if first == second else 0
                pairs[first, second] += count * (other - same) / (size - 1)
    totals: defaultdict[Hashable, float] = defaultdict(float)
    for (first, _), weight in pairs.items():
        totals[first] += weight
    total = sum(totals.values())
    if total <= 1:
        return None
    observed = sum(weight for (a, b), weight in pairs.items() if a != b)
    expected = sum(totals[a] * totals[b] for a in totals for b in totals if a != b) / (
        total - 1
    )
    if not expected:
        return None
    return 1 - observed / expected


def _check_counts(runs: int, passed: int, k: int) -> None:
    if not 0 <= passed <= runs:
        raise ValueError("passed must be from 0 to runs")
    if not 0 < k <= runs:
        raise ValueError("k must be from 1 to runs")


def pass_at_k(runs: int, passed: int, k: int) -> float:
    """Return the unbiased estimate of pass@k from `passed` solved of `runs` attempts.

    pass@k is the chance that at least one of `k` attempts solves the task. The estimate
    uses all `runs` attempts, not only the first `k`. Raises `ValueError` unless
    `0 <= passed <= runs` and `1 <= k <= runs`.
    """
    _check_counts(runs, passed, k)
    if runs - passed < k:
        return 1.0
    return 1 - math.comb(runs - passed, k) / math.comb(runs, k)


def pass_all_k(runs: int, passed: int, k: int) -> float:
    """Return the unbiased estimate of pass^k from `passed` solved of `runs` attempts.

    pass^k is the chance that all of `k` attempts solve the task. The limits of
    `pass_at_k` apply.
    """
    _check_counts(runs, passed, k)
    return math.comb(passed, k) / math.comb(runs, k)


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
