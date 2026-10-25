"""Paired comparison of two evaluation results over the cases that both ran.

A case is a key such as `(task, agent, model)` from `case_results`, with its
`(passed, runs)` count. A route is the agent with the model of a case. Call
`compare_cases` with the cases of a baseline and a candidate. It pairs the cases,
gives the paired difference with its 95% interval and tests each route. Give
`margin_points`, the smallest change that matters to you, to tell an equivalent
result from a result that needs more cases.
"""

from __future__ import annotations

from collections import defaultdict
from collections.abc import Mapping
from dataclasses import dataclass

from .stats import benjamini_hochberg, mcnemar_exact, paired_change

__all__ = ["CaseChange", "CaseComparison", "RouteTest", "compare_cases"]

Cases = Mapping[tuple[str, ...], tuple[int, int]]


@dataclass(frozen=True)
class CaseChange:
    """One case whose pass rate changed between the two sides, as passed/runs."""

    case: str
    baseline: str
    candidate: str


@dataclass(frozen=True)
class RouteTest:
    """The exact McNemar test of the paired cases of one route.

    `worse` and `better` count the cases whose pass rate fell or rose. `p_value` is the
    two-sided exact p-value. `q_value` is its Benjamini-Hochberg value over all routes
    of the comparison.
    """

    route: str
    paired_cases: int
    worse: int
    better: int
    p_value: float
    q_value: float


@dataclass(frozen=True)
class CaseComparison:
    """A paired comparison of the cases that both sides ran.

    The rates are the mean pass rates of the paired cases, in percent.
    `difference_ci95` is the Agresti-Min 95% interval of the paired difference, in
    points. `verdict` is `better` or `worse` only when that interval excludes zero.
    Without `margin_points`, every other result is `no measurable difference`. With
    it, the position of the interval against the margin gives the verdict:

    - `equivalent`: the interval is inside the margin on both sides.
    - `not worse`: the interval is above the negative margin.
    - `not better`: the interval is below the margin, but reaches the negative margin.
    - `inconclusive`: the interval reaches both sides of the margin.

    At this case count, a true change of `detectable_change_points` or more shows with
    80% power. `p_value` is the two-sided exact McNemar p-value of the changed cases.
    With repeats, it is the exact sign test of the case changes. `routes` holds this
    test for each route. `worse` and `better` list the changed cases, largest change
    first. `unpaired` counts cases on one side only; the comparison leaves them out. A
    caveat names a verdict that the exact test does not confirm.
    """

    paired_cases: int
    baseline_rate_percent: float
    candidate_rate_percent: float
    difference_points: float
    difference_ci95: tuple[float, float]
    verdict: str
    detectable_change_points: float
    p_value: float
    routes: tuple[RouteTest, ...]
    worse: tuple[CaseChange, ...]
    better: tuple[CaseChange, ...]
    unpaired: int
    caveats: tuple[str, ...]
    margin_points: float | None = None


def _route(key: tuple[str, ...]) -> str:
    return f"{key[1]} with {key[2]}"


def _case_name(key: tuple[str, ...]) -> str:
    return key[0] if len(key) == 1 else f"{key[0]} ({_route(key)})"


def _points(change: float) -> float:
    return round(change * 100, 1)


def _verdict(interval: tuple[float, float], margin: float | None) -> str:
    low, high = interval
    if low > 0:
        return "better"
    if high < 0:
        return "worse"
    if not margin:
        return "no measurable difference"
    if -margin < low and high < margin:
        return "equivalent"
    if low > -margin:
        return "not worse"
    if high < margin:
        return "not better"
    return "inconclusive"


def _route_tests(
    changes: Mapping[tuple[str, ...], float], crossed: str | None
) -> tuple[RouteTest, ...]:
    groups: dict[str, list[float]] = defaultdict(list)
    for key, change in changes.items():
        groups[crossed or _route(key)].append(change)
    counts = [
        (
            route,
            len(values),
            sum(value < 0 for value in values),
            sum(value > 0 for value in values),
        )
        for route, values in sorted(groups.items())
    ]
    p_values = [mcnemar_exact(worse, better) for _, _, worse, better in counts]
    return tuple(
        RouteTest(route, cases, worse, better, p_value, q_value)
        for (route, cases, worse, better), p_value, q_value in zip(
            counts, p_values, benjamini_hochberg(p_values), strict=True
        )
    )


def compare_cases(
    baseline: Cases,
    candidate: Cases,
    *,
    baseline_name: str = "baseline",
    candidate_name: str = "candidate",
    margin_points: float | None = None,
) -> CaseComparison:
    """Compare the `(passed, runs)` cases of a baseline and a candidate.

    Each key is `(task, agent, model)`. When the sides share no key and each side has
    one route, the comparison pairs the cases by task and adds a caveat. The names
    appear in error messages. `margin_points` is the smallest change in points that
    matters, from 0 to 100. Raise ValueError when no case pairs, or for a margin
    outside 0 to 100.
    """
    if margin_points is not None and not 0 <= margin_points <= 100:
        raise ValueError("margin_points must be from 0 to 100")
    caveats = []
    crossed = None
    if not baseline.keys() & candidate.keys():
        base_routes = {_route(key) for key in baseline}
        other_routes = {_route(key) for key in candidate}
        if len(base_routes) != 1 or len(other_routes) != 1:
            raise ValueError(
                f"{baseline_name} and {candidate_name} share no case, and one of them "
                "has more than one route. Compare results with the same agent and model."
            )
        crossed = f"{min(base_routes)} against {min(other_routes)}"
        baseline = {key[:1]: value for key, value in baseline.items()}
        candidate = {key[:1]: value for key, value in candidate.items()}
        caveats.append(
            f"The sides used different routes, {min(base_routes)} and "
            f"{min(other_routes)}. Model and harness differ together."
        )
    paired = sorted(baseline.keys() & candidate.keys())
    if not paired:
        raise ValueError(f"{baseline_name} and {candidate_name} share no case")
    changes = {}
    changed = []
    for key in paired:
        (base_passed, base_runs), (passed, runs) = baseline[key], candidate[key]
        change = passed / runs - base_passed / base_runs
        changes[key] = change
        if change:
            row = CaseChange(
                _case_name(key), f"{base_passed}/{base_runs}", f"{passed}/{runs}"
            )
            changed.append((change, row))
    (low, high), floor = paired_change(list(changes.values()))
    interval = (_points(low), _points(high))
    verdict = _verdict(interval, margin_points)
    unpaired = len(baseline.keys() ^ candidate.keys())
    if unpaired:
        cases = "1 case" if unpaired == 1 else f"{unpaired} cases"
        caveats.append(f"{cases} ran on one side only. The comparison leaves them out.")
    count = len(paired)
    values = list(changes.values())
    p_value = mcnemar_exact(
        sum(value < 0 for value in values), sum(value > 0 for value in values)
    )
    if verdict in ("better", "worse") and p_value >= 0.05:
        caveats.append(
            f"The interval says {verdict}, but the exact test gives p = "
            f"{p_value:.3f}. Confirm the change with more cases."
        )

    def rate(cases: Cases) -> float:
        return round(
            100 * sum(cases[key][0] / cases[key][1] for key in paired) / count, 1
        )

    return CaseComparison(
        paired_cases=count,
        baseline_rate_percent=rate(baseline),
        candidate_rate_percent=rate(candidate),
        difference_points=round(100 * sum(values) / count, 1),
        difference_ci95=interval,
        verdict=verdict,
        detectable_change_points=_points(floor),
        p_value=p_value,
        routes=_route_tests(changes, crossed),
        worse=tuple(
            row for change, row in sorted(changed, key=lambda i: i[0]) if change < 0
        ),
        better=tuple(
            row for change, row in sorted(changed, key=lambda i: -i[0]) if change > 0
        ),
        unpaired=unpaired,
        caveats=tuple(caveats),
        margin_points=margin_points,
    )
