"""Tests for the paired comparison of `blockether.vis.evals`.

The calibration tests run seeded simulations. They prove the claims of the
evaluation guide on false verdicts and power.
"""

import random

import pytest
from blockether.vis.evals import CaseChange, RouteTest, compare_cases

ROUTE = ("vis", "zai/glm")


def _cases(passes):
    return {
        (f"case-{index:02}", *ROUTE): (passed, 2) for index, passed in enumerate(passes)
    }


def test_compare_cases_reports_a_measurable_drop():
    found = compare_cases(_cases([2] * 12), _cases([0] * 8 + [2] * 4))
    assert found.verdict == "worse"
    assert (found.paired_cases, found.baseline_rate_percent) == (12, 100.0)
    assert found.candidate_rate_percent == 33.3
    assert found.worse[0] == CaseChange("case-00 (vis with zai/glm)", "2/2", "0/2")
    assert found.better == ()
    assert found.routes == (
        RouteTest("vis with zai/glm", 12, 8, 0, 2 / 2**8, 2 / 2**8),
    )


def test_compare_cases_without_change_has_no_verdict():
    found = compare_cases(_cases([1, 2]), _cases([1, 2]))
    assert found.verdict == "no measurable difference"
    assert found.difference_points == 0.0
    assert found.p_value == 1.0


def test_compare_cases_leaves_out_unpaired_cases():
    found = compare_cases(_cases([2, 2]), _cases([2]))
    assert (found.paired_cases, found.unpaired) == (1, 1)
    assert found.caveats == (
        "1 case ran on one side only. The comparison leaves them out.",
    )


def test_compare_cases_pairs_two_single_routes_by_task():
    other = {("case-00", "vis", "zai/glm-next"): (0, 1)}
    found = compare_cases({("case-00", *ROUTE): (1, 1)}, other)
    assert found.worse == (CaseChange("case-00", "1/1", "0/1"),)
    assert found.routes[0].route == "vis with zai/glm against vis with zai/glm-next"
    assert found.caveats == (
        "The sides used different models, zai/glm and zai/glm-next, "
        "with the same agent vis.",
    )


@pytest.mark.parametrize(
    ("other", "caveat"),
    [
        (
            ("codex", "zai/glm"),
            "The sides used different agents, vis and codex, with the same model "
            "zai/glm.",
        ),
        (
            ("codex", "openai/gpt"),
            "The sides used different agents and models, vis with zai/glm and codex "
            "with openai/gpt. The change can come from either one.",
        ),
    ],
)
def test_the_route_caveat_names_only_the_parts_that_differ(other, caveat):
    found = compare_cases({("case-00", *ROUTE): (1, 1)}, {("case-00", *other): (1, 1)})
    assert found.caveats == (caveat,)


def test_compare_cases_needs_a_shared_case():
    two_routes = {("a", *ROUTE): (1, 1), ("a", "vis", "other"): (1, 1)}
    with pytest.raises(ValueError, match="more than one route"):
        compare_cases(two_routes, {("b", "x", "y"): (1, 1)}, candidate_name="run-2")


def _changes(changes):
    baseline, candidate = {}, {}
    for index, change in enumerate(changes):
        key = (f"case-{index:03}", *ROUTE)
        baseline[key] = (1 if change < 0 else 0, 1)
        candidate[key] = (1 if change > 0 else 0, 1)
    return baseline, candidate


@pytest.mark.parametrize(
    ("changes", "verdict"),
    [
        ([0] * 100, "equivalent"),
        ([0] * 4, "inconclusive"),
        ([1] * 3 + [0] * 57, "not worse"),
        ([-1] * 3 + [0] * 57, "not better"),
    ],
)
def test_a_margin_tells_an_equivalent_result_from_too_few_cases(changes, verdict):
    found = compare_cases(*_changes(changes), margin_points=10)
    assert (found.verdict, found.margin_points) == (verdict, 10)
    assert compare_cases(*_changes(changes)).verdict == "no measurable difference"


@pytest.mark.parametrize("margin", [-1, 0, 101])
def test_compare_cases_refuses_a_margin_outside_0_to_100(margin):
    with pytest.raises(ValueError, match="margin_points"):
        compare_cases(*_changes([0]), margin_points=margin)


@pytest.mark.parametrize(
    ("change", "margin", "caveat"),
    [
        (1, 15, "The gain is real, but smaller than the margin of 15 points."),
        (
            1,
            10,
            "The gain is real, but it can be smaller than the margin of 10 points.",
        ),
        (-1, 15, "The loss is real, but smaller than the margin of 15 points."),
    ],
)
def test_a_verdict_inside_the_margin_gets_a_caveat(change, margin, caveat):
    # The 95% interval of 40 changed cases in 400 is 7.0 to 12.9 points.
    found = compare_cases(*_changes([change] * 40 + [0] * 360), margin_points=margin)
    verdict = "better" if change > 0 else "worse"
    assert (found.verdict, found.caveats) == (verdict, (caveat,))


def test_a_change_beyond_the_margin_gets_no_margin_caveat():
    found = compare_cases(*_changes([1] * 120 + [0] * 280), margin_points=10)
    assert (found.verdict, found.caveats) == ("better", ())


def test_compare_cases_names_a_verdict_that_the_exact_test_does_not_confirm():
    # 12 tasks: 6 tasks became better and 1 worse, as in a small agent comparison.
    found = compare_cases(*_changes([1] * 6 + [-1] + [0] * 5))
    assert (found.verdict, found.p_value) == ("better", 0.125)
    assert found.caveats == (
        "The interval says better, but the exact test gives p = 0.125. "
        "Confirm the change with more cases.",
    )


def _simulate(rng, tasks, change, margin=None, runs=1):
    rates = [rng.uniform(0.3, 0.7) for _ in range(tasks)]

    def side(shift):
        return {
            (f"t{index}", *ROUTE): (
                sum(rng.random() < rate + shift for _ in range(runs)),
                runs,
            )
            for index, rate in enumerate(rates)
        }

    return compare_cases(side(0.0), side(change), margin_points=margin)


@pytest.mark.parametrize(
    ("tasks", "runs"), [(12, 1), (16, 1), (30, 1), (30, 3), (80, 1)]
)
def test_without_a_change_a_verdict_is_rare(tasks, runs):
    rng = random.Random(tasks * runs)
    trials = 1000
    verdicts = sum(
        _simulate(rng, tasks, 0.0, runs=runs).verdict != "no measurable difference"
        for _ in range(trials)
    )
    assert verdicts / trials <= 0.06


def test_the_detectable_change_shows_with_80_percent_power():
    rng = random.Random(3)
    floor = _simulate(random.Random(1), 40, 0.0).detectable_change_points
    trials = 600
    better = sum(
        _simulate(rng, 40, floor / 100).verdict == "better" for _ in range(trials)
    )
    assert better / trials >= 0.80


@pytest.mark.parametrize("tasks", [60, 200])
def test_a_candidate_worse_by_the_margin_is_rarely_called_not_worse(tasks):
    rng = random.Random(5)
    trials = 600
    wrong = sum(
        _simulate(rng, tasks, -0.10, margin=10).verdict
        in ("better", "equivalent", "not worse")
        for _ in range(trials)
    )
    assert wrong / trials <= 0.04
