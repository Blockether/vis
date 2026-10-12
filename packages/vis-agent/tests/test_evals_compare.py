"""Tests for the paired comparison of `blockether.vis.evals`."""

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
    assert found.caveats[0].startswith("The sides used different routes")


def test_compare_cases_needs_a_shared_case():
    two_routes = {("a", *ROUTE): (1, 1), ("a", "vis", "other"): (1, 1)}
    with pytest.raises(ValueError, match="more than one route"):
        compare_cases(two_routes, {("b", "x", "y"): (1, 1)}, candidate_name="run-2")
