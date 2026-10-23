"""Tests for the statistics of `blockether.vis.evals`.

The reference values come from SciPy `binomtest`, statsmodels `multipletests`,
scikit-learn `cohen_kappa_score`, the PyPI `krippendorff` package, the worked example of
Krippendorff (2011) and the pass@k formula of Chen et al. (2021).
"""

import pytest
from blockether.vis.evals import (
    benjamini_hochberg,
    cluster_interval,
    cohen_kappa,
    detectable_change,
    effective_size,
    krippendorff_alpha,
    mcnemar_exact,
    paired_change,
    pass_all_k,
    pass_at_k,
    wilson_interval,
)


def _points(interval):
    return tuple(round(value * 100, 1) for value in interval)


def test_wilson_interval_matches_statistics_tables():
    assert _points(wilson_interval(2, 6)) == (9.7, 70.0)
    assert _points(wilson_interval(0, 10)) == (0.0, 27.8)
    assert wilson_interval(0, 0) is None
    # A smaller effective size gives a wider interval.
    low, high = wilson_interval(5, 10, size=5)
    assert low < wilson_interval(5, 10)[0] and high > wilson_interval(5, 10)[1]


def test_clustered_repeats_count_between_the_cases_and_the_runs():
    # Repeats that always agree count as one run; mixed repeats count in full.
    assert effective_size([(2, 2), (0, 2)]) == 2.0
    assert effective_size([(1, 2), (1, 2)]) == 4.0
    assert _points(cluster_interval([(2, 2), (0, 2)])) == _points(
        wilson_interval(2, 4, size=2)
    )
    assert cluster_interval([]) is None


def test_power_gives_the_smallest_detectable_change():
    # 100 independent runs at 50% detect a change of about 20 points with 80% power.
    assert detectable_change([(1, 1)] * 50 + [(0, 1)] * 50) == pytest.approx(0.194)
    assert detectable_change([]) is None


def test_paired_change_gives_the_agresti_min_interval_and_its_floor():
    # 3 of 10 paired cases broke, but the interval still includes zero.
    (low, high), floor = paired_change([-1.0] * 3 + [0.0] * 7)
    assert _points((low, high, floor)) == (-54.4, 4.4, 42.1)


def test_paired_change_keeps_the_interval_inside_the_possible_changes():
    # 39 of 40 paired cases broke in a real e2e comparison. The unbounded interval
    # started at -101.9 points, below the smallest possible change.
    (low, high), _ = paired_change([-1.0] * 39 + [0.0])
    assert low == -1.0
    assert -1.0 < high < 0.0
    (low, high), _ = paired_change([1.0] * 40)
    assert high == 1.0
    assert 0.0 < low < 1.0


def test_cohen_kappa():
    assert cohen_kappa([("a", "a"), ("a", "a"), ("b", "b"), ("a", "b")]) == 0.5
    assert cohen_kappa([("a", "a"), ("a", "a")]) is None
    assert cohen_kappa([]) is None


@pytest.mark.parametrize(
    ("worse", "better", "p_value"),
    [
        (1, 8, 0.0390625),
        (8, 1, 0.0390625),
        (0, 5, 0.0625),
        (4, 7, 0.548828125),
        (3, 3, 1.0),
        (0, 0, 1.0),
    ],
)
def test_mcnemar_exact_equals_the_binomial_reference(worse, better, p_value):
    assert mcnemar_exact(worse, better) == pytest.approx(p_value, abs=1e-12)


def test_mcnemar_exact_refuses_negative_counts():
    with pytest.raises(ValueError, match="negative"):
        mcnemar_exact(-1, 2)


@pytest.mark.parametrize(
    ("p_values", "q_values"),
    [
        ([0.01, 0.04, 0.03, 0.005], [0.02, 0.04, 0.04, 0.02]),
        ([0.9, 0.95, 0.2, 0.001, 0.04], [0.95, 0.95, 1 / 3, 0.005, 0.1]),
        ([], []),
    ],
)
def test_benjamini_hochberg_equals_the_statsmodels_reference(p_values, q_values):
    assert benjamini_hochberg(p_values) == pytest.approx(q_values, abs=1e-12)


def test_benjamini_hochberg_refuses_a_value_outside_zero_to_one():
    with pytest.raises(ValueError, match="from 0 to 1"):
        benjamini_hochberg([0.5, 1.5])


def test_krippendorff_alpha_matches_the_published_example():
    # Krippendorff (2011), "Computing Krippendorff's Alpha-Reliability": 4 raters,
    # 12 units with missing values, nominal alpha 0.743.
    raters = [
        [1, 2, 3, 3, 2, 1, 4, 1, 2, None, None, None],
        [1, 2, 3, 3, 2, 2, 4, 1, 2, 5, None, 3],
        [None, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, None],
        [1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, None],
    ]
    assert round(krippendorff_alpha(zip(*raters, strict=True)), 3) == 0.743


def test_krippendorff_alpha_matches_the_krippendorff_package():
    samples = [
        ["write", "write", "verify"],
        ["understand", "understand", "understand"],
        ["verify", "write", None],
        ["other", "other", "other"],
        ["write", "verify", "verify"],
    ]
    assert krippendorff_alpha(samples) == pytest.approx(0.465753, abs=1e-6)
    first = ["early", "other", "other", "error", "other", "early", "dead", "other"]
    second = ["early", "other", "early", "error", "other", "other", "dead", "other"]
    assert krippendorff_alpha(zip(first, second, strict=True)) == pytest.approx(
        0.642857, abs=1e-6
    )
    assert cohen_kappa(list(zip(first, second, strict=True))) == pytest.approx(
        0.619048, abs=1e-6
    )


def test_krippendorff_alpha_is_undefined_without_pairable_values():
    assert krippendorff_alpha([]) is None
    assert krippendorff_alpha([["a"], ["b", None]]) is None
    # All values are the same, so there is no variation to explain.
    assert krippendorff_alpha([["a", "a"], ["a", "a"]]) is None
    assert krippendorff_alpha([["a", "a"], ["b", "b"]]) == 1.0
    assert krippendorff_alpha([["a", "b"], ["b", "a"]]) < 0


def _chen(runs, passed, k):
    # The numerically stable form of Chen et al. (2021), figure 3.
    if runs - passed < k:
        return 1.0
    product = 1.0
    for count in range(runs - passed + 1, runs + 1):
        product *= 1 - k / count
    return 1 - product


def test_pass_at_k_matches_the_formula_of_chen_et_al():
    for runs in range(1, 12):
        for passed in range(runs + 1):
            for k in range(1, runs + 1):
                assert pass_at_k(runs, passed, k) == pytest.approx(
                    _chen(runs, passed, k)
                )
    assert pass_at_k(10, 3, 5) == pytest.approx(1 - 21 / 252)
    assert pass_at_k(5, 2, 1) == pytest.approx(0.4)


def test_pass_all_k_is_the_chance_that_k_draws_all_pass():
    assert pass_all_k(10, 6, 3) == pytest.approx(20 / 120)
    assert pass_all_k(4, 4, 4) == 1.0
    assert pass_all_k(4, 3, 4) == 0.0
    assert pass_all_k(5, 2, 1) == pytest.approx(0.4)


@pytest.mark.parametrize(
    ("runs", "passed", "k"), [(3, 4, 1), (3, -1, 1), (3, 1, 0), (3, 1, 4)]
)
def test_pass_at_k_rejects_impossible_counts(runs, passed, k):
    with pytest.raises(ValueError):
        pass_at_k(runs, passed, k)
    with pytest.raises(ValueError):
        pass_all_k(runs, passed, k)
