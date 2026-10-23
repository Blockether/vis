"""Tests for the label agreement of `blockether.vis.evals`."""

import pytest
from blockether.vis.evals import (
    MIN_LABELS,
    agreement,
    alpha_advice,
    kappa_advice,
    majority,
    sample_agreement,
    sample_alpha,
)

_ADVICE = {"noun": "stages", "labeller": "label_stage()", "fix": "Fix the rubric."}


def _items(count, wrong=0):
    stages = ["understand", "write", "verify"]
    items = []
    for index in range(count):
        human = stages[index % 3]
        automatic = stages[(index + 1) % 3] if index < wrong else human
        items.append((f"t{index}", automatic, human))
    return items


def test_agreement_without_labels_asks_for_labels():
    result = agreement([], source="judge", **_ADVICE)
    assert (result.labelled, result.agreement_percent, result.kappa) == (0, None, None)
    assert result.advice == (
        "No labels yet. Label at least 30 failed attempts with label_stage() "
        "before you trust these stages."
    )


def test_agreement_names_each_disagreement():
    result = agreement(_items(4, wrong=1), source="judge", **_ADVICE)
    assert result.disagreements == ("t0: judge write, label understand",)
    assert result.agreement_percent == 75.0
    assert (
        result.advice
        == "Label 26 more failed attempts. Kappa from 4 labels is not stable."
    )


@pytest.mark.parametrize(
    ("wrong", "start"),
    [
        (0, "Kappa 1.00 is at least 0.8. You can use these stages without a review."),
        (6, "Kappa 0.70 is good enough with a human review."),
        (12, "Kappa 0.40 is below 0.6. Do not trust these stages. Fix the rubric."),
    ],
)
def test_agreement_advice_follows_kappa_limits(wrong, start):
    result = agreement(_items(MIN_LABELS, wrong=wrong), source="judge", **_ADVICE)
    assert result.labelled == MIN_LABELS
    assert result.advice.startswith(start)


def test_kappa_advice_reports_undefined_kappa():
    assert kappa_advice(30, None, **_ADVICE).startswith("Kappa is undefined")
    assert kappa_advice(1, 0.9, **_ADVICE) == (
        "Label 29 more failed attempts. Kappa from 1 label is not stable."
    )


def test_majority_breaks_ties_by_stage_order():
    assert majority(["verify", "write", "write"]) == "write"
    assert majority(["verify", "understand"]) == "understand"
    assert majority(["b", "a"], order=()) == "b"
    with pytest.raises(ValueError):
        majority([])


def test_sample_agreement_counts_only_repeated_items():
    assert sample_agreement({"a": ["write"]}) is None
    found = {"a": ["write", "write"], "b": ["write", "verify"], "c": ["other"]}
    assert sample_agreement(found) == 50.0


def test_agreement_gives_krippendorff_alpha_next_to_kappa():
    result = agreement(_items(30, wrong=3), source="judge", **_ADVICE)
    assert result.kappa == 0.85
    # Alpha pools the values of both sides, so it differs from kappa. The PyPI
    # `krippendorff` package gives 0.8525 for the same pairs.
    assert result.alpha == 0.853
    assert agreement([], source="judge", **_ADVICE).alpha is None


def test_sample_alpha_corrects_the_sample_agreement_for_chance():
    samples = {
        "a": ["write", "write", "verify"],
        "b": ["understand", "understand", "understand"],
        "c": ["verify", "write"],
        "d": ["other", "other", "other"],
        "e": ["write", "verify", "verify"],
    }
    # The PyPI `krippendorff` package gives 0.466 for the same samples.
    assert sample_alpha(samples) == 0.466
    assert sample_agreement(samples) == 40.0
    assert sample_alpha({"a": ["write"], "b": ["verify"]}) is None


@pytest.mark.parametrize(
    ("alpha", "start"),
    [
        (None, "Alpha is undefined."),
        (0.5, "Alpha 0.50 is below 0.667."),
        (0.7, "Alpha 0.70 supports only tentative conclusions."),
        (0.9, "Alpha 0.90 is at least 0.8."),
    ],
)
def test_alpha_advice_follows_the_limits_of_krippendorff(alpha, start):
    assert alpha_advice(alpha).startswith(start)
