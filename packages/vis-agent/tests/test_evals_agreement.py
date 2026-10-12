"""Tests for the label agreement of `blockether.vis.evals`."""

import pytest
from blockether.vis.evals import (
    MIN_LABELS,
    agreement,
    kappa_advice,
    majority,
    sample_agreement,
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
