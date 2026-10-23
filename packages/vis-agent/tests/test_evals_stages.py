"""Tests for the judge stage labels of `blockether.vis.evals`."""

import json

import pytest
from blockether.vis.evals import (
    MIN_LABELS,
    judge_errors,
    judged_samples,
    load_stage_labels,
    save_stage_label,
    stage_agreement,
)


def test_judge_files_read_samples_and_errors(tmp_path):
    (tmp_path / "judge_stages.json").write_text(
        json.dumps({"a__1": ["write", "write", "verify"], "b__1": ["verify"]})
    )
    (tmp_path / "judge_report.json").write_text(
        json.dumps({"results": [{"error": "timeout"}, {"stage": "write"}]})
    )

    assert judged_samples(tmp_path) == {
        "a__1": ["write", "write", "verify"],
        "b__1": ["verify"],
    }
    assert judge_errors(tmp_path) == 1
    assert judged_samples(tmp_path / "missing") == {}
    assert judge_errors(tmp_path / "missing") == 0


def test_save_stage_label_replaces_the_label_of_a_trial(tmp_path):
    path = tmp_path / "labels" / "stage-labels.json"

    first = save_stage_label(path, "a__1", "write", "first", judged_stage="verify")
    save_stage_label(path, "a__1", "verify", "second")

    assert first.judged_stage == "verify"
    labels = load_stage_labels(path)
    assert list(labels) == ["a__1"]
    assert labels["a__1"]["stage"] == "verify"
    assert labels["a__1"]["note"] == "second"
    assert not path.with_name(path.name + ".tmp").exists()


def test_save_stage_label_rejects_an_unknown_stage_and_a_long_note(tmp_path):
    path = tmp_path / "stage-labels.json"

    with pytest.raises(ValueError, match="stage must be one of"):
        save_stage_label(path, "a__1", "deploy")
    with pytest.raises(ValueError, match="the limit is 500"):
        save_stage_label(path, "a__1", "write", "x" * 501)
    assert not path.exists()


def test_stage_agreement_uses_the_majority_of_the_samples():
    samples = {"a__1": ["write", "write", "verify"], "b__1": ["verify", "verify"]}
    labels = {"a__1": {"stage": "write"}, "b__1": {"stage": "write"}, "c__1": {}}

    found = stage_agreement(samples, labels, errors=2)

    assert found.judged == 2
    assert found.errors == 2
    assert found.stages == ("a__1: write", "b__1: verify")
    assert found.sample_agreement_percent == 50.0
    # The PyPI `krippendorff` package gives 0.333 for the same samples.
    assert found.sample_alpha == 0.333
    assert found.sample_advice.startswith("Alpha 0.33 is below 0.667.")
    assert found.labelled == 2
    assert found.agreement_percent == 50.0
    assert found.disagreements == ("b__1: judge verify, label write",)
    assert f"Label {MIN_LABELS - 2} more" in found.advice


def test_stage_agreement_without_labels_asks_for_labels():
    found = stage_agreement({"a__1": ["write"]}, {})

    assert found.labelled == 0
    assert found.kappa is None
    assert found.sample_agreement_percent is None
    assert (found.sample_alpha, found.alpha) == (None, None)
    assert found.sample_advice.startswith("Alpha is undefined.")
    assert "save_stage_label()" in found.advice
