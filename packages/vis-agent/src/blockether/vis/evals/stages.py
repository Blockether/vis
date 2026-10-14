"""Check the failure stages of a trajectory judge against human labels.

A judge reads failed scenario trials and names the stage where each one went wrong: one
of `STAGES`. It writes `judge_stages.json`, a map from trial name to the stage of each
sample, and `judge_report.json` in its job folder. Read them with `judged_samples` and
`judge_errors`. Keep your own stage of each trial with `save_stage_label`, then call
`stage_agreement` before you trust the judge.
"""

from __future__ import annotations

import json
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path

from .bench import NOTE_LIMIT
from .labels import STAGES, agreement, majority, sample_agreement

__all__ = [
    "StageAgreement",
    "StageLabel",
    "judge_errors",
    "judged_samples",
    "load_stage_labels",
    "save_stage_label",
    "stage_agreement",
]


@dataclass(frozen=True)
class StageLabel:
    """A human failure stage of one scenario trial, kept in a stage labels file.

    `judged_stage` is the stage that a judge gave the same trial, or None.
    """

    trial: str
    stage: str
    judged_stage: str | None
    note: str
    labelled_at: str


@dataclass(frozen=True)
class StageAgreement:
    """How well the failure stages of one judge agree with human stage labels.

    `stages` names the judged stage of each trial: the most common stage of its samples.
    `sample_agreement_percent` is the share of trials whose samples all agree, or None
    with one sample. `kappa` and `advice` follow the limits of `labels`.
    """

    judged: int
    errors: int
    stages: tuple[str, ...]
    sample_agreement_percent: float | None
    labelled: int
    agreement_percent: float | None
    kappa: float | None
    disagreements: tuple[str, ...]
    advice: str


def _read_json(path: Path) -> object:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None


def judged_samples(job_dir: Path | str) -> dict[str, list[str]]:
    """Read the stage of each judge sample, grouped by trial, from `judge_stages.json`.

    A file that is missing or is not a JSON object gives an empty map.
    """
    data = _read_json(Path(job_dir) / "judge_stages.json")
    if not isinstance(data, dict):
        return {}
    return {
        str(trial): [str(stage) for stage in found]
        for trial, found in data.items()
        if isinstance(found, list)
    }


def judge_errors(job_dir: Path | str) -> int:
    """Count the judge results with an error in `judge_report.json`, or 0 without it."""
    data = _read_json(Path(job_dir) / "judge_report.json")
    results = data.get("results") if isinstance(data, dict) else None
    return sum(
        1
        for result in results or ()
        if isinstance(result, dict) and result.get("error")
    )


def load_stage_labels(path: Path | str) -> dict[str, dict]:
    """Read a stage labels file: a map from trial name to its label.

    A file that is missing or is not a JSON object gives an empty map.
    """
    data = _read_json(Path(path))
    return data if isinstance(data, dict) else {}


def save_stage_label(
    path: Path | str,
    trial: str,
    stage: str,
    note: str = "",
    *,
    judged_stage: str | None = None,
) -> StageLabel:
    """Save your failure stage of one trial in the stage labels file at `path`.

    `stage` is one of `STAGES`. A new label of the same trial replaces the old one, and
    the write replaces the file at once. Raises `ValueError` for an unknown stage or a
    note longer than `NOTE_LIMIT`.
    """
    if stage not in STAGES:
        raise ValueError(f"stage must be one of {', '.join(STAGES)}, not {stage!r}")
    if len(note) > NOTE_LIMIT:
        raise ValueError(
            f"the note has {len(note)} characters; the limit is {NOTE_LIMIT}"
        )
    label = StageLabel(
        trial=trial,
        stage=stage,
        judged_stage=judged_stage,
        note=note,
        labelled_at=datetime.now(UTC).isoformat(timespec="seconds"),
    )
    labels = load_stage_labels(path)
    labels[trial] = {"stage": stage, "note": note, "labelled_at": label.labelled_at}
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    draft = target.with_name(target.name + ".tmp")
    draft.write_text(json.dumps(labels, indent=2, sort_keys=True) + "\n", "utf-8")
    draft.replace(target)
    return label


def stage_agreement(
    samples: Mapping[str, Sequence[str]],
    labels: Mapping[str, Mapping],
    errors: int = 0,
) -> StageAgreement:
    """Compare the judged stages with human stage labels of the same trials.

    `samples` maps a trial name to the stage of each judge sample, as from
    `judged_samples`. `labels` maps a trial name to a label with a `stage` value, as from
    `load_stage_labels`. Labels of trials without a judged stage do not count.
    """
    judged = {
        trial: majority(found) for trial, found in sorted(samples.items()) if found
    }
    found = agreement(
        (
            (trial, judged[trial], str(label.get("stage")))
            for trial, label in sorted(labels.items())
            if trial in judged
        ),
        source="judge",
        noun="stages",
        labeller="save_stage_label()",
        fix="Correct the judge rubric first.",
    )
    return StageAgreement(
        judged=len(judged),
        errors=errors,
        stages=tuple(f"{trial}: {stage}" for trial, stage in judged.items()),
        sample_agreement_percent=sample_agreement(samples),
        labelled=found.labelled,
        agreement_percent=found.agreement_percent,
        kappa=found.kappa,
        disagreements=found.disagreements,
        advice=found.advice,
    )
