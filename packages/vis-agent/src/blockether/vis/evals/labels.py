"""Agreement between automatic results and human labels: kappa, majority and advice.

Use these helpers before you trust an automatic classifier, such as the failure causes
of a run or the failure stages of a judge. Label the same items by hand, pair each
automatic result with its label, and call `agreement`. Its `advice` applies the
limits of this module: `MIN_LABELS` labels, a human review from `KAPPA_REVIEW` and no
review from `KAPPA_TRUST`.
"""

from __future__ import annotations

from collections import Counter
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass

from .stats import cohen_kappa

__all__ = [
    "KAPPA_REVIEW",
    "KAPPA_TRUST",
    "MIN_LABELS",
    "STAGES",
    "Agreement",
    "agreement",
    "kappa_advice",
    "majority",
    "sample_agreement",
]

MIN_LABELS = 30
"""The smallest number of labels that gives a stable kappa."""

KAPPA_REVIEW = 0.6
"""The smallest kappa that makes automatic results usable with a human review."""

KAPPA_TRUST = 0.8
"""The smallest kappa that makes automatic results usable without a review."""

STAGES = ("understand", "write", "verify", "other")
"""The failure stages of a judge, in the order of the work."""


@dataclass(frozen=True)
class Agreement:
    """How well automatic results agree with human labels of the same items.

    `labelled` counts the paired items. `agreement_percent` is the share of equal pairs,
    or None without pairs. `kappa` is Cohen's kappa, rounded to 3 places, or None when
    it is undefined. `disagreements` names each item where the two results differ.
    `advice` says if you can trust the automatic results.
    """

    labelled: int
    agreement_percent: float | None
    kappa: float | None
    disagreements: tuple[str, ...]
    advice: str


def _labels(count: int) -> str:
    return f"{count} label" if count == 1 else f"{count} labels"


def kappa_advice(
    count: int, kappa: float | None, *, noun: str, labeller: str, fix: str
) -> str:
    """Say if `count` labels with this `kappa` make the automatic `noun` usable.

    `labeller` names the way to add a label, and `fix` is the action for a low kappa.
    """
    if not count:
        return (
            f"No labels yet. Label at least {MIN_LABELS} failed attempts with "
            f"{labeller} before you trust these {noun}."
        )
    if count < MIN_LABELS:
        return (
            f"Label {MIN_LABELS - count} more failed attempts. Kappa from "
            f"{_labels(count)} is not stable."
        )
    if kappa is None:
        return (
            f"Kappa is undefined, because all labels and {noun} are the same. Label "
            f"failures of other {noun} too."
        )
    if kappa < KAPPA_REVIEW:
        return f"Kappa {kappa:.2f} is below {KAPPA_REVIEW}. Do not trust these {noun}. {fix}"
    if kappa < KAPPA_TRUST:
        return (
            f"Kappa {kappa:.2f} is good enough with a human review. Read the "
            "disagreements before you act on a finding."
        )
    return (
        f"Kappa {kappa:.2f} is at least {KAPPA_TRUST}. You can use these {noun} "
        "without a review."
    )


def agreement(
    items: Iterable[tuple[str, str, str]],
    *,
    source: str,
    noun: str,
    labeller: str,
    fix: str,
) -> Agreement:
    """Compare automatic results with human labels.

    Each item is `(name, automatic, human)`. `source` names the automatic side in each
    disagreement, for example `"judge"`. `noun`, `labeller` and `fix` go to
    `kappa_advice`.
    """
    pairs = []
    disagreements = []
    for name, automatic, human in items:
        pairs.append((automatic, human))
        if automatic != human:
            disagreements.append(f"{name}: {source} {automatic}, label {human}")
    count = len(pairs)
    kappa = cohen_kappa(pairs)
    kappa = None if kappa is None else round(kappa, 3)
    return Agreement(
        labelled=count,
        agreement_percent=round(100 * (count - len(disagreements)) / count, 1)
        if count
        else None,
        kappa=kappa,
        disagreements=tuple(disagreements),
        advice=kappa_advice(count, kappa, noun=noun, labeller=labeller, fix=fix),
    )


def majority(found: Sequence[str], order: Sequence[str] = STAGES) -> str:
    """Return the most common value of `found`. A tie gives the value first in `order`."""
    if not found:
        raise ValueError("majority needs at least one value")
    counts = Counter(found)
    ranked = list(order) + [value for value in counts if value not in order]
    return max(ranked, key=lambda value: (counts[value], -ranked.index(value)))


def sample_agreement(samples: Mapping[str, Sequence[str]]) -> float | None:
    """Return the share of items whose samples all agree, as a percent.

    Only items with more than one sample count. Return None without such items.
    """
    several = [found for found in samples.values() if len(found) > 1]
    if not several:
        return None
    same = sum(1 for found in several if len(set(found)) == 1)
    return round(100 * same / len(several), 1)
