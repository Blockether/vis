"""Issue #232: discover invocation shape without duplicating it in prose."""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Annotated

import blockether.vis.extension as vis


@dataclass(frozen=True)
class Receipt:
    sequence: int
    person: str
    bucket: str
    copies: int
    note: str | None
    total: int


@dataclass(frozen=True)
class Status:
    records: int
    total: int


def record_activity(*, phase, result, **_):
    if phase == "success":
        return vis.ActivityPresentation(
            "Record local copies",
            f"{result.copies} copies; {result.total} total",
            (vis.ActivityText(f"{result.person}: {result.bucket}"),),
        )
    return None


def status_activity(*, phase, result, **_):
    if phase == "success":
        return vis.ActivityPresentation(
            "Read local totals", f"{result.records} records; {result.total} copies"
        )
    return None


class ContractProbe:
    def __init__(self) -> None:
        self._records = 0
        self._total = 0
        self._ledger = Path(__file__).resolve().parents[2] / "contract-receipts.jsonl"

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Record local copies", show_start=False, render=record_activity
        ),
    )
    def record(
        self,
        person: str,
        /,
        *,
        bucket: Annotated[str, "Local destination label."],
        copies: Annotated[int, "Number of copies; must be positive."] = 2,
        note: str | None = None,
    ) -> Receipt:
        """Append to the local ledger; never sends a message. Names must be nonblank."""
        if not person.strip() or not bucket.strip() or copies < 1:
            raise ValueError("Names must be nonblank and copies must be positive")
        self._records += 1
        self._total += copies
        receipt = Receipt(self._records, person, bucket, copies, note, self._total)
        with self._ledger.open("a") as ledger:
            ledger.write(json.dumps(asdict(receipt), sort_keys=True) + "\n")
        return receipt

    @vis.method(
        activity=vis.Activity(
            label="Read local totals", show_start=False, render=status_activity
        )
    )
    def status(self) -> Status:
        """Read local ledger totals without changing them."""
        return Status(self._records, self._total)


vis.register_extension(
    vis.Extension(
        name="contract-probe",
        description="Record local copies and inspect their totals.",
        alias="contract_probe",
        symbols=[vis.Symbol(ContractProbe(), name="contract_probe")],
    )
)
