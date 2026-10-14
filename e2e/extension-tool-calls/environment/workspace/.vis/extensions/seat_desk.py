"""Hold typed seats for the extension tool-call scenario; front_desk calls this."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

import blockether.vis.extension as vis


@dataclass(frozen=True)
class Seat:
    row: str
    number: int


@dataclass(frozen=True)
class Hold:
    hold_id: str
    guest: str
    section: str
    seat: Seat


def journal_call(row):
    """Record fixture invocations independently of model output."""
    journal = Path(__file__).resolve().parents[2] / "seat-desk-calls.jsonl"
    with journal.open("a") as stream:
        stream.write(json.dumps(row, sort_keys=True) + "\n")


def hold_activity(*, phase, result, **_):
    if phase == "success":
        return vis.ActivityPresentation(
            "Hold a seat", f"{result.seat.row}{result.seat.number} for {result.guest}"
        )
    return None


class SeatDesk:
    def __init__(self) -> None:
        self._holds = 0

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Hold a seat", show_start=False, render=hold_activity
        ),
    )
    def hold(self, guest: str, /, *, section: str = "main") -> Hold:
        """Hold the next free seat in one section of the local seating chart."""
        journal_call(
            {
                "operation": "seat_desk.hold",
                "arguments": {"guest": guest, "section": section},
            }
        )
        if not guest.strip() or not section.strip():
            raise ValueError("Guest and section must be nonblank")
        self._holds += 1
        row = "B" if section == "balcony" else "A"
        return Hold(f"H{self._holds}", guest, section, Seat(row, self._holds))


vis.register_extension(
    vis.Extension(
        name="seat-desk",
        description="Hold seats in a local seating chart.",
        alias="seat_desk",
        symbols=[vis.Symbol(SeatDesk(), name="seat_desk")],
    )
)
