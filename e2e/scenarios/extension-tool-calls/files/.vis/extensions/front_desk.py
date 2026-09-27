"""Book guests by calling seat_desk through vis.tools, as one extension using another."""

from __future__ import annotations

import dataclasses
import json
from dataclasses import dataclass
from pathlib import Path

import blockether.vis.extension as vis


@dataclass(frozen=True)
class Booking:
    guest: str
    section: str
    hold_id: str
    seat: str
    hold_class: str
    seat_class: str
    frozen: bool


def journal_call(row):
    """Record what vis.tools answered, independently of model output."""
    journal = Path(__file__).resolve().parents[2] / "front-desk-calls.jsonl"
    with journal.open("a") as stream:
        stream.write(json.dumps(row, sort_keys=True) + "\n")


def is_frozen(value):
    try:
        value.guest = "changed"
    except dataclasses.FrozenInstanceError:
        return True
    return False


def book_activity(*, phase, result, **_):
    if phase == "success":
        return vis.ActivityPresentation(
            "Book a guest", f"{result.guest}: seat {result.seat} ({result.hold_id})"
        )
    return None


class FrontDesk:
    @vis.method(
        tag="mutation",
        activity=vis.Activity(label="Book a guest", render=book_activity),
    )
    def book(self, guest: str, /, *, section: str) -> Booking:
        """Book a guest by holding a seat through the seat_desk extension."""
        hold = vis.tools.seat_desk.hold(guest, section=section)
        evidence = {
            "operation": "front_desk.book",
            "hold_class": type(hold).__name__,
            "seat_class": type(hold.seat).__name__,
            "dataclass": dataclasses.is_dataclass(hold),
            "frozen": is_frozen(hold),
            "fields": sorted(field.name for field in dataclasses.fields(hold)),
            "item_access": hold["section"] == hold.section == section,
        }
        journal_call(evidence)
        return Booking(
            guest=hold.guest,
            section=hold.section,
            hold_id=hold.hold_id,
            seat=f"{hold.seat.row}{hold.seat.number}",
            hold_class=evidence["hold_class"],
            seat_class=evidence["seat_class"],
            frozen=evidence["frozen"],
        )


vis.register_extension(
    vis.Extension(
        name="front-desk",
        description="Book guests; holds their seats through the seat_desk extension.",
        alias="front_desk",
        symbols=[vis.Symbol(FrontDesk(), name="front_desk")],
    )
)
