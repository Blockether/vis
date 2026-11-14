"""Orders and their totals."""

from dataclasses import dataclass


@dataclass(frozen=True)
class Line:
    """One line of an order: a product, a unit price in cents and a quantity."""

    product: str
    unit_cents: int
    quantity: int = 1

    @property
    def total_cents(self) -> int:
        return self.unit_cents * self.quantity


def subtotal(lines: list[Line]) -> int:
    """Return the sum of the lines in cents."""
    return sum(line.total_cents for line in lines)


def discount(cents: int, percent: int) -> int:
    """Return the price after a discount of `percent`, rounded down to a cent."""
    if not 0 <= percent <= 100:
        raise ValueError("percent must be from 0 to 100")
    return cents * (100 - percent) // 100


def to_csv(lines: list[Line]) -> str:
    """Return the lines as CSV text with a header."""
    rows = ["product,unit_cents,quantity,total_cents"]
    rows += [f"{line.product},{line.unit_cents},{line.quantity},{line.total_cents}" for line in lines]
    return "\n".join(rows) + "\n"
