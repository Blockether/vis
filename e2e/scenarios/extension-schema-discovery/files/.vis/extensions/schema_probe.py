"""Issue #234: repeated records and bounded discovery of a deep result schema."""

from __future__ import annotations

from dataclasses import dataclass, make_dataclass
from typing import Annotated

import blockether.vis.extension as vis


@dataclass(frozen=True)
class ToolCard:
    key: str
    name: str
    purpose: str
    enabled: bool
    labels: tuple[str, ...]


@dataclass(frozen=True)
class Target:
    address: str
    region: str
    tool: ToolCard


@dataclass(frozen=True)
class Entry:
    target: Target
    jobs: int
    status: str


@dataclass(frozen=True)
class Lag:
    elapsed: Annotated[int, "Microseconds since the last successful heartbeat."]


# A deterministic generated record chain crosses the compact field budget.
# The value deliberately carries no unit: it must be read from the full contract.
DeepSample = Lag
DEEP_SAMPLE = Lag(750)
for level in reversed(range(40)):
    DeepSample = make_dataclass(
        f"SampleLevel{level:02d}",
        [("source", str), ("valid", bool), ("sample", DeepSample)],
        frozen=True,
    )
    DEEP_SAMPLE = DeepSample(f"collector-{level:02d}", True, DEEP_SAMPLE)


@dataclass(frozen=True)
class MonitorResult:
    active_jobs: int
    snapshot: tuple[Entry, ...]
    failures: tuple[Entry, ...]
    primary: Target
    diagnostics: DeepSample


ATLAS = ToolCard("atlas", "Atlas", "Index local records", True, ("search", "local"))
BEACON = ToolCard("beacon", "Beacon", "Monitor local jobs", True, ("monitor",))
TARGET = Target("10.0.0.5", "test-region", ATLAS)


def cards_activity(*, phase, result, **_):
    if phase == "success":
        count = len(result) if isinstance(result, tuple) else 1
        return vis.ActivityPresentation("Read tool cards", f"Tool cards: {count}")
    return None


def monitor_activity(*, phase, result, **_):
    if phase == "success":
        return vis.ActivityPresentation(
            "Read monitor snapshot",
            f"{result.active_jobs} active jobs; failures: {len(result.failures)}",
        )
    return None


class SchemaProbe:
    @vis.method(
        activity=vis.Activity(
            label="Read tool cards", show_start=False, render=cards_activity
        )
    )
    def cards(self, key: str | None = None) -> ToolCard | tuple[ToolCard, ...]:
        """Read available tool cards; an unknown key raises an error."""
        cards = (ATLAS, BEACON)
        if key is None:
            return cards
        for card in cards:
            if card.key == key:
                return card
        raise ValueError(f"Unknown tool key: {key}")

    @vis.method(
        activity=vis.Activity(
            label="Read monitor snapshot", show_start=False, render=monitor_activity
        )
    )
    def monitor(self) -> MonitorResult:
        """Read a fixed local monitor snapshot without changing it."""
        return MonitorResult(
            7,
            (Entry(TARGET, 7, "running"),),
            (Entry(TARGET, 0, "stalled"),),
            TARGET,
            DEEP_SAMPLE,
        )


vis.register_extension(
    vis.Extension(
        name="schema-probe",
        description="Read local tool cards and a typed monitor snapshot.",
        alias="schema_probe",
        symbols=[vis.Symbol(SchemaProbe(), name="schema_probe")],
    )
)
