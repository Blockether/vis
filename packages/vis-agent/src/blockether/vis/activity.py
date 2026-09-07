"""Immutable Activity receipts from the canonical host-owned lifecycle contract."""

from __future__ import annotations

import json
from collections.abc import Mapping
from dataclasses import dataclass
from types import MappingProxyType
from typing import Any

from blockether.vis._contracts import ACTIVITY, validate

from ._wire import freeze, to_wire


@dataclass(frozen=True, slots=True)
class ActivityResource:
    type: str
    id: str


@dataclass(frozen=True, slots=True)
class ActivityDiffLine:
    kind: str
    text: str
    is_redacted: bool | None = None


@dataclass(frozen=True, slots=True)
class ActivityEvidence:
    kind: str
    text: str
    lines: tuple[ActivityDiffLine, ...] | None = None
    additions: int | None = None
    deletions: int | None = None
    modifications: int | None = None
    is_truncated: bool | None = None
    is_redacted: bool | None = None


@dataclass(frozen=True, slots=True)
class ActivityRow:
    id: str
    sequence: int
    operation: str
    presenter: str
    signal: str
    state: str
    summary: str
    resources: tuple[ActivityResource, ...]
    evidence: tuple[ActivityEvidence, ...]
    group_token: str | None = None
    duration_ms: int | None = None
    result_summary: str | None = None
    error_summary: str | None = None
    summary_format: str | None = None
    result_format: str | None = None
    is_truncated: bool | None = None
    children: tuple[ActivityRow, ...] | None = None
    presentation: Mapping[str, Any] | None = None

    @classmethod
    def _from_validated(cls, value):
        evidence = tuple(
            ActivityEvidence(
                **{
                    **item,
                    **(
                        {
                            "lines": tuple(
                                ActivityDiffLine(**line) for line in item["lines"]
                            )
                        }
                        if "lines" in item
                        else {}
                    ),
                }
            )
            for item in value["evidence"]
        )
        children = (
            {
                "children": tuple(
                    cls._from_validated(child) for child in value["children"]
                )
            }
            if "children" in value
            else {}
        )
        return cls(
            **{
                **freeze(value),
                "resources": tuple(ActivityResource(**r) for r in value["resources"]),
                "evidence": evidence,
                **children,
            }
        )


@dataclass(frozen=True, slots=True)
class ActivityGroup:
    """Adjacent operations; rows retain their evidence, not extra invocations.

    The id survives shell polling and appending operations. Unknown operations stay
    separate. UI disclosure state belongs to the reader, never to this receipt.
    """

    id: str
    label: str | None
    rows: tuple[ActivityRow, ...]


@dataclass(frozen=True, slots=True)
class ActivityCounts:
    running: int
    succeeded: int
    failed: int
    cancelled: int


@dataclass(frozen=True, slots=True)
class ActivityOmitted:
    rows: int
    by_classification: Mapping[str, int]


@dataclass(frozen=True, slots=True)
class ActivityProjection:
    """A full replacement for one form; no owner id or View lifecycle inside it."""

    state: str
    counts: ActivityCounts
    rows: tuple[ActivityRow, ...]
    omitted: ActivityOmitted

    @property
    def groups(self) -> tuple[ActivityGroup, ...]:
        """The same chronological runs used by Companion and TUI; not serialized."""
        groups: list[ActivityGroup] = []
        for row in sorted(self.rows, key=lambda row: row.sequence):
            label = ACTIVITY["operation_groups"].get(row.operation)
            if label and groups and groups[-1].label == label:
                previous = groups[-1]
                groups[-1] = ActivityGroup(previous.id, label, (*previous.rows, row))
            else:
                first = (
                    row.children[0]
                    if row.operation == "shell" and row.children
                    else row
                )
                groups.append(ActivityGroup(first.id, label, (row,)))
        return tuple(groups)

    @classmethod
    def from_wire(cls, value: Any) -> ActivityProjection:
        validate("activity", "projection", value)
        ids = []

        def visit(rows):
            for row in rows:
                presentation = row.get("presentation")
                sections = (
                    [presentation, *presentation.get("sections", [])]
                    if presentation is not None
                    else []
                )
                content = [
                    block for section in sections for block in section["content"]
                ]
                if (
                    len(content) > 32
                    or len(
                        json.dumps(
                            presentation, ensure_ascii=False, separators=(",", ":")
                        ).encode()
                    )
                    > 32768
                    or any(
                        len(section[key].encode("utf-8")) > 512
                        for section in sections
                        for key in ("headline", "summary")
                    )
                ):
                    raise ValueError("activity presentation exceeds bound")
                for block in content:
                    if block["type"] == "progress" and "value" in block:
                        if not (0 <= block["value"] <= block["total"]):
                            raise ValueError("invalid activity progress")
                    if block["type"] == "table" and any(
                        len(cells) != len(block["columns"]) for cells in block["rows"]
                    ):
                        raise ValueError("invalid activity table width")
                ids.append(row["id"])
                visit(row.get("children", []))

        visit(value["rows"])
        if (
            len(set(ids)) != len(ids)
            or len(
                json.dumps(value, ensure_ascii=False, separators=(",", ":")).encode()
            )
            > ACTIVITY["limits"]["max_receipt_bytes"]
        ):
            raise ValueError("invalid activity projection identity or byte bound")
        return cls(
            value["state"],
            ActivityCounts(**value["counts"]),
            tuple(ActivityRow._from_validated(row) for row in value["rows"]),
            ActivityOmitted(
                value["omitted"]["rows"],
                MappingProxyType(dict(value["omitted"]["by_classification"])),
            ),
        )

    def to_wire(self) -> dict[str, Any]:
        return to_wire(self)
