"""Immutable Activity receipts, counts, grouping and evidence.

For reading event data, see the
[SDK guide](https://vis.blockether.com/python-sdk.html#read-activity-and-view-receipts).
For authoring tool presentations, see the
[Activity presentation guide](https://vis.blockether.com/extension-api.html#activity-presentation).
"""

from __future__ import annotations

import json
from collections.abc import Mapping
from dataclasses import dataclass
from types import MappingProxyType
from typing import Any

from blockether.vis._contracts import definition, validate

from ._wire import freeze, to_wire


def _valid_handle_id(value: str) -> bool:
    limits = definition("activity", "handle_id")
    return (
        isinstance(value, str)
        and bool(value.strip())
        and len(value) <= limits["maxLength"]
        and len(value.encode("utf-8")) <= limits["x-vis-max-bytes"]
        and not any(ord(c) < 32 or ord(c) in (127, 8232, 8233) for c in value)
    )


@dataclass(frozen=True, slots=True)
class ActivityResource:
    """A typed reference to a resource involved in an invocation, identified by `id`."""

    type: str
    id: str


@dataclass(frozen=True, slots=True)
class ActivityDiffLine:
    """One classified diff line; `is_redacted` marks content withheld from the receipt."""

    kind: str
    text: str
    is_redacted: bool | None = None


@dataclass(frozen=True, slots=True)
class ActivityEvidence:
    """Text or structured change evidence attached to an invocation.

    Optional counts describe the observed changes; truncation and redaction flags
    explain why the displayed evidence may not contain the full content.
    """

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
    """One invocation's lifecycle, outcome and human-readable evidence.

    `id` and `sequence` preserve identity and ordering. `state` describes the
    outcome; `summary` is display text, not a replacement for status. Linked
    receipts appear as `children` with their shared `handle_id`. `presentation`
    contains the extension's selected content, while `evidence` retains
    engine-observed information.
    """

    id: str
    sequence: int
    operation: str
    presenter: str
    signal: str
    state: str
    summary: str
    resources: tuple[ActivityResource, ...]
    evidence: tuple[ActivityEvidence, ...]
    argument_key: str | None = None
    handle_id: str | None = None
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


def _first_invocation_id(row: ActivityRow) -> str:
    return (
        row.children[0]
        if row.children and (row.handle_id is not None or row.operation == "shell")
        else row
    ).id


def _operation_group_label(operation: str, rows: list[ActivityRow]) -> str:
    labels = definition("activity", "row")["properties"]["operation"][
        "x-vis-group-labels"
    ]
    if operation in labels:
        return labels[operation]
    for row in rows:
        headline = (row.presentation or {}).get("headline", "").strip()
        if headline:
            return headline
    return operation


@dataclass(frozen=True, slots=True)
class ActivityArgumentGroup:
    """One operation with identical complete arguments; all invocation evidence is retained."""

    id: str
    rows: tuple[ActivityRow, ...]


def _argument_groups(
    rows: tuple[ActivityRow, ...],
) -> tuple[ActivityArgumentGroup, ...]:
    grouped: dict[tuple[str | None, str], list[ActivityRow]] = {}
    for row in sorted(rows, key=lambda row: row.sequence):
        key = (row.operation, row.argument_key) if row.argument_key else (None, row.id)
        grouped.setdefault(key, []).append(row)
    return tuple(
        ActivityArgumentGroup(_first_invocation_id(members[0]), tuple(members))
        for members in grouped.values()
    )


@dataclass(frozen=True, slots=True)
class ActivityGroup:
    """One exact operation across the block, ordered by first invocation.

    Shell polling remains child evidence. Unknown operations use their exact names.
    Disclosure state belongs to the reader, never to this receipt.
    """

    id: str
    label: str
    rows: tuple[ActivityRow, ...]

    @property
    def argument_groups(self) -> tuple[ActivityArgumentGroup, ...]:
        """Repeated arguments in first-entry order; unknown argument keys stay separate."""
        return _argument_groups(self.rows)


@dataclass(frozen=True, slots=True)
class ActivityCounts:
    """Invocation counts by lifecycle outcome, distinct from the visible row count."""

    running: int
    succeeded: int
    failed: int
    cancelled: int


@dataclass(frozen=True, slots=True)
class ActivityOmitted:
    """Rows omitted from this projection, with counts by omission classification."""

    rows: int
    by_classification: Mapping[str, int]


@dataclass(frozen=True, slots=True)
class ActivityProjection:
    """One form's Activity receipt, optionally a page of its durable history.

    Prefer `from_wire` to direct construction when accepting external data.
    `rows` and nested values are immutable; `to_wire` produces an independent
    mutable JSON-compatible copy. Grouping properties are computed, not stored.
    """

    state: str
    counts: ActivityCounts
    rows: tuple[ActivityRow, ...]
    omitted: ActivityOmitted
    history: Mapping[str, Any] | None = None

    @property
    def groups(self) -> tuple[ActivityGroup, ...]:
        """The same per-operation groups used by Companion and TUI; not serialized."""
        grouped: dict[str, list[ActivityRow]] = {}
        for row in sorted(self.rows, key=lambda row: row.sequence):
            grouped.setdefault(row.operation, []).append(row)
        return tuple(
            ActivityGroup(
                _first_invocation_id(rows[0]),
                _operation_group_label(operation, rows),
                tuple(rows),
            )
            for operation, rows in grouped.items()
        )

    @property
    def argument_groups(self) -> tuple[ActivityArgumentGroup, ...]:
        """Exact operation/argument pairs within this block; not serialized."""
        return _argument_groups(self.rows)

    @classmethod
    def from_wire(cls, value: Any) -> ActivityProjection:
        """Validate a receipt and construct immutable rows and nested records.

        Raises:
            ValueError: The schema, identity uniqueness, presentation bounds or
                history page limits are invalid.
        """
        validate("activity", "projection", value)
        ids = []
        leaf_count = 0

        def visit(rows):
            nonlocal leaf_count
            for row in rows:
                presentation = row.get("presentation")
                for carrier in (row, presentation):
                    if carrier is not None and "handle_id" in carrier:
                        if not _valid_handle_id(carrier["handle_id"]):
                            raise ValueError("invalid Activity handle id")
                sections = (
                    [presentation, *presentation.get("sections", [])]
                    if presentation is not None
                    else []
                )
                content = [
                    block for section in sections for block in section["content"]
                ]
                if any(
                    len(section[key].encode("utf-8")) > 512
                    for section in sections
                    for key in ("headline", "summary")
                ):
                    raise ValueError("activity headline or summary exceeds bound")
                for block in content:
                    if block["type"] == "progress" and "value" in block:
                        if not (0 <= block["value"] <= block["total"]):
                            raise ValueError("invalid activity progress")
                    if block["type"] == "table" and any(
                        len(cells) != len(block["columns"]) for cells in block["rows"]
                    ):
                        raise ValueError("invalid activity table width")
                ids.append(row["id"])
                children = row.get("children", [])
                if not children:
                    leaf_count += 1
                visit(children)

        visit(value["rows"])
        if len(set(ids)) != len(ids) or (
            "history" in value
            and (
                leaf_count
                > definition("activity", "projection")["properties"]["history"][
                    "x-vis-max-page-rows"
                ]
                or (
                    leaf_count > 1
                    and len(
                        json.dumps(
                            value, ensure_ascii=False, separators=(",", ":")
                        ).encode()
                    )
                    > definition("activity", "projection")["properties"]["history"][
                        "x-vis-page-target-bytes"
                    ]
                )
            )
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
            freeze(value["history"]) if "history" in value else None,
        )

    def to_wire(self) -> dict[str, Any]:
        """Return a fresh portable receipt without computed reader grouping fields."""
        return to_wire(self)
