"""Immutable input/live View documents and lifecycle events.

The canonical JSON Schema validates semantic nodes and operations. They remain
immutable JSON mappings rather than a second, diverging node model hierarchy.
Only the engine owns ids, sequence numbers, timeouts and terminal outcomes.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from typing import Any, ClassVar

from blockether.vis._contracts import validate

from ._wire import freeze, to_wire


class _ViewRecord:
    __slots__ = ()
    _definition: ClassVar[str]

    @classmethod
    def from_wire(cls, value: Any):
        validate("view", cls._definition, value)
        return cls(**freeze(value))

    def to_wire(self) -> dict[str, Any]:
        return to_wire(self)


@dataclass(frozen=True, slots=True)
class InputView(_ViewRecord):
    _definition = "input_view"
    id: str
    title: str
    fields: tuple[Mapping[str, Any], ...]
    submit_label: str
    cancel_label: str
    is_cancellable: bool
    timeout_ms: int
    created_at: int
    description: str | None = None
    source: str | None = None
    session_id: str | None = None


@dataclass(frozen=True, slots=True)
class LiveView(_ViewRecord):
    _definition = "live_view"
    id: str
    title: str
    nodes: tuple[Mapping[str, Any], ...]
    timeout_ms: int
    channel_ids: tuple[str, ...]
    seq: int
    created_at: int
    description: str | None = None
    source: str | None = None
    session_id: str | None = None


@dataclass(frozen=True, slots=True)
class ViewSnapshot(_ViewRecord):
    _definition = "view"
    title: str
    nodes: tuple[Mapping[str, Any], ...]
    description: str | None = None


@dataclass(frozen=True, slots=True)
class LivePatch(_ViewRecord):
    _definition = "live_patch"
    view_id: str
    seq: int
    ops: tuple[Mapping[str, Any], ...]


@dataclass(frozen=True, slots=True)
class InputResult(_ViewRecord):
    """Public close receipt; submitted values belong only to the waiting extension."""

    _definition = "input_result"
    reason: str


@dataclass(frozen=True, slots=True)
class LiveResult(_ViewRecord):
    _definition = "live_close"
    view_id: str
    is_completed: bool
    reason: str
    is_from_human: bool
    view: ViewSnapshot | None = None
    note: str | None = None
    elided: tuple[Mapping[str, Any], ...] | None = None
    summary: str | None = None
    artifact_id: str | None = None
    error: str | None = None

    @classmethod
    def from_wire(cls, value: Any) -> LiveResult:
        validate("view", cls._definition, value)
        picture = (
            {"view": ViewSnapshot.from_wire(value["view"])} if "view" in value else {}
        )
        return cls(**{**freeze(value), **picture})


@dataclass(frozen=True, slots=True)
class ViewEvent:
    """Typed payload of view.open, view.patch or view.close, not its SSE envelope."""

    kind: str
    view_id: str
    view: InputView | LiveView | None = None
    patch: LivePatch | None = None
    result: InputResult | LiveResult | None = None
    first_seq: int | None = None

    @classmethod
    def from_wire(cls, event_type: str, value: Any) -> ViewEvent:
        definition = {
            "view.open": "open_event",
            "view.patch": "patch_event",
            "view.close": "close_event",
        }[event_type]
        validate("view", definition, value)
        if event_type == "view.open":
            model = InputView if value["kind"] == "input" else LiveView
            document = model.from_wire(value["view"])
            payload, identity = {"view": document}, document.id
        elif event_type == "view.patch":
            document = LivePatch.from_wire(value["patch"])
            if value["first_seq"] > document.seq:
                raise ValueError("invalid View patch sequence")
            payload, identity = (
                {"patch": document, "first_seq": value["first_seq"]},
                document.view_id,
            )
        else:
            if value["kind"] == "input":
                document = InputResult.from_wire(value["result"])
                identity = value["view_id"]
            else:
                document = LiveResult.from_wire(value["result"])
                identity = document.view_id
            payload = {"result": document}
        if value["view_id"] != identity:
            raise ValueError("invalid View identity")
        return cls(kind=value["kind"], view_id=identity, **payload)

    def to_wire(self) -> dict[str, Any]:
        return to_wire(self)
