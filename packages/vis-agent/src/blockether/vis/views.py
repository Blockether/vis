"""Read input forms and live-view updates received from Vis.

These immutable records describe what the engine published. To *create* a form
or live interface in an extension, use the
[human-input guide](https://vis.blockether.com/human-input.html) or
[live-view guide](https://vis.blockether.com/live-views.html), not these records.

## Follow the view lifecycle

1. `ViewEvent` decodes `view.open`, `view.patch` and `view.close` payloads.
2. `InputView` describes a form; `LiveView` describes an open live interface.
3. `LivePatch` carries ordered updates. `InputResult` and `LiveResult` describe
   closure; `ViewSnapshot` is the final document when one is retained.

`blockether.vis.engine.Event.view` already decodes these payloads for stream
consumers. Use `from_wire` when you receive a raw JSON mapping yourself and
`to_wire` when you need a fresh JSON-compatible copy. Nested nodes stay immutable
mappings, validated by the same schema as Vis; they are not Python UI widgets.

## Inspect a completed view

```python
from blockether.vis.views import LiveResult

result = LiveResult.from_wire(
    {
        "view_id": "build-one",
        "is_completed": True,
        "reason": "completed",
        "is_from_human": False,
        "view": {
            "title": "Build",
            "nodes": [{"id": "status", "type": "status", "text": "Done", "tone": "ok"}],
        },
    }
)
assert result.view.nodes[0]["text"] == "Done"
assert result.to_wire()["view"]["title"] == "Build"
```

Only the engine assigns IDs, sequence numbers, timeouts and terminal outcomes.
The example decodes a receipt; it does not open a view or change an engine session.
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
        """Validate a raw mapping and return an immutable record.

        Raises:
            ValueError: The mapping violates this record's canonical View schema.
        """
        validate("view", cls._definition, value)
        return cls(**freeze(value))

    def to_wire(self) -> dict[str, Any]:
        """Return a fresh JSON-compatible copy, including nested mappings and lists."""
        return to_wire(self)


@dataclass(frozen=True, slots=True)
class InputView(_ViewRecord):
    """An open input form, including field schemas and engine-owned timeout metadata.

    This describes the form, not a person's answers. Submitted values are not
    included in public close events; they belong to the waiting extension.
    """

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
    """An open live interface at sequence `seq`, with immutable semantic nodes.

    Apply subsequent `LivePatch` operations in sequence order in your consumer;
    this snapshot does not mutate itself when another event arrives.
    """

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
    owner: Mapping[str, Any] | None = None


@dataclass(frozen=True, slots=True)
class ViewSnapshot(_ViewRecord):
    """A retained view document without open-session IDs or timeout metadata."""

    _definition = "view"
    title: str
    nodes: tuple[Mapping[str, Any], ...]
    description: str | None = None


@dataclass(frozen=True, slots=True)
class LivePatch(_ViewRecord):
    """Ordered semantic updates for `view_id`, ending at sequence `seq`.

    The containing `ViewEvent.first_seq` identifies the first sequence covered
    by a patch event. Operations remain immutable schema-validated mappings.
    """

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
    """A live interface's final outcome and optional retained document.

    Check `is_completed` and `reason` rather than assuming every close succeeds.
    `is_from_human` distinguishes a person's closure from a programmatic one;
    `view`, `summary`, `error` and attachment metadata may be absent.
    """

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
        """Validate a close receipt and decode its optional `ViewSnapshot`.

        Raises:
            ValueError: The receipt or its retained view violates the View schema.
        """
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
        """Decode a View event payload, separate from its stream envelope.

        Args:
            event_type: `"view.open"`, `"view.patch"` or `"view.close"`.
            value: The corresponding JSON payload, including its kind and view ID.

        Raises:
            KeyError: The event type is unsupported.
            ValueError: The payload, view identity or patch sequence is invalid.
        """
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
        """Return this typed View payload as fresh JSON-compatible data."""
        return to_wire(self)
