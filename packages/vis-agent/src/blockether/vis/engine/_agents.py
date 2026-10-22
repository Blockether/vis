"""Managed subagents that a session owns, not other project peers."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from blockether.vis._contracts import validate


@dataclass(frozen=True, slots=True)
class Subagent:
    session_id: str
    parent_id: str
    leader_id: str
    team_id: str
    task: str
    status: str
    iteration_budget: int
    iterations_used: int
    provider: str | None = None
    model: str | None = None
    routing_locked: bool = False
    pending_input: bool = False
    usage: dict[str, Any] | None = None
    files: dict[str, list[str]] | None = None

    @classmethod
    def from_wire(cls, value: dict[str, Any]) -> Subagent:
        validate("agents", "agent", value)
        return cls(
            **{key: value[key] for key in cls.__dataclass_fields__ if key in value}
        )


@dataclass(frozen=True, slots=True)
class SubagentRecord:
    """The state of one subagent after a control: answer, open question or request.

    ``status`` is ``asking`` when the child waits for its parent. Then ``question``,
    ``budget_request`` or ``files_request`` tells what it needs. ``held`` means that a
    human holds the queue of the child; only a person can release it. ``unknown`` names a
    session that is not a subagent of the caller. ``warning`` reports a file overlap. A child
    that a 429 stopped is ``failed`` with ``error`` (code ``rate_limited``) and ``alternatives``:
    the working provider and model routes for a new child.
    """

    session_id: str
    status: str
    answer: str | None = None
    question: str | None = None
    question_entry_id: int | None = None
    budget_request: dict[str, Any] | None = None
    files_request: dict[str, Any] | None = None
    files: dict[str, list[str]] | None = None
    iteration_budget: int | None = None
    iterations_used: int | None = None
    warning: str | None = None
    error: dict[str, Any] | None = None
    alternatives: list[dict[str, str]] | None = None

    @classmethod
    def from_wire(cls, value: dict[str, Any]) -> SubagentRecord:
        validate("agents", "record", value)
        return cls(
            **{key: value[key] for key in cls.__dataclass_fields__ if key in value}
        )

    @classmethod
    def many(cls, value: list[dict[str, Any]]) -> tuple[SubagentRecord, ...]:
        validate("agents", "records", value)
        return tuple(cls.from_wire(row) for row in value)
