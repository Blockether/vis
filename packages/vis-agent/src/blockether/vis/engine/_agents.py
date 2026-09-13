"""Managed subagents owned by a session, not arbitrary project peers."""

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
    depth: int
    iteration_budget: int
    iterations_used: int
    provider: str | None = None
    model: str | None = None
    routing_locked: bool = False
    pending_input: bool = False
    usage: dict[str, Any] | None = None

    @classmethod
    def from_wire(cls, value: dict[str, Any]) -> Subagent:
        validate("agents", "agent", value)
        return cls(
            **{key: value[key] for key in cls.__dataclass_fields__ if key in value}
        )
