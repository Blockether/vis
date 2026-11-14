"""Session groups: named sets of sessions in one project."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True, slots=True)
class SessionGroup:
    """A named group of sessions in one project.

    Get it from `ExecutionLayer.group`, `create_group` or `groups`. The record is
    a snapshot of one read. ``color`` is a palette token, for example ``blue``.
    ``archived_at`` is None while the group is active.
    """

    id: str
    name: str
    project_id: str | None = None
    color: str | None = None
    session_count: int = 0
    active_at: int | None = None
    created_at: int | None = None
    archived_at: int | None = None

    @property
    def is_archived(self) -> bool:
        return self.archived_at is not None

    @classmethod
    def from_wire(cls, value: Any) -> SessionGroup:
        if not isinstance(value, dict) or not all(
            isinstance(value.get(key), str) and value[key] for key in ("id", "name")
        ):
            raise ValueError("session group must have an id and a name")
        return cls(
            **{
                key: value[key]
                for key in cls.__dataclass_fields__
                if value.get(key) is not None
            }
        )
