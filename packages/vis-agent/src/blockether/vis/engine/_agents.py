"""Managed subagents that a session owns, not other project peers."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from blockether.vis._contracts import validate

from ._task_observer import _SessionTask

if TYPE_CHECKING:
    from ._client import Session


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
    role: str | None = None
    provider: str | None = None
    model: str | None = None
    routing_locked: bool = False
    pending_input: bool = False
    question: str | None = None
    question_entry_id: int | None = None
    budget_request: dict[str, Any] | None = None
    files_request: dict[str, Any] | None = None
    activity: str | None = None
    note: str | None = None
    usage: dict[str, Any] | None = None
    files: dict[str, list[str]] | None = None
    title: str | None = None
    result: str | None = None
    outcome: str | None = None
    created_at: int | None = None
    resumed_at: int | None = None
    finished_at: int | None = None
    worked_ms: int | None = None
    busy_since: int | None = None

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
    the working provider and model routes for a new child. Another provider failure gives code
    ``provider_failed``. Every record has the same keys
    for each status; a key without a value is ``None``. A running child has values in
    ``activity``, ``notes``, ``messages``, ``step_secs``, ``elapsed_secs`` and ``alerts``: its
    current progress. A child that ended without an answer has its last ``notes``.
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
    provider: str | None = None
    model: str | None = None
    warning: str | None = None
    error: dict[str, Any] | None = None
    alternatives: list[dict[str, str]] | None = None
    activity: str | None = None
    notes: list[str] | None = None
    messages: list[dict[str, Any]] | None = None
    step_secs: int | None = None
    elapsed_secs: int | None = None
    alerts: list[str] | None = None
    id: str | None = None
    after: list[str] | None = None
    result: str | None = None

    def __repr__(self) -> str:
        label = f"{self.id or self.session_id} {self.status}"
        if self.status == "asking":
            kind = "budget" if self.budget_request is not None else (
                "files" if self.files_request is not None else "question"
            )
            return f"{label} #{self.question_entry_id}; kind: {kind}"
        if self.error and self.error.get("code"):
            return f"{label} {self.error['code']}"
        if self.result:
            return f"{label} - {' '.join(self.result.split())}"
        return label

    __str__ = __repr__


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


class SubagentTask(_SessionTask[SubagentRecord]):
    """One attempt of a managed subagent, owned by the session that started it.

    `id` is the child ordinal, `session_id` is the child session and `task_id`
    identifies this attempt. A follow-up keeps the child and gets a new Task.
    Wait with `await task` or either Python wait function. `result()` does not wait.
    Use `record()` for current progress or a pending question. Answer the question
    through Council before waiting for the result. Child failure is a record;
    a transport or Task failure raises an exception.
    """

    _vis_cancel_on_await = True

    def __init__(self, session: Session, descriptor: dict[str, str]):
        validate("agents", "subagent_task", descriptor)
        super().__init__(session, descriptor["task_id"], "subagent")
        self._descriptor = descriptor.copy()
        self._terminal_record: SubagentRecord | None = None

    @property
    def id(self) -> str:
        """The child ordinal, such as `a1`."""
        return self._descriptor["id"]

    @property
    def session_id(self) -> str:
        """The session of the child, not the session that owns the Task."""
        return self._descriptor["session_id"]

    def _result_from(self, answer):
        from ._client import ProtocolError

        record = answer.get("record")
        if (
            record is None
            or record["session_id"] != self.session_id
            or record.get("id") not in (None, self.id)
        ):
            raise ProtocolError("Task record does not match the subagent attempt")
        return SubagentRecord.from_wire(record)

    def _cancelled_record(self, answer):
        self._terminal_record = self._result_from(answer)

    def _settlement(self):
        outcome = super()._settlement()
        if outcome[0] == "done":
            self._terminal_record = outcome[1]
        return outcome

    def record(self) -> SubagentRecord:
        """Read the current snapshot without waiting for the child to finish."""
        return self._terminal_record or self._result_from(self._status())

    def running(self) -> bool:
        return not self.done()
