"""Managed subagents that a session owns, not other project peers."""

from __future__ import annotations

import threading
from concurrent.futures import Future
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from blockether.vis._contracts import validate

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
    the working provider and model routes for a new child. Every record has the same keys
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


class SubagentTask(Future[SubagentRecord]):
    """One attempt of a managed subagent, owned by the session that started it.

    `id` is the child ordinal, `session_id` is the child session and `task_id`
    identifies this attempt. A follow-up keeps the child and gets a new Task.
    `result(timeout)` waits for the terminal record without cancelling on timeout.
    Use `record()` for current progress or a pending question. Answer the question
    through Council before waiting for the result. Child failure is a record;
    a transport or Task failure raises an exception.
    """

    def __init__(self, session: Session, descriptor: dict[str, str]):
        super().__init__()
        validate("agents", "subagent_task", descriptor)
        self._session = session
        self._descriptor = descriptor.copy()
        self._wake = threading.Event()
        self._terminal_record: SubagentRecord | None = None
        # Notify concurrent.futures.wait/as_completed before any user callback.
        self.add_done_callback(self._notify_cancelled)

    @property
    def id(self) -> str:
        """The child ordinal, such as `a1`."""
        return self._descriptor["id"]

    @property
    def session_id(self) -> str:
        """The session of the child, not the session that owns the Task."""
        return self._descriptor["session_id"]

    @property
    def task_id(self) -> str:
        """The Task identity of this attempt, such as `t2`."""
        return self._descriptor["task_id"]

    def _start(self):
        try:
            threading.Thread(
                target=self._watch, name="vis-subagent-result", daemon=True
            ).start()
        except RuntimeError as error:
            self.set_exception(error)

    @staticmethod
    def _notify_cancelled(future):
        if future.cancelled():
            future.set_running_or_notify_cancel()

    def _status(self):
        from ._client import ProtocolError

        status = validate(
            "gateway",
            "task_status",
            self._session._call(
                "GET",
                "/tasks/:task-id",
                path={"task-id": self.task_id},
                query={"include_record": "true"},
            ),
        )
        task = status["task"]
        record = status.get("record")
        if (
            record is None
            or task["kind"] != "subagent"
            or task["id"] != self.task_id
            or task["session_id"] != self._session.id
            or record["session_id"] != self.session_id
            or record.get("id") not in (None, self.id)
        ):
            raise ProtocolError("Task status does not match the subagent attempt")
        return status

    def record(self) -> SubagentRecord:
        """Read the current snapshot without waiting for the child to finish."""
        return self._terminal_record or SubagentRecord.from_wire(
            self._status()["record"]
        )

    def running(self) -> bool:
        return not self.done()

    def cancel(self) -> bool:
        """Ask the host to cancel this attempt. A refusal leaves the Task unchanged."""
        if self.done():
            return False
        accepted = validate(
            "gateway",
            "task_cancel",
            self._session._call(
                "POST", "/tasks/:task-id/cancel", path={"task-id": self.task_id}
            ),
        )["requested"]
        if accepted:
            self._wake.set()
        return accepted

    def _watch(self):
        delay = 0.1
        try:
            while True:
                task = self._status()
                state = task["task"]["state"]
                if state != "running":
                    break
                self._wake.wait(delay)
                self._wake.clear()
                delay = min(1.0, delay * 2)
            record = SubagentRecord.from_wire(task["record"])
            if state not in ("cancelled", "done"):
                raise RuntimeError(
                    task["task"].get("error") or f"Task {self.task_id} failed"
                )
        except Exception as error:
            self.set_exception(error)
        else:
            self._terminal_record = record
            if state == "cancelled":
                super().cancel()
            else:
                self.set_result(record)
