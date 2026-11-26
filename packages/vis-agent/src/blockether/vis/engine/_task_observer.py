"""Observe session-owned Tasks through one task-only page loop per owner."""

from __future__ import annotations

import threading
from abc import abstractmethod
from contextvars import copy_context
from typing import Generic, Literal, TypeVar
from weakref import WeakKeyDictionary, WeakValueDictionary

from blockether.vis._contracts import validate
from blockether.vis._task_future import _HostFuture

_observers = WeakKeyDictionary()
_door_observers = {}
_lock = threading.Lock()
_T = TypeVar("_T")


class _SessionTask(_HostFuture[_T], Generic[_T]):
    def __init__(self, session, task_id, kind):
        super().__init__()
        self._session = session
        self._task_id = task_id
        self._kind = kind

    @property
    def task_id(self) -> str:
        """The identity of this Task in its owning session."""
        return self._task_id

    def _status(self):
        from ._client import ProtocolError

        answer = validate(
            "gateway",
            "task_status",
            self._session._call(
                "GET",
                "/tasks/:task-id",
                path={"task-id": self.task_id},
                query={"include_record": "true"},
            ),
        )
        task = answer["task"]
        if (task["kind"], task["id"], task["session_id"]) != (
            self._kind,
            self.task_id,
            self._session.id,
        ):
            raise ProtocolError("Task status does not match the session Task")
        return answer

    def cancel(self, msg=None) -> bool:
        """Ask the owning session to interrupt this work."""
        if self.done():
            return False
        return validate(
            "gateway",
            "task_cancel",
            self._session._call(
                "POST",
                "/tasks/:task-id/cancel",
                path={"task-id": self.task_id},
            ),
        )["requested"]

    def _settle_cancelled(self):
        super().cancel()

    def _settlement(
        self,
    ) -> (
        tuple[Literal["done"], _T]
        | tuple[Literal["running", "cancelled"], None]
        | tuple[Literal["failed"], BaseException]
    ):
        answer = self._status()
        row = answer["task"]
        state = row["state"]
        if state == "done":
            return state, self._result_from(answer)
        if state == "cancelled":
            self._cancelled_record(answer)
            return state, None
        if state == "running":
            return state, None
        return "failed", RuntimeError(row.get("error") or f"Task {self.task_id} failed")

    @abstractmethod
    def _result_from(self, answer) -> _T:
        raise NotImplementedError

    def _cancelled_record(self, answer):
        pass


class _CouncilWatch(_SessionTask):
    def _result_from(self, answer):
        return validate("gateway", "council_readiness", answer.get("value"))


def _admit(session, submit):
    # Do not open another observer for another Session wrapper of this owner.
    with _lock:
        owners = _observers.setdefault(session.client, WeakValueDictionary())
        observer = owners.get(session.id)
        if observer is None:
            observer = owners[session.id] = _Observer(session)
    return observer.admit(submit)


class _Observer:
    def __init__(self, session):
        self._session = session
        self._lock = threading.Lock()
        self._pending = {}
        self._stream = None

    def admit(self, submit):
        with self._lock:
            fresh = self._stream is None
            handles = submit()
            pending = [handle for handle in handles if not handle.done()]
            if not pending:
                return handles
            for handle in pending:
                self._pending.setdefault(handle._task_id, []).append(handle)
            if fresh:
                try:
                    self._stream = stream = _task_events(self._session)
                    threading.Thread(
                        target=self._run,
                        args=(stream,),
                        name="vis-task-observer",
                        daemon=True,
                    ).start()
                except Exception as error:
                    if self._stream is not None:
                        self._stream.close()
                    self._stream = None
                    self._pending.clear()
                    for handle in pending:
                        handle.set_exception(error)
            return handles

    def _run(self, stream):
        from ._client import TransportError

        barrier = 0
        try:
            with stream:
                for event in stream:
                    if event.type == "subscription.ready":
                        rows = event.data.get("tasks")
                        if rows is None:
                            continue
                        barrier = event.data["tasks_seq"]
                        with self._lock:
                            pending = tuple(self._pending)
                        running = {
                            row["id"] for row in rows if row["state"] == "running"
                        }
                        for task_id in pending:
                            if task_id not in running:
                                self._read_all(task_id)
                    elif event.type == "task.ended" and event.seq > barrier:
                        self._read_all(event.data["task"]["id"])
                    with self._lock:
                        if not self._pending:
                            stream.close()
                            self._stream = None
                            return
                    if event.type == "session.deleted":
                        break
                raise TransportError(
                    "Task observation ended before the host reported an outcome"
                )
        except Exception as error:
            with self._lock:
                self._stream = None
                pending, self._pending = self._pending, {}
            for handles in pending.values():
                for handle in handles:
                    handle.set_exception(error)

    def _read_all(self, task_id):
        with self._lock:
            handles = tuple(self._pending.get(task_id, ()))
        for handle in handles:
            self._read(handle)

    def _read(self, handle):
        # A Task admitted during a ready frame may not be in its snapshot yet.
        # Leave it registered when the detail read says it is still running.
        try:
            state, result = handle._settlement()
        except Exception as error:
            state, result = "failed", error
        if state == "running":
            return
        with self._lock:
            handles = self._pending[handle._task_id]
            handles.remove(handle)
            if not handles:
                del self._pending[handle._task_id]
        if state == "done":
            handle.set_result(result)
        elif state == "cancelled":
            handle._settle_cancelled()
        else:
            handle.set_exception(result)


def _observe_door(door, batch, handle, request):
    """Read a vector of nonblocking outcomes in one call per door and tick."""
    key = getattr(door, "_task_batch_key", None)
    if key is None:
        key = (id(getattr(door, "__self__", door)), getattr(door, "__name__", None))
    with _lock:
        observer = _door_observers.get(key)
        pending = (handle, copy_context(), request)
        if observer is not None:
            observer.append(pending)
            return
        pending_reads = _door_observers[key] = [pending]
        try:
            threading.Thread(
                target=_poll_door,
                args=(key, batch, pending_reads),
                name="vis-task-door-observer",
                daemon=True,
            ).start()
        except BaseException:
            del _door_observers[key]
            raise


def _poll_door(key, batch, pending):
    delay = threading.Event()
    interval = 0.02
    while True:
        with _lock:
            snapshot = tuple(pending)
        reading = [read for read in snapshot if not read[0].done()]
        if reading:
            try:
                answers = reading[0][1].run(
                    batch, [request for _, _, request in reading]
                )
                if not isinstance(answers, list) or len(answers) != len(reading):
                    raise ValueError("Malformed Task batch reply")
            except Exception as error:
                answers = [{"state": "failed", "error": error}] * len(reading)
            for (handle, context, _), answer in zip(reading, answers, strict=True):
                context.run(handle._observe_outcome, answer)
        ended = {read[0] for read in snapshot if read[0].done()}
        with _lock:
            changed = len(pending) != len(snapshot) or bool(ended)
            pending[:] = [read for read in pending if read[0] not in ended]
            if not pending:
                del _door_observers[key]
                return
        interval = 0.02 if changed else min(interval * 2, 0.25)
        delay.wait(interval)


def _task_events(session):
    from ._client import _SessionPollingEvents

    class PollingTasks(_SessionPollingEvents):
        def __init__(self, session):
            super().__init__(session)
            self.cursor = -1

        def _poll_idle_timeout(self):
            # A quiet Task has no observation deadline. Each page is still bounded.
            return None

        def _pump(self):
            # Application callbacks belong to the caller, not this reader.
            pass

        def _page(self):
            from ._client import ProtocolError

            answer = self.client.routes.get_session_events_since(
                self.session.id, query={"cursor": self.cursor, "tasks": "true"}
            )
            if (
                not isinstance(answer, dict)
                or not isinstance(answer.get("events"), list)
                or type(answer.get("cursor")) is not int
                or answer["cursor"] < 0
            ):
                raise ProtocolError("malformed Task event page")
            for row in answer["events"]:
                yield None, row
            self.cursor = answer["cursor"]

    return PollingTasks(session)
