"""Session-bound Council. Only managed teams can automatically wake idle sessions."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any
from uuid import uuid4

from blockether.vis._contracts import validate

from ._agents import Subagent, SubagentRecord, SubagentTask


@dataclass(frozen=True, slots=True)
class CouncilSource:
    session_id: str
    turn: int | None
    iteration: int | None
    form: int | None
    operation_id: str | None = None
    session_state_id: str | None = None
    session_turn_soul_id: str | None = None
    session_turn_state_id: str | None = None
    session_turn_iteration_id: str | None = None
    tool_call_id: str | None = None

    @classmethod
    def from_wire(cls, data):
        validate("council", "source_ref", data)
        return cls(
            data["session_id"],
            (data["scope"] or {}).get("turn"),
            (data["scope"] or {}).get("iter"),
            (data["scope"] or {}).get("next_form"),
            data.get("operation_id"),
            data.get("session_state_id"),
            data.get("session_turn_soul_id"),
            data.get("session_turn_state_id"),
            data.get("session_turn_iteration_id"),
            data.get("tool_call_id"),
        )


@dataclass(frozen=True, slots=True)
class CouncilReply:
    session_id: str
    state: str
    reply_entry_id: int | None = None


@dataclass(frozen=True, slots=True)
class CouncilEntry:
    entry_id: int
    thread_id: int
    group_id: str | None
    content: str
    author_session_id: str
    created_at: int
    source: str
    ping: tuple[str, ...]
    title: str | None = None
    source_ref: CouncilSource | None = None
    reply_required: bool = False
    reply_to: int | None = None
    replies: tuple[CouncilReply, ...] = ()

    @classmethod
    def from_wire(cls, data):
        validate("council", "entry", data)
        return cls(
            **{
                **data,
                "ping": tuple(data["ping"]),
                "replies": tuple(
                    CouncilReply(**item) for item in data.get("replies", [])
                ),
                "source_ref": CouncilSource.from_wire(data["source_ref"])
                if "source_ref" in data
                else None,
            }
        )


@dataclass(frozen=True, slots=True)
class CouncilMember:
    session_id: str
    title: str
    state: str


@dataclass(frozen=True, slots=True)
class CouncilThread:
    thread_id: int
    title: str
    author_session_id: str
    created_at: int


@dataclass(frozen=True, slots=True)
class CouncilPage:
    entries: tuple[CouncilEntry | CouncilThread, ...]
    after: int
    has_more: bool

    @classmethod
    def from_wire(cls, data, row, definition):
        validate("council", definition, data)
        return cls(
            tuple(row(item) for item in data["entries"]),
            data["after"],
            data["has_more"],
        )


@dataclass(frozen=True, slots=True)
class Council:
    """Communicate with other Vis sessions.

    Get this handle with `Session.council`. Do not construct it with internal binding
    fields. When acquired, the handle captures its session's group and current
    activation. It does not follow whichever run is latest.

    Use `members` to discover active peers, `publish` to send a message and `get`
    to inspect replies. Publication, delivery and a completed answer are separate
    states. `threads` and `read` return cursor-based pages of conversation history.

    Communication requires an available Council group. A disabled or missing
    group is reported when a communication operation or `group_id` needs it.
    """

    _session: Any
    _group_id: str
    _activation_id: str | None
    _binding_error: Exception | None = field(default=None, repr=False, compare=False)

    @property
    def group_id(self) -> str:
        """Return the captured group or report why communication is unavailable."""
        if self._binding_error is not None:
            raise self._binding_error
        return self._group_id

    def _call(self, method, suffix, **kwargs):
        if method == "GET":
            kwargs["query"] = {**kwargs.get("query", {}), "group_id": self.group_id}
        return self._session._call(method, "/council" + suffix, **kwargs)

    def members(self) -> tuple[CouncilMember, ...]:
        """Active session ids, titles and states. This list is only a snapshot."""
        rows = self._call("GET", "/members")
        return tuple(
            CouncilMember(**validate("council", "member", row)) for row in rows
        )

    def publish(
        self,
        content: str,
        *,
        thread_id: int | None = None,
        title: str | None = None,
        ping: list[str] | str | None = None,
        idempotency_key: str | None = None,
        reply_required: bool = False,
        reply_to: int | None = None,
        files: dict[str, list[str]] | None = None,
    ) -> CouncilEntry:
        """Publish a message, optionally requesting replies from selected sessions.

        Args:
            content: Message text. Keep secrets and private data out of messages.
            thread_id: Existing thread to continue. Omit it to start a new thread.
            title: Optional readable title for the message.
            ping: Session IDs, `"all"` for active peers, or no recipients. Explicit
                IDs wake an idle peer of the group or an eligible managed teammate.
            idempotency_key: Reuse this key when retrying uncertain IO. The same
                publication is returned without notifying recipients twice.
            reply_required: Track an answer obligation for each addressed recipient.
            reply_to: Explicit unanswered request to answer. Without it, a no-ping
                thread continuation answers only the latest addressed entry, and only
                if that entry is an unanswered request. It never falls back to older
                requests.
            files: File rules for the follow-up task of the one pinged idle child
                session. They apply to that attempt only; omit them to use the
                child's original spawn grant.

        Returns:
            `CouncilEntry` with its entry/thread IDs and per-recipient `replies`.
            Pending, delivered and unavailable states are not completed answers.
            Inspect the entry later with `get` to see replies and their states.

        Each message owns its host-generated `source_ref`. That reference identifies
        this publication, not a reported incident. When you report an incident,
        include the original session and turn/iteration/form. Follow-ups and
        acknowledgements do not grant wake rights.
        """
        body = {
            "content": content,
            "group_id": self.group_id,
            "activation_id": self._activation_id,
            "idempotency_key": str(uuid4())
            if idempotency_key is None
            else idempotency_key,
        }
        body.update(
            {
                key: value
                for key, value in {
                    "thread_id": thread_id,
                    "title": title,
                    "ping": ping,
                    "reply_required": reply_required
                    if reply_required is not False
                    else None,
                    "reply_to": reply_to,
                    "files": files,
                }.items()
                if value is not None
            }
        )
        validate("council", "publish", body)
        return CouncilEntry.from_wire(self._call("POST", "/entries", body=body))

    def spawn(
        self,
        task: str,
        *,
        title: str | None = None,
        role: str | None = None,
        provider: str | None = None,
        model: str | None = None,
        iteration_budget: int | None = None,
        allowed_models: list[dict[str, str]] | None = None,
        files: dict[str, list[str]] | None = None,
        key: str | None = None,
    ) -> Subagent:
        """Create a subagent and publish its delegated task through Council.

        Council must be enabled. The parent must be active, with a complete checkpoint
        in its live runtime. Context is copied, but Python handles are not. Children
        share the checkout, inherit authorization and use their own bounded model
        iterations. A key makes retries idempotent within the parent turn, and reusing
        it with different input is rejected. This is not an ordinary independent fork,
        and it does not grant broader permissions. ``files`` narrows file access with
        ``write``, ``read_only`` and ``deny`` rules: paths or globs, also for future files.
        ``role`` is ``research``, ``implement`` or ``verify``: a label for people that sets
        no limit. ``title`` is a short label for people: one line of 1 to 60
        characters.
        """
        body = {"task": task}
        body.update(
            {
                name: value
                for name, value in {
                    "title": title,
                    "role": role,
                    "provider": provider,
                    "iteration_budget": iteration_budget,
                    "model": model,
                    "allowed_models": allowed_models,
                    "files": files,
                    "key": key,
                }.items()
                if value is not None
            }
        )
        validate("agents", "spawn", body)
        return Subagent.from_wire(self._session._call("POST", "/agents", body=body))

    def timeline(self, session_id: str) -> dict[str, Any]:
        """Return the steps of one owned subagent, oldest first.

        ``items`` holds each turn start, progress note, Council message and the end of the
        current task with its ``status``, one-line result and optional ``outcome``.
        Each item has ``at`` in epoch milliseconds.
        """
        return validate(
            "agents",
            "timeline",
            self._session._call(
                "GET", "/agents/:child-id/timeline", path={"child-id": session_id}
            ),
        )

    def _records(self, suffix, body, definition):
        validate("agents", definition, body)
        return SubagentRecord.many(
            self._session._call("POST", "/agents" + suffix, body=body)
        )

    def _child_reference(self, child):
        if isinstance(child, SubagentTask):
            if child._session != self._session:
                raise ValueError("the SubagentTask belongs to another session")
            return child.session_id
        return child

    def changed(self, *, after: str | None = None):
        """Return a Task for the next batch of open child requests.

        Omit `after` to include requests that are already open. Pass the result's
        `cursor` to the next call. Cancelling the watch does not cancel a child.
        """
        from ._task_observer import _admit, _CouncilWatch

        body = {"after": after} if after is not None else {}
        validate("gateway", "council_watch_request", body)

        immediate = False

        def submit():
            nonlocal immediate
            row = validate(
                "gateway",
                "council_watch_task",
                self._session.client.routes.post_session_council_changed(
                    self._session.id, body=body
                ),
            )
            watch = _CouncilWatch(self._session, row["id"], row["kind"])
            if "value" in row:
                watch.set_result(watch._result_from(row))
                immediate = True
            return [watch]

        (watch,) = _admit(self._session, submit)
        if immediate:
            watch._acknowledge()
        return watch

    def run(self, tasks: list[str | dict[str, Any]]) -> list[SubagentTask]:
        """Start each task and return its handle in input order without waiting.

        Give a task as text or a dict with `task` and optional `title`, `role`,
        `model`, `provider`, `iteration_budget`, `files` and `after`. Use `after`
        for dependencies. To start a follow-up, give `task` and `child`, with
        optional `files` for that attempt only; without `files` the child has
        its original spawn grant. Both take child IDs, child
        session IDs or handles from this session. A child request does not settle the Task.
        """
        from ._client import ProtocolError

        def normalize(task):
            if not isinstance(task, dict):
                return task
            task = task.copy()
            if "child" in task:
                task["child"] = self._child_reference(task["child"])
            if isinstance(task.get("after"), list):
                task["after"] = [
                    self._child_reference(child) for child in task["after"]
                ]
            return task

        body = {
            "tasks": [normalize(task) for task in tasks]
            if isinstance(tasks, list)
            else tasks
        }
        validate("agents", "run", body)
        from ._task_observer import _admit

        def submit():
            descriptors = validate(
                "agents",
                "subagent_tasks",
                self._session._call("POST", "/agents/run", body=body),
            )
            if len(descriptors) != len(tasks) or len(
                {row["task_id"] for row in descriptors}
            ) != len(tasks):
                raise ProtocolError(
                    "Council.run did not return one Task for each input"
                )
            return [SubagentTask(self._session, row) for row in descriptors]

        return _admit(self._session, submit)

    def extend(self, session_id: str, iterations: int) -> SubagentRecord:
        """Give a subagent more model iterations; the budget has no upper limit.

        Use it to answer a budget request, or before a follow-up of a budget-limited child.
        """
        body = {"session_id": session_id, "iterations": iterations}
        return self._records("/extend", body, "extend")[0]

    def deny(self, session_id: str, reason: str) -> SubagentRecord:
        """Refuse the open budget or file request of a subagent, with a short reason."""
        body = {"session_id": session_id, "reason": reason}
        return self._records("/deny", body, "deny")[0]

    def answer(self, session_id: str, text: str) -> SubagentRecord:
        """Answer the open question of a subagent for its parent; the child continues."""
        body = {"session_id": session_id, "text": text}
        return self._records("/answer", body, "answer")[0]

    def grant(self, session_id: str, files: dict[str, list[str]]) -> SubagentRecord:
        """Add ``write``, ``read_only`` or ``deny`` file rules to the grant of a subagent.

        A grant gives at most your own access. A write rule that the child can already
        write changes nothing. A write rule under a ``read_only`` or ``deny`` rule of the
        child fails: use `run` with `child` and `files` for a new scope. The record has a
        ``warning`` for unchanged write rules and for an active sibling that can write
        the same files.
        """
        body = {"session_id": session_id, "files": files}
        return self._records("/grant", body, "grant")[0]

    def route(
        self, model: str, *, provider: str, session_id: str | None = None
    ) -> dict[str, Any]:
        """Change this session or an owned child at the next request boundary.

        Human model locks and inherited allowlists still take priority. The shared
        router is unchanged. Cache reuse across models is not guaranteed.
        """
        body = {"model": model, "provider": provider}
        if session_id is not None:
            body["session_id"] = session_id
        validate("agents", "route", body)
        return self._session._call("POST", "/agents/route", body=body)

    def wake(
        self,
        content: str,
        *,
        thread_id: int | None = None,
        title: str | None = None,
        idempotency_key: str | None = None,
    ) -> CouncilEntry:
        """Notify this bound session, including after the handle's activation ends.

        An active session receives a ping. Only a managed subagent can wake itself. An
        idle independent leader stays idle. Held queues are not resumed. Reusing an
        idempotency key returns the original entry without another delivery.
        """
        body = {
            "content": content,
            "group_id": self.group_id,
            "idempotency_key": str(uuid4())
            if idempotency_key is None
            else idempotency_key,
        }
        body.update(
            {
                key: value
                for key, value in {
                    "thread_id": thread_id,
                    "title": title,
                }.items()
                if value is not None
            }
        )
        validate("council", "wake", body)
        return CouncilEntry.from_wire(self._call("POST", "/wake", body=body))

    def threads(self, *, after: int = 0, limit: int = 50) -> CouncilPage:
        """List roots in entry-ID order. Replies do not reorder roots."""
        query = {"after": after, "limit": limit}
        validate("council", "page_request", query)
        return CouncilPage.from_wire(
            self._call("GET", "/threads", query=query),
            lambda d: CouncilThread(**d),
            "thread_page",
        )

    def read(
        self, *, thread_id: int | None = None, after: int = 0, limit: int = 50
    ) -> CouncilPage:
        """Read a bounded page. This never consumes or acknowledges a ping."""
        query = {"after": after, "limit": limit}
        if thread_id is not None:
            query["thread_id"] = thread_id
        validate("council", "page_request", query)
        return CouncilPage.from_wire(
            self._call("GET", "/entries", query=query),
            CouncilEntry.from_wire,
            "entry_page",
        )

    def get(self, entry_id: int) -> CouncilEntry:
        """Read the full content behind a preview, in this handle's group."""
        validate("council", "get_request", {"entry_id": entry_id})
        return CouncilEntry.from_wire(
            self._call("GET", "/entries/:entry-id", path={"entry-id": entry_id})
        )
