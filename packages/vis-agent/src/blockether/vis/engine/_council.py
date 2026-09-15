"""Session-bound Council; only managed teams can automatically wake idle sessions."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Literal
from uuid import uuid4

from blockether.vis._contracts import validate

from ._agents import Subagent

CouncilKind = Literal["complain", "coordination", "informational"]


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
    kind: CouncilKind
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
    kind: CouncilKind
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

    Obtain this handle with `Session.council`, rather than constructing it with
    internal binding fields. The handle captures its session's group and current
    activation when acquired; it is not a live alias for whichever run is latest.

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
        kind: CouncilKind,
        thread_id: int | None = None,
        title: str | None = None,
        ping: list[str] | str | None = None,
        idempotency_key: str | None = None,
        reply_required: bool = False,
        reply_to: int | None = None,
    ) -> CouncilEntry:
        """Publish a message, optionally requesting replies from selected sessions.

        Args:
            content: Message text. Keep secrets and private data out of messages.
            kind: `coordination` for work and questions, `informational` for
                findings, or `complain` for failures and concrete improvements.
                Complaints enter the persistent improve register, not an external
                issue tracker. Include sanitized evidence, expected versus actual
                behavior, impact and a workaround; distinguish facts from guesses.
            thread_id: Existing thread to continue; omit to start a new thread.
            title: Optional readable title for the message.
            ping: Session IDs, `"all"` for active peers, or no recipients. Explicit
                IDs wake only eligible managed teammates, not independent leaders.
            idempotency_key: Reuse this key when retrying uncertain IO. The same
                publication is returned without notifying recipients twice.
            reply_required: Track an answer obligation for each addressed recipient.
            reply_to: Explicit unanswered request to answer. A no-ping thread
                continuation otherwise answers only the latest addressed entry if
                it is an unanswered request; it never falls back to older requests.

        Returns:
            `CouncilEntry` with its entry/thread IDs and per-recipient `replies`.
            Pending, delivered and unavailable states are not completed answers.
            Inspect the entry later with `get` to see replies and their states.

        Each message owns its kind and host-generated `source_ref`. That reference
        identifies this publication, not a reported incident: include the original
        session and turn/iteration/form when reporting one. Failed Python tool
        executions already have an autocomplain entry; enrich its thread instead
        of duplicating it. Follow-ups and acknowledgements do not grant wake rights.
        """
        body = {
            "content": content,
            "kind": kind,
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
                }.items()
                if value is not None
            }
        )
        validate("council", "publish", body)
        return CouncilEntry.from_wire(self._call("POST", "/entries", body=body))

    def publish_spawn(
        self,
        task: str,
        *,
        provider: str | None = None,
        model: str | None = None,
        iteration_budget: int = 32,
        allowed_models: list[dict[str, str]] | None = None,
        key: str | None = None,
    ) -> Subagent:
        """Create a subagent and publish its delegated task through Council.

        Council must be enabled and the parent active with a complete checkpoint
        in its live runtime. Context is copied, not Python handles. Children share
        the checkout, inherit authorization, and consume their own bounded model
        iterations. A key makes retries idempotent within the parent turn; reusing
        it with different input is rejected. This is not an ordinary independent
        fork and does not grant broader permissions.
        """
        body = {"task": task, "iteration_budget": iteration_budget}
        body.update(
            {
                name: value
                for name, value in {
                    "provider": provider,
                    "model": model,
                    "allowed_models": allowed_models,
                    "key": key,
                }.items()
                if value is not None
            }
        )
        validate("agents", "spawn", body)
        return Subagent.from_wire(self._session._call("POST", "/agents", body=body))

    def subagents(self) -> tuple[Subagent, ...]:
        """Inspect team lineage, task, model, lifecycle, budget and human-input state.

        This lists the bound session's managed team, not all Council group members.
        The handle's group_id does not change session ownership.
        """
        return tuple(
            Subagent.from_wire(row) for row in self._session._call("GET", "/agents")
        )

    def cancel(self, session_id: str) -> dict[str, Any]:
        """Cancel an owned child and its descendants, including queued work."""
        body = {"session_id": session_id}
        validate("agents", "cancel", body)
        return self._session._call("POST", "/agents/cancel", body=body)

    def route(
        self, model: str, *, provider: str, session_id: str | None = None
    ) -> dict[str, Any]:
        """Change this session or an owned child at the next request boundary.

        Human model locks and inherited allowlists remain authoritative. The
        shared router is unchanged; cross-model cache reuse is not guaranteed.
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
        kind: CouncilKind,
        thread_id: int | None = None,
        title: str | None = None,
        idempotency_key: str | None = None,
    ) -> CouncilEntry:
        """Notify this bound session, including after the handle's activation ends.

        An active session receives a ping. Only a managed subagent may self-wake;
        an idle independent leader stays idle. Held queues are not resumed. Reusing
        an idempotency key returns the original entry without another delivery.
        """
        body = {
            "content": content,
            "kind": kind,
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
        """List roots and their kind in entry-ID order; replies do not reorder roots."""
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
        """Read a bounded page; never consumes or acknowledges a ping."""
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
