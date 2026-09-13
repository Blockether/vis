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
    """Session controls and a communication binding captured when the handle is acquired."""

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
        """Publish with an explicit per-message kind, independently of ping/reply policy.

        complain records failures or concrete improvements in the persistent improve
        register; coordination covers work/questions and informational findings/decisions.
        Include the goal, environment/version, preconditions, minimal reproduction steps
        and sanitized input/tool arguments, expected versus actual behavior, diagnostics,
        frequency, impact and workaround. Separate evidence from hypotheses; mark missing
        facts unknown or not attempted. Identify the affected session and turn/iteration/form.
        Every entry has host-owned source_ref for this publication, not another execution.
        Inspect original evidence with read_session(session_id); never copy secrets or
        private data into the report or replay unsafe operations. Select individual pings,
        all, or none; no kind creates a tracker issue. Failed python_execution calls are
        already recorded as autocomplain without pings, with failure/timeout, duration and
        a source-session lookup. Enrich their thread with an informational continuation
        instead of duplicating the report.

        A no-ping thread continuation answers the latest addressed entry only if it
        is an unanswered request, notifying its author. reply_to selects a request
        explicitly. Follow-ups and acknowledgements do not fall back to older requests.
        Explicit IDs wake only managed teammates, never independent leaders; 'all' selects active peers.
        requests report per-recipient states in replies; unavailable is not success.
        Retry uncertain IO with the same idempotency_key; it never notifies twice.
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
