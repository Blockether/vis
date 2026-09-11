"""Session-bound Council; explicit pings can wake idle peers without rebinding the author."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Literal
from uuid import uuid4

from blockether.vis._contracts import validate

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
    _session: Any
    group_id: str
    _activation_id: str | None

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
        Every entry has host-owned source_ref; unavailable execution coordinates are null.
        Include the relevant turn/iteration when discussing another execution. Select
        individual pings, all, or none; no kind creates a tracker issue. Failed
        python_execution calls are already recorded as autocomplain without pings.

        A no-ping thread continuation answers the latest addressed entry only if it
        is an unanswered request, notifying its author. reply_to selects a request
        explicitly. Follow-ups and acknowledgements do not fall back to older requests.
        Explicit IDs can wake idle peers; 'all' selects active peers only. Required
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
