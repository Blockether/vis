"""Join Council rooms and manage machine membership through the relay protocol.

Joining does not share local sessions. Configure the gateway's scoped Settings
separately. The relay operator can read messages; Rooms does not provide E2EE.
For room setup and scoped Settings, use the
[Council guide](https://vis.blockether.com/council.html#connect-machines-with-a-room).
For the room calls, see the
[Council API guide](https://vis.blockether.com/council-api.html#manage-rooms).
"""

from __future__ import annotations

import json
import math
import re
import secrets
from dataclasses import dataclass, field
from time import time
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import parse_qs, urlencode, urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener
from uuid import uuid4

from blockether.vis._contracts import schema, validate
from blockether.vis.engine._council import CouncilEntry, CouncilMember, CouncilThread

_SCHEMA = schema("rooms")
_LIMITS = _SCHEMA["x-vis-limits"]
_ROUTES = tuple(
    (route, re.compile("^" + re.sub(r"\{[^}]+\}", "[^/]+", route["path"]) + "$"))
    for route in _SCHEMA["x-vis-http"]
)


class RoomsError(RuntimeError):
    """A safe relay failure. The exception never includes credentials or content."""

    def __init__(self, status: int, code: str):
        self.status = status
        self.code = code
        super().__init__(f"Rooms HTTP {status} ({code})")


class RoomsTransportError(RuntimeError):
    """The request failed. A timed-out mutation may already have committed."""


class _NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def _origin(value: str) -> str:
    parsed = urlsplit(value)
    if (
        parsed.username is not None
        or parsed.password is not None
        or not parsed.hostname
        or parsed.query
        or parsed.fragment
        or parsed.path not in ("", "/")
        or not (
            parsed.scheme == "https"
            or (parsed.scheme == "http" and parsed.hostname in ("127.0.0.1", "::1"))
        )
    ):
        raise ValueError("Rooms needs an HTTPS origin or a loopback HTTP origin")
    return f"{parsed.scheme}://{parsed.netloc}".rstrip("/")


@dataclass(frozen=True, slots=True)
class MachineIdentity:
    """Persist this identity securely before joining. Never publish its credential."""

    machine_id: str
    name: str
    credential: str = field(repr=False)

    @classmethod
    def generate(cls, name: str) -> MachineIdentity:
        identity = cls(str(uuid4()), name, secrets.token_urlsafe(32))
        validate("rooms", "machine_registration", identity.registration())
        return identity

    def registration(self, *, can_create_rooms: bool = False) -> dict[str, Any]:
        return {
            "machine_id": self.machine_id,
            "name": self.name,
            "credential": self.credential,
            "can_create_rooms": can_create_rooms,
        }


@dataclass(frozen=True, slots=True)
class Machine:
    machine_id: str
    name: str
    can_create_rooms: bool
    created_at: int


@dataclass(frozen=True, slots=True)
class MachineDeletion:
    machine_id: str
    deleted_rooms: int
    retained_history: bool


@dataclass(frozen=True, slots=True)
class Room:
    room_id: str
    name: str
    owner_machine_id: str
    created_at: int


@dataclass(frozen=True, slots=True)
class Membership:
    machine_id: str
    name: str
    role: str
    joined_at: int


@dataclass(frozen=True, slots=True)
class Invite:
    invite_id: str
    room_id: str
    expires_at: int
    max_uses: int
    uses: int
    revoked: bool
    invite_url: str = field(repr=False)


@dataclass(frozen=True, slots=True)
class JoinedRoom:
    machine: Machine
    room: Room


@dataclass(frozen=True, slots=True)
class EntryPage:
    entries: tuple[CouncilEntry, ...]
    after: int
    has_more: bool


@dataclass(frozen=True, slots=True)
class ThreadPage:
    entries: tuple[CouncilThread, ...]
    after: int
    has_more: bool


class RoomsClient:
    """An explicit relay client, separate from GatewayClient and Push grants.

    Requests and responses use the canonical Rooms schema. Redirects are refused.
    Safe publication and redemption retries reuse their original request IDs.
    """

    def __init__(self, base_url: str, credential: str, *, timeout: float = 10):
        self.base_url = _origin(base_url)
        validate("rooms", "secret", credential)
        if not math.isfinite(timeout) or timeout <= 0:
            raise ValueError("timeout must be positive and finite")
        self._credential = credential
        self._timeout = timeout
        self._opener = build_opener(_NoRedirect())

    def _request(self, method, path, body=None, *, query=None, retry=False):
        route = next(
            (
                item
                for item, pattern in _ROUTES
                if item["method"] == method and pattern.fullmatch(path)
            ),
            None,
        )
        if route is None:
            raise ValueError("unknown Rooms operation")
        if route["query"]:
            validate("rooms", route["query"], query or {})
        elif query:
            raise ValueError("this Rooms operation has no query")
        payload = None
        if route["request"]:
            validate("rooms", route["request"], body)
            payload = json.dumps(body, ensure_ascii=False, allow_nan=False).encode(
                "utf-8"
            )
            if len(payload) > _LIMITS["request_bytes"]:
                raise ValueError("Rooms request exceeds its byte limit")
        url = self.base_url + path + ("?" + urlencode(query) if query else "")
        for attempt in range(2 if retry else 1):
            request = Request(
                url,
                data=payload,
                method=method,
                headers={
                    "Authorization": "Bearer " + self._credential,
                    "Content-Type": "application/json",
                    "Accept": "application/json",
                },
            )
            try:
                with self._opener.open(request, timeout=self._timeout) as response:
                    content = response.read(_LIMITS["response_bytes"] + 1)
                if len(content) > _LIMITS["response_bytes"]:
                    raise ValueError("Rooms response exceeds its byte limit")
                try:
                    result = json.loads(content)
                except (ValueError, UnicodeError):
                    raise ValueError("invalid Rooms JSON response") from None
                return validate("rooms", route["response"], result)
            except HTTPError as error:
                with error:
                    content = error.read(_LIMITS["response_bytes"] + 1)
                code = "http_error"
                try:
                    if len(content) <= _LIMITS["response_bytes"]:
                        failure = validate("rooms", "error", json.loads(content))
                        code = failure["error"]["code"]
                except (ValueError, UnicodeError):
                    pass
                raise RoomsError(error.code, code) from None
            except (URLError, TimeoutError, OSError):
                if retry and attempt == 0:
                    continue
                raise RoomsTransportError(
                    "Rooms request failed; a mutation may have committed"
                ) from None
        raise RoomsTransportError("Rooms request failed")

    @staticmethod
    def _path(room_id: str, suffix: str = "") -> str:
        validate("rooms", "id", room_id)
        return f"/v1/rooms/{room_id}{suffix}"

    def register(
        self, identity: MachineIdentity, *, can_create_rooms: bool = False
    ) -> Machine:
        """Register a machine with an administrator credential."""
        return Machine(
            **self._request(
                "POST",
                "/v1/rooms/machines",
                identity.registration(can_create_rooms=can_create_rooms),
            )
        )

    def set_moderator(self, machine_id: str, enabled: bool) -> Machine:
        validate("rooms", "id", machine_id)
        return Machine(
            **self._request(
                "PATCH",
                f"/v1/rooms/machines/{machine_id}",
                {"can_create_rooms": enabled},
            )
        )

    def delete_machine(self, machine_id: str) -> MachineDeletion:
        """Delete a machine and the rooms that it owns.

        Use the machine's own credential or the Rooms administrator token. Entries
        in other rooms stay, but the machine credential stops working.
        """
        validate("rooms", "id", machine_id)
        return MachineDeletion(
            **self._request("DELETE", f"/v1/rooms/machines/{machine_id}")
        )

    def machine(self) -> Machine:
        return Machine(**self._request("GET", "/v1/rooms/machine"))

    def rename(self, name: str) -> Machine:
        """Rename this machine. Its ID, credential and room memberships stay the same."""
        return Machine(**self._request("PATCH", "/v1/rooms/machine", {"name": name}))

    def rooms(self) -> tuple[Room, ...]:
        return tuple(Room(**item) for item in self._request("GET", "/v1/rooms"))

    def create_room(
        self, name: str, owner_machine_id: str, *, room_id: str | None = None
    ) -> Room:
        return Room(
            **self._request(
                "POST",
                "/v1/rooms",
                {
                    "room_id": room_id or str(uuid4()),
                    "name": name,
                    "owner_machine_id": owner_machine_id,
                },
            )
        )

    def delete_room(self, room_id: str) -> None:
        """Delete a room with its messages. All members lose access."""
        self._request("DELETE", self._path(room_id))

    def create_invite(
        self, room_id: str, *, expires_at: int | None = None, max_uses: int = 1
    ) -> Invite:
        result = self._request(
            "POST",
            self._path(room_id, "/invites"),
            {
                "invite_id": str(uuid4()),
                "token": secrets.token_urlsafe(32),
                "expires_at": expires_at
                if expires_at is not None
                else int(time() * 1000) + 86400000,
                "max_uses": max_uses,
            },
        )
        return Invite(**result["invite"], invite_url=result["invite_url"])

    def revoke_invite(self, room_id: str, invite_id: str) -> None:
        validate("rooms", "id", invite_id)
        self._request("DELETE", self._path(room_id, f"/invites/{invite_id}"))

    def join(
        self,
        invite_url: str,
        identity: MachineIdentity,
        *,
        request_id: str | None = None,
    ) -> JoinedRoom:
        """Redeem a link after confirmation. Reuse request_id after an uncertain result."""
        parsed = urlsplit(invite_url)
        if (
            _origin(f"{parsed.scheme}://{parsed.netloc}") != self.base_url
            or parsed.path != "/rooms/join"
            or parsed.query
        ):
            raise ValueError("invite link belongs to another relay or route")
        try:
            fragment = parse_qs(parsed.fragment, strict_parsing=True)
        except ValueError:
            raise ValueError("invalid invite fragment") from None
        if set(fragment) != {"invite"} or len(fragment["invite"]) != 1:
            raise ValueError("invite link needs one fragment token")
        if identity.credential != self._credential:
            raise ValueError("join needs this machine's credential")
        result = self._request(
            "POST",
            "/v1/rooms/join",
            {
                "request_id": request_id or str(uuid4()),
                "invite_token": fragment["invite"][0],
                "machine_id": identity.machine_id,
                "machine_name": identity.name,
            },
            retry=True,
        )
        return JoinedRoom(Machine(**result["machine"]), Room(**result["room"]))

    def members(self, room_id: str) -> tuple[Membership, ...]:
        return tuple(
            Membership(**item)
            for item in self._request("GET", self._path(room_id, "/members"))
        )

    def remove_member(self, room_id: str, machine_id: str) -> None:
        validate("rooms", "id", machine_id)
        self._request("DELETE", self._path(room_id, f"/members/{machine_id}"))

    def presence(
        self,
        room_id: str,
        sessions: list[dict[str, Any]],
        *,
        lease_seconds: int | None = None,
    ) -> int:
        body = {"sessions": sessions}
        if lease_seconds is not None:
            body["lease_seconds"] = lease_seconds
        return self._request("POST", self._path(room_id, "/presence"), body)[
            "expires_at"
        ]

    def sessions(self, room_id: str) -> tuple[CouncilMember, ...]:
        return tuple(
            CouncilMember(**item)
            for item in self._request("GET", self._path(room_id, "/sessions"))
        )

    def publish(
        self, room_id: str, session_id: str, content: str, *, kind: str, **options
    ) -> CouncilEntry:
        publication = {"content": content, "kind": kind, **options}
        if "thread_id" in publication or "reply_to" in publication:
            publication.pop("title", None)
        publication.setdefault("idempotency_key", str(uuid4()))
        return CouncilEntry.from_wire(
            self._request(
                "POST",
                self._path(room_id, "/entries"),
                {"session_id": session_id, "publication": publication},
                retry=True,
            )
        )

    def read(
        self,
        room_id: str,
        *,
        after: int = 0,
        limit: int | None = None,
        thread_id: int | None = None,
    ) -> EntryPage:
        query = {
            "after": after,
            **({"limit": limit} if limit is not None else {}),
            **({"thread_id": thread_id} if thread_id is not None else {}),
        }
        validate("rooms", "page_request", query)
        result = self._request("GET", self._path(room_id, "/entries"), query=query)
        return EntryPage(
            tuple(CouncilEntry.from_wire(item) for item in result["entries"]),
            result["after"],
            result["has_more"],
        )

    def threads(
        self, room_id: str, *, after: int = 0, limit: int | None = None
    ) -> ThreadPage:
        query = {"after": after, **({"limit": limit} if limit is not None else {})}
        validate("rooms", "page_request", query)
        result = self._request("GET", self._path(room_id, "/threads"), query=query)
        return ThreadPage(
            tuple(CouncilThread(**item) for item in result["entries"]),
            result["after"],
            result["has_more"],
        )

    def get(self, room_id: str, entry_id: int) -> CouncilEntry:
        validate("council", "entry_id", entry_id)
        return CouncilEntry.from_wire(
            self._request("GET", self._path(room_id, f"/entries/{entry_id}"))
        )

    def pending(self, room_id: str, session_id: str) -> tuple[CouncilEntry, ...]:
        validate("rooms", "id", session_id)
        return tuple(
            CouncilEntry.from_wire(item)
            for item in self._request(
                "GET", self._path(room_id, "/pending"), query={"session_id": session_id}
            )
        )

    def inbox(
        self, room_id: str, session_id: str, *, after: int = 0, limit: int | None = None
    ) -> EntryPage:
        query = {
            "session_id": session_id,
            "after": after,
            **({"limit": limit} if limit is not None else {}),
        }
        validate("rooms", "inbox_request", query)
        result = self._request("GET", self._path(room_id, "/inbox"), query=query)
        return EntryPage(
            tuple(CouncilEntry.from_wire(item) for item in result["entries"]),
            result["after"],
            result["has_more"],
        )

    def wake(
        self, room_id: str, session_id: str, content: str, *, kind: str, **options
    ) -> CouncilEntry:
        event = {"content": content, "kind": kind, **options}
        event.setdefault("idempotency_key", str(uuid4()))
        return CouncilEntry.from_wire(
            self._request(
                "POST",
                self._path(room_id, "/wake"),
                {"session_id": session_id, "event": event},
                retry=True,
            )
        )

    def receipts(self, room_id: str, receipts: list[dict[str, Any]]) -> None:
        self._request("POST", self._path(room_id, "/receipts"), {"receipts": receipts})
