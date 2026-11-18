"""Shared engine session API and explicit HTTP gateway transport.

ExecutionLayer owns the route and session API without choosing a transport.
GatewayClient adds authenticated HTTP, leases and bounded SSE. A timeout never
means that a submitted mutation was rolled back.
"""

from __future__ import annotations

import asyncio
import gzip
import http.client
import ipaddress
import json
import math
import re
import socket
import threading
import time
import uuid
import zlib
from abc import ABC, abstractmethod
from collections.abc import Generator, Iterable, Mapping
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import PurePosixPath
from typing import (
    TYPE_CHECKING,
    Any,
    BinaryIO,
    Generic,
    NotRequired,
    Self,
    TypeAlias,
    TypedDict,
    TypeVar,
    cast,
    overload,
)
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urlencode, urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener

from blockether.vis._contracts import definition, schema, validate
from blockether.vis.activity import ActivityProjection
from blockether.vis.engine import _types
from blockether.vis.engine._groups import SessionGroup
from blockether.vis.engine._models import Model, SessionModel
from blockether.vis.views import EVENT_DEFINITIONS, InputView, LiveView, ViewEvent

if TYPE_CHECKING:
    from blockether.vis.extension import LiveAttachment

    from ._agent import ResponseModel
    from ._council import Council
    from ._sounds import Sounds
    from ._speech import Speech
    from ._types import StreamEvent

JSONValue: TypeAlias = (
    str | int | float | bool | None | list["JSONValue"] | dict[str, "JSONValue"]
)
Query: TypeAlias = Mapping[str, str | int | float | bool | None]


class SessionOptions(TypedDict):
    """The session creation options of a project, from `session_options`.

    Give them to `create_session` with `**`. `root` is the folder that the
    session works in. `channel` names the kind of client.
    """

    root: str
    channel: NotRequired[str]


_PROTOCOL = definition("gateway", "handshake")["properties"]["protocol"]["const"]
_MIN_GATEWAY = definition("gateway", "handshake")["properties"]["min_gateway"]["const"]
_ROUTES = schema("gateway")["x-vis-routes"]
_LEASE_POLICY = schema("gateway")["x-vis-client-lease"]
_MAX_DECISION_ARCHIVE = definition("gateway", "decision_archive_bytes")["maximum"]
_TURN_TERMINAL_EVENTS = frozenset(
    branch["const"]
    for branch in definition("gateway", "session_event_type")["oneOf"]
    if branch.get("x-vis-turn-terminal")
)
_JOB_EVENTS = {
    branch["x-vis-direction"]: branch["const"]
    for branch in definition("gateway", "job_event_type")["oneOf"]
}
# A polling stream waits this long after a page with an event, and doubles the
# wait after each quiet page up to the maximum.
_POLL_MIN_SECONDS = 0.1
_POLL_MAX_SECONDS = 1.0
_TURN_SETTLED_STATUSES = frozenset(
    {"completed", "failed", "cancelled", "suspended", "error"}
)
# Events after which a turn record cannot change again, or no longer exists.
_TURN_END_EVENTS = _TURN_TERMINAL_EVENTS | {"turn.queued.deleted", "session.deleted"}


class TransportError(RuntimeError):
    """Connection failed or the client has been closed."""


class VisTimeout(TransportError):
    """Deadline expired. The remote operation can still be running."""


class ProtocolError(TransportError):
    """Peer is incompatible or returned malformed protocol data."""


class GatewayError(RuntimeError):
    """HTTP failure with a safe code, never the gateway's free-form message.

    Decision input-limit errors also expose ``input_tokens`` and ``max_input_tokens``.
    These are ``None`` when the gateway does not supply valid numeric diagnostics.
    A refused view action, such as a press on a disconnected app, has its
    ``reason`` token as ``code``.
    """

    def __init__(
        self,
        status: int,
        code: str,
        *,
        input_tokens: int | None = None,
        max_input_tokens: int | None = None,
    ):
        self.status = status
        self.code = code
        self.input_tokens = input_tokens
        self.max_input_tokens = max_input_tokens
        message = f"Gateway HTTP {status} ({code})"
        if code == "input-too-long":
            if input_tokens is not None and max_input_tokens is not None:
                message += (
                    f": Decision input has {input_tokens} tokens; "
                    f"the per-question limit is {max_input_tokens}."
                )
            else:
                message += ": Decision input exceeds the per-question token limit."
            message += (
                " Shorten the state, question instructions or criteria."
                " Input is not truncated."
            )
        super().__init__(message)


class _NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


_REFUSAL_REASON = re.compile(r"[a-z][a-z0-9_]{0,63}")


def _gateway_error(
    status: int, content: bytes, token: str | None = None
) -> GatewayError:
    """Decode the shared error envelope without exposing its message or credentials."""
    code = "http_error"
    counts = {}
    try:
        body = json.loads(content)
        error = body.get("error")
        if isinstance(error, dict) and isinstance(error.get("type"), str):
            code = error["type"]
            if token and token in code:
                code = "http_error"
            if code == "input-too-long":
                validate("gateway", "error_response", {"error": error})
                counts = {
                    key: int(error[key])
                    for key in ("input_tokens", "max_input_tokens")
                    if key in error and not (token and token in str(int(error[key])))
                }
        elif body.get("is_accepted") is False:
            # A refused view action answers its outcome. Its reason is a closed token.
            reason = body.get("reason")
            if isinstance(reason, str) and _REFUSAL_REASON.fullmatch(reason):
                if not (token and token in reason):
                    code = reason
    except (ValueError, AttributeError):
        pass
    return GatewayError(status, code, **counts)


# A smaller JSON body costs more to compress than it saves.
_GZIP_MIN_BYTES = 1024


def _is_loopback(host: str) -> bool:
    """Tell if a gateway host is this machine, where compression saves nothing."""
    if host == "localhost":
        return True
    try:
        return ipaddress.ip_address(host).is_loopback
    except ValueError:
        return False


def _inflated(content: bytes, limit: int) -> bytes:
    """Give at most `limit` plain bytes of a gzip body that can be cut short."""
    try:
        return zlib.decompressobj(31).decompress(content, limit)
    except zlib.error:
        return b""


class _Inflated:
    """One whole gzip answer, read as its plain bytes."""

    def __init__(self, raw):
        self._raw = raw
        self.status = raw.status
        self.headers = raw.headers
        del self.headers["Content-Encoding"]
        del self.headers["Content-Length"]

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self._raw.close()

    def read(self) -> bytes:
        try:
            return gzip.decompress(self._raw.read())
        except (EOFError, zlib.error):
            raise OSError("gzip answer cut short") from None


def _duration(value: float) -> float:
    if not math.isfinite(value) or value <= 0:
        raise ValueError("timeout must be positive and finite")
    return value


def _segment(value: Any) -> str:
    value = str(value)
    if not value or value in {".", ".."}:
        raise ValueError("path identifiers must be nonempty, non-dot segments")
    return quote(value, safe="")


def _response_socket(raw) -> socket.socket | None:
    """Answer the socket of an open HTTP response, when it has one."""
    return getattr(getattr(getattr(raw, "fp", None), "raw", None), "_sock", None)


def _set_read_timeout(raw, seconds: float) -> None:
    """Set the read timeout of an open HTTP response socket, when it has one."""
    sock = _response_socket(raw)
    if sock is not None:
        sock.settimeout(seconds)


def _group_id(group: SessionGroup | str) -> str:
    return group.id if isinstance(group, SessionGroup) else group


def _project_scope(root: str | None, project_id: str | None, id_name: str) -> dict:
    """Name one project by its folder or by its ID, in the field names of a route."""
    if (root is None) == (project_id is None):
        raise ValueError("give root or project_id, not both")
    return {"root": root} if root is not None else {id_name: project_id}


def _group(data: Any) -> SessionGroup:
    try:
        return SessionGroup.from_wire(data)
    except ValueError:
        raise ProtocolError("invalid session group response") from None


def _agent_name(row: Any) -> str:
    """The name of an `agent_name` setting answer."""
    if not isinstance(row, dict) or row.get("id") != "agent_name":
        raise ProtocolError("invalid agent_name setting answer")
    value = row.get("value")
    if not isinstance(value, str) or not value.strip():
        raise ProtocolError("invalid agent_name setting answer")
    return value.strip()


def _machine_name(row: Any) -> str | None:
    """The effective name of a `machine_name` setting answer. Blank is no name."""
    if not isinstance(row, dict) or row.get("id") != "machine_name":
        raise ProtocolError("invalid machine_name setting answer")
    value = row.get("value")
    if value is not None and not isinstance(value, str):
        raise ProtocolError("invalid machine_name setting answer")
    return (value or "").strip() or None


def _field(data: Any, name: str, kind: type) -> Any:
    """Validate a consumed field without reflecting a potentially sensitive body."""
    value = data.get(name) if isinstance(data, dict) else None
    if type(value) is not kind or (kind is str and not value):
        raise ProtocolError(f"invalid response field: {name}")
    if type(value) is int and value < 0:
        raise ProtocolError(f"invalid response field: {name}")
    return value


@dataclass(frozen=True, slots=True)
class Response:
    """Response bytes with their status and headers, before content decoding.

    Returned by binary and transcript APIs. Use `content` directly for files,
    inspect `headers` for their media type, or call `json` for JSON transcripts.
    `json` raises `ProtocolError` for malformed or non-JSON content.
    """

    status: int
    content: bytes
    headers: dict[str, str]

    def json(self) -> Any:
        """Decode `content` as JSON, or raise `ProtocolError` if it is invalid."""
        try:
            return json.loads(self.content)
        except (ValueError, UnicodeError):
            raise ProtocolError("invalid JSON response") from None


@dataclass(frozen=True, slots=True)
class Event:
    """Typed envelope that `Events` yields. Payload fields depend on `type`.

    Attributes:
        type: Event name identifying how to interpret the payload.
        session_id: Conversation that emitted the event.
        seq: Sequence number for a journal event, when supplied.
        cursor: Replay position from a subscription control event, when
            supplied.
        turn_id: Request associated with this event, or `None` for session
            events.
        data: Event-specific dictionary. It is not a final turn result by
            itself. A check of `type` gives `data` the typed fields of that
            event type, from the payload schema of the gateway contract.
        activity: Validated activity projection, when this event carries one.
        view: Validated view event, when this event carries one.

    Stream readers validate wire records through `from_wire`. Use `Turn.read` or
    `Turn.wait` for the final result of a request. Do not treat each event as a
    completed response.
    """

    type: str
    session_id: str
    seq: int | None = None
    cursor: int | None = None
    turn_id: str | None = None
    data: Mapping[str, Any] = field(default_factory=dict)
    activity: ActivityProjection | None = None
    view: ViewEvent | None = None

    @classmethod
    def from_wire(cls, value: Any) -> Event:
        event_type = _field(value, "type", str)
        session_id = _field(value, "session_id", str)
        for name in ("seq", "cursor"):
            if name in value:
                _field(value, name, int)
        turn_id = value.get("turn_id")
        if turn_id is not None:
            _field(value, "turn_id", str)
        if event_type == "subscription.ready":
            _field(value, "cursor", int)
        else:
            _field(value, "seq", int)
        activity = None
        if event_type == "block.activity":
            _field(value, "iteration", int)
            _field(value, "form_index", int)
            try:
                activity = ActivityProjection.from_wire(value.get("activity"))
            except ValueError:
                raise ProtocolError("invalid Activity event") from None
        envelope = {"type", "session_id", "seq", "cursor", "turn_id"}
        if activity is not None:
            envelope.add("activity")
        view = None
        if event_type in EVENT_DEFINITIONS:
            view_fields = {"kind", "view_id", "view", "patch", "result", "first_seq"}
            try:
                view = ViewEvent.from_wire(
                    event_type, {k: v for k, v in value.items() if k in view_fields}
                )
            except ValueError:
                raise ProtocolError("invalid View event") from None
            envelope.update(view_fields)
        return cls(
            type=event_type,
            session_id=session_id,
            seq=value.get("seq"),
            cursor=value.get("cursor"),
            turn_id=turn_id,
            data={k: v for k, v in value.items() if k not in envelope},
            activity=activity,
            view=view,
        )


class ExecutionLayer(ABC):
    """Shared session API for local processes and explicit gateway connections.

    Applications normally get a layer from `blockether.vis.engine.start` or
    `blockether.vis.engine.connect`, then use `create_session` to start a
    conversation or `session` to attach to an existing session ID. `Agent` wraps
    this API when you only need one conversation.

    The named HTTP methods below expose lower-level engine routes. Their `body`
    and `query` values use the gateway's wire schema, not any Python object. For
    ordinary workflows, use `Session` and `Turn` methods. Route failures raise
    `GatewayError`. IO failures and malformed replies raise `TransportError` or
    its subclasses. Mutations are not automatically retried.

    Args:
        timeout: Positive finite transport timeout in seconds. Defaults to 30.
            This is independent of a turn's wait deadline.

    Use each layer and its session handles on one calling thread. Two exceptions
    apply to event streams: `close` is safe from any thread, and `async for`
    reads a stream on one worker thread that the stream owns. Context entry
    calls `connect`, and context exit calls `close`. A custom transport
    implements `connect`, `close`, `session_options` and `_open`. `_open`
    returns a context-managed binary response with status and headers. Path and
    channel defaults belong to the layer, not to `Agent`.
    """

    def __init__(self, *, timeout: float = 30):
        self.timeout = _duration(timeout)
        self._lease: str | None = None
        self._closed = False
        self._streams: set[_EventStream] = set()
        self._client_extensions = {}

    @abstractmethod
    def connect(self) -> ExecutionLayer:
        """Connect or start the layer. Repeated calls keep the same connection."""

    @abstractmethod
    def close(self) -> None:
        """Release this layer's resources without deleting remote sessions."""

    @abstractmethod
    def session_options(self, project=".") -> SessionOptions:
        """Validate a project and return its wire-ready session creation options."""

    @abstractmethod
    def _open(
        self, method, route, *, query=None, body=None, content=None, timeout=None
    ) -> Any:
        """Run one transport exchange. Do not retry mutations."""

    def _open_whole(self, method, route, **options) -> Any:
        """Run one exchange whose answer the caller reads whole, not as a stream."""
        return self._open(method, route, **options)

    def _ensure_client_lease(self, *, pid: int | None = None) -> str:
        if self._closed:
            raise TransportError("client is closed")
        if self._lease is None:
            body: dict[str, JSONValue] = {"kind": "python-sdk"}
            if pid is not None:
                body["pid"] = pid
            lease = self._request("POST", "/v1/clients", body=body).json()
            client_id: str = _field(lease, "client_id", str)
            self._lease = client_id
        return self._lease

    def __enter__(self):
        return self.connect()

    def __exit__(self, *args):
        self.close()

    def _request(
        self,
        method: str,
        route: str,
        *,
        path=None,
        query=None,
        body=None,
        content: bytes | BinaryIO | None = None,
        timeout: float | None = None,
        upload_sha256: str | None = None,
        upload_length: int | None = None,
    ) -> Response:
        """Call an SDK route template. Binary responses stay bytes.

        Method, template and path parameter names are checked before network IO.
        Mutations are sent once, including on timeout or ambiguous disconnect.
        """
        entry = next((r for r in _ROUTES if r["path"] == route), None)
        if (
            not entry
            or entry["audience"] != "sdk"
            or method.lower() not in entry["operations"]
        ):
            raise ValueError("not a canonical SDK operation")
        operation = entry["operations"][method.lower()]
        if operation["response"] == "sse":
            raise ValueError("SSE requires a streaming API")
        request_kind = operation["request"]
        if body is not None and request_kind != "json":
            raise ValueError("this operation does not accept a JSON body")
        if content is not None and request_kind != "binary":
            raise ValueError("this operation does not accept a binary body")
        streaming = content is not None and not isinstance(content, bytes)
        if streaming and not callable(getattr(content, "read", None)):
            raise ValueError("binary operations require bytes or a readable stream")
        if streaming or upload_sha256 is not None or upload_length is not None:
            if (
                route != "/v1/decisions/models"
                or method != "POST"
                or not streaming
                or not isinstance(upload_sha256, str)
                or len(upload_sha256) != 64
                or any(
                    character not in "0123456789abcdef" for character in upload_sha256
                )
                or type(upload_length) is not int
                or upload_length <= 0
            ):
                raise ValueError(
                    "decision upload requires a bounded stream and SHA-256"
                )
            if upload_length > _MAX_DECISION_ARCHIVE:
                raise ValueError(
                    f"Decision archive is {upload_length} bytes; "
                    f"maximum is {_MAX_DECISION_ARCHIVE} bytes"
                )
        names = {s[1:] for s in route.split("/") if s.startswith(":")}
        path = path or {}
        if set(path) != names:
            raise ValueError("path parameters do not match route")
        resolved = "/".join(
            _segment(path[s[1:]]) if s.startswith(":") else s for s in route.split("/")
        )
        try:
            with self._open_whole(
                method.upper(),
                resolved,
                query=query,
                body=body,
                content=content,
                timeout=timeout,
                **(
                    {"upload_sha256": upload_sha256, "upload_length": upload_length}
                    if streaming
                    else {}
                ),
            ) as raw:
                return Response(raw.status, raw.read(), dict(raw.headers))
        except TimeoutError:
            raise VisTimeout("gateway response timed out") from None
        except OSError:
            raise TransportError("gateway response interrupted") from None

    @property
    def speech(self) -> Speech:
        """Return the `Speech` API: voices, synthesis and transcription."""
        from ._speech import Speech

        return Speech(self)

    @property
    def sounds(self) -> Sounds:
        """Return the `Sounds` API: the sounds that the gateway machine plays."""
        from ._sounds import Sounds

        return Sounds(self)

    def session(self, sid: str) -> Session:
        """Return a lightweight handle for an existing session ID.

        This does not fetch or validate the remote session. Use `Session.read`
        to get its state. That request reports a missing session. The handle
        borrows this layer and does not own its lifetime.
        """
        return Session(self, sid)

    def attach_app(self, app, *, owner: str, on_event=None) -> LiveAttachment:
        """Open or adopt a global app as owner program `owner`.

        A global app belongs to no session; people open it from the global apps
        button. `app` is a `vis.ui.app(..., key=...)` declaration or an app row
        from `apps()`. A declaration needs a `key`, and its `scope` is `global`.
        Opening the same owner and key again adopts the open app, also after a
        gateway restart. With `is_persistent=False`, a gateway restart drops it.
        The other rules are those of `Session.attach`; the door is
        `POST /v1/apps`. Answers a `LiveAttachment`.
        """
        from blockether.vis.extension import _APP_MEDIA_TYPE, LiveAttachment, _AppDoor

        if isinstance(app, Mapping) and app.get("media_type") != _APP_MEDIA_TYPE:
            if app.get("scope", "global") != "global":
                raise ValueError(
                    "attach_app opens only global apps; use Session.attach"
                )
            if not str(app.get("key") or "").strip():
                raise ValueError("a global app needs vis.ui.app(..., key=...)")
            app = {**app, "scope": "global"}
        return LiveAttachment._from(app, door=_AppDoor(self, owner), on_event=on_event)

    def apps(self, archived: bool = False) -> list[LiveView]:
        """Read the global apps, oldest first, as typed `LiveView` records.

        A closed app stays listed. `archived=True` lists only the archived apps.
        The list is empty while the person turns global apps off. This is a
        snapshot; the owner gets the events of its app from `attach_app`.
        """
        answer = self.get_apps(query={"archived": "true"} if archived else None)
        try:
            return [LiveView.from_wire(item) for item in _field(answer, "views", list)]
        except ValueError:
            raise ProtocolError("invalid live View response") from None

    def app(self, view_id: str) -> LiveView:
        """Read one global app by id, also a closed or restored app."""
        data = _field(self.get_app(view_id), "view", dict)
        try:
            return LiveView.from_wire(data)
        except ValueError:
            raise ProtocolError("invalid live View response") from None

    def app_action(self, view_id: str, action: str, **values) -> _types.ViewOutcome:
        """Send an operator action to a global app, as `Session.view_action` does."""
        body = {**values, "action": action}
        validate("view", "operator_action", body)
        return self.post_app_actions(view_id, body=body)

    def web_brand(self) -> _types.WebBrand:
        """Read the white label of the web app that this gateway serves.

        The answer has only the keys that are set: `name`, `logo`, `href`,
        `links` and `theme`. An empty answer means the Vis default.
        """
        brand = self.get_web_brand()
        if not isinstance(brand, dict):
            raise ProtocolError("invalid web brand response")
        return brand

    def set_web_brand(
        self,
        *,
        name: str | None = None,
        logo: str | None = None,
        href: str | None = None,
        links: Iterable[Mapping[str, str]] | None = None,
        theme: Mapping[str, JSONValue] | None = None,
    ) -> _types.WebBrand:
        """Replace the white label of the web app that this gateway serves.

        `name` shows beside the logo and in the page title. `logo` is an
        `https://` address, a path of the web app or a base64 `data:image/`
        URL. `href` is the address that the logo opens. `links` are the icon
        links of the header: each has `icon` (an app icon name), `label` and
        `href`. `theme` makes one theme the only theme, and the app then shows
        no theme choice: `base` names a shipped theme, `colors` replaces its
        CSS variables and `label` names it.

        The call replaces the whole brand: an argument that is None goes back
        to the Vis default. An open page shows the new brand after a reload.
        Answers the saved brand.
        """
        body: dict[str, JSONValue] = {}
        if name is not None:
            body["name"] = name
        if logo is not None:
            body["logo"] = logo
        if href is not None:
            body["href"] = href
        if links is not None:
            body["links"] = [{**link} for link in links]
        if theme is not None:
            body["theme"] = {**theme}
        validate("gateway", "web_brand", body)
        brand = self.put_web_brand(body=body)
        if not isinstance(brand, dict):
            raise ProtocolError("invalid web brand response")
        return brand

    def clear_web_brand(self) -> None:
        """Remove the white label: the web app shows the Vis default again."""
        self.delete_web_brand()

    def create_session(
        self,
        *,
        provider: str | None = None,
        model: str | None = None,
        group: SessionGroup | str | None = None,
        timeout: float | None = None,
        **options: object,
    ) -> Session:
        """Create a conversation and return a `Session` handle.

        Args:
            provider: Optional provider ID from `models`. Give it with `model`.
            model: Optional model name. The pair pins the model of the session
                before its first turn.
            group: Optional `SessionGroup` or group ID. The new session is in that
                group and in the project of the group. A `root` option still sets
                the folder that the session works in.
            timeout: Optional timeout in seconds for this creation request.
            **options: Session creation fields from the gateway contract. For a
                project path, pass `**layer.session_options(project)`. This
                applies local and gateway path rules and channel defaults
                correctly.

        This sends a creation request immediately. Unlike `session`, it creates
        a new record. To attach to it later, use the returned handle's `id`.
        """
        if (provider is None) != (model is None):
            raise ValueError("provider and model must be given together")
        if provider is not None:
            options = {**options, "provider": provider, "model": model}
        if group is not None:
            options = {**options, "group_id": _group_id(group)}
        data = self._request(
            "POST",
            "/v1/sessions",
            body=options,
            timeout=self._session_create_timeout(timeout),
        ).json()
        return self.session(_field(data, "id", str))

    def _session_create_timeout(self, timeout: float | None) -> float | None:
        """Return the timeout of a session creation request."""
        return timeout

    def groups(
        self,
        *,
        root: str | None = None,
        project_id: str | None = None,
        archived: str = "exclude",
    ) -> list[SessionGroup]:
        """List the session groups of one project, newest activity first.

        Name the project with `root`, its folder on the gateway machine, or with
        `project_id`. `archived` is `exclude`, `include` or `only`. A folder
        without a project has no groups.
        """
        query = {**_project_scope(root, project_id, "project"), "archived": archived}
        rows = _field(self.get_session_groups(query=query), "groups", list)
        return [_group(row) for row in rows]

    def create_group(
        self,
        name: str,
        *,
        root: str | None = None,
        project_id: str | None = None,
        color: str | None = None,
    ) -> SessionGroup:
        """Create a session group in one project and return it.

        With `root`, the gateway also creates the project of that folder when it
        does not exist. `color` is a palette token. A second group with the same
        name in the project raises `GatewayError` with the status 409. To get
        the group that exists, use `group`.
        """
        body = {"name": name, **_project_scope(root, project_id, "project_id")}
        if color is not None:
            body["color"] = color
        return _group(self.post_session_groups(body=body))

    def group(
        self,
        name: str,
        *,
        root: str | None = None,
        project_id: str | None = None,
        color: str | None = None,
    ) -> SessionGroup:
        """Return the group `name` of one project. Create it when it does not exist.

        An archived group with that name is returned as it is. `color` applies
        only to a new group.
        """

        def find(same) -> SessionGroup | None:
            rows = self.groups(root=root, project_id=project_id, archived="include")
            return next((row for row in rows if same(row.name)), None)

        found = find(name.__eq__)
        if found is not None:
            return found
        try:
            return self.create_group(
                name, root=root, project_id=project_id, color=color
            )
        except GatewayError as error:
            if error.status != 409:
                raise
            # Another program created the group between the read and the write.
            found = find(name.__eq__) or find(
                lambda held: held.casefold() == name.casefold()
            )
            if found is None:
                raise
            return found

    def update_group(
        self,
        group: SessionGroup | str,
        *,
        name: str | None = None,
        color: str | None = None,
        archived: bool | None = None,
    ) -> SessionGroup:
        """Rename, recolor, archive or restore a group and return its new record."""
        body: dict[str, JSONValue] = {
            key: value
            for key, value in (("name", name), ("color", color), ("archived", archived))
            if value is not None
        }
        if not body:
            raise ValueError("give name, color or archived")
        return _group(self.patch_session_group(_group_id(group), body=body))

    def delete_group(
        self, group: SessionGroup | str, *, delete_sessions: bool = False
    ) -> _types.SessionGroupDeleted:
        """Delete a group. Its sessions stay in the project without a group.

        With `delete_sessions=True`, the sessions of the group are deleted too.
        The answer has `scattered_session_ids` and `deleted_session_ids`.
        """
        mode = "delete" if delete_sessions else "detach"
        return self.delete_session_group(_group_id(group), query={"sessions": mode})

    def agent_name(self) -> str:
        """Return the name that the gateway gives to the agent.

        This is the name for a session without a project name. The environment
        variable `VIS_AGENT_NAME` of the gateway and a locked name are the name
        of the agent in each project.
        """
        return _agent_name(self.get_setting("agent_name", query={"scope": "global"}))

    def set_agent_name(self, name: str, *, locked: bool | None = None) -> str:
        """Save the agent name of the gateway and return the name in effect.

        `locked=True` also locks the name: it is then the only name of the
        agent, no project changes it and no client can rename the agent.
        `locked=False` removes the lock. Without `locked`, the lock stays as it
        is, and the gateway refuses the change of a locked name with
        `GatewayError` (status 409). While `VIS_AGENT_NAME` is set on the
        gateway, the gateway refuses each change in the same way.
        """
        if not isinstance(name, str):
            raise TypeError("an agent name must be text")
        if locked is not None and not isinstance(locked, bool):
            raise TypeError("locked must be True, False or None")
        body: dict[str, JSONValue] = {
            "scope": "global",
            "id": "agent_name",
            "action": "value",
            "value": name.strip(),
        }
        if locked is not None:
            body["is_locked"] = locked
        return _agent_name(self.post_settings(body=body))

    def machine_name(self) -> str | None:
        """Return the name of the gateway machine, or None when it has no name.

        Clients show this name for the machine in place of the host of its URL.
        The environment variable `VIS_MACHINE_NAME` of the gateway wins over the
        saved name.
        """
        return _machine_name(
            self.get_setting("machine_name", query={"scope": "global"})
        )

    def set_machine_name(self, name: str | None) -> str | None:
        """Save the name of the gateway machine and return the name in effect.

        None or a blank name removes the saved name. The gateway refuses a name
        that is too long or has control characters with `GatewayError`. When
        `VIS_MACHINE_NAME` is set on the gateway, the answer is still the name
        from that variable: the saved name applies when the variable is not set.
        """
        if name is not None and not isinstance(name, str):
            raise TypeError("a machine name must be text or None")
        body: dict[str, JSONValue] = {"scope": "global", "id": "machine_name"}
        if name is None or not name.strip():
            body["action"] = "inherit"
        else:
            body.update(action="value", value=name.strip())
        return _machine_name(self.post_settings(body=body))

    def restore_session(
        self,
        data: bytes,
        *,
        root: str | None = None,
        group_id: str | None = None,
        key: str | None = None,
        timeout: float | None = None,
    ) -> Session:
        """Restore a session snapshot as a new session and return its handle.

        Args:
            data: The bytes that `Session.export` returned, for example read from a
                `.vissnap` file.
            root: Optional folder for the restored sessions. Without it, the gateway
                uses the project folder of `group_id`, else its own folder.
            group_id: Optional session group for the restored session.
            key: Optional restore key. A second restore with the same key returns
                the session of the first restore and creates nothing.
            timeout: Optional timeout in seconds for this request.

        Each restored session and child gets a new ID. Your own permissions apply,
        not the permissions in the snapshot. A child that was running fails, so you
        can resume it. Read the manifest first with `snapshot_info`.
        """
        query = {
            name: value
            for name, value in (("root", root), ("group_id", group_id), ("key", key))
            if value is not None
        }
        reply = self.post_sessions_import(query=query, timeout=timeout, content=data)
        return self.session(_field(reply, "id", str))

    @staticmethod
    def snapshot_info(data: bytes) -> dict[str, Any]:
        """Read the manifest of session snapshot `data` without a gateway request.

        The manifest gives `format_version`, `vis_version`, `created_at`,
        `root_session_id`, `sessions`, `counts`, `sizes` and `payload_sha256`.
        Raises `ValueError` when `data` is not a Vis session snapshot.
        """
        view = memoryview(data)
        if len(view) < 16 or bytes(view[:8]) != b"VISSNAP\0":
            raise ValueError("This file is not a Vis session snapshot")
        version = int.from_bytes(view[8:12], "big", signed=True)
        length = int.from_bytes(view[12:16], "big", signed=True)
        if version != 1:
            raise ValueError(f"Unsupported session snapshot format version {version}")
        if length < 0 or 16 + length > len(view):
            raise ValueError("The session snapshot manifest is damaged")
        try:
            manifest = json.loads(bytes(view[16 : 16 + length]))
        except (ValueError, UnicodeError):
            raise ValueError("The session snapshot manifest is damaged") from None
        if not isinstance(manifest, dict):
            raise ValueError("The session snapshot manifest is damaged")
        return manifest

    def _pump_client_extensions(self, session_id):
        extensions = self._client_extensions.get(session_id)
        if extensions is not None and extensions.manifest:
            extensions.drain(self, session_id)

    def _clear_client_extensions(self):
        for extensions in self._client_extensions.values():
            extensions.clear()
        self._client_extensions.clear()

    def put_session_client_extensions(
        self, sid: str, *, body: JSONValue, timeout: float | None = None
    ) -> _types.EmptyObject:
        """PUT /v1/sessions/:sid/client-extensions — bind client-owned declarations."""
        return self._request(
            "PUT",
            "/v1/sessions/:sid/client-extensions",
            path={"sid": sid},
            body=body,
            timeout=timeout,
        ).json()

    def delete_session_client_extensions(
        self, sid: str, *, timeout: float | None = None
    ) -> _types.EmptyObject:
        """DELETE /v1/sessions/:sid/client-extensions — detach owned callbacks."""
        return self._request(
            "DELETE",
            "/v1/sessions/:sid/client-extensions",
            path={"sid": sid},
            timeout=timeout,
        ).json()

    def get_session_client_calls(
        self, sid: str, *, timeout: float | None = None
    ) -> _types.ClientCalls:
        """GET /v1/sessions/:sid/client-calls — retrieve pending owned calls."""
        return self._request(
            "GET",
            "/v1/sessions/:sid/client-calls",
            path={"sid": sid},
            timeout=timeout,
        ).json()

    def post_session_client_call_result(
        self, sid: str, call_id: str, *, body: JSONValue, timeout: float | None = None
    ) -> _types.EmptyObject:
        """POST /v1/sessions/:sid/client-calls/:call_id/result — acknowledge one call."""
        return self._request(
            "POST",
            "/v1/sessions/:sid/client-calls/:call_id/result",
            path={"sid": sid, "call_id": call_id},
            body=body,
            timeout=timeout,
        ).json()

    def post_session_client_call_activity(
        self, sid: str, call_id: str, *, body: JSONValue, timeout: float | None = None
    ) -> _types.EmptyObject:
        """POST /v1/sessions/:sid/client-calls/:call_id/activity — publish presentation."""
        return self._request(
            "POST",
            "/v1/sessions/:sid/client-calls/:call_id/activity",
            path={"sid": sid, "call_id": call_id},
            body=body,
            timeout=timeout,
        ).json()

    def list_sessions(self, **query) -> _types.SessionList:
        """Return one page with next_cursor. It does not fetch every session.

        By default the list does not show a session that has no title, no turns
        and no running turn. Give include_empty="true" to also show these
        sessions, for example a session that you created and did not run yet.
        """
        return self._request("GET", "/v1/sessions", query=query).json()

    def get_capabilities(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Capabilities:
        """GET /v1/capabilities — json response."""
        response = self._request(
            "GET", "/v1/capabilities", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_devices(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Devices:
        """GET /v1/devices — json response."""
        response = self._request(
            "GET", "/v1/devices", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_devices(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.DeviceRegistered:
        """POST /v1/devices — json response."""
        response = self._request(
            "POST", "/v1/devices", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_device(
        self, token: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Removed:
        """DELETE /v1/devices/:token — json response."""
        response = self._request(
            "DELETE",
            "/v1/devices/:token",
            path={"token": token},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_devices_test(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.DeviceTest:
        """POST /v1/devices/actions/test — json response."""
        response = self._request(
            "POST", "/v1/devices/actions/test", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_fs(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.FsListing:
        """GET /v1/fs — json response."""
        response = self._request("GET", "/v1/fs", path={}, query=query, timeout=timeout)
        return response.json()

    def post_fs_mkdir(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.FsEntry:
        """POST /v1/fs/actions/mkdir — json response."""
        response = self._request(
            "POST",
            "/v1/fs/actions/mkdir",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_mcp_servers(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpServers:
        """GET /v1/mcp/servers — json response."""
        response = self._request(
            "GET", "/v1/mcp/servers", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_mcp_servers(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpServer:
        """POST /v1/mcp/servers — json response."""
        response = self._request(
            "POST", "/v1/mcp/servers", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_mcp_server(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpDeleted:
        """DELETE /v1/mcp/servers/:name — json response."""
        response = self._request(
            "DELETE",
            "/v1/mcp/servers/:name",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def put_mcp_server(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpServer:
        """PUT /v1/mcp/servers/:name — json response."""
        response = self._request(
            "PUT",
            "/v1/mcp/servers/:name",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_enable(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpServer:
        """POST /v1/mcp/servers/:name/actions/enable — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/actions/enable",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_kill(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpServer:
        """POST /v1/mcp/servers/:name/actions/kill — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/actions/kill",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_server_start(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpServer:
        """POST /v1/mcp/servers/:name/actions/start — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/actions/start",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_server_auth_cancel(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpAuthCancelled:
        """POST /v1/mcp/servers/:name/auth/cancel — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/auth/cancel",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_auth_complete(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpAuthFlow:
        """POST /v1/mcp/servers/:name/auth/complete — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/auth/complete",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_auth_logout(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpTokenStatus:
        """POST /v1/mcp/servers/:name/auth/logout — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/auth/logout",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_server_auth_poll(
        self,
        name: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpAuthFlow:
        """POST /v1/mcp/servers/:name/auth/poll — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/auth/poll",
            path={"name": name},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_mcp_server_auth_start(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.McpAuthFlow:
        """POST /v1/mcp/servers/:name/auth/start — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/:name/auth/start",
            path={"name": name},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_mcp_servers_test(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.McpTest:
        """POST /v1/mcp/servers/actions/test — json response."""
        response = self._request(
            "POST",
            "/v1/mcp/servers/actions/test",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_models(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ModelsCatalog:
        """GET /v1/models — json response."""
        response = self._request(
            "GET", "/v1/models", path={}, query=query, timeout=timeout
        )
        return response.json()

    def models(
        self,
        *,
        provider: str | None = None,
        is_available: bool | None = None,
        timeout: float | None = None,
    ) -> tuple[Model, ...]:
        """Return the models that the gateway pickers show, in picker order.

        Args:
            provider: Optional provider ID. Only its models are returned.
            is_available: Optional filter. True keeps models of signed-in
                providers. False keeps the others.
            timeout: Optional timeout in seconds for this request.

        Each `Model` carries its price, billing, capabilities and the limits
        report of its provider. The gateway reads provider limits in parallel
        and bounds each read, so a slow provider gives an error report.
        """
        data = self.get_models(timeout=timeout)
        return tuple(
            model
            for model in Model.many(_field(data, "models", list))
            if (provider is None or model.provider == provider)
            and (is_available is None or model.is_available is is_available)
        )

    def model_limits(
        self, provider: str, *, timeout: float | None = None
    ) -> _types.ProviderReport | None:
        """Return the current limits report of one provider.

        Use it to refresh the `Model.limits` of that provider's models. The
        report follows the provider limits schema. A failed read gives an
        error report, not an exception.
        """
        answer = self.get_provider_limits(provider, timeout=timeout)
        report = answer.get("report") if isinstance(answer, dict) else None
        if not isinstance(report, dict):
            raise ProtocolError("provider limits must be an object")
        validate("provider", "report", report)
        return report

    def get_projects(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ProjectList:
        """GET /v1/projects — json response."""
        response = self._request(
            "GET", "/v1/projects", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_projects(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Project:
        """POST /v1/projects — json response."""
        response = self._request(
            "POST", "/v1/projects", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_project(
        self, pid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """DELETE /v1/projects/:pid — empty response."""
        self._request(
            "DELETE",
            "/v1/projects/:pid",
            path={"pid": pid},
            query=query,
            timeout=timeout,
        )
        return None

    def get_project(
        self, pid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Project:
        """GET /v1/projects/:pid — json response."""
        response = self._request(
            "GET", "/v1/projects/:pid", path={"pid": pid}, query=query, timeout=timeout
        )
        return response.json()

    def patch_project(
        self,
        pid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Project:
        """PATCH /v1/projects/:pid — json response."""
        response = self._request(
            "PATCH",
            "/v1/projects/:pid",
            path={"pid": pid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def patch_project_sessions(
        self,
        pid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProjectSessionsMoved:
        """PATCH /v1/projects/:pid/sessions — json response."""
        response = self._request(
            "PATCH",
            "/v1/projects/:pid/sessions",
            path={"pid": pid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_projects_ensure(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Project:
        """POST /v1/projects/actions/ensure — json response."""
        response = self._request(
            "POST",
            "/v1/projects/actions/ensure",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_projects_overview(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ProjectOverview:
        """GET /v1/projects/overview — json response."""
        response = self._request(
            "GET", "/v1/projects/overview", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_provider_presets(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ProviderPresets:
        """GET /v1/provider-presets — json response."""
        response = self._request(
            "GET", "/v1/provider-presets", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_providers(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.RouterFleet:
        """POST /v1/providers — json response."""
        response = self._request(
            "POST", "/v1/providers", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_provider(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.RouterFleet:
        """DELETE /v1/providers/:provider-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/providers/:provider-id",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_provider_auth_cancel(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/cancel — json response."""
        response = self._request(
            "POST",
            "/v1/providers/:provider-id/auth/cancel",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_auth_complete(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/complete — json response."""
        response = self._request(
            "POST",
            "/v1/providers/:provider-id/auth/complete",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_auth_poll(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/poll — json response."""
        response = self._request(
            "POST",
            "/v1/providers/:provider-id/auth/poll",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_auth_start(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/auth/start — json response."""
        response = self._request(
            "POST",
            "/v1/providers/:provider-id/auth/start",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_provider_limits(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderLimits:
        """GET /v1/providers/:provider-id/limits — json response."""
        response = self._request(
            "GET",
            "/v1/providers/:provider-id/limits",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_provider_reset_credits_consume(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ProviderResetOutcome:
        """POST /v1/providers/:provider-id/reset-credits/consume — json response."""
        response = self._request(
            "POST",
            "/v1/providers/:provider-id/reset-credits/consume",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_provider_logout(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderAuth:
        """POST /v1/providers/:provider-id/logout — json response."""
        response = self._request(
            "POST",
            "/v1/providers/:provider-id/logout",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_provider_models(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderModels:
        """GET /v1/providers/:provider-id/models — json response."""
        response = self._request(
            "GET",
            "/v1/providers/:provider-id/models",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_provider_status(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ProviderStatus:
        """GET /v1/providers/:provider-id/status — json response."""
        response = self._request(
            "GET",
            "/v1/providers/:provider-id/status",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_router(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.RouterFleet:
        """GET /v1/router — json response."""
        response = self._request(
            "GET", "/v1/router", path={}, query=query, timeout=timeout
        )
        return response.json()

    def patch_router(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.RouterFleet:
        """PATCH /v1/router — json response."""
        response = self._request(
            "PATCH", "/v1/router", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_session_groups(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionGroupPage:
        """GET /v1/session-groups — the groups of one project, in the human's order."""
        response = self._request(
            "GET", "/v1/session-groups", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_session_groups(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionGroup:
        """POST /v1/session-groups — create one group inside a project."""
        response = self._request(
            "POST",
            "/v1/session-groups",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_session_group(
        self, gid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionGroupDeleted:
        """DELETE /v1/session-groups/:gid — drop a group and say what becomes of it."""
        response = self._request(
            "DELETE",
            "/v1/session-groups/:gid",
            path={"gid": gid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_group(
        self,
        gid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionGroup:
        """PATCH /v1/session-groups/:gid — rename, recolour or reorder a group."""
        response = self._request(
            "PATCH",
            "/v1/session-groups/:gid",
            path={"gid": gid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_sessions(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionList:
        """GET /v1/sessions — json response."""
        response = self._request(
            "GET", "/v1/sessions", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_sessions(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Session:
        """POST /v1/sessions — json response."""
        response = self._request(
            "POST", "/v1/sessions", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_session(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """DELETE /v1/sessions/:sid — empty response."""
        self._request(
            "DELETE",
            "/v1/sessions/:sid",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return None

    def get_session(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Session:
        """GET /v1/sessions/:sid — json response."""
        response = self._request(
            "GET", "/v1/sessions/:sid", path={"sid": sid}, query=query, timeout=timeout
        )
        return response.json()

    def patch_session(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Session:
        """PATCH /v1/sessions/:sid — json response."""
        response = self._request(
            "PATCH",
            "/v1/sessions/:sid",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_activity(
        self,
        sid: str,
        aid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ActivityPage:
        """GET /v1/sessions/:sid/activity/:aid — a bounded page of complete Activity records.

        Query with after, limit and q. Follow history.next_after until it is
        null. When you combine pages into one copy, reject a changed revision.
        """
        response = self._request(
            "GET",
            "/v1/sessions/:sid/activity/:aid",
            path={"sid": sid, "aid": aid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_activity_export(
        self,
        sid: str,
        aid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> Response:
        """GET /v1/sessions/:sid/activity/:aid/export — complete unfiltered Activity text.

        The gateway streams this response, and the returned Response buffers its
        bytes. Supply revision to reject a changed history before the export
        starts. Raises ProtocolError if a concurrent change marks the export
        incomplete.
        """
        response = self._request(
            "GET",
            "/v1/sessions/:sid/activity/:aid/export",
            path={"sid": sid, "aid": aid},
            query=query,
            timeout=timeout,
        )
        if response.content.endswith(
            b"\n\nINCOMPLETE EXPORT: Activity changed. Reload and retry.\n"
        ):
            raise ProtocolError("Activity export is incomplete; reload and retry")
        return response

    def get_session_alert(
        self, sid: str, *, timeout: float | None = None
    ) -> _types.SessionAlert:
        """GET /v1/sessions/:sid/alert — the banner text for the latest turn."""
        return self._request(
            "GET",
            "/v1/sessions/:sid/alert",
            path={"sid": sid},
            timeout=timeout,
        ).json()

    def get_session_artifacts(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionArtifacts:
        """GET /v1/sessions/:sid/artifacts — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/artifacts",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_attachments(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.Upload:
        """POST /v1/sessions/:sid/attachments — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/attachments",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def post_session_cancel_current(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.TurnCancel:
        """POST /v1/sessions/:sid/cancel-current — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/cancel-current",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_context(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionContext:
        """GET /v1/sessions/:sid/context — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/context",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_drain_queue(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.TurnQueueStep:
        """POST /v1/sessions/:sid/drain-queue — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/drain-queue",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_events_since(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionEvents:
        """GET /v1/sessions/:sid/events-since — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/events-since",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_forks(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionForkTurns:
        """GET /v1/sessions/:sid/forks — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/forks",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_forks(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionWrapped:
        """POST /v1/sessions/:sid/forks — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/forks",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_fs_actions_open(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionFsOpen:
        """POST /v1/sessions/:sid/fs/actions/open — open one file in the editor."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/fs/actions/open",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_fs_file(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.FilePreview:
        """GET /v1/sessions/:sid/fs/file — preview one workspace file by its kind."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/fs/file",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_fs_file_content(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/fs/file/content — binary response.

        Stream one recording or clip. Pass the path, expires and ticket of the
        content_url that the media preview of get_session_fs_file gives.
        """
        response = self._request(
            "GET",
            "/v1/sessions/:sid/fs/file/content",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def put_session_group(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Session:
        """PUT /v1/sessions/:sid/group — file this session under a group."""
        response = self._request(
            "PUT",
            "/v1/sessions/:sid/group",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_iteration_attachments(
        self,
        sid: str,
        iid: str,
        idx: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> Response:
        """GET /v1/sessions/:sid/iterations/:iid/attachments/:idx — binary response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/iterations/:iid/attachments/:idx",
            path={"sid": sid, "iid": iid, "idx": idx},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_iteration_attachment_link(
        self,
        sid: str,
        iid: str,
        idx: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AttachmentLink:
        """GET /v1/sessions/:sid/iterations/:iid/attachments/:idx/link — JSON response.

        Get a signed content_url for an attachment that keeps a reference to a
        file on the gateway machine. A frame, a download link or another app
        opens that address without the gateway token.
        """
        response = self._request(
            "GET",
            "/v1/sessions/:sid/iterations/:iid/attachments/:idx/link",
            path={"sid": sid, "iid": iid, "idx": idx},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def put_session_iteration_attachment_agent_note(
        self,
        sid: str,
        iid: str,
        idx: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AgentNote:
        """PUT /v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note — set the note of an HTML page for the model."""
        response = self._request(
            "PUT",
            "/v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note",
            path={"sid": sid, "iid": iid, "idx": idx},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_model(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionModel:
        """GET /v1/sessions/:sid/model — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/model",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_model(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionModel:
        """PATCH /v1/sessions/:sid/model — json response."""
        response = self._request(
            "PATCH",
            "/v1/sessions/:sid/model",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_queue_send_now(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.QueueSendNow:
        """POST /v1/sessions/:sid/queue/send-now — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/queue/send-now",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def put_session_read(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionRead:
        """PUT /v1/sessions/:sid/read — how far this reader has read the session."""
        response = self._request(
            "PUT",
            "/v1/sessions/:sid/read",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_release(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """POST /v1/sessions/:sid/release — empty response."""
        self._request(
            "POST",
            "/v1/sessions/:sid/release",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return None

    def get_session_resources(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionResources:
        """GET /v1/sessions/:sid/resources — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/resources",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_resources_logs(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionResourceLogs:
        """GET /v1/sessions/:sid/resources/logs — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/resources/logs",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_resources_stop(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionResourceStop:
        """POST /v1/sessions/:sid/resources/stop — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/resources/stop",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_resume_queue(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.TurnQueueStep:
        """POST /v1/sessions/:sid/resume-queue — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/resume-queue",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_seq(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSeq:
        """GET /v1/sessions/:sid/seq — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/seq",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_slashes(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSlashes:
        """GET /v1/sessions/:sid/slashes — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/slashes",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_keybindings(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.KeybindingList:
        """GET /v1/sessions/:sid/keybindings — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/keybindings",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_keybindings_run(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.KeybindingRunResult:
        """POST /v1/sessions/:sid/keybindings/run — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/keybindings/run",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_snapshot(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/snapshot — binary response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/snapshot",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def post_session_speech(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.VoiceJob:
        """POST /v1/sessions/:sid/speech — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/speech",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_session_speech_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.Forgotten:
        """DELETE /v1/sessions/:sid/speech/jobs/:job-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/sessions/:sid/speech/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_speech_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.VoiceJob:
        """GET /v1/sessions/:sid/speech/jobs/:job-id — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/speech/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_speech_job_audio(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> Response:
        """GET /v1/sessions/:sid/speech/jobs/:job-id/audio — binary response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/speech/jobs/:job-id/audio",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_suggest(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSuggestions:
        """GET /v1/sessions/:sid/suggest — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/suggest",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_transcript(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Transcript:
        """GET /v1/sessions/:sid/transcript — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/transcript",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_transcript_html(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/transcript.html — html response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/transcript.html",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_transcript_md(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/sessions/:sid/transcript.md — markdown response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/transcript.md",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response

    def get_session_turns(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.TurnList:
        """GET /v1/sessions/:sid/turns — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/turns",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_turns(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Turn:
        """POST /v1/sessions/:sid/turns — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/turns",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_session_turn(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnCancel:
        """DELETE /v1/sessions/:sid/turns/:tid — json response."""
        response = self._request(
            "DELETE",
            "/v1/sessions/:sid/turns/:tid",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_turn(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.Turn:
        """GET /v1/sessions/:sid/turns/:tid — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/turns/:tid",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_turn(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Turn:
        """PATCH /v1/sessions/:sid/turns/:tid — json response."""
        response = self._request(
            "PATCH",
            "/v1/sessions/:sid/turns/:tid",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_turn_attachments(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnAttachments:
        """GET /v1/sessions/:sid/turns/:tid/attachments — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/turns/:tid/attachments",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_turn_cancel(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnCancel:
        """POST /v1/sessions/:sid/turns/:tid/cancel — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/turns/:tid/cancel",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_turn_trace(
        self,
        sid: str,
        tid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.TurnTrace:
        """GET /v1/sessions/:sid/turns/:tid/trace — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/turns/:tid/trace",
            path={"sid": sid, "tid": tid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_usage(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionUsage:
        """GET /v1/sessions/:sid/usage — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/usage",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_view(
        self,
        sid: str,
        view_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ViewOutcome:
        """POST /v1/sessions/:sid/views/:view-id/actions — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/views/:view-id/actions",
            path={"sid": sid, "view-id": view_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_session_views_input(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.ViewRequests:
        """GET /v1/sessions/:sid/views/input — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/views/input",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_views_live(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AppList:
        """GET /v1/sessions/:sid/views/live — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/views/live",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_views_live_view(
        self,
        sid: str,
        view_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AppView:
        """GET /v1/sessions/:sid/views/live/:view-id — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/views/live/:view-id",
            path={"sid": sid, "view-id": view_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_views_live_log(
        self,
        sid: str,
        view_id: str,
        node_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.ViewLog:
        """GET /v1/sessions/:sid/views/live/:view-id/log — json response.

        The node rides the query string: node ids are free text a surface chose
        and may hold ``/``, which a path segment cannot carry.
        """
        response = self._request(
            "GET",
            "/v1/sessions/:sid/views/live/:view-id/log",
            path={"sid": sid, "view-id": view_id},
            query={"node": node_id, **(query or {})},
            timeout=timeout,
        )
        return response.json()

    def post_session_apps(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AppDoorAnswer:
        """POST /v1/sessions/:sid/apps — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/apps",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_apps(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AppList:
        """GET /v1/apps — json response."""
        response = self._request(
            "GET", "/v1/apps", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_apps(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AppDoorAnswer:
        """POST /v1/apps — json response."""
        response = self._request(
            "POST", "/v1/apps", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_app(
        self, view_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AppView:
        """GET /v1/apps/:view-id — json response."""
        response = self._request(
            "GET",
            "/v1/apps/:view-id",
            path={"view-id": view_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_app_actions(
        self,
        view_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ViewOutcome:
        """POST /v1/apps/:view-id/actions — json response."""
        response = self._request(
            "POST",
            "/v1/apps/:view-id/actions",
            path={"view-id": view_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_web_brand(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.WebBrand:
        """GET /v1/web/brand — json response."""
        response = self._request(
            "GET", "/v1/web/brand", path={}, query=query, timeout=timeout
        )
        return response.json()

    def put_web_brand(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.WebBrand:
        """PUT /v1/web/brand — json response."""
        response = self._request(
            "PUT", "/v1/web/brand", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_web_brand(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> None:
        """DELETE /v1/web/brand — empty response."""
        self._request("DELETE", "/v1/web/brand", path={}, query=query, timeout=timeout)
        return None

    def post_session_voice(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.VoiceJob:
        """POST /v1/sessions/:sid/voice — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/voice",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def delete_session_voice_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.Forgotten:
        """DELETE /v1/sessions/:sid/voice/jobs/:job-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/sessions/:sid/voice/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_voice_job(
        self,
        sid: str,
        job_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.VoiceJob:
        """GET /v1/sessions/:sid/voice/jobs/:job-id — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/voice/jobs/:job-id",
            path={"sid": sid, "job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_workspace(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionWorkspace:
        """GET /v1/sessions/:sid/workspace — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/workspace",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_session_workspace_root(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionWorkspace:
        """PATCH /v1/sessions/:sid/workspace/root — json response."""
        response = self._request(
            "PATCH",
            "/v1/sessions/:sid/workspace/root",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_sessions_archive(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SessionArchive:
        """POST /v1/sessions/actions/archive — archive many sessions; skip working ones."""
        response = self._request(
            "POST",
            "/v1/sessions/actions/archive",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_sessions_import(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.Session:
        """POST /v1/sessions/actions/import — restore a session snapshot."""
        response = self._request(
            "POST",
            "/v1/sessions/actions/import",
            path={},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def get_sessions_search(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SessionSearch:
        """GET /v1/sessions/actions/search — json response."""
        response = self._request(
            "GET", "/v1/sessions/actions/search", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_machines_order(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.MachineOrder:
        """POST /v1/machines/order — register gateway IDs and return their durable order."""
        response = self._request(
            "POST",
            "/v1/machines/order",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_automations(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AutomationsList:
        """GET /v1/automations — json response."""
        response = self._request(
            "GET", "/v1/automations", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_automations(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AutomationsAutomation:
        """POST /v1/automations — json response."""
        response = self._request(
            "POST", "/v1/automations", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_automation_runs(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AutomationsRunList:
        """GET /v1/automations/runs — json response."""
        response = self._request(
            "GET", "/v1/automations/runs", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_automation_run(
        self, run_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.AutomationsRun:
        """GET /v1/automations/runs/:run-id — json response."""
        response = self._request(
            "GET",
            "/v1/automations/runs/:run-id",
            path={"run-id": run_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_automation(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AutomationsAutomation:
        """GET /v1/automations/:automation-id — json response."""
        response = self._request(
            "GET",
            "/v1/automations/:automation-id",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_automation(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AutomationsAutomation:
        """PATCH /v1/automations/:automation-id — json response."""
        response = self._request(
            "PATCH",
            "/v1/automations/:automation-id",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_automation(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AutomationsDeleted:
        """DELETE /v1/automations/:automation-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/automations/:automation-id",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_automation_run(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> _types.AutomationsRun:
        """POST /v1/automations/:automation-id/run — json response."""
        response = self._request(
            "POST",
            "/v1/automations/:automation-id/run",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_automation_secrets(
        self,
        automation_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.AutomationsSecretCreated:
        """POST /v1/automations/:automation-id/secrets — json response."""
        response = self._request(
            "POST",
            "/v1/automations/:automation-id/secrets",
            path={"automation-id": automation_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_agreement(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsAgreement:
        """POST /v1/evals/agreement — json response."""
        response = self._request(
            "POST",
            "/v1/evals/agreement",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_compare(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsCaseComparison:
        """POST /v1/evals/compare — json response."""
        response = self._request(
            "POST",
            "/v1/evals/compare",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_diagnose(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsDiagnosis:
        """POST /v1/evals/diagnose — json response."""
        response = self._request(
            "POST",
            "/v1/evals/diagnose",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_labels(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsAgreement:
        """POST /v1/evals/labels — json response."""
        response = self._request(
            "POST", "/v1/evals/labels", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def post_evals_leaderboard(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsLeaderboard:
        """POST /v1/evals/leaderboard — json response."""
        response = self._request(
            "POST",
            "/v1/evals/leaderboard",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_evals_report(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.EvalsBenchReport:
        """POST /v1/evals/report — json response."""
        response = self._request(
            "POST", "/v1/evals/report", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_settings(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Settings:
        """GET /v1/settings — json response."""
        response = self._request(
            "GET", "/v1/settings", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_settings(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Setting:
        """POST /v1/settings — json response."""
        response = self._request(
            "POST", "/v1/settings", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def patch_settings(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.Settings:
        """PATCH /v1/settings — json response."""
        response = self._request(
            "PATCH", "/v1/settings", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_setting(
        self, id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Setting:
        """GET /v1/settings/:id — json response."""
        response = self._request(
            "GET", "/v1/settings/:id", path={"id": id}, query=query, timeout=timeout
        )
        return response.json()

    def post_setting_preview(
        self,
        id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SettingPreviewResult:
        """POST /v1/settings/:id/preview — json response."""
        response = self._request(
            "POST",
            "/v1/settings/:id/preview",
            path={"id": id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_extensions_reload(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.ExtensionReload:
        """POST /v1/extensions/reload — json response."""
        response = self._request(
            "POST",
            "/v1/extensions/reload",
            path={},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_speech(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.VoiceJob:
        """POST /v1/speech — json response."""
        response = self._request(
            "POST", "/v1/speech", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_speech_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Forgotten:
        """DELETE /v1/speech/jobs/:job-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/speech/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_speech_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceJob:
        """GET /v1/speech/jobs/:job-id — json response."""
        response = self._request(
            "GET",
            "/v1/speech/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_speech_job_audio(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/speech/jobs/:job-id/audio — binary response."""
        response = self._request(
            "GET",
            "/v1/speech/jobs/:job-id/audio",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response

    def get_speech_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """GET /v1/speech/model — json response."""
        response = self._request(
            "GET", "/v1/speech/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_speech_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """POST /v1/speech/model — json response."""
        response = self._request(
            "POST", "/v1/speech/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_speech_voices(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.SpeechVoices:
        """GET /v1/speech/voices — json response."""
        response = self._request(
            "GET", "/v1/speech/voices", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_speech_voices(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.SpeechVoice:
        """POST /v1/speech/voices — json response."""
        response = self._request(
            "POST",
            "/v1/speech/voices",
            path={},
            query=query,
            timeout=timeout,
            content=content,
        )
        return response.json()

    def delete_speech_voice(
        self, voice_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Forgotten:
        """DELETE /v1/speech/voices/:voice-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/speech/voices/:voice-id",
            path={"voice-id": voice_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def patch_speech_voice(
        self,
        voice_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> _types.SpeechVoice:
        """PATCH /v1/speech/voices/:voice-id — json response."""
        response = self._request(
            "PATCH",
            "/v1/speech/voices/:voice-id",
            path={"voice-id": voice_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def get_speech_voice_sample(
        self, voice_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> Response:
        """GET /v1/speech/voices/:voice-id/sample — binary response."""
        response = self._request(
            "GET",
            "/v1/speech/voices/:voice-id/sample",
            path={"voice-id": voice_id},
            query=query,
            timeout=timeout,
        )
        return response

    def get_decision_models(
        self, *, timeout: float | None = None
    ) -> _types.DecisionModels:
        """GET /v1/decisions/models — installed and resident decision versions."""
        response = self._request(
            "GET", "/v1/decisions/models", path={}, timeout=timeout
        )
        return response.json()

    def get_decision_model(
        self, model_ref: str, *, timeout: float | None = None
    ) -> _types.DecisionModel:
        """GET /v1/decisions/models/:model-ref — immutable status after upload."""
        return self._request(
            "GET",
            "/v1/decisions/models/:model-ref",
            path={"model-ref": model_ref},
            timeout=timeout,
        ).json()

    def get_decision_alias(
        self, alias: str, *, timeout: float | None = None
    ) -> _types.DecisionAlias:
        """GET /v1/decisions/aliases/:alias — current ref before a CAS update."""
        return self._request(
            "GET",
            "/v1/decisions/aliases/:alias",
            path={"alias": alias},
            timeout=timeout,
        ).json()

    def put_decision_alias(
        self,
        alias: str,
        *,
        model_ref: str,
        expected_current: str | None = None,
        timeout: float | None = None,
    ) -> _types.DecisionAlias:
        """PUT /v1/decisions/aliases/:alias — explicit compare-and-swap update."""
        return self._request(
            "PUT",
            "/v1/decisions/aliases/:alias",
            path={"alias": alias},
            body={"model_ref": model_ref, "expected_current": expected_current},
            timeout=timeout,
        ).json()

    def post_decision_training_job(
        self, *, body: JSONValue, timeout: float | None = None
    ) -> _types.DecisionJob:
        """POST /v1/decisions/training/jobs — train on approved local data."""
        return self._request(
            "POST", "/v1/decisions/training/jobs", path={}, body=body, timeout=timeout
        ).json()

    def get_decision_training_job(
        self, job_id: str, *, timeout: float | None = None
    ) -> _types.DecisionJob:
        """GET /v1/decisions/training/jobs/:job-id — durable progress."""
        return self._request(
            "GET",
            "/v1/decisions/training/jobs/:job-id",
            path={"job-id": job_id},
            timeout=timeout,
        ).json()

    def delete_decision_training_job(
        self, job_id: str, *, timeout: float | None = None
    ) -> _types.DecisionJob:
        """DELETE /v1/decisions/training/jobs/:job-id — cancel or discard."""
        return self._request(
            "DELETE",
            "/v1/decisions/training/jobs/:job-id",
            path={"job-id": job_id},
            timeout=timeout,
        ).json()

    def post_systemone(
        self, *, body: JSONValue, timeout: float | None = None
    ) -> _types.DecisionAnswer:
        """POST /v1/systemone — typed decisions from an explicitly named model."""
        response = self._request(
            "POST", "/v1/systemone", path={}, timeout=timeout, body=body
        )
        return response.json()

    def post_voice(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> _types.VoiceJob:
        """POST /v1/voice — json response."""
        response = self._request(
            "POST", "/v1/voice", path={}, query=query, timeout=timeout, content=content
        )
        return response.json()

    def delete_voice_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.Forgotten:
        """DELETE /v1/voice/jobs/:job-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/voice/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_voice_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceJob:
        """GET /v1/voice/jobs/:job-id — json response."""
        response = self._request(
            "GET",
            "/v1/voice/jobs/:job-id",
            path={"job-id": job_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_voice_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """GET /v1/voice/model — json response."""
        response = self._request(
            "GET", "/v1/voice/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_voice_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> _types.VoiceModel:
        """POST /v1/voice/model — json response."""
        response = self._request(
            "POST", "/v1/voice/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def _job_events(self, route, job_id, event_name, snapshot, **options):
        return JobEvents(self, route, job_id, event_name, **options)

    def speech_events(self, job_id: str, **options) -> JobEvents:
        """Synthesis snapshots over SSE (HTTP) or bounded polling (local)."""
        return self._job_events(
            self._job_route("speech", job_id),
            job_id,
            _JOB_EVENTS["synthesize"],
            lambda: self.get_speech_job(job_id),
            **options,
        )

    def voice_events(self, job_id: str, **options) -> JobEvents:
        """Transcription snapshots over SSE (HTTP) or bounded polling (local)."""
        return self._job_events(
            self._job_route("voice", job_id),
            job_id,
            _JOB_EVENTS["transcribe"],
            lambda: self.get_voice_job(job_id),
            **options,
        )

    def session_speech_events(self, sid: str, job_id: str, **options) -> JobEvents:
        """Session-scoped synthesis snapshots. Close never cancels the job."""
        return self._job_events(
            self._job_route("speech", job_id, sid),
            job_id,
            _JOB_EVENTS["synthesize"],
            lambda: self.get_session_speech_job(sid, job_id),
            **options,
        )

    def session_voice_events(self, sid: str, job_id: str, **options) -> JobEvents:
        """Session-scoped transcription snapshots. Close never cancels the job."""
        return self._job_events(
            self._job_route("voice", job_id, sid),
            job_id,
            _JOB_EVENTS["transcribe"],
            lambda: self.get_session_voice_job(sid, job_id),
            **options,
        )

    @staticmethod
    def _job_route(kind, job_id, sid=None):
        prefix = f"/v1/sessions/{_segment(sid)}" if sid is not None else "/v1"
        return f"{prefix}/{kind}/jobs/{_segment(job_id)}/events"


class _UploadStream:
    """Bound a chunked upload to its declared length without buffering the body."""

    def __init__(self, source: BinaryIO, length: int):
        self._source = source
        self._remaining = length

    def read(self, size: int = 8192) -> bytes:
        if size == 0:
            return b""
        if self._remaining == 0:
            if self._source.read(1):
                raise ValueError("decision upload stream length does not match")
            return b""
        chunk = self._source.read(min(size if size > 0 else 8192, self._remaining))
        if not chunk or len(chunk) > self._remaining:
            raise ValueError("decision upload stream length does not match")
        self._remaining -= len(chunk)
        return chunk


class GatewayClient(ExecutionLayer):
    """Use an existing Vis gateway through an explicit HTTP(S) origin.

    `blockether.vis.engine.connect` returns a connected instance of this class.
    Choose this layer when sessions must outlive your Python process, or when
    the engine runs on another machine. It never starts or discovers a gateway,
    and it never reads locally saved credentials. Get an authorized gateway URL
    and token separately. Keep tokens out of source code and logs.

    Args:
        url: HTTP(S) origin such as `https://gateway.example.com`, without a
            path, query, fragment or embedded credentials.
        token: Optional bearer token supplied by your application.
        timeout: Positive finite transport timeout in seconds. Defaults to 30.

    Construction makes no network request. Context entry calls `connect` to
    check protocol compatibility and acquire a client lease. Closing releases
    this client's lease and streams, not the gateway or its stored sessions.
    Reuse the same layer for multiple agents, then close it after those agents.

    ```python
    import os
    from blockether.vis.engine import Agent, connect

    with connect(
        "https://gateway.example.com", token=os.environ["VIS_GATEWAY_TOKEN"]
    ) as client:
        with Agent(project="/workspace/project", execution_layer=client) as agent:
            result = agent.run("Summarize this project without changing files.")
            print(result["status"])
    ```

    The project path is absolute and belongs to the gateway machine, not this
    Python process. TLS certificate verification is enabled. Redirects are
    refused, so credentials cannot follow them to another origin. With a gateway
    on another machine, whole answers and large JSON bodies travel compressed
    with gzip. Event streams stay plain. Each instance and its session handles
    use one calling thread. HTTP failures raise `GatewayError`. Connection,
    deadline and protocol failures use the `TransportError` exception family.
    """

    def __init__(self, url: str, *, token: str | None = None, timeout: float = 30):
        parts = urlsplit(url)
        if (
            parts.scheme not in {"http", "https"}
            or not parts.hostname
            or parts.username is not None
            or parts.password is not None
            or parts.query
            or parts.fragment
            or parts.path not in {"", "/"}
        ):
            raise ValueError(
                "provide an HTTP(S) origin without credentials, path or query"
            )
        self._url = url.rstrip("/")
        self._token = token
        super().__init__(timeout=timeout)
        self._heartbeat_stop = threading.Event()
        self._heartbeat = None
        # Wakes the keepalive at once, for example when an answer names a lost lease.
        self._lease_wake = threading.Event()
        # Why this client cannot send requests anymore, or None.
        self._lease_error: str | None = None
        # The lease that an answer named as unknown, until a new lease replaces it.
        self._lease_lost: str | None = None
        self._lease_seen = 0.0
        # Compression pays only over a network. A loopback exchange stays plain.
        self._is_remote = not _is_loopback(parts.hostname)
        # True after an answer says that this gateway inflates gzip request bodies.
        self._inflates = False
        self._opener = build_opener(_NoRedirect())

    def _open(
        self,
        method,
        route,
        *,
        query=None,
        body=None,
        content=None,
        timeout=None,
        upload_sha256=None,
        upload_length=None,
        is_whole=False,
    ):
        if self._closed:
            raise TransportError("client is closed")
        if self._lease_error:
            raise TransportError(self._lease_error)
        lease = self._lease
        headers = {
            "x-vis-protocol": str(_PROTOCOL),
            "x-vis-min-gateway-protocol": str(_MIN_GATEWAY),
            "x-vis-client": "vis-python",
            "Accept": "application/json",
        }
        if self._token:
            headers["Authorization"] = "Bearer " + self._token
        if lease:
            headers["x-vis-client-id"] = lease
        if body is not None:
            if content is not None:
                raise ValueError("body and content are mutually exclusive")
            content = json.dumps(body, allow_nan=False).encode()
            headers["Content-Type"] = "application/json"
            if self._is_remote and self._inflates and len(content) >= _GZIP_MIN_BYTES:
                content = gzip.compress(content, 6, mtime=0)
                headers["Content-Encoding"] = "gzip"
        # A stream stays plain: each event must arrive when the gateway writes it.
        if is_whole and self._is_remote:
            headers["Accept-Encoding"] = "gzip"
        if upload_sha256 is not None:
            headers["Content-Type"] = "application/zip"
            headers["Transfer-Encoding"] = "chunked"
            # _request sends a hash only with a bounded stream and its length.
            assert content is not None and not isinstance(content, bytes)
            assert upload_length is not None
            content = _UploadStream(content, upload_length)
            headers["X-Content-SHA256"] = upload_sha256
        url = self._url + route + ("?" + urlencode(query) if query else "")
        try:
            raw = self._opener.open(
                Request(url, data=content, headers=headers, method=method),
                timeout=_duration(self.timeout if timeout is None else timeout),
            )
        except HTTPError as exc:
            with exc:
                self._note_lease(lease, exc, is_answered=False)
                self._note_codings(exc)
                detail = exc.read(65536)
                if exc.headers.get("Content-Encoding") == "gzip":
                    detail = _inflated(detail, 65536)
                raise _gateway_error(exc.code, detail, self._token) from None
        except TimeoutError:
            raise VisTimeout("gateway request timed out") from None
        except (URLError, OSError) as exc:
            if isinstance(getattr(exc, "reason", None), TimeoutError):
                raise VisTimeout("gateway request timed out") from None
            raise TransportError("gateway connection failed") from None
        self._note_lease(lease, raw, is_answered=True)
        self._note_codings(raw)
        return raw

    def _open_whole(self, method, route, **options):
        raw = self._open(method, route, is_whole=True, **options)
        if raw.headers.get("Content-Encoding") == "gzip":
            return _Inflated(raw)
        return raw

    def _note_codings(self, answer):
        """Record that an answer says the gateway inflates gzip request bodies."""
        if "gzip" in (answer.headers.get("Accept-Encoding") or "").lower():
            self._inflates = True

    def _note_lease(self, sent, answer, *, is_answered):
        """Record what one gateway answer says about the lease that it carried."""
        headers = getattr(answer, "headers", None)
        if not sent or headers is None:
            return
        if headers.get(_LEASE_POLICY["unknown_header"]) == sent:
            self._lease_lost = sent
            self._lease_wake.set()
        elif is_answered:
            self._lease_seen = time.monotonic()

    def post_decision_model(
        self,
        *,
        content: BinaryIO,
        sha256: str,
        length: int,
        timeout: float | None = None,
    ) -> _types.DecisionModel:
        """POST /v1/decisions/models — stream a verified FP32 bundle once."""
        return self._request(
            "POST",
            "/v1/decisions/models",
            content=content,
            upload_sha256=sha256,
            upload_length=length,
            timeout=timeout,
        ).json()

    def connect(self) -> GatewayClient:
        """Check protocol compatibility and acquire this client's lease once.

        Return this client. Repeated calls reuse its lease. An incompatible
        gateway raises `ProtocolError`. Authentication and other HTTP failures
        raise `GatewayError`.

        A short outage of the gateway does not end the lease. When the gateway
        no longer holds the lease, for example after a restart, the client takes
        a new lease. A client with registered application extensions cannot move
        them to a new lease. It then raises `TransportError` for each request:
        create a new client. A refused keepalive has the same result.
        """
        if self._lease_error:
            raise TransportError(self._lease_error)
        if self._lease:
            return self
        caps = self._request("GET", "/v1/capabilities").json()
        peer = caps.get("protocol", {}) if isinstance(caps, dict) else {}
        if not isinstance(peer, dict):
            raise ProtocolError("malformed gateway handshake")
        version, minimum = peer.get("protocol"), peer.get("min_client")
        if (
            type(version) is not int
            or type(minimum) is not int
            or version < _MIN_GATEWAY
            or minimum > _PROTOCOL
        ):
            raise ProtocolError("incompatible gateway protocol")
        self._ensure_client_lease()
        self._lease_seen = time.monotonic()
        self._heartbeat = threading.Thread(
            target=self._keepalive, name="vis-sdk-lease", daemon=True
        )
        self._heartbeat.start()
        return self

    def _keepalive(self):
        policy = _LEASE_POLICY
        interval = policy["keepalive_ms"] / 1000
        # While the gateway does not answer, try again at the refresh granularity.
        retry = policy["touch_ms"] / 1000
        ttl = policy["ttl_ms"] / 1000
        wait = interval
        while True:
            self._lease_wake.wait(wait)
            self._lease_wake.clear()
            if self._heartbeat_stop.is_set():
                return
            try:
                lost = self._lease_lost is not None and self._lease_lost == self._lease
                if lost or time.monotonic() - self._lease_seen >= ttl:
                    # A restart lost the lease, or the reaper retired it in an outage.
                    if not self._renew_lease():
                        return
                else:
                    with self._open(
                        "GET",
                        policy["keepalive_route"],
                        timeout=min(
                            self.timeout, policy["keepalive_timeout_ms"] / 1000
                        ),
                    ) as raw:
                        raw.read(65536)
                wait = interval
            except GatewayError as exc:
                if exc.status < 500 and exc.status not in (408, 429):
                    self._lease_error = (
                        "client lease keepalive failed; create a new client"
                    )
                    return
                wait = retry
            except (TransportError, OSError):
                wait = retry

    def _renew_lease(self) -> bool:
        """Take a new lease after the gateway lost this one. False when it must stop."""
        if any(extensions.manifest for extensions in self._client_extensions.values()):
            # The registration of these extensions belonged to the lost lease.
            self._lease_error = "client lease lost; create a new client"
            return False
        lease = self._request("POST", "/v1/clients", body={"kind": "python-sdk"}).json()
        self._lease = _field(lease, "client_id", str)
        self._lease_lost = None
        self._lease_seen = time.monotonic()
        return True

    def close(self) -> None:
        """Close streams and release this client's lease without stopping the gateway.

        Stored remote sessions are not deleted. Repeated calls are safe, but
        this closed client cannot be reused. Use `Turn.cancel` when you need
        explicit cancellation rather than merely detaching a client.
        """
        if self._closed:
            return
        self._heartbeat_stop.set()
        self._lease_wake.set()
        if self._heartbeat is not None:
            self._heartbeat.join()
        self._lease_error = None
        try:
            for stream in tuple(self._streams):
                stream.close()
            if self._lease:
                self._request("DELETE", "/v1/clients/:cid", path={"cid": self._lease})
        finally:
            self._closed = True
            self._clear_client_extensions()
            self._lease = None
            self._token = None

    def session_options(self, project=".") -> SessionOptions:
        """Prepare an app-channel session using a path on the gateway machine."""
        path = PurePosixPath(project)
        if not path.is_absolute():
            raise ValueError("project must be an absolute path on the gateway")
        return {"root": str(path), "channel": "app"}


def connect(
    url: str, *, token: str | None = None, timeout: float = 30
) -> GatewayClient:
    """Connect to a Vis gateway that runs separately, and return its client.

    Use this when sessions must outlive your Python program, or when the engine
    runs on another machine. To start an engine on this computer, call
    `blockether.vis.engine.start`. This function never starts or discovers a
    gateway, and it never reads locally saved credentials.

    ```python
    import os
    from blockether.vis.engine import Agent, connect

    with connect(
        "https://gateway.example.com", token=os.environ["VIS_GATEWAY_TOKEN"]
    ) as client:
        with Agent(project="/workspace/project", execution_layer=client) as agent:
            print(agent.run("Summarize this project without changing files.")["status"])
    ```

    Args:
        url: HTTP(S) origin such as `https://gateway.example.com`, without a
            path, query, fragment or embedded credentials.
        token: Optional bearer token supplied by your application.
        timeout: Positive finite transport timeout in seconds. Defaults to 30.

    The call checks protocol compatibility and takes a client lease. Close the
    client, or use it as a context manager. Closing releases the lease and the
    streams of this client, not the gateway or its stored sessions.

    Raises:
        ValueError: `url` is not a plain HTTP(S) origin.
        GatewayError: The gateway refused the connection, for example the token.
        TransportError: The gateway cannot be reached or uses another protocol.
    """
    client = GatewayClient(url, token=token, timeout=timeout)
    try:
        return client.connect()
    except BaseException:
        client.close()
        raise


@dataclass(frozen=True, slots=True)
class Session:
    """A conversation handle bound to one execution layer and session ID.

    Get it from `Agent.session`, `ExecutionLayer.create_session` or
    `ExecutionLayer.session`. Constructing a handle does not read the session.
    The handle borrows the layer. Keep the layer open while you use the handle.

    Use `send` for a new turn and `turns` for history. `read` fetches the
    current session state. `events` follows progress. `input_views` and `answer`
    let your application handle pending human input. `transcript`, `artifacts`,
    `upload` and `download_attachment` cover content exchange. `council` binds
    messaging and managed-agent controls to this conversation.

    Requests return dictionaries in contract form, unless a method documents a
    typed handle or `Response`. HTTP errors raise `GatewayError`, including
    missing or inaccessible session IDs. No method automatically retries
    mutations.
    """

    client: ExecutionLayer
    id: str

    def _call(self, method, suffix="", **kwargs) -> Any:
        path = {"sid": self.id, **kwargs.pop("path", {})}
        response = self.client._request(
            method, "/v1/sessions/:sid" + suffix, path=path, **kwargs
        )
        return response.json() if response.content else None

    def council(self, *, group_id: str | None = None) -> Council:
        """Bind communication to the current run without blocking session-only controls.

        Disabled or unavailable Council groups are reported when you use
        communication or group_id. A successful binding keeps the activation
        that it had when it was acquired.
        """
        from ._council import Council

        try:
            binding = self._call(
                "GET",
                "/council",
                query={"group_id": group_id} if group_id is not None else {},
            )
        except GatewayError as error:
            if error.code not in {"disabled", "group-not-found"}:
                raise
            return Council(self, "", None, error)
        validate("council", "binding", binding)
        return Council(self, binding["default_group_id"], binding["activation_id"])

    def read(self) -> _types.Session:
        """Fetch the current session record as a dictionary.

        Pending application-owned tool calls are serviced on the calling thread
        before fetching state. This is a fresh read, not a cached snapshot.
        """
        self.client._pump_client_extensions(self.id)
        return self._call("GET")

    def update(self, **fields: object) -> _types.Session:
        """Patch session fields and return the engine's JSON response.

        Field names and validation follow the gateway session-update contract.
        The engine rejects unknown or invalid fields.
        """
        return self._call("PATCH", body=fields)

    def set_group(self, group: SessionGroup | str | None) -> _types.Session:
        """Put this session in a group and return the session record.

        The session moves to the project of the group. With None, the session
        leaves its group and stays in its project.
        """
        group_id = None if group is None else _group_id(group)
        return self._call("PUT", "/group", body={"group_id": group_id})

    def delete(self) -> None:
        """Ask the engine to delete this conversation and return its response.

        This is a destructive session operation, not a connection close. It does
        not undo changes already made to project files. Do not use this handle
        for further conversation requests after successful deletion.
        """
        return self._call("DELETE")

    def turns(self, **query: str | int | float | bool | None) -> list[_types.Turn]:
        """Read turn history, or the queued subset with status='queued'."""
        return _field(self._call("GET", "/turns", query=query), "turns", list)

    def artifacts(self, **query) -> _types.SessionArtifacts:
        """Return the gateway's artifact inventory without downloading its bytes."""
        return self._call("GET", "/artifacts", query=query)

    def transcript(self, *, format: str = "json", **query) -> Response:
        """Read a JSON, Markdown or HTML transcript, preserving bytes and headers."""
        suffixes = {"json": "", "markdown": ".md", "html": ".html"}
        if format not in suffixes:
            raise ValueError("transcript format must be json, markdown or html")
        return self.client._request(
            "GET",
            "/v1/sessions/:sid/transcript" + suffixes[format],
            path={"sid": self.id},
            query=query,
        )

    def upload(
        self, content: bytes, *, filename: str, media_type: str
    ) -> _types.Upload:
        """Upload bytes. Pass the returned upload_id in send(attachments=[...])."""
        if not filename or not media_type:
            raise ValueError("filename and media_type are required")
        return self._call(
            "POST",
            "/attachments",
            content=content,
            query={"filename": filename, "media_type": media_type},
        )

    def download_attachment(self, iteration_id: str, index: int) -> Response:
        """Download one persisted iteration attachment without decoding binary data."""
        if type(index) is not int or index < 0:
            raise ValueError("attachment index must be a nonnegative integer")
        return self.client._request(
            "GET",
            "/v1/sessions/:sid/iterations/:iid/attachments/:idx",
            path={"sid": self.id, "iid": iteration_id, "idx": index},
        )

    def attachments(self, turns: Any = None) -> list[Attachment]:
        """List the files that the agent attached, oldest turn first.

        Without `turns`, the list covers the whole conversation. `turns` is one
        turn or an iterable of turns. A turn is a `Turn`, a turn ID, or an integer
        index into `turns()`: 0 is the first turn and -1 is the last turn. An
        index outside the history raises `IndexError`.

        The list holds metadata only. Call `Attachment.read` for the bytes. When
        the agent attaches a filename again, the list holds each version. A file
        that a person uploaded with a request is not in the list, and a file for
        the model only is not in it either.
        """
        found = [
            Attachment._from(self, row)
            for row in _field(self.artifacts(), "artifacts", list)
        ]
        if turns is None:
            return found
        wanted = self._turn_ids(turns)
        return [item for item in found if item.turn_id in wanted]

    def _turn_ids(self, turns: Any) -> set[str]:
        """Resolve `Turn` handles, turn IDs and history indexes to turn IDs."""
        if isinstance(turns, (Turn, str)) or not isinstance(turns, Iterable):
            turns = [turns]
        history: list[Any] = []
        ids = set()
        for turn in turns:
            if isinstance(turn, Turn):
                ids.add(turn.id)
            elif isinstance(turn, str):
                ids.add(turn)
            elif type(turn) is int:
                history = history or self.turns()
                ids.add(_field(history[turn], "turn_id", str))
            else:
                raise TypeError("a turn must be a Turn, a turn ID or an integer index")
        return ids

    def goal(
        self, objective: str, *, iteration_budget: int | None = None, **options
    ) -> Turn:
        """Submit an explicit goal through /goal and return its execution Turn.

        No goal is inferred from ordinary messages. Read its state in
        `read()['goal']`. To pause, resume or cancel it, use
        `send('/goal --pause')`, `--resume` or `--cancel`. `--clear` forgets the
        goal; the goal then has status 'cleared' and the model no longer sees it.

        The optional budget counts loop iterations, including prose and empty
        replies. The last iteration can run its tools, and then the limit blocks
        the next model request. Token usage is recorded only as a statistic.
        """
        # The gateway schema owns the bounds of a goal.
        rules = definition("gateway", "session_goal")["properties"]
        text, budget = rules["objective"], rules["iteration_budget"]
        if (
            not isinstance(objective, str)
            or not text["minLength"] <= len(objective.strip()) <= text["maxLength"]
        ):
            raise ValueError(
                f"objective must contain {text['minLength']}–{text['maxLength']} "
                "characters"
            )
        if iteration_budget is not None and (
            type(iteration_budget) is not int
            or not budget["minimum"] <= iteration_budget <= budget["maximum"]
        ):
            raise ValueError(
                f"iteration_budget must be an integer from {budget['minimum']} to "
                f"{budget['maximum']}"
            )
        if "token_budget" in options:
            raise TypeError(
                "Use iteration_budget; goal budgets count iterations, not tokens"
            )
        budget = f"--budget {iteration_budget} " if iteration_budget is not None else ""
        return self.send(f"/goal {budget}-- {objective.strip()}", **options)

    def send(
        self,
        request: str,
        *,
        idempotency_key: str | None = None,
        **options: object,
    ) -> Turn:
        """Submit a user message or slash command and return its `Turn` handle.

        Args:
            request: Message text sent to this conversation.
            idempotency_key: Stable key for retrying the same submission after
                uncertain IO. If you omit it, each call creates a new key.
            **options: Turn submission fields such as `provider`, `model` and
                `attachments`. Upload attachment bytes first with `upload`, and
                pass the returned upload IDs in `attachments`.

        A return does not mean that the model has finished. Use `Turn.wait` or
        `Turn.read`. The returned cursor marks the event position before this
        submission. Pass it to `events` to follow later progress. Model requests
        can cost money and change project files. Submission failures propagate,
        and the SDK never automatically retries this mutation.
        """
        cursor = _field(self._call("GET", "/seq"), "seq", int)
        data = self._call(
            "POST",
            "/turns",
            body={
                **options,
                "request": request,
                "idempotency_key": idempotency_key or str(uuid.uuid4()),
            },
        )
        return Turn(self, _field(data, "turn_id", str), cursor)

    @overload
    def run(
        self,
        request: str,
        *,
        timeout: float = 300,
        response_model: None = None,
        **options,
    ) -> _types.Turn: ...

    @overload
    def run(
        self,
        request: str,
        *,
        response_model: type[ResponseModel],
        timeout: float = 300,
        max_corrections: int = 2,
        **options,
    ) -> ResponseModel: ...

    def run(
        self,
        request: str,
        *,
        timeout: float = 300,
        response_model: type[ResponseModel] | None = None,
        max_corrections: int = 2,
        **options,
    ) -> _types.Turn | ResponseModel:
        """Submit a request and wait for its result, as `Agent.run` does.

        Without `response_model`, return the turn dictionary in contract form.
        With a Pydantic model, validate the final answer and return the instance.
        An invalid answer starts up to `max_corrections` correction turns.
        `Agent.run` describes the answer rules, the errors and the timeout.
        """
        from ._agent import _run

        return _run(
            lambda: self,
            request,
            timeout=timeout,
            response_model=response_model,
            max_corrections=max_corrections,
            options=options,
        )

    def model(self) -> SessionModel:
        """Read the model of the next turn: the pin, else the default pair."""
        return SessionModel.from_wire(self._call("GET", "/model"))

    def set_model(self, provider: str | None, model: str | None) -> SessionModel:
        """Pin the provider and model of the next turns, or clear the pin with None.

        Get the names from `ExecutionLayer.models`. The gateway refuses a provider
        that it does not serve. A running or queued turn keeps its model. A model
        request can cost money; `Model.pricing` gives the rates.
        """
        if (provider is None) != (model is None):
            raise ValueError("provider and model must be given together")
        return SessionModel.from_wire(
            self._call("PATCH", "/model", body={"provider": provider, "model": model})
        )

    def fork(self, through_turn_id: str | None = None) -> Session:
        """Copy this conversation into a new independent session and return it.

        The fork holds every turn through `through_turn_id`, or through the last
        turn when you omit it. The source session does not change.
        """
        body = {} if through_turn_id is None else {"through_turn_id": through_turn_id}
        data = self._call("POST", "/forks", body=body)
        return self.client.session(_field(_field(data, "session", dict), "id", str))

    def export(self) -> bytes:
        """Return a snapshot of this session and its managed children as bytes.

        Save the bytes as a `.vissnap` file, and restore them with
        `ExecutionLayer.restore_session`. The snapshot holds turns, attachments
        and the Council entries inside the session tree. It does not hold
        provider credentials or workspace files. The `session_snapshot_limit`
        setting limits its size.
        """
        return self.client.get_session_snapshot(self.id).content

    def input_views(self) -> list[InputView]:
        """Read pending input requests as typed `blockether.vis.views.InputView` records.

        Use their view IDs with `answer` to submit values. An empty list means no
        input requests were returned in this snapshot.
        """
        data = _field(self._call("GET", "/views/input"), "requests", list)
        try:
            return [InputView.from_wire(item) for item in data]
        except ValueError:
            raise ProtocolError("invalid input View response") from None

    def live_views(
        self, placement: str | None = None, archived: bool = False
    ) -> list[LiveView]:
        """Read current live views as typed `blockether.vis.views.LiveView` records.

        This fetches a snapshot and does not subscribe to future updates. Use
        `events` to follow view events. A `placement` (`inline`, `panel` or
        `attachment`) keeps only those views. `archived=True` lists only the
        archived apps. Malformed view data raises `ProtocolError`.
        """
        query = {} if placement is None else {"placement": str(placement)}
        if archived:
            query["archived"] = "true"
        data = _field(
            self._call("GET", "/views/live", query=query or None), "views", list
        )
        try:
            return [LiveView.from_wire(item) for item in data]
        except ValueError:
            raise ProtocolError("invalid live View response") from None

    def live_view(self, view_id: str) -> LiveView:
        """Read one live view or app by id, also a closed or restored app.

        This is a snapshot, like `live_views`. Malformed view data raises
        `ProtocolError`.
        """
        answer = self._call("GET", "/views/live/:view-id", path={"view-id": view_id})
        data = _field(answer, "view", dict)
        try:
            return LiveView.from_wire(data)
        except ValueError:
            raise ProtocolError("invalid live View response") from None

    def view_action(self, view_id: str, action: str, **values) -> _types.ViewOutcome:
        """Send an operator action to a view and return the engine's JSON response.

        `values` supplies the action-specific fields of the operator action
        schema. The request is validated before submission. For the common case
        of submitting input values, use `answer`. A refused action of a closed,
        disconnected or changed app raises `GatewayError` with status 409 and the
        refusal `reason` as `code`, for example `disconnected`.
        """
        body = {**values, "action": action}
        validate("view", "operator_action", body)
        return self._call(
            "POST",
            "/views/:view-id/actions",
            path={"view-id": view_id},
            body=body,
        )

    def attach(self, app, *, owner, on_event=None) -> LiveAttachment:
        """Open or adopt an app in this session as owner program `owner`.

        `app` is a `vis.ui.app(...)` declaration or an app attachment row. The
        engine records the owner as `sdk/<owner>`; `owner` uses 1 to 64 characters
        of `a-z`, `0-9`, `.`, `_` and `-`. A declaration with a `key` adopts the
        open app of the same owner and key, so a restarted program gets its app
        back. Answers the same `LiveAttachment` handle that extensions use.

        `on_event(event)` runs on a daemon thread that long-polls the app events,
        until the app closes. A callback error is logged and polling goes on. To
        serve events on the calling thread, use `LiveAttachment.run` instead.
        """
        from blockether.vis.extension import LiveAttachment, _AppDoor

        return LiveAttachment._from(app, door=_AppDoor(self, owner), on_event=on_event)

    def keybindings(self) -> list[_types.Keybinding]:
        """Read the TUI key bindings that the extensions of this session declare.

        Each binding has `key`, `extension` and `command` with `label` and
        `group`, `run` with `label` and `group`, or `action`. The list is in
        extension order, then declaration order.
        """
        answer = self._call("GET", "/keybindings")
        return list(_field(answer, "keybindings", list))

    def run_keybinding(
        self, extension: str, key: str, group: str | None = None
    ) -> _types.KeybindingRunResult:
        """Run the function of one run binding, as if the person pressed its key.

        Give the `extension`, `key` and `group` that `keybindings()` lists. The
        run adds no turn. The answer is `{"status": "ok" | "error", "title",
        "body"}`; a function that fails answers `status` `error`.
        """
        body = {"extension": extension, "key": key}
        if group is not None:
            body["group"] = group
        answer = self._call("POST", "/keybindings/run", body=body)
        if not isinstance(answer, dict):
            raise ProtocolError("invalid key binding run result")
        return cast(_types.KeybindingRunResult, dict(answer))

    def answer(self, view_id, values) -> _types.ViewOutcome:
        """Submit input values to a view, returning the engine's response.

        Pass a view ID from `input_views` and values matching that input request.
        This is shorthand for `view_action(view_id, "submit", values=values)`.
        """
        return self.view_action(view_id, "submit", values=values)

    def events(
        self,
        *,
        cursor: int = 0,
        reconnects: int = 3,
        retry_delay: float = 0.2,
        idle_timeout: float | None = None,
    ) -> Events:
        """Follow this conversation's progress as an iterable of `Event` records.

        Args:
            cursor: Nonnegative replay position. Defaults to 0.
            reconnects: Retries after a lost connection. Defaults to 3.
            retry_delay: Seconds between retries. Defaults to 0.2.
            idle_timeout: Positive seconds without a new event after which the
                iteration ends normally, with no exception. The time starts when
                you call this method and again after each event. Heartbeats do
                not count as events. Defaults to None: the stream follows the
                conversation until you close it.

        Use a context manager to close the stream when you stop iterating. The
        stream spans the conversation, not just one turn. To track a specific
        request, filter on `turn_id`. Closing the stream does not cancel work.
        `close` is safe from another thread and stops a waiting iteration at once.

        Iterate with `for`, or with `async for` in a coroutine (`async with`
        closes the stream). The async form reads on one worker thread that the
        stream owns, one read at a time. Do not iterate one stream from two tasks.
        Cancellation of the waiting task closes the stream.

        Gateway sessions normally use SSE. Local execution and application-owned
        callbacks use finite event-page polling so Python callbacks run on the
        calling thread, or on the worker thread of `async for`. Both expose the
        same event interface.
        """
        options = {
            "cursor": cursor,
            "reconnects": reconnects,
            "retry_delay": retry_delay,
            "idle_timeout": idle_timeout,
        }
        extensions = self.client._client_extensions.get(self.id)
        if extensions is not None and extensions.manifest:
            return _SessionPollingEvents(self, **options)
        return Events(self, **options)


@dataclass(frozen=True, slots=True)
class Turn:
    """Handle for one submitted request, independent of other session turns.

    Returned by `Agent.send` or `Session.send`. `id` identifies this request.
    `cursor` is the session event position captured before submission. The
    handle borrows its session and execution layer. Keep that layer open while
    you call `read`, `wait` or `cancel`.
    """

    session: Session
    id: str
    cursor: int = 0

    def read(self, *, timeout=None) -> _types.Turn:
        """Fetch this turn's current record, including its `status`.

        `timeout` optionally overrides this request's transport deadline in
        seconds. Application-owned callbacks are serviced before reading state.
        """
        self.session.client._pump_client_extensions(self.session.id)
        return self.session._call(
            "GET", "/turns/:tid", path={"tid": self.id}, timeout=timeout
        )

    def cancel(self) -> _types.TurnCancel:
        """Request cancellation of this turn and return the engine's response.

        Use `read` or `wait` to inspect the resulting status. Cancellation does
        not revert file edits or other side effects that already happened.
        """
        return self.session._call("POST", "/turns/:tid/cancel", path={"tid": self.id})

    def attachments(self) -> list[Attachment]:
        """List the files that the agent attached in this turn.

        The rules are those of `Session.attachments`. Read a finished turn for a
        complete list; a running turn can attach more files.
        """
        return self.session.attachments(self)

    def wait(self, *, timeout: float = 300) -> _types.Turn:
        """Wait until this turn finishes or suspends, returning its record.

        Args:
            timeout: Positive finite overall wait deadline in seconds.

        Returns:
            A dictionary in contract form. Its `status` is `completed`,
            `failed`, `cancelled`, `suspended` or `error`. A failure status is
            data, not a raised model-work exception. Inspect it before you use
            the result.

        Raises:
            VisTimeout: The deadline expired. This method does not request
                cancellation. You can wait again or explicitly call `cancel`.

        Transport and HTTP errors also propagate. For a local engine, a timeout
        of the underlying pipe operation closes that process. This keeps replies
        in order. It is different from only ending the wait.

        The record is read once, and again after the session event stream
        reports the end of the turn. Application-owned callbacks are serviced
        while the stream waits. The reconnect budget of the stream counts only
        connections that bring no new event, so a proxy that ends long streams
        does not end the wait.
        """
        end = time.monotonic() + _duration(timeout)

        def remaining():
            left = end - time.monotonic()
            if left <= 0:
                raise VisTimeout("turn wait timed out; turn was not cancelled")
            return left

        # Read the position first: a turn that is still open here ends later.
        cursor = _field(
            self.session._call("GET", "/seq", timeout=remaining()), "seq", int
        )
        data = self.read(timeout=remaining())
        if _field(data, "status", str) in _TURN_SETTLED_STATUSES:
            return data
        while True:
            events = self.session.events(cursor=cursor)
            events._deadline = end
            is_progress = False
            try:
                with events:
                    for event in events:
                        is_progress = is_progress or event.type != "subscription.ready"
                        if event.type == "session.deleted" or (
                            event.turn_id == self.id and event.type in _TURN_END_EVENTS
                        ):
                            data = self.read(timeout=remaining())
                            if _field(data, "status", str) in _TURN_SETTLED_STATUSES:
                                return data
            except VisTimeout:
                raise VisTimeout(
                    "turn wait timed out; turn was not cancelled"
                ) from None
            except TransportError:
                # A proxy can end each connection. Streams that brought new events
                # are not failures: follow again from the cursor, until the deadline.
                if not is_progress:
                    raise
                cursor = events.cursor
                continue
            raise TransportError("event stream ended before the turn")


@dataclass(frozen=True, slots=True)
class Attachment:
    """One file that the agent attached in a turn.

    Returned by `Session.attachments` and `Turn.attachments`. The record holds
    metadata only: `read` downloads the bytes and `text` decodes them. `index`
    is the position of the file in the iteration `iteration_id`, the address
    that `Session.download_attachment` takes. `version` counts from 1 and grows
    each time the agent attaches the same `filename` again. The record borrows
    its session and execution layer. Keep that layer open while you read.
    """

    session: Session
    turn_id: str
    iteration_id: str
    index: int
    filename: str | None
    media_type: str
    kind: str
    size: int
    version: int
    id: str | None = None

    @classmethod
    def _from(cls, session: Session, row: Any) -> Attachment:
        """Build the record from one row of the gateway artifact inventory."""
        filename = row.get("filename") if isinstance(row, dict) else None
        identity = row.get("attachment_id") if isinstance(row, dict) else None
        if not isinstance(filename, (str, type(None))) or not isinstance(
            identity, (str, type(None))
        ):
            raise ProtocolError("invalid response field: attachment")
        return cls(
            session=session,
            turn_id=_field(row, "turn_id", str),
            iteration_id=_field(row, "iteration_id", str),
            index=_field(row, "index", int),
            filename=filename,
            media_type=_field(row, "media_type", str),
            kind=_field(row, "kind", str),
            size=_field(row, "size", int),
            version=_field(row, "version", int),
            id=identity,
        )

    def read(self) -> bytes:
        """Download the bytes of the file."""
        return self.session.download_attachment(self.iteration_id, self.index).content

    def text(self, encoding: str = "utf-8") -> str:
        """Download the file and decode it as text."""
        return self.read().decode(encoding)


class _ActivityStreamDecoder:
    """Rebuild compact Activity receipts of ONE SSE connection.

    The gateway sends a form's receipt whole once on a connection. Each later
    `block.activity` event of that form carries `row_ids`, every row id in order,
    and only the rows that changed. This decoder returns the whole receipt without
    `row_ids` and passes every other event unchanged. It mirrors
    `com.blockether.vis.contract.activity/decode-stream-event`.
    """

    _FORM_LIMIT = 256

    def __init__(self):
        self._forms = {}

    @staticmethod
    def _rows(activity):
        rows = activity.get("rows")
        if not isinstance(rows, list):
            return None
        by_id = {}
        for row in rows:
            row_id = row.get("id") if isinstance(row, dict) else None
            if not isinstance(row_id, str) or not row_id or row_id in by_id:
                return None
            by_id[row_id] = row
        return by_id

    def __call__(self, event):
        if not isinstance(event, dict):
            return event
        turn = (event.get("session_id"), event.get("turn_id"))
        activity = event.get("activity")
        if event.get("type") != "block.activity" or not isinstance(activity, dict):
            if event.get("type") in _TURN_TERMINAL_EVENTS:
                self._forms = {k: v for k, v in self._forms.items() if k[:2] != turn}
            return event
        key = (*turn, event.get("iteration"), event.get("form_index"))
        changed = self._rows(activity)
        row_ids = event.get("row_ids")
        if isinstance(row_ids, list) and changed is not None:
            known = {**self._forms.get(key, {}), **changed}
            event = {k: v for k, v in event.items() if k != "row_ids"}
            event["activity"] = {
                **activity,
                "rows": [
                    known[i] for i in row_ids if isinstance(i, str) and i in known
                ],
            }
        rows = self._rows(event["activity"])
        if rows is None:
            self._forms.pop(key, None)
        else:
            if len(self._forms) >= self._FORM_LIMIT:
                self._forms = {}
            self._forms[key] = rows
        return event


_T = TypeVar("_T")


class _EventStream(Generic[_T]):
    """One bounded SSE reader. Subclasses own event identity and replay policy.

    The owner thread iterates. `close` is safe from any thread: it sets a flag,
    shuts down the open response socket and wakes a retry or poll delay. The
    generator steps under `_step`, so a close from another thread never resumes
    a running generator. `async for` steps on one worker thread of the stream.
    """

    def __init__(self, client, *, reconnects=3, retry_delay=0.2, idle_timeout=None):
        if (
            type(reconnects) is not int
            or reconnects < 0
            or not math.isfinite(retry_delay)
            or retry_delay < 0
        ):
            raise ValueError("invalid replay options")
        self.client = client
        self.reconnects, self.retry_delay = reconnects, retry_delay
        self._idle_timeout = None if idle_timeout is None else _duration(idle_timeout)
        self._idle_end: float | None = None
        self._raw = None
        self._closing = threading.Event()
        # Guards `_raw` and `_worker` against a close from another thread.
        self._guard = threading.Lock()
        self._step = threading.Lock()
        self._worker: ThreadPoolExecutor | None = None
        # A monotonic end of an SDK wait. It replaces the idle timeout.
        self._deadline: float | None = None
        self._touch()
        self._iterator = self._iterate()
        client._streams.add(self)

    @property
    def _closed(self) -> bool:
        return self._closing.is_set()

    def __enter__(self) -> Self:
        return self

    def __exit__(self, *args) -> None:
        self.close()

    def __iter__(self) -> Self:
        return self

    def __next__(self) -> _T:
        with self._step:
            if not self._closed:
                event = next(self._iterator)
                if not self._closed:
                    return event
            # A close from another thread cannot close a running generator.
            self._iterator.close()
            raise StopIteration

    async def __aenter__(self) -> Self:
        return self

    async def __aexit__(self, *args) -> None:
        self.close()

    def __aiter__(self) -> Self:
        return self

    async def __anext__(self) -> _T:
        loop = asyncio.get_running_loop()
        with self._guard:
            if self._closed:
                raise StopAsyncIteration
            if self._worker is None:
                self._worker = ThreadPoolExecutor(1, thread_name_prefix="vis-events")
            future = loop.run_in_executor(self._worker, self._next_or_none)
        try:
            event = await future
        except asyncio.CancelledError:
            self.close()
            raise
        if event is None:
            raise StopAsyncIteration
        return event

    def _next_or_none(self) -> _T | None:
        """Step once on the worker thread. A future cannot carry StopIteration."""
        try:
            return self.__next__()
        except StopIteration:
            return None

    def close(self) -> None:
        """Release this event subscription without cancelling conversation work.

        Close the active response and iterator, and detach the stream from its
        client. Repeated calls are safe, and further iteration stops. The client
        and session stay open. Context exit calls this method automatically.
        A call from another thread stops a waiting iteration at once: the
        iteration ends normally and does not connect again.
        """
        self._closing.set()
        with self._guard:
            sock = _response_socket(self._raw) if self._raw is not None else None
            if sock is not None:
                try:
                    # A blocked readline returns end of stream.
                    sock.shutdown(socket.SHUT_RDWR)
                except OSError:
                    pass
            worker = self._worker
        if worker is not None:
            worker.shutdown(wait=False)
        # A pipe failure can close its owning client from inside this generator.
        if self._step.acquire(blocking=False):
            try:
                self._iterator.close()
            finally:
                self._step.release()
        self.client._streams.discard(self)

    def _touch(self) -> None:
        """Start the idle time again, after the call or a new event."""
        if self._idle_timeout is not None:
            self._idle_end = time.monotonic() + self._idle_timeout

    def _limit(self) -> float | None:
        """Answer the earliest monotonic end of this iteration, or None."""
        ends = [end for end in (self._deadline, self._idle_end) if end is not None]
        return min(ends) if ends else None

    def _should_stop(self) -> bool:
        """Answer True when the iteration ends normally: a close or idle time."""
        return self._closed or (
            self._idle_end is not None and time.monotonic() >= self._idle_end
        )

    def _read_timeout(self):
        """Answer the socket read timeout, shortened to the deadline of a wait."""
        if self._expired():
            raise VisTimeout("event wait timed out")
        limit = self._limit()
        if limit is None:
            return self.client.timeout
        return min(self.client.timeout, max(limit - time.monotonic(), 0.001))

    def _expired(self):
        return self._deadline is not None and time.monotonic() >= self._deadline

    def _attach(self, raw) -> bool:
        """Publish the open response for `close`. Answer False after a close."""
        with self._guard:
            if self._closed:
                return False
            self._raw = raw
            return True

    def _detach(self) -> None:
        with self._guard:
            self._raw = None

    def _iterate(self) -> Generator[_T, None, None]:
        try:
            for attempt in range(self.reconnects + 1):
                if attempt and self._closing.wait(self.retry_delay):
                    return
                if self._should_stop():
                    return
                try:
                    route, query = self._endpoint()
                    with self.client._open(
                        "GET", route, query=query, timeout=self._read_timeout()
                    ) as raw:
                        if not self._attach(raw):
                            return
                        decode = _ActivityStreamDecoder()
                        if raw.headers.get_content_type() != "text/event-stream":
                            raise ProtocolError("expected event stream")
                        data, size, event_name = [], 0, None
                        for line in iter(lambda: raw.readline(1048577), b""):
                            if self._should_stop():
                                return
                            if self._limit() is not None:
                                # Heartbeats keep the socket busy past a deadline.
                                _set_read_timeout(raw, self._read_timeout())
                            size += len(line)
                            if size > 1048576:
                                raise ProtocolError("event frame exceeds 1 MiB")
                            if line in {b"\n", b"\r\n", b"\r"}:
                                if data:
                                    try:
                                        value = decode(json.loads(b"\n".join(data)))
                                    except (ValueError, UnicodeError):
                                        raise ProtocolError(
                                            "malformed event JSON"
                                        ) from None
                                    event = self._accept(event_name, value)
                                    if event is not None:
                                        yield event
                                        self._touch()
                                        if self._terminal(event):
                                            return
                                data, size, event_name = [], 0, None
                            elif line.startswith(b"data:"):
                                data.append(line[5:].removeprefix(b" ").rstrip(b"\r\n"))
                            elif line.startswith(b"event:"):
                                try:
                                    event_name = line[6:].strip().decode("utf-8")
                                except UnicodeError:
                                    raise ProtocolError("invalid event name") from None
                except (ProtocolError, GatewayError):
                    raise
                except (OSError, TransportError, http.client.IncompleteRead):
                    # A cut chunked stream raises IncompleteRead, also after close.
                    if self._expired():
                        raise VisTimeout("event wait timed out") from None
                    if self._should_stop():
                        return
                    if reason := getattr(self.client, "_lease_error", None):
                        raise TransportError(reason) from None
                finally:
                    self._detach()
            raise TransportError("event reconnect budget exhausted")
        finally:
            self.client._streams.discard(self)

    def _terminal(self, event):
        return False

    def _endpoint(self):
        """Return the route and query of this subscription."""
        raise NotImplementedError

    def _accept(self, name, value) -> _T | None:
        """Return the event of one frame, or None to skip the frame."""
        raise NotImplementedError


class _PollingEvents(_EventStream[_T]):
    """Poll finite event pages so callback code runs on the SDK calling thread."""

    def _iterate(self) -> Generator[_T, None, None]:
        idle = time.monotonic() + self.client.timeout
        delay = _POLL_MIN_SECONDS
        try:
            while True:
                if self._expired():
                    raise VisTimeout("event wait timed out")
                if self._should_stop():
                    return
                if self._limit() is None and time.monotonic() >= idle:
                    raise VisTimeout("event stream idle timeout")
                self._pump()
                for name, value in self._page():
                    event = self._accept(name, value)
                    if event is not None:
                        yield event
                        self._touch()
                        if self._terminal(event):
                            return
                        idle = time.monotonic() + self.client.timeout
                        delay = _POLL_MIN_SECONDS
                limit = self._limit()
                limit = idle if limit is None else limit
                # A close wakes the delay at once.
                self._closing.wait(min(delay, max(0, limit - time.monotonic())))
                # A quiet stream costs at most one page each second.
                delay = min(delay * 2, _POLL_MAX_SECONDS)
        finally:
            self.client._streams.discard(self)

    def _pump(self):
        pass

    def _page(self):
        """Return the next finite page of `(event name, value)` pairs."""
        raise NotImplementedError


class Events(_EventStream["StreamEvent"]):
    """Iterate typed session events, with replay and bounded reconnection.

    Get a stream from `Session.events`. It yields `Event` objects. Inspect
    `type`, `turn_id` and the event-specific `data` instead of parsing raw SSE.
    The stream covers the whole conversation and does not stop when one turn
    completes. Use it in a `with` block, or call `close` when you are done.

    Args:
        session: Conversation whose events you want to follow.
        cursor: Nonnegative replay position, normally a saved cursor or the
            cursor from `Session.send`. Defaults to 0.
        **options: `reconnects` limits retries (default 3). `retry_delay` is a
            nonnegative finite delay in seconds (default 0.2). `idle_timeout`
            ends the iteration normally after that many seconds without a new
            event (default None).

    The stream replays every stored event after `cursor`, including the progress
    of turns that finished before you subscribed. Duplicate events are
    suppressed. `subscription.ready` resets the cursor, also after a daemon
    restart. So save the stream's current `cursor`, and do not assume that it
    always increases.

    Without `idle_timeout`, the socket read timeout is the client's timeout.
    `close` is safe from another thread. The stream also supports `async for`
    and `async with`; see `Session.events`. Closing the stream never cancels a
    turn. Invalid replay options raise `ValueError`. Malformed events raise
    `ProtocolError`.
    """

    def __init__(self, session: Session, *, cursor=0, **options):
        if type(cursor) is not int or cursor < 0:
            raise ValueError("invalid replay cursor")
        self.session, self.cursor = session, cursor
        super().__init__(session.client, **options)

    def _endpoint(self):
        return "/v1/events", {
            "sids": f"{self.session.id}:{self.cursor}",
            "replay": "full",
        }

    def _accept(self, name, value) -> StreamEvent | None:
        event = cast("StreamEvent", Event.from_wire(value))
        if event.session_id != self.session.id:
            raise ProtocolError("unexpected event session")
        if name is not None and name != event.type:
            raise ProtocolError("event name disagrees with payload")
        seq = event.cursor if event.type == "subscription.ready" else event.seq
        assert seq is not None  # Event.from_wire requires the field of each type.
        if event.type == "subscription.ready" or seq > self.cursor:
            self.cursor = seq
            return event
        return None


class _SessionPollingEvents(_PollingEvents["StreamEvent"], Events):
    """Use the session journal for stdio and application-owned gateway callbacks."""

    def _pump(self):
        self.client._pump_client_extensions(self.session.id)

    def _page(self):
        data = self.client.get_session_events_since(
            self.session.id, query={"cursor": self.cursor, "replay": "full"}
        )
        if not isinstance(data, dict) or not isinstance(data.get("events"), list):
            raise ProtocolError("malformed session event page")
        return [(None, value) for value in data["events"]]


@dataclass(frozen=True, slots=True)
class JobEvent:
    """Current speech job snapshot, not a session event or a replay cursor."""

    type: str
    id: str
    phase: str
    is_done: bool
    data: dict[str, Any] = field(default_factory=dict)


class JobEvents(_EventStream[JobEvent]):
    """Snapshot-first job SSE.

    A reconnect reads the state again. A terminal is_done ends the stream.

    Job streams have no cursor and make no exactly-once claim. Identical
    snapshots are suppressed within this iterator. Other fields stay as the JSON
    that the contract defines.
    """

    def __init__(self, client, route, job_id, event_name, **options):
        self._route, self._job_id, self._event_name = route, job_id, event_name
        self._previous = None
        super().__init__(client, **options)

    def _endpoint(self):
        return self._route, None

    def _accept(self, name, value):
        if name != self._event_name or _field(value, "id", str) != self._job_id:
            raise ProtocolError("unexpected job event")
        event = JobEvent(
            name,
            value["id"],
            _field(value, "phase", str),
            _field(value, "is_done", bool),
            {k: v for k, v in value.items() if k not in {"id", "phase", "is_done"}},
        )
        if event == self._previous:
            return None
        self._previous = event
        return event

    def _terminal(self, event):
        return event.is_done
