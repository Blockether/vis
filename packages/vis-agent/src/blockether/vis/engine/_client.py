"""Explicit remote Vis client. No discovery, engine downloads or mutation retries.

Use GatewayClient as a context manager. Closing releases its client lease, not
sessions or the gateway. Raw calls are restricted to canonical SDK routes.
Events are bounded reconnecting SSE iterators; close them when abandoning a run.
Timeouts never imply that a submitted mutation was rolled back.
"""

from __future__ import annotations

import json
import math
import threading
import time
import uuid
from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Any, TypeAlias
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urlencode, urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener

from blockether.vis._contracts import GATEWAY, validate
from blockether.vis.activity import ActivityProjection
from blockether.vis.views import InputView, LiveView, ViewEvent

JSONValue: TypeAlias = (
    str | int | float | bool | None | list["JSONValue"] | dict[str, "JSONValue"]
)
Query: TypeAlias = Mapping[str, str | int | float | bool | None]


class TransportError(RuntimeError):
    """Connection failed or the client has been closed."""


class VisTimeout(TransportError):
    """Deadline expired; the remote operation may still be running."""


class ProtocolError(TransportError):
    """Peer is incompatible or returned malformed protocol data."""


class GatewayError(RuntimeError):
    """HTTP failure; status and code are safe to inspect without logging bodies."""

    def __init__(self, status: int, code: str):
        self.status = status
        self.code = code
        super().__init__(f"Gateway HTTP {status} ({code})")


class _NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def _gateway_error(
    status: int, content: bytes, token: str | None = None
) -> GatewayError:
    """Decode the shared error envelope without exposing its message or credentials."""
    code = "http_error"
    try:
        error = json.loads(content).get("error")
        if isinstance(error, dict) and isinstance(error.get("type"), str):
            code = error["type"]
            if token and token in code:
                code = "http_error"
    except (ValueError, AttributeError):
        pass
    return GatewayError(status, code)


def _duration(value: float) -> float:
    if not math.isfinite(value) or value <= 0:
        raise ValueError("timeout must be positive and finite")
    return value


def _segment(value: Any) -> str:
    value = str(value)
    if not value or value in {".", ".."}:
        raise ValueError("path identifiers must be nonempty, non-dot segments")
    return quote(value, safe="")


def _field(data: Any, name: str, kind: type) -> Any:
    """Validate a consumed field without reflecting a potentially sensitive body."""
    value = data.get(name) if isinstance(data, dict) else None
    if type(value) is not kind or (kind is str and not value):
        raise ProtocolError(f"invalid response field: {name}")
    if kind is int and value < 0:
        raise ProtocolError(f"invalid response field: {name}")
    return value


@dataclass(frozen=True, slots=True)
class Response:
    status: int
    content: bytes
    headers: dict[str, str]

    def json(self) -> Any:
        try:
            return json.loads(self.content)
        except (ValueError, UnicodeError):
            raise ProtocolError("invalid JSON response") from None


@dataclass(frozen=True, slots=True)
class Event:
    """Validated event envelope; only event-specific payload remains dynamic."""

    type: str
    session_id: str
    seq: int | None = None
    cursor: int | None = None
    turn_id: str | None = None
    data: dict[str, Any] = field(default_factory=dict)
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
        if event_type in {"view.open", "view.patch", "view.close"}:
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


class GatewayClient:
    """Connect to an explicit HTTP(S) origin using an optional bearer token.

    TLS certificate verification is enabled. Redirects are refused so credentials
    cannot follow a redirect to another origin. No local credentials are read.
    Instances and their session handles are intended for one calling thread.
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
        self.timeout = _duration(timeout)
        self._lease: str | None = None
        self._closed = False
        self._streams: set[_EventStream] = set()
        self._heartbeat_stop = threading.Event()
        self._heartbeat = None
        self._lease_error = False
        self._lease_seen = 0.0
        self._opener = build_opener(_NoRedirect())

    def _open(
        self, method, route, *, query=None, body=None, content=None, timeout=None
    ):
        if self._closed:
            raise TransportError("client is closed")
        if self._lease_error:
            raise TransportError("client lease keepalive failed; reconnect explicitly")
        headers = {
            GATEWAY["headers"]["protocol"]: str(GATEWAY["protocol"]["version"]),
            GATEWAY["headers"]["minimum_gateway_protocol"]: str(
                GATEWAY["protocol"]["minimum_gateway"]
            ),
            GATEWAY["headers"]["client"]: "vis-python",
            "Accept": "application/json",
        }
        if self._token:
            headers["Authorization"] = "Bearer " + self._token
        if self._lease:
            headers[GATEWAY["headers"]["client_id"]] = self._lease
        if body is not None:
            if content is not None:
                raise ValueError("body and content are mutually exclusive")
            content = json.dumps(body, allow_nan=False).encode()
            headers["Content-Type"] = "application/json"
        url = self._url + route + ("?" + urlencode(query) if query else "")
        try:
            return self._opener.open(
                Request(url, data=content, headers=headers, method=method),
                timeout=_duration(self.timeout if timeout is None else timeout),
            )
        except HTTPError as exc:
            with exc:
                raise _gateway_error(exc.code, exc.read(65536), self._token) from None
        except TimeoutError:
            raise VisTimeout("gateway request timed out") from None
        except (URLError, OSError) as exc:
            if isinstance(getattr(exc, "reason", None), TimeoutError):
                raise VisTimeout("gateway request timed out") from None
            raise TransportError("gateway connection failed") from None

    def _request(
        self,
        method: str,
        route: str,
        *,
        path=None,
        query=None,
        body=None,
        content: bytes | None = None,
        timeout: float | None = None,
    ) -> Response:
        """Call a canonical SDK route template; binary responses remain bytes.

        Method, template and path parameter names are checked before network IO.
        Mutations are sent once, including on timeout or ambiguous disconnect.
        """
        entry = next((r for r in GATEWAY["routes"] if r["path"] == route), None)
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
        if content is not None and (
            request_kind != "binary" or not isinstance(content, bytes)
        ):
            raise ValueError("binary operations require bytes content")
        names = {s[1:] for s in route.split("/") if s.startswith(":")}
        if set(path or {}) != names:
            raise ValueError("path parameters do not match route")
        resolved = "/".join(
            _segment(path[s[1:]]) if s.startswith(":") else s for s in route.split("/")
        )
        try:
            with self._open(
                method.upper(),
                resolved,
                query=query,
                body=body,
                content=content,
                timeout=timeout,
            ) as raw:
                return Response(raw.status, raw.read(), dict(raw.headers))
        except TimeoutError:
            raise VisTimeout("gateway response timed out") from None
        except OSError:
            raise TransportError("gateway response interrupted") from None

    def connect(self) -> GatewayClient:
        if self._lease_error:
            raise TransportError("client lease keepalive failed; create a new client")
        if self._lease:
            return self
        caps = self._request("GET", "/v1/capabilities").json()
        peer = caps.get("protocol", {}) if isinstance(caps, dict) else {}
        if not isinstance(peer, dict):
            raise ProtocolError("malformed gateway handshake")
        version, minimum = peer.get("protocol"), peer.get("min_client")
        ours = GATEWAY["protocol"]
        if (
            type(version) is not int
            or type(minimum) is not int
            or version < ours["minimum_gateway"]
            or minimum > ours["version"]
        ):
            raise ProtocolError("incompatible gateway protocol")
        lease = self._request("POST", "/v1/clients", body={"kind": "python-sdk"}).json()
        if not isinstance(lease, dict) or not isinstance(lease.get("client_id"), str):
            raise ProtocolError("missing client lease")
        self._lease = lease["client_id"]
        self._lease_seen = time.monotonic()
        self._heartbeat = threading.Thread(
            target=self._keepalive, name="vis-sdk-lease", daemon=True
        )
        self._heartbeat.start()
        return self

    def _keepalive(self):
        policy = GATEWAY["client_lease"]
        interval = policy["keepalive_ms"] / 1000
        while not self._heartbeat_stop.wait(interval):
            try:
                if time.monotonic() - self._lease_seen >= policy["ttl_ms"] / 1000:
                    raise TransportError("client lease expired")
                with self._open(
                    "GET",
                    policy["keepalive_route"],
                    timeout=min(self.timeout, policy["keepalive_timeout_ms"] / 1000),
                ) as raw:
                    raw.read(65536)
                self._lease_seen = time.monotonic()
            except (TransportError, GatewayError, OSError):
                self._lease_error = True
                return

    def close(self) -> None:
        if self._closed:
            return
        self._heartbeat_stop.set()
        if self._heartbeat is not None:
            self._heartbeat.join()
        self._lease_error = False
        try:
            for stream in tuple(self._streams):
                stream.close()
            if self._lease:
                self._request("DELETE", "/v1/clients/:cid", path={"cid": self._lease})
        finally:
            self._closed = True
            self._lease = None
            self._token = None

    def __enter__(self) -> GatewayClient:
        return self.connect()

    def __exit__(self, *args):
        self.close()

    def session(self, sid: str) -> Session:
        return Session(self, sid)

    def create_session(self, *, timeout=None, **options) -> Session:
        data = self._request(
            "POST", "/v1/sessions", body=options, timeout=timeout
        ).json()
        return self.session(_field(data, "id", str))

    def list_sessions(self, **query):
        """Return one page including next_cursor; no hidden full-fleet fetch."""
        return self._request("GET", "/v1/sessions", query=query).json()

    def get_capabilities(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
        """GET /v1/capabilities — json response."""
        response = self._request(
            "GET", "/v1/capabilities", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_devices(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
        """POST /v1/devices — json response."""
        response = self._request(
            "POST", "/v1/devices", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_device(
        self, token: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
        """POST /v1/devices/actions/test — json response."""
        response = self._request(
            "POST", "/v1/devices/actions/test", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_fs(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
        """GET /v1/fs — json response."""
        response = self._request("GET", "/v1/fs", path={}, query=query, timeout=timeout)
        return response.json()

    def post_fs_mkdir(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """POST /v1/mcp/servers — json response."""
        response = self._request(
            "POST", "/v1/mcp/servers", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_mcp_server(
        self, name: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/models — json response."""
        response = self._request(
            "GET", "/v1/models", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_projects(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/projects/overview — json response."""
        response = self._request(
            "GET", "/v1/projects/overview", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_provider_presets(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/providers/:provider-id/limits — json response."""
        response = self._request(
            "GET",
            "/v1/providers/:provider-id/limits",
            path={"provider-id": provider_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_provider_logout(
        self,
        provider_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """PATCH /v1/router — json response."""
        response = self._request(
            "PATCH", "/v1/router", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_sessions(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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

    def get_session_artifacts(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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

    def post_session_iteration_attachments(
        self,
        sid: str,
        iid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
        """POST /v1/sessions/:sid/iterations/:iid/attachments — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/iterations/:iid/attachments",
            path={"sid": sid, "iid": iid},
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

    def get_session_model(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/sessions/:sid/slashes — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/slashes",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_speech(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/sessions/:sid/views/live — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/views/live",
            path={"sid": sid},
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
    ) -> JSONValue:
        """GET /v1/sessions/:sid/views/live/:view-id/log/:node-id — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/views/live/:view-id/log/:node-id",
            path={"sid": sid, "view-id": view_id, "node-id": node_id},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_voice(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/sessions/:sid/workspace — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/workspace",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_session_workspace_drafts(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
        """GET /v1/sessions/:sid/workspace/drafts — json response."""
        response = self._request(
            "GET",
            "/v1/sessions/:sid/workspace/drafts",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def post_session_workspace_drafts(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
        """POST /v1/sessions/:sid/workspace/drafts — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/workspace/drafts",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def delete_session_workspace_draft(
        self,
        sid: str,
        workspace_id: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
        """DELETE /v1/sessions/:sid/workspace/drafts/:workspace-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/sessions/:sid/workspace/drafts/:workspace-id",
            path={"sid": sid, "workspace-id": workspace_id},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def post_session_workspace_resume(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
        """POST /v1/sessions/:sid/workspace/resume — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/workspace/resume",
            path={"sid": sid},
            query=query,
            timeout=timeout,
            body=body,
        )
        return response.json()

    def patch_session_workspace_root(
        self,
        sid: str,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
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

    def post_session_workspace_stash(
        self, sid: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
        """POST /v1/sessions/:sid/workspace/stash — json response."""
        response = self._request(
            "POST",
            "/v1/sessions/:sid/workspace/stash",
            path={"sid": sid},
            query=query,
            timeout=timeout,
        )
        return response.json()

    def get_sessions_search(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
        """GET /v1/sessions/actions/search — json response."""
        response = self._request(
            "GET", "/v1/sessions/actions/search", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_settings(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
        """POST /v1/settings — json response."""
        response = self._request(
            "POST", "/v1/settings", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def get_setting(
        self, id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
        """GET /v1/settings/:id — json response."""
        response = self._request(
            "GET", "/v1/settings/:id", path={"id": id}, query=query, timeout=timeout
        )
        return response.json()

    def post_speech(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        body: JSONValue = None,
    ) -> JSONValue:
        """POST /v1/speech — json response."""
        response = self._request(
            "POST", "/v1/speech", path={}, query=query, timeout=timeout, body=body
        )
        return response.json()

    def delete_speech_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/speech/model — json response."""
        response = self._request(
            "GET", "/v1/speech/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_speech_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
        """POST /v1/speech/model — json response."""
        response = self._request(
            "POST", "/v1/speech/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def get_speech_voices(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """DELETE /v1/speech/voices/:voice-id — json response."""
        response = self._request(
            "DELETE",
            "/v1/speech/voices/:voice-id",
            path={"voice-id": voice_id},
            query=query,
            timeout=timeout,
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

    def post_voice(
        self,
        *,
        query: Query | None = None,
        timeout: float | None = None,
        content: bytes | None,
    ) -> JSONValue:
        """POST /v1/voice — json response."""
        response = self._request(
            "POST", "/v1/voice", path={}, query=query, timeout=timeout, content=content
        )
        return response.json()

    def delete_voice_job(
        self, job_id: str, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
    ) -> JSONValue:
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
    ) -> JSONValue:
        """GET /v1/voice/model — json response."""
        response = self._request(
            "GET", "/v1/voice/model", path={}, query=query, timeout=timeout
        )
        return response.json()

    def post_voice_model(
        self, *, query: Query | None = None, timeout: float | None = None
    ) -> JSONValue:
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
            GATEWAY["events"]["jobs"]["synthesize"],
            lambda: self.get_speech_job(job_id),
            **options,
        )

    def voice_events(self, job_id: str, **options) -> JobEvents:
        """Transcription snapshots over SSE (HTTP) or bounded polling (local)."""
        return self._job_events(
            self._job_route("voice", job_id),
            job_id,
            GATEWAY["events"]["jobs"]["transcribe"],
            lambda: self.get_voice_job(job_id),
            **options,
        )

    def session_speech_events(self, sid: str, job_id: str, **options) -> JobEvents:
        """Session-scoped synthesis snapshots; close never cancels the job."""
        return self._job_events(
            self._job_route("speech", job_id, sid),
            job_id,
            GATEWAY["events"]["jobs"]["synthesize"],
            lambda: self.get_session_speech_job(sid, job_id),
            **options,
        )

    def session_voice_events(self, sid: str, job_id: str, **options) -> JobEvents:
        """Session-scoped transcription snapshots; close never cancels the job."""
        return self._job_events(
            self._job_route("voice", job_id, sid),
            job_id,
            GATEWAY["events"]["jobs"]["transcribe"],
            lambda: self.get_session_voice_job(sid, job_id),
            **options,
        )

    @staticmethod
    def _job_route(kind, job_id, sid=None):
        prefix = f"/v1/sessions/{_segment(sid)}" if sid is not None else "/v1"
        return f"{prefix}/{kind}/jobs/{_segment(job_id)}/events"


@dataclass(frozen=True, slots=True)
class Session:
    client: GatewayClient
    id: str

    def _call(self, method, suffix="", **kwargs):
        path = {"sid": self.id, **kwargs.pop("path", {})}
        response = self.client._request(
            method, "/v1/sessions/:sid" + suffix, path=path, **kwargs
        )
        return response.json() if response.content else None

    def council(self, *, group_id: str | None = None):
        """Bind a Council handle to this activation, or a read-only handle when idle."""
        from ._council import Council

        binding = self._call(
            "GET",
            "/council",
            query={"group_id": group_id} if group_id is not None else {},
        )
        validate("council", "binding", binding)
        return Council(self, binding["default_group_id"], binding["activation_id"])

    def read(self):
        return self._call("GET")

    def update(self, **fields):
        return self._call("PATCH", body=fields)

    def delete(self):
        return self._call("DELETE")

    def turns(self, **query):
        """Read turn history, or the queued subset with status='queued'."""
        return _field(self._call("GET", "/turns", query=query), "turns", list)

    def artifacts(self, **query):
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

    def upload(self, content: bytes, *, filename: str, media_type: str):
        """Upload bytes; pass the returned upload_id in send(attachments=[...])."""
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

    def send(
        self, request: str, *, idempotency_key: str | None = None, **options
    ) -> Turn:
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

    def input_views(self) -> list[InputView]:
        data = _field(self._call("GET", "/views/input"), "requests", list)
        try:
            return [InputView.from_wire(item) for item in data]
        except ValueError:
            raise ProtocolError("invalid input View response") from None

    def live_views(self) -> list[LiveView]:
        data = _field(self._call("GET", "/views/live"), "views", list)
        try:
            return [LiveView.from_wire(item) for item in data]
        except ValueError:
            raise ProtocolError("invalid live View response") from None

    def view_action(self, view_id: str, action: str, **values):
        body = {**values, "action": action}
        validate("view", "operator_action", body)
        return self._call(
            "POST",
            "/views/:view-id/actions",
            path={"view-id": view_id},
            body=body,
        )

    def answer(self, view_id, values):
        return self.view_action(view_id, "submit", values=values)

    def events(self, **options) -> Events:
        return Events(self, **options)


@dataclass(frozen=True, slots=True)
class Turn:
    session: Session
    id: str
    cursor: int = 0

    def read(self, *, timeout=None):
        return self.session._call(
            "GET", "/turns/:tid", path={"tid": self.id}, timeout=timeout
        )

    def cancel(self):
        return self.session._call("POST", "/turns/:tid/cancel", path={"tid": self.id})

    def wait(self, *, timeout: float = 300):
        """Wait for this turn only; deadline expiration does not cancel it."""
        end = time.monotonic() + _duration(timeout)
        while True:
            remaining = end - time.monotonic()
            if remaining <= 0:
                raise VisTimeout("turn wait timed out; turn was not cancelled")
            data = self.read(timeout=remaining)
            if _field(data, "status", str) in {
                "completed",
                "failed",
                "cancelled",
                "suspended",
                "error",
            }:
                return data
            time.sleep(min(0.1, max(0, end - time.monotonic())))


class _EventStream:
    """One bounded SSE reader; subclasses own event identity and replay policy."""

    def __init__(self, client, *, reconnects=3, retry_delay=0.2):
        if (
            type(reconnects) is not int
            or reconnects < 0
            or not math.isfinite(retry_delay)
            or retry_delay < 0
        ):
            raise ValueError("invalid replay options")
        self.client = client
        self.reconnects, self.retry_delay = reconnects, retry_delay
        self._raw = None
        self._closed = False
        self._iterator = self._iterate()
        client._streams.add(self)

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    def __iter__(self):
        return self

    def __next__(self):
        if self._closed:
            raise StopIteration
        return next(self._iterator)

    def close(self):
        self._closed = True
        if self._raw is not None:
            self._raw.close()
        # A pipe failure can close its owning client from inside this generator.
        if not self._iterator.gi_running:
            self._iterator.close()
        self.client._streams.discard(self)

    def _iterate(self):
        try:
            for attempt in range(self.reconnects + 1):
                if attempt:
                    time.sleep(self.retry_delay)
                try:
                    route, query = self._endpoint()
                    with self.client._open("GET", route, query=query) as raw:
                        self._raw = raw
                        if raw.headers.get_content_type() != "text/event-stream":
                            raise ProtocolError("expected event stream")
                        data, size, event_name = [], 0, None
                        for line in iter(lambda: raw.readline(1048577), b""):
                            size += len(line)
                            if size > 1048576:
                                raise ProtocolError("event frame exceeds 1 MiB")
                            if line in {b"\n", b"\r\n", b"\r"}:
                                if data:
                                    try:
                                        value = json.loads(b"\n".join(data))
                                    except (ValueError, UnicodeError):
                                        raise ProtocolError(
                                            "malformed event JSON"
                                        ) from None
                                    event = self._accept(event_name, value)
                                    if event is not None:
                                        yield event
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
                except (OSError, TransportError):
                    if self.client._lease_error:
                        raise TransportError(
                            "client lease keepalive failed; reconnect explicitly"
                        ) from None
                finally:
                    self._raw = None
            raise TransportError("event reconnect budget exhausted")
        finally:
            self.client._streams.discard(self)

    def _terminal(self, event):
        return False


class Events(_EventStream):
    """Session SSE with cursor resume, duplicate suppression and bounded reconnects.

    subscription.ready resets the cursor, including after a daemon restart.
    The idle timeout is the client's timeout. Closing never cancels a turn.
    """

    def __init__(self, session: Session, *, cursor=0, **options):
        if type(cursor) is not int or cursor < 0:
            raise ValueError("invalid replay cursor")
        self.session, self.cursor = session, cursor
        super().__init__(session.client, **options)

    def _endpoint(self):
        return "/v1/events", {"sids": f"{self.session.id}:{self.cursor}"}

    def _accept(self, name, value):
        event = Event.from_wire(value)
        if event.session_id != self.session.id:
            raise ProtocolError("unexpected event session")
        if name is not None and name != event.type:
            raise ProtocolError("event name disagrees with payload")
        seq = event.cursor if event.type == "subscription.ready" else event.seq
        if event.type == "subscription.ready" or seq > self.cursor:
            self.cursor = seq
            return event
        return None


@dataclass(frozen=True, slots=True)
class JobEvent:
    """Current speech job snapshot, not a session event or a replay cursor."""

    type: str
    id: str
    phase: str
    is_done: bool
    data: dict[str, Any] = field(default_factory=dict)


class JobEvents(_EventStream):
    """Snapshot-first job SSE; reconnect rereads state, terminal is_done ends it.

    Job streams have no cursor and make no exactly-once claim. Identical snapshots
    are suppressed within this iterator. Remaining fields retain the canonical JSON.
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
