"""Session API for a Python extension, through the door of its Vis host.

The host serves each request in its own process, without a port, token or child
process. It allows session, turn, Council, subagent and model routes. It refuses a
change to a session that the extension does not own, and it records the extension
as the origin of each session that the extension creates.
"""

from __future__ import annotations

import json
from collections.abc import Callable
from pathlib import PurePosixPath

from ._client import (
    ExecutionLayer,
    JSONValue,
    ProtocolError,
    TransportError,
)
from ._local import _envelope, _LocalJobEvents, _LocalSession, _reply, _request_frame


class HostLayer(ExecutionLayer):
    """Execution layer that sends each SDK request through an extension host door.

    `call` takes one JSON request frame and returns one JSON reply envelope. The
    extension module creates this layer; an extension does not construct it.
    Responses follow the gateway contract. Refusals raise `GatewayError`, and
    a host without a gateway raises `TransportError`. Events use polling.
    """

    def __init__(self, call: Callable[[str], str], *, timeout: float = 30):
        super().__init__(timeout=timeout)
        self._call = call

    def connect(self) -> HostLayer:
        """Return this layer. The host is already running."""
        if self._closed:
            raise TransportError("host layer is closed")
        return self

    def close(self) -> None:
        """Stop new requests through this layer. Sessions continue."""
        self._closed = True
        self._clear_client_extensions()

    def session_options(self, project=".") -> dict[str, JSONValue]:
        """Prepare an app-channel session for an absolute path on the host machine."""
        path = PurePosixPath(project)
        if not path.is_absolute():
            raise ValueError("project must be an absolute path on the host machine")
        return {"root": str(path), "channel": "app"}

    def session(self, sid):
        return _LocalSession(self, sid)

    def _job_events(self, route, job_id, event_name, snapshot, **options):
        return _LocalJobEvents(self, route, job_id, event_name, snapshot, **options)

    def _ensure_client_lease(self, *, pid: int | None = None) -> str:
        raise TransportError(
            "client extensions are not available through the extension host"
        )

    def _open(
        self, method, route, *, query=None, body=None, content=None, timeout=None
    ):
        if self._closed:
            raise TransportError("host layer is closed")
        frame = _request_frame(method, route, query=query, body=body, content=content)
        try:
            text = self._call(json.dumps(frame, allow_nan=False))
        except RuntimeError as error:
            raise TransportError(str(error)) from None
        try:
            reply = json.loads(text)
        except (ValueError, TypeError):
            raise ProtocolError("malformed host reply") from None
        return _reply(*_envelope(reply, "host reply"))
