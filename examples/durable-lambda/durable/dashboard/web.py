"""A call of the Function URL, its answer and the header policies of each answer."""

from __future__ import annotations

import base64
import json
from dataclasses import dataclass
from typing import Any

from durable.lambda_runtime import HttpStream

# -- the header policies of the front function ---------------------------------------

# Each answer gets these headers.
SECURITY = {
    "strict-transport-security": "max-age=63072000",
    "x-content-type-options": "nosniff",
    "referrer-policy": "no-referrer",
    "x-frame-options": "DENY",
}
# The Durable Lambda pages load only their own files.
PAGE_POLICY = (
    "default-src 'self'; img-src 'self' data: blob:; style-src 'self' 'unsafe-inline'; "
    "script-src 'self'; connect-src 'self'; font-src 'self'; object-src 'none'; "
    "base-uri 'none'; form-action 'self'; frame-ancestors 'none'"
)
# The Vis web app starts with inline scripts. It shows text that a session wrote, so
# it loads images and data only from this address: a link in a transcript cannot
# send data to another host.
WEB_POLICY = (
    "default-src 'self'; script-src 'self' 'unsafe-inline' 'wasm-unsafe-eval'; style-src 'self' 'unsafe-inline'; "
    "img-src 'self' data: blob:; font-src 'self' data:; media-src 'self' data: blob:; connect-src 'self'; "
    "worker-src 'self' blob:; frame-src 'self' blob: data:; object-src 'none'; base-uri 'self'; "
    "form-action 'self'; frame-ancestors 'none'"
)
# An answer of the worker can hold text from a session. As a document, it gets no
# network, no cookie and no access to the pages of this address.
RELAY_POLICY = "sandbox allow-downloads; default-src 'none'; style-src 'unsafe-inline'; img-src data: blob:; font-src data:; base-uri 'none'; form-action 'none'; frame-ancestors 'none'"
# The headers of a worker answer that the browser gets. The worker runs the code of a
# session, so it cannot set a cookie, a redirect or a header that changes the browser rules.
RELAYED = {"content-type", "content-encoding", "content-disposition", "content-range", "accept-ranges", "cache-control", "etag", "last-modified", "retry-after", "x-vis-client-lease-unknown"}


@dataclass
class Request:
    """One call of the Function URL."""

    method: str
    path: str
    query: str
    headers: dict[str, str]
    body: bytes

    @classmethod
    def of(cls, event: dict[str, Any]) -> Request:
        """Read a call from the event of the Function URL."""
        body = event.get("body") or ""
        raw = base64.b64decode(body) if event.get("isBase64Encoded") else body.encode()
        return cls(
            method=event["requestContext"]["http"]["method"].upper(),
            path=event.get("rawPath") or "/",
            query=event.get("rawQueryString") or "",
            headers={k.lower(): v for k, v in (event.get("headers") or {}).items()},
            body=raw,
        )

    @property
    def target(self) -> str:
        """The path and the query, as the gateway gets them."""
        return self.path + ("?" + self.query if self.query else "")


def answer(status: int, body: bytes | str, ctype: str = "application/json", **headers: str) -> HttpStream:
    """Build a complete answer with the security headers."""
    data = body.encode() if isinstance(body, str) else body
    return HttpStream(status=status, headers={**SECURITY, "content-type": ctype, **headers}, chunks=[data])


def error(status: int, message: str) -> HttpStream:
    """Build a JSON answer that reports a problem. No cache keeps it."""
    return answer(status, json.dumps({"error": message}), **{"cache-control": "no-store"})
