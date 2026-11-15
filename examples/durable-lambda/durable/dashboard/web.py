"""A call of the Function URL and its answer."""

from __future__ import annotations

import base64
import json
from dataclasses import dataclass
from typing import Any

from durable.lambda_runtime import HttpStream

SECURITY = {
    "strict-transport-security": "max-age=63072000",
    "x-content-type-options": "nosniff",
    "referrer-policy": "no-referrer",
    "x-frame-options": "DENY",
}


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
    return answer(status, json.dumps({"error": message}), **{"cache-control": "no-store"})
