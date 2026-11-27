"""The factory API client for workflow processes."""

from __future__ import annotations

import json
import urllib.error
import urllib.request
from typing import Any

from factory.settings import API_S, BASE, extension_connection


class DurableError(RuntimeError):
    """The Software factory API refused a call."""


class Durable:
    """The authenticated factory API at the local address."""

    def __init__(self, base: str | None = None) -> None:
        """Use the given address or the configured local address."""
        local_url, self.token = extension_connection()
        self.base = (base or local_url).rstrip("/") + f"{BASE}/api/"

    def call(self, method: str, path: str, body: Any = None, timeout: float = API_S) -> Any:
        """Call the API and return the JSON answer."""
        data = None if body is None else json.dumps(body).encode()
        request = urllib.request.Request(self.base + path, data, {"content-type": "application/json", "Authorization": f"Bearer {self.token}"}, method=method)
        try:
            with urllib.request.urlopen(request, timeout=timeout) as answer:
                return json.loads(answer.read() or b"null")
        except urllib.error.HTTPError as failure:
            try:
                message = json.loads(failure.read())["error"]
            except (ValueError, KeyError, TypeError):
                message = f"HTTP {failure.code}"
            raise DurableError(message) from None

    def event(self, run_id: str, kind: str, **fields: Any) -> Any:
        """Report what a run process did. The API records it in the run."""
        return self.call("POST", f"runs/{run_id}/events", {"type": kind, **fields})
