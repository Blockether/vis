"""A client of the playground API, for the code that runs next to the gateway.

The broker of the worker serves the API on the loopback interface. Its address is in
`PLAYGROUND_BROKER`. The workflow runs use this client. It needs only the standard library.
"""

from __future__ import annotations

import json
import os
import urllib.error
import urllib.request
from typing import Any


class PlaygroundError(RuntimeError):
    """The playground API refused a call."""


class Playground:
    def __init__(self, base: str | None = None) -> None:
        self.base = (base or os.environ["PLAYGROUND_BROKER"]).rstrip("/") + "/play/api/"

    def call(self, method: str, path: str, body: Any = None, timeout: float = 120) -> Any:
        """Call the API and return the JSON answer."""
        data = None if body is None else json.dumps(body).encode()
        request = urllib.request.Request(self.base + path, data, {"content-type": "application/json"}, method=method)
        try:
            with urllib.request.urlopen(request, timeout=timeout) as answer:
                return json.loads(answer.read() or b"null")
        except urllib.error.HTTPError as failure:
            try:
                message = json.loads(failure.read())["error"]
            except (ValueError, KeyError, TypeError):
                message = f"HTTP {failure.code}"
            raise PlaygroundError(message) from None
