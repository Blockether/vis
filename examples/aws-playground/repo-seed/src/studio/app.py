"""The web application of the studio: one WSGI application, no web framework.

`application` is the entry of the project. A WSGI server calls it for each request.
"""

from __future__ import annotations

import json
import re
from collections.abc import Callable, Iterable
from pathlib import Path
from typing import Any

from studio import songs

PUBLIC = Path(__file__).parent / "public"
TYPES = {
    ".html": "text/html; charset=utf-8",
    ".css": "text/css; charset=utf-8",
    ".js": "text/javascript; charset=utf-8",
}
Answer = tuple[str, str, bytes, dict[str, str]]


def data(value: Any, status: str = "200 OK") -> Answer:
    """Return a JSON answer."""
    return status, "application/json", json.dumps(value).encode(), {}


def public(name: str) -> Answer | None:
    """Return a file of the directory `public`, or None."""
    file = PUBLIC / name
    if not re.fullmatch(r"[A-Za-z0-9_-]+\.[a-z]+", name) or not file.is_file():
        return None
    return "200 OK", TYPES.get(file.suffix, "application/octet-stream"), file.read_bytes(), {}


def route(method: str, path: str) -> Answer:
    """Return the answer to one request."""
    if method != "GET":
        return data({"error": "GET only"}, "405 Method Not Allowed")
    if path == "/":
        return public("index.html") or data({"error": "not found"}, "404 Not Found")
    if path == "/api/songs":
        return data([songs.summary(item) for item in songs.CATALOG])
    if (found := re.fullmatch(r"/api/songs/([a-z0-9-]+)", path)) and (item := songs.song(found[1])):
        return data({**songs.summary(item), "tracks": songs.tracks(item)})
    if (found := re.fullmatch(r"/songs/([a-z0-9-]+)\.mid", path)) and (item := songs.song(found[1])):
        return (
            "200 OK",
            "audio/midi",
            songs.midi_file(item),
            {"Content-Disposition": f'attachment; filename="{item.id}.mid"'},
        )
    if (found := re.fullmatch(r"/public/(.+)", path)) and (answer := public(found[1])):
        return answer
    return data({"error": "not found"}, "404 Not Found")


def application(environ: dict[str, Any], start_response: Callable[..., Any]) -> Iterable[bytes]:
    """Answer one WSGI request."""
    status, kind, body, headers = route(environ["REQUEST_METHOD"], environ.get("PATH_INFO") or "/")
    start_response(
        status,
        [("Content-Type", kind), ("Content-Length", str(len(body))), ("Cache-Control", "no-store"), *headers.items()],
    )
    return [body]
