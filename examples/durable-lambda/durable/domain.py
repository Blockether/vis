"""The rules of the Durable Lambda API: its names, its limits, the statuses of a run and its refusals."""

from __future__ import annotations

import re
from typing import Any

NAME = re.compile(r"[a-z][a-z0-9_]{1,39}")
SESSION = re.compile(r"[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}")
RUN = re.compile(r"[0-9a-f]{32}")
FILE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._ -]{0,79}")
# The runs that work at the same time. The other runs wait.
PARALLEL = 3
# The activities that one run can have.
STEPS = 1000
LIVE = ("queued", "running")
# A waiting run has no process: it needs a file from a person.
OPEN = (*LIVE, "waiting")
# The largest file that a person gives to a run, and the largest attached file that S3 gets.
INPUT_LIMIT, ATTACHED_LIMIT = 300_000, 2_000_000
# An answer of the API: the status, the headers and the body.
Answer = tuple[int, list[list[str]], bytes]


class Refused(Exception):
    """A call that the API does not accept."""

    def __init__(self, status: int, message: str, **detail: Any) -> None:
        super().__init__(message)
        self.status, self.detail = status, detail


class Conflict(Exception):
    """Git could not put the work of a workspace on the new release."""

    def __init__(self, files: list[str]) -> None:
        super().__init__("the work does not apply to the new release")
        self.files = files


def need(pattern: re.Pattern[str], value: Any, what: str) -> str:
    """Return a value that has the form of a pattern, or refuse it. The value becomes a part of an S3 key."""
    if not isinstance(value, str) or not pattern.fullmatch(value):
        raise Refused(400, f"{what} is not valid")
    return value


def relative(path: Any) -> str:
    """Return a file path below a session prefix, or refuse it."""
    parts = str(path).split("/")
    if not path or len(str(path)) > 300 or str(path).startswith("/") or ".." in parts or "" in parts:
        raise Refused(400, "a path is not valid")
    return str(path)
