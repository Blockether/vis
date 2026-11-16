"""The rules of the Software factory API: the form of its names, the statuses of a run and its refusals.

The limits of the API are settings: see `settings.py`.
"""

from __future__ import annotations

import re
from typing import Any

# The form of each name that becomes a part of an S3 key: the name of a workflow or
# of a gate, the id of a session, the id of a run and the name of a file.
NAME = re.compile(r"[a-z][a-z0-9_]{1,39}")
SESSION = re.compile(r"[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}")
RUN = re.compile(r"[0-9a-f]{32}")
FILE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._ -]{0,79}")
# The statuses of a run that has, or waits for, a process.
LIVE = ("queued", "running")
# A waiting run has no process: it waits at a gate for the decision of a person.
OPEN = (*LIVE, "waiting")
# An answer of the API: the status, the headers and the body.
Answer = tuple[int, list[list[str]], bytes]


class Refused(Exception):
    """A call that the API does not accept."""

    def __init__(self, status: int, message: str, **detail: Any) -> None:
        """Keep the HTTP status of the answer and the fields that its JSON body gets."""
        super().__init__(message)
        self.status, self.detail = status, detail


class Elsewhere(Exception):
    """A call for a run or a session that the worker of another shard owns."""

    def __init__(self, shard: str) -> None:
        """Keep the shard that answers the call."""
        super().__init__(f"the shard {shard} owns it")
        self.shard = shard


class Conflict(Exception):
    """Git could not put the work of a workspace on the new release."""

    def __init__(self, files: list[str]) -> None:
        """Keep the files that Git could not merge."""
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
