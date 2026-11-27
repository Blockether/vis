"""Versioned JSON records in the factory table."""

from __future__ import annotations

import json
import re
import secrets
import time
from collections.abc import Callable, Iterable, Mapping
from typing import Any

from vis_optmem.shared import Absent, Condition, Equals, Item, Table

NAME = re.compile(r"[a-z][a-z0-9_]{0,39}")
SESSION = re.compile(r"[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}")
ID = re.compile(r"[0-9a-f]{20}")
FILE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._ -]{0,79}")
Document = dict[str, Any]


class Refused(Exception):
    """A call the factory does not accept."""

    def __init__(self, status: int, message: str) -> None:
        """Keep the status and the error text for the caller."""
        super().__init__(message)
        self.status = status


def need(pattern: re.Pattern[str], value: Any, what: str) -> str:
    """Return a valid name, or refuse it."""
    if not isinstance(value, str) or not pattern.fullmatch(value):
        raise Refused(400, f"{what} is not valid")
    return value


def new_id(now: float | None = None) -> str:
    """Make a time-sortable id."""
    return f"{int((time.time() if now is None else now) * 1000):012x}{secrets.token_hex(4)}"


def pack(doc: Document, version: int = 1) -> Item:
    """Encode one record as flat table fields."""
    return {"doc": json.dumps(doc, ensure_ascii=False, separators=(",", ":")), "version": version}


def unpack(item: Item) -> Document:
    """Read the document of a record."""
    return json.loads(item["doc"])


class Records:
    """Read and write the records of one factory."""

    def __init__(self, table: Table) -> None:
        """Keep the table for all kinds of records."""
        self.table = table

    def get(self, key: str, what: str) -> Document:
        """Return one document, or refuse a key that does not exist."""
        item = self.table.get(key)
        if item is None:
            raise Refused(404, f"no such {what}")
        return unpack(item)

    def listing(self, prefix: str) -> list[Document]:
        """Return the documents of a range, newest first."""
        return [unpack(item) for _, item in reversed(self.table.query(prefix))]

    def write(self, puts: Mapping[str, Item], conditions: Iterable[Condition], event: tuple[str, Document] | None = None, *, at: float | None = None) -> bool:
        """Write a change and its event in one transaction."""
        if event is None:
            return self.table.transact(puts, conditions=conditions)
        return self.emit(*event, puts=puts, conditions=conditions, at=at) is not None

    def emit(self, kind: str, data: Document, *, puts: Mapping[str, Item] | None = None, conditions: Iterable[Condition] = (), at: float | None = None) -> str | None:
        """Write an event and any related changes. Return its id, or None on conflict."""
        # The head makes the event key follow every event that is already visible.
        # A scheduler cursor can then never pass an event that is not written yet.
        head_key = "cursor/event_head"
        head = self.table.get(head_key)
        now = time.time() if at is None else at
        event_id = new_id(now)
        if head and event_id <= unpack(head)["id"]:
            last = unpack(head)["id"]
            event_id = f"{int(last[:12], 16) + 1:012x}{secrets.token_hex(4)}"
        version = int(head["version"]) if head else 0
        check = Equals(head_key, "version", version) if head else Absent(head_key)
        events = {head_key: pack({"id": event_id}, version + 1), f"event/{event_id}": pack({"id": event_id, "kind": kind, "at": now, "data": data})}
        return event_id if self.table.transact({**(puts or {}), **events}, conditions=(*conditions, check)) else None

    def update(self, key: str, what: str, change: Callable[[Document], tuple[str, Document] | None]) -> Document:
        """Apply a change to the newest version. `change` can also return an event."""
        while True:
            item = self.table.get(key)
            if item is None:
                raise Refused(404, f"no such {what}")
            doc, version = unpack(item), int(item["version"])
            event = change(doc)
            if self.write({key: {**item, **pack(doc, version + 1)}}, [Equals(key, "version", version)], event):
                return doc
