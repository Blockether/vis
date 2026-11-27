"""Suggestion records, their decisions and their discussion sessions."""

from __future__ import annotations

import hashlib
import threading
import time
from collections.abc import Callable
from typing import Any, get_args

from vis_optmem.shared import Absent, Equals, Table

from factory import settings
from factory.models import Effort, Kind
from factory.records import ID, SESSION, Document, Records, Refused, need, new_id, pack, unpack

KINDS, EFFORTS = get_args(Kind), get_args(Effort)


def text(value: Any, name: str, limit: int, *, empty: bool = False) -> str:
    """Return a valid field of text."""
    if not isinstance(value, str) or (not value.strip() and not empty):
        raise Refused(400, f"{name} must be text")
    if len(value) > limit:
        raise Refused(413, f"{name} is too long")
    return value.strip()


def transition(status: str, doc: Document, comment: str = "") -> tuple[str, Document]:
    """Make the event for a change to a suggestion."""
    return f"suggestion.{status}", {"suggestion": doc["id"], "title": doc["title"], "comment": comment}


class Suggestions:
    """Keep suggestions on the same table as their events."""

    def __init__(self, table: Table, discuss: Callable[[Document], str]) -> None:
        """Use the given function to open one durable discussion session."""
        self.records, self.table, self.open_talk = Records(table), table, discuss
        self.talk_lock = threading.Lock()

    def listing(self, status: str = "") -> list[Document]:
        """List suggestions, newest first, with an optional status filter."""
        return [doc for doc in self.records.listing("suggestion/") if not status or doc["status"] == status]

    def get(self, suggestion_id: str) -> Document:
        """Return the suggestion with this id."""
        return self.records.get(f"suggestion/{need(ID, suggestion_id, 'the suggestion id')}", "suggestion")

    def create(self, data: Document) -> Document:
        """Save a suggestion and its event. A run can give the same title only once."""
        title = text(data.get("title"), "the title", 200)
        body = text(data.get("body"), "the body", settings.WORKFLOW_LIMIT)
        kind, effort = data.get("kind"), data.get("effort")
        if kind not in KINDS or effort not in EFFORTS:
            raise Refused(400, "the suggestion kind or effort is not valid")
        run, session = data.get("run"), data.get("session")
        if run is not None:
            need(ID, run, "the run id")
        if session is not None:
            need(SESSION, session, "the session id")
        mark = f"suggested/{run}/{hashlib.sha256(title.encode()).hexdigest()}" if run else None
        while True:
            if mark and (item := self.table.get(mark)):
                return self.get(unpack(item)["id"])
            suggestion_id = new_id()
            key = f"suggestion/{suggestion_id}"
            doc = {
                "id": suggestion_id,
                "title": title,
                "body": body,
                "kind": kind,
                "effort": effort,
                "status": "open",
                "session": session,
                "run": run,
                "talk": None,
                "comments": [],
                "created": time.time(),
                "decided": None,
            }
            puts, checks = {key: pack(doc)}, [Absent(key)]
            if mark:
                puts[mark] = pack({"id": suggestion_id})
                checks.append(Absent(mark))
            if self.records.write(puts, checks, transition("created", doc)):
                return doc

    def decide(self, suggestion_id: str, status: str, comment: Any) -> Document:
        """Accept or decline an open suggestion and record the event."""
        line = text(comment, "the comment", settings.COMMENT_LIMIT, empty=True)

        def change(doc: Document) -> tuple[str, Document]:
            """Make a decision only once."""
            if doc["status"] != "open":
                raise Refused(409, "the suggestion was already decided")
            now = time.time()
            doc.update(status=status, decided=now)
            if line:
                doc["comments"].append({"at": now, "author": "person", "text": line})
            return transition(status, doc, line)

        return self.records.update(f"suggestion/{need(ID, suggestion_id, 'the suggestion id')}", "suggestion", change)

    def comment(self, suggestion_id: str, data: Document) -> Document:
        """Add a comment to a suggestion."""
        line = text(data.get("text"), "the comment", settings.COMMENT_LIMIT)
        author = data.get("author", "person")
        if author not in ("person", "factory"):
            raise Refused(400, "the author must be person or factory")

        def change(doc: Document) -> None:
            """Append the comment to the most recent record."""
            if author != "factory" or not any(comment["author"] == author and comment["text"] == line for comment in doc["comments"]):
                doc["comments"].append({"at": time.time(), "author": author, "text": line})

        return self.records.update(f"suggestion/{need(ID, suggestion_id, 'the suggestion id')}", "suggestion", change)

    def discuss(self, suggestion_id: str) -> str:
        """Return the one session that holds the discussion of this suggestion."""
        key = f"suggestion/{need(ID, suggestion_id, 'the suggestion id')}"
        with self.talk_lock:
            while True:
                item = self.table.get(key)
                if item is None:
                    raise Refused(404, "no such suggestion")
                doc = unpack(item)
                if doc["talk"]:
                    return doc["talk"]
                if item.get("talk_claim") == 1:
                    break
                version = int(item["version"])
                if self.records.write({key: {**item, "version": version + 1, "talk_claim": 1}}, [Equals(key, "version", version)]):
                    break
            # A failed gateway call keeps the claim. The next call uses the same
            # gateway identity to finish it without making a second session.
            session = self.open_talk(doc)

            def change(latest: Document) -> None:
                """Record the session, unless another call already did so."""
                if not latest["talk"]:
                    latest["talk"] = session

            return self.records.update(key, "suggestion", change)["talk"]
