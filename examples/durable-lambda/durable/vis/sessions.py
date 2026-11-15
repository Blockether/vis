"""Where each Vis session works: its work directory, from the first call to the delete.

The gateway knows the directory of a session. The workspaces own the directory and
its copy in S3 (see `worktree.py`).
"""

from __future__ import annotations

import json
import os
import time
from typing import Any

from durable import worktree
from durable.domain import SESSION, Conflict, Refused, need
from durable.helpers import log
from durable.settings import HOME_S, PLACES, WORK
from durable.vis.gateway import Gateway
from durable.workflows.runs import Runs
from durable.worktree import FAILURES, Workspaces, name_of


class Sessions:
    """The work directories of the sessions of this worker."""

    def __init__(self, release: str, workspaces: Workspaces, runs: Runs) -> None:
        """Start without a gateway. `release` is the release of this worker, for a session that starts before a newer release."""
        self.release, self.workspaces, self.runs = release, workspaces, runs
        # The worker sets these after the gateway starts: the gateway and the group of the sessions of a person.
        self.engine: Gateway | None = None
        self.people = ""
        # The directory of each session that reported a change, and the time of the answer of the gateway.
        self.homes: dict[str, tuple[float, str]] = {}

    def home(self, session: Any, is_fresh: bool = True) -> str:
        """Return the work directory of a session, or refuse a session that the gateway does not have."""
        need(SESSION, session, "the session id")
        at, root = self.homes.get(session, (0.0, ""))
        if is_fresh or time.time() - at > HOME_S:
            row = self.engine.soul(session) if self.engine else None
            root = ((row or {}).get("workspace") or {}).get("root") or ""
            self.homes[session] = (time.time(), root)
        if not root:
            raise Refused(404, "no such session")
        return root

    def touch(self, session: str, is_fresh: bool = True) -> None:
        """Make the work directory of a session ready, and record that the session changes it."""
        try:
            root = self.home(session, is_fresh)
            if not self.workspaces.ensure(root):
                log("the session has no work directory", session, root)
            self.workspaces.mark(root)
        except (Refused, *FAILURES) as failure:
            log("no look at the work directory", session, repr(failure))

    def places(self) -> list[tuple[str, str]]:
        """Return the id and the title of each session that a run can work in: a session with a turn in a clone of the repository."""
        rows = self.engine.sessions() if self.engine else []
        found = [row for row in rows if os.path.dirname(os.path.realpath((row.get("workspace") or {}).get("root") or "/")) == os.path.realpath(WORK)]
        return [(row["id"], f"{row.get('title') or 'Untitled session'} · {row['id'][:8]}") for row in found[:PLACES]]

    def place(self, body: bytes) -> tuple[bytes, str | None]:
        """Give a new session its own work directory. Return the body for the gateway, and the directory.

        Each session of a person works in its own clone of the current release, so two
        people do not change the same files. The session joins the group of these
        sessions when the call names no group: the group keeps it in the one project.
        """
        try:
            asked = json.loads(body) if body else {}
        except ValueError:
            return body, None
        if not isinstance(asked, dict) or asked.get("workspace_id"):
            return body, None
        root = self.workspaces.create(worktree.current() or self.release)
        asked.update(root=root, group_id=asked.get("group_id") or self.people or None)
        return json.dumps(asked).encode(), root

    def placed(self, root: str, status: int, answer: bytes) -> None:
        """Keep the directory that a new session got, or remove it when no session works in it.

        A call with the key of an earlier call returns the earlier session, with its own directory.
        """
        try:
            made = json.loads(answer) if status in (200, 201) else {}
            session = made.get("id") if isinstance(made, dict) else None
            if session and os.path.realpath(self.home(session)) == os.path.realpath(root):
                return self.workspaces.own(root, session)
        except (ValueError, Refused):
            pass
        self.workspaces.remove(name_of(root) or "")

    def leaving(self, session: str) -> list[str]:
        """Return the work directory of a session that a call deletes now."""
        try:
            return [self.home(session)]
        except Refused:
            return []

    def reap(self, roots: list[str] | None) -> None:
        """Remove the work directory and the S3 copy of each session that the gateway deleted.

        `roots` has the directory of the one session of the call. Without it, the call
        could delete many sessions: then each workspace whose session is gone goes. A
        directory stays while another session or a run that did not end works in it.
        """
        if not self.engine:
            return
        used = self.engine.roots() | self.runs.roots()
        if roots is None:
            names = [name for name in self.workspaces.names() if (owner := self.workspaces.owner(name)) and self.engine.is_gone(owner)]
        else:
            names = [name for name in map(name_of, roots) if name]
        for name in names:
            if os.path.realpath(f"{WORK}/{name}") not in used:
                self.workspaces.remove(name)

    def directory(self, session: str) -> str:
        """Return the work directory of a session as a directory that exists."""
        root = self.home(session)
        try:
            is_ready = self.workspaces.ensure(root) and os.path.isfile(f"{root}/.git/durable-base")
        except FAILURES as failure:
            log("no work directory", session, repr(failure))
            is_ready = False
        if not is_ready:
            raise Refused(409, "the session has no work directory of Durable Lambda")
        return root

    def workspace(self, session: str, **_: Any) -> dict[str, Any]:
        """Return the release of the work directory of a session, its commits and its changed files."""
        return self.workspaces.status(self.directory(session))

    def resync(self, session: str, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Put the work of a session on the current release of the repository.

        A person cannot do this while a turn or a run changes the files. The tool of the
        extension can: it is a step of that turn.
        """
        root = self.directory(session)
        if not name_of(root):
            raise Refused(400, "the project directory takes each release when a worker starts")
        if not payload.get("is_own_turn") and ((self.engine and self.engine.busy(session)) or os.path.realpath(root) in self.runs.roots()):
            raise Refused(409, "a turn or a run works in the session: try again when it ends")
        try:
            return self.workspaces.resync(root)
        except Conflict as conflict:
            raise Refused(409, "the work does not apply to the new release: the directory stays as it was", files=conflict.files) from None
        except FAILURES as failure:
            log("resync failed", session, repr(failure))
            raise Refused(500, "Git could not put the work on the new release") from None

    def cancel(self, session: str) -> None:
        """Stop the turn of a session."""
        if self.engine:
            self.engine.cancel(session)
