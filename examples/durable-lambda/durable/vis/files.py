"""The files of the Vis sessions, as S3 objects that the pages read.

attached/<session>/<name>    a copy of each file that a session attached
tmp/<session>/<path>         the files that a session wrote, kept for 7 days
"""

from __future__ import annotations

import base64
import os
import re
from collections.abc import Callable
from typing import Any

from blockether.vis.engine import GatewayError, Session, TransportError

from durable import aws
from durable.domain import ATTACHED_LIMIT, FILE, SESSION, Answer, Refused, need, relative
from durable.helpers import log


class Files:
    """The files of the sessions. `attach` records an attached file in the runs of its session. `touch` records that a session changed its work directory."""

    def __init__(self, attach: Callable[[str, str], None], touch: Callable[..., None]) -> None:
        self.attach, self.touch = attach, touch
        # The worker sets this after the gateway starts: open a session with the SDK.
        self.open: Callable[[str], Session] | None = None
        # The copy that S3 has of each attached file: (session, name) -> its turn and version.
        self.copied: dict[tuple[str, str], dict[str, Any]] = {}

    def listed(self, kind: str, session: str) -> list[dict[str, Any]]:
        """List the files of a session below `attached/` or `tmp/`."""
        prefix = f"{kind}/{need(SESSION, session, 'the session id')}/"
        return [{"name": item["Key"][len(prefix) :], "size": item["Size"], "at": item["LastModified"].timestamp()} for item in aws.keys(prefix)]

    def stored(self, session: str, name: str) -> dict[str, Any]:
        """Return the turn and the version of the copy that S3 has of an attached file."""
        known = (session, name)
        if known not in self.copied:  # a new worker: the metadata of the object names its copy
            found = aws.metadata(f"attached/{session}/{name}") or {}
            self.copied[known] = {"turn": found.get("turn"), "version": int(found.get("version", 0))}
        return self.copied[known]

    def mirror(self, session: str) -> None:
        """Copy each new file that a session attached to `attached/<session>/`.

        Vis keeps the attached files of a session. S3 has a copy, so the pages read all
        documents from one place.
        """
        if self.open is None:
            return
        try:
            files = [file for file in self.open(session).attachments() if file.filename and FILE.fullmatch(file.filename)]
            newest = {file.filename: file for file in sorted(files, key=lambda file: file.version)}
            for name, file in newest.items():
                if self.stored(session, name)["version"] < file.version and file.size <= ATTACHED_LIMIT:
                    about = {"turn": file.turn_id, "version": file.version}
                    aws.put(f"attached/{session}/{name}", file.read(), ContentType="text/plain; charset=utf-8", Metadata={key: str(value) for key, value in about.items() if value is not None})
                    self.copied[session, name] = about
                    self.attach(session, name)
        except (GatewayError, TransportError) as failure:  # the gateway does not know the session, or it stops
            log("no copy of the attached files", session, repr(failure))

    def documents(self, session: str) -> list[dict[str, Any]]:
        """List the attached files of a session, each with the turn that attached it and its version."""
        self.mirror(need(SESSION, session, "the session id"))
        return [{**file, **self.stored(session, file["name"])} for file in self.listed("attached", session)]

    def attached(self, query: dict[str, str], **_: Any) -> dict[str, Any]:
        return {"files": self.documents(query.get("session", ""))}

    def attached_file(self, session: str, name: str, **_: Any) -> Answer:
        key = f"attached/{need(SESSION, session, 'the session id')}/{need(FILE, name, 'the file name')}"
        return self.download(key, name)

    def download(self, key: str, name: str) -> Answer:
        raw, _ = aws.get(key)
        if raw is None:
            raise Refused(404, "no such file")
        # The browser never runs a stored file: it is plain text and a download.
        headers = [["content-type", "text/plain; charset=utf-8"], ["cache-control", "no-store"]]
        filename = re.sub(r"[^\w.-]", "_", os.path.basename(name))
        return 200, [*headers, ["content-disposition", f'attachment; filename="{filename}"']], raw

    def temporary(self, query: dict[str, str], **_: Any) -> dict[str, Any]:
        return {"files": self.listed("tmp", query.get("session", ""))}

    def temporary_file(self, session: str, path: str, **_: Any) -> Answer:
        return self.download(f"tmp/{need(SESSION, session, 'the session id')}/{relative(path)}", path)

    def sync(self, payload: dict[str, Any], **_: Any) -> dict[str, Any]:
        """Copy the files that a session wrote to `tmp/<session>/`."""
        session = need(SESSION, payload.get("session"), "the session id")
        files, removed = payload.get("files") or [], payload.get("removed") or []
        for path in [item.get("path") for item in files] + removed:
            relative(path)
        for item in files:
            aws.put(f"tmp/{session}/{item['path']}", base64.b64decode(item.get("data") or ""))
        for path in removed:
            aws.delete(f"tmp/{session}/{path}")
        # The session reports after each step that can change a file, so its work directory gets a new copy.
        self.touch(session, is_fresh=False)
        return {"synced": len(files), "removed": len(removed)}
