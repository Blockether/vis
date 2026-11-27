"""Keep each workflow and its versions in the factory table.

The table is the one store of the workflow code. The directory `settings.WORKFLOWS` is
a working copy, and it is a Vis project: a session changes a file there, and `sync`
keeps that file as a new version. Only `sync` and a person on the page make a version.
"""

from __future__ import annotations

import errno
import hashlib
import os
import secrets
import tempfile
import threading
import time
from collections.abc import Callable
from pathlib import Path
from typing import Any

from vis_optmem.shared import Absent, Equals, Table

from factory import settings
from factory.records import NAME, Document, Records, Refused, need, pack, unpack
from factory.workflows.source import describe

EMPTY: Document = {"inputs": [], "activities": [], "triggers": []}


def digest(raw: bytes | None) -> str | None:
    """Return the mark of a file content, or None for no file."""
    return None if raw is None else hashlib.sha256(raw).hexdigest()


def number(value: Any) -> int:
    """Return a version number, or refuse it."""
    if type(value) is not int or not 1 <= value <= 99_999_999:
        raise Refused(400, "the version is not valid")
    return value


class Workflows:
    """Read the workflow versions, and make a version from a changed file."""

    def __init__(self, table: Table, loads: Callable[[str, str], None] | None = None) -> None:
        """Use the table of the factory. `loads` refuses a source that a run cannot load."""
        self.records, self.table, self.loads = Records(table), table, loads
        self.lock = threading.RLock()
        # The file of a workflow that does not load, and why. A new look at the file replaces it.
        self.errors: dict[str, str] = {}
        # The mark of each file that the check refused, so the same file has one check.
        self.refused: dict[str, str | None] = {}
        # A version does not change, so its description is read one time.
        self.known: dict[str, tuple[int, Document, str]] = {}

    # -- read ------------------------------------------------------------------------

    def head(self, name: Any) -> Document:
        """Return the head of a workflow: its current version number and its state."""
        return self.records.get(f"workflow/{need(NAME, name, 'the workflow name')}", "workflow")

    def code(self, name: str, version: int) -> Document:
        """Return one version with its source."""
        return self.records.get(f"code/{name}/{version:08d}", "version")

    def current(self, name: Any) -> tuple[Document, str]:
        """Return what the current version says about itself, and its source."""
        head = self.head(name)
        if head["deleted"]:
            raise Refused(404, "no such workflow")
        known = self.known.get(name)
        if known is None or known[0] != head["version"]:
            text = self.code(name, head["version"])["source"]
            known = self.known[name] = (head["version"], {**describe(name, text), "version": head["version"], "updated": head["at"]}, text)
        about, choice = known[1], self.choice(name)
        default = {"provider": about["provider"], "model": about["model"]} if about["model"] else None
        return {**about, "choice": choice, "uses": choice or default}, known[2]

    def choice(self, name: str) -> Document | None:
        """Return the model that a person selected for a workflow on the page, or None for the model of the code."""
        item = self.table.get(f"uses/{name}")
        doc = unpack(item) if item else {}
        return {"provider": doc["provider"], "model": doc["model"]} if doc.get("model") else None

    def versions(self, name: str) -> list[Document]:
        """List the versions of a workflow without their source, newest first."""
        fields = ("version", "at", "origin", "from")
        return [{key: doc.get(key) for key in fields} for doc in self.records.listing(f"code/{name}/")]

    def listing(self) -> list[Document]:
        """List each workflow with what its current version says about itself."""
        found = []
        with self.lock:
            errors = dict(self.errors)
        for head in self.records.listing("workflow/"):
            name = head["name"]
            row = {"name": name, "title": name, **EMPTY, "version": head["version"], "updated": head["at"], "deleted": head["deleted"]}
            if not head["deleted"]:
                try:
                    row.update(self.current(name)[0])
                except Refused as refusal:
                    row["error"] = str(refusal)
            found.append({**row, "draft": errors.pop(name, None)})
        # A new file that does not load has no version.
        found += [{"name": name, "title": name, **EMPTY, "version": 0, "updated": None, "deleted": False, "draft": error} for name, error in errors.items()]
        return sorted(found, key=lambda row: row["title"].lower())

    def detail(self, name: Any, version: Any = None) -> Document:
        """Return one version of a workflow, with the list of its versions."""
        head = self.head(name)
        code = self.code(name, head["version"] if version is None else number(version))
        about: Document = {"name": name, "title": name, **EMPTY}
        if code["origin"] != "delete":
            try:
                about = describe(name, code["source"])
            except Refused as refusal:
                about["error"] = str(refusal)
        return {
            **about,
            "source": code["source"],
            "version": code["version"],
            "at": code["at"],
            "origin": code["origin"],
            "current": head["version"],
            "deleted": head["deleted"],
            "choice": self.choice(name),
            "draft": self.errors.get(name),
            "versions": self.versions(name),
        }

    def check(self, name: str, text: str) -> Document:
        """Return what a source says about itself. Refuse a source that a run cannot load."""
        if len(text.encode()) > settings.WORKFLOW_LIMIT:
            raise Refused(413, "the workflow is too large")
        about = describe(name, text)
        if self.loads is not None:
            self.loads(name, text)
        return about

    # -- write -----------------------------------------------------------------------

    def put(self, name: str, source: str | None, origin: str, file: str | None, **more: Any) -> Document:
        """Write the next version and the head in one step. No source removes the workflow."""
        key = f"workflow/{name}"
        while True:
            item = self.table.get(key)
            version = unpack(item)["version"] + 1 if item else 1
            now = time.time()
            head = {"name": name, "version": version, "deleted": source is None, "at": now, "file": file}
            code = {"name": name, "version": version, "source": source or "", "at": now, "origin": origin, **more}
            condition = Equals(key, "version", int(item["version"])) if item else Absent(key)
            if self.records.write({key: pack(head, int(item["version"]) + 1 if item else 1), f"code/{name}/{version:08d}": pack(code)}, [condition]):
                return head

    def choose(self, name: str, provider: str | None, model: str | None) -> None:
        """Keep the model that a person selected for a workflow. No model clears the selection. The last selection replaces the others."""
        self.records.write({f"uses/{name}": pack({"name": name, "provider": provider, "model": model, "at": time.time()})}, [])

    def mark(self, name: str, file: str | None) -> None:
        """Record the file content that agrees with the table. A lost write is repeated by the next look."""
        key = f"workflow/{name}"
        item = self.table.get(key)
        if item is None or unpack(item)["file"] == file:
            return
        self.records.write({key: pack({**unpack(item), "file": file}, int(item["version"]) + 1)}, [Equals(key, "version", int(item["version"]))])

    def seed(self, directory: Path) -> None:
        """Give a new database the first workflows, in one step. A database with a workflow stays as it is."""
        if self.table.query("workflow/"):
            return
        puts, now = {}, time.time()
        for path in sorted(directory.glob("*.py")):
            name, text = path.stem, path.read_text(encoding="utf-8")
            describe(need(NAME, name, "the workflow name"), text)
            puts[f"workflow/{name}"] = pack({"name": name, "version": 1, "deleted": False, "at": now, "file": None})
            puts[f"code/{name}/{1:08d}"] = pack({"name": name, "version": 1, "source": text, "at": now, "origin": "seed"})
        self.table.transact(puts, conditions=[Absent(key) for key in puts])

    def restore(self, name: Any, version: Any) -> Document:
        """Make an old version the current one, as a new version."""
        with self.lock:
            old = self.code(need(NAME, name, "the workflow name"), number(version))
            if old["origin"] == "delete":
                raise Refused(400, "that version removed the workflow")
            self.check(name, old["source"])
            # A change in the file comes first, so the restored version stays the newest one.
            self.settle(name)
            head = self.head(name)
            if old["version"] == head["version"]:
                raise Refused(400, "that is the current version")
            self.put(name, old["source"], "restore", head["file"], **{"from": old["version"]})
            self.settle(name)
            return self.detail(name)

    def remove(self, name: Any) -> Document:
        """Remove a workflow. Its versions stay, and a person can restore one."""
        with self.lock:
            head = self.head(name)
            if not head["deleted"]:
                # The person removes the file as it is now, also with a change that has no version.
                self.put(name, None, "delete", digest(self.raw(name)))
            self.settle(name)
            return {"removed": name}

    # -- the working copy ------------------------------------------------------------

    def path(self, name: str) -> Path:
        """Return the file of a workflow in the project."""
        return Path(settings.WORKFLOWS) / f"{name}.py"

    def raw(self, name: str) -> bytes | None:
        """Read the file of a workflow, or None without a file. A link is not a workflow file."""
        return self.read(self.path(name))

    def read(self, path: Path) -> bytes | None:
        """Read a file of the project without following a link."""
        try:
            file = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
        except FileNotFoundError:
            return None
        except OSError as failure:
            if failure.errno == errno.ELOOP:
                raise Refused(400, "the workflow file is a link") from None
            raise
        with os.fdopen(file, "rb") as content:
            # One byte more than the limit shows a file that is too large.
            return content.read(settings.WORKFLOW_LIMIT + 1)

    def take(self, name: str, before: bytes) -> bool:
        """Remove the file if it still is `before`. Leave a file that changed after the factory read it."""
        path = self.path(name)
        aside = path.with_name(f".{name}.{secrets.token_hex(4)}.old")
        try:
            os.rename(path, aside)  # the factory compares what it moved, so a late write is not lost
        except FileNotFoundError:
            return True
        try:
            if self.read(aside) == before:
                return True
            try:
                os.link(aside, path)
            except FileExistsError:
                pass  # a newer file is there
            return False
        finally:
            aside.unlink(missing_ok=True)

    def write(self, name: str, source: str, before: bytes | None) -> bool:
        """Put the source in the file. Do nothing if the file is not `before` now: the next look takes that change."""
        path = self.path(name)
        path.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as temp:
            temporary = Path(temp.name)
            try:
                temp.write(source.encode())
                temp.close()
                if before is not None and not self.take(name, before):
                    return False
                os.link(temporary, path)  # this fails if a file is there now
            except FileExistsError:
                return False
            finally:
                temporary.unlink(missing_ok=True)
        return True

    def settle(self, name: str) -> None:
        """Make one file and the table agree. Each step can run again after a stop."""
        try:
            raw = self.raw(name)
            item = self.table.get(f"workflow/{name}")
            head = unpack(item) if item else None
            source = self.code(name, head["version"])["source"] if head and not head["deleted"] else None
            same = source is not None and raw == source.encode()
            if raw is not None and not same and (head is None or digest(raw) != head["file"]):
                # The project changed the file: keep it as the next version.
                if self.refused.get(name) == digest(raw):
                    return
                try:
                    if len(raw) > settings.WORKFLOW_LIMIT:
                        raise Refused(413, "the workflow is too large")
                    try:
                        text = raw.decode()
                    except UnicodeDecodeError:
                        raise Refused(400, "the file is not UTF-8 text") from None
                    self.check(name, text)
                except Refused:
                    self.refused[name] = digest(raw)
                    raise
                self.put(name, text, "project", digest(raw))
            elif source is None:
                # The project did not change the file of a removed workflow: remove the file.
                if raw is not None:
                    self.take(name, raw)
                self.mark(name, None)
            elif same or self.write(name, source, raw):
                # The project did not change the file: the file follows the table.
                self.mark(name, digest(source.encode()))
            self.refused.pop(name, None)
            self.errors.pop(name, None)
        except Refused as refusal:
            self.errors[name] = str(refusal)
        except OSError as failure:
            self.errors[name] = f"the factory cannot use the workflow file: {failure.strerror}"

    def sync(self) -> None:
        """Look at each workflow file and each workflow of the table."""
        with self.lock:
            names = {head["name"] for head in self.records.listing("workflow/")}
            try:
                names |= {path.stem for path in Path(settings.WORKFLOWS).glob("*.py") if NAME.fullmatch(path.stem)}
            except OSError:
                pass
            for name in set(self.errors) - names:
                del self.errors[name]
            for name in sorted(names):
                self.settle(name)
