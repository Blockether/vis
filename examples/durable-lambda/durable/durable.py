"""What makes the worker durable: the lease and the checkpoints.

The lease is one S3 object that names the one live worker and its last checkpoint.
Every write is conditional on the ETag that this worker read. A second worker that
starts by mistake loses the first conditional write and stops. This makes S3 a
database with one writer.

A checkpoint is the Vis home directory as S3 objects. S3 is the database of record. A
checkpoint is a tar file of the home directory with a consistent copy of each SQLite
database. A checkpoint counts only after the lease names it, so a worker that lost the
lease cannot overwrite newer state.
"""

from __future__ import annotations

import hashlib
import io
import json
import os
import shutil
import sqlite3
import tarfile
import time
import uuid
from typing import Any

from durable import aws
from durable.helpers import log, private, safe
from durable.settings import HOME, LEASE, LEASE_TTL


class Lease:
    """The lease of this worker. `data` is the last content that it wrote."""

    def __init__(self, data: dict[str, Any], etag: str | None) -> None:
        self.id = str(uuid.uuid4())
        self.data, self.etag, self.saves = data, etag, 0

    def write(self, **fields: Any) -> None:
        """Write the lease with new fields. A lost lease raises ClientError."""
        data = {**self.data, **fields, "owner": self.id}
        condition = {"IfMatch": self.etag} if self.etag else {"IfNoneMatch": "*"}
        self.etag = aws.put(LEASE, json.dumps(data).encode(), **condition)
        self.data = data

    def renew(self, **fields: Any) -> None:
        """Keep the lease for LEASE_TTL more seconds."""
        self.write(expires=time.time() + LEASE_TTL, **fields)

    def release(self, **fields: Any) -> None:
        """Give the lease up, so the next worker starts without a wait."""
        self.write(expires=0, **fields)

    @classmethod
    def acquire(cls) -> Lease | None:
        """Take the lease, or return None when another worker holds it."""
        data, etag = aws.lease()
        if aws.alive(data):
            return None
        held = cls({k: v for k, v in data.items() if k != "owner"}, etag)
        try:
            held.renew()
        except aws.ClientError:
            return None
        return held


# Caches and logs are not state. The next worker builds them again.
# Vis extracts `.vis/native` and `.vis/python/vis-guest` from its binary at each start.
UNSAVED = (".vis/logs", ".vis/models", ".vis/cache", ".vis/native", ".vis/python/vis-guest", ".m2", ".gitlibs", ".cache", ".clojure/.cpcache")
# The journal files of SQLite and the lock files. A checkpoint has a consistent copy
# of each database, so these files stay out.
JOURNALS = ("-wal", "-shm", "-journal", ".lock")


def files() -> list[tuple[str, str]]:
    """Return the (path, name in the tar file) of each file to save."""
    found = []
    for folder, folders, names in os.walk(HOME):
        relative = os.path.relpath(folder, HOME)
        folders[:] = [f for f in folders if os.path.normpath(os.path.join(relative, f)) not in UNSAVED]
        for name in names:
            if not name.endswith(JOURNALS):
                path = os.path.join(folder, name)
                found.append((path, os.path.normpath(os.path.join(relative, name))))
    return found


def fingerprint() -> str:
    """Digest the name, the size and the time of each saved file and SQLite journal."""
    digest = hashlib.sha256()
    for path, name in files():
        for candidate in (path, path + "-wal"):
            try:
                stat = os.stat(candidate)
                digest.update(f"{name}:{stat.st_size}:{stat.st_mtime_ns};".encode())
            except FileNotFoundError:
                pass
    return digest.hexdigest()


def restore(key: str | None) -> None:
    """Replace the home directory with a checkpoint. Without a key, start a new home."""
    shutil.rmtree(HOME, ignore_errors=True)
    os.makedirs(os.path.join(HOME, ".vis"), exist_ok=True)
    if not key:
        return
    data, _ = aws.get(key)
    if data is None:
        raise RuntimeError(f"the lease names the checkpoint {key}, but S3 does not have it")
    with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as archive:
        # A session can make a link to an absolute path. The restore skips such a link: it must not stop the worker.
        archive.extractall(HOME, filter=safe)
    log("restored", key, len(data), "bytes")


def backup(path: str) -> str | None:
    """Copy a SQLite database while the gateway writes. Return the copy, or None when the file is not a database."""
    copy = private("checkpoint.db")
    source, target = sqlite3.connect(path), sqlite3.connect(copy)
    try:
        with target:
            source.backup(target)
        return copy
    except sqlite3.Error:
        return None
    finally:
        source.close()
        target.close()


def pack() -> bytes:
    """Return the home directory as a tar file."""
    buffer = io.BytesIO()
    with tarfile.open(fileobj=buffer, mode="w:gz", compresslevel=3) as archive:
        for path, name in files():
            try:
                # A session can write a file `*.db` that is not a database. It goes in as it is.
                copy = backup(path) if path.endswith(".db") and not os.path.islink(path) else None
                archive.add(copy or path, arcname=name)
            except FileNotFoundError:
                pass  # the gateway removed the file after the directory scan
    return buffer.getvalue()


def save(held: Lease) -> str:
    """Upload a checkpoint and commit it in the lease. Return its key."""
    started, previous = time.time(), held.data.get("state")
    held.saves += 1
    key = f"state/{held.id}/{held.saves:06d}.tar.gz"
    data = pack()
    aws.put(key, data, IfNoneMatch="*")
    try:
        held.renew(state=key)
    except aws.ClientError:
        aws.delete(key)
        raise
    if previous:
        aws.delete(previous)
    log("saved", key, len(data), "bytes in", round(time.time() - started, 2), "s")
    return key
