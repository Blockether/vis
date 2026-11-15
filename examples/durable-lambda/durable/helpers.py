"""The small functions that more than one module uses: the log, private files and archives."""

from __future__ import annotations

import json
import os
import tarfile

from durable.settings import ROLE, WORKER_DIR


def private(name: str) -> str:
    """Return the path of a worker file, in a directory that only the worker user reads."""
    os.makedirs(WORKER_DIR, mode=0o700, exist_ok=True)
    return os.path.join(WORKER_DIR, name)


def write_secret(path: str, text: str) -> None:
    """Write a file that only the user of this function can read."""
    with open(os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600), "w", encoding="utf-8") as file:
        file.write(text)


def log(*parts: object) -> None:
    """Write one JSON line to the function log."""
    print(json.dumps({"vis": ROLE, "msg": " ".join(map(str, parts))}), flush=True)


def remove_file(path: str) -> None:
    """Remove a file. A file that is not there is not a failure."""
    if os.path.exists(path):
        os.remove(path)


def safe(member: tarfile.TarInfo, path: str) -> tarfile.TarInfo | None:
    """Skip a member that would leave the directory of the archive."""
    try:
        return tarfile.data_filter(member, path)
    except tarfile.FilterError:
        log("skipped", member.name)
        return None
