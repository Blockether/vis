"""The sync function: it puts a snapshot of the repository on EFS.

The repo function makes the snapshot and invokes this function. A release is a
directory `releases/<commit>`. The file `CURRENT` names the release that the next
worker uses. Each step is one rename, so a reader sees the old release or the new
release, and never a part of one.
"""

from __future__ import annotations

import io
import json
import os
import re
import shutil
import subprocess
import tarfile
import time
import uuid
from typing import Any

from playground import aws
from playground.env import EFS, log
from playground.runtime import Context

RELEASES, WORK, CURRENT = f"{EFS}/releases", f"{EFS}/work", f"{EFS}/CURRENT"
# The number of releases that stay on EFS, and the days that an unused work directory stays.
KEEP, WORK_DAYS = 5, 14


def safe(member: tarfile.TarInfo, path: str) -> tarfile.TarInfo | None:
    """Skip a member that would leave the directory of the archive."""
    try:
        return tarfile.data_filter(member, path)
    except tarfile.FilterError:
        log("skipped", member.name)
        return None


def unpack(sha: str, key: str) -> None:
    """Put the snapshot in `releases/<sha>` with one rename."""
    data, _ = aws.get(key)
    if data is None:
        raise RuntimeError(f"S3 does not have the snapshot {key}")
    incoming = f"{RELEASES}/.incoming-{uuid.uuid4()}"
    with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as archive:
        archive.extractall(incoming, filter=safe)
    os.rename(incoming, f"{RELEASES}/{sha}")


def current() -> str:
    """Return the commit of the current release, or an empty string before the first sync."""
    try:
        with open(CURRENT, encoding="utf-8") as file:
            return json.load(file)["sha"]
    except (OSError, ValueError, KeyError):
        return ""


def point(sha: str) -> None:
    """Make a release the current release with one rename."""
    temporary = f"{CURRENT}.{uuid.uuid4()}"
    with open(temporary, "w", encoding="utf-8") as file:
        json.dump({"sha": sha, "at": time.time()}, file)
        file.flush()
        os.fsync(file.fileno())
    os.replace(temporary, CURRENT)


def clone(release: str, name: str, environment: dict[str, str]) -> str:
    """Return `work/<name>`, a clone of a release. The directory appears with one rename."""
    target = f"{WORK}/{name}"
    if not os.path.isdir(target):
        incoming = f"{WORK}/.clone-{uuid.uuid4()}"
        subprocess.run(["git", "clone", "--quiet", "--no-hardlinks", f"{RELEASES}/{release}", incoming], env=environment, check=True)
        subprocess.run(["git", "remote", "remove", "origin"], cwd=incoming, env=environment, check=True)
        # The publish step sends only the commits after this one.
        with open(f"{incoming}/.git/playground-base", "w", encoding="utf-8") as file:
            file.write(release)
        try:
            os.rename(incoming, target)
        except OSError:
            shutil.rmtree(incoming, ignore_errors=True)
            if not os.path.isdir(target):  # else another worker made the directory first
                raise
    return target


def sweep(keep: str) -> None:
    """Remove each work directory that nothing changed for WORK_DAYS days.

    The worker calls this, because it owns the directories. Git writes in `.git`
    at each commit, so the newer time of the two directories is the last use.
    """
    limit = time.time() - WORK_DAYS * 86400
    for entry in os.scandir(WORK):
        # A session can leave a link or a file here. Only a real directory is a work directory.
        if entry.name == keep or not entry.is_dir(follow_symlinks=False):
            continue
        try:
            used = max(entry.stat(follow_symlinks=False).st_mtime, os.lstat(f"{entry.path}/.git").st_mtime)
        except OSError:
            continue  # not a clone, or a worker removes it now
        if used < limit:
            shutil.rmtree(entry.path, ignore_errors=True)


def prune(keep: str) -> None:
    """Remove the old releases and the parts of a failed sync."""
    entries = sorted(os.scandir(RELEASES), key=lambda entry: entry.stat().st_mtime, reverse=True)
    releases = [entry for entry in entries if not entry.name.startswith(".")]
    stale = [entry for entry in entries if entry.name.startswith(".") and time.time() - entry.stat().st_mtime > 3600]
    for entry in stale + [entry for entry in releases[KEEP:] if entry.name != keep]:
        shutil.rmtree(entry.path, ignore_errors=True)


def handler(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Make the snapshot of the commit `sha` the current release."""
    sha = str(event.get("sha", ""))
    if not re.fullmatch(r"[0-9a-f]{40}", sha):
        raise ValueError("sha must be a commit id")
    # Only this function writes a release: no other user gets write access to its files.
    os.umask(0o022)
    os.makedirs(RELEASES, exist_ok=True)
    os.makedirs(WORK, exist_ok=True)
    # The worker has another user id and makes its project directories here. The
    # kernel checks a sticky bit with the user id of the function, not of the access
    # point, so the directory must not have it.
    os.chmod(WORK, 0o777)
    if not os.path.isdir(f"{RELEASES}/{sha}"):
        unpack(sha, f"repo/snapshots/{sha}.tar.gz")
    previous = current()
    if previous != sha:
        point(sha)
    prune(sha)
    log("current release", sha, "was", previous or "none")
    return {"current": sha, "previous": previous}
