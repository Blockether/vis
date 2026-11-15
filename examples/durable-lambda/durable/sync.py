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

from durable import aws
from durable.env import EFS, log
from durable.runtime import Context

RELEASES, WORK, CURRENT = f"{EFS}/releases", f"{EFS}/work", f"{EFS}/CURRENT"
# The name of the one project directory. It follows the releases, so its sessions stay in one project.
MAIN = "main"
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
        with open(f"{incoming}/.git/durable-base", "w", encoding="utf-8") as file:
            file.write(release)
        try:
            os.rename(incoming, target)
        except OSError:
            shutil.rmtree(incoming, ignore_errors=True)
            if not os.path.isdir(target):  # else another worker made the directory first
                raise
    return target


def earlier() -> str:
    """Return the project directory that was used last before `work/main` existed, or an empty string.

    Before `work/main`, each release had a project directory with the commit as its name.
    """
    found = [entry.path for entry in os.scandir(WORK) if re.fullmatch(r"[0-9a-f]{12}", entry.name) and os.path.isfile(f"{entry.path}/.git/durable-base")]
    return max(found, key=lambda path: os.lstat(f"{path}/.git").st_mtime, default="")


def advance(release: str, environment: dict[str, str]) -> str:
    """Return `work/main`, the project directory, with the commits of a release.

    Call this only while no session works: before the gateway starts. The directory
    takes the release as a fast-forward, else as a merge. When Git refuses both, the
    directory stays as it is and the next worker tries again.
    """
    target = clone(release, MAIN, environment)
    note = f"{target}/.git/durable-base"
    with open(note, encoding="utf-8") as file:
        if file.read().strip() == release:
            return target

    def git(*arguments: str) -> bool:
        return subprocess.run(["git", *arguments], cwd=target, env=environment, capture_output=True, check=False).returncode == 0

    merged = git("fetch", "--quiet", f"{RELEASES}/{release}", "HEAD") and (git("merge", "--quiet", "--ff-only", "FETCH_HEAD") or git("merge", "--quiet", "--no-edit", "FETCH_HEAD"))
    if merged and git("merge-base", "--is-ancestor", release, "HEAD"):
        # The publish step sends only the commits after this one.
        with open(f"{note}.new", "w", encoding="utf-8") as file:
            file.write(release)
        os.replace(f"{note}.new", note)
    else:
        git("merge", "--abort")
        log("the project directory keeps its release: its work does not merge with", release[:12])
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
