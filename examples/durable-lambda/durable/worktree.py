"""The directories of the repository on EFS: the releases and the work directories.

The repo function makes a snapshot of the repository and invokes the sync function,
which is `handler` here. A release is a directory `releases/<commit>`. The file
`CURRENT` names the release that the next worker uses. Each step is one rename, so a
reader sees the old release or the new release, and never a part of one.

A session of a person and a workflow run each work in a directory `work/<name>`: a
workspace, which is a clone of a release. The project directory `work/main` follows
the releases. EFS is the fast copy. S3 holds the durable copy of each workspace, so
the work stays when the directory goes: the worker removes an unused directory, and a
file system can be new.

    work/<name>/meta.json         the commits of the copy, its time and its session
    work/<name>/<commit>.bundle   the commits and the files after the release of the workspace
    outbox/<id>.bundle            commits that wait for the repo function

The copy holds each branch of the directory, and one commit with each file that is not
committed. Git does not put an ignored file in it.
"""

from __future__ import annotations

import io
import json
import os
import re
import shutil
import subprocess
import tarfile
import threading
import time
import uuid
from collections.abc import Callable
from typing import Any

from durable import aws
from durable.domain import Conflict, Refused
from durable.helpers import log, private, remove_file, safe
from durable.lambda_runtime import Context
from durable.settings import EFS, REPO_FUNCTION

RELEASES, WORK, CURRENT = f"{EFS}/releases", f"{EFS}/work", f"{EFS}/CURRENT"
# The name of the one project directory. It follows the releases, so its sessions stay in one project.
MAIN = "main"
# The number of releases that stay on EFS, and the days that an unused work directory stays.
KEEP, WORK_DAYS = 5, 14


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


# -- the workspaces ----------------------------------------------------------------

# The name of a workspace is a part of an S3 key. The project directory `main` is not a workspace.
NAME = re.compile(r"[0-9a-f][0-9a-f-]{7,63}")
# The commit of the last copy, in the directory.
REF = "refs/durable/snapshot"
# The largest bundle that S3 gets.
LIMIT = 100_000_000
FAILURES = (RuntimeError, OSError, subprocess.SubprocessError, aws.ClientError, aws.BotoCoreError)


def name_of(root: str) -> str | None:
    """Return the name of the workspace of a directory, or None for another directory."""
    root = os.path.realpath(root)
    name = os.path.basename(root)
    return name if os.path.dirname(root) == os.path.realpath(WORK) and NAME.fullmatch(name) else None


class Workspaces:
    """The workspaces of this worker. `environment` returns the variables of a Git process."""

    def __init__(self, environment: Callable[[], dict[str, str]]) -> None:
        self.environment = environment
        self.lock = threading.Lock()
        # One thread at a time changes a workspace or makes its copy.
        self.locks: dict[str, threading.RLock] = {}
        # The workspaces that changed after their last copy, and each workspace that this worker used.
        self.dirty: set[str] = set()
        self.used: set[str] = set()
        # What S3 has of each workspace that this worker read or wrote.
        self.saved: dict[str, dict[str, Any] | None] = {}

    def held(self, name: str) -> threading.RLock:
        with self.lock:
            return self.locks.setdefault(name, threading.RLock())

    def git(self, root: str, *arguments: str, check: bool = True, extra: dict[str, str] | None = None) -> str:
        """Run Git in a directory and return its output."""
        done = subprocess.run(["git", *arguments], cwd=root, env={**self.environment(), **(extra or {})}, capture_output=True, text=True, timeout=300)
        if check and done.returncode:
            raise RuntimeError(f"git {arguments[0]} failed: {done.stderr.strip()[-300:]}")
        return done.stdout.strip()

    def base(self, root: str) -> str:
        """Return the release that a workspace started from."""
        with open(f"{root}/.git/durable-base", encoding="utf-8") as file:
            return file.read().strip()

    def create(self, release: str) -> str:
        """Return a new workspace: a clone of a release."""
        return clone(release, uuid.uuid4().hex, self.environment())

    def owner(self, name: str) -> str:
        """Return the session that a workspace belongs to, or an empty string."""
        try:
            with open(f"{WORK}/{name}/.git/durable-session", encoding="utf-8") as file:
                return file.read().strip()
        except OSError:
            return str((self.meta(name) or {}).get("session") or "")

    def own(self, root: str, session: str) -> None:
        """Record the session of a workspace. The first session keeps the workspace."""
        name = name_of(root)
        if name and not os.path.isfile(f"{root}/.git/durable-session"):
            with open(f"{root}/.git/durable-session", "w", encoding="utf-8") as file:
                file.write(session)
        self.mark(root)

    def names(self) -> set[str]:
        """Return the name of each workspace that EFS or S3 has."""
        found = {entry.name for entry in os.scandir(WORK) if name_of(entry.path)}
        return found | {name for name in (item["Key"].split("/")[1] for item in aws.keys("work/")) if NAME.fullmatch(name)}

    def mark(self, root: str) -> None:
        """Record that a session changed a workspace."""
        name = name_of(root)
        if name:
            with self.lock:
                self.dirty.add(name)
                self.used.add(name)

    def meta(self, name: str, fresh: bool = False) -> dict[str, Any] | None:
        """Return what S3 has of a workspace, or None."""
        if fresh or name not in self.saved:
            raw, _ = aws.get(f"work/{name}/meta.json")
            self.saved[name] = json.loads(raw) if raw else None
        return self.saved[name]

    # -- the copy in S3 ----------------------------------------------------------

    def snapshot(self, name: str) -> dict[str, Any] | None:
        """Copy a workspace to S3 when it changed. Return what S3 has, or None without a directory."""
        root = f"{WORK}/{name}"
        with self.held(name):
            if not os.path.isfile(f"{root}/.git/durable-base"):
                return None
            head = self.git(root, "rev-parse", "HEAD")
            branch = self.git(root, "symbolic-ref", "--quiet", "--short", "HEAD", check=False)
            # A second index holds each file of the directory. The index of the session stays as it is.
            index = private(f"index-{uuid.uuid4().hex}")
            try:
                if os.path.isfile(f"{root}/.git/index"):
                    shutil.copyfile(f"{root}/.git/index", index)
                self.git(root, "add", "--all", extra={"GIT_INDEX_FILE": index})
                tree = self.git(root, "write-tree", extra={"GIT_INDEX_FILE": index})
            finally:
                remove_file(index)
                remove_file(f"{index}.lock")
            known, session = self.meta(name), self.owner(name)
            if known and (known["head"], known["tree"], known["branch"], known.get("session", "")) == (head, tree, branch, session):
                return known
            work = head if tree == self.git(root, "rev-parse", "HEAD^{tree}") else self.git(root, "commit-tree", tree, "-p", head, "-m", "The work of a session")
            self.git(root, "update-ref", REF, work)
            base = self.base(root)
            meta = {"name": name, "session": session, "base": base, "head": head, "branch": branch, "tree": tree, "work": work, "saved": time.time(), "bundle": None, "bytes": 0}
            if self.git(root, "rev-list", "--max-count=1", REF, "--branches", f"^{base}"):
                file = private(f"{uuid.uuid4().hex}.bundle")
                try:
                    self.git(root, "bundle", "create", file, REF, "--branches", f"^{base}")
                    size = os.path.getsize(file)
                    if size > LIMIT:
                        log("no copy of the workspace", name, "its bundle has", size, "bytes")
                        return known
                    with open(file, "rb") as source:
                        data = source.read()
                finally:
                    remove_file(file)
                meta.update(bundle=f"work/{name}/{work}.bundle", bytes=size)
                aws.put(meta["bundle"], data)
            # The meta object names the bundle, so a reader never gets a half of a copy.
            aws.put(f"work/{name}/meta.json", json.dumps(meta).encode())
            for item in aws.keys(f"work/{name}/"):
                if item["Key"] not in (f"work/{name}/meta.json", meta["bundle"]):
                    aws.delete(item["Key"])
            self.saved[name] = meta
            return meta

    def save(self, is_last: bool = False) -> int:
        """Copy each changed workspace to S3. Return the number of copies that failed.

        The last save of a worker looks at each workspace that the worker used: a turn
        can change a file after its last report.
        """
        with self.lock:
            names, self.dirty = sorted(self.dirty | self.used if is_last else self.dirty), set()
        failed = 0
        for name in names:
            try:
                self.snapshot(name)
            except FAILURES as failure:
                failed += 1
                log("no copy of the workspace", name, repr(failure))
                with self.lock:  # the next look tries again
                    self.dirty.add(name)
        return failed

    def restore(self, name: str) -> bool:
        """Make the directory of a workspace from its copy in S3. Return True when the directory exists."""
        target = f"{WORK}/{name}"
        with self.held(name):
            if os.path.isdir(target):
                return True
            meta = self.meta(name, fresh=True)
            release = current()
            if not meta or not release:
                return False
            incoming, file = f"{WORK}/.restore-{uuid.uuid4().hex}", private(f"{uuid.uuid4().hex}.bundle")
            try:
                # The clone has each commit of the repository. The bundle adds the commits of the workspace.
                subprocess.run(["git", "clone", "--quiet", "--no-hardlinks", f"{RELEASES}/{release}", incoming], env=self.environment(), check=True, capture_output=True)
                self.git(incoming, "remote", "remove", "origin")
                self.git(incoming, "checkout", "--quiet", "--detach")
                if meta["bundle"]:
                    data, _ = aws.get(meta["bundle"])
                    if data is None:
                        raise RuntimeError(f"S3 does not have {meta['bundle']}")
                    with open(file, "wb") as copy:
                        copy.write(data)
                    self.git(incoming, "fetch", "--quiet", "--force", file, "refs/heads/*:refs/heads/*", f"{REF}:{REF}")
                # The files come from the work commit. The branch and the index come from the last commit of the session.
                self.git(incoming, "checkout", "--quiet", "--detach", meta["work"])
                if meta["branch"]:
                    self.git(incoming, "update-ref", f"refs/heads/{meta['branch']}", meta["head"])
                    self.git(incoming, "symbolic-ref", "HEAD", f"refs/heads/{meta['branch']}")
                else:
                    self.git(incoming, "update-ref", "--no-deref", "HEAD", meta["head"])
                self.git(incoming, "reset", "--quiet", "--mixed")
                for note, value in (("durable-base", meta["base"]), ("durable-session", meta.get("session"))):
                    if value:
                        with open(f"{incoming}/.git/{note}", "w", encoding="utf-8") as record:
                            record.write(value)
                os.rename(incoming, target)
            except FAILURES as failure:
                shutil.rmtree(incoming, ignore_errors=True)
                log("no restore of the workspace", name, repr(failure))
                return os.path.isdir(target)
            finally:
                remove_file(file)
            log("restored the workspace", name, "from S3")
            return True

    def ensure(self, root: str) -> bool:
        """Make the directory of a workspace when EFS does not have it. Return True when the directory exists.

        The directory comes from the copy in S3. A workspace that S3 does not know starts
        again from the current release.
        """
        name = name_of(root)
        if os.path.isdir(root) or not name:
            return os.path.isdir(root)
        with self.held(name):
            release = current()
            if release and self.meta(name, fresh=True) is None:
                clone(release, name, self.environment())
                log("the workspace", name, "starts again from the release: S3 has no copy")
                return True
            return self.restore(name)

    def remove(self, name: str) -> None:
        """Remove a workspace: its directory and its copy in S3."""
        if not NAME.fullmatch(name):
            return
        with self.held(name):
            with self.lock:
                self.dirty.discard(name)
                self.used.discard(name)
            shutil.rmtree(f"{WORK}/{name}", ignore_errors=True)
            for item in aws.keys(f"work/{name}/"):
                aws.delete(item["Key"])
            self.saved.pop(name, None)
        log("removed the workspace", name)

    # -- the release of a workspace ----------------------------------------------

    def status(self, root: str) -> dict[str, Any]:
        """Return the release of a workspace, its commits and its changed files."""
        name = name_of(root)
        base, release = self.base(root), current()
        meta = self.meta(name) if name else None
        return {
            "name": name,
            "root": root,
            "branch": self.git(root, "symbolic-ref", "--quiet", "--short", "HEAD", check=False),
            "base": base,
            "release": release,
            "is_behind": bool(release) and base != release,
            "commits": int(self.git(root, "rev-list", "--count", f"{base}..HEAD")),
            "changed": len(self.git(root, "status", "--porcelain").splitlines()),
            "saved": meta["saved"] if meta else None,
        }

    def resync(self, root: str) -> dict[str, Any]:
        """Put the work of a workspace on the current release.

        The commits of the workspace move to the release with a rebase. The files that
        are not committed go on them again. When Git finds a conflict, the directory
        stays as it was and the call raises `Conflict` with the files.
        """
        name = name_of(root)
        if not name:
            raise RuntimeError("the directory is not a workspace")
        with self.held(name):
            before, release = self.snapshot(name), current()
            if before is None or not release:
                raise RuntimeError("the workspace has no directory")
            if before["base"] == release:
                return {**self.status(root), "resynced": False}
            head, work = before["head"], before["work"]

            def back(files: list[str]) -> Conflict:
                """Put the directory back as it was: the commits, then the files that are not committed."""
                self.git(root, "rebase", "--abort", check=False)
                self.git(root, "reset", "--quiet", "--hard", work)
                self.git(root, "reset", "--quiet", "--mixed", head)
                return Conflict(files)

            def unmerged() -> list[str]:
                return self.git(root, "diff", "--name-only", "--diff-filter=U", check=False).splitlines()

            # The work commit holds each file, so the directory can be clean for the rebase.
            self.git(root, "reset", "--quiet", "--hard")
            self.git(root, "clean", "--quiet", "-fd")
            try:
                self.git(root, "fetch", "--quiet", f"{RELEASES}/{release}", "HEAD")
            except RuntimeError:
                back([])
                raise
            try:
                self.git(root, "rebase", "--quiet", "FETCH_HEAD")
            except RuntimeError:
                raise back(unmerged()) from None
            if work != head:
                try:
                    self.git(root, "cherry-pick", "--no-commit", work)
                except RuntimeError:
                    raise back(unmerged()) from None
                self.git(root, "reset", "--quiet")
            with open(f"{root}/.git/durable-base.new", "w", encoding="utf-8") as note:
                note.write(release)
            os.replace(f"{root}/.git/durable-base.new", f"{root}/.git/durable-base")
            self.snapshot(name)
            return {**self.status(root), "resynced": True}

    # -- publishing --------------------------------------------------------------

    def publish(self, root: str, message: str) -> dict[str, Any]:
        """Commit the work of a project directory and give it to the repo function."""
        root = os.path.realpath(root)
        if os.path.dirname(root) != os.path.realpath(WORK) or not os.path.isfile(f"{root}/.git/durable-base"):
            raise Refused(400, "the directory is not a project of Durable Lambda")
        self.mark(root)
        base = self.base(root)
        message = " ".join((message or "Changes from Durable Lambda").split())[:200]
        self.git(root, "add", "--all")
        if self.git(root, "status", "--porcelain"):
            self.git(root, "commit", "--quiet", "--message", message)
        head = self.git(root, "rev-parse", "HEAD")
        if head == base:
            raise Refused(409, "there are no new commits to publish")
        publication = uuid.uuid4().hex
        bundle = private(f"{publication}.bundle")
        self.git(root, "branch", "--force", "publish", "HEAD")
        try:
            self.git(root, "bundle", "create", bundle, "publish", f"^{base}")
            with open(bundle, "rb") as file:
                aws.put(f"outbox/{publication}.bundle", file.read())
        finally:
            remove_file(bundle)
        raw, failed = aws.invoke(REPO_FUNCTION, action="publish", id=publication)
        result = json.loads(raw)
        if failed:
            log("publish failed", result)
            raise Refused(502, "the repo function could not push the commits")
        if result.get("refused"):
            raise Refused(422, result["refused"])
        return {**result, "commits": int(self.git(root, "rev-list", "--count", f"{base}..{head}"))}
