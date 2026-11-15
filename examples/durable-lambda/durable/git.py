"""The repo function: the only code of the stack that holds the GitHub deploy key.

The key gives access to one repository. The worker has no route to GitHub. `fetch`
copies the main branch to S3 and the sync function puts it on EFS. `publish` pushes
the commits of a session, which the worker left in S3 as a Git bundle. A push never
uses force: when main moved, the commits go to a new branch.
"""

from __future__ import annotations

import io
import json
import os
import re
import subprocess
import tarfile
import tempfile
import time
from typing import Any

from durable import aws
from durable.helpers import log, write_secret
from durable.lambda_runtime import Context
from durable.settings import BRANCH, BUNDLE_LIMIT, DEPLOY_KEY_FILE, DEPLOY_KEY_PARAMETER, ENV, GIT_S, GUARDED, KNOWN_HOSTS, REPO, REPO_URL, SYNC_FUNCTION

# The branch of the stack, and the name that the commits of a bundle get in the clone.
MAIN, INCOMING = f"refs/heads/{BRANCH}", "refs/heads/incoming"


def environment() -> dict[str, str]:
    """Return the environment of Git, with the deploy key and the pinned host keys of GitHub."""
    if not os.path.exists(DEPLOY_KEY_FILE):
        write_secret(DEPLOY_KEY_FILE, aws.parameter(DEPLOY_KEY_PARAMETER).strip() + "\n")
    ssh = f"ssh -F /dev/null -i {DEPLOY_KEY_FILE} -o IdentitiesOnly=yes -o BatchMode=yes -o StrictHostKeyChecking=yes -o UserKnownHostsFile={KNOWN_HOSTS} -o GlobalKnownHostsFile=/dev/null"
    return {
        "PATH": ENV.get("PATH", "/usr/bin:/bin"),
        "HOME": "/tmp",
        "GIT_SSH_COMMAND": ssh,
        "GIT_TERMINAL_PROMPT": "0",
        # Git reads no configuration file that another program can write.
        "GIT_CONFIG_GLOBAL": "/dev/null",
        "GIT_CONFIG_NOSYSTEM": "1",
    }


def git(*arguments: str, cwd: str | None = None, check: bool = True) -> subprocess.CompletedProcess[str]:
    """Run Git with the deploy key. A failure raises, unless `check` is false."""
    done = subprocess.run(["git", *arguments], cwd=cwd, env=environment(), capture_output=True, text=True, timeout=GIT_S)
    if check and done.returncode:
        raise RuntimeError(f"git {arguments[0]} failed: {done.stderr.strip()[-400:]}")
    return done


def head() -> str:
    """Return the commit of the main branch on GitHub."""
    line = git("ls-remote", REPO_URL, MAIN).stdout.split()
    if not line:
        raise RuntimeError("the repository has no main branch")
    return line[0]


def clone(folder: str, *options: str) -> str:
    """Clone the main branch into `folder/repo` and return that path."""
    target = os.path.join(folder, "repo")
    git("clone", "--quiet", "--branch", BRANCH, *options, REPO_URL, target)
    return target


def tree(target: str, commit: str, path: str) -> str:
    """Return the id of a directory in a commit, or an empty string when the commit does not have it."""
    return git("rev-parse", "--verify", "--quiet", f"{commit}:{path}", cwd=target, check=False).stdout.strip()


def fetch(force: bool = False) -> dict[str, Any]:
    """Copy a new main branch to S3 and to EFS. Without a new commit, do nothing."""
    sha = head()
    recorded, _ = aws.get("repo/HEAD")
    if not force and recorded and json.loads(recorded).get("sha") == sha:
        return {"sha": sha, "changed": False}
    with tempfile.TemporaryDirectory(dir="/tmp") as folder:
        target = clone(folder)
        sha = git("rev-parse", "HEAD", cwd=target).stdout.strip()
        git("remote", "remove", "origin", cwd=target)
        buffer = io.BytesIO()
        with tarfile.open(fileobj=buffer, mode="w:gz", compresslevel=3) as archive:
            archive.add(target, arcname=".")
    aws.put(f"repo/snapshots/{sha}.tar.gz", buffer.getvalue())
    answer, failed = aws.invoke(SYNC_FUNCTION, sha=sha)
    if failed:
        raise RuntimeError(f"the sync function failed: {answer[:400]!r}")
    aws.put("repo/HEAD", json.dumps({"sha": sha, "at": time.time()}).encode())
    log("fetched", sha)
    return {"sha": sha, "changed": True}


class Rejected(ValueError):
    """The function does not push these commits. The session gets the reason."""


def push(publication: str, data: bytes) -> dict[str, Any]:
    """Push the commits of a bundle to main, or to a new branch when main moved."""
    with tempfile.TemporaryDirectory(dir="/tmp") as folder:
        # A bare clone: the function never puts the files of a session on its disk.
        target, bundle = clone(folder, "--bare"), os.path.join(folder, "session.bundle")
        with open(bundle, "wb") as file:
            file.write(data)
        git("bundle", "verify", bundle, cwd=target)
        # Full names and no tags: a name in the bundle cannot stand for another commit.
        git("fetch", "--quiet", "--no-tags", bundle, f"refs/heads/publish:{INCOMING}", cwd=target)
        sha = git("rev-parse", "--verify", INCOMING, cwd=target).stdout.strip()
        base = git("merge-base", MAIN, INCOMING, cwd=target, check=False).stdout.strip()
        if not base:
            raise Rejected("the commits do not start from the main branch")
        forward = base in (sha, git("rev-parse", "--verify", MAIN, cwd=target).stdout.strip())
        # A repeated publication: main or a branch of an earlier publication has the commits, so there is nothing to check or to push.
        earlier = BRANCH if base == sha else git("for-each-ref", "--count=1", "--points-at", sha, "--format=%(refname:lstrip=2)", "refs/heads/durable", cwd=target).stdout.strip()
        branch = earlier or (BRANCH if forward else f"durable/{publication[:12]}")
        if not earlier:
            guarded = tree(target, INCOMING, GUARDED)
            touched = git("rev-list", "--full-history", "--max-count=1", f"{base}..{INCOMING}", "--", GUARDED, cwd=target).stdout.strip()
            if touched or guarded != tree(target, base, GUARDED):
                raise Rejected(f"a session cannot change the directory {GUARDED}, because GitHub runs its files")
            if guarded != tree(target, MAIN, GUARDED):
                raise Rejected(f"the main branch changed the directory {GUARDED} after this session started: start a new session")
            git("push", "--quiet", "origin", f"{INCOMING}:refs/heads/{branch}", cwd=target)
    return {"branch": branch, "sha": sha, "url": f"https://github.com/{REPO}/commit/{sha}", "merged": branch == BRANCH, "pushed": not earlier}


def publish(publication: str) -> dict[str, Any]:
    """Push the bundle `outbox/<publication>.bundle`. Return the branch and the commit, or why the function refused."""
    if not re.fullmatch(r"[0-9a-f]{32}", publication):
        raise ValueError("id must be a publication id")
    key = f"outbox/{publication}.bundle"
    try:
        # Look at the size first: the function does not read a bundle that is too large.
        if (aws.head(key) or {"size": 0})["size"] > BUNDLE_LIMIT:
            raise Rejected("the bundle is too large")
        data, _ = aws.get(key)
        if not data or len(data) > BUNDLE_LIMIT:
            raise Rejected("the bundle is empty or too large")
        result = push(publication, data)
    except Rejected as refusal:
        log("refused", publication, str(refusal))
        return {"refused": str(refusal)}
    finally:
        # Each publication has its own bundle, so a bundle has no use after one attempt.
        aws.delete(key)
    log("published" if result["pushed"] else "already published", result["sha"], "to", result["branch"])
    if result["merged"]:
        fetch()
    return result


def handler(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Do the action of the event: `fetch` (the default) or `publish`."""
    action = event.get("action", "fetch")
    if action == "publish":
        return publish(str(event.get("id", "")))
    if action == "fetch":
        return fetch(bool(event.get("force")))
    raise ValueError(f"unknown action {action!r}")
