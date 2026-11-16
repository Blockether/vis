"""The repo function: the only code of the stack that holds the GitHub deploy key.

The key gives access to one repository. The worker has no route to GitHub. `fetch`
copies the main branch to S3 and the sync function puts it on EFS. `publish` pushes
the commits of a session, which the worker left in S3 as a Git bundle. A push never
uses force: when main moved, the commits go to a new branch. `store` commits the file
of a workflow that a person saved in the editor.

The function is also the only writer of `workflows/` in S3. Each fetch makes these
objects the same as the workflow files of main, so a change in GitHub and a change in
the editor end in the same place.
"""

from __future__ import annotations

import glob
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
from durable.domain import NAME
from durable.helpers import log, write_secret
from durable.lambda_runtime import Context
from durable.settings import (
    BRANCH,
    BUNDLE_LIMIT,
    DEPLOY_KEY_FILE,
    DEPLOY_KEY_PARAMETER,
    ENV,
    GIT_EMAIL,
    GIT_NAME,
    GIT_S,
    GUARDED,
    KNOWN_HOSTS,
    REPO,
    REPO_URL,
    SAMPLES,
    SYNC_FUNCTION,
    WORKFLOW_DIR,
    WORKFLOW_LIMIT,
)

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


def workflows(target: str) -> dict[str, bytes]:
    """Return the workflow files of the commit of a clone, by workflow name.

    Git gives the list with the type of each entry, and only a regular file is a
    workflow. A session can push a link, and the target of a link can be a file of this
    function, such as the deploy key.
    """
    found = {}
    for line in git("ls-tree", "-l", "HEAD", f"{WORKFLOW_DIR}/", cwd=target).stdout.splitlines():
        about, _, path = line.partition("\t")
        mode, _, _, size = about.split()
        name = os.path.basename(path).removesuffix(".py")
        if mode in ("100644", "100755") and path.endswith(".py") and NAME.fullmatch(name) and int(size) <= WORKFLOW_LIMIT:
            with open(os.path.join(target, path), "rb") as file:
                found[name] = file.read()
    return found


def write(target: str, name: str, data: bytes | None) -> None:
    """Write the file of a workflow in a clone. Without data, remove it. A link at its place goes away first."""
    path = os.path.join(target, WORKFLOW_DIR, f"{name}.py")
    if os.path.lexists(path):
        os.remove(path)
    if data is not None:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "wb") as file:
            file.write(data)


def commit(target: str, message: str) -> bool:
    """Commit the changed workflow files of a clone and push them to main. Return False when no file changed.

    The push uses no force: when main moved after the clone, it fails.
    """
    git("add", "--all", "--", WORKFLOW_DIR, cwd=target)
    if not git("status", "--porcelain", "--", WORKFLOW_DIR, cwd=target).stdout.strip():
        return False
    git("-c", f"user.name={GIT_NAME}", "-c", f"user.email={GIT_EMAIL}", "commit", "--quiet", "--message", message, cwd=target)
    git("push", "--quiet", "origin", f"HEAD:{MAIN}", cwd=target)
    return True


def samples() -> dict[str, bytes]:
    """Return the workflows of the image, by workflow name."""
    found = {}
    for path in sorted(glob.glob(f"{SAMPLES}/*.py")):
        with open(path, "rb") as file:
            found[os.path.basename(path).removesuffix(".py")] = file.read()
    return found


def mirror(target: str, first: bool) -> list[str]:
    """Make the workflows of S3 the same as the workflows of a clone of main. Return their names.

    The repository wins: S3 gets each file of the clone and loses each workflow that the
    clone does not have. At the first time of a stack, the repository first gets the
    workflows that only the stack has, as one commit: the files of S3, or the samples of
    the image when there is no workflow at all.
    """
    stored = {item["Key"][len("workflows/") : -len(".py")]: item["Key"] for item in aws.keys("workflows/")}
    files = workflows(target)
    if first:
        extra = {name: aws.get(key)[0] for name, key in stored.items() if name not in files and NAME.fullmatch(name)}
        extra = extra or ({} if files else samples())
        for name, data in extra.items():
            write(target, name, data)
        if extra and commit(target, "Add the workflows of the stack"):
            files = workflows(target)
    for name, data in files.items():
        if name not in stored or aws.get(stored[name])[0] != data:
            aws.put(f"workflows/{name}.py", data)
    for name, key in stored.items():
        if name not in files:
            aws.delete(key)
    return sorted(files)


def fetch(force: bool = False) -> dict[str, Any]:
    """Copy a new main branch to S3 and to EFS, and its workflows to S3. Without a new commit, do nothing."""
    sha = head()
    recorded, _ = aws.get("repo/HEAD")
    record = json.loads(recorded) if recorded else {}
    # A record without workflows is from a stack that did not copy them yet.
    if not force and record.get("sha") == sha and "workflows" in record:
        return {"sha": sha, "changed": False}
    with tempfile.TemporaryDirectory(dir="/tmp") as folder:
        target = clone(folder)
        names = mirror(target, "workflows" not in record)
        sha = git("rev-parse", "HEAD", cwd=target).stdout.strip()
        git("remote", "remove", "origin", cwd=target)
        buffer = io.BytesIO()
        with tarfile.open(fileobj=buffer, mode="w:gz", compresslevel=3) as archive:
            archive.add(target, arcname=".")
    aws.put(f"repo/snapshots/{sha}.tar.gz", buffer.getvalue())
    answer, failed = aws.invoke(SYNC_FUNCTION, sha=sha)
    if failed:
        raise RuntimeError(f"the sync function failed: {answer[:400]!r}")
    # The record comes last: a fetch that failed before this line runs again.
    aws.put("repo/HEAD", json.dumps({"sha": sha, "at": time.time(), "workflows": names}).encode())
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


def store(name: str, source: str | None, base: str | None = None) -> dict[str, Any]:
    """Commit the file of a workflow to main and push it. Without a source, remove the file.

    The push uses no force. When main moved after the clone, the push fails and the
    person saves again. A source that main already has makes no commit. `base` is the
    source that the person changed: when main has another source, the answer has
    `moved` and there is no commit, because the save would remove a change of GitHub.
    The fetch at the end gives S3 the workflows of main in each case.
    """
    if not NAME.fullmatch(name):
        raise ValueError("name must be a workflow name")
    if source is not None and len(source) > WORKFLOW_LIMIT:
        raise ValueError("the workflow is too large")
    file = f"{WORKFLOW_DIR}/{name}.py"
    with tempfile.TemporaryDirectory(dir="/tmp") as folder:
        target = clone(folder, "--depth", "1")
        moved = base is not None and workflows(target).get(name) != base.encode()
        changed = False
        if not moved:
            write(target, name, None if source is None else source.encode())
            changed = commit(target, f"{'Remove' if source is None else 'Save'} the workflow {name}")
        sha = git("rev-parse", "HEAD", cwd=target).stdout.strip()
    if changed:
        log("stored", file, "in", sha)
    fetch()
    return {"sha": sha, "changed": changed, "moved": moved, "file": file, "url": f"https://github.com/{REPO}/blob/{sha}/{file}"}


def handler(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Do the action of the event: `fetch` (the default), `publish` or `store`."""
    action = event.get("action", "fetch")
    if action == "publish":
        return publish(str(event.get("id", "")))
    if action == "store":
        source, base = event.get("source"), event.get("base")
        return store(str(event.get("name", "")), None if source is None else str(source), None if base is None else str(base))
    if action == "fetch":
        return fetch(bool(event.get("force")))
    raise ValueError(f"unknown action {action!r}")
