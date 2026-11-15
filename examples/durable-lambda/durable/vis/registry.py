"""What the worker registers in Vis: the project directory, the session groups and the live apps."""

from __future__ import annotations

import hashlib
import os

from blockether.vis.engine import GatewayClient

from durable import aws, worktree
from durable.app import Api
from durable.settings import ENV, HOME, REPO_FUNCTION
from durable.vis import gateway
from durable.vis.liveapps.workflows import Workflows


def keep_review(old: str, new: str) -> None:
    """Give the review journals of a directory to its new name.

    Vis Review files the journals of a session below a hash of the project path.
    """
    base = os.path.join(HOME, ".vis", "review")
    before, after = (os.path.join(base, hashlib.sha256(root.encode()).hexdigest()[:16]) for root in (old, new))
    if os.path.isdir(before) and not os.path.exists(after):
        os.rename(before, after)
        with open(os.path.join(after, "ROOT"), "w", encoding="utf-8") as file:
            file.write(new + "\n")


def project() -> tuple[str, str]:
    """Return the project directory and the commit of the current release.

    `work/main` is the directory of the one project of Vis. It takes each new release,
    so the project stays when the repository changes. Each session works in its own
    clone next to it: see `durable/worktree.py`.
    """
    release = worktree.current()
    if not release:
        # The first worker of a new stack asks for the first release.
        aws.invoke(REPO_FUNCTION, action="fetch", force=True)
        release = worktree.current()
    if not release:
        raise RuntimeError("EFS has no release of the repository")
    worktree.sweep(worktree.MAIN)
    main, moved = f"{worktree.WORK}/{worktree.MAIN}", f"{worktree.WORK}/{worktree.MAIN}/.git/durable-earlier"
    earlier = "" if os.path.isdir(main) else worktree.earlier()
    if earlier:
        # The directory keeps its work. The note is for a worker that stops before it saves the journals.
        os.rename(earlier, main)
        with open(moved, "w", encoding="utf-8") as file:
            file.write(earlier)
    if os.path.isfile(moved):
        with open(moved, encoding="utf-8") as file:
            keep_review(file.read().strip(), main)
    return worktree.advance(release, gateway.environment("")), release


def register(engine: gateway.Gateway, api: Api) -> None:
    """Give the API its gateway, make the session groups of the project and start the live apps."""
    root = api.project
    api.sessions.engine, api.runs.busy = engine, engine.busy
    api.runs.group = engine.organize(root, ENV.get("VIS_REPO", "").rpartition("/")[2] or "durable")
    api.sessions.people = engine.group(root, gateway.PEOPLE)
    # Each call gets its own SDK client: one client serves one thread.
    client = lambda: GatewayClient(f"http://{gateway.HOST}:{gateway.PORT}", token=engine.token)  # noqa: E731
    api.files.open = lambda session: client().session(session)
    Workflows(api, client()).start()
