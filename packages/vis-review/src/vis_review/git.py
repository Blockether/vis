"""Git actions of the Review app. Each command starts in the journal root."""

from __future__ import annotations

import os
import shutil
from dataclasses import dataclass

from vis_lang_interface import process

from vis_review.journal import Journal, base_ref


@dataclass(frozen=True)
class Result:
    """The outcome of one action and the output of its commands."""

    ok: bool
    message: str
    output: str = ""
    conflicts: tuple[str, ...] = ()


@dataclass(frozen=True)
class Status:
    """The current branch and the files that Git can commit."""

    ok: bool = False
    message: str = "No Git work tree"
    output: str = ""
    branch: str | None = None
    upstream: str | None = None
    ahead: int = 0
    behind: int = 0
    dirty: bool = False
    own_dirty: bool = False
    base: str | None = None
    ahead_of_base: bool = False
    has_origin: bool = False
    has_gh: bool = False
    operation: str | None = None


def _run(root: str, program: str, *args: str, stdin=None) -> process.ToolRun:
    try:
        return process.run(
            [program, *args],
            cwd=root,
            timeout_s=60,
            stdin=stdin,
            env={
                "GIT_TERMINAL_PROMPT": "0",
                "GIT_EDITOR": ":",
                "GH_PROMPT_DISABLED": "1",
            },
        )
    except (process.ToolMissing, process.ToolTimeout, OSError) as error:
        return process.ToolRun((program, *args), 1, "", str(error), 0)


def _git(root: str, *args: str, stdin=None) -> process.ToolRun:
    return _run(root, "git", *args, stdin=stdin)


def _output(*runs: process.ToolRun) -> str:
    return "\n".join(s.strip() for run in runs for s in (run.out, run.err) if s.strip())


def _read(root: str, *args: str) -> str | None:
    done = _git(root, *args)
    return (done.out.strip() or None) if done.is_ok else None


def _operation(root: str) -> str | None:
    directory = _read(root, "rev-parse", "--absolute-git-dir")
    if directory is not None:
        for action, markers in (
            ("rebase", ("rebase-merge", "rebase-apply")),
            ("merge", ("MERGE_HEAD",)),
            ("cherry-pick", ("CHERRY_PICK_HEAD",)),
            ("revert", ("REVERT_HEAD",)),
        ):
            if any(os.path.exists(os.path.join(directory, name)) for name in markers):
                return action
    return None


def _changes(root: str) -> tuple[process.ToolRun, dict[str, str], set[str]]:
    done = _git(
        root,
        "--no-optional-locks",
        "status",
        "--porcelain=v2",
        "--branch",
        "--no-renames",
        "-z",
        "-uall",
    )
    attrs = {}
    paths = set()
    for row in done.out.split("\0"):
        if row.startswith("# "):
            key, _, value = row[2:].partition(" ")
            attrs[key] = value
        elif row[:2] in ("1 ", "u ", "? "):
            fields = row.split(" ", {"1": 8, "u": 10, "?": 1}[row[0]])
            paths.add(fields[-1])
    return done, attrs, paths


def _own_dirty(journal: Journal, changed: set[str]) -> list[str]:
    if not changed:
        return []
    prefix = _read(journal.root, "rev-parse", "--show-prefix") or ""
    return sorted({p for p in journal.own_paths() if prefix + p in changed})


def status(journal: Journal) -> Status:
    """Read work tree status without fetching or taking the Git index lock."""
    root = journal.root
    done, attrs, paths = _changes(root)
    if not done.is_ok:
        return Status(output=_output(done))
    current = attrs.get("branch.head")
    branch = None if current == "(detached)" else current
    ahead, behind = (int(n[1:]) for n in attrs.get("branch.ab", "+0 -0").split())
    base = base_ref(root)
    tip = base.branch if base else None
    delta = _read(root, "rev-list", "--count", f"{tip}..HEAD") if tip else None
    return Status(
        ok=True,
        message="Git status",
        output=_output(done),
        branch=branch,
        upstream=attrs.get("branch.upstream"),
        ahead=ahead,
        behind=behind,
        dirty=bool(paths),
        own_dirty=bool(_own_dirty(journal, paths)),
        base=tip,
        ahead_of_base=bool(delta and int(delta)),
        has_origin=_read(root, "config", "--get", "remote.origin.url") is not None,
        has_gh=shutil.which("gh") is not None,
        operation=_operation(root),
    )


def _answer(root: str, message: str, *runs: process.ToolRun) -> Result:
    ok = runs[-1].is_ok
    conflicts = ()
    if not ok:
        conflicts = tuple(
            filter(
                None,
                (
                    _read(root, "diff", "--name-only", "--diff-filter=U", "-z") or ""
                ).split("\0"),
            )
        )
    return Result(ok, message, _output(*runs), conflicts)


def _compact(journal: Journal, done: Result, head: str) -> Result:
    if done.ok:
        journal.mark_merged(head)
    return done


def commit(journal: Journal, message: str) -> Result:
    """Commit only paths of this session. Leave other staged paths in the index."""
    message = message.strip()
    if not message or "\0" in message:
        return Result(False, "Enter a commit message")
    root = journal.root
    if _operation(root):
        return Result(False, "Finish the Git operation before creating a commit")
    listed, _, changed = _changes(root)
    if not listed.is_ok:
        return _answer(root, "Could not read the changed paths", listed)
    selected = _own_dirty(journal, changed)
    if not selected:
        return Result(False, "No files of this session to commit")
    names = "\0".join(selected) + "\0"
    # Commit --only stages tracked paths in its own index. Add only new paths first.
    new = _git(root, "ls-files", "--others", "--exclude-standard", "-z")
    if not new.is_ok:
        return _answer(root, "Could not list new files", new)
    untracked = sorted(set(new.out.split("\0")) & set(selected))
    if untracked:
        added = _git(
            root,
            "--literal-pathspecs",
            "add",
            "--intent-to-add",
            "--pathspec-from-file=-",
            "--pathspec-file-nul",
            stdin="\0".join(untracked) + "\0",
        )
        if not added.is_ok:
            return _answer(root, "Could not stage the new files", added)
    saved = _git(
        root,
        "--literal-pathspecs",
        "commit",
        "--only",
        "-m",
        message,
        "--pathspec-from-file=-",
        "--pathspec-file-nul",
        stdin=names,
    )
    return _answer(root, "Commit created" if saved.is_ok else "Commit failed", saved)


def _target(root: str, base: str) -> tuple[str, str] | None:
    if base.startswith("origin/"):
        return "origin", base.removeprefix("origin/")
    if _read(root, "config", "--get", "remote.origin.url"):
        return "origin", base
    return None


def _push(journal: Journal, branch: str | None = None) -> Result:
    root = journal.root
    if _operation(root):
        return Result(False, "Finish the Git operation before pushing")
    local = _read(root, "symbolic-ref", "--quiet", "--short", "HEAD")
    if not local:
        return Result(False, "Check out a branch before pushing")
    if branch is None:
        remote = _read(root, "config", "--get", f"branch.{local}.remote")
        ref = _read(root, "config", "--get", f"branch.{local}.merge")
        if not remote or remote == "." or not ref or not ref.startswith("refs/heads/"):
            return Result(False, "No remote upstream. Use Push to branch.")
        branch = ref.removeprefix("refs/heads/")
        options = ()
    else:
        remote, options = "origin", ("--set-upstream",)
        if (
            not branch
            or branch.startswith("-")
            or not _git(root, "check-ref-format", f"refs/heads/{branch}").is_ok
        ):
            return Result(False, "Enter a valid branch name")
    base = base_ref(root)
    target = _target(root, base.branch) if base else None
    # An explicit refspec ignores configured push refspecs and cannot force-update a branch.
    head = _read(root, "rev-parse", "--verify", "HEAD")
    if head is None:
        return Result(False, "Create a commit before pushing")
    sent = _git(
        root, "push", *options, "--", remote, f"refs/heads/{local}:refs/heads/{branch}"
    )
    done = _answer(
        root, f"Pushed to {remote}/{branch}" if sent.is_ok else "Push failed", sent
    )
    return (
        _compact(journal, done, head)
        if sent.is_ok and target == (remote, branch)
        else done
    )


def push(journal: Journal) -> Result:
    """Push the current branch to its upstream without forcing an update."""
    return _push(journal)


def push_to_branch(journal: Journal, branch: str) -> Result:
    """Push the current branch to the named remote branch and set its upstream."""
    return _push(journal, branch.strip())


def commit_and_push(journal: Journal, message: str) -> Result:
    """Commit this session and push it. Keep the commit if the push fails."""
    committed = commit(journal, message)
    if not committed.ok:
        return committed
    sent = push(journal)
    return Result(
        sent.ok,
        f"Commit created. {sent.message}",
        "\n".join(filter(None, (committed.output, sent.output))),
        sent.conflicts,
    )


def _ready(journal: Journal) -> tuple[Status, Result | None]:
    state = status(journal)
    if not state.ok:
        return state, Result(False, state.message, state.output)
    if state.operation:
        return state, Result(False, f"Finish the {state.operation} first")
    if not state.branch or state.dirty:
        return state, Result(False, "Check out a branch with a clean work tree")
    return state, None


def pull(journal: Journal) -> Result:
    """Pull upstream and rebase local commits."""
    state, refused = _ready(journal)
    if refused is not None:
        return refused
    if not state.upstream:
        return Result(False, "No upstream branch to pull")
    done = _git(journal.root, "pull", "--rebase", "--no-autostash")
    return _answer(journal.root, "Pull complete" if done.is_ok else "Pull failed", done)


def rebase(journal: Journal) -> Result:
    """Rebase the current branch on the known base branch."""
    state, refused = _ready(journal)
    if refused is not None:
        return refused
    if not state.base:
        return Result(False, "No base branch to rebase onto")
    done = _git(journal.root, "rebase", "--no-autostash", state.base)
    return _answer(
        journal.root, "Rebase complete" if done.is_ok else "Rebase stopped", done
    )


def merge(journal: Journal) -> Result:
    """Merge into the base branch and push it. Leave conflicts for a person."""
    state, refused = _ready(journal)
    if refused is not None:
        return refused
    root = journal.root
    target = _target(root, state.base) if state.base else None
    if target is None:
        return Result(False, "No remote base branch to merge into")
    remote, branch = target
    if state.branch == branch:
        return Result(False, "Already on the base branch. Use Push.")
    if not state.ahead_of_base:
        return Result(False, "No commits to merge into the base branch")
    # Fetch the tip before the merge, then let a normal push reject races.
    fetched = _git(root, "fetch", "--", remote, f"refs/heads/{branch}")
    if not fetched.is_ok:
        return _answer(root, "Could not fetch the base branch", fetched)
    remote_tip = _read(root, "rev-parse", "--verify", "FETCH_HEAD")
    if remote_tip is None:
        return Result(False, "Could not read the base branch", _output(fetched))
    local_ref = f"refs/heads/{branch}"
    local_tip = _read(root, "rev-parse", "--verify", local_ref)
    if (
        local_tip
        and not _git(root, "merge-base", "--is-ancestor", local_tip, remote_tip).is_ok
    ):
        return Result(
            False, f"Local {branch} has commits that the remote does not have"
        )
    switched = _git(
        root, "switch", *(("--", branch) if local_tip else ("-c", branch, remote_tip))
    )
    if not switched.is_ok:
        return _answer(root, "Could not switch to the base branch", switched)
    aligned = _git(root, "merge", "--ff-only", remote_tip)
    if not aligned.is_ok:
        return _answer(root, "Could not update the base branch", aligned)
    merged = _git(root, "merge", "--ff", "--no-edit", f"refs/heads/{state.branch}")
    if not merged.is_ok:
        return _answer(root, "Merge stopped", merged)
    head = _read(root, "rev-parse", "--verify", local_ref)
    sent = _git(
        root, "push", "--set-upstream", "--", remote, f"{local_ref}:{local_ref}"
    )
    done = _answer(
        root,
        f"Merged and pushed {remote}/{branch}"
        if sent.is_ok
        else f"Merged locally. Push to {remote}/{branch} failed",
        merged,
        sent,
    )
    return _compact(journal, done, head) if head else done


def abort(journal: Journal) -> Result:
    """Abort an interrupted merge or rebase."""
    what = _operation(journal.root)
    if what not in ("rebase", "merge"):
        return Result(False, "No merge or rebase to abort")
    done = _git(journal.root, what, "--abort")
    return _answer(
        journal.root,
        f"Aborted {what}" if done.is_ok else f"Could not abort {what}",
        done,
    )


def create_pr(journal: Journal) -> Result:
    """Open a pull request with GitHub CLI, without interactive prompts."""
    state = status(journal)
    base = state.base.removeprefix("origin/") if state.base else None
    if not state.has_gh:
        return Result(False, "Install GitHub CLI to create a pull request")
    if not state.branch or not state.upstream or not base or state.branch == base:
        return Result(False, "Push a feature branch before creating a pull request")
    if state.ahead or state.behind or not state.ahead_of_base or state.operation:
        return Result(
            False, "Sync a branch with changes before creating a pull request"
        )
    head = state.upstream.partition("/")[2]
    done = _run(
        journal.root, "gh", "pr", "create", "--base", base, "--head", head, "--fill"
    )
    return _answer(
        journal.root,
        "Pull request created" if done.is_ok else "Could not create a pull request",
        done,
    )
