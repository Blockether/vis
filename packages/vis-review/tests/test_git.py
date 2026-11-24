"""Git actions against a work tree and a bare remote, without real network writes."""

import os
import subprocess
from types import SimpleNamespace

import pytest

from vis_review import git


def command(root, *args, check=True):
    return subprocess.run(
        ["git", *args],
        cwd=root,
        env={**os.environ, "GIT_CONFIG_GLOBAL": os.devnull},
        capture_output=True,
        text=True,
        check=check,
    ).stdout.strip()


@pytest.fixture
def repo(tmp_path, monkeypatch):
    monkeypatch.setenv("GIT_CONFIG_GLOBAL", os.devnull)
    monkeypatch.setenv("GIT_CONFIG_NOSYSTEM", "1")
    root = tmp_path / "work"
    root.mkdir()
    command(root, "init", "-q", "-b", "main")
    command(root, "config", "user.name", "Test Person")
    command(root, "config", "user.email", "test@example.com")
    (root / "shared").write_text("original\n")
    (root / "other").write_text("original\n")
    command(root, "add", ".")
    command(root, "commit", "-qm", "Baseline")
    remote = tmp_path / "remote.git"
    command(root, "init", "-q", "--bare", "-b", "main", str(remote))
    command(root, "remote", "add", "origin", str(remote))
    command(root, "push", "-qu", "origin", "main")
    own, marked = [], []
    journal = SimpleNamespace(
        root=str(root), own_paths=lambda: own.copy(), mark_merged=marked.append
    )
    return root, remote, journal, own, marked


def write(root, name, text):
    (root / name).write_text(text + "\n")


def test_commit_keeps_unrelated_staged_and_untracked_paths_out(repo):
    root, _, journal, own, _ = repo
    own[:] = ["shared", ":(glob)*", "gone"]
    write(root, "other", "their staged edit")
    command(root, "add", "other")
    command(root, "rm", "shared")
    write(root, ":(glob)*", "literal own path")
    write(root, "untracked", "their untracked file")
    state = git.status(journal)
    assert state.dirty and state.own_dirty
    assert (state.branch, state.upstream, state.ahead, state.behind) == (
        "main",
        "origin/main",
        0,
        0,
    )
    done = git.commit(journal, "Change two files")
    assert done.ok, done
    assert set(
        command(
            root, "diff-tree", "--no-commit-id", "--name-only", "-r", "HEAD"
        ).splitlines()
    ) == {"shared", ":(glob)*"}
    assert command(root, "diff", "--cached", "--name-only") == "other"
    assert git.status(journal).ahead == 1


def test_commit_and_push_on_base_marks_only_changes_that_arrived(repo):
    root, remote, journal, own, marked = repo
    own.append("shared")
    write(root, "shared", "mine")
    done = git.commit_and_push(journal, "My change")
    assert done.ok, done
    assert marked == [command(remote, "rev-parse", "main")]
    assert command(remote, "show", "main:shared") == "mine"


def test_push_to_new_branch_sets_upstream_and_does_not_compact(repo):
    root, remote, journal, own, marked = repo
    command(root, "switch", "-qc", "topic")
    own.append("shared")
    write(root, "shared", "mine")
    done = git.commit(journal, "My change")
    assert done.ok, done
    assert not git.push(journal).ok
    assert not git.push_to_branch(journal, "--help").ok
    done = git.push_to_branch(journal, "review/my-topic")
    assert done.ok, done
    assert git.status(journal).upstream == "origin/review/my-topic"
    assert command(remote, "show", "review/my-topic:shared") == "mine"
    assert marked == []
    write(root, "shared", "more")
    done = git.commit_and_push(journal, "More changes")
    assert done.ok, done
    assert command(remote, "show", "review/my-topic:shared") == "more"


def advance_base(repo, tmp_path, path="other"):
    _, remote, _, _, _ = repo
    peer = tmp_path / "peer"
    command(tmp_path, "clone", "-q", str(remote), str(peer))
    command(peer, "config", "user.name", "Peer")
    command(peer, "config", "user.email", "peer@example.com")
    write(peer, path, "theirs")
    command(peer, "commit", "-qam", "Their change")
    command(peer, "push", "-q")
    return command(peer, "rev-parse", "HEAD")


def feature(repo):
    root, _, journal, own, _ = repo
    command(root, "switch", "-qc", "topic")
    own.append("shared")
    write(root, "shared", "mine")
    result = git.commit(journal, "My change")
    assert result.ok, result
    return command(root, "rev-parse", "HEAD")


def test_push_refuses_a_non_fast_forward_update(repo, tmp_path):
    root, remote, journal, own, marked = repo
    tip = advance_base(repo, tmp_path)
    own.append("shared")
    write(root, "shared", "mine")
    result = git.commit_and_push(journal, "My change")
    assert not result.ok
    assert command(root, "show", "HEAD:shared") == "mine"
    assert command(remote, "rev-parse", "main") == tip
    assert not marked


def test_pull_rebases_local_commits(repo, tmp_path):
    root, _, journal, own, _ = repo
    tip = advance_base(repo, tmp_path)
    own.append("shared")
    write(root, "shared", "mine")
    committed = git.commit(journal, "My change")
    assert committed.ok, committed
    done = git.pull(journal)
    assert done.ok, done
    assert command(root, "rev-parse", "HEAD^") == tip
    assert (root / "shared").read_text() == "mine\n"


def test_rebase_stops_on_conflicts_and_abort_restores_the_branch(repo, tmp_path):
    root, _, journal, _, _ = repo
    before = feature(repo)
    advance_base(repo, tmp_path, "shared")
    command(root, "fetch", "-q", "origin")
    done = git.rebase(journal)
    assert not done.ok and done.conflicts == ("shared",)
    assert git.status(journal).operation == "rebase"
    aborted = git.abort(journal)
    assert aborted.ok, aborted
    assert git.status(journal).branch == "topic"
    assert command(root, "rev-parse", "HEAD") == before
    assert (root / "shared").read_text() == "mine\n"


@pytest.mark.parametrize("diverged", [False, True])
def test_merge_pushes_the_base_and_marks_its_tip(repo, tmp_path, diverged):
    root, remote, journal, _, marked = repo
    topic = feature(repo)
    tip = advance_base(repo, tmp_path) if diverged else None
    done = git.merge(journal)
    assert done.ok, done
    landed = command(remote, "rev-parse", "main")
    assert marked == [landed]
    assert git.status(journal).branch == "main"
    assert command(remote, "show", "main:shared") == "mine"
    if diverged:
        assert command(root, "show", "-s", "--format=%P", "HEAD").split() == [
            tip,
            topic,
        ]
    else:
        assert landed == topic


def test_merge_conflict_does_not_push_and_can_be_aborted(repo, tmp_path):
    root, remote, journal, _, marked = repo
    feature(repo)
    tip = advance_base(repo, tmp_path, "shared")
    done = git.merge(journal)
    assert not done.ok and done.conflicts == ("shared",)
    assert git.status(journal).operation == "merge"
    assert not marked
    assert command(remote, "rev-parse", "main") == tip
    aborted = git.abort(journal)
    assert aborted.ok, aborted
    assert (root / "shared").read_text() == "theirs\n"


def test_create_pr_uses_the_pushed_head_and_base(repo, tmp_path, monkeypatch):
    root, _, journal, _, _ = repo
    feature(repo)
    pushed = git.push_to_branch(journal, "review/topic")
    assert pushed.ok, pushed
    fake = tmp_path / "gh"
    fake.write_text('#!/bin/sh\nprintf "%s\\n" "$@"\n')
    fake.chmod(0o700)
    monkeypatch.setenv("PATH", f"{tmp_path}{os.pathsep}{os.environ['PATH']}")
    done = git.create_pr(journal)
    assert done.ok, done
    assert done.output.splitlines() == [
        "pr",
        "create",
        "--base",
        "main",
        "--head",
        "review/topic",
        "--fill",
    ]
    assert git.status(journal).branch == "topic"
