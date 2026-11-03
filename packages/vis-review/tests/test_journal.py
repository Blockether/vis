"""Rules of the write journal: checkpoints, diffs and undo."""

import json
import os
import subprocess

import pytest
from vis_lang_interface.tree import walked

from vis_review import journal as journal_module
from vis_review import store
from vis_review.journal import Journal, base_ref


def journal(tmp_path, key="s1"):
    root = tmp_path / "root"
    root.mkdir(exist_ok=True)
    return Journal(root, key, tmp_path / "base", files=walked, clock=lambda: 1.0)


def write(j, rel, text):
    path = os.path.join(j.root, rel)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as out:
        out.write(text)
    # Stamps are (mtime_ns, size): move the mtime so an equal size still shows.
    stat = os.stat(path)
    os.utime(path, ns=(stat.st_atime_ns, stat.st_mtime_ns + 1_000_000_000))


def read(j, rel):
    with open(os.path.join(j.root, rel)) as source:
        return source.read()


def test_the_first_checkpoint_is_a_baseline_of_every_file(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a\n")
    write(j, "src/b.py", "b\n")
    entry = j.checkpoint("block", "first")
    assert entry.kind == "baseline"
    assert set(entry.files) == {"a.py", "src/b.py"}
    assert j.checkpoint("block") is None


def test_a_checkpoint_records_only_the_changes(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a\n")
    write(j, "gone.py", "x\n")
    j.checkpoint()
    write(j, "a.py", "a2\n")
    os.unlink(os.path.join(j.root, "gone.py"))
    write(j, "new/deep/c.py", "c\n")
    entry = j.checkpoint("block", "edit")
    assert entry.kind == "block"
    assert entry.files["gone.py"] is None
    assert set(entry.files) == {"a.py", "gone.py", "new/deep/c.py"}


def test_a_touch_without_a_content_change_records_nothing(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a\n")
    j.checkpoint()
    write(j, "a.py", "a\n")
    assert j.checkpoint("block") is None


def test_a_diff_counts_lines_and_gives_a_git_patch(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "one\ntwo\n")
    j.checkpoint()
    write(j, "a.py", "one\nTWO\nthree\n")
    write(j, "b.py", "new\n")
    j.checkpoint("block")
    diffs = {d.path: d for d in j.diff(1)}
    assert (diffs["a.py"].status, diffs["a.py"].added, diffs["a.py"].removed) == (
        "modified",
        2,
        1,
    )
    assert diffs["a.py"].patch.startswith(
        "diff --git a/a.py b/a.py\n--- a/a.py\n+++ b/a.py\n"
    )
    assert "+TWO\n" in diffs["a.py"].patch
    assert diffs["b.py"].status == "added"
    assert "--- /dev/null\n" in diffs["b.py"].patch


def test_a_binary_file_shows_no_lines(tmp_path):
    j = journal(tmp_path)
    j.checkpoint()
    with open(os.path.join(j.root, "x.bin"), "wb") as out:
        out.write(b"\0\1\2")
    j.checkpoint("block")
    (d,) = j.diff(1)
    assert d.is_binary and "Binary files differ" in d.patch


def test_restore_brings_back_a_checkpoint_and_can_be_undone(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "v1\n")
    j.checkpoint()
    write(j, "a.py", "v2\n")
    write(j, "b.py", "b\n")
    j.checkpoint("block")
    done = j.restore(1)
    assert read(j, "a.py") == "v1\n"
    assert not os.path.exists(os.path.join(j.root, "b.py"))
    assert done.entry.kind == "undo" and set(done.restored) == {"a.py", "b.py"}
    assert j.checkpoint("block") is None
    j.restore(2)
    assert read(j, "a.py") == "v2\n" and read(j, "b.py") == "b\n"


def test_a_repeated_restore_changes_nothing(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "v1\n")
    j.checkpoint()
    write(j, "a.py", "v2\n")
    j.checkpoint("block")
    j.restore(1)
    again = j.restore(1)
    assert again.entry is None and again.restored == ()


def test_an_undo_first_keeps_the_changes_no_checkpoint_holds(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "v1\n")
    j.checkpoint()
    write(j, "a.py", "by hand\n")
    j.restore(1)
    outside = [e for e in j.entries if e.kind == "outside"]
    assert len(outside) == 1
    j.restore(outside[0].seq)
    assert read(j, "a.py") == "by hand\n"


def test_restore_limited_to_paths_leaves_other_files(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a1\n")
    write(j, "b.py", "b1\n")
    j.checkpoint()
    write(j, "a.py", "a2\n")
    write(j, "b.py", "b2\n")
    j.checkpoint("block")
    j.restore(1, ["a.py"])
    assert (read(j, "a.py"), read(j, "b.py")) == ("a1\n", "b2\n")


def test_revert_undoes_one_checkpoint_and_keeps_later_ones(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a1\n")
    write(j, "b.py", "b1\n")
    j.checkpoint()
    write(j, "a.py", "a2\n")
    second = j.checkpoint("block")
    write(j, "b.py", "b2\n")
    j.checkpoint("block")
    done = j.revert(second.seq)
    assert (read(j, "a.py"), read(j, "b.py")) == ("a1\n", "b2\n")
    assert done.restored == ("a.py",)
    assert j.revert(second.seq).entry is None


def test_revert_leaves_a_file_that_changed_later_as_a_conflict(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a1\n")
    j.checkpoint()
    write(j, "a.py", "a2\n")
    second = j.checkpoint("block")
    write(j, "a.py", "a3\n")
    j.checkpoint("block")
    done = j.revert(second.seq)
    assert done.conflicts == ("a.py",) and done.entry is None
    assert read(j, "a.py") == "a3\n"


def test_revert_refuses_the_baseline(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a\n")
    j.checkpoint()
    with pytest.raises(ValueError):
        j.revert(1)


def test_a_new_process_reads_the_journal_and_sees_changes_made_while_away(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a1\n")
    write(j, "b.py", "b\n")
    j.checkpoint()
    write(j, "a.py", "a2\n")
    os.unlink(os.path.join(j.root, "b.py"))
    again = journal(tmp_path)
    assert again.last == 1
    entry = again.checkpoint("outside")
    assert entry.files == {"a.py": store.digest(b"a2\n"), "b.py": None}


def test_a_line_cut_short_by_an_interrupted_write_is_skipped(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a\n")
    j.checkpoint()
    with open(j.path, "a") as out:
        out.write(json.dumps({"seq": 2, "at": 1})[:-3])
    again = journal(tmp_path)
    assert again.last == 1
    write(again, "a.py", "b\n")
    assert again.checkpoint("block").seq == 2


def test_a_file_too_large_to_keep_is_missing_from_an_undo(tmp_path, monkeypatch):
    monkeypatch.setattr(store, "MOST_BYTES", 4)
    j = journal(tmp_path)
    write(j, "big.txt", "large content\n")
    j.checkpoint()
    write(j, "big.txt", "changed content\n")
    j.checkpoint("block")
    done = j.restore(1)
    assert done.missing == ("big.txt",) and done.entry is None
    assert read(j, "big.txt") == "changed content\n"


def test_two_keys_on_one_root_keep_two_journals_and_share_blobs(tmp_path):
    a = journal(tmp_path, "s1")
    write(a, "a.py", "same\n")
    a.checkpoint()
    b = journal(tmp_path, "s2")
    b.checkpoint()
    assert a.path != b.path
    assert a.state == b.state


def git(cwd, *args):
    env = {
        **os.environ,
        "GIT_CONFIG_GLOBAL": os.devnull,
        "GIT_AUTHOR_NAME": "t",
        "GIT_AUTHOR_EMAIL": "t@example.com",
        "GIT_COMMITTER_NAME": "t",
        "GIT_COMMITTER_EMAIL": "t@example.com",
    }
    done = subprocess.run(
        ["git", *args], cwd=cwd, env=env, check=True, capture_output=True
    )
    return done.stdout.decode()


def repo(tmp_path):
    """A git repository with one commit on main, holding text and a picture."""
    top = tmp_path / "repo"
    (top / "sub").mkdir(parents=True)
    git(top, "init", "-q", "-b", "main")
    (top / ".gitignore").write_text("*.log\n")
    (top / "sub" / "a.py").write_text("one\n")
    (top / "sub" / "gone.py").write_text("gone\n")
    (top / "sub" / "keep.py").write_text("keep\n")
    (top / "sub" / "logo.bin").write_bytes(b"\x89PNG\0old")
    (top / "top.py").write_text("outside the root\n")
    git(top, "add", "-A")
    git(top, "commit", "-q", "-m", "first")
    return top


def test_against_head_shows_each_uncommitted_file_with_its_committed_content(
    tmp_path,
):
    # T3 Code "Uncommitted" scope (scenario S08): the old side is the commit.
    top = repo(tmp_path)
    sub = top / "sub"
    (sub / "a.py").write_text("two\n")
    (sub / "gone.py").unlink()
    (sub / "new.py").write_text("new\n")
    (sub / "logo.bin").write_bytes(b"\x89PNG\0new")
    (sub / "noise.log").write_text("ignored\n")
    (sub / "staged.py").write_text("staged\n")
    git(top, "add", "sub/staged.py")
    (top / "top.py").write_text("changed outside\n")
    index = (top / ".git" / "index").read_bytes()
    j = Journal(sub, "s1", tmp_path / "base", files=walked, clock=lambda: 1.0)
    j.checkpoint()
    diffs = {d.path: d for d in j.against("HEAD")}
    assert {p: d.status for p, d in diffs.items()} == {
        "a.py": "modified",
        "gone.py": "deleted",
        "logo.bin": "modified",
        "new.py": "added",
        "staged.py": "added",
    }
    assert "-one\n+two\n" in diffs["a.py"].patch
    assert diffs["logo.bin"].is_binary
    assert j.store.get(diffs["logo.bin"].before) == b"\x89PNG\0old"
    assert (top / ".git" / "index").read_bytes() == index


def test_against_is_none_without_a_git_commit(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "a\n")
    j.checkpoint()
    assert j.against("HEAD") is None
    top = repo(tmp_path)
    inside = Journal(top, "s2", tmp_path / "base", files=walked)
    assert inside.against("no-such-branch") is None


def test_the_base_branch_is_where_head_left_main(tmp_path):
    # T3 Code comparison "Automatic" (scenario S09).
    top = repo(tmp_path)
    first = git(top, "rev-parse", "HEAD").strip()
    git(top, "switch", "-q", "-c", "feature")
    (top / "sub" / "a.py").write_text("feature\n")
    git(top, "commit", "-q", "-am", "feature work")
    assert base_ref(str(top)) == first
    git(top, "switch", "-q", "--orphan", "lonely")
    assert base_ref(str(top)) is None


def test_own_diff_leaves_out_the_edits_of_other_sessions(tmp_path):
    # Goal dcc7197a: the review of one session showed the edits that another
    # session made in the same root, recorded as "outside" checkpoints.
    j = journal(tmp_path)
    write(j, "mine.py", "m1\n")
    write(j, "shared.py", "s1\n")
    j.checkpoint()
    write(j, "theirs.py", "other session\n")
    write(j, "shared.py", "s2 by them\n")
    j.checkpoint("outside", "Changes between blocks")
    write(j, "mine.py", "m2\n")
    write(j, "shared.py", "s3 by me\n")
    j.checkpoint("block", "edit")
    write(j, "theirs.py", "other session again\n")
    write(j, "mine.py", "m3 by them\n")
    j.checkpoint("outside", "Changes between blocks")
    diffs = {d.path: d for d in j.own_diff()}
    assert set(diffs) == {"mine.py", "shared.py"}
    assert "-m1\n+m2\n" in diffs["mine.py"].patch
    assert "-s2 by them\n+s3 by me\n" in diffs["shared.py"].patch
    assert j.own_start("shared.py") == 2


def test_an_undo_in_the_session_takes_its_change_out_of_the_own_diff(tmp_path):
    j = journal(tmp_path)
    write(j, "a.py", "v1\n")
    j.checkpoint()
    write(j, "a.py", "v2\n")
    j.checkpoint("block")
    j.restore(1, ["a.py"])
    assert j.own_diff() == []


@pytest.mark.parametrize(
    ("status", "old", "new"),
    [
        ("modified", "a\nb\nc\nd\n", "a\nB\nc\nd\ntail"),
        ("added", "", "x\ny\n"),
        ("deleted", "x\ny\n", ""),
    ],
)
def test_git_and_difflib_give_the_same_patch(monkeypatch, status, old, new):
    # Goal dcc7197a: difflib took 2.5 s for 5,000 changed lines, so git diffs
    # large files. Both paths must give the same patch text.
    monkeypatch.setattr(journal_module, "GIT_DIFF_LINES", 10**9)
    slow = journal_module.unified("f.py", status, old, new, 3)
    monkeypatch.setattr(journal_module, "GIT_DIFF_LINES", 0)
    assert journal_module.unified("f.py", status, old, new, 3) == slow


def test_a_repaint_reuses_the_diff_of_the_same_contents(tmp_path, monkeypatch):
    # Goal dcc7197a: each paint diffed every file again, so each click waited.
    j = journal(tmp_path)
    write(j, "a.py", "a\n")
    j.checkpoint()
    write(j, "a.py", "b\n")
    j.checkpoint("block")
    first = j.diff(1)
    monkeypatch.setattr(journal_module, "unified", None)
    assert j.diff(1) == first
