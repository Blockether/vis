"""Rules of the write journal: checkpoints, diffs and undo."""

import json
import os
import subprocess

import pytest
from vis_lang_interface.tree import walked

from vis_review import journal as journal_module
from vis_review import store
from vis_review.journal import Base, Journal, base_ref, member_key


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
    # "Uncommitted" scope (scenario S08): the old side is the commit.
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
    # Comparison "Automatic" (scenario S09).
    top = repo(tmp_path)
    first = git(top, "rev-parse", "HEAD").strip()
    git(top, "switch", "-q", "-c", "feature")
    (top / "sub" / "a.py").write_text("feature\n")
    git(top, "commit", "-q", "-am", "feature work")
    assert base_ref(str(top)) == Base(first, "main")
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


def _lines_with(**changed):
    return "".join(f"{changed.get(f'l{n}', f'line {n}')}\n" for n in range(1, 31))


@pytest.mark.parametrize("view", ["own_diff", "own_open"])
def test_two_sessions_that_change_one_file_see_only_their_own_lines(tmp_path, view):
    # Each session that changed the same file must show only its own changes.
    top = repo(tmp_path)
    (top / "sub" / "shared.py").write_text(_lines_with())
    git(top, "add", "-A")
    git(top, "commit", "-q", "-m", "shared")
    mine = Journal(
        top / "sub", "s1", tmp_path / "base", files=walked, clock=lambda: 1.0
    )
    theirs = Journal(
        top / "sub", "s2", tmp_path / "base", files=walked, clock=lambda: 1.0
    )
    mine.checkpoint()
    theirs.checkpoint()
    write(mine, "shared.py", _lines_with(l2="mine 2"))
    mine.checkpoint("block", "first own edit")
    theirs.checkpoint("outside", "Changes between blocks")
    write(theirs, "shared.py", _lines_with(l2="mine 2", l15="theirs 15"))
    theirs.checkpoint("block", "their edit")
    mine.checkpoint("outside", "Changes between blocks")
    write(mine, "shared.py", _lines_with(l2="mine 2", l15="theirs 15", l28="mine 28"))
    mine.checkpoint("block", "second own edit")
    theirs.checkpoint("outside", "Changes between blocks")

    def patch(j):
        diffs = j.own_diff() if view == "own_diff" else j.own_open("HEAD")
        assert [d.path for d in diffs] == ["shared.py"]
        return diffs[0]

    own, other = patch(mine), patch(theirs)
    assert "-line 2\n+mine 2\n" in own.patch
    assert "-line 28\n+mine 28\n" in own.patch
    assert "+theirs 15" not in own.patch
    assert (own.added, own.removed) == (2, 2)
    assert "-line 15\n+theirs 15\n" in other.patch
    assert "+mine" not in other.patch
    assert (other.added, other.removed) == (1, 1)


def test_undo_of_an_own_file_keeps_the_lines_of_another_session(tmp_path):
    # Undo file in "This session" brought the file back to before the first own
    # change, so it also took out the later edit of another session.
    mine, theirs = journal(tmp_path, "s1"), journal(tmp_path, "s2")
    write(mine, "shared.py", _lines_with())
    mine.checkpoint()
    theirs.checkpoint()
    write(mine, "shared.py", _lines_with(l2="mine 2"))
    mine.checkpoint("block", "own edit")
    theirs.checkpoint("outside", "Changes between blocks")
    write(theirs, "shared.py", _lines_with(l2="mine 2", l15="theirs 15"))
    theirs.checkpoint("block", "their edit")
    done = mine.undo_own("shared.py")
    assert done.restored == ("shared.py",)
    assert read(mine, "shared.py") == _lines_with(l15="theirs 15")
    assert mine.own_diff() == []
    assert mine.undo_own("shared.py").restored == ()
    write(theirs, "shared.py", _lines_with(l2="theirs 2", l15="theirs 15"))
    theirs.checkpoint("block", "their edit of line 2")
    write(mine, "shared.py", _lines_with(l2="mine again", l15="theirs 15"))
    mine.checkpoint("block", "own edit of their line")
    theirs.checkpoint("outside", "Changes between blocks")
    write(theirs, "shared.py", _lines_with(l2="theirs again", l15="theirs 15"))
    theirs.checkpoint("block", "their edit of the same line")
    assert mine.undo_own("shared.py").conflicts == ("shared.py",)
    assert read(mine, "shared.py") == _lines_with(l2="theirs again", l15="theirs 15")


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


def test_an_interrupted_checkpoint_loses_no_change(tmp_path, monkeypatch):
    # Session f183fc01: the 300 s limit interrupted the Review hook, and the
    # changes of that step never reached the journal.
    j = journal(tmp_path)
    write(j, "a.py", "v1\n")
    j.checkpoint()
    write(j, "a.py", "v2\n")
    real = Journal._append

    def interrupted(self, *args, **kwargs):
        monkeypatch.setattr(Journal, "_append", real)
        raise KeyboardInterrupt

    monkeypatch.setattr(Journal, "_append", interrupted)
    with pytest.raises(KeyboardInterrupt):
        j.checkpoint("block")
    entry = j.checkpoint("block")
    assert entry is not None and entry.files == {"a.py": store.digest(b"v2\n")}


def test_a_repaint_replays_only_the_new_checkpoints(tmp_path, monkeypatch):
    # Session f183fc01: each repaint merged all 3,372 changes of others again,
    # 35 s after every block.
    monkeypatch.setattr(journal_module, "MOST_CACHED_DIFFS", 0)
    j = journal(tmp_path)
    write(j, "shared.py", _lines_with())
    j.checkpoint()
    write(j, "shared.py", _lines_with(l2="mine 2"))
    j.checkpoint("block", "own edit")
    write(j, "shared.py", _lines_with(l2="mine 2", l15="theirs 15"))
    j.checkpoint("outside", "Changes between blocks")
    first = j.own_paths()
    merges = []
    real = Journal._merged
    monkeypatch.setattr(
        Journal, "_merged", lambda self, *a: merges.append(a) or real(self, *a)
    )
    write(j, "b.py", "b\n")
    j.checkpoint("block", "edit b")
    assert j.own_paths() == sorted([*first, "b.py"])
    assert merges == []


def _applied(old, hunks):
    out, at = [], 0
    for start, end, lines in hunks:
        out += old[at:start] + list(lines)
        at = end
    return out + old[at:]


def test_hunks_of_a_large_file_give_difflib_only_small_regions(monkeypatch):
    # Session f183fc01: difflib matched whole 15,000-line files, about 1.5 s each.
    # The first block after a gateway restart replayed 2,428 merges, and the
    # Review hook held the block for 300 s.
    old = [f"line {n}\n" if n % 3 else "\n" for n in range(15_000)]
    new = list(old)
    for n in range(10, 15_000, 500):
        new[n] = f"edited {n}\n"
    new.insert(7_000, "added\n")
    sizes = []
    real = journal_module.difflib.SequenceMatcher

    def counted(junk, a, b, **kwargs):
        sizes.append(len(a) * len(b))
        return real(junk, a, b, **kwargs)

    monkeypatch.setattr(journal_module.difflib, "SequenceMatcher", counted)
    assert _applied(old, journal_module._hunks(old, new)) == new
    assert max(sizes, default=0) <= journal_module.MOST_DIFFLIB_CELLS


@pytest.mark.parametrize("seed", range(200))
def test_hunks_rebuild_the_new_lines(seed):
    import random

    rnd = random.Random(seed)
    old = [rnd.choice("abcde\n") for _ in range(rnd.randint(0, 60))]
    new = [rnd.choice("abcdef\n") for _ in range(rnd.randint(0, 60))]
    hunks = journal_module._hunks(old, new)
    assert _applied(old, hunks) == new
    assert all(a[1] <= b[0] for a, b in zip(hunks, hunks[1:]))


def test_a_large_region_without_unique_lines_stays_one_hunk(monkeypatch):
    monkeypatch.setattr(journal_module, "MOST_DIFFLIB_CELLS", 10)
    old, new = ["x\n", "y\n"] * 4, ["y\n", "x\n"] * 4
    assert journal_module._hunks(old, new) == [(0, 8, tuple(new))]


def test_an_edit_of_another_session_merges_into_a_small_file():
    # Session f183fc01: anchors split each file first and saw a conflict that
    # difflib does not see, so the own view showed the lines of another session.
    base = ["a\n", "g\n", "c\n", "i\n"]
    ours = ["a\n", "p\n", "b\n", "g\n", "c\n", "i\n"]
    theirs = ["c\n", "a\n", "a\n", "i\n"]
    hunks = journal_module._hunks
    assert journal_module._merge(base, hunks(base, ours), hunks(base, theirs)) == [
        "c\n",
        "a\n",
        "p\n",
        "b\n",
        "a\n",
        "i\n",
    ]


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


def test_own_open_shows_only_the_own_files_that_the_base_does_not_have(tmp_path):
    # Session 27112fd6: "This session" showed 331 files, and main had 329 of them.
    top = repo(tmp_path)
    j = Journal(top / "sub", "s1", tmp_path / "base", files=walked, clock=lambda: 1.0)
    j.checkpoint()
    write(j, "a.py", "merged\n")
    j.checkpoint("block", "edit a")
    git(top, "commit", "-q", "-am", "merge a")
    write(j, "keep.py", "open\n")
    write(j, "new.py", "new\n")
    os.unlink(os.path.join(j.root, "gone.py"))
    j.checkpoint("block", "edit more")
    write(j, "theirs.py", "other session\n")
    j.checkpoint("outside", "Changes between blocks")
    diffs = {d.path: d for d in j.own_open("HEAD")}
    assert {p: d.status for p, d in diffs.items()} == {
        "gone.py": "deleted",
        "keep.py": "modified",
        "new.py": "added",
    }
    assert "-keep\n+open\n" in diffs["keep.py"].patch
    assert journal(tmp_path).own_open("HEAD") is None


def test_a_block_leaves_out_the_files_that_git_brought_in(tmp_path, monkeypatch):
    # Session fd5c4248: the block that ran `git pull` owned 24 files of other sessions.
    top = repo(tmp_path)
    git(top, "switch", "-q", "-c", "upstream")
    (top / "sub" / "a.py").write_text("upstream\n")
    monkeypatch.setenv("GIT_COMMITTER_DATE", "2001-01-01T00:00:00+0000")
    git(top, "commit", "-q", "-am", "work of another session")
    monkeypatch.delenv("GIT_COMMITTER_DATE")
    git(top, "switch", "-q", "main")
    j = Journal(top / "sub", "s1", tmp_path / "base", files=walked)
    j.checkpoint()
    j.checkpoint("outside", "Changes between blocks")
    write(j, "keep.py", "mine\n")
    git(top, "merge", "-q", "--ff-only", "upstream")
    entry = j.checkpoint("block", "git pull")
    assert (entry.kind, set(entry.files)) == ("block", {"keep.py"})
    assert [d.path for d in j.own_diff()] == ["keep.py"]
    assert j.store.get(j.state["a.py"]) == b"upstream\n"


def test_a_block_owns_the_file_that_it_commits(tmp_path):
    top = repo(tmp_path)
    j = Journal(top / "sub", "s1", tmp_path / "base", files=walked)
    j.checkpoint()
    j.checkpoint("outside", "Changes between blocks")
    write(j, "a.py", "mine\n")
    git(top, "commit", "-q", "-am", "my work")
    entry = j.checkpoint("block", "edit and commit")
    assert (entry.kind, set(entry.files)) == ("block", {"a.py"})


def test_a_block_leaves_out_a_change_that_another_session_recorded_first(tmp_path):
    # Session fd5c4248 owned CHANGELOG.md: another session wrote it while a long block ran.
    mine, theirs = journal(tmp_path, "s1"), journal(tmp_path, "s2")
    write(mine, "mine.py", "m1\n")
    mine.checkpoint()
    theirs.checkpoint()
    write(theirs, "theirs.py", "t1\n")
    theirs.checkpoint("block", "a short edit")
    write(mine, "mine.py", "m2\n")
    entry = mine.checkpoint("block", "a long block")
    assert (entry.kind, set(entry.files)) == ("block", {"mine.py"})
    assert [d.path for d in mine.own_diff()] == ["mine.py"]
    assert mine.store.get(mine.state["theirs.py"]) == b"t1\n"


def test_a_step_keeps_a_new_file_that_it_shows_it_wrote(tmp_path):
    # Two blocks ran together. The other session ended first and recorded both new
    # files as its own, so "This session" lost the file that the session made.
    mine, theirs = journal(tmp_path, "s1"), journal(tmp_path, "s2")
    write(mine, "base.py", "b\n")
    mine.checkpoint()
    theirs.checkpoint()
    write(mine, "mine_new.py", "m1\n")
    write(theirs, "theirs.py", "t1\n")
    theirs.checkpoint("block", "ended first")
    entry = mine.checkpoint(
        "block", "a long block", wrote=lambda rel: rel == "mine_new.py"
    )
    assert (entry.kind, set(entry.files)) == ("block", {"mine_new.py"})
    assert [d.path for d in mine.own_diff()] == ["mine_new.py"]


def test_a_session_keeps_a_file_that_it_changed_before(tmp_path):
    # Two blocks ran together and the other one ended first: an own file stays own.
    mine, theirs = journal(tmp_path, "s1"), journal(tmp_path, "s2")
    write(mine, "shared.py", "s1\n")
    mine.checkpoint()
    theirs.checkpoint()
    write(mine, "shared.py", "s2\n")
    mine.checkpoint("block", "the first own change")
    theirs.checkpoint("outside", "Changes between blocks")
    write(mine, "shared.py", "s3\n")
    theirs.checkpoint("block", "ended first")
    entry = mine.checkpoint("block", "the second own change")
    assert (entry.kind, set(entry.files)) == ("block", {"shared.py"})


def test_a_leader_owns_the_changes_that_its_subagents_recorded(tmp_path):
    # The review of the leader showed the work of its subagents as changes of others.
    leader = journal(tmp_path, "L")
    member = journal(tmp_path, member_key("L", "S"))
    other = journal(tmp_path, "s2")
    write(leader, "base.py", "b\n")
    for j in (leader, member, other):
        j.checkpoint()
    write(member, "base.py", "a1\n")
    member.checkpoint("block", "a subagent block")
    write(other, "other.py", "o1\n")
    other.checkpoint("block", "another session")
    write(leader, "mine.py", "m1\n")
    leader.checkpoint("block", "a long leader block")
    assert [(e.kind, set(e.files)) for e in leader.entries[1:]] == [
        ("team", {"base.py"}),
        ("outside", {"other.py"}),
        ("block", {"mine.py"}),
    ]
    assert leader.own_paths() == ["base.py", "mine.py"]
    assert member.own_paths() == ["base.py"]
    assert leader.undo_own("base.py").restored == ("base.py",)
    assert read(leader, "base.py") == "b\n"


def test_a_leader_owns_a_subagent_change_that_it_saw_before_the_subagent_recorded_it(
    tmp_path,
):
    # A subagent records its step when the step ends; the leader can see the file first.
    leader = journal(tmp_path, "L")
    member = journal(tmp_path, member_key("L", "S"))
    leader.checkpoint()
    member.checkpoint()
    write(member, "agent.py", "a1\n")
    entry = leader.checkpoint("outside", "Changes between blocks")
    assert entry.kind == "outside"
    assert leader.own_paths() == []
    member.checkpoint("block", "a subagent block ends")
    assert leader.own_paths() == ["agent.py"]


def test_a_leader_keeps_a_subagent_change_that_another_session_edited_before_its_scan(
    tmp_path,
):
    leader = journal(tmp_path, "L")
    member = journal(tmp_path, member_key("L", "S"))
    write(leader, "f.py", "a\nb\nc\n")
    leader.checkpoint()
    member.checkpoint()
    write(member, "f.py", "A\nb\nc\n")
    member.checkpoint("block", "a subagent block")
    write(member, "f.py", "A\nb\nC\n")
    leader.checkpoint("outside", "Changes between blocks")
    [diff] = leader.own_diff(context=0)
    assert "+A" in diff.patch and "+C" not in diff.patch


def test_a_leader_keeps_an_old_change_of_others_that_a_later_subagent_repeats(tmp_path):
    times = iter(range(1, 100))
    root = tmp_path / "root"
    root.mkdir()
    leader, member = (
        Journal(root, key, tmp_path / "base", files=walked, clock=lambda: next(times))
        for key in ("L", member_key("L", "S"))
    )
    write(leader, "f.py", "a\n")
    leader.checkpoint()
    write(leader, "f.py", "b\n")
    leader.checkpoint("outside", "Changes between blocks")
    write(leader, "f.py", "a\n")
    leader.checkpoint("outside", "Changes between blocks")
    member.checkpoint()
    write(member, "f.py", "b\n")
    member.checkpoint("block", "a subagent block")
    assert [e.kind for e in leader.entries] == ["baseline", "outside", "outside"]
    assert leader.own_paths() == []


def test_a_leader_owns_a_file_that_a_subagent_deleted_before_it_recorded_the_step(
    tmp_path,
):
    leader = journal(tmp_path, "L")
    member = journal(tmp_path, member_key("L", "S"))
    write(leader, "f.py", "a\n")
    leader.checkpoint()
    member.checkpoint()
    os.remove(os.path.join(member.root, "f.py"))
    leader.checkpoint("outside", "Changes between blocks")
    member.checkpoint("block", "a subagent block ends")
    assert leader.own_paths() == ["f.py"]


def test_a_subagent_step_starts_at_its_last_scan_and_not_at_its_last_change(tmp_path):
    times = iter(range(1, 100))
    root = tmp_path / "root"
    root.mkdir()
    leader, member = (
        Journal(root, key, tmp_path / "base", files=walked, clock=lambda: next(times))
        for key in ("L", member_key("L", "S"))
    )
    write(leader, "f.py", "a\n")
    leader.checkpoint()
    member.checkpoint()
    write(leader, "f.py", "b\n")
    leader.checkpoint("outside", "Changes between blocks")
    write(leader, "f.py", "a\n")
    leader.checkpoint("outside", "Changes between blocks")
    # The before-hook of an idle subagent block saves no checkpoint, but it scans.
    assert member.checkpoint("outside", "Changes between blocks") is None
    write(member, "f.py", "b\n")
    member.checkpoint("block", "a subagent block")
    assert leader.own_paths() == []
