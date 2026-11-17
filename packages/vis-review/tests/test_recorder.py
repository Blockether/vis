"""Decisions of the recorder: one journal per session and root, and the labels."""

from types import SimpleNamespace

from vis_lang_interface.tree import walked

from vis_review.recorder import (
    Recorder,
    block_label,
    block_wrote,
    patch_label,
    patch_wrote,
)


def test_each_root_of_a_session_has_its_own_journal_and_a_session_reuses_it(tmp_path):
    first, draft = tmp_path / "root", tmp_path / "draft"
    first.mkdir()
    draft.mkdir()
    recorder = Recorder(tmp_path / "base", files=walked)
    assert recorder.journal("s1", first) is recorder.journal("s1", str(first) + "/")
    assert recorder.journal("s1", first) is not recorder.journal("s1", draft)
    assert recorder.journal("s1", first) is not recorder.journal("s2", first)


def test_a_checkpoint_with_a_change_repaints_the_open_app_and_one_without_does_not(
    tmp_path,
):
    root = tmp_path / "root"
    root.mkdir()
    (root / "a.py").write_text("a = 1\n")
    recorder = Recorder(tmp_path / "base", files=walked)
    painted = []
    journal = recorder.journal("s1", root)
    recorder.keep(
        "s1",
        SimpleNamespace(
            journal=journal,
            refresh=lambda: painted.append(1),
            is_behind=lambda: False,
        ),
    )
    assert recorder.record("s1", root, "block", "first").kind == "baseline"
    assert recorder.record("s1", root, "block", "nothing") is None
    assert painted == [1]


def test_a_step_keeps_the_file_that_its_code_or_its_patch_names(tmp_path):
    # Two sessions work in one root and their steps run together. The other session
    # ends first each time, so its journal records each new file as its own.
    root = tmp_path / "root"
    root.mkdir()
    (root / "base.py").write_text("b = 1\n")
    recorder = Recorder(tmp_path / "base", files=walked)
    recorder.record("s1", root, "block", "first")
    recorder.record("s2", root, "block", "first")
    (root / "src").mkdir()
    (root / "src" / "made.py").write_text("m = 1\n")
    (root / "data.py").write_text("d = 1\n")
    recorder.record("s2", root, "block", "ended first")
    code = 'path = root / "src" / "made.py"\npath.write_text("m = 1\\n")'
    block = recorder.record(
        "s1", root, "block", "a long block", block_wrote([{"code": code}])
    )
    assert (block.kind, set(block.files)) == ("block", {"src/made.py"})
    (root / "patched.py").write_text("p = 1\n")
    (root / "other.py").write_text("o = 1\n")
    recorder.record("s2", root, "block", "ended first again")
    patch = recorder.record(
        "s1",
        root,
        "patch",
        "Patch patched.py",
        patch_wrote([str(root / "patched.py"), []], str(root)),
    )
    assert (patch.kind, set(patch.files)) == ("patch", {"patched.py"})
    own = [d.path for d in recorder.journal("s1", root).own_diff()]
    assert own == ["patched.py", "src/made.py"]


def test_a_block_label_is_its_first_line_of_code_that_is_not_a_comment():
    code = "# next step\n\n  print(cat(path))\nmore()"
    assert block_label([{"code": code}]) == "print(cat(path))"
    assert block_label([{"code": "# only a comment"}]) == "Code block"
    assert block_label([]) == "Code block"


def test_a_patch_label_names_the_path_under_the_root(tmp_path):
    assert (
        patch_label([str(tmp_path / "src" / "a.py"), []], str(tmp_path))
        == "Patch src/a.py"
    )
    assert patch_label(["src/a.py"], str(tmp_path)) == "Patch src/a.py"
    assert patch_label([{"path": 1}], str(tmp_path)) == "Patch"
