"""Decisions of the review app: what it shows, and what each control changes."""

import os
import time
from types import SimpleNamespace

from vis_lang_interface.tree import walked

from vis_review.ai import AiFinding, AiReview
from vis_review.app import ReviewApp, extension_opener
from vis_review.journal import Journal


def setup(tmp_path, recorder, **doors):
    root = tmp_path / "root"
    root.mkdir()
    (root / "a.py").write_text("a = 1\n")
    journal = Journal(root, "s1", tmp_path / "base", files=walked)
    app = ReviewApp(journal, extension_opener, **doors).open()
    return root, journal, app


def edit(root, rel, text, kind="block"):
    path = root / rel
    path.write_text(text)
    stat = os.stat(path)
    os.utime(path, ns=(stat.st_atime_ns, stat.st_mtime_ns + 1_000_000_000))


def wait_for(check, timeout=2.0):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        if check():
            return True
        time.sleep(0.01)
    return check()


def rows(recorder, node_id):
    return [row["id"] for row in recorder.node(node_id)["rows"]]


def test_a_new_change_shows_after_a_checkpoint_and_nodes_keep_their_place(
    tmp_path, recorder
):
    root, journal, app = setup(tmp_path, recorder)
    order = recorder.ids()
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block", "edit a")
    app.refresh()
    assert rows(recorder, "files") == ["a.py"]
    assert "+a = 2" in recorder.node("diff")["patch"]
    assert recorder.ids() == order


def test_choosing_a_checkpoint_shows_only_its_own_change(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    edit(root, "b.py", "b = 1\n")
    second = journal.checkpoint("block")
    recorder.send("select", "checkpoints", item_ids=[str(second.seq)])
    assert wait_for(
        lambda: (
            "b.py" in recorder.node("diff")["patch"]
            and "a.py" not in recorder.node("diff")["patch"]
        )
    )
    recorder.send("select", "checkpoints", item_ids=[str(second.seq)])
    assert wait_for(lambda: "a.py" in recorder.node("diff")["patch"])


def test_undo_checkpoint_changes_the_files_back(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    second = journal.checkpoint("block")
    recorder.send("select", "checkpoints", item_ids=[str(second.seq)])
    recorder.send("activate", "undo")
    assert wait_for(lambda: (root / "a.py").read_text() == "a = 1\n")
    assert wait_for(lambda: journal.entries[-1].kind == "undo")


def test_undo_file_brings_back_only_the_chosen_file(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    edit(root, "b.py", "b = 1\n")
    journal.checkpoint("block")
    app.refresh()
    recorder.send("select", "files", item_ids=["b.py"])
    recorder.send("activate", "undo-file")
    assert wait_for(lambda: not (root / "b.py").exists())
    assert (root / "a.py").read_text() == "a = 2\n"
    # vis-review e2e: the gateway refused the repaint, because b.py stayed selected.
    assert wait_for(lambda: journal.entries[-1].kind == "undo")
    assert wait_for(lambda: recorder.node("notice")["tone"] == "ok")
    files = recorder.node("files")
    assert set(files.get("selected_ids", [])) <= {row["id"] for row in files["rows"]}


def test_checks_show_findings_as_rows_and_as_threads_on_their_lines(tmp_path, recorder):
    lint = SimpleNamespace(
        diagnostics=[
            SimpleNamespace(
                path=str(tmp_path / "root" / "a.py"),
                line=1,
                level="error",
                message="bad",
                rule="E1",
            )
        ]
    )

    def call(tool, verb, **_):
        return lint if verb == "lint_code" else SimpleNamespace(changed=[])

    root, journal, app = setup(tmp_path, recorder, call=call)
    edit(root, "a.py", "a = 2  \n")
    journal.checkpoint("block")
    recorder.send("activate", "checks")
    assert wait_for(lambda: len(rows(recorder, "findings")) == 2)
    threads = recorder.node("diff")["threads"]
    assert {
        (t["anchor"]["line"], t["messages"][0]["body"].split(":")[0]) for t in threads
    } == {(1, "error lint"), (1, "warning trailing-whitespace")}


def test_the_ai_review_shows_its_summary_and_findings(tmp_path, recorder):
    def ask(request, model):
        return AiReview(
            summary="Looks risky.",
            findings=[
                AiFinding(path="a.py", line=1, level="warning", message="Check this.")
            ],
        )

    root, journal, app = setup(tmp_path, recorder, ask=ask)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    recorder.send("activate", "ai")
    assert wait_for(lambda: recorder.node("notice").get("detail") == "Looks risky.")
    assert rows(recorder, "findings") == ["f0"]


def test_sent_comments_go_to_the_session_and_answer_the_submission(tmp_path, recorder):
    sent = []
    root, journal, app = setup(tmp_path, recorder, send=sent.append)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    app.refresh()
    recorder.send(
        "submit",
        "diff",
        submission_id="sub-1",
        comments=[
            {
                "node_id": "diff",
                "thread_id": "t1",
                "message_id": "m1",
                "body": "Why 2?",
                "anchor": {"path": "a.py", "side": "new", "line": 1},
            }
        ],
    )
    assert wait_for(lambda: sent)
    assert "- a.py:1: Why 2?" in sent[0]


def test_controls_that_need_a_door_feature_are_off_without_it(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    app.refresh()
    buttons = {b["id"]: b["is_disabled"] for b in recorder.node("actions")["children"]}
    assert buttons["ai"] and buttons["tests"] and not buttons["checks"]


def test_a_block_checkpoint_does_not_wait_for_a_running_ai_review(tmp_path, recorder):
    # A block hook refreshes the app; it must not wait the minutes an AI review takes.
    import threading

    started, release = threading.Event(), threading.Event()

    def ask(request, model):
        started.set()
        release.wait(5)
        return AiReview(summary="ok", findings=[])

    root, journal, app = setup(tmp_path, recorder, ask=ask)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    worker = threading.Thread(
        target=app.dispatch, args=({"type": "activate", "node_id": "ai"},)
    )
    worker.start()
    assert started.wait(2)
    refreshed = threading.Thread(target=app.refresh)
    refreshed.start()
    refreshed.join(1)
    assert not refreshed.is_alive()
    app.dispatch({"type": "activate", "node_id": "ai"})
    assert "still running" in recorder.node("notice")["text"]
    release.set()
    worker.join(5)
    assert wait_for(lambda: "AI review done" in recorder.node("notice")["text"])
