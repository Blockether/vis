"""Decisions of the review app: what it shows, and what each control changes."""

import os
import subprocess
import time
from types import SimpleNamespace

import blockether.vis.extension as vis
from vis_lang_interface.tree import walked

from vis_review.ai import AiFinding, AiReview
from vis_review.app import ReviewApp, extension_opener, image_node
from vis_review.journal import FileDiff, Journal


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


def items(recorder, menu_id):
    return [child["id"] for child in recorder.node(menu_id)["children"]]


def controls(recorder):
    """Every button of the diff toolbar by id, the buttons in its menus and submenus included."""
    found = {}
    nodes = list(recorder.node("diff")["children"])
    while nodes:
        node = nodes.pop()
        if node["type"] == "group":
            nodes.extend(node["children"])
        else:
            found[node["id"]] = node["is_disabled"]
    return found


def test_a_new_change_shows_after_a_checkpoint_and_nodes_keep_their_place(
    tmp_path, recorder
):
    root, journal, app = setup(tmp_path, recorder)
    order = recorder.ids()
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block", "edit a")
    app.refresh()
    assert items(recorder, "file") == ["file-all", "file:a.py"]
    assert "+a = 2" in recorder.node("diff")["patch"]
    assert recorder.ids() == order


def test_choosing_a_checkpoint_shows_only_its_own_change(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    edit(root, "b.py", "b = 1\n")
    second = journal.checkpoint("block")
    recorder.send("activate", f"checkpoint-{second.seq}")
    assert wait_for(
        lambda: (
            "b.py" in recorder.node("diff")["patch"]
            and "a.py" not in recorder.node("diff")["patch"]
        )
    )
    assert recorder.node("scope")["label"] == f"Checkpoint #{second.seq}"
    recorder.send("activate", f"checkpoint-{second.seq}")
    assert wait_for(lambda: "a.py" in recorder.node("diff")["patch"])


def test_undo_checkpoint_changes_the_files_back(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    second = journal.checkpoint("block")
    recorder.send("activate", f"checkpoint-{second.seq}")
    recorder.send("activate", "undo")
    assert wait_for(lambda: (root / "a.py").read_text() == "a = 1\n")
    assert wait_for(lambda: journal.entries[-1].kind == "undo")


def test_undo_file_brings_back_only_the_chosen_file(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    edit(root, "b.py", "b = 1\n")
    journal.checkpoint("block")
    app.refresh()
    recorder.send("activate", "file:b.py")
    recorder.send("activate", "undo-file")
    assert wait_for(lambda: not (root / "b.py").exists())
    assert (root / "a.py").read_text() == "a = 2\n"
    # vis-review e2e: the gateway refused the repaint, because b.py stayed selected.
    assert wait_for(lambda: journal.entries[-1].kind == "undo")
    assert wait_for(lambda: recorder.node("notice")["tone"] == "ok")
    assert recorder.node("file")["label"] == "All files"


def test_checks_show_findings_as_threads_on_their_lines(tmp_path, recorder):
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
    assert wait_for(lambda: len(recorder.node("diff").get("threads") or []) == 2)
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
    [thread] = recorder.node("diff")["threads"]
    assert thread["messages"][0]["body"].endswith("Check this.")


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
    buttons = controls(recorder)
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


def png(width, height, color=b"\x00\x00\x00"):
    import struct
    import zlib

    rows = b"".join(b"\x00" + color * width for _ in range(height))

    def chunk(kind, data):
        crc = struct.pack(">I", zlib.crc32(kind + data))
        return struct.pack(">I", len(data)) + kind + data + crc

    header = struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
    return (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", header)
        + chunk(b"IDAT", zlib.compress(rows))
        + chunk(b"IEND", b"")
    )


def put(root, rel, data):
    path = root / rel
    path.write_bytes(data)
    stat = os.stat(path)
    os.utime(path, ns=(stat.st_atime_ns, stat.st_mtime_ns + 1_000_000_000))


def images(recorder):
    group = next(
        (n for n in recorder._open_view()["nodes"] if n["id"] == "images"), None
    )
    return {} if group is None else {n["label"]: n for n in group["children"]}


def test_a_changed_picture_shows_its_old_and_new_version(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    put(root, "logo.png", png(2, 2, b"\x00\x00\xff"))
    journal.checkpoint("block", "add logo")
    put(root, "logo.png", png(3, 2, b"\x00\xff\x00"))
    put(root, "new.png", png(1, 1))
    put(root, "huge.png", png(1, 1) + b"\x00" * (3 * 1024 * 1024))
    journal.checkpoint("block", "change logo")
    app.refresh()
    shown = images(recorder)
    # From the start, logo.png is new: one picture. Huge files show a note.
    assert "before" not in shown["logo.png (added)"]
    assert "no preview" in shown["huge.png (added)"]["text"]
    last = journal.entries[-1].seq
    recorder.send("activate", f"checkpoint-{last}")
    assert wait_for(lambda: "logo.png (modified)" in images(recorder))
    changed = images(recorder)["logo.png (modified)"]
    assert (changed["width"], changed["before"]["width"]) == (3, 2)
    assert changed["media_type"] == changed["before"]["media_type"] == "image/png"


def test_a_deleted_picture_shows_its_old_version_alone():
    # CRAP audit A11: the deleted branch of image_node.
    old = png(2, 3)
    diff = FileDiff("gone.png", "deleted", 0, 0, True, "", "b-old", None)
    node = image_node(diff, {"b-old": old}.get)
    assert node["type"] == "image" and "before" not in node
    assert node["alt"] == "The deleted file gone.png"
    assert (node["width"], node["height"]) == (2, 3)


def test_a_binary_file_that_is_no_picture_gives_no_image():
    # CRAP audit A11: a changed binary that no surface draws shows no image node.
    diff = FileDiff("data.bin", "modified", 0, 0, True, "", "b-old", "b-new")
    blobs = {"b-old": b"\x00\x01binary", "b-new": b"\x00\x02binary"}
    assert image_node(diff, blobs.get) is None


def test_the_images_go_away_when_the_range_has_no_picture(tmp_path, recorder):
    root, journal, app = setup(tmp_path, recorder)
    put(root, "logo.png", png(2, 2))
    journal.checkpoint("block")
    edit(root, "a.py", "a = 2\n")
    text = journal.checkpoint("block")
    app.refresh()
    assert "images" in recorder.ids()
    recorder.send("activate", f"checkpoint-{text.seq}")
    assert wait_for(lambda: "images" not in recorder.ids())


def test_the_log_tells_timings_failures_and_refused_actions(tmp_path, recorder):
    def ask(request, model):
        raise RuntimeError("model is away")

    root, journal, app = setup(tmp_path, recorder, ask=ask)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    app.refresh()
    recorder.send("activate", "checks")
    recorder.send("activate", "ai")
    recorder.send("submit", "diff", comments=[{"body": "Why?"}], submission_id="s")

    def log():
        return [
            (e["level"], e["message"]) for e in recorder._open_view().get("log", [])
        ]

    assert wait_for(lambda: any("Refused the comments" in m for _, m in log()))
    levels = dict((m.split(" ")[0], lvl) for lvl, m in log())
    assert levels["Checks"] == "info"
    failure = next(e for e in recorder._open_view()["log"] if e["level"] == "error")
    assert "model is away" in failure["message"] and "Traceback" in failure["detail"]


def test_a_picture_without_preview_leaves_the_budget_to_smaller_ones(
    tmp_path, recorder, monkeypatch
):
    # Audit L2: only a picture that shows counts against the view bound.
    import vis_review.app as review

    small = png(1, 1)
    monkeypatch.setattr(review, "MOST_PICTURE_BYTES", 3 * len(small))
    root, journal, app = setup(tmp_path, recorder)
    put(root, "a_big.png", png(40, 40) + b"\x00" * 4 * len(small))
    put(root, "b_small.png", small)
    journal.checkpoint("block")
    app.refresh()
    shown = images(recorder)
    assert shown["a_big.png (added)"]["type"] == "status"
    assert shown["b_small.png (added)"]["type"] == "image"


def test_the_image_cache_keeps_only_the_newest_bytes(tmp_path, recorder, monkeypatch):
    # Audit L3: a long session with many pictures must not grow without a bound.
    import vis_review.app as review

    picture = png(2, 2)
    monkeypatch.setattr(review, "MOST_CACHED_PICTURE_BYTES", len(picture) + 10)
    root, journal, app = setup(tmp_path, recorder)
    put(root, "one.png", picture)
    journal.checkpoint("block")
    app.refresh()
    put(root, "one.png", png(3, 3))
    journal.checkpoint("block")
    app.refresh()
    assert [key[2] for key in app._images] == [journal.state["one.png"]]


def test_the_image_cache_also_bounds_changes_without_a_picture(
    tmp_path, recorder, monkeypatch
):
    # Review audit (goal 4b41c770): changes without a picture cost no bytes, so
    # 1000 of them stayed in the cache.
    import vis_review.app as review

    monkeypatch.setattr(review, "MOST_CACHED_IMAGES", 3)
    _, _, app = setup(tmp_path, recorder)
    for n in range(10):
        diff = FileDiff(f"d{n}.bin", "modified", 0, 0, True, "", "b-old", "b-new")
        assert app._image((n, n + 1, diff.path), diff) is None
    assert len(app._images) == 3


def buttons_of(recorder):
    """Each action button id with its is_disabled flag."""
    return controls(recorder)


def chosen(recorder):
    """The ids of the toolbar buttons that the app marks as the choice that holds now."""
    nodes = list(recorder.node("diff")["children"])
    found = set()
    while nodes:
        node = nodes.pop()
        if node["type"] == "group":
            nodes.extend(node["children"])
        elif node.get("is_selected"):
            found.add(node["id"])
    return found


def test_show_full_files_puts_every_unchanged_line_in_the_diff(tmp_path, recorder):
    # "Expand all" (scenario S15): the far apart changes join in one hunk.
    root, journal, app = setup(tmp_path, recorder)
    lines = [f"line {n}\n" for n in range(40)]
    edit(root, "a.py", "".join(lines))
    journal.checkpoint("block")
    lines[2], lines[37] = "first change\n", "last change\n"
    edit(root, "a.py", "".join(lines))
    journal.checkpoint("block")
    recorder.send("activate", "checkpoint-3")
    assert wait_for(lambda: recorder.node("diff")["patch"].count("@@ -") == 2)
    recorder.send("activate", "full")
    assert wait_for(lambda: recorder.node("diff")["patch"].count("@@ -") == 1)
    assert " line 20" in recorder.node("diff")["patch"]
    recorder.send("activate", "full")
    assert wait_for(lambda: recorder.node("diff")["patch"].count("@@ -") == 2)


def test_latest_change_shows_only_the_newest_checkpoint(tmp_path, recorder):
    # "Latest turn" scope (scenario S08).
    root, journal, app = setup(tmp_path, recorder)
    assert buttons_of(recorder)["latest"]
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    edit(root, "b.py", "b = 1\n")
    journal.checkpoint("block")
    app.refresh()
    assert not buttons_of(recorder)["latest"]
    recorder.send("activate", "latest")
    assert wait_for(lambda: items(recorder, "file") == ["file-all", "file:b.py"])


def test_uncommitted_compares_with_the_commit_and_falls_back_without_git(
    tmp_path, recorder
):
    # "Uncommitted" scope (scenario S08). Outside git, the app says so.
    root, journal, app = setup(tmp_path, recorder)
    recorder.send("activate", "uncommitted")
    assert wait_for(lambda: recorder.node("notice")["tone"] == "warn")
    assert buttons_of(recorder)["undo-file"] is True
    assert app.against is None
    env = {
        **os.environ,
        "GIT_CONFIG_GLOBAL": os.devnull,
        "GIT_AUTHOR_NAME": "t",
        "GIT_AUTHOR_EMAIL": "t@example.com",
        "GIT_COMMITTER_NAME": "t",
        "GIT_COMMITTER_EMAIL": "t@example.com",
    }
    for args in (["init", "-q"], ["add", "-A"], ["commit", "-q", "-m", "c"]):
        subprocess.run(["git", *args], cwd=root, env=env, check=True)
    edit(root, "a.py", "a = 2\n")
    recorder.send("activate", "uncommitted")
    assert wait_for(lambda: recorder.node("diff")["old_revision"] == "HEAD")
    assert "-a = 1\n+a = 2" in recorder.node("diff")["patch"]
    recorder.send("activate", "file:a.py")
    assert wait_for(lambda: app.path == "a.py")
    assert buttons_of(recorder)["undo-file"] is True
    recorder.send("activate", "all")
    assert wait_for(lambda: recorder.node("diff")["old_revision"] == "#1")


def test_a_session_review_shows_only_its_own_changes_by_default(tmp_path, recorder):
    # Goal dcc7197a: two sessions in one root saw each other's edits.
    root = tmp_path / "root"
    root.mkdir()
    (root / "a.py").write_text("a = 1\n")
    journal = Journal(root, "s1", tmp_path / "base", files=walked)
    app = ReviewApp(journal, extension_opener, is_session_scoped=True).open()
    edit(root, "theirs.py", "x = 1\n")
    edit(root, "a.py", "a = 1 by them\n")
    journal.checkpoint("outside", "Changes between blocks")
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block", "edit a")
    app.refresh()
    assert items(recorder, "file") == ["file-all", "file:a.py"]
    assert chosen(recorder) == {"session", "file-all"}
    assert buttons_of(recorder)["session"] is False
    recorder.send("activate", "all")
    assert wait_for(
        lambda: items(recorder, "file") == ["file-all", "file:a.py", "file:theirs.py"]
    )
    recorder.send("activate", "session")
    assert wait_for(lambda: items(recorder, "file") == ["file-all", "file:a.py"])
    edit(root, "a.py", "a = 3 by them\n")
    journal.checkpoint("outside", "Changes between blocks")
    recorder.send("activate", "file:a.py")
    assert wait_for(lambda: app.path == "a.py")
    recorder.send("activate", "undo-file")
    assert wait_for(lambda: (root / "a.py").read_text() == "a = 1 by them\n")
    assert (root / "theirs.py").read_text() == "x = 1\n"


def _buttons_in(nodes):
    for node in nodes:
        if node.get("type") == "button":
            yield node["id"]
        yield from _buttons_in(node.get("children") or ())


def test_each_key_presses_a_button_that_the_app_shows(tmp_path, recorder):
    # A key that names no button would show in the terminal key legend and do nothing.
    for name, scoped in (("global", False), ("session", True)):
        root = tmp_path / name
        root.mkdir()
        journal = Journal(root, "s1", tmp_path / name / "base", files=walked)
        ReviewApp(journal, extension_opener, is_session_scoped=scoped).open()
        view = recorder._open_view()
        shown = set(_buttons_in(view["nodes"]))
        pressed = [shortcut["node_id"] for shortcut in view["shortcuts"]]
        assert pressed and set(pressed) <= shown
        assert ("session" in pressed) == scoped


def test_a_finding_on_no_line_shows_in_the_notice(tmp_path, recorder):
    # T18 user report: the findings table went away with the diff-only layout, so a finding
    # that no diff line holds must still reach the person.
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    recorder.send("activate", "checks")
    assert wait_for(
        lambda: (
            "notice" in recorder.ids()
            and "No language tools here" in recorder.node("notice").get("detail", "")
        )
    )
    assert not recorder.node("diff").get("threads")


def test_an_idle_app_shows_only_the_diff(tmp_path, recorder):
    # T18 user report: the app must show only the diff and its toolbar, with no status rows.
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    journal.checkpoint("block")
    app.refresh()
    assert recorder.ids() == ["diff"]


def test_an_added_picture_shows_no_status_row(tmp_path, recorder):
    # User report 2026-11-09: an added picture gave a "Waiting / Ready / info binary-file"
    # row under the diff. The file list and the image already show the new file.
    root, journal, app = setup(tmp_path, recorder)
    put(root, "logo.png", png(2, 2))
    journal.checkpoint("block")
    app.refresh()
    assert recorder.ids() == ["diff", "images"]


def test_many_findings_keep_the_view_valid(tmp_path, recorder):
    # User report 2026-11-08: /review did not open. A crash log gave 1,890 findings,
    # and a diff node holds at most 200 threads.
    limit = next(
        limit for field, _, limit in vis.testing.bounds("diff") if field == "threads"
    )
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 1  \n" * (limit + 1))
    journal.checkpoint("block")
    app.refresh()
    diff = recorder.node("diff")
    assert len(diff["threads"]) == limit
    assert "1 more finding not shown" in diff["label"]


def test_a_finding_in_a_file_cut_from_the_patch_is_no_thread(
    tmp_path, recorder, monkeypatch
):
    # A thread on a file that the patch leaves out points at a line that no one can see.
    import vis_review.app as review_app

    monkeypatch.setattr(review_app, "MOST_PATCH_CHARS", 60)
    root, journal, app = setup(tmp_path, recorder)
    edit(root, "a.py", "a = 2\n")
    edit(root, "z.py", "z = 1  \n")
    journal.checkpoint("block")
    app.refresh()
    assert not recorder.node("diff").get("threads")
    assert "z.py:1" in recorder.node("notice")["detail"]
