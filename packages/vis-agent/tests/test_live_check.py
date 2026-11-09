"""One owner for the live view check: the schema, as the engine reads it (#453)."""

import json
import os
import subprocess
import sys
import time
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts, _outside

SRC = Path(vis.__file__).parents[2]


def thread(index):
    anchor = {"path": "a.py", "side": "new", "line": 1}
    return {"id": f"t{index}", "anchor": anchor, "messages": []}


def diff(threads):
    return {"id": "d", "type": "diff", "patch": "", "threads": threads}


def opened(recorder, *nodes):
    envelope = {"op": "open", "view": {"title": "Review", "nodes": list(nodes)}}
    return recorder.host_live(json.dumps(envelope))


def test_the_recorder_fails_on_a_schema_rule_the_hand_check_missed():
    # The /review app sent 1890 threads and the engine refused the view.
    with pytest.raises(AssertionError) as raised:
        opened(vis._LiveRecorder(object()), diff([thread(i) for i in range(201)]))
    assert (
        "node d /threads - the array has 201 items, the maximum is 200 (maxItems)"
        in str(raised.value)
    )


def test_a_fault_names_the_node_the_path_and_the_rule_but_not_the_value():
    node = {"id": "h", "type": "heading", "text": "Hi", "level": 2}
    node.update({f"key{i}": "secret-value" for i in range(7)})
    text = _contracts.live_node_fault(node)
    assert text.startswith("node h / - unknown property ")
    assert text.count("(additionalProperties)") == 5
    assert text.endswith("; and 2 more")
    assert "secret-value" not in text
    assert "https://" not in text


def test_a_send_logs_a_node_fault_and_opens_the_rest(monkeypatch, capsys):
    # The warning goes through vis.log, the path that reaches the extension log.
    monkeypatch.setattr(vis, "_host", _outside.host)
    nodes = [vis.ui.status("now", "waiting"), {"id": "odd", "type": "sparkline"}]
    with vis.live("Deploy", nodes, flush_ms=0) as view:
        shown = view.state()["nodes"]
    assert shown[0]["text"] == "waiting"
    assert (shown[1]["id"], shown[1]["type"], shown[1]["tone"]) == (
        "odd",
        "status",
        "error",
    )
    warnings = [
        line for line in capsys.readouterr().err.splitlines() if "[vis warn]" in line
    ]
    assert len(warnings) == 1
    assert "vis live view: node odd /type - unknown node type (type)" in warnings[0]


def test_a_send_raises_for_a_fault_of_the_whole_view(monkeypatch):
    monkeypatch.setattr(vis, "_host", _outside.host)
    with pytest.raises(RuntimeError, match='two nodes have the id "now"'):
        vis.live("Deploy", [vis.ui.status("now", "a"), vis.ui.status("now", "b")])


def test_the_recorder_refuses_a_patch_that_breaks_a_node(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Doc", [vis.ui.heading("h", "Title")], flush_ms=0) as view:
        with pytest.raises(
            RuntimeError,
            match=r"^Invalid live-view patch: /ops/0/level - .*\(maximum\)$",
        ):
            view["h"]._op("set", level=9)
            view.flush()
        assert recorder.node("h")["level"] == 2


@pytest.mark.parametrize(
    ("count", "kept", "left"), [(0, 0, 0), (200, 200, 0), (201, 200, 1)]
)
def test_fit_keeps_the_schema_bound_and_counts_the_rest(count, kept, left):
    assert vis.ui.bound("diff", "threads") == 200
    items, rest = vis.ui.fit(range(count), "diff", "threads")
    assert (len(items), rest) == (kept, left)
    assert items == list(range(kept))


@pytest.mark.parametrize(
    ("node_type", "field"), [("sparkline", "x"), ("diff", "label")]
)
def test_bound_names_an_unknown_type_or_field(node_type, field):
    with pytest.raises(ValueError):
        vis.ui.bound(node_type, field)


def test_bounds_give_each_limit_of_a_node_type():
    assert ("threads", "maxItems", 200) in list(vis.testing.bounds("diff"))
    with pytest.raises(ValueError):
        list(vis.testing.bounds("sparkline"))


def live_check(*args, stdin=""):
    env = dict(os.environ, PYTHONPATH=str(SRC))
    return subprocess.run(
        [sys.executable, "-m", "blockether.vis.live_check", *args],
        input=stdin,
        capture_output=True,
        text=True,
        env=env,
        check=False,
    )


def test_live_check_passes_a_recorded_view_and_names_a_bad_one(tmp_path):
    recorder = vis._LiveRecorder(object())
    opened(recorder, vis.ui.button("go", "Go"), diff([thread(1)]))
    good = tmp_path / "good.json"
    good.write_text(recorder.view_json())
    ran = live_check(str(good))
    assert (ran.returncode, ran.stdout) == (0, "live_check: the view is valid\n")
    bad = json.loads(recorder.view_json())
    bad["nodes"][1]["threads"] = [thread(i) for i in range(201)]
    ran = live_check("-", stdin=json.dumps(bad))
    assert ran.returncode == 1
    assert ran.stdout == (
        "node d /threads - the array has 201 items, the maximum is 200 (maxItems)\n"
    )


@pytest.mark.parametrize(
    ("stdin", "words"), [("", "the input is empty"), ("{", "not JSON")]
)
def test_live_check_names_input_that_is_not_a_view(stdin, words):
    ran = live_check(stdin=stdin)
    assert ran.returncode == 2
    assert words in ran.stderr


def test_the_recorder_gives_an_event_to_the_listener_after_its_lock_is_free():
    # A press held the recorder lock while the listener waited for the app lock,
    # and a producer thread held the app lock while it patched: a deadlock.
    import threading

    recorder = vis._LiveRecorder(object())
    opened(recorder, vis.ui.button("go", "Go"), vis.ui.status("s", "idle"))
    app_lock = threading.Lock()
    heard = []
    entered = threading.Event()

    def producer():
        with app_lock:
            entered.set()
            time.sleep(0.2)
            recorder.host_live(
                json.dumps(
                    {
                        "op": "patch",
                        "patch": {
                            "ops": [{"op": "set", "node_id": "s", "text": "busy"}]
                        },
                    }
                )
            )

    def listener(event):
        with app_lock:
            heard.append(event["type"])

    recorder._listener = listener
    recorder._view["placement"] = "attachment"
    thread = threading.Thread(target=producer, daemon=True)
    thread.start()
    entered.wait(5)
    presser = threading.Thread(target=recorder.activate, args=("go",), daemon=True)
    presser.start()
    presser.join(5)
    thread.join(5)
    assert not presser.is_alive() and not thread.is_alive(), "the recorder deadlocked"
    assert heard == ["activate"]
    assert recorder.node("s")["text"] == "busy"


def test_the_recorder_gives_each_event_on_the_thread_that_made_it():
    # The thread that freed the lock last gave every queued event, also the
    # events of another thread: a slow free let a patch thread run a press.
    import threading

    recorder = vis._LiveRecorder(object())
    opened(recorder, vis.ui.button("go", "Go"), vis.ui.status("s", "idle"))
    lock = recorder._condition._lock
    free = lock._on_free
    heard = []

    def slow_free():
        if threading.current_thread().name == "presser":
            time.sleep(0.3)
        free()

    lock._on_free = slow_free
    recorder._listener = lambda event: heard.append(
        (event["type"], threading.current_thread().name)
    )
    recorder._view["placement"] = "attachment"
    patch = {
        "op": "patch",
        "patch": {"ops": [{"op": "set", "node_id": "s", "text": "x"}]},
    }
    patcher = threading.Thread(
        target=recorder.host_live,
        args=(json.dumps(patch),),
        name="patcher",
        daemon=True,
    )
    presser = threading.Thread(
        target=recorder.activate, args=("go",), name="presser", daemon=True
    )
    presser.start()
    time.sleep(0.1)
    patcher.start()
    presser.join(5)
    patcher.join(5)
    assert heard == [("activate", "presser")]


def test_the_recorder_gives_every_event_when_the_listener_fails():
    # A listener error on the first event lost the other events of the thread.
    recorder = vis._LiveRecorder(object())
    opened(recorder, vis.ui.button("go", "Go"))
    heard = []

    def listener(event):
        heard.append(event["type"])
        if len(heard) == 1:
            raise ValueError("listener failed")

    with pytest.raises(ValueError, match="listener failed"):
        with recorder._condition:
            recorder._queue(listener, {"type": "activate"})
            recorder._queue(listener, {"type": "change"})
    assert heard == ["activate", "change"]


def test_the_recorder_keeps_the_error_of_the_with_body():
    # A listener error replaced the error that the body of the `with` raised.
    recorder = vis._LiveRecorder(object())
    opened(recorder, vis.ui.button("go", "Go"))

    def listener(event):
        raise ValueError("listener failed")

    with pytest.raises(KeyError) as raised:
        with recorder._condition:
            recorder._queue(listener, {"type": "activate"})
            raise KeyError("body failed")
    assert any("listener failed" in note for note in raised.value.__notes__)
