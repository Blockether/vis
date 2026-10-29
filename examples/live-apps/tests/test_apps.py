"""Every proof app, driven through the extension door against the test host."""

import re

import pytest
from live_apps import (
    apps,
    asteroids,
    counter,
    game2048,
    incident,
    kanban,
    runtime,
    todo,
    ui,
    wizard,
)
from live_apps.host import Apps, open_app


def start(recorder, model, store=None):
    runner = runtime.Runner(model, open_app, store=store).open()
    return runner


def stats(recorder, node_id):
    return {s["id"]: s["value_text"] for s in recorder.node(node_id)["stats"]}


def rows(recorder, node_id):
    return {r["id"]: r["cells"] for r in recorder.node(node_id)["rows"]}


# -- every model ---------------------------------------------------------------


@pytest.mark.parametrize("key", list(apps()))
def test_each_app_opens_valid_and_within_the_node_bound(recorder, key):
    model = apps()[key]
    runner = start(recorder, model)
    assert runner.view.id == recorder.view_id
    assert recorder._open_view()["placement"] == "attachment"
    assert recorder._open_view()["key"] == model.KEY
    assert runtime.count_nodes(model.render(model.initial())) <= runtime.MAX_NODES
    assert recorder.shortcuts() == list(model.shortcuts(model.initial()))


@pytest.mark.parametrize("key", list(apps()))
def test_reduce_is_pure(key):
    model = apps()[key]
    state = model.initial()
    frozen = repr(state)
    model.reduce(state | {}, {"type": "activate", "node_id": "nothing"})
    model.render(state)
    assert repr(state) == frozen


NAMED_KEYS = {
    "Enter",
    "Space",
    "Backspace",
    "Delete",
    "ArrowUp",
    "ArrowDown",
    "ArrowLeft",
    "ArrowRight",
    "Home",
    "End",
    "PageUp",
    "PageDown",
}


def _texts(nodes):
    for node in runtime.walk(nodes):
        for field in ("text", "detail", "description"):
            if isinstance(node.get(field), str):
                yield node[field]


def _press(model, state, shortcut):
    """The event of one shortcut press, as the engine gives it to the owner."""
    if "node_id" in shortcut:
        return {"type": "activate", "node_id": shortcut["node_id"]}
    return {"type": "shortcut", "action": shortcut["action"], "key": shortcut["key"]}


def _reachable(model, presses=40):
    """The initial state and the states after pressing each declared key in turn."""
    state = model.initial()
    yield state
    for index in range(presses):
        keys = list(getattr(model, "shortcuts", lambda _: [])(state))
        if not keys:
            return
        state = model.reduce(state, _press(model, state, keys[index % len(keys)]))
        yield state


@pytest.mark.parametrize("key", list(apps()))
def test_help_texts_name_only_declared_keys(key):
    # The help text and the declared shortcuts must agree in every state.
    model = apps()[key]
    for state in _reachable(model):
        declared = {s["key"] for s in getattr(model, "shortcuts", lambda _: [])(state)}
        for text in _texts(model.render(state)):
            if text.startswith("Keys: "):
                assert text == ui.key_help(model.shortcuts(state))
            for match in re.finditer(r"\b[Pp]ress (\S+?)[ .,]", text):
                # "press Add" names a button; only a key name must be declared.
                if len(match.group(1)) == 1 or match.group(1) in NAMED_KEYS:
                    assert match.group(1) in declared, (text, declared)
            for match in re.finditer(r"the (\S+) and (\S+) keys", text):
                assert {match.group(1), match.group(2)} <= declared, (text, declared)
            if "arrow keys" in text:
                assert {
                    "ArrowUp",
                    "ArrowDown",
                    "ArrowLeft",
                    "ArrowRight",
                } <= declared, text


def test_counter_help_names_every_key_and_0_resets_at_0(recorder):
    start(recorder, counter)
    help_text = recorder.node("help")["text"]
    for name in ("+", "-", "s", "0", "\u2191", "\u2193"):
        assert name in help_text
    state = counter.reduce(
        counter.initial(), {"type": "shortcut", "action": "reset", "key": "0"}
    )
    assert state["count"] == 0


# -- counter -------------------------------------------------------------------


def test_counter_buttons_and_shortcuts(recorder):
    runner = start(recorder, counter)
    recorder.activate("inc")
    recorder.press("+")
    recorder.press("ArrowUp")
    assert stats(recorder, "numbers")["count"] == "3"
    recorder.press("s")
    recorder.press("-")
    assert stats(recorder, "numbers")["count"] == "-2"
    assert recorder.node("dec")["label"] == "\u22125"
    assert any(s["key"] == "0" for s in recorder.shortcuts())
    recorder.press("0")
    assert stats(recorder, "numbers")["count"] == "0"
    assert recorder.node("reset")["is_disabled"] is True
    # 0 stays bound at zero: a second reset changes only the presses.
    assert recorder.press("0").get("reason") != "unbound"
    assert stats(recorder, "numbers")["count"] == "0"
    assert runner.errors == []


# -- todo ----------------------------------------------------------------------


def test_todo_add_select_toggle_delete_and_filter(recorder):
    runner = start(recorder, todo)
    recorder.activate("add")
    assert recorder.node("notice")["text"] == "Write a task first."
    for text in ("milk", "bread", "eggs"):
        recorder.type_text("new", text)
        recorder.activate("add")
    assert [c[1] for c in rows(recorder, "items").values()] == ["milk", "bread", "eggs"]
    # The input is empty again after Add.
    assert recorder.node("new").get("value") in (None, "")
    recorder.choose("items", ["t1", "t3"])
    recorder.press("x")
    assert rows(recorder, "items")["t1"][0] == "\u2713"
    assert stats(recorder, "counts")["done"] == "2"
    recorder.activate("show-active")
    assert list(rows(recorder, "items")) == ["t2"]
    recorder.press("f")
    assert list(rows(recorder, "items")) == ["t1", "t3"]
    recorder.activate("show-all")
    recorder.activate("clear-done")
    assert list(rows(recorder, "items")) == ["t2"]
    recorder.choose("items", ["t2"])
    recorder.press("Delete")
    assert rows(recorder, "items") == {}
    assert runner.errors == []


def test_todo_reads_the_selection_without_a_select_event(recorder):
    # Today the engine gives no select event (finding F2): the runner reads the
    # selection from the picture when a button is pressed.
    start(recorder, todo)
    recorder.type_text("new", "one")
    recorder.activate("add")
    recorder.select("items", ["t1"])
    recorder.activate("toggle")
    assert rows(recorder, "items")["t1"][0] == "\u2713"


def test_todo_refuses_a_long_task(recorder):
    start(recorder, todo)
    recorder.type_text("new", "x" * 121)
    recorder.activate("add")
    assert "at most 120" in recorder.node("notice")["text"]
    assert rows(recorder, "items") == {}


# -- kanban --------------------------------------------------------------------


def test_kanban_moves_cards_and_keeps_the_wip_limit(recorder):
    runner = start(recorder, kanban)
    assert recorder.node("left")["is_disabled"] is True
    recorder.choose("todo", ["c3"])
    assert recorder.node("right")["is_disabled"] is False
    recorder.press("l")
    assert "c3" in rows(recorder, "doing")
    assert recorder.node("doing")["selected_ids"] == ["c3"]
    for title in ("a", "b"):
        recorder.type_text("title", title)
        recorder.press("n")
    assert list(rows(recorder, "todo")) == ["c4", "c5"]
    recorder.choose("todo", ["c4"])
    recorder.press("l")
    assert "c4" in rows(recorder, "doing")
    recorder.choose("todo", ["c5"])
    recorder.press("l")
    assert "at most 3" in recorder.node("notice")["text"]
    assert recorder.node("notice")["tone"] == "warn"
    assert "c5" in rows(recorder, "todo")
    recorder.press("p")
    assert rows(recorder, "todo")["c5"][1] == "high"
    recorder.press("Delete")
    assert "c5" not in rows(recorder, "todo")
    assert recorder.node("doing")["label"] == "Doing (3)"
    assert runner.errors == []


# -- 2048 ----------------------------------------------------------------------


def test_2048_slide_rules():
    assert game2048.slide_row([2, 2, 2, 2]) == ([4, 4, 0, 0], 8)
    assert game2048.slide_row([2, 0, 2, 4]) == ([4, 4, 0, 0], 4)
    assert game2048.slide_row([4, 4, 8, 0]) == ([8, 8, 0, 0], 8)
    board = [[2, 0, 0, 0], [2, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
    assert game2048.move(board, "up")[0][0] == [4, 0, 0, 0]
    assert game2048.move(board, "right")[0][0] == [0, 0, 0, 2]
    assert game2048.move(board, "down")[0][3] == [4, 0, 0, 0]


def test_2048_plays_with_keys_wins_and_loses(recorder):
    runner = start(recorder, game2048)
    before = runner.state["board"]
    for key in ("ArrowLeft", "ArrowUp", "ArrowRight", "ArrowDown", "h", "k"):
        recorder.press(key)
    assert runner.state["moves"] >= 1
    assert runner.state["board"] != before
    tiles = sum(1 for row in runner.state["board"] for v in row if v)
    assert tiles >= 3
    # Force a win and check the keys change.
    state = runner.state | {"board": [[1024, 1024, 0, 0], [0] * 4, [0] * 4, [0] * 4]}
    runner.state = state
    recorder.press("ArrowLeft")
    assert runner.state["status"] == "won"
    assert recorder.node("state")["tone"] == "ok"
    assert recorder.press("ArrowLeft")["reason"] == "unbound"
    recorder.press("c")
    assert runner.state["status"] == "playing"
    lost = [[2, 4, 2, 4], [4, 2, 4, 2], [2, 4, 2, 4], [4, 2, 4, 0]]
    runner.state = runner.state | {"board": lost, "seed": 3}
    recorder.press("ArrowRight")
    assert runner.state["status"] in ("lost", "playing")
    recorder.press("n")
    assert runner.state["status"] == "playing" and runner.state["moves"] == 0
    assert runner.errors == []


# -- asteroids -----------------------------------------------------------------


def _patches(recorder):
    return sum(1 for envelope in recorder.ops() if envelope.get("op") == "patch")


def _tick(state, times=1):
    for _ in range(times):
        state = asteroids.reduce(state, {"type": "tick"})
    return state


def _playing(**changes):
    """A started game; `ship` keys in `changes` change the ship."""
    state = asteroids.reduce(
        asteroids.initial(), {"type": "activate", "node_id": "pause"}
    )
    state["ship"] |= changes.pop("ship", {})
    return state | changes


def test_asteroids_a_held_key_sends_one_patch_for_each_tick(recorder):
    # Each patch is one durable line in the app record and in the session
    # journal. A repeating key must not add lines: only the tick paints.
    runner = start(recorder, asteroids)
    recorder.press("Space")
    assert runner.state["status"] == "playing"
    before = _patches(recorder)
    for _ in range(30):
        recorder.press("ArrowLeft")
    assert _patches(recorder) == before
    runner.dispatch({"type": "tick"})
    assert _patches(recorder) == before + 1
    assert runner.state["ship"]["heading"] == asteroids.HEADINGS - asteroids.MAX_TURN
    assert runner.errors == []


def test_asteroids_sends_no_patch_when_it_does_not_play(recorder):
    # An open app that nobody plays must not grow its record.
    runner = start(recorder, asteroids)
    before = _patches(recorder)
    for _ in range(5):
        runner.dispatch({"type": "tick"})
    assert _patches(recorder) == before
    recorder.press("Space")
    recorder.press("p")
    assert runner.state["status"] == "paused"
    paused = _patches(recorder)
    for _ in range(5):
        runner.dispatch({"type": "tick"})
    assert _patches(recorder) == paused
    over = _playing(status="over")
    assert _tick(over) == over


def test_asteroids_pauses_itself_after_idle_seconds_without_a_key():
    # The ship is safe, so no rock ends the game before the idle clock does.
    state = _playing(ship={"safe": 10**6})
    limit = round(asteroids.IDLE_SECONDS / asteroids.TICK_SECONDS)
    state = _tick(state, limit - 2)
    state = asteroids.reduce(
        state, {"type": "shortcut", "action": "thrust", "key": "w"}
    )
    state = _tick(state, limit - 1)
    assert state["status"] == "playing"
    state = _tick(state)
    assert (state["status"], state["paused_by"]) == ("paused", "idle")
    status = next(n for n in asteroids.render(state) if n["id"] == "state")
    assert status["text"].startswith("Paused after")


@pytest.mark.parametrize(("size", "left", "points"), [(3, [2, 2], 20), (1, [], 100)])
def test_asteroids_a_shot_splits_a_rock_and_scores_it(size, left, points):
    rock = {"x": 30.0, "y": 6.0, "vx": 0.0, "vy": 0.0, "size": size}
    state = _playing(rocks=[rock], ship={"heading": asteroids.HEADINGS // 4})
    state = asteroids.reduce(
        state, {"type": "shortcut", "action": "fire", "key": "Space"}
    )
    state = _tick(state, 4)
    assert sorted(r["size"] for r in state["rocks"]) == left
    assert state["score"] == points
    assert state["bullets"] == []


def test_asteroids_a_crash_costs_a_life_and_the_last_one_ends_the_game():
    def crash(state):
        ship = state["ship"]
        rock = {"x": ship["x"], "y": ship["y"], "vx": 0.0, "vy": 0.0, "size": 3}
        return _tick(state | {"rocks": [rock]})

    state = crash(_playing(ship={"safe": 0}))
    assert (state["ship"], state["lives"]) == (None, asteroids.LIVES - 1)
    state = _tick(state, asteroids.RESPAWN_TICKS)
    assert state["ship"]["safe"] > 0
    state = crash(_playing(lives=1, best=500, ship={"safe": 0}))
    state = _tick(state, 10)
    assert state["status"] == "over"
    state = asteroids.reduce(
        state, {"type": "shortcut", "action": "fire", "key": "Space"}
    )
    assert (state["status"], state["lives"], state["best"]) == ("playing", 3, 500)


def _held_objects(recorder):
    return {item["id"]: item for item in recorder.node("field")["objects"]}


def test_asteroids_the_host_holds_the_rendered_scene_in_every_state(recorder):
    # A tick sends only changed keys, so the host merges. After any play the
    # merged objects must be the ones the model draws, or the screens drift.
    runner = start(recorder, asteroids)
    recorder.press("Space")
    keys = [
        "ArrowLeft",
        "ArrowUp",
        "Space",
        "ArrowRight",
        "ArrowDown",
        "ArrowUp",
        "Space",
    ]
    for step in range(600):
        recorder.press(keys[step % len(keys)])
        runner.dispatch({"type": "tick"})
        if runner.state["status"] == "over":
            recorder.press("n")
        if step % 50 == 0 or step == 599:
            drawn = asteroids.scene(runner.state)["objects"]
            held = _held_objects(recorder)
            assert list(held) == [item["id"] for item in drawn]
            assert all({k: held[item["id"]][k] for k in item} == item for item in drawn)
    assert runner.errors == []


def test_asteroids_a_tick_sends_only_what_moved(recorder):
    # A patch is one durable line per tick: a rock that moves must not send
    # its mesh, scale and colour again.
    runner = start(recorder, asteroids)
    recorder.press("Space")
    runner.dispatch({"type": "tick"})
    patch = [e for e in recorder.ops() if e.get("op") == "patch"][-1]
    sent = [
        item
        for op in patch["patch"]["ops"]
        if op.get("node_id") == "field"
        for item in op.get("objects") or []
    ]
    rocks = [item for item in sent if item["id"].startswith("rock")]
    assert rocks
    assert all(set(item) <= {"id", "x", "y", "rx", "ry"} for item in rocks)


# -- incident ------------------------------------------------------------------


def test_incident_ticks_ack_and_send_receipts(recorder):
    effects = []
    runner = runtime.Runner(
        incident, open_app, on_effect=lambda r, e: effects.append(e)
    ).open()
    for _ in range(12):
        runner.dispatch({"type": "tick"})
    assert runner.state["incidents"], "ticks open incidents"
    first = runner.state["incidents"][0]["id"]
    recorder.choose("incidents", [first])
    recorder.press("a")
    assert rows(recorder, "incidents")[first][3] == "acked"
    for _ in range(8):
        runner.dispatch({"type": "tick"})
    assert any(
        i["id"] == first and i["status"] == "resolved"
        for i in runner.state["incidents"]
    )
    recorder.press("p")
    assert recorder.node("health")["text"] == "Paused"
    tick = runner.state["tick"]
    runner.dispatch({"type": "tick"})
    assert runner.state["tick"] == tick
    recorder.press("!")
    runner.dispatch(
        {
            "type": "submit",
            "node_id": "notes",
            "submission_id": "sub-1",
            "comments": [{"node_id": "notes", "body": "db is slow"}],
        }
    )
    assert [e["do"] for e in effects] == ["set_status", "reply"]
    assert effects[0]["submission_id"] == "sub-1"
    assert "db is slow" in recorder.node("events")["lines"][-1]
    assert stats(recorder, "summary")["sends"] == "1"
    assert runner.errors == []


def test_incident_producer_thread_drives_the_view(recorder):
    runner = runtime.Runner(incident, open_app).open()
    incident_tick = incident.TICK_SECONDS
    try:
        incident.TICK_SECONDS = 0.01
        runner.start()
        import time

        deadline = time.monotonic() + 5
        while runner.state["tick"] < 20 and time.monotonic() < deadline:
            time.sleep(0.01)
            recorder.activate("spawn")
    finally:
        runner.stop()
        incident.TICK_SECONDS = incident_tick
    assert runner.state["tick"] >= 20
    assert runner.errors == []
    assert len(rows(recorder, "incidents")) == len(runner.state["incidents"])


def test_incident_tick_loop_survives_a_gateway_outage(recorder):
    # A gateway restart fails some ticks; the loop must not end.
    runner = runtime.Runner(incident, open_app).open()
    incident_tick = incident.TICK_SECONDS
    paint = runner._paint
    failures = []

    def flaky(state):
        if len(failures) < 3:
            failures.append(state["tick"])
            raise ConnectionError("gateway connection failed")
        paint(state)

    runner._paint = flaky
    try:
        incident.TICK_SECONDS = 0.01
        runner.start()
        import time

        deadline = time.monotonic() + 5
        while runner.state["tick"] < 5 and time.monotonic() < deadline:
            time.sleep(0.01)
    finally:
        runner.stop()
        incident.TICK_SECONDS = incident_tick
    assert len(failures) == 3
    assert runner.state["tick"] >= 5


# -- wizard --------------------------------------------------------------------


def test_wizard_checks_each_step_then_deploys(recorder):
    runner = start(recorder, wizard)
    # The n key presses Next as a plain press: the reducer checks the step.
    recorder.press("n")
    assert recorder.node("check")["tone"] == "error"
    assert "\u26a0" in recorder.node("name")["description"]
    # A submit of Next: the engine checks required inputs first.
    refused = recorder.submit("next")
    assert refused["is_accepted"] is False
    assert set(refused["errors"]) == {"name", "env"}
    # Then the validate= function of the name judges the value.
    refused = recorder.submit("next", {"name": "Bad Name", "env": "prod"})
    assert refused["errors"] == {"name": "Use lowercase letters, digits and dashes."}
    assert runner.state["step"] == 0
    recorder.type_text("name", "Bad Name")
    recorder.type_text("env", "prod")
    recorder.press("n")
    assert "lowercase" in recorder.node("name")["description"]
    assert recorder.submit("next", {"name": "payments-api"})["is_accepted"]
    assert runner.state["step"] == 1
    assert runner.state["form"] == dict(
        wizard.initial()["form"], name="payments-api", env="prod"
    )
    refused = recorder.submit("next")
    assert refused["errors"] == {"regions": "is required"}
    recorder.type_text("regions", ["eu-west", "us-east"])
    recorder.type_text("replicas", 1)
    # The rule that crosses steps stays in the reducer.
    recorder.submit("next")
    assert "at least 2" in recorder.node("replicas")["description"]
    recorder.type_text("replicas", 3)
    recorder.submit("next")
    assert runner.state["step"] == 2
    refused = recorder.submit("deploy")
    assert refused["is_accepted"] is False and "token" in refused["errors"]
    recorder.press("b")
    assert runner.state["step"] == 1
    assert recorder.node("replicas")["value"] == 3
    recorder.press("n")
    assert runner.state["step"] == 2
    refused = recorder.submit("deploy", {"token": "s3cret", "confirm": False})
    assert refused["errors"] == {"confirm": "Confirm the deploy."}
    assert runner.state["run"] is None
    recorder.submit("deploy", {"token": "s3cret", "confirm": True})
    assert runner.state["run"]["status"] == "running"
    assert runner.state["step"] == 3
    assert "payments-api" in recorder.node("summary")["text"]
    assert "s3cret" not in str(runner.state)
    for _ in range(len(wizard.STAGES)):
        runner.dispatch({"type": "tick"})
    assert runner.state["run"]["status"] == "done"
    assert recorder.node("deploy-progress")["done"] == len(wizard.STAGES)
    recorder.press("r")
    assert runner.state["step"] == 0 and runner.state["runs"] == 1
    assert runner.errors == []


def test_wizard_validators_follow_the_step_patches(recorder):
    # D4b: the inputs of each step come in a patch with their validators.
    start(recorder, wizard)
    assert "name" in recorder._declared
    assert "confirm" not in recorder._declared
    recorder.submit("next", {"name": "api", "env": "dev"})
    assert "name" not in recorder._declared
    recorder.submit("next", {"regions": ["eu-west"]})
    assert recorder._declared == {"confirm"}
    refused = recorder.submit("deploy", {"token": "t", "confirm": False})
    assert refused["errors"] == {"confirm": "Confirm the deploy."}
    recorder.press("b")
    recorder.press("b")
    refused = recorder.submit("next", {"name": "Bad Name", "env": "dev"})
    assert "lowercase" in refused["errors"]["name"]


def test_todo_validates_in_its_reducer(recorder):
    # The proof that an owner can also validate in its own reducer.
    runner = start(recorder, todo)
    recorder.activate("add")
    assert recorder.node("notice")["text"] == "Write a task first."
    assert runner.state["items"] == []


# -- gallery and the extension door --------------------------------------------


def test_extension_adds_and_moves_tui_keys(monkeypatch):
    # The example proves both parts: a new key and a moved built-in key.
    import importlib.util
    from pathlib import Path

    import blockether.vis.extension as vis

    registered = []
    monkeypatch.setattr(vis, "register_extension", registered.append)
    path = Path(__file__).resolve().parents[1] / "extension.py"
    spec = importlib.util.spec_from_file_location("live_apps_entry", path)
    spec.loader.exec_module(importlib.util.module_from_spec(spec))
    (extension,) = registered
    bindings = {b.key: (b.command, b.action, b.label) for b in extension.keybindings}
    assert bindings == {
        "L": ("/apps", None, "Live apps"),
        "e": (None, "focus-attachments", None),
        "i": ("/apps incident", None, "Incidents"),
    }
    # The TUI reserves p, g and the digits after C-x.
    assert not set(bindings) & {"p", "g", *"0123456789"}


def test_gallery_opens_apps_by_card_and_number(recorder):
    opened = Apps(open_app)
    gallery = opened.open("gallery")
    assert len(recorder.shortcuts()) == 7
    recorder.activate("open-counter")
    assert "counter" in opened.runners
    assert gallery.state["opened"] == ["counter"]


def test_adopting_by_key_paints_the_saved_state(recorder):
    store = {}
    first = runtime.Runner(todo, open_app, store=store).open()
    recorder.type_text("new", "keep me")
    recorder.activate("add")
    assert first.state["items"][0]["text"] == "keep me"
    # A new owner process: same key, same store, a fresh picture.
    second = runtime.Runner(todo, open_app, store=store).open()
    assert second.state["items"][0]["text"] == "keep me"
    assert rows(recorder, "items")["t1"][1] == "keep me"


def test_a_failing_event_is_logged_and_the_app_keeps_working(recorder, caplog):
    runner = start(recorder, counter)
    original = counter.reduce

    def broken(state, event):
        raise RuntimeError("boom")

    counter.reduce = broken
    try:
        recorder.activate("inc")
    finally:
        counter.reduce = original
    assert runner.errors == ["RuntimeError: boom"]
    assert "event" in caplog.text
    recorder.activate("inc")
    assert stats(recorder, "numbers")["count"] == "1"


# -- the differ ----------------------------------------------------------------


def test_diff_sends_only_what_changed():
    old = counter.render(counter.initial())
    new = counter.render(
        counter.reduce(counter.initial(), {"type": "activate", "node_id": "inc"})
    )
    steps = runtime.diff(old, new)
    assert {s["do"] for s in steps} <= {"set", "items"}
    assert runtime.diff(new, new) == []


def test_diff_replaces_a_changed_group_and_drops_first():
    old = wizard.render(wizard.initial())
    state = wizard.initial() | {"step": 1}
    steps = runtime.diff(old, wizard.render(state))
    kinds = [s["do"] for s in steps]
    assert kinds.index("drop") < kinds.index("add")
    with pytest.raises(ValueError):
        runtime.diff(old, [{"type": "paragraph", "id": "other", "text": "x"}])


def test_render_refuses_too_many_nodes():
    nodes = [{"type": "paragraph", "id": f"p{i}", "text": "x"} for i in range(65)]
    with pytest.raises(ValueError, match="maximum is 64"):
        runtime.check_tree(nodes)
