"""Apps from an SDK program: the HTTP app door, app shortcuts and `run`.

The engine side is `_LiveRecorder`, the in-memory live host, behind a fake
`POST /v1/sessions/:sid/apps` route. So no gateway and no model run here.
"""

import json
import threading
import time

import blockether.vis.extension as vis
import pytest
from blockether.vis._contracts import validate
from blockether.vis.engine._client import Session
from blockether.vis.engine._extensions import ClientExtensions


class Response:
    def __init__(self, data):
        self._data = data
        self.content = json.dumps(data).encode()

    def json(self):
        return self._data


class Gateway:
    """Answer the app door route from one `_LiveRecorder` engine."""

    def __init__(self):
        self.engine = vis._LiveRecorder(object(), view_id="app-1")
        self.calls = []

    def _request(self, method, route, *, path=None, body=None, timeout=None, **_):
        self.calls.append((method, route, path, body, timeout))
        assert (method, route) == ("POST", "/v1/sessions/:sid/apps")
        answer = self.engine.host_live(json.dumps(body["envelope"]))
        return Response(json.loads(answer))

    def envelopes(self, op):
        return [c[3]["envelope"] for c in self.calls if c[3]["envelope"]["op"] == op]


def app(**options):
    return vis.ui.app(
        "Review",
        vis.ui.button("go", label="Go"),
        key="review",
        shortcuts=[vis.ui.shortcut("r", "refresh", label="Refresh")],
        **options,
    )


def wait_for(predicate, seconds=5.0):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if predicate():
            return True
        time.sleep(0.01)
    return False


def test_shortcut_builder_follows_the_contract():
    for key in ("r", "A", "?", "Enter", "ArrowUp"):
        validate("view", "shortcut", vis.ui.shortcut(key, "next"))
    pressed = vis.ui.shortcut("Space", node_id="go", label="Press Go")
    assert pressed == {"key": "Space", "node_id": "go", "label": "Press Go"}
    validate("view", "shortcut", pressed)
    with pytest.raises(ValueError, match="ArrowUp"):
        vis.ui.shortcut("Escape", "close")
    with pytest.raises(ValueError, match="exactly one"):
        vis.ui.shortcut("r")
    with pytest.raises(ValueError, match="exactly one"):
        vis.ui.shortcut("r", "next", node_id="go")
    with pytest.raises(ValueError, match="at most 40"):
        vis.ui.shortcut("r", "next", label="x" * 41)
    with pytest.raises(TypeError, match="list"):
        vis.ui.app("Review", vis.ui.button("go", label="Go"), shortcuts="r")


def test_session_attach_opens_the_app_over_the_sdk_route():
    gateway = Gateway()
    handle = Session(gateway, "s-1").attach(app(), owner="review-bot")
    assert isinstance(handle, vis.LiveAttachment) and handle.id == "app-1"
    method, route, path, body, _ = gateway.calls[0]
    assert path == {"sid": "s-1"} and body["owner"] == "review-bot"
    assert body["envelope"]["view"]["shortcuts"] == [
        {"key": "r", "action": "refresh", "label": "Refresh"}
    ]
    handle.set_shortcuts([vis.ui.shortcut("n", "next")])
    assert gateway.engine._view["shortcuts"] == [{"key": "n", "action": "next"}]
    handle.set_shortcuts([])
    assert "shortcuts" not in gateway.engine._view
    with pytest.raises(ValueError, match="owner"):
        Session(gateway, "s-1").attach(app(), owner="Review Bot")


def test_an_app_note_reaches_the_view_and_a_blank_text_removes_it():
    # The model reads the newest note of an app with the next user message.
    gateway = Gateway()
    handle = Session(gateway, "s-1").attach(app(), owner="bot")
    handle.set_agent_note("  Card 3 is done  ")
    op = gateway.envelopes("patch")[-1]["patch"]["ops"][0]
    assert op == {"op": "agent-note", "text": "  Card 3 is done  "}
    validate("view", "live_op", op)
    assert gateway.engine._view["agent_note"] == "Card 3 is done"
    handle.set_agent_note(" ")
    assert "agent_note" not in gateway.engine._view
    sent = len(gateway.calls)
    with pytest.raises(TypeError, match="text"):
        handle.set_agent_note(3)
    with pytest.raises(ValueError, match="at most 4000"):
        handle.set_agent_note("x" * 4001)
    assert len(gateway.calls) == sent
    handle.close()


def test_on_event_long_polls_on_a_daemon_thread_until_the_app_closes():
    gateway = Gateway()
    seen = []
    handle = Session(gateway, "s-1").attach(app(), owner="bot", on_event=seen.append)
    poller = next(t for t in threading.enumerate() if t.name == "vis-app-app-1")
    assert poller.daemon
    assert gateway.engine.press_shortcut("r")["shortcut"] == "refresh"
    assert wait_for(lambda: seen)
    assert (seen[0].type, seen[0].action, seen[0].key) == ("shortcut", "refresh", "r")
    gateway.engine.activate("go")
    assert wait_for(lambda: len(seen) == 2)
    assert seen[1].type == "activate" and seen[1].node_id == "go"
    # The poll waits below the 30 s cap of the SDK door.
    assert all(e["timeout_ms"] <= 25000 for e in gateway.envelopes("events"))
    handle.close()
    poller.join(5)
    assert not poller.is_alive()


def test_a_failing_callback_is_logged_and_polling_goes_on(caplog):
    gateway = Gateway()
    seen = []

    def callback(event):
        seen.append(event)
        if len(seen) == 1:
            raise RuntimeError("first event breaks")

    handle = Session(gateway, "s-1").attach(app(), owner="bot", on_event=callback)
    gateway.engine.press_shortcut("r")
    assert wait_for(lambda: seen)
    gateway.engine.press_shortcut("r")
    assert wait_for(lambda: len(seen) == 2)
    assert "on_event callback failed" in caplog.text
    assert "first event breaks" in caplog.text
    handle.close()


def test_run_serves_events_until_the_callback_returns_false():
    gateway = Gateway()
    handle = Session(gateway, "s-1").attach(app(), owner="bot")
    threading.Timer(0.05, gateway.engine.press_shortcut, ["r"]).start()
    seen = []
    started = time.monotonic()
    served = handle.run(lambda event: seen.append(event.action) or False, timeout=5)
    assert time.monotonic() - started < 2
    assert (served, seen) == (1, ["refresh"])
    assert handle.run(lambda event: None, timeout=0.05) == 0
    handle.close()
    assert handle.run(lambda event: None) == 0


def test_door_events_split_a_long_wait_into_capped_polls():
    gateway = Gateway()
    handle = Session(gateway, "s-1").attach(app(), owner="bot")
    gateway.engine.press_shortcut("r")
    assert [e.action for e in handle.events(timeout=60)] == ["refresh"]
    assert gateway.envelopes("events")[-1]["timeout_ms"] == 25000
    handle.close()


def test_a_client_extension_callback_opens_its_app_over_the_session_door():
    gateway = Gateway()

    def open_review() -> str:
        """Open the review app and answer its id."""
        return vis.attach(app()).id

    bindings = ClientExtensions(
        [
            vis.Extension(
                name="My Review Tools",
                description="Review tools.",
                alias="review",
                symbols=[vis.Symbol(open_review, activity=vis.Activity(label="Open"))],
            )
        ]
    )
    call = {"id": "one", "name": "open_review", "args": [], "kwargs": {}}
    result = bindings.dispatch(call, lambda _: True, Session(gateway, "s-1"))
    assert result == {"status": "success", "result": "app-1"}
    assert gateway.calls[0][3]["owner"] == "my-review-tools"
    assert vis._app_door.get() is None


def test_recorder_press_shortcut_refuses_unbound_keys_and_closed_apps():
    recorder = vis._LiveRecorder(object())
    recorder.host_live(json.dumps({"op": "open", "view": app()}))
    assert recorder.press_shortcut("x") == {
        "is_accepted": False,
        "reason": "unbound",
        "key": "x",
    }
    recorder.host_live(json.dumps({"op": "close", "view_id": "test-live-view"}))
    assert recorder.press_shortcut("r")["reason"] == "closed"


def test_recorder_press_shortcut_presses_a_node_like_the_engine():
    # Live apps D5: a node shortcut presses its node and answers the node id.
    view = app()
    view["nodes"].append(
        {"id": "off", "type": "button", "label": "Off", "is_disabled": True}
    )
    view["shortcuts"] = [
        {"key": "Enter", "node_id": "go"},
        {"key": "d", "node_id": "off"},
    ]
    recorder = vis._LiveRecorder(object())
    recorder.host_live(json.dumps({"op": "open", "view": view}))
    assert recorder.press_shortcut("Enter") == {
        "is_accepted": True,
        "key": "Enter",
        "node_id": "go",
    }
    assert recorder.press_shortcut("d") == {
        "is_accepted": False,
        "reason": "disabled",
        "key": "d",
    }
    assert [(e["type"], e["node_id"]) for e in recorder._events] == [("activate", "go")]
    assert recorder.node("go")["clicks"] == 1


def test_recorder_select_queues_one_select_event_with_the_row_ids():
    # Live apps D3: the owner hears an accepted selection.
    view = app()
    view["nodes"].append(
        {
            "id": "t",
            "type": "table",
            "is_selectable": True,
            "columns": [{"id": "n", "label": "N"}],
            "rows": [{"id": "r1", "cells": ["1"]}, {"id": "r2", "cells": ["2"]}],
        }
    )
    heard = []
    recorder = vis._LiveRecorder(object())
    recorder.host_live(json.dumps({"op": "open", "view": view}))
    recorder._listener = heard.append
    recorder.select("t", ["r2"])
    assert [(e["type"], e["node_id"], e["item_ids"]) for e in recorder._events] == [
        ("select", "t", ["r2"])
    ]
    event = vis.LiveEvent._from_wire(heard[0])
    assert (event.type, event.node_id, event.item_ids) == ("select", "t", ["r2"])


def digits(value):
    return None if str(value).isdigit() else "must be digits"


def port_app():
    return vis.ui.app(
        "Ports",
        vis.ui.plaintext("port", label="Port", validate=digits),
        vis.ui.button("send", label="Send", action="submit"),
        key="ports",
    )


def test_validators_run_in_the_extension_before_the_host_accepts_a_submit(monkeypatch):
    # Live apps D4: the owner judges its inputs; the function never crosses as JSON.
    recorder = vis._LiveRecorder(object())
    monkeypatch.setattr(vis._host, "live", recorder.live)
    vis.attach(port_app())
    opened = recorder.said[0]
    assert opened["validators"] == ["port"]
    assert "validate" not in json.dumps(opened["view"])
    assert recorder.submit("send", {"port": "abc"}) == {
        "is_accepted": False,
        "reason": "invalid",
        "errors": {"port": "must be digits"},
    }
    assert recorder.submit("send", {"port": "8080"})["is_accepted"] is True
    assert [e["type"] for e in recorder._events].count("validate") == 0


def test_a_raising_or_false_validator_refuses_with_the_host_wording():
    def boom(value):
        raise RuntimeError("no network")

    errors = vis._judged(
        {"a": [boom], "b": [lambda v: False], "c": [lambda v, values: None]},
        {"a": "x", "b": "y", "c": "z"},
    )
    assert errors == {"a": "could not be validated: no network", "b": "is not valid"}
    assert vis._judged({"a": [boom]}, {"a": ""}) == {}


def test_the_app_door_answers_a_validate_question_and_never_calls_on_event():
    gateway = Gateway()
    heard = []
    app = Session(gateway, "s-1").attach(
        port_app(), owner="tools", on_event=heard.append
    )
    try:
        app._host_event(
            {
                "type": "validate",
                "request_id": "q-1",
                "node_id": "send",
                "values": {"port": "x1"},
            }
        )
        answers = gateway.envelopes("validated")
        assert answers == [
            {
                "op": "validated",
                "view_id": app.id,
                "request_id": "q-1",
                "errors": {"port": "must be digits"},
            }
        ]
        assert heard == []
    finally:
        app.close()


def test_events_judges_a_validate_question_where_the_door_pushes_nothing():
    # The sandbox door pushes no events, so the reader of events judges them.
    recorder = vis._LiveRecorder(object())
    app = vis.LiveAttachment._from(port_app(), door=recorder.host_live)
    recorder._events.append(
        {"type": "validate", "request_id": "q-2", "values": {"port": "80"}, "seq": 1}
    )
    recorder._cursor = 1
    recorder._verdicts["q-2"] = None
    assert app.events() == []
    assert recorder._verdicts["q-2"] == {}


def test_a_submit_without_an_owner_answer_is_refused():
    recorder = vis._LiveRecorder(object())
    recorder.host_live(
        json.dumps(
            {
                "op": "open",
                "validators": ["port"],
                "view": vis._live_validators(dict(port_app()))[0],
            }
        )
    )
    assert recorder.submit("send", {"port": "80"}) == {
        "is_accepted": False,
        "reason": "invalid",
        "errors": {"send": "The app did not answer. Try again."},
    }


def test_attach_refuses_on_event_for_a_file():
    with pytest.raises(TypeError, match="only for an app"):
        vis.attach(b"data", "a.txt", on_event=print)


class RestartingGateway(Gateway):
    """A gateway that can restart: it drops its owner and starts its events at 0."""

    def __init__(self):
        super().__init__()
        self.down = 0
        self.is_stale = False

    def restart(self):
        with self.engine._condition:
            self.engine._events = []
            self.engine._cursor = 0
            self.down = 2
            self.is_stale = True

    def _request(self, method, route, *, path=None, body=None, timeout=None, **_):
        from blockether.vis.engine._client import TransportError

        envelope = body["envelope"]
        if self.down:
            self.down -= 1
            self.calls.append((method, route, path, body, timeout))
            raise TransportError("connection refused")
        if self.is_stale and envelope["op"] == "events":
            self.calls.append((method, route, path, body, timeout))
            time.sleep(0.01)
            return Response(
                {
                    "view_id": "app-1",
                    "is_open": True,
                    "is_stale": True,
                    "events": [],
                    "cursor": 0,
                }
            )
        if envelope["op"] == "open":
            self.is_stale = False
        if envelope["op"] == "events":
            # Short polls, so that a restart never waits behind a long one.
            time.sleep(0.005)
            body = dict(body, envelope=dict(envelope, timeout_ms=0))
        return super()._request(method, route, path=path, body=body, timeout=timeout)


def test_an_adopting_owner_gets_no_old_event(monkeypatch):
    # Live apps D8: a second program that opens the same key reads from its open.
    gateway = Gateway()
    first = Session(gateway, "s-1").attach(app(), owner="bot")
    for _ in range(3):
        gateway.engine.activate("go")
    seen = []
    Session(gateway, "s-1").attach(app(), owner="bot", on_event=seen.append)
    assert gateway.envelopes("events")[0]["after"] == 3
    gateway.engine.activate("go")
    assert wait_for(lambda: seen)
    time.sleep(0.05)
    assert [e.type for e in seen] == ["activate"]
    first.close()


def test_the_door_opens_a_keyed_app_again_after_a_restart(monkeypatch, caplog):
    # Live apps D6: the poll waits for the gateway, opens the app again and reads
    # only the events after that open.
    monkeypatch.setattr(vis.time, "sleep", lambda s, nap=time.sleep: nap(0.001))
    gateway = RestartingGateway()
    seen = []
    handle = Session(gateway, "s-1").attach(app(), owner="bot", on_event=seen.append)
    gateway.engine.activate("go")
    assert wait_for(lambda: len(seen) == 1)
    gateway.restart()
    assert wait_for(lambda: len(gateway.envelopes("open")) == 2)
    assert gateway.envelopes("open")[1] == gateway.envelopes("open")[0]
    gateway.engine.activate("go")
    assert wait_for(lambda: len(seen) == 2)
    assert [e.type for e in seen] == ["activate", "activate"]
    assert "not reachable" in caplog.text
    handle.set_shortcuts([vis.ui.shortcut("n", "next")])
    assert gateway.engine._view["shortcuts"] == [{"key": "n", "action": "next"}]
    handle.close()


def test_a_keyless_app_stops_its_poll_after_a_restart(monkeypatch, caplog):
    monkeypatch.setattr(vis.time, "sleep", lambda s, nap=time.sleep: nap(0.001))
    gateway = RestartingGateway()
    keyless = vis.ui.app("Review", vis.ui.button("go", label="Go"))
    before = set(threading.enumerate())
    Session(gateway, "s-1").attach(keyless, owner="bot", on_event=lambda e: None)
    poller = next(
        t for t in set(threading.enumerate()) - before if t.name == "vis-app-app-1"
    )
    gateway.restart()
    poller.join(5)
    assert not poller.is_alive()
    assert len(gateway.envelopes("open")) == 1
    assert "has no key" in caplog.text


def test_the_poll_stops_when_its_client_has_ended(monkeypatch, caplog):
    # #8683: a closed client, or one that lost its lease for good, never answers.
    monkeypatch.setattr(vis.time, "sleep", lambda s, nap=time.sleep: nap(0.001))
    gateway = RestartingGateway()
    before = set(threading.enumerate())
    Session(gateway, "s-1").attach(app(), owner="bot", on_event=lambda e: None)
    poller = next(
        t for t in set(threading.enumerate()) - before if t.name == "vis-app-app-1"
    )
    gateway._lease_error = "client lease lost; create a new client"
    gateway.down = 1_000_000
    poller.join(5)
    assert not poller.is_alive()
    assert "its client ended" in caplog.text


def test_a_validated_input_added_by_patch_declares_its_validator(monkeypatch):
    # Live apps D4b: validators follow the node tree, also after the open.
    recorder = vis._LiveRecorder(object())
    monkeypatch.setattr(vis._host, "live", recorder.live)
    handle = vis.attach(
        vis.ui.app(
            "Ports", vis.ui.button("send", label="Send", action="submit"), key="p"
        ),
        on_event=lambda event: None,
    )
    handle.add(vis.ui.plaintext("port", label="Port", validate=digits))
    patched = [e for e in recorder.said if e["op"] == "patch"][-1]
    assert patched["validators"] == ["port"]
    assert "validate" not in json.dumps(patched["patch"])
    assert recorder.submit("send", {"port": "abc"})["errors"] == {
        "port": "must be digits"
    }
    handle.drop("port")
    assert [e for e in recorder.said if e["op"] == "patch"][-1]["validators"] == []
    assert recorder.submit("send", {})["is_accepted"] is True


def test_a_dropped_group_drops_the_validators_of_its_inputs(monkeypatch):
    # Live apps D4b: a group takes its validated inputs with it. An input that
    # comes back with the same id is not judged by the old validator.
    recorder = vis._LiveRecorder(object())
    monkeypatch.setattr(vis._host, "live", recorder.live)
    handle = vis.attach(
        vis.ui.app(
            "Ports", vis.ui.button("send", label="Send", action="submit"), key="p"
        ),
        on_event=lambda event: None,
    )
    handle.add(
        vis.ui.column(
            "net",
            vis.ui.plaintext("port", label="Port", validate=digits),
            vis.ui.column(
                "deep", vis.ui.plaintext("host", label="Host", validate=digits)
            ),
        )
    )
    patches = lambda: [e for e in recorder.said if e["op"] == "patch"]  # noqa: E731
    assert patches()[-1]["validators"] == ["host", "port"]
    handle.drop("net")
    assert patches()[-1]["validators"] == []
    assert "port" not in handle and "host" not in handle
    handle.add(vis.ui.plaintext("port", label="Port"))
    handle.add(vis.ui.plaintext("zip", label="Zip", validate=digits))
    assert patches()[-1]["validators"] == ["zip"]
    assert recorder.submit("send", {"port": "abc"})["is_accepted"] is True


def test_the_door_opens_an_app_again_with_its_current_validators(monkeypatch):
    monkeypatch.setattr(vis.time, "sleep", lambda s, nap=time.sleep: nap(0.001))
    gateway = RestartingGateway()
    handle = Session(gateway, "s-1").attach(app(), owner="bot", on_event=lambda e: None)
    handle.add(vis.ui.plaintext("port", label="Port", validate=digits))
    assert gateway.envelopes("patch")[-1]["validators"] == ["port"]
    gateway.restart()
    assert wait_for(lambda: len(gateway.envelopes("open")) == 2)
    assert gateway.envelopes("open")[1]["validators"] == ["port"]
    handle.close()


def test_shortcut_lists_fail_early_in_the_sdk(monkeypatch):
    # Live apps D7: the SDK refuses a bad shortcut list before the host does.
    r = vis.ui.shortcut("r", "refresh")
    with pytest.raises(ValueError, match="given twice"):
        vis.ui.app("A", vis.ui.button("go", label="Go"), shortcuts=[r, r])
    many = [vis.ui.shortcut(chr(ord("a") + i % 26) * 1, "x") for i in range(26)]
    many += [vis.ui.shortcut(k, "x") for k in "0123456"]
    with pytest.raises(ValueError, match="at most 32"):
        vis.ui.app("A", vis.ui.button("go", label="Go"), shortcuts=many)
    for bad, message in (
        ({"key": "r"}, "exactly one"),
        ({"key": "r", "action": "a", "node_id": "go"}, "exactly one"),
        ({"key": "Escape", "action": "a"}, "ArrowUp"),
        ({"key": "r", "action": "a", "when": "x"}, "no field"),
    ):
        with pytest.raises(ValueError, match=message):
            vis.ui.app("A", vis.ui.button("go", label="Go"), shortcuts=[bad])
        with pytest.raises(ValueError, match=message):
            vis.live("A", [vis.ui.button("go", label="Go")], shortcuts=[bad])
    gateway = Gateway()
    handle = Session(gateway, "s-1").attach(app(), owner="bot")
    sent = len(gateway.calls)
    with pytest.raises(ValueError, match="given twice"):
        handle.set_shortcuts([r, r])
    assert len(gateway.calls) == sent
    handle.close()
