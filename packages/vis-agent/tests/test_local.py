"""Owned local-engine process lifecycle; real-engine coverage is explicitly selected."""

import os
import shlex

import pytest
from blockether.vis.engine import LocalEngine, TransportError


def test_missing_executable_is_reported_without_a_live_process(tmp_path):
    engine = LocalEngine(executable=str(tmp_path / "missing"), root=tmp_path)
    with pytest.raises(TransportError):
        engine.connect()
    engine.close()


def test_real_local_engine(tmp_path):
    command = os.environ.get("VIS_TEST_LOCAL_COMMAND")
    if not command:
        pytest.skip("set VIS_TEST_LOCAL_COMMAND to exercise the real Vis engine")
    with LocalEngine(
        executable=shlex.split(command), root=tmp_path, startup_timeout=180
    ) as engine:
        assert engine.get_capabilities()["protocol"]["protocol"] > 0
        session = engine.create_session(title="SDK local test", root=str(tmp_path))
        assert session.read()["id"] == session.id
        assert session.turns() == []
        session.update(title="SDK updated")
        assert session.read()["title"] == "SDK updated"
        session.delete()
        process = engine._process
    assert process.poll() is not None


@pytest.mark.parametrize(
    "code, error",
    [
        ("import time; time.sleep(30)", "timeout"),
        ("print('{\"protocol\": -1}', flush=True)", "protocol"),
        ('print("not json", flush=True)', "protocol"),
        ("pass", "transport"),
    ],
)
def test_startup_failure_closes_owned_process(tmp_path, code, error):
    import sys

    from blockether.vis.engine import ProtocolError, VisTimeout

    expected = {
        "timeout": VisTimeout,
        "protocol": ProtocolError,
        "transport": TransportError,
    }[error]
    engine = LocalEngine(
        executable=[sys.executable, "-c", code], root=tmp_path, startup_timeout=0.2
    )
    with pytest.raises(expected):
        engine.connect()
    assert engine._process.poll() is not None
    assert not os.path.exists(engine._home.name)


def test_request_timeout_preserves_error_and_closes_process(tmp_path):
    import json
    import sys

    from blockether.vis._contracts import GATEWAY
    from blockether.vis.engine import VisTimeout

    hello = json.dumps({"protocol": GATEWAY["protocol"]["version"]})
    code = f"import time; print({hello!r}, flush=True); time.sleep(30)"
    with LocalEngine(
        executable=[sys.executable, "-c", code], root=tmp_path, timeout=0.1
    ) as engine:
        with pytest.raises(VisTimeout):
            engine.get_capabilities()
        assert engine._process.poll() is not None


def test_local_event_polling_uses_the_canonical_cursor_key(monkeypatch, tmp_path):
    engine = LocalEngine(executable="unused", root=tmp_path)
    calls = []

    def page(sid, *, query):
        calls.append(query)
        return {"events": [{"type": "turn.completed", "session_id": sid, "seq": 8}]}

    monkeypatch.setattr(engine, "get_session_events_since", page)
    with engine.session("s").events(cursor=7) as events:
        assert next(events).seq == 8
    assert calls == [{"cursor": 7}]
    engine.close()


@pytest.mark.parametrize("kind", ["session", "job"])
def test_local_event_polling_has_a_bounded_idle_timeout(monkeypatch, tmp_path, kind):
    from blockether.vis.engine import VisTimeout

    engine = LocalEngine(executable="unused", root=tmp_path, timeout=0.01)
    polls = []

    def page(*args, **kwargs):
        polls.append(1)
        if len(polls) > 1:
            raise AssertionError("polled after the idle deadline")
        return (
            {"events": []}
            if kind == "session"
            else {"id": "j", "phase": "running", "is_done": False}
        )

    monkeypatch.setattr(engine, "get_session_events_since", page)
    monkeypatch.setattr(engine, "get_speech_job", page)
    with (
        engine.session("s").events()
        if kind == "session"
        else engine.speech_events("j") as events
    ):
        if kind == "job":
            assert next(events).phase == "running"
        with pytest.raises(VisTimeout):
            next(events)
    engine.close()


def test_invalid_local_event_does_not_advance_the_cursor(monkeypatch, tmp_path):
    from blockether.vis.engine import ProtocolError

    engine = LocalEngine(executable="unused", root=tmp_path)
    monkeypatch.setattr(
        engine,
        "get_session_events_since",
        lambda *a, **k: {
            "events": [
                {"type": "block.activity", "session_id": "s", "seq": 8, "activity": {}}
            ]
        },
    )
    with engine.session("s").events(cursor=7) as events:
        with pytest.raises(ProtocolError):
            next(events)
        assert events.cursor == 7
    engine.close()


@pytest.mark.parametrize(
    "scope,kind",
    [(scope, kind) for scope in ("", "session_") for kind in ("speech", "voice")],
)
def test_local_job_events_poll_the_canonical_resource(
    monkeypatch, tmp_path, scope, kind
):
    engine = LocalEngine(executable="unused", root=tmp_path)
    calls = []
    args = ("s", "j") if scope else ("j",)

    def snapshot(*received, **kwargs):
        calls.append(received)
        return {"id": "j", "phase": "completed", "is_done": True}

    monkeypatch.setattr(engine, f"get_{scope}{kind}_job", snapshot)
    with getattr(engine, f"{scope}{kind}_events")(*args) as events:
        result = list(events)
        assert result[0].id == "j"
        assert result[0].is_done
        assert result[0].type == ("speech.job" if kind == "speech" else "voice.job")
    assert calls == [args]
    assert not engine._streams
    engine.close()


def test_stream_protocol_failure_closes_the_owned_process(tmp_path):
    import sys

    from blockether.vis._contracts import GATEWAY
    from blockether.vis.engine import ProtocolError

    code = (
        f"import sys; print('{{\"protocol\": {GATEWAY['protocol']['version']}}}', flush=True); "
        "sys.stdin.readline(); print('not-json', flush=True); sys.stdin.read()"
    )
    with LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path) as engine:
        with engine.session("s").events() as events:
            with pytest.raises(ProtocolError):
                next(events)
        assert engine._process.poll() is not None
        assert not os.path.exists(engine._home.name)


def test_local_errors_preserve_the_canonical_gateway_code(tmp_path):
    import base64
    import json
    import sys

    from blockether.vis._contracts import GATEWAY
    from blockether.vis.engine import GatewayError

    error = base64.b64encode(
        json.dumps({"error": {"type": "not_found"}}).encode()
    ).decode()
    hello = json.dumps({"protocol": GATEWAY["protocol"]["version"]})
    reply = json.dumps({"status": 404, "headers": {}, "content": error})
    code = f"import sys; print({hello!r}, flush=True); sys.stdin.readline(); print({reply!r}, flush=True); sys.stdin.read()"
    with LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path) as engine:
        with pytest.raises(GatewayError) as failure:
            engine.get_session("s")
        assert failure.value.status == 404
        assert failure.value.code == "not_found"
