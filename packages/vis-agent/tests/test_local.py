"""Owned local-engine process lifecycle; real-engine coverage is explicitly selected."""

import os
import shlex

import pytest
from blockether.vis._contracts import definition
from blockether.vis.engine import LocalEngine, TransportError

_PROTOCOL = definition("gateway", "handshake")["properties"]["protocol"]["const"]


def test_missing_executable_is_reported_without_a_live_process(tmp_path):
    engine = LocalEngine(executable=str(tmp_path / "missing"), root=tmp_path)
    with pytest.raises(TransportError):
        engine.connect()
    engine.close()


def test_local_engine_starts_stdio_command(tmp_path):
    import json
    import sys

    hello = json.dumps({"protocol": _PROTOCOL})
    code = (
        "import sys; assert sys.argv[1:] == ['stdio']; "
        f"print({hello!r}, flush=True); sys.stdin.read()"
    )
    with LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path) as engine:
        assert engine._process.poll() is None


def test_real_local_engine(tmp_path, monkeypatch):
    command = os.environ.get("VIS_TEST_LOCAL_COMMAND")
    if not command:
        pytest.skip("set VIS_TEST_LOCAL_COMMAND to exercise the real Vis engine")
    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("JAVA_TOOL_OPTIONS", f"-Duser.home={home}")
    monkeypatch.delenv("VIS_GATEWAY_URL", raising=False)
    # A lifecycle test must not depend on the developer's login or provider config.
    import json

    config = tmp_path / ".vis"
    config.mkdir()
    (config / "config.yml").write_text(
        json.dumps(
            {
                "default_provider": "sdk-lifecycle",
                "default_model": "lifecycle-model",
                "providers": [
                    {
                        "id": "sdk-lifecycle",
                        "base_url": "http://127.0.0.1:1/v1",
                        "compatibility": "openai",
                        "models": [
                            {
                                "name": "lifecycle-model",
                                "context": 32000,
                                "output_limit": 4096,
                                "is_tool_call": True,
                            }
                        ],
                    }
                ],
            }
        )
    )
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


@pytest.mark.parametrize("ignore_term", [False, True], ids=["term", "kill"])
def test_close_falls_back_to_owned_process_when_group_signal_is_denied(
    tmp_path, monkeypatch, ignore_term
):
    import json
    import signal
    import sys

    hello = json.dumps({"protocol": _PROTOCOL})
    code = f"""import signal, time
signal.signal(signal.SIGTERM, signal.SIG_IGN if {ignore_term!r} else signal.SIG_DFL)
print({hello!r}, flush=True)
time.sleep(60)
"""
    engine = LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path)
    engine.connect()
    process = engine._process
    group_signals = []
    direct_signals = []
    send_signal = process.send_signal

    def denied(pgid, sig):
        assert pgid == process.pid
        assert os.getpgid(process.pid) == process.pid != os.getpgrp()
        group_signals.append(sig)
        raise PermissionError("group signal denied")

    def owned_signal(sig):
        direct_signals.append(sig)
        send_signal(sig)

    monkeypatch.setattr(os, "killpg", denied)
    monkeypatch.setattr(process, "send_signal", owned_signal)
    try:
        engine.close()
        expected = [signal.SIGTERM, signal.SIGKILL] if ignore_term else [signal.SIGTERM]
        assert group_signals == direct_signals == expected
        assert process.poll() is not None
        assert process.stdin.closed and process.stdout.closed
        assert not os.path.exists(engine._home.name)
        engine.close()
        assert group_signals == direct_signals == expected
    finally:
        if process.poll() is None:
            send_signal(signal.SIGKILL)
        process.wait(timeout=5)
        process.stdin.close()
        process.stdout.close()
        engine._home.cleanup()


def test_failed_close_is_terminal_but_cleanup_can_be_retried(tmp_path, monkeypatch):
    import json
    import signal
    import sys

    hello = json.dumps({"protocol": _PROTOCOL})
    code = f"import time; print({hello!r}, flush=True); time.sleep(60)"
    engine = LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path)
    engine.connect()
    process = engine._process
    send_signal = process.send_signal

    def denied(*args):
        raise PermissionError("signal denied")

    try:
        with monkeypatch.context() as denied_signals:
            denied_signals.setattr(os, "killpg", denied)
            denied_signals.setattr(process, "send_signal", denied)
            with pytest.raises(PermissionError, match="signal denied"):
                engine.close()
        assert process.poll() is None
        assert os.path.exists(engine._home.name)
        with pytest.raises(TransportError, match="closed"):
            engine.connect()
        engine.close()
        assert process.poll() is not None
        assert process.stdin.closed and process.stdout.closed
        assert not os.path.exists(engine._home.name)
        engine.close()
    finally:
        if process.poll() is None:
            send_signal(signal.SIGKILL)
        process.wait(timeout=5)
        process.stdin.close()
        process.stdout.close()
        engine._home.cleanup()


def test_close_signals_owned_group_and_reaps_its_child(tmp_path):
    import json
    import signal
    import sys

    hello = json.dumps({"protocol": _PROTOCOL})
    stopped = tmp_path / "child-stopped"
    child_code = f"""import signal, sys, time
from pathlib import Path
def stop(sig, frame):
    Path({str(stopped)!r}).write_text("stopped")
    sys.exit(0)
signal.signal(signal.SIGTERM, stop)
print("ready", flush=True)
time.sleep(60)
"""
    code = f"""import signal, subprocess, sys, time
child = subprocess.Popen([sys.executable, "-c", {child_code!r}], stdout=subprocess.PIPE)
assert child.stdout.readline().strip() == b"ready"
def stop(sig, frame):
    child.wait(timeout=5)
    sys.exit(0)
signal.signal(signal.SIGTERM, stop)
print({hello!r}, flush=True)
time.sleep(60)
"""
    engine = LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path)
    engine.connect()
    process = engine._process
    try:
        engine.close()
        assert process.returncode == 0
        assert stopped.read_text() == "stopped"
        assert process.stdin.closed and process.stdout.closed
        assert not os.path.exists(engine._home.name)
    finally:
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGKILL)
        process.wait(timeout=5)
        process.stdin.close()
        process.stdout.close()
        engine._home.cleanup()


def test_request_timeout_preserves_error_and_closes_process(tmp_path):
    import json
    import sys

    from blockether.vis.engine import VisTimeout

    hello = json.dumps({"protocol": _PROTOCOL})
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

    from blockether.vis.engine import ProtocolError

    code = (
        f"import sys; print('{{\"protocol\": {_PROTOCOL}}}', flush=True); "
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

    from blockether.vis.engine import GatewayError

    error = base64.b64encode(
        json.dumps({"error": {"type": "not_found"}}).encode()
    ).decode()
    hello = json.dumps({"protocol": _PROTOCOL})
    reply = json.dumps({"status": 404, "headers": {}, "content": error})
    code = f"import sys; print({hello!r}, flush=True); sys.stdin.readline(); print({reply!r}, flush=True); sys.stdin.read()"
    with LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path) as engine:
        with pytest.raises(GatewayError) as failure:
            engine.get_session("s")
        assert failure.value.status == 404
        assert failure.value.code == "not_found"


def test_execution_layers_are_siblings_without_http_state(tmp_path):
    from blockether.vis.engine import ExecutionLayer, GatewayClient

    assert issubclass(LocalEngine, ExecutionLayer)
    assert issubclass(GatewayClient, ExecutionLayer)
    assert not issubclass(LocalEngine, GatewayClient)
    engine = LocalEngine(root=tmp_path)
    assert engine.timeout == 30
    assert engine._command == ["vis-agent"]
    for name in ("_url", "_token", "_opener", "_heartbeat", "_heartbeat_stop"):
        assert not hasattr(engine, name)
    engine.close()


def test_local_client_lease_uses_only_the_whitelisted_frame_header(tmp_path):
    import json
    import sys

    hello = json.dumps({"protocol": _PROTOCOL})
    code = f"""import base64, json, sys
print({hello!r}, flush=True)
for count, line in enumerate(sys.stdin, 1):
    request = json.loads(line)
    if request["route"] == "/v1/clients":
        assert request["method"] == "POST"
        assert "headers" not in request
        assert request["body"] == {{"kind": "python-sdk", "pid": {os.getpid()}}}
        value = {{"client_id": "local-owner"}}
    else:
        value = {{"request": request, "count": count}}
    content = base64.b64encode(json.dumps(value).encode()).decode()
    print(json.dumps({{"status": 200, "headers": {{}}, "content": content}}), flush=True)
"""
    with LocalEngine(executable=[sys.executable, "-c", code], root=tmp_path) as engine:
        assert engine._ensure_client_lease() == "local-owner"
        assert engine._ensure_client_lease() == "local-owner"
        reply = engine.get_capabilities()
        assert reply["count"] == 2
        assert reply["request"]["headers"] == {"x-vis-client-id": "local-owner"}
    with pytest.raises(TransportError, match="closed"):
        engine._ensure_client_lease()
