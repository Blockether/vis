"""Installed SDK against real HTTP/stdio engines and a deterministic model endpoint.

Set VIS_TEST_LOCAL_COMMAND to the engine argv. No gateway discovery, existing
sessions, remote models or user configuration are used. Only the model is a double.
"""

import json
import os
import shlex
import signal
import socket
import subprocess
import tempfile
import threading
import time
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import pytest
from blockether.vis.client import Event, GatewayClient, GatewayError, ProtocolError
from blockether.vis.local import LocalEngine

EXTENSION = """from blockether import vis

def sdk_flow(mode):
    "Run a deterministic tool, live View, form and state update."
    with vis.live("SDK flow", [vis.status("now", "Waiting", tone="running")], flush_ms=0) as view:
        answer = vis.ask("SDK " + mode, [vis.plaintext("name", label="Name", is_required=True)], timeout_ms=60000)
        if not answer:
            return {"reason": answer.reason}
        view.status("Answered", tone="ok")
        vis.state["name"] = answer["name"]
        return {"name": vis.state["name"]}

vis.extension(name="sdk-fixture", description="SDK integration fixture", alias="sdk",
    symbols=[vis.symbol(sdk_flow, activity=vis.Activity(presenter="observation", label="SDK flow"))])
"""


@contextmanager
def model_endpoint():
    requests = []

    class Model(BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            if not body.get("tools"):
                delta, finish = {"content": "SDK fixture"}, "stop"
            else:
                requests.append(body)
                position = len(requests)
                if position % 2:
                    mode = "complete" if position == 1 else "cancel"
                    delta = {
                        "tool_calls": [
                            {
                                "index": 0,
                                "id": f"call_{position}",
                                "type": "function",
                                "function": {
                                    "name": "python_execution",
                                    "arguments": json.dumps(
                                        {"code": f"print(await sdk_flow({mode!r}))"}
                                    ),
                                },
                            }
                        ]
                    }
                    finish = "tool_calls"
                else:
                    delta, finish = {"content": "SDK flow completed"}, "stop"
            value = {
                "id": "fixture",
                "object": "chat.completion.chunk",
                "model": "sdk-test",
                "choices": [{"index": 0, "delta": delta, "finish_reason": finish}],
            }
            data = ("data: " + json.dumps(value) + "\n\ndata: [DONE]\n\n").encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

    server = ThreadingHTTPServer(("127.0.0.1", 0), Model)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}/v1", requests
    finally:
        server.shutdown()
        server.server_close()
        thread.join(2)


@contextmanager
def real_client(transport, command, work):
    if transport == "stdio":
        engine = LocalEngine(
            executable=command, root=work, timeout=60, startup_timeout=180
        )
        try:
            with engine:
                yield engine
        finally:
            engine.close()
            if engine._process is not None:
                assert engine._process.poll() is not None
            if engine._home is not None:
                assert not Path(engine._home.name).exists()
        return
    with socket.socket() as reservation:
        reservation.bind(("127.0.0.1", 0))
        port = reservation.getsockname()[1]
    token = work / "gateway-token"
    token.write_text("sdk-fixture-token")
    token.chmod(0o600)
    with tempfile.TemporaryFile() as output:
        process = subprocess.Popen(
            [
                *command,
                "gateway",
                "start",
                "--host",
                "127.0.0.1",
                "--port",
                str(port),
                "--db",
                str(work / "http.sqlite"),
                "--require-token",
                "--token-file",
                str(token),
            ],
            cwd=work,
            stdout=output,
            stderr=output,
            start_new_session=True,
        )
        try:
            deadline = time.monotonic() + 180
            while True:
                assert process.poll() is None, "fixture gateway exited before binding"
                try:
                    with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                        break
                except OSError:
                    assert time.monotonic() < deadline, (
                        "fixture gateway startup timed out"
                    )
                    time.sleep(0.1)
            url = f"http://127.0.0.1:{port}"
            with pytest.raises(GatewayError) as unauthorized:
                GatewayClient(url, token="not-the-fixture-token").connect()
            assert unauthorized.value.status == 401
            with GatewayClient(url, token="sdk-fixture-token", timeout=60) as client:
                yield client
            # SDK close releases its lease, not somebody else's server.
            assert process.poll() is None
        finally:
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                process.wait(10)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(5)
        assert process.poll() is not None


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_agent_tool_view_activity_and_cancellation(
    tmp_path, monkeypatch, transport
):
    raw = os.environ.get("VIS_TEST_LOCAL_COMMAND")
    if not raw:
        pytest.skip("set VIS_TEST_LOCAL_COMMAND to the actual engine argv")
    home = tmp_path / "home"
    config = home / ".vis"
    config.mkdir(parents=True)
    work = tmp_path / "project"
    extensions = work / ".vis" / "extensions"
    extensions.mkdir(parents=True)
    (extensions / "fixture.py").write_text(EXTENSION)
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("JAVA_TOOL_OPTIONS", f"-Duser.home={home}")
    monkeypatch.delenv("VIS_GATEWAY_URL", raising=False)
    # This fixture has no user data; diagnose unsolicited stdout without allowing it.
    decode = json.loads

    def checked_json(data, *args, **kwargs):
        try:
            return decode(data, *args, **kwargs)
        except json.JSONDecodeError:
            pytest.fail(f"non-JSON fixture transport output: {data[:200]!r}")

    monkeypatch.setattr(json, "loads", checked_json)
    decode_event = Event.from_wire

    def checked_event(value):
        try:
            return decode_event(value)
        except ProtocolError:
            pytest.fail(f"invalid fixture event: {value!r}")

    monkeypatch.setattr(Event, "from_wire", checked_event)
    with model_endpoint() as (model_url, requests):
        (config / "config.yml").write_text(
            json.dumps(
                {
                    "providers": [
                        {
                            "id": "sdk-fixture",
                            "api_style": "openai",
                            "base_url": model_url,
                            "api_key": "fixture",
                            "is_stateless": True,
                            "models": [
                                {
                                    "name": "sdk-test",
                                    "context": 200000,
                                    "is_tool_call": True,
                                }
                            ],
                        }
                    ],
                    "default_provider": "sdk-fixture",
                    "default_model": "sdk-test",
                }
            )
        )
        command = shlex.split(raw)
        if len(command) == 1:
            # Native images consume system properties before application arguments.
            command.append(f"-Duser.home={home}")
        with real_client(transport, command, work) as client:
            session = client.create_session(
                title="SDK fixture", root=str(work), channel="app"
            )
            for mode in ("complete", "cancel"):
                turn = session.send(mode)
                seen = []
                with session.events(cursor=turn.cursor, reconnects=0) as events:
                    for event in events:
                        seen.append(event)
                        if event.type == "iteration.error":
                            pytest.fail(
                                f"fixture engine error: {str(event.data)[:1200]}"
                            )
                        if event.type == "view.open":
                            if event.view.kind == "live":
                                assert any(
                                    v.id == event.view.view_id
                                    for v in session.live_views()
                                )
                            else:
                                assert any(
                                    v.id == event.view.view_id
                                    for v in session.input_views()
                                )
                                if mode == "complete":
                                    assert session.answer(
                                        event.view.view_id, {"name": "Ada"}
                                    )["is_accepted"]
                                else:
                                    turn.cancel()
                        if event.type in {
                            "turn.completed",
                            "turn.failed",
                            "turn.cancelled",
                        }:
                            break
                        assert len(seen) < 200, [e.type for e in seen]
                if not any(
                    e.type == "view.open" and e.view.kind == "input" for e in seen
                ):
                    pytest.fail(
                        "missing input View; fixture tool evidence: "
                        + repr(
                            [
                                (e.type, e.data)
                                for e in seen
                                if e.type.startswith("block.")
                            ]
                        )[:3000]
                    )
                closed_views = {
                    e.view.kind: e.view.result for e in seen if e.type == "view.close"
                }
                assert set(closed_views) == {"input", "live"}
                # Public close receipts must not disclose the form answer. HTTP
                # omits the live picture already delivered by open/patch; stdio
                # polling reads the durable receipt, which retains that picture.
                assert set(closed_views["input"].to_wire()) == {"reason"}
                assert (closed_views["live"].view is None) == (transport == "http")
                assert any(e.activity is not None for e in seen)
                result = turn.wait(timeout=30)
                assert result["status"] == (
                    "completed" if mode == "complete" else "cancelled"
                ), result
                assert session.input_views() == []
                assert session.live_views() == []
                if mode == "complete":
                    assert any(e.type == "view.patch" for e in seen)
                    assert any(e.activity and e.activity.counts.succeeded for e in seen)
            assert len(requests) == 3
            assert session.transcript().content
            assert len(session.turns()) == 2
            session.delete()
