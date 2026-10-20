"""Run the Companion Shortcut actions (Swift) against a real gateway.

Set VIS_TEST_LOCAL_COMMAND to the engine argv. The test also needs the Swift
toolchain. Only the model is a double, so no paid model calls are made.
"""

import json
import os
import shlex
import shutil
import subprocess
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from threading import Thread

import pytest
from test_engine import PROVIDER_EXTENSION, real_client

CORE = Path(__file__).resolve().parents[3] / "apps/vis-companion/native/shortcuts"


def _text(message):
    content = message.get("content")
    if isinstance(content, list):
        return "".join(
            part.get("text", "") for part in content if isinstance(part, dict)
        )
    return content or ""


def _answer(body):
    """The scripted final answer, chosen by the last user message."""
    users = [_text(m) for m in body.get("messages", []) if m.get("role") == "user"]
    last = users[-1] if users else ""
    if "is not a valid structured result" in last:
        return '{"city": "Paris"}'
    if "Match this JSON Schema" in last:
        return '{"city": 3}'
    return "Plain fixture answer"


@contextmanager
def scripted_model():
    class Model(BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            content = _answer(body) if body.get("tools") else "Shortcuts fixture"
            value = {
                "id": "fixture",
                "object": "chat.completion.chunk",
                "model": "sdk-test",
                "choices": [
                    {"index": 0, "delta": {"content": content}, "finish_reason": "stop"}
                ],
            }
            data = ("data: " + json.dumps(value) + "\n\ndata: [DONE]\n\n").encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

    server = ThreadingHTTPServer(("127.0.0.1", 0), Model)
    thread = Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_address[1]}/v1"
    finally:
        server.shutdown()
        server.server_close()
        thread.join(2)


def test_swift_shortcut_actions_against_a_real_gateway(tmp_path, monkeypatch):
    raw = os.environ.get("VIS_TEST_LOCAL_COMMAND")
    if not raw:
        pytest.skip("set VIS_TEST_LOCAL_COMMAND to the actual engine argv")
    swift = shutil.which("swift")
    if swift is None:
        pytest.skip("the Swift toolchain is not installed")
    home = tmp_path / "home"
    config = home / ".vis"
    config.mkdir(parents=True)
    work = tmp_path / "project"
    extensions = work / ".vis" / "extensions"
    extensions.mkdir(parents=True)
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("JAVA_TOOL_OPTIONS", f"-Duser.home={home}")
    monkeypatch.delenv("VIS_GATEWAY_URL", raising=False)
    protocol = json.loads(
        (
            Path(__file__).resolve().parents[3]
            / "packages/vis-contract/resources/vis-contract/schema/gateway.json"
        ).read_text()
    )["$defs"]["handshake"]["properties"]["protocol"]["const"]
    with scripted_model() as model_url:
        (extensions / "provider.py").write_text(
            PROVIDER_EXTENSION.replace("__MODEL_URL__", repr(model_url))
            .replace("__MANAGED__", repr(True))
            .replace("__CREDENTIAL_HEADERS__", repr(None))
        )
        (config / "config.yml").write_text(
            json.dumps(
                {
                    "providers": [],
                    "toggles": {"council": False, "subagents": False},
                    "default_provider": "sdk-fixture",
                    "default_model": "sdk-test",
                }
            )
        )
        command = shlex.split(raw)
        if len(command) == 1:
            command.append(f"-Duser.home={home}")
        with real_client("http", command, work) as client:
            env = {
                **os.environ,
                "VIS_SHORTCUTS_LIVE_URL": client._url,
                "VIS_SHORTCUTS_LIVE_TOKEN": "sdk-fixture-token",
                "VIS_SHORTCUTS_LIVE_PROTOCOL": str(protocol),
            }
            result = subprocess.run(
                [
                    swift,
                    "test",
                    "--package-path",
                    str(CORE),
                    "--filter",
                    "LiveGatewayTests",
                ],
                env=env,
                capture_output=True,
                text=True,
                timeout=900,
            )
    output = result.stdout + result.stderr
    assert result.returncode == 0, output[-6000:]
    assert "Executed 2 tests, with 0 failures" in output, output[-6000:]
    assert "skipped" not in output, output[-6000:]
