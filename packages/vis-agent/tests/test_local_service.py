"""The shared local gateway of `start(shared=True)` (#489); real-engine coverage is explicitly selected."""

import contextlib
import json
import os
import shlex
import subprocess
import sys
import textwrap
import time
from pathlib import Path

import pytest
from blockether.vis._contracts import definition
from blockether.vis.engine import GatewayClient, TransportError, start, stop_shared

_HANDSHAKE = definition("gateway", "handshake")["properties"]
_SRC = str(Path(__file__).resolve().parents[1] / "src")

# A gateway with the routes that the service and a client lease use. Each start
# adds a line to `starts`, so a test can count the gateways.
_FAKE_GATEWAY = textwrap.dedent(
    """
    import json, os, sys, threading
    from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
    from pathlib import Path

    args = [arg for arg in sys.argv[1:] if not arg.startswith("-D")]
    assert args[:2] == ["gateway", "start"], args
    # A native engine reads properties as arguments, a JVM from JAVA_TOOL_OPTIONS.
    java = os.environ.get("JAVA_TOOL_OPTIONS", "").split()
    properties = [arg for arg in sys.argv[1:] + java if arg.startswith("-D")]
    option = dict(zip(args[2::2], args[3::2]))
    assert "--require-token" in args and option["--host"] == "127.0.0.1"
    token = Path(option["--token-file"]).read_text().strip()
    assert os.environ["VIS_DB_PATH"] == option["--db"]
    assert os.path.isdir(os.environ["VIS_RUNTIME_DIR"])
    assert os.environ["VIS_GATEWAY_MANAGED"] == "1"
    with open(Path(option["--db"]).parent / "starts", "a") as starts:
        starts.write(f"{os.getpid()}\\n")
    (Path(option["--db"]).parent / "properties").write_text(json.dumps(properties))
    leases = set()

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *args):
            pass

        def answer(self, body, status=200):
            data = json.dumps(body).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def authorized(self):
            if self.headers.get("Authorization") == "Bearer " + token:
                return True
            self.answer({"error": "unauthorized", "message": "token"}, 401)
            return False

        def do_GET(self):
            if self.path == "/healthz":
                secret = self.headers.get("X-Vis-Gateway-Secret")
                return self.answer({"status": "ok", "pid": os.getpid(),
                                    "secret_match": secret == token,
                                    "clients": len(leases), "running_turns": 0})
            if not self.authorized():
                return
            if self.path == "/v1/capabilities":
                return self.answer({"protocol": {"protocol": PROTOCOL,
                                                 "min_client": 1}})
            return self.answer({"status": "ok"})

        def do_POST(self):
            length = int(self.headers.get("Content-Length") or 0)
            self.rfile.read(length)
            if not self.authorized():
                return
            if self.path == "/v1/clients":
                lease = f"lease-{len(leases)}"
                leases.add(lease)
                return self.answer({"client_id": lease})
            if self.path == "/v1/admin/stop":
                self.answer({"status": "stopping"})
                threading.Thread(target=server.shutdown).start()
                return
            self.answer({"error": "not_found", "message": self.path}, 404)

        def do_DELETE(self):
            if self.authorized():
                leases.discard(self.path.rsplit("/", 1)[-1])
                self.answer({"status": "released"})
                # A managed gateway stops when its last lease ends.
                if not leases:
                    threading.Thread(target=server.shutdown).start()

    server = ThreadingHTTPServer(("127.0.0.1", int(option["--port"])), Handler)
    server.serve_forever()
    """
).replace("PROTOCOL", str(_HANDSHAKE["protocol"]["const"]))


@pytest.fixture
def gateway(tmp_path):
    """The fake gateway command and a private service folder; stop it at the end."""
    script = tmp_path / "fake_gateway.py"
    script.write_text(_FAKE_GATEWAY)
    home = tmp_path / "service"
    yield [sys.executable, str(script)], home
    stop_shared(home=home)


def _starts(home):
    path = home / "starts"
    return path.read_text().split() if path.exists() else []


def _wait_exit(pid, seconds=10):
    deadline = time.monotonic() + seconds
    while _alive(pid) and time.monotonic() < deadline:
        time.sleep(0.05)


def _alive(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    return True


def test_second_ensure_reuses_the_running_gateway_fast(gateway):
    command, home = gateway
    with start(shared=True, executable=command, home=home) as first:
        assert isinstance(first, GatewayClient)
        assert first.get_capabilities()["protocol"]["protocol"] > 0
        started = time.monotonic()
        with start(shared=True, executable=command, home=home) as second:
            assert time.monotonic() - started < 1
            assert second._url == first._url
    assert len(_starts(home)) == 1


def test_gateway_stops_after_its_last_client_and_starts_again(gateway):
    # A forgotten gateway must not run forever: it stops when the last lease ends.
    command, home = gateway
    with start(shared=True, executable=command, home=home):
        pid = json.loads((home / "gateway.json").read_text())["pid"]
    _wait_exit(pid)
    assert not _alive(pid)
    with start(shared=True, executable=command, home=home):
        assert json.loads((home / "gateway.json").read_text())["pid"] != pid
    assert len(_starts(home)) == 2


def test_gateway_keeps_push_state_and_logs_in_its_home(gateway, tmp_path):
    # The push state in VIS_HOME holds the user's paired phones and browsers. A
    # gateway that read it would send the notifications of SDK turns there.
    command, home = gateway
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    launcher = bin_dir / "vis-agent"
    launcher.write_text(f'#!/bin/sh\nexec {shlex.join(command)} "$@"\n')
    (bin_dir / "vis-agent.pinned").write_text("")
    (bin_dir / "vis-agent-native").write_text("#!/bin/sh\n")
    for path in (launcher, bin_dir / "vis-agent-native"):
        path.chmod(0o755)
    # An argument list runs a JVM; a pinned launcher runs the native engine.
    for executable in (command, [str(launcher)]):
        with start(shared=True, executable=executable, home=home):
            folder = home.resolve()
            properties = json.loads((home / "properties").read_text())
            assert f"-Dvis.push.home={folder / 'push'}" in properties
            assert f"-Dvis.logs.dir={folder / 'logs'}" in properties
        stop_shared(home=home)


def test_concurrent_ensure_starts_one_gateway(gateway):
    command, home = gateway
    code = (
        "import sys; from blockether.vis.engine import start; "
        f"print(start(shared=True, executable={command!r}, home={str(home)!r})._url)"
    )
    env = {**os.environ, "PYTHONPATH": _SRC}
    callers = [
        subprocess.Popen(
            [sys.executable, "-c", code], env=env, stdout=subprocess.PIPE, text=True
        )
        for _ in range(4)
    ]
    urls = {caller.communicate(timeout=60)[0].strip() for caller in callers}
    assert all(caller.returncode == 0 for caller in callers)
    assert len(urls) == 1
    assert len(_starts(home)) == 1


def test_a_stale_record_is_replaced_without_ending_its_process(gateway):
    command, home = gateway
    home.mkdir(mode=0o700)
    # A recorded PID that runs another program, and a port that nobody serves.
    stranger = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    try:
        (home / "token").write_text("old-token")
        (home / "gateway.json").write_text(
            json.dumps(
                {
                    "url": "http://127.0.0.1:9",
                    "pid": stranger.pid,
                    "group": stranger.pid,
                    "command": command,
                    "db": str(home / "sessions.sqlite"),
                    "started_at": 0,
                }
            )
        )
        client = start(shared=True, executable=command, home=home)
        with client:
            pass
        record = json.loads((home / "gateway.json").read_text())
        assert record["pid"] != stranger.pid
        assert client._url == record["url"] != "http://127.0.0.1:9"
        assert stranger.poll() is None
    finally:
        stranger.kill()
        stranger.wait()


def test_stop_shared_ends_the_gateway_for_its_clients(gateway):
    command, home = gateway
    client = start(shared=True, executable=command, home=home)
    pid = json.loads((home / "gateway.json").read_text())["pid"]
    # An explicit stop ends the gateway while clients still hold leases.
    assert stop_shared(home=home) is True
    _wait_exit(pid)
    assert not _alive(pid)
    with contextlib.suppress(TransportError):
        client.close()
    assert not (home / "gateway.json").exists()
    assert stop_shared(home=home) is False


def test_real_local_service(tmp_path, monkeypatch):
    command = os.environ.get("VIS_TEST_LOCAL_COMMAND")
    if not command:
        pytest.skip("set VIS_TEST_LOCAL_COMMAND to exercise the real Vis engine")
    user = tmp_path / "user"
    user.mkdir()
    monkeypatch.setenv("HOME", str(user))
    monkeypatch.setenv("VIS_HOME", str(user / ".vis"))
    monkeypatch.setenv("JAVA_TOOL_OPTIONS", f"-Duser.home={user}")
    monkeypatch.delenv("VIS_GATEWAY_URL", raising=False)
    home = tmp_path / "service"
    project = tmp_path / "project"
    project.mkdir()
    argv = shlex.split(command)
    try:
        with start(
            shared=True, executable=argv, home=home, startup_timeout=180
        ) as layer:
            session = layer.create_session(**layer.session_options(project))
            assert session.read()["id"] == session.id
            assert session.turns() == []
            pid = json.loads((home / "gateway.json").read_text())["pid"]
            started = time.monotonic()
            with start(shared=True, executable=argv, home=home):
                assert time.monotonic() - started < 5
        # The managed gateway stops by itself after the last lease ends.
        _wait_exit(pid, 30)
        assert not _alive(pid)
        with start(
            shared=True, executable=argv, home=home, startup_timeout=180
        ) as layer:
            rows = layer.list_sessions(include_empty=True)["sessions"]
            assert any(row["id"] == session.id for row in rows)
        # Push state and logs stay out of the user's VIS_HOME.
        assert (home / "push" / "web-push").is_dir()
        assert not (user / ".vis" / "web-push").exists()
        assert not [log for log in (user / ".vis").glob("logs/**/*") if log.is_file()]
        assert any((home / "logs").glob("*/gateway-*.log"))
    finally:
        stop_shared(home=home)
