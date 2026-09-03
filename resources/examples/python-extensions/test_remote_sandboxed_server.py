"""Tests for remote_sandboxed_server — no network, no ssh binary.

The transport seam (_spawn) is faked; everything asserted here is about
the typed result objects and the argv we WOULD have executed.
"""

import importlib.util
import os
from dataclasses import FrozenInstanceError

import pytest

_MODULE_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "remote_sandboxed_server.py"
)
_spec = importlib.util.spec_from_file_location("remote_sandboxed_server", _MODULE_PATH)
rss = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(rss)


def configure(monkeypatch, host="root@10.0.0.5", port="2222", password=None):
    """Point the module at a fake endpoint and a blank state store."""
    for name in ("REMOTE_SERVER_HOST", "REMOTE_SERVER_PORT", "REMOTE_SERVER_PASSWORD"):
        monkeypatch.delenv(name, raising=False)
    if host is not None:
        monkeypatch.setenv("REMOTE_SERVER_HOST", host)
    if port is not None:
        monkeypatch.setenv("REMOTE_SERVER_PORT", port)
    if password is not None:
        monkeypatch.setenv("REMOTE_SERVER_PASSWORD", password)
    monkeypatch.setattr(rss, "_state", lambda key: None)


def fake_spawn(codes):
    """A _spawn fake returning queued outcomes and recording its calls."""
    calls = []
    queue = list(codes)

    def _spawn(argv, input_bytes, timeout_s, extra_env=None):
        calls.append(
            {
                "argv": argv,
                "input": input_bytes,
                "timeout": timeout_s,
                "env": extra_env or {},
            }
        )
        return queue.pop(0)

    return _spawn, calls


# -- configuration -----------------------------------------------------------


def test_endpoint_requires_a_host(monkeypatch):
    configure(monkeypatch, host=None)
    with pytest.raises(ValueError, match="/rss-host"):
        rss._endpoint()


def test_endpoint_parses_user_host_and_port(monkeypatch):
    configure(monkeypatch)
    endpoint = rss._endpoint()
    assert endpoint.destination == "root@10.0.0.5"
    assert endpoint.port == 2222
    assert endpoint.password is None


def test_endpoint_defaults_port_to_22(monkeypatch):
    configure(monkeypatch, port=None)
    assert rss._endpoint().port == 22


def test_endpoint_repr_never_carries_the_password(monkeypatch):
    configure(monkeypatch, password="s3cret-value")
    assert "s3cret-value" not in repr(rss._endpoint())


# -- argv construction -------------------------------------------------------


def test_argv_without_password_is_batchmode(monkeypatch):
    configure(monkeypatch)
    argv, extra = rss._ssh_argv(rss._endpoint(), "echo hi")
    assert argv[0] == "ssh"
    assert argv[argv.index("-p") + 1] == "2222"
    assert "BatchMode=yes" in argv
    assert argv[-2:] == ["root@10.0.0.5", "echo hi"]
    assert extra == {}


def test_argv_with_password_uses_askpass_not_argv(monkeypatch, tmp_path):
    monkeypatch.setattr(rss.tempfile, "gettempdir", lambda: str(tmp_path))
    configure(monkeypatch, password="s3cret-value")
    argv, extra = rss._ssh_argv(rss._endpoint(), "echo hi")
    joined = " ".join(argv)
    assert "s3cret-value" not in joined
    assert "BatchMode=yes" not in joined
    assert extra["SSH_ASKPASS_REQUIRE"] == "force"
    helper = extra["SSH_ASKPASS"]
    assert os.path.basename(helper).startswith("rss-askpass-")
    mode = os.stat(helper).st_mode & 0o777
    assert mode == 0o700
    body = open(helper).read()
    assert "s3cret-value" not in body
    assert rss._SECRET_ENV in body


# -- run() -------------------------------------------------------------------


def test_run_builds_a_command_result(monkeypatch):
    configure(monkeypatch)
    spawn, calls = fake_spawn([(0, b"out\n", b"warn", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    result = rss.remote_sandboxed_server.run("printf hi", timeout_s=7)
    assert isinstance(result, rss.CommandResult)
    assert result.command == "printf hi"
    assert result.exit_code == 0
    assert result.stdout == "out\n"
    assert result.stderr == "warn"
    assert result.is_timed_out is False
    assert result.is_truncated is False
    assert result.duration_ms >= 0
    assert calls[0]["argv"][-1] == "printf hi"
    assert calls[0]["timeout"] == 7


def test_run_timeout_reports_none_exit(monkeypatch):
    configure(monkeypatch)
    spawn, _ = fake_spawn([(None, b"", b"killed", True)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    result = rss.remote_sandboxed_server.run("sleep 999")
    assert result.exit_code is None
    assert result.is_timed_out is True


def test_run_truncates_oversized_capture(monkeypatch):
    configure(monkeypatch)
    spawn, _ = fake_spawn([(0, b"a" * (rss._MAX_CAPTURE_BYTES + 5), b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    result = rss.remote_sandboxed_server.run("yes")
    assert len(result.stdout) == rss._MAX_CAPTURE_BYTES
    assert result.is_truncated is True


def test_results_are_frozen():
    result = rss.CommandResult(
        command="x",
        exit_code=0,
        stdout="",
        stderr="",
        duration_ms=1,
        is_timed_out=False,
        is_truncated=False,
    )
    with pytest.raises(FrozenInstanceError):
        result.exit_code = 3


# -- service() ---------------------------------------------------------------


def test_service_parses_systemctl_show(monkeypatch):
    configure(monkeypatch)
    show = (
        b"LoadState=loaded\nActiveState=active\nSubState=running\n"
        b"UnitFileState=enabled\nMainPID=4242\n"
    )
    spawn, calls = fake_spawn([(0, show, b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    status = rss.remote_sandboxed_server.service("visgw")
    assert isinstance(status, rss.ServiceStatus)
    assert status.load_state == "loaded"
    assert status.active_state == "active"
    assert status.sub_state == "running"
    assert status.is_enabled is True
    assert status.main_pid == 4242
    assert "systemctl show visgw" in calls[0]["argv"][-1]


def test_service_main_pid_zero_becomes_none(monkeypatch):
    configure(monkeypatch)
    spawn, _ = fake_spawn([(0, b"MainPID=0\nActiveState=inactive\n", b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    status = rss.remote_sandboxed_server.service("visgw")
    assert status.main_pid is None
    assert status.is_enabled is False


# -- health() ----------------------------------------------------------------


def test_health_parses_body_and_code(monkeypatch):
    configure(monkeypatch)
    spawn, _ = fake_spawn([(0, b'{"status":"ok"}\n200', b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    check = rss.remote_sandboxed_server.health(port=7890, path="/healthz")
    assert isinstance(check, rss.HealthCheck)
    assert check.url == "http://127.0.0.1:7890/healthz"
    assert check.status_code == 200
    assert check.is_ok is True
    assert check.error is None
    assert "ok" in check.body_excerpt


def test_health_reports_connection_failures(monkeypatch):
    configure(monkeypatch)
    spawn, _ = fake_spawn([(7, b"\n000", b"curl: (7) Failed to connect", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    check = rss.remote_sandboxed_server.health(port=1)
    assert check.status_code is None
    assert check.is_ok is False
    assert "Failed to connect" in (check.error or "")


# -- info() ------------------------------------------------------------------


def test_info_parses_linux_facts(monkeypatch):
    configure(monkeypatch)
    facts = (
        b"hostname=gw\nkernel=Linux 6.8.0\nuptime_s=123456\n"
        b"mem_total_kb=16384000\nmem_available_kb=14447000\n"
    )
    spawn, _ = fake_spawn([(0, facts, b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    info = rss.remote_sandboxed_server.info()
    assert isinstance(info, rss.HostInfo)
    assert info.host == "root@10.0.0.5"
    assert info.hostname == "gw"
    assert info.kernel == "Linux 6.8.0"
    assert info.uptime_seconds == 123456
    assert info.mem_total_mb == 16000
    assert info.mem_available_mb == 14108


def test_info_truncates_decimal_uptime(monkeypatch):
    configure(monkeypatch)
    facts = b"hostname=gw\nkernel=Linux\nuptime_s=983256.91\nmem_total_kb=\nmem_available_kb=\n"
    spawn, _ = fake_spawn([(0, facts, b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    assert rss.remote_sandboxed_server.info().uptime_seconds == 983256


def test_info_answers_none_where_proc_is_missing(monkeypatch):
    configure(monkeypatch)
    facts = b"hostname=gw\nkernel=Linux\nuptime_s=\nmem_total_kb=\nmem_available_kb=\n"
    spawn, _ = fake_spawn([(0, facts, b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    info = rss.remote_sandboxed_server.info()
    assert info.uptime_seconds is None
    assert info.mem_total_mb is None
    assert info.mem_available_mb is None


# -- put() / get() -----------------------------------------------------------


def test_put_streams_local_bytes_into_remote_cat(monkeypatch, tmp_path):
    configure(monkeypatch)
    local = tmp_path / "payload.bin"
    local.write_bytes(b"\x00\x01hello")
    spawn, calls = fake_spawn([(0, b"", b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    result = rss.remote_sandboxed_server.put(str(local), "/srv/app/payload.bin")
    assert isinstance(result, rss.TransferResult)
    assert result.size_bytes == 7
    assert calls[0]["input"] == b"\x00\x01hello"
    assert "mkdir -p /srv/app" in calls[0]["argv"][-1]
    assert "cat > /srv/app/payload.bin" in calls[0]["argv"][-1]


def test_put_failure_raises_with_remote_stderr(monkeypatch, tmp_path):
    configure(monkeypatch)
    local = tmp_path / "x"
    local.write_bytes(b"x")
    spawn, _ = fake_spawn([(1, b"", b"disk full", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    with pytest.raises(RuntimeError, match="disk full"):
        rss.remote_sandboxed_server.put(str(local), "/srv/x")


def test_get_writes_remote_bytes_to_local_parents(monkeypatch, tmp_path):
    configure(monkeypatch)
    spawn, calls = fake_spawn([(0, b"\x00\x01binary", b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    target = tmp_path / "deep/dir/out.bin"
    result = rss.remote_sandboxed_server.get("/etc/app.conf", str(target))
    assert result.size_bytes == 8
    assert target.read_bytes() == b"\x00\x01binary"
    assert calls[0]["argv"][-1] == "cat /etc/app.conf"


def test_get_missing_file_raises(monkeypatch, tmp_path):
    configure(monkeypatch)
    spawn, _ = fake_spawn([(1, b"", b"cat: /nope: No such file", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    with pytest.raises(RuntimeError, match="No such file"):
        rss.remote_sandboxed_server.get("/nope", str(tmp_path / "out"))


# -- the password never travels in a result ----------------------------------


def test_run_never_echoes_the_password(monkeypatch):
    configure(monkeypatch, password="s3cret-value")
    spawn, _ = fake_spawn([(0, b"ok", b"", False)])
    monkeypatch.setattr(rss, "_spawn", spawn)
    result = rss.remote_sandboxed_server.run("true")
    assert "s3cret-value" not in repr(result)
