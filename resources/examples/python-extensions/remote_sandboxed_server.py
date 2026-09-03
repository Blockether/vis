"""Remote sandboxed server — one SSH door to a machine you administer.

The flagship object-first extension: every operation returns a typed,
frozen domain object (CommandResult, ServiceStatus, HealthCheck,
HostInfo, TransferResult). Nothing hands the model a formatted string, a
JSON blob or a shapeless dict to parse back into structure.

Configuration — three environment variables, declared through env=, so
every `environment:` source works ({literal: ...}, {env: ...}, a keychain
item, or a helper command whose trimmed stdout is the value):

    REMOTE_SERVER_HOST      user@host; without a user part ssh picks its
                            own default, which is rarely what you want
    REMOTE_SERVER_PORT      SSH port, defaults to 22
    REMOTE_SERVER_PASSWORD  optional; absent means key/agent auth only

Environment wins over vis.state; `/rss-host user@host [port]` persists
the non-secret part to vis.state for ad-hoc use without a config file.

Transport is the local ssh binary. A password is handed over through an
SSH_ASKPASS helper (OpenSSH >= 8.4, SSH_ASKPASS_REQUIRE=force), so it
never appears in an argv, a result object or a log line; the helper file
is 0700 in the temp dir and prints a private environment variable back
to ssh. Without a password the connection is BatchMode: key or agent
only, nothing to hang on.

Trust: this file runs in a trusted extension context and executes
ARBITRARY commands as the configured remote user. Configure it only for
a machine where that is what you want, and read the model's run() and
put() calls as that user doing work on the server.
"""

import contextlib
import hashlib
import os
import shlex
import signal
import subprocess
import tempfile
import time
from dataclasses import dataclass, field

import vis

_CONNECT_TIMEOUT_S = 15
_MAX_CAPTURE_BYTES = 256 * 1024
_MAX_TRANSFER_BYTES = 64 * 1024 * 1024
_SECRET_ENV = "RSS_REMOTE_SERVER_SECRET"
_ASKPASS_BODY = '#!/bin/sh\nexec printf %s "$' + _SECRET_ENV + '"\n'


@dataclass(frozen=True)
class CommandResult:
    """Outcome of one command executed on the remote server.

    `exit_code` is None exactly when the command was killed locally after
    `timeout_s` — the remote side may still be running it. `stdout` and
    `stderr` are decoded UTF-8 (replacement characters on garbage) and
    capped at 256 KiB each; `is_truncated` says the cap cut something off.
    `duration_ms` measures the local wait for ssh, not the command's own
    runtime.
    """

    command: str
    exit_code: int | None
    stdout: str
    stderr: str
    duration_ms: int
    is_timed_out: bool
    is_truncated: bool


@dataclass(frozen=True)
class ServiceStatus:
    """State of one systemd unit on the remote server.

    `load_state`, `active_state` and `sub_state` are systemd's own
    spellings (loaded/active/running, ...). `is_enabled` reads
    UnitFileState == "enabled", so a "static" unit answers False even
    though systemd may still start it. `main_pid` is the unit's MainPID,
    None when systemd reports 0 (no single main process).
    """

    unit: str
    load_state: str
    active_state: str
    sub_state: str
    is_enabled: bool
    main_pid: int | None


@dataclass(frozen=True)
class HealthCheck:
    """Result of one HTTP request made FROM the remote server.

    Asking the remote machine is the point: it sees loopback services no
    outside check can reach. `status_code` is None when curl never got an
    answer (refused, timed out, DNS) and `error` then carries curl's
    stderr. `body_excerpt` is the first 512 bytes of the body, decoded
    with replacement. `is_ok` is True exactly for 2xx.
    """

    url: str
    status_code: int | None
    is_ok: bool
    body_excerpt: str
    error: str | None


@dataclass(frozen=True)
class HostInfo:
    """Identity, uptime and memory facts about the remote machine.

    Linux-shaped: uptime and memory come from /proc, so a non-Linux guest
    answers None for them instead of failing the whole call.
    `uptime_seconds` is whole seconds since boot; the memory fields are
    whole megabytes (1 MiB units), total and currently available.
    """

    host: str
    hostname: str | None
    kernel: str | None
    uptime_seconds: int | None
    mem_total_mb: int | None
    mem_available_mb: int | None


@dataclass(frozen=True)
class TransferResult:
    """One completed file copy over the same ssh door.

    `size_bytes` is the payload that crossed. `local_path` and
    `remote_path` are the paths as given (tilde-expanded on the local
    side only). A copy that cannot complete raises with the remote
    stderr, so a TransferResult always names a finished transfer.
    """

    local_path: str
    remote_path: str
    size_bytes: int
    duration_ms: int


@dataclass(frozen=True)
class _Endpoint:
    """Where to connect and how, for one round trip.

    `destination` is user@host or a bare host. `password` is excluded
    from the repr so a debug print of an endpoint cannot leak it.
    """

    destination: str
    port: int
    password: str | None = field(repr=False, default=None)


def _env(name: str) -> str | None:
    return os.environ.get(name) or None


def _state(key: str) -> object:
    with contextlib.suppress(Exception):
        return vis.state.get(key)
    return None


def _endpoint() -> _Endpoint:
    """Resolve the connection target; environment wins over vis.state."""
    host = _env("REMOTE_SERVER_HOST") or _state("host")
    if not host:
        raise ValueError(
            "no remote server configured — set REMOTE_SERVER_HOST "
            "(user@host) or run /rss-host user@host [port]"
        )
    user, _, hostname = str(host).partition("@")
    destination = f"{user}@{hostname}" if hostname else user
    port_raw = _env("REMOTE_SERVER_PORT") or _state("port") or 22
    port = int(str(port_raw).strip())
    password = _env("REMOTE_SERVER_PASSWORD") or _state("password") or None
    return _Endpoint(destination=destination, port=port, password=password)


def _askpass_path(password: str) -> str:
    """Materialize the 0700 askpass helper for this password, once."""
    digest = hashlib.sha256(password.encode()).hexdigest()[:16]
    path = os.path.join(tempfile.gettempdir(), f"rss-askpass-{digest}")
    if not os.path.exists(path):
        handle = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o700)
        with os.fdopen(handle, "w") as helper:
            helper.write(_ASKPASS_BODY)
    else:
        # An earlier revision wrote 0600; ssh must EXEC this helper.
        os.chmod(path, 0o700)
    return path


def _ssh_argv(endpoint: _Endpoint, command: str) -> tuple[list[str], dict[str, str]]:
    """Build the ssh argv plus any extra environment it needs.

    The password, when present, crosses only through the askpass helper
    and its private environment variable — never through argv.
    """
    argv = [
        "ssh",
        "-p",
        str(endpoint.port),
        "-o",
        f"ConnectTimeout={_CONNECT_TIMEOUT_S}",
        "-o",
        "StrictHostKeyChecking=accept-new",
        "-o",
        "NumberOfPasswordPrompts=1",
    ]
    extra: dict[str, str] = {}
    if endpoint.password:
        extra = {
            "SSH_ASKPASS": _askpass_path(endpoint.password),
            "SSH_ASKPASS_REQUIRE": "force",
            "DISPLAY": ":0",
            _SECRET_ENV: endpoint.password,
        }
    else:
        argv += ["-o", "BatchMode=yes"]
    argv += ["--", endpoint.destination, command]
    return argv, extra


def _spawn(
    argv: list[str],
    input_bytes: bytes | None,
    timeout_s: int,
    extra_env: dict[str, str] | None = None,
) -> tuple[int | None, bytes, bytes, bool]:
    """Run argv as a direct child; kill the whole group on timeout.

    Answers (exit_code, stdout, stderr, is_timed_out); exit_code is None
    only when the child was killed after `timeout_s`.
    """
    env = dict(os.environ)
    env.update(extra_env or {})
    proc = subprocess.Popen(
        argv,
        stdin=subprocess.PIPE if input_bytes is not None else subprocess.DEVNULL,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
        start_new_session=True,
    )
    try:
        out, err = proc.communicate(input_bytes, timeout=timeout_s)
        return proc.returncode, out or b"", err or b"", False
    except subprocess.TimeoutExpired:
        with contextlib.suppress(OSError):
            os.killpg(proc.pid, signal.SIGKILL)
        out, err = proc.communicate()
        return None, out or b"", err or b"", True


def _decode(chunk: bytes) -> tuple[str, bool]:
    text = chunk.decode("utf-8", errors="replace")
    if len(chunk) > _MAX_CAPTURE_BYTES:
        return text[:_MAX_CAPTURE_BYTES], True
    return text, False


def _execute(
    command: str, timeout_s: int = 60, input_bytes: bytes | None = None
) -> CommandResult:
    """One round trip to the configured server."""
    endpoint = _endpoint()
    argv, extra = _ssh_argv(endpoint, command)
    started = time.monotonic()
    code, out, err, timed_out = _spawn(argv, input_bytes, timeout_s, extra)
    stdout, out_cut = _decode(out)
    stderr, err_cut = _decode(err)
    return CommandResult(
        command=command,
        exit_code=code,
        stdout=stdout,
        stderr=stderr,
        duration_ms=int((time.monotonic() - started) * 1000),
        is_timed_out=timed_out,
        is_truncated=out_cut or err_cut,
    )


def _require_ok(result: CommandResult, what: str) -> None:
    if result.exit_code != 0:
        detail = result.stderr.strip() or result.stdout.strip()
        detail = detail or f"exit code {result.exit_code}"
        raise RuntimeError(f"{what} failed on the remote server: {detail[:400]}")


def _to_int(value: object) -> int | None:
    """Parse a whole number, truncating any decimal part.

    /proc/uptime reports seconds as '983256.91', so a strict int() parse
    would turn a healthy guest into None. Absent or non-numeric values
    still answer None.
    """
    try:
        return int(float(str(value).strip()))
    except (TypeError, ValueError):
        return None


class RemoteSandboxedServer:
    """The administered server: run, inspect and move files over one ssh door.

    Every method is one round trip through the same transport, so one
    configuration and one credential path serve everything. The
    observation methods never change the machine; run() and put() do
    exactly what their command says, as the configured remote user.
    """

    @vis.method(tag="mutation")
    def run(self, command: str, timeout_s: int = 60) -> CommandResult:
        """Execute one shell command on the remote server.

        The command runs through the remote login shell exactly as ssh
        passes it. `timeout_s` bounds the LOCAL wait: on timeout the ssh
        process is killed but the remote command may keep running, so
        poll with run() again instead of assuming it stopped.
        """
        return _execute(command, timeout_s=timeout_s)

    def service(self, unit: str, timeout_s: int = 30) -> ServiceStatus:
        """Read one systemd unit's state; no systemctl action is taken.

        Requires systemd on the remote machine; the unit name goes to
        `systemctl show` as one argument, so suffixes follow systemd's
        own defaulting (visgw -> visgw.service).
        """
        command = (
            f"systemctl show {shlex.quote(unit)}"
            " --property=LoadState --property=ActiveState"
            " --property=SubState --property=UnitFileState --property=MainPID"
        )
        result = _execute(command, timeout_s=timeout_s)
        _require_ok(result, f"service {unit}")
        fields = dict(
            line.partition("=")[::2]
            for line in result.stdout.splitlines()
            if "=" in line
        )
        fields = {key.strip(): value.strip() for key, value in fields.items()}
        return ServiceStatus(
            unit=unit,
            load_state=fields.get("LoadState", ""),
            active_state=fields.get("ActiveState", ""),
            sub_state=fields.get("SubState", ""),
            is_enabled=fields.get("UnitFileState") == "enabled",
            main_pid=_to_int(fields.get("MainPID")) or None,
        )

    def health(
        self,
        port: int = 80,
        path: str = "/healthz",
        scheme: str = "http",
        timeout_s: int = 10,
    ) -> HealthCheck:
        """One HTTP request to the remote machine's own loopback.

        curl runs ON the server, so this reaches services bound to
        127.0.0.1 that no outside check can see. `scheme` is "http" or
        "https"; `path` should start with "/". Endpoints that need auth
        headers belong in a run("curl ...") call, not here.
        """
        url = f"{scheme}://127.0.0.1:{int(port)}{path}"
        command = (
            f"curl -sS -m {int(timeout_s)} -w '\n%{{http_code}}' -- {shlex.quote(url)}"
        )
        result = _execute(command, timeout_s=timeout_s + 5)
        body, _, code_raw = result.stdout.rpartition("\n")
        status_code = _to_int(code_raw) or None
        if result.exit_code != 0:
            error = result.stderr.strip() or f"curl exit code {result.exit_code}"
            return HealthCheck(
                url=url,
                status_code=status_code,
                is_ok=False,
                body_excerpt=body[:512],
                error=error[:400],
            )
        return HealthCheck(
            url=url,
            status_code=status_code,
            is_ok=status_code is not None and 200 <= status_code < 300,
            body_excerpt=body[:512],
            error=None,
        )

    def info(self, timeout_s: int = 30) -> HostInfo:
        """Collect identity, uptime and memory facts in one round trip.

        Linux guests answer everything; the /proc-derived fields are None
        elsewhere (a BSD guest, a router) rather than failing the call.
        """
        command = (
            "printf 'hostname=%s\nkernel=%s\nuptime_s=%s\n"
            "mem_total_kb=%s\nmem_available_kb=%s\n'"
            ' "$(hostname)" "$(uname -sr)"'
            " \"$(cut -d' ' -f1 /proc/uptime)\""
            " \"$(awk '/^MemTotal/{print $2}' /proc/meminfo)\""
            " \"$(awk '/^MemAvailable/{print $2}' /proc/meminfo)\""
        )
        endpoint = _endpoint()
        result = _execute(command, timeout_s=timeout_s)
        _require_ok(result, "info")
        values = dict(
            line.partition("=")[::2]
            for line in result.stdout.splitlines()
            if "=" in line
        )
        values = {key.strip(): value.strip() for key, value in values.items()}

        def megabytes(key: str) -> int | None:
            total_kb = _to_int(values.get(key))
            return None if total_kb is None else total_kb // 1024

        return HostInfo(
            host=endpoint.destination,
            hostname=values.get("hostname") or None,
            kernel=values.get("kernel") or None,
            uptime_seconds=_to_int(values.get("uptime_s")),
            mem_total_mb=megabytes("mem_total_kb"),
            mem_available_mb=megabytes("mem_available_kb"),
        )

    @vis.method(tag="mutation")
    def put(
        self, local_path: str, remote_path: str, timeout_s: int = 120
    ) -> TransferResult:
        """Copy one local file to a remote path; remote parents created.

        The payload crosses as stdin to a remote `cat`, so it is
        binary-safe and never appears in a command line. A failed copy
        (missing local file, remote disk full) raises with the remote
        stderr; a completed TransferResult names a copy that exited 0.
        """
        source = os.path.expanduser(local_path)
        with open(source, "rb") as handle:
            payload = handle.read()
        if len(payload) > _MAX_TRANSFER_BYTES:
            raise ValueError(
                f"{local_path} is {len(payload)} bytes; the copy cap is "
                f"{_MAX_TRANSFER_BYTES} — move it with run() and a stream"
            )
        parent = os.path.dirname(remote_path)
        redirect = f"cat > {shlex.quote(remote_path)}"
        command = (
            f"mkdir -p {shlex.quote(parent)} && {redirect}" if parent else redirect
        )
        result = _execute(command, timeout_s=timeout_s, input_bytes=payload)
        _require_ok(result, f"put {remote_path}")
        return TransferResult(
            local_path=source,
            remote_path=remote_path,
            size_bytes=len(payload),
            duration_ms=result.duration_ms,
        )

    def get(
        self, remote_path: str, local_path: str, timeout_s: int = 120
    ) -> TransferResult:
        """Copy one remote file to a local path; local parents created.

        Binary-safe through the same pipe. A missing or unreadable remote
        file raises with the remote stderr. The bytes land only in the
        local file, never inside a result object.
        """
        endpoint = _endpoint()
        command = f"cat {shlex.quote(remote_path)}"
        argv, extra = _ssh_argv(endpoint, command)
        started = time.monotonic()
        code, out, err, _ = _spawn(argv, None, timeout_s, extra)
        if len(out) > _MAX_TRANSFER_BYTES:
            raise ValueError(
                f"{remote_path} is larger than the {_MAX_TRANSFER_BYTES} byte "
                "copy cap — pull it with run() and a targeted command"
            )
        if code != 0:
            detail = (
                err.decode("utf-8", errors="replace").strip() or f"exit code {code}"
            )
            raise RuntimeError(
                f"get {remote_path} failed on the remote server: {detail[:400]}"
            )
        target = os.path.expanduser(local_path)
        os.makedirs(os.path.dirname(os.path.abspath(target)), exist_ok=True)
        with open(target, "wb") as handle:
            handle.write(out)
        return TransferResult(
            local_path=local_path,
            remote_path=remote_path,
            size_bytes=len(out),
            duration_ms=int((time.monotonic() - started) * 1000),
        )


remote_sandboxed_server = RemoteSandboxedServer()

PROMPT = """rss_ surface active — one administered server over SSH (remote_sandboxed_server):
  run(command, timeout_s=60)        execute a remote shell command
  service(unit)                     systemd unit state
  health(port, path="/healthz")     loopback HTTP from the server itself
  info()                            host, uptime, memory
  put(local, remote) / get(remote, local)   file copies
Results are typed frozen objects (CommandResult, ServiceStatus, ...)."""


def _slash_host(ctx: dict) -> dict:
    args = ctx.get("args") or []
    if not args:
        host = vis.state.get("host")
        return vis.ok(
            f"remote server: {host}"
            if host
            else "no host set — /rss-host user@host [port]"
        )
    target = args[0]
    if "@" not in target:
        return vis.err("host must be user@host, e.g. root@10.0.0.5")
    port = args[1] if len(args) > 1 else vis.state.get("port") or 22
    if not str(port).isdigit():
        return vis.err(f"port must be numeric, got {port!r}")
    vis.state["host"] = target
    vis.state["port"] = int(port)
    return vis.ok(f"remote server set: {target} port {port}")


vis.extension(
    name="remote-sandboxed-server",
    description=(
        "One administered server over SSH: run, service, health, info, put, get."
    ),
    version="0.1.0",
    kind="integration",
    alias="rss",
    symbols=[
        vis.symbol(
            remote_sandboxed_server, name="remote_sandboxed_server", tag="observation"
        )
    ],
    prompt=PROMPT,
    slash_commands=[
        vis.slash(
            "rss-host",
            _slash_host,
            doc="Set or show the administered server.",
            usage="/rss-host user@host [port]",
        )
    ],
    env=["REMOTE_SERVER_HOST", "REMOTE_SERVER_PORT", "REMOTE_SERVER_PASSWORD"],
)
