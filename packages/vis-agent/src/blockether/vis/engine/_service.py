"""One warm local Vis gateway that SDK processes share (#489).

`LocalService.ensure()` finds or starts one `vis-agent gateway` in a private state
folder and returns a connected `GatewayClient` for it. While a client holds a lease,
the next `Agent` attaches in milliseconds instead of starting an engine. The
gateway stops by itself when its last client leaves. Supports Linux and macOS.
"""

from __future__ import annotations

import contextlib
import json
import os
import secrets
import shutil
import signal
import socket
import subprocess
import threading
import time
import urllib.request
from collections.abc import Iterator, Sequence
from pathlib import Path
from typing import TypedDict

from ._client import (
    GatewayClient,
    GatewayError,
    SessionOptions,
    TransportError,
    VisTimeout,
    _duration,
)
from ._install import (
    _COMMAND,
    _NATIVE,
    _PINNED,
    InstallError,
    _vis_home,
    ensure_installed,
)

_HOST = "127.0.0.1"
# Errors quote at most this much of the gateway log, starting at a line.
_LOG_TAIL = 4096
# A started gateway that nobody waits for would stay a zombie of this process.
_STARTED: list[subprocess.Popen[bytes]] = []


class _Record(TypedDict):
    url: str
    pid: int
    group: int
    command: list[str]
    db: str
    started_at: float


class _ServiceClient(GatewayClient):
    """A client of the shared local gateway: local paths, startup-length session wait."""

    def __init__(self, url: str, *, token: str, timeout: float, startup_timeout: float):
        super().__init__(url, token=token, timeout=timeout)
        self._startup_timeout = startup_timeout

    def session_options(self, project=".") -> SessionOptions:
        """Resolve an existing local directory: the gateway runs on this computer."""
        path = Path(project).resolve(strict=True)
        if not path.is_dir():
            raise NotADirectoryError(str(path))
        return {"root": str(path), "channel": "app"}

    def _session_create_timeout(self, timeout: float | None) -> float | None:
        """Wait for a session up to the startup deadline of the service."""
        return max(self.timeout, self._startup_timeout) if timeout is None else timeout


class LocalService:
    """Share one warm local Vis gateway between SDK processes.

    `Agent()` without an execution layer starts a private engine for each agent.
    The first start of a new engine copy can take 40 s or more, and each later
    start takes about 1.5 s. `ensure` starts one gateway once and keeps it running,
    so each later call attaches in milliseconds:

    ```python
    from blockether.vis.engine import Agent, LocalService

    with LocalService.ensure() as layer:
        with Agent(project=".", execution_layer=layer) as agent:
            print(agent.run("Summarize this project without changing files.")["status"])
    ```

    The gateway keeps its database, token, runtime folder, logs and push state in
    `home` (default `$VIS_HOME/sdk/service`). It uses the provider settings of
    `VIS_HOME`, but never the database, the gateway or the paired devices of your
    own Vis. So its turns do not send notifications to your phone or browser. It listens only on
    127.0.0.1, on a free port, and requires its token.

    The gateway stops by itself when no client holds a lease and no turn runs, as
    the gateway of the Vis terminal app does. A client that was not closed keeps
    its lease for up to 2 minutes after its process ends. A gateway that no client
    ever used stops after 30 s. Sessions stay in its database, so the next `ensure`
    starts the gateway again and finds them. Keep one client open while you work to
    keep the gateway warm.

    With the default `vis-agent`, the engine comes from the same place as for
    `blockether.vis.engine.LocalEngine`: the installed command of this SDK's
    release, else a checked download into `$VIS_HOME/sdk`.
    """

    @classmethod
    def ensure(
        cls,
        *,
        executable: str | os.PathLike[str] | Sequence[str] = _COMMAND,
        home: str | os.PathLike[str] | None = None,
        timeout: float = 30,
        startup_timeout: float = 120,
    ) -> GatewayClient:
        """Return a connected client of the shared gateway, and start it when none runs.

        Processes and threads that call this at the same time get one gateway: a
        file lock in `home` lets only one of them start it. The client takes its
        lease while the call holds the lock, so the gateway cannot stop before the
        caller uses it. A record of a gateway that stopped, does not answer or
        stops during the call is replaced. A running gateway of another engine
        (for example after an SDK update) is replaced only when no client uses it
        and no turn runs; else the call reuses it.

        Args:
            executable: Executable path or name, or an argument sequence. The SDK
                appends `gateway start` and its options.
            home: State folder of the gateway. Default `$VIS_HOME/sdk/service`.
            timeout: Positive finite timeout in seconds for each request.
            startup_timeout: Positive finite deadline in seconds for the start of
                the gateway and for each session creation.

        Close the client, or use it as a context manager. Closing releases its lease.
        When it was the last lease, the gateway stops by itself.

        Raises:
            TransportError: The engine cannot be found or downloaded, or the
                gateway exited during its start. The message ends with its log.
            VisTimeout: The gateway did not answer within `startup_timeout`.
        """
        timeout, startup_timeout = _duration(timeout), _duration(startup_timeout)
        folder = _folder(home)
        command = _resolve(executable)
        deadline = time.monotonic() + startup_timeout + timeout
        with _locked(folder / "lock", deadline):
            record = _read(folder)
            token = _token(folder)
            health = _health(record, token) if record else None
            if record and health:
                busy = health.get("clients") or health.get("running_turns")
                if record["command"] == command or busy:
                    client = _connected(record, token, timeout, startup_timeout)
                    if client is not None:
                        return client
                    # It stopped by itself after its last client left.
                    _retire(folder, record)
                else:
                    _stop(folder, record, token)
            elif record:
                _retire(folder, record)
            record, token = _start(folder, command, startup_timeout)
            return _client(record, token, timeout, startup_timeout).connect()

    @classmethod
    def discover(
        cls,
        *,
        home: str | os.PathLike[str] | None = None,
        timeout: float = 30,
        startup_timeout: float = 120,
    ) -> GatewayClient | None:
        """Return a connected client of the running shared gateway, or None. Never start one."""
        timeout, startup_timeout = _duration(timeout), _duration(startup_timeout)
        folder = _folder(home)
        record = _read(folder)
        token = _token(folder)
        if record is None or token is None or _health(record, token) is None:
            return None
        return _connected(record, token, timeout, startup_timeout)

    @classmethod
    def stop(cls, *, home: str | os.PathLike[str] | None = None) -> bool:
        """Stop the shared gateway, and return True when one was running.

        This stops the gateway for every process that uses it. Running turns stop
        too. Stored sessions stay in its database for the next start.
        """
        folder = _folder(home)
        with _locked(folder / "lock", time.monotonic() + 60):
            record = _read(folder)
            if record is None:
                return False
            token = _token(folder)
            if _health(record, token):
                _stop(folder, record, token)
                return True
            _retire(folder, record)
            return False


def _folder(home: str | os.PathLike[str] | None) -> Path:
    """Create the private state folder. The engine refuses a runtime folder others can write."""
    if os.name != "posix":
        raise ValueError("the local service currently supports Linux and macOS")
    folder = Path(home) if home is not None else _vis_home() / "sdk" / "service"
    folder.mkdir(mode=0o700, parents=True, exist_ok=True)
    folder = folder.resolve(strict=True)
    for path in (folder, folder / "runtime"):
        path.mkdir(mode=0o700, exist_ok=True)
        path.chmod(0o700)
    return folder


def _resolve(executable: str | os.PathLike[str] | Sequence[str]) -> list[str]:
    """Return the engine argv, with the same resolution and download as `LocalEngine`."""
    command = (
        [os.fspath(executable)]
        if isinstance(executable, (str, os.PathLike))
        else list(executable)
    )
    if not command or not all(isinstance(arg, str) and arg for arg in command):
        raise ValueError("executable must be a nonempty argv sequence")
    if command == [_COMMAND]:
        try:
            return [ensure_installed()]
        except InstallError as error:
            raise TransportError(f"could not start local service: {error}") from None
    return command


@contextlib.contextmanager
def _locked(path: Path, deadline: float) -> Iterator[None]:
    """Hold the exclusive lock of the state folder. Each open file has its own lock."""
    import fcntl  # POSIX only; the SDK still imports on other systems.

    fd = os.open(path, os.O_RDWR | os.O_CREAT, 0o600)
    try:
        while True:
            try:
                fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() >= deadline:
                    raise VisTimeout(
                        "another process holds the local service lock"
                    ) from None
                time.sleep(0.05)
        yield
    finally:
        os.close(fd)


def _read(folder: Path) -> _Record | None:
    try:
        data = json.loads((folder / "gateway.json").read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    fields = {"url": str, "pid": int, "group": int, "command": list, "db": str}
    if not isinstance(data, dict) or not all(
        type(data.get(key)) is kind for key, kind in fields.items()
    ):
        return None
    return data  # pyright: ignore[reportReturnType]


def _write(folder: Path, record: _Record) -> None:
    """Replace the record in one step, so a reader never sees half of it."""
    target = folder / "gateway.json"
    partial = target.with_suffix(f".{os.getpid()}.tmp")
    fd = os.open(partial, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w", encoding="utf-8") as stream:
        json.dump(record, stream)
    os.replace(partial, target)


def _token(folder: Path) -> str | None:
    try:
        return (folder / "token").read_text(encoding="utf-8").strip() or None
    except OSError:
        return None


def _health(record: _Record, token: str | None) -> dict[str, object] | None:
    """Return the health of the recorded gateway, or None when it is not that gateway."""
    if token is None or not _alive(record["pid"]):
        return None
    health = _probe(record["url"], token)
    if health is None or health.get("pid") != record["pid"]:
        return None
    return health


def _alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _owned(record: _Record) -> bool:
    """True when the recorded process still runs this service's gateway, not a reused PID."""
    if not _alive(record["pid"]):
        return False
    try:
        line = subprocess.run(
            ["ps", "-p", str(record["pid"]), "-o", "command="],
            capture_output=True,
            text=True,
            timeout=5,
            check=False,
        ).stdout
    except (OSError, subprocess.SubprocessError):
        return False
    return record["db"] in line


def _signal_group(record: _Record, sig: int) -> None:
    try:
        os.killpg(record["group"], sig)
    except (ProcessLookupError, PermissionError):
        pass


def _wait_exit(pid: int, seconds: float) -> bool:
    deadline = time.monotonic() + seconds
    while _alive(pid):
        if time.monotonic() >= deadline:
            return False
        time.sleep(0.05)
    return True


def _retire(folder: Path, record: _Record) -> None:
    """Remove a stale record. End its process only when it is still this gateway."""
    if _owned(record):
        for sig in (signal.SIGTERM, signal.SIGKILL):
            _signal_group(record, sig)
            if _wait_exit(record["pid"], 5):
                break
    (folder / "gateway.json").unlink(missing_ok=True)


def _stop(folder: Path, record: _Record, token: str | None) -> None:
    """Ask the gateway to stop, then end its process group if it does not exit."""
    request = urllib.request.Request(
        record["url"] + "/v1/admin/stop",
        data=b"",
        method="POST",
        headers={"Authorization": f"Bearer {token}"},
    )
    try:
        with urllib.request.urlopen(request, timeout=10) as reply:
            reply.read(65536)
    except (OSError, ValueError):
        pass
    if not _wait_exit(record["pid"], 15):
        _retire(folder, record)
    (folder / "gateway.json").unlink(missing_ok=True)


def _runs_native(command: list[str]) -> bool:
    """True when `command` runs the native engine, which takes `-D` arguments.

    A binary is native. A `vis-agent` launcher runs the native engine beside it when
    it is pinned, else the installed one unless its track is `dev`; with no native
    engine it runs source on the JVM. Any other argument list runs a JVM.
    """
    if len(command) != 1:
        return False
    found = shutil.which(command[0])
    if found is None:
        return False
    path = Path(found).resolve()
    try:
        with open(path, "rb") as stream:
            if stream.read(2) != b"#!":
                return True
    except OSError:
        return False
    install = _vis_home() / "install"
    beside = path.parent / _NATIVE
    if (path.parent / _PINNED).is_file() and os.access(beside, os.X_OK):
        return True
    try:
        if (install / "track").read_text(encoding="utf-8").strip() == "dev":
            return False
    except OSError:
        pass
    return any(os.access(native, os.X_OK) for native in (beside, install / _NATIVE))


def _java_option(option: str) -> str:
    """Quote an option with a space for JAVA_TOOL_OPTIONS."""
    return f'"{option}"' if any(char.isspace() for char in option) else option


def _free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
        probe.bind((_HOST, 0))
        return probe.getsockname()[1]


def _start(
    folder: Path, command: list[str], startup_timeout: float
) -> tuple[_Record, str]:
    """Start the gateway, wait for its health and write its record."""
    token = secrets.token_urlsafe(32)
    fd = os.open(folder / "token", os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w", encoding="utf-8") as stream:
        stream.write(token)
    db = str(folder / "sessions.sqlite")
    url = f"http://{_HOST}:{_free_port()}"
    env = {**os.environ, "VIS_DB_PATH": db, "VIS_RUNTIME_DIR": str(folder / "runtime")}
    # Push state (paired devices, APNs, FCM and Web Push keys) and logs stay in
    # `folder`: else this gateway would send its turns to the user's devices. Only
    # properties move them. A native engine reads them as arguments, a JVM engine
    # from JAVA_TOOL_OPTIONS, and the JVM command line refuses unknown flags.
    properties = [
        f"-Dvis.push.home={folder / 'push'}",
        f"-Dvis.logs.dir={folder / 'logs'}",
    ]
    if _runs_native(command):
        command = [*command, *properties]
    else:
        env["JAVA_TOOL_OPTIONS"] = " ".join(
            [env.get("JAVA_TOOL_OPTIONS", ""), *map(_java_option, properties)]
        ).strip()
    # Managed: it stops when its last lease ends, and a stray SIGINT or SIGHUP is
    # ignored. Its registry key is its own database, so no other Vis adopts it.
    env["VIS_GATEWAY_MANAGED"] = "1"
    env.pop("VIS_GATEWAY_URL", None)
    log = folder / "gateway.log"
    try:
        with open(log, "wb") as output:
            process = subprocess.Popen(
                [
                    *command,
                    "gateway",
                    "start",
                    "--host",
                    _HOST,
                    "--port",
                    url.rsplit(":", 1)[1],
                    "--token-file",
                    str(folder / "token"),
                    "--db",
                    db,
                    "--require-token",
                ],
                cwd=folder,
                env=env,
                stdin=subprocess.DEVNULL,
                stdout=output,
                stderr=output,
                start_new_session=True,
            )
    except OSError as error:
        raise TransportError(f"could not start local service: {error}") from None
    _STARTED.append(process)
    threading.Thread(
        target=_reap, args=(process,), name="vis-sdk-service", daemon=True
    ).start()
    record: _Record = {
        "url": url,
        "pid": process.pid,
        "group": process.pid,
        "command": command,
        "db": db,
        "started_at": time.time(),
    }
    deadline = time.monotonic() + startup_timeout
    while True:
        # The launcher can start the engine as a child, so the gateway names its PID.
        health = _probe(url, token)
        pid = None if health is None else health.get("pid")
        if type(pid) is int:
            record["pid"] = pid
            _write(folder, record)
            return record, token
        if process.poll() is not None:
            raise TransportError(
                f"local service exited with status {process.returncode}"
                + _log_tail(log)
            )
        if time.monotonic() >= deadline:
            _signal_group(record, signal.SIGKILL)
            raise VisTimeout("local service did not answer in time" + _log_tail(log))
        time.sleep(0.1)


def _probe(url: str, token: str) -> dict[str, object] | None:
    """Return the health of the gateway at `url` when it holds `token`, else None."""
    request = urllib.request.Request(
        url + "/healthz",
        headers={
            "X-Vis-Gateway-Secret": token,
            # A probe must not make the engine repair its own registry.
            "X-Vis-Suppress-Registry-Recovery": "true",
        },
    )
    try:
        with urllib.request.urlopen(request, timeout=2) as reply:
            health = json.loads(reply.read(65536))
    except (OSError, ValueError):
        return None
    if (
        isinstance(health, dict)
        and health.get("status") == "ok"
        and health.get("secret_match") is True
    ):
        return health
    return None


def _reap(process: subprocess.Popen[bytes]) -> None:
    process.wait()
    with contextlib.suppress(ValueError):
        _STARTED.remove(process)


def _log_tail(log: Path) -> str:
    try:
        with open(log, "rb") as stream:
            size = stream.seek(0, os.SEEK_END)
            stream.seek(max(0, size - _LOG_TAIL))
            end = stream.read()
    except OSError:
        return ""
    if size > _LOG_TAIL:
        end = end.partition(b"\n")[2] or end
    text = end.decode("utf-8", "replace").strip()
    return f"\nLocal service log:\n{text}" if text else ""


def _connected(
    record: _Record, token: str | None, timeout: float, startup_timeout: float
) -> GatewayClient | None:
    """Return a client with a lease, or None when the gateway stopped meanwhile."""
    client = _client(record, token, timeout, startup_timeout)
    try:
        return client.connect()
    except (TransportError, GatewayError):
        client.close()
        return None


def _client(
    record: _Record, token: str | None, timeout: float, startup_timeout: float
) -> GatewayClient:
    if token is None:
        raise TransportError("the local service has no token; call LocalService.stop()")
    return _ServiceClient(
        record["url"], token=token, timeout=timeout, startup_timeout=startup_timeout
    )
