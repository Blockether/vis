"""Owned local Vis subprocess, with the same session API and no HTTP listener.

Explicit executable selection only; no download, gateway discovery or user-server
shutdown. Each engine gets a temporary database. Closing discards that database.
Supports Linux/macOS. Instances use one calling thread, like GatewayClient.
"""

from __future__ import annotations

import base64
import io
import json
import os
import select
import signal
import subprocess
import tempfile
import time
from email.message import Message
from pathlib import Path
from urllib.parse import urlencode

from blockether.vis.client import (
    Events,
    GatewayClient,
    JobEvents,
    ProtocolError,
    Session,
    TransportError,
    VisTimeout,
    _duration,
    _gateway_error,
)
from blockether.vis_contract import GATEWAY


class _Reply(io.BytesIO):
    def __init__(self, status, content, headers):
        super().__init__(content)
        self.status = status
        self.headers = Message()
        for name, value in headers.items():
            self.headers[name] = str(value)


class LocalEngine(GatewayClient):
    """Run a private engine via sdk-stdio; executable may be an argv sequence.

    root is the working directory. startup_timeout bounds initial boot; timeout
    bounds each operation. A pipe timeout stops the owned process because a late
    reply must never be mistaken for the next request's answer.
    """

    def __init__(self, *, executable, root, timeout=30, startup_timeout=120):
        super().__init__("http://127.0.0.1", timeout=timeout)
        if os.name != "posix":
            raise ValueError("local engines currently support Linux and macOS")
        self._command = (
            [str(executable)]
            if isinstance(executable, (str, os.PathLike))
            else list(executable)
        )
        if not self._command or not all(
            isinstance(arg, str) and arg for arg in self._command
        ):
            raise ValueError("executable must be a nonempty argv sequence")
        self._root = str(Path(root).resolve(strict=True))
        self._startup_timeout = _duration(startup_timeout)
        self._process = None
        self._home = None
        self._buffer = b""

    def _read_line(self, deadline):
        while b"\n" not in self._buffer:
            remaining = deadline - time.monotonic()
            if (
                remaining <= 0
                or not select.select([self._process.stdout], [], [], remaining)[0]
            ):
                raise VisTimeout("local engine response timed out")
            chunk = os.read(self._process.stdout.fileno(), 65536)
            if not chunk:
                raise TransportError("local engine closed its output")
            self._buffer += chunk
            if len(self._buffer) > 67108864:
                raise ProtocolError("local engine frame exceeds 64 MiB")
        line, self._buffer = self._buffer.split(b"\n", 1)
        try:
            return json.loads(line)
        except (ValueError, UnicodeError):
            raise ProtocolError("malformed local engine response") from None

    def connect(self):
        if self._closed:
            raise TransportError("client is closed")
        if self._process is not None:
            return self
        self._home = tempfile.TemporaryDirectory(prefix="vis-sdk-")
        env = {
            **os.environ,
            "VIS_DB_PATH": str(Path(self._home.name) / "sessions.sqlite"),
        }
        try:
            self._process = subprocess.Popen(
                [*self._command, "sdk-stdio"],
                cwd=self._root,
                env=env,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                start_new_session=True,
            )
            os.set_blocking(self._process.stdin.fileno(), False)
            hello = self._read_line(time.monotonic() + self._startup_timeout)
            if (
                not isinstance(hello, dict)
                or type(hello.get("protocol")) is not int
                or hello["protocol"] != GATEWAY["protocol"]["version"]
            ):
                raise ProtocolError("incompatible local engine protocol")
            return self
        except BaseException as error:
            self.close()
            if isinstance(error, OSError):
                raise TransportError("could not start local engine") from None
            raise

    def _open(
        self, method, route, *, query=None, body=None, content=None, timeout=None
    ):
        self.connect()
        deadline = time.monotonic() + _duration(
            self.timeout if timeout is None else timeout
        )
        request = {
            "method": method,
            "route": route,
            "query": urlencode(query) if query else None,
        }
        if body is not None:
            request["body"] = body
        if content is not None:
            request["content"] = base64.b64encode(content).decode("ascii")
        data = (json.dumps(request, allow_nan=False) + "\n").encode()
        if len(data) > 67108864:
            raise ValueError("local request exceeds 64 MiB")
        try:
            offset = 0
            while offset < len(data):
                remaining = deadline - time.monotonic()
                if (
                    remaining <= 0
                    or not select.select([], [self._process.stdin], [], remaining)[1]
                ):
                    raise VisTimeout("local engine request timed out")
                offset += os.write(self._process.stdin.fileno(), data[offset:])
            response = self._read_line(deadline)
            if (
                not isinstance(response, dict)
                or type(response.get("status")) is not int
                or not isinstance(response.get("headers"), dict)
            ):
                raise ProtocolError("malformed local response envelope")
            decoded = base64.b64decode(response["content"], validate=True)
        except (TransportError, ProtocolError):
            self.close()
            raise
        except (OSError, ValueError, TypeError, KeyError):
            self.close()
            raise ProtocolError(
                "malformed local engine exchange; process was closed"
            ) from None
        if response["status"] >= 400:
            raise _gateway_error(response["status"], decoded[:65536])
        return _Reply(response["status"], decoded, response["headers"])

    def session(self, sid):
        return _LocalSession(self, sid)

    def _job_events(self, route, job_id, event_name, snapshot, **options):
        return _LocalJobEvents(self, route, job_id, event_name, snapshot, **options)

    def close(self):
        if self._closed:
            return
        self._closed = True
        for stream in tuple(self._streams):
            stream.close()
        process = self._process
        if process is not None:
            if process.stdin:
                process.stdin.close()
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(process.pid, signal.SIGTERM)
                except ProcessLookupError:
                    pass
                try:
                    process.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
            if process.stdout:
                process.stdout.close()
        if self._home is not None:
            self._home.cleanup()


class _LocalSession(Session):
    __slots__ = ()

    def events(self, **options):
        return _LocalEvents(self, **options)


class _PollingEvents:
    """Common bounded stdio polling; identity/replay stay in the typed stream."""

    def _iterate(self):
        deadline = time.monotonic() + self.client.timeout
        try:
            while not self._closed:
                if time.monotonic() >= deadline:
                    raise VisTimeout("local event stream idle timeout")
                for name, value in self._page():
                    event = self._accept(name, value)
                    if event is not None:
                        yield event
                        if self._terminal(event):
                            return
                        deadline = time.monotonic() + self.client.timeout
                time.sleep(min(0.1, max(0, deadline - time.monotonic())))
        finally:
            self.client._streams.discard(self)


class _LocalEvents(_PollingEvents, Events):
    """Read the owned engine journal over stdio; no hidden SSE connection."""

    def _page(self):
        data = self.client.get_session_events_since(
            self.session.id, query={"cursor": self.cursor}
        )
        if not isinstance(data, dict) or not isinstance(data.get("events"), list):
            raise ProtocolError("malformed local event page")
        return [(None, value) for value in data["events"]]


class _LocalJobEvents(_PollingEvents, JobEvents):
    def __init__(self, client, route, job_id, event_name, snapshot, **options):
        self._snapshot = snapshot
        super().__init__(client, route, job_id, event_name, **options)

    def _page(self):
        return [(self._event_name, self._snapshot())]
