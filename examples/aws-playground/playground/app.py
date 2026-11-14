"""The app function: it runs the web app of the project for the Studio page.

Only the front function invokes it. The function is the security boundary of the app.
Its network has a path only to EFS. Its role permits a read-only EFS mount and a write
to its log group, but no access to other AWS data. The project holds no setting of this
function: the stack decides the interpreter, the entry point and each limit.

The code of the project runs in a child process (`harness.py`) that does not get the
variables of the function. That is not the limit: Lambda refuses the kernel calls that
close `/proc`, so the child can read the credentials of the role from the parent. The
limit is the function: its role and its network give the code nothing more than the
files of the project. The source of a request is the current release, or a work
directory in which a session changes the files.
"""

from __future__ import annotations

import base64
import http.client
import os
import re
import signal
import socket
import subprocess
import threading
import time
from dataclasses import dataclass
from typing import Any

from playground import harness, sync
from playground.env import APP_ENTRY, APP_PYTHON, log
from playground.runtime import Context

# The name of a work directory: the sessions of a release, or one workflow run.
NAME = re.compile(r"[0-9a-f]{12}(-[0-9a-f]{8})?")
SHA = re.compile(r"[0-9a-f]{40,64}")
METHODS = {"GET", "HEAD", "POST"}
# The headers of the browser that the app gets. It gets no cookie and no password.
PASSED = {"accept", "accept-language", "content-type"}
# The seconds for a start and for one request, the bytes of an answer, the number of
# child processes, the number of listed work directories and the bytes of a log tail.
START_S, REQUEST_S, BODY, CHILDREN, LISTED, TAIL = 30, 25, 4_000_000, 3, 50, 3000
SCRATCH = "/tmp/app"


@dataclass
class Child:
    """One process of the harness and its address."""

    process: subprocess.Popen[bytes]
    port: int
    record: str
    ready: bool = False
    used: float = 0.0


# The child of each project directory. A lock, because `local.py` calls from threads.
_children: dict[str, Child] = {}
_lock = threading.Lock()


def directory(source: str) -> str | None:
    """Return the project directory of a source: `release`, or the name of a work directory."""
    if source == "release":
        sha = sync.current()
        path = f"{sync.RELEASES}/{sha}" if SHA.fullmatch(sha) else ""
    else:
        path = f"{sync.WORK}/{source}" if NAME.fullmatch(source) else ""
    return path if path and os.path.isdir(path) else None


def environment() -> dict[str, str]:
    """Return the variables of a child. The code of the project gets no credentials by accident."""
    home = f"{SCRATCH}/home"
    return {"PATH": "/usr/local/bin:/usr/bin:/bin", "HOME": home, "TMPDIR": f"{SCRATCH}/tmp", "MPLCONFIGDIR": home, "LANG": "C.UTF-8"}


def start(path: str) -> Child:
    for name in ("home", "tmp"):
        os.makedirs(f"{SCRATCH}/{name}", mode=0o700, exist_ok=True)
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    record = f"{SCRATCH}/{port}.log"
    # The interpreter ignores the PYTHON variables and does not import from the current directory.
    # Not `-I` or `-s`: the Python of Amazon Linux then drops the packages of the image in /usr/local.
    command = [APP_PYTHON, "-P", "-E", "-B", harness.__file__, path, str(port), APP_ENTRY]
    with open(record, "wb") as output:
        process = subprocess.Popen(command, cwd=path, env=environment(), stdin=subprocess.DEVNULL, stdout=output, stderr=subprocess.STDOUT, start_new_session=True)  # noqa: S603
    log("app started", path, port)
    return Child(process, port, record)


def stop(child: Child) -> None:
    """Stop a child and each process that it started."""
    try:
        os.killpg(child.process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    child.process.wait()
    if os.path.exists(child.record):
        os.remove(child.record)


def drop(path: str) -> None:
    with _lock:
        child = _children.pop(path, None)
    if child:
        stop(child)


def child_of(path: str) -> Child:
    """Return the live child of a project directory. Stop the oldest child when there are too many."""
    with _lock:
        found = _children.get(path)
        if found and found.process.poll() is not None:
            stop(_children.pop(path))
            found = None
        if not found:
            while len(_children) >= CHILDREN:
                stop(_children.pop(min(_children, key=lambda key: _children[key].used)))
            found = _children[path] = start(path)
        found.used = time.time()
        return found


def listens(child: Child, deadline: float) -> bool:
    """Wait until the child accepts a connection. False when it stopped or the time ended."""
    while not child.ready and time.time() < deadline and child.process.poll() is None:
        try:
            socket.create_connection(("127.0.0.1", child.port), timeout=0.5).close()
            child.ready = True
        except OSError:
            time.sleep(0.05)
    return child.ready


def tail(child: Child) -> str:
    try:
        with open(child.record, "rb") as file:
            return file.read()[-TAIL:].decode(errors="replace")
    except OSError:
        return ""


def reply(status: int, text: str) -> dict[str, Any]:
    return {"status": status, "headers": [["content-type", "text/plain; charset=utf-8"], ["cache-control", "no-store"]], "body": base64.b64encode(text.encode()).decode()}


def request(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Send one request of the browser to the app of a source and return its answer."""
    path = directory(str(event.get("source", "")))
    method = str(event.get("method", "GET"))
    if path is None or method not in METHODS:
        return reply(404, "no such source")
    child = child_of(path)
    if not listens(child, time.time() + min(START_S, context.remaining_s() - REQUEST_S - 3)):
        text = tail(child)
        drop(path)
        log("app did not start", path, text[-500:])
        return reply(502, f"The app did not start.\n\n{text}")
    target = str(event.get("path", "/")) + ("?" + event["query"] if event.get("query") else "")
    headers = {name: str(value) for name, value in (event.get("headers") or {}).items() if name in PASSED}
    connection = http.client.HTTPConnection("127.0.0.1", child.port, timeout=REQUEST_S)
    try:
        connection.request(method, target, body=base64.b64decode(event.get("body") or ""), headers=headers)
        answer = connection.getresponse()
        data = answer.read(BODY + 1)
    except TimeoutError:
        drop(path)  # the code of the project does not end: the next request gets a new child
        return reply(504, "The app did not answer in time.")
    except (OSError, ValueError, http.client.HTTPException) as failure:
        return reply(502, f"The app did not answer: {failure}\n\n{tail(child)}")
    finally:
        connection.close()
    if len(data) > BODY:
        return reply(502, "The answer of the app is too large.")
    return {"status": answer.status, "headers": [list(pair) for pair in answer.getheaders()], "body": base64.b64encode(data).decode()}


def sources(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Return the commit of the current release and the newest work directories."""
    work = []
    try:
        entries = list(os.scandir(sync.WORK))
    except OSError:
        entries = []
    for entry in entries:
        if NAME.fullmatch(entry.name) and entry.is_dir(follow_symlinks=False):
            work.append({"name": entry.name, "modified": int(entry.stat().st_mtime)})
    work.sort(key=lambda item: item["modified"], reverse=True)
    return {"release": sync.current(), "work": work[:LISTED]}


def version(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Return the stamp of the source files of a source. A page loads the app again when it changes."""
    path = directory(str(event.get("source", "")))
    return {"version": harness.stamp(path) if path else ""}


ACTIONS = {"request": request, "sources": sources, "version": version}


def handler(event: dict[str, Any], context: Context) -> dict[str, Any]:
    return ACTIONS[event["action"]](event, context)
