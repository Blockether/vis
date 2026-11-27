"""The host used when `vis` runs outside the engine.

This module implements the Host protocol. Local operations run locally, and prompts use
the terminal. Session-bound operations refuse without an engine. `VIS_OUTSIDE_ANSWERS`
primes prompt values. With `VIS_OUTSIDE_NONINTERACTIVE=1`, prompts return
`undeliverable` instead of blocking. `VIS_OUTSIDE_EXTENSION` names the extension that
owns the live views, the author of the owner's messages.
"""

from __future__ import annotations

import atexit
import base64
import json
import os
import queue
import re
import secrets
import shlex
import shutil
import signal
import stat
import subprocess
import sys
import textwrap
import threading
import time
import uuid
from copy import deepcopy
from datetime import date
from pathlib import Path
from typing import TYPE_CHECKING, Any

from blockether.vis import _contracts

if TYPE_CHECKING:
    from blockether.vis.extension import Host


def check_host(host) -> Host:
    """Refuse a host that does not answer every implemented host operation.

    Returns the host, so a constructor can `return check_host(built)`. An incomplete
    host then fails where it is built, and the error names the missing ops. It does not
    fail later inside an extension.
    """
    missing = [
        name for name in _IMPLEMENTATIONS if not callable(getattr(host, name, None))
    ]
    if missing:
        raise TypeError(
            "host does not answer required operations: " + ", ".join(missing)
        )
    return host


__all__ = ["Refused", "answer_with", "host", "state_home"]


# -- Host capabilities --------------------------------------------------------


class Refused(RuntimeError):
    """An op the contract refuses to serve outside a Vis process."""


def jailed_shell(opts):
    """Refuse a shell operation without the Vis host's confinement boundary."""
    raise Refused(
        "vis.jailed_shell needs the Vis host's jail; nothing outside a Vis process "
        "can enforce it. Run the command with vis.shell when an unjailed process "
        "is what you meant, or run this extension under vis-agent."
    )


def request(frame):
    """Refuse session requests, including child creation, without a Vis host."""
    raise Refused(
        "vis.run, vis.sessions, vis.session and vis.council need a Vis host. Outside an "
        "extension, use GatewayClient or StdioEngineClient from blockether.vis.engine."
    )


def bound_session():
    """Report that no Vis session is bound outside a Vis process."""
    return None


def call_tool(tool, args, kwargs):
    """Refuse another tool without a bound Vis session to run it in."""
    raise Refused(
        f"vis.tools needs a bound Vis session to run `{tool}`. "
        "Outside Vis, import and call the other package directly."
    )


def read_session(options):
    """Refuse a session read without a bound Vis session and its database."""
    raise Refused(
        "vis.read_session needs a bound Vis session. Outside an extension, read the "
        "session with the SDK client's routes.get_session_transcript(session_id)."
    )


def list_sessions(options):
    """Refuse a session search without a bound Vis session and its database."""
    raise Refused(
        "vis.list_sessions needs a bound Vis session. Outside an extension, list the "
        "sessions with the SDK client's list_sessions()."
    )


def call_env():
    """No op hook runs outside a Vis process, so no call has added variables."""
    return {}


def tool_call_provider():
    """No model reply starts a call outside a Vis process, so no provider sent it."""
    return None


# -- Where the outside host keeps things --------------------------------------


def workspace_root():
    """Use the current process directory when no Vis session owns this call."""
    return str(Path.cwd())


def workspace_files():
    """List the files that Git sees under the process directory, or None."""
    root = Path.cwd()
    try:
        done = subprocess.run(
            ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
            cwd=root,
            capture_output=True,
            timeout=60,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    if done.returncode != 0:
        return None
    names = done.stdout.decode("utf-8", "surrogateescape").split("\0")
    return [str(root / name) for name in names if name]


def python_environment():
    """Use the project environment when no Vis session owns this call."""
    return "project"


def python_runtime():
    """Find Vis on `PATH` and its shared packages by the documented environment rule."""
    agent = shutil.which("vis-agent")
    packages = os.environ.get("VIS_PYTHON_PACKAGES")
    if not packages:
        home = os.environ.get("VIS_HOME") or Path.home() / ".vis"
        packages = Path(home) / "python" / "packages"
    return {
        "launcher": [agent] if agent else [],
        "python_executable": None,
        "uv_executable": None,
        "shared_packages": str(packages),
    }


def setting_declaration(spec):
    """Use the package's own JSON Schemas for outside-host declarations."""
    from ._contracts import definition, problems, validate

    spec = dict(spec)
    spec.setdefault(
        "scopes",
        definition("toggle", "contribution")["properties"]["scopes"]["default"],
    )
    found = problems("toggle", "contribution", spec)
    if found:
        name = spec.get("id")
        label = (
            f"Invalid setting {name}" if isinstance(name, str) else "Invalid setting"
        )
        raise ValueError(f"{label}: {' '.join(found)}")
    return validate("toggle", "contribution", spec)


def setting(id, default):
    """Outside callbacks have no session overlay and use the declaration default."""
    return default


def state_home():
    """The directory this host writes to. `VIS_OUTSIDE_HOME` moves it."""
    home = os.environ.get("VIS_OUTSIDE_HOME")
    root = Path(home) if home else Path.home() / ".vis" / "outside"
    root.mkdir(parents=True, exist_ok=True)
    return root


def _state_file():
    return state_home() / "state.json"


_LOCK = threading.RLock()


def _read_state():
    path = _state_file()
    if not path.exists():
        return {}
    try:
        with open(path, encoding="utf-8") as fh:
            return json.load(fh)
    except (OSError, ValueError):
        # A corrupt file is not worth taking an extension down for: the values are
        # a convenience out here, and the next put rewrites the whole document.
        return {}


def _write_state(doc):
    path = _state_file()
    tmp = path.with_suffix(".json.tmp")
    with open(tmp, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2, sort_keys=True, default=str)
        fh.write("\n")
    tmp.replace(path)


def state_get(key):
    with _LOCK:
        return _read_state().get(str(key))


def state_put(key, value):
    with _LOCK:
        doc = _read_state()
        doc[str(key)] = value
        _write_state(doc)
    return None


def state_del(key):
    with _LOCK:
        doc = _read_state()
        doc.pop(str(key), None)
        _write_state(doc)
    return None


def state_keys():
    with _LOCK:
        # A key written as null reads back as missing, so it is not a key.
        return sorted(key for key, value in _read_state().items() if value is not None)


# -- Talking to the operator --------------------------------------------------


def log(level, message):
    # stderr, never stdout: an extension's own output is often the thing being
    # piped somewhere, and a log line has no business in it.
    print(f"[vis {str(level).lower()}] {message}", file=sys.stderr, flush=True)
    return None


def notify(
    text, level="info", title=None, duration_ms=None, notice_id=None, scope=None
):
    head = f"{title}: " if title else ""
    print(
        f"[vis notify/{str(level).lower()}] {head}{text}", file=sys.stderr, flush=True
    )
    return None


# -- Bus ----------------------------------------------------------------------
# Outside Vis there is one process, so one extension, and no session. The bus
# delivers as the engine does: `publish` does not wait for a handler, one thread of
# the bus runs one handler at a time, each subscription gets its signals in publish
# order, and a subscription with `bus_backlog` waiting signals does not get the next
# one. `vis.bus` checks the names, the scope and the size before it calls here.

_SUBSCRIBERS = {}  # subscription ID -> _Subscriber
_DELIVERIES = queue.SimpleQueue()  # (subscription ID, _Subscriber, signal)
_BUS_LOCK = threading.Lock()  # guards the waiting counts and the thread of the bus
_BUS_THREAD = []


class _Subscriber:
    __slots__ = ("pattern", "listener", "scope", "waiting")

    def __init__(self, pattern, listener, scope):
        self.pattern = pattern
        self.listener = listener
        self.scope = scope
        self.waiting = 0


def _bus_matches(pattern, topic):
    """True when `topic` is one of the topics of `pattern`, as in the engine."""
    if (pattern.split(".", 1)[0] == "vis") != (topic.split(".", 1)[0] == "vis"):
        return False
    words, topic_words = pattern.split("."), topic.split(".")
    for index, word in enumerate(words):
        if word == "#":
            return True
        if index >= len(topic_words) or word not in ("*", topic_words[index]):
            return False
    return len(words) == len(topic_words)


def _bus_deliver():
    """Run the handlers, one at a time, in the order of the signals."""
    while True:
        subscription_id, row, signal = _DELIVERIES.get()
        try:
            # As in the engine, no delivery starts after `close()` returns.
            if subscription_id in _SUBSCRIBERS:
                row.listener(signal)
        except BaseException as error:  # the engine logs each Throwable and goes on
            log("warn", f"A signal subscriber failed: {signal['topic']}: {error!r}")
        finally:
            with _BUS_LOCK:
                row.waiting -= 1


def bus_publish(topic, data_json, scope):
    signal = {
        "topic": topic,
        "data_json": data_json,
        "scope": scope,
        "published_at_ms": int(time.time() * 1000),
    }
    backlog = _contracts.definition("gateway", "bus_backlog")["maximum"]
    targets = [
        (subscription_id, row)
        for subscription_id, row in list(_SUBSCRIBERS.items())
        if row.scope in (None, scope) and _bus_matches(row.pattern, topic)
    ]
    dropped = 0
    with _BUS_LOCK:
        for subscription_id, row in targets:
            if row.waiting >= backlog:
                dropped += 1
                continue
            row.waiting += 1
            _DELIVERIES.put((subscription_id, row, signal))
        if targets and not (_BUS_THREAD and _BUS_THREAD[0].is_alive()):
            thread = threading.Thread(target=_bus_deliver, name="vis-bus", daemon=True)
            _BUS_THREAD[:] = [thread]
            thread.start()
    for _ in range(dropped):
        log(
            "warn",
            f"A signal subscriber is too far behind; it does not get this signal: {topic}",
        )
    return len(targets)


def bus_subscribe(pattern, listener, scope):
    subscription_id = str(uuid.uuid4())
    _SUBSCRIBERS[subscription_id] = _Subscriber(pattern, listener, scope)
    return subscription_id


def bus_unsubscribe(subscription_id):
    return _SUBSCRIBERS.pop(str(subscription_id), None) is not None


# -- Secrets ------------------------------------------------------------------

_VAULT = {}
_PREFIX = _contracts.definition("view", "secret_handle")["pattern"].removeprefix("^")


def _stash(plaintext):
    handle = _PREFIX + secrets.token_hex(8)
    _VAULT[handle] = plaintext
    return handle


def reveal_secret(handle):
    return _VAULT.get(str(handle))


def forget_secret(handle):
    return _VAULT.pop(str(handle), None) is not None


# -- Attachments ----------------------------------------------------------------
# Outside Vis, no session stores artifacts. They stay in this process memory, so
# an extension test can attach an artifact and read it back. A refusal has the
# words of the engine and is a `RuntimeError`: in Vis it is `VisToolError`, a
# `RuntimeError`. `vis-contract/fixtures/attachment-verbs.json` holds the answers.

_ARTIFACTS = []
_ARTIFACT_BYTES = {}


# The characters that Python reads as a space and the engine does not.
_NO_ENGINE_SPACE = "\x85\u00a0\u2007\u202f"


def _option_text(value):
    # A given option as the engine reads it: its text, and no value for None, for
    # False and for blank text. The text keeps its spaces.
    if value is None or value is False:
        return None
    text = "true" if value is True else str(value)
    is_blank = all(ch.isspace() and ch not in _NO_ENGINE_SPACE for ch in text)
    return None if is_blank else text


def attach(options):
    """Store one artifact in memory and return its descriptor as JSON."""
    data = base64.b64decode(options.get("base64") or "")
    if not data:
        raise RuntimeError("attach: empty payload.")
    name = options.get("filename") or "artifact"
    media_type = (
        _option_text(options.get("media_type"))
        or options.get("media_type_hint")
        or "application/octet-stream"
    )
    with _LOCK:
        row = {
            "id": str(uuid.uuid4()),
            "source": "tool",
            "filename": name,
            "version": 1 + sum(1 for r in _ARTIFACTS if r["filename"] == name),
            "media_type": media_type,
            "kind": _option_text(options.get("kind"))
            or ("image" if media_type.startswith("image/") else "file"),
            "size": len(data),
            "is_pending": True,
            "audience": options.get("audience") or "both",
        }
        _ARTIFACTS.append(row)
        _ARTIFACT_BYTES[row["id"]] = data
    return json.dumps(row)


def attachment_limit():
    """No limit: outside Vis, `attach` reads each file whole into this process."""
    return None


def attach_reference(options):
    """Refuse a reference. Without a limit, `attach` never asks for one."""
    raise RuntimeError("attach: a reference to a file needs a Vis session.")


def list_attachments():
    """List the in-memory artifact descriptors as JSON, oldest first."""
    with _LOCK:
        return json.dumps(_ARTIFACTS)


# The engine quotes a name as Clojure `pr-str` does: each other character stays.
_QUOTED = str.maketrans(
    {
        '"': '\\"',
        "\\": "\\\\",
        "\n": "\\n",
        "\t": "\\t",
        "\r": "\\r",
        "\f": "\\f",
        "\b": "\\b",
    }
)


def _quoted(text):
    return '"' + str(text).translate(_QUOTED) + '"'


def _artifact(caller, target, version):
    # The same addressing as the engine: an id, or a filename with an optional
    # version, where a negative version counts back from the latest one.
    if version is not None and not -(2**63) <= version < 2**63:
        raise RuntimeError(f"Value out of range for long: {version}")
    with _LOCK:
        if version is None:
            for row in _ARTIFACTS:
                if row["id"] == target:
                    return row
        versions = sorted(
            (r for r in _ARTIFACTS if r["filename"] == target),
            key=lambda r: r["version"],
        )
    if versions:
        if version is None:
            return versions[-1]
        if -len(versions) <= version <= -1:
            return versions[version]
        for row in versions:
            if row["version"] == version:
                return row
        known = ", ".join(str(row["version"]) for row in versions)
        raise RuntimeError(
            f"{caller}: {_quoted(target)} has no version {version} (versions: {known})."
        )
    hint = (
        "local image: show_attachment(attach(path))"
        if caller == "show_attachment"
        else "use list_attachments()"
    )
    raise RuntimeError(f"{caller}: unknown id or filename {_quoted(target)}; {hint}.")


def get_attachment(target, version):
    """Return one in-memory artifact descriptor as JSON."""
    return json.dumps(_artifact("get_attachment", target, version))


def read_attachment(target, version):
    """Return the bytes of one in-memory artifact as base64 text."""
    row = _artifact("read_attachment", target, version)
    return base64.b64encode(_ARTIFACT_BYTES[row["id"]]).decode("ascii")


def show_attachment(target, version):
    """Accept an image. Outside Vis, no model request follows to show it to."""
    row = _artifact("show_attachment", target, version)
    if not row["media_type"].startswith("image/"):
        raise RuntimeError(
            f"show_attachment: {_quoted(row['filename'])} is not an image."
        )
    return [row["id"], row["filename"], row["media_type"], row["size"]]


# -- Environment --------------------------------------------------------------


def declare_env(names_json):
    # The engine resolves declared names through the operator's configured
    # secrets; out here the process environment IS the configuration.
    names = [str(name) for name in json.loads(names_json) or []]
    # A name without a value stays absent, as in the engine, never the text "None".
    return json.dumps({name: os.environ[name] for name in names if name in os.environ})


# -- Shell --------------------------------------------------------------------

# The engine's shell result shape (`internal.foundation.shell/shell-result-base`),
# in its order. Every op answers ALL of these keys so no lookup can KeyError, and
# a key the outside cannot know (a jail's accounting, sampled CPU and RSS) is
# present and None rather than absent. `python_package_test` pins this tuple
# against the engine's own def.
_SHELL_RESULT_KEYS = (
    "stage",
    "id",
    "cwd",
    "command",
    "status",
    "pid",
    "exit",
    "duration_ms",
    "uptime_ms",
    "started_at",
    "finished_at",
    "log_path",
    "cpu_ms",
    "cpu_percent",
    "rss_bytes",
    "timed_out",
    "timeout_secs",
    "waiting_for_input",
    "out",
    "out_omitted_chars",
    "offset",
    "next_offset",
    "is_eof",
    "attach",
    "already_running",
    "keys",
    "note",
)

_RUNS = {}
_RUNS_LOCK = threading.Lock()
_LOG_RETENTION_SECS = 14 * 86400
_LOG_SWEEP_INTERVAL_SECS = 3600
_LOG_SWEEPERS = {}


def _end_runs():
    """End every shell of this host that still runs, when the host exits.

    Each shell leads its own session, so nothing else ends it after this process.
    """
    with _RUNS_LOCK:
        live = list(_RUNS.values())
    for run in live:
        run.cancel()


atexit.register(_end_runs)


def _sweep_logs(log_root):
    """Remove stale SDK logs without following links or touching other producers."""
    cutoff = time.time() - _LOG_RETENTION_SECS
    if log_root.is_symlink():
        return
    try:
        dated_dirs = list(log_root.iterdir())
    except OSError:
        return
    for dated in dated_dirs:
        try:
            if date.fromisoformat(dated.name).isoformat() != dated.name:
                continue
            outside = dated / "outside"
            if dated.is_symlink() or outside.is_symlink():
                continue
            stale_dirs = set()
            for directory in (outside, dated):
                try:
                    if directory.lstat().st_mtime < cutoff:
                        stale_dirs.add(directory)
                except FileNotFoundError:
                    pass
            try:
                log_files = list(outside.iterdir())
            except FileNotFoundError:
                log_files = []
            deleted = False
            for log_file in log_files:
                try:
                    attrs = log_file.lstat()
                    if stat.S_ISREG(attrs.st_mode) and attrs.st_mtime < cutoff:
                        log_file.unlink()
                        deleted = True
                except OSError:
                    pass
            for directory in (outside, dated):
                try:
                    # Do not remove a fresh directory before its writer opens a log.
                    if deleted or directory in stale_dirs:
                        directory.rmdir()
                        deleted = True
                except OSError:
                    pass
        except (OSError, ValueError):
            continue


def _start_log_sweeper(log_root):
    # Resolve relative overrides once: later cwd/env changes must not redirect it.
    log_root = log_root.absolute()
    with _LOCK:
        if log_root in _LOG_SWEEPERS:
            return
        stop = threading.Event()
        interval = _LOG_SWEEP_INTERVAL_SECS

        def sweep():
            while not stop.is_set():
                try:
                    _sweep_logs(log_root)
                except OSError:
                    pass
                if stop.wait(interval):
                    break

        thread = threading.Thread(
            target=sweep, name="vis-outside-log-sweep", daemon=True
        )
        _LOG_SWEEPERS[log_root] = (stop, thread)
        try:
            thread.start()
        except RuntimeError:
            # Retention is best-effort; a later shell can retry thread creation.
            _LOG_SWEEPERS.pop(log_root, None)


class _Run:
    def __init__(self, command, cwd, env, stdin=None):
        self.id = "sh-" + secrets.token_hex(4)
        self.command = command
        self.cwd = str(Path(cwd).expanduser()) if cwd else os.getcwd()
        self.started_at = time.time()
        home = os.environ.get("VIS_OUTSIDE_HOME")
        log_root = (Path(home) if home else Path.home() / ".vis") / "logs"
        date = time.strftime("%Y-%m-%d", time.gmtime(self.started_at))
        log_dir = log_root / date / "outside"
        log_dir.mkdir(parents=True, exist_ok=True)
        self.log_path = str(log_dir / f"shell-{self.id}.log")
        self.finished_at = None
        self.is_stopped = False
        self._lock = threading.RLock()
        self._terminal = None
        self._sink = open(self.log_path, "wb")
        try:
            self.process = subprocess.Popen(
                ["/bin/sh", "-c", command],
                cwd=self.cwd,
                # A name mapped to None is unset for the child, as in the engine.
                env={
                    name: value
                    for name, value in {**os.environ, **(env or {})}.items()
                    if value is not None
                },
                # Empty stdin unless asked, as in the engine: a program that reads stdin
                # must not wait for bytes that nothing types.
                stdin=subprocess.PIPE if stdin == "tty" else subprocess.DEVNULL,
                stdout=self._sink,
                stderr=subprocess.STDOUT,
                start_new_session=True,
            )
        except BaseException:
            self._sink.close()
            raise
        _start_log_sweeper(log_root)

    def poll(self):
        with self._lock:
            code = self.process.poll()
            if code is not None and self.finished_at is None:
                self.finished_at = time.time()
                self._sink.close()
                if self.process.stdin is not None:
                    self.process.stdin.close()
            # Report an end by signal N as exit 128 + N, as the engine does.
            return 128 - code if code is not None and code < 0 else code

    def signal_group(self, sig):
        try:
            os.killpg(self.process.pid, sig)
        except (PermissionError, ProcessLookupError):
            pass

    def cancel(self):
        with self._lock:
            if self.is_stopped or self.poll() is not None:
                return False
            self.signal_group(signal.SIGTERM)
            time.sleep(0.1)
            # Keep the leader unreaped until the final signal, so the group ID
            # cannot refer to another process if the leader exits first.
            self.signal_group(signal.SIGKILL)
            try:
                self.process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                pass
            self.poll()
            self.is_stopped = True
            self._terminal = {"state": "cancelled", "snapshot": self._final_snapshot()}
            return True

    def outcome(self, timeout_ms):
        deadline = time.monotonic() + timeout_ms / 1000
        while True:
            with self._lock:
                if self._terminal is not None:
                    return self._terminal
                if self.poll() is not None:
                    self._terminal = {"state": "done", "value": self._final_snapshot()}
                    return self._terminal
            left = deadline - time.monotonic()
            if left <= 0:
                return {"state": "running"}
            time.sleep(min(0.05, left))

    def _final_snapshot(self):
        result = _result(self, "run")
        raw = self.log()
        out, omitted = _result_text(raw)
        return dict(result, out=out, out_omitted_chars=omitted, next_offset=len(raw))

    def snapshot(self):
        # A cancelling thread must keep the group leader unreaped. Do not wait
        # for that thread, and do not poll the subprocess while it has the lock.
        terminal = self._terminal
        if terminal is not None:
            return terminal.get("snapshot") or terminal["value"]
        if self._lock.acquire(blocking=False):
            try:
                self.poll()
            finally:
                self._lock.release()
        result = _result(self, "run", refresh=False)
        start = end = 0
        raw = b""
        try:
            with open(self.log_path, "rb") as log:
                end = log.seek(0, os.SEEK_END)
                start = log.seek(max(0, end - _LOG_PAGE_CAP_BYTES))
                raw = log.read(end - start)
        except OSError:
            pass
        while raw and start and raw[0] & 0xC0 == 0x80:
            raw = raw[1:]
            start += 1
        result.update(out=raw.decode("utf-8", "replace"), offset=start, next_offset=end)
        if result["status"] == "running":
            result.pop("timed_out")
            result.pop("timeout_secs")
        terminal = self._terminal
        if terminal is not None:
            return terminal.get("snapshot") or terminal["value"]
        return result

    def log(self):
        try:
            return Path(self.log_path).read_bytes()
        except OSError:
            return b""


# The bounds of the engine's log reader (`internal.foundation.shell-log`) and of its
# wait answer. The contract has no schema for them. The `shell-handle` fixture pins
# each of them against the engine.
_LOG_PAGE_BYTES = 16384
_LOG_PAGE_CAP_BYTES = 262144
_WAIT_HEAD_CHARS = 100000
_WAIT_TAIL_CHARS = 300000


def _tail_start(raw, end, limit, count):
    # The byte where the last `count` lines before `end` start, inside the last
    # `limit` bytes. A newline at `end` closes the last line; it does not start an
    # empty line. A window with fewer lines starts at its first byte.
    base = end - min(end, limit)
    index = end - 1 if raw[end - 1 : end] == b"\n" else end
    for _ in range(count):
        index = raw.rfind(b"\n", base, index)
        if index < 0:
            return base
    return index + 1


def _split_bytes(buf, is_cut):
    # The bytes of a split UTF-8 character at the start of `buf` and, when the log
    # continues after `buf`, at its end. A read never answers a part of a character.
    head = 0
    while head < min(len(buf), 3) and buf[head] & 0xC0 == 0x80:
        head += 1
    tail = 0
    index = len(buf) - 1
    while is_cut and index >= 0 and len(buf) - index <= 4:
        byte = buf[index]
        if byte & 0xC0 != 0x80:
            width = (
                2
                if byte & 0xE0 == 0xC0
                else 3
                if byte & 0xF0 == 0xE0
                else 4
                if byte & 0xF8 == 0xF0
                else 1
            )
            if len(buf) - index < width:
                tail = len(buf) - index
            break
        index -= 1
    return head, tail


def _read_log(raw, offset, limit, lines):
    """Answer one log window as `(text, start, next)` with the rules of the engine.

    A negative offset reads the last n lines; a positive offset is a byte cursor.
    `lines` is the line window: the last n lines without an offset, the next n lines
    from an offset, and the n lines that end at the offset when it is negative.
    `limit` is the byte size of one read. Without an offset, the read ends at the
    end of the log.
    """
    size = len(raw)
    cap = min(max(_LOG_PAGE_BYTES if limit is None else limit, 1), _LOG_PAGE_CAP_BYTES)
    count = lines or 0
    anchor = size if offset is None or offset < 0 else min(offset, size)
    if count < 0:
        start = _tail_start(raw, anchor, cap, -count)
    elif offset is None:
        start = _tail_start(raw, size, cap, count) if count else max(0, size - cap)
    elif offset < 0:
        start = _tail_start(raw, size, cap, -offset)
    else:
        start = anchor
    buf = raw[start : start + cap]
    head, tail = _split_bytes(buf, start + len(buf) < size)
    keep = max(0, len(buf) - head - tail)
    if count:
        # A line ends at its newline, so the last line of the window is whole.
        end = head
        for _ in range(abs(count)):
            newline = buf.find(b"\n", end, head + keep)
            if newline < 0:
                end = head + keep
                break
            end = newline + 1
        keep = end - head
    if count < 0:
        keep = min(keep, max(0, anchor - start - head))
    text = buf[head : head + keep].decode("utf-8", "replace")
    return text, start + head, start + head + keep


def _result_text(raw):
    """Return bounded terminal text and the number of characters left out."""
    text = raw.decode("utf-8", "replace")
    omitted = len(text) - _WAIT_HEAD_CHARS - _WAIT_TAIL_CHARS
    if omitted <= 0:
        return text, 0
    marker = f"\n\n…[{omitted} chars omitted]…\n\n"
    return text[:_WAIT_HEAD_CHARS] + marker + text[-_WAIT_TAIL_CHARS:], omitted


def _whole(value, what):
    # The number rules of the engine, with its words.
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        shown = (
            json.dumps(value, ensure_ascii=False)
            if isinstance(value, (str, bool))
            else value
        )
        raise Refused(f"{what} must be a number, got {shown}.")
    if value != value or value in (float("inf"), float("-inf")):
        raise Refused(f"{what} must be a finite number, got {value}.")
    if value != int(value):
        raise Refused(
            f"{what} must be a whole number, got {value} — a fraction rounds into "
            "a different value."
        )
    return int(value)


def _not_negative(value, what):
    value = _whole(value, what)
    if value is not None and value < 0:
        raise Refused(f"{what} must not be negative, got {value}.")
    return value


def _shell_option(opts, *names):
    # The first of the names that the options give. The engine reads the same names.
    return next((opts[name] for name in names if opts.get(name) is not None), None)


def _result(run, stage, *, refresh=True, **fields):
    # Log pages and the terminal result include log text and its cursor.
    if refresh:
        run.poll()
    finished = run.finished_at
    code = run.process.returncode if finished is not None else None
    if code is not None and code < 0:
        code = 128 - code
    now = time.time()
    ended = finished if finished is not None else now
    result: dict[str, Any] = dict.fromkeys(_SHELL_RESULT_KEYS)
    result.update(
        {
            "stage": stage,
            "id": run.id,
            "cwd": run.cwd,
            "command": (
                run.command if isinstance(run.command, str) else shlex.join(run.command)
            ),
            "status": "stopped"
            if run.is_stopped
            else "running"
            if code is None
            else "exited",
            "pid": run.process.pid,
            "exit": code,
            "duration_ms": int((ended - run.started_at) * 1000),
            "uptime_ms": int((ended - run.started_at) * 1000),
            "started_at": int(run.started_at * 1000),
            "finished_at": int(finished * 1000) if finished is not None else None,
            "log_path": run.log_path,
            "timed_out": False,
            "timeout_secs": None,
            "waiting_for_input": False,
            "out": "",
            "out_omitted_chars": 0,
            "offset": 0,
            "next_offset": 0,
            "is_eof": code is not None,
            "attach": None,
            "already_running": False,
            "keys": None,
            "note": "vis-agent outside a Vis session: a local subprocess, no jail",
        }
    )
    result.update(fields)
    return result


def _shell_cwd(cwd):
    # The directory that a start names, or the refusal of the engine in its words.
    if cwd is None or not cwd.strip():
        return None
    path = Path(cwd).expanduser()
    if not path.exists():
        raise Refused(f"shell cwd '{cwd}' does not exist ({path.resolve()}).")
    if not path.is_dir():
        raise Refused(
            f"shell cwd '{cwd}' is a file, not a directory ({path.resolve()})."
        )
    return str(path)


def _shell_env(env):
    # The variables of one start: a literal as its text, None to unset a name. A
    # source map needs the stores of Vis, so this host refuses it.
    if env is None:
        return {}
    if not isinstance(env, dict):
        raise Refused(f"env must be a map of NAME → value, got {env!r}")
    values = {}
    for name, value in env.items():
        key = name.strip() if isinstance(name, str) else ""
        if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", key):
            raise Refused(f"env {name!r}: not an environment variable name.")
        if isinstance(value, bool):
            value = "true" if value else "false"
        elif isinstance(value, (int, float)):
            value = str(value)
        elif value is not None and not isinstance(value, str):
            raise Refused(
                f"env {name}: outside Vis a value is a string, a number, a boolean "
                f"or None to unset — got {value!r}."
            )
        values[key] = value
    return values


def _one_line(text, limit):
    # A command as one display line of at most `limit` characters.
    text = " ".join(str(text).split())
    return text[:limit] + "…" if len(text) > limit else text


def _live_run(run_id, command, cwd):
    # The live shell that a start names again by its id, or None when the id is
    # free. The same id with other work is refused, as in the engine.
    run = _RUNS.get(run_id)
    if run is None or run.is_stopped or run.poll() is not None:
        return None
    reason = None
    if command != run.command:
        reason = f"it is running a different command ({_one_line(run.command, 60)})"
    elif cwd and os.path.realpath(cwd) != os.path.realpath(run.cwd):
        reason = f"it is running in a different directory ({os.path.realpath(run.cwd)})"
    if reason:
        raise Refused(
            f"Shell '{run_id}' is already running and {reason} — nothing was started. "
            "Cancel it first with sh.cancel(), or start this command under a different id."
        )
    return run


def _known_run(opts):
    run_id = opts["id"].strip()
    with _RUNS_LOCK:
        run = _RUNS.get(run_id)
    if run is None:
        raise Refused(f"No running background shell {run_id!r} in this session.")
    return run


def shell(opts):
    """Start a validated command or attach to a live local shell."""
    if opts.get("op") == "attach":
        run = _known_run(opts)
        with run._lock:
            if run.is_stopped or run.poll() is not None:
                raise Refused(
                    f"No running background shell {run.id!r} in this session."
                )
            return _shell_descriptor(run, _result(run, "run", already_running=True))
    command = opts["command"]
    stdin = opts.get("stdin")
    env = _shell_env(opts.get("env"))
    cwd = _shell_cwd(opts.get("cwd"))
    # A named start keeps the id that it was given. While that shell runs, the
    # same start answers the same process and starts no second one.
    run_id = (opts.get("id") or "").strip()
    with _RUNS_LOCK:
        live = _live_run(run_id, command, cwd) if run_id else None
        if live is not None:
            snapshot = _result(
                live,
                "run",
                already_running=True,
                note=f"Shell '{run_id}' is already running. Nothing was restarted.",
            )
            return _shell_descriptor(live, snapshot)
        run = _Run(command, cwd, env, stdin)
        if run_id:
            run.id = run_id
        _RUNS[run.id] = run
        snapshot = _result(run, "run", status="running", exit=None, is_eof=False)
        return _shell_descriptor(run, snapshot)


def _shell_descriptor(run, snapshot):
    # The local host can pass its capability by reference without a token registry.
    return {"__vis_shell_task__": {"token": run, "snapshot": snapshot}}


def shell_task(run, op, opts):
    """Drive the exact local process that this capability identifies."""
    if op == "_task_await_batch":
        return [
            run.outcome(0) if isinstance(run, _Run) else {"state": "missing"}
            for run, _ in opts
        ]
    if not isinstance(run, _Run):
        raise Refused("This extension does not own that shell Task.")
    if op == "await":
        return run.outcome(min(250, opts.get("timeout_ms", 0)))
    if op == "cancel":
        return run.cancel()
    if op == "snapshot":
        return run.snapshot()
    if op == "logs":
        offset = _whole(_shell_option(opts, "offset", "start"), "offset")
        lines = _whole(_shell_option(opts, "lines", "n", "tail"), "lines")
        limit = _not_negative(_shell_option(opts, "limit", "bytes"), "limit")
        raw = run.log()
        out, start, end = _read_log(raw, offset, limit, lines)
        return _result(
            run, "logs", out=out, offset=start, next_offset=end, is_eof=end >= len(raw)
        )
    if op == "send":
        text = opts.get("text", "")
        if not isinstance(text, str):
            raise Refused("shell `text` must be a string.")
        if opts.get("is_enter", True):
            text += "\n"
        if run.is_stopped or run.poll() is not None:
            raise Refused(
                f"Background shell '{run.id}' has exited — nothing to send to. "
                "Read its output with sh.logs(offset=0)."
            )
        if run.process.stdin is None:
            raise Refused(
                "The stdin of this shell is empty. To type into it, start the "
                'command with {"stdin": "tty"}.'
            )
        try:
            run.process.stdin.write(text.encode("utf-8"))
            run.process.stdin.flush()
        except (OSError, ValueError, AttributeError):
            pass
        return _result(run, "send")
    raise Refused(f"Unknown SDK shell Task operation: {op!r}.")


# -- Asking a human, with no dialog surface -----------------------------------

_COMPONENT_SCHEMAS = {
    branch["properties"]["type"]["const"]: branch
    for branch in _contracts.definition("view", "component")["oneOf"]
}
_FIELD_SCHEMAS = _contracts.input_branches()
_LAYOUT = {kind for kind in _COMPONENT_SCHEMAS if _contracts.is_layout(kind)}
# Every component that shows text or state and answers nothing.
_DECOR = set(_COMPONENT_SCHEMAS) - set(_FIELD_SCHEMAS) - _LAYOUT
_FIELDS = set(_FIELD_SCHEMAS)
_TEXT = {
    kind
    for kind, branch in _FIELD_SCHEMAS.items()
    if "min_length" in branch["properties"] and "min_length" not in branch["required"]
}
_CHOICE = _contracts.choice_input_types()
_SECRET = _contracts.secret_input_types()
_RANGE = _FIELD_SCHEMAS["range"]["properties"]
_OTP = _FIELD_SCHEMAS["otp"]["properties"]["max_length"]

_PRIMED = {}


def answer_with(values):
    """Answer the NEXT asks from `values` instead of prompting.

    A test can drive an extension end to end this way. A person at the terminal still
    types any value that the mapping does not name. `answer_with({})` clears the
    mapping.
    """
    _PRIMED.clear()
    _PRIMED.update({str(k): v for k, v in dict(values or {}).items()})
    return _PRIMED


def _primed():
    out = dict(_PRIMED)
    raw = os.environ.get("VIS_OUTSIDE_ANSWERS")
    if raw:
        try:
            out.update({str(k): v for k, v in json.loads(raw).items()})
        except (ValueError, AttributeError):
            log("warn", "VIS_OUTSIDE_ANSWERS is not a JSON object; ignoring it")
    return out


def _is_interactive():
    if os.environ.get("VIS_OUTSIDE_NONINTERACTIVE"):
        return False
    try:
        return bool(sys.stdin) and sys.stdin.isatty()
    except (AttributeError, ValueError):
        return False


def _option_pair(option):
    if isinstance(option, dict):
        value = option.get("value", option.get("label"))
        return value, str(option.get("label", value))
    return option, str(option)


def _field_nodes(nodes):
    # Flatten visual layouts in reading order.
    for node in nodes or []:
        if not isinstance(node, dict):
            continue
        kind = node.get("type") or "plaintext"
        if kind in _LAYOUT:
            yield from _field_nodes(node.get("children"))
        else:
            yield node


def _print_decor(node):
    kind = node.get("type")
    text = str(node.get("text", node.get("label", "")))
    if kind == "heading":
        print("\n{}\n{}".format(text, "-" * len(text)), file=sys.stderr)
    else:
        print(textwrap.fill(text, 78), file=sys.stderr)


def _prompt(node, kind):
    label = str(node.get("label") or node.get("name"))
    description = node.get("description")
    if description:
        print(textwrap.fill("  " + str(description), 78), file=sys.stderr)
    if kind in _CHOICE:
        pairs = [_option_pair(o) for o in node.get("options") or []]
        for i, (_value, text) in enumerate(pairs, 1):
            print(f"  {i}) {text}", file=sys.stderr)
        many = node.get("is_multiple") is True
        picked = input(
            f"{label} [{'numbers, comma separated' if many else 'number'}]: "
        )
        chosen = [p.strip() for p in picked.split(",") if p.strip()]
        values = [
            pairs[int(p) - 1][0]
            for p in chosen
            if p.isdigit() and 0 < int(p) <= len(pairs)
        ]
        if many:
            return values if chosen else node.get("default", [])
        return values[0] if values else None
    if kind == "checkbox":
        default = bool(node.get("default"))
        typed = (
            input("{} [{}]: ".format(label, "Y/n" if default else "y/N"))
            .strip()
            .lower()
        )
        return default if not typed else typed.startswith("y")
    if kind == "range":
        low = node.get("min", _RANGE["min"]["default"])
        high = node.get("max", _RANGE["max"]["default"])
        step = node.get("step", _RANGE["step"]["default"])
        typed = input(f"{label} [{low}-{high} step {step}]: ").strip()
        if not typed:
            return node.get("default")
        number = float(typed)
        return int(number) if float(number).is_integer() else number
    if kind == "datetime":
        mode = node.get("mode") or _contracts.datetime_default_mode()
        form = _contracts.datetime_modes()[mode]["form"]
        return input(f"{label} [{form}]: ").strip() or node.get("default")
    if kind == "multiline":
        print(f"{label} (end with a lone '.'):", file=sys.stderr)
        lines = []
        while True:
            line = input()
            if line.strip() == ".":
                break
            lines.append(line)
        return "\n".join(lines)
    if kind in _SECRET:
        import getpass

        boxes = node.get("max_length") or node.get("min_length") or _OTP["default"]
        hint = f" ({boxes} digits)" if kind == "otp" else ""
        return getpass.getpass(f"{label}{hint}: ")
    return input(f"{label}: ")


def _validate(node, value, values, counts, run):
    name = str(node.get("name"))
    if node.get("is_required") and (value is None or value == "" or value == []):
        return "is required"
    if node.get("type") in _CHOICE:
        choice = {
            **node,
            "options": [
                {"value": _option_pair(one)[0]} for one in node.get("options") or []
            ],
        }
        if not _contracts.input_accepts(choice, value):
            return "must use the declared options"
    if value is None or value == "":
        # A blank value is never validated: that is is_required's only job.
        return None
    if node.get("type") == "datetime":
        complaint = (
            _contracts.datetime_complaint(node, value)
            if isinstance(value, str)
            else "must be a date or a time"
        )
        if complaint:
            return complaint
    for index in range(int(counts.get(name, 0))):
        verdict = json.loads(run(name, index, json.dumps(value), json.dumps(values)))
        if verdict is False:
            return "is not valid"
        if isinstance(verdict, str):
            return verdict
    return None


def request_input(request_json, counts_json, run):
    """`vis.ask` with no dialog surface: the terminal is the surface."""
    request = json.loads(request_json)
    counts = json.loads(counts_json) or {}
    invalid = _check_request(request)
    if invalid:
        raise Refused(invalid)
    request_id = str(uuid.uuid4())
    nodes = list(_field_nodes(request.get("nodes")))
    primed = _primed()
    interactive = _is_interactive()
    if not interactive and not all(
        str(n.get("name")) in primed for n in nodes if _answers(n)
    ):
        # No surface, nothing primed: the engine's own word for it, so an
        # extension branches on one vocabulary either side of the boundary.
        log(
            "error",
            "vis.ask cannot reach a human here — no terminal and no primed answers",
        )
        return json.dumps(
            {
                "is_submitted": False,
                "reason": "undeliverable",
                "request_id": request_id,
                "values": {},
            }
        )
    title = str(request.get("title") or "")
    print(f"\n== {title} ==", file=sys.stderr)
    if request.get("description"):
        print(textwrap.fill(str(request["description"]), 78), file=sys.stderr)
    values = {}
    try:
        for node in nodes:
            if not _answers(node):
                _print_decor(node)
                continue
            name = str(node.get("name"))
            kind = node.get("type") or "plaintext"
            for attempt in range(20):
                if name in primed:
                    value = primed[name]
                elif kind in _TEXT or kind not in _FIELDS:
                    value = _prompt(node, kind) or node.get("default")
                else:
                    value = _prompt(node, kind)
                if value is None and node.get("default") is not None:
                    value = node.get("default")
                if kind in _CHOICE and node.get("is_multiple") is True:
                    options = [
                        _option_pair(option)[0] for option in node.get("options") or []
                    ]
                    value = _contracts.ordered_choice(options, value)
                values[name] = value
                complaint = _validate(node, value, values, counts, run)
                if not complaint:
                    break
                print(
                    "  {} {}".format(node.get("label") or name, complaint),
                    file=sys.stderr,
                )
                if name in primed or not interactive or attempt == 19:
                    return json.dumps(
                        {
                            "is_submitted": False,
                            "reason": "cancelled",
                            "request_id": request_id,
                            "values": {},
                        }
                    )
    except (KeyboardInterrupt, EOFError):
        print("", file=sys.stderr)
        return json.dumps(
            {
                "is_submitted": False,
                "reason": "cancelled",
                "request_id": request_id,
                "values": {},
            }
        )
    for node in nodes:
        if _answers(node) and (node.get("type") in _SECRET):
            name = str(node.get("name"))
            if values.get(name):
                values[name] = _stash(values[name])
    return json.dumps(
        {"is_submitted": True, "reason": "", "request_id": request_id, "values": values}
    )


# -- Judging a form without asking anyone -------------------------------------


def _answers(node):
    # An answer comes from an input, not a decoration or a layout.
    kind = node.get("type") or "plaintext"
    return kind in _FIELDS


def _check_node(node, seen):
    if not isinstance(node, dict):
        return f"every field must be a map, got {node!r}"
    kind = node.get("type") or "plaintext"
    if kind in _LAYOUT:
        children = node.get("children")
        if not isinstance(children, (list, tuple)) or not children:
            return f"{kind} needs at least one node"
        for child in children:
            complaint = _check_node(child, seen)
            if complaint:
                return complaint
        return None
    if kind in _DECOR:
        if node.get("name"):
            return f"a {kind} reads, it does not answer, so it takes no name"
        return None
    if kind not in _FIELDS:
        return "unknown field type {!r} — one of {}".format(
            kind, ", ".join(sorted(_FIELDS))
        )
    name = node.get("name")
    if not isinstance(name, str) or not name.strip():
        return "every field needs a name: it is what keys the answer"
    if name in seen:
        return f"two fields are named {name!r}"
    seen.add(name)
    if kind in _CHOICE:
        options = node.get("options") or []
        if "is_multiple" in node and not isinstance(node["is_multiple"], bool):
            return f"{name!r} is_multiple must be true or false"
        if kind == "select" and "default" in node:
            choice = {
                **node,
                "options": [{"value": _option_pair(one)[0]} for one in options],
            }
            if not _contracts.input_accepts(choice, node["default"]):
                return f"{name!r} default must use the declared options"
        rule = _FIELD_SCHEMAS[kind]["properties"]["options"]
        if len(options) < rule["minItems"]:
            return f"{name!r} is a {kind} and needs at least {rule['minItems']} options"
        if "maxItems" in rule and len(options) > rule["maxItems"]:
            return f"{name!r} is a {kind} and takes at most {rule['maxItems']} options"
    if kind == "otp":
        boxes = node.get("max_length") or node.get("min_length") or _OTP["default"]
        if int(boxes) > int(_OTP["maximum"]):
            return "{!r} asks for {} boxes, more than the {} a dialog fits".format(
                name,
                boxes,
                _OTP["maximum"],
            )
    if kind == "range":
        low = node.get("min", _RANGE["min"]["default"])
        high = node.get("max", _RANGE["max"]["default"])
        step = node.get("step", _RANGE["step"]["default"])
        if low >= high:
            return f"{name!r} has min {low} and max {high}: a track needs room"
        if step <= 0:
            return f"{name!r} has step {step}: a track advances"
    if kind == "datetime":
        modes = _contracts.datetime_modes()
        mode = node.get("mode") or _contracts.datetime_default_mode()
        if mode not in modes:
            return "{!r} has mode {!r} — one of {}".format(
                name, mode, ", ".join(sorted(modes))
            )
        low, high = node.get("min"), node.get("max")
        for key, bound in (("min", low), ("max", high)):
            if bound is not None and not _contracts.is_datetime_text(mode, bound):
                return "{!r} has {} {!r}: it must have the form {}".format(
                    name, key, bound, modes[mode]["form"]
                )
        if low is not None and high is not None and low > high:
            return f"{name!r} has min {low} after max {high}"
    return None


def _check_request(request):
    if not isinstance(request, dict):
        return "a request is a map of title and nodes"
    title = request.get("title")
    if not isinstance(title, str) or not title.strip():
        return "a request needs a title"
    nodes = request.get("nodes")
    if not isinstance(nodes, (list, tuple)) or not nodes:
        return "a request needs at least one node"
    seen = set()
    for node in nodes:
        complaint = _check_node(node, seen)
        if complaint:
            return complaint
    if not seen:
        return "a request needs at least one field that answers"
    return None


# -- A live view with no engine: the terminal is the record --------------------


# Outside Vis nobody is watching a pane, so a view becomes a TRANSCRIPT on
# stderr: the title once, a line per push, the verdict at the end. The nodes are
# held here too, because `state` must answer what the view really carries — the
# same rules as the engine, from the shared `_contracts.apply_live_ops`, so an
# extension polling its own view reads the truth either side of the boundary.
_LIVE_REASONS = _contracts.definition("view", "settlement_reason")["enum"]
_LIVE_HANDLE_OPS = (
    "patch",
    "state",
    "events",
    "validations",
    "close",
    "watch",
    "watch-result",
    "unwatch",
)
_LIVE_VIEWS = {}


def _live_author():
    """The author that Vis writes on a message of the owner. `VIS_OUTSIDE_EXTENSION`
    names the extension that owns the views; without it the owner is the agent."""
    return _contracts.owner_author(os.environ.get("VIS_OUTSIDE_EXTENSION"))


def _live_node_kept(node):
    """`node` as this host keeps it: the schema defaults, the keys that the host
    writes, and log text made visible."""
    node = _contracts.fill_live_node(node)
    for one in _contracts._wire_tree([node]):
        _contracts.normalize_canvas_ids(one)
        _contracts.stamp_live_threads(one, int(time.time() * 1000), _live_author())
        kind = one.get("type")
        found = _contracts.component_variant(kind)
        properties = found[1]["properties"] if found else {}
        if "is_secret" in properties:
            one["is_secret"] = properties["is_secret"]["const"]
        if kind == "log":
            one["lines"] = [
                _contracts.log_text(line) for line in one.get("lines") or []
            ]
            one.setdefault("total_lines", len(one["lines"]))
        if kind == "scene" and isinstance(one.get("objects"), list):
            try:
                one["objects"] = _contracts.scene_objects([], one["objects"])
            except (AttributeError, TypeError, ValueError):
                pass  # The check shows this node as an error card.
        # The host counts the clicks of a button and a selectable card.
        if kind == "button" or (kind == "card" and one.get("is_selectable")):
            one.setdefault("clicks", 0)
    return node


def _live_cards(view, faults):
    """Show each node of `faults` as an error card, as the engine does, and log it."""
    if not faults:
        return

    def swap(nodes):
        for index, node in enumerate(nodes):
            node_id = node.get("id")
            if node_id in faults:
                nodes[index] = _contracts.error_card(node_id, faults[node_id])
            elif isinstance(node.get("children"), list):
                swap(node["children"])

    swap(view["nodes"])
    for node_id, text in faults.items():
        _live_fault(view, node_id, text)


def _live_fault(view, node_id, text):
    """Tell the fault of one node on stderr and in the app log; answer its error card."""
    print(f"! {text}", file=sys.stderr)
    if view.get("placement") == "attachment":
        entry = {"level": "error", "message": text}
        _contracts.append_log(view, entry, at=int(time.time() * 1000), source="host")
    return _contracts.error_card(node_id, text)


def _live_checked(view):
    """The opened view as this host keeps it; Refused for a fault of the whole view.

    As the engine: an opened view shows a node with a fault as an error card and
    drops each shortcut that names it.
    """
    faults = _contracts.live_view_faults(view)
    if faults.view:
        raise Refused(_contracts.VIEW_REFUSAL + "; ".join(faults.lines()))
    view = json.loads(json.dumps(view))
    view["nodes"] = [_live_node_kept(node) for node in view["nodes"]]
    if faults.dropped:
        names = {one.node_id for one in faults.dropped}
        kept = [
            one
            for one in view.get("shortcuts") or []
            if one.get("node_id") not in names
        ]
        if kept:
            view["shortcuts"] = kept
        else:
            view.pop("shortcuts", None)
        for one in faults.dropped:
            log("warn", f"vis live view: {one.note}")
    for text in faults.nodes.values():
        log("warn", f"vis live view: {text}")
    _live_cards(view, faults.nodes)
    return view


def _live_leaves(nodes):
    # A terminal transcript reads the content without visual layout.
    leaves = []
    for node in nodes or []:
        if node.get("type") in _LAYOUT:
            leaves.extend(_live_leaves(node.get("children")))
        else:
            leaves.append(node)
    return leaves


def _live_said(op, written):
    """The line that the transcript owes op `op`, which wrote `written`."""
    name = op.get("op")
    if name == "shortcuts":
        # No person presses a key outside Vis: the list only replaces the old one.
        return f"shortcuts: {len(op.get('shortcuts') or [])}"
    if name == "log":
        # Outside Vis the transcript is the Log panel: each entry gets one line.
        return f"log {written['level']}: {written['message']}"
    if name == "agent-note":
        # Outside Vis no model reads the note: the transcript shows it instead.
        text = op["text"].strip()
        return f"agent note: {text}" if text else "agent note: removed"
    if name in ("add-node", "remove-node"):
        if written is None:
            return None
        sign = "+" if name == "add-node" else "-"
        return "{} {}".format(sign, written.get("label") or written.get("id"))
    return _live_line(name, op, written)


def _live_line(name, op, node):
    label = node.get("label") or node.get("id")
    kind = node.get("type")
    if name == "clear":
        return f"{label}: cleared"
    if name == "replace":
        return "{}: {} rows".format(label, len(node.get("rows") or []))
    if name == "remove":
        return "{}: -{}".format(label, len(op.get("item_ids") or []))
    if name == "append" and kind == "log":
        return "\n".join(_contracts.log_text(line) for line in op.get("lines") or [])
    if name == "append":
        key = _contracts.live_collection(kind)
        counted = [
            f"+{len(op[held] or [])} {held}"
            for held in (key, "groups", "meshes")
            if op.get(held)
        ]
        return "{}: {}".format(label, ", ".join(counted) or f"+0 {key}")
    if kind in ("paragraph", "heading", "code", "spinner"):
        return str(node.get("text") or "")
    if kind == "button":
        return f"{label}: {node.get('clicks', 0)} activations"
    if kind == "status":
        detail = node.get("detail")
        return "{}: {}{}".format(
            label, node.get("text") or "", f" — {detail}" if detail else ""
        )
    if kind == "progress":
        total = node.get("total")
        if total:
            return "{}: {} of {}".format(label, node.get("done") or 0, total)
        value = node.get("value")
        return "{}: {}".format(
            label, "working" if value is None else f"{round(value * 100)}%"
        )
    changed = ", ".join(k for k in op if k not in ("op", "node_id"))
    return f"{label}: {changed}"


def _live_say(lines):
    for line in lines:
        if line:
            print(f"  {line}", file=sys.stderr)


def _live_verdict(held, ending):
    reason = str(ending.get("reason") or "completed")
    if reason not in _LIVE_REASONS:
        reasons = ", ".join(_LIVE_REASONS)
        raise Refused(f"a view ends for one of {reasons}, got {reason!r}")
    view = held["view"]
    verdict = {
        "view_id": held["view_id"],
        "is_completed": reason == "completed",
        "reason": reason,
        # Nobody is watching a stderr transcript, so nobody can stop it: the key
        # is still PRESENT, because extension code reads the same verdict shape
        # with an engine and without one.
        "is_from_human": False,
    }
    for key in ("summary", "error", "artifact_id"):
        if str(ending.get(key) or "").strip():
            verdict[key] = str(ending[key]).strip()
    # Nothing is elided outside: this host cuts nothing, so the absent key says
    # so rather than a count of zero.
    verdict["view"] = {
        "title": view.get("title"),
        "description": view.get("description"),
        # Flattened, exactly as the engine's own picture is: a reader reads
        # CONTENT, and a row is only how a surface arranged it.
        "nodes": _live_leaves(view.get("nodes")),
    }
    return verdict


def _held_live(view_id):
    held = _LIVE_VIEWS.get(view_id)
    if held is None:
        raise Refused(
            f"no live view {view_id} is open — it was closed, interrupted, or never opened"
        )
    return held


def _read_live_watch(request):
    held = _held_live(request["view_id"])
    with held["condition"]:
        return held["watches"].call(request)


def live(envelope_json, listener=None):
    """`vis.live` with no pane: stderr is the surface, and the record with it.

    No human acts here, so a `listener` never gets an event.
    """
    envelope = json.loads(envelope_json)
    if not isinstance(envelope, dict):
        raise Refused("a live envelope must be a JSON object")
    op = str(envelope.get("op") or "open")
    if op == "_task_await_batch":
        from blockether.vis.extension import _view_watch_batch

        return json.dumps(_view_watch_batch(envelope, _read_live_watch))
    if op == "open":
        view = envelope.get("view")
        kept = _live_checked(view)
        view_id = str(uuid.uuid4())
        held = {"view_id": view_id, "view": kept, "seq": 0}
        from blockether.vis.extension import _LocalLiveWatches

        def readiness(after_seq, _after):
            if "result" in held:
                return "closed"
            if held["seq"] > after_seq:
                return "changed"
            return None

        held["condition"] = threading.Condition()
        held["watches"] = _LocalLiveWatches(
            held["condition"], readiness, lambda: held.get("result")
        )
        held["view"]["seq"] = 0
        held["view"]["id"] = view_id
        _LIVE_VIEWS[view_id] = held
        print("\n== {} ==".format(kept.get("title")), file=sys.stderr)
        if kept.get("description"):
            print(textwrap.fill(str(kept["description"]), 78), file=sys.stderr)
        return json.dumps({"view_id": view_id, "is_open": True, "view": held["view"]})
    if op not in _LIVE_HANDLE_OPS:
        ops = ", ".join(("open", *_LIVE_HANDLE_OPS))
        raise Refused(f"a live op is one of {ops}, got {op!r}")
    view_id = str(envelope.get("view_id") or "")
    held = _held_live(view_id)
    with held["condition"]:
        if op in {"watch", "watch-result", "unwatch"}:
            return json.dumps(held["watches"].call(envelope))
        answer = _live_handle(held, envelope, op, view_id)
        if op in {"patch", "close"}:
            held["watches"].notify()
        return answer


def _live_handle(held, envelope, op, view_id):
    if op == "validations":
        # The transcript has no input surface that could submit a form.
        return json.dumps({"validations": []})
    if held.get("result"):
        # The engine answers an ended view from its record rather than refusing;
        # the loop pushing into it learns WHY it stopped, in one shape.
        return json.dumps(
            {"view_id": view_id, "is_open": False, "result": held["result"]}
        )
    if op == "state":
        return json.dumps({"view_id": view_id, "is_open": True, "view": held["view"]})
    if op == "events":
        # Outside Vis nobody can act on the view, so no event ever arrives.
        after = envelope.get("after", 0)
        return json.dumps(
            {
                "view_id": view_id,
                "is_open": True,
                "cursor": after,
                "seq": held["seq"],
                "events": [],
            }
        )
    if op == "patch":
        refusal = _contracts.live_patch_refusal(
            envelope.get("patch"), view_id, held["seq"]
        )
        if refusal:
            raise Refused(refusal)
        ops = envelope["patch"]["ops"]
        candidate = deepcopy(held["view"])
        try:
            written = _contracts.apply_live_ops(
                candidate,
                ops,
                kept=_live_node_kept,
                at=int(time.time() * 1000),
                author=_live_author(),
            )
        except ValueError as error:
            raise Refused(str(error)) from None
        held["view"] = candidate
        _live_say(
            [_live_said(one, wrote) for one, wrote in zip(ops, written, strict=True)]
        )
        held["seq"] += 1
        held["view"]["seq"] = held["seq"]
        return json.dumps({"view_id": view_id, "is_open": True, "seq": held["seq"]})
    ending_spec = envelope.get("ending") or {}
    verdict = _live_verdict(held, ending_spec)
    model_result = ending_spec.get("model_result", verdict)
    held["result"] = model_result
    ending = verdict.get("summary") or verdict.get("error") or ""
    print(
        "== {} · {}{} ==".format(
            held["view"].get("title"),
            verdict["reason"],
            f" — {ending}" if ending else "",
        ),
        file=sys.stderr,
    )
    return json.dumps({"view_id": view_id, "is_open": False, "result": model_result})


# -- The host itself ----------------------------------------------------------

_IMPLEMENTATIONS = {
    "workspace_root": workspace_root,
    "project_root": workspace_root,
    "python_environment": python_environment,
    "python_runtime": python_runtime,
    "workspace_files": workspace_files,
    "setting_declaration": setting_declaration,
    "setting": setting,
    "setting_root": workspace_root,
    "state_get": state_get,
    "state_put": state_put,
    "state_del": state_del,
    "state_keys": state_keys,
    "log": log,
    "notify": notify,
    "bus_publish": bus_publish,
    "bus_subscribe": bus_subscribe,
    "bus_unsubscribe": bus_unsubscribe,
    "shell": shell,
    "shell_task": shell_task,
    "jailed_shell": jailed_shell,
    "call_env": call_env,
    "tool_call_provider": tool_call_provider,
    "is_detached": lambda: False,
    "request": request,
    "bound_session": bound_session,
    "call_tool": call_tool,
    "read_session": read_session,
    "list_sessions": list_sessions,
    "request_input": request_input,
    "live": live,
    "activity": lambda blocks: False,
    "reveal_secret": reveal_secret,
    "forget_secret": forget_secret,
    "declare_env": declare_env,
    "attach": attach,
    "list_attachments": list_attachments,
    "attach_reference": attach_reference,
    "attachment_limit": attachment_limit,
    "get_attachment": get_attachment,
    "read_attachment": read_attachment,
    "show_attachment": show_attachment,
}


class _OutsideHost:
    """The host `vis` binds when there is no engine in the room.

    It has one attribute per contract op, because the engine injects the same shape. An
    extension holds a `blockether.vis.extension.Host` in both cases. A third host then has an
    interface to implement, not a dict shape to guess.
    """

    def __init__(self, ops):
        for name, fn in ops.items():
            setattr(self, name, fn)

    def __repr__(self):
        return f"<vis outside host: {len(_IMPLEMENTATIONS)} ops>"


host = check_host(_OutsideHost(_IMPLEMENTATIONS))
