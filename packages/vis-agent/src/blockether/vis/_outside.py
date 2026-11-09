"""The host used when `vis` runs outside the engine.

This module implements the Host protocol. Local operations run locally, and prompts use
the terminal. Session-bound operations refuse without an engine. `VIS_OUTSIDE_ANSWERS`
primes prompt values. With `VIS_OUTSIDE_NONINTERACTIVE=1`, prompts return
`undeliverable` instead of blocking. `VIS_OUTSIDE_EXTENSION` names the extension that
owns the live views, the author of the owner's messages.
"""

from __future__ import annotations

import base64
import json
import os
import secrets
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
    """Refuse a host session request outside a Vis process."""
    raise Refused(
        "vis.sessions, vis.session and vis.council need a Vis host. Outside an "
        "extension, use GatewayClient or LocalEngine from blockether.vis.engine."
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


def read_session(target):
    """Refuse a session read without a bound Vis session and its database."""
    raise Refused(
        "vis.read_session needs a bound Vis session. Outside an extension, read the "
        "session with the SDK client's get_session_transcript(session_id)."
    )


def list_sessions(search):
    """Refuse a session search without a bound Vis session and its database."""
    raise Refused(
        "vis.list_sessions needs a bound Vis session. Outside an extension, list the "
        "sessions with the SDK client's list_sessions()."
    )


def call_env():
    """No op hook runs outside a Vis process, so no call has added variables."""
    return {}


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
# an extension test can attach an artifact and read it back.

_ARTIFACTS = []
_ARTIFACT_BYTES = {}


def attach(options):
    """Store one artifact in memory and return its descriptor as JSON."""
    data = base64.b64decode(options.get("base64") or "")
    if not data:
        raise ValueError("vis.attach: empty payload.")
    name = options.get("filename") or "artifact"
    media_type = (
        options.get("media_type")
        or options.get("media_type_hint")
        or "application/octet-stream"
    )
    with _LOCK:
        row = {
            "id": uuid.uuid4().hex,
            "source": "tool",
            "filename": name,
            "version": 1 + sum(1 for r in _ARTIFACTS if r["filename"] == name),
            "media_type": media_type,
            "kind": options.get("kind")
            or ("image" if media_type.startswith("image/") else "file"),
            "size": len(data),
            "is_pending": True,
            "audience": options.get("audience") or "both",
        }
        _ARTIFACTS.append(row)
        _ARTIFACT_BYTES[row["id"]] = data
    return json.dumps(row)


def list_attachments():
    """List the in-memory artifact descriptors as JSON, oldest first."""
    with _LOCK:
        return json.dumps(_ARTIFACTS)


def _artifact(caller, target, version):
    # The same addressing as the engine: an id, or a filename with an optional
    # version, where a negative version counts back from the latest one.
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
        raise LookupError(f"{caller}: {target!r} has no version {version}.")
    raise LookupError(f"{caller}: unknown id or filename {target!r}.")


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
        raise ValueError(f"show_attachment: {row['filename']!r} is not an image.")
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
_LOG_RETENTION_SECS = 14 * 86400
_LOG_SWEEP_INTERVAL_SECS = 3600
_LOG_SWEEPERS = {}


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
    def __init__(self, command, cwd, timeout_secs, env, stdin=None):
        self.id = "sh-" + secrets.token_hex(4)
        self.command = command
        self.cwd = str(Path(cwd).expanduser()) if cwd else os.getcwd()
        self.timeout_secs = timeout_secs
        self.started_at = time.time()
        home = os.environ.get("VIS_OUTSIDE_HOME")
        log_root = (Path(home) if home else Path.home() / ".vis") / "logs"
        date = time.strftime("%Y-%m-%d", time.gmtime(self.started_at))
        log_dir = log_root / date / "outside"
        log_dir.mkdir(parents=True, exist_ok=True)
        self.log_path = str(log_dir / f"shell-{self.id}.log")
        self.finished_at = None
        self.timed_out = False
        self.stopped = False
        self._sink = open(self.log_path, "wb")
        self.process = subprocess.Popen(
            command
            if isinstance(command, (list, tuple))
            else ["/bin/sh", "-c", command],
            cwd=self.cwd,
            env={**os.environ, **(env or {})},
            # Empty stdin unless asked, as in the engine: a program that reads stdin
            # must not wait for bytes that nothing types.
            stdin=subprocess.PIPE if stdin == "tty" else subprocess.DEVNULL,
            stdout=self._sink,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        )
        _start_log_sweeper(log_root)

    def poll(self):
        code = self.process.poll()
        if code is not None and self.finished_at is None:
            self.finished_at = time.time()
            try:
                self._sink.close()
            except OSError:
                pass
        if (
            code is None
            and self.timeout_secs
            and (time.time() - self.started_at) > float(self.timeout_secs)
        ):
            self.timed_out = True
            self.kill()
            return self.process.poll()
        return code

    def kill(self):
        try:
            self.process.terminate()
            self.process.wait(timeout=2)
        except (OSError, subprocess.TimeoutExpired):
            try:
                self.process.kill()
            except OSError:
                pass
        self.poll()

    def read(self, offset=None, limit=None):
        # A NEGATIVE offset reads the last n LINES; a positive one is a byte cursor.
        try:
            raw = Path(self.log_path).read_bytes()
        except OSError:
            raw = b""
        if offset is not None and int(offset) < 0:
            lines = raw.decode("utf-8", "replace").splitlines()
            text = "\n".join(lines[int(offset) :])
            return text, len(raw), len(raw) - len(text.encode("utf-8"))
        start = max(0, int(offset or 0))
        chunk = raw[start:]
        omitted = 0
        if limit is not None and len(chunk) > int(limit):
            omitted = len(chunk) - int(limit)
            chunk = chunk[: int(limit)]
        return chunk.decode("utf-8", "replace"), start + len(chunk), omitted


def _result(run, stage, **extra):
    code = run.poll()
    now = time.time()
    result: dict[str, Any] = dict.fromkeys(_SHELL_RESULT_KEYS)
    out, next_offset, omitted = run.read(
        extra.pop("_offset", None), extra.pop("_limit", None)
    )
    result.update(
        {
            "stage": stage,
            "id": run.id,
            "cwd": run.cwd,
            "command": run.command,
            "status": (
                "running"
                if code is None
                else (
                    "stopped"
                    if run.stopped
                    else ("timed_out" if run.timed_out else "exited")
                )
            ),
            "pid": run.process.pid,
            "exit": code,
            "duration_ms": int(((run.finished_at or now) - run.started_at) * 1000),
            "uptime_ms": int((now - run.started_at) * 1000),
            "started_at": int(run.started_at * 1000),
            "finished_at": int(run.finished_at * 1000) if run.finished_at else None,
            "log_path": run.log_path,
            "timed_out": run.timed_out,
            "timeout_secs": run.timeout_secs,
            "waiting_for_input": False,
            "out": out,
            "out_omitted_chars": omitted,
            "offset": 0,
            "next_offset": next_offset,
            "is_eof": code is not None,
            "attach": None,
            "already_running": False,
            "keys": None,
            "note": "vis-agent outside a Vis session: a local subprocess, no jail",
        }
    )
    result.update(extra)
    return result


_SPAWN_OPS = ("run", "background")
_HANDLE_OPS = ("logs", "wait", "send", "stop")


def _shell_vocabulary():
    """The ops this host answers, worded the way the engine words its own refusal."""
    names = [
        '"{}"{}'.format(op, " (default)" if op == "run" else "")
        for op in _SPAWN_OPS + _HANDLE_OPS
    ]
    return ", ".join(names[:-1]) + " or " + names[-1]


def shell(opts):
    """Start a process, or drive one this host already started.

    Spawn operations create a process. Handle operations use the process ID that the
    host keeps.
    """
    opts = dict(opts or {})
    op = str(opts.get("op") or "run").strip()
    if op in _SPAWN_OPS:
        stdin = opts.get("stdin")
        if stdin not in (None, "null", "tty"):
            raise Refused(f'shell `stdin` is "tty" or "null", not {stdin!r}.')
        run = _Run(
            opts.get("command"),
            opts.get("cwd"),
            opts.get("timeout_secs"),
            opts.get("env"),
            stdin,
        )
        # A named spawn (the engine's `background`) keeps the id it was given, so
        # the same name reaches the same process on the next call.
        if opts.get("id"):
            run.id = str(opts["id"])
        _RUNS[run.id] = run
        # Settle briefly so a command that finishes at once reports its exit, the
        # way the engine's spawn does, without turning a spawn into a blocking wait.
        for _ in range(20):
            if run.poll() is not None:
                break
            time.sleep(0.01)
        return _result(run, op)
    if op not in _HANDLE_OPS:
        raise Refused(f"Unknown shell op {op!r} — use {_shell_vocabulary()}.")
    run = _RUNS.get(opts.get("id"))
    if run is None:
        raise Refused("no such shell in this process: {!r}".format(opts.get("id")))
    if op == "logs":
        return _result(
            run, "logs", _offset=opts.get("offset"), _limit=opts.get("limit")
        )
    if op == "send":
        text = str(opts.get("text", ""))
        if opts.get("is_enter", True):
            text += "\n"
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
    if op == "stop":
        run.stopped = True
        run.kill()
        return _result(run, "stop")
    if op == "wait":
        deadline = time.time() + float(opts.get("seconds", 120))
        while time.time() < deadline and run.poll() is None:
            time.sleep(0.05)
        return _result(run, "wait")
    # Reached only when the contract declares a handle op this host never grew a
    # branch for: say so, instead of blaming the caller for the engine's vocabulary.
    raise Refused(f"shell op {op!r} is declared in the contract but unimplemented here")


# -- Asking a human, with no dialog surface -----------------------------------

_COMPONENT_SCHEMAS = {
    branch["properties"]["type"]["const"]: branch
    for branch in _contracts.definition("view", "component")["oneOf"]
}
_FIELD_SCHEMAS = {
    kind: branch
    for kind, branch in _COMPONENT_SCHEMAS.items()
    if "name" in branch["required"]
}
_GROUP = next(
    kind
    for kind, branch in _COMPONENT_SCHEMAS.items()
    if "children" in branch["properties"]
)
# Every component that shows text or state and answers nothing.
_DECOR = set(_COMPONENT_SCHEMAS) - set(_FIELD_SCHEMAS) - {_GROUP}
_FIELDS = set(_FIELD_SCHEMAS)
_TEXT = {
    kind
    for kind, branch in _FIELD_SCHEMAS.items()
    if "min_length" in branch["properties"] and "min_length" not in branch["required"]
}
_CHOICE = {
    kind for kind, branch in _FIELD_SCHEMAS.items() if "options" in branch["properties"]
}
_SECRET = {
    kind
    for kind, branch in _FIELD_SCHEMAS.items()
    if branch["properties"]["is_secret"]["const"]
}
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
    # Depth first, groups flattened: a row and a column are the same question in
    # the same order when the surface is a terminal.
    for node in nodes or []:
        if not isinstance(node, dict):
            continue
        kind = node.get("type") or "plaintext"
        if kind == _GROUP:
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
        many = kind == "multiselect"
        picked = input(
            f"{label} [{'numbers, comma separated' if many else 'number'}]: "
        )
        chosen = [p.strip() for p in picked.split(",") if p.strip()]
        values = [
            pairs[int(p) - 1][0]
            for p in chosen
            if p.isdigit() and 0 < int(p) <= len(pairs)
        ]
        return values if many else (values[0] if values else None)
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
    if value is None or value == "":
        # A blank value is never validated: that is is_required's only job.
        return None
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
    # True for a node that holds an ANSWER: not a decoration, not a group.
    kind = node.get("type") or "plaintext"
    return kind not in _DECOR and kind != _GROUP


def _check_node(node, seen):
    if not isinstance(node, dict):
        return f"every field must be a map, got {node!r}"
    kind = node.get("type") or "plaintext"
    if kind == _GROUP:
        children = node.get("children")
        if not isinstance(children, (list, tuple)) or not children:
            return f"a {_GROUP} must arrange at least one node"
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
    if kind in _CHOICE and not (node.get("options") or []):
        return f"{name!r} is a {kind} and needs options to pick from"
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
_LIVE_HANDLE_OPS = ("patch", "state", "events", "close")
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
    # The nodes that PAINT, groups flattened: what a reader reads is CONTENT, and a
    # row is only how a surface arranged it.
    leaves = []
    for node in nodes or []:
        if node.get("type") == _GROUP:
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


def live(envelope_json, listener=None):
    """`vis.live` with no pane: stderr is the surface, and the record with it.

    No human acts here, so a `listener` never gets an event.
    """
    envelope = json.loads(envelope_json)
    if not isinstance(envelope, dict):
        raise Refused("a live envelope must be a JSON object")
    op = str(envelope.get("op") or "open")
    if op == "open":
        view = envelope.get("view")
        kept = _live_checked(view)
        view_id = str(uuid.uuid4())
        held = {"view_id": view_id, "view": kept, "seq": 0}
        held["condition"] = threading.Condition()
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
    held = _LIVE_VIEWS.get(view_id)
    if held is None:
        raise Refused(
            f"no live view {view_id} is open — it was closed, interrupted, or never opened"
        )
    with held["condition"]:
        if op == "state" and envelope.get("timeout_ms", 0) > 0:
            changed = held["condition"].wait_for(
                lambda: "result" in held or held["seq"] != envelope.get("after_seq"),
                timeout=envelope["timeout_ms"] / 1000,
            )
            if not changed:
                return json.dumps(
                    {"view_id": view_id, "is_open": True, "timed_out": True}
                )
        answer = _live_handle(held, envelope, op, view_id)
        if op in {"patch", "close"}:
            held["condition"].notify_all()
        return answer


def _live_handle(held, envelope, op, view_id):
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
            {"view_id": view_id, "is_open": True, "cursor": after, "events": []}
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
    "workspace_files": workspace_files,
    "setting_declaration": setting_declaration,
    "setting": setting,
    "state_get": state_get,
    "state_put": state_put,
    "state_del": state_del,
    "state_keys": state_keys,
    "log": log,
    "notify": notify,
    "shell": shell,
    "jailed_shell": jailed_shell,
    "call_env": call_env,
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
