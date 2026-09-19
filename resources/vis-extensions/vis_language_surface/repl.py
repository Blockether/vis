"""The managed Python REPL, run by the worker that serves this surface.

`repl_start`, `repl_status`, `repl_stop` and `repl_eval` reach this module for
Python. It launches the project's own interpreter — uv, Poetry, a `.venv` or
`python3` — as a persistent child running a small line-framed eval server: one
JSON request per line in, one JSON answer per line out, with globals that live
between evaluations, which is what makes it a REPL rather than a series of
scripts.

One process per project directory, owned by this worker: closing its stdin is
what ends it, so a worker that goes away never leaves an interpreter behind.
"""

from __future__ import annotations

import collections
import json
import os
import queue
import shutil
import subprocess
import threading
import time
from pathlib import Path

DRIVER = r"""import sys, json, io, ast, contextlib, traceback

_G = {'__name__': '__vis_repl__'}


def _repr(value):
    try:
        s = repr(value)
    except Exception as ex:
        s = '<unreprable ' + type(value).__name__ + ': ' + str(ex) + '>'
    return s[:8000]


def _safe(o, depth=0):
    # Make a REAL Python object representable as JSON-safe nested data so the
    # model can read its actual fields, not just an opaque repr. Handles
    # primitives, list/tuple/set, dict, namedtuples (_asdict), numpy/pandas
    # (tolist/to_dict), and plain objects (__dict__, tagged with __type__);
    # anything else degrades to repr. Bounded by depth + per-collection cap.
    if depth > 6:
        return repr(o)
    if o is None or isinstance(o, (bool, int, float, str)):
        return o
    if isinstance(o, (list, tuple)):
        return [_safe(x, depth + 1) for x in list(o)[:1000]]
    if isinstance(o, (set, frozenset)):
        return [_safe(x, depth + 1) for x in list(o)[:1000]]
    if isinstance(o, dict):
        return {str(k): _safe(v, depth + 1) for k, v in list(o.items())[:1000]}
    for attr in ('_asdict', 'tolist', 'to_dict'):
        f = getattr(o, attr, None)
        if callable(f):
            try:
                return _safe(f(), depth + 1)
            except Exception:
                pass
    dd = getattr(o, '__dict__', None)
    if isinstance(dd, dict) and dd:
        out = {'__type__': type(o).__name__}
        for k, v in list(dd.items())[:1000]:
            out[str(k)] = _safe(v, depth + 1)
        return out
    # OPAQUE object — can't be turned into data (file handle, generator, model,
    # connection, C-extension object). It is NOT lost: it stays LIVE in the
    # REPL's globals, so bind it to a name (`m = load_model()`) and keep calling
    # it in later evals. Here we just describe it — type, repr, and (top level)
    # its public attributes/methods — so the model knows what it can do with it.
    info = {'__type__': type(o).__name__, '__repr__': _repr(o), '__opaque__': True}
    if depth == 0:
        attrs = [n for n in dir(o) if not n.startswith('_')][:50]
        if attrs:
            info['__attrs__'] = attrs
    return info


def _run(code):
    out = io.StringIO()
    err = io.StringIO()
    value = None
    ok = True
    exc = None
    has_value = False
    try:
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            block = ast.parse(code, mode='exec')
            body = block.body
            if body and isinstance(body[-1], ast.Expr):
                has_value = True
                pre = ast.Module(body[:-1], [])
                last = ast.Expression(body[-1].value)
                exec(compile(pre, '<repl>', 'exec'), _G)
                value = eval(compile(last, '<repl>', 'eval'), _G)
            else:
                exec(compile(block, '<repl>', 'exec'), _G)
    except BaseException:
        ok = False
        exc = traceback.format_exc()
    has_v = has_value and value is not None
    try:
        data = _safe(value) if has_v else None
    except Exception:
        data = None
    return {'ok': ok,
            'out': out.getvalue(),
            'err': err.getvalue(),
            'value': (_repr(value) if has_v else None),
            'data': data,
            'type': (type(value).__name__ if has_v else None),
            'exc': exc}


def _main():
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
        except Exception:
            sys.stdout.write(json.dumps({'ok': False, 'exc': 'bad request'}) + '\n')
            sys.stdout.flush()
            continue
        res = {'ok': True, 'pong': True} if req.get('op') == 'ping' else _run(req.get('code', ''))
        sys.stdout.write(json.dumps(res) + '\n')
        sys.stdout.flush()


_main()
"""

STDERR_TAIL_LINES = 40
"""How many of the child's last stderr lines a failed start hands back."""

PING_TIMEOUT_S = 5.0
DEFAULT_EVAL_TIMEOUT_S = 30.0

_REPLS: dict[str, _Repl] = {}
_REPLS_LOCK = threading.Lock()


def _text(value) -> str:
    return "" if value is None else str(value)


def _abbreviate_home(path: str) -> str:
    home = os.path.expanduser("~")
    if home and path.startswith(home):
        return "~" + path[len(home) :]
    return path


def _project_dir(options) -> str:
    """The canonical directory this call names. Vis resolves `cwd` before the
    call; a direct caller gets the process directory."""
    raw = options.get("cwd") if isinstance(options, dict) else None
    return os.path.realpath(os.path.expanduser(_text(raw) or os.getcwd()))


def _repl_id(options, cwd: str) -> str:
    for key in ("id", "repl_id"):
        value = options.get(key) if isinstance(options, dict) else None
        if isinstance(value, str) and value.strip():
            return value.strip()
    return "pyrepl:" + cwd


def _venv_python(root: Path) -> str | None:
    """ABSOLUTE path of a project-local virtualenv's interpreter, or None.

    Never RESOLVED: `.venv/bin/python3` is a symlink chain ending at the base
    installation, and following it walks out of the virtualenv — `sys.prefix`
    becomes the base prefix, `pyvenv.cfg` is never read, and the run dies with
    `No module named pytest` while the same suite passes under `.venv/bin/python`.
    """
    for venv in (".venv", "venv"):
        for name in ("bin/python", "bin/python3"):
            candidate = root / venv / name
            if candidate.is_file():
                return str(candidate)
    return None


def _is_uv_project(root: Path) -> bool:
    """A `uv.lock`, or a real `[tool.uv]` table in `pyproject.toml` — read as
    TOML, never as substring soup: `[tool.uvicorn]`, a commented-out `[tool.uv]`
    and a description that merely mentions one are not uv projects, and picking
    `uv run python` for them launches the wrong interpreter."""
    if (root / "uv.lock").is_file():
        return True
    pyproject = root / "pyproject.toml"
    if not pyproject.is_file():
        return False
    try:
        import tomllib

        tool = tomllib.loads(pyproject.read_text(encoding="utf-8")).get("tool")
    except Exception:
        return False
    return isinstance(tool, dict) and isinstance(tool.get("uv"), dict)


def detect_command(cwd: str) -> list[str]:
    """The argv PREFIX that launches a project-aware Python in `cwd`, first hit
    winning: uv (`uv.lock` / `[tool.uv]` plus `uv` on PATH), Poetry
    (`poetry.lock` plus `poetry` on PATH), a project virtualenv, then the
    system interpreter."""
    root = Path(cwd)
    if _is_uv_project(root) and shutil.which("uv"):
        return ["uv", "run", "python"]
    if (root / "poetry.lock").is_file() and shutil.which("poetry"):
        return ["poetry", "run", "python"]
    venv = _venv_python(root)
    if venv:
        return [venv]
    return ["python3" if shutil.which("python3") else "python"]


def _child_environment(values) -> dict[str, str]:
    """This worker's environment — already the project's, `.env` and
    `environment:` declarations included — with one call's delta applied. A
    name mapped to None is UNSET for the child."""
    environment = dict(os.environ)
    for name, value in (values or {}).items():
        if value is None:
            environment.pop(str(name), None)
        else:
            environment[str(name)] = str(value)
    return environment


class ReplError(RuntimeError):
    """A REPL call that cannot be served — down, timed out, or dead."""


class _Repl:
    """One interpreter child and the framed conversation with it."""

    def __init__(self, process, cwd: str, cmd: list[str], env_fingerprint):
        self.process = process
        self.cwd = cwd
        self.cmd = cmd
        self.env_fingerprint = dict(env_fingerprint or {})
        self.started_at = time.time()
        self._lock = threading.Lock()
        self._answers: queue.Queue = queue.Queue()
        self._stderr: collections.deque = collections.deque(maxlen=STDERR_TAIL_LINES)
        self._pump("vis-repl-stdout", process.stdout, self._answers.put, sentinel=True)
        # Nothing else reads the child's stderr: left unread the pipe fills and
        # the child BLOCKS mid-write, and once the process is reaped the stream
        # is closed and a failed start loses exactly the words explaining it.
        self._pump("vis-repl-stderr", process.stderr, self._stderr.append)

    def _pump(self, name, stream, sink, sentinel=False):
        def run():
            try:
                for line in stream:
                    sink(line.rstrip("\n"))
            except Exception:
                pass
            finally:
                if sentinel:
                    sink(None)

        thread = threading.Thread(target=run, name=name, daemon=True)
        thread.start()
        return thread

    @property
    def pid(self) -> int:
        return self.process.pid

    def is_alive(self) -> bool:
        return self.process.poll() is None

    def stderr_tail(self) -> list[str]:
        # Reached only after the child was reaped, so EOF is already on its way.
        # The pause is for the loaded machine that has not scheduled the pump yet:
        # without it a dead launch can answer with no tail at all, losing the one
        # line that explains it.
        deadline = time.time() + 0.5
        while not self._stderr and time.time() < deadline:
            time.sleep(0.01)
        return [line for line in self._stderr if line.strip()]

    def displayed_cmd(self) -> list[str]:
        # The inlined driver source is elided: this argv rides into `status`,
        # the resource registry and the footer.
        return [*self.cmd[:-1], "<vis python driver>"]

    def request(self, payload: dict, timeout_s: float) -> dict:
        with self._lock:
            if not self.is_alive():
                raise ReplError(
                    "Python REPL closed the connection; start it again with "
                    'repl_start("python").'
                )
            try:
                self.process.stdin.write(json.dumps(payload) + "\n")
                self.process.stdin.flush()
            except (BrokenPipeError, ValueError, OSError) as ex:
                self.kill()
                raise ReplError(
                    "Python REPL closed the connection; start it again with "
                    f'repl_start("python"). ({ex})'
                ) from ex
            try:
                line = self._answers.get(timeout=timeout_s)
            except queue.Empty:
                raise ReplError(
                    f"Python eval timed out after {int(timeout_s * 1000)}ms"
                ) from None
            if line is None:
                exit_code = self.process.poll()
                self.kill()
                raise ReplError(
                    "Python REPL closed the connection; start it again with "
                    f'repl_start("python"). (exit {exit_code}) '
                    + " ".join(self.stderr_tail()[-3:])
                )
            try:
                return json.loads(line)
            except ValueError as ex:
                self.kill()
                raise ReplError(
                    "Python REPL returned an invalid response and is dead; start it "
                    'again with repl_start("python").'
                ) from ex

    def kill(self) -> None:
        for step in (self.process.terminate, self.process.kill):
            if self.process.poll() is not None:
                break
            try:
                step()
                self.process.wait(timeout=2)
            except Exception:
                pass
        for stream in (self.process.stdin, self.process.stdout, self.process.stderr):
            try:
                stream.close()
            except Exception:
                pass


def _live(cwd: str) -> _Repl | None:
    with _REPLS_LOCK:
        repl = _REPLS.get(cwd)
        if repl is not None and not repl.is_alive():
            _REPLS.pop(cwd, None)
            return None
        return repl


def _forget(cwd: str) -> _Repl | None:
    with _REPLS_LOCK:
        return _REPLS.pop(cwd, None)


def _status(cwd: str, repl: _Repl | None) -> dict:
    """The lifecycle view every language answers: a key appears only where it
    MEANS something, so a REPL that is down has no pid and no command."""
    answer = {"result": "status", "cwd": cwd, "status": "up" if repl else "down"}
    if repl:
        answer["running"] = True
        answer["pid"] = repl.pid
        answer["cmd"] = repl.displayed_cmd()
        if repl.env_fingerprint:
            # By NAME and digest only — never a value: this rides into a result,
            # a log and the transcript.
            answer["env"] = dict(repl.env_fingerprint)
    return answer


def status(options=None) -> dict:
    options = options or {}
    cwd = _project_dir(options)
    answer = _status(cwd, _live(cwd))
    answer["id"] = _repl_id(options, cwd)
    return answer


def start(options=None) -> dict:
    """Start this worker's Python REPL for `cwd`, or reuse the live one.

    A live REPL is NEVER silently replaced: its globals ARE the session's work,
    so a second start answers `already-running`. A start succeeds only after the
    child answers its protocol ping; a failed child never masquerades as a
    usable REPL.
    """
    options = options or {}
    cwd = _project_dir(options)
    repl_id = _repl_id(options, cwd)
    live = _live(cwd)
    if live:
        answer = _status(cwd, live)
        answer["result"] = "already-running"
        answer["id"] = repl_id
        return answer

    cmd = [*detect_command(cwd), "-c", DRIVER]
    process = subprocess.Popen(
        cmd,
        cwd=cwd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=_child_environment(options.get("env")),
        text=True,
        encoding="utf-8",
        errors="replace",
        bufsize=1,
    )
    repl = _Repl(process, cwd, cmd, options.get("env_fingerprint"))
    try:
        pong = repl.request({"op": "ping"}, PING_TIMEOUT_S)
        if pong.get("pong") is not True:
            raise ReplError("Python REPL did not acknowledge its startup ping")
    except Exception as ex:
        repl.kill()
        tail = repl.stderr_tail()
        answer = {
            "result": "failed",
            "status": "failed",
            "id": repl_id,
            "pid": repl.pid,
            "cmd": repl.displayed_cmd(),
            "cwd": cwd,
            "message": f"Python REPL failed its startup handshake: {ex}",
        }
        if process.poll() is not None:
            answer["exit"] = process.poll()
        if tail:
            answer["log_tail"] = tail
        return answer

    with _REPLS_LOCK:
        _REPLS[cwd] = repl
    answer = _status(cwd, repl)
    answer["result"] = "started"
    answer["id"] = repl_id
    return answer


def stop(options=None) -> dict:
    """Stop this worker's interpreter for `cwd`. No-op-safe: with nothing
    managed the result says `not-managed`, not a stop that never happened."""
    options = options or {}
    cwd = _project_dir(options)
    repl = _forget(cwd)
    if repl:
        repl.kill()
    return {
        "result": "stopped" if repl else "not-managed",
        "cwd": cwd,
        "status": "down",
        "id": _repl_id(options, cwd),
    }


def evaluate(options) -> dict:
    """Evaluate `code` in this worker's REPL for `cwd`, with globals persistent
    across calls. Answers `{ok, out, err, value, data, type, exc}` — `value` is
    the last expression's repr, `data` its JSON-safe structured view, `type` the
    class name — plus the `code` that ran, which the REPL op-card shows."""
    options = {"code": options} if isinstance(options, str) else (options or {})
    code = options.get("code")
    if code is None:
        code = options.get("source")
    if code is None:
        raise ValueError('repl_eval(python) expects a code string or {"code": ...}')
    cwd = _project_dir(options)
    repl = _live(cwd)
    if repl is None:
        shown = _abbreviate_home(cwd)
        raise ReplError(
            f"Python REPL is not up for {shown}; "
            f'call repl_start("python", {{"cwd": "{shown}"}}) first'
        )
    timeout_ms = options.get("timeout_ms")
    timeout_s = (float(timeout_ms) / 1000.0) if timeout_ms else DEFAULT_EVAL_TIMEOUT_S
    answer = repl.request({"code": _text(code)}, timeout_s)
    answer["code"] = _text(code)
    return answer


def repl_start(op, options=None) -> dict:
    """The REPL lifecycle for Python: `start`, `status` and `stop`.

    There is no `connect` and no restart — Vis owns the interpreter process, and
    a REPL is stopped and started rather than replaced underneath the work
    standing on it.
    """
    op = op if isinstance(op, str) else "status"
    if op == "status":
        return status(options)
    if op == "stop":
        return stop(options)
    if op == "start":
        return start(options)
    raise ValueError(
        f"python REPL lifecycle: unknown op {op!r} — the verbs are repl_start / "
        "repl_status / repl_stop; there is no repl_connect for Python, Vis owns "
        "the interpreter process."
    )


def repl_eval(options) -> dict:
    """`repl_eval` for Python."""
    return evaluate(options)
