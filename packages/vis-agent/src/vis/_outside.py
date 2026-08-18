"""The host the `vis` module finds when there is no Vis in the room.

`vis/__init__.py` is the extension API and nothing else: every call it makes goes
through `_host`, the dict of callables the engine seeds before it execs the module
inside an extension context. Installed from PyPI there is no engine, so this
module answers the same thirteen names — and it answers them the way the
`vis-contract` package says each one behaves outside the sandbox, never by guessing:
  outside == "local"   the op has an honest local meaning, so do it locally:
                       state is a JSON file, `log`/`notify` are stderr lines,
                       `shell` is a real subprocess, secrets live in a vault that
                       dies with the process.
  outside == "prompt"  there is no dialog surface, so ASK THE TERMINAL. `vis.ask`
                       prints the same field tree and collects the same values,
                       runs the extension's own validators, and hands back the
                       same answer shape a mounted surface would.
  outside == "refuse"  nothing out here can enforce what the op promises (a jail
                       is a property of the agent's process boundary), so the op
                       raises the refusal the CONTRACT states, by name.

The contract is the document, this file is one implementation of it: an op added
to `packages/vis-contract/resources/vis-contract/python-host.edn` and left
unimplemented here fails at import with the op named, rather than at the call site
inside somebody's extension. That is the whole point of keeping the two apart.

Priming an answer instead of typing it (unit tests, CI):

    import vis
    vis.outside.answer_with({"env": "staging", "token": "hunter2"})
    assert vis.ask("Deploy", [vis.select("env", ["staging", "prod"])])["env"] == "staging"

`VIS_OUTSIDE_ANSWERS` (a JSON object) does the same from the environment, and
`VIS_OUTSIDE_NONINTERACTIVE=1` makes every ask answer `undeliverable` the way a
session with no surface mounted does.
"""

from __future__ import annotations

import json
import os
import secrets
import subprocess
import sys
import textwrap
import threading
import time
import uuid
from pathlib import Path

import vis_contract

__all__ = ["Refused", "answer_with", "contract", "host", "state_home"]


# -- The contract -------------------------------------------------------------


# The DECLARATION is its own package (`pip install vis-contract`), so this host and
# the engine that seeds the real one read one document. Nothing is transcribed
# here: every op name, every refusal and the whole shell grammar come from it.
contract = vis_contract.CONTRACT
_OPS = vis_contract.OPS
_HUMAN = vis_contract.HUMAN_INPUT


class Refused(RuntimeError):
    """An op the contract refuses to serve outside a Vis process."""


def _refusal(name):
    op = _OPS[name]

    def refuse(*_args, **_kwargs):
        raise Refused(op["refusal"])

    refuse.__name__ = name
    refuse.__doc__ = op["summary"]
    return refuse


# -- Where the outside host keeps things --------------------------------------


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


def notify(text, level="info"):
    print(f"[vis notify/{str(level).lower()}] {text}", file=sys.stderr, flush=True)
    return None


# -- Secrets ------------------------------------------------------------------

_VAULT = {}
_PREFIX = _HUMAN["secret_handle_prefix"]


def _stash(plaintext):
    handle = _PREFIX + secrets.token_hex(8)
    _VAULT[handle] = plaintext
    return handle


def reveal_secret(handle):
    return _VAULT.get(str(handle))


def forget_secret(handle):
    return _VAULT.pop(str(handle), None) is not None


# -- Environment --------------------------------------------------------------


def declare_env(names_json):
    # The engine resolves declared names through the operator's configured
    # secrets; out here the process environment IS the configuration.
    names = json.loads(names_json) or []
    return json.dumps({str(n): os.environ.get(str(n)) for n in names})


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
    "stdout",
    "stdout_omitted_chars",
    "offset",
    "next_offset",
    "is_eof",
    "attach",
    "already_running",
    "keys",
    "note",
)

_RUNS = {}


class _Run:
    def __init__(self, command, cwd, timeout_secs, env):
        self.id = "sh-" + secrets.token_hex(4)
        self.command = command
        self.cwd = str(Path(cwd).expanduser()) if cwd else os.getcwd()
        self.timeout_secs = timeout_secs
        self.log_path = str(state_home() / (f"shell-{self.id}.log"))
        self.started_at = time.time()
        self.finished_at = None
        self.timed_out = False
        self.stopped = False
        self._sink = open(self.log_path, "wb")
        self.process = subprocess.Popen(
            command
            if isinstance(command, (list, tuple))
            else ["/bin/sh", "-c", command],
            cwd=self.cwd,
            env=env,
            stdin=subprocess.PIPE,
            stdout=self._sink,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        )

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
    result = dict.fromkeys(_SHELL_RESULT_KEYS)
    stdout, next_offset, omitted = run.read(
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
            "stdout": stdout,
            "stdout_omitted_chars": omitted,
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


_SHELL = contract["shell"]
_SPAWN_OPS = tuple(_SHELL["spawn_ops"])
_HANDLE_OPS = tuple(_SHELL["handle_ops"])


def _shell_vocabulary():
    """The ops this host answers, worded the way the engine words its own refusal."""
    names = [
        '"{}"{}'.format(op, " (default)" if op == _SHELL["default_op"] else "")
        for op in _SPAWN_OPS + _HANDLE_OPS
    ]
    return ", ".join(names[:-1]) + " or " + names[-1]


def shell(opts):
    """Start a process, or drive one this host already started.

    The op vocabulary is the CONTRACT's, not this file's: `vis_contract.SHELL`
    names which ops spawn and which drive a handle, so an extension written
    against the engine's `{"op": "run", …}` means the same thing out here.
    """
    opts = dict(opts or {})
    op = str(opts.get("op") or _SHELL["default_op"]).strip()
    if op in _SPAWN_OPS:
        run = _Run(
            opts.get("command"),
            opts.get("cwd"),
            opts.get("timeout_secs"),
            opts.get("env"),
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

_GROUP = _HUMAN["group_type"]
_DECOR = set(_HUMAN["decor_types"])
_FIELDS = set(_HUMAN["field_types"])
_TEXT = set(_HUMAN["text_types"])
_CHOICE = set(_HUMAN["choice_types"])
_SECRET = set(_HUMAN["secret_types"])

_PRIMED = {}


def answer_with(values):
    """Answer the NEXT asks from `values` instead of prompting.

    A test drives an extension end to end this way; anything the mapping does not
    name is still typed by whoever is at the terminal. `answer_with({})` clears it.
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
            yield from _field_nodes(node.get("fields"))
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
        low = node.get("min", _HUMAN["range"]["min"])
        high = node.get("max", _HUMAN["range"]["max"])
        step = node.get("step", _HUMAN["range"]["step"])
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

        boxes = (
            node.get("max_length") or node.get("min_length") or _HUMAN["otp"]["length"]
        )
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
    nodes = list(_field_nodes(request.get("fields")))
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
        children = node.get("fields")
        if not isinstance(children, (list, tuple)) or not children:
            return f"a {_GROUP} must arrange at least one field"
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
        boxes = (
            node.get("max_length") or node.get("min_length") or _HUMAN["otp"]["length"]
        )
        if int(boxes) > int(_HUMAN["otp"]["ceiling"]):
            return "{!r} asks for {} boxes, more than the {} a dialog fits".format(
                name,
                boxes,
                _HUMAN["otp"]["ceiling"],
            )
    if kind == "range":
        low = node.get("min", _HUMAN["range"]["min"])
        high = node.get("max", _HUMAN["range"]["max"])
        step = node.get("step", _HUMAN["range"]["step"])
        if low >= high:
            return f"{name!r} has min {low} and max {high}: a track needs room"
        if step <= 0:
            return f"{name!r} has step {step}: a track advances"
    return None


def _check_request(request):
    if not isinstance(request, dict):
        return "a request is a map of title and fields"
    title = request.get("title")
    if not isinstance(title, str) or not title.strip():
        return "a request needs a title"
    fields = request.get("fields")
    if not isinstance(fields, (list, tuple)) or not fields:
        return "a request needs at least one field"
    seen = set()
    for node in fields:
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
# same mechanical rules the engine applies (`set` merges its keys onto the node,
# `append` upserts an item by its id and concatenates log lines, `clear` empties,
# `remove` drops ids), so an extension polling its own view reads the truth
# either side of the boundary.
_LIVE = _HUMAN["live"]
_LIVE_HOST = contract["live"]
_LIVE_ITEMS = {
    "stat": "stats",
    "steps": "steps",
    "table": "rows",
    "link": "links",
    "log": "lines",
}
_LIVE_VIEWS = {}


def _live_check_node(node, seen):
    if not isinstance(node, dict):
        return f"every node must be a map, got {node!r}"
    kind = node.get("type")
    is_group = kind == _GROUP
    if is_group:
        # Layout is the FORM's own vocabulary: a view arranges its nodes with the
        # same row and column a question does, and the group paints nothing itself.
        direction = node.get("direction")
        if direction is not None and direction not in _HUMAN["group_directions"]:
            ways = ", ".join(_HUMAN["group_directions"])
            return f"a {_GROUP} runs one of {ways}, got {direction!r}"
        children = node.get("fields")
        if not isinstance(children, (list, tuple)) or not children:
            return f"a {_GROUP} must arrange at least one node"
    elif kind not in _LIVE["node_types"]:
        types = ", ".join(_LIVE["node_types"])
        return f"a node is one of {types}, got {kind!r}"
    node_id = node.get("id")
    if not isinstance(node_id, str) or not node_id.strip():
        return f"a {kind} node needs an id"
    if node_id in seen:
        return f"two nodes answer to {node_id!r}"
    seen.add(node_id)
    if is_group:
        for child in node["fields"]:
            complaint = _live_check_node(child, seen)
            if complaint:
                return complaint
    return None


def _check_view(view):
    if not isinstance(view, dict):
        return "a live view is a map of title and nodes"
    title = view.get("title")
    if not isinstance(title, str) or not title.strip():
        return "a live view needs a title"
    nodes = view.get("nodes")
    if not isinstance(nodes, (list, tuple)) or not nodes:
        return "a live view needs at least one node"
    seen = set()
    for node in nodes:
        complaint = _live_check_node(node, seen)
        if complaint:
            return complaint
    # The bound counts the TREE: a row holding twenty nodes is twenty nodes.
    if len(seen) > _LIVE["max_nodes"]:
        return "a live view holds at most {} nodes".format(_LIVE["max_nodes"])
    return None


def _live_ids(nodes):
    # Every id in the tree: a node added anywhere may not shadow one already here.
    seen = set()
    for node in nodes or []:
        node_id = node.get("id")
        if node_id is not None:
            seen.add(node_id)
        seen |= _live_ids(node.get("fields"))
    return seen


def _live_leaves(nodes):
    # The nodes that PAINT, groups flattened: what a reader reads is CONTENT, and a
    # row is only how a surface arranged it.
    leaves = []
    for node in nodes or []:
        if node.get("type") == _GROUP:
            leaves.extend(_live_leaves(node.get("fields")))
        else:
            leaves.append(node)
    return leaves


def _live_find(nodes, node_id):
    # The list a node lives in and its index there, ANYWHERE in the tree: an op
    # names a node by id and never says which row is holding it.
    for index, node in enumerate(nodes or []):
        if node.get("id") == node_id:
            return nodes, index
        children = node.get("fields")
        if isinstance(children, list):
            found = _live_find(children, node_id)
            if found is not None:
                return found
    return None


def _live_upsert(held, incoming):
    # An item keeps the position it was first given: a row that updates in place
    # is what makes a table readable while it fills.
    merged = list(held or [])
    for item in incoming or []:
        item_id = item.get("id") if isinstance(item, dict) else None
        at = None
        if item_id is not None:
            at = next(
                (i for i, one in enumerate(merged) if one.get("id") == item_id), None
            )
        if at is None:
            merged.append(item)
        else:
            merged[at] = {**merged[at], **item}
    return merged


def _live_bound(node):
    # The record outside is as long as the contract lets a surface hold: a log
    # keeps its window, a table its rows, so a day-long loop cannot grow this
    # process without a bound.
    if node.get("type") == "log":
        window = int(node.get("window_lines") or _LIVE["log"]["window_lines"])
        node["lines"] = list(node.get("lines") or [])[-window:]
    elif node.get("type") == "table":
        rows = int(node.get("max_rows") or _LIVE["table"]["max_rows"])
        node["rows"] = list(node.get("rows") or [])[-rows:]
    return node


def _live_items_key(node, name):
    key = _LIVE_ITEMS.get(node.get("type"))
    if key is None:
        kind = node.get("type")
        raise Refused(f"a {kind} node holds no items, so it takes no {name}")
    return key


def _live_apply(view, op):
    """Fold one op into `view` and answer the line the transcript owes it."""
    name = op.get("op") if isinstance(op, dict) else None
    if name not in _LIVE["ops"]:
        names = ", ".join(_LIVE["ops"])
        raise Refused(f"a live op is one of {names}, got {name!r}")
    if name == "add-node":
        spec = op.get("node_spec")
        complaint = _live_check_node(spec, _live_ids(view["nodes"]))
        if complaint:
            raise Refused(complaint)
        found = _live_find(view["nodes"], op.get("after"))
        # A new node joins the row that holds the node it named, not the top of
        # the view: `after` is a sibling, so the arrangement stays the one declared.
        siblings, at = (
            (view["nodes"], len(view["nodes"])) if found is None else (found[0], found[1] + 1)
        )
        siblings.insert(at, json.loads(json.dumps(spec)))
        return "+ {}".format(spec.get("label") or spec.get("id"))
    found = _live_find(view["nodes"], op.get("node_id"))
    if found is None:
        raise Refused("no node {!r} in this view".format(op.get("node_id")))
    siblings, index = found
    node = dict(siblings[index])
    if name == "remove-node":
        # A group leaves with its children: nothing outlives the row it stood in.
        siblings.pop(index)
        return "- {}".format(node.get("label") or node.get("id"))
    if name == "set":
        node.update({k: v for k, v in op.items() if k not in ("op", "node_id")})
    elif name == "append":
        key = _live_items_key(node, "lines to append")
        payload = list(op.get(key) or [])
        if key == "lines":
            node[key] = list(node.get(key) or []) + payload
        else:
            node[key] = _live_upsert(node.get(key), payload)
    elif name == "clear":
        node[_live_items_key(node, "clear")] = []
    elif name == "remove":
        key = _live_items_key(node, "item_ids")
        dropped = set(op.get("item_ids") or [])
        node[key] = [i for i in (node.get(key) or []) if i.get("id") not in dropped]
    siblings[index] = _live_bound(node)
    return _live_line(name, op, node)


def _live_line(name, op, node):
    label = node.get("label") or node.get("id")
    kind = node.get("type")
    if name == "clear":
        return f"{label}: cleared"
    if name == "remove":
        return "{}: -{}".format(label, len(op.get("item_ids") or []))
    if name == "append" and kind == "log":
        return "\n".join(str(line) for line in (op.get("lines") or []))
    if name == "append":
        key = _LIVE_ITEMS[kind]
        return f"{label}: +{len(op.get(key) or [])} {key}"
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
    if reason not in _LIVE["reasons"]:
        reasons = ", ".join(_LIVE["reasons"])
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


def live(envelope_json):
    """`vis.live` with no pane: stderr is the surface, and the record with it."""
    envelope = json.loads(envelope_json)
    if not isinstance(envelope, dict):
        raise Refused("a live envelope must be a JSON object")
    op = str(envelope.get("op") or _LIVE_HOST["default_op"])
    if op in _LIVE_HOST["spawn_ops"]:
        view = envelope.get("view")
        complaint = _check_view(view)
        if complaint:
            raise Refused(complaint)
        view_id = str(uuid.uuid4())
        held = {"view_id": view_id, "view": json.loads(json.dumps(view)), "seq": 0}
        held["view"]["id"] = view_id
        _LIVE_VIEWS[view_id] = held
        print("\n== {} ==".format(view.get("title")), file=sys.stderr)
        if view.get("description"):
            print(textwrap.fill(str(view["description"]), 78), file=sys.stderr)
        return json.dumps({"view_id": view_id, "is_open": True, "view": held["view"]})
    if op not in _LIVE_HOST["handle_ops"]:
        ops = ", ".join(list(_LIVE_HOST["spawn_ops"]) + list(_LIVE_HOST["handle_ops"]))
        raise Refused(f"a live op is one of {ops}, got {op!r}")
    view_id = str(envelope.get("view_id") or "")
    held = _LIVE_VIEWS.get(view_id)
    if held is None:
        raise Refused(
            f"no live view {view_id} is open — it was closed, interrupted, or never opened"
        )
    if held.get("result"):
        # The engine answers an ended view from its record rather than refusing;
        # the loop pushing into it learns WHY it stopped, in one shape.
        return json.dumps(
            {"view_id": view_id, "is_open": False, "result": held["result"]}
        )
    if op == "state":
        return json.dumps({"view_id": view_id, "is_open": True, "view": held["view"]})
    if op == "patch":
        ops = (envelope.get("patch") or {}).get("ops") or []
        _live_say([_live_apply(held["view"], one) for one in ops])
        held["seq"] += 1
        return json.dumps({"view_id": view_id, "is_open": True, "seq": held["seq"]})
    verdict = _live_verdict(held, envelope.get("ending") or {})
    held["result"] = verdict
    ending = verdict.get("summary") or verdict.get("error") or ""
    print(
        "== {} · {}{} ==".format(
            held["view"].get("title"),
            verdict["reason"],
            f" — {ending}" if ending else "",
        ),
        file=sys.stderr,
    )
    return json.dumps({"view_id": view_id, "is_open": False, "result": verdict})


# -- The host itself ----------------------------------------------------------

_IMPLEMENTATIONS = {
    "state_get": state_get,
    "state_put": state_put,
    "state_del": state_del,
    "state_keys": state_keys,
    "log": log,
    "notify": notify,
    "shell": shell,
    "request_input": request_input,
    "live": live,
    "reveal_secret": reveal_secret,
    "forget_secret": forget_secret,
    "declare_env": declare_env,
}


class _OutsideHost:
    """The host `vis` binds when there is no engine in the room.

    One attribute per contract op, because that is the shape the engine injects
    too: an extension holds a `vis_contract.Host` either way, and anyone writing a
    third host has an interface to implement rather than a dict shape to guess.
    """

    def __init__(self, ops):
        for name, fn in ops.items():
            setattr(self, name, fn)

    def __repr__(self):
        return "<vis outside host: contract v{}, {} ops>".format(
            contract["version"], len(_OPS)
        )


def _build_host():
    # The DOCUMENT decides what exists; this file only claims to implement it, and
    # `check_host` is the contract's own gate: an op declared with nothing behind it
    # fails here, at import, naming itself — not in front of a user halfway through
    # an extension.
    built = {}
    for op in contract["ops"]:
        name = op["name"]
        if op["outside"] == "refuse":
            built[name] = _refusal(name)
        elif name in _IMPLEMENTATIONS:
            built[name] = _IMPLEMENTATIONS[name]
    return vis_contract.check_host(_OutsideHost(built))


host = _build_host()
