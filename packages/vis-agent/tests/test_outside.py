"""The SDK outside Vis implements the Host protocol and schema-backed payloads."""

import inspect
import json
import os
import sys
import threading
import time
from datetime import UTC, datetime
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts, _outside

HOST_METHODS = {
    name: method
    for name, method in vars(vis.Host).items()
    if inspect.isfunction(method) and not name.startswith("_")
}


@pytest.fixture(autouse=True)
def stop_log_sweepers():
    yield
    sweepers = getattr(_outside, "_LOG_SWEEPERS", {})
    for stop, thread in list(sweepers.values()):
        stop.set()
        thread.join(5)
        assert not thread.is_alive()
    sweepers.clear()


# -- The protocol and the implementation -------------------------------------


def test_the_host_is_a_contract_host_and_serves_exactly_the_declared_ops():
    # The protocol is the interface anyone else implements, so this host has to
    # satisfy it the same way a stranger's would.
    assert isinstance(_outside.host, vis.Host)
    served = sorted(n for n in vars(_outside.host) if not n.startswith("_"))
    assert served == sorted(HOST_METHODS)


def test_every_op_accepts_the_arity_the_protocol_declares():
    for name, method in HOST_METHODS.items():
        fn = getattr(_outside.host, name)
        arity = len(inspect.signature(method).parameters) - 1
        params = [
            p
            for p in inspect.signature(fn).parameters.values()
            if p.kind in (p.POSITIONAL_ONLY, p.POSITIONAL_OR_KEYWORD)
        ]
        required = [p for p in params if p.default is p.empty]
        assert len(required) <= arity <= len(params), name


class _Recorder:
    """A host somebody else could have written: the contract's ops, nothing else."""

    def __init__(self):
        self.calls = []
        for name in HOST_METHODS:
            setattr(self, name, self._record(name))

    def _record(self, name):
        def op(*args):
            self.calls.append((name, args))
            return None

        return op


def test_any_object_that_satisfies_the_protocol_can_be_the_host(monkeypatch):
    # Three hosts exist now — the engine's, `_outside`'s and this one — and the
    # module cannot tell them apart, because the protocol is the only agreement
    # between them. That is what makes a third host somebody else's to write.
    stranger = _Recorder()
    assert isinstance(stranger, vis.Host)
    assert _outside.check_host(stranger) is stranger

    monkeypatch.setattr(vis, "_host", stranger)
    vis.log("info", "hello")
    vis.notify("done")
    vis.state["seat"] = 4
    # This host records instead of storing, so the read that guards a delete finds
    # nothing — and a mapping owes its caller a KeyError for that.
    with pytest.raises(KeyError):
        del vis.state["seat"]
    vis.reveal("vis-secret:abc")

    assert [name for name, _ in stranger.calls] == [
        "log",
        "notify",
        "state_put",
        "state_get",
        "reveal_secret",
    ]


def test_workspace_root_tracks_outside_process_directory(tmp_path, monkeypatch):
    # Regression, #280: the public SDK must supply the same working root to extensions.
    monkeypatch.chdir(tmp_path)
    assert vis.workspace_root() == tmp_path
    assert isinstance(vis.workspace_root(), Path)


@pytest.mark.parametrize("name", ["jailed_shell", "council_wake"])
def test_session_bound_operations_refuse_without_the_host(name):
    with pytest.raises(_outside.Refused, match=f"vis\\.{name}"):
        getattr(vis._host, name)({"command": "true"})


# -- State, logs, secrets, environment -----------------------------------------


def test_state_survives_the_call_that_wrote_it(outside_home):
    vis.state["deploy"] = {"env": "staging", "count": 2}
    assert vis.state["deploy"] == {"env": "staging", "count": 2}
    assert "deploy" in vis.state
    assert json.loads((outside_home / "state.json").read_text())["deploy"]["count"] == 2
    del vis.state["deploy"]
    assert "deploy" not in vis.state
    assert vis.state.get("deploy", "gone") == "gone"


def test_state_is_a_whole_mapping(outside_home):
    # Regression: `vis.state` answered five methods, so `pop` raised
    # AttributeError and `list(vis.state)` fell through to the sequence protocol
    # and asked the host for the key `0`.
    vis.state.update({"repo": "acme/widgets", "count": 2})
    assert vis.state.setdefault("count", 99) == 2
    assert vis.state.setdefault("branch", "main") == "main"

    assert sorted(vis.state) == ["branch", "count", "repo"]
    assert len(vis.state) == 3
    assert sorted(vis.state.keys()) == ["branch", "count", "repo"]
    assert ("count", 2) in vis.state.items()
    assert sorted(vis.state.values(), key=str) == [2, "acme/widgets", "main"]
    assert dict(vis.state) == {"repo": "acme/widgets", "count": 2, "branch": "main"}
    assert vis.state == {"repo": "acme/widgets", "count": 2, "branch": "main"}

    assert vis.state.pop("count") == 2
    assert vis.state.pop("count", "gone") == "gone"
    with pytest.raises(KeyError):
        vis.state.pop("count")
    with pytest.raises(KeyError):
        del vis.state["count"]

    vis.state.clear()
    assert dict(vis.state) == {}
    assert json.loads((outside_home / "state.json").read_text()) == {}


def test_a_key_written_as_null_is_no_key_at_all():
    # JSON null and "never written" are one value at this boundary, so a mapping
    # that listed the key would then refuse to hand it back.
    vis.state["ghost"] = None
    assert "ghost" not in vis.state
    assert list(vis.state) == []
    assert vis.state.get("ghost", "gone") == "gone"


def test_a_secret_is_a_handle_until_it_is_revealed():
    handle = _outside._stash("hunter2")
    assert _contracts.validate("view", "secret_handle", handle) == handle
    assert "hunter2" not in handle
    assert vis.reveal(handle) == "hunter2"
    assert vis.forget(handle) is True
    assert vis.reveal(handle) is None


def test_host_env_reads_the_process_environment(monkeypatch):
    monkeypatch.setenv("VIS_TEST_TOKEN", "from-the-environment")
    assert vis.host_env("VIS_TEST_TOKEN") == "from-the-environment"
    assert vis.host_env("VIS_TEST_ABSENT", "fallback") == "fallback"


def test_log_and_notify_go_to_stderr(capsys):
    vis.log("info", "a line")
    vis.notify("something happened")
    captured = capsys.readouterr()
    assert captured.out == ""
    assert "a line" in captured.err
    assert "something happened" in captured.err


# -- Shell ---------------------------------------------------------------------


@pytest.mark.parametrize("with_outside", [False, True])
def test_log_sweep_preserves_fresh_empty_directories(tmp_path, with_outside):
    log_root = tmp_path / "logs"
    fresh = log_root / "2026-09-14"
    stale = log_root / "2026-09-13"
    fresh.mkdir(parents=True)
    stale.mkdir()
    if with_outside:
        (fresh / "outside").mkdir()
        (stale / "outside").mkdir()
        os.utime(stale / "outside", (0, 0))
    os.utime(stale, (0, 0))

    # A writer may be between mkdir and opening its log during a periodic sweep.
    _outside._sweep_logs(log_root)

    assert fresh.is_dir()
    if with_outside:
        assert (fresh / "outside").is_dir()
    assert not stale.exists()


def test_shell_runs_a_command_and_answers_the_engine_shape():
    run = vis.shell({"command": "printf hello"}).wait(10)
    assert run["exit"] == 0
    assert run["out"] == "hello"
    assert run["status"] == "exited"
    assert set(run) == set(_outside._SHELL_RESULT_KEYS)
    assert run.logs()["out"] == "hello"


def _aged_log(path, days):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("diagnostic")
    stamp = time.time() - days * 86400
    os.utime(path, (stamp, stamp))
    return path


@pytest.mark.parametrize("override", [False, True])
def test_shell_sweeps_old_logs_at_start_and_periodically(
    tmp_path, monkeypatch, override
):
    home = tmp_path / "home"
    monkeypatch.setattr(Path, "home", lambda: home)
    if override:
        log_root = tmp_path / "logs"
    else:
        monkeypatch.delenv("VIS_OUTSIDE_HOME")
        log_root = home / ".vis" / "logs"
    stale = _aged_log(log_root / "2020-01-01" / "outside" / "shell-old.log", 15)
    fresh = _aged_log(log_root / "2020-01-02" / "outside" / "shell-live.log", 1)
    other = _aged_log(log_root / "2020-01-01" / "gateway-old.log", 30)
    state = _aged_log(_outside.state_home() / "state.json", 30)
    swept = threading.Event()
    original = _outside._sweep_logs

    def sweep(root):
        original(root)
        swept.set()

    monkeypatch.setattr(_outside, "_sweep_logs", sweep)
    monkeypatch.setattr(_outside, "_LOG_SWEEP_INTERVAL_SECS", 0.01)
    run = vis.shell({"command": "printf retention"}).wait(10)
    assert swept.wait(5)
    assert not stale.exists()
    assert not stale.parent.exists()
    assert fresh.exists() and other.exists() and state.exists()
    assert Path(run["log_path"]).exists()
    # No second shell is needed: the daemon removes logs created after startup.
    later = _aged_log(log_root / "2020-01-03" / "outside" / "shell-later.log", 15)
    swept.clear()
    deadline = time.monotonic() + 5
    # File unlink and parent removal are separate steps in the sweep thread.
    while later.parent.parent.exists() and time.monotonic() < deadline:
        assert swept.wait(1)
        swept.clear()
    assert not later.exists()
    assert not later.parent.parent.exists()
    sweepers = list(_outside._LOG_SWEEPERS.values())
    assert len(sweepers) == 1 and sweepers[0][1].daemon
    vis.shell({"command": "true"}).wait(10)
    assert list(_outside._LOG_SWEEPERS.values()) == sweepers


@pytest.mark.parametrize("link_kind", ["root", "date", "outside", "file", "nested"])
def test_log_sweep_never_follows_symlinks(tmp_path, link_kind):
    root = tmp_path / "logs"
    target = tmp_path / "precious"
    suffix = {
        "root": "2020-01-01/outside/shell-keep.log",
        "date": "outside/shell-keep.log",
    }.get(link_kind, "shell-keep.log")
    victim = _aged_log(target / suffix, 30)
    if link_kind == "root":
        root.symlink_to(target, target_is_directory=True)
    else:
        dated = root / "2020-01-01"
        outside = dated / "outside"
        link = {
            "date": dated,
            "outside": outside,
            "file": outside / "shell-link.log",
            "nested": outside / "nested",
        }[link_kind]
        link.parent.mkdir(parents=True, exist_ok=True)
        link.symlink_to(
            victim if link_kind == "file" else target,
            target_is_directory=link_kind != "file",
        )
    _outside._sweep_logs(root)
    assert victim.read_text() == "diagnostic"
    assert (root if link_kind == "root" else link).is_symlink()


def test_log_sweep_uses_a_fourteen_day_mtime_cutoff(tmp_path, monkeypatch):
    now = 1_800_000_000
    monkeypatch.setattr(_outside.time, "time", lambda: now)
    root = tmp_path / "logs"
    old = _aged_log(root / "2020-01-01/outside/old.log", 15)
    edge = _aged_log(root / "2020-01-01/outside/edge.log", 14)
    fresh = _aged_log(root / "2020-01-01/outside/fresh.log", 13)
    invalid = _aged_log(root / "not-a-date/outside/keep.log", 30)
    _outside._sweep_logs(root)
    assert not old.exists()
    assert edge.exists() and fresh.exists() and invalid.exists()
    monkeypatch.setattr(_outside.time, "time", lambda: now + 1)
    _outside._sweep_logs(root)
    assert not edge.exists()
    assert fresh.exists()


def test_log_sweeper_retries_io_failure_and_keeps_its_original_root(
    tmp_path, monkeypatch
):
    original_home = tmp_path / "original"
    original_home.mkdir()
    monkeypatch.chdir(original_home)
    root = original_home / "logs"
    stale = _aged_log(root / "2020-01-01/outside/old.log", 30)
    elsewhere = tmp_path / "elsewhere"
    keep = _aged_log(elsewhere / "logs/2020-01-01/outside/keep.log", 30)
    first = threading.Event()
    done = threading.Event()
    calls = []
    original = _outside._sweep_logs

    def sweep(captured):
        calls.append(captured)
        if len(calls) == 1:
            first.set()
            raise OSError("temporary failure")
        original(captured)
        done.set()

    monkeypatch.setattr(_outside, "_sweep_logs", sweep)
    monkeypatch.setattr(_outside, "_LOG_SWEEP_INTERVAL_SECS", 0.01)
    _outside._start_log_sweeper(Path("logs"))
    assert first.wait(5)
    monkeypatch.chdir(elsewhere)
    monkeypatch.setenv("VIS_OUTSIDE_HOME", str(elsewhere))
    assert done.wait(5)
    assert len(calls) >= 2 and set(calls) == {root}
    assert not stale.exists()
    assert keep.exists()


def test_log_sweeper_start_failure_does_not_break_shell(monkeypatch):
    def fail_start(_thread):
        raise RuntimeError("cannot start thread")

    with monkeypatch.context() as patched:
        patched.setattr(_outside.threading.Thread, "start", fail_start)
        run = vis.shell({"command": "printf survived"}).wait(10)
        assert run["exit"] == 0 and run["out"] == "survived"
        assert not _outside._LOG_SWEEPERS
    vis.shell({"command": "true"}).wait(10)
    assert len(_outside._LOG_SWEEPERS) == 1


@pytest.mark.parametrize("override", [False, True])
def test_shell_logs_use_the_utc_date_and_keep_state_separate(
    tmp_path, monkeypatch, override
):
    home = tmp_path / "home"
    monkeypatch.setattr(Path, "home", lambda: home)
    if override:
        log_root = tmp_path / "logs"
        state_root = tmp_path
    else:
        monkeypatch.delenv("VIS_OUTSIDE_HOME")
        log_root = home / ".vis" / "logs"
        state_root = home / ".vis" / "outside"
    started = datetime.fromisoformat("2026-09-14T00:00:01+00:00").timestamp()
    monkeypatch.setattr(_outside.time, "time", lambda: started)

    run = vis.shell({"command": "printf dated"}).wait(10)

    assert run["exit"] == 0
    assert Path(run["log_path"]).parent == log_root / "2026-09-14" / "outside"
    assert Path(run["log_path"]).read_text() == "dated"
    assert _outside.state_home() == state_root
    assert not list(state_root.glob("shell-*.log"))


def test_shell_log_paths_survive_utc_midnight_without_overwriting(monkeypatch):
    clock = [datetime(2026, 9, 13, 23, 59, 59, tzinfo=UTC).timestamp()]
    monkeypatch.setattr(_outside.time, "time", lambda: clock[0])
    first = vis.shell({"command": "printf first"}).wait(10)
    first_path = Path(first["log_path"])
    clock[0] += 2
    second = vis.shell({"command": "printf second"}).wait(10)
    third = vis.shell({"command": "printf third"}).wait(10)

    assert first_path.parent.parent.name == "2026-09-13"
    assert Path(second["log_path"]).parent.parent.name == "2026-09-14"
    assert len({first["log_path"], second["log_path"], third["log_path"]}) == 3
    assert first.logs()["log_path"] == str(first_path)
    assert first.logs()["out"] == first_path.read_text() == "first"
    assert second.logs()["out"] == "second"
    assert third.logs()["out"] == "third"


@pytest.mark.parametrize("environment", [None, {}, {"VIS_SHELL_OVERRIDE": "child"}])
def test_shell_environment_overlays_the_parent_without_mutating_it(
    monkeypatch, environment
):
    monkeypatch.setenv("VIS_SHELL_INHERITED", "parent")
    monkeypatch.setenv("VIS_SHELL_OVERRIDE", "parent")
    names = ["PATH", "VIS_SHELL_INHERITED", "VIS_SHELL_OVERRIDE"]
    expected = {name: os.environ.get(name) for name in names}
    expected.update(environment or {})
    options = {
        "command": [
            sys.executable,
            "-c",
            "import json, os; "
            f"print(json.dumps({{name: os.environ.get(name) for name in {names!r}}}))",
        ]
    }
    if environment is not None:
        options["env"] = environment
    run = vis.shell(options).wait(10)
    assert run["exit"] == 0
    assert json.loads(run["out"]) == expected
    assert os.environ["VIS_SHELL_OVERRIDE"] == "parent"


def test_a_shell_handle_stops_what_it_started():
    run = vis.shell({"command": "sleep 30"})
    assert run["status"] == "running"
    stopped = run.stop()
    assert stopped["status"] == "stopped"
    assert stopped["exit"] is not None


def test_shell_reads_the_last_lines_from_a_negative_offset():
    run = vis.shell({"command": "printf 'one\ntwo\nthree\n'"}).wait(10)
    assert run.logs(-2)["out"].splitlines() == ["two", "three"]


@pytest.mark.parametrize("op", ["run", "background"])
def test_every_spawn_op_the_engine_speaks_starts_a_process(op):
    # The engine's `shell` takes `{"op": "run"|"background", …}`; an extension that
    # writes what the engine documents must not be refused out here.
    started = vis.shell({"op": op, "command": "printf hi"})
    assert started["stage"] == op
    run = started.wait(10)
    assert run["exit"] == 0
    assert run["out"] == "hi"


def test_a_named_spawn_answers_the_same_handle_the_next_call_reaches():
    run = vis.shell({"op": "background", "id": "tail-me", "command": "sleep 30"})
    assert run["id"] == "tail-me"
    assert run.stop()["status"] == "stopped"


def test_an_op_no_engine_speaks_is_refused_by_the_whole_vocabulary():
    with pytest.raises(_outside.Refused) as refusal:
        vis.shell({"op": "detonate", "command": "printf hi"})
    said = str(refusal.value)
    assert "detonate" in said
    for op in ("run", "background", "logs", "wait", "send", "stop"):
        assert f'"{op}"' in said


@pytest.mark.parametrize("op", ["logs", "wait", "send", "stop"])
def test_a_handle_op_names_the_handle_it_cannot_find(op):
    with pytest.raises(_outside.Refused, match="no such shell"):
        vis.shell({"op": op, "id": "never-started"})


# -- Asking a human ------------------------------------------------------------


FORM = [
    vis.heading("Target"),
    vis.select("env", ["staging", "prod"], label="Where", is_required=True),
    vis.password("token", label="Deploy token"),
]


def test_primed_answers_stand_in_for_a_human():
    vis.outside.answer_with({"env": "prod", "token": "hunter2"})
    answer = vis.ask("Deploy", FORM)
    assert answer
    assert answer.is_submitted is True
    assert answer["env"] == "prod"
    assert answer.reveal("token") == "hunter2"
    assert answer["token"] != "hunter2", "a secret leaves as a handle, out here too"


def test_answers_can_be_primed_from_the_environment(monkeypatch):
    monkeypatch.setenv(
        "VIS_OUTSIDE_ANSWERS", json.dumps({"env": "staging", "token": "t"})
    )
    assert vis.ask("Deploy", FORM)["env"] == "staging"


def test_an_ask_nobody_can_answer_is_undeliverable(monkeypatch):
    monkeypatch.setenv("VIS_OUTSIDE_NONINTERACTIVE", "1")
    answer = vis.ask("Deploy", FORM)
    assert not answer
    assert answer.reason == "undeliverable"
    assert answer.values == {}


def test_the_extensions_own_validators_run_out_here_too(monkeypatch):
    monkeypatch.setenv("VIS_OUTSIDE_NONINTERACTIVE", "1")
    vis.outside.answer_with({"port": "eighty"})
    answer = vis.ask(
        "Ports",
        [
            vis.plaintext(
                "port", validate=lambda v: None if v.isdigit() else "must be digits"
            )
        ],
    )
    assert not answer
    assert answer.reason == "cancelled"

    vis.outside.answer_with({"port": "80"})
    assert (
        vis.ask("Ports", [vis.plaintext("port", validate=lambda v: None)])["port"]
        == "80"
    )


def test_a_required_field_left_empty_is_not_submitted(monkeypatch):
    monkeypatch.setenv("VIS_OUTSIDE_NONINTERACTIVE", "1")
    vis.outside.answer_with({"env": "", "token": "t"})
    assert not vis.ask("Deploy", FORM)


# -- Judging a form ------------------------------------------------------------


def _refusal(title, fields, **options):
    """The line `vis.ask` refuses this form with, or None when it takes it."""
    try:
        vis.ask(title, fields, **options)
    except _outside.Refused as exc:
        return str(exc)
    return None


def test_ask_takes_a_well_formed_request_without_a_word(monkeypatch):
    monkeypatch.setenv("VIS_OUTSIDE_NONINTERACTIVE", "1")
    assert _refusal("Deploy", FORM) is None


@pytest.mark.parametrize(
    "form",
    [
        [vis.select("env", [])],
        [{"name": "env", "type": "dropdown"}],
        [vis.plaintext("env"), vis.plaintext("env")],
        [vis.otp("code", max_length=99)],
        [vis.slider("canary", min=100, max=0)],
        [vis.heading("nothing to answer")],
    ],
)
def test_ask_names_what_is_wrong_with_a_form(form):
    complaint = _refusal("Deploy", form)
    assert isinstance(complaint, str) and complaint


def test_ask_knows_only_the_contracts_field_types(monkeypatch):
    monkeypatch.setenv("VIS_OUTSIDE_NONINTERACTIVE", "1")
    for branch in _contracts.definition("view", "field")["oneOf"]:
        wire_type = branch["properties"]["type"]["const"]
        node = {"name": "a", "type": wire_type}
        if "options" in branch["properties"]:
            node["options"] = ["one", "two"]
        assert _refusal("Deploy", [node]) is None, wire_type


# -- A live view -----------------------------------------------------------------


def _painted(nodes):
    # The nodes that PAINT, groups flattened — the same walk the host does when it
    # hands the model a picture.
    flat = {}
    for node in nodes:
        if node["type"] == "group":
            flat.update(_painted(node["fields"]))
        else:
            flat[node["id"]] = node
    return flat


def test_a_live_view_outside_is_a_transcript_and_a_readable_state(capsys):
    # Nobody is watching a pane out here, so stderr carries the story and the
    # host still holds the nodes: an extension polling its own view reads the
    # same truth it would read inside a session.
    with vis.live(
        "Deploy",
        [
            vis.status("now", "Starting", tone="running"),
            # Layout is the FORM's own row, not a second vocabulary — and out
            # here, where nothing paints, it still travels with the view.
            vis.row(
                "reading",
                vis.table(
                    "jobs",
                    columns=[
                        vis.table_column("job", "Job"),
                        vis.table_column("state", "State"),
                    ],
                    is_selectable=True,
                    selected_ids=[],
                ),
                vis.stat(
                    "why",
                    stats=[{"id": "queued", "label": "Queued", "value_text": "1"}],
                ),
            ),
            vis.output("tail", label="Output"),
        ],
        description="staging",
    ) as view:
        view.status("Building", tone="running")
        view.row("api", ["deploy / api", "queued"], parent="deploy")
        view.row("api", ["deploy / api", "done"], tone="ok", parent="deploy")
        view["jobs"].select("api")
        view.write("cloning", "compiling")
        state = view.state()
        nodes = _painted(state["nodes"])
        # The row was upserted by its id, not appended twice.
        assert [row["cells"] for row in nodes["jobs"]["rows"]] == [
            ["deploy / api", "done"]
        ]
        assert nodes["jobs"]["rows"][0]["tone"] == "ok"
        assert nodes["jobs"]["rows"][0]["parent"] == "deploy"
        assert nodes["jobs"]["selected_ids"] == ["api"]
        assert nodes["tail"]["lines"] == ["cloning", "compiling"]
        assert nodes["now"]["text"] == "Building"
        # A row is a node of its own in the state the host holds, and the ops
        # above reached the table and the counter INSIDE it by id alone.
        arranged = state["nodes"][1]
        assert arranged["type"] == "group"
        assert arranged["direction"] == "row"
        assert [child["id"] for child in arranged["fields"]] == ["jobs", "why"]

    verdict = view.result
    assert verdict["is_completed"] is True
    assert verdict["reason"] == "completed"
    # Outside Vis nobody is watching, so nobody can have stopped it — but the key
    # is there either way, because the extension reads ONE verdict shape.
    assert verdict["is_from_human"] is False
    assert view.is_from_human is False
    assert view.note is None
    assert verdict["view"]["title"] == "Deploy"
    # The verdict a MODEL reads is flat: a row is how a surface arranged the work,
    # never part of what the work said.
    assert [node["id"] for node in verdict["view"]["nodes"]] == [
        "now",
        "jobs",
        "why",
        "tail",
    ]
    transcript = capsys.readouterr().err
    assert "== Deploy ==" in transcript
    assert "Building" in transcript
    assert "cloning" in transcript
    assert "completed" in transcript


def test_a_live_table_declares_the_groups_its_rows_hang_under():
    # A group has an id of its own, so the label a surface paints over it can
    # change mid-run without moving the rows under it, and a head can carry a
    # tone the rows never had.
    with vis.live(
        "Checks",
        [
            vis.table(
                "checks",
                columns=[vis.table_column("job", "Job")],
                groups=[vis.table_group("build", label="Build", order=0)],
                rows=[vis.table_row("compile", ["compile"], parent="build")],
            )
        ],
    ) as view:
        view.group("tests", label="Tests", order=1, is_open=True)
        view.row("unit", ["unit"], tone="error", parent="tests")
        # Re-toning a head keeps the label it was declared with.
        view.group("tests", tone="error")
        table = view.state()["nodes"][0]
        assert table["groups"] == [
            {"id": "build", "label": "Build", "order": 0},
            {
                "id": "tests",
                "label": "Tests",
                "order": 1,
                "is_open": True,
                "tone": "error",
            },
        ]
        assert [row.get("parent") for row in table["rows"]] == ["build", "tests"]
        # An undeclared parent is still a group: nothing has to declare one.
        view.row("lint", ["lint"], parent="lint")
        assert len(view.state()["nodes"][0]["groups"]) == 2


@pytest.mark.parametrize(
    "view",
    [
        ("", [vis.status("now", "waiting")]),
        ("Deploy", []),
        ("Deploy", [{"id": "now", "type": "sparkline"}]),
        ("Deploy", [vis.status("now", "waiting"), vis.status("now", "again")]),
        ("Deploy", [vis.status("", "waiting")]),
    ],
)
def test_a_live_view_names_what_is_wrong_with_it(view):
    with pytest.raises(_outside.Refused) as raised:
        vis.live(*view)
    assert str(raised.value)


@pytest.mark.parametrize("op", ["patch", "state", "close"])
def test_a_live_handle_op_names_the_view_it_cannot_find(op):
    with pytest.raises(_outside.Refused) as raised:
        _outside.live(json.dumps({"op": op, "view_id": "nope"}))
    assert "nope" in str(raised.value)


def test_a_push_at_a_node_the_view_never_declared_is_refused():
    view = vis.live("Deploy", [vis.status("now", "waiting")])
    envelope = {
        "op": "patch",
        "view_id": view.view_id,
        "patch": {"ops": [{"op": "set", "node_id": "ghost", "text": "?"}]},
    }
    with pytest.raises(_outside.Refused) as raised:
        _outside.live(json.dumps(envelope))
    assert "ghost" in str(raised.value)


def test_a_view_may_answer_the_model_with_an_optimized_string():
    view = vis.live("Scan", [vis.status("now", "reading")])
    result = view.close(
        summary="the durable human-facing ending",
        model_result="Scan complete: 12 files checked, no findings.",
    )

    assert result == "Scan complete: 12 files checked, no findings."
    assert view.close() == result


def test_a_view_that_ended_answers_its_verdict_rather_than_vanishing():
    view = vis.live("Scan", [vis.status("now", "reading")])
    verdict = view.close(reason="interrupted", summary="the human stopped watching")

    assert verdict["is_completed"] is False
    assert view.reason == "interrupted"
    # An extension may end its own view as interrupted; only a PERSON's stop is
    # stamped as one, and only a person leaves a note.
    assert verdict["is_from_human"] is False
    assert "note" not in verdict
    # A `finally` closing what an interrupt already closed must not mint a
    # second, cheerier ending.
    assert view.close()["reason"] == "interrupted"
    with pytest.raises(vis.Interrupted):
        view.status("one more line")


def test_a_burst_of_pushes_crosses_the_boundary_once_per_window(monkeypatch):
    # The batching window is the contract's, and a compute loop that reports
    # every iteration must not pay a host call for every iteration.
    assert vis._FLUSH_MS == 100
    crossed = []
    serve = _outside.live

    def counted(envelope_json):
        crossed.append(json.loads(envelope_json)["op"])
        return serve(envelope_json)

    monkeypatch.setattr(vis._host, "live", counted)
    view = vis.live("Scan", [vis.output("tail")], flush_ms=60_000)
    view.write("starting")
    # Leading edge: the first sign of life does not wait for a window.
    assert crossed == ["open", "patch"]

    for index in range(50):
        view.write(f"line {index}")
    assert crossed == ["open", "patch"]

    verdict = view.close()
    assert crossed.count("patch") == 2
    lines = verdict["view"]["nodes"][0]["lines"]
    assert lines[0] == "starting"
    assert lines[-1] == "line 49"


# Regression, session 8d48e75e-5cd5-41d2-9b13-77fb639de366: one selection
# transition crossed as several partial pictures instead of one materialized batch.
def test_an_explicit_batch_crosses_as_one_complete_patch(monkeypatch):
    crossed = []
    serve = _outside.live

    def counted(envelope_json):
        crossed.append(json.loads(envelope_json))
        return serve(envelope_json)

    monkeypatch.setattr(vis._host, "live", counted)
    view = vis.live(
        "CI",
        [
            vis.status("run", "Job A"),
            vis.steps("steps", steps=[{"id": "a", "label": "Old"}]),
        ],
        flush_ms=60_000,
    )

    with view.batch():
        view["run"].set("Job B")
        view["steps"].clear()
        view["steps"].set("b", label="New")
        view.add(vis.output("output", label="Job B"), after="run")
        view["output"].write("ready")
        assert [one["op"] for one in crossed] == ["open"]

    patches = [one for one in crossed if one["op"] == "patch"]
    assert len(patches) == 1
    assert [op["op"] for op in patches[0]["patch"]["ops"]] == [
        "set",
        "clear",
        "append",
        "add-node",
        "append",
    ]
    assert view.state()["nodes"][0]["text"] == "Job B"
    view.close()


def test_the_first_op_crosses_on_a_freshly_booted_machine(monkeypatch):
    # Regression: `time.monotonic()` counts from an arbitrary origin, so on a machine
    # up for eight seconds every stamp of "never" fell INSIDE a 60s window — the first
    # sign of life waited for the whole window instead of crossing at once, and the
    # poll behind `is_interrupted` never asked. It failed only on freshly booted CI.
    monkeypatch.setattr(vis.time, "monotonic", lambda: 8.0)
    crossed = []
    serve = _outside.live

    def counted(envelope_json):
        crossed.append(json.loads(envelope_json)["op"])
        return serve(envelope_json)

    monkeypatch.setattr(vis._host, "live", counted)
    view = vis.live("Scan", [vis.output("tail")], flush_ms=60_000)
    view.write("starting")

    assert crossed == ["open", "patch"]
    assert view.is_interrupted is False
    assert crossed == ["open", "patch", "state"]
    # The clock has not moved since that read, so the window still holds the next one.
    assert view.is_interrupted is False
    assert crossed == ["open", "patch", "state"]
    view.close()


def test_a_nap_answers_a_surface_the_moment_it_touches_the_view(monkeypatch):
    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: a watcher slept out its
    # whole provider tick before reading shared state, so a tap on a row waited seconds
    # for details the extension already had.
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    view = vis.live(
        "CI",
        [
            vis.table(
                "jobs",
                columns=[vis.table_column("job", "Job")],
                rows=[vis.table_row("a", ["one"]), vis.table_row("b", ["two"])],
                is_selectable=True,
                selected_ids=["a"],
            )
        ],
    )

    # A change before the host starts waiting must not be lost.
    before = len(recorder.said)
    assert view.sleep(0) is False
    assert len(recorder.said) == before
    recorder.select("jobs", ["b"])
    assert view.sleep(3.0) is True
    assert recorder.node("jobs")["selected_ids"] == ["b"]
    assert len(recorder.said) == before + 1
    assert view.sleep(0.01) is False
    view.close()
