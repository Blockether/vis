"""The package outside a Vis session: the contract is the specification.

Every test here asks the same question in a different place — does `vis` behave
the way `vis_contract`'s document says it behaves when no engine is in the room — so a
contract op that grows, moves or changes its outside behavior fails HERE, in the
package, and not in an extension somebody wrote against it.
"""

import inspect
import json

import pytest
import vis
import vis_contract
from vis import _outside

CONTRACT = _outside.contract


def _op(name):
    return next(op for op in CONTRACT["ops"] if op["name"] == name)


# -- The document and the implementation ---------------------------------------


def test_the_host_is_a_contract_host_and_serves_exactly_the_declared_ops():
    # The protocol is the interface anyone else implements, so this host has to
    # satisfy it the same way a stranger's would.
    assert isinstance(_outside.host, vis_contract.Host)
    served = sorted(n for n in vars(_outside.host) if not n.startswith("_"))
    assert served == sorted(op["name"] for op in CONTRACT["ops"])


def test_every_op_accepts_the_arity_the_contract_declares():
    for op in CONTRACT["ops"]:
        fn = getattr(_outside.host, op["name"])
        if op["outside"] == "refuse":
            continue  # a refusal takes anything and answers the same way
        params = [
            p
            for p in inspect.signature(fn).parameters.values()
            if p.kind in (p.POSITIONAL_ONLY, p.POSITIONAL_OR_KEYWORD)
        ]
        required = [p for p in params if p.default is p.empty]
        assert len(required) <= op["arity"] <= len(params), op["name"]


class _Recorder:
    """A host somebody else could have written: the contract's ops, nothing else."""

    def __init__(self):
        self.calls = []
        for name in vis_contract.OPS:
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
    assert isinstance(stranger, vis_contract.Host)
    assert vis_contract.check_host(stranger) is stranger

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


def test_a_refusing_op_raises_the_refusal_the_contract_states():
    for op in CONTRACT["ops"]:
        if op["outside"] != "refuse":
            continue
        with pytest.raises(_outside.Refused) as raised:
            getattr(vis, op["name"])({"command": "true"})
        assert str(raised.value) == op["refusal"]
        assert op["name"] in op["refusal"]


def test_the_jailed_shells_are_the_only_refusals():
    refused = {op["name"] for op in CONTRACT["ops"] if op["outside"] == "refuse"}
    assert refused == {"jailed_shell", "jailed_shell_session"}


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
    assert handle.startswith(CONTRACT["human_input"]["secret_handle_prefix"])
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


def test_shell_runs_a_command_and_answers_the_engine_shape():
    run = vis.shell({"command": "printf hello"}).wait(10)
    assert run["exit"] == 0
    assert run["out"] == "hello"
    assert run["status"] == "exited"
    assert set(run) == set(_outside._SHELL_RESULT_KEYS)
    assert run.logs()["out"] == "hello"


def test_a_shell_handle_stops_what_it_started():
    run = vis.shell({"command": "sleep 30"})
    assert run["status"] == "running"
    stopped = run.stop()
    assert stopped["status"] == "stopped"
    assert stopped["exit"] is not None


def test_shell_reads_the_last_lines_from_a_negative_offset():
    run = vis.shell({"command": "printf 'one\ntwo\nthree\n'"}).wait(10)
    assert run.logs(-2)["out"].splitlines() == ["two", "three"]


@pytest.mark.parametrize("op", CONTRACT["shell"]["spawn_ops"])
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
    for op in CONTRACT["shell"]["spawn_ops"] + CONTRACT["shell"]["handle_ops"]:
        assert f'"{op}"' in said


@pytest.mark.parametrize("op", CONTRACT["shell"]["handle_ops"])
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
    for wire_type in CONTRACT["human_input"]["field_types"]:
        node = {"name": "a", "type": wire_type}
        if wire_type in CONTRACT["human_input"]["choice_types"]:
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
                    is_focusable=True,
                    focused_ids=[],
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
        view.row("api", ["api", "queued"])
        view.row("api", ["api", "done"], tone="ok")
        view["jobs"].focus("api")
        view.write("cloning", "compiling")
        state = view.state()
        nodes = _painted(state["nodes"])
        # The row was upserted by its id, not appended twice.
        assert [row["cells"] for row in nodes["jobs"]["rows"]] == [["api", "done"]]
        assert nodes["jobs"]["rows"][0]["tone"] == "ok"
        assert nodes["jobs"]["focused_ids"] == ["api"]
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


@pytest.mark.parametrize("op", CONTRACT["live"]["handle_ops"])
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
    assert vis._FLUSH_MS == CONTRACT["live"]["flush_ms"]
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
