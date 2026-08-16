"""The package outside a Vis session: the contract is the specification.

Every test here asks the same question in a different place — does `vis` behave
the way `vis/contract.json` says it behaves when no engine is in the room — so a
contract op that grows, moves or changes its outside behavior fails HERE, in the
package, and not in an extension somebody wrote against it.
"""

import inspect
import json

import pytest
import vis
from vis import _outside

CONTRACT = _outside.contract


def _op(name):
    return next(op for op in CONTRACT["ops"] if op["name"] == name)


# -- The document and the implementation ---------------------------------------


def test_the_host_serves_exactly_the_declared_ops():
    assert sorted(_outside.host) == sorted(op["name"] for op in CONTRACT["ops"])


def test_every_op_accepts_the_arity_the_contract_declares():
    for op in CONTRACT["ops"]:
        fn = _outside.host[op["name"]]
        if op["outside"] == "refuse":
            continue  # a refusal takes anything and answers the same way
        params = [
            p
            for p in inspect.signature(fn).parameters.values()
            if p.kind in (p.POSITIONAL_ONLY, p.POSITIONAL_OR_KEYWORD)
        ]
        required = [p for p in params if p.default is p.empty]
        assert len(required) <= op["arity"] <= len(params), op["name"]


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
    assert run["stdout"] == "hello"
    assert run["status"] == "exited"
    assert set(run) == set(_outside._SHELL_RESULT_KEYS)
    assert run.logs()["stdout"] == "hello"


def test_a_shell_handle_stops_what_it_started():
    run = vis.shell({"command": "sleep 30"})
    assert run["status"] == "running"
    stopped = run.stop()
    assert stopped["status"] == "stopped"
    assert stopped["exit"] is not None


def test_shell_reads_the_last_lines_from_a_negative_offset():
    run = vis.shell({"command": "printf 'one\ntwo\nthree\n'"}).wait(10)
    assert run.logs(-2)["stdout"].splitlines() == ["two", "three"]


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


def test_check_reads_a_well_formed_request_and_says_nothing():
    assert vis.check("Deploy", FORM) is None


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
def test_check_names_what_is_wrong_with_a_form(form):
    complaint = vis.check("Deploy", form)
    assert isinstance(complaint, str) and complaint


def test_check_knows_only_the_contracts_field_types():
    for wire_type in CONTRACT["human_input"]["field_types"]:
        node = {"name": "a", "type": wire_type}
        if wire_type in CONTRACT["human_input"]["choice_types"]:
            node["options"] = ["one", "two"]
        assert vis.check("Deploy", [node]) is None, wire_type
