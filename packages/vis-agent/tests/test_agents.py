"""Managed teams use the real session-bound SDK transport and canonical schema."""

import json
import threading
from concurrent.futures import CancelledError, TimeoutError, as_completed, wait

import pytest
from blockether.vis.engine import (
    Council,
    GatewayClient,
    Subagent,
    SubagentRecord,
    SubagentTask,
)
from test_client import compatible, endpoint

CHILD = {
    "session_id": "child",
    "parent_id": "leader",
    "leader_id": "leader",
    "team_id": "task",
    "task": "Check the contract",
    "role": "verify",
    "status": "running",
    "iteration_budget": 4,
    "iterations_used": 1,
    "provider": "p",
    "model": "small",
    "routing_locked": False,
    "pending_input": True,
    "usage": {"cost_usd": 0.02},
}


def council_compatible(method, path, body):
    if result := compatible(method, path, body):
        return result
    if path.split("?")[0].endswith("/council"):
        return 200, {"default_group_id": "G", "activation_id": "active"}
    return None


def test_council_is_the_only_public_team_api():
    import blockether.vis.engine as engine

    assert "Agents" not in engine.__all__
    assert not hasattr(engine, "Agents")
    assert not hasattr(engine.Session, "agents")
    assert not hasattr(Council, "publish_spawn")
    assert not hasattr(Council, "list")
    # The subscription.ready frame carries the team; the gateway has no team read.
    assert not hasattr(Council, "subagents")
    assert not hasattr(Council, "resume")
    assert not hasattr(Council, "wait")
    assert not hasattr(Council, "cancel")
    assert all(
        callable(getattr(Council, operation))
        for operation in (
            "spawn",
            "run",
            "extend",
            "deny",
            "grant",
            "route",
        )
    )


def test_managed_team_transport():
    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        if path.endswith("/agents"):
            return 200, CHILD
        return 200, [{"session_id": "child", "status": "cancelled"}]

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        assert isinstance(team, Council)
        child = team.spawn(
            "Check the contract",
            role="verify",
            provider="p",
            model="small",
            iteration_budget=4,
            allowed_models=[{"provider": "p", "model": "small"}],
            files={"write": ["src/owner/**"]},
            key="contract",
        )
        assert isinstance(child, Subagent)
        assert child.parent_id == "leader" and child.role == "verify"
        assert child.pending_input and child.usage == {"cost_usd": 0.02}
        team.route("small", provider="p", session_id=child.session_id)
        requests = [
            (method, path, json.loads(body) if body else None)
            for method, path, _, body in calls
            if "/agents" in path
        ]
        assert requests == [
            (
                "POST",
                "/v1/sessions/leader/agents",
                {
                    "task": "Check the contract",
                    "role": "verify",
                    "iteration_budget": 4,
                    "provider": "p",
                    "model": "small",
                    "allowed_models": [{"provider": "p", "model": "small"}],
                    "files": {"write": ["src/owner/**"]},
                    "key": "contract",
                },
            ),
            (
                "POST",
                "/v1/sessions/leader/agents/route",
                {"model": "small", "provider": "p", "session_id": "child"},
            ),
        ]


def task_status(task_id, state, record, *, owner="leader"):
    return {
        "task": {
            "id": task_id,
            "session_id": owner,
            "kind": "subagent",
            "name": "Check the contract",
            "state": state,
            "origin": {},
            "started_at": 1,
            "view_ids": [],
            "detached": False,
        },
        "record": record,
    }


def test_child_controls_answer_records_without_settling_the_task():
    asking = {
        "session_id": "child",
        "status": "asking",
        "question": "May I write docs?",
        "question_entry_id": 7,
        "files_request": {"files": {"write": ["docs/**"]}, "reason": "Docs"},
        "iteration_budget": 4,
        "iterations_used": 2,
    }
    answered = threading.Event()
    callback = threading.Event()
    completed = {"session_id": "child", "status": "completed", "answer": "Checked"}

    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        if path.endswith("/run"):
            if json.loads(body)["tasks"][0] == "Check":
                return 200, [{"id": "a1", "session_id": "child", "task_id": "t1"}]
            return 200, [
                {"id": "a1", "session_id": "child", "task_id": "t2"},
                {"id": "a2", "session_id": "second", "task_id": "t3"},
            ]
        if path.endswith("/tasks/t1?include_record=true"):
            if not answered.is_set():
                return 200, task_status("t1", "running", asking)
            return 200, task_status("t1", "done", completed)
        if path.endswith("/tasks/t2?include_record=true"):
            return 200, task_status("t2", "done", {**completed, "answer": "Follow-up"})
        if path.endswith("/tasks/t3?include_record=true"):
            return 200, task_status(
                "t3", "done", {"session_id": "second", "status": "blocked"}
            )
        if path.endswith("/answer"):
            answered.set()
        return 200, [asking]

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        (task,) = team.run(["Check"])
        assert isinstance(task, SubagentTask)
        assert (task.id, task.session_id, task.task_id) == ("a1", "child", "t1")
        assert task.record().files_request["reason"] == "Docs"
        assert task.record().question_entry_id == 7
        assert task.running() and not task.done()
        with pytest.raises(TimeoutError):
            task.result(timeout=0.02)
        assert wait([task], timeout=0).not_done == {task}
        task.add_done_callback(lambda _: callback.set())
        assert isinstance(team.extend("child", 5), SubagentRecord)
        assert team.deny("child", "Not now").status == "asking"
        assert team.grant("child", {"write": ["docs/**"]}).iterations_used == 2
        assert team.answer("child", "Use docs/").session_id == "child"
        assert list(as_completed([task], timeout=3)) == [task]
        assert callback.wait(3)
        assert task.exception() is None and task.result().answer == "Checked"
        assert not task.cancel()
        followup, dependent = team.run(
            [
                {"task": "Continue", "child": task, "files": {"write": ["docs/**"]}},
                {"task": "Verify", "after": [task, "a3"]},
            ]
        )
        assert (followup.id, followup.session_id, followup.task_id) == (
            task.id,
            task.session_id,
            "t2",
        )
        assert followup.result(3).answer == "Follow-up"
        assert dependent.result(3).status == "blocked"
        assert task.result().answer == task.record().answer == "Checked"
        with pytest.raises(ValueError, match="another session"):
            client.session("unowned").council().run(
                [{"task": "Continue", "child": task}]
            )
        launches = [
            json.loads(body)
            for method, path, _, body in calls
            if path.endswith("/agents/run")
        ]
        assert launches == [
            {"tasks": ["Check"]},
            {
                "tasks": [
                    {
                        "task": "Continue",
                        "child": "child",
                        "files": {"write": ["docs/**"]},
                    },
                    {"task": "Verify", "after": ["child", "a3"]},
                ]
            },
        ]


@pytest.mark.parametrize("accepted", [True, False])
def test_cancel_observes_host_settlement_and_sends_one_mutation(accepted):
    cancelled = threading.Event()
    settled = threading.Event()

    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        if path.endswith("/run"):
            return 200, [{"id": "a1", "session_id": "child", "task_id": "t1"}]
        if path.endswith("/cancel"):
            cancelled.set()
            return 200, {"requested": accepted}
        state = ("cancelled" if accepted else "done") if settled.is_set() else "running"
        return 200, task_status(
            "t1",
            state,
            {
                "session_id": "child",
                "status": {
                    "cancelled": "cancelled",
                    "done": "failed",
                    "running": "running",
                }[state],
            },
        )

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        (task,) = client.session("leader").council().run(["Check"])
        assert task.cancel() is accepted
        assert cancelled.is_set() and not task.done()
        settled.set()
        assert wait([task], timeout=3).done == {task}
        if accepted:
            assert task.cancelled()
            with pytest.raises(CancelledError):
                task.result()
            assert task.record().status == "cancelled"
        else:
            assert task.result().status == "failed"
        assert [
            (verb, path) for verb, path, _, _ in calls if path.endswith("/cancel")
        ] == [
            ("POST", "/v1/sessions/leader/tasks/t1/cancel"),
        ]


def test_extension_task_polls_on_the_background_host_door():
    from base64 import b64encode

    from blockether.vis.engine._host import HostLayer

    caller = threading.get_ident()
    polled = threading.Event()
    cancelled = threading.Event()

    def exchange(message):
        frame = json.loads(message)
        route = frame["route"]
        if route.endswith("/council"):
            value = {"default_group_id": "G", "activation_id": "active"}
        elif route.endswith("/agents/run"):
            assert caller == threading.get_ident()
            value = [{"task_id": "t1", "id": "a1", "session_id": "child"}]
        elif route.endswith("/tasks/t1/cancel"):
            assert frame["method"] == "POST"
            cancelled.set()
            value = {"requested": True}
        else:
            assert route.endswith("/tasks/t1") and frame["method"] == "GET"
            assert caller != threading.get_ident()
            state = "cancelled" if cancelled.is_set() else "running"
            value = task_status("t1", state, {"session_id": "child", "status": state})
            polled.set()
        return json.dumps(
            {
                "status": 200,
                "headers": {},
                "content": b64encode(json.dumps(value).encode()).decode(),
            }
        )

    with HostLayer(exchange) as client:
        (task,) = client.session("leader").council().run(["Check"])
        assert polled.wait(3) and not task.done()
        assert task.cancel()
        with pytest.raises(CancelledError):
            task.result(3)


def test_task_observer_shares_framed_stdio_with_the_caller(tmp_path):
    import sys
    from textwrap import dedent

    from blockether.vis._contracts import definition
    from blockether.vis.engine import StdioEngineClient

    hello = {
        "protocol": definition("gateway", "handshake")["properties"]["protocol"][
            "const"
        ]
    }
    status = {
        state: task_status("t1", state, {"session_id": "child", "status": state})
        for state in ("running", "cancelled")
    }
    code = dedent(f"""
        import base64, json, sys
        print(json.dumps({hello!r}), flush=True)
        state = "running"
        for line in sys.stdin:
            frame = json.loads(line)
            route = frame["route"]
            if route.endswith("/council"):
                data = {{"default_group_id": "G", "activation_id": "active"}}
            elif route.endswith("/agents/run"):
                data = [{{"task_id": "t1", "id": "a1", "session_id": "child"}}]
            elif route.endswith("/tasks/t1/cancel"):
                assert frame["method"] == "POST"
                state = "cancelled"
                data = {{"requested": True}}
            else:
                assert route.endswith("/tasks/t1") and frame["method"] == "GET"
                data = {status!r}[state]
            content = base64.b64encode(json.dumps(data).encode()).decode()
            print(json.dumps({{"status": 200, "headers": {{}}, "content": content}}), flush=True)
        """)
    with StdioEngineClient(
        executable=[sys.executable, "-c", code], root=tmp_path
    ) as client:
        (task,) = client.session("leader").council().run(["Check"])
        assert task.record().status == "running"
        assert task.cancel()
        with pytest.raises(CancelledError):
            task.result(3)
        assert task.record().status == "cancelled"


def test_foreign_task_status_is_not_accepted_as_a_child_result():
    from blockether.vis.engine import ProtocolError

    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        if path.endswith("/run"):
            return 200, [{"id": "a1", "session_id": "child", "task_id": "t1"}]
        return 200, task_status(
            "t1",
            "done",
            {
                "session_id": "child",
                "status": "completed",
                "answer": "Foreign",
            },
            owner="other",
        )

    with endpoint(respond) as (url, _), GatewayClient(url) as client:
        (task,) = client.session("leader").council().run(["Check"])
        assert isinstance(task.exception(3), ProtocolError)
        with pytest.raises(ProtocolError):
            task.result()


@pytest.mark.parametrize(
    "record",
    [
        {"status": "completed"},
        {"session_id": "child", "status": "invented"},
        {"session_id": "child", "status": "completed", "future_field": 1},
        {
            "session_id": "child",
            "status": "failed",
            "error": {"code": "other", "message": "x"},
        },
    ],
)
def test_child_record_response_is_validated(record):
    with pytest.raises(ValueError):
        SubagentRecord.from_wire(record)


def test_rate_limited_child_record_names_working_routes():
    record = SubagentRecord.from_wire(
        {
            "session_id": "child",
            "status": "failed",
            "error": {
                "code": "rate_limited",
                "message": "Provider rate-limited",
                "provider": "busy",
                "model": "large",
            },
            "alternatives": [{"provider": "spare", "model": "small"}],
        }
    )
    assert record.error["code"] == "rate_limited"
    assert record.alternatives == [{"provider": "spare", "model": "small"}]


@pytest.mark.parametrize("code", ["disabled", "group-not-found"])
def test_unavailable_council_binding_does_not_block_team_controls(code):
    from blockether.vis.engine import GatewayError

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if "/council" in path:
            return 403, {"error": {"type": code, "message": "Unavailable binding"}}
        if path.endswith("/agents"):
            return 200, CHILD
        return 200, [{"session_id": "child", "status": "cancelled"}]

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        team.route("small", provider="p")
        team.spawn("Check the contract")
        for communicate in (
            lambda: team.group_id,
            team.members,
            team.threads,
            team.read,
            lambda: team.get(1),
            lambda: team.publish("Question"),
            lambda: team.wake("Update"),
        ):
            with pytest.raises(GatewayError) as error:
                communicate()
            assert error.value.code == code
            assert error.value.status == 403
        assert sum("/council" in path for _, path, _, _ in calls) == 1


def test_group_members_are_not_the_managed_team():
    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        if "/council/members" in path:
            return 200, [
                {"session_id": "peer", "title": "Independent", "state": "running"}
            ]
        return 200, [CHILD]

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        council = client.session("leader").council(group_id="G")
        assert [member.session_id for member in council.members()] == ["peer"]
        assert [
            (method, path)
            for method, path, _, _ in calls
            if path.startswith("/v1/sessions/")
        ] == [
            ("GET", "/v1/sessions/leader/council?group_id=G"),
            ("GET", "/v1/sessions/leader/council/members?group_id=G"),
        ]


@pytest.mark.parametrize(
    "opts",
    [
        {"task": " "},
        {"task": "x", "iteration_budget": 0},
        {"task": "x", "model": "small"},
        {"task": "x", "allowed_models": []},
        {"task": "x", "files": {"exec": ["a"]}},
    ],
)
def test_spawn_rejects_invalid_input_before_http(opts):
    with endpoint(council_compatible) as (url, calls), GatewayClient(url) as client:
        with pytest.raises(ValueError):
            client.session("leader").council().spawn(**opts)
        assert not any("/agents" in path for _, path, _, _ in calls)


def test_child_response_is_validated():
    with pytest.raises(ValueError):
        Subagent.from_wire({**CHILD, "status": "invented"})


def test_child_progress_note_is_read_and_validated():
    child = Subagent.from_wire({**CHILD, "note": "Reading the parser"})
    assert child.note == "Reading the parser"
    assert Subagent.from_wire(CHILD).note is None
    with pytest.raises(ValueError):
        Subagent.from_wire({**CHILD, "note": ""})


@pytest.mark.parametrize("usage", ["invalid", [], 1, True])
def test_child_usage_response_is_validated(usage):
    with pytest.raises(ValueError):
        Subagent.from_wire({**CHILD, "usage": usage})


def test_team_and_child_fields_are_frozen():
    from dataclasses import FrozenInstanceError

    child = Subagent.from_wire(CHILD)
    with pytest.raises(FrozenInstanceError):
        child.status = "completed"
    with pytest.raises(FrozenInstanceError):
        Council(None, "G", None)._session = object()


@pytest.mark.parametrize(
    "operation,kwargs",
    [
        ("route", {"model": "", "provider": "p"}),
        ("route", {"model": "small", "provider": ""}),
        ("route", {"model": "small", "provider": "p", "session_id": ""}),
        ("run", {"tasks": []}),
        ("run", {"tasks": [{"task": "x", "key": "k"}]}),
        ("run", {"tasks": [{"task": "x", "child": "a1", "after": ["a2"]}]}),
        ("extend", {"session_id": "child", "iterations": 0}),
        ("deny", {"session_id": "child", "reason": ""}),
        ("spawn", {"task": "x", "provider": "p"}),
        ("spawn", {"task": "x", "iteration_budget": True}),
        ("spawn", {"task": "x", "key": ""}),
        ("spawn", {"task": "x", "allowed_models": [{"model": "small"}]}),
        (
            "spawn",
            {"task": "x", "allowed_models": [{"provider": "p", "model": "small"}] * 2},
        ),
    ],
)
def test_team_operations_validate_before_http(operation, kwargs):
    with endpoint(council_compatible) as (url, calls), GatewayClient(url) as client:
        with pytest.raises(ValueError):
            getattr(client.session("leader").council(), operation)(**kwargs)
        assert not any("/agents" in path for _, path, _, _ in calls)


@pytest.mark.parametrize(
    "operation,kwargs,code",
    [
        ("spawn", {"task": "Check the contract"}, "inactive-session"),
        ("spawn", {"task": "Check the contract"}, "no-checkpoint"),
        (
            "spawn",
            {"task": "Check the contract", "key": "retry"},
            "idempotency-conflict",
        ),
        ("run", {"tasks": [{"task": "Continue", "child": "unowned"}]}, "not-owned"),
        ("route", {"provider": "p", "model": "small"}, "routing-locked"),
    ],
)
def test_host_refusals_remain_errors_without_retry(operation, kwargs, code):
    from blockether.vis.engine import GatewayError

    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        return 409, {"error": {"type": code, "message": "Refused"}}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        with pytest.raises(GatewayError) as error:
            getattr(client.session("leader").council(), operation)(**kwargs)
        assert error.value.status == 409
        assert error.value.code == code
        assert sum("/agents" in path for _, path, _, _ in calls) == 1


def test_empty_team_and_default_spawn_and_self_route():
    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        if path.endswith("/route"):
            return 200, {
                "session_id": "leader",
                "provider": "p",
                "model": "small",
                "effective": "next_request",
            }
        return 200, CHILD

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        team.spawn("Check the contract")
        assert team.route("small", provider="p")["effective"] == "next_request"
        bodies = [
            json.loads(body)
            for method, path, _, body in calls
            if "/agents" in path and method == "POST"
        ]
        assert bodies == [
            {"task": "Check the contract"},
            {"model": "small", "provider": "p"},
        ]


@pytest.mark.parametrize(
    "status",
    ["queued", "running", "completed", "failed", "cancelled", "budget_limited"],
)
def test_all_lifecycle_states_and_optional_response_defaults(status):
    required = {
        key: CHILD[key]
        for key in (
            "session_id",
            "parent_id",
            "leader_id",
            "team_id",
            "task",
            "iteration_budget",
            "iterations_used",
        )
    }
    child = Subagent.from_wire(
        {**required, "status": status, "future_field": "ignored"}
    )
    assert child.status == status
    assert child.model is None and child.provider is None and child.usage is None
    assert not child.pending_input and not child.routing_locked


def test_managed_team_recipe(capsys):
    # The public guide hides experimental teams; keep their recipe coverage here.

    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        return 200, CHILD

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        child = team.spawn(
            "Check the SDK contract; report test evidence and remaining risks",
            iteration_budget=16,
            key="sdk-contract",
        )
        assert child.session_id == CHILD["session_id"]
        print(child.task, child.status, child.iterations_used, child.usage)
        assert "Check the contract running 1" in capsys.readouterr().out
        spawn = next(
            json.loads(body)
            for method, path, _, body in calls
            if method == "POST" and path.endswith("/agents")
        )
        assert spawn["iteration_budget"] == 16
        assert spawn["key"] == "sdk-contract"


@pytest.mark.parametrize(
    "status,code", [(401, "unauthorized"), (403, "forbidden"), (500, "unavailable")]
)
def test_unrelated_binding_errors_are_not_deferred(status, code):
    from blockether.vis.engine import GatewayError

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        return status, {"error": {"type": code, "message": "Refused"}}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        with pytest.raises(GatewayError) as error:
            client.session("leader").council()
        assert error.value.status == status
        assert error.value.code == code
        assert not any("/agents" in path for _, path, _, _ in calls)
