"""Managed teams use the real session-bound SDK transport and canonical schema."""

import json

import pytest
from blockether.vis.engine import Council, GatewayClient, Subagent, SubagentRecord
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
    assert not hasattr(Council, "resume")
    assert all(
        callable(getattr(Council, operation))
        for operation in (
            "spawn",
            "subagents",
            "run",
            "wait",
            "cancel",
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
            return 200, [CHILD] if method == "GET" else CHILD
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
        assert team.subagents() == (child,)
        team.route("small", provider="p", session_id=child.session_id)
        assert team.cancel(child.session_id).status == "cancelled"
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
            ("GET", "/v1/sessions/leader/agents", None),
            (
                "POST",
                "/v1/sessions/leader/agents/route",
                {"model": "small", "provider": "p", "session_id": "child"},
            ),
            ("POST", "/v1/sessions/leader/agents/cancel", {"session_id": "child"}),
        ]


def test_child_controls_answer_records_and_wait_longer_than_the_gateway():
    asking = {
        "session_id": "child",
        "status": "asking",
        "question": "May I write docs?",
        "question_entry_id": 7,
        "files_request": {"files": {"write": ["docs/**"]}, "reason": "Docs"},
        "iteration_budget": 4,
        "iterations_used": 2,
    }

    def respond(method, path, body):
        if result := council_compatible(method, path, body):
            return result
        return 200, [asking]

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        session = client.session("leader")
        timeouts = []
        request = client._request

        def spy(*args, **kwargs):
            timeouts.append(kwargs.get("timeout"))
            return request(*args, **kwargs)

        client._request = spy
        team = session.council()
        timeouts.clear()
        records = team.run(["Check", {"task": "Docs", "files": {"write": ["docs/**"]}}])
        assert records == (SubagentRecord.from_wire(asking),)
        assert records[0].files_request["reason"] == "Docs"
        assert team.wait(["child"], timeout_secs=5)[0].status == "asking"
        assert team.wait()[0].question_entry_id == 7
        assert isinstance(team.extend("child", 5), SubagentRecord)
        assert team.deny("child", "Not now").status == "asking"
        assert team.answer("child", "Use docs/").session_id == "child"
        assert team.grant("child", {"write": ["docs/**"]}).iterations_used == 2
        assert timeouts == [270, 35, 270, None, None, None, None]
        requests = [
            (path.removeprefix("/v1/sessions/leader/agents"), json.loads(body))
            for method, path, _, body in calls
            if "/agents" in path
        ]
        assert requests == [
            (
                "/run",
                {
                    "tasks": [
                        "Check",
                        {"task": "Docs", "files": {"write": ["docs/**"]}},
                    ],
                    "timeout_secs": 240,
                },
            ),
            ("/wait", {"session_ids": ["child"], "timeout_secs": 5}),
            ("/wait", {"timeout_secs": 240}),
            ("/extend", {"session_id": "child", "iterations": 5}),
            ("/deny", {"session_id": "child", "reason": "Not now"}),
            ("/answer", {"session_id": "child", "text": "Use docs/"}),
            ("/grant", {"session_id": "child", "files": {"write": ["docs/**"]}}),
        ]


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
            return 200, [CHILD] if method == "GET" else CHILD
        return 200, [{"session_id": "child", "status": "cancelled"}]

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        assert team.subagents() == (Subagent.from_wire(CHILD),)
        team.cancel("child")
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
        assert [child.session_id for child in council.subagents()] == ["child"]
        assert [
            (method, path)
            for method, path, _, _ in calls
            if path.startswith("/v1/sessions/")
        ] == [
            ("GET", "/v1/sessions/leader/council?group_id=G"),
            ("GET", "/v1/sessions/leader/council/members?group_id=G"),
            ("GET", "/v1/sessions/leader/agents"),
        ]


@pytest.mark.parametrize(
    "opts",
    [
        {"task": " "},
        {"task": "x", "iteration_budget": 0},
        {"task": "x", "iteration_budget": 601},
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
        ("cancel", {"session_id": ""}),
        ("route", {"model": "", "provider": "p"}),
        ("route", {"model": "small", "provider": ""}),
        ("route", {"model": "small", "provider": "p", "session_id": ""}),
        ("run", {"tasks": []}),
        ("run", {"tasks": ["x"], "timeout_secs": 0}),
        ("run", {"tasks": [{"task": "x", "key": "k"}]}),
        ("wait", {"session_ids": ["a", "a"]}),
        ("wait", {"timeout_secs": 1801}),
        ("extend", {"session_id": "child", "iterations": 0}),
        ("extend", {"session_id": "child", "iterations": 601}),
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
        ("cancel", {"session_id": "unowned"}, "not-owned"),
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
        return 200, [] if method == "GET" else CHILD

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        assert team.subagents() == ()
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
        return 200, [CHILD] if method == "GET" else CHILD

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        team = client.session("leader").council()
        child = team.spawn(
            "Check the SDK contract; report test evidence and remaining risks",
            iteration_budget=16,
            key="sdk-contract",
        )
        assert child.session_id == CHILD["session_id"]
        for child in team.subagents():
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
