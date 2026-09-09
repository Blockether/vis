"""Explicit goals reuse the existing turn boundary, never a second goal API."""

import json

import pytest
from blockether.vis.engine import GatewayClient
from test_client import compatible, endpoint


@pytest.mark.parametrize("budget", [None, 40])
def test_goal_uses_slash_turn(budget):
    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path.endswith("/seq"):
            return 200, {"seq": 3}
        if path.endswith("/turns"):
            payload = json.loads(body)
            prefix = "/goal -- " if budget is None else f"/goal --budget {budget} -- "
            assert payload["request"] == prefix + '--pause "quoted"\nsecond line'
            assert payload["idempotency_key"] == "goal-request"
            return 202, {"turn_id": "goal-turn", "status": "queued"}
        return 404, {}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        session = client.session("s1")
        turn = session.goal(
            '  --pause "quoted"\nsecond line  ',
            token_budget=budget,
            idempotency_key="goal-request",
        )
        assert turn.id == "goal-turn"
        assert not hasattr(session, "get_goal")
        assert not hasattr(session, "update_goal")
        assert not any("/goals" in call[1] for call in calls)


@pytest.mark.parametrize(
    "objective,budget",
    [
        ("", None),
        (" ", None),
        ("x" * 8193, None),
        (None, None),
        ("work", 0),
        ("work", -1),
        ("work", True),
        ("work", 1.5),
        ("work", 2**63),
    ],
)
def test_goal_validation_happens_before_network(objective, budget):
    client = GatewayClient("http://127.0.0.1:1")
    try:
        with pytest.raises(ValueError):
            client.session("s1").goal(objective, token_budget=budget)
    finally:
        client.close()


@pytest.mark.parametrize(
    "goal",
    [
        None,
        {
            "version": 1,
            "id": "goal-fixture",
            "objective": "Verify canonical session data",
            "status": "paused",
            "token_budget": 100000,
            "tokens_used": 12400,
            "time_used_ms": 32000,
            "revision": 3,
            "created_at": 1,
            "updated_at": 2,
            "reason": None,
        },
    ],
)
def test_goal_is_part_of_session_data(goal):
    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if method == "GET" and path == "/v1/sessions/s1":
            return 200, {"id": "s1", "goal": goal}
        if method == "GET" and path == "/v1/sessions":
            return 200, {"sessions": [{"id": "s1", "goal": goal}]}
        return 404, {}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        assert client.session("s1").read()["goal"] == goal
        assert client.list_sessions()["sessions"][0]["goal"] == goal
        assert not any("/goals" in call[1] for call in calls)
