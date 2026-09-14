"""Agent callback ownership through the public SDK and real loopback HTTP."""

import gc
import json
import threading
import weakref
from urllib.parse import unquote, urlsplit

import blockether.vis.extension as vis
import pytest
from blockether.vis._contracts import GATEWAY
from blockether.vis.engine import Agent, GatewayClient, GatewayError
from test_client import compatible, endpoint


def declaration(fn, name="application"):
    return vis.Extension(
        name=name,
        description="Application-owned callbacks.",
        alias=name,
        symbols=[
            vis.Symbol(fn, activity=vis.Activity(label="Read state", show_start=False))
        ],
    )


@pytest.fixture
def gateway():
    state = {
        "sessions": [],
        "manifests": {},
        "pending": {},
        "results": {},
        "reject": False,
    }

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        parts = [unquote(part) for part in urlsplit(path).path.split("/")]
        if path == "/v1/sessions" and method == "POST":
            sid = f"session {len(state['sessions']) + 1}"
            state["sessions"].append(sid)
            return 201, {"id": sid}
        sid = parts[3]
        suffix = parts[4:]
        if suffix == ["client-extensions"]:
            if method == "PUT":
                if state["reject"]:
                    return 409, {
                        "error": {"type": "collision", "message": "Rejected append"}
                    }
                state["manifests"][sid] = json.loads(body)["extensions"]
                return 200, {"is_registered": True}
            state["manifests"].pop(sid, None)
            return 200, {"is_removed": True}
        if suffix == ["client-calls"]:
            return 200, {"calls": state["pending"].get(sid, [])}
        if len(suffix) == 3 and suffix[0] == "client-calls":
            if suffix[2] == "result":
                state["results"].setdefault(sid, []).append(json.loads(body))
                state["pending"][sid] = []
            return 200, {"is_accepted": True}
        if suffix == ["seq"]:
            return 200, {"seq": 0}
        if suffix == ["turns"]:
            return 202, {"turn_id": "turn-one", "status": "queued"}
        if suffix == ["turns", "turn-one"]:
            status = "running" if state["pending"].get(sid) else "completed"
            return 200, {"turn_id": "turn-one", "status": status, "content": []}
        if not suffix:
            return 200, {"id": sid, "title": "Application"}
        return 404, {"error": {"type": "not_found", "message": path}}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        yield client, state, calls


def queue(state, sid, name="remember", value="Ada"):
    state["pending"][sid] = [
        {"id": "call /one", "name": name, "args": [value], "kwargs": {}}
    ]


@pytest.mark.parametrize("pump", ["wait", "read"])
def test_constructor_extensions_pump_on_calling_thread_and_use_lease(gateway, pump):
    client, state, calls = gateway
    seen = []
    owner = threading.get_ident()

    def remember(value: str) -> str:
        """Remember a value in the calling application."""
        seen.append((value, threading.get_ident()))
        return value.upper()

    with Agent(
        "/work", execution_layer=client, extensions=[declaration(remember)]
    ) as agent:
        sid = agent.session.id
        queue(state, sid)
        turn = agent.send("Remember Ada")
        if pump == "wait":
            assert turn.wait(timeout=1)["status"] == "completed"
        else:
            assert agent.session.read()["id"] == sid
        assert seen == [("Ada", owner)]
        assert state["results"][sid] == [{"status": "success", "result": "ADA"}]
        callback_calls = [call for call in calls if "client-" in call[1]]
        assert any(
            "session%201/client-calls/call%20%2Fone/result" in call[1]
            for call in callback_calls
        )
        for call in callback_calls:
            headers = {key.lower(): value for key, value in call[2].items()}
            assert headers[GATEWAY["headers"]["client_id"]] == "sdk-lease"
    assert sid not in state["manifests"]
    assert not any(
        method == "DELETE" and path == "/v1/clients/sdk-lease"
        for method, path, *_ in calls
    )


@pytest.mark.parametrize("inside_context", [False, True])
def test_register_extension_before_send_and_refuse_session_send_bypass(
    gateway, inside_context
):
    client, state, calls = gateway

    def remember(value: str) -> str:
        """Return a supplied value."""
        return value

    agent = Agent("/work", execution_layer=client)
    try:
        if inside_context:
            agent.__enter__()
        agent.register_extension(declaration(remember))
        sid = agent.session.id
        assert len(state["manifests"][sid]) == 1
        agent.session.send("Start directly")
        count = len(calls)
        with pytest.raises(RuntimeError, match="first request"):
            agent.register_extension(declaration(remember, "another"))
        assert len(calls) == count
    finally:
        agent.close()


def test_shared_layer_agents_isolate_callbacks_and_close_releases_references(gateway):
    client, state, _ = gateway
    seen = []

    def make_extension(label):
        def remember(value: str) -> str:
            """Remember a value in the owning application's list."""
            seen.append((label, value))
            return label

        return declaration(remember), weakref.ref(remember)

    first_extension, first_ref = make_extension("first")
    second_extension, _ = make_extension("second")
    first = Agent("/work", execution_layer=client, extensions=[first_extension])
    second = Agent("/work", execution_layer=client, extensions=[second_extension])
    del first_extension, second_extension
    try:
        first_sid, second_sid = first.session.id, second.session.id
        first_stream = first.session.events()
        second_stream = second.session.events()
        queue(state, first_sid)
        queue(state, second_sid)
        first.session.read()
        assert seen == [("first", "Ada")]
        first.close()
        gc.collect()
        assert first_ref() is None
        assert next(first_stream, None) is None
        assert first_sid not in state["manifests"]
        assert second_sid in state["manifests"]
        second.session.read()
        assert seen == [("first", "Ada"), ("second", "Ada")]
        second_stream.close()
    finally:
        first.close()
        second.close()


def test_invalid_constructor_extension_does_not_connect():
    with endpoint(lambda *_: pytest.fail("Invalid extension must not connect")) as (
        url,
        calls,
    ):
        client = GatewayClient(url)
        try:
            with pytest.raises(TypeError, match="Extension"):
                Agent("/work", execution_layer=client, extensions=[object()])
            assert calls == []
        finally:
            client.close()


def test_failed_append_preserves_registered_callback(gateway):
    client, state, _ = gateway

    def remember(value: str) -> str:
        """Return the original registered value."""
        return value

    def additional(value: str) -> str:
        """Return an additional value."""
        return value

    with Agent(
        "/work", execution_layer=client, extensions=[declaration(remember)]
    ) as agent:
        sid = agent.session.id
        original = state["manifests"][sid]
        state["reject"] = True
        with pytest.raises(GatewayError, match="collision"):
            agent.register_extension(declaration(additional, "additional"))
        assert state["manifests"][sid] == original
        queue(state, sid)
        agent.session.read()
        assert state["results"][sid] == [{"status": "success", "result": "Ada"}]
