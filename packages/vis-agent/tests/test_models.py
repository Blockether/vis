"""The model catalog: price, billing and provider limits over the canonical schema."""

import json

import pytest
from blockether.vis.engine import GatewayClient, Model, Session, SessionModel
from pydantic import BaseModel
from test_client import compatible, endpoint


def report(provider, status="ok"):
    return {
        "provider_id": provider,
        "status": status,
        "fetched_at_ms": 1,
        "static": {},
        "dynamic": {"limits": []},
    }


def model(provider, name, *, is_available, pricing=None):
    return {
        "provider": provider,
        "name": name,
        "label": name.title(),
        "pricing": pricing,
        "pricing_source": "catalog" if pricing else None,
        "billing": "metered",
        "context": 200000,
        "is_reasoning": True,
        "reasoning_options": ["low", "high"],
        "is_tool_call": True,
        "is_image_input": False,
        "modalities": {"input": ["text"], "output": ["text"]},
        "is_available": is_available,
        "is_default": False,
        "limits": report(provider),
    }


PRICING = {
    "input": 3.0,
    "output": 15.0,
    "tiers": [{"input": 6.0, "output": 22.5, "above_tokens": 200000}],
}
CATALOG = {
    "providers": [],
    "models": [
        model("alpha", "large", is_available=True, pricing=PRICING),
        model("beta", "small", is_available=False),
    ],
}


def respond(method, path, body):
    if result := compatible(method, path, body):
        return result
    if path.startswith("/v1/models"):
        return 200, CATALOG
    if path.startswith("/v1/providers/alpha/limits"):
        return 200, report("alpha", "unauthenticated")
    return 404, {"error": "missing"}


def test_models_returns_typed_catalog_with_filters():
    with endpoint(respond) as (url, _):
        with GatewayClient(url) as client:
            models = client.models()
            assert [(m.provider, m.name) for m in models] == [
                ("alpha", "large"),
                ("beta", "small"),
            ]
            assert all(isinstance(m, Model) for m in models)
            assert models[0].pricing["tiers"][0]["above_tokens"] == 200000
            assert models[0].reasoning_options == ("low", "high")
            assert models[0].limits["provider_id"] == "alpha"
            assert models[1].pricing is None
            assert [m.name for m in client.models(provider="beta")] == ["small"]
            assert [m.name for m in client.models(is_available=True)] == ["large"]
            assert client.models(provider="alpha", is_available=False) == ()
            assert client.model_limits("alpha")["status"] == "unauthenticated"


def test_model_rejects_a_catalog_row_outside_the_schema():
    row = model("alpha", "large", is_available=True)
    assert Model.from_wire(row).billing == "metered"
    with pytest.raises(ValueError):
        Model.from_wire({**row, "billing": "invented"})
    with pytest.raises(ValueError):
        Model.from_wire({**row, "pricing": {"input": 1.0}})


def session_respond(method, path, raw):
    if result := compatible(method, path, raw):
        return result
    body = json.loads(raw) if raw else {}
    route = path.split("?")[0]
    if route == "/v1/sessions" and method == "POST":
        return 201, {"id": "s1"}
    if route == "/v1/sessions/s1/model":
        pin = (
            {"provider": body["provider"], "model": body["model"]}
            if method == "PATCH" and body["model"]
            else None
        )
        return 200, {"model": pin, "default": {"provider": "alpha", "model": "large"}}
    if route == "/v1/sessions/s1/forks":
        return 200, {"session": {"id": "s2"}}
    return 404, {"error": "missing"}


def test_session_model_pin_and_fork():
    with endpoint(session_respond) as (url, calls):
        with GatewayClient(url) as client:
            session = client.create_session(provider="beta", model="small")
            assert session.id == "s1"
            created = next(call for call in calls if call[1] == "/v1/sessions")
            assert {"provider": "beta", "model": "small"}.items() <= json.loads(
                created[3]
            ).items()
            assert session.model() == SessionModel(
                "alpha", "large", False, "alpha", "large"
            )
            pinned = session.set_model("beta", "small")
            assert (pinned.provider, pinned.model, pinned.is_pinned) == (
                "beta",
                "small",
                True,
            )
            assert session.set_model(None, None).is_pinned is False
            assert session.fork().id == "s2"
            session.fork("t1")
            assert json.loads(calls[-1][3]) == {"through_turn_id": "t1"}
            with pytest.raises(ValueError):
                session.set_model("beta", None)
            with pytest.raises(ValueError):
                client.create_session(model="small")


class Answer(BaseModel):
    text: str


def test_session_run_waits_on_the_same_conversation(monkeypatch):
    sent = []

    class Done:
        def wait(self, timeout):
            return {"status": "completed", "content": [], "turn_id": "t1"}

    def send(self, request, **options):
        sent.append((self.id, request, options))
        return Done()

    monkeypatch.setattr(Session, "send", send)
    session = GatewayClient("http://127.0.0.1:9").session("s1")
    assert session.run("Hello", model="small")["status"] == "completed"
    assert sent == [("s1", "Hello", {"model": "small"})]
    with pytest.raises(ValueError):
        session.run("Hello", response_model=Answer, max_corrections=-1)
    assert len(sent) == 1
