"""The model catalog: price, billing and provider limits over the canonical schema."""

import pytest
from blockether.vis.engine import GatewayClient, Model
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
