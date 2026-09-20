"""Canonical JSON Schemas validate payloads, not parallel vocabulary catalogs."""

import inspect
import json
from collections import Counter

import pytest
from blockether.vis import _contracts, _outside
from blockether.vis.extension import Host
from jsonschema import Draft202012Validator


def protocol_methods():
    """The declared calls on `Host`, by name."""
    return {
        name: member
        for name, member in vars(Host).items()
        if inspect.isfunction(member) and not name.startswith("_")
    }


def complete_host():
    """A host answering every operation in the Python protocol."""

    class Host:
        pass

    for name in protocol_methods():
        setattr(Host, name, lambda self, *args, **kwargs: None)
    return Host()


def test_schemas_are_the_only_contract_resources():
    assert not list(_contracts._DATA.glob("*.json"))
    assert set(_contracts._SCHEMA_NAMES) == {
        path.stem for path in (_contracts._DATA / "schema").glob("*.json")
    }
    for name in _contracts._SCHEMA_NAMES:
        source = _contracts.schema(name)
        assert source == json.loads(
            (_contracts._DATA / "schema" / f"{name}.json").read_text(encoding="utf-8")
        )
        Draft202012Validator.check_schema(source)
    for retired in ("CONTRACT", "OPS", "VERSION", "GATEWAY", "VIEW", "ACTIVITY"):
        assert not hasattr(_contracts, retired)


def test_gateway_routes_and_lease_policy_are_shared():
    gateway = _contracts.schema("gateway")
    routes = gateway["x-vis-routes"]
    operations = [
        operation for route in routes for operation in route["operations"].values()
    ]
    by_path = {route["path"]: route for route in routes}
    lease = gateway["x-vis-client-lease"]
    assert 0 < lease["touch_ms"] < lease["keepalive_ms"] < lease["ttl_ms"]
    assert 0 < lease["keepalive_timeout_ms"] < lease["keepalive_ms"]
    assert len(routes) == 126
    assert len(operations) == 155
    assert Counter(operation["request"] for operation in operations) == {
        "none": 105,
        "json": 46,
        "binary": 4,
    }
    assert by_path["/v1/speech/voices"]["operations"]["post"] == {
        "request": "binary",
        "response": "json",
    }
    assert by_path["/v1/events"]["operations"]["get"]["response"] == "sse"


def test_gateway_envelopes_validate_real_payloads():
    fields = _contracts.definition("gateway", "handshake")["properties"]
    handshake = {
        key: value["const"] for key, value in fields.items() if "const" in value
    }
    handshake.update(version="1.0.0", build="fixture")
    assert _contracts.validate("gateway", "handshake", handshake) is handshake
    with pytest.raises(ValueError):
        _contracts.validate("gateway", "handshake", {"keys": {"version": "version"}})
    error = {"error": {"type": "missing", "message": "Not found"}}
    assert _contracts.validate("gateway", "error_response", error) is error
    with pytest.raises(ValueError):
        _contracts.validate("gateway", "error_response", {"error": {"message": 7}})


def test_the_host_implements_exactly_the_protocol_operations():
    assert set(vars(_outside.host)) == set(protocol_methods())
    for name, member in protocol_methods().items():
        assert (member.__doc__ or "").strip(), name
        count = len(inspect.signature(member).parameters) - 1
        inspect.signature(getattr(_outside.host, name)).bind(*([None] * count))


def test_check_host_answers_a_complete_host():
    host = complete_host()
    assert _outside.check_host(host) is host
    assert isinstance(host, Host)


def test_check_host_names_the_ops_a_host_does_not_answer():
    class Partial:
        def state_get(self, key):
            return None

    with pytest.raises(TypeError) as raised:
        _outside.check_host(Partial())
    message = str(raised.value)
    assert "shell" in message
    assert "state_get" not in message


@pytest.mark.parametrize(
    "name",
    [
        "ops",
        "live",
        "shell",
        "version",
        "python-host",
        "../view",
        "https://example.com/schema",
    ],
)
def test_schema_refuses_unknown_names(name):
    with pytest.raises(ValueError):
        _contracts.schema(name)


def test_payload_free_validation():
    declaration = {"presenter": "tests", "label": "Checking"}
    assert _contracts.validate("activity", "declaration", declaration) is declaration
    for bad in (
        {"presenter": "unknown"},
        {"presenter": "tests", "label": "one\ntwo"},
        {"presenter": "tests", "secret": "do-not-echo"},
    ):
        with pytest.raises(ValueError) as error:
            _contracts.validate("activity", "declaration", bad)
        assert "do-not-echo" not in str(error.value)
    with pytest.raises(ValueError):
        _contracts.validate("view", "missing-definition", {})


def test_extension_declarations_use_the_canonical_config_contract():
    declaration = {
        "extensions": {
            "vis-tools": {
                "source": "https://github.com/example/vis-tools",
                "subdirectory": "extensions/vis-tools",
                "version": "0.1.0",
            }
        }
    }
    assert _contracts.validate("config", "config", declaration) is declaration
    for spec in (
        {},
        {"source": "./tools", "trust": True},
        {"source": "./tools", "revision": "main"},
        {"source": "./tools", "version": "1.0.0", "revision": "a" * 40},
    ):
        with pytest.raises(ValueError):
            _contracts.validate("config", "config", {"extensions": {"vis-tools": spec}})
