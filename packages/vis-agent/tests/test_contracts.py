"""Contract document and host protocol checks."""

import inspect
import json
from collections import Counter
from pathlib import Path

import pytest
from blockether.vis import _contracts, _outside
from blockether.vis.extension import Host


def protocol_methods():
    """The declared calls on `Host`, by name."""
    return {
        name: member
        for name, member in vars(Host).items()
        if inspect.isfunction(member) and not name.startswith("_")
    }


def complete_host():
    """A host answering every op the document declares, and nothing else."""

    class Host:
        pass

    for name in _contracts.OPS:
        setattr(Host, name, lambda self, *args, **kwargs: None)
    return Host()


def test_the_contract_is_read_from_canonical_documents():
    assert not (Path(_contracts.__file__).with_name("contract.json")).exists()
    for name, key in {
        "gateway": "gateway",
        "view": "view",
        "content": "content",
        "config": "config",
        "toggle": "toggle",
        "provider": "provider",
        "surface": "surface",
        "test-runner": "test_runner",
    }.items():
        document = json.loads(
            (_contracts._DATA / f"{name}.json").read_text(encoding="utf-8")
        )
        assert _contracts.CONTRACT[key] == document

    host = json.loads(
        (_contracts._DATA / "python-host.json").read_text(encoding="utf-8")
    )
    assert _contracts.VERSION == host["version"]
    assert list(_contracts.OPS.values()) == host["ops"]


def test_gateway_contract_is_whole():
    gateway = _contracts.GATEWAY
    operations = [
        operation
        for route in gateway["routes"]
        for operation in route["operations"].values()
    ]
    by_path = {route["path"]: route for route in gateway["routes"]}

    assert gateway is _contracts.CONTRACT["gateway"]
    assert gateway["version"] == 5
    lease = gateway["client_lease"]
    assert 0 < lease["touch_ms"] < lease["keepalive_ms"] < lease["ttl_ms"]
    assert 0 < lease["keepalive_timeout_ms"] < lease["keepalive_ms"]
    assert len(gateway["routes"]) == 108
    assert len(operations) == 132
    assert Counter(operation["request"] for operation in operations) == {
        "none": 92,
        "json": 36,
        "binary": 4,
    }
    assert Counter(operation["response"] for operation in operations) == {
        "json": 115,
        "resource": 2,
        "sse": 5,
        "empty": 3,
        "binary": 4,
        "negotiated": 1,
        "html": 1,
        "markdown": 1,
    }
    assert by_path["/v1/speech/voices"]["operations"]["post"] == {
        "request": "binary",
        "response": "json",
    }
    assert by_path["/v1/events"]["operations"]["get"]["response"] == "sse"
    assert gateway["events"]["session"] == sorted(gateway["events"]["session"])
    assert gateway["envelopes"]["handshake"]["keys"] == {
        "build": "build",
        "min_client": "min_client",
        "min_gateway": "min_gateway",
        "protocol": "protocol",
        "version": "version",
    }
    assert gateway["envelopes"]["error_response"]["error_keys"] == {
        "message": "message",
        "type": "type",
    }


def test_every_op_is_completely_declared():
    assert _contracts.OPS
    for name, op in _contracts.OPS.items():
        assert op["name"] == name
        assert op["global"] == f"__vis_host_{name}__"
        assert 0 <= op["arity"] <= 3
        assert op["summary"].strip()
        assert op["outside"] in {"local", "prompt", "refuse"}


def test_a_refusal_is_written_exactly_where_an_op_refuses():
    for name, op in _contracts.OPS.items():
        refuses = op["outside"] == "refuse"
        assert refuses == ("refusal" in op)
        assert refuses == (_contracts.refusal(name) is not None)
        if refuses:
            assert f"vis.{name}" in op["refusal"]


def test_the_protocol_declares_exactly_the_documents_ops():
    assert set(protocol_methods()) == set(_contracts.OPS)


def test_the_protocol_takes_the_arguments_the_document_counts():
    for name, member in protocol_methods().items():
        positional = [
            parameter
            for parameter in inspect.signature(member).parameters.values()
            if parameter.kind
            in (parameter.POSITIONAL_ONLY, parameter.POSITIONAL_OR_KEYWORD)
        ]
        assert len(positional) - 1 == _contracts.OPS[name]["arity"], name


def test_every_op_carries_its_summary_into_the_protocol():
    for name, member in protocol_methods().items():
        assert (member.__doc__ or "").strip(), name


def test_the_shell_grammar_is_a_grammar():
    shell = _contracts.SHELL
    assert shell["default_op"] in shell["spawn_ops"]
    assert not set(shell["spawn_ops"]) & set(shell["handle_ops"])
    assert "shell" in _contracts.OPS


def test_the_content_vocabulary_is_whole():
    content = _contracts.CONTENT
    assert content is _contracts.CONTRACT["content"]
    assert content["version"] == 2
    assert set(content["roles"]) == {"user", "assistant", "system", "developer", "tool"}
    assert set(content["block_types"]) == {
        "attachment",
        "code",
        "error",
        "notice",
        "prose",
        "reasoning",
        "speech",
        "tool",
    }
    assert set(content["delta_fields"]) == {"markdown", "text"}


def test_the_toggle_vocabulary_is_whole():
    toggle = _contracts.TOGGLE
    assert toggle is _contracts.CONTRACT["toggle"]
    assert toggle["version"] == 1
    assert toggle["default_type"] in toggle["types"]
    assert set(toggle["boolean_wire"]) == {"true", "false"}
    assert not set(toggle["boolean_wire"]["true"]) & set(
        toggle["boolean_wire"]["false"]
    )
    assert set(toggle["config_truthy"]) <= set(toggle["boolean_wire"]["true"])


def test_the_provider_limits_vocabulary_is_whole():
    provider = _contracts.PROVIDER
    assert provider is _contracts.CONTRACT["provider"]
    assert provider["version"] == 1
    limits = provider["limits"]
    assert set(limits) == {
        "statuses",
        "scopes",
        "kinds",
        "window_kinds",
        "window_units",
        "precisions",
        "sources",
    }
    for name, tokens in limits.items():
        assert tokens == sorted(set(tokens)), name
    # A report the host could not classify is still a report a channel must paint.
    assert {"ok", "unauthenticated", "unknown-provider"} <= set(limits["statuses"])


def test_the_view_vocabulary_is_whole():
    view = _contracts.VIEW
    assert set(view["kinds"]) == {"input", "live"}
    for key in (
        "field_types",
        "text_types",
        "choice_types",
        "secret_types",
        "decor_types",
        "group_directions",
    ):
        assert view[key], key
    assert view["group_type"]
    assert view["secret_handle_prefix"].endswith(":")
    assert view["otp"]["length"] <= view["otp"]["ceiling"]
    assert view["range"]["min"] < view["range"]["max"]
    # Every text, choice and secret type is an input field type in one View vocabulary.
    for key in ("text_types", "choice_types", "secret_types"):
        assert set(view[key]) <= set(view["field_types"]), key
    assert view["live"]["node_types"]
    assert view["live"]["ops"]


def test_every_portable_contract_area_is_exported():
    assert _contracts.CONFIG is _contracts.CONTRACT["config"]
    assert _contracts.SURFACE is _contracts.CONTRACT["surface"]
    assert _contracts.TEST_RUNNER is _contracts.CONTRACT["test_runner"]
    assert _contracts.SURFACE["capabilities"] == ["format", "lint", "test"]
    assert "paths" in _contracts.TEST_RUNNER["selector_keys"]


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
    assert str(_contracts.VERSION) in message


@pytest.mark.parametrize(
    "name",
    [
        "ops",
        "live",
        "shell",
        "version",
        "test_runner",
        "../view",
        "https://example.com/schema",
    ],
)
def test_schema_refuses_non_document_names(name):
    with pytest.raises(ValueError):
        _contracts.schema(name)


def test_activity_export_and_payload_free_validation():
    assert _contracts.ACTIVITY is _contracts.CONTRACT["activity"]
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


def test_op_answers_one_entry_or_nothing():
    assert _contracts.op("shell")["name"] == "shell"
    assert _contracts.op("detonate") is None
    assert _contracts.refusal("shell") is None
