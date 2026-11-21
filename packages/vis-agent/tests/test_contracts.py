"""Canonical JSON Schemas validate payloads, not parallel vocabulary catalogs."""

import ast
import inspect
import json
import keyword
import re
import textwrap
from collections import Counter
from pathlib import Path

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
    assert len(routes) == 179
    assert len(operations) == 218
    assert Counter(operation["request"] for operation in operations) == {
        "none": 132,
        "json": 80,
        "binary": 6,
    }
    assert by_path["/v1/decisions/models"]["operations"]["get"] == {
        "request": "none",
        "response": "json",
        "response_schema": "decision_models",
    }
    assert by_path["/v1/decisions/models"]["operations"]["post"] == {
        "request": "binary",
        "response": "json",
        "response_schema": "decision_model",
    }
    assert by_path["/v1/decisions/models/:model-ref"]["operations"]["get"] == {
        "request": "none",
        "response": "json",
        "response_schema": "decision_model",
    }
    assert by_path["/v1/decisions/aliases/:alias"]["operations"]["put"] == {
        "request": "json",
        "response": "json",
        "response_schema": "decision_alias",
    }
    assert by_path["/v1/decisions/training/jobs"]["operations"]["post"] == {
        "request": "json",
        "response": "json",
        "response_schema": "decision_job",
    }
    assert by_path["/v1/decisions/training/jobs/:job-id"]["operations"]["delete"] == {
        "request": "none",
        "response": "json",
        "response_schema": "decision_job",
    }
    assert by_path["/v1/systemone"]["operations"]["post"] == {
        "request": "json",
        "response": "json",
        "response_schema": "decision_answer",
    }
    assert by_path["/v1/speech/voices"]["operations"]["post"] == {
        "request": "binary",
        "response": "json",
        "response_schema": "speech_voice",
        "extension": "callback",
    }
    assert by_path["/v1/speech/voices/:voice-id"]["operations"]["patch"] == {
        "request": "json",
        "response": "json",
        "response_schema": "speech_voice",
        "extension": "callback",
    }
    assert by_path["/v1/settings"]["operations"]["patch"] == {
        "request": "json",
        "response": "json",
        "request_schema": "settings_batch",
        "response_schema": "settings",
    }
    assert by_path["/v1/extensions/reload"]["operations"]["post"] == {
        "request": "json",
        "response": "json",
        "request_schema": "settings_target",
        "response_schema": "extension_reload",
    }
    assert by_path["/v1/events"]["operations"]["get"]["response"] == "sse"
    assert by_path["/v1/sessions/:sid/voice/live/:vid/media"]["operations"]["get"] == {
        "request": "none",
        "response": "websocket",
    }


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
    with pytest.raises(
        ValueError, match=r": label does not match the pattern .* \(pattern\)"
    ):
        _contracts.validate(
            "activity", "declaration", {"presenter": "tests", "label": "one\ntwo"}
        )
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


@pytest.mark.parametrize(
    "block",
    [
        {
            "id": "c",
            "type": "conversation",
            "text": "The work is ready.",
            "phase": "answer",
        },
        {
            "id": "p",
            "type": "conversation",
            "text": "I will check.",
            "phase": "progress",
        },
        {"id": "v", "type": "visual", "markdown": "```mermaid\ngraph TD; A-->B\n```"},
        {"id": "s", "type": "silence", "reason": "user_requested"},
    ],
)
def test_handsfree_content_resolves_the_registered_voice_schema(block):
    assert _contracts.validate("content", "block", block) == block


@pytest.mark.parametrize(
    "text", ["", " ", "x" * 401, "**Markup**", "https://example.com"]
)
def test_handsfree_conversation_rejects_invalid_speech(text):
    with pytest.raises(ValueError, match="invalid content.block contract"):
        _contracts.validate(
            "content",
            "block",
            {"id": "c", "type": "conversation", "text": text, "phase": "answer"},
        )


# Generated SDK response types (#487).

_SCALARS = {
    "string": "str",
    "integer": "int",
    "number": "float",
    "boolean": "bool",
    "null": "None",
}


def _camel(name):
    return "".join(
        part[:1].upper() + part[1:] for part in re.split(r"[^0-9A-Za-z]+", name) if part
    )


_ENGINE = Path(__file__).parents[1] / "src/blockether/vis/engine"


def _sdk_classes():
    """The class names that the engine modules define, without the generated types."""
    return {
        node.name
        for module in sorted(_ENGINE.glob("*.py"))
        if module.name != "_types.py"
        for node in ast.parse(module.read_text(encoding="utf-8")).body
        if isinstance(node, ast.ClassDef)
    }


class _Renderer:
    """Renders gateway response schemas as Python TypedDict source."""

    def __init__(self, gateway, documents):
        self.gateway = gateway
        self.documents = documents
        self.blocks = {}
        self.order = []
        self.document = None
        self.classes = _sdk_classes()

    def written(self, title):
        """The name that the source gives `title`. An SDK class name gets `Record`."""
        return title + "Record" if title in self.classes else title

    def target(self, ref):
        document, _, pointer = ref.partition("#")
        document = document or self.document
        node = self.documents[document] if document else self.gateway
        for part in pointer.strip("/").split("/"):
            node = node[part]
        return node

    def type_of(self, node, hint):
        if node is True or not node:
            return "JSONValue"
        if "$ref" in node:
            ref = node["$ref"]
            document, _, pointer = ref.partition("#")
            if pointer.startswith("/$defs/") and "/" not in pointer[7:]:
                return self.named(pointer[7:], document or self.document)
            return self.foreign(self.target(ref))
        if isinstance(node.get("const"), str):
            return f"Literal[{json.dumps(node['const'])}]"
        if "const" in node:
            return _SCALARS.get(
                type(node["const"]).__name__.replace("int", "integer"), "JSONValue"
            )
        if "enum" in node:
            if all(isinstance(value, str) for value in node["enum"]):
                return (
                    "Literal["
                    + ", ".join(json.dumps(value) for value in node["enum"])
                    + "]"
                )
            return "JSONValue"
        for key in ("anyOf", "oneOf"):
            if key in node:
                return self.union(
                    self.type_of(
                        branch, hint + (str(index + 1) if len(node[key]) > 1 else "")
                    )
                    for index, branch in enumerate(node[key])
                )
        kind = node.get("type")
        if isinstance(kind, list):
            return self.union(self.type_of({**node, "type": one}, hint) for one in kind)
        if kind == "array":
            return f"list[{self.type_of(node.get('items', {}), hint + 'Item')}]"
        if kind == "object":
            if node.get("properties"):
                return self.typed_dict(hint, node)
            return "dict[str, JSONValue]"
        return _SCALARS.get(kind, "JSONValue")

    def foreign(self, node):
        """A definition of another schema document, as its plain JSON shape."""
        kind = node.get("type")
        if kind == "array":
            return "list[dict[str, JSONValue]]"
        if kind == "object":
            return "dict[str, JSONValue]"
        return "JSONValue"

    def union(self, members):
        unique = []
        for member in members:
            for part in member.split(" | "):
                if part not in unique:
                    unique.append(part)
        if "JSONValue" in unique:
            return "JSONValue"
        literals = [part[8:-1] for part in unique if part.startswith("Literal[")]
        others = [part for part in unique if not part.startswith("Literal[")]
        if len(literals) > 1:
            others.insert(0, "Literal[" + ", ".join(literals) + "]")
        else:
            others = unique
        return " | ".join(others)

    def named(self, name, document=None):
        """The type name of definition `name`; another document prefixes its name."""
        prefix = document.removesuffix(".json") + "_" if document else ""
        title = _camel(prefix + name)
        source = self.documents[document] if document else self.gateway
        node = source["$defs"][name]
        target, _, pointer = node.get("$ref", "").partition("#")
        if (
            target
            and set(node) == {"$ref"}
            and _camel(target.removesuffix(".json") + "_" + pointer[7:]) == title
        ):
            # A wrapper of a definition with the same name is that definition.
            return self.named(pointer[7:], target)
        if title not in self.blocks:
            self.blocks[title] = None
            outer, self.document = self.document, document
            try:
                if node.get("type") == "object" and node.get("properties"):
                    self.typed_dict(title, node)
                else:
                    rendered = self.type_of(node, title + "Variant")
                    self.blocks[title] = _wrapped(
                        f"{self.written(title)}: TypeAlias = ", rendered, ""
                    )
                    self.order.append(title)
            finally:
                self.document = outer
        return self.written(title)

    def typed_dict(self, title, node):
        name = self.written(title)
        if self.blocks.get(title):
            return name
        self.blocks[title] = "pending"
        required = set(node.get("required", []))
        rows = []
        for key, value in node["properties"].items():
            kind = self.type_of(value, title + _camel(key))
            rows.append((key, kind if key in required else f"NotRequired[{kind}]"))
        doc = node.get("description", "").strip()
        if any(not key.isidentifier() or keyword.iskeyword(key) for key, _ in rows):
            fields = "".join(
                f"        {json.dumps(key)}: {json.dumps(kind)},\n"
                for key, kind in rows
            )
            text = f"{name} = TypedDict(\n    {json.dumps(name)},\n    {{\n{fields}    }},\n)\n"
        else:
            body = "".join(_wrapped(f"{key}: ", kind, "    ") for key, kind in rows)
            text = f"class {name}(TypedDict):\n"
            if doc:
                lines = textwrap.wrap(doc, 80)
                text += (
                    '    """'
                    + "\n    ".join(lines)
                    + ("\n    " if len(lines) > 1 else "")
                    + '"""\n\n'
                )
            text += body
        self.blocks[title] = text
        self.order.append(title)
        return name


def _wrapped(prefix, kind, indent):
    """One annotated line, split in the ruff style when it is too long."""
    line = f"{indent}{prefix}{kind}"
    if len(line) <= 88:
        return line + "\n"
    if "Literal[" not in kind:
        rows = f"\n{indent}    | ".join(kind.split(" | "))
        return f"{indent}{prefix}(\n{indent}    {rows}\n{indent})\n"
    head, _, rest = kind.partition("Literal[")
    values, _, tail = rest.partition("]")
    inner = indent + "    "
    if head.startswith("NotRequired[") and tail == "]":
        inner += "    "
        rows = "".join(f"{inner}{value},\n" for value in values.split(", "))
        return f"{indent}{prefix}NotRequired[\n{indent}    Literal[\n{rows}{indent}    ]\n{indent}]\n"
    if head == "NotRequired[list[" and tail == "] | None]":
        inner += "        "
        rows = "".join(f"{inner}{value},\n" for value in values.split(", "))
        return (
            f"{indent}{prefix}NotRequired[\n{indent}    list[\n{indent}        Literal[\n"
            f"{rows}{indent}        ]\n{indent}    ]\n{indent}    | None\n{indent}]\n"
        )
    rows = "".join(f"{inner}{value},\n" for value in values.split(", "))
    return f"{indent}{prefix}{head}Literal[\n{rows}{indent}]{tail}\n"


def render_types(gateway, documents):
    """The source of `engine/_types.py` for the gateway response schemas."""
    renderer = _Renderer(gateway, documents)
    routes = {}
    for route in gateway["x-vis-routes"]:
        for method, operation in route["operations"].items():
            if "response_schema" in operation:
                name = renderer.named(operation["response_schema"])
                routes[f"{method.upper()} {route['path']}"] = name
    head = (
        '"""Generated gateway response types. Do not edit.\n\n'
        "The `response_schema` of each route in `vis-contract/schema/gateway.json` gives\n"
        "one type here. Run `python tests/test_contracts.py` in `packages/vis-agent` to\n"
        'write this file again.\n"""\n\n'
        "from __future__ import annotations\n\n"
        "from typing import TYPE_CHECKING, Literal, NotRequired, TypeAlias, TypedDict\n\n"
        "JSONValue: TypeAlias = (\n"
        '    str | int | float | bool | None | list["JSONValue"] | dict[str, "JSONValue"]\n'
        ")\n"
    )
    events = _EventRenderer(renderer).render()
    blocks = "\n\n".join(renderer.blocks[title] for title in renderer.order)
    table = "".join(
        f"    {json.dumps(key)}: {json.dumps(value)},\n"
        for key, value in sorted(routes.items())
    )
    return (
        f"{head}\n\n{blocks}\n\n"
        "RESPONSE_TYPES: dict[str, str] = {\n" + table + "}\n"
        '"""The response type name of each `METHOD path`."""\n'
        f"\n\n{events}"
    )


# The keys that `Event.from_wire` keeps out of `Event.data`.
_EVENT_ENVELOPE = ("type", "session_id", "seq", "cursor", "turn_id")
_VIEW_FIELDS = ("kind", "view_id", "view", "patch", "result", "first_seq")


class _EventRenderer:
    """Renders one `Event` type for each session event type (#487).

    Each type gets a TypedDict of its `Event.data` and an `Event` subclass for
    type checkers only, so a check of `event.type` narrows `event.data`.
    """

    def __init__(self, renderer):
        self.renderer = renderer

    def resolve(self, node):
        """The payload object schema of `node` and its document."""
        document = None
        while "$ref" in node:
            target, _, _ = node["$ref"].partition("#")
            document = target or document
            self.renderer.document, outer = document, self.renderer.document
            try:
                node = self.renderer.target(node["$ref"])
            finally:
                self.renderer.document = outer
        return node, document

    def data_type(self, event_type, payload_name):
        node, document = self.resolve(self.renderer.gateway["$defs"][payload_name])
        branches = node.get("oneOf", [node])
        properties, required = {}, None
        for branch in branches:
            properties.update(branch.get("properties", {}))
            names = set(branch.get("required", []))
            required = names if required is None else required & names
        dropped = set(_EVENT_ENVELOPE)
        if event_type == "block.activity":
            dropped.add("activity")
        if event_type.startswith("view."):
            dropped.update(_VIEW_FIELDS)
        kept = {key: value for key, value in properties.items() if key not in dropped}
        if event_type != "subscription.ready":
            kept |= {"schema": {"type": "integer"}, "ts": {"type": "integer"}}
            required = (required or set()) | {"schema", "ts"}
        title = _camel(event_type) + "Data"
        assert title not in self.renderer.blocks, title
        self.renderer.document, outer = document, self.renderer.document
        try:
            title = self.renderer.typed_dict(
                title,
                {
                    "description": f"The `data` of an event of type `{event_type}`.",
                    "properties": kept,
                    "required": sorted(required or ()),
                },
            )
        finally:
            self.renderer.document = outer
        return title, "turn_id" in (required_of(node) & set(properties))

    def render(self):
        variants = self.renderer.gateway["$defs"]["session_event_type"]["oneOf"]
        classes, names = [], []
        for variant in variants:
            event_type = variant["const"]
            data, has_turn = self.data_type(event_type, variant["x-vis-payload"])
            name = _camel(event_type) + "Event"
            assert name not in self.renderer.blocks, name
            rows = [f"type: Literal[{json.dumps(event_type)}]"]
            rows.append(
                "cursor: int" if event_type == "subscription.ready" else "seq: int"
            )
            if has_turn:
                rows.append("turn_id: str")
            rows.append(f"data: {data}")
            if event_type == "block.activity":
                rows.append("activity: ActivityProjection")
            if event_type.startswith("view."):
                rows.append("view: ViewEvent")
            body = "".join(f"        {row}\n" for row in rows)
            classes.append(
                f"    class {name}(Event):\n"
                f'        """An event of type `{event_type}`."""\n\n{body}'
            )
            names.append(name)
        union = "".join(f"        | {name}\n" for name in names)[10:]
        return (
            "if TYPE_CHECKING:\n"
            "    from blockether.vis.activity import ActivityProjection\n"
            "    from blockether.vis.engine._client import Event\n"
            "    from blockether.vis.views import ViewEvent\n\n"
            + "\n".join(classes)
            + "\n    StreamEvent: TypeAlias = (\n        "
            + union
            + "    )\n"
            '    """An event of a session stream. Check `type` to narrow `data`."""\n'
        )


def required_of(node):
    """The keys that each branch of `node` requires."""
    required = None
    for branch in node.get("oneOf", [node]):
        names = set(branch.get("required", []))
        required = names if required is None else required & names
    return required or set()


_TYPES = Path(__file__).parents[1] / "src/blockether/vis/engine/_types.py"


def _rendered_types():
    """The `engine/_types.py` source for the shipped gateway schema."""
    documents = {
        f"{name}.json": _contracts.schema(name) for name in _contracts._SCHEMA_NAMES
    }
    return render_types(_contracts.schema("gateway"), documents)


def test_response_types_are_current_with_the_gateway_schema():
    # A route response schema that changes without this module gives the SDK stale types.
    assert _TYPES.read_text(encoding="utf-8") == _rendered_types(), (
        "Run `python tests/test_contracts.py` in packages/vis-agent."
    )


def test_each_json_route_has_a_response_type():
    from blockether.vis.engine import _types

    for route in _contracts.schema("gateway")["x-vis-routes"]:
        for method, operation in route["operations"].items():
            if operation["response"] == "json":
                name = _types.RESPONSE_TYPES[f"{method.upper()} {route['path']}"]
                assert hasattr(_types, name), name


if __name__ == "__main__":
    _TYPES.write_text(_rendered_types(), encoding="utf-8")
