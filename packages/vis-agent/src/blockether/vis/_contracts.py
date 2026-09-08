"""Private reader and validator for the canonical, language-neutral Vis documents."""

import json
from functools import lru_cache
from pathlib import Path
from typing import Any

_DATA = Path(__file__).with_name("_data")
if not _DATA.is_dir():
    _DATA = Path(__file__).resolve().parents[4] / "vis-contract/resources/vis-contract"


def _load_document(name):
    return json.loads((_DATA / f"{name}.json").read_text(encoding="utf-8"))


_DOCUMENT_NAMES = (
    "activity",
    "council",
    "gateway",
    "view",
    "content",
    "config",
    "toggle",
    "provider",
    "surface",
    "test-runner",
)
_SCHEMA_NAMES = (*_DOCUMENT_NAMES, "python-host", "common")

_host = _load_document("python-host")
CONTRACT = {
    "version": _host["version"],
    "ops": _host["ops"],
    "shell": _host["shell"],
    "live": _host["live"],
    **{name.replace("-", "_"): _load_document(name) for name in _DOCUMENT_NAMES},
}
"""All canonical contract documents, keyed as the public Python API expects."""

ACTIVITY = CONTRACT["activity"]
"""Host-owned tool lifecycle evidence, closed vocabulary, and presentation limits."""

GATEWAY = CONTRACT["gateway"]
"""Canonical routes, headers, events, envelopes and replay semantics."""

VERSION = CONTRACT["version"]
"""Bumped whenever an op is added, removed or re-shaped."""

OPS = {entry["name"]: entry for entry in CONTRACT["ops"]}
"""Every declared op by name, in document order."""

SHELL = CONTRACT["shell"]
"""The `shell` verb's lifecycle grammar: `default_op`, `handle_ops`, `spawn_ops`."""

LIVE = CONTRACT["live"]
"""The `live` operation vocabulary and flush interval."""

VIEW = CONTRACT["view"]
"""The closed View vocabulary — lifecycle kinds, semantic nodes and their bounds."""

CONTENT = CONTRACT["content"]
"""Canonical content vocabulary."""

CONFIG = CONTRACT["config"]
"""Canonical configuration vocabulary."""

TOGGLE = CONTRACT["toggle"]
"""Canonical toggle vocabulary."""

PROVIDER = CONTRACT["provider"]
"""Canonical provider-limits vocabulary."""

SURFACE = CONTRACT["surface"]
"""Canonical language-surface vocabulary."""

TEST_RUNNER = CONTRACT["test_runner"]
"""Canonical test-runner vocabulary."""


def schema(name: str) -> dict[str, Any]:
    """Read the shipped JSON Schema; never retrieve schemas from the network."""
    if name not in _SCHEMA_NAMES:
        raise ValueError("unknown contract document")
    return _load_document("schema/" + name)


@lru_cache(maxsize=64)
def _validator(name: str, definition: str):
    from jsonschema import Draft202012Validator, FormatChecker
    from referencing import Registry, Resource

    source = schema(name)
    if definition not in source.get("$defs", {}):
        raise ValueError("unknown contract definition")
    registry = Registry().with_resources(
        (document["$id"], Resource.from_contents(document))
        for document in (schema(name) for name in _SCHEMA_NAMES)
    )
    target = {key: source[key] for key in ("$schema", "$id", "$defs")}
    target["$ref"] = "#/$defs/" + definition
    return Draft202012Validator(
        target, registry=registry, format_checker=FormatChecker()
    )


def _json_value(item):
    import math

    if item is None or type(item) in (str, bool, int):
        return True
    if type(item) is float:
        return math.isfinite(item)
    if type(item) is list:
        return all(_json_value(child) for child in item)
    return type(item) is dict and all(
        type(k) is str and _json_value(v) for k, v in item.items()
    )


def validate(name: str, definition: str, value: Any) -> Any:
    """Validate portable JSON data, returning it or raising a payload-free ValueError.

    Schemas resolve only from the installed package. Runtime lifecycle, IO and
    authorization belong to the engine, not JSON Schema.
    """
    try:
        valid = _json_value(value) and _validator(name, definition).is_valid(value)
    except (RecursionError, TypeError, ValueError):
        valid = False
    if not valid:
        raise ValueError(f"invalid {name}.{definition} contract")
    return value


def op(name):
    """The declared op called `name`, or None."""
    return OPS.get(name)


def refusal(name):
    """The refusal message for an op unavailable outside Vis, if any."""
    return OPS.get(name, {}).get("refusal")
