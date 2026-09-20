"""Private reader and validator for the canonical, language-neutral JSON Schemas."""

import json
from functools import cache, lru_cache
from pathlib import Path
from typing import Any

_DATA = Path(__file__).with_name("_data")
if not _DATA.is_dir():
    _DATA = Path(__file__).resolve().parents[4] / "vis-contract/resources/vis-contract"

_SCHEMA_NAMES = (
    "activity",
    "agents",
    "common",
    "config",
    "content",
    "council",
    "diff",
    "gateway",
    "improve",
    "plans",
    "provider",
    "symbol",
    "toggle",
    "view",
)


@cache
def schema(name: str) -> dict[str, Any]:
    """Read a shipped JSON Schema; never retrieve schemas from the network."""
    if name not in _SCHEMA_NAMES:
        raise ValueError("unknown contract schema")
    return json.loads((_DATA / "schema" / f"{name}.json").read_text(encoding="utf-8"))


def definition(name: str, key: str) -> dict[str, Any]:
    """Read one named definition directly from its canonical JSON Schema."""
    return schema(name)["$defs"][key]


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
