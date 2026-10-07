"""Private reader and validator for the shared, language-neutral JSON Schemas."""

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
    "automations",
    "common",
    "config",
    "content",
    "council",
    "diff",
    "gateway",
    "improve",
    "provider",
    "rooms",
    "symbol",
    "toggle",
    "view",
)


@cache
def schema(name: str) -> dict[str, Any]:
    """Read a shipped JSON Schema. Never fetch schemas from the network."""
    if name not in _SCHEMA_NAMES:
        raise ValueError("unknown contract schema")
    return json.loads((_DATA / "schema" / f"{name}.json").read_text(encoding="utf-8"))


def definition(name: str, key: str) -> dict[str, Any]:
    """Read one named definition directly from its JSON Schema."""
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


def _problem(error) -> str:
    """One readable line for a schema error: field, rule and limit, never the value."""
    field = ".".join(str(part) for part in error.absolute_path) or "declaration"
    rule, limit = error.validator, error.validator_value
    if rule in ("maxLength", "minLength"):
        bound = "maximum" if rule == "maxLength" else "minimum"
        fix = f" Shorten the {field}." if rule == "maxLength" else ""
        return (
            f"{field} is {len(error.instance)} characters; {bound} is {limit} ({rule})."
            + fix
        )
    if rule == "required":
        missing = ", ".join(key for key in limit if key not in error.instance)
        return f"{missing} is missing (required)."
    if rule == "enum":
        return f"{field} must be one of: {', '.join(map(str, limit))} (enum)."
    if rule == "pattern":
        if (
            limit
            == definition("toggle", "contribution")["properties"]["description"][
                "pattern"
            ]
        ):
            return f"{field} must be one line without line breaks (pattern)."
        return f"{field} does not match the pattern {limit} (pattern)."
    return f"{field} breaks the {rule} rule."


def problems(name: str, definition: str, value: Any) -> list[str]:
    """Readable rule failures of `value`, one line for each. Empty when it is valid.

    Each line names the field, the rule and its limit. It never echoes the value.
    """
    if not _json_value(value):
        return ["the value is not JSON data."]
    errors = _validator(name, definition).iter_errors(value)
    return [
        _problem(error)
        for error in sorted(errors, key=lambda e: list(map(str, e.absolute_path)))
    ]


def validate(name: str, definition: str, value: Any) -> Any:
    """Validate portable JSON data, returning it or raising a payload-free ValueError.

    The error names each failed field, rule and limit, never the value. Schemas
    resolve only from the installed package. Runtime lifecycle, IO and
    authorization belong to the engine, not JSON Schema.
    """
    try:
        found = problems(name, definition, value)
    except (RecursionError, TypeError, ValueError):
        found = [""]
    if found:
        detail = " ".join(problem for problem in found if problem)
        raise ValueError(
            f"invalid {name}.{definition} contract" + (f": {detail}" if detail else "")
        )
    return value
