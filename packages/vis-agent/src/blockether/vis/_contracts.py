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
    "evals",
    "gateway",
    "provider",
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


# An appended live item replaces the item with its id. The host fills these keys
# when the item leaves them out; every other required key must come with it.
_LIVE_ITEM_DEFAULTS = {"step": {"tone": "idle"}, "link": {"target_kind": "url"}}
_LIVE_ITEMS = {"stats": "stat", "steps": "step", "rows": "row", "links": "link"}


def live_items(key: str, items: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Return appended live items as the host keeps them; refuse a missing key."""
    item = _LIVE_ITEMS[key]
    required = definition("view", item)["required"]
    result = []
    for one in items:
        one = {**_LIVE_ITEM_DEFAULTS.get(item, {}), **one}
        if missing := [name for name in required if name not in one]:
            raise ValueError(
                f"{item} {one.get('id')}: {', '.join(missing)} is missing (required)"
            )
        result.append(one)
    return result


def _button_branch() -> dict[str, Any]:
    return next(
        branch
        for branch in definition("view", "component")["oneOf"]
        if branch["properties"]["type"].get("const") == "button"
    )


def button_variant_error(variant: Any) -> str | None:
    """Answer why a declared button variant is invalid, or None when it is valid."""
    allowed = _button_branch()["properties"]["variant"]["enum"]
    if variant is None or variant in allowed:
        return None
    return f"button variant must be one of {', '.join(allowed)}, not {variant!r}"


def _defaults(key: str) -> dict[str, Any]:
    properties = definition("view", key)["properties"]
    return {
        name: one["default"] for name, one in properties.items() if "default" in one
    }


def scene_meshes() -> dict[str, dict[str, Any]]:
    """Return the builtin scene meshes by name, as the schema ships them."""
    return {
        branch["const"]: {
            "vertices": branch["x-vis-vertices"],
            "faces": branch["x-vis-faces"],
        }
        for branch in definition("view", "scene_builtin_mesh")["oneOf"]
    }


def _scene_branch() -> dict[str, Any]:
    return next(
        branch
        for branch in definition("view", "component")["oneOf"]
        if branch["properties"]["type"].get("const") == "scene"
    )


def scene_objects(
    held: list[dict[str, Any]], changes: list[dict[str, Any]]
) -> list[dict[str, Any]]:
    """Merge scene object changes by id, as the host does.

    A known object keeps every key that the change leaves out. A new object
    starts from the schema defaults and needs its mesh.
    """
    defaults = _defaults("scene_object")
    merged = [dict(one) for one in held or []]
    at = {one.get("id"): index for index, one in enumerate(merged)}
    for change in changes or []:
        object_id = change.get("id")
        if object_id in at:
            merged[at[object_id]] = {**merged[at[object_id]], **change}
        elif "mesh" not in change:
            raise ValueError(f"scene object {object_id}: mesh is missing (required)")
        else:
            at[object_id] = len(merged)
            merged.append({**defaults, **change})
    return merged


def scene_node(node: dict[str, Any]) -> str | None:
    """Fill a scene node's defaults in place; answer what makes it invalid."""
    properties = _scene_branch()["properties"]
    for name in ("background", "fov", "is_wrapping", "tick_ms"):
        node.setdefault(name, properties[name]["default"])
    for name in ("width", "height"):
        size = node.get(name)
        if isinstance(size, bool) or not isinstance(size, (int, float)) or size <= 0:
            return f"a scene needs a positive {name}"
    node["meshes"] = list(node.get("meshes") or [])
    try:
        node["objects"] = scene_objects([], list(node.get("objects") or []))
    except ValueError as error:
        return str(error)
    return scene_error(node)


def scene_error(node: dict[str, Any]) -> str | None:
    """Answer what makes a scene's meshes or objects invalid, or None."""
    builtin = scene_meshes()
    meshes = node.get("meshes") or []
    objects = node.get("objects") or []
    for key, items in (("meshes", meshes), ("objects", objects)):
        ids = [item.get("id") for item in items]
        if repeated := sorted({i for i in ids if ids.count(i) > 1}, key=str):
            return f"scene {key} repeat the id {repeated[0]!r}"
        if len(items) > _scene_branch()["properties"][key]["maxItems"]:
            return f"a scene holds at most {_scene_branch()['properties'][key]['maxItems']} {key}"
    for mesh in meshes:
        if mesh.get("id") in builtin:
            return f"scene mesh {mesh.get('id')!r} shadows a builtin mesh"
        count = len(mesh.get("vertices") or [])
        if any(
            not 0 <= index < count for face in mesh.get("faces") or [] for index in face
        ):
            return f"scene mesh {mesh.get('id')!r} has a face with a missing vertex"
    known = set(builtin) | {mesh.get("id") for mesh in meshes}
    for one in objects:
        if one.get("mesh") not in known:
            return f"scene object {one.get('id')!r} has an unknown mesh {one.get('mesh')!r}"
    return None


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
