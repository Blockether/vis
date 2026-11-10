"""Private reader and validator for the shared, language-neutral JSON Schemas."""

import json
import math
import re
import uuid
from functools import cache, lru_cache
from pathlib import Path
from typing import Any, NamedTuple

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
    "keymap",
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


def rule(name: str, *path: str | int) -> Any:
    """Read the rule at `path` below the `$defs` of a schema.

    A `$ref` to the same schema gives the rule that it names.
    """
    root = schema(name)
    node: Any = root["$defs"]
    for key in path:
        node = node[key]
        while isinstance(node, dict) and str(node.get("$ref")).startswith("#/"):
            target: Any = root
            for part in node["$ref"][2:].split("/"):
                target = target[int(part) if isinstance(target, list) else part]
            node = target
    return node


def activity_block(kind: str) -> dict[str, Any]:
    """The rule of each field of one Activity content block, by block type."""
    return next(
        branch["properties"]
        for branch in rule("activity", "content", "items", "oneOf")
        if branch["properties"]["type"]["const"] == kind
    )


def activity_text_bytes() -> int:
    """The UTF-8 byte bound of an Activity headline, summary, outcome and aside."""
    return rule("activity", "section", "properties", "summary")["x-vis-max-bytes"]


def valid_handle_id(value: Any) -> bool:
    """Whether `value` is the handle id of an Activity presentation."""
    limits = definition("activity", "handle_id")
    return (
        isinstance(value, str)
        and bool(value.strip())
        and len(value) <= limits["maxLength"]
        and len(value.encode("utf-8")) <= limits["x-vis-max-bytes"]
        and not any(ord(c) < 32 or ord(c) in (127, 8232, 8233) for c in value)
    )


def api_styles() -> list[str]:
    """Each name of a provider wire dialect that the config schema takes."""
    return [
        alias
        for branch in definition("config", "apiStyle")["oneOf"]
        for alias in branch["enum"]
    ]


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


def lanes_error(lanes: Any) -> str | None:
    """Answer what makes the lanes of a timeline invalid, or None when they are valid.

    The lines are the shared node check of a timeline that holds `lanes`, as every
    host gives them (`live-view-checks.json`).
    """
    items = list(lanes) if isinstance(lanes, (list, tuple)) else lanes
    node = {"id": "timeline", "type": "timeline", **_filled_op({"lanes": items})}
    lines = live_node_lines(fill_live_node(node))
    return joined_lines(lines) if lines else None


def checked_lanes(lanes: Any) -> list[dict[str, Any]]:
    """Return timeline lanes with their defaults filled; refuse invalid lanes.

    Every host and builder calls this one check, so they refuse the same lanes.
    """
    if not isinstance(lanes, (list, tuple)) or not all(
        isinstance(lane, dict) for lane in lanes
    ):
        raise ValueError("timeline lanes must be a list of maps")
    filled = live_items("lanes", [dict(lane) for lane in lanes])
    if complaint := lanes_error(filled):
        raise ValueError(complaint)
    return filled


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


@lru_cache(maxsize=128)
def _validator(name: str, definition: str):
    from jsonschema import Draft202012Validator, FormatChecker
    from referencing import Registry, Resource

    source = schema(name)
    # A definition may name a part of one, for example `component/oneOf/3`.
    if definition.split("/")[0] not in source.get("$defs", {}):
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


# -- App debug log ---------------------------------------------------------------
# The Python form of `com.blockether.vis.contract.app/append-log`. The test
# recorder and the outside-Vis transcript keep a log as the engine keeps it.


def clip(text: str, limit: int, *, is_detail: bool = False) -> str:
    """`text` cut to `limit` code points, as the engine cuts a log entry.

    A message keeps its start and ends with an ellipsis. A detail keeps its start
    and its end, joined by a line with an ellipsis, so a stack trace keeps both
    its first frame and its cause.
    """
    if len(text) <= limit:
        return text
    if not is_detail:
        return text[: limit - 1] + "\u2026"
    start = (limit - 3) // 2
    return text[:start] + "\n\u2026\n" + text[len(text) - (limit - 3 - start) :]


def log_entry_error(view: dict[str, Any], entry: dict[str, Any]) -> str | None:
    """Answer why `entry` cannot go into the log of `view`, or None.

    Only an app takes a log entry, and an entry needs a known level and a message.
    """
    levels = definition("view", "log_level")["enum"]
    if view.get("placement") != "attachment":
        return "only an app (placement attachment) takes a log entry"
    if entry.get("level") not in levels:
        return "a log entry needs a level: " + ", ".join(sorted(levels))
    message = entry.get("message")
    if not isinstance(message, str) or not message.strip():
        return "a log entry needs a message"
    detail = entry.get("detail")
    if detail is not None and not isinstance(detail, str):
        return "the detail of a log entry must be text"
    return None


def cut_log_texts(op: Any) -> Any:
    """Log op `op` with its texts inside the entry bounds, as the engine cuts them
    before it checks a patch: the message is trimmed and cut, a blank detail is
    dropped and a long detail loses its middle. Another value stays as it is."""
    if not isinstance(op, dict) or op.get("op") != "log":
        return op
    op = dict(op)
    if isinstance(op.get("message"), str):
        op["message"] = clip(op["message"].strip(), _log_rule("message")["maxLength"])
    detail = op.get("detail")
    if isinstance(detail, str) and not detail.strip():
        op.pop("detail")
    elif isinstance(detail, str):
        op["detail"] = clip(detail, _log_rule("detail")["maxLength"], is_detail=True)
    return op


def append_log(
    view: dict[str, Any], entry: dict[str, Any], *, at: int, source: str
) -> dict[str, Any]:
    """Put `entry` at the end of the log of app `view` and answer the new entry.

    The entry takes the next `seq`. A long message or detail is cut, so an entry
    is never lost. The log keeps only its newest entries. Raises ValueError for
    an entry that the engine refuses.
    """
    complaint = log_entry_error(view, entry)
    sources = _log_rule("source")["enum"]
    if complaint is None and source not in sources:
        complaint = "a log entry needs a source: " + ", ".join(sorted(sources))
    if complaint is not None:
        raise ValueError(complaint)
    log = list(view.get("log") or [])
    added = {
        "seq": int(log[-1]["seq"] if log else 0) + 1,
        "at": int(at),
        "level": entry["level"],
        "source": source,
        "message": clip(entry["message"].strip(), _log_rule("message")["maxLength"]),
    }
    detail = entry.get("detail")
    if isinstance(detail, str) and detail.strip():
        added["detail"] = clip(detail, _log_rule("detail")["maxLength"], is_detail=True)
    log.append(added)
    view["log"] = log[
        -definition("view", "live_view")["properties"]["log"]["maxItems"] :
    ]
    return added


def _log_rule(key: str) -> dict[str, Any]:
    return definition("view", "log_entry")["properties"][key]


# -- Pictures --------------------------------------------------------------------
# The Python form of `com.blockether.vis.contract.view/read-picture`. The magic
# bytes and the bounds come from `$defs.picture`, so the SDK and the engine judge
# a picture the same way.


def picture_type(data: bytes) -> str | None:
    """The media type whose `x-vis-magic` the first bytes of `data` match, or None."""
    for media_type in definition("view", "picture")["x-vis-magic"]:
        if matches_magic(data, media_type):
            return media_type
    return None


def matches_magic(data: bytes, media_type: str) -> bool:
    """True when `data` starts like one magic of `media_type` in the contract."""
    choices = definition("view", "picture")["x-vis-magic"].get(media_type) or []
    return any(
        all(
            data[part["offset"] : part["offset"] + len(part["hex"]) // 2]
            == bytes.fromhex(part["hex"])
            for part in choice
        )
        for choice in choices
    )


def picture_size(data: bytes, media_type: str) -> tuple[int, int] | None:
    """`(width, height)` from the header of picture `data`, or None when it does not say."""
    import struct

    try:
        if media_type == "image/png":
            return struct.unpack(">II", data[16:24]) if data[12:16] == b"IHDR" else None
        if media_type == "image/gif":
            return struct.unpack("<HH", data[6:10])
        if media_type == "image/webp":
            return _webp_size(data)
        if media_type == "image/jpeg":
            return _jpeg_size(data)
    except (struct.error, IndexError):
        return None
    return None


def _webp_size(data: bytes) -> tuple[int, int] | None:
    import struct

    chunk = data[12:16]
    if chunk == b"VP8 ":
        width, height = struct.unpack("<HH", data[26:30])
        return width & 0x3FFF, height & 0x3FFF
    if chunk == b"VP8L":
        b0, b1, b2, b3 = data[21:25]
        width = 1 + (((b1 & 0x3F) << 8) | b0)
        height = 1 + (((b3 & 0x0F) << 10) | (b2 << 2) | ((b1 & 0xC0) >> 6))
        return width, height
    if chunk == b"VP8X" and len(data) >= 30:
        return (
            1 + int.from_bytes(data[24:27], "little"),
            1 + int.from_bytes(data[27:30], "little"),
        )
    return None


# The JPEG markers of a frame header, which holds the picture size.
_JPEG_FRAMES = frozenset(range(0xC0, 0xD0)) - {0xC4, 0xC8, 0xCC}


def _jpeg_size(data: bytes) -> tuple[int, int] | None:
    import struct

    at = 2
    while at + 4 <= len(data):
        marker = data[at + 1]
        if data[at] != 0xFF:
            return None
        if marker == 0xFF:
            at += 1
        elif marker == 0x01 or 0xD0 <= marker <= 0xD9:
            at += 2
        elif marker in _JPEG_FRAMES:
            height, width = struct.unpack(">HH", data[at + 5 : at + 9])
            return width, height
        else:
            at += 2 + struct.unpack(">H", data[at + 2 : at + 4])[0]
    return None


def read_picture(picture: Any) -> dict[str, Any]:
    """`picture` with `width` and `height` from its header; ValueError when Vis refuses it.

    The bytes must start like the media type, the header must give a size within
    the pixel bound, and a declared size must match the header.
    """
    import base64
    import binascii

    rule = definition("view", "picture")
    types = sorted(rule["x-vis-magic"])
    if not isinstance(picture, dict) or picture.get("media_type") not in types:
        raise ValueError("a picture needs a media type: " + ", ".join(types))
    media_type, data = picture["media_type"], picture.get("data")
    longest = rule["properties"]["data"]["maxLength"]
    if isinstance(data, str) and len(data) > longest:
        raise ValueError(
            f"picture data is {len(data)} characters; the most is {longest}"
        )
    try:
        decoded = (
            base64.b64decode(data, validate=True) if isinstance(data, str) else None
        )
    except (binascii.Error, ValueError):
        decoded = None
    if decoded is None:
        raise ValueError("picture data must be base64 text")
    if not matches_magic(decoded, media_type):
        raise ValueError(f"picture data is not {media_type} data")
    size = picture_size(decoded, media_type)
    if not size or size[0] < 1 or size[1] < 1:
        raise ValueError("the picture header does not give its size")
    width, height = int(size[0]), int(size[1])
    most = rule["x-vis-max-pixels"]
    if width * height > most:
        raise ValueError(f"the picture has {width}x{height} pixels; the most is {most}")
    declared = (picture.get("width"), picture.get("height"))
    if (declared[0] is not None and declared[0] != width) or (
        declared[1] is not None and declared[1] != height
    ):
        raise ValueError(
            f"the declared size {declared[0]}x{declared[1]} differs from the "
            f"header size {width}x{height}"
        )
    return dict(picture, width=width, height=height)


def picture_byte_count(data: Any) -> int:
    """How many bytes the base64 `data` of a picture holds."""
    if not isinstance(data, str):
        return 0
    return len(data) * 3 // 4 - (len(data[-2:]) - len(data[-2:].rstrip("=")))


def node_picture_bytes(node: dict[str, Any]) -> int:
    """The picture bytes of one image node, its `before` picture included."""
    if node.get("type") != "image":
        return 0
    before = node.get("before")
    return picture_byte_count(node.get("data")) + picture_byte_count(
        before.get("data") if isinstance(before, dict) else None
    )


def picture_budget_error(nodes: list[dict[str, Any]]) -> str | None:
    """None when the pictures of live `nodes` (a tree) fit the view bound, else why."""

    def tree(items):
        for node in items or []:
            yield node
            yield from tree(node.get("children"))

    total = sum(node_picture_bytes(node) for node in tree(nodes))
    most = definition("view", "live_view")["x-vis-max-picture-bytes"]
    if total > most:
        return f"the pictures of this view hold {total} bytes; the most is {most}"
    return None


# -- Live views ------------------------------------------------------------------
# One check for every Python host: the SDK before it sends, the test recorder,
# the outside-Vis host and the dev command. It is the same algorithm as the
# engine check `com.blockether.vis.contract.view/live-view-faults`, and every case
# of `vis-contract/fixtures/live-view-checks.json` holds both hosts to one answer.
# The schema owns the shape and the defaults of each node. `x-vis-error-messages` of
# the common schema owns the text of each fault. This code holds only the rules
# that JSON Schema cannot say.


@cache
def _messages() -> dict[str, Any]:
    return schema("common")["x-vis-error-messages"]


def _clip_name(text: Any) -> str:
    return str(text)[: _messages()["most_name_chars"]]


def _number_text(n: Any) -> str:
    if isinstance(n, float) and n.is_integer() and abs(n) < 1e15:
        return str(int(n))
    return str(n)


def _value_text(value: Any) -> str:
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, str):
        return value
    if isinstance(value, (int, float)):
        return _number_text(value)
    return str(value)


def _json_type(value: Any) -> str:
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "boolean"
    if isinstance(value, int):
        return "integer"
    if isinstance(value, float):
        return "number"
    if isinstance(value, str):
        return "string"
    return "array" if isinstance(value, list) else "object"


def _render(template: str, values: dict[str, Any]) -> str:
    def value(match):
        found = values.get(match.group(1))
        return "" if found is None else str(found)

    return re.sub(r"\{([a-z_]+)\}", value, template)


def rule_line(path: str, message: str, rule: str) -> str:
    """One fault line `{path} - {message} ({rule})` of the message table."""
    return _render(
        _messages()["line"], {"path": path or "/", "message": message, "rule": rule}
    )


def named_line(path: str, rule_name: str, values: dict[str, Any] | None = None) -> str:
    """The line of hand rule `rule_name` of the message table at `path`."""
    template, rule = _messages()["rules"][rule_name]
    shown = {
        key: _clip_name(value) if isinstance(value, str) else _value_text(value)
        for key, value in (values or {}).items()
    }
    return rule_line(path, _render(template, shown), rule)


def rule_text(rule_name: str, values: dict[str, Any]) -> str:
    """The message of hand rule `rule_name` of the message table, without a path: the
    reason of a patch refusal that names its node itself."""
    template, _ = _messages()["rules"][rule_name]
    return _render(template, {key: _value_text(one) for key, one in values.items()})


def _pointer(path) -> str:
    return "".join(
        "/" + str(part).replace("~", "~0").replace("/", "~1") for part in path
    )


def _error_lines(error, prefix: str = "") -> list[str]:
    """The lines of one schema error. A line never holds the value of the instance."""
    table = _messages()["keywords"]
    where, keyword, limit = (
        prefix + _pointer(error.absolute_path),
        error.validator,
        error.validator_value,
    )
    instance = error.instance

    def line(message):
        return rule_line(where, message, keyword)

    if keyword == "required":
        missing = [k for k in limit if isinstance(instance, dict) and k not in instance]
        return [
            line(_render(table[keyword], {"property": _clip_name(k)})) for k in missing
        ]
    if keyword == "additionalProperties":
        known = set((error.schema or {}).get("properties", {}))
        patterns = list((error.schema or {}).get("patternProperties", {}))
        return [
            line(_render(table[keyword], {"property": _clip_name(key)}))
            for key in instance
            if key not in known and not any(re.search(p, key) for p in patterns)
        ]
    if keyword == "oneOf":
        # The branch whose discriminator `const` holds is the form the value chose.
        by_branch: dict[Any, list] = {}
        for sub in error.context or []:
            by_branch.setdefault(sub.relative_schema_path[0], []).append(sub)
        chosen = [
            faults
            for faults in by_branch.values()
            if not any(
                sub.validator == "const"
                and len(sub.relative_schema_path) == 4
                and sub.relative_schema_path[1] == "properties"
                for sub in faults
            )
        ]
        if len(chosen) == 1:
            return [text for sub in chosen[0] for text in _error_lines(sub, prefix)]
    if keyword == "pattern":
        return [
            line(
                _messages()["blank"]
                if limit == _messages()["blank_pattern"]
                else table[keyword]
            )
        ]
    bounded = {
        "minimum",
        "maximum",
        "exclusiveMinimum",
        "exclusiveMaximum",
        "minItems",
        "maxItems",
        "minLength",
        "maxLength",
    }
    values = {
        "limit": _value_text(limit) if keyword in bounded else None,
        "allowed": ", ".join(map(_value_text, limit))
        if keyword == "enum"
        else _value_text(limit)
        if keyword == "const"
        else None,
        "expected": " or ".join(limit if isinstance(limit, list) else [limit])
        if keyword == "type"
        else None,
        "actual": _json_type(instance),
        "count": len(instance) if isinstance(instance, (str, list)) else None,
        "rule": keyword,
    }
    return [line(_render(table.get(keyword) or _messages()["default"], values))]


def _explain_lines(
    name: str, target: str, value: Any, *, prefix: str = "", skip: str | None = None
) -> list[str]:
    """The sorted, distinct fault lines of `value` against `target`. A failed
    `oneOf` gives the faults of the one branch whose discriminator `const` holds.
    Without one such branch, it is one line. `prefix` goes in front of each path,
    and the faults inside top key `skip` are left out."""
    lines = {
        line
        for error in _validator(name, target).iter_errors(value)
        if not (skip and error.absolute_path and error.absolute_path[0] == skip)
        for line in _error_lines(error, prefix)
    }
    return sorted(lines)


@cache
def _live_op_index() -> dict[str, int]:
    return {
        branch["properties"]["op"]["const"]: index
        for index, branch in enumerate(definition("view", "live_op")["oneOf"])
    }


def _live_op_lines(path: str, op: Any) -> list[str]:
    """The fault lines of wire patch op `op` at `path`. An op is checked against the
    one `live_op` variant that its `op` names, as the engine does: its validator
    reports at most ten errors, so a check against the whole union can stop before
    the faults of the chosen form. The node of an `add-node` op is left to the node
    check after the op. Each case is in `live-view-checks.json`."""
    name = op.get("op") if isinstance(op, dict) else None
    index = _live_op_index().get(name) if isinstance(name, str) else None
    if index is not None:
        return _explain_lines(
            "view", f"live_op/oneOf/{index}", op, prefix=path, skip="node_spec"
        )
    table = _messages()["keywords"]
    if isinstance(op, dict):
        allowed = ", ".join(_live_op_index())
        return [
            rule_line(
                path + "/op", _render(table["enum"], {"allowed": allowed}), "enum"
            )
        ]
    return [
        rule_line(
            path,
            _render(table["type"], {"expected": "object", "actual": _json_type(op)}),
            "type",
        )
    ]


def live_patch_lines(patch: Any) -> list[str]:
    """The sorted fault lines of wire live patch `patch`, which every host gives
    before it applies an op. The patch keys and the op list are checked with a
    stand-in op, and each op on its own."""
    ops = patch.get("ops") if isinstance(patch, dict) else None
    shell = dict(patch) if isinstance(patch, dict) else patch
    if isinstance(ops, list) and ops:
        shell["ops"] = [{"op": "clear", "node_id": "p"}]
    lines = set(_explain_lines("view", "live_patch", shell))
    for index, op in enumerate(ops if isinstance(ops, list) else []):
        lines.update(_live_op_lines(f"/ops/{index}", _filled_op(op)))
    return sorted(lines)


def _filled_op(op: Any) -> Any:
    """Wire op `op` with each item filled as the host keeps it: see `_filled_item`. A
    lane without a `tone` is an `idle` lane, and a link with a scheme is a `url` link."""
    if not isinstance(op, dict):
        return op
    return {key: _filled_items(key, value) for key, value in op.items()}


def joined_lines(lines: list[str]) -> str:
    """At most `most_shown` of `lines`, joined with `; `, then `and N more`."""
    most = _messages()["most_shown"]
    left = len(lines) - most
    text = "; ".join(lines[:most])
    if left > 0:
        text += "; " + _render(_messages()["more"], {"count": left})
    return text


def fault_text(node_id: Any, lines: list[str]) -> str:
    """`node <id> <lines>` of node `node_id`, at most five lines."""
    return _render(
        _messages()["node"],
        {
            "id": str(node_id)[: _messages()["most_name_chars"]],
            "lines": joined_lines(lines),
        },
    )


def view_fault_text(lines: list[str]) -> str:
    """`view <lines>` of the view faults `lines`."""
    return _render(_messages()["view"], {"lines": joined_lines(lines)})


def component_variant(node_type: Any) -> tuple[int, dict[str, Any]] | None:
    """The index and schema of the `component` variant of `node_type`, or None."""
    if not isinstance(node_type, str):
        return None
    for index, branch in enumerate(definition("view", "component")["oneOf"]):
        if branch["properties"]["type"].get("const") == node_type:
            return index, branch
    return None


class Bound(NamedTuple):
    """One schema bound of a node field: `maxItems` for a list, `maxLength` for text."""

    field: str
    keyword: str
    limit: int


def bounds(node_type: str) -> list[Bound]:
    """Each `maxItems` and `maxLength` bound of the fields of `node_type`, in schema order.

    Raises ValueError for a type that the view schema does not know.
    """
    found = component_variant(node_type)
    if found is None:
        raise ValueError(f"unknown node type {node_type!r}")
    return [
        Bound(field, keyword, rule[keyword])
        for field, rule in found[1]["properties"].items()
        for keyword in ("maxItems", "maxLength")
        if keyword in rule
    ]


def deepcopy_json(value: Any) -> Any:
    """A deep copy of JSON data. A tuple becomes a list, as on the wire."""
    return json.loads(json.dumps(value))


def _ref_definition(rule: dict[str, Any]) -> dict[str, Any] | None:
    found = re.fullmatch(r"#/\$defs/(.+)", str((rule.get("items") or {}).get("$ref")))
    return definition("view", found.group(1)) if found else None


def _filled_defaults(item: dict[str, Any], shape: dict[str, Any]) -> dict[str, Any]:
    """`item` with the schema default of each required key of `shape` that it leaves
    out, then with each key that `x-vis-fill` fills: `true` takes the schema default,
    a property name takes the value of that property."""
    item = dict(item)
    properties = shape.get("properties", {})
    for key in shape.get("required", []):
        rule = properties.get(key, {})
        if key not in item and "default" in rule:
            item[key] = deepcopy_json(rule["default"])
    for key, rule in sorted(properties.items()):
        fill = rule.get("x-vis-fill") if isinstance(rule, dict) else None
        if fill is None or key in item:
            continue
        if fill is True:
            item[key] = deepcopy_json(rule["default"])
        elif fill in item:
            item[key] = deepcopy_json(item[fill])
    return item


_SCHEME = re.compile(r"^[A-Za-z][A-Za-z0-9+.-]*:")


def _filled_item(shape: dict[str, Any], key: str, item: Any) -> Any:
    if not isinstance(item, dict):
        return item
    item = _filled_defaults(item, shape)
    if (
        key == "links"
        and "target_kind" not in item
        and isinstance(item.get("target"), str)
        and _SCHEME.search(item["target"])
    ):
        item["target_kind"] = "url"
    return item


def fill_live_node(node: Any) -> Any:
    """A filled copy of live wire node `node`, as every host fills it: a null key is
    an absent key, a node without a type is a `plaintext` input, an input takes its
    name and label from its id, a link with a scheme is a `url` link, and each
    required key with a schema `default` takes it. The children are filled too."""
    if not isinstance(node, dict):
        return node
    node = {key: value for key, value in node.items() if value is not None}
    node.setdefault("type", "plaintext")
    found = component_variant(node["type"])
    shape = found[1] if found else {}
    properties = shape.get("properties", {})
    if "is_secret" in properties:
        node.pop("is_secret", None)
        if "id" not in node and "name" in node:
            node["id"] = node["name"]
        if "id" in node:
            node.setdefault("name", node["id"])
            node.setdefault("label", node["id"])
    if shape:
        node = _filled_defaults(node, shape)
    for key, rule in properties.items():
        item_shape = _ref_definition(rule)
        if item_shape and isinstance(node.get(key), list):
            node[key] = [_filled_item(item_shape, key, one) for one in node[key]]
    if isinstance(node.get("children"), list):
        node["children"] = [fill_live_node(child) for child in node["children"]]
    return node


_LIVE_ITEMS = {
    "stats": "stat",
    "steps": "step",
    "lanes": "lane",
    "rows": "row",
    "links": "link",
}


def live_items(key: str, items: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Return appended live items as the host keeps them; refuse a missing key."""
    shape = definition("view", _LIVE_ITEMS[key])
    result = []
    for one in items:
        one = _filled_item(shape, key, one)
        if missing := [name for name in shape["required"] if name not in one]:
            raise ValueError(
                f"{_LIVE_ITEMS[key]} {one.get('id')} - "
                f"{', '.join(missing)} is missing (required)"
            )
        result.append(one)
    return result


def _non_finite_paths(value: Any, path: str = "") -> list[str]:
    if isinstance(value, dict):
        return [
            p for k, v in value.items() for p in _non_finite_paths(v, f"{path}/{k}")
        ]
    if isinstance(value, list):
        return [
            p for i, v in enumerate(value) for p in _non_finite_paths(v, f"{path}/{i}")
        ]
    if isinstance(value, float) and not math.isfinite(value):
        return [path]
    return []


def _child_path(line: str) -> bool:
    # A group child is checked on its own. Only the rule that the group puts on
    # the type of a child stays with the group.
    return re.search(r"^/children/\d+[/ ]", line) is not None


def _toolbar_stamp_lines(children: Any, path: str) -> list[str]:
    """A `read_only` line for each key that only the host writes on a node of diff
    toolbar `children`, a menu child too. The diff node check does not see them,
    because a toolbar button is no node of the view."""
    lines: list[str] = []
    for i, child in enumerate(children if isinstance(children, list) else []):
        if not isinstance(child, dict):
            continue
        found = component_variant(child.get("type"))
        properties = found[1].get("properties", {}) if found else {}
        lines += [
            named_line(f"{path}/{i}/{key}", "read_only")
            for key, rule in sorted(properties.items())
            if rule.get("readOnly") and key in child
        ]
        lines += _toolbar_stamp_lines(child.get("children"), f"{path}/{i}/children")
    return lines


def _repeated(ids) -> str | None:
    counts: dict[str, int] = {}
    for one in ids:
        if one is not None:
            counts[str(one)] = counts.get(str(one), 0) + 1
    return min((one for one, n in counts.items() if n > 1), default=None)


def _is_number(value: Any) -> bool:
    return isinstance(value, (int, float)) and not isinstance(value, bool)


def _char_count(text: str) -> int:
    # The engine counts UTF-16 units.
    return len(text.encode("utf-16-le")) // 2


def input_branches() -> dict[str, dict[str, Any]]:
    """The schema of each input component of a view, by type.

    An input component holds one answer, so its schema has `is_required`.
    """
    return {
        branch["properties"]["type"]["const"]: branch
        for branch in definition("view", "component")["oneOf"]
        if "is_required" in branch["properties"]
    }


def input_types() -> set[str]:
    """The type of each input component of a view."""
    return set(input_branches())


def secret_input_types() -> set[str]:
    """The type of each input component that hides its value."""
    return {
        kind
        for kind, branch in input_branches().items()
        if branch["properties"]["is_secret"].get("const") is True
    }


def _input_kinds() -> tuple[set[str], set[str]]:
    text = {
        kind
        for kind, branch in input_branches().items()
        if "min_length" in branch["properties"]
        and "min_length" not in branch.get("required", [])
    }
    return secret_input_types(), text


def _range_defaults() -> dict[str, Any]:
    properties = component_variant("range")[1]["properties"]  # type: ignore[index]
    return {key: properties[key]["default"] for key in ("min", "max", "step")}


def _in_domain(node: dict[str, Any], value: Any, *, is_default: bool) -> bool:
    kind = node.get("type")
    _, text_kinds = _input_kinds()
    chosen = {
        option.get("value")
        for option in node.get("options") or []
        if isinstance(option, dict)
    }
    if is_default:
        if value is None:
            return False
        if kind in text_kinds:
            return isinstance(value, str)
        if kind == "select":
            return value in chosen
        if kind == "multiselect":
            return (
                isinstance(value, list)
                and all(one in chosen for one in value)
                and len(value) == len({json.dumps(one) for one in value})
            )
        if kind == "checkbox":
            return isinstance(value, bool)
        if kind == "range":
            return _is_number(value) and node["min"] <= value <= node["max"]
        if kind == "otp":
            return (
                isinstance(value, str)
                and re.fullmatch(r"\d+", value) is not None
                and node["min_length"] <= _char_count(value) <= node["max_length"]
            )
        return False
    if kind in ("plaintext", "multiline"):
        if value is None:
            return True
        if not isinstance(value, str):
            return False
        if not value.strip():
            return True
        n = _char_count(value)
        return (node.get("min_length") is None or node["min_length"] <= n) and (
            node.get("max_length") is None or n <= node["max_length"]
        )
    if kind == "select":
        return value is None or (isinstance(value, str) and value in chosen)
    if kind == "multiselect":
        return (
            isinstance(value, list)
            and all(isinstance(one, str) and one in chosen for one in value)
            and len(value) == len(set(value))
        )
    if kind == "checkbox":
        return isinstance(value, bool)
    if kind == "range":
        return _is_number(value) and node["min"] <= value <= node["max"]
    return False


def _input_lines(node: dict[str, Any]) -> list[str]:
    secret_kinds, _ = _input_kinds()
    kind = node.get("type")
    lines = []
    secret = kind in secret_kinds
    if secret and node.get("value") is not None:
        lines.append(named_line("/value", "secret_value"))
    if secret and "default" in node:
        lines.append(named_line("/default", "secret_default"))
    if "validate" in node:
        lines.append(named_line("/validate", "live_validate"))
    options = node.get("options")
    if isinstance(options, list):
        values = [
            one.get("value") if isinstance(one, dict) else None for one in options
        ]
        if _repeated(values) is not None:
            lines.append(named_line("/options", "repeated_option"))
    if node.get("id") != node.get("name"):
        lines.append(named_line("/name", "id_name"))
    low, high = node.get("min_length"), node.get("max_length")
    if low is not None and high is not None and low > high:
        lines.append(named_line("/min_length", "length_order"))
    checked = {**_range_defaults(), **node} if kind == "range" else node
    # A default or a value is held to the domain only when the range makes one.
    has_domain = True
    if kind == "range":
        if not checked["min"] < checked["max"]:
            lines.append(named_line("/min", "range_order"))
            has_domain = False
        elif not 0 < checked["step"] <= checked["max"] - checked["min"]:
            lines.append(named_line("/step", "range_step"))
            has_domain = False
    if "default" in node and not secret and has_domain:
        if not _in_domain(checked, node["default"], is_default=True):
            lines.append(named_line("/default", "default_domain"))
    if "value" in node and not secret and has_domain:
        if not _in_domain(checked, node["value"], is_default=False):
            lines.append(named_line("/value", "value_domain"))
    return lines


def _toolbar_command(node: Any) -> bool:
    return isinstance(node, dict) and (
        node.get("type") == "button"
        or (
            node.get("type") == "group"
            and isinstance(node.get("children"), list)
            and all(_toolbar_command(child) for child in node["children"])
        )
    )


def _picture_reason(raw: dict[str, Any]) -> str | None:
    try:
        read_picture(raw)
    except ValueError as error:
        # The engine names the picture in the line, so the reason starts later.
        return re.sub(r"^(a )?picture (?=needs|data)", "", str(error))
    return None


def _ids(items: Any) -> list[Any]:
    return [one.get("id") if isinstance(one, dict) else None for one in items or []]


def _hand_lines(node: dict[str, Any], *, is_declaration: bool) -> list[str]:
    """The rules of filled node `node` that JSON Schema cannot say. Each one is a
    case of `live-view-checks.json`."""
    kind = node.get("type")
    properties = component_variant(kind)[1]["properties"]  # type: ignore[index]
    lines = []
    if is_declaration:
        lines += [
            named_line(f"/{key}", "read_only")
            for key, rule in sorted(properties.items())
            if rule.get("readOnly") and key in node
        ]
    if "is_required" in properties:
        lines += _input_lines(node)
    if kind == "log":
        tones = node.get("line_tones")
        if isinstance(tones, list) and len(tones) != len(node.get("lines") or []):
            lines.append(named_line("/line_tones", "line_tones"))
    elif kind == "group":
        if "max_columns" in node and node.get("direction") != "grid":
            lines.append(named_line("/max_columns", "grid_columns"))
    elif kind == "diff":
        if is_declaration:
            lines += _toolbar_stamp_lines(node.get("children"), "/children")
        for i, child in enumerate(node.get("children") or []):
            if isinstance(child, dict) and child.get("type") == "group":
                if not _toolbar_command(child):
                    lines.append(named_line(f"/children/{i}", "toolbar_menu"))
        for i, thread in enumerate(node.get("threads") or []):
            if isinstance(thread, dict) and "anchor" not in thread:
                lines.append(named_line(f"/threads/{i}", "thread_anchor"))
        if (one := _repeated(_ids(node.get("threads")))) is not None:
            lines.append(named_line("/threads", "repeated_item_id", {"id": one}))
    elif kind in ("stat", "steps"):
        key = "stats" if kind == "stat" else "steps"
        if (one := _repeated(_ids(node.get(key)))) is not None:
            lines.append(named_line(f"/{key}", "repeated_item_id", {"id": one}))
    elif kind == "timeline":
        lanes = node.get("lanes") or []
        if (one := _repeated(_ids(lanes))) is not None:
            lines.append(named_line("/lanes", "repeated_item_id", {"id": one}))
        for i, lane in enumerate(lanes):
            start, end = (
                (lane.get("start"), lane.get("end"))
                if isinstance(lane, dict)
                else (None, None)
            )
            if (
                isinstance(start, (int, float))
                and isinstance(end, (int, float))
                and _is_number(start)
                and _is_number(end)
                and end < start
            ):
                lines.append(named_line(f"/lanes/{i}", "lane_order"))
    elif kind == "table":
        order = node.get("order")
        columns, rows = set(_ids(node.get("columns"))), set(_ids(node.get("rows")))
        if isinstance(order, dict) and order.get("by") not in columns:
            lines.append(named_line("/order", "table_order"))
        most, held = node.get("max_rows"), node.get("rows")
        if (
            isinstance(most, (int, float))
            and not isinstance(most, bool)
            and isinstance(held, list)
            and len(held) > most
        ):
            lines.append(
                named_line("/rows", "table_rows", {"count": len(held), "limit": most})
            )
        if "selected_ids" in node:
            if node.get("is_selectable") is not True:
                lines.append(named_line("/selected_ids", "not_selectable"))
            elif not all(one in rows for one in node["selected_ids"]):
                lines.append(named_line("/selected_ids", "table_selection"))
    elif kind == "scene":
        meshes, objects = node.get("meshes") or [], node.get("objects") or []
        builtin = scene_meshes()
        known = set(builtin) | set(_ids(meshes))
        if (one := _repeated(_ids(meshes))) is not None:
            lines.append(named_line("/meshes", "repeated_item_id", {"id": one}))
        if (one := _repeated(_ids(objects))) is not None:
            lines.append(named_line("/objects", "repeated_item_id", {"id": one}))

        def flat(items):
            for item in items or []:
                yield from flat(item) if isinstance(item, list) else [item]

        for i, mesh in enumerate(meshes):
            if mesh.get("id") in builtin:
                lines.append(named_line(f"/meshes/{i}", "mesh_shadow"))
            elif not all(
                int(v) < len(mesh.get("vertices") or [])
                for v in flat(mesh.get("faces"))
            ):
                lines.append(named_line(f"/meshes/{i}", "face_vertex"))
        for i, one in enumerate(objects):
            if one.get("mesh") not in known:
                lines.append(named_line(f"/objects/{i}", "unknown_mesh"))
    elif kind == "image":
        for path, raw in (("/", node), ("/before", node.get("before"))):
            if isinstance(raw, dict) and "data" in raw:
                if (reason := _picture_reason(raw)) is not None:
                    lines.append(named_line(path, "picture", {"reason": reason}))
    return lines


def live_node_lines(node: Any, *, is_declaration: bool = True) -> list[str]:
    """The sorted fault lines of wire node `node` after `fill_live_node`. A line
    names the path in the node, the broken rule and its keyword, never a value.
    With `is_declaration`, a key that only the host writes is a fault. The faults
    of a group child belong to the child, which the caller checks on its own."""
    node = fill_live_node(node)
    if not isinstance(node, dict):
        return [named_line("/", "node_not_map")]
    kind = node.get("type")
    found = component_variant(kind)
    if found is None:
        return [named_line("/type", "unknown_type")]
    bad = _non_finite_paths(node)
    if bad:
        return sorted({named_line(path, "not_finite") for path in bad})
    is_group = kind == "group"
    lines = _explain_lines("view", f"component/oneOf/{found[0]}", node)
    if is_group:
        lines = [line for line in lines if not _child_path(line)]
        # A group child is checked on its own, so it needs a map and an id.
        children = node.get("children")
        for i, child in enumerate(children if isinstance(children, list) else []):
            if not isinstance(child, dict):
                lines.append(named_line(f"/children/{i}", "child_not_map"))
            elif not (isinstance(child.get("id"), str) and child["id"].strip()):
                rule = (
                    "group_needs_id"
                    if child.get("type") == "group"
                    else "node_needs_id"
                )
                lines.append(named_line(f"/children/{i}", rule))
    if not lines:
        lines = _hand_lines(node, is_declaration=is_declaration)
    return sorted(set(lines))


def error_card(node_id: str, text: str) -> dict[str, Any]:
    """The node that a host shows in place of node `node_id` with fault `text`. The
    text is not cut, because the schema gives a status text no length bound."""
    return {"id": node_id, "type": "status", "tone": "error", "text": text}


def live_node_fault(node: Any, *, is_declaration: bool = True) -> str | None:
    """The fault text of one wire node, or None when it is valid."""
    lines = live_node_lines(node, is_declaration=is_declaration)
    if not lines:
        return None
    return fault_text(node.get("id") if isinstance(node, dict) else None, lines)


class Dropped(NamedTuple):
    """A shortcut that a host drops, because it names a node with a fault."""

    key: Any
    node_id: Any
    note: str


class ViewFaults(NamedTuple):
    """The faults of a live view: `view` lines refuse the whole view, `nodes` maps a
    node id to the text of its fault, and a host shows each such node as an error
    card. `dropped` holds the shortcuts that name such a node."""

    view: list[str]
    nodes: dict[str, str]
    dropped: tuple[Dropped, ...] = ()

    @property
    def is_valid(self) -> bool:
        return not self.view and not self.nodes

    def view_text(self) -> str:
        """`view <lines>`: the text of the view faults."""
        return view_fault_text(self.view)

    def lines(self) -> list[str]:
        """Every fault, one text each: the view faults first."""
        return ([self.view_text()] if self.view else []) + list(self.nodes.values())


_VIEW_STAMPS = {
    "id": "view",
    "channel_ids": ["app"],
    "timeout_ms": 0,
    "seq": 0,
    "created_at": 1,
}


def _wire_tree(nodes: Any):
    for node in nodes if isinstance(nodes, list) else []:
        if isinstance(node, dict):
            yield node
            if node.get("type") == "group":
                yield from _wire_tree(node.get("children"))


def _command_tree(nodes: Any):
    """Each map node, also each child of a group or a diff toolbar: the nodes that a
    shortcut can press."""
    for node in nodes if isinstance(nodes, list) else []:
        if isinstance(node, dict):
            yield node
            yield from _command_tree(node.get("children"))


def _shortcut_lines(view: dict[str, Any], nodes: Any) -> list[str]:
    """The view faults of the shortcuts: only an app takes shortcuts, a key is bound
    once, and a `node_id` names a button or a selectable card of the view. The
    schema holds the shape of each shortcut."""
    shortcuts = view.get("shortcuts")
    if not isinstance(shortcuts, list):
        return []
    if view.get("placement") != "attachment":
        return [named_line("/shortcuts", "shortcut_needs_app")]
    lines = []
    seen = set()
    for i, shortcut in enumerate(shortcuts):
        key = (
            str(shortcut.get("key"))
            if isinstance(shortcut, dict) and "key" in shortcut
            else None
        )
        if key in seen:
            lines.append(named_line(f"/shortcuts/{i}", "repeated_shortcut_key"))
        elif key is not None:
            seen.add(key)
    by_id = {node.get("id"): node for node in _command_tree(nodes)}
    for i, shortcut in enumerate(shortcuts):
        node_id = shortcut.get("node_id") if isinstance(shortcut, dict) else None
        if not isinstance(node_id, str):
            continue
        node = by_id.get(node_id)
        if node is None:
            lines.append(named_line(f"/shortcuts/{i}", "shortcut_target"))
        elif not (
            node.get("type") == "button"
            or (node.get("type") == "card" and node.get("is_selectable") is True)
        ):
            lines.append(named_line(f"/shortcuts/{i}", "shortcut_not_pressable"))
    return lines


def live_op_target_lines(node_type: Any, node_id: Any, op: dict[str, Any]) -> list[str]:
    """The sorted fault lines of wire op `op` on a node of wire type `node_type`, from
    `x-vis-patch-targets`: the op does not apply to that type, or a key of the op that
    the type does not take. An op without a target table has no lines. A thread op on
    a thread node must name the id `node_id` of that node."""
    name = op.get("op")
    table = definition("view", "live_op")["x-vis-patch-targets"].get(name)
    values = {"op": name, "type": node_type}
    if name == "description" or not isinstance(table, dict):
        return []
    if node_type not in table:
        return [named_line("/op", "op_target", values)]
    taken = set(table[node_type]) | {"op", "node_id"}
    lines = [
        named_line(f"/{key}", "op_key", values)
        for key in sorted(op)
        if key not in taken
    ]
    thread = op.get("thread")
    if (name, node_type) == ("thread", "thread") and (
        not isinstance(thread, dict) or thread.get("id") != node_id
    ):
        lines.append(named_line("/thread/id", "own_thread"))
    return lines


def _fault_key(line: str) -> tuple[str, str | None]:
    """The path and the rule of fault line `line`, without the counts of its message."""
    rule = re.search(r"\(([^()]*)\)$", line)
    return line.split(" - ", 1)[0], rule.group(1) if rule else None


def live_patched_lines(lines_of: Any, before: Any, after: Any) -> list[str]:
    """The fault lines of `after` whose path and rule `before` did not have: a patch
    is refused only for a fault that it adds, not for one that it makes smaller or
    larger."""
    had = {_fault_key(one) for one in lines_of(before)} if before is not None else set()
    return [line for line in lines_of(after) if _fault_key(line) not in had]


def live_patch_refusal(patch: Any, view_id: str, seq: int) -> str | None:
    """The refusal text of wire live patch `patch` for the view `view_id` at `seq`,
    before any op applies, or None. A patch that names its own `view_id` or `seq`
    is refused: the host stamps them."""
    given = patch if isinstance(patch, dict) else {}
    stamps = [key for key in ("view_id", "seq") if key in given]
    ops = given.get("ops")
    checked = {key: value for key, value in given.items() if key not in stamps}
    checked.update(
        view_id=view_id,
        seq=seq + 1,
        ops=[cut_log_texts(op) for op in ops] if isinstance(ops, list) else ops,
    )
    lines = {named_line("/" + key, "read_only") for key in stamps}
    lines.update(live_patch_lines(checked))
    if lines:
        return PATCH_REFUSAL + joined_lines(sorted(lines))
    return None


def missing_node_text(node_id: Any, ids: list[Any]) -> str:
    """The refusal of a node op that names node `node_id`, which the view with node
    ids `ids` does not hold, as the engine gives it."""
    ids_text = ", ".join(str(one) for one in ids)
    return PATCH_REFUSAL + fault_text(
        node_id, [named_line("/node_id", "missing_node", {"ids": ids_text})]
    )


def live_collection(node_type: Any) -> str | None:
    """The key of the items that a node of wire type `node_type` holds, or None: the
    first key that an append takes in `x-vis-patch-targets`."""
    keys = definition("view", "live_op")["x-vis-patch-targets"]["append"].get(node_type)
    return keys[0] if keys else None


def _upserted(held: Any, incoming: Any, *, is_merged: bool = False) -> list[Any]:
    """`incoming` merged into `held` by id: an item keeps the position that it was
    first given. Only a table group declaration merges onto the one before it."""
    merged = list(held or [])
    at = {one.get("id"): i for i, one in enumerate(merged) if isinstance(one, dict)}
    for item in incoming or []:
        item_id = item.get("id") if isinstance(item, dict) else None
        if item_id is None or item_id not in at:
            at[item_id] = len(merged)
            merged.append(item)
        else:
            old = merged[at[item_id]]
            merged[at[item_id]] = {**old, **item} if is_merged else item
    return merged


def _filled_items(key: str, items: Any) -> Any:
    """`items` of collection `key` with the schema defaults of each item."""
    if key not in _LIVE_ITEMS or not isinstance(items, list):
        return items
    shape = definition("view", _LIVE_ITEMS[key])
    return [_filled_item(shape, key, one) for one in items]


def _pruned_selection(node: dict[str, Any]) -> None:
    """A table selection names only rows that still exist."""
    if node.get("type") == "table" and "selected_ids" in node:
        kept = {row.get("id") for row in node.get("rows") or []}
        node["selected_ids"] = [one for one in node["selected_ids"] if one in kept]


def log_text(text: Any) -> str:
    """Make terminal controls visible without interpreting or deleting log text."""
    return "".join(
        f"\\u{ord(char):04x}"
        if (ord(char) < 32 and char not in "\t\n") or 127 <= ord(char) <= 159
        else char
        for char in str(text)
    )


def _trimmed(value: Any) -> str | None:
    """Text `value` without the spaces at its ends, or None for blank text or no text."""
    return value.strip() or None if isinstance(value, str) else None


def _live_message(message: Any, author: dict[str, Any], at: int) -> Any:
    """Message `message` of a thread op as the host keeps it: a trimmed id or a new
    one, the owner `author` and the time `at` when it gives none."""
    if not isinstance(message, dict):
        return message
    kept = {
        **message,
        "id": _trimmed(message.get("id")) or str(uuid.uuid4()),
        "author": dict(author),
        "created_at": message.get("created_at", at),
    }
    if _trimmed(message.get("submission_id")):
        kept["submission_id"] = _trimmed(message["submission_id"])
    return kept


def _live_thread(thread: Any, author: dict[str, Any], at: int) -> Any:
    """Thread `thread` of a thread op as the host keeps it. A blank draft is no
    draft; a value that does not fit stays, so the node check refuses it."""
    if not isinstance(thread, dict):
        return thread
    kept = {k: v for k, v in thread.items() if k != "draft"}
    kept["id"] = _trimmed(thread.get("id")) or thread.get("id")
    messages = thread.get("messages")
    kept["messages"] = (
        [_live_message(one, author, at) for one in messages]
        if isinstance(messages, list)
        else messages
        if messages is not None
        else []
    )
    draft = thread.get("draft")
    if _trimmed(draft) or not isinstance(draft, (str, type(None))):
        kept["draft"] = draft
    return kept


# The heads of a refusal of a live view and of a live-view patch, as every host gives them.
VIEW_REFUSAL = "Invalid live view: "
PATCH_REFUSAL = "Invalid live-view patch: "


def owner_author(extension: Any = None) -> dict[str, Any]:
    """The author that the host writes on a message of the owner: extension
    `extension` by name, or, without a name, the agent of the sandbox."""
    name = _trimmed(extension)
    return {"kind": "extension", "name": name} if name else {"kind": "agent"}


def stamp_live_threads(node: Any, at: int, author: Any = None) -> None:
    """Write each thread of node `node` in place as the host keeps it when a view opens
    or an op adds the node: the owner `author` (by default the agent) on each message,
    an id, and the time `at` when a message gives none."""
    author = author or {"kind": "agent"}
    if not isinstance(node, dict):
        return
    if node.get("type") == "thread":
        kept = _live_thread(node, author, at)
        node.clear()
        node.update(kept)
    elif node.get("type") == "diff" and isinstance(node.get("threads"), list):
        node["threads"] = [_live_thread(one, author, at) for one in node["threads"]]


def owner_live_thread(node: dict[str, Any], thread: Any) -> Any:
    """Thread `thread` of an owner's thread op on node `node` as it lands: a message
    that the host stored keeps the stored copy, found by id, and the saved draft of
    the human stays. An owner never writes a draft."""
    if not isinstance(thread, dict):
        return thread
    held = (
        node.get("threads") or []
        if node.get("type") == "diff"
        else [node]
        if node.get("type") == "thread"
        else []
    )
    before = next(
        (
            one
            for one in held
            if isinstance(one, dict) and one.get("id") == thread.get("id")
        ),
        {},
    )
    posted = {
        one.get("id"): one
        for one in before.get("messages") or []
        if isinstance(one, dict)
    }
    kept = {k: v for k, v in thread.items() if k != "draft"}
    if isinstance(thread.get("messages"), list):
        kept["messages"] = [
            posted.get(one.get("id"), one) if isinstance(one, dict) else one
            for one in thread["messages"]
        ]
    if before.get("draft") is not None:
        kept["draft"] = before["draft"]
    return kept


def apply_live_node_op(
    node: dict[str, Any],
    op: dict[str, Any],
    *,
    author: dict[str, Any] | None = None,
    at: int = 0,
) -> dict[str, Any]:
    """Node `node` after node op `op`, as every host applies it, on a copy. A
    ValueError carries the fault text when the shared target table refuses the op.
    `apply_live_ops` checks the node after the op. A message of a
    thread op gets the owner `author` and, when it gives no time, the time `at`."""
    kind, name = node.get("type"), op.get("op")
    if lines := live_op_target_lines(kind, node.get("id"), op):
        raise ValueError(fault_text(node.get("id"), lines))
    node = deepcopy_json(node)
    given = {k: deepcopy_json(v) for k, v in op.items() if k not in ("op", "node_id")}
    # The target table takes a clear or a remove only for a node that holds items.
    key = "threads" if kind == "diff" else live_collection(kind) or ""
    if name == "thread":
        thread = owner_live_thread(
            node, _live_thread(given.get("thread"), author or {"kind": "agent"}, at)
        )
        if kind == "thread":
            for one in ("anchor", "draft", "is_resolved"):
                node.pop(one, None)
            node.update(
                {
                    k: thread[k]
                    for k in ("anchor", "messages", "draft", "is_resolved")
                    if k in thread
                }
            )
        else:
            ids = [
                one.get("id") if isinstance(one, dict) else None
                for one in node.get("threads") or []
            ]
            held = list(node.get("threads") or [])
            thread_id = thread.get("id") if isinstance(thread, dict) else None
            if thread_id in ids:
                held[ids.index(thread_id)] = thread
            else:
                held.append(thread)
            node["threads"] = held
    elif name == "set":
        node.update({k: _filled_items(k, v) for k, v in given.items()})
        # A blank detail CLEARS it, like a declaration that leaves the key out.
        if "detail" in given and not str(given["detail"]).strip():
            node.pop("detail", None)
    elif name == "set-value":
        if given.get("value") is None:
            node.pop("value", None)
        else:
            node["value"] = given["value"]
    elif name == "append" and kind == "log":
        added = [log_text(line) for line in given.get("lines") or []]
        held = list(node.get("lines") or [])
        if "tone" in given or "line_tones" in node:
            tones = node.get("line_tones") or [None] * len(held)
            node["line_tones"] = tones + [given.get("tone")] * len(added)
        node["total_lines"] = node.get("total_lines", len(held)) + len(added)
        node["lines"] = held + added
        window = node.get("window_lines")
        if isinstance(window, int) and not isinstance(window, bool):
            node["lines"] = node["lines"][-window:] if window else []
            if "line_tones" in node:
                node["line_tones"] = node["line_tones"][-window:] if window else []
    elif name == "append":
        # Meshes land before objects, so one append carries a mesh and its objects.
        for one in sorted(given, key=lambda k: (k != "meshes", k)):
            if one == "objects":
                node["objects"] = scene_objects(node.get("objects") or [], given[one])
            else:
                node[one] = _upserted(
                    node.get(one),
                    _filled_items(one, given[one]),
                    is_merged=one == "groups",
                )
    elif name == "clear" and kind == "log":
        node.update(lines=[], total_lines=0)
        node.pop("line_tones", None)
    elif name == "clear":
        node[key] = []
    elif name == "remove":
        dropped = set(given.get("item_ids") or [])
        node[key] = [
            one
            for one in node.get(key) or []
            if not (isinstance(one, dict) and one.get("id") in dropped)
        ]
    elif name == "replace":
        node["rows"] = _upserted([], _filled_items("rows", given.get("rows")))
    if name in ("clear", "remove", "replace"):
        _pruned_selection(node)
    return node


def _live_tree(nodes: Any):
    """Each node of `nodes` and of their children, at any depth."""
    for node in nodes or []:
        yield node
        yield from _live_tree(node.get("children"))


def _live_place(
    nodes: Any, node_id: Any, group: Any = None
) -> tuple[list[Any], int, Any] | None:
    """The list that holds the node `node_id` at any depth, its index there and the
    layout group that holds the list, or None at the top."""
    for index, node in enumerate(nodes or []):
        if node.get("id") == node_id:
            return nodes, index, group
        if isinstance(node.get("children"), list):
            if (found := _live_place(node["children"], node_id, node)) is not None:
                return found
    return None


def _checked_live_node(node: Any) -> None:
    """Refuse the patch when node `node` breaks its own rules after an op."""
    if node is not None and (fault := live_node_fault(node, is_declaration=False)):
        raise ValueError(PATCH_REFUSAL + fault)


def apply_live_ops(
    view: dict[str, Any],
    ops: list[dict[str, Any]],
    *,
    kept: Any,
    at: int,
    author: dict[str, Any] | None = None,
) -> list[Any]:
    """Apply the ops of a live patch that `live_patch_refusal` accepted to `view` in
    place, as every host applies them, then check the patched view. `kept` gives an
    added node as the host keeps it; `at` is the time of a log entry or a message.
    `author` is the owner that the host writes on each message of a thread op, by
    default the agent. Answer what each op wrote, for a transcript: the node, the
    log entry, or None.

    A ValueError carries the refusal text. The view can be part changed then, so
    give a copy.

    The check order is the same in every host: after each op, the node that it
    changed, and for `add-node` or `remove-node` also the group that holds the node;
    the first fault refuses. After the last op, the view faults."""
    written: list[Any] = []
    for op in ops:
        name = op.get("op")
        if name == "add-node":
            if fault := live_node_fault(op.get("node_spec")):
                raise ValueError(PATCH_REFUSAL + fault)
            node = kept(deepcopy_json(op["node_spec"]))
            after = op.get("after")
            found = _live_place(view["nodes"], after)
            if after is not None and found is None:
                raise ValueError(
                    PATCH_REFUSAL
                    + fault_text(
                        node.get("id"),
                        [named_line("/after", "missing_after", {"after": after})],
                    )
                )
            siblings, index, group = (
                (view["nodes"], len(view["nodes"]), None)
                if found is None
                else (found[0], found[1] + 1, found[2])
            )
            siblings.insert(index, node)
            _checked_live_node(group)
            written.append(node)
        elif name == "shortcuts":
            if op.get("shortcuts"):
                view["shortcuts"] = deepcopy_json(op["shortcuts"])
            else:
                view.pop("shortcuts", None)
            written.append(None)
        elif name == "log":
            written.append(append_log(view, op, at=at, source="app"))
        elif name == "agent-note":
            if op["text"].strip():
                view["agent_note"] = op["text"].strip()
            else:
                view.pop("agent_note", None)
            written.append(None)
        elif name == "remove-node":
            # To remove a node that is not there changes nothing.
            found = _live_place(view["nodes"], op.get("node_id"))
            written.append(found[0].pop(found[1]) if found is not None else None)
            _checked_live_node(found[2] if found is not None else None)
        else:
            found = _live_place(view["nodes"], op.get("node_id"))
            if found is None:
                ids = [one.get("id") for one in _live_tree(view["nodes"])]
                raise ValueError(missing_node_text(op.get("node_id"), ids))
            siblings, index, _ = found
            try:
                siblings[index] = apply_live_node_op(
                    siblings[index], op, author=author, at=at
                )
            except ValueError as error:
                raise ValueError(PATCH_REFUSAL + str(error)) from None
            _checked_live_node(siblings[index])
            written.append(siblings[index])
    if lines := live_view_faults(view, is_declaration=False).view:
        raise ValueError(PATCH_REFUSAL + view_fault_text(lines))
    return written


def live_view_faults(view: Any, *, is_declaration: bool = True) -> ViewFaults:
    """Check live wire view `view` as every host does, on a copy.

    A view line refuses the whole view: not a map, a view key that the schema
    refuses, a top node that is no map or has no id, two nodes with one id, too
    many nodes or too many picture bytes. Else each node that breaks its own rules
    maps its id to its fault text. With `is_declaration`, the keys that only the
    host writes are faults.
    """
    if not isinstance(view, dict):
        return ViewFaults([named_line("/", "view_not_map")], {})
    try:
        # The check reads what crosses the wire: a tuple is sent as a list.
        view = deepcopy_json(view)
    except (TypeError, ValueError):
        return ViewFaults([named_line("/", "view_not_json")], {})
    if "channel_id" in view and "channel_ids" not in view:
        view["channel_ids"] = [view.pop("channel_id")]
    nodes = view.get("nodes")
    if isinstance(nodes, list):
        nodes = [fill_live_node(node) for node in nodes]
    stamped = {
        **_VIEW_STAMPS,
        **{k: v for k, v in view.items() if k != "nodes"},
        # Each node is checked on its own; here the view only counts them.
        "nodes": [{"id": "node", "type": "divider"} for _ in nodes]
        if isinstance(nodes, list)
        else nodes,
    }
    if _non_finite_paths(stamped):
        lines = [named_line("/", "view_not_json")]
    else:
        lines = [
            line
            for line in _explain_lines("view", "live_view", stamped)
            if not re.match(r"^/nodes/\d+ ", line)
        ]
    if isinstance(nodes, list):
        for i, node in enumerate(nodes):
            if not isinstance(node, dict):
                lines.append(named_line(f"/nodes/{i}", "node_not_map"))
            elif not (isinstance(node.get("id"), str) and node["id"].strip()):
                rule = (
                    "group_needs_id" if node.get("type") == "group" else "node_needs_id"
                )
                lines.append(named_line(f"/nodes/{i}", rule))
    if is_declaration:
        properties = definition("view", "live_view")["properties"]
        lines += [
            named_line(f"/{key}", "read_only")
            for key, rule in sorted(properties.items())
            if rule.get("readOnly") and key in view
        ]
    shortcuts = _shortcut_lines(view, nodes)
    # A shortcut rule names the problem of its path; the oneOf line says less.
    covered = {line.split(" - ", 1)[0] for line in shortcuts}
    lines = [
        line
        for line in lines
        if not (line.endswith("(oneOf)") and line.split(" - ", 1)[0] in covered)
    ]
    lines += shortcuts
    tree = list(_wire_tree(nodes))
    if (one := _repeated(node.get("id") for node in tree)) is not None:
        lines.append(named_line("/nodes", "repeated_node_id", {"id": one}))
    most = definition("view", "live_view")["x-vis-max-nodes"]
    if len(tree) > most:
        lines.append(
            named_line("/nodes", "too_many_nodes", {"count": len(tree), "limit": most})
        )
    total = sum(node_picture_bytes(node) for node in tree)
    budget = definition("view", "live_view")["x-vis-max-picture-bytes"]
    if total > budget:
        lines.append(
            named_line("/nodes", "picture_budget", {"count": total, "limit": budget})
        )
    if lines:
        return ViewFaults(sorted(set(lines)), {})
    found: dict[str, str] = {}
    broken: set[Any] = set()

    def walk(node):
        node_lines = live_node_lines(node, is_declaration=is_declaration)
        if node_lines:
            found[node["id"]] = fault_text(node["id"], node_lines)
            broken.update(one.get("id") for one in _wire_tree([node]))
        elif node.get("type") == "group":
            for child in node.get("children") or []:
                if isinstance(child, dict):
                    walk(child)

    for node in nodes:
        walk(node)
    dropped = tuple(
        Dropped(
            shortcut.get("key"),
            shortcut.get("node_id"),
            named_line(
                f"/shortcuts/{index}",
                "shortcut_dropped",
                {"id": _value_text(shortcut.get("node_id"))},
            ),
        )
        for index, shortcut in enumerate(view.get("shortcuts") or [])
        if isinstance(shortcut, dict) and shortcut.get("node_id") in broken
    )
    return ViewFaults([], found, dropped)
