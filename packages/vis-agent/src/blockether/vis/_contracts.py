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


# An appended live item replaces the item with its id. The host fills these keys
# when the item leaves them out; every other required key must come with it.
_LIVE_ITEM_DEFAULTS = {
    "step": {"tone": "idle"},
    "lane": {"tone": "idle"},
    "link": {"target_kind": "url"},
}
_LIVE_ITEMS = {
    "stats": "stat",
    "steps": "step",
    "lanes": "lane",
    "rows": "row",
    "links": "link",
}


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


def _timeline_branch() -> dict[str, Any]:
    return next(
        branch
        for branch in definition("view", "component")["oneOf"]
        if branch["properties"]["type"].get("const") == "timeline"
    )


def _epoch_ms(value: Any) -> bool:
    return type(value) is int and value >= 0


def lanes_error(lanes: Any) -> str | None:
    """Answer what makes the lanes of a timeline invalid, or None when they are valid.

    The schema bounds the count and the time fields. It cannot say that ids are unique
    in one node or that a lane never ends before it starts, so this check says it.
    """
    if not isinstance(lanes, (list, tuple)):
        return "timeline lanes must be a list"
    limit = _timeline_branch()["properties"]["lanes"]["maxItems"]
    if len(lanes) > limit:
        return f"a timeline holds at most {limit} lanes"
    ids = [lane.get("id") if isinstance(lane, dict) else None for lane in lanes]
    if repeated := sorted({i for i in ids if ids.count(i) > 1}, key=str):
        return f"timeline lane id repeats: {repeated[0]}"
    for lane in lanes:
        if not isinstance(lane, dict):
            return f"every lane must be a map, got {lane!r}"
        name = lane.get("id")
        if not _epoch_ms(lane.get("start")):
            return (
                f"lane {name}: start must be epoch milliseconds, a nonnegative integer"
            )
        if "end" in lane and not _epoch_ms(lane["end"]):
            return f"lane {name}: end must be epoch milliseconds, a nonnegative integer"
        if "end" in lane and lane["end"] < lane["start"]:
            return "a lane ends before it starts"
    return None


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
    return picture_byte_count(node.get("data")) + picture_byte_count(
        (node.get("before") or {}).get("data")
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
