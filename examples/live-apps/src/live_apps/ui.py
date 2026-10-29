"""Small node builders and event helpers that every app model uses.

The builders answer the same plain dicts as `vis.ui`, so a model needs no SDK
import and a test can compare renders as data.
"""

from __future__ import annotations

from typing import Any

TONES = ("idle", "running", "ok", "warn", "error")


def heading(node_id: str, text: str, level: int = 2) -> dict:
    return {"type": "heading", "id": node_id, "text": text, "level": level}


def paragraph(node_id: str, text: str) -> dict:
    return {"type": "paragraph", "id": node_id, "text": text}


def status(node_id: str, text: str, tone: str = "idle", **spec: Any) -> dict:
    return {"type": "status", "id": node_id, "text": text, "tone": tone, **spec}


def button(
    node_id: str, label: str, *, is_disabled: bool = False, action: str | None = None
) -> dict:
    node = {"type": "button", "id": node_id, "label": label, "is_disabled": is_disabled}
    if action:
        node["action"] = action
    return node


def card(node_id: str, title: str, **spec: Any) -> dict:
    return {
        "type": "card",
        "id": node_id,
        "title": title,
        **{k: v for k, v in spec.items() if v is not None},
    }


def stat(node_id: str, stats: list[dict]) -> dict:
    return {"type": "stat", "id": node_id, "stats": stats}


def one_stat(stat_id: str, label: str, value: Any, tone: str | None = None) -> dict:
    item = {"id": stat_id, "label": label, "value_text": str(value)}
    if tone:
        item["tone"] = tone
    return item


def group(
    node_id: str, *children: dict, direction: str = "column", **spec: Any
) -> dict:
    node = {
        "type": "group",
        "id": node_id,
        "direction": direction,
        "children": list(children),
    }
    node.update({k: v for k, v in spec.items() if v is not None})
    return node


def row(node_id: str, *children: dict) -> dict:
    return group(node_id, *children, direction="row")


def wrap(node_id: str, *children: dict) -> dict:
    return group(node_id, *children, direction="wrap")


def grid(node_id: str, *children: dict, max_columns: int | None = None) -> dict:
    node = group(node_id, *children, direction="grid")
    if max_columns:
        node["max_columns"] = max_columns
    return node


def table(
    node_id: str, columns: list[tuple[str, str]], rows: list[dict], **spec: Any
) -> dict:
    node = {
        "type": "table",
        "id": node_id,
        # The schema needs a nonblank label: a blank one falls back to the id.
        "columns": [
            {"id": cid, "label": label if label.strip() else cid}
            for cid, label in columns
        ],
        "rows": rows,
    }
    node.update({k: v for k, v in spec.items() if v is not None})
    return node


def table_row(
    row_id: str, cells: list[Any], tone: str | None = None, parent: str | None = None
) -> dict:
    item = {"id": row_id, "cells": ["" if c is None else str(c) for c in cells]}
    if tone:
        item["tone"] = tone
    if parent:
        item["parent"] = parent
    return item


def steps(node_id: str, items: list[dict]) -> dict:
    return {"type": "steps", "id": node_id, "steps": items}


def progress(node_id: str, done: int, total: int, label: str | None = None) -> dict:
    node = {"type": "progress", "id": node_id, "done": done, "total": total}
    if label:
        node["label"] = label
    return node


def log(node_id: str, lines: list[str], **spec: Any) -> dict:
    return {"type": "log", "id": node_id, "lines": list(lines), **spec}


def scene(
    node_id: str, width: float, height: float, objects: list[dict], **spec: Any
) -> dict:
    """A 3D world: each surface draws `objects` and eases them between ticks."""
    node = {
        "type": "scene",
        "id": node_id,
        "width": width,
        "height": height,
        "objects": objects,
    }
    node.update({k: v for k, v in spec.items() if v is not None})
    return node


def scene_object(object_id: str, mesh: str, **spec: Any) -> dict:
    """One object of a scene: a mesh with a place, a turn, a scale and a colour."""
    item = {"id": object_id, "mesh": mesh}
    item.update({k: v for k, v in spec.items() if v is not None})
    return item


def option(value: str, label: str | None = None) -> dict:
    return {"value": value, "label": label or value}


def field(kind: str, name: str, label: str, **spec: Any) -> dict:
    node = {"type": kind, "id": name, "name": name, "label": label}
    node.update({k: v for k, v in spec.items() if v is not None})
    return node


def shortcut(
    key: str, label: str, *, action: str | None = None, node_id: str | None = None
) -> dict:
    """One app shortcut in the C1 wire shape: exactly one of action and node_id."""
    if (action is None) == (node_id is None):
        raise ValueError("a shortcut needs exactly one of action and node_id")
    item = {"key": key, "label": label}
    item["action" if action else "node_id"] = action or node_id
    return item


KEY_NAMES = {
    "ArrowUp": "↑",
    "ArrowDown": "↓",
    "ArrowLeft": "←",
    "ArrowRight": "→",
}


def key_help(shortcuts: list[dict]) -> str:
    """One help line made from the declared shortcuts, so the two always agree."""
    parts = [
        f"{KEY_NAMES.get(s['key'], s['key'])} {s['label'].lower()}" for s in shortcuts
    ]
    return "Keys: " + ", ".join(parts) + "." if parts else "This app has no keys."


def intent(event: dict) -> str | None:
    """What a person asked for: the pressed node id, or the shortcut action id."""
    kind = event.get("type")
    if kind == "activate":
        return event.get("node_id")
    if kind == "shortcut":
        return event.get("action")
    return None
