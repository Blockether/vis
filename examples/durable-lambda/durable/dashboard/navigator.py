"""The list of the sessions in Vis web, with the sessions of each shard.

The gateway of `main` answers the list and its groups. The front adds the rows that each
other shard recorded in S3 (`shards.show`), so a look at the list starts no worker.
Each home has its own ids: a row gets the project of `main` that has the same directory
and the group of `main` that has the same name.
"""

from __future__ import annotations

import json
import time
import urllib.parse
from collections.abc import Callable
from typing import Any

from durable import shards
from durable.dashboard.web import Request

LIST, GROUPS = "/v1/sessions", "/v1/session-groups"
# The fields of a list call for the head of the list. A later page and a cut of the list come from `main` only.
HEAD = {"order", "root", "limit", "grouped"}
# A project of `main` gets a new read of its groups, for a name that it did not have, only after GROUPS_S.
GROUPS_S = 60
# The time of the last read of the groups of each project directory of `main`, and the id of each group by its name.
_groups: dict[str, tuple[float, dict[str, str]]] = {}


def wanted(request: Request) -> bool:
    """Return True for a call whose answer gets the sessions of the other shards."""
    fields = dict(urllib.parse.parse_qsl(request.query))
    path = request.path.rstrip("/")
    return request.method == "GET" and ((path == LIST and set(fields) <= HEAD) or (path == GROUPS and "root" in fields))


def joined(request: Request, raw: bytes, ask: Callable[[str, str], bytes | None]) -> bytes:
    """Return an answer of `main` with the sessions of the other shards. `ask` makes one more GET call to `main`."""
    rows = shards.shown()
    try:
        answer = json.loads(raw)
    except ValueError:
        return raw
    if not rows or not isinstance(answer, dict):
        return raw
    fields = dict(urllib.parse.parse_qsl(request.query))
    if request.path.rstrip("/") == GROUPS:
        return json.dumps(counted(fields["root"], answer, rows)).encode()
    return json.dumps(listed(fields, answer, rows, ask)).encode()


def known(root: str, answer: dict[str, Any]) -> dict[str, str]:
    """Record the groups of a project of `main` from an answer. Return the id of each group by its name."""
    at, found = _groups.get(root, (0.0, {}))
    found = {**found, **{group["name"]: group["id"] for group in answer.get("groups") or []}}
    _groups[root] = (at, found)
    return found


def group(root: str, name: str | None, ask: Callable[[str, str], bytes | None]) -> str | None:
    """Return the id of the group of `main` that has a name, or None."""
    at, found = _groups.get(root, (0.0, {}))
    if name and name not in found and time.time() - at > GROUPS_S:
        raw = ask(GROUPS, urllib.parse.urlencode({"root": root, "archived": "include"}))
        _groups[root] = (time.time(), found)
        found = known(root, json.loads(raw)) if raw else found
    return found.get(name) if name else None


def counted(root: str, answer: dict[str, Any], rows: list[dict[str, Any]]) -> dict[str, Any]:
    """Add the sessions of the other shards to the counts of the groups of a project."""
    known(root, answer)
    for one in answer.get("groups") or []:
        inside = [row for row in rows if row.get("project_root") == root and row.get("group") == one["name"]]
        one["session_count"] = one.get("session_count", 0) + len(inside)
        one["active_at"] = max([one.get("active_at") or 0, *(row.get("modified_at") or 0 for row in inside)])
        answer["session_total"] = answer.get("session_total", 0) + len(inside)
    return answer


def mixed(base: list[dict[str, Any]], more: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Put each new row before the first row that is older. The rows of `main` keep their order."""
    rows = list(base)
    for row in sorted(more, key=lambda row: -(row.get("modified_at") or 0)):
        at = next((index for index, other in enumerate(rows) if (other.get("modified_at") or 0) < (row.get("modified_at") or 0)), len(rows))
        rows.insert(at, row)
    return rows


def listed(fields: dict[str, str], answer: dict[str, Any], rows: list[dict[str, Any]], ask: Callable[[str, str], bytes | None]) -> dict[str, Any]:
    """Add the sessions of the other shards to the head of the list and to its counts.

    The head gets each row, also above its limit: a later page has only the rows of `main`.
    """
    overview = answer.get("overview") or {}
    projects = {project.get("root"): project for project in overview.get("projects") or []}
    filed, loose = [], []
    for row in rows:
        project = projects.get(row.get("project_root"))
        if project is None:
            continue
        for counts in (overview, project):
            for count, field in (("session_count", "id"), ("live_count", "live"), ("unread_count", "is_unread"), ("awaiting_count", "is_awaiting_input")):
                counts[count] = counts.get(count, 0) + bool(row.get(field))
        project["last_activity_ms"] = max(project.get("last_activity_ms") or 0, row.get("modified_at") or 0)
        if fields.get("root") not in (None, project["root"]):
            continue
        own = {name: value for name, value in row.items() if name not in ("project_root", "group")}
        own.update(project_id=project["project_id"], project_name=project["name"], group_id=group(project["root"], row.get("group"), ask))
        (filed if fields.get("grouped") == "aside" and own["group_id"] else loose).append(own)
    if filed:
        answer["grouped"] = mixed(answer.get("grouped") or [], filed)
    answer["sessions"] = mixed(answer.get("sessions") or [], loose)
    answer["total"] = answer.get("total", 0) + len(loose)
    return answer
