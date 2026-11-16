"""The list of the sessions in Vis web, with the sessions of each shard.

The gateway of `main` answers the list, its groups and the search. The front adds the rows
that each other shard recorded in S3 (`shards.show`), so a look at the list starts no worker.
It adds them to the first page only, in the archive view of the call, and a search
finds them by their title.
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

LIST, GROUPS, SEARCH = "/v1/sessions", "/v1/session-groups", "/v1/sessions/actions/search"
# The fields of a list call and of a search call whose answer gets the rows of the other shards.
# A later page (`after`) has only the rows of `main`: the first page has each row of the other shards.
HEAD = {"order", "root", "limit", "grouped", "archived", "ids", "group_id"}
ASKED = {"q", "limit", "archived", "channel", "root"}
# What a search says of a session that it found by its title.
TITLE = {"rank": 0, "is_in_title": True, "is_in_request": False, "is_in_reply": False, "is_in_thinking": False, "request_snippet": None, "reply_snippet": None, "hits": []}
# A project of `main` gets a new read of its groups, for a name that it did not have, only after GROUPS_S.
GROUPS_S = 60
# The time of the last read of the groups of each project directory of `main`, and the id of each group by its name.
_groups: dict[str, tuple[float, dict[str, str]]] = {}
# The project of `main` for each project directory, and the time of the last read for a directory that was not known.
_projects: dict[str, dict[str, Any]] = {}
_asked = 0.0


def wanted(request: Request) -> bool:
    """Return True for a call whose answer gets the sessions of the other shards."""
    fields = dict(urllib.parse.parse_qsl(request.query))
    path = request.path.rstrip("/")
    return request.method == "GET" and ((path == LIST and set(fields) <= HEAD) or (path == SEARCH and set(fields) <= ASKED) or (path == GROUPS and "root" in fields))


def joined(request: Request, raw: bytes, ask: Callable[[str, str], bytes | None]) -> bytes:
    """Return an answer of `main` with the sessions of the other shards. `ask` makes one more GET call to `main`."""
    rows = shards.shown()
    try:
        answer = json.loads(raw)
    except ValueError:
        return raw
    if not rows or not isinstance(answer, dict):
        return raw
    fields, path = dict(urllib.parse.parse_qsl(request.query)), request.path.rstrip("/")
    active = [row for row in rows if not row.get("archived_at")]
    if path == GROUPS:
        return json.dumps(counted(fields["root"], answer, active)).encode()
    # The view of the archive: the active sessions, the archived sessions or both.
    view = (fields.get("archived") or "exclude").lower()
    seen = rows if view == "include" else active if view == "exclude" else [row for row in rows if row.get("archived_at")]
    if path == SEARCH:
        return json.dumps(found(fields, answer, seen, ask)).encode()
    return json.dumps(listed(fields, answer, active, seen, ask)).encode()


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


def project(root: str | None, ask: Callable[[str, str], bytes | None]) -> dict[str, Any] | None:
    """Return the project of `main` that has a directory, or None."""
    global _asked
    if root not in _projects and time.time() - _asked > GROUPS_S:
        _asked, raw = time.time(), ask(LIST, "limit=1")
        recorded((json.loads(raw).get("overview") or {}) if raw else {})
    return _projects.get(root or "")


def recorded(overview: dict[str, Any]) -> dict[str, dict[str, Any]]:
    """Record the projects of `main` from the overview of a list. Return the project of each directory."""
    projects = {one.get("root"): one for one in overview.get("projects") or []}
    _projects.update(projects)
    return projects


def placed(row: dict[str, Any], home: dict[str, Any], ask: Callable[[str, str], bytes | None]) -> dict[str, Any]:
    """Return the row of a session of another shard with the project and the group of `main`."""
    own = {name: value for name, value in row.items() if name not in ("project_root", "group")}
    own.update(project_id=home["project_id"], project_name=home["name"], group_id=group(home["root"], row.get("group"), ask))
    return own


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


def listed(fields: dict[str, str], answer: dict[str, Any], active: list[dict[str, Any]], seen: list[dict[str, Any]], ask: Callable[[str, str], bytes | None]) -> dict[str, Any]:
    """Add the sessions of the other shards to the first page of the list and to its counts.

    The counts get the `active` sessions. The page gets each row of `seen` that the call
    asks for, also above its limit: a later page has only the rows of `main`.
    """
    overview = answer.get("overview") or {}
    projects = recorded(overview)
    for row in active:
        home = projects.get(row.get("project_root"))
        if home is None:
            continue
        for counts in (overview, home):
            for count, field in (("session_count", "id"), ("live_count", "live"), ("unread_count", "is_unread"), ("awaiting_count", "is_awaiting_input")):
                counts[count] = counts.get(count, 0) + bool(row.get(field))
        home["last_activity_ms"] = max(home.get("last_activity_ms") or 0, row.get("modified_at") or 0)
    ids = set(filter(None, fields["ids"].split(","))) if "ids" in fields else None
    filed, loose = [], []
    for row in seen:
        home = projects.get(row.get("project_root"))
        if home is None or fields.get("root") not in (None, home["root"]) or (ids is not None and row["id"] not in ids):
            continue
        own = placed(row, home, ask)
        if fields.get("group_id") not in (None, own["group_id"]):
            continue
        (filed if fields.get("grouped") == "aside" and own["group_id"] else loose).append(own)
    if filed:
        answer["grouped"] = mixed(answer.get("grouped") or [], filed)
    answer["sessions"] = mixed(answer.get("sessions") or [], loose)
    answer["total"] = answer.get("total", 0) + len(loose)
    return answer


def found(fields: dict[str, str], answer: dict[str, Any], seen: list[dict[str, Any]], ask: Callable[[str, str], bytes | None]) -> dict[str, Any]:
    """Add the sessions of the other shards to the first page of a search.

    A search without words lists the newest sessions. A search with words finds a session
    of another shard by its title: the transcript is in the home of its shard.
    """
    words, more = (fields.get("q") or "").strip().lower(), []
    for row in seen:
        home = project(row.get("project_root"), ask)
        if home is None or fields.get("root") not in (None, home["root"]) or words not in (row.get("title") or "").lower():
            continue
        more.append({**placed(row, home, ask), **({"match": dict(TITLE)} if words else {})})
    answer["sessions"] = mixed(answer.get("sessions") or [], more)
    answer["total"] = answer.get("total", 0) + len(more)
    return answer
